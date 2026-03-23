package ming

import (
	"fmt"
	"strconv"
	"strings"
)

// TopEnv creates a new top-level environment with builtins.
func TopEnv() *Env {
	env := NewEnv(nil)
	for name, proc := range builtins {
		env.Set(name, proc)
	}
	env.Set("display", &EnvBuiltinProc{Name: "display", Fn: builtinDisplay})
	env.Set("write", &EnvBuiltinProc{Name: "write", Fn: builtinWrite})
	env.Set("newline", &EnvBuiltinProc{Name: "newline", Fn: builtinNewline})
	return env
}

// EvalStr evaluates one or more Scheme expressions and returns the string
// representation of the last result.
func EvalStr(input string) (string, error) {
	tokens, err := Tokenize(input)
	if err != nil {
		return "", &EvalError{Message: err.Error()}
	}

	parser := NewParser(tokens)
	exprs, err := parser.ParseAll()
	if err != nil {
		return "", &EvalError{Message: err.Error()}
	}

	if len(exprs) == 0 {
		return "", &EvalError{Message: "no expressions"}
	}

	env := TopEnv()
	var result SchemeValue
	for _, expr := range exprs {
		result, err = Eval(expr, env)
		if err != nil {
			return "", err
		}
	}

	return result.String(), nil
}

// EvalStrWithOutput evaluates Scheme expressions and returns both the result
// string and any captured output from display/write/newline.
func EvalStrWithOutput(input string) (result string, output string, err error) {
	tokens, err := Tokenize(input)
	if err != nil {
		return "", "", &EvalError{Message: err.Error()}
	}

	parser := NewParser(tokens)
	exprs, err := parser.ParseAll()
	if err != nil {
		return "", "", &EvalError{Message: err.Error()}
	}

	if len(exprs) == 0 {
		return "", "", &EvalError{Message: "no expressions"}
	}

	env := TopEnv()
	var buf strings.Builder
	env.Set("$$output$$", &outputPort{buf: &buf})

	var res SchemeValue
	for _, expr := range exprs {
		res, err = Eval(expr, env)
		if err != nil {
			return "", "", err
		}
	}

	return res.String(), buf.String(), nil
}

// outputPort is an internal value that holds the output buffer.
type outputPort struct {
	buf *strings.Builder
}

func (o *outputPort) String() string { return "#<output-port>" }

// Eval evaluates an expression in the given environment.
// Uses a trampoline loop for tail call optimization.
func Eval(expr Expr, env *Env) (SchemeValue, error) {
	for {
		switch e := expr.(type) {
		case *NumberExpr:
			return &SchemeInt{Value: e.Value}, nil

		case *BoolExpr:
			return &SchemeBool{Value: e.Value}, nil

		case *StringExpr:
			return &SchemeString{Value: e.Value}, nil

		case *CharExpr:
			return &SchemeChar{Value: e.Value}, nil

		case *SymbolExpr:
			if v, ok := env.Get(e.Name); ok {
				return v, nil
			}
			line, col := e.Pos()
			return nil, &EvalError{Message: fmt.Sprintf("%d:%d: unbound variable '%s'", line, col, e.Name)}

		case *ListExpr:
			if len(e.Elements) == 0 {
				line, col := e.Pos()
				return nil, &EvalError{Message: fmt.Sprintf("%d:%d: empty application", line, col)}
			}

			// Check for special forms
			if sym, ok := e.Elements[0].(*SymbolExpr); ok {
				switch sym.Name {
				case "and":
					// TCO: last expr in and is tail
					args := e.Elements[1:]
					if len(args) == 0 {
						return &SchemeBool{Value: true}, nil
					}
					for _, a := range args[:len(args)-1] {
						result, err := Eval(a, env)
						if err != nil {
							return nil, err
						}
						if !isTruthy(result) {
							return result, nil
						}
					}
					expr = args[len(args)-1]
					continue

				case "or":
					// TCO: last expr in or is tail
					args := e.Elements[1:]
					if len(args) == 0 {
						return &SchemeBool{Value: false}, nil
					}
					for _, a := range args[:len(args)-1] {
						result, err := Eval(a, env)
						if err != nil {
							return nil, err
						}
						if isTruthy(result) {
							return result, nil
						}
					}
					expr = args[len(args)-1]
					continue

				case "set!":
					if len(e.Elements) != 3 {
						line, col := e.Pos()
						return nil, &EvalError{Message: fmt.Sprintf("%d:%d: set!: bad syntax", line, col)}
					}
					sym, ok := e.Elements[1].(*SymbolExpr)
					if !ok {
						line, col := e.Elements[1].Pos()
						return nil, &EvalError{Message: fmt.Sprintf("%d:%d: set!: not a variable", line, col)}
					}
					val, err := Eval(e.Elements[2], env)
					if err != nil {
						return nil, err
					}
					if !env.SetExisting(sym.Name, val) {
						line, col := sym.Pos()
						return nil, &EvalError{Message: fmt.Sprintf("%d:%d: set!: variable %s is not bound", line, col, sym.Name)}
					}
					return &SchemeVoid{}, nil

				case "define":
					return evalDefine(e, env)

				case "if":
					// TCO: branch is tail
					if len(e.Elements) < 3 || len(e.Elements) > 4 {
						line, col := e.Pos()
						return nil, &EvalError{Message: fmt.Sprintf("%d:%d: if: bad syntax", line, col)}
					}
					cond, err := Eval(e.Elements[1], env)
					if err != nil {
						return nil, err
					}
					if isTruthy(cond) {
						expr = e.Elements[2]
						continue
					}
					if len(e.Elements) == 4 {
						expr = e.Elements[3]
						continue
					}
					return &SchemeVoid{}, nil

				case "quote":
					if len(e.Elements) != 2 {
						line, col := e.Pos()
						return nil, &EvalError{Message: fmt.Sprintf("%d:%d: quote: requires exactly 1 argument", line, col)}
					}
					return quoteExpr(e.Elements[1]), nil

				case "lambda":
					return evalLambda(e, env)

				case "let":
					// TCO: inline let so body tail is tail
					newExpr, newEnv, err := setupLet(e, env)
					if err != nil {
						return nil, err
					}
					expr = newExpr
					env = newEnv
					continue

				case "begin":
					// TCO: last expr in begin is tail
					body := e.Elements[1:]
					if len(body) == 0 {
						return &SchemeVoid{}, nil
					}
					for _, b := range body[:len(body)-1] {
						_, err := Eval(b, env)
						if err != nil {
							return nil, err
						}
					}
					expr = body[len(body)-1]
					continue

				case "cond":
					// TCO: selected branch body tail is tail
					newExpr, newEnv, done, result, err := setupCond(e, env)
					if err != nil {
						return nil, err
					}
					if done {
						return result, nil
					}
					expr = newExpr
					env = newEnv
					continue
				}
			}

			// Evaluate operator
			op, err := Eval(e.Elements[0], env)
			if err != nil {
				return nil, err
			}

			// Evaluate arguments
			args := make([]SchemeValue, len(e.Elements)-1)
			for i, arg := range e.Elements[1:] {
				args[i], err = Eval(arg, env)
				if err != nil {
					return nil, err
				}
			}

			// Apply
			switch fn := op.(type) {
			case *BuiltinProc:
				return fn.Fn(args, e)
			case *EnvBuiltinProc:
				return fn.Fn(args, e, env)
			case *Lambda:
				// TCO: lambda application is tail call
				if len(args) != len(fn.Params) {
					line, col := e.Pos()
					return nil, &EvalError{Message: fmt.Sprintf("%d:%d: wrong number of arguments: expected %d, got %d", line, col, len(fn.Params), len(args))}
				}
				localEnv := NewEnv(fn.Env)
				for i, p := range fn.Params {
					localEnv.Set(p, args[i])
				}
				// Evaluate all but last body expr, then tail-call the last
				for _, bodyExpr := range fn.Body[:len(fn.Body)-1] {
					_, err := Eval(bodyExpr, localEnv)
					if err != nil {
						return nil, err
					}
				}
				expr = fn.Body[len(fn.Body)-1]
				env = localEnv
				continue
			}

			line, col := e.Pos()
			return nil, &EvalError{Message: fmt.Sprintf("%d:%d: not a procedure", line, col)}

		default:
			return nil, &EvalError{Message: "unknown expression type"}
		}
	}
}

// Lambda is a user-defined closure.
type Lambda struct {
	Params []string
	Body   []Expr
	Env    *Env
}

func (l *Lambda) String() string {
	return "#<procedure>"
}

func evalDefine(e *ListExpr, env *Env) (SchemeValue, error) {
	if len(e.Elements) < 3 {
		line, col := e.Pos()
		return nil, &EvalError{Message: fmt.Sprintf("%d:%d: define: bad syntax", line, col)}
	}

	switch target := e.Elements[1].(type) {
	case *SymbolExpr:
		// (define x expr)
		val, err := Eval(e.Elements[2], env)
		if err != nil {
			return nil, err
		}
		env.Set(target.Name, val)
		return &SchemeVoid{}, nil

	case *ListExpr:
		// (define (f params...) body...)
		if len(target.Elements) == 0 {
			line, col := e.Pos()
			return nil, &EvalError{Message: fmt.Sprintf("%d:%d: define: bad syntax", line, col)}
		}
		nameSym, ok := target.Elements[0].(*SymbolExpr)
		if !ok {
			line, col := e.Pos()
			return nil, &EvalError{Message: fmt.Sprintf("%d:%d: define: bad syntax", line, col)}
		}
		params := make([]string, len(target.Elements)-1)
		for i, p := range target.Elements[1:] {
			ps, ok := p.(*SymbolExpr)
			if !ok {
				line, col := e.Pos()
				return nil, &EvalError{Message: fmt.Sprintf("%d:%d: define: bad syntax", line, col)}
			}
			params[i] = ps.Name
		}
		lam := &Lambda{
			Params: params,
			Body:   e.Elements[2:],
			Env:    env,
		}
		env.Set(nameSym.Name, lam)
		return &SchemeVoid{}, nil

	default:
		line, col := e.Pos()
		return nil, &EvalError{Message: fmt.Sprintf("%d:%d: define: bad syntax", line, col)}
	}
}


func evalLambda(e *ListExpr, env *Env) (SchemeValue, error) {
	if len(e.Elements) < 3 {
		line, col := e.Pos()
		return nil, &EvalError{Message: fmt.Sprintf("%d:%d: lambda: bad syntax", line, col)}
	}

	paramList, ok := e.Elements[1].(*ListExpr)
	if !ok {
		line, col := e.Pos()
		return nil, &EvalError{Message: fmt.Sprintf("%d:%d: lambda: bad syntax", line, col)}
	}

	params := make([]string, len(paramList.Elements))
	for i, p := range paramList.Elements {
		ps, ok := p.(*SymbolExpr)
		if !ok {
			line, col := e.Pos()
			return nil, &EvalError{Message: fmt.Sprintf("%d:%d: lambda: bad syntax", line, col)}
		}
		params[i] = ps.Name
	}

	return &Lambda{
		Params: params,
		Body:   e.Elements[2:],
		Env:    env,
	}, nil
}

// setupLet prepares the environment for a let form and returns the tail expression.
// For named let, it sets up the recursive binding.
func setupLet(e *ListExpr, env *Env) (Expr, *Env, error) {
	if len(e.Elements) < 3 {
		line, col := e.Pos()
		return nil, nil, &EvalError{Message: fmt.Sprintf("%d:%d: let: bad syntax", line, col)}
	}

	// Named let: (let name ((var init) ...) body...)
	if sym, ok := e.Elements[1].(*SymbolExpr); ok {
		if len(e.Elements) < 4 {
			line, col := e.Pos()
			return nil, nil, &EvalError{Message: fmt.Sprintf("%d:%d: let: bad syntax", line, col)}
		}
		bindingsList, ok := e.Elements[2].(*ListExpr)
		if !ok {
			line, col := e.Pos()
			return nil, nil, &EvalError{Message: fmt.Sprintf("%d:%d: let: bad syntax", line, col)}
		}
		params := make([]string, len(bindingsList.Elements))
		inits := make([]SchemeValue, len(bindingsList.Elements))
		for i, b := range bindingsList.Elements {
			pair, ok := b.(*ListExpr)
			if !ok || len(pair.Elements) != 2 {
				line, col := e.Pos()
				return nil, nil, &EvalError{Message: fmt.Sprintf("%d:%d: let: bad syntax", line, col)}
			}
			ps, ok := pair.Elements[0].(*SymbolExpr)
			if !ok {
				line, col := e.Pos()
				return nil, nil, &EvalError{Message: fmt.Sprintf("%d:%d: let: bad syntax", line, col)}
			}
			params[i] = ps.Name
			val, err := Eval(pair.Elements[1], env)
			if err != nil {
				return nil, nil, err
			}
			inits[i] = val
		}
		lam := &Lambda{Params: params, Body: e.Elements[3:], Env: env}
		loopEnv := NewEnv(env)
		loopEnv.Set(sym.Name, lam)
		lam.Env = loopEnv
		localEnv := NewEnv(loopEnv)
		for i, p := range params {
			localEnv.Set(p, inits[i])
		}
		body := lam.Body
		// Eval all but last, return last as tail
		for _, bodyExpr := range body[:len(body)-1] {
			_, err := Eval(bodyExpr, localEnv)
			if err != nil {
				return nil, nil, err
			}
		}
		return body[len(body)-1], localEnv, nil
	}

	// Regular let: (let ((var init) ...) body...)
	bindingsList, ok := e.Elements[1].(*ListExpr)
	if !ok {
		line, col := e.Pos()
		return nil, nil, &EvalError{Message: fmt.Sprintf("%d:%d: let: bad syntax", line, col)}
	}
	localEnv := NewEnv(env)
	for _, b := range bindingsList.Elements {
		pair, ok := b.(*ListExpr)
		if !ok || len(pair.Elements) != 2 {
			line, col := e.Pos()
			return nil, nil, &EvalError{Message: fmt.Sprintf("%d:%d: let: bad syntax", line, col)}
		}
		s, ok := pair.Elements[0].(*SymbolExpr)
		if !ok {
			line, col := e.Pos()
			return nil, nil, &EvalError{Message: fmt.Sprintf("%d:%d: let: bad syntax", line, col)}
		}
		val, err := Eval(pair.Elements[1], env)
		if err != nil {
			return nil, nil, err
		}
		localEnv.Set(s.Name, val)
	}
	body := e.Elements[2:]
	for _, bodyExpr := range body[:len(body)-1] {
		_, err := Eval(bodyExpr, localEnv)
		if err != nil {
			return nil, nil, err
		}
	}
	return body[len(body)-1], localEnv, nil
}

// setupCond finds the matching cond clause and returns the tail expression.
func setupCond(e *ListExpr, env *Env) (Expr, *Env, bool, SchemeValue, error) {
	for _, clause := range e.Elements[1:] {
		cl, ok := clause.(*ListExpr)
		if !ok || len(cl.Elements) < 2 {
			line, col := e.Pos()
			return nil, nil, false, nil, &EvalError{Message: fmt.Sprintf("%d:%d: cond: bad syntax", line, col)}
		}
		if sym, ok := cl.Elements[0].(*SymbolExpr); ok && sym.Name == "else" {
			body := cl.Elements[1:]
			for _, b := range body[:len(body)-1] {
				_, err := Eval(b, env)
				if err != nil {
					return nil, nil, false, nil, err
				}
			}
			return body[len(body)-1], env, false, nil, nil
		}
		cond, err := Eval(cl.Elements[0], env)
		if err != nil {
			return nil, nil, false, nil, err
		}
		if isTruthy(cond) {
			body := cl.Elements[1:]
			for _, b := range body[:len(body)-1] {
				_, err := Eval(b, env)
				if err != nil {
					return nil, nil, false, nil, err
				}
			}
			return body[len(body)-1], env, false, nil, nil
		}
	}
	return nil, nil, true, &SchemeVoid{}, nil
}

// quoteExpr converts a parsed Expr into a SchemeValue without evaluation.
func quoteExpr(expr Expr) SchemeValue {
	switch e := expr.(type) {
	case *NumberExpr:
		return &SchemeInt{Value: e.Value}
	case *BoolExpr:
		return &SchemeBool{Value: e.Value}
	case *StringExpr:
		return &SchemeString{Value: e.Value}
	case *SymbolExpr:
		return &SchemeSymbol{Name: e.Name}
	case *ListExpr:
		var result SchemeValue = &SchemeEmpty{}
		for i := len(e.Elements) - 1; i >= 0; i-- {
			result = &SchemePair{Car: quoteExpr(e.Elements[i]), Cdr: result}
		}
		return result
	default:
		return &SchemeVoid{}
	}
}

// BuiltinProc is a built-in procedure.
type BuiltinProc struct {
	Name string
	Fn   func(args []SchemeValue, callExpr *ListExpr) (SchemeValue, error)
}

func (b *BuiltinProc) String() string {
	return fmt.Sprintf("#<procedure %s>", b.Name)
}

// isTruthy returns true for all values except #f.
func isTruthy(v SchemeValue) bool {
	if b, ok := v.(*SchemeBool); ok {
		return b.Value
	}
	return true
}


// requireInts extracts int64 values from args, returning an error if any aren't integers.
func requireInts(args []SchemeValue, name string, callExpr *ListExpr) ([]int64, error) {
	nums := make([]int64, len(args))
	for i, a := range args {
		n, ok := a.(*SchemeInt)
		if !ok {
			line, col := callExpr.Pos()
			return nil, &EvalError{Message: fmt.Sprintf("%d:%d: %s: expected number, got %s", line, col, name, a.String())}
		}
		nums[i] = n.Value
	}
	return nums, nil
}

// builtins is the map of built-in procedures.
var builtins = map[string]*BuiltinProc{}

func init() {
	builtins["+"] = &BuiltinProc{Name: "+", Fn: builtinAdd}
	builtins["-"] = &BuiltinProc{Name: "-", Fn: builtinSub}
	builtins["*"] = &BuiltinProc{Name: "*", Fn: builtinMul}
	builtins["/"] = &BuiltinProc{Name: "/", Fn: builtinDiv}
	builtins["<"] = &BuiltinProc{Name: "<", Fn: builtinLT}
	builtins[">"] = &BuiltinProc{Name: ">", Fn: builtinGT}
	builtins["="] = &BuiltinProc{Name: "=", Fn: builtinEq}
	builtins["<="] = &BuiltinProc{Name: "<=", Fn: builtinLE}
	builtins[">="] = &BuiltinProc{Name: ">=", Fn: builtinGE}
	builtins["not"] = &BuiltinProc{Name: "not", Fn: builtinNot}
	builtins["cons"] = &BuiltinProc{Name: "cons", Fn: builtinCons}
	builtins["car"] = &BuiltinProc{Name: "car", Fn: builtinCar}
	builtins["cdr"] = &BuiltinProc{Name: "cdr", Fn: builtinCdr}
	builtins["null?"] = &BuiltinProc{Name: "null?", Fn: builtinNullQ}
	builtins["pair?"] = &BuiltinProc{Name: "pair?", Fn: builtinPairQ}
	builtins["list"] = &BuiltinProc{Name: "list", Fn: builtinList}
	builtins["length"] = &BuiltinProc{Name: "length", Fn: builtinLength}
	builtins["append"] = &BuiltinProc{Name: "append", Fn: builtinAppend}
	builtins["number?"] = &BuiltinProc{Name: "number?", Fn: builtinNumberQ}
	builtins["string?"] = &BuiltinProc{Name: "string?", Fn: builtinStringQ}
	builtins["boolean?"] = &BuiltinProc{Name: "boolean?", Fn: builtinBooleanQ}
	builtins["symbol?"] = &BuiltinProc{Name: "symbol?", Fn: builtinSymbolQ}
	builtins["char?"] = &BuiltinProc{Name: "char?", Fn: builtinCharQ}
	builtins["string-append"] = &BuiltinProc{Name: "string-append", Fn: builtinStringAppend}
	builtins["string-length"] = &BuiltinProc{Name: "string-length", Fn: builtinStringLength}
	builtins["substring"] = &BuiltinProc{Name: "substring", Fn: builtinSubstring}
	builtins["string->number"] = &BuiltinProc{Name: "string->number", Fn: builtinStringToNumber}
	builtins["number->string"] = &BuiltinProc{Name: "number->string", Fn: builtinNumberToString}
	builtins["symbol->string"] = &BuiltinProc{Name: "symbol->string", Fn: builtinSymbolToString}
	builtins["string->symbol"] = &BuiltinProc{Name: "string->symbol", Fn: builtinStringToSymbol}
	builtins["string-ref"] = &BuiltinProc{Name: "string-ref", Fn: builtinStringRef}
	builtins["string-copy"] = &BuiltinProc{Name: "string-copy", Fn: builtinStringCopy}
	builtins["string-set!"] = &BuiltinProc{Name: "string-set!", Fn: builtinStringSet}
}

func builtinAdd(args []SchemeValue, callExpr *ListExpr) (SchemeValue, error) {
	nums, err := requireInts(args, "+", callExpr)
	if err != nil {
		return nil, err
	}
	var sum int64
	for _, n := range nums {
		sum += n
	}
	return &SchemeInt{Value: sum}, nil
}

func builtinSub(args []SchemeValue, callExpr *ListExpr) (SchemeValue, error) {
	if len(args) == 0 {
		line, col := callExpr.Pos()
		return nil, &EvalError{Message: fmt.Sprintf("%d:%d: -: requires at least 1 argument", line, col)}
	}
	nums, err := requireInts(args, "-", callExpr)
	if err != nil {
		return nil, err
	}
	if len(nums) == 1 {
		return &SchemeInt{Value: -nums[0]}, nil
	}
	result := nums[0]
	for _, n := range nums[1:] {
		result -= n
	}
	return &SchemeInt{Value: result}, nil
}

func builtinMul(args []SchemeValue, callExpr *ListExpr) (SchemeValue, error) {
	nums, err := requireInts(args, "*", callExpr)
	if err != nil {
		return nil, err
	}
	var product int64 = 1
	for _, n := range nums {
		product *= n
	}
	return &SchemeInt{Value: product}, nil
}

func builtinDiv(args []SchemeValue, callExpr *ListExpr) (SchemeValue, error) {
	if len(args) < 2 {
		line, col := callExpr.Pos()
		return nil, &EvalError{Message: fmt.Sprintf("%d:%d: /: requires at least 2 arguments", line, col)}
	}
	nums, err := requireInts(args, "/", callExpr)
	if err != nil {
		return nil, err
	}
	result := nums[0]
	for _, n := range nums[1:] {
		if n == 0 {
			line, col := callExpr.Pos()
			return nil, &EvalError{Message: fmt.Sprintf("%d:%d: /: division by zero", line, col)}
		}
		result /= n
	}
	return &SchemeInt{Value: result}, nil
}

func builtinLT(args []SchemeValue, callExpr *ListExpr) (SchemeValue, error) {
	nums, err := requireInts(args, "<", callExpr)
	if err != nil {
		return nil, err
	}
	if len(nums) < 2 {
		line, col := callExpr.Pos()
		return nil, &EvalError{Message: fmt.Sprintf("%d:%d: <: requires at least 2 arguments", line, col)}
	}
	for i := 0; i < len(nums)-1; i++ {
		if !(nums[i] < nums[i+1]) {
			return &SchemeBool{Value: false}, nil
		}
	}
	return &SchemeBool{Value: true}, nil
}

func builtinGT(args []SchemeValue, callExpr *ListExpr) (SchemeValue, error) {
	nums, err := requireInts(args, ">", callExpr)
	if err != nil {
		return nil, err
	}
	if len(nums) < 2 {
		line, col := callExpr.Pos()
		return nil, &EvalError{Message: fmt.Sprintf("%d:%d: >: requires at least 2 arguments", line, col)}
	}
	for i := 0; i < len(nums)-1; i++ {
		if !(nums[i] > nums[i+1]) {
			return &SchemeBool{Value: false}, nil
		}
	}
	return &SchemeBool{Value: true}, nil
}

func builtinEq(args []SchemeValue, callExpr *ListExpr) (SchemeValue, error) {
	nums, err := requireInts(args, "=", callExpr)
	if err != nil {
		return nil, err
	}
	if len(nums) < 2 {
		line, col := callExpr.Pos()
		return nil, &EvalError{Message: fmt.Sprintf("%d:%d: =: requires at least 2 arguments", line, col)}
	}
	for i := 0; i < len(nums)-1; i++ {
		if nums[i] != nums[i+1] {
			return &SchemeBool{Value: false}, nil
		}
	}
	return &SchemeBool{Value: true}, nil
}

func builtinLE(args []SchemeValue, callExpr *ListExpr) (SchemeValue, error) {
	nums, err := requireInts(args, "<=", callExpr)
	if err != nil {
		return nil, err
	}
	if len(nums) < 2 {
		line, col := callExpr.Pos()
		return nil, &EvalError{Message: fmt.Sprintf("%d:%d: <=: requires at least 2 arguments", line, col)}
	}
	for i := 0; i < len(nums)-1; i++ {
		if !(nums[i] <= nums[i+1]) {
			return &SchemeBool{Value: false}, nil
		}
	}
	return &SchemeBool{Value: true}, nil
}

func builtinGE(args []SchemeValue, callExpr *ListExpr) (SchemeValue, error) {
	nums, err := requireInts(args, ">=", callExpr)
	if err != nil {
		return nil, err
	}
	if len(nums) < 2 {
		line, col := callExpr.Pos()
		return nil, &EvalError{Message: fmt.Sprintf("%d:%d: >=: requires at least 2 arguments", line, col)}
	}
	for i := 0; i < len(nums)-1; i++ {
		if !(nums[i] >= nums[i+1]) {
			return &SchemeBool{Value: false}, nil
		}
	}
	return &SchemeBool{Value: true}, nil
}


func builtinNot(args []SchemeValue, callExpr *ListExpr) (SchemeValue, error) {
	if len(args) != 1 {
		line, col := callExpr.Pos()
		return nil, &EvalError{Message: fmt.Sprintf("%d:%d: not: requires exactly 1 argument", line, col)}
	}
	return &SchemeBool{Value: !isTruthy(args[0])}, nil
}

func builtinCons(args []SchemeValue, callExpr *ListExpr) (SchemeValue, error) {
	if len(args) != 2 {
		line, col := callExpr.Pos()
		return nil, &EvalError{Message: fmt.Sprintf("%d:%d: cons: requires exactly 2 arguments", line, col)}
	}
	return &SchemePair{Car: args[0], Cdr: args[1]}, nil
}

func builtinCar(args []SchemeValue, callExpr *ListExpr) (SchemeValue, error) {
	if len(args) != 1 {
		line, col := callExpr.Pos()
		return nil, &EvalError{Message: fmt.Sprintf("%d:%d: car: requires exactly 1 argument", line, col)}
	}
	p, ok := args[0].(*SchemePair)
	if !ok {
		line, col := callExpr.Pos()
		return nil, &EvalError{Message: fmt.Sprintf("%d:%d: car: expected pair", line, col)}
	}
	return p.Car, nil
}

func builtinCdr(args []SchemeValue, callExpr *ListExpr) (SchemeValue, error) {
	if len(args) != 1 {
		line, col := callExpr.Pos()
		return nil, &EvalError{Message: fmt.Sprintf("%d:%d: cdr: requires exactly 1 argument", line, col)}
	}
	p, ok := args[0].(*SchemePair)
	if !ok {
		line, col := callExpr.Pos()
		return nil, &EvalError{Message: fmt.Sprintf("%d:%d: cdr: expected pair", line, col)}
	}
	return p.Cdr, nil
}

func builtinNullQ(args []SchemeValue, callExpr *ListExpr) (SchemeValue, error) {
	if len(args) != 1 {
		line, col := callExpr.Pos()
		return nil, &EvalError{Message: fmt.Sprintf("%d:%d: null?: requires exactly 1 argument", line, col)}
	}
	_, isEmpty := args[0].(*SchemeEmpty)
	return &SchemeBool{Value: isEmpty}, nil
}

func builtinPairQ(args []SchemeValue, callExpr *ListExpr) (SchemeValue, error) {
	if len(args) != 1 {
		line, col := callExpr.Pos()
		return nil, &EvalError{Message: fmt.Sprintf("%d:%d: pair?: requires exactly 1 argument", line, col)}
	}
	_, isPair := args[0].(*SchemePair)
	return &SchemeBool{Value: isPair}, nil
}

func builtinList(args []SchemeValue, callExpr *ListExpr) (SchemeValue, error) {
	var result SchemeValue = &SchemeEmpty{}
	for i := len(args) - 1; i >= 0; i-- {
		result = &SchemePair{Car: args[i], Cdr: result}
	}
	return result, nil
}

func builtinLength(args []SchemeValue, callExpr *ListExpr) (SchemeValue, error) {
	if len(args) != 1 {
		line, col := callExpr.Pos()
		return nil, &EvalError{Message: fmt.Sprintf("%d:%d: length: requires exactly 1 argument", line, col)}
	}
	var count int64
	cur := args[0]
	for {
		switch c := cur.(type) {
		case *SchemePair:
			count++
			cur = c.Cdr
		case *SchemeEmpty:
			return &SchemeInt{Value: count}, nil
		default:
			line, col := callExpr.Pos()
			return nil, &EvalError{Message: fmt.Sprintf("%d:%d: length: expected list", line, col)}
		}
	}
}

func builtinAppend(args []SchemeValue, callExpr *ListExpr) (SchemeValue, error) {
	if len(args) == 0 {
		return &SchemeEmpty{}, nil
	}
	if len(args) == 1 {
		return args[0], nil
	}
	// Append all lists
	result := args[len(args)-1]
	for i := len(args) - 2; i >= 0; i-- {
		result = appendList(args[i], result)
	}
	return result, nil
}

func appendList(lst, tail SchemeValue) SchemeValue {
	switch l := lst.(type) {
	case *SchemeEmpty:
		return tail
	case *SchemePair:
		return &SchemePair{Car: l.Car, Cdr: appendList(l.Cdr, tail)}
	default:
		return tail
	}
}

func builtinNumberQ(args []SchemeValue, callExpr *ListExpr) (SchemeValue, error) {
	if len(args) != 1 {
		line, col := callExpr.Pos()
		return nil, &EvalError{Message: fmt.Sprintf("%d:%d: number?: requires exactly 1 argument", line, col)}
	}
	_, ok := args[0].(*SchemeInt)
	return &SchemeBool{Value: ok}, nil
}

func builtinStringQ(args []SchemeValue, callExpr *ListExpr) (SchemeValue, error) {
	if len(args) != 1 {
		line, col := callExpr.Pos()
		return nil, &EvalError{Message: fmt.Sprintf("%d:%d: string?: requires exactly 1 argument", line, col)}
	}
	_, ok := args[0].(*SchemeString)
	return &SchemeBool{Value: ok}, nil
}

func builtinBooleanQ(args []SchemeValue, callExpr *ListExpr) (SchemeValue, error) {
	if len(args) != 1 {
		line, col := callExpr.Pos()
		return nil, &EvalError{Message: fmt.Sprintf("%d:%d: boolean?: requires exactly 1 argument", line, col)}
	}
	_, ok := args[0].(*SchemeBool)
	return &SchemeBool{Value: ok}, nil
}

func builtinSymbolQ(args []SchemeValue, callExpr *ListExpr) (SchemeValue, error) {
	if len(args) != 1 {
		line, col := callExpr.Pos()
		return nil, &EvalError{Message: fmt.Sprintf("%d:%d: symbol?: requires exactly 1 argument", line, col)}
	}
	_, ok := args[0].(*SchemeSymbol)
	return &SchemeBool{Value: ok}, nil
}

func builtinCharQ(args []SchemeValue, callExpr *ListExpr) (SchemeValue, error) {
	if len(args) != 1 {
		line, col := callExpr.Pos()
		return nil, &EvalError{Message: fmt.Sprintf("%d:%d: char?: requires exactly 1 argument", line, col)}
	}
	_, ok := args[0].(*SchemeChar)
	return &SchemeBool{Value: ok}, nil
}

// EnvBuiltinProc is a builtin that needs access to the environment (for output).
type EnvBuiltinProc struct {
	Name string
	Fn   func(args []SchemeValue, callExpr *ListExpr, env *Env) (SchemeValue, error)
}

func (b *EnvBuiltinProc) String() string {
	return fmt.Sprintf("#<procedure %s>", b.Name)
}

func getOutputPort(env *Env) *strings.Builder {
	if v, ok := env.Get("$$output$$"); ok {
		if p, ok := v.(*outputPort); ok {
			return p.buf
		}
	}
	return nil
}

// displayValue writes a value in display format (no quotes on strings).
func displayValue(v SchemeValue) string {
	switch val := v.(type) {
	case *SchemeString:
		return val.Value
	case *SchemePair:
		return displayPair(val)
	case *SchemeChar:
		return string(val.Value)
	default:
		return v.String()
	}
}

func displayPair(p *SchemePair) string {
	var parts []string
	cur := SchemeValue(p)
	for {
		switch c := cur.(type) {
		case *SchemePair:
			parts = append(parts, displayValue(c.Car))
			cur = c.Cdr
		case *SchemeEmpty:
			return "(" + strings.Join(parts, " ") + ")"
		default:
			return "(" + strings.Join(parts, " ") + " . " + displayValue(cur) + ")"
		}
	}
}

func builtinDisplay(args []SchemeValue, callExpr *ListExpr, env *Env) (SchemeValue, error) {
	if len(args) != 1 {
		line, col := callExpr.Pos()
		return nil, &EvalError{Message: fmt.Sprintf("%d:%d: display: requires exactly 1 argument", line, col)}
	}
	if buf := getOutputPort(env); buf != nil {
		buf.WriteString(displayValue(args[0]))
	}
	return &SchemeVoid{}, nil
}

func builtinWrite(args []SchemeValue, callExpr *ListExpr, env *Env) (SchemeValue, error) {
	if len(args) != 1 {
		line, col := callExpr.Pos()
		return nil, &EvalError{Message: fmt.Sprintf("%d:%d: write: requires exactly 1 argument", line, col)}
	}
	if buf := getOutputPort(env); buf != nil {
		buf.WriteString(args[0].String())
	}
	return &SchemeVoid{}, nil
}

func builtinNewline(args []SchemeValue, callExpr *ListExpr, env *Env) (SchemeValue, error) {
	if len(args) != 0 {
		line, col := callExpr.Pos()
		return nil, &EvalError{Message: fmt.Sprintf("%d:%d: newline: requires 0 arguments", line, col)}
	}
	if buf := getOutputPort(env); buf != nil {
		buf.WriteString("\n")
	}
	return &SchemeVoid{}, nil
}

func builtinStringAppend(args []SchemeValue, callExpr *ListExpr) (SchemeValue, error) {
	var sb strings.Builder
	for _, a := range args {
		s, ok := a.(*SchemeString)
		if !ok {
			line, col := callExpr.Pos()
			return nil, &EvalError{Message: fmt.Sprintf("%d:%d: string-append: expected string", line, col)}
		}
		sb.WriteString(s.Value)
	}
	return &SchemeString{Value: sb.String()}, nil
}

func builtinStringLength(args []SchemeValue, callExpr *ListExpr) (SchemeValue, error) {
	if len(args) != 1 {
		line, col := callExpr.Pos()
		return nil, &EvalError{Message: fmt.Sprintf("%d:%d: string-length: requires exactly 1 argument", line, col)}
	}
	s, ok := args[0].(*SchemeString)
	if !ok {
		line, col := callExpr.Pos()
		return nil, &EvalError{Message: fmt.Sprintf("%d:%d: string-length: expected string", line, col)}
	}
	return &SchemeInt{Value: int64(len([]rune(s.Value)))}, nil
}

func builtinSubstring(args []SchemeValue, callExpr *ListExpr) (SchemeValue, error) {
	if len(args) != 3 {
		line, col := callExpr.Pos()
		return nil, &EvalError{Message: fmt.Sprintf("%d:%d: substring: requires exactly 3 arguments", line, col)}
	}
	s, ok := args[0].(*SchemeString)
	if !ok {
		line, col := callExpr.Pos()
		return nil, &EvalError{Message: fmt.Sprintf("%d:%d: substring: expected string", line, col)}
	}
	start, ok := args[1].(*SchemeInt)
	if !ok {
		line, col := callExpr.Pos()
		return nil, &EvalError{Message: fmt.Sprintf("%d:%d: substring: expected number", line, col)}
	}
	end, ok := args[2].(*SchemeInt)
	if !ok {
		line, col := callExpr.Pos()
		return nil, &EvalError{Message: fmt.Sprintf("%d:%d: substring: expected number", line, col)}
	}
	runes := []rune(s.Value)
	return &SchemeString{Value: string(runes[start.Value:end.Value])}, nil
}

func builtinStringToNumber(args []SchemeValue, callExpr *ListExpr) (SchemeValue, error) {
	if len(args) != 1 {
		line, col := callExpr.Pos()
		return nil, &EvalError{Message: fmt.Sprintf("%d:%d: string->number: requires exactly 1 argument", line, col)}
	}
	s, ok := args[0].(*SchemeString)
	if !ok {
		line, col := callExpr.Pos()
		return nil, &EvalError{Message: fmt.Sprintf("%d:%d: string->number: expected string", line, col)}
	}
	n, err := strconv.ParseInt(s.Value, 10, 64)
	if err != nil {
		return &SchemeBool{Value: false}, nil
	}
	return &SchemeInt{Value: n}, nil
}

func builtinNumberToString(args []SchemeValue, callExpr *ListExpr) (SchemeValue, error) {
	if len(args) != 1 {
		line, col := callExpr.Pos()
		return nil, &EvalError{Message: fmt.Sprintf("%d:%d: number->string: requires exactly 1 argument", line, col)}
	}
	n, ok := args[0].(*SchemeInt)
	if !ok {
		line, col := callExpr.Pos()
		return nil, &EvalError{Message: fmt.Sprintf("%d:%d: number->string: expected number", line, col)}
	}
	return &SchemeString{Value: strconv.FormatInt(n.Value, 10)}, nil
}

func builtinSymbolToString(args []SchemeValue, callExpr *ListExpr) (SchemeValue, error) {
	if len(args) != 1 {
		line, col := callExpr.Pos()
		return nil, &EvalError{Message: fmt.Sprintf("%d:%d: symbol->string: requires exactly 1 argument", line, col)}
	}
	s, ok := args[0].(*SchemeSymbol)
	if !ok {
		line, col := callExpr.Pos()
		return nil, &EvalError{Message: fmt.Sprintf("%d:%d: symbol->string: expected symbol", line, col)}
	}
	return &SchemeString{Value: s.Name}, nil
}

func builtinStringToSymbol(args []SchemeValue, callExpr *ListExpr) (SchemeValue, error) {
	if len(args) != 1 {
		line, col := callExpr.Pos()
		return nil, &EvalError{Message: fmt.Sprintf("%d:%d: string->symbol: requires exactly 1 argument", line, col)}
	}
	s, ok := args[0].(*SchemeString)
	if !ok {
		line, col := callExpr.Pos()
		return nil, &EvalError{Message: fmt.Sprintf("%d:%d: string->symbol: expected string", line, col)}
	}
	return &SchemeSymbol{Name: s.Value}, nil
}

func builtinStringRef(args []SchemeValue, callExpr *ListExpr) (SchemeValue, error) {
	if len(args) != 2 {
		line, col := callExpr.Pos()
		return nil, &EvalError{Message: fmt.Sprintf("%d:%d: string-ref: requires exactly 2 arguments", line, col)}
	}
	s, ok := args[0].(*SchemeString)
	if !ok {
		line, col := callExpr.Pos()
		return nil, &EvalError{Message: fmt.Sprintf("%d:%d: string-ref: expected string", line, col)}
	}
	idx, ok := args[1].(*SchemeInt)
	if !ok {
		line, col := callExpr.Pos()
		return nil, &EvalError{Message: fmt.Sprintf("%d:%d: string-ref: expected number", line, col)}
	}
	runes := []rune(s.Value)
	if idx.Value < 0 || idx.Value >= int64(len(runes)) {
		line, col := callExpr.Pos()
		return nil, &EvalError{Message: fmt.Sprintf("%d:%d: string-ref: index out of range", line, col)}
	}
	return &SchemeChar{Value: runes[idx.Value]}, nil
}

func builtinStringCopy(args []SchemeValue, callExpr *ListExpr) (SchemeValue, error) {
	if len(args) != 1 {
		line, col := callExpr.Pos()
		return nil, &EvalError{Message: fmt.Sprintf("%d:%d: string-copy: requires exactly 1 argument", line, col)}
	}
	s, ok := args[0].(*SchemeString)
	if !ok {
		line, col := callExpr.Pos()
		return nil, &EvalError{Message: fmt.Sprintf("%d:%d: string-copy: expected string", line, col)}
	}
	return &SchemeString{Value: s.Value}, nil
}

func builtinStringSet(args []SchemeValue, callExpr *ListExpr) (SchemeValue, error) {
	if len(args) != 3 {
		line, col := callExpr.Pos()
		return nil, &EvalError{Message: fmt.Sprintf("%d:%d: string-set!: requires exactly 3 arguments", line, col)}
	}
	s, ok := args[0].(*SchemeString)
	if !ok {
		line, col := callExpr.Pos()
		return nil, &EvalError{Message: fmt.Sprintf("%d:%d: string-set!: expected string", line, col)}
	}
	idx, ok := args[1].(*SchemeInt)
	if !ok {
		line, col := callExpr.Pos()
		return nil, &EvalError{Message: fmt.Sprintf("%d:%d: string-set!: expected number", line, col)}
	}
	ch, ok := args[2].(*SchemeChar)
	if !ok {
		line, col := callExpr.Pos()
		return nil, &EvalError{Message: fmt.Sprintf("%d:%d: string-set!: expected char", line, col)}
	}
	runes := []rune(s.Value)
	if idx.Value < 0 || idx.Value >= int64(len(runes)) {
		line, col := callExpr.Pos()
		return nil, &EvalError{Message: fmt.Sprintf("%d:%d: string-set!: index out of range", line, col)}
	}
	runes[idx.Value] = ch.Value
	s.Value = string(runes)
	return &SchemeVoid{}, nil
}
