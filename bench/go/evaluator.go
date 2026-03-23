package ming

import (
	"fmt"
	"strconv"
	"strings"
)

// evalErr creates an EvalError with position info from an expression.
func evalErr(expr *Value, msg string) *EvalError {
	return &EvalError{Message: msg, Line: expr.Line, Col: expr.Col}
}

func evalErrf(expr *Value, format string, args ...interface{}) *EvalError {
	return evalErr(expr, fmt.Sprintf(format, args...))
}

// Environment holds variable bindings.
type Env struct {
	bindings map[string]*Value
	parent   *Env
}

func newEnv(parent *Env) *Env {
	return &Env{bindings: make(map[string]*Value), parent: parent}
}

func (e *Env) get(name string) (*Value, bool) {
	if v, ok := e.bindings[name]; ok {
		return v, true
	}
	if e.parent != nil {
		return e.parent.get(name)
	}
	return nil, false
}

func (e *Env) set(name string, v *Value) {
	e.bindings[name] = v
}

// listToSlice converts a Scheme list to a Go slice.
func listToSlice(v *Value) []*Value {
	var result []*Value
	cur := v
	for cur.Type == TypePair {
		result = append(result, cur.Car)
		cur = cur.Cdr
	}
	return result
}

func makeBuiltin(name string, fn func([]*Value) (*Value, error)) *Value {
	return &Value{Type: TypeBuiltin, Str: name, BuiltinFunc: fn}
}

func makeLambda(params []string, body []*Value, closure *Env) *Value {
	return &Value{Type: TypeLambda, Params: params, Body: body, Closure: closure}
}

// eval evaluates a single expression in the given environment.
func eval(expr *Value, env *Env) (*Value, error) {
	switch expr.Type {
	case TypeInteger, TypeBoolean, TypeString:
		return expr, nil
	case TypeSymbol:
		v, ok := env.get(expr.Str)
		if !ok {
			return nil, evalErrf(expr, "unbound variable: %s", expr.Str)
		}
		return v, nil
	case TypePair:
		return evalList(expr, env)
	case TypeNull:
		return nil, evalErr(expr, "empty application")
	default:
		return nil, evalErr(expr, "cannot evaluate")
	}
}

func evalList(expr *Value, env *Env) (*Value, error) {
	head := expr.Car
	args := expr.Cdr

	// Special forms
	if head.Type == TypeSymbol {
		switch head.Str {
		case "and":
			return evalAnd(args, env)
		case "or":
			return evalOr(args, env)
		case "define":
			return evalDefine(args, env, expr)
		case "if":
			return evalIf(args, env, expr)
		case "quote":
			return args.Car, nil
		case "lambda":
			return evalLambda(args, env)
		case "let":
			return evalLet(args, env)
		case "begin":
			return evalBegin(args, env)
		case "cond":
			return evalCond(args, env)
		}
	}

	// Function application
	fn, err := eval(head, env)
	if err != nil {
		return nil, err
	}

	// Evaluate arguments
	argList := listToSlice(args)
	evalArgs := make([]*Value, len(argList))
	for i, a := range argList {
		v, err := eval(a, env)
		if err != nil {
			return nil, err
		}
		evalArgs[i] = v
	}

	result, err := applyProc(fn, evalArgs)
	if err != nil {
		// Attach position from the call site if not already present
		if ee, ok := err.(*EvalError); ok && ee.Line == 0 {
			ee.Line = expr.Line
			ee.Col = expr.Col
		}
		return nil, err
	}
	return result, nil
}

func evalDefine(args *Value, env *Env, expr *Value) (*Value, error) {
	if args.Type == TypeNull {
		return nil, evalErr(expr, "bad define syntax")
	}
	target := args.Car
	if target.Type == TypeSymbol {
		// (define x expr)
		val, err := eval(args.Cdr.Car, env)
		if err != nil {
			return nil, err
		}
		env.set(target.Str, val)
		return voidValue, nil
	}
	if target.Type == TypePair {
		// (define (f params...) body...)
		name := target.Car.Str
		paramList := listToSlice(target.Cdr)
		params := make([]string, len(paramList))
		for i, p := range paramList {
			params[i] = p.Str
		}
		body := listToSlice(args.Cdr)
		env.set(name, makeLambda(params, body, env))
		return voidValue, nil
	}
	return nil, evalErr(expr, "bad define syntax")
}

func evalIf(args *Value, env *Env, expr *Value) (*Value, error) {
	if args.Type == TypeNull {
		return nil, evalErr(expr, "bad if syntax: missing condition")
	}
	cond, err := eval(args.Car, env)
	if err != nil {
		return nil, err
	}
	if isTruthy(cond) {
		return eval(args.Cdr.Car, env)
	}
	// else branch (if present)
	if args.Cdr.Cdr.Type == TypePair {
		return eval(args.Cdr.Cdr.Car, env)
	}
	return voidValue, nil
}

func evalLambda(args *Value, env *Env) (*Value, error) {
	paramList := listToSlice(args.Car)
	params := make([]string, len(paramList))
	for i, p := range paramList {
		params[i] = p.Str
	}
	body := listToSlice(args.Cdr)
	return makeLambda(params, body, env), nil
}

func evalAnd(args *Value, env *Env) (*Value, error) {
	result := makeBool(true)
	cur := args
	for cur.Type == TypePair {
		v, err := eval(cur.Car, env)
		if err != nil {
			return nil, err
		}
		if !isTruthy(v) {
			return v, nil
		}
		result = v
		cur = cur.Cdr
	}
	return result, nil
}

func evalOr(args *Value, env *Env) (*Value, error) {
	result := makeBool(false)
	cur := args
	for cur.Type == TypePair {
		v, err := eval(cur.Car, env)
		if err != nil {
			return nil, err
		}
		if isTruthy(v) {
			return v, nil
		}
		result = v
		cur = cur.Cdr
	}
	return result, nil
}

func evalLet(args *Value, env *Env) (*Value, error) {
	// Named let: (let name ((var init) ...) body ...)
	if args.Car.Type == TypeSymbol {
		name := args.Car.Str
		bindings := args.Cdr.Car
		body := args.Cdr.Cdr

		var params []string
		var inits []*Value
		cur := bindings
		for cur.Type == TypePair {
			b := cur.Car
			params = append(params, b.Car.Str)
			val, err := eval(b.Cdr.Car, env)
			if err != nil {
				return nil, err
			}
			inits = append(inits, val)
			cur = cur.Cdr
		}
		bodySlice := listToSlice(body)
		localEnv := newEnv(env)
		lambda := makeLambda(params, bodySlice, localEnv)
		localEnv.set(name, lambda)
		return applyProc(lambda, inits)
	}

	// Regular let: (let ((var init) ...) body ...)
	bindings := args.Car
	body := args.Cdr
	localEnv := newEnv(env)
	cur := bindings
	for cur.Type == TypePair {
		binding := cur.Car
		name := binding.Car.Str
		val, err := eval(binding.Cdr.Car, env)
		if err != nil {
			return nil, err
		}
		localEnv.set(name, val)
		cur = cur.Cdr
	}
	var result *Value
	var err error
	bodyCur := body
	for bodyCur.Type == TypePair {
		result, err = eval(bodyCur.Car, localEnv)
		if err != nil {
			return nil, err
		}
		bodyCur = bodyCur.Cdr
	}
	return result, nil
}

func evalBegin(args *Value, env *Env) (*Value, error) {
	var result *Value = voidValue
	var err error
	cur := args
	for cur.Type == TypePair {
		result, err = eval(cur.Car, env)
		if err != nil {
			return nil, err
		}
		cur = cur.Cdr
	}
	return result, nil
}

func evalCond(args *Value, env *Env) (*Value, error) {
	cur := args
	for cur.Type == TypePair {
		clause := cur.Car
		test := clause.Car
		// else clause
		if test.Type == TypeSymbol && test.Str == "else" {
			return evalBegin(clause.Cdr, env)
		}
		val, err := eval(test, env)
		if err != nil {
			return nil, err
		}
		if isTruthy(val) {
			if clause.Cdr.Type == TypeNull {
				return val, nil
			}
			return evalBegin(clause.Cdr, env)
		}
		cur = cur.Cdr
	}
	return voidValue, nil
}

// applyProc calls a procedure (builtin or lambda) with evaluated arguments.
func applyProc(fn *Value, args []*Value) (*Value, error) {
	switch fn.Type {
	case TypeBuiltin:
		return fn.BuiltinFunc(args)
	case TypeLambda:
		if len(args) != len(fn.Params) {
			return nil, &EvalError{Message: fmt.Sprintf("expected %d arguments, got %d", len(fn.Params), len(args))}
		}
		localEnv := newEnv(fn.Closure)
		for i, p := range fn.Params {
			localEnv.set(p, args[i])
		}
		var result *Value
		var err error
		for _, bodyExpr := range fn.Body {
			result, err = eval(bodyExpr, localEnv)
			if err != nil {
				return nil, err
			}
		}
		return result, nil
	default:
		return nil, &EvalError{Message: fmt.Sprintf("not a procedure: %s", fn.Display())}
	}
}

func applyBuiltin(name string, args []*Value) (*Value, error) {
	switch name {
	case "+":
		sum := int64(0)
		for _, a := range args {
			if a.Type != TypeInteger {
				return nil, &EvalError{Message: "'+' expects numbers"}
			}
			sum += a.Int
		}
		return makeInt(sum), nil

	case "-":
		if len(args) == 0 {
			return nil, &EvalError{Message: "'-' expects at least one argument"}
		}
		if args[0].Type != TypeInteger {
			return nil, &EvalError{Message: "'-' expects numbers"}
		}
		if len(args) == 1 {
			return makeInt(-args[0].Int), nil
		}
		result := args[0].Int
		for _, a := range args[1:] {
			if a.Type != TypeInteger {
				return nil, &EvalError{Message: "'-' expects numbers"}
			}
			result -= a.Int
		}
		return makeInt(result), nil

	case "*":
		product := int64(1)
		for _, a := range args {
			if a.Type != TypeInteger {
				return nil, &EvalError{Message: "'*' expects numbers"}
			}
			product *= a.Int
		}
		return makeInt(product), nil

	case "/":
		if len(args) < 2 {
			return nil, &EvalError{Message: "'/' expects at least two arguments"}
		}
		if args[0].Type != TypeInteger {
			return nil, &EvalError{Message: "'/' expects numbers"}
		}
		result := args[0].Int
		for _, a := range args[1:] {
			if a.Type != TypeInteger {
				return nil, &EvalError{Message: "'/' expects numbers"}
			}
			if a.Int == 0 {
				return nil, &EvalError{Message: "division by zero"}
			}
			result /= a.Int
		}
		return makeInt(result), nil

	case "<":
		return compareInts(args, func(a, b int64) bool { return a < b })
	case ">":
		return compareInts(args, func(a, b int64) bool { return a > b })
	case "=":
		return compareInts(args, func(a, b int64) bool { return a == b })
	case "<=":
		return compareInts(args, func(a, b int64) bool { return a <= b })
	case ">=":
		return compareInts(args, func(a, b int64) bool { return a >= b })

	case "not":
		if len(args) != 1 {
			return nil, &EvalError{Message: "'not' expects exactly one argument"}
		}
		return makeBool(!isTruthy(args[0])), nil

	default:
		return nil, &EvalError{Message: fmt.Sprintf("unknown procedure: %s", name)}
	}
}

func compareInts(args []*Value, cmp func(int64, int64) bool) (*Value, error) {
	if len(args) < 2 {
		return nil, &EvalError{Message: "comparison expects at least two arguments"}
	}
	for _, a := range args {
		if a.Type != TypeInteger {
			return nil, &EvalError{Message: "comparison expects numbers"}
		}
	}
	for i := 0; i < len(args)-1; i++ {
		if !cmp(args[i].Int, args[i+1].Int) {
			return makeBool(false), nil
		}
	}
	return makeBool(true), nil
}

// builtinEnv creates the top-level environment with builtin procedure names.
func builtinEnv(out *strings.Builder) *Env {
	env := newEnv(nil)
	builtinDefs := map[string]func([]*Value) (*Value, error){
		"+":   func(args []*Value) (*Value, error) { return applyBuiltin("+", args) },
		"-":   func(args []*Value) (*Value, error) { return applyBuiltin("-", args) },
		"*":   func(args []*Value) (*Value, error) { return applyBuiltin("*", args) },
		"/":   func(args []*Value) (*Value, error) { return applyBuiltin("/", args) },
		"<":   func(args []*Value) (*Value, error) { return applyBuiltin("<", args) },
		">":   func(args []*Value) (*Value, error) { return applyBuiltin(">", args) },
		"=":   func(args []*Value) (*Value, error) { return applyBuiltin("=", args) },
		"<=":  func(args []*Value) (*Value, error) { return applyBuiltin("<=", args) },
		">=":  func(args []*Value) (*Value, error) { return applyBuiltin(">=", args) },
		"not": func(args []*Value) (*Value, error) { return applyBuiltin("not", args) },
		"cons": func(args []*Value) (*Value, error) {
			if len(args) != 2 {
				return nil, &EvalError{Message: "'cons' expects exactly two arguments"}
			}
			return makePair(args[0], args[1]), nil
		},
		"car": func(args []*Value) (*Value, error) {
			if len(args) != 1 || args[0].Type != TypePair {
				return nil, &EvalError{Message: "'car' expects a pair"}
			}
			return args[0].Car, nil
		},
		"cdr": func(args []*Value) (*Value, error) {
			if len(args) != 1 || args[0].Type != TypePair {
				return nil, &EvalError{Message: "'cdr' expects a pair"}
			}
			return args[0].Cdr, nil
		},
		"null?": func(args []*Value) (*Value, error) {
			if len(args) != 1 {
				return nil, &EvalError{Message: "'null?' expects exactly one argument"}
			}
			return makeBool(args[0].Type == TypeNull), nil
		},
		"list": func(args []*Value) (*Value, error) {
			result := nullValue
			for i := len(args) - 1; i >= 0; i-- {
				result = makePair(args[i], result)
			}
			return result, nil
		},
		"length": func(args []*Value) (*Value, error) {
			if len(args) != 1 {
				return nil, &EvalError{Message: "'length' expects exactly one argument"}
			}
			count := int64(0)
			cur := args[0]
			for cur.Type == TypePair {
				count++
				cur = cur.Cdr
			}
			return makeInt(count), nil
		},
		"string?": func(args []*Value) (*Value, error) {
			if len(args) != 1 {
				return nil, &EvalError{Message: "'string?' expects exactly one argument"}
			}
			return makeBool(args[0].Type == TypeString), nil
		},
		"number?": func(args []*Value) (*Value, error) {
			if len(args) != 1 {
				return nil, &EvalError{Message: "'number?' expects exactly one argument"}
			}
			return makeBool(args[0].Type == TypeInteger), nil
		},
		"boolean?": func(args []*Value) (*Value, error) {
			if len(args) != 1 {
				return nil, &EvalError{Message: "'boolean?' expects exactly one argument"}
			}
			return makeBool(args[0].Type == TypeBoolean), nil
		},
		"pair?": func(args []*Value) (*Value, error) {
			if len(args) != 1 {
				return nil, &EvalError{Message: "'pair?' expects exactly one argument"}
			}
			return makeBool(args[0].Type == TypePair), nil
		},
		"append": func(args []*Value) (*Value, error) {
			if len(args) == 0 {
				return nullValue, nil
			}
			// append all lists together
			result := args[len(args)-1]
			for i := len(args) - 2; i >= 0; i-- {
				cur := args[i]
				// collect elements of this list
				var elems []*Value
				for cur.Type == TypePair {
					elems = append(elems, cur.Car)
					cur = cur.Cdr
				}
				for j := len(elems) - 1; j >= 0; j-- {
					result = makePair(elems[j], result)
				}
			}
			return result, nil
		},
		"symbol?": func(args []*Value) (*Value, error) {
			if len(args) != 1 {
				return nil, &EvalError{Message: "'symbol?' expects exactly one argument"}
			}
			return makeBool(args[0].Type == TypeSymbol), nil
		},
		"display": func(args []*Value) (*Value, error) {
			if len(args) != 1 {
				return nil, &EvalError{Message: "'display' expects exactly one argument"}
			}
			out.WriteString(args[0].DisplayPlain())
			return voidValue, nil
		},
		"write": func(args []*Value) (*Value, error) {
			if len(args) != 1 {
				return nil, &EvalError{Message: "'write' expects exactly one argument"}
			}
			out.WriteString(args[0].WriteRepr())
			return voidValue, nil
		},
		"newline": func(args []*Value) (*Value, error) {
			out.WriteString("\n")
			return voidValue, nil
		},
		"string-append": func(args []*Value) (*Value, error) {
			var sb strings.Builder
			for _, a := range args {
				if a.Type != TypeString {
					return nil, &EvalError{Message: "'string-append' expects strings"}
				}
				sb.WriteString(a.Str)
			}
			return makeString(sb.String()), nil
		},
		"string-length": func(args []*Value) (*Value, error) {
			if len(args) != 1 || args[0].Type != TypeString {
				return nil, &EvalError{Message: "'string-length' expects a string"}
			}
			return makeInt(int64(len([]rune(args[0].Str)))), nil
		},
		"substring": func(args []*Value) (*Value, error) {
			if len(args) != 3 || args[0].Type != TypeString || args[1].Type != TypeInteger || args[2].Type != TypeInteger {
				return nil, &EvalError{Message: "'substring' expects a string and two integers"}
			}
			runes := []rune(args[0].Str)
			start := int(args[1].Int)
			end := int(args[2].Int)
			if start < 0 || end > len(runes) || start > end {
				return nil, &EvalError{Message: "'substring' index out of range"}
			}
			return makeString(string(runes[start:end])), nil
		},
		"string->number": func(args []*Value) (*Value, error) {
			if len(args) != 1 || args[0].Type != TypeString {
				return nil, &EvalError{Message: "'string->number' expects a string"}
			}
			n, err := strconv.ParseInt(args[0].Str, 10, 64)
			if err != nil {
				return makeBool(false), nil
			}
			return makeInt(n), nil
		},
		"number->string": func(args []*Value) (*Value, error) {
			if len(args) != 1 || args[0].Type != TypeInteger {
				return nil, &EvalError{Message: "'number->string' expects a number"}
			}
			return makeString(strconv.FormatInt(args[0].Int, 10)), nil
		},
		"symbol->string": func(args []*Value) (*Value, error) {
			if len(args) != 1 || args[0].Type != TypeSymbol {
				return nil, &EvalError{Message: "'symbol->string' expects a symbol"}
			}
			return makeString(args[0].Str), nil
		},
		"string->symbol": func(args []*Value) (*Value, error) {
			if len(args) != 1 || args[0].Type != TypeString {
				return nil, &EvalError{Message: "'string->symbol' expects a string"}
			}
			return makeSymbol(args[0].Str), nil
		},
		"string-ref": func(args []*Value) (*Value, error) {
			if len(args) != 2 || args[0].Type != TypeString || args[1].Type != TypeInteger {
				return nil, &EvalError{Message: "'string-ref' expects a string and an integer"}
			}
			runes := []rune(args[0].Str)
			idx := int(args[1].Int)
			if idx < 0 || idx >= len(runes) {
				return nil, &EvalError{Message: "'string-ref' index out of range"}
			}
			return makeChar(runes[idx]), nil
		},
		"char?": func(args []*Value) (*Value, error) {
			if len(args) != 1 {
				return nil, &EvalError{Message: "'char?' expects exactly one argument"}
			}
			return makeBool(args[0].Type == TypeChar), nil
		},
	}
	for name, fn := range builtinDefs {
		env.set(name, makeBuiltin(name, fn))
	}
	return env
}

// EvalStr evaluates one or more Scheme expressions and returns the string
// representation of the last result.
func EvalStr(input string) (string, error) {
	r, _, err := evalWithOutput(input)
	return r, err
}

// EvalStrWithOutput evaluates Scheme expressions and returns both the result
// string and any captured output from display/write/newline.
func EvalStrWithOutput(input string) (result string, output string, err error) {
	return evalWithOutput(input)
}

func evalWithOutput(input string) (string, string, error) {
	exprs, err := readAll(input)
	if err != nil {
		if ee, ok := err.(*EvalError); ok {
			return "", "", ee
		}
		return "", "", &EvalError{Message: err.Error()}
	}
	if len(exprs) == 0 {
		return "", "", &EvalError{Message: "no expressions"}
	}

	var out strings.Builder
	env := builtinEnv(&out)
	var result *Value
	for _, expr := range exprs {
		result, err = eval(expr, env)
		if err != nil {
			return "", "", err
		}
	}

	r := ""
	if result.Type != TypeVoid {
		r = result.Display()
	}
	return r, out.String(), nil
}
