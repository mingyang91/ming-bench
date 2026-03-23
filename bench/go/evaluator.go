package ming

import (
	"fmt"
)

// TopEnv creates a new top-level environment with builtins.
func TopEnv() *Env {
	env := NewEnv(nil)
	for name, proc := range builtins {
		env.Set(name, proc)
	}
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
	r, err := EvalStr(input)
	return r, "", err
}

// Eval evaluates an expression in the given environment.
func Eval(expr Expr, env *Env) (SchemeValue, error) {
	switch e := expr.(type) {
	case *NumberExpr:
		return &SchemeInt{Value: e.Value}, nil

	case *BoolExpr:
		return &SchemeBool{Value: e.Value}, nil

	case *StringExpr:
		return &SchemeString{Value: e.Value}, nil

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
				return evalAnd(e.Elements[1:], env)
			case "or":
				return evalOr(e.Elements[1:], env)
			case "define":
				return evalDefine(e, env)
			case "if":
				return evalIf(e, env)
			case "quote":
				if len(e.Elements) != 2 {
					line, col := e.Pos()
					return nil, &EvalError{Message: fmt.Sprintf("%d:%d: quote: requires exactly 1 argument", line, col)}
				}
				return quoteExpr(e.Elements[1]), nil
			case "lambda":
				return evalLambda(e, env)
			case "let":
				return evalLet(e, env)
			case "begin":
				return evalBegin(e.Elements[1:], env)
			case "cond":
				return evalCond(e, env)
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
		case *Lambda:
			return applyLambda(fn, args, e)
		}

		line, col := e.Pos()
		return nil, &EvalError{Message: fmt.Sprintf("%d:%d: not a procedure", line, col)}

	default:
		return nil, &EvalError{Message: "unknown expression type"}
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

func evalIf(e *ListExpr, env *Env) (SchemeValue, error) {
	if len(e.Elements) < 3 || len(e.Elements) > 4 {
		line, col := e.Pos()
		return nil, &EvalError{Message: fmt.Sprintf("%d:%d: if: bad syntax", line, col)}
	}

	cond, err := Eval(e.Elements[1], env)
	if err != nil {
		return nil, err
	}

	if isTruthy(cond) {
		return Eval(e.Elements[2], env)
	}
	if len(e.Elements) == 4 {
		return Eval(e.Elements[3], env)
	}
	return &SchemeVoid{}, nil
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

func applyLambda(fn *Lambda, args []SchemeValue, callExpr *ListExpr) (SchemeValue, error) {
	if len(args) != len(fn.Params) {
		line, col := callExpr.Pos()
		return nil, &EvalError{Message: fmt.Sprintf("%d:%d: wrong number of arguments: expected %d, got %d", line, col, len(fn.Params), len(args))}
	}

	localEnv := NewEnv(fn.Env)
	for i, p := range fn.Params {
		localEnv.Set(p, args[i])
	}

	var result SchemeValue
	var err error
	for _, bodyExpr := range fn.Body {
		result, err = Eval(bodyExpr, localEnv)
		if err != nil {
			return nil, err
		}
	}
	return result, nil
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

func evalAnd(exprs []Expr, env *Env) (SchemeValue, error) {
	var result SchemeValue = &SchemeBool{Value: true}
	for _, expr := range exprs {
		var err error
		result, err = Eval(expr, env)
		if err != nil {
			return nil, err
		}
		if !isTruthy(result) {
			return result, nil
		}
	}
	return result, nil
}

func evalOr(exprs []Expr, env *Env) (SchemeValue, error) {
	var result SchemeValue = &SchemeBool{Value: false}
	for _, expr := range exprs {
		var err error
		result, err = Eval(expr, env)
		if err != nil {
			return nil, err
		}
		if isTruthy(result) {
			return result, nil
		}
	}
	return result, nil
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

func evalLet(e *ListExpr, env *Env) (SchemeValue, error) {
	if len(e.Elements) < 3 {
		line, col := e.Pos()
		return nil, &EvalError{Message: fmt.Sprintf("%d:%d: let: bad syntax", line, col)}
	}

	// Named let: (let name ((var init) ...) body...)
	if sym, ok := e.Elements[1].(*SymbolExpr); ok {
		if len(e.Elements) < 4 {
			line, col := e.Pos()
			return nil, &EvalError{Message: fmt.Sprintf("%d:%d: let: bad syntax", line, col)}
		}
		bindingsList, ok := e.Elements[2].(*ListExpr)
		if !ok {
			line, col := e.Pos()
			return nil, &EvalError{Message: fmt.Sprintf("%d:%d: let: bad syntax", line, col)}
		}
		params := make([]string, len(bindingsList.Elements))
		inits := make([]SchemeValue, len(bindingsList.Elements))
		for i, b := range bindingsList.Elements {
			pair, ok := b.(*ListExpr)
			if !ok || len(pair.Elements) != 2 {
				line, col := e.Pos()
				return nil, &EvalError{Message: fmt.Sprintf("%d:%d: let: bad syntax", line, col)}
			}
			ps, ok := pair.Elements[0].(*SymbolExpr)
			if !ok {
				line, col := e.Pos()
				return nil, &EvalError{Message: fmt.Sprintf("%d:%d: let: bad syntax", line, col)}
			}
			params[i] = ps.Name
			val, err := Eval(pair.Elements[1], env)
			if err != nil {
				return nil, err
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
		var result SchemeValue
		var err error
		for _, bodyExpr := range lam.Body {
			result, err = Eval(bodyExpr, localEnv)
			if err != nil {
				return nil, err
			}
		}
		return result, nil
	}

	// Regular let: (let ((var init) ...) body...)
	bindingsList, ok := e.Elements[1].(*ListExpr)
	if !ok {
		line, col := e.Pos()
		return nil, &EvalError{Message: fmt.Sprintf("%d:%d: let: bad syntax", line, col)}
	}

	localEnv := NewEnv(env)
	for _, b := range bindingsList.Elements {
		pair, ok := b.(*ListExpr)
		if !ok || len(pair.Elements) != 2 {
			line, col := e.Pos()
			return nil, &EvalError{Message: fmt.Sprintf("%d:%d: let: bad syntax", line, col)}
		}
		sym, ok := pair.Elements[0].(*SymbolExpr)
		if !ok {
			line, col := e.Pos()
			return nil, &EvalError{Message: fmt.Sprintf("%d:%d: let: bad syntax", line, col)}
		}
		val, err := Eval(pair.Elements[1], env)
		if err != nil {
			return nil, err
		}
		localEnv.Set(sym.Name, val)
	}

	var result SchemeValue
	var err error
	for _, bodyExpr := range e.Elements[2:] {
		result, err = Eval(bodyExpr, localEnv)
		if err != nil {
			return nil, err
		}
	}
	return result, nil
}

func evalBegin(exprs []Expr, env *Env) (SchemeValue, error) {
	var result SchemeValue = &SchemeVoid{}
	var err error
	for _, expr := range exprs {
		result, err = Eval(expr, env)
		if err != nil {
			return nil, err
		}
	}
	return result, nil
}

func evalCond(e *ListExpr, env *Env) (SchemeValue, error) {
	for _, clause := range e.Elements[1:] {
		cl, ok := clause.(*ListExpr)
		if !ok || len(cl.Elements) < 2 {
			line, col := e.Pos()
			return nil, &EvalError{Message: fmt.Sprintf("%d:%d: cond: bad syntax", line, col)}
		}
		// Check for else clause
		if sym, ok := cl.Elements[0].(*SymbolExpr); ok && sym.Name == "else" {
			return evalBegin(cl.Elements[1:], env)
		}
		cond, err := Eval(cl.Elements[0], env)
		if err != nil {
			return nil, err
		}
		if isTruthy(cond) {
			return evalBegin(cl.Elements[1:], env)
		}
	}
	return &SchemeVoid{}, nil
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
