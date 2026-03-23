package ming

import "fmt"

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
			return nil, &EvalError{Message: fmt.Sprintf("unbound variable: %s", expr.Str)}
		}
		return v, nil
	case TypePair:
		return evalList(expr, env)
	case TypeNull:
		return nil, &EvalError{Message: "empty application"}
	default:
		return nil, &EvalError{Message: "cannot evaluate"}
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
			return evalDefine(args, env)
		case "if":
			return evalIf(args, env)
		case "quote":
			return args.Car, nil
		case "lambda":
			return evalLambda(args, env)
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

	return applyProc(fn, evalArgs)
}

func evalDefine(args *Value, env *Env) (*Value, error) {
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
	return nil, &EvalError{Message: "bad define syntax"}
}

func evalIf(args *Value, env *Env) (*Value, error) {
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
func builtinEnv() *Env {
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
	}
	for name, fn := range builtinDefs {
		env.set(name, makeBuiltin(name, fn))
	}
	return env
}

// EvalStr evaluates one or more Scheme expressions and returns the string
// representation of the last result.
func EvalStr(input string) (string, error) {
	exprs, err := readAll(input)
	if err != nil {
		return "", &EvalError{Message: err.Error()}
	}
	if len(exprs) == 0 {
		return "", &EvalError{Message: "no expressions"}
	}

	env := builtinEnv()
	var result *Value
	for _, expr := range exprs {
		result, err = eval(expr, env)
		if err != nil {
			return "", err
		}
	}

	if result.Type == TypeVoid {
		return "", nil
	}
	return result.Display(), nil
}

// EvalStrWithOutput evaluates Scheme expressions and returns both the result
// string and any captured output from display/write/newline.
func EvalStrWithOutput(input string) (result string, output string, err error) {
	r, err := EvalStr(input)
	return r, "", err
}
