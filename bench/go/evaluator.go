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

func makeGlobalEnv() *Env {
	env := newEnv(nil)
	return env
}

// listToSlice converts a scheme list to a Go slice.
func listToSlice(v *Value) []*Value {
	var result []*Value
	cur := v
	for cur.Type == TypePair {
		result = append(result, cur.Car)
		cur = cur.Cdr
	}
	return result
}

func eval(expr *Value, env *Env) (*Value, error) {
	switch expr.Type {
	case TypeInteger, TypeBoolean, TypeString:
		return expr, nil
	case TypeSymbol:
		v, ok := env.get(expr.StrVal)
		if !ok {
			return nil, &EvalError{Message: fmt.Sprintf("unbound variable: %s", expr.StrVal)}
		}
		return v, nil
	case TypeNull:
		return nil, &EvalError{Message: "empty application"}
	case TypePair:
		return evalList(expr, env)
	}
	return nil, &EvalError{Message: "unknown expression type"}
}

// isBuiltin checks if a symbol name is a built-in procedure.
func isBuiltin(name string) bool {
	switch name {
	case "+", "-", "*", "/", "<", ">", "=", "<=", ">=", "not":
		return true
	}
	return false
}

func evalList(expr *Value, env *Env) (*Value, error) {
	head := expr.Car

	// Special forms
	if head.Type == TypeSymbol {
		switch head.StrVal {
		case "and":
			return evalAnd(expr.Cdr, env)
		case "or":
			return evalOr(expr.Cdr, env)
		case "define":
			return evalDefine(expr.Cdr, env)
		case "if":
			return evalIf(expr.Cdr, env)
		case "quote":
			return expr.Cdr.Car, nil
		case "lambda":
			return evalLambda(expr.Cdr, env)
		}
	}

	// Evaluate arguments
	args := listToSlice(expr.Cdr)
	evaledArgs := make([]*Value, len(args))
	for i, a := range args {
		v, err := eval(a, env)
		if err != nil {
			return nil, err
		}
		evaledArgs[i] = v
	}

	// Built-in procedure call
	if head.Type == TypeSymbol && isBuiltin(head.StrVal) {
		return applyBuiltin(head, evaledArgs)
	}

	// Evaluate head for user-defined procedures
	fn, err := eval(head, env)
	if err != nil {
		return nil, err
	}

	// Lambda application
	if fn.Type == TypeLambda {
		return applyLambda(fn, evaledArgs)
	}

	return nil, &EvalError{Message: fmt.Sprintf("not a procedure: %s", fn.Display())}
}

func evalDefine(args *Value, env *Env) (*Value, error) {
	first := args.Car
	if first.Type == TypeSymbol {
		// (define x expr)
		val, err := eval(args.Cdr.Car, env)
		if err != nil {
			return nil, err
		}
		env.set(first.StrVal, val)
		return Void, nil
	}
	if first.Type == TypePair {
		// (define (f params...) body...)
		name := first.Car.StrVal
		paramsList := listToSlice(first.Cdr)
		params := make([]string, len(paramsList))
		for i, p := range paramsList {
			params[i] = p.StrVal
		}
		body := listToSlice(args.Cdr)
		fn := &Value{
			Type:       TypeLambda,
			Params:     params,
			Body:       body,
			ClosureEnv: env,
		}
		env.set(name, fn)
		return Void, nil
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
	// else branch
	if args.Cdr.Cdr.Type != TypeNull {
		return eval(args.Cdr.Cdr.Car, env)
	}
	return Void, nil
}

func evalLambda(args *Value, env *Env) (*Value, error) {
	paramsList := listToSlice(args.Car)
	params := make([]string, len(paramsList))
	for i, p := range paramsList {
		params[i] = p.StrVal
	}
	body := listToSlice(args.Cdr)
	return &Value{
		Type:       TypeLambda,
		Params:     params,
		Body:       body,
		ClosureEnv: env,
	}, nil
}

func applyLambda(fn *Value, args []*Value) (*Value, error) {
	localEnv := newEnv(fn.ClosureEnv)
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
}

func evalAnd(args *Value, env *Env) (*Value, error) {
	if args.Type == TypeNull {
		return NewBool(true), nil
	}
	items := listToSlice(args)
	var result *Value = NewBool(true)
	for _, item := range items {
		v, err := eval(item, env)
		if err != nil {
			return nil, err
		}
		if v.Type == TypeBoolean && !v.BoolVal {
			return v, nil // short-circuit
		}
		result = v
	}
	return result, nil
}

func evalOr(args *Value, env *Env) (*Value, error) {
	if args.Type == TypeNull {
		return NewBool(false), nil
	}
	items := listToSlice(args)
	for _, item := range items {
		v, err := eval(item, env)
		if err != nil {
			return nil, err
		}
		if isTruthy(v) {
			return v, nil
		}
	}
	return NewBool(false), nil
}

func isTruthy(v *Value) bool {
	return !(v.Type == TypeBoolean && !v.BoolVal)
}

func applyBuiltin(head *Value, args []*Value) (*Value, error) {
	if head.Type != TypeSymbol {
		return nil, &EvalError{Message: fmt.Sprintf("not a procedure: %s", head.Display())}
	}
	name := head.StrVal

	switch name {
	case "+":
		sum := int64(0)
		for _, a := range args {
			if a.Type != TypeInteger {
				return nil, &EvalError{Message: "expected number"}
			}
			sum += a.IntVal
		}
		return NewInt(sum), nil

	case "-":
		if len(args) == 0 {
			return nil, &EvalError{Message: "- requires at least one argument"}
		}
		if args[0].Type != TypeInteger {
			return nil, &EvalError{Message: "expected number"}
		}
		if len(args) == 1 {
			return NewInt(-args[0].IntVal), nil
		}
		result := args[0].IntVal
		for _, a := range args[1:] {
			if a.Type != TypeInteger {
				return nil, &EvalError{Message: "expected number"}
			}
			result -= a.IntVal
		}
		return NewInt(result), nil

	case "*":
		product := int64(1)
		for _, a := range args {
			if a.Type != TypeInteger {
				return nil, &EvalError{Message: "expected number"}
			}
			product *= a.IntVal
		}
		return NewInt(product), nil

	case "/":
		if len(args) < 2 {
			return nil, &EvalError{Message: "/ requires at least two arguments"}
		}
		if args[0].Type != TypeInteger {
			return nil, &EvalError{Message: "expected number"}
		}
		result := args[0].IntVal
		for _, a := range args[1:] {
			if a.Type != TypeInteger {
				return nil, &EvalError{Message: "expected number"}
			}
			if a.IntVal == 0 {
				return nil, &EvalError{Message: "division by zero"}
			}
			result /= a.IntVal
		}
		return NewInt(result), nil

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
			return nil, &EvalError{Message: "not requires exactly one argument"}
		}
		return NewBool(!isTruthy(args[0])), nil
	}

	return nil, &EvalError{Message: fmt.Sprintf("unbound variable: %s", name)}
}

func compareInts(args []*Value, cmp func(int64, int64) bool) (*Value, error) {
	if len(args) < 2 {
		return nil, &EvalError{Message: "comparison requires at least two arguments"}
	}
	for _, a := range args {
		if a.Type != TypeInteger {
			return nil, &EvalError{Message: "expected number"}
		}
	}
	for i := 0; i < len(args)-1; i++ {
		if !cmp(args[i].IntVal, args[i+1].IntVal) {
			return NewBool(false), nil
		}
	}
	return NewBool(true), nil
}

// EvalStr evaluates one or more Scheme expressions and returns the string
// representation of the last result.
func EvalStr(input string) (string, error) {
	exprs, err := parse(input)
	if err != nil {
		return "", &EvalError{Message: err.Error()}
	}
	if len(exprs) == 0 {
		return "", &EvalError{Message: "no expressions"}
	}

	env := makeGlobalEnv()
	var last *Value
	for _, expr := range exprs {
		v, err := eval(expr, env)
		if err != nil {
			return "", err
		}
		if v.Type != TypeVoid {
			last = v
		}
	}
	if last == nil {
		return "", nil
	}
	return last.Display(), nil
}

// EvalStrWithOutput evaluates Scheme expressions and returns both the result
// string and any captured output from display/write/newline.
func EvalStrWithOutput(input string) (result string, output string, err error) {
	r, err := EvalStr(input)
	return r, "", err
}
