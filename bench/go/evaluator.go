package ming

import "fmt"

// Env holds variable bindings.
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

func (e *Env) set(name string, val *Value) {
	e.bindings[name] = val
}

func makeGlobalEnv() *Env {
	env := newEnv(nil)
	env.set("+", builtinVal("+", builtinAdd))
	env.set("-", builtinVal("-", builtinSub))
	env.set("*", builtinVal("*", builtinMulFn))
	env.set("/", builtinVal("/", builtinDivide))
	env.set("<", builtinVal("<", makeCompare("<", func(a, b int64) bool { return a < b })))
	env.set(">", builtinVal(">", makeCompare(">", func(a, b int64) bool { return a > b })))
	env.set("=", builtinVal("=", makeCompare("=", func(a, b int64) bool { return a == b })))
	env.set("<=", builtinVal("<=", makeCompare("<=", func(a, b int64) bool { return a <= b })))
	env.set(">=", builtinVal(">=", makeCompare(">=", func(a, b int64) bool { return a >= b })))
	env.set("not", builtinVal("not", builtinNot))
	return env
}

func eval(expr *Value, env *Env) (*Value, error) {
	switch expr.Kind {
	case KindInteger, KindBoolean, KindString:
		return expr, nil
	case KindSymbol:
		v, ok := env.get(expr.Str)
		if !ok {
			return nil, &EvalError{Message: fmt.Sprintf("unbound variable: %s", expr.Str)}
		}
		return v, nil
	case KindPair:
		return evalList(expr, env)
	case KindNull:
		return nil, &EvalError{Message: "cannot evaluate empty list"}
	}
	return nil, &EvalError{Message: "unknown expression type"}
}

func evalList(expr *Value, env *Env) (*Value, error) {
	head := expr.Car

	// Special forms
	if head.Kind == KindSymbol {
		switch head.Str {
		case "and":
			return evalAnd(expr.Cdr, env)
		case "or":
			return evalOr(expr.Cdr, env)
		case "if":
			return evalIf(expr.Cdr, env)
		case "define":
			return evalDefine(expr.Cdr, env)
		case "quote":
			return expr.Cdr.Car, nil
		case "lambda":
			return evalLambda(expr.Cdr, env)
		}
	}

	// Function application
	fn, err := eval(head, env)
	if err != nil {
		return nil, err
	}

	// Evaluate arguments
	args, err := evalArgs(expr.Cdr, env)
	if err != nil {
		return nil, err
	}

	if fn.Kind == KindBuiltin {
		return fn.Builtin(args)
	}

	if fn.Kind == KindLambda {
		return applyLambda(fn, args)
	}

	return nil, &EvalError{Message: fmt.Sprintf("not a procedure: %s", fn.String())}
}

func evalArgs(list *Value, env *Env) ([]*Value, error) {
	var args []*Value
	cur := list
	for cur.Kind == KindPair {
		v, err := eval(cur.Car, env)
		if err != nil {
			return nil, err
		}
		args = append(args, v)
		cur = cur.Cdr
	}
	return args, nil
}

func evalAnd(args *Value, env *Env) (*Value, error) {
	result := boolVal(true)
	cur := args
	for cur.Kind == KindPair {
		v, err := eval(cur.Car, env)
		if err != nil {
			return nil, err
		}
		if !v.isTruthy() {
			return v, nil
		}
		result = v
		cur = cur.Cdr
	}
	return result, nil
}

func evalOr(args *Value, env *Env) (*Value, error) {
	result := boolVal(false)
	cur := args
	for cur.Kind == KindPair {
		v, err := eval(cur.Car, env)
		if err != nil {
			return nil, err
		}
		if v.isTruthy() {
			return v, nil
		}
		result = v
		cur = cur.Cdr
	}
	return result, nil
}

func evalIf(args *Value, env *Env) (*Value, error) {
	cond, err := eval(args.Car, env)
	if err != nil {
		return nil, err
	}
	if cond.isTruthy() {
		return eval(args.Cdr.Car, env)
	}
	// else branch (optional)
	if args.Cdr.Cdr.Kind == KindNull {
		return voidVal(), nil
	}
	return eval(args.Cdr.Cdr.Car, env)
}

func evalDefine(args *Value, env *Env) (*Value, error) {
	target := args.Car
	if target.Kind == KindSymbol {
		// (define x expr)
		val, err := eval(args.Cdr.Car, env)
		if err != nil {
			return nil, err
		}
		env.set(target.Str, val)
		return voidVal(), nil
	}
	if target.Kind == KindPair {
		// (define (name params...) body...)
		name := target.Car.Str
		params := listToStrings(target.Cdr)
		body := listToSlice(args.Cdr)
		fn := &Value{
			Kind:       KindLambda,
			Params:     params,
			Body:       body,
			ClosureEnv: env,
		}
		env.set(name, fn)
		return voidVal(), nil
	}
	return nil, &EvalError{Message: "define: bad syntax"}
}

func evalLambda(args *Value, env *Env) (*Value, error) {
	params := listToStrings(args.Car)
	body := listToSlice(args.Cdr)
	return &Value{
		Kind:       KindLambda,
		Params:     params,
		Body:       body,
		ClosureEnv: env,
	}, nil
}

func applyLambda(fn *Value, args []*Value) (*Value, error) {
	if len(args) != len(fn.Params) {
		return nil, &EvalError{Message: fmt.Sprintf("expected %d arguments, got %d", len(fn.Params), len(args))}
	}
	localEnv := newEnv(fn.ClosureEnv)
	for i, p := range fn.Params {
		localEnv.set(p, args[i])
	}
	var result *Value
	var err error
	for _, expr := range fn.Body {
		result, err = eval(expr, localEnv)
		if err != nil {
			return nil, err
		}
	}
	return result, nil
}

func listToStrings(v *Value) []string {
	var result []string
	cur := v
	for cur.Kind == KindPair {
		result = append(result, cur.Car.Str)
		cur = cur.Cdr
	}
	return result
}

func listToSlice(v *Value) []*Value {
	var result []*Value
	cur := v
	for cur.Kind == KindPair {
		result = append(result, cur.Car)
		cur = cur.Cdr
	}
	return result
}

// Builtin implementations

func builtinAdd(args []*Value) (*Value, error) {
	sum := int64(0)
	for _, a := range args {
		if a.Kind != KindInteger {
			return nil, &EvalError{Message: "+: expected number"}
		}
		sum += a.Int
	}
	return intVal(sum), nil
}

func builtinSub(args []*Value) (*Value, error) {
	if len(args) == 0 {
		return nil, &EvalError{Message: "-: expected at least 1 argument"}
	}
	if args[0].Kind != KindInteger {
		return nil, &EvalError{Message: "-: expected number"}
	}
	if len(args) == 1 {
		return intVal(-args[0].Int), nil
	}
	result := args[0].Int
	for _, a := range args[1:] {
		if a.Kind != KindInteger {
			return nil, &EvalError{Message: "-: expected number"}
		}
		result -= a.Int
	}
	return intVal(result), nil
}

func builtinMulFn(args []*Value) (*Value, error) {
	product := int64(1)
	for _, a := range args {
		if a.Kind != KindInteger {
			return nil, &EvalError{Message: "*: expected number"}
		}
		product *= a.Int
	}
	return intVal(product), nil
}

func builtinDivide(args []*Value) (*Value, error) {
	if len(args) < 2 {
		return nil, &EvalError{Message: "/: expected at least 2 arguments"}
	}
	if args[0].Kind != KindInteger {
		return nil, &EvalError{Message: "/: expected number"}
	}
	result := args[0].Int
	for _, a := range args[1:] {
		if a.Kind != KindInteger {
			return nil, &EvalError{Message: "/: expected number"}
		}
		if a.Int == 0 {
			return nil, &EvalError{Message: "division by zero"}
		}
		result /= a.Int
	}
	return intVal(result), nil
}

func makeCompare(name string, cmp func(int64, int64) bool) BuiltinFunc {
	return func(args []*Value) (*Value, error) {
		if len(args) < 2 {
			return nil, &EvalError{Message: fmt.Sprintf("%s: expected at least 2 arguments", name)}
		}
		for _, a := range args {
			if a.Kind != KindInteger {
				return nil, &EvalError{Message: fmt.Sprintf("%s: expected number", name)}
			}
		}
		for i := 0; i < len(args)-1; i++ {
			if !cmp(args[i].Int, args[i+1].Int) {
				return boolVal(false), nil
			}
		}
		return boolVal(true), nil
	}
}

func builtinNot(args []*Value) (*Value, error) {
	if len(args) != 1 {
		return nil, &EvalError{Message: "not: expected 1 argument"}
	}
	return boolVal(!args[0].isTruthy()), nil
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
	var result *Value
	for _, expr := range exprs {
		result, err = eval(expr, env)
		if err != nil {
			return "", err
		}
	}

	if result.Kind == KindVoid {
		return "", nil
	}
	return result.String(), nil
}

// EvalStrWithOutput evaluates Scheme expressions and returns both the result
// string and any captured output from display/write/newline.
func EvalStrWithOutput(input string) (result string, output string, err error) {
	return "", "", &EvalError{Message: "not implemented"}
}
