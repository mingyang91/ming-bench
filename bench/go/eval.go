package ming

import "fmt"

// BuiltinFunc is a built-in procedure.
type BuiltinFunc struct {
	Name string
	Fn   func(args []Value) (Value, error)
}

func (b *BuiltinFunc) String() string {
	return fmt.Sprintf("#<procedure %s>", b.Name)
}

func eval(expr Expr, env *Env) (Value, error) {
	switch e := expr.(type) {
	case *NumberExpr:
		return &IntVal{Val: e.Val}, nil
	case *BoolExpr:
		return &BoolVal{Val: e.Val}, nil
	case *StringExpr:
		return &StringVal{Val: e.Val}, nil
	case *SymbolExpr:
		v, ok := env.get(e.Name)
		if !ok {
			return nil, &EvalError{Message: fmt.Sprintf("%d:%d: unbound variable: %s", e.Ln, e.Cl, e.Name)}
		}
		return v, nil
	case *ListExpr:
		if len(e.Items) == 0 {
			return &NilVal{}, nil
		}
		return evalList(e, env)
	default:
		return nil, &EvalError{Message: "unknown expression type"}
	}
}

func evalList(e *ListExpr, env *Env) (Value, error) {
	// Check for special forms
	if sym, ok := e.Items[0].(*SymbolExpr); ok {
		switch sym.Name {
		case "and":
			return evalAnd(e, env)
		case "or":
			return evalOr(e, env)
		}
	}

	// Function application
	fn, err := eval(e.Items[0], env)
	if err != nil {
		return nil, err
	}

	args := make([]Value, 0, len(e.Items)-1)
	for _, arg := range e.Items[1:] {
		v, err := eval(arg, env)
		if err != nil {
			return nil, err
		}
		args = append(args, v)
	}

	switch f := fn.(type) {
	case *BuiltinFunc:
		return f.Fn(args)
	default:
		return nil, &EvalError{Message: fmt.Sprintf("%d:%d: not a procedure: %s", e.Ln, e.Cl, fn.String())}
	}
}

func evalAnd(e *ListExpr, env *Env) (Value, error) {
	var result Value = &BoolVal{Val: true}
	for _, item := range e.Items[1:] {
		v, err := eval(item, env)
		if err != nil {
			return nil, err
		}
		if !isTruthy(v) {
			return v, nil
		}
		result = v
	}
	return result, nil
}

func evalOr(e *ListExpr, env *Env) (Value, error) {
	var result Value = &BoolVal{Val: false}
	for _, item := range e.Items[1:] {
		v, err := eval(item, env)
		if err != nil {
			return nil, err
		}
		if isTruthy(v) {
			return v, nil
		}
		result = v
	}
	return result, nil
}

func makeGlobalEnv() *Env {
	env := newEnv(nil)

	// Arithmetic
	env.set("+", &BuiltinFunc{Name: "+", Fn: builtinAdd})
	env.set("-", &BuiltinFunc{Name: "-", Fn: builtinSub})
	env.set("*", &BuiltinFunc{Name: "*", Fn: builtinMul})
	env.set("/", &BuiltinFunc{Name: "/", Fn: builtinDiv})

	// Comparison
	env.set("<", &BuiltinFunc{Name: "<", Fn: builtinLT})
	env.set(">", &BuiltinFunc{Name: ">", Fn: builtinGT})
	env.set("=", &BuiltinFunc{Name: "=", Fn: builtinEq})
	env.set("<=", &BuiltinFunc{Name: "<=", Fn: builtinLE})

	// Logic
	env.set("not", &BuiltinFunc{Name: "not", Fn: builtinNot})

	return env
}

func builtinAdd(args []Value) (Value, error) {
	var sum int64
	for _, a := range args {
		n, ok := a.(*IntVal)
		if !ok {
			return nil, &EvalError{Message: fmt.Sprintf("+: not a number: %s", a.String())}
		}
		sum += n.Val
	}
	return &IntVal{Val: sum}, nil
}

func builtinSub(args []Value) (Value, error) {
	if len(args) == 0 {
		return nil, &EvalError{Message: "-: requires at least 1 argument"}
	}
	first, ok := args[0].(*IntVal)
	if !ok {
		return nil, &EvalError{Message: fmt.Sprintf("-: not a number: %s", args[0].String())}
	}
	if len(args) == 1 {
		return &IntVal{Val: -first.Val}, nil
	}
	result := first.Val
	for _, a := range args[1:] {
		n, ok := a.(*IntVal)
		if !ok {
			return nil, &EvalError{Message: fmt.Sprintf("-: not a number: %s", a.String())}
		}
		result -= n.Val
	}
	return &IntVal{Val: result}, nil
}

func builtinMul(args []Value) (Value, error) {
	result := int64(1)
	for _, a := range args {
		n, ok := a.(*IntVal)
		if !ok {
			return nil, &EvalError{Message: fmt.Sprintf("*: not a number: %s", a.String())}
		}
		result *= n.Val
	}
	return &IntVal{Val: result}, nil
}

func builtinDiv(args []Value) (Value, error) {
	if len(args) < 2 {
		return nil, &EvalError{Message: "/: requires at least 2 arguments"}
	}
	first, ok := args[0].(*IntVal)
	if !ok {
		return nil, &EvalError{Message: fmt.Sprintf("/: not a number: %s", args[0].String())}
	}
	result := first.Val
	for _, a := range args[1:] {
		n, ok := a.(*IntVal)
		if !ok {
			return nil, &EvalError{Message: fmt.Sprintf("/: not a number: %s", a.String())}
		}
		if n.Val == 0 {
			return nil, &EvalError{Message: "/: division by zero"}
		}
		result /= n.Val
	}
	return &IntVal{Val: result}, nil
}

func numericCompare(name string, args []Value, cmp func(a, b int64) bool) (Value, error) {
	if len(args) < 2 {
		return nil, &EvalError{Message: fmt.Sprintf("%s: requires at least 2 arguments", name)}
	}
	prev, ok := args[0].(*IntVal)
	if !ok {
		return nil, &EvalError{Message: fmt.Sprintf("%s: not a number: %s", name, args[0].String())}
	}
	for _, a := range args[1:] {
		n, ok := a.(*IntVal)
		if !ok {
			return nil, &EvalError{Message: fmt.Sprintf("%s: not a number: %s", name, a.String())}
		}
		if !cmp(prev.Val, n.Val) {
			return &BoolVal{Val: false}, nil
		}
		prev = n
	}
	return &BoolVal{Val: true}, nil
}

func builtinLT(args []Value) (Value, error) {
	return numericCompare("<", args, func(a, b int64) bool { return a < b })
}

func builtinGT(args []Value) (Value, error) {
	return numericCompare(">", args, func(a, b int64) bool { return a > b })
}

func builtinEq(args []Value) (Value, error) {
	return numericCompare("=", args, func(a, b int64) bool { return a == b })
}

func builtinLE(args []Value) (Value, error) {
	return numericCompare("<=", args, func(a, b int64) bool { return a <= b })
}

func builtinNot(args []Value) (Value, error) {
	if len(args) != 1 {
		return nil, &EvalError{Message: "not: requires exactly 1 argument"}
	}
	return &BoolVal{Val: !isTruthy(args[0])}, nil
}
