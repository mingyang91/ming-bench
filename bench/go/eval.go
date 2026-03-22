package ming

import "fmt"

func Eval(expr *Expr, env *Env) (*Value, error) {
	switch expr.Type {
	case ExprInteger:
		return IntegerValue(expr.IntVal), nil
	case ExprBoolean:
		return BooleanValue(expr.BoolVal), nil
	case ExprString:
		return StringValue(expr.StrVal), nil
	case ExprSymbol:
		val, ok := env.Get(expr.StrVal)
		if !ok {
			return nil, errAtf(expr, "unbound variable: %s", expr.StrVal)
		}
		return val, nil
	case ExprList:
		if len(expr.List) == 0 {
			return nil, errAt(expr, "empty application")
		}
		return evalList(expr, env)
	default:
		return nil, errAtf(expr, "unknown expression type")
	}
}

func evalList(expr *Expr, env *Env) (*Value, error) {
	head := expr.List[0]

	// Handle special forms
	if head.Type == ExprSymbol {
		switch head.StrVal {
		case "and":
			return evalAnd(expr, env)
		case "or":
			return evalOr(expr, env)
		}
	}

	// Function application
	op, err := Eval(head, env)
	if err != nil {
		return nil, err
	}

	// Evaluate arguments
	args := make([]*Value, len(expr.List)-1)
	for i, argExpr := range expr.List[1:] {
		val, err := Eval(argExpr, env)
		if err != nil {
			return nil, err
		}
		args[i] = val
	}

	return applyBuiltin(op, args, expr)
}

func applyBuiltin(op *Value, args []*Value, expr *Expr) (*Value, error) {
	if op.Type != TypeSymbol || len(op.StrVal) < 10 || op.StrVal[:10] != "__builtin:" {
		return nil, errAtf(expr, "not a procedure")
	}
	name := op.StrVal[10:]

	switch name {
	case "+":
		return builtinAdd(args, expr)
	case "-":
		return builtinSub(args, expr)
	case "*":
		return builtinMul(args, expr)
	case "/":
		return builtinDiv(args, expr)
	case "<":
		return builtinCmp(args, expr, func(a, b int64) bool { return a < b })
	case ">":
		return builtinCmp(args, expr, func(a, b int64) bool { return a > b })
	case "=":
		return builtinCmp(args, expr, func(a, b int64) bool { return a == b })
	case "<=":
		return builtinCmp(args, expr, func(a, b int64) bool { return a <= b })
	case ">=":
		return builtinCmp(args, expr, func(a, b int64) bool { return a >= b })
	case "not":
		if len(args) != 1 {
			return nil, errAtf(expr, "not: expected 1 argument, got %d", len(args))
		}
		return BooleanValue(!args[0].IsTruthy()), nil
	default:
		return nil, errAtf(expr, "unknown procedure: %s", name)
	}
}

func builtinAdd(args []*Value, expr *Expr) (*Value, error) {
	var sum int64
	for _, a := range args {
		if a.Type != TypeInteger {
			return nil, errAtf(expr, "+: expected number")
		}
		sum += a.IntVal
	}
	return IntegerValue(sum), nil
}

func builtinSub(args []*Value, expr *Expr) (*Value, error) {
	if len(args) == 0 {
		return nil, errAtf(expr, "-: expected at least 1 argument")
	}
	for _, a := range args {
		if a.Type != TypeInteger {
			return nil, errAtf(expr, "-: expected number")
		}
	}
	if len(args) == 1 {
		return IntegerValue(-args[0].IntVal), nil
	}
	result := args[0].IntVal
	for _, a := range args[1:] {
		result -= a.IntVal
	}
	return IntegerValue(result), nil
}

func builtinMul(args []*Value, expr *Expr) (*Value, error) {
	var product int64 = 1
	for _, a := range args {
		if a.Type != TypeInteger {
			return nil, errAtf(expr, "*: expected number")
		}
		product *= a.IntVal
	}
	return IntegerValue(product), nil
}

func builtinDiv(args []*Value, expr *Expr) (*Value, error) {
	if len(args) < 2 {
		return nil, errAtf(expr, "/: expected at least 2 arguments")
	}
	for _, a := range args {
		if a.Type != TypeInteger {
			return nil, errAtf(expr, "/: expected number")
		}
	}
	result := args[0].IntVal
	for _, a := range args[1:] {
		if a.IntVal == 0 {
			return nil, errAtf(expr, "/: division by zero")
		}
		result /= a.IntVal
	}
	return IntegerValue(result), nil
}

func builtinCmp(args []*Value, expr *Expr, cmp func(int64, int64) bool) (*Value, error) {
	if len(args) < 2 {
		return nil, errAtf(expr, "comparison: expected at least 2 arguments")
	}
	for _, a := range args {
		if a.Type != TypeInteger {
			return nil, errAtf(expr, "comparison: expected number")
		}
	}
	for i := 0; i < len(args)-1; i++ {
		if !cmp(args[i].IntVal, args[i+1].IntVal) {
			return False, nil
		}
	}
	return True, nil
}

func evalAnd(expr *Expr, env *Env) (*Value, error) {
	if len(expr.List) == 1 {
		return True, nil
	}
	var result *Value = True
	for _, e := range expr.List[1:] {
		var err error
		result, err = Eval(e, env)
		if err != nil {
			return nil, err
		}
		if !result.IsTruthy() {
			return result, nil
		}
	}
	return result, nil
}

func evalOr(expr *Expr, env *Env) (*Value, error) {
	if len(expr.List) == 1 {
		return False, nil
	}
	for _, e := range expr.List[1:] {
		result, err := Eval(e, env)
		if err != nil {
			return nil, err
		}
		if result.IsTruthy() {
			return result, nil
		}
	}
	return False, nil
}

func makeDefaultEnv() *Env {
	env := NewEnv(nil)
	builtins := []string{"+", "-", "*", "/", "<", ">", "=", "<=", ">=", "not"}
	for _, name := range builtins {
		env.Set(name, &Value{Type: TypeSymbol, StrVal: fmt.Sprintf("__builtin:%s", name)})
	}
	return env
}
