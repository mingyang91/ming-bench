package ming

import "fmt"

func Eval(expr *Expr, env *Env) (*Value, error) {
	switch expr.Type {
	case ExprInteger:
		return IntValue(expr.IntVal), nil
	case ExprBoolean:
		return BoolValue(expr.BoolVal), nil
	case ExprString:
		return StringValue(expr.StrVal), nil
	case ExprSymbol:
		val, ok := env.Get(expr.StrVal)
		if !ok {
			return nil, fmt.Errorf("%d:%d: unbound variable '%s'", expr.Line, expr.Col, expr.StrVal)
		}
		return val, nil
	case ExprList:
		return evalList(expr, env)
	}
	return nil, fmt.Errorf("%d:%d: unknown expression type", expr.Line, expr.Col)
}

func evalList(expr *Expr, env *Env) (*Value, error) {
	if len(expr.Elements) == 0 {
		return nil, fmt.Errorf("%d:%d: empty application", expr.Line, expr.Col)
	}

	// Check for special forms
	head := expr.Elements[0]
	if head.Type == ExprSymbol {
		switch head.StrVal {
		case "and":
			return evalAnd(expr, env)
		case "or":
			return evalOr(expr, env)
		}
	}

	// Function application
	fn, err := Eval(head, env)
	if err != nil {
		return nil, err
	}

	// Evaluate arguments
	args := make([]*Value, len(expr.Elements)-1)
	for i, argExpr := range expr.Elements[1:] {
		val, err := Eval(argExpr, env)
		if err != nil {
			return nil, err
		}
		args[i] = val
	}

	// Call builtin
	if fn.Type == TypeSymbol && len(fn.StrVal) > 8 && fn.StrVal[:8] == "builtin:" {
		return callBuiltin(fn.StrVal, args, expr.Line, expr.Col)
	}

	return nil, fmt.Errorf("%d:%d: not a procedure", expr.Line, expr.Col)
}

func evalAnd(expr *Expr, env *Env) (*Value, error) {
	if len(expr.Elements) == 1 {
		return True, nil
	}
	var result *Value = True
	for _, e := range expr.Elements[1:] {
		val, err := Eval(e, env)
		if err != nil {
			return nil, err
		}
		result = val
		if !isTruthy(val) {
			return val, nil
		}
	}
	return result, nil
}

func evalOr(expr *Expr, env *Env) (*Value, error) {
	if len(expr.Elements) == 1 {
		return False, nil
	}
	for _, e := range expr.Elements[1:] {
		val, err := Eval(e, env)
		if err != nil {
			return nil, err
		}
		if isTruthy(val) {
			return val, nil
		}
	}
	return False, nil
}
