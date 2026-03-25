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
		case "define":
			return evalDefine(expr, env)
		case "if":
			return evalIf(expr, env)
		case "quote":
			return evalQuote(expr)
		case "lambda":
			return evalLambda(expr, env)
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

	// Call lambda
	if fn.Type == TypeLambda {
		if len(args) != len(fn.Params) {
			return nil, fmt.Errorf("%d:%d: wrong number of arguments: expected %d, got %d", expr.Line, expr.Col, len(fn.Params), len(args))
		}
		callEnv := NewEnv(fn.ClosureEnv)
		for i, param := range fn.Params {
			callEnv.Set(param, args[i])
		}
		var result *Value
		for _, bodyExpr := range fn.Body {
			var err error
			result, err = Eval(bodyExpr, callEnv)
			if err != nil {
				return nil, err
			}
		}
		return result, nil
	}

	return nil, fmt.Errorf("%d:%d: not a procedure", expr.Line, expr.Col)
}

func evalDefine(expr *Expr, env *Env) (*Value, error) {
	if len(expr.Elements) < 3 {
		return nil, fmt.Errorf("%d:%d: 'define' requires at least 2 arguments", expr.Line, expr.Col)
	}
	target := expr.Elements[1]
	// Shorthand: (define (f x) body) => (define f (lambda (x) body))
	if target.Type == ExprList && len(target.Elements) > 0 {
		name := target.Elements[0]
		if name.Type != ExprSymbol {
			return nil, fmt.Errorf("%d:%d: expected symbol in define", expr.Line, expr.Col)
		}
		params := make([]string, len(target.Elements)-1)
		for i, p := range target.Elements[1:] {
			if p.Type != ExprSymbol {
				return nil, fmt.Errorf("%d:%d: expected symbol as parameter", p.Line, p.Col)
			}
			params[i] = p.StrVal
		}
		lambda := &Value{
			Type:       TypeLambda,
			Params:     params,
			Body:       expr.Elements[2:],
			ClosureEnv: env,
		}
		env.Set(name.StrVal, lambda)
		return Void, nil
	}
	// Simple: (define x expr)
	if target.Type != ExprSymbol {
		return nil, fmt.Errorf("%d:%d: expected symbol after define", expr.Line, expr.Col)
	}
	val, err := Eval(expr.Elements[2], env)
	if err != nil {
		return nil, err
	}
	env.Set(target.StrVal, val)
	return Void, nil
}

func evalIf(expr *Expr, env *Env) (*Value, error) {
	if len(expr.Elements) < 3 || len(expr.Elements) > 4 {
		return nil, fmt.Errorf("%d:%d: 'if' requires 2 or 3 arguments", expr.Line, expr.Col)
	}
	cond, err := Eval(expr.Elements[1], env)
	if err != nil {
		return nil, err
	}
	if isTruthy(cond) {
		return Eval(expr.Elements[2], env)
	}
	if len(expr.Elements) == 4 {
		return Eval(expr.Elements[3], env)
	}
	return Void, nil
}

func evalQuote(expr *Expr) (*Value, error) {
	if len(expr.Elements) != 2 {
		return nil, fmt.Errorf("%d:%d: 'quote' requires exactly 1 argument", expr.Line, expr.Col)
	}
	return exprToValue(expr.Elements[1]), nil
}

func exprToValue(expr *Expr) *Value {
	switch expr.Type {
	case ExprInteger:
		return IntValue(expr.IntVal)
	case ExprBoolean:
		return BoolValue(expr.BoolVal)
	case ExprString:
		return StringValue(expr.StrVal)
	case ExprSymbol:
		return SymbolValue(expr.StrVal)
	case ExprList:
		if len(expr.Elements) == 0 {
			return Null
		}
		result := Null
		for i := len(expr.Elements) - 1; i >= 0; i-- {
			result = PairValue(exprToValue(expr.Elements[i]), result)
		}
		return result
	}
	return Void
}

func evalLambda(expr *Expr, env *Env) (*Value, error) {
	if len(expr.Elements) < 3 {
		return nil, fmt.Errorf("%d:%d: 'lambda' requires parameters and body", expr.Line, expr.Col)
	}
	paramExpr := expr.Elements[1]
	if paramExpr.Type != ExprList {
		return nil, fmt.Errorf("%d:%d: 'lambda' parameters must be a list", expr.Line, expr.Col)
	}
	params := make([]string, len(paramExpr.Elements))
	for i, p := range paramExpr.Elements {
		if p.Type != ExprSymbol {
			return nil, fmt.Errorf("%d:%d: expected symbol as parameter", p.Line, p.Col)
		}
		params[i] = p.StrVal
	}
	return &Value{
		Type:       TypeLambda,
		Params:     params,
		Body:       expr.Elements[2:],
		ClosureEnv: env,
	}, nil
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
