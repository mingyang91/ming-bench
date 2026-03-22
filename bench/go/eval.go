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
		case "define":
			return evalDefine(expr, env)
		case "if":
			return evalIf(expr, env)
		case "quote":
			return evalQuote(expr, env)
		case "lambda":
			return evalLambda(expr, env)
		case "let":
			return evalLet(expr, env)
		case "begin":
			return evalBegin(expr, env)
		case "cond":
			return evalCond(expr, env)
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

	return applyFunc(op, args, expr)
}

func applyFunc(op *Value, args []*Value, expr *Expr) (*Value, error) {
	if op.Type == TypeLambda {
		return applyLambda(op, args, expr)
	}
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
	case "cons":
		if len(args) != 2 {
			return nil, errAtf(expr, "cons: expected 2 arguments, got %d", len(args))
		}
		return PairValue(args[0], args[1]), nil
	case "car":
		if len(args) != 1 {
			return nil, errAtf(expr, "car: expected 1 argument, got %d", len(args))
		}
		if args[0].Type != TypePair {
			return nil, errAtf(expr, "car: expected pair")
		}
		return args[0].Car, nil
	case "cdr":
		if len(args) != 1 {
			return nil, errAtf(expr, "cdr: expected 1 argument, got %d", len(args))
		}
		if args[0].Type != TypePair {
			return nil, errAtf(expr, "cdr: expected pair")
		}
		return args[0].Cdr, nil
	case "null?":
		if len(args) != 1 {
			return nil, errAtf(expr, "null?: expected 1 argument, got %d", len(args))
		}
		return BooleanValue(args[0].Type == TypeNull), nil
	case "list":
		result := Null
		for i := len(args) - 1; i >= 0; i-- {
			result = PairValue(args[i], result)
		}
		return result, nil
	case "length":
		if len(args) != 1 {
			return nil, errAtf(expr, "length: expected 1 argument, got %d", len(args))
		}
		count := int64(0)
		cur := args[0]
		for cur.Type == TypePair {
			count++
			cur = cur.Cdr
		}
		if cur.Type != TypeNull {
			return nil, errAtf(expr, "length: expected proper list")
		}
		return IntegerValue(count), nil
	case "pair?":
		if len(args) != 1 {
			return nil, errAtf(expr, "pair?: expected 1 argument, got %d", len(args))
		}
		return BooleanValue(args[0].Type == TypePair), nil
	case "number?":
		if len(args) != 1 {
			return nil, errAtf(expr, "number?: expected 1 argument, got %d", len(args))
		}
		return BooleanValue(args[0].Type == TypeInteger), nil
	case "string?":
		if len(args) != 1 {
			return nil, errAtf(expr, "string?: expected 1 argument, got %d", len(args))
		}
		return BooleanValue(args[0].Type == TypeString), nil
	case "boolean?":
		if len(args) != 1 {
			return nil, errAtf(expr, "boolean?: expected 1 argument, got %d", len(args))
		}
		return BooleanValue(args[0].Type == TypeBoolean), nil
	case "symbol?":
		if len(args) != 1 {
			return nil, errAtf(expr, "symbol?: expected 1 argument, got %d", len(args))
		}
		return BooleanValue(args[0].Type == TypeSymbol), nil
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

func evalDefine(expr *Expr, env *Env) (*Value, error) {
	if len(expr.List) < 3 {
		return nil, errAt(expr, "define: expected at least 2 arguments")
	}
	target := expr.List[1]
	if target.Type == ExprSymbol {
		// (define x expr)
		val, err := Eval(expr.List[2], env)
		if err != nil {
			return nil, err
		}
		env.Set(target.StrVal, val)
		return Void, nil
	}
	if target.Type == ExprList && len(target.List) >= 1 && target.List[0].Type == ExprSymbol {
		// (define (f params...) body...)
		name := target.List[0].StrVal
		params := make([]string, len(target.List)-1)
		for i, p := range target.List[1:] {
			if p.Type != ExprSymbol {
				return nil, errAt(p, "define: parameter must be a symbol")
			}
			params[i] = p.StrVal
		}
		lambda := &Value{
			Type:    TypeLambda,
			Params:  params,
			Body:    expr.List[2:],
			Closure: env,
		}
		env.Set(name, lambda)
		return Void, nil
	}
	return nil, errAt(target, "define: invalid syntax")
}

func evalIf(expr *Expr, env *Env) (*Value, error) {
	if len(expr.List) < 3 || len(expr.List) > 4 {
		return nil, errAt(expr, "if: expected 2 or 3 arguments")
	}
	cond, err := Eval(expr.List[1], env)
	if err != nil {
		return nil, err
	}
	if cond.IsTruthy() {
		return Eval(expr.List[2], env)
	}
	if len(expr.List) == 4 {
		return Eval(expr.List[3], env)
	}
	return Void, nil
}

func evalQuote(expr *Expr, env *Env) (*Value, error) {
	if len(expr.List) != 2 {
		return nil, errAt(expr, "quote: expected 1 argument")
	}
	return exprToValue(expr.List[1])
}

func exprToValue(expr *Expr) (*Value, error) {
	switch expr.Type {
	case ExprInteger:
		return IntegerValue(expr.IntVal), nil
	case ExprBoolean:
		return BooleanValue(expr.BoolVal), nil
	case ExprString:
		return StringValue(expr.StrVal), nil
	case ExprSymbol:
		return SymbolValue(expr.StrVal), nil
	case ExprList:
		if len(expr.List) == 0 {
			return Null, nil
		}
		// Build a proper list from the elements
		result := Null
		for i := len(expr.List) - 1; i >= 0; i-- {
			val, err := exprToValue(expr.List[i])
			if err != nil {
				return nil, err
			}
			result = PairValue(val, result)
		}
		return result, nil
	default:
		return nil, fmt.Errorf("cannot quote expression")
	}
}

func evalLambda(expr *Expr, env *Env) (*Value, error) {
	if len(expr.List) < 3 {
		return nil, errAt(expr, "lambda: expected at least 2 arguments")
	}
	paramExpr := expr.List[1]
	if paramExpr.Type != ExprList {
		return nil, errAt(paramExpr, "lambda: parameters must be a list")
	}
	params := make([]string, len(paramExpr.List))
	for i, p := range paramExpr.List {
		if p.Type != ExprSymbol {
			return nil, errAt(p, "lambda: parameter must be a symbol")
		}
		params[i] = p.StrVal
	}
	return &Value{
		Type:    TypeLambda,
		Params:  params,
		Body:    expr.List[2:],
		Closure: env,
	}, nil
}

func applyLambda(fn *Value, args []*Value, expr *Expr) (*Value, error) {
	if len(args) != len(fn.Params) {
		return nil, errAtf(expr, "expected %d arguments, got %d", len(fn.Params), len(args))
	}
	localEnv := NewEnv(fn.Closure)
	for i, param := range fn.Params {
		localEnv.Set(param, args[i])
	}
	var result *Value
	var err error
	for _, bodyExpr := range fn.Body {
		result, err = Eval(bodyExpr, localEnv)
		if err != nil {
			return nil, err
		}
	}
	return result, nil
}

func evalLet(expr *Expr, env *Env) (*Value, error) {
	if len(expr.List) < 3 {
		return nil, errAt(expr, "let: expected at least 2 arguments")
	}
	bindings := expr.List[1]
	if bindings.Type != ExprList {
		return nil, errAt(bindings, "let: bindings must be a list")
	}
	localEnv := NewEnv(env)
	for _, b := range bindings.List {
		if b.Type != ExprList || len(b.List) != 2 {
			return nil, errAt(b, "let: invalid binding")
		}
		if b.List[0].Type != ExprSymbol {
			return nil, errAt(b.List[0], "let: binding name must be a symbol")
		}
		val, err := Eval(b.List[1], env)
		if err != nil {
			return nil, err
		}
		localEnv.Set(b.List[0].StrVal, val)
	}
	var result *Value
	var err error
	for _, bodyExpr := range expr.List[2:] {
		result, err = Eval(bodyExpr, localEnv)
		if err != nil {
			return nil, err
		}
	}
	return result, nil
}

func evalBegin(expr *Expr, env *Env) (*Value, error) {
	if len(expr.List) < 2 {
		return Void, nil
	}
	var result *Value
	var err error
	for _, e := range expr.List[1:] {
		result, err = Eval(e, env)
		if err != nil {
			return nil, err
		}
	}
	return result, nil
}

func evalCond(expr *Expr, env *Env) (*Value, error) {
	for _, clause := range expr.List[1:] {
		if clause.Type != ExprList || len(clause.List) < 2 {
			return nil, errAt(clause, "cond: invalid clause")
		}
		// Check for else clause
		if clause.List[0].Type == ExprSymbol && clause.List[0].StrVal == "else" {
			var result *Value
			var err error
			for _, e := range clause.List[1:] {
				result, err = Eval(e, env)
				if err != nil {
					return nil, err
				}
			}
			return result, nil
		}
		test, err := Eval(clause.List[0], env)
		if err != nil {
			return nil, err
		}
		if test.IsTruthy() {
			var result *Value
			for _, e := range clause.List[1:] {
				result, err = Eval(e, env)
				if err != nil {
					return nil, err
				}
			}
			return result, nil
		}
	}
	return Void, nil
}

func makeDefaultEnv() *Env {
	env := NewEnv(nil)
	builtins := []string{"+", "-", "*", "/", "<", ">", "=", "<=", ">=", "not",
		"cons", "car", "cdr", "null?", "list", "length",
		"pair?", "number?", "string?", "boolean?", "symbol?"}
	for _, name := range builtins {
		env.Set(name, &Value{Type: TypeSymbol, StrVal: fmt.Sprintf("__builtin:%s", name)})
	}
	return env
}
