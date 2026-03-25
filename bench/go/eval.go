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
	case ExprChar:
		runes := []rune(expr.StrVal)
		return CharValue(runes[0]), nil
	case ExprFloat:
		return FloatValue(expr.FloatVal), nil
	case ExprRational:
		return RationalValue(expr.Num, expr.Den), nil
	case ExprSymbol:
		val, ok := env.Get(expr.StrVal)
		if !ok {
			return nil, fmt.Errorf("%d:%d: unbound variable '%s'", expr.Line, expr.Col, expr.StrVal)
		}
		return val, nil
	case ExprLiteral:
		return expr.LitVal, nil
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
		case "let":
			return evalLet(expr, env)
		case "begin":
			return evalBegin(expr, env)
		case "cond":
			return evalCond(expr, env)
		case "set!":
			return evalSetBang(expr, env)
		case "define-syntax":
			return evalDefineSyntax(expr, env)
		case "define-record-type":
			return evalDefineRecordType(expr, env)
		case "case-lambda":
			return evalCaseLambda(expr, env)
		}
		// Check for macro application
		if val, ok := env.Get(head.StrVal); ok && val.Type == TypeMacro {
			expanded, err := expandMacro(val.Macro, expr)
			if err != nil {
				return nil, err
			}
			return Eval(expanded, env)
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
		return callBuiltin(fn.StrVal, args, env, expr.Line, expr.Col)
	}

	// Call Go native function
	if fn.Type == TypeGoFunc {
		return fn.GoFunc(args)
	}

	// Call lambda
	if fn.Type == TypeLambda {
		return callLambda(fn, args, expr.Line, expr.Col)
	}

	// Call case-lambda
	if fn.Type == TypeCaseLambda {
		return callCaseLambda(fn, args, expr.Line, expr.Col)
	}

	// Macro from ExprLiteral
	if fn.Type == TypeMacro {
		expanded, err := expandMacro(fn.Macro, expr)
		if err != nil {
			return nil, err
		}
		return Eval(expanded, env)
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
		params, restParam, err := parseParams(target.Elements[1:])
		if err != nil {
			return nil, err
		}
		lambda := &Value{
			Type:       TypeLambda,
			Params:     params,
			RestParam:  restParam,
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
	case ExprFloat:
		return FloatValue(expr.FloatVal)
	case ExprRational:
		return RationalValue(expr.Num, expr.Den)
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

func parseParams(elements []*Expr) (params []string, restParam string, err error) {
	for i, p := range elements {
		if p.Type != ExprSymbol {
			return nil, "", fmt.Errorf("%d:%d: expected symbol as parameter", p.Line, p.Col)
		}
		if p.StrVal == "." {
			if i+1 != len(elements)-1 {
				return nil, "", fmt.Errorf("%d:%d: expected exactly one parameter after '.'", p.Line, p.Col)
			}
			rest := elements[i+1]
			if rest.Type != ExprSymbol {
				return nil, "", fmt.Errorf("%d:%d: expected symbol after '.'", rest.Line, rest.Col)
			}
			return params, rest.StrVal, nil
		}
		params = append(params, p.StrVal)
	}
	return params, "", nil
}

func evalLambda(expr *Expr, env *Env) (*Value, error) {
	if len(expr.Elements) < 3 {
		return nil, fmt.Errorf("%d:%d: 'lambda' requires parameters and body", expr.Line, expr.Col)
	}
	paramExpr := expr.Elements[1]
	// (lambda args body) — single symbol means all args go to rest
	if paramExpr.Type == ExprSymbol {
		return &Value{
			Type:       TypeLambda,
			RestParam:  paramExpr.StrVal,
			Body:       expr.Elements[2:],
			ClosureEnv: env,
		}, nil
	}
	if paramExpr.Type != ExprList {
		return nil, fmt.Errorf("%d:%d: 'lambda' parameters must be a list", expr.Line, expr.Col)
	}
	params, restParam, err := parseParams(paramExpr.Elements)
	if err != nil {
		return nil, err
	}
	return &Value{
		Type:       TypeLambda,
		Params:     params,
		RestParam:  restParam,
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

func evalLet(expr *Expr, env *Env) (*Value, error) {
	if len(expr.Elements) < 3 {
		return nil, fmt.Errorf("%d:%d: 'let' requires bindings and body", expr.Line, expr.Col)
	}

	bindingsIdx := 1
	// Named let: (let name ((var init) ...) body ...)
	var loopName string
	if expr.Elements[1].Type == ExprSymbol {
		loopName = expr.Elements[1].StrVal
		bindingsIdx = 2
		if len(expr.Elements) < 4 {
			return nil, fmt.Errorf("%d:%d: named 'let' requires bindings and body", expr.Line, expr.Col)
		}
	}

	bindingsExpr := expr.Elements[bindingsIdx]
	if bindingsExpr.Type != ExprList {
		return nil, fmt.Errorf("%d:%d: 'let' bindings must be a list", expr.Line, expr.Col)
	}

	names := make([]string, len(bindingsExpr.Elements))
	vals := make([]*Value, len(bindingsExpr.Elements))
	for i, binding := range bindingsExpr.Elements {
		if binding.Type != ExprList || len(binding.Elements) != 2 {
			return nil, fmt.Errorf("%d:%d: invalid let binding", expr.Line, expr.Col)
		}
		if binding.Elements[0].Type != ExprSymbol {
			return nil, fmt.Errorf("%d:%d: let binding name must be a symbol", expr.Line, expr.Col)
		}
		names[i] = binding.Elements[0].StrVal
		val, err := Eval(binding.Elements[1], env)
		if err != nil {
			return nil, err
		}
		vals[i] = val
	}

	letEnv := NewEnv(env)
	for i, name := range names {
		letEnv.Set(name, vals[i])
	}

	body := expr.Elements[bindingsIdx+1:]

	if loopName != "" {
		// Named let: bind the loop name to a lambda that re-enters
		lambda := &Value{
			Type:       TypeLambda,
			Params:     names,
			Body:       body,
			ClosureEnv: letEnv,
		}
		letEnv.Set(loopName, lambda)
	}

	var result *Value
	for _, bodyExpr := range body {
		var err error
		result, err = Eval(bodyExpr, letEnv)
		if err != nil {
			return nil, err
		}
	}
	return result, nil
}

func evalSetBang(expr *Expr, env *Env) (*Value, error) {
	if len(expr.Elements) != 3 {
		return nil, fmt.Errorf("%d:%d: 'set!' requires exactly 2 arguments", expr.Line, expr.Col)
	}
	target := expr.Elements[1]
	if target.Type != ExprSymbol {
		return nil, fmt.Errorf("%d:%d: 'set!' expects a symbol", expr.Line, expr.Col)
	}
	val, err := Eval(expr.Elements[2], env)
	if err != nil {
		return nil, err
	}
	if !env.Update(target.StrVal, val) {
		return nil, fmt.Errorf("%d:%d: unbound variable '%s'", expr.Line, expr.Col, target.StrVal)
	}
	return Void, nil
}

func evalBegin(expr *Expr, env *Env) (*Value, error) {
	if len(expr.Elements) < 2 {
		return Void, nil
	}
	var result *Value
	for _, e := range expr.Elements[1:] {
		var err error
		result, err = Eval(e, env)
		if err != nil {
			return nil, err
		}
	}
	return result, nil
}

func callLambda(fn *Value, args []*Value, line, col int) (*Value, error) {
	if fn.RestParam != "" {
		if len(args) < len(fn.Params) {
			return nil, fmt.Errorf("%d:%d: wrong number of arguments: expected at least %d, got %d", line, col, len(fn.Params), len(args))
		}
		callEnv := NewEnv(fn.ClosureEnv)
		for i, param := range fn.Params {
			callEnv.Set(param, args[i])
		}
		// Collect rest args into a list
		rest := Null
		for i := len(args) - 1; i >= len(fn.Params); i-- {
			rest = PairValue(args[i], rest)
		}
		callEnv.Set(fn.RestParam, rest)
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
	if len(args) != len(fn.Params) {
		return nil, fmt.Errorf("%d:%d: wrong number of arguments: expected %d, got %d", line, col, len(fn.Params), len(args))
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

func evalDefineRecordType(expr *Expr, env *Env) (*Value, error) {
	// (define-record-type <name> (constructor field ...) predicate (field accessor) ...)
	if len(expr.Elements) < 5 {
		return nil, fmt.Errorf("%d:%d: define-record-type requires type name, constructor, predicate, and fields", expr.Line, expr.Col)
	}

	// Type name
	typeName := expr.Elements[1]
	if typeName.Type != ExprSymbol {
		return nil, fmt.Errorf("%d:%d: expected symbol for record type name", expr.Line, expr.Col)
	}

	// Constructor: (make-foo field1 field2 ...)
	ctorExpr := expr.Elements[2]
	if ctorExpr.Type != ExprList || len(ctorExpr.Elements) < 1 {
		return nil, fmt.Errorf("%d:%d: expected constructor specification", expr.Line, expr.Col)
	}
	ctorName := ctorExpr.Elements[0]
	if ctorName.Type != ExprSymbol {
		return nil, fmt.Errorf("%d:%d: expected symbol for constructor name", expr.Line, expr.Col)
	}
	ctorFields := make([]string, len(ctorExpr.Elements)-1)
	for i, f := range ctorExpr.Elements[1:] {
		if f.Type != ExprSymbol {
			return nil, fmt.Errorf("%d:%d: expected symbol for constructor field", expr.Line, expr.Col)
		}
		ctorFields[i] = f.StrVal
	}

	// Predicate
	predExpr := expr.Elements[3]
	if predExpr.Type != ExprSymbol {
		return nil, fmt.Errorf("%d:%d: expected symbol for predicate name", expr.Line, expr.Col)
	}

	// Field specs: (field-name accessor-name) ...
	fieldNames := make([]string, 0)
	accessorMap := make(map[string]int) // accessor-name -> field index
	for _, fieldSpec := range expr.Elements[4:] {
		if fieldSpec.Type != ExprList || len(fieldSpec.Elements) < 2 {
			return nil, fmt.Errorf("%d:%d: expected (field accessor) specification", expr.Line, expr.Col)
		}
		fieldName := fieldSpec.Elements[0]
		accessor := fieldSpec.Elements[1]
		if fieldName.Type != ExprSymbol || accessor.Type != ExprSymbol {
			return nil, fmt.Errorf("%d:%d: expected symbols in field specification", expr.Line, expr.Col)
		}
		fieldNames = append(fieldNames, fieldName.StrVal)
		accessorMap[accessor.StrVal] = len(fieldNames) - 1
	}

	// Build field index mapping: ctorField -> index in fieldNames
	ctorFieldIdx := make([]int, len(ctorFields))
	for i, cf := range ctorFields {
		found := false
		for j, fn := range fieldNames {
			if cf == fn {
				ctorFieldIdx[i] = j
				found = true
				break
			}
		}
		if !found {
			return nil, fmt.Errorf("%d:%d: constructor field '%s' not in field specs", expr.Line, expr.Col, cf)
		}
	}

	rt := &RecordType{Name: typeName.StrVal, Fields: fieldNames}

	// Define constructor
	numFields := len(fieldNames)
	constructor := &Value{
		Type: TypeLambda,
		Params: ctorFields,
		Body: nil, // handled specially via BuiltinFunc
		ClosureEnv: env,
	}
	// We'll use a Go-native approach: bind the constructor as a lambda that creates records
	// Instead, let's define it as a builtin-like closure
	rtCopy := rt
	ctorIdxCopy := ctorFieldIdx
	nf := numFields
	env.Set(ctorName.StrVal, makeGoFunc(func(args []*Value) (*Value, error) {
		if len(args) != len(ctorIdxCopy) {
			return nil, fmt.Errorf("wrong number of arguments to constructor %s", ctorName.StrVal)
		}
		fields := make([]*Value, nf)
		for i, idx := range ctorIdxCopy {
			fields[idx] = args[i]
		}
		return &Value{Type: TypeRecord, RecordType: rtCopy, RecordFields: fields}, nil
	}))
	_ = constructor

	// Define predicate
	env.Set(predExpr.StrVal, makeGoFunc(func(args []*Value) (*Value, error) {
		if len(args) != 1 {
			return nil, fmt.Errorf("wrong number of arguments to predicate %s", predExpr.StrVal)
		}
		if args[0].Type == TypeRecord && args[0].RecordType == rtCopy {
			return True, nil
		}
		return False, nil
	}))

	// Define accessors
	for accessorName, fieldIdx := range accessorMap {
		idx := fieldIdx // capture
		aName := accessorName
		env.Set(aName, makeGoFunc(func(args []*Value) (*Value, error) {
			if len(args) != 1 {
				return nil, fmt.Errorf("wrong number of arguments to accessor %s", aName)
			}
			if args[0].Type != TypeRecord || args[0].RecordType != rtCopy {
				return nil, fmt.Errorf("accessor %s applied to wrong type", aName)
			}
			return args[0].RecordFields[idx], nil
		}))
	}

	return Void, nil
}

func evalCond(expr *Expr, env *Env) (*Value, error) {
	for _, clause := range expr.Elements[1:] {
		if clause.Type != ExprList || len(clause.Elements) < 2 {
			return nil, fmt.Errorf("%d:%d: invalid cond clause", expr.Line, expr.Col)
		}
		// Check for else clause
		if clause.Elements[0].Type == ExprSymbol && clause.Elements[0].StrVal == "else" {
			var result *Value
			for _, e := range clause.Elements[1:] {
				var err error
				result, err = Eval(e, env)
				if err != nil {
					return nil, err
				}
			}
			return result, nil
		}
		test, err := Eval(clause.Elements[0], env)
		if err != nil {
			return nil, err
		}
		if isTruthy(test) {
			var result *Value
			for _, e := range clause.Elements[1:] {
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

func evalCaseLambda(expr *Expr, env *Env) (*Value, error) {
	// (case-lambda (formals body ...) ...)
	clauses := make([]CaseLambdaClause, 0, len(expr.Elements)-1)
	for _, clauseExpr := range expr.Elements[1:] {
		if clauseExpr.Type != ExprList || len(clauseExpr.Elements) < 2 {
			return nil, fmt.Errorf("%d:%d: invalid case-lambda clause", expr.Line, expr.Col)
		}
		formalsExpr := clauseExpr.Elements[0]
		if formalsExpr.Type != ExprList {
			return nil, fmt.Errorf("%d:%d: case-lambda clause formals must be a list", expr.Line, expr.Col)
		}
		params, restParam, err := parseParams(formalsExpr.Elements)
		if err != nil {
			return nil, err
		}
		clauses = append(clauses, CaseLambdaClause{
			Params:    params,
			RestParam: restParam,
			Body:      clauseExpr.Elements[1:],
		})
	}
	return &Value{
		Type:        TypeCaseLambda,
		CaseClauses: clauses,
		ClosureEnv:  env,
	}, nil
}

func callCaseLambda(fn *Value, args []*Value, line, col int) (*Value, error) {
	for _, clause := range fn.CaseClauses {
		if clause.RestParam != "" {
			if len(args) >= len(clause.Params) {
				callEnv := NewEnv(fn.ClosureEnv)
				for i, param := range clause.Params {
					callEnv.Set(param, args[i])
				}
				rest := Null
				for i := len(args) - 1; i >= len(clause.Params); i-- {
					rest = PairValue(args[i], rest)
				}
				callEnv.Set(clause.RestParam, rest)
				var result *Value
				for _, bodyExpr := range clause.Body {
					var err error
					result, err = Eval(bodyExpr, callEnv)
					if err != nil {
						return nil, err
					}
				}
				return result, nil
			}
		} else if len(args) == len(clause.Params) {
			callEnv := NewEnv(fn.ClosureEnv)
			for i, param := range clause.Params {
				callEnv.Set(param, args[i])
			}
			var result *Value
			for _, bodyExpr := range clause.Body {
				var err error
				result, err = Eval(bodyExpr, callEnv)
				if err != nil {
					return nil, err
				}
			}
			return result, nil
		}
	}
	return nil, fmt.Errorf("%d:%d: no matching clause in case-lambda for %d arguments", line, col, len(args))
}
