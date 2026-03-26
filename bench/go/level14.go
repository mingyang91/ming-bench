package ming

import "fmt"

type letBinding struct {
	name  symbolExpr
	value any
}

type doBinding struct {
	name    symbolExpr
	init    any
	step    any
	hasStep bool
}

func evalLetrec(scope *env, args []any, sequential bool) (any, error) {
	formName := "letrec"
	if sequential {
		formName = "letrec*"
	}

	if len(args) < 2 {
		return nil, &EvalError{Message: fmt.Sprintf("%s expects bindings and a body", formName)}
	}

	bindingsExpr, ok := args[0].(listExpr)
	if !ok {
		return nil, &EvalError{Message: fmt.Sprintf("%s bindings must be a list", formName)}
	}

	bindings, err := parseLetBindingSpecs(bindingsExpr.elements, formName)
	if err != nil {
		return nil, err
	}

	letrecScope := newEnv(scope)
	for _, binding := range bindings {
		letrecScope.defineSymbol(binding.name, uninitializedValue{})
	}

	if sequential {
		for _, binding := range bindings {
			value, err := eval(letrecScope, binding.value)
			if err != nil {
				return nil, err
			}
			letrecScope.setSymbol(binding.name, value)
		}
	} else {
		values := make([]any, len(bindings))
		for i, binding := range bindings {
			value, err := eval(letrecScope, binding.value)
			if err != nil {
				return nil, err
			}
			values[i] = value
		}
		for i, binding := range bindings {
			letrecScope.setSymbol(binding.name, values[i])
		}
	}

	return evalSequence(letrecScope, args[1:])
}

func evalCase(scope *env, args []any) (any, error) {
	if len(args) < 1 {
		return nil, &EvalError{Message: "case expects a key and clauses"}
	}

	key, err := eval(scope, args[0])
	if err != nil {
		return nil, err
	}

	for i, clauseExpr := range args[1:] {
		clause, ok := clauseExpr.(listExpr)
		if !ok || len(clause.elements) == 0 {
			return nil, exprSourcePos(clauseExpr).errorf("case clauses must be non-empty lists")
		}

		if symbol, ok := clause.elements[0].(symbolExpr); ok && symbol.name == "else" {
			if i != len(args)-2 {
				return nil, symbol.pos.errorf("case else clause must be last")
			}
			if len(clause.elements) == 1 {
				return voidValue{}, nil
			}
			return evalSequence(scope, clause.elements[1:])
		}

		datums, ok := clause.elements[0].(listExpr)
		if !ok {
			return nil, exprSourcePos(clause.elements[0]).errorf("case clause datums must be a list")
		}

		for _, datumExpr := range datums.elements {
			if valuesEqv(key, quoteDatum(datumExpr)) {
				if len(clause.elements) == 1 {
					return voidValue{}, nil
				}
				return evalSequence(scope, clause.elements[1:])
			}
		}
	}

	return voidValue{}, nil
}

func evalDo(scope *env, args []any) (any, error) {
	if len(args) < 2 {
		return nil, &EvalError{Message: "do expects bindings, a test clause, and optional body expressions"}
	}

	bindingsExpr, ok := args[0].(listExpr)
	if !ok {
		return nil, &EvalError{Message: "do bindings must be a list"}
	}

	testClause, ok := args[1].(listExpr)
	if !ok || len(testClause.elements) == 0 {
		return nil, exprSourcePos(args[1]).errorf("do test clause must be a non-empty list")
	}

	bindings, err := parseDoBindings(bindingsExpr.elements)
	if err != nil {
		return nil, err
	}

	loopScope := newEnv(scope)
	initValues := make([]any, len(bindings))
	for i, binding := range bindings {
		value, err := eval(scope, binding.init)
		if err != nil {
			return nil, err
		}
		initValues[i] = value
	}
	for i, binding := range bindings {
		loopScope.defineSymbol(binding.name, initValues[i])
	}

	for {
		testValue, err := eval(loopScope, testClause.elements[0])
		if err != nil {
			return nil, err
		}
		if isTruthy(testValue) {
			if len(testClause.elements) == 1 {
				return voidValue{}, nil
			}
			return evalSequence(loopScope, testClause.elements[1:])
		}

		if _, err := evalSequence(loopScope, args[2:]); err != nil {
			return nil, err
		}

		nextValues := make([]any, len(bindings))
		for i, binding := range bindings {
			if binding.hasStep {
				value, err := eval(loopScope, binding.step)
				if err != nil {
					return nil, err
				}
				nextValues[i] = value
				continue
			}

			value, ok := loopScope.lookupSymbol(binding.name)
			if !ok {
				return nil, binding.name.pos.errorf("unbound variable: %s", binding.name.name)
			}
			nextValues[i] = value
		}
		for i, binding := range bindings {
			loopScope.setSymbol(binding.name, nextValues[i])
		}
	}
}

func parseLetBindingSpecs(bindings []any, formName string) ([]letBinding, error) {
	specs := make([]letBinding, 0, len(bindings))
	for _, bindingExpr := range bindings {
		binding, ok := bindingExpr.(listExpr)
		if !ok || len(binding.elements) != 2 {
			return nil, exprSourcePos(bindingExpr).errorf("%s bindings must be name/value pairs", formName)
		}

		name, ok := binding.elements[0].(symbolExpr)
		if !ok {
			return nil, exprSourcePos(binding.elements[0]).errorf("%s binding name must be a symbol", formName)
		}

		specs = append(specs, letBinding{
			name:  name,
			value: binding.elements[1],
		})
	}
	return specs, nil
}

func parseDoBindings(bindings []any) ([]doBinding, error) {
	specs := make([]doBinding, 0, len(bindings))
	for _, bindingExpr := range bindings {
		binding, ok := bindingExpr.(listExpr)
		if !ok || len(binding.elements) < 2 || len(binding.elements) > 3 {
			return nil, exprSourcePos(bindingExpr).errorf("do bindings must be (name init [step])")
		}

		name, ok := binding.elements[0].(symbolExpr)
		if !ok {
			return nil, exprSourcePos(binding.elements[0]).errorf("do binding name must be a symbol")
		}

		spec := doBinding{
			name: name,
			init: binding.elements[1],
		}
		if len(binding.elements) == 3 {
			spec.step = binding.elements[2]
			spec.hasStep = true
		}
		specs = append(specs, spec)
	}
	return specs, nil
}

func builtinEqv(args []any) (any, error) {
	if len(args) != 2 {
		return nil, &EvalError{Message: "eqv? expects exactly 2 arguments"}
	}
	return valuesEqv(args[0], args[1]), nil
}

func builtinVector(args []any) (any, error) {
	elements := append([]any(nil), args...)
	return &vectorValue{elements: elements}, nil
}

func builtinMakeVector(args []any) (any, error) {
	if len(args) != 1 && len(args) != 2 {
		return nil, &EvalError{Message: "make-vector expects 1 or 2 arguments"}
	}

	length, err := expectNonNegativeIndex(args[0], "make-vector")
	if err != nil {
		return nil, err
	}

	fill := any(voidValue{})
	if len(args) == 2 {
		fill = args[1]
	}

	elements := make([]any, length)
	for i := range elements {
		elements[i] = fill
	}
	return &vectorValue{elements: elements}, nil
}

func builtinVectorRef(args []any) (any, error) {
	if len(args) != 2 {
		return nil, &EvalError{Message: "vector-ref expects exactly 2 arguments"}
	}

	vector, err := expectVector(args[0], "vector-ref")
	if err != nil {
		return nil, err
	}
	index, err := expectNonNegativeIndex(args[1], "vector-ref")
	if err != nil {
		return nil, err
	}
	if index >= int64(len(vector.elements)) {
		return nil, &EvalError{Message: "vector-ref index out of range"}
	}
	return vector.elements[index], nil
}

func builtinVectorSet(args []any) (any, error) {
	if len(args) != 3 {
		return nil, &EvalError{Message: "vector-set! expects exactly 3 arguments"}
	}

	vector, err := expectVector(args[0], "vector-set!")
	if err != nil {
		return nil, err
	}
	index, err := expectNonNegativeIndex(args[1], "vector-set!")
	if err != nil {
		return nil, err
	}
	if index >= int64(len(vector.elements)) {
		return nil, &EvalError{Message: "vector-set! index out of range"}
	}
	vector.elements[index] = args[2]
	return voidValue{}, nil
}

func builtinVectorLength(args []any) (any, error) {
	if len(args) != 1 {
		return nil, &EvalError{Message: "vector-length expects exactly 1 argument"}
	}

	vector, err := expectVector(args[0], "vector-length")
	if err != nil {
		return nil, err
	}
	return int64(len(vector.elements)), nil
}

func builtinVectorPredicate(args []any) (any, error) {
	if len(args) != 1 {
		return nil, &EvalError{Message: "vector? expects exactly 1 argument"}
	}
	_, ok := args[0].(*vectorValue)
	return ok, nil
}

func builtinVectorToList(args []any) (any, error) {
	if len(args) != 1 {
		return nil, &EvalError{Message: "vector->list expects exactly 1 argument"}
	}

	vector, err := expectVector(args[0], "vector->list")
	if err != nil {
		return nil, err
	}
	return makeListValue(vector.elements), nil
}

func builtinListToVector(args []any) (any, error) {
	if len(args) != 1 {
		return nil, &EvalError{Message: "list->vector expects exactly 1 argument"}
	}

	elements, err := properListElements(args[0], "list->vector")
	if err != nil {
		return nil, err
	}
	return &vectorValue{elements: elements}, nil
}

func expectVector(value any, who string) (*vectorValue, error) {
	vector, ok := value.(*vectorValue)
	if !ok {
		return nil, &EvalError{Message: fmt.Sprintf("%s expects a vector, got %s", who, typeName(value))}
	}
	return vector, nil
}

func valuesEqv(left, right any) bool {
	return valuesEq(left, right)
}
