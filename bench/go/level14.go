package ming

type vectorValue struct {
	elements []value
}

type uninitializedValue struct {
	name string
}

type doBinding struct {
	name bindingName
	init expr
	step expr
}

func (it *interpreter) evalCase(scope *env, list *listExpr) (value, error) {
	if len(list.elements) < 2 {
		return nil, newEvalError(ErrSyntax, "case: expected key and clauses", list.at)
	}

	key, err := it.eval(list.elements[1], scope)
	if err != nil {
		return nil, err
	}

	clauses := list.elements[2:]
	for idx, clauseExpr := range clauses {
		clause, ok := clauseExpr.(*listExpr)
		if !ok || len(clause.elements) == 0 {
			return nil, newEvalError(ErrSyntax, "case: expected non-empty clause", clauseExpr.pos())
		}

		if sym, ok := clause.elements[0].(*symbolExpr); ok && sym.name == "else" {
			if idx != len(clauses)-1 {
				return nil, newEvalError(ErrSyntax, "case: else clause must be last", sym.at)
			}
			if len(clause.elements) == 1 {
				return voidValue{}, nil
			}
			return it.evalSequence(scope, clause.elements[1:])
		}

		datums, ok := clause.elements[0].(*listExpr)
		if !ok {
			return nil, newEvalError(ErrSyntax, "case: expected datum list", clause.elements[0].pos())
		}

		matched := false
		for _, datumExpr := range datums.elements {
			datum, err := datumToValue(datumExpr)
			if err != nil {
				return nil, err
			}
			if caseDatumEqual(key, datum) {
				matched = true
				break
			}
		}
		if !matched {
			continue
		}

		if len(clause.elements) == 1 {
			return voidValue{}, nil
		}
		return it.evalSequence(scope, clause.elements[1:])
	}

	return voidValue{}, nil
}

func caseDatumEqual(left value, right value) bool {
	if isNumberValue(left) && isNumberValue(right) {
		return numberEqual(left, right)
	}
	return eqValue(left, right)
}

func (it *interpreter) evalLetRec(scope *env, list *listExpr, sequential bool) (value, error) {
	if len(list.elements) < 3 {
		return nil, newEvalError(ErrSyntax, "letrec: expected bindings and body", list.at)
	}

	bindingList, ok := list.elements[1].(*listExpr)
	if !ok {
		return nil, newEvalError(ErrSyntax, "letrec: expected binding list", list.elements[1].pos())
	}

	bindings, err := parseLetBindings(bindingList)
	if err != nil {
		return nil, err
	}

	body := list.elements[2:]
	if len(body) == 0 {
		return nil, newEvalError(ErrSyntax, "letrec: expected body", list.at)
	}

	letEnv := newEnv(scope)
	defs := make([]*binding, len(bindings))
	for i, bindingSpec := range bindings {
		defs[i] = it.defineBindingName(letEnv, bindingSpec.name, &uninitializedValue{name: bindingSpec.name.name})
	}

	if sequential {
		for i, bindingSpec := range bindings {
			current, err := it.eval(bindingSpec.init, letEnv)
			if err != nil {
				return nil, err
			}
			defs[i].value = current
		}
	} else {
		values := make([]value, len(bindings))
		for i, bindingSpec := range bindings {
			current, err := it.eval(bindingSpec.init, letEnv)
			if err != nil {
				return nil, err
			}
			values[i] = current
		}
		for i, current := range values {
			defs[i].value = current
		}
	}

	return it.evalSequence(letEnv, body)
}

func (it *interpreter) evalDo(scope *env, list *listExpr) (value, error) {
	if len(list.elements) < 3 {
		return nil, newEvalError(ErrSyntax, "do: expected bindings, test, and body", list.at)
	}

	bindingList, ok := list.elements[1].(*listExpr)
	if !ok {
		return nil, newEvalError(ErrSyntax, "do: expected binding list", list.elements[1].pos())
	}

	testClause, ok := list.elements[2].(*listExpr)
	if !ok || len(testClause.elements) == 0 {
		return nil, newEvalError(ErrSyntax, "do: expected test clause", list.elements[2].pos())
	}

	bindings, err := parseDoBindings(bindingList)
	if err != nil {
		return nil, err
	}

	loopEnv := newEnv(scope)
	defs := make([]*binding, len(bindings))
	for i, bindingSpec := range bindings {
		current, err := it.eval(bindingSpec.init, scope)
		if err != nil {
			return nil, err
		}
		defs[i] = it.defineBindingName(loopEnv, bindingSpec.name, current)
	}

	commands := list.elements[3:]
	for {
		test, err := it.eval(testClause.elements[0], loopEnv)
		if err != nil {
			return nil, err
		}
		if isTruthy(test) {
			if len(testClause.elements) == 1 {
				return voidValue{}, nil
			}
			return it.evalSequence(loopEnv, testClause.elements[1:])
		}

		if len(commands) > 0 {
			if _, err := it.evalSequence(loopEnv, commands); err != nil {
				return nil, err
			}
		}

		nextValues := make([]value, len(bindings))
		for i, bindingSpec := range bindings {
			if bindingSpec.step == nil {
				nextValues[i] = defs[i].value
				continue
			}

			current, err := it.eval(bindingSpec.step, loopEnv)
			if err != nil {
				return nil, err
			}
			nextValues[i] = current
		}

		for i, next := range nextValues {
			defs[i].value = next
		}
	}
}

func parseDoBindings(list *listExpr) ([]doBinding, error) {
	bindings := make([]doBinding, 0, len(list.elements))
	for _, bindingExpr := range list.elements {
		binding, ok := bindingExpr.(*listExpr)
		if !ok || len(binding.elements) < 2 || len(binding.elements) > 3 {
			return nil, newEvalError(ErrSyntax, "do: expected binding with init and optional step", bindingExpr.pos())
		}

		name, ok := binding.elements[0].(*symbolExpr)
		if !ok {
			return nil, newEvalError(ErrSyntax, "do: expected binding name", binding.elements[0].pos())
		}

		spec := doBinding{
			name: bindingName{name: name.name, key: name.key},
			init: binding.elements[1],
		}
		if len(binding.elements) == 3 {
			spec.step = binding.elements[2]
		}
		bindings = append(bindings, spec)
	}
	return bindings, nil
}

func builtinVector(_ *interpreter, args []value, _ position) (value, error) {
	return &vectorValue{elements: append([]value(nil), args...)}, nil
}

func builtinMakeVector(_ *interpreter, args []value, callPos position) (value, error) {
	if len(args) != 1 && len(args) != 2 {
		return nil, wrongArgCount(callPos, "make-vector", "expected 1 or 2 arguments")
	}

	length, err := expectIndex(args[0], callPos)
	if err != nil {
		return nil, err
	}

	fill := value(voidValue{})
	if len(args) == 2 {
		fill = args[1]
	}

	elements := make([]value, length)
	for i := range elements {
		elements[i] = fill
	}
	return &vectorValue{elements: elements}, nil
}

func builtinVectorPred(_ *interpreter, args []value, callPos position) (value, error) {
	if len(args) != 1 {
		return nil, wrongArgCount(callPos, "vector?", "expected exactly 1 argument")
	}
	_, ok := args[0].(*vectorValue)
	return ok, nil
}

func builtinVectorLength(_ *interpreter, args []value, callPos position) (value, error) {
	if len(args) != 1 {
		return nil, wrongArgCount(callPos, "vector-length", "expected exactly 1 argument")
	}

	vec, err := expectVector(args[0], callPos)
	if err != nil {
		return nil, err
	}
	return int64(len(vec.elements)), nil
}

func builtinVectorRef(_ *interpreter, args []value, callPos position) (value, error) {
	if len(args) != 2 {
		return nil, wrongArgCount(callPos, "vector-ref", "expected exactly 2 arguments")
	}

	vec, err := expectVector(args[0], callPos)
	if err != nil {
		return nil, err
	}
	index, err := expectIndex(args[1], callPos)
	if err != nil {
		return nil, err
	}
	if index >= len(vec.elements) {
		return nil, newEvalError(ErrOutOfRange, "vector-ref: index out of range", callPos)
	}
	return vec.elements[index], nil
}

func builtinVectorSet(_ *interpreter, args []value, callPos position) (value, error) {
	if len(args) != 3 {
		return nil, wrongArgCount(callPos, "vector-set!", "expected exactly 3 arguments")
	}

	vec, err := expectVector(args[0], callPos)
	if err != nil {
		return nil, err
	}
	index, err := expectIndex(args[1], callPos)
	if err != nil {
		return nil, err
	}
	if index >= len(vec.elements) {
		return nil, newEvalError(ErrOutOfRange, "vector-set!: index out of range", callPos)
	}
	vec.elements[index] = args[2]
	return voidValue{}, nil
}

func builtinVectorToList(_ *interpreter, args []value, callPos position) (value, error) {
	if len(args) != 1 {
		return nil, wrongArgCount(callPos, "vector->list", "expected exactly 1 argument")
	}

	vec, err := expectVector(args[0], callPos)
	if err != nil {
		return nil, err
	}
	return buildList(append([]value(nil), vec.elements...)), nil
}

func expectVector(v value, pos position) (*vectorValue, error) {
	vec, ok := v.(*vectorValue)
	if !ok {
		return nil, newEvalError(ErrTypeMismatch, "expected vector", pos)
	}
	return vec, nil
}
