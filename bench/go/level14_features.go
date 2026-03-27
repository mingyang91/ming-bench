package ming

import "strings"

type uninitializedValue struct {
	name string
}

type vectorValue struct {
	elems []value
}

type doBinding struct {
	name    string
	init    locatedExpr
	step    locatedExpr
	hasStep bool
}

func (u uninitializedValue) schemeString() string {
	return "#<uninitialized " + u.name + ">"
}

func (uninitializedValue) isTruthy() bool {
	return true
}

func (v *vectorValue) schemeString() string {
	return formatVector(v, outputModeWrite)
}

func (*vectorValue) isTruthy() bool {
	return true
}

func formatVector(v *vectorValue, mode outputMode) string {
	return newFormatState().formatVector(v, mode)
}

func (s *formatState) formatVector(v *vectorValue, mode outputMode) string {
	if _, seen := s.activeVectors[v]; seen {
		return "#<cycle>"
	}

	s.activeVectors[v] = struct{}{}
	defer delete(s.activeVectors, v)

	var builder strings.Builder
	builder.WriteString("#(")
	for i, elem := range v.elems {
		if i > 0 {
			builder.WriteByte(' ')
		}
		builder.WriteString(s.formatValue(elem, mode))
	}
	builder.WriteByte(')')
	return builder.String()
}

func registerLevel14Builtins(global *env) {
	global.define("eqv?", builtinProc{name: "eqv?", fn: evalEqvPred})
	global.define("vector", builtinProc{name: "vector", fn: evalVector})
	global.define("make-vector", builtinProc{name: "make-vector", fn: evalMakeVector})
	global.define("vector-ref", builtinProc{name: "vector-ref", fn: evalVectorRef})
	global.define("vector-set!", builtinProc{name: "vector-set!", fn: evalVectorSet})
	global.define("vector-length", builtinProc{name: "vector-length", fn: evalVectorLength})
	global.define("vector?", builtinProc{name: "vector?", fn: evalVectorPred})
	global.define("vector->list", builtinProc{name: "vector->list", fn: evalVectorToList})
	global.define("list->vector", builtinProc{name: "list->vector", fn: evalListToVector})
}

func evalEqvPred(args []value) (value, error) {
	if len(args) != 2 {
		return nil, newCurrentEvalError("'eqv?' expects exactly 2 arguments")
	}
	return boolValue(schemeEqv(args[0], args[1])), nil
}

func evalVector(args []value) (value, error) {
	elems := make([]value, len(args))
	copy(elems, args)
	return &vectorValue{elems: elems}, nil
}

func evalMakeVector(args []value) (value, error) {
	if len(args) != 1 && len(args) != 2 {
		return nil, newCurrentEvalError("'make-vector' expects 1 or 2 arguments")
	}

	length, err := expectInteger(args[0])
	if err != nil {
		return nil, err
	}
	if length < 0 {
		return nil, newCurrentEvalError("'make-vector' expects a non-negative length")
	}

	fill := value(boolValue(false))
	if len(args) == 2 {
		fill = args[1]
	}

	elems := make([]value, length)
	for i := range elems {
		elems[i] = fill
	}
	return &vectorValue{elems: elems}, nil
}

func evalVectorRef(args []value) (value, error) {
	if len(args) != 2 {
		return nil, newCurrentEvalError("'vector-ref' expects exactly 2 arguments")
	}

	vector, err := expectVector(args[0])
	if err != nil {
		return nil, err
	}
	index, err := expectNonNegativeIndex(args[1], "vector-ref")
	if err != nil {
		return nil, err
	}
	if index >= len(vector.elems) {
		return nil, newCurrentEvalError("'vector-ref' index out of range")
	}

	return vector.elems[index], nil
}

func evalVectorSet(args []value) (value, error) {
	if len(args) != 3 {
		return nil, newCurrentEvalError("'vector-set!' expects exactly 3 arguments")
	}

	vector, err := expectVector(args[0])
	if err != nil {
		return nil, err
	}
	index, err := expectNonNegativeIndex(args[1], "vector-set!")
	if err != nil {
		return nil, err
	}
	if index >= len(vector.elems) {
		return nil, newCurrentEvalError("'vector-set!' index out of range")
	}

	vector.elems[index] = args[2]
	return voidValue{}, nil
}

func evalVectorLength(args []value) (value, error) {
	if len(args) != 1 {
		return nil, newCurrentEvalError("'vector-length' expects exactly 1 argument")
	}

	vector, err := expectVector(args[0])
	if err != nil {
		return nil, err
	}
	return newExactInteger(len(vector.elems)), nil
}

func evalVectorPred(args []value) (value, error) {
	return evalTypePredicate(args, "vector?", func(v value) bool {
		_, ok := v.(*vectorValue)
		return ok
	})
}

func evalVectorToList(args []value) (value, error) {
	if len(args) != 1 {
		return nil, newCurrentEvalError("'vector->list' expects exactly 1 argument")
	}

	vector, err := expectVector(args[0])
	if err != nil {
		return nil, err
	}
	return listFromValues(vector.elems), nil
}

func evalListToVector(args []value) (value, error) {
	if len(args) != 1 {
		return nil, newCurrentEvalError("'list->vector' expects exactly 1 argument")
	}

	elems, err := properListElements(args[0])
	if err != nil {
		return nil, err
	}
	return &vectorValue{elems: elems}, nil
}

func schemeEqv(a, b value) bool {
	switch av := a.(type) {
	case numberValue:
		bv, ok := b.(numberValue)
		return ok && av.equal(bv)
	default:
		return schemeEq(a, b)
	}
}

func expectVector(v value) (*vectorValue, error) {
	vector, ok := v.(*vectorValue)
	if !ok {
		return nil, newCurrentEvalError("expected vector, got %s", v.schemeString())
	}
	return vector, nil
}

func evalLetrec(parts []locatedExpr, env *env) (value, error) {
	return evalLetrecForm(parts, env, false)
}

func evalLetrecStar(parts []locatedExpr, env *env) (value, error) {
	return evalLetrecForm(parts, env, true)
}

func evalLetrecTail(parts []locatedExpr, env *env) (value, *tailCall, error) {
	return evalLetrecFormTail(parts, env, false)
}

func evalLetrecStarTail(parts []locatedExpr, env *env) (value, *tailCall, error) {
	return evalLetrecFormTail(parts, env, true)
}

func evalLetrecForm(parts []locatedExpr, env *env, sequential bool) (value, error) {
	formName := "letrec"
	if sequential {
		formName = "letrec*"
	}
	if len(parts) < 2 {
		return nil, newCurrentEvalError("'%s' expects bindings and a body", formName)
	}

	bindingExprs, ok := parts[0].form.(listExpr)
	if !ok {
		return nil, newEvalError(parts[0].pos, "'%s' bindings must be a list", formName)
	}

	bindings, err := parseLetBindings(bindingExprs)
	if err != nil {
		return nil, err
	}

	letrecEnv := newEnv(env)
	if sequential {
		for _, item := range bindings {
			slot := &binding{value: uninitializedValue{name: item.name}}
			letrecEnv.defineBinding(item.name, slot)

			v, err := evalExpr(item.init, letrecEnv)
			if err != nil {
				return nil, err
			}
			slot.value = v
		}
	} else {
		slots := make([]*binding, len(bindings))
		for i, item := range bindings {
			slot := &binding{value: uninitializedValue{name: item.name}}
			letrecEnv.defineBinding(item.name, slot)
			slots[i] = slot
		}

		for i, item := range bindings {
			v, err := evalExpr(item.init, letrecEnv)
			if err != nil {
				return nil, err
			}
			slots[i].value = v
		}
	}

	return evalSequence(parts[1:], letrecEnv)
}

func evalLetrecFormTail(parts []locatedExpr, env *env, sequential bool) (value, *tailCall, error) {
	formName := "letrec"
	if sequential {
		formName = "letrec*"
	}
	if len(parts) < 2 {
		return nil, nil, newCurrentEvalError("'%s' expects bindings and a body", formName)
	}

	bindingExprs, ok := parts[0].form.(listExpr)
	if !ok {
		return nil, nil, newEvalError(parts[0].pos, "'%s' bindings must be a list", formName)
	}

	bindings, err := parseLetBindings(bindingExprs)
	if err != nil {
		return nil, nil, err
	}

	letrecEnv := newEnv(env)
	if sequential {
		for _, item := range bindings {
			slot := &binding{value: uninitializedValue{name: item.name}}
			letrecEnv.defineBinding(item.name, slot)

			v, err := evalExpr(item.init, letrecEnv)
			if err != nil {
				return nil, nil, err
			}
			slot.value = v
		}
	} else {
		slots := make([]*binding, len(bindings))
		for i, item := range bindings {
			slot := &binding{value: uninitializedValue{name: item.name}}
			letrecEnv.defineBinding(item.name, slot)
			slots[i] = slot
		}

		for i, item := range bindings {
			v, err := evalExpr(item.init, letrecEnv)
			if err != nil {
				return nil, nil, err
			}
			slots[i].value = v
		}
	}

	return evalSequenceTail(parts[1:], letrecEnv)
}

func evalCase(parts []locatedExpr, env *env) (value, error) {
	if len(parts) < 1 {
		return nil, newCurrentEvalError("'case' expects a key and at least 1 clause")
	}

	key, err := evalExpr(parts[0], env)
	if err != nil {
		return nil, err
	}

	for i, clauseExpr := range parts[1:] {
		clause, ok := clauseExpr.form.(listExpr)
		if !ok || len(clause) == 0 {
			return nil, newEvalError(clauseExpr.pos, "'case' clauses must be non-empty lists")
		}

		if keyword, ok := clause[0].form.(symbolExpr); ok && string(keyword) == "else" {
			if i != len(parts[1:])-1 {
				return nil, newEvalError(clause[0].pos, "'case' else clause must be last")
			}
			if len(clause) == 1 {
				return voidValue{}, nil
			}
			return evalSequence(clause[1:], env)
		}

		datums, ok := clause[0].form.(listExpr)
		if !ok {
			return nil, newEvalError(clause[0].pos, "'case' clause datums must be a list")
		}

		for _, datumExpr := range datums {
			datum, err := quoteExpr(datumExpr)
			if err != nil {
				return nil, err
			}
			if schemeEqv(key, datum) {
				if len(clause) == 1 {
					return voidValue{}, nil
				}
				return evalSequence(clause[1:], env)
			}
		}
	}

	return voidValue{}, nil
}

func evalCaseTail(parts []locatedExpr, env *env) (value, *tailCall, error) {
	if len(parts) < 1 {
		return nil, nil, newCurrentEvalError("'case' expects a key and at least 1 clause")
	}

	key, err := evalExpr(parts[0], env)
	if err != nil {
		return nil, nil, err
	}

	for i, clauseExpr := range parts[1:] {
		clause, ok := clauseExpr.form.(listExpr)
		if !ok || len(clause) == 0 {
			return nil, nil, newEvalError(clauseExpr.pos, "'case' clauses must be non-empty lists")
		}

		if keyword, ok := clause[0].form.(symbolExpr); ok && string(keyword) == "else" {
			if i != len(parts[1:])-1 {
				return nil, nil, newEvalError(clause[0].pos, "'case' else clause must be last")
			}
			if len(clause) == 1 {
				return voidValue{}, nil, nil
			}
			return evalSequenceTail(clause[1:], env)
		}

		datums, ok := clause[0].form.(listExpr)
		if !ok {
			return nil, nil, newEvalError(clause[0].pos, "'case' clause datums must be a list")
		}

		for _, datumExpr := range datums {
			datum, err := quoteExpr(datumExpr)
			if err != nil {
				return nil, nil, err
			}
			if schemeEqv(key, datum) {
				if len(clause) == 1 {
					return voidValue{}, nil, nil
				}
				return evalSequenceTail(clause[1:], env)
			}
		}
	}

	return voidValue{}, nil, nil
}

func evalDo(parts []locatedExpr, env *env) (value, error) {
	if len(parts) < 2 {
		return nil, newCurrentEvalError("'do' expects bindings, a termination clause, and optional body expressions")
	}

	bindingExprs, ok := parts[0].form.(listExpr)
	if !ok {
		return nil, newEvalError(parts[0].pos, "'do' bindings must be a list")
	}
	bindings, err := parseDoBindings(bindingExprs)
	if err != nil {
		return nil, err
	}

	testClause, ok := parts[1].form.(listExpr)
	if !ok || len(testClause) == 0 {
		return nil, newEvalError(parts[1].pos, "'do' termination clause must be a non-empty list")
	}

	initValues := make([]value, len(bindings))
	for i, item := range bindings {
		v, err := evalExpr(item.init, env)
		if err != nil {
			return nil, err
		}
		initValues[i] = v
	}

	doEnv := newEnv(env)
	slots := make([]*binding, len(bindings))
	for i, item := range bindings {
		slot := &binding{value: initValues[i]}
		doEnv.defineBinding(item.name, slot)
		slots[i] = slot
	}

	for {
		test, err := evalExpr(testClause[0], doEnv)
		if err != nil {
			return nil, err
		}
		if test.isTruthy() {
			if len(testClause) == 1 {
				return voidValue{}, nil
			}
			return evalSequence(testClause[1:], doEnv)
		}

		if len(parts) > 2 {
			if _, err := evalSequence(parts[2:], doEnv); err != nil {
				return nil, err
			}
		}

		nextValues := make([]value, len(bindings))
		for i, item := range bindings {
			if !item.hasStep {
				nextValues[i] = slots[i].value
				continue
			}

			v, err := evalExpr(item.step, doEnv)
			if err != nil {
				return nil, err
			}
			nextValues[i] = v
		}

		for i := range slots {
			slots[i].value = nextValues[i]
		}
	}
}

func evalDoTail(parts []locatedExpr, env *env) (value, *tailCall, error) {
	if len(parts) < 2 {
		return nil, nil, newCurrentEvalError("'do' expects bindings, a termination clause, and optional body expressions")
	}

	bindingExprs, ok := parts[0].form.(listExpr)
	if !ok {
		return nil, nil, newEvalError(parts[0].pos, "'do' bindings must be a list")
	}
	bindings, err := parseDoBindings(bindingExprs)
	if err != nil {
		return nil, nil, err
	}

	testClause, ok := parts[1].form.(listExpr)
	if !ok || len(testClause) == 0 {
		return nil, nil, newEvalError(parts[1].pos, "'do' termination clause must be a non-empty list")
	}

	initValues := make([]value, len(bindings))
	for i, item := range bindings {
		v, err := evalExpr(item.init, env)
		if err != nil {
			return nil, nil, err
		}
		initValues[i] = v
	}

	doEnv := newEnv(env)
	slots := make([]*binding, len(bindings))
	for i, item := range bindings {
		slot := &binding{value: initValues[i]}
		doEnv.defineBinding(item.name, slot)
		slots[i] = slot
	}

	for {
		test, err := evalExpr(testClause[0], doEnv)
		if err != nil {
			return nil, nil, err
		}
		if test.isTruthy() {
			if len(testClause) == 1 {
				return voidValue{}, nil, nil
			}
			return evalSequenceTail(testClause[1:], doEnv)
		}

		if len(parts) > 2 {
			if _, err := evalSequence(parts[2:], doEnv); err != nil {
				return nil, nil, err
			}
		}

		nextValues := make([]value, len(bindings))
		for i, item := range bindings {
			if !item.hasStep {
				nextValues[i] = slots[i].value
				continue
			}

			v, err := evalExpr(item.step, doEnv)
			if err != nil {
				return nil, nil, err
			}
			nextValues[i] = v
		}

		for i := range slots {
			slots[i].value = nextValues[i]
		}
	}
}

func parseDoBindings(items listExpr) ([]doBinding, error) {
	bindings := make([]doBinding, 0, len(items))
	seen := make(map[string]struct{}, len(items))

	for _, item := range items {
		bindingExpr, ok := item.form.(listExpr)
		if !ok || (len(bindingExpr) != 2 && len(bindingExpr) != 3) {
			return nil, newEvalError(item.pos, "'do' bindings must be (name init) or (name init step) forms")
		}

		name, ok := bindingExpr[0].form.(symbolExpr)
		if !ok {
			return nil, newEvalError(bindingExpr[0].pos, "'do' binding names must be symbols")
		}
		if _, exists := seen[string(name)]; exists {
			return nil, newEvalError(bindingExpr[0].pos, "duplicate binding: %s", string(name))
		}
		seen[string(name)] = struct{}{}

		binding := doBinding{
			name: string(name),
			init: bindingExpr[1],
		}
		if len(bindingExpr) == 3 {
			binding.step = bindingExpr[2]
			binding.hasStep = true
		}

		bindings = append(bindings, binding)
	}

	return bindings, nil
}
