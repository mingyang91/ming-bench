package ming

import "fmt"

type vectorValue struct {
	elements []value
}

type vectorNode struct {
	elements []node
	pos      sourcePos
}

type namedBindingSpec struct {
	name string
	expr node
}

type doBindingSpec struct {
	name    string
	init    node
	step    node
	hasStep bool
}

func builtinEqv() builtinProc {
	return func(args []value) (value, error) {
		if len(args) != 2 {
			return nil, &EvalError{Message: "eqv? expects exactly 2 arguments"}
		}
		return booleanValue(eqValues(args[0], args[1])), nil
	}
}

func builtinVector() builtinProc {
	return func(args []value) (value, error) {
		return &vectorValue{elements: copyValues(args)}, nil
	}
}

func builtinMakeVector() builtinProc {
	return func(args []value) (value, error) {
		if len(args) < 1 || len(args) > 2 {
			return nil, &EvalError{Message: "make-vector expects 1 or 2 arguments"}
		}

		length, err := expectIntegerValue(args[0])
		if err != nil {
			return nil, err
		}
		if length < 0 {
			return nil, &EvalError{Message: "make-vector length must be non-negative"}
		}

		fill := value(booleanValue(false))
		if len(args) == 2 {
			fill = args[1]
		}

		elements := make([]value, length)
		for i := range elements {
			elements[i] = fill
		}

		return &vectorValue{elements: elements}, nil
	}
}

func builtinVectorRef() builtinProc {
	return func(args []value) (value, error) {
		if len(args) != 2 {
			return nil, &EvalError{Message: "vector-ref expects exactly 2 arguments"}
		}

		vector, err := expectVectorValue(args[0])
		if err != nil {
			return nil, err
		}
		index, err := expectIntegerValue(args[1])
		if err != nil {
			return nil, err
		}
		if index < 0 || index >= len(vector.elements) {
			return nil, &EvalError{Message: "vector-ref index out of range"}
		}

		return vector.elements[index], nil
	}
}

func builtinVectorSet() builtinProc {
	return func(args []value) (value, error) {
		if len(args) != 3 {
			return nil, &EvalError{Message: "vector-set! expects exactly 3 arguments"}
		}

		vector, err := expectVectorValue(args[0])
		if err != nil {
			return nil, err
		}
		index, err := expectIntegerValue(args[1])
		if err != nil {
			return nil, err
		}
		if index < 0 || index >= len(vector.elements) {
			return nil, &EvalError{Message: "vector-set! index out of range"}
		}

		vector.elements[index] = args[2]
		return voidValue{}, nil
	}
}

func builtinVectorLength() builtinProc {
	return func(args []value) (value, error) {
		if len(args) != 1 {
			return nil, &EvalError{Message: "vector-length expects exactly 1 argument"}
		}

		vector, err := expectVectorValue(args[0])
		if err != nil {
			return nil, err
		}
		return integerValue(len(vector.elements)), nil
	}
}

func builtinVectorToList() builtinProc {
	return func(args []value) (value, error) {
		if len(args) != 1 {
			return nil, &EvalError{Message: "vector->list expects exactly 1 argument"}
		}

		vector, err := expectVectorValue(args[0])
		if err != nil {
			return nil, err
		}
		return makeList(copyValues(vector.elements)), nil
	}
}

func builtinListToVector() builtinProc {
	return func(args []value) (value, error) {
		if len(args) != 1 {
			return nil, &EvalError{Message: "list->vector expects exactly 1 argument"}
		}

		list, err := expectListValue(args[0])
		if err != nil {
			return nil, err
		}
		return &vectorValue{elements: copyValues(list.elements)}, nil
	}
}

func evalCase(args []node, env *environment) (value, *evalStep, error) {
	if len(args) < 2 {
		return nil, nil, &EvalError{Message: "case expects a key and at least 1 clause"}
	}

	key, err := eval(args[0], env)
	if err != nil {
		return nil, nil, err
	}

	clauses := args[1:]
	for i, clauseExpr := range clauses {
		clause, ok := clauseExpr.(listNode)
		if !ok || len(clause.elements) == 0 {
			return nil, nil, &EvalError{Message: "case clauses must be non-empty lists"}
		}

		if name, ok := symbolName(clause.elements[0]); ok && name == "else" {
			if i != len(clauses)-1 {
				return nil, nil, &EvalError{Message: "else clause must be last"}
			}
			if len(clause.elements) == 1 {
				return voidValue{}, nil, nil
			}
			return prepareSequence(clause.elements[1:], env)
		}

		datums, ok := clause.elements[0].(listNode)
		if !ok {
			return nil, nil, &EvalError{Message: "case clause datums must be a list"}
		}

		for _, datumExpr := range datums.elements {
			datum, err := datumFromNode(datumExpr)
			if err != nil {
				return nil, nil, err
			}
			if eqValues(key, datum) {
				if len(clause.elements) == 1 {
					return voidValue{}, nil, nil
				}
				return prepareSequence(clause.elements[1:], env)
			}
		}
	}

	return voidValue{}, nil, nil
}

func evalLetrec(args []node, env *environment, sequential bool) (value, *evalStep, error) {
	formName := "letrec"
	if sequential {
		formName = "letrec*"
	}

	if len(args) < 2 {
		return nil, nil, &EvalError{Message: fmt.Sprintf("%s expects bindings and a body", formName)}
	}

	bindingList, ok := args[0].(listNode)
	if !ok {
		return nil, nil, &EvalError{Message: fmt.Sprintf("%s bindings must be a list", formName)}
	}

	specs, err := parseNamedBindings(bindingList.elements, formName)
	if err != nil {
		return nil, nil, err
	}

	letEnv := newEnvironment(env)
	cells := make([]*binding, len(specs))
	for i, spec := range specs {
		cell := &binding{value: voidValue{}}
		letEnv.defineBinding(spec.name, cell)
		cells[i] = cell
	}

	if sequential {
		for i, spec := range specs {
			val, err := eval(spec.expr, letEnv)
			if err != nil {
				return nil, nil, err
			}
			cells[i].value = val
		}
	} else {
		values := make([]value, len(specs))
		for i, spec := range specs {
			val, err := eval(spec.expr, letEnv)
			if err != nil {
				return nil, nil, err
			}
			values[i] = val
		}
		for i := range cells {
			cells[i].value = values[i]
		}
	}

	return prepareSequence(args[1:], letEnv)
}

func evalDo(args []node, env *environment) (value, error) {
	if len(args) < 2 {
		return nil, &EvalError{Message: "do expects bindings, a termination clause, and an optional body"}
	}

	bindingList, ok := args[0].(listNode)
	if !ok {
		return nil, &EvalError{Message: "do bindings must be a list"}
	}

	bindings, err := parseDoBindings(bindingList.elements)
	if err != nil {
		return nil, err
	}

	termination, ok := args[1].(listNode)
	if !ok || len(termination.elements) == 0 {
		return nil, &EvalError{Message: "do termination clause must be a non-empty list"}
	}

	initialValues := make([]value, len(bindings))
	for i, spec := range bindings {
		val, err := eval(spec.init, env)
		if err != nil {
			return nil, err
		}
		initialValues[i] = val
	}

	loopEnv := newEnvironment(env)
	cells := make([]*binding, len(bindings))
	for i, spec := range bindings {
		cell := &binding{value: initialValues[i]}
		loopEnv.defineBinding(spec.name, cell)
		cells[i] = cell
	}

	for {
		done, err := eval(termination.elements[0], loopEnv)
		if err != nil {
			return nil, err
		}
		if isTruthy(done) {
			if len(termination.elements) == 1 {
				return voidValue{}, nil
			}
			return evalSequence(termination.elements[1:], loopEnv)
		}

		for _, expr := range args[2:] {
			if _, err := eval(expr, loopEnv); err != nil {
				return nil, err
			}
		}

		nextValues := make([]value, len(bindings))
		for i, spec := range bindings {
			if !spec.hasStep {
				nextValues[i] = cells[i].value
				continue
			}

			val, err := eval(spec.step, loopEnv)
			if err != nil {
				return nil, err
			}
			nextValues[i] = val
		}

		for i, cell := range cells {
			cell.value = nextValues[i]
		}
	}
}

func parseNamedBindings(bindingExprs []node, formName string) ([]namedBindingSpec, error) {
	specs := make([]namedBindingSpec, len(bindingExprs))
	for i, bindingExpr := range bindingExprs {
		binding, ok := bindingExpr.(listNode)
		if !ok || len(binding.elements) != 2 {
			return nil, &EvalError{Message: fmt.Sprintf("%s bindings must contain name/value pairs", formName)}
		}

		name, ok := symbolName(binding.elements[0])
		if !ok {
			return nil, &EvalError{Message: fmt.Sprintf("%s binding names must be symbols", formName)}
		}

		for j := 0; j < i; j++ {
			if specs[j].name == name {
				return nil, &EvalError{Message: fmt.Sprintf("duplicate binding: %s", name)}
			}
		}

		specs[i] = namedBindingSpec{
			name: name,
			expr: binding.elements[1],
		}
	}
	return specs, nil
}

func parseDoBindings(bindingExprs []node) ([]doBindingSpec, error) {
	specs := make([]doBindingSpec, len(bindingExprs))
	for i, bindingExpr := range bindingExprs {
		binding, ok := bindingExpr.(listNode)
		if !ok || len(binding.elements) < 2 || len(binding.elements) > 3 {
			return nil, &EvalError{Message: "do bindings must contain a name, init, and optional step"}
		}

		name, ok := symbolName(binding.elements[0])
		if !ok {
			return nil, &EvalError{Message: "do binding names must be symbols"}
		}

		for j := 0; j < i; j++ {
			if specs[j].name == name {
				return nil, &EvalError{Message: fmt.Sprintf("duplicate binding: %s", name)}
			}
		}

		specs[i] = doBindingSpec{
			name: name,
			init: binding.elements[1],
		}
		if len(binding.elements) == 3 {
			specs[i].hasStep = true
			specs[i].step = binding.elements[2]
		}

	}
	return specs, nil
}

func expectVectorValue(v value) (*vectorValue, error) {
	vector, ok := v.(*vectorValue)
	if !ok {
		return nil, &EvalError{Message: "expected vector"}
	}
	return vector, nil
}

func isVectorValue(v value) bool {
	_, ok := v.(*vectorValue)
	return ok
}

func (p *parser) parseVector() (node, error) {
	pos := p.currentPos()
	p.offset += 2

	var elements []node
	for {
		p.skipWhitespaceAndComments()
		if p.eof() {
			return nil, errorAt(pos, "unterminated vector")
		}
		if p.peek() == ')' {
			p.offset++
			return vectorNode{elements: elements, pos: pos}, nil
		}

		element, err := p.parseExpr()
		if err != nil {
			return nil, err
		}
		elements = append(elements, element)
	}
}

func (p *parser) hasPrefix(prefix string) bool {
	return len(p.source[p.offset:]) >= len(prefix) && p.source[p.offset:p.offset+len(prefix)] == prefix
}
