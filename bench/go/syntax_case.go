package ming

type syntaxContext struct {
	defEnv *environment
}

type syntaxObject struct {
	expr expr
}

type syntaxSequenceValue struct {
	captures []syntaxCapture
}

type syntaxCaseMacro struct {
	name        string
	transformer any
	defEnv      *environment
}

func (i *interpreter) parseMacroDefinition(name string, transformer expr, env *environment) (*macroExpander, error) {
	if form, ok := transformer.(*listExpr); ok && len(form.elements) > 0 {
		if keyword, ok := form.elements[0].(*symbolExpr); ok && keyword.value == "syntax-rules" {
			macro, err := parseSyntaxRuleMacro(name, transformer, env)
			if err != nil {
				return nil, err
			}
			return &macroExpander{impl: macro}, nil
		}
	}

	procedure, err := i.eval(transformer, env)
	if err != nil {
		return nil, err
	}
	if !isCallableValue(procedure) {
		return nil, newEvalError(transformer.exprPos(), "define-syntax requires a transformer procedure")
	}

	return &macroExpander{
		impl: &syntaxCaseMacro{
			name:        name,
			transformer: procedure,
			defEnv:      env,
		},
	}, nil
}

func (m *syntaxCaseMacro) expand(i *interpreter, invocation *listExpr) (expr, error) {
	result, err := i.callMacroTransformer(
		m.transformer,
		[]any{&syntaxObject{expr: cloneExpr(invocation)}},
		invocation.pos,
		m.defEnv,
	)
	if err != nil {
		return nil, err
	}

	syntax, ok := result.(*syntaxObject)
	if !ok {
		return nil, newEvalError(invocation.pos, "macro transformer for %s must return syntax", m.name)
	}
	return cloneExpr(syntax.expr), nil
}

func (i *interpreter) callMacroTransformer(procedure any, args []any, pos position, defEnv *environment) (any, error) {
	switch p := procedure.(type) {
	case *lambdaProcedure:
		if !p.matchesArity(len(args)) && !p.hasRest {
			return nil, newEvalError(pos, "wrong number of arguments: expected %d, got %d", len(p.params), len(args))
		}
		if !p.matchesArity(len(args)) && p.hasRest {
			return nil, newEvalError(pos, "wrong number of arguments: expected at least %d, got %d", len(p.params), len(args))
		}

		callEnv := newEnvironment(p.env)
		callEnv.syntaxCtx = &syntaxContext{defEnv: defEnv}
		for index, name := range p.params {
			callEnv.define(name, args[index])
		}
		if p.hasRest {
			callEnv.define(p.restName, buildList(args[len(p.params):]))
		}

		result, err := i.evalSequence(p.body, callEnv, true)
		if err != nil {
			return nil, err
		}
		return i.resolveTailResult(result)

	case *caseLambdaProcedure:
		clause := p.matchingClause(len(args))
		if clause == nil {
			return nil, newEvalError(pos, "wrong number of arguments: no matching case-lambda clause for %d argument(s)", len(args))
		}
		return i.callMacroTransformer(clause, args, pos, defEnv)

	case *builtinProcedure:
		result, err := p.Call(i, args, pos)
		if err != nil {
			return nil, err
		}
		return i.resolveTailResult(result)

	case callable:
		result, err := p.Call(i, args, pos)
		if err != nil {
			return nil, err
		}
		return i.resolveTailResult(result)

	default:
		return nil, newEvalError(pos, "attempt to call non-procedure")
	}
}

func (i *interpreter) evalSyntax(args []expr, pos position, env *environment) (any, error) {
	if len(args) != 1 {
		return nil, newEvalError(pos, "syntax expects exactly 1 argument")
	}

	defEnv := env.syntaxDefinitionEnv()
	if defEnv == nil {
		defEnv = env
	}

	captures := env.collectSyntaxBindings()
	patternVars := map[string]struct{}{}
	for name := range captures {
		patternVars[name] = struct{}{}
	}

	state := templateState{
		intp:           i,
		defEnv:         defEnv,
		patternVars:    patternVars,
		captures:       captures,
		freeIntroduced: map[string]string{},
	}
	expanded, err := state.expand(args[0], nil, nil)
	if err != nil {
		return nil, err
	}
	return &syntaxObject{expr: expanded}, nil
}

func (i *interpreter) evalSyntaxCase(args []expr, pos position, env *environment) (any, error) {
	if len(args) < 3 {
		return nil, newEvalError(pos, "syntax-case expects an input, literals, and at least 1 clause")
	}

	target, err := i.eval(args[0], env)
	if err != nil {
		return nil, err
	}

	input, ok := target.(*syntaxObject)
	if !ok {
		return nil, newEvalError(args[0].exprPos(), "syntax-case expects a syntax object")
	}

	literalList, ok := args[1].(*listExpr)
	if !ok {
		return nil, newEvalError(args[1].exprPos(), "syntax-case literals must be a list")
	}

	literals := map[string]struct{}{}
	for _, literalExpr := range literalList.elements {
		literal, ok := literalExpr.(*symbolExpr)
		if !ok {
			return nil, newEvalError(literalExpr.exprPos(), "syntax-case literals must be symbols")
		}
		literals[literal.value] = struct{}{}
	}

	for _, clauseExpr := range args[2:] {
		clause, ok := clauseExpr.(*listExpr)
		if !ok || len(clause.elements) < 2 || len(clause.elements) > 3 {
			return nil, newEvalError(clauseExpr.exprPos(), "syntax-case clauses must contain a pattern, an optional guard, and a body")
		}

		captures, matched := matchSyntaxPattern(clause.elements[0], input.expr, literals)
		if !matched {
			continue
		}

		clauseEnv := newEnvironment(env)
		for name, capture := range captures {
			clauseEnv.defineSyntaxBinding(name, capture)
		}

		bodyIndex := 1
		if len(clause.elements) == 3 {
			fender, err := i.eval(clause.elements[1], clauseEnv)
			if err != nil {
				return nil, err
			}
			if !isTruthy(fender) {
				continue
			}
			bodyIndex = 2
		}

		return i.eval(clause.elements[bodyIndex], clauseEnv)
	}

	return nil, newEvalError(pos, "syntax-case found no matching clause")
}

func (i *interpreter) evalWithSyntax(args []expr, pos position, env *environment) (any, error) {
	if len(args) < 2 {
		return nil, newEvalError(pos, "with-syntax expects bindings and a body")
	}

	bindings, ok := args[0].(*listExpr)
	if !ok {
		return nil, newEvalError(args[0].exprPos(), "with-syntax bindings must be a list")
	}

	bodyEnv := newEnvironment(env)
	for _, bindingExpr := range bindings.elements {
		binding, ok := bindingExpr.(*listExpr)
		if !ok || len(binding.elements) != 2 {
			return nil, newEvalError(bindingExpr.exprPos(), "with-syntax bindings must contain a pattern and an expression")
		}

		value, err := i.eval(binding.elements[1], env)
		if err != nil {
			return nil, err
		}

		capture, err := captureFromSyntaxValue(value, binding.elements[1].exprPos())
		if err != nil {
			return nil, err
		}

		if err := bindSyntaxPattern(bodyEnv, binding.elements[0], capture); err != nil {
			return nil, err
		}
	}

	return i.evalSequence(args[1:], bodyEnv, true)
}

func bindSyntaxPattern(env *environment, pattern expr, capture syntaxCapture) error {
	if symbol, ok := pattern.(*symbolExpr); ok {
		if symbol.value == "..." {
			return newEvalError(symbol.pos, "with-syntax pattern cannot be ellipsis")
		}
		if symbol.value == "_" {
			return nil
		}
		env.defineSyntaxBinding(symbol.value, capture)
		return nil
	}

	if capture.isRepeated {
		return newEvalError(pattern.exprPos(), "with-syntax pattern requires a syntax object")
	}

	captures, matched := matchSyntaxPattern(pattern, capture.scalar, map[string]struct{}{})
	if !matched {
		return newEvalError(pattern.exprPos(), "with-syntax pattern did not match")
	}

	for name, matchedCapture := range captures {
		env.defineSyntaxBinding(name, matchedCapture)
	}
	return nil
}

func syntaxRuntimeValue(capture syntaxCapture) any {
	cloned := cloneSyntaxCapture(capture)
	if cloned.isRepeated {
		return &syntaxSequenceValue{captures: cloned.repeated}
	}
	return &syntaxObject{expr: cloned.scalar}
}

func captureFromSyntaxValue(value any, pos position) (syntaxCapture, error) {
	switch syntax := value.(type) {
	case *syntaxObject:
		return syntaxCapture{scalar: cloneExpr(syntax.expr)}, nil
	case *syntaxSequenceValue:
		return syntaxCapture{
			repeated: cloneSyntaxCaptureSlice(syntax.captures),
			isRepeated: true,
		}, nil
	default:
		return syntaxCapture{}, newEvalError(pos, "expected a syntax object")
	}
}

func datumFromSyntaxValue(value any, pos position) (any, error) {
	capture, err := captureFromSyntaxValue(value, pos)
	if err != nil {
		return nil, err
	}
	return syntaxCaptureDatum(capture)
}

func syntaxCaptureDatum(capture syntaxCapture) (any, error) {
	if capture.isRepeated {
		values := make([]any, len(capture.repeated))
		for index, repeated := range capture.repeated {
			value, err := syntaxCaptureDatum(repeated)
			if err != nil {
				return nil, err
			}
			values[index] = value
		}
		return buildList(values), nil
	}
	return datumFromExpr(capture.scalar)
}

func exprFromDatum(value any, pos position) (expr, error) {
	switch datum := value.(type) {
	case int:
		return &integerExpr{value: datum, pos: pos}, nil
	case rationalValue:
		return &rationalExpr{value: datum, pos: pos}, nil
	case inexactValue:
		return &inexactExpr{value: datum, pos: pos}, nil
	case bool:
		return &booleanExpr{value: datum, pos: pos}, nil
	case stringValue:
		return &stringExpr{value: string(datum), pos: pos}, nil
	case *mutableString:
		return &stringExpr{value: datum.String(), pos: pos}, nil
	case symbolValue:
		return &symbolExpr{value: string(datum), pos: pos}, nil
	case charValue:
		return &charExpr{value: rune(datum), pos: pos}, nil
	case emptyList:
		return &listExpr{pos: pos}, nil
	case *pairValue:
		return exprFromPairDatum(datum, pos)
	case *syntaxObject:
		return cloneExpr(datum.expr), nil
	default:
		return nil, newEvalError(pos, "datum->syntax expects a datum")
	}
}

func exprFromPairDatum(pair *pairValue, pos position) (expr, error) {
	elements := []expr{}
	seen := map[*pairValue]struct{}{}
	current := any(pair)

	for {
		node, ok := current.(*pairValue)
		if !ok {
			tail, err := exprFromDatum(current, pos)
			if err != nil {
				return nil, err
			}
			return &listExpr{elements: elements, tail: tail, pos: pos}, nil
		}

		if _, ok := seen[node]; ok {
			return nil, newEvalError(pos, "datum->syntax expects a finite datum")
		}
		seen[node] = struct{}{}

		converted, err := exprFromDatum(node.car, pos)
		if err != nil {
			return nil, err
		}
		elements = append(elements, converted)

		switch next := node.cdr.(type) {
		case emptyList:
			return &listExpr{elements: elements, pos: pos}, nil
		case *pairValue:
			current = next
		default:
			tail, err := exprFromDatum(next, pos)
			if err != nil {
				return nil, err
			}
			return &listExpr{elements: elements, tail: tail, pos: pos}, nil
		}
	}
}

func syntaxSourcePos(value any, fallback position) position {
	switch syntax := value.(type) {
	case *syntaxObject:
		if syntax.expr != nil {
			return syntax.expr.exprPos()
		}
	case *syntaxSequenceValue:
		if len(syntax.captures) > 0 {
			return syntaxCapturePos(syntax.captures[0], fallback)
		}
	}
	return fallback
}

func syntaxCapturePos(capture syntaxCapture, fallback position) position {
	if capture.isRepeated {
		if len(capture.repeated) > 0 {
			return syntaxCapturePos(capture.repeated[0], fallback)
		}
		return fallback
	}
	if capture.scalar == nil {
		return fallback
	}
	return capture.scalar.exprPos()
}

func cloneSyntaxCapture(capture syntaxCapture) syntaxCapture {
	if capture.isRepeated {
		return syntaxCapture{
			repeated:   cloneSyntaxCaptureSlice(capture.repeated),
			isRepeated: true,
		}
	}
	return syntaxCapture{scalar: cloneExpr(capture.scalar)}
}

func cloneSyntaxCaptureSlice(captures []syntaxCapture) []syntaxCapture {
	cloned := make([]syntaxCapture, len(captures))
	for index, capture := range captures {
		cloned[index] = cloneSyntaxCapture(capture)
	}
	return cloned
}
