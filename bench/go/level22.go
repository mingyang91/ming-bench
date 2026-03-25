package ming

type transformerMacro struct {
	name string
	proc value
}

type syntaxObject struct {
	datum expr
}

type syntaxSequenceValue struct {
	items []expr
}

func isSyntaxTransformer(v value) bool {
	switch v.(type) {
	case *syntaxRuleMacro, *transformerMacro:
		return true
	default:
		return false
	}
}

func expectSyntaxObject(v value, pos position) (*syntaxObject, error) {
	syntax, ok := v.(*syntaxObject)
	if !ok {
		return nil, newEvalError(ErrTypeMismatch, "expected syntax object", pos)
	}
	return syntax, nil
}

func builtinSyntaxToDatum(_ *interpreter, args []value, callPos position) (value, error) {
	if len(args) != 1 {
		return nil, wrongArgCount(callPos, "syntax->datum", "expected exactly 1 argument")
	}

	syntax, err := expectSyntaxObject(args[0], callPos)
	if err != nil {
		return nil, err
	}
	return datumToValue(syntax.datum)
}

func builtinDatumToSyntax(_ *interpreter, args []value, callPos position) (value, error) {
	if len(args) != 2 {
		return nil, wrongArgCount(callPos, "datum->syntax", "expected exactly 2 arguments")
	}

	context, err := expectSyntaxObject(args[0], callPos)
	if err != nil {
		return nil, err
	}

	at := callPos
	if context.datum != nil {
		at = context.datum.pos()
	}

	datum, err := datumValueToExpr(args[1], at)
	if err != nil {
		return nil, err
	}
	return &syntaxObject{datum: datum}, nil
}

func datumValueToExpr(v value, at position) (expr, error) {
	switch current := v.(type) {
	case int64:
		return &intExpr{value: current, at: at}, nil
	case rationalValue:
		return &rationalExpr{value: current, at: at}, nil
	case inexactValue:
		return &inexactExpr{value: current, at: at}, nil
	case bool:
		return &boolExpr{value: current, at: at}, nil
	case *stringValue:
		return &stringExpr{value: current.text(), at: at}, nil
	case charValue:
		return &charExpr{value: rune(current), at: at}, nil
	case symbolValue:
		return &symbolExpr{name: string(current), at: at}, nil
	case emptyListValue:
		return &listExpr{at: at}, nil
	case *pairValue:
		elements := []expr{}
		seen := map[*pairValue]struct{}{}
		rest := value(current)
		for {
			switch pair := rest.(type) {
			case *pairValue:
				if _, ok := seen[pair]; ok {
					return nil, newEvalError(ErrTypeMismatch, "datum->syntax: expected datum", at)
				}
				seen[pair] = struct{}{}

				element, err := datumValueToExpr(pair.car, at)
				if err != nil {
					return nil, err
				}
				elements = append(elements, element)
				rest = pair.cdr
			case emptyListValue:
				return buildDottedExprList(elements, nil, at), nil
			default:
				tail, err := datumValueToExpr(rest, at)
				if err != nil {
					return nil, err
				}
				return buildDottedExprList(elements, tail, at), nil
			}
		}
	default:
		return nil, newEvalError(ErrTypeMismatch, "datum->syntax: expected datum", at)
	}
}

func (it *interpreter) evalSyntax(scope *env, list *listExpr) (value, error) {
	if len(list.elements) != 2 {
		return nil, newEvalError(ErrSyntax, "syntax: expected exactly one template", list.at)
	}

	match, patternVars := syntaxMatchFromScope(scope)
	expanded, err := it.expandTemplate(
		list.elements[1],
		&syntaxRuleMacro{defEnv: scope},
		&syntaxRule{patternVars: patternVars},
		match,
		nil,
		nil,
	)
	if err != nil {
		return nil, err
	}

	return &syntaxObject{datum: expanded}, nil
}

func syntaxMatchFromScope(scope *env) (*syntaxMatch, map[string]struct{}) {
	match := newSyntaxMatch()
	patternVars := map[string]struct{}{}
	seen := map[string]struct{}{}

	for current := scope; current != nil; current = current.parent {
		for name, binding := range current.values {
			if _, ok := seen[name]; ok {
				continue
			}

			switch value := binding.value.(type) {
			case *syntaxObject:
				match.single[name] = cloneExpr(value.datum)
			case *syntaxSequenceValue:
				match.repeated[name] = cloneExprSlice(value.items)
			default:
				continue
			}

			seen[name] = struct{}{}
			patternVars[name] = struct{}{}
		}
	}

	return match, patternVars
}

func cloneExprSlice(items []expr) []expr {
	if len(items) == 0 {
		return nil
	}

	cloned := make([]expr, len(items))
	for i, item := range items {
		cloned[i] = cloneExpr(item)
	}
	return cloned
}

func parseSyntaxLiterals(node expr, pos position) (map[string]struct{}, error) {
	list, ok := node.(*listExpr)
	if !ok {
		return nil, newEvalError(ErrSyntax, "syntax-case: expected literal identifier list", pos)
	}

	literals := map[string]struct{}{}
	for _, literalExpr := range list.elements {
		literal, ok := literalExpr.(*symbolExpr)
		if !ok || literal.name == "..." {
			return nil, newEvalError(ErrSyntax, "syntax-case: expected literal identifier", literalExpr.pos())
		}
		literals[literal.name] = struct{}{}
	}
	return literals, nil
}

func matchPatternTopLevel(pattern expr, input expr, macro *syntaxRuleMacro, rule *syntaxRule, match *syntaxMatch, keywordHead bool) bool {
	switch current := pattern.(type) {
	case *listExpr:
		other, ok := input.(*listExpr)
		return ok && matchPatternList(current.elements, other.elements, macro, rule, match, keywordHead)
	default:
		return matchPattern(pattern, input, macro, rule, match, keywordHead, false)
	}
}

func bindSyntaxMatch(it *interpreter, scope *env, match *syntaxMatch) {
	for name, node := range match.single {
		it.defineName(scope, name, &syntaxObject{datum: cloneExpr(node)})
	}
	for name, nodes := range match.repeated {
		it.defineName(scope, name, &syntaxSequenceValue{items: cloneExprSlice(nodes)})
	}
}

func matchSyntaxPattern(pattern expr, input expr, literals map[string]struct{}, keywordHead bool) (*syntaxMatch, error) {
	patternVars := map[string]struct{}{}
	collectPatternVars(pattern, literals, keywordHead, patternVars)

	macro := &syntaxRuleMacro{literals: literals}
	rule := &syntaxRule{
		pattern:     cloneExpr(pattern),
		patternVars: patternVars,
	}
	match := newSyntaxMatch()
	if !matchPatternTopLevel(pattern, input, macro, rule, match, keywordHead) {
		return nil, nil
	}
	return match, nil
}

func (it *interpreter) evalSyntaxCase(scope *env, list *listExpr) (value, error) {
	if len(list.elements) < 4 {
		return nil, newEvalError(ErrSyntax, "syntax-case: expected syntax object, literals, and clauses", list.at)
	}

	target, err := it.eval(list.elements[1], scope)
	if err != nil {
		return nil, err
	}

	syntax, err := expectSyntaxObject(target, list.elements[1].pos())
	if err != nil {
		return nil, err
	}

	literals, err := parseSyntaxLiterals(list.elements[2], list.elements[2].pos())
	if err != nil {
		return nil, err
	}

	for _, clauseExpr := range list.elements[3:] {
		clause, ok := clauseExpr.(*listExpr)
		if !ok || len(clause.elements) < 2 || len(clause.elements) > 3 {
			return nil, newEvalError(ErrSyntax, "syntax-case: expected clause with pattern and template", clauseExpr.pos())
		}

		match, err := matchSyntaxPattern(clause.elements[0], syntax.datum, literals, false)
		if err != nil {
			return nil, err
		}
		if match == nil {
			continue
		}

		clauseScope := newEnv(scope)
		bindSyntaxMatch(it, clauseScope, match)

		bodyIndex := 1
		if len(clause.elements) == 3 {
			fender, err := it.eval(clause.elements[1], clauseScope)
			if err != nil {
				return nil, err
			}
			if !isTruthy(fender) {
				continue
			}
			bodyIndex = 2
		}

		return it.eval(clause.elements[bodyIndex], clauseScope)
	}

	return nil, newEvalError(ErrSyntax, "syntax-case: no matching clause", list.at)
}

func (it *interpreter) evalWithSyntax(scope *env, list *listExpr) (value, error) {
	if len(list.elements) < 3 {
		return nil, newEvalError(ErrSyntax, "with-syntax: expected bindings and body", list.at)
	}

	bindingList, ok := list.elements[1].(*listExpr)
	if !ok {
		return nil, newEvalError(ErrSyntax, "with-syntax: expected binding list", list.elements[1].pos())
	}

	bodyScope := newEnv(scope)
	for _, bindingExpr := range bindingList.elements {
		binding, ok := bindingExpr.(*listExpr)
		if !ok || len(binding.elements) != 2 {
			return nil, newEvalError(ErrSyntax, "with-syntax: expected binding pair", bindingExpr.pos())
		}

		current, err := it.eval(binding.elements[1], scope)
		if err != nil {
			return nil, err
		}

		syntax, err := expectSyntaxObject(current, binding.elements[1].pos())
		if err != nil {
			return nil, err
		}

		match, err := matchSyntaxPattern(binding.elements[0], syntax.datum, map[string]struct{}{}, false)
		if err != nil {
			return nil, err
		}
		if match == nil {
			return nil, newEvalError(ErrSyntax, "with-syntax: pattern did not match", binding.elements[0].pos())
		}

		bindSyntaxMatch(it, bodyScope, match)
	}

	return it.evalSequence(bodyScope, list.elements[2:])
}
