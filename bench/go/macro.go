package ming

import "fmt"

type syntaxRuleMacro struct {
	name     string
	key      string
	defEnv   *env
	literals map[string]struct{}
	rules    []syntaxRule
}

type syntaxRule struct {
	pattern     expr
	template    expr
	patternVars map[string]struct{}
}

type syntaxMatch struct {
	single   map[string]expr
	repeated map[string][]expr
}

type syntaxMatchSnapshot struct {
	single   map[string]expr
	repeated map[string][]expr
}

func newSyntaxMatch() *syntaxMatch {
	return &syntaxMatch{
		single:   map[string]expr{},
		repeated: map[string][]expr{},
	}
}

func (m *syntaxMatch) snapshot() syntaxMatchSnapshot {
	single := make(map[string]expr, len(m.single))
	for name, node := range m.single {
		single[name] = node
	}

	repeated := make(map[string][]expr, len(m.repeated))
	for name, nodes := range m.repeated {
		repeated[name] = append([]expr(nil), nodes...)
	}

	return syntaxMatchSnapshot{
		single:   single,
		repeated: repeated,
	}
}

func (m *syntaxMatch) restore(snapshot syntaxMatchSnapshot) {
	m.single = snapshot.single
	m.repeated = snapshot.repeated
}

func (m *syntaxMatch) capture(name string, node expr, repeated bool) bool {
	if repeated {
		m.repeated[name] = append(m.repeated[name], node)
		return true
	}

	if existing, ok := m.single[name]; ok {
		return syntaxExprEqual(existing, node)
	}

	m.single[name] = node
	return true
}

func (it *interpreter) evalDefineSyntax(scope *env, list *listExpr) (value, error) {
	if len(list.elements) != 3 {
		return nil, newEvalError(ErrSyntax, "define-syntax: expected a name and transformer", list.at)
	}

	name, ok := list.elements[1].(*symbolExpr)
	if !ok {
		return nil, newEvalError(ErrSyntax, "define-syntax: expected syntax name", list.elements[1].pos())
	}

	binding := it.defineSymbol(scope, name, nil)
	macro, err := it.parseSyntaxRules(name.name, binding.key, list.elements[2], scope)
	if err != nil {
		return nil, err
	}

	binding.value = macro
	return voidValue{}, nil
}

func (it *interpreter) parseSyntaxRules(name string, key string, spec expr, scope *env) (*syntaxRuleMacro, error) {
	form, ok := spec.(*listExpr)
	if !ok || len(form.elements) < 3 {
		return nil, newEvalError(ErrSyntax, "define-syntax: expected syntax-rules form", spec.pos())
	}

	head, ok := form.elements[0].(*symbolExpr)
	if !ok || head.name != "syntax-rules" {
		return nil, newEvalError(ErrSyntax, "define-syntax: expected syntax-rules form", form.elements[0].pos())
	}

	literalList, ok := form.elements[1].(*listExpr)
	if !ok {
		return nil, newEvalError(ErrSyntax, "syntax-rules: expected literal identifier list", form.elements[1].pos())
	}

	literals := map[string]struct{}{}
	for _, literalExpr := range literalList.elements {
		literal, ok := literalExpr.(*symbolExpr)
		if !ok || literal.name == "..." {
			return nil, newEvalError(ErrSyntax, "syntax-rules: expected literal identifier", literalExpr.pos())
		}
		literals[literal.name] = struct{}{}
	}

	rules := make([]syntaxRule, 0, len(form.elements)-2)
	for _, clauseExpr := range form.elements[2:] {
		clause, ok := clauseExpr.(*listExpr)
		if !ok || len(clause.elements) != 2 {
			return nil, newEvalError(ErrSyntax, "syntax-rules: expected (pattern template) clause", clauseExpr.pos())
		}

		patternVars := map[string]struct{}{}
		collectPatternVars(clause.elements[0], literals, true, patternVars)

		rules = append(rules, syntaxRule{
			pattern:     cloneExpr(clause.elements[0]),
			template:    cloneExpr(clause.elements[1]),
			patternVars: patternVars,
		})
	}

	return &syntaxRuleMacro{
		name:     name,
		key:      key,
		defEnv:   scope,
		literals: literals,
		rules:    rules,
	}, nil
}

func (it *interpreter) expandMacroCall(list *listExpr, scope *env) (expr, bool, error) {
	head, ok := list.elements[0].(*symbolExpr)
	if !ok {
		return nil, false, nil
	}

	binding, ok := it.lookupSymbolBinding(scope, head)
	if !ok {
		return nil, false, nil
	}

	macro, ok := binding.value.(*syntaxRuleMacro)
	if !ok {
		return nil, false, nil
	}

	resolved, err := it.resolveSyntax(list, scope)
	if err != nil {
		return nil, false, err
	}

	resolvedList, ok := resolved.(*listExpr)
	if !ok {
		return nil, false, newEvalError(ErrSyntax, "macro expansion expected list form", list.at)
	}

	expanded, err := it.expandSyntaxRulesMacro(macro, resolvedList)
	if err != nil {
		return nil, false, err
	}
	return expanded, true, nil
}

func (it *interpreter) expandSyntaxRulesMacro(macro *syntaxRuleMacro, form *listExpr) (expr, error) {
	for _, rule := range macro.rules {
		match := newSyntaxMatch()
		if !matchRule(rule.pattern, form, macro, &rule, match) {
			continue
		}

		return it.expandTemplate(rule.template, macro, &rule, match, nil, nil)
	}

	return nil, newEvalError(ErrSyntax, fmt.Sprintf("%s: no matching syntax-rules clause", macro.name), form.at)
}

func matchRule(pattern expr, form *listExpr, macro *syntaxRuleMacro, rule *syntaxRule, match *syntaxMatch) bool {
	switch pat := pattern.(type) {
	case *listExpr:
		return matchPatternList(pat.elements, form.elements, macro, rule, match, true)
	default:
		return matchPattern(pattern, form, macro, rule, match, false, false)
	}
}

func matchPatternList(patterns []expr, inputs []expr, macro *syntaxRuleMacro, rule *syntaxRule, match *syntaxMatch, keywordHead bool) bool {
	var walk func(patternIndex int, inputIndex int) bool
	walk = func(patternIndex int, inputIndex int) bool {
		if patternIndex == len(patterns) {
			return inputIndex == len(inputs)
		}

		pattern := patterns[patternIndex]
		hasEllipsis := patternIndex+1 < len(patterns) && isEllipsisExpr(patterns[patternIndex+1])
		if hasEllipsis {
			for count := 0; inputIndex+count <= len(inputs); count++ {
				snapshot := match.snapshot()
				ok := true
				for offset := 0; offset < count; offset++ {
					if !matchPattern(pattern, inputs[inputIndex+offset], macro, rule, match, keywordHead && patternIndex == 0, true) {
						ok = false
						break
					}
				}
				if ok && walk(patternIndex+2, inputIndex+count) {
					return true
				}
				match.restore(snapshot)
			}
			return false
		}

		if inputIndex >= len(inputs) {
			return false
		}
		if !matchPattern(pattern, inputs[inputIndex], macro, rule, match, keywordHead && patternIndex == 0, false) {
			return false
		}
		return walk(patternIndex+1, inputIndex+1)
	}

	return walk(0, 0)
}

func matchPattern(pattern expr, input expr, macro *syntaxRuleMacro, rule *syntaxRule, match *syntaxMatch, keywordPosition bool, repeated bool) bool {
	switch pat := pattern.(type) {
	case *intExpr:
		other, ok := input.(*intExpr)
		return ok && pat.value == other.value
	case *boolExpr:
		other, ok := input.(*boolExpr)
		return ok && pat.value == other.value
	case *stringExpr:
		other, ok := input.(*stringExpr)
		return ok && pat.value == other.value
	case *charExpr:
		other, ok := input.(*charExpr)
		return ok && pat.value == other.value
	case *symbolExpr:
		switch {
		case pat.name == "_":
			return true
		case pat.name == "...":
			return false
		case keywordPosition:
			other, ok := input.(*symbolExpr)
			return ok && pat.name == other.name
		case isLiteralIdentifier(macro, pat.name):
			other, ok := input.(*symbolExpr)
			return ok && pat.name == other.name
		case hasPatternVar(rule, pat.name):
			return match.capture(pat.name, input, repeated)
		default:
			other, ok := input.(*symbolExpr)
			return ok && pat.name == other.name
		}
	case *listExpr:
		other, ok := input.(*listExpr)
		if !ok {
			return false
		}
		return matchPatternList(pat.elements, other.elements, macro, rule, match, false)
	default:
		return false
	}
}

func collectPatternVars(node expr, literals map[string]struct{}, keywordPosition bool, vars map[string]struct{}) {
	switch current := node.(type) {
	case *symbolExpr:
		if keywordPosition || current.name == "_" || current.name == "..." {
			return
		}
		if _, ok := literals[current.name]; ok {
			return
		}
		vars[current.name] = struct{}{}
	case *listExpr:
		for i, element := range current.elements {
			collectPatternVars(element, literals, keywordPosition && i == 0, vars)
		}
	}
}

func isLiteralIdentifier(macro *syntaxRuleMacro, name string) bool {
	_, ok := macro.literals[name]
	return ok
}

func hasPatternVar(rule *syntaxRule, name string) bool {
	_, ok := rule.patternVars[name]
	return ok
}

func isEllipsisExpr(node expr) bool {
	sym, ok := node.(*symbolExpr)
	return ok && sym.name == "..."
}

func (it *interpreter) expandTemplate(node expr, macro *syntaxRuleMacro, rule *syntaxRule, match *syntaxMatch, locals map[string]string, repeatIndex *int) (expr, error) {
	switch current := node.(type) {
	case *intExpr, *boolExpr, *stringExpr, *charExpr:
		return cloneExpr(current), nil
	case *symbolExpr:
		return it.expandTemplateSymbol(current, macro, rule, match, locals, repeatIndex)
	case *listExpr:
		return it.expandTemplateList(current, macro, rule, match, locals, repeatIndex)
	default:
		return nil, newEvalError(ErrSyntax, "invalid syntax template", node.pos())
	}
}

func (it *interpreter) expandTemplateSymbol(sym *symbolExpr, macro *syntaxRuleMacro, rule *syntaxRule, match *syntaxMatch, locals map[string]string, repeatIndex *int) (expr, error) {
	if hasPatternVar(rule, sym.name) {
		if repeatIndex != nil {
			if repeated, ok := match.repeated[sym.name]; ok {
				if *repeatIndex >= len(repeated) {
					return nil, newEvalError(ErrSyntax, fmt.Sprintf("template variable %s out of range", sym.name), sym.at)
				}
				return cloneExpr(repeated[*repeatIndex]), nil
			}
		}

		if single, ok := match.single[sym.name]; ok {
			return cloneExpr(single), nil
		}

		if _, ok := match.repeated[sym.name]; ok {
			return nil, newEvalError(ErrSyntax, fmt.Sprintf("template variable %s requires ellipsis", sym.name), sym.at)
		}
	}

	expanded := &symbolExpr{name: sym.name, at: sym.at}
	if locals != nil {
		if key, ok := locals[sym.name]; ok {
			expanded.key = key
			return expanded, nil
		}
	}

	if binding, ok := macro.defEnv.lookupBinding(sym.name); ok {
		expanded.key = binding.key
	}
	return expanded, nil
}

func (it *interpreter) expandTemplateList(list *listExpr, macro *syntaxRuleMacro, rule *syntaxRule, match *syntaxMatch, locals map[string]string, repeatIndex *int) (expr, error) {
	if len(list.elements) == 0 {
		return &listExpr{at: list.at}, nil
	}

	if head, ok := list.elements[0].(*symbolExpr); ok && !hasPatternVar(rule, head.name) {
		switch head.name {
		case "let":
			return it.expandTemplateLet(list, macro, rule, match, locals, repeatIndex)
		case "lambda":
			return it.expandTemplateLambda(list, macro, rule, match, locals, repeatIndex)
		}
	}

	return it.expandTemplateListGeneric(list, macro, rule, match, locals, repeatIndex)
}

func (it *interpreter) expandTemplateListGeneric(list *listExpr, macro *syntaxRuleMacro, rule *syntaxRule, match *syntaxMatch, locals map[string]string, repeatIndex *int) (expr, error) {
	elements := make([]expr, 0, len(list.elements))
	for i := 0; i < len(list.elements); i++ {
		if i+1 < len(list.elements) && isEllipsisExpr(list.elements[i+1]) {
			count, hasCount, err := repeatedTemplateCount(list.elements[i], rule, match)
			if err != nil {
				return nil, err
			}
			if hasCount {
				for repetition := 0; repetition < count; repetition++ {
					index := repetition
					expanded, err := it.expandTemplate(list.elements[i], macro, rule, match, locals, &index)
					if err != nil {
						return nil, err
					}
					elements = append(elements, expanded)
				}
			}
			i++
			continue
		}

		expanded, err := it.expandTemplate(list.elements[i], macro, rule, match, locals, repeatIndex)
		if err != nil {
			return nil, err
		}
		elements = append(elements, expanded)
	}

	return &listExpr{elements: elements, at: list.at}, nil
}

func (it *interpreter) expandTemplateLet(list *listExpr, macro *syntaxRuleMacro, rule *syntaxRule, match *syntaxMatch, locals map[string]string, repeatIndex *int) (expr, error) {
	if len(list.elements) < 3 {
		return it.expandTemplateListGeneric(list, macro, rule, match, locals, repeatIndex)
	}

	bodyLocals := copyLocalBindings(locals)
	elements := []expr{&symbolExpr{name: "let", at: list.elements[0].pos()}}

	bindingIndex := 1
	if _, ok := list.elements[1].(*listExpr); !ok {
		name, err := it.expandTemplateBindingIdentifier(list.elements[1], macro, rule, match, locals, repeatIndex)
		if err != nil {
			return nil, err
		}
		elements = append(elements, name)
		if name.key != "" {
			bodyLocals[name.name] = name.key
		}
		bindingIndex = 2
	}

	if bindingIndex >= len(list.elements) {
		return it.expandTemplateListGeneric(list, macro, rule, match, locals, repeatIndex)
	}

	bindingList, ok := list.elements[bindingIndex].(*listExpr)
	if !ok {
		return it.expandTemplateListGeneric(list, macro, rule, match, locals, repeatIndex)
	}

	expandedBindings := make([]expr, 0, len(bindingList.elements))
	for _, bindingExpr := range bindingList.elements {
		binding, ok := bindingExpr.(*listExpr)
		if !ok || len(binding.elements) != 2 {
			return nil, newEvalError(ErrSyntax, "syntax template: let binding must be a pair", bindingExpr.pos())
		}

		name, err := it.expandTemplateBindingIdentifier(binding.elements[0], macro, rule, match, locals, repeatIndex)
		if err != nil {
			return nil, err
		}
		initExpr, err := it.expandTemplate(binding.elements[1], macro, rule, match, locals, repeatIndex)
		if err != nil {
			return nil, err
		}

		if name.key != "" {
			bodyLocals[name.name] = name.key
		}
		expandedBindings = append(expandedBindings, &listExpr{
			elements: []expr{name, initExpr},
			at:       binding.at,
		})
	}

	elements = append(elements, &listExpr{elements: expandedBindings, at: bindingList.at})
	for _, bodyExpr := range list.elements[bindingIndex+1:] {
		expandedBody, err := it.expandTemplate(bodyExpr, macro, rule, match, bodyLocals, repeatIndex)
		if err != nil {
			return nil, err
		}
		elements = append(elements, expandedBody)
	}

	return &listExpr{elements: elements, at: list.at}, nil
}

func (it *interpreter) expandTemplateLambda(list *listExpr, macro *syntaxRuleMacro, rule *syntaxRule, match *syntaxMatch, locals map[string]string, repeatIndex *int) (expr, error) {
	if len(list.elements) < 3 {
		return it.expandTemplateListGeneric(list, macro, rule, match, locals, repeatIndex)
	}

	bodyLocals := copyLocalBindings(locals)
	formals, err := it.expandTemplateFormals(list.elements[1], macro, rule, match, locals, &bodyLocals, repeatIndex)
	if err != nil {
		return nil, err
	}

	elements := []expr{
		&symbolExpr{name: "lambda", at: list.elements[0].pos()},
		formals,
	}

	for _, bodyExpr := range list.elements[2:] {
		expandedBody, err := it.expandTemplate(bodyExpr, macro, rule, match, bodyLocals, repeatIndex)
		if err != nil {
			return nil, err
		}
		elements = append(elements, expandedBody)
	}

	return &listExpr{elements: elements, at: list.at}, nil
}

func (it *interpreter) expandTemplateFormals(node expr, macro *syntaxRuleMacro, rule *syntaxRule, match *syntaxMatch, locals map[string]string, bodyLocals *map[string]string, repeatIndex *int) (expr, error) {
	switch current := node.(type) {
	case *symbolExpr:
		if current.name == "." {
			return nil, newEvalError(ErrSyntax, "syntax template: invalid lambda formals", current.at)
		}
		name, err := it.expandTemplateBindingIdentifier(current, macro, rule, match, locals, repeatIndex)
		if err != nil {
			return nil, err
		}
		if name.key != "" {
			(*bodyLocals)[name.name] = name.key
		}
		return name, nil
	case *listExpr:
		elements := make([]expr, 0, len(current.elements))
		for index, element := range current.elements {
			sym, isSymbol := element.(*symbolExpr)
			if isSymbol && sym.name == "." {
				elements = append(elements, &symbolExpr{name: ".", at: sym.at})
				continue
			}

			name, err := it.expandTemplateBindingIdentifier(element, macro, rule, match, locals, repeatIndex)
			if err != nil {
				return nil, err
			}
			if name.key != "" {
				(*bodyLocals)[name.name] = name.key
			}
			elements = append(elements, name)

			if isSymbol && sym.name == "." && index == len(current.elements)-1 {
				return nil, newEvalError(ErrSyntax, "syntax template: invalid lambda formals", sym.at)
			}
		}
		return &listExpr{elements: elements, at: current.at}, nil
	default:
		return nil, newEvalError(ErrSyntax, "syntax template: expected lambda formals", node.pos())
	}
}

func (it *interpreter) expandTemplateBindingIdentifier(node expr, macro *syntaxRuleMacro, rule *syntaxRule, match *syntaxMatch, locals map[string]string, repeatIndex *int) (*symbolExpr, error) {
	sym, ok := node.(*symbolExpr)
	if !ok {
		expanded, err := it.expandTemplate(node, macro, rule, match, locals, repeatIndex)
		if err != nil {
			return nil, err
		}
		resolved, ok := expanded.(*symbolExpr)
		if !ok {
			return nil, newEvalError(ErrSyntax, "syntax template: expected identifier", node.pos())
		}
		return resolved, nil
	}

	if hasPatternVar(rule, sym.name) {
		expanded, err := it.expandTemplateSymbol(sym, macro, rule, match, locals, repeatIndex)
		if err != nil {
			return nil, err
		}
		resolved, ok := expanded.(*symbolExpr)
		if !ok {
			return nil, newEvalError(ErrSyntax, "syntax template: expected identifier", sym.at)
		}
		return resolved, nil
	}

	return &symbolExpr{
		name: sym.name,
		key:  it.freshBindingKey(sym.name),
		at:   sym.at,
	}, nil
}

func repeatedTemplateCount(node expr, rule *syntaxRule, match *syntaxMatch) (int, bool, error) {
	var count int
	var found bool

	var walk func(expr) error
	walk = func(current expr) error {
		switch expr := current.(type) {
		case *symbolExpr:
			if !hasPatternVar(rule, expr.name) {
				return nil
			}

			repeated, ok := match.repeated[expr.name]
			if !ok {
				return nil
			}
			if !found {
				count = len(repeated)
				found = true
				return nil
			}
			if count != len(repeated) {
				return newEvalError(ErrSyntax, "syntax template: mismatched ellipsis counts", expr.at)
			}
		case *listExpr:
			for _, element := range expr.elements {
				if err := walk(element); err != nil {
					return err
				}
			}
		}
		return nil
	}

	if err := walk(node); err != nil {
		return 0, false, err
	}
	return count, found, nil
}

func (it *interpreter) resolveSyntax(node expr, scope *env) (expr, error) {
	switch current := node.(type) {
	case *intExpr, *boolExpr, *stringExpr, *charExpr:
		return cloneExpr(current), nil
	case *symbolExpr:
		resolved := &symbolExpr{name: current.name, key: current.key, at: current.at}
		if resolved.key == "" {
			if binding, ok := scope.lookupBinding(current.name); ok {
				resolved.key = binding.key
			}
		}
		return resolved, nil
	case *listExpr:
		return it.resolveSyntaxList(current, scope)
	default:
		return nil, newEvalError(ErrSyntax, "invalid syntax object", node.pos())
	}
}

func (it *interpreter) resolveSyntaxList(list *listExpr, scope *env) (expr, error) {
	if len(list.elements) == 0 {
		return &listExpr{at: list.at}, nil
	}

	head, ok := list.elements[0].(*symbolExpr)
	if !ok {
		return it.resolveSyntaxListGeneric(list, scope)
	}

	switch head.name {
	case "quote":
		return cloneExpr(list), nil
	case "lambda":
		return it.resolveSyntaxLambda(list, scope)
	case "let":
		return it.resolveSyntaxLet(list, scope)
	case "and", "or", "define", "define-syntax", "if", "set!", "begin", "cond":
		elements := []expr{&symbolExpr{name: head.name, at: head.at}}
		for _, element := range list.elements[1:] {
			resolved, err := it.resolveSyntax(element, scope)
			if err != nil {
				return nil, err
			}
			elements = append(elements, resolved)
		}
		return &listExpr{elements: elements, at: list.at}, nil
	default:
		return it.resolveSyntaxListGeneric(list, scope)
	}
}

func (it *interpreter) resolveSyntaxListGeneric(list *listExpr, scope *env) (expr, error) {
	elements := make([]expr, 0, len(list.elements))
	for _, element := range list.elements {
		resolved, err := it.resolveSyntax(element, scope)
		if err != nil {
			return nil, err
		}
		elements = append(elements, resolved)
	}
	return &listExpr{elements: elements, at: list.at}, nil
}

func (it *interpreter) resolveSyntaxLambda(list *listExpr, scope *env) (expr, error) {
	if len(list.elements) < 3 {
		return it.resolveSyntaxListGeneric(list, scope)
	}

	bodyScope := newEnv(scope)
	formals, err := it.resolveLambdaFormals(list.elements[1], bodyScope)
	if err != nil {
		return nil, err
	}

	elements := []expr{
		&symbolExpr{name: "lambda", at: list.elements[0].pos()},
		formals,
	}

	for _, bodyExpr := range list.elements[2:] {
		resolved, err := it.resolveSyntax(bodyExpr, bodyScope)
		if err != nil {
			return nil, err
		}
		elements = append(elements, resolved)
	}

	return &listExpr{elements: elements, at: list.at}, nil
}

func (it *interpreter) resolveLambdaFormals(node expr, scope *env) (expr, error) {
	switch current := node.(type) {
	case *symbolExpr:
		if current.name == "." {
			return nil, newEvalError(ErrSyntax, "lambda: expected parameter name", current.at)
		}
		return it.resolveBindingIdentifier(current, scope)
	case *listExpr:
		elements := make([]expr, 0, len(current.elements))
		for i, element := range current.elements {
			sym, ok := element.(*symbolExpr)
			if ok && sym.name == "." {
				if i == len(current.elements)-1 {
					return nil, newEvalError(ErrSyntax, "expected rest parameter name", sym.at)
				}
				elements = append(elements, &symbolExpr{name: ".", at: sym.at})
				continue
			}

			binding, err := it.resolveBindingIdentifier(element, scope)
			if err != nil {
				return nil, err
			}
			elements = append(elements, binding)
		}
		return &listExpr{elements: elements, at: current.at}, nil
	default:
		return nil, newEvalError(ErrSyntax, "lambda: expected parameter list", node.pos())
	}
}

func (it *interpreter) resolveSyntaxLet(list *listExpr, scope *env) (expr, error) {
	if len(list.elements) < 3 {
		return it.resolveSyntaxListGeneric(list, scope)
	}

	elements := []expr{&symbolExpr{name: "let", at: list.elements[0].pos()}}
	bodyScope := newEnv(scope)
	bindingIndex := 1

	if _, ok := list.elements[1].(*listExpr); !ok {
		name, err := it.resolveBindingIdentifier(list.elements[1], bodyScope)
		if err != nil {
			return nil, err
		}
		elements = append(elements, name)
		bindingIndex = 2
	}

	if bindingIndex >= len(list.elements) {
		return it.resolveSyntaxListGeneric(list, scope)
	}

	bindingList, ok := list.elements[bindingIndex].(*listExpr)
	if !ok {
		return nil, newEvalError(ErrSyntax, "let: expected binding list", list.elements[bindingIndex].pos())
	}

	expandedBindings := make([]expr, 0, len(bindingList.elements))
	for _, bindingExpr := range bindingList.elements {
		binding, ok := bindingExpr.(*listExpr)
		if !ok || len(binding.elements) != 2 {
			return nil, newEvalError(ErrSyntax, "let: expected binding pair", bindingExpr.pos())
		}

		name, err := it.resolveBindingIdentifier(binding.elements[0], bodyScope)
		if err != nil {
			return nil, err
		}
		initExpr, err := it.resolveSyntax(binding.elements[1], scope)
		if err != nil {
			return nil, err
		}

		expandedBindings = append(expandedBindings, &listExpr{
			elements: []expr{name, initExpr},
			at:       binding.at,
		})
	}

	elements = append(elements, &listExpr{elements: expandedBindings, at: bindingList.at})
	for _, bodyExpr := range list.elements[bindingIndex+1:] {
		resolved, err := it.resolveSyntax(bodyExpr, bodyScope)
		if err != nil {
			return nil, err
		}
		elements = append(elements, resolved)
	}

	return &listExpr{elements: elements, at: list.at}, nil
}

func (it *interpreter) resolveBindingIdentifier(node expr, scope *env) (*symbolExpr, error) {
	sym, ok := node.(*symbolExpr)
	if !ok || sym.name == "." {
		return nil, newEvalError(ErrSyntax, "expected identifier", node.pos())
	}

	resolved := &symbolExpr{
		name: sym.name,
		key:  sym.key,
		at:   sym.at,
	}
	if resolved.key == "" {
		resolved.key = it.freshBindingKey(sym.name)
	}
	scope.bind(&binding{name: resolved.name, key: resolved.key})
	return resolved, nil
}

func cloneExpr(node expr) expr {
	switch current := node.(type) {
	case *intExpr:
		return &intExpr{value: current.value, at: current.at}
	case *boolExpr:
		return &boolExpr{value: current.value, at: current.at}
	case *stringExpr:
		return &stringExpr{value: current.value, at: current.at}
	case *charExpr:
		return &charExpr{value: current.value, at: current.at}
	case *symbolExpr:
		return &symbolExpr{name: current.name, key: current.key, at: current.at}
	case *listExpr:
		elements := make([]expr, 0, len(current.elements))
		for _, element := range current.elements {
			elements = append(elements, cloneExpr(element))
		}
		return &listExpr{elements: elements, at: current.at}
	default:
		return nil
	}
}

func syntaxExprEqual(left expr, right expr) bool {
	switch leftExpr := left.(type) {
	case *intExpr:
		rightExpr, ok := right.(*intExpr)
		return ok && leftExpr.value == rightExpr.value
	case *boolExpr:
		rightExpr, ok := right.(*boolExpr)
		return ok && leftExpr.value == rightExpr.value
	case *stringExpr:
		rightExpr, ok := right.(*stringExpr)
		return ok && leftExpr.value == rightExpr.value
	case *charExpr:
		rightExpr, ok := right.(*charExpr)
		return ok && leftExpr.value == rightExpr.value
	case *symbolExpr:
		rightExpr, ok := right.(*symbolExpr)
		return ok && leftExpr.name == rightExpr.name && leftExpr.key == rightExpr.key
	case *listExpr:
		rightExpr, ok := right.(*listExpr)
		if !ok || len(leftExpr.elements) != len(rightExpr.elements) {
			return false
		}
		for i := range leftExpr.elements {
			if !syntaxExprEqual(leftExpr.elements[i], rightExpr.elements[i]) {
				return false
			}
		}
		return true
	default:
		return false
	}
}

func copyLocalBindings(locals map[string]string) map[string]string {
	if len(locals) == 0 {
		return map[string]string{}
	}

	copied := make(map[string]string, len(locals))
	for name, key := range locals {
		copied[name] = key
	}
	return copied
}
