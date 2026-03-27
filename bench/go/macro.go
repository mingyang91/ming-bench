package ming

import (
	"fmt"
	"strings"
)

type macroImplementation interface {
	expand(*interpreter, *listExpr) (expr, error)
}

type macroExpander struct {
	impl macroImplementation
}

func (m *macroExpander) expand(i *interpreter, invocation *listExpr) (expr, error) {
	return m.impl.expand(i, invocation)
}

type syntaxRuleMacro struct {
	name     string
	literals map[string]struct{}
	rules    []syntaxRule
	defEnv   *environment
}

type syntaxRule struct {
	pattern     expr
	template    expr
	patternVars map[string]struct{}
}

type syntaxCapture struct {
	scalar      expr
	repeated    []syntaxCapture
	isRepeated bool
}

func parseSyntaxRuleMacro(name string, transformer expr, env *environment) (*syntaxRuleMacro, error) {
	form, ok := transformer.(*listExpr)
	if !ok || len(form.elements) < 3 {
		return nil, newEvalError(transformer.exprPos(), "define-syntax requires a syntax-rules transformer")
	}

	keyword, ok := form.elements[0].(*symbolExpr)
	if !ok || keyword.value != "syntax-rules" {
		return nil, newEvalError(form.elements[0].exprPos(), "define-syntax requires a syntax-rules transformer")
	}

	literalList, ok := form.elements[1].(*listExpr)
	if !ok {
		return nil, newEvalError(form.elements[1].exprPos(), "syntax-rules literals must be a list")
	}

	literals := map[string]struct{}{}
	for _, literalExpr := range literalList.elements {
		literal, ok := literalExpr.(*symbolExpr)
		if !ok {
			return nil, newEvalError(literalExpr.exprPos(), "syntax-rules literals must be symbols")
		}
		literals[literal.value] = struct{}{}
	}
	literals[name] = struct{}{}

	rules := make([]syntaxRule, 0, len(form.elements)-2)
	for _, ruleExpr := range form.elements[2:] {
		ruleList, ok := ruleExpr.(*listExpr)
		if !ok || len(ruleList.elements) != 2 {
			return nil, newEvalError(ruleExpr.exprPos(), "syntax-rules clauses must contain a pattern and template")
		}

		patternList, ok := ruleList.elements[0].(*listExpr)
		if !ok || len(patternList.elements) == 0 {
			return nil, newEvalError(ruleList.elements[0].exprPos(), "syntax-rules patterns must be non-empty lists")
		}

		head, ok := patternList.elements[0].(*symbolExpr)
		if !ok || (head.value != name && head.value != "_") {
			return nil, newEvalError(patternList.elements[0].exprPos(), "syntax-rules pattern must start with the macro name or _")
		}

		patternVars := map[string]struct{}{}
		collectPatternVars(ruleList.elements[0], literals, patternVars)

		rules = append(rules, syntaxRule{
			pattern:     ruleList.elements[0],
			template:    ruleList.elements[1],
			patternVars: patternVars,
		})
	}

	return &syntaxRuleMacro{
		name:     name,
		literals: literals,
		rules:    rules,
		defEnv:   env,
	}, nil
}

func (m *syntaxRuleMacro) expand(i *interpreter, invocation *listExpr) (expr, error) {
	for _, rule := range m.rules {
		captures, ok := matchSyntaxPattern(rule.pattern, invocation, m.literals)
		if !ok {
			continue
		}

		state := templateState{
			intp:           i,
			defEnv:         m.defEnv,
			patternVars:    rule.patternVars,
			captures:       captures,
			freeIntroduced: map[string]string{},
		}
		return state.expand(rule.template, nil, nil)
	}

	return nil, newEvalError(invocation.pos, "no matching syntax-rules clause for %s", m.name)
}

func collectPatternVars(pattern expr, literals map[string]struct{}, vars map[string]struct{}) {
	switch p := pattern.(type) {
	case *symbolExpr:
		if p.value == "..." || p.value == "_" {
			return
		}
		if _, ok := literals[p.value]; ok {
			return
		}
		vars[p.value] = struct{}{}
	case *listExpr:
		for _, element := range p.elements {
			collectPatternVars(element, literals, vars)
		}
		if p.tail != nil {
			collectPatternVars(p.tail, literals, vars)
		}
	}
}

func matchSyntaxPattern(pattern expr, input expr, literals map[string]struct{}) (map[string]syntaxCapture, bool) {
	switch p := pattern.(type) {
	case *symbolExpr:
		if p.value == "..." {
			return nil, false
		}
		if p.value == "_" {
			return map[string]syntaxCapture{}, true
		}
		if _, ok := literals[p.value]; ok {
			symbol, ok := input.(*symbolExpr)
			return map[string]syntaxCapture{}, ok && symbol.value == p.value
		}
		return map[string]syntaxCapture{
			p.value: syntaxCapture{scalar: input},
		}, true

	case *integerExpr:
		other, ok := input.(*integerExpr)
		return map[string]syntaxCapture{}, ok && p.value == other.value
	case *rationalExpr:
		other, ok := input.(*rationalExpr)
		return map[string]syntaxCapture{}, ok && p.value == other.value
	case *inexactExpr:
		other, ok := input.(*inexactExpr)
		return map[string]syntaxCapture{}, ok && p.value == other.value
	case *booleanExpr:
		other, ok := input.(*booleanExpr)
		return map[string]syntaxCapture{}, ok && p.value == other.value
	case *stringExpr:
		other, ok := input.(*stringExpr)
		return map[string]syntaxCapture{}, ok && p.value == other.value
	case *charExpr:
		other, ok := input.(*charExpr)
		return map[string]syntaxCapture{}, ok && p.value == other.value
	case *listExpr:
		other, ok := input.(*listExpr)
		if !ok {
			return nil, false
		}
		return matchSyntaxList(p, other, literals)
	default:
		return nil, false
	}
}

func matchSyntaxList(pattern *listExpr, input *listExpr, literals map[string]struct{}) (map[string]syntaxCapture, bool) {
	return matchSyntaxListParts(pattern.elements, pattern.tail, input.elements, input.tail, input.pos, literals)
}

func matchSyntaxListParts(patterns []expr, patternTail expr, inputs []expr, inputTail expr, inputPos position, literals map[string]struct{}) (map[string]syntaxCapture, bool) {
	if len(patterns) == 0 {
		if patternTail != nil {
			return matchSyntaxPattern(patternTail, syntaxListRemainderExpr(inputs, inputTail, inputPos), literals)
		}
		if len(inputs) == 0 && inputTail == nil {
			return map[string]syntaxCapture{}, true
		}
		return nil, false
	}

	if len(patterns) >= 2 && isEllipsisExpr(patterns[1]) {
		repeatedPattern := patterns[0]
		repeatedVars := map[string]struct{}{}
		collectPatternVars(repeatedPattern, literals, repeatedVars)

		for count := 0; count <= len(inputs); count++ {
			iterationCaptures := make([]map[string]syntaxCapture, 0, count)
			ok := true
			for index := 0; index < count; index++ {
				captures, matched := matchSyntaxPattern(repeatedPattern, inputs[index], literals)
				if !matched {
					ok = false
					break
				}
				iterationCaptures = append(iterationCaptures, captures)
			}
			if !ok {
				continue
			}

			restCaptures, matched := matchSyntaxListParts(patterns[2:], patternTail, inputs[count:], inputTail, inputPos, literals)
			if !matched {
				continue
			}

			repeatedCaptures, ok := combineRepeatedCaptures(iterationCaptures, repeatedVars)
			if !ok {
				continue
			}

			merged, ok := mergeCaptureMaps(repeatedCaptures, restCaptures)
			if ok {
				return merged, true
			}
		}
		return nil, false
	}

	if len(inputs) == 0 {
		return nil, false
	}

	firstCaptures, matched := matchSyntaxPattern(patterns[0], inputs[0], literals)
	if !matched {
		return nil, false
	}

	restCaptures, matched := matchSyntaxListParts(patterns[1:], patternTail, inputs[1:], inputTail, inputPos, literals)
	if !matched {
		return nil, false
	}

	return mergeCaptureMaps(firstCaptures, restCaptures)
}

func syntaxListRemainderExpr(inputs []expr, inputTail expr, inputPos position) expr {
	if len(inputs) == 0 {
		if inputTail != nil {
			return inputTail
		}
		return &listExpr{pos: inputPos}
	}
	return &listExpr{
		elements: append([]expr(nil), inputs...),
		tail:     inputTail,
		pos:      inputPos,
	}
}

func combineRepeatedCaptures(iterationCaptures []map[string]syntaxCapture, repeatedVars map[string]struct{}) (map[string]syntaxCapture, bool) {
	result := map[string]syntaxCapture{}
	for name := range repeatedVars {
		repeated := make([]syntaxCapture, 0, len(iterationCaptures))
		for _, captures := range iterationCaptures {
			capture, ok := captures[name]
			if !ok {
				return nil, false
			}
			repeated = append(repeated, capture)
		}
		result[name] = syntaxCapture{repeated: repeated, isRepeated: true}
	}
	return result, true
}

func mergeCaptureMaps(left map[string]syntaxCapture, right map[string]syntaxCapture) (map[string]syntaxCapture, bool) {
	merged := map[string]syntaxCapture{}
	for name, capture := range left {
		merged[name] = capture
	}
	for name, capture := range right {
		if existing, ok := merged[name]; ok {
			if !captureEqual(existing, capture) {
				return nil, false
			}
			continue
		}
		merged[name] = capture
	}
	return merged, true
}

func captureEqual(left syntaxCapture, right syntaxCapture) bool {
	if left.isRepeated != right.isRepeated {
		return false
	}
	if len(left.repeated) != len(right.repeated) {
		return false
	}
	if left.isRepeated {
		for index := range left.repeated {
			if !captureEqual(left.repeated[index], right.repeated[index]) {
				return false
			}
		}
		return true
	}
	return exprEqual(left.scalar, right.scalar)
}

func exprEqual(left expr, right expr) bool {
	switch l := left.(type) {
	case *integerExpr:
		r, ok := right.(*integerExpr)
		return ok && l.value == r.value
	case *rationalExpr:
		r, ok := right.(*rationalExpr)
		return ok && l.value == r.value
	case *inexactExpr:
		r, ok := right.(*inexactExpr)
		return ok && l.value == r.value
	case *booleanExpr:
		r, ok := right.(*booleanExpr)
		return ok && l.value == r.value
	case *stringExpr:
		r, ok := right.(*stringExpr)
		return ok && l.value == r.value
	case *charExpr:
		r, ok := right.(*charExpr)
		return ok && l.value == r.value
	case *symbolExpr:
		r, ok := right.(*symbolExpr)
		return ok && l.value == r.value && l.binding == r.binding && l.macro == r.macro
	case *listExpr:
		r, ok := right.(*listExpr)
		if !ok || len(l.elements) != len(r.elements) {
			return false
		}
		for index := range l.elements {
			if !exprEqual(l.elements[index], r.elements[index]) {
				return false
			}
		}
		return exprEqual(l.tail, r.tail)
	default:
		return left == nil && right == nil
	}
}

type templateState struct {
	intp           *interpreter
	defEnv         *environment
	patternVars    map[string]struct{}
	captures       map[string]syntaxCapture
	freeIntroduced map[string]string
}

func (s *templateState) expand(template expr, path []int, renames map[string]string) (expr, error) {
	switch t := template.(type) {
	case *integerExpr:
		return &integerExpr{value: t.value, pos: t.pos}, nil
	case *rationalExpr:
		return &rationalExpr{value: t.value, pos: t.pos}, nil
	case *inexactExpr:
		return &inexactExpr{value: t.value, pos: t.pos}, nil
	case *booleanExpr:
		return &booleanExpr{value: t.value, pos: t.pos}, nil
	case *stringExpr:
		return &stringExpr{value: t.value, pos: t.pos}, nil
	case *charExpr:
		return &charExpr{value: t.value, pos: t.pos}, nil
	case *symbolExpr:
		return s.expandSymbol(t, path, renames)
	case *listExpr:
		return s.expandList(t, path, renames)
	default:
		return nil, newEvalError(template.exprPos(), "unsupported syntax template")
	}
}

func (s *templateState) expandSymbol(symbol *symbolExpr, path []int, renames map[string]string) (expr, error) {
	if _, ok := s.patternVars[symbol.value]; ok {
		capture, ok := s.captures[symbol.value]
		if !ok {
			return nil, newEvalError(symbol.pos, "missing syntax capture for %s", symbol.value)
		}
		return captureExprAtPath(capture, path, symbol.pos)
	}

	if renamed, ok := renames[symbol.value]; ok {
		return &symbolExpr{value: renamed, pos: symbol.pos}, nil
	}

	if isCoreSyntaxKeyword(symbol.value) {
		return &symbolExpr{value: symbol.value, pos: symbol.pos}, nil
	}

	var binding *binding
	if resolved, ok := s.defEnv.lookupBinding(symbol.value); ok {
		binding = resolved
	}

	var macro *macroExpander
	if resolved, ok := s.defEnv.lookupMacro(symbol.value); ok {
		macro = resolved
	}

	if binding != nil || macro != nil {
		return &symbolExpr{
			value:   symbol.value,
			pos:     symbol.pos,
			binding: binding,
			macro:   macro,
		}, nil
	}

	introduced, ok := s.freeIntroduced[symbol.value]
	if !ok {
		introduced = s.intp.gensym(symbol.value)
		s.freeIntroduced[symbol.value] = introduced
	}
	return &symbolExpr{value: introduced, pos: symbol.pos}, nil
}

func (s *templateState) expandList(list *listExpr, path []int, renames map[string]string) (expr, error) {
	if len(list.elements) > 0 {
		if head, ok := list.elements[0].(*symbolExpr); ok {
			if _, isPatternVar := s.patternVars[head.value]; !isPatternVar {
				switch head.value {
				case "quote":
					return cloneExpr(list), nil
				case "let":
					return s.expandLet(list, path, renames)
				case "lambda":
					return s.expandLambda(list, path, renames)
				}
			}
		}
	}

	elements := make([]expr, 0, len(list.elements))
	for index := 0; index < len(list.elements); index++ {
		element := list.elements[index]
		if index+1 < len(list.elements) && isEllipsisExpr(list.elements[index+1]) {
			count, ok, err := s.repeatCount(element, path)
			if err != nil {
				return nil, err
			}
			if !ok {
				return nil, newEvalError(element.exprPos(), "template ellipsis requires a repeated pattern variable")
			}
			for repetition := 0; repetition < count; repetition++ {
				expanded, err := s.expand(element, appendPath(path, repetition), renames)
				if err != nil {
					return nil, err
				}
				elements = append(elements, expanded)
			}
			index++
			continue
		}

		expanded, err := s.expand(element, path, renames)
		if err != nil {
			return nil, err
		}
		elements = append(elements, expanded)
	}

	var tail expr
	if list.tail != nil {
		expandedTail, err := s.expand(list.tail, path, renames)
		if err != nil {
			return nil, err
		}
		tail = expandedTail
	}

	return &listExpr{elements: elements, tail: tail, pos: list.pos}, nil
}

func (s *templateState) expandLet(list *listExpr, path []int, renames map[string]string) (expr, error) {
	if len(list.elements) < 3 {
		return s.expandPlainList(list, path, renames)
	}

	head := &symbolExpr{value: "let", pos: list.elements[0].exprPos()}

	if nameExpr, ok := list.elements[1].(*symbolExpr); ok {
		if _, isPatternVar := s.patternVars[nameExpr.value]; !isPatternVar {
			return s.expandNamedLet(head, list, path, renames)
		}
	}

	bindingsExpr, ok := list.elements[1].(*listExpr)
	if !ok {
		return s.expandPlainList(list, path, renames)
	}

	bodyRenames := copyRenameMap(renames)
	expandedBindings := make([]expr, 0, len(bindingsExpr.elements))
	for _, bindingExpr := range bindingsExpr.elements {
		bindingList, ok := bindingExpr.(*listExpr)
		if !ok || len(bindingList.elements) != 2 {
			return nil, newEvalError(bindingExpr.exprPos(), "let template bindings must contain a name and value")
		}

		name, err := s.expandBinder(bindingList.elements[0], path, bodyRenames)
		if err != nil {
			return nil, err
		}
		value, err := s.expand(bindingList.elements[1], path, renames)
		if err != nil {
			return nil, err
		}
		expandedBindings = append(expandedBindings, &listExpr{
			elements: []expr{name, value},
			pos:      bindingList.pos,
		})
	}

	elements := []expr{head, &listExpr{elements: expandedBindings, pos: bindingsExpr.pos}}
	for _, bodyExpr := range list.elements[2:] {
		expanded, err := s.expand(bodyExpr, path, bodyRenames)
		if err != nil {
			return nil, err
		}
		elements = append(elements, expanded)
	}
	return &listExpr{elements: elements, pos: list.pos}, nil
}

func (s *templateState) expandNamedLet(head *symbolExpr, list *listExpr, path []int, renames map[string]string) (expr, error) {
	if len(list.elements) < 4 {
		return s.expandPlainList(list, path, renames)
	}

	bindingsExpr, ok := list.elements[2].(*listExpr)
	if !ok {
		return s.expandPlainList(list, path, renames)
	}

	bodyRenames := copyRenameMap(renames)
	name, err := s.expandBinder(list.elements[1], path, bodyRenames)
	if err != nil {
		return nil, err
	}

	expandedBindings := make([]expr, 0, len(bindingsExpr.elements))
	for _, bindingExpr := range bindingsExpr.elements {
		bindingList, ok := bindingExpr.(*listExpr)
		if !ok || len(bindingList.elements) != 2 {
			return nil, newEvalError(bindingExpr.exprPos(), "let template bindings must contain a name and value")
		}

		bindingName, err := s.expandBinder(bindingList.elements[0], path, bodyRenames)
		if err != nil {
			return nil, err
		}
		value, err := s.expand(bindingList.elements[1], path, renames)
		if err != nil {
			return nil, err
		}
		expandedBindings = append(expandedBindings, &listExpr{
			elements: []expr{bindingName, value},
			pos:      bindingList.pos,
		})
	}

	elements := []expr{head, name, &listExpr{elements: expandedBindings, pos: bindingsExpr.pos}}
	for _, bodyExpr := range list.elements[3:] {
		expanded, err := s.expand(bodyExpr, path, bodyRenames)
		if err != nil {
			return nil, err
		}
		elements = append(elements, expanded)
	}
	return &listExpr{elements: elements, pos: list.pos}, nil
}

func (s *templateState) expandLambda(list *listExpr, path []int, renames map[string]string) (expr, error) {
	if len(list.elements) < 3 {
		return s.expandPlainList(list, path, renames)
	}

	head := &symbolExpr{value: "lambda", pos: list.elements[0].exprPos()}
	bodyRenames := copyRenameMap(renames)

	params, err := s.expandLambdaParams(list.elements[1], path, bodyRenames)
	if err != nil {
		return nil, err
	}

	elements := []expr{head, params}
	for _, bodyExpr := range list.elements[2:] {
		expanded, err := s.expand(bodyExpr, path, bodyRenames)
		if err != nil {
			return nil, err
		}
		elements = append(elements, expanded)
	}
	return &listExpr{elements: elements, pos: list.pos}, nil
}

func (s *templateState) expandLambdaParams(params expr, path []int, renames map[string]string) (expr, error) {
	if symbol, ok := params.(*symbolExpr); ok {
		if symbol.value == "." {
			return &symbolExpr{value: ".", pos: symbol.pos}, nil
		}
		return s.expandBinder(symbol, path, renames)
	}

	list, ok := params.(*listExpr)
	if !ok {
		return nil, newEvalError(params.exprPos(), "lambda template parameters must be a list or symbol")
	}

	elements := make([]expr, 0, len(list.elements))
	for _, element := range list.elements {
		if symbol, ok := element.(*symbolExpr); ok && symbol.value == "." {
			elements = append(elements, &symbolExpr{value: ".", pos: symbol.pos})
			continue
		}
		binder, err := s.expandBinder(element, path, renames)
		if err != nil {
			return nil, err
		}
		elements = append(elements, binder)
	}

	var tail expr
	if list.tail != nil {
		binder, err := s.expandBinder(list.tail, path, renames)
		if err != nil {
			return nil, err
		}
		tail = binder
	}

	return &listExpr{elements: elements, tail: tail, pos: list.pos}, nil
}

func (s *templateState) expandBinder(binder expr, path []int, renames map[string]string) (expr, error) {
	symbol, ok := binder.(*symbolExpr)
	if !ok {
		expanded, err := s.expand(binder, path, renames)
		if err != nil {
			return nil, err
		}
		binderSymbol, ok := expanded.(*symbolExpr)
		if !ok {
			return nil, newEvalError(binder.exprPos(), "binding names must expand to symbols")
		}
		return binderSymbol, nil
	}

	if _, isPatternVar := s.patternVars[symbol.value]; isPatternVar {
		expanded, err := s.expand(symbol, path, renames)
		if err != nil {
			return nil, err
		}
		binderSymbol, ok := expanded.(*symbolExpr)
		if !ok {
			return nil, newEvalError(binder.exprPos(), "binding names must expand to symbols")
		}
		return binderSymbol, nil
	}

	fresh := s.intp.gensym(symbol.value)
	renames[symbol.value] = fresh
	return &symbolExpr{value: fresh, pos: symbol.pos}, nil
}

func (s *templateState) expandPlainList(list *listExpr, path []int, renames map[string]string) (expr, error) {
	elements := make([]expr, 0, len(list.elements))
	for _, element := range list.elements {
		expanded, err := s.expand(element, path, renames)
		if err != nil {
			return nil, err
		}
		elements = append(elements, expanded)
	}

	var tail expr
	if list.tail != nil {
		expandedTail, err := s.expand(list.tail, path, renames)
		if err != nil {
			return nil, err
		}
		tail = expandedTail
	}

	return &listExpr{elements: elements, tail: tail, pos: list.pos}, nil
}

func (s *templateState) repeatCount(template expr, path []int) (int, bool, error) {
	count := -1
	var err error

	walkTemplate(template, func(symbol *symbolExpr) bool {
		if _, ok := s.patternVars[symbol.value]; !ok {
			return true
		}
		capture, ok := s.captures[symbol.value]
		if !ok {
			err = newEvalError(symbol.pos, "missing syntax capture for %s", symbol.value)
			return false
		}
		repetitionCount, ok, countErr := captureRepeatCount(capture, path, symbol.pos)
		if countErr != nil {
			err = countErr
			return false
		}
		if !ok {
			return true
		}
		if count == -1 {
			count = repetitionCount
			return true
		}
		if count != repetitionCount {
			err = newEvalError(symbol.pos, "mismatched repetition counts in syntax template")
			return false
		}
		return true
	})

	if err != nil {
		return 0, false, err
	}
	if count == -1 {
		return 0, false, nil
	}
	return count, true, nil
}

func walkTemplate(template expr, visit func(*symbolExpr) bool) bool {
	switch t := template.(type) {
	case *symbolExpr:
		return visit(t)
	case *listExpr:
		for index := 0; index < len(t.elements); index++ {
			if index+1 < len(t.elements) && isEllipsisExpr(t.elements[index+1]) {
				if !walkTemplate(t.elements[index], visit) {
					return false
				}
				index++
				continue
			}
			if !walkTemplate(t.elements[index], visit) {
				return false
			}
		}
		if t.tail != nil && !walkTemplate(t.tail, visit) {
			return false
		}
	}
	return true
}

func captureExprAtPath(capture syntaxCapture, path []int, pos position) (expr, error) {
	if len(path) == 0 {
		if capture.isRepeated {
			return nil, newEvalError(pos, "missing ellipsis for repeated pattern variable")
		}
		return cloneExpr(capture.scalar), nil
	}
	if !capture.isRepeated {
		return nil, newEvalError(pos, "too many ellipses for pattern variable")
	}
	index := path[0]
	if index < 0 || index >= len(capture.repeated) {
		return nil, newEvalError(pos, "pattern repetition out of range")
	}
	return captureExprAtPath(capture.repeated[index], path[1:], pos)
}

func captureRepeatCount(capture syntaxCapture, path []int, pos position) (int, bool, error) {
	if len(path) == 0 {
		if !capture.isRepeated {
			return 0, false, nil
		}
		return len(capture.repeated), true, nil
	}
	if !capture.isRepeated {
		return 0, false, nil
	}
	index := path[0]
	if index < 0 || index >= len(capture.repeated) {
		return 0, false, newEvalError(pos, "pattern repetition out of range")
	}
	return captureRepeatCount(capture.repeated[index], path[1:], pos)
}

func cloneExpr(expression expr) expr {
	switch e := expression.(type) {
	case nil:
		return nil
	case *integerExpr:
		return &integerExpr{value: e.value, pos: e.pos}
	case *rationalExpr:
		return &rationalExpr{value: e.value, pos: e.pos}
	case *inexactExpr:
		return &inexactExpr{value: e.value, pos: e.pos}
	case *booleanExpr:
		return &booleanExpr{value: e.value, pos: e.pos}
	case *stringExpr:
		return &stringExpr{value: e.value, pos: e.pos}
	case *charExpr:
		return &charExpr{value: e.value, pos: e.pos}
	case *symbolExpr:
		return &symbolExpr{
			value:   e.value,
			pos:     e.pos,
			binding: e.binding,
			macro:   e.macro,
		}
	case *listExpr:
		elements := make([]expr, 0, len(e.elements))
		for _, element := range e.elements {
			elements = append(elements, cloneExpr(element))
		}
		return &listExpr{elements: elements, tail: cloneExpr(e.tail), pos: e.pos}
	default:
		return nil
	}
}

func appendPath(path []int, index int) []int {
	next := make([]int, len(path)+1)
	copy(next, path)
	next[len(path)] = index
	return next
}

func copyRenameMap(source map[string]string) map[string]string {
	if len(source) == 0 {
		return map[string]string{}
	}
	copyMap := make(map[string]string, len(source))
	for key, value := range source {
		copyMap[key] = value
	}
	return copyMap
}

func isEllipsisExpr(expression expr) bool {
	symbol, ok := expression.(*symbolExpr)
	return ok && symbol.value == "..."
}

func isCoreSyntaxKeyword(name string) bool {
	switch name {
	case "and", "or", "begin", "if", "cond", "guard", "case", "do",
		"define", "set!", "let", "let*", "letrec", "letrec*",
		"quote", "lambda", "case-lambda", "define-record-type",
		"define-syntax", "syntax-rules", "syntax", "syntax-case", "with-syntax",
		"else":
		return true
	default:
		return false
	}
}

func (i *interpreter) gensym(base string) string {
	i.gensymCounter++
	sanitized := sanitizeIdentifier(base)
	return fmt.Sprintf("#:%s:%d", sanitized, i.gensymCounter)
}

func sanitizeIdentifier(name string) string {
	var builder strings.Builder
	for _, ch := range name {
		if ('a' <= ch && ch <= 'z') || ('A' <= ch && ch <= 'Z') || ('0' <= ch && ch <= '9') || ch == '_' {
			builder.WriteRune(ch)
			continue
		}
		builder.WriteByte('_')
	}
	if builder.Len() == 0 {
		return "g"
	}
	return builder.String()
}
