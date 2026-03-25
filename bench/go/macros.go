package ming

import "fmt"

type syntaxRulesMacro struct {
	keyword  string
	literals map[string]struct{}
	rules    []syntaxRule
	env      *environment
}

type syntaxRule struct {
	pattern  node
	template node
}

type macroMatch struct {
	single   map[string]node
	repeated map[string][]node
}

type templateContext struct {
	macro      *syntaxRulesMacro
	match      *macroMatch
	introduced map[string]string
}

var macroKeywordNames = map[string]struct{}{
	"and":                {},
	"begin":              {},
	"case":               {},
	"cond":               {},
	"case-lambda":        {},
	"define":             {},
	"define-record-type": {},
	"define-syntax":      {},
	"do":                 {},
	"if":                 {},
	"lambda":             {},
	"let":                {},
	"letrec":             {},
	"letrec*":            {},
	"or":                 {},
	"quote":              {},
	"set!":               {},
	"syntax-rules":       {},
}

var macroGensymCounter int

func evalDefineSyntax(args []node, env *environment) (value, error) {
	if len(args) != 2 {
		return nil, &EvalError{Message: "define-syntax expects exactly 2 arguments"}
	}

	nameNode, ok := args[0].(symbolNode)
	if !ok {
		return nil, &EvalError{Message: "define-syntax requires a symbol"}
	}

	transformer, err := parseSyntaxRules(nameNode.name, args[1], env)
	if err != nil {
		return nil, err
	}

	env.defineMacro(nameNode.name, transformer)
	return voidValue{}, nil
}

func expandMacros(expr node, env *environment) (node, error) {
	current := expr
	for {
		next, changed, err := expandOnce(current, env)
		if err != nil {
			return nil, err
		}
		if !changed {
			return current, nil
		}
		current = next
	}
}

func expandOnce(expr node, env *environment) (node, bool, error) {
	list, ok := expr.(listNode)
	if !ok || len(list.elements) == 0 {
		return expr, false, nil
	}

	if name, ok := symbolName(list.elements[0]); ok && name == "quote" {
		return expr, false, nil
	}

	name, ok := symbolName(list.elements[0])
	if !ok {
		return expr, false, nil
	}

	transformer, ok := env.lookupMacro(name)
	if !ok {
		return expr, false, nil
	}

	expanded, err := transformer.expand(list)
	if err != nil {
		return nil, false, err
	}
	return expanded, true, nil
}

func parseSyntaxRules(keyword string, expr node, env *environment) (*syntaxRulesMacro, error) {
	form, ok := expr.(listNode)
	if !ok || len(form.elements) < 3 {
		return nil, &EvalError{Message: "define-syntax expects a syntax-rules form"}
	}

	head, ok := symbolName(form.elements[0])
	if !ok || head != "syntax-rules" {
		return nil, &EvalError{Message: "define-syntax expects a syntax-rules form"}
	}

	literalList, ok := form.elements[1].(listNode)
	if !ok {
		return nil, &EvalError{Message: "syntax-rules literals must be a list"}
	}

	literals := map[string]struct{}{}
	for _, literal := range literalList.elements {
		name, ok := symbolName(literal)
		if !ok {
			return nil, &EvalError{Message: "syntax-rules literals must be symbols"}
		}
		literals[name] = struct{}{}
	}

	rules := make([]syntaxRule, 0, len(form.elements)-2)
	for _, ruleExpr := range form.elements[2:] {
		ruleList, ok := ruleExpr.(listNode)
		if !ok || len(ruleList.elements) != 2 {
			return nil, &EvalError{Message: "syntax-rules clauses must contain a pattern and template"}
		}

		rules = append(rules, syntaxRule{
			pattern:  ruleList.elements[0],
			template: ruleList.elements[1],
		})
	}

	return &syntaxRulesMacro{
		keyword:  keyword,
		literals: literals,
		rules:    rules,
		env:      env,
	}, nil
}

func (e *environment) defineMacro(name string, macro *syntaxRulesMacro) {
	e.macros[name] = macro
}

func (e *environment) lookupMacro(name string) (*syntaxRulesMacro, bool) {
	for current := e; current != nil; current = current.parent {
		transformer, ok := current.macros[name]
		if ok {
			return transformer, true
		}
	}
	return nil, false
}

func (m *syntaxRulesMacro) expand(call listNode) (node, error) {
	for _, rule := range m.rules {
		match := newMacroMatch()
		if !m.matchPattern(rule.pattern, call, match, false) {
			continue
		}

		ctx := &templateContext{
			macro:      m,
			match:      match,
			introduced: map[string]string{},
		}
		return ctx.instantiate(rule.template, -1)
	}

	return nil, errorAt(call.pos, "no matching syntax-rules clause for %s", m.keyword)
}

func (m *syntaxRulesMacro) matchPattern(pattern node, expr node, captures *macroMatch, repeated bool) bool {
	switch pattern := pattern.(type) {
	case integerValue:
		other, ok := expr.(integerValue)
		return ok && other == pattern
	case booleanValue:
		other, ok := expr.(booleanValue)
		return ok && other == pattern
	case stringValue:
		other, ok := expr.(stringValue)
		return ok && other == pattern
	case charValue:
		other, ok := expr.(charValue)
		return ok && other == pattern
	case symbolNode:
		if pattern.name == "..." {
			return false
		}

		if m.isLiteral(pattern.name) {
			other, ok := expr.(symbolNode)
			return ok && other.name == pattern.name
		}

		if repeated {
			captures.repeated[pattern.name] = append(captures.repeated[pattern.name], cloneNode(expr))
			return true
		}

		if existing, ok := captures.single[pattern.name]; ok {
			return syntaxEqual(existing, expr)
		}
		captures.single[pattern.name] = cloneNode(expr)
		return true
	case listNode:
		other, ok := expr.(listNode)
		if !ok {
			return false
		}
		return m.matchList(pattern.elements, other.elements, captures, repeated)
	default:
		return false
	}
}

func (m *syntaxRulesMacro) matchList(patternElems []node, exprElems []node, captures *macroMatch, repeated bool) bool {
	if len(patternElems) == 0 {
		return len(exprElems) == 0
	}

	if len(patternElems) >= 2 && isEllipsisNode(patternElems[1]) {
		subpattern := patternElems[0]
		tail := patternElems[2:]
		minTail := minPatternLength(tail)
		if len(exprElems) < minTail {
			return false
		}

		maxCount := len(exprElems) - minTail
		for count := 0; count <= maxCount; count++ {
			trial := captures.clone()
			m.seedRepeatedBindings(subpattern, trial)
			ok := true
			for i := 0; i < count; i++ {
				if !m.matchPattern(subpattern, exprElems[i], trial, true) {
					ok = false
					break
				}
			}
			if ok && m.matchList(tail, exprElems[count:], trial, repeated) {
				*captures = *trial
				return true
			}
		}
		return false
	}

	if len(exprElems) == 0 {
		return false
	}
	if !m.matchPattern(patternElems[0], exprElems[0], captures, repeated) {
		return false
	}
	return m.matchList(patternElems[1:], exprElems[1:], captures, repeated)
}

func (m *syntaxRulesMacro) seedRepeatedBindings(pattern node, captures *macroMatch) {
	switch pattern := pattern.(type) {
	case symbolNode:
		if pattern.name == "..." || m.isLiteral(pattern.name) {
			return
		}
		if _, ok := captures.repeated[pattern.name]; !ok {
			captures.repeated[pattern.name] = nil
		}
	case listNode:
		for _, element := range pattern.elements {
			m.seedRepeatedBindings(element, captures)
		}
	}
}

func (m *syntaxRulesMacro) isLiteral(name string) bool {
	if name == m.keyword {
		return true
	}
	_, ok := m.literals[name]
	return ok
}

func newMacroMatch() *macroMatch {
	return &macroMatch{
		single:   map[string]node{},
		repeated: map[string][]node{},
	}
}

func (m *macroMatch) clone() *macroMatch {
	copyMatch := newMacroMatch()
	for name, expr := range m.single {
		copyMatch.single[name] = expr
	}
	for name, exprs := range m.repeated {
		copied := make([]node, len(exprs))
		copy(copied, exprs)
		copyMatch.repeated[name] = copied
	}
	return copyMatch
}

func (ctx *templateContext) instantiate(template node, repeatIndex int) (node, error) {
	switch template := template.(type) {
	case integerValue, booleanValue, stringValue, charValue:
		return template, nil
	case symbolNode:
		if template.name == "..." {
			return nil, &EvalError{Message: "invalid template ellipsis"}
		}

		if expr, ok := ctx.match.single[template.name]; ok {
			return cloneNode(expr), nil
		}

		if exprs, ok := ctx.match.repeated[template.name]; ok {
			if repeatIndex < 0 || repeatIndex >= len(exprs) {
				return nil, &EvalError{Message: "ellipsis variable used outside matching repetition"}
			}
			return cloneNode(exprs[repeatIndex]), nil
		}

		return ctx.introducedSymbol(template), nil
	case listNode:
		elements := make([]node, 0, len(template.elements))
		for i := 0; i < len(template.elements); i++ {
			if i+1 < len(template.elements) && isEllipsisNode(template.elements[i+1]) {
				repeated, err := ctx.instantiateRepeated(template.elements[i])
				if err != nil {
					return nil, err
				}
				elements = append(elements, repeated...)
				i++
				continue
			}

			element, err := ctx.instantiate(template.elements[i], repeatIndex)
			if err != nil {
				return nil, err
			}
			elements = append(elements, element)
		}

		return listNode{elements: elements, pos: template.pos}, nil
	default:
		return nil, &EvalError{Message: "invalid syntax template"}
	}
}

func (ctx *templateContext) instantiateRepeated(template node) ([]node, error) {
	vars := map[string]struct{}{}
	collectRepeatedTemplateVars(template, ctx.match, vars)
	if len(vars) == 0 {
		return nil, &EvalError{Message: "template ellipsis requires a repeated pattern variable"}
	}

	count := -1
	for name := range vars {
		current := len(ctx.match.repeated[name])
		if count == -1 {
			count = current
			continue
		}
		if count != current {
			return nil, &EvalError{Message: "mismatched ellipsis lengths"}
		}
	}

	result := make([]node, 0, count)
	for i := 0; i < count; i++ {
		element, err := ctx.instantiate(template, i)
		if err != nil {
			return nil, err
		}
		result = append(result, element)
	}
	return result, nil
}

func (ctx *templateContext) introducedSymbol(sym symbolNode) symbolNode {
	if isCoreKeyword(sym.name) {
		return symbolNode{name: sym.name, pos: sym.pos}
	}

	if _, ok := ctx.macro.env.lookupMacro(sym.name); ok {
		return symbolNode{name: sym.name, pos: sym.pos}
	}

	if binding, ok := ctx.macro.env.lookupBinding(sym.name); ok {
		return symbolNode{name: sym.name, pos: sym.pos, captured: binding}
	}

	renamed, ok := ctx.introduced[sym.name]
	if !ok {
		renamed = nextMacroName(sym.name)
		ctx.introduced[sym.name] = renamed
	}
	return symbolNode{name: renamed, pos: sym.pos}
}

func cloneNode(expr node) node {
	switch expr := expr.(type) {
	case integerValue, booleanValue, stringValue, charValue:
		return expr
	case symbolNode:
		return symbolNode{
			name:     expr.name,
			pos:      expr.pos,
			captured: expr.captured,
		}
	case listNode:
		elements := make([]node, len(expr.elements))
		for i, element := range expr.elements {
			elements[i] = cloneNode(element)
		}
		return listNode{elements: elements, pos: expr.pos}
	default:
		return nil
	}
}

func syntaxEqual(left node, right node) bool {
	switch left := left.(type) {
	case integerValue:
		right, ok := right.(integerValue)
		return ok && left == right
	case booleanValue:
		right, ok := right.(booleanValue)
		return ok && left == right
	case stringValue:
		right, ok := right.(stringValue)
		return ok && left == right
	case charValue:
		right, ok := right.(charValue)
		return ok && left == right
	case symbolNode:
		right, ok := right.(symbolNode)
		return ok && left.name == right.name
	case listNode:
		right, ok := right.(listNode)
		if !ok || len(left.elements) != len(right.elements) {
			return false
		}
		for i, element := range left.elements {
			if !syntaxEqual(element, right.elements[i]) {
				return false
			}
		}
		return true
	default:
		return false
	}
}

func collectRepeatedTemplateVars(expr node, match *macroMatch, names map[string]struct{}) {
	switch expr := expr.(type) {
	case symbolNode:
		if _, ok := match.repeated[expr.name]; ok {
			names[expr.name] = struct{}{}
		}
	case listNode:
		for _, element := range expr.elements {
			collectRepeatedTemplateVars(element, match, names)
		}
	}
}

func isEllipsisNode(expr node) bool {
	name, ok := symbolName(expr)
	return ok && name == "..."
}

func minPatternLength(patternElems []node) int {
	total := 0
	for i := 0; i < len(patternElems); i++ {
		if i+1 < len(patternElems) && isEllipsisNode(patternElems[i+1]) {
			i++
			continue
		}
		total++
	}
	return total
}

func isCoreKeyword(name string) bool {
	_, ok := macroKeywordNames[name]
	return ok
}

func nextMacroName(name string) string {
	macroGensymCounter++
	return fmt.Sprintf("__macro_%d_%s", macroGensymCounter, name)
}
