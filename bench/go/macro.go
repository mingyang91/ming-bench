package ming

import "fmt"

type syntaxMacro struct {
	name      string
	literals  map[string]struct{}
	rules     []syntaxRule
	defEnv    *env
	aliasKeys map[string]string
}

type syntaxRule struct {
	pattern      any
	template     any
	patternVars  map[string]struct{}
	repeatedVars map[string]struct{}
}

type matchBindings struct {
	scalars map[string]any
	repeats map[string][]any
}

func parseSyntaxRules(name symbolExpr, expr any, defEnv *env) (*syntaxMacro, error) {
	form, ok := expr.(listExpr)
	if !ok {
		return nil, name.pos.errorf("define-syntax expects a syntax-rules form")
	}
	if len(form.elements) < 3 {
		return nil, form.pos.errorf("syntax-rules expects literals and at least one rule")
	}

	head, ok := form.elements[0].(symbolExpr)
	if !ok || head.name != "syntax-rules" {
		return nil, form.pos.errorf("define-syntax expects a syntax-rules form")
	}

	literalList, ok := form.elements[1].(listExpr)
	if !ok {
		return nil, exprSourcePos(form.elements[1]).errorf("syntax-rules literals must be a list")
	}

	literals := map[string]struct{}{
		name.name: {},
	}
	for _, literalExpr := range literalList.elements {
		literal, ok := literalExpr.(symbolExpr)
		if !ok {
			return nil, exprSourcePos(literalExpr).errorf("syntax-rules literals must be identifiers")
		}
		literals[literal.name] = struct{}{}
	}

	rules := make([]syntaxRule, 0, len(form.elements)-2)
	for _, ruleExpr := range form.elements[2:] {
		ruleForm, ok := ruleExpr.(listExpr)
		if !ok || len(ruleForm.elements) != 2 {
			return nil, exprSourcePos(ruleExpr).errorf("syntax-rules expects pattern/template pairs")
		}

		patternVars, repeatedVars, err := analyzePatternVars(ruleForm.elements[0], literals)
		if err != nil {
			return nil, err
		}

		rules = append(rules, syntaxRule{
			pattern:      ruleForm.elements[0],
			template:     ruleForm.elements[1],
			patternVars:  patternVars,
			repeatedVars: repeatedVars,
		})
	}

	return &syntaxMacro{
		name:      name.name,
		literals:  literals,
		rules:     rules,
		defEnv:    defEnv,
		aliasKeys: map[string]string{},
	}, nil
}

func analyzePatternVars(pattern any, literals map[string]struct{}) (map[string]struct{}, map[string]struct{}, error) {
	patternVars := map[string]struct{}{}
	repeatedVars := map[string]struct{}{}
	varKinds := map[string]bool{}

	var walk func(any, bool) error
	walk = func(node any, repeated bool) error {
		switch node := node.(type) {
		case symbolExpr:
			if node.name == "..." {
				return node.pos.errorf("unexpected ellipsis in syntax-rules pattern")
			}
			if node.name == "_" {
				return nil
			}
			if _, ok := literals[node.name]; ok {
				return nil
			}
			if prevRepeated, ok := varKinds[node.name]; ok {
				if prevRepeated != repeated {
					return node.pos.errorf("pattern variable %s used with inconsistent ellipsis depth", node.name)
				}
				return nil
			}
			varKinds[node.name] = repeated
			patternVars[node.name] = struct{}{}
			if repeated {
				repeatedVars[node.name] = struct{}{}
			}
		case listExpr:
			for i := 0; i < len(node.elements); i++ {
				if isEllipsisExpr(node.elements[i]) {
					return node.pos.errorf("unexpected ellipsis in syntax-rules pattern")
				}
				if i+1 < len(node.elements) && isEllipsisExpr(node.elements[i+1]) {
					if i+2 != len(node.elements) {
						return node.pos.errorf("ellipsis must appear at the end of a pattern list")
					}
					if err := walk(node.elements[i], true); err != nil {
						return err
					}
					i++
					continue
				}
				if err := walk(node.elements[i], repeated); err != nil {
					return err
				}
			}
		}
		return nil
	}

	if err := walk(pattern, false); err != nil {
		return nil, nil, err
	}

	return patternVars, repeatedVars, nil
}

func (m *syntaxMacro) expand(call listExpr) (any, error) {
	for _, rule := range m.rules {
		bindings, ok := rule.match(call, m.literals)
		if !ok {
			continue
		}
		return m.instantiate(rule, rule.template, bindings, map[string]string{}, nil)
	}
	return nil, call.pos.errorf("no syntax-rules pattern matched for %s", m.name)
}

func (r syntaxRule) match(expr any, literals map[string]struct{}) (*matchBindings, bool) {
	bindings := &matchBindings{
		scalars: map[string]any{},
		repeats: map[string][]any{},
	}
	for name := range r.repeatedVars {
		bindings.repeats[name] = []any{}
	}

	if !r.matchPattern(r.pattern, expr, literals, bindings, false) {
		return nil, false
	}
	return bindings, true
}

func (r syntaxRule) matchPattern(pattern any, expr any, literals map[string]struct{}, bindings *matchBindings, repeated bool) bool {
	switch pattern := pattern.(type) {
	case symbolExpr:
		return r.matchSymbol(pattern, expr, literals, bindings, repeated)
	case listExpr:
		return r.matchList(pattern, expr, literals, bindings, repeated)
	default:
		return syntaxEqual(pattern, expr)
	}
}

func (r syntaxRule) matchSymbol(pattern symbolExpr, expr any, literals map[string]struct{}, bindings *matchBindings, repeated bool) bool {
	if pattern.name == "_" {
		return true
	}
	if _, ok := literals[pattern.name]; ok {
		symbol, ok := expr.(symbolExpr)
		return ok && symbol.name == pattern.name
	}
	if _, ok := r.repeatedVars[pattern.name]; ok && repeated {
		bindings.repeats[pattern.name] = append(bindings.repeats[pattern.name], cloneSyntax(expr))
		return true
	}
	if _, ok := r.patternVars[pattern.name]; ok {
		if existing, ok := bindings.scalars[pattern.name]; ok {
			return syntaxEqual(existing, expr)
		}
		bindings.scalars[pattern.name] = cloneSyntax(expr)
		return true
	}
	return syntaxEqual(pattern, expr)
}

func (r syntaxRule) matchList(pattern listExpr, expr any, literals map[string]struct{}, bindings *matchBindings, repeated bool) bool {
	actual, ok := expr.(listExpr)
	if !ok {
		return false
	}

	exprIndex := 0
	for patternIndex := 0; patternIndex < len(pattern.elements); patternIndex++ {
		if isEllipsisExpr(pattern.elements[patternIndex]) {
			return false
		}
		if patternIndex+1 < len(pattern.elements) && isEllipsisExpr(pattern.elements[patternIndex+1]) {
			if patternIndex+2 != len(pattern.elements) {
				return false
			}
			for exprIndex < len(actual.elements) {
				if !r.matchPattern(pattern.elements[patternIndex], actual.elements[exprIndex], literals, bindings, true) {
					return false
				}
				exprIndex++
			}
			return true
		}
		if exprIndex >= len(actual.elements) {
			return false
		}
		if !r.matchPattern(pattern.elements[patternIndex], actual.elements[exprIndex], literals, bindings, repeated) {
			return false
		}
		exprIndex++
	}

	return exprIndex == len(actual.elements)
}

func (m *syntaxMacro) instantiate(rule syntaxRule, template any, bindings *matchBindings, renames map[string]string, repeatIndex *int) (any, error) {
	switch template := template.(type) {
	case symbolExpr:
		return m.instantiateSymbol(rule, template, bindings, renames, repeatIndex)
	case listExpr:
		return m.instantiateList(rule, template, bindings, renames, repeatIndex)
	case stringExpr:
		return template, nil
	default:
		return template, nil
	}
}

func (m *syntaxMacro) instantiateSymbol(rule syntaxRule, template symbolExpr, bindings *matchBindings, renames map[string]string, repeatIndex *int) (any, error) {
	if _, ok := rule.patternVars[template.name]; ok {
		if _, ok := rule.repeatedVars[template.name]; ok {
			if repeatIndex == nil {
				return nil, template.pos.errorf("pattern variable %s must be used with ellipsis in the template", template.name)
			}
			values := bindings.repeats[template.name]
			if *repeatIndex < 0 || *repeatIndex >= len(values) {
				return nil, template.pos.errorf("macro ellipsis index out of range")
			}
			return cloneSyntax(values[*repeatIndex]), nil
		}
		value, ok := bindings.scalars[template.name]
		if !ok {
			return nil, template.pos.errorf("unbound pattern variable: %s", template.name)
		}
		return cloneSyntax(value), nil
	}

	if key, ok := renames[template.name]; ok {
		template.key = key
		return template, nil
	}
	if isCoreSyntaxName(template.name) {
		return template, nil
	}
	if aliasKey := m.captureIdentifier(template.name); aliasKey != "" {
		template.key = aliasKey
	}
	return template, nil
}

func (m *syntaxMacro) instantiateList(rule syntaxRule, template listExpr, bindings *matchBindings, renames map[string]string, repeatIndex *int) (any, error) {
	if len(template.elements) > 0 {
		if head, ok := template.elements[0].(symbolExpr); ok {
			if _, isPatternVar := rule.patternVars[head.name]; !isPatternVar {
				switch head.name {
				case "let":
					return m.instantiateLet(rule, template, bindings, renames, repeatIndex)
				case "lambda":
					return m.instantiateLambda(rule, template, bindings, renames, repeatIndex)
				}
			}
		}
	}
	return m.instantiatePlainList(rule, template, bindings, renames, repeatIndex)
}

func (m *syntaxMacro) instantiatePlainList(rule syntaxRule, template listExpr, bindings *matchBindings, renames map[string]string, repeatIndex *int) (any, error) {
	elements := make([]any, 0, len(template.elements))
	for i := 0; i < len(template.elements); i++ {
		if isEllipsisExpr(template.elements[i]) {
			return nil, template.pos.errorf("unexpected ellipsis in syntax-rules template")
		}
		if i+1 < len(template.elements) && isEllipsisExpr(template.elements[i+1]) {
			if repeatIndex != nil {
				return nil, template.pos.errorf("nested ellipsis in syntax-rules templates is not supported")
			}
			count, err := rule.repeatCount(template.elements[i], bindings)
			if err != nil {
				return nil, err
			}
			for j := 0; j < count; j++ {
				index := j
				value, err := m.instantiate(rule, template.elements[i], bindings, renames, &index)
				if err != nil {
					return nil, err
				}
				elements = append(elements, value)
			}
			i++
			continue
		}

		value, err := m.instantiate(rule, template.elements[i], bindings, renames, repeatIndex)
		if err != nil {
			return nil, err
		}
		elements = append(elements, value)
	}

	return listExpr{elements: elements, pos: template.pos}, nil
}

func (m *syntaxMacro) instantiateLet(rule syntaxRule, template listExpr, bindings *matchBindings, renames map[string]string, repeatIndex *int) (any, error) {
	if len(template.elements) < 3 {
		return m.instantiatePlainList(rule, template, bindings, renames, repeatIndex)
	}

	bindingList, ok := template.elements[1].(listExpr)
	if !ok {
		return m.instantiatePlainList(rule, template, bindings, renames, repeatIndex)
	}

	bodyRenames := copyRenames(renames)
	instantiatedBindings := listExpr{pos: bindingList.pos}
	for _, bindingExpr := range bindingList.elements {
		binding, ok := bindingExpr.(listExpr)
		if !ok || len(binding.elements) != 2 {
			return nil, exprSourcePos(bindingExpr).errorf("macro-generated let binding must contain a name and value")
		}

		name, ok := binding.elements[0].(symbolExpr)
		if !ok {
			return nil, exprSourcePos(binding.elements[0]).errorf("macro-generated let binding name must be a symbol")
		}

		instantiatedName, introducedKey, err := m.instantiateBinderSymbol(rule, name, bindings, renames, repeatIndex)
		if err != nil {
			return nil, err
		}
		if introducedKey != "" {
			bodyRenames[name.name] = introducedKey
		}

		initExpr, err := m.instantiate(rule, binding.elements[1], bindings, renames, repeatIndex)
		if err != nil {
			return nil, err
		}

		instantiatedBindings.elements = append(instantiatedBindings.elements, listExpr{
			elements: []any{instantiatedName, initExpr},
			pos:      binding.pos,
		})
	}

	elements := []any{template.elements[0], instantiatedBindings}
	for _, bodyExpr := range template.elements[2:] {
		value, err := m.instantiate(rule, bodyExpr, bindings, bodyRenames, repeatIndex)
		if err != nil {
			return nil, err
		}
		elements = append(elements, value)
	}

	return listExpr{elements: elements, pos: template.pos}, nil
}

func (m *syntaxMacro) instantiateLambda(rule syntaxRule, template listExpr, bindings *matchBindings, renames map[string]string, repeatIndex *int) (any, error) {
	if len(template.elements) < 3 {
		return m.instantiatePlainList(rule, template, bindings, renames, repeatIndex)
	}

	formals, bodyRenames, err := m.instantiateFormals(rule, template.elements[1], bindings, renames, repeatIndex)
	if err != nil {
		return nil, err
	}

	elements := []any{template.elements[0], formals}
	for _, bodyExpr := range template.elements[2:] {
		value, err := m.instantiate(rule, bodyExpr, bindings, bodyRenames, repeatIndex)
		if err != nil {
			return nil, err
		}
		elements = append(elements, value)
	}

	return listExpr{elements: elements, pos: template.pos}, nil
}

func (m *syntaxMacro) instantiateFormals(rule syntaxRule, formals any, bindings *matchBindings, renames map[string]string, repeatIndex *int) (any, map[string]string, error) {
	bodyRenames := copyRenames(renames)

	switch formals := formals.(type) {
	case symbolExpr:
		instantiated, introducedKey, err := m.instantiateBinderSymbol(rule, formals, bindings, renames, repeatIndex)
		if err != nil {
			return nil, nil, err
		}
		if introducedKey != "" {
			bodyRenames[formals.name] = introducedKey
		}
		return instantiated, bodyRenames, nil
	case listExpr:
		elements := make([]any, 0, len(formals.elements))
		for _, formalExpr := range formals.elements {
			symbol, ok := formalExpr.(symbolExpr)
			if !ok {
				return nil, nil, exprSourcePos(formalExpr).errorf("macro-generated lambda parameters must be symbols")
			}
			if symbol.name == "." {
				elements = append(elements, symbol)
				continue
			}

			instantiated, introducedKey, err := m.instantiateBinderSymbol(rule, symbol, bindings, renames, repeatIndex)
			if err != nil {
				return nil, nil, err
			}
			if introducedKey != "" {
				bodyRenames[symbol.name] = introducedKey
			}
			elements = append(elements, instantiated)
		}
		return listExpr{elements: elements, pos: formals.pos}, bodyRenames, nil
	default:
		return nil, nil, exprSourcePos(formals).errorf("macro-generated lambda parameters must be symbols")
	}
}

func (m *syntaxMacro) instantiateBinderSymbol(rule syntaxRule, template symbolExpr, bindings *matchBindings, renames map[string]string, repeatIndex *int) (symbolExpr, string, error) {
	if _, ok := rule.patternVars[template.name]; ok {
		value, err := m.instantiateSymbol(rule, template, bindings, renames, repeatIndex)
		if err != nil {
			return symbolExpr{}, "", err
		}
		symbol, ok := value.(symbolExpr)
		if !ok {
			return symbolExpr{}, "", template.pos.errorf("macro-generated binding position requires an identifier")
		}
		return symbol, "", nil
	}

	template.key = m.nextGeneratedKey("bind", template.name)
	return template, template.key, nil
}

func (m *syntaxMacro) captureIdentifier(name string) string {
	if aliasKey, ok := m.aliasKeys[name]; ok {
		return aliasKey
	}

	cell, hasValueBinding := m.defEnv.lookupCellByKey(name)
	macro, hasMacroBinding := m.defEnv.lookupMacroByKey(name)
	if !hasValueBinding && !hasMacroBinding {
		return ""
	}

	aliasKey := m.nextGeneratedKey("capture", name)
	if hasValueBinding {
		m.defEnv.defineAlias(aliasKey, cell)
	}
	if hasMacroBinding {
		m.defEnv.defineMacroKey(aliasKey, macro)
	}
	m.aliasKeys[name] = aliasKey
	return aliasKey
}

func (r syntaxRule) repeatCount(template any, bindings *matchBindings) (int, error) {
	count := -1

	var walk func(any) error
	walk = func(node any) error {
		switch node := node.(type) {
		case symbolExpr:
			if _, ok := r.repeatedVars[node.name]; ok {
				size := len(bindings.repeats[node.name])
				if count == -1 {
					count = size
				} else if count != size {
					return node.pos.errorf("macro ellipsis variables expanded to different lengths")
				}
			}
		case listExpr:
			for _, elem := range node.elements {
				if err := walk(elem); err != nil {
					return err
				}
			}
		}
		return nil
	}

	if err := walk(template); err != nil {
		return 0, err
	}
	if count == -1 {
		return 0, exprSourcePos(template).errorf("ellipsis template must contain a repeated pattern variable")
	}
	return count, nil
}

func syntaxEqual(left, right any) bool {
	return valuesEqual(quoteDatum(cloneSyntax(left)), quoteDatum(cloneSyntax(right)))
}

func cloneSyntax(expr any) any {
	switch expr := expr.(type) {
	case symbolExpr:
		return expr
	case stringExpr:
		return expr
	case listExpr:
		elements := make([]any, len(expr.elements))
		for i, elem := range expr.elements {
			elements[i] = cloneSyntax(elem)
		}
		return listExpr{elements: elements, pos: expr.pos}
	default:
		return expr
	}
}

func copyRenames(src map[string]string) map[string]string {
	dst := make(map[string]string, len(src))
	for name, key := range src {
		dst[name] = key
	}
	return dst
}

func exprSourcePos(expr any) sourcePos {
	switch expr := expr.(type) {
	case symbolExpr:
		return expr.pos
	case stringExpr:
		return expr.pos
	case listExpr:
		return expr.pos
	default:
		return sourcePos{line: 1, col: 1}
	}
}

func isEllipsisExpr(expr any) bool {
	symbol, ok := expr.(symbolExpr)
	return ok && symbol.name == "..."
}

func isCoreSyntaxName(name string) bool {
	switch name {
	case "and", "begin", "case", "case-lambda", "cond", "define", "define-record-type", "define-syntax", "do", "guard", "if", "lambda", "let", "let*", "letrec", "letrec*", "or", "quote", "set!", "syntax", "syntax-case", "syntax-rules", "with-syntax":
		return true
	default:
		return false
	}
}

func (m *syntaxMacro) nextGeneratedKey(kind string, name string) string {
	if m.defEnv == nil || m.defEnv.runtime == nil {
		return fmt.Sprintf("__%s_0_%s", kind, name)
	}
	m.defEnv.runtime.macroKeyCounter++
	return fmt.Sprintf("__%s_%d_%s", kind, m.defEnv.runtime.macroKeyCounter, name)
}
