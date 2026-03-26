package ming

import "fmt"

type macroExpr struct {
	name     string
	transformer expr
	literals map[string]struct{}
	rules    []macroRule
	defEnv   *env
}

type macroRule struct {
	pattern  listExpr
	template expr
}

type syntaxBindings struct {
	single   map[string]expr
	repeated map[string][]expr
}

type templateContext struct {
	bindings *syntaxBindings
	defEnv   *env
	renamed  map[string]string
}

var macroExpansionCounter int

func evalDefineSyntax(environment *env, forms []expr) (expr, error) {
	if len(forms) != 2 {
		return nil, &EvalError{Message: "define-syntax expects exactly 2 arguments"}
	}

	name, ok := forms[0].(symbolExpr)
	if !ok {
		return nil, &EvalError{Message: "define-syntax name must be a symbol"}
	}

	if transformer, ok, err := tryCompileSyntaxRules(environment, name.name, forms[1]); err != nil {
		return nil, err
	} else if ok {
		environment.define(name.name, transformer)
		return voidExpr{}, nil
	}

	transformer, err := evalSingleExpr(environment, forms[1], "define-syntax")
	if err != nil {
		return nil, err
	}
	if !isProcedure(transformer) {
		return nil, errorAt(name.pos, "define-syntax requires a transformer procedure")
	}

	environment.define(name.name, macroExpr{
		name:        name.name,
		transformer: transformer,
		defEnv:      environment,
	})
	return voidExpr{}, nil
}

func tryCompileSyntaxRules(environment *env, macroName string, form expr) (macroExpr, bool, error) {
	rulesForm, ok := form.(listExpr)
	if !ok || len(rulesForm.items) == 0 {
		return macroExpr{}, false, nil
	}

	head, ok := rulesForm.items[0].(symbolExpr)
	if !ok || head.name != "syntax-rules" {
		return macroExpr{}, false, nil
	}

	transformer, err := compileSyntaxRules(environment, macroName, form)
	if err != nil {
		return macroExpr{}, true, err
	}
	return transformer, true, nil
}

func compileSyntaxRules(environment *env, macroName string, form expr) (macroExpr, error) {
	rulesForm, ok := form.(listExpr)
	if !ok || len(rulesForm.items) < 3 {
		return macroExpr{}, &EvalError{Message: "syntax-rules expects a literals list and at least one rule"}
	}

	head, ok := rulesForm.items[0].(symbolExpr)
	if !ok || head.name != "syntax-rules" {
		return macroExpr{}, &EvalError{Message: "define-syntax expects a syntax-rules transformer"}
	}

	literalList, ok := rulesForm.items[1].(listExpr)
	if !ok {
		return macroExpr{}, &EvalError{Message: "syntax-rules literals must be a list"}
	}

	literals := make(map[string]struct{}, len(literalList.items))
	for _, literal := range literalList.items {
		symbol, ok := literal.(symbolExpr)
		if !ok {
			return macroExpr{}, &EvalError{Message: "syntax-rules literals must be symbols"}
		}
		literals[symbol.name] = struct{}{}
	}

	rules := make([]macroRule, 0, len(rulesForm.items)-2)
	for _, rawRule := range rulesForm.items[2:] {
		rule, ok := rawRule.(listExpr)
		if !ok || len(rule.items) != 2 {
			return macroExpr{}, &EvalError{Message: "syntax-rules clauses must have the form (pattern template)"}
		}

		pattern, ok := rule.items[0].(listExpr)
		if !ok || len(pattern.items) == 0 {
			return macroExpr{}, &EvalError{Message: "syntax-rules patterns must be non-empty lists"}
		}

		patternHead, ok := pattern.items[0].(symbolExpr)
		if !ok || patternHead.name != macroName {
			return macroExpr{}, &EvalError{Message: "syntax-rules pattern must start with the macro name"}
		}

		rules = append(rules, macroRule{
			pattern:  pattern,
			template: rule.items[1],
		})
	}

	return macroExpr{
		name:     macroName,
		literals: literals,
		rules:    rules,
		defEnv:   environment,
	}, nil
}

func expandMacro(macro macroExpr, call listExpr) (expr, error) {
	if macro.transformer != nil {
		return expandProcedureMacro(macro, call)
	}

	for _, rule := range macro.rules {
		bindings := newSyntaxBindings()
		if !matchMacroPattern(rule.pattern, call, macro.literals, bindings) {
			continue
		}

		return expandTemplate(rule.template, templateContext{
			bindings: bindings,
			defEnv:   macro.defEnv,
			renamed:  map[string]string{},
		}, nil)
	}

	return nil, &EvalError{Message: fmt.Sprintf("no matching syntax-rules pattern for %s", macro.name)}
}

func expandProcedureMacro(macro macroExpr, call listExpr) (expr, error) {
	result, err := applyCallable(macro.transformer, []expr{
		&syntaxExpr{datum: cloneSyntax(call)},
	})
	if err != nil {
		return nil, err
	}

	single, err := expectSingleValue(result, "macro transformer")
	if err != nil {
		return nil, err
	}

	syntax, ok := single.(*syntaxExpr)
	if !ok {
		return nil, &EvalError{Message: "macro transformer must return a syntax object"}
	}

	return cloneSyntax(syntax.datum), nil
}

func newSyntaxBindings() *syntaxBindings {
	return &syntaxBindings{
		single:   map[string]expr{},
		repeated: map[string][]expr{},
	}
}

func (b *syntaxBindings) hasPatternVar(name string) bool {
	_, okSingle := b.single[name]
	_, okRepeated := b.repeated[name]
	return okSingle || okRepeated
}

func (b *syntaxBindings) bindSingle(name string, value expr) bool {
	if name == "_" {
		return true
	}

	if existing, ok := b.single[name]; ok {
		return equalExpr(existing, value)
	}
	if _, ok := b.repeated[name]; ok {
		return false
	}

	b.single[name] = value
	return true
}

func (b *syntaxBindings) appendRepeatedMatches(local *syntaxBindings, names map[string]struct{}) bool {
	for name := range names {
		if repeatedValues, ok := local.repeated[name]; ok && len(repeatedValues) > 0 {
			return false
		}

		value, ok := local.single[name]
		if !ok {
			return false
		}
		if _, ok := b.single[name]; ok {
			return false
		}
		b.repeated[name] = append(b.repeated[name], value)
	}
	return true
}

func (b *syntaxBindings) ensureRepeatedNames(names map[string]struct{}) bool {
	for name := range names {
		if _, ok := b.single[name]; ok {
			return false
		}
		if _, ok := b.repeated[name]; !ok {
			b.repeated[name] = nil
		}
	}
	return true
}

func (b *syntaxBindings) lookup(name string, repeatIndex *int) (expr, bool, error) {
	if repeatIndex != nil {
		if values, ok := b.repeated[name]; ok {
			index := *repeatIndex
			if index < 0 || index >= len(values) {
				return nil, false, &EvalError{Message: fmt.Sprintf("ellipsis index out of range for %s", name)}
			}
			return values[index], true, nil
		}
	}

	if value, ok := b.single[name]; ok {
		return value, true, nil
	}

	if repeatIndex == nil {
		if _, ok := b.repeated[name]; ok {
			return nil, false, &EvalError{Message: fmt.Sprintf("pattern variable %s used outside ellipsis", name)}
		}
	}

	return nil, false, nil
}

func matchMacroPattern(pattern listExpr, call listExpr, literals map[string]struct{}, bindings *syntaxBindings) bool {
	if len(pattern.items) == 0 || len(call.items) == 0 {
		return false
	}

	patternHead, ok := pattern.items[0].(symbolExpr)
	if !ok {
		return false
	}

	callHead, ok := call.items[0].(symbolExpr)
	if !ok || callHead.name != patternHead.name {
		return false
	}

	return matchListPattern(pattern.items[1:], call.items[1:], literals, bindings)
}

func matchPattern(pattern expr, candidate expr, literals map[string]struct{}, bindings *syntaxBindings) bool {
	switch p := pattern.(type) {
	case symbolExpr:
		if p.name == "_" {
			return true
		}
		if p.name == "..." {
			return false
		}
		if _, ok := literals[p.name]; ok {
			symbol, ok := candidate.(symbolExpr)
			return ok && symbol.name == p.name
		}
		return bindings.bindSingle(p.name, candidate)
	case listExpr:
		list, ok := candidate.(listExpr)
		if !ok {
			return false
		}
		return matchListPattern(p.items, list.items, literals, bindings)
	default:
		return equalExpr(pattern, candidate)
	}
}

func matchListPattern(patternItems []expr, candidateItems []expr, literals map[string]struct{}, bindings *syntaxBindings) bool {
	patternIndex := 0
	candidateIndex := 0

	for patternIndex < len(patternItems) {
		if patternIndex+1 < len(patternItems) && isEllipsisExpr(patternItems[patternIndex+1]) {
			if patternIndex+2 != len(patternItems) {
				return false
			}

			repeatedNames := map[string]struct{}{}
			collectPatternVars(patternItems[patternIndex], literals, repeatedNames)
			if !bindings.ensureRepeatedNames(repeatedNames) {
				return false
			}

			for candidateIndex < len(candidateItems) {
				local := newSyntaxBindings()
				if !matchPattern(patternItems[patternIndex], candidateItems[candidateIndex], literals, local) {
					return false
				}
				if !bindings.appendRepeatedMatches(local, repeatedNames) {
					return false
				}
				candidateIndex++
			}

			patternIndex += 2
			break
		}

		if candidateIndex >= len(candidateItems) {
			return false
		}
		if !matchPattern(patternItems[patternIndex], candidateItems[candidateIndex], literals, bindings) {
			return false
		}
		patternIndex++
		candidateIndex++
	}

	return patternIndex == len(patternItems) && candidateIndex == len(candidateItems)
}

func collectPatternVars(pattern expr, literals map[string]struct{}, out map[string]struct{}) {
	switch value := pattern.(type) {
	case symbolExpr:
		if value.name == "_" || value.name == "..." {
			return
		}
		if _, ok := literals[value.name]; ok {
			return
		}
		out[value.name] = struct{}{}
	case listExpr:
		for i := 0; i < len(value.items); i++ {
			if isEllipsisExpr(value.items[i]) {
				continue
			}
			collectPatternVars(value.items[i], literals, out)
		}
	}
}

func expandTemplate(template expr, ctx templateContext, repeatIndex *int) (expr, error) {
	switch value := template.(type) {
	case symbolExpr:
		return expandTemplateSymbol(value, ctx, repeatIndex)
	case listExpr:
		if expanded, handled, err := expandIntroducedLet(value, ctx, repeatIndex); handled || err != nil {
			return expanded, err
		}
		return expandTemplateList(value, ctx, repeatIndex)
	default:
		return cloneSyntax(template), nil
	}
}

func expandTemplateSymbol(symbol symbolExpr, ctx templateContext, repeatIndex *int) (expr, error) {
	if value, ok, err := ctx.bindings.lookup(symbol.name, repeatIndex); err != nil {
		return nil, err
	} else if ok {
		return cloneSyntax(value), nil
	}

	if renamed, ok := ctx.renamed[symbol.name]; ok {
		return symbolExpr{name: renamed, pos: symbol.pos}, nil
	}

	return symbolExpr{name: symbol.name, pos: symbol.pos, lookupEnv: ctx.defEnv}, nil
}

func expandTemplateList(list listExpr, ctx templateContext, repeatIndex *int) (expr, error) {
	items := make([]expr, 0, len(list.items))

	for i := 0; i < len(list.items); i++ {
		if i+1 < len(list.items) && isEllipsisExpr(list.items[i+1]) {
			repeatCount, err := templateRepeatCount(list.items[i], ctx.bindings)
			if err != nil {
				return nil, err
			}

			for repeat := 0; repeat < repeatCount; repeat++ {
				repeatIndexCopy := repeat
				item, err := expandTemplate(list.items[i], ctx, &repeatIndexCopy)
				if err != nil {
					return nil, err
				}
				items = append(items, item)
			}

			i++
			continue
		}

		item, err := expandTemplate(list.items[i], ctx, repeatIndex)
		if err != nil {
			return nil, err
		}
		items = append(items, item)
	}

	return listExpr{items: items, pos: list.pos}, nil
}

func expandIntroducedLet(list listExpr, ctx templateContext, repeatIndex *int) (expr, bool, error) {
	if len(list.items) < 3 || !isPlainTemplateIdentifier(list.items[0], "let", ctx.bindings) {
		return nil, false, nil
	}

	rawBindings, ok := list.items[1].(listExpr)
	if !ok {
		return nil, false, nil
	}

	head, err := expandTemplate(list.items[0], ctx, repeatIndex)
	if err != nil {
		return nil, true, err
	}

	renamed := copyRenameMap(ctx.renamed)
	bindings := make([]expr, 0, len(rawBindings.items))
	for _, rawBinding := range rawBindings.items {
		binding, ok := rawBinding.(listExpr)
		if !ok || len(binding.items) != 2 {
			return nil, true, &EvalError{Message: "macro-generated let bindings must have the form (name value)"}
		}

		name, err := expandBindingIdentifier(binding.items[0], ctx, repeatIndex, renamed)
		if err != nil {
			return nil, true, err
		}

		value, err := expandTemplate(binding.items[1], ctx, repeatIndex)
		if err != nil {
			return nil, true, err
		}

		bindings = append(bindings, listExpr{
			items: []expr{name, value},
			pos:   binding.pos,
		})
	}

	bodyCtx := ctx
	bodyCtx.renamed = renamed

	items := make([]expr, 0, len(list.items))
	items = append(items, head)
	items = append(items, listExpr{items: bindings, pos: rawBindings.pos})
	for _, rawBody := range list.items[2:] {
		body, err := expandTemplate(rawBody, bodyCtx, repeatIndex)
		if err != nil {
			return nil, true, err
		}
		items = append(items, body)
	}

	return listExpr{items: items, pos: list.pos}, true, nil
}

func expandBindingIdentifier(binding expr, ctx templateContext, repeatIndex *int, renamed map[string]string) (expr, error) {
	symbol, ok := binding.(symbolExpr)
	if !ok {
		return expandTemplate(binding, ctx, repeatIndex)
	}

	if value, ok, err := ctx.bindings.lookup(symbol.name, repeatIndex); err != nil {
		return nil, err
	} else if ok {
		return cloneSyntax(value), nil
	}

	fresh := freshMacroName(symbol.name)
	renamed[symbol.name] = fresh
	return symbolExpr{name: fresh, pos: symbol.pos}, nil
}

func templateRepeatCount(template expr, bindings *syntaxBindings) (int, error) {
	names := map[string]struct{}{}
	collectRepeatedTemplateVars(template, bindings, names)
	if len(names) == 0 {
		return 0, &EvalError{Message: "template ellipsis must reference a repeated pattern variable"}
	}

	count := -1
	for name := range names {
		repeatedValues := bindings.repeated[name]
		if count == -1 {
			count = len(repeatedValues)
			continue
		}
		if count != len(repeatedValues) {
			return 0, &EvalError{Message: "template ellipsis has mismatched repetition counts"}
		}
	}

	if count < 0 {
		count = 0
	}
	return count, nil
}

func collectRepeatedTemplateVars(template expr, bindings *syntaxBindings, out map[string]struct{}) {
	switch value := template.(type) {
	case symbolExpr:
		if _, ok := bindings.repeated[value.name]; ok {
			out[value.name] = struct{}{}
		}
	case listExpr:
		for _, item := range value.items {
			if isEllipsisExpr(item) {
				continue
			}
			collectRepeatedTemplateVars(item, bindings, out)
		}
	}
}

func isPlainTemplateIdentifier(form expr, name string, bindings *syntaxBindings) bool {
	symbol, ok := form.(symbolExpr)
	if !ok || symbol.name != name {
		return false
	}
	return !bindings.hasPatternVar(name)
}

func isEllipsisExpr(form expr) bool {
	symbol, ok := form.(symbolExpr)
	return ok && symbol.name == "..."
}

func copyRenameMap(src map[string]string) map[string]string {
	dst := make(map[string]string, len(src))
	for key, value := range src {
		dst[key] = value
	}
	return dst
}

func freshMacroName(base string) string {
	macroExpansionCounter++
	return fmt.Sprintf("__ming_macro_%s_%d", base, macroExpansionCounter)
}

func cloneSyntax(value expr) expr {
	switch form := value.(type) {
	case symbolExpr:
		return symbolExpr{
			name:      form.name,
			pos:       form.pos,
			lookupEnv: form.lookupEnv,
		}
	case listExpr:
		items := make([]expr, len(form.items))
		for i, item := range form.items {
			items[i] = cloneSyntax(item)
		}
		return listExpr{
			items: items,
			pos:   form.pos,
		}
	case *pairExpr:
		return &pairExpr{
			car: cloneSyntax(form.car),
			cdr: cloneSyntax(form.cdr),
		}
	case *stringExpr:
		return form.copy(form.mutable)
	default:
		return value
	}
}
