package ming

import (
	"strconv"
)

type binding struct {
	value value
}

type macroBinding struct {
	name          string
	transformer   *syntaxRulesMacro
	proc          procedure
	definitionEnv *env
}

type syntaxRulesMacro struct {
	name     string
	literals map[string]struct{}
	rules    []syntaxRule
	env      *env
}

type syntaxRule struct {
	pattern  listExpr
	template locatedExpr
}

type macroMatch struct {
	single   map[string]locatedExpr
	repeated map[string][]locatedExpr
}

type templateSymbolExpr string

type hygieneScope struct {
	parent  *hygieneScope
	renamed map[string]string
}

type hygieneState struct {
	definitionEnv *env
	expansionEnv  *env
	varAliases    map[string]string
	macroAliases  map[string]string
}

var macroGensymCounter int

func evalDefineSyntax(parts []locatedExpr, env *env) (value, error) {
	if len(parts) != 2 {
		return nil, newCurrentEvalError("'define-syntax' expects exactly 2 arguments")
	}

	name, ok := parts[0].form.(symbolExpr)
	if !ok {
		return nil, newEvalError(parts[0].pos, "'define-syntax' name must be a symbol")
	}

	if isSyntaxRulesExpr(parts[1]) {
		transformer, err := parseSyntaxRules(parts[1], string(name), env)
		if err != nil {
			return nil, err
		}

		env.defineMacroBinding(string(name), &macroBinding{
			name:          string(name),
			transformer:   transformer,
			definitionEnv: env,
		})
		return voidValue{}, nil
	}

	transformerValue, err := evalExpr(parts[1], env)
	if err != nil {
		return nil, err
	}

	proc, ok := transformerValue.(procedure)
	if !ok {
		return nil, newEvalError(parts[1].pos, "'define-syntax' transformer must be a procedure")
	}

	env.defineMacroBinding(string(name), &macroBinding{
		name:          string(name),
		proc:          proc,
		definitionEnv: env,
	})
	return voidValue{}, nil
}

func isSyntaxRulesExpr(expr locatedExpr) bool {
	items, ok := expr.form.(listExpr)
	if !ok || len(items) == 0 {
		return false
	}

	head, ok := items[0].form.(symbolExpr)
	return ok && string(head) == "syntax-rules"
}

func parseSyntaxRules(expr locatedExpr, name string, env *env) (*syntaxRulesMacro, error) {
	items, ok := expr.form.(listExpr)
	if !ok || len(items) < 3 {
		return nil, newEvalError(expr.pos, "'syntax-rules' expects a literal list and at least 1 rule")
	}

	head, ok := items[0].form.(symbolExpr)
	if !ok || string(head) != "syntax-rules" {
		return nil, newEvalError(items[0].pos, "expected 'syntax-rules'")
	}

	literalExprs, ok := items[1].form.(listExpr)
	if !ok {
		return nil, newEvalError(items[1].pos, "'syntax-rules' literals must be a list")
	}

	literals := make(map[string]struct{}, len(literalExprs))
	for _, literalExpr := range literalExprs {
		literal, ok := literalExpr.form.(symbolExpr)
		if !ok || string(literal) == "..." {
			return nil, newEvalError(literalExpr.pos, "'syntax-rules' literals must be symbols")
		}
		literals[string(literal)] = struct{}{}
	}

	rules := make([]syntaxRule, 0, len(items)-2)
	for _, ruleExpr := range items[2:] {
		ruleItems, ok := ruleExpr.form.(listExpr)
		if !ok || len(ruleItems) != 2 {
			return nil, newEvalError(ruleExpr.pos, "'syntax-rules' rules must be (pattern template) pairs")
		}

		pattern, ok := ruleItems[0].form.(listExpr)
		if !ok || len(pattern) == 0 {
			return nil, newEvalError(ruleItems[0].pos, "'syntax-rules' patterns must be non-empty lists")
		}

		rules = append(rules, syntaxRule{
			pattern:  pattern,
			template: ruleItems[1],
		})
	}

	return &syntaxRulesMacro{
		name:     name,
		literals: literals,
		rules:    rules,
		env:      env,
	}, nil
}

func expandMacroCall(call listExpr, macro *macroBinding, callEnv *env) (locatedExpr, *env, error) {
	if macro.transformer != nil {
		transformer := macro.transformer
		for _, rule := range transformer.rules {
			match, ok, err := matchMacroPattern(rule.pattern, call, transformer.literals)
			if err != nil {
				return locatedExpr{}, nil, err
			}
			if !ok {
				continue
			}

			instantiated, err := instantiateTemplate(rule.template, match, nil)
			if err != nil {
				return locatedExpr{}, nil, err
			}

			expansionEnv := callEnv
			hygienic, err := hygienizeExpansion(instantiated, transformer.env, expansionEnv, nil)
			if err != nil {
				return locatedExpr{}, nil, err
			}
			return hygienic, expansionEnv, nil
		}

		return locatedExpr{}, nil, newEvalError(call[0].pos, "no matching syntax-rules clause for %s", transformer.name)
	}

	if macro.proc != nil {
		return expandProcedureMacroCall(call, macro, callEnv)
	}

	return locatedExpr{}, nil, newEvalError(call[0].pos, "invalid macro transformer for %s", macro.name)
}

func expandProcedureMacroCall(call listExpr, macro *macroBinding, callEnv *env) (locatedExpr, *env, error) {
	if len(call) == 0 {
		return locatedExpr{}, nil, newCurrentEvalError("cannot expand empty macro call")
	}

	result, err := applyProcedureWithContinuation(
		macro.proc,
		[]value{syntaxValueFromExpr(locatedExpr{form: call, pos: call[0].pos})},
		call[0].pos,
		nil,
	)
	if err != nil {
		return locatedExpr{}, nil, err
	}

	syntaxResult, ok := result.(syntaxValue)
	if !ok {
		return locatedExpr{}, nil, newEvalError(call[0].pos, "macro transformer %s must return syntax", macro.name)
	}

	expansionEnv := callEnv
	hygienic, err := hygienizeExpansion(syntaxResult.expr, macro.definitionEnv, expansionEnv, nil)
	if err != nil {
		return locatedExpr{}, nil, err
	}

	return hygienic, expansionEnv, nil
}

func matchMacroPattern(pattern listExpr, call listExpr, literals map[string]struct{}) (*macroMatch, bool, error) {
	match := &macroMatch{
		single:   make(map[string]locatedExpr),
		repeated: make(map[string][]locatedExpr),
	}

	ok, err := matchPatternList(pattern[1:], call[1:], literals, match, false)
	if err != nil || !ok {
		return nil, ok, err
	}
	return match, true, nil
}

func matchPatternList(patterns []locatedExpr, inputs []locatedExpr, literals map[string]struct{}, match *macroMatch, repeated bool) (bool, error) {
	if len(patterns) == 0 {
		return len(inputs) == 0, nil
	}

	if len(patterns) >= 2 && isEllipsisExpr(patterns[1]) {
		unit := patterns[0]
		rest := patterns[2:]
		for count := 0; count <= len(inputs); count++ {
			candidate := match.clone()
			ok := true
			for i := 0; i < count; i++ {
				ok, err := matchPatternExpr(unit, inputs[i], literals, candidate, true)
				if err != nil {
					return false, err
				}
				if !ok {
					break
				}
			}
			if !ok {
				continue
			}
			candidate.ensureRepeatedBindings(unit, literals)
			ok, err := matchPatternList(rest, inputs[count:], literals, candidate, repeated)
			if err != nil {
				return false, err
			}
			if ok {
				*match = *candidate
				return true, nil
			}
		}
		return false, nil
	}

	if len(inputs) == 0 {
		return false, nil
	}

	ok, err := matchPatternExpr(patterns[0], inputs[0], literals, match, repeated)
	if err != nil || !ok {
		return ok, err
	}
	return matchPatternList(patterns[1:], inputs[1:], literals, match, repeated)
}

func matchPatternExpr(pattern locatedExpr, input locatedExpr, literals map[string]struct{}, match *macroMatch, repeated bool) (bool, error) {
	switch pat := pattern.form.(type) {
	case symbolExpr:
		name := string(pat)
		if name == "..." {
			return false, newEvalError(pattern.pos, "invalid use of ellipsis in syntax-rules pattern")
		}
		if name == "_" {
			return true, nil
		}
		if _, isLiteral := literals[name]; isLiteral {
			inputName, ok, _ := symbolLikeName(input.form)
			return ok && inputName == name, nil
		}
		return match.bind(name, input, repeated), nil
	case listExpr:
		inputList, ok := input.form.(listExpr)
		if !ok {
			return false, nil
		}
		return matchPatternList(pat, inputList, literals, match, repeated)
	case numberExpr:
		value, ok := input.form.(numberExpr)
		return ok && pat == value, nil
	case boolExpr:
		value, ok := input.form.(boolExpr)
		return ok && pat == value, nil
	case stringExpr:
		value, ok := input.form.(stringExpr)
		return ok && pat == value, nil
	case charExpr:
		value, ok := input.form.(charExpr)
		return ok && pat == value, nil
	default:
		return false, newEvalError(pattern.pos, "unsupported syntax-rules pattern")
	}
}

func instantiateTemplate(template locatedExpr, match *macroMatch, repetition []int) (locatedExpr, error) {
	switch expr := template.form.(type) {
	case symbolExpr:
		name := string(expr)
		if name == "..." {
			return locatedExpr{}, newEvalError(template.pos, "invalid use of ellipsis in syntax-rules template")
		}
		if repeatedValues, ok := match.repeated[name]; ok {
			if len(repetition) == 0 {
				return locatedExpr{}, newEvalError(template.pos, "repeated pattern variable used outside ellipsis: %s", name)
			}
			index := repetition[len(repetition)-1]
			if index < 0 || index >= len(repeatedValues) {
				return locatedExpr{}, newEvalError(template.pos, "ellipsis expansion out of range for %s", name)
			}
			return cloneLocatedExpr(repeatedValues[index]), nil
		}
		if singleValue, ok := match.single[name]; ok {
			return cloneLocatedExpr(singleValue), nil
		}
		return locatedExpr{form: templateSymbolExpr(name), pos: template.pos}, nil
	case listExpr:
		items := make([]locatedExpr, 0, len(expr))
		for i := 0; i < len(expr); i++ {
			if i+1 < len(expr) && isEllipsisExpr(expr[i+1]) {
				count, err := templateRepeatCount(expr[i], match)
				if err != nil {
					return locatedExpr{}, err
				}
				for index := 0; index < count; index++ {
					item, err := instantiateTemplate(expr[i], match, append(repetition, index))
					if err != nil {
						return locatedExpr{}, err
					}
					items = append(items, item)
				}
				i++
				continue
			}

			item, err := instantiateTemplate(expr[i], match, repetition)
			if err != nil {
				return locatedExpr{}, err
			}
			items = append(items, item)
		}
		return locatedExpr{form: listExpr(items), pos: template.pos}, nil
	case numberExpr, boolExpr, stringExpr, charExpr:
		return cloneLocatedExpr(template), nil
	default:
		return locatedExpr{}, newEvalError(template.pos, "unsupported syntax-rules template")
	}
}

func templateRepeatCount(template locatedExpr, match *macroMatch) (int, error) {
	names := repeatedTemplateNames(template, nil)
	if len(names) == 0 {
		return 0, newEvalError(template.pos, "ellipsis template must reference a repeated pattern variable")
	}

	count := -1
	for _, name := range names {
		values, ok := match.repeated[name]
		if !ok {
			continue
		}
		if count == -1 {
			count = len(values)
			continue
		}
		if len(values) != count {
			return 0, newEvalError(template.pos, "inconsistent ellipsis lengths in syntax-rules template")
		}
	}

	if count < 0 {
		return 0, newEvalError(template.pos, "ellipsis template must reference a repeated pattern variable")
	}
	return count, nil
}

func repeatedPatternNames(pattern locatedExpr, literals map[string]struct{}, names []string) []string {
	switch expr := pattern.form.(type) {
	case symbolExpr:
		name := string(expr)
		if name == "..." || name == "_" {
			return names
		}
		if _, isLiteral := literals[name]; isLiteral {
			return names
		}
		return append(names, name)
	case listExpr:
		for _, item := range expr {
			names = repeatedPatternNames(item, literals, names)
		}
	}
	return names
}

func repeatedTemplateNames(template locatedExpr, names []string) []string {
	switch expr := template.form.(type) {
	case symbolExpr:
		return append(names, string(expr))
	case listExpr:
		for i := 0; i < len(expr); i++ {
			if isEllipsisExpr(expr[i]) {
				continue
			}
			names = repeatedTemplateNames(expr[i], names)
		}
	}
	return names
}

func hygienizeExpansion(expr locatedExpr, definitionEnv *env, expansionEnv *env, scope *hygieneScope) (locatedExpr, error) {
	state := &hygieneState{
		definitionEnv: definitionEnv,
		expansionEnv:  expansionEnv,
		varAliases:    make(map[string]string),
		macroAliases:  make(map[string]string),
	}
	return state.hygienizeExpr(expr, scope)
}

func (state *hygieneState) hygienizeExpr(expr locatedExpr, scope *hygieneScope) (locatedExpr, error) {
	switch form := expr.form.(type) {
	case templateSymbolExpr:
		return locatedExpr{form: symbolExpr(state.referenceName(string(form), scope)), pos: expr.pos}, nil
	case listExpr:
		return state.hygienizeList(form, expr.pos, scope)
	default:
		return cloneLocatedExpr(expr), nil
	}
}

func (state *hygieneState) hygienizeList(items listExpr, pos SourcePos, scope *hygieneScope) (locatedExpr, error) {
	if len(items) == 0 {
		return locatedExpr{form: listExpr{}, pos: pos}, nil
	}

	operatorName, operatorIsSymbol, operatorIntroduced := symbolLikeName(items[0].form)
	if operatorIsSymbol {
		switch operatorName {
		case "quote":
			return state.hygienizeQuote(items, pos)
		case "lambda":
			return state.hygienizeLambda(items, pos, scope)
		case "case-lambda":
			return state.hygienizeCaseLambda(items, pos, scope)
		case "let":
			return state.hygienizeLet(items, pos, scope)
		case "define":
			return state.hygienizeDefine(items, pos, scope)
		case "set!":
			return state.hygienizeSet(items, pos, scope)
		case "if", "begin", "and", "or", "cond", "define-syntax", "syntax", "syntax-case", "with-syntax":
			newItems := make([]locatedExpr, 0, len(items))
			newItems = append(newItems, locatedExpr{form: symbolExpr(operatorName), pos: items[0].pos})
			for _, item := range items[1:] {
				expanded, err := state.hygienizeExpr(item, scope)
				if err != nil {
					return locatedExpr{}, err
				}
				newItems = append(newItems, expanded)
			}
			return locatedExpr{form: listExpr(newItems), pos: pos}, nil
		}
	}

	newItems := make([]locatedExpr, 0, len(items))
	if operatorIntroduced {
		newItems = append(newItems, locatedExpr{form: symbolExpr(state.operatorName(operatorName, scope)), pos: items[0].pos})
	} else {
		operator, err := state.hygienizeExpr(items[0], scope)
		if err != nil {
			return locatedExpr{}, err
		}
		newItems = append(newItems, operator)
	}

	for _, item := range items[1:] {
		expanded, err := state.hygienizeExpr(item, scope)
		if err != nil {
			return locatedExpr{}, err
		}
		newItems = append(newItems, expanded)
	}

	return locatedExpr{form: listExpr(newItems), pos: pos}, nil
}

func (state *hygieneState) hygienizeQuote(items listExpr, pos SourcePos) (locatedExpr, error) {
	newItems := make([]locatedExpr, 0, len(items))
	newItems = append(newItems, locatedExpr{form: symbolExpr("quote"), pos: items[0].pos})
	for _, item := range items[1:] {
		newItems = append(newItems, hygienizeDatum(item))
	}
	return locatedExpr{form: listExpr(newItems), pos: pos}, nil
}

func (state *hygieneState) hygienizeLambda(items listExpr, pos SourcePos, scope *hygieneScope) (locatedExpr, error) {
	if len(items) < 3 {
		return locatedExpr{}, newEvalError(pos, "'lambda' expects a parameter list and body")
	}

	formals, bodyScope, err := state.hygienizeFormals(items[1], scope)
	if err != nil {
		return locatedExpr{}, err
	}

	newItems := []locatedExpr{
		{form: symbolExpr("lambda"), pos: items[0].pos},
		formals,
	}
	for _, bodyExpr := range items[2:] {
		expanded, err := state.hygienizeExpr(bodyExpr, bodyScope)
		if err != nil {
			return locatedExpr{}, err
		}
		newItems = append(newItems, expanded)
	}
	return locatedExpr{form: listExpr(newItems), pos: pos}, nil
}

func (state *hygieneState) hygienizeCaseLambda(items listExpr, pos SourcePos, scope *hygieneScope) (locatedExpr, error) {
	if len(items) < 2 {
		return locatedExpr{}, newEvalError(pos, "'case-lambda' expects at least 1 clause")
	}

	newItems := []locatedExpr{
		{form: symbolExpr("case-lambda"), pos: items[0].pos},
	}
	for _, clauseExpr := range items[1:] {
		clause, ok := clauseExpr.form.(listExpr)
		if !ok || len(clause) < 2 {
			return locatedExpr{}, newEvalError(clauseExpr.pos, "'case-lambda' clauses must have a parameter list and body")
		}

		formals, bodyScope, err := state.hygienizeFormals(clause[0], scope)
		if err != nil {
			return locatedExpr{}, err
		}

		newClause := []locatedExpr{formals}
		for _, bodyExpr := range clause[1:] {
			expanded, err := state.hygienizeExpr(bodyExpr, bodyScope)
			if err != nil {
				return locatedExpr{}, err
			}
			newClause = append(newClause, expanded)
		}

		newItems = append(newItems, locatedExpr{
			form: listExpr(newClause),
			pos:  clauseExpr.pos,
		})
	}

	return locatedExpr{form: listExpr(newItems), pos: pos}, nil
}

func (state *hygieneState) hygienizeLet(items listExpr, pos SourcePos, scope *hygieneScope) (locatedExpr, error) {
	if len(items) < 3 {
		return locatedExpr{}, newEvalError(pos, "'let' expects bindings and a body")
	}

	newItems := []locatedExpr{
		{form: symbolExpr("let"), pos: items[0].pos},
	}

	bindingsIndex := 1
	bodyStart := 2
	bodyScope := scope

	if name, ok := items[1].form.(templateSymbolExpr); ok {
		fresh := nextMacroName(string(name))
		bodyScope = extendScope(scope, string(name), fresh)
		newItems = append(newItems, locatedExpr{form: symbolExpr(fresh), pos: items[1].pos})
		bindingsIndex = 2
		bodyStart = 3
	} else if _, ok := items[1].form.(symbolExpr); ok {
		newItems = append(newItems, cloneLocatedExpr(items[1]))
		bindingsIndex = 2
		bodyStart = 3
	}

	if len(items) <= bindingsIndex {
		return locatedExpr{}, newEvalError(pos, "'let' expects bindings and a body")
	}

	bindings, bindingScope, err := state.hygienizeLetBindings(items[bindingsIndex], bodyScope)
	if err != nil {
		return locatedExpr{}, err
	}
	newItems = append(newItems, bindings)

	for _, bodyExpr := range items[bodyStart:] {
		expanded, err := state.hygienizeExpr(bodyExpr, bindingScope)
		if err != nil {
			return locatedExpr{}, err
		}
		newItems = append(newItems, expanded)
	}

	return locatedExpr{form: listExpr(newItems), pos: pos}, nil
}

func (state *hygieneState) hygienizeLetBindings(bindingsExpr locatedExpr, scope *hygieneScope) (locatedExpr, *hygieneScope, error) {
	bindings, ok := bindingsExpr.form.(listExpr)
	if !ok {
		return locatedExpr{}, nil, newEvalError(bindingsExpr.pos, "'let' bindings must be a list")
	}

	childScope := extendScope(scope, "", "")
	newBindings := make([]locatedExpr, 0, len(bindings))
	for _, bindingExpr := range bindings {
		bindingItems, ok := bindingExpr.form.(listExpr)
		if !ok || len(bindingItems) != 2 {
			return locatedExpr{}, nil, newEvalError(bindingExpr.pos, "'let' bindings must be (name value) pairs")
		}

		nameExpr, updatedScope, err := state.hygienizeBindingExpr(bindingItems[0], childScope)
		if err != nil {
			return locatedExpr{}, nil, err
		}
		childScope = updatedScope

		valueExpr, err := state.hygienizeExpr(bindingItems[1], scope)
		if err != nil {
			return locatedExpr{}, nil, err
		}

		newBindings = append(newBindings, locatedExpr{
			form: listExpr{nameExpr, valueExpr},
			pos:  bindingExpr.pos,
		})
	}

	return locatedExpr{form: listExpr(newBindings), pos: bindingsExpr.pos}, childScope, nil
}

func (state *hygieneState) hygienizeDefine(items listExpr, pos SourcePos, scope *hygieneScope) (locatedExpr, error) {
	if len(items) < 3 {
		return locatedExpr{}, newEvalError(pos, "'define' expects at least 2 arguments")
	}

	newItems := []locatedExpr{
		{form: symbolExpr("define"), pos: items[0].pos},
	}

	switch target := items[1].form.(type) {
	case templateSymbolExpr, symbolExpr:
		nameExpr, _, err := state.hygienizeBindingExpr(items[1], scope)
		if err != nil {
			return locatedExpr{}, err
		}
		valueExpr, err := state.hygienizeExpr(items[2], scope)
		if err != nil {
			return locatedExpr{}, err
		}
		newItems = append(newItems, nameExpr, valueExpr)
		return locatedExpr{form: listExpr(newItems), pos: pos}, nil
	case listExpr:
		if len(target) == 0 {
			return locatedExpr{}, newEvalError(items[1].pos, "function name is required")
		}

		functionName, bodyScope, err := state.hygienizeBindingExpr(target[0], scope)
		if err != nil {
			return locatedExpr{}, err
		}

		formals, formalsScope, err := state.hygienizeFormals(locatedExpr{
			form: listExpr(target[1:]),
			pos:  items[1].pos,
		}, bodyScope)
		if err != nil {
			return locatedExpr{}, err
		}

		targetItems := append([]locatedExpr{functionName}, formals.form.(listExpr)...)
		newItems = append(newItems, locatedExpr{
			form: listExpr(targetItems),
			pos:  items[1].pos,
		})

		for _, bodyExpr := range items[2:] {
			expanded, err := state.hygienizeExpr(bodyExpr, formalsScope)
			if err != nil {
				return locatedExpr{}, err
			}
			newItems = append(newItems, expanded)
		}
		return locatedExpr{form: listExpr(newItems), pos: pos}, nil
	default:
		return locatedExpr{}, newEvalError(items[1].pos, "invalid define target")
	}
}

func (state *hygieneState) hygienizeSet(items listExpr, pos SourcePos, scope *hygieneScope) (locatedExpr, error) {
	if len(items) != 3 {
		return locatedExpr{}, newEvalError(pos, "'set!' expects exactly 2 arguments")
	}

	target, err := state.hygienizeExpr(items[1], scope)
	if err != nil {
		return locatedExpr{}, err
	}
	valueExpr, err := state.hygienizeExpr(items[2], scope)
	if err != nil {
		return locatedExpr{}, err
	}

	return locatedExpr{
		form: listExpr{
			{form: symbolExpr("set!"), pos: items[0].pos},
			target,
			valueExpr,
		},
		pos: pos,
	}, nil
}

func (state *hygieneState) hygienizeFormals(expr locatedExpr, scope *hygieneScope) (locatedExpr, *hygieneScope, error) {
	switch formals := expr.form.(type) {
	case templateSymbolExpr:
		if string(formals) == "." {
			return locatedExpr{}, nil, newEvalError(expr.pos, "parameter name must be a symbol")
		}
		formal, childScope, err := state.hygienizeBindingExpr(expr, scope)
		if err != nil {
			return locatedExpr{}, nil, err
		}
		return formal, childScope, nil
	case symbolExpr:
		if string(formals) == "." {
			return locatedExpr{}, nil, newEvalError(expr.pos, "parameter name must be a symbol")
		}
		formal, childScope, err := state.hygienizeBindingExpr(expr, scope)
		if err != nil {
			return locatedExpr{}, nil, err
		}
		return formal, childScope, nil
	case listExpr:
		childScope := extendScope(scope, "", "")
		newItems := make([]locatedExpr, 0, len(formals))
		for _, item := range formals {
			switch name := item.form.(type) {
			case symbolExpr:
				if string(name) == "." {
					newItems = append(newItems, cloneLocatedExpr(item))
					continue
				}
			case templateSymbolExpr:
				if string(name) == "." {
					newItems = append(newItems, locatedExpr{form: symbolExpr("."), pos: item.pos})
					continue
				}
			}

			formal, updatedScope, err := state.hygienizeBindingExpr(item, childScope)
			if err != nil {
				return locatedExpr{}, nil, err
			}
			childScope = updatedScope
			newItems = append(newItems, formal)
		}
		return locatedExpr{form: listExpr(newItems), pos: expr.pos}, childScope, nil
	default:
		return locatedExpr{}, nil, newEvalError(expr.pos, "'lambda' parameter list must be a list or symbol")
	}
}

func (state *hygieneState) hygienizeBindingExpr(expr locatedExpr, scope *hygieneScope) (locatedExpr, *hygieneScope, error) {
	switch name := expr.form.(type) {
	case templateSymbolExpr:
		fresh := nextMacroName(string(name))
		return locatedExpr{form: symbolExpr(fresh), pos: expr.pos}, extendScope(scope, string(name), fresh), nil
	case symbolExpr:
		if string(name) == "." {
			return locatedExpr{}, nil, newEvalError(expr.pos, "parameter name must be a symbol")
		}
		return cloneLocatedExpr(expr), scope, nil
	default:
		return locatedExpr{}, nil, newEvalError(expr.pos, "expected identifier")
	}
}

func (state *hygieneState) referenceName(name string, scope *hygieneScope) string {
	if fresh, ok := scopeLookup(scope, name); ok {
		return fresh
	}
	if alias, ok := state.variableAlias(name); ok {
		return alias
	}
	return name
}

func (state *hygieneState) operatorName(name string, scope *hygieneScope) string {
	if fresh, ok := scopeLookup(scope, name); ok {
		return fresh
	}
	if isSyntaxKeyword(name) {
		return name
	}
	if alias, ok := state.macroAlias(name); ok {
		return alias
	}
	if alias, ok := state.variableAlias(name); ok {
		return alias
	}
	return name
}

func (state *hygieneState) variableAlias(name string) (string, bool) {
	if alias, ok := state.varAliases[name]; ok {
		return alias, true
	}
	binding, ok := state.definitionEnv.lookupBinding(name)
	if !ok {
		return "", false
	}
	alias := nextMacroName(name)
	state.expansionEnv.defineBinding(alias, binding)
	state.varAliases[name] = alias
	return alias, true
}

func (state *hygieneState) macroAlias(name string) (string, bool) {
	if alias, ok := state.macroAliases[name]; ok {
		return alias, true
	}
	macro, ok := state.definitionEnv.lookupMacro(name)
	if !ok {
		return "", false
	}
	alias := nextMacroName(name)
	state.expansionEnv.defineMacroBinding(alias, macro)
	state.macroAliases[name] = alias
	return alias, true
}

func (match *macroMatch) bind(name string, expr locatedExpr, repeated bool) bool {
	if repeated {
		match.repeated[name] = append(match.repeated[name], cloneLocatedExpr(expr))
		return true
	}

	if existing, ok := match.single[name]; ok {
		return exprSyntaxEqual(existing, expr)
	}

	match.single[name] = cloneLocatedExpr(expr)
	return true
}

func (match *macroMatch) clone() *macroMatch {
	copyMatch := &macroMatch{
		single:   make(map[string]locatedExpr, len(match.single)),
		repeated: make(map[string][]locatedExpr, len(match.repeated)),
	}

	for name, expr := range match.single {
		copyMatch.single[name] = cloneLocatedExpr(expr)
	}
	for name, exprs := range match.repeated {
		copied := make([]locatedExpr, len(exprs))
		for i, expr := range exprs {
			copied[i] = cloneLocatedExpr(expr)
		}
		copyMatch.repeated[name] = copied
	}
	return copyMatch
}

func (match *macroMatch) ensureRepeatedBindings(pattern locatedExpr, literals map[string]struct{}) {
	for _, name := range repeatedPatternNames(pattern, literals, nil) {
		if _, ok := match.repeated[name]; !ok {
			match.repeated[name] = []locatedExpr{}
		}
	}
}

func cloneLocatedExpr(expr locatedExpr) locatedExpr {
	switch form := expr.form.(type) {
	case listExpr:
		items := make([]locatedExpr, len(form))
		for i, item := range form {
			items[i] = cloneLocatedExpr(item)
		}
		return locatedExpr{form: listExpr(items), pos: expr.pos}
	default:
		return expr
	}
}

func exprSyntaxEqual(left locatedExpr, right locatedExpr) bool {
	if leftName, leftIsSymbol, _ := symbolLikeName(left.form); leftIsSymbol {
		rightName, rightIsSymbol, _ := symbolLikeName(right.form)
		return rightIsSymbol && leftName == rightName
	}

	switch leftExpr := left.form.(type) {
	case listExpr:
		rightExpr, ok := right.form.(listExpr)
		if !ok || len(leftExpr) != len(rightExpr) {
			return false
		}
		for i := range leftExpr {
			if !exprSyntaxEqual(leftExpr[i], rightExpr[i]) {
				return false
			}
		}
		return true
	case numberExpr:
		rightExpr, ok := right.form.(numberExpr)
		return ok && leftExpr == rightExpr
	case boolExpr:
		rightExpr, ok := right.form.(boolExpr)
		return ok && leftExpr == rightExpr
	case stringExpr:
		rightExpr, ok := right.form.(stringExpr)
		return ok && leftExpr == rightExpr
	case charExpr:
		rightExpr, ok := right.form.(charExpr)
		return ok && leftExpr == rightExpr
	default:
		return false
	}
}

func hygienizeDatum(expr locatedExpr) locatedExpr {
	switch form := expr.form.(type) {
	case templateSymbolExpr:
		return locatedExpr{form: symbolExpr(form), pos: expr.pos}
	case listExpr:
		items := make([]locatedExpr, len(form))
		for i, item := range form {
			items[i] = hygienizeDatum(item)
		}
		return locatedExpr{form: listExpr(items), pos: expr.pos}
	default:
		return cloneLocatedExpr(expr)
	}
}

func symbolLikeName(form expr) (string, bool, bool) {
	switch name := form.(type) {
	case symbolExpr:
		return string(name), true, false
	case templateSymbolExpr:
		return string(name), true, true
	default:
		return "", false, false
	}
}

func extendScope(parent *hygieneScope, name string, fresh string) *hygieneScope {
	scope := &hygieneScope{
		parent:  parent,
		renamed: make(map[string]string),
	}
	if name != "" {
		scope.renamed[name] = fresh
	}
	return scope
}

func scopeLookup(scope *hygieneScope, name string) (string, bool) {
	for current := scope; current != nil; current = current.parent {
		if fresh, ok := current.renamed[name]; ok {
			return fresh, true
		}
	}
	return "", false
}

func isEllipsisExpr(expr locatedExpr) bool {
	name, ok := expr.form.(symbolExpr)
	return ok && string(name) == "..."
}

func isSyntaxKeyword(name string) bool {
	switch name {
	case "and", "begin", "case-lambda", "cond", "define", "define-syntax", "if", "lambda", "let", "or", "quote", "set!", "syntax", "syntax-case", "with-syntax":
		return true
	default:
		return false
	}
}

func nextMacroName(prefix string) string {
	macroGensymCounter++
	return "__macro_" + sanitizeMacroName(prefix) + "_" + strconv.Itoa(macroGensymCounter)
}

func sanitizeMacroName(name string) string {
	if name == "" {
		return "tmp"
	}

	out := make([]rune, 0, len(name))
	for _, ch := range name {
		switch {
		case ch >= 'a' && ch <= 'z':
			out = append(out, ch)
		case ch >= 'A' && ch <= 'Z':
			out = append(out, ch)
		case ch >= '0' && ch <= '9':
			out = append(out, ch)
		default:
			out = append(out, '_')
		}
	}
	return string(out)
}
