package ming

import "fmt"

type syntaxExpr struct {
	datum expr
}

type syntaxListExpr struct {
	values []expr
}

func evalSyntax(environment *env, forms []expr) (expr, error) {
	if len(forms) != 1 {
		return nil, &EvalError{Message: "syntax expects exactly 1 argument"}
	}

	datum, err := expandSyntaxTemplate(forms[0], environment, environment, nil, map[string]string{})
	if err != nil {
		return nil, err
	}

	return &syntaxExpr{datum: datum}, nil
}

func evalWithSyntax(environment *env, forms []expr) (evalStep, error) {
	if len(forms) < 2 {
		return evalStep{}, &EvalError{Message: "with-syntax expects bindings and a body"}
	}

	bindings, ok := forms[0].(listExpr)
	if !ok {
		return evalStep{}, &EvalError{Message: "with-syntax bindings must be a list"}
	}

	withSyntaxEnv := &env{
		parent:   environment,
		bindings: map[string]expr{},
	}

	for _, binding := range bindings.items {
		pair, ok := binding.(listExpr)
		if !ok || len(pair.items) != 2 {
			return evalStep{}, &EvalError{Message: "with-syntax bindings must have the form (name value)"}
		}

		name, ok := pair.items[0].(symbolExpr)
		if !ok {
			return evalStep{}, &EvalError{Message: "with-syntax binding name must be a symbol"}
		}

		value, err := evalSingleExpr(environment, pair.items[1], "with-syntax")
		if err != nil {
			return evalStep{}, err
		}

		switch value.(type) {
		case *syntaxExpr, *syntaxListExpr:
			withSyntaxEnv.define(name.name, value)
		default:
			return evalStep{}, errorAt(name.pos, "with-syntax values must be syntax objects")
		}
	}

	return evalSequenceTail(withSyntaxEnv, forms[1:])
}

func evalSyntaxCase(environment *env, forms []expr) (expr, error) {
	if len(forms) < 3 {
		return nil, &EvalError{Message: "syntax-case expects a value, literals, and at least one clause"}
	}

	targetValue, err := evalSingleExpr(environment, forms[0], "syntax-case")
	if err != nil {
		return nil, err
	}

	target, ok := targetValue.(*syntaxExpr)
	if !ok {
		return nil, &EvalError{Message: "syntax-case expects a syntax object"}
	}

	literalList, ok := forms[1].(listExpr)
	if !ok {
		return nil, &EvalError{Message: "syntax-case literals must be a list"}
	}

	literals := make(map[string]struct{}, len(literalList.items))
	for _, literal := range literalList.items {
		symbol, ok := literal.(symbolExpr)
		if !ok {
			return nil, &EvalError{Message: "syntax-case literals must be symbols"}
		}
		literals[symbol.name] = struct{}{}
	}

	for _, rawClause := range forms[2:] {
		clause, ok := rawClause.(listExpr)
		if !ok || (len(clause.items) != 2 && len(clause.items) != 3) {
			return nil, &EvalError{Message: "syntax-case clauses must have the form (pattern template) or (pattern guard template)"}
		}

		bindings := newSyntaxBindings()
		matched := matchPattern(clause.items[0], target.datum, literals, bindings)
		if !matched {
			continue
		}

		clauseEnv := &env{
			parent:   environment,
			bindings: map[string]expr{},
		}

		for name, value := range bindings.single {
			clauseEnv.define(name, &syntaxExpr{datum: cloneSyntax(value)})
		}
		for name, values := range bindings.repeated {
			cloned := make([]expr, len(values))
			for i, value := range values {
				cloned[i] = cloneSyntax(value)
			}
			clauseEnv.define(name, &syntaxListExpr{values: cloned})
		}

		if len(clause.items) == 3 {
			guard, err := evalSingleExpr(clauseEnv, clause.items[1], "syntax-case")
			if err != nil {
				return nil, err
			}
			if !isTruthy(guard) {
				continue
			}
			return evalExpr(clauseEnv, clause.items[2])
		}

		return evalExpr(clauseEnv, clause.items[1])
	}

	return nil, &EvalError{Message: "syntax-case found no matching clause"}
}

func builtinSyntaxToDatum(args []expr) (expr, error) {
	if len(args) != 1 {
		return nil, &EvalError{Message: "syntax->datum expects exactly 1 argument"}
	}

	syntax, ok := args[0].(*syntaxExpr)
	if !ok {
		return nil, &EvalError{Message: "syntax->datum expects a syntax object"}
	}

	return quoteDatum(syntax.datum), nil
}

func builtinDatumToSyntax(args []expr) (expr, error) {
	if len(args) != 2 {
		return nil, &EvalError{Message: "datum->syntax expects exactly 2 arguments"}
	}

	if _, ok := args[0].(*syntaxExpr); !ok {
		return nil, &EvalError{Message: "datum->syntax expects a syntax object as its first argument"}
	}

	datum, err := datumToSyntaxDatum(args[1])
	if err != nil {
		return nil, err
	}

	return &syntaxExpr{datum: datum}, nil
}

func datumToSyntaxDatum(value expr) (expr, error) {
	switch v := value.(type) {
	case intExpr, rationalExpr, inexactExpr, boolExpr, charExpr:
		return v, nil
	case symbolExpr:
		return cloneSyntax(v), nil
	case listExpr:
		items := make([]expr, len(v.items))
		for i, item := range v.items {
			converted, err := datumToSyntaxDatum(item)
			if err != nil {
				return nil, err
			}
			items[i] = converted
		}
		return listExpr{items: items, pos: v.pos}, nil
	case *pairExpr:
		items, ok := listElements(v)
		if !ok {
			return nil, &EvalError{Message: "cannot convert improper list to syntax"}
		}

		converted := make([]expr, len(items))
		for i, item := range items {
			value, err := datumToSyntaxDatum(item)
			if err != nil {
				return nil, err
			}
			converted[i] = value
		}
		return listExpr{items: converted}, nil
	case *stringExpr:
		return v.copy(v.mutable), nil
	case *vectorExpr:
		items := make([]expr, len(v.items))
		for i, item := range v.items {
			converted, err := datumToSyntaxDatum(item)
			if err != nil {
				return nil, err
			}
			items[i] = converted
		}
		return &vectorExpr{items: items}, nil
	case *syntaxExpr:
		return cloneSyntax(v.datum), nil
	default:
		return nil, &EvalError{Message: "datum->syntax expects a datum-compatible value"}
	}
}

func expandSyntaxTemplate(template expr, templateEnv *env, defEnv *env, repeatIndex *int, renamed map[string]string) (expr, error) {
	switch value := template.(type) {
	case symbolExpr:
		return expandSyntaxTemplateSymbol(value, templateEnv, defEnv, repeatIndex, renamed)
	case listExpr:
		if expanded, handled, err := expandIntroducedSyntaxDefine(value, templateEnv, defEnv, repeatIndex, renamed); handled || err != nil {
			return expanded, err
		}
		if expanded, handled, err := expandIntroducedSyntaxLambda(value, templateEnv, defEnv, repeatIndex, renamed); handled || err != nil {
			return expanded, err
		}
		if expanded, handled, err := expandIntroducedSyntaxLet(value, templateEnv, defEnv, repeatIndex, renamed); handled || err != nil {
			return expanded, err
		}
		return expandSyntaxTemplateList(value, templateEnv, defEnv, repeatIndex, renamed)
	default:
		return cloneSyntax(template), nil
	}
}

func expandSyntaxTemplateSymbol(symbol symbolExpr, templateEnv *env, defEnv *env, repeatIndex *int, renamed map[string]string) (expr, error) {
	if value, ok, err := lookupSyntaxTemplateBinding(templateEnv, symbol.name, repeatIndex); err != nil {
		return nil, err
	} else if ok {
		return value, nil
	}

	if renamed != nil {
		if fresh, ok := renamed[symbol.name]; ok {
			return symbolExpr{name: fresh, pos: symbol.pos}, nil
		}
	}

	return symbolExpr{name: symbol.name, pos: symbol.pos, lookupEnv: defEnv}, nil
}

func expandSyntaxTemplateList(list listExpr, templateEnv *env, defEnv *env, repeatIndex *int, renamed map[string]string) (expr, error) {
	items := make([]expr, 0, len(list.items))

	for i := 0; i < len(list.items); i++ {
		if i+1 < len(list.items) && isEllipsisExpr(list.items[i+1]) {
			repeatCount, err := syntaxTemplateRepeatCount(list.items[i], templateEnv)
			if err != nil {
				return nil, err
			}

			for repeat := 0; repeat < repeatCount; repeat++ {
				repeatIndexCopy := repeat
				item, err := expandSyntaxTemplate(list.items[i], templateEnv, defEnv, &repeatIndexCopy, renamed)
				if err != nil {
					return nil, err
				}
				items = append(items, item)
			}

			i++
			continue
		}

		item, err := expandSyntaxTemplate(list.items[i], templateEnv, defEnv, repeatIndex, renamed)
		if err != nil {
			return nil, err
		}
		items = append(items, item)
	}

	return listExpr{items: items, pos: list.pos}, nil
}

func expandIntroducedSyntaxDefine(list listExpr, templateEnv *env, defEnv *env, repeatIndex *int, renamed map[string]string) (expr, bool, error) {
	if len(list.items) < 3 || !isPlainSyntaxTemplateIdentifier(list.items[0], "define", templateEnv) {
		return nil, false, nil
	}

	head, err := expandSyntaxTemplate(list.items[0], templateEnv, defEnv, repeatIndex, renamed)
	if err != nil {
		return nil, true, err
	}

	localRenamed := copyRenameMap(renamed)
	items := make([]expr, 0, len(list.items))
	items = append(items, head)

	switch target := list.items[1].(type) {
	case symbolExpr:
		name, err := expandSyntaxBindingIdentifier(target, templateEnv, defEnv, repeatIndex, localRenamed)
		if err != nil {
			return nil, true, err
		}
		items = append(items, name)
	case listExpr:
		if len(target.items) == 0 {
			return nil, true, &EvalError{Message: "macro-generated define target cannot be empty"}
		}

		signature := make([]expr, 0, len(target.items))
		name, err := expandSyntaxBindingIdentifier(target.items[0], templateEnv, defEnv, repeatIndex, localRenamed)
		if err != nil {
			return nil, true, err
		}
		signature = append(signature, name)

		for _, rawParam := range target.items[1:] {
			param, err := expandSyntaxParamIdentifier(rawParam, templateEnv, defEnv, repeatIndex, localRenamed)
			if err != nil {
				return nil, true, err
			}
			signature = append(signature, param)
		}

		items = append(items, listExpr{items: signature, pos: target.pos})
	default:
		return nil, false, nil
	}

	for _, rawBody := range list.items[2:] {
		body, err := expandSyntaxTemplate(rawBody, templateEnv, defEnv, repeatIndex, localRenamed)
		if err != nil {
			return nil, true, err
		}
		items = append(items, body)
	}

	return listExpr{items: items, pos: list.pos}, true, nil
}

func expandIntroducedSyntaxLambda(list listExpr, templateEnv *env, defEnv *env, repeatIndex *int, renamed map[string]string) (expr, bool, error) {
	if len(list.items) < 3 || !isPlainSyntaxTemplateIdentifier(list.items[0], "lambda", templateEnv) {
		return nil, false, nil
	}

	head, err := expandSyntaxTemplate(list.items[0], templateEnv, defEnv, repeatIndex, renamed)
	if err != nil {
		return nil, true, err
	}

	localRenamed := copyRenameMap(renamed)
	params, err := expandSyntaxParamSpec(list.items[1], templateEnv, defEnv, repeatIndex, localRenamed)
	if err != nil {
		return nil, true, err
	}

	items := make([]expr, 0, len(list.items))
	items = append(items, head, params)
	for _, rawBody := range list.items[2:] {
		body, err := expandSyntaxTemplate(rawBody, templateEnv, defEnv, repeatIndex, localRenamed)
		if err != nil {
			return nil, true, err
		}
		items = append(items, body)
	}

	return listExpr{items: items, pos: list.pos}, true, nil
}

func expandIntroducedSyntaxLet(list listExpr, templateEnv *env, defEnv *env, repeatIndex *int, renamed map[string]string) (expr, bool, error) {
	if len(list.items) < 3 || !isPlainSyntaxTemplateIdentifier(list.items[0], "let", templateEnv) {
		return nil, false, nil
	}

	rawBindings, ok := list.items[1].(listExpr)
	if !ok {
		return nil, false, nil
	}

	head, err := expandSyntaxTemplate(list.items[0], templateEnv, defEnv, repeatIndex, renamed)
	if err != nil {
		return nil, true, err
	}

	localRenamed := copyRenameMap(renamed)
	bindings := make([]expr, 0, len(rawBindings.items))
	for _, rawBinding := range rawBindings.items {
		binding, ok := rawBinding.(listExpr)
		if !ok || len(binding.items) != 2 {
			return nil, true, &EvalError{Message: "macro-generated let bindings must have the form (name value)"}
		}

		name, err := expandSyntaxBindingIdentifier(binding.items[0], templateEnv, defEnv, repeatIndex, localRenamed)
		if err != nil {
			return nil, true, err
		}

		value, err := expandSyntaxTemplate(binding.items[1], templateEnv, defEnv, repeatIndex, localRenamed)
		if err != nil {
			return nil, true, err
		}

		bindings = append(bindings, listExpr{
			items: []expr{name, value},
			pos:   binding.pos,
		})
	}

	items := make([]expr, 0, len(list.items))
	items = append(items, head)
	items = append(items, listExpr{items: bindings, pos: rawBindings.pos})
	for _, rawBody := range list.items[2:] {
		body, err := expandSyntaxTemplate(rawBody, templateEnv, defEnv, repeatIndex, localRenamed)
		if err != nil {
			return nil, true, err
		}
		items = append(items, body)
	}

	return listExpr{items: items, pos: list.pos}, true, nil
}

func expandSyntaxParamSpec(params expr, templateEnv *env, defEnv *env, repeatIndex *int, renamed map[string]string) (expr, error) {
	switch value := params.(type) {
	case symbolExpr:
		return expandSyntaxParamIdentifier(value, templateEnv, defEnv, repeatIndex, renamed)
	case listExpr:
		items := make([]expr, 0, len(value.items))
		for _, rawParam := range value.items {
			param, err := expandSyntaxParamIdentifier(rawParam, templateEnv, defEnv, repeatIndex, renamed)
			if err != nil {
				return nil, err
			}
			items = append(items, param)
		}
		return listExpr{items: items, pos: value.pos}, nil
	default:
		return expandSyntaxTemplate(params, templateEnv, defEnv, repeatIndex, renamed)
	}
}

func expandSyntaxParamIdentifier(param expr, templateEnv *env, defEnv *env, repeatIndex *int, renamed map[string]string) (expr, error) {
	if symbol, ok := param.(symbolExpr); ok && symbol.name == "." {
		return symbol, nil
	}
	return expandSyntaxBindingIdentifier(param, templateEnv, defEnv, repeatIndex, renamed)
}

func expandSyntaxBindingIdentifier(binding expr, templateEnv *env, defEnv *env, repeatIndex *int, renamed map[string]string) (expr, error) {
	symbol, ok := binding.(symbolExpr)
	if !ok {
		return expandSyntaxTemplate(binding, templateEnv, defEnv, repeatIndex, renamed)
	}

	if value, ok, err := lookupSyntaxTemplateBinding(templateEnv, symbol.name, repeatIndex); err != nil {
		return nil, err
	} else if ok {
		return value, nil
	}

	fresh := freshMacroName(defEnv, symbol.name)
	renamed[symbol.name] = fresh
	return symbolExpr{name: fresh, pos: symbol.pos}, nil
}

func lookupSyntaxTemplateBinding(environment *env, name string, repeatIndex *int) (expr, bool, error) {
	value, ok := environment.lookup(name)
	if !ok {
		return nil, false, nil
	}

	switch syntax := value.(type) {
	case *syntaxExpr:
		return cloneSyntax(syntax.datum), true, nil
	case *syntaxListExpr:
		if repeatIndex == nil {
			return nil, false, &EvalError{Message: fmt.Sprintf("pattern variable %s used outside ellipsis", name)}
		}

		index := *repeatIndex
		if index < 0 || index >= len(syntax.values) {
			return nil, false, &EvalError{Message: fmt.Sprintf("ellipsis index out of range for %s", name)}
		}
		return cloneSyntax(syntax.values[index]), true, nil
	default:
		return nil, false, nil
	}
}

func syntaxTemplateRepeatCount(template expr, environment *env) (int, error) {
	names := map[string]struct{}{}
	collectRepeatedSyntaxTemplateVars(template, environment, names)
	if len(names) == 0 {
		return 0, &EvalError{Message: "template ellipsis must reference a repeated pattern variable"}
	}

	count := -1
	for name := range names {
		value, _ := environment.lookup(name)
		repeated, ok := value.(*syntaxListExpr)
		if !ok {
			continue
		}
		if count == -1 {
			count = len(repeated.values)
			continue
		}
		if count != len(repeated.values) {
			return 0, &EvalError{Message: "template ellipsis has mismatched repetition counts"}
		}
	}

	if count < 0 {
		count = 0
	}
	return count, nil
}

func collectRepeatedSyntaxTemplateVars(template expr, environment *env, out map[string]struct{}) {
	switch value := template.(type) {
	case symbolExpr:
		if binding, ok := environment.lookup(value.name); ok {
			if _, ok := binding.(*syntaxListExpr); ok {
				out[value.name] = struct{}{}
			}
		}
	case listExpr:
		for _, item := range value.items {
			if isEllipsisExpr(item) {
				continue
			}
			collectRepeatedSyntaxTemplateVars(item, environment, out)
		}
	}
}

func isPlainSyntaxTemplateIdentifier(form expr, name string, environment *env) bool {
	symbol, ok := form.(symbolExpr)
	if !ok || symbol.name != name {
		return false
	}

	if value, ok := environment.lookup(name); ok {
		switch value.(type) {
		case *syntaxExpr, *syntaxListExpr:
			return false
		}
	}
	return true
}
