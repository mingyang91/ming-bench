package ming

type syntaxContext int

const (
	syntaxContextUseSite syntaxContext = iota
	syntaxContextIntroduced
)

type syntaxValue struct {
	expr    locatedExpr
	context syntaxContext
}

type syntaxRepeatValue struct {
	items []syntaxValue
}

func (syntaxValue) schemeString() string {
	return "#<syntax>"
}

func (syntaxValue) isTruthy() bool {
	return true
}

func (syntaxRepeatValue) schemeString() string {
	return "#<syntax-sequence>"
}

func (syntaxRepeatValue) isTruthy() bool {
	return true
}

func evalSyntax(parts []locatedExpr, env *env) (value, error) {
	if len(parts) != 1 {
		return nil, newCurrentEvalError("'syntax' expects exactly 1 argument")
	}

	expr, context, err := expandSyntaxTemplate(parts[0], env, nil)
	if err != nil {
		return nil, err
	}

	return syntaxValue{expr: expr, context: context}, nil
}

func evalSyntaxCase(parts []locatedExpr, env *env) (value, error) {
	if len(parts) < 3 {
		return nil, newCurrentEvalError("'syntax-case' expects an input, literals, and at least 1 clause")
	}

	inputValue, err := evalExpr(parts[0], env)
	if err != nil {
		return nil, err
	}

	input, err := expectSyntaxObject(inputValue, "syntax-case")
	if err != nil {
		return nil, err
	}

	literals, err := parseSyntaxCaseLiterals(parts[1])
	if err != nil {
		return nil, err
	}

	for _, clauseExpr := range parts[2:] {
		clause, ok := clauseExpr.form.(listExpr)
		if !ok || len(clause) < 2 {
			return nil, newEvalError(clauseExpr.pos, "'syntax-case' clauses must have a pattern and body")
		}

		clauseEnv, matched, err := bindSyntaxPattern(env, clause[0], input, literals)
		if err != nil {
			return nil, err
		}
		if !matched {
			continue
		}

		bodyStart := 1
		if len(clause) >= 3 {
			fender, err := evalExpr(clause[1], clauseEnv)
			if err != nil {
				return nil, err
			}
			if !fender.isTruthy() {
				continue
			}
			bodyStart = 2
		}

		return evalSequence(clause[bodyStart:], clauseEnv)
	}

	return nil, newEvalError(parts[0].pos, "no matching syntax-case clause")
}

func evalWithSyntax(parts []locatedExpr, env *env) (value, error) {
	if len(parts) < 2 {
		return nil, newCurrentEvalError("'with-syntax' expects bindings and a body")
	}

	bindings, ok := parts[0].form.(listExpr)
	if !ok {
		return nil, newEvalError(parts[0].pos, "'with-syntax' bindings must be a list")
	}

	bodyEnv := env
	for _, bindingExpr := range bindings {
		binding, ok := bindingExpr.form.(listExpr)
		if !ok || len(binding) != 2 {
			return nil, newEvalError(bindingExpr.pos, "'with-syntax' bindings must be (pattern expr) pairs")
		}

		boundValue, err := evalExpr(binding[1], env)
		if err != nil {
			return nil, err
		}

		syntaxObj, err := expectSyntaxObject(boundValue, "with-syntax")
		if err != nil {
			return nil, err
		}

		nextEnv, matched, err := bindSyntaxPattern(bodyEnv, binding[0], syntaxObj, nil)
		if err != nil {
			return nil, err
		}
		if !matched {
			return nil, newEvalError(bindingExpr.pos, "'with-syntax' binding did not match")
		}
		bodyEnv = nextEnv
	}

	return evalSequence(parts[1:], bodyEnv)
}

func parseSyntaxCaseLiterals(expr locatedExpr) (map[string]struct{}, error) {
	items, ok := expr.form.(listExpr)
	if !ok {
		return nil, newEvalError(expr.pos, "'syntax-case' literals must be a list")
	}

	literals := make(map[string]struct{}, len(items))
	for _, item := range items {
		name, ok := item.form.(symbolExpr)
		if !ok || string(name) == "..." {
			return nil, newEvalError(item.pos, "'syntax-case' literals must be identifiers")
		}
		literals[string(name)] = struct{}{}
	}

	return literals, nil
}

func bindSyntaxPattern(base *env, pattern locatedExpr, input syntaxValue, literals map[string]struct{}) (*env, bool, error) {
	match := &macroMatch{
		single:   make(map[string]locatedExpr),
		repeated: make(map[string][]locatedExpr),
	}

	ok, err := matchPatternExpr(pattern, input.expr, literals, match, false)
	if err != nil || !ok {
		return nil, ok, err
	}

	boundEnv := newEnv(base)
	for name, expr := range match.single {
		boundEnv.define(name, syntaxValueFromExpr(expr))
	}
	for name, exprs := range match.repeated {
		items := make([]syntaxValue, len(exprs))
		for i, expr := range exprs {
			items[i] = syntaxValueFromExpr(expr)
		}
		boundEnv.define(name, syntaxRepeatValue{items: items})
	}

	return boundEnv, true, nil
}

func syntaxValueFromExpr(expr locatedExpr) syntaxValue {
	context := syntaxContextUseSite
	if _, ok := expr.form.(templateSymbolExpr); ok {
		context = syntaxContextIntroduced
	}

	return syntaxValue{
		expr:    cloneLocatedExpr(expr),
		context: context,
	}
}

func expandSyntaxTemplate(template locatedExpr, env *env, repetition []int) (locatedExpr, syntaxContext, error) {
	switch expr := template.form.(type) {
	case symbolExpr:
		name := string(expr)
		if name == "..." {
			return locatedExpr{}, syntaxContextUseSite, newEvalError(template.pos, "invalid use of ellipsis in syntax template")
		}

		if binding, ok := env.lookupBinding(name); ok {
			switch value := binding.value.(type) {
			case syntaxValue:
				return cloneLocatedExpr(value.expr), value.context, nil
			case syntaxRepeatValue:
				if len(repetition) == 0 {
					return locatedExpr{}, syntaxContextUseSite, newEvalError(template.pos, "repeated syntax variable used outside ellipsis: %s", name)
				}
				index := repetition[len(repetition)-1]
				if index < 0 || index >= len(value.items) {
					return locatedExpr{}, syntaxContextUseSite, newEvalError(template.pos, "ellipsis expansion out of range for %s", name)
				}
				item := value.items[index]
				return cloneLocatedExpr(item.expr), item.context, nil
			}
		}

		return locatedExpr{form: templateSymbolExpr(name), pos: template.pos}, syntaxContextIntroduced, nil
	case listExpr:
		items := make([]locatedExpr, 0, len(expr))
		for i := 0; i < len(expr); i++ {
			if i+1 < len(expr) && isEllipsisExpr(expr[i+1]) {
				count, err := syntaxTemplateRepeatCount(expr[i], env)
				if err != nil {
					return locatedExpr{}, syntaxContextUseSite, err
				}
				for index := 0; index < count; index++ {
					item, _, err := expandSyntaxTemplate(expr[i], env, append(repetition, index))
					if err != nil {
						return locatedExpr{}, syntaxContextUseSite, err
					}
					items = append(items, item)
				}
				i++
				continue
			}

			item, _, err := expandSyntaxTemplate(expr[i], env, repetition)
			if err != nil {
				return locatedExpr{}, syntaxContextUseSite, err
			}
			items = append(items, item)
		}
		return locatedExpr{form: listExpr(items), pos: template.pos}, syntaxContextUseSite, nil
	case numberExpr, boolExpr, stringExpr, charExpr:
		return cloneLocatedExpr(template), syntaxContextUseSite, nil
	default:
		return locatedExpr{}, syntaxContextUseSite, newEvalError(template.pos, "unsupported syntax template")
	}
}

func syntaxTemplateRepeatCount(template locatedExpr, env *env) (int, error) {
	names := repeatedSyntaxTemplateNames(template, env, nil)
	if len(names) == 0 {
		return 0, newEvalError(template.pos, "ellipsis template must reference a repeated syntax variable")
	}

	count := -1
	for _, name := range names {
		binding, ok := env.lookupBinding(name)
		if !ok {
			continue
		}

		values, ok := binding.value.(syntaxRepeatValue)
		if !ok {
			continue
		}

		if count == -1 {
			count = len(values.items)
			continue
		}
		if len(values.items) != count {
			return 0, newEvalError(template.pos, "inconsistent ellipsis lengths in syntax template")
		}
	}

	if count < 0 {
		return 0, newEvalError(template.pos, "ellipsis template must reference a repeated syntax variable")
	}
	return count, nil
}

func repeatedSyntaxTemplateNames(template locatedExpr, env *env, names []string) []string {
	switch expr := template.form.(type) {
	case symbolExpr:
		name := string(expr)
		if name == "..." {
			return names
		}
		binding, ok := env.lookupBinding(name)
		if !ok {
			return names
		}
		if _, ok := binding.value.(syntaxRepeatValue); ok {
			return append(names, name)
		}
	case listExpr:
		for i := 0; i < len(expr); i++ {
			if isEllipsisExpr(expr[i]) {
				continue
			}
			names = repeatedSyntaxTemplateNames(expr[i], env, names)
		}
	}
	return names
}

func evalSyntaxToDatum(args []value) (value, error) {
	if len(args) != 1 {
		return nil, newCurrentEvalError("'syntax->datum' expects exactly 1 argument")
	}

	syntaxObj, err := expectSyntaxObject(args[0], "syntax->datum")
	if err != nil {
		return nil, err
	}

	return syntaxExprToDatum(syntaxObj.expr)
}

func evalDatumToSyntax(args []value) (value, error) {
	if len(args) != 2 {
		return nil, newCurrentEvalError("'datum->syntax' expects exactly 2 arguments")
	}

	context, err := expectSyntaxObject(args[0], "datum->syntax")
	if err != nil {
		return nil, err
	}

	expr, err := datumToSyntaxExpr(args[1], context.context)
	if err != nil {
		return nil, err
	}

	return syntaxValue{expr: expr, context: context.context}, nil
}

func expectSyntaxObject(v value, name string) (syntaxValue, error) {
	syntaxObj, ok := v.(syntaxValue)
	if !ok {
		return syntaxValue{}, newCurrentEvalError("'%s' expects a syntax object, got %s", name, v.schemeString())
	}
	return syntaxObj, nil
}

func syntaxExprToDatum(expr locatedExpr) (value, error) {
	switch form := expr.form.(type) {
	case numberExpr:
		return form, nil
	case boolExpr:
		return boolValue(form), nil
	case stringExpr:
		return newStringValue(string(form)), nil
	case charExpr:
		return charValue(form), nil
	case symbolExpr:
		return symbolValue(form), nil
	case templateSymbolExpr:
		return symbolValue(form), nil
	case listExpr:
		return exprListToDatum(form, syntaxExprToDatum)
	default:
		return nil, newEvalError(expr.pos, "unsupported syntax datum")
	}
}

func datumToSyntaxExpr(v value, context syntaxContext) (locatedExpr, error) {
	switch datum := v.(type) {
	case numberValue:
		return locatedExpr{form: numberExpr(datum), pos: defaultSourcePos()}, nil
	case boolValue:
		return locatedExpr{form: boolExpr(datum), pos: defaultSourcePos()}, nil
	case *stringValue:
		return locatedExpr{form: stringExpr(datum.text()), pos: defaultSourcePos()}, nil
	case charValue:
		return locatedExpr{form: charExpr(datum), pos: defaultSourcePos()}, nil
	case symbolValue:
		if context == syntaxContextIntroduced {
			return locatedExpr{form: templateSymbolExpr(datum), pos: defaultSourcePos()}, nil
		}
		return locatedExpr{form: symbolExpr(datum), pos: defaultSourcePos()}, nil
	case emptyListValue:
		return locatedExpr{form: listExpr{}, pos: defaultSourcePos()}, nil
	case pairValue:
		items, err := datumPairToSyntaxList(datum, context, make(map[*pairCell]struct{}))
		if err != nil {
			return locatedExpr{}, err
		}
		return locatedExpr{form: items, pos: defaultSourcePos()}, nil
	default:
		return locatedExpr{}, newCurrentEvalError("'datum->syntax' cannot convert %s", v.schemeString())
	}
}

func datumPairToSyntaxList(v value, context syntaxContext, seen map[*pairCell]struct{}) (listExpr, error) {
	items := make([]locatedExpr, 0)
	current := v

	for {
		switch datum := current.(type) {
		case emptyListValue:
			return listExpr(items), nil
		case pairValue:
			if _, ok := seen[datum.cell]; ok {
				return nil, newCurrentEvalError("'datum->syntax' cannot convert cyclic list")
			}
			seen[datum.cell] = struct{}{}

			expr, err := datumToSyntaxExpr(datum.carValue(), context)
			if err != nil {
				return nil, err
			}
			items = append(items, expr)
			current = datum.cdrValue()
		default:
			tailExpr, err := datumToSyntaxExpr(datum, context)
			if err != nil {
				return nil, err
			}
			items = append(items, locatedExpr{form: symbolExpr("."), pos: defaultSourcePos()}, tailExpr)
			return listExpr(items), nil
		}
	}
}
