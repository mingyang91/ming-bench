package ming

import "fmt"

type macroExpander interface {
	expand(call listExpr) (any, error)
}

type transformerMacro struct {
	proc any
}

type syntaxObject struct {
	expr any
}

type syntaxPatternValue struct {
	expr any
}

type syntaxPatternRepeatValue struct {
	values []any
}

func (m transformerMacro) expand(call listExpr) (any, error) {
	value, err := applyProcedureRaw(m.proc, []any{syntaxObject{expr: call}})
	if err != nil {
		return nil, err
	}

	value, err = consumeSingleValue(value)
	if err != nil {
		return nil, err
	}

	return syntaxValueExpr(value)
}

func isSyntaxRulesForm(expr any) bool {
	form, ok := expr.(listExpr)
	if !ok || len(form.elements) == 0 {
		return false
	}

	head, ok := form.elements[0].(symbolExpr)
	return ok && head.name == "syntax-rules"
}

func markTransformerProcedure(proc any, defEnv *env) any {
	switch callable := proc.(type) {
	case closure:
		callable.syntaxDefEnv = defEnv
		return callable
	case caseClosure:
		clauses := make([]closure, len(callable.clauses))
		copy(clauses, callable.clauses)
		for i := range clauses {
			clauses[i].syntaxDefEnv = defEnv
		}
		callable.clauses = clauses
		return callable
	default:
		return proc
	}
}

func evalSyntax(scope *env, args []any) (any, error) {
	if len(args) != 1 {
		return nil, &EvalError{Message: "syntax expects exactly 1 argument"}
	}

	rule, bindings := collectSyntaxTemplateBindings(scope)
	defEnv := scope.syntaxDefEnv
	if defEnv == nil {
		defEnv = scope
	}

	expander := syntaxMacro{
		defEnv:    defEnv,
		aliasKeys: map[string]string{},
	}
	expr, err := expander.instantiate(rule, args[0], bindings, map[string]string{}, nil)
	if err != nil {
		return nil, err
	}

	return syntaxObject{expr: expr}, nil
}

func evalSyntaxCase(scope *env, args []any) (any, error) {
	if len(args) < 3 {
		return nil, &EvalError{Message: "syntax-case expects an input, literals, and at least 1 clause"}
	}

	value, err := eval(scope, args[0])
	if err != nil {
		return nil, err
	}

	input, err := expectSyntaxValue(value, "syntax-case")
	if err != nil {
		return nil, err
	}

	literals, err := parseSyntaxLiterals(args[1])
	if err != nil {
		return nil, err
	}

	for _, clauseExpr := range args[2:] {
		clause, ok := clauseExpr.(listExpr)
		if !ok || len(clause.elements) < 2 {
			return nil, exprSourcePos(clauseExpr).errorf("syntax-case clauses must contain a pattern and template")
		}

		bindings, _, matched, err := matchSyntaxPattern(clause.elements[0], input.expr, literals)
		if err != nil {
			return nil, err
		}
		if !matched {
			continue
		}

		clauseScope := newEnv(scope)
		installSyntaxBindings(clauseScope, bindings)

		bodyIndex := 1
		if len(clause.elements) > 2 {
			passed, err := eval(clauseScope, clause.elements[1])
			if err != nil {
				return nil, err
			}
			if !isTruthy(passed) {
				continue
			}
			bodyIndex = 2
		}
		if bodyIndex >= len(clause.elements) {
			return nil, exprSourcePos(clauseExpr).errorf("syntax-case clause is missing a template")
		}

		return evalSequence(clauseScope, clause.elements[bodyIndex:])
	}

	return nil, exprSourcePos(args[0]).errorf("syntax-case found no matching clause")
}

func evalWithSyntax(scope *env, args []any) (any, error) {
	if len(args) < 2 {
		return nil, &EvalError{Message: "with-syntax expects bindings and a body"}
	}

	bindingList, ok := args[0].(listExpr)
	if !ok {
		return nil, exprSourcePos(args[0]).errorf("with-syntax bindings must be a list")
	}

	bodyScope := newEnv(scope)
	for _, bindingExpr := range bindingList.elements {
		binding, ok := bindingExpr.(listExpr)
		if !ok || len(binding.elements) != 2 {
			return nil, exprSourcePos(bindingExpr).errorf("with-syntax bindings must contain a pattern and expression")
		}

		value, err := eval(scope, binding.elements[1])
		if err != nil {
			return nil, err
		}

		stx, err := expectSyntaxValue(value, "with-syntax")
		if err != nil {
			return nil, err
		}

		bindings, _, matched, err := matchSyntaxPattern(binding.elements[0], stx.expr, map[string]struct{}{})
		if err != nil {
			return nil, err
		}
		if !matched {
			return nil, exprSourcePos(bindingExpr).errorf("with-syntax pattern did not match")
		}

		installSyntaxBindings(bodyScope, bindings)
	}

	return evalSequence(bodyScope, args[1:])
}

func builtinSyntaxToDatum(args []any) (any, error) {
	if len(args) != 1 {
		return nil, &EvalError{Message: "syntax->datum expects exactly 1 argument"}
	}

	stx, err := expectSyntaxValue(args[0], "syntax->datum")
	if err != nil {
		return nil, err
	}
	return syntaxToDatum(stx.expr), nil
}

func builtinDatumToSyntax(args []any) (any, error) {
	if len(args) != 2 {
		return nil, &EvalError{Message: "datum->syntax expects exactly 2 arguments"}
	}

	template, err := expectSyntaxValue(args[0], "datum->syntax")
	if err != nil {
		return nil, err
	}

	expr, err := datumToSyntax(template.expr, args[1])
	if err != nil {
		return nil, err
	}
	return syntaxObject{expr: expr}, nil
}

func builtinIdentifierPredicate(args []any) (any, error) {
	if len(args) != 1 {
		return nil, &EvalError{Message: "identifier? expects exactly 1 argument"}
	}

	stx, err := expectSyntaxValue(args[0], "identifier?")
	if err != nil {
		return nil, err
	}

	_, ok := stx.expr.(symbolExpr)
	return ok, nil
}

func syntaxValueExpr(value any) (any, error) {
	switch value := value.(type) {
	case syntaxObject:
		return cloneSyntax(value.expr), nil
	case syntaxPatternValue:
		return cloneSyntax(value.expr), nil
	default:
		return nil, &EvalError{Message: "macro transformer must return a syntax object"}
	}
}

func expectSyntaxValue(value any, who string) (syntaxObject, error) {
	switch value := value.(type) {
	case syntaxObject:
		return value, nil
	case syntaxPatternValue:
		return syntaxObject{expr: value.expr}, nil
	default:
		return syntaxObject{}, &EvalError{Message: fmt.Sprintf("%s expects a syntax object, got %s", who, typeName(value))}
	}
}

func parseSyntaxLiterals(expr any) (map[string]struct{}, error) {
	literalList, ok := expr.(listExpr)
	if !ok {
		return nil, exprSourcePos(expr).errorf("syntax-case literals must be a list")
	}

	literals := map[string]struct{}{}
	for _, literalExpr := range literalList.elements {
		literal, ok := literalExpr.(symbolExpr)
		if !ok {
			return nil, exprSourcePos(literalExpr).errorf("syntax-case literals must be identifiers")
		}
		literals[literal.name] = struct{}{}
	}
	return literals, nil
}

func matchSyntaxPattern(pattern any, expr any, literals map[string]struct{}) (*matchBindings, syntaxRule, bool, error) {
	patternVars, repeatedVars, err := analyzePatternVars(pattern, literals)
	if err != nil {
		return nil, syntaxRule{}, false, err
	}

	rule := syntaxRule{
		pattern:      pattern,
		patternVars:  patternVars,
		repeatedVars: repeatedVars,
	}
	bindings := &matchBindings{
		scalars: map[string]any{},
		repeats: map[string][]any{},
	}
	for name := range repeatedVars {
		bindings.repeats[name] = []any{}
	}

	if !rule.matchPattern(pattern, expr, literals, bindings, false) {
		return nil, rule, false, nil
	}
	return bindings, rule, true, nil
}

func collectSyntaxTemplateBindings(scope *env) (syntaxRule, *matchBindings) {
	rule := syntaxRule{
		patternVars:  map[string]struct{}{},
		repeatedVars: map[string]struct{}{},
	}
	bindings := &matchBindings{
		scalars: map[string]any{},
		repeats: map[string][]any{},
	}
	seen := map[string]struct{}{}

	for current := scope; current != nil; current = current.parent {
		for name, cell := range current.bindings {
			if _, ok := seen[name]; ok {
				continue
			}

			switch value := cell.value.(type) {
			case syntaxPatternValue:
				seen[name] = struct{}{}
				rule.patternVars[name] = struct{}{}
				bindings.scalars[name] = cloneSyntax(value.expr)
			case syntaxPatternRepeatValue:
				seen[name] = struct{}{}
				rule.patternVars[name] = struct{}{}
				rule.repeatedVars[name] = struct{}{}

				cloned := make([]any, len(value.values))
				for i, entry := range value.values {
					cloned[i] = cloneSyntax(entry)
				}
				bindings.repeats[name] = cloned
			}
		}
	}

	return rule, bindings
}

func installSyntaxBindings(scope *env, bindings *matchBindings) {
	for name, value := range bindings.scalars {
		scope.defineKey(name, syntaxPatternValue{expr: cloneSyntax(value)})
	}
	for name, values := range bindings.repeats {
		cloned := make([]any, len(values))
		for i, value := range values {
			cloned[i] = cloneSyntax(value)
		}
		scope.defineKey(name, syntaxPatternRepeatValue{values: cloned})
	}
}

func syntaxToDatum(expr any) any {
	switch expr := expr.(type) {
	case symbolExpr:
		return symbolExpr{name: expr.name}
	case stringExpr:
		return expr.value
	case listExpr:
		elements := make([]any, len(expr.elements))
		for i, elem := range expr.elements {
			elements[i] = syntaxToDatum(elem)
		}
		return makeListValue(elements)
	default:
		return quoteDatum(expr)
	}
}

type syntaxContext struct {
	pos sourcePos
	key string
}

func datumToSyntax(template any, datum any) (any, error) {
	ctx := syntaxContext{pos: exprSourcePos(template)}
	if symbol, ok := template.(symbolExpr); ok {
		ctx.key = symbol.key
	}
	return datumToSyntaxWithContext(ctx, datum)
}

func datumToSyntaxWithContext(ctx syntaxContext, datum any) (any, error) {
	switch datum := datum.(type) {
	case syntaxObject:
		return cloneSyntax(datum.expr), nil
	case syntaxPatternValue:
		return cloneSyntax(datum.expr), nil
	case int64, rationalValue, float64, bool, charValue:
		return datum, nil
	case string:
		return stringExpr{value: datum, pos: ctx.pos}, nil
	case *mutableString:
		return stringExpr{value: string(datum.runes), pos: ctx.pos}, nil
	case symbolExpr:
		return symbolExpr{name: datum.name, key: ctx.key, pos: ctx.pos}, nil
	case emptyListValue:
		return listExpr{elements: nil, pos: ctx.pos}, nil
	case pairValue:
		elements, err := properListElements(datum, "datum->syntax")
		if err != nil {
			return nil, err
		}

		result := make([]any, len(elements))
		for i, elem := range elements {
			value, err := datumToSyntaxWithContext(ctx, elem)
			if err != nil {
				return nil, err
			}
			result[i] = value
		}
		return listExpr{elements: result, pos: ctx.pos}, nil
	default:
		return nil, &EvalError{Message: fmt.Sprintf("datum->syntax expects a datum, got %s", typeName(datum))}
	}
}
