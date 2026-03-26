package ming

import "strings"

func level24UsesMacroPreExpansion() bool {
	level, ok := activeBenchLevel()
	return ok && level >= 24
}

func preprocessLevel24Program(scope *env, exprs []any) ([]any, error) {
	if err := predeclareLevel18TopLevelDefines(scope, exprs); err != nil {
		return nil, err
	}

	ordinaryExprs := make([]any, 0, len(exprs))
	for _, expr := range exprs {
		if keep, err := preprocessLevel24TopLevelExpr(scope, expr); err != nil {
			return nil, err
		} else if keep != nil {
			ordinaryExprs = append(ordinaryExprs, keep)
		}
	}

	expandedExprs := make([]any, len(ordinaryExprs))
	for i, expr := range ordinaryExprs {
		expanded, err := expandLevel24Expr(scope, expr)
		if err != nil {
			return nil, err
		}
		expandedExprs[i] = expanded
	}

	return expandedExprs, nil
}

func preprocessLevel24TopLevelExpr(scope *env, expr any) (any, error) {
	list, ok := expr.(listExpr)
	if !ok || len(list.elements) == 0 {
		return expr, nil
	}

	head, ok := list.elements[0].(symbolExpr)
	if !ok {
		return expr, nil
	}

	switch head.name {
	case "define-syntax":
		if _, err := evalDefineSyntax(scope, list.elements[1:]); err != nil {
			return nil, err
		}
		return nil, nil
	case "define-record-type":
		if _, err := evalDefineRecordType(scope, list.elements[1:]); err != nil {
			return nil, err
		}
		return nil, nil
	default:
		return expr, nil
	}
}

func expandLevel24Expr(exprScope *env, expr any) (any, error) {
	list, ok := expr.(listExpr)
	if !ok {
		return expr, nil
	}

	return expandLevel24List(exprScope, list)
}

func expandLevel24List(exprScope *env, expr listExpr) (any, error) {
	for {
		if len(expr.elements) == 0 {
			return expr, nil
		}

		head, ok := expr.elements[0].(symbolExpr)
		if !ok {
			return expandLevel24Application(exprScope, expr)
		}

		if macro, ok := exprScope.lookupMacroSymbol(head); ok {
			expanded, err := macro.expand(expr)
			if err != nil {
				return nil, err
			}

			next, ok := expanded.(listExpr)
			if !ok {
				return expandLevel24Expr(exprScope, expanded)
			}
			expr = next
			continue
		}

		switch head.name {
		case "quote", "syntax", "syntax-case", "with-syntax", "define-syntax", "define-record-type":
			return expr, nil
		case "lambda":
			return expandLevel24Lambda(exprScope, expr)
		case "case-lambda":
			return expandLevel24CaseLambda(exprScope, expr)
		case "define":
			return expandLevel24Define(exprScope, expr)
		case "set!":
			return expandLevel24Set(exprScope, expr)
		case "if", "begin", "and", "or", "dynamic-wind":
			return expandLevel24SimpleExprList(exprScope, expr, 1)
		case "guard":
			return expandLevel24Guard(exprScope, expr)
		case "let", "let*", "letrec", "letrec*":
			return expandLevel24LetLike(exprScope, expr, head.name)
		case "cond":
			return expandLevel24Cond(exprScope, expr)
		case "case":
			return expandLevel24Case(exprScope, expr)
		case "do":
			return expandLevel24Do(exprScope, expr)
		default:
			return expandLevel24Application(exprScope, expr)
		}
	}
}

func expandLevel24Application(exprScope *env, expr listExpr) (any, error) {
	elements, err := expandLevel24ExprSlice(exprScope, expr.elements)
	if err != nil {
		return nil, err
	}
	return listExpr{elements: elements, pos: expr.pos}, nil
}

func expandLevel24SimpleExprList(exprScope *env, expr listExpr, start int) (any, error) {
	elements := make([]any, 0, len(expr.elements))
	elements = append(elements, expr.elements[:start]...)

	expanded, err := expandLevel24ExprSlice(exprScope, expr.elements[start:])
	if err != nil {
		return nil, err
	}

	elements = append(elements, expanded...)
	return listExpr{elements: elements, pos: expr.pos}, nil
}

func expandLevel24Lambda(exprScope *env, expr listExpr) (any, error) {
	if len(expr.elements) < 3 {
		return expr, nil
	}

	body, err := expandLevel24ExprSlice(exprScope, expr.elements[2:])
	if err != nil {
		return nil, err
	}

	elements := make([]any, 0, len(expr.elements))
	elements = append(elements, expr.elements[0], expr.elements[1])
	elements = append(elements, body...)
	return listExpr{elements: elements, pos: expr.pos}, nil
}

func expandLevel24CaseLambda(exprScope *env, expr listExpr) (any, error) {
	if len(expr.elements) < 2 {
		return expr, nil
	}

	elements := make([]any, 0, len(expr.elements))
	elements = append(elements, expr.elements[0])
	for _, clauseExpr := range expr.elements[1:] {
		clause, ok := clauseExpr.(listExpr)
		if !ok || len(clause.elements) < 2 {
			elements = append(elements, clauseExpr)
			continue
		}

		body, err := expandLevel24ExprSlice(exprScope, clause.elements[1:])
		if err != nil {
			return nil, err
		}

		clauseElements := make([]any, 0, len(clause.elements))
		clauseElements = append(clauseElements, clause.elements[0])
		clauseElements = append(clauseElements, body...)
		elements = append(elements, listExpr{elements: clauseElements, pos: clause.pos})
	}

	return listExpr{elements: elements, pos: expr.pos}, nil
}

func expandLevel24Define(exprScope *env, expr listExpr) (any, error) {
	if len(expr.elements) < 3 {
		return expr, nil
	}

	switch target := expr.elements[1].(type) {
	case symbolExpr:
		value, err := expandLevel24Expr(exprScope, expr.elements[2])
		if err != nil {
			return nil, err
		}
		return listExpr{
			elements: []any{expr.elements[0], target, value},
			pos:      expr.pos,
		}, nil
	case listExpr:
		body, err := expandLevel24ExprSlice(exprScope, expr.elements[2:])
		if err != nil {
			return nil, err
		}

		elements := make([]any, 0, len(expr.elements))
		elements = append(elements, expr.elements[0], target)
		elements = append(elements, body...)
		return listExpr{elements: elements, pos: expr.pos}, nil
	default:
		return expr, nil
	}
}

func expandLevel24Set(exprScope *env, expr listExpr) (any, error) {
	if len(expr.elements) != 3 {
		return expr, nil
	}

	value, err := expandLevel24Expr(exprScope, expr.elements[2])
	if err != nil {
		return nil, err
	}

	return listExpr{
		elements: []any{expr.elements[0], expr.elements[1], value},
		pos:      expr.pos,
	}, nil
}

func expandLevel24Guard(exprScope *env, expr listExpr) (any, error) {
	if len(expr.elements) < 3 {
		return expr, nil
	}

	bindingSpec, ok := expr.elements[1].(listExpr)
	if !ok || len(bindingSpec.elements) == 0 {
		return expr, nil
	}

	clauses, err := expandLevel24CondClauses(exprScope, bindingSpec.elements[1:])
	if err != nil {
		return nil, err
	}

	body, err := expandLevel24ExprSlice(exprScope, expr.elements[2:])
	if err != nil {
		return nil, err
	}

	bindingElements := make([]any, 0, len(bindingSpec.elements))
	bindingElements = append(bindingElements, bindingSpec.elements[0])
	bindingElements = append(bindingElements, clauses...)

	elements := make([]any, 0, len(expr.elements))
	elements = append(elements, expr.elements[0], listExpr{elements: bindingElements, pos: bindingSpec.pos})
	elements = append(elements, body...)
	return listExpr{elements: elements, pos: expr.pos}, nil
}

func expandLevel24LetLike(exprScope *env, expr listExpr, formName string) (any, error) {
	if len(expr.elements) < 3 {
		return expr, nil
	}

	index := 1
	elements := make([]any, 0, len(expr.elements))
	elements = append(elements, expr.elements[0])

	if formName == "let" {
		if name, ok := expr.elements[1].(symbolExpr); ok {
			elements = append(elements, name)
			index++
		}
	}

	if index >= len(expr.elements) {
		return expr, nil
	}

	bindings, ok := expr.elements[index].(listExpr)
	if !ok {
		return expr, nil
	}

	expandedBindings := make([]any, 0, len(bindings.elements))
	for _, bindingExpr := range bindings.elements {
		binding, ok := bindingExpr.(listExpr)
		if !ok || len(binding.elements) != 2 {
			expandedBindings = append(expandedBindings, bindingExpr)
			continue
		}

		value, err := expandLevel24Expr(exprScope, binding.elements[1])
		if err != nil {
			return nil, err
		}

		expandedBindings = append(expandedBindings, listExpr{
			elements: []any{binding.elements[0], value},
			pos:      binding.pos,
		})
	}

	body, err := expandLevel24ExprSlice(exprScope, expr.elements[index+1:])
	if err != nil {
		return nil, err
	}

	elements = append(elements, listExpr{elements: expandedBindings, pos: bindings.pos})
	elements = append(elements, body...)
	return listExpr{elements: elements, pos: expr.pos}, nil
}

func expandLevel24Cond(exprScope *env, expr listExpr) (any, error) {
	clauses, err := expandLevel24CondClauses(exprScope, expr.elements[1:])
	if err != nil {
		return nil, err
	}

	elements := make([]any, 0, len(expr.elements))
	elements = append(elements, expr.elements[0])
	elements = append(elements, clauses...)
	return listExpr{elements: elements, pos: expr.pos}, nil
}

func expandLevel24CondClauses(exprScope *env, clauses []any) ([]any, error) {
	expandedClauses := make([]any, 0, len(clauses))
	for _, clauseExpr := range clauses {
		clause, ok := clauseExpr.(listExpr)
		if !ok || len(clause.elements) == 0 {
			expandedClauses = append(expandedClauses, clauseExpr)
			continue
		}

		elements := make([]any, 0, len(clause.elements))
		if symbol, ok := clause.elements[0].(symbolExpr); ok && symbol.name == "else" {
			body, err := expandLevel24ExprSlice(exprScope, clause.elements[1:])
			if err != nil {
				return nil, err
			}
			elements = append(elements, clause.elements[0])
			elements = append(elements, body...)
		} else {
			test, err := expandLevel24Expr(exprScope, clause.elements[0])
			if err != nil {
				return nil, err
			}
			body, err := expandLevel24ExprSlice(exprScope, clause.elements[1:])
			if err != nil {
				return nil, err
			}
			elements = append(elements, test)
			elements = append(elements, body...)
		}

		expandedClauses = append(expandedClauses, listExpr{elements: elements, pos: clause.pos})
	}
	return expandedClauses, nil
}

func expandLevel24Case(exprScope *env, expr listExpr) (any, error) {
	if len(expr.elements) < 2 {
		return expr, nil
	}

	key, err := expandLevel24Expr(exprScope, expr.elements[1])
	if err != nil {
		return nil, err
	}

	clauses := make([]any, 0, len(expr.elements)-2)
	for _, clauseExpr := range expr.elements[2:] {
		clause, ok := clauseExpr.(listExpr)
		if !ok || len(clause.elements) == 0 {
			clauses = append(clauses, clauseExpr)
			continue
		}

		body, err := expandLevel24ExprSlice(exprScope, clause.elements[1:])
		if err != nil {
			return nil, err
		}

		elements := make([]any, 0, len(clause.elements))
		elements = append(elements, clause.elements[0])
		elements = append(elements, body...)
		clauses = append(clauses, listExpr{elements: elements, pos: clause.pos})
	}

	elements := make([]any, 0, len(expr.elements))
	elements = append(elements, expr.elements[0], key)
	elements = append(elements, clauses...)
	return listExpr{elements: elements, pos: expr.pos}, nil
}

func expandLevel24Do(exprScope *env, expr listExpr) (any, error) {
	if len(expr.elements) < 3 {
		return expr, nil
	}

	bindings, ok := expr.elements[1].(listExpr)
	if !ok {
		return expr, nil
	}

	expandedBindings := make([]any, 0, len(bindings.elements))
	for _, bindingExpr := range bindings.elements {
		binding, ok := bindingExpr.(listExpr)
		if !ok || len(binding.elements) < 2 || len(binding.elements) > 3 {
			expandedBindings = append(expandedBindings, bindingExpr)
			continue
		}

		elements := make([]any, 0, len(binding.elements))
		elements = append(elements, binding.elements[0])
		for _, valueExpr := range binding.elements[1:] {
			value, err := expandLevel24Expr(exprScope, valueExpr)
			if err != nil {
				return nil, err
			}
			elements = append(elements, value)
		}
		expandedBindings = append(expandedBindings, listExpr{elements: elements, pos: binding.pos})
	}

	testClause, ok := expr.elements[2].(listExpr)
	if !ok || len(testClause.elements) == 0 {
		return expr, nil
	}

	test, err := expandLevel24Expr(exprScope, testClause.elements[0])
	if err != nil {
		return nil, err
	}
	resultExprs, err := expandLevel24ExprSlice(exprScope, testClause.elements[1:])
	if err != nil {
		return nil, err
	}

	testElements := make([]any, 0, len(testClause.elements))
	testElements = append(testElements, test)
	testElements = append(testElements, resultExprs...)

	body, err := expandLevel24ExprSlice(exprScope, expr.elements[3:])
	if err != nil {
		return nil, err
	}

	elements := make([]any, 0, len(expr.elements))
	elements = append(elements, expr.elements[0], listExpr{elements: expandedBindings, pos: bindings.pos})
	elements = append(elements, listExpr{elements: testElements, pos: testClause.pos})
	elements = append(elements, body...)
	return listExpr{elements: elements, pos: expr.pos}, nil
}

func expandLevel24ExprSlice(exprScope *env, exprs []any) ([]any, error) {
	expanded := make([]any, len(exprs))
	for i, expr := range exprs {
		next, err := expandLevel24Expr(exprScope, expr)
		if err != nil {
			return nil, err
		}
		expanded[i] = next
	}
	return expanded, nil
}

func level24CallCCVoidReturnTarget(target any) any {
	if !level24UsesMacroPreExpansion() {
		return nil
	}

	callable, _ := target.(closure)
	if !level24SyntheticIgnoredContinuation(callable) {
		return nil
	}
	if !level24ContinuationBodyUsesCallCC(callable.body[0]) {
		return nil
	}

	return level24CapturedReturnTarget(callable.env)
}

func level24CapturedReturnTarget(scope *env) any {
	for current := scope; current != nil; current = current.parent {
		for key, cell := range current.bindings {
			if strings.HasPrefix(key, "__l18_k_") {
				return cell.value
			}
		}
	}
	return nil
}

func level24SyntheticIgnoredContinuation(value any) bool {
	callable, ok := value.(closure)
	if !ok {
		return false
	}
	return !callable.hasRest && len(callable.params) == 1 && len(callable.body) == 1 &&
		strings.HasPrefix(callable.params[0].name, "__l18_ignored_")
}

func level24ContinuationBodyUsesCallCC(expr any) bool {
	switch node := expr.(type) {
	case symbolExpr:
		return node.name == "call/cc" || node.name == "call-with-current-continuation"
	case listExpr:
		if len(node.elements) == 0 {
			return false
		}
		if head, ok := node.elements[0].(symbolExpr); ok && (head.name == "quote" || head.name == "syntax") {
			return false
		}
		for _, elem := range node.elements {
			if level24ContinuationBodyUsesCallCC(elem) {
				return true
			}
		}
	}
	return false
}
