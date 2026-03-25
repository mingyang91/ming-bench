package ming

func expandQuasiquoteForm(list *listExpr) (expr, error) {
	if len(list.elements) != 2 {
		return nil, newEvalError(ErrSyntax, "quasiquote: expected exactly one argument", list.at)
	}
	return expandQuasiquoteExpr(list.elements[1], 1)
}

func expandQuasiquoteExpr(node expr, depth int) (expr, error) {
	if form, ok := prefixedExpr(node, "unquote"); ok {
		if len(form.elements) != 2 {
			return nil, newEvalError(ErrSyntax, "unquote: expected exactly one argument", form.at)
		}
		if depth == 1 {
			return cloneExpr(form.elements[1]), nil
		}

		inner, err := expandQuasiquoteExpr(form.elements[1], depth-1)
		if err != nil {
			return nil, err
		}
		return listCallExpr(form.at, "list", quoteSymbolExpr("unquote", form.at), inner), nil
	}

	if form, ok := prefixedExpr(node, "unquote-splicing"); ok {
		if len(form.elements) != 2 {
			return nil, newEvalError(ErrSyntax, "unquote-splicing: expected exactly one argument", form.at)
		}
		if depth == 1 {
			return nil, newEvalError(ErrSyntax, "unquote-splicing: not in list context", form.at)
		}

		inner, err := expandQuasiquoteExpr(form.elements[1], depth-1)
		if err != nil {
			return nil, err
		}
		return listCallExpr(form.at, "list", quoteSymbolExpr("unquote-splicing", form.at), inner), nil
	}

	if form, ok := prefixedExpr(node, "quasiquote"); ok {
		if len(form.elements) != 2 {
			return nil, newEvalError(ErrSyntax, "quasiquote: expected exactly one argument", form.at)
		}

		inner, err := expandQuasiquoteExpr(form.elements[1], depth+1)
		if err != nil {
			return nil, err
		}
		return listCallExpr(form.at, "list", quoteSymbolExpr("quasiquote", form.at), inner), nil
	}

	if list, ok := node.(*listExpr); ok {
		return expandQuasiquoteList(list, depth)
	}

	return quoteExpr(cloneExpr(node), node.pos()), nil
}

func expandQuasiquoteList(list *listExpr, depth int) (expr, error) {
	elements, tailExpr, hasTail, err := splitDottedListElements(list.elements)
	if err != nil {
		return nil, err
	}

	var result expr
	if hasTail {
		result, err = expandQuasiquoteTail(tailExpr, depth)
		if err != nil {
			return nil, err
		}
	} else {
		result = quoteExpr(&listExpr{at: list.at}, list.at)
	}

	for i := len(elements) - 1; i >= 0; i-- {
		if depth == 1 {
			if form, ok := prefixedExpr(elements[i], "unquote-splicing"); ok {
				if len(form.elements) != 2 {
					return nil, newEvalError(ErrSyntax, "unquote-splicing: expected exactly one argument", form.at)
				}
				result = listCallExpr(form.at, "append", cloneExpr(form.elements[1]), result)
				continue
			}
		}

		item, err := expandQuasiquoteExpr(elements[i], depth)
		if err != nil {
			return nil, err
		}
		result = listCallExpr(list.at, "cons", item, result)
	}

	return result, nil
}

func expandQuasiquoteTail(node expr, depth int) (expr, error) {
	if depth == 1 {
		if form, ok := prefixedExpr(node, "unquote"); ok {
			if len(form.elements) != 2 {
				return nil, newEvalError(ErrSyntax, "unquote: expected exactly one argument", form.at)
			}
			return cloneExpr(form.elements[1]), nil
		}
	}
	return expandQuasiquoteExpr(node, depth)
}

func prefixedExpr(node expr, name string) (*listExpr, bool) {
	list, ok := node.(*listExpr)
	if !ok || len(list.elements) == 0 {
		return nil, false
	}

	head, ok := list.elements[0].(*symbolExpr)
	if !ok || head.name != name {
		return nil, false
	}
	return list, true
}

func listCallExpr(at position, name string, args ...expr) *listExpr {
	elements := make([]expr, 0, len(args)+1)
	elements = append(elements, &symbolExpr{name: name, at: at})
	elements = append(elements, args...)
	return &listExpr{elements: elements, at: at}
}

func quoteExpr(node expr, at position) *listExpr {
	return &listExpr{
		elements: []expr{
			&symbolExpr{name: "quote", at: at},
			node,
		},
		at: at,
	}
}

func quoteSymbolExpr(name string, at position) *listExpr {
	return quoteExpr(&symbolExpr{name: name, at: at}, at)
}
