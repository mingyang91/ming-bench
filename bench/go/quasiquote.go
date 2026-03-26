package ming

func evalQuasiquote(environment *env, forms []expr) (expr, error) {
	if len(forms) != 1 {
		return nil, &EvalError{Message: "quasiquote expects exactly 1 argument"}
	}
	return evalQuasiquoteExpr(environment, forms[0], 1)
}

func evalQuasiquoteExpr(environment *env, form expr, depth int) (expr, error) {
	switch value := form.(type) {
	case listExpr:
		if quoted, ok, err := maybeEvalQuasiquoteEscape(environment, value, depth); ok || err != nil {
			return quoted, err
		}
		return evalQuasiquoteList(environment, value, depth)
	case *vectorExpr:
		return evalQuasiquoteVector(environment, value, depth)
	default:
		return quoteDatum(form), nil
	}
}

func maybeEvalQuasiquoteEscape(environment *env, form listExpr, depth int) (expr, bool, error) {
	if arg, ok := quasiquoteEscapeArg(form, "unquote"); ok {
		if depth == 1 {
			value, err := evalSingleExpr(environment, arg, "quasiquote")
			return value, true, err
		}

		value, err := evalQuasiquoteExpr(environment, arg, depth-1)
		if err != nil {
			return nil, true, err
		}
		return properListFromSlice([]expr{symbolExpr{name: "unquote"}, value}), true, nil
	}

	if arg, ok := quasiquoteEscapeArg(form, "quasiquote"); ok {
		value, err := evalQuasiquoteExpr(environment, arg, depth+1)
		if err != nil {
			return nil, true, err
		}
		return properListFromSlice([]expr{symbolExpr{name: "quasiquote"}, value}), true, nil
	}

	if arg, ok := quasiquoteEscapeArg(form, "unquote-splicing"); ok {
		if depth == 1 {
			return nil, true, &EvalError{Message: "unquote-splicing is only valid within a list or vector quasiquote"}
		}

		value, err := evalQuasiquoteExpr(environment, arg, depth-1)
		if err != nil {
			return nil, true, err
		}
		return properListFromSlice([]expr{symbolExpr{name: "unquote-splicing"}, value}), true, nil
	}

	return nil, false, nil
}

func quasiquoteEscapeArg(form listExpr, name string) (expr, bool) {
	if len(form.items) != 2 {
		return nil, false
	}
	head, ok := form.items[0].(symbolExpr)
	if !ok || head.name != name {
		return nil, false
	}
	return form.items[1], true
}

func evalQuasiquoteList(environment *env, list listExpr, depth int) (expr, error) {
	items := list.items
	hasTail := false
	var tailForm expr
	if dotIndex, ok := quasiquoteDotIndex(items); ok {
		hasTail = true
		tailForm = items[len(items)-1]
		items = items[:dotIndex]
	}

	tail := expr(listExpr{})
	var err error
	if hasTail {
		tail, err = evalQuasiquoteExpr(environment, tailForm, depth)
		if err != nil {
			return nil, err
		}
	}

	for i := len(items) - 1; i >= 0; i-- {
		if arg, ok := quasiquoteEscapeArgFromExpr(items[i], "unquote-splicing"); ok && depth == 1 {
			value, err := evalSingleExpr(environment, arg, "quasiquote")
			if err != nil {
				return nil, err
			}

			spliced, ok := listElements(value)
			if !ok {
				return nil, &EvalError{Message: "unquote-splicing expects a list"}
			}
			for j := len(spliced) - 1; j >= 0; j-- {
				tail = &pairExpr{car: spliced[j], cdr: tail}
			}
			continue
		}

		value, err := evalQuasiquoteExpr(environment, items[i], depth)
		if err != nil {
			return nil, err
		}
		tail = &pairExpr{car: value, cdr: tail}
	}

	return tail, nil
}

func quasiquoteDotIndex(items []expr) (int, bool) {
	if len(items) < 3 {
		return 0, false
	}
	for i, item := range items {
		symbol, ok := item.(symbolExpr)
		if !ok || symbol.name != "." {
			continue
		}
		return i, i == len(items)-2
	}
	return 0, false
}

func quasiquoteEscapeArgFromExpr(form expr, name string) (expr, bool) {
	list, ok := form.(listExpr)
	if !ok {
		return nil, false
	}
	return quasiquoteEscapeArg(list, name)
}

func evalQuasiquoteVector(environment *env, vector *vectorExpr, depth int) (expr, error) {
	items := make([]expr, 0, len(vector.items))
	for _, item := range vector.items {
		if arg, ok := quasiquoteEscapeArgFromExpr(item, "unquote-splicing"); ok && depth == 1 {
			value, err := evalSingleExpr(environment, arg, "quasiquote")
			if err != nil {
				return nil, err
			}

			spliced, ok := listElements(value)
			if !ok {
				return nil, &EvalError{Message: "unquote-splicing expects a list"}
			}
			items = append(items, spliced...)
			continue
		}

		value, err := evalQuasiquoteExpr(environment, item, depth)
		if err != nil {
			return nil, err
		}
		items = append(items, value)
	}
	return &vectorExpr{items: items}, nil
}
