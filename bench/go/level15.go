package ming

import "unicode/utf8"

func builtinStringToList(args []expr) (expr, error) {
	if len(args) != 1 {
		return nil, &EvalError{Message: "string->list expects exactly 1 argument"}
	}

	text, ok := asString(args[0])
	if !ok {
		return nil, &EvalError{Message: "string->list expects a string"}
	}

	items := make([]expr, len(text.runes))
	for i, r := range text.runes {
		items[i] = charExpr(r)
	}
	return listExpr{items: items}, nil
}

func builtinListToString(args []expr) (expr, error) {
	if len(args) != 1 {
		return nil, &EvalError{Message: "list->string expects exactly 1 argument"}
	}

	list, ok := args[0].(listExpr)
	if !ok {
		return nil, &EvalError{Message: "list->string expects a list"}
	}

	runes := make([]rune, len(list.items))
	for i, item := range list.items {
		ch, ok := item.(charExpr)
		if !ok {
			return nil, &EvalError{Message: "list->string expects a list of characters"}
		}
		runes[i] = rune(ch)
	}

	return newAllocatedString(string(runes)), nil
}

func builtinCharToInteger(args []expr) (expr, error) {
	ch, err := unaryCharArg(args, "char->integer")
	if err != nil {
		return nil, err
	}
	return intExpr(ch), nil
}

func builtinIntegerToChar(args []expr) (expr, error) {
	if len(args) != 1 {
		return nil, &EvalError{Message: "integer->char expects exactly 1 argument"}
	}

	value, ok := exactIntegerValue(args[0])
	if !ok {
		return nil, &EvalError{Message: "integer->char expects an exact integer"}
	}
	if value < 0 || value > utf8.MaxRune {
		return nil, &EvalError{Message: "integer->char expects a valid character code"}
	}
	if value >= 0xD800 && value <= 0xDFFF {
		return nil, &EvalError{Message: "integer->char expects a valid character code"}
	}

	return charExpr(rune(value)), nil
}
