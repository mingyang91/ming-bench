package ming

import "unicode/utf8"

func builtinStringToList(_ *interpreter, args []value, callPos position) (value, error) {
	if len(args) != 1 {
		return nil, wrongArgCount(callPos, "string->list", "expected exactly 1 argument")
	}

	s, err := expectString(args[0], callPos)
	if err != nil {
		return nil, err
	}

	items := make([]value, len(s.chars))
	for i, ch := range s.chars {
		items[i] = charValue(ch)
	}
	return buildList(items), nil
}

func builtinListToString(_ *interpreter, args []value, callPos position) (value, error) {
	if len(args) != 1 {
		return nil, wrongArgCount(callPos, "list->string", "expected exactly 1 argument")
	}

	items, err := listToSlice(args[0], callPos)
	if err != nil {
		return nil, err
	}

	chars := make([]rune, len(items))
	for i, item := range items {
		ch, err := expectChar(item, callPos)
		if err != nil {
			return nil, err
		}
		chars[i] = rune(ch)
	}

	return newStringValue(string(chars)), nil
}

func builtinCharToInteger(_ *interpreter, args []value, callPos position) (value, error) {
	if len(args) != 1 {
		return nil, wrongArgCount(callPos, "char->integer", "expected exactly 1 argument")
	}

	ch, err := expectChar(args[0], callPos)
	if err != nil {
		return nil, err
	}

	return int64(ch), nil
}

func builtinIntegerToChar(_ *interpreter, args []value, callPos position) (value, error) {
	if len(args) != 1 {
		return nil, wrongArgCount(callPos, "integer->char", "expected exactly 1 argument")
	}

	n, err := expectExactInteger(args[0], callPos, "integer->char")
	if err != nil {
		return nil, err
	}

	ch := rune(n)
	if int64(ch) != n || !utf8.ValidRune(ch) {
		return nil, newEvalError(ErrOutOfRange, "integer->char: invalid code point", callPos)
	}

	return charValue(ch), nil
}

func expectExactInteger(v value, pos position, name string) (int64, error) {
	switch n := v.(type) {
	case int64:
		return n, nil
	case rationalValue:
		if n.den == 1 {
			return n.num, nil
		}
	}

	return 0, newEvalError(ErrTypeMismatch, name+": expected exact integer", pos)
}
