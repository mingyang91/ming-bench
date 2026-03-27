package ming

import "unicode/utf8"

func registerLevel15Builtins(global *env) {
	global.define("string->list", builtinProc{name: "string->list", fn: evalStringToList})
	global.define("list->string", builtinProc{name: "list->string", fn: evalListToString})
	global.define("char->integer", builtinProc{name: "char->integer", fn: evalCharToInteger})
	global.define("integer->char", builtinProc{name: "integer->char", fn: evalIntegerToChar})
}

func evalStringToList(args []value) (value, error) {
	if len(args) != 1 {
		return nil, newCurrentEvalError("'string->list' expects exactly 1 argument")
	}

	text, err := expectString(args[0])
	if err != nil {
		return nil, err
	}

	runes := []rune(text)
	items := make([]value, len(runes))
	for i, ch := range runes {
		items[i] = charValue(ch)
	}
	return listFromValues(items), nil
}

func evalListToString(args []value) (value, error) {
	if len(args) != 1 {
		return nil, newCurrentEvalError("'list->string' expects exactly 1 argument")
	}

	items, err := properListElements(args[0])
	if err != nil {
		return nil, err
	}

	runes := make([]rune, len(items))
	for i, item := range items {
		ch, err := expectChar(item)
		if err != nil {
			return nil, err
		}
		runes[i] = ch
	}
	return newStringValue(string(runes)), nil
}

func evalCharToInteger(args []value) (value, error) {
	if len(args) != 1 {
		return nil, newCurrentEvalError("'char->integer' expects exactly 1 argument")
	}

	ch, err := expectChar(args[0])
	if err != nil {
		return nil, err
	}
	return newExactInteger(int(ch)), nil
}

func evalIntegerToChar(args []value) (value, error) {
	if len(args) != 1 {
		return nil, newCurrentEvalError("'integer->char' expects exactly 1 argument")
	}

	codepoint, err := expectInteger(args[0])
	if err != nil {
		return nil, err
	}

	ch := rune(codepoint)
	if !utf8.ValidRune(ch) {
		return nil, newCurrentEvalError("'integer->char' expects a valid character code point")
	}

	return charValue(ch), nil
}
