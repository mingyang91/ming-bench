package ming

import (
	"math"
	"strings"
)

func builtinEqvPred(_ *interpreter, args []value, callPos position) (value, error) {
	if len(args) != 2 {
		return nil, wrongArgCount(callPos, "eqv?", "expected exactly 2 arguments")
	}
	return caseDatumEqual(args[0], args[1]), nil
}

func builtinSetCar(_ *interpreter, args []value, callPos position) (value, error) {
	if len(args) != 2 {
		return nil, wrongArgCount(callPos, "set-car!", "expected exactly 2 arguments")
	}

	pair, err := expectPair(args[0], callPos, "set-car!")
	if err != nil {
		return nil, err
	}
	pair.car = args[1]
	return voidValue{}, nil
}

func builtinSetCdr(_ *interpreter, args []value, callPos position) (value, error) {
	if len(args) != 2 {
		return nil, wrongArgCount(callPos, "set-cdr!", "expected exactly 2 arguments")
	}

	pair, err := expectPair(args[0], callPos, "set-cdr!")
	if err != nil {
		return nil, err
	}
	pair.cdr = args[1]
	return voidValue{}, nil
}

func builtinCaar(_ *interpreter, args []value, callPos position) (value, error) {
	if len(args) != 1 {
		return nil, wrongArgCount(callPos, "caar", "expected exactly 1 argument")
	}

	pair, err := expectPair(args[0], callPos, "caar")
	if err != nil {
		return nil, err
	}
	inner, err := expectPair(pair.car, callPos, "caar")
	if err != nil {
		return nil, err
	}
	return inner.car, nil
}

func builtinCadr(_ *interpreter, args []value, callPos position) (value, error) {
	if len(args) != 1 {
		return nil, wrongArgCount(callPos, "cadr", "expected exactly 1 argument")
	}

	pair, err := expectPair(args[0], callPos, "cadr")
	if err != nil {
		return nil, err
	}
	inner, err := expectPair(pair.cdr, callPos, "cadr")
	if err != nil {
		return nil, err
	}
	return inner.car, nil
}

func builtinCdar(_ *interpreter, args []value, callPos position) (value, error) {
	if len(args) != 1 {
		return nil, wrongArgCount(callPos, "cdar", "expected exactly 1 argument")
	}

	pair, err := expectPair(args[0], callPos, "cdar")
	if err != nil {
		return nil, err
	}
	inner, err := expectPair(pair.car, callPos, "cdar")
	if err != nil {
		return nil, err
	}
	return inner.cdr, nil
}

func builtinCddr(_ *interpreter, args []value, callPos position) (value, error) {
	if len(args) != 1 {
		return nil, wrongArgCount(callPos, "cddr", "expected exactly 1 argument")
	}

	pair, err := expectPair(args[0], callPos, "cddr")
	if err != nil {
		return nil, err
	}
	inner, err := expectPair(pair.cdr, callPos, "cddr")
	if err != nil {
		return nil, err
	}
	return inner.cdr, nil
}

func builtinReverse(_ *interpreter, args []value, callPos position) (value, error) {
	if len(args) != 1 {
		return nil, wrongArgCount(callPos, "reverse", "expected exactly 1 argument")
	}

	items, err := listToSlice(args[0], callPos)
	if err != nil {
		return nil, err
	}

	result := value(emptyListValue{})
	for _, item := range items {
		result = &pairValue{car: item, cdr: result}
	}
	return result, nil
}

func builtinMember(_ *interpreter, args []value, callPos position) (value, error) {
	if len(args) != 2 {
		return nil, wrongArgCount(callPos, "member", "expected exactly 2 arguments")
	}

	current := args[1]
	seen := map[*pairValue]struct{}{}
	for {
		switch list := current.(type) {
		case emptyListValue:
			return false, nil
		case *pairValue:
			if _, ok := seen[list]; ok {
				return false, nil
			}
			seen[list] = struct{}{}
			if deepEqual(args[0], list.car) {
				return current, nil
			}
			current = list.cdr
		default:
			return nil, newEvalError(ErrTypeMismatch, "member: expected list", callPos)
		}
	}
}

func builtinAssv(_ *interpreter, args []value, callPos position) (value, error) {
	if len(args) != 2 {
		return nil, wrongArgCount(callPos, "assv", "expected exactly 2 arguments")
	}

	current := args[1]
	seen := map[*pairValue]struct{}{}
	for {
		switch list := current.(type) {
		case emptyListValue:
			return false, nil
		case *pairValue:
			if _, ok := seen[list]; ok {
				return false, nil
			}
			seen[list] = struct{}{}

			entry, ok := list.car.(*pairValue)
			if !ok {
				return nil, newEvalError(ErrTypeMismatch, "assv: expected list of pairs", callPos)
			}
			if caseDatumEqual(args[0], entry.car) {
				return list.car, nil
			}
			current = list.cdr
		default:
			return nil, newEvalError(ErrTypeMismatch, "assv: expected list", callPos)
		}
	}
}

func builtinGCD(_ *interpreter, args []value, callPos position) (value, error) {
	if len(args) == 0 {
		return int64(0), nil
	}

	result, err := expectInt(args[0], callPos)
	if err != nil {
		return nil, err
	}
	result = absInt64(result)

	for _, arg := range args[1:] {
		current, err := expectInt(arg, callPos)
		if err != nil {
			return nil, err
		}
		result = gcdInt64(result, current)
	}

	return absInt64(result), nil
}

func builtinLCM(_ *interpreter, args []value, callPos position) (value, error) {
	if len(args) == 0 {
		return int64(1), nil
	}

	result, err := expectInt(args[0], callPos)
	if err != nil {
		return nil, err
	}
	result = absInt64(result)

	for _, arg := range args[1:] {
		current, err := expectInt(arg, callPos)
		if err != nil {
			return nil, err
		}
		current = absInt64(current)
		if result == 0 || current == 0 {
			result = 0
			continue
		}
		result = absInt64(result/gcdInt64(result, current)) * current
	}

	return result, nil
}

func builtinTruncate(_ *interpreter, args []value, callPos position) (value, error) {
	if len(args) != 1 {
		return nil, wrongArgCount(callPos, "truncate", "expected exactly 1 argument")
	}

	switch n := args[0].(type) {
	case int64:
		return n, nil
	case rationalValue:
		return n.num / n.den, nil
	case inexactValue:
		return inexactValue(math.Trunc(float64(n))), nil
	default:
		return nil, newEvalError(ErrTypeMismatch, "truncate: expected number", callPos)
	}
}

func builtinRound(_ *interpreter, args []value, callPos position) (value, error) {
	if len(args) != 1 {
		return nil, wrongArgCount(callPos, "round", "expected exactly 1 argument")
	}

	switch n := args[0].(type) {
	case int64:
		return n, nil
	case rationalValue:
		return int64(math.Round(float64(n.num) / float64(n.den))), nil
	case inexactValue:
		return inexactValue(math.Round(float64(n))), nil
	default:
		return nil, newEvalError(ErrTypeMismatch, "round: expected number", callPos)
	}
}

func builtinMakeString(_ *interpreter, args []value, callPos position) (value, error) {
	if len(args) != 1 && len(args) != 2 {
		return nil, wrongArgCount(callPos, "make-string", "expected 1 or 2 arguments")
	}

	length, err := expectIndex(args[0], callPos)
	if err != nil {
		return nil, err
	}

	fill := rune(0)
	if len(args) == 2 {
		ch, err := expectChar(args[1], callPos)
		if err != nil {
			return nil, err
		}
		fill = rune(ch)
	}

	chars := make([]rune, length)
	for i := range chars {
		chars[i] = fill
	}

	return &stringValue{
		chars:   chars,
		mutable: stringsMutableInCurrentLevel(),
	}, nil
}

func builtinString(_ *interpreter, args []value, callPos position) (value, error) {
	chars := make([]rune, len(args))
	for i, arg := range args {
		ch, err := expectChar(arg, callPos)
		if err != nil {
			return nil, err
		}
		chars[i] = rune(ch)
	}
	return newStringValue(string(chars)), nil
}

func builtinStringGreaterPred(_ *interpreter, args []value, callPos position) (value, error) {
	return compareStrings(args, callPos, "string>?", func(left, right string) bool { return left > right })
}

func builtinStringLessEqualPred(_ *interpreter, args []value, callPos position) (value, error) {
	return compareStrings(args, callPos, "string<=?", func(left, right string) bool { return left <= right })
}

func builtinStringGreaterEqualPred(_ *interpreter, args []value, callPos position) (value, error) {
	return compareStrings(args, callPos, "string>=?", func(left, right string) bool { return left >= right })
}

func builtinForEach(it *interpreter, args []value, callPos position) (value, error) {
	if len(args) < 2 {
		return nil, wrongArgCount(callPos, "for-each", "expected at least 2 arguments")
	}

	argLists := make([][]value, 0, len(args)-1)
	expectedLen := -1
	for _, arg := range args[1:] {
		items, err := listToSlice(arg, callPos)
		if err != nil {
			return nil, err
		}
		if expectedLen == -1 {
			expectedLen = len(items)
		} else if len(items) != expectedLen {
			return nil, newEvalError(ErrWrongArgCount, "for-each: expected lists of equal length", callPos)
		}
		argLists = append(argLists, items)
	}

	callArgs := make([]value, len(argLists))
	for i := 0; i < expectedLen; i++ {
		for j, items := range argLists {
			callArgs[j] = items[i]
		}
		if _, err := it.applyProcedure(args[0], callArgs, callPos); err != nil {
			return nil, err
		}
	}

	return voidValue{}, nil
}

func builtinListToVector(_ *interpreter, args []value, callPos position) (value, error) {
	if len(args) != 1 {
		return nil, wrongArgCount(callPos, "list->vector", "expected exactly 1 argument")
	}

	items, err := listToSlice(args[0], callPos)
	if err != nil {
		return nil, err
	}
	return &vectorValue{elements: append([]value(nil), items...)}, nil
}

func (it *interpreter) evalLetStar(scope *env, list *listExpr) (value, error) {
	if len(list.elements) < 3 {
		return nil, newEvalError(ErrSyntax, "let*: expected bindings and body", list.at)
	}

	bindingList, ok := list.elements[1].(*listExpr)
	if !ok {
		return nil, newEvalError(ErrSyntax, "let*: expected binding list", list.elements[1].pos())
	}

	bindings, err := parseLetBindings(bindingList)
	if err != nil {
		return nil, err
	}

	body := list.elements[2:]
	if len(body) == 0 {
		return nil, newEvalError(ErrSyntax, "let*: expected body", list.at)
	}

	letEnv := newEnv(scope)
	for _, binding := range bindings {
		current, err := it.eval(binding.init, letEnv)
		if err != nil {
			return nil, err
		}
		it.defineBindingName(letEnv, binding.name, current)
	}

	return it.evalSequence(letEnv, body)
}

func (it *interpreter) prepareTailLetStar(scope *env, list *listExpr) (evalStep, error) {
	if len(list.elements) < 3 {
		return evalStep{}, newEvalError(ErrSyntax, "let*: expected bindings and body", list.at)
	}

	bindingList, ok := list.elements[1].(*listExpr)
	if !ok {
		return evalStep{}, newEvalError(ErrSyntax, "let*: expected binding list", list.elements[1].pos())
	}

	bindings, err := parseLetBindings(bindingList)
	if err != nil {
		return evalStep{}, err
	}

	body := list.elements[2:]
	if len(body) == 0 {
		return evalStep{}, newEvalError(ErrSyntax, "let*: expected body", list.at)
	}

	letEnv := newEnv(scope)
	for _, binding := range bindings {
		current, err := it.eval(binding.init, letEnv)
		if err != nil {
			return evalStep{}, err
		}
		it.defineBindingName(letEnv, binding.name, current)
	}

	return it.prepareTailSequence(letEnv, body)
}

func builtinError(_ *interpreter, args []value, callPos position) (value, error) {
	if len(args) == 0 {
		return nil, wrongArgCount(callPos, "error", "expected at least 1 argument")
	}

	parts := make([]string, 0, len(args))
	for _, arg := range args {
		formatted, err := formatDisplayValue(arg)
		if err != nil {
			return nil, err
		}
		parts = append(parts, formatted)
	}
	return nil, newEvalError(ErrRaised, strings.Join(parts, " "), callPos)
}

func expectPair(v value, pos position, name string) (*pairValue, error) {
	pair, ok := v.(*pairValue)
	if !ok {
		return nil, newEvalError(ErrTypeMismatch, name+": expected pair", pos)
	}
	return pair, nil
}

func absInt64(n int64) int64 {
	if n < 0 {
		return -n
	}
	return n
}
