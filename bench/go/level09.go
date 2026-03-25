package ming

import (
	"strings"
	"unicode"
)

func builtinAbs(_ *interpreter, args []value, callPos position) (value, error) {
	if len(args) != 1 {
		return nil, wrongArgCount(callPos, "abs", "expected exactly 1 argument")
	}

	n, err := expectInt(args[0], callPos)
	if err != nil {
		return nil, err
	}
	if n < 0 {
		return -n, nil
	}
	return n, nil
}

func builtinModulo(_ *interpreter, args []value, callPos position) (value, error) {
	left, right, err := expectBinaryInts(args, callPos, "modulo")
	if err != nil {
		return nil, err
	}
	if right == 0 {
		return nil, newEvalError(ErrDivisionByZero, "division by zero", callPos)
	}

	result := left % right
	if result != 0 && ((result < 0 && right > 0) || (result > 0 && right < 0)) {
		result += right
	}
	return result, nil
}

func builtinRemainder(_ *interpreter, args []value, callPos position) (value, error) {
	left, right, err := expectBinaryInts(args, callPos, "remainder")
	if err != nil {
		return nil, err
	}
	if right == 0 {
		return nil, newEvalError(ErrDivisionByZero, "division by zero", callPos)
	}
	return left % right, nil
}

func builtinQuotient(_ *interpreter, args []value, callPos position) (value, error) {
	left, right, err := expectBinaryInts(args, callPos, "quotient")
	if err != nil {
		return nil, err
	}
	if right == 0 {
		return nil, newEvalError(ErrDivisionByZero, "division by zero", callPos)
	}
	return left / right, nil
}

func builtinMin(_ *interpreter, args []value, callPos position) (value, error) {
	return foldInts(args, callPos, "min", func(best, current int64) int64 {
		if current < best {
			return current
		}
		return best
	})
}

func builtinMax(_ *interpreter, args []value, callPos position) (value, error) {
	return foldInts(args, callPos, "max", func(best, current int64) int64 {
		if current > best {
			return current
		}
		return best
	})
}

func builtinExpt(_ *interpreter, args []value, callPos position) (value, error) {
	base, exponent, err := expectBinaryInts(args, callPos, "expt")
	if err != nil {
		return nil, err
	}
	if exponent < 0 {
		return nil, newEvalError(ErrOutOfRange, "expt: expected non-negative exponent", callPos)
	}

	result := int64(1)
	factor := base
	for exponent > 0 {
		if exponent%2 == 1 {
			result *= factor
		}
		exponent /= 2
		if exponent > 0 {
			factor *= factor
		}
	}
	return result, nil
}

func builtinZeroPred(_ *interpreter, args []value, callPos position) (value, error) {
	return unaryIntPred(args, callPos, "zero?", func(n int64) bool { return n == 0 })
}

func builtinPositivePred(_ *interpreter, args []value, callPos position) (value, error) {
	return unaryIntPred(args, callPos, "positive?", func(n int64) bool { return n > 0 })
}

func builtinNegativePred(_ *interpreter, args []value, callPos position) (value, error) {
	return unaryIntPred(args, callPos, "negative?", func(n int64) bool { return n < 0 })
}

func builtinOddPred(_ *interpreter, args []value, callPos position) (value, error) {
	return unaryIntPred(args, callPos, "odd?", func(n int64) bool { return n%2 != 0 })
}

func builtinEvenPred(_ *interpreter, args []value, callPos position) (value, error) {
	return unaryIntPred(args, callPos, "even?", func(n int64) bool { return n%2 == 0 })
}

func builtinEqPred(_ *interpreter, args []value, callPos position) (value, error) {
	if len(args) != 2 {
		return nil, wrongArgCount(callPos, "eq?", "expected exactly 2 arguments")
	}
	return eqValue(args[0], args[1]), nil
}

func builtinDeepEqualPred(_ *interpreter, args []value, callPos position) (value, error) {
	if len(args) != 2 {
		return nil, wrongArgCount(callPos, "equal?", "expected exactly 2 arguments")
	}
	return deepEqual(args[0], args[1]), nil
}

func builtinListPred(_ *interpreter, args []value, callPos position) (value, error) {
	if len(args) != 1 {
		return nil, wrongArgCount(callPos, "list?", "expected exactly 1 argument")
	}
	return isProperList(args[0]), nil
}

func builtinListRef(_ *interpreter, args []value, callPos position) (value, error) {
	if len(args) != 2 {
		return nil, wrongArgCount(callPos, "list-ref", "expected exactly 2 arguments")
	}

	index, err := expectIndex(args[1], callPos)
	if err != nil {
		return nil, err
	}

	target, err := listTailAt(args[0], index, "list-ref", callPos)
	if err != nil {
		return nil, err
	}

	pair, ok := target.(*pairValue)
	if !ok {
		if _, ok := target.(emptyListValue); ok {
			return nil, newEvalError(ErrOutOfRange, "list-ref: index out of range", callPos)
		}
		return nil, newEvalError(ErrTypeMismatch, "list-ref: expected list", callPos)
	}
	return pair.car, nil
}

func builtinListTail(_ *interpreter, args []value, callPos position) (value, error) {
	if len(args) != 2 {
		return nil, wrongArgCount(callPos, "list-tail", "expected exactly 2 arguments")
	}

	index, err := expectIndex(args[1], callPos)
	if err != nil {
		return nil, err
	}
	if err := ensureListLike(args[0], "list-tail", callPos); err != nil {
		return nil, err
	}

	return listTailAt(args[0], index, "list-tail", callPos)
}

func builtinAssoc(_ *interpreter, args []value, callPos position) (value, error) {
	if len(args) != 2 {
		return nil, wrongArgCount(callPos, "assoc", "expected exactly 2 arguments")
	}

	current := args[1]
	for {
		switch list := current.(type) {
		case emptyListValue:
			return false, nil
		case *pairValue:
			entry, ok := list.car.(*pairValue)
			if !ok {
				return nil, newEvalError(ErrTypeMismatch, "assoc: expected list of pairs", callPos)
			}
			if deepEqual(args[0], entry.car) {
				return list.car, nil
			}
			current = list.cdr
		default:
			return nil, newEvalError(ErrTypeMismatch, "assoc: expected list", callPos)
		}
	}
}

func builtinMap(it *interpreter, args []value, callPos position) (value, error) {
	if len(args) < 2 {
		return nil, wrongArgCount(callPos, "map", "expected at least 2 arguments")
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
			return nil, newEvalError(ErrWrongArgCount, "map: expected lists of equal length", callPos)
		}
		argLists = append(argLists, items)
	}

	results := make([]value, 0, expectedLen)
	callArgs := make([]value, len(argLists))
	for i := 0; i < expectedLen; i++ {
		for j, items := range argLists {
			callArgs[j] = items[i]
		}
		current, err := it.applyProcedure(args[0], callArgs, callPos)
		if err != nil {
			return nil, err
		}
		results = append(results, current)
	}

	return buildList(results), nil
}

func builtinCharAlphabeticPred(_ *interpreter, args []value, callPos position) (value, error) {
	return unaryCharPred(args, callPos, "char-alphabetic?", func(ch rune) bool { return unicode.IsLetter(ch) })
}

func builtinCharNumericPred(_ *interpreter, args []value, callPos position) (value, error) {
	return unaryCharPred(args, callPos, "char-numeric?", func(ch rune) bool { return unicode.IsDigit(ch) })
}

func builtinCharUpcase(_ *interpreter, args []value, callPos position) (value, error) {
	if len(args) != 1 {
		return nil, wrongArgCount(callPos, "char-upcase", "expected exactly 1 argument")
	}

	ch, err := expectChar(args[0], callPos)
	if err != nil {
		return nil, err
	}
	return charValue(unicode.ToUpper(rune(ch))), nil
}

func builtinCharDowncase(_ *interpreter, args []value, callPos position) (value, error) {
	if len(args) != 1 {
		return nil, wrongArgCount(callPos, "char-downcase", "expected exactly 1 argument")
	}

	ch, err := expectChar(args[0], callPos)
	if err != nil {
		return nil, err
	}
	return charValue(unicode.ToLower(rune(ch))), nil
}

func builtinCharEqualPred(_ *interpreter, args []value, callPos position) (value, error) {
	return compareChars(args, callPos, "char=?", func(left, right rune) bool { return left == right })
}

func builtinCharLessPred(_ *interpreter, args []value, callPos position) (value, error) {
	return compareChars(args, callPos, "char<?", func(left, right rune) bool { return left < right })
}

func builtinStringEqualPred(_ *interpreter, args []value, callPos position) (value, error) {
	return compareStrings(args, callPos, "string=?", func(left, right string) bool { return left == right })
}

func builtinStringLessPred(_ *interpreter, args []value, callPos position) (value, error) {
	return compareStrings(args, callPos, "string<?", func(left, right string) bool { return left < right })
}

func builtinStringCIEqualPred(_ *interpreter, args []value, callPos position) (value, error) {
	return compareStrings(args, callPos, "string-ci=?", strings.EqualFold)
}

func builtinStringUpcase(_ *interpreter, args []value, callPos position) (value, error) {
	if len(args) != 1 {
		return nil, wrongArgCount(callPos, "string-upcase", "expected exactly 1 argument")
	}

	s, err := expectString(args[0], callPos)
	if err != nil {
		return nil, err
	}
	return newStringValue(strings.ToUpper(s.text())), nil
}

func builtinStringDowncase(_ *interpreter, args []value, callPos position) (value, error) {
	if len(args) != 1 {
		return nil, wrongArgCount(callPos, "string-downcase", "expected exactly 1 argument")
	}

	s, err := expectString(args[0], callPos)
	if err != nil {
		return nil, err
	}
	return newStringValue(strings.ToLower(s.text())), nil
}

func expectBinaryInts(args []value, callPos position, name string) (int64, int64, error) {
	if len(args) != 2 {
		return 0, 0, wrongArgCount(callPos, name, "expected exactly 2 arguments")
	}

	left, err := expectInt(args[0], callPos)
	if err != nil {
		return 0, 0, err
	}
	right, err := expectInt(args[1], callPos)
	if err != nil {
		return 0, 0, err
	}
	return left, right, nil
}

func foldInts(args []value, callPos position, name string, combine func(int64, int64) int64) (value, error) {
	if len(args) == 0 {
		return nil, wrongArgCount(callPos, name, "expected at least 1 argument")
	}

	best, err := expectInt(args[0], callPos)
	if err != nil {
		return nil, err
	}
	for _, arg := range args[1:] {
		current, err := expectInt(arg, callPos)
		if err != nil {
			return nil, err
		}
		best = combine(best, current)
	}
	return best, nil
}

func unaryIntPred(args []value, callPos position, name string, pred func(int64) bool) (value, error) {
	if len(args) != 1 {
		return nil, wrongArgCount(callPos, name, "expected exactly 1 argument")
	}

	n, err := expectInt(args[0], callPos)
	if err != nil {
		return nil, err
	}
	return pred(n), nil
}

func unaryCharPred(args []value, callPos position, name string, pred func(rune) bool) (value, error) {
	if len(args) != 1 {
		return nil, wrongArgCount(callPos, name, "expected exactly 1 argument")
	}

	ch, err := expectChar(args[0], callPos)
	if err != nil {
		return nil, err
	}
	return pred(rune(ch)), nil
}

func compareChars(args []value, callPos position, name string, cmp func(rune, rune) bool) (value, error) {
	if len(args) < 2 {
		return nil, wrongArgCount(callPos, name, "expected at least 2 arguments")
	}

	prev, err := expectChar(args[0], callPos)
	if err != nil {
		return nil, err
	}
	prevRune := rune(prev)
	for _, arg := range args[1:] {
		current, err := expectChar(arg, callPos)
		if err != nil {
			return nil, err
		}
		currentRune := rune(current)
		if !cmp(prevRune, currentRune) {
			return false, nil
		}
		prevRune = currentRune
	}
	return true, nil
}

func compareStrings(args []value, callPos position, name string, cmp func(string, string) bool) (value, error) {
	if len(args) < 2 {
		return nil, wrongArgCount(callPos, name, "expected at least 2 arguments")
	}

	prev, err := expectString(args[0], callPos)
	if err != nil {
		return nil, err
	}
	prevText := prev.text()
	for _, arg := range args[1:] {
		current, err := expectString(arg, callPos)
		if err != nil {
			return nil, err
		}
		currentText := current.text()
		if !cmp(prevText, currentText) {
			return false, nil
		}
		prevText = currentText
	}
	return true, nil
}

func isProperList(v value) bool {
	for {
		switch current := v.(type) {
		case emptyListValue:
			return true
		case *pairValue:
			v = current.cdr
		default:
			return false
		}
	}
}

func ensureListLike(v value, name string, callPos position) error {
	switch v.(type) {
	case emptyListValue, *pairValue:
		return nil
	default:
		return newEvalError(ErrTypeMismatch, name+": expected list", callPos)
	}
}

func listTailAt(v value, index int, name string, callPos position) (value, error) {
	current := v
	for i := 0; i < index; i++ {
		pair, ok := current.(*pairValue)
		if !ok {
			if _, ok := current.(emptyListValue); ok {
				return nil, newEvalError(ErrOutOfRange, name+": index out of range", callPos)
			}
			return nil, newEvalError(ErrTypeMismatch, name+": expected list", callPos)
		}
		current = pair.cdr
	}
	return current, nil
}

func eqValue(left value, right value) bool {
	switch left := left.(type) {
	case int64:
		right, ok := right.(int64)
		return ok && left == right
	case rationalValue:
		right, ok := right.(rationalValue)
		return ok && left == right
	case inexactValue:
		right, ok := right.(inexactValue)
		return ok && left == right
	case bool:
		right, ok := right.(bool)
		return ok && left == right
	case symbolValue:
		right, ok := right.(symbolValue)
		return ok && left == right
	case charValue:
		right, ok := right.(charValue)
		return ok && left == right
	case emptyListValue:
		_, ok := right.(emptyListValue)
		return ok
	case *stringValue:
		right, ok := right.(*stringValue)
		return ok && left == right
	case *pairValue:
		right, ok := right.(*pairValue)
		return ok && left == right
	case *builtinProc:
		right, ok := right.(*builtinProc)
		return ok && left == right
	case *closureProc:
		right, ok := right.(*closureProc)
		return ok && left == right
	case *recordValue:
		right, ok := right.(*recordValue)
		return ok && left == right
	case voidValue:
		_, ok := right.(voidValue)
		return ok
	default:
		return false
	}
}

func deepEqual(left value, right value) bool {
	if isNumberValue(left) && isNumberValue(right) {
		return numberEqual(left, right)
	}

	switch left := left.(type) {
	case int64:
		right, ok := right.(int64)
		return ok && left == right
	case bool:
		right, ok := right.(bool)
		return ok && left == right
	case symbolValue:
		right, ok := right.(symbolValue)
		return ok && left == right
	case charValue:
		right, ok := right.(charValue)
		return ok && left == right
	case *stringValue:
		right, ok := right.(*stringValue)
		return ok && left.text() == right.text()
	case emptyListValue:
		_, ok := right.(emptyListValue)
		return ok
	case *pairValue:
		right, ok := right.(*pairValue)
		return ok && deepEqual(left.car, right.car) && deepEqual(left.cdr, right.cdr)
	default:
		return eqValue(left, right)
	}
}
