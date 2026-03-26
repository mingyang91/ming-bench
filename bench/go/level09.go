package ming

import (
	"fmt"
	"strings"
	"unicode"
)

func builtinAbs(args []any) (any, error) {
	if len(args) != 1 {
		return nil, &EvalError{Message: "abs expects exactly 1 argument"}
	}

	n, err := expectInt(args[0])
	if err != nil {
		return nil, err
	}
	if n < 0 {
		return -n, nil
	}
	return n, nil
}

func builtinModulo(args []any) (any, error) {
	dividend, divisor, err := expectTwoInts("modulo", args)
	if err != nil {
		return nil, err
	}
	if divisor == 0 {
		return nil, &EvalError{Message: "division by zero"}
	}

	result := dividend % divisor
	if result != 0 && (result < 0) != (divisor < 0) {
		result += divisor
	}
	return result, nil
}

func builtinRemainder(args []any) (any, error) {
	dividend, divisor, err := expectTwoInts("remainder", args)
	if err != nil {
		return nil, err
	}
	if divisor == 0 {
		return nil, &EvalError{Message: "division by zero"}
	}
	return dividend % divisor, nil
}

func builtinQuotient(args []any) (any, error) {
	dividend, divisor, err := expectTwoInts("quotient", args)
	if err != nil {
		return nil, err
	}
	if divisor == 0 {
		return nil, &EvalError{Message: "division by zero"}
	}
	return dividend / divisor, nil
}

func builtinMin(args []any) (any, error) {
	return builtinExtremum("min", args, func(current, next int64) bool {
		return next < current
	})
}

func builtinMax(args []any) (any, error) {
	return builtinExtremum("max", args, func(current, next int64) bool {
		return next > current
	})
}

func builtinExpt(args []any) (any, error) {
	base, exponent, err := expectTwoInts("expt", args)
	if err != nil {
		return nil, err
	}
	if exponent < 0 {
		return nil, &EvalError{Message: "expt expects a non-negative exponent"}
	}

	result := int64(1)
	for exponent > 0 {
		if exponent&1 == 1 {
			result *= base
		}
		base *= base
		exponent >>= 1
	}
	return result, nil
}

func builtinZero(args []any) (any, error) {
	return builtinNumericPredicateValue("zero?", args, func(n int64) bool { return n == 0 })
}

func builtinPositive(args []any) (any, error) {
	return builtinNumericPredicateValue("positive?", args, func(n int64) bool { return n > 0 })
}

func builtinNegative(args []any) (any, error) {
	return builtinNumericPredicateValue("negative?", args, func(n int64) bool { return n < 0 })
}

func builtinOdd(args []any) (any, error) {
	return builtinNumericPredicateValue("odd?", args, func(n int64) bool { return n%2 != 0 })
}

func builtinEven(args []any) (any, error) {
	return builtinNumericPredicateValue("even?", args, func(n int64) bool { return n%2 == 0 })
}

func builtinListRef(args []any) (any, error) {
	if len(args) != 2 {
		return nil, &EvalError{Message: "list-ref expects exactly 2 arguments"}
	}

	index, err := expectNonNegativeIndex(args[1], "list-ref")
	if err != nil {
		return nil, err
	}

	tail, err := listTailValue(args[0], index, "list-ref")
	if err != nil {
		return nil, err
	}

	pair, ok := tail.(pairValue)
	if !ok {
		return nil, &EvalError{Message: "list-ref index out of range"}
	}
	return pair.car, nil
}

func builtinListTail(args []any) (any, error) {
	if len(args) != 2 {
		return nil, &EvalError{Message: "list-tail expects exactly 2 arguments"}
	}

	index, err := expectNonNegativeIndex(args[1], "list-tail")
	if err != nil {
		return nil, err
	}

	return listTailValue(args[0], index, "list-tail")
}

func builtinListP(args []any) (any, error) {
	if len(args) != 1 {
		return nil, &EvalError{Message: "list? expects exactly 1 argument"}
	}
	return isProperList(args[0]), nil
}

func builtinEq(args []any) (any, error) {
	if len(args) != 2 {
		return nil, &EvalError{Message: "eq? expects exactly 2 arguments"}
	}
	return valuesEq(args[0], args[1]), nil
}

func builtinEqual(args []any) (any, error) {
	if len(args) != 2 {
		return nil, &EvalError{Message: "equal? expects exactly 2 arguments"}
	}
	return valuesEqual(args[0], args[1]), nil
}

func builtinAssoc(args []any) (any, error) {
	if len(args) != 2 {
		return nil, &EvalError{Message: "assoc expects exactly 2 arguments"}
	}

	key := args[0]
	current := args[1]
	for {
		switch list := current.(type) {
		case emptyListValue:
			return false, nil
		case pairValue:
			entry, ok := list.car.(pairValue)
			if !ok {
				return nil, &EvalError{Message: "assoc expects an association list"}
			}
			if valuesEqual(key, entry.car) {
				return entry, nil
			}
			current = list.cdr
		default:
			return nil, &EvalError{Message: fmt.Sprintf("assoc expects a list, got %s", typeName(args[1]))}
		}
	}
}

func builtinMap(args []any) (any, error) {
	if len(args) < 2 {
		return nil, &EvalError{Message: "map expects at least 2 arguments"}
	}

	proc := args[0]
	lists := make([][]any, len(args)-1)
	expectedLen := -1
	for i, listArg := range args[1:] {
		elements, err := properListElements(listArg, "map")
		if err != nil {
			return nil, err
		}
		if expectedLen < 0 {
			expectedLen = len(elements)
		} else if len(elements) != expectedLen {
			return nil, &EvalError{Message: "map expects lists of equal length"}
		}
		lists[i] = elements
	}

	results := make([]any, expectedLen)
	callArgs := make([]any, len(lists))
	for i := 0; i < expectedLen; i++ {
		for j := range lists {
			callArgs[j] = lists[j][i]
		}
		value, err := applyProcedure(proc, callArgs)
		if err != nil {
			return nil, err
		}
		results[i] = value
	}

	return makeListValue(results), nil
}

func builtinCharAlphabetic(args []any) (any, error) {
	return builtinCharPredicateValue("char-alphabetic?", args, unicode.IsLetter)
}

func builtinCharNumeric(args []any) (any, error) {
	return builtinCharPredicateValue("char-numeric?", args, unicode.IsDigit)
}

func builtinCharUpcase(args []any) (any, error) {
	return builtinCharTransform("char-upcase", args, unicode.ToUpper)
}

func builtinCharDowncase(args []any) (any, error) {
	return builtinCharTransform("char-downcase", args, unicode.ToLower)
}

func builtinCharEqual(args []any) (any, error) {
	return builtinCharCompare("char=?", args, func(left, right rune) bool { return left == right })
}

func builtinCharLess(args []any) (any, error) {
	return builtinCharCompare("char<?", args, func(left, right rune) bool { return left < right })
}

func builtinStringEqual(args []any) (any, error) {
	return builtinStringCompare("string=?", args, func(left, right string) bool { return left == right })
}

func builtinStringLess(args []any) (any, error) {
	return builtinStringCompare("string<?", args, func(left, right string) bool {
		return strings.Compare(left, right) < 0
	})
}

func builtinStringCIEqual(args []any) (any, error) {
	if len(args) < 2 {
		return nil, &EvalError{Message: "string-ci=? expects at least 2 arguments"}
	}

	prev, err := expectString(args[0])
	if err != nil {
		return nil, err
	}
	for _, arg := range args[1:] {
		next, err := expectString(arg)
		if err != nil {
			return nil, err
		}
		if !strings.EqualFold(prev, next) {
			return false, nil
		}
		prev = next
	}
	return true, nil
}

func builtinStringUpcase(args []any) (any, error) {
	if len(args) != 1 {
		return nil, &EvalError{Message: "string-upcase expects exactly 1 argument"}
	}

	s, err := expectString(args[0])
	if err != nil {
		return nil, err
	}
	return strings.ToUpper(s), nil
}

func builtinStringDowncase(args []any) (any, error) {
	if len(args) != 1 {
		return nil, &EvalError{Message: "string-downcase expects exactly 1 argument"}
	}

	s, err := expectString(args[0])
	if err != nil {
		return nil, err
	}
	return strings.ToLower(s), nil
}

func expectTwoInts(name string, args []any) (int64, int64, error) {
	if len(args) != 2 {
		return 0, 0, &EvalError{Message: fmt.Sprintf("%s expects exactly 2 arguments", name)}
	}

	left, err := expectInt(args[0])
	if err != nil {
		return 0, 0, err
	}
	right, err := expectInt(args[1])
	if err != nil {
		return 0, 0, err
	}
	return left, right, nil
}

func builtinExtremum(name string, args []any, pick func(current, next int64) bool) (any, error) {
	if len(args) == 0 {
		return nil, &EvalError{Message: fmt.Sprintf("%s expects at least 1 argument", name)}
	}

	best, err := expectInt(args[0])
	if err != nil {
		return nil, err
	}
	for _, arg := range args[1:] {
		n, err := expectInt(arg)
		if err != nil {
			return nil, err
		}
		if pick(best, n) {
			best = n
		}
	}
	return best, nil
}

func builtinNumericPredicateValue(name string, args []any, pred func(int64) bool) (any, error) {
	if len(args) != 1 {
		return nil, &EvalError{Message: fmt.Sprintf("%s expects exactly 1 argument", name)}
	}

	n, err := expectInt(args[0])
	if err != nil {
		return nil, err
	}
	return pred(n), nil
}

func listTailValue(value any, index int64, name string) (any, error) {
	switch value.(type) {
	case pairValue, emptyListValue:
	default:
		return nil, &EvalError{Message: fmt.Sprintf("%s expects a list, got %s", name, typeName(value))}
	}

	current := value
	for i := int64(0); i < index; i++ {
		pair, ok := current.(pairValue)
		if !ok {
			return nil, &EvalError{Message: fmt.Sprintf("%s index out of range", name)}
		}
		current = pair.cdr
	}
	return current, nil
}

func isProperList(value any) bool {
	current := value
	seen := map[*pairCell]struct{}{}
	for {
		switch list := current.(type) {
		case emptyListValue:
			return true
		case pairValue:
			if _, ok := seen[list.pairCell]; ok {
				return false
			}
			seen[list.pairCell] = struct{}{}
			current = list.cdr
		default:
			return false
		}
	}
}

func valuesEq(left, right any) bool {
	if isNumberValue(left) && isNumberValue(right) {
		return numberValuesEqual(left, right)
	}

	switch lhs := left.(type) {
	case int64:
		rhs, ok := right.(int64)
		return ok && lhs == rhs
	case bool:
		rhs, ok := right.(bool)
		return ok && lhs == rhs
	case charValue:
		rhs, ok := right.(charValue)
		return ok && lhs == rhs
	case string:
		rhs, ok := right.(string)
		return ok && lhs == rhs
	case *mutableString:
		rhs, ok := right.(*mutableString)
		return ok && lhs == rhs
	case symbolExpr:
		rhs, ok := right.(symbolExpr)
		return ok && lhs.name == rhs.name
	case emptyListValue:
		_, ok := right.(emptyListValue)
		return ok
	case pairValue:
		rhs, ok := right.(pairValue)
		return ok && lhs == rhs
	case *vectorValue:
		rhs, ok := right.(*vectorValue)
		return ok && lhs == rhs
	case builtinProc:
		rhs, ok := right.(builtinProc)
		return ok && lhs.name == rhs.name
	case *recordValue:
		rhs, ok := right.(*recordValue)
		return ok && lhs == rhs
	case voidValue:
		_, ok := right.(voidValue)
		return ok
	default:
		return false
	}
}

func valuesEqual(left, right any) bool {
	return valuesEqualWithState(left, right, equalityState{
		pairs:   map[pairVisit]bool{},
		vectors: map[vectorVisit]bool{},
	})
}

type pairVisit struct {
	left  *pairCell
	right *pairCell
}

type vectorVisit struct {
	left  *vectorValue
	right *vectorValue
}

type equalityState struct {
	pairs   map[pairVisit]bool
	vectors map[vectorVisit]bool
}

func valuesEqualWithState(left, right any, state equalityState) bool {
	if isNumberValue(left) && isNumberValue(right) {
		return numberValuesEqual(left, right)
	}

	switch lhs := left.(type) {
	case int64:
		rhs, ok := right.(int64)
		return ok && lhs == rhs
	case bool:
		rhs, ok := right.(bool)
		return ok && lhs == rhs
	case charValue:
		rhs, ok := right.(charValue)
		return ok && lhs == rhs
	case string:
		switch rhs := right.(type) {
		case string:
			return lhs == rhs
		case *mutableString:
			return lhs == string(rhs.runes)
		default:
			return false
		}
	case *mutableString:
		switch rhs := right.(type) {
		case string:
			return string(lhs.runes) == rhs
		case *mutableString:
			return string(lhs.runes) == string(rhs.runes)
		default:
			return false
		}
	case symbolExpr:
		rhs, ok := right.(symbolExpr)
		return ok && lhs.name == rhs.name
	case emptyListValue:
		_, ok := right.(emptyListValue)
		return ok
	case pairValue:
		rhs, ok := right.(pairValue)
		if !ok {
			return false
		}
		key := pairVisit{left: lhs.pairCell, right: rhs.pairCell}
		if state.pairs[key] {
			return true
		}
		state.pairs[key] = true
		return valuesEqualWithState(lhs.car, rhs.car, state) &&
			valuesEqualWithState(lhs.cdr, rhs.cdr, state)
	case *vectorValue:
		rhs, ok := right.(*vectorValue)
		if !ok || len(lhs.elements) != len(rhs.elements) {
			return false
		}
		key := vectorVisit{left: lhs, right: rhs}
		if state.vectors[key] {
			return true
		}
		state.vectors[key] = true
		for i := range lhs.elements {
			if !valuesEqualWithState(lhs.elements[i], rhs.elements[i], state) {
				return false
			}
		}
		return true
	case builtinProc:
		rhs, ok := right.(builtinProc)
		return ok && lhs.name == rhs.name
	case *recordValue:
		rhs, ok := right.(*recordValue)
		return ok && lhs == rhs
	case voidValue:
		_, ok := right.(voidValue)
		return ok
	default:
		return false
	}
}

func expectChar(value any) (charValue, error) {
	ch, ok := value.(charValue)
	if !ok {
		return 0, &EvalError{Message: fmt.Sprintf("expected char, got %s", typeName(value))}
	}
	return ch, nil
}

func builtinCharPredicateValue(name string, args []any, pred func(rune) bool) (any, error) {
	if len(args) != 1 {
		return nil, &EvalError{Message: fmt.Sprintf("%s expects exactly 1 argument", name)}
	}

	ch, err := expectChar(args[0])
	if err != nil {
		return nil, err
	}
	return pred(rune(ch)), nil
}

func builtinCharTransform(name string, args []any, transform func(rune) rune) (any, error) {
	if len(args) != 1 {
		return nil, &EvalError{Message: fmt.Sprintf("%s expects exactly 1 argument", name)}
	}

	ch, err := expectChar(args[0])
	if err != nil {
		return nil, err
	}
	return charValue(transform(rune(ch))), nil
}

func builtinCharCompare(name string, args []any, cmp func(left, right rune) bool) (any, error) {
	if len(args) < 2 {
		return nil, &EvalError{Message: fmt.Sprintf("%s expects at least 2 arguments", name)}
	}

	prev, err := expectChar(args[0])
	if err != nil {
		return nil, err
	}
	for _, arg := range args[1:] {
		next, err := expectChar(arg)
		if err != nil {
			return nil, err
		}
		if !cmp(rune(prev), rune(next)) {
			return false, nil
		}
		prev = next
	}
	return true, nil
}

func builtinStringCompare(name string, args []any, cmp func(left, right string) bool) (any, error) {
	if len(args) < 2 {
		return nil, &EvalError{Message: fmt.Sprintf("%s expects at least 2 arguments", name)}
	}

	prev, err := expectString(args[0])
	if err != nil {
		return nil, err
	}
	for _, arg := range args[1:] {
		next, err := expectString(arg)
		if err != nil {
			return nil, err
		}
		if !cmp(prev, next) {
			return false, nil
		}
		prev = next
	}
	return true, nil
}
