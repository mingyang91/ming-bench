package ming

import (
	"math"
	"strings"
	"unicode"
)

func registerLevel09Builtins(global *env) {
	global.define("abs", builtinProc{name: "abs", fn: evalAbs})
	global.define("assoc", builtinProc{name: "assoc", fn: evalAssoc})
	global.define("assv", builtinProc{name: "assv", fn: evalAssv})
	global.define("char-alphabetic?", builtinProc{name: "char-alphabetic?", fn: evalCharAlphabeticPred})
	global.define("char-numeric?", builtinProc{name: "char-numeric?", fn: evalCharNumericPred})
	global.define("char-upcase", builtinProc{name: "char-upcase", fn: evalCharUpcase})
	global.define("char-downcase", builtinProc{name: "char-downcase", fn: evalCharDowncase})
	global.define("char=?", builtinProc{name: "char=?", fn: evalCharEq})
	global.define("char<?", builtinProc{name: "char<?", fn: evalCharLess})
	global.define("eq?", builtinProc{name: "eq?", fn: evalEqPred})
	global.define("equal?", builtinProc{name: "equal?", fn: evalEqualPred})
	global.define("expt", builtinProc{name: "expt", fn: evalExpt})
	global.define("for-each", builtinProc{name: "for-each", fn: evalForEach})
	global.define("gcd", builtinProc{name: "gcd", fn: evalGCD})
	global.define("lcm", builtinProc{name: "lcm", fn: evalLCM})
	global.define("list?", builtinProc{name: "list?", fn: evalListPred})
	global.define("list-ref", builtinProc{name: "list-ref", fn: evalListRef})
	global.define("list-tail", builtinProc{name: "list-tail", fn: evalListTail})
	global.define("map", builtinProc{name: "map", fn: evalMapBuiltin})
	global.define("max", builtinProc{name: "max", fn: evalMax})
	global.define("member", builtinProc{name: "member", fn: evalMember})
	global.define("min", builtinProc{name: "min", fn: evalMin})
	global.define("modulo", builtinProc{name: "modulo", fn: evalModulo})
	global.define("odd?", builtinProc{name: "odd?", fn: evalOddPred})
	global.define("even?", builtinProc{name: "even?", fn: evalEvenPred})
	global.define("positive?", builtinProc{name: "positive?", fn: evalPositivePred})
	global.define("negative?", builtinProc{name: "negative?", fn: evalNegativePred})
	global.define("quotient", builtinProc{name: "quotient", fn: evalQuotient})
	global.define("remainder", builtinProc{name: "remainder", fn: evalRemainder})
	global.define("reverse", builtinProc{name: "reverse", fn: evalReverse})
	global.define("string=?", builtinProc{name: "string=?", fn: evalStringEq})
	global.define("string<?", builtinProc{name: "string<?", fn: evalStringLess})
	global.define("string>?", builtinProc{name: "string>?", fn: evalStringGreater})
	global.define("string<=?", builtinProc{name: "string<=?", fn: evalStringLessEqual})
	global.define("string>=?", builtinProc{name: "string>=?", fn: evalStringGreaterEqual})
	global.define("string-ci=?", builtinProc{name: "string-ci=?", fn: evalStringCiEq})
	global.define("string-upcase", builtinProc{name: "string-upcase", fn: evalStringUpcase})
	global.define("string-downcase", builtinProc{name: "string-downcase", fn: evalStringDowncase})
	global.define("truncate", builtinProc{name: "truncate", fn: evalTruncate})
	global.define("round", builtinProc{name: "round", fn: evalRound})
	global.define("zero?", builtinProc{name: "zero?", fn: evalZeroPred})
}

func evalAbs(args []value) (value, error) {
	if len(args) != 1 {
		return nil, newCurrentEvalError("'abs' expects exactly 1 argument")
	}

	n, err := expectNumber(args[0])
	if err != nil {
		return nil, err
	}
	return n.abs(), nil
}

func evalAssoc(args []value) (value, error) {
	if len(args) != 2 {
		return nil, newCurrentEvalError("'assoc' expects exactly 2 arguments")
	}

	items, err := properListElements(args[1])
	if err != nil {
		return nil, err
	}

	for _, item := range items {
		pair, ok := item.(pairValue)
		if !ok {
			return nil, newCurrentEvalError("'assoc' expects a list of pairs, got %s", item.schemeString())
		}
		if schemeEqual(args[0], pair.carValue()) {
			return pair, nil
		}
	}

	return boolValue(false), nil
}

func evalAssv(args []value) (value, error) {
	if len(args) != 2 {
		return nil, newCurrentEvalError("'assv' expects exactly 2 arguments")
	}

	items, err := properListElements(args[1])
	if err != nil {
		return nil, err
	}

	for _, item := range items {
		pair, ok := item.(pairValue)
		if !ok {
			return nil, newCurrentEvalError("'assv' expects a list of pairs, got %s", item.schemeString())
		}
		if schemeEqv(args[0], pair.carValue()) {
			return pair, nil
		}
	}

	return boolValue(false), nil
}

func evalCharAlphabeticPred(args []value) (value, error) {
	return evalUnaryCharPredicate(args, "char-alphabetic?", unicode.IsLetter)
}

func evalCharNumericPred(args []value) (value, error) {
	return evalUnaryCharPredicate(args, "char-numeric?", unicode.IsDigit)
}

func evalCharUpcase(args []value) (value, error) {
	return evalUnaryCharTransform(args, "char-upcase", unicode.ToUpper)
}

func evalCharDowncase(args []value) (value, error) {
	return evalUnaryCharTransform(args, "char-downcase", unicode.ToLower)
}

func evalCharEq(args []value) (value, error) {
	return evalCharCompare(args, "char=?", func(a, b rune) bool { return a == b })
}

func evalCharLess(args []value) (value, error) {
	return evalCharCompare(args, "char<?", func(a, b rune) bool { return a < b })
}

func evalEqPred(args []value) (value, error) {
	if len(args) != 2 {
		return nil, newCurrentEvalError("'eq?' expects exactly 2 arguments")
	}
	return boolValue(schemeEq(args[0], args[1])), nil
}

func evalEqualPred(args []value) (value, error) {
	if len(args) != 2 {
		return nil, newCurrentEvalError("'equal?' expects exactly 2 arguments")
	}
	return boolValue(schemeEqual(args[0], args[1])), nil
}

func evalExpt(args []value) (value, error) {
	if len(args) != 2 {
		return nil, newCurrentEvalError("'expt' expects exactly 2 arguments")
	}

	base, err := expectInteger(args[0])
	if err != nil {
		return nil, err
	}
	exp, err := expectInteger(args[1])
	if err != nil {
		return nil, err
	}
	if exp < 0 {
		return nil, newCurrentEvalError("'expt' expects a non-negative exponent")
	}

	result := 1
	for exp > 0 {
		if exp%2 == 1 {
			result *= base
		}
		exp /= 2
		if exp > 0 {
			base *= base
		}
	}
	return newExactInteger(result), nil
}

func evalGCD(args []value) (value, error) {
	result := 0
	for _, arg := range args {
		n, err := expectInteger(arg)
		if err != nil {
			return nil, err
		}
		result = gcd(result, n)
	}
	return newExactInteger(absInt(result)), nil
}

func evalLCM(args []value) (value, error) {
	if len(args) == 0 {
		return newExactInteger(1), nil
	}

	result := 1
	for _, arg := range args {
		n, err := expectInteger(arg)
		if err != nil {
			return nil, err
		}

		if result == 0 || n == 0 {
			result = 0
			continue
		}

		result = absInt(result/gcd(result, n) * n)
	}

	return newExactInteger(result), nil
}

func evalListPred(args []value) (value, error) {
	if len(args) != 1 {
		return nil, newCurrentEvalError("'list?' expects exactly 1 argument")
	}
	return boolValue(isProperList(args[0])), nil
}

func evalListRef(args []value) (value, error) {
	if len(args) != 2 {
		return nil, newCurrentEvalError("'list-ref' expects exactly 2 arguments")
	}
	if !isProperList(args[0]) {
		return nil, newCurrentEvalError("'list-ref' expects a proper list, got %s", args[0].schemeString())
	}

	index, err := expectNonNegativeIndex(args[1], "list-ref")
	if err != nil {
		return nil, err
	}

	current := args[0]
	for i := 0; i < index; i++ {
		pair, ok := current.(pairValue)
		if !ok {
			return nil, newCurrentEvalError("'list-ref' index out of range")
		}
		current = pair.cdrValue()
	}

	pair, ok := current.(pairValue)
	if !ok {
		return nil, newCurrentEvalError("'list-ref' index out of range")
	}
	return pair.carValue(), nil
}

func evalListTail(args []value) (value, error) {
	if len(args) != 2 {
		return nil, newCurrentEvalError("'list-tail' expects exactly 2 arguments")
	}
	if !isProperList(args[0]) {
		return nil, newCurrentEvalError("'list-tail' expects a proper list, got %s", args[0].schemeString())
	}

	index, err := expectNonNegativeIndex(args[1], "list-tail")
	if err != nil {
		return nil, err
	}

	current := args[0]
	for i := 0; i < index; i++ {
		pair, ok := current.(pairValue)
		if !ok {
			return nil, newCurrentEvalError("'list-tail' index out of range")
		}
		current = pair.cdrValue()
	}

	switch current.(type) {
	case pairValue, emptyListValue:
		return current, nil
	default:
		return nil, newCurrentEvalError("'list-tail' index out of range")
	}
}

func evalMapBuiltin(args []value) (value, error) {
	if len(args) < 2 {
		return nil, newCurrentEvalError("'map' expects a procedure and at least 1 list")
	}

	proc, ok := args[0].(procedure)
	if !ok {
		return nil, newCurrentEvalError("'map' expects a procedure, got %s", args[0].schemeString())
	}

	lists := make([][]value, len(args)-1)
	expectedLen := -1
	for i, arg := range args[1:] {
		elems, err := properListElements(arg)
		if err != nil {
			return nil, err
		}
		if expectedLen == -1 {
			expectedLen = len(elems)
		} else if len(elems) != expectedLen {
			return nil, newCurrentEvalError("'map' expects lists of equal length")
		}
		lists[i] = elems
	}

	results := make([]value, 0, expectedLen)
	callArgs := make([]value, len(lists))
	for i := 0; i < expectedLen; i++ {
		for j := range lists {
			callArgs[j] = lists[j][i]
		}
		result, err := proc.call(callArgs)
		if err != nil {
			return nil, err
		}
		results = append(results, result)
	}

	return listFromValues(results), nil
}

func evalForEach(args []value) (value, error) {
	if len(args) < 2 {
		return nil, newCurrentEvalError("'for-each' expects a procedure and at least 1 list")
	}

	proc, ok := args[0].(procedure)
	if !ok {
		return nil, newCurrentEvalError("'for-each' expects a procedure, got %s", args[0].schemeString())
	}

	lists := make([][]value, len(args)-1)
	expectedLen := -1
	for i, arg := range args[1:] {
		elems, err := properListElements(arg)
		if err != nil {
			return nil, err
		}
		if expectedLen == -1 {
			expectedLen = len(elems)
		} else if len(elems) != expectedLen {
			return nil, newCurrentEvalError("'for-each' expects lists of equal length")
		}
		lists[i] = elems
	}

	callArgs := make([]value, len(lists))
	for i := 0; i < expectedLen; i++ {
		for j := range lists {
			callArgs[j] = lists[j][i]
		}
		if _, err := proc.call(callArgs); err != nil {
			return nil, err
		}
	}

	return voidValue{}, nil
}

func evalMember(args []value) (value, error) {
	if len(args) != 2 {
		return nil, newCurrentEvalError("'member' expects exactly 2 arguments")
	}

	current := args[1]
	seen := make(map[*pairCell]struct{})

	for {
		switch list := current.(type) {
		case emptyListValue:
			return boolValue(false), nil
		case pairValue:
			if _, exists := seen[list.cell]; exists {
				return nil, newCurrentEvalError("'member' expects a proper list, got %s", args[1].schemeString())
			}
			seen[list.cell] = struct{}{}
			if schemeEqual(args[0], list.carValue()) {
				return current, nil
			}
			current = list.cdrValue()
		default:
			return nil, newCurrentEvalError("'member' expects a proper list, got %s", args[1].schemeString())
		}
	}
}

func evalReverse(args []value) (value, error) {
	if len(args) != 1 {
		return nil, newCurrentEvalError("'reverse' expects exactly 1 argument")
	}

	elems, err := properListElements(args[0])
	if err != nil {
		return nil, err
	}

	result := value(emptyList)
	for _, elem := range elems {
		result = newPair(elem, result)
	}

	return result, nil
}

func evalMax(args []value) (value, error) {
	return evalMinMax(args, "max", func(cmp int) bool { return cmp < 0 })
}

func evalMin(args []value) (value, error) {
	return evalMinMax(args, "min", func(cmp int) bool { return cmp > 0 })
}

func evalModulo(args []value) (value, error) {
	a, b, err := evalDivisionOperands(args, "modulo")
	if err != nil {
		return nil, err
	}

	rem := a % b
	if rem != 0 && ((rem < 0 && b > 0) || (rem > 0 && b < 0)) {
		rem += b
	}
	return newExactInteger(rem), nil
}

func evalOddPred(args []value) (value, error) {
	if len(args) != 1 {
		return nil, newCurrentEvalError("'odd?' expects exactly 1 argument")
	}

	n, err := expectInteger(args[0])
	if err != nil {
		return nil, err
	}
	return boolValue(n%2 != 0), nil
}

func evalEvenPred(args []value) (value, error) {
	if len(args) != 1 {
		return nil, newCurrentEvalError("'even?' expects exactly 1 argument")
	}

	n, err := expectInteger(args[0])
	if err != nil {
		return nil, err
	}
	return boolValue(n%2 == 0), nil
}

func evalPositivePred(args []value) (value, error) {
	return evalUnaryNumberPredicate(args, "positive?", func(n numberValue) bool { return n.compare(newExactInteger(0)) > 0 })
}

func evalNegativePred(args []value) (value, error) {
	return evalUnaryNumberPredicate(args, "negative?", func(n numberValue) bool { return n.compare(newExactInteger(0)) < 0 })
}

func evalQuotient(args []value) (value, error) {
	a, b, err := evalDivisionOperands(args, "quotient")
	if err != nil {
		return nil, err
	}
	return newExactInteger(a / b), nil
}

func evalRemainder(args []value) (value, error) {
	a, b, err := evalDivisionOperands(args, "remainder")
	if err != nil {
		return nil, err
	}
	return newExactInteger(a % b), nil
}

func evalStringEq(args []value) (value, error) {
	return evalStringCompare(args, "string=?", func(a, b string) bool { return a == b })
}

func evalStringLess(args []value) (value, error) {
	return evalStringCompare(args, "string<?", stringLess)
}

func evalStringGreater(args []value) (value, error) {
	return evalStringCompare(args, "string>?", func(a, b string) bool { return stringLess(b, a) })
}

func evalStringLessEqual(args []value) (value, error) {
	return evalStringCompare(args, "string<=?", func(a, b string) bool { return !stringLess(b, a) })
}

func evalStringGreaterEqual(args []value) (value, error) {
	return evalStringCompare(args, "string>=?", func(a, b string) bool { return !stringLess(a, b) })
}

func evalStringCiEq(args []value) (value, error) {
	return evalStringCompare(args, "string-ci=?", strings.EqualFold)
}

func evalStringUpcase(args []value) (value, error) {
	return evalUnaryStringTransform(args, "string-upcase", strings.ToUpper)
}

func evalStringDowncase(args []value) (value, error) {
	return evalUnaryStringTransform(args, "string-downcase", strings.ToLower)
}

func evalTruncate(args []value) (value, error) {
	if len(args) != 1 {
		return nil, newCurrentEvalError("'truncate' expects exactly 1 argument")
	}

	n, err := expectNumber(args[0])
	if err != nil {
		return nil, err
	}

	if n.exact {
		return newExactInteger(n.numer / n.denom), nil
	}

	return newNumberValue(int(math.Trunc(n.float64())), 1, false), nil
}

func evalRound(args []value) (value, error) {
	if len(args) != 1 {
		return nil, newCurrentEvalError("'round' expects exactly 1 argument")
	}

	n, err := expectNumber(args[0])
	if err != nil {
		return nil, err
	}

	if n.exact {
		return newExactInteger(roundExactToEven(n.numer, n.denom)), nil
	}

	return newNumberValue(int(math.RoundToEven(n.float64())), 1, false), nil
}

func evalZeroPred(args []value) (value, error) {
	return evalUnaryNumberPredicate(args, "zero?", func(n numberValue) bool { return n.numer == 0 })
}

func roundExactToEven(numer, denom int) int {
	quot := numer / denom
	rem := absInt(numer % denom)
	twice := rem * 2

	switch {
	case twice < denom:
		return quot
	case twice > denom:
		if numer < 0 {
			return quot - 1
		}
		return quot + 1
	default:
		if quot%2 == 0 {
			return quot
		}
		if numer < 0 {
			return quot - 1
		}
		return quot + 1
	}
}

func evalUnaryNumberPredicate(args []value, name string, pred func(numberValue) bool) (value, error) {
	if len(args) != 1 {
		return nil, newCurrentEvalError("'%s' expects exactly 1 argument", name)
	}

	n, err := expectNumber(args[0])
	if err != nil {
		return nil, err
	}
	return boolValue(pred(n)), nil
}

func evalUnaryCharPredicate(args []value, name string, pred func(rune) bool) (value, error) {
	if len(args) != 1 {
		return nil, newCurrentEvalError("'%s' expects exactly 1 argument", name)
	}

	ch, err := expectChar(args[0])
	if err != nil {
		return nil, err
	}
	return boolValue(pred(ch)), nil
}

func evalUnaryCharTransform(args []value, name string, transform func(rune) rune) (value, error) {
	if len(args) != 1 {
		return nil, newCurrentEvalError("'%s' expects exactly 1 argument", name)
	}

	ch, err := expectChar(args[0])
	if err != nil {
		return nil, err
	}
	return charValue(transform(ch)), nil
}

func evalCharCompare(args []value, name string, pred func(rune, rune) bool) (value, error) {
	if len(args) < 2 {
		return nil, newCurrentEvalError("'%s' expects at least 2 arguments", name)
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
		if !pred(prev, next) {
			return boolValue(false), nil
		}
		prev = next
	}

	return boolValue(true), nil
}

func evalStringCompare(args []value, name string, pred func(string, string) bool) (value, error) {
	if len(args) < 2 {
		return nil, newCurrentEvalError("'%s' expects at least 2 arguments", name)
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
		if !pred(prev, next) {
			return boolValue(false), nil
		}
		prev = next
	}

	return boolValue(true), nil
}

func evalUnaryStringTransform(args []value, name string, transform func(string) string) (value, error) {
	if len(args) != 1 {
		return nil, newCurrentEvalError("'%s' expects exactly 1 argument", name)
	}

	s, err := expectString(args[0])
	if err != nil {
		return nil, err
	}
	return newStringValue(transform(s)), nil
}

func evalMinMax(args []value, name string, replace func(cmp int) bool) (value, error) {
	if len(args) == 0 {
		return nil, newCurrentEvalError("'%s' expects at least 1 argument", name)
	}

	best, err := expectNumber(args[0])
	if err != nil {
		return nil, err
	}

	for _, arg := range args[1:] {
		next, err := expectNumber(arg)
		if err != nil {
			return nil, err
		}
		if replace(best.compare(next)) {
			best = next
		}
	}

	return best, nil
}

func evalDivisionOperands(args []value, name string) (int, int, error) {
	if len(args) != 2 {
		return 0, 0, newCurrentEvalError("'%s' expects exactly 2 arguments", name)
	}

	a, err := expectInteger(args[0])
	if err != nil {
		return 0, 0, err
	}
	b, err := expectInteger(args[1])
	if err != nil {
		return 0, 0, err
	}
	if b == 0 {
		return 0, 0, newCurrentEvalError("division by zero")
	}
	return a, b, nil
}

func expectNonNegativeIndex(v value, name string) (int, error) {
	index, err := expectInteger(v)
	if err != nil {
		return 0, err
	}
	if index < 0 {
		return 0, newCurrentEvalError("'%s' expects a non-negative index", name)
	}
	return index, nil
}

func isProperList(v value) bool {
	seen := make(map[*pairCell]struct{})

	for {
		switch list := v.(type) {
		case emptyListValue:
			return true
		case pairValue:
			if _, exists := seen[list.cell]; exists {
				return false
			}
			seen[list.cell] = struct{}{}
			v = list.cdrValue()
		default:
			return false
		}
	}
}

type pairEqualKey struct {
	left  *pairCell
	right *pairCell
}

type vectorEqualKey struct {
	left  *vectorValue
	right *vectorValue
}

func schemeEq(a, b value) bool {
	switch av := a.(type) {
	case numberValue:
		bv, ok := b.(numberValue)
		return ok && av == bv
	case boolValue:
		bv, ok := b.(boolValue)
		return ok && av == bv
	case symbolValue:
		bv, ok := b.(symbolValue)
		return ok && av == bv
	case charValue:
		bv, ok := b.(charValue)
		return ok && av == bv
	case emptyListValue:
		_, ok := b.(emptyListValue)
		return ok
	case *stringValue:
		bv, ok := b.(*stringValue)
		return ok && av == bv
	case pairValue:
		bv, ok := b.(pairValue)
		return ok && av.cell == bv.cell
	case *vectorValue:
		bv, ok := b.(*vectorValue)
		return ok && av == bv
	case *recordValue:
		bv, ok := b.(*recordValue)
		return ok && av == bv
	case builtinProc:
		bv, ok := b.(builtinProc)
		return ok && av.name == bv.name
	case recordConstructorProc:
		bv, ok := b.(recordConstructorProc)
		return ok && av.name == bv.name && av.recordType == bv.recordType
	case recordPredicateProc:
		bv, ok := b.(recordPredicateProc)
		return ok && av.name == bv.name && av.recordType == bv.recordType
	case recordAccessorProc:
		bv, ok := b.(recordAccessorProc)
		return ok && av.name == bv.name && av.recordType == bv.recordType && av.fieldIndex == bv.fieldIndex
	case voidValue:
		_, ok := b.(voidValue)
		return ok
	default:
		return false
	}
}

func schemeEqual(a, b value) bool {
	return schemeEqualWithSeen(
		a,
		b,
		make(map[pairEqualKey]struct{}),
		make(map[vectorEqualKey]struct{}),
	)
}

func schemeEqualWithSeen(a, b value, seenPairs map[pairEqualKey]struct{}, seenVectors map[vectorEqualKey]struct{}) bool {
	switch av := a.(type) {
	case numberValue:
		bv, ok := b.(numberValue)
		return ok && av.equal(bv)
	case boolValue:
		bv, ok := b.(boolValue)
		return ok && av == bv
	case symbolValue:
		bv, ok := b.(symbolValue)
		return ok && av == bv
	case charValue:
		bv, ok := b.(charValue)
		return ok && av == bv
	case emptyListValue:
		_, ok := b.(emptyListValue)
		return ok
	case *stringValue:
		bv, ok := b.(*stringValue)
		return ok && av.text() == bv.text()
	case pairValue:
		bv, ok := b.(pairValue)
		if !ok {
			return false
		}

		key := pairEqualKey{left: av.cell, right: bv.cell}
		if _, seen := seenPairs[key]; seen {
			return true
		}
		seenPairs[key] = struct{}{}

		return schemeEqualWithSeen(av.carValue(), bv.carValue(), seenPairs, seenVectors) &&
			schemeEqualWithSeen(av.cdrValue(), bv.cdrValue(), seenPairs, seenVectors)
	case *vectorValue:
		bv, ok := b.(*vectorValue)
		if !ok || len(av.elems) != len(bv.elems) {
			return false
		}

		key := vectorEqualKey{left: av, right: bv}
		if _, seen := seenVectors[key]; seen {
			return true
		}
		seenVectors[key] = struct{}{}

		for i := range av.elems {
			if !schemeEqualWithSeen(av.elems[i], bv.elems[i], seenPairs, seenVectors) {
				return false
			}
		}
		return true
	case builtinProc:
		bv, ok := b.(builtinProc)
		return ok && av.name == bv.name
	case voidValue:
		_, ok := b.(voidValue)
		return ok
	default:
		return false
	}
}

func stringLess(a, b string) bool {
	left := []rune(a)
	right := []rune(b)

	limit := len(left)
	if len(right) < limit {
		limit = len(right)
	}

	for i := 0; i < limit; i++ {
		if left[i] < right[i] {
			return true
		}
		if left[i] > right[i] {
			return false
		}
	}

	return len(left) < len(right)
}
