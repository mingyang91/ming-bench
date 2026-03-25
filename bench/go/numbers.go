package ming

import (
	"math"
	"strconv"
	"strings"
)

type rationalValue struct {
	num int64
	den int64
}

type inexactValue float64

type exactFraction struct {
	num int64
	den int64
}

func parseNumberLiteral(text string) (value, bool, error) {
	if isIntegerLiteral(text) {
		parsed, err := strconv.ParseInt(text, 10, 64)
		if err != nil {
			return nil, true, err
		}
		return parsed, true, nil
	}

	if looksLikeRationalLiteral(text) {
		parsed, err := parseRationalLiteral(text)
		if err != nil {
			return nil, true, err
		}
		return parsed, true, nil
	}

	if looksLikeInexactLiteral(text) {
		parsed, err := strconv.ParseFloat(text, 64)
		if err != nil {
			return nil, true, err
		}
		return inexactValue(parsed), true, nil
	}

	return nil, false, nil
}

func parseExactDecimalLiteral(text string) (value, error) {
	if isIntegerLiteral(text) {
		return strconv.ParseInt(text, 10, 64)
	}

	sign := int64(1)
	if text[0] == '+' || text[0] == '-' {
		if text[0] == '-' {
			sign = -1
		}
		text = text[1:]
	}

	parts := strings.SplitN(text, ".", 2)
	if len(parts) != 2 || !isDigits(parts[0]) || !isDigits(parts[1]) {
		return nil, strconv.ErrSyntax
	}

	digits := parts[0] + parts[1]
	num, err := strconv.ParseInt(digits, 10, 64)
	if err != nil {
		return nil, err
	}

	den := int64(1)
	for i := 0; i < len(parts[1]); i++ {
		den *= 10
	}

	return makeExactNumber(sign*num, den), nil
}

func looksLikeRationalLiteral(text string) bool {
	if strings.Count(text, "/") != 1 {
		return false
	}

	slash := strings.IndexByte(text, '/')
	if slash <= 0 || slash >= len(text)-1 {
		return false
	}

	return isSignedDigits(text[:slash]) && isSignedDigits(text[slash+1:])
}

func parseRationalLiteral(text string) (value, error) {
	slash := strings.IndexByte(text, '/')
	num, err := strconv.ParseInt(text[:slash], 10, 64)
	if err != nil {
		return nil, err
	}

	den, err := strconv.ParseInt(text[slash+1:], 10, 64)
	if err != nil {
		return nil, err
	}
	if den == 0 {
		return nil, strconv.ErrSyntax
	}

	return makeExactNumber(num, den), nil
}

func looksLikeInexactLiteral(text string) bool {
	if strings.Count(text, ".") != 1 {
		return false
	}

	if strings.Contains(text, "/") {
		return false
	}

	if text[0] == '+' || text[0] == '-' {
		if len(text) == 1 {
			return false
		}
		text = text[1:]
	}

	parts := strings.SplitN(text, ".", 2)
	return len(parts) == 2 && isDigits(parts[0]) && isDigits(parts[1])
}

func isDigits(text string) bool {
	if text == "" {
		return false
	}

	for i := 0; i < len(text); i++ {
		if text[i] < '0' || text[i] > '9' {
			return false
		}
	}
	return true
}

func isSignedDigits(text string) bool {
	if text == "" {
		return false
	}

	if text[0] == '+' || text[0] == '-' {
		if len(text) == 1 {
			return false
		}
		text = text[1:]
	}

	return isDigits(text)
}

func makeExactNumber(num int64, den int64) value {
	if den < 0 {
		num = -num
		den = -den
	}
	if num == 0 {
		return int64(0)
	}

	divisor := gcdInt64(num, den)
	num /= divisor
	den /= divisor
	if den == 1 {
		return num
	}
	return rationalValue{num: num, den: den}
}

func gcdInt64(left int64, right int64) int64 {
	if left < 0 {
		left = -left
	}
	if right < 0 {
		right = -right
	}
	if left == 0 {
		if right == 0 {
			return 1
		}
		return right
	}
	for right != 0 {
		left, right = right, left%right
	}
	return left
}

func isNumberValue(v value) bool {
	switch v.(type) {
	case int64, rationalValue, inexactValue:
		return true
	default:
		return false
	}
}

func isExactNumberValue(v value) bool {
	switch v.(type) {
	case int64, rationalValue:
		return true
	default:
		return false
	}
}

func isInexactNumberValue(v value) bool {
	_, ok := v.(inexactValue)
	return ok
}

func isIntegerNumberValue(v value) bool {
	switch n := v.(type) {
	case int64:
		return true
	case rationalValue:
		return n.den == 1
	case inexactValue:
		f := float64(n)
		return !math.IsNaN(f) && !math.IsInf(f, 0) && math.Trunc(f) == f
	default:
		return false
	}
}

func isRationalNumberValue(v value) bool {
	switch n := v.(type) {
	case int64, rationalValue:
		return true
	case inexactValue:
		f := float64(n)
		return !math.IsNaN(f) && !math.IsInf(f, 0)
	default:
		return false
	}
}

func expectNumberValue(v value, pos position) (value, error) {
	if !isNumberValue(v) {
		return nil, newEvalError(ErrTypeMismatch, "expected number", pos)
	}
	return v, nil
}

func expectExactFraction(v value, pos position, name string) (exactFraction, error) {
	switch n := v.(type) {
	case int64:
		return exactFraction{num: n, den: 1}, nil
	case rationalValue:
		return exactFraction{num: n.num, den: n.den}, nil
	default:
		return exactFraction{}, newEvalError(ErrTypeMismatch, name+": expected exact number", pos)
	}
}

func toExactFraction(v value) (exactFraction, bool) {
	switch n := v.(type) {
	case int64:
		return exactFraction{num: n, den: 1}, true
	case rationalValue:
		return exactFraction{num: n.num, den: n.den}, true
	default:
		return exactFraction{}, false
	}
}

func toFloat64(v value) (float64, bool) {
	switch n := v.(type) {
	case int64:
		return float64(n), true
	case rationalValue:
		return float64(n.num) / float64(n.den), true
	case inexactValue:
		return float64(n), true
	default:
		return 0, false
	}
}

func negateNumber(v value) value {
	switch n := v.(type) {
	case int64:
		return -n
	case rationalValue:
		return rationalValue{num: -n.num, den: n.den}
	case inexactValue:
		return inexactValue(-float64(n))
	default:
		return nil
	}
}

func addNumbers(left value, right value) value {
	if isInexactNumberValue(left) || isInexactNumberValue(right) {
		leftFloat, _ := toFloat64(left)
		rightFloat, _ := toFloat64(right)
		return inexactValue(leftFloat + rightFloat)
	}

	leftExact, _ := toExactFraction(left)
	rightExact, _ := toExactFraction(right)
	return makeExactNumber(
		leftExact.num*rightExact.den+rightExact.num*leftExact.den,
		leftExact.den*rightExact.den,
	)
}

func subNumbers(left value, right value) value {
	if isInexactNumberValue(left) || isInexactNumberValue(right) {
		leftFloat, _ := toFloat64(left)
		rightFloat, _ := toFloat64(right)
		return inexactValue(leftFloat - rightFloat)
	}

	leftExact, _ := toExactFraction(left)
	rightExact, _ := toExactFraction(right)
	return makeExactNumber(
		leftExact.num*rightExact.den-rightExact.num*leftExact.den,
		leftExact.den*rightExact.den,
	)
}

func mulNumbers(left value, right value) value {
	if isInexactNumberValue(left) || isInexactNumberValue(right) {
		leftFloat, _ := toFloat64(left)
		rightFloat, _ := toFloat64(right)
		return inexactValue(leftFloat * rightFloat)
	}

	leftExact, _ := toExactFraction(left)
	rightExact, _ := toExactFraction(right)
	return makeExactNumber(leftExact.num*rightExact.num, leftExact.den*rightExact.den)
}

func divNumbers(left value, right value, pos position) (value, error) {
	if isInexactNumberValue(left) || isInexactNumberValue(right) {
		leftFloat, _ := toFloat64(left)
		rightFloat, _ := toFloat64(right)
		if rightFloat == 0 {
			return nil, newEvalError(ErrDivisionByZero, "division by zero", pos)
		}
		return inexactValue(leftFloat / rightFloat), nil
	}

	leftExact, _ := toExactFraction(left)
	rightExact, _ := toExactFraction(right)
	if rightExact.num == 0 {
		return nil, newEvalError(ErrDivisionByZero, "division by zero", pos)
	}
	return makeExactNumber(leftExact.num*rightExact.den, leftExact.den*rightExact.num), nil
}

func numberEqual(left value, right value) bool {
	return compareTwoNumbers(
		left,
		right,
		func(left exactFraction, right exactFraction) bool {
			return left.num*right.den == right.num*left.den
		},
		func(left float64, right float64) bool { return left == right },
	)
}

func compareTwoNumbers(
	left value,
	right value,
	exactCmp func(exactFraction, exactFraction) bool,
	inexactCmp func(float64, float64) bool,
) bool {
	if isExactNumberValue(left) && isExactNumberValue(right) {
		leftExact, _ := toExactFraction(left)
		rightExact, _ := toExactFraction(right)
		return exactCmp(leftExact, rightExact)
	}

	leftFloat, leftOK := toFloat64(left)
	rightFloat, rightOK := toFloat64(right)
	if !leftOK || !rightOK {
		return false
	}
	return inexactCmp(leftFloat, rightFloat)
}

func formatNumberValue(v value) (string, error) {
	switch n := v.(type) {
	case int64:
		return strconv.FormatInt(n, 10), nil
	case rationalValue:
		return strconv.FormatInt(n.num, 10) + "/" + strconv.FormatInt(n.den, 10), nil
	case inexactValue:
		formatted := strconv.FormatFloat(float64(n), 'f', -1, 64)
		if !strings.ContainsAny(formatted, ".eE") {
			formatted += ".0"
		}
		return formatted, nil
	default:
		return "", &EvalError{Message: "cannot format value"}
	}
}

func exactToInexact(v value, pos position) (value, error) {
	switch n := v.(type) {
	case int64:
		return inexactValue(float64(n)), nil
	case rationalValue:
		return inexactValue(float64(n.num) / float64(n.den)), nil
	case inexactValue:
		return n, nil
	default:
		return nil, newEvalError(ErrTypeMismatch, "exact->inexact: expected number", pos)
	}
}

func inexactToExact(v value, pos position) (value, error) {
	switch n := v.(type) {
	case int64, rationalValue:
		return v, nil
	case inexactValue:
		text := strconv.FormatFloat(float64(n), 'f', -1, 64)
		parsed, err := parseExactDecimalLiteral(text)
		if err != nil {
			return nil, newEvalError(ErrTypeMismatch, "inexact->exact: cannot convert number", pos)
		}
		return parsed, nil
	default:
		return nil, newEvalError(ErrTypeMismatch, "inexact->exact: expected number", pos)
	}
}

func builtinExactPred(_ *interpreter, args []value, callPos position) (value, error) {
	if len(args) != 1 {
		return nil, wrongArgCount(callPos, "exact?", "expected exactly 1 argument")
	}
	return isExactNumberValue(args[0]), nil
}

func builtinInexactPred(_ *interpreter, args []value, callPos position) (value, error) {
	if len(args) != 1 {
		return nil, wrongArgCount(callPos, "inexact?", "expected exactly 1 argument")
	}
	return isInexactNumberValue(args[0]), nil
}

func builtinExactToInexact(_ *interpreter, args []value, callPos position) (value, error) {
	if len(args) != 1 {
		return nil, wrongArgCount(callPos, "exact->inexact", "expected exactly 1 argument")
	}
	return exactToInexact(args[0], callPos)
}

func builtinInexactToExact(_ *interpreter, args []value, callPos position) (value, error) {
	if len(args) != 1 {
		return nil, wrongArgCount(callPos, "inexact->exact", "expected exactly 1 argument")
	}
	return inexactToExact(args[0], callPos)
}

func builtinNumerator(_ *interpreter, args []value, callPos position) (value, error) {
	if len(args) != 1 {
		return nil, wrongArgCount(callPos, "numerator", "expected exactly 1 argument")
	}

	parts, err := expectExactFraction(args[0], callPos, "numerator")
	if err != nil {
		return nil, err
	}
	return parts.num, nil
}

func builtinDenominator(_ *interpreter, args []value, callPos position) (value, error) {
	if len(args) != 1 {
		return nil, wrongArgCount(callPos, "denominator", "expected exactly 1 argument")
	}

	parts, err := expectExactFraction(args[0], callPos, "denominator")
	if err != nil {
		return nil, err
	}
	return parts.den, nil
}

func builtinIntegerPred(_ *interpreter, args []value, callPos position) (value, error) {
	if len(args) != 1 {
		return nil, wrongArgCount(callPos, "integer?", "expected exactly 1 argument")
	}
	return isIntegerNumberValue(args[0]), nil
}

func builtinRationalPred(_ *interpreter, args []value, callPos position) (value, error) {
	if len(args) != 1 {
		return nil, wrongArgCount(callPos, "rational?", "expected exactly 1 argument")
	}
	return isRationalNumberValue(args[0]), nil
}
