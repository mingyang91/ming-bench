package ming

import (
	"fmt"
	"math"
	"strconv"
	"strings"
)

type rationalValue struct {
	numer int64
	denom int64
}

type numericValue struct {
	exact   bool
	numer   int64
	denom   int64
	inexact float64
}

func parseNumberLiteral(token string) (any, bool, error) {
	if !looksLikeNumberToken(token) {
		return nil, false, nil
	}

	if strings.Count(token, "/") == 1 {
		parts := strings.SplitN(token, "/", 2)
		if parts[0] == "" || parts[1] == "" {
			return nil, false, nil
		}

		numer, err := strconv.ParseInt(parts[0], 10, 64)
		if err != nil {
			return nil, false, nil
		}
		denom, err := strconv.ParseInt(parts[1], 10, 64)
		if err != nil {
			return nil, false, nil
		}
		if denom == 0 {
			return nil, true, &EvalError{Message: "invalid rational literal"}
		}
		return makeExactNumber(numer, denom), true, nil
	}

	if n, err := strconv.ParseInt(token, 10, 64); err == nil {
		return n, true, nil
	}

	if strings.ContainsAny(token, ".eE") {
		f, err := strconv.ParseFloat(token, 64)
		if err == nil {
			return f, true, nil
		}
	}

	return nil, false, nil
}

func looksLikeNumberToken(token string) bool {
	if token == "" {
		return false
	}

	start := 0
	if token[0] == '+' || token[0] == '-' {
		if len(token) == 1 {
			return false
		}
		start = 1
	}

	if start >= len(token) {
		return false
	}

	ch := token[start]
	return ('0' <= ch && ch <= '9') || ch == '.'
}

func isNumberValue(value any) bool {
	switch value.(type) {
	case int64, rationalValue, float64:
		return true
	default:
		return false
	}
}

func isExactNumberValue(value any) bool {
	switch value.(type) {
	case int64, rationalValue:
		return true
	default:
		return false
	}
}

func isInexactNumberValue(value any) bool {
	_, ok := value.(float64)
	return ok
}

func isIntegerNumberValue(value any) bool {
	switch n := value.(type) {
	case int64:
		return true
	case rationalValue:
		return n.denom == 1 || n.numer%n.denom == 0
	case float64:
		return math.Trunc(n) == n
	default:
		return false
	}
}

func isRationalNumberValue(value any) bool {
	return isNumberValue(value)
}

func toNumericValue(value any) (numericValue, error) {
	switch n := value.(type) {
	case int64:
		return numericValue{exact: true, numer: n, denom: 1}, nil
	case rationalValue:
		return numericValue{exact: true, numer: n.numer, denom: n.denom}, nil
	case float64:
		return numericValue{inexact: n}, nil
	default:
		return numericValue{}, &EvalError{Message: fmt.Sprintf("expected number, got %s", typeName(value))}
	}
}

func (n numericValue) toFloat64() float64 {
	if n.exact {
		return float64(n.numer) / float64(n.denom)
	}
	return n.inexact
}

func (n numericValue) isZero() bool {
	if n.exact {
		return n.numer == 0
	}
	return n.inexact == 0
}

func makeExactNumber(numer, denom int64) any {
	if denom == 0 {
		return rationalValue{numer: numer, denom: denom}
	}

	if numer == 0 {
		return int64(0)
	}

	if denom < 0 {
		numer = -numer
		denom = -denom
	}

	divisor := gcd64(numer, denom)
	numer /= divisor
	denom /= divisor

	if denom == 1 {
		return numer
	}
	return rationalValue{numer: numer, denom: denom}
}

func gcd64(a, b int64) int64 {
	if a < 0 {
		a = -a
	}
	if b < 0 {
		b = -b
	}
	if a == 0 {
		if b == 0 {
			return 1
		}
		return b
	}
	for b != 0 {
		a, b = b, a%b
	}
	return a
}

func negateNumber(value any) (any, error) {
	n, err := toNumericValue(value)
	if err != nil {
		return nil, err
	}

	if n.exact {
		return makeExactNumber(-n.numer, n.denom), nil
	}
	return -n.inexact, nil
}

func addNumberValues(left, right any) (any, error) {
	lhs, err := toNumericValue(left)
	if err != nil {
		return nil, err
	}
	rhs, err := toNumericValue(right)
	if err != nil {
		return nil, err
	}

	if !lhs.exact || !rhs.exact {
		return lhs.toFloat64() + rhs.toFloat64(), nil
	}

	return makeExactNumber(lhs.numer*rhs.denom+rhs.numer*lhs.denom, lhs.denom*rhs.denom), nil
}

func subtractNumberValues(left, right any) (any, error) {
	lhs, err := toNumericValue(left)
	if err != nil {
		return nil, err
	}
	rhs, err := toNumericValue(right)
	if err != nil {
		return nil, err
	}

	if !lhs.exact || !rhs.exact {
		return lhs.toFloat64() - rhs.toFloat64(), nil
	}

	return makeExactNumber(lhs.numer*rhs.denom-rhs.numer*lhs.denom, lhs.denom*rhs.denom), nil
}

func multiplyNumberValues(left, right any) (any, error) {
	lhs, err := toNumericValue(left)
	if err != nil {
		return nil, err
	}
	rhs, err := toNumericValue(right)
	if err != nil {
		return nil, err
	}

	if !lhs.exact || !rhs.exact {
		return lhs.toFloat64() * rhs.toFloat64(), nil
	}

	return makeExactNumber(lhs.numer*rhs.numer, lhs.denom*rhs.denom), nil
}

func divideNumberValues(left, right any) (any, error) {
	lhs, err := toNumericValue(left)
	if err != nil {
		return nil, err
	}
	rhs, err := toNumericValue(right)
	if err != nil {
		return nil, err
	}

	if rhs.isZero() {
		return nil, &EvalError{Message: "division by zero"}
	}

	if !lhs.exact || !rhs.exact {
		return lhs.toFloat64() / rhs.toFloat64(), nil
	}

	return makeExactNumber(lhs.numer*rhs.denom, lhs.denom*rhs.numer), nil
}

func compareNumberValues(left, right any) (int, error) {
	lhs, err := toNumericValue(left)
	if err != nil {
		return 0, err
	}
	rhs, err := toNumericValue(right)
	if err != nil {
		return 0, err
	}

	if lhs.exact && rhs.exact {
		leftScaled := lhs.numer * rhs.denom
		rightScaled := rhs.numer * lhs.denom
		switch {
		case leftScaled < rightScaled:
			return -1, nil
		case leftScaled > rightScaled:
			return 1, nil
		default:
			return 0, nil
		}
	}

	leftFloat := lhs.toFloat64()
	rightFloat := rhs.toFloat64()
	switch {
	case leftFloat < rightFloat:
		return -1, nil
	case leftFloat > rightFloat:
		return 1, nil
	default:
		return 0, nil
	}
}

func numberValuesEqual(left, right any) bool {
	cmp, err := compareNumberValues(left, right)
	return err == nil && cmp == 0
}

func exactToInexactValue(value any) (any, error) {
	n, err := toNumericValue(value)
	if err != nil {
		return nil, err
	}
	return n.toFloat64(), nil
}

func inexactToExactValue(value any) (any, error) {
	switch n := value.(type) {
	case int64, rationalValue:
		return value, nil
	case float64:
		return parseExactDecimal(formatInexactValue(n))
	default:
		return nil, &EvalError{Message: fmt.Sprintf("expected number, got %s", typeName(value))}
	}
}

func parseExactDecimal(text string) (any, error) {
	if text == "" {
		return nil, &EvalError{Message: "inexact->exact expects a finite decimal"}
	}

	sign := int64(1)
	if text[0] == '+' {
		text = text[1:]
	} else if text[0] == '-' {
		sign = -1
		text = text[1:]
	}

	exp := 0
	if index := strings.IndexAny(text, "eE"); index >= 0 {
		var err error
		exp, err = strconv.Atoi(text[index+1:])
		if err != nil {
			return nil, &EvalError{Message: "inexact->exact expects a finite decimal"}
		}
		text = text[:index]
	}

	whole := text
	fraction := ""
	if index := strings.IndexByte(text, '.'); index >= 0 {
		whole = text[:index]
		fraction = text[index+1:]
	}
	if whole == "" {
		whole = "0"
	}

	digits := whole + fraction
	if digits == "" {
		return nil, &EvalError{Message: "inexact->exact expects a finite decimal"}
	}

	numer, err := strconv.ParseInt(digits, 10, 64)
	if err != nil {
		return nil, &EvalError{Message: "inexact->exact expects a finite decimal"}
	}
	if sign < 0 {
		numer = -numer
	}

	denom := int64(1)
	for range fraction {
		denom *= 10
	}
	if exp > 0 {
		for i := 0; i < exp; i++ {
			numer *= 10
		}
	} else if exp < 0 {
		for i := 0; i < -exp; i++ {
			denom *= 10
		}
	}

	return makeExactNumber(numer, denom), nil
}

func formatNumberValue(value any) string {
	switch n := value.(type) {
	case int64:
		return strconv.FormatInt(n, 10)
	case rationalValue:
		return fmt.Sprintf("%d/%d", n.numer, n.denom)
	case float64:
		return formatInexactValue(n)
	default:
		return ""
	}
}

func formatInexactValue(value float64) string {
	text := strconv.FormatFloat(value, 'g', -1, 64)
	if !strings.ContainsAny(text, ".eE") {
		text += ".0"
	}
	return text
}

func builtinExactPredicate(args []any) (any, error) {
	return builtinPredicate("exact?", args, isExactNumberValue)
}

func builtinInexactPredicate(args []any) (any, error) {
	return builtinPredicate("inexact?", args, isInexactNumberValue)
}

func builtinIntegerPredicate(args []any) (any, error) {
	return builtinPredicate("integer?", args, isIntegerNumberValue)
}

func builtinRationalPredicate(args []any) (any, error) {
	return builtinPredicate("rational?", args, isRationalNumberValue)
}

func builtinExactToInexact(args []any) (any, error) {
	if len(args) != 1 {
		return nil, &EvalError{Message: "exact->inexact expects exactly 1 argument"}
	}
	return exactToInexactValue(args[0])
}

func builtinInexactToExact(args []any) (any, error) {
	if len(args) != 1 {
		return nil, &EvalError{Message: "inexact->exact expects exactly 1 argument"}
	}
	return inexactToExactValue(args[0])
}

func builtinNumerator(args []any) (any, error) {
	if len(args) != 1 {
		return nil, &EvalError{Message: "numerator expects exactly 1 argument"}
	}

	n, err := toNumericValue(args[0])
	if err != nil {
		return nil, err
	}
	if !n.exact {
		return nil, &EvalError{Message: "numerator expects an exact number"}
	}
	return makeExactNumber(n.numer, 1), nil
}

func builtinDenominator(args []any) (any, error) {
	if len(args) != 1 {
		return nil, &EvalError{Message: "denominator expects exactly 1 argument"}
	}

	n, err := toNumericValue(args[0])
	if err != nil {
		return nil, err
	}
	if !n.exact {
		return nil, &EvalError{Message: "denominator expects an exact number"}
	}
	return n.denom, nil
}

func builtinGCD(args []any) (any, error) {
	if len(args) == 0 {
		return int64(0), nil
	}

	result := int64(0)
	for _, arg := range args {
		value, err := expectInt(arg)
		if err != nil {
			return nil, err
		}
		result = gcd64(result, value)
	}
	if result < 0 {
		return -result, nil
	}
	return result, nil
}

func builtinLCM(args []any) (any, error) {
	if len(args) == 0 {
		return int64(1), nil
	}

	result := int64(1)
	for _, arg := range args {
		value, err := expectInt(arg)
		if err != nil {
			return nil, err
		}
		if result == 0 || value == 0 {
			result = 0
			continue
		}
		result = result / gcd64(result, value) * value
		if result < 0 {
			result = -result
		}
	}
	return result, nil
}

func builtinTruncate(args []any) (any, error) {
	if len(args) != 1 {
		return nil, &EvalError{Message: "truncate expects exactly 1 argument"}
	}

	switch n := args[0].(type) {
	case int64:
		return n, nil
	case rationalValue:
		return n.numer / n.denom, nil
	case float64:
		return int64(math.Trunc(n)), nil
	default:
		return nil, &EvalError{Message: fmt.Sprintf("expected number, got %s", typeName(args[0]))}
	}
}

func builtinRound(args []any) (any, error) {
	if len(args) != 1 {
		return nil, &EvalError{Message: "round expects exactly 1 argument"}
	}

	switch n := args[0].(type) {
	case int64:
		return n, nil
	case rationalValue:
		return int64(math.RoundToEven(float64(n.numer) / float64(n.denom))), nil
	case float64:
		return int64(math.RoundToEven(n)), nil
	default:
		return nil, &EvalError{Message: fmt.Sprintf("expected number, got %s", typeName(args[0]))}
	}
}
