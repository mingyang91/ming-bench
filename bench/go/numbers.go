package ming

import (
	"fmt"
	"strconv"
	"strings"
)

type rationalValue struct {
	numerator   int
	denominator int
}

type inexactValue float64

type numericValue struct {
	exact       bool
	numerator   int
	denominator int
	inexact     float64
}

func isNumberValue(v value) bool {
	switch v.(type) {
	case integerValue, rationalValue, inexactValue:
		return true
	default:
		return false
	}
}

func isExactNumber(v value) bool {
	switch v.(type) {
	case integerValue, rationalValue:
		return true
	default:
		return false
	}
}

func isInexactNumber(v value) bool {
	_, ok := v.(inexactValue)
	return ok
}

func isIntegerNumber(v value) bool {
	switch v := v.(type) {
	case integerValue:
		return true
	case rationalValue:
		return v.denominator == 1
	default:
		return false
	}
}

func isRationalNumber(v value) bool {
	switch v.(type) {
	case integerValue, rationalValue:
		return true
	default:
		return false
	}
}

func parseNumericToken(token string) (value, bool, error) {
	if rational, ok, err := parseRationalToken(token); ok || err != nil {
		return rational, ok, err
	}
	if inexact, ok := parseInexactToken(token); ok {
		return inexact, true, nil
	}
	if isIntegerLiteral(token) {
		n, err := strconv.Atoi(token)
		if err != nil {
			return nil, true, fmt.Errorf("invalid integer: %s", token)
		}
		return integerValue(n), true, nil
	}
	return nil, false, nil
}

func parseRationalToken(token string) (value, bool, error) {
	if strings.Count(token, "/") != 1 {
		return nil, false, nil
	}

	parts := strings.SplitN(token, "/", 2)
	if len(parts) != 2 || parts[0] == "" || parts[1] == "" {
		return nil, false, nil
	}
	if !isIntegerLiteral(parts[0]) || !isIntegerLiteral(parts[1]) {
		return nil, false, nil
	}

	numerator, err := strconv.Atoi(parts[0])
	if err != nil {
		return nil, true, fmt.Errorf("invalid rational: %s", token)
	}
	denominator, err := strconv.Atoi(parts[1])
	if err != nil {
		return nil, true, fmt.Errorf("invalid rational: %s", token)
	}
	if denominator == 0 {
		return nil, true, fmt.Errorf("invalid rational: %s", token)
	}

	return makeExactNumber(numerator, denominator), true, nil
}

func parseInexactToken(token string) (value, bool) {
	if !strings.ContainsRune(token, '.') {
		return nil, false
	}

	f, err := strconv.ParseFloat(token, 64)
	if err != nil {
		return nil, false
	}
	return inexactValue(f), true
}

func expectNumberValue(v value) (numericValue, error) {
	switch v := v.(type) {
	case integerValue:
		return exactNumericValue(int(v), 1), nil
	case rationalValue:
		return exactNumericValue(v.numerator, v.denominator), nil
	case inexactValue:
		return numericValue{exact: false, inexact: float64(v)}, nil
	default:
		return numericValue{}, &EvalError{Message: "expected number"}
	}
}

func exactNumericValue(numerator, denominator int) numericValue {
	numerator, denominator = normalizeRationalParts(numerator, denominator)
	return numericValue{
		exact:       true,
		numerator:   numerator,
		denominator: denominator,
	}
}

func normalizeRationalParts(numerator, denominator int) (int, int) {
	if denominator < 0 {
		numerator = -numerator
		denominator = -denominator
	}
	if numerator == 0 {
		return 0, 1
	}

	divisor := gcd(absInt(numerator), absInt(denominator))
	return numerator / divisor, denominator / divisor
}

func makeExactNumber(numerator, denominator int) value {
	numerator, denominator = normalizeRationalParts(numerator, denominator)
	if denominator == 1 {
		return integerValue(numerator)
	}
	return rationalValue{
		numerator:   numerator,
		denominator: denominator,
	}
}

func absInt(n int) int {
	if n < 0 {
		return -n
	}
	return n
}

func gcd(a, b int) int {
	if a == 0 {
		if b == 0 {
			return 1
		}
		return b
	}

	for b != 0 {
		a, b = b, a%b
	}
	if a < 0 {
		return -a
	}
	return a
}

func (n numericValue) asFloat64() float64 {
	if n.exact {
		return float64(n.numerator) / float64(n.denominator)
	}
	return n.inexact
}

func (n numericValue) toValue() value {
	if !n.exact {
		return inexactValue(n.inexact)
	}
	return makeExactNumber(n.numerator, n.denominator)
}

func addNumeric(left, right numericValue) numericValue {
	if !left.exact || !right.exact {
		return numericValue{exact: false, inexact: left.asFloat64() + right.asFloat64()}
	}
	return exactNumericValue(
		left.numerator*right.denominator+right.numerator*left.denominator,
		left.denominator*right.denominator,
	)
}

func subNumeric(left, right numericValue) numericValue {
	if !left.exact || !right.exact {
		return numericValue{exact: false, inexact: left.asFloat64() - right.asFloat64()}
	}
	return exactNumericValue(
		left.numerator*right.denominator-right.numerator*left.denominator,
		left.denominator*right.denominator,
	)
}

func mulNumeric(left, right numericValue) numericValue {
	if !left.exact || !right.exact {
		return numericValue{exact: false, inexact: left.asFloat64() * right.asFloat64()}
	}
	return exactNumericValue(
		left.numerator*right.numerator,
		left.denominator*right.denominator,
	)
}

func divNumeric(left, right numericValue) (numericValue, error) {
	if (right.exact && right.numerator == 0) || (!right.exact && right.inexact == 0) {
		return numericValue{}, &EvalError{Message: "division by zero"}
	}
	if !left.exact || !right.exact {
		return numericValue{exact: false, inexact: left.asFloat64() / right.asFloat64()}, nil
	}
	return exactNumericValue(
		left.numerator*right.denominator,
		left.denominator*right.numerator,
	), nil
}

func compareNumeric(left, right numericValue, name string) bool {
	if left.exact && right.exact {
		leftValue := left.numerator * right.denominator
		rightValue := right.numerator * left.denominator
		switch name {
		case "<":
			return leftValue < rightValue
		case ">":
			return leftValue > rightValue
		case "=":
			return leftValue == rightValue
		case "<=":
			return leftValue <= rightValue
		case ">=":
			return leftValue >= rightValue
		default:
			return false
		}
	}

	leftFloat := left.asFloat64()
	rightFloat := right.asFloat64()
	switch name {
	case "<":
		return leftFloat < rightFloat
	case ">":
		return leftFloat > rightFloat
	case "=":
		return leftFloat == rightFloat
	case "<=":
		return leftFloat <= rightFloat
	case ">=":
		return leftFloat >= rightFloat
	default:
		return false
	}
}

func formatNumberValue(v value) (string, bool) {
	switch v := v.(type) {
	case integerValue:
		return strconv.Itoa(int(v)), true
	case rationalValue:
		return fmt.Sprintf("%d/%d", v.numerator, v.denominator), true
	case inexactValue:
		return formatInexactNumber(float64(v)), true
	default:
		return "", false
	}
}

func formatInexactNumber(v float64) string {
	text := strconv.FormatFloat(v, 'f', 16, 64)
	text = strings.TrimRight(text, "0")
	if strings.HasSuffix(text, ".") {
		text += "0"
	}
	if text == "-0.0" {
		return "0.0"
	}
	return text
}

func exactToInexact(v value) (value, error) {
	number, err := expectNumberValue(v)
	if err != nil {
		return nil, err
	}
	if !number.exact {
		return v, nil
	}
	return inexactValue(number.asFloat64()), nil
}

func inexactToExact(v value) (value, error) {
	number, err := expectNumberValue(v)
	if err != nil {
		return nil, err
	}
	if number.exact {
		return number.toValue(), nil
	}
	return exactFromDecimalString(formatInexactNumber(number.inexact))
}

func exactFromDecimalString(text string) (value, error) {
	if text == "" {
		return nil, &EvalError{Message: "invalid inexact number"}
	}

	sign := 1
	if strings.HasPrefix(text, "-") {
		sign = -1
		text = text[1:]
	}

	parts := strings.SplitN(text, ".", 2)
	wholePart := parts[0]
	if wholePart == "" {
		wholePart = "0"
	}

	whole, err := strconv.Atoi(wholePart)
	if err != nil {
		return nil, &EvalError{Message: "invalid inexact number"}
	}

	if len(parts) == 1 {
		return integerValue(sign * whole), nil
	}

	fractional := strings.TrimRight(parts[1], "0")
	if fractional == "" {
		return integerValue(sign * whole), nil
	}

	scale := 1
	for i := 0; i < len(fractional); i++ {
		scale *= 10
	}

	fractionValue, err := strconv.Atoi(fractional)
	if err != nil {
		return nil, &EvalError{Message: "invalid inexact number"}
	}

	numerator := whole*scale + fractionValue
	return makeExactNumber(sign*numerator, scale), nil
}

func rationalParts(v value) (int, int, error) {
	switch v := v.(type) {
	case integerValue:
		return int(v), 1, nil
	case rationalValue:
		return v.numerator, v.denominator, nil
	default:
		return 0, 0, &EvalError{Message: "expected rational"}
	}
}
