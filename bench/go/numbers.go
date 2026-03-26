package ming

import (
	"math"
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

func parseNumberLiteral(text string) (any, bool) {
	if value, err := strconv.Atoi(text); err == nil {
		return value, true
	}
	if value, ok := parseRationalLiteral(text); ok {
		return value, true
	}
	if value, ok := parseInexactLiteral(text); ok {
		return value, true
	}
	return nil, false
}

func parseRationalLiteral(text string) (any, bool) {
	if strings.Count(text, "/") != 1 {
		return nil, false
	}

	parts := strings.SplitN(text, "/", 2)
	if parts[0] == "" || parts[1] == "" {
		return nil, false
	}

	numerator, err := strconv.Atoi(parts[0])
	if err != nil {
		return nil, false
	}
	denominator, err := strconv.Atoi(parts[1])
	if err != nil || denominator == 0 {
		return nil, false
	}

	return makeExactNumber(numerator, denominator), true
}

func parseInexactLiteral(text string) (inexactValue, bool) {
	if strings.Contains(text, "/") || !strings.ContainsAny(text, ".eE") {
		return 0, false
	}

	value, err := strconv.ParseFloat(text, 64)
	if err != nil || math.IsNaN(value) || math.IsInf(value, 0) {
		return 0, false
	}

	return inexactValue(normalizeInexactFloat(value)), true
}

func numericFromValue(value any) (numericValue, bool) {
	switch n := value.(type) {
	case int:
		return numericValue{
			exact:       true,
			numerator:   n,
			denominator: 1,
		}, true
	case rationalValue:
		return numericValue{
			exact:       true,
			numerator:   n.numerator,
			denominator: n.denominator,
		}, true
	case inexactValue:
		return numericValue{
			exact:   false,
			inexact: normalizeInexactFloat(float64(n)),
		}, true
	default:
		return numericValue{}, false
	}
}

func makeExactNumber(numerator, denominator int) any {
	numerator, denominator = normalizeRational(numerator, denominator)
	if denominator == 1 {
		return numerator
	}
	return rationalValue{
		numerator:   numerator,
		denominator: denominator,
	}
}

func exactNumeric(numerator, denominator int) numericValue {
	numerator, denominator = normalizeRational(numerator, denominator)
	return numericValue{
		exact:       true,
		numerator:   numerator,
		denominator: denominator,
	}
}

func numericResult(value numericValue) any {
	if value.exact {
		return makeExactNumber(value.numerator, value.denominator)
	}
	return inexactValue(normalizeInexactFloat(value.inexact))
}

func normalizeRational(numerator, denominator int) (int, int) {
	if denominator < 0 {
		numerator = -numerator
		denominator = -denominator
	}
	if numerator == 0 {
		return 0, 1
	}

	divisor := gcd(absInt(numerator), denominator)
	return numerator / divisor, denominator / divisor
}

func gcd(left, right int) int {
	for right != 0 {
		left, right = right, left%right
	}
	if left < 0 {
		return -left
	}
	return left
}

func absInt(value int) int {
	if value < 0 {
		return -value
	}
	return value
}

func normalizeInexactFloat(value float64) float64 {
	if value == 0 {
		return 0
	}
	return value
}

func (n numericValue) asFloat64() float64 {
	if n.exact {
		return float64(n.numerator) / float64(n.denominator)
	}
	return n.inexact
}

func (n numericValue) isZero() bool {
	if n.exact {
		return n.numerator == 0
	}
	return n.inexact == 0
}

func (n numericValue) isInteger() bool {
	if n.exact {
		return n.denominator == 1
	}
	if math.IsNaN(n.inexact) || math.IsInf(n.inexact, 0) {
		return false
	}
	return math.Trunc(n.inexact) == n.inexact
}

func compareNumericValues(left, right numericValue) int {
	if left.exact && right.exact {
		lhs := left.numerator * right.denominator
		rhs := right.numerator * left.denominator
		switch {
		case lhs < rhs:
			return -1
		case lhs > rhs:
			return 1
		default:
			return 0
		}
	}

	leftFloat := left.asFloat64()
	rightFloat := right.asFloat64()
	switch {
	case leftFloat < rightFloat:
		return -1
	case leftFloat > rightFloat:
		return 1
	default:
		return 0
	}
}

func addNumericValues(left, right numericValue) numericValue {
	if !left.exact || !right.exact {
		return numericValue{
			inexact: normalizeInexactFloat(left.asFloat64() + right.asFloat64()),
		}
	}
	return exactNumeric(
		left.numerator*right.denominator+right.numerator*left.denominator,
		left.denominator*right.denominator,
	)
}

func subtractNumericValues(left, right numericValue) numericValue {
	if !left.exact || !right.exact {
		return numericValue{
			inexact: normalizeInexactFloat(left.asFloat64() - right.asFloat64()),
		}
	}
	return exactNumeric(
		left.numerator*right.denominator-right.numerator*left.denominator,
		left.denominator*right.denominator,
	)
}

func multiplyNumericValues(left, right numericValue) numericValue {
	if !left.exact || !right.exact {
		return numericValue{
			inexact: normalizeInexactFloat(left.asFloat64() * right.asFloat64()),
		}
	}
	return exactNumeric(
		left.numerator*right.numerator,
		left.denominator*right.denominator,
	)
}

func divideNumericValues(left, right numericValue) numericValue {
	if !left.exact || !right.exact {
		return numericValue{
			inexact: normalizeInexactFloat(left.asFloat64() / right.asFloat64()),
		}
	}
	return exactNumeric(
		left.numerator*right.denominator,
		left.denominator*right.numerator,
	)
}

func exactToInexact(value any) (any, bool) {
	numeric, ok := numericFromValue(value)
	if !ok {
		return nil, false
	}
	if !numeric.exact {
		return inexactValue(normalizeInexactFloat(numeric.inexact)), true
	}
	return inexactValue(normalizeInexactFloat(numeric.asFloat64())), true
}

func inexactToExact(value any) (any, bool) {
	numeric, ok := numericFromValue(value)
	if !ok {
		return nil, false
	}
	if numeric.exact {
		return makeExactNumber(numeric.numerator, numeric.denominator), true
	}
	return decimalStringToExact(formatInexactNumber(inexactValue(numeric.inexact))), true
}

func decimalStringToExact(text string) any {
	sign := 1
	if strings.HasPrefix(text, "+") {
		text = text[1:]
	} else if strings.HasPrefix(text, "-") {
		sign = -1
		text = text[1:]
	}

	if !strings.Contains(text, ".") {
		value, err := strconv.Atoi(text)
		if err != nil {
			return 0
		}
		return sign * value
	}

	parts := strings.SplitN(text, ".", 2)
	digits := parts[0] + parts[1]
	if digits == "" {
		digits = "0"
	}
	value, err := strconv.Atoi(digits)
	if err != nil {
		return 0
	}
	value *= sign

	return makeExactNumber(value, pow10(len(parts[1])))
}

func pow10(power int) int {
	result := 1
	for step := 0; step < power; step++ {
		result *= 10
	}
	return result
}

func formatNumericValue(value any) (string, bool) {
	switch n := value.(type) {
	case int:
		return strconv.Itoa(n), true
	case rationalValue:
		return strconv.Itoa(n.numerator) + "/" + strconv.Itoa(n.denominator), true
	case inexactValue:
		return formatInexactNumber(n), true
	default:
		return "", false
	}
}

func formatInexactNumber(value inexactValue) string {
	text := strconv.FormatFloat(normalizeInexactFloat(float64(value)), 'f', -1, 64)
	if !strings.ContainsAny(text, ".eE") {
		text += ".0"
	}
	return text
}
