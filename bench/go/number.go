package ming

import (
	"fmt"
	"strconv"
	"strings"
)

type numberValue struct {
	numer int
	denom int
	exact bool
}

type numberExpr = numberValue

func newExactInteger(n int) numberValue {
	return newNumberValue(n, 1, true)
}

func newNumberValue(numer, denom int, exact bool) numberValue {
	if denom == 0 {
		panic("numberValue with zero denominator")
	}
	if denom < 0 {
		numer = -numer
		denom = -denom
	}
	if numer == 0 {
		return numberValue{numer: 0, denom: 1, exact: exact}
	}

	divisor := gcd(absInt(numer), denom)
	return numberValue{
		numer: numer / divisor,
		denom: denom / divisor,
		exact: exact,
	}
}

func (n numberValue) schemeString() string {
	if n.exact {
		if n.denom == 1 {
			return strconv.Itoa(n.numer)
		}
		return strconv.Itoa(n.numer) + "/" + strconv.Itoa(n.denom)
	}
	return formatInexactFloat(n.float64())
}

func (numberValue) isTruthy() bool {
	return true
}

func (n numberValue) add(other numberValue) numberValue {
	return newNumberValue(
		n.numer*other.denom+other.numer*n.denom,
		n.denom*other.denom,
		n.exact && other.exact,
	)
}

func (n numberValue) sub(other numberValue) numberValue {
	return newNumberValue(
		n.numer*other.denom-other.numer*n.denom,
		n.denom*other.denom,
		n.exact && other.exact,
	)
}

func (n numberValue) mul(other numberValue) numberValue {
	return newNumberValue(
		n.numer*other.numer,
		n.denom*other.denom,
		n.exact && other.exact,
	)
}

func (n numberValue) div(other numberValue) numberValue {
	return newNumberValue(
		n.numer*other.denom,
		n.denom*other.numer,
		n.exact && other.exact,
	)
}

func (n numberValue) negate() numberValue {
	return newNumberValue(-n.numer, n.denom, n.exact)
}

func (n numberValue) abs() numberValue {
	if n.numer < 0 {
		return n.negate()
	}
	return n
}

func (n numberValue) compare(other numberValue) int {
	left := n.numer * other.denom
	right := other.numer * n.denom
	switch {
	case left < right:
		return -1
	case left > right:
		return 1
	default:
		return 0
	}
}

func (n numberValue) equal(other numberValue) bool {
	return n.compare(other) == 0
}

func (n numberValue) isInteger() bool {
	return n.denom == 1
}

func (n numberValue) float64() float64 {
	return float64(n.numer) / float64(n.denom)
}

func (n numberValue) toExact() numberValue {
	n.exact = true
	return n
}

func (n numberValue) toInexact() numberValue {
	n.exact = false
	return n
}

func parseNumberLiteral(raw string) (numberValue, bool, error) {
	if value, ok, err := parseRationalLiteral(raw); ok || err != nil {
		return value, ok, err
	}
	if value, ok, err := parseDecimalLiteral(raw); ok || err != nil {
		return value, ok, err
	}
	if n, err := strconv.Atoi(raw); err == nil {
		return newExactInteger(n), true, nil
	}
	return numberValue{}, false, nil
}

func parseRationalLiteral(raw string) (numberValue, bool, error) {
	if strings.Count(raw, "/") != 1 {
		return numberValue{}, false, nil
	}

	sign, body := splitNumericSign(raw)
	parts := strings.SplitN(body, "/", 2)
	if !digitsOnly(parts[0]) || !digitsOnly(parts[1]) {
		return numberValue{}, false, nil
	}

	numer, _ := strconv.Atoi(parts[0])
	denom, _ := strconv.Atoi(parts[1])
	if denom == 0 {
		return numberValue{}, true, fmt.Errorf("invalid rational literal")
	}

	return newNumberValue(sign*numer, denom, true), true, nil
}

func parseDecimalLiteral(raw string) (numberValue, bool, error) {
	if strings.Count(raw, ".") != 1 {
		return numberValue{}, false, nil
	}

	sign, body := splitNumericSign(raw)
	parts := strings.SplitN(body, ".", 2)
	if parts[0] == "" && parts[1] == "" {
		return numberValue{}, false, nil
	}
	if parts[0] != "" && !digitsOnly(parts[0]) {
		return numberValue{}, false, nil
	}
	if parts[1] != "" && !digitsOnly(parts[1]) {
		return numberValue{}, false, nil
	}

	intPart := 0
	if parts[0] != "" {
		intPart, _ = strconv.Atoi(parts[0])
	}

	fracPart := 0
	if parts[1] != "" {
		fracPart, _ = strconv.Atoi(parts[1])
	}

	denom := 1
	for i := 0; i < len(parts[1]); i++ {
		denom *= 10
	}

	numer := intPart*denom + fracPart
	if sign < 0 {
		numer = -numer
	}
	return newNumberValue(numer, denom, false), true, nil
}

func formatInexactFloat(f float64) string {
	text := strconv.FormatFloat(f, 'g', -1, 64)
	if !strings.ContainsAny(text, ".eE") {
		text += ".0"
	}
	return text
}

func splitNumericSign(raw string) (int, string) {
	switch {
	case strings.HasPrefix(raw, "+"):
		return 1, raw[1:]
	case strings.HasPrefix(raw, "-"):
		return -1, raw[1:]
	default:
		return 1, raw
	}
}

func digitsOnly(text string) bool {
	if text == "" {
		return false
	}
	for _, ch := range text {
		if ch < '0' || ch > '9' {
			return false
		}
	}
	return true
}

func gcd(a, b int) int {
	for b != 0 {
		a, b = b, a%b
	}
	if a < 0 {
		return -a
	}
	return a
}

func absInt(n int) int {
	if n < 0 {
		return -n
	}
	return n
}
