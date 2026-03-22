package ming

import (
	"math"
	"strconv"
	"strings"
)

// gcd computes the greatest common divisor (always positive).
func gcd(a, b int64) int64 {
	if a < 0 {
		a = -a
	}
	if b < 0 {
		b = -b
	}
	for b != 0 {
		a, b = b, a%b
	}
	return a
}

// RationalValue creates a reduced rational or integer value.
// Denominator must not be zero.
func RationalValue(num, den int64) *Value {
	if den < 0 {
		num, den = -num, -den
	}
	g := gcd(num, den)
	num /= g
	den /= g
	if den == 1 {
		return IntegerValue(num)
	}
	return &Value{Type: TypeRational, Num: num, Den: den}
}

// FloatValue creates an inexact float value.
func FloatValue(f float64) *Value {
	return &Value{Type: TypeFloat, FloatVal: f}
}

// isNumeric returns true if the value is any numeric type.
func isNumeric(v *Value) bool {
	return v.Type == TypeInteger || v.Type == TypeRational || v.Type == TypeFloat
}

// toFloat converts any numeric value to float64.
func toFloat(v *Value) float64 {
	switch v.Type {
	case TypeInteger:
		return float64(v.IntVal)
	case TypeRational:
		return float64(v.Num) / float64(v.Den)
	case TypeFloat:
		return v.FloatVal
	}
	return 0
}

// isExact returns true if the numeric value is exact.
func isExact(v *Value) bool {
	return v.Type == TypeInteger || v.Type == TypeRational
}

// numAdd adds two numeric values, preserving exactness when possible.
func numAdd(a, b *Value) *Value {
	if a.Type == TypeFloat || b.Type == TypeFloat {
		return FloatValue(toFloat(a) + toFloat(b))
	}
	// Both exact
	an, ad := toRational(a)
	bn, bd := toRational(b)
	return RationalValue(an*bd+bn*ad, ad*bd)
}

// numSub subtracts two numeric values.
func numSub(a, b *Value) *Value {
	if a.Type == TypeFloat || b.Type == TypeFloat {
		return FloatValue(toFloat(a) - toFloat(b))
	}
	an, ad := toRational(a)
	bn, bd := toRational(b)
	return RationalValue(an*bd-bn*ad, ad*bd)
}

// numMul multiplies two numeric values.
func numMul(a, b *Value) *Value {
	if a.Type == TypeFloat || b.Type == TypeFloat {
		return FloatValue(toFloat(a) * toFloat(b))
	}
	an, ad := toRational(a)
	bn, bd := toRational(b)
	return RationalValue(an*bn, ad*bd)
}

// numDiv divides two numeric values.
func numDiv(a, b *Value) *Value {
	if a.Type == TypeFloat || b.Type == TypeFloat {
		return FloatValue(toFloat(a) / toFloat(b))
	}
	an, ad := toRational(a)
	bn, bd := toRational(b)
	return RationalValue(an*bd, ad*bn)
}

// numNeg negates a numeric value.
func numNeg(a *Value) *Value {
	switch a.Type {
	case TypeInteger:
		return IntegerValue(-a.IntVal)
	case TypeRational:
		return RationalValue(-a.Num, a.Den)
	case TypeFloat:
		return FloatValue(-a.FloatVal)
	}
	return a
}

// toRational converts an exact number to numerator/denominator.
func toRational(v *Value) (int64, int64) {
	switch v.Type {
	case TypeInteger:
		return v.IntVal, 1
	case TypeRational:
		return v.Num, v.Den
	}
	return 0, 1
}

// numCmpFloat compares two numeric values as floats.
func numCmpFloat(a, b *Value) float64 {
	return toFloat(a) - toFloat(b)
}

// numEqual tests numeric equality across types.
func numEqual(a, b *Value) bool {
	if a.Type == TypeFloat || b.Type == TypeFloat {
		return toFloat(a) == toFloat(b)
	}
	an, ad := toRational(a)
	bn, bd := toRational(b)
	return an*bd == bn*ad
}

// numLess tests if a < b across types.
func numLess(a, b *Value) bool {
	if a.Type == TypeFloat || b.Type == TypeFloat {
		return toFloat(a) < toFloat(b)
	}
	an, ad := toRational(a)
	bn, bd := toRational(b)
	// ad and bd are always positive
	return an*bd < bn*ad
}

// numGreater tests if a > b.
func numGreater(a, b *Value) bool {
	return numLess(b, a)
}

// floatToExact converts a float to exact representation.
func floatToExact(f float64) *Value {
	// Check if it's an integer
	if f == math.Trunc(f) && !math.IsInf(f, 0) && !math.IsNaN(f) {
		return IntegerValue(int64(f))
	}
	// Convert using continued fractions / rational approximation
	// Use the standard approach: multiply by power of 10, then reduce
	s := strconv.FormatFloat(f, 'f', -1, 64)
	parts := strings.Split(s, ".")
	if len(parts) == 1 {
		n, _ := strconv.ParseInt(parts[0], 10, 64)
		return IntegerValue(n)
	}
	decimals := len(parts[1])
	den := int64(1)
	for i := 0; i < decimals; i++ {
		den *= 10
	}
	whole, _ := strconv.ParseInt(parts[0]+parts[1], 10, 64)
	return RationalValue(whole, den)
}

// formatFloat formats a float for Scheme display.
func formatFloat(f float64) string {
	s := strconv.FormatFloat(f, 'f', -1, 64)
	if !strings.Contains(s, ".") {
		s += ".0"
	}
	return s
}

// isNumberStr checks if a string is a valid number (int, rational, or float).
func isNumberStr(s string) bool {
	return isIntegerStr(s) || isRationalStr(s) || isFloatStr(s)
}

func isIntegerStr(s string) bool {
	if len(s) == 0 {
		return false
	}
	start := 0
	if s[0] == '-' || s[0] == '+' {
		if len(s) == 1 {
			return false
		}
		start = 1
	}
	for i := start; i < len(s); i++ {
		if s[i] < '0' || s[i] > '9' {
			return false
		}
	}
	return true
}

func isRationalStr(s string) bool {
	idx := strings.IndexByte(s, '/')
	if idx < 0 {
		return false
	}
	num := s[:idx]
	den := s[idx+1:]
	return isIntegerStr(num) && isIntegerStr(den) && len(den) > 0 && den[0] != '-' && den[0] != '+'
}

func isFloatStr(s string) bool {
	if len(s) == 0 {
		return false
	}
	start := 0
	if s[0] == '-' || s[0] == '+' {
		if len(s) == 1 {
			return false
		}
		start = 1
	}
	dotCount := 0
	digitCount := 0
	for i := start; i < len(s); i++ {
		if s[i] == '.' {
			dotCount++
			if dotCount > 1 {
				return false
			}
		} else if s[i] >= '0' && s[i] <= '9' {
			digitCount++
		} else {
			return false
		}
	}
	return dotCount == 1 && digitCount > 0
}

func parseRational(s string) (int64, int64) {
	idx := strings.IndexByte(s, '/')
	num, _ := strconv.ParseInt(s[:idx], 10, 64)
	den, _ := strconv.ParseInt(s[idx+1:], 10, 64)
	return num, den
}

func parseFloatStr(s string) float64 {
	f, _ := strconv.ParseFloat(s, 64)
	return f
}

// numeratorOf returns the numerator of a numeric value.
func numeratorOf(v *Value) int64 {
	switch v.Type {
	case TypeInteger:
		return v.IntVal
	case TypeRational:
		return v.Num
	}
	return 0
}

// denominatorOf returns the denominator of a numeric value.
func denominatorOf(v *Value) int64 {
	switch v.Type {
	case TypeInteger:
		return 1
	case TypeRational:
		return v.Den
	}
	return 1
}

// isIntegerValue returns true if the value is an exact integer (including rational with den=1).
func isIntegerValue(v *Value) bool {
	switch v.Type {
	case TypeInteger:
		return true
	case TypeRational:
		return v.Den == 1 // should not happen due to RationalValue normalization
	case TypeFloat:
		return v.FloatVal == math.Trunc(v.FloatVal) && !math.IsInf(v.FloatVal, 0) && !math.IsNaN(v.FloatVal)
	}
	return false
}

func exactToInexact(v *Value) *Value {
	return FloatValue(toFloat(v))
}

// formatExactToInexact formats an exact value as inexact, ensuring ".0" suffix for integers.
func formatExactToInexact(v *Value) string {
	return formatFloat(toFloat(v))
}

// exactToInexactDisplay returns the display form of exact->inexact conversion.
func exactToInexactValue(v *Value) *Value {
	return FloatValue(toFloat(v))
}

// makeVector is used elsewhere; this file only has number helpers.

// numIsZero returns true if the numeric value is zero.
func numIsZero(v *Value) bool {
	switch v.Type {
	case TypeInteger:
		return v.IntVal == 0
	case TypeRational:
		return v.Num == 0
	case TypeFloat:
		return v.FloatVal == 0
	}
	return false
}

