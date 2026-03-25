package ming

import (
	"fmt"
	"math"
	"strings"
	"strconv"
)

// gcd computes greatest common divisor of two non-negative integers.
func gcd(a, b int64) int64 {
	for b != 0 {
		a, b = b, a%b
	}
	return a
}

func abs64(x int64) int64 {
	if x < 0 {
		return -x
	}
	return x
}

// RationalValue creates a simplified rational. Returns an integer if den==1.
func RationalValue(num, den int64) *Value {
	if den == 0 {
		panic("zero denominator")
	}
	if den < 0 {
		num, den = -num, -den
	}
	if num == 0 {
		return IntValue(0)
	}
	g := gcd(abs64(num), den)
	num /= g
	den /= g
	if den == 1 {
		return IntValue(num)
	}
	return &Value{Type: TypeRational, Num: num, Den: den}
}

func FloatValue(f float64) *Value {
	return &Value{Type: TypeFloat, FloatVal: f}
}

// isNumeric checks if value is any numeric type.
func isNumeric(v *Value) bool {
	return v.Type == TypeInteger || v.Type == TypeRational || v.Type == TypeFloat
}

// toFloat64 converts any numeric value to float64.
func toFloat64(v *Value) float64 {
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

// toRational converts integer or rational to (num, den). Panics on float.
func toRational(v *Value) (int64, int64) {
	switch v.Type {
	case TypeInteger:
		return v.IntVal, 1
	case TypeRational:
		return v.Num, v.Den
	}
	panic("toRational on non-exact")
}

// hasFloat returns true if any value is a float.
func hasFloat(args []*Value) bool {
	for _, a := range args {
		if a.Type == TypeFloat {
			return true
		}
	}
	return false
}

// hasRational returns true if any value is a rational.
func hasRational(args []*Value) bool {
	for _, a := range args {
		if a.Type == TypeRational {
			return true
		}
	}
	return false
}

// addRat adds two rationals (num1/den1) + (num2/den2) and returns simplified.
func addRat(n1, d1, n2, d2 int64) (int64, int64) {
	num := n1*d2 + n2*d1
	den := d1 * d2
	if num == 0 {
		return 0, 1
	}
	g := gcd(abs64(num), abs64(den))
	return num / g, den / g
}

// subRat subtracts two rationals (num1/den1) - (num2/den2).
func subRat(n1, d1, n2, d2 int64) (int64, int64) {
	return addRat(n1, d1, -n2, d2)
}

// mulRat multiplies two rationals.
func mulRat(n1, d1, n2, d2 int64) (int64, int64) {
	num := n1 * n2
	den := d1 * d2
	if num == 0 {
		return 0, 1
	}
	g := gcd(abs64(num), abs64(den))
	return num / g, den / g
}

// divRat divides two rationals.
func divRat(n1, d1, n2, d2 int64) (int64, int64) {
	return mulRat(n1, d1, d2, n2)
}

// numericEqual compares two numeric values for equality.
func numericEqual(a, b *Value) bool {
	if a.Type == TypeFloat || b.Type == TypeFloat {
		return toFloat64(a) == toFloat64(b)
	}
	an, ad := toRational(a)
	bn, bd := toRational(b)
	return an*bd == bn*ad
}

// numericLess compares a < b for any numeric types.
func numericLess(a, b *Value) bool {
	if a.Type == TypeFloat || b.Type == TypeFloat {
		return toFloat64(a) < toFloat64(b)
	}
	an, ad := toRational(a)
	bn, bd := toRational(b)
	return an*bd < bn*ad
}

// formatFloat formats a float for display.
func formatFloat(f float64) string {
	s := strconv.FormatFloat(f, 'f', -1, 64)
	// Ensure there's a decimal point
	if !strings.Contains(s, ".") {
		s += ".0"
	}
	return s
}

// floatToRational converts a float64 to exact rational using binary decomposition.
func floatToRational(f float64) (int64, int64) {
	if f == 0 {
		return 0, 1
	}
	neg := f < 0
	if neg {
		f = -f
	}

	// Use the binary representation
	bits := math.Float64bits(f)
	mantissa := int64(bits&((1<<52)-1)) | (1 << 52)
	exp := int((bits>>52)&0x7ff) - 1023 - 52

	var num, den int64
	if exp >= 0 {
		if exp > 40 { // avoid overflow
			num = int64(f)
			den = 1
		} else {
			num = mantissa << uint(exp)
			den = 1
		}
	} else {
		num = mantissa
		aexp := -exp
		if aexp > 62 { // avoid overflow
			// fallback: use decimal approximation
			return decimalToRational(f, neg)
		}
		den = int64(1) << uint(aexp)
	}

	g := gcd(abs64(num), abs64(den))
	num /= g
	den /= g

	if neg {
		num = -num
	}
	return num, den
}

func decimalToRational(f float64, neg bool) (int64, int64) {
	s := fmt.Sprintf("%.15g", f)
	// Find decimal point
	dot := strings.Index(s, ".")
	if dot < 0 {
		n, _ := strconv.ParseInt(s, 10, 64)
		if neg {
			n = -n
		}
		return n, 1
	}
	decimals := len(s) - dot - 1
	// Remove dot
	s = s[:dot] + s[dot+1:]
	num, _ := strconv.ParseInt(s, 10, 64)
	den := int64(1)
	for i := 0; i < decimals; i++ {
		den *= 10
	}
	g := gcd(abs64(num), den)
	num /= g
	den /= g
	if neg {
		num = -num
	}
	return num, den
}
