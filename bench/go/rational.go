package ming

import (
	"math"
	"math/big"
)

// Numeric tower helpers for exact/inexact arithmetic.

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

// makeRational creates a simplified rational. Returns SchemeInt if den==1.
func makeRational(num, den int64) SchemeValue {
	if den < 0 {
		num, den = -num, -den
	}
	g := gcd(num, den)
	if g != 0 {
		num, den = num/g, den/g
	}
	if den == 1 {
		return &SchemeInt{Value: num}
	}
	return &SchemeRational{Num: num, Den: den}
}

// isNumber checks if a SchemeValue is any numeric type.
func isNumber(v SchemeValue) bool {
	switch v.(type) {
	case *SchemeInt, *SchemeRational, *SchemeFloat:
		return true
	}
	return false
}

// toFloat64 converts any number to float64.
func toFloat64(v SchemeValue) (float64, bool) {
	switch n := v.(type) {
	case *SchemeInt:
		return float64(n.Value), true
	case *SchemeRational:
		return float64(n.Num) / float64(n.Den), true
	case *SchemeFloat:
		return n.Value, true
	}
	return 0, false
}

// isExact returns true for exact numbers (int, rational).
func isExact(v SchemeValue) bool {
	switch v.(type) {
	case *SchemeInt, *SchemeRational:
		return true
	}
	return false
}

// numAdd adds two SchemeValues (must be numbers). Returns result.
func numAdd(a, b SchemeValue) SchemeValue {
	// If either is inexact, result is inexact
	if _, ok := a.(*SchemeFloat); ok {
		af, _ := toFloat64(a)
		bf, _ := toFloat64(b)
		return &SchemeFloat{Value: af + bf}
	}
	if _, ok := b.(*SchemeFloat); ok {
		af, _ := toFloat64(a)
		bf, _ := toFloat64(b)
		return &SchemeFloat{Value: af + bf}
	}
	// Both exact: compute as rationals
	an, ad := toRat(a)
	bn, bd := toRat(b)
	return makeRational(an*bd+bn*ad, ad*bd)
}

// numSub subtracts b from a.
func numSub(a, b SchemeValue) SchemeValue {
	if _, ok := a.(*SchemeFloat); ok {
		af, _ := toFloat64(a)
		bf, _ := toFloat64(b)
		return &SchemeFloat{Value: af - bf}
	}
	if _, ok := b.(*SchemeFloat); ok {
		af, _ := toFloat64(a)
		bf, _ := toFloat64(b)
		return &SchemeFloat{Value: af - bf}
	}
	an, ad := toRat(a)
	bn, bd := toRat(b)
	return makeRational(an*bd-bn*ad, ad*bd)
}

// numMul multiplies two numbers.
func numMul(a, b SchemeValue) SchemeValue {
	if _, ok := a.(*SchemeFloat); ok {
		af, _ := toFloat64(a)
		bf, _ := toFloat64(b)
		return &SchemeFloat{Value: af * bf}
	}
	if _, ok := b.(*SchemeFloat); ok {
		af, _ := toFloat64(a)
		bf, _ := toFloat64(b)
		return &SchemeFloat{Value: af * bf}
	}
	an, ad := toRat(a)
	bn, bd := toRat(b)
	return makeRational(an*bn, ad*bd)
}

// numDiv divides a by b.
func numDiv(a, b SchemeValue) SchemeValue {
	if _, ok := a.(*SchemeFloat); ok {
		af, _ := toFloat64(a)
		bf, _ := toFloat64(b)
		return &SchemeFloat{Value: af / bf}
	}
	if _, ok := b.(*SchemeFloat); ok {
		af, _ := toFloat64(a)
		bf, _ := toFloat64(b)
		return &SchemeFloat{Value: af / bf}
	}
	an, ad := toRat(a)
	bn, bd := toRat(b)
	return makeRational(an*bd, ad*bn)
}

// numNeg negates a number.
func numNeg(a SchemeValue) SchemeValue {
	switch n := a.(type) {
	case *SchemeInt:
		return &SchemeInt{Value: -n.Value}
	case *SchemeRational:
		return &SchemeRational{Num: -n.Num, Den: n.Den}
	case *SchemeFloat:
		return &SchemeFloat{Value: -n.Value}
	}
	return a
}

// numCompare compares two numbers. Returns -1, 0, or 1.
func numCompare(a, b SchemeValue) int {
	// If either is inexact, compare as floats
	_, aFloat := a.(*SchemeFloat)
	_, bFloat := b.(*SchemeFloat)
	if aFloat || bFloat {
		af, _ := toFloat64(a)
		bf, _ := toFloat64(b)
		if af < bf {
			return -1
		}
		if af > bf {
			return 1
		}
		return 0
	}
	// Both exact: compare as rationals (cross-multiply)
	an, ad := toRat(a)
	bn, bd := toRat(b)
	lhs := an * bd
	rhs := bn * ad
	if lhs < rhs {
		return -1
	}
	if lhs > rhs {
		return 1
	}
	return 0
}

// numEqual checks numeric equality across types.
func numEqual(a, b SchemeValue) bool {
	return numCompare(a, b) == 0
}

// toRat returns numerator and denominator for an exact number.
func toRat(v SchemeValue) (int64, int64) {
	switch n := v.(type) {
	case *SchemeInt:
		return n.Value, 1
	case *SchemeRational:
		return n.Num, n.Den
	}
	// For float, convert (shouldn't normally be called for floats in exact context)
	f, _ := toFloat64(v)
	r := new(big.Rat).SetFloat64(f)
	num := r.Num().Int64()
	den := r.Denom().Int64()
	return num, den
}

// numAbs returns absolute value.
func numAbs(v SchemeValue) SchemeValue {
	switch n := v.(type) {
	case *SchemeInt:
		if n.Value < 0 {
			return &SchemeInt{Value: -n.Value}
		}
		return n
	case *SchemeRational:
		if n.Num < 0 {
			return &SchemeRational{Num: -n.Num, Den: n.Den}
		}
		return n
	case *SchemeFloat:
		return &SchemeFloat{Value: math.Abs(n.Value)}
	}
	return v
}

// numIsZero checks if a number is zero.
func numIsZero(v SchemeValue) bool {
	switch n := v.(type) {
	case *SchemeInt:
		return n.Value == 0
	case *SchemeRational:
		return n.Num == 0
	case *SchemeFloat:
		return n.Value == 0
	}
	return false
}

// numIsPositive checks if a number is positive.
func numIsPositive(v SchemeValue) bool {
	switch n := v.(type) {
	case *SchemeInt:
		return n.Value > 0
	case *SchemeRational:
		return n.Num > 0
	case *SchemeFloat:
		return n.Value > 0
	}
	return false
}

// numIsNegative checks if a number is negative.
func numIsNegative(v SchemeValue) bool {
	switch n := v.(type) {
	case *SchemeInt:
		return n.Value < 0
	case *SchemeRational:
		return n.Num < 0
	case *SchemeFloat:
		return n.Value < 0
	}
	return false
}

// exactToInexact converts an exact number to inexact.
func exactToInexact(v SchemeValue) SchemeValue {
	f, _ := toFloat64(v)
	return &SchemeFloat{Value: f}
}

// inexactToExact converts an inexact number to exact.
func inexactToExact(v SchemeValue) SchemeValue {
	switch n := v.(type) {
	case *SchemeInt, *SchemeRational:
		return v // already exact
	case *SchemeFloat:
		// Use math/big to get exact rational representation
		r := new(big.Rat).SetFloat64(n.Value)
		num := r.Num().Int64()
		den := r.Denom().Int64()
		return makeRational(num, den)
	}
	return v
}
