package ming

import (
	"fmt"
	"math"
)

// gcd computes the greatest common divisor of two non-negative integers.
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

// makeRational creates a simplified rational or integer value.
func makeRational(num, den int64) Value {
	if den == 0 {
		// Should not happen in normal flow
		return &IntVal{Val: 0}
	}
	if den < 0 {
		num = -num
		den = -den
	}
	g := gcd(num, den)
	if g < 0 {
		g = -g
	}
	num /= g
	den /= g
	if den == 1 {
		return &IntVal{Val: num}
	}
	return &RationalVal{Num: num, Den: den}
}

// toFloat64 converts any numeric value to float64.
func toFloat64(v Value) (float64, bool) {
	switch n := v.(type) {
	case *IntVal:
		return float64(n.Val), true
	case *FloatVal:
		return n.Val, true
	case *RationalVal:
		return float64(n.Num) / float64(n.Den), true
	}
	return 0, false
}

// isExact returns true if the value is an exact number.
func isExact(v Value) bool {
	switch v.(type) {
	case *IntVal, *RationalVal:
		return true
	}
	return false
}

// isNumber returns true if the value is any numeric type.
func isNumber(v Value) bool {
	switch v.(type) {
	case *IntVal, *FloatVal, *RationalVal:
		return true
	}
	return false
}

// toRational converts a numeric value to (num, den) representation.
// For inexact, returns false.
func toRational(v Value) (int64, int64, bool) {
	switch n := v.(type) {
	case *IntVal:
		return n.Val, 1, true
	case *RationalVal:
		return n.Num, n.Den, true
	}
	return 0, 0, false
}

// addExact adds two exact numbers, returning an exact result.
func addExact(aN, aD, bN, bD int64) Value {
	return makeRational(aN*bD+bN*aD, aD*bD)
}

func subExact(aN, aD, bN, bD int64) Value {
	return makeRational(aN*bD-bN*aD, aD*bD)
}

func mulExact(aN, aD, bN, bD int64) Value {
	return makeRational(aN*bN, aD*bD)
}

func divExact(aN, aD, bN, bD int64) Value {
	return makeRational(aN*bD, aD*bN)
}

// numericAdd handles mixed numeric addition.
func numericAdd(args []Value) (Value, error) {
	// Check if all are exact
	allExact := true
	for _, a := range args {
		if !isNumber(a) {
			return nil, &EvalError{Message: fmt.Sprintf("+: not a number: %s", a.String())}
		}
		if !isExact(a) {
			allExact = false
		}
	}
	if allExact {
		rn, rd := int64(0), int64(1)
		for _, a := range args {
			an, ad, _ := toRational(a)
			rn = rn*ad + an*rd
			rd = rd * ad
			g := gcd(rn, rd)
			if g < 0 {
				g = -g
			}
			rn /= g
			rd /= g
		}
		return makeRational(rn, rd), nil
	}
	var sum float64
	for _, a := range args {
		f, _ := toFloat64(a)
		sum += f
	}
	return &FloatVal{Val: sum}, nil
}

func numericSub(args []Value) (Value, error) {
	if len(args) == 0 {
		return nil, &EvalError{Message: "-: requires at least 1 argument"}
	}
	for _, a := range args {
		if !isNumber(a) {
			return nil, &EvalError{Message: fmt.Sprintf("-: not a number: %s", a.String())}
		}
	}
	if len(args) == 1 {
		switch n := args[0].(type) {
		case *IntVal:
			return &IntVal{Val: -n.Val}, nil
		case *RationalVal:
			return &RationalVal{Num: -n.Num, Den: n.Den}, nil
		case *FloatVal:
			return &FloatVal{Val: -n.Val}, nil
		}
	}
	allExact := true
	for _, a := range args {
		if !isExact(a) {
			allExact = false
		}
	}
	if allExact {
		rn, rd, _ := toRational(args[0])
		for _, a := range args[1:] {
			an, ad, _ := toRational(a)
			rn = rn*ad - an*rd
			rd = rd * ad
			g := gcd(rn, rd)
			if g < 0 {
				g = -g
			}
			rn /= g
			rd /= g
		}
		return makeRational(rn, rd), nil
	}
	f0, _ := toFloat64(args[0])
	for _, a := range args[1:] {
		f, _ := toFloat64(a)
		f0 -= f
	}
	return &FloatVal{Val: f0}, nil
}

func numericMul(args []Value) (Value, error) {
	allExact := true
	for _, a := range args {
		if !isNumber(a) {
			return nil, &EvalError{Message: fmt.Sprintf("*: not a number: %s", a.String())}
		}
		if !isExact(a) {
			allExact = false
		}
	}
	if allExact {
		rn, rd := int64(1), int64(1)
		for _, a := range args {
			an, ad, _ := toRational(a)
			rn *= an
			rd *= ad
			g := gcd(rn, rd)
			if g < 0 {
				g = -g
			}
			rn /= g
			rd /= g
		}
		return makeRational(rn, rd), nil
	}
	result := 1.0
	for _, a := range args {
		f, _ := toFloat64(a)
		result *= f
	}
	return &FloatVal{Val: result}, nil
}

func numericDiv(args []Value) (Value, error) {
	if len(args) < 2 {
		return nil, &EvalError{Message: "/: requires at least 2 arguments"}
	}
	for _, a := range args {
		if !isNumber(a) {
			return nil, &EvalError{Message: fmt.Sprintf("/: not a number: %s", a.String())}
		}
	}
	allExact := true
	for _, a := range args {
		if !isExact(a) {
			allExact = false
		}
	}
	if allExact {
		rn, rd, _ := toRational(args[0])
		for _, a := range args[1:] {
			an, ad, _ := toRational(a)
			if an == 0 {
				return nil, &EvalError{Message: "/: division by zero"}
			}
			rn = rn * ad
			rd = rd * an
			g := gcd(rn, rd)
			if g < 0 {
				g = -g
			}
			rn /= g
			rd /= g
		}
		return makeRational(rn, rd), nil
	}
	f0, _ := toFloat64(args[0])
	for _, a := range args[1:] {
		f, _ := toFloat64(a)
		if f == 0 {
			return nil, &EvalError{Message: "/: division by zero"}
		}
		f0 /= f
	}
	return &FloatVal{Val: f0}, nil
}

// numericCompareGeneric handles mixed-type numeric comparison.
func numericCompareGeneric(name string, args []Value, cmpF func(a, b float64) bool) (Value, error) {
	if len(args) < 2 {
		return nil, &EvalError{Message: fmt.Sprintf("%s: requires at least 2 arguments", name)}
	}
	prevF, ok := toFloat64(args[0])
	if !ok {
		return nil, &EvalError{Message: fmt.Sprintf("%s: not a number: %s", name, args[0].String())}
	}
	for _, a := range args[1:] {
		f, ok := toFloat64(a)
		if !ok {
			return nil, &EvalError{Message: fmt.Sprintf("%s: not a number: %s", name, a.String())}
		}
		if !cmpF(prevF, f) {
			return &BoolVal{Val: false}, nil
		}
		prevF = f
	}
	return &BoolVal{Val: true}, nil
}

func builtinExactQ(args []Value) (Value, error) {
	if len(args) != 1 {
		return nil, &EvalError{Message: "exact?: requires exactly 1 argument"}
	}
	return &BoolVal{Val: isExact(args[0])}, nil
}

func builtinInexactQ(args []Value) (Value, error) {
	if len(args) != 1 {
		return nil, &EvalError{Message: "inexact?: requires exactly 1 argument"}
	}
	_, ok := args[0].(*FloatVal)
	return &BoolVal{Val: ok}, nil
}

func builtinExactToInexact(args []Value) (Value, error) {
	if len(args) != 1 {
		return nil, &EvalError{Message: "exact->inexact: requires exactly 1 argument"}
	}
	f, ok := toFloat64(args[0])
	if !ok {
		return nil, &EvalError{Message: "exact->inexact: not a number"}
	}
	return &FloatVal{Val: f}, nil
}

func builtinInexactToExact(args []Value) (Value, error) {
	if len(args) != 1 {
		return nil, &EvalError{Message: "inexact->exact: requires exactly 1 argument"}
	}
	switch n := args[0].(type) {
	case *IntVal:
		return n, nil
	case *RationalVal:
		return n, nil
	case *FloatVal:
		// Convert float to exact rational using rationalization
		// For simple cases like 0.5 -> 1/2
		if n.Val == math.Trunc(n.Val) {
			return &IntVal{Val: int64(n.Val)}, nil
		}
		// Use continued fraction approximation
		num, den := floatToRational(n.Val)
		return makeRational(num, den), nil
	}
	return nil, &EvalError{Message: "inexact->exact: not a number"}
}

// floatToRational converts a float64 to a rational approximation.
func floatToRational(f float64) (int64, int64) {
	if f == 0 {
		return 0, 1
	}
	neg := false
	if f < 0 {
		neg = true
		f = -f
	}
	// Use the standard approach: multiply by power of 2 to get integer ratio
	// For common fractions like 0.5, 0.25, 0.333... this works well
	// Try continued fraction expansion
	const maxIter = 64
	const epsilon = 1e-12
	h0, h1 := int64(0), int64(1)
	k0, k1 := int64(1), int64(0)
	x := f
	for i := 0; i < maxIter; i++ {
		a := int64(math.Floor(x))
		h0, h1 = h1, a*h1+h0
		k0, k1 = k1, a*k1+k0
		rem := x - float64(a)
		if rem < epsilon {
			break
		}
		x = 1.0 / rem
		if x > 1e15 {
			break
		}
	}
	num := h1
	den := k1
	if neg {
		num = -num
	}
	return num, den
}

func builtinNumerator(args []Value) (Value, error) {
	if len(args) != 1 {
		return nil, &EvalError{Message: "numerator: requires exactly 1 argument"}
	}
	switch n := args[0].(type) {
	case *IntVal:
		return n, nil
	case *RationalVal:
		return &IntVal{Val: n.Num}, nil
	}
	return nil, &EvalError{Message: "numerator: not an exact number"}
}

func builtinDenominator(args []Value) (Value, error) {
	if len(args) != 1 {
		return nil, &EvalError{Message: "denominator: requires exactly 1 argument"}
	}
	switch args[0].(type) {
	case *IntVal:
		return &IntVal{Val: 1}, nil
	case *RationalVal:
		return &IntVal{Val: args[0].(*RationalVal).Den}, nil
	}
	return nil, &EvalError{Message: "denominator: not an exact number"}
}

func builtinIntegerQ(args []Value) (Value, error) {
	if len(args) != 1 {
		return nil, &EvalError{Message: "integer?: requires exactly 1 argument"}
	}
	switch n := args[0].(type) {
	case *IntVal:
		return &BoolVal{Val: true}, nil
	case *RationalVal:
		// A rational is an integer if den == 1 (but we simplify, so this means num/den was already an int)
		return &BoolVal{Val: n.Den == 1}, nil
	case *FloatVal:
		return &BoolVal{Val: n.Val == math.Trunc(n.Val)}, nil
	}
	return &BoolVal{Val: false}, nil
}

func builtinRationalQ(args []Value) (Value, error) {
	if len(args) != 1 {
		return nil, &EvalError{Message: "rational?: requires exactly 1 argument"}
	}
	switch args[0].(type) {
	case *IntVal, *RationalVal:
		return &BoolVal{Val: true}, nil
	}
	return &BoolVal{Val: false}, nil
}

func registerL11Builtins(env *Env) {
	env.set("exact?", &BuiltinFunc{Name: "exact?", Fn: builtinExactQ})
	env.set("inexact?", &BuiltinFunc{Name: "inexact?", Fn: builtinInexactQ})
	env.set("exact->inexact", &BuiltinFunc{Name: "exact->inexact", Fn: builtinExactToInexact})
	env.set("inexact->exact", &BuiltinFunc{Name: "inexact->exact", Fn: builtinInexactToExact})
	env.set("numerator", &BuiltinFunc{Name: "numerator", Fn: builtinNumerator})
	env.set("denominator", &BuiltinFunc{Name: "denominator", Fn: builtinDenominator})
	env.set("integer?", &BuiltinFunc{Name: "integer?", Fn: builtinIntegerQ})
	env.set("rational?", &BuiltinFunc{Name: "rational?", Fn: builtinRationalQ})
}
