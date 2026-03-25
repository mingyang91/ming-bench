package ming

import (
	"fmt"
	"strconv"
	"strings"
)

// Value represents a Scheme value.
type Value interface {
	String() string
}

type IntVal struct{ Val int64 }
type FloatVal struct{ Val float64 }
type RatVal struct{ Num, Den int64 } // always simplified, Den > 0
type BoolVal struct{ Val bool }
type StringVal struct{ Val string }
type SymbolVal struct{ Val string }
type PairVal struct{ Car, Cdr Value }
type NilVal struct{}
type VoidVal struct{}
type CharVal struct{ Val rune }

// LambdaVal is a user-defined closure.
type LambdaVal struct {
	Params    []string
	RestParam string // if non-empty, collects remaining args into a list
	Body      []Expr
	Env       *Env
}

func (v *LambdaVal) String() string {
	return "#<procedure>"
}

func (v *IntVal) String() string {
	return fmt.Sprintf("%d", v.Val)
}

func (v *FloatVal) String() string {
	s := strconv.FormatFloat(v.Val, 'f', -1, 64)
	// Ensure there's a decimal point
	if !strings.ContainsRune(s, '.') {
		s += ".0"
	}
	return s
}

func (v *RatVal) String() string {
	return fmt.Sprintf("%d/%d", v.Num, v.Den)
}

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

// makeRat creates a simplified rational. Returns IntVal if denominator is 1.
func makeRat(num, den int64) Value {
	if den == 0 {
		panic("rational with zero denominator")
	}
	if den < 0 {
		num, den = -num, -den
	}
	g := gcd(num, den)
	num /= g
	den /= g
	if den == 1 {
		return &IntVal{Val: num}
	}
	return &RatVal{Num: num, Den: den}
}

// isNumber checks if a value is any numeric type.
func isNumber(v Value) bool {
	switch v.(type) {
	case *IntVal, *FloatVal, *RatVal:
		return true
	}
	return false
}

// toFloat64 converts any number to float64.
func toFloat64(v Value) (float64, bool) {
	switch n := v.(type) {
	case *IntVal:
		return float64(n.Val), true
	case *FloatVal:
		return n.Val, true
	case *RatVal:
		return float64(n.Num) / float64(n.Den), true
	}
	return 0, false
}

// toRational converts any exact number to (num, den). Returns false for inexact.
func toRational(v Value) (int64, int64, bool) {
	switch n := v.(type) {
	case *IntVal:
		return n.Val, 1, true
	case *RatVal:
		return n.Num, n.Den, true
	}
	return 0, 0, false
}

// isExact returns true if the value is an exact number.
func isExact(v Value) bool {
	switch v.(type) {
	case *IntVal, *RatVal:
		return true
	}
	return false
}

func (v *BoolVal) String() string {
	if v.Val {
		return "#t"
	}
	return "#f"
}

func (v *StringVal) String() string {
	return fmt.Sprintf("%q", v.Val)
}

func (v *SymbolVal) String() string {
	return v.Val
}

func (v *NilVal) String() string {
	return "()"
}

func (v *VoidVal) String() string {
	return ""
}

func (v *CharVal) String() string {
	switch v.Val {
	case ' ':
		return "#\\space"
	case '\n':
		return "#\\newline"
	case '\t':
		return "#\\tab"
	default:
		return fmt.Sprintf("#\\%c", v.Val)
	}
}

func (v *PairVal) String() string {
	var buf strings.Builder
	buf.WriteByte('(')
	cur := Value(v)
	first := true
	for {
		p, ok := cur.(*PairVal)
		if !ok {
			break
		}
		if !first {
			buf.WriteByte(' ')
		}
		buf.WriteString(p.Car.String())
		first = false
		cur = p.Cdr
	}
	if _, ok := cur.(*NilVal); !ok {
		buf.WriteString(" . ")
		buf.WriteString(cur.String())
	}
	buf.WriteByte(')')
	return buf.String()
}

// displayValue returns the display representation (no quotes on strings).
func displayValue(v Value) string {
	switch val := v.(type) {
	case *StringVal:
		return val.Val
	case *CharVal:
		return string(val.Val)
	case *PairVal:
		var buf strings.Builder
		buf.WriteByte('(')
		cur := Value(val)
		first := true
		for {
			p, ok := cur.(*PairVal)
			if !ok {
				break
			}
			if !first {
				buf.WriteByte(' ')
			}
			buf.WriteString(displayValue(p.Car))
			first = false
			cur = p.Cdr
		}
		if _, ok := cur.(*NilVal); !ok {
			buf.WriteString(" . ")
			buf.WriteString(displayValue(cur))
		}
		buf.WriteByte(')')
		return buf.String()
	default:
		return v.String()
	}
}

// writeValue returns the write representation (quotes on strings).
func writeValue(v Value) string {
	return v.String()
}

func isTruthy(v Value) bool {
	if b, ok := v.(*BoolVal); ok {
		return b.Val
	}
	return true
}
