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
type StringVal struct {
	Val       string
	Immutable bool
}
type SymbolVal struct{ Val string }
type PairVal struct{ Car, Cdr Value }
type NilVal struct{}
type VoidVal struct{}
type CharVal struct{ Val rune }
type VectorVal struct{ Elems []Value }

// RecordTypeTag is a unique identifier for a record type.
type RecordTypeTag struct{ Name string }

// RecordVal is an instance of a define-record-type.
type RecordVal struct {
	Type   *RecordTypeTag
	Fields map[string]Value
}

func (v *RecordVal) String() string {
	return fmt.Sprintf("#<record %s>", v.Type.Name)
}

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

// CaseLambdaVal is a procedure with multiple arity clauses.
type CaseLambdaVal struct {
	Clauses []*LambdaVal
}

func (v *CaseLambdaVal) String() string {
	return "#<procedure>"
}

// ValuesVal represents multiple return values from (values ...).
type ValuesVal struct{ Vals []Value }

func (v *ValuesVal) String() string {
	if len(v.Vals) == 0 {
		return ""
	}
	return v.Vals[0].String()
}

// CallCCVal is the call/cc primitive, stored as a first-class value.
type CallCCVal struct{}

func (v *CallCCVal) String() string {
	return "#<procedure call-with-current-continuation>"
}

// windEntry represents one dynamic-wind frame.
type windEntry struct {
	In  Value // in-thunk
	Out Value // out-thunk
}

// ContinuationVal is a captured continuation from call/cc.
type ContinuationVal struct {
	topExprs  []Expr // top-level expressions from the capturing expression onward
	topEnv    *Env   // top-level environment (shared, mutable)
	bodyExprs []Expr // if non-nil, restart from these body expressions instead
	bodyEnv   *Env   // environment for bodyExprs
	winds     []windEntry // dynamic-wind stack at capture time
}

func (v *ContinuationVal) String() string {
	return "#<continuation>"
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

func (v *VectorVal) String() string {
	var buf strings.Builder
	buf.WriteString("#(")
	for i, e := range v.Elems {
		if i > 0 {
			buf.WriteByte(' ')
		}
		buf.WriteString(e.String())
	}
	buf.WriteByte(')')
	return buf.String()
}

func (v *PairVal) String() string {
	return writePair(v, true)
}

func writePair(v *PairVal, useWrite bool) string {
	visited := make(map[*PairVal]bool)
	var buf strings.Builder
	buf.WriteByte('(')
	cur := Value(v)
	first := true
	for {
		p, ok := cur.(*PairVal)
		if !ok {
			break
		}
		if visited[p] {
			if !first {
				buf.WriteString(" ...")
			}
			break
		}
		visited[p] = true
		if !first {
			buf.WriteByte(' ')
		}
		if useWrite {
			buf.WriteString(writeValueSafe(p.Car, visited))
		} else {
			buf.WriteString(displayValueSafe(p.Car, visited))
		}
		first = false
		cur = p.Cdr
	}
	if _, ok := cur.(*NilVal); !ok {
		if p, ok := cur.(*PairVal); ok && visited[p] {
			// already handled cycle above
		} else {
			buf.WriteString(" . ")
			if useWrite {
				buf.WriteString(writeValueSafe(cur, visited))
			} else {
				buf.WriteString(displayValueSafe(cur, visited))
			}
		}
	}
	buf.WriteByte(')')
	return buf.String()
}

func writeValueSafe(v Value, visited map[*PairVal]bool) string {
	if p, ok := v.(*PairVal); ok {
		if visited[p] {
			return "(...)"
		}
		visited[p] = true
		var buf strings.Builder
		buf.WriteByte('(')
		cur := Value(p)
		first := true
		for {
			pp, ok := cur.(*PairVal)
			if !ok {
				break
			}
			if !first && visited[pp] {
				buf.WriteString(" ...")
				cur = pp // mark as handled
				break
			}
			visited[pp] = true
			if !first {
				buf.WriteByte(' ')
			}
			buf.WriteString(writeValueSafe(pp.Car, visited))
			first = false
			cur = pp.Cdr
		}
		if _, ok := cur.(*NilVal); !ok {
			if pp, ok := cur.(*PairVal); ok && visited[pp] {
				// cycle handled
			} else {
				buf.WriteString(" . ")
				buf.WriteString(writeValueSafe(cur, visited))
			}
		}
		buf.WriteByte(')')
		return buf.String()
	}
	return v.String()
}

func displayValueSafe(v Value, visited map[*PairVal]bool) string {
	switch val := v.(type) {
	case *StringVal:
		return val.Val
	case *CharVal:
		return string(val.Val)
	case *PairVal:
		if visited[val] {
			return "(...)"
		}
		visited[val] = true
		var buf strings.Builder
		buf.WriteByte('(')
		cur := Value(val)
		first := true
		for {
			pp, ok := cur.(*PairVal)
			if !ok {
				break
			}
			if !first && visited[pp] {
				buf.WriteString(" ...")
				cur = pp
				break
			}
			visited[pp] = true
			if !first {
				buf.WriteByte(' ')
			}
			buf.WriteString(displayValueSafe(pp.Car, visited))
			first = false
			cur = pp.Cdr
		}
		if _, ok := cur.(*NilVal); !ok {
			if pp, ok := cur.(*PairVal); ok && visited[pp] {
				// cycle
			} else {
				buf.WriteString(" . ")
				buf.WriteString(displayValueSafe(cur, visited))
			}
		}
		buf.WriteByte(')')
		return buf.String()
	case *VectorVal:
		var buf strings.Builder
		buf.WriteString("#(")
		for i, e := range val.Elems {
			if i > 0 {
				buf.WriteByte(' ')
			}
			buf.WriteString(displayValueSafe(e, visited))
		}
		buf.WriteByte(')')
		return buf.String()
	default:
		return v.String()
	}
}

// displayValue returns the display representation (no quotes on strings).
func displayValue(v Value) string {
	visited := make(map[*PairVal]bool)
	return displayValueSafe(v, visited)
}

// writeValue returns the write representation (quotes on strings).
func writeValue(v Value) string {
	if p, ok := v.(*PairVal); ok {
		return writePair(p, true)
	}
	return v.String()
}

func isTruthy(v Value) bool {
	if b, ok := v.(*BoolVal); ok {
		return b.Val
	}
	return true
}
