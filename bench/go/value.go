package ming

import (
	"fmt"
	"strings"
)

// Value represents a Scheme value.
type Value interface {
	String() string
}

type IntVal struct{ Val int64 }
type FloatVal struct{ Val float64 }
type RationalVal struct{ Num, Den int64 } // always simplified, Den > 0
type BoolVal struct{ Val bool }
type StringVal struct {
	Val     string
	Mutable bool
}
type PairVal struct{ Car, Cdr Value }
type NilVal struct{}
type SymbolVal struct{ Name string }
type CharVal struct{ Val rune }
type VoidVal struct{}
type VectorVal struct{ Elems []Value }
type LambdaVal struct {
	Params   []string
	Rest     string // rest parameter name (dot notation), empty if none
	Body     []Expr
	Env      *Env
}
type CaseLambdaVal struct {
	Clauses []*LambdaVal
}

func (v *LambdaVal) String() string {
	return "#<procedure>"
}

func (v *CaseLambdaVal) String() string {
	return "#<procedure>"
}

func (v *IntVal) String() string {
	return fmt.Sprintf("%d", v.Val)
}

func (v *FloatVal) String() string {
	s := fmt.Sprintf("%.16g", v.Val)
	// Ensure there's a decimal point for non-special values
	hasDot := false
	for _, c := range s {
		if c == '.' || c == 'e' || c == 'E' || c == 'i' || c == 'n' {
			hasDot = true
			break
		}
	}
	if !hasDot {
		s += ".0"
	}
	return s
}

func (v *RationalVal) String() string {
	return fmt.Sprintf("%d/%d", v.Num, v.Den)
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

func (v *NilVal) String() string {
	return "()"
}

func (v *SymbolVal) String() string {
	return v.Name
}

func (v *CharVal) String() string {
	switch v.Val {
	case ' ':
		return `#\space`
	case '\n':
		return `#\newline`
	default:
		return `#\` + string(v.Val)
	}
}

func (v *VoidVal) String() string {
	return ""
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

// displayValue formats a value for display (no quotes on strings, raw chars).
func displayValue(v Value) string {
	switch val := v.(type) {
	case *StringVal:
		return val.Val
	case *CharVal:
		return string(val.Val)
	case *PairVal:
		var buf strings.Builder
		buf.WriteByte('(')
		visited := map[*PairVal]bool{val: true}
		cur := Value(val)
		first := true
		for {
			p, ok := cur.(*PairVal)
			if !ok {
				break
			}
			if !first {
				if visited[p] {
					buf.WriteString(" . ...")
					buf.WriteByte(')')
					return buf.String()
				}
				visited[p] = true
			}
			if !first {
				buf.WriteByte(' ')
			}
			first = false
			buf.WriteString(displayValue(p.Car))
			cur = p.Cdr
		}
		if _, ok := cur.(*NilVal); !ok {
			buf.WriteString(" . ")
			buf.WriteString(displayValue(cur))
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
			buf.WriteString(displayValue(e))
		}
		buf.WriteByte(')')
		return buf.String()
	default:
		return v.String()
	}
}

func (v *PairVal) String() string {
	var buf strings.Builder
	buf.WriteByte('(')
	visited := map[*PairVal]bool{v: true}
	cur := Value(v)
	first := true
	for {
		p, ok := cur.(*PairVal)
		if !ok {
			break
		}
		if !first {
			if visited[p] {
				buf.WriteString(" . ...")
				buf.WriteByte(')')
				return buf.String()
			}
			visited[p] = true
		}
		if !first {
			buf.WriteByte(' ')
		}
		first = false
		buf.WriteString(p.Car.String())
		cur = p.Cdr
	}
	if _, ok := cur.(*NilVal); !ok {
		buf.WriteString(" . ")
		buf.WriteString(cur.String())
	}
	buf.WriteByte(')')
	return buf.String()
}

func isTruthy(v Value) bool {
	if b, ok := v.(*BoolVal); ok {
		return b.Val
	}
	return true
}
