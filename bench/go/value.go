package ming

import (
	"fmt"
	"strings"
)

// Value represents a Scheme value.
type Value interface {
	String() string
}

type IntVal struct {
	Val int64
}

func (v *IntVal) String() string { return fmt.Sprintf("%d", v.Val) }

type BoolVal struct {
	Val bool
}

func (v *BoolVal) String() string {
	if v.Val {
		return "#t"
	}
	return "#f"
}

type StringVal struct {
	Val string
}

func (v *StringVal) String() string {
	return fmt.Sprintf("%q", v.Val)
}

type SymbolVal struct {
	Name string
}

func (v *SymbolVal) String() string { return v.Name }

type PairVal struct {
	Car Value
	Cdr Value
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

type NilVal struct{}

func (v *NilVal) String() string { return "()" }

type VoidVal struct{}

func (v *VoidVal) String() string { return "" }

// LambdaVal represents a user-defined closure.
type LambdaVal struct {
	Params []string
	Body   []*Expr
	Env    *Env
}

func (v *LambdaVal) String() string { return "#<procedure>" }

// CharVal represents a Scheme character.
type CharVal struct {
	Val rune
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
		return "#\\" + string(v.Val)
	}
}

// DisplayString returns the display representation of a value (no quotes on strings).
func DisplayString(v Value) string {
	switch val := v.(type) {
	case *StringVal:
		return val.Val
	case *CharVal:
		return string(val.Val)
	case *PairVal:
		return displayPair(val)
	default:
		return v.String()
	}
}

func displayPair(v *PairVal) string {
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
		first = false
		buf.WriteString(DisplayString(p.Car))
		cur = p.Cdr
	}
	if _, ok := cur.(*NilVal); !ok {
		buf.WriteString(" . ")
		buf.WriteString(DisplayString(cur))
	}
	buf.WriteByte(')')
	return buf.String()
}

// isTruthy returns true for all values except #f.
func isTruthy(v Value) bool {
	if b, ok := v.(*BoolVal); ok {
		return b.Val
	}
	return true
}
