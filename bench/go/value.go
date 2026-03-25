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
type BoolVal struct{ Val bool }
type StringVal struct{ Val string }
type PairVal struct{ Car, Cdr Value }
type NilVal struct{}
type SymbolVal struct{ Name string }
type VoidVal struct{}

func (v *IntVal) String() string {
	return fmt.Sprintf("%d", v.Val)
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

func (v *VoidVal) String() string {
	return ""
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

func isTruthy(v Value) bool {
	if b, ok := v.(*BoolVal); ok {
		return b.Val
	}
	return true
}
