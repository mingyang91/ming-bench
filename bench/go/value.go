package ming

import (
	"fmt"
	"strings"
)

// ValueType represents the type of a Scheme value.
type ValueType int

const (
	TypeInteger ValueType = iota
	TypeBoolean
	TypeString
	TypeSymbol
	TypePair
	TypeNull
	TypeVoid
)

// Value represents a Scheme value.
type Value struct {
	Type    ValueType
	IntVal  int64
	BoolVal bool
	StrVal  string
	Car     *Value
	Cdr     *Value
}

var Void = &Value{Type: TypeVoid}
var Null = &Value{Type: TypeNull}
var True = &Value{Type: TypeBoolean, BoolVal: true}
var False = &Value{Type: TypeBoolean, BoolVal: false}

func IntegerValue(n int64) *Value {
	return &Value{Type: TypeInteger, IntVal: n}
}

func BooleanValue(b bool) *Value {
	if b {
		return True
	}
	return False
}

func StringValue(s string) *Value {
	return &Value{Type: TypeString, StrVal: s}
}

func SymbolValue(s string) *Value {
	return &Value{Type: TypeSymbol, StrVal: s}
}

func PairValue(car, cdr *Value) *Value {
	return &Value{Type: TypePair, Car: car, Cdr: cdr}
}

// IsTruthy returns true for all values except #f.
func (v *Value) IsTruthy() bool {
	return !(v.Type == TypeBoolean && !v.BoolVal)
}

// String returns the external representation of the value.
func (v *Value) String() string {
	switch v.Type {
	case TypeInteger:
		return fmt.Sprintf("%d", v.IntVal)
	case TypeBoolean:
		if v.BoolVal {
			return "#t"
		}
		return "#f"
	case TypeString:
		return fmt.Sprintf("%q", v.StrVal)
	case TypeSymbol:
		return v.StrVal
	case TypeNull:
		return "()"
	case TypeVoid:
		return ""
	case TypePair:
		var buf strings.Builder
		buf.WriteByte('(')
		buf.WriteString(v.Car.String())
		cur := v.Cdr
		for cur.Type == TypePair {
			buf.WriteByte(' ')
			buf.WriteString(cur.Car.String())
			cur = cur.Cdr
		}
		if cur.Type != TypeNull {
			buf.WriteString(" . ")
			buf.WriteString(cur.String())
		}
		buf.WriteByte(')')
		return buf.String()
	default:
		return "<unknown>"
	}
}
