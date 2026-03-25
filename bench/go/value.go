package ming

import (
	"fmt"
	"strings"
)

type ValueType int

const (
	TypeInteger ValueType = iota
	TypeBoolean
	TypeString
	TypeSymbol
	TypePair
	TypeNull
	TypeVoid
	TypeLambda
)

type Value struct {
	Type    ValueType
	IntVal  int64
	BoolVal bool
	StrVal  string
	Car     *Value
	Cdr     *Value
	// Lambda fields
	Params []string
	Body   []*Expr
	ClosureEnv *Env
}

var Void = &Value{Type: TypeVoid}
var Null = &Value{Type: TypeNull}
var True = &Value{Type: TypeBoolean, BoolVal: true}
var False = &Value{Type: TypeBoolean, BoolVal: false}

func IntValue(n int64) *Value {
	return &Value{Type: TypeInteger, IntVal: n}
}

func BoolValue(b bool) *Value {
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

func (v *Value) Display() string {
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
		return displayList(v)
	case TypeLambda:
		return "#<procedure>"
	}
	return ""
}

func displayList(v *Value) string {
	var sb strings.Builder
	sb.WriteByte('(')
	cur := v
	first := true
	for cur.Type == TypePair {
		if !first {
			sb.WriteByte(' ')
		}
		first = false
		sb.WriteString(cur.Car.Display())
		cur = cur.Cdr
	}
	if cur.Type != TypeNull {
		sb.WriteString(" . ")
		sb.WriteString(cur.Display())
	}
	sb.WriteByte(')')
	return sb.String()
}

func isTruthy(v *Value) bool {
	return !(v.Type == TypeBoolean && !v.BoolVal)
}
