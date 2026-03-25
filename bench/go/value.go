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
	TypeChar
)

type Value struct {
	Type    ValueType
	IntVal  int64
	BoolVal bool
	StrVal  string
	Car     *Value
	Cdr     *Value
	CharVal rune
	// Lambda fields
	Params    []string
	RestParam string // variadic rest parameter (empty if none)
	Body      []*Expr
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

func CharValue(c rune) *Value {
	return &Value{Type: TypeChar, CharVal: c}
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
	case TypeChar:
		return fmt.Sprintf("#\\%c", v.CharVal)
	}
	return ""
}

// WriteRepr returns the write representation (strings quoted, chars with #\).
func (v *Value) WriteRepr() string {
	switch v.Type {
	case TypeString:
		return fmt.Sprintf("%q", v.StrVal)
	case TypePair:
		return writeList(v)
	default:
		return v.Display()
	}
}

// DisplayStr returns the display representation (strings unquoted).
func (v *Value) DisplayStr() string {
	switch v.Type {
	case TypeString:
		return v.StrVal
	case TypePair:
		return displayListUnquoted(v)
	default:
		return v.Display()
	}
}

func displayListUnquoted(v *Value) string {
	var sb strings.Builder
	sb.WriteByte('(')
	cur := v
	first := true
	for cur.Type == TypePair {
		if !first {
			sb.WriteByte(' ')
		}
		first = false
		sb.WriteString(cur.Car.DisplayStr())
		cur = cur.Cdr
	}
	if cur.Type != TypeNull {
		sb.WriteString(" . ")
		sb.WriteString(cur.DisplayStr())
	}
	sb.WriteByte(')')
	return sb.String()
}

func writeList(v *Value) string {
	var sb strings.Builder
	sb.WriteByte('(')
	cur := v
	first := true
	for cur.Type == TypePair {
		if !first {
			sb.WriteByte(' ')
		}
		first = false
		sb.WriteString(cur.Car.WriteRepr())
		cur = cur.Cdr
	}
	if cur.Type != TypeNull {
		sb.WriteString(" . ")
		sb.WriteString(cur.WriteRepr())
	}
	sb.WriteByte(')')
	return sb.String()
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
