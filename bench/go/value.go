package ming

import "fmt"

// ValueType represents the type of a Scheme value.
type ValueType int

const (
	TypeInt ValueType = iota
	TypeBool
	TypeString
	TypeSymbol
	TypePair
	TypeNil // empty list
	TypeVoid
	TypeLambda
	TypeChar
)

// Value represents a Scheme value.
type Value struct {
	Type    ValueType
	IntVal  int64
	BoolVal bool
	StrVal  string
	Runes   []rune // mutable string storage (used by string-copy/string-set!)
	Car     *Value
	Cdr     *Value
	// Lambda fields
	Params []string
	Body   []*Expr
	ClosureEnv *Env
}

var Void = &Value{Type: TypeVoid}
var Nil = &Value{Type: TypeNil}

func IntValue(n int64) *Value    { return &Value{Type: TypeInt, IntVal: n} }
func BoolValue(b bool) *Value    { return &Value{Type: TypeBool, BoolVal: b} }
func StringValue(s string) *Value { return &Value{Type: TypeString, StrVal: s} }
func MutableStringValue(s string) *Value { return &Value{Type: TypeString, Runes: []rune(s)} }

// StrContent returns the effective string content, preferring Runes if set.
func (v *Value) StrContent() string {
	if v.Runes != nil {
		return string(v.Runes)
	}
	return v.StrVal
}
func SymbolValue(s string) *Value { return &Value{Type: TypeSymbol, StrVal: s} }
func CharValue(r rune) *Value    { return &Value{Type: TypeChar, IntVal: int64(r)} }

func (v *Value) String() string {
	switch v.Type {
	case TypeInt:
		return fmt.Sprintf("%d", v.IntVal)
	case TypeBool:
		if v.BoolVal {
			return "#t"
		}
		return "#f"
	case TypeString:
		return fmt.Sprintf("%q", v.StrContent())
	case TypeSymbol:
		return v.StrVal
	case TypeNil:
		return "()"
	case TypeVoid:
		return ""
	case TypeLambda:
		return "#<procedure>"
	case TypeChar:
		return fmt.Sprintf("#\\%c", rune(v.IntVal))
	case TypePair:
		return "(" + pairInner(v) + ")"
	default:
		return "<unknown>"
	}
}

func pairInner(v *Value) string {
	s := v.Car.String()
	switch v.Cdr.Type {
	case TypeNil:
		return s
	case TypePair:
		return s + " " + pairInner(v.Cdr)
	default:
		return s + " . " + v.Cdr.String()
	}
}

// DisplayString returns the display representation (no quotes on strings).
func (v *Value) DisplayString() string {
	switch v.Type {
	case TypeString:
		return v.StrContent()
	case TypeChar:
		return string(rune(v.IntVal))
	case TypePair:
		return "(" + pairInnerDisplay(v) + ")"
	default:
		return v.String()
	}
}

func pairInnerDisplay(v *Value) string {
	s := v.Car.DisplayString()
	switch v.Cdr.Type {
	case TypeNil:
		return s
	case TypePair:
		return s + " " + pairInnerDisplay(v.Cdr)
	default:
		return s + " . " + v.Cdr.DisplayString()
	}
}

// IsTruthy returns true for all values except #f.
func (v *Value) IsTruthy() bool {
	return !(v.Type == TypeBool && !v.BoolVal)
}
