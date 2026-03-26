package ming

import (
	"fmt"
	"strconv"
	"strings"
)

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
	TypeRational
	TypeFloat
	TypeSyntax
)

// Value represents a Scheme value.
type Value struct {
	Type    ValueType
	IntVal   int64
	BoolVal  bool
	StrVal   string
	Num      int64   // rational numerator
	Denom    int64   // rational denominator
	FloatVal float64 // inexact float
	Runes   []rune // mutable string storage (used by string-copy/string-set!)
	Car     *Value
	Cdr     *Value
	// Lambda fields
	Params    []string
	RestParam string // variadic rest parameter (dot notation)
	Body      []*Expr
	ClosureEnv *Env
	// Macro fields
	Syntax *SyntaxRules
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
func CharValue(r rune) *Value { return &Value{Type: TypeChar, IntVal: int64(r)} }
func FloatValue(f float64) *Value { return &Value{Type: TypeFloat, FloatVal: f} }

func RationalValue(num, denom int64) *Value {
	if denom < 0 {
		num, denom = -num, -denom
	}
	if num == 0 {
		return IntValue(0)
	}
	g := gcd(num, denom)
	num /= g
	denom /= g
	if denom == 1 {
		return IntValue(num)
	}
	return &Value{Type: TypeRational, Num: num, Denom: denom}
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

func (v *Value) IsNumeric() bool {
	return v.Type == TypeInt || v.Type == TypeRational || v.Type == TypeFloat
}

func (v *Value) IsExact() bool {
	return v.Type == TypeInt || v.Type == TypeRational
}

func (v *Value) ToFloat64() float64 {
	switch v.Type {
	case TypeInt:
		return float64(v.IntVal)
	case TypeRational:
		return float64(v.Num) / float64(v.Denom)
	case TypeFloat:
		return v.FloatVal
	}
	return 0
}

// ToRat returns numerator and denominator for exact values.
func (v *Value) ToRat() (int64, int64) {
	switch v.Type {
	case TypeInt:
		return v.IntVal, 1
	case TypeRational:
		return v.Num, v.Denom
	}
	return 0, 1
}

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
	case TypeRational:
		return fmt.Sprintf("%d/%d", v.Num, v.Denom)
	case TypeFloat:
		s := strconv.FormatFloat(v.FloatVal, 'f', -1, 64)
		if !strings.Contains(s, ".") {
			s += ".0"
		}
		return s
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
