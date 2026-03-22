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
	TypeLambda
	TypeChar
	TypeContinuation
	TypeMacro
	TypeVector
	TypeMultipleValues
	TypeRational
	TypeFloat
	TypeRecord
	TypeSyntax
)

// Value represents a Scheme value.
type Value struct {
	Type    ValueType
	IntVal  int64
	BoolVal bool
	StrVal  string
	Runes   []rune // mutable string storage (used when non-nil)
	Car     *Value
	Cdr     *Value
	// Lambda fields
	Params    []string
	RestParam string // variadic rest parameter name (empty if none)
	Body      []*Expr
	Closure   *Env
	// Continuation fields
	ContFunc func(*Value) // invoked when continuation is called; always panics
	// Macro fields
	Macro     *SyntaxRules
	MacroProc *Value // syntax-case transformer (a lambda)
	// Vector fields
	VecElems []*Value
	// Multiple values fields
	Values []*Value
	// Rational fields (Num/Den, always reduced, Den > 0)
	Num int64
	Den int64
	// Float field
	FloatVal float64
	// Record fields
	RecordTypeID   int64    // unique ID for this record type
	RecordTypeName string   // name of the record type (e.g., "<point>")
	RecordFields   []*Value // field values in constructor order
	// Syntax object fields
	SyntaxExpr  *Expr   // for TypeSyntax: wrapped expression
	SyntaxExprs []*Expr // for TypeSyntax: list of expressions (ellipsis binding)
	// Environment reference (for internal use)
	EnvRef *Env
	// Native Go function (when set on TypeLambda, called instead of Body)
	GoFunc func([]*Value) (*Value, error)
}

// StrContent returns the string content, preferring mutable Runes if set.
func (v *Value) StrContent() string {
	if v.Runes != nil {
		return string(v.Runes)
	}
	return v.StrVal
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

func CharValue(c rune) *Value {
	return &Value{Type: TypeChar, IntVal: int64(c)}
}

// DisplayString returns the display representation (no quotes on strings).
func (v *Value) DisplayString() string {
	switch v.Type {
	case TypeString:
		return v.StrContent()
	case TypeChar:
		return string(rune(v.IntVal))
	default:
		return v.String()
	}
}

// IsTruthy returns true for all values except #f.
func (v *Value) IsTruthy() bool {
	return !(v.Type == TypeBoolean && !v.BoolVal)
}

// String returns the external representation of the value.
func (v *Value) String() string {
	switch v.Type {
	case TypeRational:
		return fmt.Sprintf("%d/%d", v.Num, v.Den)
	case TypeFloat:
		return formatFloat(v.FloatVal)
	case TypeInteger:
		return fmt.Sprintf("%d", v.IntVal)
	case TypeBoolean:
		if v.BoolVal {
			return "#t"
		}
		return "#f"
	case TypeString:
		return fmt.Sprintf("%q", v.StrContent())
	case TypeSymbol:
		return v.StrVal
	case TypeNull:
		return "()"
	case TypeVoid:
		return ""
	case TypeChar:
		c := rune(v.IntVal)
		switch c {
		case ' ':
			return "#\\space"
		case '\n':
			return "#\\newline"
		case '\t':
			return "#\\tab"
		default:
			return fmt.Sprintf("#\\%c", c)
		}
	case TypeLambda:
		return "#<procedure>"
	case TypeContinuation:
		return "#<continuation>"
	case TypeMacro:
		return "#<macro>"
	case TypeVector:
		var buf strings.Builder
		buf.WriteString("#(")
		for i, elem := range v.VecElems {
			if i > 0 {
				buf.WriteByte(' ')
			}
			buf.WriteString(elem.String())
		}
		buf.WriteByte(')')
		return buf.String()
	case TypePair:
		var buf strings.Builder
		buf.WriteByte('(')
		buf.WriteString(v.Car.String())
		cur := v.Cdr
		seen := map[*Value]bool{v: true}
		for cur.Type == TypePair {
			if seen[cur] {
				buf.WriteString(" ...")
				break
			}
			seen[cur] = true
			buf.WriteByte(' ')
			buf.WriteString(cur.Car.String())
			cur = cur.Cdr
		}
		if cur.Type != TypeNull && !seen[cur] {
			buf.WriteString(" . ")
			buf.WriteString(cur.String())
		}
		buf.WriteByte(')')
		return buf.String()
	case TypeRecord:
		return fmt.Sprintf("#<%s>", v.RecordTypeName)
	case TypeSyntax:
		return "#<syntax>"
	default:
		return "<unknown>"
	}
}
