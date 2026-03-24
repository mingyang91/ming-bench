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

type IntVal struct {
	Val int64
}

func (v *IntVal) String() string { return fmt.Sprintf("%d", v.Val) }

// RationalVal represents an exact rational number (num/denom, always simplified, denom > 0).
type RationalVal struct {
	Num   int64
	Denom int64
}

func (v *RationalVal) String() string { return fmt.Sprintf("%d/%d", v.Num, v.Denom) }

// FloatVal represents an inexact number.
type FloatVal struct {
	Val float64
}

func (v *FloatVal) String() string {
	s := strconv.FormatFloat(v.Val, 'f', -1, 64)
	// Ensure there's always a decimal point
	if !strings.Contains(s, ".") {
		s += ".0"
	}
	return s
}

// gcd computes the greatest common divisor (always positive).
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

// makeRational creates a simplified rational or integer value.
func makeRational(num, denom int64) Value {
	if denom == 0 {
		return nil // should not happen
	}
	if denom < 0 {
		num, denom = -num, -denom
	}
	g := gcd(num, denom)
	num /= g
	denom /= g
	if denom == 1 {
		return &IntVal{Val: num}
	}
	return &RationalVal{Num: num, Denom: denom}
}

// isNumeric returns true if the value is a number (int, rational, or float).
func isNumeric(v Value) bool {
	switch v.(type) {
	case *IntVal, *RationalVal, *FloatVal:
		return true
	}
	return false
}

// toFloat64 converts any numeric value to float64.
func toFloat64(v Value) (float64, bool) {
	switch n := v.(type) {
	case *IntVal:
		return float64(n.Val), true
	case *RationalVal:
		return float64(n.Num) / float64(n.Denom), true
	case *FloatVal:
		return n.Val, true
	}
	return 0, false
}

// isExact returns true if the value is an exact number.
func isExact(v Value) bool {
	switch v.(type) {
	case *IntVal, *RationalVal:
		return true
	}
	return false
}

// toRational converts exact numbers to (num, denom) form.
func toRational(v Value) (int64, int64, bool) {
	switch n := v.(type) {
	case *IntVal:
		return n.Val, 1, true
	case *RationalVal:
		return n.Num, n.Denom, true
	}
	return 0, 0, false
}

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
	Val       string
	Immutable bool
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
	visited := make(map[*PairVal]bool)
	cur := Value(v)
	first := true
	for {
		p, ok := cur.(*PairVal)
		if !ok {
			break
		}
		if visited[p] {
			buf.WriteString(" ...")
			break
		}
		visited[p] = true
		if !first {
			buf.WriteByte(' ')
		}
		first = false
		buf.WriteString(p.Car.String())
		cur = p.Cdr
	}
	if _, ok := cur.(*NilVal); !ok {
		if p, ok := cur.(*PairVal); !ok || !visited[p] {
			buf.WriteString(" . ")
			buf.WriteString(cur.String())
		}
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
	Params    []string
	RestParam string // variadic rest parameter (empty if none)
	Body      []*Expr
	Env       *Env
}

func (v *LambdaVal) String() string { return "#<procedure>" }

// CaseLambdaClauses represents one clause of a case-lambda.
type CaseLambdaClause struct {
	Params    []string
	RestParam string
	Body      []*Expr
}

// CaseLambdaVal represents a case-lambda (multi-arity procedure).
type CaseLambdaVal struct {
	Clauses []CaseLambdaClause
	Env     *Env
}

func (v *CaseLambdaVal) String() string { return "#<procedure>" }

// ApplyVal is the first-class apply procedure.
type ApplyVal struct{}

func (v *ApplyVal) String() string { return "#<procedure apply>" }

// MapVal is the built-in map procedure.
type MapVal struct{}

func (v *MapVal) String() string { return "#<procedure map>" }

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
	case *VectorVal:
		return displayVector(val)
	default:
		return v.String()
	}
}

func displayVector(v *VectorVal) string {
	var buf strings.Builder
	buf.WriteString("#(")
	for i, e := range v.Elems {
		if i > 0 {
			buf.WriteByte(' ')
		}
		buf.WriteString(DisplayString(e))
	}
	buf.WriteByte(')')
	return buf.String()
}

func displayPair(v *PairVal) string {
	var buf strings.Builder
	buf.WriteByte('(')
	visited := make(map[*PairVal]bool)
	cur := Value(v)
	first := true
	for {
		p, ok := cur.(*PairVal)
		if !ok {
			break
		}
		if visited[p] {
			buf.WriteString(" ...")
			break
		}
		visited[p] = true
		if !first {
			buf.WriteByte(' ')
		}
		first = false
		buf.WriteString(DisplayString(p.Car))
		cur = p.Cdr
	}
	if _, ok := cur.(*NilVal); !ok {
		if p, ok := cur.(*PairVal); !ok || !visited[p] {
			buf.WriteString(" . ")
			buf.WriteString(DisplayString(cur))
		}
	}
	buf.WriteByte(')')
	return buf.String()
}

// RecordType describes a record type created by define-record-type.
type RecordType struct {
	Name   string
	Fields []string
}

// RecordVal represents an instance of a record type.
type RecordVal struct {
	Type   *RecordType
	Fields []Value
}

func (v *RecordVal) String() string {
	return fmt.Sprintf("#<%s>", v.Type.Name)
}

// VectorVal represents a Scheme vector (fixed-size mutable array).
type VectorVal struct {
	Elems []Value
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

// CallCCVal is the first-class call/cc procedure.
type CallCCVal struct{}

func (v *CallCCVal) String() string { return "#<procedure call/cc>" }

// DynamicWindVal is the first-class dynamic-wind procedure.
type DynamicWindVal struct{}

func (v *DynamicWindVal) String() string { return "#<procedure dynamic-wind>" }

// windRecord tracks an active dynamic-wind extent.
type windRecord struct {
	id       int
	inThunk  Value
	outThunk Value
}

// ContinuationVal represents a captured first-class continuation.
type ContinuationVal struct {
	Frames    []contFrame
	WindStack []windRecord
}

func (v *ContinuationVal) String() string { return "#<continuation>" }

// contFrame represents one level of the evaluation context for continuation capture.
type contFrame struct {
	Kind    contFrameKind
	Exprs   []*Expr // for body/topLevel
	Env     *Env
	Idx     int    // for body/topLevel: current expression index
	VarName string // for define/set
}

type contFrameKind int

const (
	frameBody    contFrameKind = iota // let/begin/lambda body
	frameTopLevel                     // top-level expression sequence
	frameDefine                       // pending define
	frameSet                          // pending set!
)

// isTruthy returns true for all values except #f.
func isTruthy(v Value) bool {
	if b, ok := v.(*BoolVal); ok {
		return b.Val
	}
	return true
}
