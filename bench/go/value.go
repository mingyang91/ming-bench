package ming

import (
	"fmt"
	"strings"
)

type ValueKind int

const (
	KindInteger ValueKind = iota
	KindBoolean
	KindString
	KindSymbol
	KindPair
	KindNull
	KindVoid
	KindBuiltin
	KindLambda
	KindChar
	KindTailCall // internal: trampoline for TCO
)

type BuiltinFunc func(args []*Value) (*Value, error)

type Value struct {
	Kind    ValueKind
	Int     int64
	Bool    bool
	Str     string
	Car     *Value
	Cdr     *Value
	Builtin BuiltinFunc
	// Lambda fields
	Params    []string
	RestParam string // variadic rest parameter name (empty if none)
	Body      []*Value
	ClosureEnv *Env
	// Source position
	Line int
	Col  int
}

func intVal(n int64) *Value   { return &Value{Kind: KindInteger, Int: n} }
func boolVal(b bool) *Value   { return &Value{Kind: KindBoolean, Bool: b} }
func strVal(s string) *Value  { return &Value{Kind: KindString, Str: s} }
func symVal(s string) *Value  { return &Value{Kind: KindSymbol, Str: s} }
func nullVal() *Value         { return &Value{Kind: KindNull} }
func voidVal() *Value         { return &Value{Kind: KindVoid} }
func charVal(ch rune) *Value  { return &Value{Kind: KindChar, Int: int64(ch)} }

func (v *Value) withPos(line, col int) *Value {
	v.Line = line
	v.Col = col
	return v
}
func pairVal(car, cdr *Value) *Value {
	return &Value{Kind: KindPair, Car: car, Cdr: cdr}
}
func builtinVal(name string, fn BuiltinFunc) *Value {
	return &Value{Kind: KindBuiltin, Str: name, Builtin: fn}
}

func (v *Value) isTruthy() bool {
	return !(v.Kind == KindBoolean && !v.Bool)
}

func (v *Value) String() string {
	switch v.Kind {
	case KindInteger:
		return fmt.Sprintf("%d", v.Int)
	case KindBoolean:
		if v.Bool {
			return "#t"
		}
		return "#f"
	case KindString:
		return fmt.Sprintf("%q", v.Str)
	case KindSymbol:
		return v.Str
	case KindNull:
		return "()"
	case KindVoid:
		return ""
	case KindBuiltin:
		return fmt.Sprintf("#<procedure:%s>", v.Str)
	case KindLambda:
		return "#<procedure>"
	case KindPair:
		return printList(v)
	case KindChar:
		return fmt.Sprintf("#\\%c", rune(v.Int))
	}
	return ""
}

// Display returns the display representation (no quotes on strings).
func (v *Value) Display() string {
	switch v.Kind {
	case KindString:
		return v.Str
	case KindChar:
		return string(rune(v.Int))
	case KindPair:
		return displayList(v)
	default:
		return v.String()
	}
}

func displayList(v *Value) string {
	var buf strings.Builder
	buf.WriteByte('(')
	cur := v
	first := true
	for cur.Kind == KindPair {
		if !first {
			buf.WriteByte(' ')
		}
		buf.WriteString(cur.Car.Display())
		first = false
		cur = cur.Cdr
	}
	if cur.Kind != KindNull {
		buf.WriteString(" . ")
		buf.WriteString(cur.Display())
	}
	buf.WriteByte(')')
	return buf.String()
}

func printList(v *Value) string {
	var buf strings.Builder
	buf.WriteByte('(')
	cur := v
	first := true
	for cur.Kind == KindPair {
		if !first {
			buf.WriteByte(' ')
		}
		buf.WriteString(cur.Car.String())
		first = false
		cur = cur.Cdr
	}
	if cur.Kind != KindNull {
		buf.WriteString(" . ")
		buf.WriteString(cur.String())
	}
	buf.WriteByte(')')
	return buf.String()
}
