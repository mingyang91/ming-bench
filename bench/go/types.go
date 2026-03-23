package ming

import (
	"fmt"
	"strconv"
	"strings"
)

// SchemeValue represents any Scheme value.
type SchemeValue interface {
	String() string
}

type SchemeInt struct {
	Value int64
}

func (v *SchemeInt) String() string {
	return fmt.Sprintf("%d", v.Value)
}

type SchemeRational struct {
	Num int64
	Den int64
}

func (v *SchemeRational) String() string {
	return fmt.Sprintf("%d/%d", v.Num, v.Den)
}

type SchemeFloat struct {
	Value float64
}

func (v *SchemeFloat) String() string {
	s := strconv.FormatFloat(v.Value, 'g', -1, 64)
	if !strings.Contains(s, ".") && !strings.Contains(s, "e") && !strings.Contains(s, "E") && !strings.Contains(s, "Inf") && !strings.Contains(s, "NaN") {
		s += ".0"
	}
	return s
}

type SchemeBool struct {
	Value bool
}

func (v *SchemeBool) String() string {
	if v.Value {
		return "#t"
	}
	return "#f"
}

type SchemeString struct {
	Value   string
	Mutable bool
}

func (v *SchemeString) String() string {
	return fmt.Sprintf("%q", v.Value)
}

type SchemeSymbol struct {
	Name string
}

func (v *SchemeSymbol) String() string {
	return v.Name
}

type SchemeList struct {
	Elements []SchemeValue
}

func (v *SchemeList) String() string {
	parts := make([]string, len(v.Elements))
	for i, e := range v.Elements {
		parts[i] = e.String()
	}
	return "(" + strings.Join(parts, " ") + ")"
}

// SchemeEmpty represents the empty list '().
type SchemeEmpty struct{}

func (v *SchemeEmpty) String() string {
	return "()"
}

// SchemePair is a cons cell.
type SchemePair struct {
	Car SchemeValue
	Cdr SchemeValue
}

func (v *SchemePair) String() string {
	var parts []string
	cur := SchemeValue(v)
	for {
		switch c := cur.(type) {
		case *SchemePair:
			parts = append(parts, c.Car.String())
			cur = c.Cdr
		case *SchemeEmpty:
			return "(" + strings.Join(parts, " ") + ")"
		default:
			// dotted pair
			return "(" + strings.Join(parts, " ") + " . " + cur.String() + ")"
		}
	}
}

type SchemeChar struct {
	Value rune
}

func (v *SchemeChar) String() string {
	switch v.Value {
	case ' ':
		return `#\space`
	case '\n':
		return `#\newline`
	case '\t':
		return `#\tab`
	default:
		return fmt.Sprintf(`#\%c`, v.Value)
	}
}

type SchemeVoid struct{}

func (v *SchemeVoid) String() string {
	return ""
}

// SchemeVector is a fixed-size mutable array.
type SchemeVector struct {
	Elements []SchemeValue
}

func (v *SchemeVector) String() string {
	parts := make([]string, len(v.Elements))
	for i, e := range v.Elements {
		parts[i] = e.String()
	}
	return "#(" + strings.Join(parts, " ") + ")"
}

// SchemeCallCC represents the call/cc procedure as a first-class value.
type SchemeCallCC struct{}

func (v *SchemeCallCC) String() string { return "#<procedure call/cc>" }

// SchemeDynamicWind represents the dynamic-wind procedure as a first-class value.
type SchemeDynamicWind struct{}

func (v *SchemeDynamicWind) String() string { return "#<procedure dynamic-wind>" }

// SchemeRecord is an instance of a define-record-type.
type SchemeRecord struct {
	TypeID   *SchemeRecordType
	Fields   []SchemeValue
}

func (v *SchemeRecord) String() string {
	return fmt.Sprintf("#<record:%s>", v.TypeID.Name)
}

// SchemeRecordType is the runtime descriptor for a record type.
type SchemeRecordType struct {
	Name       string
	FieldNames []string
}

// SchemeMultipleValues wraps zero or more values returned by (values ...).
type SchemeMultipleValues struct {
	Values []SchemeValue
}

func (v *SchemeMultipleValues) String() string {
	if len(v.Values) == 0 {
		return ""
	}
	return v.Values[0].String()
}
