package ming

import (
	"fmt"
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
	Value string
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
