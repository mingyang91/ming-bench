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

type SchemeVoid struct{}

func (v *SchemeVoid) String() string {
	return ""
}
