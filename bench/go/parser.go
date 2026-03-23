package ming

import (
	"fmt"
	"strconv"
	"strings"
	"unicode"
)

// Value types
type ValueType int

const (
	TypeInteger ValueType = iota
	TypeBoolean
	TypeString
	TypeSymbol
	TypePair
	TypeNull
	TypeVoid
)

type Value struct {
	Type    ValueType
	IntVal  int64
	BoolVal bool
	StrVal  string
	Car     *Value
	Cdr     *Value
}

var Void = &Value{Type: TypeVoid}
var Null = &Value{Type: TypeNull}

func NewInt(n int64) *Value    { return &Value{Type: TypeInteger, IntVal: n} }
func NewBool(b bool) *Value    { return &Value{Type: TypeBoolean, BoolVal: b} }
func NewString(s string) *Value { return &Value{Type: TypeString, StrVal: s} }
func NewSymbol(s string) *Value { return &Value{Type: TypeSymbol, StrVal: s} }
func NewPair(car, cdr *Value) *Value { return &Value{Type: TypePair, Car: car, Cdr: cdr} }

func (v *Value) Display() string {
	switch v.Type {
	case TypeInteger:
		return strconv.FormatInt(v.IntVal, 10)
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
	}
	return ""
}

func displayList(v *Value) string {
	var parts []string
	cur := v
	for cur.Type == TypePair {
		parts = append(parts, cur.Car.Display())
		cur = cur.Cdr
	}
	if cur.Type == TypeNull {
		return "(" + strings.Join(parts, " ") + ")"
	}
	// Dotted pair
	return "(" + strings.Join(parts, " ") + " . " + cur.Display() + ")"
}

// Parser
type parser struct {
	input []rune
	pos   int
}

func parse(input string) ([]*Value, error) {
	p := &parser{input: []rune(input), pos: 0}
	var exprs []*Value
	for {
		p.skipWhitespaceAndComments()
		if p.pos >= len(p.input) {
			break
		}
		expr, err := p.parseExpr()
		if err != nil {
			return nil, err
		}
		exprs = append(exprs, expr)
	}
	return exprs, nil
}

func (p *parser) skipWhitespaceAndComments() {
	for p.pos < len(p.input) {
		if unicode.IsSpace(p.input[p.pos]) {
			p.pos++
		} else if p.input[p.pos] == ';' {
			for p.pos < len(p.input) && p.input[p.pos] != '\n' {
				p.pos++
			}
		} else {
			break
		}
	}
}

func (p *parser) parseExpr() (*Value, error) {
	p.skipWhitespaceAndComments()
	if p.pos >= len(p.input) {
		return nil, fmt.Errorf("unexpected end of input")
	}

	ch := p.input[p.pos]

	if ch == '(' {
		return p.parseList()
	}
	if ch == '\'' {
		p.pos++
		expr, err := p.parseExpr()
		if err != nil {
			return nil, err
		}
		return NewPair(NewSymbol("quote"), NewPair(expr, Null)), nil
	}
	if ch == '"' {
		return p.parseString()
	}
	if ch == '#' {
		return p.parseHash()
	}

	return p.parseAtom()
}

func (p *parser) parseList() (*Value, error) {
	p.pos++ // skip '('
	var items []*Value
	for {
		p.skipWhitespaceAndComments()
		if p.pos >= len(p.input) {
			return nil, fmt.Errorf("unexpected end of input: unclosed '('")
		}
		if p.input[p.pos] == ')' {
			p.pos++
			// Build list from items
			result := Null
			for i := len(items) - 1; i >= 0; i-- {
				result = NewPair(items[i], result)
			}
			return result, nil
		}
		expr, err := p.parseExpr()
		if err != nil {
			return nil, err
		}
		items = append(items, expr)
	}
}

func (p *parser) parseString() (*Value, error) {
	p.pos++ // skip opening "
	var buf []rune
	for p.pos < len(p.input) {
		ch := p.input[p.pos]
		if ch == '\\' {
			p.pos++
			if p.pos >= len(p.input) {
				return nil, fmt.Errorf("unexpected end of string")
			}
			esc := p.input[p.pos]
			switch esc {
			case 'n':
				buf = append(buf, '\n')
			case 't':
				buf = append(buf, '\t')
			case '"':
				buf = append(buf, '"')
			case '\\':
				buf = append(buf, '\\')
			default:
				buf = append(buf, '\\', esc)
			}
			p.pos++
			continue
		}
		if ch == '"' {
			p.pos++
			return NewString(string(buf)), nil
		}
		buf = append(buf, ch)
		p.pos++
	}
	return nil, fmt.Errorf("unterminated string")
}

func (p *parser) parseHash() (*Value, error) {
	p.pos++ // skip '#'
	if p.pos >= len(p.input) {
		return nil, fmt.Errorf("unexpected end of input after #")
	}
	ch := p.input[p.pos]
	p.pos++
	switch ch {
	case 't':
		return NewBool(true), nil
	case 'f':
		return NewBool(false), nil
	}
	return nil, fmt.Errorf("unknown hash literal: #%c", ch)
}

func (p *parser) parseAtom() (*Value, error) {
	start := p.pos
	for p.pos < len(p.input) {
		ch := p.input[p.pos]
		if unicode.IsSpace(ch) || ch == '(' || ch == ')' || ch == '"' || ch == ';' {
			break
		}
		p.pos++
	}
	token := string(p.input[start:p.pos])
	if token == "" {
		return nil, fmt.Errorf("unexpected character: %c", p.input[start])
	}

	// Try integer
	if n, err := strconv.ParseInt(token, 10, 64); err == nil {
		return NewInt(n), nil
	}

	// Symbol
	return NewSymbol(strings.ToLower(token)), nil
}
