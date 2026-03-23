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
	TypeLambda
)

// Pos represents a source position (1-based line and column).
type Pos struct {
	Line int
	Col  int
}

func (p Pos) String() string {
	return fmt.Sprintf("%d:%d", p.Line, p.Col)
}

type Value struct {
	Type    ValueType
	IntVal  int64
	BoolVal bool
	StrVal  string
	Car     *Value
	Cdr     *Value
	// Lambda fields
	Params     []string
	Body       []*Value
	ClosureEnv *Env
	// Source position
	SrcPos Pos
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
	case TypeLambda:
		return "#<procedure>"
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
	line  int
	col   int
}

func (p *parser) curPos() Pos {
	return Pos{Line: p.line, Col: p.col}
}

func (p *parser) advance() {
	if p.pos < len(p.input) {
		if p.input[p.pos] == '\n' {
			p.line++
			p.col = 1
		} else {
			p.col++
		}
		p.pos++
	}
}

func parse(input string) ([]*Value, error) {
	p := &parser{input: []rune(input), pos: 0, line: 1, col: 1}
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
			p.advance()
		} else if p.input[p.pos] == ';' {
			for p.pos < len(p.input) && p.input[p.pos] != '\n' {
				p.advance()
			}
		} else {
			break
		}
	}
}

func (p *parser) parseExpr() (*Value, error) {
	p.skipWhitespaceAndComments()
	if p.pos >= len(p.input) {
		return nil, fmt.Errorf("unexpected end of input at %s", p.curPos())
	}

	ch := p.input[p.pos]

	if ch == '(' {
		return p.parseList()
	}
	if ch == '\'' {
		pos := p.curPos()
		p.advance()
		expr, err := p.parseExpr()
		if err != nil {
			return nil, err
		}
		q := NewSymbol("quote")
		q.SrcPos = pos
		v := NewPair(q, NewPair(expr, Null))
		v.SrcPos = pos
		return v, nil
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
	pos := p.curPos()
	p.advance() // skip '('
	var items []*Value
	for {
		p.skipWhitespaceAndComments()
		if p.pos >= len(p.input) {
			return nil, fmt.Errorf("unexpected end of input at %s: unclosed '('", pos)
		}
		if p.input[p.pos] == ')' {
			p.advance()
			// Build list from items
			result := Null
			for i := len(items) - 1; i >= 0; i-- {
				r := NewPair(items[i], result)
				r.SrcPos = items[i].SrcPos
				result = r
			}
			result.SrcPos = pos
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
	pos := p.curPos()
	p.advance() // skip opening "
	var buf []rune
	for p.pos < len(p.input) {
		ch := p.input[p.pos]
		if ch == '\\' {
			p.advance()
			if p.pos >= len(p.input) {
				return nil, fmt.Errorf("unexpected end of string at %s", pos)
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
			p.advance()
			continue
		}
		if ch == '"' {
			p.advance()
			v := NewString(string(buf))
			v.SrcPos = pos
			return v, nil
		}
		buf = append(buf, ch)
		p.advance()
	}
	return nil, fmt.Errorf("unterminated string at %s", pos)
}

func (p *parser) parseHash() (*Value, error) {
	pos := p.curPos()
	p.advance() // skip '#'
	if p.pos >= len(p.input) {
		return nil, fmt.Errorf("unexpected end of input at %s after #", pos)
	}
	ch := p.input[p.pos]
	p.advance()
	switch ch {
	case 't':
		v := NewBool(true)
		v.SrcPos = pos
		return v, nil
	case 'f':
		v := NewBool(false)
		v.SrcPos = pos
		return v, nil
	}
	return nil, fmt.Errorf("unknown hash literal at %s: #%c", pos, ch)
}

func (p *parser) parseAtom() (*Value, error) {
	pos := p.curPos()
	start := p.pos
	for p.pos < len(p.input) {
		ch := p.input[p.pos]
		if unicode.IsSpace(ch) || ch == '(' || ch == ')' || ch == '"' || ch == ';' {
			break
		}
		p.advance()
	}
	token := string(p.input[start:p.pos])
	if token == "" {
		return nil, fmt.Errorf("unexpected character at %s: %c", pos, p.input[start])
	}

	// Try integer
	if n, err := strconv.ParseInt(token, 10, 64); err == nil {
		v := NewInt(n)
		v.SrcPos = pos
		return v, nil
	}

	// Symbol
	v := NewSymbol(strings.ToLower(token))
	v.SrcPos = pos
	return v, nil
}
