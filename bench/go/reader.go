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
	TypeBuiltin
)

// Value represents a Scheme value.
type Value struct {
	Type    ValueType
	Int     int64
	Bool    bool
	Str     string
	Car     *Value
	Cdr     *Value
	// Lambda fields
	Params  []string
	Body    []*Value
	Closure *Env
	// Builtin function
	BuiltinFunc func([]*Value) (*Value, error)
}

var voidValue = &Value{Type: TypeVoid}
var nullValue = &Value{Type: TypeNull}

func makeInt(n int64) *Value    { return &Value{Type: TypeInteger, Int: n} }
func makeBool(b bool) *Value    { return &Value{Type: TypeBoolean, Bool: b} }
func makeString(s string) *Value { return &Value{Type: TypeString, Str: s} }
func makeSymbol(s string) *Value { return &Value{Type: TypeSymbol, Str: s} }
func makePair(car, cdr *Value) *Value {
	return &Value{Type: TypePair, Car: car, Cdr: cdr}
}

func (v *Value) Display() string {
	switch v.Type {
	case TypeInteger:
		return strconv.FormatInt(v.Int, 10)
	case TypeBoolean:
		if v.Bool {
			return "#t"
		}
		return "#f"
	case TypeString:
		return fmt.Sprintf("%q", v.Str)
	case TypeSymbol:
		return v.Str
	case TypeNull:
		return "()"
	case TypePair:
		return displayList(v)
	case TypeVoid:
		return ""
	case TypeLambda:
		return "#<procedure>"
	case TypeBuiltin:
		return fmt.Sprintf("#<builtin %s>", v.Str)
	default:
		return ""
	}
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

// isTruthy returns whether a value is truthy (everything except #f).
func isTruthy(v *Value) bool {
	return !(v.Type == TypeBoolean && !v.Bool)
}

// Reader / Parser

type reader struct {
	input []rune
	pos   int
}

func newReader(input string) *reader {
	return &reader{input: []rune(input), pos: 0}
}

func (r *reader) atEnd() bool {
	return r.pos >= len(r.input)
}

func (r *reader) peek() rune {
	if r.atEnd() {
		return 0
	}
	return r.input[r.pos]
}

func (r *reader) next() rune {
	ch := r.input[r.pos]
	r.pos++
	return ch
}

func (r *reader) skipWhitespaceAndComments() {
	for !r.atEnd() {
		ch := r.peek()
		if unicode.IsSpace(ch) {
			r.next()
		} else if ch == ';' {
			// Line comment
			for !r.atEnd() && r.peek() != '\n' {
				r.next()
			}
		} else {
			break
		}
	}
}

func (r *reader) readExpr() (*Value, error) {
	r.skipWhitespaceAndComments()
	if r.atEnd() {
		return nil, fmt.Errorf("unexpected end of input")
	}

	ch := r.peek()

	switch {
	case ch == '(':
		return r.readList()
	case ch == '\'':
		r.next()
		expr, err := r.readExpr()
		if err != nil {
			return nil, err
		}
		return makePair(makeSymbol("quote"), makePair(expr, nullValue)), nil
	case ch == '"':
		return r.readString()
	case ch == '#':
		return r.readHash()
	default:
		return r.readAtom()
	}
}

func (r *reader) readList() (*Value, error) {
	r.next() // consume '('
	var items []*Value
	for {
		r.skipWhitespaceAndComments()
		if r.atEnd() {
			return nil, fmt.Errorf("unexpected end of input in list")
		}
		if r.peek() == ')' {
			r.next()
			// Build list from items
			result := nullValue
			for i := len(items) - 1; i >= 0; i-- {
				result = makePair(items[i], result)
			}
			return result, nil
		}
		expr, err := r.readExpr()
		if err != nil {
			return nil, err
		}
		items = append(items, expr)
	}
}

func (r *reader) readString() (*Value, error) {
	r.next() // consume opening '"'
	var buf []rune
	for {
		if r.atEnd() {
			return nil, fmt.Errorf("unterminated string")
		}
		ch := r.next()
		if ch == '"' {
			return makeString(string(buf)), nil
		}
		if ch == '\\' {
			if r.atEnd() {
				return nil, fmt.Errorf("unterminated string escape")
			}
			esc := r.next()
			switch esc {
			case 'n':
				buf = append(buf, '\n')
			case 't':
				buf = append(buf, '\t')
			case '\\':
				buf = append(buf, '\\')
			case '"':
				buf = append(buf, '"')
			default:
				buf = append(buf, '\\', esc)
			}
		} else {
			buf = append(buf, ch)
		}
	}
}

func (r *reader) readHash() (*Value, error) {
	r.next() // consume '#'
	if r.atEnd() {
		return nil, fmt.Errorf("unexpected end of input after #")
	}
	ch := r.next()
	switch ch {
	case 't':
		return makeBool(true), nil
	case 'f':
		return makeBool(false), nil
	default:
		return nil, fmt.Errorf("unknown hash literal: #%c", ch)
	}
}

func isDelimiter(ch rune) bool {
	return unicode.IsSpace(ch) || ch == '(' || ch == ')' || ch == '"' || ch == ';'
}

func (r *reader) readAtom() (*Value, error) {
	start := r.pos
	for !r.atEnd() && !isDelimiter(r.peek()) {
		r.next()
	}
	token := string(r.input[start:r.pos])

	// Try integer
	if n, err := strconv.ParseInt(token, 10, 64); err == nil {
		return makeInt(n), nil
	}

	// Symbol
	return makeSymbol(strings.ToLower(token)), nil
}

// readAll reads all expressions from the input.
func readAll(input string) ([]*Value, error) {
	r := newReader(input)
	var exprs []*Value
	for {
		r.skipWhitespaceAndComments()
		if r.atEnd() {
			break
		}
		expr, err := r.readExpr()
		if err != nil {
			return nil, err
		}
		exprs = append(exprs, expr)
	}
	return exprs, nil
}
