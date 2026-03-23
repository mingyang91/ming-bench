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
	TypeChar
	TypeContinuation
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
	Params    []string
	RestParam string // variadic rest parameter name (empty if none)
	Body      []*Value
	Closure   *Env
	// Builtin function
	BuiltinFunc func([]*Value) (*Value, error)
	// Continuation function
	ContFunc func(*Value)
	// RestFn evaluates the remaining computation. Set lazily after callCC returns.
	RestFn func(*Value) (*Value, error)
	// Source position
	Line int
	Col  int
}

var voidValue = &Value{Type: TypeVoid}
var nullValue = &Value{Type: TypeNull}

func makeInt(n int64) *Value    { return &Value{Type: TypeInteger, Int: n} }
func makeBool(b bool) *Value    { return &Value{Type: TypeBoolean, Bool: b} }
func makeString(s string) *Value { return &Value{Type: TypeString, Str: s} }
func makeSymbol(s string) *Value { return &Value{Type: TypeSymbol, Str: s} }
func makeChar(c rune) *Value     { return &Value{Type: TypeChar, Int: int64(c)} }
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
	case TypeChar:
		return fmt.Sprintf("#\\%c", rune(v.Int))
	case TypeContinuation:
		return "#<continuation>"
	default:
		return ""
	}
}

// DisplayPlain returns the display representation (no quotes on strings).
func (v *Value) DisplayPlain() string {
	switch v.Type {
	case TypeString:
		return v.Str
	case TypeChar:
		return string(rune(v.Int))
	default:
		return v.Display()
	}
}

// WriteRepr returns the write representation (strings quoted).
func (v *Value) WriteRepr() string {
	return v.Display()
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
	line  int
	col   int
}

func newReader(input string) *reader {
	return &reader{input: []rune(input), pos: 0, line: 1, col: 1}
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
	if ch == '\n' {
		r.line++
		r.col = 1
	} else {
		r.col++
	}
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
		return nil, &EvalError{Message: "unexpected end of input", Line: r.line, Col: r.col}
	}

	line, col := r.line, r.col
	ch := r.peek()

	var v *Value
	var err error
	switch {
	case ch == '(':
		v, err = r.readList()
	case ch == '\'':
		r.next()
		expr, err2 := r.readExpr()
		if err2 != nil {
			return nil, err2
		}
		v = makePair(makeSymbol("quote"), makePair(expr, nullValue))
	case ch == '"':
		v, err = r.readString()
	case ch == '#':
		v, err = r.readHash()
	default:
		v, err = r.readAtom()
	}
	if err != nil {
		return nil, err
	}
	v.Line = line
	v.Col = col
	return v, nil
}

func (r *reader) readList() (*Value, error) {
	r.next() // consume '('
	var items []*Value
	for {
		r.skipWhitespaceAndComments()
		if r.atEnd() {
			return nil, &EvalError{Message: "unexpected end of input in list", Line: r.line, Col: r.col}
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
		// Check for dot notation
		if r.peek() == '.' && r.pos+1 < len(r.input) && isDelimiter(r.input[r.pos+1]) {
			r.next() // consume '.'
			r.skipWhitespaceAndComments()
			cdr, err := r.readExpr()
			if err != nil {
				return nil, err
			}
			r.skipWhitespaceAndComments()
			if r.atEnd() || r.peek() != ')' {
				return nil, &EvalError{Message: "expected ')' after dotted pair", Line: r.line, Col: r.col}
			}
			r.next() // consume ')'
			// Build dotted list
			result := cdr
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
			return nil, &EvalError{Message: "unterminated string", Line: r.line, Col: r.col}
		}
		ch := r.next()
		if ch == '"' {
			return makeString(string(buf)), nil
		}
		if ch == '\\' {
			if r.atEnd() {
				return nil, &EvalError{Message: "unterminated string escape", Line: r.line, Col: r.col}
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
		return nil, &EvalError{Message: "unexpected end of input after #", Line: r.line, Col: r.col}
	}
	ch := r.next()
	switch ch {
	case 't':
		return makeBool(true), nil
	case 'f':
		return makeBool(false), nil
	case '\\':
		// Character literal: #\x or #\space, #\newline, etc.
		if r.atEnd() {
			return nil, &EvalError{Message: "unexpected end of input in character literal", Line: r.line, Col: r.col}
		}
		c := r.next()
		// Check for named characters
		if !r.atEnd() && !isDelimiter(r.peek()) {
			// Multi-character name like #\space, #\newline
			name := string(c)
			for !r.atEnd() && !isDelimiter(r.peek()) {
				name += string(r.next())
			}
			switch strings.ToLower(name) {
			case "space":
				return makeChar(' '), nil
			case "newline":
				return makeChar('\n'), nil
			case "tab":
				return makeChar('\t'), nil
			default:
				return nil, &EvalError{Message: fmt.Sprintf("unknown character name: #\\%s", name), Line: r.line, Col: r.col}
			}
		}
		return makeChar(c), nil
	default:
		return nil, &EvalError{Message: fmt.Sprintf("unknown hash literal: #%c", ch), Line: r.line, Col: r.col}
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
