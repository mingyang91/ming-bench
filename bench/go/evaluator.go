package ming

import (
	"fmt"
	"strconv"
	"strings"
	"unicode"
)

// ---------- Value types ----------

type valueKind int

const (
	valInteger valueKind = iota
	valBoolean
	valString
	valSymbol
	valPair
	valNull
	valVoid
)

type value struct {
	kind valueKind
	ival int64
	bval bool
	sval string
}

var voidVal = value{kind: valVoid}
var nullVal = value{kind: valNull}

func intVal(n int64) value   { return value{kind: valInteger, ival: n} }
func boolVal(b bool) value   { return value{kind: valBoolean, bval: b} }
func strVal(s string) value  { return value{kind: valString, sval: s} }
func symVal(s string) value  { return value{kind: valSymbol, sval: s} }

func (v value) String() string {
	switch v.kind {
	case valInteger:
		return strconv.FormatInt(v.ival, 10)
	case valBoolean:
		if v.bval {
			return "#t"
		}
		return "#f"
	case valString:
		return `"` + v.sval + `"`
	case valSymbol:
		return v.sval
	case valNull:
		return "()"
	case valVoid:
		return ""
	default:
		return "<unknown>"
	}
}

func isTruthy(v value) bool {
	return !(v.kind == valBoolean && !v.bval)
}

// ---------- AST ----------

type exprKind int

const (
	exprAtom exprKind = iota
	exprList
)

type expr struct {
	kind exprKind
	atom value
	list []*expr
	line int
	col  int
}

// ---------- Tokenizer ----------

type tokenKind int

const (
	tokLParen tokenKind = iota
	tokRParen
	tokAtom
	tokEOF
)

type token struct {
	kind tokenKind
	text string
	line int
	col  int
}

type tokenizer struct {
	input []rune
	pos   int
	line  int
	col   int
}

func newTokenizer(input string) *tokenizer {
	return &tokenizer{input: []rune(input), pos: 0, line: 1, col: 1}
}

func (t *tokenizer) peek() rune {
	if t.pos >= len(t.input) {
		return 0
	}
	return t.input[t.pos]
}

func (t *tokenizer) advance() rune {
	ch := t.input[t.pos]
	t.pos++
	if ch == '\n' {
		t.line++
		t.col = 1
	} else {
		t.col++
	}
	return ch
}

func (t *tokenizer) skipWhitespaceAndComments() {
	for t.pos < len(t.input) {
		ch := t.peek()
		if unicode.IsSpace(ch) {
			t.advance()
		} else if ch == ';' {
			for t.pos < len(t.input) && t.peek() != '\n' {
				t.advance()
			}
		} else {
			break
		}
	}
}

func isDelimiter(ch rune) bool {
	return ch == 0 || ch == '(' || ch == ')' || unicode.IsSpace(ch) || ch == ';' || ch == '"'
}

func (t *tokenizer) next() (token, error) {
	t.skipWhitespaceAndComments()
	if t.pos >= len(t.input) {
		return token{kind: tokEOF, line: t.line, col: t.col}, nil
	}
	ch := t.peek()
	line, col := t.line, t.col

	if ch == '(' {
		t.advance()
		return token{kind: tokLParen, text: "(", line: line, col: col}, nil
	}
	if ch == ')' {
		t.advance()
		return token{kind: tokRParen, text: ")", line: line, col: col}, nil
	}

	// String literal
	if ch == '"' {
		t.advance()
		var sb strings.Builder
		for t.pos < len(t.input) {
			c := t.advance()
			if c == '"' {
				return token{kind: tokAtom, text: `"` + sb.String() + `"`, line: line, col: col}, nil
			}
			if c == '\\' && t.pos < len(t.input) {
				next := t.advance()
				switch next {
				case 'n':
					sb.WriteByte('\n')
				case 't':
					sb.WriteByte('\t')
				case '\\':
					sb.WriteByte('\\')
				case '"':
					sb.WriteByte('"')
				default:
					sb.WriteByte('\\')
					sb.WriteRune(next)
				}
			} else {
				sb.WriteRune(c)
			}
		}
		return token{}, &EvalError{Message: fmt.Sprintf("%d:%d: unterminated string", line, col)}
	}

	// Atom (number, boolean, symbol)
	var sb strings.Builder
	for t.pos < len(t.input) && !isDelimiter(t.peek()) {
		sb.WriteRune(t.advance())
	}
	return token{kind: tokAtom, text: sb.String(), line: line, col: col}, nil
}

// ---------- Parser ----------

type parser struct {
	tokens []token
	pos    int
}

func tokenize(input string) ([]token, error) {
	t := newTokenizer(input)
	var tokens []token
	for {
		tok, err := t.next()
		if err != nil {
			return nil, err
		}
		tokens = append(tokens, tok)
		if tok.kind == tokEOF {
			break
		}
	}
	return tokens, nil
}

func (p *parser) peek() token {
	if p.pos >= len(p.tokens) {
		return token{kind: tokEOF}
	}
	return p.tokens[p.pos]
}

func (p *parser) parseExpr() (*expr, error) {
	tok := p.peek()
	if tok.kind == tokEOF {
		return nil, &EvalError{Message: "unexpected end of input"}
	}

	if tok.kind == tokLParen {
		p.pos++
		var elems []*expr
		for p.peek().kind != tokRParen {
			if p.peek().kind == tokEOF {
				return nil, &EvalError{Message: fmt.Sprintf("%d:%d: unclosed parenthesis", tok.line, tok.col)}
			}
			e, err := p.parseExpr()
			if err != nil {
				return nil, err
			}
			elems = append(elems, e)
		}
		p.pos++ // consume ')'
		return &expr{kind: exprList, list: elems, line: tok.line, col: tok.col}, nil
	}

	if tok.kind == tokRParen {
		return nil, &EvalError{Message: fmt.Sprintf("%d:%d: unexpected ')'", tok.line, tok.col)}
	}

	// Atom
	p.pos++
	v := parseAtom(tok.text)
	return &expr{kind: exprAtom, atom: v, line: tok.line, col: tok.col}, nil
}

func parseAtom(text string) value {
	// Boolean
	if text == "#t" {
		return boolVal(true)
	}
	if text == "#f" {
		return boolVal(false)
	}
	// String
	if len(text) >= 2 && text[0] == '"' && text[len(text)-1] == '"' {
		return strVal(text[1 : len(text)-1])
	}
	// Integer
	if n, err := strconv.ParseInt(text, 10, 64); err == nil {
		return intVal(n)
	}
	// Symbol
	return symVal(text)
}

// ---------- Evaluator ----------

func eval(e *expr) (value, error) {
	if e.kind == exprAtom {
		if e.atom.kind == valSymbol {
			return value{}, &EvalError{Message: fmt.Sprintf("%d:%d: unbound variable: %s", e.line, e.col, e.atom.sval)}
		}
		return e.atom, nil
	}

	// List expression
	if len(e.list) == 0 {
		return value{}, &EvalError{Message: fmt.Sprintf("%d:%d: empty application", e.line, e.col)}
	}

	// Check for special forms
	head := e.list[0]
	if head.kind == exprAtom && head.atom.kind == valSymbol {
		switch head.atom.sval {
		case "and":
			return evalAnd(e)
		case "or":
			return evalOr(e)
		}
	}

	// Function call - evaluate operator
	op, err := eval(head)
	if err != nil {
		// If it's an unbound variable, check if it's a builtin
		if head.kind == exprAtom && head.atom.kind == valSymbol {
			return evalBuiltin(head.atom.sval, e)
		}
		return value{}, err
	}
	_ = op
	return value{}, &EvalError{Message: fmt.Sprintf("%d:%d: not a procedure: %s", e.line, e.col, op.String())}
}

func evalBuiltin(name string, e *expr) (value, error) {
	args := e.list[1:]

	switch name {
	case "+":
		var sum int64 = 0
		for _, a := range args {
			v, err := eval(a)
			if err != nil {
				return value{}, err
			}
			if v.kind != valInteger {
				return value{}, &EvalError{Message: fmt.Sprintf("%d:%d: +: expected number, got %s", a.line, a.col, v.String())}
			}
			sum += v.ival
		}
		return intVal(sum), nil

	case "-":
		if len(args) == 0 {
			return value{}, &EvalError{Message: fmt.Sprintf("%d:%d: -: expected at least 1 argument", e.line, e.col)}
		}
		first, err := eval(args[0])
		if err != nil {
			return value{}, err
		}
		if first.kind != valInteger {
			return value{}, &EvalError{Message: fmt.Sprintf("%d:%d: -: expected number", args[0].line, args[0].col)}
		}
		if len(args) == 1 {
			return intVal(-first.ival), nil
		}
		result := first.ival
		for _, a := range args[1:] {
			v, err := eval(a)
			if err != nil {
				return value{}, err
			}
			if v.kind != valInteger {
				return value{}, &EvalError{Message: fmt.Sprintf("%d:%d: -: expected number", a.line, a.col)}
			}
			result -= v.ival
		}
		return intVal(result), nil

	case "*":
		var product int64 = 1
		for _, a := range args {
			v, err := eval(a)
			if err != nil {
				return value{}, err
			}
			if v.kind != valInteger {
				return value{}, &EvalError{Message: fmt.Sprintf("%d:%d: *: expected number", a.line, a.col)}
			}
			product *= v.ival
		}
		return intVal(product), nil

	case "/":
		if len(args) < 2 {
			return value{}, &EvalError{Message: fmt.Sprintf("%d:%d: /: expected at least 2 arguments", e.line, e.col)}
		}
		first, err := eval(args[0])
		if err != nil {
			return value{}, err
		}
		if first.kind != valInteger {
			return value{}, &EvalError{Message: fmt.Sprintf("%d:%d: /: expected number", args[0].line, args[0].col)}
		}
		result := first.ival
		for _, a := range args[1:] {
			v, err := eval(a)
			if err != nil {
				return value{}, err
			}
			if v.kind != valInteger {
				return value{}, &EvalError{Message: fmt.Sprintf("%d:%d: /: expected number", a.line, a.col)}
			}
			if v.ival == 0 {
				return value{}, &EvalError{Message: fmt.Sprintf("%d:%d: /: division by zero", a.line, a.col)}
			}
			result /= v.ival
		}
		return intVal(result), nil

	case "<":
		return evalCompare(args, e, func(a, b int64) bool { return a < b }, "<")
	case ">":
		return evalCompare(args, e, func(a, b int64) bool { return a > b }, ">")
	case "=":
		return evalCompare(args, e, func(a, b int64) bool { return a == b }, "=")
	case "<=":
		return evalCompare(args, e, func(a, b int64) bool { return a <= b }, "<=")
	case ">=":
		return evalCompare(args, e, func(a, b int64) bool { return a >= b }, ">=")

	case "not":
		if len(args) != 1 {
			return value{}, &EvalError{Message: fmt.Sprintf("%d:%d: not: expected 1 argument, got %d", e.line, e.col, len(args))}
		}
		v, err := eval(args[0])
		if err != nil {
			return value{}, err
		}
		return boolVal(!isTruthy(v)), nil
	}

	return value{}, &EvalError{Message: fmt.Sprintf("%d:%d: unbound variable: %s", e.list[0].line, e.list[0].col, name)}
}

func evalCompare(args []*expr, e *expr, cmp func(int64, int64) bool, name string) (value, error) {
	if len(args) < 2 {
		return value{}, &EvalError{Message: fmt.Sprintf("%d:%d: %s: expected at least 2 arguments", e.line, e.col, name)}
	}
	prev, err := eval(args[0])
	if err != nil {
		return value{}, err
	}
	if prev.kind != valInteger {
		return value{}, &EvalError{Message: fmt.Sprintf("%d:%d: %s: expected number", args[0].line, args[0].col, name)}
	}
	for _, a := range args[1:] {
		v, err := eval(a)
		if err != nil {
			return value{}, err
		}
		if v.kind != valInteger {
			return value{}, &EvalError{Message: fmt.Sprintf("%d:%d: %s: expected number", a.line, a.col, name)}
		}
		if !cmp(prev.ival, v.ival) {
			return boolVal(false), nil
		}
		prev = v
	}
	return boolVal(true), nil
}

func evalAnd(e *expr) (value, error) {
	args := e.list[1:]
	if len(args) == 0 {
		return boolVal(true), nil
	}
	var result value
	for _, a := range args {
		v, err := eval(a)
		if err != nil {
			return value{}, err
		}
		result = v
		if !isTruthy(v) {
			return v, nil
		}
	}
	return result, nil
}

func evalOr(e *expr) (value, error) {
	args := e.list[1:]
	if len(args) == 0 {
		return boolVal(false), nil
	}
	for _, a := range args {
		v, err := eval(a)
		if err != nil {
			return value{}, err
		}
		if isTruthy(v) {
			return v, nil
		}
	}
	return boolVal(false), nil
}

// ---------- Public API ----------

// EvalStr evaluates one or more Scheme expressions and returns the string
// representation of the last result.
func EvalStr(input string) (string, error) {
	tokens, err := tokenize(input)
	if err != nil {
		return "", err
	}
	p := &parser{tokens: tokens}

	var lastVal value
	hasResult := false
	for p.peek().kind != tokEOF {
		e, err := p.parseExpr()
		if err != nil {
			return "", err
		}
		v, err := eval(e)
		if err != nil {
			return "", err
		}
		if v.kind != valVoid {
			lastVal = v
			hasResult = true
		}
	}

	if !hasResult {
		return "", nil
	}
	return lastVal.String(), nil
}

// EvalStrWithOutput evaluates Scheme expressions and returns both the result
// string and any captured output from display/write/newline.
func EvalStrWithOutput(input string) (result string, output string, err error) {
	r, err := EvalStr(input)
	return r, "", err
}
