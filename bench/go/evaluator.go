package ming

import (
	"fmt"
	"strconv"
	"strings"
	"unicode"
)

// ---------- Value types ----------

type valueType int

const (
	valInt valueType = iota
	valBool
	valString
	valSymbol
	valPair
	valNil // empty list
	valVoid
)

type Value struct {
	typ    valueType
	ival   int64
	bval   bool
	sval   string
	car    *Value
	cdr    *Value
}

func intVal(n int64) *Value   { return &Value{typ: valInt, ival: n} }
func boolVal(b bool) *Value   { return &Value{typ: valBool, bval: b} }
func strVal(s string) *Value  { return &Value{typ: valString, sval: s} }
func symVal(s string) *Value  { return &Value{typ: valSymbol, sval: s} }
func nilVal() *Value          { return &Value{typ: valNil} }
func voidVal() *Value         { return &Value{typ: valVoid} }

func (v *Value) String() string {
	switch v.typ {
	case valInt:
		return strconv.FormatInt(v.ival, 10)
	case valBool:
		if v.bval {
			return "#t"
		}
		return "#f"
	case valString:
		return `"` + v.sval + `"`
	case valSymbol:
		return v.sval
	case valNil:
		return "()"
	case valPair:
		return "(" + pairStr(v) + ")"
	case valVoid:
		return ""
	default:
		return "<unknown>"
	}
}

func pairStr(v *Value) string {
	var parts []string
	for v.typ == valPair {
		parts = append(parts, v.car.String())
		v = v.cdr
	}
	if v.typ == valNil {
		return strings.Join(parts, " ")
	}
	return strings.Join(parts, " ") + " . " + v.String()
}

func isTruthy(v *Value) bool {
	return !(v.typ == valBool && !v.bval)
}

// ---------- Tokenizer ----------

type tokenKind int

const (
	tokLParen tokenKind = iota
	tokRParen
	tokNumber
	tokString
	tokBool
	tokSymbol
	tokEOF
)

type token struct {
	kind tokenKind
	sval string
	ival int64
	bval bool
	line int
	col  int
}

type lexer struct {
	input []rune
	pos   int
	line  int
	col   int
}

func newLexer(input string) *lexer {
	return &lexer{input: []rune(input), pos: 0, line: 1, col: 1}
}

func (l *lexer) peek() rune {
	if l.pos >= len(l.input) {
		return 0
	}
	return l.input[l.pos]
}

func (l *lexer) advance() rune {
	ch := l.input[l.pos]
	l.pos++
	if ch == '\n' {
		l.line++
		l.col = 1
	} else {
		l.col++
	}
	return ch
}

func (l *lexer) skipWhitespaceAndComments() {
	for l.pos < len(l.input) {
		ch := l.peek()
		if unicode.IsSpace(ch) {
			l.advance()
		} else if ch == ';' {
			for l.pos < len(l.input) && l.peek() != '\n' {
				l.advance()
			}
		} else {
			break
		}
	}
}

func isDelimiter(ch rune) bool {
	return ch == 0 || ch == '(' || ch == ')' || unicode.IsSpace(ch) || ch == ';' || ch == '"'
}

func (l *lexer) nextToken() (token, error) {
	l.skipWhitespaceAndComments()
	if l.pos >= len(l.input) {
		return token{kind: tokEOF, line: l.line, col: l.col}, nil
	}

	line, col := l.line, l.col
	ch := l.peek()

	switch {
	case ch == '(':
		l.advance()
		return token{kind: tokLParen, line: line, col: col}, nil
	case ch == ')':
		l.advance()
		return token{kind: tokRParen, line: line, col: col}, nil
	case ch == '"':
		return l.readString(line, col)
	case ch == '#':
		l.advance()
		next := l.peek()
		if next == 't' {
			l.advance()
			return token{kind: tokBool, bval: true, line: line, col: col}, nil
		} else if next == 'f' {
			l.advance()
			return token{kind: tokBool, bval: false, line: line, col: col}, nil
		}
		return token{}, &EvalError{Message: fmt.Sprintf("%d:%d: unexpected character after #", line, col)}
	default:
		return l.readAtom(line, col)
	}
}

func (l *lexer) readString(line, col int) (token, error) {
	l.advance() // opening "
	var buf []rune
	for l.pos < len(l.input) {
		ch := l.advance()
		if ch == '"' {
			return token{kind: tokString, sval: string(buf), line: line, col: col}, nil
		}
		if ch == '\\' {
			if l.pos >= len(l.input) {
				return token{}, &EvalError{Message: fmt.Sprintf("%d:%d: unterminated string escape", line, col)}
			}
			esc := l.advance()
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
	return token{}, &EvalError{Message: fmt.Sprintf("%d:%d: unterminated string", line, col)}
}

func (l *lexer) readAtom(line, col int) (token, error) {
	var buf []rune
	for l.pos < len(l.input) && !isDelimiter(l.peek()) {
		buf = append(buf, l.advance())
	}
	s := string(buf)

	// Try parsing as integer
	if n, err := strconv.ParseInt(s, 10, 64); err == nil {
		return token{kind: tokNumber, ival: n, line: line, col: col}, nil
	}

	return token{kind: tokSymbol, sval: s, line: line, col: col}, nil
}

// ---------- Parser ----------

type parser struct {
	lex    *lexer
	peeked *token
}

func newParser(input string) *parser {
	return &parser{lex: newLexer(input)}
}

func (p *parser) peek() (token, error) {
	if p.peeked != nil {
		return *p.peeked, nil
	}
	t, err := p.lex.nextToken()
	if err != nil {
		return token{}, err
	}
	p.peeked = &t
	return t, nil
}

func (p *parser) next() (token, error) {
	if p.peeked != nil {
		t := *p.peeked
		p.peeked = nil
		return t, nil
	}
	return p.lex.nextToken()
}

type astNode struct {
	// atom
	isAtom bool
	tok    token
	// list
	children []*astNode
	line     int
	col      int
}

func (p *parser) parseExpr() (*astNode, error) {
	t, err := p.next()
	if err != nil {
		return nil, err
	}

	switch t.kind {
	case tokLParen:
		node := &astNode{line: t.line, col: t.col}
		for {
			pk, err := p.peek()
			if err != nil {
				return nil, err
			}
			if pk.kind == tokRParen {
				p.next()
				break
			}
			if pk.kind == tokEOF {
				return nil, &EvalError{Message: fmt.Sprintf("%d:%d: unexpected end of input", t.line, t.col)}
			}
			child, err := p.parseExpr()
			if err != nil {
				return nil, err
			}
			node.children = append(node.children, child)
		}
		return node, nil
	case tokRParen:
		return nil, &EvalError{Message: fmt.Sprintf("%d:%d: unexpected ')'", t.line, t.col)}
	case tokEOF:
		return nil, nil
	default:
		return &astNode{isAtom: true, tok: t, line: t.line, col: t.col}, nil
	}
}

func (p *parser) parseAll() ([]*astNode, error) {
	var nodes []*astNode
	for {
		node, err := p.parseExpr()
		if err != nil {
			return nil, err
		}
		if node == nil {
			break
		}
		nodes = append(nodes, node)
	}
	return nodes, nil
}

// ---------- Evaluator ----------

type env struct {
	vars   map[string]*Value
	parent *env
}

func newEnv(parent *env) *env {
	return &env{vars: make(map[string]*Value), parent: parent}
}

func (e *env) get(name string) (*Value, bool) {
	if v, ok := e.vars[name]; ok {
		return v, true
	}
	if e.parent != nil {
		return e.parent.get(name)
	}
	return nil, false
}

func (e *env) set(name string, v *Value) {
	e.vars[name] = v
}

func eval(node *astNode, e *env) (*Value, error) {
	if node.isAtom {
		return evalAtom(node, e)
	}
	return evalList(node, e)
}

func evalAtom(node *astNode, e *env) (*Value, error) {
	t := node.tok
	switch t.kind {
	case tokNumber:
		return intVal(t.ival), nil
	case tokBool:
		return boolVal(t.bval), nil
	case tokString:
		return strVal(t.sval), nil
	case tokSymbol:
		if v, ok := e.get(t.sval); ok {
			return v, nil
		}
		return nil, &EvalError{Message: fmt.Sprintf("%d:%d: unbound variable: %s", t.line, t.col, t.sval)}
	}
	return nil, &EvalError{Message: fmt.Sprintf("%d:%d: unexpected token", t.line, t.col)}
}

func evalList(node *astNode, e *env) (*Value, error) {
	if len(node.children) == 0 {
		return nilVal(), nil
	}

	first := node.children[0]
	// Special forms
	if first.isAtom && first.tok.kind == tokSymbol {
		switch first.tok.sval {
		case "and":
			return evalAnd(node, e)
		case "or":
			return evalOr(node, e)
		}
	}

	// Evaluate operator
	op, err := eval(first, e)
	if err != nil {
		return nil, err
	}

	// Evaluate arguments
	args := make([]*Value, 0, len(node.children)-1)
	for _, child := range node.children[1:] {
		v, err := eval(child, e)
		if err != nil {
			return nil, err
		}
		args = append(args, v)
	}

	// Built-in functions (symbol-based dispatch via the value)
	if op.typ == valSymbol {
		return applyBuiltin(op.sval, args, node)
	}

	return nil, &EvalError{Message: fmt.Sprintf("%d:%d: not a procedure", node.line, node.col)}
}

func evalAnd(node *astNode, e *env) (*Value, error) {
	if len(node.children) == 1 {
		return boolVal(true), nil
	}
	var result *Value
	for _, child := range node.children[1:] {
		v, err := eval(child, e)
		if err != nil {
			return nil, err
		}
		result = v
		if !isTruthy(v) {
			return v, nil
		}
	}
	return result, nil
}

func evalOr(node *astNode, e *env) (*Value, error) {
	if len(node.children) == 1 {
		return boolVal(false), nil
	}
	var result *Value
	for _, child := range node.children[1:] {
		v, err := eval(child, e)
		if err != nil {
			return nil, err
		}
		result = v
		if isTruthy(v) {
			return v, nil
		}
	}
	return result, nil
}

func requireInts(args []*Value, name string, node *astNode) error {
	for _, a := range args {
		if a.typ != valInt {
			return &EvalError{Message: fmt.Sprintf("%d:%d: %s: expected number", node.line, node.col, name)}
		}
	}
	return nil
}

func applyBuiltin(name string, args []*Value, node *astNode) (*Value, error) {
	switch name {
	case "+":
		if err := requireInts(args, "+", node); err != nil {
			return nil, err
		}
		sum := int64(0)
		for _, a := range args {
			sum += a.ival
		}
		return intVal(sum), nil

	case "-":
		if len(args) == 0 {
			return nil, &EvalError{Message: fmt.Sprintf("%d:%d: -: need at least 1 argument", node.line, node.col)}
		}
		if err := requireInts(args, "-", node); err != nil {
			return nil, err
		}
		if len(args) == 1 {
			return intVal(-args[0].ival), nil
		}
		result := args[0].ival
		for _, a := range args[1:] {
			result -= a.ival
		}
		return intVal(result), nil

	case "*":
		if err := requireInts(args, "*", node); err != nil {
			return nil, err
		}
		product := int64(1)
		for _, a := range args {
			product *= a.ival
		}
		return intVal(product), nil

	case "/":
		if len(args) < 2 {
			return nil, &EvalError{Message: fmt.Sprintf("%d:%d: /: need at least 2 arguments", node.line, node.col)}
		}
		if err := requireInts(args, "/", node); err != nil {
			return nil, err
		}
		result := args[0].ival
		for _, a := range args[1:] {
			if a.ival == 0 {
				return nil, &EvalError{Message: fmt.Sprintf("%d:%d: /: division by zero", node.line, node.col)}
			}
			result /= a.ival
		}
		return intVal(result), nil

	case "<":
		if len(args) != 2 {
			return nil, &EvalError{Message: fmt.Sprintf("%d:%d: <: need 2 arguments", node.line, node.col)}
		}
		if err := requireInts(args, "<", node); err != nil {
			return nil, err
		}
		return boolVal(args[0].ival < args[1].ival), nil

	case ">":
		if len(args) != 2 {
			return nil, &EvalError{Message: fmt.Sprintf("%d:%d: >: need 2 arguments", node.line, node.col)}
		}
		if err := requireInts(args, ">", node); err != nil {
			return nil, err
		}
		return boolVal(args[0].ival > args[1].ival), nil

	case "=":
		if len(args) != 2 {
			return nil, &EvalError{Message: fmt.Sprintf("%d:%d: =: need 2 arguments", node.line, node.col)}
		}
		if err := requireInts(args, "=", node); err != nil {
			return nil, err
		}
		return boolVal(args[0].ival == args[1].ival), nil

	case "<=":
		if len(args) != 2 {
			return nil, &EvalError{Message: fmt.Sprintf("%d:%d: <=: need 2 arguments", node.line, node.col)}
		}
		if err := requireInts(args, "<=", node); err != nil {
			return nil, err
		}
		return boolVal(args[0].ival <= args[1].ival), nil

	case ">=":
		if len(args) != 2 {
			return nil, &EvalError{Message: fmt.Sprintf("%d:%d: >=: need 2 arguments", node.line, node.col)}
		}
		if err := requireInts(args, ">=", node); err != nil {
			return nil, err
		}
		return boolVal(args[0].ival >= args[1].ival), nil

	case "not":
		if len(args) != 1 {
			return nil, &EvalError{Message: fmt.Sprintf("%d:%d: not: need 1 argument", node.line, node.col)}
		}
		return boolVal(!isTruthy(args[0])), nil

	default:
		return nil, &EvalError{Message: fmt.Sprintf("%d:%d: unbound variable: %s", node.line, node.col, name)}
	}
}

func makeGlobalEnv() *env {
	e := newEnv(nil)
	builtins := []string{"+", "-", "*", "/", "<", ">", "=", "<=", ">=", "not"}
	for _, name := range builtins {
		e.set(name, symVal(name))
	}
	return e
}

// EvalStr evaluates one or more Scheme expressions and returns the string
// representation of the last result.
func EvalStr(input string) (string, error) {
	p := newParser(input)
	nodes, err := p.parseAll()
	if err != nil {
		return "", err
	}
	if len(nodes) == 0 {
		return "", &EvalError{Message: "empty input"}
	}

	e := makeGlobalEnv()
	var last *Value
	for _, node := range nodes {
		v, err := eval(node, e)
		if err != nil {
			return "", err
		}
		last = v
	}
	return last.String(), nil
}

// EvalStrWithOutput evaluates Scheme expressions and returns both the result
// string and any captured output from display/write/newline.
func EvalStrWithOutput(input string) (result string, output string, err error) {
	r, err := EvalStr(input)
	return r, "", err
}
