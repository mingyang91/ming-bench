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
	valLambda
	valChar
)

type Value struct {
	typ    valueType
	ival   int64
	bval   bool
	sval   string
	car    *Value
	cdr    *Value
	// lambda fields
	params []string
	body   []*astNode
	closure *env
}

func intVal(n int64) *Value   { return &Value{typ: valInt, ival: n} }
func boolVal(b bool) *Value   { return &Value{typ: valBool, bval: b} }
func strVal(s string) *Value  { return &Value{typ: valString, sval: s} }
func symVal(s string) *Value  { return &Value{typ: valSymbol, sval: s} }
func charVal(c rune) *Value   { return &Value{typ: valChar, ival: int64(c)} }
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
	case valChar:
		ch := rune(v.ival)
		switch ch {
		case ' ':
			return `#\space`
		case '\n':
			return `#\newline`
		case '\t':
			return `#\tab`
		default:
			return `#\` + string(ch)
		}
	case valLambda:
		return "#<procedure>"
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

// displayStr returns the display representation (no quotes on strings).
func (v *Value) displayStr() string {
	switch v.typ {
	case valString:
		return v.sval
	case valPair:
		return "(" + pairDisplayStr(v) + ")"
	default:
		return v.String()
	}
}

func pairDisplayStr(v *Value) string {
	var parts []string
	for v.typ == valPair {
		parts = append(parts, v.car.displayStr())
		v = v.cdr
	}
	if v.typ == valNil {
		return strings.Join(parts, " ")
	}
	return strings.Join(parts, " ") + " . " + v.displayStr()
}

func isTruthy(v *Value) bool {
	return !(v.typ == valBool && !v.bval)
}

// interp holds interpreter state including output buffer.
type interp struct {
	output strings.Builder
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
	tokQuote
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
	case ch == '\'':
		l.advance()
		return token{kind: tokQuote, line: line, col: col}, nil
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
	case tokQuote:
		inner, err := p.parseExpr()
		if err != nil {
			return nil, err
		}
		if inner == nil {
			return nil, &EvalError{Message: fmt.Sprintf("%d:%d: unexpected end of input after quote", t.line, t.col)}
		}
		return &astNode{
			children: []*astNode{
				{isAtom: true, tok: token{kind: tokSymbol, sval: "quote", line: t.line, col: t.col}, line: t.line, col: t.col},
				inner,
			},
			line: t.line, col: t.col,
		}, nil
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

func eval(node *astNode, e *env, ip *interp) (*Value, error) {
	if node.isAtom {
		return evalAtom(node, e)
	}
	return evalList(node, e, ip)
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

func evalList(node *astNode, e *env, ip *interp) (*Value, error) {
	if len(node.children) == 0 {
		return nilVal(), nil
	}

	first := node.children[0]
	// Special forms
	if first.isAtom && first.tok.kind == tokSymbol {
		switch first.tok.sval {
		case "and":
			return evalAnd(node, e, ip)
		case "or":
			return evalOr(node, e, ip)
		case "define":
			return evalDefine(node, e, ip)
		case "if":
			return evalIf(node, e, ip)
		case "quote":
			if len(node.children) != 2 {
				return nil, &EvalError{Message: fmt.Sprintf("%d:%d: quote: need 1 argument", node.line, node.col)}
			}
			return quoteNode(node.children[1]), nil
		case "lambda":
			return evalLambda(node, e)
		case "let":
			return evalLet(node, e, ip)
		case "begin":
			return evalBegin(node, e, ip)
		case "cond":
			return evalCond(node, e, ip)
		}
	}

	// Evaluate operator
	op, err := eval(first, e, ip)
	if err != nil {
		return nil, err
	}

	// Evaluate arguments
	args := make([]*Value, 0, len(node.children)-1)
	for _, child := range node.children[1:] {
		v, err := eval(child, e, ip)
		if err != nil {
			return nil, err
		}
		args = append(args, v)
	}

	// Built-in functions (symbol-based dispatch via the value)
	if op.typ == valSymbol {
		return applyBuiltin(op.sval, args, node, ip)
	}

	// Lambda application
	if op.typ == valLambda {
		if len(args) != len(op.params) {
			return nil, &EvalError{Message: fmt.Sprintf("%d:%d: wrong number of arguments: expected %d, got %d", node.line, node.col, len(op.params), len(args))}
		}
		localEnv := newEnv(op.closure)
		for i, param := range op.params {
			localEnv.set(param, args[i])
		}
		var result *Value
		for _, bodyExpr := range op.body {
			var err error
			result, err = eval(bodyExpr, localEnv, ip)
			if err != nil {
				return nil, err
			}
		}
		return result, nil
	}

	return nil, &EvalError{Message: fmt.Sprintf("%d:%d: not a procedure", node.line, node.col)}
}

func evalAnd(node *astNode, e *env, ip *interp) (*Value, error) {
	if len(node.children) == 1 {
		return boolVal(true), nil
	}
	var result *Value
	for _, child := range node.children[1:] {
		v, err := eval(child, e, ip)
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

func evalOr(node *astNode, e *env, ip *interp) (*Value, error) {
	if len(node.children) == 1 {
		return boolVal(false), nil
	}
	var result *Value
	for _, child := range node.children[1:] {
		v, err := eval(child, e, ip)
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

func evalDefine(node *astNode, e *env, ip *interp) (*Value, error) {
	if len(node.children) < 3 {
		return nil, &EvalError{Message: fmt.Sprintf("%d:%d: define: bad syntax", node.line, node.col)}
	}
	target := node.children[1]
	if target.isAtom && target.tok.kind == tokSymbol {
		// (define x expr)
		val, err := eval(node.children[2], e, ip)
		if err != nil {
			return nil, err
		}
		e.set(target.tok.sval, val)
		return voidVal(), nil
	}
	// (define (f params...) body...)
	if !target.isAtom && len(target.children) >= 1 {
		name := target.children[0]
		if !name.isAtom || name.tok.kind != tokSymbol {
			return nil, &EvalError{Message: fmt.Sprintf("%d:%d: define: bad syntax", node.line, node.col)}
		}
		params := make([]string, 0, len(target.children)-1)
		for _, p := range target.children[1:] {
			if !p.isAtom || p.tok.kind != tokSymbol {
				return nil, &EvalError{Message: fmt.Sprintf("%d:%d: define: bad parameter", node.line, node.col)}
			}
			params = append(params, p.tok.sval)
		}
		lam := &Value{typ: valLambda, params: params, body: node.children[2:], closure: e}
		e.set(name.tok.sval, lam)
		return voidVal(), nil
	}
	return nil, &EvalError{Message: fmt.Sprintf("%d:%d: define: bad syntax", node.line, node.col)}
}

func evalIf(node *astNode, e *env, ip *interp) (*Value, error) {
	if len(node.children) < 3 || len(node.children) > 4 {
		return nil, &EvalError{Message: fmt.Sprintf("%d:%d: if: bad syntax", node.line, node.col)}
	}
	cond, err := eval(node.children[1], e, ip)
	if err != nil {
		return nil, err
	}
	if isTruthy(cond) {
		return eval(node.children[2], e, ip)
	}
	if len(node.children) == 4 {
		return eval(node.children[3], e, ip)
	}
	return voidVal(), nil
}

func evalLambda(node *astNode, e *env) (*Value, error) {
	if len(node.children) < 3 {
		return nil, &EvalError{Message: fmt.Sprintf("%d:%d: lambda: bad syntax", node.line, node.col)}
	}
	paramNode := node.children[1]
	if paramNode.isAtom {
		return nil, &EvalError{Message: fmt.Sprintf("%d:%d: lambda: bad parameter list", node.line, node.col)}
	}
	params := make([]string, 0, len(paramNode.children))
	for _, p := range paramNode.children {
		if !p.isAtom || p.tok.kind != tokSymbol {
			return nil, &EvalError{Message: fmt.Sprintf("%d:%d: lambda: bad parameter", node.line, node.col)}
		}
		params = append(params, p.tok.sval)
	}
	return &Value{typ: valLambda, params: params, body: node.children[2:], closure: e}, nil
}

func evalLet(node *astNode, e *env, ip *interp) (*Value, error) {
	if len(node.children) < 3 {
		return nil, &EvalError{Message: fmt.Sprintf("%d:%d: let: bad syntax", node.line, node.col)}
	}

	// Named let: (let name ((var init) ...) body ...)
	offset := 1
	var loopName string
	if node.children[1].isAtom && node.children[1].tok.kind == tokSymbol {
		if len(node.children) < 4 {
			return nil, &EvalError{Message: fmt.Sprintf("%d:%d: let: bad syntax", node.line, node.col)}
		}
		loopName = node.children[1].tok.sval
		offset = 2
	}

	bindings := node.children[offset]
	if bindings.isAtom {
		return nil, &EvalError{Message: fmt.Sprintf("%d:%d: let: bad syntax", node.line, node.col)}
	}

	params := make([]string, 0, len(bindings.children))
	initVals := make([]*Value, 0, len(bindings.children))
	for _, b := range bindings.children {
		if b.isAtom || len(b.children) != 2 {
			return nil, &EvalError{Message: fmt.Sprintf("%d:%d: let: bad binding", node.line, node.col)}
		}
		name := b.children[0]
		if !name.isAtom || name.tok.kind != tokSymbol {
			return nil, &EvalError{Message: fmt.Sprintf("%d:%d: let: bad binding", node.line, node.col)}
		}
		val, err := eval(b.children[1], e, ip)
		if err != nil {
			return nil, err
		}
		params = append(params, name.tok.sval)
		initVals = append(initVals, val)
	}

	if loopName != "" {
		// Create a lambda for the loop and bind it, then call with init vals
		localEnv := newEnv(e)
		lam := &Value{typ: valLambda, params: params, body: node.children[offset+1:], closure: localEnv}
		localEnv.set(loopName, lam)
		// Call with initial values
		callEnv := newEnv(localEnv)
		for i, p := range params {
			callEnv.set(p, initVals[i])
		}
		var result *Value
		for _, bodyExpr := range node.children[offset+1:] {
			var err error
			result, err = eval(bodyExpr, callEnv, ip)
			if err != nil {
				return nil, err
			}
		}
		return result, nil
	}

	localEnv := newEnv(e)
	for i, p := range params {
		localEnv.set(p, initVals[i])
	}
	var result *Value
	for _, bodyExpr := range node.children[offset+1:] {
		var err error
		result, err = eval(bodyExpr, localEnv, ip)
		if err != nil {
			return nil, err
		}
	}
	return result, nil
}

func evalBegin(node *astNode, e *env, ip *interp) (*Value, error) {
	if len(node.children) < 2 {
		return voidVal(), nil
	}
	var result *Value
	for _, child := range node.children[1:] {
		var err error
		result, err = eval(child, e, ip)
		if err != nil {
			return nil, err
		}
	}
	return result, nil
}

func evalCond(node *astNode, e *env, ip *interp) (*Value, error) {
	for _, clause := range node.children[1:] {
		if clause.isAtom || len(clause.children) < 2 {
			return nil, &EvalError{Message: fmt.Sprintf("%d:%d: cond: bad clause", node.line, node.col)}
		}
		test := clause.children[0]
		if test.isAtom && test.tok.kind == tokSymbol && test.tok.sval == "else" {
			var result *Value
			for _, expr := range clause.children[1:] {
				var err error
				result, err = eval(expr, e, ip)
				if err != nil {
					return nil, err
				}
			}
			return result, nil
		}
		cond, err := eval(test, e, ip)
		if err != nil {
			return nil, err
		}
		if isTruthy(cond) {
			var result *Value
			for _, expr := range clause.children[1:] {
				result, err = eval(expr, e, ip)
				if err != nil {
					return nil, err
				}
			}
			return result, nil
		}
	}
	return voidVal(), nil
}

func quoteNode(node *astNode) *Value {
	if node.isAtom {
		switch node.tok.kind {
		case tokNumber:
			return intVal(node.tok.ival)
		case tokBool:
			return boolVal(node.tok.bval)
		case tokString:
			return strVal(node.tok.sval)
		case tokSymbol:
			return symVal(node.tok.sval)
		}
	}
	// List
	if len(node.children) == 0 {
		return nilVal()
	}
	result := nilVal()
	for i := len(node.children) - 1; i >= 0; i-- {
		result = &Value{typ: valPair, car: quoteNode(node.children[i]), cdr: result}
	}
	return result
}

func requireInts(args []*Value, name string, node *astNode) error {
	for _, a := range args {
		if a.typ != valInt {
			return &EvalError{Message: fmt.Sprintf("%d:%d: %s: expected number", node.line, node.col, name)}
		}
	}
	return nil
}

func applyBuiltin(name string, args []*Value, node *astNode, ip *interp) (*Value, error) {
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

	case "cons":
		if len(args) != 2 {
			return nil, &EvalError{Message: fmt.Sprintf("%d:%d: cons: need 2 arguments", node.line, node.col)}
		}
		return &Value{typ: valPair, car: args[0], cdr: args[1]}, nil

	case "car":
		if len(args) != 1 {
			return nil, &EvalError{Message: fmt.Sprintf("%d:%d: car: need 1 argument", node.line, node.col)}
		}
		if args[0].typ != valPair {
			return nil, &EvalError{Message: fmt.Sprintf("%d:%d: car: expected pair", node.line, node.col)}
		}
		return args[0].car, nil

	case "cdr":
		if len(args) != 1 {
			return nil, &EvalError{Message: fmt.Sprintf("%d:%d: cdr: need 1 argument", node.line, node.col)}
		}
		if args[0].typ != valPair {
			return nil, &EvalError{Message: fmt.Sprintf("%d:%d: cdr: expected pair", node.line, node.col)}
		}
		return args[0].cdr, nil

	case "null?":
		if len(args) != 1 {
			return nil, &EvalError{Message: fmt.Sprintf("%d:%d: null?: need 1 argument", node.line, node.col)}
		}
		return boolVal(args[0].typ == valNil), nil

	case "list":
		result := nilVal()
		for i := len(args) - 1; i >= 0; i-- {
			result = &Value{typ: valPair, car: args[i], cdr: result}
		}
		return result, nil

	case "length":
		if len(args) != 1 {
			return nil, &EvalError{Message: fmt.Sprintf("%d:%d: length: need 1 argument", node.line, node.col)}
		}
		count := int64(0)
		v := args[0]
		for v.typ == valPair {
			count++
			v = v.cdr
		}
		if v.typ != valNil {
			return nil, &EvalError{Message: fmt.Sprintf("%d:%d: length: not a proper list", node.line, node.col)}
		}
		return intVal(count), nil

	case "append":
		if len(args) == 0 {
			return nilVal(), nil
		}
		// Append all lists together
		var parts []*Value
		for i := 0; i < len(args)-1; i++ {
			v := args[i]
			for v.typ == valPair {
				parts = append(parts, v.car)
				v = v.cdr
			}
			if v.typ != valNil {
				return nil, &EvalError{Message: fmt.Sprintf("%d:%d: append: not a proper list", node.line, node.col)}
			}
		}
		result := args[len(args)-1]
		for i := len(parts) - 1; i >= 0; i-- {
			result = &Value{typ: valPair, car: parts[i], cdr: result}
		}
		return result, nil

	case "number?":
		if len(args) != 1 {
			return nil, &EvalError{Message: fmt.Sprintf("%d:%d: number?: need 1 argument", node.line, node.col)}
		}
		return boolVal(args[0].typ == valInt), nil

	case "string?":
		if len(args) != 1 {
			return nil, &EvalError{Message: fmt.Sprintf("%d:%d: string?: need 1 argument", node.line, node.col)}
		}
		return boolVal(args[0].typ == valString), nil

	case "boolean?":
		if len(args) != 1 {
			return nil, &EvalError{Message: fmt.Sprintf("%d:%d: boolean?: need 1 argument", node.line, node.col)}
		}
		return boolVal(args[0].typ == valBool), nil

	case "pair?":
		if len(args) != 1 {
			return nil, &EvalError{Message: fmt.Sprintf("%d:%d: pair?: need 1 argument", node.line, node.col)}
		}
		return boolVal(args[0].typ == valPair), nil

	case "symbol?":
		if len(args) != 1 {
			return nil, &EvalError{Message: fmt.Sprintf("%d:%d: symbol?: need 1 argument", node.line, node.col)}
		}
		return boolVal(args[0].typ == valSymbol), nil

	case "char?":
		if len(args) != 1 {
			return nil, &EvalError{Message: fmt.Sprintf("%d:%d: char?: need 1 argument", node.line, node.col)}
		}
		return boolVal(args[0].typ == valChar), nil

	case "display":
		if len(args) != 1 {
			return nil, &EvalError{Message: fmt.Sprintf("%d:%d: display: need 1 argument", node.line, node.col)}
		}
		ip.output.WriteString(args[0].displayStr())
		return voidVal(), nil

	case "write":
		if len(args) != 1 {
			return nil, &EvalError{Message: fmt.Sprintf("%d:%d: write: need 1 argument", node.line, node.col)}
		}
		ip.output.WriteString(args[0].String())
		return voidVal(), nil

	case "newline":
		if len(args) != 0 {
			return nil, &EvalError{Message: fmt.Sprintf("%d:%d: newline: need 0 arguments", node.line, node.col)}
		}
		ip.output.WriteString("\n")
		return voidVal(), nil

	case "string-append":
		var buf strings.Builder
		for _, a := range args {
			if a.typ != valString {
				return nil, &EvalError{Message: fmt.Sprintf("%d:%d: string-append: expected string", node.line, node.col)}
			}
			buf.WriteString(a.sval)
		}
		return strVal(buf.String()), nil

	case "string-length":
		if len(args) != 1 {
			return nil, &EvalError{Message: fmt.Sprintf("%d:%d: string-length: need 1 argument", node.line, node.col)}
		}
		if args[0].typ != valString {
			return nil, &EvalError{Message: fmt.Sprintf("%d:%d: string-length: expected string", node.line, node.col)}
		}
		return intVal(int64(len([]rune(args[0].sval)))), nil

	case "substring":
		if len(args) != 3 {
			return nil, &EvalError{Message: fmt.Sprintf("%d:%d: substring: need 3 arguments", node.line, node.col)}
		}
		if args[0].typ != valString || args[1].typ != valInt || args[2].typ != valInt {
			return nil, &EvalError{Message: fmt.Sprintf("%d:%d: substring: bad arguments", node.line, node.col)}
		}
		runes := []rune(args[0].sval)
		start, end := int(args[1].ival), int(args[2].ival)
		if start < 0 || end < start || end > len(runes) {
			return nil, &EvalError{Message: fmt.Sprintf("%d:%d: substring: index out of range", node.line, node.col)}
		}
		return strVal(string(runes[start:end])), nil

	case "string->number":
		if len(args) != 1 {
			return nil, &EvalError{Message: fmt.Sprintf("%d:%d: string->number: need 1 argument", node.line, node.col)}
		}
		if args[0].typ != valString {
			return nil, &EvalError{Message: fmt.Sprintf("%d:%d: string->number: expected string", node.line, node.col)}
		}
		n, err := strconv.ParseInt(args[0].sval, 10, 64)
		if err != nil {
			return boolVal(false), nil
		}
		return intVal(n), nil

	case "number->string":
		if len(args) != 1 {
			return nil, &EvalError{Message: fmt.Sprintf("%d:%d: number->string: need 1 argument", node.line, node.col)}
		}
		if args[0].typ != valInt {
			return nil, &EvalError{Message: fmt.Sprintf("%d:%d: number->string: expected number", node.line, node.col)}
		}
		return strVal(strconv.FormatInt(args[0].ival, 10)), nil

	case "symbol->string":
		if len(args) != 1 {
			return nil, &EvalError{Message: fmt.Sprintf("%d:%d: symbol->string: need 1 argument", node.line, node.col)}
		}
		if args[0].typ != valSymbol {
			return nil, &EvalError{Message: fmt.Sprintf("%d:%d: symbol->string: expected symbol", node.line, node.col)}
		}
		return strVal(args[0].sval), nil

	case "string->symbol":
		if len(args) != 1 {
			return nil, &EvalError{Message: fmt.Sprintf("%d:%d: string->symbol: need 1 argument", node.line, node.col)}
		}
		if args[0].typ != valString {
			return nil, &EvalError{Message: fmt.Sprintf("%d:%d: string->symbol: expected string", node.line, node.col)}
		}
		return symVal(args[0].sval), nil

	case "string-ref":
		if len(args) != 2 {
			return nil, &EvalError{Message: fmt.Sprintf("%d:%d: string-ref: need 2 arguments", node.line, node.col)}
		}
		if args[0].typ != valString || args[1].typ != valInt {
			return nil, &EvalError{Message: fmt.Sprintf("%d:%d: string-ref: bad arguments", node.line, node.col)}
		}
		runes := []rune(args[0].sval)
		idx := int(args[1].ival)
		if idx < 0 || idx >= len(runes) {
			return nil, &EvalError{Message: fmt.Sprintf("%d:%d: string-ref: index out of range", node.line, node.col)}
		}
		return charVal(runes[idx]), nil

	default:
		return nil, &EvalError{Message: fmt.Sprintf("%d:%d: unbound variable: %s", node.line, node.col, name)}
	}
}

func makeGlobalEnv() *env {
	e := newEnv(nil)
	builtins := []string{"+", "-", "*", "/", "<", ">", "=", "<=", ">=", "not",
		"cons", "car", "cdr", "null?", "list", "length",
		"number?", "string?", "boolean?", "pair?", "symbol?", "char?",
		"append",
		"display", "write", "newline",
		"string-append", "string-length", "substring",
		"string->number", "number->string",
		"symbol->string", "string->symbol",
		"string-ref"}
	for _, name := range builtins {
		e.set(name, symVal(name))
	}
	return e
}

func evalAll(input string) (result string, output string, err error) {
	p := newParser(input)
	nodes, err := p.parseAll()
	if err != nil {
		return "", "", err
	}
	if len(nodes) == 0 {
		return "", "", &EvalError{Message: "empty input"}
	}

	e := makeGlobalEnv()
	ip := &interp{}
	var last *Value
	for _, node := range nodes {
		v, err := eval(node, e, ip)
		if err != nil {
			return "", "", err
		}
		last = v
	}
	return last.String(), ip.output.String(), nil
}

// EvalStr evaluates one or more Scheme expressions and returns the string
// representation of the last result.
func EvalStr(input string) (string, error) {
	r, _, err := evalAll(input)
	return r, err
}

// EvalStrWithOutput evaluates Scheme expressions and returns both the result
// string and any captured output from display/write/newline.
func EvalStrWithOutput(input string) (result string, output string, err error) {
	return evalAll(input)
}
