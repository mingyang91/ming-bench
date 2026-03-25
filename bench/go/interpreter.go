package ming

import (
	"fmt"
	"strconv"
	"strings"
)

type position struct {
	line   int
	column int
}

type expr interface {
	pos() position
}

type intExpr struct {
	value int64
	at    position
}

func (e *intExpr) pos() position { return e.at }

type boolExpr struct {
	value bool
	at    position
}

func (e *boolExpr) pos() position { return e.at }

type stringExpr struct {
	value string
	at    position
}

func (e *stringExpr) pos() position { return e.at }

type symbolExpr struct {
	name string
	at   position
}

func (e *symbolExpr) pos() position { return e.at }

type listExpr struct {
	elements []expr
	at       position
}

func (e *listExpr) pos() position { return e.at }

type tokenKind int

const (
	tokenEOF tokenKind = iota
	tokenLParen
	tokenRParen
	tokenInteger
	tokenBoolean
	tokenString
	tokenSymbol
)

type token struct {
	kind   tokenKind
	text   string
	number int64
	boolean bool
	at     position
}

type lexer struct {
	input  string
	index  int
	line   int
	column int
}

func newLexer(input string) *lexer {
	return &lexer{
		input:  input,
		line:   1,
		column: 1,
	}
}

func (l *lexer) lexAll() ([]token, error) {
	var tokens []token
	for {
		tok, err := l.nextToken()
		if err != nil {
			return nil, err
		}
		tokens = append(tokens, tok)
		if tok.kind == tokenEOF {
			return tokens, nil
		}
	}
}

func (l *lexer) nextToken() (token, error) {
	l.skipIgnored()
	if l.index >= len(l.input) {
		return token{kind: tokenEOF, at: position{line: l.line, column: l.column}}, nil
	}

	start := position{line: l.line, column: l.column}
	switch ch := l.peek(); ch {
	case '(':
		l.advance()
		return token{kind: tokenLParen, text: "(", at: start}, nil
	case ')':
		l.advance()
		return token{kind: tokenRParen, text: ")", at: start}, nil
	case '"':
		return l.readString(start)
	default:
		return l.readAtom(start)
	}
}

func (l *lexer) skipIgnored() {
	for {
		for l.index < len(l.input) {
			ch := l.peek()
			if ch == ' ' || ch == '\t' || ch == '\r' || ch == '\n' {
				l.advance()
				continue
			}
			break
		}
		if l.index < len(l.input) && l.peek() == ';' {
			for l.index < len(l.input) && l.peek() != '\n' {
				l.advance()
			}
			continue
		}
		return
	}
}

func (l *lexer) readString(start position) (token, error) {
	var raw strings.Builder
	raw.WriteByte(l.advance())

	escaped := false
	for l.index < len(l.input) {
		ch := l.advance()
		raw.WriteByte(ch)

		if escaped {
			escaped = false
			continue
		}
		if ch == '\\' {
			escaped = true
			continue
		}
		if ch == '"' {
			unquoted, err := strconv.Unquote(raw.String())
			if err != nil {
				return token{}, newEvalError(ErrSyntax, "invalid string literal", start)
			}
			return token{kind: tokenString, text: unquoted, at: start}, nil
		}
	}

	return token{}, newEvalError(ErrSyntax, "unterminated string literal", start)
}

func (l *lexer) readAtom(start position) (token, error) {
	begin := l.index
	for l.index < len(l.input) {
		ch := l.peek()
		if isDelimiter(ch) {
			break
		}
		l.advance()
	}

	text := l.input[begin:l.index]
	switch text {
	case "#t":
		return token{kind: tokenBoolean, boolean: true, text: text, at: start}, nil
	case "#f":
		return token{kind: tokenBoolean, boolean: false, text: text, at: start}, nil
	}

	if isIntegerLiteral(text) {
		value, err := strconv.ParseInt(text, 10, 64)
		if err != nil {
			return token{}, newEvalError(ErrSyntax, fmt.Sprintf("invalid integer literal: %s", text), start)
		}
		return token{kind: tokenInteger, number: value, text: text, at: start}, nil
	}

	if text == "" {
		return token{}, newEvalError(ErrSyntax, "unexpected token", start)
	}

	return token{kind: tokenSymbol, text: text, at: start}, nil
}

func (l *lexer) peek() byte {
	return l.input[l.index]
}

func (l *lexer) advance() byte {
	ch := l.input[l.index]
	l.index++
	if ch == '\n' {
		l.line++
		l.column = 1
	} else {
		l.column++
	}
	return ch
}

func isDelimiter(ch byte) bool {
	switch ch {
	case ' ', '\t', '\r', '\n', '(', ')', ';':
		return true
	default:
		return false
	}
}

func isIntegerLiteral(text string) bool {
	if text == "" {
		return false
	}

	if text[0] == '+' || text[0] == '-' {
		if len(text) == 1 {
			return false
		}
		text = text[1:]
	}

	for i := 0; i < len(text); i++ {
		if text[i] < '0' || text[i] > '9' {
			return false
		}
	}
	return true
}

type parser struct {
	tokens []token
	index  int
}

func newParser(tokens []token) *parser {
	return &parser{tokens: tokens}
}

func (p *parser) parseProgram() ([]expr, error) {
	var exprs []expr
	for p.peek().kind != tokenEOF {
		node, err := p.parseExpr()
		if err != nil {
			return nil, err
		}
		exprs = append(exprs, node)
	}
	return exprs, nil
}

func (p *parser) parseExpr() (expr, error) {
	tok := p.peek()
	switch tok.kind {
	case tokenInteger:
		p.index++
		return &intExpr{value: tok.number, at: tok.at}, nil
	case tokenBoolean:
		p.index++
		return &boolExpr{value: tok.boolean, at: tok.at}, nil
	case tokenString:
		p.index++
		return &stringExpr{value: tok.text, at: tok.at}, nil
	case tokenSymbol:
		p.index++
		return &symbolExpr{name: tok.text, at: tok.at}, nil
	case tokenLParen:
		return p.parseList()
	case tokenRParen:
		return nil, newEvalError(ErrSyntax, "unexpected ')'", tok.at)
	case tokenEOF:
		return nil, newEvalError(ErrSyntax, "unexpected end of input", tok.at)
	default:
		return nil, newEvalError(ErrSyntax, "unexpected token", tok.at)
	}
}

func (p *parser) parseList() (expr, error) {
	start := p.peek()
	p.index++

	var elements []expr
	for {
		tok := p.peek()
		switch tok.kind {
		case tokenRParen:
			p.index++
			return &listExpr{elements: elements, at: start.at}, nil
		case tokenEOF:
			return nil, newEvalError(ErrSyntax, "unterminated list", start.at)
		default:
			node, err := p.parseExpr()
			if err != nil {
				return nil, err
			}
			elements = append(elements, node)
		}
	}
}

func (p *parser) peek() token {
	return p.tokens[p.index]
}

type value interface{}

type builtinProc struct {
	name string
	fn   func(args []value, callPos position) (value, error)
}

type interpreter struct {
	env map[string]value
}

func newInterpreter() *interpreter {
	it := &interpreter{
		env: map[string]value{},
	}
	it.installBuiltins()
	return it
}

func (it *interpreter) installBuiltins() {
	it.env["+"] = &builtinProc{name: "+", fn: builtinAdd}
	it.env["-"] = &builtinProc{name: "-", fn: builtinSub}
	it.env["*"] = &builtinProc{name: "*", fn: builtinMul}
	it.env["/"] = &builtinProc{name: "/", fn: builtinDiv}
	it.env["<"] = &builtinProc{name: "<", fn: builtinLessThan}
	it.env[">"] = &builtinProc{name: ">", fn: builtinGreaterThan}
	it.env["="] = &builtinProc{name: "=", fn: builtinEqual}
	it.env["<="] = &builtinProc{name: "<=", fn: builtinLessEqual}
	it.env["not"] = &builtinProc{name: "not", fn: builtinNot}
}

func (it *interpreter) evalProgram(exprs []expr) (value, error) {
	if len(exprs) == 0 {
		return nil, newEvalError(ErrSyntax, "expected expression", position{line: 1, column: 1})
	}

	var result value
	for _, node := range exprs {
		value, err := it.eval(node)
		if err != nil {
			return nil, err
		}
		result = value
	}
	return result, nil
}

func (it *interpreter) eval(node expr) (value, error) {
	switch expr := node.(type) {
	case *intExpr:
		return expr.value, nil
	case *boolExpr:
		return expr.value, nil
	case *stringExpr:
		return expr.value, nil
	case *symbolExpr:
		value, ok := it.env[expr.name]
		if !ok {
			return nil, newEvalError(ErrUnboundVariable, fmt.Sprintf("unbound variable: %s", expr.name), expr.at)
		}
		return value, nil
	case *listExpr:
		return it.evalList(expr)
	default:
		return nil, newEvalError(ErrSyntax, "unknown expression", node.pos())
	}
}

func (it *interpreter) evalList(list *listExpr) (value, error) {
	if len(list.elements) == 0 {
		return nil, newEvalError(ErrSyntax, "cannot evaluate empty list", list.at)
	}

	if sym, ok := list.elements[0].(*symbolExpr); ok {
		switch sym.name {
		case "and":
			return it.evalAnd(list.elements[1:])
		case "or":
			return it.evalOr(list.elements[1:])
		}
	}

	operator, err := it.eval(list.elements[0])
	if err != nil {
		return nil, err
	}

	args := make([]value, 0, len(list.elements)-1)
	for _, argExpr := range list.elements[1:] {
		arg, err := it.eval(argExpr)
		if err != nil {
			return nil, err
		}
		args = append(args, arg)
	}

	proc, ok := operator.(*builtinProc)
	if !ok {
		return nil, newEvalError(ErrNotProcedure, "attempted to call a non-procedure", list.at)
	}

	return proc.fn(args, list.at)
}

func (it *interpreter) evalAnd(args []expr) (value, error) {
	result := value(true)
	for _, arg := range args {
		current, err := it.eval(arg)
		if err != nil {
			return nil, err
		}
		if !isTruthy(current) {
			return current, nil
		}
		result = current
	}
	return result, nil
}

func (it *interpreter) evalOr(args []expr) (value, error) {
	for _, arg := range args {
		current, err := it.eval(arg)
		if err != nil {
			return nil, err
		}
		if isTruthy(current) {
			return current, nil
		}
	}
	return false, nil
}

func isTruthy(v value) bool {
	boolean, ok := v.(bool)
	return !ok || boolean
}

func builtinAdd(args []value, callPos position) (value, error) {
	var total int64
	for _, arg := range args {
		n, err := expectInt(arg, callPos)
		if err != nil {
			return nil, err
		}
		total += n
	}
	return total, nil
}

func builtinSub(args []value, callPos position) (value, error) {
	if len(args) == 0 {
		return nil, wrongArgCount(callPos, "-", "expected at least 1 argument")
	}

	first, err := expectInt(args[0], callPos)
	if err != nil {
		return nil, err
	}
	if len(args) == 1 {
		return -first, nil
	}

	total := first
	for _, arg := range args[1:] {
		n, err := expectInt(arg, callPos)
		if err != nil {
			return nil, err
		}
		total -= n
	}
	return total, nil
}

func builtinMul(args []value, callPos position) (value, error) {
	product := int64(1)
	for _, arg := range args {
		n, err := expectInt(arg, callPos)
		if err != nil {
			return nil, err
		}
		product *= n
	}
	return product, nil
}

func builtinDiv(args []value, callPos position) (value, error) {
	if len(args) < 2 {
		return nil, wrongArgCount(callPos, "/", "expected at least 2 arguments")
	}

	quotient, err := expectInt(args[0], callPos)
	if err != nil {
		return nil, err
	}
	for _, arg := range args[1:] {
		n, err := expectInt(arg, callPos)
		if err != nil {
			return nil, err
		}
		if n == 0 {
			return nil, newEvalError(ErrDivisionByZero, "division by zero", callPos)
		}
		quotient /= n
	}
	return quotient, nil
}

func builtinLessThan(args []value, callPos position) (value, error) {
	return compareNumbers(args, callPos, "<", func(left, right int64) bool { return left < right })
}

func builtinGreaterThan(args []value, callPos position) (value, error) {
	return compareNumbers(args, callPos, ">", func(left, right int64) bool { return left > right })
}

func builtinEqual(args []value, callPos position) (value, error) {
	return compareNumbers(args, callPos, "=", func(left, right int64) bool { return left == right })
}

func builtinLessEqual(args []value, callPos position) (value, error) {
	return compareNumbers(args, callPos, "<=", func(left, right int64) bool { return left <= right })
}

func compareNumbers(args []value, callPos position, name string, cmp func(int64, int64) bool) (value, error) {
	if len(args) < 2 {
		return nil, wrongArgCount(callPos, name, "expected at least 2 arguments")
	}

	prev, err := expectInt(args[0], callPos)
	if err != nil {
		return nil, err
	}
	for _, arg := range args[1:] {
		current, err := expectInt(arg, callPos)
		if err != nil {
			return nil, err
		}
		if !cmp(prev, current) {
			return false, nil
		}
		prev = current
	}
	return true, nil
}

func builtinNot(args []value, callPos position) (value, error) {
	if len(args) != 1 {
		return nil, wrongArgCount(callPos, "not", "expected exactly 1 argument")
	}
	return !isTruthy(args[0]), nil
}

func expectInt(v value, pos position) (int64, error) {
	n, ok := v.(int64)
	if !ok {
		return 0, newEvalError(ErrTypeMismatch, "expected number", pos)
	}
	return n, nil
}

func wrongArgCount(pos position, name string, message string) error {
	return newEvalError(ErrWrongArgCount, fmt.Sprintf("%s: %s", name, message), pos)
}

func newEvalError(kind EvalErrorKind, message string, pos position) *EvalError {
	return &EvalError{
		Message: message,
		Kind:    kind,
		Line:    pos.line,
		Column:  pos.column,
	}
}

func parseProgram(input string) ([]expr, error) {
	lexer := newLexer(input)
	tokens, err := lexer.lexAll()
	if err != nil {
		return nil, err
	}

	parser := newParser(tokens)
	return parser.parseProgram()
}

func evalInput(input string) (value, string, error) {
	exprs, err := parseProgram(input)
	if err != nil {
		return nil, "", err
	}

	interpreter := newInterpreter()
	result, err := interpreter.evalProgram(exprs)
	if err != nil {
		return nil, "", err
	}
	return result, "", nil
}

func formatValue(v value) (string, error) {
	switch value := v.(type) {
	case int64:
		return strconv.FormatInt(value, 10), nil
	case bool:
		if value {
			return "#t", nil
		}
		return "#f", nil
	case string:
		return strconv.Quote(value), nil
	default:
		return "", &EvalError{Message: "cannot format value"}
	}
}
