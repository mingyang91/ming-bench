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

type rationalExpr struct {
	value rationalValue
	at    position
}

func (e *rationalExpr) pos() position { return e.at }

type inexactExpr struct {
	value inexactValue
	at    position
}

func (e *inexactExpr) pos() position { return e.at }

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

type charExpr struct {
	value rune
	at    position
}

func (e *charExpr) pos() position { return e.at }

type symbolExpr struct {
	name string
	key  string
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
	tokenQuote
	tokenInteger
	tokenRational
	tokenInexact
	tokenBoolean
	tokenString
	tokenChar
	tokenSymbol
)

type token struct {
	kind    tokenKind
	text    string
	number  int64
	rational rationalValue
	inexact inexactValue
	boolean bool
	char    rune
	at      position
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
	case '\'':
		l.advance()
		return token{kind: tokenQuote, text: "'", at: start}, nil
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

	if strings.HasPrefix(text, "#\\") {
		ch, ok := parseCharLiteral(text)
		if !ok {
			return token{}, newEvalError(ErrSyntax, fmt.Sprintf("invalid character literal: %s", text), start)
		}
		return token{kind: tokenChar, char: ch, text: text, at: start}, nil
	}

	if isIntegerLiteral(text) {
		value, err := strconv.ParseInt(text, 10, 64)
		if err != nil {
			return token{}, newEvalError(ErrSyntax, fmt.Sprintf("invalid integer literal: %s", text), start)
		}
		return token{kind: tokenInteger, number: value, text: text, at: start}, nil
	}

	if parsed, ok, err := parseNumberLiteral(text); ok || err != nil {
		if err != nil {
			return token{}, newEvalError(ErrSyntax, fmt.Sprintf("invalid numeric literal: %s", text), start)
		}
		switch number := parsed.(type) {
		case int64:
			return token{kind: tokenInteger, number: number, text: text, at: start}, nil
		case rationalValue:
			return token{kind: tokenRational, rational: number, text: text, at: start}, nil
		case inexactValue:
			return token{kind: tokenInexact, inexact: number, text: text, at: start}, nil
		}
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

func parseCharLiteral(text string) (rune, bool) {
	if !strings.HasPrefix(text, "#\\") {
		return 0, false
	}

	name := text[2:]
	switch name {
	case "space":
		return ' ', true
	case "newline":
		return '\n', true
	}

	runes := []rune(name)
	if len(runes) != 1 {
		return 0, false
	}
	return runes[0], true
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
	case tokenRational:
		p.index++
		return &rationalExpr{value: tok.rational, at: tok.at}, nil
	case tokenInexact:
		p.index++
		return &inexactExpr{value: tok.inexact, at: tok.at}, nil
	case tokenBoolean:
		p.index++
		return &boolExpr{value: tok.boolean, at: tok.at}, nil
	case tokenString:
		p.index++
		return &stringExpr{value: tok.text, at: tok.at}, nil
	case tokenChar:
		p.index++
		return &charExpr{value: tok.char, at: tok.at}, nil
	case tokenSymbol:
		p.index++
		return &symbolExpr{name: tok.text, at: tok.at}, nil
	case tokenQuote:
		return p.parseQuoted()
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

func (p *parser) parseQuoted() (expr, error) {
	tok := p.peek()
	p.index++

	quoted, err := p.parseExpr()
	if err != nil {
		return nil, err
	}

	return &listExpr{
		elements: []expr{
			&symbolExpr{name: "quote", at: tok.at},
			quoted,
		},
		at: tok.at,
	}, nil
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

type symbolValue string

type charValue rune

type stringValue struct {
	chars   []rune
	mutable bool
}

func newStringValue(text string) *stringValue {
	return &stringValue{
		chars:   []rune(text),
		mutable: true,
	}
}

func (s *stringValue) text() string {
	return string(s.chars)
}

func (s *stringValue) copy(mutable bool) *stringValue {
	chars := append([]rune(nil), s.chars...)
	return &stringValue{
		chars:   chars,
		mutable: mutable,
	}
}

type emptyListValue struct{}

type pairValue struct {
	car value
	cdr value
}

type voidValue struct{}

type builtinProc struct {
	name string
	fn   func(it *interpreter, args []value, callPos position) (value, error)
}

type bindingName struct {
	name string
	key  string
}

type closureProc struct {
	name    string
	params  []bindingName
	rest    bindingName
	hasRest bool
	body    []expr
	env     *env
}

type binding struct {
	name  string
	key   string
	value value
}

type env struct {
	parent *env
	values map[string]*binding
	keys   map[string]*binding
}

func newEnv(parent *env) *env {
	return &env{
		parent: parent,
		values: map[string]*binding{},
		keys:   map[string]*binding{},
	}
}

func (e *env) bind(b *binding) {
	e.values[b.name] = b
	e.keys[b.key] = b
}

func (e *env) lookup(name string) (value, bool) {
	binding, ok := e.lookupBinding(name)
	if !ok {
		return nil, false
	}
	return binding.value, true
}

func (e *env) lookupBinding(name string) (*binding, bool) {
	for current := e; current != nil; current = current.parent {
		if binding, ok := current.values[name]; ok {
			return binding, true
		}
	}
	return nil, false
}

func (e *env) lookupBindingKey(key string) (*binding, bool) {
	for current := e; current != nil; current = current.parent {
		if binding, ok := current.keys[key]; ok {
			return binding, true
		}
	}
	return nil, false
}

type interpreter struct {
	global *env
	output strings.Builder
	nextID int
}

func newInterpreter() *interpreter {
	it := &interpreter{
		global: newEnv(nil),
	}
	it.installBuiltins()
	return it
}

func (it *interpreter) freshBindingKey(name string) string {
	it.nextID++
	return fmt.Sprintf("%s#%d", name, it.nextID)
}

func (it *interpreter) defineName(scope *env, name string, v value) *binding {
	if existing, ok := scope.values[name]; ok {
		existing.value = v
		return existing
	}

	b := &binding{
		name:  name,
		key:   it.freshBindingKey(name),
		value: v,
	}
	scope.bind(b)
	return b
}

func (it *interpreter) defineSymbol(scope *env, sym *symbolExpr, v value) *binding {
	if sym.key != "" {
		if existing, ok := scope.keys[sym.key]; ok {
			existing.value = v
			return existing
		}

		b := &binding{
			name:  sym.name,
			key:   sym.key,
			value: v,
		}
		scope.bind(b)
		return b
	}

	return it.defineName(scope, sym.name, v)
}

func (it *interpreter) defineBindingName(scope *env, name bindingName, v value) *binding {
	return it.defineSymbol(scope, &symbolExpr{name: name.name, key: name.key}, v)
}

func (it *interpreter) lookupSymbolBinding(scope *env, sym *symbolExpr) (*binding, bool) {
	if sym.key != "" {
		return scope.lookupBindingKey(sym.key)
	}
	return scope.lookupBinding(sym.name)
}

func (it *interpreter) lookupSymbol(scope *env, sym *symbolExpr) (value, bool) {
	binding, ok := it.lookupSymbolBinding(scope, sym)
	if !ok {
		return nil, false
	}
	return binding.value, true
}

func (it *interpreter) installBuiltins() {
	it.defineName(it.global, "+", &builtinProc{name: "+", fn: builtinAdd})
	it.defineName(it.global, "-", &builtinProc{name: "-", fn: builtinSub})
	it.defineName(it.global, "*", &builtinProc{name: "*", fn: builtinMul})
	it.defineName(it.global, "/", &builtinProc{name: "/", fn: builtinDiv})
	it.defineName(it.global, "<", &builtinProc{name: "<", fn: builtinLessThan})
	it.defineName(it.global, ">", &builtinProc{name: ">", fn: builtinGreaterThan})
	it.defineName(it.global, "=", &builtinProc{name: "=", fn: builtinEqual})
	it.defineName(it.global, "<=", &builtinProc{name: "<=", fn: builtinLessEqual})
	it.defineName(it.global, "not", &builtinProc{name: "not", fn: builtinNot})
	it.defineName(it.global, "cons", &builtinProc{name: "cons", fn: builtinCons})
	it.defineName(it.global, "car", &builtinProc{name: "car", fn: builtinCar})
	it.defineName(it.global, "cdr", &builtinProc{name: "cdr", fn: builtinCdr})
	it.defineName(it.global, "null?", &builtinProc{name: "null?", fn: builtinNull})
	it.defineName(it.global, "list", &builtinProc{name: "list", fn: builtinList})
	it.defineName(it.global, "length", &builtinProc{name: "length", fn: builtinLength})
	it.defineName(it.global, "append", &builtinProc{name: "append", fn: builtinAppend})
	it.defineName(it.global, "string?", &builtinProc{name: "string?", fn: builtinStringPred})
	it.defineName(it.global, "number?", &builtinProc{name: "number?", fn: builtinNumberPred})
	it.defineName(it.global, "exact?", &builtinProc{name: "exact?", fn: builtinExactPred})
	it.defineName(it.global, "inexact?", &builtinProc{name: "inexact?", fn: builtinInexactPred})
	it.defineName(it.global, "exact->inexact", &builtinProc{name: "exact->inexact", fn: builtinExactToInexact})
	it.defineName(it.global, "inexact->exact", &builtinProc{name: "inexact->exact", fn: builtinInexactToExact})
	it.defineName(it.global, "numerator", &builtinProc{name: "numerator", fn: builtinNumerator})
	it.defineName(it.global, "denominator", &builtinProc{name: "denominator", fn: builtinDenominator})
	it.defineName(it.global, "integer?", &builtinProc{name: "integer?", fn: builtinIntegerPred})
	it.defineName(it.global, "rational?", &builtinProc{name: "rational?", fn: builtinRationalPred})
	it.defineName(it.global, "boolean?", &builtinProc{name: "boolean?", fn: builtinBooleanPred})
	it.defineName(it.global, "pair?", &builtinProc{name: "pair?", fn: builtinPairPred})
	it.defineName(it.global, "symbol?", &builtinProc{name: "symbol?", fn: builtinSymbolPred})
	it.defineName(it.global, "procedure?", &builtinProc{name: "procedure?", fn: builtinProcedurePred})
	it.defineName(it.global, "display", &builtinProc{name: "display", fn: builtinDisplay})
	it.defineName(it.global, "write", &builtinProc{name: "write", fn: builtinWrite})
	it.defineName(it.global, "newline", &builtinProc{name: "newline", fn: builtinNewline})
	it.defineName(it.global, "apply", &builtinProc{name: "apply", fn: builtinApply})
	it.defineName(it.global, "abs", &builtinProc{name: "abs", fn: builtinAbs})
	it.defineName(it.global, "modulo", &builtinProc{name: "modulo", fn: builtinModulo})
	it.defineName(it.global, "remainder", &builtinProc{name: "remainder", fn: builtinRemainder})
	it.defineName(it.global, "quotient", &builtinProc{name: "quotient", fn: builtinQuotient})
	it.defineName(it.global, "min", &builtinProc{name: "min", fn: builtinMin})
	it.defineName(it.global, "max", &builtinProc{name: "max", fn: builtinMax})
	it.defineName(it.global, "expt", &builtinProc{name: "expt", fn: builtinExpt})
	it.defineName(it.global, "zero?", &builtinProc{name: "zero?", fn: builtinZeroPred})
	it.defineName(it.global, "positive?", &builtinProc{name: "positive?", fn: builtinPositivePred})
	it.defineName(it.global, "negative?", &builtinProc{name: "negative?", fn: builtinNegativePred})
	it.defineName(it.global, "odd?", &builtinProc{name: "odd?", fn: builtinOddPred})
	it.defineName(it.global, "even?", &builtinProc{name: "even?", fn: builtinEvenPred})
	it.defineName(it.global, "string-copy", &builtinProc{name: "string-copy", fn: builtinStringCopy})
	it.defineName(it.global, "string-set!", &builtinProc{name: "string-set!", fn: builtinStringSet})
	it.defineName(it.global, "string-append", &builtinProc{name: "string-append", fn: builtinStringAppend})
	it.defineName(it.global, "string-length", &builtinProc{name: "string-length", fn: builtinStringLength})
	it.defineName(it.global, "substring", &builtinProc{name: "substring", fn: builtinSubstring})
	it.defineName(it.global, "string->number", &builtinProc{name: "string->number", fn: builtinStringToNumber})
	it.defineName(it.global, "number->string", &builtinProc{name: "number->string", fn: builtinNumberToString})
	it.defineName(it.global, "symbol->string", &builtinProc{name: "symbol->string", fn: builtinSymbolToString})
	it.defineName(it.global, "string->symbol", &builtinProc{name: "string->symbol", fn: builtinStringToSymbol})
	it.defineName(it.global, "string-ref", &builtinProc{name: "string-ref", fn: builtinStringRef})
	it.defineName(it.global, "string=?", &builtinProc{name: "string=?", fn: builtinStringEqualPred})
	it.defineName(it.global, "string<?", &builtinProc{name: "string<?", fn: builtinStringLessPred})
	it.defineName(it.global, "string-ci=?", &builtinProc{name: "string-ci=?", fn: builtinStringCIEqualPred})
	it.defineName(it.global, "string-upcase", &builtinProc{name: "string-upcase", fn: builtinStringUpcase})
	it.defineName(it.global, "string-downcase", &builtinProc{name: "string-downcase", fn: builtinStringDowncase})
	it.defineName(it.global, "char?", &builtinProc{name: "char?", fn: builtinCharPred})
	it.defineName(it.global, "char-alphabetic?", &builtinProc{name: "char-alphabetic?", fn: builtinCharAlphabeticPred})
	it.defineName(it.global, "char-numeric?", &builtinProc{name: "char-numeric?", fn: builtinCharNumericPred})
	it.defineName(it.global, "char-upcase", &builtinProc{name: "char-upcase", fn: builtinCharUpcase})
	it.defineName(it.global, "char-downcase", &builtinProc{name: "char-downcase", fn: builtinCharDowncase})
	it.defineName(it.global, "char=?", &builtinProc{name: "char=?", fn: builtinCharEqualPred})
	it.defineName(it.global, "char<?", &builtinProc{name: "char<?", fn: builtinCharLessPred})
	it.defineName(it.global, "eq?", &builtinProc{name: "eq?", fn: builtinEqPred})
	it.defineName(it.global, "equal?", &builtinProc{name: "equal?", fn: builtinDeepEqualPred})
	it.defineName(it.global, "list?", &builtinProc{name: "list?", fn: builtinListPred})
	it.defineName(it.global, "list-ref", &builtinProc{name: "list-ref", fn: builtinListRef})
	it.defineName(it.global, "list-tail", &builtinProc{name: "list-tail", fn: builtinListTail})
	it.defineName(it.global, "assoc", &builtinProc{name: "assoc", fn: builtinAssoc})
	it.defineName(it.global, "map", &builtinProc{name: "map", fn: builtinMap})
}

func (it *interpreter) evalProgram(exprs []expr) (value, error) {
	if len(exprs) == 0 {
		return nil, newEvalError(ErrSyntax, "expected expression", position{line: 1, column: 1})
	}

	return it.evalSequence(it.global, exprs)
}

func (it *interpreter) evalSequence(scope *env, exprs []expr) (value, error) {
	result := value(voidValue{})
	for _, node := range exprs {
		current, err := it.eval(node, scope)
		if err != nil {
			return nil, err
		}
		result = current
	}
	return result, nil
}

func (it *interpreter) eval(node expr, scope *env) (value, error) {
	switch expr := node.(type) {
	case *intExpr:
		return expr.value, nil
	case *rationalExpr:
		return expr.value, nil
	case *inexactExpr:
		return expr.value, nil
	case *boolExpr:
		return expr.value, nil
	case *stringExpr:
		return newStringValue(expr.value), nil
	case *charExpr:
		return charValue(expr.value), nil
	case *symbolExpr:
		value, ok := it.lookupSymbol(scope, expr)
		if !ok {
			return nil, newEvalError(ErrUnboundVariable, fmt.Sprintf("unbound variable: %s", expr.name), expr.at)
		}
		if _, isMacro := value.(*syntaxRuleMacro); isMacro {
			return nil, newEvalError(ErrSyntax, fmt.Sprintf("cannot use syntax as value: %s", expr.name), expr.at)
		}
		return value, nil
	case *listExpr:
		return it.evalList(expr, scope)
	default:
		return nil, newEvalError(ErrSyntax, "unknown expression", node.pos())
	}
}

func (it *interpreter) evalList(list *listExpr, scope *env) (value, error) {
	if len(list.elements) == 0 {
		return nil, newEvalError(ErrSyntax, "cannot evaluate empty list", list.at)
	}

	if sym, ok := list.elements[0].(*symbolExpr); ok {
		switch sym.name {
		case "and":
			return it.evalAnd(scope, list.elements[1:])
		case "or":
			return it.evalOr(scope, list.elements[1:])
		case "define":
			return it.evalDefine(scope, list)
		case "define-record-type":
			return it.evalDefineRecordType(scope, list)
		case "define-syntax":
			return it.evalDefineSyntax(scope, list)
		case "if":
			return it.evalIf(scope, list)
		case "quote":
			return it.evalQuote(list)
		case "lambda":
			return it.evalLambda(scope, list)
		case "case-lambda":
			return it.evalCaseLambda(scope, list)
		case "set!":
			return it.evalSet(scope, list)
		case "begin":
			return it.evalBegin(scope, list)
		case "cond":
			return it.evalCond(scope, list)
		case "let":
			return it.evalLet(scope, list)
		}

		if expanded, ok, err := it.expandMacroCall(list, scope); ok || err != nil {
			if err != nil {
				return nil, err
			}
			return it.eval(expanded, scope)
		}
	}

	operator, err := it.eval(list.elements[0], scope)
	if err != nil {
		return nil, err
	}

	args := make([]value, 0, len(list.elements)-1)
	for _, argExpr := range list.elements[1:] {
		arg, err := it.eval(argExpr, scope)
		if err != nil {
			return nil, err
		}
		args = append(args, arg)
	}

	return it.applyProcedure(operator, args, list.at)
}

func (it *interpreter) evalDefine(scope *env, list *listExpr) (value, error) {
	if len(list.elements) < 3 {
		return nil, newEvalError(ErrSyntax, "define: expected a name and value", list.at)
	}

	switch target := list.elements[1].(type) {
	case *symbolExpr:
		if len(list.elements) != 3 {
			return nil, newEvalError(ErrSyntax, "define: expected exactly one value expression", list.at)
		}
		result, err := it.eval(list.elements[2], scope)
		if err != nil {
			return nil, err
		}
		it.defineSymbol(scope, target, result)
		return voidValue{}, nil
	case *listExpr:
		if len(target.elements) == 0 {
			return nil, newEvalError(ErrSyntax, "define: expected function name", target.at)
		}
		name, ok := target.elements[0].(*symbolExpr)
		if !ok {
			return nil, newEvalError(ErrSyntax, "define: expected function name", target.elements[0].pos())
		}
		params, rest, hasRest, err := parseParamNames(target.elements[1:])
		if err != nil {
			return nil, err
		}
		proc := &closureProc{
			name:    name.name,
			params:  params,
			rest:    rest,
			hasRest: hasRest,
			body:    list.elements[2:],
			env:     scope,
		}
		it.defineSymbol(scope, name, proc)
		return voidValue{}, nil
	default:
		return nil, newEvalError(ErrSyntax, "define: expected a symbol or function signature", list.elements[1].pos())
	}
}

func (it *interpreter) evalSet(scope *env, list *listExpr) (value, error) {
	if len(list.elements) != 3 {
		return nil, newEvalError(ErrSyntax, "set!: expected a name and value", list.at)
	}

	target, ok := list.elements[1].(*symbolExpr)
	if !ok {
		return nil, newEvalError(ErrSyntax, "set!: expected variable name", list.elements[1].pos())
	}

	binding, ok := it.lookupSymbolBinding(scope, target)
	if !ok {
		return nil, newEvalError(ErrUnboundVariable, fmt.Sprintf("set!: unbound variable: %s", target.name), target.at)
	}

	result, err := it.eval(list.elements[2], scope)
	if err != nil {
		return nil, err
	}

	binding.value = result
	return voidValue{}, nil
}

func (it *interpreter) evalIf(scope *env, list *listExpr) (value, error) {
	if len(list.elements) != 3 && len(list.elements) != 4 {
		return nil, newEvalError(ErrSyntax, "if: expected a test, consequent, and optional alternate", list.at)
	}

	test, err := it.eval(list.elements[1], scope)
	if err != nil {
		return nil, err
	}
	if isTruthy(test) {
		return it.eval(list.elements[2], scope)
	}
	if len(list.elements) == 4 {
		return it.eval(list.elements[3], scope)
	}
	return voidValue{}, nil
}

func (it *interpreter) evalQuote(list *listExpr) (value, error) {
	if len(list.elements) != 2 {
		return nil, newEvalError(ErrSyntax, "quote: expected exactly one argument", list.at)
	}
	return datumToValue(list.elements[1])
}

func (it *interpreter) evalLambda(scope *env, list *listExpr) (value, error) {
	if len(list.elements) < 3 {
		return nil, newEvalError(ErrSyntax, "lambda: expected parameters and body", list.at)
	}

	params, rest, hasRest, err := parseLambdaParams(list.elements[1])
	if err != nil {
		return nil, err
	}

	return &closureProc{
		params:  params,
		rest:    rest,
		hasRest: hasRest,
		body:    list.elements[2:],
		env:     scope,
	}, nil
}

func (it *interpreter) evalBegin(scope *env, list *listExpr) (value, error) {
	if len(list.elements) == 1 {
		return voidValue{}, nil
	}
	return it.evalSequence(scope, list.elements[1:])
}

func (it *interpreter) evalCond(scope *env, list *listExpr) (value, error) {
	if len(list.elements) == 1 {
		return voidValue{}, nil
	}

	for idx, clauseExpr := range list.elements[1:] {
		clause, ok := clauseExpr.(*listExpr)
		if !ok || len(clause.elements) == 0 {
			return nil, newEvalError(ErrSyntax, "cond: expected non-empty clause", clauseExpr.pos())
		}

		if sym, ok := clause.elements[0].(*symbolExpr); ok && sym.name == "else" {
			if idx != len(list.elements[1:])-1 {
				return nil, newEvalError(ErrSyntax, "cond: else clause must be last", sym.at)
			}
			if len(clause.elements) == 1 {
				return voidValue{}, nil
			}
			return it.evalSequence(scope, clause.elements[1:])
		}

		test, err := it.eval(clause.elements[0], scope)
		if err != nil {
			return nil, err
		}
		if isTruthy(test) {
			if len(clause.elements) == 1 {
				return test, nil
			}
			return it.evalSequence(scope, clause.elements[1:])
		}
	}

	return voidValue{}, nil
}

type letBinding struct {
	name bindingName
	init expr
}

func (it *interpreter) evalLet(scope *env, list *listExpr) (value, error) {
	if len(list.elements) < 3 {
		return nil, newEvalError(ErrSyntax, "let: expected bindings and body", list.at)
	}

	bindingIndex := 1
	var name *symbolExpr
	if sym, ok := list.elements[1].(*symbolExpr); ok {
		name = sym
		bindingIndex = 2
		if len(list.elements) < 4 {
			return nil, newEvalError(ErrSyntax, "let: expected named bindings and body", list.at)
		}
	}

	bindingList, ok := list.elements[bindingIndex].(*listExpr)
	if !ok {
		return nil, newEvalError(ErrSyntax, "let: expected binding list", list.elements[bindingIndex].pos())
	}

	bindings, err := parseLetBindings(bindingList)
	if err != nil {
		return nil, err
	}

	body := list.elements[bindingIndex+1:]
	if len(body) == 0 {
		return nil, newEvalError(ErrSyntax, "let: expected body", list.at)
	}

	args := make([]value, 0, len(bindings))
	params := make([]bindingName, 0, len(bindings))
	for _, binding := range bindings {
		current, err := it.eval(binding.init, scope)
		if err != nil {
			return nil, err
		}
		args = append(args, current)
		params = append(params, binding.name)
	}

	if name == nil {
		letEnv := newEnv(scope)
		for i, param := range params {
			it.defineBindingName(letEnv, param, args[i])
		}
		return it.evalSequence(letEnv, body)
	}

	letEnv := newEnv(scope)
	proc := &closureProc{
		name:   name.name,
		params: params,
		body:   body,
		env:    letEnv,
	}
	it.defineSymbol(letEnv, name, proc)
	return it.applyClosure(proc, args, list.at)
}

func (it *interpreter) applyProcedure(proc value, args []value, callPos position) (value, error) {
	switch proc := proc.(type) {
	case *builtinProc:
		return proc.fn(it, args, callPos)
	case *closureProc:
		return it.applyClosure(proc, args, callPos)
	case *caseLambdaProc:
		return it.applyCaseLambda(proc, args, callPos)
	default:
		return nil, newEvalError(ErrNotProcedure, "attempted to call a non-procedure", callPos)
	}
}

func (it *interpreter) applyClosure(proc *closureProc, args []value, callPos position) (value, error) {
	name := "lambda"
	if proc.name != "" {
		name = proc.name
	}

	if proc.hasRest {
		if len(args) < len(proc.params) {
			return nil, wrongArgCount(callPos, name, fmt.Sprintf("expected at least %d arguments, got %d", len(proc.params), len(args)))
		}
	} else if len(args) != len(proc.params) {
		return nil, wrongArgCount(callPos, name, fmt.Sprintf("expected %d arguments, got %d", len(proc.params), len(args)))
	}
	return it.applyProcedureBody(proc.env, proc.params, proc.rest, proc.hasRest, proc.body, args)
}

func (it *interpreter) evalAnd(scope *env, args []expr) (value, error) {
	result := value(true)
	for _, arg := range args {
		current, err := it.eval(arg, scope)
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

func (it *interpreter) evalOr(scope *env, args []expr) (value, error) {
	for _, arg := range args {
		current, err := it.eval(arg, scope)
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

func parseLambdaParams(node expr) ([]bindingName, bindingName, bool, error) {
	switch formals := node.(type) {
	case *listExpr:
		return parseParamNames(formals.elements)
	case *symbolExpr:
		if formals.name == "." {
			return nil, bindingName{}, false, newEvalError(ErrSyntax, "expected parameter name", formals.at)
		}
		return nil, bindingName{name: formals.name, key: formals.key}, true, nil
	default:
		return nil, bindingName{}, false, newEvalError(ErrSyntax, "lambda: expected parameter list", node.pos())
	}
}

func parseParamNames(nodes []expr) ([]bindingName, bindingName, bool, error) {
	params := make([]bindingName, 0, len(nodes))
	for i, node := range nodes {
		sym, ok := node.(*symbolExpr)
		if !ok {
			return nil, bindingName{}, false, newEvalError(ErrSyntax, "expected parameter name", node.pos())
		}
		if sym.name != "." {
			params = append(params, bindingName{name: sym.name, key: sym.key})
			continue
		}

		if i == len(nodes)-1 {
			return nil, bindingName{}, false, newEvalError(ErrSyntax, "expected rest parameter name", sym.at)
		}

		restNode := nodes[i+1]
		rest, ok := restNode.(*symbolExpr)
		if !ok || rest.name == "." {
			return nil, bindingName{}, false, newEvalError(ErrSyntax, "expected rest parameter name", restNode.pos())
		}
		if i+2 != len(nodes) {
			return nil, bindingName{}, false, newEvalError(ErrSyntax, "expected '.' before final parameter", nodes[i+2].pos())
		}
		return params, bindingName{name: rest.name, key: rest.key}, true, nil
	}
	return params, bindingName{}, false, nil
}

func parseLetBindings(list *listExpr) ([]letBinding, error) {
	bindings := make([]letBinding, 0, len(list.elements))
	for _, bindingExpr := range list.elements {
		binding, ok := bindingExpr.(*listExpr)
		if !ok || len(binding.elements) != 2 {
			return nil, newEvalError(ErrSyntax, "let: expected binding pair", bindingExpr.pos())
		}

		name, ok := binding.elements[0].(*symbolExpr)
		if !ok {
			return nil, newEvalError(ErrSyntax, "let: expected binding name", binding.elements[0].pos())
		}

		bindings = append(bindings, letBinding{
			name: bindingName{name: name.name, key: name.key},
			init: binding.elements[1],
		})
	}
	return bindings, nil
}

func datumToValue(node expr) (value, error) {
	switch expr := node.(type) {
	case *intExpr:
		return expr.value, nil
	case *rationalExpr:
		return expr.value, nil
	case *inexactExpr:
		return expr.value, nil
	case *boolExpr:
		return expr.value, nil
	case *stringExpr:
		return newStringValue(expr.value), nil
	case *charExpr:
		return charValue(expr.value), nil
	case *symbolExpr:
		return symbolValue(expr.name), nil
	case *listExpr:
		values := make([]value, 0, len(expr.elements))
		for _, element := range expr.elements {
			item, err := datumToValue(element)
			if err != nil {
				return nil, err
			}
			values = append(values, item)
		}
		return buildList(values), nil
	default:
		return nil, newEvalError(ErrSyntax, "invalid quoted datum", node.pos())
	}
}

func buildList(items []value) value {
	result := value(emptyListValue{})
	for i := len(items) - 1; i >= 0; i-- {
		result = &pairValue{car: items[i], cdr: result}
	}
	return result
}

func listToSlice(v value, pos position) ([]value, error) {
	items := []value{}
	for {
		switch current := v.(type) {
		case emptyListValue:
			return items, nil
		case *pairValue:
			items = append(items, current.car)
			v = current.cdr
		default:
			return nil, newEvalError(ErrTypeMismatch, "expected list", pos)
		}
	}
}

func builtinAdd(_ *interpreter, args []value, callPos position) (value, error) {
	total := value(int64(0))
	for _, arg := range args {
		current, err := expectNumberValue(arg, callPos)
		if err != nil {
			return nil, err
		}
		total = addNumbers(total, current)
	}
	return total, nil
}

func builtinSub(_ *interpreter, args []value, callPos position) (value, error) {
	if len(args) == 0 {
		return nil, wrongArgCount(callPos, "-", "expected at least 1 argument")
	}

	first, err := expectNumberValue(args[0], callPos)
	if err != nil {
		return nil, err
	}
	if len(args) == 1 {
		return negateNumber(first), nil
	}

	total := first
	for _, arg := range args[1:] {
		current, err := expectNumberValue(arg, callPos)
		if err != nil {
			return nil, err
		}
		total = subNumbers(total, current)
	}
	return total, nil
}

func builtinMul(_ *interpreter, args []value, callPos position) (value, error) {
	product := value(int64(1))
	for _, arg := range args {
		current, err := expectNumberValue(arg, callPos)
		if err != nil {
			return nil, err
		}
		product = mulNumbers(product, current)
	}
	return product, nil
}

func builtinDiv(_ *interpreter, args []value, callPos position) (value, error) {
	if len(args) < 2 {
		return nil, wrongArgCount(callPos, "/", "expected at least 2 arguments")
	}

	quotient, err := expectNumberValue(args[0], callPos)
	if err != nil {
		return nil, err
	}
	for _, arg := range args[1:] {
		current, err := expectNumberValue(arg, callPos)
		if err != nil {
			return nil, err
		}
		quotient, err = divNumbers(quotient, current, callPos)
		if err != nil {
			return nil, err
		}
	}
	return quotient, nil
}

func builtinLessThan(_ *interpreter, args []value, callPos position) (value, error) {
	return compareNumbers(
		args,
		callPos,
		"<",
		func(left exactFraction, right exactFraction) bool {
			return left.num*right.den < right.num*left.den
		},
		func(left float64, right float64) bool { return left < right },
	)
}

func builtinGreaterThan(_ *interpreter, args []value, callPos position) (value, error) {
	return compareNumbers(
		args,
		callPos,
		">",
		func(left exactFraction, right exactFraction) bool {
			return left.num*right.den > right.num*left.den
		},
		func(left float64, right float64) bool { return left > right },
	)
}

func builtinEqual(_ *interpreter, args []value, callPos position) (value, error) {
	return compareNumbers(
		args,
		callPos,
		"=",
		func(left exactFraction, right exactFraction) bool {
			return left.num*right.den == right.num*left.den
		},
		func(left float64, right float64) bool { return left == right },
	)
}

func builtinLessEqual(_ *interpreter, args []value, callPos position) (value, error) {
	return compareNumbers(
		args,
		callPos,
		"<=",
		func(left exactFraction, right exactFraction) bool {
			return left.num*right.den <= right.num*left.den
		},
		func(left float64, right float64) bool { return left <= right },
	)
}

func compareNumbers(
	args []value,
	callPos position,
	name string,
	exactCmp func(exactFraction, exactFraction) bool,
	inexactCmp func(float64, float64) bool,
) (value, error) {
	if len(args) < 2 {
		return nil, wrongArgCount(callPos, name, "expected at least 2 arguments")
	}

	prev, err := expectNumberValue(args[0], callPos)
	if err != nil {
		return nil, err
	}
	for _, arg := range args[1:] {
		current, err := expectNumberValue(arg, callPos)
		if err != nil {
			return nil, err
		}
		if !compareTwoNumbers(
			prev,
			current,
			exactCmp,
			inexactCmp,
		) {
			return false, nil
		}
		prev = current
	}
	return true, nil
}

func builtinNot(_ *interpreter, args []value, callPos position) (value, error) {
	if len(args) != 1 {
		return nil, wrongArgCount(callPos, "not", "expected exactly 1 argument")
	}
	return !isTruthy(args[0]), nil
}

func builtinCons(_ *interpreter, args []value, callPos position) (value, error) {
	if len(args) != 2 {
		return nil, wrongArgCount(callPos, "cons", "expected exactly 2 arguments")
	}
	return &pairValue{car: args[0], cdr: args[1]}, nil
}

func builtinCar(_ *interpreter, args []value, callPos position) (value, error) {
	if len(args) != 1 {
		return nil, wrongArgCount(callPos, "car", "expected exactly 1 argument")
	}

	pair, ok := args[0].(*pairValue)
	if !ok {
		return nil, newEvalError(ErrTypeMismatch, "car: expected pair", callPos)
	}
	return pair.car, nil
}

func builtinCdr(_ *interpreter, args []value, callPos position) (value, error) {
	if len(args) != 1 {
		return nil, wrongArgCount(callPos, "cdr", "expected exactly 1 argument")
	}

	pair, ok := args[0].(*pairValue)
	if !ok {
		return nil, newEvalError(ErrTypeMismatch, "cdr: expected pair", callPos)
	}
	return pair.cdr, nil
}

func builtinNull(_ *interpreter, args []value, callPos position) (value, error) {
	if len(args) != 1 {
		return nil, wrongArgCount(callPos, "null?", "expected exactly 1 argument")
	}
	_, ok := args[0].(emptyListValue)
	return ok, nil
}

func builtinList(_ *interpreter, args []value, callPos position) (value, error) {
	return buildList(args), nil
}

func builtinLength(_ *interpreter, args []value, callPos position) (value, error) {
	if len(args) != 1 {
		return nil, wrongArgCount(callPos, "length", "expected exactly 1 argument")
	}

	items, err := listToSlice(args[0], callPos)
	if err != nil {
		return nil, err
	}
	return int64(len(items)), nil
}

func builtinAppend(_ *interpreter, args []value, callPos position) (value, error) {
	if len(args) == 0 {
		return emptyListValue{}, nil
	}

	result := args[len(args)-1]
	for i := len(args) - 2; i >= 0; i-- {
		items, err := listToSlice(args[i], callPos)
		if err != nil {
			return nil, err
		}
		for j := len(items) - 1; j >= 0; j-- {
			result = &pairValue{car: items[j], cdr: result}
		}
	}
	return result, nil
}

func builtinApply(it *interpreter, args []value, callPos position) (value, error) {
	if len(args) < 2 {
		return nil, wrongArgCount(callPos, "apply", "expected at least 2 arguments")
	}

	tailArgs, err := listToSlice(args[len(args)-1], callPos)
	if err != nil {
		return nil, err
	}

	appliedArgs := make([]value, 0, len(args)-2+len(tailArgs))
	appliedArgs = append(appliedArgs, args[1:len(args)-1]...)
	appliedArgs = append(appliedArgs, tailArgs...)
	return it.applyProcedure(args[0], appliedArgs, callPos)
}

func builtinStringPred(_ *interpreter, args []value, callPos position) (value, error) {
	if len(args) != 1 {
		return nil, wrongArgCount(callPos, "string?", "expected exactly 1 argument")
	}
	_, ok := args[0].(*stringValue)
	return ok, nil
}

func builtinNumberPred(_ *interpreter, args []value, callPos position) (value, error) {
	if len(args) != 1 {
		return nil, wrongArgCount(callPos, "number?", "expected exactly 1 argument")
	}
	return isNumberValue(args[0]), nil
}

func builtinBooleanPred(_ *interpreter, args []value, callPos position) (value, error) {
	if len(args) != 1 {
		return nil, wrongArgCount(callPos, "boolean?", "expected exactly 1 argument")
	}
	_, ok := args[0].(bool)
	return ok, nil
}

func builtinPairPred(_ *interpreter, args []value, callPos position) (value, error) {
	if len(args) != 1 {
		return nil, wrongArgCount(callPos, "pair?", "expected exactly 1 argument")
	}
	_, ok := args[0].(*pairValue)
	return ok, nil
}

func builtinSymbolPred(_ *interpreter, args []value, callPos position) (value, error) {
	if len(args) != 1 {
		return nil, wrongArgCount(callPos, "symbol?", "expected exactly 1 argument")
	}
	_, ok := args[0].(symbolValue)
	return ok, nil
}

func builtinDisplay(it *interpreter, args []value, callPos position) (value, error) {
	if len(args) != 1 {
		return nil, wrongArgCount(callPos, "display", "expected exactly 1 argument")
	}

	formatted, err := formatDisplayValue(args[0])
	if err != nil {
		return nil, err
	}
	it.output.WriteString(formatted)
	return voidValue{}, nil
}

func builtinWrite(it *interpreter, args []value, callPos position) (value, error) {
	if len(args) != 1 {
		return nil, wrongArgCount(callPos, "write", "expected exactly 1 argument")
	}

	formatted, err := formatValue(args[0])
	if err != nil {
		return nil, err
	}
	it.output.WriteString(formatted)
	return voidValue{}, nil
}

func builtinNewline(it *interpreter, args []value, callPos position) (value, error) {
	if len(args) != 0 {
		return nil, wrongArgCount(callPos, "newline", "expected exactly 0 arguments")
	}
	it.output.WriteByte('\n')
	return voidValue{}, nil
}

func builtinStringCopy(_ *interpreter, args []value, callPos position) (value, error) {
	if len(args) != 1 {
		return nil, wrongArgCount(callPos, "string-copy", "expected exactly 1 argument")
	}

	s, err := expectString(args[0], callPos)
	if err != nil {
		return nil, err
	}
	return s.copy(true), nil
}

func builtinStringSet(_ *interpreter, args []value, callPos position) (value, error) {
	if len(args) != 3 {
		return nil, wrongArgCount(callPos, "string-set!", "expected exactly 3 arguments")
	}

	s, err := expectString(args[0], callPos)
	if err != nil {
		return nil, err
	}
	index, err := expectIndex(args[1], callPos)
	if err != nil {
		return nil, err
	}
	ch, err := expectChar(args[2], callPos)
	if err != nil {
		return nil, err
	}

	if index < 0 || index >= len(s.chars) {
		return nil, newEvalError(ErrOutOfRange, "string-set!: index out of range", callPos)
	}
	s.chars[index] = rune(ch)
	return voidValue{}, nil
}

func builtinStringAppend(_ *interpreter, args []value, callPos position) (value, error) {
	var builder strings.Builder
	for _, arg := range args {
		s, err := expectString(arg, callPos)
		if err != nil {
			return nil, err
		}
		builder.WriteString(s.text())
	}
	return newStringValue(builder.String()), nil
}

func builtinStringLength(_ *interpreter, args []value, callPos position) (value, error) {
	if len(args) != 1 {
		return nil, wrongArgCount(callPos, "string-length", "expected exactly 1 argument")
	}

	s, err := expectString(args[0], callPos)
	if err != nil {
		return nil, err
	}
	return int64(len(s.chars)), nil
}

func builtinSubstring(_ *interpreter, args []value, callPos position) (value, error) {
	if len(args) != 3 {
		return nil, wrongArgCount(callPos, "substring", "expected exactly 3 arguments")
	}

	s, err := expectString(args[0], callPos)
	if err != nil {
		return nil, err
	}
	start, err := expectIndex(args[1], callPos)
	if err != nil {
		return nil, err
	}
	end, err := expectIndex(args[2], callPos)
	if err != nil {
		return nil, err
	}

	if start < 0 || end < start || end > len(s.chars) {
		return nil, newEvalError(ErrOutOfRange, "substring: index out of range", callPos)
	}
	return &stringValue{
		chars:   append([]rune(nil), s.chars[start:end]...),
		mutable: true,
	}, nil
}

func builtinStringToNumber(_ *interpreter, args []value, callPos position) (value, error) {
	if len(args) != 1 {
		return nil, wrongArgCount(callPos, "string->number", "expected exactly 1 argument")
	}

	s, err := expectString(args[0], callPos)
	if err != nil {
		return nil, err
	}

	n, ok, err := parseNumberLiteral(s.text())
	if err != nil || !ok {
		return false, nil
	}
	return n, nil
}

func builtinNumberToString(_ *interpreter, args []value, callPos position) (value, error) {
	if len(args) != 1 {
		return nil, wrongArgCount(callPos, "number->string", "expected exactly 1 argument")
	}

	n, err := expectNumberValue(args[0], callPos)
	if err != nil {
		return nil, err
	}
	formatted, err := formatNumberValue(n)
	if err != nil {
		return nil, err
	}
	return newStringValue(formatted), nil
}

func builtinSymbolToString(_ *interpreter, args []value, callPos position) (value, error) {
	if len(args) != 1 {
		return nil, wrongArgCount(callPos, "symbol->string", "expected exactly 1 argument")
	}

	sym, err := expectSymbol(args[0], callPos)
	if err != nil {
		return nil, err
	}
	return newStringValue(string(sym)), nil
}

func builtinStringToSymbol(_ *interpreter, args []value, callPos position) (value, error) {
	if len(args) != 1 {
		return nil, wrongArgCount(callPos, "string->symbol", "expected exactly 1 argument")
	}

	s, err := expectString(args[0], callPos)
	if err != nil {
		return nil, err
	}
	return symbolValue(s.text()), nil
}

func builtinStringRef(_ *interpreter, args []value, callPos position) (value, error) {
	if len(args) != 2 {
		return nil, wrongArgCount(callPos, "string-ref", "expected exactly 2 arguments")
	}

	s, err := expectString(args[0], callPos)
	if err != nil {
		return nil, err
	}
	index, err := expectIndex(args[1], callPos)
	if err != nil {
		return nil, err
	}

	if index < 0 || index >= len(s.chars) {
		return nil, newEvalError(ErrOutOfRange, "string-ref: index out of range", callPos)
	}
	return charValue(s.chars[index]), nil
}

func builtinCharPred(_ *interpreter, args []value, callPos position) (value, error) {
	if len(args) != 1 {
		return nil, wrongArgCount(callPos, "char?", "expected exactly 1 argument")
	}
	_, ok := args[0].(charValue)
	return ok, nil
}

func expectInt(v value, pos position) (int64, error) {
	n, ok := v.(int64)
	if !ok {
		return 0, newEvalError(ErrTypeMismatch, "expected number", pos)
	}
	return n, nil
}

func expectString(v value, pos position) (*stringValue, error) {
	s, ok := v.(*stringValue)
	if !ok {
		return nil, newEvalError(ErrTypeMismatch, "expected string", pos)
	}
	return s, nil
}

func expectSymbol(v value, pos position) (symbolValue, error) {
	sym, ok := v.(symbolValue)
	if !ok {
		return "", newEvalError(ErrTypeMismatch, "expected symbol", pos)
	}
	return sym, nil
}

func expectChar(v value, pos position) (charValue, error) {
	ch, ok := v.(charValue)
	if !ok {
		return 0, newEvalError(ErrTypeMismatch, "expected character", pos)
	}
	return ch, nil
}

func expectIndex(v value, pos position) (int, error) {
	n, err := expectInt(v, pos)
	if err != nil {
		return 0, err
	}
	if n < 0 {
		return 0, newEvalError(ErrOutOfRange, "expected non-negative index", pos)
	}
	return int(n), nil
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
		return nil, interpreter.output.String(), err
	}
	return result, interpreter.output.String(), nil
}

func formatValue(v value) (string, error) {
	switch value := v.(type) {
	case int64:
		return formatNumberValue(value)
	case rationalValue:
		return formatNumberValue(value)
	case inexactValue:
		return formatNumberValue(value)
	case bool:
		if value {
			return "#t", nil
		}
		return "#f", nil
	case *stringValue:
		return strconv.Quote(value.text()), nil
	case symbolValue:
		return string(value), nil
	case charValue:
		return formatChar(rune(value)), nil
	case emptyListValue:
		return "()", nil
	case *pairValue:
		return formatPair(value)
	case *builtinProc, *closureProc, *caseLambdaProc:
		return "#<procedure>", nil
	case *recordValue:
		return fmt.Sprintf("#<record %s>", value.typ.name), nil
	case voidValue:
		return "#<void>", nil
	default:
		return "", &EvalError{Message: "cannot format value"}
	}
}

func formatDisplayValue(v value) (string, error) {
	switch value := v.(type) {
	case *stringValue:
		return value.text(), nil
	case charValue:
		return string(rune(value)), nil
	case *pairValue:
		return formatPairWith(value, formatDisplayValue)
	default:
		return formatValue(v)
	}
}

func formatChar(ch rune) string {
	switch ch {
	case ' ':
		return "#\\space"
	case '\n':
		return "#\\newline"
	default:
		return "#\\" + string(ch)
	}
}

func formatPair(pair *pairValue) (string, error) {
	return formatPairWith(pair, formatValue)
}

func formatPairWith(pair *pairValue, formatter func(value) (string, error)) (string, error) {
	parts := []string{}
	current := value(pair)

	for {
		switch cell := current.(type) {
		case *pairValue:
			formatted, err := formatter(cell.car)
			if err != nil {
				return "", err
			}
			parts = append(parts, formatted)
			current = cell.cdr
		case emptyListValue:
			return "(" + strings.Join(parts, " ") + ")", nil
		default:
			tail, err := formatter(cell)
			if err != nil {
				return "", err
			}
			return "(" + strings.Join(parts, " ") + " . " + tail + ")", nil
		}
	}
}
