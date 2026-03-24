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
	valLambda
	valChar
)

type pair struct {
	car value
	cdr value
}

type lambda struct {
	params []string
	body   []*expr
	env    *env
}

type value struct {
	kind   valueKind
	ival   int64
	bval   bool
	sval   string
	cval   rune
	pair   *pair
	lambda *lambda
	mstr   *[]rune // mutable string buffer (set by string-copy)
}

var voidVal = value{kind: valVoid}
var nullVal = value{kind: valNull}

// strContent returns the string content, handling both immutable and mutable strings.
func (v value) strContent() string {
	if v.mstr != nil {
		return string(*v.mstr)
	}
	return v.sval
}

func intVal(n int64) value  { return value{kind: valInteger, ival: n} }
func boolVal(b bool) value  { return value{kind: valBoolean, bval: b} }
func strVal(s string) value { return value{kind: valString, sval: s} }
func symVal(s string) value { return value{kind: valSymbol, sval: s} }
func charVal(c rune) value  { return value{kind: valChar, cval: c} }
func pairVal(car, cdr value) value {
	return value{kind: valPair, pair: &pair{car: car, cdr: cdr}}
}

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
		if v.mstr != nil {
			return `"` + string(*v.mstr) + `"`
		}
		return `"` + v.sval + `"`
	case valSymbol:
		return v.sval
	case valNull:
		return "()"
	case valVoid:
		return ""
	case valPair:
		return "(" + writePairInner(v) + ")"
	case valChar:
		return fmt.Sprintf("#\\%c", v.cval)
	case valLambda:
		return "#<procedure>"
	default:
		return "<unknown>"
	}
}

// displayStr returns the display representation (no quotes on strings).
func (v value) displayStr() string {
	switch v.kind {
	case valString:
		if v.mstr != nil {
			return string(*v.mstr)
		}
		return v.sval
	case valPair:
		return "(" + displayPairInner(v) + ")"
	default:
		return v.String()
	}
}

func displayPairInner(v value) string {
	var sb strings.Builder
	sb.WriteString(v.pair.car.displayStr())
	cdr := v.pair.cdr
	for cdr.kind == valPair {
		sb.WriteByte(' ')
		sb.WriteString(cdr.pair.car.displayStr())
		cdr = cdr.pair.cdr
	}
	if cdr.kind != valNull {
		sb.WriteString(" . ")
		sb.WriteString(cdr.displayStr())
	}
	return sb.String()
}

func writePairInner(v value) string {
	var sb strings.Builder
	sb.WriteString(v.pair.car.String())
	cdr := v.pair.cdr
	for cdr.kind == valPair {
		sb.WriteByte(' ')
		sb.WriteString(cdr.pair.car.String())
		cdr = cdr.pair.cdr
	}
	if cdr.kind != valNull {
		sb.WriteString(" . ")
		sb.WriteString(cdr.String())
	}
	return sb.String()
}

func isTruthy(v value) bool {
	return !(v.kind == valBoolean && !v.bval)
}

// ---------- Environment ----------

type env struct {
	bindings map[string]value
	parent   *env
	output   *strings.Builder // non-nil only on root env
}

func newEnv(parent *env) *env {
	return &env{bindings: make(map[string]value), parent: parent}
}

func (e *env) get(name string) (value, bool) {
	if v, ok := e.bindings[name]; ok {
		return v, true
	}
	if e.parent != nil {
		return e.parent.get(name)
	}
	return value{}, false
}

func (e *env) set(name string, v value) {
	e.bindings[name] = v
}

// setExisting mutates an existing binding, walking up the chain. Returns false if unbound.
func (e *env) setExisting(name string, v value) bool {
	if _, ok := e.bindings[name]; ok {
		e.bindings[name] = v
		return true
	}
	if e.parent != nil {
		return e.parent.setExisting(name, v)
	}
	return false
}

func (e *env) getOutput() *strings.Builder {
	if e.output != nil {
		return e.output
	}
	if e.parent != nil {
		return e.parent.getOutput()
	}
	return nil
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
	tokQuote
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
	return ch == 0 || ch == '(' || ch == ')' || unicode.IsSpace(ch) || ch == ';' || ch == '"' || ch == '\''
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
	if ch == '\'' {
		t.advance()
		return token{kind: tokQuote, text: "'", line: line, col: col}, nil
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

	if tok.kind == tokQuote {
		p.pos++
		inner, err := p.parseExpr()
		if err != nil {
			return nil, err
		}
		return &expr{
			kind: exprList,
			list: []*expr{
				{kind: exprAtom, atom: symVal("quote"), line: tok.line, col: tok.col},
				inner,
			},
			line: tok.line,
			col:  tok.col,
		}, nil
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
	if text == "#t" {
		return boolVal(true)
	}
	if text == "#f" {
		return boolVal(false)
	}
	// Character literals: #\x, #\space, #\newline, #\tab
	if len(text) >= 3 && text[0] == '#' && text[1] == '\\' {
		name := text[2:]
		switch name {
		case "space":
			return charVal(' ')
		case "newline":
			return charVal('\n')
		case "tab":
			return charVal('\t')
		default:
			runes := []rune(name)
			if len(runes) == 1 {
				return charVal(runes[0])
			}
		}
	}
	if len(text) >= 2 && text[0] == '"' && text[len(text)-1] == '"' {
		return strVal(text[1 : len(text)-1])
	}
	if n, err := strconv.ParseInt(text, 10, 64); err == nil {
		return intVal(n)
	}
	return symVal(text)
}

// ---------- Evaluator ----------

func evalInEnv(e *expr, env *env) (value, error) {
	if e.kind == exprAtom {
		if e.atom.kind == valSymbol {
			if v, ok := env.get(e.atom.sval); ok {
				return v, nil
			}
			return value{}, &EvalError{Message: fmt.Sprintf("%d:%d: unbound variable: %s", e.line, e.col, e.atom.sval)}
		}
		return e.atom, nil
	}

	// List expression
	if len(e.list) == 0 {
		return value{}, &EvalError{Message: fmt.Sprintf("%d:%d: empty application", e.line, e.col)}
	}

	head := e.list[0]
	if head.kind == exprAtom && head.atom.kind == valSymbol {
		switch head.atom.sval {
		case "define":
			return evalDefine(e, env)
		case "set!":
			return evalSetBang(e, env)
		case "if":
			return evalIf(e, env)
		case "quote":
			if len(e.list) != 2 {
				return value{}, &EvalError{Message: fmt.Sprintf("%d:%d: quote: expected 1 argument", e.line, e.col)}
			}
			return quoteExpr(e.list[1]), nil
		case "lambda":
			return evalLambdaForm(e, env)
		case "and":
			return evalAnd(e, env)
		case "or":
			return evalOr(e, env)
		case "let":
			return evalLet(e, env)
		case "begin":
			return evalBegin(e, env)
		case "cond":
			return evalCond(e, env)
		}
	}

	// Check if head is a builtin symbol not in env
	if head.kind == exprAtom && head.atom.kind == valSymbol {
		if _, ok := env.get(head.atom.sval); !ok && isBuiltin(head.atom.sval) {
			// Evaluate args and call builtin
			args := e.list[1:]
			evaledArgs := make([]value, len(args))
			for i, a := range args {
				v, err := evalInEnv(a, env)
				if err != nil {
					return value{}, err
				}
				evaledArgs[i] = v
			}
			return evalBuiltin(head.atom.sval, evaledArgs, e, env)
		}
	}

	// Evaluate operator
	op, err := evalInEnv(head, env)
	if err != nil {
		return value{}, err
	}

	// Lambda call
	if op.kind == valLambda {
		args := e.list[1:]
		evaledArgs := make([]value, len(args))
		for i, a := range args {
			v, err := evalInEnv(a, env)
			if err != nil {
				return value{}, err
			}
			evaledArgs[i] = v
		}
		return callLambda(op.lambda, evaledArgs, e)
	}

	return value{}, &EvalError{Message: fmt.Sprintf("%d:%d: not a procedure: %s", e.line, e.col, op.String())}
}

func evalDefine(e *expr, env *env) (value, error) {
	if len(e.list) < 3 {
		return value{}, &EvalError{Message: fmt.Sprintf("%d:%d: define: bad syntax", e.line, e.col)}
	}
	target := e.list[1]

	// (define (f args...) body...)
	if target.kind == exprList && len(target.list) > 0 {
		nameExpr := target.list[0]
		if nameExpr.kind != exprAtom || nameExpr.atom.kind != valSymbol {
			return value{}, &EvalError{Message: fmt.Sprintf("%d:%d: define: expected symbol", nameExpr.line, nameExpr.col)}
		}
		name := nameExpr.atom.sval
		params := make([]string, len(target.list)-1)
		for i, p := range target.list[1:] {
			if p.kind != exprAtom || p.atom.kind != valSymbol {
				return value{}, &EvalError{Message: fmt.Sprintf("%d:%d: define: expected parameter name", p.line, p.col)}
			}
			params[i] = p.atom.sval
		}
		lam := &lambda{params: params, body: e.list[2:], env: env}
		env.set(name, value{kind: valLambda, lambda: lam})
		return voidVal, nil
	}

	// (define x expr)
	if target.kind != exprAtom || target.atom.kind != valSymbol {
		return value{}, &EvalError{Message: fmt.Sprintf("%d:%d: define: expected symbol", target.line, target.col)}
	}
	if len(e.list) != 3 {
		return value{}, &EvalError{Message: fmt.Sprintf("%d:%d: define: expected 1 expression", e.line, e.col)}
	}
	v, err := evalInEnv(e.list[2], env)
	if err != nil {
		return value{}, err
	}
	env.set(target.atom.sval, v)
	return voidVal, nil
}

func evalSetBang(e *expr, env *env) (value, error) {
	if len(e.list) != 3 {
		return value{}, &EvalError{Message: fmt.Sprintf("%d:%d: set!: bad syntax", e.line, e.col)}
	}
	target := e.list[1]
	if target.kind != exprAtom || target.atom.kind != valSymbol {
		return value{}, &EvalError{Message: fmt.Sprintf("%d:%d: set!: expected symbol", target.line, target.col)}
	}
	v, err := evalInEnv(e.list[2], env)
	if err != nil {
		return value{}, err
	}
	if !env.setExisting(target.atom.sval, v) {
		return value{}, &EvalError{Message: fmt.Sprintf("%d:%d: set!: unbound variable %s", target.line, target.col, target.atom.sval)}
	}
	return voidVal, nil
}

func evalIf(e *expr, env *env) (value, error) {
	if len(e.list) < 3 || len(e.list) > 4 {
		return value{}, &EvalError{Message: fmt.Sprintf("%d:%d: if: expected 2 or 3 arguments", e.line, e.col)}
	}
	cond, err := evalInEnv(e.list[1], env)
	if err != nil {
		return value{}, err
	}
	if isTruthy(cond) {
		return evalInEnv(e.list[2], env)
	}
	if len(e.list) == 4 {
		return evalInEnv(e.list[3], env)
	}
	return voidVal, nil
}

func evalLambdaForm(e *expr, env *env) (value, error) {
	if len(e.list) < 3 {
		return value{}, &EvalError{Message: fmt.Sprintf("%d:%d: lambda: bad syntax", e.line, e.col)}
	}
	paramExpr := e.list[1]
	if paramExpr.kind != exprList {
		return value{}, &EvalError{Message: fmt.Sprintf("%d:%d: lambda: expected parameter list", paramExpr.line, paramExpr.col)}
	}
	params := make([]string, len(paramExpr.list))
	for i, p := range paramExpr.list {
		if p.kind != exprAtom || p.atom.kind != valSymbol {
			return value{}, &EvalError{Message: fmt.Sprintf("%d:%d: lambda: expected parameter name", p.line, p.col)}
		}
		params[i] = p.atom.sval
	}
	lam := &lambda{params: params, body: e.list[2:], env: env}
	return value{kind: valLambda, lambda: lam}, nil
}

func callLambda(lam *lambda, args []value, callExpr *expr) (value, error) {
	if len(args) != len(lam.params) {
		return value{}, &EvalError{Message: fmt.Sprintf("%d:%d: expected %d arguments, got %d", callExpr.line, callExpr.col, len(lam.params), len(args))}
	}
	callEnv := newEnv(lam.env)
	for i, p := range lam.params {
		callEnv.set(p, args[i])
	}
	var result value
	var err error
	for _, bodyExpr := range lam.body {
		result, err = evalInEnv(bodyExpr, callEnv)
		if err != nil {
			return value{}, err
		}
	}
	return result, nil
}

func quoteExpr(e *expr) value {
	if e.kind == exprAtom {
		return e.atom
	}
	// List -> build a proper list from pairs
	result := nullVal
	for i := len(e.list) - 1; i >= 0; i-- {
		result = pairVal(quoteExpr(e.list[i]), result)
	}
	return result
}

func isBuiltin(name string) bool {
	switch name {
	case "+", "-", "*", "/", "<", ">", "=", "<=", ">=", "not",
		"cons", "car", "cdr", "null?", "list", "length", "append",
		"string?", "number?", "boolean?", "pair?", "symbol?", "char?",
		"display", "write", "newline",
		"string-append", "string-length", "substring", "string-ref",
		"string->number", "number->string", "symbol->string", "string->symbol",
		"string-copy", "string-set!":
		return true
	}
	return false
}

func evalBuiltin(name string, args []value, e *expr, environ *env) (value, error) {
	switch name {
	case "+":
		var sum int64 = 0
		for _, v := range args {
			if v.kind != valInteger {
				return value{}, &EvalError{Message: fmt.Sprintf("%d:%d: +: expected number", e.line, e.col)}
			}
			sum += v.ival
		}
		return intVal(sum), nil

	case "-":
		if len(args) == 0 {
			return value{}, &EvalError{Message: fmt.Sprintf("%d:%d: -: expected at least 1 argument", e.line, e.col)}
		}
		if args[0].kind != valInteger {
			return value{}, &EvalError{Message: fmt.Sprintf("%d:%d: -: expected number", e.line, e.col)}
		}
		if len(args) == 1 {
			return intVal(-args[0].ival), nil
		}
		result := args[0].ival
		for _, v := range args[1:] {
			if v.kind != valInteger {
				return value{}, &EvalError{Message: fmt.Sprintf("%d:%d: -: expected number", e.line, e.col)}
			}
			result -= v.ival
		}
		return intVal(result), nil

	case "*":
		var product int64 = 1
		for _, v := range args {
			if v.kind != valInteger {
				return value{}, &EvalError{Message: fmt.Sprintf("%d:%d: *: expected number", e.line, e.col)}
			}
			product *= v.ival
		}
		return intVal(product), nil

	case "/":
		if len(args) < 2 {
			return value{}, &EvalError{Message: fmt.Sprintf("%d:%d: /: expected at least 2 arguments", e.line, e.col)}
		}
		if args[0].kind != valInteger {
			return value{}, &EvalError{Message: fmt.Sprintf("%d:%d: /: expected number", e.line, e.col)}
		}
		result := args[0].ival
		for _, v := range args[1:] {
			if v.kind != valInteger {
				return value{}, &EvalError{Message: fmt.Sprintf("%d:%d: /: expected number", e.line, e.col)}
			}
			if v.ival == 0 {
				return value{}, &EvalError{Message: fmt.Sprintf("%d:%d: /: division by zero", e.line, e.col)}
			}
			result /= v.ival
		}
		return intVal(result), nil

	case "<":
		return evalCompareVals(args, e, func(a, b int64) bool { return a < b }, "<")
	case ">":
		return evalCompareVals(args, e, func(a, b int64) bool { return a > b }, ">")
	case "=":
		return evalCompareVals(args, e, func(a, b int64) bool { return a == b }, "=")
	case "<=":
		return evalCompareVals(args, e, func(a, b int64) bool { return a <= b }, "<=")
	case ">=":
		return evalCompareVals(args, e, func(a, b int64) bool { return a >= b }, ">=")

	case "not":
		if len(args) != 1 {
			return value{}, &EvalError{Message: fmt.Sprintf("%d:%d: not: expected 1 argument, got %d", e.line, e.col, len(args))}
		}
		return boolVal(!isTruthy(args[0])), nil

	case "cons":
		if len(args) != 2 {
			return value{}, &EvalError{Message: fmt.Sprintf("%d:%d: cons: expected 2 arguments, got %d", e.line, e.col, len(args))}
		}
		return pairVal(args[0], args[1]), nil

	case "car":
		if len(args) != 1 {
			return value{}, &EvalError{Message: fmt.Sprintf("%d:%d: car: expected 1 argument, got %d", e.line, e.col, len(args))}
		}
		if args[0].kind != valPair {
			return value{}, &EvalError{Message: fmt.Sprintf("%d:%d: car: expected pair", e.line, e.col)}
		}
		return args[0].pair.car, nil

	case "cdr":
		if len(args) != 1 {
			return value{}, &EvalError{Message: fmt.Sprintf("%d:%d: cdr: expected 1 argument, got %d", e.line, e.col, len(args))}
		}
		if args[0].kind != valPair {
			return value{}, &EvalError{Message: fmt.Sprintf("%d:%d: cdr: expected pair", e.line, e.col)}
		}
		return args[0].pair.cdr, nil

	case "null?":
		if len(args) != 1 {
			return value{}, &EvalError{Message: fmt.Sprintf("%d:%d: null?: expected 1 argument, got %d", e.line, e.col, len(args))}
		}
		return boolVal(args[0].kind == valNull), nil

	case "list":
		result := nullVal
		for i := len(args) - 1; i >= 0; i-- {
			result = pairVal(args[i], result)
		}
		return result, nil

	case "length":
		if len(args) != 1 {
			return value{}, &EvalError{Message: fmt.Sprintf("%d:%d: length: expected 1 argument, got %d", e.line, e.col, len(args))}
		}
		var count int64
		cur := args[0]
		for cur.kind == valPair {
			count++
			cur = cur.pair.cdr
		}
		if cur.kind != valNull {
			return value{}, &EvalError{Message: fmt.Sprintf("%d:%d: length: not a proper list", e.line, e.col)}
		}
		return intVal(count), nil

	case "string?":
		if len(args) != 1 {
			return value{}, &EvalError{Message: fmt.Sprintf("%d:%d: string?: expected 1 argument", e.line, e.col)}
		}
		return boolVal(args[0].kind == valString), nil

	case "number?":
		if len(args) != 1 {
			return value{}, &EvalError{Message: fmt.Sprintf("%d:%d: number?: expected 1 argument", e.line, e.col)}
		}
		return boolVal(args[0].kind == valInteger), nil

	case "boolean?":
		if len(args) != 1 {
			return value{}, &EvalError{Message: fmt.Sprintf("%d:%d: boolean?: expected 1 argument", e.line, e.col)}
		}
		return boolVal(args[0].kind == valBoolean), nil

	case "pair?":
		if len(args) != 1 {
			return value{}, &EvalError{Message: fmt.Sprintf("%d:%d: pair?: expected 1 argument", e.line, e.col)}
		}
		return boolVal(args[0].kind == valPair), nil

	case "symbol?":
		if len(args) != 1 {
			return value{}, &EvalError{Message: fmt.Sprintf("%d:%d: symbol?: expected 1 argument", e.line, e.col)}
		}
		return boolVal(args[0].kind == valSymbol), nil

	case "append":
		if len(args) == 0 {
			return nullVal, nil
		}
		if len(args) == 1 {
			return args[0], nil
		}
		// Append two lists
		result := args[len(args)-1]
		for i := len(args) - 2; i >= 0; i-- {
			lst := args[i]
			// Collect elements of lst
			var elems []value
			cur := lst
			for cur.kind == valPair {
				elems = append(elems, cur.pair.car)
				cur = cur.pair.cdr
			}
			// Build from right
			for j := len(elems) - 1; j >= 0; j-- {
				result = pairVal(elems[j], result)
			}
		}
		return result, nil

	case "char?":
		if len(args) != 1 {
			return value{}, &EvalError{Message: fmt.Sprintf("%d:%d: char?: expected 1 argument", e.line, e.col)}
		}
		return boolVal(args[0].kind == valChar), nil

	case "display":
		if len(args) != 1 {
			return value{}, &EvalError{Message: fmt.Sprintf("%d:%d: display: expected 1 argument", e.line, e.col)}
		}
		if out := environ.getOutput(); out != nil {
			out.WriteString(args[0].displayStr())
		}
		return voidVal, nil

	case "write":
		if len(args) != 1 {
			return value{}, &EvalError{Message: fmt.Sprintf("%d:%d: write: expected 1 argument", e.line, e.col)}
		}
		if out := environ.getOutput(); out != nil {
			out.WriteString(args[0].String())
		}
		return voidVal, nil

	case "newline":
		if len(args) != 0 {
			return value{}, &EvalError{Message: fmt.Sprintf("%d:%d: newline: expected 0 arguments", e.line, e.col)}
		}
		if out := environ.getOutput(); out != nil {
			out.WriteByte('\n')
		}
		return voidVal, nil

	case "string-append":
		var sb strings.Builder
		for _, v := range args {
			if v.kind != valString {
				return value{}, &EvalError{Message: fmt.Sprintf("%d:%d: string-append: expected string", e.line, e.col)}
			}
			sb.WriteString(v.strContent())
		}
		return strVal(sb.String()), nil

	case "string-length":
		if len(args) != 1 {
			return value{}, &EvalError{Message: fmt.Sprintf("%d:%d: string-length: expected 1 argument", e.line, e.col)}
		}
		if args[0].kind != valString {
			return value{}, &EvalError{Message: fmt.Sprintf("%d:%d: string-length: expected string", e.line, e.col)}
		}
		return intVal(int64(len([]rune(args[0].strContent())))), nil

	case "substring":
		if len(args) != 3 {
			return value{}, &EvalError{Message: fmt.Sprintf("%d:%d: substring: expected 3 arguments", e.line, e.col)}
		}
		if args[0].kind != valString || args[1].kind != valInteger || args[2].kind != valInteger {
			return value{}, &EvalError{Message: fmt.Sprintf("%d:%d: substring: invalid argument types", e.line, e.col)}
		}
		runes := []rune(args[0].strContent())
		start, end := int(args[1].ival), int(args[2].ival)
		if start < 0 || end < start || end > len(runes) {
			return value{}, &EvalError{Message: fmt.Sprintf("%d:%d: substring: index out of range", e.line, e.col)}
		}
		return strVal(string(runes[start:end])), nil

	case "string-ref":
		if len(args) != 2 {
			return value{}, &EvalError{Message: fmt.Sprintf("%d:%d: string-ref: expected 2 arguments", e.line, e.col)}
		}
		if args[0].kind != valString || args[1].kind != valInteger {
			return value{}, &EvalError{Message: fmt.Sprintf("%d:%d: string-ref: invalid argument types", e.line, e.col)}
		}
		runes := []rune(args[0].strContent())
		idx := int(args[1].ival)
		if idx < 0 || idx >= len(runes) {
			return value{}, &EvalError{Message: fmt.Sprintf("%d:%d: string-ref: index out of range", e.line, e.col)}
		}
		return charVal(runes[idx]), nil

	case "string->number":
		if len(args) != 1 {
			return value{}, &EvalError{Message: fmt.Sprintf("%d:%d: string->number: expected 1 argument", e.line, e.col)}
		}
		if args[0].kind != valString {
			return value{}, &EvalError{Message: fmt.Sprintf("%d:%d: string->number: expected string", e.line, e.col)}
		}
		n, err := strconv.ParseInt(args[0].strContent(), 10, 64)
		if err != nil {
			return boolVal(false), nil
		}
		return intVal(n), nil

	case "number->string":
		if len(args) != 1 {
			return value{}, &EvalError{Message: fmt.Sprintf("%d:%d: number->string: expected 1 argument", e.line, e.col)}
		}
		if args[0].kind != valInteger {
			return value{}, &EvalError{Message: fmt.Sprintf("%d:%d: number->string: expected number", e.line, e.col)}
		}
		return strVal(strconv.FormatInt(args[0].ival, 10)), nil

	case "symbol->string":
		if len(args) != 1 {
			return value{}, &EvalError{Message: fmt.Sprintf("%d:%d: symbol->string: expected 1 argument", e.line, e.col)}
		}
		if args[0].kind != valSymbol {
			return value{}, &EvalError{Message: fmt.Sprintf("%d:%d: symbol->string: expected symbol", e.line, e.col)}
		}
		return strVal(args[0].sval), nil

	case "string->symbol":
		if len(args) != 1 {
			return value{}, &EvalError{Message: fmt.Sprintf("%d:%d: string->symbol: expected 1 argument", e.line, e.col)}
		}
		if args[0].kind != valString {
			return value{}, &EvalError{Message: fmt.Sprintf("%d:%d: string->symbol: expected string", e.line, e.col)}
		}
		return symVal(args[0].strContent()), nil

	case "string-copy":
		if len(args) != 1 {
			return value{}, &EvalError{Message: fmt.Sprintf("%d:%d: string-copy: expected 1 argument", e.line, e.col)}
		}
		if args[0].kind != valString {
			return value{}, &EvalError{Message: fmt.Sprintf("%d:%d: string-copy: expected string", e.line, e.col)}
		}
		runes := []rune(args[0].strContent())
		return value{kind: valString, mstr: &runes}, nil

	case "string-set!":
		if len(args) != 3 {
			return value{}, &EvalError{Message: fmt.Sprintf("%d:%d: string-set!: expected 3 arguments", e.line, e.col)}
		}
		if args[0].kind != valString || args[0].mstr == nil {
			return value{}, &EvalError{Message: fmt.Sprintf("%d:%d: string-set!: expected mutable string", e.line, e.col)}
		}
		if args[1].kind != valInteger {
			return value{}, &EvalError{Message: fmt.Sprintf("%d:%d: string-set!: expected integer index", e.line, e.col)}
		}
		if args[2].kind != valChar {
			return value{}, &EvalError{Message: fmt.Sprintf("%d:%d: string-set!: expected char", e.line, e.col)}
		}
		idx := int(args[1].ival)
		if idx < 0 || idx >= len(*args[0].mstr) {
			return value{}, &EvalError{Message: fmt.Sprintf("%d:%d: string-set!: index out of range", e.line, e.col)}
		}
		(*args[0].mstr)[idx] = args[2].cval
		return voidVal, nil
	}

	return value{}, &EvalError{Message: fmt.Sprintf("%d:%d: unbound variable: %s", e.line, e.col, name)}
}

func evalCompareVals(args []value, e *expr, cmp func(int64, int64) bool, name string) (value, error) {
	if len(args) < 2 {
		return value{}, &EvalError{Message: fmt.Sprintf("%d:%d: %s: expected at least 2 arguments", e.line, e.col, name)}
	}
	if args[0].kind != valInteger {
		return value{}, &EvalError{Message: fmt.Sprintf("%d:%d: %s: expected number", e.line, e.col, name)}
	}
	prev := args[0]
	for _, v := range args[1:] {
		if v.kind != valInteger {
			return value{}, &EvalError{Message: fmt.Sprintf("%d:%d: %s: expected number", e.line, e.col, name)}
		}
		if !cmp(prev.ival, v.ival) {
			return boolVal(false), nil
		}
		prev = v
	}
	return boolVal(true), nil
}

func evalAnd(e *expr, env *env) (value, error) {
	args := e.list[1:]
	if len(args) == 0 {
		return boolVal(true), nil
	}
	var result value
	for _, a := range args {
		v, err := evalInEnv(a, env)
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

func evalOr(e *expr, env *env) (value, error) {
	args := e.list[1:]
	if len(args) == 0 {
		return boolVal(false), nil
	}
	for _, a := range args {
		v, err := evalInEnv(a, env)
		if err != nil {
			return value{}, err
		}
		if isTruthy(v) {
			return v, nil
		}
	}
	return boolVal(false), nil
}

func evalLet(e *expr, env *env) (value, error) {
	if len(e.list) < 3 {
		return value{}, &EvalError{Message: fmt.Sprintf("%d:%d: let: bad syntax", e.line, e.col)}
	}
	// Named let: (let name ((var init) ...) body...)
	if e.list[1].kind == exprAtom && e.list[1].atom.kind == valSymbol {
		if len(e.list) < 4 {
			return value{}, &EvalError{Message: fmt.Sprintf("%d:%d: let: bad syntax", e.line, e.col)}
		}
		loopName := e.list[1].atom.sval
		bindingsExpr := e.list[2]
		if bindingsExpr.kind != exprList {
			return value{}, &EvalError{Message: fmt.Sprintf("%d:%d: let: expected bindings list", bindingsExpr.line, bindingsExpr.col)}
		}
		params := make([]string, len(bindingsExpr.list))
		initVals := make([]value, len(bindingsExpr.list))
		for i, b := range bindingsExpr.list {
			if b.kind != exprList || len(b.list) != 2 {
				return value{}, &EvalError{Message: fmt.Sprintf("%d:%d: let: bad binding", b.line, b.col)}
			}
			if b.list[0].kind != exprAtom || b.list[0].atom.kind != valSymbol {
				return value{}, &EvalError{Message: fmt.Sprintf("%d:%d: let: expected symbol", b.list[0].line, b.list[0].col)}
			}
			params[i] = b.list[0].atom.sval
			v, err := evalInEnv(b.list[1], env)
			if err != nil {
				return value{}, err
			}
			initVals[i] = v
		}
		// Create a lambda for the loop and bind it in a new env
		letEnv := newEnv(env)
		lam := &lambda{params: params, body: e.list[3:], env: letEnv}
		letEnv.set(loopName, value{kind: valLambda, lambda: lam})
		return callLambda(lam, initVals, e)
	}

	bindingsExpr := e.list[1]
	if bindingsExpr.kind != exprList {
		return value{}, &EvalError{Message: fmt.Sprintf("%d:%d: let: expected bindings list", bindingsExpr.line, bindingsExpr.col)}
	}
	letEnv := newEnv(env)
	for _, b := range bindingsExpr.list {
		if b.kind != exprList || len(b.list) != 2 {
			return value{}, &EvalError{Message: fmt.Sprintf("%d:%d: let: bad binding", b.line, b.col)}
		}
		if b.list[0].kind != exprAtom || b.list[0].atom.kind != valSymbol {
			return value{}, &EvalError{Message: fmt.Sprintf("%d:%d: let: expected symbol", b.list[0].line, b.list[0].col)}
		}
		v, err := evalInEnv(b.list[1], env)
		if err != nil {
			return value{}, err
		}
		letEnv.set(b.list[0].atom.sval, v)
	}
	var result value
	var err error
	for _, bodyExpr := range e.list[2:] {
		result, err = evalInEnv(bodyExpr, letEnv)
		if err != nil {
			return value{}, err
		}
	}
	return result, nil
}

func evalBegin(e *expr, env *env) (value, error) {
	args := e.list[1:]
	if len(args) == 0 {
		return voidVal, nil
	}
	var result value
	var err error
	for _, a := range args {
		result, err = evalInEnv(a, env)
		if err != nil {
			return value{}, err
		}
	}
	return result, nil
}

func evalCond(e *expr, env *env) (value, error) {
	clauses := e.list[1:]
	for _, clause := range clauses {
		if clause.kind != exprList || len(clause.list) < 2 {
			return value{}, &EvalError{Message: fmt.Sprintf("%d:%d: cond: bad clause", clause.line, clause.col)}
		}
		// else clause
		if clause.list[0].kind == exprAtom && clause.list[0].atom.kind == valSymbol && clause.list[0].atom.sval == "else" {
			var result value
			var err error
			for _, bodyExpr := range clause.list[1:] {
				result, err = evalInEnv(bodyExpr, env)
				if err != nil {
					return value{}, err
				}
			}
			return result, nil
		}
		cond, err := evalInEnv(clause.list[0], env)
		if err != nil {
			return value{}, err
		}
		if isTruthy(cond) {
			var result value
			for _, bodyExpr := range clause.list[1:] {
				result, err = evalInEnv(bodyExpr, env)
				if err != nil {
					return value{}, err
				}
			}
			return result, nil
		}
	}
	return voidVal, nil
}

// ---------- Public API ----------

func makeTopLevelEnv() *env {
	e := newEnv(nil)
	// Register builtins as lambda-like values would be complex;
	// instead we resolve them at call time via the environment lookup
	// falling through to isBuiltin check.
	return e
}

func evalWithEnv(input string, environ *env) (string, error) {
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
		v, err := evalInEnv(e, environ)
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

func EvalStr(input string) (string, error) {
	environ := makeTopLevelEnv()
	environ.output = &strings.Builder{} // output captured but discarded
	return evalWithEnv(input, environ)
}

func EvalStrWithOutput(input string) (result string, output string, err error) {
	environ := makeTopLevelEnv()
	var buf strings.Builder
	environ.output = &buf
	r, err := evalWithEnv(input, environ)
	return r, buf.String(), err
}
