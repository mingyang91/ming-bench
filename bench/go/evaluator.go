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
	pair   *pair
	lambda *lambda
}

var voidVal = value{kind: valVoid}
var nullVal = value{kind: valNull}

func intVal(n int64) value  { return value{kind: valInteger, ival: n} }
func boolVal(b bool) value  { return value{kind: valBoolean, bval: b} }
func strVal(s string) value { return value{kind: valString, sval: s} }
func symVal(s string) value { return value{kind: valSymbol, sval: s} }
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
		return `"` + v.sval + `"`
	case valSymbol:
		return v.sval
	case valNull:
		return "()"
	case valVoid:
		return ""
	case valPair:
		return "(" + writePairInner(v) + ")"
	case valLambda:
		return "#<procedure>"
	default:
		return "<unknown>"
	}
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
			return evalBuiltin(head.atom.sval, evaledArgs, e)
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
	case "+", "-", "*", "/", "<", ">", "=", "<=", ">=", "not":
		return true
	}
	return false
}

func evalBuiltin(name string, args []value, e *expr) (value, error) {
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

// ---------- Public API ----------

func makeTopLevelEnv() *env {
	e := newEnv(nil)
	// Register builtins as lambda-like values would be complex;
	// instead we resolve them at call time via the environment lookup
	// falling through to isBuiltin check.
	return e
}

func EvalStr(input string) (string, error) {
	tokens, err := tokenize(input)
	if err != nil {
		return "", err
	}
	p := &parser{tokens: tokens}
	env := makeTopLevelEnv()

	var lastVal value
	hasResult := false
	for p.peek().kind != tokEOF {
		e, err := p.parseExpr()
		if err != nil {
			return "", err
		}
		v, err := evalInEnv(e, env)
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

func EvalStrWithOutput(input string) (result string, output string, err error) {
	r, err := EvalStr(input)
	return r, "", err
}
