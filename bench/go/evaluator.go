package ming

import (
	"fmt"
	"math"
	"strconv"
	"strings"
	"sync"
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
	valBuiltin
	valMacro
	valRational
	valFloat
	valRecord
	valCaseLambda
)

type pair struct {
	car value
	cdr value
}

type lambda struct {
	params    []string
	restParam string // variadic rest parameter (empty if none)
	body      []*expr
	env       *env
}

type syntaxRule struct {
	pattern  []*expr // pattern elements after the macro name
	template *expr
}

type macro struct {
	name     string
	literals []string
	rules    []syntaxRule
	defEnv   *env // definition-site environment for hygiene
}

type recordType struct {
	name string
}

type record struct {
	rtype  *recordType
	fields []value
}

type value struct {
	kind   valueKind
	ival   int64
	bval   bool
	sval   string
	cval   rune
	fval   float64
	numer  int64 // rational numerator
	denom  int64 // rational denominator (always > 0)
	pair   *pair
	lambda *lambda
	macro  *macro
	mstr   *[]rune // mutable string buffer (set by string-copy)
	rec     *record
	clauses []*lambda // case-lambda clauses
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
func floatVal(f float64) value { return value{kind: valFloat, fval: f} }

func gcd(a, b int64) int64 {
	if a < 0 {
		a = -a
	}
	if b < 0 {
		b = -b
	}
	for b != 0 {
		a, b = b, a%b
	}
	return a
}

// ratOrIntVal creates a rational value, simplifying to integer if denom==1.
func ratOrIntVal(n, d int64) value {
	if d == 0 {
		panic("zero denominator")
	}
	if d < 0 {
		n, d = -n, -d
	}
	g := gcd(n, d)
	n, d = n/g, d/g
	if d == 1 {
		return intVal(n)
	}
	return value{kind: valRational, numer: n, denom: d}
}

func isNumber(v value) bool {
	return v.kind == valInteger || v.kind == valRational || v.kind == valFloat
}

// toRational converts an integer or rational to (numer, denom).
func toRational(v value) (int64, int64) {
	switch v.kind {
	case valInteger:
		return v.ival, 1
	case valRational:
		return v.numer, v.denom
	default:
		panic("toRational on non-exact")
	}
}

func toFloat64(v value) float64 {
	switch v.kind {
	case valInteger:
		return float64(v.ival)
	case valRational:
		return float64(v.numer) / float64(v.denom)
	case valFloat:
		return v.fval
	default:
		return 0
	}
}

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
		switch v.cval {
		case ' ':
			return "#\\space"
		case '\n':
			return "#\\newline"
		case '\t':
			return "#\\tab"
		default:
			return fmt.Sprintf("#\\%c", v.cval)
		}
	case valRational:
		return fmt.Sprintf("%d/%d", v.numer, v.denom)
	case valFloat:
		s := strconv.FormatFloat(v.fval, 'f', -1, 64)
		// Ensure there's a decimal point
		if !strings.Contains(s, ".") {
			s += ".0"
		}
		return s
	case valRecord:
		return fmt.Sprintf("#<record:%s>", v.rec.rtype.name)
	case valLambda:
		return "#<procedure>"
	case valBuiltin:
		return "#<procedure>"
	case valCaseLambda:
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
	// Rational literal: digits/digits (e.g., 1/3, -3/4)
	if idx := strings.Index(text, "/"); idx > 0 && idx < len(text)-1 {
		nStr, dStr := text[:idx], text[idx+1:]
		if n, err := strconv.ParseInt(nStr, 10, 64); err == nil {
			if d, err2 := strconv.ParseInt(dStr, 10, 64); err2 == nil && d != 0 {
				return ratOrIntVal(n, d)
			}
		}
	}
	// Float literal
	if f, err := strconv.ParseFloat(text, 64); err == nil {
		if strings.ContainsAny(text, ".eE") {
			return floatVal(f)
		}
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
		case "case-lambda":
			return evalCaseLambda(e, env)
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
		case "define-syntax":
			return evalDefineSyntax(e, env)
		case "define-record-type":
			return evalDefineRecordType(e, env)
		}
	}

	// Check for macro application
	if head.kind == exprAtom && head.atom.kind == valSymbol {
		if v, ok := env.get(head.atom.sval); ok && v.kind == valMacro {
			expanded, err := expandMacro(v.macro, e, env)
			if err != nil {
				return value{}, err
			}
			return evalInEnv(expanded, env)
		}
	}

	// Evaluate operator
	op, err := evalInEnv(head, env)
	if err != nil {
		return value{}, err
	}

	// Evaluate arguments
	args := e.list[1:]
	evaledArgs := make([]value, len(args))
	for i, a := range args {
		v, err := evalInEnv(a, env)
		if err != nil {
			return value{}, err
		}
		evaledArgs[i] = v
	}

	return callValue(op, evaledArgs, e, env)
}

func callValue(op value, args []value, callExpr *expr, environ *env) (value, error) {
	switch op.kind {
	case valLambda:
		return callLambda(op.lambda, args, callExpr)
	case valCaseLambda:
		return callCaseLambda(op.clauses, args, callExpr)
	case valBuiltin:
		if op.sval == "__native" {
			nativeFuncsMu.Lock()
			fn := nativeFuncs[int(op.ival)]
			nativeFuncsMu.Unlock()
			return fn(args)
		}
		if op.sval == "apply" {
			return evalApply(args, callExpr, environ)
		}
		return evalBuiltin(op.sval, args, callExpr, environ)
	default:
		return value{}, &EvalError{Message: fmt.Sprintf("%d:%d: not a procedure: %s", callExpr.line, callExpr.col, op.String())}
	}
}

func evalApply(args []value, e *expr, environ *env) (value, error) {
	if len(args) < 2 {
		return value{}, &EvalError{Message: fmt.Sprintf("%d:%d: apply: expected at least 2 arguments", e.line, e.col)}
	}
	fn := args[0]
	// Last argument must be a list; prefix args come before it
	lastArg := args[len(args)-1]
	// Collect prefix args
	var allArgs []value
	for _, a := range args[1 : len(args)-1] {
		allArgs = append(allArgs, a)
	}
	// Flatten the last argument (must be a list)
	cur := lastArg
	for cur.kind == valPair {
		allArgs = append(allArgs, cur.pair.car)
		cur = cur.pair.cdr
	}
	if cur.kind != valNull {
		return value{}, &EvalError{Message: fmt.Sprintf("%d:%d: apply: last argument must be a proper list", e.line, e.col)}
	}
	return callValue(fn, allArgs, e, environ)
}

func evalDefine(e *expr, env *env) (value, error) {
	if len(e.list) < 3 {
		return value{}, &EvalError{Message: fmt.Sprintf("%d:%d: define: bad syntax", e.line, e.col)}
	}
	target := e.list[1]

	// (define (f args...) body...) or (define (f args . rest) body...)
	if target.kind == exprList && len(target.list) > 0 {
		nameExpr := target.list[0]
		if nameExpr.kind != exprAtom || nameExpr.atom.kind != valSymbol {
			return value{}, &EvalError{Message: fmt.Sprintf("%d:%d: define: expected symbol", nameExpr.line, nameExpr.col)}
		}
		name := nameExpr.atom.sval
		params, restParam, err := parseDottedParams(target.list[1:], e)
		if err != nil {
			return value{}, err
		}
		lam := &lambda{params: params, restParam: restParam, body: e.list[2:], env: env}
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
	// (lambda args body) — single symbol means all-rest
	if paramExpr.kind == exprAtom && paramExpr.atom.kind == valSymbol {
		lam := &lambda{restParam: paramExpr.atom.sval, body: e.list[2:], env: env}
		return value{kind: valLambda, lambda: lam}, nil
	}
	if paramExpr.kind != exprList {
		return value{}, &EvalError{Message: fmt.Sprintf("%d:%d: lambda: expected parameter list", paramExpr.line, paramExpr.col)}
	}
	params, restParam, err := parseDottedParams(paramExpr.list, e)
	if err != nil {
		return value{}, err
	}
	lam := &lambda{params: params, restParam: restParam, body: e.list[2:], env: env}
	return value{kind: valLambda, lambda: lam}, nil
}

func evalCaseLambda(e *expr, env *env) (value, error) {
	// (case-lambda (params body...) ...)
	var clauses []*lambda
	for _, clause := range e.list[1:] {
		if clause.kind != exprList || len(clause.list) < 2 {
			return value{}, &EvalError{Message: fmt.Sprintf("%d:%d: case-lambda: bad clause", clause.line, clause.col)}
		}
		paramExpr := clause.list[0]
		if paramExpr.kind != exprList {
			return value{}, &EvalError{Message: fmt.Sprintf("%d:%d: case-lambda: expected parameter list", paramExpr.line, paramExpr.col)}
		}
		params, restParam, err := parseDottedParams(paramExpr.list, e)
		if err != nil {
			return value{}, err
		}
		clauses = append(clauses, &lambda{params: params, restParam: restParam, body: clause.list[1:], env: env})
	}
	return value{kind: valCaseLambda, clauses: clauses}, nil
}

func callCaseLambda(clauses []*lambda, args []value, callExpr *expr) (value, error) {
	for _, lam := range clauses {
		if lam.restParam != "" {
			if len(args) >= len(lam.params) {
				return callLambda(lam, args, callExpr)
			}
		} else {
			if len(args) == len(lam.params) {
				return callLambda(lam, args, callExpr)
			}
		}
	}
	return value{}, &EvalError{Message: fmt.Sprintf("%d:%d: case-lambda: no matching clause for %d arguments", callExpr.line, callExpr.col, len(args))}
}

// parseDottedParams parses a parameter list that may contain dot notation: (a b . rest)
func parseDottedParams(plist []*expr, e *expr) ([]string, string, error) {
	var params []string
	var restParam string
	for i, p := range plist {
		if p.kind == exprAtom && p.atom.kind == valSymbol && p.atom.sval == "." {
			// Next element is the rest param
			if i+1 >= len(plist) || i+2 != len(plist) {
				return nil, "", &EvalError{Message: fmt.Sprintf("%d:%d: bad dot in parameter list", p.line, p.col)}
			}
			rp := plist[i+1]
			if rp.kind != exprAtom || rp.atom.kind != valSymbol {
				return nil, "", &EvalError{Message: fmt.Sprintf("%d:%d: expected symbol after dot", rp.line, rp.col)}
			}
			restParam = rp.atom.sval
			return params, restParam, nil
		}
		if p.kind != exprAtom || p.atom.kind != valSymbol {
			return nil, "", &EvalError{Message: fmt.Sprintf("%d:%d: expected parameter name", p.line, p.col)}
		}
		params = append(params, p.atom.sval)
	}
	return params, restParam, nil
}

func callLambda(lam *lambda, args []value, callExpr *expr) (value, error) {
	if lam.restParam != "" {
		if len(args) < len(lam.params) {
			return value{}, &EvalError{Message: fmt.Sprintf("%d:%d: expected at least %d arguments, got %d", callExpr.line, callExpr.col, len(lam.params), len(args))}
		}
	} else {
		if len(args) != len(lam.params) {
			return value{}, &EvalError{Message: fmt.Sprintf("%d:%d: expected %d arguments, got %d", callExpr.line, callExpr.col, len(lam.params), len(args))}
		}
	}
	callEnv := newEnv(lam.env)
	for i, p := range lam.params {
		callEnv.set(p, args[i])
	}
	if lam.restParam != "" {
		// Collect remaining args into a list
		rest := nullVal
		for i := len(args) - 1; i >= len(lam.params); i-- {
			rest = pairVal(args[i], rest)
		}
		callEnv.set(lam.restParam, rest)
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
		"string-copy", "string-set!",
		"abs", "modulo", "remainder", "quotient", "min", "max", "expt",
		"zero?", "positive?", "negative?", "odd?", "even?",
		"list-ref", "list-tail", "list?", "assoc", "map",
		"char=?", "char<?", "char-alphabetic?", "char-numeric?", "char-upcase", "char-downcase",
		"string=?", "string<?", "string-ci=?", "string-upcase", "string-downcase",
		"eq?", "equal?",
		"integer?", "rational?", "exact?", "inexact?",
		"exact->inexact", "inexact->exact",
		"numerator", "denominator",
		"procedure?":
		return true
	}
	return false
}

func evalBuiltin(name string, args []value, e *expr, environ *env) (value, error) {
	switch name {
	case "+":
		result := intVal(0)
		for _, v := range args {
			if !isNumber(v) {
				return value{}, &EvalError{Message: fmt.Sprintf("%d:%d: +: expected number", e.line, e.col)}
			}
			result = numAdd(result, v)
		}
		return result, nil

	case "-":
		if len(args) == 0 {
			return value{}, &EvalError{Message: fmt.Sprintf("%d:%d: -: expected at least 1 argument", e.line, e.col)}
		}
		if !isNumber(args[0]) {
			return value{}, &EvalError{Message: fmt.Sprintf("%d:%d: -: expected number", e.line, e.col)}
		}
		if len(args) == 1 {
			return numNeg(args[0]), nil
		}
		result := args[0]
		for _, v := range args[1:] {
			if !isNumber(v) {
				return value{}, &EvalError{Message: fmt.Sprintf("%d:%d: -: expected number", e.line, e.col)}
			}
			result = numSub(result, v)
		}
		return result, nil

	case "*":
		result := intVal(1)
		for _, v := range args {
			if !isNumber(v) {
				return value{}, &EvalError{Message: fmt.Sprintf("%d:%d: *: expected number", e.line, e.col)}
			}
			result = numMul(result, v)
		}
		return result, nil

	case "/":
		if len(args) < 2 {
			return value{}, &EvalError{Message: fmt.Sprintf("%d:%d: /: expected at least 2 arguments", e.line, e.col)}
		}
		if !isNumber(args[0]) {
			return value{}, &EvalError{Message: fmt.Sprintf("%d:%d: /: expected number", e.line, e.col)}
		}
		result := args[0]
		for _, v := range args[1:] {
			if !isNumber(v) {
				return value{}, &EvalError{Message: fmt.Sprintf("%d:%d: /: expected number", e.line, e.col)}
			}
			if numIsZero(v) {
				return value{}, &EvalError{Message: fmt.Sprintf("%d:%d: /: division by zero", e.line, e.col)}
			}
			result = numDiv(result, v)
		}
		return result, nil

	case "<":
		return evalNumCompare(args, e, func(a, b float64) bool { return a < b }, "<")
	case ">":
		return evalNumCompare(args, e, func(a, b float64) bool { return a > b }, ">")
	case "=":
		return evalNumCompare(args, e, func(a, b float64) bool { return a == b }, "=")
	case "<=":
		return evalNumCompare(args, e, func(a, b float64) bool { return a <= b }, "<=")
	case ">=":
		return evalNumCompare(args, e, func(a, b float64) bool { return a >= b }, ">=")

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
		return boolVal(isNumber(args[0])), nil

	case "integer?":
		if len(args) != 1 {
			return value{}, &EvalError{Message: fmt.Sprintf("%d:%d: integer?: expected 1 argument", e.line, e.col)}
		}
		switch args[0].kind {
		case valInteger:
			return boolVal(true), nil
		case valRational:
			// 4/2 simplifies to integer, but if it's still rational, it's not integer
			return boolVal(false), nil
		case valFloat:
			f := args[0].fval
			return boolVal(f == math.Trunc(f) && !math.IsInf(f, 0) && !math.IsNaN(f)), nil
		default:
			return boolVal(false), nil
		}

	case "rational?":
		if len(args) != 1 {
			return value{}, &EvalError{Message: fmt.Sprintf("%d:%d: rational?: expected 1 argument", e.line, e.col)}
		}
		return boolVal(args[0].kind == valInteger || args[0].kind == valRational), nil

	case "exact?":
		if len(args) != 1 {
			return value{}, &EvalError{Message: fmt.Sprintf("%d:%d: exact?: expected 1 argument", e.line, e.col)}
		}
		return boolVal(args[0].kind == valInteger || args[0].kind == valRational), nil

	case "inexact?":
		if len(args) != 1 {
			return value{}, &EvalError{Message: fmt.Sprintf("%d:%d: inexact?: expected 1 argument", e.line, e.col)}
		}
		return boolVal(args[0].kind == valFloat), nil

	case "exact->inexact":
		if len(args) != 1 || !isNumber(args[0]) {
			return value{}, &EvalError{Message: fmt.Sprintf("%d:%d: exact->inexact: expected 1 number", e.line, e.col)}
		}
		return floatVal(toFloat64(args[0])), nil

	case "inexact->exact":
		if len(args) != 1 || !isNumber(args[0]) {
			return value{}, &EvalError{Message: fmt.Sprintf("%d:%d: inexact->exact: expected 1 number", e.line, e.col)}
		}
		if args[0].kind == valInteger || args[0].kind == valRational {
			return args[0], nil
		}
		// Convert float to rational via continued fraction approximation
		return floatToExact(args[0].fval), nil

	case "numerator":
		if len(args) != 1 {
			return value{}, &EvalError{Message: fmt.Sprintf("%d:%d: numerator: expected 1 argument", e.line, e.col)}
		}
		switch args[0].kind {
		case valInteger:
			return args[0], nil
		case valRational:
			return intVal(args[0].numer), nil
		default:
			return value{}, &EvalError{Message: fmt.Sprintf("%d:%d: numerator: expected rational", e.line, e.col)}
		}

	case "denominator":
		if len(args) != 1 {
			return value{}, &EvalError{Message: fmt.Sprintf("%d:%d: denominator: expected 1 argument", e.line, e.col)}
		}
		switch args[0].kind {
		case valInteger:
			return intVal(1), nil
		case valRational:
			return intVal(args[0].denom), nil
		default:
			return value{}, &EvalError{Message: fmt.Sprintf("%d:%d: denominator: expected rational", e.line, e.col)}
		}

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

	case "procedure?":
		if len(args) != 1 {
			return value{}, &EvalError{Message: fmt.Sprintf("%d:%d: procedure?: expected 1 argument", e.line, e.col)}
		}
		k := args[0].kind
		return boolVal(k == valLambda || k == valBuiltin || k == valCaseLambda), nil

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
		s := args[0].strContent()
		if n, err := strconv.ParseInt(s, 10, 64); err == nil {
			return intVal(n), nil
		}
		if f, err := strconv.ParseFloat(s, 64); err == nil {
			return floatVal(f), nil
		}
		return boolVal(false), nil

	case "number->string":
		if len(args) != 1 {
			return value{}, &EvalError{Message: fmt.Sprintf("%d:%d: number->string: expected 1 argument", e.line, e.col)}
		}
		if !isNumber(args[0]) {
			return value{}, &EvalError{Message: fmt.Sprintf("%d:%d: number->string: expected number", e.line, e.col)}
		}
		return strVal(args[0].String()), nil

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

	// --- L09 Numeric utilities ---

	case "abs":
		if len(args) != 1 || args[0].kind != valInteger {
			return value{}, &EvalError{Message: fmt.Sprintf("%d:%d: abs: expected 1 number", e.line, e.col)}
		}
		n := args[0].ival
		if n < 0 {
			n = -n
		}
		return intVal(n), nil

	case "modulo":
		if len(args) != 2 || args[0].kind != valInteger || args[1].kind != valInteger {
			return value{}, &EvalError{Message: fmt.Sprintf("%d:%d: modulo: expected 2 numbers", e.line, e.col)}
		}
		if args[1].ival == 0 {
			return value{}, &EvalError{Message: fmt.Sprintf("%d:%d: modulo: division by zero", e.line, e.col)}
		}
		a, b := args[0].ival, args[1].ival
		r := a % b
		if r != 0 && (r < 0) != (b < 0) {
			r += b
		}
		return intVal(r), nil

	case "remainder":
		if len(args) != 2 || args[0].kind != valInteger || args[1].kind != valInteger {
			return value{}, &EvalError{Message: fmt.Sprintf("%d:%d: remainder: expected 2 numbers", e.line, e.col)}
		}
		if args[1].ival == 0 {
			return value{}, &EvalError{Message: fmt.Sprintf("%d:%d: remainder: division by zero", e.line, e.col)}
		}
		return intVal(args[0].ival % args[1].ival), nil

	case "quotient":
		if len(args) != 2 || args[0].kind != valInteger || args[1].kind != valInteger {
			return value{}, &EvalError{Message: fmt.Sprintf("%d:%d: quotient: expected 2 numbers", e.line, e.col)}
		}
		if args[1].ival == 0 {
			return value{}, &EvalError{Message: fmt.Sprintf("%d:%d: quotient: division by zero", e.line, e.col)}
		}
		return intVal(args[0].ival / args[1].ival), nil

	case "min":
		if len(args) == 0 {
			return value{}, &EvalError{Message: fmt.Sprintf("%d:%d: min: expected at least 1 argument", e.line, e.col)}
		}
		if args[0].kind != valInteger {
			return value{}, &EvalError{Message: fmt.Sprintf("%d:%d: min: expected number", e.line, e.col)}
		}
		result := args[0].ival
		for _, v := range args[1:] {
			if v.kind != valInteger {
				return value{}, &EvalError{Message: fmt.Sprintf("%d:%d: min: expected number", e.line, e.col)}
			}
			if v.ival < result {
				result = v.ival
			}
		}
		return intVal(result), nil

	case "max":
		if len(args) == 0 {
			return value{}, &EvalError{Message: fmt.Sprintf("%d:%d: max: expected at least 1 argument", e.line, e.col)}
		}
		if args[0].kind != valInteger {
			return value{}, &EvalError{Message: fmt.Sprintf("%d:%d: max: expected number", e.line, e.col)}
		}
		result := args[0].ival
		for _, v := range args[1:] {
			if v.kind != valInteger {
				return value{}, &EvalError{Message: fmt.Sprintf("%d:%d: max: expected number", e.line, e.col)}
			}
			if v.ival > result {
				result = v.ival
			}
		}
		return intVal(result), nil

	case "expt":
		if len(args) != 2 || args[0].kind != valInteger || args[1].kind != valInteger {
			return value{}, &EvalError{Message: fmt.Sprintf("%d:%d: expt: expected 2 numbers", e.line, e.col)}
		}
		base, exp := args[0].ival, args[1].ival
		var result int64 = 1
		if exp < 0 {
			return intVal(0), nil
		}
		for i := int64(0); i < exp; i++ {
			result *= base
		}
		return intVal(result), nil

	case "zero?":
		if len(args) != 1 || args[0].kind != valInteger {
			return value{}, &EvalError{Message: fmt.Sprintf("%d:%d: zero?: expected 1 number", e.line, e.col)}
		}
		return boolVal(args[0].ival == 0), nil

	case "positive?":
		if len(args) != 1 || args[0].kind != valInteger {
			return value{}, &EvalError{Message: fmt.Sprintf("%d:%d: positive?: expected 1 number", e.line, e.col)}
		}
		return boolVal(args[0].ival > 0), nil

	case "negative?":
		if len(args) != 1 || args[0].kind != valInteger {
			return value{}, &EvalError{Message: fmt.Sprintf("%d:%d: negative?: expected 1 number", e.line, e.col)}
		}
		return boolVal(args[0].ival < 0), nil

	case "odd?":
		if len(args) != 1 || args[0].kind != valInteger {
			return value{}, &EvalError{Message: fmt.Sprintf("%d:%d: odd?: expected 1 number", e.line, e.col)}
		}
		return boolVal(args[0].ival%2 != 0), nil

	case "even?":
		if len(args) != 1 || args[0].kind != valInteger {
			return value{}, &EvalError{Message: fmt.Sprintf("%d:%d: even?: expected 1 number", e.line, e.col)}
		}
		return boolVal(args[0].ival%2 == 0), nil

	// --- L09 List utilities ---

	case "list-ref":
		if len(args) != 2 || args[1].kind != valInteger {
			return value{}, &EvalError{Message: fmt.Sprintf("%d:%d: list-ref: expected list and integer", e.line, e.col)}
		}
		idx := args[1].ival
		cur := args[0]
		for i := int64(0); i < idx; i++ {
			if cur.kind != valPair {
				return value{}, &EvalError{Message: fmt.Sprintf("%d:%d: list-ref: index out of range", e.line, e.col)}
			}
			cur = cur.pair.cdr
		}
		if cur.kind != valPair {
			return value{}, &EvalError{Message: fmt.Sprintf("%d:%d: list-ref: index out of range", e.line, e.col)}
		}
		return cur.pair.car, nil

	case "list-tail":
		if len(args) != 2 || args[1].kind != valInteger {
			return value{}, &EvalError{Message: fmt.Sprintf("%d:%d: list-tail: expected list and integer", e.line, e.col)}
		}
		idx := args[1].ival
		cur := args[0]
		for i := int64(0); i < idx; i++ {
			if cur.kind != valPair {
				return value{}, &EvalError{Message: fmt.Sprintf("%d:%d: list-tail: index out of range", e.line, e.col)}
			}
			cur = cur.pair.cdr
		}
		return cur, nil

	case "list?":
		if len(args) != 1 {
			return value{}, &EvalError{Message: fmt.Sprintf("%d:%d: list?: expected 1 argument", e.line, e.col)}
		}
		cur := args[0]
		for cur.kind == valPair {
			cur = cur.pair.cdr
		}
		return boolVal(cur.kind == valNull), nil

	case "assoc":
		if len(args) != 2 {
			return value{}, &EvalError{Message: fmt.Sprintf("%d:%d: assoc: expected 2 arguments", e.line, e.col)}
		}
		key := args[0]
		cur := args[1]
		for cur.kind == valPair {
			entry := cur.pair.car
			if entry.kind == valPair && valuesEqual(key, entry.pair.car) {
				return entry, nil
			}
			cur = cur.pair.cdr
		}
		return boolVal(false), nil

	case "map":
		if len(args) < 2 {
			return value{}, &EvalError{Message: fmt.Sprintf("%d:%d: map: expected at least 2 arguments", e.line, e.col)}
		}
		fn := args[0]
		lists := args[1:]
		result := nullVal
		var results []value
		for {
			// Check if any list is exhausted
			mapArgs := make([]value, len(lists))
			done := false
			for i, lst := range lists {
				if lst.kind != valPair {
					done = true
					break
				}
				mapArgs[i] = lst.pair.car
			}
			if done {
				break
			}
			v, err := callValue(fn, mapArgs, e, environ)
			if err != nil {
				return value{}, err
			}
			results = append(results, v)
			// Advance all lists
			for i, lst := range lists {
				lists[i] = lst.pair.cdr
			}
		}
		for i := len(results) - 1; i >= 0; i-- {
			result = pairVal(results[i], result)
		}
		return result, nil

	// --- L09 Character utilities ---

	case "char=?":
		if len(args) != 2 || args[0].kind != valChar || args[1].kind != valChar {
			return value{}, &EvalError{Message: fmt.Sprintf("%d:%d: char=?: expected 2 chars", e.line, e.col)}
		}
		return boolVal(args[0].cval == args[1].cval), nil

	case "char<?":
		if len(args) != 2 || args[0].kind != valChar || args[1].kind != valChar {
			return value{}, &EvalError{Message: fmt.Sprintf("%d:%d: char<?: expected 2 chars", e.line, e.col)}
		}
		return boolVal(args[0].cval < args[1].cval), nil

	case "char-alphabetic?":
		if len(args) != 1 || args[0].kind != valChar {
			return value{}, &EvalError{Message: fmt.Sprintf("%d:%d: char-alphabetic?: expected 1 char", e.line, e.col)}
		}
		return boolVal(unicode.IsLetter(args[0].cval)), nil

	case "char-numeric?":
		if len(args) != 1 || args[0].kind != valChar {
			return value{}, &EvalError{Message: fmt.Sprintf("%d:%d: char-numeric?: expected 1 char", e.line, e.col)}
		}
		return boolVal(unicode.IsDigit(args[0].cval)), nil

	case "char-upcase":
		if len(args) != 1 || args[0].kind != valChar {
			return value{}, &EvalError{Message: fmt.Sprintf("%d:%d: char-upcase: expected 1 char", e.line, e.col)}
		}
		return charVal(unicode.ToUpper(args[0].cval)), nil

	case "char-downcase":
		if len(args) != 1 || args[0].kind != valChar {
			return value{}, &EvalError{Message: fmt.Sprintf("%d:%d: char-downcase: expected 1 char", e.line, e.col)}
		}
		return charVal(unicode.ToLower(args[0].cval)), nil

	// --- L09 String utilities ---

	case "string=?":
		if len(args) != 2 || args[0].kind != valString || args[1].kind != valString {
			return value{}, &EvalError{Message: fmt.Sprintf("%d:%d: string=?: expected 2 strings", e.line, e.col)}
		}
		return boolVal(args[0].strContent() == args[1].strContent()), nil

	case "string<?":
		if len(args) != 2 || args[0].kind != valString || args[1].kind != valString {
			return value{}, &EvalError{Message: fmt.Sprintf("%d:%d: string<?: expected 2 strings", e.line, e.col)}
		}
		return boolVal(args[0].strContent() < args[1].strContent()), nil

	case "string-ci=?":
		if len(args) != 2 || args[0].kind != valString || args[1].kind != valString {
			return value{}, &EvalError{Message: fmt.Sprintf("%d:%d: string-ci=?: expected 2 strings", e.line, e.col)}
		}
		return boolVal(strings.EqualFold(args[0].strContent(), args[1].strContent())), nil

	case "string-upcase":
		if len(args) != 1 || args[0].kind != valString {
			return value{}, &EvalError{Message: fmt.Sprintf("%d:%d: string-upcase: expected 1 string", e.line, e.col)}
		}
		return strVal(strings.ToUpper(args[0].strContent())), nil

	case "string-downcase":
		if len(args) != 1 || args[0].kind != valString {
			return value{}, &EvalError{Message: fmt.Sprintf("%d:%d: string-downcase: expected 1 string", e.line, e.col)}
		}
		return strVal(strings.ToLower(args[0].strContent())), nil

	// --- L09 Equality ---

	case "eq?":
		if len(args) != 2 {
			return value{}, &EvalError{Message: fmt.Sprintf("%d:%d: eq?: expected 2 arguments", e.line, e.col)}
		}
		return boolVal(valuesEq(args[0], args[1])), nil

	case "equal?":
		if len(args) != 2 {
			return value{}, &EvalError{Message: fmt.Sprintf("%d:%d: equal?: expected 2 arguments", e.line, e.col)}
		}
		return boolVal(valuesEqual(args[0], args[1])), nil
	}

	return value{}, &EvalError{Message: fmt.Sprintf("%d:%d: unbound variable: %s", e.line, e.col, name)}
}

// valuesEq implements eq? — identity comparison (same object or same atomic value).
func valuesEq(a, b value) bool {
	if a.kind != b.kind {
		return false
	}
	switch a.kind {
	case valInteger:
		return a.ival == b.ival
	case valRational:
		return a.numer == b.numer && a.denom == b.denom
	case valFloat:
		return a.fval == b.fval
	case valBoolean:
		return a.bval == b.bval
	case valSymbol:
		return a.sval == b.sval
	case valChar:
		return a.cval == b.cval
	case valNull:
		return true
	case valPair:
		return a.pair == b.pair
	case valString:
		return a.mstr != nil && b.mstr != nil && a.mstr == b.mstr
	default:
		return false
	}
}

// valuesEqual implements equal? — deep structural equality.
func valuesEqual(a, b value) bool {
	if a.kind != b.kind {
		// Cross-numeric comparison
		if isNumber(a) && isNumber(b) {
			return toFloat64(a) == toFloat64(b)
		}
		return false
	}
	switch a.kind {
	case valInteger:
		return a.ival == b.ival
	case valRational:
		return a.numer == b.numer && a.denom == b.denom
	case valFloat:
		return a.fval == b.fval
	case valBoolean:
		return a.bval == b.bval
	case valSymbol:
		return a.sval == b.sval
	case valChar:
		return a.cval == b.cval
	case valString:
		return a.strContent() == b.strContent()
	case valNull:
		return true
	case valPair:
		return valuesEqual(a.pair.car, b.pair.car) && valuesEqual(a.pair.cdr, b.pair.cdr)
	default:
		return false
	}
}

func evalNumCompare(args []value, e *expr, cmp func(float64, float64) bool, name string) (value, error) {
	if len(args) < 2 {
		return value{}, &EvalError{Message: fmt.Sprintf("%d:%d: %s: expected at least 2 arguments", e.line, e.col, name)}
	}
	if !isNumber(args[0]) {
		return value{}, &EvalError{Message: fmt.Sprintf("%d:%d: %s: expected number", e.line, e.col, name)}
	}
	prev := toFloat64(args[0])
	for _, v := range args[1:] {
		if !isNumber(v) {
			return value{}, &EvalError{Message: fmt.Sprintf("%d:%d: %s: expected number", e.line, e.col, name)}
		}
		cur := toFloat64(v)
		if !cmp(prev, cur) {
			return boolVal(false), nil
		}
		prev = cur
	}
	return boolVal(true), nil
}

// ---------- Numeric tower arithmetic ----------

func numIsZero(v value) bool {
	switch v.kind {
	case valInteger:
		return v.ival == 0
	case valRational:
		return v.numer == 0
	case valFloat:
		return v.fval == 0
	}
	return false
}

func numNeg(v value) value {
	switch v.kind {
	case valInteger:
		return intVal(-v.ival)
	case valRational:
		return value{kind: valRational, numer: -v.numer, denom: v.denom}
	case valFloat:
		return floatVal(-v.fval)
	}
	return v
}

func numAdd(a, b value) value {
	if a.kind == valFloat || b.kind == valFloat {
		return floatVal(toFloat64(a) + toFloat64(b))
	}
	an, ad := toRational(a)
	bn, bd := toRational(b)
	return ratOrIntVal(an*bd+bn*ad, ad*bd)
}

func numSub(a, b value) value {
	if a.kind == valFloat || b.kind == valFloat {
		return floatVal(toFloat64(a) - toFloat64(b))
	}
	an, ad := toRational(a)
	bn, bd := toRational(b)
	return ratOrIntVal(an*bd-bn*ad, ad*bd)
}

func numMul(a, b value) value {
	if a.kind == valFloat || b.kind == valFloat {
		return floatVal(toFloat64(a) * toFloat64(b))
	}
	an, ad := toRational(a)
	bn, bd := toRational(b)
	return ratOrIntVal(an*bn, ad*bd)
}

func numDiv(a, b value) value {
	if a.kind == valFloat || b.kind == valFloat {
		return floatVal(toFloat64(a) / toFloat64(b))
	}
	an, ad := toRational(a)
	bn, bd := toRational(b)
	return ratOrIntVal(an*bd, ad*bn)
}

// floatToExact converts a float64 to an exact rational using simple fraction approximation.
func floatToExact(f float64) value {
	if f == math.Trunc(f) {
		return intVal(int64(f))
	}
	// Use a power-of-2 denominator approach for common cases
	// Try denominators up to 2^30
	const maxDenom int64 = 1 << 30
	bestN, bestD := int64(0), int64(1)
	bestErr := math.Abs(f)
	for d := int64(1); d <= maxDenom; d++ {
		n := int64(math.Round(f * float64(d)))
		err := math.Abs(f - float64(n)/float64(d))
		if err < bestErr {
			bestN, bestD = n, d
			bestErr = err
			if err == 0 {
				break
			}
		}
		if err < 1e-15 {
			bestN, bestD = n, d
			break
		}
	}
	return ratOrIntVal(bestN, bestD)
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

func builtinVal(name string) value {
	return value{kind: valBuiltin, sval: name}
}

// ---------- Macros (syntax-rules) ----------

var macroCounter int

func freshName(base string) string {
	macroCounter++
	return fmt.Sprintf("%s@@%d", base, macroCounter)
}

func evalDefineSyntax(e *expr, environ *env) (value, error) {
	if len(e.list) != 3 {
		return value{}, &EvalError{Message: fmt.Sprintf("%d:%d: define-syntax: bad syntax", e.line, e.col)}
	}
	nameExpr := e.list[1]
	if nameExpr.kind != exprAtom || nameExpr.atom.kind != valSymbol {
		return value{}, &EvalError{Message: fmt.Sprintf("%d:%d: define-syntax: expected symbol", e.line, e.col)}
	}
	name := nameExpr.atom.sval

	transformer := e.list[2]
	if transformer.kind != exprList || len(transformer.list) < 2 {
		return value{}, &EvalError{Message: fmt.Sprintf("%d:%d: define-syntax: expected syntax-rules", e.line, e.col)}
	}
	if transformer.list[0].kind != exprAtom || transformer.list[0].atom.sval != "syntax-rules" {
		return value{}, &EvalError{Message: fmt.Sprintf("%d:%d: define-syntax: expected syntax-rules", e.line, e.col)}
	}

	// Parse (syntax-rules (literals...) clause ...)
	if len(transformer.list) < 2 {
		return value{}, &EvalError{Message: fmt.Sprintf("%d:%d: syntax-rules: bad syntax", e.line, e.col)}
	}
	litExpr := transformer.list[1]
	if litExpr.kind != exprList {
		return value{}, &EvalError{Message: fmt.Sprintf("%d:%d: syntax-rules: expected literal list", e.line, e.col)}
	}
	var literals []string
	for _, l := range litExpr.list {
		if l.kind != exprAtom || l.atom.kind != valSymbol {
			return value{}, &EvalError{Message: fmt.Sprintf("%d:%d: syntax-rules: literals must be identifiers", e.line, e.col)}
		}
		literals = append(literals, l.atom.sval)
	}

	var rules []syntaxRule
	for _, clause := range transformer.list[2:] {
		if clause.kind != exprList || len(clause.list) != 2 {
			return value{}, &EvalError{Message: fmt.Sprintf("%d:%d: syntax-rules: bad clause", e.line, e.col)}
		}
		pat := clause.list[0]
		tmpl := clause.list[1]
		if pat.kind != exprList || len(pat.list) == 0 {
			return value{}, &EvalError{Message: fmt.Sprintf("%d:%d: syntax-rules: pattern must be a list", e.line, e.col)}
		}
		// pat.list[0] is the macro name placeholder, rest is the pattern
		rules = append(rules, syntaxRule{pattern: pat.list[1:], template: tmpl})
	}

	m := &macro{name: name, literals: literals, rules: rules, defEnv: environ}
	environ.set(name, value{kind: valMacro, macro: m})
	return voidVal, nil
}

// expandMacro tries each rule in order; returns expanded AST expr.
func expandMacro(m *macro, callExpr *expr, callEnv *env) (*expr, error) {
	args := callExpr.list[1:] // arguments to the macro call
	for _, rule := range m.rules {
		bindings := make(map[string][]*expr) // pattern var -> list of matched exprs
		if matchPattern(rule.pattern, args, m.literals, bindings) {
			// Build rename map for hygiene: any template-introduced identifier
			// that is not a pattern variable gets renamed.
			patVars := collectPatternVars(rule.pattern, m.literals)
			renames := make(map[string]string) // original -> fresh
			collectTemplateIdents(rule.template, patVars, renames, m.defEnv)
			// Create bindings in defEnv for renamed identifiers
			hygieneEnv := newEnv(m.defEnv)
			for orig, fresh := range renames {
				if v, ok := m.defEnv.get(orig); ok {
					hygieneEnv.set(fresh, v)
				}
			}
			// Also alias renamed identifiers in callEnv so set!/get works
			for orig, fresh := range renames {
				// The renamed ident should resolve in the *call* environment only
				// if it was defined there. For definition-site hygiene, we install
				// in callEnv pointing to the defEnv value.
				if v, ok := m.defEnv.get(orig); ok {
					callEnv.set(fresh, v)
				}
			}
			expanded := expandTemplate(rule.template, bindings, renames, callExpr)
			return expanded, nil
		}
	}
	return nil, &EvalError{Message: fmt.Sprintf("%d:%d: %s: no matching pattern", callExpr.line, callExpr.col, m.name)}
}

// matchPattern matches a pattern against args. pattern and args are slices of *expr.
// bindings maps pattern variable names to matched expressions.
func matchPattern(pattern []*expr, args []*expr, literals []string, bindings map[string][]*expr) bool {
	pi := 0
	ai := 0
	for pi < len(pattern) {
		// Check for ellipsis: current pattern element followed by ...
		if pi+1 < len(pattern) && isEllipsis(pattern[pi+1]) {
			patVar := pattern[pi]
			if patVar.kind != exprAtom || patVar.atom.kind != valSymbol {
				return false
			}
			varName := patVar.atom.sval
			// Collect remaining args (greedy, since ellipsis is typically last)
			// Number of remaining required pattern elements after the ellipsis
			remaining := len(pattern) - pi - 2
			available := len(args) - ai - remaining
			if available < 0 {
				return false
			}
			var matched []*expr
			for i := 0; i < available; i++ {
				matched = append(matched, args[ai+i])
			}
			bindings[varName] = matched
			ai += available
			pi += 2 // skip pattern var and ellipsis
			continue
		}

		if ai >= len(args) {
			return false
		}

		pat := pattern[pi]
		arg := args[ai]

		if pat.kind == exprAtom && pat.atom.kind == valSymbol {
			name := pat.atom.sval
			if isLiteral(name, literals) {
				// Must match literally
				if arg.kind != exprAtom || arg.atom.kind != valSymbol || arg.atom.sval != name {
					return false
				}
			} else if name == "_" {
				// Wildcard, matches anything
			} else {
				// Pattern variable
				bindings[name] = []*expr{arg}
			}
		} else if pat.kind == exprList && arg.kind == exprList {
			// Recursively match sub-patterns
			if !matchPattern(pat.list, arg.list, literals, bindings) {
				return false
			}
		} else {
			return false
		}
		pi++
		ai++
	}
	return ai == len(args)
}

func isEllipsis(e *expr) bool {
	return e.kind == exprAtom && e.atom.kind == valSymbol && e.atom.sval == "..."
}

func isLiteral(name string, literals []string) bool {
	for _, l := range literals {
		if l == name {
			return true
		}
	}
	return false
}

// collectPatternVars returns the set of pattern variable names.
func collectPatternVars(pattern []*expr, literals []string) map[string]bool {
	vars := make(map[string]bool)
	for i, p := range pattern {
		if isEllipsis(p) {
			continue
		}
		_ = i
		if p.kind == exprAtom && p.atom.kind == valSymbol {
			name := p.atom.sval
			if !isLiteral(name, literals) && name != "_" && name != "..." {
				vars[name] = true
			}
		} else if p.kind == exprList {
			for k, v := range collectPatternVars(p.list, literals) {
				vars[k] = v
			}
		}
	}
	return vars
}

// collectTemplateIdents finds identifiers in the template that are NOT pattern variables
// and creates fresh renames for them (hygiene).
func collectTemplateIdents(tmpl *expr, patVars map[string]bool, renames map[string]string, defEnv *env) {
	if tmpl.kind == exprAtom && tmpl.atom.kind == valSymbol {
		name := tmpl.atom.sval
		if !patVars[name] && name != "..." && !isSpecialForm(name) && !isBuiltin(name) {
			// Only rename if the identifier is bound in the definition env
			// (i.e., it refers to a definition-site binding worth preserving)
			if _, bound := defEnv.get(name); bound {
				if _, already := renames[name]; !already {
					renames[name] = freshName(name)
				}
			}
		}
		return
	}
	if tmpl.kind == exprList {
		for _, child := range tmpl.list {
			collectTemplateIdents(child, patVars, renames, defEnv)
		}
	}
}

// ---------- define-record-type ----------

func evalDefineRecordType(e *expr, environ *env) (value, error) {
	// (define-record-type <name> (constructor field-name ...) predicate (field-name accessor) ...)
	if len(e.list) < 4 {
		return value{}, &EvalError{Message: fmt.Sprintf("%d:%d: define-record-type: bad syntax", e.line, e.col)}
	}

	// 1. Type name
	nameExpr := e.list[1]
	if nameExpr.kind != exprAtom || nameExpr.atom.kind != valSymbol {
		return value{}, &EvalError{Message: fmt.Sprintf("%d:%d: define-record-type: expected type name", e.line, e.col)}
	}
	rt := &recordType{name: nameExpr.atom.sval}

	// 2. Constructor: (constructor-name field-name ...)
	ctorExpr := e.list[2]
	if ctorExpr.kind != exprList || len(ctorExpr.list) < 1 {
		return value{}, &EvalError{Message: fmt.Sprintf("%d:%d: define-record-type: expected constructor", e.line, e.col)}
	}
	if ctorExpr.list[0].kind != exprAtom || ctorExpr.list[0].atom.kind != valSymbol {
		return value{}, &EvalError{Message: fmt.Sprintf("%d:%d: define-record-type: expected constructor name", e.line, e.col)}
	}
	ctorName := ctorExpr.list[0].atom.sval
	ctorFields := make([]string, len(ctorExpr.list)-1)
	for i := 1; i < len(ctorExpr.list); i++ {
		if ctorExpr.list[i].kind != exprAtom || ctorExpr.list[i].atom.kind != valSymbol {
			return value{}, &EvalError{Message: fmt.Sprintf("%d:%d: define-record-type: expected field name", e.line, e.col)}
		}
		ctorFields[i-1] = ctorExpr.list[i].atom.sval
	}

	// 3. Predicate
	predExpr := e.list[3]
	if predExpr.kind != exprAtom || predExpr.atom.kind != valSymbol {
		return value{}, &EvalError{Message: fmt.Sprintf("%d:%d: define-record-type: expected predicate name", e.line, e.col)}
	}
	predName := predExpr.atom.sval

	// 4. Field specs: (field-name accessor)
	// Build mapping from field name to index based on constructor field order
	fieldIndex := make(map[string]int)
	for i, f := range ctorFields {
		fieldIndex[f] = i
	}

	// Define constructor as a builtin lambda
	numFields := len(ctorFields)
	capturedRT := rt
	environ.set(ctorName, value{kind: valBuiltin, sval: "__record-ctor-" + ctorName})

	// We'll use lambda closures instead of builtins for cleaner implementation
	// Constructor
	ctorParams := make([]string, numFields)
	copy(ctorParams, ctorFields)
	environ.set(ctorName, makeLambdaVal(func(args []value) (value, error) {
		if len(args) != numFields {
			return value{}, &EvalError{Message: fmt.Sprintf("%s: expected %d arguments, got %d", ctorName, numFields, len(args))}
		}
		fields := make([]value, numFields)
		copy(fields, args)
		return value{kind: valRecord, rec: &record{rtype: capturedRT, fields: fields}}, nil
	}))

	// Predicate
	environ.set(predName, makeLambdaVal(func(args []value) (value, error) {
		if len(args) != 1 {
			return value{}, &EvalError{Message: fmt.Sprintf("%s: expected 1 argument", predName)}
		}
		return boolVal(args[0].kind == valRecord && args[0].rec.rtype == capturedRT), nil
	}))

	// Field accessors
	for i := 4; i < len(e.list); i++ {
		fieldSpec := e.list[i]
		if fieldSpec.kind != exprList || len(fieldSpec.list) < 2 {
			return value{}, &EvalError{Message: fmt.Sprintf("%d:%d: define-record-type: bad field spec", e.line, e.col)}
		}
		fieldNameExpr := fieldSpec.list[0]
		accessorExpr := fieldSpec.list[1]
		if fieldNameExpr.kind != exprAtom || fieldNameExpr.atom.kind != valSymbol {
			return value{}, &EvalError{Message: fmt.Sprintf("%d:%d: define-record-type: expected field name", e.line, e.col)}
		}
		if accessorExpr.kind != exprAtom || accessorExpr.atom.kind != valSymbol {
			return value{}, &EvalError{Message: fmt.Sprintf("%d:%d: define-record-type: expected accessor name", e.line, e.col)}
		}
		fname := fieldNameExpr.atom.sval
		accName := accessorExpr.atom.sval
		idx, ok := fieldIndex[fname]
		if !ok {
			return value{}, &EvalError{Message: fmt.Sprintf("%d:%d: define-record-type: unknown field %s", e.line, e.col, fname)}
		}
		capturedIdx := idx
		capturedAccName := accName
		environ.set(accName, makeLambdaVal(func(args []value) (value, error) {
			if len(args) != 1 {
				return value{}, &EvalError{Message: fmt.Sprintf("%s: expected 1 argument", capturedAccName)}
			}
			if args[0].kind != valRecord || args[0].rec.rtype != capturedRT {
				return value{}, &EvalError{Message: fmt.Sprintf("%s: not a %s record", capturedAccName, capturedRT.name)}
			}
			return args[0].rec.fields[capturedIdx], nil
		}))
	}

	return voidVal, nil
}

// nativeFunc wraps a Go function as a callable value.
type nativeFunc struct {
	fn func([]value) (value, error)
}

var valNative valueKind = -1 // sentinel, not used

func makeLambdaVal(fn func([]value) (value, error)) value {
	// We store native functions as builtins with a special wrapper
	v := value{kind: valBuiltin, sval: "__native"}
	// Store the function pointer via a closure-based approach using lambda
	// Actually, let's use a different approach - store in a global registry
	nativeFuncsMu.Lock()
	id := nativeFuncCounter
	nativeFuncCounter++
	nativeFuncs[id] = fn
	nativeFuncsMu.Unlock()
	v.ival = int64(id)
	return v
}

var (
	nativeFuncs    = make(map[int]func([]value) (value, error))
	nativeFuncCounter int
	nativeFuncsMu  sync.Mutex
)

func isSpecialForm(name string) bool {
	switch name {
	case "define", "set!", "if", "quote", "lambda", "and", "or", "let", "begin", "cond", "define-syntax", "define-record-type":
		return true
	}
	return false
}

// expandTemplate substitutes pattern variables and applies renames.
func expandTemplate(tmpl *expr, bindings map[string][]*expr, renames map[string]string, callExpr *expr) *expr {
	if tmpl.kind == exprAtom {
		if tmpl.atom.kind == valSymbol {
			name := tmpl.atom.sval
			// Pattern variable (non-ellipsis context)
			if vals, ok := bindings[name]; ok && len(vals) == 1 {
				return vals[0]
			}
			// Hygiene rename
			if fresh, ok := renames[name]; ok {
				return &expr{kind: exprAtom, atom: symVal(fresh), line: callExpr.line, col: callExpr.col}
			}
		}
		return tmpl
	}

	// List template
	var result []*expr
	for i := 0; i < len(tmpl.list); i++ {
		child := tmpl.list[i]
		// Check if next element is ellipsis
		if i+1 < len(tmpl.list) && isEllipsis(tmpl.list[i+1]) {
			// Expand the ellipsis pattern
			expanded := expandEllipsis(child, bindings, renames, callExpr)
			result = append(result, expanded...)
			i++ // skip ellipsis
			continue
		}
		result = append(result, expandTemplate(child, bindings, renames, callExpr))
	}
	return &expr{kind: exprList, list: result, line: callExpr.line, col: callExpr.col}
}

// expandEllipsis expands a template element that is followed by ...
func expandEllipsis(tmpl *expr, bindings map[string][]*expr, renames map[string]string, callExpr *expr) []*expr {
	// Find the ellipsis variable in this template
	ellipsisVar := findEllipsisVar(tmpl, bindings)
	if ellipsisVar == "" {
		return nil
	}
	vals := bindings[ellipsisVar]
	var result []*expr
	for i := range vals {
		// Create a single-element binding for this iteration
		iterBindings := make(map[string][]*expr)
		for k, v := range bindings {
			iterBindings[k] = v
		}
		iterBindings[ellipsisVar] = []*expr{vals[i]}
		result = append(result, expandTemplate(tmpl, iterBindings, renames, callExpr))
	}
	return result
}

// findEllipsisVar finds the pattern variable in a template that has multiple bindings.
func findEllipsisVar(tmpl *expr, bindings map[string][]*expr) string {
	if tmpl.kind == exprAtom && tmpl.atom.kind == valSymbol {
		if _, ok := bindings[tmpl.atom.sval]; ok {
			return tmpl.atom.sval
		}
	}
	if tmpl.kind == exprList {
		for _, child := range tmpl.list {
			if v := findEllipsisVar(child, bindings); v != "" {
				return v
			}
		}
	}
	return ""
}

func makeTopLevelEnv() *env {
	e := newEnv(nil)
	builtins := []string{
		"+", "-", "*", "/", "<", ">", "=", "<=", ">=", "not",
		"cons", "car", "cdr", "null?", "list", "length", "append",
		"string?", "number?", "boolean?", "pair?", "symbol?", "char?",
		"display", "write", "newline",
		"string-append", "string-length", "substring", "string-ref",
		"string->number", "number->string", "symbol->string", "string->symbol",
		"string-copy", "string-set!", "apply",
		"abs", "modulo", "remainder", "quotient", "min", "max", "expt",
		"zero?", "positive?", "negative?", "odd?", "even?",
		"list-ref", "list-tail", "list?", "assoc", "map",
		"char=?", "char<?", "char-alphabetic?", "char-numeric?", "char-upcase", "char-downcase",
		"string=?", "string<?", "string-ci=?", "string-upcase", "string-downcase",
		"eq?", "equal?",
		"integer?", "rational?", "exact?", "inexact?",
		"exact->inexact", "inexact->exact",
		"numerator", "denominator",
		"procedure?",
	}
	for _, name := range builtins {
		e.set(name, builtinVal(name))
	}
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
