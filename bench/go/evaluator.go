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
	valMacro
	valFloat
	valRational
	valRecord
	valGoFunc
	valVector
)

type Value struct {
	typ    valueType
	ival   int64
	fval   float64
	dval   int64 // denominator for valRational
	bval   bool
	sval   string
	car    *Value
	cdr    *Value
	// lambda fields
	params    []string
	restParam string // variadic rest parameter (after dot)
	body      []*astNode
	closure   *env
	macro     *syntaxRulesMacro
	// record fields
	recordTag    *recordType
	recordFields []*Value
	// case-lambda clauses
	caseClauses []caseClause
	// native Go function
	goFunc func([]*Value) (*Value, error)
	goName string // name for display
	// string immutability (L15)
	immutable bool
}

type caseClause struct {
	params    []string
	restParam string
	body      []*astNode
}

type recordType struct {
	name       string
	fieldNames []string
}

var recordTypeCounter int

func intVal(n int64) *Value    { return &Value{typ: valInt, ival: n} }
func boolVal(b bool) *Value    { return &Value{typ: valBool, bval: b} }
func strVal(s string) *Value          { return &Value{typ: valString, sval: s} }
func strValImmutable(s string) *Value { return &Value{typ: valString, sval: s, immutable: true} }
func symVal(s string) *Value   { return &Value{typ: valSymbol, sval: s} }
func charVal(c rune) *Value    { return &Value{typ: valChar, ival: int64(c)} }
func floatVal(f float64) *Value { return &Value{typ: valFloat, fval: f} }
func nilVal() *Value           { return &Value{typ: valNil} }
func voidVal() *Value          { return &Value{typ: valVoid} }

func gcd64(a, b int64) int64 {
	for b != 0 {
		a, b = b, a%b
	}
	return a
}

func abs64(n int64) int64 {
	if n < 0 {
		return -n
	}
	return n
}

func makeRational(num, den int64) *Value {
	if den < 0 {
		num, den = -num, -den
	}
	g := gcd64(abs64(num), den)
	if g != 0 {
		num, den = num/g, den/g
	}
	if den == 1 {
		return intVal(num)
	}
	return &Value{typ: valRational, ival: num, dval: den}
}

func isNumeric(v *Value) bool {
	return v.typ == valInt || v.typ == valFloat || v.typ == valRational
}

func isExact(v *Value) bool {
	return v.typ == valInt || v.typ == valRational
}

func toFloat64(v *Value) float64 {
	switch v.typ {
	case valInt:
		return float64(v.ival)
	case valFloat:
		return v.fval
	case valRational:
		return float64(v.ival) / float64(v.dval)
	}
	return 0
}

func toRatParts(v *Value) (int64, int64) {
	switch v.typ {
	case valInt:
		return v.ival, 1
	case valRational:
		return v.ival, v.dval
	}
	return 0, 1
}

func numAdd(a, b *Value) *Value {
	if a.typ == valFloat || b.typ == valFloat {
		return floatVal(toFloat64(a) + toFloat64(b))
	}
	an, ad := toRatParts(a)
	bn, bd := toRatParts(b)
	return makeRational(an*bd+bn*ad, ad*bd)
}

func numSub(a, b *Value) *Value {
	if a.typ == valFloat || b.typ == valFloat {
		return floatVal(toFloat64(a) - toFloat64(b))
	}
	an, ad := toRatParts(a)
	bn, bd := toRatParts(b)
	return makeRational(an*bd-bn*ad, ad*bd)
}

func numMul(a, b *Value) *Value {
	if a.typ == valFloat || b.typ == valFloat {
		return floatVal(toFloat64(a) * toFloat64(b))
	}
	an, ad := toRatParts(a)
	bn, bd := toRatParts(b)
	return makeRational(an*bn, ad*bd)
}

func numDiv(a, b *Value) *Value {
	if a.typ == valFloat || b.typ == valFloat {
		return floatVal(toFloat64(a) / toFloat64(b))
	}
	an, ad := toRatParts(a)
	bn, bd := toRatParts(b)
	return makeRational(an*bd, ad*bn)
}

func numNeg(a *Value) *Value {
	switch a.typ {
	case valInt:
		return intVal(-a.ival)
	case valFloat:
		return floatVal(-a.fval)
	case valRational:
		return &Value{typ: valRational, ival: -a.ival, dval: a.dval}
	}
	return a
}

func numCmp(a, b *Value) int {
	if a.typ == valFloat || b.typ == valFloat {
		af, bf := toFloat64(a), toFloat64(b)
		if af < bf {
			return -1
		}
		if af > bf {
			return 1
		}
		return 0
	}
	an, ad := toRatParts(a)
	bn, bd := toRatParts(b)
	lhs := an * bd
	rhs := bn * ad
	if lhs < rhs {
		return -1
	}
	if lhs > rhs {
		return 1
	}
	return 0
}

func floatToExact(f float64) *Value {
	if f == float64(int64(f)) {
		return intVal(int64(f))
	}
	neg := f < 0
	if neg {
		f = -f
	}
	num := f
	den := int64(1)
	for num != float64(int64(num)) && den < (1<<52) {
		num *= 2
		den *= 2
	}
	n := int64(num)
	if neg {
		n = -n
	}
	return makeRational(n, den)
}

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
	case valFloat:
		s := strconv.FormatFloat(v.fval, 'f', -1, 64)
		if !strings.Contains(s, ".") {
			s += ".0"
		}
		return s
	case valRational:
		return fmt.Sprintf("%d/%d", v.ival, v.dval)
	case valLambda:
		return "#<procedure>"
	case valMacro:
		return "#<macro>"
	case valRecord:
		return "#<record>"
	case valGoFunc:
		return "#<procedure>"
	case valVector:
		parts := make([]string, len(v.recordFields))
		for i, el := range v.recordFields {
			parts[i] = el.String()
		}
		return "#(" + strings.Join(parts, " ") + ")"
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
	case valVector:
		parts := make([]string, len(v.recordFields))
		for i, el := range v.recordFields {
			parts[i] = el.displayStr()
		}
		return "#(" + strings.Join(parts, " ") + ")"
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
	tokFloat
	tokRational
	tokString
	tokBool
	tokSymbol
	tokQuote
	tokChar
	tokDot
	tokEOF
)

type token struct {
	kind tokenKind
	sval string
	ival int64
	fval float64
	dval int64
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
		} else if next == '\\' {
			l.advance() // consume backslash
			if l.pos >= len(l.input) {
				return token{}, &EvalError{Message: fmt.Sprintf("%d:%d: unexpected end of character literal", line, col)}
			}
			// Read character name or single char
			first := l.advance()
			// Check for named characters
			if unicode.IsLetter(first) && l.pos < len(l.input) && unicode.IsLetter(l.peek()) {
				name := string(first)
				for l.pos < len(l.input) && unicode.IsLetter(l.peek()) {
					name += string(l.advance())
				}
				switch name {
				case "space":
					return token{kind: tokChar, ival: int64(' '), line: line, col: col}, nil
				case "newline":
					return token{kind: tokChar, ival: int64('\n'), line: line, col: col}, nil
				case "tab":
					return token{kind: tokChar, ival: int64('\t'), line: line, col: col}, nil
				default:
					return token{}, &EvalError{Message: fmt.Sprintf("%d:%d: unknown character name: %s", line, col, name)}
				}
			}
			return token{kind: tokChar, ival: int64(first), line: line, col: col}, nil
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

	if s == "." {
		return token{kind: tokDot, line: line, col: col}, nil
	}

	// Try parsing as integer
	if n, err := strconv.ParseInt(s, 10, 64); err == nil {
		return token{kind: tokNumber, ival: n, line: line, col: col}, nil
	}

	// Try parsing as rational (e.g., 1/3, -1/3)
	if idx := strings.Index(s, "/"); idx > 0 && idx < len(s)-1 {
		numStr := s[:idx]
		denStr := s[idx+1:]
		if num, err := strconv.ParseInt(numStr, 10, 64); err == nil {
			if den, err := strconv.ParseInt(denStr, 10, 64); err == nil && den != 0 {
				return token{kind: tokRational, ival: num, dval: den, line: line, col: col}, nil
			}
		}
	}

	// Try parsing as float
	if strings.Contains(s, ".") || strings.ContainsAny(s, "eE") {
		if f, err := strconv.ParseFloat(s, 64); err == nil {
			return token{kind: tokFloat, fval: f, line: line, col: col}, nil
		}
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
	hasDot   bool // true if list contains a dot (e.g., (a . b) or (x . rest))
	dotPos   int  // index in children where dot appeared
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
			if pk.kind == tokDot {
				// Consume dot
				p.next()
				// Mark this node as having a dot (store dot position)
				node.hasDot = true
				node.dotPos = len(node.children)
				continue
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

func (e *env) setMutate(name string, v *Value) bool {
	if _, ok := e.vars[name]; ok {
		e.vars[name] = v
		return true
	}
	if e.parent != nil {
		return e.parent.setMutate(name, v)
	}
	return false
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
	case tokFloat:
		return floatVal(t.fval), nil
	case tokRational:
		return makeRational(t.ival, t.dval), nil
	case tokBool:
		return boolVal(t.bval), nil
	case tokString:
		return strValImmutable(t.sval), nil
	case tokChar:
		return charVal(rune(t.ival)), nil
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
		case "case-lambda":
			return evalCaseLambda(node, e)
		case "let":
			return evalLet(node, e, ip)
		case "begin":
			return evalBegin(node, e, ip)
		case "cond":
			return evalCond(node, e, ip)
		case "set!":
			return evalSet(node, e, ip)
		case "define-syntax":
			return evalDefineSyntax(node, e)
		case "define-record-type":
			return evalDefineRecordType(node, e)
		case "letrec":
			return evalLetrec(node, e, ip, false)
		case "letrec*":
			return evalLetrec(node, e, ip, true)
		case "case":
			return evalCase(node, e, ip)
		case "do":
			return evalDo(node, e, ip)
		}

		// Check if symbol resolves to a macro
		if v, ok := e.get(first.tok.sval); ok && v.typ == valMacro {
			expanded, expandEnv, err := expandMacro(v.macro, node, e)
			if err != nil {
				return nil, err
			}
			return eval(expanded, expandEnv, ip)
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
		return applyLambda(op, args, node, ip)
	}

	// Native Go function application
	if op.typ == valGoFunc {
		return op.goFunc(args)
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
	// (define (f params...) body...) or (define (f x . rest) body...)
	if !target.isAtom && len(target.children) >= 1 {
		name := target.children[0]
		if !name.isAtom || name.tok.kind != tokSymbol {
			return nil, &EvalError{Message: fmt.Sprintf("%d:%d: define: bad syntax", node.line, node.col)}
		}
		// Build a fake param node from the rest of target's children
		paramNode := &astNode{
			children: target.children[1:],
			hasDot:   target.hasDot,
			dotPos:   target.dotPos - 1, // adjust for name being first child
			line:     target.line,
			col:      target.col,
		}
		if target.hasDot && target.dotPos <= 0 {
			paramNode.dotPos = 0
		}
		params, restParam, err := parseLambdaParams(paramNode)
		if err != nil {
			return nil, err
		}
		lam := &Value{typ: valLambda, params: params, restParam: restParam, body: node.children[2:], closure: e}
		e.set(name.tok.sval, lam)
		return voidVal(), nil
	}
	return nil, &EvalError{Message: fmt.Sprintf("%d:%d: define: bad syntax", node.line, node.col)}
}

func evalSet(node *astNode, e *env, ip *interp) (*Value, error) {
	if len(node.children) != 3 {
		return nil, &EvalError{Message: fmt.Sprintf("%d:%d: set!: bad syntax", node.line, node.col)}
	}
	target := node.children[1]
	if !target.isAtom || target.tok.kind != tokSymbol {
		return nil, &EvalError{Message: fmt.Sprintf("%d:%d: set!: not a variable", node.line, node.col)}
	}
	val, err := eval(node.children[2], e, ip)
	if err != nil {
		return nil, err
	}
	if !e.setMutate(target.tok.sval, val) {
		return nil, &EvalError{Message: fmt.Sprintf("%d:%d: set!: unbound variable: %s", target.tok.line, target.tok.col, target.tok.sval)}
	}
	return voidVal(), nil
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

func parseLambdaParams(paramNode *astNode) (params []string, restParam string, err error) {
	if paramNode.isAtom {
		// (lambda args body) — single symbol captures all args
		if paramNode.tok.kind == tokSymbol {
			return nil, paramNode.tok.sval, nil
		}
		return nil, "", &EvalError{Message: fmt.Sprintf("%d:%d: lambda: bad parameter list", paramNode.line, paramNode.col)}
	}
	if paramNode.hasDot {
		// (x y . rest) — dotPos elements before dot, 1 after
		for _, p := range paramNode.children[:paramNode.dotPos] {
			if !p.isAtom || p.tok.kind != tokSymbol {
				return nil, "", &EvalError{Message: fmt.Sprintf("%d:%d: lambda: bad parameter", paramNode.line, paramNode.col)}
			}
			params = append(params, p.tok.sval)
		}
		if paramNode.dotPos >= len(paramNode.children) {
			return nil, "", &EvalError{Message: fmt.Sprintf("%d:%d: lambda: missing rest parameter after dot", paramNode.line, paramNode.col)}
		}
		rest := paramNode.children[paramNode.dotPos]
		if !rest.isAtom || rest.tok.kind != tokSymbol {
			return nil, "", &EvalError{Message: fmt.Sprintf("%d:%d: lambda: bad rest parameter", paramNode.line, paramNode.col)}
		}
		return params, rest.tok.sval, nil
	}
	for _, p := range paramNode.children {
		if !p.isAtom || p.tok.kind != tokSymbol {
			return nil, "", &EvalError{Message: fmt.Sprintf("%d:%d: lambda: bad parameter", paramNode.line, paramNode.col)}
		}
		params = append(params, p.tok.sval)
	}
	return params, "", nil
}

func evalLambda(node *astNode, e *env) (*Value, error) {
	if len(node.children) < 3 {
		return nil, &EvalError{Message: fmt.Sprintf("%d:%d: lambda: bad syntax", node.line, node.col)}
	}
	params, restParam, err := parseLambdaParams(node.children[1])
	if err != nil {
		return nil, err
	}
	return &Value{typ: valLambda, params: params, restParam: restParam, body: node.children[2:], closure: e}, nil
}

func evalCaseLambda(node *astNode, e *env) (*Value, error) {
	if len(node.children) < 2 {
		return nil, &EvalError{Message: fmt.Sprintf("%d:%d: case-lambda: bad syntax", node.line, node.col)}
	}
	var clauses []caseClause
	for _, clause := range node.children[1:] {
		if clause.isAtom || len(clause.children) < 2 {
			return nil, &EvalError{Message: fmt.Sprintf("%d:%d: case-lambda: bad clause", node.line, node.col)}
		}
		params, restParam, err := parseLambdaParams(clause.children[0])
		if err != nil {
			return nil, err
		}
		clauses = append(clauses, caseClause{params: params, restParam: restParam, body: clause.children[1:]})
	}
	return &Value{typ: valLambda, caseClauses: clauses, closure: e}, nil
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
		case tokFloat:
			return floatVal(node.tok.fval)
		case tokRational:
			return makeRational(node.tok.ival, node.tok.dval)
		case tokBool:
			return boolVal(node.tok.bval)
		case tokString:
			return strValImmutable(node.tok.sval)
		case tokChar:
			return charVal(rune(node.tok.ival))
		case tokSymbol:
			return symVal(node.tok.sval)
		}
	}
	// List
	if len(node.children) == 0 {
		return nilVal()
	}
	if node.hasDot {
		// Dotted pair: elements before dotPos are car chain, element at dotPos is final cdr
		result := quoteNode(node.children[node.dotPos])
		for i := node.dotPos - 1; i >= 0; i-- {
			result = &Value{typ: valPair, car: quoteNode(node.children[i]), cdr: result}
		}
		return result
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

func requireNums(args []*Value, name string, node *astNode) error {
	for _, a := range args {
		if !isNumeric(a) {
			return &EvalError{Message: fmt.Sprintf("%d:%d: %s: expected number", node.line, node.col, name)}
		}
	}
	return nil
}

func applyLambda(op *Value, args []*Value, node *astNode, ip *interp) (*Value, error) {
	// case-lambda: dispatch to matching clause
	if op.caseClauses != nil {
		for _, cl := range op.caseClauses {
			if cl.restParam != "" {
				if len(args) < len(cl.params) {
					continue
				}
			} else {
				if len(args) != len(cl.params) {
					continue
				}
			}
			localEnv := newEnv(op.closure)
			for i, param := range cl.params {
				localEnv.set(param, args[i])
			}
			if cl.restParam != "" {
				rest := nilVal()
				for i := len(args) - 1; i >= len(cl.params); i-- {
					rest = &Value{typ: valPair, car: args[i], cdr: rest}
				}
				localEnv.set(cl.restParam, rest)
			}
			var result *Value
			for _, bodyExpr := range cl.body {
				var err error
				result, err = eval(bodyExpr, localEnv, ip)
				if err != nil {
					return nil, err
				}
			}
			return result, nil
		}
		return nil, &EvalError{Message: fmt.Sprintf("%d:%d: case-lambda: no matching clause for %d arguments", node.line, node.col, len(args))}
	}
	if op.restParam != "" {
		// Has rest parameter
		if len(args) < len(op.params) {
			return nil, &EvalError{Message: fmt.Sprintf("%d:%d: wrong number of arguments: expected at least %d, got %d", node.line, node.col, len(op.params), len(args))}
		}
		localEnv := newEnv(op.closure)
		for i, param := range op.params {
			localEnv.set(param, args[i])
		}
		// Collect remaining args into a list
		rest := nilVal()
		for i := len(args) - 1; i >= len(op.params); i-- {
			rest = &Value{typ: valPair, car: args[i], cdr: rest}
		}
		localEnv.set(op.restParam, rest)
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

func applyBuiltin(name string, args []*Value, node *astNode, ip *interp) (*Value, error) {
	switch name {
	case "+":
		if err := requireNums(args, "+", node); err != nil {
			return nil, err
		}
		if len(args) == 0 {
			return intVal(0), nil
		}
		result := args[0]
		for _, a := range args[1:] {
			result = numAdd(result, a)
		}
		return result, nil

	case "-":
		if len(args) == 0 {
			return nil, &EvalError{Message: fmt.Sprintf("%d:%d: -: need at least 1 argument", node.line, node.col)}
		}
		if err := requireNums(args, "-", node); err != nil {
			return nil, err
		}
		if len(args) == 1 {
			return numNeg(args[0]), nil
		}
		result := args[0]
		for _, a := range args[1:] {
			result = numSub(result, a)
		}
		return result, nil

	case "*":
		if err := requireNums(args, "*", node); err != nil {
			return nil, err
		}
		if len(args) == 0 {
			return intVal(1), nil
		}
		result := args[0]
		for _, a := range args[1:] {
			result = numMul(result, a)
		}
		return result, nil

	case "/":
		if len(args) < 2 {
			return nil, &EvalError{Message: fmt.Sprintf("%d:%d: /: need at least 2 arguments", node.line, node.col)}
		}
		if err := requireNums(args, "/", node); err != nil {
			return nil, err
		}
		result := args[0]
		for _, a := range args[1:] {
			if toFloat64(a) == 0 {
				return nil, &EvalError{Message: fmt.Sprintf("%d:%d: /: division by zero", node.line, node.col)}
			}
			result = numDiv(result, a)
		}
		return result, nil

	case "<":
		if len(args) != 2 {
			return nil, &EvalError{Message: fmt.Sprintf("%d:%d: <: need 2 arguments", node.line, node.col)}
		}
		if err := requireNums(args, "<", node); err != nil {
			return nil, err
		}
		return boolVal(numCmp(args[0], args[1]) < 0), nil

	case ">":
		if len(args) != 2 {
			return nil, &EvalError{Message: fmt.Sprintf("%d:%d: >: need 2 arguments", node.line, node.col)}
		}
		if err := requireNums(args, ">", node); err != nil {
			return nil, err
		}
		return boolVal(numCmp(args[0], args[1]) > 0), nil

	case "=":
		if len(args) != 2 {
			return nil, &EvalError{Message: fmt.Sprintf("%d:%d: =: need 2 arguments", node.line, node.col)}
		}
		if err := requireNums(args, "=", node); err != nil {
			return nil, err
		}
		return boolVal(numCmp(args[0], args[1]) == 0), nil

	case "<=":
		if len(args) != 2 {
			return nil, &EvalError{Message: fmt.Sprintf("%d:%d: <=: need 2 arguments", node.line, node.col)}
		}
		if err := requireNums(args, "<=", node); err != nil {
			return nil, err
		}
		return boolVal(numCmp(args[0], args[1]) <= 0), nil

	case ">=":
		if len(args) != 2 {
			return nil, &EvalError{Message: fmt.Sprintf("%d:%d: >=: need 2 arguments", node.line, node.col)}
		}
		if err := requireNums(args, ">=", node); err != nil {
			return nil, err
		}
		return boolVal(numCmp(args[0], args[1]) >= 0), nil

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
		return boolVal(isNumeric(args[0])), nil

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

	case "procedure?":
		if len(args) != 1 {
			return nil, &EvalError{Message: fmt.Sprintf("%d:%d: procedure?: need 1 argument", node.line, node.col)}
		}
		return boolVal(args[0].typ == valLambda || args[0].typ == valGoFunc), nil

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
		if !isNumeric(args[0]) {
			return nil, &EvalError{Message: fmt.Sprintf("%d:%d: number->string: expected number", node.line, node.col)}
		}
		return strVal(args[0].String()), nil

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

	case "string-copy":
		if len(args) != 1 {
			return nil, &EvalError{Message: fmt.Sprintf("%d:%d: string-copy: need 1 argument", node.line, node.col)}
		}
		if args[0].typ != valString {
			return nil, &EvalError{Message: fmt.Sprintf("%d:%d: string-copy: expected string", node.line, node.col)}
		}
		return strVal(args[0].sval), nil

	case "string-set!":
		if len(args) != 3 {
			return nil, &EvalError{Message: fmt.Sprintf("%d:%d: string-set!: need 3 arguments", node.line, node.col)}
		}
		if args[0].typ != valString || args[1].typ != valInt || args[2].typ != valChar {
			return nil, &EvalError{Message: fmt.Sprintf("%d:%d: string-set!: bad arguments", node.line, node.col)}
		}
		if args[0].immutable {
			return nil, &EvalError{Message: fmt.Sprintf("%d:%d: string-set!: strings are immutable", node.line, node.col)}
		}
		runes := []rune(args[0].sval)
		idx := int(args[1].ival)
		if idx < 0 || idx >= len(runes) {
			return nil, &EvalError{Message: fmt.Sprintf("%d:%d: string-set!: index out of range", node.line, node.col)}
		}
		runes[idx] = rune(args[2].ival)
		args[0].sval = string(runes)
		return voidVal(), nil

	case "string->list":
		if len(args) != 1 {
			return nil, &EvalError{Message: fmt.Sprintf("%d:%d: string->list: need 1 argument", node.line, node.col)}
		}
		if args[0].typ != valString {
			return nil, &EvalError{Message: fmt.Sprintf("%d:%d: string->list: expected string", node.line, node.col)}
		}
		runes := []rune(args[0].sval)
		result := nilVal()
		for i := len(runes) - 1; i >= 0; i-- {
			result = &Value{typ: valPair, car: charVal(runes[i]), cdr: result}
		}
		return result, nil

	case "list->string":
		if len(args) != 1 {
			return nil, &EvalError{Message: fmt.Sprintf("%d:%d: list->string: need 1 argument", node.line, node.col)}
		}
		var runes []rune
		cur := args[0]
		for cur.typ == valPair {
			if cur.car.typ != valChar {
				return nil, &EvalError{Message: fmt.Sprintf("%d:%d: list->string: expected list of characters", node.line, node.col)}
			}
			runes = append(runes, rune(cur.car.ival))
			cur = cur.cdr
		}
		if cur.typ != valNil {
			return nil, &EvalError{Message: fmt.Sprintf("%d:%d: list->string: expected proper list", node.line, node.col)}
		}
		return strVal(string(runes)), nil

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

	case "apply":
		return applyApply(args, node, ip)

	case "map":
		return applyMap(args, node, ip)

	case "abs":
		if len(args) != 1 || args[0].typ != valInt {
			return nil, &EvalError{Message: fmt.Sprintf("%d:%d: abs: expected number", node.line, node.col)}
		}
		n := args[0].ival
		if n < 0 {
			n = -n
		}
		return intVal(n), nil

	case "modulo":
		if len(args) != 2 {
			return nil, &EvalError{Message: fmt.Sprintf("%d:%d: modulo: need 2 arguments", node.line, node.col)}
		}
		if err := requireInts(args, "modulo", node); err != nil {
			return nil, err
		}
		if args[1].ival == 0 {
			return nil, &EvalError{Message: fmt.Sprintf("%d:%d: modulo: division by zero", node.line, node.col)}
		}
		r := args[0].ival % args[1].ival
		// modulo takes sign of divisor
		if r != 0 && (r < 0) != (args[1].ival < 0) {
			r += args[1].ival
		}
		return intVal(r), nil

	case "remainder":
		if len(args) != 2 {
			return nil, &EvalError{Message: fmt.Sprintf("%d:%d: remainder: need 2 arguments", node.line, node.col)}
		}
		if err := requireInts(args, "remainder", node); err != nil {
			return nil, err
		}
		if args[1].ival == 0 {
			return nil, &EvalError{Message: fmt.Sprintf("%d:%d: remainder: division by zero", node.line, node.col)}
		}
		return intVal(args[0].ival % args[1].ival), nil

	case "quotient":
		if len(args) != 2 {
			return nil, &EvalError{Message: fmt.Sprintf("%d:%d: quotient: need 2 arguments", node.line, node.col)}
		}
		if err := requireInts(args, "quotient", node); err != nil {
			return nil, err
		}
		if args[1].ival == 0 {
			return nil, &EvalError{Message: fmt.Sprintf("%d:%d: quotient: division by zero", node.line, node.col)}
		}
		return intVal(args[0].ival / args[1].ival), nil

	case "min":
		if len(args) == 0 {
			return nil, &EvalError{Message: fmt.Sprintf("%d:%d: min: need at least 1 argument", node.line, node.col)}
		}
		if err := requireInts(args, "min", node); err != nil {
			return nil, err
		}
		m := args[0].ival
		for _, a := range args[1:] {
			if a.ival < m {
				m = a.ival
			}
		}
		return intVal(m), nil

	case "max":
		if len(args) == 0 {
			return nil, &EvalError{Message: fmt.Sprintf("%d:%d: max: need at least 1 argument", node.line, node.col)}
		}
		if err := requireInts(args, "max", node); err != nil {
			return nil, err
		}
		m := args[0].ival
		for _, a := range args[1:] {
			if a.ival > m {
				m = a.ival
			}
		}
		return intVal(m), nil

	case "expt":
		if len(args) != 2 {
			return nil, &EvalError{Message: fmt.Sprintf("%d:%d: expt: need 2 arguments", node.line, node.col)}
		}
		if err := requireInts(args, "expt", node); err != nil {
			return nil, err
		}
		base, exp := args[0].ival, args[1].ival
		result := int64(1)
		for i := int64(0); i < exp; i++ {
			result *= base
		}
		return intVal(result), nil

	case "zero?":
		if len(args) != 1 || args[0].typ != valInt {
			return nil, &EvalError{Message: fmt.Sprintf("%d:%d: zero?: expected number", node.line, node.col)}
		}
		return boolVal(args[0].ival == 0), nil

	case "positive?":
		if len(args) != 1 || args[0].typ != valInt {
			return nil, &EvalError{Message: fmt.Sprintf("%d:%d: positive?: expected number", node.line, node.col)}
		}
		return boolVal(args[0].ival > 0), nil

	case "negative?":
		if len(args) != 1 || args[0].typ != valInt {
			return nil, &EvalError{Message: fmt.Sprintf("%d:%d: negative?: expected number", node.line, node.col)}
		}
		return boolVal(args[0].ival < 0), nil

	case "odd?":
		if len(args) != 1 || args[0].typ != valInt {
			return nil, &EvalError{Message: fmt.Sprintf("%d:%d: odd?: expected number", node.line, node.col)}
		}
		return boolVal(args[0].ival%2 != 0), nil

	case "even?":
		if len(args) != 1 || args[0].typ != valInt {
			return nil, &EvalError{Message: fmt.Sprintf("%d:%d: even?: expected number", node.line, node.col)}
		}
		return boolVal(args[0].ival%2 == 0), nil

	case "list-ref":
		if len(args) != 2 {
			return nil, &EvalError{Message: fmt.Sprintf("%d:%d: list-ref: need 2 arguments", node.line, node.col)}
		}
		if args[1].typ != valInt {
			return nil, &EvalError{Message: fmt.Sprintf("%d:%d: list-ref: expected integer index", node.line, node.col)}
		}
		idx := int(args[1].ival)
		v := args[0]
		for i := 0; i < idx; i++ {
			if v.typ != valPair {
				return nil, &EvalError{Message: fmt.Sprintf("%d:%d: list-ref: index out of range", node.line, node.col)}
			}
			v = v.cdr
		}
		if v.typ != valPair {
			return nil, &EvalError{Message: fmt.Sprintf("%d:%d: list-ref: index out of range", node.line, node.col)}
		}
		return v.car, nil

	case "list-tail":
		if len(args) != 2 {
			return nil, &EvalError{Message: fmt.Sprintf("%d:%d: list-tail: need 2 arguments", node.line, node.col)}
		}
		if args[1].typ != valInt {
			return nil, &EvalError{Message: fmt.Sprintf("%d:%d: list-tail: expected integer index", node.line, node.col)}
		}
		idx := int(args[1].ival)
		v := args[0]
		for i := 0; i < idx; i++ {
			if v.typ != valPair {
				return nil, &EvalError{Message: fmt.Sprintf("%d:%d: list-tail: index out of range", node.line, node.col)}
			}
			v = v.cdr
		}
		return v, nil

	case "list?":
		if len(args) != 1 {
			return nil, &EvalError{Message: fmt.Sprintf("%d:%d: list?: need 1 argument", node.line, node.col)}
		}
		v := args[0]
		for v.typ == valPair {
			v = v.cdr
		}
		return boolVal(v.typ == valNil), nil

	case "assoc":
		if len(args) != 2 {
			return nil, &EvalError{Message: fmt.Sprintf("%d:%d: assoc: need 2 arguments", node.line, node.col)}
		}
		key := args[0]
		alist := args[1]
		for alist.typ == valPair {
			entry := alist.car
			if entry.typ == valPair && valEqual(key, entry.car) {
				return entry, nil
			}
			alist = alist.cdr
		}
		return boolVal(false), nil

	case "eq?":
		if len(args) != 2 {
			return nil, &EvalError{Message: fmt.Sprintf("%d:%d: eq?: need 2 arguments", node.line, node.col)}
		}
		return boolVal(valEq(args[0], args[1])), nil

	case "equal?":
		if len(args) != 2 {
			return nil, &EvalError{Message: fmt.Sprintf("%d:%d: equal?: need 2 arguments", node.line, node.col)}
		}
		return boolVal(valEqual(args[0], args[1])), nil

	case "char-alphabetic?":
		if len(args) != 1 || args[0].typ != valChar {
			return nil, &EvalError{Message: fmt.Sprintf("%d:%d: char-alphabetic?: expected char", node.line, node.col)}
		}
		return boolVal(unicode.IsLetter(rune(args[0].ival))), nil

	case "char-numeric?":
		if len(args) != 1 || args[0].typ != valChar {
			return nil, &EvalError{Message: fmt.Sprintf("%d:%d: char-numeric?: expected char", node.line, node.col)}
		}
		return boolVal(unicode.IsDigit(rune(args[0].ival))), nil

	case "char-upcase":
		if len(args) != 1 || args[0].typ != valChar {
			return nil, &EvalError{Message: fmt.Sprintf("%d:%d: char-upcase: expected char", node.line, node.col)}
		}
		return charVal(unicode.ToUpper(rune(args[0].ival))), nil

	case "char-downcase":
		if len(args) != 1 || args[0].typ != valChar {
			return nil, &EvalError{Message: fmt.Sprintf("%d:%d: char-downcase: expected char", node.line, node.col)}
		}
		return charVal(unicode.ToLower(rune(args[0].ival))), nil

	case "char=?":
		if len(args) != 2 || args[0].typ != valChar || args[1].typ != valChar {
			return nil, &EvalError{Message: fmt.Sprintf("%d:%d: char=?: expected chars", node.line, node.col)}
		}
		return boolVal(args[0].ival == args[1].ival), nil

	case "char<?":
		if len(args) != 2 || args[0].typ != valChar || args[1].typ != valChar {
			return nil, &EvalError{Message: fmt.Sprintf("%d:%d: char<?: expected chars", node.line, node.col)}
		}
		return boolVal(args[0].ival < args[1].ival), nil

	case "string=?":
		if len(args) != 2 || args[0].typ != valString || args[1].typ != valString {
			return nil, &EvalError{Message: fmt.Sprintf("%d:%d: string=?: expected strings", node.line, node.col)}
		}
		return boolVal(args[0].sval == args[1].sval), nil

	case "string<?":
		if len(args) != 2 || args[0].typ != valString || args[1].typ != valString {
			return nil, &EvalError{Message: fmt.Sprintf("%d:%d: string<?: expected strings", node.line, node.col)}
		}
		return boolVal(args[0].sval < args[1].sval), nil

	case "string-ci=?":
		if len(args) != 2 || args[0].typ != valString || args[1].typ != valString {
			return nil, &EvalError{Message: fmt.Sprintf("%d:%d: string-ci=?: expected strings", node.line, node.col)}
		}
		return boolVal(strings.EqualFold(args[0].sval, args[1].sval)), nil

	case "string-upcase":
		if len(args) != 1 || args[0].typ != valString {
			return nil, &EvalError{Message: fmt.Sprintf("%d:%d: string-upcase: expected string", node.line, node.col)}
		}
		return strVal(strings.ToUpper(args[0].sval)), nil

	case "string-downcase":
		if len(args) != 1 || args[0].typ != valString {
			return nil, &EvalError{Message: fmt.Sprintf("%d:%d: string-downcase: expected string", node.line, node.col)}
		}
		return strVal(strings.ToLower(args[0].sval)), nil

	case "char->integer":
		if len(args) != 1 || args[0].typ != valChar {
			return nil, &EvalError{Message: fmt.Sprintf("%d:%d: char->integer: expected char", node.line, node.col)}
		}
		return intVal(args[0].ival), nil

	case "integer->char":
		if len(args) != 1 || args[0].typ != valInt {
			return nil, &EvalError{Message: fmt.Sprintf("%d:%d: integer->char: expected integer", node.line, node.col)}
		}
		return charVal(rune(args[0].ival)), nil

	case "integer?":
		if len(args) != 1 {
			return nil, &EvalError{Message: fmt.Sprintf("%d:%d: integer?: need 1 argument", node.line, node.col)}
		}
		return boolVal(args[0].typ == valInt), nil

	case "rational?":
		if len(args) != 1 {
			return nil, &EvalError{Message: fmt.Sprintf("%d:%d: rational?: need 1 argument", node.line, node.col)}
		}
		return boolVal(args[0].typ == valInt || args[0].typ == valRational), nil

	case "exact?":
		if len(args) != 1 {
			return nil, &EvalError{Message: fmt.Sprintf("%d:%d: exact?: need 1 argument", node.line, node.col)}
		}
		return boolVal(isExact(args[0])), nil

	case "inexact?":
		if len(args) != 1 {
			return nil, &EvalError{Message: fmt.Sprintf("%d:%d: inexact?: need 1 argument", node.line, node.col)}
		}
		return boolVal(args[0].typ == valFloat), nil

	case "exact->inexact":
		if len(args) != 1 || !isNumeric(args[0]) {
			return nil, &EvalError{Message: fmt.Sprintf("%d:%d: exact->inexact: expected number", node.line, node.col)}
		}
		return floatVal(toFloat64(args[0])), nil

	case "inexact->exact":
		if len(args) != 1 || !isNumeric(args[0]) {
			return nil, &EvalError{Message: fmt.Sprintf("%d:%d: inexact->exact: expected number", node.line, node.col)}
		}
		if args[0].typ == valFloat {
			return floatToExact(args[0].fval), nil
		}
		return args[0], nil

	case "eqv?":
		if len(args) != 2 {
			return nil, &EvalError{Message: fmt.Sprintf("%d:%d: eqv?: need 2 arguments", node.line, node.col)}
		}
		return boolVal(valEqv(args[0], args[1])), nil

	case "vector":
		elems := make([]*Value, len(args))
		copy(elems, args)
		return &Value{typ: valVector, recordFields: elems}, nil

	case "make-vector":
		if len(args) < 1 || len(args) > 2 || args[0].typ != valInt {
			return nil, &EvalError{Message: fmt.Sprintf("%d:%d: make-vector: bad arguments", node.line, node.col)}
		}
		n := int(args[0].ival)
		fill := intVal(0)
		if len(args) == 2 {
			fill = args[1]
		}
		elems := make([]*Value, n)
		for i := range elems {
			elems[i] = fill
		}
		return &Value{typ: valVector, recordFields: elems}, nil

	case "vector-ref":
		if len(args) != 2 || args[0].typ != valVector || args[1].typ != valInt {
			return nil, &EvalError{Message: fmt.Sprintf("%d:%d: vector-ref: bad arguments", node.line, node.col)}
		}
		idx := int(args[1].ival)
		if idx < 0 || idx >= len(args[0].recordFields) {
			return nil, &EvalError{Message: fmt.Sprintf("%d:%d: vector-ref: index out of range", node.line, node.col)}
		}
		return args[0].recordFields[idx], nil

	case "vector-set!":
		if len(args) != 3 || args[0].typ != valVector || args[1].typ != valInt {
			return nil, &EvalError{Message: fmt.Sprintf("%d:%d: vector-set!: bad arguments", node.line, node.col)}
		}
		idx := int(args[1].ival)
		if idx < 0 || idx >= len(args[0].recordFields) {
			return nil, &EvalError{Message: fmt.Sprintf("%d:%d: vector-set!: index out of range", node.line, node.col)}
		}
		args[0].recordFields[idx] = args[2]
		return voidVal(), nil

	case "vector-length":
		if len(args) != 1 || args[0].typ != valVector {
			return nil, &EvalError{Message: fmt.Sprintf("%d:%d: vector-length: expected vector", node.line, node.col)}
		}
		return intVal(int64(len(args[0].recordFields))), nil

	case "vector?":
		if len(args) != 1 {
			return nil, &EvalError{Message: fmt.Sprintf("%d:%d: vector?: need 1 argument", node.line, node.col)}
		}
		return boolVal(args[0].typ == valVector), nil

	case "vector->list":
		if len(args) != 1 || args[0].typ != valVector {
			return nil, &EvalError{Message: fmt.Sprintf("%d:%d: vector->list: expected vector", node.line, node.col)}
		}
		result := nilVal()
		for i := len(args[0].recordFields) - 1; i >= 0; i-- {
			result = &Value{typ: valPair, car: args[0].recordFields[i], cdr: result}
		}
		return result, nil

	case "list->vector":
		if len(args) != 1 {
			return nil, &EvalError{Message: fmt.Sprintf("%d:%d: list->vector: need 1 argument", node.line, node.col)}
		}
		var elems []*Value
		v := args[0]
		for v.typ == valPair {
			elems = append(elems, v.car)
			v = v.cdr
		}
		return &Value{typ: valVector, recordFields: elems}, nil

	case "numerator":
		if len(args) != 1 {
			return nil, &EvalError{Message: fmt.Sprintf("%d:%d: numerator: need 1 argument", node.line, node.col)}
		}
		switch args[0].typ {
		case valInt:
			return args[0], nil
		case valRational:
			return intVal(args[0].ival), nil
		default:
			return nil, &EvalError{Message: fmt.Sprintf("%d:%d: numerator: expected rational", node.line, node.col)}
		}

	case "denominator":
		if len(args) != 1 {
			return nil, &EvalError{Message: fmt.Sprintf("%d:%d: denominator: need 1 argument", node.line, node.col)}
		}
		switch args[0].typ {
		case valInt:
			return intVal(1), nil
		case valRational:
			return intVal(args[0].dval), nil
		default:
			return nil, &EvalError{Message: fmt.Sprintf("%d:%d: denominator: expected rational", node.line, node.col)}
		}

	default:
		return nil, &EvalError{Message: fmt.Sprintf("%d:%d: unbound variable: %s", node.line, node.col, name)}
	}
}

func valEq(a, b *Value) bool {
	if a.typ != b.typ {
		return false
	}
	switch a.typ {
	case valInt:
		return a.ival == b.ival
	case valFloat:
		return a.fval == b.fval
	case valRational:
		return a.ival == b.ival && a.dval == b.dval
	case valBool:
		return a.bval == b.bval
	case valChar:
		return a.ival == b.ival
	case valSymbol:
		return a.sval == b.sval
	case valNil:
		return true
	case valVoid:
		return true
	default:
		return a == b
	}
}

func valEqual(a, b *Value) bool {
	if a.typ != b.typ {
		return false
	}
	switch a.typ {
	case valInt:
		return a.ival == b.ival
	case valFloat:
		return a.fval == b.fval
	case valRational:
		return a.ival == b.ival && a.dval == b.dval
	case valBool:
		return a.bval == b.bval
	case valChar:
		return a.ival == b.ival
	case valString:
		return a.sval == b.sval
	case valSymbol:
		return a.sval == b.sval
	case valNil:
		return true
	case valPair:
		return valEqual(a.car, b.car) && valEqual(a.cdr, b.cdr)
	case valVector:
		if len(a.recordFields) != len(b.recordFields) {
			return false
		}
		for i := range a.recordFields {
			if !valEqual(a.recordFields[i], b.recordFields[i]) {
				return false
			}
		}
		return true
	default:
		return a == b
	}
}

func applyMap(args []*Value, node *astNode, ip *interp) (*Value, error) {
	if len(args) < 2 {
		return nil, &EvalError{Message: fmt.Sprintf("%d:%d: map: need at least 2 arguments", node.line, node.col)}
	}
	fn := args[0]
	lists := args[1:]
	// Collect results
	var results []*Value
	for {
		// Check if any list is exhausted
		allPair := true
		for _, l := range lists {
			if l.typ == valNil {
				allPair = false
				break
			}
			if l.typ != valPair {
				return nil, &EvalError{Message: fmt.Sprintf("%d:%d: map: not a proper list", node.line, node.col)}
			}
		}
		if !allPair {
			break
		}
		// Gather car of each list
		callArgs := make([]*Value, len(lists))
		for i, l := range lists {
			callArgs[i] = l.car
		}
		// Apply fn
		var v *Value
		var err error
		if fn.typ == valLambda {
			v, err = applyLambda(fn, callArgs, node, ip)
		} else if fn.typ == valSymbol {
			v, err = applyBuiltin(fn.sval, callArgs, node, ip)
		} else {
			return nil, &EvalError{Message: fmt.Sprintf("%d:%d: map: not a procedure", node.line, node.col)}
		}
		if err != nil {
			return nil, err
		}
		results = append(results, v)
		// Advance all lists
		for i, l := range lists {
			lists[i] = l.cdr
		}
	}
	// Build result list
	result := nilVal()
	for i := len(results) - 1; i >= 0; i-- {
		result = &Value{typ: valPair, car: results[i], cdr: result}
	}
	return result, nil
}

func applyApply(args []*Value, node *astNode, ip *interp) (*Value, error) {
	if len(args) < 2 {
		return nil, &EvalError{Message: fmt.Sprintf("%d:%d: apply: need at least 2 arguments", node.line, node.col)}
	}
	fn := args[0]
	// Last argument must be a list; prefix args are prepended
	lastArg := args[len(args)-1]
	// Collect prefix args
	var allArgs []*Value
	for _, a := range args[1 : len(args)-1] {
		allArgs = append(allArgs, a)
	}
	// Unpack the last argument (a list)
	v := lastArg
	for v.typ == valPair {
		allArgs = append(allArgs, v.car)
		v = v.cdr
	}
	if v.typ != valNil {
		return nil, &EvalError{Message: fmt.Sprintf("%d:%d: apply: last argument is not a proper list", node.line, node.col)}
	}

	if fn.typ == valLambda {
		return applyLambda(fn, allArgs, node, ip)
	}
	if fn.typ == valSymbol {
		return applyBuiltin(fn.sval, allArgs, node, ip)
	}
	return nil, &EvalError{Message: fmt.Sprintf("%d:%d: apply: not a procedure", node.line, node.col)}
}

// evalLetrec implements both letrec and letrec* forms.
func evalLetrec(node *astNode, e *env, ip *interp, star bool) (*Value, error) {
	if len(node.children) < 3 {
		return nil, &EvalError{Message: fmt.Sprintf("%d:%d: letrec: bad syntax", node.line, node.col)}
	}
	bindings := node.children[1]
	if bindings.isAtom {
		return nil, &EvalError{Message: fmt.Sprintf("%d:%d: letrec: bad syntax", node.line, node.col)}
	}
	localEnv := newEnv(e)
	// First, bind all variables to undefined (void)
	names := make([]string, 0, len(bindings.children))
	for _, b := range bindings.children {
		if b.isAtom || len(b.children) != 2 {
			return nil, &EvalError{Message: fmt.Sprintf("%d:%d: letrec: bad binding", node.line, node.col)}
		}
		name := b.children[0]
		if !name.isAtom || name.tok.kind != tokSymbol {
			return nil, &EvalError{Message: fmt.Sprintf("%d:%d: letrec: bad binding", node.line, node.col)}
		}
		names = append(names, name.tok.sval)
		localEnv.set(name.tok.sval, voidVal())
	}
	// Now evaluate init expressions
	if star {
		// letrec*: evaluate sequentially, each init sees previous bindings
		for i, b := range bindings.children {
			val, err := eval(b.children[1], localEnv, ip)
			if err != nil {
				return nil, err
			}
			localEnv.set(names[i], val)
		}
	} else {
		// letrec: evaluate all inits in localEnv, then assign
		vals := make([]*Value, len(bindings.children))
		for i, b := range bindings.children {
			val, err := eval(b.children[1], localEnv, ip)
			if err != nil {
				return nil, err
			}
			vals[i] = val
		}
		for i, name := range names {
			localEnv.set(name, vals[i])
		}
	}
	// Evaluate body
	var result *Value
	for _, bodyExpr := range node.children[2:] {
		var err error
		result, err = eval(bodyExpr, localEnv, ip)
		if err != nil {
			return nil, err
		}
	}
	return result, nil
}

// evalCase implements (case expr ((datum ...) body ...) ... (else body ...))
func evalCase(node *astNode, e *env, ip *interp) (*Value, error) {
	if len(node.children) < 3 {
		return nil, &EvalError{Message: fmt.Sprintf("%d:%d: case: bad syntax", node.line, node.col)}
	}
	key, err := eval(node.children[1], e, ip)
	if err != nil {
		return nil, err
	}
	for _, clause := range node.children[2:] {
		if clause.isAtom || len(clause.children) < 2 {
			return nil, &EvalError{Message: fmt.Sprintf("%d:%d: case: bad clause", node.line, node.col)}
		}
		datums := clause.children[0]
		// Check for else clause
		if datums.isAtom && datums.tok.kind == tokSymbol && datums.tok.sval == "else" {
			var result *Value
			for _, bodyExpr := range clause.children[1:] {
				result, err = eval(bodyExpr, e, ip)
				if err != nil {
					return nil, err
				}
			}
			return result, nil
		}
		// Check if key matches any datum
		if datums.isAtom {
			return nil, &EvalError{Message: fmt.Sprintf("%d:%d: case: bad clause", node.line, node.col)}
		}
		matched := false
		for _, d := range datums.children {
			dv := quoteNode(d)
			if valEqv(key, dv) {
				matched = true
				break
			}
		}
		if matched {
			var result *Value
			for _, bodyExpr := range clause.children[1:] {
				result, err = eval(bodyExpr, e, ip)
				if err != nil {
					return nil, err
				}
			}
			return result, nil
		}
	}
	return voidVal(), nil
}

// valEqv implements eqv? semantics
func valEqv(a, b *Value) bool {
	return valEq(a, b)
}

// evalDo implements (do ((var init step) ...) (test expr ...) body ...)
func evalDo(node *astNode, e *env, ip *interp) (*Value, error) {
	if len(node.children) < 3 {
		return nil, &EvalError{Message: fmt.Sprintf("%d:%d: do: bad syntax", node.line, node.col)}
	}
	bindingsNode := node.children[1]
	testNode := node.children[2]
	bodyExprs := node.children[3:]

	if bindingsNode.isAtom {
		return nil, &EvalError{Message: fmt.Sprintf("%d:%d: do: bad syntax", node.line, node.col)}
	}
	if testNode.isAtom || len(testNode.children) < 1 {
		return nil, &EvalError{Message: fmt.Sprintf("%d:%d: do: bad syntax", node.line, node.col)}
	}

	type doVar struct {
		name    string
		stepIdx int // index in bindingsNode.children, -1 if no step
	}
	vars := make([]doVar, 0, len(bindingsNode.children))
	localEnv := newEnv(e)

	// Initialize variables
	for i, b := range bindingsNode.children {
		if b.isAtom || len(b.children) < 2 || len(b.children) > 3 {
			return nil, &EvalError{Message: fmt.Sprintf("%d:%d: do: bad variable spec", node.line, node.col)}
		}
		name := b.children[0]
		if !name.isAtom || name.tok.kind != tokSymbol {
			return nil, &EvalError{Message: fmt.Sprintf("%d:%d: do: bad variable spec", node.line, node.col)}
		}
		initVal, err := eval(b.children[1], e, ip)
		if err != nil {
			return nil, err
		}
		stepIdx := -1
		if len(b.children) == 3 {
			stepIdx = i
		}
		vars = append(vars, doVar{name: name.tok.sval, stepIdx: stepIdx})
		localEnv.set(name.tok.sval, initVal)
	}

	// Iterate
	for {
		// Evaluate test
		testResult, err := eval(testNode.children[0], localEnv, ip)
		if err != nil {
			return nil, err
		}
		if isTruthy(testResult) {
			// Evaluate result expressions
			if len(testNode.children) == 1 {
				return voidVal(), nil
			}
			var result *Value
			for _, expr := range testNode.children[1:] {
				result, err = eval(expr, localEnv, ip)
				if err != nil {
					return nil, err
				}
			}
			return result, nil
		}
		// Evaluate body (for side effects)
		for _, bodyExpr := range bodyExprs {
			_, err := eval(bodyExpr, localEnv, ip)
			if err != nil {
				return nil, err
			}
		}
		// Compute step values (using current env, parallel update)
		newVals := make([]*Value, len(vars))
		for i, v := range vars {
			if v.stepIdx >= 0 {
				stepExpr := bindingsNode.children[v.stepIdx].children[2]
				newVals[i], err = eval(stepExpr, localEnv, ip)
				if err != nil {
					return nil, err
				}
			}
		}
		// Update variables
		for i, v := range vars {
			if v.stepIdx >= 0 {
				localEnv.set(v.name, newVals[i])
			}
		}
	}
}

func makeGlobalEnv() *env {
	e := newEnv(nil)
	builtins := []string{"+", "-", "*", "/", "<", ">", "=", "<=", ">=", "not",
		"cons", "car", "cdr", "null?", "list", "length",
		"number?", "string?", "boolean?", "pair?", "symbol?", "char?",
		"integer?", "rational?", "procedure?",
		"append",
		"display", "write", "newline",
		"string-append", "string-length", "substring",
		"string->number", "number->string",
		"symbol->string", "string->symbol",
		"string-ref",
		"string-copy", "string-set!",
		"string->list", "list->string",
		"apply", "map",
		"abs", "modulo", "remainder", "quotient",
		"min", "max", "expt",
		"zero?", "positive?", "negative?", "odd?", "even?",
		"list-ref", "list-tail", "list?", "assoc",
		"eq?", "equal?",
		"char-alphabetic?", "char-numeric?",
		"char-upcase", "char-downcase",
		"char=?", "char<?",
		"string=?", "string<?", "string-ci=?",
		"string-upcase", "string-downcase",
		"char->integer", "integer->char",
		"exact?", "inexact?", "exact->inexact", "inexact->exact",
		"numerator", "denominator",
		"eqv?",
		"vector", "make-vector", "vector-ref", "vector-set!", "vector-length", "vector?",
		"vector->list", "list->vector"}
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

// evalDefineRecordType implements R7RS define-record-type.
// (define-record-type <name> (constructor field ...) predicate (field accessor) ...)
func evalDefineRecordType(node *astNode, e *env) (*Value, error) {
	// Expect at least 4 children: define-record-type, <name>, (constructor fields...), predicate, field-specs...
	if len(node.children) < 4 {
		return nil, &EvalError{Message: fmt.Sprintf("%d:%d: define-record-type: bad syntax", node.line, node.col)}
	}

	// Parse type name
	typeName := node.children[1]
	if !typeName.isAtom || typeName.tok.kind != tokSymbol {
		return nil, &EvalError{Message: fmt.Sprintf("%d:%d: define-record-type: expected type name", node.line, node.col)}
	}

	// Parse constructor: (constructor-name field ...)
	ctorNode := node.children[2]
	if ctorNode.isAtom || len(ctorNode.children) < 1 {
		return nil, &EvalError{Message: fmt.Sprintf("%d:%d: define-record-type: bad constructor", node.line, node.col)}
	}
	ctorName := ctorNode.children[0].tok.sval
	var ctorFields []string
	for _, c := range ctorNode.children[1:] {
		ctorFields = append(ctorFields, c.tok.sval)
	}

	// Parse predicate name
	predNode := node.children[3]
	if !predNode.isAtom || predNode.tok.kind != tokSymbol {
		return nil, &EvalError{Message: fmt.Sprintf("%d:%d: define-record-type: expected predicate name", node.line, node.col)}
	}
	predName := predNode.tok.sval

	// Parse field specs: (field-name accessor-name)
	// Build a map from field name to its index in the constructor
	fieldIndex := make(map[string]int)
	for i, f := range ctorFields {
		fieldIndex[f] = i
	}

	// Create the record type descriptor
	rt := &recordType{
		name:       typeName.tok.sval,
		fieldNames: ctorFields,
	}
	recordTypeCounter++

	// Define constructor
	numFields := len(ctorFields)
	e.set(ctorName, &Value{
		typ:    valGoFunc,
		goName: ctorName,
		goFunc: func(args []*Value) (*Value, error) {
			if len(args) != numFields {
				return nil, &EvalError{Message: fmt.Sprintf("constructor %s: expected %d args, got %d", ctorName, numFields, len(args))}
			}
			fields := make([]*Value, numFields)
			copy(fields, args)
			return &Value{typ: valRecord, recordTag: rt, recordFields: fields}, nil
		},
	})

	// Define predicate
	e.set(predName, &Value{
		typ:    valGoFunc,
		goName: predName,
		goFunc: func(args []*Value) (*Value, error) {
			if len(args) != 1 {
				return nil, &EvalError{Message: fmt.Sprintf("%s: expected 1 arg", predName)}
			}
			return boolVal(args[0].typ == valRecord && args[0].recordTag == rt), nil
		},
	})

	// Define field accessors
	for i := 4; i < len(node.children); i++ {
		fieldSpec := node.children[i]
		if fieldSpec.isAtom || len(fieldSpec.children) < 2 {
			return nil, &EvalError{Message: fmt.Sprintf("%d:%d: define-record-type: bad field spec", node.line, node.col)}
		}
		fieldName := fieldSpec.children[0].tok.sval
		accessorName := fieldSpec.children[1].tok.sval
		idx, ok := fieldIndex[fieldName]
		if !ok {
			return nil, &EvalError{Message: fmt.Sprintf("%d:%d: define-record-type: unknown field %s", node.line, node.col, fieldName)}
		}
		capturedIdx := idx
		capturedAccessor := accessorName
		e.set(accessorName, &Value{
			typ:    valGoFunc,
			goName: accessorName,
			goFunc: func(args []*Value) (*Value, error) {
				if len(args) != 1 {
					return nil, &EvalError{Message: fmt.Sprintf("%s: expected 1 arg", capturedAccessor)}
				}
				if args[0].typ != valRecord || args[0].recordTag != rt {
					return nil, &EvalError{Message: fmt.Sprintf("%s: not a %s record", capturedAccessor, rt.name)}
				}
				return args[0].recordFields[capturedIdx], nil
			},
		})
	}

	return voidVal(), nil
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
