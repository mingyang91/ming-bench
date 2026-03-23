package ming

import (
	"fmt"
	"math"
	"strconv"
	"strings"
	"unicode"
)

// ---------- Values ----------

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
	valBuiltin
	valChar
	valContinuation
	valMacro
	valVector
	valMultipleValues
	valFloat
	valRational
	valRecord
	valSyntax // wraps an *expr (syntax object)
)

type value struct {
	typ    valueType
	ival   int64
	bval   bool
	sval   string
	car    *value
	cdr    *value
	// lambda fields
	params    []string
	restParam string // "" if none, otherwise the name of the rest parameter
	body      []*expr
	closure   *env
	// float
	fval float64
	// rational (num/den, always simplified, den > 0)
	num int64
	den int64
	// char
	cval rune
	// builtin function
	builtin func(args []*value, line, col int) (*value, error)
	// continuation fields
	contExpr      *expr    // the call/cc expression (for replay matching)
	contIdx       int      // top-level expression index
	contLetStack  []letCtx // stack of enclosing let contexts at capture time
	contWindStack []windEntry // dynamic-wind stack at capture time
	// call/cc marker
	isCallCC bool
	// mutable flag (e.g., strings from string-copy)
	mutable bool
	// macro fields
	macroRules    []syntaxRule
	macroLiterals []string
	macroDefEnv   *env
	// vector
	vecval []*value
	// multiple values
	multiVals []*value
	// record fields
	recType   *recordType
	recFields []*value
	// syntax object (wraps an *expr)
	syntaxExpr *expr
	// macro transformer (lambda-based, for syntax-case macros)
	macroTransformer *value
	// case-lambda clauses
	caseClauses []caseClause
}

// caseClause represents one clause in a case-lambda
type caseClause struct {
	params    []string
	restParam string
	body      []*expr
}

// recordType holds metadata for a define-record-type
type recordType struct {
	name       string   // e.g., "<point>"
	fieldNames []string // field names in constructor order
}

var voidVal = &value{typ: valVoid}
var nilVal = &value{typ: valNil}

func intVal(n int64) *value   { return &value{typ: valInt, ival: n} }
func boolVal(b bool) *value   { return &value{typ: valBool, bval: b} }
func strVal(s string) *value  { return &value{typ: valString, sval: s} }
func symVal(s string) *value  { return &value{typ: valSymbol, sval: s} }

func (v *value) isTruthy() bool {
	return !(v.typ == valBool && !v.bval)
}

func floatVal(f float64) *value { return &value{typ: valFloat, fval: f} }

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

func ratVal(n, d int64) *value {
	if d < 0 {
		n, d = -n, -d
	}
	g := gcd(n, d)
	n, d = n/g, d/g
	if d == 1 {
		return intVal(n)
	}
	return &value{typ: valRational, num: n, den: d}
}

func isNumeric(v *value) bool {
	return v.typ == valInt || v.typ == valFloat || v.typ == valRational
}

// toFloat converts any numeric value to float64
func toFloat(v *value) float64 {
	switch v.typ {
	case valInt:
		return float64(v.ival)
	case valFloat:
		return v.fval
	case valRational:
		return float64(v.num) / float64(v.den)
	}
	return 0
}

// toRational converts int or rational to (num, den) pair; returns ok=false for float
func toRational(v *value) (int64, int64, bool) {
	switch v.typ {
	case valInt:
		return v.ival, 1, true
	case valRational:
		return v.num, v.den, true
	}
	return 0, 0, false
}

func isExact(v *value) bool {
	return v.typ == valInt || v.typ == valRational
}

// floatToRational converts a float64 to a numerator/denominator pair
func floatToRational(f float64) (int64, int64) {
	if f == math.Trunc(f) {
		return int64(f), 1
	}
	// Multiply by powers of 10 to clear decimal
	neg := f < 0
	if neg {
		f = -f
	}
	d := int64(1)
	for i := 0; i < 16; i++ {
		if f == math.Trunc(f) {
			break
		}
		f *= 10
		d *= 10
	}
	n := int64(math.Round(f))
	if neg {
		n = -n
	}
	g := gcd(n, d)
	return n / g, d / g
}

func charVal(c rune) *value { return &value{typ: valChar, cval: c} }

func (v *value) String() string {
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
		return printList(v)
	case valVoid:
		return ""
	case valChar:
		return formatChar(v.cval)
	case valContinuation:
		return "#<continuation>"
	case valVector:
		var buf strings.Builder
		buf.WriteString("#(")
		for i, el := range v.vecval {
			if i > 0 {
				buf.WriteByte(' ')
			}
			buf.WriteString(el.String())
		}
		buf.WriteByte(')')
		return buf.String()
	case valFloat:
		s := strconv.FormatFloat(v.fval, 'f', -1, 64)
		// Ensure there's a decimal point
		if !strings.Contains(s, ".") {
			s += ".0"
		}
		return s
	case valRational:
		return fmt.Sprintf("%d/%d", v.num, v.den)
	case valRecord:
		return fmt.Sprintf("#<%s>", v.recType.name)
	case valSyntax:
		return "#<syntax>"
	}
	return ""
}

// displayString returns the display representation (no quotes for strings).
func (v *value) displayString() string {
	switch v.typ {
	case valString:
		return v.sval
	case valChar:
		return string(v.cval)
	default:
		return v.String()
	}
}

func formatChar(c rune) string {
	switch c {
	case ' ':
		return `#\space`
	case '\n':
		return `#\newline`
	case '\t':
		return `#\tab`
	default:
		return `#\` + string(c)
	}
}

func printList(v *value) string {
	var buf strings.Builder
	buf.WriteByte('(')
	cur := v
	slow := v
	first := true
	step := 0
	for cur.typ == valPair {
		if !first {
			buf.WriteByte(' ')
		}
		buf.WriteString(cur.car.String())
		first = false
		cur = cur.cdr
		step++
		if step%2 == 0 {
			slow = slow.cdr
			if slow == cur {
				buf.WriteString(" ...")
				buf.WriteByte(')')
				return buf.String()
			}
		}
	}
	if cur.typ != valNil {
		buf.WriteString(" . ")
		buf.WriteString(cur.String())
	}
	buf.WriteByte(')')
	return buf.String()
}

// ---------- Tokenizer ----------

type token struct {
	text string
	line int
	col  int
}

func tokenize(input string) []token {
	var tokens []token
	i := 0
	line := 1
	col := 1
	runes := []rune(input)
	n := len(runes)

	for i < n {
		ch := runes[i]

		// Skip whitespace
		if unicode.IsSpace(ch) {
			if ch == '\n' {
				line++
				col = 1
			} else {
				col++
			}
			i++
			continue
		}

		// Skip comments
		if ch == ';' {
			for i < n && runes[i] != '\n' {
				i++
			}
			continue
		}

		// Parens
		if ch == '(' || ch == ')' {
			tokens = append(tokens, token{string(ch), line, col})
			i++
			col++
			continue
		}

		// String literal
		if ch == '"' {
			startLine, startCol := line, col
			var buf strings.Builder
			i++
			col++
			for i < n && runes[i] != '"' {
				if runes[i] == '\\' && i+1 < n {
					i++
					col++
					switch runes[i] {
					case 'n':
						buf.WriteByte('\n')
					case 't':
						buf.WriteByte('\t')
					case '\\':
						buf.WriteByte('\\')
					case '"':
						buf.WriteByte('"')
					default:
						buf.WriteRune(runes[i])
					}
				} else {
					if runes[i] == '\n' {
						line++
						col = 0
					}
					buf.WriteRune(runes[i])
				}
				i++
				col++
			}
			if i < n {
				i++ // skip closing quote
				col++
			}
			tokens = append(tokens, token{`"` + buf.String() + `"`, startLine, startCol})
			continue
		}

		// Syntax template shorthand #'
		if ch == '#' && i+1 < n && runes[i+1] == '\'' {
			tokens = append(tokens, token{"#'", line, col})
			i += 2
			col += 2
			continue
		}

		// Quote shorthand
		if ch == '\'' {
			tokens = append(tokens, token{"'", line, col})
			i++
			col++
			continue
		}

		// Quasiquote
		if ch == '`' {
			tokens = append(tokens, token{"`", line, col})
			i++
			col++
			continue
		}

		// Unquote / unquote-splicing
		if ch == ',' {
			if i+1 < n && runes[i+1] == '@' {
				tokens = append(tokens, token{",@", line, col})
				i += 2
				col += 2
			} else {
				tokens = append(tokens, token{",", line, col})
				i++
				col++
			}
			continue
		}

		// Atom (symbol, number, boolean)
		startCol := col
		start := i
		for i < n && !unicode.IsSpace(runes[i]) && runes[i] != '(' && runes[i] != ')' && runes[i] != '"' && runes[i] != ';' {
			i++
			col++
		}
		tokens = append(tokens, token{string(runes[start:i]), line, startCol})
	}

	return tokens
}

// ---------- Parser ----------

type expr struct {
	kind   string // "int", "bool", "string", "symbol", "list", "float", "rational"
	ival   int64
	bval   bool
	sval   string
	fval   float64
	ival2  int64 // denominator for rational
	items  []*expr
	dotted bool // true if improper list: last item is cdr
	line   int
	col    int
	envRef *env // hygienic macro: resolve symbol in this env instead of current
}

func parse(tokens []token) ([]*expr, error) {
	pos := 0
	var results []*expr
	for pos < len(tokens) {
		e, newpos, err := parseExpr(tokens, pos)
		if err != nil {
			return nil, err
		}
		results = append(results, e)
		pos = newpos
	}
	return results, nil
}

func parseExpr(tokens []token, pos int) (*expr, int, error) {
	if pos >= len(tokens) {
		return nil, pos, fmt.Errorf("unexpected end of input")
	}
	tok := tokens[pos]

	if tok.text == "(" {
		var items []*expr
		dotted := false
		pos++
		for pos < len(tokens) && tokens[pos].text != ")" {
			if tokens[pos].text == "." {
				// Dotted pair notation: (a b . c)
				pos++ // skip "."
				if pos >= len(tokens) || tokens[pos].text == ")" {
					return nil, 0, &EvalError{Message: fmt.Sprintf("%d:%d: bad dotted pair", tok.line, tok.col)}
				}
				last, newpos, err := parseExpr(tokens, pos)
				if err != nil {
					return nil, 0, err
				}
				items = append(items, last)
				dotted = true
				pos = newpos
				break
			}
			e, newpos, err := parseExpr(tokens, pos)
			if err != nil {
				return nil, 0, err
			}
			items = append(items, e)
			pos = newpos
		}
		if pos >= len(tokens) || tokens[pos].text != ")" {
			return nil, 0, &EvalError{Message: fmt.Sprintf("%d:%d: unterminated list", tok.line, tok.col)}
		}
		pos++ // skip ")"
		return &expr{kind: "list", items: items, dotted: dotted, line: tok.line, col: tok.col}, pos, nil
	}

	if tok.text == ")" {
		return nil, 0, &EvalError{Message: fmt.Sprintf("%d:%d: unexpected ')'", tok.line, tok.col)}
	}

	if tok.text == "'" {
		e, newpos, err := parseExpr(tokens, pos+1)
		if err != nil {
			return nil, 0, err
		}
		return &expr{kind: "list", items: []*expr{{kind: "symbol", sval: "quote", line: tok.line, col: tok.col}, e}, line: tok.line, col: tok.col}, newpos, nil
	}

	// Quasiquote: `expr → (quasiquote expr)
	if tok.text == "`" {
		e, newpos, err := parseExpr(tokens, pos+1)
		if err != nil {
			return nil, 0, err
		}
		return &expr{kind: "list", items: []*expr{{kind: "symbol", sval: "quasiquote", line: tok.line, col: tok.col}, e}, line: tok.line, col: tok.col}, newpos, nil
	}

	// Unquote: ,expr → (unquote expr)
	if tok.text == "," {
		e, newpos, err := parseExpr(tokens, pos+1)
		if err != nil {
			return nil, 0, err
		}
		return &expr{kind: "list", items: []*expr{{kind: "symbol", sval: "unquote", line: tok.line, col: tok.col}, e}, line: tok.line, col: tok.col}, newpos, nil
	}

	// Unquote-splicing: ,@expr → (unquote-splicing expr)
	if tok.text == ",@" {
		e, newpos, err := parseExpr(tokens, pos+1)
		if err != nil {
			return nil, 0, err
		}
		return &expr{kind: "list", items: []*expr{{kind: "symbol", sval: "unquote-splicing", line: tok.line, col: tok.col}, e}, line: tok.line, col: tok.col}, newpos, nil
	}

	// Syntax template shorthand: #'expr → (syntax expr)
	if tok.text == "#'" {
		e, newpos, err := parseExpr(tokens, pos+1)
		if err != nil {
			return nil, 0, err
		}
		return &expr{kind: "list", items: []*expr{{kind: "symbol", sval: "syntax", line: tok.line, col: tok.col}, e}, line: tok.line, col: tok.col}, newpos, nil
	}

	// String literal
	if len(tok.text) >= 2 && tok.text[0] == '"' && tok.text[len(tok.text)-1] == '"' {
		return &expr{kind: "string", sval: tok.text[1 : len(tok.text)-1], line: tok.line, col: tok.col}, pos + 1, nil
	}

	// Character literal
	if len(tok.text) >= 3 && tok.text[0] == '#' && tok.text[1] == '\\' {
		rest := tok.text[2:]
		var c rune
		switch rest {
		case "space":
			c = ' '
		case "newline":
			c = '\n'
		case "tab":
			c = '\t'
		default:
			runes := []rune(rest)
			if len(runes) == 1 {
				c = runes[0]
			} else {
				return nil, 0, &EvalError{Message: fmt.Sprintf("%d:%d: bad character literal: %s", tok.line, tok.col, tok.text)}
			}
		}
		return &expr{kind: "char", line: tok.line, col: tok.col, sval: string(c)}, pos + 1, nil
	}

	// Boolean
	if tok.text == "#t" || tok.text == "#true" {
		return &expr{kind: "bool", bval: true, line: tok.line, col: tok.col}, pos + 1, nil
	}
	if tok.text == "#f" || tok.text == "#false" {
		return &expr{kind: "bool", bval: false, line: tok.line, col: tok.col}, pos + 1, nil
	}

	// Integer
	if n, err := strconv.ParseInt(tok.text, 10, 64); err == nil {
		return &expr{kind: "int", ival: n, line: tok.line, col: tok.col}, pos + 1, nil
	}

	// Rational literal: digits/digits (e.g., 1/3, -6/4)
	if idx := strings.Index(tok.text, "/"); idx > 0 && idx < len(tok.text)-1 {
		numStr := tok.text[:idx]
		denStr := tok.text[idx+1:]
		if n, err1 := strconv.ParseInt(numStr, 10, 64); err1 == nil {
			if d, err2 := strconv.ParseInt(denStr, 10, 64); err2 == nil && d != 0 {
				return &expr{kind: "rational", ival: n, ival2: d, line: tok.line, col: tok.col}, pos + 1, nil
			}
		}
	}

	// Float literal
	if f, err := strconv.ParseFloat(tok.text, 64); err == nil {
		return &expr{kind: "float", fval: f, line: tok.line, col: tok.col}, pos + 1, nil
	}

	// Symbol
	return &expr{kind: "symbol", sval: tok.text, line: tok.line, col: tok.col}, pos + 1, nil
}

// ---------- Environment ----------

type env struct {
	bindings map[string]*value
	parent   *env
	// syntax-case context for #' template expansion
	syntaxBindings *matchResult
	syntaxDefEnv   *env
	syntaxPatVars  map[string]bool
}

func newEnv(parent *env) *env {
	return &env{bindings: make(map[string]*value), parent: parent}
}

func (e *env) get(name string) (*value, bool) {
	if v, ok := e.bindings[name]; ok {
		return v, true
	}
	if e.parent != nil {
		return e.parent.get(name)
	}
	return nil, false
}

func (e *env) set(name string, v *value) {
	e.bindings[name] = v
}

// setExisting mutates an existing binding, walking up the chain. Returns false if unbound.
func (e *env) setExisting(name string, v *value) bool {
	if _, ok := e.bindings[name]; ok {
		e.bindings[name] = v
		return true
	}
	if e.parent != nil {
		return e.parent.setExisting(name, v)
	}
	return false
}

// ---------- Continuations ----------

// letCtx tracks a let expression, its environment, and the body index being evaluated.
type letCtx struct {
	expr    *expr
	env     *env
	bodyIdx int
}

// windEntry tracks a dynamic-wind in/out thunk pair.
type windEntry struct {
	inThunk  *value
	outThunk *value
}

// schemeRaise is panicked when (raise val) is called.
type schemeRaise struct {
	val *value
}

// continuationJump is panicked when a continuation is invoked.
type continuationJump struct {
	contExpr  *expr  // the call/cc expression to replay from
	contIdx   int    // top-level expression index to replay from
	val       *value // value to deliver to the continuation
	letStack  []letCtx // stack of enclosing let contexts at capture time
	windStack []windEntry // dynamic-wind stack at capture time
}

// ---------- Interpreter ----------

type interp struct {
	output      strings.Builder
	exprs       []*expr // all top-level expressions
	exprIdx     int     // current top-level expression index
	replayExpr  *expr   // if non-nil, the call/cc expr to short-circuit
	replayValue *value  // value to return from the replayed call/cc
	// Let environment stack for continuation capture
	letStack []letCtx
	// Replay: stack of let contexts to match during replay
	replayLetStack []letCtx
	// Dynamic-wind stack
	windStack []windEntry
}

// ---------- Evaluator ----------

func (ip *interp) eval(e *expr, envir *env) (*value, error) {
	for {
		switch e.kind {
		case "int":
			return intVal(e.ival), nil
		case "float":
			return floatVal(e.fval), nil
		case "rational":
			return ratVal(e.ival, e.ival2), nil
		case "bool":
			return boolVal(e.bval), nil
		case "string":
			return strVal(e.sval), nil
		case "char":
			runes := []rune(e.sval)
			return charVal(runes[0]), nil
		case "symbol":
			lookupEnv := envir
			if e.envRef != nil {
				lookupEnv = e.envRef
			}
			v, ok := lookupEnv.get(e.sval)
			if !ok {
				return nil, &EvalError{Message: fmt.Sprintf("%d:%d: unbound variable: %s", e.line, e.col, e.sval)}
			}
			return v, nil
		case "list":
			if len(e.items) == 0 {
				return nil, &EvalError{Message: fmt.Sprintf("%d:%d: empty application", e.line, e.col)}
			}
			head := e.items[0]

			if head.kind == "symbol" {
				switch head.sval {
				case "and":
					exprs := e.items[1:]
					if len(exprs) == 0 {
						return boolVal(true), nil
					}
					for _, ae := range exprs[:len(exprs)-1] {
						v, err := ip.eval(ae, envir)
						if err != nil {
							return nil, err
						}
						if !v.isTruthy() {
							return v, nil
						}
					}
					e = exprs[len(exprs)-1]
					continue

				case "or":
					exprs := e.items[1:]
					if len(exprs) == 0 {
						return boolVal(false), nil
					}
					for _, oe := range exprs[:len(exprs)-1] {
						v, err := ip.eval(oe, envir)
						if err != nil {
							return nil, err
						}
						if v.isTruthy() {
							return v, nil
						}
					}
					e = exprs[len(exprs)-1]
					continue

				case "define":
					return ip.evalDefine(e, envir)

				case "set!":
					if len(e.items) != 3 {
						return nil, &EvalError{Message: fmt.Sprintf("%d:%d: set!: bad syntax", e.line, e.col)}
					}
					name := e.items[1].sval
					val, err := ip.eval(e.items[2], envir)
					if err != nil {
						return nil, err
					}
					if !envir.setExisting(name, val) {
						return nil, &EvalError{Message: fmt.Sprintf("%d:%d: set!: unbound variable: %s", e.line, e.col, name)}
					}
					return voidVal, nil

				case "if":
					if len(e.items) < 3 || len(e.items) > 4 {
						return nil, &EvalError{Message: fmt.Sprintf("%d:%d: if: bad syntax", e.line, e.col)}
					}
					cond, err := ip.eval(e.items[1], envir)
					if err != nil {
						return nil, err
					}
					if cond.isTruthy() {
						e = e.items[2]
						continue
					}
					if len(e.items) == 4 {
						e = e.items[3]
						continue
					}
					return voidVal, nil

				case "quote":
					if len(e.items) != 2 {
						return nil, &EvalError{Message: fmt.Sprintf("%d:%d: quote: expected 1 argument", e.line, e.col)}
					}
					return quoteExpr(e.items[1]), nil

				case "quasiquote":
					if len(e.items) != 2 {
						return nil, &EvalError{Message: fmt.Sprintf("%d:%d: quasiquote: expected 1 argument", e.line, e.col)}
					}
					return ip.evalQuasiquote(e.items[1], envir)

				case "lambda":
					return evalLambda(e, envir)

				case "case-lambda":
					return evalCaseLambda(e, envir)

				case "let":
					newE, newEnvir, err := ip.setupLet(e, envir)
					if err != nil {
						return nil, err
					}
					e = newE
					envir = newEnvir
					continue

				case "begin":
					exprs := e.items[1:]
					if len(exprs) == 0 {
						return voidVal, nil
					}
					for _, be := range exprs[:len(exprs)-1] {
						_, err := ip.eval(be, envir)
						if err != nil {
							return nil, err
						}
					}
					e = exprs[len(exprs)-1]
					continue

				case "cond":
					found := false
					for _, clause := range e.items[1:] {
						if clause.kind != "list" || len(clause.items) < 1 {
							return nil, &EvalError{Message: fmt.Sprintf("%d:%d: cond: bad clause", e.line, e.col)}
						}
						if clause.items[0].kind == "symbol" && clause.items[0].sval == "else" {
							body := clause.items[1:]
							for _, be := range body[:len(body)-1] {
								_, err := ip.eval(be, envir)
								if err != nil {
									return nil, err
								}
							}
							e = body[len(body)-1]
							found = true
							break
						}
						test, err := ip.eval(clause.items[0], envir)
						if err != nil {
							return nil, err
						}
						if test.isTruthy() {
							if len(clause.items) == 1 {
								// (cond (test)) — return test value
								return test, nil
							}
							// (cond (test => proc)) — apply proc to test value
							if len(clause.items) == 3 && clause.items[1].kind == "symbol" && clause.items[1].sval == "=>" {
								proc, err := ip.eval(clause.items[2], envir)
								if err != nil {
									return nil, err
								}
								return ip.applyProc(proc, []*value{test}, clause.line, clause.col)
							}
							body := clause.items[1:]
							for _, be := range body[:len(body)-1] {
								_, err := ip.eval(be, envir)
								if err != nil {
									return nil, err
								}
							}
							e = body[len(body)-1]
							found = true
							break
						}
					}
					if !found {
						return voidVal, nil
					}
					continue

				case "let*":
					if len(e.items) < 3 {
						return nil, &EvalError{Message: fmt.Sprintf("%d:%d: let*: bad syntax", e.line, e.col)}
					}
					bindingsExpr := e.items[1]
					if bindingsExpr.kind != "list" {
						return nil, &EvalError{Message: fmt.Sprintf("%d:%d: let*: bad syntax", e.line, e.col)}
					}
					localEnv := newEnv(envir)
					for _, b := range bindingsExpr.items {
						if b.kind != "list" || len(b.items) != 2 {
							return nil, &EvalError{Message: fmt.Sprintf("%d:%d: let*: bad binding", e.line, e.col)}
						}
						v, err := ip.eval(b.items[1], localEnv)
						if err != nil {
							return nil, err
						}
						localEnv.set(b.items[0].sval, v)
					}
					body := e.items[2:]
					for _, be := range body[:len(body)-1] {
						_, err := ip.eval(be, localEnv)
						if err != nil {
							return nil, err
						}
					}
					e = body[len(body)-1]
					envir = localEnv
					continue

				case "letrec":
					newE, newEnvir, err := ip.evalLetrec(e, envir, false)
					if err != nil {
						return nil, err
					}
					e = newE
					envir = newEnvir
					continue

				case "letrec*":
					newE, newEnvir, err := ip.evalLetrec(e, envir, true)
					if err != nil {
						return nil, err
					}
					e = newE
					envir = newEnvir
					continue

				case "case":
					newE, err := ip.evalCase(e, envir)
					if err != nil {
						return nil, err
					}
					if newE == nil {
						return voidVal, nil
					}
					e = newE
					continue

				case "do":
					result, tailE, tailEnv, err := ip.evalDo(e, envir)
					if err != nil {
						return nil, err
					}
					if tailE != nil {
						e = tailE
						envir = tailEnv
						continue
					}
					return result, nil

				case "define-syntax":
					return ip.evalDefineSyntax(e, envir)

				case "define-record-type":
					return ip.evalDefineRecordType(e, envir)

				case "dynamic-wind":
					return ip.evalDynamicWind(e, envir)

				case "guard":
					return ip.evalGuard(e, envir)

				case "with-exception-handler":
					return ip.evalWithExceptionHandler(e, envir)

				case "syntax-case":
					return ip.evalSyntaxCase(e, envir)

				case "syntax":
					return ip.evalSyntax(e, envir)

				case "with-syntax":
					return ip.evalWithSyntax(e, envir)
				}

				// Check for macro expansion
				macroLookupEnv := envir
				if head.envRef != nil {
					macroLookupEnv = head.envRef
				}
				if mv, ok := macroLookupEnv.get(head.sval); ok && mv.typ == valMacro {
					if mv.macroTransformer != nil {
						expanded, expandErr := ip.applySyntaxTransformer(mv, e)
						if expandErr != nil {
							return nil, expandErr
						}
						e = expanded
						continue
					}
					expanded, expandErr := expandMacro(mv, e, head.sval)
					if expandErr != nil {
						return nil, expandErr
					}
					e = expanded
					continue
				}
			}

			// Function call
			fn, err := ip.eval(head, envir)
			if err != nil {
				return nil, err
			}

			args := make([]*value, len(e.items)-1)
			for i, arg := range e.items[1:] {
				v, err := ip.eval(arg, envir)
				if err != nil {
					return nil, err
				}
				args[i] = v
			}

			// call/cc: handle specially
			if fn.isCallCC {
				if len(args) != 1 {
					return nil, &EvalError{Message: fmt.Sprintf("%d:%d: call/cc: expected 1 argument", e.line, e.col)}
				}
				return ip.handleCallCC(args[0], e)
			}

			// Continuation invocation
			if fn.typ == valContinuation {
				var jumpVal *value
				if len(args) == 1 {
					jumpVal = args[0]
				} else {
					jumpVal = &value{typ: valMultipleValues, multiVals: args}
				}
				panic(&continuationJump{
					contExpr: fn.contExpr,
					contIdx:  fn.contIdx,
					val:      jumpVal,
					letStack: fn.contLetStack,
				})
			}

			// TCO: inline lambda application
			if fn.typ == valLambda {
				localEnv, bindErr := bindLambdaArgs(fn, args, e.line, e.col)
				if bindErr != nil {
					return nil, bindErr
				}
				// Eval all body exprs except last, then tail-call last
				for _, bodyExpr := range fn.body[:len(fn.body)-1] {
					_, err := ip.eval(bodyExpr, localEnv)
					if err != nil {
						return nil, err
					}
				}
				e = fn.body[len(fn.body)-1]
				envir = localEnv
				continue
			}

			return applyFunc(fn, args, e)
		}
		return nil, &EvalError{Message: fmt.Sprintf("%d:%d: unknown expression type", e.line, e.col)}
	}
}

// handleCallCC implements call/cc. It captures a continuation and calls proc with it.
func (ip *interp) handleCallCC(proc *value, e *expr) (*value, error) {
	// Check for replay: if this is the call/cc we're replaying, return the value
	if ip.replayExpr == e {
		val := ip.replayValue
		ip.replayExpr = nil
		ip.replayValue = nil
		return val, nil
	}

	// Create continuation value — capture the current let stack and wind stack
	savedLetStack := make([]letCtx, len(ip.letStack))
	copy(savedLetStack, ip.letStack)
	savedWindStack := make([]windEntry, len(ip.windStack))
	copy(savedWindStack, ip.windStack)

	cont := &value{
		typ:           valContinuation,
		contExpr:      e,
		contIdx:       ip.exprIdx,
		contLetStack:  savedLetStack,
		contWindStack: savedWindStack,
	}

	// Call proc(cont) with escape recovery
	var result *value
	var resultErr error

	func() {
		defer func() {
			if r := recover(); r != nil {
				if jump, ok := r.(*continuationJump); ok && jump.contExpr == e {
					// Escape: this continuation was invoked within its dynamic extent
					result = jump.val
					resultErr = nil
				} else {
					panic(r) // not our continuation, re-panic
				}
			}
		}()

		// Apply proc to cont
		if proc.typ == valLambda {
			localEnv, bindErr := bindLambdaArgs(proc, []*value{cont}, e.line, e.col)
			if bindErr != nil {
				resultErr = bindErr
				return
			}
			for _, bodyExpr := range proc.body[:len(proc.body)-1] {
				_, err := ip.eval(bodyExpr, localEnv)
				if err != nil {
					resultErr = err
					return
				}
			}
			result, resultErr = ip.eval(proc.body[len(proc.body)-1], localEnv)
		} else if proc.typ == valBuiltin {
			result, resultErr = proc.builtin([]*value{cont}, e.line, e.col)
		} else {
			resultErr = &EvalError{Message: fmt.Sprintf("%d:%d: call/cc: not a procedure", e.line, e.col)}
		}
	}()

	return result, resultErr
}

// callThunk calls a zero-argument procedure.
func (ip *interp) callThunk(thunk *value, line, col int) (*value, error) {
	if thunk.typ == valLambda {
		localEnv := newEnv(thunk.closure)
		var result *value
		var err error
		for _, bodyExpr := range thunk.body {
			result, err = ip.eval(bodyExpr, localEnv)
			if err != nil {
				return nil, err
			}
		}
		return result, nil
	} else if thunk.typ == valBuiltin {
		return thunk.builtin(nil, line, col)
	}
	return nil, &EvalError{Message: fmt.Sprintf("%d:%d: dynamic-wind: not a procedure", line, col)}
}

// applyProc calls a procedure (lambda or builtin) with the given arguments.
func (ip *interp) applyProc(proc *value, args []*value, line, col int) (*value, error) {
	if proc.typ == valLambda {
		localEnv, err := bindLambdaArgs(proc, args, line, col)
		if err != nil {
			return nil, err
		}
		var result *value
		for _, bodyExpr := range proc.body {
			result, err = ip.eval(bodyExpr, localEnv)
			if err != nil {
				return nil, err
			}
		}
		return result, nil
	} else if proc.typ == valBuiltin {
		return proc.builtin(args, line, col)
	}
	return nil, &EvalError{Message: fmt.Sprintf("%d:%d: call-with-values: not a procedure", line, col)}
}

// evalDynamicWind implements (dynamic-wind in-thunk body-thunk out-thunk).
func (ip *interp) evalDynamicWind(e *expr, envir *env) (*value, error) {
	if len(e.items) != 4 {
		return nil, &EvalError{Message: fmt.Sprintf("%d:%d: dynamic-wind: expected 3 arguments", e.line, e.col)}
	}

	inThunk, err := ip.eval(e.items[1], envir)
	if err != nil {
		return nil, err
	}
	bodyThunk, err := ip.eval(e.items[2], envir)
	if err != nil {
		return nil, err
	}
	outThunk, err := ip.eval(e.items[3], envir)
	if err != nil {
		return nil, err
	}

	// Run in-thunk
	_, err = ip.callThunk(inThunk, e.line, e.col)
	if err != nil {
		return nil, err
	}

	// Push wind entry
	ip.windStack = append(ip.windStack, windEntry{inThunk: inThunk, outThunk: outThunk})

	// Run body-thunk, catching continuation jumps and raises to run out-thunk
	var result *value
	var bodyErr error
	var jump *continuationJump
	var raised *schemeRaise

	func() {
		defer func() {
			if r := recover(); r != nil {
				switch j := r.(type) {
				case *continuationJump:
					jump = j
				case *schemeRaise:
					raised = j
				default:
					panic(r)
				}
			}
		}()
		result, bodyErr = ip.callThunk(bodyThunk, e.line, e.col)
	}()

	// Pop wind entry
	ip.windStack = ip.windStack[:len(ip.windStack)-1]

	// Run out-thunk
	_, err = ip.callThunk(outThunk, e.line, e.col)
	if err != nil {
		return nil, err
	}

	if jump != nil {
		panic(jump)
	}
	if raised != nil {
		panic(raised)
	}

	return result, bodyErr
}

// evalGuard implements (guard (var clause ...) body ...).
func (ip *interp) evalGuard(e *expr, envir *env) (*value, error) {
	if len(e.items) < 3 {
		return nil, &EvalError{Message: fmt.Sprintf("%d:%d: guard: bad syntax", e.line, e.col)}
	}
	clauseList := e.items[1]
	if clauseList.kind != "list" || len(clauseList.items) < 1 {
		return nil, &EvalError{Message: fmt.Sprintf("%d:%d: guard: bad syntax", e.line, e.col)}
	}
	varName := clauseList.items[0].sval
	clauses := clauseList.items[1:]
	body := e.items[2:]

	// Evaluate body, catching schemeRaise
	var result *value
	var bodyErr error
	var raised *schemeRaise

	func() {
		defer func() {
			if r := recover(); r != nil {
				if sr, ok := r.(*schemeRaise); ok {
					raised = sr
				} else {
					panic(r)
				}
			}
		}()
		for _, be := range body[:len(body)-1] {
			_, bodyErr = ip.eval(be, envir)
			if bodyErr != nil {
				return
			}
		}
		result, bodyErr = ip.eval(body[len(body)-1], envir)
	}()

	if bodyErr != nil {
		return nil, bodyErr
	}

	if raised == nil {
		return result, nil
	}

	// Exception was raised — bind var and test clauses
	guardEnv := newEnv(envir)
	guardEnv.set(varName, raised.val)

	for _, clause := range clauses {
		if clause.kind != "list" || len(clause.items) < 2 {
			return nil, &EvalError{Message: fmt.Sprintf("%d:%d: guard: bad clause", e.line, e.col)}
		}
		if clause.items[0].kind == "symbol" && clause.items[0].sval == "else" {
			clauseBody := clause.items[1:]
			for _, ce := range clauseBody[:len(clauseBody)-1] {
				_, err := ip.eval(ce, guardEnv)
				if err != nil {
					return nil, err
				}
			}
			return ip.eval(clauseBody[len(clauseBody)-1], guardEnv)
		}
		test, err := ip.eval(clause.items[0], guardEnv)
		if err != nil {
			return nil, err
		}
		if test.isTruthy() {
			clauseBody := clause.items[1:]
			for _, ce := range clauseBody[:len(clauseBody)-1] {
				_, err := ip.eval(ce, guardEnv)
				if err != nil {
					return nil, err
				}
			}
			return ip.eval(clauseBody[len(clauseBody)-1], guardEnv)
		}
	}

	// No clause matched and no else — re-raise
	panic(raised)
}

// evalWithExceptionHandler implements (with-exception-handler handler thunk).
func (ip *interp) evalWithExceptionHandler(e *expr, envir *env) (*value, error) {
	if len(e.items) != 3 {
		return nil, &EvalError{Message: fmt.Sprintf("%d:%d: with-exception-handler: expected 2 arguments", e.line, e.col)}
	}

	handler, err := ip.eval(e.items[1], envir)
	if err != nil {
		return nil, err
	}
	thunk, err := ip.eval(e.items[2], envir)
	if err != nil {
		return nil, err
	}

	// Run thunk, catching schemeRaise
	var result *value
	var thunkErr error
	var raised *schemeRaise

	func() {
		defer func() {
			if r := recover(); r != nil {
				if sr, ok := r.(*schemeRaise); ok {
					raised = sr
				} else {
					panic(r)
				}
			}
		}()
		result, thunkErr = ip.callThunk(thunk, e.line, e.col)
	}()

	if thunkErr != nil {
		return nil, thunkErr
	}

	if raised == nil {
		return result, nil
	}

	// Call handler with the raised value
	if handler.typ == valLambda {
		localEnv, bindErr := bindLambdaArgs(handler, []*value{raised.val}, e.line, e.col)
		if bindErr != nil {
			return nil, bindErr
		}
		var hResult *value
		for _, bodyExpr := range handler.body {
			var hErr error
			hResult, hErr = ip.eval(bodyExpr, localEnv)
			if hErr != nil {
				return nil, hErr
			}
		}
		return hResult, nil
	} else if handler.typ == valBuiltin {
		return handler.builtin([]*value{raised.val}, e.line, e.col)
	}
	return nil, &EvalError{Message: fmt.Sprintf("%d:%d: with-exception-handler: handler is not a procedure", e.line, e.col)}
}

func bindLambdaArgs(fn *value, args []*value, line, col int) (*env, error) {
	// case-lambda: find matching clause
	if fn.caseClauses != nil {
		for _, cl := range fn.caseClauses {
			if cl.restParam == "" {
				if len(args) != len(cl.params) {
					continue
				}
			} else {
				if len(args) < len(cl.params) {
					continue
				}
			}
			// Match found — bind and set body on fn for caller to use
			localEnv := newEnv(fn.closure)
			for i, p := range cl.params {
				localEnv.set(p, args[i])
			}
			if cl.restParam != "" {
				rest := nilVal
				for i := len(args) - 1; i >= len(cl.params); i-- {
					rest = &value{typ: valPair, car: args[i], cdr: rest}
				}
				localEnv.set(cl.restParam, rest)
			}
			// Set the body on fn so callers can access it for TCO
			fn.body = cl.body
			return localEnv, nil
		}
		return nil, &EvalError{Message: fmt.Sprintf("%d:%d: wrong number of arguments: no matching clause for %d args", line, col, len(args))}
	}

	if fn.restParam == "" {
		if len(args) != len(fn.params) {
			return nil, &EvalError{Message: fmt.Sprintf("%d:%d: wrong number of arguments: expected %d, got %d", line, col, len(fn.params), len(args))}
		}
	} else {
		if len(args) < len(fn.params) {
			return nil, &EvalError{Message: fmt.Sprintf("%d:%d: wrong number of arguments: expected at least %d, got %d", line, col, len(fn.params), len(args))}
		}
	}
	localEnv := newEnv(fn.closure)
	for i, p := range fn.params {
		localEnv.set(p, args[i])
	}
	if fn.restParam != "" {
		rest := nilVal
		for i := len(args) - 1; i >= len(fn.params); i-- {
			rest = &value{typ: valPair, car: args[i], cdr: rest}
		}
		localEnv.set(fn.restParam, rest)
	}
	return localEnv, nil
}

func applyFunc(fn *value, args []*value, callExpr *expr) (*value, error) {
	line, col := callExpr.line, callExpr.col

	switch fn.typ {
	case valBuiltin:
		return fn.builtin(args, line, col)
	case valContinuation:
		var jumpVal *value
		if len(args) == 1 {
			jumpVal = args[0]
		} else {
			jumpVal = &value{typ: valMultipleValues, multiVals: args}
		}
		panic(&continuationJump{
			contExpr: fn.contExpr,
			contIdx:  fn.contIdx,
			val:      jumpVal,
			letStack: fn.contLetStack,
		})
	default:
		return nil, &EvalError{Message: fmt.Sprintf("%d:%d: not a procedure", line, col)}
	}
}

func (ip *interp) evalDefine(e *expr, envir *env) (*value, error) {
	if len(e.items) < 3 {
		return nil, &EvalError{Message: fmt.Sprintf("%d:%d: define: bad syntax", e.line, e.col)}
	}
	target := e.items[1]

	// (define (f params...) body...) or (define (f x . rest) body...)
	if target.kind == "list" {
		if len(target.items) == 0 {
			return nil, &EvalError{Message: fmt.Sprintf("%d:%d: define: bad syntax", e.line, e.col)}
		}
		name := target.items[0].sval
		// Build a fake param list expr from target.items[1:]
		paramListExpr := &expr{kind: "list", items: target.items[1:], dotted: target.dotted, line: target.line, col: target.col}
		params, restParam, perr := parseLambdaParams(paramListExpr)
		if perr != nil {
			return nil, &EvalError{Message: fmt.Sprintf("%d:%d: define: %s", e.line, e.col, perr)}
		}
		fn := &value{
			typ:       valLambda,
			params:    params,
			restParam: restParam,
			body:      e.items[2:],
			closure:   envir,
		}
		envir.set(name, fn)
		return voidVal, nil
	}

	// (define x expr)
	if target.kind != "symbol" {
		return nil, &EvalError{Message: fmt.Sprintf("%d:%d: define: bad syntax", e.line, e.col)}
	}
	val, err := ip.eval(e.items[2], envir)
	if err != nil {
		return nil, err
	}
	envir.set(target.sval, val)
	return voidVal, nil
}

// evalDefineRecordType implements R7RS define-record-type.
// (define-record-type <name> (constructor field ...) predicate (field accessor) ...)
func (ip *interp) evalDefineRecordType(e *expr, envir *env) (*value, error) {
	if len(e.items) < 5 {
		return nil, &EvalError{Message: fmt.Sprintf("%d:%d: define-record-type: bad syntax", e.line, e.col)}
	}

	// 1. Type name (e.g., <point>)
	typeName := e.items[1].sval

	// 2. Constructor clause: (make-point x y)
	ctorExpr := e.items[2]
	if ctorExpr.kind != "list" || len(ctorExpr.items) < 1 {
		return nil, &EvalError{Message: fmt.Sprintf("%d:%d: define-record-type: bad constructor", e.line, e.col)}
	}
	ctorName := ctorExpr.items[0].sval
	ctorFields := make([]string, len(ctorExpr.items)-1)
	for i, f := range ctorExpr.items[1:] {
		ctorFields[i] = f.sval
	}

	// 3. Predicate name
	predName := e.items[3].sval

	// 4. Field clauses: (field accessor) ...
	// Build a map from field name -> accessor name
	type fieldDef struct {
		name     string
		accessor string
	}
	var fields []fieldDef
	for _, clause := range e.items[4:] {
		if clause.kind != "list" || len(clause.items) < 2 {
			return nil, &EvalError{Message: fmt.Sprintf("%d:%d: define-record-type: bad field spec", e.line, e.col)}
		}
		fields = append(fields, fieldDef{
			name:     clause.items[0].sval,
			accessor: clause.items[1].sval,
		})
	}

	// Create record type metadata
	rt := &recordType{
		name:       typeName,
		fieldNames: ctorFields,
	}

	// Build field-name -> index map
	fieldIndex := make(map[string]int)
	for i, fn := range ctorFields {
		fieldIndex[fn] = i
	}

	// Define constructor
	nFields := len(ctorFields)
	envir.set(ctorName, &value{
		typ: valBuiltin,
		builtin: func(args []*value, line, col int) (*value, error) {
			if len(args) != nFields {
				return nil, &EvalError{Message: fmt.Sprintf("%d:%d: %s: expected %d arguments, got %d", line, col, ctorName, nFields, len(args))}
			}
			fv := make([]*value, nFields)
			copy(fv, args)
			return &value{typ: valRecord, recType: rt, recFields: fv}, nil
		},
	})

	// Define predicate
	envir.set(predName, &value{
		typ: valBuiltin,
		builtin: func(args []*value, line, col int) (*value, error) {
			if len(args) != 1 {
				return nil, &EvalError{Message: fmt.Sprintf("%d:%d: %s: expected 1 argument", line, col, predName)}
			}
			return boolVal(args[0].typ == valRecord && args[0].recType == rt), nil
		},
	})

	// Define accessors
	for _, fd := range fields {
		idx, ok := fieldIndex[fd.name]
		if !ok {
			return nil, &EvalError{Message: fmt.Sprintf("%d:%d: define-record-type: field %s not in constructor", e.line, e.col, fd.name)}
		}
		accName := fd.accessor
		fieldIdx := idx
		envir.set(accName, &value{
			typ: valBuiltin,
			builtin: func(args []*value, line, col int) (*value, error) {
				if len(args) != 1 {
					return nil, &EvalError{Message: fmt.Sprintf("%d:%d: %s: expected 1 argument", line, col, accName)}
				}
				if args[0].typ != valRecord || args[0].recType != rt {
					return nil, &EvalError{Message: fmt.Sprintf("%d:%d: %s: not a %s", line, col, accName, typeName)}
				}
				return args[0].recFields[fieldIdx], nil
			},
		})
	}

	return voidVal, nil
}

func parseLambdaParams(paramExpr *expr) (params []string, restParam string, err error) {
	if paramExpr.kind == "symbol" {
		// (lambda args body) — single rest param capturing all args
		return nil, paramExpr.sval, nil
	}
	if paramExpr.kind != "list" {
		return nil, "", fmt.Errorf("bad parameter list")
	}
	// Check for dot notation: (a b . rest)
	if paramExpr.dotted && len(paramExpr.items) >= 1 {
		last := paramExpr.items[len(paramExpr.items)-1]
		for _, pp := range paramExpr.items[:len(paramExpr.items)-1] {
			params = append(params, pp.sval)
		}
		restParam = last.sval
		return params, restParam, nil
	}
	for i, p := range paramExpr.items {
		if p.kind == "symbol" && p.sval == "." {
			if i+1 != len(paramExpr.items)-1 {
				return nil, "", fmt.Errorf("bad dotted parameter list")
			}
			for _, pp := range paramExpr.items[:i] {
				params = append(params, pp.sval)
			}
			restParam = paramExpr.items[i+1].sval
			return params, restParam, nil
		}
	}
	params = make([]string, len(paramExpr.items))
	for i, p := range paramExpr.items {
		params[i] = p.sval
	}
	return params, "", nil
}

func evalLambda(e *expr, envir *env) (*value, error) {
	if len(e.items) < 3 {
		return nil, &EvalError{Message: fmt.Sprintf("%d:%d: lambda: bad syntax", e.line, e.col)}
	}
	params, restParam, perr := parseLambdaParams(e.items[1])
	if perr != nil {
		return nil, &EvalError{Message: fmt.Sprintf("%d:%d: lambda: %s", e.line, e.col, perr)}
	}
	return &value{
		typ:       valLambda,
		params:    params,
		restParam: restParam,
		body:      e.items[2:],
		closure:   envir,
	}, nil
}

func evalCaseLambda(e *expr, envir *env) (*value, error) {
	if len(e.items) < 2 {
		return nil, &EvalError{Message: fmt.Sprintf("%d:%d: case-lambda: bad syntax", e.line, e.col)}
	}
	var clauses []caseClause
	for _, clause := range e.items[1:] {
		if clause.kind != "list" || len(clause.items) < 2 {
			return nil, &EvalError{Message: fmt.Sprintf("%d:%d: case-lambda: bad clause", e.line, e.col)}
		}
		params, restParam, perr := parseLambdaParams(clause.items[0])
		if perr != nil {
			return nil, &EvalError{Message: fmt.Sprintf("%d:%d: case-lambda: %s", e.line, e.col, perr)}
		}
		clauses = append(clauses, caseClause{
			params:    params,
			restParam: restParam,
			body:      clause.items[1:],
		})
	}
	return &value{
		typ:         valLambda,
		caseClauses: clauses,
		closure:     envir,
	}, nil
}

// setupLet prepares the let environment and returns the tail expression to evaluate.
func (ip *interp) setupLet(e *expr, envir *env) (tailExpr *expr, tailEnv *env, err error) {
	if len(e.items) < 3 {
		return nil, nil, &EvalError{Message: fmt.Sprintf("%d:%d: let: bad syntax", e.line, e.col)}
	}

	idx := 1
	var name string

	if e.items[1].kind == "symbol" {
		if len(e.items) < 4 {
			return nil, nil, &EvalError{Message: fmt.Sprintf("%d:%d: let: bad syntax", e.line, e.col)}
		}
		name = e.items[1].sval
		idx = 2
	}

	bindingsExpr := e.items[idx]
	body := e.items[idx+1:]

	if bindingsExpr.kind != "list" {
		return nil, nil, &EvalError{Message: fmt.Sprintf("%d:%d: let: bad syntax", e.line, e.col)}
	}

	// Check if we should reuse a saved environment (continuation replay)
	var localEnv *env
	skipToIdx := -1 // body index to skip to during replay (-1 = no skip)
	replayMatch := false
	if len(ip.replayLetStack) > 0 && ip.replayLetStack[0].expr == e {
		// Match only the outermost let on the replay stack.
		// Inner lets (e.g., inside function bodies) should re-initialize.
		localEnv = ip.replayLetStack[0].env
		skipToIdx = ip.replayLetStack[0].bodyIdx
		ip.replayLetStack = nil // consumed — inner lets evaluate normally
		replayMatch = true
	}
	if !replayMatch {
		params := make([]string, len(bindingsExpr.items))
		vals := make([]*value, len(bindingsExpr.items))
		for i, b := range bindingsExpr.items {
			if b.kind != "list" || len(b.items) != 2 {
				return nil, nil, &EvalError{Message: fmt.Sprintf("%d:%d: let: bad binding", e.line, e.col)}
			}
			params[i] = b.items[0].sval
			v, err := ip.eval(b.items[1], envir)
			if err != nil {
				return nil, nil, err
			}
			vals[i] = v
		}

		localEnv = newEnv(envir)
		for i, p := range params {
			localEnv.set(p, vals[i])
		}
	}

	if name != "" {
		// For named let during replay, we need to get params from bindings
		var namedParams []string
		for _, b := range bindingsExpr.items {
			namedParams = append(namedParams, b.items[0].sval)
		}
		fn := &value{
			typ:     valLambda,
			params:  namedParams,
			body:    body,
			closure: localEnv,
		}
		localEnv.set(name, fn)
	}

	// Push let context for continuation capture
	ip.letStack = append(ip.letStack, letCtx{expr: e, env: localEnv, bodyIdx: 0})
	stackIdx := len(ip.letStack) - 1

	// Eval all body exprs except last
	for i, b := range body[:len(body)-1] {
		ip.letStack[stackIdx].bodyIdx = i
		if skipToIdx > 0 && i < skipToIdx {
			continue // skip body expressions before the replay target
		}
		_, err := ip.eval(b, localEnv)
		if err != nil {
			ip.letStack = ip.letStack[:stackIdx]
			return nil, nil, err
		}
	}

	// Set body index for tail expression
	ip.letStack[stackIdx].bodyIdx = len(body) - 1

	// Pop let context (will be re-pushed if needed by tail eval)
	ip.letStack = ip.letStack[:stackIdx]

	return body[len(body)-1], localEnv, nil
}

// evalLetrec implements letrec (star=false) and letrec* (star=true).
func (ip *interp) evalLetrec(e *expr, envir *env, star bool) (tailExpr *expr, tailEnv *env, err error) {
	if len(e.items) < 3 {
		return nil, nil, &EvalError{Message: fmt.Sprintf("%d:%d: letrec: bad syntax", e.line, e.col)}
	}
	bindingsExpr := e.items[1]
	body := e.items[2:]
	if bindingsExpr.kind != "list" {
		return nil, nil, &EvalError{Message: fmt.Sprintf("%d:%d: letrec: bad syntax", e.line, e.col)}
	}
	localEnv := newEnv(envir)
	// Initialize all bindings to void first (for mutual recursion)
	names := make([]string, len(bindingsExpr.items))
	for i, b := range bindingsExpr.items {
		if b.kind != "list" || len(b.items) != 2 {
			return nil, nil, &EvalError{Message: fmt.Sprintf("%d:%d: letrec: bad binding", e.line, e.col)}
		}
		names[i] = b.items[0].sval
		localEnv.set(names[i], voidVal)
	}
	if star {
		// letrec*: evaluate sequentially, each init can see previous bindings
		for i, b := range bindingsExpr.items {
			v, err := ip.eval(b.items[1], localEnv)
			if err != nil {
				return nil, nil, err
			}
			localEnv.set(names[i], v)
		}
	} else {
		// letrec: evaluate all inits, then assign
		vals := make([]*value, len(bindingsExpr.items))
		for i, b := range bindingsExpr.items {
			v, err := ip.eval(b.items[1], localEnv)
			if err != nil {
				return nil, nil, err
			}
			vals[i] = v
		}
		for i, name := range names {
			localEnv.set(name, vals[i])
		}
	}
	// Eval body
	for _, b := range body[:len(body)-1] {
		_, err := ip.eval(b, localEnv)
		if err != nil {
			return nil, nil, err
		}
	}
	return body[len(body)-1], localEnv, nil
}

// evalCase implements (case expr ((datum ...) body ...) ... (else body ...))
func (ip *interp) evalCase(e *expr, envir *env) (*expr, error) {
	if len(e.items) < 2 {
		return nil, &EvalError{Message: fmt.Sprintf("%d:%d: case: bad syntax", e.line, e.col)}
	}
	key, err := ip.eval(e.items[1], envir)
	if err != nil {
		return nil, err
	}
	for _, clause := range e.items[2:] {
		if clause.kind != "list" || len(clause.items) < 2 {
			return nil, &EvalError{Message: fmt.Sprintf("%d:%d: case: bad clause", e.line, e.col)}
		}
		datums := clause.items[0]
		body := clause.items[1:]
		// else clause
		if datums.kind == "symbol" && datums.sval == "else" {
			for _, b := range body[:len(body)-1] {
				_, err := ip.eval(b, envir)
				if err != nil {
					return nil, err
				}
			}
			return body[len(body)-1], nil
		}
		// datum list
		if datums.kind != "list" {
			return nil, &EvalError{Message: fmt.Sprintf("%d:%d: case: bad clause", e.line, e.col)}
		}
		for _, d := range datums.items {
			dv := quoteExpr(d)
			if schemeEqv(key, dv) {
				for _, b := range body[:len(body)-1] {
					_, err := ip.eval(b, envir)
					if err != nil {
						return nil, err
					}
				}
				return body[len(body)-1], nil
			}
		}
	}
	return nil, nil // no match, no else → void
}

// evalDo implements (do ((var init step) ...) (test expr ...) body ...)
func (ip *interp) evalDo(e *expr, envir *env) (result *value, tailE *expr, tailEnv *env, err error) {
	if len(e.items) < 3 {
		return nil, nil, nil, &EvalError{Message: fmt.Sprintf("%d:%d: do: bad syntax", e.line, e.col)}
	}
	varSpecs := e.items[1]
	testClause := e.items[2]
	body := e.items[3:]
	if varSpecs.kind != "list" || testClause.kind != "list" || len(testClause.items) < 1 {
		return nil, nil, nil, &EvalError{Message: fmt.Sprintf("%d:%d: do: bad syntax", e.line, e.col)}
	}

	// Parse variable specs
	type doVar struct {
		name string
		step *expr // nil if no step
	}
	vars := make([]doVar, len(varSpecs.items))
	doEnv := newEnv(envir)

	for i, spec := range varSpecs.items {
		if spec.kind != "list" || len(spec.items) < 2 || len(spec.items) > 3 {
			return nil, nil, nil, &EvalError{Message: fmt.Sprintf("%d:%d: do: bad variable spec", e.line, e.col)}
		}
		vars[i].name = spec.items[0].sval
		initVal, err := ip.eval(spec.items[1], envir)
		if err != nil {
			return nil, nil, nil, err
		}
		doEnv.set(vars[i].name, initVal)
		if len(spec.items) == 3 {
			vars[i].step = spec.items[2]
		}
	}

	testExpr := testClause.items[0]
	resultExprs := testClause.items[1:]

	for {
		// Test
		testVal, err := ip.eval(testExpr, doEnv)
		if err != nil {
			return nil, nil, nil, err
		}
		if testVal.isTruthy() {
			// Evaluate result expressions
			if len(resultExprs) == 0 {
				return voidVal, nil, nil, nil
			}
			for _, re := range resultExprs[:len(resultExprs)-1] {
				_, err := ip.eval(re, doEnv)
				if err != nil {
					return nil, nil, nil, err
				}
			}
			return nil, resultExprs[len(resultExprs)-1], doEnv, nil
		}
		// Evaluate body (for side effects)
		for _, b := range body {
			_, err := ip.eval(b, doEnv)
			if err != nil {
				return nil, nil, nil, err
			}
		}
		// Parallel step: evaluate all step expressions with current values
		newVals := make([]*value, len(vars))
		for i, v := range vars {
			if v.step != nil {
				sv, err := ip.eval(v.step, doEnv)
				if err != nil {
					return nil, nil, nil, err
				}
				newVals[i] = sv
			}
		}
		// Update all at once
		for i, v := range vars {
			if v.step != nil {
				doEnv.set(v.name, newVals[i])
			}
		}
	}
}

// schemeEqv implements eqv? semantics (value equality for numbers, bools, chars, symbols; identity for others)
func schemeEqv(a, b *value) bool {
	if a.typ != b.typ {
		return false
	}
	switch a.typ {
	case valInt:
		return a.ival == b.ival
	case valBool:
		return a.bval == b.bval
	case valSymbol:
		return a.sval == b.sval
	case valChar:
		return a.cval == b.cval
	case valNil:
		return true
	case valFloat:
		return a.fval == b.fval
	case valRational:
		return a.num == b.num && a.den == b.den
	default:
		return a == b
	}
}

func quoteExpr(e *expr) *value {
	switch e.kind {
	case "int":
		return intVal(e.ival)
	case "float":
		return floatVal(e.fval)
	case "rational":
		return ratVal(e.ival, e.ival2)
	case "bool":
		return boolVal(e.bval)
	case "string":
		return strVal(e.sval)
	case "char":
		runes := []rune(e.sval)
		return charVal(runes[0])
	case "symbol":
		return symVal(e.sval)
	case "list":
		if len(e.items) == 0 {
			return nilVal
		}
		if e.dotted {
			// Improper list: last item is the cdr
			result := quoteExpr(e.items[len(e.items)-1])
			for i := len(e.items) - 2; i >= 0; i-- {
				result = &value{typ: valPair, car: quoteExpr(e.items[i]), cdr: result}
			}
			return result
		}
		// Build proper list from items
		result := nilVal
		for i := len(e.items) - 1; i >= 0; i-- {
			result = &value{typ: valPair, car: quoteExpr(e.items[i]), cdr: result}
		}
		return result
	}
	return nilVal
}

func (ip *interp) evalQuasiquote(e *expr, envir *env) (*value, error) {
	return ip.qqExpand(e, envir, 0)
}

func (ip *interp) qqExpand(e *expr, envir *env, depth int) (*value, error) {
	// Check for (unquote x)
	if e.kind == "list" && len(e.items) == 2 && e.items[0].kind == "symbol" && e.items[0].sval == "unquote" {
		if depth == 0 {
			return ip.eval(e.items[1], envir)
		}
		// Nested quasiquote: decrement depth
		inner, err := ip.qqExpand(e.items[1], envir, depth-1)
		if err != nil {
			return nil, err
		}
		return &value{typ: valPair, car: symVal("unquote"), cdr: &value{typ: valPair, car: inner, cdr: nilVal}}, nil
	}

	// Check for (quasiquote x) - nested
	if e.kind == "list" && len(e.items) == 2 && e.items[0].kind == "symbol" && e.items[0].sval == "quasiquote" {
		inner, err := ip.qqExpand(e.items[1], envir, depth+1)
		if err != nil {
			return nil, err
		}
		return &value{typ: valPair, car: symVal("quasiquote"), cdr: &value{typ: valPair, car: inner, cdr: nilVal}}, nil
	}

	if e.kind == "list" {
		return ip.qqExpandList(e, envir, depth)
	}

	// Atoms: just quote them
	return quoteExpr(e), nil
}

func (ip *interp) qqExpandList(e *expr, envir *env, depth int) (*value, error) {
	if len(e.items) == 0 {
		return nilVal, nil
	}

	// Collect expanded items, handling splicing
	var parts []*value
	for i, item := range e.items {
		// Check for unquote-splicing
		if item.kind == "list" && len(item.items) == 2 && item.items[0].kind == "symbol" && item.items[0].sval == "unquote-splicing" {
			if depth == 0 {
				val, err := ip.eval(item.items[1], envir)
				if err != nil {
					return nil, err
				}
				// Splice in the list
				cur := val
				for cur.typ == valPair {
					parts = append(parts, cur.car)
					cur = cur.cdr
				}
				continue
			}
		}

		// For dotted lists, the last item is the cdr
		if e.dotted && i == len(e.items)-1 {
			tail, err := ip.qqExpand(item, envir, depth)
			if err != nil {
				return nil, err
			}
			// Build result with this tail
			result := tail
			for j := len(parts) - 1; j >= 0; j-- {
				result = &value{typ: valPair, car: parts[j], cdr: result}
			}
			return result, nil
		}

		expanded, err := ip.qqExpand(item, envir, depth)
		if err != nil {
			return nil, err
		}
		parts = append(parts, expanded)
	}

	// Build proper list from parts
	result := nilVal
	for i := len(parts) - 1; i >= 0; i-- {
		result = &value{typ: valPair, car: parts[i], cdr: result}
	}
	return result, nil
}

func compareInts(args []*value, op func(int64, int64) bool, name string, line, col int) (*value, error) {
	if len(args) < 2 {
		return nil, &EvalError{Message: fmt.Sprintf("%d:%d: %s: expected at least 2 arguments", line, col, name)}
	}
	for i := 0; i < len(args)-1; i++ {
		if !isNumeric(args[i]) || !isNumeric(args[i+1]) {
			return nil, &EvalError{Message: fmt.Sprintf("%d:%d: %s: expected number", line, col, name)}
		}
		fa := toFloat(args[i])
		fb := toFloat(args[i+1])
		// Map float comparison to the int64 op via -1/0/1
		var cmp int64
		if fa < fb {
			cmp = -1
		} else if fa > fb {
			cmp = 1
		} else {
			cmp = 0
		}
		if !op(cmp, 0) {
			return boolVal(false), nil
		}
	}
	return boolVal(true), nil
}

func schemeEq(a, b *value) bool {
	if a.typ != b.typ {
		return false
	}
	switch a.typ {
	case valInt:
		return a.ival == b.ival
	case valBool:
		return a.bval == b.bval
	case valSymbol:
		return a.sval == b.sval
	case valChar:
		return a.cval == b.cval
	case valNil:
		return true
	default:
		return a == b // pointer identity
	}
}

type equalPair struct{ a, b *value }

func schemeEqual(a, b *value) bool {
	seen := make(map[equalPair]bool)
	return schemeEqualRec(a, b, seen)
}

func schemeEqualRec(a, b *value, seen map[equalPair]bool) bool {
	if a == b {
		return true
	}
	if a.typ != b.typ {
		return false
	}
	switch a.typ {
	case valInt:
		return a.ival == b.ival
	case valBool:
		return a.bval == b.bval
	case valString:
		return a.sval == b.sval
	case valSymbol:
		return a.sval == b.sval
	case valChar:
		return a.cval == b.cval
	case valNil:
		return true
	case valPair:
		key := equalPair{a, b}
		if seen[key] {
			return true // assume equal for cycles
		}
		seen[key] = true
		return schemeEqualRec(a.car, b.car, seen) && schemeEqualRec(a.cdr, b.cdr, seen)
	case valVector:
		if len(a.vecval) != len(b.vecval) {
			return false
		}
		for i := range a.vecval {
			if !schemeEqualRec(a.vecval[i], b.vecval[i], seen) {
				return false
			}
		}
		return true
	default:
		return a == b
	}
}

// ---------- Top-level ----------

func makeBuiltin(name string, fn func(args []*value, line, col int) (*value, error)) *value {
	return &value{typ: valBuiltin, sval: name, builtin: fn}
}

// hasInexact checks if any arg is inexact (float)
func hasInexact(args []*value) bool {
	for _, a := range args {
		if a.typ == valFloat {
			return true
		}
	}
	return false
}

func numAdd(args []*value) *value {
	if len(args) == 0 {
		return intVal(0)
	}
	if hasInexact(args) {
		sum := 0.0
		for _, a := range args {
			sum += toFloat(a)
		}
		return floatVal(sum)
	}
	// All exact
	n, d := int64(0), int64(1)
	for _, a := range args {
		an, ad, _ := toRational(a)
		n = n*ad + an*d
		d = d * ad
		g := gcd(n, d)
		n, d = n/g, d/g
	}
	if d < 0 {
		n, d = -n, -d
	}
	if d == 1 {
		return intVal(n)
	}
	return &value{typ: valRational, num: n, den: d}
}

func numSub(args []*value) *value {
	if len(args) == 1 {
		a := args[0]
		switch a.typ {
		case valInt:
			return intVal(-a.ival)
		case valFloat:
			return floatVal(-a.fval)
		case valRational:
			return &value{typ: valRational, num: -a.num, den: a.den}
		}
	}
	if hasInexact(args) {
		result := toFloat(args[0])
		for _, a := range args[1:] {
			result -= toFloat(a)
		}
		return floatVal(result)
	}
	n, d, _ := toRational(args[0])
	for _, a := range args[1:] {
		an, ad, _ := toRational(a)
		n = n*ad - an*d
		d = d * ad
		g := gcd(n, d)
		n, d = n/g, d/g
	}
	if d < 0 {
		n, d = -n, -d
	}
	if d == 1 {
		return intVal(n)
	}
	return &value{typ: valRational, num: n, den: d}
}

func numMul(args []*value) *value {
	if len(args) == 0 {
		return intVal(1)
	}
	if hasInexact(args) {
		result := 1.0
		for _, a := range args {
			result *= toFloat(a)
		}
		return floatVal(result)
	}
	n, d := int64(1), int64(1)
	for _, a := range args {
		an, ad, _ := toRational(a)
		n *= an
		d *= ad
		g := gcd(n, d)
		n, d = n/g, d/g
	}
	if d < 0 {
		n, d = -n, -d
	}
	if d == 1 {
		return intVal(n)
	}
	return &value{typ: valRational, num: n, den: d}
}

func numDiv(args []*value, line, col int) (*value, error) {
	if hasInexact(args) {
		result := toFloat(args[0])
		for _, a := range args[1:] {
			f := toFloat(a)
			if f == 0 {
				return nil, &EvalError{Message: fmt.Sprintf("%d:%d: /: division by zero", line, col)}
			}
			result /= f
		}
		return floatVal(result), nil
	}
	n, d, _ := toRational(args[0])
	for _, a := range args[1:] {
		an, ad, _ := toRational(a)
		if an == 0 {
			return nil, &EvalError{Message: fmt.Sprintf("%d:%d: /: division by zero", line, col)}
		}
		n = n * ad
		d = d * an
		g := gcd(n, d)
		n, d = n/g, d/g
	}
	if d < 0 {
		n, d = -n, -d
	}
	if d == 1 {
		return intVal(n), nil
	}
	return &value{typ: valRational, num: n, den: d}, nil
}

func makeGlobalEnv(ip *interp) *env {
	e := newEnv(nil)

	e.set("+", makeBuiltin("+", func(args []*value, line, col int) (*value, error) {
		for _, a := range args {
			if !isNumeric(a) {
				return nil, &EvalError{Message: fmt.Sprintf("%d:%d: +: expected number", line, col)}
			}
		}
		return numAdd(args), nil
	}))

	e.set("-", makeBuiltin("-", func(args []*value, line, col int) (*value, error) {
		if len(args) == 0 {
			return nil, &EvalError{Message: fmt.Sprintf("%d:%d: -: expected at least 1 argument", line, col)}
		}
		for _, a := range args {
			if !isNumeric(a) {
				return nil, &EvalError{Message: fmt.Sprintf("%d:%d: -: expected number", line, col)}
			}
		}
		return numSub(args), nil
	}))

	e.set("*", makeBuiltin("*", func(args []*value, line, col int) (*value, error) {
		for _, a := range args {
			if !isNumeric(a) {
				return nil, &EvalError{Message: fmt.Sprintf("%d:%d: *: expected number", line, col)}
			}
		}
		return numMul(args), nil
	}))

	e.set("/", makeBuiltin("/", func(args []*value, line, col int) (*value, error) {
		if len(args) < 2 {
			return nil, &EvalError{Message: fmt.Sprintf("%d:%d: /: expected at least 2 arguments", line, col)}
		}
		for _, a := range args {
			if !isNumeric(a) {
				return nil, &EvalError{Message: fmt.Sprintf("%d:%d: /: expected number", line, col)}
			}
		}
		return numDiv(args, line, col)
	}))

	e.set("<", makeBuiltin("<", func(args []*value, line, col int) (*value, error) {
		return compareInts(args, func(a, b int64) bool { return a < b }, "<", line, col)
	}))
	e.set(">", makeBuiltin(">", func(args []*value, line, col int) (*value, error) {
		return compareInts(args, func(a, b int64) bool { return a > b }, ">", line, col)
	}))
	e.set("=", makeBuiltin("=", func(args []*value, line, col int) (*value, error) {
		return compareInts(args, func(a, b int64) bool { return a == b }, "=", line, col)
	}))
	e.set("<=", makeBuiltin("<=", func(args []*value, line, col int) (*value, error) {
		return compareInts(args, func(a, b int64) bool { return a <= b }, "<=", line, col)
	}))
	e.set(">=", makeBuiltin(">=", func(args []*value, line, col int) (*value, error) {
		return compareInts(args, func(a, b int64) bool { return a >= b }, ">=", line, col)
	}))

	e.set("not", makeBuiltin("not", func(args []*value, line, col int) (*value, error) {
		if len(args) != 1 {
			return nil, &EvalError{Message: fmt.Sprintf("%d:%d: not: expected 1 argument", line, col)}
		}
		return boolVal(!args[0].isTruthy()), nil
	}))

	// List operations
	e.set("cons", makeBuiltin("cons", func(args []*value, line, col int) (*value, error) {
		if len(args) != 2 {
			return nil, &EvalError{Message: fmt.Sprintf("%d:%d: cons: expected 2 arguments", line, col)}
		}
		return &value{typ: valPair, car: args[0], cdr: args[1]}, nil
	}))

	e.set("car", makeBuiltin("car", func(args []*value, line, col int) (*value, error) {
		if len(args) != 1 || args[0].typ != valPair {
			return nil, &EvalError{Message: fmt.Sprintf("%d:%d: car: expected pair", line, col)}
		}
		return args[0].car, nil
	}))

	e.set("cdr", makeBuiltin("cdr", func(args []*value, line, col int) (*value, error) {
		if len(args) != 1 || args[0].typ != valPair {
			return nil, &EvalError{Message: fmt.Sprintf("%d:%d: cdr: expected pair", line, col)}
		}
		return args[0].cdr, nil
	}))

	e.set("null?", makeBuiltin("null?", func(args []*value, line, col int) (*value, error) {
		if len(args) != 1 {
			return nil, &EvalError{Message: fmt.Sprintf("%d:%d: null?: expected 1 argument", line, col)}
		}
		return boolVal(args[0].typ == valNil), nil
	}))

	e.set("list", makeBuiltin("list", func(args []*value, line, col int) (*value, error) {
		result := nilVal
		for i := len(args) - 1; i >= 0; i-- {
			result = &value{typ: valPair, car: args[i], cdr: result}
		}
		return result, nil
	}))

	e.set("length", makeBuiltin("length", func(args []*value, line, col int) (*value, error) {
		if len(args) != 1 {
			return nil, &EvalError{Message: fmt.Sprintf("%d:%d: length: expected 1 argument", line, col)}
		}
		count := int64(0)
		cur := args[0]
		for cur.typ == valPair {
			count++
			cur = cur.cdr
		}
		return intVal(count), nil
	}))

	e.set("append", makeBuiltin("append", func(args []*value, line, col int) (*value, error) {
		if len(args) == 0 {
			return nilVal, nil
		}
		if len(args) == 1 {
			return args[0], nil
		}
		// Append all lists
		result := args[len(args)-1]
		for i := len(args) - 2; i >= 0; i-- {
			lst := args[i]
			// Collect elements of lst
			var elems []*value
			cur := lst
			for cur.typ == valPair {
				elems = append(elems, cur.car)
				cur = cur.cdr
			}
			// Build from right
			for j := len(elems) - 1; j >= 0; j-- {
				result = &value{typ: valPair, car: elems[j], cdr: result}
			}
		}
		return result, nil
	}))

	e.set("reverse", makeBuiltin("reverse", func(args []*value, line, col int) (*value, error) {
		if len(args) != 1 {
			return nil, &EvalError{Message: fmt.Sprintf("%d:%d: reverse: expected 1 argument", line, col)}
		}
		result := nilVal
		cur := args[0]
		for cur.typ == valPair {
			result = &value{typ: valPair, car: cur.car, cdr: result}
			cur = cur.cdr
		}
		return result, nil
	}))

	// apply
	e.set("apply", makeBuiltin("apply", func(args []*value, line, col int) (*value, error) {
		if len(args) < 2 {
			return nil, &EvalError{Message: fmt.Sprintf("%d:%d: apply: expected at least 2 arguments", line, col)}
		}
		fn := args[0]
		// Last arg must be a list; prefix args are prepended
		lastArg := args[len(args)-1]
		var fnArgs []*value
		// Collect prefix args (between fn and the final list)
		for _, a := range args[1 : len(args)-1] {
			fnArgs = append(fnArgs, a)
		}
		// Unpack the final list
		cur := lastArg
		for cur.typ == valPair {
			fnArgs = append(fnArgs, cur.car)
			cur = cur.cdr
		}
		if fn.typ == valContinuation {
			if len(fnArgs) != 1 {
				return nil, &EvalError{Message: fmt.Sprintf("%d:%d: apply: continuation expects 1 argument", line, col)}
			}
			panic(&continuationJump{
				contExpr: fn.contExpr,
				contIdx:  fn.contIdx,
				val:      fnArgs[0],
				letStack:  fn.contLetStack,
			windStack: fn.contWindStack,
			})
		}
		if fn.typ == valLambda {
			localEnv, bindErr := bindLambdaArgs(fn, fnArgs, line, col)
			if bindErr != nil {
				return nil, bindErr
			}
			// Evaluate body
			var result *value
			for _, bodyExpr := range fn.body {
				var err error
				result, err = ip.eval(bodyExpr, localEnv)
				if err != nil {
					return nil, err
				}
			}
			return result, nil
		}
		if fn.typ == valBuiltin {
			return fn.builtin(fnArgs, line, col)
		}
		return nil, &EvalError{Message: fmt.Sprintf("%d:%d: apply: not a procedure", line, col)}
	}))

	// Type predicates
	e.set("string?", makeBuiltin("string?", func(args []*value, line, col int) (*value, error) {
		if len(args) != 1 {
			return nil, &EvalError{Message: fmt.Sprintf("%d:%d: string?: expected 1 argument", line, col)}
		}
		return boolVal(args[0].typ == valString), nil
	}))

	e.set("number?", makeBuiltin("number?", func(args []*value, line, col int) (*value, error) {
		if len(args) != 1 {
			return nil, &EvalError{Message: fmt.Sprintf("%d:%d: number?: expected 1 argument", line, col)}
		}
		return boolVal(isNumeric(args[0])), nil
	}))

	e.set("integer?", makeBuiltin("integer?", func(args []*value, line, col int) (*value, error) {
		if len(args) != 1 {
			return nil, &EvalError{Message: fmt.Sprintf("%d:%d: integer?: expected 1 argument", line, col)}
		}
		switch args[0].typ {
		case valInt:
			return boolVal(true), nil
		case valRational:
			// e.g., 4/2 simplifies to int, but if it's still rational, den != 1
			return boolVal(false), nil
		case valFloat:
			f := args[0].fval
			return boolVal(f == math.Trunc(f) && !math.IsInf(f, 0) && !math.IsNaN(f)), nil
		}
		return boolVal(false), nil
	}))

	e.set("rational?", makeBuiltin("rational?", func(args []*value, line, col int) (*value, error) {
		if len(args) != 1 {
			return nil, &EvalError{Message: fmt.Sprintf("%d:%d: rational?: expected 1 argument", line, col)}
		}
		return boolVal(args[0].typ == valInt || args[0].typ == valRational), nil
	}))

	e.set("exact?", makeBuiltin("exact?", func(args []*value, line, col int) (*value, error) {
		if len(args) != 1 {
			return nil, &EvalError{Message: fmt.Sprintf("%d:%d: exact?: expected 1 argument", line, col)}
		}
		return boolVal(isExact(args[0])), nil
	}))

	e.set("inexact?", makeBuiltin("inexact?", func(args []*value, line, col int) (*value, error) {
		if len(args) != 1 {
			return nil, &EvalError{Message: fmt.Sprintf("%d:%d: inexact?: expected 1 argument", line, col)}
		}
		return boolVal(args[0].typ == valFloat), nil
	}))

	e.set("exact->inexact", makeBuiltin("exact->inexact", func(args []*value, line, col int) (*value, error) {
		if len(args) != 1 || !isNumeric(args[0]) {
			return nil, &EvalError{Message: fmt.Sprintf("%d:%d: exact->inexact: expected number", line, col)}
		}
		return floatVal(toFloat(args[0])), nil
	}))

	e.set("inexact->exact", makeBuiltin("inexact->exact", func(args []*value, line, col int) (*value, error) {
		if len(args) != 1 || !isNumeric(args[0]) {
			return nil, &EvalError{Message: fmt.Sprintf("%d:%d: inexact->exact: expected number", line, col)}
		}
		if isExact(args[0]) {
			return args[0], nil
		}
		// Convert float to rational
		f := args[0].fval
		// Use the standard approach: multiply out the decimal
		if f == math.Trunc(f) {
			return intVal(int64(f)), nil
		}
		// Convert via fraction approximation
		n, d := floatToRational(f)
		return ratVal(n, d), nil
	}))

	e.set("numerator", makeBuiltin("numerator", func(args []*value, line, col int) (*value, error) {
		if len(args) != 1 || !isNumeric(args[0]) {
			return nil, &EvalError{Message: fmt.Sprintf("%d:%d: numerator: expected number", line, col)}
		}
		switch args[0].typ {
		case valInt:
			return args[0], nil
		case valRational:
			return intVal(args[0].num), nil
		case valFloat:
			n, d := floatToRational(args[0].fval)
			g := gcd(n, d)
			return floatVal(float64(n / g)), nil
		}
		return nil, &EvalError{Message: fmt.Sprintf("%d:%d: numerator: expected number", line, col)}
	}))

	e.set("denominator", makeBuiltin("denominator", func(args []*value, line, col int) (*value, error) {
		if len(args) != 1 || !isNumeric(args[0]) {
			return nil, &EvalError{Message: fmt.Sprintf("%d:%d: denominator: expected number", line, col)}
		}
		switch args[0].typ {
		case valInt:
			return intVal(1), nil
		case valRational:
			return intVal(args[0].den), nil
		case valFloat:
			n, d := floatToRational(args[0].fval)
			g := gcd(n, d)
			return floatVal(float64(d / g)), nil
		}
		return nil, &EvalError{Message: fmt.Sprintf("%d:%d: denominator: expected number", line, col)}
	}))

	e.set("boolean?", makeBuiltin("boolean?", func(args []*value, line, col int) (*value, error) {
		if len(args) != 1 {
			return nil, &EvalError{Message: fmt.Sprintf("%d:%d: boolean?: expected 1 argument", line, col)}
		}
		return boolVal(args[0].typ == valBool), nil
	}))

	e.set("pair?", makeBuiltin("pair?", func(args []*value, line, col int) (*value, error) {
		if len(args) != 1 {
			return nil, &EvalError{Message: fmt.Sprintf("%d:%d: pair?: expected 1 argument", line, col)}
		}
		return boolVal(args[0].typ == valPair), nil
	}))

	e.set("symbol?", makeBuiltin("symbol?", func(args []*value, line, col int) (*value, error) {
		if len(args) != 1 {
			return nil, &EvalError{Message: fmt.Sprintf("%d:%d: symbol?: expected 1 argument", line, col)}
		}
		return boolVal(args[0].typ == valSymbol), nil
	}))

	e.set("char?", makeBuiltin("char?", func(args []*value, line, col int) (*value, error) {
		if len(args) != 1 {
			return nil, &EvalError{Message: fmt.Sprintf("%d:%d: char?: expected 1 argument", line, col)}
		}
		return boolVal(args[0].typ == valChar), nil
	}))

	// I/O
	e.set("display", makeBuiltin("display", func(args []*value, line, col int) (*value, error) {
		if len(args) != 1 {
			return nil, &EvalError{Message: fmt.Sprintf("%d:%d: display: expected 1 argument", line, col)}
		}
		ip.output.WriteString(args[0].displayString())
		return voidVal, nil
	}))

	e.set("write", makeBuiltin("write", func(args []*value, line, col int) (*value, error) {
		if len(args) != 1 {
			return nil, &EvalError{Message: fmt.Sprintf("%d:%d: write: expected 1 argument", line, col)}
		}
		ip.output.WriteString(args[0].String())
		return voidVal, nil
	}))

	e.set("newline", makeBuiltin("newline", func(args []*value, line, col int) (*value, error) {
		ip.output.WriteByte('\n')
		return voidVal, nil
	}))

	// String operations
	e.set("string-append", makeBuiltin("string-append", func(args []*value, line, col int) (*value, error) {
		var buf strings.Builder
		for _, a := range args {
			if a.typ != valString {
				return nil, &EvalError{Message: fmt.Sprintf("%d:%d: string-append: expected string", line, col)}
			}
			buf.WriteString(a.sval)
		}
		return strVal(buf.String()), nil
	}))

	e.set("string-length", makeBuiltin("string-length", func(args []*value, line, col int) (*value, error) {
		if len(args) != 1 || args[0].typ != valString {
			return nil, &EvalError{Message: fmt.Sprintf("%d:%d: string-length: expected string", line, col)}
		}
		return intVal(int64(len([]rune(args[0].sval)))), nil
	}))

	e.set("substring", makeBuiltin("substring", func(args []*value, line, col int) (*value, error) {
		if len(args) != 3 || args[0].typ != valString || args[1].typ != valInt || args[2].typ != valInt {
			return nil, &EvalError{Message: fmt.Sprintf("%d:%d: substring: bad arguments", line, col)}
		}
		runes := []rune(args[0].sval)
		start := int(args[1].ival)
		end := int(args[2].ival)
		if start < 0 || end < start || end > len(runes) {
			return nil, &EvalError{Message: fmt.Sprintf("%d:%d: substring: index out of range", line, col)}
		}
		return strVal(string(runes[start:end])), nil
	}))

	e.set("string->number", makeBuiltin("string->number", func(args []*value, line, col int) (*value, error) {
		if len(args) != 1 || args[0].typ != valString {
			return nil, &EvalError{Message: fmt.Sprintf("%d:%d: string->number: expected string", line, col)}
		}
		n, err := strconv.ParseInt(args[0].sval, 10, 64)
		if err != nil {
			return boolVal(false), nil
		}
		return intVal(n), nil
	}))

	e.set("number->string", makeBuiltin("number->string", func(args []*value, line, col int) (*value, error) {
		if len(args) != 1 || args[0].typ != valInt {
			return nil, &EvalError{Message: fmt.Sprintf("%d:%d: number->string: expected number", line, col)}
		}
		return strVal(strconv.FormatInt(args[0].ival, 10)), nil
	}))

	e.set("symbol->string", makeBuiltin("symbol->string", func(args []*value, line, col int) (*value, error) {
		if len(args) != 1 || args[0].typ != valSymbol {
			return nil, &EvalError{Message: fmt.Sprintf("%d:%d: symbol->string: expected symbol", line, col)}
		}
		return strVal(args[0].sval), nil
	}))

	e.set("string->symbol", makeBuiltin("string->symbol", func(args []*value, line, col int) (*value, error) {
		if len(args) != 1 || args[0].typ != valString {
			return nil, &EvalError{Message: fmt.Sprintf("%d:%d: string->symbol: expected string", line, col)}
		}
		return symVal(args[0].sval), nil
	}))

	e.set("string-copy", makeBuiltin("string-copy", func(args []*value, line, col int) (*value, error) {
		if len(args) != 1 || args[0].typ != valString {
			return nil, &EvalError{Message: fmt.Sprintf("%d:%d: string-copy: expected string", line, col)}
		}
		v := strVal(args[0].sval)
		v.mutable = true
		return v, nil
	}))

	e.set("string-set!", makeBuiltin("string-set!", func(args []*value, line, col int) (*value, error) {
		if len(args) != 3 || args[0].typ != valString || args[1].typ != valInt || args[2].typ != valChar {
			return nil, &EvalError{Message: fmt.Sprintf("%d:%d: string-set!: bad arguments", line, col)}
		}
		if !args[0].mutable {
			return nil, &EvalError{Message: fmt.Sprintf("%d:%d: string-set!: strings are immutable", line, col)}
		}
		runes := []rune(args[0].sval)
		idx := int(args[1].ival)
		if idx < 0 || idx >= len(runes) {
			return nil, &EvalError{Message: fmt.Sprintf("%d:%d: string-set!: index out of range", line, col)}
		}
		runes[idx] = args[2].cval
		args[0].sval = string(runes)
		return voidVal, nil
	}))

	e.set("string-ref", makeBuiltin("string-ref", func(args []*value, line, col int) (*value, error) {
		if len(args) != 2 || args[0].typ != valString || args[1].typ != valInt {
			return nil, &EvalError{Message: fmt.Sprintf("%d:%d: string-ref: bad arguments", line, col)}
		}
		runes := []rune(args[0].sval)
		idx := int(args[1].ival)
		if idx < 0 || idx >= len(runes) {
			return nil, &EvalError{Message: fmt.Sprintf("%d:%d: string-ref: index out of range", line, col)}
		}
		return charVal(runes[idx]), nil
	}))

	e.set("string->list", makeBuiltin("string->list", func(args []*value, line, col int) (*value, error) {
		if len(args) != 1 || args[0].typ != valString {
			return nil, &EvalError{Message: fmt.Sprintf("%d:%d: string->list: expected string", line, col)}
		}
		runes := []rune(args[0].sval)
		result := nilVal
		for i := len(runes) - 1; i >= 0; i-- {
			result = &value{typ: valPair, car: charVal(runes[i]), cdr: result}
		}
		return result, nil
	}))

	e.set("list->string", makeBuiltin("list->string", func(args []*value, line, col int) (*value, error) {
		if len(args) != 1 {
			return nil, &EvalError{Message: fmt.Sprintf("%d:%d: list->string: expected 1 argument", line, col)}
		}
		var buf strings.Builder
		cur := args[0]
		for cur.typ == valPair {
			if cur.car.typ != valChar {
				return nil, &EvalError{Message: fmt.Sprintf("%d:%d: list->string: expected list of chars", line, col)}
			}
			buf.WriteRune(cur.car.cval)
			cur = cur.cdr
		}
		if cur.typ != valNil {
			return nil, &EvalError{Message: fmt.Sprintf("%d:%d: list->string: expected proper list", line, col)}
		}
		return strVal(buf.String()), nil
	}))

	e.set("char->integer", makeBuiltin("char->integer", func(args []*value, line, col int) (*value, error) {
		if len(args) != 1 || args[0].typ != valChar {
			return nil, &EvalError{Message: fmt.Sprintf("%d:%d: char->integer: expected char", line, col)}
		}
		return intVal(int64(args[0].cval)), nil
	}))

	e.set("integer->char", makeBuiltin("integer->char", func(args []*value, line, col int) (*value, error) {
		if len(args) != 1 || args[0].typ != valInt {
			return nil, &EvalError{Message: fmt.Sprintf("%d:%d: integer->char: expected integer", line, col)}
		}
		return charVal(rune(args[0].ival)), nil
	}))

	// eq? — pointer/identity equality
	e.set("eq?", makeBuiltin("eq?", func(args []*value, line, col int) (*value, error) {
		if len(args) != 2 {
			return nil, &EvalError{Message: fmt.Sprintf("%d:%d: eq?: expected 2 arguments", line, col)}
		}
		return boolVal(schemeEq(args[0], args[1])), nil
	}))

	// equal? — deep structural equality
	e.set("equal?", makeBuiltin("equal?", func(args []*value, line, col int) (*value, error) {
		if len(args) != 2 {
			return nil, &EvalError{Message: fmt.Sprintf("%d:%d: equal?: expected 2 arguments", line, col)}
		}
		return boolVal(schemeEqual(args[0], args[1])), nil
	}))

	// map — supports multiple list arguments
	e.set("map", makeBuiltin("map", func(args []*value, line, col int) (*value, error) {
		if len(args) < 2 {
			return nil, &EvalError{Message: fmt.Sprintf("%d:%d: map: expected at least 2 arguments", line, col)}
		}
		fn := args[0]
		lists := args[1:]
		// Collect current pointers
		curs := make([]*value, len(lists))
		copy(curs, lists)
		var result []*value
		for {
			// Check if any list is exhausted
			allPair := true
			for _, c := range curs {
				if c.typ != valPair {
					allPair = false
					break
				}
			}
			if !allPair {
				break
			}
			// Collect car of each list
			fnArgs := make([]*value, len(curs))
			for i, c := range curs {
				fnArgs[i] = c.car
			}
			// Apply fn
			var v *value
			var err error
			if fn.typ == valLambda {
				localEnv, bindErr := bindLambdaArgs(fn, fnArgs, line, col)
				if bindErr != nil {
					return nil, bindErr
				}
				for _, bodyExpr := range fn.body[:len(fn.body)-1] {
					_, err = ip.eval(bodyExpr, localEnv)
					if err != nil {
						return nil, err
					}
				}
				v, err = ip.eval(fn.body[len(fn.body)-1], localEnv)
			} else if fn.typ == valBuiltin {
				v, err = fn.builtin(fnArgs, line, col)
			} else {
				return nil, &EvalError{Message: fmt.Sprintf("%d:%d: map: not a procedure", line, col)}
			}
			if err != nil {
				return nil, err
			}
			result = append(result, v)
			// Advance all pointers
			for i, c := range curs {
				curs[i] = c.cdr
			}
		}
		// Build list from result
		out := nilVal
		for i := len(result) - 1; i >= 0; i-- {
			out = &value{typ: valPair, car: result[i], cdr: out}
		}
		return out, nil
	}))

	// abs
	e.set("abs", makeBuiltin("abs", func(args []*value, line, col int) (*value, error) {
		if len(args) != 1 || args[0].typ != valInt {
			return nil, &EvalError{Message: fmt.Sprintf("%d:%d: abs: expected number", line, col)}
		}
		n := args[0].ival
		if n < 0 {
			n = -n
		}
		return intVal(n), nil
	}))

	// modulo — result takes sign of divisor
	e.set("modulo", makeBuiltin("modulo", func(args []*value, line, col int) (*value, error) {
		if len(args) != 2 || args[0].typ != valInt || args[1].typ != valInt {
			return nil, &EvalError{Message: fmt.Sprintf("%d:%d: modulo: expected 2 numbers", line, col)}
		}
		if args[1].ival == 0 {
			return nil, &EvalError{Message: fmt.Sprintf("%d:%d: modulo: division by zero", line, col)}
		}
		a, b := args[0].ival, args[1].ival
		r := a % b
		if r != 0 && (r > 0) != (b > 0) {
			r += b
		}
		return intVal(r), nil
	}))

	// remainder — result takes sign of dividend
	e.set("remainder", makeBuiltin("remainder", func(args []*value, line, col int) (*value, error) {
		if len(args) != 2 || args[0].typ != valInt || args[1].typ != valInt {
			return nil, &EvalError{Message: fmt.Sprintf("%d:%d: remainder: expected 2 numbers", line, col)}
		}
		if args[1].ival == 0 {
			return nil, &EvalError{Message: fmt.Sprintf("%d:%d: remainder: division by zero", line, col)}
		}
		return intVal(args[0].ival % args[1].ival), nil
	}))

	// quotient — truncated integer division
	e.set("quotient", makeBuiltin("quotient", func(args []*value, line, col int) (*value, error) {
		if len(args) != 2 || args[0].typ != valInt || args[1].typ != valInt {
			return nil, &EvalError{Message: fmt.Sprintf("%d:%d: quotient: expected 2 numbers", line, col)}
		}
		if args[1].ival == 0 {
			return nil, &EvalError{Message: fmt.Sprintf("%d:%d: quotient: division by zero", line, col)}
		}
		return intVal(args[0].ival / args[1].ival), nil
	}))

	// min, max — variadic
	e.set("min", makeBuiltin("min", func(args []*value, line, col int) (*value, error) {
		if len(args) == 0 {
			return nil, &EvalError{Message: fmt.Sprintf("%d:%d: min: expected at least 1 argument", line, col)}
		}
		m := args[0].ival
		for _, a := range args[1:] {
			if a.ival < m {
				m = a.ival
			}
		}
		return intVal(m), nil
	}))

	e.set("max", makeBuiltin("max", func(args []*value, line, col int) (*value, error) {
		if len(args) == 0 {
			return nil, &EvalError{Message: fmt.Sprintf("%d:%d: max: expected at least 1 argument", line, col)}
		}
		m := args[0].ival
		for _, a := range args[1:] {
			if a.ival > m {
				m = a.ival
			}
		}
		return intVal(m), nil
	}))

	// expt — integer exponentiation
	e.set("expt", makeBuiltin("expt", func(args []*value, line, col int) (*value, error) {
		if len(args) != 2 || args[0].typ != valInt || args[1].typ != valInt {
			return nil, &EvalError{Message: fmt.Sprintf("%d:%d: expt: expected 2 numbers", line, col)}
		}
		base, exp := args[0].ival, args[1].ival
		result := int64(1)
		for i := int64(0); i < exp; i++ {
			result *= base
		}
		return intVal(result), nil
	}))

	// zero?, positive?, negative?
	e.set("zero?", makeBuiltin("zero?", func(args []*value, line, col int) (*value, error) {
		if len(args) != 1 || args[0].typ != valInt {
			return nil, &EvalError{Message: fmt.Sprintf("%d:%d: zero?: expected number", line, col)}
		}
		return boolVal(args[0].ival == 0), nil
	}))

	e.set("positive?", makeBuiltin("positive?", func(args []*value, line, col int) (*value, error) {
		if len(args) != 1 || args[0].typ != valInt {
			return nil, &EvalError{Message: fmt.Sprintf("%d:%d: positive?: expected number", line, col)}
		}
		return boolVal(args[0].ival > 0), nil
	}))

	e.set("negative?", makeBuiltin("negative?", func(args []*value, line, col int) (*value, error) {
		if len(args) != 1 || args[0].typ != valInt {
			return nil, &EvalError{Message: fmt.Sprintf("%d:%d: negative?: expected number", line, col)}
		}
		return boolVal(args[0].ival < 0), nil
	}))

	// odd?, even?
	e.set("odd?", makeBuiltin("odd?", func(args []*value, line, col int) (*value, error) {
		if len(args) != 1 || args[0].typ != valInt {
			return nil, &EvalError{Message: fmt.Sprintf("%d:%d: odd?: expected number", line, col)}
		}
		return boolVal(args[0].ival%2 != 0), nil
	}))

	e.set("even?", makeBuiltin("even?", func(args []*value, line, col int) (*value, error) {
		if len(args) != 1 || args[0].typ != valInt {
			return nil, &EvalError{Message: fmt.Sprintf("%d:%d: even?: expected number", line, col)}
		}
		return boolVal(args[0].ival%2 == 0), nil
	}))

	// list-ref
	e.set("list-ref", makeBuiltin("list-ref", func(args []*value, line, col int) (*value, error) {
		if len(args) != 2 || args[1].typ != valInt {
			return nil, &EvalError{Message: fmt.Sprintf("%d:%d: list-ref: bad arguments", line, col)}
		}
		idx := int(args[1].ival)
		cur := args[0]
		for i := 0; i < idx; i++ {
			if cur.typ != valPair {
				return nil, &EvalError{Message: fmt.Sprintf("%d:%d: list-ref: index out of range", line, col)}
			}
			cur = cur.cdr
		}
		if cur.typ != valPair {
			return nil, &EvalError{Message: fmt.Sprintf("%d:%d: list-ref: index out of range", line, col)}
		}
		return cur.car, nil
	}))

	// list-tail
	e.set("list-tail", makeBuiltin("list-tail", func(args []*value, line, col int) (*value, error) {
		if len(args) != 2 || args[1].typ != valInt {
			return nil, &EvalError{Message: fmt.Sprintf("%d:%d: list-tail: bad arguments", line, col)}
		}
		idx := int(args[1].ival)
		cur := args[0]
		for i := 0; i < idx; i++ {
			if cur.typ != valPair {
				return nil, &EvalError{Message: fmt.Sprintf("%d:%d: list-tail: index out of range", line, col)}
			}
			cur = cur.cdr
		}
		return cur, nil
	}))

	// list? — with tortoise-and-hare cycle detection
	e.set("list?", makeBuiltin("list?", func(args []*value, line, col int) (*value, error) {
		if len(args) != 1 {
			return nil, &EvalError{Message: fmt.Sprintf("%d:%d: list?: expected 1 argument", line, col)}
		}
		slow := args[0]
		fast := args[0]
		for fast.typ == valPair {
			fast = fast.cdr
			if fast.typ != valPair {
				break
			}
			fast = fast.cdr
			slow = slow.cdr
			if slow == fast {
				return boolVal(false), nil // cycle detected
			}
		}
		return boolVal(fast.typ == valNil), nil
	}))

	// assoc — uses equal?
	e.set("assoc", makeBuiltin("assoc", func(args []*value, line, col int) (*value, error) {
		if len(args) != 2 {
			return nil, &EvalError{Message: fmt.Sprintf("%d:%d: assoc: expected 2 arguments", line, col)}
		}
		key := args[0]
		cur := args[1]
		for cur.typ == valPair {
			pair := cur.car
			if pair.typ == valPair && schemeEqual(pair.car, key) {
				return pair, nil
			}
			cur = cur.cdr
		}
		return boolVal(false), nil
	}))

	// Character predicates and operations
	e.set("char-alphabetic?", makeBuiltin("char-alphabetic?", func(args []*value, line, col int) (*value, error) {
		if len(args) != 1 || args[0].typ != valChar {
			return nil, &EvalError{Message: fmt.Sprintf("%d:%d: char-alphabetic?: expected char", line, col)}
		}
		return boolVal(unicode.IsLetter(args[0].cval)), nil
	}))

	e.set("char-numeric?", makeBuiltin("char-numeric?", func(args []*value, line, col int) (*value, error) {
		if len(args) != 1 || args[0].typ != valChar {
			return nil, &EvalError{Message: fmt.Sprintf("%d:%d: char-numeric?: expected char", line, col)}
		}
		return boolVal(unicode.IsDigit(args[0].cval)), nil
	}))

	e.set("char-upcase", makeBuiltin("char-upcase", func(args []*value, line, col int) (*value, error) {
		if len(args) != 1 || args[0].typ != valChar {
			return nil, &EvalError{Message: fmt.Sprintf("%d:%d: char-upcase: expected char", line, col)}
		}
		return charVal(unicode.ToUpper(args[0].cval)), nil
	}))

	e.set("char-downcase", makeBuiltin("char-downcase", func(args []*value, line, col int) (*value, error) {
		if len(args) != 1 || args[0].typ != valChar {
			return nil, &EvalError{Message: fmt.Sprintf("%d:%d: char-downcase: expected char", line, col)}
		}
		return charVal(unicode.ToLower(args[0].cval)), nil
	}))

	e.set("char=?", makeBuiltin("char=?", func(args []*value, line, col int) (*value, error) {
		if len(args) != 2 || args[0].typ != valChar || args[1].typ != valChar {
			return nil, &EvalError{Message: fmt.Sprintf("%d:%d: char=?: expected 2 chars", line, col)}
		}
		return boolVal(args[0].cval == args[1].cval), nil
	}))

	e.set("char<?", makeBuiltin("char<?", func(args []*value, line, col int) (*value, error) {
		if len(args) != 2 || args[0].typ != valChar || args[1].typ != valChar {
			return nil, &EvalError{Message: fmt.Sprintf("%d:%d: char<?: expected 2 chars", line, col)}
		}
		return boolVal(args[0].cval < args[1].cval), nil
	}))

	// String comparison
	e.set("string=?", makeBuiltin("string=?", func(args []*value, line, col int) (*value, error) {
		if len(args) != 2 || args[0].typ != valString || args[1].typ != valString {
			return nil, &EvalError{Message: fmt.Sprintf("%d:%d: string=?: expected 2 strings", line, col)}
		}
		return boolVal(args[0].sval == args[1].sval), nil
	}))

	e.set("string<?", makeBuiltin("string<?", func(args []*value, line, col int) (*value, error) {
		if len(args) != 2 || args[0].typ != valString || args[1].typ != valString {
			return nil, &EvalError{Message: fmt.Sprintf("%d:%d: string<?: expected 2 strings", line, col)}
		}
		return boolVal(args[0].sval < args[1].sval), nil
	}))

	e.set("string-ci=?", makeBuiltin("string-ci=?", func(args []*value, line, col int) (*value, error) {
		if len(args) != 2 || args[0].typ != valString || args[1].typ != valString {
			return nil, &EvalError{Message: fmt.Sprintf("%d:%d: string-ci=?: expected 2 strings", line, col)}
		}
		return boolVal(strings.EqualFold(args[0].sval, args[1].sval)), nil
	}))

	e.set("string-upcase", makeBuiltin("string-upcase", func(args []*value, line, col int) (*value, error) {
		if len(args) != 1 || args[0].typ != valString {
			return nil, &EvalError{Message: fmt.Sprintf("%d:%d: string-upcase: expected string", line, col)}
		}
		return strVal(strings.ToUpper(args[0].sval)), nil
	}))

	e.set("string-downcase", makeBuiltin("string-downcase", func(args []*value, line, col int) (*value, error) {
		if len(args) != 1 || args[0].typ != valString {
			return nil, &EvalError{Message: fmt.Sprintf("%d:%d: string-downcase: expected string", line, col)}
		}
		return strVal(strings.ToLower(args[0].sval)), nil
	}))

	// eqv?
	e.set("eqv?", makeBuiltin("eqv?", func(args []*value, line, col int) (*value, error) {
		if len(args) != 2 {
			return nil, &EvalError{Message: fmt.Sprintf("%d:%d: eqv?: expected 2 arguments", line, col)}
		}
		return boolVal(schemeEqv(args[0], args[1])), nil
	}))

	// assq — uses eq?
	e.set("assq", makeBuiltin("assq", func(args []*value, line, col int) (*value, error) {
		if len(args) != 2 {
			return nil, &EvalError{Message: fmt.Sprintf("%d:%d: assq: expected 2 arguments", line, col)}
		}
		key := args[0]
		cur := args[1]
		for cur.typ == valPair {
			pair := cur.car
			if pair.typ == valPair && schemeEq(pair.car, key) {
				return pair, nil
			}
			cur = cur.cdr
		}
		return boolVal(false), nil
	}))

	// memq — uses eq?
	e.set("memq", makeBuiltin("memq", func(args []*value, line, col int) (*value, error) {
		if len(args) != 2 {
			return nil, &EvalError{Message: fmt.Sprintf("%d:%d: memq: expected 2 arguments", line, col)}
		}
		key := args[0]
		cur := args[1]
		for cur.typ == valPair {
			if schemeEq(cur.car, key) {
				return cur, nil
			}
			cur = cur.cdr
		}
		return boolVal(false), nil
	}))

	e.set("memv", makeBuiltin("memv", func(args []*value, line, col int) (*value, error) {
		if len(args) != 2 {
			return nil, &EvalError{Message: fmt.Sprintf("%d:%d: memv: expected 2 arguments", line, col)}
		}
		key := args[0]
		cur := args[1]
		for cur.typ == valPair {
			if schemeEqv(cur.car, key) {
				return cur, nil
			}
			cur = cur.cdr
		}
		return boolVal(false), nil
	}))

	// Vector operations
	e.set("vector", makeBuiltin("vector", func(args []*value, line, col int) (*value, error) {
		elems := make([]*value, len(args))
		copy(elems, args)
		return &value{typ: valVector, vecval: elems}, nil
	}))

	e.set("make-vector", makeBuiltin("make-vector", func(args []*value, line, col int) (*value, error) {
		if len(args) < 1 || len(args) > 2 || args[0].typ != valInt {
			return nil, &EvalError{Message: fmt.Sprintf("%d:%d: make-vector: bad arguments", line, col)}
		}
		n := int(args[0].ival)
		fill := voidVal
		if len(args) == 2 {
			fill = args[1]
		}
		elems := make([]*value, n)
		for i := range elems {
			elems[i] = fill
		}
		return &value{typ: valVector, vecval: elems}, nil
	}))

	e.set("vector-ref", makeBuiltin("vector-ref", func(args []*value, line, col int) (*value, error) {
		if len(args) != 2 || args[0].typ != valVector || args[1].typ != valInt {
			return nil, &EvalError{Message: fmt.Sprintf("%d:%d: vector-ref: bad arguments", line, col)}
		}
		idx := int(args[1].ival)
		if idx < 0 || idx >= len(args[0].vecval) {
			return nil, &EvalError{Message: fmt.Sprintf("%d:%d: vector-ref: index out of range", line, col)}
		}
		return args[0].vecval[idx], nil
	}))

	e.set("vector-set!", makeBuiltin("vector-set!", func(args []*value, line, col int) (*value, error) {
		if len(args) != 3 || args[0].typ != valVector || args[1].typ != valInt {
			return nil, &EvalError{Message: fmt.Sprintf("%d:%d: vector-set!: bad arguments", line, col)}
		}
		idx := int(args[1].ival)
		if idx < 0 || idx >= len(args[0].vecval) {
			return nil, &EvalError{Message: fmt.Sprintf("%d:%d: vector-set!: index out of range", line, col)}
		}
		args[0].vecval[idx] = args[2]
		return voidVal, nil
	}))

	e.set("vector-length", makeBuiltin("vector-length", func(args []*value, line, col int) (*value, error) {
		if len(args) != 1 || args[0].typ != valVector {
			return nil, &EvalError{Message: fmt.Sprintf("%d:%d: vector-length: expected vector", line, col)}
		}
		return intVal(int64(len(args[0].vecval))), nil
	}))

	e.set("vector?", makeBuiltin("vector?", func(args []*value, line, col int) (*value, error) {
		if len(args) != 1 {
			return nil, &EvalError{Message: fmt.Sprintf("%d:%d: vector?: expected 1 argument", line, col)}
		}
		return boolVal(args[0].typ == valVector), nil
	}))

	e.set("vector->list", makeBuiltin("vector->list", func(args []*value, line, col int) (*value, error) {
		if len(args) != 1 || args[0].typ != valVector {
			return nil, &EvalError{Message: fmt.Sprintf("%d:%d: vector->list: expected vector", line, col)}
		}
		result := nilVal
		for i := len(args[0].vecval) - 1; i >= 0; i-- {
			result = &value{typ: valPair, car: args[0].vecval[i], cdr: result}
		}
		return result, nil
	}))

	e.set("list->vector", makeBuiltin("list->vector", func(args []*value, line, col int) (*value, error) {
		if len(args) != 1 {
			return nil, &EvalError{Message: fmt.Sprintf("%d:%d: list->vector: expected 1 argument", line, col)}
		}
		var elems []*value
		cur := args[0]
		for cur.typ == valPair {
			elems = append(elems, cur.car)
			cur = cur.cdr
		}
		return &value{typ: valVector, vecval: elems}, nil
	}))

	// raise
	e.set("error", makeBuiltin("error", func(args []*value, line, col int) (*value, error) {
		if len(args) < 1 {
			return nil, &EvalError{Message: fmt.Sprintf("%d:%d: error: expected at least 1 argument", line, col)}
		}
		msg := args[0].displayString()
		for _, a := range args[1:] {
			msg += " " + a.String()
		}
		return nil, &EvalError{Message: msg}
	}))

	e.set("raise", makeBuiltin("raise", func(args []*value, line, col int) (*value, error) {
		if len(args) != 1 {
			return nil, &EvalError{Message: fmt.Sprintf("%d:%d: raise: expected 1 argument", line, col)}
		}
		panic(&schemeRaise{val: args[0]})
	}))

	// values
	e.set("values", makeBuiltin("values", func(args []*value, line, col int) (*value, error) {
		if len(args) == 1 {
			return args[0], nil
		}
		return &value{typ: valMultipleValues, multiVals: args}, nil
	}))

	// call-with-values
	e.set("call-with-values", makeBuiltin("call-with-values", func(args []*value, line, col int) (*value, error) {
		if len(args) != 2 {
			return nil, &EvalError{Message: fmt.Sprintf("%d:%d: call-with-values: expected 2 arguments", line, col)}
		}
		producer := args[0]
		consumer := args[1]

		// Call producer with no arguments
		produced, err := ip.applyProc(producer, nil, line, col)
		if err != nil {
			return nil, err
		}

		// Unpack multiple values
		var consumerArgs []*value
		if produced != nil && produced.typ == valMultipleValues {
			consumerArgs = produced.multiVals
		} else if produced != nil {
			consumerArgs = []*value{produced}
		}

		// Call consumer with produced values
		return ip.applyProc(consumer, consumerArgs, line, col)
	}))

	// cxr helpers
	cxrHelper := func(name string, ops string) {
		e.set(name, makeBuiltin(name, func(args []*value, line, col int) (*value, error) {
			if len(args) != 1 {
				return nil, &EvalError{Message: fmt.Sprintf("%d:%d: %s: expected 1 argument", line, col, name)}
			}
			v := args[0]
			for i := len(ops) - 1; i >= 0; i-- {
				if v.typ != valPair {
					return nil, &EvalError{Message: fmt.Sprintf("%d:%d: %s: not a pair", line, col, name)}
				}
				if ops[i] == 'a' {
					v = v.car
				} else {
					v = v.cdr
				}
			}
			return v, nil
		}))
	}
	cxrHelper("caar", "aa")
	cxrHelper("cadr", "ad")
	cxrHelper("cdar", "da")
	cxrHelper("cddr", "dd")
	cxrHelper("caddr", "add")
	cxrHelper("cadddr", "addd")
	cxrHelper("caaar", "aaa")
	cxrHelper("caadr", "aad")
	cxrHelper("cadar", "ada")
	cxrHelper("caaddr", "aadd")
	cxrHelper("cadaar", "adaa")
	cxrHelper("cadadr", "adad")
	cxrHelper("caddar", "adda")
	cxrHelper("cdaar", "daa")
	cxrHelper("cdadr", "dad")
	cxrHelper("cddar", "dda")
	cxrHelper("cdddr", "ddd")
	cxrHelper("caaaar", "aaaa")
	cxrHelper("caaadr", "aaad")
	cxrHelper("caadar", "aada")
	cxrHelper("cdaaar", "daaa")
	cxrHelper("cdaadr", "daad")
	cxrHelper("cdadar", "dada")
	cxrHelper("cdaddr", "dadd")
	cxrHelper("cddaar", "ddaa")
	cxrHelper("cddadr", "ddad")
	cxrHelper("cdddar", "ddda")
	cxrHelper("cddddr", "dddd")

	// assv (uses eqv?)
	e.set("assv", makeBuiltin("assv", func(args []*value, line, col int) (*value, error) {
		if len(args) != 2 {
			return nil, &EvalError{Message: fmt.Sprintf("%d:%d: assv: expected 2 arguments", line, col)}
		}
		key := args[0]
		cur := args[1]
		for cur.typ == valPair {
			pair := cur.car
			if pair.typ == valPair && schemeEqv(pair.car, key) {
				return pair, nil
			}
			cur = cur.cdr
		}
		return boolVal(false), nil
	}))

	// member (uses equal?)
	e.set("member", makeBuiltin("member", func(args []*value, line, col int) (*value, error) {
		if len(args) != 2 {
			return nil, &EvalError{Message: fmt.Sprintf("%d:%d: member: expected 2 arguments", line, col)}
		}
		obj := args[0]
		cur := args[1]
		for cur.typ == valPair {
			if schemeEqual(obj, cur.car) {
				return cur, nil
			}
			cur = cur.cdr
		}
		return boolVal(false), nil
	}))

	// set-car!
	e.set("set-car!", makeBuiltin("set-car!", func(args []*value, line, col int) (*value, error) {
		if len(args) != 2 {
			return nil, &EvalError{Message: fmt.Sprintf("%d:%d: set-car!: expected 2 arguments", line, col)}
		}
		if args[0].typ != valPair {
			return nil, &EvalError{Message: fmt.Sprintf("%d:%d: set-car!: not a pair", line, col)}
		}
		args[0].car = args[1]
		return voidVal, nil
	}))

	// set-cdr!
	e.set("set-cdr!", makeBuiltin("set-cdr!", func(args []*value, line, col int) (*value, error) {
		if len(args) != 2 {
			return nil, &EvalError{Message: fmt.Sprintf("%d:%d: set-cdr!: expected 2 arguments", line, col)}
		}
		if args[0].typ != valPair {
			return nil, &EvalError{Message: fmt.Sprintf("%d:%d: set-cdr!: not a pair", line, col)}
		}
		args[0].cdr = args[1]
		return voidVal, nil
	}))

	// for-each — like map but discards results
	e.set("for-each", makeBuiltin("for-each", func(args []*value, line, col int) (*value, error) {
		if len(args) < 2 {
			return nil, &EvalError{Message: fmt.Sprintf("%d:%d: for-each: expected at least 2 arguments", line, col)}
		}
		fn := args[0]
		lists := args[1:]
		curs := make([]*value, len(lists))
		copy(curs, lists)
		for {
			allPair := true
			for _, c := range curs {
				if c.typ != valPair {
					allPair = false
					break
				}
			}
			if !allPair {
				break
			}
			fnArgs := make([]*value, len(curs))
			for i, c := range curs {
				fnArgs[i] = c.car
			}
			var err error
			if fn.typ == valLambda {
				localEnv, bindErr := bindLambdaArgs(fn, fnArgs, line, col)
				if bindErr != nil {
					return nil, bindErr
				}
				for _, bodyExpr := range fn.body {
					_, err = ip.eval(bodyExpr, localEnv)
					if err != nil {
						return nil, err
					}
				}
			} else if fn.typ == valBuiltin {
				_, err = fn.builtin(fnArgs, line, col)
			} else {
				return nil, &EvalError{Message: fmt.Sprintf("%d:%d: for-each: not a procedure", line, col)}
			}
			if err != nil {
				return nil, err
			}
			for i, c := range curs {
				curs[i] = c.cdr
			}
		}
		return voidVal, nil
	}))

	// procedure?
	e.set("procedure?", makeBuiltin("procedure?", func(args []*value, line, col int) (*value, error) {
		if len(args) != 1 {
			return nil, &EvalError{Message: fmt.Sprintf("%d:%d: procedure?: expected 1 argument", line, col)}
		}
		t := args[0].typ
		return boolVal(t == valLambda || t == valBuiltin || t == valContinuation), nil
	}))

	// gcd
	e.set("gcd", makeBuiltin("gcd", func(args []*value, line, col int) (*value, error) {
		if len(args) == 0 {
			return intVal(0), nil
		}
		result := args[0].ival
		if result < 0 {
			result = -result
		}
		for _, a := range args[1:] {
			result = gcd(result, a.ival)
		}
		return intVal(result), nil
	}))

	// lcm
	e.set("lcm", makeBuiltin("lcm", func(args []*value, line, col int) (*value, error) {
		if len(args) == 0 {
			return intVal(1), nil
		}
		result := args[0].ival
		if result < 0 {
			result = -result
		}
		for _, a := range args[1:] {
			b := a.ival
			if b < 0 {
				b = -b
			}
			if result == 0 || b == 0 {
				result = 0
			} else {
				result = result / gcd(result, b) * b
			}
		}
		return intVal(result), nil
	}))

	// truncate
	e.set("truncate", makeBuiltin("truncate", func(args []*value, line, col int) (*value, error) {
		if len(args) != 1 {
			return nil, &EvalError{Message: fmt.Sprintf("%d:%d: truncate: expected 1 argument", line, col)}
		}
		switch args[0].typ {
		case valInt:
			return args[0], nil
		case valFloat:
			return intVal(int64(args[0].fval)), nil
		default:
			return nil, &EvalError{Message: fmt.Sprintf("%d:%d: truncate: expected number", line, col)}
		}
	}))

	// round
	e.set("round", makeBuiltin("round", func(args []*value, line, col int) (*value, error) {
		if len(args) != 1 {
			return nil, &EvalError{Message: fmt.Sprintf("%d:%d: round: expected 1 argument", line, col)}
		}
		switch args[0].typ {
		case valInt:
			return args[0], nil
		case valFloat:
			return intVal(int64(math.RoundToEven(args[0].fval))), nil
		default:
			return nil, &EvalError{Message: fmt.Sprintf("%d:%d: round: expected number", line, col)}
		}
	}))

	// make-string
	e.set("make-string", makeBuiltin("make-string", func(args []*value, line, col int) (*value, error) {
		if len(args) < 1 || len(args) > 2 {
			return nil, &EvalError{Message: fmt.Sprintf("%d:%d: make-string: expected 1-2 arguments", line, col)}
		}
		n := int(args[0].ival)
		ch := ' '
		if len(args) == 2 && args[1].typ == valChar {
			ch = args[1].cval
		}
		return &value{typ: valString, sval: strings.Repeat(string(ch), n), mutable: true}, nil
	}))

	// string (creates string from chars)
	e.set("string", makeBuiltin("string", func(args []*value, line, col int) (*value, error) {
		var buf strings.Builder
		for _, a := range args {
			if a.typ != valChar {
				return nil, &EvalError{Message: fmt.Sprintf("%d:%d: string: expected char arguments", line, col)}
			}
			buf.WriteRune(a.cval)
		}
		return strVal(buf.String()), nil
	}))

	// string>?
	e.set("string>?", makeBuiltin("string>?", func(args []*value, line, col int) (*value, error) {
		if len(args) != 2 || args[0].typ != valString || args[1].typ != valString {
			return nil, &EvalError{Message: fmt.Sprintf("%d:%d: string>?: expected 2 strings", line, col)}
		}
		return boolVal(args[0].sval > args[1].sval), nil
	}))

	// string<=?
	e.set("string<=?", makeBuiltin("string<=?", func(args []*value, line, col int) (*value, error) {
		if len(args) != 2 || args[0].typ != valString || args[1].typ != valString {
			return nil, &EvalError{Message: fmt.Sprintf("%d:%d: string<=?: expected 2 strings", line, col)}
		}
		return boolVal(args[0].sval <= args[1].sval), nil
	}))

	// string>=?
	e.set("string>=?", makeBuiltin("string>=?", func(args []*value, line, col int) (*value, error) {
		if len(args) != 2 || args[0].typ != valString || args[1].typ != valString {
			return nil, &EvalError{Message: fmt.Sprintf("%d:%d: string>=?: expected 2 strings", line, col)}
		}
		return boolVal(args[0].sval >= args[1].sval), nil
	}))

	// call/cc as a first-class value
	callccVal := &value{typ: valBuiltin, sval: "call/cc", isCallCC: true}
	e.set("call/cc", callccVal)
	e.set("call-with-current-continuation", callccVal)

	// syntax->datum: unwrap a syntax object to a plain value
	e.set("syntax->datum", makeBuiltin("syntax->datum", func(args []*value, line, col int) (*value, error) {
		if len(args) != 1 {
			return nil, &EvalError{Message: fmt.Sprintf("%d:%d: syntax->datum: expected 1 argument", line, col)}
		}
		if args[0].typ != valSyntax {
			return nil, &EvalError{Message: fmt.Sprintf("%d:%d: syntax->datum: expected syntax object", line, col)}
		}
		return exprToValue(args[0].syntaxExpr), nil
	}))

	// datum->syntax: wrap a value as a syntax object with lexical context
	e.set("datum->syntax", makeBuiltin("datum->syntax", func(args []*value, line, col int) (*value, error) {
		if len(args) != 2 {
			return nil, &EvalError{Message: fmt.Sprintf("%d:%d: datum->syntax: expected 2 arguments", line, col)}
		}
		// First arg is a syntax object (for lexical context), second is a datum
		datum := args[1]
		return &value{typ: valSyntax, syntaxExpr: valueToExpr(datum)}, nil
	}))

	return e
}

func evalInput(input string) (last *value, ip *interp, err error) {
	tokens := tokenize(input)
	exprs, parseErr := parse(tokens)
	if parseErr != nil {
		return nil, nil, parseErr
	}
	if len(exprs) == 0 {
		return nil, &interp{}, nil
	}

	ip = &interp{}
	ip.exprs = exprs
	globalEnv := makeGlobalEnv(ip)

	startIdx := 0

	for {
		var jumpCaught *continuationJump

		func() {
			defer func() {
				if r := recover(); r != nil {
					if jump, ok := r.(*continuationJump); ok {
						jumpCaught = jump
					} else {
						panic(r)
					}
				}
			}()

			for i := startIdx; i < len(exprs); i++ {
				ip.exprIdx = i
				last, err = ip.eval(exprs[i], globalEnv)
				if err != nil {
					return
				}
			}
		}()

		if err != nil {
			return nil, nil, err
		}

		if jumpCaught != nil {
			// Run in-thunks for the target wind stack (rewinding)
			targetWind := jumpCaught.windStack
			for i := 0; i < len(targetWind); i++ {
				_, windErr := ip.callThunk(targetWind[i].inThunk, 0, 0)
				if windErr != nil {
					return nil, nil, windErr
				}
			}
			// Restore wind stack
			ip.windStack = make([]windEntry, len(targetWind))
			copy(ip.windStack, targetWind)

			// Reentrant continuation: replay from the saved expression index
			ip.replayExpr = jumpCaught.contExpr
			ip.replayValue = jumpCaught.val
			startIdx = jumpCaught.contIdx
			// Restore the let stack for replay (skip body exprs before call/cc)
			ip.replayLetStack = jumpCaught.letStack
			// Reset let tracking for the new evaluation
			ip.letStack = nil
			continue
		}

		break
	}

	return last, ip, nil
}

// EvalStr evaluates one or more Scheme expressions and returns the string
// representation of the last result.
func EvalStr(input string) (string, error) {
	last, _, err := evalInput(input)
	if err != nil {
		return "", err
	}
	if last == nil {
		return "", nil
	}
	return last.String(), nil
}

// EvalStrWithOutput evaluates Scheme expressions and returns both the result
// string and any captured output from display/write/newline.
func EvalStrWithOutput(input string) (result string, output string, err error) {
	last, ip, err := evalInput(input)
	if err != nil {
		return "", "", err
	}
	r := ""
	if last != nil {
		r = last.String()
	}
	return r, ip.output.String(), nil
}
