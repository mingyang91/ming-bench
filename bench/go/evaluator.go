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
	typeInt valueType = iota
	typeBool
	typeString
	typeSymbol
	typeVoid
	typePair
	typeNull
	typeLambda
	typeChar
	typeContinuation
)

// continuationEscape is used as a panic value to implement non-local exit.
type continuationEscape struct {
	val  value
	id   int           // identifies which continuation is being invoked
	cont *continuation // the continuation being invoked (for re-entry)
}

var contIDCounter int

// continuationResult is panicked when a saved continuation is invoked outside
// its dynamic extent. It carries the final result of the resumed computation.
type continuationResult struct {
	val value
	err error
}

// continuation represents a captured first-class continuation.
// Uses goroutine swap: the goroutine that called callCC blocks permanently
// on resumeCh. Each time the continuation is invoked, a value is sent on
// resumeCh, waking the goroutine to re-execute from the callCC return point.
type continuation struct {
	id       int
	resumeCh chan value
}

type pair struct {
	car, cdr value
}

type lambda struct {
	params []string
	rest   string // rest parameter name (empty if none)
	body   []*expr
	env    *env
}

type value struct {
	typ       valueType
	intVal    int64
	boolV     bool
	strVal    string
	charVal   rune
	pairVal   *pair
	lambdaVal *lambda
	mutableStr *[]rune // non-nil for mutable strings (string-copy)
	contVal   *continuation // for typeContinuation
}

var voidValue = value{typ: typeVoid}
var nullValue = value{typ: typeNull}

func intValue(n int64) value     { return value{typ: typeInt, intVal: n} }
func boolValue(b bool) value     { return value{typ: typeBool, boolV: b} }
func stringValue(s string) value { return value{typ: typeString, strVal: s} }
func symbolValue(s string) value { return value{typ: typeSymbol, strVal: s} }
func charValue(c rune) value     { return value{typ: typeChar, charVal: c} }

// getStr returns the current string content, respecting mutable backing.
func (v value) getStr() string {
	if v.mutableStr != nil {
		return string(*v.mutableStr)
	}
	return v.strVal
}
func pairValue(car, cdr value) value {
	return value{typ: typePair, pairVal: &pair{car: car, cdr: cdr}}
}

func (v value) String() string {
	switch v.typ {
	case typeInt:
		return strconv.FormatInt(v.intVal, 10)
	case typeBool:
		if v.boolV {
			return "#t"
		}
		return "#f"
	case typeString:
		return `"` + v.getStr() + `"`
	case typeSymbol:
		return v.strVal
	case typeVoid:
		return ""
	case typeNull:
		return "()"
	case typePair:
		return formatList(v)
	case typeLambda:
		return "#<procedure>"
	case typeContinuation:
		return "#<continuation>"
	case typeChar:
		switch v.charVal {
		case ' ':
			return `#\space`
		case '\n':
			return `#\newline`
		case '\t':
			return `#\tab`
		default:
			return `#\` + string(v.charVal)
		}
	}
	return ""
}

// displayString formats a value for display (no quotes on strings).
func displayString(v value) string {
	switch v.typ {
	case typeString:
		return v.getStr()
	case typeChar:
		return string(v.charVal)
	case typePair:
		return displayList(v)
	default:
		return v.String()
	}
}

func displayList(v value) string {
	var sb strings.Builder
	sb.WriteByte('(')
	first := true
	cur := v
	for cur.typ == typePair {
		if !first {
			sb.WriteByte(' ')
		}
		first = false
		sb.WriteString(displayString(cur.pairVal.car))
		cur = cur.pairVal.cdr
	}
	if cur.typ != typeNull {
		sb.WriteString(" . ")
		sb.WriteString(displayString(cur))
	}
	sb.WriteByte(')')
	return sb.String()
}

func formatList(v value) string {
	var sb strings.Builder
	sb.WriteByte('(')
	first := true
	cur := v
	for cur.typ == typePair {
		if !first {
			sb.WriteByte(' ')
		}
		first = false
		sb.WriteString(cur.pairVal.car.String())
		cur = cur.pairVal.cdr
	}
	if cur.typ != typeNull {
		sb.WriteString(" . ")
		sb.WriteString(cur.String())
	}
	sb.WriteByte(')')
	return sb.String()
}

func isTruthy(v value) bool {
	return !(v.typ == typeBool && !v.boolV)
}

// ---------- Tokenizer ----------

type token struct {
	kind string // "lparen", "rparen", "atom", "quote"
	text string
	line int
	col  int
}

func tokenize(input string) ([]token, error) {
	var tokens []token
	i := 0
	line := 1
	col := 1

	for i < len(input) {
		ch := input[i]

		// Skip whitespace
		if ch == ' ' || ch == '\t' || ch == '\n' || ch == '\r' {
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
			for i < len(input) && input[i] != '\n' {
				i++
			}
			continue
		}

		if ch == '\'' {
			tokens = append(tokens, token{kind: "quote", text: "'", line: line, col: col})
			i++
			col++
			continue
		}

		if ch == '(' {
			tokens = append(tokens, token{kind: "lparen", text: "(", line: line, col: col})
			i++
			col++
			continue
		}

		if ch == ')' {
			tokens = append(tokens, token{kind: "rparen", text: ")", line: line, col: col})
			i++
			col++
			continue
		}

		// String literal
		if ch == '"' {
			startCol := col
			startLine := line
			i++
			col++
			var sb strings.Builder
			for i < len(input) && input[i] != '"' {
				if input[i] == '\\' && i+1 < len(input) {
					i++
					col++
					switch input[i] {
					case 'n':
						sb.WriteByte('\n')
					case 't':
						sb.WriteByte('\t')
					case '"':
						sb.WriteByte('"')
					case '\\':
						sb.WriteByte('\\')
					default:
						sb.WriteByte(input[i])
					}
				} else {
					if input[i] == '\n' {
						line++
						col = 0
					}
					sb.WriteByte(input[i])
				}
				i++
				col++
			}
			if i >= len(input) {
				return nil, fmt.Errorf("%d:%d: unterminated string", startLine, startCol)
			}
			i++ // closing quote
			col++
			tokens = append(tokens, token{kind: "atom", text: `"` + sb.String() + `"`, line: startLine, col: startCol})
			continue
		}

		// Atom (number, symbol, boolean)
		startCol := col
		start := i
		for i < len(input) && input[i] != ' ' && input[i] != '\t' && input[i] != '\n' && input[i] != '\r' && input[i] != '(' && input[i] != ')' && input[i] != ';' && input[i] != '"' && input[i] != '\'' {
			i++
			col++
		}
		tokens = append(tokens, token{kind: "atom", text: input[start:i], line: line, col: startCol})
	}
	return tokens, nil
}

// ---------- Parser ----------

type expr struct {
	kind string // "atom", "list"
	atom value
	list []*expr
	line int
	col  int
}

func parse(tokens []token) ([]*expr, error) {
	pos := 0
	var results []*expr
	for pos < len(tokens) {
		e, newPos, err := parseExpr(tokens, pos)
		if err != nil {
			return nil, err
		}
		results = append(results, e)
		pos = newPos
	}
	return results, nil
}

func parseExpr(tokens []token, pos int) (*expr, int, error) {
	if pos >= len(tokens) {
		return nil, pos, fmt.Errorf("unexpected end of input")
	}
	tok := tokens[pos]

	if tok.kind == "quote" {
		// 'x => (quote x)
		inner, newPos, err := parseExpr(tokens, pos+1)
		if err != nil {
			return nil, 0, err
		}
		quoteExpr := &expr{
			kind: "list",
			list: []*expr{
				{kind: "atom", atom: symbolValue("quote"), line: tok.line, col: tok.col},
				inner,
			},
			line: tok.line,
			col:  tok.col,
		}
		return quoteExpr, newPos, nil
	}

	if tok.kind == "lparen" {
		var elems []*expr
		pos++
		for pos < len(tokens) && tokens[pos].kind != "rparen" {
			e, newPos, err := parseExpr(tokens, pos)
			if err != nil {
				return nil, 0, err
			}
			elems = append(elems, e)
			pos = newPos
		}
		if pos >= len(tokens) {
			return nil, 0, fmt.Errorf("%d:%d: unmatched parenthesis", tok.line, tok.col)
		}
		pos++ // skip rparen
		return &expr{kind: "list", list: elems, line: tok.line, col: tok.col}, pos, nil
	}

	if tok.kind == "rparen" {
		return nil, 0, fmt.Errorf("%d:%d: unexpected ')'", tok.line, tok.col)
	}

	// Atom
	v := parseAtom(tok.text)
	return &expr{kind: "atom", atom: v, line: tok.line, col: tok.col}, pos + 1, nil
}

func parseAtom(text string) value {
	if text == "#t" {
		return boolValue(true)
	}
	if text == "#f" {
		return boolValue(false)
	}
	if len(text) >= 2 && text[0] == '"' && text[len(text)-1] == '"' {
		return stringValue(text[1 : len(text)-1])
	}
	if strings.HasPrefix(text, `#\`) {
		name := text[2:]
		switch name {
		case "space":
			return charValue(' ')
		case "newline":
			return charValue('\n')
		case "tab":
			return charValue('\t')
		default:
			if len([]rune(name)) == 1 {
				return charValue([]rune(name)[0])
			}
		}
	}
	if n, err := strconv.ParseInt(text, 10, 64); err == nil {
		return intValue(n)
	}
	return symbolValue(text)
}

func isAllDigits(s string) bool {
	for _, c := range s {
		if !unicode.IsDigit(c) {
			return false
		}
	}
	return len(s) > 0
}

// ---------- Evaluator ----------

type env struct {
	bindings map[string]value
	parent   *env
	output   *strings.Builder // shared output buffer for display/write/newline
}

// evalResult holds the result of a top-level computation for continuation support.
type evalResult struct {
	val value
	err error
}

func newEnv(parent *env) *env {
	e := &env{bindings: make(map[string]value), parent: parent}
	if parent != nil {
		e.output = parent.output
	}
	return e
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

// setExisting mutates an existing binding, walking up the env chain.
// Returns false if the variable is not bound anywhere.
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

func evalExpr(e *expr, environ *env) (value, error) {
	for {
		if e.kind == "atom" {
			if e.atom.typ == typeSymbol {
				v, ok := environ.get(e.atom.strVal)
				if !ok {
					return value{}, fmt.Errorf("%d:%d: unbound variable: %s", e.line, e.col, e.atom.strVal)
				}
				return v, nil
			}
			return e.atom, nil
		}

		// List expression (function call or special form)
		if len(e.list) == 0 {
			return value{}, fmt.Errorf("%d:%d: empty application", e.line, e.col)
		}

		head := e.list[0]
		if head.kind == "atom" && head.atom.typ == typeSymbol {
			switch head.atom.strVal {
			case "and":
				// TCO: tail-call the last expression in and
				if len(e.list) == 1 {
					return boolValue(true), nil
				}
				for _, sub := range e.list[1 : len(e.list)-1] {
					v, err := evalExpr(sub, environ)
					if err != nil {
						return value{}, err
					}
					if !isTruthy(v) {
						return v, nil
					}
				}
				e = e.list[len(e.list)-1]
				continue

			case "or":
				// TCO: tail-call the last expression in or
				if len(e.list) == 1 {
					return boolValue(false), nil
				}
				for _, sub := range e.list[1 : len(e.list)-1] {
					v, err := evalExpr(sub, environ)
					if err != nil {
						return value{}, err
					}
					if isTruthy(v) {
						return v, nil
					}
				}
				e = e.list[len(e.list)-1]
				continue

			case "define":
				return evalDefine(e, environ)

			case "if":
				if len(e.list) < 3 || len(e.list) > 4 {
					return value{}, fmt.Errorf("%d:%d: if: bad syntax", head.line, head.col)
				}
				cond, err := evalExpr(e.list[1], environ)
				if err != nil {
					return value{}, err
				}
				if isTruthy(cond) {
					e = e.list[2]
					continue
				}
				if len(e.list) == 4 {
					e = e.list[3]
					continue
				}
				return voidValue, nil

			case "quote":
				if len(e.list) != 2 {
					return value{}, fmt.Errorf("%d:%d: quote: expected 1 argument", head.line, head.col)
				}
				return quoteExpr(e.list[1]), nil

			case "lambda":
				return evalLambda(e, environ)

			case "let":
				newE, newEnv, err := setupLet(e, environ)
				if err != nil {
					return value{}, err
				}
				e = newE
				environ = newEnv
				continue

			case "begin":
				if len(e.list) < 2 {
					return voidValue, nil
				}
				for _, body := range e.list[1 : len(e.list)-1] {
					_, err := evalExpr(body, environ)
					if err != nil {
						return value{}, err
					}
				}
				e = e.list[len(e.list)-1]
				continue

			case "set!":
				if len(e.list) != 3 {
					return value{}, fmt.Errorf("%d:%d: set!: bad syntax", head.line, head.col)
				}
				target := e.list[1]
				if target.kind != "atom" || target.atom.typ != typeSymbol {
					return value{}, fmt.Errorf("%d:%d: set!: expected symbol", target.line, target.col)
				}
				val, err := evalExpr(e.list[2], environ)
				if err != nil {
					return value{}, err
				}
				if !environ.setExisting(target.atom.strVal, val) {
					return value{}, fmt.Errorf("%d:%d: set!: unbound variable: %s", target.line, target.col, target.atom.strVal)
				}
				return voidValue, nil

			case "cond":
				found := false
				for _, clause := range e.list[1:] {
					if clause.kind != "list" || len(clause.list) < 2 {
						return value{}, fmt.Errorf("%d:%d: cond: bad clause", clause.line, clause.col)
					}
					isElse := clause.list[0].kind == "atom" && clause.list[0].atom.typ == typeSymbol && clause.list[0].atom.strVal == "else"
					if !isElse {
						test, err := evalExpr(clause.list[0], environ)
						if err != nil {
							return value{}, err
						}
						if !isTruthy(test) {
							continue
						}
					}
					// Evaluate all but last body, then tail-call last
					for _, body := range clause.list[1 : len(clause.list)-1] {
						_, err := evalExpr(body, environ)
						if err != nil {
							return value{}, err
						}
					}
					e = clause.list[len(clause.list)-1]
					found = true
					break
				}
				if !found {
					return voidValue, nil
				}
				continue
			}
		}

		// Evaluate the operator
		opVal, err := evalExpr(head, environ)
		if err != nil {
			return value{}, err
		}

		// Evaluate arguments
		args := make([]value, len(e.list)-1)
		for i, sub := range e.list[1:] {
			v, err := evalExpr(sub, environ)
			if err != nil {
				return value{}, err
			}
			args[i] = v
		}

		// Apply
		switch opVal.typ {
		case typeContinuation:
			if len(args) != 1 {
				return value{}, fmt.Errorf("%d:%d: continuation: expected 1 argument, got %d", e.line, e.col, len(args))
			}
			invokeContinuation(opVal.contVal, args[0])
		case typeLambda:
			lam := opVal.lambdaVal
			if lam.rest == "" {
				if len(args) != len(lam.params) {
					return value{}, fmt.Errorf("%d:%d: expected %d arguments, got %d", e.line, e.col, len(lam.params), len(args))
				}
			} else {
				if len(args) < len(lam.params) {
					return value{}, fmt.Errorf("%d:%d: expected at least %d arguments, got %d", e.line, e.col, len(lam.params), len(args))
				}
			}
			callEnv := newEnv(lam.env)
			for i, p := range lam.params {
				callEnv.set(p, args[i])
			}
			if lam.rest != "" {
				rest := nullValue
				for i := len(args) - 1; i >= len(lam.params); i-- {
					rest = pairValue(args[i], rest)
				}
				callEnv.set(lam.rest, rest)
			}
			// Evaluate all but last body, then tail-call last
			for _, body := range lam.body[:len(lam.body)-1] {
				_, err := evalExpr(body, callEnv)
				if err != nil {
					return value{}, err
				}
			}
			e = lam.body[len(lam.body)-1]
			environ = callEnv
			continue
		case typeSymbol:
			return applyBuiltin(opVal.strVal, args, e, environ)
		default:
			return value{}, fmt.Errorf("%d:%d: not a procedure: %s", head.line, head.col, opVal.String())
		}
	}
}

// setupLet prepares the let environment and returns the last body expression for TCO.
func setupLet(e *expr, environ *env) (*expr, *env, error) {
	if len(e.list) < 3 {
		return nil, nil, fmt.Errorf("%d:%d: let: bad syntax", e.list[0].line, e.list[0].col)
	}

	// Named let: (let name ((var init) ...) body ...)
	nameIdx := 1
	bodyStart := 2
	var namedLetName string
	if e.list[1].kind == "atom" && e.list[1].atom.typ == typeSymbol {
		if len(e.list) < 4 {
			return nil, nil, fmt.Errorf("%d:%d: let: bad syntax", e.list[0].line, e.list[0].col)
		}
		namedLetName = e.list[1].atom.strVal
		nameIdx = 2
		bodyStart = 3
	}

	bindings := e.list[nameIdx]
	if bindings.kind != "list" {
		return nil, nil, fmt.Errorf("%d:%d: let: expected bindings list", bindings.line, bindings.col)
	}

	params := make([]string, len(bindings.list))
	initVals := make([]value, len(bindings.list))
	for i, b := range bindings.list {
		if b.kind != "list" || len(b.list) != 2 {
			return nil, nil, fmt.Errorf("%d:%d: let: bad binding", b.line, b.col)
		}
		if b.list[0].kind != "atom" || b.list[0].atom.typ != typeSymbol {
			return nil, nil, fmt.Errorf("%d:%d: let: expected symbol", b.list[0].line, b.list[0].col)
		}
		params[i] = b.list[0].atom.strVal
		val, err := evalExpr(b.list[1], environ)
		if err != nil {
			return nil, nil, err
		}
		initVals[i] = val
	}

	bodyExprs := e.list[bodyStart:]

	if namedLetName != "" {
		letEnv := newEnv(environ)
		lam := &lambda{params: params, body: bodyExprs, env: letEnv}
		letEnv.set(namedLetName, value{typ: typeLambda, lambdaVal: lam})
		callEnv := newEnv(letEnv)
		for i, p := range params {
			callEnv.set(p, initVals[i])
		}
		// Evaluate all but last body
		for _, body := range bodyExprs[:len(bodyExprs)-1] {
			_, err := evalExpr(body, callEnv)
			if err != nil {
				return nil, nil, err
			}
		}
		return bodyExprs[len(bodyExprs)-1], callEnv, nil
	}

	letEnv := newEnv(environ)
	for i, p := range params {
		letEnv.set(p, initVals[i])
	}
	// Evaluate all but last body
	for _, body := range bodyExprs[:len(bodyExprs)-1] {
		_, err := evalExpr(body, letEnv)
		if err != nil {
			return nil, nil, err
		}
	}
	return bodyExprs[len(bodyExprs)-1], letEnv, nil
}

func evalDefine(e *expr, environ *env) (value, error) {
	if len(e.list) < 3 {
		return value{}, fmt.Errorf("%d:%d: define: bad syntax", e.list[0].line, e.list[0].col)
	}
	target := e.list[1]

	// (define (f params...) body...) or (define (f x . rest) body...)
	if target.kind == "list" {
		if len(target.list) == 0 {
			return value{}, fmt.Errorf("%d:%d: define: bad syntax", e.list[0].line, e.list[0].col)
		}
		name := target.list[0]
		if name.kind != "atom" || name.atom.typ != typeSymbol {
			return value{}, fmt.Errorf("%d:%d: define: expected symbol", name.line, name.col)
		}
		params, rest, err := parseDotParams(target.list[1:])
		if err != nil {
			return value{}, err
		}
		lam := &lambda{params: params, rest: rest, body: e.list[2:], env: environ}
		environ.set(name.atom.strVal, value{typ: typeLambda, lambdaVal: lam})
		return voidValue, nil
	}

	// (define x expr)
	if target.kind != "atom" || target.atom.typ != typeSymbol {
		return value{}, fmt.Errorf("%d:%d: define: expected symbol", target.line, target.col)
	}
	if len(e.list) != 3 {
		return value{}, fmt.Errorf("%d:%d: define: bad syntax", e.list[0].line, e.list[0].col)
	}
	val, err := evalExpr(e.list[2], environ)
	if err != nil {
		return value{}, err
	}
	environ.set(target.atom.strVal, val)
	return voidValue, nil
}


func evalLambda(e *expr, environ *env) (value, error) {
	if len(e.list) < 3 {
		return value{}, fmt.Errorf("%d:%d: lambda: bad syntax", e.list[0].line, e.list[0].col)
	}
	paramExpr := e.list[1]
	// (lambda args body) — single symbol captures all args
	if paramExpr.kind == "atom" && paramExpr.atom.typ == typeSymbol {
		lam := &lambda{rest: paramExpr.atom.strVal, body: e.list[2:], env: environ}
		return value{typ: typeLambda, lambdaVal: lam}, nil
	}
	if paramExpr.kind != "list" {
		return value{}, fmt.Errorf("%d:%d: lambda: expected parameter list", paramExpr.line, paramExpr.col)
	}
	params, rest, err := parseDotParams(paramExpr.list)
	if err != nil {
		return value{}, err
	}
	lam := &lambda{params: params, rest: rest, body: e.list[2:], env: environ}
	return value{typ: typeLambda, lambdaVal: lam}, nil
}

// parseDotParams extracts regular params and an optional rest param from a parameter list.
// E.g., [x, ., rest] => (["x"], "rest", nil)
func parseDotParams(paramList []*expr) ([]string, string, error) {
	dotIdx := -1
	for i, p := range paramList {
		if p.kind == "atom" && p.atom.typ == typeSymbol && p.atom.strVal == "." {
			dotIdx = i
			break
		}
	}
	if dotIdx == -1 {
		// No dot — all regular params
		params := make([]string, len(paramList))
		for i, p := range paramList {
			if p.kind != "atom" || p.atom.typ != typeSymbol {
				return nil, "", fmt.Errorf("%d:%d: expected symbol", p.line, p.col)
			}
			params[i] = p.atom.strVal
		}
		return params, "", nil
	}
	// Dot found — must have exactly one symbol after it
	if dotIdx+1 >= len(paramList) || dotIdx+2 != len(paramList) {
		return nil, "", fmt.Errorf("%d:%d: bad dot syntax in parameter list", paramList[dotIdx].line, paramList[dotIdx].col)
	}
	restExpr := paramList[dotIdx+1]
	if restExpr.kind != "atom" || restExpr.atom.typ != typeSymbol {
		return nil, "", fmt.Errorf("%d:%d: expected symbol after dot", restExpr.line, restExpr.col)
	}
	params := make([]string, dotIdx)
	for i := 0; i < dotIdx; i++ {
		p := paramList[i]
		if p.kind != "atom" || p.atom.typ != typeSymbol {
			return nil, "", fmt.Errorf("%d:%d: expected symbol", p.line, p.col)
		}
		params[i] = p.atom.strVal
	}
	return params, restExpr.atom.strVal, nil
}

func quoteExpr(e *expr) value {
	if e.kind == "atom" {
		return e.atom
	}
	// List => build a proper list from elements
	result := nullValue
	for i := len(e.list) - 1; i >= 0; i-- {
		result = pairValue(quoteExpr(e.list[i]), result)
	}
	return result
}


// callCC implements call/cc with full continuation support using goroutine swap.
//
// When callCC is entered, the CALLING goroutine is permanently blocked on resumeCh.
// A body goroutine runs (f k). When the body returns or k is invoked, a value is
// sent on resumeCh. The blocked goroutine wakes up and returns from callCC.
//
// For re-invocation: when k is invoked again later, the invoking goroutine sends
// on resumeCh and terminates itself. The original goroutine (which is blocked in
// a loop inside callCC) wakes up, returns the new value, and the computation
// re-executes from the callCC return point.
//
// The original goroutine LOOPS: after returning from callCC and executing the rest
// of the computation, it comes back to wait on resumeCh. This is achieved by
// returning through a restartable mechanism. See invokeContinuation.
func callCC(fn value, e *expr, environ *env, head *expr, name string) (value, error) {
	contIDCounter++
	myID := contIDCounter

	resumeCh := make(chan value, 1)
	cont := &continuation{
		id:       myID,
		resumeCh: resumeCh,
	}
	contVal := value{typ: typeContinuation, contVal: cont}

	// Spawn body goroutine to evaluate (f k)
	go func() {
		defer func() {
			if r := recover(); r != nil {
				if esc, ok := r.(continuationEscape); ok && esc.id == myID {
					resumeCh <- esc.val
					return
				}
				// For other panics, we need to propagate them. Store in a side channel.
				panic(r)
			}
		}()
		switch fn.typ {
		case typeLambda:
			v, err := applyLambda(fn.lambdaVal, []value{contVal}, e, environ)
			if err != nil {
				// Propagate error — TODO: need error channel
				panic(err)
			}
			resumeCh <- v
		case typeContinuation:
			panic(continuationEscape{val: contVal, id: fn.contVal.id, cont: fn.contVal})
		default:
			panic(fmt.Errorf("%d:%d: %s: expected procedure", head.line, head.col, name))
		}
	}()

	// Block waiting for body result or k invocation
	v := <-resumeCh
	return v, nil
}

// invokeContinuation invokes a continuation with a value.
// First tries escape (panic within dynamic extent), then falls back to
// invoke function for saved continuations.
func invokeContinuation(cont *continuation, val value) {
	// Try escape first — if we're inside the callCC's dynamic extent,
	// the panic will be caught by callCC's recover
	panic(continuationEscape{val: val, id: cont.id, cont: cont})
}

// Never returns — the panic above always fires.
// But if invoke is set, we could use it. However, escape should work for both cases
// because callCC ALWAYS has a recover handler for its own id.

func applyLambda(lam *lambda, args []value, e *expr, environ *env) (value, error) {
	if lam.rest == "" {
		if len(args) != len(lam.params) {
			return value{}, fmt.Errorf("%d:%d: expected %d arguments, got %d", e.line, e.col, len(lam.params), len(args))
		}
	} else {
		if len(args) < len(lam.params) {
			return value{}, fmt.Errorf("%d:%d: expected at least %d arguments, got %d", e.line, e.col, len(lam.params), len(args))
		}
	}
	callEnv := newEnv(lam.env)
	for i, p := range lam.params {
		callEnv.set(p, args[i])
	}
	if lam.rest != "" {
		rest := nullValue
		for i := len(args) - 1; i >= len(lam.params); i-- {
			rest = pairValue(args[i], rest)
		}
		callEnv.set(lam.rest, rest)
	}
	for _, body := range lam.body[:len(lam.body)-1] {
		_, err := evalExpr(body, callEnv)
		if err != nil {
			return value{}, err
		}
	}
	return evalExpr(lam.body[len(lam.body)-1], callEnv)
}

func applyBuiltin(name string, args []value, e *expr, environ *env) (value, error) {
	head := e.list[0]
	switch name {
	case "+":
		sum := int64(0)
		for i, a := range args {
			if a.typ != typeInt {
				return value{}, fmt.Errorf("%d:%d: +: expected number, got %s", e.list[i+1].line, e.list[i+1].col, a.String())
			}
			sum += a.intVal
		}
		return intValue(sum), nil

	case "-":
		if len(args) == 0 {
			return value{}, fmt.Errorf("%d:%d: -: expected at least 1 argument", head.line, head.col)
		}
		if args[0].typ != typeInt {
			return value{}, fmt.Errorf("%d:%d: -: expected number", e.list[1].line, e.list[1].col)
		}
		if len(args) == 1 {
			return intValue(-args[0].intVal), nil
		}
		result := args[0].intVal
		for i, a := range args[1:] {
			if a.typ != typeInt {
				return value{}, fmt.Errorf("%d:%d: -: expected number", e.list[i+2].line, e.list[i+2].col)
			}
			result -= a.intVal
		}
		return intValue(result), nil

	case "*":
		product := int64(1)
		for i, a := range args {
			if a.typ != typeInt {
				return value{}, fmt.Errorf("%d:%d: *: expected number", e.list[i+1].line, e.list[i+1].col)
			}
			product *= a.intVal
		}
		return intValue(product), nil

	case "/":
		if len(args) < 2 {
			return value{}, fmt.Errorf("%d:%d: /: expected at least 2 arguments", head.line, head.col)
		}
		if args[0].typ != typeInt {
			return value{}, fmt.Errorf("%d:%d: /: expected number", e.list[1].line, e.list[1].col)
		}
		result := args[0].intVal
		for i, a := range args[1:] {
			if a.typ != typeInt {
				return value{}, fmt.Errorf("%d:%d: /: expected number", e.list[i+2].line, e.list[i+2].col)
			}
			if a.intVal == 0 {
				return value{}, fmt.Errorf("%d:%d: /: division by zero", e.list[i+2].line, e.list[i+2].col)
			}
			result /= a.intVal
		}
		return intValue(result), nil

	case "<":
		return compareInts(args, e, func(a, b int64) bool { return a < b }, "<")
	case ">":
		return compareInts(args, e, func(a, b int64) bool { return a > b }, ">")
	case "=":
		return compareInts(args, e, func(a, b int64) bool { return a == b }, "=")
	case "<=":
		return compareInts(args, e, func(a, b int64) bool { return a <= b }, "<=")
	case ">=":
		return compareInts(args, e, func(a, b int64) bool { return a >= b }, ">=")

	case "not":
		if len(args) != 1 {
			return value{}, fmt.Errorf("%d:%d: not: expected 1 argument, got %d", head.line, head.col, len(args))
		}
		return boolValue(!isTruthy(args[0])), nil

	case "cons":
		if len(args) != 2 {
			return value{}, fmt.Errorf("%d:%d: cons: expected 2 arguments, got %d", head.line, head.col, len(args))
		}
		return pairValue(args[0], args[1]), nil

	case "car":
		if len(args) != 1 {
			return value{}, fmt.Errorf("%d:%d: car: expected 1 argument, got %d", head.line, head.col, len(args))
		}
		if args[0].typ != typePair {
			return value{}, fmt.Errorf("%d:%d: car: expected pair, got %s", head.line, head.col, args[0].String())
		}
		return args[0].pairVal.car, nil

	case "cdr":
		if len(args) != 1 {
			return value{}, fmt.Errorf("%d:%d: cdr: expected 1 argument, got %d", head.line, head.col, len(args))
		}
		if args[0].typ != typePair {
			return value{}, fmt.Errorf("%d:%d: cdr: expected pair, got %s", head.line, head.col, args[0].String())
		}
		return args[0].pairVal.cdr, nil

	case "null?":
		if len(args) != 1 {
			return value{}, fmt.Errorf("%d:%d: null?: expected 1 argument, got %d", head.line, head.col, len(args))
		}
		return boolValue(args[0].typ == typeNull), nil

	case "list":
		result := nullValue
		for i := len(args) - 1; i >= 0; i-- {
			result = pairValue(args[i], result)
		}
		return result, nil

	case "length":
		if len(args) != 1 {
			return value{}, fmt.Errorf("%d:%d: length: expected 1 argument, got %d", head.line, head.col, len(args))
		}
		count := int64(0)
		cur := args[0]
		for cur.typ == typePair {
			count++
			cur = cur.pairVal.cdr
		}
		if cur.typ != typeNull {
			return value{}, fmt.Errorf("%d:%d: length: expected proper list", head.line, head.col)
		}
		return intValue(count), nil

	case "string?":
		if len(args) != 1 {
			return value{}, fmt.Errorf("%d:%d: string?: expected 1 argument, got %d", head.line, head.col, len(args))
		}
		return boolValue(args[0].typ == typeString), nil

	case "number?":
		if len(args) != 1 {
			return value{}, fmt.Errorf("%d:%d: number?: expected 1 argument, got %d", head.line, head.col, len(args))
		}
		return boolValue(args[0].typ == typeInt), nil

	case "boolean?":
		if len(args) != 1 {
			return value{}, fmt.Errorf("%d:%d: boolean?: expected 1 argument, got %d", head.line, head.col, len(args))
		}
		return boolValue(args[0].typ == typeBool), nil

	case "pair?":
		if len(args) != 1 {
			return value{}, fmt.Errorf("%d:%d: pair?: expected 1 argument, got %d", head.line, head.col, len(args))
		}
		return boolValue(args[0].typ == typePair), nil

	case "symbol?":
		if len(args) != 1 {
			return value{}, fmt.Errorf("%d:%d: symbol?: expected 1 argument, got %d", head.line, head.col, len(args))
		}
		return boolValue(args[0].typ == typeSymbol), nil

	case "append":
		if len(args) == 0 {
			return nullValue, nil
		}
		if len(args) == 1 {
			return args[0], nil
		}
		// Append all lists together
		result := args[len(args)-1]
		for i := len(args) - 2; i >= 0; i-- {
			result = appendList(args[i], result)
		}
		return result, nil

	case "display":
		if len(args) != 1 {
			return value{}, fmt.Errorf("%d:%d: display: expected 1 argument, got %d", head.line, head.col, len(args))
		}
		if out := environ.getOutput(); out != nil {
			out.WriteString(displayString(args[0]))
		}
		return voidValue, nil

	case "write":
		if len(args) != 1 {
			return value{}, fmt.Errorf("%d:%d: write: expected 1 argument, got %d", head.line, head.col, len(args))
		}
		if out := environ.getOutput(); out != nil {
			out.WriteString(args[0].String())
		}
		return voidValue, nil

	case "newline":
		if len(args) != 0 {
			return value{}, fmt.Errorf("%d:%d: newline: expected 0 arguments, got %d", head.line, head.col, len(args))
		}
		if out := environ.getOutput(); out != nil {
			out.WriteByte('\n')
		}
		return voidValue, nil

	case "string-append":
		var sb strings.Builder
		for i, a := range args {
			if a.typ != typeString {
				return value{}, fmt.Errorf("%d:%d: string-append: expected string", e.list[i+1].line, e.list[i+1].col)
			}
			sb.WriteString(a.getStr())
		}
		return stringValue(sb.String()), nil

	case "string-length":
		if len(args) != 1 {
			return value{}, fmt.Errorf("%d:%d: string-length: expected 1 argument, got %d", head.line, head.col, len(args))
		}
		if args[0].typ != typeString {
			return value{}, fmt.Errorf("%d:%d: string-length: expected string", head.line, head.col)
		}
		return intValue(int64(len([]rune(args[0].getStr())))), nil

	case "substring":
		if len(args) != 3 {
			return value{}, fmt.Errorf("%d:%d: substring: expected 3 arguments, got %d", head.line, head.col, len(args))
		}
		if args[0].typ != typeString {
			return value{}, fmt.Errorf("%d:%d: substring: expected string", head.line, head.col)
		}
		if args[1].typ != typeInt || args[2].typ != typeInt {
			return value{}, fmt.Errorf("%d:%d: substring: expected integer indices", head.line, head.col)
		}
		runes := []rune(args[0].getStr())
		start := int(args[1].intVal)
		end := int(args[2].intVal)
		if start < 0 || end < start || end > len(runes) {
			return value{}, fmt.Errorf("%d:%d: substring: index out of range", head.line, head.col)
		}
		return stringValue(string(runes[start:end])), nil

	case "string->number":
		if len(args) != 1 {
			return value{}, fmt.Errorf("%d:%d: string->number: expected 1 argument, got %d", head.line, head.col, len(args))
		}
		if args[0].typ != typeString {
			return value{}, fmt.Errorf("%d:%d: string->number: expected string", head.line, head.col)
		}
		n, err := strconv.ParseInt(args[0].getStr(), 10, 64)
		if err != nil {
			return boolValue(false), nil
		}
		return intValue(n), nil

	case "number->string":
		if len(args) != 1 {
			return value{}, fmt.Errorf("%d:%d: number->string: expected 1 argument, got %d", head.line, head.col, len(args))
		}
		if args[0].typ != typeInt {
			return value{}, fmt.Errorf("%d:%d: number->string: expected number", head.line, head.col)
		}
		return stringValue(strconv.FormatInt(args[0].intVal, 10)), nil

	case "symbol->string":
		if len(args) != 1 {
			return value{}, fmt.Errorf("%d:%d: symbol->string: expected 1 argument, got %d", head.line, head.col, len(args))
		}
		if args[0].typ != typeSymbol {
			return value{}, fmt.Errorf("%d:%d: symbol->string: expected symbol", head.line, head.col)
		}
		return stringValue(args[0].strVal), nil

	case "string->symbol":
		if len(args) != 1 {
			return value{}, fmt.Errorf("%d:%d: string->symbol: expected 1 argument, got %d", head.line, head.col, len(args))
		}
		if args[0].typ != typeString {
			return value{}, fmt.Errorf("%d:%d: string->symbol: expected string", head.line, head.col)
		}
		return symbolValue(args[0].getStr()), nil

	case "string-ref":
		if len(args) != 2 {
			return value{}, fmt.Errorf("%d:%d: string-ref: expected 2 arguments, got %d", head.line, head.col, len(args))
		}
		if args[0].typ != typeString {
			return value{}, fmt.Errorf("%d:%d: string-ref: expected string", head.line, head.col)
		}
		if args[1].typ != typeInt {
			return value{}, fmt.Errorf("%d:%d: string-ref: expected integer index", head.line, head.col)
		}
		runes := []rune(args[0].getStr())
		idx := int(args[1].intVal)
		if idx < 0 || idx >= len(runes) {
			return value{}, fmt.Errorf("%d:%d: string-ref: index out of range", head.line, head.col)
		}
		return charValue(runes[idx]), nil

	case "string-copy":
		if len(args) != 1 {
			return value{}, fmt.Errorf("%d:%d: string-copy: expected 1 argument, got %d", head.line, head.col, len(args))
		}
		if args[0].typ != typeString {
			return value{}, fmt.Errorf("%d:%d: string-copy: expected string", head.line, head.col)
		}
		runes := []rune(args[0].getStr())
		cp := make([]rune, len(runes))
		copy(cp, runes)
		return value{typ: typeString, mutableStr: &cp}, nil

	case "string-set!":
		if len(args) != 3 {
			return value{}, fmt.Errorf("%d:%d: string-set!: expected 3 arguments, got %d", head.line, head.col, len(args))
		}
		if args[0].typ != typeString {
			return value{}, fmt.Errorf("%d:%d: string-set!: expected string", head.line, head.col)
		}
		if args[0].mutableStr == nil {
			return value{}, fmt.Errorf("%d:%d: string-set!: string is immutable", head.line, head.col)
		}
		if args[1].typ != typeInt {
			return value{}, fmt.Errorf("%d:%d: string-set!: expected integer index", head.line, head.col)
		}
		if args[2].typ != typeChar {
			return value{}, fmt.Errorf("%d:%d: string-set!: expected character", head.line, head.col)
		}
		idx := int(args[1].intVal)
		if idx < 0 || idx >= len(*args[0].mutableStr) {
			return value{}, fmt.Errorf("%d:%d: string-set!: index out of range", head.line, head.col)
		}
		(*args[0].mutableStr)[idx] = args[2].charVal
		return voidValue, nil

	case "char?":
		if len(args) != 1 {
			return value{}, fmt.Errorf("%d:%d: char?: expected 1 argument, got %d", head.line, head.col, len(args))
		}
		return boolValue(args[0].typ == typeChar), nil

	case "call/cc", "call-with-current-continuation":
		if len(args) != 1 {
			return value{}, fmt.Errorf("%d:%d: %s: expected 1 argument, got %d", head.line, head.col, name, len(args))
		}
		fn := args[0]
		return callCC(fn, e, environ, head, name)

	case "apply":
		if len(args) < 2 {
			return value{}, fmt.Errorf("%d:%d: apply: expected at least 2 arguments", head.line, head.col)
		}
		fn := args[0]
		// Last arg must be a list; prefix args are prepended
		lastArg := args[len(args)-1]
		var applyArgs []value
		for _, a := range args[1 : len(args)-1] {
			applyArgs = append(applyArgs, a)
		}
		// Unpack the trailing list
		cur := lastArg
		for cur.typ == typePair {
			applyArgs = append(applyArgs, cur.pairVal.car)
			cur = cur.pairVal.cdr
		}
		// Apply the function
		switch fn.typ {
		case typeContinuation:
			if len(applyArgs) != 1 {
				return value{}, fmt.Errorf("%d:%d: apply: continuation expects 1 argument, got %d", head.line, head.col, len(applyArgs))
			}
			invokeContinuation(fn.contVal, applyArgs[0])
		case typeLambda:
			lam := fn.lambdaVal
			if lam.rest == "" {
				if len(applyArgs) != len(lam.params) {
					return value{}, fmt.Errorf("%d:%d: apply: expected %d arguments, got %d", head.line, head.col, len(lam.params), len(applyArgs))
				}
			} else {
				if len(applyArgs) < len(lam.params) {
					return value{}, fmt.Errorf("%d:%d: apply: expected at least %d arguments, got %d", head.line, head.col, len(lam.params), len(applyArgs))
				}
			}
			callEnv := newEnv(lam.env)
			for i, p := range lam.params {
				callEnv.set(p, applyArgs[i])
			}
			if lam.rest != "" {
				rest := nullValue
				for i := len(applyArgs) - 1; i >= len(lam.params); i-- {
					rest = pairValue(applyArgs[i], rest)
				}
				callEnv.set(lam.rest, rest)
			}
			for _, body := range lam.body[:len(lam.body)-1] {
				_, err := evalExpr(body, callEnv)
				if err != nil {
					return value{}, err
				}
			}
			return evalExpr(lam.body[len(lam.body)-1], callEnv)
		case typeSymbol:
			return applyBuiltin(fn.strVal, applyArgs, e, environ)
		default:
			return value{}, fmt.Errorf("%d:%d: apply: not a procedure", head.line, head.col)
		}
	}

	return value{}, fmt.Errorf("%d:%d: unbound variable: %s", head.line, head.col, name)
}

func compareInts(args []value, e *expr, cmp func(int64, int64) bool, name string) (value, error) {
	if len(args) < 2 {
		return value{}, fmt.Errorf("%d:%d: %s: expected at least 2 arguments", e.list[0].line, e.list[0].col, name)
	}
	for i, a := range args {
		if a.typ != typeInt {
			return value{}, fmt.Errorf("%d:%d: %s: expected number", e.list[i+1].line, e.list[i+1].col, name)
		}
	}
	for i := 0; i < len(args)-1; i++ {
		if !cmp(args[i].intVal, args[i+1].intVal) {
			return boolValue(false), nil
		}
	}
	return boolValue(true), nil
}

func appendList(lst value, tail value) value {
	if lst.typ == typeNull {
		return tail
	}
	if lst.typ != typePair {
		return tail
	}
	return pairValue(lst.pairVal.car, appendList(lst.pairVal.cdr, tail))
}

// ---------- Public API ----------

// EvalStr evaluates one or more Scheme expressions and returns the string
// representation of the last result.
func EvalStr(input string) (string, error) {
	tokens, err := tokenize(input)
	if err != nil {
		return "", &EvalError{Message: err.Error()}
	}
	exprs, err := parse(tokens)
	if err != nil {
		return "", &EvalError{Message: err.Error()}
	}
	if len(exprs) == 0 {
		return "", &EvalError{Message: "no expressions"}
	}

	environ := newEnv(nil)
	initBuiltins(environ)

	last, err := evalProgram(exprs, environ)
	if err != nil {
		return "", &EvalError{Message: err.Error()}
	}

	if last.typ == typeVoid {
		return "", nil
	}
	return last.String(), nil
}

// EvalStrWithOutput evaluates Scheme expressions and returns both the result
// string and any captured output from display/write/newline.
func EvalStrWithOutput(input string) (result string, output string, err error) {
	tokens, err := tokenize(input)
	if err != nil {
		return "", "", &EvalError{Message: err.Error()}
	}
	exprs, err := parse(tokens)
	if err != nil {
		return "", "", &EvalError{Message: err.Error()}
	}
	if len(exprs) == 0 {
		return "", "", &EvalError{Message: "no expressions"}
	}

	var outBuf strings.Builder
	environ := newEnv(nil)
	environ.output = &outBuf
	initBuiltins(environ)

	last, evalErr := evalProgram(exprs, environ)
	if evalErr != nil {
		return "", "", &EvalError{Message: evalErr.Error()}
	}

	resultStr := ""
	if last.typ != typeVoid {
		resultStr = last.String()
	}
	return resultStr, outBuf.String(), nil
}

var builtinNames = []string{"+", "-", "*", "/", "<", ">", "=", "<=", ">=", "not", "cons", "car", "cdr", "null?", "list", "length", "append", "string?", "number?", "boolean?", "pair?", "symbol?", "display", "write", "newline", "string-append", "string-length", "substring", "string->number", "number->string", "symbol->string", "string->symbol", "string-ref", "char?", "string-copy", "string-set!", "apply", "call/cc", "call-with-current-continuation"}

func initBuiltins(environ *env) {
	for _, name := range builtinNames {
		environ.set(name, symbolValue(name))
	}
}

// evalProgram evaluates a sequence of top-level expressions.
// Catches continuationEscape for saved continuations invoked outside their
// dynamic extent, and uses their invoke function to replay the computation.
func evalProgram(exprs []*expr, environ *env) (last value, retErr error) {
	defer func() {
		if r := recover(); r != nil {
			if esc, ok := r.(continuationEscape); ok {
				// A saved continuation was invoked outside its dynamic extent.
				// Use invoke to replay the computation.
				if esc.cont != nil && esc.cont.invoke != nil {
					last, retErr = esc.cont.invoke(esc.val)
					return
				}
			}
			if cr, ok := r.(continuationResult); ok {
				last = cr.val
				retErr = cr.err
				return
			}
			panic(r)
		}
	}()
	for _, e := range exprs {
		v, err := evalExpr(e, environ)
		if err != nil {
			return value{}, err
		}
		last = v
	}
	return last, nil
}

