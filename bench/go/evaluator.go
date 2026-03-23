package ming

import (
	"fmt"
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
)

type value struct {
	typ    valueType
	ival   int64
	bval   bool
	sval   string
	car    *value
	cdr    *value
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
	}
	return ""
}

func printList(v *value) string {
	var buf strings.Builder
	buf.WriteByte('(')
	cur := v
	first := true
	for cur.typ == valPair {
		if !first {
			buf.WriteByte(' ')
		}
		buf.WriteString(cur.car.String())
		first = false
		cur = cur.cdr
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

		// Quote shorthand
		if ch == '\'' {
			tokens = append(tokens, token{"'", line, col})
			i++
			col++
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
	kind   string // "int", "bool", "string", "symbol", "list"
	ival   int64
	bval   bool
	sval   string
	items  []*expr
	line   int
	col    int
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
		pos++
		for pos < len(tokens) && tokens[pos].text != ")" {
			e, newpos, err := parseExpr(tokens, pos)
			if err != nil {
				return nil, 0, err
			}
			items = append(items, e)
			pos = newpos
		}
		if pos >= len(tokens) {
			return nil, 0, &EvalError{Message: fmt.Sprintf("%d:%d: unterminated list", tok.line, tok.col)}
		}
		pos++ // skip ")"
		return &expr{kind: "list", items: items, line: tok.line, col: tok.col}, pos, nil
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

	// String literal
	if len(tok.text) >= 2 && tok.text[0] == '"' && tok.text[len(tok.text)-1] == '"' {
		return &expr{kind: "string", sval: tok.text[1 : len(tok.text)-1], line: tok.line, col: tok.col}, pos + 1, nil
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

	// Symbol
	return &expr{kind: "symbol", sval: tok.text, line: tok.line, col: tok.col}, pos + 1, nil
}

// ---------- Environment ----------

type env struct {
	bindings map[string]*value
	parent   *env
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

// ---------- Evaluator ----------

func eval(e *expr, env *env) (*value, error) {
	switch e.kind {
	case "int":
		return intVal(e.ival), nil
	case "bool":
		return boolVal(e.bval), nil
	case "string":
		return strVal(e.sval), nil
	case "symbol":
		v, ok := env.get(e.sval)
		if !ok {
			return nil, &EvalError{Message: fmt.Sprintf("%d:%d: unbound variable: %s", e.line, e.col, e.sval)}
		}
		return v, nil
	case "list":
		if len(e.items) == 0 {
			return nil, &EvalError{Message: fmt.Sprintf("%d:%d: empty application", e.line, e.col)}
		}
		head := e.items[0]

		// Special forms
		if head.kind == "symbol" {
			switch head.sval {
			case "and":
				return evalAnd(e.items[1:], env)
			case "or":
				return evalOr(e.items[1:], env)
			}
		}

		// Function call
		fn, err := eval(head, env)
		if err != nil {
			return nil, err
		}
		_ = fn

		// Evaluate arguments
		args := make([]*value, len(e.items)-1)
		for i, arg := range e.items[1:] {
			v, err := eval(arg, env)
			if err != nil {
				return nil, err
			}
			args[i] = v
		}

		return applyBuiltin(head, fn, args, e)
	}
	return nil, &EvalError{Message: fmt.Sprintf("%d:%d: unknown expression type", e.line, e.col)}
}

func evalAnd(exprs []*expr, env *env) (*value, error) {
	if len(exprs) == 0 {
		return boolVal(true), nil
	}
	var result *value
	for _, e := range exprs {
		v, err := eval(e, env)
		if err != nil {
			return nil, err
		}
		result = v
		if !v.isTruthy() {
			return v, nil
		}
	}
	return result, nil
}

func evalOr(exprs []*expr, env *env) (*value, error) {
	if len(exprs) == 0 {
		return boolVal(false), nil
	}
	for _, e := range exprs {
		v, err := eval(e, env)
		if err != nil {
			return nil, err
		}
		if v.isTruthy() {
			return v, nil
		}
	}
	return boolVal(false), nil
}

func applyBuiltin(head *expr, fn *value, args []*value, callExpr *expr) (*value, error) {
	if fn.typ != valSymbol {
		return nil, &EvalError{Message: fmt.Sprintf("%d:%d: not a procedure", callExpr.line, callExpr.col)}
	}

	name := fn.sval
	line, col := callExpr.line, callExpr.col

	switch name {
	case "+":
		sum := int64(0)
		for _, a := range args {
			if a.typ != valInt {
				return nil, &EvalError{Message: fmt.Sprintf("%d:%d: +: expected number", line, col)}
			}
			sum += a.ival
		}
		return intVal(sum), nil

	case "-":
		if len(args) == 0 {
			return nil, &EvalError{Message: fmt.Sprintf("%d:%d: -: expected at least 1 argument", line, col)}
		}
		if args[0].typ != valInt {
			return nil, &EvalError{Message: fmt.Sprintf("%d:%d: -: expected number", line, col)}
		}
		if len(args) == 1 {
			return intVal(-args[0].ival), nil
		}
		result := args[0].ival
		for _, a := range args[1:] {
			if a.typ != valInt {
				return nil, &EvalError{Message: fmt.Sprintf("%d:%d: -: expected number", line, col)}
			}
			result -= a.ival
		}
		return intVal(result), nil

	case "*":
		product := int64(1)
		for _, a := range args {
			if a.typ != valInt {
				return nil, &EvalError{Message: fmt.Sprintf("%d:%d: *: expected number", line, col)}
			}
			product *= a.ival
		}
		return intVal(product), nil

	case "/":
		if len(args) < 2 {
			return nil, &EvalError{Message: fmt.Sprintf("%d:%d: /: expected at least 2 arguments", line, col)}
		}
		if args[0].typ != valInt {
			return nil, &EvalError{Message: fmt.Sprintf("%d:%d: /: expected number", line, col)}
		}
		result := args[0].ival
		for _, a := range args[1:] {
			if a.typ != valInt {
				return nil, &EvalError{Message: fmt.Sprintf("%d:%d: /: expected number", line, col)}
			}
			if a.ival == 0 {
				return nil, &EvalError{Message: fmt.Sprintf("%d:%d: /: division by zero", line, col)}
			}
			result /= a.ival
		}
		return intVal(result), nil

	case "<":
		return compareInts(args, func(a, b int64) bool { return a < b }, "<", line, col)
	case ">":
		return compareInts(args, func(a, b int64) bool { return a > b }, ">", line, col)
	case "=":
		return compareInts(args, func(a, b int64) bool { return a == b }, "=", line, col)
	case "<=":
		return compareInts(args, func(a, b int64) bool { return a <= b }, "<=", line, col)
	case ">=":
		return compareInts(args, func(a, b int64) bool { return a >= b }, ">=", line, col)

	case "not":
		if len(args) != 1 {
			return nil, &EvalError{Message: fmt.Sprintf("%d:%d: not: expected 1 argument", line, col)}
		}
		return boolVal(!args[0].isTruthy()), nil
	}

	return nil, &EvalError{Message: fmt.Sprintf("%d:%d: unbound variable: %s", line, col, name)}
}

func compareInts(args []*value, op func(int64, int64) bool, name string, line, col int) (*value, error) {
	if len(args) < 2 {
		return nil, &EvalError{Message: fmt.Sprintf("%d:%d: %s: expected at least 2 arguments", line, col, name)}
	}
	for i := 0; i < len(args)-1; i++ {
		if args[i].typ != valInt || args[i+1].typ != valInt {
			return nil, &EvalError{Message: fmt.Sprintf("%d:%d: %s: expected number", line, col, name)}
		}
		if !op(args[i].ival, args[i+1].ival) {
			return boolVal(false), nil
		}
	}
	return boolVal(true), nil
}

// ---------- Top-level ----------

func makeGlobalEnv() *env {
	e := newEnv(nil)
	// Builtins are stored as symbol values that match their name
	builtins := []string{"+", "-", "*", "/", "<", ">", "=", "<=", ">=", "not"}
	for _, b := range builtins {
		e.set(b, symVal(b))
	}
	return e
}

// EvalStr evaluates one or more Scheme expressions and returns the string
// representation of the last result.
func EvalStr(input string) (string, error) {
	tokens := tokenize(input)
	exprs, err := parse(tokens)
	if err != nil {
		return "", err
	}
	if len(exprs) == 0 {
		return "", nil
	}

	env := makeGlobalEnv()
	var last *value
	for _, e := range exprs {
		v, err := eval(e, env)
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
