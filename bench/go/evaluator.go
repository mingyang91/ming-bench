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
)

type value struct {
	typ    valueType
	intVal int64
	boolV  bool
	strVal string
}

var voidValue = value{typ: typeVoid}

func intValue(n int64) value   { return value{typ: typeInt, intVal: n} }
func boolValue(b bool) value   { return value{typ: typeBool, boolV: b} }
func stringValue(s string) value { return value{typ: typeString, strVal: s} }
func symbolValue(s string) value { return value{typ: typeSymbol, strVal: s} }

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
		return `"` + v.strVal + `"`
	case typeSymbol:
		return v.strVal
	case typeVoid:
		return ""
	}
	return ""
}

func isTruthy(v value) bool {
	return !(v.typ == typeBool && !v.boolV)
}

// ---------- Tokenizer ----------

type token struct {
	kind string // "lparen", "rparen", "atom"
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
		for i < len(input) && input[i] != ' ' && input[i] != '\t' && input[i] != '\n' && input[i] != '\r' && input[i] != '(' && input[i] != ')' && input[i] != ';' && input[i] != '"' {
			i++
			col++
		}
		tokens = append(tokens, token{kind: "atom", text: input[start:i], line: line, col: startCol})
	}
	return tokens, nil
}

// ---------- Parser ----------

type expr struct {
	kind     string // "atom", "list"
	atom     value
	list     []*expr
	line     int
	col      int
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
	if n, err := strconv.ParseInt(text, 10, 64); err == nil {
		return intValue(n)
	}
	// Check for negative numbers with leading -
	if len(text) > 1 && text[0] == '-' && isAllDigits(text[1:]) {
		if n, err := strconv.ParseInt(text, 10, 64); err == nil {
			return intValue(n)
		}
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

func evalExpr(e *expr, environ *env) (value, error) {
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
			return evalAnd(e, environ)
		case "or":
			return evalOr(e, environ)
		}
	}

	// Evaluate all elements
	vals := make([]value, len(e.list))
	for i, sub := range e.list {
		v, err := evalExpr(sub, environ)
		if err != nil {
			return value{}, err
		}
		vals[i] = v
	}

	op := vals[0]
	args := vals[1:]

	if op.typ != typeSymbol {
		return value{}, fmt.Errorf("%d:%d: not a procedure: %s", head.line, head.col, op.String())
	}

	return applyBuiltin(op.strVal, args, e)
}

func evalAnd(e *expr, environ *env) (value, error) {
	if len(e.list) == 1 {
		return boolValue(true), nil
	}
	var result value
	for _, sub := range e.list[1:] {
		v, err := evalExpr(sub, environ)
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

func evalOr(e *expr, environ *env) (value, error) {
	if len(e.list) == 1 {
		return boolValue(false), nil
	}
	for _, sub := range e.list[1:] {
		v, err := evalExpr(sub, environ)
		if err != nil {
			return value{}, err
		}
		if isTruthy(v) {
			return v, nil
		}
	}
	return boolValue(false), nil
}

func applyBuiltin(name string, args []value, e *expr) (value, error) {
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
	// Pre-bind builtins as symbols
	for _, name := range []string{"+", "-", "*", "/", "<", ">", "=", "<=", ">=", "not", "and", "or"} {
		environ.set(name, symbolValue(name))
	}

	var last value
	for _, e := range exprs {
		v, err := evalExpr(e, environ)
		if err != nil {
			return "", &EvalError{Message: err.Error()}
		}
		last = v
	}

	return last.String(), nil
}

// EvalStrWithOutput evaluates Scheme expressions and returns both the result
// string and any captured output from display/write/newline.
func EvalStrWithOutput(input string) (result string, output string, err error) {
	return "", "", &EvalError{Message: "not implemented"}
}
