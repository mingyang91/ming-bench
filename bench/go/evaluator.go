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
	valLambda
	valBuiltin
	valChar
)

type value struct {
	typ    valueType
	ival   int64
	bval   bool
	sval   string
	car    *value
	cdr    *value
	// lambda fields
	params []string
	body   []*expr
	closure *env
	// char
	cval rune
	// builtin function
	builtin func(args []*value, line, col int) (*value, error)
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

// ---------- Interpreter ----------

type interp struct {
	output strings.Builder
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
			case "define":
				return evalDefine(e, env)
			case "if":
				return evalIf(e, env)
			case "quote":
				if len(e.items) != 2 {
					return nil, &EvalError{Message: fmt.Sprintf("%d:%d: quote: expected 1 argument", e.line, e.col)}
				}
				return quoteExpr(e.items[1]), nil
			case "lambda":
				return evalLambda(e, env)
			case "let":
				return evalLet(e, env)
			case "begin":
				return evalBegin(e.items[1:], env)
			case "cond":
				return evalCond(e, env)
			}
		}

		// Function call
		fn, err := eval(head, env)
		if err != nil {
			return nil, err
		}

		// Evaluate arguments
		args := make([]*value, len(e.items)-1)
		for i, arg := range e.items[1:] {
			v, err := eval(arg, env)
			if err != nil {
				return nil, err
			}
			args[i] = v
		}

		return applyFunc(fn, args, e)
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

func applyFunc(fn *value, args []*value, callExpr *expr) (*value, error) {
	line, col := callExpr.line, callExpr.col

	switch fn.typ {
	case valBuiltin:
		return fn.builtin(args, line, col)
	case valLambda:
		if len(args) != len(fn.params) {
			return nil, &EvalError{Message: fmt.Sprintf("%d:%d: wrong number of arguments: expected %d, got %d", line, col, len(fn.params), len(args))}
		}
		localEnv := newEnv(fn.closure)
		for i, p := range fn.params {
			localEnv.set(p, args[i])
		}
		var result *value
		var err error
		for _, bodyExpr := range fn.body {
			result, err = eval(bodyExpr, localEnv)
			if err != nil {
				return nil, err
			}
		}
		return result, nil
	default:
		return nil, &EvalError{Message: fmt.Sprintf("%d:%d: not a procedure", line, col)}
	}
}

func evalDefine(e *expr, env *env) (*value, error) {
	if len(e.items) < 3 {
		return nil, &EvalError{Message: fmt.Sprintf("%d:%d: define: bad syntax", e.line, e.col)}
	}
	target := e.items[1]

	// (define (f params...) body...)
	if target.kind == "list" {
		if len(target.items) == 0 {
			return nil, &EvalError{Message: fmt.Sprintf("%d:%d: define: bad syntax", e.line, e.col)}
		}
		name := target.items[0].sval
		params := make([]string, len(target.items)-1)
		for i, p := range target.items[1:] {
			params[i] = p.sval
		}
		fn := &value{
			typ:     valLambda,
			params:  params,
			body:    e.items[2:],
			closure: env,
		}
		env.set(name, fn)
		return voidVal, nil
	}

	// (define x expr)
	if target.kind != "symbol" {
		return nil, &EvalError{Message: fmt.Sprintf("%d:%d: define: bad syntax", e.line, e.col)}
	}
	val, err := eval(e.items[2], env)
	if err != nil {
		return nil, err
	}
	env.set(target.sval, val)
	return voidVal, nil
}

func evalIf(e *expr, env *env) (*value, error) {
	if len(e.items) < 3 || len(e.items) > 4 {
		return nil, &EvalError{Message: fmt.Sprintf("%d:%d: if: bad syntax", e.line, e.col)}
	}
	cond, err := eval(e.items[1], env)
	if err != nil {
		return nil, err
	}
	if cond.isTruthy() {
		return eval(e.items[2], env)
	}
	if len(e.items) == 4 {
		return eval(e.items[3], env)
	}
	return voidVal, nil
}

func evalLambda(e *expr, env *env) (*value, error) {
	if len(e.items) < 3 {
		return nil, &EvalError{Message: fmt.Sprintf("%d:%d: lambda: bad syntax", e.line, e.col)}
	}
	paramExpr := e.items[1]
	if paramExpr.kind != "list" {
		return nil, &EvalError{Message: fmt.Sprintf("%d:%d: lambda: bad parameter list", e.line, e.col)}
	}
	params := make([]string, len(paramExpr.items))
	for i, p := range paramExpr.items {
		params[i] = p.sval
	}
	return &value{
		typ:     valLambda,
		params:  params,
		body:    e.items[2:],
		closure: env,
	}, nil
}

func evalLet(e *expr, env *env) (*value, error) {
	// (let ((x 1) (y 2)) body...) or named let: (let name ((x 1)) body...)
	if len(e.items) < 3 {
		return nil, &EvalError{Message: fmt.Sprintf("%d:%d: let: bad syntax", e.line, e.col)}
	}

	idx := 1
	var name string

	// Named let: (let loop ((i 0)) body...)
	if e.items[1].kind == "symbol" {
		if len(e.items) < 4 {
			return nil, &EvalError{Message: fmt.Sprintf("%d:%d: let: bad syntax", e.line, e.col)}
		}
		name = e.items[1].sval
		idx = 2
	}

	bindingsExpr := e.items[idx]
	body := e.items[idx+1:]

	if bindingsExpr.kind != "list" {
		return nil, &EvalError{Message: fmt.Sprintf("%d:%d: let: bad syntax", e.line, e.col)}
	}

	params := make([]string, len(bindingsExpr.items))
	vals := make([]*value, len(bindingsExpr.items))
	for i, b := range bindingsExpr.items {
		if b.kind != "list" || len(b.items) != 2 {
			return nil, &EvalError{Message: fmt.Sprintf("%d:%d: let: bad binding", e.line, e.col)}
		}
		params[i] = b.items[0].sval
		v, err := eval(b.items[1], env)
		if err != nil {
			return nil, err
		}
		vals[i] = v
	}

	localEnv := newEnv(env)
	for i, p := range params {
		localEnv.set(p, vals[i])
	}

	if name != "" {
		// Named let: bind the name to a lambda for recursion
		fn := &value{
			typ:     valLambda,
			params:  params,
			body:    body,
			closure: localEnv,
		}
		localEnv.set(name, fn)
	}

	var result *value
	var err error
	for _, b := range body {
		result, err = eval(b, localEnv)
		if err != nil {
			return nil, err
		}
	}
	return result, nil
}

func evalBegin(exprs []*expr, env *env) (*value, error) {
	if len(exprs) == 0 {
		return voidVal, nil
	}
	var result *value
	var err error
	for _, e := range exprs {
		result, err = eval(e, env)
		if err != nil {
			return nil, err
		}
	}
	return result, nil
}

func evalCond(e *expr, env *env) (*value, error) {
	for _, clause := range e.items[1:] {
		if clause.kind != "list" || len(clause.items) < 2 {
			return nil, &EvalError{Message: fmt.Sprintf("%d:%d: cond: bad clause", e.line, e.col)}
		}
		// else clause
		if clause.items[0].kind == "symbol" && clause.items[0].sval == "else" {
			return evalBegin(clause.items[1:], env)
		}
		test, err := eval(clause.items[0], env)
		if err != nil {
			return nil, err
		}
		if test.isTruthy() {
			return evalBegin(clause.items[1:], env)
		}
	}
	return voidVal, nil
}

func quoteExpr(e *expr) *value {
	switch e.kind {
	case "int":
		return intVal(e.ival)
	case "bool":
		return boolVal(e.bval)
	case "string":
		return strVal(e.sval)
	case "symbol":
		return symVal(e.sval)
	case "list":
		if len(e.items) == 0 {
			return nilVal
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

func makeBuiltin(name string, fn func(args []*value, line, col int) (*value, error)) *value {
	return &value{typ: valBuiltin, sval: name, builtin: fn}
}

func makeGlobalEnv(ip *interp) *env {
	e := newEnv(nil)

	e.set("+", makeBuiltin("+", func(args []*value, line, col int) (*value, error) {
		sum := int64(0)
		for _, a := range args {
			if a.typ != valInt {
				return nil, &EvalError{Message: fmt.Sprintf("%d:%d: +: expected number", line, col)}
			}
			sum += a.ival
		}
		return intVal(sum), nil
	}))

	e.set("-", makeBuiltin("-", func(args []*value, line, col int) (*value, error) {
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
	}))

	e.set("*", makeBuiltin("*", func(args []*value, line, col int) (*value, error) {
		product := int64(1)
		for _, a := range args {
			if a.typ != valInt {
				return nil, &EvalError{Message: fmt.Sprintf("%d:%d: *: expected number", line, col)}
			}
			product *= a.ival
		}
		return intVal(product), nil
	}))

	e.set("/", makeBuiltin("/", func(args []*value, line, col int) (*value, error) {
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
		return boolVal(args[0].typ == valInt), nil
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
	env := makeGlobalEnv(ip)
	for _, e := range exprs {
		last, err = eval(e, env)
		if err != nil {
			return nil, nil, err
		}
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
