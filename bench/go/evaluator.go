package ming

import (
	"fmt"
	"strconv"
	"strings"
)

// EvalStr evaluates one or more Scheme expressions and returns the string
// representation of the last result.
func EvalStr(input string) (string, error) {
	result, _, err := evalStrInternal(input)
	if err != nil {
		return "", err
	}
	return formatValue(result), nil
}

// EvalStrWithOutput evaluates Scheme expressions and returns both the result
// string and any captured output from display/write/newline.
func EvalStrWithOutput(input string) (result string, output string, err error) {
	value, output, err := evalStrInternal(input)
	if err != nil {
		return "", output, err
	}
	return formatValue(value), output, nil
}

type stringExpr struct {
	value string
}

type symbolExpr struct {
	name string
}

type listExpr struct {
	elements []any
}

type voidValue struct{}

type parser struct {
	input string
	pos   int
}

type env struct {
	parent   *env
	bindings map[string]any
}

type builtinFunc func(args []any) (any, error)

type builtinProc struct {
	name string
	fn   builtinFunc
}

type closure struct {
	params []string
	body   []any
	env    *env
}

func newEnv(parent *env) *env {
	return &env{
		parent:   parent,
		bindings: map[string]any{},
	}
}

func (e *env) define(name string, value any) {
	e.bindings[name] = value
}

func (e *env) lookup(name string) (any, bool) {
	for scope := e; scope != nil; scope = scope.parent {
		if value, ok := scope.bindings[name]; ok {
			return value, true
		}
	}
	return nil, false
}

func newGlobalEnv() *env {
	scope := newEnv(nil)
	scope.define("+", builtinProc{name: "+", fn: builtinAdd})
	scope.define("-", builtinProc{name: "-", fn: builtinSub})
	scope.define("*", builtinProc{name: "*", fn: builtinMul})
	scope.define("/", builtinProc{name: "/", fn: builtinDiv})
	scope.define("<", builtinProc{name: "<", fn: func(args []any) (any, error) {
		return builtinCompare(args, func(a, b int64) bool { return a < b })
	}})
	scope.define(">", builtinProc{name: ">", fn: func(args []any) (any, error) {
		return builtinCompare(args, func(a, b int64) bool { return a > b })
	}})
	scope.define("=", builtinProc{name: "=", fn: func(args []any) (any, error) {
		return builtinCompare(args, func(a, b int64) bool { return a == b })
	}})
	scope.define("<=", builtinProc{name: "<=", fn: func(args []any) (any, error) {
		return builtinCompare(args, func(a, b int64) bool { return a <= b })
	}})
	scope.define("not", builtinProc{name: "not", fn: builtinNot})
	return scope
}

func evalStrInternal(input string) (any, string, error) {
	p := parser{input: input}
	exprs, err := p.parseProgram()
	if err != nil {
		return nil, "", err
	}
	if len(exprs) == 0 {
		return nil, "", &EvalError{Message: "empty input"}
	}

	scope := newGlobalEnv()
	result := any(voidValue{})
	for _, expr := range exprs {
		result, err = eval(scope, expr)
		if err != nil {
			return nil, "", err
		}
	}

	return result, "", nil
}

func (p *parser) parseProgram() ([]any, error) {
	var exprs []any
	for {
		p.skipIgnorable()
		if p.atEnd() {
			return exprs, nil
		}

		expr, err := p.parseExpr()
		if err != nil {
			return nil, err
		}
		exprs = append(exprs, expr)
	}
}

func (p *parser) parseExpr() (any, error) {
	p.skipIgnorable()
	if p.atEnd() {
		return nil, &EvalError{Message: "unexpected end of input"}
	}

	switch p.peek() {
	case '(':
		return p.parseList()
	case '"':
		return p.parseString()
	default:
		return p.parseAtom()
	}
}

func (p *parser) parseList() (any, error) {
	p.pos++

	var elements []any
	for {
		p.skipIgnorable()
		if p.atEnd() {
			return nil, &EvalError{Message: "unterminated list"}
		}
		if p.peek() == ')' {
			p.pos++
			return listExpr{elements: elements}, nil
		}

		expr, err := p.parseExpr()
		if err != nil {
			return nil, err
		}
		elements = append(elements, expr)
	}
}

func (p *parser) parseString() (any, error) {
	p.pos++

	var b strings.Builder
	for !p.atEnd() {
		ch := p.peek()
		p.pos++

		switch ch {
		case '"':
			return stringExpr{value: b.String()}, nil
		case '\\':
			if p.atEnd() {
				return nil, &EvalError{Message: "unterminated string"}
			}

			escaped := p.peek()
			p.pos++
			switch escaped {
			case 'n':
				b.WriteByte('\n')
			case 't':
				b.WriteByte('\t')
			case '"':
				b.WriteByte('"')
			case '\\':
				b.WriteByte('\\')
			default:
				b.WriteByte(escaped)
			}
		default:
			b.WriteByte(ch)
		}
	}

	return nil, &EvalError{Message: "unterminated string"}
}

func (p *parser) parseAtom() (any, error) {
	start := p.pos
	for !p.atEnd() {
		ch := p.peek()
		if isDelimiter(ch) {
			break
		}
		p.pos++
	}

	token := p.input[start:p.pos]
	if token == "" {
		return nil, &EvalError{Message: "unexpected token"}
	}

	switch token {
	case "#t":
		return true, nil
	case "#f":
		return false, nil
	}

	if n, err := strconv.ParseInt(token, 10, 64); err == nil {
		return n, nil
	}

	return symbolExpr{name: token}, nil
}

func (p *parser) skipIgnorable() {
	for !p.atEnd() {
		ch := p.peek()
		switch ch {
		case ' ', '\t', '\n', '\r':
			p.pos++
		case ';':
			for !p.atEnd() && p.peek() != '\n' {
				p.pos++
			}
		default:
			return
		}
	}
}

func (p *parser) atEnd() bool {
	return p.pos >= len(p.input)
}

func (p *parser) peek() byte {
	return p.input[p.pos]
}

func isDelimiter(ch byte) bool {
	switch ch {
	case ' ', '\t', '\n', '\r', '(', ')', ';':
		return true
	default:
		return false
	}
}

func eval(scope *env, expr any) (any, error) {
	switch node := expr.(type) {
	case int64:
		return node, nil
	case bool:
		return node, nil
	case stringExpr:
		return node.value, nil
	case symbolExpr:
		value, ok := scope.lookup(node.name)
		if !ok {
			return nil, &EvalError{Message: fmt.Sprintf("unbound variable: %s", node.name)}
		}
		return value, nil
	case listExpr:
		return evalList(scope, node)
	case string:
		return node, nil
	case builtinProc:
		return node, nil
	case closure:
		return node, nil
	case voidValue:
		return node, nil
	default:
		return nil, &EvalError{Message: "unsupported expression"}
	}
}

func evalList(scope *env, expr listExpr) (any, error) {
	if len(expr.elements) == 0 {
		return nil, &EvalError{Message: "cannot evaluate empty list"}
	}

	if head, ok := expr.elements[0].(symbolExpr); ok {
		args := expr.elements[1:]
		switch head.name {
		case "define":
			return evalDefine(scope, args)
		case "if":
			return evalIf(scope, args)
		case "quote":
			return evalQuote(args)
		case "lambda":
			return evalLambda(scope, args)
		case "and":
			return evalAnd(scope, args)
		case "or":
			return evalOr(scope, args)
		}
	}

	proc, err := eval(scope, expr.elements[0])
	if err != nil {
		return nil, err
	}

	args := make([]any, 0, len(expr.elements)-1)
	for _, argExpr := range expr.elements[1:] {
		arg, err := eval(scope, argExpr)
		if err != nil {
			return nil, err
		}
		args = append(args, arg)
	}

	return applyProcedure(proc, args)
}

func evalDefine(scope *env, args []any) (any, error) {
	if len(args) < 2 {
		return nil, &EvalError{Message: "define expects a name and value"}
	}

	switch target := args[0].(type) {
	case symbolExpr:
		if len(args) != 2 {
			return nil, &EvalError{Message: "define variable form expects exactly 2 arguments"}
		}

		value, err := eval(scope, args[1])
		if err != nil {
			return nil, err
		}
		scope.define(target.name, value)
		return voidValue{}, nil
	case listExpr:
		if len(target.elements) == 0 {
			return nil, &EvalError{Message: "define function form requires a name"}
		}

		name, ok := target.elements[0].(symbolExpr)
		if !ok {
			return nil, &EvalError{Message: "define function name must be a symbol"}
		}

		params, err := parseParams(target.elements[1:])
		if err != nil {
			return nil, err
		}

		proc := closure{
			params: params,
			body:   args[1:],
			env:    scope,
		}
		scope.define(name.name, proc)
		return voidValue{}, nil
	default:
		return nil, &EvalError{Message: "define requires a symbol or function signature"}
	}
}

func evalIf(scope *env, args []any) (any, error) {
	if len(args) != 3 {
		return nil, &EvalError{Message: "if expects exactly 3 arguments"}
	}

	cond, err := eval(scope, args[0])
	if err != nil {
		return nil, err
	}

	if isTruthy(cond) {
		return eval(scope, args[1])
	}
	return eval(scope, args[2])
}

func evalQuote(args []any) (any, error) {
	if len(args) != 1 {
		return nil, &EvalError{Message: "quote expects exactly 1 argument"}
	}
	return quoteDatum(args[0]), nil
}

func evalLambda(scope *env, args []any) (any, error) {
	if len(args) < 2 {
		return nil, &EvalError{Message: "lambda expects parameters and a body"}
	}

	paramsExpr, ok := args[0].(listExpr)
	if !ok {
		return nil, &EvalError{Message: "lambda parameters must be a list"}
	}

	params, err := parseParams(paramsExpr.elements)
	if err != nil {
		return nil, err
	}

	return closure{
		params: params,
		body:   args[1:],
		env:    scope,
	}, nil
}

func evalAnd(scope *env, args []any) (any, error) {
	result := any(true)
	for _, arg := range args {
		value, err := eval(scope, arg)
		if err != nil {
			return nil, err
		}
		if !isTruthy(value) {
			return value, nil
		}
		result = value
	}
	return result, nil
}

func evalOr(scope *env, args []any) (any, error) {
	result := any(false)
	for _, arg := range args {
		value, err := eval(scope, arg)
		if err != nil {
			return nil, err
		}
		if isTruthy(value) {
			return value, nil
		}
		result = value
	}
	return result, nil
}

func parseParams(params []any) ([]string, error) {
	names := make([]string, 0, len(params))
	for _, param := range params {
		name, ok := param.(symbolExpr)
		if !ok {
			return nil, &EvalError{Message: "parameter names must be symbols"}
		}
		names = append(names, name.name)
	}
	return names, nil
}

func applyProcedure(proc any, args []any) (any, error) {
	switch callable := proc.(type) {
	case builtinProc:
		return callable.fn(args)
	case closure:
		if len(args) != len(callable.params) {
			return nil, &EvalError{Message: fmt.Sprintf("expected %d arguments, got %d", len(callable.params), len(args))}
		}

		callScope := newEnv(callable.env)
		for i, param := range callable.params {
			callScope.define(param, args[i])
		}

		return evalSequence(callScope, callable.body)
	default:
		return nil, &EvalError{Message: fmt.Sprintf("expected procedure, got %s", typeName(proc))}
	}
}

func evalSequence(scope *env, exprs []any) (any, error) {
	result := any(voidValue{})
	for _, expr := range exprs {
		value, err := eval(scope, expr)
		if err != nil {
			return nil, err
		}
		result = value
	}
	return result, nil
}

func builtinNot(args []any) (any, error) {
	if len(args) != 1 {
		return nil, &EvalError{Message: "not expects exactly 1 argument"}
	}
	return !isTruthy(args[0]), nil
}

func builtinAdd(args []any) (any, error) {
	var sum int64
	for _, arg := range args {
		n, err := expectInt(arg)
		if err != nil {
			return nil, err
		}
		sum += n
	}
	return sum, nil
}

func builtinSub(args []any) (any, error) {
	if len(args) == 0 {
		return nil, &EvalError{Message: "- expects at least 1 argument"}
	}

	first, err := expectInt(args[0])
	if err != nil {
		return nil, err
	}

	if len(args) == 1 {
		return -first, nil
	}

	result := first
	for _, arg := range args[1:] {
		n, err := expectInt(arg)
		if err != nil {
			return nil, err
		}
		result -= n
	}
	return result, nil
}

func builtinMul(args []any) (any, error) {
	result := int64(1)
	for _, arg := range args {
		n, err := expectInt(arg)
		if err != nil {
			return nil, err
		}
		result *= n
	}
	return result, nil
}

func builtinDiv(args []any) (any, error) {
	if len(args) < 2 {
		return nil, &EvalError{Message: "/ expects at least 2 arguments"}
	}

	result, err := expectInt(args[0])
	if err != nil {
		return nil, err
	}

	for _, arg := range args[1:] {
		n, err := expectInt(arg)
		if err != nil {
			return nil, err
		}
		if n == 0 {
			return nil, &EvalError{Message: "division by zero"}
		}
		result /= n
	}

	return result, nil
}

func builtinCompare(args []any, cmp func(a, b int64) bool) (any, error) {
	if len(args) < 2 {
		return nil, &EvalError{Message: "comparison expects at least 2 arguments"}
	}

	prev, err := expectInt(args[0])
	if err != nil {
		return nil, err
	}

	for _, arg := range args[1:] {
		next, err := expectInt(arg)
		if err != nil {
			return nil, err
		}
		if !cmp(prev, next) {
			return false, nil
		}
		prev = next
	}

	return true, nil
}

func expectInt(value any) (int64, error) {
	n, ok := value.(int64)
	if !ok {
		return 0, &EvalError{Message: fmt.Sprintf("expected number, got %s", typeName(value))}
	}
	return n, nil
}

func quoteDatum(expr any) any {
	switch node := expr.(type) {
	case int64:
		return node
	case bool:
		return node
	case stringExpr:
		return node.value
	case symbolExpr:
		return node
	case listExpr:
		elements := make([]any, len(node.elements))
		for i, elem := range node.elements {
			elements[i] = quoteDatum(elem)
		}
		return listExpr{elements: elements}
	default:
		return expr
	}
}

func isTruthy(value any) bool {
	if b, ok := value.(bool); ok {
		return b
	}
	return true
}

func typeName(value any) string {
	switch value.(type) {
	case int64:
		return "number"
	case bool:
		return "boolean"
	case string:
		return "string"
	case symbolExpr:
		return "symbol"
	case listExpr:
		return "list"
	case builtinProc, closure:
		return "procedure"
	case voidValue:
		return "void"
	default:
		return "value"
	}
}

func formatValue(value any) string {
	switch v := value.(type) {
	case voidValue:
		return ""
	case int64:
		return strconv.FormatInt(v, 10)
	case bool:
		if v {
			return "#t"
		}
		return "#f"
	case string:
		return strconv.Quote(v)
	case symbolExpr:
		return v.name
	case listExpr:
		if len(v.elements) == 0 {
			return "()"
		}

		parts := make([]string, len(v.elements))
		for i, elem := range v.elements {
			parts[i] = formatValue(elem)
		}
		return "(" + strings.Join(parts, " ") + ")"
	default:
		return ""
	}
}
