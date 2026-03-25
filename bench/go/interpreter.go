package ming

import (
	"fmt"
	"strconv"
	"strings"
	"unicode"
	"unicode/utf8"
)

type node interface{}

type listNode struct {
	elements []node
}

type symbolNode struct {
	name string
}

type value interface{}

type booleanValue bool
type integerValue int
type stringValue string
type symbolValue string

type listValue struct {
	elements []value
}

type voidValue struct{}

type builtinProc func(args []value) (value, error)

type closureValue struct {
	params []string
	body   []node
	env    *environment
}

type environment struct {
	parent *environment
	values map[string]value
}

func evalString(input string) (string, error) {
	nodes, err := parseProgram(input)
	if err != nil {
		return "", err
	}
	if len(nodes) == 0 {
		return "", &EvalError{Message: "empty input"}
	}

	env := baseEnv()
	last := value(voidValue{})
	for _, expr := range nodes {
		last, err = eval(expr, env)
		if err != nil {
			return "", err
		}
	}
	return formatValue(last)
}

func baseEnv() *environment {
	env := newEnvironment(nil)
	env.define("+", builtinNumericFold("+"))
	env.define("-", builtinSub())
	env.define("*", builtinNumericFold("*"))
	env.define("/", builtinDiv())
	env.define("<", builtinCompare("<"))
	env.define(">", builtinCompare(">"))
	env.define("=", builtinCompare("="))
	env.define("<=", builtinCompare("<="))
	env.define("not", builtinNot())
	return env
}

func newEnvironment(parent *environment) *environment {
	return &environment{
		parent: parent,
		values: map[string]value{},
	}
}

func (e *environment) define(name string, val value) {
	e.values[name] = val
}

func (e *environment) lookup(name string) (value, bool) {
	for current := e; current != nil; current = current.parent {
		val, ok := current.values[name]
		if ok {
			return val, true
		}
	}
	return nil, false
}

func eval(expr node, env *environment) (value, error) {
	switch expr := expr.(type) {
	case integerValue, booleanValue, stringValue:
		return expr, nil
	case symbolNode:
		val, ok := env.lookup(expr.name)
		if !ok {
			return nil, &EvalError{Message: fmt.Sprintf("unbound variable: %s", expr.name)}
		}
		return val, nil
	case listNode:
		return evalList(expr, env)
	default:
		return nil, &EvalError{Message: "unknown expression"}
	}
}

func evalList(list listNode, env *environment) (value, error) {
	if len(list.elements) == 0 {
		return nil, &EvalError{Message: "cannot evaluate empty list"}
	}

	if name, ok := symbolName(list.elements[0]); ok {
		switch name {
		case "and":
			return evalAnd(list.elements[1:], env)
		case "or":
			return evalOr(list.elements[1:], env)
		case "define":
			return evalDefine(list.elements[1:], env)
		case "if":
			return evalIf(list.elements[1:], env)
		case "quote":
			return evalQuote(list.elements[1:])
		case "lambda":
			return evalLambda(list.elements[1:], env)
		}
	}

	operator, err := eval(list.elements[0], env)
	if err != nil {
		return nil, err
	}

	args, err := evalArgs(list.elements[1:], env)
	if err != nil {
		return nil, err
	}
	return apply(operator, args)
}

func evalArgs(args []node, env *environment) ([]value, error) {
	values := make([]value, len(args))
	for i, arg := range args {
		evaluated, err := eval(arg, env)
		if err != nil {
			return nil, err
		}
		values[i] = evaluated
	}
	return values, nil
}

func evalAnd(args []node, env *environment) (value, error) {
	result := value(booleanValue(true))
	for _, arg := range args {
		evaluated, err := eval(arg, env)
		if err != nil {
			return nil, err
		}
		result = evaluated
		if !isTruthy(evaluated) {
			return evaluated, nil
		}
	}
	return result, nil
}

func evalOr(args []node, env *environment) (value, error) {
	result := value(booleanValue(false))
	for _, arg := range args {
		evaluated, err := eval(arg, env)
		if err != nil {
			return nil, err
		}
		result = evaluated
		if isTruthy(evaluated) {
			return evaluated, nil
		}
	}
	return result, nil
}

func evalDefine(args []node, env *environment) (value, error) {
	if len(args) < 2 {
		return nil, &EvalError{Message: "define expects a name and value"}
	}

	switch target := args[0].(type) {
	case symbolNode:
		if len(args) != 2 {
			return nil, &EvalError{Message: "define expects exactly 2 arguments"}
		}
		val, err := eval(args[1], env)
		if err != nil {
			return nil, err
		}
		env.define(target.name, val)
		return voidValue{}, nil
	case listNode:
		if len(target.elements) == 0 {
			return nil, &EvalError{Message: "define requires a function name"}
		}

		name, ok := symbolName(target.elements[0])
		if !ok {
			return nil, &EvalError{Message: "define requires a function name"}
		}

		proc, err := makeClosure(target.elements[1:], args[1:], env)
		if err != nil {
			return nil, err
		}
		env.define(name, proc)
		return voidValue{}, nil
	default:
		return nil, &EvalError{Message: "define requires a symbol"}
	}
}

func evalIf(args []node, env *environment) (value, error) {
	if len(args) != 3 {
		return nil, &EvalError{Message: "if expects exactly 3 arguments"}
	}

	condition, err := eval(args[0], env)
	if err != nil {
		return nil, err
	}
	if isTruthy(condition) {
		return eval(args[1], env)
	}
	return eval(args[2], env)
}

func evalQuote(args []node) (value, error) {
	if len(args) != 1 {
		return nil, &EvalError{Message: "quote expects exactly 1 argument"}
	}
	return datumFromNode(args[0])
}

func evalLambda(args []node, env *environment) (value, error) {
	if len(args) < 2 {
		return nil, &EvalError{Message: "lambda expects parameters and a body"}
	}

	params, ok := args[0].(listNode)
	if !ok {
		return nil, &EvalError{Message: "lambda parameters must be a list"}
	}
	return makeClosure(params.elements, args[1:], env)
}

func makeClosure(paramExprs []node, body []node, env *environment) (*closureValue, error) {
	if len(body) == 0 {
		return nil, &EvalError{Message: "lambda requires a body"}
	}

	params, err := parseParamNames(paramExprs)
	if err != nil {
		return nil, err
	}

	return &closureValue{
		params: params,
		body:   body,
		env:    env,
	}, nil
}

func parseParamNames(paramExprs []node) ([]string, error) {
	params := make([]string, len(paramExprs))
	for i, expr := range paramExprs {
		name, ok := symbolName(expr)
		if !ok {
			return nil, &EvalError{Message: "parameter list must contain only symbols"}
		}
		params[i] = name
	}
	return params, nil
}

func apply(proc value, args []value) (value, error) {
	switch proc := proc.(type) {
	case builtinProc:
		return proc(args)
	case *closureValue:
		if len(args) != len(proc.params) {
			return nil, &EvalError{Message: fmt.Sprintf("expected %d arguments, got %d", len(proc.params), len(args))}
		}

		callEnv := newEnvironment(proc.env)
		for i, name := range proc.params {
			callEnv.define(name, args[i])
		}
		return evalSequence(proc.body, callEnv)
	default:
		return nil, &EvalError{Message: "not a procedure"}
	}
}

func evalSequence(exprs []node, env *environment) (value, error) {
	last := value(voidValue{})
	for _, expr := range exprs {
		var err error
		last, err = eval(expr, env)
		if err != nil {
			return nil, err
		}
	}
	return last, nil
}

func builtinNumericFold(name string) builtinProc {
	return func(args []value) (value, error) {
		if len(args) == 0 {
			switch name {
			case "+":
				return integerValue(0), nil
			case "*":
				return integerValue(1), nil
			default:
				return nil, &EvalError{Message: fmt.Sprintf("%s expects at least 1 argument", name)}
			}
		}

		total := 0
		if name == "*" {
			total = 1
		}

		for _, arg := range args {
			current, err := expectIntegerValue(arg)
			if err != nil {
				return nil, err
			}
			if name == "+" {
				total += current
			} else {
				total *= current
			}
		}
		return integerValue(total), nil
	}
}

func builtinSub() builtinProc {
	return func(args []value) (value, error) {
		if len(args) == 0 {
			return nil, &EvalError{Message: "- expects at least 1 argument"}
		}

		first, err := expectIntegerValue(args[0])
		if err != nil {
			return nil, err
		}
		if len(args) == 1 {
			return integerValue(-first), nil
		}

		total := first
		for _, arg := range args[1:] {
			current, err := expectIntegerValue(arg)
			if err != nil {
				return nil, err
			}
			total -= current
		}
		return integerValue(total), nil
	}
}

func builtinDiv() builtinProc {
	return func(args []value) (value, error) {
		if len(args) < 2 {
			return nil, &EvalError{Message: "/ expects at least 2 arguments"}
		}

		total, err := expectIntegerValue(args[0])
		if err != nil {
			return nil, err
		}

		for _, arg := range args[1:] {
			current, err := expectIntegerValue(arg)
			if err != nil {
				return nil, err
			}
			if current == 0 {
				return nil, &EvalError{Message: "division by zero"}
			}
			total /= current
		}
		return integerValue(total), nil
	}
}

func builtinCompare(name string) builtinProc {
	return func(args []value) (value, error) {
		if len(args) < 2 {
			return nil, &EvalError{Message: fmt.Sprintf("%s expects at least 2 arguments", name)}
		}

		prev, err := expectIntegerValue(args[0])
		if err != nil {
			return nil, err
		}

		for _, arg := range args[1:] {
			current, err := expectIntegerValue(arg)
			if err != nil {
				return nil, err
			}

			var ok bool
			switch name {
			case "<":
				ok = prev < current
			case ">":
				ok = prev > current
			case "=":
				ok = prev == current
			case "<=":
				ok = prev <= current
			default:
				return nil, &EvalError{Message: "unknown comparison"}
			}
			if !ok {
				return booleanValue(false), nil
			}
			prev = current
		}
		return booleanValue(true), nil
	}
}

func builtinNot() builtinProc {
	return func(args []value) (value, error) {
		if len(args) != 1 {
			return nil, &EvalError{Message: "not expects exactly 1 argument"}
		}
		return booleanValue(!isTruthy(args[0])), nil
	}
}

func expectIntegerValue(v value) (int, error) {
	n, ok := v.(integerValue)
	if !ok {
		return 0, &EvalError{Message: "expected integer"}
	}
	return int(n), nil
}

func datumFromNode(expr node) (value, error) {
	switch expr := expr.(type) {
	case integerValue, booleanValue, stringValue:
		return expr, nil
	case symbolNode:
		return symbolValue(expr.name), nil
	case listNode:
		elements := make([]value, len(expr.elements))
		for i, element := range expr.elements {
			datum, err := datumFromNode(element)
			if err != nil {
				return nil, err
			}
			elements[i] = datum
		}
		return listValue{elements: elements}, nil
	default:
		return nil, &EvalError{Message: "invalid quoted datum"}
	}
}

func formatValue(v value) (string, error) {
	switch v := v.(type) {
	case integerValue:
		return strconv.Itoa(int(v)), nil
	case booleanValue:
		if v {
			return "#t", nil
		}
		return "#f", nil
	case stringValue:
		return strconv.Quote(string(v)), nil
	case symbolValue:
		return string(v), nil
	case listValue:
		if len(v.elements) == 0 {
			return "()", nil
		}

		parts := make([]string, len(v.elements))
		for i, element := range v.elements {
			formatted, err := formatValue(element)
			if err != nil {
				return "", err
			}
			parts[i] = formatted
		}
		return "(" + strings.Join(parts, " ") + ")", nil
	case voidValue:
		return "", nil
	default:
		return "", &EvalError{Message: "cannot format value"}
	}
}

func isTruthy(v value) bool {
	b, ok := v.(booleanValue)
	return !ok || bool(b)
}

func symbolName(expr node) (string, bool) {
	sym, ok := expr.(symbolNode)
	if !ok {
		return "", false
	}
	return sym.name, true
}

func parseProgram(input string) ([]node, error) {
	p := parser{source: input}
	var nodes []node
	for {
		p.skipWhitespaceAndComments()
		if p.eof() {
			return nodes, nil
		}
		expr, err := p.parseExpr()
		if err != nil {
			return nil, err
		}
		nodes = append(nodes, expr)
	}
}

type parser struct {
	source string
	offset int
}

func (p *parser) parseExpr() (node, error) {
	p.skipWhitespaceAndComments()
	if p.eof() {
		return nil, &EvalError{Message: "unexpected end of input"}
	}

	switch p.peek() {
	case '(':
		return p.parseList()
	case '\'':
		return p.parseQuote()
	case '"':
		return p.parseString()
	case ')':
		return nil, &EvalError{Message: "unexpected )"}
	default:
		return p.parseAtom()
	}
}

func (p *parser) parseList() (node, error) {
	p.offset++
	var elements []node
	for {
		p.skipWhitespaceAndComments()
		if p.eof() {
			return nil, &EvalError{Message: "unterminated list"}
		}
		if p.peek() == ')' {
			p.offset++
			return listNode{elements: elements}, nil
		}
		elem, err := p.parseExpr()
		if err != nil {
			return nil, err
		}
		elements = append(elements, elem)
	}
}

func (p *parser) parseQuote() (node, error) {
	p.offset++
	expr, err := p.parseExpr()
	if err != nil {
		return nil, err
	}
	return listNode{
		elements: []node{
			symbolNode{name: "quote"},
			expr,
		},
	}, nil
}

func (p *parser) parseString() (node, error) {
	p.offset++
	var builder strings.Builder
	for !p.eof() {
		r := p.peek()
		p.offset++
		if r == '"' {
			return stringValue(builder.String()), nil
		}
		if r == '\\' {
			if p.eof() {
				return nil, &EvalError{Message: "unterminated string escape"}
			}
			escaped := p.peek()
			p.offset++
			switch escaped {
			case '"', '\\':
				builder.WriteRune(escaped)
			case 'n':
				builder.WriteByte('\n')
			case 't':
				builder.WriteByte('\t')
			default:
				return nil, &EvalError{Message: fmt.Sprintf("unsupported escape: \\%c", escaped)}
			}
			continue
		}
		builder.WriteRune(r)
	}
	return nil, &EvalError{Message: "unterminated string"}
}

func (p *parser) parseAtom() (node, error) {
	start := p.offset
	for !p.eof() {
		r := p.peek()
		if unicode.IsSpace(r) || r == '(' || r == ')' || r == ';' || r == '\'' {
			break
		}
		p.offset += utf8.RuneLen(r)
	}

	token := p.source[start:p.offset]
	switch token {
	case "#t":
		return booleanValue(true), nil
	case "#f":
		return booleanValue(false), nil
	}

	if isIntegerLiteral(token) {
		n, err := strconv.Atoi(token)
		if err != nil {
			return nil, &EvalError{Message: fmt.Sprintf("invalid integer: %s", token)}
		}
		return integerValue(n), nil
	}

	return symbolNode{name: token}, nil
}

func (p *parser) skipWhitespaceAndComments() {
	for !p.eof() {
		r := p.peek()
		if unicode.IsSpace(r) {
			p.offset += utf8.RuneLen(r)
			continue
		}
		if r == ';' {
			for !p.eof() {
				r = p.peek()
				p.offset += utf8.RuneLen(r)
				if r == '\n' {
					break
				}
			}
			continue
		}
		return
	}
}

func (p *parser) eof() bool {
	return p.offset >= len(p.source)
}

func (p *parser) peek() rune {
	r, _ := utf8.DecodeRuneInString(p.source[p.offset:])
	return r
}

func isIntegerLiteral(token string) bool {
	if token == "" {
		return false
	}
	if token[0] == '-' {
		if len(token) == 1 {
			return false
		}
		token = token[1:]
	}
	for _, r := range token {
		if !unicode.IsDigit(r) {
			return false
		}
	}
	return true
}
