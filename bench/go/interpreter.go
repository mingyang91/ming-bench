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

type builtinProc func(args []node) (value, error)

func evalString(input string) (string, error) {
	nodes, err := parseProgram(input)
	if err != nil {
		return "", err
	}
	if len(nodes) == 0 {
		return "", &EvalError{Message: "empty input"}
	}

	env := baseEnv()
	var last value
	for _, expr := range nodes {
		last, err = eval(expr, env)
		if err != nil {
			return "", err
		}
	}
	return formatValue(last)
}

func baseEnv() map[string]value {
	env := map[string]value{
		"+":   builtinNumericFold("+"),
		"-":   builtinSub(),
		"*":   builtinNumericFold("*"),
		"/":   builtinDiv(),
		"<":   builtinCompare("<"),
		">":   builtinCompare(">"),
		"=":   builtinCompare("="),
		"<=":  builtinCompare("<="),
		"not": builtinNot(),
	}
	return env
}

func eval(expr node, env map[string]value) (value, error) {
	switch expr := expr.(type) {
	case integerValue, booleanValue, stringValue:
		return expr, nil
	case symbolNode:
		val, ok := env[expr.name]
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

func evalList(list listNode, env map[string]value) (value, error) {
	if len(list.elements) == 0 {
		return nil, &EvalError{Message: "cannot evaluate empty list"}
	}

	if name, ok := symbolName(list.elements[0]); ok {
		switch name {
		case "and":
			return evalAnd(list.elements[1:], env)
		case "or":
			return evalOr(list.elements[1:], env)
		}
	}

	operator, err := eval(list.elements[0], env)
	if err != nil {
		return nil, err
	}

	proc, ok := operator.(builtinProc)
	if !ok {
		return nil, &EvalError{Message: "not a procedure"}
	}
	return proc(list.elements[1:])
}

func evalAnd(args []node, env map[string]value) (value, error) {
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

func evalOr(args []node, env map[string]value) (value, error) {
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

func builtinNumericFold(name string) builtinProc {
	return func(args []node) (value, error) {
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
			current, err := expectIntegerNode(arg)
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
	return func(args []node) (value, error) {
		if len(args) == 0 {
			return nil, &EvalError{Message: "- expects at least 1 argument"}
		}

		first, err := expectIntegerNode(args[0])
		if err != nil {
			return nil, err
		}
		if len(args) == 1 {
			return integerValue(-first), nil
		}

		total := first
		for _, arg := range args[1:] {
			current, err := expectIntegerNode(arg)
			if err != nil {
				return nil, err
			}
			total -= current
		}
		return integerValue(total), nil
	}
}

func builtinDiv() builtinProc {
	return func(args []node) (value, error) {
		if len(args) < 2 {
			return nil, &EvalError{Message: "/ expects at least 2 arguments"}
		}

		total, err := expectIntegerNode(args[0])
		if err != nil {
			return nil, err
		}

		for _, arg := range args[1:] {
			current, err := expectIntegerNode(arg)
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
	return func(args []node) (value, error) {
		if len(args) < 2 {
			return nil, &EvalError{Message: fmt.Sprintf("%s expects at least 2 arguments", name)}
		}

		prev, err := expectIntegerNode(args[0])
		if err != nil {
			return nil, err
		}

		for _, arg := range args[1:] {
			current, err := expectIntegerNode(arg)
			if err != nil {
				return nil, err
			}
			ok := false
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
	return func(args []node) (value, error) {
		if len(args) != 1 {
			return nil, &EvalError{Message: "not expects exactly 1 argument"}
		}
		v, err := evalNodeValue(args[0])
		if err != nil {
			return nil, err
		}
		return booleanValue(!isTruthy(v)), nil
	}
}

func evalNodeValue(expr node) (value, error) {
	return eval(expr, baseEnv())
}

func expectIntegerNode(expr node) (int, error) {
	v, err := evalNodeValue(expr)
	if err != nil {
		return 0, err
	}
	n, ok := v.(integerValue)
	if !ok {
		return 0, &EvalError{Message: "expected integer"}
	}
	return int(n), nil
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
		if unicode.IsSpace(r) || r == '(' || r == ')' || r == ';' {
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
