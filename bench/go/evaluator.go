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

type parser struct {
	input string
	pos   int
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

	var result any
	for _, expr := range exprs {
		result, err = eval(expr)
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

func eval(expr any) (any, error) {
	switch node := expr.(type) {
	case int64:
		return node, nil
	case bool:
		return node, nil
	case stringExpr:
		return node.value, nil
	case symbolExpr:
		return nil, &EvalError{Message: fmt.Sprintf("unbound variable: %s", node.name)}
	case listExpr:
		return evalList(node)
	default:
		return nil, &EvalError{Message: "unsupported expression"}
	}
}

func evalList(expr listExpr) (any, error) {
	if len(expr.elements) == 0 {
		return nil, &EvalError{Message: "cannot evaluate empty list"}
	}

	head, ok := expr.elements[0].(symbolExpr)
	if !ok {
		return nil, &EvalError{Message: "first list element must be a procedure name"}
	}

	args := expr.elements[1:]
	switch head.name {
	case "and":
		return evalAnd(args)
	case "or":
		return evalOr(args)
	case "not":
		return evalNot(args)
	case "+":
		return evalAdd(args)
	case "-":
		return evalSub(args)
	case "*":
		return evalMul(args)
	case "/":
		return evalDiv(args)
	case "<":
		return evalCompare(args, func(a, b int64) bool { return a < b })
	case ">":
		return evalCompare(args, func(a, b int64) bool { return a > b })
	case "=":
		return evalCompare(args, func(a, b int64) bool { return a == b })
	case "<=":
		return evalCompare(args, func(a, b int64) bool { return a <= b })
	default:
		return nil, &EvalError{Message: fmt.Sprintf("unknown procedure: %s", head.name)}
	}
}

func evalAnd(args []any) (any, error) {
	result := any(true)
	for _, arg := range args {
		value, err := eval(arg)
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

func evalOr(args []any) (any, error) {
	result := any(false)
	for _, arg := range args {
		value, err := eval(arg)
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

func evalNot(args []any) (any, error) {
	if len(args) != 1 {
		return nil, &EvalError{Message: "not expects exactly 1 argument"}
	}

	value, err := eval(args[0])
	if err != nil {
		return nil, err
	}
	return !isTruthy(value), nil
}

func evalAdd(args []any) (any, error) {
	var sum int64
	for _, value := range args {
		n, err := evalInt(value)
		if err != nil {
			return nil, err
		}
		sum += n
	}
	return sum, nil
}

func evalSub(args []any) (any, error) {
	if len(args) == 0 {
		return nil, &EvalError{Message: "- expects at least 1 argument"}
	}

	first, err := evalInt(args[0])
	if err != nil {
		return nil, err
	}

	if len(args) == 1 {
		return -first, nil
	}

	result := first
	for _, value := range args[1:] {
		n, err := evalInt(value)
		if err != nil {
			return nil, err
		}
		result -= n
	}
	return result, nil
}

func evalMul(args []any) (any, error) {
	result := int64(1)
	for _, value := range args {
		n, err := evalInt(value)
		if err != nil {
			return nil, err
		}
		result *= n
	}
	return result, nil
}

func evalDiv(args []any) (any, error) {
	if len(args) < 2 {
		return nil, &EvalError{Message: "/ expects at least 2 arguments"}
	}

	result, err := evalInt(args[0])
	if err != nil {
		return nil, err
	}

	for _, value := range args[1:] {
		n, err := evalInt(value)
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

func evalCompare(args []any, cmp func(a, b int64) bool) (any, error) {
	if len(args) < 2 {
		return nil, &EvalError{Message: "comparison expects at least 2 arguments"}
	}

	prev, err := evalInt(args[0])
	if err != nil {
		return nil, err
	}

	for _, value := range args[1:] {
		next, err := evalInt(value)
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

func evalInt(expr any) (int64, error) {
	value, err := eval(expr)
	if err != nil {
		return 0, err
	}

	n, ok := value.(int64)
	if !ok {
		return 0, &EvalError{Message: fmt.Sprintf("expected number, got %s", typeName(value))}
	}
	return n, nil
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
	default:
		return "value"
	}
}

func formatValue(value any) string {
	switch v := value.(type) {
	case int64:
		return strconv.FormatInt(v, 10)
	case bool:
		if v {
			return "#t"
		}
		return "#f"
	case string:
		return strconv.Quote(v)
	default:
		return ""
	}
}
