package ming

import (
	"fmt"
	"strconv"
	"strings"
	"unicode"
)

type position struct {
	line   int
	column int
}

type tokenKind int

const (
	tokenLParen tokenKind = iota
	tokenRParen
	tokenAtom
	tokenString
)

type token struct {
	kind tokenKind
	text string
	pos  position
}

type expr interface {
	exprPos() position
}

type integerExpr struct {
	value int
	pos   position
}

func (e *integerExpr) exprPos() position { return e.pos }

type booleanExpr struct {
	value bool
	pos   position
}

func (e *booleanExpr) exprPos() position { return e.pos }

type stringExpr struct {
	value string
	pos   position
}

func (e *stringExpr) exprPos() position { return e.pos }

type symbolExpr struct {
	value string
	pos   position
}

func (e *symbolExpr) exprPos() position { return e.pos }

type listExpr struct {
	elements []expr
	pos      position
}

func (e *listExpr) exprPos() position { return e.pos }

type stringValue string

type interpreter struct {
	output strings.Builder
}

func evalInput(input string) (string, string, error) {
	intp := &interpreter{}

	parsed, err := parseProgram(input)
	if err != nil {
		return "", "", err
	}

	var result any
	for _, expression := range parsed {
		result, err = intp.eval(expression)
		if err != nil {
			return "", intp.output.String(), err
		}
	}

	return formatValue(result), intp.output.String(), nil
}

func parseProgram(input string) ([]expr, error) {
	tokens, err := lex(input)
	if err != nil {
		return nil, err
	}

	parser := &tokenParser{tokens: tokens}
	return parser.parseProgram()
}

func (i *interpreter) eval(expression expr) (any, error) {
	switch e := expression.(type) {
	case *integerExpr:
		return e.value, nil
	case *booleanExpr:
		return e.value, nil
	case *stringExpr:
		return stringValue(e.value), nil
	case *symbolExpr:
		return nil, newEvalError(e.pos, "unbound variable: %s", e.value)
	case *listExpr:
		return i.evalList(e)
	default:
		return nil, newEvalError(expression.exprPos(), "internal error: unknown expression")
	}
}

func (i *interpreter) evalList(list *listExpr) (any, error) {
	if len(list.elements) == 0 {
		return nil, newEvalError(list.pos, "cannot evaluate empty list")
	}

	operator, ok := list.elements[0].(*symbolExpr)
	if !ok {
		return nil, newEvalError(list.elements[0].exprPos(), "first element in list is not a procedure")
	}

	switch operator.value {
	case "and":
		return i.evalAnd(list.elements[1:], operator.pos)
	case "or":
		return i.evalOr(list.elements[1:], operator.pos)
	default:
		args := make([]any, 0, len(list.elements)-1)
		for _, argExpr := range list.elements[1:] {
			arg, err := i.eval(argExpr)
			if err != nil {
				return nil, err
			}
			args = append(args, arg)
		}
		return applyBuiltin(operator.value, args, operator.pos)
	}
}

func (i *interpreter) evalAnd(args []expr, pos position) (any, error) {
	if len(args) == 0 {
		return true, nil
	}

	var result any = true
	for _, argExpr := range args {
		value, err := i.eval(argExpr)
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

func (i *interpreter) evalOr(args []expr, pos position) (any, error) {
	for _, argExpr := range args {
		value, err := i.eval(argExpr)
		if err != nil {
			return nil, err
		}
		if isTruthy(value) {
			return value, nil
		}
	}

	return false, nil
}

func applyBuiltin(name string, args []any, pos position) (any, error) {
	switch name {
	case "+":
		total := 0
		for _, arg := range args {
			n, err := expectInt(arg, pos, name)
			if err != nil {
				return nil, err
			}
			total += n
		}
		return total, nil

	case "-":
		if len(args) == 0 {
			return nil, newEvalError(pos, "%s expects at least 1 argument", name)
		}
		first, err := expectInt(args[0], pos, name)
		if err != nil {
			return nil, err
		}
		if len(args) == 1 {
			return -first, nil
		}
		result := first
		for _, arg := range args[1:] {
			n, err := expectInt(arg, pos, name)
			if err != nil {
				return nil, err
			}
			result -= n
		}
		return result, nil

	case "*":
		product := 1
		for _, arg := range args {
			n, err := expectInt(arg, pos, name)
			if err != nil {
				return nil, err
			}
			product *= n
		}
		return product, nil

	case "/":
		if len(args) < 2 {
			return nil, newEvalError(pos, "%s expects at least 2 arguments", name)
		}
		first, err := expectInt(args[0], pos, name)
		if err != nil {
			return nil, err
		}
		result := first
		for _, arg := range args[1:] {
			n, err := expectInt(arg, pos, name)
			if err != nil {
				return nil, err
			}
			if n == 0 {
				return nil, newEvalError(pos, "division by zero")
			}
			result /= n
		}
		return result, nil

	case "<":
		return numericCompare(name, args, pos, func(a, b int) bool { return a < b })
	case ">":
		return numericCompare(name, args, pos, func(a, b int) bool { return a > b })
	case "=":
		return numericCompare(name, args, pos, func(a, b int) bool { return a == b })
	case "<=":
		return numericCompare(name, args, pos, func(a, b int) bool { return a <= b })
	case "not":
		if len(args) != 1 {
			return nil, newEvalError(pos, "%s expects exactly 1 argument", name)
		}
		return !isTruthy(args[0]), nil
	default:
		return nil, newEvalError(pos, "unknown procedure: %s", name)
	}
}

func numericCompare(name string, args []any, pos position, compare func(int, int) bool) (bool, error) {
	if len(args) < 2 {
		return false, newEvalError(pos, "%s expects at least 2 arguments", name)
	}

	prev, err := expectInt(args[0], pos, name)
	if err != nil {
		return false, err
	}

	for _, arg := range args[1:] {
		current, err := expectInt(arg, pos, name)
		if err != nil {
			return false, err
		}
		if !compare(prev, current) {
			return false, nil
		}
		prev = current
	}

	return true, nil
}

func expectInt(value any, pos position, procedure string) (int, error) {
	n, ok := value.(int)
	if !ok {
		return 0, newEvalError(pos, "%s expects numeric arguments", procedure)
	}
	return n, nil
}

func isTruthy(value any) bool {
	boolean, ok := value.(bool)
	return !ok || boolean
}

func formatValue(value any) string {
	switch v := value.(type) {
	case nil:
		return ""
	case int:
		return strconv.Itoa(v)
	case bool:
		if v {
			return "#t"
		}
		return "#f"
	case stringValue:
		return strconv.Quote(string(v))
	default:
		return fmt.Sprintf("%v", v)
	}
}

func newEvalError(pos position, format string, args ...any) *EvalError {
	return &EvalError{
		Message: fmt.Sprintf(format, args...),
		Line:    pos.line,
		Column:  pos.column,
	}
}

func lex(input string) ([]token, error) {
	var tokens []token
	line := 1
	column := 1

	for index := 0; index < len(input); {
		ch := input[index]

		if ch == '\n' {
			line++
			column = 1
			index++
			continue
		}
		if unicode.IsSpace(rune(ch)) {
			column++
			index++
			continue
		}
		if ch == ';' {
			for index < len(input) && input[index] != '\n' {
				index++
				column++
			}
			continue
		}

		pos := position{line: line, column: column}

		switch ch {
		case '(':
			tokens = append(tokens, token{kind: tokenLParen, text: "(", pos: pos})
			index++
			column++
		case ')':
			tokens = append(tokens, token{kind: tokenRParen, text: ")", pos: pos})
			index++
			column++
		case '"':
			text, width, err := lexString(input[index:], pos)
			if err != nil {
				return nil, err
			}
			tokens = append(tokens, token{kind: tokenString, text: text, pos: pos})
			index += width
			column += width
		default:
			start := index
			for index < len(input) {
				current := input[index]
				if current == '(' || current == ')' || current == '"' || current == ';' || unicode.IsSpace(rune(current)) {
					break
				}
				index++
				column++
			}
			tokens = append(tokens, token{
				kind: tokenAtom,
				text: input[start:index],
				pos:  pos,
			})
		}
	}

	return tokens, nil
}

func lexString(input string, pos position) (string, int, error) {
	var builder strings.Builder

	for index := 1; index < len(input); index++ {
		ch := input[index]
		if ch == '"' {
			return builder.String(), index + 1, nil
		}
		if ch == '\\' {
			index++
			if index >= len(input) {
				return "", 0, newEvalError(pos, "unterminated string")
			}
			switch input[index] {
			case '"':
				builder.WriteByte('"')
			case '\\':
				builder.WriteByte('\\')
			case 'n':
				builder.WriteByte('\n')
			case 't':
				builder.WriteByte('\t')
			default:
				builder.WriteByte(input[index])
			}
			continue
		}
		if ch == '\n' {
			return "", 0, newEvalError(pos, "unterminated string")
		}
		builder.WriteByte(ch)
	}

	return "", 0, newEvalError(pos, "unterminated string")
}

type tokenParser struct {
	tokens []token
	index  int
}

func (p *tokenParser) parseProgram() ([]expr, error) {
	expressions := make([]expr, 0, len(p.tokens))
	for p.index < len(p.tokens) {
		expression, err := p.parseExpr()
		if err != nil {
			return nil, err
		}
		expressions = append(expressions, expression)
	}
	return expressions, nil
}

func (p *tokenParser) parseExpr() (expr, error) {
	if p.index >= len(p.tokens) {
		return nil, &EvalError{Message: "unexpected end of input"}
	}

	current := p.tokens[p.index]
	p.index++

	switch current.kind {
	case tokenLParen:
		return p.parseList(current.pos)
	case tokenRParen:
		return nil, newEvalError(current.pos, "unexpected )")
	case tokenString:
		return &stringExpr{value: current.text, pos: current.pos}, nil
	case tokenAtom:
		return parseAtom(current), nil
	default:
		return nil, newEvalError(current.pos, "unexpected token")
	}
}

func (p *tokenParser) parseList(pos position) (expr, error) {
	var elements []expr

	for {
		if p.index >= len(p.tokens) {
			return nil, newEvalError(pos, "unterminated list")
		}
		if p.tokens[p.index].kind == tokenRParen {
			p.index++
			return &listExpr{elements: elements, pos: pos}, nil
		}
		element, err := p.parseExpr()
		if err != nil {
			return nil, err
		}
		elements = append(elements, element)
	}
}

func parseAtom(tok token) expr {
	switch tok.text {
	case "#t":
		return &booleanExpr{value: true, pos: tok.pos}
	case "#f":
		return &booleanExpr{value: false, pos: tok.pos}
	}

	if value, err := strconv.Atoi(tok.text); err == nil {
		return &integerExpr{value: value, pos: tok.pos}
	}

	return &symbolExpr{value: tok.text, pos: tok.pos}
}
