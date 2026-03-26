package ming

import (
	"fmt"
	"strconv"
	"strings"
	"unicode"
	"unicode/utf8"
)

type expr interface{}

type intExpr int
type boolExpr bool
type stringExpr string
type symbolExpr string
type listExpr []expr

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
}

type parser struct {
	tokens []token
	pos    int
}

func evalProgram(input string) (string, error) {
	tokens, err := tokenize(input)
	if err != nil {
		return "", err
	}

	p := parser{tokens: tokens}
	program, err := p.parseProgram()
	if err != nil {
		return "", err
	}
	if len(program) == 0 {
		return "", &EvalError{Message: "empty program"}
	}

	var result expr
	for _, form := range program {
		result, err = evalExpr(form)
		if err != nil {
			return "", err
		}
	}

	return renderExpr(result), nil
}

func tokenize(input string) ([]token, error) {
	var tokens []token

	for len(input) > 0 {
		r, size := utf8.DecodeRuneInString(input)
		switch {
		case unicode.IsSpace(r):
			input = input[size:]
		case r == ';':
			input = skipLineComment(input[size:])
		case r == '(':
			tokens = append(tokens, token{kind: tokenLParen, text: "("})
			input = input[size:]
		case r == ')':
			tokens = append(tokens, token{kind: tokenRParen, text: ")"})
			input = input[size:]
		case r == '"':
			text, rest, err := scanString(input[size:])
			if err != nil {
				return nil, err
			}
			tokens = append(tokens, token{kind: tokenString, text: text})
			input = rest
		default:
			text, rest := scanAtom(input)
			tokens = append(tokens, token{kind: tokenAtom, text: text})
			input = rest
		}
	}

	return tokens, nil
}

func skipLineComment(input string) string {
	for len(input) > 0 {
		r, size := utf8.DecodeRuneInString(input)
		input = input[size:]
		if r == '\n' {
			return input
		}
	}
	return ""
}

func scanString(input string) (string, string, error) {
	var b strings.Builder

	for len(input) > 0 {
		r, size := utf8.DecodeRuneInString(input)
		input = input[size:]

		switch r {
		case '"':
			return b.String(), input, nil
		case '\\':
			if len(input) == 0 {
				return "", "", &EvalError{Message: "unterminated string escape"}
			}
			esc, escSize := utf8.DecodeRuneInString(input)
			input = input[escSize:]
			switch esc {
			case '"', '\\':
				b.WriteRune(esc)
			case 'n':
				b.WriteByte('\n')
			case 't':
				b.WriteByte('\t')
			default:
				b.WriteRune(esc)
			}
		default:
			b.WriteRune(r)
		}
	}

	return "", "", &EvalError{Message: "unterminated string literal"}
}

func scanAtom(input string) (string, string) {
	for i, r := range input {
		if unicode.IsSpace(r) || r == '(' || r == ')' || r == ';' {
			return input[:i], input[i:]
		}
	}
	return input, ""
}

func (p *parser) parseProgram() ([]expr, error) {
	var forms []expr
	for p.pos < len(p.tokens) {
		form, err := p.parseExpr()
		if err != nil {
			return nil, err
		}
		forms = append(forms, form)
	}
	return forms, nil
}

func (p *parser) parseExpr() (expr, error) {
	if p.pos >= len(p.tokens) {
		return nil, &EvalError{Message: "unexpected end of input"}
	}

	tok := p.tokens[p.pos]
	p.pos++

	switch tok.kind {
	case tokenLParen:
		var items []expr
		for {
			if p.pos >= len(p.tokens) {
				return nil, &EvalError{Message: "unterminated list"}
			}
			if p.tokens[p.pos].kind == tokenRParen {
				p.pos++
				return listExpr(items), nil
			}
			item, err := p.parseExpr()
			if err != nil {
				return nil, err
			}
			items = append(items, item)
		}
	case tokenRParen:
		return nil, &EvalError{Message: "unexpected ')'"}
	case tokenString:
		return stringExpr(tok.text), nil
	case tokenAtom:
		return parseAtom(tok.text), nil
	default:
		return nil, &EvalError{Message: "unknown token"}
	}
}

func parseAtom(text string) expr {
	switch text {
	case "#t":
		return boolExpr(true)
	case "#f":
		return boolExpr(false)
	}

	if n, err := strconv.Atoi(text); err == nil {
		return intExpr(n)
	}

	return symbolExpr(text)
}

func evalExpr(form expr) (expr, error) {
	switch v := form.(type) {
	case intExpr, boolExpr, stringExpr:
		return v, nil
	case symbolExpr:
		return nil, &EvalError{Message: fmt.Sprintf("unbound symbol: %s", string(v))}
	case listExpr:
		return evalList(v)
	default:
		return nil, &EvalError{Message: "unsupported expression"}
	}
}

func evalList(items listExpr) (expr, error) {
	if len(items) == 0 {
		return nil, &EvalError{Message: "cannot evaluate empty list"}
	}

	operator, ok := items[0].(symbolExpr)
	if !ok {
		return nil, &EvalError{Message: "first list element is not a procedure"}
	}

	switch string(operator) {
	case "and":
		return evalAnd(items[1:])
	case "or":
		return evalOr(items[1:])
	case "not":
		args, err := evalArgs(items[1:])
		if err != nil {
			return nil, err
		}
		if len(args) != 1 {
			return nil, &EvalError{Message: "not expects exactly 1 argument"}
		}
		return boolExpr(!isTruthy(args[0])), nil
	case "+":
		return evalAdd(items[1:])
	case "-":
		return evalSub(items[1:])
	case "*":
		return evalMul(items[1:])
	case "/":
		return evalDiv(items[1:])
	case "<":
		return evalComparison(items[1:], func(a, b int) bool { return a < b }, "<")
	case ">":
		return evalComparison(items[1:], func(a, b int) bool { return a > b }, ">")
	case "=":
		return evalComparison(items[1:], func(a, b int) bool { return a == b }, "=")
	case "<=":
		return evalComparison(items[1:], func(a, b int) bool { return a <= b }, "<=")
	default:
		return nil, &EvalError{Message: fmt.Sprintf("unknown procedure: %s", string(operator))}
	}
}

func evalArgs(forms []expr) ([]expr, error) {
	args := make([]expr, 0, len(forms))
	for _, form := range forms {
		value, err := evalExpr(form)
		if err != nil {
			return nil, err
		}
		args = append(args, value)
	}
	return args, nil
}

func evalAnd(forms []expr) (expr, error) {
	result := expr(boolExpr(true))
	for _, form := range forms {
		value, err := evalExpr(form)
		if err != nil {
			return nil, err
		}
		result = value
		if !isTruthy(value) {
			return value, nil
		}
	}
	return result, nil
}

func evalOr(forms []expr) (expr, error) {
	for _, form := range forms {
		value, err := evalExpr(form)
		if err != nil {
			return nil, err
		}
		if isTruthy(value) {
			return value, nil
		}
	}
	return boolExpr(false), nil
}

func evalAdd(forms []expr) (expr, error) {
	args, err := evalNumericArgs(forms)
	if err != nil {
		return nil, err
	}

	total := 0
	for _, n := range args {
		total += n
	}
	return intExpr(total), nil
}

func evalSub(forms []expr) (expr, error) {
	args, err := evalNumericArgs(forms)
	if err != nil {
		return nil, err
	}
	if len(args) == 0 {
		return nil, &EvalError{Message: "- expects at least 1 argument"}
	}
	if len(args) == 1 {
		return intExpr(-args[0]), nil
	}

	result := args[0]
	for _, n := range args[1:] {
		result -= n
	}
	return intExpr(result), nil
}

func evalMul(forms []expr) (expr, error) {
	args, err := evalNumericArgs(forms)
	if err != nil {
		return nil, err
	}

	result := 1
	for _, n := range args {
		result *= n
	}
	return intExpr(result), nil
}

func evalDiv(forms []expr) (expr, error) {
	args, err := evalNumericArgs(forms)
	if err != nil {
		return nil, err
	}
	if len(args) == 0 {
		return nil, &EvalError{Message: "/ expects at least 1 argument"}
	}

	result := args[0]
	if len(args) == 1 {
		if result == 0 {
			return nil, &EvalError{Message: "division by zero"}
		}
		return intExpr(1 / result), nil
	}

	for _, n := range args[1:] {
		if n == 0 {
			return nil, &EvalError{Message: "division by zero"}
		}
		result /= n
	}
	return intExpr(result), nil
}

func evalComparison(forms []expr, cmp func(int, int) bool, name string) (expr, error) {
	args, err := evalNumericArgs(forms)
	if err != nil {
		return nil, err
	}
	if len(args) < 2 {
		return nil, &EvalError{Message: fmt.Sprintf("%s expects at least 2 arguments", name)}
	}

	for i := 0; i < len(args)-1; i++ {
		if !cmp(args[i], args[i+1]) {
			return boolExpr(false), nil
		}
	}
	return boolExpr(true), nil
}

func evalNumericArgs(forms []expr) ([]int, error) {
	values, err := evalArgs(forms)
	if err != nil {
		return nil, err
	}

	args := make([]int, 0, len(values))
	for _, value := range values {
		number, ok := value.(intExpr)
		if !ok {
			return nil, &EvalError{Message: "expected number"}
		}
		args = append(args, int(number))
	}
	return args, nil
}

func isTruthy(value expr) bool {
	b, ok := value.(boolExpr)
	return !ok || bool(b)
}

func renderExpr(value expr) string {
	switch v := value.(type) {
	case intExpr:
		return strconv.Itoa(int(v))
	case boolExpr:
		if bool(v) {
			return "#t"
		}
		return "#f"
	case stringExpr:
		return strconv.Quote(string(v))
	case symbolExpr:
		return string(v)
	case listExpr:
		parts := make([]string, 0, len(v))
		for _, item := range v {
			parts = append(parts, renderExpr(item))
		}
		return "(" + strings.Join(parts, " ") + ")"
	default:
		return ""
	}
}
