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
type voidExpr struct{}

type builtinFunc func([]expr) (expr, error)

type builtinProc struct {
	name string
	fn   builtinFunc
}

type closureExpr struct {
	params []string
	body   []expr
	env    *env
}

type env struct {
	parent   *env
	bindings map[string]expr
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

	environment := newGlobalEnv()
	result, err := evalSequence(environment, program)
	if err != nil {
		return "", err
	}

	return renderExpr(result), nil
}

func newGlobalEnv() *env {
	root := &env{bindings: map[string]expr{}}

	root.define("+", builtinProc{name: "+", fn: builtinAdd})
	root.define("-", builtinProc{name: "-", fn: builtinSub})
	root.define("*", builtinProc{name: "*", fn: builtinMul})
	root.define("/", builtinProc{name: "/", fn: builtinDiv})
	root.define("<", builtinProc{name: "<", fn: comparisonBuiltin("<", func(a, b int) bool { return a < b })})
	root.define(">", builtinProc{name: ">", fn: comparisonBuiltin(">", func(a, b int) bool { return a > b })})
	root.define("=", builtinProc{name: "=", fn: comparisonBuiltin("=", func(a, b int) bool { return a == b })})
	root.define("<=", builtinProc{name: "<=", fn: comparisonBuiltin("<=", func(a, b int) bool { return a <= b })})
	root.define("not", builtinProc{name: "not", fn: builtinNot})

	return root
}

func (e *env) define(name string, value expr) {
	e.bindings[name] = value
}

func (e *env) lookup(name string) (expr, bool) {
	for current := e; current != nil; current = current.parent {
		if value, ok := current.bindings[name]; ok {
			return value, true
		}
	}
	return nil, false
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

func evalSequence(environment *env, forms []expr) (expr, error) {
	result := expr(voidExpr{})
	for _, form := range forms {
		value, err := evalExpr(environment, form)
		if err != nil {
			return nil, err
		}
		result = value
	}
	return result, nil
}

func evalExpr(environment *env, form expr) (expr, error) {
	switch v := form.(type) {
	case intExpr, boolExpr, stringExpr:
		return v, nil
	case symbolExpr:
		value, ok := environment.lookup(string(v))
		if !ok {
			return nil, &EvalError{Message: fmt.Sprintf("unbound symbol: %s", string(v))}
		}
		return value, nil
	case listExpr:
		return evalList(environment, v)
	default:
		return nil, &EvalError{Message: "unsupported expression"}
	}
}

func evalList(environment *env, items listExpr) (expr, error) {
	if len(items) == 0 {
		return nil, &EvalError{Message: "cannot evaluate empty list"}
	}

	if operator, ok := items[0].(symbolExpr); ok {
		switch string(operator) {
		case "define":
			return evalDefine(environment, items[1:])
		case "if":
			return evalIf(environment, items[1:])
		case "quote":
			return evalQuote(items[1:])
		case "lambda":
			return evalLambda(environment, items[1:])
		case "and":
			return evalAnd(environment, items[1:])
		case "or":
			return evalOr(environment, items[1:])
		}
	}

	operatorValue, err := evalExpr(environment, items[0])
	if err != nil {
		return nil, err
	}

	return applyProcedure(environment, operatorValue, items[1:])
}

func evalDefine(environment *env, forms []expr) (expr, error) {
	if len(forms) < 2 {
		return nil, &EvalError{Message: "define expects a name and value"}
	}

	switch target := forms[0].(type) {
	case symbolExpr:
		if len(forms) != 2 {
			return nil, &EvalError{Message: "define expects exactly 2 arguments"}
		}
		value, err := evalExpr(environment, forms[1])
		if err != nil {
			return nil, err
		}
		environment.define(string(target), value)
		return voidExpr{}, nil
	case listExpr:
		if len(target) == 0 {
			return nil, &EvalError{Message: "define function name cannot be empty"}
		}
		name, ok := target[0].(symbolExpr)
		if !ok {
			return nil, &EvalError{Message: "define function name must be a symbol"}
		}
		params, err := parseParamList(target[1:])
		if err != nil {
			return nil, err
		}
		closure := closureExpr{
			params: params,
			body:   append([]expr(nil), forms[1:]...),
			env:    environment,
		}
		environment.define(string(name), closure)
		return voidExpr{}, nil
	default:
		return nil, &EvalError{Message: "define target must be a symbol or parameter list"}
	}
}

func evalIf(environment *env, forms []expr) (expr, error) {
	if len(forms) != 3 {
		return nil, &EvalError{Message: "if expects exactly 3 arguments"}
	}

	condition, err := evalExpr(environment, forms[0])
	if err != nil {
		return nil, err
	}
	if isTruthy(condition) {
		return evalExpr(environment, forms[1])
	}
	return evalExpr(environment, forms[2])
}

func evalQuote(forms []expr) (expr, error) {
	if len(forms) != 1 {
		return nil, &EvalError{Message: "quote expects exactly 1 argument"}
	}
	return forms[0], nil
}

func evalLambda(environment *env, forms []expr) (expr, error) {
	if len(forms) < 2 {
		return nil, &EvalError{Message: "lambda expects parameters and a body"}
	}

	paramList, ok := forms[0].(listExpr)
	if !ok {
		return nil, &EvalError{Message: "lambda parameters must be a list"}
	}

	params, err := parseParamList(paramList)
	if err != nil {
		return nil, err
	}

	return closureExpr{
		params: params,
		body:   append([]expr(nil), forms[1:]...),
		env:    environment,
	}, nil
}

func parseParamList(items []expr) ([]string, error) {
	params := make([]string, 0, len(items))
	for _, item := range items {
		symbol, ok := item.(symbolExpr)
		if !ok {
			return nil, &EvalError{Message: "parameter name must be a symbol"}
		}
		params = append(params, string(symbol))
	}
	return params, nil
}

func applyProcedure(environment *env, proc expr, argForms []expr) (expr, error) {
	args, err := evalArgs(environment, argForms)
	if err != nil {
		return nil, err
	}

	switch callable := proc.(type) {
	case builtinProc:
		return callable.fn(args)
	case closureExpr:
		if len(args) != len(callable.params) {
			return nil, &EvalError{Message: fmt.Sprintf("expected %d arguments, got %d", len(callable.params), len(args))}
		}

		callEnv := &env{
			parent:   callable.env,
			bindings: map[string]expr{},
		}
		for i, name := range callable.params {
			callEnv.define(name, args[i])
		}
		return evalSequence(callEnv, callable.body)
	default:
		return nil, &EvalError{Message: "first list element is not a procedure"}
	}
}

func evalArgs(environment *env, forms []expr) ([]expr, error) {
	args := make([]expr, 0, len(forms))
	for _, form := range forms {
		value, err := evalExpr(environment, form)
		if err != nil {
			return nil, err
		}
		args = append(args, value)
	}
	return args, nil
}

func evalAnd(environment *env, forms []expr) (expr, error) {
	result := expr(boolExpr(true))
	for _, form := range forms {
		value, err := evalExpr(environment, form)
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

func evalOr(environment *env, forms []expr) (expr, error) {
	for _, form := range forms {
		value, err := evalExpr(environment, form)
		if err != nil {
			return nil, err
		}
		if isTruthy(value) {
			return value, nil
		}
	}
	return boolExpr(false), nil
}

func builtinAdd(args []expr) (expr, error) {
	numbers, err := numericArgs(args)
	if err != nil {
		return nil, err
	}

	total := 0
	for _, n := range numbers {
		total += n
	}
	return intExpr(total), nil
}

func builtinSub(args []expr) (expr, error) {
	numbers, err := numericArgs(args)
	if err != nil {
		return nil, err
	}
	if len(numbers) == 0 {
		return nil, &EvalError{Message: "- expects at least 1 argument"}
	}
	if len(numbers) == 1 {
		return intExpr(-numbers[0]), nil
	}

	result := numbers[0]
	for _, n := range numbers[1:] {
		result -= n
	}
	return intExpr(result), nil
}

func builtinMul(args []expr) (expr, error) {
	numbers, err := numericArgs(args)
	if err != nil {
		return nil, err
	}

	result := 1
	for _, n := range numbers {
		result *= n
	}
	return intExpr(result), nil
}

func builtinDiv(args []expr) (expr, error) {
	numbers, err := numericArgs(args)
	if err != nil {
		return nil, err
	}
	if len(numbers) == 0 {
		return nil, &EvalError{Message: "/ expects at least 1 argument"}
	}

	result := numbers[0]
	if len(numbers) == 1 {
		if result == 0 {
			return nil, &EvalError{Message: "division by zero"}
		}
		return intExpr(1 / result), nil
	}

	for _, n := range numbers[1:] {
		if n == 0 {
			return nil, &EvalError{Message: "division by zero"}
		}
		result /= n
	}
	return intExpr(result), nil
}

func builtinNot(args []expr) (expr, error) {
	if len(args) != 1 {
		return nil, &EvalError{Message: "not expects exactly 1 argument"}
	}
	return boolExpr(!isTruthy(args[0])), nil
}

func comparisonBuiltin(name string, cmp func(int, int) bool) builtinFunc {
	return func(args []expr) (expr, error) {
		numbers, err := numericArgs(args)
		if err != nil {
			return nil, err
		}
		if len(numbers) < 2 {
			return nil, &EvalError{Message: fmt.Sprintf("%s expects at least 2 arguments", name)}
		}

		for i := 0; i < len(numbers)-1; i++ {
			if !cmp(numbers[i], numbers[i+1]) {
				return boolExpr(false), nil
			}
		}
		return boolExpr(true), nil
	}
}

func numericArgs(values []expr) ([]int, error) {
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
	case voidExpr:
		return ""
	case builtinProc:
		return "#<procedure:" + v.name + ">"
	case closureExpr:
		return "#<procedure>"
	default:
		return ""
	}
}
