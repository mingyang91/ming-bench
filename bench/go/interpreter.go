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
type symbolExpr struct {
	name string
	pos  sourcePos
}
type listExpr struct {
	items []expr
	pos   sourcePos
}
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
	tokenQuote
	tokenAtom
	tokenString
)

type token struct {
	kind tokenKind
	text string
	pos  sourcePos
}

type parser struct {
	tokens []token
	pos    int
}

func (p sourcePos) advance(r rune) sourcePos {
	if r == '\n' {
		return sourcePos{Line: p.Line + 1, Col: 1}
	}
	return sourcePos{Line: p.Line, Col: p.Col + 1}
}

func formPos(form expr) sourcePos {
	switch v := form.(type) {
	case symbolExpr:
		return v.pos
	case listExpr:
		return v.pos
	default:
		return sourcePos{}
	}
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
		return "", errorAt(sourcePos{Line: 1, Col: 1}, "empty program")
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
	root.define("append", builtinProc{name: "append", fn: builtinAppend})
	root.define("car", builtinProc{name: "car", fn: builtinCar})
	root.define("cdr", builtinProc{name: "cdr", fn: builtinCdr})
	root.define("cons", builtinProc{name: "cons", fn: builtinCons})
	root.define("length", builtinProc{name: "length", fn: builtinLength})
	root.define("list", builtinProc{name: "list", fn: builtinList})
	root.define("null?", builtinProc{name: "null?", fn: builtinNull})
	root.define("boolean?", builtinProc{name: "boolean?", fn: typePredicate(func(value expr) bool {
		_, ok := value.(boolExpr)
		return ok
	})})
	root.define("number?", builtinProc{name: "number?", fn: typePredicate(func(value expr) bool {
		_, ok := value.(intExpr)
		return ok
	})})
	root.define("pair?", builtinProc{name: "pair?", fn: typePredicate(func(value expr) bool {
		list, ok := value.(listExpr)
		return ok && len(list.items) > 0
	})})
	root.define("string?", builtinProc{name: "string?", fn: typePredicate(func(value expr) bool {
		_, ok := value.(stringExpr)
		return ok
	})})
	root.define("symbol?", builtinProc{name: "symbol?", fn: typePredicate(func(value expr) bool {
		_, ok := value.(symbolExpr)
		return ok
	})})

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
	pos := sourcePos{Line: 1, Col: 1}

	for len(input) > 0 {
		r, size := utf8.DecodeRuneInString(input)
		start := pos
		switch {
		case unicode.IsSpace(r):
			input = input[size:]
			pos = pos.advance(r)
		case r == ';':
			input = input[size:]
			pos = pos.advance(r)
			input, pos = skipLineComment(input, pos)
		case r == '(':
			tokens = append(tokens, token{kind: tokenLParen, text: "(", pos: start})
			input = input[size:]
			pos = pos.advance(r)
		case r == ')':
			tokens = append(tokens, token{kind: tokenRParen, text: ")", pos: start})
			input = input[size:]
			pos = pos.advance(r)
		case r == '\'':
			tokens = append(tokens, token{kind: tokenQuote, text: "'", pos: start})
			input = input[size:]
			pos = pos.advance(r)
		case r == '"':
			text, rest, nextPos, err := scanString(input[size:], pos.advance(r), start)
			if err != nil {
				return nil, err
			}
			tokens = append(tokens, token{kind: tokenString, text: text, pos: start})
			input = rest
			pos = nextPos
		default:
			text, rest, nextPos := scanAtom(input, pos)
			tokens = append(tokens, token{kind: tokenAtom, text: text, pos: start})
			input = rest
			pos = nextPos
		}
	}

	return tokens, nil
}

func skipLineComment(input string, pos sourcePos) (string, sourcePos) {
	for len(input) > 0 {
		r, size := utf8.DecodeRuneInString(input)
		input = input[size:]
		pos = pos.advance(r)
		if r == '\n' {
			return input, pos
		}
	}
	return "", pos
}

func scanString(input string, pos sourcePos, start sourcePos) (string, string, sourcePos, error) {
	var b strings.Builder

	for len(input) > 0 {
		r, size := utf8.DecodeRuneInString(input)
		input = input[size:]
		current := pos
		pos = pos.advance(r)

		switch r {
		case '"':
			return b.String(), input, pos, nil
		case '\\':
			if len(input) == 0 {
				return "", "", pos, errorAt(current, "unterminated string escape")
			}
			esc, escSize := utf8.DecodeRuneInString(input)
			input = input[escSize:]
			pos = pos.advance(esc)
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

	return "", "", pos, errorAt(start, "unterminated string literal")
}

func scanAtom(input string, pos sourcePos) (string, string, sourcePos) {
	for i, r := range input {
		if unicode.IsSpace(r) || r == '(' || r == ')' || r == ';' {
			return input[:i], input[i:], pos
		}
		pos = pos.advance(r)
	}
	return input, "", pos
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
		return nil, errorAt(sourcePos{Line: 1, Col: 1}, "unexpected end of input")
	}

	tok := p.tokens[p.pos]
	p.pos++

	switch tok.kind {
	case tokenLParen:
		var items []expr
		for {
			if p.pos >= len(p.tokens) {
				return nil, errorAt(tok.pos, "unterminated list")
			}
			if p.tokens[p.pos].kind == tokenRParen {
				p.pos++
				return listExpr{items: items, pos: tok.pos}, nil
			}
			item, err := p.parseExpr()
			if err != nil {
				return nil, err
			}
			items = append(items, item)
		}
	case tokenRParen:
		return nil, errorAt(tok.pos, "unexpected ')'")
	case tokenQuote:
		quoted, err := p.parseExpr()
		if err != nil {
			return nil, attachPos(err, tok.pos)
		}
		return listExpr{
			items: []expr{
				symbolExpr{name: "quote", pos: tok.pos},
				quoted,
			},
			pos: tok.pos,
		}, nil
	case tokenString:
		return stringExpr(tok.text), nil
	case tokenAtom:
		return parseAtom(tok), nil
	default:
		return nil, errorAt(tok.pos, "unknown token")
	}
}

func parseAtom(tok token) expr {
	switch tok.text {
	case "#t":
		return boolExpr(true)
	case "#f":
		return boolExpr(false)
	}

	if n, err := strconv.Atoi(tok.text); err == nil {
		return intExpr(n)
	}

	return symbolExpr{name: tok.text, pos: tok.pos}
}

func evalSequence(environment *env, forms []expr) (expr, error) {
	result := expr(voidExpr{})
	for _, form := range forms {
		value, err := evalExpr(environment, form)
		if err != nil {
			return nil, attachPos(err, formPos(form))
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
		value, ok := environment.lookup(v.name)
		if !ok {
			return nil, errorAt(v.pos, fmt.Sprintf("unbound symbol: %s", v.name))
		}
		return value, nil
	case listExpr:
		return evalList(environment, v)
	default:
		return nil, &EvalError{Message: "unsupported expression"}
	}
}

func evalList(environment *env, items listExpr) (expr, error) {
	if len(items.items) == 0 {
		return nil, errorAt(items.pos, "cannot evaluate empty list")
	}

	if operator, ok := items.items[0].(symbolExpr); ok {
		switch operator.name {
		case "define":
			value, err := evalDefine(environment, items.items[1:])
			return value, attachPos(err, operator.pos)
		case "if":
			value, err := evalIf(environment, items.items[1:])
			return value, attachPos(err, operator.pos)
		case "begin":
			value, err := evalBegin(environment, items.items[1:])
			return value, attachPos(err, operator.pos)
		case "cond":
			value, err := evalCond(environment, items.items[1:])
			return value, attachPos(err, operator.pos)
		case "let":
			value, err := evalLet(environment, items.items[1:])
			return value, attachPos(err, operator.pos)
		case "quote":
			value, err := evalQuote(items.items[1:])
			return value, attachPos(err, operator.pos)
		case "lambda":
			value, err := evalLambda(environment, items.items[1:])
			return value, attachPos(err, operator.pos)
		case "and":
			value, err := evalAnd(environment, items.items[1:])
			return value, attachPos(err, operator.pos)
		case "or":
			value, err := evalOr(environment, items.items[1:])
			return value, attachPos(err, operator.pos)
		}
	}

	operatorValue, err := evalExpr(environment, items.items[0])
	if err != nil {
		return nil, err
	}

	return applyProcedure(environment, operatorValue, items.items[1:], items.pos)
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
		environment.define(target.name, value)
		return voidExpr{}, nil
	case listExpr:
		if len(target.items) == 0 {
			return nil, &EvalError{Message: "define function name cannot be empty"}
		}
		name, ok := target.items[0].(symbolExpr)
		if !ok {
			return nil, &EvalError{Message: "define function name must be a symbol"}
		}
		params, err := parseParamList(target.items[1:])
		if err != nil {
			return nil, err
		}
		closure := closureExpr{
			params: params,
			body:   append([]expr(nil), forms[1:]...),
			env:    environment,
		}
		environment.define(name.name, closure)
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

func evalBegin(environment *env, forms []expr) (expr, error) {
	if len(forms) == 0 {
		return voidExpr{}, nil
	}
	return evalSequence(environment, forms)
}

func evalCond(environment *env, forms []expr) (expr, error) {
	for i, form := range forms {
		clause, ok := form.(listExpr)
		if !ok || len(clause.items) == 0 {
			return nil, &EvalError{Message: "cond clauses must be non-empty lists"}
		}

		if symbol, ok := clause.items[0].(symbolExpr); ok && symbol.name == "else" {
			if i != len(forms)-1 {
				return nil, &EvalError{Message: "cond else clause must be last"}
			}
			if len(clause.items) == 1 {
				return voidExpr{}, nil
			}
			return evalSequence(environment, clause.items[1:])
		}

		testValue, err := evalExpr(environment, clause.items[0])
		if err != nil {
			return nil, err
		}
		if !isTruthy(testValue) {
			continue
		}
		if len(clause.items) == 1 {
			return testValue, nil
		}
		return evalSequence(environment, clause.items[1:])
	}

	return voidExpr{}, nil
}

func evalQuote(forms []expr) (expr, error) {
	if len(forms) != 1 {
		return nil, &EvalError{Message: "quote expects exactly 1 argument"}
	}
	return forms[0], nil
}

func evalLet(environment *env, forms []expr) (expr, error) {
	if len(forms) < 2 {
		return nil, &EvalError{Message: "let expects bindings and a body"}
	}

	if name, ok := forms[0].(symbolExpr); ok {
		return evalNamedLet(environment, name.name, forms[1:])
	}

	bindings, ok := forms[0].(listExpr)
	if !ok {
		return nil, &EvalError{Message: "let bindings must be a list"}
	}

	names, values, err := evalBindings(environment, bindings)
	if err != nil {
		return nil, err
	}

	letEnv := &env{
		parent:   environment,
		bindings: map[string]expr{},
	}
	for i, name := range names {
		letEnv.define(name, values[i])
	}

	return evalSequence(letEnv, forms[1:])
}

func evalNamedLet(environment *env, name string, forms []expr) (expr, error) {
	if len(forms) < 2 {
		return nil, &EvalError{Message: "named let expects bindings and a body"}
	}

	bindings, ok := forms[0].(listExpr)
	if !ok {
		return nil, &EvalError{Message: "named let bindings must be a list"}
	}

	names, values, err := evalBindings(environment, bindings)
	if err != nil {
		return nil, err
	}

	letEnv := &env{
		parent:   environment,
		bindings: map[string]expr{},
	}
	closure := closureExpr{
		params: append([]string(nil), names...),
		body:   append([]expr(nil), forms[1:]...),
		env:    letEnv,
	}
	letEnv.define(name, closure)

	return applyCallable(closure, values)
}

func evalLambda(environment *env, forms []expr) (expr, error) {
	if len(forms) < 2 {
		return nil, &EvalError{Message: "lambda expects parameters and a body"}
	}

	paramList, ok := forms[0].(listExpr)
	if !ok {
		return nil, &EvalError{Message: "lambda parameters must be a list"}
	}

	params, err := parseParamList(paramList.items)
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
		params = append(params, symbol.name)
	}
	return params, nil
}

func evalBindings(environment *env, bindings listExpr) ([]string, []expr, error) {
	names := make([]string, 0, len(bindings.items))
	values := make([]expr, 0, len(bindings.items))

	for _, binding := range bindings.items {
		pair, ok := binding.(listExpr)
		if !ok || len(pair.items) != 2 {
			return nil, nil, &EvalError{Message: "let bindings must have the form (name value)"}
		}

		name, ok := pair.items[0].(symbolExpr)
		if !ok {
			return nil, nil, &EvalError{Message: "let binding name must be a symbol"}
		}

		value, err := evalExpr(environment, pair.items[1])
		if err != nil {
			return nil, nil, err
		}

		names = append(names, name.name)
		values = append(values, value)
	}

	return names, values, nil
}

func applyProcedure(environment *env, proc expr, argForms []expr, callPos sourcePos) (expr, error) {
	args, err := evalArgs(environment, argForms)
	if err != nil {
		return nil, attachPos(err, callPos)
	}

	value, err := applyCallable(proc, args)
	return value, attachPos(err, callPos)
}

func applyCallable(proc expr, args []expr) (expr, error) {
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

func builtinAppend(args []expr) (expr, error) {
	result := make([]expr, 0)
	for _, arg := range args {
		list, ok := arg.(listExpr)
		if !ok {
			return nil, &EvalError{Message: "append expects list arguments"}
		}
		result = append(result, list.items...)
	}
	return listExpr{items: result}, nil
}

func builtinCar(args []expr) (expr, error) {
	if len(args) != 1 {
		return nil, &EvalError{Message: "car expects exactly 1 argument"}
	}

	list, ok := args[0].(listExpr)
	if !ok || len(list.items) == 0 {
		return nil, &EvalError{Message: "car expects a non-empty list"}
	}
	return list.items[0], nil
}

func builtinCdr(args []expr) (expr, error) {
	if len(args) != 1 {
		return nil, &EvalError{Message: "cdr expects exactly 1 argument"}
	}

	list, ok := args[0].(listExpr)
	if !ok || len(list.items) == 0 {
		return nil, &EvalError{Message: "cdr expects a non-empty list"}
	}
	items := append([]expr(nil), list.items[1:]...)
	return listExpr{items: items}, nil
}

func builtinCons(args []expr) (expr, error) {
	if len(args) != 2 {
		return nil, &EvalError{Message: "cons expects exactly 2 arguments"}
	}

	list, ok := args[1].(listExpr)
	if !ok {
		return nil, &EvalError{Message: "cons expects a list as its second argument"}
	}

	result := make([]expr, 0, len(list.items)+1)
	result = append(result, args[0])
	result = append(result, list.items...)
	return listExpr{items: result}, nil
}

func builtinLength(args []expr) (expr, error) {
	if len(args) != 1 {
		return nil, &EvalError{Message: "length expects exactly 1 argument"}
	}

	list, ok := args[0].(listExpr)
	if !ok {
		return nil, &EvalError{Message: "length expects a list"}
	}
	return intExpr(len(list.items)), nil
}

func builtinList(args []expr) (expr, error) {
	result := make([]expr, len(args))
	copy(result, args)
	return listExpr{items: result}, nil
}

func builtinNull(args []expr) (expr, error) {
	if len(args) != 1 {
		return nil, &EvalError{Message: "null? expects exactly 1 argument"}
	}

	list, ok := args[0].(listExpr)
	return boolExpr(ok && len(list.items) == 0), nil
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

func typePredicate(test func(expr) bool) builtinFunc {
	return func(args []expr) (expr, error) {
		if len(args) != 1 {
			return nil, &EvalError{Message: "predicate expects exactly 1 argument"}
		}
		return boolExpr(test(args[0])), nil
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
		return v.name
	case listExpr:
		parts := make([]string, 0, len(v.items))
		for _, item := range v.items {
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
