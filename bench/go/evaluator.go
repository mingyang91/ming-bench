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
		return "", ensureSourcePos(err)
	}
	return formatValue(result), nil
}

// EvalStrWithOutput evaluates Scheme expressions and returns both the result
// string and any captured output from display/write/newline.
func EvalStrWithOutput(input string) (result string, output string, err error) {
	value, output, err := evalStrInternal(input)
	if err != nil {
		return "", output, ensureSourcePos(err)
	}
	return formatValue(value), output, nil
}

type stringExpr struct {
	value string
	pos   sourcePos
}

type symbolExpr struct {
	name string
	pos  sourcePos
}

type listExpr struct {
	elements []any
	pos      sourcePos
}

type sourcePos struct {
	line int
	col  int
}

type pairValue struct {
	car any
	cdr any
}

type emptyListValue struct{}

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
	scope.define("cons", builtinProc{name: "cons", fn: builtinCons})
	scope.define("car", builtinProc{name: "car", fn: builtinCar})
	scope.define("cdr", builtinProc{name: "cdr", fn: builtinCdr})
	scope.define("null?", builtinProc{name: "null?", fn: func(args []any) (any, error) {
		return builtinPredicate("null?", args, func(value any) bool {
			_, ok := value.(emptyListValue)
			return ok
		})
	}})
	scope.define("append", builtinProc{name: "append", fn: builtinAppend})
	scope.define("list", builtinProc{name: "list", fn: builtinList})
	scope.define("length", builtinProc{name: "length", fn: builtinLength})
	scope.define("string?", builtinProc{name: "string?", fn: func(args []any) (any, error) {
		return builtinPredicate("string?", args, func(value any) bool {
			_, ok := value.(string)
			return ok
		})
	}})
	scope.define("number?", builtinProc{name: "number?", fn: func(args []any) (any, error) {
		return builtinPredicate("number?", args, func(value any) bool {
			_, ok := value.(int64)
			return ok
		})
	}})
	scope.define("boolean?", builtinProc{name: "boolean?", fn: func(args []any) (any, error) {
		return builtinPredicate("boolean?", args, func(value any) bool {
			_, ok := value.(bool)
			return ok
		})
	}})
	scope.define("pair?", builtinProc{name: "pair?", fn: func(args []any) (any, error) {
		return builtinPredicate("pair?", args, func(value any) bool {
			_, ok := value.(pairValue)
			return ok
		})
	}})
	scope.define("symbol?", builtinProc{name: "symbol?", fn: func(args []any) (any, error) {
		return builtinPredicate("symbol?", args, func(value any) bool {
			_, ok := value.(symbolExpr)
			return ok
		})
	}})
	return scope
}

func evalStrInternal(input string) (any, string, error) {
	p := parser{input: input}
	exprs, err := p.parseProgram()
	if err != nil {
		return nil, "", err
	}
	if len(exprs) == 0 {
		return nil, "", sourcePos{line: 1, col: 1}.errorf("empty input")
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
		return nil, p.currentPos().errorf("unexpected end of input")
	}

	switch p.peek() {
	case '(':
		return p.parseList()
	case '"':
		return p.parseString()
	case '\'':
		return p.parseQuoteShorthand()
	default:
		return p.parseAtom()
	}
}

func (p *parser) parseQuoteShorthand() (any, error) {
	start := p.pos
	p.pos++

	expr, err := p.parseExpr()
	if err != nil {
		return nil, err
	}

	pos := p.posAt(start)
	return listExpr{
		elements: []any{
			symbolExpr{name: "quote", pos: pos},
			expr,
		},
		pos: pos,
	}, nil
}

func (p *parser) parseList() (any, error) {
	start := p.pos
	p.pos++

	var elements []any
	for {
		p.skipIgnorable()
		if p.atEnd() {
			return nil, p.posAt(start).errorf("unterminated list")
		}
		if p.peek() == ')' {
			p.pos++
			return listExpr{elements: elements, pos: p.posAt(start)}, nil
		}

		expr, err := p.parseExpr()
		if err != nil {
			return nil, err
		}
		elements = append(elements, expr)
	}
}

func (p *parser) parseString() (any, error) {
	start := p.pos
	p.pos++

	var b strings.Builder
	for !p.atEnd() {
		ch := p.peek()
		p.pos++

		switch ch {
		case '"':
			return stringExpr{value: b.String(), pos: p.posAt(start)}, nil
		case '\\':
			if p.atEnd() {
				return nil, p.posAt(start).errorf("unterminated string")
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

	return nil, p.posAt(start).errorf("unterminated string")
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
		return nil, p.posAt(start).errorf("unexpected token")
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

	return symbolExpr{name: token, pos: p.posAt(start)}, nil
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

func (p *parser) currentPos() sourcePos {
	return p.posAt(p.pos)
}

func (p *parser) posAt(offset int) sourcePos {
	if offset < 0 {
		offset = 0
	}
	if offset > len(p.input) {
		offset = len(p.input)
	}

	line := 1
	col := 1
	for i := 0; i < offset; i++ {
		if p.input[i] == '\n' {
			line++
			col = 1
			continue
		}
		col++
	}

	return sourcePos{line: line, col: col}
}

func (pos sourcePos) errorf(format string, args ...any) *EvalError {
	return &EvalError{
		Message: fmt.Sprintf(format, args...),
		Line:    pos.line,
		Col:     pos.col,
	}
}

func ensureSourcePos(err error) error {
	return attachSourcePos(err, sourcePos{line: 1, col: 1})
}

func attachSourcePos(err error, pos sourcePos) error {
	if err == nil {
		return nil
	}

	evalErr, ok := err.(*EvalError)
	if !ok {
		return pos.errorf("%s", err.Error())
	}
	if evalErr.Line > 0 && evalErr.Col > 0 {
		return err
	}
	return &EvalError{
		Message: evalErr.Message,
		Line:    pos.line,
		Col:     pos.col,
	}
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
			return nil, node.pos.errorf("unbound variable: %s", node.name)
		}
		return value, nil
	case listExpr:
		value, err := evalList(scope, node)
		if err != nil {
			return nil, attachSourcePos(err, node.pos)
		}
		return value, nil
	case string:
		return node, nil
	case builtinProc:
		return node, nil
	case closure:
		return node, nil
	case pairValue:
		return node, nil
	case emptyListValue:
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
		case "begin":
			return evalBegin(scope, args)
		case "let":
			return evalLet(scope, args)
		case "cond":
			return evalCond(scope, args)
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

func evalBegin(scope *env, args []any) (any, error) {
	return evalSequence(scope, args)
}

func evalLet(scope *env, args []any) (any, error) {
	if len(args) < 2 {
		return nil, &EvalError{Message: "let expects bindings and a body"}
	}

	if name, ok := args[0].(symbolExpr); ok {
		if len(args) < 3 {
			return nil, &EvalError{Message: "named let expects bindings and a body"}
		}

		bindingsExpr, ok := args[1].(listExpr)
		if !ok {
			return nil, &EvalError{Message: "let bindings must be a list"}
		}

		params, values, err := evalBindings(scope, bindingsExpr.elements)
		if err != nil {
			return nil, err
		}

		letScope := newEnv(scope)
		proc := closure{
			params: params,
			body:   args[2:],
			env:    letScope,
		}
		letScope.define(name.name, proc)
		return applyProcedure(proc, values)
	}

	bindingsExpr, ok := args[0].(listExpr)
	if !ok {
		return nil, &EvalError{Message: "let bindings must be a list"}
	}

	params, values, err := evalBindings(scope, bindingsExpr.elements)
	if err != nil {
		return nil, err
	}

	letScope := newEnv(scope)
	for i, param := range params {
		letScope.define(param, values[i])
	}

	return evalSequence(letScope, args[1:])
}

func evalCond(scope *env, args []any) (any, error) {
	for i, clauseExpr := range args {
		clause, ok := clauseExpr.(listExpr)
		if !ok || len(clause.elements) == 0 {
			return nil, &EvalError{Message: "cond clauses must be non-empty lists"}
		}

		if symbol, ok := clause.elements[0].(symbolExpr); ok && symbol.name == "else" {
			if i != len(args)-1 {
				return nil, &EvalError{Message: "cond else clause must be last"}
			}
			if len(clause.elements) == 1 {
				return voidValue{}, nil
			}
			return evalSequence(scope, clause.elements[1:])
		}

		testValue, err := eval(scope, clause.elements[0])
		if err != nil {
			return nil, err
		}
		if !isTruthy(testValue) {
			continue
		}
		if len(clause.elements) == 1 {
			return testValue, nil
		}
		return evalSequence(scope, clause.elements[1:])
	}

	return voidValue{}, nil
}

func evalBindings(scope *env, bindings []any) ([]string, []any, error) {
	names := make([]string, 0, len(bindings))
	values := make([]any, 0, len(bindings))
	for _, bindingExpr := range bindings {
		binding, ok := bindingExpr.(listExpr)
		if !ok || len(binding.elements) != 2 {
			return nil, nil, &EvalError{Message: "let bindings must be name/value pairs"}
		}

		name, ok := binding.elements[0].(symbolExpr)
		if !ok {
			return nil, nil, &EvalError{Message: "let binding name must be a symbol"}
		}

		value, err := eval(scope, binding.elements[1])
		if err != nil {
			return nil, nil, err
		}

		names = append(names, name.name)
		values = append(values, value)
	}
	return names, values, nil
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

func builtinPredicate(name string, args []any, pred func(any) bool) (any, error) {
	if len(args) != 1 {
		return nil, &EvalError{Message: fmt.Sprintf("%s expects exactly 1 argument", name)}
	}
	return pred(args[0]), nil
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

func builtinCons(args []any) (any, error) {
	if len(args) != 2 {
		return nil, &EvalError{Message: "cons expects exactly 2 arguments"}
	}
	return pairValue{car: args[0], cdr: args[1]}, nil
}

func builtinCar(args []any) (any, error) {
	if len(args) != 1 {
		return nil, &EvalError{Message: "car expects exactly 1 argument"}
	}

	pair, ok := args[0].(pairValue)
	if !ok {
		return nil, &EvalError{Message: fmt.Sprintf("car expects a pair, got %s", typeName(args[0]))}
	}
	return pair.car, nil
}

func builtinCdr(args []any) (any, error) {
	if len(args) != 1 {
		return nil, &EvalError{Message: "cdr expects exactly 1 argument"}
	}

	pair, ok := args[0].(pairValue)
	if !ok {
		return nil, &EvalError{Message: fmt.Sprintf("cdr expects a pair, got %s", typeName(args[0]))}
	}
	return pair.cdr, nil
}

func builtinList(args []any) (any, error) {
	return makeListValue(args), nil
}

func builtinLength(args []any) (any, error) {
	if len(args) != 1 {
		return nil, &EvalError{Message: "length expects exactly 1 argument"}
	}

	var length int64
	current := args[0]
	for {
		switch value := current.(type) {
		case emptyListValue:
			return length, nil
		case pairValue:
			length++
			current = value.cdr
		default:
			return nil, &EvalError{Message: fmt.Sprintf("length expects a list, got %s", typeName(args[0]))}
		}
	}
}

func builtinAppend(args []any) (any, error) {
	if len(args) == 0 {
		return emptyListValue{}, nil
	}
	if len(args) == 1 {
		return args[0], nil
	}

	result := args[len(args)-1]
	for i := len(args) - 2; i >= 0; i-- {
		elements, err := properListElements(args[i], "append")
		if err != nil {
			return nil, err
		}
		for j := len(elements) - 1; j >= 0; j-- {
			result = pairValue{car: elements[j], cdr: result}
		}
	}

	return result, nil
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
		return makeListValue(elements)
	default:
		return expr
	}
}

func makeListValue(elements []any) any {
	result := any(emptyListValue{})
	for i := len(elements) - 1; i >= 0; i-- {
		result = pairValue{car: elements[i], cdr: result}
	}
	return result
}

func properListElements(value any, builtinName string) ([]any, error) {
	var elements []any
	current := value
	for {
		switch list := current.(type) {
		case emptyListValue:
			return elements, nil
		case pairValue:
			elements = append(elements, list.car)
			current = list.cdr
		default:
			return nil, &EvalError{Message: fmt.Sprintf("%s expects a list, got %s", builtinName, typeName(value))}
		}
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
	case pairValue:
		return "pair"
	case emptyListValue:
		return "list"
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
	case emptyListValue:
		return "()"
	case pairValue:
		return formatPairValue(v)
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

func formatPairValue(pair pairValue) string {
	parts := []string{formatValue(pair.car)}
	current := pair.cdr
	for {
		switch value := current.(type) {
		case emptyListValue:
			return "(" + strings.Join(parts, " ") + ")"
		case pairValue:
			parts = append(parts, formatValue(value.car))
			current = value.cdr
		default:
			return "(" + strings.Join(parts, " ") + " . " + formatValue(value) + ")"
		}
	}
}
