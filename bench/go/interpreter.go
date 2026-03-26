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
	tokenQuote
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

type charExpr struct {
	value rune
	pos   position
}

func (e *charExpr) exprPos() position { return e.pos }

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
type symbolValue string
type charValue rune
type emptyList struct{}
type voidValue struct{}

type mutableString struct {
	runes []rune
}

func newMutableString(text string) *mutableString {
	runes := []rune(text)
	return &mutableString{runes: append([]rune(nil), runes...)}
}

func (s *mutableString) String() string {
	return string(s.runes)
}

type pairValue struct {
	car any
	cdr any
}

type callable interface {
	Call(*interpreter, []any, position) (any, error)
}

type builtinProcedure struct {
	name string
	fn   func(*interpreter, []any, position) (any, error)
}

func (p *builtinProcedure) Call(i *interpreter, args []any, pos position) (any, error) {
	return p.fn(i, args, pos)
}

type lambdaProcedure struct {
	params   []string
	restName string
	hasRest  bool
	body     []expr
	env      *environment
}

func (p *lambdaProcedure) Call(i *interpreter, args []any, pos position) (any, error) {
	if !p.hasRest && len(args) != len(p.params) {
		return nil, newEvalError(pos, "wrong number of arguments: expected %d, got %d", len(p.params), len(args))
	}
	if p.hasRest && len(args) < len(p.params) {
		return nil, newEvalError(pos, "wrong number of arguments: expected at least %d, got %d", len(p.params), len(args))
	}

	callEnv := newEnvironment(p.env)
	for index, name := range p.params {
		callEnv.define(name, args[index])
	}
	if p.hasRest {
		callEnv.define(p.restName, buildList(args[len(p.params):]))
	}

	return i.evalSequence(p.body, callEnv)
}

type environment struct {
	parent *environment
	values map[string]any
}

func newEnvironment(parent *environment) *environment {
	return &environment{
		parent: parent,
		values: map[string]any{},
	}
}

func (e *environment) define(name string, value any) {
	e.values[name] = value
}

func (e *environment) lookup(name string) (any, bool) {
	for current := e; current != nil; current = current.parent {
		if value, ok := current.values[name]; ok {
			return value, true
		}
	}
	return nil, false
}

func (e *environment) assign(name string, value any) bool {
	for current := e; current != nil; current = current.parent {
		if _, ok := current.values[name]; ok {
			current.values[name] = value
			return true
		}
	}
	return false
}

type interpreter struct {
	output strings.Builder
	global *environment
}

func newInterpreter() *interpreter {
	global := newEnvironment(nil)
	installBuiltins(global)
	return &interpreter{global: global}
}

func evalInput(input string) (string, string, error) {
	parsed, err := parseProgram(input)
	if err != nil {
		return "", "", err
	}

	intp := newInterpreter()
	result := any(voidValue{})

	for _, expression := range parsed {
		result, err = intp.eval(expression, intp.global)
		if err != nil {
			return "", intp.output.String(), err
		}
	}

	return formatValue(result), intp.output.String(), nil
}

func installBuiltins(env *environment) {
	for _, name := range []string{
		"+", "-", "*", "/", "<", ">", "=", "<=", "not",
		"cons", "car", "cdr", "null?", "list", "length", "append",
		"string?", "number?", "boolean?", "pair?", "symbol?",
		"apply", "eq?", "equal?",
		"display", "write", "newline",
		"string-append", "string-length", "substring",
		"string->number", "number->string",
		"symbol->string", "string->symbol",
		"string-ref", "string-copy", "string-set!", "char?",
		"abs", "modulo", "remainder", "quotient", "min", "max", "expt",
		"zero?", "positive?", "negative?", "odd?", "even?",
		"list-ref", "list-tail", "list?", "assoc", "map",
		"char-alphabetic?", "char-numeric?", "char-upcase", "char-downcase", "char=?", "char<?",
		"string=?", "string<?", "string-ci=?", "string-upcase", "string-downcase",
	} {
		name := name
		env.define(name, &builtinProcedure{
			name: name,
			fn: func(i *interpreter, args []any, pos position) (any, error) {
				return applyBuiltin(i, name, args, pos)
			},
		})
	}
}

func parseProgram(input string) ([]expr, error) {
	tokens, err := lex(input)
	if err != nil {
		return nil, err
	}

	parser := &tokenParser{tokens: tokens}
	return parser.parseProgram()
}

func (i *interpreter) eval(expression expr, env *environment) (any, error) {
	switch e := expression.(type) {
	case *integerExpr:
		return e.value, nil
	case *booleanExpr:
		return e.value, nil
	case *stringExpr:
		return stringValue(e.value), nil
	case *charExpr:
		return charValue(e.value), nil
	case *symbolExpr:
		value, ok := env.lookup(e.value)
		if !ok {
			return nil, newEvalError(e.pos, "unbound variable: %s", e.value)
		}
		return value, nil
	case *listExpr:
		return i.evalList(e, env)
	default:
		return nil, newEvalError(expression.exprPos(), "internal error: unknown expression")
	}
}

func (i *interpreter) evalList(list *listExpr, env *environment) (any, error) {
	if len(list.elements) == 0 {
		return nil, newEvalError(list.pos, "cannot evaluate empty list")
	}

	if operator, ok := list.elements[0].(*symbolExpr); ok {
		switch operator.value {
		case "and":
			return i.evalAnd(list.elements[1:], env)
		case "or":
			return i.evalOr(list.elements[1:], env)
		case "begin":
			return i.evalBegin(list.elements[1:], env)
		case "if":
			return i.evalIf(list.elements[1:], operator.pos, env)
		case "cond":
			return i.evalCond(list.elements[1:], operator.pos, env)
		case "define":
			return i.evalDefine(list.elements[1:], operator.pos, env)
		case "set!":
			return i.evalSet(list.elements[1:], operator.pos, env)
		case "let":
			return i.evalLet(list.elements[1:], operator.pos, env)
		case "quote":
			return i.evalQuote(list.elements[1:], operator.pos)
		case "lambda":
			return i.evalLambda(list.elements[1:], operator.pos, env)
		}
	}

	operatorValue, err := i.eval(list.elements[0], env)
	if err != nil {
		return nil, err
	}

	args := make([]any, 0, len(list.elements)-1)
	for _, argExpr := range list.elements[1:] {
		arg, err := i.eval(argExpr, env)
		if err != nil {
			return nil, err
		}
		args = append(args, arg)
	}

	return applyProcedure(i, operatorValue, args, list.elements[0].exprPos())
}

func (i *interpreter) evalSequence(expressions []expr, env *environment) (any, error) {
	result := any(voidValue{})
	for _, expression := range expressions {
		var err error
		result, err = i.eval(expression, env)
		if err != nil {
			return nil, err
		}
	}
	return result, nil
}

func (i *interpreter) evalAnd(args []expr, env *environment) (any, error) {
	if len(args) == 0 {
		return true, nil
	}

	result := any(true)
	for _, argExpr := range args {
		value, err := i.eval(argExpr, env)
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

func (i *interpreter) evalOr(args []expr, env *environment) (any, error) {
	for _, argExpr := range args {
		value, err := i.eval(argExpr, env)
		if err != nil {
			return nil, err
		}
		if isTruthy(value) {
			return value, nil
		}
	}

	return false, nil
}

func (i *interpreter) evalBegin(args []expr, env *environment) (any, error) {
	return i.evalSequence(args, env)
}

func (i *interpreter) evalIf(args []expr, pos position, env *environment) (any, error) {
	if len(args) < 2 || len(args) > 3 {
		return nil, newEvalError(pos, "if expects 2 or 3 arguments")
	}

	condition, err := i.eval(args[0], env)
	if err != nil {
		return nil, err
	}

	if isTruthy(condition) {
		return i.eval(args[1], env)
	}
	if len(args) == 3 {
		return i.eval(args[2], env)
	}
	return voidValue{}, nil
}

func (i *interpreter) evalCond(args []expr, pos position, env *environment) (any, error) {
	for index, clauseExpr := range args {
		clause, ok := clauseExpr.(*listExpr)
		if !ok || len(clause.elements) == 0 {
			return nil, newEvalError(clauseExpr.exprPos(), "cond clauses must be non-empty lists")
		}

		if symbol, ok := clause.elements[0].(*symbolExpr); ok && symbol.value == "else" {
			if index != len(args)-1 {
				return nil, newEvalError(symbol.pos, "cond else clause must be last")
			}
			return i.evalSequence(clause.elements[1:], env)
		}

		testValue, err := i.eval(clause.elements[0], env)
		if err != nil {
			return nil, err
		}
		if isTruthy(testValue) {
			if len(clause.elements) == 1 {
				return testValue, nil
			}
			return i.evalSequence(clause.elements[1:], env)
		}
	}

	return voidValue{}, nil
}

func (i *interpreter) evalDefine(args []expr, pos position, env *environment) (any, error) {
	if len(args) < 2 {
		return nil, newEvalError(pos, "define expects at least 2 arguments")
	}

	switch target := args[0].(type) {
	case *symbolExpr:
		if len(args) != 2 {
			return nil, newEvalError(pos, "define expects exactly 2 arguments")
		}
		value, err := i.eval(args[1], env)
		if err != nil {
			return nil, err
		}
		env.define(target.value, value)
		return voidValue{}, nil

	case *listExpr:
		if len(target.elements) == 0 {
			return nil, newEvalError(target.pos, "define requires a function name")
		}

		name, ok := target.elements[0].(*symbolExpr)
		if !ok {
			return nil, newEvalError(target.elements[0].exprPos(), "define requires a symbol name")
		}

		params, err := parseParameterExprs(target.elements[1:])
		if err != nil {
			return nil, err
		}

		procedure := &lambdaProcedure{
			params:   params.required,
			restName: params.restName,
			hasRest:  params.hasRest,
			body:     args[1:],
			env:      env,
		}
		env.define(name.value, procedure)
		return voidValue{}, nil

	default:
		return nil, newEvalError(args[0].exprPos(), "define requires a symbol or parameter list")
	}
}

func (i *interpreter) evalSet(args []expr, pos position, env *environment) (any, error) {
	if len(args) != 2 {
		return nil, newEvalError(pos, "set! expects exactly 2 arguments")
	}

	name, ok := args[0].(*symbolExpr)
	if !ok {
		return nil, newEvalError(args[0].exprPos(), "set! requires a symbol name")
	}

	value, err := i.eval(args[1], env)
	if err != nil {
		return nil, err
	}

	if !env.assign(name.value, value) {
		return nil, newEvalError(name.pos, "unbound variable: %s", name.value)
	}

	return voidValue{}, nil
}

func (i *interpreter) evalQuote(args []expr, pos position) (any, error) {
	if len(args) != 1 {
		return nil, newEvalError(pos, "quote expects exactly 1 argument")
	}
	return datumFromExpr(args[0])
}

func (i *interpreter) evalLet(args []expr, pos position, env *environment) (any, error) {
	if len(args) < 2 {
		return nil, newEvalError(pos, "let expects bindings and a body")
	}

	if name, ok := args[0].(*symbolExpr); ok {
		return i.evalNamedLet(name, args[1:], pos, env)
	}

	return i.evalPlainLet(args, pos, env)
}

func (i *interpreter) evalNamedLet(name *symbolExpr, args []expr, pos position, env *environment) (any, error) {
	if len(args) < 2 {
		return nil, newEvalError(pos, "named let expects bindings and a body")
	}

	bindings, err := parseLetBindings(args[0])
	if err != nil {
		return nil, err
	}

	params := make([]string, 0, len(bindings))
	values := make([]any, 0, len(bindings))
	for _, binding := range bindings {
		value, err := i.eval(binding.valueExpr, env)
		if err != nil {
			return nil, err
		}
		params = append(params, binding.name)
		values = append(values, value)
	}

	letEnv := newEnvironment(env)
	procedure := &lambdaProcedure{
		params: params,
		body:   args[1:],
		env:    letEnv,
	}
	letEnv.define(name.value, procedure)

	return procedure.Call(i, values, name.pos)
}

func (i *interpreter) evalPlainLet(args []expr, _ position, env *environment) (any, error) {
	bindings, err := parseLetBindings(args[0])
	if err != nil {
		return nil, err
	}

	letEnv := newEnvironment(env)
	for _, binding := range bindings {
		value, err := i.eval(binding.valueExpr, env)
		if err != nil {
			return nil, err
		}
		letEnv.define(binding.name, value)
	}

	return i.evalSequence(args[1:], letEnv)
}

func (i *interpreter) evalLambda(args []expr, pos position, env *environment) (any, error) {
	if len(args) < 2 {
		return nil, newEvalError(pos, "lambda expects parameters and a body")
	}

	params, err := parseLambdaParameters(args[0])
	if err != nil {
		return nil, err
	}

	return &lambdaProcedure{
		params:   params.required,
		restName: params.restName,
		hasRest:  params.hasRest,
		body:     args[1:],
		env:      env,
	}, nil
}

type letBinding struct {
	name      string
	valueExpr expr
}

type parameterSpec struct {
	required []string
	restName string
	hasRest  bool
}

func parseLetBindings(expression expr) ([]letBinding, error) {
	bindingsList, ok := expression.(*listExpr)
	if !ok {
		return nil, newEvalError(expression.exprPos(), "let bindings must be a list")
	}

	bindings := make([]letBinding, 0, len(bindingsList.elements))
	for _, bindingExpr := range bindingsList.elements {
		binding, ok := bindingExpr.(*listExpr)
		if !ok || len(binding.elements) != 2 {
			return nil, newEvalError(bindingExpr.exprPos(), "let bindings must contain a name and value")
		}

		name, ok := binding.elements[0].(*symbolExpr)
		if !ok {
			return nil, newEvalError(binding.elements[0].exprPos(), "let binding name must be a symbol")
		}

		bindings = append(bindings, letBinding{
			name:      name.value,
			valueExpr: binding.elements[1],
		})
	}

	return bindings, nil
}

func parseLambdaParameters(expression expr) (parameterSpec, error) {
	switch params := expression.(type) {
	case *listExpr:
		return parseParameterExprs(params.elements)
	case *symbolExpr:
		return parameterSpec{
			restName: params.value,
			hasRest:  true,
		}, nil
	default:
		return parameterSpec{}, newEvalError(expression.exprPos(), "lambda parameters must be a list or symbol")
	}
}

func parseParameterExprs(expressions []expr) (parameterSpec, error) {
	spec := parameterSpec{
		required: make([]string, 0, len(expressions)),
	}
	sawDot := false

	for index, expression := range expressions {
		symbol, ok := expression.(*symbolExpr)
		if !ok {
			return parameterSpec{}, newEvalError(expression.exprPos(), "parameter name must be a symbol")
		}

		if symbol.value == "." {
			if sawDot || index == len(expressions)-1 {
				return parameterSpec{}, newEvalError(symbol.pos, "invalid dotted parameter list")
			}
			sawDot = true
			continue
		}

		if sawDot {
			if index != len(expressions)-1 {
				return parameterSpec{}, newEvalError(expression.exprPos(), "invalid dotted parameter list")
			}
			spec.restName = symbol.value
			spec.hasRest = true
			continue
		}

		spec.required = append(spec.required, symbol.value)
	}

	if sawDot && !spec.hasRest {
		return parameterSpec{}, newEvalError(expressions[len(expressions)-1].exprPos(), "invalid dotted parameter list")
	}

	return spec, nil
}

func datumFromExpr(expression expr) (any, error) {
	switch e := expression.(type) {
	case *integerExpr:
		return e.value, nil
	case *booleanExpr:
		return e.value, nil
	case *stringExpr:
		return stringValue(e.value), nil
	case *charExpr:
		return charValue(e.value), nil
	case *symbolExpr:
		return symbolValue(e.value), nil
	case *listExpr:
		return datumList(e.elements)
	default:
		return nil, newEvalError(expression.exprPos(), "unsupported quoted form")
	}
}

func datumList(elements []expr) (any, error) {
	result := any(emptyList{})
	for index := len(elements) - 1; index >= 0; index-- {
		value, err := datumFromExpr(elements[index])
		if err != nil {
			return nil, err
		}
		result = &pairValue{car: value, cdr: result}
	}
	return result, nil
}

func applyProcedure(i *interpreter, operator any, args []any, pos position) (any, error) {
	procedure, ok := operator.(callable)
	if !ok {
		return nil, newEvalError(pos, "attempt to call non-procedure")
	}
	return procedure.Call(i, args, pos)
}

func applyBuiltin(i *interpreter, name string, args []any, pos position) (any, error) {
	switch name {
	case "apply":
		if len(args) < 2 {
			return nil, newEvalError(pos, "%s expects at least 2 arguments", name)
		}
		listArgs, err := listElements(args[len(args)-1], pos, name)
		if err != nil {
			return nil, err
		}
		callArgs := make([]any, 0, len(args)-2+len(listArgs))
		callArgs = append(callArgs, args[1:len(args)-1]...)
		callArgs = append(callArgs, listArgs...)
		return applyProcedure(i, args[0], callArgs, pos)

	case "eq?":
		if len(args) != 2 {
			return nil, newEvalError(pos, "%s expects exactly 2 arguments", name)
		}
		return eqValues(args[0], args[1]), nil

	case "equal?":
		if len(args) != 2 {
			return nil, newEvalError(pos, "%s expects exactly 2 arguments", name)
		}
		return equalValues(args[0], args[1]), nil

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

	case "abs":
		if len(args) != 1 {
			return nil, newEvalError(pos, "%s expects exactly 1 argument", name)
		}
		value, err := expectInt(args[0], pos, name)
		if err != nil {
			return nil, err
		}
		if value < 0 {
			return -value, nil
		}
		return value, nil

	case "quotient":
		if len(args) != 2 {
			return nil, newEvalError(pos, "%s expects exactly 2 arguments", name)
		}
		dividend, err := expectInt(args[0], pos, name)
		if err != nil {
			return nil, err
		}
		divisor, err := expectInt(args[1], pos, name)
		if err != nil {
			return nil, err
		}
		if divisor == 0 {
			return nil, newEvalError(pos, "division by zero")
		}
		return dividend / divisor, nil

	case "remainder":
		if len(args) != 2 {
			return nil, newEvalError(pos, "%s expects exactly 2 arguments", name)
		}
		dividend, err := expectInt(args[0], pos, name)
		if err != nil {
			return nil, err
		}
		divisor, err := expectInt(args[1], pos, name)
		if err != nil {
			return nil, err
		}
		if divisor == 0 {
			return nil, newEvalError(pos, "division by zero")
		}
		return dividend % divisor, nil

	case "modulo":
		if len(args) != 2 {
			return nil, newEvalError(pos, "%s expects exactly 2 arguments", name)
		}
		dividend, err := expectInt(args[0], pos, name)
		if err != nil {
			return nil, err
		}
		divisor, err := expectInt(args[1], pos, name)
		if err != nil {
			return nil, err
		}
		if divisor == 0 {
			return nil, newEvalError(pos, "division by zero")
		}
		remainder := dividend % divisor
		if remainder != 0 && ((remainder > 0 && divisor < 0) || (remainder < 0 && divisor > 0)) {
			remainder += divisor
		}
		return remainder, nil

	case "min":
		if len(args) == 0 {
			return nil, newEvalError(pos, "%s expects at least 1 argument", name)
		}
		best, err := expectInt(args[0], pos, name)
		if err != nil {
			return nil, err
		}
		for _, arg := range args[1:] {
			value, err := expectInt(arg, pos, name)
			if err != nil {
				return nil, err
			}
			if value < best {
				best = value
			}
		}
		return best, nil

	case "max":
		if len(args) == 0 {
			return nil, newEvalError(pos, "%s expects at least 1 argument", name)
		}
		best, err := expectInt(args[0], pos, name)
		if err != nil {
			return nil, err
		}
		for _, arg := range args[1:] {
			value, err := expectInt(arg, pos, name)
			if err != nil {
				return nil, err
			}
			if value > best {
				best = value
			}
		}
		return best, nil

	case "expt":
		if len(args) != 2 {
			return nil, newEvalError(pos, "%s expects exactly 2 arguments", name)
		}
		base, err := expectInt(args[0], pos, name)
		if err != nil {
			return nil, err
		}
		exponent, err := expectInt(args[1], pos, name)
		if err != nil {
			return nil, err
		}
		if exponent < 0 {
			return nil, newEvalError(pos, "%s expects a non-negative exponent", name)
		}
		result := 1
		for exponent > 0 {
			if exponent%2 == 1 {
				result *= base
			}
			exponent /= 2
			if exponent > 0 {
				base *= base
			}
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

	case "zero?":
		if len(args) != 1 {
			return nil, newEvalError(pos, "%s expects exactly 1 argument", name)
		}
		value, err := expectInt(args[0], pos, name)
		if err != nil {
			return nil, err
		}
		return value == 0, nil

	case "positive?":
		if len(args) != 1 {
			return nil, newEvalError(pos, "%s expects exactly 1 argument", name)
		}
		value, err := expectInt(args[0], pos, name)
		if err != nil {
			return nil, err
		}
		return value > 0, nil

	case "negative?":
		if len(args) != 1 {
			return nil, newEvalError(pos, "%s expects exactly 1 argument", name)
		}
		value, err := expectInt(args[0], pos, name)
		if err != nil {
			return nil, err
		}
		return value < 0, nil

	case "odd?":
		if len(args) != 1 {
			return nil, newEvalError(pos, "%s expects exactly 1 argument", name)
		}
		value, err := expectInt(args[0], pos, name)
		if err != nil {
			return nil, err
		}
		return value%2 != 0, nil

	case "even?":
		if len(args) != 1 {
			return nil, newEvalError(pos, "%s expects exactly 1 argument", name)
		}
		value, err := expectInt(args[0], pos, name)
		if err != nil {
			return nil, err
		}
		return value%2 == 0, nil
	case "not":
		if len(args) != 1 {
			return nil, newEvalError(pos, "%s expects exactly 1 argument", name)
		}
		return !isTruthy(args[0]), nil

	case "cons":
		if len(args) != 2 {
			return nil, newEvalError(pos, "%s expects exactly 2 arguments", name)
		}
		return &pairValue{car: args[0], cdr: args[1]}, nil

	case "car":
		if len(args) != 1 {
			return nil, newEvalError(pos, "%s expects exactly 1 argument", name)
		}
		pair, err := expectPair(args[0], pos, name)
		if err != nil {
			return nil, err
		}
		return pair.car, nil

	case "cdr":
		if len(args) != 1 {
			return nil, newEvalError(pos, "%s expects exactly 1 argument", name)
		}
		pair, err := expectPair(args[0], pos, name)
		if err != nil {
			return nil, err
		}
		return pair.cdr, nil

	case "null?":
		if len(args) != 1 {
			return nil, newEvalError(pos, "%s expects exactly 1 argument", name)
		}
		_, ok := args[0].(emptyList)
		return ok, nil

	case "list":
		return buildList(args), nil

	case "length":
		if len(args) != 1 {
			return nil, newEvalError(pos, "%s expects exactly 1 argument", name)
		}
		elements, err := listElements(args[0], pos, name)
		if err != nil {
			return nil, err
		}
		return len(elements), nil

	case "append":
		result := any(emptyList{})
		if len(args) > 0 {
			if _, err := listElements(args[len(args)-1], pos, name); err != nil {
				return nil, err
			}
			result = args[len(args)-1]
		}

		for index := len(args) - 2; index >= 0; index-- {
			elements, err := listElements(args[index], pos, name)
			if err != nil {
				return nil, err
			}
			for elementIndex := len(elements) - 1; elementIndex >= 0; elementIndex-- {
				result = &pairValue{car: elements[elementIndex], cdr: result}
			}
		}
		return result, nil

	case "list-ref":
		if len(args) != 2 {
			return nil, newEvalError(pos, "%s expects exactly 2 arguments", name)
		}
		index, err := expectInt(args[1], pos, name)
		if err != nil {
			return nil, err
		}
		tail, err := listTailAt(args[0], index, pos, name)
		if err != nil {
			return nil, err
		}
		pair, ok := tail.(*pairValue)
		if !ok {
			return nil, newEvalError(pos, "%s index out of range", name)
		}
		return pair.car, nil

	case "list-tail":
		if len(args) != 2 {
			return nil, newEvalError(pos, "%s expects exactly 2 arguments", name)
		}
		index, err := expectInt(args[1], pos, name)
		if err != nil {
			return nil, err
		}
		return listTailAt(args[0], index, pos, name)

	case "list?":
		if len(args) != 1 {
			return nil, newEvalError(pos, "%s expects exactly 1 argument", name)
		}
		return isProperList(args[0]), nil

	case "assoc":
		if len(args) != 2 {
			return nil, newEvalError(pos, "%s expects exactly 2 arguments", name)
		}
		elements, err := listElements(args[1], pos, name)
		if err != nil {
			return nil, err
		}
		for _, element := range elements {
			pair, ok := element.(*pairValue)
			if !ok {
				return nil, newEvalError(pos, "%s expects an association list", name)
			}
			if equalValues(args[0], pair.car) {
				return element, nil
			}
		}
		return false, nil

	case "map":
		if len(args) < 2 {
			return nil, newEvalError(pos, "%s expects at least 2 arguments", name)
		}
		lists := make([][]any, len(args)-1)
		expectedLen := -1
		for index, arg := range args[1:] {
			elements, err := listElements(arg, pos, name)
			if err != nil {
				return nil, err
			}
			if expectedLen == -1 {
				expectedLen = len(elements)
			} else if len(elements) != expectedLen {
				return nil, newEvalError(pos, "%s expects lists of equal length", name)
			}
			lists[index] = elements
		}
		results := make([]any, 0, expectedLen)
		callArgs := make([]any, len(lists))
		for index := 0; index < expectedLen; index++ {
			for listIndex := range lists {
				callArgs[listIndex] = lists[listIndex][index]
			}
			result, err := applyProcedure(i, args[0], callArgs, pos)
			if err != nil {
				return nil, err
			}
			results = append(results, result)
		}
		return buildList(results), nil

	case "string?":
		if len(args) != 1 {
			return nil, newEvalError(pos, "%s expects exactly 1 argument", name)
		}
		switch args[0].(type) {
		case stringValue, *mutableString:
			return true, nil
		}
		ok := false
		return ok, nil

	case "number?":
		if len(args) != 1 {
			return nil, newEvalError(pos, "%s expects exactly 1 argument", name)
		}
		_, ok := args[0].(int)
		return ok, nil

	case "boolean?":
		if len(args) != 1 {
			return nil, newEvalError(pos, "%s expects exactly 1 argument", name)
		}
		_, ok := args[0].(bool)
		return ok, nil

	case "pair?":
		if len(args) != 1 {
			return nil, newEvalError(pos, "%s expects exactly 1 argument", name)
		}
		_, ok := args[0].(*pairValue)
		return ok, nil

	case "symbol?":
		if len(args) != 1 {
			return nil, newEvalError(pos, "%s expects exactly 1 argument", name)
		}
		_, ok := args[0].(symbolValue)
		return ok, nil

	case "display":
		if len(args) != 1 {
			return nil, newEvalError(pos, "%s expects exactly 1 argument", name)
		}
		i.output.WriteString(formatDisplayValue(args[0]))
		return voidValue{}, nil

	case "write":
		if len(args) != 1 {
			return nil, newEvalError(pos, "%s expects exactly 1 argument", name)
		}
		i.output.WriteString(formatValue(args[0]))
		return voidValue{}, nil

	case "newline":
		if len(args) != 0 {
			return nil, newEvalError(pos, "%s expects exactly 0 arguments", name)
		}
		i.output.WriteByte('\n')
		return voidValue{}, nil

	case "string-append":
		var builder strings.Builder
		for _, arg := range args {
			text, err := expectString(arg, pos, name)
			if err != nil {
				return nil, err
			}
			builder.WriteString(text)
		}
		return stringValue(builder.String()), nil

	case "string-length":
		if len(args) != 1 {
			return nil, newEvalError(pos, "%s expects exactly 1 argument", name)
		}
		text, err := expectString(args[0], pos, name)
		if err != nil {
			return nil, err
		}
		return len([]rune(text)), nil

	case "substring":
		if len(args) != 3 {
			return nil, newEvalError(pos, "%s expects exactly 3 arguments", name)
		}
		text, err := expectString(args[0], pos, name)
		if err != nil {
			return nil, err
		}
		start, err := expectInt(args[1], pos, name)
		if err != nil {
			return nil, err
		}
		end, err := expectInt(args[2], pos, name)
		if err != nil {
			return nil, err
		}
		runes := []rune(text)
		if start < 0 || end < 0 || start > end || end > len(runes) {
			return nil, newEvalError(pos, "%s index out of range", name)
		}
		return stringValue(string(runes[start:end])), nil

	case "string->number":
		if len(args) != 1 {
			return nil, newEvalError(pos, "%s expects exactly 1 argument", name)
		}
		text, err := expectString(args[0], pos, name)
		if err != nil {
			return nil, err
		}
		value, convErr := strconv.Atoi(text)
		if convErr != nil {
			return false, nil
		}
		return value, nil

	case "number->string":
		if len(args) != 1 {
			return nil, newEvalError(pos, "%s expects exactly 1 argument", name)
		}
		value, err := expectInt(args[0], pos, name)
		if err != nil {
			return nil, err
		}
		return stringValue(strconv.Itoa(value)), nil

	case "symbol->string":
		if len(args) != 1 {
			return nil, newEvalError(pos, "%s expects exactly 1 argument", name)
		}
		symbol, err := expectSymbol(args[0], pos, name)
		if err != nil {
			return nil, err
		}
		return stringValue(symbol), nil

	case "string->symbol":
		if len(args) != 1 {
			return nil, newEvalError(pos, "%s expects exactly 1 argument", name)
		}
		text, err := expectString(args[0], pos, name)
		if err != nil {
			return nil, err
		}
		return symbolValue(text), nil

	case "string-ref":
		if len(args) != 2 {
			return nil, newEvalError(pos, "%s expects exactly 2 arguments", name)
		}
		text, err := expectString(args[0], pos, name)
		if err != nil {
			return nil, err
		}
		index, err := expectInt(args[1], pos, name)
		if err != nil {
			return nil, err
		}
		runes := []rune(text)
		if index < 0 || index >= len(runes) {
			return nil, newEvalError(pos, "%s index out of range", name)
		}
		return charValue(runes[index]), nil

	case "string-copy":
		if len(args) != 1 {
			return nil, newEvalError(pos, "%s expects exactly 1 argument", name)
		}
		text, err := expectString(args[0], pos, name)
		if err != nil {
			return nil, err
		}
		return newMutableString(text), nil

	case "string=?":
		return stringCompare(name, args, pos, func(a, b string) bool { return a == b })

	case "string<?":
		return stringCompare(name, args, pos, func(a, b string) bool { return compareStrings(a, b) < 0 })

	case "string-ci=?":
		return stringCompare(name, args, pos, func(a, b string) bool { return strings.EqualFold(a, b) })

	case "string-upcase":
		if len(args) != 1 {
			return nil, newEvalError(pos, "%s expects exactly 1 argument", name)
		}
		text, err := expectString(args[0], pos, name)
		if err != nil {
			return nil, err
		}
		return stringValue(transformString(text, unicode.ToUpper)), nil

	case "string-downcase":
		if len(args) != 1 {
			return nil, newEvalError(pos, "%s expects exactly 1 argument", name)
		}
		text, err := expectString(args[0], pos, name)
		if err != nil {
			return nil, err
		}
		return stringValue(transformString(text, unicode.ToLower)), nil

	case "string-set!":
		if len(args) != 3 {
			return nil, newEvalError(pos, "%s expects exactly 3 arguments", name)
		}
		text, err := expectMutableString(args[0], pos, name)
		if err != nil {
			return nil, err
		}
		index, err := expectInt(args[1], pos, name)
		if err != nil {
			return nil, err
		}
		ch, err := expectChar(args[2], pos, name)
		if err != nil {
			return nil, err
		}
		if index < 0 || index >= len(text.runes) {
			return nil, newEvalError(pos, "%s index out of range", name)
		}
		text.runes[index] = rune(ch)
		return voidValue{}, nil

	case "char?":
		if len(args) != 1 {
			return nil, newEvalError(pos, "%s expects exactly 1 argument", name)
		}
		_, ok := args[0].(charValue)
		return ok, nil

	case "char-alphabetic?":
		if len(args) != 1 {
			return nil, newEvalError(pos, "%s expects exactly 1 argument", name)
		}
		ch, err := expectChar(args[0], pos, name)
		if err != nil {
			return nil, err
		}
		return unicode.IsLetter(rune(ch)), nil

	case "char-numeric?":
		if len(args) != 1 {
			return nil, newEvalError(pos, "%s expects exactly 1 argument", name)
		}
		ch, err := expectChar(args[0], pos, name)
		if err != nil {
			return nil, err
		}
		return unicode.IsDigit(rune(ch)), nil

	case "char-upcase":
		if len(args) != 1 {
			return nil, newEvalError(pos, "%s expects exactly 1 argument", name)
		}
		ch, err := expectChar(args[0], pos, name)
		if err != nil {
			return nil, err
		}
		return charValue(unicode.ToUpper(rune(ch))), nil

	case "char-downcase":
		if len(args) != 1 {
			return nil, newEvalError(pos, "%s expects exactly 1 argument", name)
		}
		ch, err := expectChar(args[0], pos, name)
		if err != nil {
			return nil, err
		}
		return charValue(unicode.ToLower(rune(ch))), nil

	case "char=?":
		return charCompare(name, args, pos, func(a, b rune) bool { return a == b })

	case "char<?":
		return charCompare(name, args, pos, func(a, b rune) bool { return a < b })
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

func charCompare(name string, args []any, pos position, compare func(rune, rune) bool) (bool, error) {
	if len(args) < 2 {
		return false, newEvalError(pos, "%s expects at least 2 arguments", name)
	}

	prev, err := expectChar(args[0], pos, name)
	if err != nil {
		return false, err
	}

	for _, arg := range args[1:] {
		current, err := expectChar(arg, pos, name)
		if err != nil {
			return false, err
		}
		if !compare(rune(prev), rune(current)) {
			return false, nil
		}
		prev = current
	}

	return true, nil
}

func stringCompare(name string, args []any, pos position, compare func(string, string) bool) (bool, error) {
	if len(args) < 2 {
		return false, newEvalError(pos, "%s expects at least 2 arguments", name)
	}

	prev, err := expectString(args[0], pos, name)
	if err != nil {
		return false, err
	}

	for _, arg := range args[1:] {
		current, err := expectString(arg, pos, name)
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

func compareStrings(left, right string) int {
	leftRunes := []rune(left)
	rightRunes := []rune(right)
	limit := len(leftRunes)
	if len(rightRunes) < limit {
		limit = len(rightRunes)
	}

	for index := 0; index < limit; index++ {
		if leftRunes[index] < rightRunes[index] {
			return -1
		}
		if leftRunes[index] > rightRunes[index] {
			return 1
		}
	}

	switch {
	case len(leftRunes) < len(rightRunes):
		return -1
	case len(leftRunes) > len(rightRunes):
		return 1
	default:
		return 0
	}
}

func transformString(text string, transform func(rune) rune) string {
	runes := []rune(text)
	for index, value := range runes {
		runes[index] = transform(value)
	}
	return string(runes)
}

func listTailAt(value any, index int, pos position, procedure string) (any, error) {
	if index < 0 {
		return nil, newEvalError(pos, "%s index out of range", procedure)
	}
	if index == 0 {
		switch value.(type) {
		case emptyList, *pairValue:
			return value, nil
		default:
			return nil, newEvalError(pos, "%s expects a list", procedure)
		}
	}

	current := value
	for step := 0; step < index; step++ {
		pair, ok := current.(*pairValue)
		if !ok {
			return nil, newEvalError(pos, "%s index out of range", procedure)
		}
		current = pair.cdr
	}

	return current, nil
}

func isProperList(value any) bool {
	for {
		switch current := value.(type) {
		case emptyList:
			return true
		case *pairValue:
			value = current.cdr
		default:
			return false
		}
	}
}

func eqValues(left, right any) bool {
	switch l := left.(type) {
	case int:
		r, ok := right.(int)
		return ok && l == r
	case bool:
		r, ok := right.(bool)
		return ok && l == r
	case stringValue:
		r, ok := right.(stringValue)
		return ok && l == r
	case symbolValue:
		r, ok := right.(symbolValue)
		return ok && l == r
	case charValue:
		r, ok := right.(charValue)
		return ok && l == r
	case emptyList:
		_, ok := right.(emptyList)
		return ok
	case *mutableString:
		r, ok := right.(*mutableString)
		return ok && l == r
	case *pairValue:
		r, ok := right.(*pairValue)
		return ok && l == r
	case *builtinProcedure:
		r, ok := right.(*builtinProcedure)
		return ok && l == r
	case *lambdaProcedure:
		r, ok := right.(*lambdaProcedure)
		return ok && l == r
	default:
		return false
	}
}

func equalValues(left, right any) bool {
	switch l := left.(type) {
	case int:
		r, ok := right.(int)
		return ok && l == r
	case bool:
		r, ok := right.(bool)
		return ok && l == r
	case stringValue:
		switch r := right.(type) {
		case stringValue:
			return l == r
		case *mutableString:
			return string(l) == r.String()
		default:
			return false
		}
	case *mutableString:
		switch r := right.(type) {
		case stringValue:
			return l.String() == string(r)
		case *mutableString:
			return l.String() == r.String()
		default:
			return false
		}
	case symbolValue:
		r, ok := right.(symbolValue)
		return ok && l == r
	case charValue:
		r, ok := right.(charValue)
		return ok && l == r
	case emptyList:
		_, ok := right.(emptyList)
		return ok
	case *pairValue:
		r, ok := right.(*pairValue)
		return ok && equalValues(l.car, r.car) && equalValues(l.cdr, r.cdr)
	case *builtinProcedure:
		r, ok := right.(*builtinProcedure)
		return ok && l == r
	case *lambdaProcedure:
		r, ok := right.(*lambdaProcedure)
		return ok && l == r
	default:
		return false
	}
}

func expectInt(value any, pos position, procedure string) (int, error) {
	n, ok := value.(int)
	if !ok {
		return 0, newEvalError(pos, "%s expects numeric arguments", procedure)
	}
	return n, nil
}

func expectString(value any, pos position, procedure string) (string, error) {
	switch text := value.(type) {
	case stringValue:
		return string(text), nil
	case *mutableString:
		return text.String(), nil
	default:
		return "", newEvalError(pos, "%s expects a string", procedure)
	}
}

func expectSymbol(value any, pos position, procedure string) (string, error) {
	symbol, ok := value.(symbolValue)
	if !ok {
		return "", newEvalError(pos, "%s expects a symbol", procedure)
	}
	return string(symbol), nil
}

func expectMutableString(value any, pos position, procedure string) (*mutableString, error) {
	text, ok := value.(*mutableString)
	if !ok {
		return nil, newEvalError(pos, "%s expects a mutable string", procedure)
	}
	return text, nil
}

func expectChar(value any, pos position, procedure string) (charValue, error) {
	ch, ok := value.(charValue)
	if !ok {
		return 0, newEvalError(pos, "%s expects a character", procedure)
	}
	return ch, nil
}

func expectPair(value any, pos position, procedure string) (*pairValue, error) {
	pair, ok := value.(*pairValue)
	if !ok {
		return nil, newEvalError(pos, "%s expects a pair", procedure)
	}
	return pair, nil
}

func listElements(value any, pos position, procedure string) ([]any, error) {
	elements := []any{}
	for {
		switch current := value.(type) {
		case emptyList:
			return elements, nil
		case *pairValue:
			elements = append(elements, current.car)
			value = current.cdr
		default:
			return nil, newEvalError(pos, "%s expects a proper list", procedure)
		}
	}
}

func buildList(values []any) any {
	result := any(emptyList{})
	for index := len(values) - 1; index >= 0; index-- {
		result = &pairValue{car: values[index], cdr: result}
	}
	return result
}

func isTruthy(value any) bool {
	boolean, ok := value.(bool)
	return !ok || boolean
}

func formatValue(value any) string {
	switch v := value.(type) {
	case nil:
		return ""
	case voidValue:
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
	case *mutableString:
		return strconv.Quote(v.String())
	case symbolValue:
		return string(v)
	case charValue:
		return formatCharLiteral(rune(v))
	case emptyList:
		return "()"
	case *pairValue:
		return formatPair(v)
	case callable:
		return "#<procedure>"
	default:
		return fmt.Sprintf("%v", v)
	}
}

func formatDisplayValue(value any) string {
	switch v := value.(type) {
	case stringValue:
		return string(v)
	case *mutableString:
		return v.String()
	case charValue:
		return string(rune(v))
	default:
		return formatValue(value)
	}
}

func formatCharLiteral(value rune) string {
	switch value {
	case ' ':
		return "#\\space"
	case '\n':
		return "#\\newline"
	default:
		return "#\\" + string(value)
	}
}

func formatPair(pair *pairValue) string {
	var builder strings.Builder
	builder.WriteByte('(')

	current := pair
	for {
		builder.WriteString(formatValue(current.car))

		switch next := current.cdr.(type) {
		case emptyList:
			builder.WriteByte(')')
			return builder.String()
		case *pairValue:
			builder.WriteByte(' ')
			current = next
		default:
			builder.WriteString(" . ")
			builder.WriteString(formatValue(next))
			builder.WriteByte(')')
			return builder.String()
		}
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
		case '\'':
			tokens = append(tokens, token{kind: tokenQuote, text: "'", pos: pos})
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
				if current == '(' || current == ')' || current == '\'' || current == '"' || current == ';' || unicode.IsSpace(rune(current)) {
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
	case tokenQuote:
		quoted, err := p.parseExpr()
		if err != nil {
			return nil, err
		}
		return &listExpr{
			elements: []expr{
				&symbolExpr{value: "quote", pos: current.pos},
				quoted,
			},
			pos: current.pos,
		}, nil
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

	if value, ok := parseCharLiteral(tok.text); ok {
		return &charExpr{value: value, pos: tok.pos}
	}

	if value, err := strconv.Atoi(tok.text); err == nil {
		return &integerExpr{value: value, pos: tok.pos}
	}

	return &symbolExpr{value: tok.text, pos: tok.pos}
}

func parseCharLiteral(text string) (rune, bool) {
	if !strings.HasPrefix(text, "#\\") {
		return 0, false
	}

	name := text[2:]
	switch name {
	case "space":
		return ' ', true
	case "newline":
		return '\n', true
	}

	runes := []rune(name)
	if len(runes) != 1 {
		return 0, false
	}
	return runes[0], true
}
