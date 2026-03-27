package ming

import (
	"fmt"
	"math"
	"os"
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

type rationalExpr struct {
	value rationalValue
	pos   position
}

func (e *rationalExpr) exprPos() position { return e.pos }

type inexactExpr struct {
	value inexactValue
	pos   position
}

func (e *inexactExpr) exprPos() position { return e.pos }

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
	value   string
	pos     position
	binding *binding
	macro   *syntaxRuleMacro
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

func newRuntimeString(text string, immutable bool) any {
	if immutable {
		return stringValue(text)
	}
	return newMutableString(text)
}

type vectorValue struct {
	elements []any
}

func newVectorValue(elements []any) *vectorValue {
	return &vectorValue{elements: append([]any(nil), elements...)}
}

type pairValue struct {
	car any
	cdr any
}

type pairComparison struct {
	left  *pairValue
	right *pairValue
}

type vectorComparison struct {
	left  *vectorValue
	right *vectorValue
}

type formatState struct {
	pairs   map[*pairValue]struct{}
	vectors map[*vectorValue]struct{}
}

type callable interface {
	Call(*interpreter, []any, position) (any, error)
}

type tailCall struct {
	procedure callable
	args      []any
	pos       position
}

func newTailCall(procedure callable, args []any, pos position) *tailCall {
	return &tailCall{
		procedure: procedure,
		args:      append([]any(nil), args...),
		pos:       pos,
	}
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

func (p *lambdaProcedure) matchesArity(argCount int) bool {
	if p.hasRest {
		return argCount >= len(p.params)
	}
	return argCount == len(p.params)
}

func (p *lambdaProcedure) Call(i *interpreter, args []any, pos position) (any, error) {
	if !p.matchesArity(len(args)) && !p.hasRest {
		return nil, newEvalError(pos, "wrong number of arguments: expected %d, got %d", len(p.params), len(args))
	}
	if !p.matchesArity(len(args)) && p.hasRest {
		return nil, newEvalError(pos, "wrong number of arguments: expected at least %d, got %d", len(p.params), len(args))
	}

	callEnv := newEnvironment(p.env)
	for index, name := range p.params {
		callEnv.define(name, args[index])
	}
	if p.hasRest {
		callEnv.define(p.restName, buildList(args[len(p.params):]))
	}

	return i.evalSequence(p.body, callEnv, true)
}

type caseLambdaProcedure struct {
	clauses []*lambdaProcedure
}

func (p *caseLambdaProcedure) matchingClause(argCount int) *lambdaProcedure {
	for _, clause := range p.clauses {
		if clause.matchesArity(argCount) {
			return clause
		}
	}
	return nil
}

func (p *caseLambdaProcedure) Call(i *interpreter, args []any, pos position) (any, error) {
	clause := p.matchingClause(len(args))
	if clause == nil {
		return nil, newEvalError(pos, "wrong number of arguments: no matching case-lambda clause for %d argument(s)", len(args))
	}
	return clause.Call(i, args, pos)
}

type recordType struct {
	name       string
	fieldNames []string
}

type recordValue struct {
	recordType *recordType
	fields     []any
}

type recordConstructor struct {
	name       string
	recordType *recordType
}

func (p *recordConstructor) Call(_ *interpreter, args []any, pos position) (any, error) {
	if len(args) != len(p.recordType.fieldNames) {
		return nil, newEvalError(pos, "%s expects exactly %d arguments", p.name, len(p.recordType.fieldNames))
	}
	fields := append([]any(nil), args...)
	return &recordValue{recordType: p.recordType, fields: fields}, nil
}

type recordPredicate struct {
	recordType *recordType
}

func (p *recordPredicate) Call(_ *interpreter, args []any, pos position) (any, error) {
	if len(args) != 1 {
		return nil, newEvalError(pos, "record predicate expects exactly 1 argument")
	}
	record, ok := args[0].(*recordValue)
	return ok && record.recordType == p.recordType, nil
}

type recordAccessor struct {
	name       string
	recordType *recordType
	index      int
}

func (p *recordAccessor) Call(_ *interpreter, args []any, pos position) (any, error) {
	if len(args) != 1 {
		return nil, newEvalError(pos, "%s expects exactly 1 argument", p.name)
	}
	record, ok := args[0].(*recordValue)
	if !ok || record.recordType != p.recordType {
		return nil, newEvalError(pos, "%s expects a %s record", p.name, p.recordType.name)
	}
	return record.fields[p.index], nil
}

type binding struct {
	value any
}

type environment struct {
	parent *environment
	values map[string]*binding
	macros map[string]*syntaxRuleMacro
}

func newEnvironment(parent *environment) *environment {
	return &environment{
		parent: parent,
		values: map[string]*binding{},
		macros: map[string]*syntaxRuleMacro{},
	}
}

func (e *environment) define(name string, value any) {
	e.defineBinding(name, &binding{value: value})
}

func (e *environment) defineBinding(name string, binding *binding) {
	e.values[name] = binding
}

func (e *environment) lookupBinding(name string) (*binding, bool) {
	for current := e; current != nil; current = current.parent {
		if binding, ok := current.values[name]; ok {
			return binding, true
		}
	}
	return nil, false
}

func (e *environment) lookup(name string) (any, bool) {
	binding, ok := e.lookupBinding(name)
	if !ok {
		return nil, false
	}
	return binding.value, true
}

func (e *environment) defineMacro(name string, macro *syntaxRuleMacro) {
	e.macros[name] = macro
}

func (e *environment) lookupMacro(name string) (*syntaxRuleMacro, bool) {
	for current := e; current != nil; current = current.parent {
		if macro, ok := current.macros[name]; ok {
			return macro, true
		}
	}
	return nil, false
}

func (e *environment) assign(name string, value any) bool {
	binding, ok := e.lookupBinding(name)
	if !ok {
		return false
	}
	binding.value = value
	return true
}

type interpreter struct {
	output           strings.Builder
	global           *environment
	gensymCounter    int
	immutableStrings bool
	currentWinds     []*dynamicWind
	currentHandlers  []*exceptionHandler
}

func newInterpreter() *interpreter {
	global := newEnvironment(nil)
	installBuiltins(global)
	return &interpreter{
		global:           global,
		immutableStrings: stringsAreImmutableAtCurrentLevel(),
	}
}

func stringsAreImmutableAtCurrentLevel() bool {
	levelText := os.Getenv("BENCH_LEVEL")
	if levelText == "" {
		return true
	}

	level, err := strconv.Atoi(levelText)
	if err != nil {
		return true
	}

	return level >= 15
}

func evalInput(input string) (string, string, error) {
	parsed, err := parseProgram(input)
	if err != nil {
		return "", "", err
	}

	intp := newInterpreter()
	result := any(voidValue{})

	if continuationsEnabledAtCurrentLevel() {
		result, err = intp.evalProgramWithContinuations(parsed)
		if err != nil {
			return "", intp.output.String(), normalizeInterpreterError(err)
		}
	} else {
		for _, expression := range parsed {
			result, err = intp.eval(expression, intp.global)
			if err != nil {
				return "", intp.output.String(), normalizeInterpreterError(err)
			}
		}
	}

	return formatValue(result), intp.output.String(), nil
}

func (i *interpreter) resolveTailResult(result any) (any, error) {
	for {
		call, ok := result.(*tailCall)
		if !ok {
			return result, nil
		}

		var err error
		result, err = call.procedure.Call(i, call.args, call.pos)
		if err != nil {
			return nil, err
		}
	}
}

func installBuiltins(env *environment) {
	for _, name := range []string{
		"+", "-", "*", "/", "<", ">", "=", "<=", ">=", "not",
		"cons", "car", "cdr", "set-car!", "set-cdr!", "null?", "list", "length", "append", "reverse",
		"vector", "make-vector", "vector?", "vector-length", "vector-ref", "vector-set!", "vector->list", "list->vector",
		"string?", "number?", "integer?", "rational?", "exact?", "inexact?", "boolean?", "pair?", "symbol?", "procedure?",
		"apply", "eqv?", "eq?", "equal?", "call/cc", "call-with-current-continuation", "dynamic-wind", "raise", "with-exception-handler",
		"display", "write", "newline", "error",
		"string-append", "string-length", "substring", "make-string", "string",
		"string->number", "number->string", "exact->inexact", "inexact->exact", "numerator", "denominator",
		"symbol->string", "string->symbol",
		"string-ref", "string-copy", "string-set!", "string->list", "list->string", "char?", "char->integer", "integer->char",
		"abs", "modulo", "remainder", "quotient", "min", "max", "expt", "gcd", "lcm", "truncate", "round",
		"zero?", "positive?", "negative?", "odd?", "even?",
		"list-ref", "list-tail", "list?", "assoc", "assv", "member", "map", "for-each",
		"char-alphabetic?", "char-numeric?", "char-upcase", "char-downcase", "char=?", "char<?",
		"string=?", "string<?", "string>?", "string<=?", "string>=?", "string-ci=?", "string-upcase", "string-downcase",
	} {
		name := name
		env.define(name, &builtinProcedure{
			name: name,
			fn: func(i *interpreter, args []any, pos position) (any, error) {
				return applyBuiltin(i, name, args, pos)
			},
		})
	}

	for _, name := range []string{
		"caar", "cadr", "cdar", "cddr",
		"caaar", "caadr", "cadar", "caddr", "cdaar", "cdadr", "cddar", "cdddr",
		"caaaar", "caaadr", "caadar", "caaddr", "cadaar", "cadadr", "caddar", "cadddr",
		"cdaaar", "cdaadr", "cdadar", "cdaddr", "cddaar", "cddadr", "cdddar", "cddddr",
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
	result, err := i.evalExpr(expression, env, false)
	if err != nil {
		return nil, err
	}
	return i.resolveTailResult(result)
}

func (i *interpreter) evalExpr(expression expr, env *environment, tail bool) (any, error) {
	switch e := expression.(type) {
	case *integerExpr:
		return e.value, nil
	case *rationalExpr:
		return e.value, nil
	case *inexactExpr:
		return e.value, nil
	case *booleanExpr:
		return e.value, nil
	case *stringExpr:
		return stringValue(e.value), nil
	case *charExpr:
		return charValue(e.value), nil
	case *symbolExpr:
		if e.binding != nil {
			return e.binding.value, nil
		}
		value, ok := env.lookup(e.value)
		if !ok {
			return nil, newEvalError(e.pos, "unbound variable: %s", e.value)
		}
		return value, nil
	case *listExpr:
		return i.evalList(e, env, tail)
	default:
		return nil, newEvalError(expression.exprPos(), "internal error: unknown expression")
	}
}

func (i *interpreter) evalList(list *listExpr, env *environment, tail bool) (any, error) {
	if len(list.elements) == 0 {
		return nil, newEvalError(list.pos, "cannot evaluate empty list")
	}

	if operator, ok := list.elements[0].(*symbolExpr); ok {
		if operator.macro != nil {
			expanded, err := operator.macro.expand(i, list)
			if err != nil {
				return nil, err
			}
			return i.evalExpr(expanded, env, tail)
		}

		if operator.binding == nil {
			if operator.value == "define-syntax" {
				return i.evalDefineSyntax(list.elements[1:], operator.pos, env)
			}
			if macro, ok := env.lookupMacro(operator.value); ok {
				expanded, err := macro.expand(i, list)
				if err != nil {
					return nil, err
				}
				return i.evalExpr(expanded, env, tail)
			}
		}

		if operator.binding == nil {
			switch operator.value {
			case "and":
				return i.evalAnd(list.elements[1:], env, tail)
			case "or":
				return i.evalOr(list.elements[1:], env, tail)
			case "begin":
				return i.evalBegin(list.elements[1:], env, tail)
			case "if":
				return i.evalIf(list.elements[1:], operator.pos, env, tail)
			case "cond":
				return i.evalCond(list.elements[1:], operator.pos, env, tail)
			case "guard":
				expanded, err := expandGuardForm(list.elements[1:], operator.pos)
				if err != nil {
					return nil, err
				}
				return i.evalExpr(expanded, env, tail)
			case "case":
				return i.evalCase(list.elements[1:], operator.pos, env, tail)
			case "do":
				return i.evalDo(list.elements[1:], operator.pos, env, tail)
			case "define":
				return i.evalDefine(list.elements[1:], operator.pos, env)
			case "set!":
				return i.evalSet(list.elements[1:], operator.pos, env)
			case "let":
				return i.evalLet(list.elements[1:], operator.pos, env, tail)
			case "let*":
				return i.evalLetStar(list.elements[1:], operator.pos, env, tail)
			case "letrec":
				return i.evalLetRec(list.elements[1:], operator.pos, env, false, "letrec", tail)
			case "letrec*":
				return i.evalLetRec(list.elements[1:], operator.pos, env, true, "letrec*", tail)
			case "quote":
				return i.evalQuote(list.elements[1:], operator.pos)
			case "lambda":
				return i.evalLambda(list.elements[1:], operator.pos, env)
			case "case-lambda":
				return i.evalCaseLambda(list.elements[1:], operator.pos, env)
			case "define-record-type":
				return i.evalDefineRecordType(list.elements[1:], operator.pos, env)
			}
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

	return applyProcedure(i, operatorValue, args, list.elements[0].exprPos(), tail)
}

func (i *interpreter) evalSequence(expressions []expr, env *environment, tail bool) (any, error) {
	result := any(voidValue{})
	for index, expression := range expressions {
		if tail && index == len(expressions)-1 {
			return i.evalExpr(expression, env, true)
		}

		var err error
		result, err = i.eval(expression, env)
		if err != nil {
			return nil, err
		}
	}
	return result, nil
}

func (i *interpreter) evalAnd(args []expr, env *environment, tail bool) (any, error) {
	if len(args) == 0 {
		return true, nil
	}

	result := any(true)
	for index, argExpr := range args {
		if tail && index == len(args)-1 {
			return i.evalExpr(argExpr, env, true)
		}

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

func (i *interpreter) evalOr(args []expr, env *environment, tail bool) (any, error) {
	for index, argExpr := range args {
		if tail && index == len(args)-1 {
			return i.evalExpr(argExpr, env, true)
		}

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

func (i *interpreter) evalBegin(args []expr, env *environment, tail bool) (any, error) {
	return i.evalSequence(args, env, tail)
}

func (i *interpreter) evalIf(args []expr, pos position, env *environment, tail bool) (any, error) {
	if len(args) < 2 || len(args) > 3 {
		return nil, newEvalError(pos, "if expects 2 or 3 arguments")
	}

	condition, err := i.eval(args[0], env)
	if err != nil {
		return nil, err
	}

	if isTruthy(condition) {
		if tail {
			return i.evalExpr(args[1], env, true)
		}
		return i.eval(args[1], env)
	}
	if len(args) == 3 {
		if tail {
			return i.evalExpr(args[2], env, true)
		}
		return i.eval(args[2], env)
	}
	return voidValue{}, nil
}

func (i *interpreter) evalCond(args []expr, pos position, env *environment, tail bool) (any, error) {
	for index, clauseExpr := range args {
		clause, ok := clauseExpr.(*listExpr)
		if !ok || len(clause.elements) == 0 {
			return nil, newEvalError(clauseExpr.exprPos(), "cond clauses must be non-empty lists")
		}

		if symbol, ok := clause.elements[0].(*symbolExpr); ok && symbol.value == "else" {
			if index != len(args)-1 {
				return nil, newEvalError(symbol.pos, "cond else clause must be last")
			}
			return i.evalSequence(clause.elements[1:], env, tail)
		}

		testValue, err := i.eval(clause.elements[0], env)
		if err != nil {
			return nil, err
		}
		if isTruthy(testValue) {
			if len(clause.elements) == 1 {
				return testValue, nil
			}
			return i.evalSequence(clause.elements[1:], env, tail)
		}
	}

	return voidValue{}, nil
}

func (i *interpreter) evalCase(args []expr, pos position, env *environment, tail bool) (any, error) {
	if len(args) == 0 {
		return nil, newEvalError(pos, "case expects a key and at least 1 clause")
	}

	key, err := i.eval(args[0], env)
	if err != nil {
		return nil, err
	}

	clauses := args[1:]
	for index, clauseExpr := range clauses {
		clause, ok := clauseExpr.(*listExpr)
		if !ok || len(clause.elements) == 0 {
			return nil, newEvalError(clauseExpr.exprPos(), "case clauses must be non-empty lists")
		}

		if symbol, ok := clause.elements[0].(*symbolExpr); ok && symbol.value == "else" {
			if index != len(clauses)-1 {
				return nil, newEvalError(symbol.pos, "case else clause must be last")
			}
			if len(clause.elements) == 1 {
				return voidValue{}, nil
			}
			return i.evalSequence(clause.elements[1:], env, tail)
		}

		datumList, ok := clause.elements[0].(*listExpr)
		if !ok {
			return nil, newEvalError(clause.elements[0].exprPos(), "case clause datums must be a list")
		}

		matched := false
		for _, datumExpr := range datumList.elements {
			datum, err := datumFromExpr(datumExpr)
			if err != nil {
				return nil, err
			}
			if eqValues(key, datum) {
				matched = true
				break
			}
		}

		if matched {
			if len(clause.elements) == 1 {
				return voidValue{}, nil
			}
			return i.evalSequence(clause.elements[1:], env, tail)
		}
	}

	return voidValue{}, nil
}

func (i *interpreter) evalDo(args []expr, pos position, env *environment, tail bool) (any, error) {
	if len(args) < 2 {
		return nil, newEvalError(pos, "do expects bindings, a termination clause, and an optional body")
	}

	bindings, err := parseDoBindings(args[0])
	if err != nil {
		return nil, err
	}

	termination, ok := args[1].(*listExpr)
	if !ok || len(termination.elements) == 0 {
		return nil, newEvalError(args[1].exprPos(), "do termination clause must be a non-empty list")
	}

	initialValues := make([]any, len(bindings))
	for index, binding := range bindings {
		value, err := i.eval(binding.initExpr, env)
		if err != nil {
			return nil, err
		}
		initialValues[index] = value
	}

	loopEnv := newEnvironment(env)
	slots := make([]*binding, len(bindings))
	for index, spec := range bindings {
		slot := &binding{value: initialValues[index]}
		loopEnv.defineBinding(spec.name, slot)
		slots[index] = slot
	}

	for {
		shouldStop, err := i.eval(termination.elements[0], loopEnv)
		if err != nil {
			return nil, err
		}
		if isTruthy(shouldStop) {
			if len(termination.elements) == 1 {
				return voidValue{}, nil
			}
			return i.evalSequence(termination.elements[1:], loopEnv, tail)
		}

		if _, err := i.evalSequence(args[2:], loopEnv, false); err != nil {
			return nil, err
		}

		nextValues := make([]any, len(bindings))
		for index, spec := range bindings {
			if spec.stepExpr == nil {
				nextValues[index] = slots[index].value
				continue
			}
			value, err := i.eval(spec.stepExpr, loopEnv)
			if err != nil {
				return nil, err
			}
			nextValues[index] = value
		}

		for index, value := range nextValues {
			slots[index].value = value
		}
	}
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

	if name.binding != nil {
		name.binding.value = value
		return voidValue{}, nil
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

func (i *interpreter) evalLet(args []expr, pos position, env *environment, tail bool) (any, error) {
	if len(args) < 2 {
		return nil, newEvalError(pos, "let expects bindings and a body")
	}

	if name, ok := args[0].(*symbolExpr); ok {
		return i.evalNamedLet(name, args[1:], pos, env, tail)
	}

	return i.evalPlainLet(args, pos, env, tail)
}

func (i *interpreter) evalNamedLet(name *symbolExpr, args []expr, pos position, env *environment, tail bool) (any, error) {
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

	return applyProcedure(i, procedure, values, name.pos, tail)
}

func (i *interpreter) evalPlainLet(args []expr, _ position, env *environment, tail bool) (any, error) {
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

	return i.evalSequence(args[1:], letEnv, tail)
}

func (i *interpreter) evalLetStar(args []expr, pos position, env *environment, tail bool) (any, error) {
	if len(args) < 2 {
		return nil, newEvalError(pos, "let* expects bindings and a body")
	}

	bindings, err := parseLetBindings(args[0])
	if err != nil {
		return nil, err
	}

	letEnv := newEnvironment(env)
	for _, binding := range bindings {
		value, err := i.eval(binding.valueExpr, letEnv)
		if err != nil {
			return nil, err
		}
		letEnv.define(binding.name, value)
	}

	return i.evalSequence(args[1:], letEnv, tail)
}

func (i *interpreter) evalLetRec(args []expr, pos position, env *environment, sequential bool, formName string, tail bool) (any, error) {
	if len(args) < 2 {
		return nil, newEvalError(pos, "%s expects bindings and a body", formName)
	}

	bindings, err := parseLetBindings(args[0])
	if err != nil {
		return nil, err
	}

	letEnv := newEnvironment(env)
	slots := make([]*binding, len(bindings))
	for index, spec := range bindings {
		slot := &binding{}
		letEnv.defineBinding(spec.name, slot)
		slots[index] = slot
	}

	if sequential {
		for index, spec := range bindings {
			value, err := i.eval(spec.valueExpr, letEnv)
			if err != nil {
				return nil, err
			}
			slots[index].value = value
		}
	} else {
		values := make([]any, len(bindings))
		for index, spec := range bindings {
			value, err := i.eval(spec.valueExpr, letEnv)
			if err != nil {
				return nil, err
			}
			values[index] = value
		}
		for index, value := range values {
			slots[index].value = value
		}
	}

	return i.evalSequence(args[1:], letEnv, tail)
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

func (i *interpreter) evalCaseLambda(args []expr, pos position, env *environment) (any, error) {
	if len(args) == 0 {
		return nil, newEvalError(pos, "case-lambda expects at least 1 clause")
	}

	clauses := make([]*lambdaProcedure, 0, len(args))
	for _, clauseExpr := range args {
		clause, ok := clauseExpr.(*listExpr)
		if !ok || len(clause.elements) < 2 {
			return nil, newEvalError(clauseExpr.exprPos(), "case-lambda clauses must contain parameters and a body")
		}

		params, err := parseLambdaParameters(clause.elements[0])
		if err != nil {
			return nil, err
		}

		clauses = append(clauses, &lambdaProcedure{
			params:   params.required,
			restName: params.restName,
			hasRest:  params.hasRest,
			body:     clause.elements[1:],
			env:      env,
		})
	}

	return &caseLambdaProcedure{clauses: clauses}, nil
}

func (i *interpreter) evalDefineSyntax(args []expr, pos position, env *environment) (any, error) {
	if len(args) != 2 {
		return nil, newEvalError(pos, "define-syntax expects exactly 2 arguments")
	}

	name, ok := args[0].(*symbolExpr)
	if !ok {
		return nil, newEvalError(args[0].exprPos(), "define-syntax requires a symbol name")
	}

	macro, err := parseSyntaxRuleMacro(name.value, args[1], env)
	if err != nil {
		return nil, err
	}
	env.defineMacro(name.value, macro)
	return voidValue{}, nil
}

func (i *interpreter) evalDefineRecordType(args []expr, pos position, env *environment) (any, error) {
	if len(args) < 3 {
		return nil, newEvalError(pos, "define-record-type expects a name, constructor, predicate, and field clauses")
	}

	typeName, ok := args[0].(*symbolExpr)
	if !ok {
		return nil, newEvalError(args[0].exprPos(), "define-record-type requires a type name")
	}

	constructorSpec, ok := args[1].(*listExpr)
	if !ok || len(constructorSpec.elements) == 0 {
		return nil, newEvalError(args[1].exprPos(), "define-record-type requires a constructor clause")
	}

	constructorName, ok := constructorSpec.elements[0].(*symbolExpr)
	if !ok {
		return nil, newEvalError(constructorSpec.elements[0].exprPos(), "record constructor name must be a symbol")
	}

	fieldIndexes := make(map[string]int, len(constructorSpec.elements)-1)
	fieldNames := make([]string, 0, len(constructorSpec.elements)-1)
	for index, fieldExpr := range constructorSpec.elements[1:] {
		fieldName, ok := fieldExpr.(*symbolExpr)
		if !ok {
			return nil, newEvalError(fieldExpr.exprPos(), "record field name must be a symbol")
		}
		if _, exists := fieldIndexes[fieldName.value]; exists {
			return nil, newEvalError(fieldName.pos, "duplicate record field: %s", fieldName.value)
		}
		fieldIndexes[fieldName.value] = index
		fieldNames = append(fieldNames, fieldName.value)
	}

	predicateName, ok := args[2].(*symbolExpr)
	if !ok {
		return nil, newEvalError(args[2].exprPos(), "record predicate name must be a symbol")
	}

	recordType := &recordType{name: typeName.value, fieldNames: fieldNames}
	env.define(constructorName.value, &recordConstructor{name: constructorName.value, recordType: recordType})
	env.define(predicateName.value, &recordPredicate{recordType: recordType})

	for _, fieldClauseExpr := range args[3:] {
		fieldClause, ok := fieldClauseExpr.(*listExpr)
		if !ok || len(fieldClause.elements) != 2 {
			return nil, newEvalError(fieldClauseExpr.exprPos(), "record field clauses must contain a field and accessor")
		}

		fieldName, ok := fieldClause.elements[0].(*symbolExpr)
		if !ok {
			return nil, newEvalError(fieldClause.elements[0].exprPos(), "record field name must be a symbol")
		}

		accessorName, ok := fieldClause.elements[1].(*symbolExpr)
		if !ok {
			return nil, newEvalError(fieldClause.elements[1].exprPos(), "record accessor name must be a symbol")
		}

		index, ok := fieldIndexes[fieldName.value]
		if !ok {
			return nil, newEvalError(fieldName.pos, "unknown record field: %s", fieldName.value)
		}

		env.define(accessorName.value, &recordAccessor{
			name:       accessorName.value,
			recordType: recordType,
			index:      index,
		})
	}

	return voidValue{}, nil
}

type letBinding struct {
	name      string
	valueExpr expr
}

type doBinding struct {
	name     string
	initExpr expr
	stepExpr expr
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

func parseDoBindings(expression expr) ([]doBinding, error) {
	bindingsList, ok := expression.(*listExpr)
	if !ok {
		return nil, newEvalError(expression.exprPos(), "do bindings must be a list")
	}

	bindings := make([]doBinding, 0, len(bindingsList.elements))
	for _, bindingExpr := range bindingsList.elements {
		binding, ok := bindingExpr.(*listExpr)
		if !ok || len(binding.elements) < 2 || len(binding.elements) > 3 {
			return nil, newEvalError(bindingExpr.exprPos(), "do bindings must contain a name, init, and optional step")
		}

		name, ok := binding.elements[0].(*symbolExpr)
		if !ok {
			return nil, newEvalError(binding.elements[0].exprPos(), "do binding name must be a symbol")
		}

		var step expr
		if len(binding.elements) == 3 {
			step = binding.elements[2]
		}

		bindings = append(bindings, doBinding{
			name:     name.value,
			initExpr: binding.elements[1],
			stepExpr: step,
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
	case *rationalExpr:
		return e.value, nil
	case *inexactExpr:
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

func expandApplyArgs(args []any, pos position, name string) ([]any, error) {
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
	return callArgs, nil
}

func applyProcedure(i *interpreter, operator any, args []any, pos position, tail bool) (any, error) {
	switch procedure := operator.(type) {
	case *lambdaProcedure:
		if tail {
			return newTailCall(procedure, args, pos), nil
		}
		result, err := procedure.Call(i, args, pos)
		if err != nil {
			return nil, err
		}
		return i.resolveTailResult(result)

	case *caseLambdaProcedure:
		clause := procedure.matchingClause(len(args))
		if clause == nil {
			return nil, newEvalError(pos, "wrong number of arguments: no matching case-lambda clause for %d argument(s)", len(args))
		}
		if tail {
			return newTailCall(clause, args, pos), nil
		}
		result, err := clause.Call(i, args, pos)
		if err != nil {
			return nil, err
		}
		return i.resolveTailResult(result)

	case *builtinProcedure:
		if tail && procedure.name == "apply" {
			callArgs, err := expandApplyArgs(args, pos, procedure.name)
			if err != nil {
				return nil, err
			}
			return applyProcedure(i, args[0], callArgs, pos, true)
		}

		result, err := procedure.Call(i, args, pos)
		if err != nil {
			return nil, err
		}
		return i.resolveTailResult(result)

	case callable:
		result, err := procedure.Call(i, args, pos)
		if err != nil {
			return nil, err
		}
		return i.resolveTailResult(result)

	default:
		return nil, newEvalError(pos, "attempt to call non-procedure")
	}
}

func isCallableValue(value any) bool {
	if _, ok := value.(*continuationProcedure); ok {
		return true
	}
	_, ok := value.(callable)
	return ok
}

func applyBuiltin(i *interpreter, name string, args []any, pos position) (any, error) {
	if isCxrProcedureName(name) {
		return applyCxr(name, args, pos)
	}

	switch name {
	case "apply":
		callArgs, err := expandApplyArgs(args, pos, name)
		if err != nil {
			return nil, err
		}
		return applyProcedure(i, args[0], callArgs, pos, false)

	case "dynamic-wind":
		if len(args) != 3 {
			return nil, newEvalError(pos, "%s expects exactly 3 arguments", name)
		}
		if !isCallableValue(args[0]) || !isCallableValue(args[1]) || !isCallableValue(args[2]) {
			return nil, newEvalError(pos, "attempt to call non-procedure")
		}
		if _, err := applyProcedure(i, args[0], nil, pos, false); err != nil {
			return nil, err
		}
		result, err := applyProcedure(i, args[1], nil, pos, false)
		if err != nil {
			return nil, err
		}
		if _, err := applyProcedure(i, args[2], nil, pos, false); err != nil {
			return nil, err
		}
		return result, nil

	case "raise":
		if len(args) != 1 {
			return nil, newEvalError(pos, "%s expects exactly 1 argument", name)
		}
		return nil, &raisedSignal{value: args[0], pos: pos}

	case "with-exception-handler":
		if len(args) != 2 {
			return nil, newEvalError(pos, "%s expects exactly 2 arguments", name)
		}
		if !isCallableValue(args[0]) || !isCallableValue(args[1]) {
			return nil, newEvalError(pos, "attempt to call non-procedure")
		}

		result, err := applyProcedure(i, args[1], nil, pos, false)
		if err == nil {
			return result, nil
		}

		raised, ok := err.(*raisedSignal)
		if !ok {
			return nil, err
		}

		return applyProcedure(i, args[0], []any{raised.value}, pos, false)

	case "eqv?":
		if len(args) != 2 {
			return nil, newEvalError(pos, "%s expects exactly 2 arguments", name)
		}
		return eqValues(args[0], args[1]), nil

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
		total := exactNumeric(0, 1)
		for _, arg := range args {
			numeric, ok := numericFromValue(arg)
			if !ok {
				return nil, newEvalError(pos, "%s expects numeric arguments", name)
			}
			total = addNumericValues(total, numeric)
		}
		return numericResult(total), nil

	case "-":
		if len(args) == 0 {
			return nil, newEvalError(pos, "%s expects at least 1 argument", name)
		}
		first, ok := numericFromValue(args[0])
		if !ok {
			return nil, newEvalError(pos, "%s expects numeric arguments", name)
		}
		if len(args) == 1 {
			return numericResult(subtractNumericValues(exactNumeric(0, 1), first)), nil
		}
		result := first
		for _, arg := range args[1:] {
			numeric, ok := numericFromValue(arg)
			if !ok {
				return nil, newEvalError(pos, "%s expects numeric arguments", name)
			}
			result = subtractNumericValues(result, numeric)
		}
		return numericResult(result), nil

	case "*":
		product := exactNumeric(1, 1)
		for _, arg := range args {
			numeric, ok := numericFromValue(arg)
			if !ok {
				return nil, newEvalError(pos, "%s expects numeric arguments", name)
			}
			product = multiplyNumericValues(product, numeric)
		}
		return numericResult(product), nil

	case "/":
		if len(args) < 2 {
			return nil, newEvalError(pos, "%s expects at least 2 arguments", name)
		}
		first, ok := numericFromValue(args[0])
		if !ok {
			return nil, newEvalError(pos, "%s expects numeric arguments", name)
		}
		result := first
		for _, arg := range args[1:] {
			numeric, ok := numericFromValue(arg)
			if !ok {
				return nil, newEvalError(pos, "%s expects numeric arguments", name)
			}
			if numeric.isZero() {
				return nil, newEvalError(pos, "division by zero")
			}
			result = divideNumericValues(result, numeric)
		}
		return numericResult(result), nil

	case "abs":
		if len(args) != 1 {
			return nil, newEvalError(pos, "%s expects exactly 1 argument", name)
		}
		numeric, ok := numericFromValue(args[0])
		if !ok {
			return nil, newEvalError(pos, "%s expects numeric arguments", name)
		}
		if numeric.exact {
			if numeric.numerator < 0 {
				numeric.numerator = -numeric.numerator
			}
			return numericResult(numeric), nil
		}
		return inexactValue(normalizeInexactFloat(math.Abs(numeric.inexact))), nil

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
		best := args[0]
		bestNumeric, ok := numericFromValue(best)
		if !ok {
			return nil, newEvalError(pos, "%s expects numeric arguments", name)
		}
		for _, arg := range args[1:] {
			value, ok := numericFromValue(arg)
			if !ok {
				return nil, newEvalError(pos, "%s expects numeric arguments", name)
			}
			if compareNumericValues(value, bestNumeric) < 0 {
				best = arg
				bestNumeric = value
			}
		}
		return best, nil

	case "max":
		if len(args) == 0 {
			return nil, newEvalError(pos, "%s expects at least 1 argument", name)
		}
		best := args[0]
		bestNumeric, ok := numericFromValue(best)
		if !ok {
			return nil, newEvalError(pos, "%s expects numeric arguments", name)
		}
		for _, arg := range args[1:] {
			value, ok := numericFromValue(arg)
			if !ok {
				return nil, newEvalError(pos, "%s expects numeric arguments", name)
			}
			if compareNumericValues(value, bestNumeric) > 0 {
				best = arg
				bestNumeric = value
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

	case "gcd":
		result := 0
		for _, arg := range args {
			value, err := expectInt(arg, pos, name)
			if err != nil {
				return nil, err
			}
			result = gcd(result, value)
		}
		return absInt(result), nil

	case "lcm":
		result := 1
		if len(args) == 0 {
			return result, nil
		}
		for _, arg := range args {
			value, err := expectInt(arg, pos, name)
			if err != nil {
				return nil, err
			}
			value = absInt(value)
			if result == 0 || value == 0 {
				result = 0
				continue
			}
			result = absInt(result/gcd(result, value) * value)
		}
		return result, nil

	case "truncate":
		if len(args) != 1 {
			return nil, newEvalError(pos, "%s expects exactly 1 argument", name)
		}
		numeric, ok := numericFromValue(args[0])
		if !ok {
			return nil, newEvalError(pos, "%s expects a number", name)
		}
		if numeric.exact {
			return numeric.numerator / numeric.denominator, nil
		}
		return inexactValue(normalizeInexactFloat(math.Trunc(numeric.inexact))), nil

	case "round":
		if len(args) != 1 {
			return nil, newEvalError(pos, "%s expects exactly 1 argument", name)
		}
		numeric, ok := numericFromValue(args[0])
		if !ok {
			return nil, newEvalError(pos, "%s expects a number", name)
		}
		rounded := math.Round(numeric.asFloat64())
		if numeric.exact {
			return int(rounded), nil
		}
		return inexactValue(normalizeInexactFloat(rounded)), nil

	case "<":
		return numericCompare(name, args, pos, func(cmp int) bool { return cmp < 0 })
	case ">":
		return numericCompare(name, args, pos, func(cmp int) bool { return cmp > 0 })
	case "=":
		return numericCompare(name, args, pos, func(cmp int) bool { return cmp == 0 })
	case "<=":
		return numericCompare(name, args, pos, func(cmp int) bool { return cmp <= 0 })
	case ">=":
		return numericCompare(name, args, pos, func(cmp int) bool { return cmp >= 0 })

	case "zero?":
		if len(args) != 1 {
			return nil, newEvalError(pos, "%s expects exactly 1 argument", name)
		}
		numeric, ok := numericFromValue(args[0])
		if !ok {
			return nil, newEvalError(pos, "%s expects numeric arguments", name)
		}
		return numeric.isZero(), nil

	case "positive?":
		if len(args) != 1 {
			return nil, newEvalError(pos, "%s expects exactly 1 argument", name)
		}
		numeric, ok := numericFromValue(args[0])
		if !ok {
			return nil, newEvalError(pos, "%s expects numeric arguments", name)
		}
		return compareNumericValues(numeric, exactNumeric(0, 1)) > 0, nil

	case "negative?":
		if len(args) != 1 {
			return nil, newEvalError(pos, "%s expects exactly 1 argument", name)
		}
		numeric, ok := numericFromValue(args[0])
		if !ok {
			return nil, newEvalError(pos, "%s expects numeric arguments", name)
		}
		return compareNumericValues(numeric, exactNumeric(0, 1)) < 0, nil

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

	case "set-car!":
		if len(args) != 2 {
			return nil, newEvalError(pos, "%s expects exactly 2 arguments", name)
		}
		pair, err := expectPair(args[0], pos, name)
		if err != nil {
			return nil, err
		}
		pair.car = args[1]
		return voidValue{}, nil

	case "set-cdr!":
		if len(args) != 2 {
			return nil, newEvalError(pos, "%s expects exactly 2 arguments", name)
		}
		pair, err := expectPair(args[0], pos, name)
		if err != nil {
			return nil, err
		}
		pair.cdr = args[1]
		return voidValue{}, nil

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

	case "reverse":
		if len(args) != 1 {
			return nil, newEvalError(pos, "%s expects exactly 1 argument", name)
		}
		elements, err := listElements(args[0], pos, name)
		if err != nil {
			return nil, err
		}
		result := any(emptyList{})
		for _, element := range elements {
			result = &pairValue{car: element, cdr: result}
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

	case "assv":
		if len(args) != 2 {
			return nil, newEvalError(pos, "%s expects exactly 2 arguments", name)
		}
		seen := map[*pairValue]struct{}{}
		for current := args[1]; ; {
			switch list := current.(type) {
			case emptyList:
				return false, nil
			case *pairValue:
				if _, ok := seen[list]; ok {
					return nil, newEvalError(pos, "%s expects a proper list", name)
				}
				seen[list] = struct{}{}
				entry, ok := list.car.(*pairValue)
				if !ok {
					return nil, newEvalError(pos, "%s expects an association list", name)
				}
				if eqValues(args[0], entry.car) {
					return list.car, nil
				}
				current = list.cdr
			default:
				return nil, newEvalError(pos, "%s expects a proper list", name)
			}
		}

	case "member":
		if len(args) != 2 {
			return nil, newEvalError(pos, "%s expects exactly 2 arguments", name)
		}
		seen := map[*pairValue]struct{}{}
		for current := args[1]; ; {
			switch list := current.(type) {
			case emptyList:
				return false, nil
			case *pairValue:
				if _, ok := seen[list]; ok {
					return nil, newEvalError(pos, "%s expects a proper list", name)
				}
				seen[list] = struct{}{}
				if equalValues(args[0], list.car) {
					return list, nil
				}
				current = list.cdr
			default:
				return nil, newEvalError(pos, "%s expects a proper list", name)
			}
		}

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
			result, err := applyProcedure(i, args[0], callArgs, pos, false)
			if err != nil {
				return nil, err
			}
			results = append(results, result)
		}
		return buildList(results), nil

	case "for-each":
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
		callArgs := make([]any, len(lists))
		for index := 0; index < expectedLen; index++ {
			for listIndex := range lists {
				callArgs[listIndex] = lists[listIndex][index]
			}
			if _, err := applyProcedure(i, args[0], callArgs, pos, false); err != nil {
				return nil, err
			}
		}
		return voidValue{}, nil

	case "vector":
		return newVectorValue(args), nil

	case "make-vector":
		if len(args) < 1 || len(args) > 2 {
			return nil, newEvalError(pos, "%s expects 1 or 2 arguments", name)
		}
		length, err := expectInt(args[0], pos, name)
		if err != nil {
			return nil, err
		}
		if length < 0 {
			return nil, newEvalError(pos, "%s expects a non-negative length", name)
		}
		fill := any(voidValue{})
		if len(args) == 2 {
			fill = args[1]
		}
		elements := make([]any, length)
		for index := range elements {
			elements[index] = fill
		}
		return &vectorValue{elements: elements}, nil

	case "vector?":
		if len(args) != 1 {
			return nil, newEvalError(pos, "%s expects exactly 1 argument", name)
		}
		_, ok := args[0].(*vectorValue)
		return ok, nil

	case "vector-length":
		if len(args) != 1 {
			return nil, newEvalError(pos, "%s expects exactly 1 argument", name)
		}
		vector, err := expectVector(args[0], pos, name)
		if err != nil {
			return nil, err
		}
		return len(vector.elements), nil

	case "vector-ref":
		if len(args) != 2 {
			return nil, newEvalError(pos, "%s expects exactly 2 arguments", name)
		}
		vector, err := expectVector(args[0], pos, name)
		if err != nil {
			return nil, err
		}
		index, err := expectInt(args[1], pos, name)
		if err != nil {
			return nil, err
		}
		if index < 0 || index >= len(vector.elements) {
			return nil, newEvalError(pos, "%s index out of range", name)
		}
		return vector.elements[index], nil

	case "vector-set!":
		if len(args) != 3 {
			return nil, newEvalError(pos, "%s expects exactly 3 arguments", name)
		}
		vector, err := expectVector(args[0], pos, name)
		if err != nil {
			return nil, err
		}
		index, err := expectInt(args[1], pos, name)
		if err != nil {
			return nil, err
		}
		if index < 0 || index >= len(vector.elements) {
			return nil, newEvalError(pos, "%s index out of range", name)
		}
		vector.elements[index] = args[2]
		return voidValue{}, nil

	case "vector->list":
		if len(args) != 1 {
			return nil, newEvalError(pos, "%s expects exactly 1 argument", name)
		}
		vector, err := expectVector(args[0], pos, name)
		if err != nil {
			return nil, err
		}
		return buildList(vector.elements), nil

	case "list->vector":
		if len(args) != 1 {
			return nil, newEvalError(pos, "%s expects exactly 1 argument", name)
		}
		elements, err := listElements(args[0], pos, name)
		if err != nil {
			return nil, err
		}
		return newVectorValue(elements), nil

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
		_, ok := numericFromValue(args[0])
		return ok, nil

	case "integer?":
		if len(args) != 1 {
			return nil, newEvalError(pos, "%s expects exactly 1 argument", name)
		}
		numeric, ok := numericFromValue(args[0])
		return ok && numeric.isInteger(), nil

	case "rational?":
		if len(args) != 1 {
			return nil, newEvalError(pos, "%s expects exactly 1 argument", name)
		}
		_, ok := numericFromValue(args[0])
		return ok, nil

	case "exact?":
		if len(args) != 1 {
			return nil, newEvalError(pos, "%s expects exactly 1 argument", name)
		}
		numeric, ok := numericFromValue(args[0])
		return ok && numeric.exact, nil

	case "inexact?":
		if len(args) != 1 {
			return nil, newEvalError(pos, "%s expects exactly 1 argument", name)
		}
		numeric, ok := numericFromValue(args[0])
		return ok && !numeric.exact, nil

	case "exact->inexact":
		if len(args) != 1 {
			return nil, newEvalError(pos, "%s expects exactly 1 argument", name)
		}
		value, ok := exactToInexact(args[0])
		if !ok {
			return nil, newEvalError(pos, "%s expects a number", name)
		}
		return value, nil

	case "inexact->exact":
		if len(args) != 1 {
			return nil, newEvalError(pos, "%s expects exactly 1 argument", name)
		}
		value, ok := inexactToExact(args[0])
		if !ok {
			return nil, newEvalError(pos, "%s expects a number", name)
		}
		return value, nil

	case "numerator":
		if len(args) != 1 {
			return nil, newEvalError(pos, "%s expects exactly 1 argument", name)
		}
		numeric, ok := numericFromValue(args[0])
		if !ok || !numeric.exact {
			return nil, newEvalError(pos, "%s expects an exact number", name)
		}
		return numeric.numerator, nil

	case "denominator":
		if len(args) != 1 {
			return nil, newEvalError(pos, "%s expects exactly 1 argument", name)
		}
		numeric, ok := numericFromValue(args[0])
		if !ok || !numeric.exact {
			return nil, newEvalError(pos, "%s expects an exact number", name)
		}
		return numeric.denominator, nil

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

	case "procedure?":
		if len(args) != 1 {
			return nil, newEvalError(pos, "%s expects exactly 1 argument", name)
		}
		if _, ok := args[0].(*continuationProcedure); ok {
			return true, nil
		}
		_, ok := args[0].(callable)
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

	case "error":
		if len(args) == 0 {
			return nil, newEvalError(pos, "%s expects at least 1 argument", name)
		}
		parts := make([]string, 0, len(args))
		for _, arg := range args {
			switch value := arg.(type) {
			case stringValue:
				parts = append(parts, string(value))
			case *mutableString:
				parts = append(parts, value.String())
			default:
				parts = append(parts, formatValue(arg))
			}
		}
		return nil, newEvalError(pos, strings.Join(parts, " "))

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

	case "make-string":
		if len(args) < 1 || len(args) > 2 {
			return nil, newEvalError(pos, "%s expects 1 or 2 arguments", name)
		}
		length, err := expectInt(args[0], pos, name)
		if err != nil {
			return nil, err
		}
		if length < 0 {
			return nil, newEvalError(pos, "%s expects a non-negative length", name)
		}
		fill := rune(' ')
		if len(args) == 2 {
			ch, err := expectChar(args[1], pos, name)
			if err != nil {
				return nil, err
			}
			fill = rune(ch)
		}
		runes := make([]rune, length)
		for index := range runes {
			runes[index] = fill
		}
		return newRuntimeString(string(runes), i.immutableStrings), nil

	case "string":
		runes := make([]rune, len(args))
		for index, arg := range args {
			ch, err := expectChar(arg, pos, name)
			if err != nil {
				return nil, err
			}
			runes[index] = rune(ch)
		}
		return newRuntimeString(string(runes), i.immutableStrings), nil

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
		value, ok := parseNumberLiteral(text)
		if !ok {
			return false, nil
		}
		return value, nil

	case "number->string":
		if len(args) != 1 {
			return nil, newEvalError(pos, "%s expects exactly 1 argument", name)
		}
		text, ok := formatNumericValue(args[0])
		if !ok {
			return nil, newEvalError(pos, "%s expects a number", name)
		}
		return stringValue(text), nil

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
		return newRuntimeString(text, i.immutableStrings), nil

	case "string->list":
		if len(args) != 1 {
			return nil, newEvalError(pos, "%s expects exactly 1 argument", name)
		}
		text, err := expectString(args[0], pos, name)
		if err != nil {
			return nil, err
		}
		runes := []rune(text)
		elements := make([]any, len(runes))
		for index, value := range runes {
			elements[index] = charValue(value)
		}
		return buildList(elements), nil

	case "list->string":
		if len(args) != 1 {
			return nil, newEvalError(pos, "%s expects exactly 1 argument", name)
		}
		elements, err := listElements(args[0], pos, name)
		if err != nil {
			return nil, err
		}
		runes := make([]rune, len(elements))
		for index, element := range elements {
			ch, err := expectChar(element, pos, name)
			if err != nil {
				return nil, err
			}
			runes[index] = rune(ch)
		}
		return stringValue(string(runes)), nil

	case "string=?":
		return stringCompare(name, args, pos, func(a, b string) bool { return a == b })

	case "string<?":
		return stringCompare(name, args, pos, func(a, b string) bool { return compareStrings(a, b) < 0 })

	case "string>?":
		return stringCompare(name, args, pos, func(a, b string) bool { return compareStrings(a, b) > 0 })

	case "string<=?":
		return stringCompare(name, args, pos, func(a, b string) bool { return compareStrings(a, b) <= 0 })

	case "string>=?":
		return stringCompare(name, args, pos, func(a, b string) bool { return compareStrings(a, b) >= 0 })

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
		text, ok := args[0].(*mutableString)
		if !ok {
			if _, err := expectString(args[0], pos, name); err != nil {
				return nil, err
			}
			if _, err := expectInt(args[1], pos, name); err != nil {
				return nil, err
			}
			if _, err := expectChar(args[2], pos, name); err != nil {
				return nil, err
			}
			return nil, newEvalError(pos, "%s cannot mutate immutable strings", name)
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

	case "char->integer":
		if len(args) != 1 {
			return nil, newEvalError(pos, "%s expects exactly 1 argument", name)
		}
		ch, err := expectChar(args[0], pos, name)
		if err != nil {
			return nil, err
		}
		return int(rune(ch)), nil

	case "integer->char":
		if len(args) != 1 {
			return nil, newEvalError(pos, "%s expects exactly 1 argument", name)
		}
		value, err := expectInt(args[0], pos, name)
		if err != nil {
			return nil, err
		}
		if value < 0 || value > 0x10FFFF || (value >= 0xD800 && value <= 0xDFFF) {
			return nil, newEvalError(pos, "%s expects a valid Unicode scalar value", name)
		}
		return charValue(rune(value)), nil

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

func numericCompare(name string, args []any, pos position, compare func(int) bool) (bool, error) {
	if len(args) < 2 {
		return false, newEvalError(pos, "%s expects at least 2 arguments", name)
	}

	prev, ok := numericFromValue(args[0])
	if !ok {
		return false, newEvalError(pos, "%s expects numeric arguments", name)
	}

	for _, arg := range args[1:] {
		current, ok := numericFromValue(arg)
		if !ok {
			return false, newEvalError(pos, "%s expects numeric arguments", name)
		}
		if !compare(compareNumericValues(prev, current)) {
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

func isCxrProcedureName(name string) bool {
	if len(name) < 4 || len(name) > 6 || name[0] != 'c' || name[len(name)-1] != 'r' {
		return false
	}
	for index := 1; index < len(name)-1; index++ {
		if name[index] != 'a' && name[index] != 'd' {
			return false
		}
	}
	return true
}

func applyCxr(name string, args []any, pos position) (any, error) {
	if len(args) != 1 {
		return nil, newEvalError(pos, "%s expects exactly 1 argument", name)
	}

	current := args[0]
	for index := len(name) - 2; index >= 1; index-- {
		pair, err := expectPair(current, pos, name)
		if err != nil {
			return nil, err
		}
		if name[index] == 'a' {
			current = pair.car
		} else {
			current = pair.cdr
		}
	}
	return current, nil
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
	slow := value
	fast := value

	for {
		switch current := fast.(type) {
		case emptyList:
			return true
		case *pairValue:
			fast = current.cdr
		default:
			return false
		}

		switch current := fast.(type) {
		case emptyList:
			return true
		case *pairValue:
			fast = current.cdr
		default:
			return false
		}

		slowPair, ok := slow.(*pairValue)
		if !ok {
			return false
		}
		slow = slowPair.cdr
		if slow == fast {
			return false
		}
	}
}

func eqValues(left, right any) bool {
	if leftNumeric, ok := numericFromValue(left); ok {
		rightNumeric, ok := numericFromValue(right)
		return ok && compareNumericValues(leftNumeric, rightNumeric) == 0
	}

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
	case voidValue:
		_, ok := right.(voidValue)
		return ok
	case emptyList:
		_, ok := right.(emptyList)
		return ok
	case *mutableString:
		r, ok := right.(*mutableString)
		return ok && l == r
	case *vectorValue:
		r, ok := right.(*vectorValue)
		return ok && l == r
	case *pairValue:
		r, ok := right.(*pairValue)
		return ok && l == r
	case *recordValue:
		r, ok := right.(*recordValue)
		return ok && l == r
	case *builtinProcedure:
		r, ok := right.(*builtinProcedure)
		return ok && l == r
	case *lambdaProcedure:
		r, ok := right.(*lambdaProcedure)
		return ok && l == r
	case *caseLambdaProcedure:
		r, ok := right.(*caseLambdaProcedure)
		return ok && l == r
	case *continuationProcedure:
		r, ok := right.(*continuationProcedure)
		return ok && l == r
	default:
		return false
	}
}

func equalValues(left, right any) bool {
	return equalValuesSeen(
		left,
		right,
		map[pairComparison]struct{}{},
		map[vectorComparison]struct{}{},
	)
}

func equalValuesSeen(left, right any, seenPairs map[pairComparison]struct{}, seenVectors map[vectorComparison]struct{}) bool {
	if leftNumeric, ok := numericFromValue(left); ok {
		rightNumeric, ok := numericFromValue(right)
		return ok && compareNumericValues(leftNumeric, rightNumeric) == 0
	}

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
	case voidValue:
		_, ok := right.(voidValue)
		return ok
	case emptyList:
		_, ok := right.(emptyList)
		return ok
	case *pairValue:
		r, ok := right.(*pairValue)
		if !ok {
			return false
		}
		key := pairComparison{left: l, right: r}
		if _, ok := seenPairs[key]; ok {
			return true
		}
		seenPairs[key] = struct{}{}
		return equalValuesSeen(l.car, r.car, seenPairs, seenVectors) &&
			equalValuesSeen(l.cdr, r.cdr, seenPairs, seenVectors)
	case *vectorValue:
		r, ok := right.(*vectorValue)
		if !ok || len(l.elements) != len(r.elements) {
			return false
		}
		key := vectorComparison{left: l, right: r}
		if _, ok := seenVectors[key]; ok {
			return true
		}
		seenVectors[key] = struct{}{}
		for index := range l.elements {
			if !equalValuesSeen(l.elements[index], r.elements[index], seenPairs, seenVectors) {
				return false
			}
		}
		return true
	case *recordValue:
		r, ok := right.(*recordValue)
		return ok && l == r
	case *builtinProcedure:
		r, ok := right.(*builtinProcedure)
		return ok && l == r
	case *lambdaProcedure:
		r, ok := right.(*lambdaProcedure)
		return ok && l == r
	case *caseLambdaProcedure:
		r, ok := right.(*caseLambdaProcedure)
		return ok && l == r
	case *continuationProcedure:
		r, ok := right.(*continuationProcedure)
		return ok && l == r
	default:
		return false
	}
}

func expectInt(value any, pos position, procedure string) (int, error) {
	numeric, ok := numericFromValue(value)
	if !ok || !numeric.exact || !numeric.isInteger() {
		return 0, newEvalError(pos, "%s expects numeric arguments", procedure)
	}
	return numeric.numerator, nil
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

func expectVector(value any, pos position, procedure string) (*vectorValue, error) {
	vector, ok := value.(*vectorValue)
	if !ok {
		return nil, newEvalError(pos, "%s expects a vector", procedure)
	}
	return vector, nil
}

func listElements(value any, pos position, procedure string) ([]any, error) {
	elements := []any{}
	seen := map[*pairValue]struct{}{}
	for {
		switch current := value.(type) {
		case emptyList:
			return elements, nil
		case *pairValue:
			if _, ok := seen[current]; ok {
				return nil, newEvalError(pos, "%s expects a proper list", procedure)
			}
			seen[current] = struct{}{}
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
	return formatValueWithState(value, &formatState{
		pairs:   map[*pairValue]struct{}{},
		vectors: map[*vectorValue]struct{}{},
	})
}

func formatValueWithState(value any, state *formatState) string {
	if text, ok := formatNumericValue(value); ok {
		return text
	}

	switch v := value.(type) {
	case nil:
		return ""
	case voidValue:
		return ""
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
	case *vectorValue:
		return formatVectorWithState(v, state)
	case *pairValue:
		return formatPairWithState(v, state)
	case *recordValue:
		return fmt.Sprintf("#<record %s>", v.recordType.name)
	case *continuationProcedure:
		return "#<procedure>"
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
	return formatPairWithState(pair, &formatState{
		pairs:   map[*pairValue]struct{}{},
		vectors: map[*vectorValue]struct{}{},
	})
}

func formatPairWithState(pair *pairValue, state *formatState) string {
	if _, ok := state.pairs[pair]; ok {
		return "#<circular>"
	}
	state.pairs[pair] = struct{}{}
	defer delete(state.pairs, pair)

	var builder strings.Builder
	builder.WriteByte('(')
	formatPairElements(pair, &builder, state)
	builder.WriteByte(')')
	return builder.String()
}

func formatPairElements(pair *pairValue, builder *strings.Builder, state *formatState) {
	builder.WriteString(formatValueWithState(pair.car, state))

	switch next := pair.cdr.(type) {
	case emptyList:
		return
	case *pairValue:
		if _, ok := state.pairs[next]; ok {
			builder.WriteString(" . #<circular>")
			return
		}
		state.pairs[next] = struct{}{}
		builder.WriteByte(' ')
		formatPairElements(next, builder, state)
		delete(state.pairs, next)
	default:
		builder.WriteString(" . ")
		builder.WriteString(formatValueWithState(next, state))
	}
}

func formatVector(vector *vectorValue) string {
	return formatVectorWithState(vector, &formatState{
		pairs:   map[*pairValue]struct{}{},
		vectors: map[*vectorValue]struct{}{},
	})
}

func formatVectorWithState(vector *vectorValue, state *formatState) string {
	if _, ok := state.vectors[vector]; ok {
		return "#<circular>"
	}
	state.vectors[vector] = struct{}{}
	defer delete(state.vectors, vector)

	var builder strings.Builder
	builder.WriteString("#(")

	for index, element := range vector.elements {
		if index > 0 {
			builder.WriteByte(' ')
		}
		builder.WriteString(formatValueWithState(element, state))
	}

	builder.WriteByte(')')
	return builder.String()
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

	if value, ok := parseNumberLiteral(tok.text); ok {
		switch number := value.(type) {
		case int:
			return &integerExpr{value: number, pos: tok.pos}
		case rationalValue:
			return &rationalExpr{value: number, pos: tok.pos}
		case inexactValue:
			return &inexactExpr{value: number, pos: tok.pos}
		}
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
