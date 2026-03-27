package ming

import (
	"strconv"
	"strings"
)

type value interface {
	schemeString() string
	isTruthy() bool
}

type procedure interface {
	value
	call(args []value) (value, error)
}

type boolValue bool
type stringValue struct {
	chars   []rune
	mutable bool
}
type symbolValue string
type charValue rune
type voidValue struct{}
type emptyListValue struct{}

type pairValue struct {
	car value
	cdr value
}

type builtinProc struct {
	name string
	fn   func(args []value) (value, error)
}

type closureValue struct {
	params    []string
	restParam string
	hasRest   bool
	body      []locatedExpr
	env       *env
}

type caseClosureValue struct {
	clauses []closureValue
}

type letBinding struct {
	name string
	init locatedExpr
}

type env struct {
	parent *env
	vars   map[string]*binding
	macros map[string]*macroBinding
}

type outputMode int

const (
	outputModeWrite outputMode = iota
	outputModeDisplay
)

var emptyList = emptyListValue{}
var currentOutput *strings.Builder

func newStringValue(text string) *stringValue {
	return &stringValue{
		chars:   []rune(text),
		mutable: true,
	}
}

func copyStringValue(s *stringValue, mutable bool) *stringValue {
	chars := make([]rune, len(s.chars))
	copy(chars, s.chars)
	return &stringValue{
		chars:   chars,
		mutable: mutable,
	}
}

func (s *stringValue) text() string {
	return string(s.chars)
}

func (b boolValue) schemeString() string {
	if b {
		return "#t"
	}
	return "#f"
}

func (b boolValue) isTruthy() bool {
	return bool(b)
}

func (s *stringValue) schemeString() string {
	return strconv.Quote(s.text())
}

func (*stringValue) isTruthy() bool {
	return true
}

func (s symbolValue) schemeString() string {
	return string(s)
}

func (symbolValue) isTruthy() bool {
	return true
}

func (c charValue) schemeString() string {
	return formatChar(rune(c), outputModeWrite)
}

func (charValue) isTruthy() bool {
	return true
}

func (voidValue) schemeString() string {
	return ""
}

func (voidValue) isTruthy() bool {
	return true
}

func (emptyListValue) schemeString() string {
	return "()"
}

func (emptyListValue) isTruthy() bool {
	return true
}

func (p pairValue) schemeString() string {
	return formatPair(p, outputModeWrite)
}

func formatValue(v value, mode outputMode) string {
	switch value := v.(type) {
	case *stringValue:
		if mode == outputModeDisplay {
			return value.text()
		}
		return strconv.Quote(value.text())
	case charValue:
		return formatChar(rune(value), mode)
	case pairValue:
		return formatPair(value, mode)
	default:
		return v.schemeString()
	}
}

func formatPair(p pairValue, mode outputMode) string {
	var builder strings.Builder
	builder.WriteByte('(')

	current := value(p)
	for {
		switch next := current.(type) {
		case pairValue:
			builder.WriteString(formatValue(next.car, mode))
			current = next.cdr
			if _, ok := current.(pairValue); ok {
				builder.WriteByte(' ')
			}
		case emptyListValue:
			builder.WriteByte(')')
			return builder.String()
		default:
			builder.WriteString(" . ")
			builder.WriteString(formatValue(next, mode))
			builder.WriteByte(')')
			return builder.String()
		}
	}
}

func formatChar(ch rune, mode outputMode) string {
	if mode == outputModeDisplay {
		return string(ch)
	}

	switch ch {
	case ' ':
		return "#\\space"
	case '\n':
		return "#\\newline"
	default:
		return "#\\" + string(ch)
	}
}

func (pairValue) isTruthy() bool {
	return true
}

func (p builtinProc) schemeString() string {
	return "#<procedure:" + p.name + ">"
}

func (builtinProc) isTruthy() bool {
	return true
}

func (p builtinProc) call(args []value) (value, error) {
	return p.fn(args)
}

func (closureValue) schemeString() string {
	return "#<procedure>"
}

func (closureValue) isTruthy() bool {
	return true
}

func (p closureValue) acceptsArgCount(argCount int) bool {
	if p.hasRest {
		return argCount >= len(p.params)
	}
	return argCount == len(p.params)
}

func (p closureValue) call(args []value) (value, error) {
	if !p.acceptsArgCount(len(args)) {
		if p.hasRest {
			return nil, newCurrentEvalError("expected at least %d arguments, got %d", len(p.params), len(args))
		}
		return nil, newCurrentEvalError("expected %d arguments, got %d", len(p.params), len(args))
	}

	callEnv := newEnv(p.env)
	for i, name := range p.params {
		callEnv.define(name, args[i])
	}
	if p.hasRest {
		callEnv.define(p.restParam, listFromValues(args[len(p.params):]))
	}

	return evalSequence(p.body, callEnv)
}

func (caseClosureValue) schemeString() string {
	return "#<procedure>"
}

func (caseClosureValue) isTruthy() bool {
	return true
}

func (p caseClosureValue) call(args []value) (value, error) {
	for _, clause := range p.clauses {
		if clause.acceptsArgCount(len(args)) {
			return clause.call(args)
		}
	}

	return nil, newCurrentEvalError("no matching case-lambda clause for %d arguments", len(args))
}

func newEnv(parent *env) *env {
	return &env{
		parent: parent,
		vars:   make(map[string]*binding),
		macros: make(map[string]*macroBinding),
	}
}

func (e *env) define(name string, v value) {
	e.defineBinding(name, &binding{value: v})
}

func (e *env) defineBinding(name string, b *binding) {
	e.vars[name] = b
}

func (e *env) set(name string, v value) bool {
	for current := e; current != nil; current = current.parent {
		if binding, ok := current.vars[name]; ok {
			binding.value = v
			return true
		}
	}
	return false
}

func (e *env) lookup(name string) (value, bool) {
	binding, ok := e.lookupBinding(name)
	if !ok {
		return nil, false
	}
	return binding.value, true
}

func (e *env) lookupBinding(name string) (*binding, bool) {
	for current := e; current != nil; current = current.parent {
		if binding, ok := current.vars[name]; ok {
			return binding, true
		}
	}
	return nil, false
}

func (e *env) defineMacro(name string, transformer *syntaxRulesMacro) {
	e.defineMacroBinding(name, &macroBinding{transformer: transformer})
}

func (e *env) defineMacroBinding(name string, macro *macroBinding) {
	e.macros[name] = macro
}

func (e *env) lookupMacro(name string) (*macroBinding, bool) {
	for current := e; current != nil; current = current.parent {
		if macro, ok := current.macros[name]; ok {
			return macro, true
		}
	}
	return nil, false
}

func newGlobalEnv() *env {
	global := newEnv(nil)
	global.define("+", builtinProc{name: "+", fn: evalAdd})
	global.define("-", builtinProc{name: "-", fn: evalSub})
	global.define("*", builtinProc{name: "*", fn: evalMul})
	global.define("/", builtinProc{name: "/", fn: evalDiv})
	global.define("<", builtinProc{name: "<", fn: func(args []value) (value, error) {
		return evalCompare(args, "<", func(a, b numberValue) bool { return a.compare(b) < 0 })
	}})
	global.define(">", builtinProc{name: ">", fn: func(args []value) (value, error) {
		return evalCompare(args, ">", func(a, b numberValue) bool { return a.compare(b) > 0 })
	}})
	global.define("=", builtinProc{name: "=", fn: func(args []value) (value, error) {
		return evalCompare(args, "=", func(a, b numberValue) bool { return a.equal(b) })
	}})
	global.define("<=", builtinProc{name: "<=", fn: func(args []value) (value, error) {
		return evalCompare(args, "<=", func(a, b numberValue) bool { return a.compare(b) <= 0 })
	}})
	global.define("not", builtinProc{name: "not", fn: evalNot})
	global.define("cons", builtinProc{name: "cons", fn: evalCons})
	global.define("car", builtinProc{name: "car", fn: evalCar})
	global.define("cdr", builtinProc{name: "cdr", fn: evalCdr})
	global.define("append", builtinProc{name: "append", fn: evalAppend})
	global.define("list", builtinProc{name: "list", fn: evalListBuiltin})
	global.define("length", builtinProc{name: "length", fn: evalLength})
	global.define("null?", builtinProc{name: "null?", fn: evalNullPred})
	global.define("number?", builtinProc{name: "number?", fn: evalNumberPred})
	global.define("boolean?", builtinProc{name: "boolean?", fn: evalBooleanPred})
	global.define("string?", builtinProc{name: "string?", fn: evalStringPred})
	global.define("pair?", builtinProc{name: "pair?", fn: evalPairPred})
	global.define("symbol?", builtinProc{name: "symbol?", fn: evalSymbolPred})
	global.define("char?", builtinProc{name: "char?", fn: evalCharPred})
	global.define("procedure?", builtinProc{name: "procedure?", fn: evalProcedurePred})
	global.define("display", builtinProc{name: "display", fn: evalDisplay})
	global.define("write", builtinProc{name: "write", fn: evalWrite})
	global.define("newline", builtinProc{name: "newline", fn: evalNewline})
	global.define("apply", builtinProc{name: "apply", fn: evalApply})
	global.define("string-append", builtinProc{name: "string-append", fn: evalStringAppend})
	global.define("string-length", builtinProc{name: "string-length", fn: evalStringLength})
	global.define("substring", builtinProc{name: "substring", fn: evalSubstring})
	global.define("string->number", builtinProc{name: "string->number", fn: evalStringToNumber})
	global.define("number->string", builtinProc{name: "number->string", fn: evalNumberToString})
	global.define("symbol->string", builtinProc{name: "symbol->string", fn: evalSymbolToString})
	global.define("string->symbol", builtinProc{name: "string->symbol", fn: evalStringToSymbol})
	global.define("string-ref", builtinProc{name: "string-ref", fn: evalStringRef})
	global.define("string-copy", builtinProc{name: "string-copy", fn: evalStringCopy})
	global.define("string-set!", builtinProc{name: "string-set!", fn: evalStringSet})
	registerLevel09Builtins(global)
	registerLevel11Builtins(global)
	return global
}

func evalInput(input string) (result string, output string, err error) {
	exprs, err := parseProgram(input)
	if err != nil {
		return "", "", err
	}

	env := newGlobalEnv()
	var outputBuilder strings.Builder
	restoreOutput := pushOutputBuffer(&outputBuilder)
	defer restoreOutput()
	restore := pushEvalPos(defaultSourcePos())
	defer restore()

	last := value(voidValue{})

	for _, expr := range exprs {
		last, err = evalExpr(expr, env)
		if err != nil {
			return "", "", err
		}
	}

	return last.schemeString(), outputBuilder.String(), nil
}

func evalSequence(exprs []locatedExpr, env *env) (value, error) {
	last := value(voidValue{})
	for _, expr := range exprs {
		var err error
		last, err = evalExpr(expr, env)
		if err != nil {
			return nil, err
		}
	}
	return last, nil
}

func evalExpr(e locatedExpr, env *env) (value, error) {
	restore := pushEvalPos(e.pos)
	defer restore()

	switch expr := e.form.(type) {
	case numberExpr:
		return expr, nil
	case boolExpr:
		return boolValue(expr), nil
	case stringExpr:
		return newStringValue(string(expr)), nil
	case charExpr:
		return charValue(expr), nil
	case symbolExpr:
		v, ok := env.lookup(string(expr))
		if !ok {
			return nil, newCurrentEvalError("unbound variable: %s", string(expr))
		}
		return v, nil
	case listExpr:
		return evalList(expr, env)
	default:
		return nil, newCurrentEvalError("unknown expression")
	}
}

func evalList(items listExpr, env *env) (value, error) {
	if len(items) == 0 {
		return nil, newCurrentEvalError("cannot evaluate empty list")
	}

	if operator, ok := items[0].form.(symbolExpr); ok {
		switch string(operator) {
		case "and":
			return evalAnd(items[1:], env)
		case "or":
			return evalOr(items[1:], env)
		case "begin":
			return evalSequence(items[1:], env)
		case "if":
			return evalIf(items[1:], env)
		case "cond":
			return evalCond(items[1:], env)
		case "define":
			return evalDefine(items[1:], env)
		case "define-syntax":
			return evalDefineSyntax(items[1:], env)
		case "define-record-type":
			return evalDefineRecordType(items[1:], env)
		case "set!":
			return evalSet(items[1:], env)
		case "quote":
			return evalQuote(items[1:])
		case "let":
			return evalLet(items[1:], env)
		case "lambda":
			return evalLambda(items[1:], env)
		case "case-lambda":
			return evalCaseLambda(items[1:], env)
		}

		if macro, found := env.lookupMacro(string(operator)); found {
			expanded, expansionEnv, err := expandMacroCall(items, macro, env)
			if err != nil {
				return nil, err
			}
			return evalExpr(expanded, expansionEnv)
		}
	}

	operator, err := evalExpr(items[0], env)
	if err != nil {
		return nil, err
	}

	proc, ok := operator.(procedure)
	if !ok {
		restore := pushEvalPos(items[0].pos)
		defer restore()
		return nil, newCurrentEvalError("attempt to call non-procedure: %s", operator.schemeString())
	}

	args := make([]value, 0, len(items)-1)
	for _, item := range items[1:] {
		arg, err := evalExpr(item, env)
		if err != nil {
			return nil, err
		}
		args = append(args, arg)
	}

	restore := pushEvalPos(items[0].pos)
	defer restore()
	return proc.call(args)
}

func evalAnd(items []locatedExpr, env *env) (value, error) {
	result := value(boolValue(true))
	for _, item := range items {
		next, err := evalExpr(item, env)
		if err != nil {
			return nil, err
		}
		result = next
		if !next.isTruthy() {
			return next, nil
		}
	}
	return result, nil
}

func evalOr(items []locatedExpr, env *env) (value, error) {
	for _, item := range items {
		next, err := evalExpr(item, env)
		if err != nil {
			return nil, err
		}
		if next.isTruthy() {
			return next, nil
		}
	}
	return boolValue(false), nil
}

func evalIf(parts []locatedExpr, env *env) (value, error) {
	if len(parts) != 2 && len(parts) != 3 {
		return nil, newCurrentEvalError("'if' expects 2 or 3 arguments")
	}

	cond, err := evalExpr(parts[0], env)
	if err != nil {
		return nil, err
	}

	if cond.isTruthy() {
		return evalExpr(parts[1], env)
	}

	if len(parts) == 3 {
		return evalExpr(parts[2], env)
	}

	return voidValue{}, nil
}

func evalDefine(parts []locatedExpr, env *env) (value, error) {
	if len(parts) < 2 {
		return nil, newCurrentEvalError("'define' expects at least 2 arguments")
	}

	switch target := parts[0].form.(type) {
	case symbolExpr:
		if len(parts) != 2 {
			return nil, newCurrentEvalError("'define' expects exactly 2 arguments for variable definitions")
		}

		v, err := evalExpr(parts[1], env)
		if err != nil {
			return nil, err
		}
		env.define(string(target), v)
		return voidValue{}, nil
	case listExpr:
		if len(target) == 0 {
			return nil, newCurrentEvalError("function name is required")
		}

		name, ok := target[0].form.(symbolExpr)
		if !ok {
			return nil, newEvalError(target[0].pos, "function name must be a symbol")
		}

		formals, err := parseFormalsList(target[1:])
		if err != nil {
			return nil, err
		}

		proc := closureValue{
			params:    formals.params,
			restParam: formals.restParam,
			hasRest:   formals.hasRest,
			body:      parts[1:],
			env:       env,
		}
		env.define(string(name), proc)
		return voidValue{}, nil
	default:
		return nil, newCurrentEvalError("invalid define target")
	}
}

func evalSet(parts []locatedExpr, env *env) (value, error) {
	if len(parts) != 2 {
		return nil, newCurrentEvalError("'set!' expects exactly 2 arguments")
	}

	name, ok := parts[0].form.(symbolExpr)
	if !ok {
		return nil, newEvalError(parts[0].pos, "'set!' target must be a symbol")
	}

	v, err := evalExpr(parts[1], env)
	if err != nil {
		return nil, err
	}

	if !env.set(string(name), v) {
		return nil, newEvalError(parts[0].pos, "unbound variable: %s", string(name))
	}

	return voidValue{}, nil
}

func evalQuote(parts []locatedExpr) (value, error) {
	if len(parts) != 1 {
		return nil, newCurrentEvalError("'quote' expects exactly 1 argument")
	}
	return quoteExpr(parts[0])
}

func evalLambda(parts []locatedExpr, env *env) (value, error) {
	if len(parts) < 2 {
		return nil, newCurrentEvalError("'lambda' expects a parameter list and body")
	}

	formals, err := parseFormalsExpr(parts[0])
	if err != nil {
		return nil, err
	}

	return closureValue{
		params:    formals.params,
		restParam: formals.restParam,
		hasRest:   formals.hasRest,
		body:      parts[1:],
		env:       env,
	}, nil
}

func evalCaseLambda(parts []locatedExpr, env *env) (value, error) {
	if len(parts) == 0 {
		return nil, newCurrentEvalError("'case-lambda' expects at least 1 clause")
	}

	clauses := make([]closureValue, 0, len(parts))
	for _, clauseExpr := range parts {
		clause, ok := clauseExpr.form.(listExpr)
		if !ok || len(clause) < 2 {
			return nil, newEvalError(clauseExpr.pos, "'case-lambda' clauses must have a parameter list and body")
		}

		formals, err := parseFormalsExpr(clause[0])
		if err != nil {
			return nil, err
		}

		clauses = append(clauses, closureValue{
			params:    formals.params,
			restParam: formals.restParam,
			hasRest:   formals.hasRest,
			body:      clause[1:],
			env:       env,
		})
	}

	return caseClosureValue{clauses: clauses}, nil
}

func evalCond(clauses []locatedExpr, env *env) (value, error) {
	for i, clauseExpr := range clauses {
		clause, ok := clauseExpr.form.(listExpr)
		if !ok || len(clause) == 0 {
			return nil, newEvalError(clauseExpr.pos, "'cond' clauses must be non-empty lists")
		}

		if keyword, ok := clause[0].form.(symbolExpr); ok && string(keyword) == "else" {
			if i != len(clauses)-1 {
				return nil, newEvalError(clause[0].pos, "'cond' else clause must be last")
			}
			if len(clause) == 1 {
				return voidValue{}, nil
			}
			return evalSequence(clause[1:], env)
		}

		test, err := evalExpr(clause[0], env)
		if err != nil {
			return nil, err
		}
		if test.isTruthy() {
			if len(clause) == 1 {
				return test, nil
			}
			return evalSequence(clause[1:], env)
		}
	}

	return voidValue{}, nil
}

func evalLet(parts []locatedExpr, env *env) (value, error) {
	if len(parts) < 2 {
		return nil, newCurrentEvalError("'let' expects bindings and a body")
	}

	if name, ok := parts[0].form.(symbolExpr); ok {
		if len(parts) < 3 {
			return nil, newCurrentEvalError("named 'let' expects bindings and a body")
		}

		bindingExprs, ok := parts[1].form.(listExpr)
		if !ok {
			return nil, newEvalError(parts[1].pos, "'let' bindings must be a list")
		}

		bindings, err := parseLetBindings(bindingExprs)
		if err != nil {
			return nil, err
		}

		args := make([]value, 0, len(bindings))
		params := make([]string, 0, len(bindings))
		for _, binding := range bindings {
			arg, err := evalExpr(binding.init, env)
			if err != nil {
				return nil, err
			}
			args = append(args, arg)
			params = append(params, binding.name)
		}

		letEnv := newEnv(env)
		proc := closureValue{
			params: params,
			body:   parts[2:],
			env:    letEnv,
		}
		letEnv.define(string(name), proc)
		return proc.call(args)
	}

	bindingExprs, ok := parts[0].form.(listExpr)
	if !ok {
		return nil, newEvalError(parts[0].pos, "'let' bindings must be a list")
	}

	bindings, err := parseLetBindings(bindingExprs)
	if err != nil {
		return nil, err
	}

	letEnv := newEnv(env)
	for _, binding := range bindings {
		v, err := evalExpr(binding.init, env)
		if err != nil {
			return nil, err
		}
		letEnv.define(binding.name, v)
	}

	return evalSequence(parts[1:], letEnv)
}

type formalsSpec struct {
	params    []string
	restParam string
	hasRest   bool
}

func parseFormalsExpr(e locatedExpr) (formalsSpec, error) {
	switch formals := e.form.(type) {
	case listExpr:
		return parseFormalsList(formals)
	case symbolExpr:
		if string(formals) == "." {
			return formalsSpec{}, newEvalError(e.pos, "parameter name must be a symbol")
		}
		return formalsSpec{
			restParam: string(formals),
			hasRest:   true,
		}, nil
	default:
		return formalsSpec{}, newEvalError(e.pos, "'lambda' parameter list must be a list or symbol")
	}
}

func parseFormalsList(items []locatedExpr) (formalsSpec, error) {
	formals := formalsSpec{
		params: make([]string, 0, len(items)),
	}
	seen := make(map[string]struct{}, len(items))

	addName := func(item locatedExpr) (string, error) {
		name, ok := item.form.(symbolExpr)
		if !ok || string(name) == "." {
			return "", newEvalError(item.pos, "parameter name must be a symbol")
		}
		if _, exists := seen[string(name)]; exists {
			return "", newEvalError(item.pos, "duplicate parameter: %s", string(name))
		}
		seen[string(name)] = struct{}{}
		return string(name), nil
	}

	for i, item := range items {
		name, ok := item.form.(symbolExpr)
		if ok && string(name) == "." {
			if formals.hasRest || i != len(items)-2 {
				return formalsSpec{}, newEvalError(item.pos, "invalid dotted parameter list")
			}

			restName, err := addName(items[i+1])
			if err != nil {
				return formalsSpec{}, err
			}
			formals.restParam = restName
			formals.hasRest = true
			return formals, nil
		}

		paramName, err := addName(item)
		if err != nil {
			return formalsSpec{}, err
		}
		formals.params = append(formals.params, paramName)
	}

	return formals, nil
}

func parseLetBindings(items listExpr) ([]letBinding, error) {
	bindings := make([]letBinding, 0, len(items))
	seen := make(map[string]struct{}, len(items))

	for _, item := range items {
		binding, ok := item.form.(listExpr)
		if !ok || len(binding) != 2 {
			return nil, newEvalError(item.pos, "'let' bindings must be (name value) pairs")
		}

		name, ok := binding[0].form.(symbolExpr)
		if !ok {
			return nil, newEvalError(binding[0].pos, "'let' binding names must be symbols")
		}

		if _, exists := seen[string(name)]; exists {
			return nil, newEvalError(binding[0].pos, "duplicate binding: %s", string(name))
		}
		seen[string(name)] = struct{}{}

		bindings = append(bindings, letBinding{
			name: string(name),
			init: binding[1],
		})
	}

	return bindings, nil
}

func quoteExpr(e locatedExpr) (value, error) {
	switch expr := e.form.(type) {
	case numberExpr:
		return expr, nil
	case boolExpr:
		return boolValue(expr), nil
	case stringExpr:
		return newStringValue(string(expr)), nil
	case charExpr:
		return charValue(expr), nil
	case symbolExpr:
		return symbolValue(expr), nil
	case listExpr:
		return quoteList(expr)
	default:
		return nil, newEvalError(e.pos, "unknown quoted expression")
	}
}

func quoteList(items listExpr) (value, error) {
	result := value(emptyList)
	for i := len(items) - 1; i >= 0; i-- {
		v, err := quoteExpr(items[i])
		if err != nil {
			return nil, err
		}
		result = pairValue{
			car: v,
			cdr: result,
		}
	}
	return result, nil
}

func evalAdd(args []value) (value, error) {
	sum := newExactInteger(0)
	for _, arg := range args {
		n, err := expectNumber(arg)
		if err != nil {
			return nil, err
		}
		sum = sum.add(n)
	}
	return sum, nil
}

func evalSub(args []value) (value, error) {
	if len(args) == 0 {
		return nil, newCurrentEvalError("'-' expects at least 1 argument")
	}

	first, err := expectNumber(args[0])
	if err != nil {
		return nil, err
	}
	if len(args) == 1 {
		return first.negate(), nil
	}

	result := first
	for _, arg := range args[1:] {
		n, err := expectNumber(arg)
		if err != nil {
			return nil, err
		}
		result = result.sub(n)
	}
	return result, nil
}

func evalMul(args []value) (value, error) {
	product := newExactInteger(1)
	for _, arg := range args {
		n, err := expectNumber(arg)
		if err != nil {
			return nil, err
		}
		product = product.mul(n)
	}
	return product, nil
}

func evalDiv(args []value) (value, error) {
	if len(args) < 2 {
		return nil, newCurrentEvalError("'/' expects at least 2 arguments")
	}

	first, err := expectNumber(args[0])
	if err != nil {
		return nil, err
	}

	result := first
	for _, arg := range args[1:] {
		n, err := expectNumber(arg)
		if err != nil {
			return nil, err
		}
		if n.numer == 0 {
			return nil, newCurrentEvalError("division by zero")
		}
		result = result.div(n)
	}
	return result, nil
}

func evalCompare(args []value, name string, pred func(numberValue, numberValue) bool) (value, error) {
	if len(args) < 2 {
		return nil, newCurrentEvalError("'%s' expects at least 2 arguments", name)
	}

	prev, err := expectNumber(args[0])
	if err != nil {
		return nil, err
	}

	for _, arg := range args[1:] {
		next, err := expectNumber(arg)
		if err != nil {
			return nil, err
		}
		if !pred(prev, next) {
			return boolValue(false), nil
		}
		prev = next
	}

	return boolValue(true), nil
}

func evalNot(args []value) (value, error) {
	if len(args) != 1 {
		return nil, newCurrentEvalError("'not' expects exactly 1 argument")
	}
	return boolValue(!args[0].isTruthy()), nil
}

func evalCons(args []value) (value, error) {
	if len(args) != 2 {
		return nil, newCurrentEvalError("'cons' expects exactly 2 arguments")
	}
	return pairValue{car: args[0], cdr: args[1]}, nil
}

func evalCar(args []value) (value, error) {
	if len(args) != 1 {
		return nil, newCurrentEvalError("'car' expects exactly 1 argument")
	}

	pair, ok := args[0].(pairValue)
	if !ok {
		return nil, newCurrentEvalError("'car' expects a pair, got %s", args[0].schemeString())
	}

	return pair.car, nil
}

func evalCdr(args []value) (value, error) {
	if len(args) != 1 {
		return nil, newCurrentEvalError("'cdr' expects exactly 1 argument")
	}

	pair, ok := args[0].(pairValue)
	if !ok {
		return nil, newCurrentEvalError("'cdr' expects a pair, got %s", args[0].schemeString())
	}

	return pair.cdr, nil
}

func evalAppend(args []value) (value, error) {
	if len(args) == 0 {
		return emptyList, nil
	}
	if len(args) == 1 {
		return args[0], nil
	}

	var elems []value
	for _, arg := range args[:len(args)-1] {
		listElems, err := properListElements(arg)
		if err != nil {
			return nil, err
		}
		elems = append(elems, listElems...)
	}

	result := args[len(args)-1]
	for i := len(elems) - 1; i >= 0; i-- {
		result = pairValue{
			car: elems[i],
			cdr: result,
		}
	}

	return result, nil
}

func evalListBuiltin(args []value) (value, error) {
	result := value(emptyList)
	for i := len(args) - 1; i >= 0; i-- {
		result = pairValue{
			car: args[i],
			cdr: result,
		}
	}
	return result, nil
}

func evalLength(args []value) (value, error) {
	if len(args) != 1 {
		return nil, newCurrentEvalError("'length' expects exactly 1 argument")
	}

	length, err := properListLength(args[0])
	if err != nil {
		return nil, err
	}

	return newExactInteger(length), nil
}

func evalNullPred(args []value) (value, error) {
	return evalTypePredicate(args, "null?", func(v value) bool {
		_, ok := v.(emptyListValue)
		return ok
	})
}

func evalNumberPred(args []value) (value, error) {
	return evalTypePredicate(args, "number?", func(v value) bool {
		_, ok := v.(numberValue)
		return ok
	})
}

func evalBooleanPred(args []value) (value, error) {
	return evalTypePredicate(args, "boolean?", func(v value) bool {
		_, ok := v.(boolValue)
		return ok
	})
}

func evalStringPred(args []value) (value, error) {
	return evalTypePredicate(args, "string?", func(v value) bool {
		_, ok := v.(*stringValue)
		return ok
	})
}

func evalPairPred(args []value) (value, error) {
	return evalTypePredicate(args, "pair?", func(v value) bool {
		_, ok := v.(pairValue)
		return ok
	})
}

func evalSymbolPred(args []value) (value, error) {
	return evalTypePredicate(args, "symbol?", func(v value) bool {
		_, ok := v.(symbolValue)
		return ok
	})
}

func evalCharPred(args []value) (value, error) {
	return evalTypePredicate(args, "char?", func(v value) bool {
		_, ok := v.(charValue)
		return ok
	})
}

func evalProcedurePred(args []value) (value, error) {
	return evalTypePredicate(args, "procedure?", func(v value) bool {
		_, ok := v.(procedure)
		return ok
	})
}

func evalDisplay(args []value) (value, error) {
	if len(args) != 1 {
		return nil, newCurrentEvalError("'display' expects exactly 1 argument")
	}
	appendOutput(formatValue(args[0], outputModeDisplay))
	return voidValue{}, nil
}

func evalWrite(args []value) (value, error) {
	if len(args) != 1 {
		return nil, newCurrentEvalError("'write' expects exactly 1 argument")
	}
	appendOutput(formatValue(args[0], outputModeWrite))
	return voidValue{}, nil
}

func evalNewline(args []value) (value, error) {
	if len(args) != 0 {
		return nil, newCurrentEvalError("'newline' expects exactly 0 arguments")
	}
	appendOutput("\n")
	return voidValue{}, nil
}

func evalApply(args []value) (value, error) {
	if len(args) < 2 {
		return nil, newCurrentEvalError("'apply' expects at least 2 arguments")
	}

	proc, ok := args[0].(procedure)
	if !ok {
		return nil, newCurrentEvalError("'apply' expects a procedure, got %s", args[0].schemeString())
	}

	tailArgs, err := properListElements(args[len(args)-1])
	if err != nil {
		return nil, err
	}

	flatArgs := make([]value, 0, len(args)-2+len(tailArgs))
	flatArgs = append(flatArgs, args[1:len(args)-1]...)
	flatArgs = append(flatArgs, tailArgs...)

	return proc.call(flatArgs)
}

func evalStringAppend(args []value) (value, error) {
	var builder strings.Builder
	for _, arg := range args {
		s, err := expectString(arg)
		if err != nil {
			return nil, err
		}
		builder.WriteString(s)
	}
	return newStringValue(builder.String()), nil
}

func evalStringLength(args []value) (value, error) {
	if len(args) != 1 {
		return nil, newCurrentEvalError("'string-length' expects exactly 1 argument")
	}

	s, err := expectString(args[0])
	if err != nil {
		return nil, err
	}

	return newExactInteger(len([]rune(s))), nil
}

func evalSubstring(args []value) (value, error) {
	if len(args) != 3 {
		return nil, newCurrentEvalError("'substring' expects exactly 3 arguments")
	}

	s, err := expectString(args[0])
	if err != nil {
		return nil, err
	}

	start, err := expectInteger(args[1])
	if err != nil {
		return nil, err
	}
	end, err := expectInteger(args[2])
	if err != nil {
		return nil, err
	}

	runes := []rune(s)
	if start < 0 || end < 0 || start > end || end > len(runes) {
		return nil, newCurrentEvalError("'substring' indices out of range")
	}

	return newStringValue(string(runes[start:end])), nil
}

func evalStringToNumber(args []value) (value, error) {
	if len(args) != 1 {
		return nil, newCurrentEvalError("'string->number' expects exactly 1 argument")
	}

	s, err := expectString(args[0])
	if err != nil {
		return nil, err
	}

	n, ok, convErr := parseNumberLiteral(s)
	if convErr != nil || !ok {
		return boolValue(false), nil
	}

	return n, nil
}

func evalNumberToString(args []value) (value, error) {
	if len(args) != 1 {
		return nil, newCurrentEvalError("'number->string' expects exactly 1 argument")
	}

	n, err := expectNumber(args[0])
	if err != nil {
		return nil, err
	}

	return newStringValue(n.schemeString()), nil
}

func evalSymbolToString(args []value) (value, error) {
	if len(args) != 1 {
		return nil, newCurrentEvalError("'symbol->string' expects exactly 1 argument")
	}

	symbol, err := expectSymbol(args[0])
	if err != nil {
		return nil, err
	}

	return newStringValue(symbol), nil
}

func evalStringToSymbol(args []value) (value, error) {
	if len(args) != 1 {
		return nil, newCurrentEvalError("'string->symbol' expects exactly 1 argument")
	}

	s, err := expectString(args[0])
	if err != nil {
		return nil, err
	}

	return symbolValue(s), nil
}

func evalStringRef(args []value) (value, error) {
	if len(args) != 2 {
		return nil, newCurrentEvalError("'string-ref' expects exactly 2 arguments")
	}

	s, err := expectString(args[0])
	if err != nil {
		return nil, err
	}

	index, err := expectInteger(args[1])
	if err != nil {
		return nil, err
	}

	runes := []rune(s)
	if index < 0 || index >= len(runes) {
		return nil, newCurrentEvalError("'string-ref' index out of range")
	}

	return charValue(runes[index]), nil
}

func evalStringCopy(args []value) (value, error) {
	if len(args) != 1 {
		return nil, newCurrentEvalError("'string-copy' expects exactly 1 argument")
	}

	s, err := expectStringValue(args[0])
	if err != nil {
		return nil, err
	}

	return copyStringValue(s, true), nil
}

func evalStringSet(args []value) (value, error) {
	if len(args) != 3 {
		return nil, newCurrentEvalError("'string-set!' expects exactly 3 arguments")
	}

	s, err := expectStringValue(args[0])
	if err != nil {
		return nil, err
	}

	index, err := expectInteger(args[1])
	if err != nil {
		return nil, err
	}

	ch, err := expectChar(args[2])
	if err != nil {
		return nil, err
	}

	if index < 0 || index >= len(s.chars) {
		return nil, newCurrentEvalError("'string-set!' index out of range")
	}
	if !s.mutable {
		return nil, newCurrentEvalError("'string-set!' cannot mutate immutable string")
	}

	s.chars[index] = ch
	return voidValue{}, nil
}

func evalTypePredicate(args []value, name string, pred func(value) bool) (value, error) {
	if len(args) != 1 {
		return nil, newCurrentEvalError("'%s' expects exactly 1 argument", name)
	}
	return boolValue(pred(args[0])), nil
}

func pushOutputBuffer(builder *strings.Builder) func() {
	prev := currentOutput
	currentOutput = builder
	return func() {
		currentOutput = prev
	}
}

func appendOutput(text string) {
	if currentOutput != nil {
		currentOutput.WriteString(text)
	}
}

func listFromValues(items []value) value {
	result := value(emptyList)
	for i := len(items) - 1; i >= 0; i-- {
		result = pairValue{
			car: items[i],
			cdr: result,
		}
	}
	return result
}

func properListElements(v value) ([]value, error) {
	var elems []value
	current := v

	for {
		switch list := current.(type) {
		case emptyListValue:
			return elems, nil
		case pairValue:
			elems = append(elems, list.car)
			current = list.cdr
		default:
			return nil, newCurrentEvalError("expected list, got %s", v.schemeString())
		}
	}
}

func properListLength(v value) (int, error) {
	elems, err := properListElements(v)
	if err != nil {
		return 0, err
	}
	return len(elems), nil
}

func expectNumber(v value) (numberValue, error) {
	n, ok := v.(numberValue)
	if !ok {
		return numberValue{}, newCurrentEvalError("expected number, got %s", v.schemeString())
	}
	return n, nil
}

func expectInteger(v value) (int, error) {
	n, err := expectNumber(v)
	if err != nil {
		return 0, err
	}
	if !n.isInteger() {
		return 0, newCurrentEvalError("expected integer, got %s", v.schemeString())
	}
	return n.numer, nil
}

func expectStringValue(v value) (*stringValue, error) {
	s, ok := v.(*stringValue)
	if !ok {
		return nil, newCurrentEvalError("expected string, got %s", v.schemeString())
	}
	return s, nil
}

func expectString(v value) (string, error) {
	s, err := expectStringValue(v)
	if err != nil {
		return "", err
	}
	return s.text(), nil
}

func expectChar(v value) (rune, error) {
	ch, ok := v.(charValue)
	if !ok {
		return 0, newCurrentEvalError("expected character, got %s", v.schemeString())
	}
	return rune(ch), nil
}

func expectSymbol(v value) (string, error) {
	s, ok := v.(symbolValue)
	if !ok {
		return "", newCurrentEvalError("expected symbol, got %s", v.schemeString())
	}
	return string(s), nil
}
