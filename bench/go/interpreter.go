package ming

import (
	"fmt"
	"math"
	"strconv"
	"strings"
	"unicode"
	"unicode/utf8"
)

type node interface{}

type listNode struct {
	elements []node
	pos      sourcePos
}

type dottedListNode struct {
	elements []node
	tail     node
	pos      sourcePos
}

type symbolNode struct {
	name     string
	pos      sourcePos
	captured *binding
}

type value interface{}

type booleanValue bool
type integerValue int
type stringValue string
type symbolValue string
type charValue rune
type mutableStringValue struct {
	runes []rune
}
type pairValue struct {
	car value
	cdr value
}

type listValue struct {
	elements []value
}

type voidValue struct{}

type evalContext struct {
	output strings.Builder
}

type evalStep struct {
	expr node
	env  *environment
}

type builtinProc func(args []value) (value, error)

type closureValue struct {
	params    []string
	restParam string
	hasRest   bool
	body      []node
	env       *environment
}

type caseClosureValue struct {
	clauses []*closureValue
}

type binding struct {
	value value
}

type macroState struct {
	hasMacros bool
}

type environment struct {
	parent       *environment
	values       map[string]*binding
	smallNames   [4]string
	smallValues  [4]*binding
	smallCount   int
	macros       map[string]macroTransformer
	macroState   *macroState
	fastEval     bool
	syntaxDefEnv *environment
}

func evalString(input string) (string, error) {
	result, _, err := evalStringWithOutput(input)
	return result, err
}

func evalStringWithOutput(input string) (string, string, error) {
	nodes, err := parseProgram(input)
	if err != nil {
		return "", "", err
	}
	if len(nodes) == 0 {
		return "", "", errorAt(startPos(), "empty input")
	}

	ctx := &evalContext{}
	env := baseEnv(ctx)
	env.fastEval = canUseFastEval(nodes)

	var last value
	if env.fastEval {
		last, err = evalSequenceFast(nodes, env)
	} else {
		last, err = runEvalSequence(nodes, env)
	}
	if err != nil {
		return "", "", err
	}
	result, err := formatValue(last)
	if err != nil {
		return "", "", err
	}
	return result, ctx.output.String(), nil
}

func baseEnv(ctx *evalContext) *environment {
	env := newEnvironment(nil)
	env.define("+", builtinNumericFold("+"))
	env.define("-", builtinSub())
	env.define("*", builtinNumericFold("*"))
	env.define("/", builtinDiv())
	env.define("<", builtinCompare("<"))
	env.define(">", builtinCompare(">"))
	env.define("=", builtinCompare("="))
	env.define("<=", builtinCompare("<="))
	env.define(">=", builtinCompare(">="))
	env.define("eq?", builtinEq())
	env.define("eqv?", builtinEqv())
	env.define("equal?", builtinEqual())
	env.define("not", builtinNot())
	env.define("abs", builtinAbs())
	env.define("gcd", builtinGCD())
	env.define("lcm", builtinLCM())
	env.define("truncate", builtinTruncate())
	env.define("round", builtinRound())
	env.define("modulo", builtinModulo())
	env.define("remainder", builtinRemainder())
	env.define("quotient", builtinQuotient())
	env.define("min", builtinMinMax("min"))
	env.define("max", builtinMinMax("max"))
	env.define("expt", builtinExpt())
	env.define("zero?", builtinIntegerPredicate(func(n int) bool { return n == 0 }))
	env.define("positive?", builtinIntegerPredicate(func(n int) bool { return n > 0 }))
	env.define("negative?", builtinIntegerPredicate(func(n int) bool { return n < 0 }))
	env.define("odd?", builtinIntegerPredicate(func(n int) bool { return n%2 != 0 }))
	env.define("even?", builtinIntegerPredicate(func(n int) bool { return n%2 == 0 }))
	env.define("cons", builtinCons())
	env.define("car", builtinCar())
	env.define("cdr", builtinCdr())
	env.define("cddr", builtinCxr("cddr"))
	env.define("set-car!", builtinSetCar())
	env.define("set-cdr!", builtinSetCdr())
	env.define("null?", builtinNull())
	env.define("list", builtinList())
	env.define("vector", builtinVector())
	env.define("make-vector", builtinMakeVector())
	env.define("vector-ref", builtinVectorRef())
	env.define("vector-set!", builtinVectorSet())
	env.define("vector-length", builtinVectorLength())
	env.define("vector->list", builtinVectorToList())
	env.define("list->vector", builtinListToVector())
	env.define("list?", builtinPredicate(isListValue))
	env.define("vector?", builtinPredicate(isVectorValue))
	env.define("length", builtinLength())
	env.define("reverse", builtinReverse())
	env.define("list-ref", builtinListRef())
	env.define("list-tail", builtinListTail())
	env.define("append", builtinAppend())
	env.define("apply", builtinApply())
	env.define("values", builtinValues())
	env.define("call-with-values", builtinCallWithValues())
	env.define("map", builtinMap())
	env.define("for-each", builtinForEach())
	env.define("memq", builtinMemq())
	env.define("memv", builtinMemv())
	env.define("member", builtinMember())
	env.define("assq", builtinAssq())
	env.define("assv", builtinAssv())
	env.define("assoc", builtinAssoc())
	env.define("procedure?", builtinProcedurePredicate())
	env.define("error", builtinError())
	env.define("string?", builtinPredicate(func(v value) bool {
		return isStringValue(v)
	}))
	env.define("number?", builtinPredicate(func(v value) bool {
		return isNumberValue(v)
	}))
	env.define("integer?", builtinPredicate(isIntegerNumber))
	env.define("rational?", builtinPredicate(isRationalNumber))
	env.define("exact?", builtinPredicate(isExactNumber))
	env.define("inexact?", builtinPredicate(isInexactNumber))
	env.define("boolean?", builtinPredicate(func(v value) bool {
		_, ok := v.(booleanValue)
		return ok
	}))
	env.define("pair?", builtinPredicate(func(v value) bool {
		return isPairValue(v)
	}))
	env.define("symbol?", builtinPredicate(func(v value) bool {
		_, ok := v.(symbolValue)
		return ok
	}))
	env.define("char?", builtinPredicate(func(v value) bool {
		_, ok := v.(charValue)
		return ok
	}))
	env.define("display", builtinDisplay(ctx))
	env.define("write", builtinWrite(ctx))
	env.define("newline", builtinNewline(ctx))
	env.define("raise", raiseBuiltin)
	env.define("with-exception-handler", withExceptionHandlerBuiltin)
	env.define("make-string", builtinMakeString())
	env.define("string", builtinString())
	env.define("string-append", builtinStringAppend())
	env.define("string-length", builtinStringLength())
	env.define("substring", builtinSubstring())
	env.define("string->number", builtinStringToNumber())
	env.define("number->string", builtinNumberToString())
	env.define("exact->inexact", builtinExactToInexact())
	env.define("inexact->exact", builtinInexactToExact())
	env.define("numerator", builtinNumerator())
	env.define("denominator", builtinDenominator())
	env.define("symbol->string", builtinSymbolToString())
	env.define("string->symbol", builtinStringToSymbol())
	env.define("string->list", builtinStringToList())
	env.define("list->string", builtinListToString())
	env.define("string-ref", builtinStringRef())
	env.define("string-copy", builtinStringCopy())
	env.define("string-set!", builtinStringSet())
	env.define("string=?", builtinStringCompare("string=?"))
	env.define("string<?", builtinStringCompare("string<?"))
	env.define("string>?", builtinStringCompare("string>?"))
	env.define("string<=?", builtinStringCompare("string<=?"))
	env.define("string>=?", builtinStringCompare("string>=?"))
	env.define("string-ci=?", builtinStringCIEqual())
	env.define("string-upcase", builtinStringCase("upcase"))
	env.define("string-downcase", builtinStringCase("downcase"))
	env.define("char->integer", builtinCharToInteger())
	env.define("integer->char", builtinIntegerToChar())
	env.define("char-alphabetic?", builtinCharPredicate(unicode.IsLetter))
	env.define("char-numeric?", builtinCharPredicate(unicode.IsDigit))
	env.define("char-upcase", builtinCharCase("upcase"))
	env.define("char-downcase", builtinCharCase("downcase"))
	env.define("char=?", builtinCharCompare("char=?"))
	env.define("char<?", builtinCharCompare("char<?"))
	env.define("call/cc", callCCBuiltin)
	env.define("call-with-current-continuation", callCCBuiltin)
	env.define("dynamic-wind", dynamicWindBuiltin)
	env.define("syntax->datum", builtinSyntaxToDatum())
	env.define("datum->syntax", builtinDatumToSyntax())
	env.define("identifier?", builtinIdentifierPredicate())
	env.define("free-identifier=?", builtinFreeIdentifierEqual())
	return env
}

func newEnvironment(parent *environment) *environment {
	env := &environment{
		parent: parent,
	}
	if parent != nil {
		env.syntaxDefEnv = parent.syntaxDefEnv
		env.macroState = parent.macroState
		env.fastEval = parent.fastEval
	} else {
		env.macroState = &macroState{}
	}
	return env
}

func (e *environment) define(name string, val value) {
	e.defineBinding(name, &binding{value: val})
}

func (e *environment) defineBinding(name string, cell *binding) {
	for i := 0; i < e.smallCount; i++ {
		if e.smallNames[i] == name {
			e.smallValues[i] = cell
			return
		}
	}

	if e.values != nil {
		e.values[name] = cell
		return
	}

	if e.smallCount < len(e.smallNames) {
		e.smallNames[e.smallCount] = name
		e.smallValues[e.smallCount] = cell
		e.smallCount++
		return
	}

	e.values = make(map[string]*binding, e.smallCount+1)
	for i := 0; i < e.smallCount; i++ {
		e.values[e.smallNames[i]] = e.smallValues[i]
		e.smallNames[i] = ""
		e.smallValues[i] = nil
	}
	e.smallCount = 0
	e.values[name] = cell
}

func (e *environment) lookupBinding(name string) (*binding, bool) {
	for current := e; current != nil; current = current.parent {
		for i := 0; i < current.smallCount; i++ {
			if current.smallNames[i] == name {
				return current.smallValues[i], true
			}
		}

		if current.values != nil {
			cell, ok := current.values[name]
			if ok {
				return cell, true
			}
		}
	}
	return nil, false
}

func (e *environment) lookup(name string) (value, bool) {
	cell, ok := e.lookupBinding(name)
	if !ok {
		return nil, false
	}
	return cell.value, true
}

func (e *environment) set(name string, val value) bool {
	cell, ok := e.lookupBinding(name)
	if !ok {
		return false
	}
	cell.value = val
	return true
}

func eval(expr node, env *environment) (value, error) {
	if env != nil && env.fastEval {
		return evalFast(expr, env)
	}
	return runEval(expr, env)
}

func evalFast(expr node, env *environment) (value, error) {
	for {
		switch current := expr.(type) {
		case integerValue, rationalValue, inexactValue, booleanValue, stringValue, charValue:
			return current, nil
		case vectorNode:
			return datumFromNode(current)
		case symbolNode:
			if current.captured != nil {
				return current.captured.value, nil
			}

			val, ok := env.lookup(current.name)
			if !ok {
				return nil, errorAt(current.pos, "unbound variable: %s", current.name)
			}
			return val, nil
		case listNode:
			result, step, err := evalList(current, env)
			if err != nil {
				return nil, err
			}
			if step == nil {
				return result, nil
			}
			expr = step.expr
			env = step.env
		default:
			return nil, errorAt(nodePos(expr), "unknown expression")
		}
	}
}

var slowEvalSymbols = map[string]struct{}{
	"call/cc":                        {},
	"call-with-current-continuation": {},
	"dynamic-wind":                   {},
	"guard":                          {},
	"raise":                          {},
	"with-exception-handler":         {},
	"define-syntax":                  {},
	"syntax":                         {},
	"syntax-case":                    {},
	"with-syntax":                    {},
}

func canUseFastEval(nodes []node) bool {
	for _, expr := range nodes {
		if containsSlowEvalFeature(expr) {
			return false
		}
	}
	return true
}

func containsSlowEvalFeature(expr node) bool {
	switch expr := expr.(type) {
	case symbolNode:
		_, blocked := slowEvalSymbols[expr.name]
		return blocked
	case listNode:
		for _, element := range expr.elements {
			if containsSlowEvalFeature(element) {
				return true
			}
		}
		return false
	case dottedListNode:
		for _, element := range expr.elements {
			if containsSlowEvalFeature(element) {
				return true
			}
		}
		return containsSlowEvalFeature(expr.tail)
	case vectorNode:
		for _, element := range expr.elements {
			if containsSlowEvalFeature(element) {
				return true
			}
		}
		return false
	default:
		return false
	}
}

func evalList(list listNode, env *environment) (value, *evalStep, error) {
	if len(list.elements) == 0 {
		return nil, nil, errorAt(list.pos, "cannot evaluate empty list")
	}

	if name, ok := symbolName(list.elements[0]); ok {
		switch name {
		case "and":
			result, step, err := evalAnd(list.elements[1:], env)
			return result, step, withErrorPos(err, list.pos)
		case "or":
			result, step, err := evalOr(list.elements[1:], env)
			return result, step, withErrorPos(err, list.pos)
		case "define":
			result, err := evalDefine(list.elements[1:], env)
			return result, nil, withErrorPos(err, list.pos)
		case "define-syntax":
			result, err := evalDefineSyntax(list.elements[1:], env)
			return result, nil, withErrorPos(err, list.pos)
		case "define-record-type":
			result, err := evalDefineRecordType(list.elements[1:], env)
			return result, nil, withErrorPos(err, list.pos)
		case "set!":
			result, err := evalSet(list.elements[1:], env)
			return result, nil, withErrorPos(err, list.pos)
		case "if":
			result, step, err := evalIf(list.elements[1:], env)
			return result, step, withErrorPos(err, list.pos)
		case "quote":
			result, err := evalQuote(list.elements[1:])
			return result, nil, withErrorPos(err, list.pos)
		case "quasiquote":
			result, err := evalQuasiquote(list.elements[1:], env)
			return result, nil, withErrorPos(err, list.pos)
		case "lambda":
			result, err := evalLambda(list.elements[1:], env)
			return result, nil, withErrorPos(err, list.pos)
		case "case-lambda":
			result, err := evalCaseLambda(list.elements[1:], env)
			return result, nil, withErrorPos(err, list.pos)
		case "begin":
			result, step, err := evalBegin(list.elements[1:], env)
			return result, step, withErrorPos(err, list.pos)
		case "cond":
			result, step, err := evalCond(list.elements[1:], env)
			return result, step, withErrorPos(err, list.pos)
		case "case":
			result, step, err := evalCase(list.elements[1:], env)
			return result, step, withErrorPos(err, list.pos)
		case "let":
			result, step, err := evalLet(list.elements[1:], env)
			return result, step, withErrorPos(err, list.pos)
		case "let*":
			result, step, err := evalLetStar(list.elements[1:], env)
			return result, step, withErrorPos(err, list.pos)
		case "letrec":
			result, step, err := evalLetrec(list.elements[1:], env, false)
			return result, step, withErrorPos(err, list.pos)
		case "letrec*":
			result, step, err := evalLetrec(list.elements[1:], env, true)
			return result, step, withErrorPos(err, list.pos)
		case "do":
			result, err := evalDo(list.elements[1:], env)
			return result, nil, withErrorPos(err, list.pos)
		}
	}

	operator, err := eval(list.elements[0], env)
	if err != nil {
		return nil, nil, err
	}

	args, err := evalArgs(list.elements[1:], env)
	if err != nil {
		return nil, nil, err
	}
	result, step, err := startProcedureCall(operator, args, list.pos)
	return result, step, withErrorPos(err, list.pos)
}

func evalArgs(args []node, env *environment) ([]value, error) {
	values := make([]value, len(args))
	for i, arg := range args {
		evaluated, err := eval(arg, env)
		if err != nil {
			return nil, err
		}
		values[i] = evaluated
	}
	return values, nil
}

func evalAnd(args []node, env *environment) (value, *evalStep, error) {
	if len(args) == 0 {
		return booleanValue(true), nil, nil
	}

	for _, arg := range args[:len(args)-1] {
		evaluated, err := eval(arg, env)
		if err != nil {
			return nil, nil, err
		}
		if !isTruthy(evaluated) {
			return evaluated, nil, nil
		}
	}

	return nil, &evalStep{expr: args[len(args)-1], env: env}, nil
}

func evalOr(args []node, env *environment) (value, *evalStep, error) {
	if len(args) == 0 {
		return booleanValue(false), nil, nil
	}

	for _, arg := range args[:len(args)-1] {
		evaluated, err := eval(arg, env)
		if err != nil {
			return nil, nil, err
		}
		if isTruthy(evaluated) {
			return evaluated, nil, nil
		}
	}

	return nil, &evalStep{expr: args[len(args)-1], env: env}, nil
}

func evalDefine(args []node, env *environment) (value, error) {
	if len(args) < 2 {
		return nil, &EvalError{Message: "define expects a name and value"}
	}

	switch target := args[0].(type) {
	case symbolNode:
		if len(args) != 2 {
			return nil, &EvalError{Message: "define expects exactly 2 arguments"}
		}
		val, err := eval(args[1], env)
		if err != nil {
			return nil, err
		}
		env.define(target.name, val)
		return voidValue{}, nil
	case listNode:
		if len(target.elements) == 0 {
			return nil, &EvalError{Message: "define requires a function name"}
		}

		name, ok := symbolName(target.elements[0])
		if !ok {
			return nil, &EvalError{Message: "define requires a function name"}
		}

		proc, err := makeClosure(target.elements[1:], args[1:], env)
		if err != nil {
			return nil, err
		}
		env.define(name, proc)
		return voidValue{}, nil
	default:
		return nil, &EvalError{Message: "define requires a symbol"}
	}
}

func evalSet(args []node, env *environment) (value, error) {
	if len(args) != 2 {
		return nil, &EvalError{Message: "set! expects exactly 2 arguments"}
	}

	target, ok := args[0].(symbolNode)
	if !ok {
		return nil, &EvalError{Message: "set! requires a symbol"}
	}

	val, err := eval(args[1], env)
	if err != nil {
		return nil, err
	}

	if target.captured != nil {
		target.captured.value = val
		return voidValue{}, nil
	}

	if !env.set(target.name, val) {
		return nil, errorAt(target.pos, "unbound variable: %s", target.name)
	}
	return voidValue{}, nil
}

func evalIf(args []node, env *environment) (value, *evalStep, error) {
	if len(args) != 2 && len(args) != 3 {
		return nil, nil, &EvalError{Message: "if expects 2 or 3 arguments"}
	}

	condition, err := eval(args[0], env)
	if err != nil {
		return nil, nil, err
	}
	if isTruthy(condition) {
		return nil, &evalStep{expr: args[1], env: env}, nil
	}
	if len(args) == 2 {
		return voidValue{}, nil, nil
	}
	return nil, &evalStep{expr: args[2], env: env}, nil
}

func evalQuote(args []node) (value, error) {
	if len(args) != 1 {
		return nil, &EvalError{Message: "quote expects exactly 1 argument"}
	}
	return datumFromNode(args[0])
}

func evalQuasiquote(args []node, env *environment) (value, error) {
	if len(args) != 1 {
		return nil, &EvalError{Message: "quasiquote expects exactly 1 argument"}
	}
	return evalQuasiquoteNode(args[0], env, 1)
}

func evalLambda(args []node, env *environment) (value, error) {
	if len(args) < 2 {
		return nil, &EvalError{Message: "lambda expects parameters and a body"}
	}
	return makeClosureFromFormals(args[0], args[1:], env)
}

func evalCaseLambda(args []node, env *environment) (value, error) {
	if len(args) == 0 {
		return nil, &EvalError{Message: "case-lambda expects at least 1 clause"}
	}

	clauses := make([]*closureValue, 0, len(args))
	for _, clauseExpr := range args {
		clause, ok := clauseExpr.(listNode)
		if !ok || len(clause.elements) < 2 {
			return nil, &EvalError{Message: "case-lambda clauses must contain formals and a body"}
		}

		proc, err := makeClosureFromFormals(clause.elements[0], clause.elements[1:], env)
		if err != nil {
			return nil, err
		}
		clauses = append(clauses, proc)
	}

	return &caseClosureValue{clauses: clauses}, nil
}

func evalBegin(args []node, env *environment) (value, *evalStep, error) {
	return prepareSequence(args, env)
}

func evalCond(args []node, env *environment) (value, *evalStep, error) {
	for i, clauseExpr := range args {
		clause, ok := clauseExpr.(listNode)
		if !ok || len(clause.elements) == 0 {
			return nil, nil, &EvalError{Message: "cond clauses must be non-empty lists"}
		}

		if name, ok := symbolName(clause.elements[0]); ok && name == "else" {
			if i != len(args)-1 {
				return nil, nil, &EvalError{Message: "else clause must be last"}
			}
			if len(clause.elements) == 1 {
				return voidValue{}, nil, nil
			}
			return prepareSequence(clause.elements[1:], env)
		}

		testValue, err := eval(clause.elements[0], env)
		if err != nil {
			return nil, nil, err
		}
		if !isTruthy(testValue) {
			continue
		}
		if len(clause.elements) == 1 {
			return testValue, nil, nil
		}
		if isCondArrowClause(clause) {
			receiver, err := eval(clause.elements[2], env)
			if err != nil {
				return nil, nil, err
			}
			return startProcedureCall(receiver, []value{testValue}, clause.pos)
		}
		return prepareSequence(clause.elements[1:], env)
	}
	return voidValue{}, nil, nil
}

func evalLet(args []node, env *environment) (value, *evalStep, error) {
	if len(args) < 2 {
		return nil, nil, &EvalError{Message: "let expects bindings and a body"}
	}

	if name, ok := symbolName(args[0]); ok {
		return evalNamedLet(name, args[1:], env)
	}

	bindingList, ok := args[0].(listNode)
	if !ok {
		return nil, nil, &EvalError{Message: "let bindings must be a list"}
	}

	names, values, err := evalBindings(bindingList.elements, env)
	if err != nil {
		return nil, nil, err
	}

	letEnv := newEnvironment(env)
	for i, name := range names {
		letEnv.define(name, values[i])
	}
	return prepareSequence(args[1:], letEnv)
}

func evalLetStar(args []node, env *environment) (value, *evalStep, error) {
	if len(args) < 2 {
		return nil, nil, &EvalError{Message: "let* expects bindings and a body"}
	}

	bindingList, ok := args[0].(listNode)
	if !ok {
		return nil, nil, &EvalError{Message: "let* bindings must be a list"}
	}

	letEnv := newEnvironment(env)
	for _, bindingExpr := range bindingList.elements {
		binding, ok := bindingExpr.(listNode)
		if !ok || len(binding.elements) != 2 {
			return nil, nil, &EvalError{Message: "let* bindings must contain name/value pairs"}
		}

		name, ok := symbolName(binding.elements[0])
		if !ok {
			return nil, nil, &EvalError{Message: "let* binding names must be symbols"}
		}

		boundValue, err := eval(binding.elements[1], letEnv)
		if err != nil {
			return nil, nil, err
		}
		letEnv.define(name, boundValue)
	}

	return prepareSequence(args[1:], letEnv)
}

func evalNamedLet(name string, args []node, env *environment) (value, *evalStep, error) {
	if len(args) < 2 {
		return nil, nil, &EvalError{Message: "named let expects bindings and a body"}
	}

	bindingList, ok := args[0].(listNode)
	if !ok {
		return nil, nil, &EvalError{Message: "named let bindings must be a list"}
	}

	names, values, err := evalBindings(bindingList.elements, env)
	if err != nil {
		return nil, nil, err
	}

	letEnv := newEnvironment(env)
	proc := &closureValue{
		params: names,
		body:   args[1:],
		env:    letEnv,
	}
	letEnv.define(name, proc)
	return startClosureCall(proc, values, sourcePos{})
}

func evalBindings(bindingExprs []node, env *environment) ([]string, []value, error) {
	names := make([]string, len(bindingExprs))
	values := make([]value, len(bindingExprs))
	for i, bindingExpr := range bindingExprs {
		binding, ok := bindingExpr.(listNode)
		if !ok || len(binding.elements) != 2 {
			return nil, nil, &EvalError{Message: "let bindings must contain name/value pairs"}
		}

		name, ok := symbolName(binding.elements[0])
		if !ok {
			return nil, nil, &EvalError{Message: "let binding names must be symbols"}
		}

		boundValue, err := eval(binding.elements[1], env)
		if err != nil {
			return nil, nil, err
		}

		names[i] = name
		values[i] = boundValue
	}
	return names, values, nil
}

func makeClosure(paramExprs []node, body []node, env *environment) (*closureValue, error) {
	return makeClosureWithParams(paramExprs, body, env)
}

func makeClosureFromFormals(formals node, body []node, env *environment) (*closureValue, error) {
	switch formals := formals.(type) {
	case listNode:
		return makeClosureWithParams(formals.elements, body, env)
	case dottedListNode:
		paramExprs := make([]node, 0, len(formals.elements)+2)
		paramExprs = append(paramExprs, formals.elements...)
		paramExprs = append(paramExprs, symbolNode{name: ".", pos: formals.pos})
		paramExprs = append(paramExprs, formals.tail)
		return makeClosureWithParams(paramExprs, body, env)
	case symbolNode:
		if formals.name == "." {
			return nil, &EvalError{Message: "parameter list must contain only symbols"}
		}
		if len(body) == 0 {
			return nil, &EvalError{Message: "lambda requires a body"}
		}
		return &closureValue{
			restParam: formals.name,
			hasRest:   true,
			body:      body,
			env:       env,
		}, nil
	default:
		return nil, &EvalError{Message: "lambda parameters must be a list or symbol"}
	}
}

func makeClosureWithParams(paramExprs []node, body []node, env *environment) (*closureValue, error) {
	if len(body) == 0 {
		return nil, &EvalError{Message: "lambda requires a body"}
	}

	params, restParam, hasRest, err := parseParamNames(paramExprs)
	if err != nil {
		return nil, err
	}

	return &closureValue{
		params:    params,
		restParam: restParam,
		hasRest:   hasRest,
		body:      body,
		env:       env,
	}, nil
}

func parseParamNames(paramExprs []node) ([]string, string, bool, error) {
	params := make([]string, 0, len(paramExprs))
	for i, expr := range paramExprs {
		name, ok := symbolName(expr)
		if !ok {
			return nil, "", false, &EvalError{Message: "parameter list must contain only symbols"}
		}

		if name == "." {
			if i != len(paramExprs)-2 {
				return nil, "", false, &EvalError{Message: "invalid dotted parameter list"}
			}

			restName, ok := symbolName(paramExprs[i+1])
			if !ok || restName == "." {
				return nil, "", false, &EvalError{Message: "parameter list must contain only symbols"}
			}
			return params, restName, true, nil
		}

		params = append(params, name)
	}
	return params, "", false, nil
}

func applyProcedure(proc value, args []value, pos sourcePos) (value, error) {
	if procedureUsesFastEval(proc) {
		result, step, err := startProcedureCall(proc, args, pos)
		if err != nil {
			return nil, err
		}
		if step == nil {
			return result, nil
		}
		return eval(step.expr, step.env)
	}
	return runProcedureCall(proc, args, pos)
}

func procedureUsesFastEval(proc value) bool {
	switch proc := proc.(type) {
	case builtinProc:
		return true
	case *closureValue:
		return proc.env != nil && proc.env.fastEval
	case *caseClosureValue:
		if len(proc.clauses) == 0 {
			return false
		}
		for _, clause := range proc.clauses {
			if clause.env == nil || !clause.env.fastEval {
				return false
			}
		}
		return true
	default:
		return false
	}
}

func startProcedureCall(proc value, args []value, pos sourcePos) (value, *evalStep, error) {
	switch proc := proc.(type) {
	case builtinProc:
		result, err := proc(args)
		return result, nil, withErrorPos(err, pos)
	case *closureValue:
		return startClosureCall(proc, args, pos)
	case *caseClosureValue:
		for _, clause := range proc.clauses {
			if closureAcceptsArgCount(clause, len(args)) {
				return startClosureCall(clause, args, pos)
			}
		}
		return nil, nil, errorAt(pos, "no matching case-lambda clause for %d arguments", len(args))
	default:
		if isProcedureValue(proc) {
			result, err := runProcedureCall(proc, args, pos)
			return result, nil, err
		}
		return nil, nil, errorAt(pos, "not a procedure")
	}
}

func closureAcceptsArgCount(proc *closureValue, argCount int) bool {
	if proc.hasRest {
		return argCount >= len(proc.params)
	}
	return argCount == len(proc.params)
}

func applyClosure(proc *closureValue, args []value, pos sourcePos) (value, error) {
	result, step, err := startClosureCall(proc, args, pos)
	if err != nil {
		return nil, err
	}
	if step == nil {
		return result, nil
	}
	return eval(step.expr, step.env)
}

func startClosureCall(proc *closureValue, args []value, pos sourcePos) (value, *evalStep, error) {
	if !proc.hasRest && len(args) != len(proc.params) {
		return nil, nil, errorAt(pos, "expected %d arguments, got %d", len(proc.params), len(args))
	}
	if proc.hasRest && len(args) < len(proc.params) {
		return nil, nil, errorAt(pos, "expected at least %d arguments, got %d", len(proc.params), len(args))
	}

	callEnv := newEnvironment(proc.env)
	for i, name := range proc.params {
		callEnv.define(name, args[i])
	}
	if proc.hasRest {
		callEnv.define(proc.restParam, makeList(copyValues(args[len(proc.params):])))
	}
	return prepareSequence(proc.body, callEnv)
}

func evalSequence(exprs []node, env *environment) (value, error) {
	if env != nil && env.fastEval {
		return evalSequenceFast(exprs, env)
	}
	return runEvalSequence(exprs, env)
}

func evalSequenceFast(exprs []node, env *environment) (value, error) {
	result, step, err := prepareSequence(exprs, env)
	if err != nil {
		return nil, err
	}
	if step == nil {
		return result, nil
	}
	return eval(step.expr, step.env)
}

func prepareSequence(exprs []node, env *environment) (value, *evalStep, error) {
	if len(exprs) == 0 {
		return voidValue{}, nil, nil
	}

	for _, expr := range exprs[:len(exprs)-1] {
		if _, err := eval(expr, env); err != nil {
			return nil, nil, err
		}
	}

	return nil, &evalStep{expr: exprs[len(exprs)-1], env: env}, nil
}

func isCondArrowClause(clause listNode) bool {
	if len(clause.elements) != 3 {
		return false
	}

	name, ok := symbolName(clause.elements[1])
	return ok && name == "=>"
}

func builtinNumericFold(name string) builtinProc {
	return func(args []value) (value, error) {
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

		total := exactNumericValue(0, 1)
		if name == "*" {
			total = exactNumericValue(1, 1)
		}

		for _, arg := range args {
			current, err := expectNumberValue(arg)
			if err != nil {
				return nil, err
			}
			if name == "+" {
				total = addNumeric(total, current)
			} else {
				total = mulNumeric(total, current)
			}
		}
		return total.toValue(), nil
	}
}

func builtinSub() builtinProc {
	return func(args []value) (value, error) {
		if len(args) == 0 {
			return nil, &EvalError{Message: "- expects at least 1 argument"}
		}

		first, err := expectNumberValue(args[0])
		if err != nil {
			return nil, err
		}
		if len(args) == 1 {
			return subNumeric(exactNumericValue(0, 1), first).toValue(), nil
		}

		total := first
		for _, arg := range args[1:] {
			current, err := expectNumberValue(arg)
			if err != nil {
				return nil, err
			}
			total = subNumeric(total, current)
		}
		return total.toValue(), nil
	}
}

func builtinDiv() builtinProc {
	return func(args []value) (value, error) {
		if len(args) < 2 {
			return nil, &EvalError{Message: "/ expects at least 2 arguments"}
		}

		total, err := expectNumberValue(args[0])
		if err != nil {
			return nil, err
		}

		for _, arg := range args[1:] {
			current, err := expectNumberValue(arg)
			if err != nil {
				return nil, err
			}
			total, err = divNumeric(total, current)
			if err != nil {
				return nil, err
			}
		}
		return total.toValue(), nil
	}
}

func builtinCompare(name string) builtinProc {
	return func(args []value) (value, error) {
		if len(args) < 2 {
			return nil, &EvalError{Message: fmt.Sprintf("%s expects at least 2 arguments", name)}
		}

		prev, err := expectNumberValue(args[0])
		if err != nil {
			return nil, err
		}

		for _, arg := range args[1:] {
			current, err := expectNumberValue(arg)
			if err != nil {
				return nil, err
			}

			ok := compareNumeric(prev, current, name)
			if name != "<" && name != ">" && name != "=" && name != "<=" && name != ">=" {
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
	return func(args []value) (value, error) {
		if len(args) != 1 {
			return nil, &EvalError{Message: "not expects exactly 1 argument"}
		}
		return booleanValue(!isTruthy(args[0])), nil
	}
}

func builtinCons() builtinProc {
	return func(args []value) (value, error) {
		if len(args) != 2 {
			return nil, &EvalError{Message: "cons expects exactly 2 arguments"}
		}
		return &pairValue{car: args[0], cdr: args[1]}, nil
	}
}

func builtinCar() builtinProc {
	return func(args []value) (value, error) {
		if len(args) != 1 {
			return nil, &EvalError{Message: "car expects exactly 1 argument"}
		}

		switch v := args[0].(type) {
		case listValue:
			if len(v.elements) == 0 {
				return nil, &EvalError{Message: "car expects a non-empty list"}
			}
			return v.elements[0], nil
		case *pairValue:
			return v.car, nil
		default:
			return nil, &EvalError{Message: "car expects a pair"}
		}
	}
}

func builtinCdr() builtinProc {
	return func(args []value) (value, error) {
		if len(args) != 1 {
			return nil, &EvalError{Message: "cdr expects exactly 1 argument"}
		}

		switch v := args[0].(type) {
		case listValue:
			if len(v.elements) == 0 {
				return nil, &EvalError{Message: "cdr expects a non-empty list"}
			}
			return makeList(copyValues(v.elements[1:])), nil
		case *pairValue:
			return v.cdr, nil
		default:
			return nil, &EvalError{Message: "cdr expects a pair"}
		}
	}
}

func builtinCxr(name string) builtinProc {
	return func(args []value) (value, error) {
		if len(args) != 1 {
			return nil, &EvalError{Message: fmt.Sprintf("%s expects exactly 1 argument", name)}
		}

		current := args[0]
		for i := len(name) - 2; i >= 1; i-- {
			switch name[i] {
			case 'a':
				next, err := builtinCar()([]value{current})
				if err != nil {
					return nil, err
				}
				current = next
			case 'd':
				next, err := builtinCdr()([]value{current})
				if err != nil {
					return nil, err
				}
				current = next
			default:
				return nil, &EvalError{Message: fmt.Sprintf("unsupported accessor: %s", name)}
			}
		}

		return current, nil
	}
}

func builtinSetCar() builtinProc {
	return func(args []value) (value, error) {
		if len(args) != 2 {
			return nil, &EvalError{Message: "set-car! expects exactly 2 arguments"}
		}

		pair, ok := args[0].(*pairValue)
		if !ok {
			return nil, &EvalError{Message: "set-car! expects a pair"}
		}

		pair.car = args[1]
		return voidValue{}, nil
	}
}

func builtinSetCdr() builtinProc {
	return func(args []value) (value, error) {
		if len(args) != 2 {
			return nil, &EvalError{Message: "set-cdr! expects exactly 2 arguments"}
		}

		pair, ok := args[0].(*pairValue)
		if !ok {
			return nil, &EvalError{Message: "set-cdr! expects a pair"}
		}

		pair.cdr = args[1]
		return voidValue{}, nil
	}
}

func builtinNull() builtinProc {
	return func(args []value) (value, error) {
		if len(args) != 1 {
			return nil, &EvalError{Message: "null? expects exactly 1 argument"}
		}

		return booleanValue(isEmptyListValue(args[0])), nil
	}
}

func builtinList() builtinProc {
	return func(args []value) (value, error) {
		return makeList(copyValues(args)), nil
	}
}

func builtinLength() builtinProc {
	return func(args []value) (value, error) {
		if len(args) != 1 {
			return nil, &EvalError{Message: "length expects exactly 1 argument"}
		}

		cursor := newListCursor(args[0])
		count := 0
		for {
			_, ok, err := cursor.next()
			if err != nil {
				return nil, err
			}
			if !ok {
				return integerValue(count), nil
			}
			count++
		}
	}
}

func builtinReverse() builtinProc {
	return func(args []value) (value, error) {
		if len(args) != 1 {
			return nil, &EvalError{Message: "reverse expects exactly 1 argument"}
		}

		cursor := newListCursor(args[0])
		var reversed value = listValue{}
		for {
			element, ok, err := cursor.next()
			if err != nil {
				return nil, err
			}
			if !ok {
				return reversed, nil
			}
			reversed = &pairValue{car: element, cdr: reversed}
		}
	}
}

func builtinAppend() builtinProc {
	return func(args []value) (value, error) {
		if len(args) == 0 {
			return listValue{}, nil
		}
		if len(args) == 1 {
			return args[0], nil
		}

		var head *pairValue
		var tail *pairValue
		for _, arg := range args[:len(args)-1] {
			cursor := newListCursor(arg)
			for {
				element, ok, err := cursor.next()
				if err != nil {
					return nil, err
				}
				if !ok {
					break
				}

				cell := &pairValue{car: element, cdr: listValue{}}
				if head == nil {
					head = cell
					tail = cell
				} else {
					tail.cdr = cell
					tail = cell
				}
			}
		}

		last := args[len(args)-1]
		if head == nil {
			return last, nil
		}
		tail.cdr = last
		return head, nil
	}
}

func builtinApply() builtinProc {
	return func(args []value) (value, error) {
		if len(args) < 2 {
			return nil, &EvalError{Message: "apply expects at least 2 arguments"}
		}

		last, err := expectListValue(args[len(args)-1])
		if err != nil {
			return nil, err
		}

		combined := make([]value, 0, len(args)-2+len(last.elements))
		combined = append(combined, args[1:len(args)-1]...)
		combined = append(combined, last.elements...)
		return applyProcedure(args[0], combined, sourcePos{})
	}
}

func builtinMap() builtinProc {
	return func(args []value) (value, error) {
		if len(args) < 2 {
			return nil, &EvalError{Message: "map expects a procedure and at least 1 list"}
		}

		cursors := make([]listCursor, len(args)-1)
		for i, arg := range args[1:] {
			cursors[i] = newListCursor(arg)
		}

		callArgs := make([]value, len(cursors))
		var head *pairValue
		var tail *pairValue
		for {
			for i := range cursors {
				element, ok, err := cursors[i].next()
				if err != nil {
					return nil, err
				}
				if !ok {
					if head == nil {
						return listValue{}, nil
					}
					return head, nil
				}
				callArgs[i] = element
			}

			result, err := applyProcedure(args[0], callArgs, sourcePos{})
			if err != nil {
				return nil, err
			}

			cell := &pairValue{car: result, cdr: listValue{}}
			if head == nil {
				head = cell
				tail = cell
			} else {
				tail.cdr = cell
				tail = cell
			}
		}
	}
}

func builtinForEach() builtinProc {
	return func(args []value) (value, error) {
		if len(args) < 2 {
			return nil, &EvalError{Message: "for-each expects a procedure and at least 1 list"}
		}

		cursors := make([]listCursor, len(args)-1)
		for i, arg := range args[1:] {
			cursors[i] = newListCursor(arg)
		}

		callArgs := make([]value, len(cursors))
		for {
			for i := range cursors {
				element, ok, err := cursors[i].next()
				if err != nil {
					return nil, err
				}
				if !ok {
					return voidValue{}, nil
				}
				callArgs[i] = element
			}
			if _, err := applyProcedure(args[0], callArgs, sourcePos{}); err != nil {
				return nil, err
			}
		}
	}
}

func builtinMember() builtinProc {
	return builtinMemSearch("member", equalValues)
}

func builtinMemq() builtinProc {
	return builtinMemSearch("memq", eqValues)
}

func builtinMemv() builtinProc {
	return builtinMemSearch("memv", eqValues)
}

func builtinMemSearch(name string, matches func(left value, right value) bool) builtinProc {
	return func(args []value) (value, error) {
		if len(args) != 2 {
			return nil, &EvalError{Message: fmt.Sprintf("%s expects exactly 2 arguments", name)}
		}

		current := args[1]
		seen := map[*pairValue]struct{}{}
		for {
			switch list := current.(type) {
			case listValue:
				for i, element := range list.elements {
					if matches(args[0], element) {
						return makeList(copyValues(list.elements[i:])), nil
					}
				}
				return booleanValue(false), nil
			case *pairValue:
				if _, ok := seen[list]; ok {
					return nil, &EvalError{Message: "expected list"}
				}
				seen[list] = struct{}{}
				if matches(args[0], list.car) {
					return current, nil
				}
				current = list.cdr
			default:
				return nil, &EvalError{Message: "expected list"}
			}
		}
	}
}

func builtinError() builtinProc {
	return func(args []value) (value, error) {
		if len(args) == 0 {
			return nil, &EvalError{Message: "error expects at least 1 argument"}
		}

		parts := make([]string, 0, len(args))
		for i, arg := range args {
			var (
				text string
				err  error
			)
			if i == 0 {
				text, err = formatDisplayValue(arg)
			} else {
				text, err = formatValue(arg)
			}
			if err != nil {
				return nil, err
			}
			parts = append(parts, text)
		}

		return nil, &EvalError{Message: strings.Join(parts, " ")}
	}
}

func builtinAssoc() builtinProc {
	return builtinAssocSearch("assoc", equalValues)
}

func builtinAssq() builtinProc {
	return builtinAssocSearch("assq", eqValues)
}

func builtinAssv() builtinProc {
	return builtinAssocSearch("assv", eqValues)
}

func builtinAssocSearch(name string, matches func(left value, right value) bool) builtinProc {
	return func(args []value) (value, error) {
		if len(args) != 2 {
			return nil, &EvalError{Message: fmt.Sprintf("%s expects exactly 2 arguments", name)}
		}

		alist, err := expectListValue(args[1])
		if err != nil {
			return nil, err
		}

		for _, entry := range alist.elements {
			key, ok := assocKey(entry)
			if !ok {
				return nil, &EvalError{Message: fmt.Sprintf("%s expects a list of pairs", name)}
			}
			if matches(args[0], key) {
				return entry, nil
			}
		}

		return booleanValue(false), nil
	}
}

func builtinEq() builtinProc {
	return func(args []value) (value, error) {
		if len(args) != 2 {
			return nil, &EvalError{Message: "eq? expects exactly 2 arguments"}
		}
		return booleanValue(eqValues(args[0], args[1])), nil
	}
}

func builtinEqual() builtinProc {
	return func(args []value) (value, error) {
		if len(args) != 2 {
			return nil, &EvalError{Message: "equal? expects exactly 2 arguments"}
		}
		return booleanValue(equalValues(args[0], args[1])), nil
	}
}

func builtinProcedurePredicate() builtinProc {
	return func(args []value) (value, error) {
		if len(args) != 1 {
			return nil, &EvalError{Message: "procedure? expects exactly 1 argument"}
		}
		return booleanValue(isProcedureValue(args[0])), nil
	}
}

func builtinAbs() builtinProc {
	return func(args []value) (value, error) {
		if len(args) != 1 {
			return nil, &EvalError{Message: "abs expects exactly 1 argument"}
		}

		n, err := expectIntegerValue(args[0])
		if err != nil {
			return nil, err
		}
		if n < 0 {
			n = -n
		}
		return integerValue(n), nil
	}
}

func builtinGCD() builtinProc {
	return func(args []value) (value, error) {
		if len(args) == 0 {
			return integerValue(0), nil
		}

		result := 0
		for _, arg := range args {
			n, err := expectIntegerValue(arg)
			if err != nil {
				return nil, err
			}
			if n < 0 {
				n = -n
			}
			result = gcdIntegers(result, n)
		}
		return integerValue(result), nil
	}
}

func builtinLCM() builtinProc {
	return func(args []value) (value, error) {
		if len(args) == 0 {
			return integerValue(1), nil
		}

		result := 1
		for _, arg := range args {
			n, err := expectIntegerValue(arg)
			if err != nil {
				return nil, err
			}
			if n < 0 {
				n = -n
			}
			if result == 0 || n == 0 {
				result = 0
				continue
			}
			result = result / gcdIntegers(result, n) * n
		}
		return integerValue(result), nil
	}
}

func builtinTruncate() builtinProc {
	return func(args []value) (value, error) {
		if len(args) != 1 {
			return nil, &EvalError{Message: "truncate expects exactly 1 argument"}
		}
		return truncateValue(args[0])
	}
}

func builtinRound() builtinProc {
	return func(args []value) (value, error) {
		if len(args) != 1 {
			return nil, &EvalError{Message: "round expects exactly 1 argument"}
		}
		return roundValue(args[0])
	}
}

func builtinModulo() builtinProc {
	return func(args []value) (value, error) {
		dividend, divisor, err := expectIntegerPair("modulo", args)
		if err != nil {
			return nil, err
		}

		remainder := dividend % divisor
		if remainder != 0 && (remainder > 0) != (divisor > 0) {
			remainder += divisor
		}
		return integerValue(remainder), nil
	}
}

func builtinRemainder() builtinProc {
	return func(args []value) (value, error) {
		dividend, divisor, err := expectIntegerPair("remainder", args)
		if err != nil {
			return nil, err
		}
		return integerValue(dividend % divisor), nil
	}
}

func builtinQuotient() builtinProc {
	return func(args []value) (value, error) {
		dividend, divisor, err := expectIntegerPair("quotient", args)
		if err != nil {
			return nil, err
		}
		return integerValue(dividend / divisor), nil
	}
}

func builtinMinMax(name string) builtinProc {
	return func(args []value) (value, error) {
		if len(args) == 0 {
			return nil, &EvalError{Message: fmt.Sprintf("%s expects at least 1 argument", name)}
		}

		best, err := expectIntegerValue(args[0])
		if err != nil {
			return nil, err
		}
		for _, arg := range args[1:] {
			current, err := expectIntegerValue(arg)
			if err != nil {
				return nil, err
			}
			if name == "min" {
				if current < best {
					best = current
				}
			} else if current > best {
				best = current
			}
		}

		return integerValue(best), nil
	}
}

func builtinExpt() builtinProc {
	return func(args []value) (value, error) {
		if len(args) != 2 {
			return nil, &EvalError{Message: "expt expects exactly 2 arguments"}
		}

		base, err := expectIntegerValue(args[0])
		if err != nil {
			return nil, err
		}
		exponent, err := expectIntegerValue(args[1])
		if err != nil {
			return nil, err
		}
		if exponent < 0 {
			return nil, &EvalError{Message: "expt expects a non-negative exponent"}
		}

		result := 1
		factor := base
		for exponent > 0 {
			if exponent%2 == 1 {
				result *= factor
			}
			exponent /= 2
			if exponent > 0 {
				factor *= factor
			}
		}

		return integerValue(result), nil
	}
}

func builtinIntegerPredicate(check func(int) bool) builtinProc {
	return func(args []value) (value, error) {
		if len(args) != 1 {
			return nil, &EvalError{Message: "predicate expects exactly 1 argument"}
		}
		n, err := expectIntegerValue(args[0])
		if err != nil {
			return nil, err
		}
		return booleanValue(check(n)), nil
	}
}

func builtinExactToInexact() builtinProc {
	return func(args []value) (value, error) {
		if len(args) != 1 {
			return nil, &EvalError{Message: "exact->inexact expects exactly 1 argument"}
		}
		return exactToInexact(args[0])
	}
}

func builtinInexactToExact() builtinProc {
	return func(args []value) (value, error) {
		if len(args) != 1 {
			return nil, &EvalError{Message: "inexact->exact expects exactly 1 argument"}
		}
		return inexactToExact(args[0])
	}
}

func builtinNumerator() builtinProc {
	return func(args []value) (value, error) {
		if len(args) != 1 {
			return nil, &EvalError{Message: "numerator expects exactly 1 argument"}
		}

		numerator, _, err := rationalParts(args[0])
		if err != nil {
			return nil, err
		}
		return integerValue(numerator), nil
	}
}

func builtinDenominator() builtinProc {
	return func(args []value) (value, error) {
		if len(args) != 1 {
			return nil, &EvalError{Message: "denominator expects exactly 1 argument"}
		}

		_, denominator, err := rationalParts(args[0])
		if err != nil {
			return nil, err
		}
		return integerValue(denominator), nil
	}
}

func builtinListRef() builtinProc {
	return func(args []value) (value, error) {
		if len(args) != 2 {
			return nil, &EvalError{Message: "list-ref expects exactly 2 arguments"}
		}

		index, err := expectIntegerValue(args[1])
		if err != nil {
			return nil, err
		}

		return listRefValue(args[0], index)
	}
}

func builtinListTail() builtinProc {
	return func(args []value) (value, error) {
		if len(args) != 2 {
			return nil, &EvalError{Message: "list-tail expects exactly 2 arguments"}
		}

		index, err := expectIntegerValue(args[1])
		if err != nil {
			return nil, err
		}

		return listTailValue(args[0], index)
	}
}

func builtinPredicate(check func(value) bool) builtinProc {
	return func(args []value) (value, error) {
		if len(args) != 1 {
			return nil, &EvalError{Message: "predicate expects exactly 1 argument"}
		}
		return booleanValue(check(args[0])), nil
	}
}

func builtinDisplay(ctx *evalContext) builtinProc {
	return func(args []value) (value, error) {
		if len(args) != 1 {
			return nil, &EvalError{Message: "display expects exactly 1 argument"}
		}

		text, err := formatDisplayValue(args[0])
		if err != nil {
			return nil, err
		}
		ctx.output.WriteString(text)
		return voidValue{}, nil
	}
}

func builtinWrite(ctx *evalContext) builtinProc {
	return func(args []value) (value, error) {
		if len(args) != 1 {
			return nil, &EvalError{Message: "write expects exactly 1 argument"}
		}

		text, err := formatValue(args[0])
		if err != nil {
			return nil, err
		}
		ctx.output.WriteString(text)
		return voidValue{}, nil
	}
}

func builtinNewline(ctx *evalContext) builtinProc {
	return func(args []value) (value, error) {
		if len(args) != 0 {
			return nil, &EvalError{Message: "newline expects exactly 0 arguments"}
		}

		ctx.output.WriteByte('\n')
		return voidValue{}, nil
	}
}

func builtinStringAppend() builtinProc {
	return func(args []value) (value, error) {
		var builder strings.Builder
		for _, arg := range args {
			str, err := expectStringValue(arg)
			if err != nil {
				return nil, err
			}
			builder.WriteString(str)
		}
		return stringValue(builder.String()), nil
	}
}

func builtinMakeString() builtinProc {
	return func(args []value) (value, error) {
		if len(args) < 1 || len(args) > 2 {
			return nil, &EvalError{Message: "make-string expects 1 or 2 arguments"}
		}

		length, err := expectIntegerValue(args[0])
		if err != nil {
			return nil, err
		}
		if length < 0 {
			return nil, &EvalError{Message: "make-string length must be non-negative"}
		}

		fill := rune(0)
		if len(args) == 2 {
			char, err := expectCharValue(args[1])
			if err != nil {
				return nil, err
			}
			fill = rune(char)
		}

		runes := make([]rune, length)
		for i := range runes {
			runes[i] = fill
		}
		return stringValue(string(runes)), nil
	}
}

func builtinString() builtinProc {
	return func(args []value) (value, error) {
		runes := make([]rune, len(args))
		for i, arg := range args {
			char, err := expectCharValue(arg)
			if err != nil {
				return nil, err
			}
			runes[i] = rune(char)
		}
		return stringValue(string(runes)), nil
	}
}

func builtinStringLength() builtinProc {
	return func(args []value) (value, error) {
		if len(args) != 1 {
			return nil, &EvalError{Message: "string-length expects exactly 1 argument"}
		}

		str, err := expectStringValue(args[0])
		if err != nil {
			return nil, err
		}
		return integerValue(utf8.RuneCountInString(str)), nil
	}
}

func builtinSubstring() builtinProc {
	return func(args []value) (value, error) {
		if len(args) != 3 {
			return nil, &EvalError{Message: "substring expects exactly 3 arguments"}
		}

		str, err := expectStringValue(args[0])
		if err != nil {
			return nil, err
		}

		start, err := expectIntegerValue(args[1])
		if err != nil {
			return nil, err
		}
		end, err := expectIntegerValue(args[2])
		if err != nil {
			return nil, err
		}

		runes := []rune(str)
		if start < 0 || end < start || end > len(runes) {
			return nil, &EvalError{Message: "substring indices out of range"}
		}

		return stringValue(string(runes[start:end])), nil
	}
}

func builtinStringToNumber() builtinProc {
	return func(args []value) (value, error) {
		if len(args) != 1 {
			return nil, &EvalError{Message: "string->number expects exactly 1 argument"}
		}

		str, err := expectStringValue(args[0])
		if err != nil {
			return nil, err
		}

		number, ok, parseErr := parseNumericToken(str)
		if parseErr != nil || !ok {
			return booleanValue(false), nil
		}
		return number, nil
	}
}

func builtinNumberToString() builtinProc {
	return func(args []value) (value, error) {
		if len(args) != 1 {
			return nil, &EvalError{Message: "number->string expects exactly 1 argument"}
		}

		formatted, ok := formatNumberValue(args[0])
		if !ok {
			return nil, &EvalError{Message: "expected number"}
		}
		return stringValue(formatted), nil
	}
}

func builtinSymbolToString() builtinProc {
	return func(args []value) (value, error) {
		if len(args) != 1 {
			return nil, &EvalError{Message: "symbol->string expects exactly 1 argument"}
		}

		sym, err := expectSymbolValue(args[0])
		if err != nil {
			return nil, err
		}
		return stringValue(sym), nil
	}
}

func builtinStringToSymbol() builtinProc {
	return func(args []value) (value, error) {
		if len(args) != 1 {
			return nil, &EvalError{Message: "string->symbol expects exactly 1 argument"}
		}

		str, err := expectStringValue(args[0])
		if err != nil {
			return nil, err
		}
		return symbolValue(str), nil
	}
}

func builtinStringToList() builtinProc {
	return func(args []value) (value, error) {
		if len(args) != 1 {
			return nil, &EvalError{Message: "string->list expects exactly 1 argument"}
		}

		runes, err := expectStringRunes(args[0])
		if err != nil {
			return nil, err
		}

		elements := make([]value, len(runes))
		for i, r := range runes {
			elements[i] = charValue(r)
		}
		return makeList(elements), nil
	}
}

func builtinListToString() builtinProc {
	return func(args []value) (value, error) {
		if len(args) != 1 {
			return nil, &EvalError{Message: "list->string expects exactly 1 argument"}
		}

		list, err := expectListValue(args[0])
		if err != nil {
			return nil, err
		}

		runes := make([]rune, len(list.elements))
		for i, element := range list.elements {
			char, err := expectCharValue(element)
			if err != nil {
				return nil, err
			}
			runes[i] = rune(char)
		}

		return stringValue(string(runes)), nil
	}
}

func builtinStringRef() builtinProc {
	return func(args []value) (value, error) {
		if len(args) != 2 {
			return nil, &EvalError{Message: "string-ref expects exactly 2 arguments"}
		}

		str, err := expectStringValue(args[0])
		if err != nil {
			return nil, err
		}
		index, err := expectIntegerValue(args[1])
		if err != nil {
			return nil, err
		}

		runes := []rune(str)
		if index < 0 || index >= len(runes) {
			return nil, &EvalError{Message: "string-ref index out of range"}
		}
		return charValue(runes[index]), nil
	}
}

func builtinStringCopy() builtinProc {
	return func(args []value) (value, error) {
		if len(args) != 1 {
			return nil, &EvalError{Message: "string-copy expects exactly 1 argument"}
		}

		runes, err := expectStringRunes(args[0])
		if err != nil {
			return nil, err
		}

		return &mutableStringValue{runes: copyRunes(runes)}, nil
	}
}

func builtinStringSet() builtinProc {
	return func(args []value) (value, error) {
		if len(args) != 3 {
			return nil, &EvalError{Message: "string-set! expects exactly 3 arguments"}
		}

		var str *mutableStringValue
		switch candidate := args[0].(type) {
		case *mutableStringValue:
			str = candidate
		case stringValue:
			return nil, &EvalError{Message: "string-set! cannot modify immutable strings"}
		default:
			return nil, &EvalError{Message: "expected string"}
		}

		index, err := expectIntegerValue(args[1])
		if err != nil {
			return nil, err
		}
		char, err := expectCharValue(args[2])
		if err != nil {
			return nil, err
		}
		if index < 0 || index >= len(str.runes) {
			return nil, &EvalError{Message: "string-set! index out of range"}
		}

		str.runes[index] = rune(char)
		return voidValue{}, nil
	}
}

func builtinStringCompare(name string) builtinProc {
	return func(args []value) (value, error) {
		if len(args) < 2 {
			return nil, &EvalError{Message: fmt.Sprintf("%s expects at least 2 arguments", name)}
		}

		prev, err := expectStringValue(args[0])
		if err != nil {
			return nil, err
		}
		for _, arg := range args[1:] {
			current, err := expectStringValue(arg)
			if err != nil {
				return nil, err
			}

			ok := false
			switch name {
			case "string=?":
				ok = prev == current
			case "string<?":
				ok = prev < current
			case "string>?":
				ok = prev > current
			case "string<=?":
				ok = prev <= current
			case "string>=?":
				ok = prev >= current
			default:
				return nil, &EvalError{Message: "unknown string comparison"}
			}
			if !ok {
				return booleanValue(false), nil
			}
			prev = current
		}

		return booleanValue(true), nil
	}
}

func builtinStringCIEqual() builtinProc {
	return func(args []value) (value, error) {
		if len(args) < 2 {
			return nil, &EvalError{Message: "string-ci=? expects at least 2 arguments"}
		}

		prev, err := expectStringValue(args[0])
		if err != nil {
			return nil, err
		}
		for _, arg := range args[1:] {
			current, err := expectStringValue(arg)
			if err != nil {
				return nil, err
			}
			if !strings.EqualFold(prev, current) {
				return booleanValue(false), nil
			}
			prev = current
		}

		return booleanValue(true), nil
	}
}

func builtinStringCase(name string) builtinProc {
	return func(args []value) (value, error) {
		if len(args) != 1 {
			return nil, &EvalError{Message: fmt.Sprintf("string-%s expects exactly 1 argument", name)}
		}

		runes, err := expectStringRunes(args[0])
		if err != nil {
			return nil, err
		}
		for i, r := range runes {
			if name == "upcase" {
				runes[i] = unicode.ToUpper(r)
			} else {
				runes[i] = unicode.ToLower(r)
			}
		}
		return stringValue(string(runes)), nil
	}
}

func builtinCharPredicate(check func(rune) bool) builtinProc {
	return func(args []value) (value, error) {
		if len(args) != 1 {
			return nil, &EvalError{Message: "predicate expects exactly 1 argument"}
		}

		char, err := expectCharValue(args[0])
		if err != nil {
			return nil, err
		}
		return booleanValue(check(rune(char))), nil
	}
}

func builtinCharCase(name string) builtinProc {
	return func(args []value) (value, error) {
		if len(args) != 1 {
			return nil, &EvalError{Message: fmt.Sprintf("char-%s expects exactly 1 argument", name)}
		}

		char, err := expectCharValue(args[0])
		if err != nil {
			return nil, err
		}
		if name == "upcase" {
			return charValue(unicode.ToUpper(rune(char))), nil
		}
		return charValue(unicode.ToLower(rune(char))), nil
	}
}

func builtinCharToInteger() builtinProc {
	return func(args []value) (value, error) {
		if len(args) != 1 {
			return nil, &EvalError{Message: "char->integer expects exactly 1 argument"}
		}

		char, err := expectCharValue(args[0])
		if err != nil {
			return nil, err
		}
		return integerValue(char), nil
	}
}

func builtinIntegerToChar() builtinProc {
	return func(args []value) (value, error) {
		if len(args) != 1 {
			return nil, &EvalError{Message: "integer->char expects exactly 1 argument"}
		}

		codePoint, err := expectIntegerValue(args[0])
		if err != nil {
			return nil, err
		}

		r := rune(codePoint)
		if !utf8.ValidRune(r) {
			return nil, &EvalError{Message: "integer->char expects a valid Unicode code point"}
		}

		return charValue(r), nil
	}
}

func builtinCharCompare(name string) builtinProc {
	return func(args []value) (value, error) {
		if len(args) < 2 {
			return nil, &EvalError{Message: fmt.Sprintf("%s expects at least 2 arguments", name)}
		}

		prev, err := expectCharValue(args[0])
		if err != nil {
			return nil, err
		}
		for _, arg := range args[1:] {
			current, err := expectCharValue(arg)
			if err != nil {
				return nil, err
			}

			ok := false
			switch name {
			case "char=?":
				ok = prev == current
			case "char<?":
				ok = prev < current
			default:
				return nil, &EvalError{Message: "unknown character comparison"}
			}
			if !ok {
				return booleanValue(false), nil
			}
			prev = current
		}

		return booleanValue(true), nil
	}
}

func expectIntegerValue(v value) (int, error) {
	switch v := v.(type) {
	case integerValue:
		return int(v), nil
	case rationalValue:
		if v.denominator == 1 {
			return v.numerator, nil
		}
	}
	return 0, &EvalError{Message: "expected integer"}
}

func expectStringValue(v value) (string, error) {
	switch v := v.(type) {
	case stringValue:
		return string(v), nil
	case *mutableStringValue:
		return string(v.runes), nil
	default:
		return "", &EvalError{Message: "expected string"}
	}
}

func expectSymbolValue(v value) (string, error) {
	sym, ok := v.(symbolValue)
	if !ok {
		return "", &EvalError{Message: "expected symbol"}
	}
	return string(sym), nil
}

func expectListValue(v value) (listValue, error) {
	elements, err := listElements(v)
	if err != nil {
		return listValue{}, err
	}
	return listValue{elements: elements}, nil
}

func expectStringRunes(v value) ([]rune, error) {
	switch v := v.(type) {
	case stringValue:
		return []rune(string(v)), nil
	case *mutableStringValue:
		return copyRunes(v.runes), nil
	default:
		return nil, &EvalError{Message: "expected string"}
	}
}

func expectMutableStringValue(v value) (*mutableStringValue, error) {
	str, ok := v.(*mutableStringValue)
	if !ok {
		return nil, &EvalError{Message: "expected mutable string"}
	}
	return str, nil
}

func expectCharValue(v value) (charValue, error) {
	char, ok := v.(charValue)
	if !ok {
		return 0, &EvalError{Message: "expected character"}
	}
	return char, nil
}

func isStringValue(v value) bool {
	switch v.(type) {
	case stringValue, *mutableStringValue:
		return true
	default:
		return false
	}
}

func copyValues(values []value) []value {
	if len(values) == 0 {
		return nil
	}

	copied := make([]value, len(values))
	copy(copied, values)
	return copied
}

func copyRunes(runes []rune) []rune {
	if len(runes) == 0 {
		return nil
	}

	copied := make([]rune, len(runes))
	copy(copied, runes)
	return copied
}

func makeList(elements []value) value {
	return makeListWithTail(elements, listValue{})
}

func makeListWithTail(elements []value, tail value) value {
	if len(elements) == 0 {
		return tail
	}

	var result value = tail
	for i := len(elements) - 1; i >= 0; i-- {
		result = &pairValue{car: elements[i], cdr: result}
	}
	return result
}

func isEmptyListValue(v value) bool {
	list, ok := v.(listValue)
	return ok && len(list.elements) == 0
}

type listCursor struct {
	rest    []value
	current value
	seen    map[*pairValue]struct{}
}

func newListCursor(v value) listCursor {
	if list, ok := v.(listValue); ok {
		return listCursor{rest: list.elements}
	}
	return listCursor{
		current: v,
		seen:    map[*pairValue]struct{}{},
	}
}

func (c *listCursor) next() (value, bool, error) {
	for {
		if len(c.rest) > 0 {
			element := c.rest[0]
			c.rest = c.rest[1:]
			return element, true, nil
		}

		switch current := c.current.(type) {
		case nil:
			return nil, false, nil
		case listValue:
			if len(current.elements) == 0 {
				c.current = nil
				return nil, false, nil
			}
			c.rest = current.elements
			c.current = nil
		case *pairValue:
			if _, ok := c.seen[current]; ok {
				return nil, false, &EvalError{Message: "expected list"}
			}
			c.seen[current] = struct{}{}
			c.current = current.cdr
			return current.car, true, nil
		default:
			return nil, false, &EvalError{Message: "expected list"}
		}
	}
}

func listElements(v value) ([]value, error) {
	elements := []value{}
	current := v
	seen := map[*pairValue]struct{}{}

	for {
		switch currentPair := current.(type) {
		case listValue:
			elements = append(elements, currentPair.elements...)
			return elements, nil
		case *pairValue:
			if _, ok := seen[currentPair]; ok {
				return nil, &EvalError{Message: "expected list"}
			}
			seen[currentPair] = struct{}{}
			elements = append(elements, currentPair.car)
			current = currentPair.cdr
		default:
			return nil, &EvalError{Message: "expected list"}
		}
	}
}

func properListElements(v value) ([]value, bool) {
	elements, err := listElements(v)
	if err != nil {
		return nil, false
	}
	return elements, true
}

func equalValueSlices(left []value, right []value) bool {
	if len(left) != len(right) {
		return false
	}
	for i, element := range left {
		if !equalValues(element, right[i]) {
			return false
		}
	}
	return true
}

func gcdIntegers(a int, b int) int {
	for b != 0 {
		a, b = b, a%b
	}
	if a < 0 {
		return -a
	}
	return a
}

func truncateValue(v value) (value, error) {
	switch v := v.(type) {
	case integerValue:
		return v, nil
	case rationalValue:
		return integerValue(v.numerator / v.denominator), nil
	case inexactValue:
		return inexactValue(math.Trunc(float64(v))), nil
	default:
		return nil, &EvalError{Message: "expected number"}
	}
}

func roundValue(v value) (value, error) {
	switch v := v.(type) {
	case integerValue:
		return v, nil
	case rationalValue:
		sign := 1
		numerator := v.numerator
		if numerator < 0 {
			sign = -1
			numerator = -numerator
		}
		quotient := numerator / v.denominator
		remainder := numerator % v.denominator
		if remainder*2 >= v.denominator {
			quotient++
		}
		return integerValue(sign * quotient), nil
	case inexactValue:
		return inexactValue(math.Round(float64(v))), nil
	default:
		return nil, &EvalError{Message: "expected number"}
	}
}

func listRefValue(v value, index int) (value, error) {
	if index < 0 {
		return nil, &EvalError{Message: "list-ref index out of range"}
	}

	tail, err := listTailValue(v, index)
	if err != nil {
		return nil, err
	}

	switch tail := tail.(type) {
	case listValue:
		if len(tail.elements) == 0 {
			return nil, &EvalError{Message: "list-ref index out of range"}
		}
		return tail.elements[0], nil
	case *pairValue:
		return tail.car, nil
	default:
		return nil, &EvalError{Message: "list-ref index out of range"}
	}
}

func listTailValue(v value, index int) (value, error) {
	if index < 0 {
		return nil, &EvalError{Message: "list-tail index out of range"}
	}

	current := v
	remaining := index
	seen := map[*pairValue]struct{}{}

	for {
		if remaining == 0 {
			switch current.(type) {
			case listValue, *pairValue:
				return current, nil
			default:
				return nil, &EvalError{Message: "expected list"}
			}
		}

		switch currentValue := current.(type) {
		case listValue:
			if remaining > len(currentValue.elements) {
				return nil, &EvalError{Message: "list-tail index out of range"}
			}
			return makeList(copyValues(currentValue.elements[remaining:])), nil
		case *pairValue:
			if _, ok := seen[currentValue]; ok {
				return nil, &EvalError{Message: "expected list"}
			}
			seen[currentValue] = struct{}{}
			current = currentValue.cdr
			remaining--
		default:
			return nil, &EvalError{Message: "expected list"}
		}
	}
}

func isProperList(v value) bool {
	current := v
	seen := map[*pairValue]struct{}{}

	for {
		switch currentPair := current.(type) {
		case listValue:
			return true
		case *pairValue:
			if _, ok := seen[currentPair]; ok {
				return false
			}
			seen[currentPair] = struct{}{}
			current = currentPair.cdr
		default:
			return false
		}
	}
}

func datumFromNode(expr node) (value, error) {
	switch expr := expr.(type) {
	case integerValue, rationalValue, inexactValue, booleanValue, stringValue, charValue:
		return expr, nil
	case symbolNode:
		return symbolValue(expr.name), nil
	case vectorNode:
		elements := make([]value, len(expr.elements))
		for i, element := range expr.elements {
			datum, err := datumFromNode(element)
			if err != nil {
				return nil, err
			}
			elements[i] = datum
		}
		return &vectorValue{elements: elements}, nil
	case dottedListNode:
		tail, err := datumFromNode(expr.tail)
		if err != nil {
			return nil, err
		}

		for i := len(expr.elements) - 1; i >= 0; i-- {
			car, err := datumFromNode(expr.elements[i])
			if err != nil {
				return nil, err
			}
			tail = &pairValue{car: car, cdr: tail}
		}
		return tail, nil
	case listNode:
		elements := make([]value, len(expr.elements))
		for i, element := range expr.elements {
			datum, err := datumFromNode(element)
			if err != nil {
				return nil, err
			}
			elements[i] = datum
		}
		return makeList(elements), nil
	default:
		return nil, &EvalError{Message: "invalid quoted datum"}
	}
}

func evalQuasiquoteNode(expr node, env *environment, depth int) (value, error) {
	if inner, ok := quasiquoteInner(expr); ok {
		evaluated, err := evalQuasiquoteNode(inner, env, depth+1)
		if err != nil {
			return nil, err
		}
		return makeList([]value{symbolValue("quasiquote"), evaluated}), nil
	}

	if inner, ok := unquoteInner(expr); ok {
		if depth == 1 {
			return eval(inner, env)
		}
		evaluated, err := evalQuasiquoteNode(inner, env, depth-1)
		if err != nil {
			return nil, err
		}
		return makeList([]value{symbolValue("unquote"), evaluated}), nil
	}

	if inner, ok := unquoteSplicingInner(expr); ok {
		if depth == 1 {
			return nil, &EvalError{Message: "unquote-splicing is only valid within a list or vector"}
		}
		evaluated, err := evalQuasiquoteNode(inner, env, depth-1)
		if err != nil {
			return nil, err
		}
		return makeList([]value{symbolValue("unquote-splicing"), evaluated}), nil
	}

	switch expr := expr.(type) {
	case integerValue, rationalValue, inexactValue, booleanValue, stringValue, charValue:
		return expr, nil
	case symbolNode:
		return symbolValue(expr.name), nil
	case listNode:
		return evalQuasiquoteList(expr.elements, nil, false, env, depth)
	case dottedListNode:
		return evalQuasiquoteList(expr.elements, expr.tail, true, env, depth)
	case vectorNode:
		return evalQuasiquoteVector(expr.elements, env, depth)
	default:
		return nil, &EvalError{Message: "invalid quasiquote datum"}
	}
}

func evalQuasiquoteList(elements []node, tailExpr node, hasTail bool, env *environment, depth int) (value, error) {
	values := make([]value, 0, len(elements))
	for _, element := range elements {
		if inner, ok := unquoteSplicingInner(element); ok && depth == 1 {
			spliced, err := eval(inner, env)
			if err != nil {
				return nil, err
			}
			items, err := listElements(spliced)
			if err != nil {
				return nil, err
			}
			values = append(values, items...)
			continue
		}

		evaluated, err := evalQuasiquoteNode(element, env, depth)
		if err != nil {
			return nil, err
		}
		values = append(values, evaluated)
	}

	if !hasTail {
		return makeList(values), nil
	}

	if _, ok := unquoteSplicingInner(tailExpr); ok && depth == 1 {
		return nil, &EvalError{Message: "unquote-splicing is only valid in sequence position"}
	}

	tail, err := evalQuasiquoteNode(tailExpr, env, depth)
	if err != nil {
		return nil, err
	}
	return makeListWithTail(values, tail), nil
}

func evalQuasiquoteVector(elements []node, env *environment, depth int) (value, error) {
	values := make([]value, 0, len(elements))
	for _, element := range elements {
		if inner, ok := unquoteSplicingInner(element); ok && depth == 1 {
			spliced, err := eval(inner, env)
			if err != nil {
				return nil, err
			}
			items, err := listElements(spliced)
			if err != nil {
				return nil, err
			}
			values = append(values, items...)
			continue
		}

		evaluated, err := evalQuasiquoteNode(element, env, depth)
		if err != nil {
			return nil, err
		}
		values = append(values, evaluated)
	}
	return &vectorValue{elements: values}, nil
}

func quasiquoteInner(expr node) (node, bool) {
	return readerFormInner(expr, "quasiquote")
}

func unquoteInner(expr node) (node, bool) {
	return readerFormInner(expr, "unquote")
}

func unquoteSplicingInner(expr node) (node, bool) {
	return readerFormInner(expr, "unquote-splicing")
}

func readerFormInner(expr node, name string) (node, bool) {
	list, ok := expr.(listNode)
	if !ok || len(list.elements) != 2 {
		return nil, false
	}

	head, ok := symbolName(list.elements[0])
	if !ok || head != name {
		return nil, false
	}
	return list.elements[1], true
}

func formatValue(v value) (string, error) {
	if formatted, ok := formatNumberValue(v); ok {
		return formatted, nil
	}

	switch v := v.(type) {
	case booleanValue:
		if v {
			return "#t", nil
		}
		return "#f", nil
	case stringValue:
		return strconv.Quote(string(v)), nil
	case *mutableStringValue:
		return strconv.Quote(string(v.runes)), nil
	case symbolValue:
		return string(v), nil
	case charValue:
		return formatChar(v), nil
	case *vectorValue:
		parts := make([]string, len(v.elements))
		for i, element := range v.elements {
			formatted, err := formatValue(element)
			if err != nil {
				return "", err
			}
			parts[i] = formatted
		}
		return "#(" + strings.Join(parts, " ") + ")", nil
	case multiValueValue:
		return "", multiValueContextError(len(v.values))
	case *recordValue:
		return "#<record " + v.recordType.name + ">", nil
	case builtinProc, *closureValue, *caseClosureValue, *continuationValue, *callCCProcValue, *callWithValuesProcValue, *dynamicWindProcValue, *raiseProcValue, *withExceptionHandlerProcValue:
		return "#<procedure>", nil
	case *syntaxValue:
		return "#<syntax>", nil
	case *repeatedSyntaxValue:
		return "#<syntax-list>", nil
	case *pairValue:
		return formatPair(v, formatValue)
	case listValue:
		if len(v.elements) == 0 {
			return "()", nil
		}

		parts := make([]string, len(v.elements))
		for i, element := range v.elements {
			formatted, err := formatValue(element)
			if err != nil {
				return "", err
			}
			parts[i] = formatted
		}
		return "(" + strings.Join(parts, " ") + ")", nil
	case voidValue:
		return "", nil
	default:
		return "", &EvalError{Message: "cannot format value"}
	}
}

func formatDisplayValue(v value) (string, error) {
	switch v := v.(type) {
	case stringValue:
		return string(v), nil
	case *mutableStringValue:
		return string(v.runes), nil
	case charValue:
		return string(rune(v)), nil
	case *pairValue:
		return formatPair(v, formatDisplayValue)
	case listValue:
		if len(v.elements) == 0 {
			return "()", nil
		}

		parts := make([]string, len(v.elements))
		for i, element := range v.elements {
			formatted, err := formatDisplayValue(element)
			if err != nil {
				return "", err
			}
			parts[i] = formatted
		}
		return "(" + strings.Join(parts, " ") + ")", nil
	case multiValueValue:
		return "", multiValueContextError(len(v.values))
	default:
		return formatValue(v)
	}
}

func formatPair(v *pairValue, formatter func(value) (string, error)) (string, error) {
	parts := []string{}
	current := v
	for {
		head, err := formatter(current.car)
		if err != nil {
			return "", err
		}
		parts = append(parts, head)

		switch tail := current.cdr.(type) {
		case *pairValue:
			current = tail
		case listValue:
			for _, element := range tail.elements {
				formatted, err := formatter(element)
				if err != nil {
					return "", err
				}
				parts = append(parts, formatted)
			}
			return "(" + strings.Join(parts, " ") + ")", nil
		default:
			formattedTail, err := formatter(tail)
			if err != nil {
				return "", err
			}
			return "(" + strings.Join(parts, " ") + " . " + formattedTail + ")", nil
		}
	}
}

func formatChar(v charValue) string {
	switch rune(v) {
	case ' ':
		return "#\\space"
	case '\n':
		return "#\\newline"
	default:
		return "#\\" + string(rune(v))
	}
}

func isTruthy(v value) bool {
	b, ok := v.(booleanValue)
	return !ok || bool(b)
}

func isListValue(v value) bool {
	return isProperList(v)
}

func isPairValue(v value) bool {
	switch v := v.(type) {
	case listValue:
		return len(v.elements) > 0
	case *pairValue:
		return true
	default:
		return false
	}
}

func isProcedureValue(v value) bool {
	switch v.(type) {
	case builtinProc, *closureValue, *caseClosureValue, *continuationValue, *callCCProcValue, *callWithValuesProcValue, *dynamicWindProcValue, *raiseProcValue, *withExceptionHandlerProcValue:
		return true
	default:
		return false
	}
}

func expectIntegerPair(name string, args []value) (int, int, error) {
	if len(args) != 2 {
		return 0, 0, &EvalError{Message: fmt.Sprintf("%s expects exactly 2 arguments", name)}
	}

	first, err := expectIntegerValue(args[0])
	if err != nil {
		return 0, 0, err
	}
	second, err := expectIntegerValue(args[1])
	if err != nil {
		return 0, 0, err
	}
	if second == 0 {
		return 0, 0, &EvalError{Message: "division by zero"}
	}

	return first, second, nil
}

func assocKey(v value) (value, bool) {
	switch pair := v.(type) {
	case listValue:
		if len(pair.elements) == 0 {
			return nil, false
		}
		return pair.elements[0], true
	case *pairValue:
		return pair.car, true
	default:
		return nil, false
	}
}

func eqValues(left, right value) bool {
	switch left := left.(type) {
	case integerValue:
		switch right := right.(type) {
		case integerValue:
			return left == right
		case rationalValue:
			return right.denominator == 1 && int(left) == right.numerator
		case inexactValue:
			return float64(left) == float64(right)
		default:
			return false
		}
	case rationalValue:
		switch right := right.(type) {
		case integerValue:
			return left.denominator == 1 && left.numerator == int(right)
		case rationalValue:
			return left.numerator == right.numerator && left.denominator == right.denominator
		case inexactValue:
			return float64(left.numerator)/float64(left.denominator) == float64(right)
		default:
			return false
		}
	case inexactValue:
		switch right := right.(type) {
		case integerValue:
			return float64(left) == float64(right)
		case rationalValue:
			return float64(left) == float64(right.numerator)/float64(right.denominator)
		case inexactValue:
			return left == right
		default:
			return false
		}
	case booleanValue:
		right, ok := right.(booleanValue)
		return ok && left == right
	case stringValue:
		right, ok := right.(stringValue)
		return ok && left == right
	case symbolValue:
		right, ok := right.(symbolValue)
		return ok && left == right
	case charValue:
		right, ok := right.(charValue)
		return ok && left == right
	case *mutableStringValue:
		right, ok := right.(*mutableStringValue)
		return ok && left == right
	case listValue:
		right, ok := right.(listValue)
		return ok && len(left.elements) == 0 && len(right.elements) == 0
	case *vectorValue:
		right, ok := right.(*vectorValue)
		return ok && left == right
	case *pairValue:
		right, ok := right.(*pairValue)
		return ok && left == right
	case *closureValue:
		right, ok := right.(*closureValue)
		return ok && left == right
	case *caseClosureValue:
		right, ok := right.(*caseClosureValue)
		return ok && left == right
	case *continuationValue:
		right, ok := right.(*continuationValue)
		return ok && left == right
	case *callCCProcValue:
		right, ok := right.(*callCCProcValue)
		return ok && left == right
	case *callWithValuesProcValue:
		right, ok := right.(*callWithValuesProcValue)
		return ok && left == right
	case *dynamicWindProcValue:
		right, ok := right.(*dynamicWindProcValue)
		return ok && left == right
	case *raiseProcValue:
		right, ok := right.(*raiseProcValue)
		return ok && left == right
	case *withExceptionHandlerProcValue:
		right, ok := right.(*withExceptionHandlerProcValue)
		return ok && left == right
	case *recordValue:
		right, ok := right.(*recordValue)
		return ok && left == right
	case voidValue:
		_, ok := right.(voidValue)
		return ok
	default:
		return false
	}
}

func equalValues(left, right value) bool {
	switch left := left.(type) {
	case integerValue:
		switch right := right.(type) {
		case integerValue:
			return left == right
		case rationalValue:
			return right.denominator == 1 && int(left) == right.numerator
		case inexactValue:
			return float64(left) == float64(right)
		default:
			return false
		}
	case rationalValue:
		switch right := right.(type) {
		case integerValue:
			return left.denominator == 1 && left.numerator == int(right)
		case rationalValue:
			return left.numerator == right.numerator && left.denominator == right.denominator
		case inexactValue:
			return float64(left.numerator)/float64(left.denominator) == float64(right)
		default:
			return false
		}
	case inexactValue:
		switch right := right.(type) {
		case integerValue:
			return float64(left) == float64(right)
		case rationalValue:
			return float64(left) == float64(right.numerator)/float64(right.denominator)
		case inexactValue:
			return left == right
		default:
			return false
		}
	case booleanValue:
		right, ok := right.(booleanValue)
		return ok && left == right
	case stringValue:
		switch right := right.(type) {
		case stringValue:
			return left == right
		case *mutableStringValue:
			return string(left) == string(right.runes)
		default:
			return false
		}
	case symbolValue:
		right, ok := right.(symbolValue)
		return ok && left == right
	case charValue:
		right, ok := right.(charValue)
		return ok && left == right
	case *mutableStringValue:
		switch right := right.(type) {
		case stringValue:
			return string(left.runes) == string(right)
		case *mutableStringValue:
			return string(left.runes) == string(right.runes)
		default:
			return false
		}
	case listValue:
		rightElements, ok := properListElements(right)
		if !ok {
			return false
		}
		return equalValueSlices(left.elements, rightElements)
	case *vectorValue:
		right, ok := right.(*vectorValue)
		if !ok || len(left.elements) != len(right.elements) {
			return false
		}
		for i, element := range left.elements {
			if !equalValues(element, right.elements[i]) {
				return false
			}
		}
		return true
	case *pairValue:
		if rightList, ok := right.(listValue); ok {
			leftElements, ok := properListElements(left)
			return ok && equalValueSlices(leftElements, rightList.elements)
		}
		right, ok := right.(*pairValue)
		return ok && equalValues(left.car, right.car) && equalValues(left.cdr, right.cdr)
	case *recordValue:
		right, ok := right.(*recordValue)
		return ok && left == right
	case voidValue:
		_, ok := right.(voidValue)
		return ok
	default:
		return false
	}
}

func symbolName(expr node) (string, bool) {
	sym, ok := expr.(symbolNode)
	if !ok {
		return "", false
	}
	return sym.name, true
}

func nodePos(expr node) sourcePos {
	switch expr := expr.(type) {
	case listNode:
		return expr.pos
	case dottedListNode:
		return expr.pos
	case vectorNode:
		return expr.pos
	case symbolNode:
		return expr.pos
	default:
		return sourcePos{}
	}
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
		return nil, errorAt(p.currentPos(), "unexpected end of input")
	}

	switch p.peek() {
	case '(':
		return p.parseList()
	case '\'':
		return p.parseQuote()
	case '`':
		return p.parseQuasiquote()
	case ',':
		if p.hasPrefix(",@") {
			return p.parseUnquoteSplicing()
		}
		return p.parseUnquote()
	case '"':
		return p.parseString()
	case ')':
		return nil, errorAt(p.currentPos(), "unexpected )")
	case '#':
		if p.hasPrefix("#'") {
			return p.parseSyntaxQuote()
		}
		if p.hasPrefix("#(") {
			return p.parseVector()
		}
		return p.parseAtom()
	default:
		return p.parseAtom()
	}
}

func (p *parser) parseList() (node, error) {
	pos := p.currentPos()
	p.offset++
	var elements []node
	for {
		p.skipWhitespaceAndComments()
		if p.eof() {
			return nil, errorAt(pos, "unterminated list")
		}
		if p.peek() == ')' {
			p.offset++
			return listNode{elements: elements, pos: pos}, nil
		}
		elem, err := p.parseExpr()
		if err != nil {
			return nil, err
		}
		elements = append(elements, elem)
	}
}

func (p *parser) parseDatum() (node, error) {
	p.skipWhitespaceAndComments()
	if p.eof() {
		return nil, errorAt(p.currentPos(), "unexpected end of input")
	}

	switch p.peek() {
	case '(':
		return p.parseDatumList()
	case '\'':
		return p.parseQuote()
	case '`':
		return p.parseQuasiquote()
	case ',':
		if p.hasPrefix(",@") {
			return p.parseUnquoteSplicing()
		}
		return p.parseUnquote()
	case '"':
		return p.parseString()
	case ')':
		return nil, errorAt(p.currentPos(), "unexpected )")
	case '#':
		if p.hasPrefix("#'") {
			return p.parseSyntaxQuote()
		}
		if p.hasPrefix("#(") {
			return p.parseVector()
		}
		return p.parseAtom()
	default:
		return p.parseAtom()
	}
}

func (p *parser) parseDatumList() (node, error) {
	pos := p.currentPos()
	p.offset++

	var elements []node
	var tail node
	hasTail := false

	for {
		p.skipWhitespaceAndComments()
		if p.eof() {
			return nil, errorAt(pos, "unterminated list")
		}
		if p.peek() == ')' {
			p.offset++
			if hasTail {
				return dottedListNode{elements: elements, tail: tail, pos: pos}, nil
			}
			return listNode{elements: elements, pos: pos}, nil
		}

		if !hasTail && p.isDatumDot() {
			dotPos := p.currentPos()
			if len(elements) == 0 {
				return nil, errorAt(dotPos, "invalid dotted list")
			}

			p.offset++
			p.skipWhitespaceAndComments()

			var err error
			tail, err = p.parseDatum()
			if err != nil {
				return nil, err
			}
			hasTail = true

			p.skipWhitespaceAndComments()
			if p.eof() {
				return nil, errorAt(pos, "unterminated list")
			}
			if p.peek() != ')' {
				return nil, errorAt(dotPos, "invalid dotted list")
			}
			continue
		}

		element, err := p.parseDatum()
		if err != nil {
			return nil, err
		}
		elements = append(elements, element)
	}
}

func (p *parser) parseQuote() (node, error) {
	pos := p.currentPos()
	p.offset++
	expr, err := p.parseDatum()
	if err != nil {
		return nil, err
	}
	return listNode{
		elements: []node{
			symbolNode{name: "quote", pos: pos},
			expr,
		},
		pos: pos,
	}, nil
}

func (p *parser) parseQuasiquote() (node, error) {
	pos := p.currentPos()
	p.offset++
	expr, err := p.parseDatum()
	if err != nil {
		return nil, err
	}
	return listNode{
		elements: []node{
			symbolNode{name: "quasiquote", pos: pos},
			expr,
		},
		pos: pos,
	}, nil
}

func (p *parser) parseUnquote() (node, error) {
	pos := p.currentPos()
	p.offset++
	expr, err := p.parseExpr()
	if err != nil {
		return nil, err
	}
	return listNode{
		elements: []node{
			symbolNode{name: "unquote", pos: pos},
			expr,
		},
		pos: pos,
	}, nil
}

func (p *parser) parseUnquoteSplicing() (node, error) {
	pos := p.currentPos()
	p.offset += 2
	expr, err := p.parseExpr()
	if err != nil {
		return nil, err
	}
	return listNode{
		elements: []node{
			symbolNode{name: "unquote-splicing", pos: pos},
			expr,
		},
		pos: pos,
	}, nil
}

func (p *parser) parseSyntaxQuote() (node, error) {
	pos := p.currentPos()
	p.offset += 2
	expr, err := p.parseDatum()
	if err != nil {
		return nil, err
	}
	return listNode{
		elements: []node{
			symbolNode{name: "syntax", pos: pos},
			expr,
		},
		pos: pos,
	}, nil
}

func (p *parser) parseString() (node, error) {
	pos := p.currentPos()
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
				return nil, errorAt(pos, "unterminated string escape")
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
				return nil, errorAt(pos, "unsupported escape: \\%c", escaped)
			}
			continue
		}
		builder.WriteRune(r)
	}
	return nil, errorAt(pos, "unterminated string")
}

func (p *parser) parseAtom() (node, error) {
	pos := p.currentPos()
	start := p.offset
	for !p.eof() {
		r := p.peek()
		if unicode.IsSpace(r) || r == '(' || r == ')' || r == ';' || r == '\'' {
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

	if strings.HasPrefix(token, "#\\") {
		return parseCharLiteral(token, pos)
	}

	if number, ok, err := parseNumericToken(token); err != nil {
		return nil, errorAt(pos, err.Error())
	} else if ok {
		return number, nil
	}

	return symbolNode{name: token, pos: pos}, nil
}

func (p *parser) isDatumDot() bool {
	if p.peek() != '.' {
		return false
	}

	nextOffset := p.offset + 1
	if nextOffset >= len(p.source) {
		return true
	}

	r, _ := utf8.DecodeRuneInString(p.source[nextOffset:])
	return unicode.IsSpace(r) || r == ')' || r == ';'
}

func parseCharLiteral(token string, pos sourcePos) (node, error) {
	literal := token[2:]
	switch literal {
	case "space":
		return charValue(' '), nil
	case "newline":
		return charValue('\n'), nil
	}

	runes := []rune(literal)
	if len(runes) != 1 {
		return nil, errorAt(pos, "invalid character literal: %s", token)
	}

	return charValue(runes[0]), nil
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

func (p *parser) currentPos() sourcePos {
	return p.posAt(p.offset)
}

func (p *parser) posAt(offset int) sourcePos {
	pos := startPos()
	for i := 0; i < offset && i < len(p.source); {
		r, size := utf8.DecodeRuneInString(p.source[i:])
		if r == '\n' {
			pos.Line++
			pos.Col = 1
		} else {
			pos.Col++
		}
		i += size
	}
	return pos
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
