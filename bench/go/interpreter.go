package ming

import (
	"fmt"
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

type symbolNode struct {
	name string
	pos  sourcePos
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

type listValue struct {
	elements []value
}

type voidValue struct{}

type evalContext struct {
	output strings.Builder
}

type builtinProc func(args []value) (value, error)

type closureValue struct {
	params []string
	body   []node
	env    *environment
}

type binding struct {
	value value
}

type environment struct {
	parent *environment
	values map[string]*binding
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
	last := value(voidValue{})
	for _, expr := range nodes {
		last, err = eval(expr, env)
		if err != nil {
			return "", "", err
		}
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
	env.define("not", builtinNot())
	env.define("cons", builtinCons())
	env.define("car", builtinCar())
	env.define("cdr", builtinCdr())
	env.define("null?", builtinNull())
	env.define("list", builtinList())
	env.define("length", builtinLength())
	env.define("append", builtinAppend())
	env.define("string?", builtinPredicate(func(v value) bool {
		return isStringValue(v)
	}))
	env.define("number?", builtinPredicate(func(v value) bool {
		_, ok := v.(integerValue)
		return ok
	}))
	env.define("boolean?", builtinPredicate(func(v value) bool {
		_, ok := v.(booleanValue)
		return ok
	}))
	env.define("pair?", builtinPredicate(func(v value) bool {
		list, ok := v.(listValue)
		return ok && len(list.elements) > 0
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
	env.define("string-append", builtinStringAppend())
	env.define("string-length", builtinStringLength())
	env.define("substring", builtinSubstring())
	env.define("string->number", builtinStringToNumber())
	env.define("number->string", builtinNumberToString())
	env.define("symbol->string", builtinSymbolToString())
	env.define("string->symbol", builtinStringToSymbol())
	env.define("string-ref", builtinStringRef())
	env.define("string-copy", builtinStringCopy())
	env.define("string-set!", builtinStringSet())
	return env
}

func newEnvironment(parent *environment) *environment {
	return &environment{
		parent: parent,
		values: map[string]*binding{},
	}
}

func (e *environment) define(name string, val value) {
	e.values[name] = &binding{value: val}
}

func (e *environment) lookupBinding(name string) (*binding, bool) {
	for current := e; current != nil; current = current.parent {
		cell, ok := current.values[name]
		if ok {
			return cell, true
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
	switch expr := expr.(type) {
	case integerValue, booleanValue, stringValue, charValue:
		return expr, nil
	case symbolNode:
		val, ok := env.lookup(expr.name)
		if !ok {
			return nil, errorAt(expr.pos, "unbound variable: %s", expr.name)
		}
		return val, nil
	case listNode:
		return evalList(expr, env)
	default:
		return nil, errorAt(nodePos(expr), "unknown expression")
	}
}

func evalList(list listNode, env *environment) (value, error) {
	if len(list.elements) == 0 {
		return nil, errorAt(list.pos, "cannot evaluate empty list")
	}

	if name, ok := symbolName(list.elements[0]); ok {
		switch name {
		case "and":
			result, err := evalAnd(list.elements[1:], env)
			return result, withErrorPos(err, list.pos)
		case "or":
			result, err := evalOr(list.elements[1:], env)
			return result, withErrorPos(err, list.pos)
		case "define":
			result, err := evalDefine(list.elements[1:], env)
			return result, withErrorPos(err, list.pos)
		case "set!":
			result, err := evalSet(list.elements[1:], env)
			return result, withErrorPos(err, list.pos)
		case "if":
			result, err := evalIf(list.elements[1:], env)
			return result, withErrorPos(err, list.pos)
		case "quote":
			result, err := evalQuote(list.elements[1:])
			return result, withErrorPos(err, list.pos)
		case "lambda":
			result, err := evalLambda(list.elements[1:], env)
			return result, withErrorPos(err, list.pos)
		case "begin":
			result, err := evalBegin(list.elements[1:], env)
			return result, withErrorPos(err, list.pos)
		case "cond":
			result, err := evalCond(list.elements[1:], env)
			return result, withErrorPos(err, list.pos)
		case "let":
			result, err := evalLet(list.elements[1:], env)
			return result, withErrorPos(err, list.pos)
		}
	}

	operator, err := eval(list.elements[0], env)
	if err != nil {
		return nil, err
	}

	args, err := evalArgs(list.elements[1:], env)
	if err != nil {
		return nil, err
	}
	return apply(operator, args, list.pos)
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

func evalAnd(args []node, env *environment) (value, error) {
	result := value(booleanValue(true))
	for _, arg := range args {
		evaluated, err := eval(arg, env)
		if err != nil {
			return nil, err
		}
		result = evaluated
		if !isTruthy(evaluated) {
			return evaluated, nil
		}
	}
	return result, nil
}

func evalOr(args []node, env *environment) (value, error) {
	result := value(booleanValue(false))
	for _, arg := range args {
		evaluated, err := eval(arg, env)
		if err != nil {
			return nil, err
		}
		result = evaluated
		if isTruthy(evaluated) {
			return evaluated, nil
		}
	}
	return result, nil
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

	if !env.set(target.name, val) {
		return nil, errorAt(target.pos, "unbound variable: %s", target.name)
	}
	return voidValue{}, nil
}

func evalIf(args []node, env *environment) (value, error) {
	if len(args) != 3 {
		return nil, &EvalError{Message: "if expects exactly 3 arguments"}
	}

	condition, err := eval(args[0], env)
	if err != nil {
		return nil, err
	}
	if isTruthy(condition) {
		return eval(args[1], env)
	}
	return eval(args[2], env)
}

func evalQuote(args []node) (value, error) {
	if len(args) != 1 {
		return nil, &EvalError{Message: "quote expects exactly 1 argument"}
	}
	return datumFromNode(args[0])
}

func evalLambda(args []node, env *environment) (value, error) {
	if len(args) < 2 {
		return nil, &EvalError{Message: "lambda expects parameters and a body"}
	}

	params, ok := args[0].(listNode)
	if !ok {
		return nil, &EvalError{Message: "lambda parameters must be a list"}
	}
	return makeClosure(params.elements, args[1:], env)
}

func evalBegin(args []node, env *environment) (value, error) {
	if len(args) == 0 {
		return voidValue{}, nil
	}
	return evalSequence(args, env)
}

func evalCond(args []node, env *environment) (value, error) {
	for i, clauseExpr := range args {
		clause, ok := clauseExpr.(listNode)
		if !ok || len(clause.elements) == 0 {
			return nil, &EvalError{Message: "cond clauses must be non-empty lists"}
		}

		if name, ok := symbolName(clause.elements[0]); ok && name == "else" {
			if i != len(args)-1 {
				return nil, &EvalError{Message: "else clause must be last"}
			}
			if len(clause.elements) == 1 {
				return voidValue{}, nil
			}
			return evalSequence(clause.elements[1:], env)
		}

		testValue, err := eval(clause.elements[0], env)
		if err != nil {
			return nil, err
		}
		if !isTruthy(testValue) {
			continue
		}
		if len(clause.elements) == 1 {
			return testValue, nil
		}
		return evalSequence(clause.elements[1:], env)
	}
	return voidValue{}, nil
}

func evalLet(args []node, env *environment) (value, error) {
	if len(args) < 2 {
		return nil, &EvalError{Message: "let expects bindings and a body"}
	}

	if name, ok := symbolName(args[0]); ok {
		return evalNamedLet(name, args[1:], env)
	}

	bindingList, ok := args[0].(listNode)
	if !ok {
		return nil, &EvalError{Message: "let bindings must be a list"}
	}

	names, values, err := evalBindings(bindingList.elements, env)
	if err != nil {
		return nil, err
	}

	letEnv := newEnvironment(env)
	for i, name := range names {
		letEnv.define(name, values[i])
	}
	return evalSequence(args[1:], letEnv)
}

func evalNamedLet(name string, args []node, env *environment) (value, error) {
	if len(args) < 2 {
		return nil, &EvalError{Message: "named let expects bindings and a body"}
	}

	bindingList, ok := args[0].(listNode)
	if !ok {
		return nil, &EvalError{Message: "named let bindings must be a list"}
	}

	names, values, err := evalBindings(bindingList.elements, env)
	if err != nil {
		return nil, err
	}

	letEnv := newEnvironment(env)
	proc := &closureValue{
		params: names,
		body:   args[1:],
		env:    letEnv,
	}
	letEnv.define(name, proc)
	return apply(proc, values, sourcePos{})
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
	if len(body) == 0 {
		return nil, &EvalError{Message: "lambda requires a body"}
	}

	params, err := parseParamNames(paramExprs)
	if err != nil {
		return nil, err
	}

	return &closureValue{
		params: params,
		body:   body,
		env:    env,
	}, nil
}

func parseParamNames(paramExprs []node) ([]string, error) {
	params := make([]string, len(paramExprs))
	for i, expr := range paramExprs {
		name, ok := symbolName(expr)
		if !ok {
			return nil, &EvalError{Message: "parameter list must contain only symbols"}
		}
		params[i] = name
	}
	return params, nil
}

func apply(proc value, args []value, pos sourcePos) (value, error) {
	switch proc := proc.(type) {
	case builtinProc:
		result, err := proc(args)
		return result, withErrorPos(err, pos)
	case *closureValue:
		if len(args) != len(proc.params) {
			return nil, errorAt(pos, "expected %d arguments, got %d", len(proc.params), len(args))
		}

		callEnv := newEnvironment(proc.env)
		for i, name := range proc.params {
			callEnv.define(name, args[i])
		}
		return evalSequence(proc.body, callEnv)
	default:
		return nil, errorAt(pos, "not a procedure")
	}
}

func evalSequence(exprs []node, env *environment) (value, error) {
	last := value(voidValue{})
	for _, expr := range exprs {
		var err error
		last, err = eval(expr, env)
		if err != nil {
			return nil, err
		}
	}
	return last, nil
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

		total := 0
		if name == "*" {
			total = 1
		}

		for _, arg := range args {
			current, err := expectIntegerValue(arg)
			if err != nil {
				return nil, err
			}
			if name == "+" {
				total += current
			} else {
				total *= current
			}
		}
		return integerValue(total), nil
	}
}

func builtinSub() builtinProc {
	return func(args []value) (value, error) {
		if len(args) == 0 {
			return nil, &EvalError{Message: "- expects at least 1 argument"}
		}

		first, err := expectIntegerValue(args[0])
		if err != nil {
			return nil, err
		}
		if len(args) == 1 {
			return integerValue(-first), nil
		}

		total := first
		for _, arg := range args[1:] {
			current, err := expectIntegerValue(arg)
			if err != nil {
				return nil, err
			}
			total -= current
		}
		return integerValue(total), nil
	}
}

func builtinDiv() builtinProc {
	return func(args []value) (value, error) {
		if len(args) < 2 {
			return nil, &EvalError{Message: "/ expects at least 2 arguments"}
		}

		total, err := expectIntegerValue(args[0])
		if err != nil {
			return nil, err
		}

		for _, arg := range args[1:] {
			current, err := expectIntegerValue(arg)
			if err != nil {
				return nil, err
			}
			if current == 0 {
				return nil, &EvalError{Message: "division by zero"}
			}
			total /= current
		}
		return integerValue(total), nil
	}
}

func builtinCompare(name string) builtinProc {
	return func(args []value) (value, error) {
		if len(args) < 2 {
			return nil, &EvalError{Message: fmt.Sprintf("%s expects at least 2 arguments", name)}
		}

		prev, err := expectIntegerValue(args[0])
		if err != nil {
			return nil, err
		}

		for _, arg := range args[1:] {
			current, err := expectIntegerValue(arg)
			if err != nil {
				return nil, err
			}

			var ok bool
			switch name {
			case "<":
				ok = prev < current
			case ">":
				ok = prev > current
			case "=":
				ok = prev == current
			case "<=":
				ok = prev <= current
			default:
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

		list, ok := args[1].(listValue)
		if !ok {
			return nil, &EvalError{Message: "cons expects a list as its second argument"}
		}

		elements := make([]value, 0, len(list.elements)+1)
		elements = append(elements, args[0])
		elements = append(elements, list.elements...)
		return listValue{elements: elements}, nil
	}
}

func builtinCar() builtinProc {
	return func(args []value) (value, error) {
		if len(args) != 1 {
			return nil, &EvalError{Message: "car expects exactly 1 argument"}
		}

		list, err := expectListValue(args[0])
		if err != nil {
			return nil, err
		}
		if len(list.elements) == 0 {
			return nil, &EvalError{Message: "car expects a non-empty list"}
		}
		return list.elements[0], nil
	}
}

func builtinCdr() builtinProc {
	return func(args []value) (value, error) {
		if len(args) != 1 {
			return nil, &EvalError{Message: "cdr expects exactly 1 argument"}
		}

		list, err := expectListValue(args[0])
		if err != nil {
			return nil, err
		}
		if len(list.elements) == 0 {
			return nil, &EvalError{Message: "cdr expects a non-empty list"}
		}
		return listValue{elements: copyValues(list.elements[1:])}, nil
	}
}

func builtinNull() builtinProc {
	return func(args []value) (value, error) {
		if len(args) != 1 {
			return nil, &EvalError{Message: "null? expects exactly 1 argument"}
		}

		list, ok := args[0].(listValue)
		return booleanValue(ok && len(list.elements) == 0), nil
	}
}

func builtinList() builtinProc {
	return func(args []value) (value, error) {
		return listValue{elements: copyValues(args)}, nil
	}
}

func builtinLength() builtinProc {
	return func(args []value) (value, error) {
		if len(args) != 1 {
			return nil, &EvalError{Message: "length expects exactly 1 argument"}
		}

		list, err := expectListValue(args[0])
		if err != nil {
			return nil, err
		}
		return integerValue(len(list.elements)), nil
	}
}

func builtinAppend() builtinProc {
	return func(args []value) (value, error) {
		if len(args) == 0 {
			return listValue{elements: nil}, nil
		}

		var combined []value
		for _, arg := range args {
			list, err := expectListValue(arg)
			if err != nil {
				return nil, err
			}
			combined = append(combined, list.elements...)
		}
		return listValue{elements: copyValues(combined)}, nil
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

		n, parseErr := strconv.Atoi(str)
		if parseErr != nil {
			return booleanValue(false), nil
		}
		return integerValue(n), nil
	}
}

func builtinNumberToString() builtinProc {
	return func(args []value) (value, error) {
		if len(args) != 1 {
			return nil, &EvalError{Message: "number->string expects exactly 1 argument"}
		}

		n, err := expectIntegerValue(args[0])
		if err != nil {
			return nil, err
		}
		return stringValue(strconv.Itoa(n)), nil
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

		str, err := expectMutableStringValue(args[0])
		if err != nil {
			return nil, err
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

func expectIntegerValue(v value) (int, error) {
	n, ok := v.(integerValue)
	if !ok {
		return 0, &EvalError{Message: "expected integer"}
	}
	return int(n), nil
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
	list, ok := v.(listValue)
	if !ok {
		return listValue{}, &EvalError{Message: "expected list"}
	}
	return list, nil
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

func datumFromNode(expr node) (value, error) {
	switch expr := expr.(type) {
	case integerValue, booleanValue, stringValue, charValue:
		return expr, nil
	case symbolNode:
		return symbolValue(expr.name), nil
	case listNode:
		elements := make([]value, len(expr.elements))
		for i, element := range expr.elements {
			datum, err := datumFromNode(element)
			if err != nil {
				return nil, err
			}
			elements[i] = datum
		}
		return listValue{elements: elements}, nil
	default:
		return nil, &EvalError{Message: "invalid quoted datum"}
	}
}

func formatValue(v value) (string, error) {
	switch v := v.(type) {
	case integerValue:
		return strconv.Itoa(int(v)), nil
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
	default:
		return formatValue(v)
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
	case '"':
		return p.parseString()
	case ')':
		return nil, errorAt(p.currentPos(), "unexpected )")
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

func (p *parser) parseQuote() (node, error) {
	pos := p.currentPos()
	p.offset++
	expr, err := p.parseExpr()
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

	if isIntegerLiteral(token) {
		n, err := strconv.Atoi(token)
		if err != nil {
			return nil, errorAt(pos, "invalid integer: %s", token)
		}
		return integerValue(n), nil
	}

	return symbolNode{name: token, pos: pos}, nil
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
