package ming

import (
	"fmt"
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

type numberValue int
type boolValue bool
type stringValue string
type symbolValue string
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
	params []string
	body   []expr
	env    *env
}

type env struct {
	parent *env
	vars   map[string]value
}

var emptyList = emptyListValue{}

func (n numberValue) schemeString() string {
	return strconv.Itoa(int(n))
}

func (numberValue) isTruthy() bool {
	return true
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

func (s stringValue) schemeString() string {
	return strconv.Quote(string(s))
}

func (stringValue) isTruthy() bool {
	return true
}

func (s symbolValue) schemeString() string {
	return string(s)
}

func (symbolValue) isTruthy() bool {
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
	var builder strings.Builder
	builder.WriteByte('(')

	current := p
	for {
		builder.WriteString(current.car.schemeString())

		switch next := current.cdr.(type) {
		case emptyListValue:
			builder.WriteByte(')')
			return builder.String()
		case pairValue:
			builder.WriteByte(' ')
			current = next
		default:
			builder.WriteString(" . ")
			builder.WriteString(next.schemeString())
			builder.WriteByte(')')
			return builder.String()
		}
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

func (p closureValue) call(args []value) (value, error) {
	if len(args) != len(p.params) {
		return nil, &EvalError{Message: fmt.Sprintf("expected %d arguments, got %d", len(p.params), len(args))}
	}

	callEnv := newEnv(p.env)
	for i, name := range p.params {
		callEnv.define(name, args[i])
	}

	return evalSequence(p.body, callEnv)
}

func newEnv(parent *env) *env {
	return &env{
		parent: parent,
		vars:   make(map[string]value),
	}
}

func (e *env) define(name string, v value) {
	e.vars[name] = v
}

func (e *env) lookup(name string) (value, bool) {
	for current := e; current != nil; current = current.parent {
		if v, ok := current.vars[name]; ok {
			return v, true
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
		return evalCompare(args, "<", func(a, b int) bool { return a < b })
	}})
	global.define(">", builtinProc{name: ">", fn: func(args []value) (value, error) {
		return evalCompare(args, ">", func(a, b int) bool { return a > b })
	}})
	global.define("=", builtinProc{name: "=", fn: func(args []value) (value, error) {
		return evalCompare(args, "=", func(a, b int) bool { return a == b })
	}})
	global.define("<=", builtinProc{name: "<=", fn: func(args []value) (value, error) {
		return evalCompare(args, "<=", func(a, b int) bool { return a <= b })
	}})
	global.define("not", builtinProc{name: "not", fn: evalNot})
	return global
}

func evalInput(input string) (result string, output string, err error) {
	exprs, err := parseProgram(input)
	if err != nil {
		return "", "", err
	}

	env := newGlobalEnv()
	last := value(voidValue{})

	for _, expr := range exprs {
		last, err = evalExpr(expr, env)
		if err != nil {
			return "", "", err
		}
	}

	return last.schemeString(), "", nil
}

func evalSequence(exprs []expr, env *env) (value, error) {
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

func evalExpr(e expr, env *env) (value, error) {
	switch expr := e.(type) {
	case numberExpr:
		return numberValue(expr), nil
	case boolExpr:
		return boolValue(expr), nil
	case stringExpr:
		return stringValue(expr), nil
	case symbolExpr:
		v, ok := env.lookup(string(expr))
		if !ok {
			return nil, &EvalError{Message: fmt.Sprintf("unbound variable: %s", string(expr))}
		}
		return v, nil
	case listExpr:
		return evalList(expr, env)
	default:
		return nil, &EvalError{Message: "unknown expression"}
	}
}

func evalList(items listExpr, env *env) (value, error) {
	if len(items) == 0 {
		return nil, &EvalError{Message: "cannot evaluate empty list"}
	}

	if operator, ok := items[0].(symbolExpr); ok {
		switch string(operator) {
		case "and":
			return evalAnd(items[1:], env)
		case "or":
			return evalOr(items[1:], env)
		case "if":
			return evalIf(items[1:], env)
		case "define":
			return evalDefine(items[1:], env)
		case "quote":
			return evalQuote(items[1:])
		case "lambda":
			return evalLambda(items[1:], env)
		}
	}

	operator, err := evalExpr(items[0], env)
	if err != nil {
		return nil, err
	}

	proc, ok := operator.(procedure)
	if !ok {
		return nil, &EvalError{Message: fmt.Sprintf("attempt to call non-procedure: %s", operator.schemeString())}
	}

	args := make([]value, 0, len(items)-1)
	for _, item := range items[1:] {
		arg, err := evalExpr(item, env)
		if err != nil {
			return nil, err
		}
		args = append(args, arg)
	}

	return proc.call(args)
}

func evalAnd(items []expr, env *env) (value, error) {
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

func evalOr(items []expr, env *env) (value, error) {
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

func evalIf(parts []expr, env *env) (value, error) {
	if len(parts) != 2 && len(parts) != 3 {
		return nil, &EvalError{Message: "'if' expects 2 or 3 arguments"}
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

func evalDefine(parts []expr, env *env) (value, error) {
	if len(parts) < 2 {
		return nil, &EvalError{Message: "'define' expects at least 2 arguments"}
	}

	switch target := parts[0].(type) {
	case symbolExpr:
		if len(parts) != 2 {
			return nil, &EvalError{Message: "'define' expects exactly 2 arguments for variable definitions"}
		}

		v, err := evalExpr(parts[1], env)
		if err != nil {
			return nil, err
		}
		env.define(string(target), v)
		return voidValue{}, nil
	case listExpr:
		if len(target) == 0 {
			return nil, &EvalError{Message: "function name is required"}
		}

		name, ok := target[0].(symbolExpr)
		if !ok {
			return nil, &EvalError{Message: "function name must be a symbol"}
		}

		params, err := parseParams(target[1:])
		if err != nil {
			return nil, err
		}

		proc := closureValue{
			params: params,
			body:   parts[1:],
			env:    env,
		}
		env.define(string(name), proc)
		return voidValue{}, nil
	default:
		return nil, &EvalError{Message: "invalid define target"}
	}
}

func evalQuote(parts []expr) (value, error) {
	if len(parts) != 1 {
		return nil, &EvalError{Message: "'quote' expects exactly 1 argument"}
	}
	return quoteExpr(parts[0])
}

func evalLambda(parts []expr, env *env) (value, error) {
	if len(parts) < 2 {
		return nil, &EvalError{Message: "'lambda' expects a parameter list and body"}
	}

	paramExprs, ok := parts[0].(listExpr)
	if !ok {
		return nil, &EvalError{Message: "'lambda' parameter list must be a list"}
	}

	params, err := parseParams(paramExprs)
	if err != nil {
		return nil, err
	}

	return closureValue{
		params: params,
		body:   parts[1:],
		env:    env,
	}, nil
}

func parseParams(items []expr) ([]string, error) {
	params := make([]string, 0, len(items))
	seen := make(map[string]struct{}, len(items))
	for _, item := range items {
		name, ok := item.(symbolExpr)
		if !ok {
			return nil, &EvalError{Message: "parameter name must be a symbol"}
		}
		if _, exists := seen[string(name)]; exists {
			return nil, &EvalError{Message: fmt.Sprintf("duplicate parameter: %s", string(name))}
		}
		seen[string(name)] = struct{}{}
		params = append(params, string(name))
	}
	return params, nil
}

func quoteExpr(e expr) (value, error) {
	switch expr := e.(type) {
	case numberExpr:
		return numberValue(expr), nil
	case boolExpr:
		return boolValue(expr), nil
	case stringExpr:
		return stringValue(expr), nil
	case symbolExpr:
		return symbolValue(expr), nil
	case listExpr:
		return quoteList(expr)
	default:
		return nil, &EvalError{Message: "unknown quoted expression"}
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
	sum := 0
	for _, arg := range args {
		n, err := expectNumber(arg)
		if err != nil {
			return nil, err
		}
		sum += n
	}
	return numberValue(sum), nil
}

func evalSub(args []value) (value, error) {
	if len(args) == 0 {
		return nil, &EvalError{Message: "'-' expects at least 1 argument"}
	}

	first, err := expectNumber(args[0])
	if err != nil {
		return nil, err
	}
	if len(args) == 1 {
		return numberValue(-first), nil
	}

	result := first
	for _, arg := range args[1:] {
		n, err := expectNumber(arg)
		if err != nil {
			return nil, err
		}
		result -= n
	}
	return numberValue(result), nil
}

func evalMul(args []value) (value, error) {
	product := 1
	for _, arg := range args {
		n, err := expectNumber(arg)
		if err != nil {
			return nil, err
		}
		product *= n
	}
	return numberValue(product), nil
}

func evalDiv(args []value) (value, error) {
	if len(args) < 2 {
		return nil, &EvalError{Message: "'/' expects at least 2 arguments"}
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
		if n == 0 {
			return nil, &EvalError{Message: "division by zero"}
		}
		result /= n
	}
	return numberValue(result), nil
}

func evalCompare(args []value, name string, pred func(int, int) bool) (value, error) {
	if len(args) < 2 {
		return nil, &EvalError{Message: fmt.Sprintf("'%s' expects at least 2 arguments", name)}
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
		return nil, &EvalError{Message: "'not' expects exactly 1 argument"}
	}
	return boolValue(!args[0].isTruthy()), nil
}

func expectNumber(v value) (int, error) {
	n, ok := v.(numberValue)
	if !ok {
		return 0, &EvalError{Message: fmt.Sprintf("expected number, got %s", v.schemeString())}
	}
	return int(n), nil
}
