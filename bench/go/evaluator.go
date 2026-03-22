package ming

import (
	"fmt"
	"strconv"
	"strings"
)

// Env holds variable bindings.
type Env struct {
	bindings map[string]*Value
	parent   *Env
	output   *strings.Builder // shared output buffer (only on root env)
}

func (e *Env) getOutput() *strings.Builder {
	if e.output != nil {
		return e.output
	}
	if e.parent != nil {
		return e.parent.getOutput()
	}
	return nil
}

func newEnv(parent *Env) *Env {
	return &Env{bindings: make(map[string]*Value), parent: parent}
}

func (e *Env) get(name string) (*Value, bool) {
	if v, ok := e.bindings[name]; ok {
		return v, true
	}
	if e.parent != nil {
		return e.parent.get(name)
	}
	return nil, false
}

func (e *Env) set(name string, val *Value) {
	e.bindings[name] = val
}

func makeGlobalEnv() *Env {
	env := newEnv(nil)
	env.set("+", builtinVal("+", builtinAdd))
	env.set("-", builtinVal("-", builtinSub))
	env.set("*", builtinVal("*", builtinMulFn))
	env.set("/", builtinVal("/", builtinDivide))
	env.set("<", builtinVal("<", makeCompare("<", func(a, b int64) bool { return a < b })))
	env.set(">", builtinVal(">", makeCompare(">", func(a, b int64) bool { return a > b })))
	env.set("=", builtinVal("=", makeCompare("=", func(a, b int64) bool { return a == b })))
	env.set("<=", builtinVal("<=", makeCompare("<=", func(a, b int64) bool { return a <= b })))
	env.set(">=", builtinVal(">=", makeCompare(">=", func(a, b int64) bool { return a >= b })))
	env.set("not", builtinVal("not", builtinNot))
	env.set("cons", builtinVal("cons", builtinCons))
	env.set("car", builtinVal("car", builtinCar))
	env.set("cdr", builtinVal("cdr", builtinCdr))
	env.set("null?", builtinVal("null?", builtinNullQ))
	env.set("list", builtinVal("list", builtinList))
	env.set("length", builtinVal("length", builtinLength))
	env.set("append", builtinVal("append", builtinAppend))
	env.set("string?", builtinVal("string?", makeTypePred(KindString)))
	env.set("number?", builtinVal("number?", makeTypePred(KindInteger)))
	env.set("boolean?", builtinVal("boolean?", makeTypePred(KindBoolean)))
	env.set("pair?", builtinVal("pair?", makeTypePred(KindPair)))
	env.set("symbol?", builtinVal("symbol?", makeTypePred(KindSymbol)))
	env.set("char?", builtinVal("char?", makeTypePred(KindChar)))
	env.set("procedure?", builtinVal("procedure?", func(args []*Value) (*Value, error) {
		if len(args) != 1 {
			return nil, &EvalError{Message: "procedure?: expected 1 argument"}
		}
		return boolVal(args[0].Kind == KindBuiltin || args[0].Kind == KindLambda), nil
	}))
	env.set("string-append", builtinVal("string-append", builtinStringAppend))
	env.set("string-length", builtinVal("string-length", builtinStringLength))
	env.set("substring", builtinVal("substring", builtinSubstring))
	env.set("string->number", builtinVal("string->number", builtinStringToNumber))
	env.set("number->string", builtinVal("number->string", builtinNumberToString))
	env.set("symbol->string", builtinVal("symbol->string", builtinSymbolToString))
	env.set("string->symbol", builtinVal("string->symbol", builtinStringToSymbol))
	env.set("string-ref", builtinVal("string-ref", builtinStringRef))
	env.set("string-set!", builtinVal("string-set!", builtinStringSet))
	env.set("string-copy", builtinVal("string-copy", builtinStringCopy))
	return env
}

// tailCallVal creates a tail-call trampoline value.
func tailCallVal(expr *Value, env *Env) *Value {
	return &Value{Kind: KindTailCall, Car: expr, ClosureEnv: env}
}

func eval(expr *Value, env *Env) (*Value, error) {
	for {
		switch expr.Kind {
		case KindInteger, KindBoolean, KindString, KindChar:
			return expr, nil
		case KindSymbol:
			v, ok := env.get(expr.Str)
			if !ok {
				return nil, posError(expr, fmt.Sprintf("unbound variable: %s", expr.Str))
			}
			return v, nil
		case KindPair:
			result, err := evalList(expr, env)
			if err != nil {
				return nil, err
			}
			if result.Kind == KindTailCall {
				expr = result.Car
				env = result.ClosureEnv
				continue
			}
			return result, nil
		case KindNull:
			return nil, posError(expr, "cannot evaluate empty list")
		}
		return nil, posError(expr, "unknown expression type")
	}
}

func evalList(expr *Value, env *Env) (*Value, error) {
	head := expr.Car

	// Special forms
	if head.Kind == KindSymbol {
		switch head.Str {
		case "and":
			return evalAnd(expr.Cdr, env)
		case "or":
			return evalOr(expr.Cdr, env)
		case "if":
			return evalIf(expr.Cdr, env, expr)
		case "define":
			return evalDefine(expr.Cdr, env, expr)
		case "quote":
			return expr.Cdr.Car, nil
		case "lambda":
			return evalLambda(expr.Cdr, env)
		case "let":
			return evalLet(expr.Cdr, env)
		case "begin":
			return evalBegin(expr.Cdr, env)
		case "cond":
			return evalCond(expr.Cdr, env)
		case "display":
			return evalDisplay(expr.Cdr, env)
		case "write":
			return evalWrite(expr.Cdr, env)
		case "newline":
			return evalNewline(env)
		}
	}

	// Function application
	fn, err := eval(head, env)
	if err != nil {
		return nil, err
	}

	// Evaluate arguments
	args, err := evalArgs(expr.Cdr, env)
	if err != nil {
		return nil, err
	}

	if fn.Kind == KindBuiltin {
		result, err := fn.Builtin(args)
		if err != nil {
			return nil, wrapErrorPos(err, expr)
		}
		return result, nil
	}

	if fn.Kind == KindLambda {
		return applyLambda(fn, args)
	}

	return nil, posError(expr, fmt.Sprintf("not a procedure: %s", fn.String()))
}

func evalArgs(list *Value, env *Env) ([]*Value, error) {
	var args []*Value
	cur := list
	for cur.Kind == KindPair {
		v, err := eval(cur.Car, env)
		if err != nil {
			return nil, err
		}
		args = append(args, v)
		cur = cur.Cdr
	}
	return args, nil
}

func evalAnd(args *Value, env *Env) (*Value, error) {
	if args.Kind == KindNull {
		return boolVal(true), nil
	}
	cur := args
	for cur.Cdr.Kind == KindPair {
		v, err := eval(cur.Car, env)
		if err != nil {
			return nil, err
		}
		if !v.isTruthy() {
			return v, nil
		}
		cur = cur.Cdr
	}
	// Tail call for the last expression
	return tailCallVal(cur.Car, env), nil
}

func evalOr(args *Value, env *Env) (*Value, error) {
	if args.Kind == KindNull {
		return boolVal(false), nil
	}
	cur := args
	for cur.Cdr.Kind == KindPair {
		v, err := eval(cur.Car, env)
		if err != nil {
			return nil, err
		}
		if v.isTruthy() {
			return v, nil
		}
		cur = cur.Cdr
	}
	// Tail call for the last expression
	return tailCallVal(cur.Car, env), nil
}

func evalIf(args *Value, env *Env, expr *Value) (*Value, error) {
	if args.Kind == KindNull {
		return nil, posError(expr, "if: bad syntax (missing condition)")
	}
	cond, err := eval(args.Car, env)
	if err != nil {
		return nil, err
	}
	if cond.isTruthy() {
		return tailCallVal(args.Cdr.Car, env), nil
	}
	// else branch (optional)
	if args.Cdr.Cdr.Kind == KindNull {
		return voidVal(), nil
	}
	return tailCallVal(args.Cdr.Cdr.Car, env), nil
}

func evalDefine(args *Value, env *Env, expr *Value) (*Value, error) {
	if args.Kind == KindNull {
		return nil, posError(expr, "define: bad syntax")
	}
	target := args.Car
	if target.Kind == KindSymbol {
		// (define x expr)
		val, err := eval(args.Cdr.Car, env)
		if err != nil {
			return nil, err
		}
		env.set(target.Str, val)
		return voidVal(), nil
	}
	if target.Kind == KindPair {
		// (define (name params...) body...)
		name := target.Car.Str
		params := listToStrings(target.Cdr)
		body := listToSlice(args.Cdr)
		fn := &Value{
			Kind:       KindLambda,
			Params:     params,
			Body:       body,
			ClosureEnv: env,
		}
		env.set(name, fn)
		return voidVal(), nil
	}
	return nil, posError(expr, "define: bad syntax")
}

func evalLambda(args *Value, env *Env) (*Value, error) {
	params := listToStrings(args.Car)
	body := listToSlice(args.Cdr)
	return &Value{
		Kind:       KindLambda,
		Params:     params,
		Body:       body,
		ClosureEnv: env,
	}, nil
}

func applyLambda(fn *Value, args []*Value) (*Value, error) {
	if len(args) != len(fn.Params) {
		return nil, &EvalError{Message: fmt.Sprintf("expected %d arguments, got %d", len(fn.Params), len(args))}
	}
	localEnv := newEnv(fn.ClosureEnv)
	for i, p := range fn.Params {
		localEnv.set(p, args[i])
	}
	// Evaluate all body expressions except the last
	for i := 0; i < len(fn.Body)-1; i++ {
		_, err := eval(fn.Body[i], localEnv)
		if err != nil {
			return nil, err
		}
	}
	// Return tail call for the last body expression
	return tailCallVal(fn.Body[len(fn.Body)-1], localEnv), nil
}

func listToStrings(v *Value) []string {
	var result []string
	cur := v
	for cur.Kind == KindPair {
		result = append(result, cur.Car.Str)
		cur = cur.Cdr
	}
	return result
}

func listToSlice(v *Value) []*Value {
	var result []*Value
	cur := v
	for cur.Kind == KindPair {
		result = append(result, cur.Car)
		cur = cur.Cdr
	}
	return result
}

// Builtin implementations

func builtinAdd(args []*Value) (*Value, error) {
	sum := int64(0)
	for _, a := range args {
		if a.Kind != KindInteger {
			return nil, &EvalError{Message: "+: expected number"}
		}
		sum += a.Int
	}
	return intVal(sum), nil
}

func builtinSub(args []*Value) (*Value, error) {
	if len(args) == 0 {
		return nil, &EvalError{Message: "-: expected at least 1 argument"}
	}
	if args[0].Kind != KindInteger {
		return nil, &EvalError{Message: "-: expected number"}
	}
	if len(args) == 1 {
		return intVal(-args[0].Int), nil
	}
	result := args[0].Int
	for _, a := range args[1:] {
		if a.Kind != KindInteger {
			return nil, &EvalError{Message: "-: expected number"}
		}
		result -= a.Int
	}
	return intVal(result), nil
}

func builtinMulFn(args []*Value) (*Value, error) {
	product := int64(1)
	for _, a := range args {
		if a.Kind != KindInteger {
			return nil, &EvalError{Message: "*: expected number"}
		}
		product *= a.Int
	}
	return intVal(product), nil
}

func builtinDivide(args []*Value) (*Value, error) {
	if len(args) < 2 {
		return nil, &EvalError{Message: "/: expected at least 2 arguments"}
	}
	if args[0].Kind != KindInteger {
		return nil, &EvalError{Message: "/: expected number"}
	}
	result := args[0].Int
	for _, a := range args[1:] {
		if a.Kind != KindInteger {
			return nil, &EvalError{Message: "/: expected number"}
		}
		if a.Int == 0 {
			return nil, &EvalError{Message: "division by zero"}
		}
		result /= a.Int
	}
	return intVal(result), nil
}

func makeCompare(name string, cmp func(int64, int64) bool) BuiltinFunc {
	return func(args []*Value) (*Value, error) {
		if len(args) < 2 {
			return nil, &EvalError{Message: fmt.Sprintf("%s: expected at least 2 arguments", name)}
		}
		for _, a := range args {
			if a.Kind != KindInteger {
				return nil, &EvalError{Message: fmt.Sprintf("%s: expected number", name)}
			}
		}
		for i := 0; i < len(args)-1; i++ {
			if !cmp(args[i].Int, args[i+1].Int) {
				return boolVal(false), nil
			}
		}
		return boolVal(true), nil
	}
}

func builtinNot(args []*Value) (*Value, error) {
	if len(args) != 1 {
		return nil, &EvalError{Message: "not: expected 1 argument"}
	}
	return boolVal(!args[0].isTruthy()), nil
}

func evalLet(args *Value, env *Env) (*Value, error) {
	// Named let: (let name ((var init) ...) body ...)
	if args.Car.Kind == KindSymbol {
		name := args.Car.Str
		bindings := args.Cdr.Car
		body := listToSlice(args.Cdr.Cdr)
		var params []string
		var inits []*Value
		cur := bindings
		for cur.Kind == KindPair {
			b := cur.Car
			params = append(params, b.Car.Str)
			val, err := eval(b.Cdr.Car, env)
			if err != nil {
				return nil, err
			}
			inits = append(inits, val)
			cur = cur.Cdr
		}
		localEnv := newEnv(env)
		fn := &Value{
			Kind:       KindLambda,
			Params:     params,
			Body:       body,
			ClosureEnv: localEnv,
		}
		localEnv.set(name, fn)
		return applyLambda(fn, inits)
	}
	// Regular let
	bindings := args.Car
	body := args.Cdr
	localEnv := newEnv(env)
	cur := bindings
	for cur.Kind == KindPair {
		binding := cur.Car
		name := binding.Car.Str
		val, err := eval(binding.Cdr.Car, env)
		if err != nil {
			return nil, err
		}
		localEnv.set(name, val)
		cur = cur.Cdr
	}
	// Evaluate all but last, then tail call for last
	if body.Kind == KindNull {
		return voidVal(), nil
	}
	bcur := body
	for bcur.Cdr.Kind == KindPair {
		_, err := eval(bcur.Car, localEnv)
		if err != nil {
			return nil, err
		}
		bcur = bcur.Cdr
	}
	return tailCallVal(bcur.Car, localEnv), nil
}

func evalBegin(args *Value, env *Env) (*Value, error) {
	if args.Kind == KindNull {
		return voidVal(), nil
	}
	cur := args
	for cur.Cdr.Kind == KindPair {
		_, err := eval(cur.Car, env)
		if err != nil {
			return nil, err
		}
		cur = cur.Cdr
	}
	// Tail call for the last expression
	return tailCallVal(cur.Car, env), nil
}

func evalCond(args *Value, env *Env) (*Value, error) {
	cur := args
	for cur.Kind == KindPair {
		clause := cur.Car
		test := clause.Car
		if test.Kind == KindSymbol && test.Str == "else" {
			// evaluate body
			return evalBody(clause.Cdr, env)
		}
		cond, err := eval(test, env)
		if err != nil {
			return nil, err
		}
		if cond.isTruthy() {
			return evalBody(clause.Cdr, env)
		}
		cur = cur.Cdr
	}
	return voidVal(), nil
}

func evalBody(body *Value, env *Env) (*Value, error) {
	if body.Kind == KindNull {
		return voidVal(), nil
	}
	cur := body
	for cur.Cdr.Kind == KindPair {
		_, err := eval(cur.Car, env)
		if err != nil {
			return nil, err
		}
		cur = cur.Cdr
	}
	// Tail call for the last expression
	return tailCallVal(cur.Car, env), nil
}

func builtinCons(args []*Value) (*Value, error) {
	if len(args) != 2 {
		return nil, &EvalError{Message: "cons: expected 2 arguments"}
	}
	return pairVal(args[0], args[1]), nil
}

func builtinCar(args []*Value) (*Value, error) {
	if len(args) != 1 || args[0].Kind != KindPair {
		return nil, &EvalError{Message: "car: expected pair"}
	}
	return args[0].Car, nil
}

func builtinCdr(args []*Value) (*Value, error) {
	if len(args) != 1 || args[0].Kind != KindPair {
		return nil, &EvalError{Message: "cdr: expected pair"}
	}
	return args[0].Cdr, nil
}

func builtinNullQ(args []*Value) (*Value, error) {
	if len(args) != 1 {
		return nil, &EvalError{Message: "null?: expected 1 argument"}
	}
	return boolVal(args[0].Kind == KindNull), nil
}

func builtinList(args []*Value) (*Value, error) {
	result := nullVal()
	for i := len(args) - 1; i >= 0; i-- {
		result = pairVal(args[i], result)
	}
	return result, nil
}

func builtinLength(args []*Value) (*Value, error) {
	if len(args) != 1 {
		return nil, &EvalError{Message: "length: expected 1 argument"}
	}
	n := int64(0)
	cur := args[0]
	for cur.Kind == KindPair {
		n++
		cur = cur.Cdr
	}
	return intVal(n), nil
}

func builtinAppend(args []*Value) (*Value, error) {
	if len(args) == 0 {
		return nullVal(), nil
	}
	if len(args) == 1 {
		return args[0], nil
	}
	// Append all lists
	result := args[len(args)-1]
	for i := len(args) - 2; i >= 0; i-- {
		result = appendList(args[i], result)
	}
	return result, nil
}

func appendList(a, b *Value) *Value {
	if a.Kind == KindNull {
		return b
	}
	return pairVal(a.Car, appendList(a.Cdr, b))
}

func makeTypePred(kind ValueKind) BuiltinFunc {
	return func(args []*Value) (*Value, error) {
		if len(args) != 1 {
			return nil, &EvalError{Message: "type predicate: expected 1 argument"}
		}
		return boolVal(args[0].Kind == kind), nil
	}
}

// I/O special forms

func evalDisplay(args *Value, env *Env) (*Value, error) {
	val, err := eval(args.Car, env)
	if err != nil {
		return nil, err
	}
	if out := env.getOutput(); out != nil {
		out.WriteString(val.Display())
	}
	return voidVal(), nil
}

func evalWrite(args *Value, env *Env) (*Value, error) {
	val, err := eval(args.Car, env)
	if err != nil {
		return nil, err
	}
	if out := env.getOutput(); out != nil {
		out.WriteString(val.String())
	}
	return voidVal(), nil
}

func evalNewline(env *Env) (*Value, error) {
	if out := env.getOutput(); out != nil {
		out.WriteByte('\n')
	}
	return voidVal(), nil
}

// String builtins

func builtinStringAppend(args []*Value) (*Value, error) {
	var buf strings.Builder
	for _, a := range args {
		if a.Kind != KindString {
			return nil, &EvalError{Message: "string-append: expected string"}
		}
		buf.WriteString(a.Str)
	}
	return strVal(buf.String()), nil
}

func builtinStringLength(args []*Value) (*Value, error) {
	if len(args) != 1 || args[0].Kind != KindString {
		return nil, &EvalError{Message: "string-length: expected string"}
	}
	return intVal(int64(len([]rune(args[0].Str)))), nil
}

func builtinSubstring(args []*Value) (*Value, error) {
	if len(args) != 3 || args[0].Kind != KindString || args[1].Kind != KindInteger || args[2].Kind != KindInteger {
		return nil, &EvalError{Message: "substring: expected string, start, end"}
	}
	runes := []rune(args[0].Str)
	start := int(args[1].Int)
	end := int(args[2].Int)
	if start < 0 || end > len(runes) || start > end {
		return nil, &EvalError{Message: "substring: index out of range"}
	}
	return strVal(string(runes[start:end])), nil
}

func builtinStringToNumber(args []*Value) (*Value, error) {
	if len(args) != 1 || args[0].Kind != KindString {
		return nil, &EvalError{Message: "string->number: expected string"}
	}
	n, err := strconv.ParseInt(args[0].Str, 10, 64)
	if err != nil {
		return boolVal(false), nil
	}
	return intVal(n), nil
}

func builtinNumberToString(args []*Value) (*Value, error) {
	if len(args) != 1 || args[0].Kind != KindInteger {
		return nil, &EvalError{Message: "number->string: expected number"}
	}
	return strVal(strconv.FormatInt(args[0].Int, 10)), nil
}

func builtinSymbolToString(args []*Value) (*Value, error) {
	if len(args) != 1 || args[0].Kind != KindSymbol {
		return nil, &EvalError{Message: "symbol->string: expected symbol"}
	}
	return strVal(args[0].Str), nil
}

func builtinStringToSymbol(args []*Value) (*Value, error) {
	if len(args) != 1 || args[0].Kind != KindString {
		return nil, &EvalError{Message: "string->symbol: expected string"}
	}
	return symVal(args[0].Str), nil
}

func builtinStringRef(args []*Value) (*Value, error) {
	if len(args) != 2 || args[0].Kind != KindString || args[1].Kind != KindInteger {
		return nil, &EvalError{Message: "string-ref: expected string and index"}
	}
	runes := []rune(args[0].Str)
	idx := int(args[1].Int)
	if idx < 0 || idx >= len(runes) {
		return nil, &EvalError{Message: "string-ref: index out of range"}
	}
	return charVal(runes[idx]), nil
}

func builtinStringSet(args []*Value) (*Value, error) {
	if len(args) != 3 || args[0].Kind != KindString || args[1].Kind != KindInteger || args[2].Kind != KindChar {
		return nil, &EvalError{Message: "string-set!: expected string, index, and char"}
	}
	runes := []rune(args[0].Str)
	idx := int(args[1].Int)
	if idx < 0 || idx >= len(runes) {
		return nil, &EvalError{Message: "string-set!: index out of range"}
	}
	runes[idx] = rune(args[2].Int)
	args[0].Str = string(runes)
	return voidVal(), nil
}

func builtinStringCopy(args []*Value) (*Value, error) {
	if len(args) != 1 || args[0].Kind != KindString {
		return nil, &EvalError{Message: "string-copy: expected string"}
	}
	return strVal(args[0].Str), nil
}

// EvalStr evaluates one or more Scheme expressions and returns the string
// representation of the last result.
func EvalStr(input string) (string, error) {
	exprs, err := parse(input)
	if err != nil {
		return "", &EvalError{Message: err.Error()}
	}
	if len(exprs) == 0 {
		return "", &EvalError{Message: "no expressions"}
	}

	env := makeGlobalEnv()
	env.output = &strings.Builder{}
	var result *Value
	for _, expr := range exprs {
		result, err = eval(expr, env)
		if err != nil {
			return "", err
		}
	}

	if result.Kind == KindVoid {
		return "", nil
	}
	return result.String(), nil
}

// EvalStrWithOutput evaluates Scheme expressions and returns both the result
// string and any captured output from display/write/newline.
func EvalStrWithOutput(input string) (resultStr string, output string, err error) {
	exprs, parseErr := parse(input)
	if parseErr != nil {
		return "", "", &EvalError{Message: parseErr.Error()}
	}
	if len(exprs) == 0 {
		return "", "", &EvalError{Message: "no expressions"}
	}

	env := makeGlobalEnv()
	var buf strings.Builder
	env.output = &buf

	var result *Value
	for _, expr := range exprs {
		result, err = eval(expr, env)
		if err != nil {
			return "", "", err
		}
	}

	if result.Kind == KindVoid {
		return "", buf.String(), nil
	}
	return result.String(), buf.String(), nil
}
