package ming

import (
	"fmt"
	"strconv"
	"strings"
	"sync/atomic"
)

// continuationJump is the panic value used for continuation invocation.
type continuationJump struct {
	value     *Value
	exprIndex int
	contID    int64
}

var contIDCounter int64

func Eval(expr *Expr, env *Env) (*Value, error) {
	for {
		switch expr.Type {
		case ExprInteger:
			return IntegerValue(expr.IntVal), nil
		case ExprBoolean:
			return BooleanValue(expr.BoolVal), nil
		case ExprString:
			return StringValue(expr.StrVal), nil
		case ExprChar:
			return CharValue(rune(expr.IntVal)), nil
		case ExprSymbol:
			val, ok := env.Get(expr.StrVal)
			if !ok {
				return nil, errAtf(expr, "unbound variable: %s", expr.StrVal)
			}
			return val, nil
		case ExprList:
			if len(expr.List) == 0 {
				return nil, errAt(expr, "empty application")
			}
			newExpr, newEnv, val, err, isTail := evalListTCO(expr, env)
			if err != nil {
				return nil, err
			}
			if !isTail {
				return val, nil
			}
			expr = newExpr
			env = newEnv
			continue
		default:
			return nil, errAtf(expr, "unknown expression type")
		}
	}
}

// evalListTCO returns (tailExpr, tailEnv, value, error, isTailCall).
// If isTailCall is true, the caller should loop with tailExpr/tailEnv.
// If isTailCall is false, value is the result.
func evalListTCO(expr *Expr, env *Env) (*Expr, *Env, *Value, error, bool) {
	head := expr.List[0]

	// Handle special forms
	if head.Type == ExprSymbol {
		switch head.StrVal {
		case "and":
			e, ev, v, err := evalAndTCO(expr, env)
			if err != nil {
				return nil, nil, nil, err, false
			}
			if e != nil {
				return e, ev, nil, nil, true
			}
			return nil, nil, v, nil, false
		case "or":
			e, ev, v, err := evalOrTCO(expr, env)
			if err != nil {
				return nil, nil, nil, err, false
			}
			if e != nil {
				return e, ev, nil, nil, true
			}
			return nil, nil, v, nil, false
		case "define":
			v, err := evalDefine(expr, env)
			return nil, nil, v, err, false
		case "set!":
			if len(expr.List) != 3 {
				return nil, nil, nil, errAt(expr, "set!: expected 2 arguments"), false
			}
			sym := expr.List[1]
			if sym.Type != ExprSymbol {
				return nil, nil, nil, errAt(sym, "set!: first argument must be a symbol"), false
			}
			val, err := Eval(expr.List[2], env)
			if err != nil {
				return nil, nil, nil, err, false
			}
			if !env.SetExisting(sym.StrVal, val) {
				return nil, nil, nil, errAtf(expr, "set!: unbound variable '%s'", sym.StrVal), false
			}
			return nil, nil, Void, nil, false
		case "if":
			e, ev, v, err := evalIfTCO(expr, env)
			if err != nil {
				return nil, nil, nil, err, false
			}
			if e != nil {
				return e, ev, nil, nil, true
			}
			return nil, nil, v, nil, false
		case "quote":
			v, err := evalQuote(expr, env)
			return nil, nil, v, err, false
		case "lambda":
			v, err := evalLambda(expr, env)
			return nil, nil, v, err, false
		case "let":
			e, ev, v, err := evalLetTCO(expr, env)
			if err != nil {
				return nil, nil, nil, err, false
			}
			if e != nil {
				return e, ev, nil, nil, true
			}
			return nil, nil, v, nil, false
		case "begin":
			e, ev, v, err := evalBeginTCO(expr.List[1:], env)
			if err != nil {
				return nil, nil, nil, err, false
			}
			if e != nil {
				return e, ev, nil, nil, true
			}
			return nil, nil, v, nil, false
		case "cond":
			e, ev, v, err := evalCondTCO(expr, env)
			if err != nil {
				return nil, nil, nil, err, false
			}
			if e != nil {
				return e, ev, nil, nil, true
			}
			return nil, nil, v, nil, false
		}
	}

	// Function application
	op, err := Eval(head, env)
	if err != nil {
		return nil, nil, nil, err, false
	}

	// Evaluate arguments
	args := make([]*Value, len(expr.List)-1)
	for i, argExpr := range expr.List[1:] {
		val, err := Eval(argExpr, env)
		if err != nil {
			return nil, nil, nil, err, false
		}
		args[i] = val
	}

	return applyFuncTCO(op, args, expr, env)
}

// applyFuncTCO returns a tail-call or a value.
func applyFuncTCO(op *Value, args []*Value, expr *Expr, env *Env) (*Expr, *Env, *Value, error, bool) {
	if op.Type == TypeLambda {
		return applyLambdaTCO(op, args, expr)
	}
	if op.Type == TypeContinuation {
		if len(args) != 1 {
			return nil, nil, nil, errAtf(expr, "continuation: expected 1 argument, got %d", len(args)), false
		}
		op.ContFunc(args[0]) // panics, never returns
		panic("unreachable")
	}
	if op.Type != TypeSymbol || len(op.StrVal) < 10 || op.StrVal[:10] != "__builtin:" {
		return nil, nil, nil, errAtf(expr, "not a procedure"), false
	}
	v, err := applyBuiltin(op.StrVal[10:], args, expr, env)
	return nil, nil, v, err, false
}

func applyBuiltin(name string, args []*Value, expr *Expr, env *Env) (*Value, error) {

	switch name {
	case "+":
		return builtinAdd(args, expr)
	case "-":
		return builtinSub(args, expr)
	case "*":
		return builtinMul(args, expr)
	case "/":
		return builtinDiv(args, expr)
	case "<":
		return builtinCmp(args, expr, func(a, b int64) bool { return a < b })
	case ">":
		return builtinCmp(args, expr, func(a, b int64) bool { return a > b })
	case "=":
		return builtinCmp(args, expr, func(a, b int64) bool { return a == b })
	case "<=":
		return builtinCmp(args, expr, func(a, b int64) bool { return a <= b })
	case ">=":
		return builtinCmp(args, expr, func(a, b int64) bool { return a >= b })
	case "not":
		if len(args) != 1 {
			return nil, errAtf(expr, "not: expected 1 argument, got %d", len(args))
		}
		return BooleanValue(!args[0].IsTruthy()), nil
	case "cons":
		if len(args) != 2 {
			return nil, errAtf(expr, "cons: expected 2 arguments, got %d", len(args))
		}
		return PairValue(args[0], args[1]), nil
	case "car":
		if len(args) != 1 {
			return nil, errAtf(expr, "car: expected 1 argument, got %d", len(args))
		}
		if args[0].Type != TypePair {
			return nil, errAtf(expr, "car: expected pair")
		}
		return args[0].Car, nil
	case "cdr":
		if len(args) != 1 {
			return nil, errAtf(expr, "cdr: expected 1 argument, got %d", len(args))
		}
		if args[0].Type != TypePair {
			return nil, errAtf(expr, "cdr: expected pair")
		}
		return args[0].Cdr, nil
	case "null?":
		if len(args) != 1 {
			return nil, errAtf(expr, "null?: expected 1 argument, got %d", len(args))
		}
		return BooleanValue(args[0].Type == TypeNull), nil
	case "list":
		result := Null
		for i := len(args) - 1; i >= 0; i-- {
			result = PairValue(args[i], result)
		}
		return result, nil
	case "length":
		if len(args) != 1 {
			return nil, errAtf(expr, "length: expected 1 argument, got %d", len(args))
		}
		count := int64(0)
		cur := args[0]
		for cur.Type == TypePair {
			count++
			cur = cur.Cdr
		}
		if cur.Type != TypeNull {
			return nil, errAtf(expr, "length: expected proper list")
		}
		return IntegerValue(count), nil
	case "pair?":
		if len(args) != 1 {
			return nil, errAtf(expr, "pair?: expected 1 argument, got %d", len(args))
		}
		return BooleanValue(args[0].Type == TypePair), nil
	case "number?":
		if len(args) != 1 {
			return nil, errAtf(expr, "number?: expected 1 argument, got %d", len(args))
		}
		return BooleanValue(args[0].Type == TypeInteger), nil
	case "string?":
		if len(args) != 1 {
			return nil, errAtf(expr, "string?: expected 1 argument, got %d", len(args))
		}
		return BooleanValue(args[0].Type == TypeString), nil
	case "boolean?":
		if len(args) != 1 {
			return nil, errAtf(expr, "boolean?: expected 1 argument, got %d", len(args))
		}
		return BooleanValue(args[0].Type == TypeBoolean), nil
	case "symbol?":
		if len(args) != 1 {
			return nil, errAtf(expr, "symbol?: expected 1 argument, got %d", len(args))
		}
		return BooleanValue(args[0].Type == TypeSymbol), nil
	case "char?":
		if len(args) != 1 {
			return nil, errAtf(expr, "char?: expected 1 argument, got %d", len(args))
		}
		return BooleanValue(args[0].Type == TypeChar), nil
	case "display":
		if len(args) != 1 {
			return nil, errAtf(expr, "display: expected 1 argument, got %d", len(args))
		}
		if out := env.Output(); out != nil {
			out.WriteString(args[0].DisplayString())
		}
		return Void, nil
	case "write":
		if len(args) != 1 {
			return nil, errAtf(expr, "write: expected 1 argument, got %d", len(args))
		}
		if out := env.Output(); out != nil {
			out.WriteString(args[0].String())
		}
		return Void, nil
	case "newline":
		if len(args) != 0 {
			return nil, errAtf(expr, "newline: expected 0 arguments, got %d", len(args))
		}
		if out := env.Output(); out != nil {
			out.WriteByte('\n')
		}
		return Void, nil
	case "string-append":
		var buf strings.Builder
		for _, a := range args {
			if a.Type != TypeString {
				return nil, errAtf(expr, "string-append: expected string")
			}
			buf.WriteString(a.StrContent())
		}
		return StringValue(buf.String()), nil
	case "string-length":
		if len(args) != 1 {
			return nil, errAtf(expr, "string-length: expected 1 argument, got %d", len(args))
		}
		if args[0].Type != TypeString {
			return nil, errAtf(expr, "string-length: expected string")
		}
		return IntegerValue(int64(len([]rune(args[0].StrContent())))), nil
	case "substring":
		if len(args) != 3 {
			return nil, errAtf(expr, "substring: expected 3 arguments, got %d", len(args))
		}
		if args[0].Type != TypeString || args[1].Type != TypeInteger || args[2].Type != TypeInteger {
			return nil, errAtf(expr, "substring: invalid argument types")
		}
		runes := []rune(args[0].StrContent())
		start, end := int(args[1].IntVal), int(args[2].IntVal)
		if start < 0 || end < start || end > len(runes) {
			return nil, errAtf(expr, "substring: index out of range")
		}
		return StringValue(string(runes[start:end])), nil
	case "string->number":
		if len(args) != 1 {
			return nil, errAtf(expr, "string->number: expected 1 argument, got %d", len(args))
		}
		if args[0].Type != TypeString {
			return nil, errAtf(expr, "string->number: expected string")
		}
		n, err := strconv.ParseInt(args[0].StrContent(), 10, 64)
		if err != nil {
			return False, nil
		}
		return IntegerValue(n), nil
	case "number->string":
		if len(args) != 1 {
			return nil, errAtf(expr, "number->string: expected 1 argument, got %d", len(args))
		}
		if args[0].Type != TypeInteger {
			return nil, errAtf(expr, "number->string: expected number")
		}
		return StringValue(strconv.FormatInt(args[0].IntVal, 10)), nil
	case "symbol->string":
		if len(args) != 1 {
			return nil, errAtf(expr, "symbol->string: expected 1 argument, got %d", len(args))
		}
		if args[0].Type != TypeSymbol {
			return nil, errAtf(expr, "symbol->string: expected symbol")
		}
		return StringValue(args[0].StrVal), nil
	case "string->symbol":
		if len(args) != 1 {
			return nil, errAtf(expr, "string->symbol: expected 1 argument, got %d", len(args))
		}
		if args[0].Type != TypeString {
			return nil, errAtf(expr, "string->symbol: expected string")
		}
		return SymbolValue(args[0].StrContent()), nil
	case "string-ref":
		if len(args) != 2 {
			return nil, errAtf(expr, "string-ref: expected 2 arguments, got %d", len(args))
		}
		if args[0].Type != TypeString || args[1].Type != TypeInteger {
			return nil, errAtf(expr, "string-ref: invalid argument types")
		}
		runes := []rune(args[0].StrContent())
		idx := int(args[1].IntVal)
		if idx < 0 || idx >= len(runes) {
			return nil, errAtf(expr, "string-ref: index out of range")
		}
		return CharValue(runes[idx]), nil
	case "apply":
		return builtinApply(args, expr, env)
	case "call/cc", "call-with-current-continuation":
		return builtinCallCC(args, expr, env)
	case "string-set!":
		if len(args) != 3 {
			return nil, errAtf(expr, "string-set!: expected 3 arguments, got %d", len(args))
		}
		if args[0].Type != TypeString {
			return nil, errAtf(expr, "string-set!: expected string")
		}
		if args[1].Type != TypeInteger {
			return nil, errAtf(expr, "string-set!: expected integer index")
		}
		if args[2].Type != TypeChar {
			return nil, errAtf(expr, "string-set!: expected char")
		}
		if args[0].Runes == nil {
			return nil, errAtf(expr, "string-set!: string is immutable")
		}
		idx := int(args[1].IntVal)
		if idx < 0 || idx >= len(args[0].Runes) {
			return nil, errAtf(expr, "string-set!: index out of range")
		}
		args[0].Runes[idx] = rune(args[2].IntVal)
		return Void, nil
	case "string-copy":
		if len(args) != 1 {
			return nil, errAtf(expr, "string-copy: expected 1 argument, got %d", len(args))
		}
		if args[0].Type != TypeString {
			return nil, errAtf(expr, "string-copy: expected string")
		}
		src := []rune(args[0].StrContent())
		cp := make([]rune, len(src))
		copy(cp, src)
		return &Value{Type: TypeString, Runes: cp}, nil
	default:
		return nil, errAtf(expr, "unknown procedure: %s", name)
	}
}

func builtinApply(args []*Value, expr *Expr, env *Env) (*Value, error) {
	if len(args) < 2 {
		return nil, errAtf(expr, "apply: expected at least 2 arguments, got %d", len(args))
	}
	fn := args[0]
	// Last argument must be a list; prefix args are prepended
	lastArg := args[len(args)-1]
	var finalArgs []*Value
	// Collect prefix args (between fn and the last arg)
	for _, a := range args[1 : len(args)-1] {
		finalArgs = append(finalArgs, a)
	}
	// Unpack the last argument (a list)
	cur := lastArg
	for cur.Type == TypePair {
		finalArgs = append(finalArgs, cur.Car)
		cur = cur.Cdr
	}
	if cur.Type != TypeNull {
		return nil, errAtf(expr, "apply: last argument must be a proper list")
	}
	// Apply the function
	tailExpr, tailEnv, val, err, isTail := applyFuncTCO(fn, finalArgs, expr, env)
	if err != nil {
		return nil, err
	}
	if isTail {
		return Eval(tailExpr, tailEnv)
	}
	return val, nil
}

func builtinCallCC(args []*Value, expr *Expr, env *Env) (*Value, error) {
	if len(args) != 1 {
		return nil, errAtf(expr, "call/cc: expected 1 argument, got %d", len(args))
	}
	proc := args[0]

	// If resuming a saved continuation, return the resume value immediately
	if ctx := env.getEvalCtx(); ctx != nil && ctx.resuming {
		ctx.resuming = false
		v := ctx.resumeValue
		ctx.resumeValue = nil
		return v, nil
	}

	// Capture the current top-level expression index for re-entrant support
	exprIdx := 0
	if ctx := env.getEvalCtx(); ctx != nil {
		exprIdx = ctx.currentIndex
	}

	contID := atomic.AddInt64(&contIDCounter, 1)

	cont := &Value{
		Type: TypeContinuation,
		ContFunc: func(val *Value) {
			panic(continuationJump{value: val, exprIndex: exprIdx, contID: contID})
		},
	}

	// Call proc with the continuation; recover in-extent escapes
	var result *Value
	var evalErr error

	func() {
		defer func() {
			if r := recover(); r != nil {
				if j, ok := r.(continuationJump); ok && j.contID == contID {
					result = j.value
				} else {
					panic(r) // re-panic for other continuations or real panics
				}
			}
		}()

		te, tenv, v, err, isTail := applyFuncTCO(proc, []*Value{cont}, expr, env)
		if err != nil {
			evalErr = err
			return
		}
		if isTail {
			result, evalErr = Eval(te, tenv)
		} else {
			result = v
		}
	}()

	return result, evalErr
}

func builtinAdd(args []*Value, expr *Expr) (*Value, error) {
	var sum int64
	for _, a := range args {
		if a.Type != TypeInteger {
			return nil, errAtf(expr, "+: expected number")
		}
		sum += a.IntVal
	}
	return IntegerValue(sum), nil
}

func builtinSub(args []*Value, expr *Expr) (*Value, error) {
	if len(args) == 0 {
		return nil, errAtf(expr, "-: expected at least 1 argument")
	}
	for _, a := range args {
		if a.Type != TypeInteger {
			return nil, errAtf(expr, "-: expected number")
		}
	}
	if len(args) == 1 {
		return IntegerValue(-args[0].IntVal), nil
	}
	result := args[0].IntVal
	for _, a := range args[1:] {
		result -= a.IntVal
	}
	return IntegerValue(result), nil
}

func builtinMul(args []*Value, expr *Expr) (*Value, error) {
	var product int64 = 1
	for _, a := range args {
		if a.Type != TypeInteger {
			return nil, errAtf(expr, "*: expected number")
		}
		product *= a.IntVal
	}
	return IntegerValue(product), nil
}

func builtinDiv(args []*Value, expr *Expr) (*Value, error) {
	if len(args) < 2 {
		return nil, errAtf(expr, "/: expected at least 2 arguments")
	}
	for _, a := range args {
		if a.Type != TypeInteger {
			return nil, errAtf(expr, "/: expected number")
		}
	}
	result := args[0].IntVal
	for _, a := range args[1:] {
		if a.IntVal == 0 {
			return nil, errAtf(expr, "/: division by zero")
		}
		result /= a.IntVal
	}
	return IntegerValue(result), nil
}

func builtinCmp(args []*Value, expr *Expr, cmp func(int64, int64) bool) (*Value, error) {
	if len(args) < 2 {
		return nil, errAtf(expr, "comparison: expected at least 2 arguments")
	}
	for _, a := range args {
		if a.Type != TypeInteger {
			return nil, errAtf(expr, "comparison: expected number")
		}
	}
	for i := 0; i < len(args)-1; i++ {
		if !cmp(args[i].IntVal, args[i+1].IntVal) {
			return False, nil
		}
	}
	return True, nil
}

// evalAndTCO returns (tailExpr, tailEnv, value, error). tailExpr != nil means tail call.
func evalAndTCO(expr *Expr, env *Env) (*Expr, *Env, *Value, error) {
	if len(expr.List) == 1 {
		return nil, nil, True, nil
	}
	// Evaluate all but the last
	for _, e := range expr.List[1 : len(expr.List)-1] {
		result, err := Eval(e, env)
		if err != nil {
			return nil, nil, nil, err
		}
		if !result.IsTruthy() {
			return nil, nil, result, nil
		}
	}
	// Last expression is in tail position
	return expr.List[len(expr.List)-1], env, nil, nil
}

// evalOrTCO returns (tailExpr, tailEnv, value, error). tailExpr != nil means tail call.
func evalOrTCO(expr *Expr, env *Env) (*Expr, *Env, *Value, error) {
	if len(expr.List) == 1 {
		return nil, nil, False, nil
	}
	// Evaluate all but the last
	for _, e := range expr.List[1 : len(expr.List)-1] {
		result, err := Eval(e, env)
		if err != nil {
			return nil, nil, nil, err
		}
		if result.IsTruthy() {
			return nil, nil, result, nil
		}
	}
	// Last expression is in tail position
	return expr.List[len(expr.List)-1], env, nil, nil
}

func evalDefine(expr *Expr, env *Env) (*Value, error) {
	if len(expr.List) < 3 {
		return nil, errAt(expr, "define: expected at least 2 arguments")
	}
	target := expr.List[1]
	if target.Type == ExprSymbol {
		// (define x expr)
		val, err := Eval(expr.List[2], env)
		if err != nil {
			return nil, err
		}
		env.Set(target.StrVal, val)
		return Void, nil
	}
	if target.Type == ExprList && len(target.List) >= 1 && target.List[0].Type == ExprSymbol {
		// (define (f params...) body...)
		name := target.List[0].StrVal
		paramExpr := &Expr{Type: ExprList, List: target.List[1:], Line: target.Line, Col: target.Col}
		params, restParam, err := parseLambdaParams(paramExpr)
		if err != nil {
			return nil, err
		}
		lambda := &Value{
			Type:      TypeLambda,
			Params:    params,
			RestParam: restParam,
			Body:      expr.List[2:],
			Closure:   env,
		}
		env.Set(name, lambda)
		return Void, nil
	}
	return nil, errAt(target, "define: invalid syntax")
}

func evalIfTCO(expr *Expr, env *Env) (*Expr, *Env, *Value, error) {
	if len(expr.List) < 3 || len(expr.List) > 4 {
		return nil, nil, nil, errAt(expr, "if: expected 2 or 3 arguments")
	}
	cond, err := Eval(expr.List[1], env)
	if err != nil {
		return nil, nil, nil, err
	}
	if cond.IsTruthy() {
		return expr.List[2], env, nil, nil
	}
	if len(expr.List) == 4 {
		return expr.List[3], env, nil, nil
	}
	return nil, nil, Void, nil
}

func evalQuote(expr *Expr, env *Env) (*Value, error) {
	if len(expr.List) != 2 {
		return nil, errAt(expr, "quote: expected 1 argument")
	}
	return exprToValue(expr.List[1])
}

func exprToValue(expr *Expr) (*Value, error) {
	switch expr.Type {
	case ExprInteger:
		return IntegerValue(expr.IntVal), nil
	case ExprBoolean:
		return BooleanValue(expr.BoolVal), nil
	case ExprString:
		return StringValue(expr.StrVal), nil
	case ExprChar:
		return CharValue(rune(expr.IntVal)), nil
	case ExprSymbol:
		return SymbolValue(expr.StrVal), nil
	case ExprList:
		if len(expr.List) == 0 {
			return Null, nil
		}
		// Build a proper list from the elements
		result := Null
		for i := len(expr.List) - 1; i >= 0; i-- {
			val, err := exprToValue(expr.List[i])
			if err != nil {
				return nil, err
			}
			result = PairValue(val, result)
		}
		return result, nil
	default:
		return nil, fmt.Errorf("cannot quote expression")
	}
}

func parseLambdaParams(paramExpr *Expr) ([]string, string, error) {
	if paramExpr.Type == ExprSymbol {
		// (lambda args body) — all args collected as rest
		return nil, paramExpr.StrVal, nil
	}
	if paramExpr.Type != ExprList {
		return nil, "", errAt(paramExpr, "lambda: parameters must be a list")
	}
	// Check for dot notation: (x y . rest)
	var params []string
	var restParam string
	for i, p := range paramExpr.List {
		if p.Type != ExprSymbol {
			return nil, "", errAt(p, "lambda: parameter must be a symbol")
		}
		if p.StrVal == "." {
			if i+2 != len(paramExpr.List) {
				return nil, "", errAt(p, "lambda: invalid dot in parameter list")
			}
			rest := paramExpr.List[i+1]
			if rest.Type != ExprSymbol {
				return nil, "", errAt(rest, "lambda: rest parameter must be a symbol")
			}
			restParam = rest.StrVal
			break
		}
		params = append(params, p.StrVal)
	}
	if restParam == "" {
		params = make([]string, len(paramExpr.List))
		for i, p := range paramExpr.List {
			params[i] = p.StrVal
		}
	}
	return params, restParam, nil
}

func evalLambda(expr *Expr, env *Env) (*Value, error) {
	if len(expr.List) < 3 {
		return nil, errAt(expr, "lambda: expected at least 2 arguments")
	}
	params, restParam, err := parseLambdaParams(expr.List[1])
	if err != nil {
		return nil, err
	}
	return &Value{
		Type:      TypeLambda,
		Params:    params,
		RestParam: restParam,
		Body:      expr.List[2:],
		Closure:   env,
	}, nil
}

// applyLambdaTCO sets up the lambda env and returns a tail call to its last body expr.
func applyLambdaTCO(fn *Value, args []*Value, expr *Expr) (*Expr, *Env, *Value, error, bool) {
	if fn.RestParam != "" {
		if len(args) < len(fn.Params) {
			return nil, nil, nil, errAtf(expr, "expected at least %d arguments, got %d", len(fn.Params), len(args)), false
		}
	} else {
		if len(args) != len(fn.Params) {
			return nil, nil, nil, errAtf(expr, "expected %d arguments, got %d", len(fn.Params), len(args)), false
		}
	}
	localEnv := NewEnv(fn.Closure)
	for i, param := range fn.Params {
		localEnv.Set(param, args[i])
	}
	if fn.RestParam != "" {
		rest := Null
		for i := len(args) - 1; i >= len(fn.Params); i-- {
			rest = PairValue(args[i], rest)
		}
		localEnv.Set(fn.RestParam, rest)
	}
	// Evaluate all but last body expression
	for _, bodyExpr := range fn.Body[:len(fn.Body)-1] {
		_, err := Eval(bodyExpr, localEnv)
		if err != nil {
			return nil, nil, nil, err, false
		}
	}
	// Tail call to last body expression
	return fn.Body[len(fn.Body)-1], localEnv, nil, nil, true
}

func evalLetTCO(expr *Expr, env *Env) (*Expr, *Env, *Value, error) {
	if len(expr.List) < 3 {
		return nil, nil, nil, errAt(expr, "let: expected at least 2 arguments")
	}

	// Named let: (let name ((var init) ...) body...)
	if expr.List[1].Type == ExprSymbol {
		return evalNamedLetTCO(expr, env)
	}

	bindings := expr.List[1]
	if bindings.Type != ExprList {
		return nil, nil, nil, errAt(bindings, "let: bindings must be a list")
	}
	localEnv := NewEnv(env)
	for _, b := range bindings.List {
		if b.Type != ExprList || len(b.List) != 2 {
			return nil, nil, nil, errAt(b, "let: invalid binding")
		}
		if b.List[0].Type != ExprSymbol {
			return nil, nil, nil, errAt(b.List[0], "let: binding name must be a symbol")
		}
		val, err := Eval(b.List[1], env)
		if err != nil {
			return nil, nil, nil, err
		}
		localEnv.Set(b.List[0].StrVal, val)
	}
	body := expr.List[2:]
	// Evaluate all but last
	for _, bodyExpr := range body[:len(body)-1] {
		_, err := Eval(bodyExpr, localEnv)
		if err != nil {
			return nil, nil, nil, err
		}
	}
	// Tail call to last body expression
	return body[len(body)-1], localEnv, nil, nil
}

func evalNamedLetTCO(expr *Expr, env *Env) (*Expr, *Env, *Value, error) {
	name := expr.List[1].StrVal
	bindingsExpr := expr.List[2]
	if bindingsExpr.Type != ExprList {
		return nil, nil, nil, errAt(bindingsExpr, "let: bindings must be a list")
	}
	// Extract parameter names and initial values
	params := make([]string, len(bindingsExpr.List))
	initVals := make([]*Value, len(bindingsExpr.List))
	for i, b := range bindingsExpr.List {
		if b.Type != ExprList || len(b.List) != 2 {
			return nil, nil, nil, errAt(b, "let: invalid binding")
		}
		if b.List[0].Type != ExprSymbol {
			return nil, nil, nil, errAt(b.List[0], "let: binding name must be a symbol")
		}
		params[i] = b.List[0].StrVal
		val, err := Eval(b.List[1], env)
		if err != nil {
			return nil, nil, nil, err
		}
		initVals[i] = val
	}
	// Create a lambda for the named let
	body := expr.List[3:]
	lambda := &Value{
		Type:    TypeLambda,
		Params:  params,
		Body:    body,
		Closure: env,
	}
	// Bind the name in the lambda's closure so it can recurse
	localEnv := NewEnv(env)
	localEnv.Set(name, lambda)
	lambda.Closure = localEnv

	// Now apply it with initial values
	callEnv := NewEnv(localEnv)
	for i, p := range params {
		callEnv.Set(p, initVals[i])
	}
	// Evaluate all but last body
	for _, bodyExpr := range body[:len(body)-1] {
		_, err := Eval(bodyExpr, callEnv)
		if err != nil {
			return nil, nil, nil, err
		}
	}
	return body[len(body)-1], callEnv, nil, nil
}

// evalBeginTCO takes a slice of body expressions (not including the 'begin' keyword).
func evalBeginTCO(body []*Expr, env *Env) (*Expr, *Env, *Value, error) {
	if len(body) == 0 {
		return nil, nil, Void, nil
	}
	for _, e := range body[:len(body)-1] {
		_, err := Eval(e, env)
		if err != nil {
			return nil, nil, nil, err
		}
	}
	return body[len(body)-1], env, nil, nil
}

func evalCondTCO(expr *Expr, env *Env) (*Expr, *Env, *Value, error) {
	for _, clause := range expr.List[1:] {
		if clause.Type != ExprList || len(clause.List) < 2 {
			return nil, nil, nil, errAt(clause, "cond: invalid clause")
		}
		// Check for else clause
		if clause.List[0].Type == ExprSymbol && clause.List[0].StrVal == "else" {
			body := clause.List[1:]
			for _, e := range body[:len(body)-1] {
				_, err := Eval(e, env)
				if err != nil {
					return nil, nil, nil, err
				}
			}
			return body[len(body)-1], env, nil, nil
		}
		test, err := Eval(clause.List[0], env)
		if err != nil {
			return nil, nil, nil, err
		}
		if test.IsTruthy() {
			body := clause.List[1:]
			for _, e := range body[:len(body)-1] {
				_, err := Eval(e, env)
				if err != nil {
					return nil, nil, nil, err
				}
			}
			return body[len(body)-1], env, nil, nil
		}
	}
	return nil, nil, Void, nil
}

func makeDefaultEnv() *Env {
	env := NewEnv(nil)
	builtins := []string{"+", "-", "*", "/", "<", ">", "=", "<=", ">=", "not",
		"cons", "car", "cdr", "null?", "list", "length",
		"pair?", "number?", "string?", "boolean?", "symbol?", "char?",
		"display", "write", "newline",
		"string-append", "string-length", "substring",
		"string->number", "number->string",
		"symbol->string", "string->symbol",
		"string-ref",
		"string-set!", "string-copy",
		"apply",
		"call/cc", "call-with-current-continuation"}
	for _, name := range builtins {
		env.Set(name, &Value{Type: TypeSymbol, StrVal: fmt.Sprintf("__builtin:%s", name)})
	}
	return env
}
