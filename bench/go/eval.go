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
	callExpr  *Expr // the call/cc expression that created this continuation
}

// schemeException is the panic value used for raise.
type schemeException struct {
	value *Value
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
		case "letrec":
			e, ev, v, err := evalLetrecTCO(expr, env, false)
			if err != nil {
				return nil, nil, nil, err, false
			}
			if e != nil {
				return e, ev, nil, nil, true
			}
			return nil, nil, v, nil, false
		case "letrec*":
			e, ev, v, err := evalLetrecTCO(expr, env, true)
			if err != nil {
				return nil, nil, nil, err, false
			}
			if e != nil {
				return e, ev, nil, nil, true
			}
			return nil, nil, v, nil, false
		case "case":
			e, ev, v, err := evalCaseTCO(expr, env)
			if err != nil {
				return nil, nil, nil, err, false
			}
			if e != nil {
				return e, ev, nil, nil, true
			}
			return nil, nil, v, nil, false
		case "define-syntax":
			v, err := evalDefineSyntax(expr, env)
			return nil, nil, v, err, false
		case "guard":
			v, err := evalGuard(expr, env)
			return nil, nil, v, err, false
		}

		// Check for macro application
		if val, ok := env.Get(head.StrVal); ok && val.Type == TypeMacro {
			expanded, err := val.Macro.expandMacro(expr, env)
			if err != nil {
				return nil, nil, nil, err, false
			}
			return expanded, env, nil, nil, true
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
	case "reverse":
		if len(args) != 1 {
			return nil, errAtf(expr, "reverse: expected 1 argument, got %d", len(args))
		}
		result := &Value{Type: TypeNull}
		cur := args[0]
		for cur.Type == TypePair {
			result = PairValue(cur.Car, result)
			cur = cur.Cdr
		}
		if cur.Type != TypeNull {
			return nil, errAtf(expr, "reverse: expected proper list")
		}
		return result, nil
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
	case "dynamic-wind":
		return builtinDynamicWind(args, expr, env)
	case "raise":
		return builtinRaise(args, expr)
	case "with-exception-handler":
		return builtinWithExceptionHandler(args, expr, env)
	case "equal?":
		if len(args) != 2 {
			return nil, errAtf(expr, "equal?: expected 2 arguments, got %d", len(args))
		}
		return BooleanValue(schemeEqual(args[0], args[1])), nil
	case "eq?", "eqv?":
		if len(args) != 2 {
			return nil, errAtf(expr, "%s: expected 2 arguments, got %d", name, len(args))
		}
		return BooleanValue(schemeEq(args[0], args[1])), nil
	case "abs":
		if len(args) != 1 || args[0].Type != TypeInteger {
			return nil, errAtf(expr, "abs: expected 1 integer argument")
		}
		v := args[0].IntVal
		if v < 0 {
			v = -v
		}
		return IntegerValue(v), nil
	case "modulo":
		if len(args) != 2 || args[0].Type != TypeInteger || args[1].Type != TypeInteger {
			return nil, errAtf(expr, "modulo: expected 2 integer arguments")
		}
		if args[1].IntVal == 0 {
			return nil, errAtf(expr, "modulo: division by zero")
		}
		a, b := args[0].IntVal, args[1].IntVal
		r := a % b
		if r != 0 && (r > 0) != (b > 0) {
			r += b
		}
		return IntegerValue(r), nil
	case "remainder":
		if len(args) != 2 || args[0].Type != TypeInteger || args[1].Type != TypeInteger {
			return nil, errAtf(expr, "remainder: expected 2 integer arguments")
		}
		if args[1].IntVal == 0 {
			return nil, errAtf(expr, "remainder: division by zero")
		}
		return IntegerValue(args[0].IntVal % args[1].IntVal), nil
	case "quotient":
		if len(args) != 2 || args[0].Type != TypeInteger || args[1].Type != TypeInteger {
			return nil, errAtf(expr, "quotient: expected 2 integer arguments")
		}
		if args[1].IntVal == 0 {
			return nil, errAtf(expr, "quotient: division by zero")
		}
		return IntegerValue(args[0].IntVal / args[1].IntVal), nil
	case "min":
		if len(args) == 0 {
			return nil, errAtf(expr, "min: expected at least 1 argument")
		}
		m := args[0]
		if m.Type != TypeInteger {
			return nil, errAtf(expr, "min: expected number")
		}
		for _, a := range args[1:] {
			if a.Type != TypeInteger {
				return nil, errAtf(expr, "min: expected number")
			}
			if a.IntVal < m.IntVal {
				m = a
			}
		}
		return m, nil
	case "max":
		if len(args) == 0 {
			return nil, errAtf(expr, "max: expected at least 1 argument")
		}
		m := args[0]
		if m.Type != TypeInteger {
			return nil, errAtf(expr, "max: expected number")
		}
		for _, a := range args[1:] {
			if a.Type != TypeInteger {
				return nil, errAtf(expr, "max: expected number")
			}
			if a.IntVal > m.IntVal {
				m = a
			}
		}
		return m, nil
	case "expt":
		if len(args) != 2 || args[0].Type != TypeInteger || args[1].Type != TypeInteger {
			return nil, errAtf(expr, "expt: expected 2 integer arguments")
		}
		base, exp := args[0].IntVal, args[1].IntVal
		result := int64(1)
		for i := int64(0); i < exp; i++ {
			result *= base
		}
		return IntegerValue(result), nil
	case "zero?":
		if len(args) != 1 || args[0].Type != TypeInteger {
			return nil, errAtf(expr, "zero?: expected 1 integer argument")
		}
		return BooleanValue(args[0].IntVal == 0), nil
	case "positive?":
		if len(args) != 1 || args[0].Type != TypeInteger {
			return nil, errAtf(expr, "positive?: expected 1 integer argument")
		}
		return BooleanValue(args[0].IntVal > 0), nil
	case "negative?":
		if len(args) != 1 || args[0].Type != TypeInteger {
			return nil, errAtf(expr, "negative?: expected 1 integer argument")
		}
		return BooleanValue(args[0].IntVal < 0), nil
	case "odd?":
		if len(args) != 1 || args[0].Type != TypeInteger {
			return nil, errAtf(expr, "odd?: expected 1 integer argument")
		}
		return BooleanValue(args[0].IntVal%2 != 0), nil
	case "even?":
		if len(args) != 1 || args[0].Type != TypeInteger {
			return nil, errAtf(expr, "even?: expected 1 integer argument")
		}
		return BooleanValue(args[0].IntVal%2 == 0), nil
	case "list-ref":
		if len(args) != 2 || args[1].Type != TypeInteger {
			return nil, errAtf(expr, "list-ref: expected list and integer")
		}
		cur := args[0]
		idx := args[1].IntVal
		for i := int64(0); i < idx; i++ {
			if cur.Type != TypePair {
				return nil, errAtf(expr, "list-ref: index out of range")
			}
			cur = cur.Cdr
		}
		if cur.Type != TypePair {
			return nil, errAtf(expr, "list-ref: index out of range")
		}
		return cur.Car, nil
	case "list-tail":
		if len(args) != 2 || args[1].Type != TypeInteger {
			return nil, errAtf(expr, "list-tail: expected list and integer")
		}
		cur := args[0]
		idx := args[1].IntVal
		for i := int64(0); i < idx; i++ {
			if cur.Type != TypePair {
				return nil, errAtf(expr, "list-tail: index out of range")
			}
			cur = cur.Cdr
		}
		return cur, nil
	case "list?":
		if len(args) != 1 {
			return nil, errAtf(expr, "list?: expected 1 argument")
		}
		cur := args[0]
		for cur.Type == TypePair {
			cur = cur.Cdr
		}
		return BooleanValue(cur.Type == TypeNull), nil
	case "assoc":
		if len(args) != 2 {
			return nil, errAtf(expr, "assoc: expected 2 arguments")
		}
		key := args[0]
		cur := args[1]
		for cur.Type == TypePair {
			entry := cur.Car
			if entry.Type == TypePair && schemeEqual(entry.Car, key) {
				return entry, nil
			}
			cur = cur.Cdr
		}
		return False, nil
	case "map":
		return builtinMap(args, expr, env)
	case "for-each":
		return builtinForEach(args, expr, env)
	case "char-alphabetic?":
		if len(args) != 1 || args[0].Type != TypeChar {
			return nil, errAtf(expr, "char-alphabetic?: expected 1 char argument")
		}
		c := rune(args[0].IntVal)
		return BooleanValue((c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z')), nil
	case "char-numeric?":
		if len(args) != 1 || args[0].Type != TypeChar {
			return nil, errAtf(expr, "char-numeric?: expected 1 char argument")
		}
		c := rune(args[0].IntVal)
		return BooleanValue(c >= '0' && c <= '9'), nil
	case "char-upcase":
		if len(args) != 1 || args[0].Type != TypeChar {
			return nil, errAtf(expr, "char-upcase: expected 1 char argument")
		}
		c := rune(args[0].IntVal)
		if c >= 'a' && c <= 'z' {
			c = c - 'a' + 'A'
		}
		return CharValue(c), nil
	case "char-downcase":
		if len(args) != 1 || args[0].Type != TypeChar {
			return nil, errAtf(expr, "char-downcase: expected 1 char argument")
		}
		c := rune(args[0].IntVal)
		if c >= 'A' && c <= 'Z' {
			c = c - 'A' + 'a'
		}
		return CharValue(c), nil
	case "char=?":
		if len(args) != 2 || args[0].Type != TypeChar || args[1].Type != TypeChar {
			return nil, errAtf(expr, "char=?: expected 2 char arguments")
		}
		return BooleanValue(args[0].IntVal == args[1].IntVal), nil
	case "char<?":
		if len(args) != 2 || args[0].Type != TypeChar || args[1].Type != TypeChar {
			return nil, errAtf(expr, "char<?: expected 2 char arguments")
		}
		return BooleanValue(args[0].IntVal < args[1].IntVal), nil
	case "string=?":
		if len(args) != 2 || args[0].Type != TypeString || args[1].Type != TypeString {
			return nil, errAtf(expr, "string=?: expected 2 string arguments")
		}
		return BooleanValue(args[0].StrContent() == args[1].StrContent()), nil
	case "string<?":
		if len(args) != 2 || args[0].Type != TypeString || args[1].Type != TypeString {
			return nil, errAtf(expr, "string<?: expected 2 string arguments")
		}
		return BooleanValue(args[0].StrContent() < args[1].StrContent()), nil
	case "string-ci=?":
		if len(args) != 2 || args[0].Type != TypeString || args[1].Type != TypeString {
			return nil, errAtf(expr, "string-ci=?: expected 2 string arguments")
		}
		return BooleanValue(strings.EqualFold(args[0].StrContent(), args[1].StrContent())), nil
	case "string-upcase":
		if len(args) != 1 || args[0].Type != TypeString {
			return nil, errAtf(expr, "string-upcase: expected 1 string argument")
		}
		return StringValue(strings.ToUpper(args[0].StrContent())), nil
	case "string-downcase":
		if len(args) != 1 || args[0].Type != TypeString {
			return nil, errAtf(expr, "string-downcase: expected 1 string argument")
		}
		return StringValue(strings.ToLower(args[0].StrContent())), nil
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
	case "string->list":
		if len(args) != 1 || args[0].Type != TypeString {
			return nil, errAtf(expr, "string->list: expected 1 string argument")
		}
		runes := []rune(args[0].StrContent())
		result := Null
		for i := len(runes) - 1; i >= 0; i-- {
			result = PairValue(CharValue(runes[i]), result)
		}
		return result, nil
	case "list->string":
		if len(args) != 1 {
			return nil, errAtf(expr, "list->string: expected 1 argument")
		}
		var runes []rune
		cur := args[0]
		for cur != Null {
			if cur.Type != TypePair || cur.Car.Type != TypeChar {
				return nil, errAtf(expr, "list->string: expected list of chars")
			}
			runes = append(runes, rune(cur.Car.IntVal))
			cur = cur.Cdr
		}
		return StringValue(string(runes)), nil
	case "char->integer":
		if len(args) != 1 || args[0].Type != TypeChar {
			return nil, errAtf(expr, "char->integer: expected 1 char argument")
		}
		return IntegerValue(args[0].IntVal), nil
	case "integer->char":
		if len(args) != 1 || args[0].Type != TypeInteger {
			return nil, errAtf(expr, "integer->char: expected 1 integer argument")
		}
		return CharValue(rune(args[0].IntVal)), nil
	case "vector":
		elems := make([]*Value, len(args))
		copy(elems, args)
		return &Value{Type: TypeVector, VecElems: elems}, nil
	case "make-vector":
		if len(args) < 1 || len(args) > 2 || args[0].Type != TypeInteger {
			return nil, errAtf(expr, "make-vector: expected integer length and optional fill")
		}
		n := int(args[0].IntVal)
		fill := IntegerValue(0)
		if len(args) == 2 {
			fill = args[1]
		}
		elems := make([]*Value, n)
		for i := range elems {
			elems[i] = fill
		}
		return &Value{Type: TypeVector, VecElems: elems}, nil
	case "vector-ref":
		if len(args) != 2 || args[0].Type != TypeVector || args[1].Type != TypeInteger {
			return nil, errAtf(expr, "vector-ref: expected vector and integer")
		}
		idx := int(args[1].IntVal)
		if idx < 0 || idx >= len(args[0].VecElems) {
			return nil, errAtf(expr, "vector-ref: index out of range")
		}
		return args[0].VecElems[idx], nil
	case "vector-set!":
		if len(args) != 3 || args[0].Type != TypeVector || args[1].Type != TypeInteger {
			return nil, errAtf(expr, "vector-set!: expected vector, integer, and value")
		}
		idx := int(args[1].IntVal)
		if idx < 0 || idx >= len(args[0].VecElems) {
			return nil, errAtf(expr, "vector-set!: index out of range")
		}
		args[0].VecElems[idx] = args[2]
		return Void, nil
	case "vector-length":
		if len(args) != 1 || args[0].Type != TypeVector {
			return nil, errAtf(expr, "vector-length: expected vector")
		}
		return IntegerValue(int64(len(args[0].VecElems))), nil
	case "vector?":
		if len(args) != 1 {
			return nil, errAtf(expr, "vector?: expected 1 argument")
		}
		return BooleanValue(args[0].Type == TypeVector), nil
	case "vector->list":
		if len(args) != 1 || args[0].Type != TypeVector {
			return nil, errAtf(expr, "vector->list: expected vector")
		}
		result := Null
		for i := len(args[0].VecElems) - 1; i >= 0; i-- {
			result = PairValue(args[0].VecElems[i], result)
		}
		return result, nil
	case "list->vector":
		if len(args) != 1 {
			return nil, errAtf(expr, "list->vector: expected 1 argument")
		}
		var elems []*Value
		cur := args[0]
		for cur.Type == TypePair {
			elems = append(elems, cur.Car)
			cur = cur.Cdr
		}
		if cur.Type != TypeNull {
			return nil, errAtf(expr, "list->vector: expected proper list")
		}
		return &Value{Type: TypeVector, VecElems: elems}, nil
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
	// Only consume if this is the exact call/cc expression that created the continuation
	if ctx := env.getEvalCtx(); ctx != nil && ctx.resuming && ctx.resumeExpr == expr {
		ctx.resuming = false
		v := ctx.resumeValue
		ctx.resumeValue = nil
		ctx.resumeExpr = nil
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
			panic(continuationJump{value: val, exprIndex: exprIdx, contID: contID, callExpr: expr})
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

func builtinDynamicWind(args []*Value, expr *Expr, env *Env) (*Value, error) {
	if len(args) != 3 {
		return nil, errAtf(expr, "dynamic-wind: expected 3 arguments, got %d", len(args))
	}
	inThunk := args[0]
	bodyThunk := args[1]
	outThunk := args[2]

	callThunk := func(thunk *Value) (*Value, error) {
		te, tenv, v, err, isTail := applyFuncTCO(thunk, []*Value{}, expr, env)
		if err != nil {
			return nil, err
		}
		if isTail {
			return Eval(te, tenv)
		}
		return v, nil
	}

	// Call in-thunk
	if _, err := callThunk(inThunk); err != nil {
		return nil, err
	}

	// Call body-thunk, catching continuation/exception jumps so out-thunk always runs
	var result *Value
	var bodyErr error
	var jump *continuationJump
	var exc *schemeException

	func() {
		defer func() {
			if r := recover(); r != nil {
				switch j := r.(type) {
				case continuationJump:
					jump = &j
				case schemeException:
					exc = &j
				default:
					panic(r)
				}
			}
		}()
		result, bodyErr = callThunk(bodyThunk)
	}()

	// Out-thunk runs whether body completed normally or via jump
	if _, err := callThunk(outThunk); err != nil {
		return nil, err
	}

	// Re-panic after out-thunk
	if jump != nil {
		panic(*jump)
	}
	if exc != nil {
		panic(*exc)
	}

	if bodyErr != nil {
		return nil, bodyErr
	}

	return result, nil
}

func builtinRaise(args []*Value, expr *Expr) (*Value, error) {
	if len(args) != 1 {
		return nil, errAtf(expr, "raise: expected 1 argument, got %d", len(args))
	}
	panic(schemeException{value: args[0]})
}

func builtinWithExceptionHandler(args []*Value, expr *Expr, env *Env) (*Value, error) {
	if len(args) != 2 {
		return nil, errAtf(expr, "with-exception-handler: expected 2 arguments, got %d", len(args))
	}
	handler := args[0]
	thunk := args[1]

	var result *Value
	var evalErr error
	var cjump *continuationJump

	func() {
		defer func() {
			if r := recover(); r != nil {
				switch exc := r.(type) {
				case schemeException:
					// Call the handler with the exception value
					te, tenv, v, err, isTail := applyFuncTCO(handler, []*Value{exc.value}, expr, env)
					if err != nil {
						evalErr = err
						return
					}
					if isTail {
						result, evalErr = Eval(te, tenv)
					} else {
						result = v
					}
				case continuationJump:
					cjump = &exc
				default:
					panic(r)
				}
			}
		}()

		te, tenv, v, err, isTail := applyFuncTCO(thunk, []*Value{}, expr, env)
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

	if cjump != nil {
		panic(*cjump)
	}

	return result, evalErr
}

// evalGuard implements (guard (var clause ...) body ...)
func evalGuard(expr *Expr, env *Env) (*Value, error) {
	// (guard (var clause1 clause2 ...) body ...)
	if len(expr.List) < 3 {
		return nil, errAt(expr, "guard: bad syntax")
	}
	clauseList := expr.List[1]
	if clauseList.Type != ExprList || len(clauseList.List) < 2 {
		return nil, errAt(expr, "guard: bad syntax")
	}
	varSym := clauseList.List[0]
	if varSym.Type != ExprSymbol {
		return nil, errAt(expr, "guard: variable must be a symbol")
	}
	clauses := clauseList.List[1:]
	body := expr.List[2:]

	// Evaluate body, catching schemeException
	var result *Value
	var bodyErr error
	var caught *schemeException
	var cjump *continuationJump

	func() {
		defer func() {
			if r := recover(); r != nil {
				switch exc := r.(type) {
				case schemeException:
					caught = &exc
				case continuationJump:
					cjump = &exc
				default:
					panic(r)
				}
			}
		}()

		// Evaluate body expressions
		for i, b := range body {
			result, bodyErr = Eval(b, env)
			if bodyErr != nil {
				return
			}
			_ = i
		}
	}()

	if cjump != nil {
		panic(*cjump)
	}

	if bodyErr != nil {
		return nil, bodyErr
	}

	if caught == nil {
		// No exception — return body result
		return result, nil
	}

	// Exception was caught — bind var and test clauses
	guardEnv := NewEnv(env)
	guardEnv.Set(varSym.StrVal, caught.value)

	for _, clause := range clauses {
		if clause.Type != ExprList || len(clause.List) == 0 {
			return nil, errAt(clause, "guard: bad clause")
		}

		// Check for else clause
		if clause.List[0].Type == ExprSymbol && clause.List[0].StrVal == "else" {
			// Evaluate else body
			var val *Value
			for _, e := range clause.List[1:] {
				var err error
				val, err = Eval(e, guardEnv)
				if err != nil {
					return nil, err
				}
			}
			return val, nil
		}

		// Evaluate test
		test, err := Eval(clause.List[0], guardEnv)
		if err != nil {
			return nil, err
		}
		if test.IsTruthy() {
			// Evaluate clause body (or return test if no body)
			if len(clause.List) == 1 {
				return test, nil
			}
			var val *Value
			for _, e := range clause.List[1:] {
				val, err = Eval(e, guardEnv)
				if err != nil {
					return nil, err
				}
			}
			return val, nil
		}
	}

	// No clause matched — re-raise
	panic(schemeException{value: caught.value})
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

func evalDefineSyntax(expr *Expr, env *Env) (*Value, error) {
	if len(expr.List) != 3 {
		return nil, errAt(expr, "define-syntax: expected 2 arguments")
	}
	name := expr.List[1]
	if name.Type != ExprSymbol {
		return nil, errAt(name, "define-syntax: name must be a symbol")
	}
	transformer := expr.List[2]
	if transformer.Type != ExprList || len(transformer.List) == 0 ||
		transformer.List[0].Type != ExprSymbol || transformer.List[0].StrVal != "syntax-rules" {
		return nil, errAt(transformer, "define-syntax: expected syntax-rules")
	}
	sr, err := parseSyntaxRules(transformer, env)
	if err != nil {
		return nil, err
	}
	env.Set(name.StrVal, &Value{Type: TypeMacro, Macro: sr})
	return Void, nil
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

func schemeEqual(a, b *Value) bool {
	if a.Type != b.Type {
		return false
	}
	switch a.Type {
	case TypeInteger:
		return a.IntVal == b.IntVal
	case TypeBoolean:
		return a.BoolVal == b.BoolVal
	case TypeString:
		return a.StrContent() == b.StrContent()
	case TypeSymbol:
		return a.StrVal == b.StrVal
	case TypeChar:
		return a.IntVal == b.IntVal
	case TypeNull:
		return true
	case TypePair:
		return schemeEqual(a.Car, b.Car) && schemeEqual(a.Cdr, b.Cdr)
	case TypeVector:
		if len(a.VecElems) != len(b.VecElems) {
			return false
		}
		for i := range a.VecElems {
			if !schemeEqual(a.VecElems[i], b.VecElems[i]) {
				return false
			}
		}
		return true
	default:
		return a == b
	}
}

func schemeEq(a, b *Value) bool {
	if a == b {
		return true
	}
	if a.Type != b.Type {
		return false
	}
	switch a.Type {
	case TypeInteger:
		return a.IntVal == b.IntVal
	case TypeBoolean:
		return a.BoolVal == b.BoolVal
	case TypeSymbol:
		return a.StrVal == b.StrVal
	case TypeChar:
		return a.IntVal == b.IntVal
	case TypeNull:
		return true
	default:
		return false
	}
}

func builtinMap(args []*Value, expr *Expr, env *Env) (*Value, error) {
	if len(args) < 2 {
		return nil, errAtf(expr, "map: expected at least 2 arguments")
	}
	fn := args[0]
	lists := args[1:]
	// Collect results
	var results []*Value
	for {
		// Check if any list is exhausted
		allPair := true
		for _, l := range lists {
			if l.Type == TypeNull {
				allPair = false
				break
			}
			if l.Type != TypePair {
				return nil, errAtf(expr, "map: expected proper list")
			}
		}
		if !allPair {
			break
		}
		// Collect car of each list
		mapArgs := make([]*Value, len(lists))
		for i, l := range lists {
			mapArgs[i] = l.Car
		}
		// Apply function
		te, tenv, v, err, isTail := applyFuncTCO(fn, mapArgs, expr, env)
		if err != nil {
			return nil, err
		}
		if isTail {
			v, err = Eval(te, tenv)
			if err != nil {
				return nil, err
			}
		}
		results = append(results, v)
		// Advance lists
		for i, l := range lists {
			lists[i] = l.Cdr
		}
	}
	// Build result list
	result := Null
	for i := len(results) - 1; i >= 0; i-- {
		result = PairValue(results[i], result)
	}
	return result, nil
}

func builtinForEach(args []*Value, expr *Expr, env *Env) (*Value, error) {
	if len(args) < 2 {
		return nil, errAtf(expr, "for-each: expected at least 2 arguments")
	}
	fn := args[0]
	lists := args[1:]
	for {
		allPair := true
		for _, l := range lists {
			if l.Type == TypeNull {
				allPair = false
				break
			}
			if l.Type != TypePair {
				return nil, errAtf(expr, "for-each: expected proper list")
			}
		}
		if !allPair {
			break
		}
		mapArgs := make([]*Value, len(lists))
		for i, l := range lists {
			mapArgs[i] = l.Car
		}
		te, tenv, v, err, isTail := applyFuncTCO(fn, mapArgs, expr, env)
		if err != nil {
			return nil, err
		}
		if isTail {
			v, err = Eval(te, tenv)
			if err != nil {
				return nil, err
			}
		}
		_ = v
		for i, l := range lists {
			lists[i] = l.Cdr
		}
	}
	return Void, nil
}

func evalLetrecTCO(expr *Expr, env *Env, star bool) (*Expr, *Env, *Value, error) {
	if len(expr.List) < 3 {
		return nil, nil, nil, errAt(expr, "letrec: expected at least 2 arguments")
	}
	bindings := expr.List[1]
	if bindings.Type != ExprList {
		return nil, nil, nil, errAt(bindings, "letrec: bindings must be a list")
	}
	localEnv := NewEnv(env)
	// Initialize all bindings to void
	for _, b := range bindings.List {
		if b.Type != ExprList || len(b.List) != 2 || b.List[0].Type != ExprSymbol {
			return nil, nil, nil, errAt(b, "letrec: invalid binding")
		}
		localEnv.Set(b.List[0].StrVal, Void)
	}
	// Evaluate init expressions
	for _, b := range bindings.List {
		evalEnv := localEnv
		if !star {
			// For letrec, init exprs can see the bindings (for lambda captures)
			evalEnv = localEnv
		}
		val, err := Eval(b.List[1], evalEnv)
		if err != nil {
			return nil, nil, nil, err
		}
		localEnv.Set(b.List[0].StrVal, val)
	}
	body := expr.List[2:]
	for _, bodyExpr := range body[:len(body)-1] {
		_, err := Eval(bodyExpr, localEnv)
		if err != nil {
			return nil, nil, nil, err
		}
	}
	return body[len(body)-1], localEnv, nil, nil
}

func evalCaseTCO(expr *Expr, env *Env) (*Expr, *Env, *Value, error) {
	if len(expr.List) < 3 {
		return nil, nil, nil, errAt(expr, "case: expected at least 2 arguments")
	}
	key, err := Eval(expr.List[1], env)
	if err != nil {
		return nil, nil, nil, err
	}
	for _, clause := range expr.List[2:] {
		if clause.Type != ExprList || len(clause.List) < 2 {
			return nil, nil, nil, errAt(clause, "case: invalid clause")
		}
		datums := clause.List[0]
		// else clause
		if datums.Type == ExprSymbol && datums.StrVal == "else" {
			body := clause.List[1:]
			for _, e := range body[:len(body)-1] {
				_, err := Eval(e, env)
				if err != nil {
					return nil, nil, nil, err
				}
			}
			return body[len(body)-1], env, nil, nil
		}
		// Match datums
		if datums.Type != ExprList {
			return nil, nil, nil, errAt(datums, "case: datums must be a list")
		}
		matched := false
		for _, d := range datums.List {
			dv, err := exprToValue(d)
			if err != nil {
				return nil, nil, nil, err
			}
			if schemeEqv(key, dv) {
				matched = true
				break
			}
		}
		if matched {
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

func schemeEqv(a, b *Value) bool {
	if a.Type != b.Type {
		return false
	}
	switch a.Type {
	case TypeInteger:
		return a.IntVal == b.IntVal
	case TypeBoolean:
		return a.BoolVal == b.BoolVal
	case TypeSymbol:
		return a.StrVal == b.StrVal
	case TypeChar:
		return a.IntVal == b.IntVal
	case TypeNull:
		return true
	default:
		return a == b
	}
}

func makeDefaultEnv() *Env {
	env := NewEnv(nil)
	builtins := []string{"+", "-", "*", "/", "<", ">", "=", "<=", ">=", "not",
		"cons", "car", "cdr", "null?", "list", "length", "reverse",
		"pair?", "number?", "string?", "boolean?", "symbol?", "char?",
		"display", "write", "newline",
		"string-append", "string-length", "substring",
		"string->number", "number->string",
		"symbol->string", "string->symbol",
		"string-ref",
		"string-set!", "string-copy",
		"string->list", "list->string",
		"char->integer", "integer->char",
		"apply",
		"call/cc", "call-with-current-continuation",
		"equal?", "eq?", "eqv?",
		"abs", "modulo", "remainder", "quotient",
		"min", "max", "expt",
		"zero?", "positive?", "negative?", "odd?", "even?",
		"list-ref", "list-tail", "list?",
		"assoc", "map", "for-each",
		"char-alphabetic?", "char-numeric?",
		"char-upcase", "char-downcase",
		"char=?", "char<?",
		"string=?", "string<?", "string-ci=?",
		"string-upcase", "string-downcase",
		"vector", "make-vector", "vector-ref", "vector-set!", "vector-length", "vector?",
		"vector->list", "list->vector",
		"dynamic-wind",
		"raise", "with-exception-handler"}
	for _, name := range builtins {
		env.Set(name, &Value{Type: TypeSymbol, StrVal: fmt.Sprintf("__builtin:%s", name)})
	}
	return env
}
