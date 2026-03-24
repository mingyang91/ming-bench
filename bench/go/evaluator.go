package ming

import (
	"fmt"
	"strconv"
	"strings"
)

// EvalStr evaluates one or more Scheme expressions and returns the string
// representation of the last result.
func EvalStr(input string) (string, error) {
	exprs, err := parse(input)
	if err != nil {
		return "", &EvalError{Message: err.Error()}
	}
	if len(exprs) == 0 {
		return "", &EvalError{Message: "empty input"}
	}

	env := defaultEnv(nil)
	var result Value
	for _, expr := range exprs {
		result, err = eval(expr, env)
		if err != nil {
			return "", err
		}
	}
	// void produces empty string
	if _, ok := result.(*VoidVal); ok {
		return "", nil
	}
	return result.String(), nil
}

// EvalStrWithOutput evaluates Scheme expressions and returns both the result
// string and any captured output from display/write/newline.
func EvalStrWithOutput(input string) (result string, output string, err error) {
	exprs, parseErr := parse(input)
	if parseErr != nil {
		return "", "", &EvalError{Message: parseErr.Error()}
	}
	if len(exprs) == 0 {
		return "", "", &EvalError{Message: "empty input"}
	}

	var buf strings.Builder
	env := defaultEnv(&buf)
	var res Value
	for _, expr := range exprs {
		res, err = eval(expr, env)
		if err != nil {
			return "", "", err
		}
	}
	if _, ok := res.(*VoidVal); ok {
		return "", buf.String(), nil
	}
	return res.String(), buf.String(), nil
}

// eval evaluates an expression in the given environment.
func eval(expr *Expr, env *Env) (Value, error) {
	switch expr.Kind {
	case ExprInt:
		return &IntVal{Val: expr.IVal}, nil
	case ExprBool:
		return &BoolVal{Val: expr.BVal}, nil
	case ExprString:
		return &StringVal{Val: expr.SVal}, nil
	case ExprChar:
		return &CharVal{Val: expr.RVal}, nil
	case ExprSymbol:
		v, ok := env.Get(expr.SVal)
		if !ok {
			return nil, &EvalError{Message: fmt.Sprintf("%d:%d: unbound variable: %s", expr.Line, expr.Col, expr.SVal)}
		}
		return v, nil
	case ExprList:
		return evalList(expr, env)
	}
	return nil, &EvalError{Message: fmt.Sprintf("%d:%d: unknown expression", expr.Line, expr.Col)}
}

func evalList(expr *Expr, env *Env) (Value, error) {
	if len(expr.List) == 0 {
		return nil, &EvalError{Message: fmt.Sprintf("%d:%d: empty application", expr.Line, expr.Col)}
	}

	head := expr.List[0]

	// special forms
	if head.Kind == ExprSymbol {
		switch head.SVal {
		case "and":
			return evalAnd(expr, env)
		case "or":
			return evalOr(expr, env)
		case "define":
			return evalDefine(expr, env)
		case "if":
			return evalIf(expr, env)
		case "quote":
			return evalQuote(expr, env)
		case "lambda":
			return evalLambda(expr, env)
		case "let":
			return evalLet(expr, env)
		case "begin":
			return evalBegin(expr, env)
		case "cond":
			return evalCond(expr, env)
		case "set!":
			return evalSet(expr, env)
		}
	}

	// evaluate operator
	op, err := eval(head, env)
	if err != nil {
		return nil, err
	}

	// evaluate arguments
	args := make([]Value, len(expr.List)-1)
	for i, a := range expr.List[1:] {
		args[i], err = eval(a, env)
		if err != nil {
			return nil, err
		}
	}

	// apply
	result, err := applyProc(op, args, expr)
	if err != nil {
		return nil, err
	}
	return result, nil
}

func evalAnd(expr *Expr, env *Env) (Value, error) {
	if len(expr.List) == 1 {
		return &BoolVal{Val: true}, nil
	}
	var result Value
	for _, e := range expr.List[1:] {
		var err error
		result, err = eval(e, env)
		if err != nil {
			return nil, err
		}
		if !isTruthy(result) {
			return result, nil
		}
	}
	return result, nil
}

func evalOr(expr *Expr, env *Env) (Value, error) {
	if len(expr.List) == 1 {
		return &BoolVal{Val: false}, nil
	}
	var result Value
	for _, e := range expr.List[1:] {
		var err error
		result, err = eval(e, env)
		if err != nil {
			return nil, err
		}
		if isTruthy(result) {
			return result, nil
		}
	}
	return result, nil
}

// BuiltinFunc is a built-in procedure.
type BuiltinFunc struct {
	Name string
	Fn   func(args []Value) (Value, error)
}

func (b *BuiltinFunc) String() string { return fmt.Sprintf("#<procedure %s>", b.Name) }

func defaultEnv(output *strings.Builder) *Env {
	env := NewEnv(nil)

	env.Set("+", &BuiltinFunc{Name: "+", Fn: builtinAdd})
	env.Set("-", &BuiltinFunc{Name: "-", Fn: builtinSub})
	env.Set("*", &BuiltinFunc{Name: "*", Fn: builtinMul})
	env.Set("/", &BuiltinFunc{Name: "/", Fn: builtinDiv})
	env.Set("<", &BuiltinFunc{Name: "<", Fn: builtinLt})
	env.Set(">", &BuiltinFunc{Name: ">", Fn: builtinGt})
	env.Set("=", &BuiltinFunc{Name: "=", Fn: builtinEq})
	env.Set("<=", &BuiltinFunc{Name: "<=", Fn: builtinLe})
	env.Set("not", &BuiltinFunc{Name: "not", Fn: builtinNot})

	// L03 builtins
	env.Set("cons", &BuiltinFunc{Name: "cons", Fn: builtinCons})
	env.Set("car", &BuiltinFunc{Name: "car", Fn: builtinCar})
	env.Set("cdr", &BuiltinFunc{Name: "cdr", Fn: builtinCdr})
	env.Set("null?", &BuiltinFunc{Name: "null?", Fn: builtinNullQ})
	env.Set("list", &BuiltinFunc{Name: "list", Fn: builtinList})
	env.Set("length", &BuiltinFunc{Name: "length", Fn: builtinLength})
	env.Set("number?", &BuiltinFunc{Name: "number?", Fn: builtinNumberQ})
	env.Set("string?", &BuiltinFunc{Name: "string?", Fn: builtinStringQ})
	env.Set("boolean?", &BuiltinFunc{Name: "boolean?", Fn: builtinBooleanQ})
	env.Set("pair?", &BuiltinFunc{Name: "pair?", Fn: builtinPairQ})
	env.Set("symbol?", &BuiltinFunc{Name: "symbol?", Fn: builtinSymbolQ})
	env.Set("append", &BuiltinFunc{Name: "append", Fn: builtinAppend})

	// L05 builtins — I/O
	env.Set("display", &BuiltinFunc{Name: "display", Fn: func(args []Value) (Value, error) {
		if len(args) != 1 {
			return nil, fmt.Errorf("display: expected 1 argument, got %d", len(args))
		}
		if output != nil {
			output.WriteString(DisplayString(args[0]))
		}
		return &VoidVal{}, nil
	}})
	env.Set("write", &BuiltinFunc{Name: "write", Fn: func(args []Value) (Value, error) {
		if len(args) != 1 {
			return nil, fmt.Errorf("write: expected 1 argument, got %d", len(args))
		}
		if output != nil {
			output.WriteString(args[0].String())
		}
		return &VoidVal{}, nil
	}})
	env.Set("newline", &BuiltinFunc{Name: "newline", Fn: func(args []Value) (Value, error) {
		if len(args) != 0 {
			return nil, fmt.Errorf("newline: expected 0 arguments, got %d", len(args))
		}
		if output != nil {
			output.WriteByte('\n')
		}
		return &VoidVal{}, nil
	}})

	// L05 builtins — string operations
	env.Set("string-append", &BuiltinFunc{Name: "string-append", Fn: builtinStringAppend})
	env.Set("string-length", &BuiltinFunc{Name: "string-length", Fn: builtinStringLength})
	env.Set("substring", &BuiltinFunc{Name: "substring", Fn: builtinSubstring})
	env.Set("string->number", &BuiltinFunc{Name: "string->number", Fn: builtinStringToNumber})
	env.Set("number->string", &BuiltinFunc{Name: "number->string", Fn: builtinNumberToString})
	env.Set("symbol->string", &BuiltinFunc{Name: "symbol->string", Fn: builtinSymbolToString})
	env.Set("string->symbol", &BuiltinFunc{Name: "string->symbol", Fn: builtinStringToSymbol})
	env.Set("string-ref", &BuiltinFunc{Name: "string-ref", Fn: builtinStringRef})
	env.Set("char?", &BuiltinFunc{Name: "char?", Fn: builtinCharQ})

	// L06 builtins — mutable strings
	env.Set("string-copy", &BuiltinFunc{Name: "string-copy", Fn: builtinStringCopy})
	env.Set("string-set!", &BuiltinFunc{Name: "string-set!", Fn: builtinStringSet})

	return env
}

func requireInts(name string, args []Value) ([]int64, error) {
	nums := make([]int64, len(args))
	for i, a := range args {
		n, ok := a.(*IntVal)
		if !ok {
			return nil, fmt.Errorf("%s: expected number, got %s", name, a.String())
		}
		nums[i] = n.Val
	}
	return nums, nil
}

func builtinAdd(args []Value) (Value, error) {
	nums, err := requireInts("+", args)
	if err != nil {
		return nil, err
	}
	var sum int64
	for _, n := range nums {
		sum += n
	}
	return &IntVal{Val: sum}, nil
}

func builtinSub(args []Value) (Value, error) {
	if len(args) == 0 {
		return nil, fmt.Errorf("-: need at least 1 argument")
	}
	nums, err := requireInts("-", args)
	if err != nil {
		return nil, err
	}
	if len(nums) == 1 {
		return &IntVal{Val: -nums[0]}, nil
	}
	result := nums[0]
	for _, n := range nums[1:] {
		result -= n
	}
	return &IntVal{Val: result}, nil
}

func builtinMul(args []Value) (Value, error) {
	nums, err := requireInts("*", args)
	if err != nil {
		return nil, err
	}
	result := int64(1)
	for _, n := range nums {
		result *= n
	}
	return &IntVal{Val: result}, nil
}

func builtinDiv(args []Value) (Value, error) {
	if len(args) < 2 {
		return nil, fmt.Errorf("/: need at least 2 arguments")
	}
	nums, err := requireInts("/", args)
	if err != nil {
		return nil, err
	}
	result := nums[0]
	for _, n := range nums[1:] {
		if n == 0 {
			return nil, fmt.Errorf("/: division by zero")
		}
		result /= n
	}
	return &IntVal{Val: result}, nil
}

func builtinLt(args []Value) (Value, error) {
	if len(args) != 2 {
		return nil, fmt.Errorf("<: expected 2 arguments")
	}
	nums, err := requireInts("<", args)
	if err != nil {
		return nil, err
	}
	return &BoolVal{Val: nums[0] < nums[1]}, nil
}

func builtinGt(args []Value) (Value, error) {
	if len(args) != 2 {
		return nil, fmt.Errorf(">: expected 2 arguments")
	}
	nums, err := requireInts(">", args)
	if err != nil {
		return nil, err
	}
	return &BoolVal{Val: nums[0] > nums[1]}, nil
}

func builtinEq(args []Value) (Value, error) {
	if len(args) != 2 {
		return nil, fmt.Errorf("=: expected 2 arguments")
	}
	nums, err := requireInts("=", args)
	if err != nil {
		return nil, err
	}
	return &BoolVal{Val: nums[0] == nums[1]}, nil
}

func builtinLe(args []Value) (Value, error) {
	if len(args) != 2 {
		return nil, fmt.Errorf("<=: expected 2 arguments")
	}
	nums, err := requireInts("<=", args)
	if err != nil {
		return nil, err
	}
	return &BoolVal{Val: nums[0] <= nums[1]}, nil
}

func builtinNot(args []Value) (Value, error) {
	if len(args) != 1 {
		return nil, fmt.Errorf("not: expected 1 argument")
	}
	return &BoolVal{Val: !isTruthy(args[0])}, nil
}

// applyProc applies a procedure (builtin or lambda) to arguments.
func applyProc(op Value, args []Value, callExpr *Expr) (Value, error) {
	switch fn := op.(type) {
	case *BuiltinFunc:
		result, err := fn.Fn(args)
		if err != nil {
			return nil, &EvalError{Message: fmt.Sprintf("%d:%d: %s", callExpr.Line, callExpr.Col, err.Error())}
		}
		return result, nil
	case *LambdaVal:
		if len(args) != len(fn.Params) {
			return nil, &EvalError{Message: fmt.Sprintf("%d:%d: expected %d arguments, got %d", callExpr.Line, callExpr.Col, len(fn.Params), len(args))}
		}
		childEnv := NewEnv(fn.Env)
		for i, p := range fn.Params {
			childEnv.Set(p, args[i])
		}
		var result Value
		var err error
		for _, bodyExpr := range fn.Body {
			result, err = eval(bodyExpr, childEnv)
			if err != nil {
				return nil, err
			}
		}
		return result, nil
	default:
		return nil, &EvalError{Message: fmt.Sprintf("%d:%d: not a procedure", callExpr.List[0].Line, callExpr.List[0].Col)}
	}
}

func evalDefine(expr *Expr, env *Env) (Value, error) {
	if len(expr.List) < 3 {
		return nil, &EvalError{Message: fmt.Sprintf("%d:%d: define: bad syntax", expr.Line, expr.Col)}
	}
	target := expr.List[1]

	// (define (f params...) body...)
	if target.Kind == ExprList && len(target.List) > 0 {
		name := target.List[0]
		if name.Kind != ExprSymbol {
			return nil, &EvalError{Message: fmt.Sprintf("%d:%d: define: expected symbol", name.Line, name.Col)}
		}
		params := make([]string, len(target.List)-1)
		for i, p := range target.List[1:] {
			if p.Kind != ExprSymbol {
				return nil, &EvalError{Message: fmt.Sprintf("%d:%d: define: expected parameter name", p.Line, p.Col)}
			}
			params[i] = p.SVal
		}
		lam := &LambdaVal{Params: params, Body: expr.List[2:], Env: env}
		env.Set(name.SVal, lam)
		return &VoidVal{}, nil
	}

	// (define x val)
	if target.Kind != ExprSymbol {
		return nil, &EvalError{Message: fmt.Sprintf("%d:%d: define: expected symbol", target.Line, target.Col)}
	}
	val, err := eval(expr.List[2], env)
	if err != nil {
		return nil, err
	}
	env.Set(target.SVal, val)
	return &VoidVal{}, nil
}

func evalIf(expr *Expr, env *Env) (Value, error) {
	if len(expr.List) < 3 || len(expr.List) > 4 {
		return nil, &EvalError{Message: fmt.Sprintf("%d:%d: if: bad syntax", expr.Line, expr.Col)}
	}
	cond, err := eval(expr.List[1], env)
	if err != nil {
		return nil, err
	}
	if isTruthy(cond) {
		return eval(expr.List[2], env)
	}
	if len(expr.List) == 4 {
		return eval(expr.List[3], env)
	}
	return &VoidVal{}, nil
}

func evalQuote(expr *Expr, env *Env) (Value, error) {
	if len(expr.List) != 2 {
		return nil, &EvalError{Message: fmt.Sprintf("%d:%d: quote: expected 1 argument", expr.Line, expr.Col)}
	}
	return exprToValue(expr.List[1]), nil
}

func exprToValue(e *Expr) Value {
	switch e.Kind {
	case ExprInt:
		return &IntVal{Val: e.IVal}
	case ExprBool:
		return &BoolVal{Val: e.BVal}
	case ExprString:
		return &StringVal{Val: e.SVal}
	case ExprSymbol:
		return &SymbolVal{Name: e.SVal}
	case ExprList:
		if len(e.List) == 0 {
			return &NilVal{}
		}
		// Build proper list from elements
		result := Value(&NilVal{})
		for i := len(e.List) - 1; i >= 0; i-- {
			result = &PairVal{Car: exprToValue(e.List[i]), Cdr: result}
		}
		return result
	}
	return &VoidVal{}
}

func evalLet(expr *Expr, env *Env) (Value, error) {
	// (let ((var val) ...) body...)
	// or named let: (let name ((var val) ...) body...)
	if len(expr.List) < 3 {
		return nil, &EvalError{Message: fmt.Sprintf("%d:%d: let: bad syntax", expr.Line, expr.Col)}
	}

	nameIdx := 1
	var loopName string

	// named let?
	if expr.List[1].Kind == ExprSymbol {
		loopName = expr.List[1].SVal
		nameIdx = 2
		if len(expr.List) < 4 {
			return nil, &EvalError{Message: fmt.Sprintf("%d:%d: let: bad syntax", expr.Line, expr.Col)}
		}
	}

	bindingsExpr := expr.List[nameIdx]
	if bindingsExpr.Kind != ExprList {
		return nil, &EvalError{Message: fmt.Sprintf("%d:%d: let: expected bindings list", bindingsExpr.Line, bindingsExpr.Col)}
	}

	params := make([]string, len(bindingsExpr.List))
	vals := make([]Value, len(bindingsExpr.List))
	for i, b := range bindingsExpr.List {
		if b.Kind != ExprList || len(b.List) != 2 || b.List[0].Kind != ExprSymbol {
			return nil, &EvalError{Message: fmt.Sprintf("%d:%d: let: bad binding", b.Line, b.Col)}
		}
		params[i] = b.List[0].SVal
		var err error
		vals[i], err = eval(b.List[1], env)
		if err != nil {
			return nil, err
		}
	}

	body := expr.List[nameIdx+1:]

	if loopName != "" {
		// named let: create a lambda and bind it, then call it
		lam := &LambdaVal{Params: params, Body: body, Env: env}
		childEnv := NewEnv(env)
		childEnv.Set(loopName, lam)
		lam.Env = childEnv
		for i, p := range params {
			childEnv.Set(p, vals[i])
		}
		var result Value
		var err error
		for _, bodyExpr := range body {
			result, err = eval(bodyExpr, childEnv)
			if err != nil {
				return nil, err
			}
		}
		return result, nil
	}

	childEnv := NewEnv(env)
	for i, p := range params {
		childEnv.Set(p, vals[i])
	}
	var result Value
	var err error
	for _, bodyExpr := range body {
		result, err = eval(bodyExpr, childEnv)
		if err != nil {
			return nil, err
		}
	}
	return result, nil
}

func evalBegin(expr *Expr, env *Env) (Value, error) {
	if len(expr.List) < 2 {
		return &VoidVal{}, nil
	}
	var result Value
	var err error
	for _, e := range expr.List[1:] {
		result, err = eval(e, env)
		if err != nil {
			return nil, err
		}
	}
	return result, nil
}

func evalSet(expr *Expr, env *Env) (Value, error) {
	if len(expr.List) != 3 {
		return nil, &EvalError{Message: fmt.Sprintf("%d:%d: set!: bad syntax", expr.Line, expr.Col)}
	}
	target := expr.List[1]
	if target.Kind != ExprSymbol {
		return nil, &EvalError{Message: fmt.Sprintf("%d:%d: set!: expected symbol", target.Line, target.Col)}
	}
	val, err := eval(expr.List[2], env)
	if err != nil {
		return nil, err
	}
	if !env.SetMut(target.SVal, val) {
		return nil, &EvalError{Message: fmt.Sprintf("%d:%d: set!: unbound variable: %s", target.Line, target.Col, target.SVal)}
	}
	return &VoidVal{}, nil
}

func evalCond(expr *Expr, env *Env) (Value, error) {
	for _, clause := range expr.List[1:] {
		if clause.Kind != ExprList || len(clause.List) < 2 {
			return nil, &EvalError{Message: fmt.Sprintf("%d:%d: cond: bad clause", clause.Line, clause.Col)}
		}
		// else clause
		if clause.List[0].Kind == ExprSymbol && clause.List[0].SVal == "else" {
			var result Value
			var err error
			for _, e := range clause.List[1:] {
				result, err = eval(e, env)
				if err != nil {
					return nil, err
				}
			}
			return result, nil
		}
		test, err := eval(clause.List[0], env)
		if err != nil {
			return nil, err
		}
		if isTruthy(test) {
			var result Value
			for _, e := range clause.List[1:] {
				result, err = eval(e, env)
				if err != nil {
					return nil, err
				}
			}
			return result, nil
		}
	}
	return &VoidVal{}, nil
}

func evalLambda(expr *Expr, env *Env) (Value, error) {
	if len(expr.List) < 3 {
		return nil, &EvalError{Message: fmt.Sprintf("%d:%d: lambda: bad syntax", expr.Line, expr.Col)}
	}
	paramExpr := expr.List[1]
	if paramExpr.Kind != ExprList {
		return nil, &EvalError{Message: fmt.Sprintf("%d:%d: lambda: expected parameter list", paramExpr.Line, paramExpr.Col)}
	}
	params := make([]string, len(paramExpr.List))
	for i, p := range paramExpr.List {
		if p.Kind != ExprSymbol {
			return nil, &EvalError{Message: fmt.Sprintf("%d:%d: lambda: expected parameter name", p.Line, p.Col)}
		}
		params[i] = p.SVal
	}
	return &LambdaVal{Params: params, Body: expr.List[2:], Env: env}, nil
}

// L03 builtins

func builtinCons(args []Value) (Value, error) {
	if len(args) != 2 {
		return nil, fmt.Errorf("cons: expected 2 arguments, got %d", len(args))
	}
	return &PairVal{Car: args[0], Cdr: args[1]}, nil
}

func builtinCar(args []Value) (Value, error) {
	if len(args) != 1 {
		return nil, fmt.Errorf("car: expected 1 argument, got %d", len(args))
	}
	p, ok := args[0].(*PairVal)
	if !ok {
		return nil, fmt.Errorf("car: expected pair, got %s", args[0].String())
	}
	return p.Car, nil
}

func builtinCdr(args []Value) (Value, error) {
	if len(args) != 1 {
		return nil, fmt.Errorf("cdr: expected 1 argument, got %d", len(args))
	}
	p, ok := args[0].(*PairVal)
	if !ok {
		return nil, fmt.Errorf("cdr: expected pair, got %s", args[0].String())
	}
	return p.Cdr, nil
}

func builtinNullQ(args []Value) (Value, error) {
	if len(args) != 1 {
		return nil, fmt.Errorf("null?: expected 1 argument, got %d", len(args))
	}
	_, isNil := args[0].(*NilVal)
	return &BoolVal{Val: isNil}, nil
}

func builtinList(args []Value) (Value, error) {
	result := Value(&NilVal{})
	for i := len(args) - 1; i >= 0; i-- {
		result = &PairVal{Car: args[i], Cdr: result}
	}
	return result, nil
}

func builtinLength(args []Value) (Value, error) {
	if len(args) != 1 {
		return nil, fmt.Errorf("length: expected 1 argument, got %d", len(args))
	}
	count := int64(0)
	cur := args[0]
	for {
		switch v := cur.(type) {
		case *NilVal:
			return &IntVal{Val: count}, nil
		case *PairVal:
			count++
			cur = v.Cdr
		default:
			return nil, fmt.Errorf("length: expected list")
		}
	}
}

func builtinAppend(args []Value) (Value, error) {
	if len(args) == 0 {
		return &NilVal{}, nil
	}
	if len(args) == 1 {
		return args[0], nil
	}
	// Collect all elements from all lists except the last, then attach the last
	var elems []Value
	for i := 0; i < len(args)-1; i++ {
		cur := args[i]
		for {
			switch v := cur.(type) {
			case *NilVal:
				goto nextList
			case *PairVal:
				elems = append(elems, v.Car)
				cur = v.Cdr
			default:
				return nil, fmt.Errorf("append: expected list")
			}
		}
	nextList:
	}
	result := args[len(args)-1]
	for i := len(elems) - 1; i >= 0; i-- {
		result = &PairVal{Car: elems[i], Cdr: result}
	}
	return result, nil
}

func builtinNumberQ(args []Value) (Value, error) {
	if len(args) != 1 {
		return nil, fmt.Errorf("number?: expected 1 argument")
	}
	_, ok := args[0].(*IntVal)
	return &BoolVal{Val: ok}, nil
}

func builtinStringQ(args []Value) (Value, error) {
	if len(args) != 1 {
		return nil, fmt.Errorf("string?: expected 1 argument")
	}
	_, ok := args[0].(*StringVal)
	return &BoolVal{Val: ok}, nil
}

func builtinBooleanQ(args []Value) (Value, error) {
	if len(args) != 1 {
		return nil, fmt.Errorf("boolean?: expected 1 argument")
	}
	_, ok := args[0].(*BoolVal)
	return &BoolVal{Val: ok}, nil
}

func builtinPairQ(args []Value) (Value, error) {
	if len(args) != 1 {
		return nil, fmt.Errorf("pair?: expected 1 argument")
	}
	_, ok := args[0].(*PairVal)
	return &BoolVal{Val: ok}, nil
}

func builtinSymbolQ(args []Value) (Value, error) {
	if len(args) != 1 {
		return nil, fmt.Errorf("symbol?: expected 1 argument")
	}
	_, ok := args[0].(*SymbolVal)
	return &BoolVal{Val: ok}, nil
}

// L05 builtins

func builtinStringAppend(args []Value) (Value, error) {
	var buf strings.Builder
	for _, a := range args {
		s, ok := a.(*StringVal)
		if !ok {
			return nil, fmt.Errorf("string-append: expected string, got %s", a.String())
		}
		buf.WriteString(s.Val)
	}
	return &StringVal{Val: buf.String()}, nil
}

func builtinStringLength(args []Value) (Value, error) {
	if len(args) != 1 {
		return nil, fmt.Errorf("string-length: expected 1 argument, got %d", len(args))
	}
	s, ok := args[0].(*StringVal)
	if !ok {
		return nil, fmt.Errorf("string-length: expected string, got %s", args[0].String())
	}
	return &IntVal{Val: int64(len([]rune(s.Val)))}, nil
}

func builtinSubstring(args []Value) (Value, error) {
	if len(args) != 3 {
		return nil, fmt.Errorf("substring: expected 3 arguments, got %d", len(args))
	}
	s, ok := args[0].(*StringVal)
	if !ok {
		return nil, fmt.Errorf("substring: expected string, got %s", args[0].String())
	}
	start, ok := args[1].(*IntVal)
	if !ok {
		return nil, fmt.Errorf("substring: expected number, got %s", args[1].String())
	}
	end, ok := args[2].(*IntVal)
	if !ok {
		return nil, fmt.Errorf("substring: expected number, got %s", args[2].String())
	}
	runes := []rune(s.Val)
	if start.Val < 0 || end.Val < start.Val || int(end.Val) > len(runes) {
		return nil, fmt.Errorf("substring: index out of range")
	}
	return &StringVal{Val: string(runes[start.Val:end.Val])}, nil
}

func builtinStringToNumber(args []Value) (Value, error) {
	if len(args) != 1 {
		return nil, fmt.Errorf("string->number: expected 1 argument, got %d", len(args))
	}
	s, ok := args[0].(*StringVal)
	if !ok {
		return nil, fmt.Errorf("string->number: expected string, got %s", args[0].String())
	}
	n, err := strconv.ParseInt(s.Val, 10, 64)
	if err != nil {
		return &BoolVal{Val: false}, nil
	}
	return &IntVal{Val: n}, nil
}

func builtinNumberToString(args []Value) (Value, error) {
	if len(args) != 1 {
		return nil, fmt.Errorf("number->string: expected 1 argument, got %d", len(args))
	}
	n, ok := args[0].(*IntVal)
	if !ok {
		return nil, fmt.Errorf("number->string: expected number, got %s", args[0].String())
	}
	return &StringVal{Val: strconv.FormatInt(n.Val, 10)}, nil
}

func builtinSymbolToString(args []Value) (Value, error) {
	if len(args) != 1 {
		return nil, fmt.Errorf("symbol->string: expected 1 argument, got %d", len(args))
	}
	s, ok := args[0].(*SymbolVal)
	if !ok {
		return nil, fmt.Errorf("symbol->string: expected symbol, got %s", args[0].String())
	}
	return &StringVal{Val: s.Name}, nil
}

func builtinStringToSymbol(args []Value) (Value, error) {
	if len(args) != 1 {
		return nil, fmt.Errorf("string->symbol: expected 1 argument, got %d", len(args))
	}
	s, ok := args[0].(*StringVal)
	if !ok {
		return nil, fmt.Errorf("string->symbol: expected string, got %s", args[0].String())
	}
	return &SymbolVal{Name: s.Val}, nil
}

func builtinStringRef(args []Value) (Value, error) {
	if len(args) != 2 {
		return nil, fmt.Errorf("string-ref: expected 2 arguments, got %d", len(args))
	}
	s, ok := args[0].(*StringVal)
	if !ok {
		return nil, fmt.Errorf("string-ref: expected string, got %s", args[0].String())
	}
	idx, ok := args[1].(*IntVal)
	if !ok {
		return nil, fmt.Errorf("string-ref: expected number, got %s", args[1].String())
	}
	runes := []rune(s.Val)
	if idx.Val < 0 || int(idx.Val) >= len(runes) {
		return nil, fmt.Errorf("string-ref: index out of range")
	}
	return &CharVal{Val: runes[idx.Val]}, nil
}

func builtinCharQ(args []Value) (Value, error) {
	if len(args) != 1 {
		return nil, fmt.Errorf("char?: expected 1 argument, got %d", len(args))
	}
	_, ok := args[0].(*CharVal)
	return &BoolVal{Val: ok}, nil
}

// L06 builtins

func builtinStringCopy(args []Value) (Value, error) {
	if len(args) != 1 {
		return nil, fmt.Errorf("string-copy: expected 1 argument, got %d", len(args))
	}
	s, ok := args[0].(*StringVal)
	if !ok {
		return nil, fmt.Errorf("string-copy: expected string, got %s", args[0].String())
	}
	return &StringVal{Val: s.Val}, nil
}

func builtinStringSet(args []Value) (Value, error) {
	if len(args) != 3 {
		return nil, fmt.Errorf("string-set!: expected 3 arguments, got %d", len(args))
	}
	s, ok := args[0].(*StringVal)
	if !ok {
		return nil, fmt.Errorf("string-set!: expected string, got %s", args[0].String())
	}
	idx, ok := args[1].(*IntVal)
	if !ok {
		return nil, fmt.Errorf("string-set!: expected number, got %s", args[1].String())
	}
	ch, ok := args[2].(*CharVal)
	if !ok {
		return nil, fmt.Errorf("string-set!: expected char, got %s", args[2].String())
	}
	runes := []rune(s.Val)
	if idx.Val < 0 || int(idx.Val) >= len(runes) {
		return nil, fmt.Errorf("string-set!: index out of range")
	}
	runes[idx.Val] = ch.Val
	s.Val = string(runes)
	return &VoidVal{}, nil
}
