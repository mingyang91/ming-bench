package ming

import "fmt"

// Env represents a Scheme environment (scope).
type Env struct {
	bindings map[string]*Value
	parent   *Env
}

func NewEnv(parent *Env) *Env {
	return &Env{bindings: make(map[string]*Value), parent: parent}
}

func (e *Env) Get(name string) (*Value, bool) {
	if v, ok := e.bindings[name]; ok {
		return v, true
	}
	if e.parent != nil {
		return e.parent.Get(name)
	}
	return nil, false
}

func (e *Env) Set(name string, val *Value) {
	e.bindings[name] = val
}

// Eval evaluates an expression in the given environment.
func Eval(expr *Expr, env *Env) (*Value, error) {
	switch expr.Type {
	case ExprInt:
		return IntValue(expr.IntVal), nil
	case ExprBool:
		return BoolValue(expr.BoolVal), nil
	case ExprString:
		return StringValue(expr.StrVal), nil
	case ExprSymbol:
		v, ok := env.Get(expr.StrVal)
		if !ok {
			return nil, fmt.Errorf("%d:%d: unbound variable: %s", expr.Line, expr.Col, expr.StrVal)
		}
		return v, nil
	case ExprList:
		return evalList(expr, env)
	default:
		return nil, fmt.Errorf("%d:%d: unknown expression type", expr.Line, expr.Col)
	}
}

func evalList(expr *Expr, env *Env) (*Value, error) {
	if len(expr.List) == 0 {
		return nil, fmt.Errorf("%d:%d: empty application", expr.Line, expr.Col)
	}

	head := expr.List[0]

	// Special forms
	if head.Type == ExprSymbol {
		switch head.StrVal {
		case "and":
			return evalAnd(expr, env)
		case "or":
			return evalOr(expr, env)
		case "define":
			return evalDefine(expr, env)
		case "if":
			return evalIf(expr, env)
		case "quote":
			return evalQuote(expr)
		case "lambda":
			return evalLambda(expr, env)
		case "let":
			return evalLet(expr, env)
		case "begin":
			return evalBegin(expr, env)
		case "cond":
			return evalCond(expr, env)
		}
	}

	// Evaluate operator
	op, err := Eval(head, env)
	if err != nil {
		return nil, err
	}

	// Evaluate arguments
	args := make([]*Value, 0, len(expr.List)-1)
	for _, a := range expr.List[1:] {
		v, err := Eval(a, env)
		if err != nil {
			return nil, err
		}
		args = append(args, v)
	}

	// Dispatch builtins
	if op.Type == TypeSymbol && len(op.StrVal) > 10 && op.StrVal[:10] == "__builtin:" {
		name := op.StrVal[10:]
		if fn, ok := builtinRegistry[name]; ok {
			return fn(args, expr)
		}
	}

	// Dispatch lambda calls
	if op.Type == TypeLambda {
		if len(args) != len(op.Params) {
			return nil, fmt.Errorf("%d:%d: wrong number of arguments: expected %d, got %d", expr.Line, expr.Col, len(op.Params), len(args))
		}
		callEnv := NewEnv(op.ClosureEnv)
		for i, param := range op.Params {
			callEnv.Set(param, args[i])
		}
		var result *Value
		for _, bodyExpr := range op.Body {
			var err error
			result, err = Eval(bodyExpr, callEnv)
			if err != nil {
				return nil, err
			}
		}
		return result, nil
	}

	return nil, fmt.Errorf("%d:%d: not a procedure", head.Line, head.Col)
}

func evalAnd(expr *Expr, env *Env) (*Value, error) {
	result := BoolValue(true)
	for _, e := range expr.List[1:] {
		v, err := Eval(e, env)
		if err != nil {
			return nil, err
		}
		if !v.IsTruthy() {
			return v, nil
		}
		result = v
	}
	return result, nil
}

func evalOr(expr *Expr, env *Env) (*Value, error) {
	result := BoolValue(false)
	for _, e := range expr.List[1:] {
		v, err := Eval(e, env)
		if err != nil {
			return nil, err
		}
		if v.IsTruthy() {
			return v, nil
		}
		result = v
	}
	return result, nil
}

// BuiltinFunc is a builtin function type.
type BuiltinFunc func(args []*Value, expr *Expr) (*Value, error)

// MakeDefaultEnv creates an environment with L01 builtins.
func MakeDefaultEnv() *Env {
	env := NewEnv(nil)

	builtins := map[string]BuiltinFunc{
		"+":  builtinAdd,
		"-":  builtinSub,
		"*":  builtinMul,
		"/":  builtinDiv,
		"<":  builtinLt,
		">":  builtinGt,
		"=":  builtinEq,
		"<=": builtinLe,
		"not":      builtinNot,
		"cons":     builtinCons,
		"car":      builtinCar,
		"cdr":      builtinCdr,
		"null?":    builtinNullQ,
		"list":     builtinList,
		"length":   builtinLength,
		"append":   builtinAppend,
		"number?":  builtinNumberQ,
		"string?":  builtinStringQ,
		"boolean?": builtinBooleanQ,
		"pair?":    builtinPairQ,
		"symbol?":  builtinSymbolQ,
	}

	for name, fn := range builtins {
		env.Set(name, &Value{Type: TypeSymbol, StrVal: "__builtin:" + name})
		_ = fn // stored in dispatch below
	}

	// Store builtins in a global map for dispatch
	for name, fn := range builtins {
		builtinRegistry[name] = fn
	}

	return env
}

var builtinRegistry = map[string]BuiltinFunc{}

func init() {
	// Will be populated by MakeDefaultEnv
}

func requireInts(args []*Value, expr *Expr, name string) error {
	for _, a := range args {
		if a.Type != TypeInt {
			return fmt.Errorf("%d:%d: %s: expected number, got %s", expr.Line, expr.Col, name, a.String())
		}
	}
	return nil
}

func builtinAdd(args []*Value, expr *Expr) (*Value, error) {
	if err := requireInts(args, expr, "+"); err != nil {
		return nil, err
	}
	var sum int64
	for _, a := range args {
		sum += a.IntVal
	}
	return IntValue(sum), nil
}

func builtinSub(args []*Value, expr *Expr) (*Value, error) {
	if len(args) == 0 {
		return nil, fmt.Errorf("%d:%d: -: need at least 1 argument", expr.Line, expr.Col)
	}
	if err := requireInts(args, expr, "-"); err != nil {
		return nil, err
	}
	if len(args) == 1 {
		return IntValue(-args[0].IntVal), nil
	}
	result := args[0].IntVal
	for _, a := range args[1:] {
		result -= a.IntVal
	}
	return IntValue(result), nil
}

func builtinMul(args []*Value, expr *Expr) (*Value, error) {
	if err := requireInts(args, expr, "*"); err != nil {
		return nil, err
	}
	result := int64(1)
	for _, a := range args {
		result *= a.IntVal
	}
	return IntValue(result), nil
}

func builtinDiv(args []*Value, expr *Expr) (*Value, error) {
	if len(args) < 2 {
		return nil, fmt.Errorf("%d:%d: /: need at least 2 arguments", expr.Line, expr.Col)
	}
	if err := requireInts(args, expr, "/"); err != nil {
		return nil, err
	}
	result := args[0].IntVal
	for _, a := range args[1:] {
		if a.IntVal == 0 {
			return nil, fmt.Errorf("%d:%d: /: division by zero", expr.Line, expr.Col)
		}
		result /= a.IntVal
	}
	return IntValue(result), nil
}

func builtinLt(args []*Value, expr *Expr) (*Value, error) {
	if len(args) != 2 {
		return nil, fmt.Errorf("%d:%d: <: expected 2 arguments", expr.Line, expr.Col)
	}
	if err := requireInts(args, expr, "<"); err != nil {
		return nil, err
	}
	return BoolValue(args[0].IntVal < args[1].IntVal), nil
}

func builtinGt(args []*Value, expr *Expr) (*Value, error) {
	if len(args) != 2 {
		return nil, fmt.Errorf("%d:%d: >: expected 2 arguments", expr.Line, expr.Col)
	}
	if err := requireInts(args, expr, ">"); err != nil {
		return nil, err
	}
	return BoolValue(args[0].IntVal > args[1].IntVal), nil
}

func builtinEq(args []*Value, expr *Expr) (*Value, error) {
	if len(args) != 2 {
		return nil, fmt.Errorf("%d:%d: =: expected 2 arguments", expr.Line, expr.Col)
	}
	if err := requireInts(args, expr, "="); err != nil {
		return nil, err
	}
	return BoolValue(args[0].IntVal == args[1].IntVal), nil
}

func builtinLe(args []*Value, expr *Expr) (*Value, error) {
	if len(args) != 2 {
		return nil, fmt.Errorf("%d:%d: <=: expected 2 arguments", expr.Line, expr.Col)
	}
	if err := requireInts(args, expr, "<="); err != nil {
		return nil, err
	}
	return BoolValue(args[0].IntVal <= args[1].IntVal), nil
}

func builtinNot(args []*Value, expr *Expr) (*Value, error) {
	if len(args) != 1 {
		return nil, fmt.Errorf("%d:%d: not: expected 1 argument", expr.Line, expr.Col)
	}
	return BoolValue(!args[0].IsTruthy()), nil
}

func evalDefine(expr *Expr, env *Env) (*Value, error) {
	if len(expr.List) < 3 {
		return nil, fmt.Errorf("%d:%d: define: too few arguments", expr.Line, expr.Col)
	}
	target := expr.List[1]
	if target.Type == ExprSymbol {
		// (define x val)
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
		params := make([]string, 0, len(target.List)-1)
		for _, p := range target.List[1:] {
			if p.Type != ExprSymbol {
				return nil, fmt.Errorf("%d:%d: define: parameter must be a symbol", p.Line, p.Col)
			}
			params = append(params, p.StrVal)
		}
		lambda := &Value{
			Type:       TypeLambda,
			Params:     params,
			Body:       expr.List[2:],
			ClosureEnv: env,
		}
		env.Set(name, lambda)
		return Void, nil
	}
	return nil, fmt.Errorf("%d:%d: define: bad syntax", expr.Line, expr.Col)
}

func evalIf(expr *Expr, env *Env) (*Value, error) {
	if len(expr.List) < 3 || len(expr.List) > 4 {
		return nil, fmt.Errorf("%d:%d: if: expected 2 or 3 arguments", expr.Line, expr.Col)
	}
	cond, err := Eval(expr.List[1], env)
	if err != nil {
		return nil, err
	}
	if cond.IsTruthy() {
		return Eval(expr.List[2], env)
	}
	if len(expr.List) == 4 {
		return Eval(expr.List[3], env)
	}
	return Void, nil
}

func evalQuote(expr *Expr) (*Value, error) {
	if len(expr.List) != 2 {
		return nil, fmt.Errorf("%d:%d: quote: expected 1 argument", expr.Line, expr.Col)
	}
	return exprToValue(expr.List[1]), nil
}

func exprToValue(expr *Expr) *Value {
	switch expr.Type {
	case ExprInt:
		return IntValue(expr.IntVal)
	case ExprBool:
		return BoolValue(expr.BoolVal)
	case ExprString:
		return StringValue(expr.StrVal)
	case ExprSymbol:
		return SymbolValue(expr.StrVal)
	case ExprList:
		if len(expr.List) == 0 {
			return Nil
		}
		// Build a proper list from the elements
		result := Nil
		for i := len(expr.List) - 1; i >= 0; i-- {
			result = &Value{Type: TypePair, Car: exprToValue(expr.List[i]), Cdr: result}
		}
		return result
	default:
		return Void
	}
}

func evalLet(expr *Expr, env *Env) (*Value, error) {
	if len(expr.List) < 3 {
		return nil, fmt.Errorf("%d:%d: let: too few arguments", expr.Line, expr.Col)
	}
	bindingExpr := expr.List[1]
	body := expr.List[2:]

	// Named let: (let name ((var init) ...) body...)
	if bindingExpr.Type == ExprSymbol {
		if len(expr.List) < 4 {
			return nil, fmt.Errorf("%d:%d: let: too few arguments", expr.Line, expr.Col)
		}
		loopName := bindingExpr.StrVal
		bindingExpr = expr.List[2]
		body = expr.List[3:]

		params := make([]string, 0, len(bindingExpr.List))
		initVals := make([]*Value, 0, len(bindingExpr.List))
		for _, b := range bindingExpr.List {
			if b.Type != ExprList || len(b.List) != 2 || b.List[0].Type != ExprSymbol {
				return nil, fmt.Errorf("%d:%d: let: bad binding", b.Line, b.Col)
			}
			params = append(params, b.List[0].StrVal)
			v, err := Eval(b.List[1], env)
			if err != nil {
				return nil, err
			}
			initVals = append(initVals, v)
		}
		// Create lambda for the loop
		letEnv := NewEnv(env)
		lambda := &Value{
			Type:       TypeLambda,
			Params:     params,
			Body:       body,
			ClosureEnv: letEnv,
		}
		letEnv.Set(loopName, lambda)
		// Call with initial values
		callEnv := NewEnv(letEnv)
		for i, p := range params {
			callEnv.Set(p, initVals[i])
		}
		var result *Value
		for _, bodyExpr := range body {
			var err error
			result, err = Eval(bodyExpr, callEnv)
			if err != nil {
				return nil, err
			}
		}
		return result, nil
	}

	if bindingExpr.Type != ExprList {
		return nil, fmt.Errorf("%d:%d: let: bindings must be a list", bindingExpr.Line, bindingExpr.Col)
	}
	letEnv := NewEnv(env)
	for _, b := range bindingExpr.List {
		if b.Type != ExprList || len(b.List) != 2 || b.List[0].Type != ExprSymbol {
			return nil, fmt.Errorf("%d:%d: let: bad binding", b.Line, b.Col)
		}
		v, err := Eval(b.List[1], env)
		if err != nil {
			return nil, err
		}
		letEnv.Set(b.List[0].StrVal, v)
	}
	var result *Value
	for _, bodyExpr := range body {
		var err error
		result, err = Eval(bodyExpr, letEnv)
		if err != nil {
			return nil, err
		}
	}
	return result, nil
}

func evalBegin(expr *Expr, env *Env) (*Value, error) {
	var result *Value = Void
	for _, e := range expr.List[1:] {
		var err error
		result, err = Eval(e, env)
		if err != nil {
			return nil, err
		}
	}
	return result, nil
}

func evalCond(expr *Expr, env *Env) (*Value, error) {
	for _, clause := range expr.List[1:] {
		if clause.Type != ExprList || len(clause.List) < 2 {
			return nil, fmt.Errorf("%d:%d: cond: bad clause", clause.Line, clause.Col)
		}
		test := clause.List[0]
		if test.Type == ExprSymbol && test.StrVal == "else" {
			var result *Value
			for _, bodyExpr := range clause.List[1:] {
				var err error
				result, err = Eval(bodyExpr, env)
				if err != nil {
					return nil, err
				}
			}
			return result, nil
		}
		cond, err := Eval(test, env)
		if err != nil {
			return nil, err
		}
		if cond.IsTruthy() {
			var result *Value
			for _, bodyExpr := range clause.List[1:] {
				result, err = Eval(bodyExpr, env)
				if err != nil {
					return nil, err
				}
			}
			return result, nil
		}
	}
	return Void, nil
}

func builtinCons(args []*Value, expr *Expr) (*Value, error) {
	if len(args) != 2 {
		return nil, fmt.Errorf("%d:%d: cons: expected 2 arguments", expr.Line, expr.Col)
	}
	return &Value{Type: TypePair, Car: args[0], Cdr: args[1]}, nil
}

func builtinCar(args []*Value, expr *Expr) (*Value, error) {
	if len(args) != 1 || args[0].Type != TypePair {
		return nil, fmt.Errorf("%d:%d: car: expected a pair", expr.Line, expr.Col)
	}
	return args[0].Car, nil
}

func builtinCdr(args []*Value, expr *Expr) (*Value, error) {
	if len(args) != 1 || args[0].Type != TypePair {
		return nil, fmt.Errorf("%d:%d: cdr: expected a pair", expr.Line, expr.Col)
	}
	return args[0].Cdr, nil
}

func builtinNullQ(args []*Value, expr *Expr) (*Value, error) {
	if len(args) != 1 {
		return nil, fmt.Errorf("%d:%d: null?: expected 1 argument", expr.Line, expr.Col)
	}
	return BoolValue(args[0].Type == TypeNil), nil
}

func builtinList(args []*Value, expr *Expr) (*Value, error) {
	result := Nil
	for i := len(args) - 1; i >= 0; i-- {
		result = &Value{Type: TypePair, Car: args[i], Cdr: result}
	}
	return result, nil
}

func builtinLength(args []*Value, expr *Expr) (*Value, error) {
	if len(args) != 1 {
		return nil, fmt.Errorf("%d:%d: length: expected 1 argument", expr.Line, expr.Col)
	}
	var count int64
	v := args[0]
	for v.Type == TypePair {
		count++
		v = v.Cdr
	}
	return IntValue(count), nil
}

func builtinAppend(args []*Value, expr *Expr) (*Value, error) {
	if len(args) == 0 {
		return Nil, nil
	}
	if len(args) == 1 {
		return args[0], nil
	}
	// Append all lists together
	result := args[len(args)-1]
	for i := len(args) - 2; i >= 0; i-- {
		lst := args[i]
		if lst.Type == TypeNil {
			continue
		}
		// Collect elements of lst
		var elems []*Value
		for lst.Type == TypePair {
			elems = append(elems, lst.Car)
			lst = lst.Cdr
		}
		for j := len(elems) - 1; j >= 0; j-- {
			result = &Value{Type: TypePair, Car: elems[j], Cdr: result}
		}
	}
	return result, nil
}

func builtinNumberQ(args []*Value, expr *Expr) (*Value, error) {
	if len(args) != 1 {
		return nil, fmt.Errorf("%d:%d: number?: expected 1 argument", expr.Line, expr.Col)
	}
	return BoolValue(args[0].Type == TypeInt), nil
}

func builtinStringQ(args []*Value, expr *Expr) (*Value, error) {
	if len(args) != 1 {
		return nil, fmt.Errorf("%d:%d: string?: expected 1 argument", expr.Line, expr.Col)
	}
	return BoolValue(args[0].Type == TypeString), nil
}

func builtinBooleanQ(args []*Value, expr *Expr) (*Value, error) {
	if len(args) != 1 {
		return nil, fmt.Errorf("%d:%d: boolean?: expected 1 argument", expr.Line, expr.Col)
	}
	return BoolValue(args[0].Type == TypeBool), nil
}

func builtinPairQ(args []*Value, expr *Expr) (*Value, error) {
	if len(args) != 1 {
		return nil, fmt.Errorf("%d:%d: pair?: expected 1 argument", expr.Line, expr.Col)
	}
	return BoolValue(args[0].Type == TypePair), nil
}

func builtinSymbolQ(args []*Value, expr *Expr) (*Value, error) {
	if len(args) != 1 {
		return nil, fmt.Errorf("%d:%d: symbol?: expected 1 argument", expr.Line, expr.Col)
	}
	return BoolValue(args[0].Type == TypeSymbol), nil
}

func evalLambda(expr *Expr, env *Env) (*Value, error) {
	if len(expr.List) < 3 {
		return nil, fmt.Errorf("%d:%d: lambda: too few arguments", expr.Line, expr.Col)
	}
	paramList := expr.List[1]
	if paramList.Type != ExprList {
		return nil, fmt.Errorf("%d:%d: lambda: parameters must be a list", paramList.Line, paramList.Col)
	}
	params := make([]string, 0, len(paramList.List))
	for _, p := range paramList.List {
		if p.Type != ExprSymbol {
			return nil, fmt.Errorf("%d:%d: lambda: parameter must be a symbol", p.Line, p.Col)
		}
		params = append(params, p.StrVal)
	}
	return &Value{
		Type:       TypeLambda,
		Params:     params,
		Body:       expr.List[2:],
		ClosureEnv: env,
	}, nil
}
