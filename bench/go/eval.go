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
		"not": builtinNot,
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
