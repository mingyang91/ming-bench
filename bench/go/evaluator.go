package ming

import (
	"fmt"
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

	env := defaultEnv()
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
	return "", "", &EvalError{Message: "not implemented"}
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
	fn, ok := op.(*BuiltinFunc)
	if !ok {
		return nil, &EvalError{Message: fmt.Sprintf("%d:%d: not a procedure", head.Line, head.Col)}
	}
	result, err := fn.Fn(args)
	if err != nil {
		// wrap with position if not already positioned
		return nil, &EvalError{Message: fmt.Sprintf("%d:%d: %s", expr.Line, expr.Col, err.Error())}
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

func defaultEnv() *Env {
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
