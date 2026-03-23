package ming

import "fmt"

// Environment holds variable bindings.
type Env struct {
	bindings map[string]*Value
	parent   *Env
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

func (e *Env) set(name string, v *Value) {
	e.bindings[name] = v
}

// listToSlice converts a Scheme list to a Go slice.
func listToSlice(v *Value) []*Value {
	var result []*Value
	cur := v
	for cur.Type == TypePair {
		result = append(result, cur.Car)
		cur = cur.Cdr
	}
	return result
}

// eval evaluates a single expression in the given environment.
func eval(expr *Value, env *Env) (*Value, error) {
	switch expr.Type {
	case TypeInteger, TypeBoolean, TypeString:
		return expr, nil
	case TypeSymbol:
		v, ok := env.get(expr.Str)
		if !ok {
			return nil, &EvalError{Message: fmt.Sprintf("unbound variable: %s", expr.Str)}
		}
		return v, nil
	case TypePair:
		return evalList(expr, env)
	case TypeNull:
		return nil, &EvalError{Message: "empty application"}
	default:
		return nil, &EvalError{Message: "cannot evaluate"}
	}
}

func evalList(expr *Value, env *Env) (*Value, error) {
	head := expr.Car
	args := expr.Cdr

	// Special forms
	if head.Type == TypeSymbol {
		switch head.Str {
		case "and":
			return evalAnd(args, env)
		case "or":
			return evalOr(args, env)
		}
	}

	// Function application
	fn, err := eval(head, env)
	if err != nil {
		return nil, err
	}

	// Evaluate arguments
	argList := listToSlice(args)
	evalArgs := make([]*Value, len(argList))
	for i, a := range argList {
		v, err := eval(a, env)
		if err != nil {
			return nil, err
		}
		evalArgs[i] = v
	}

	return apply(fn, evalArgs)
}

func evalAnd(args *Value, env *Env) (*Value, error) {
	result := makeBool(true)
	cur := args
	for cur.Type == TypePair {
		v, err := eval(cur.Car, env)
		if err != nil {
			return nil, err
		}
		if !isTruthy(v) {
			return v, nil
		}
		result = v
		cur = cur.Cdr
	}
	return result, nil
}

func evalOr(args *Value, env *Env) (*Value, error) {
	result := makeBool(false)
	cur := args
	for cur.Type == TypePair {
		v, err := eval(cur.Car, env)
		if err != nil {
			return nil, err
		}
		if isTruthy(v) {
			return v, nil
		}
		result = v
		cur = cur.Cdr
	}
	return result, nil
}

// apply calls a builtin function with evaluated arguments.
func apply(fn *Value, args []*Value) (*Value, error) {
	if fn.Type != TypeSymbol {
		return nil, &EvalError{Message: fmt.Sprintf("not a procedure: %s", fn.Display())}
	}
	return applyBuiltin(fn.Str, args)
}

func applyBuiltin(name string, args []*Value) (*Value, error) {
	switch name {
	case "+":
		sum := int64(0)
		for _, a := range args {
			if a.Type != TypeInteger {
				return nil, &EvalError{Message: "'+' expects numbers"}
			}
			sum += a.Int
		}
		return makeInt(sum), nil

	case "-":
		if len(args) == 0 {
			return nil, &EvalError{Message: "'-' expects at least one argument"}
		}
		if args[0].Type != TypeInteger {
			return nil, &EvalError{Message: "'-' expects numbers"}
		}
		if len(args) == 1 {
			return makeInt(-args[0].Int), nil
		}
		result := args[0].Int
		for _, a := range args[1:] {
			if a.Type != TypeInteger {
				return nil, &EvalError{Message: "'-' expects numbers"}
			}
			result -= a.Int
		}
		return makeInt(result), nil

	case "*":
		product := int64(1)
		for _, a := range args {
			if a.Type != TypeInteger {
				return nil, &EvalError{Message: "'*' expects numbers"}
			}
			product *= a.Int
		}
		return makeInt(product), nil

	case "/":
		if len(args) < 2 {
			return nil, &EvalError{Message: "'/' expects at least two arguments"}
		}
		if args[0].Type != TypeInteger {
			return nil, &EvalError{Message: "'/' expects numbers"}
		}
		result := args[0].Int
		for _, a := range args[1:] {
			if a.Type != TypeInteger {
				return nil, &EvalError{Message: "'/' expects numbers"}
			}
			if a.Int == 0 {
				return nil, &EvalError{Message: "division by zero"}
			}
			result /= a.Int
		}
		return makeInt(result), nil

	case "<":
		return compareInts(args, func(a, b int64) bool { return a < b })
	case ">":
		return compareInts(args, func(a, b int64) bool { return a > b })
	case "=":
		return compareInts(args, func(a, b int64) bool { return a == b })
	case "<=":
		return compareInts(args, func(a, b int64) bool { return a <= b })
	case ">=":
		return compareInts(args, func(a, b int64) bool { return a >= b })

	case "not":
		if len(args) != 1 {
			return nil, &EvalError{Message: "'not' expects exactly one argument"}
		}
		return makeBool(!isTruthy(args[0])), nil

	default:
		return nil, &EvalError{Message: fmt.Sprintf("unknown procedure: %s", name)}
	}
}

func compareInts(args []*Value, cmp func(int64, int64) bool) (*Value, error) {
	if len(args) < 2 {
		return nil, &EvalError{Message: "comparison expects at least two arguments"}
	}
	for _, a := range args {
		if a.Type != TypeInteger {
			return nil, &EvalError{Message: "comparison expects numbers"}
		}
	}
	for i := 0; i < len(args)-1; i++ {
		if !cmp(args[i].Int, args[i+1].Int) {
			return makeBool(false), nil
		}
	}
	return makeBool(true), nil
}

// builtinEnv creates the top-level environment with builtin procedure names.
func builtinEnv() *Env {
	env := newEnv(nil)
	builtins := []string{"+", "-", "*", "/", "<", ">", "=", "<=", ">=", "not"}
	for _, name := range builtins {
		env.set(name, makeSymbol(name))
	}
	return env
}

// EvalStr evaluates one or more Scheme expressions and returns the string
// representation of the last result.
func EvalStr(input string) (string, error) {
	exprs, err := readAll(input)
	if err != nil {
		return "", &EvalError{Message: err.Error()}
	}
	if len(exprs) == 0 {
		return "", &EvalError{Message: "no expressions"}
	}

	env := builtinEnv()
	var result *Value
	for _, expr := range exprs {
		result, err = eval(expr, env)
		if err != nil {
			return "", err
		}
	}

	if result.Type == TypeVoid {
		return "", nil
	}
	return result.Display(), nil
}

// EvalStrWithOutput evaluates Scheme expressions and returns both the result
// string and any captured output from display/write/newline.
func EvalStrWithOutput(input string) (result string, output string, err error) {
	r, err := EvalStr(input)
	return r, "", err
}
