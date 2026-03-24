package ming

import "fmt"

// Env is a variable environment with lexical scoping.
type Env struct {
	bindings map[string]Value
	parent   *Env
}

func newEnv(parent *Env) *Env {
	return &Env{bindings: make(map[string]Value), parent: parent}
}

func (e *Env) get(name string) (Value, bool) {
	if v, ok := e.bindings[name]; ok {
		return v, true
	}
	if e.parent != nil {
		return e.parent.get(name)
	}
	return nil, false
}

func (e *Env) set(name string, val Value) {
	e.bindings[name] = val
}

// BuiltinFunc is a built-in procedure.
type BuiltinFunc struct {
	Name string
	Fn   func(args []Value) (Value, error)
}

func (b *BuiltinFunc) String() string {
	return fmt.Sprintf("#<procedure %s>", b.Name)
}

func evalExpr(expr Expr, env *Env) (Value, error) {
	switch e := expr.(type) {
	case *NumberExpr:
		return &IntVal{Val: e.Val}, nil
	case *StringExpr:
		return &StringVal{Val: e.Val}, nil
	case *BoolExpr:
		return &BoolVal{Val: e.Val}, nil
	case *SymbolExpr:
		v, ok := env.get(e.Name)
		if !ok {
			return nil, &EvalError{Message: fmt.Sprintf("%d:%d: unbound variable: %s", e.Line, e.Col, e.Name)}
		}
		return v, nil
	case *ListExpr:
		if len(e.Elems) == 0 {
			return &NilVal{}, nil
		}
		// check for special forms
		if sym, ok := e.Elems[0].(*SymbolExpr); ok {
			switch sym.Name {
			case "and":
				return evalAnd(e.Elems[1:], env)
			case "or":
				return evalOr(e.Elems[1:], env)
			case "not":
				if len(e.Elems) != 2 {
					return nil, &EvalError{Message: fmt.Sprintf("%d:%d: not requires 1 argument", sym.Line, sym.Col)}
				}
				v, err := evalExpr(e.Elems[1], env)
				if err != nil {
					return nil, err
				}
				return &BoolVal{Val: !isTruthy(v)}, nil
			}
		}

		// function application
		fn, err := evalExpr(e.Elems[0], env)
		if err != nil {
			return nil, err
		}
		args := make([]Value, len(e.Elems)-1)
		for i, a := range e.Elems[1:] {
			args[i], err = evalExpr(a, env)
			if err != nil {
				return nil, err
			}
		}
		bf, ok := fn.(*BuiltinFunc)
		if !ok {
			line, col := e.Elems[0].pos()
			return nil, &EvalError{Message: fmt.Sprintf("%d:%d: not a procedure", line, col)}
		}
		return bf.Fn(args)
	}
	return nil, &EvalError{Message: "unknown expression type"}
}

func evalAnd(exprs []Expr, env *Env) (Value, error) {
	var result Value = &BoolVal{Val: true}
	for _, e := range exprs {
		v, err := evalExpr(e, env)
		if err != nil {
			return nil, err
		}
		if !isTruthy(v) {
			return v, nil
		}
		result = v
	}
	return result, nil
}

func evalOr(exprs []Expr, env *Env) (Value, error) {
	var result Value = &BoolVal{Val: false}
	for _, e := range exprs {
		v, err := evalExpr(e, env)
		if err != nil {
			return nil, err
		}
		if isTruthy(v) {
			return v, nil
		}
		result = v
	}
	return result, nil
}

func makeGlobalEnv() *Env {
	env := newEnv(nil)

	env.set("+", &BuiltinFunc{Name: "+", Fn: func(args []Value) (Value, error) {
		var sum int64
		for _, a := range args {
			n, ok := a.(*IntVal)
			if !ok {
				return nil, &EvalError{Message: "+: not a number"}
			}
			sum += n.Val
		}
		return &IntVal{Val: sum}, nil
	}})

	env.set("-", &BuiltinFunc{Name: "-", Fn: func(args []Value) (Value, error) {
		if len(args) == 0 {
			return nil, &EvalError{Message: "-: need at least 1 argument"}
		}
		first, ok := args[0].(*IntVal)
		if !ok {
			return nil, &EvalError{Message: "-: not a number"}
		}
		if len(args) == 1 {
			return &IntVal{Val: -first.Val}, nil
		}
		result := first.Val
		for _, a := range args[1:] {
			n, ok := a.(*IntVal)
			if !ok {
				return nil, &EvalError{Message: "-: not a number"}
			}
			result -= n.Val
		}
		return &IntVal{Val: result}, nil
	}})

	env.set("*", &BuiltinFunc{Name: "*", Fn: func(args []Value) (Value, error) {
		var product int64 = 1
		for _, a := range args {
			n, ok := a.(*IntVal)
			if !ok {
				return nil, &EvalError{Message: "*: not a number"}
			}
			product *= n.Val
		}
		return &IntVal{Val: product}, nil
	}})

	env.set("/", &BuiltinFunc{Name: "/", Fn: func(args []Value) (Value, error) {
		if len(args) < 2 {
			return nil, &EvalError{Message: "/: need at least 2 arguments"}
		}
		first, ok := args[0].(*IntVal)
		if !ok {
			return nil, &EvalError{Message: "/: not a number"}
		}
		result := first.Val
		for _, a := range args[1:] {
			n, ok := a.(*IntVal)
			if !ok {
				return nil, &EvalError{Message: "/: not a number"}
			}
			if n.Val == 0 {
				return nil, &EvalError{Message: "/: division by zero"}
			}
			result /= n.Val
		}
		return &IntVal{Val: result}, nil
	}})

	// Comparisons
	env.set("<", &BuiltinFunc{Name: "<", Fn: makeCompare("<", func(a, b int64) bool { return a < b })})
	env.set(">", &BuiltinFunc{Name: ">", Fn: makeCompare(">", func(a, b int64) bool { return a > b })})
	env.set("=", &BuiltinFunc{Name: "=", Fn: makeCompare("=", func(a, b int64) bool { return a == b })})
	env.set("<=", &BuiltinFunc{Name: "<=", Fn: makeCompare("<=", func(a, b int64) bool { return a <= b })})
	env.set(">=", &BuiltinFunc{Name: ">=", Fn: makeCompare(">=", func(a, b int64) bool { return a >= b })})

	return env
}

func makeCompare(name string, op func(int64, int64) bool) func([]Value) (Value, error) {
	return func(args []Value) (Value, error) {
		if len(args) < 2 {
			return nil, &EvalError{Message: fmt.Sprintf("%s: need at least 2 arguments", name)}
		}
		for i := 0; i < len(args)-1; i++ {
			a, ok := args[i].(*IntVal)
			if !ok {
				return nil, &EvalError{Message: fmt.Sprintf("%s: not a number", name)}
			}
			b, ok := args[i+1].(*IntVal)
			if !ok {
				return nil, &EvalError{Message: fmt.Sprintf("%s: not a number", name)}
			}
			if !op(a.Val, b.Val) {
				return &BoolVal{Val: false}, nil
			}
		}
		return &BoolVal{Val: true}, nil
	}
}

// EvalStr evaluates one or more Scheme expressions and returns the string
// representation of the last result.
func EvalStr(input string) (string, error) {
	exprs, err := parse(input)
	if err != nil {
		return "", &EvalError{Message: err.Error()}
	}
	if len(exprs) == 0 {
		return "", nil
	}
	env := makeGlobalEnv()
	var last Value
	for _, expr := range exprs {
		last, err = evalExpr(expr, env)
		if err != nil {
			return "", err
		}
	}
	if _, ok := last.(*VoidVal); ok {
		return "", nil
	}
	return last.String(), nil
}

// EvalStrWithOutput evaluates Scheme expressions and returns both the result
// string and any captured output from display/write/newline.
func EvalStrWithOutput(input string) (result string, output string, err error) {
	r, err := EvalStr(input)
	return r, "", err
}
