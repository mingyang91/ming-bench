package ming

import "fmt"

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

func makeGlobalEnv() *Env {
	env := NewEnv(nil)

	// Arithmetic
	env.Set("+", &Value{Type: TypeSymbol, StrVal: "builtin:+"})
	env.Set("-", &Value{Type: TypeSymbol, StrVal: "builtin:-"})
	env.Set("*", &Value{Type: TypeSymbol, StrVal: "builtin:*"})
	env.Set("/", &Value{Type: TypeSymbol, StrVal: "builtin:/"})

	// Comparisons
	env.Set("<", &Value{Type: TypeSymbol, StrVal: "builtin:<"})
	env.Set(">", &Value{Type: TypeSymbol, StrVal: "builtin:>"})
	env.Set("=", &Value{Type: TypeSymbol, StrVal: "builtin:="})
	env.Set("<=", &Value{Type: TypeSymbol, StrVal: "builtin:<="})

	// Logical
	env.Set("not", &Value{Type: TypeSymbol, StrVal: "builtin:not"})

	return env
}

func callBuiltin(name string, args []*Value, line, col int) (*Value, error) {
	switch name {
	case "builtin:+":
		var sum int64
		for _, a := range args {
			if a.Type != TypeInteger {
				return nil, fmt.Errorf("%d:%d: '+' expects numbers", line, col)
			}
			sum += a.IntVal
		}
		return IntValue(sum), nil

	case "builtin:-":
		if len(args) == 0 {
			return nil, fmt.Errorf("%d:%d: '-' requires at least one argument", line, col)
		}
		if args[0].Type != TypeInteger {
			return nil, fmt.Errorf("%d:%d: '-' expects numbers", line, col)
		}
		if len(args) == 1 {
			return IntValue(-args[0].IntVal), nil
		}
		result := args[0].IntVal
		for _, a := range args[1:] {
			if a.Type != TypeInteger {
				return nil, fmt.Errorf("%d:%d: '-' expects numbers", line, col)
			}
			result -= a.IntVal
		}
		return IntValue(result), nil

	case "builtin:*":
		var product int64 = 1
		for _, a := range args {
			if a.Type != TypeInteger {
				return nil, fmt.Errorf("%d:%d: '*' expects numbers", line, col)
			}
			product *= a.IntVal
		}
		return IntValue(product), nil

	case "builtin:/":
		if len(args) < 2 {
			return nil, fmt.Errorf("%d:%d: '/' requires at least two arguments", line, col)
		}
		if args[0].Type != TypeInteger {
			return nil, fmt.Errorf("%d:%d: '/' expects numbers", line, col)
		}
		result := args[0].IntVal
		for _, a := range args[1:] {
			if a.Type != TypeInteger {
				return nil, fmt.Errorf("%d:%d: '/' expects numbers", line, col)
			}
			if a.IntVal == 0 {
				return nil, fmt.Errorf("%d:%d: division by zero", line, col)
			}
			result /= a.IntVal
		}
		return IntValue(result), nil

	case "builtin:<":
		if len(args) != 2 || args[0].Type != TypeInteger || args[1].Type != TypeInteger {
			return nil, fmt.Errorf("%d:%d: '<' expects two numbers", line, col)
		}
		return BoolValue(args[0].IntVal < args[1].IntVal), nil

	case "builtin:>":
		if len(args) != 2 || args[0].Type != TypeInteger || args[1].Type != TypeInteger {
			return nil, fmt.Errorf("%d:%d: '>' expects two numbers", line, col)
		}
		return BoolValue(args[0].IntVal > args[1].IntVal), nil

	case "builtin:=":
		if len(args) != 2 || args[0].Type != TypeInteger || args[1].Type != TypeInteger {
			return nil, fmt.Errorf("%d:%d: '=' expects two numbers", line, col)
		}
		return BoolValue(args[0].IntVal == args[1].IntVal), nil

	case "builtin:<=":
		if len(args) != 2 || args[0].Type != TypeInteger || args[1].Type != TypeInteger {
			return nil, fmt.Errorf("%d:%d: '<=' expects two numbers", line, col)
		}
		return BoolValue(args[0].IntVal <= args[1].IntVal), nil

	case "builtin:not":
		if len(args) != 1 {
			return nil, fmt.Errorf("%d:%d: 'not' expects one argument", line, col)
		}
		return BoolValue(!isTruthy(args[0])), nil
	}

	return nil, fmt.Errorf("%d:%d: unknown builtin %s", line, col, name)
}
