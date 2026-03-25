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

	// List operations
	env.Set("cons", &Value{Type: TypeSymbol, StrVal: "builtin:cons"})
	env.Set("car", &Value{Type: TypeSymbol, StrVal: "builtin:car"})
	env.Set("cdr", &Value{Type: TypeSymbol, StrVal: "builtin:cdr"})
	env.Set("null?", &Value{Type: TypeSymbol, StrVal: "builtin:null?"})
	env.Set("list", &Value{Type: TypeSymbol, StrVal: "builtin:list"})
	env.Set("length", &Value{Type: TypeSymbol, StrVal: "builtin:length"})
	env.Set("pair?", &Value{Type: TypeSymbol, StrVal: "builtin:pair?"})
	env.Set("append", &Value{Type: TypeSymbol, StrVal: "builtin:append"})

	// Type predicates
	env.Set("boolean?", &Value{Type: TypeSymbol, StrVal: "builtin:boolean?"})
	env.Set("number?", &Value{Type: TypeSymbol, StrVal: "builtin:number?"})
	env.Set("string?", &Value{Type: TypeSymbol, StrVal: "builtin:string?"})
	env.Set("symbol?", &Value{Type: TypeSymbol, StrVal: "builtin:symbol?"})

	// Comparison
	env.Set(">=", &Value{Type: TypeSymbol, StrVal: "builtin:>="})

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

	case "builtin:>=":
		if len(args) != 2 || args[0].Type != TypeInteger || args[1].Type != TypeInteger {
			return nil, fmt.Errorf("%d:%d: '>=' expects two numbers", line, col)
		}
		return BoolValue(args[0].IntVal >= args[1].IntVal), nil

	case "builtin:cons":
		if len(args) != 2 {
			return nil, fmt.Errorf("%d:%d: 'cons' expects 2 arguments", line, col)
		}
		return PairValue(args[0], args[1]), nil

	case "builtin:car":
		if len(args) != 1 || args[0].Type != TypePair {
			return nil, fmt.Errorf("%d:%d: 'car' expects a pair", line, col)
		}
		return args[0].Car, nil

	case "builtin:cdr":
		if len(args) != 1 || args[0].Type != TypePair {
			return nil, fmt.Errorf("%d:%d: 'cdr' expects a pair", line, col)
		}
		return args[0].Cdr, nil

	case "builtin:null?":
		if len(args) != 1 {
			return nil, fmt.Errorf("%d:%d: 'null?' expects 1 argument", line, col)
		}
		return BoolValue(args[0].Type == TypeNull), nil

	case "builtin:list":
		result := Null
		for i := len(args) - 1; i >= 0; i-- {
			result = PairValue(args[i], result)
		}
		return result, nil

	case "builtin:length":
		if len(args) != 1 {
			return nil, fmt.Errorf("%d:%d: 'length' expects 1 argument", line, col)
		}
		var count int64
		cur := args[0]
		for cur.Type == TypePair {
			count++
			cur = cur.Cdr
		}
		if cur.Type != TypeNull {
			return nil, fmt.Errorf("%d:%d: 'length' expects a proper list", line, col)
		}
		return IntValue(count), nil

	case "builtin:pair?":
		if len(args) != 1 {
			return nil, fmt.Errorf("%d:%d: 'pair?' expects 1 argument", line, col)
		}
		return BoolValue(args[0].Type == TypePair), nil

	case "builtin:boolean?":
		if len(args) != 1 {
			return nil, fmt.Errorf("%d:%d: 'boolean?' expects 1 argument", line, col)
		}
		return BoolValue(args[0].Type == TypeBoolean), nil

	case "builtin:number?":
		if len(args) != 1 {
			return nil, fmt.Errorf("%d:%d: 'number?' expects 1 argument", line, col)
		}
		return BoolValue(args[0].Type == TypeInteger), nil

	case "builtin:string?":
		if len(args) != 1 {
			return nil, fmt.Errorf("%d:%d: 'string?' expects 1 argument", line, col)
		}
		return BoolValue(args[0].Type == TypeString), nil

	case "builtin:append":
		if len(args) == 0 {
			return Null, nil
		}
		if len(args) == 1 {
			return args[0], nil
		}
		// append two lists
		if len(args) == 2 {
			a, b := args[0], args[1]
			if a.Type == TypeNull {
				return b, nil
			}
			// Build list from a, then attach b
			var items []*Value
			cur := a
			for cur.Type == TypePair {
				items = append(items, cur.Car)
				cur = cur.Cdr
			}
			result := b
			for i := len(items) - 1; i >= 0; i-- {
				result = PairValue(items[i], result)
			}
			return result, nil
		}
		// Multiple args: fold right
		result := args[len(args)-1]
		for i := len(args) - 2; i >= 0; i-- {
			twoArgs := []*Value{args[i], result}
			var err error
			result, err = callBuiltin("builtin:append", twoArgs, line, col)
			if err != nil {
				return nil, err
			}
		}
		return result, nil

	case "builtin:symbol?":
		if len(args) != 1 {
			return nil, fmt.Errorf("%d:%d: 'symbol?' expects 1 argument", line, col)
		}
		return BoolValue(args[0].Type == TypeSymbol), nil
	}

	return nil, fmt.Errorf("%d:%d: unknown builtin %s", line, col, name)
}
