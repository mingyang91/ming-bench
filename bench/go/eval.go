package ming

import "fmt"

// BuiltinFunc is a built-in procedure.
type BuiltinFunc struct {
	Name string
	Fn   func(args []Value) (Value, error)
}

func (b *BuiltinFunc) String() string {
	return fmt.Sprintf("#<procedure %s>", b.Name)
}

func eval(expr Expr, env *Env) (Value, error) {
	switch e := expr.(type) {
	case *NumberExpr:
		return &IntVal{Val: e.Val}, nil
	case *BoolExpr:
		return &BoolVal{Val: e.Val}, nil
	case *StringExpr:
		return &StringVal{Val: e.Val}, nil
	case *SymbolExpr:
		v, ok := env.get(e.Name)
		if !ok {
			return nil, &EvalError{Message: fmt.Sprintf("%d:%d: unbound variable: %s", e.Ln, e.Cl, e.Name)}
		}
		return v, nil
	case *ListExpr:
		if len(e.Items) == 0 {
			return &NilVal{}, nil
		}
		return evalList(e, env)
	default:
		return nil, &EvalError{Message: "unknown expression type"}
	}
}

func evalList(e *ListExpr, env *Env) (Value, error) {
	// Check for special forms
	if sym, ok := e.Items[0].(*SymbolExpr); ok {
		switch sym.Name {
		case "and":
			return evalAnd(e, env)
		case "or":
			return evalOr(e, env)
		case "define":
			return evalDefine(e, env)
		case "if":
			return evalIf(e, env)
		case "quote":
			if len(e.Items) != 2 {
				return nil, &EvalError{Message: fmt.Sprintf("%d:%d: quote: requires exactly 1 argument", e.Ln, e.Cl)}
			}
			return quoteExpr(e.Items[1]), nil
		case "lambda":
			return evalLambda(e, env)
		case "begin":
			return evalBegin(e, env)
		case "cond":
			return evalCond(e, env)
		case "let":
			return evalLet(e, env)
		}
	}

	// Function application
	fn, err := eval(e.Items[0], env)
	if err != nil {
		return nil, err
	}

	args := make([]Value, 0, len(e.Items)-1)
	for _, arg := range e.Items[1:] {
		v, err := eval(arg, env)
		if err != nil {
			return nil, err
		}
		args = append(args, v)
	}

	switch f := fn.(type) {
	case *BuiltinFunc:
		result, err := f.Fn(args)
		if err != nil {
			if ee, ok := err.(*EvalError); ok {
				// Prepend position if not already present
				if len(ee.Message) == 0 || ee.Message[0] < '0' || ee.Message[0] > '9' {
					ee.Message = fmt.Sprintf("%d:%d: %s", e.Ln, e.Cl, ee.Message)
				}
			}
			return nil, err
		}
		return result, nil
	case *LambdaVal:
		return applyLambda(f, args, e.Ln, e.Cl)
	default:
		return nil, &EvalError{Message: fmt.Sprintf("%d:%d: not a procedure: %s", e.Ln, e.Cl, fn.String())}
	}
}

func evalAnd(e *ListExpr, env *Env) (Value, error) {
	var result Value = &BoolVal{Val: true}
	for _, item := range e.Items[1:] {
		v, err := eval(item, env)
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

func evalOr(e *ListExpr, env *Env) (Value, error) {
	var result Value = &BoolVal{Val: false}
	for _, item := range e.Items[1:] {
		v, err := eval(item, env)
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

func evalDefine(e *ListExpr, env *Env) (Value, error) {
	if len(e.Items) < 3 {
		return nil, &EvalError{Message: fmt.Sprintf("%d:%d: define: requires at least 2 arguments", e.Ln, e.Cl)}
	}
	switch target := e.Items[1].(type) {
	case *SymbolExpr:
		// (define x expr)
		val, err := eval(e.Items[2], env)
		if err != nil {
			return nil, err
		}
		env.set(target.Name, val)
		return &VoidVal{}, nil
	case *ListExpr:
		// (define (f params...) body...)
		if len(target.Items) == 0 {
			return nil, &EvalError{Message: fmt.Sprintf("%d:%d: define: empty name list", e.Ln, e.Cl)}
		}
		nameSym, ok := target.Items[0].(*SymbolExpr)
		if !ok {
			return nil, &EvalError{Message: fmt.Sprintf("%d:%d: define: expected symbol", e.Ln, e.Cl)}
		}
		params := make([]string, 0, len(target.Items)-1)
		for _, p := range target.Items[1:] {
			ps, ok := p.(*SymbolExpr)
			if !ok {
				return nil, &EvalError{Message: fmt.Sprintf("%d:%d: define: expected symbol in params", e.Ln, e.Cl)}
			}
			params = append(params, ps.Name)
		}
		lambda := &LambdaVal{Params: params, Body: e.Items[2:], Env: env}
		env.set(nameSym.Name, lambda)
		return &VoidVal{}, nil
	default:
		return nil, &EvalError{Message: fmt.Sprintf("%d:%d: define: expected symbol or list", e.Ln, e.Cl)}
	}
}

func evalIf(e *ListExpr, env *Env) (Value, error) {
	if len(e.Items) < 3 || len(e.Items) > 4 {
		return nil, &EvalError{Message: fmt.Sprintf("%d:%d: if: requires 2 or 3 arguments", e.Ln, e.Cl)}
	}
	cond, err := eval(e.Items[1], env)
	if err != nil {
		return nil, err
	}
	if isTruthy(cond) {
		return eval(e.Items[2], env)
	}
	if len(e.Items) == 4 {
		return eval(e.Items[3], env)
	}
	return &VoidVal{}, nil
}

func evalLambda(e *ListExpr, env *Env) (Value, error) {
	if len(e.Items) < 3 {
		return nil, &EvalError{Message: fmt.Sprintf("%d:%d: lambda: requires params and body", e.Ln, e.Cl)}
	}
	paramList, ok := e.Items[1].(*ListExpr)
	if !ok {
		return nil, &EvalError{Message: fmt.Sprintf("%d:%d: lambda: expected parameter list", e.Ln, e.Cl)}
	}
	params := make([]string, 0, len(paramList.Items))
	for _, p := range paramList.Items {
		ps, ok := p.(*SymbolExpr)
		if !ok {
			return nil, &EvalError{Message: fmt.Sprintf("%d:%d: lambda: expected symbol in params", e.Ln, e.Cl)}
		}
		params = append(params, ps.Name)
	}
	return &LambdaVal{Params: params, Body: e.Items[2:], Env: env}, nil
}

func applyLambda(fn *LambdaVal, args []Value, ln, cl int) (Value, error) {
	if len(args) != len(fn.Params) {
		return nil, &EvalError{Message: fmt.Sprintf("%d:%d: wrong number of arguments: expected %d, got %d", ln, cl, len(fn.Params), len(args))}
	}
	childEnv := newEnv(fn.Env)
	for i, p := range fn.Params {
		childEnv.set(p, args[i])
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
}

func quoteExpr(expr Expr) Value {
	switch e := expr.(type) {
	case *NumberExpr:
		return &IntVal{Val: e.Val}
	case *BoolExpr:
		return &BoolVal{Val: e.Val}
	case *StringExpr:
		return &StringVal{Val: e.Val}
	case *SymbolExpr:
		return &SymbolVal{Name: e.Name}
	case *ListExpr:
		if len(e.Items) == 0 {
			return &NilVal{}
		}
		// Build a proper list from the items
		result := Value(&NilVal{})
		for i := len(e.Items) - 1; i >= 0; i-- {
			result = &PairVal{Car: quoteExpr(e.Items[i]), Cdr: result}
		}
		return result
	default:
		return &NilVal{}
	}
}

func makeGlobalEnv() *Env {
	env := newEnv(nil)

	// Arithmetic
	env.set("+", &BuiltinFunc{Name: "+", Fn: builtinAdd})
	env.set("-", &BuiltinFunc{Name: "-", Fn: builtinSub})
	env.set("*", &BuiltinFunc{Name: "*", Fn: builtinMul})
	env.set("/", &BuiltinFunc{Name: "/", Fn: builtinDiv})

	// Comparison
	env.set("<", &BuiltinFunc{Name: "<", Fn: builtinLT})
	env.set(">", &BuiltinFunc{Name: ">", Fn: builtinGT})
	env.set("=", &BuiltinFunc{Name: "=", Fn: builtinEq})
	env.set("<=", &BuiltinFunc{Name: "<=", Fn: builtinLE})

	// Logic
	env.set("not", &BuiltinFunc{Name: "not", Fn: builtinNot})

	// List operations
	env.set("cons", &BuiltinFunc{Name: "cons", Fn: builtinCons})
	env.set("car", &BuiltinFunc{Name: "car", Fn: builtinCar})
	env.set("cdr", &BuiltinFunc{Name: "cdr", Fn: builtinCdr})
	env.set("null?", &BuiltinFunc{Name: "null?", Fn: builtinNullQ})
	env.set("pair?", &BuiltinFunc{Name: "pair?", Fn: builtinPairQ})
	env.set("list", &BuiltinFunc{Name: "list", Fn: builtinList})
	env.set("length", &BuiltinFunc{Name: "length", Fn: builtinLength})
	env.set("append", &BuiltinFunc{Name: "append", Fn: builtinAppend})

	// Type predicates
	env.set("number?", &BuiltinFunc{Name: "number?", Fn: builtinNumberQ})
	env.set("boolean?", &BuiltinFunc{Name: "boolean?", Fn: builtinBooleanQ})
	env.set("string?", &BuiltinFunc{Name: "string?", Fn: builtinStringQ})
	env.set("symbol?", &BuiltinFunc{Name: "symbol?", Fn: builtinSymbolQ})

	return env
}

func builtinAdd(args []Value) (Value, error) {
	var sum int64
	for _, a := range args {
		n, ok := a.(*IntVal)
		if !ok {
			return nil, &EvalError{Message: fmt.Sprintf("+: not a number: %s", a.String())}
		}
		sum += n.Val
	}
	return &IntVal{Val: sum}, nil
}

func builtinSub(args []Value) (Value, error) {
	if len(args) == 0 {
		return nil, &EvalError{Message: "-: requires at least 1 argument"}
	}
	first, ok := args[0].(*IntVal)
	if !ok {
		return nil, &EvalError{Message: fmt.Sprintf("-: not a number: %s", args[0].String())}
	}
	if len(args) == 1 {
		return &IntVal{Val: -first.Val}, nil
	}
	result := first.Val
	for _, a := range args[1:] {
		n, ok := a.(*IntVal)
		if !ok {
			return nil, &EvalError{Message: fmt.Sprintf("-: not a number: %s", a.String())}
		}
		result -= n.Val
	}
	return &IntVal{Val: result}, nil
}

func builtinMul(args []Value) (Value, error) {
	result := int64(1)
	for _, a := range args {
		n, ok := a.(*IntVal)
		if !ok {
			return nil, &EvalError{Message: fmt.Sprintf("*: not a number: %s", a.String())}
		}
		result *= n.Val
	}
	return &IntVal{Val: result}, nil
}

func builtinDiv(args []Value) (Value, error) {
	if len(args) < 2 {
		return nil, &EvalError{Message: "/: requires at least 2 arguments"}
	}
	first, ok := args[0].(*IntVal)
	if !ok {
		return nil, &EvalError{Message: fmt.Sprintf("/: not a number: %s", args[0].String())}
	}
	result := first.Val
	for _, a := range args[1:] {
		n, ok := a.(*IntVal)
		if !ok {
			return nil, &EvalError{Message: fmt.Sprintf("/: not a number: %s", a.String())}
		}
		if n.Val == 0 {
			return nil, &EvalError{Message: "/: division by zero"}
		}
		result /= n.Val
	}
	return &IntVal{Val: result}, nil
}

func numericCompare(name string, args []Value, cmp func(a, b int64) bool) (Value, error) {
	if len(args) < 2 {
		return nil, &EvalError{Message: fmt.Sprintf("%s: requires at least 2 arguments", name)}
	}
	prev, ok := args[0].(*IntVal)
	if !ok {
		return nil, &EvalError{Message: fmt.Sprintf("%s: not a number: %s", name, args[0].String())}
	}
	for _, a := range args[1:] {
		n, ok := a.(*IntVal)
		if !ok {
			return nil, &EvalError{Message: fmt.Sprintf("%s: not a number: %s", name, a.String())}
		}
		if !cmp(prev.Val, n.Val) {
			return &BoolVal{Val: false}, nil
		}
		prev = n
	}
	return &BoolVal{Val: true}, nil
}

func builtinLT(args []Value) (Value, error) {
	return numericCompare("<", args, func(a, b int64) bool { return a < b })
}

func builtinGT(args []Value) (Value, error) {
	return numericCompare(">", args, func(a, b int64) bool { return a > b })
}

func builtinEq(args []Value) (Value, error) {
	return numericCompare("=", args, func(a, b int64) bool { return a == b })
}

func builtinLE(args []Value) (Value, error) {
	return numericCompare("<=", args, func(a, b int64) bool { return a <= b })
}

func evalBegin(e *ListExpr, env *Env) (Value, error) {
	var result Value = &VoidVal{}
	var err error
	for _, item := range e.Items[1:] {
		result, err = eval(item, env)
		if err != nil {
			return nil, err
		}
	}
	return result, nil
}

func evalCond(e *ListExpr, env *Env) (Value, error) {
	for _, clause := range e.Items[1:] {
		cl, ok := clause.(*ListExpr)
		if !ok || len(cl.Items) < 2 {
			return nil, &EvalError{Message: "cond: bad clause"}
		}
		// Check for else
		if sym, ok := cl.Items[0].(*SymbolExpr); ok && sym.Name == "else" {
			var result Value
			var err error
			for _, expr := range cl.Items[1:] {
				result, err = eval(expr, env)
				if err != nil {
					return nil, err
				}
			}
			return result, nil
		}
		test, err := eval(cl.Items[0], env)
		if err != nil {
			return nil, err
		}
		if isTruthy(test) {
			var result Value
			for _, expr := range cl.Items[1:] {
				result, err = eval(expr, env)
				if err != nil {
					return nil, err
				}
			}
			return result, nil
		}
	}
	return &VoidVal{}, nil
}

func evalLet(e *ListExpr, env *Env) (Value, error) {
	if len(e.Items) < 3 {
		return nil, &EvalError{Message: "let: requires bindings and body"}
	}

	// Named let: (let name ((var init) ...) body ...)
	if sym, ok := e.Items[1].(*SymbolExpr); ok {
		if len(e.Items) < 4 {
			return nil, &EvalError{Message: "let: named let requires bindings and body"}
		}
		bindingsList, ok := e.Items[2].(*ListExpr)
		if !ok {
			return nil, &EvalError{Message: "let: expected bindings list"}
		}
		params := make([]string, 0, len(bindingsList.Items))
		initVals := make([]Value, 0, len(bindingsList.Items))
		for _, b := range bindingsList.Items {
			bl, ok := b.(*ListExpr)
			if !ok || len(bl.Items) != 2 {
				return nil, &EvalError{Message: "let: bad binding"}
			}
			ps, ok := bl.Items[0].(*SymbolExpr)
			if !ok {
				return nil, &EvalError{Message: "let: expected symbol in binding"}
			}
			val, err := eval(bl.Items[1], env)
			if err != nil {
				return nil, err
			}
			params = append(params, ps.Name)
			initVals = append(initVals, val)
		}
		// Create lambda for the named let
		lambda := &LambdaVal{Params: params, Body: e.Items[3:], Env: env}
		// Create env where the name is bound to the lambda
		letEnv := newEnv(env)
		letEnv.set(sym.Name, lambda)
		lambda.Env = letEnv
		return applyLambda(lambda, initVals, e.Ln, e.Cl)
	}

	bindings, ok := e.Items[1].(*ListExpr)
	if !ok {
		return nil, &EvalError{Message: "let: expected bindings list"}
	}
	childEnv := newEnv(env)
	for _, b := range bindings.Items {
		bl, ok := b.(*ListExpr)
		if !ok || len(bl.Items) != 2 {
			return nil, &EvalError{Message: "let: bad binding"}
		}
		sym, ok := bl.Items[0].(*SymbolExpr)
		if !ok {
			return nil, &EvalError{Message: "let: expected symbol in binding"}
		}
		val, err := eval(bl.Items[1], env)
		if err != nil {
			return nil, err
		}
		childEnv.set(sym.Name, val)
	}
	var result Value
	var err error
	for _, bodyExpr := range e.Items[2:] {
		result, err = eval(bodyExpr, childEnv)
		if err != nil {
			return nil, err
		}
	}
	return result, nil
}

func builtinCons(args []Value) (Value, error) {
	if len(args) != 2 {
		return nil, &EvalError{Message: "cons: requires exactly 2 arguments"}
	}
	return &PairVal{Car: args[0], Cdr: args[1]}, nil
}

func builtinCar(args []Value) (Value, error) {
	if len(args) != 1 {
		return nil, &EvalError{Message: "car: requires exactly 1 argument"}
	}
	p, ok := args[0].(*PairVal)
	if !ok {
		return nil, &EvalError{Message: fmt.Sprintf("car: not a pair: %s", args[0].String())}
	}
	return p.Car, nil
}

func builtinCdr(args []Value) (Value, error) {
	if len(args) != 1 {
		return nil, &EvalError{Message: "cdr: requires exactly 1 argument"}
	}
	p, ok := args[0].(*PairVal)
	if !ok {
		return nil, &EvalError{Message: fmt.Sprintf("cdr: not a pair: %s", args[0].String())}
	}
	return p.Cdr, nil
}

func builtinNullQ(args []Value) (Value, error) {
	if len(args) != 1 {
		return nil, &EvalError{Message: "null?: requires exactly 1 argument"}
	}
	_, ok := args[0].(*NilVal)
	return &BoolVal{Val: ok}, nil
}

func builtinPairQ(args []Value) (Value, error) {
	if len(args) != 1 {
		return nil, &EvalError{Message: "pair?: requires exactly 1 argument"}
	}
	_, ok := args[0].(*PairVal)
	return &BoolVal{Val: ok}, nil
}

func builtinNumberQ(args []Value) (Value, error) {
	if len(args) != 1 {
		return nil, &EvalError{Message: "number?: requires exactly 1 argument"}
	}
	_, ok := args[0].(*IntVal)
	return &BoolVal{Val: ok}, nil
}

func builtinBooleanQ(args []Value) (Value, error) {
	if len(args) != 1 {
		return nil, &EvalError{Message: "boolean?: requires exactly 1 argument"}
	}
	_, ok := args[0].(*BoolVal)
	return &BoolVal{Val: ok}, nil
}

func builtinStringQ(args []Value) (Value, error) {
	if len(args) != 1 {
		return nil, &EvalError{Message: "string?: requires exactly 1 argument"}
	}
	_, ok := args[0].(*StringVal)
	return &BoolVal{Val: ok}, nil
}

func builtinSymbolQ(args []Value) (Value, error) {
	if len(args) != 1 {
		return nil, &EvalError{Message: "symbol?: requires exactly 1 argument"}
	}
	_, ok := args[0].(*SymbolVal)
	return &BoolVal{Val: ok}, nil
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
		return nil, &EvalError{Message: "length: requires exactly 1 argument"}
	}
	count := int64(0)
	cur := args[0]
	for {
		if _, ok := cur.(*NilVal); ok {
			break
		}
		p, ok := cur.(*PairVal)
		if !ok {
			return nil, &EvalError{Message: fmt.Sprintf("length: not a proper list: %s", args[0].String())}
		}
		count++
		cur = p.Cdr
	}
	return &IntVal{Val: count}, nil
}

func builtinAppend(args []Value) (Value, error) {
	if len(args) == 0 {
		return &NilVal{}, nil
	}
	if len(args) == 1 {
		return args[0], nil
	}
	// Collect all elements from all lists except last, then append last
	var elems []Value
	for _, a := range args[:len(args)-1] {
		cur := a
		for {
			if _, ok := cur.(*NilVal); ok {
				break
			}
			p, ok := cur.(*PairVal)
			if !ok {
				return nil, &EvalError{Message: "append: not a proper list"}
			}
			elems = append(elems, p.Car)
			cur = p.Cdr
		}
	}
	result := args[len(args)-1]
	for i := len(elems) - 1; i >= 0; i-- {
		result = &PairVal{Car: elems[i], Cdr: result}
	}
	return result, nil
}

func builtinNot(args []Value) (Value, error) {
	if len(args) != 1 {
		return nil, &EvalError{Message: "not: requires exactly 1 argument"}
	}
	return &BoolVal{Val: !isTruthy(args[0])}, nil
}
