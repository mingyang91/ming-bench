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

func makeGlobalEnv() *Env {
	env := newEnv(nil)
	return env
}

// listToSlice converts a scheme list to a Go slice.
func listToSlice(v *Value) []*Value {
	var result []*Value
	cur := v
	for cur.Type == TypePair {
		result = append(result, cur.Car)
		cur = cur.Cdr
	}
	return result
}

func eval(expr *Value, env *Env) (*Value, error) {
	switch expr.Type {
	case TypeInteger, TypeBoolean, TypeString:
		return expr, nil
	case TypeSymbol:
		v, ok := env.get(expr.StrVal)
		if !ok {
			return nil, &EvalError{Message: fmt.Sprintf("unbound variable: %s", expr.StrVal)}
		}
		return v, nil
	case TypeNull:
		return nil, &EvalError{Message: "empty application"}
	case TypePair:
		return evalList(expr, env)
	}
	return nil, &EvalError{Message: "unknown expression type"}
}

// isBuiltin checks if a symbol name is a built-in procedure.
func isBuiltin(name string) bool {
	switch name {
	case "+", "-", "*", "/", "<", ">", "=", "<=", ">=", "not",
		"cons", "car", "cdr", "null?", "list", "length", "append",
		"string?", "number?", "boolean?", "pair?", "symbol?":
		return true
	}
	return false
}

func evalList(expr *Value, env *Env) (*Value, error) {
	head := expr.Car

	// Special forms
	if head.Type == TypeSymbol {
		switch head.StrVal {
		case "and":
			return evalAnd(expr.Cdr, env)
		case "or":
			return evalOr(expr.Cdr, env)
		case "define":
			return evalDefine(expr.Cdr, env)
		case "if":
			return evalIf(expr.Cdr, env)
		case "quote":
			return expr.Cdr.Car, nil
		case "lambda":
			return evalLambda(expr.Cdr, env)
		case "let":
			return evalLet(expr.Cdr, env)
		case "begin":
			return evalBegin(expr.Cdr, env)
		case "cond":
			return evalCond(expr.Cdr, env)
		}
	}

	// Evaluate arguments
	args := listToSlice(expr.Cdr)
	evaledArgs := make([]*Value, len(args))
	for i, a := range args {
		v, err := eval(a, env)
		if err != nil {
			return nil, err
		}
		evaledArgs[i] = v
	}

	// Built-in procedure call
	if head.Type == TypeSymbol && isBuiltin(head.StrVal) {
		return applyBuiltin(head, evaledArgs)
	}

	// Evaluate head for user-defined procedures
	fn, err := eval(head, env)
	if err != nil {
		return nil, err
	}

	// Lambda application
	if fn.Type == TypeLambda {
		return applyLambda(fn, evaledArgs)
	}

	return nil, &EvalError{Message: fmt.Sprintf("not a procedure: %s", fn.Display())}
}

func evalDefine(args *Value, env *Env) (*Value, error) {
	first := args.Car
	if first.Type == TypeSymbol {
		// (define x expr)
		val, err := eval(args.Cdr.Car, env)
		if err != nil {
			return nil, err
		}
		env.set(first.StrVal, val)
		return Void, nil
	}
	if first.Type == TypePair {
		// (define (f params...) body...)
		name := first.Car.StrVal
		paramsList := listToSlice(first.Cdr)
		params := make([]string, len(paramsList))
		for i, p := range paramsList {
			params[i] = p.StrVal
		}
		body := listToSlice(args.Cdr)
		fn := &Value{
			Type:       TypeLambda,
			Params:     params,
			Body:       body,
			ClosureEnv: env,
		}
		env.set(name, fn)
		return Void, nil
	}
	return nil, &EvalError{Message: "bad define syntax"}
}

func evalIf(args *Value, env *Env) (*Value, error) {
	cond, err := eval(args.Car, env)
	if err != nil {
		return nil, err
	}
	if isTruthy(cond) {
		return eval(args.Cdr.Car, env)
	}
	// else branch
	if args.Cdr.Cdr.Type != TypeNull {
		return eval(args.Cdr.Cdr.Car, env)
	}
	return Void, nil
}

func evalLambda(args *Value, env *Env) (*Value, error) {
	paramsList := listToSlice(args.Car)
	params := make([]string, len(paramsList))
	for i, p := range paramsList {
		params[i] = p.StrVal
	}
	body := listToSlice(args.Cdr)
	return &Value{
		Type:       TypeLambda,
		Params:     params,
		Body:       body,
		ClosureEnv: env,
	}, nil
}

func applyLambda(fn *Value, args []*Value) (*Value, error) {
	localEnv := newEnv(fn.ClosureEnv)
	for i, p := range fn.Params {
		localEnv.set(p, args[i])
	}
	var result *Value
	var err error
	for _, bodyExpr := range fn.Body {
		result, err = eval(bodyExpr, localEnv)
		if err != nil {
			return nil, err
		}
	}
	return result, nil
}

func evalLet(args *Value, env *Env) (*Value, error) {
	first := args.Car
	// Named let: (let name ((var init) ...) body...)
	if first.Type == TypeSymbol {
		name := first.StrVal
		bindings := listToSlice(args.Cdr.Car)
		body := listToSlice(args.Cdr.Cdr)
		params := make([]string, len(bindings))
		inits := make([]*Value, len(bindings))
		for i, b := range bindings {
			pair := listToSlice(b)
			params[i] = pair[0].StrVal
			v, err := eval(pair[1], env)
			if err != nil {
				return nil, err
			}
			inits[i] = v
		}
		// Create lambda for the loop
		fn := &Value{
			Type:       TypeLambda,
			Params:     params,
			Body:       body,
			ClosureEnv: env,
		}
		// Bind the name in the lambda's closure so it can recurse
		loopEnv := newEnv(env)
		loopEnv.set(name, fn)
		fn.ClosureEnv = loopEnv
		return applyLambda(fn, inits)
	}
	// Regular let: (let ((var init) ...) body...)
	bindings := listToSlice(first)
	localEnv := newEnv(env)
	for _, b := range bindings {
		pair := listToSlice(b)
		v, err := eval(pair[1], env)
		if err != nil {
			return nil, err
		}
		localEnv.set(pair[0].StrVal, v)
	}
	body := listToSlice(args.Cdr)
	var result *Value
	var err error
	for _, expr := range body {
		result, err = eval(expr, localEnv)
		if err != nil {
			return nil, err
		}
	}
	return result, nil
}

func evalBegin(args *Value, env *Env) (*Value, error) {
	items := listToSlice(args)
	var result *Value
	var err error
	for _, item := range items {
		result, err = eval(item, env)
		if err != nil {
			return nil, err
		}
	}
	if result == nil {
		return Void, nil
	}
	return result, nil
}

func evalCond(args *Value, env *Env) (*Value, error) {
	clauses := listToSlice(args)
	for _, clause := range clauses {
		items := listToSlice(clause)
		// Check for else clause
		if items[0].Type == TypeSymbol && items[0].StrVal == "else" {
			var result *Value
			var err error
			for _, expr := range items[1:] {
				result, err = eval(expr, env)
				if err != nil {
					return nil, err
				}
			}
			return result, nil
		}
		test, err := eval(items[0], env)
		if err != nil {
			return nil, err
		}
		if isTruthy(test) {
			if len(items) == 1 {
				return test, nil
			}
			var result *Value
			for _, expr := range items[1:] {
				result, err = eval(expr, env)
				if err != nil {
					return nil, err
				}
			}
			return result, nil
		}
	}
	return Void, nil
}

func evalAnd(args *Value, env *Env) (*Value, error) {
	if args.Type == TypeNull {
		return NewBool(true), nil
	}
	items := listToSlice(args)
	var result *Value = NewBool(true)
	for _, item := range items {
		v, err := eval(item, env)
		if err != nil {
			return nil, err
		}
		if v.Type == TypeBoolean && !v.BoolVal {
			return v, nil // short-circuit
		}
		result = v
	}
	return result, nil
}

func evalOr(args *Value, env *Env) (*Value, error) {
	if args.Type == TypeNull {
		return NewBool(false), nil
	}
	items := listToSlice(args)
	for _, item := range items {
		v, err := eval(item, env)
		if err != nil {
			return nil, err
		}
		if isTruthy(v) {
			return v, nil
		}
	}
	return NewBool(false), nil
}

func isTruthy(v *Value) bool {
	return !(v.Type == TypeBoolean && !v.BoolVal)
}

func applyBuiltin(head *Value, args []*Value) (*Value, error) {
	if head.Type != TypeSymbol {
		return nil, &EvalError{Message: fmt.Sprintf("not a procedure: %s", head.Display())}
	}
	name := head.StrVal

	switch name {
	case "+":
		sum := int64(0)
		for _, a := range args {
			if a.Type != TypeInteger {
				return nil, &EvalError{Message: "expected number"}
			}
			sum += a.IntVal
		}
		return NewInt(sum), nil

	case "-":
		if len(args) == 0 {
			return nil, &EvalError{Message: "- requires at least one argument"}
		}
		if args[0].Type != TypeInteger {
			return nil, &EvalError{Message: "expected number"}
		}
		if len(args) == 1 {
			return NewInt(-args[0].IntVal), nil
		}
		result := args[0].IntVal
		for _, a := range args[1:] {
			if a.Type != TypeInteger {
				return nil, &EvalError{Message: "expected number"}
			}
			result -= a.IntVal
		}
		return NewInt(result), nil

	case "*":
		product := int64(1)
		for _, a := range args {
			if a.Type != TypeInteger {
				return nil, &EvalError{Message: "expected number"}
			}
			product *= a.IntVal
		}
		return NewInt(product), nil

	case "/":
		if len(args) < 2 {
			return nil, &EvalError{Message: "/ requires at least two arguments"}
		}
		if args[0].Type != TypeInteger {
			return nil, &EvalError{Message: "expected number"}
		}
		result := args[0].IntVal
		for _, a := range args[1:] {
			if a.Type != TypeInteger {
				return nil, &EvalError{Message: "expected number"}
			}
			if a.IntVal == 0 {
				return nil, &EvalError{Message: "division by zero"}
			}
			result /= a.IntVal
		}
		return NewInt(result), nil

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
			return nil, &EvalError{Message: "not requires exactly one argument"}
		}
		return NewBool(!isTruthy(args[0])), nil

	case "cons":
		if len(args) != 2 {
			return nil, &EvalError{Message: "cons requires exactly two arguments"}
		}
		return NewPair(args[0], args[1]), nil

	case "car":
		if len(args) != 1 || args[0].Type != TypePair {
			return nil, &EvalError{Message: "car requires a pair"}
		}
		return args[0].Car, nil

	case "cdr":
		if len(args) != 1 || args[0].Type != TypePair {
			return nil, &EvalError{Message: "cdr requires a pair"}
		}
		return args[0].Cdr, nil

	case "null?":
		if len(args) != 1 {
			return nil, &EvalError{Message: "null? requires exactly one argument"}
		}
		return NewBool(args[0].Type == TypeNull), nil

	case "list":
		result := Null
		for i := len(args) - 1; i >= 0; i-- {
			result = NewPair(args[i], result)
		}
		return result, nil

	case "length":
		if len(args) != 1 {
			return nil, &EvalError{Message: "length requires exactly one argument"}
		}
		count := int64(0)
		cur := args[0]
		for cur.Type == TypePair {
			count++
			cur = cur.Cdr
		}
		return NewInt(count), nil

	case "append":
		if len(args) == 0 {
			return Null, nil
		}
		result := args[len(args)-1]
		for i := len(args) - 2; i >= 0; i-- {
			lst := args[i]
			// collect elements then prepend
			var elems []*Value
			cur := lst
			for cur.Type == TypePair {
				elems = append(elems, cur.Car)
				cur = cur.Cdr
			}
			for j := len(elems) - 1; j >= 0; j-- {
				result = NewPair(elems[j], result)
			}
		}
		return result, nil

	case "string?":
		if len(args) != 1 {
			return nil, &EvalError{Message: "string? requires exactly one argument"}
		}
		return NewBool(args[0].Type == TypeString), nil

	case "number?":
		if len(args) != 1 {
			return nil, &EvalError{Message: "number? requires exactly one argument"}
		}
		return NewBool(args[0].Type == TypeInteger), nil

	case "boolean?":
		if len(args) != 1 {
			return nil, &EvalError{Message: "boolean? requires exactly one argument"}
		}
		return NewBool(args[0].Type == TypeBoolean), nil

	case "pair?":
		if len(args) != 1 {
			return nil, &EvalError{Message: "pair? requires exactly one argument"}
		}
		return NewBool(args[0].Type == TypePair), nil

	case "symbol?":
		if len(args) != 1 {
			return nil, &EvalError{Message: "symbol? requires exactly one argument"}
		}
		return NewBool(args[0].Type == TypeSymbol), nil
	}

	return nil, &EvalError{Message: fmt.Sprintf("unbound variable: %s", name)}
}

func compareInts(args []*Value, cmp func(int64, int64) bool) (*Value, error) {
	if len(args) < 2 {
		return nil, &EvalError{Message: "comparison requires at least two arguments"}
	}
	for _, a := range args {
		if a.Type != TypeInteger {
			return nil, &EvalError{Message: "expected number"}
		}
	}
	for i := 0; i < len(args)-1; i++ {
		if !cmp(args[i].IntVal, args[i+1].IntVal) {
			return NewBool(false), nil
		}
	}
	return NewBool(true), nil
}

// EvalStr evaluates one or more Scheme expressions and returns the string
// representation of the last result.
func EvalStr(input string) (string, error) {
	exprs, err := parse(input)
	if err != nil {
		return "", &EvalError{Message: err.Error()}
	}
	if len(exprs) == 0 {
		return "", &EvalError{Message: "no expressions"}
	}

	env := makeGlobalEnv()
	var last *Value
	for _, expr := range exprs {
		v, err := eval(expr, env)
		if err != nil {
			return "", err
		}
		if v.Type != TypeVoid {
			last = v
		}
	}
	if last == nil {
		return "", nil
	}
	return last.Display(), nil
}

// EvalStrWithOutput evaluates Scheme expressions and returns both the result
// string and any captured output from display/write/newline.
func EvalStrWithOutput(input string) (result string, output string, err error) {
	r, err := EvalStr(input)
	return r, "", err
}
