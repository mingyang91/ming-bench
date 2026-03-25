package ming

import (
	"fmt"
	"strconv"
	"strings"
)

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
	case *CharExpr:
		return &CharVal{Val: e.Val}, nil
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
			case "define":
				return evalDefine(e, env)
			case "if":
				return evalIf(e, env)
			case "lambda":
				return evalLambda(e, env)
			case "quote":
				if len(e.Elems) != 2 {
					return nil, &EvalError{Message: fmt.Sprintf("%d:%d: quote requires 1 argument", sym.Line, sym.Col)}
				}
				return quoteExpr(e.Elems[1])
			case "begin":
				return evalBegin(e.Elems[1:], env)
			case "let":
				return evalLet(e, env)
			case "cond":
				return evalCond(e, env)
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
		switch f := fn.(type) {
		case *BuiltinFunc:
			result, ferr := f.Fn(args)
			if ferr != nil {
				line, col := e.Elems[0].pos()
				return nil, &EvalError{Message: fmt.Sprintf("%d:%d: %s", line, col, ferr.Error())}
			}
			return result, nil
		case *LambdaVal:
			if len(args) != len(f.Params) {
				line, col := e.Elems[0].pos()
				return nil, &EvalError{Message: fmt.Sprintf("%d:%d: wrong number of arguments: expected %d, got %d", line, col, len(f.Params), len(args))}
			}
			callEnv := newEnv(f.Env)
			for i, p := range f.Params {
				callEnv.set(p, args[i])
			}
			var result Value
			for _, bodyExpr := range f.Body {
				var err2 error
				result, err2 = evalExpr(bodyExpr, callEnv)
				if err2 != nil {
					return nil, err2
				}
			}
			return result, nil
		default:
			line, col := e.Elems[0].pos()
			return nil, &EvalError{Message: fmt.Sprintf("%d:%d: not a procedure", line, col)}
		}
	}
	return nil, &EvalError{Message: "unknown expression type"}
}

func evalDefine(e *ListExpr, env *Env) (Value, error) {
	if len(e.Elems) < 3 {
		return nil, &EvalError{Message: fmt.Sprintf("%d:%d: define requires at least 2 arguments", e.Line, e.Col)}
	}
	switch target := e.Elems[1].(type) {
	case *SymbolExpr:
		// (define x expr)
		val, err := evalExpr(e.Elems[2], env)
		if err != nil {
			return nil, err
		}
		env.set(target.Name, val)
		return &VoidVal{}, nil
	case *ListExpr:
		// (define (f params...) body...)
		if len(target.Elems) == 0 {
			return nil, &EvalError{Message: fmt.Sprintf("%d:%d: define: empty name list", e.Line, e.Col)}
		}
		nameSym, ok := target.Elems[0].(*SymbolExpr)
		if !ok {
			return nil, &EvalError{Message: fmt.Sprintf("%d:%d: define: expected symbol", e.Line, e.Col)}
		}
		params := make([]string, len(target.Elems)-1)
		for i, p := range target.Elems[1:] {
			ps, ok := p.(*SymbolExpr)
			if !ok {
				return nil, &EvalError{Message: fmt.Sprintf("%d:%d: define: expected symbol in parameter list", e.Line, e.Col)}
			}
			params[i] = ps.Name
		}
		lambda := &LambdaVal{Params: params, Body: e.Elems[2:], Env: env}
		env.set(nameSym.Name, lambda)
		return &VoidVal{}, nil
	default:
		return nil, &EvalError{Message: fmt.Sprintf("%d:%d: define: expected symbol or list", e.Line, e.Col)}
	}
}

func evalIf(e *ListExpr, env *Env) (Value, error) {
	if len(e.Elems) < 3 || len(e.Elems) > 4 {
		return nil, &EvalError{Message: fmt.Sprintf("%d:%d: if requires 2 or 3 arguments", e.Line, e.Col)}
	}
	cond, err := evalExpr(e.Elems[1], env)
	if err != nil {
		return nil, err
	}
	if isTruthy(cond) {
		return evalExpr(e.Elems[2], env)
	}
	if len(e.Elems) == 4 {
		return evalExpr(e.Elems[3], env)
	}
	return &VoidVal{}, nil
}

func evalLambda(e *ListExpr, env *Env) (Value, error) {
	if len(e.Elems) < 3 {
		return nil, &EvalError{Message: fmt.Sprintf("%d:%d: lambda requires params and body", e.Line, e.Col)}
	}
	paramList, ok := e.Elems[1].(*ListExpr)
	if !ok {
		return nil, &EvalError{Message: fmt.Sprintf("%d:%d: lambda: expected parameter list", e.Line, e.Col)}
	}
	params := make([]string, len(paramList.Elems))
	for i, p := range paramList.Elems {
		ps, ok := p.(*SymbolExpr)
		if !ok {
			return nil, &EvalError{Message: fmt.Sprintf("%d:%d: lambda: expected symbol in parameter list", e.Line, e.Col)}
		}
		params[i] = ps.Name
	}
	return &LambdaVal{Params: params, Body: e.Elems[2:], Env: env}, nil
}

func quoteExpr(expr Expr) (Value, error) {
	switch e := expr.(type) {
	case *NumberExpr:
		return &IntVal{Val: e.Val}, nil
	case *StringExpr:
		return &StringVal{Val: e.Val}, nil
	case *BoolExpr:
		return &BoolVal{Val: e.Val}, nil
	case *CharExpr:
		return &CharVal{Val: e.Val}, nil
	case *SymbolExpr:
		return &SymbolVal{Val: e.Name}, nil
	case *ListExpr:
		if len(e.Elems) == 0 {
			return &NilVal{}, nil
		}
		// Build a proper list from the elements
		var result Value = &NilVal{}
		for i := len(e.Elems) - 1; i >= 0; i-- {
			car, err := quoteExpr(e.Elems[i])
			if err != nil {
				return nil, err
			}
			result = &PairVal{Car: car, Cdr: result}
		}
		return result, nil
	}
	return nil, &EvalError{Message: "quote: unsupported expression type"}
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

func evalBegin(exprs []Expr, env *Env) (Value, error) {
	var result Value = &VoidVal{}
	for _, e := range exprs {
		var err error
		result, err = evalExpr(e, env)
		if err != nil {
			return nil, err
		}
	}
	return result, nil
}

func evalLet(e *ListExpr, env *Env) (Value, error) {
	if len(e.Elems) < 3 {
		return nil, &EvalError{Message: fmt.Sprintf("%d:%d: let requires bindings and body", e.Line, e.Col)}
	}

	// Named let: (let name ((var init) ...) body ...)
	if sym, ok := e.Elems[1].(*SymbolExpr); ok {
		if len(e.Elems) < 4 {
			return nil, &EvalError{Message: fmt.Sprintf("%d:%d: named let requires bindings and body", e.Line, e.Col)}
		}
		bindList, ok := e.Elems[2].(*ListExpr)
		if !ok {
			return nil, &EvalError{Message: fmt.Sprintf("%d:%d: let: expected binding list", e.Line, e.Col)}
		}
		params := make([]string, len(bindList.Elems))
		initVals := make([]Value, len(bindList.Elems))
		for i, b := range bindList.Elems {
			pair, ok := b.(*ListExpr)
			if !ok || len(pair.Elems) != 2 {
				return nil, &EvalError{Message: fmt.Sprintf("%d:%d: let: bad binding", e.Line, e.Col)}
			}
			ps, ok := pair.Elems[0].(*SymbolExpr)
			if !ok {
				return nil, &EvalError{Message: fmt.Sprintf("%d:%d: let: expected symbol", e.Line, e.Col)}
			}
			params[i] = ps.Name
			v, err := evalExpr(pair.Elems[1], env)
			if err != nil {
				return nil, err
			}
			initVals[i] = v
		}
		// Create a lambda and bind it in a new env
		letEnv := newEnv(env)
		lambda := &LambdaVal{Params: params, Body: e.Elems[3:], Env: letEnv}
		letEnv.set(sym.Name, lambda)
		// Call with initial values
		callEnv := newEnv(letEnv)
		for i, p := range params {
			callEnv.set(p, initVals[i])
		}
		var result Value
		for _, bodyExpr := range e.Elems[3:] {
			var err error
			result, err = evalExpr(bodyExpr, callEnv)
			if err != nil {
				return nil, err
			}
		}
		return result, nil
	}

	// Regular let: (let ((var init) ...) body ...)
	bindList, ok := e.Elems[1].(*ListExpr)
	if !ok {
		return nil, &EvalError{Message: fmt.Sprintf("%d:%d: let: expected binding list", e.Line, e.Col)}
	}
	letEnv := newEnv(env)
	for _, b := range bindList.Elems {
		pair, ok := b.(*ListExpr)
		if !ok || len(pair.Elems) != 2 {
			return nil, &EvalError{Message: fmt.Sprintf("%d:%d: let: bad binding", e.Line, e.Col)}
		}
		ps, ok := pair.Elems[0].(*SymbolExpr)
		if !ok {
			return nil, &EvalError{Message: fmt.Sprintf("%d:%d: let: expected symbol", e.Line, e.Col)}
		}
		v, err := evalExpr(pair.Elems[1], env)
		if err != nil {
			return nil, err
		}
		letEnv.set(ps.Name, v)
	}
	var result Value
	for _, bodyExpr := range e.Elems[2:] {
		var err error
		result, err = evalExpr(bodyExpr, letEnv)
		if err != nil {
			return nil, err
		}
	}
	return result, nil
}

func evalCond(e *ListExpr, env *Env) (Value, error) {
	for _, clause := range e.Elems[1:] {
		cl, ok := clause.(*ListExpr)
		if !ok || len(cl.Elems) == 0 {
			return nil, &EvalError{Message: fmt.Sprintf("%d:%d: cond: bad clause", e.Line, e.Col)}
		}
		// else clause
		if sym, ok := cl.Elems[0].(*SymbolExpr); ok && sym.Name == "else" {
			return evalBegin(cl.Elems[1:], env)
		}
		test, err := evalExpr(cl.Elems[0], env)
		if err != nil {
			return nil, err
		}
		if isTruthy(test) {
			if len(cl.Elems) == 1 {
				return test, nil
			}
			return evalBegin(cl.Elems[1:], env)
		}
	}
	return &VoidVal{}, nil
}

func makeGlobalEnv(output *strings.Builder) *Env {
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

	// List operations
	env.set("cons", &BuiltinFunc{Name: "cons", Fn: func(args []Value) (Value, error) {
		if len(args) != 2 {
			return nil, &EvalError{Message: "cons: need 2 arguments"}
		}
		return &PairVal{Car: args[0], Cdr: args[1]}, nil
	}})

	env.set("car", &BuiltinFunc{Name: "car", Fn: func(args []Value) (Value, error) {
		if len(args) != 1 {
			return nil, &EvalError{Message: "car: need 1 argument"}
		}
		p, ok := args[0].(*PairVal)
		if !ok {
			return nil, &EvalError{Message: "car: not a pair"}
		}
		return p.Car, nil
	}})

	env.set("cdr", &BuiltinFunc{Name: "cdr", Fn: func(args []Value) (Value, error) {
		if len(args) != 1 {
			return nil, &EvalError{Message: "cdr: need 1 argument"}
		}
		p, ok := args[0].(*PairVal)
		if !ok {
			return nil, &EvalError{Message: "cdr: not a pair"}
		}
		return p.Cdr, nil
	}})

	env.set("null?", &BuiltinFunc{Name: "null?", Fn: func(args []Value) (Value, error) {
		if len(args) != 1 {
			return nil, &EvalError{Message: "null?: need 1 argument"}
		}
		_, isNil := args[0].(*NilVal)
		return &BoolVal{Val: isNil}, nil
	}})

	env.set("list", &BuiltinFunc{Name: "list", Fn: func(args []Value) (Value, error) {
		var result Value = &NilVal{}
		for i := len(args) - 1; i >= 0; i-- {
			result = &PairVal{Car: args[i], Cdr: result}
		}
		return result, nil
	}})

	env.set("length", &BuiltinFunc{Name: "length", Fn: func(args []Value) (Value, error) {
		if len(args) != 1 {
			return nil, &EvalError{Message: "length: need 1 argument"}
		}
		var count int64
		cur := args[0]
		for {
			if _, ok := cur.(*NilVal); ok {
				break
			}
			p, ok := cur.(*PairVal)
			if !ok {
				return nil, &EvalError{Message: "length: not a proper list"}
			}
			count++
			cur = p.Cdr
		}
		return &IntVal{Val: count}, nil
	}})

	env.set("append", &BuiltinFunc{Name: "append", Fn: func(args []Value) (Value, error) {
		if len(args) == 0 {
			return &NilVal{}, nil
		}
		if len(args) == 1 {
			return args[0], nil
		}
		// Build result from right to left
		result := args[len(args)-1]
		for i := len(args) - 2; i >= 0; i-- {
			cur := args[i]
			// Collect elements of this list
			var elems []Value
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
			for j := len(elems) - 1; j >= 0; j-- {
				result = &PairVal{Car: elems[j], Cdr: result}
			}
		}
		return result, nil
	}})

	// I/O
	env.set("display", &BuiltinFunc{Name: "display", Fn: func(args []Value) (Value, error) {
		if len(args) != 1 {
			return nil, &EvalError{Message: "display: need 1 argument"}
		}
		output.WriteString(displayValue(args[0]))
		return &VoidVal{}, nil
	}})

	env.set("write", &BuiltinFunc{Name: "write", Fn: func(args []Value) (Value, error) {
		if len(args) != 1 {
			return nil, &EvalError{Message: "write: need 1 argument"}
		}
		output.WriteString(writeValue(args[0]))
		return &VoidVal{}, nil
	}})

	env.set("newline", &BuiltinFunc{Name: "newline", Fn: func(args []Value) (Value, error) {
		if len(args) != 0 {
			return nil, &EvalError{Message: "newline: need 0 arguments"}
		}
		output.WriteByte('\n')
		return &VoidVal{}, nil
	}})

	// String operations
	env.set("string-append", &BuiltinFunc{Name: "string-append", Fn: func(args []Value) (Value, error) {
		var buf strings.Builder
		for _, a := range args {
			s, ok := a.(*StringVal)
			if !ok {
				return nil, &EvalError{Message: "string-append: not a string"}
			}
			buf.WriteString(s.Val)
		}
		return &StringVal{Val: buf.String()}, nil
	}})

	env.set("string-length", &BuiltinFunc{Name: "string-length", Fn: func(args []Value) (Value, error) {
		if len(args) != 1 {
			return nil, &EvalError{Message: "string-length: need 1 argument"}
		}
		s, ok := args[0].(*StringVal)
		if !ok {
			return nil, &EvalError{Message: "string-length: not a string"}
		}
		return &IntVal{Val: int64(len(s.Val))}, nil
	}})

	env.set("substring", &BuiltinFunc{Name: "substring", Fn: func(args []Value) (Value, error) {
		if len(args) != 3 {
			return nil, &EvalError{Message: "substring: need 3 arguments"}
		}
		s, ok := args[0].(*StringVal)
		if !ok {
			return nil, &EvalError{Message: "substring: not a string"}
		}
		start, ok := args[1].(*IntVal)
		if !ok {
			return nil, &EvalError{Message: "substring: start not a number"}
		}
		end, ok := args[2].(*IntVal)
		if !ok {
			return nil, &EvalError{Message: "substring: end not a number"}
		}
		if start.Val < 0 || end.Val > int64(len(s.Val)) || start.Val > end.Val {
			return nil, &EvalError{Message: "substring: index out of range"}
		}
		return &StringVal{Val: s.Val[start.Val:end.Val]}, nil
	}})

	env.set("string->number", &BuiltinFunc{Name: "string->number", Fn: func(args []Value) (Value, error) {
		if len(args) != 1 {
			return nil, &EvalError{Message: "string->number: need 1 argument"}
		}
		s, ok := args[0].(*StringVal)
		if !ok {
			return nil, &EvalError{Message: "string->number: not a string"}
		}
		n, err := strconv.ParseInt(s.Val, 10, 64)
		if err != nil {
			return &BoolVal{Val: false}, nil
		}
		return &IntVal{Val: n}, nil
	}})

	env.set("number->string", &BuiltinFunc{Name: "number->string", Fn: func(args []Value) (Value, error) {
		if len(args) != 1 {
			return nil, &EvalError{Message: "number->string: need 1 argument"}
		}
		n, ok := args[0].(*IntVal)
		if !ok {
			return nil, &EvalError{Message: "number->string: not a number"}
		}
		return &StringVal{Val: strconv.FormatInt(n.Val, 10)}, nil
	}})

	env.set("symbol->string", &BuiltinFunc{Name: "symbol->string", Fn: func(args []Value) (Value, error) {
		if len(args) != 1 {
			return nil, &EvalError{Message: "symbol->string: need 1 argument"}
		}
		s, ok := args[0].(*SymbolVal)
		if !ok {
			return nil, &EvalError{Message: "symbol->string: not a symbol"}
		}
		return &StringVal{Val: s.Val}, nil
	}})

	env.set("string->symbol", &BuiltinFunc{Name: "string->symbol", Fn: func(args []Value) (Value, error) {
		if len(args) != 1 {
			return nil, &EvalError{Message: "string->symbol: need 1 argument"}
		}
		s, ok := args[0].(*StringVal)
		if !ok {
			return nil, &EvalError{Message: "string->symbol: not a string"}
		}
		return &SymbolVal{Val: s.Val}, nil
	}})

	env.set("string-ref", &BuiltinFunc{Name: "string-ref", Fn: func(args []Value) (Value, error) {
		if len(args) != 2 {
			return nil, &EvalError{Message: "string-ref: need 2 arguments"}
		}
		s, ok := args[0].(*StringVal)
		if !ok {
			return nil, &EvalError{Message: "string-ref: not a string"}
		}
		idx, ok := args[1].(*IntVal)
		if !ok {
			return nil, &EvalError{Message: "string-ref: index not a number"}
		}
		if idx.Val < 0 || idx.Val >= int64(len(s.Val)) {
			return nil, &EvalError{Message: "string-ref: index out of range"}
		}
		return &CharVal{Val: rune(s.Val[idx.Val])}, nil
	}})

	env.set("string-copy", &BuiltinFunc{Name: "string-copy", Fn: func(args []Value) (Value, error) {
		if len(args) != 1 {
			return nil, &EvalError{Message: "string-copy: need 1 argument"}
		}
		s, ok := args[0].(*StringVal)
		if !ok {
			return nil, &EvalError{Message: "string-copy: not a string"}
		}
		return &StringVal{Val: s.Val}, nil
	}})

	env.set("string-set!", &BuiltinFunc{Name: "string-set!", Fn: func(args []Value) (Value, error) {
		if len(args) != 3 {
			return nil, &EvalError{Message: "string-set!: need 3 arguments"}
		}
		s, ok := args[0].(*StringVal)
		if !ok {
			return nil, &EvalError{Message: "string-set!: not a string"}
		}
		idx, ok := args[1].(*IntVal)
		if !ok {
			return nil, &EvalError{Message: "string-set!: index not a number"}
		}
		ch, ok := args[2].(*CharVal)
		if !ok {
			return nil, &EvalError{Message: "string-set!: not a character"}
		}
		if idx.Val < 0 || idx.Val >= int64(len(s.Val)) {
			return nil, &EvalError{Message: "string-set!: index out of range"}
		}
		b := []byte(s.Val)
		b[idx.Val] = byte(ch.Val)
		s.Val = string(b)
		return &VoidVal{}, nil
	}})

	env.set("char?", &BuiltinFunc{Name: "char?", Fn: func(args []Value) (Value, error) {
		if len(args) != 1 {
			return nil, &EvalError{Message: "char?: need 1 argument"}
		}
		_, ok := args[0].(*CharVal)
		return &BoolVal{Val: ok}, nil
	}})

	// Type predicates
	env.set("number?", &BuiltinFunc{Name: "number?", Fn: func(args []Value) (Value, error) {
		if len(args) != 1 {
			return nil, &EvalError{Message: "number?: need 1 argument"}
		}
		_, ok := args[0].(*IntVal)
		return &BoolVal{Val: ok}, nil
	}})

	env.set("string?", &BuiltinFunc{Name: "string?", Fn: func(args []Value) (Value, error) {
		if len(args) != 1 {
			return nil, &EvalError{Message: "string?: need 1 argument"}
		}
		_, ok := args[0].(*StringVal)
		return &BoolVal{Val: ok}, nil
	}})

	env.set("boolean?", &BuiltinFunc{Name: "boolean?", Fn: func(args []Value) (Value, error) {
		if len(args) != 1 {
			return nil, &EvalError{Message: "boolean?: need 1 argument"}
		}
		_, ok := args[0].(*BoolVal)
		return &BoolVal{Val: ok}, nil
	}})

	env.set("pair?", &BuiltinFunc{Name: "pair?", Fn: func(args []Value) (Value, error) {
		if len(args) != 1 {
			return nil, &EvalError{Message: "pair?: need 1 argument"}
		}
		_, ok := args[0].(*PairVal)
		return &BoolVal{Val: ok}, nil
	}})

	env.set("symbol?", &BuiltinFunc{Name: "symbol?", Fn: func(args []Value) (Value, error) {
		if len(args) != 1 {
			return nil, &EvalError{Message: "symbol?: need 1 argument"}
		}
		_, ok := args[0].(*SymbolVal)
		return &BoolVal{Val: ok}, nil
	}})

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
	r, _, err := EvalStrWithOutput(input)
	return r, err
}

// EvalStrWithOutput evaluates Scheme expressions and returns both the result
// string and any captured output from display/write/newline.
func EvalStrWithOutput(input string) (result string, output string, err error) {
	exprs, parseErr := parse(input)
	if parseErr != nil {
		return "", "", &EvalError{Message: parseErr.Error()}
	}
	if len(exprs) == 0 {
		return "", "", nil
	}
	var buf strings.Builder
	env := makeGlobalEnv(&buf)
	var last Value
	for _, expr := range exprs {
		last, err = evalExpr(expr, env)
		if err != nil {
			return "", "", err
		}
	}
	if _, ok := last.(*VoidVal); ok {
		return "", buf.String(), nil
	}
	return last.String(), buf.String(), nil
}
