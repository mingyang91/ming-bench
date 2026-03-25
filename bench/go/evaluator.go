package ming

import (
	"fmt"
	"strconv"
	"strings"
	"unicode"
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

func (e *Env) setExisting(name string, val Value) bool {
	if _, ok := e.bindings[name]; ok {
		e.bindings[name] = val
		return true
	}
	if e.parent != nil {
		return e.parent.setExisting(name, val)
	}
	return false
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
	case *FloatExpr:
		return &FloatVal{Val: e.Val}, nil
	case *RationalExpr:
		return makeRat(e.Num, e.Den), nil
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
	case *EnvRefExpr:
		v, ok := e.Env.get(e.Name)
		if !ok {
			return nil, &EvalError{Message: fmt.Sprintf("%d:%d: unbound variable: %s", e.Line, e.Col, e.Name)}
		}
		return v, nil
	case *VectorExpr:
		elems := make([]Value, len(e.Elems))
		for i, elem := range e.Elems {
			v, err := evalExpr(elem, env)
			if err != nil {
				return nil, err
			}
			elems[i] = v
		}
		return &VectorVal{Elems: elems}, nil
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
			case "case-lambda":
				return evalCaseLambda(e, env)
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
			case "set!":
				if len(e.Elems) != 3 {
					return nil, &EvalError{Message: fmt.Sprintf("%d:%d: set! requires 2 arguments", sym.Line, sym.Col)}
				}
				target, ok := e.Elems[1].(*SymbolExpr)
				if !ok {
					return nil, &EvalError{Message: fmt.Sprintf("%d:%d: set!: expected symbol", sym.Line, sym.Col)}
				}
				val, err := evalExpr(e.Elems[2], env)
				if err != nil {
					return nil, err
				}
				if !env.setExisting(target.Name, val) {
					return nil, &EvalError{Message: fmt.Sprintf("%d:%d: set!: unbound variable: %s", target.Line, target.Col, target.Name)}
				}
				return &VoidVal{}, nil
			case "not":
				if len(e.Elems) != 2 {
					return nil, &EvalError{Message: fmt.Sprintf("%d:%d: not requires 1 argument", sym.Line, sym.Col)}
				}
				v, err := evalExpr(e.Elems[1], env)
				if err != nil {
					return nil, err
				}
				return &BoolVal{Val: !isTruthy(v)}, nil
			case "define-syntax":
				return evalDefineSyntax(e, env)
			case "define-record-type":
				return evalDefineRecordType(e, env)
			case "letrec":
				return evalLetrec(e, env, false)
			case "letrec*":
				return evalLetrec(e, env, true)
			case "case":
				return evalCase(e, env)
			case "do":
				return evalDo(e, env)
			}
			// Check if symbol is bound to a macro
			if v, ok := env.get(sym.Name); ok {
				if macro, ok := v.(*SyntaxRulesVal); ok {
					expanded, err := expandMacro(macro, e)
					if err != nil {
						return nil, err
					}
					return evalExpr(expanded, env)
				}
			}
		}
		// Check if first element is an EnvRefExpr pointing to a macro
		if ref, ok := e.Elems[0].(*EnvRefExpr); ok {
			if v, ok := ref.Env.get(ref.Name); ok {
				if macro, ok := v.(*SyntaxRulesVal); ok {
					expanded, err := expandMacro(macro, e)
					if err != nil {
						return nil, err
					}
					return evalExpr(expanded, env)
				}
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
			return applyLambda(f, args)
		case *CaseLambdaVal:
			return applyCaseLambda(f, args)
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
		params, rest, perr := parseParams(target.Elems[1:], e.Line, e.Col)
		if perr != nil {
			return nil, perr
		}
		lambda := &LambdaVal{Params: params, RestParam: rest, Body: e.Elems[2:], Env: env}
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

// parseParams extracts parameter names and optional rest param from a list of exprs.
// Handles dot notation: (x y . rest) -> params=["x","y"], rest="rest"
func parseParams(elems []Expr, line, col int) ([]string, string, error) {
	var params []string
	var restParam string
	for i, p := range elems {
		ps, ok := p.(*SymbolExpr)
		if !ok {
			return nil, "", &EvalError{Message: fmt.Sprintf("%d:%d: expected symbol in parameter list", line, col)}
		}
		if ps.Name == "." {
			if i+1 != len(elems)-1 {
				return nil, "", &EvalError{Message: fmt.Sprintf("%d:%d: malformed dot in parameter list", line, col)}
			}
			rs, ok := elems[i+1].(*SymbolExpr)
			if !ok {
				return nil, "", &EvalError{Message: fmt.Sprintf("%d:%d: expected symbol after dot", line, col)}
			}
			restParam = rs.Name
			break
		}
		params = append(params, ps.Name)
	}
	return params, restParam, nil
}

func evalLambda(e *ListExpr, env *Env) (Value, error) {
	if len(e.Elems) < 3 {
		return nil, &EvalError{Message: fmt.Sprintf("%d:%d: lambda requires params and body", e.Line, e.Col)}
	}
	switch pl := e.Elems[1].(type) {
	case *ListExpr:
		params, rest, err := parseParams(pl.Elems, e.Line, e.Col)
		if err != nil {
			return nil, err
		}
		return &LambdaVal{Params: params, RestParam: rest, Body: e.Elems[2:], Env: env}, nil
	case *SymbolExpr:
		// (lambda args body...) — all args collected into single rest param
		return &LambdaVal{RestParam: pl.Name, Body: e.Elems[2:], Env: env}, nil
	default:
		return nil, &EvalError{Message: fmt.Sprintf("%d:%d: lambda: expected parameter list", e.Line, e.Col)}
	}
}

func evalCaseLambda(e *ListExpr, env *Env) (Value, error) {
	if len(e.Elems) < 2 {
		return nil, &EvalError{Message: fmt.Sprintf("%d:%d: case-lambda requires at least one clause", e.Line, e.Col)}
	}
	var clauses []*LambdaVal
	for _, clauseExpr := range e.Elems[1:] {
		cl, ok := clauseExpr.(*ListExpr)
		if !ok || len(cl.Elems) < 2 {
			return nil, &EvalError{Message: fmt.Sprintf("%d:%d: case-lambda: bad clause", e.Line, e.Col)}
		}
		switch pl := cl.Elems[0].(type) {
		case *ListExpr:
			params, rest, err := parseParams(pl.Elems, cl.Line, cl.Col)
			if err != nil {
				return nil, err
			}
			clauses = append(clauses, &LambdaVal{Params: params, RestParam: rest, Body: cl.Elems[1:], Env: env})
		case *SymbolExpr:
			clauses = append(clauses, &LambdaVal{RestParam: pl.Name, Body: cl.Elems[1:], Env: env})
		default:
			return nil, &EvalError{Message: fmt.Sprintf("%d:%d: case-lambda: expected parameter list", e.Line, e.Col)}
		}
	}
	return &CaseLambdaVal{Clauses: clauses}, nil
}

func applyCaseLambda(f *CaseLambdaVal, args []Value) (Value, error) {
	for _, clause := range f.Clauses {
		if clause.RestParam != "" {
			if len(args) >= len(clause.Params) {
				return applyLambda(clause, args)
			}
		} else {
			if len(args) == len(clause.Params) {
				return applyLambda(clause, args)
			}
		}
	}
	return nil, &EvalError{Message: fmt.Sprintf("case-lambda: no matching clause for %d arguments", len(args))}
}

// applyLambda calls a lambda with the given arguments, handling rest params.
func applyLambda(f *LambdaVal, args []Value) (Value, error) {
	if f.RestParam != "" {
		if len(args) < len(f.Params) {
			return nil, &EvalError{Message: fmt.Sprintf("wrong number of arguments: expected at least %d, got %d", len(f.Params), len(args))}
		}
	} else {
		if len(args) != len(f.Params) {
			return nil, &EvalError{Message: fmt.Sprintf("wrong number of arguments: expected %d, got %d", len(f.Params), len(args))}
		}
	}
	callEnv := newEnv(f.Env)
	for i, p := range f.Params {
		callEnv.set(p, args[i])
	}
	if f.RestParam != "" {
		// collect remaining args into a list
		var rest Value = &NilVal{}
		for i := len(args) - 1; i >= len(f.Params); i-- {
			rest = &PairVal{Car: args[i], Cdr: rest}
		}
		callEnv.set(f.RestParam, rest)
	}
	var result Value
	for _, bodyExpr := range f.Body {
		var err error
		result, err = evalExpr(bodyExpr, callEnv)
		if err != nil {
			return nil, err
		}
	}
	return result, nil
}

func quoteExpr(expr Expr) (Value, error) {
	switch e := expr.(type) {
	case *NumberExpr:
		return &IntVal{Val: e.Val}, nil
	case *FloatExpr:
		return &FloatVal{Val: e.Val}, nil
	case *RationalExpr:
		return makeRat(e.Num, e.Den), nil
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
	case *VectorExpr:
		elems := make([]Value, len(e.Elems))
		for i, elem := range e.Elems {
			v, err := quoteExpr(elem)
			if err != nil {
				return nil, err
			}
			elems[i] = v
		}
		return &VectorVal{Elems: elems}, nil
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

func evalLetrec(e *ListExpr, env *Env, star bool) (Value, error) {
	if len(e.Elems) < 3 {
		return nil, &EvalError{Message: fmt.Sprintf("%d:%d: letrec requires bindings and body", e.Line, e.Col)}
	}
	bindList, ok := e.Elems[1].(*ListExpr)
	if !ok {
		return nil, &EvalError{Message: fmt.Sprintf("%d:%d: letrec: expected binding list", e.Line, e.Col)}
	}
	letEnv := newEnv(env)
	// Initialize all bindings to void
	names := make([]string, len(bindList.Elems))
	for i, b := range bindList.Elems {
		pair, ok := b.(*ListExpr)
		if !ok || len(pair.Elems) != 2 {
			return nil, &EvalError{Message: fmt.Sprintf("%d:%d: letrec: bad binding", e.Line, e.Col)}
		}
		ps, ok := pair.Elems[0].(*SymbolExpr)
		if !ok {
			return nil, &EvalError{Message: fmt.Sprintf("%d:%d: letrec: expected symbol", e.Line, e.Col)}
		}
		names[i] = ps.Name
		letEnv.set(ps.Name, &VoidVal{})
	}
	if star {
		// letrec*: evaluate each init in letEnv sequentially
		for i, b := range bindList.Elems {
			pair := b.(*ListExpr)
			v, err := evalExpr(pair.Elems[1], letEnv)
			if err != nil {
				return nil, err
			}
			letEnv.set(names[i], v)
		}
	} else {
		// letrec: evaluate all inits in letEnv, then assign
		vals := make([]Value, len(bindList.Elems))
		for i, b := range bindList.Elems {
			pair := b.(*ListExpr)
			v, err := evalExpr(pair.Elems[1], letEnv)
			if err != nil {
				return nil, err
			}
			vals[i] = v
		}
		for i, name := range names {
			letEnv.set(name, vals[i])
		}
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

func evalCase(e *ListExpr, env *Env) (Value, error) {
	if len(e.Elems) < 3 {
		return nil, &EvalError{Message: fmt.Sprintf("%d:%d: case requires key and clauses", e.Line, e.Col)}
	}
	key, err := evalExpr(e.Elems[1], env)
	if err != nil {
		return nil, err
	}
	for _, clause := range e.Elems[2:] {
		cl, ok := clause.(*ListExpr)
		if !ok || len(cl.Elems) < 2 {
			return nil, &EvalError{Message: fmt.Sprintf("%d:%d: case: bad clause", e.Line, e.Col)}
		}
		// Check for else clause
		if sym, ok := cl.Elems[0].(*SymbolExpr); ok && sym.Name == "else" {
			var result Value
			for _, bodyExpr := range cl.Elems[1:] {
				result, err = evalExpr(bodyExpr, env)
				if err != nil {
					return nil, err
				}
			}
			return result, nil
		}
		// Datum list
		datums, ok := cl.Elems[0].(*ListExpr)
		if !ok {
			return nil, &EvalError{Message: fmt.Sprintf("%d:%d: case: expected datum list", e.Line, e.Col)}
		}
		matched := false
		for _, d := range datums.Elems {
			dv, err := quoteExpr(d)
			if err != nil {
				return nil, err
			}
			if schemeEqv(key, dv) {
				matched = true
				break
			}
		}
		if matched {
			var result Value
			for _, bodyExpr := range cl.Elems[1:] {
				result, err = evalExpr(bodyExpr, env)
				if err != nil {
					return nil, err
				}
			}
			return result, nil
		}
	}
	// No match, no else — return void
	return &VoidVal{}, nil
}

func evalDo(e *ListExpr, env *Env) (Value, error) {
	// (do ((var init step) ...) (test expr ...) body ...)
	if len(e.Elems) < 3 {
		return nil, &EvalError{Message: fmt.Sprintf("%d:%d: do requires bindings and test", e.Line, e.Col)}
	}
	bindList, ok := e.Elems[1].(*ListExpr)
	if !ok {
		return nil, &EvalError{Message: fmt.Sprintf("%d:%d: do: expected binding list", e.Line, e.Col)}
	}
	testClause, ok := e.Elems[2].(*ListExpr)
	if !ok || len(testClause.Elems) == 0 {
		return nil, &EvalError{Message: fmt.Sprintf("%d:%d: do: expected test clause", e.Line, e.Col)}
	}
	body := e.Elems[3:]

	type doVar struct {
		name string
		step Expr // nil if no step
	}

	vars := make([]doVar, len(bindList.Elems))
	doEnv := newEnv(env)

	// Initialize variables
	for i, b := range bindList.Elems {
		binding, ok := b.(*ListExpr)
		if !ok || len(binding.Elems) < 2 || len(binding.Elems) > 3 {
			return nil, &EvalError{Message: fmt.Sprintf("%d:%d: do: bad variable spec", e.Line, e.Col)}
		}
		sym, ok := binding.Elems[0].(*SymbolExpr)
		if !ok {
			return nil, &EvalError{Message: fmt.Sprintf("%d:%d: do: expected symbol", e.Line, e.Col)}
		}
		initVal, err := evalExpr(binding.Elems[1], env)
		if err != nil {
			return nil, err
		}
		vars[i].name = sym.Name
		if len(binding.Elems) == 3 {
			vars[i].step = binding.Elems[2]
		}
		doEnv.set(sym.Name, initVal)
	}

	// Iteration loop
	for {
		// Evaluate test
		testVal, err := evalExpr(testClause.Elems[0], doEnv)
		if err != nil {
			return nil, err
		}
		if isTruthy(testVal) {
			// Test is true — evaluate result expressions
			if len(testClause.Elems) == 1 {
				return &VoidVal{}, nil
			}
			var result Value
			for _, expr := range testClause.Elems[1:] {
				result, err = evalExpr(expr, doEnv)
				if err != nil {
					return nil, err
				}
			}
			return result, nil
		}

		// Evaluate body
		for _, bodyExpr := range body {
			_, err := evalExpr(bodyExpr, doEnv)
			if err != nil {
				return nil, err
			}
		}

		// Evaluate step expressions using PREVIOUS values (parallel update)
		newVals := make([]Value, len(vars))
		for i, v := range vars {
			if v.step != nil {
				val, err := evalExpr(v.step, doEnv)
				if err != nil {
					return nil, err
				}
				newVals[i] = val
			} else {
				val, _ := doEnv.get(v.name)
				newVals[i] = val
			}
		}
		// Update all at once
		for i, v := range vars {
			doEnv.set(v.name, newVals[i])
		}
	}
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

	env.set("+", &BuiltinFunc{Name: "+", Fn: numericAdd})
	env.set("-", &BuiltinFunc{Name: "-", Fn: numericSub})
	env.set("*", &BuiltinFunc{Name: "*", Fn: numericMul})
	env.set("/", &BuiltinFunc{Name: "/", Fn: numericDiv})

	// Comparisons
	env.set("<", &BuiltinFunc{Name: "<", Fn: makeNumCompare("<", func(a, b float64) bool { return a < b })})
	env.set(">", &BuiltinFunc{Name: ">", Fn: makeNumCompare(">", func(a, b float64) bool { return a > b })})
	env.set("=", &BuiltinFunc{Name: "=", Fn: makeNumCompare("=", func(a, b float64) bool { return a == b })})
	env.set("<=", &BuiltinFunc{Name: "<=", Fn: makeNumCompare("<=", func(a, b float64) bool { return a <= b })})
	env.set(">=", &BuiltinFunc{Name: ">=", Fn: makeNumCompare(">=", func(a, b float64) bool { return a >= b })})

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

	env.set("apply", &BuiltinFunc{Name: "apply", Fn: func(args []Value) (Value, error) {
		if len(args) < 2 {
			return nil, &EvalError{Message: "apply: need at least 2 arguments"}
		}
		fn := args[0]
		// Last arg must be a list; prefix args are prepended
		lastArg := args[len(args)-1]
		var callArgs []Value
		// Collect prefix args (between fn and last arg)
		for _, a := range args[1 : len(args)-1] {
			callArgs = append(callArgs, a)
		}
		// Flatten the last argument (must be a list)
		cur := lastArg
		for {
			if _, ok := cur.(*NilVal); ok {
				break
			}
			p, ok := cur.(*PairVal)
			if !ok {
				return nil, &EvalError{Message: "apply: last argument must be a list"}
			}
			callArgs = append(callArgs, p.Car)
			cur = p.Cdr
		}
		switch f := fn.(type) {
		case *BuiltinFunc:
			return f.Fn(callArgs)
		case *LambdaVal:
			return applyLambda(f, callArgs)
		case *CaseLambdaVal:
			return applyCaseLambda(f, callArgs)
		default:
			return nil, &EvalError{Message: "apply: first argument must be a procedure"}
		}
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
		if !isNumber(args[0]) {
			return nil, &EvalError{Message: "number->string: not a number"}
		}
		return &StringVal{Val: args[0].String()}, nil
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
		return &BoolVal{Val: isNumber(args[0])}, nil
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

	env.set("procedure?", &BuiltinFunc{Name: "procedure?", Fn: func(args []Value) (Value, error) {
		if len(args) != 1 {
			return nil, &EvalError{Message: "procedure?: need 1 argument"}
		}
		switch args[0].(type) {
		case *LambdaVal, *BuiltinFunc, *CaseLambdaVal:
			return &BoolVal{Val: true}, nil
		default:
			return &BoolVal{Val: false}, nil
		}
	}})

	// eq? — identity/simple equality
	env.set("eq?", &BuiltinFunc{Name: "eq?", Fn: func(args []Value) (Value, error) {
		if len(args) != 2 {
			return nil, &EvalError{Message: "eq?: need 2 arguments"}
		}
		return &BoolVal{Val: schemeEq(args[0], args[1])}, nil
	}})

	// equal? — deep structural equality
	env.set("equal?", &BuiltinFunc{Name: "equal?", Fn: func(args []Value) (Value, error) {
		if len(args) != 2 {
			return nil, &EvalError{Message: "equal?: need 2 arguments"}
		}
		return &BoolVal{Val: schemeEqual(args[0], args[1])}, nil
	}})

	// Numeric utilities
	env.set("abs", &BuiltinFunc{Name: "abs", Fn: func(args []Value) (Value, error) {
		if len(args) != 1 {
			return nil, &EvalError{Message: "abs: need 1 argument"}
		}
		n, ok := args[0].(*IntVal)
		if !ok {
			return nil, &EvalError{Message: "abs: not a number"}
		}
		v := n.Val
		if v < 0 {
			v = -v
		}
		return &IntVal{Val: v}, nil
	}})

	env.set("modulo", &BuiltinFunc{Name: "modulo", Fn: func(args []Value) (Value, error) {
		if len(args) != 2 {
			return nil, &EvalError{Message: "modulo: need 2 arguments"}
		}
		a, ok := args[0].(*IntVal)
		if !ok {
			return nil, &EvalError{Message: "modulo: not a number"}
		}
		b, ok := args[1].(*IntVal)
		if !ok {
			return nil, &EvalError{Message: "modulo: not a number"}
		}
		if b.Val == 0 {
			return nil, &EvalError{Message: "modulo: division by zero"}
		}
		r := a.Val % b.Val
		if r != 0 && (r > 0) != (b.Val > 0) {
			r += b.Val
		}
		return &IntVal{Val: r}, nil
	}})

	env.set("remainder", &BuiltinFunc{Name: "remainder", Fn: func(args []Value) (Value, error) {
		if len(args) != 2 {
			return nil, &EvalError{Message: "remainder: need 2 arguments"}
		}
		a, ok := args[0].(*IntVal)
		if !ok {
			return nil, &EvalError{Message: "remainder: not a number"}
		}
		b, ok := args[1].(*IntVal)
		if !ok {
			return nil, &EvalError{Message: "remainder: not a number"}
		}
		if b.Val == 0 {
			return nil, &EvalError{Message: "remainder: division by zero"}
		}
		return &IntVal{Val: a.Val % b.Val}, nil
	}})

	env.set("quotient", &BuiltinFunc{Name: "quotient", Fn: func(args []Value) (Value, error) {
		if len(args) != 2 {
			return nil, &EvalError{Message: "quotient: need 2 arguments"}
		}
		a, ok := args[0].(*IntVal)
		if !ok {
			return nil, &EvalError{Message: "quotient: not a number"}
		}
		b, ok := args[1].(*IntVal)
		if !ok {
			return nil, &EvalError{Message: "quotient: not a number"}
		}
		if b.Val == 0 {
			return nil, &EvalError{Message: "quotient: division by zero"}
		}
		return &IntVal{Val: a.Val / b.Val}, nil
	}})

	env.set("min", &BuiltinFunc{Name: "min", Fn: func(args []Value) (Value, error) {
		if len(args) == 0 {
			return nil, &EvalError{Message: "min: need at least 1 argument"}
		}
		first, ok := args[0].(*IntVal)
		if !ok {
			return nil, &EvalError{Message: "min: not a number"}
		}
		result := first.Val
		for _, a := range args[1:] {
			n, ok := a.(*IntVal)
			if !ok {
				return nil, &EvalError{Message: "min: not a number"}
			}
			if n.Val < result {
				result = n.Val
			}
		}
		return &IntVal{Val: result}, nil
	}})

	env.set("max", &BuiltinFunc{Name: "max", Fn: func(args []Value) (Value, error) {
		if len(args) == 0 {
			return nil, &EvalError{Message: "max: need at least 1 argument"}
		}
		first, ok := args[0].(*IntVal)
		if !ok {
			return nil, &EvalError{Message: "max: not a number"}
		}
		result := first.Val
		for _, a := range args[1:] {
			n, ok := a.(*IntVal)
			if !ok {
				return nil, &EvalError{Message: "max: not a number"}
			}
			if n.Val > result {
				result = n.Val
			}
		}
		return &IntVal{Val: result}, nil
	}})

	env.set("expt", &BuiltinFunc{Name: "expt", Fn: func(args []Value) (Value, error) {
		if len(args) != 2 {
			return nil, &EvalError{Message: "expt: need 2 arguments"}
		}
		base, ok := args[0].(*IntVal)
		if !ok {
			return nil, &EvalError{Message: "expt: not a number"}
		}
		exp, ok := args[1].(*IntVal)
		if !ok {
			return nil, &EvalError{Message: "expt: not a number"}
		}
		var result int64 = 1
		b := base.Val
		e := exp.Val
		if e < 0 {
			return &IntVal{Val: 0}, nil
		}
		for e > 0 {
			if e%2 == 1 {
				result *= b
			}
			b *= b
			e /= 2
		}
		return &IntVal{Val: result}, nil
	}})

	env.set("zero?", &BuiltinFunc{Name: "zero?", Fn: func(args []Value) (Value, error) {
		if len(args) != 1 {
			return nil, &EvalError{Message: "zero?: need 1 argument"}
		}
		n, ok := args[0].(*IntVal)
		if !ok {
			return nil, &EvalError{Message: "zero?: not a number"}
		}
		return &BoolVal{Val: n.Val == 0}, nil
	}})

	env.set("positive?", &BuiltinFunc{Name: "positive?", Fn: func(args []Value) (Value, error) {
		if len(args) != 1 {
			return nil, &EvalError{Message: "positive?: need 1 argument"}
		}
		n, ok := args[0].(*IntVal)
		if !ok {
			return nil, &EvalError{Message: "positive?: not a number"}
		}
		return &BoolVal{Val: n.Val > 0}, nil
	}})

	env.set("negative?", &BuiltinFunc{Name: "negative?", Fn: func(args []Value) (Value, error) {
		if len(args) != 1 {
			return nil, &EvalError{Message: "negative?: need 1 argument"}
		}
		n, ok := args[0].(*IntVal)
		if !ok {
			return nil, &EvalError{Message: "negative?: not a number"}
		}
		return &BoolVal{Val: n.Val < 0}, nil
	}})

	env.set("odd?", &BuiltinFunc{Name: "odd?", Fn: func(args []Value) (Value, error) {
		if len(args) != 1 {
			return nil, &EvalError{Message: "odd?: need 1 argument"}
		}
		n, ok := args[0].(*IntVal)
		if !ok {
			return nil, &EvalError{Message: "odd?: not a number"}
		}
		return &BoolVal{Val: n.Val%2 != 0}, nil
	}})

	env.set("even?", &BuiltinFunc{Name: "even?", Fn: func(args []Value) (Value, error) {
		if len(args) != 1 {
			return nil, &EvalError{Message: "even?: need 1 argument"}
		}
		n, ok := args[0].(*IntVal)
		if !ok {
			return nil, &EvalError{Message: "even?: not a number"}
		}
		return &BoolVal{Val: n.Val%2 == 0}, nil
	}})

	// Exact/inexact predicates and conversions (L11)
	env.set("exact?", &BuiltinFunc{Name: "exact?", Fn: func(args []Value) (Value, error) {
		if len(args) != 1 {
			return nil, &EvalError{Message: "exact?: need 1 argument"}
		}
		return &BoolVal{Val: isExact(args[0])}, nil
	}})

	env.set("inexact?", &BuiltinFunc{Name: "inexact?", Fn: func(args []Value) (Value, error) {
		if len(args) != 1 {
			return nil, &EvalError{Message: "inexact?: need 1 argument"}
		}
		_, ok := args[0].(*FloatVal)
		return &BoolVal{Val: ok}, nil
	}})

	env.set("integer?", &BuiltinFunc{Name: "integer?", Fn: func(args []Value) (Value, error) {
		if len(args) != 1 {
			return nil, &EvalError{Message: "integer?: need 1 argument"}
		}
		switch v := args[0].(type) {
		case *IntVal:
			return &BoolVal{Val: true}, nil
		case *RatVal:
			// 4/2 simplifies to IntVal, so a RatVal is never an integer
			_ = v
			return &BoolVal{Val: false}, nil
		case *FloatVal:
			return &BoolVal{Val: v.Val == float64(int64(v.Val))}, nil
		default:
			return &BoolVal{Val: false}, nil
		}
	}})

	env.set("rational?", &BuiltinFunc{Name: "rational?", Fn: func(args []Value) (Value, error) {
		if len(args) != 1 {
			return nil, &EvalError{Message: "rational?: need 1 argument"}
		}
		return &BoolVal{Val: isExact(args[0])}, nil
	}})

	env.set("exact->inexact", &BuiltinFunc{Name: "exact->inexact", Fn: func(args []Value) (Value, error) {
		if len(args) != 1 {
			return nil, &EvalError{Message: "exact->inexact: need 1 argument"}
		}
		f, ok := toFloat64(args[0])
		if !ok {
			return nil, &EvalError{Message: "exact->inexact: not a number"}
		}
		return &FloatVal{Val: f}, nil
	}})

	env.set("inexact->exact", &BuiltinFunc{Name: "inexact->exact", Fn: func(args []Value) (Value, error) {
		if len(args) != 1 {
			return nil, &EvalError{Message: "inexact->exact: need 1 argument"}
		}
		switch v := args[0].(type) {
		case *IntVal:
			return v, nil
		case *RatVal:
			return v, nil
		case *FloatVal:
			// Convert float to exact rational via continued fraction or simple approach
			// For 0.5 -> 1/2, etc. Use a simple denominator-finding approach.
			return floatToExact(v.Val), nil
		default:
			return nil, &EvalError{Message: "inexact->exact: not a number"}
		}
	}})

	env.set("numerator", &BuiltinFunc{Name: "numerator", Fn: func(args []Value) (Value, error) {
		if len(args) != 1 {
			return nil, &EvalError{Message: "numerator: need 1 argument"}
		}
		num, _, ok := toRational(args[0])
		if !ok {
			return nil, &EvalError{Message: "numerator: not an exact number"}
		}
		return &IntVal{Val: num}, nil
	}})

	env.set("denominator", &BuiltinFunc{Name: "denominator", Fn: func(args []Value) (Value, error) {
		if len(args) != 1 {
			return nil, &EvalError{Message: "denominator: need 1 argument"}
		}
		_, den, ok := toRational(args[0])
		if !ok {
			return nil, &EvalError{Message: "denominator: not an exact number"}
		}
		return &IntVal{Val: den}, nil
	}})

	// List utilities
	env.set("list-ref", &BuiltinFunc{Name: "list-ref", Fn: func(args []Value) (Value, error) {
		if len(args) != 2 {
			return nil, &EvalError{Message: "list-ref: need 2 arguments"}
		}
		idx, ok := args[1].(*IntVal)
		if !ok {
			return nil, &EvalError{Message: "list-ref: index not a number"}
		}
		cur := args[0]
		for i := int64(0); i < idx.Val; i++ {
			p, ok := cur.(*PairVal)
			if !ok {
				return nil, &EvalError{Message: "list-ref: index out of range"}
			}
			cur = p.Cdr
		}
		p, ok := cur.(*PairVal)
		if !ok {
			return nil, &EvalError{Message: "list-ref: index out of range"}
		}
		return p.Car, nil
	}})

	env.set("list-tail", &BuiltinFunc{Name: "list-tail", Fn: func(args []Value) (Value, error) {
		if len(args) != 2 {
			return nil, &EvalError{Message: "list-tail: need 2 arguments"}
		}
		idx, ok := args[1].(*IntVal)
		if !ok {
			return nil, &EvalError{Message: "list-tail: index not a number"}
		}
		cur := args[0]
		for i := int64(0); i < idx.Val; i++ {
			p, ok := cur.(*PairVal)
			if !ok {
				return nil, &EvalError{Message: "list-tail: index out of range"}
			}
			cur = p.Cdr
		}
		return cur, nil
	}})

	env.set("list?", &BuiltinFunc{Name: "list?", Fn: func(args []Value) (Value, error) {
		if len(args) != 1 {
			return nil, &EvalError{Message: "list?: need 1 argument"}
		}
		cur := args[0]
		for {
			if _, ok := cur.(*NilVal); ok {
				return &BoolVal{Val: true}, nil
			}
			p, ok := cur.(*PairVal)
			if !ok {
				return &BoolVal{Val: false}, nil
			}
			cur = p.Cdr
		}
	}})

	env.set("assoc", &BuiltinFunc{Name: "assoc", Fn: func(args []Value) (Value, error) {
		if len(args) != 2 {
			return nil, &EvalError{Message: "assoc: need 2 arguments"}
		}
		key := args[0]
		cur := args[1]
		for {
			if _, ok := cur.(*NilVal); ok {
				return &BoolVal{Val: false}, nil
			}
			p, ok := cur.(*PairVal)
			if !ok {
				return nil, &EvalError{Message: "assoc: not a proper list"}
			}
			entry, ok := p.Car.(*PairVal)
			if !ok {
				return nil, &EvalError{Message: "assoc: entry is not a pair"}
			}
			if schemeEqual(key, entry.Car) {
				return p.Car, nil
			}
			cur = p.Cdr
		}
	}})

	// Built-in map with multiple list support
	env.set("map", &BuiltinFunc{Name: "map", Fn: func(args []Value) (Value, error) {
		if len(args) < 2 {
			return nil, &EvalError{Message: "map: need at least 2 arguments"}
		}
		fn := args[0]
		lists := args[1:]
		var result []Value
		for {
			// Check if any list is exhausted
			callArgs := make([]Value, len(lists))
			done := false
			for i, l := range lists {
				if _, ok := l.(*NilVal); ok {
					done = true
					break
				}
				p, ok := l.(*PairVal)
				if !ok {
					return nil, &EvalError{Message: "map: not a proper list"}
				}
				callArgs[i] = p.Car
				lists[i] = p.Cdr
			}
			if done {
				break
			}
			var val Value
			var err error
			switch f := fn.(type) {
			case *BuiltinFunc:
				val, err = f.Fn(callArgs)
			case *LambdaVal:
				val, err = applyLambda(f, callArgs)
			case *CaseLambdaVal:
				val, err = applyCaseLambda(f, callArgs)
			default:
				return nil, &EvalError{Message: "map: first argument must be a procedure"}
			}
			if err != nil {
				return nil, err
			}
			result = append(result, val)
		}
		var list Value = &NilVal{}
		for i := len(result) - 1; i >= 0; i-- {
			list = &PairVal{Car: result[i], Cdr: list}
		}
		return list, nil
	}})

	// Character operations
	env.set("char-alphabetic?", &BuiltinFunc{Name: "char-alphabetic?", Fn: func(args []Value) (Value, error) {
		if len(args) != 1 {
			return nil, &EvalError{Message: "char-alphabetic?: need 1 argument"}
		}
		c, ok := args[0].(*CharVal)
		if !ok {
			return nil, &EvalError{Message: "char-alphabetic?: not a character"}
		}
		return &BoolVal{Val: unicode.IsLetter(c.Val)}, nil
	}})

	env.set("char-numeric?", &BuiltinFunc{Name: "char-numeric?", Fn: func(args []Value) (Value, error) {
		if len(args) != 1 {
			return nil, &EvalError{Message: "char-numeric?: need 1 argument"}
		}
		c, ok := args[0].(*CharVal)
		if !ok {
			return nil, &EvalError{Message: "char-numeric?: not a character"}
		}
		return &BoolVal{Val: unicode.IsDigit(c.Val)}, nil
	}})

	env.set("char-upcase", &BuiltinFunc{Name: "char-upcase", Fn: func(args []Value) (Value, error) {
		if len(args) != 1 {
			return nil, &EvalError{Message: "char-upcase: need 1 argument"}
		}
		c, ok := args[0].(*CharVal)
		if !ok {
			return nil, &EvalError{Message: "char-upcase: not a character"}
		}
		return &CharVal{Val: unicode.ToUpper(c.Val)}, nil
	}})

	env.set("char-downcase", &BuiltinFunc{Name: "char-downcase", Fn: func(args []Value) (Value, error) {
		if len(args) != 1 {
			return nil, &EvalError{Message: "char-downcase: need 1 argument"}
		}
		c, ok := args[0].(*CharVal)
		if !ok {
			return nil, &EvalError{Message: "char-downcase: not a character"}
		}
		return &CharVal{Val: unicode.ToLower(c.Val)}, nil
	}})

	env.set("char=?", &BuiltinFunc{Name: "char=?", Fn: func(args []Value) (Value, error) {
		if len(args) != 2 {
			return nil, &EvalError{Message: "char=?: need 2 arguments"}
		}
		a, ok := args[0].(*CharVal)
		if !ok {
			return nil, &EvalError{Message: "char=?: not a character"}
		}
		b, ok := args[1].(*CharVal)
		if !ok {
			return nil, &EvalError{Message: "char=?: not a character"}
		}
		return &BoolVal{Val: a.Val == b.Val}, nil
	}})

	env.set("char<?", &BuiltinFunc{Name: "char<?", Fn: func(args []Value) (Value, error) {
		if len(args) != 2 {
			return nil, &EvalError{Message: "char<?: need 2 arguments"}
		}
		a, ok := args[0].(*CharVal)
		if !ok {
			return nil, &EvalError{Message: "char<?: not a character"}
		}
		b, ok := args[1].(*CharVal)
		if !ok {
			return nil, &EvalError{Message: "char<?: not a character"}
		}
		return &BoolVal{Val: a.Val < b.Val}, nil
	}})

	// String comparison operations
	env.set("string=?", &BuiltinFunc{Name: "string=?", Fn: func(args []Value) (Value, error) {
		if len(args) != 2 {
			return nil, &EvalError{Message: "string=?: need 2 arguments"}
		}
		a, ok := args[0].(*StringVal)
		if !ok {
			return nil, &EvalError{Message: "string=?: not a string"}
		}
		b, ok := args[1].(*StringVal)
		if !ok {
			return nil, &EvalError{Message: "string=?: not a string"}
		}
		return &BoolVal{Val: a.Val == b.Val}, nil
	}})

	env.set("string<?", &BuiltinFunc{Name: "string<?", Fn: func(args []Value) (Value, error) {
		if len(args) != 2 {
			return nil, &EvalError{Message: "string<?: need 2 arguments"}
		}
		a, ok := args[0].(*StringVal)
		if !ok {
			return nil, &EvalError{Message: "string<?: not a string"}
		}
		b, ok := args[1].(*StringVal)
		if !ok {
			return nil, &EvalError{Message: "string<?: not a string"}
		}
		return &BoolVal{Val: a.Val < b.Val}, nil
	}})

	env.set("string-ci=?", &BuiltinFunc{Name: "string-ci=?", Fn: func(args []Value) (Value, error) {
		if len(args) != 2 {
			return nil, &EvalError{Message: "string-ci=?: need 2 arguments"}
		}
		a, ok := args[0].(*StringVal)
		if !ok {
			return nil, &EvalError{Message: "string-ci=?: not a string"}
		}
		b, ok := args[1].(*StringVal)
		if !ok {
			return nil, &EvalError{Message: "string-ci=?: not a string"}
		}
		return &BoolVal{Val: strings.EqualFold(a.Val, b.Val)}, nil
	}})

	env.set("string-upcase", &BuiltinFunc{Name: "string-upcase", Fn: func(args []Value) (Value, error) {
		if len(args) != 1 {
			return nil, &EvalError{Message: "string-upcase: need 1 argument"}
		}
		s, ok := args[0].(*StringVal)
		if !ok {
			return nil, &EvalError{Message: "string-upcase: not a string"}
		}
		return &StringVal{Val: strings.ToUpper(s.Val)}, nil
	}})

	env.set("string-downcase", &BuiltinFunc{Name: "string-downcase", Fn: func(args []Value) (Value, error) {
		if len(args) != 1 {
			return nil, &EvalError{Message: "string-downcase: need 1 argument"}
		}
		s, ok := args[0].(*StringVal)
		if !ok {
			return nil, &EvalError{Message: "string-downcase: not a string"}
		}
		return &StringVal{Val: strings.ToLower(s.Val)}, nil
	}})

	// eqv?
	env.set("eqv?", &BuiltinFunc{Name: "eqv?", Fn: func(args []Value) (Value, error) {
		if len(args) != 2 {
			return nil, &EvalError{Message: "eqv?: need 2 arguments"}
		}
		return &BoolVal{Val: schemeEqv(args[0], args[1])}, nil
	}})

	// Vector operations
	env.set("vector", &BuiltinFunc{Name: "vector", Fn: func(args []Value) (Value, error) {
		elems := make([]Value, len(args))
		copy(elems, args)
		return &VectorVal{Elems: elems}, nil
	}})

	env.set("make-vector", &BuiltinFunc{Name: "make-vector", Fn: func(args []Value) (Value, error) {
		if len(args) < 1 || len(args) > 2 {
			return nil, &EvalError{Message: "make-vector: need 1 or 2 arguments"}
		}
		n, ok := args[0].(*IntVal)
		if !ok {
			return nil, &EvalError{Message: "make-vector: first argument must be integer"}
		}
		var fill Value = &IntVal{Val: 0}
		if len(args) == 2 {
			fill = args[1]
		}
		elems := make([]Value, n.Val)
		for i := range elems {
			elems[i] = fill
		}
		return &VectorVal{Elems: elems}, nil
	}})

	env.set("vector-ref", &BuiltinFunc{Name: "vector-ref", Fn: func(args []Value) (Value, error) {
		if len(args) != 2 {
			return nil, &EvalError{Message: "vector-ref: need 2 arguments"}
		}
		vec, ok := args[0].(*VectorVal)
		if !ok {
			return nil, &EvalError{Message: "vector-ref: not a vector"}
		}
		idx, ok := args[1].(*IntVal)
		if !ok {
			return nil, &EvalError{Message: "vector-ref: index must be integer"}
		}
		if idx.Val < 0 || idx.Val >= int64(len(vec.Elems)) {
			return nil, &EvalError{Message: "vector-ref: index out of range"}
		}
		return vec.Elems[idx.Val], nil
	}})

	env.set("vector-set!", &BuiltinFunc{Name: "vector-set!", Fn: func(args []Value) (Value, error) {
		if len(args) != 3 {
			return nil, &EvalError{Message: "vector-set!: need 3 arguments"}
		}
		vec, ok := args[0].(*VectorVal)
		if !ok {
			return nil, &EvalError{Message: "vector-set!: not a vector"}
		}
		idx, ok := args[1].(*IntVal)
		if !ok {
			return nil, &EvalError{Message: "vector-set!: index must be integer"}
		}
		if idx.Val < 0 || idx.Val >= int64(len(vec.Elems)) {
			return nil, &EvalError{Message: "vector-set!: index out of range"}
		}
		vec.Elems[idx.Val] = args[2]
		return &VoidVal{}, nil
	}})

	env.set("vector-length", &BuiltinFunc{Name: "vector-length", Fn: func(args []Value) (Value, error) {
		if len(args) != 1 {
			return nil, &EvalError{Message: "vector-length: need 1 argument"}
		}
		vec, ok := args[0].(*VectorVal)
		if !ok {
			return nil, &EvalError{Message: "vector-length: not a vector"}
		}
		return &IntVal{Val: int64(len(vec.Elems))}, nil
	}})

	env.set("vector?", &BuiltinFunc{Name: "vector?", Fn: func(args []Value) (Value, error) {
		if len(args) != 1 {
			return nil, &EvalError{Message: "vector?: need 1 argument"}
		}
		_, ok := args[0].(*VectorVal)
		return &BoolVal{Val: ok}, nil
	}})

	env.set("vector->list", &BuiltinFunc{Name: "vector->list", Fn: func(args []Value) (Value, error) {
		if len(args) != 1 {
			return nil, &EvalError{Message: "vector->list: need 1 argument"}
		}
		vec, ok := args[0].(*VectorVal)
		if !ok {
			return nil, &EvalError{Message: "vector->list: not a vector"}
		}
		var result Value = &NilVal{}
		for i := len(vec.Elems) - 1; i >= 0; i-- {
			result = &PairVal{Car: vec.Elems[i], Cdr: result}
		}
		return result, nil
	}})

	env.set("list->vector", &BuiltinFunc{Name: "list->vector", Fn: func(args []Value) (Value, error) {
		if len(args) != 1 {
			return nil, &EvalError{Message: "list->vector: need 1 argument"}
		}
		var elems []Value
		cur := args[0]
		for {
			switch v := cur.(type) {
			case *PairVal:
				elems = append(elems, v.Car)
				cur = v.Cdr
				continue
			case *NilVal:
				return &VectorVal{Elems: elems}, nil
			default:
				return nil, &EvalError{Message: "list->vector: not a proper list"}
			}
		}
	}})

	return env
}

// anyInexact returns true if any arg is a FloatVal.
func anyInexact(args []Value) bool {
	for _, a := range args {
		if _, ok := a.(*FloatVal); ok {
			return true
		}
	}
	return false
}

func numericAdd(args []Value) (Value, error) {
	if anyInexact(args) {
		var sum float64
		for _, a := range args {
			f, ok := toFloat64(a)
			if !ok {
				return nil, &EvalError{Message: "+: not a number"}
			}
			sum += f
		}
		return &FloatVal{Val: sum}, nil
	}
	// All exact
	var rn, rd int64 = 0, 1
	for _, a := range args {
		an, ad, ok := toRational(a)
		if !ok {
			return nil, &EvalError{Message: "+: not a number"}
		}
		rn = rn*ad + an*rd
		rd = rd * ad
		g := gcd(rn, rd)
		rn /= g
		rd /= g
	}
	return makeRat(rn, rd), nil
}

func numericSub(args []Value) (Value, error) {
	if len(args) == 0 {
		return nil, &EvalError{Message: "-: need at least 1 argument"}
	}
	if len(args) == 1 {
		switch v := args[0].(type) {
		case *IntVal:
			return &IntVal{Val: -v.Val}, nil
		case *FloatVal:
			return &FloatVal{Val: -v.Val}, nil
		case *RatVal:
			return &RatVal{Num: -v.Num, Den: v.Den}, nil
		default:
			return nil, &EvalError{Message: "-: not a number"}
		}
	}
	if anyInexact(args) {
		f0, ok := toFloat64(args[0])
		if !ok {
			return nil, &EvalError{Message: "-: not a number"}
		}
		for _, a := range args[1:] {
			f, ok := toFloat64(a)
			if !ok {
				return nil, &EvalError{Message: "-: not a number"}
			}
			f0 -= f
		}
		return &FloatVal{Val: f0}, nil
	}
	rn, rd, ok := toRational(args[0])
	if !ok {
		return nil, &EvalError{Message: "-: not a number"}
	}
	for _, a := range args[1:] {
		an, ad, ok := toRational(a)
		if !ok {
			return nil, &EvalError{Message: "-: not a number"}
		}
		rn = rn*ad - an*rd
		rd = rd * ad
		g := gcd(rn, rd)
		rn /= g
		rd /= g
	}
	return makeRat(rn, rd), nil
}

func numericMul(args []Value) (Value, error) {
	if anyInexact(args) {
		product := 1.0
		for _, a := range args {
			f, ok := toFloat64(a)
			if !ok {
				return nil, &EvalError{Message: "*: not a number"}
			}
			product *= f
		}
		return &FloatVal{Val: product}, nil
	}
	var rn, rd int64 = 1, 1
	for _, a := range args {
		an, ad, ok := toRational(a)
		if !ok {
			return nil, &EvalError{Message: "*: not a number"}
		}
		rn *= an
		rd *= ad
		g := gcd(rn, rd)
		rn /= g
		rd /= g
	}
	return makeRat(rn, rd), nil
}

func numericDiv(args []Value) (Value, error) {
	if len(args) < 2 {
		return nil, &EvalError{Message: "/: need at least 2 arguments"}
	}
	if anyInexact(args) {
		f0, ok := toFloat64(args[0])
		if !ok {
			return nil, &EvalError{Message: "/: not a number"}
		}
		for _, a := range args[1:] {
			f, ok := toFloat64(a)
			if !ok {
				return nil, &EvalError{Message: "/: not a number"}
			}
			if f == 0 {
				return nil, &EvalError{Message: "/: division by zero"}
			}
			f0 /= f
		}
		return &FloatVal{Val: f0}, nil
	}
	rn, rd, ok := toRational(args[0])
	if !ok {
		return nil, &EvalError{Message: "/: not a number"}
	}
	for _, a := range args[1:] {
		an, ad, ok := toRational(a)
		if !ok {
			return nil, &EvalError{Message: "/: not a number"}
		}
		if an == 0 {
			return nil, &EvalError{Message: "/: division by zero"}
		}
		rn *= ad
		rd *= an
		g := gcd(rn, rd)
		rn /= g
		rd /= g
	}
	return makeRat(rn, rd), nil
}

func makeNumCompare(name string, op func(float64, float64) bool) func([]Value) (Value, error) {
	return func(args []Value) (Value, error) {
		if len(args) < 2 {
			return nil, &EvalError{Message: fmt.Sprintf("%s: need at least 2 arguments", name)}
		}
		for i := 0; i < len(args)-1; i++ {
			a, ok := toFloat64(args[i])
			if !ok {
				return nil, &EvalError{Message: fmt.Sprintf("%s: not a number", name)}
			}
			b, ok := toFloat64(args[i+1])
			if !ok {
				return nil, &EvalError{Message: fmt.Sprintf("%s: not a number", name)}
			}
			if !op(a, b) {
				return &BoolVal{Val: false}, nil
			}
		}
		return &BoolVal{Val: true}, nil
	}
}

// floatToExact converts a float64 to an exact rational.
func floatToExact(f float64) Value {
	if f == float64(int64(f)) {
		return &IntVal{Val: int64(f)}
	}
	// Use a simple approach: multiply by increasing powers of 10 until we get an integer
	num := f
	den := int64(1)
	for i := 0; i < 15; i++ {
		if num == float64(int64(num)) {
			break
		}
		num *= 10
		den *= 10
	}
	return makeRat(int64(num), den)
}

func schemeEq(a, b Value) bool {
	switch av := a.(type) {
	case *IntVal:
		if bv, ok := b.(*IntVal); ok {
			return av.Val == bv.Val
		}
	case *FloatVal:
		if bv, ok := b.(*FloatVal); ok {
			return av.Val == bv.Val
		}
	case *RatVal:
		if bv, ok := b.(*RatVal); ok {
			return av.Num == bv.Num && av.Den == bv.Den
		}
	case *BoolVal:
		if bv, ok := b.(*BoolVal); ok {
			return av.Val == bv.Val
		}
	case *SymbolVal:
		if bv, ok := b.(*SymbolVal); ok {
			return av.Val == bv.Val
		}
	case *CharVal:
		if bv, ok := b.(*CharVal); ok {
			return av.Val == bv.Val
		}
	case *NilVal:
		_, ok := b.(*NilVal)
		return ok
	case *StringVal:
		if bv, ok := b.(*StringVal); ok {
			return av == bv // pointer identity
		}
	case *PairVal:
		return a == b // pointer identity
	case *VoidVal:
		_, ok := b.(*VoidVal)
		return ok
	case *VectorVal:
		return a == b // pointer identity
	}
	return false
}

// schemeEqv is like eq? but compares numbers by value and characters by value.
func schemeEqv(a, b Value) bool {
	return schemeEq(a, b)
}

func schemeEqual(a, b Value) bool {
	switch av := a.(type) {
	case *PairVal:
		if bv, ok := b.(*PairVal); ok {
			return schemeEqual(av.Car, bv.Car) && schemeEqual(av.Cdr, bv.Cdr)
		}
		return false
	case *StringVal:
		if bv, ok := b.(*StringVal); ok {
			return av.Val == bv.Val
		}
		return false
	case *VectorVal:
		if bv, ok := b.(*VectorVal); ok {
			if len(av.Elems) != len(bv.Elems) {
				return false
			}
			for i := range av.Elems {
				if !schemeEqual(av.Elems[i], bv.Elems[i]) {
					return false
				}
			}
			return true
		}
		return false
	case *NilVal:
		_, ok := b.(*NilVal)
		return ok
	default:
		return schemeEq(a, b)
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

// evalDefineRecordType implements R7RS define-record-type.
// (define-record-type <name> (constructor field ...) predicate (field accessor) ...)
func evalDefineRecordType(e *ListExpr, env *Env) (Value, error) {
	// Minimum: type-name, constructor, predicate, at least one field spec
	if len(e.Elems) < 4 {
		return nil, &EvalError{Message: "define-record-type: too few arguments"}
	}

	// 1. Type name (e.g., <point>)
	typeSym, ok := e.Elems[1].(*SymbolExpr)
	if !ok {
		return nil, &EvalError{Message: "define-record-type: expected type name symbol"}
	}
	tag := &RecordTypeTag{Name: typeSym.Name}

	// 2. Constructor spec: (make-point x y)
	ctorList, ok := e.Elems[2].(*ListExpr)
	if !ok || len(ctorList.Elems) < 1 {
		return nil, &EvalError{Message: "define-record-type: expected constructor spec"}
	}
	ctorName, ok := ctorList.Elems[0].(*SymbolExpr)
	if !ok {
		return nil, &EvalError{Message: "define-record-type: expected constructor name"}
	}
	var ctorFields []string
	for _, fe := range ctorList.Elems[1:] {
		fs, ok := fe.(*SymbolExpr)
		if !ok {
			return nil, &EvalError{Message: "define-record-type: expected field name in constructor"}
		}
		ctorFields = append(ctorFields, fs.Name)
	}

	// 3. Predicate name
	predSym, ok := e.Elems[3].(*SymbolExpr)
	if !ok {
		return nil, &EvalError{Message: "define-record-type: expected predicate name"}
	}

	// 4. Field specs: (field-name accessor-name)
	type fieldSpec struct {
		fieldName    string
		accessorName string
	}
	var fields []fieldSpec
	for _, fe := range e.Elems[4:] {
		fl, ok := fe.(*ListExpr)
		if !ok || len(fl.Elems) < 2 {
			return nil, &EvalError{Message: "define-record-type: expected field spec (field accessor)"}
		}
		fn, ok := fl.Elems[0].(*SymbolExpr)
		if !ok {
			return nil, &EvalError{Message: "define-record-type: expected field name"}
		}
		an, ok := fl.Elems[1].(*SymbolExpr)
		if !ok {
			return nil, &EvalError{Message: "define-record-type: expected accessor name"}
		}
		fields = append(fields, fieldSpec{fn.Name, an.Name})
	}

	// Define constructor
	capturedTag := tag
	capturedFields := ctorFields
	env.set(ctorName.Name, &BuiltinFunc{
		Name: ctorName.Name,
		Fn: func(args []Value) (Value, error) {
			if len(args) != len(capturedFields) {
				return nil, &EvalError{Message: fmt.Sprintf("%s: expected %d arguments, got %d", ctorName.Name, len(capturedFields), len(args))}
			}
			rec := &RecordVal{Type: capturedTag, Fields: make(map[string]Value, len(capturedFields))}
			for i, name := range capturedFields {
				rec.Fields[name] = args[i]
			}
			return rec, nil
		},
	})

	// Define predicate
	env.set(predSym.Name, &BuiltinFunc{
		Name: predSym.Name,
		Fn: func(args []Value) (Value, error) {
			if len(args) != 1 {
				return nil, &EvalError{Message: fmt.Sprintf("%s: expected 1 argument", predSym.Name)}
			}
			rec, ok := args[0].(*RecordVal)
			return &BoolVal{Val: ok && rec.Type == capturedTag}, nil
		},
	})

	// Define accessors
	for _, fs := range fields {
		fname := fs.fieldName
		aname := fs.accessorName
		env.set(aname, &BuiltinFunc{
			Name: aname,
			Fn: func(args []Value) (Value, error) {
				if len(args) != 1 {
					return nil, &EvalError{Message: fmt.Sprintf("%s: expected 1 argument", aname)}
				}
				rec, ok := args[0].(*RecordVal)
				if !ok || rec.Type != capturedTag {
					return nil, &EvalError{Message: fmt.Sprintf("%s: not a %s record", aname, capturedTag.Name)}
				}
				v, ok := rec.Fields[fname]
				if !ok {
					return nil, &EvalError{Message: fmt.Sprintf("%s: field %s not found", aname, fname)}
				}
				return v, nil
			},
		})
	}

	return &VoidVal{}, nil
}
