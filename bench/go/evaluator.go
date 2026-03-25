package ming

import (
	"fmt"
	"math"
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

// contInvokePanic is panicked when a continuation is invoked.
type contInvokePanic struct {
	cont  *ContinuationVal
	value Value
}

// raisePanic is panicked when raise is called.
type raisePanic struct {
	value Value
}

// evalState tracks top-level evaluation context for continuation support.
type evalState struct {
	topExprs       []Expr
	topEnv         *Env
	curIdx         int
	callccOverride *Value // non-nil when a continuation restart needs call/cc to return this value
	bodyExprs      []Expr // current innermost body sequence (for body-level continuation capture)
	bodyEnv        *Env   // environment for bodyExprs
}

var currentEvalState *evalState
var currentWindStack []windEntry

// contFrameStack tracks remaining body-sequence expressions for continuation capture.
var contFrameStack []contFrame

// nonBodyEvalDepth tracks whether call/cc is at body level.
// 0 = body level, >0 = inside a subexpression.
// evalExpr increments on entry and decrements on exit.
// Body eval loops counteract by decrementing before and incrementing after evalExpr calls.
var nonBodyEvalDepth int

func pushContFrame(f contFrame) {
	contFrameStack = append(contFrameStack, f)
}

func popContFrame() {
	if len(contFrameStack) > 0 {
		contFrameStack = contFrameStack[:len(contFrameStack)-1]
	}
}

func evalExpr(expr Expr, env *Env) (Value, error) {
	nonBodyEvalDepth++
	defer func() { nonBodyEvalDepth-- }()
	for {
	switch e := expr.(type) {
	case *NumberExpr:
		return &IntVal{Val: e.Val}, nil
	case *FloatExpr:
		return &FloatVal{Val: e.Val}, nil
	case *RationalExpr:
		return makeRat(e.Num, e.Den), nil
	case *StringExpr:
		return &StringVal{Val: e.Val, Immutable: true}, nil
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
				if len(e.Elems) < 3 || len(e.Elems) > 4 {
					return nil, &EvalError{Message: fmt.Sprintf("%d:%d: if requires 2 or 3 arguments", sym.Line, sym.Col)}
				}
				cond, err := evalExpr(e.Elems[1], env)
				if err != nil {
					return nil, err
				}
				if isTruthy(cond) {
					expr = e.Elems[2]
					continue
				}
				if len(e.Elems) == 4 {
					expr = e.Elems[3]
					continue
				}
				return &VoidVal{}, nil
			case "lambda":
				return evalLambda(e, env)
			case "case-lambda":
				return evalCaseLambda(e, env)
			case "quote":
				if len(e.Elems) != 2 {
					return nil, &EvalError{Message: fmt.Sprintf("%d:%d: quote requires 1 argument", sym.Line, sym.Col)}
				}
				return quoteExpr(e.Elems[1])
			case "quasiquote":
				if len(e.Elems) != 2 {
					return nil, &EvalError{Message: "quasiquote requires 1 argument"}
				}
				return evalQuasiquote(e.Elems[1], env, 0)
			case "dynamic-wind":
				return evalDynamicWind(e, env)
			case "guard":
				return evalGuard(e, env)
			case "with-exception-handler":
				return evalWithExceptionHandler(e, env)
			case "begin":
				if len(e.Elems) < 2 {
					return &VoidVal{}, nil
				}
				beginBody := e.Elems[1:]
				for i, bodyExpr := range beginBody[:len(beginBody)-1] {
					pushContFrame(contFrame{remainExprs: beginBody[i+1:], env: env})
					nonBodyEvalDepth--
					_, err := evalExpr(bodyExpr, env)
					nonBodyEvalDepth++
					popContFrame()
					if err != nil {
						return nil, err
					}
				}
				expr = beginBody[len(beginBody)-1]
				continue
			case "let":
				if len(e.Elems) < 3 {
					return nil, &EvalError{Message: fmt.Sprintf("%d:%d: let requires bindings and body", e.Line, e.Col)}
				}
				// Named let: (let name ((var init) ...) body ...)
				if nameSym, ok := e.Elems[1].(*SymbolExpr); ok {
					if len(e.Elems) < 4 {
						return nil, &EvalError{Message: fmt.Sprintf("%d:%d: named let requires bindings and body", e.Line, e.Col)}
					}
					nlBindList, ok := e.Elems[2].(*ListExpr)
					if !ok {
						return nil, &EvalError{Message: fmt.Sprintf("%d:%d: let: expected binding list", e.Line, e.Col)}
					}
					nlParams := make([]string, len(nlBindList.Elems))
					nlInitVals := make([]Value, len(nlBindList.Elems))
					for i, b := range nlBindList.Elems {
						pair, ok := b.(*ListExpr)
						if !ok || len(pair.Elems) != 2 {
							return nil, &EvalError{Message: fmt.Sprintf("%d:%d: let: bad binding", e.Line, e.Col)}
						}
						ps, ok := pair.Elems[0].(*SymbolExpr)
						if !ok {
							return nil, &EvalError{Message: fmt.Sprintf("%d:%d: let: expected symbol", e.Line, e.Col)}
						}
						nlParams[i] = ps.Name
						v, verr := evalExpr(pair.Elems[1], env)
						if verr != nil {
							return nil, verr
						}
						nlInitVals[i] = v
					}
					nlEnv := newEnv(env)
					lambda := &LambdaVal{Params: nlParams, Body: e.Elems[3:], Env: nlEnv}
					nlEnv.set(nameSym.Name, lambda)
					callEnv := newEnv(nlEnv)
					for i, p := range nlParams {
						callEnv.set(p, nlInitVals[i])
					}
					nlBody := e.Elems[3:]
					for i, bodyExpr := range nlBody[:len(nlBody)-1] {
						pushContFrame(contFrame{remainExprs: nlBody[i+1:], env: callEnv})
						nonBodyEvalDepth--
						_, berr := evalExpr(bodyExpr, callEnv)
						nonBodyEvalDepth++
						popContFrame()
						if berr != nil {
							return nil, berr
						}
					}
					expr = nlBody[len(nlBody)-1]
					env = callEnv
					continue
				}
				// Regular let: (let ((var init) ...) body ...)
				rlBindList, ok := e.Elems[1].(*ListExpr)
				if !ok {
					return nil, &EvalError{Message: fmt.Sprintf("%d:%d: let: expected binding list", e.Line, e.Col)}
				}
				rlEnv := newEnv(env)
				for _, b := range rlBindList.Elems {
					pair, ok := b.(*ListExpr)
					if !ok || len(pair.Elems) != 2 {
						return nil, &EvalError{Message: fmt.Sprintf("%d:%d: let: bad binding", e.Line, e.Col)}
					}
					ps, ok := pair.Elems[0].(*SymbolExpr)
					if !ok {
						return nil, &EvalError{Message: fmt.Sprintf("%d:%d: let: expected symbol", e.Line, e.Col)}
					}
					v, verr := evalExpr(pair.Elems[1], env)
					if verr != nil {
						return nil, verr
					}
					rlEnv.set(ps.Name, v)
				}
				rlBody := e.Elems[2:]
				if state := currentEvalState; state != nil {
					state.bodyExprs = rlBody
					state.bodyEnv = rlEnv
				}
				for i, bodyExpr := range rlBody[:len(rlBody)-1] {
					pushContFrame(contFrame{remainExprs: rlBody[i+1:], env: rlEnv})
					nonBodyEvalDepth--
					_, berr := evalExpr(bodyExpr, rlEnv)
					nonBodyEvalDepth++
					popContFrame()
					if berr != nil {
						return nil, berr
					}
				}
				expr = rlBody[len(rlBody)-1]
				env = rlEnv
				continue
			case "let*":
				if len(e.Elems) < 3 {
					return nil, &EvalError{Message: fmt.Sprintf("%d:%d: let* requires bindings and body", e.Line, e.Col)}
				}
				lsBindList, ok := e.Elems[1].(*ListExpr)
				if !ok {
					return nil, &EvalError{Message: fmt.Sprintf("%d:%d: let*: expected binding list", e.Line, e.Col)}
				}
				lsEnv := newEnv(env)
				for _, b := range lsBindList.Elems {
					pair, ok := b.(*ListExpr)
					if !ok || len(pair.Elems) != 2 {
						return nil, &EvalError{Message: fmt.Sprintf("%d:%d: let*: bad binding", e.Line, e.Col)}
					}
					ps, ok := pair.Elems[0].(*SymbolExpr)
					if !ok {
						return nil, &EvalError{Message: fmt.Sprintf("%d:%d: let*: expected symbol", e.Line, e.Col)}
					}
					v, verr := evalExpr(pair.Elems[1], lsEnv)
					if verr != nil {
						return nil, verr
					}
					lsEnv.set(ps.Name, v)
				}
				lsBody := e.Elems[2:]
				for i, bodyExpr := range lsBody[:len(lsBody)-1] {
					pushContFrame(contFrame{remainExprs: lsBody[i+1:], env: lsEnv})
					nonBodyEvalDepth--
					_, berr := evalExpr(bodyExpr, lsEnv)
					nonBodyEvalDepth++
					popContFrame()
					if berr != nil {
						return nil, berr
					}
				}
				expr = lsBody[len(lsBody)-1]
				env = lsEnv
				continue
			case "cond":
				condHandled := false
				for _, clause := range e.Elems[1:] {
					cl, ok := clause.(*ListExpr)
					if !ok || len(cl.Elems) == 0 {
						return nil, &EvalError{Message: fmt.Sprintf("%d:%d: cond: bad clause", e.Line, e.Col)}
					}
					if csym, ok := cl.Elems[0].(*SymbolExpr); ok && csym.Name == "else" {
						if len(cl.Elems) < 2 {
							condHandled = true
							break
						}
						condBody := cl.Elems[1:]
						for ci, bodyExpr := range condBody[:len(condBody)-1] {
							pushContFrame(contFrame{remainExprs: condBody[ci+1:], env: env})
							nonBodyEvalDepth--
							_, cerr := evalExpr(bodyExpr, env)
							nonBodyEvalDepth++
							popContFrame()
							if cerr != nil {
								return nil, cerr
							}
						}
						expr = condBody[len(condBody)-1]
						condHandled = true
						break
					}
					test, terr := evalExpr(cl.Elems[0], env)
					if terr != nil {
						return nil, terr
					}
					if isTruthy(test) {
						if len(cl.Elems) == 1 {
							return test, nil
						}
						// Handle => clause: (test => proc)
						if len(cl.Elems) == 3 {
							if arrow, ok := cl.Elems[1].(*SymbolExpr); ok && arrow.Name == "=>" {
								proc, perr := evalExpr(cl.Elems[2], env)
								if perr != nil {
									return nil, perr
								}
								return applyCallable(proc, []Value{test})
							}
						}
						condBody2 := cl.Elems[1:]
						for ci, bodyExpr := range condBody2[:len(condBody2)-1] {
							pushContFrame(contFrame{remainExprs: condBody2[ci+1:], env: env})
							nonBodyEvalDepth--
							_, cerr := evalExpr(bodyExpr, env)
							nonBodyEvalDepth++
							popContFrame()
							if cerr != nil {
								return nil, cerr
							}
						}
						expr = cl.Elems[len(cl.Elems)-1]
						condHandled = true
						break
					}
				}
				if condHandled {
					continue
				}
				return &VoidVal{}, nil
			case "and":
				andArgs := e.Elems[1:]
				if len(andArgs) == 0 {
					return &BoolVal{Val: true}, nil
				}
				for _, a := range andArgs[:len(andArgs)-1] {
					v, err := evalExpr(a, env)
					if err != nil {
						return nil, err
					}
					if !isTruthy(v) {
						return v, nil
					}
				}
				expr = andArgs[len(andArgs)-1]
				continue
			case "or":
				orArgs := e.Elems[1:]
				if len(orArgs) == 0 {
					return &BoolVal{Val: false}, nil
				}
				for _, a := range orArgs[:len(orArgs)-1] {
					v, err := evalExpr(a, env)
					if err != nil {
						return nil, err
					}
					if isTruthy(v) {
						return v, nil
					}
				}
				expr = orArgs[len(orArgs)-1]
				continue
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
				tailExpr, tailEnv, lerr := evalLetrecTail(e, env, false)
				if lerr != nil {
					return nil, lerr
				}
				expr = tailExpr
				env = tailEnv
				continue
			case "letrec*":
				tailExpr, tailEnv, lerr := evalLetrecTail(e, env, true)
				if lerr != nil {
					return nil, lerr
				}
				expr = tailExpr
				env = tailEnv
				continue
			case "case":
				if len(e.Elems) < 3 {
					return nil, &EvalError{Message: fmt.Sprintf("%d:%d: case requires key and clauses", e.Line, e.Col)}
				}
				caseKey, kerr := evalExpr(e.Elems[1], env)
				if kerr != nil {
					return nil, kerr
				}
				caseHandled := false
				for _, clause := range e.Elems[2:] {
					cl, ok := clause.(*ListExpr)
					if !ok || len(cl.Elems) < 2 {
						return nil, &EvalError{Message: fmt.Sprintf("%d:%d: case: bad clause", e.Line, e.Col)}
					}
					if csym, ok := cl.Elems[0].(*SymbolExpr); ok && csym.Name == "else" {
						caseElseBody := cl.Elems[1:]
						for ci, bodyExpr := range caseElseBody[:len(caseElseBody)-1] {
							pushContFrame(contFrame{remainExprs: caseElseBody[ci+1:], env: env})
							nonBodyEvalDepth--
							_, berr := evalExpr(bodyExpr, env)
							nonBodyEvalDepth++
							popContFrame()
							if berr != nil {
								return nil, berr
							}
						}
						expr = caseElseBody[len(caseElseBody)-1]
						caseHandled = true
						break
					}
					datums, ok := cl.Elems[0].(*ListExpr)
					if !ok {
						return nil, &EvalError{Message: fmt.Sprintf("%d:%d: case: expected datum list", e.Line, e.Col)}
					}
					caseMatched := false
					for _, d := range datums.Elems {
						dv, derr := quoteExpr(d)
						if derr != nil {
							return nil, derr
						}
						if schemeEqv(caseKey, dv) {
							caseMatched = true
							break
						}
					}
					if caseMatched {
						caseMatchBody := cl.Elems[1:]
						for ci, bodyExpr := range caseMatchBody[:len(caseMatchBody)-1] {
							pushContFrame(contFrame{remainExprs: caseMatchBody[ci+1:], env: env})
							nonBodyEvalDepth--
							_, berr := evalExpr(bodyExpr, env)
							nonBodyEvalDepth++
							popContFrame()
							if berr != nil {
								return nil, berr
							}
						}
						expr = cl.Elems[len(cl.Elems)-1]
						caseHandled = true
						break
					}
				}
				if caseHandled {
					continue
				}
				return &VoidVal{}, nil
			case "do":
				return evalDo(e, env)
			case "syntax-case":
				result, scerr := evalSyntaxCase(e, env)
				if scerr != nil {
					return nil, scerr
				}
				return result, nil
			case "syntax":
				if len(e.Elems) != 2 {
					return nil, &EvalError{Message: fmt.Sprintf("%d:%d: syntax requires 1 argument", e.Line, e.Col)}
				}
				resultExpr := instantiateSyntaxCaseTemplate(e.Elems[1], env)
				return &SyntaxVal{Expr: resultExpr}, nil
			case "with-syntax":
				result, wserr := evalWithSyntax(e, env)
				if wserr != nil {
					return nil, wserr
				}
				if wsStx, ok := result.(*SyntaxVal); ok {
					_ = wsStx
				}
				return result, nil
			}
			// Check if symbol is bound to a macro
			if v, ok := env.get(sym.Name); ok {
				if macro, ok := v.(*SyntaxRulesVal); ok {
					expanded, merr := expandMacro(macro, e)
					if merr != nil {
						return nil, merr
					}
					expr = expanded
					continue
				}
				if transformer, ok := v.(*MacroTransformerVal); ok {
					expanded, merr := expandSyntaxCaseMacro(transformer, e, env)
					if merr != nil {
						return nil, merr
					}
					expr = expanded
					continue
				}
			}
		}
		// Check if first element is an EnvRefExpr pointing to a macro
		if ref, ok := e.Elems[0].(*EnvRefExpr); ok {
			if v, ok := ref.Env.get(ref.Name); ok {
				if macro, ok := v.(*SyntaxRulesVal); ok {
					expanded, merr := expandMacro(macro, e)
					if merr != nil {
						return nil, merr
					}
					expr = expanded
					continue
				}
				if transformer, ok := v.(*MacroTransformerVal); ok {
					expanded, merr := expandSyntaxCaseMacro(transformer, e, env)
					if merr != nil {
						return nil, merr
					}
					expr = expanded
					continue
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
			callEnv, cerr := setupLambdaCall(f, args)
			if cerr != nil {
				return nil, cerr
			}
			for i, bodyExpr := range f.Body[:len(f.Body)-1] {
				pushContFrame(contFrame{remainExprs: f.Body[i+1:], env: callEnv})
				nonBodyEvalDepth--
				_, berr := evalExpr(bodyExpr, callEnv)
				nonBodyEvalDepth++
				popContFrame()
				if berr != nil {
					return nil, berr
				}
			}
			expr = f.Body[len(f.Body)-1]
			env = callEnv
			continue
		case *CaseLambdaVal:
			clause, cerr := findCaseLambdaClause(f, args)
			if cerr != nil {
				return nil, cerr
			}
			clEnv, cerr2 := setupLambdaCall(clause, args)
			if cerr2 != nil {
				return nil, cerr2
			}
			for i, bodyExpr := range clause.Body[:len(clause.Body)-1] {
				pushContFrame(contFrame{remainExprs: clause.Body[i+1:], env: clEnv})
				nonBodyEvalDepth--
				_, berr := evalExpr(bodyExpr, clEnv)
				nonBodyEvalDepth++
				popContFrame()
				if berr != nil {
					return nil, berr
				}
			}
			expr = clause.Body[len(clause.Body)-1]
			env = clEnv
			continue
		case *CallCCVal:
			if len(args) != 1 {
				return nil, &EvalError{Message: "call/cc: need 1 argument"}
			}
			return handleCallCC(args[0])
		case *ContinuationVal:
			if len(args) == 1 {
				panic(contInvokePanic{cont: f, value: args[0]})
			}
			panic(contInvokePanic{cont: f, value: &ValuesVal{Vals: args}})
		default:
			line, col := e.Elems[0].pos()
			return nil, &EvalError{Message: fmt.Sprintf("%d:%d: not a procedure", line, col)}
		}
	default:
		return nil, &EvalError{Message: "unknown expression type"}
	}
	}
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
		params, rest, perr := parseParams(target.Elems[1:], target.Dot, e.Line, e.Col)
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
func parseParams(elems []Expr, dot Expr, line, col int) ([]string, string, error) {
	var params []string
	var restParam string
	for _, p := range elems {
		ps, ok := p.(*SymbolExpr)
		if !ok {
			return nil, "", &EvalError{Message: fmt.Sprintf("%d:%d: expected symbol in parameter list", line, col)}
		}
		params = append(params, ps.Name)
	}
	if dot != nil {
		rs, ok := dot.(*SymbolExpr)
		if !ok {
			return nil, "", &EvalError{Message: fmt.Sprintf("%d:%d: expected symbol after dot", line, col)}
		}
		restParam = rs.Name
	}
	return params, restParam, nil
}

func evalLambda(e *ListExpr, env *Env) (Value, error) {
	if len(e.Elems) < 3 {
		return nil, &EvalError{Message: fmt.Sprintf("%d:%d: lambda requires params and body", e.Line, e.Col)}
	}
	switch pl := e.Elems[1].(type) {
	case *ListExpr:
		params, rest, err := parseParams(pl.Elems, pl.Dot, e.Line, e.Col)
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
			params, rest, err := parseParams(pl.Elems, pl.Dot, cl.Line, cl.Col)
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
	for i, bodyExpr := range f.Body {
		if i < len(f.Body)-1 {
			pushContFrame(contFrame{remainExprs: f.Body[i+1:], env: callEnv})
		}
		nonBodyEvalDepth--
		var err error
		result, err = evalExpr(bodyExpr, callEnv)
		nonBodyEvalDepth++
		if i < len(f.Body)-1 {
			popContFrame()
		}
		if err != nil {
			return nil, err
		}
	}
	return result, nil
}

// setupLambdaCall creates the call environment for a lambda application without evaluating the body.
func setupLambdaCall(f *LambdaVal, args []Value) (*Env, error) {
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
		var rest Value = &NilVal{}
		for i := len(args) - 1; i >= len(f.Params); i-- {
			rest = &PairVal{Car: args[i], Cdr: rest}
		}
		callEnv.set(f.RestParam, rest)
	}
	return callEnv, nil
}

// findCaseLambdaClause finds the matching clause for a case-lambda application.
func findCaseLambdaClause(f *CaseLambdaVal, args []Value) (*LambdaVal, error) {
	for _, clause := range f.Clauses {
		if clause.RestParam != "" {
			if len(args) >= len(clause.Params) {
				return clause, nil
			}
		} else {
			if len(args) == len(clause.Params) {
				return clause, nil
			}
		}
	}
	return nil, &EvalError{Message: fmt.Sprintf("case-lambda: no matching clause for %d arguments", len(args))}
}

// evalLetrecTail evaluates letrec/letrec* bindings and all but the last body expression,
// returning the last body expression and its environment for TCO.
func evalLetrecTail(e *ListExpr, env *Env, star bool) (Expr, *Env, error) {
	if len(e.Elems) < 3 {
		return nil, nil, &EvalError{Message: fmt.Sprintf("%d:%d: letrec requires bindings and body", e.Line, e.Col)}
	}
	bindList, ok := e.Elems[1].(*ListExpr)
	if !ok {
		return nil, nil, &EvalError{Message: fmt.Sprintf("%d:%d: letrec: expected binding list", e.Line, e.Col)}
	}
	letEnv := newEnv(env)
	names := make([]string, len(bindList.Elems))
	for i, b := range bindList.Elems {
		pair, ok := b.(*ListExpr)
		if !ok || len(pair.Elems) != 2 {
			return nil, nil, &EvalError{Message: fmt.Sprintf("%d:%d: letrec: bad binding", e.Line, e.Col)}
		}
		ps, ok := pair.Elems[0].(*SymbolExpr)
		if !ok {
			return nil, nil, &EvalError{Message: fmt.Sprintf("%d:%d: letrec: expected symbol", e.Line, e.Col)}
		}
		names[i] = ps.Name
		letEnv.set(ps.Name, &VoidVal{})
	}
	if star {
		for i, b := range bindList.Elems {
			pair := b.(*ListExpr)
			v, err := evalExpr(pair.Elems[1], letEnv)
			if err != nil {
				return nil, nil, err
			}
			letEnv.set(names[i], v)
		}
	} else {
		vals := make([]Value, len(bindList.Elems))
		for i, b := range bindList.Elems {
			pair := b.(*ListExpr)
			v, err := evalExpr(pair.Elems[1], letEnv)
			if err != nil {
				return nil, nil, err
			}
			vals[i] = v
		}
		for i, name := range names {
			letEnv.set(name, vals[i])
		}
	}
	body := e.Elems[2:]
	for i, bodyExpr := range body[:len(body)-1] {
		pushContFrame(contFrame{remainExprs: body[i+1:], env: letEnv})
		nonBodyEvalDepth--
		_, err := evalExpr(bodyExpr, letEnv)
		nonBodyEvalDepth++
		popContFrame()
		if err != nil {
			return nil, nil, err
		}
	}
	return body[len(body)-1], letEnv, nil
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
		return &StringVal{Val: e.Val, Immutable: true}, nil
	case *BoolExpr:
		return &BoolVal{Val: e.Val}, nil
	case *CharExpr:
		return &CharVal{Val: e.Val}, nil
	case *SymbolExpr:
		return &SymbolVal{Val: e.Name}, nil
	case *ListExpr:
		if len(e.Elems) == 0 && e.Dot == nil {
			return &NilVal{}, nil
		}
		// Build list, using Dot as final cdr if present
		var result Value
		if e.Dot != nil {
			var err error
			result, err = quoteExpr(e.Dot)
			if err != nil {
				return nil, err
			}
		} else {
			result = &NilVal{}
		}
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

// evalQuasiquote expands a quasiquote expression. depth tracks nesting level.
func evalQuasiquote(expr Expr, env *Env, depth int) (Value, error) {
	switch e := expr.(type) {
	case *ListExpr:
		if len(e.Elems) == 0 {
			return &NilVal{}, nil
		}
		// Check for (unquote x) or (unquote-splicing x)
		if len(e.Elems) == 2 {
			if sym, ok := e.Elems[0].(*SymbolExpr); ok {
				if sym.Name == "unquote" {
					if depth == 0 {
						return evalExpr(e.Elems[1], env)
					}
					// nested quasiquote: decrement depth
					inner, err := evalQuasiquote(e.Elems[1], env, depth-1)
					if err != nil {
						return nil, err
					}
					return &PairVal{Car: &SymbolVal{Val: "unquote"}, Cdr: &PairVal{Car: inner, Cdr: &NilVal{}}}, nil
				}
				if sym.Name == "quasiquote" {
					inner, err := evalQuasiquote(e.Elems[1], env, depth+1)
					if err != nil {
						return nil, err
					}
					return &PairVal{Car: &SymbolVal{Val: "quasiquote"}, Cdr: &PairVal{Car: inner, Cdr: &NilVal{}}}, nil
				}
			}
		}
		// Process list elements, handling splicing
		var result []Value
		for _, elem := range e.Elems {
			if le, ok := elem.(*ListExpr); ok && len(le.Elems) == 2 {
				if sym, ok := le.Elems[0].(*SymbolExpr); ok {
					if sym.Name == "unquote-splicing" {
						if depth == 0 {
							v, err := evalExpr(le.Elems[1], env)
							if err != nil {
								return nil, err
							}
							// Splice the list into result
							for {
								switch p := v.(type) {
								case *PairVal:
									result = append(result, p.Car)
									v = p.Cdr
									continue
								case *NilVal:
								default:
									return nil, &EvalError{Message: "unquote-splicing: expected list"}
								}
								break
							}
							continue
						}
						// nested: decrement depth
						inner, err := evalQuasiquote(le.Elems[1], env, depth-1)
						if err != nil {
							return nil, err
						}
						result = append(result, &PairVal{Car: &SymbolVal{Val: "unquote-splicing"}, Cdr: &PairVal{Car: inner, Cdr: &NilVal{}}})
						continue
					}
				}
			}
			v, err := evalQuasiquote(elem, env, depth)
			if err != nil {
				return nil, err
			}
			result = append(result, v)
		}
		// Build list (with dotted cdr if present)
		var list Value
		if e.Dot != nil {
			var err error
			list, err = evalQuasiquote(e.Dot, env, depth)
			if err != nil {
				return nil, err
			}
		} else {
			list = &NilVal{}
		}
		for i := len(result) - 1; i >= 0; i-- {
			list = &PairVal{Car: result[i], Cdr: list}
		}
		return list, nil
	case *VectorExpr:
		var result []Value
		for _, elem := range e.Elems {
			if le, ok := elem.(*ListExpr); ok && len(le.Elems) == 2 {
				if sym, ok := le.Elems[0].(*SymbolExpr); ok && sym.Name == "unquote-splicing" && depth == 0 {
					v, err := evalExpr(le.Elems[1], env)
					if err != nil {
						return nil, err
					}
					for {
						switch p := v.(type) {
						case *PairVal:
							result = append(result, p.Car)
							v = p.Cdr
							continue
						case *NilVal:
						default:
							return nil, &EvalError{Message: "unquote-splicing: expected list"}
						}
						break
					}
					continue
				}
			}
			v, err := evalQuasiquote(elem, env, depth)
			if err != nil {
				return nil, err
			}
			result = append(result, v)
		}
		return &VectorVal{Elems: result}, nil
	default:
		return quoteExpr(expr)
	}
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
			// Handle => clause: (test => proc)
			if len(cl.Elems) == 3 {
				if arrow, ok := cl.Elems[1].(*SymbolExpr); ok && arrow.Name == "=>" {
					proc, err := evalExpr(cl.Elems[2], env)
					if err != nil {
						return nil, err
					}
					return applyCallable(proc, []Value{test})
				}
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

	env.set("set-car!", &BuiltinFunc{Name: "set-car!", Fn: func(args []Value) (Value, error) {
		if len(args) != 2 {
			return nil, &EvalError{Message: "set-car!: need 2 arguments"}
		}
		p, ok := args[0].(*PairVal)
		if !ok {
			return nil, &EvalError{Message: "set-car!: not a pair"}
		}
		p.Car = args[1]
		return &VoidVal{}, nil
	}})

	env.set("set-cdr!", &BuiltinFunc{Name: "set-cdr!", Fn: func(args []Value) (Value, error) {
		if len(args) != 2 {
			return nil, &EvalError{Message: "set-cdr!: need 2 arguments"}
		}
		p, ok := args[0].(*PairVal)
		if !ok {
			return nil, &EvalError{Message: "set-cdr!: not a pair"}
		}
		p.Cdr = args[1]
		return &VoidVal{}, nil
	}})

	// reverse
	env.set("reverse", &BuiltinFunc{Name: "reverse", Fn: func(args []Value) (Value, error) {
		if len(args) != 1 {
			return nil, &EvalError{Message: "reverse: need 1 argument"}
		}
		var result Value = &NilVal{}
		cur := args[0]
		for {
			if _, ok := cur.(*NilVal); ok {
				return result, nil
			}
			p, ok := cur.(*PairVal)
			if !ok {
				return nil, &EvalError{Message: "reverse: not a proper list"}
			}
			result = &PairVal{Car: p.Car, Cdr: result}
			cur = p.Cdr
		}
	}})

	// for-each
	env.set("for-each", &BuiltinFunc{Name: "for-each", Fn: func(args []Value) (Value, error) {
		if len(args) < 2 {
			return nil, &EvalError{Message: "for-each: need at least 2 arguments"}
		}
		fn := args[0]
		if len(args) == 2 {
			// Single-list case
			cur := args[1]
			for {
				if _, ok := cur.(*NilVal); ok {
					return &VoidVal{}, nil
				}
				p, ok := cur.(*PairVal)
				if !ok {
					return nil, &EvalError{Message: "for-each: not a proper list"}
				}
				_, err := applyCallable(fn, []Value{p.Car})
				if err != nil {
					return nil, err
				}
				cur = p.Cdr
			}
		}
		// Multi-list case
		lists := args[1:]
		for {
			callArgs := make([]Value, len(lists))
			allDone := false
			for i, l := range lists {
				if _, ok := l.(*NilVal); ok {
					allDone = true
					break
				}
				p, ok := l.(*PairVal)
				if !ok {
					return nil, &EvalError{Message: "for-each: not a proper list"}
				}
				callArgs[i] = p.Car
				lists[i] = p.Cdr
			}
			if allDone {
				return &VoidVal{}, nil
			}
			_, err := applyCallable(fn, callArgs)
			if err != nil {
				return nil, err
			}
		}
	}})

	// Generic cxr combinations
	cxrOps := func(name string) func([]Value) (Value, error) {
		// name is like "caar", "cdaddr" etc.
		// middle letters (between c and r) are 'a' or 'd' applied right-to-left
		mid := name[1 : len(name)-1]
		return func(args []Value) (Value, error) {
			if len(args) != 1 {
				return nil, &EvalError{Message: name + ": need 1 argument"}
			}
			v := args[0]
			for i := len(mid) - 1; i >= 0; i-- {
				p, ok := v.(*PairVal)
				if !ok {
					return nil, &EvalError{Message: name + ": not a pair"}
				}
				if mid[i] == 'a' {
					v = p.Car
				} else {
					v = p.Cdr
				}
			}
			return v, nil
		}
	}
	for _, name := range []string{
		"caar", "cadr", "cdar", "cddr",
		"caaar", "caadr", "cadar", "caddr", "cdaar", "cdadr", "cddar", "cdddr",
		"caaaar", "caaadr", "caadar", "caaddr", "cadaar", "cadadr", "caddar", "cadddr",
		"cdaaar", "cdaadr", "cdadar", "cdaddr", "cddaar", "cddadr", "cdddar", "cddddr",
	} {
		n := name
		env.set(n, &BuiltinFunc{Name: n, Fn: cxrOps(n)})
	}

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
		return applyCallable(fn, callArgs)
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
			return nil, &EvalError{Message: "string-set!: first argument must be a string"}
		}
		if s.Immutable {
			return nil, &EvalError{Message: "string-set!: strings are immutable"}
		}
		idx, ok := args[1].(*IntVal)
		if !ok {
			return nil, &EvalError{Message: "string-set!: second argument must be an integer"}
		}
		c, ok := args[2].(*CharVal)
		if !ok {
			return nil, &EvalError{Message: "string-set!: third argument must be a character"}
		}
		runes := []rune(s.Val)
		if idx.Val < 0 || int(idx.Val) >= len(runes) {
			return nil, &EvalError{Message: "string-set!: index out of range"}
		}
		runes[idx.Val] = c.Val
		s.Val = string(runes)
		return &VoidVal{}, nil
	}})

	env.set("string->list", &BuiltinFunc{Name: "string->list", Fn: func(args []Value) (Value, error) {
		if len(args) != 1 {
			return nil, &EvalError{Message: "string->list: need 1 argument"}
		}
		s, ok := args[0].(*StringVal)
		if !ok {
			return nil, &EvalError{Message: "string->list: not a string"}
		}
		var result Value = &NilVal{}
		runes := []rune(s.Val)
		for i := len(runes) - 1; i >= 0; i-- {
			result = &PairVal{Car: &CharVal{Val: runes[i]}, Cdr: result}
		}
		return result, nil
	}})

	env.set("list->string", &BuiltinFunc{Name: "list->string", Fn: func(args []Value) (Value, error) {
		if len(args) != 1 {
			return nil, &EvalError{Message: "list->string: need 1 argument"}
		}
		var runes []rune
		cur := args[0]
		for {
			if _, ok := cur.(*NilVal); ok {
				break
			}
			p, ok := cur.(*PairVal)
			if !ok {
				return nil, &EvalError{Message: "list->string: not a proper list"}
			}
			ch, ok := p.Car.(*CharVal)
			if !ok {
				return nil, &EvalError{Message: "list->string: element is not a character"}
			}
			runes = append(runes, ch.Val)
			cur = p.Cdr
		}
		return &StringVal{Val: string(runes)}, nil
	}})

	env.set("char->integer", &BuiltinFunc{Name: "char->integer", Fn: func(args []Value) (Value, error) {
		if len(args) != 1 {
			return nil, &EvalError{Message: "char->integer: need 1 argument"}
		}
		ch, ok := args[0].(*CharVal)
		if !ok {
			return nil, &EvalError{Message: "char->integer: not a character"}
		}
		return &IntVal{Val: int64(ch.Val)}, nil
	}})

	env.set("integer->char", &BuiltinFunc{Name: "integer->char", Fn: func(args []Value) (Value, error) {
		if len(args) != 1 {
			return nil, &EvalError{Message: "integer->char: need 1 argument"}
		}
		n, ok := args[0].(*IntVal)
		if !ok {
			return nil, &EvalError{Message: "integer->char: not a number"}
		}
		return &CharVal{Val: rune(n.Val)}, nil
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
		case *LambdaVal, *BuiltinFunc, *CaseLambdaVal, *ContinuationVal, *CallCCVal:
			return &BoolVal{Val: true}, nil
		default:
			return &BoolVal{Val: false}, nil
		}
	}})

	// First-class continuations
	env.set("call/cc", &CallCCVal{})
	env.set("call-with-current-continuation", &CallCCVal{})

	// values — return zero or more values; single value is transparent
	env.set("values", &BuiltinFunc{Name: "values", Fn: func(args []Value) (Value, error) {
		if len(args) == 1 {
			return args[0], nil
		}
		return &ValuesVal{Vals: args}, nil
	}})

	// call-with-values — (call-with-values producer consumer)
	env.set("call-with-values", &BuiltinFunc{Name: "call-with-values", Fn: func(args []Value) (Value, error) {
		if len(args) != 2 {
			return nil, &EvalError{Message: "call-with-values: need 2 arguments"}
		}
		producer := args[0]
		consumer := args[1]
		result, err := applyCallable(producer, nil)
		if err != nil {
			return nil, err
		}
		var consumerArgs []Value
		if mv, ok := result.(*ValuesVal); ok {
			consumerArgs = mv.Vals
		} else {
			consumerArgs = []Value{result}
		}
		return applyCallable(consumer, consumerArgs)
	}})

	// raise — signal an exception
	env.set("raise", &BuiltinFunc{Name: "raise", Fn: func(args []Value) (Value, error) {
		if len(args) != 1 {
			return nil, &EvalError{Message: "raise: need 1 argument"}
		}
		panic(raisePanic{value: args[0]})
	}})

	env.set("error", &BuiltinFunc{Name: "error", Fn: func(args []Value) (Value, error) {
		if len(args) < 1 {
			return nil, &EvalError{Message: "error: need at least 1 argument"}
		}
		msg := displayValue(args[0])
		for _, a := range args[1:] {
			msg += " " + displayValue(a)
		}
		panic(raisePanic{value: &StringVal{Val: msg}})
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
		// Floyd's tortoise-and-hare cycle detection
		slow := args[0]
		fast := args[0]
		for {
			// Check fast (advance 2 steps)
			fp, ok := fast.(*PairVal)
			if !ok {
				if _, ok := fast.(*NilVal); ok {
					return &BoolVal{Val: true}, nil
				}
				return &BoolVal{Val: false}, nil
			}
			fast = fp.Cdr
			fp2, ok := fast.(*PairVal)
			if !ok {
				if _, ok := fast.(*NilVal); ok {
					return &BoolVal{Val: true}, nil
				}
				return &BoolVal{Val: false}, nil
			}
			fast = fp2.Cdr
			// Advance slow 1 step
			slow = slow.(*PairVal).Cdr
			// If they meet, it's a cycle
			if slow == fast {
				return &BoolVal{Val: false}, nil
			}
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

	// assv — like assoc but uses eqv?
	env.set("assv", &BuiltinFunc{Name: "assv", Fn: func(args []Value) (Value, error) {
		if len(args) != 2 {
			return nil, &EvalError{Message: "assv: need 2 arguments"}
		}
		key := args[0]
		cur := args[1]
		for {
			if _, ok := cur.(*NilVal); ok {
				return &BoolVal{Val: false}, nil
			}
			p, ok := cur.(*PairVal)
			if !ok {
				return nil, &EvalError{Message: "assv: not a proper list"}
			}
			entry, ok := p.Car.(*PairVal)
			if !ok {
				return nil, &EvalError{Message: "assv: entry is not a pair"}
			}
			if schemeEqv(key, entry.Car) {
				return p.Car, nil
			}
			cur = p.Cdr
		}
	}})

	// member — uses equal?
	env.set("member", &BuiltinFunc{Name: "member", Fn: func(args []Value) (Value, error) {
		if len(args) != 2 {
			return nil, &EvalError{Message: "member: need 2 arguments"}
		}
		key := args[0]
		cur := args[1]
		for {
			if _, ok := cur.(*NilVal); ok {
				return &BoolVal{Val: false}, nil
			}
			p, ok := cur.(*PairVal)
			if !ok {
				return nil, &EvalError{Message: "member: not a proper list"}
			}
			if schemeEqual(key, p.Car) {
				return p, nil
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
			val, err := applyCallable(fn, callArgs)
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

	env.set("string>?", &BuiltinFunc{Name: "string>?", Fn: func(args []Value) (Value, error) {
		if len(args) != 2 {
			return nil, &EvalError{Message: "string>?: need 2 arguments"}
		}
		a, ok := args[0].(*StringVal)
		if !ok {
			return nil, &EvalError{Message: "string>?: not a string"}
		}
		b, ok := args[1].(*StringVal)
		if !ok {
			return nil, &EvalError{Message: "string>?: not a string"}
		}
		return &BoolVal{Val: a.Val > b.Val}, nil
	}})

	env.set("string<=?", &BuiltinFunc{Name: "string<=?", Fn: func(args []Value) (Value, error) {
		if len(args) != 2 {
			return nil, &EvalError{Message: "string<=?: need 2 arguments"}
		}
		a, ok := args[0].(*StringVal)
		if !ok {
			return nil, &EvalError{Message: "string<=?: not a string"}
		}
		b, ok := args[1].(*StringVal)
		if !ok {
			return nil, &EvalError{Message: "string<=?: not a string"}
		}
		return &BoolVal{Val: a.Val <= b.Val}, nil
	}})

	env.set("string>=?", &BuiltinFunc{Name: "string>=?", Fn: func(args []Value) (Value, error) {
		if len(args) != 2 {
			return nil, &EvalError{Message: "string>=?: need 2 arguments"}
		}
		a, ok := args[0].(*StringVal)
		if !ok {
			return nil, &EvalError{Message: "string>=?: not a string"}
		}
		b, ok := args[1].(*StringVal)
		if !ok {
			return nil, &EvalError{Message: "string>=?: not a string"}
		}
		return &BoolVal{Val: a.Val >= b.Val}, nil
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

	// gcd
	env.set("gcd", &BuiltinFunc{Name: "gcd", Fn: func(args []Value) (Value, error) {
		if len(args) == 0 {
			return &IntVal{Val: 0}, nil
		}
		result := int64(0)
		for _, a := range args {
			n, ok := a.(*IntVal)
			if !ok {
				return nil, &EvalError{Message: "gcd: not an integer"}
			}
			result = gcd(result, n.Val)
		}
		if result < 0 {
			result = -result
		}
		return &IntVal{Val: result}, nil
	}})

	// lcm
	env.set("lcm", &BuiltinFunc{Name: "lcm", Fn: func(args []Value) (Value, error) {
		if len(args) == 0 {
			return &IntVal{Val: 1}, nil
		}
		result := int64(1)
		for _, a := range args {
			n, ok := a.(*IntVal)
			if !ok {
				return nil, &EvalError{Message: "lcm: not an integer"}
			}
			v := n.Val
			if v < 0 {
				v = -v
			}
			if v == 0 {
				return &IntVal{Val: 0}, nil
			}
			result = result / gcd(result, v) * v
		}
		return &IntVal{Val: result}, nil
	}})

	// truncate — rounds toward zero
	env.set("truncate", &BuiltinFunc{Name: "truncate", Fn: func(args []Value) (Value, error) {
		if len(args) != 1 {
			return nil, &EvalError{Message: "truncate: need 1 argument"}
		}
		switch n := args[0].(type) {
		case *IntVal:
			return n, nil
		case *FloatVal:
			v := n.Val
			if v >= 0 {
				return &FloatVal{Val: float64(int64(v))}, nil
			}
			return &FloatVal{Val: float64(int64(v))}, nil
		default:
			return nil, &EvalError{Message: "truncate: not a number"}
		}
	}})

	// round — rounds to nearest even
	env.set("round", &BuiltinFunc{Name: "round", Fn: func(args []Value) (Value, error) {
		if len(args) != 1 {
			return nil, &EvalError{Message: "round: need 1 argument"}
		}
		switch n := args[0].(type) {
		case *IntVal:
			return n, nil
		case *FloatVal:
			v := n.Val
			rounded := math.RoundToEven(v)
			return &FloatVal{Val: rounded}, nil
		default:
			return nil, &EvalError{Message: "round: not a number"}
		}
	}})

	// memq — uses eq?
	env.set("memq", &BuiltinFunc{Name: "memq", Fn: func(args []Value) (Value, error) {
		if len(args) != 2 {
			return nil, &EvalError{Message: "memq: need 2 arguments"}
		}
		key := args[0]
		cur := args[1]
		for {
			if _, ok := cur.(*NilVal); ok {
				return &BoolVal{Val: false}, nil
			}
			p, ok := cur.(*PairVal)
			if !ok {
				return nil, &EvalError{Message: "memq: not a proper list"}
			}
			if schemeEq(key, p.Car) {
				return p, nil
			}
			cur = p.Cdr
		}
	}})

	// memv — uses eqv?
	env.set("memv", &BuiltinFunc{Name: "memv", Fn: func(args []Value) (Value, error) {
		if len(args) != 2 {
			return nil, &EvalError{Message: "memv: need 2 arguments"}
		}
		key := args[0]
		cur := args[1]
		for {
			if _, ok := cur.(*NilVal); ok {
				return &BoolVal{Val: false}, nil
			}
			p, ok := cur.(*PairVal)
			if !ok {
				return nil, &EvalError{Message: "memv: not a proper list"}
			}
			if schemeEqv(key, p.Car) {
				return p, nil
			}
			cur = p.Cdr
		}
	}})

	// assq — uses eq?
	env.set("assq", &BuiltinFunc{Name: "assq", Fn: func(args []Value) (Value, error) {
		if len(args) != 2 {
			return nil, &EvalError{Message: "assq: need 2 arguments"}
		}
		key := args[0]
		cur := args[1]
		for {
			if _, ok := cur.(*NilVal); ok {
				return &BoolVal{Val: false}, nil
			}
			p, ok := cur.(*PairVal)
			if !ok {
				return nil, &EvalError{Message: "assq: not a proper list"}
			}
			entry, ok := p.Car.(*PairVal)
			if !ok {
				return nil, &EvalError{Message: "assq: entry is not a pair"}
			}
			if schemeEq(key, entry.Car) {
				return p.Car, nil
			}
			cur = p.Cdr
		}
	}})

	// make-string
	env.set("make-string", &BuiltinFunc{Name: "make-string", Fn: func(args []Value) (Value, error) {
		if len(args) < 1 || len(args) > 2 {
			return nil, &EvalError{Message: "make-string: need 1 or 2 arguments"}
		}
		n, ok := args[0].(*IntVal)
		if !ok {
			return nil, &EvalError{Message: "make-string: first argument must be integer"}
		}
		ch := rune(' ')
		if len(args) == 2 {
			c, ok := args[1].(*CharVal)
			if !ok {
				return nil, &EvalError{Message: "make-string: second argument must be char"}
			}
			ch = c.Val
		}
		return &StringVal{Val: strings.Repeat(string(ch), int(n.Val))}, nil
	}})

	// string — create string from chars
	env.set("string", &BuiltinFunc{Name: "string", Fn: func(args []Value) (Value, error) {
		var buf strings.Builder
		for _, a := range args {
			c, ok := a.(*CharVal)
			if !ok {
				return nil, &EvalError{Message: "string: argument must be char"}
			}
			buf.WriteRune(c.Val)
		}
		return &StringVal{Val: buf.String()}, nil
	}})

	// syntax->datum: convert syntax object to datum value
	env.set("syntax->datum", &BuiltinFunc{Name: "syntax->datum", Fn: func(args []Value) (Value, error) {
		if len(args) != 1 {
			return nil, &EvalError{Message: "syntax->datum: need 1 argument"}
		}
		sv, ok := args[0].(*SyntaxVal)
		if !ok {
			return nil, &EvalError{Message: "syntax->datum: expected syntax object"}
		}
		return syntaxToDatum(sv.Expr)
	}})

	// datum->syntax: convert datum to syntax object with given lexical context
	env.set("datum->syntax", &BuiltinFunc{Name: "datum->syntax", Fn: func(args []Value) (Value, error) {
		if len(args) != 2 {
			return nil, &EvalError{Message: "datum->syntax: need 2 arguments"}
		}
		var ctxEnv *Env
		if ctxSv, ok := args[0].(*SyntaxVal); ok {
			ctxEnv = ctxSv.Env
		}
		return &SyntaxVal{Expr: datumToExpr(args[1]), Env: ctxEnv}, nil
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
	type pair struct{ a, b Value }
	visited := make(map[pair]bool)
	var eq func(a, b Value) bool
	eq = func(a, b Value) bool {
		// Short-circuit: same pointer
		if a == b {
			return true
		}
		switch av := a.(type) {
		case *PairVal:
			bv, ok := b.(*PairVal)
			if !ok {
				return false
			}
			k := pair{a, b}
			if visited[k] {
				return true // assume equal for cycles
			}
			visited[k] = true
			return eq(av.Car, bv.Car) && eq(av.Cdr, bv.Cdr)
		case *StringVal:
			if bv, ok := b.(*StringVal); ok {
				return av.Val == bv.Val
			}
			return false
		case *VectorVal:
			bv, ok := b.(*VectorVal)
			if !ok {
				return false
			}
			if len(av.Elems) != len(bv.Elems) {
				return false
			}
			for i := range av.Elems {
				if !eq(av.Elems[i], bv.Elems[i]) {
					return false
				}
			}
			return true
		case *NilVal:
			_, ok := b.(*NilVal)
			return ok
		default:
			return schemeEq(a, b)
		}
	}
	return eq(a, b)
}

// doWindTransition unwinds from the current wind stack and rewinds to the
// target wind stack, calling out-thunks and in-thunks in the correct order.
func doWindTransition(target []windEntry) error {
	// Find the common prefix length
	current := currentWindStack
	commonLen := 0
	for commonLen < len(current) && commonLen < len(target) {
		if current[commonLen].In == target[commonLen].In && current[commonLen].Out == target[commonLen].Out {
			commonLen++
		} else {
			break
		}
	}
	// Unwind: call out-thunks from innermost to the common prefix
	for i := len(current) - 1; i >= commonLen; i-- {
		if _, err := applyCallable(current[i].Out, nil); err != nil {
			return err
		}
	}
	// Rewind: call in-thunks from the common prefix to innermost
	for i := commonLen; i < len(target); i++ {
		if _, err := applyCallable(target[i].In, nil); err != nil {
			return err
		}
	}
	// Update current wind stack to target
	currentWindStack = make([]windEntry, len(target))
	copy(currentWindStack, target)
	return nil
}

// evalDynamicWind implements (dynamic-wind in-thunk body-thunk out-thunk).
func evalDynamicWind(e *ListExpr, env *Env) (Value, error) {
	if len(e.Elems) != 4 {
		return nil, &EvalError{Message: "dynamic-wind: need 3 arguments"}
	}
	inThunk, err := evalExpr(e.Elems[1], env)
	if err != nil {
		return nil, err
	}
	bodyThunk, err := evalExpr(e.Elems[2], env)
	if err != nil {
		return nil, err
	}
	outThunk, err := evalExpr(e.Elems[3], env)
	if err != nil {
		return nil, err
	}

	// Call in-thunk
	if _, err := applyCallable(inThunk, nil); err != nil {
		return nil, err
	}

	// Push wind entry
	entry := windEntry{In: inThunk, Out: outThunk}
	currentWindStack = append(currentWindStack, entry)

	// Call body-thunk, catching continuation escapes to run out-thunk
	var result Value
	var bodyErr error
	escaped := false
	var escapePanic interface{}

	// Save depth/stack state — panic unwinding corrupts them
	savedDepthDW := nonBodyEvalDepth
	savedStackLenDW := len(contFrameStack)

	func() {
		defer func() {
			if r := recover(); r != nil {
				if _, ok := r.(contInvokePanic); ok {
					escaped = true
					escapePanic = r
					nonBodyEvalDepth = savedDepthDW
					contFrameStack = contFrameStack[:savedStackLenDW]
					return
				}
				if _, ok := r.(raisePanic); ok {
					escaped = true
					escapePanic = r
					nonBodyEvalDepth = savedDepthDW
					contFrameStack = contFrameStack[:savedStackLenDW]
					return
				}
				panic(r)
			}
		}()
		result, bodyErr = applyCallable(bodyThunk, nil)
	}()

	// Pop wind entry
	currentWindStack = currentWindStack[:len(currentWindStack)-1]

	// Call out-thunk
	if _, err := applyCallable(outThunk, nil); err != nil {
		return nil, err
	}

	if escaped {
		// Re-panic to propagate the continuation escape
		panic(escapePanic)
	}

	return result, bodyErr
}

// evalGuard implements (guard (var clause ...) body ...).
func evalGuard(e *ListExpr, env *Env) (Value, error) {
	if len(e.Elems) < 3 {
		return nil, &EvalError{Message: "guard: need clauses and body"}
	}
	clauseList, ok := e.Elems[1].(*ListExpr)
	if !ok || len(clauseList.Elems) < 2 {
		return nil, &EvalError{Message: "guard: need variable and at least one clause"}
	}
	varSym, ok := clauseList.Elems[0].(*SymbolExpr)
	if !ok {
		return nil, &EvalError{Message: "guard: expected variable name"}
	}
	clauses := clauseList.Elems[1:]
	body := e.Elems[2:]

	var result Value
	var bodyErr error
	var raised *raisePanic

	savedDepthG := nonBodyEvalDepth
	savedStackLenG := len(contFrameStack)

	func() {
		defer func() {
			if r := recover(); r != nil {
				if rp, ok := r.(raisePanic); ok {
					raised = &rp
					nonBodyEvalDepth = savedDepthG
					contFrameStack = contFrameStack[:savedStackLenG]
					return
				}
				panic(r)
			}
		}()
		for i, bodyExpr := range body[:len(body)-1] {
			pushContFrame(contFrame{remainExprs: body[i+1:], env: env})
			nonBodyEvalDepth--
			_, err := evalExpr(bodyExpr, env)
			nonBodyEvalDepth++
			popContFrame()
			if err != nil {
				bodyErr = err
				return
			}
		}
		nonBodyEvalDepth--
		result, bodyErr = evalExpr(body[len(body)-1], env)
		nonBodyEvalDepth++
	}()

	if bodyErr != nil {
		return nil, bodyErr
	}
	if raised == nil {
		return result, nil
	}

	// Exception caught — evaluate cond-like clauses
	guardEnv := newEnv(env)
	guardEnv.set(varSym.Name, raised.value)

	for _, clause := range clauses {
		cl, ok := clause.(*ListExpr)
		if !ok || len(cl.Elems) < 1 {
			return nil, &EvalError{Message: "guard: bad clause"}
		}
		if sym, ok := cl.Elems[0].(*SymbolExpr); ok && sym.Name == "else" {
			var res Value
			for _, expr := range cl.Elems[1:] {
				var err error
				res, err = evalExpr(expr, guardEnv)
				if err != nil {
					return nil, err
				}
			}
			return res, nil
		}
		test, err := evalExpr(cl.Elems[0], guardEnv)
		if err != nil {
			return nil, err
		}
		if isTruthy(test) {
			if len(cl.Elems) == 1 {
				return test, nil
			}
			var res Value
			for _, expr := range cl.Elems[1:] {
				res, err = evalExpr(expr, guardEnv)
				if err != nil {
					return nil, err
				}
			}
			return res, nil
		}
	}
	// No clause matched — re-raise
	panic(raisePanic{value: raised.value})
}

// evalWithExceptionHandler implements (with-exception-handler handler thunk).
func evalWithExceptionHandler(e *ListExpr, env *Env) (Value, error) {
	if len(e.Elems) != 3 {
		return nil, &EvalError{Message: "with-exception-handler: need 2 arguments"}
	}
	handler, err := evalExpr(e.Elems[1], env)
	if err != nil {
		return nil, err
	}
	thunk, err := evalExpr(e.Elems[2], env)
	if err != nil {
		return nil, err
	}

	var result Value
	var thunkErr error
	var raised *raisePanic

	savedDepthWEH := nonBodyEvalDepth
	savedStackLenWEH := len(contFrameStack)

	func() {
		defer func() {
			if r := recover(); r != nil {
				if rp, ok := r.(raisePanic); ok {
					raised = &rp
					nonBodyEvalDepth = savedDepthWEH
					contFrameStack = contFrameStack[:savedStackLenWEH]
					return
				}
				panic(r)
			}
		}()
		result, thunkErr = applyCallable(thunk, nil)
	}()

	if thunkErr != nil {
		return nil, thunkErr
	}
	if raised != nil {
		return applyCallable(handler, []Value{raised.value})
	}
	return result, nil
}

// EvalStr evaluates one or more Scheme expressions and returns the string
// representation of the last result.
// handleCallCC implements call/cc. It captures the current continuation,
// calls f with it, and handles both escape and reentrant invocations.
func handleCallCC(f Value) (Value, error) {
	state := currentEvalState

	// Check for override (re-entry case: a saved continuation was invoked)
	if state != nil && state.callccOverride != nil {
		val := *state.callccOverride
		state.callccOverride = nil
		return val, nil
	}

	// Capture continuation: save the top-level expressions from the current
	// expression onward and the environment, plus any body context and frames
	var cont *ContinuationVal
	windsCopy := make([]windEntry, len(currentWindStack))
	copy(windsCopy, currentWindStack)
	// Capture continuation frames (body-sequence chain)
	framesCopy := make([]contFrame, len(contFrameStack))
	copy(framesCopy, contFrameStack)
	// nonBodyEvalDepth == 0 means call/cc is at body level
	isBodyLevel := nonBodyEvalDepth == 0
	if state != nil {
		topCopy := make([]Expr, len(state.topExprs)-state.curIdx)
		copy(topCopy, state.topExprs[state.curIdx:])
		cont = &ContinuationVal{
			topExprs:  topCopy,
			topEnv:    state.topEnv,
			bodyExprs: state.bodyExprs,
			bodyEnv:   state.bodyEnv,
			winds:     windsCopy,
			frames:    framesCopy,
			bodyLevel: isBodyLevel,
		}
	} else {
		cont = &ContinuationVal{
			winds:     windsCopy,
			frames:    framesCopy,
			bodyLevel: isBodyLevel,
		}
	}

	// Call f(cont) with panic/recover for escape continuations
	var result Value
	var err error
	escaped := false

	// Save depth/stack state before calling f — escape panics corrupt them
	savedDepth := nonBodyEvalDepth
	savedStackLen := len(contFrameStack)

	func() {
		defer func() {
			if r := recover(); r != nil {
				if ci, ok := r.(contInvokePanic); ok && ci.cont == cont {
					// Escape: continuation invoked during f's execution
					result = ci.value
					escaped = true
					// Restore depth/stack corrupted by panic unwinding
					nonBodyEvalDepth = savedDepth
					contFrameStack = contFrameStack[:savedStackLen]
					return
				}
				panic(r) // re-panic for other continuations or real panics
			}
		}()
		result, err = applyCallable(f, []Value{cont})
	}()

	if escaped {
		return result, nil
	}
	return result, err
}

// applyCallable calls any callable value with the given arguments.
func applyCallable(f Value, args []Value) (Value, error) {
	switch fn := f.(type) {
	case *LambdaVal:
		return applyLambda(fn, args)
	case *BuiltinFunc:
		return fn.Fn(args)
	case *CaseLambdaVal:
		return applyCaseLambda(fn, args)
	case *ContinuationVal:
		if len(args) == 1 {
			panic(contInvokePanic{cont: fn, value: args[0]})
		}
		panic(contInvokePanic{cont: fn, value: &ValuesVal{Vals: args}})
	case *CallCCVal:
		if len(args) != 1 {
			return nil, &EvalError{Message: "call/cc: need 1 argument"}
		}
		return handleCallCC(args[0])
	default:
		return nil, &EvalError{Message: "not a procedure"}
	}
}

// expandSyntaxCaseMacro expands a syntax-case macro application.
func expandSyntaxCaseMacro(transformer *MacroTransformerVal, form *ListExpr, useEnv *Env) (Expr, error) {
	stx := &SyntaxVal{Expr: form, Env: useEnv}

	// Call the transformer lambda with stx as argument
	callEnv := newEnv(transformer.Proc.Env)
	if len(transformer.Proc.Params) > 0 {
		callEnv.set(transformer.Proc.Params[0], stx)
	}

	var result Value
	var rerr error
	for i, bodyExpr := range transformer.Proc.Body {
		result, rerr = evalExpr(bodyExpr, callEnv)
		if rerr != nil {
			return nil, rerr
		}
		_ = i
	}

	// Result should be a SyntaxVal; unwrap to Expr
	if sv, ok := result.(*SyntaxVal); ok {
		return sv.Expr, nil
	}
	line, col := form.pos()
	return nil, &EvalError{Message: fmt.Sprintf("%d:%d: macro transformer must return syntax object", line, col)}
}

// evalSyntaxCase evaluates a syntax-case form.
func evalSyntaxCase(e *ListExpr, env *Env) (Value, error) {
	// (syntax-case stx-expr (literal ...) clause ...)
	if len(e.Elems) < 4 {
		return nil, &EvalError{Message: fmt.Sprintf("%d:%d: syntax-case: bad form", e.Line, e.Col)}
	}

	// Evaluate the scrutinee
	stxVal, err := evalExpr(e.Elems[1], env)
	if err != nil {
		return nil, err
	}

	var stxExpr Expr
	if sv, ok := stxVal.(*SyntaxVal); ok {
		stxExpr = sv.Expr
	} else {
		return nil, &EvalError{Message: fmt.Sprintf("%d:%d: syntax-case: expected syntax object", e.Line, e.Col)}
	}

	// Parse literals
	litList, ok := e.Elems[2].(*ListExpr)
	if !ok {
		return nil, &EvalError{Message: fmt.Sprintf("%d:%d: syntax-case: expected literal list", e.Line, e.Col)}
	}
	var literals []string
	for _, l := range litList.Elems {
		ls, ok := l.(*SymbolExpr)
		if !ok {
			return nil, &EvalError{Message: fmt.Sprintf("%d:%d: syntax-case: literal must be symbol", e.Line, e.Col)}
		}
		literals = append(literals, ls.Name)
	}

	// Try each clause
	for _, clause := range e.Elems[3:] {
		cl, ok := clause.(*ListExpr)
		if !ok || len(cl.Elems) < 2 {
			return nil, &EvalError{Message: fmt.Sprintf("%d:%d: syntax-case: bad clause", e.Line, e.Col)}
		}

		pattern := cl.Elems[0]
		var fender Expr
		var bodyExpr Expr
		if len(cl.Elems) == 2 {
			bodyExpr = cl.Elems[1]
		} else {
			fender = cl.Elems[1]
			bodyExpr = cl.Elems[2]
		}

		// Match pattern against stxExpr
		bindings := make(map[string]interface{})
		if matchSinglePattern(pattern, stxExpr, literals, bindings) {
			// Create new env with pattern variable bindings as SyntaxVal
			clauseEnv := newEnv(env)
			for name, val := range bindings {
				switch v := val.(type) {
				case Expr:
					clauseEnv.set(name, &SyntaxVal{Expr: v})
				case []Expr:
					elems := make([]*SyntaxVal, len(v))
					for i, ex := range v {
						elems[i] = &SyntaxVal{Expr: ex}
					}
					clauseEnv.set(name, &SyntaxListVal{Elems: elems})
				}
			}

			// Evaluate fender if present
			if fender != nil {
				fVal, ferr := evalExpr(fender, clauseEnv)
				if ferr != nil {
					return nil, ferr
				}
				if !isTruthy(fVal) {
					continue
				}
			}

			// Evaluate body
			return evalExpr(bodyExpr, clauseEnv)
		}
	}
	return nil, &EvalError{Message: fmt.Sprintf("%d:%d: syntax-case: no matching pattern", e.Line, e.Col)}
}

// evalWithSyntax evaluates a with-syntax form.
func evalWithSyntax(e *ListExpr, env *Env) (Value, error) {
	// (with-syntax ((pattern expr) ...) body ...)
	if len(e.Elems) < 3 {
		return nil, &EvalError{Message: fmt.Sprintf("%d:%d: with-syntax: bad form", e.Line, e.Col)}
	}
	bindingList, ok := e.Elems[1].(*ListExpr)
	if !ok {
		return nil, &EvalError{Message: fmt.Sprintf("%d:%d: with-syntax: expected binding list", e.Line, e.Col)}
	}
	wsEnv := newEnv(env)
	for _, binding := range bindingList.Elems {
		bl, ok := binding.(*ListExpr)
		if !ok || len(bl.Elems) != 2 {
			return nil, &EvalError{Message: fmt.Sprintf("%d:%d: with-syntax: bad binding", e.Line, e.Col)}
		}
		val, verr := evalExpr(bl.Elems[1], env)
		if verr != nil {
			return nil, verr
		}
		if sym, ok := bl.Elems[0].(*SymbolExpr); ok {
			wsEnv.set(sym.Name, val)
		} else {
			return nil, &EvalError{Message: fmt.Sprintf("%d:%d: with-syntax: expected symbol in binding", e.Line, e.Col)}
		}
	}
	// Evaluate body
	body := e.Elems[2:]
	var result Value
	var rerr error
	for _, bodyExpr := range body {
		result, rerr = evalExpr(bodyExpr, wsEnv)
		if rerr != nil {
			return nil, rerr
		}
	}
	return result, nil
}

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
	// Reset global state for each top-level evaluation
	currentWindStack = nil
	contFrameStack = contFrameStack[:0]
	nonBodyEvalDepth = 0
	var buf strings.Builder
	env := makeGlobalEnv(&buf)

	// Catch unhandled raise/error panics
	defer func() {
		if r := recover(); r != nil {
			if rp, ok := r.(raisePanic); ok {
				err = &EvalError{Message: fmt.Sprintf("unhandled exception: %s", displayValue(rp.value))}
				return
			}
			panic(r)
		}
	}()

	last, evalErr := evalTopLevel(exprs, env)
	if evalErr != nil {
		return "", "", evalErr
	}
	if _, ok := last.(*VoidVal); ok {
		return "", buf.String(), nil
	}
	return last.String(), buf.String(), nil
}

// replayContinuationFrames evaluates the remaining computation captured in
// continuation frames. Frames are stored outermost-first; we evaluate
// innermost-first (reverse order).
func replayContinuationFrames(value Value, frames []contFrame) (Value, error) {
	savedDepth := nonBodyEvalDepth
	nonBodyEvalDepth = 0
	savedStack := contFrameStack

	for fi := len(frames) - 1; fi >= 0; fi-- {
		frame := frames[fi]
		for ei, expr := range frame.remainExprs {
			// Set up contFrameStack: remaining in this frame + outer frames
			contFrameStack = contFrameStack[:0]
			if ei+1 < len(frame.remainExprs) {
				contFrameStack = append(contFrameStack, contFrame{
					remainExprs: frame.remainExprs[ei+1:],
					env:         frame.env,
				})
			}
			if fi > 0 {
				contFrameStack = append(contFrameStack, frames[:fi]...)
			}

			nonBodyEvalDepth--
			var err error
			value, err = evalExpr(expr, frame.env)
			nonBodyEvalDepth++
			if err != nil {
				contFrameStack = savedStack
				nonBodyEvalDepth = savedDepth
				return nil, err
			}
		}
	}

	contFrameStack = savedStack
	nonBodyEvalDepth = savedDepth
	return value, nil
}

// evalTopLevel evaluates a sequence of top-level expressions, handling
// continuation restarts when a saved continuation is invoked.
func evalTopLevel(exprs []Expr, env *Env) (Value, error) {
	state := &evalState{
		topExprs: exprs,
		topEnv:   env,
	}
	prevState := currentEvalState
	currentEvalState = state
	defer func() { currentEvalState = prevState }()

	// Track whether to use frame-based replay
	var replayFrames []contFrame
	var replayValue Value
	useFrameReplay := false

	for {
		var last Value
		var evalErr error
		var restart *contInvokePanic

		func() {
			defer func() {
				if r := recover(); r != nil {
					if ci, ok := r.(contInvokePanic); ok {
						restart = &ci
						return
					}
					panic(r)
				}
			}()

			if useFrameReplay {
				last, evalErr = replayContinuationFrames(replayValue, replayFrames)
				useFrameReplay = false
			} else {
				for i := 0; i < len(state.topExprs); i++ {
					state.curIdx = i
					if i < len(state.topExprs)-1 {
						pushContFrame(contFrame{
							remainExprs: state.topExprs[i+1:],
							env:         state.topEnv,
						})
					}
					nonBodyEvalDepth--
					last, evalErr = evalExpr(state.topExprs[i], state.topEnv)
					nonBodyEvalDepth++
					if i < len(state.topExprs)-1 {
						popContFrame()
					}
					if evalErr != nil {
						return
					}
				}
			}
		}()

		if evalErr != nil {
			return nil, evalErr
		}
		if restart != nil {
			// Reset wind stack, cont frame stack, and depth counter
			currentWindStack = nil
			contFrameStack = contFrameStack[:0]
			nonBodyEvalDepth = 0

			// Check if the continuation has body-level frames for precise replay.
			// Don't use frame replay if dynamic-wind is active (wind thunks need re-evaluation).
			if restart.cont.bodyLevel && len(restart.cont.frames) > 0 && len(restart.cont.winds) == 0 {
				replayFrames = restart.cont.frames
				replayValue = restart.value
				useFrameReplay = true
				continue
			}

			// Fall back to existing topExprs/bodyExprs replay mechanism
			val := restart.value
			state.callccOverride = &val
			if restart.cont.bodyExprs != nil {
				state.topExprs = restart.cont.bodyExprs
				state.topEnv = restart.cont.bodyEnv
			} else {
				state.topExprs = restart.cont.topExprs
				state.topEnv = restart.cont.topEnv
			}
			state.bodyExprs = nil
			state.bodyEnv = nil
			state.curIdx = 0
			continue
		}

		return last, nil
	}
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
