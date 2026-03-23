package ming

import (
	"fmt"
	"math"
	"strconv"
	"strings"
	"unicode"
)

// TopEnv creates a new top-level environment with builtins.
func TopEnv() *Env {
	env := NewEnv(nil)
	for name, proc := range builtins {
		env.Set(name, proc)
	}
	env.Set("display", &EnvBuiltinProc{Name: "display", Fn: builtinDisplay})
	env.Set("write", &EnvBuiltinProc{Name: "write", Fn: builtinWrite})
	env.Set("newline", &EnvBuiltinProc{Name: "newline", Fn: builtinNewline})
	env.Set("apply", &EnvBuiltinProc{Name: "apply", Fn: builtinApplyEnv})
	env.Set("map", &EnvBuiltinProc{Name: "map", Fn: builtinMapEnv})
	env.Set("for-each", &EnvBuiltinProc{Name: "for-each", Fn: builtinForEachEnv})
	callcc := &SchemeCallCC{}
	env.Set("call/cc", callcc)
	env.Set("call-with-current-continuation", callcc)
	env.Set("dynamic-wind", &SchemeDynamicWind{})
	env.Set("raise", &BuiltinProc{Name: "raise", Fn: func(args []SchemeValue, callExpr *ListExpr) (SchemeValue, error) {
		if len(args) != 1 {
			line, col := callExpr.Pos()
			return nil, &EvalError{Message: fmt.Sprintf("%d:%d: raise: requires exactly 1 argument", line, col)}
		}
		return nil, &schemeRaiseError{value: args[0]}
	}})
	env.Set("with-exception-handler", &EnvBuiltinProc{Name: "with-exception-handler", Fn: builtinWithExceptionHandler})
	env.Set("values", &BuiltinProc{Name: "values", Fn: builtinValues})
	env.Set("call-with-values", &EnvBuiltinProc{Name: "call-with-values", Fn: builtinCallWithValues})
	return env
}

// EvalStr evaluates one or more Scheme expressions and returns the string
// representation of the last result.
func EvalStr(input string) (string, error) {
	tokens, err := Tokenize(input)
	if err != nil {
		return "", &EvalError{Message: err.Error()}
	}

	parser := NewParser(tokens)
	exprs, err := parser.ParseAll()
	if err != nil {
		return "", &EvalError{Message: err.Error()}
	}

	if len(exprs) == 0 {
		return "", &EvalError{Message: "no expressions"}
	}

	env := TopEnv()
	result, err := evalAllExprs(exprs, env)
	if err != nil {
		return "", err
	}

	return result.String(), nil
}

// EvalStrWithOutput evaluates Scheme expressions and returns both the result
// string and any captured output from display/write/newline.
func EvalStrWithOutput(input string) (result string, output string, err error) {
	tokens, err := Tokenize(input)
	if err != nil {
		return "", "", &EvalError{Message: err.Error()}
	}

	parser := NewParser(tokens)
	exprs, err := parser.ParseAll()
	if err != nil {
		return "", "", &EvalError{Message: err.Error()}
	}

	if len(exprs) == 0 {
		return "", "", &EvalError{Message: "no expressions"}
	}

	env := TopEnv()
	var buf strings.Builder
	env.Set("$$output$$", &outputPort{buf: &buf})

	res, err := evalAllExprs(exprs, env)
	if err != nil {
		return "", "", err
	}

	return res.String(), buf.String(), nil
}

// outputPort is an internal value that holds the output buffer.
type outputPort struct {
	buf *strings.Builder
}

func (o *outputPort) String() string { return "#<output-port>" }

// SchemeContinuation represents a captured continuation.
type SchemeContinuation struct {
	id         int64
	callccExpr Expr
	topExprIdx int
	letInfo    *letSkipInfo
}

func (v *SchemeContinuation) String() string { return "#<continuation>" }

type letSkipInfo struct {
	letExpr *ListExpr
	env     *Env
}

// contJumpError signals a continuation invocation.
type contJumpError struct {
	cont  *SchemeContinuation
	value SchemeValue
}

func (e *contJumpError) Error() string { return "continuation jump" }

// schemeRaiseError signals a Scheme raise (exception).
type schemeRaiseError struct {
	value SchemeValue
}

func (e *schemeRaiseError) Error() string { return "raise: " + e.value.String() }

// Internal context type stored in environment for let-skip.
type letCtxVal struct {
	letExpr *ListExpr
	env     *Env
}

func (v *letCtxVal) String() string { return "" }

// contResumeInfo holds resumption state for a continuation.
type contResumeInfo struct {
	callccExpr Expr
	value      SchemeValue
}

// Global state for continuation resumption.
var activeContResume *contResumeInfo
var activeLetSkip *letSkipInfo
var contIDCounter int64

// activeCallCCIDs tracks call/cc invocations currently on the call stack.
var activeCallCCIDs = make(map[int64]bool)

// currentTopExprIdx tracks which top-level expression is being evaluated.
var currentTopExprIdx int

// evalAllExprs evaluates a sequence of expressions with continuation jump support.
func evalAllExprs(exprs []Expr, env *Env) (SchemeValue, error) {
	activeContResume = nil
	activeLetSkip = nil

	var result SchemeValue
	for i := 0; i < len(exprs); i++ {
		currentTopExprIdx = i
		var err error
		result, err = Eval(exprs[i], env)
		if err != nil {
			if jump, ok := err.(*contJumpError); ok {
				activeContResume = &contResumeInfo{
					callccExpr: jump.cont.callccExpr,
					value:      jump.value,
				}
				if jump.cont.letInfo != nil {
					activeLetSkip = jump.cont.letInfo
				}
				i = jump.cont.topExprIdx - 1 // -1 because loop increments
				continue
			}
			return nil, err
		}
	}
	return result, nil
}

// Eval evaluates an expression in the given environment.
// Uses a trampoline loop for tail call optimization.
func Eval(expr Expr, env *Env) (SchemeValue, error) {
	for {
		switch e := expr.(type) {
		case *NumberExpr:
			return &SchemeInt{Value: e.Value}, nil

		case *FloatExpr:
			return &SchemeFloat{Value: e.Value}, nil

		case *RationalExpr:
			if e.Den == 0 {
				return nil, &EvalError{Message: "division by zero in rational literal"}
			}
			return makeRational(e.Num, e.Den), nil

		case *BoolExpr:
			return &SchemeBool{Value: e.Value}, nil

		case *StringExpr:
			return &SchemeString{Value: e.Value}, nil

		case *CharExpr:
			return &SchemeChar{Value: e.Value}, nil

		case *SymbolExpr:
			if v, ok := env.Get(e.Name); ok {
				return v, nil
			}
			line, col := e.Pos()
			return nil, &EvalError{Message: fmt.Sprintf("%d:%d: unbound variable '%s'", line, col, e.Name)}

		case *ListExpr:
			if len(e.Elements) == 0 {
				line, col := e.Pos()
				return nil, &EvalError{Message: fmt.Sprintf("%d:%d: empty application", line, col)}
			}

			// Check for special forms
			if sym, ok := e.Elements[0].(*SymbolExpr); ok {
				switch sym.Name {
				case "and":
					// TCO: last expr in and is tail
					args := e.Elements[1:]
					if len(args) == 0 {
						return &SchemeBool{Value: true}, nil
					}
					for _, a := range args[:len(args)-1] {
						result, err := Eval(a, env)
						if err != nil {
							return nil, err
						}
						if !isTruthy(result) {
							return result, nil
						}
					}
					expr = args[len(args)-1]
					continue

				case "or":
					// TCO: last expr in or is tail
					args := e.Elements[1:]
					if len(args) == 0 {
						return &SchemeBool{Value: false}, nil
					}
					for _, a := range args[:len(args)-1] {
						result, err := Eval(a, env)
						if err != nil {
							return nil, err
						}
						if isTruthy(result) {
							return result, nil
						}
					}
					expr = args[len(args)-1]
					continue

				case "set!":
					if len(e.Elements) != 3 {
						line, col := e.Pos()
						return nil, &EvalError{Message: fmt.Sprintf("%d:%d: set!: bad syntax", line, col)}
					}
					sym, ok := e.Elements[1].(*SymbolExpr)
					if !ok {
						line, col := e.Elements[1].Pos()
						return nil, &EvalError{Message: fmt.Sprintf("%d:%d: set!: not a variable", line, col)}
					}
					val, err := Eval(e.Elements[2], env)
					if err != nil {
						return nil, err
					}
					if !env.SetExisting(sym.Name, val) {
						line, col := sym.Pos()
						return nil, &EvalError{Message: fmt.Sprintf("%d:%d: set!: variable %s is not bound", line, col, sym.Name)}
					}
					return &SchemeVoid{}, nil

				case "define":
					return evalDefine(e, env)

				case "if":
					// TCO: branch is tail
					if len(e.Elements) < 3 || len(e.Elements) > 4 {
						line, col := e.Pos()
						return nil, &EvalError{Message: fmt.Sprintf("%d:%d: if: bad syntax", line, col)}
					}
					cond, err := Eval(e.Elements[1], env)
					if err != nil {
						return nil, err
					}
					if isTruthy(cond) {
						expr = e.Elements[2]
						continue
					}
					if len(e.Elements) == 4 {
						expr = e.Elements[3]
						continue
					}
					return &SchemeVoid{}, nil

				case "quote":
					if len(e.Elements) != 2 {
						line, col := e.Pos()
						return nil, &EvalError{Message: fmt.Sprintf("%d:%d: quote: requires exactly 1 argument", line, col)}
					}
					return quoteExpr(e.Elements[1]), nil

				case "quasiquote":
					if len(e.Elements) != 2 {
						line, col := e.Pos()
						return nil, &EvalError{Message: fmt.Sprintf("%d:%d: quasiquote: requires exactly 1 argument", line, col)}
					}
					return evalQuasiquote(e.Elements[1], env)

				case "lambda":
					return evalLambda(e, env)

				case "case-lambda":
					return evalCaseLambda(e, env)

				case "let":
					// TCO: inline let so body tail is tail
					newExpr, newEnv, err := setupLet(e, env)
					if err != nil {
						return nil, err
					}
					expr = newExpr
					env = newEnv
					continue

				case "begin":
					// TCO: last expr in begin is tail
					body := e.Elements[1:]
					if len(body) == 0 {
						return &SchemeVoid{}, nil
					}
					for _, b := range body[:len(body)-1] {
						_, err := Eval(b, env)
						if err != nil {
							if jump, ok := err.(*contJumpError); ok && !activeCallCCIDs[jump.cont.id] {
								continue
							}
							return nil, err
						}
					}
					expr = body[len(body)-1]
					continue

				case "cond":
					// TCO: selected branch body tail is tail
					newExpr, newEnv, done, result, err := setupCond(e, env)
					if err != nil {
						return nil, err
					}
					if done {
						return result, nil
					}
					expr = newExpr
					env = newEnv
					continue

				case "define-syntax":
					return evalDefineSyntax(e, env)

				case "syntax-case":
					return evalSyntaxCase(e, env)

				case "syntax":
					return evalSyntaxTemplate(e, env)

				case "with-syntax":
					return evalWithSyntax(e, env)

				case "letrec":
					newExpr, newEnv, err := setupLetrec(e, env, false)
					if err != nil {
						return nil, err
					}
					expr = newExpr
					env = newEnv
					continue

				case "letrec*":
					newExpr, newEnv, err := setupLetrec(e, env, true)
					if err != nil {
						return nil, err
					}
					expr = newExpr
					env = newEnv
					continue

				case "case":
					newExpr, done, result, err := evalCase(e, env)
					if err != nil {
						return nil, err
					}
					if done {
						return result, nil
					}
					expr = newExpr
					continue

				case "do":
					newExpr, newEnv, err := evalDo(e, env)
					if err != nil {
						return nil, err
					}
					if newExpr == nil {
						return &SchemeVoid{}, nil
					}
					expr = newExpr
					env = newEnv
					continue

				case "let*":
					newExpr, newEnv, err := setupLetStar(e, env)
					if err != nil {
						return nil, err
					}
					expr = newExpr
					env = newEnv
					continue

				case "raise":
					// Only treat as special form if not locally shadowed
					if v, found := env.Get("raise"); found {
						if _, isBuiltin := v.(*BuiltinProc); !isBuiltin {
							break // fall through to normal application
						}
					}
					if len(e.Elements) != 2 {
						line, col := e.Pos()
						return nil, &EvalError{Message: fmt.Sprintf("%d:%d: raise: requires exactly 1 argument", line, col)}
					}
					val, err := Eval(e.Elements[1], env)
					if err != nil {
						return nil, err
					}
					return nil, &schemeRaiseError{value: val}

				case "with-exception-handler":
					if len(e.Elements) != 3 {
						line, col := e.Pos()
						return nil, &EvalError{Message: fmt.Sprintf("%d:%d: with-exception-handler: requires exactly 2 arguments", line, col)}
					}
					handlerVal, err := Eval(e.Elements[1], env)
					if err != nil {
						return nil, err
					}
					thunkVal, err := Eval(e.Elements[2], env)
					if err != nil {
						return nil, err
					}
					result, err := applyFunc(thunkVal, []SchemeValue{}, e, env)
					if err != nil {
						if raiseErr, ok := err.(*schemeRaiseError); ok {
							return applyFunc(handlerVal, []SchemeValue{raiseErr.value}, e, env)
						}
						return nil, err
					}
					return result, nil

				case "guard":
					return evalGuard(e, env)
				case "define-record-type":
					return evalDefineRecordType(e, env)
				}
			}

			// Check for macro application
			if sym, ok := e.Elements[0].(*SymbolExpr); ok {
				if v, ok := env.Get(sym.Name); ok {
					if macro, ok := v.(*SchemeMacro); ok {
						expanded, enrichedEnv, err := expandMacro(macro, e, env)
						if err != nil {
							return nil, err
						}
						expr = expanded
						env = enrichedEnv
						continue
					}
					if transformer, ok := v.(*SchemeSyntaxTransformer); ok {
						expanded, enrichedEnv, err := expandSyntaxTransformer(transformer, e, env)
						if err != nil {
							return nil, err
						}
						expr = expanded
						env = enrichedEnv
						continue
					}
				}
			}

			// Evaluate operator
			op, err := Eval(e.Elements[0], env)
			if err != nil {
				return nil, err
			}

			// Evaluate arguments
			args := make([]SchemeValue, len(e.Elements)-1)
			for i, arg := range e.Elements[1:] {
				args[i], err = Eval(arg, env)
				if err != nil {
					return nil, err
				}
			}

			// Apply
			switch fn := op.(type) {
			case *BuiltinProc:
				return fn.Fn(args, e)
			case *EnvBuiltinProc:
				return fn.Fn(args, e, env)
			case *Lambda:
				// TCO: lambda application is tail call
				localEnv, err := bindLambdaArgs(fn, args, e)
				if err != nil {
					return nil, err
				}
				// Evaluate all but last body expr, then tail-call the last
				for _, bodyExpr := range fn.Body[:len(fn.Body)-1] {
					_, err := Eval(bodyExpr, localEnv)
					if err != nil {
						// Catch stale continuation jumps at lambda boundary
						if jump, ok := err.(*contJumpError); ok && !activeCallCCIDs[jump.cont.id] {
							return jump.value, nil
						}
						return nil, err
					}
				}
				expr = fn.Body[len(fn.Body)-1]
				env = localEnv
				continue
			case *SchemeCaseLambda:
				clause, err := matchCaseLambda(fn, len(args), e)
				if err != nil {
					return nil, err
				}
				localEnv, err := bindLambdaArgs(clause, args, e)
				if err != nil {
					return nil, err
				}
				for _, bodyExpr := range clause.Body[:len(clause.Body)-1] {
					_, err := Eval(bodyExpr, localEnv)
					if err != nil {
						if jump, ok := err.(*contJumpError); ok && !activeCallCCIDs[jump.cont.id] {
							return jump.value, nil
						}
						return nil, err
					}
				}
				expr = clause.Body[len(clause.Body)-1]
				env = localEnv
				continue
			case *SchemeCallCC:
				return evalCallCC(args, e, env)
			case *SchemeContinuation:
				if len(args) == 0 {
					return nil, &contJumpError{cont: fn, value: &SchemeVoid{}}
				} else if len(args) == 1 {
					return nil, &contJumpError{cont: fn, value: args[0]}
				} else {
					return nil, &contJumpError{cont: fn, value: &SchemeMultipleValues{Values: args}}
				}
			case *SchemeDynamicWind:
				return applyFunc(fn, args, e, env)
			}

			line, col := e.Pos()
			return nil, &EvalError{Message: fmt.Sprintf("%d:%d: not a procedure", line, col)}

		default:
			return nil, &EvalError{Message: "unknown expression type"}
		}
	}
}

// Lambda is a user-defined closure.
type Lambda struct {
	Params    []string
	RestParam string // empty if no rest param
	Body      []Expr
	Env       *Env
}

func (l *Lambda) String() string {
	return "#<procedure>"
}

// SchemeCaseLambda represents a case-lambda with multiple arity clauses.
type SchemeCaseLambda struct {
	Clauses []*Lambda
}

func (cl *SchemeCaseLambda) String() string {
	return "#<procedure>"
}

// matchCaseLambda finds the clause matching the given argument count.
func matchCaseLambda(cl *SchemeCaseLambda, nargs int, callExpr *ListExpr) (*Lambda, error) {
	for _, c := range cl.Clauses {
		if c.RestParam != "" {
			if nargs >= len(c.Params) {
				return c, nil
			}
		} else {
			if nargs == len(c.Params) {
				return c, nil
			}
		}
	}
	line, col := callExpr.Pos()
	return nil, &EvalError{Message: fmt.Sprintf("%d:%d: case-lambda: no matching clause for %d arguments", line, col, nargs)}
}

func evalDefine(e *ListExpr, env *Env) (SchemeValue, error) {
	if len(e.Elements) < 3 {
		line, col := e.Pos()
		return nil, &EvalError{Message: fmt.Sprintf("%d:%d: define: bad syntax", line, col)}
	}

	switch target := e.Elements[1].(type) {
	case *SymbolExpr:
		// (define x expr)
		val, err := Eval(e.Elements[2], env)
		if err != nil {
			return nil, err
		}
		env.Set(target.Name, val)
		return &SchemeVoid{}, nil

	case *ListExpr:
		// (define (f params...) body...) or (define (f x . rest) body...)
		if len(target.Elements) == 0 {
			line, col := e.Pos()
			return nil, &EvalError{Message: fmt.Sprintf("%d:%d: define: bad syntax", line, col)}
		}
		nameSym, ok := target.Elements[0].(*SymbolExpr)
		if !ok {
			line, col := e.Pos()
			return nil, &EvalError{Message: fmt.Sprintf("%d:%d: define: bad syntax", line, col)}
		}
		params, restParam, err := parseParams(&ListExpr{Elements: target.Elements[1:], Dot: target.Dot}, e)
		if err != nil {
			return nil, err
		}
		lam := &Lambda{
			Params:    params,
			RestParam: restParam,
			Body:      e.Elements[2:],
			Env:       env,
		}
		env.Set(nameSym.Name, lam)
		return &SchemeVoid{}, nil

	default:
		line, col := e.Pos()
		return nil, &EvalError{Message: fmt.Sprintf("%d:%d: define: bad syntax", line, col)}
	}
}


func evalLambda(e *ListExpr, env *Env) (SchemeValue, error) {
	if len(e.Elements) < 3 {
		line, col := e.Pos()
		return nil, &EvalError{Message: fmt.Sprintf("%d:%d: lambda: bad syntax", line, col)}
	}

	// (lambda args body...) — single symbol means all-rest
	if sym, ok := e.Elements[1].(*SymbolExpr); ok {
		return &Lambda{
			RestParam: sym.Name,
			Body:      e.Elements[2:],
			Env:       env,
		}, nil
	}

	paramList, ok := e.Elements[1].(*ListExpr)
	if !ok {
		line, col := e.Pos()
		return nil, &EvalError{Message: fmt.Sprintf("%d:%d: lambda: bad syntax", line, col)}
	}

	params, restParam, err := parseParams(paramList, e)
	if err != nil {
		return nil, err
	}

	return &Lambda{
		Params:    params,
		RestParam: restParam,
		Body:      e.Elements[2:],
		Env:       env,
	}, nil
}

func evalCaseLambda(e *ListExpr, env *Env) (SchemeValue, error) {
	if len(e.Elements) < 2 {
		line, col := e.Pos()
		return nil, &EvalError{Message: fmt.Sprintf("%d:%d: case-lambda: bad syntax", line, col)}
	}
	var clauses []*Lambda
	for _, clauseExpr := range e.Elements[1:] {
		cl, ok := clauseExpr.(*ListExpr)
		if !ok || len(cl.Elements) < 2 {
			line, col := e.Pos()
			return nil, &EvalError{Message: fmt.Sprintf("%d:%d: case-lambda: bad clause", line, col)}
		}
		// Parse params - could be a list or a single symbol (rest-only)
		var params []string
		var restParam string
		switch p := cl.Elements[0].(type) {
		case *ListExpr:
			var err error
			params, restParam, err = parseParams(p, e)
			if err != nil {
				return nil, err
			}
		case *SymbolExpr:
			restParam = p.Name
		default:
			line, col := e.Pos()
			return nil, &EvalError{Message: fmt.Sprintf("%d:%d: case-lambda: bad clause params", line, col)}
		}
		clauses = append(clauses, &Lambda{
			Params:    params,
			RestParam: restParam,
			Body:      cl.Elements[1:],
			Env:       env,
		})
	}
	return &SchemeCaseLambda{Clauses: clauses}, nil
}

func nextContID() int64 {
	contIDCounter++
	return contIDCounter
}

func evalCallCC(args []SchemeValue, callExpr *ListExpr, env *Env) (SchemeValue, error) {
	if len(args) != 1 {
		line, col := callExpr.Pos()
		return nil, &EvalError{Message: fmt.Sprintf("%d:%d: call/cc: requires exactly 1 argument", line, col)}
	}
	proc := args[0]

	// Check if we're resuming a saved continuation
	if activeContResume != nil && activeContResume.callccExpr == callExpr {
		val := activeContResume.value
		activeContResume = nil
		return val, nil
	}

	// Get let context for continuation
	var letInfo *letSkipInfo
	if v, ok := env.Get("$$let-ctx$$"); ok {
		ctx := v.(*letCtxVal)
		letInfo = &letSkipInfo{letExpr: ctx.letExpr, env: ctx.env}
	}

	contID := nextContID()
	cont := &SchemeContinuation{
		id:         contID,
		callccExpr: callExpr,
		topExprIdx: currentTopExprIdx,
		letInfo:    letInfo,
	}

	// Track this call/cc as active (for escape detection)
	activeCallCCIDs[contID] = true
	defer delete(activeCallCCIDs, contID)

	// Call the procedure with the continuation
	result, err := applyFunc(proc, []SchemeValue{cont}, callExpr, env)
	if err != nil {
		// Check for escape continuation (invoked during the lambda)
		if jump, ok := err.(*contJumpError); ok && jump.cont.id == contID {
			return jump.value, nil
		}
		return nil, err
	}
	return result, nil
}

func applyFunc(proc SchemeValue, args []SchemeValue, callExpr *ListExpr, env *Env) (SchemeValue, error) {
	switch fn := proc.(type) {
	case *BuiltinProc:
		return fn.Fn(args, callExpr)
	case *EnvBuiltinProc:
		return fn.Fn(args, callExpr, env)
	case *Lambda:
		localEnv, err := bindLambdaArgs(fn, args, callExpr)
		if err != nil {
			return nil, err
		}
		var result SchemeValue
		for _, bodyExpr := range fn.Body {
			result, err = Eval(bodyExpr, localEnv)
			if err != nil {
				// Catch stale continuation jumps at lambda boundary
				if jump, ok := err.(*contJumpError); ok && !activeCallCCIDs[jump.cont.id] {
					return jump.value, nil
				}
				return nil, err
			}
		}
		return result, nil
	case *SchemeCaseLambda:
		clause, err := matchCaseLambda(fn, len(args), callExpr)
		if err != nil {
			return nil, err
		}
		return applyFunc(clause, args, callExpr, env)
	case *SchemeCallCC:
		return evalCallCC(args, callExpr, env)
	case *SchemeContinuation:
		if len(args) == 0 {
			return nil, &contJumpError{cont: fn, value: &SchemeVoid{}}
		} else if len(args) == 1 {
			return nil, &contJumpError{cont: fn, value: args[0]}
		} else {
			return nil, &contJumpError{cont: fn, value: &SchemeMultipleValues{Values: args}}
		}
	case *SchemeDynamicWind:
		if len(args) != 3 {
			line, col := callExpr.Pos()
			return nil, &EvalError{Message: fmt.Sprintf("%d:%d: dynamic-wind: requires exactly 3 arguments", line, col)}
		}
		inThunk, bodyThunk, outThunk := args[0], args[1], args[2]

		// Call in-thunk
		_, err := applyFunc(inThunk, []SchemeValue{}, callExpr, env)
		if err != nil {
			return nil, err
		}

		// Call body-thunk
		result, err := applyFunc(bodyThunk, []SchemeValue{}, callExpr, env)
		if err != nil {
			// Run out-thunk even on non-local exit
			applyFunc(outThunk, []SchemeValue{}, callExpr, env)
			return nil, err
		}

		// Normal exit: call out-thunk
		_, err = applyFunc(outThunk, []SchemeValue{}, callExpr, env)
		if err != nil {
			return nil, err
		}
		return result, nil
	default:
		line, col := callExpr.Pos()
		return nil, &EvalError{Message: fmt.Sprintf("%d:%d: not a procedure", line, col)}
	}
}

// parseParams extracts parameter names and optional rest parameter from a param list.
// Handles dot notation: (x y . rest)
func parseParams(paramList *ListExpr, callExpr *ListExpr) ([]string, string, error) {
	var params []string
	var restParam string
	for i, p := range paramList.Elements {
		ps, ok := p.(*SymbolExpr)
		if !ok {
			line, col := callExpr.Pos()
			return nil, "", &EvalError{Message: fmt.Sprintf("%d:%d: bad syntax", line, col)}
		}
		if ps.Name == "." {
			// Next element is rest param, must be last
			if i+2 != len(paramList.Elements) {
				line, col := callExpr.Pos()
				return nil, "", &EvalError{Message: fmt.Sprintf("%d:%d: bad syntax", line, col)}
			}
			rs, ok := paramList.Elements[i+1].(*SymbolExpr)
			if !ok {
				line, col := callExpr.Pos()
				return nil, "", &EvalError{Message: fmt.Sprintf("%d:%d: bad syntax", line, col)}
			}
			restParam = rs.Name
			break
		}
		params = append(params, ps.Name)
	}
	// Handle dotted pair syntax: (a b . rest)
	if paramList.Dot != nil {
		ds, ok := paramList.Dot.(*SymbolExpr)
		if !ok {
			line, col := callExpr.Pos()
			return nil, "", &EvalError{Message: fmt.Sprintf("%d:%d: bad syntax", line, col)}
		}
		restParam = ds.Name
	}
	return params, restParam, nil
}

// bindLambdaArgs creates a new env binding lambda params to args, handling rest params.
func bindLambdaArgs(fn *Lambda, args []SchemeValue, callExpr *ListExpr) (*Env, error) {
	if fn.RestParam == "" {
		if len(args) != len(fn.Params) {
			line, col := callExpr.Pos()
			return nil, &EvalError{Message: fmt.Sprintf("%d:%d: wrong number of arguments: expected %d, got %d", line, col, len(fn.Params), len(args))}
		}
	} else {
		if len(args) < len(fn.Params) {
			line, col := callExpr.Pos()
			return nil, &EvalError{Message: fmt.Sprintf("%d:%d: wrong number of arguments: expected at least %d, got %d", line, col, len(fn.Params), len(args))}
		}
	}
	localEnv := NewEnv(fn.Env)
	for i, p := range fn.Params {
		localEnv.Set(p, args[i])
	}
	if fn.RestParam != "" {
		rest := args[len(fn.Params):]
		localEnv.Set(fn.RestParam, sliceToList(rest))
	}
	return localEnv, nil
}

// sliceToList converts a Go slice of SchemeValues into a Scheme proper list.
func sliceToList(vals []SchemeValue) SchemeValue {
	var result SchemeValue = &SchemeEmpty{}
	for i := len(vals) - 1; i >= 0; i-- {
		result = &SchemePair{Car: vals[i], Cdr: result}
	}
	return result
}

// listToSlice converts a Scheme proper list to a Go slice.
func listToSlice(v SchemeValue) ([]SchemeValue, bool) {
	var result []SchemeValue
	cur := v
	for {
		switch c := cur.(type) {
		case *SchemePair:
			result = append(result, c.Car)
			cur = c.Cdr
		case *SchemeEmpty:
			return result, true
		default:
			return nil, false
		}
	}
}

// setupLet prepares the environment for a let form and returns the tail expression.
// For named let, it sets up the recursive binding.
func setupLet(e *ListExpr, env *Env) (Expr, *Env, error) {
	// Check for continuation let-skip (resuming a saved continuation)
	if activeLetSkip != nil && activeLetSkip.letExpr == e {
		capturedEnv := activeLetSkip.env
		activeLetSkip = nil

		// Determine body expressions
		var body []Expr
		if _, ok := e.Elements[1].(*SymbolExpr); ok {
			body = e.Elements[3:] // named let: (let name ((bindings)) body...)
		} else {
			body = e.Elements[2:] // regular let: (let ((bindings)) body...)
		}

		// Store let context for nested call/cc
		capturedEnv.Set("$$let-ctx$$", &letCtxVal{letExpr: e, env: capturedEnv})

		for _, b := range body[:len(body)-1] {
			_, err := Eval(b, capturedEnv)
			if err != nil {
				if jump, ok := err.(*contJumpError); ok && !activeCallCCIDs[jump.cont.id] {
					continue
				}
				return nil, nil, err
			}
		}
		return body[len(body)-1], capturedEnv, nil
	}

	if len(e.Elements) < 3 {
		line, col := e.Pos()
		return nil, nil, &EvalError{Message: fmt.Sprintf("%d:%d: let: bad syntax", line, col)}
	}

	// Named let: (let name ((var init) ...) body...)
	if sym, ok := e.Elements[1].(*SymbolExpr); ok {
		if len(e.Elements) < 4 {
			line, col := e.Pos()
			return nil, nil, &EvalError{Message: fmt.Sprintf("%d:%d: let: bad syntax", line, col)}
		}
		bindingsList, ok := e.Elements[2].(*ListExpr)
		if !ok {
			line, col := e.Pos()
			return nil, nil, &EvalError{Message: fmt.Sprintf("%d:%d: let: bad syntax", line, col)}
		}
		params := make([]string, len(bindingsList.Elements))
		inits := make([]SchemeValue, len(bindingsList.Elements))
		for i, b := range bindingsList.Elements {
			pair, ok := b.(*ListExpr)
			if !ok || len(pair.Elements) != 2 {
				line, col := e.Pos()
				return nil, nil, &EvalError{Message: fmt.Sprintf("%d:%d: let: bad syntax", line, col)}
			}
			ps, ok := pair.Elements[0].(*SymbolExpr)
			if !ok {
				line, col := e.Pos()
				return nil, nil, &EvalError{Message: fmt.Sprintf("%d:%d: let: bad syntax", line, col)}
			}
			params[i] = ps.Name
			val, err := Eval(pair.Elements[1], env)
			if err != nil {
				return nil, nil, err
			}
			inits[i] = val
		}
		lam := &Lambda{Params: params, Body: e.Elements[3:], Env: env}
		loopEnv := NewEnv(env)
		loopEnv.Set(sym.Name, lam)
		lam.Env = loopEnv
		localEnv := NewEnv(loopEnv)
		for i, p := range params {
			localEnv.Set(p, inits[i])
		}
		localEnv.Set("$$let-ctx$$", &letCtxVal{letExpr: e, env: localEnv})
		body := lam.Body
		// Eval all but last, return last as tail
		for _, bodyExpr := range body[:len(body)-1] {
			_, err := Eval(bodyExpr, localEnv)
			if err != nil {
				if jump, ok := err.(*contJumpError); ok && !activeCallCCIDs[jump.cont.id] {
					continue
				}
				return nil, nil, err
			}
		}
		return body[len(body)-1], localEnv, nil
	}

	// Regular let: (let ((var init) ...) body...)
	bindingsList, ok := e.Elements[1].(*ListExpr)
	if !ok {
		line, col := e.Pos()
		return nil, nil, &EvalError{Message: fmt.Sprintf("%d:%d: let: bad syntax", line, col)}
	}
	localEnv := NewEnv(env)
	for _, b := range bindingsList.Elements {
		pair, ok := b.(*ListExpr)
		if !ok || len(pair.Elements) != 2 {
			line, col := e.Pos()
			return nil, nil, &EvalError{Message: fmt.Sprintf("%d:%d: let: bad syntax", line, col)}
		}
		s, ok := pair.Elements[0].(*SymbolExpr)
		if !ok {
			line, col := e.Pos()
			return nil, nil, &EvalError{Message: fmt.Sprintf("%d:%d: let: bad syntax", line, col)}
		}
		val, err := Eval(pair.Elements[1], env)
		if err != nil {
			return nil, nil, err
		}
		localEnv.Set(s.Name, val)
	}
	localEnv.Set("$$let-ctx$$", &letCtxVal{letExpr: e, env: localEnv})
	body := e.Elements[2:]
	for _, bodyExpr := range body[:len(body)-1] {
		_, err := Eval(bodyExpr, localEnv)
		if err != nil {
			if jump, ok := err.(*contJumpError); ok && !activeCallCCIDs[jump.cont.id] {
				continue
			}
			return nil, nil, err
		}
	}
	return body[len(body)-1], localEnv, nil
}

// setupCond finds the matching cond clause and returns the tail expression.
func setupCond(e *ListExpr, env *Env) (Expr, *Env, bool, SchemeValue, error) {
	for _, clause := range e.Elements[1:] {
		cl, ok := clause.(*ListExpr)
		if !ok || len(cl.Elements) < 1 {
			line, col := e.Pos()
			return nil, nil, false, nil, &EvalError{Message: fmt.Sprintf("%d:%d: cond: bad syntax", line, col)}
		}
		if sym, ok := cl.Elements[0].(*SymbolExpr); ok && sym.Name == "else" {
			body := cl.Elements[1:]
			for _, b := range body[:len(body)-1] {
				_, err := Eval(b, env)
				if err != nil {
					return nil, nil, false, nil, err
				}
			}
			return body[len(body)-1], env, false, nil, nil
		}
		cond, err := Eval(cl.Elements[0], env)
		if err != nil {
			return nil, nil, false, nil, err
		}
		if isTruthy(cond) {
			// Single-expression clause: (cond (test)) returns test value
			if len(cl.Elements) == 1 {
				return nil, nil, true, cond, nil
			}
			// (cond (test => proc)) - apply proc to test result
			if len(cl.Elements) == 3 {
				if arrow, ok := cl.Elements[1].(*SymbolExpr); ok && arrow.Name == "=>" {
					proc, err := Eval(cl.Elements[2], env)
					if err != nil {
						return nil, nil, false, nil, err
					}
					result, err := applyFunc(proc, []SchemeValue{cond}, cl, env)
					if err != nil {
						return nil, nil, false, nil, err
					}
					return nil, nil, true, result, nil
				}
			}
			body := cl.Elements[1:]
			for _, b := range body[:len(body)-1] {
				_, err := Eval(b, env)
				if err != nil {
					return nil, nil, false, nil, err
				}
			}
			return body[len(body)-1], env, false, nil, nil
		}
	}
	return nil, nil, true, &SchemeVoid{}, nil
}

// quoteExpr converts a parsed Expr into a SchemeValue without evaluation.
func quoteExpr(expr Expr) SchemeValue {
	switch e := expr.(type) {
	case *NumberExpr:
		return &SchemeInt{Value: e.Value}
	case *FloatExpr:
		return &SchemeFloat{Value: e.Value}
	case *RationalExpr:
		return makeRational(e.Num, e.Den)
	case *BoolExpr:
		return &SchemeBool{Value: e.Value}
	case *StringExpr:
		return &SchemeString{Value: e.Value}
	case *SymbolExpr:
		return &SchemeSymbol{Name: e.Name}
	case *ListExpr:
		if len(e.Elements) == 0 && e.Dot == nil {
			return &SchemeEmpty{}
		}
		var result SchemeValue
		if e.Dot != nil {
			result = quoteExpr(e.Dot)
		} else {
			result = &SchemeEmpty{}
		}
		for i := len(e.Elements) - 1; i >= 0; i-- {
			result = &SchemePair{Car: quoteExpr(e.Elements[i]), Cdr: result}
		}
		return result
	default:
		return &SchemeVoid{}
	}
}

// evalQuasiquote handles quasiquote (backtick) expressions.
func evalQuasiquote(expr Expr, env *Env) (SchemeValue, error) {
	switch e := expr.(type) {
	case *ListExpr:
		if len(e.Elements) == 0 {
			return &SchemeEmpty{}, nil
		}
		// Check for (unquote x)
		if sym, ok := e.Elements[0].(*SymbolExpr); ok && sym.Name == "unquote" {
			if len(e.Elements) != 2 {
				line, col := e.Pos()
				return nil, &EvalError{Message: fmt.Sprintf("%d:%d: unquote: requires exactly 1 argument", line, col)}
			}
			return Eval(e.Elements[1], env)
		}
		// Build list, handling unquote-splicing
		var result []SchemeValue
		for _, elem := range e.Elements {
			if le, ok := elem.(*ListExpr); ok && len(le.Elements) >= 1 {
				if sym, ok := le.Elements[0].(*SymbolExpr); ok && sym.Name == "unquote-splicing" {
					if len(le.Elements) != 2 {
						line, col := le.Pos()
						return nil, &EvalError{Message: fmt.Sprintf("%d:%d: unquote-splicing: requires exactly 1 argument", line, col)}
					}
					val, err := Eval(le.Elements[1], env)
					if err != nil {
						return nil, err
					}
					// Splice the list into result
					items, err := schemeListToSlice(val)
					if err != nil {
						line, col := le.Pos()
						return nil, &EvalError{Message: fmt.Sprintf("%d:%d: unquote-splicing: expected list, got %s", line, col, val.String())}
					}
					result = append(result, items...)
					continue
				}
			}
			val, err := evalQuasiquote(elem, env)
			if err != nil {
				return nil, err
			}
			result = append(result, val)
		}
		// Build list from result (handle dotted pair)
		var list SchemeValue
		if e.Dot != nil {
			var err error
			list, err = evalQuasiquote(e.Dot, env)
			if err != nil {
				return nil, err
			}
		} else {
			list = &SchemeEmpty{}
		}
		for i := len(result) - 1; i >= 0; i-- {
			list = &SchemePair{Car: result[i], Cdr: list}
		}
		return list, nil
	default:
		return quoteExpr(expr), nil
	}
}

// schemeListToSlice converts a Scheme list value to a Go slice.
func schemeListToSlice(val SchemeValue) ([]SchemeValue, error) {
	var result []SchemeValue
	curr := val
	for {
		switch v := curr.(type) {
		case *SchemeEmpty:
			return result, nil
		case *SchemePair:
			result = append(result, v.Car)
			curr = v.Cdr
		case *SchemeList:
			return append(result, v.Elements...), nil
		default:
			return nil, fmt.Errorf("not a proper list")
		}
	}
}

// BuiltinProc is a built-in procedure.
type BuiltinProc struct {
	Name string
	Fn   func(args []SchemeValue, callExpr *ListExpr) (SchemeValue, error)
}

func (b *BuiltinProc) String() string {
	return fmt.Sprintf("#<procedure %s>", b.Name)
}

// isTruthy returns true for all values except #f.
func isTruthy(v SchemeValue) bool {
	if b, ok := v.(*SchemeBool); ok {
		return b.Value
	}
	return true
}


// requireInts extracts int64 values from args, returning an error if any aren't integers.
func requireInts(args []SchemeValue, name string, callExpr *ListExpr) ([]int64, error) {
	nums := make([]int64, len(args))
	for i, a := range args {
		n, ok := a.(*SchemeInt)
		if !ok {
			line, col := callExpr.Pos()
			return nil, &EvalError{Message: fmt.Sprintf("%d:%d: %s: expected number, got %s", line, col, name, a.String())}
		}
		nums[i] = n.Value
	}
	return nums, nil
}

// builtins is the map of built-in procedures.
var builtins = map[string]*BuiltinProc{}

func init() {
	builtins["+"] = &BuiltinProc{Name: "+", Fn: builtinAdd}
	builtins["-"] = &BuiltinProc{Name: "-", Fn: builtinSub}
	builtins["*"] = &BuiltinProc{Name: "*", Fn: builtinMul}
	builtins["/"] = &BuiltinProc{Name: "/", Fn: builtinDiv}
	builtins["<"] = &BuiltinProc{Name: "<", Fn: builtinLT}
	builtins[">"] = &BuiltinProc{Name: ">", Fn: builtinGT}
	builtins["="] = &BuiltinProc{Name: "=", Fn: builtinEq}
	builtins["<="] = &BuiltinProc{Name: "<=", Fn: builtinLE}
	builtins[">="] = &BuiltinProc{Name: ">=", Fn: builtinGE}
	builtins["not"] = &BuiltinProc{Name: "not", Fn: builtinNot}
	builtins["cons"] = &BuiltinProc{Name: "cons", Fn: builtinCons}
	builtins["car"] = &BuiltinProc{Name: "car", Fn: builtinCar}
	builtins["cdr"] = &BuiltinProc{Name: "cdr", Fn: builtinCdr}
	builtins["null?"] = &BuiltinProc{Name: "null?", Fn: builtinNullQ}
	builtins["pair?"] = &BuiltinProc{Name: "pair?", Fn: builtinPairQ}
	builtins["list"] = &BuiltinProc{Name: "list", Fn: builtinList}
	builtins["length"] = &BuiltinProc{Name: "length", Fn: builtinLength}
	builtins["append"] = &BuiltinProc{Name: "append", Fn: builtinAppend}
	builtins["reverse"] = &BuiltinProc{Name: "reverse", Fn: builtinReverse}
	builtins["number?"] = &BuiltinProc{Name: "number?", Fn: builtinNumberQ}
	builtins["string?"] = &BuiltinProc{Name: "string?", Fn: builtinStringQ}
	builtins["boolean?"] = &BuiltinProc{Name: "boolean?", Fn: builtinBooleanQ}
	builtins["symbol?"] = &BuiltinProc{Name: "symbol?", Fn: builtinSymbolQ}
	builtins["char?"] = &BuiltinProc{Name: "char?", Fn: builtinCharQ}
	builtins["string-append"] = &BuiltinProc{Name: "string-append", Fn: builtinStringAppend}
	builtins["string-length"] = &BuiltinProc{Name: "string-length", Fn: builtinStringLength}
	builtins["substring"] = &BuiltinProc{Name: "substring", Fn: builtinSubstring}
	builtins["string->number"] = &BuiltinProc{Name: "string->number", Fn: builtinStringToNumber}
	builtins["number->string"] = &BuiltinProc{Name: "number->string", Fn: builtinNumberToString}
	builtins["symbol->string"] = &BuiltinProc{Name: "symbol->string", Fn: builtinSymbolToString}
	builtins["string->symbol"] = &BuiltinProc{Name: "string->symbol", Fn: builtinStringToSymbol}
	builtins["syntax->datum"] = &BuiltinProc{Name: "syntax->datum", Fn: builtinSyntaxToDatum}
	builtins["datum->syntax"] = &BuiltinProc{Name: "datum->syntax", Fn: builtinDatumToSyntax}
	builtins["string-ref"] = &BuiltinProc{Name: "string-ref", Fn: builtinStringRef}
	builtins["string-copy"] = &BuiltinProc{Name: "string-copy", Fn: builtinStringCopy}
	builtins["string-set!"] = &BuiltinProc{Name: "string-set!", Fn: builtinStringSet}
	// L14 string immutability + char/integer conversion
	builtins["string->list"] = &BuiltinProc{Name: "string->list", Fn: builtinStringToList}
	builtins["list->string"] = &BuiltinProc{Name: "list->string", Fn: builtinListToString}
	builtins["char->integer"] = &BuiltinProc{Name: "char->integer", Fn: builtinCharToInteger}
	builtins["integer->char"] = &BuiltinProc{Name: "integer->char", Fn: builtinIntegerToChar}
	// L13 numeric
	builtins["abs"] = &BuiltinProc{Name: "abs", Fn: builtinAbs}
	builtins["modulo"] = &BuiltinProc{Name: "modulo", Fn: builtinModulo}
	builtins["remainder"] = &BuiltinProc{Name: "remainder", Fn: builtinRemainder}
	builtins["quotient"] = &BuiltinProc{Name: "quotient", Fn: builtinQuotient}
	builtins["min"] = &BuiltinProc{Name: "min", Fn: builtinMin}
	builtins["max"] = &BuiltinProc{Name: "max", Fn: builtinMax}
	builtins["expt"] = &BuiltinProc{Name: "expt", Fn: builtinExpt}
	builtins["zero?"] = &BuiltinProc{Name: "zero?", Fn: builtinZeroQ}
	builtins["positive?"] = &BuiltinProc{Name: "positive?", Fn: builtinPositiveQ}
	builtins["negative?"] = &BuiltinProc{Name: "negative?", Fn: builtinNegativeQ}
	builtins["odd?"] = &BuiltinProc{Name: "odd?", Fn: builtinOddQ}
	builtins["even?"] = &BuiltinProc{Name: "even?", Fn: builtinEvenQ}
	// L19 rationals / exact-inexact
	builtins["exact?"] = &BuiltinProc{Name: "exact?", Fn: builtinExactQ}
	builtins["inexact?"] = &BuiltinProc{Name: "inexact?", Fn: builtinInexactQ}
	builtins["exact->inexact"] = &BuiltinProc{Name: "exact->inexact", Fn: builtinExactToInexact}
	builtins["inexact->exact"] = &BuiltinProc{Name: "inexact->exact", Fn: builtinInexactToExact}
	builtins["numerator"] = &BuiltinProc{Name: "numerator", Fn: builtinNumerator}
	builtins["denominator"] = &BuiltinProc{Name: "denominator", Fn: builtinDenominator}
	builtins["integer?"] = &BuiltinProc{Name: "integer?", Fn: builtinIntegerQ}
	builtins["rational?"] = &BuiltinProc{Name: "rational?", Fn: builtinRationalQ}
	// L13 list
	builtins["list-ref"] = &BuiltinProc{Name: "list-ref", Fn: builtinListRef}
	builtins["list-tail"] = &BuiltinProc{Name: "list-tail", Fn: builtinListTail}
	builtins["list?"] = &BuiltinProc{Name: "list?", Fn: builtinListQ}
	builtins["assoc"] = &BuiltinProc{Name: "assoc", Fn: builtinAssoc}
	builtins["eq?"] = &BuiltinProc{Name: "eq?", Fn: builtinEqQ}
	builtins["equal?"] = &BuiltinProc{Name: "equal?", Fn: builtinEqualQ}
	// L13 char
	builtins["char-alphabetic?"] = &BuiltinProc{Name: "char-alphabetic?", Fn: builtinCharAlphabeticQ}
	builtins["char-numeric?"] = &BuiltinProc{Name: "char-numeric?", Fn: builtinCharNumericQ}
	builtins["char-upcase"] = &BuiltinProc{Name: "char-upcase", Fn: builtinCharUpcase}
	builtins["char-downcase"] = &BuiltinProc{Name: "char-downcase", Fn: builtinCharDowncase}
	builtins["char=?"] = &BuiltinProc{Name: "char=?", Fn: builtinCharEqQ}
	builtins["char<?"] = &BuiltinProc{Name: "char<?", Fn: builtinCharLtQ}
	// L13 string comparison
	builtins["string=?"] = &BuiltinProc{Name: "string=?", Fn: builtinStringEqQ}
	builtins["string<?"] = &BuiltinProc{Name: "string<?", Fn: builtinStringLtQ}
	builtins["string-ci=?"] = &BuiltinProc{Name: "string-ci=?", Fn: builtinStringCiEqQ}
	builtins["string-upcase"] = &BuiltinProc{Name: "string-upcase", Fn: builtinStringUpcase}
	builtins["string-downcase"] = &BuiltinProc{Name: "string-downcase", Fn: builtinStringDowncase}
	// L15
	builtins["eqv?"] = &BuiltinProc{Name: "eqv?", Fn: builtinEqvQ}
	builtins["vector"] = &BuiltinProc{Name: "vector", Fn: builtinVector}
	builtins["make-vector"] = &BuiltinProc{Name: "make-vector", Fn: builtinMakeVector}
	builtins["vector-ref"] = &BuiltinProc{Name: "vector-ref", Fn: builtinVectorRef}
	builtins["vector-set!"] = &BuiltinProc{Name: "vector-set!", Fn: builtinVectorSet}
	builtins["vector-length"] = &BuiltinProc{Name: "vector-length", Fn: builtinVectorLength}
	builtins["vector?"] = &BuiltinProc{Name: "vector?", Fn: builtinVectorQ}
	builtins["vector->list"] = &BuiltinProc{Name: "vector->list", Fn: builtinVectorToList}
	builtins["list->vector"] = &BuiltinProc{Name: "list->vector", Fn: builtinListToVector}
	builtins["error"] = &BuiltinProc{Name: "error", Fn: builtinError}
	// L21 pair mutation
	builtins["set-car!"] = &BuiltinProc{Name: "set-car!", Fn: builtinSetCar}
	builtins["set-cdr!"] = &BuiltinProc{Name: "set-cdr!", Fn: builtinSetCdr}
	// cxr helpers — register all standard c[ad]{2,4}r combinations
	for _, name := range []string{
		"caar", "cadr", "cdar", "cddr",
		"caaar", "caadr", "cadar", "caddr", "cdaar", "cdadr", "cddar", "cdddr",
		"caaaar", "caaadr", "caadar", "caaddr", "cadaar", "cadadr", "caddar", "cadddr",
		"cdaaar", "cdaadr", "cdadar", "cdaddr", "cddaar", "cddadr", "cdddar", "cddddr",
	} {
		builtins[name] = &BuiltinProc{Name: name, Fn: makeCxr(name)}
	}
	// L21 extras needed by real-world tests
	builtins["procedure?"] = &BuiltinProc{Name: "procedure?", Fn: builtinProcedureQ}
	builtins["gcd"] = &BuiltinProc{Name: "gcd", Fn: builtinGCD}
	builtins["lcm"] = &BuiltinProc{Name: "lcm", Fn: builtinLCM}
	builtins["truncate"] = &BuiltinProc{Name: "truncate", Fn: builtinTruncate}
	builtins["round"] = &BuiltinProc{Name: "round", Fn: builtinRound}
	builtins["make-string"] = &BuiltinProc{Name: "make-string", Fn: builtinMakeString}
	builtins["string"] = &BuiltinProc{Name: "string", Fn: builtinStringConstructor}
	builtins["string>?"] = &BuiltinProc{Name: "string>?", Fn: builtinStringGtQ}
	builtins["string<=?"] = &BuiltinProc{Name: "string<=?", Fn: builtinStringLeQ}
	builtins["string>=?"] = &BuiltinProc{Name: "string>=?", Fn: builtinStringGeQ}
	builtins["memq"] = &BuiltinProc{Name: "memq", Fn: builtinMemq}
	builtins["memv"] = &BuiltinProc{Name: "memv", Fn: builtinMemv}
	builtins["member"] = &BuiltinProc{Name: "member", Fn: builtinMember}
	builtins["assq"] = &BuiltinProc{Name: "assq", Fn: builtinAssq}
	builtins["assv"] = &BuiltinProc{Name: "assv", Fn: builtinAssv}
}

func builtinAdd(args []SchemeValue, callExpr *ListExpr) (SchemeValue, error) {
	var result SchemeValue = &SchemeInt{Value: 0}
	for _, a := range args {
		if !isNumber(a) {
			line, col := callExpr.Pos()
			return nil, &EvalError{Message: fmt.Sprintf("%d:%d: +: expected number, got %s", line, col, a.String())}
		}
		result = numAdd(result, a)
	}
	return result, nil
}

func builtinSub(args []SchemeValue, callExpr *ListExpr) (SchemeValue, error) {
	if len(args) == 0 {
		line, col := callExpr.Pos()
		return nil, &EvalError{Message: fmt.Sprintf("%d:%d: -: requires at least 1 argument", line, col)}
	}
	for _, a := range args {
		if !isNumber(a) {
			line, col := callExpr.Pos()
			return nil, &EvalError{Message: fmt.Sprintf("%d:%d: -: expected number, got %s", line, col, a.String())}
		}
	}
	if len(args) == 1 {
		return numNeg(args[0]), nil
	}
	result := args[0]
	for _, a := range args[1:] {
		result = numSub(result, a)
	}
	return result, nil
}

func builtinMul(args []SchemeValue, callExpr *ListExpr) (SchemeValue, error) {
	var result SchemeValue = &SchemeInt{Value: 1}
	for _, a := range args {
		if !isNumber(a) {
			line, col := callExpr.Pos()
			return nil, &EvalError{Message: fmt.Sprintf("%d:%d: *: expected number, got %s", line, col, a.String())}
		}
		result = numMul(result, a)
	}
	return result, nil
}

func builtinDiv(args []SchemeValue, callExpr *ListExpr) (SchemeValue, error) {
	if len(args) < 2 {
		line, col := callExpr.Pos()
		return nil, &EvalError{Message: fmt.Sprintf("%d:%d: /: requires at least 2 arguments", line, col)}
	}
	for _, a := range args {
		if !isNumber(a) {
			line, col := callExpr.Pos()
			return nil, &EvalError{Message: fmt.Sprintf("%d:%d: /: expected number, got %s", line, col, a.String())}
		}
	}
	result := args[0]
	for _, a := range args[1:] {
		if numIsZero(a) {
			line, col := callExpr.Pos()
			return nil, &EvalError{Message: fmt.Sprintf("%d:%d: /: division by zero", line, col)}
		}
		result = numDiv(result, a)
	}
	return result, nil
}

func requireNums(args []SchemeValue, name string, callExpr *ListExpr) error {
	for _, a := range args {
		if !isNumber(a) {
			line, col := callExpr.Pos()
			return &EvalError{Message: fmt.Sprintf("%d:%d: %s: expected number, got %s", line, col, name, a.String())}
		}
	}
	return nil
}

func builtinLT(args []SchemeValue, callExpr *ListExpr) (SchemeValue, error) {
	if err := requireNums(args, "<", callExpr); err != nil {
		return nil, err
	}
	if len(args) < 2 {
		line, col := callExpr.Pos()
		return nil, &EvalError{Message: fmt.Sprintf("%d:%d: <: requires at least 2 arguments", line, col)}
	}
	for i := 0; i < len(args)-1; i++ {
		if numCompare(args[i], args[i+1]) >= 0 {
			return &SchemeBool{Value: false}, nil
		}
	}
	return &SchemeBool{Value: true}, nil
}

func builtinGT(args []SchemeValue, callExpr *ListExpr) (SchemeValue, error) {
	if err := requireNums(args, ">", callExpr); err != nil {
		return nil, err
	}
	if len(args) < 2 {
		line, col := callExpr.Pos()
		return nil, &EvalError{Message: fmt.Sprintf("%d:%d: >: requires at least 2 arguments", line, col)}
	}
	for i := 0; i < len(args)-1; i++ {
		if numCompare(args[i], args[i+1]) <= 0 {
			return &SchemeBool{Value: false}, nil
		}
	}
	return &SchemeBool{Value: true}, nil
}

func builtinEq(args []SchemeValue, callExpr *ListExpr) (SchemeValue, error) {
	if err := requireNums(args, "=", callExpr); err != nil {
		return nil, err
	}
	if len(args) < 2 {
		line, col := callExpr.Pos()
		return nil, &EvalError{Message: fmt.Sprintf("%d:%d: =: requires at least 2 arguments", line, col)}
	}
	for i := 0; i < len(args)-1; i++ {
		if !numEqual(args[i], args[i+1]) {
			return &SchemeBool{Value: false}, nil
		}
	}
	return &SchemeBool{Value: true}, nil
}

func builtinLE(args []SchemeValue, callExpr *ListExpr) (SchemeValue, error) {
	if err := requireNums(args, "<=", callExpr); err != nil {
		return nil, err
	}
	if len(args) < 2 {
		line, col := callExpr.Pos()
		return nil, &EvalError{Message: fmt.Sprintf("%d:%d: <=: requires at least 2 arguments", line, col)}
	}
	for i := 0; i < len(args)-1; i++ {
		if numCompare(args[i], args[i+1]) > 0 {
			return &SchemeBool{Value: false}, nil
		}
	}
	return &SchemeBool{Value: true}, nil
}

func builtinGE(args []SchemeValue, callExpr *ListExpr) (SchemeValue, error) {
	if err := requireNums(args, ">=", callExpr); err != nil {
		return nil, err
	}
	if len(args) < 2 {
		line, col := callExpr.Pos()
		return nil, &EvalError{Message: fmt.Sprintf("%d:%d: >=: requires at least 2 arguments", line, col)}
	}
	for i := 0; i < len(args)-1; i++ {
		if numCompare(args[i], args[i+1]) < 0 {
			return &SchemeBool{Value: false}, nil
		}
	}
	return &SchemeBool{Value: true}, nil
}


func builtinNot(args []SchemeValue, callExpr *ListExpr) (SchemeValue, error) {
	if len(args) != 1 {
		line, col := callExpr.Pos()
		return nil, &EvalError{Message: fmt.Sprintf("%d:%d: not: requires exactly 1 argument", line, col)}
	}
	return &SchemeBool{Value: !isTruthy(args[0])}, nil
}

func builtinCons(args []SchemeValue, callExpr *ListExpr) (SchemeValue, error) {
	if len(args) != 2 {
		line, col := callExpr.Pos()
		return nil, &EvalError{Message: fmt.Sprintf("%d:%d: cons: requires exactly 2 arguments", line, col)}
	}
	return &SchemePair{Car: args[0], Cdr: args[1]}, nil
}

func builtinCar(args []SchemeValue, callExpr *ListExpr) (SchemeValue, error) {
	if len(args) != 1 {
		line, col := callExpr.Pos()
		return nil, &EvalError{Message: fmt.Sprintf("%d:%d: car: requires exactly 1 argument", line, col)}
	}
	p, ok := args[0].(*SchemePair)
	if !ok {
		line, col := callExpr.Pos()
		return nil, &EvalError{Message: fmt.Sprintf("%d:%d: car: expected pair", line, col)}
	}
	return p.Car, nil
}

func builtinCdr(args []SchemeValue, callExpr *ListExpr) (SchemeValue, error) {
	if len(args) != 1 {
		line, col := callExpr.Pos()
		return nil, &EvalError{Message: fmt.Sprintf("%d:%d: cdr: requires exactly 1 argument", line, col)}
	}
	p, ok := args[0].(*SchemePair)
	if !ok {
		line, col := callExpr.Pos()
		return nil, &EvalError{Message: fmt.Sprintf("%d:%d: cdr: expected pair", line, col)}
	}
	return p.Cdr, nil
}

func builtinNullQ(args []SchemeValue, callExpr *ListExpr) (SchemeValue, error) {
	if len(args) != 1 {
		line, col := callExpr.Pos()
		return nil, &EvalError{Message: fmt.Sprintf("%d:%d: null?: requires exactly 1 argument", line, col)}
	}
	_, isEmpty := args[0].(*SchemeEmpty)
	return &SchemeBool{Value: isEmpty}, nil
}

func builtinPairQ(args []SchemeValue, callExpr *ListExpr) (SchemeValue, error) {
	if len(args) != 1 {
		line, col := callExpr.Pos()
		return nil, &EvalError{Message: fmt.Sprintf("%d:%d: pair?: requires exactly 1 argument", line, col)}
	}
	_, isPair := args[0].(*SchemePair)
	return &SchemeBool{Value: isPair}, nil
}

func builtinList(args []SchemeValue, callExpr *ListExpr) (SchemeValue, error) {
	var result SchemeValue = &SchemeEmpty{}
	for i := len(args) - 1; i >= 0; i-- {
		result = &SchemePair{Car: args[i], Cdr: result}
	}
	return result, nil
}

func builtinLength(args []SchemeValue, callExpr *ListExpr) (SchemeValue, error) {
	if len(args) != 1 {
		line, col := callExpr.Pos()
		return nil, &EvalError{Message: fmt.Sprintf("%d:%d: length: requires exactly 1 argument", line, col)}
	}
	var count int64
	cur := args[0]
	for {
		switch c := cur.(type) {
		case *SchemePair:
			count++
			cur = c.Cdr
		case *SchemeEmpty:
			return &SchemeInt{Value: count}, nil
		default:
			line, col := callExpr.Pos()
			return nil, &EvalError{Message: fmt.Sprintf("%d:%d: length: expected list", line, col)}
		}
	}
}

func builtinAppend(args []SchemeValue, callExpr *ListExpr) (SchemeValue, error) {
	if len(args) == 0 {
		return &SchemeEmpty{}, nil
	}
	if len(args) == 1 {
		return args[0], nil
	}
	// Append all lists
	result := args[len(args)-1]
	for i := len(args) - 2; i >= 0; i-- {
		result = appendList(args[i], result)
	}
	return result, nil
}

func appendList(lst, tail SchemeValue) SchemeValue {
	switch l := lst.(type) {
	case *SchemeEmpty:
		return tail
	case *SchemePair:
		return &SchemePair{Car: l.Car, Cdr: appendList(l.Cdr, tail)}
	default:
		return tail
	}
}

func builtinReverse(args []SchemeValue, callExpr *ListExpr) (SchemeValue, error) {
	if len(args) != 1 {
		line, col := callExpr.Pos()
		return nil, &EvalError{Message: fmt.Sprintf("%d:%d: reverse: requires exactly 1 argument", line, col)}
	}
	var result SchemeValue = &SchemeEmpty{}
	cur := args[0]
	for {
		switch v := cur.(type) {
		case *SchemePair:
			result = &SchemePair{Car: v.Car, Cdr: result}
			cur = v.Cdr
		case *SchemeEmpty:
			return result, nil
		default:
			line, col := callExpr.Pos()
			return nil, &EvalError{Message: fmt.Sprintf("%d:%d: reverse: not a proper list", line, col)}
		}
	}
}

func builtinNumberQ(args []SchemeValue, callExpr *ListExpr) (SchemeValue, error) {
	if len(args) != 1 {
		line, col := callExpr.Pos()
		return nil, &EvalError{Message: fmt.Sprintf("%d:%d: number?: requires exactly 1 argument", line, col)}
	}
	return &SchemeBool{Value: isNumber(args[0])}, nil
}

func builtinStringQ(args []SchemeValue, callExpr *ListExpr) (SchemeValue, error) {
	if len(args) != 1 {
		line, col := callExpr.Pos()
		return nil, &EvalError{Message: fmt.Sprintf("%d:%d: string?: requires exactly 1 argument", line, col)}
	}
	_, ok := args[0].(*SchemeString)
	return &SchemeBool{Value: ok}, nil
}

func builtinBooleanQ(args []SchemeValue, callExpr *ListExpr) (SchemeValue, error) {
	if len(args) != 1 {
		line, col := callExpr.Pos()
		return nil, &EvalError{Message: fmt.Sprintf("%d:%d: boolean?: requires exactly 1 argument", line, col)}
	}
	_, ok := args[0].(*SchemeBool)
	return &SchemeBool{Value: ok}, nil
}

func builtinSymbolQ(args []SchemeValue, callExpr *ListExpr) (SchemeValue, error) {
	if len(args) != 1 {
		line, col := callExpr.Pos()
		return nil, &EvalError{Message: fmt.Sprintf("%d:%d: symbol?: requires exactly 1 argument", line, col)}
	}
	_, ok := args[0].(*SchemeSymbol)
	return &SchemeBool{Value: ok}, nil
}

func builtinCharQ(args []SchemeValue, callExpr *ListExpr) (SchemeValue, error) {
	if len(args) != 1 {
		line, col := callExpr.Pos()
		return nil, &EvalError{Message: fmt.Sprintf("%d:%d: char?: requires exactly 1 argument", line, col)}
	}
	_, ok := args[0].(*SchemeChar)
	return &SchemeBool{Value: ok}, nil
}

// EnvBuiltinProc is a builtin that needs access to the environment (for output).
type EnvBuiltinProc struct {
	Name string
	Fn   func(args []SchemeValue, callExpr *ListExpr, env *Env) (SchemeValue, error)
}

func (b *EnvBuiltinProc) String() string {
	return fmt.Sprintf("#<procedure %s>", b.Name)
}

func getOutputPort(env *Env) *strings.Builder {
	if v, ok := env.Get("$$output$$"); ok {
		if p, ok := v.(*outputPort); ok {
			return p.buf
		}
	}
	return nil
}

// displayValue writes a value in display format (no quotes on strings).
func displayValue(v SchemeValue) string {
	switch val := v.(type) {
	case *SchemeString:
		return val.Value
	case *SchemePair:
		return displayPair(val)
	case *SchemeChar:
		return string(val.Value)
	default:
		return v.String()
	}
}

func displayPair(p *SchemePair) string {
	var parts []string
	cur := SchemeValue(p)
	for {
		switch c := cur.(type) {
		case *SchemePair:
			parts = append(parts, displayValue(c.Car))
			cur = c.Cdr
		case *SchemeEmpty:
			return "(" + strings.Join(parts, " ") + ")"
		default:
			return "(" + strings.Join(parts, " ") + " . " + displayValue(cur) + ")"
		}
	}
}

func builtinDisplay(args []SchemeValue, callExpr *ListExpr, env *Env) (SchemeValue, error) {
	if len(args) != 1 {
		line, col := callExpr.Pos()
		return nil, &EvalError{Message: fmt.Sprintf("%d:%d: display: requires exactly 1 argument", line, col)}
	}
	if buf := getOutputPort(env); buf != nil {
		buf.WriteString(displayValue(args[0]))
	}
	return &SchemeVoid{}, nil
}

func builtinWrite(args []SchemeValue, callExpr *ListExpr, env *Env) (SchemeValue, error) {
	if len(args) != 1 {
		line, col := callExpr.Pos()
		return nil, &EvalError{Message: fmt.Sprintf("%d:%d: write: requires exactly 1 argument", line, col)}
	}
	if buf := getOutputPort(env); buf != nil {
		buf.WriteString(args[0].String())
	}
	return &SchemeVoid{}, nil
}

func builtinNewline(args []SchemeValue, callExpr *ListExpr, env *Env) (SchemeValue, error) {
	if len(args) != 0 {
		line, col := callExpr.Pos()
		return nil, &EvalError{Message: fmt.Sprintf("%d:%d: newline: requires 0 arguments", line, col)}
	}
	if buf := getOutputPort(env); buf != nil {
		buf.WriteString("\n")
	}
	return &SchemeVoid{}, nil
}

func builtinStringAppend(args []SchemeValue, callExpr *ListExpr) (SchemeValue, error) {
	var sb strings.Builder
	for _, a := range args {
		s, ok := a.(*SchemeString)
		if !ok {
			line, col := callExpr.Pos()
			return nil, &EvalError{Message: fmt.Sprintf("%d:%d: string-append: expected string", line, col)}
		}
		sb.WriteString(s.Value)
	}
	return &SchemeString{Value: sb.String()}, nil
}

func builtinStringLength(args []SchemeValue, callExpr *ListExpr) (SchemeValue, error) {
	if len(args) != 1 {
		line, col := callExpr.Pos()
		return nil, &EvalError{Message: fmt.Sprintf("%d:%d: string-length: requires exactly 1 argument", line, col)}
	}
	s, ok := args[0].(*SchemeString)
	if !ok {
		line, col := callExpr.Pos()
		return nil, &EvalError{Message: fmt.Sprintf("%d:%d: string-length: expected string", line, col)}
	}
	return &SchemeInt{Value: int64(len([]rune(s.Value)))}, nil
}

func builtinSubstring(args []SchemeValue, callExpr *ListExpr) (SchemeValue, error) {
	if len(args) != 3 {
		line, col := callExpr.Pos()
		return nil, &EvalError{Message: fmt.Sprintf("%d:%d: substring: requires exactly 3 arguments", line, col)}
	}
	s, ok := args[0].(*SchemeString)
	if !ok {
		line, col := callExpr.Pos()
		return nil, &EvalError{Message: fmt.Sprintf("%d:%d: substring: expected string", line, col)}
	}
	start, ok := args[1].(*SchemeInt)
	if !ok {
		line, col := callExpr.Pos()
		return nil, &EvalError{Message: fmt.Sprintf("%d:%d: substring: expected number", line, col)}
	}
	end, ok := args[2].(*SchemeInt)
	if !ok {
		line, col := callExpr.Pos()
		return nil, &EvalError{Message: fmt.Sprintf("%d:%d: substring: expected number", line, col)}
	}
	runes := []rune(s.Value)
	return &SchemeString{Value: string(runes[start.Value:end.Value])}, nil
}

func builtinStringToNumber(args []SchemeValue, callExpr *ListExpr) (SchemeValue, error) {
	if len(args) != 1 {
		line, col := callExpr.Pos()
		return nil, &EvalError{Message: fmt.Sprintf("%d:%d: string->number: requires exactly 1 argument", line, col)}
	}
	s, ok := args[0].(*SchemeString)
	if !ok {
		line, col := callExpr.Pos()
		return nil, &EvalError{Message: fmt.Sprintf("%d:%d: string->number: expected string", line, col)}
	}
	// Try integer
	if n, err := strconv.ParseInt(s.Value, 10, 64); err == nil {
		return &SchemeInt{Value: n}, nil
	}
	// Try float
	if f, err := strconv.ParseFloat(s.Value, 64); err == nil {
		return &SchemeFloat{Value: f}, nil
	}
	return &SchemeBool{Value: false}, nil
}

func builtinNumberToString(args []SchemeValue, callExpr *ListExpr) (SchemeValue, error) {
	if len(args) != 1 {
		line, col := callExpr.Pos()
		return nil, &EvalError{Message: fmt.Sprintf("%d:%d: number->string: requires exactly 1 argument", line, col)}
	}
	if !isNumber(args[0]) {
		line, col := callExpr.Pos()
		return nil, &EvalError{Message: fmt.Sprintf("%d:%d: number->string: expected number", line, col)}
	}
	return &SchemeString{Value: args[0].String()}, nil
}

func builtinSymbolToString(args []SchemeValue, callExpr *ListExpr) (SchemeValue, error) {
	if len(args) != 1 {
		line, col := callExpr.Pos()
		return nil, &EvalError{Message: fmt.Sprintf("%d:%d: symbol->string: requires exactly 1 argument", line, col)}
	}
	s, ok := args[0].(*SchemeSymbol)
	if !ok {
		line, col := callExpr.Pos()
		return nil, &EvalError{Message: fmt.Sprintf("%d:%d: symbol->string: expected symbol", line, col)}
	}
	return &SchemeString{Value: s.Name}, nil
}

func builtinStringToSymbol(args []SchemeValue, callExpr *ListExpr) (SchemeValue, error) {
	if len(args) != 1 {
		line, col := callExpr.Pos()
		return nil, &EvalError{Message: fmt.Sprintf("%d:%d: string->symbol: requires exactly 1 argument", line, col)}
	}
	s, ok := args[0].(*SchemeString)
	if !ok {
		line, col := callExpr.Pos()
		return nil, &EvalError{Message: fmt.Sprintf("%d:%d: string->symbol: expected string", line, col)}
	}
	return &SchemeSymbol{Name: s.Value}, nil
}

// syntax->datum: In our simple implementation, syntax objects ARE datums (SchemeValues),
// so this is essentially identity. But if input is a symbol wrapping a syntax object we just return it.
func builtinSyntaxToDatum(args []SchemeValue, callExpr *ListExpr) (SchemeValue, error) {
	if len(args) != 1 {
		line, col := callExpr.Pos()
		return nil, &EvalError{Message: fmt.Sprintf("%d:%d: syntax->datum: requires exactly 1 argument", line, col)}
	}
	return args[0], nil
}

// datum->syntax: Takes a template-id and a datum, returns the datum as a syntax object.
// In our simple implementation, this is essentially identity — the template-id is used for
// lexical context but we handle that at the macro expansion level.
func builtinDatumToSyntax(args []SchemeValue, callExpr *ListExpr) (SchemeValue, error) {
	if len(args) != 2 {
		line, col := callExpr.Pos()
		return nil, &EvalError{Message: fmt.Sprintf("%d:%d: datum->syntax: requires exactly 2 arguments", line, col)}
	}
	return args[1], nil
}

func builtinStringRef(args []SchemeValue, callExpr *ListExpr) (SchemeValue, error) {
	if len(args) != 2 {
		line, col := callExpr.Pos()
		return nil, &EvalError{Message: fmt.Sprintf("%d:%d: string-ref: requires exactly 2 arguments", line, col)}
	}
	s, ok := args[0].(*SchemeString)
	if !ok {
		line, col := callExpr.Pos()
		return nil, &EvalError{Message: fmt.Sprintf("%d:%d: string-ref: expected string", line, col)}
	}
	idx, ok := args[1].(*SchemeInt)
	if !ok {
		line, col := callExpr.Pos()
		return nil, &EvalError{Message: fmt.Sprintf("%d:%d: string-ref: expected number", line, col)}
	}
	runes := []rune(s.Value)
	if idx.Value < 0 || idx.Value >= int64(len(runes)) {
		line, col := callExpr.Pos()
		return nil, &EvalError{Message: fmt.Sprintf("%d:%d: string-ref: index out of range", line, col)}
	}
	return &SchemeChar{Value: runes[idx.Value]}, nil
}

func builtinStringCopy(args []SchemeValue, callExpr *ListExpr) (SchemeValue, error) {
	if len(args) != 1 {
		line, col := callExpr.Pos()
		return nil, &EvalError{Message: fmt.Sprintf("%d:%d: string-copy: requires exactly 1 argument", line, col)}
	}
	s, ok := args[0].(*SchemeString)
	if !ok {
		line, col := callExpr.Pos()
		return nil, &EvalError{Message: fmt.Sprintf("%d:%d: string-copy: expected string", line, col)}
	}
	return &SchemeString{Value: s.Value, Mutable: true}, nil
}

func builtinStringSet(args []SchemeValue, callExpr *ListExpr) (SchemeValue, error) {
	line, col := callExpr.Pos()
	if len(args) != 3 {
		return nil, &EvalError{Message: fmt.Sprintf("%d:%d: string-set!: requires exactly 3 arguments", line, col)}
	}
	s, ok := args[0].(*SchemeString)
	if !ok {
		return nil, &EvalError{Message: fmt.Sprintf("%d:%d: string-set!: expected string", line, col)}
	}
	if !s.Mutable {
		return nil, &EvalError{Message: fmt.Sprintf("%d:%d: string-set!: strings are immutable", line, col)}
	}
	idx, ok := args[1].(*SchemeInt)
	if !ok {
		return nil, &EvalError{Message: fmt.Sprintf("%d:%d: string-set!: expected integer index", line, col)}
	}
	ch, ok := args[2].(*SchemeChar)
	if !ok {
		return nil, &EvalError{Message: fmt.Sprintf("%d:%d: string-set!: expected character", line, col)}
	}
	runes := []rune(s.Value)
	i := int(idx.Value)
	if i < 0 || i >= len(runes) {
		return nil, &EvalError{Message: fmt.Sprintf("%d:%d: string-set!: index out of range", line, col)}
	}
	runes[i] = ch.Value
	s.Value = string(runes)
	return &SchemeVoid{}, nil
}

func builtinStringToList(args []SchemeValue, callExpr *ListExpr) (SchemeValue, error) {
	if len(args) != 1 {
		line, col := callExpr.Pos()
		return nil, &EvalError{Message: fmt.Sprintf("%d:%d: string->list: requires exactly 1 argument", line, col)}
	}
	s, ok := args[0].(*SchemeString)
	if !ok {
		line, col := callExpr.Pos()
		return nil, &EvalError{Message: fmt.Sprintf("%d:%d: string->list: expected string", line, col)}
	}
	var result SchemeValue = &SchemeEmpty{}
	runes := []rune(s.Value)
	for i := len(runes) - 1; i >= 0; i-- {
		result = &SchemePair{Car: &SchemeChar{Value: runes[i]}, Cdr: result}
	}
	return result, nil
}

func builtinListToString(args []SchemeValue, callExpr *ListExpr) (SchemeValue, error) {
	if len(args) != 1 {
		line, col := callExpr.Pos()
		return nil, &EvalError{Message: fmt.Sprintf("%d:%d: list->string: requires exactly 1 argument", line, col)}
	}
	var runes []rune
	cur := args[0]
	for {
		pair, ok := cur.(*SchemePair)
		if !ok {
			break
		}
		ch, ok := pair.Car.(*SchemeChar)
		if !ok {
			line, col := callExpr.Pos()
			return nil, &EvalError{Message: fmt.Sprintf("%d:%d: list->string: expected list of characters", line, col)}
		}
		runes = append(runes, ch.Value)
		cur = pair.Cdr
	}
	return &SchemeString{Value: string(runes), Mutable: true}, nil
}

func builtinCharToInteger(args []SchemeValue, callExpr *ListExpr) (SchemeValue, error) {
	if len(args) != 1 {
		line, col := callExpr.Pos()
		return nil, &EvalError{Message: fmt.Sprintf("%d:%d: char->integer: requires exactly 1 argument", line, col)}
	}
	ch, ok := args[0].(*SchemeChar)
	if !ok {
		line, col := callExpr.Pos()
		return nil, &EvalError{Message: fmt.Sprintf("%d:%d: char->integer: expected character", line, col)}
	}
	return &SchemeInt{Value: int64(ch.Value)}, nil
}

func builtinIntegerToChar(args []SchemeValue, callExpr *ListExpr) (SchemeValue, error) {
	if len(args) != 1 {
		line, col := callExpr.Pos()
		return nil, &EvalError{Message: fmt.Sprintf("%d:%d: integer->char: requires exactly 1 argument", line, col)}
	}
	n, ok := args[0].(*SchemeInt)
	if !ok {
		line, col := callExpr.Pos()
		return nil, &EvalError{Message: fmt.Sprintf("%d:%d: integer->char: expected integer", line, col)}
	}
	return &SchemeChar{Value: rune(n.Value)}, nil
}

func builtinMapEnv(args []SchemeValue, callExpr *ListExpr, env *Env) (SchemeValue, error) {
	if len(args) < 2 {
		line, col := callExpr.Pos()
		return nil, &EvalError{Message: fmt.Sprintf("%d:%d: map: requires at least 2 arguments", line, col)}
	}
	fn := args[0]
	// Convert all list args to slices
	lists := make([][]SchemeValue, len(args)-1)
	for i := 1; i < len(args); i++ {
		elems, ok := listToSlice(args[i])
		if !ok {
			line, col := callExpr.Pos()
			return nil, &EvalError{Message: fmt.Sprintf("%d:%d: map: expected list", line, col)}
		}
		lists[i-1] = elems
	}
	if len(lists) == 0 {
		return &SchemeEmpty{}, nil
	}
	n := len(lists[0])
	for _, l := range lists[1:] {
		if len(l) != n {
			line, col := callExpr.Pos()
			return nil, &EvalError{Message: fmt.Sprintf("%d:%d: map: lists must have same length", line, col)}
		}
	}
	results := make([]SchemeValue, n)
	for i := 0; i < n; i++ {
		fnArgs := make([]SchemeValue, len(lists))
		for j := range lists {
			fnArgs[j] = lists[j][i]
		}
		res, err := applyFunc(fn, fnArgs, callExpr, env)
		if err != nil {
			return nil, err
		}
		results[i] = res
	}
	return sliceToList(results), nil
}

func builtinApplyEnv(args []SchemeValue, callExpr *ListExpr, env *Env) (SchemeValue, error) {
	if len(args) < 2 {
		line, col := callExpr.Pos()
		return nil, &EvalError{Message: fmt.Sprintf("%d:%d: apply: requires at least 2 arguments", line, col)}
	}
	fn := args[0]
	// Last arg must be a list; prefix args are prepended
	lastArg := args[len(args)-1]
	tailArgs, ok := listToSlice(lastArg)
	if !ok {
		line, col := callExpr.Pos()
		return nil, &EvalError{Message: fmt.Sprintf("%d:%d: apply: last argument must be a list", line, col)}
	}
	// Combine prefix args + tail list
	var allArgs []SchemeValue
	allArgs = append(allArgs, args[1:len(args)-1]...)
	allArgs = append(allArgs, tailArgs...)

	return applyFunc(fn, allArgs, callExpr, env)
}

// --- L13 builtins ---

func builtinAbs(args []SchemeValue, callExpr *ListExpr) (SchemeValue, error) {
	if len(args) != 1 {
		line, col := callExpr.Pos()
		return nil, &EvalError{Message: fmt.Sprintf("%d:%d: abs: requires exactly 1 argument", line, col)}
	}
	if !isNumber(args[0]) {
		line, col := callExpr.Pos()
		return nil, &EvalError{Message: fmt.Sprintf("%d:%d: abs: expected number", line, col)}
	}
	return numAbs(args[0]), nil
}

func builtinModulo(args []SchemeValue, callExpr *ListExpr) (SchemeValue, error) {
	nums, err := requireInts(args, "modulo", callExpr)
	if err != nil {
		return nil, err
	}
	if len(nums) != 2 {
		line, col := callExpr.Pos()
		return nil, &EvalError{Message: fmt.Sprintf("%d:%d: modulo: requires exactly 2 arguments", line, col)}
	}
	if nums[1] == 0 {
		line, col := callExpr.Pos()
		return nil, &EvalError{Message: fmt.Sprintf("%d:%d: modulo: division by zero", line, col)}
	}
	// modulo takes the sign of the divisor
	r := nums[0] % nums[1]
	if r != 0 && (r > 0) != (nums[1] > 0) {
		r += nums[1]
	}
	return &SchemeInt{Value: r}, nil
}

func builtinRemainder(args []SchemeValue, callExpr *ListExpr) (SchemeValue, error) {
	nums, err := requireInts(args, "remainder", callExpr)
	if err != nil {
		return nil, err
	}
	if len(nums) != 2 {
		line, col := callExpr.Pos()
		return nil, &EvalError{Message: fmt.Sprintf("%d:%d: remainder: requires exactly 2 arguments", line, col)}
	}
	if nums[1] == 0 {
		line, col := callExpr.Pos()
		return nil, &EvalError{Message: fmt.Sprintf("%d:%d: remainder: division by zero", line, col)}
	}
	// Go's % already gives remainder with sign of dividend
	return &SchemeInt{Value: nums[0] % nums[1]}, nil
}

func builtinQuotient(args []SchemeValue, callExpr *ListExpr) (SchemeValue, error) {
	nums, err := requireInts(args, "quotient", callExpr)
	if err != nil {
		return nil, err
	}
	if len(nums) != 2 {
		line, col := callExpr.Pos()
		return nil, &EvalError{Message: fmt.Sprintf("%d:%d: quotient: requires exactly 2 arguments", line, col)}
	}
	if nums[1] == 0 {
		line, col := callExpr.Pos()
		return nil, &EvalError{Message: fmt.Sprintf("%d:%d: quotient: division by zero", line, col)}
	}
	// Truncate toward zero (Go's default integer division behavior)
	return &SchemeInt{Value: nums[0] / nums[1]}, nil
}

func builtinMin(args []SchemeValue, callExpr *ListExpr) (SchemeValue, error) {
	if len(args) == 0 {
		line, col := callExpr.Pos()
		return nil, &EvalError{Message: fmt.Sprintf("%d:%d: min: requires at least 1 argument", line, col)}
	}
	if err := requireNums(args, "min", callExpr); err != nil {
		return nil, err
	}
	m := args[0]
	for _, a := range args[1:] {
		if numCompare(a, m) < 0 {
			m = a
		}
	}
	return m, nil
}

func builtinMax(args []SchemeValue, callExpr *ListExpr) (SchemeValue, error) {
	if len(args) == 0 {
		line, col := callExpr.Pos()
		return nil, &EvalError{Message: fmt.Sprintf("%d:%d: max: requires at least 1 argument", line, col)}
	}
	if err := requireNums(args, "max", callExpr); err != nil {
		return nil, err
	}
	m := args[0]
	for _, a := range args[1:] {
		if numCompare(a, m) > 0 {
			m = a
		}
	}
	return m, nil
}

func builtinExpt(args []SchemeValue, callExpr *ListExpr) (SchemeValue, error) {
	if len(args) != 2 {
		line, col := callExpr.Pos()
		return nil, &EvalError{Message: fmt.Sprintf("%d:%d: expt: requires exactly 2 arguments", line, col)}
	}
	if err := requireNums(args, "expt", callExpr); err != nil {
		return nil, err
	}
	// For integer exponents with exact base, compute exactly
	if expInt, ok := args[1].(*SchemeInt); ok {
		exp := expInt.Value
		if exp >= 0 {
			var result SchemeValue = &SchemeInt{Value: 1}
			for i := int64(0); i < exp; i++ {
				result = numMul(result, args[0])
			}
			return result, nil
		}
	}
	// Fallback to float
	af, _ := toFloat64(args[0])
	bf, _ := toFloat64(args[1])
	return &SchemeFloat{Value: math.Pow(af, bf)}, nil
}

func builtinZeroQ(args []SchemeValue, callExpr *ListExpr) (SchemeValue, error) {
	if len(args) != 1 {
		line, col := callExpr.Pos()
		return nil, &EvalError{Message: fmt.Sprintf("%d:%d: zero?: requires exactly 1 argument", line, col)}
	}
	if !isNumber(args[0]) {
		line, col := callExpr.Pos()
		return nil, &EvalError{Message: fmt.Sprintf("%d:%d: zero?: expected number", line, col)}
	}
	return &SchemeBool{Value: numIsZero(args[0])}, nil
}

func builtinPositiveQ(args []SchemeValue, callExpr *ListExpr) (SchemeValue, error) {
	if len(args) != 1 {
		line, col := callExpr.Pos()
		return nil, &EvalError{Message: fmt.Sprintf("%d:%d: positive?: requires exactly 1 argument", line, col)}
	}
	if !isNumber(args[0]) {
		line, col := callExpr.Pos()
		return nil, &EvalError{Message: fmt.Sprintf("%d:%d: positive?: expected number", line, col)}
	}
	return &SchemeBool{Value: numIsPositive(args[0])}, nil
}

func builtinNegativeQ(args []SchemeValue, callExpr *ListExpr) (SchemeValue, error) {
	if len(args) != 1 {
		line, col := callExpr.Pos()
		return nil, &EvalError{Message: fmt.Sprintf("%d:%d: negative?: requires exactly 1 argument", line, col)}
	}
	if !isNumber(args[0]) {
		line, col := callExpr.Pos()
		return nil, &EvalError{Message: fmt.Sprintf("%d:%d: negative?: expected number", line, col)}
	}
	return &SchemeBool{Value: numIsNegative(args[0])}, nil
}

func builtinOddQ(args []SchemeValue, callExpr *ListExpr) (SchemeValue, error) {
	if len(args) != 1 {
		line, col := callExpr.Pos()
		return nil, &EvalError{Message: fmt.Sprintf("%d:%d: odd?: requires exactly 1 argument", line, col)}
	}
	n, ok := args[0].(*SchemeInt)
	if !ok {
		line, col := callExpr.Pos()
		return nil, &EvalError{Message: fmt.Sprintf("%d:%d: odd?: expected number", line, col)}
	}
	return &SchemeBool{Value: n.Value%2 != 0}, nil
}

// --- L19 builtins ---

func builtinExactQ(args []SchemeValue, callExpr *ListExpr) (SchemeValue, error) {
	if len(args) != 1 {
		line, col := callExpr.Pos()
		return nil, &EvalError{Message: fmt.Sprintf("%d:%d: exact?: requires exactly 1 argument", line, col)}
	}
	return &SchemeBool{Value: isExact(args[0])}, nil
}

func builtinInexactQ(args []SchemeValue, callExpr *ListExpr) (SchemeValue, error) {
	if len(args) != 1 {
		line, col := callExpr.Pos()
		return nil, &EvalError{Message: fmt.Sprintf("%d:%d: inexact?: requires exactly 1 argument", line, col)}
	}
	_, ok := args[0].(*SchemeFloat)
	return &SchemeBool{Value: ok}, nil
}

func builtinExactToInexact(args []SchemeValue, callExpr *ListExpr) (SchemeValue, error) {
	if len(args) != 1 {
		line, col := callExpr.Pos()
		return nil, &EvalError{Message: fmt.Sprintf("%d:%d: exact->inexact: requires exactly 1 argument", line, col)}
	}
	if !isNumber(args[0]) {
		line, col := callExpr.Pos()
		return nil, &EvalError{Message: fmt.Sprintf("%d:%d: exact->inexact: expected number", line, col)}
	}
	return exactToInexact(args[0]), nil
}

func builtinInexactToExact(args []SchemeValue, callExpr *ListExpr) (SchemeValue, error) {
	if len(args) != 1 {
		line, col := callExpr.Pos()
		return nil, &EvalError{Message: fmt.Sprintf("%d:%d: inexact->exact: requires exactly 1 argument", line, col)}
	}
	if !isNumber(args[0]) {
		line, col := callExpr.Pos()
		return nil, &EvalError{Message: fmt.Sprintf("%d:%d: inexact->exact: expected number", line, col)}
	}
	return inexactToExact(args[0]), nil
}

func builtinNumerator(args []SchemeValue, callExpr *ListExpr) (SchemeValue, error) {
	if len(args) != 1 {
		line, col := callExpr.Pos()
		return nil, &EvalError{Message: fmt.Sprintf("%d:%d: numerator: requires exactly 1 argument", line, col)}
	}
	switch n := args[0].(type) {
	case *SchemeInt:
		return n, nil
	case *SchemeRational:
		return &SchemeInt{Value: n.Num}, nil
	default:
		line, col := callExpr.Pos()
		return nil, &EvalError{Message: fmt.Sprintf("%d:%d: numerator: expected rational number", line, col)}
	}
}

func builtinDenominator(args []SchemeValue, callExpr *ListExpr) (SchemeValue, error) {
	if len(args) != 1 {
		line, col := callExpr.Pos()
		return nil, &EvalError{Message: fmt.Sprintf("%d:%d: denominator: requires exactly 1 argument", line, col)}
	}
	switch args[0].(type) {
	case *SchemeInt:
		return &SchemeInt{Value: 1}, nil
	case *SchemeRational:
		return &SchemeInt{Value: args[0].(*SchemeRational).Den}, nil
	default:
		line, col := callExpr.Pos()
		return nil, &EvalError{Message: fmt.Sprintf("%d:%d: denominator: expected rational number", line, col)}
	}
}

func builtinIntegerQ(args []SchemeValue, callExpr *ListExpr) (SchemeValue, error) {
	if len(args) != 1 {
		line, col := callExpr.Pos()
		return nil, &EvalError{Message: fmt.Sprintf("%d:%d: integer?: requires exactly 1 argument", line, col)}
	}
	switch n := args[0].(type) {
	case *SchemeInt:
		return &SchemeBool{Value: true}, nil
	case *SchemeFloat:
		return &SchemeBool{Value: n.Value == math.Floor(n.Value)}, nil
	default:
		return &SchemeBool{Value: false}, nil
	}
}

func builtinRationalQ(args []SchemeValue, callExpr *ListExpr) (SchemeValue, error) {
	if len(args) != 1 {
		line, col := callExpr.Pos()
		return nil, &EvalError{Message: fmt.Sprintf("%d:%d: rational?: requires exactly 1 argument", line, col)}
	}
	switch args[0].(type) {
	case *SchemeInt, *SchemeRational:
		return &SchemeBool{Value: true}, nil
	default:
		return &SchemeBool{Value: false}, nil
	}
}

func builtinEvenQ(args []SchemeValue, callExpr *ListExpr) (SchemeValue, error) {
	if len(args) != 1 {
		line, col := callExpr.Pos()
		return nil, &EvalError{Message: fmt.Sprintf("%d:%d: even?: requires exactly 1 argument", line, col)}
	}
	n, ok := args[0].(*SchemeInt)
	if !ok {
		line, col := callExpr.Pos()
		return nil, &EvalError{Message: fmt.Sprintf("%d:%d: even?: expected number", line, col)}
	}
	return &SchemeBool{Value: n.Value%2 == 0}, nil
}

func builtinListRef(args []SchemeValue, callExpr *ListExpr) (SchemeValue, error) {
	if len(args) != 2 {
		line, col := callExpr.Pos()
		return nil, &EvalError{Message: fmt.Sprintf("%d:%d: list-ref: requires exactly 2 arguments", line, col)}
	}
	idx, ok := args[1].(*SchemeInt)
	if !ok {
		line, col := callExpr.Pos()
		return nil, &EvalError{Message: fmt.Sprintf("%d:%d: list-ref: expected number", line, col)}
	}
	cur := args[0]
	for i := int64(0); i < idx.Value; i++ {
		p, ok := cur.(*SchemePair)
		if !ok {
			line, col := callExpr.Pos()
			return nil, &EvalError{Message: fmt.Sprintf("%d:%d: list-ref: index out of range", line, col)}
		}
		cur = p.Cdr
	}
	p, ok := cur.(*SchemePair)
	if !ok {
		line, col := callExpr.Pos()
		return nil, &EvalError{Message: fmt.Sprintf("%d:%d: list-ref: index out of range", line, col)}
	}
	return p.Car, nil
}

func builtinListTail(args []SchemeValue, callExpr *ListExpr) (SchemeValue, error) {
	if len(args) != 2 {
		line, col := callExpr.Pos()
		return nil, &EvalError{Message: fmt.Sprintf("%d:%d: list-tail: requires exactly 2 arguments", line, col)}
	}
	idx, ok := args[1].(*SchemeInt)
	if !ok {
		line, col := callExpr.Pos()
		return nil, &EvalError{Message: fmt.Sprintf("%d:%d: list-tail: expected number", line, col)}
	}
	cur := args[0]
	for i := int64(0); i < idx.Value; i++ {
		p, ok := cur.(*SchemePair)
		if !ok {
			line, col := callExpr.Pos()
			return nil, &EvalError{Message: fmt.Sprintf("%d:%d: list-tail: index out of range", line, col)}
		}
		cur = p.Cdr
	}
	return cur, nil
}

func builtinListQ(args []SchemeValue, callExpr *ListExpr) (SchemeValue, error) {
	if len(args) != 1 {
		line, col := callExpr.Pos()
		return nil, &EvalError{Message: fmt.Sprintf("%d:%d: list?: requires exactly 1 argument", line, col)}
	}
	// Tortoise-and-hare cycle detection
	slow := args[0]
	fast := args[0]
	for {
		// Advance fast two steps
		fp, ok := fast.(*SchemePair)
		if !ok {
			_, isEmpty := fast.(*SchemeEmpty)
			return &SchemeBool{Value: isEmpty}, nil
		}
		fast = fp.Cdr
		fp2, ok := fast.(*SchemePair)
		if !ok {
			_, isEmpty := fast.(*SchemeEmpty)
			return &SchemeBool{Value: isEmpty}, nil
		}
		fast = fp2.Cdr
		// Advance slow one step
		slow = slow.(*SchemePair).Cdr
		// If they meet, there's a cycle
		if slow == fast {
			return &SchemeBool{Value: false}, nil
		}
	}
}

// schemeEqual implements deep structural equality (equal?).
func schemeEqual(a, b SchemeValue) bool {
	switch av := a.(type) {
	case *SchemeInt:
		bv, ok := b.(*SchemeInt)
		return ok && av.Value == bv.Value
	case *SchemeRational:
		bv, ok := b.(*SchemeRational)
		return ok && av.Num == bv.Num && av.Den == bv.Den
	case *SchemeFloat:
		bv, ok := b.(*SchemeFloat)
		return ok && av.Value == bv.Value
	case *SchemeBool:
		bv, ok := b.(*SchemeBool)
		return ok && av.Value == bv.Value
	case *SchemeString:
		bv, ok := b.(*SchemeString)
		return ok && av.Value == bv.Value
	case *SchemeSymbol:
		bv, ok := b.(*SchemeSymbol)
		return ok && av.Name == bv.Name
	case *SchemeChar:
		bv, ok := b.(*SchemeChar)
		return ok && av.Value == bv.Value
	case *SchemeEmpty:
		_, ok := b.(*SchemeEmpty)
		return ok
	case *SchemePair:
		bv, ok := b.(*SchemePair)
		if !ok {
			return false
		}
		return schemeEqual(av.Car, bv.Car) && schemeEqual(av.Cdr, bv.Cdr)
	case *SchemeVector:
		bv, ok := b.(*SchemeVector)
		if !ok {
			return false
		}
		if len(av.Elements) != len(bv.Elements) {
			return false
		}
		for i := range av.Elements {
			if !schemeEqual(av.Elements[i], bv.Elements[i]) {
				return false
			}
		}
		return true
	default:
		return a == b // pointer equality for everything else
	}
}

func builtinEqualQ(args []SchemeValue, callExpr *ListExpr) (SchemeValue, error) {
	if len(args) != 2 {
		line, col := callExpr.Pos()
		return nil, &EvalError{Message: fmt.Sprintf("%d:%d: equal?: requires exactly 2 arguments", line, col)}
	}
	return &SchemeBool{Value: schemeEqual(args[0], args[1])}, nil
}

func schemeEq(a, b SchemeValue) bool {
	switch av := a.(type) {
	case *SchemeSymbol:
		bv, ok := b.(*SchemeSymbol)
		return ok && av.Name == bv.Name
	case *SchemeBool:
		bv, ok := b.(*SchemeBool)
		return ok && av.Value == bv.Value
	case *SchemeInt:
		bv, ok := b.(*SchemeInt)
		return ok && av.Value == bv.Value
	case *SchemeChar:
		bv, ok := b.(*SchemeChar)
		return ok && av.Value == bv.Value
	case *SchemeEmpty:
		_, ok := b.(*SchemeEmpty)
		return ok
	default:
		return a == b
	}
}

func builtinEqQ(args []SchemeValue, callExpr *ListExpr) (SchemeValue, error) {
	if len(args) != 2 {
		line, col := callExpr.Pos()
		return nil, &EvalError{Message: fmt.Sprintf("%d:%d: eq?: requires exactly 2 arguments", line, col)}
	}
	return &SchemeBool{Value: schemeEq(args[0], args[1])}, nil
}

func builtinAssoc(args []SchemeValue, callExpr *ListExpr) (SchemeValue, error) {
	if len(args) != 2 {
		line, col := callExpr.Pos()
		return nil, &EvalError{Message: fmt.Sprintf("%d:%d: assoc: requires exactly 2 arguments", line, col)}
	}
	key := args[0]
	cur := args[1]
	for {
		switch c := cur.(type) {
		case *SchemePair:
			pair, ok := c.Car.(*SchemePair)
			if !ok {
				line, col := callExpr.Pos()
				return nil, &EvalError{Message: fmt.Sprintf("%d:%d: assoc: expected pair in alist", line, col)}
			}
			if schemeEqual(key, pair.Car) {
				return c.Car, nil
			}
			cur = c.Cdr
		case *SchemeEmpty:
			return &SchemeBool{Value: false}, nil
		default:
			line, col := callExpr.Pos()
			return nil, &EvalError{Message: fmt.Sprintf("%d:%d: assoc: expected list", line, col)}
		}
	}
}

// Character builtins

func builtinCharAlphabeticQ(args []SchemeValue, callExpr *ListExpr) (SchemeValue, error) {
	if len(args) != 1 {
		line, col := callExpr.Pos()
		return nil, &EvalError{Message: fmt.Sprintf("%d:%d: char-alphabetic?: requires exactly 1 argument", line, col)}
	}
	c, ok := args[0].(*SchemeChar)
	if !ok {
		line, col := callExpr.Pos()
		return nil, &EvalError{Message: fmt.Sprintf("%d:%d: char-alphabetic?: expected char", line, col)}
	}
	return &SchemeBool{Value: unicode.IsLetter(c.Value)}, nil
}

func builtinCharNumericQ(args []SchemeValue, callExpr *ListExpr) (SchemeValue, error) {
	if len(args) != 1 {
		line, col := callExpr.Pos()
		return nil, &EvalError{Message: fmt.Sprintf("%d:%d: char-numeric?: requires exactly 1 argument", line, col)}
	}
	c, ok := args[0].(*SchemeChar)
	if !ok {
		line, col := callExpr.Pos()
		return nil, &EvalError{Message: fmt.Sprintf("%d:%d: char-numeric?: expected char", line, col)}
	}
	return &SchemeBool{Value: unicode.IsDigit(c.Value)}, nil
}

func builtinCharUpcase(args []SchemeValue, callExpr *ListExpr) (SchemeValue, error) {
	if len(args) != 1 {
		line, col := callExpr.Pos()
		return nil, &EvalError{Message: fmt.Sprintf("%d:%d: char-upcase: requires exactly 1 argument", line, col)}
	}
	c, ok := args[0].(*SchemeChar)
	if !ok {
		line, col := callExpr.Pos()
		return nil, &EvalError{Message: fmt.Sprintf("%d:%d: char-upcase: expected char", line, col)}
	}
	return &SchemeChar{Value: unicode.ToUpper(c.Value)}, nil
}

func builtinCharDowncase(args []SchemeValue, callExpr *ListExpr) (SchemeValue, error) {
	if len(args) != 1 {
		line, col := callExpr.Pos()
		return nil, &EvalError{Message: fmt.Sprintf("%d:%d: char-downcase: requires exactly 1 argument", line, col)}
	}
	c, ok := args[0].(*SchemeChar)
	if !ok {
		line, col := callExpr.Pos()
		return nil, &EvalError{Message: fmt.Sprintf("%d:%d: char-downcase: expected char", line, col)}
	}
	return &SchemeChar{Value: unicode.ToLower(c.Value)}, nil
}

func builtinCharEqQ(args []SchemeValue, callExpr *ListExpr) (SchemeValue, error) {
	if len(args) != 2 {
		line, col := callExpr.Pos()
		return nil, &EvalError{Message: fmt.Sprintf("%d:%d: char=?: requires exactly 2 arguments", line, col)}
	}
	a, ok := args[0].(*SchemeChar)
	if !ok {
		line, col := callExpr.Pos()
		return nil, &EvalError{Message: fmt.Sprintf("%d:%d: char=?: expected char", line, col)}
	}
	b, ok := args[1].(*SchemeChar)
	if !ok {
		line, col := callExpr.Pos()
		return nil, &EvalError{Message: fmt.Sprintf("%d:%d: char=?: expected char", line, col)}
	}
	return &SchemeBool{Value: a.Value == b.Value}, nil
}

func builtinCharLtQ(args []SchemeValue, callExpr *ListExpr) (SchemeValue, error) {
	if len(args) != 2 {
		line, col := callExpr.Pos()
		return nil, &EvalError{Message: fmt.Sprintf("%d:%d: char<?: requires exactly 2 arguments", line, col)}
	}
	a, ok := args[0].(*SchemeChar)
	if !ok {
		line, col := callExpr.Pos()
		return nil, &EvalError{Message: fmt.Sprintf("%d:%d: char<?: expected char", line, col)}
	}
	b, ok := args[1].(*SchemeChar)
	if !ok {
		line, col := callExpr.Pos()
		return nil, &EvalError{Message: fmt.Sprintf("%d:%d: char<?: expected char", line, col)}
	}
	return &SchemeBool{Value: a.Value < b.Value}, nil
}

// String comparison builtins

func builtinStringEqQ(args []SchemeValue, callExpr *ListExpr) (SchemeValue, error) {
	if len(args) != 2 {
		line, col := callExpr.Pos()
		return nil, &EvalError{Message: fmt.Sprintf("%d:%d: string=?: requires exactly 2 arguments", line, col)}
	}
	a, ok := args[0].(*SchemeString)
	if !ok {
		line, col := callExpr.Pos()
		return nil, &EvalError{Message: fmt.Sprintf("%d:%d: string=?: expected string", line, col)}
	}
	b, ok := args[1].(*SchemeString)
	if !ok {
		line, col := callExpr.Pos()
		return nil, &EvalError{Message: fmt.Sprintf("%d:%d: string=?: expected string", line, col)}
	}
	return &SchemeBool{Value: a.Value == b.Value}, nil
}

func builtinStringLtQ(args []SchemeValue, callExpr *ListExpr) (SchemeValue, error) {
	if len(args) != 2 {
		line, col := callExpr.Pos()
		return nil, &EvalError{Message: fmt.Sprintf("%d:%d: string<?: requires exactly 2 arguments", line, col)}
	}
	a, ok := args[0].(*SchemeString)
	if !ok {
		line, col := callExpr.Pos()
		return nil, &EvalError{Message: fmt.Sprintf("%d:%d: string<?: expected string", line, col)}
	}
	b, ok := args[1].(*SchemeString)
	if !ok {
		line, col := callExpr.Pos()
		return nil, &EvalError{Message: fmt.Sprintf("%d:%d: string<?: expected string", line, col)}
	}
	return &SchemeBool{Value: a.Value < b.Value}, nil
}

func builtinStringCiEqQ(args []SchemeValue, callExpr *ListExpr) (SchemeValue, error) {
	if len(args) != 2 {
		line, col := callExpr.Pos()
		return nil, &EvalError{Message: fmt.Sprintf("%d:%d: string-ci=?: requires exactly 2 arguments", line, col)}
	}
	a, ok := args[0].(*SchemeString)
	if !ok {
		line, col := callExpr.Pos()
		return nil, &EvalError{Message: fmt.Sprintf("%d:%d: string-ci=?: expected string", line, col)}
	}
	b, ok := args[1].(*SchemeString)
	if !ok {
		line, col := callExpr.Pos()
		return nil, &EvalError{Message: fmt.Sprintf("%d:%d: string-ci=?: expected string", line, col)}
	}
	return &SchemeBool{Value: strings.EqualFold(a.Value, b.Value)}, nil
}

func builtinStringUpcase(args []SchemeValue, callExpr *ListExpr) (SchemeValue, error) {
	if len(args) != 1 {
		line, col := callExpr.Pos()
		return nil, &EvalError{Message: fmt.Sprintf("%d:%d: string-upcase: requires exactly 1 argument", line, col)}
	}
	s, ok := args[0].(*SchemeString)
	if !ok {
		line, col := callExpr.Pos()
		return nil, &EvalError{Message: fmt.Sprintf("%d:%d: string-upcase: expected string", line, col)}
	}
	return &SchemeString{Value: strings.ToUpper(s.Value)}, nil
}

func builtinStringDowncase(args []SchemeValue, callExpr *ListExpr) (SchemeValue, error) {
	if len(args) != 1 {
		line, col := callExpr.Pos()
		return nil, &EvalError{Message: fmt.Sprintf("%d:%d: string-downcase: requires exactly 1 argument", line, col)}
	}
	s, ok := args[0].(*SchemeString)
	if !ok {
		line, col := callExpr.Pos()
		return nil, &EvalError{Message: fmt.Sprintf("%d:%d: string-downcase: expected string", line, col)}
	}
	return &SchemeString{Value: strings.ToLower(s.Value)}, nil
}

// --- L15: letrec, letrec*, case, do, let*, vectors ---

// setupLetrec handles both letrec and letrec*.
// In letrec, all bindings see the same env (init with void, then set!).
// In letrec*, bindings are evaluated sequentially in the shared env.
func setupLetrec(e *ListExpr, env *Env, star bool) (Expr, *Env, error) {
	if len(e.Elements) < 3 {
		line, col := e.Pos()
		return nil, nil, &EvalError{Message: fmt.Sprintf("%d:%d: letrec: bad syntax", line, col)}
	}
	bindingsList, ok := e.Elements[1].(*ListExpr)
	if !ok {
		line, col := e.Pos()
		return nil, nil, &EvalError{Message: fmt.Sprintf("%d:%d: letrec: bad syntax", line, col)}
	}
	localEnv := NewEnv(env)
	// First pass: bind all names to void
	names := make([]string, len(bindingsList.Elements))
	initExprs := make([]Expr, len(bindingsList.Elements))
	for i, b := range bindingsList.Elements {
		pair, ok := b.(*ListExpr)
		if !ok || len(pair.Elements) != 2 {
			line, col := e.Pos()
			return nil, nil, &EvalError{Message: fmt.Sprintf("%d:%d: letrec: bad syntax", line, col)}
		}
		s, ok := pair.Elements[0].(*SymbolExpr)
		if !ok {
			line, col := e.Pos()
			return nil, nil, &EvalError{Message: fmt.Sprintf("%d:%d: letrec: bad syntax", line, col)}
		}
		names[i] = s.Name
		initExprs[i] = pair.Elements[1]
		localEnv.Set(s.Name, &SchemeVoid{})
	}
	// Second pass: evaluate init expressions
	if star {
		// letrec*: evaluate sequentially, each sees previous bindings
		for i, initExpr := range initExprs {
			val, err := Eval(initExpr, localEnv)
			if err != nil {
				return nil, nil, err
			}
			localEnv.Set(names[i], val)
		}
	} else {
		// letrec: evaluate all in the shared env, then bind
		vals := make([]SchemeValue, len(initExprs))
		for i, initExpr := range initExprs {
			val, err := Eval(initExpr, localEnv)
			if err != nil {
				return nil, nil, err
			}
			vals[i] = val
		}
		for i, name := range names {
			localEnv.Set(name, vals[i])
		}
	}
	body := e.Elements[2:]
	for _, bodyExpr := range body[:len(body)-1] {
		_, err := Eval(bodyExpr, localEnv)
		if err != nil {
			return nil, nil, err
		}
	}
	return body[len(body)-1], localEnv, nil
}

// setupLetStar handles (let* ((var init) ...) body...)
func setupLetStar(e *ListExpr, env *Env) (Expr, *Env, error) {
	if len(e.Elements) < 3 {
		line, col := e.Pos()
		return nil, nil, &EvalError{Message: fmt.Sprintf("%d:%d: let*: bad syntax", line, col)}
	}
	bindingsList, ok := e.Elements[1].(*ListExpr)
	if !ok {
		line, col := e.Pos()
		return nil, nil, &EvalError{Message: fmt.Sprintf("%d:%d: let*: bad syntax", line, col)}
	}
	localEnv := NewEnv(env)
	for _, b := range bindingsList.Elements {
		pair, ok := b.(*ListExpr)
		if !ok || len(pair.Elements) != 2 {
			line, col := e.Pos()
			return nil, nil, &EvalError{Message: fmt.Sprintf("%d:%d: let*: bad syntax", line, col)}
		}
		s, ok := pair.Elements[0].(*SymbolExpr)
		if !ok {
			line, col := e.Pos()
			return nil, nil, &EvalError{Message: fmt.Sprintf("%d:%d: let*: bad syntax", line, col)}
		}
		val, err := Eval(pair.Elements[1], localEnv)
		if err != nil {
			return nil, nil, err
		}
		localEnv.Set(s.Name, val)
	}
	body := e.Elements[2:]
	for _, bodyExpr := range body[:len(body)-1] {
		_, err := Eval(bodyExpr, localEnv)
		if err != nil {
			return nil, nil, err
		}
	}
	return body[len(body)-1], localEnv, nil
}

// evalCase handles (case key ((datum ...) expr ...) ... (else expr ...))
// Returns (tailExpr, done, result, error).
func evalCase(e *ListExpr, env *Env) (Expr, bool, SchemeValue, error) {
	if len(e.Elements) < 3 {
		line, col := e.Pos()
		return nil, false, nil, &EvalError{Message: fmt.Sprintf("%d:%d: case: bad syntax", line, col)}
	}
	key, err := Eval(e.Elements[1], env)
	if err != nil {
		return nil, false, nil, err
	}
	for _, clause := range e.Elements[2:] {
		cl, ok := clause.(*ListExpr)
		if !ok || len(cl.Elements) < 1 {
			line, col := e.Pos()
			return nil, false, nil, &EvalError{Message: fmt.Sprintf("%d:%d: case: bad syntax", line, col)}
		}
		// Check for else clause
		if sym, ok := cl.Elements[0].(*SymbolExpr); ok && sym.Name == "else" {
			body := cl.Elements[1:]
			if len(body) == 0 {
				return nil, true, &SchemeVoid{}, nil
			}
			for _, b := range body[:len(body)-1] {
				_, err := Eval(b, env)
				if err != nil {
					return nil, false, nil, err
				}
			}
			return body[len(body)-1], false, nil, nil
		}
		// Datum list: ((datum ...) expr ...)
		datums, ok := cl.Elements[0].(*ListExpr)
		if !ok {
			line, col := e.Pos()
			return nil, false, nil, &EvalError{Message: fmt.Sprintf("%d:%d: case: bad syntax", line, col)}
		}
		matched := false
		for _, d := range datums.Elements {
			dv := quoteExpr(d)
			if schemeEqv(key, dv) {
				matched = true
				break
			}
		}
		if matched {
			body := cl.Elements[1:]
			if len(body) == 0 {
				return nil, true, &SchemeVoid{}, nil
			}
			for _, b := range body[:len(body)-1] {
				_, err := Eval(b, env)
				if err != nil {
					return nil, false, nil, err
				}
			}
			return body[len(body)-1], false, nil, nil
		}
	}
	// No match, no else: return void
	return nil, true, &SchemeVoid{}, nil
}

// schemeEqv implements eqv? comparison.
func schemeEqv(a, b SchemeValue) bool {
	switch av := a.(type) {
	case *SchemeInt:
		bv, ok := b.(*SchemeInt)
		return ok && av.Value == bv.Value
	case *SchemeRational:
		bv, ok := b.(*SchemeRational)
		return ok && av.Num == bv.Num && av.Den == bv.Den
	case *SchemeFloat:
		bv, ok := b.(*SchemeFloat)
		return ok && av.Value == bv.Value
	case *SchemeBool:
		bv, ok := b.(*SchemeBool)
		return ok && av.Value == bv.Value
	case *SchemeChar:
		bv, ok := b.(*SchemeChar)
		return ok && av.Value == bv.Value
	case *SchemeSymbol:
		bv, ok := b.(*SchemeSymbol)
		return ok && av.Name == bv.Name
	case *SchemeEmpty:
		_, ok := b.(*SchemeEmpty)
		return ok
	default:
		return a == b
	}
}

// evalDo handles (do ((var init step) ...) (test expr ...) body ...)
func evalDo(e *ListExpr, env *Env) (Expr, *Env, error) {
	if len(e.Elements) < 3 {
		line, col := e.Pos()
		return nil, nil, &EvalError{Message: fmt.Sprintf("%d:%d: do: bad syntax", line, col)}
	}
	varList, ok := e.Elements[1].(*ListExpr)
	if !ok {
		line, col := e.Pos()
		return nil, nil, &EvalError{Message: fmt.Sprintf("%d:%d: do: bad syntax", line, col)}
	}
	testClause, ok := e.Elements[2].(*ListExpr)
	if !ok || len(testClause.Elements) < 1 {
		line, col := e.Pos()
		return nil, nil, &EvalError{Message: fmt.Sprintf("%d:%d: do: bad syntax", line, col)}
	}
	body := e.Elements[3:]

	// Parse variable specs
	type doVar struct {
		name    string
		step    Expr // nil if no step
		hasStep bool
	}
	vars := make([]doVar, len(varList.Elements))
	localEnv := NewEnv(env)

	for i, v := range varList.Elements {
		spec, ok := v.(*ListExpr)
		if !ok || len(spec.Elements) < 2 || len(spec.Elements) > 3 {
			line, col := e.Pos()
			return nil, nil, &EvalError{Message: fmt.Sprintf("%d:%d: do: bad variable spec", line, col)}
		}
		nameSym, ok := spec.Elements[0].(*SymbolExpr)
		if !ok {
			line, col := e.Pos()
			return nil, nil, &EvalError{Message: fmt.Sprintf("%d:%d: do: bad variable spec", line, col)}
		}
		initVal, err := Eval(spec.Elements[1], env)
		if err != nil {
			return nil, nil, err
		}
		vars[i].name = nameSym.Name
		if len(spec.Elements) == 3 {
			vars[i].step = spec.Elements[2]
			vars[i].hasStep = true
		}
		localEnv.Set(nameSym.Name, initVal)
	}

	// Iteration loop
	for {
		// Test
		testVal, err := Eval(testClause.Elements[0], localEnv)
		if err != nil {
			return nil, nil, err
		}
		if isTruthy(testVal) {
			// Test is true: evaluate expr... and return last
			exprs := testClause.Elements[1:]
			if len(exprs) == 0 {
				return nil, nil, nil // will be handled: return void
			}
			for _, ex := range exprs[:len(exprs)-1] {
				_, err := Eval(ex, localEnv)
				if err != nil {
					return nil, nil, err
				}
			}
			return exprs[len(exprs)-1], localEnv, nil
		}

		// Execute body
		for _, b := range body {
			_, err := Eval(b, localEnv)
			if err != nil {
				return nil, nil, err
			}
		}

		// Parallel step: evaluate all steps using current values, then update
		newVals := make([]SchemeValue, len(vars))
		for i, v := range vars {
			if v.hasStep {
				val, err := Eval(v.step, localEnv)
				if err != nil {
					return nil, nil, err
				}
				newVals[i] = val
			}
		}
		for i, v := range vars {
			if v.hasStep {
				localEnv.Set(v.name, newVals[i])
			}
		}
	}
}

// --- L15 vector builtins ---

func builtinVector(args []SchemeValue, callExpr *ListExpr) (SchemeValue, error) {
	elems := make([]SchemeValue, len(args))
	copy(elems, args)
	return &SchemeVector{Elements: elems}, nil
}

func builtinMakeVector(args []SchemeValue, callExpr *ListExpr) (SchemeValue, error) {
	if len(args) < 1 || len(args) > 2 {
		line, col := callExpr.Pos()
		return nil, &EvalError{Message: fmt.Sprintf("%d:%d: make-vector: requires 1-2 arguments", line, col)}
	}
	n, ok := args[0].(*SchemeInt)
	if !ok {
		line, col := callExpr.Pos()
		return nil, &EvalError{Message: fmt.Sprintf("%d:%d: make-vector: expected number", line, col)}
	}
	var fill SchemeValue = &SchemeInt{Value: 0}
	if len(args) == 2 {
		fill = args[1]
	}
	elems := make([]SchemeValue, n.Value)
	for i := range elems {
		elems[i] = fill
	}
	return &SchemeVector{Elements: elems}, nil
}

func builtinVectorRef(args []SchemeValue, callExpr *ListExpr) (SchemeValue, error) {
	if len(args) != 2 {
		line, col := callExpr.Pos()
		return nil, &EvalError{Message: fmt.Sprintf("%d:%d: vector-ref: requires exactly 2 arguments", line, col)}
	}
	v, ok := args[0].(*SchemeVector)
	if !ok {
		line, col := callExpr.Pos()
		return nil, &EvalError{Message: fmt.Sprintf("%d:%d: vector-ref: expected vector", line, col)}
	}
	idx, ok := args[1].(*SchemeInt)
	if !ok {
		line, col := callExpr.Pos()
		return nil, &EvalError{Message: fmt.Sprintf("%d:%d: vector-ref: expected number", line, col)}
	}
	i := idx.Value
	if i < 0 || i >= int64(len(v.Elements)) {
		line, col := callExpr.Pos()
		return nil, &EvalError{Message: fmt.Sprintf("%d:%d: vector-ref: index out of range", line, col)}
	}
	return v.Elements[i], nil
}

func builtinVectorSet(args []SchemeValue, callExpr *ListExpr) (SchemeValue, error) {
	if len(args) != 3 {
		line, col := callExpr.Pos()
		return nil, &EvalError{Message: fmt.Sprintf("%d:%d: vector-set!: requires exactly 3 arguments", line, col)}
	}
	v, ok := args[0].(*SchemeVector)
	if !ok {
		line, col := callExpr.Pos()
		return nil, &EvalError{Message: fmt.Sprintf("%d:%d: vector-set!: expected vector", line, col)}
	}
	idx, ok := args[1].(*SchemeInt)
	if !ok {
		line, col := callExpr.Pos()
		return nil, &EvalError{Message: fmt.Sprintf("%d:%d: vector-set!: expected number", line, col)}
	}
	i := idx.Value
	if i < 0 || i >= int64(len(v.Elements)) {
		line, col := callExpr.Pos()
		return nil, &EvalError{Message: fmt.Sprintf("%d:%d: vector-set!: index out of range", line, col)}
	}
	v.Elements[i] = args[2]
	return &SchemeVoid{}, nil
}

func builtinVectorLength(args []SchemeValue, callExpr *ListExpr) (SchemeValue, error) {
	if len(args) != 1 {
		line, col := callExpr.Pos()
		return nil, &EvalError{Message: fmt.Sprintf("%d:%d: vector-length: requires exactly 1 argument", line, col)}
	}
	v, ok := args[0].(*SchemeVector)
	if !ok {
		line, col := callExpr.Pos()
		return nil, &EvalError{Message: fmt.Sprintf("%d:%d: vector-length: expected vector", line, col)}
	}
	return &SchemeInt{Value: int64(len(v.Elements))}, nil
}

func builtinVectorQ(args []SchemeValue, callExpr *ListExpr) (SchemeValue, error) {
	if len(args) != 1 {
		line, col := callExpr.Pos()
		return nil, &EvalError{Message: fmt.Sprintf("%d:%d: vector?: requires exactly 1 argument", line, col)}
	}
	_, ok := args[0].(*SchemeVector)
	return &SchemeBool{Value: ok}, nil
}

func builtinVectorToList(args []SchemeValue, callExpr *ListExpr) (SchemeValue, error) {
	if len(args) != 1 {
		line, col := callExpr.Pos()
		return nil, &EvalError{Message: fmt.Sprintf("%d:%d: vector->list: requires exactly 1 argument", line, col)}
	}
	v, ok := args[0].(*SchemeVector)
	if !ok {
		line, col := callExpr.Pos()
		return nil, &EvalError{Message: fmt.Sprintf("%d:%d: vector->list: expected vector", line, col)}
	}
	return sliceToList(v.Elements), nil
}

func builtinListToVector(args []SchemeValue, callExpr *ListExpr) (SchemeValue, error) {
	if len(args) != 1 {
		line, col := callExpr.Pos()
		return nil, &EvalError{Message: fmt.Sprintf("%d:%d: list->vector: requires exactly 1 argument", line, col)}
	}
	elems, ok := listToSlice(args[0])
	if !ok {
		line, col := callExpr.Pos()
		return nil, &EvalError{Message: fmt.Sprintf("%d:%d: list->vector: expected list", line, col)}
	}
	result := make([]SchemeValue, len(elems))
	copy(result, elems)
	return &SchemeVector{Elements: result}, nil
}

func builtinEqvQ(args []SchemeValue, callExpr *ListExpr) (SchemeValue, error) {
	if len(args) != 2 {
		line, col := callExpr.Pos()
		return nil, &EvalError{Message: fmt.Sprintf("%d:%d: eqv?: requires exactly 2 arguments", line, col)}
	}
	return &SchemeBool{Value: schemeEqv(args[0], args[1])}, nil
}

func builtinError(args []SchemeValue, callExpr *ListExpr) (SchemeValue, error) {
	line, col := callExpr.Pos()
	var msg strings.Builder
	msg.WriteString("error")
	for _, a := range args {
		msg.WriteString(" ")
		msg.WriteString(displayValue(a))
	}
	return nil, &EvalError{Message: fmt.Sprintf("%d:%d: %s", line, col, msg.String())}
}

func builtinWithExceptionHandler(args []SchemeValue, callExpr *ListExpr, env *Env) (SchemeValue, error) {
	if len(args) != 2 {
		line, col := callExpr.Pos()
		return nil, &EvalError{Message: fmt.Sprintf("%d:%d: with-exception-handler: requires exactly 2 arguments", line, col)}
	}
	handler, thunk := args[0], args[1]
	result, err := applyFunc(thunk, []SchemeValue{}, callExpr, env)
	if err != nil {
		if raiseErr, ok := err.(*schemeRaiseError); ok {
			return applyFunc(handler, []SchemeValue{raiseErr.value}, callExpr, env)
		}
		return nil, err
	}
	return result, nil
}

func builtinValues(args []SchemeValue, callExpr *ListExpr) (SchemeValue, error) {
	if len(args) == 1 {
		return args[0], nil
	}
	return &SchemeMultipleValues{Values: args}, nil
}

func builtinCallWithValues(args []SchemeValue, callExpr *ListExpr, env *Env) (SchemeValue, error) {
	if len(args) != 2 {
		line, col := callExpr.Pos()
		return nil, &EvalError{Message: fmt.Sprintf("%d:%d: call-with-values: requires exactly 2 arguments", line, col)}
	}
	producer, consumer := args[0], args[1]

	// Call producer with no arguments
	result, err := applyFunc(producer, []SchemeValue{}, callExpr, env)
	if err != nil {
		return nil, err
	}

	// Unpack multiple values into consumer args
	var consumerArgs []SchemeValue
	if mv, ok := result.(*SchemeMultipleValues); ok {
		consumerArgs = mv.Values
	} else {
		consumerArgs = []SchemeValue{result}
	}

	return applyFunc(consumer, consumerArgs, callExpr, env)
}

// evalGuard implements (guard (var clause...) body...)
func evalGuard(e *ListExpr, env *Env) (SchemeValue, error) {
	if len(e.Elements) < 3 {
		line, col := e.Pos()
		return nil, &EvalError{Message: fmt.Sprintf("%d:%d: guard: bad syntax", line, col)}
	}

	// Parse the clauses: (var clause1 clause2 ...)
	clauseList, ok := e.Elements[1].(*ListExpr)
	if !ok || len(clauseList.Elements) < 1 {
		line, col := e.Pos()
		return nil, &EvalError{Message: fmt.Sprintf("%d:%d: guard: bad syntax", line, col)}
	}

	varSym, ok := clauseList.Elements[0].(*SymbolExpr)
	if !ok {
		line, col := e.Pos()
		return nil, &EvalError{Message: fmt.Sprintf("%d:%d: guard: bad syntax", line, col)}
	}

	clauses := clauseList.Elements[1:]
	body := e.Elements[2:]

	// Evaluate the body expressions
	var result SchemeValue
	var bodyErr error
	for _, b := range body {
		result, bodyErr = Eval(b, env)
		if bodyErr != nil {
			break
		}
	}

	// If no exception, return the body result
	if bodyErr == nil {
		return result, nil
	}

	// Check if it's a raise error
	raiseErr, isRaise := bodyErr.(*schemeRaiseError)
	if !isRaise {
		return nil, bodyErr
	}

	// Bind the exception value to the variable and test clauses
	guardEnv := NewEnv(env)
	guardEnv.Set(varSym.Name, raiseErr.value)

	for _, clause := range clauses {
		cl, ok := clause.(*ListExpr)
		if !ok || len(cl.Elements) == 0 {
			line, col := e.Pos()
			return nil, &EvalError{Message: fmt.Sprintf("%d:%d: guard: bad clause", line, col)}
		}

		// Check for else clause
		if sym, ok := cl.Elements[0].(*SymbolExpr); ok && sym.Name == "else" {
			// Evaluate else body
			var res SchemeValue
			for _, expr := range cl.Elements[1:] {
				var err error
				res, err = Eval(expr, guardEnv)
				if err != nil {
					return nil, err
				}
			}
			return res, nil
		}

		// Evaluate the test
		testResult, err := Eval(cl.Elements[0], guardEnv)
		if err != nil {
			return nil, err
		}
		if isTruthy(testResult) {
			// Evaluate the clause body
			if len(cl.Elements) == 1 {
				return testResult, nil
			}
			var res SchemeValue
			for _, expr := range cl.Elements[1:] {
				res, err = Eval(expr, guardEnv)
				if err != nil {
					return nil, err
				}
			}
			return res, nil
		}
	}

	// No clause matched, re-raise
	return nil, raiseErr
}

// evalDefineRecordType implements (define-record-type <name> (constructor field ...) predicate (field accessor) ...)
func evalDefineRecordType(e *ListExpr, env *Env) (SchemeValue, error) {
	// (define-record-type <name> (constructor field...) predicate (field accessor) ...)
	// Minimum: type-name, constructor, predicate, at least 0 field specs
	if len(e.Elements) < 4 {
		line, col := e.Pos()
		return nil, &EvalError{Message: fmt.Sprintf("%d:%d: define-record-type: bad syntax", line, col)}
	}

	// 1. Type name (ignored for runtime, just a tag)
	typeSym, ok := e.Elements[1].(*SymbolExpr)
	if !ok {
		line, col := e.Elements[1].Pos()
		return nil, &EvalError{Message: fmt.Sprintf("%d:%d: define-record-type: expected type name", line, col)}
	}

	// 2. Constructor clause: (constructor-name field ...)
	ctorList, ok := e.Elements[2].(*ListExpr)
	if !ok || len(ctorList.Elements) < 1 {
		line, col := e.Elements[2].Pos()
		return nil, &EvalError{Message: fmt.Sprintf("%d:%d: define-record-type: bad constructor clause", line, col)}
	}
	ctorName, ok := ctorList.Elements[0].(*SymbolExpr)
	if !ok {
		line, col := ctorList.Elements[0].Pos()
		return nil, &EvalError{Message: fmt.Sprintf("%d:%d: define-record-type: constructor name must be a symbol", line, col)}
	}
	ctorFields := make([]string, len(ctorList.Elements)-1)
	for i, fe := range ctorList.Elements[1:] {
		fs, ok := fe.(*SymbolExpr)
		if !ok {
			line, col := fe.Pos()
			return nil, &EvalError{Message: fmt.Sprintf("%d:%d: define-record-type: constructor field must be a symbol", line, col)}
		}
		ctorFields[i] = fs.Name
	}

	// 3. Predicate name
	predSym, ok := e.Elements[3].(*SymbolExpr)
	if !ok {
		line, col := e.Elements[3].Pos()
		return nil, &EvalError{Message: fmt.Sprintf("%d:%d: define-record-type: predicate must be a symbol", line, col)}
	}

	// 4. Field specs: (field-name accessor-name)
	type fieldSpec struct {
		name     string
		accessor string
	}
	var fields []fieldSpec
	for _, fe := range e.Elements[4:] {
		fl, ok := fe.(*ListExpr)
		if !ok || len(fl.Elements) < 2 {
			line, col := fe.Pos()
			return nil, &EvalError{Message: fmt.Sprintf("%d:%d: define-record-type: bad field spec", line, col)}
		}
		fname, ok := fl.Elements[0].(*SymbolExpr)
		if !ok {
			line, col := fl.Elements[0].Pos()
			return nil, &EvalError{Message: fmt.Sprintf("%d:%d: define-record-type: field name must be a symbol", line, col)}
		}
		facc, ok := fl.Elements[1].(*SymbolExpr)
		if !ok {
			line, col := fl.Elements[1].Pos()
			return nil, &EvalError{Message: fmt.Sprintf("%d:%d: define-record-type: accessor name must be a symbol", line, col)}
		}
		fields = append(fields, fieldSpec{name: fname.Name, accessor: facc.Name})
	}

	// Build field index map from constructor field order
	fieldIndex := make(map[string]int)
	for i, f := range ctorFields {
		fieldIndex[f] = i
	}

	// Create runtime record type descriptor
	rt := &SchemeRecordType{Name: typeSym.Name, FieldNames: ctorFields}

	// Define constructor
	nFields := len(ctorFields)
	env.Set(ctorName.Name, &BuiltinProc{Name: ctorName.Name, Fn: func(args []SchemeValue, callExpr *ListExpr) (SchemeValue, error) {
		if len(args) != nFields {
			line, col := callExpr.Pos()
			return nil, &EvalError{Message: fmt.Sprintf("%d:%d: %s: expected %d arguments, got %d", line, col, ctorName.Name, nFields, len(args))}
		}
		rec := &SchemeRecord{TypeID: rt, Fields: make([]SchemeValue, nFields)}
		copy(rec.Fields, args)
		return rec, nil
	}})

	// Define predicate
	env.Set(predSym.Name, &BuiltinProc{Name: predSym.Name, Fn: func(args []SchemeValue, callExpr *ListExpr) (SchemeValue, error) {
		if len(args) != 1 {
			line, col := callExpr.Pos()
			return nil, &EvalError{Message: fmt.Sprintf("%d:%d: %s: expected 1 argument", line, col, predSym.Name)}
		}
		rec, ok := args[0].(*SchemeRecord)
		return &SchemeBool{Value: ok && rec.TypeID == rt}, nil
	}})

	// Define accessors
	for _, fs := range fields {
		idx, ok := fieldIndex[fs.name]
		if !ok {
			line, col := e.Pos()
			return nil, &EvalError{Message: fmt.Sprintf("%d:%d: define-record-type: field %s not in constructor", line, col, fs.name)}
		}
		accName := fs.accessor
		fieldIdx := idx
		env.Set(accName, &BuiltinProc{Name: accName, Fn: func(args []SchemeValue, callExpr *ListExpr) (SchemeValue, error) {
			if len(args) != 1 {
				line, col := callExpr.Pos()
				return nil, &EvalError{Message: fmt.Sprintf("%d:%d: %s: expected 1 argument", line, col, accName)}
			}
			rec, ok := args[0].(*SchemeRecord)
			if !ok || rec.TypeID != rt {
				line, col := callExpr.Pos()
				return nil, &EvalError{Message: fmt.Sprintf("%d:%d: %s: not a %s record", line, col, accName, rt.Name)}
			}
			return rec.Fields[fieldIdx], nil
		}})
	}

	return &SchemeVoid{}, nil
}

// --- L21: cxr helpers ---

func makeCxr(name string) func([]SchemeValue, *ListExpr) (SchemeValue, error) {
	// Parse the a/d pattern from the name (e.g., "cadr" -> "ad")
	ops := name[1 : len(name)-1] // strip leading 'c' and trailing 'r'
	return func(args []SchemeValue, callExpr *ListExpr) (SchemeValue, error) {
		if len(args) != 1 {
			line, col := callExpr.Pos()
			return nil, &EvalError{Message: fmt.Sprintf("%d:%d: %s: requires exactly 1 argument", line, col, name)}
		}
		v := args[0]
		// Apply ops right-to-left
		for i := len(ops) - 1; i >= 0; i-- {
			p, ok := v.(*SchemePair)
			if !ok {
				line, col := callExpr.Pos()
				return nil, &EvalError{Message: fmt.Sprintf("%d:%d: %s: expected pair", line, col, name)}
			}
			if ops[i] == 'a' {
				v = p.Car
			} else {
				v = p.Cdr
			}
		}
		return v, nil
	}
}

// --- L21: set-car!, set-cdr! ---

func builtinSetCar(args []SchemeValue, callExpr *ListExpr) (SchemeValue, error) {
	if len(args) != 2 {
		line, col := callExpr.Pos()
		return nil, &EvalError{Message: fmt.Sprintf("%d:%d: set-car!: requires exactly 2 arguments", line, col)}
	}
	p, ok := args[0].(*SchemePair)
	if !ok {
		line, col := callExpr.Pos()
		return nil, &EvalError{Message: fmt.Sprintf("%d:%d: set-car!: expected pair", line, col)}
	}
	p.Car = args[1]
	return &SchemeVoid{}, nil
}

func builtinSetCdr(args []SchemeValue, callExpr *ListExpr) (SchemeValue, error) {
	if len(args) != 2 {
		line, col := callExpr.Pos()
		return nil, &EvalError{Message: fmt.Sprintf("%d:%d: set-cdr!: requires exactly 2 arguments", line, col)}
	}
	p, ok := args[0].(*SchemePair)
	if !ok {
		line, col := callExpr.Pos()
		return nil, &EvalError{Message: fmt.Sprintf("%d:%d: set-cdr!: expected pair", line, col)}
	}
	p.Cdr = args[1]
	return &SchemeVoid{}, nil
}

// --- L21: for-each ---

func builtinForEachEnv(args []SchemeValue, callExpr *ListExpr, env *Env) (SchemeValue, error) {
	if len(args) < 2 {
		line, col := callExpr.Pos()
		return nil, &EvalError{Message: fmt.Sprintf("%d:%d: for-each: requires at least 2 arguments", line, col)}
	}
	fn := args[0]
	lists := make([][]SchemeValue, len(args)-1)
	for i := 1; i < len(args); i++ {
		elems, ok := listToSlice(args[i])
		if !ok {
			line, col := callExpr.Pos()
			return nil, &EvalError{Message: fmt.Sprintf("%d:%d: for-each: expected list", line, col)}
		}
		lists[i-1] = elems
	}
	if len(lists) == 0 {
		return &SchemeVoid{}, nil
	}
	n := len(lists[0])
	for _, l := range lists[1:] {
		if len(l) != n {
			line, col := callExpr.Pos()
			return nil, &EvalError{Message: fmt.Sprintf("%d:%d: for-each: lists must have same length", line, col)}
		}
	}
	for i := 0; i < n; i++ {
		fnArgs := make([]SchemeValue, len(lists))
		for j := range lists {
			fnArgs[j] = lists[j][i]
		}
		_, err := applyFunc(fn, fnArgs, callExpr, env)
		if err != nil {
			return nil, err
		}
	}
	return &SchemeVoid{}, nil
}

// --- L21: procedure? ---

func builtinProcedureQ(args []SchemeValue, callExpr *ListExpr) (SchemeValue, error) {
	if len(args) != 1 {
		line, col := callExpr.Pos()
		return nil, &EvalError{Message: fmt.Sprintf("%d:%d: procedure?: requires exactly 1 argument", line, col)}
	}
	switch args[0].(type) {
	case *Lambda, *BuiltinProc, *EnvBuiltinProc, *SchemeCallCC, *SchemeDynamicWind, *SchemeCaseLambda, *SchemeContinuation:
		return &SchemeBool{Value: true}, nil
	default:
		return &SchemeBool{Value: false}, nil
	}
}

// --- L21: gcd, lcm ---

func intGCD(a, b int64) int64 {
	if a < 0 {
		a = -a
	}
	if b < 0 {
		b = -b
	}
	for b != 0 {
		a, b = b, a%b
	}
	return a
}

func builtinGCD(args []SchemeValue, callExpr *ListExpr) (SchemeValue, error) {
	if len(args) == 0 {
		return &SchemeInt{Value: 0}, nil
	}
	result := toInt64(args[0])
	for _, a := range args[1:] {
		result = intGCD(result, toInt64(a))
	}
	if result < 0 {
		result = -result
	}
	return &SchemeInt{Value: result}, nil
}

func builtinLCM(args []SchemeValue, callExpr *ListExpr) (SchemeValue, error) {
	if len(args) == 0 {
		return &SchemeInt{Value: 1}, nil
	}
	result := toInt64(args[0])
	if result < 0 {
		result = -result
	}
	for _, a := range args[1:] {
		b := toInt64(a)
		if b < 0 {
			b = -b
		}
		if result == 0 || b == 0 {
			result = 0
		} else {
			result = result / intGCD(result, b) * b
		}
	}
	return &SchemeInt{Value: result}, nil
}

func toInt64(v SchemeValue) int64 {
	switch x := v.(type) {
	case *SchemeInt:
		return x.Value
	case *SchemeFloat:
		return int64(x.Value)
	case *SchemeRational:
		return x.Num / x.Den
	}
	return 0
}

// --- L21: truncate, round ---

func builtinTruncate(args []SchemeValue, callExpr *ListExpr) (SchemeValue, error) {
	if len(args) != 1 {
		line, col := callExpr.Pos()
		return nil, &EvalError{Message: fmt.Sprintf("%d:%d: truncate: requires exactly 1 argument", line, col)}
	}
	switch v := args[0].(type) {
	case *SchemeInt:
		return v, nil
	case *SchemeFloat:
		return &SchemeFloat{Value: math.Trunc(v.Value)}, nil
	case *SchemeRational:
		return &SchemeInt{Value: v.Num / v.Den}, nil
	default:
		line, col := callExpr.Pos()
		return nil, &EvalError{Message: fmt.Sprintf("%d:%d: truncate: expected number", line, col)}
	}
}

func builtinRound(args []SchemeValue, callExpr *ListExpr) (SchemeValue, error) {
	if len(args) != 1 {
		line, col := callExpr.Pos()
		return nil, &EvalError{Message: fmt.Sprintf("%d:%d: round: requires exactly 1 argument", line, col)}
	}
	switch v := args[0].(type) {
	case *SchemeInt:
		return v, nil
	case *SchemeFloat:
		return &SchemeFloat{Value: math.RoundToEven(v.Value)}, nil
	case *SchemeRational:
		return &SchemeInt{Value: int64(math.RoundToEven(float64(v.Num) / float64(v.Den)))}, nil
	default:
		line, col := callExpr.Pos()
		return nil, &EvalError{Message: fmt.Sprintf("%d:%d: round: expected number", line, col)}
	}
}

// --- L21: make-string, string (constructor) ---

func builtinMakeString(args []SchemeValue, callExpr *ListExpr) (SchemeValue, error) {
	if len(args) < 1 || len(args) > 2 {
		line, col := callExpr.Pos()
		return nil, &EvalError{Message: fmt.Sprintf("%d:%d: make-string: requires 1 or 2 arguments", line, col)}
	}
	n, ok := args[0].(*SchemeInt)
	if !ok {
		line, col := callExpr.Pos()
		return nil, &EvalError{Message: fmt.Sprintf("%d:%d: make-string: expected integer", line, col)}
	}
	ch := ' '
	if len(args) == 2 {
		c, ok := args[1].(*SchemeChar)
		if !ok {
			line, col := callExpr.Pos()
			return nil, &EvalError{Message: fmt.Sprintf("%d:%d: make-string: expected char", line, col)}
		}
		ch = c.Value
	}
	return &SchemeString{Value: strings.Repeat(string(ch), int(n.Value)), Mutable: true}, nil
}

func builtinStringConstructor(args []SchemeValue, callExpr *ListExpr) (SchemeValue, error) {
	var sb strings.Builder
	for _, a := range args {
		c, ok := a.(*SchemeChar)
		if !ok {
			line, col := callExpr.Pos()
			return nil, &EvalError{Message: fmt.Sprintf("%d:%d: string: expected char", line, col)}
		}
		sb.WriteRune(c.Value)
	}
	return &SchemeString{Value: sb.String(), Mutable: true}, nil
}

// --- L21: string>?, string<=?, string>=? ---

func builtinStringGtQ(args []SchemeValue, callExpr *ListExpr) (SchemeValue, error) {
	if len(args) != 2 {
		line, col := callExpr.Pos()
		return nil, &EvalError{Message: fmt.Sprintf("%d:%d: string>?: requires exactly 2 arguments", line, col)}
	}
	a, ok1 := args[0].(*SchemeString)
	b, ok2 := args[1].(*SchemeString)
	if !ok1 || !ok2 {
		line, col := callExpr.Pos()
		return nil, &EvalError{Message: fmt.Sprintf("%d:%d: string>?: expected strings", line, col)}
	}
	return &SchemeBool{Value: a.Value > b.Value}, nil
}

func builtinStringLeQ(args []SchemeValue, callExpr *ListExpr) (SchemeValue, error) {
	if len(args) != 2 {
		line, col := callExpr.Pos()
		return nil, &EvalError{Message: fmt.Sprintf("%d:%d: string<=?: requires exactly 2 arguments", line, col)}
	}
	a, ok1 := args[0].(*SchemeString)
	b, ok2 := args[1].(*SchemeString)
	if !ok1 || !ok2 {
		line, col := callExpr.Pos()
		return nil, &EvalError{Message: fmt.Sprintf("%d:%d: string<=?: expected strings", line, col)}
	}
	return &SchemeBool{Value: a.Value <= b.Value}, nil
}

func builtinStringGeQ(args []SchemeValue, callExpr *ListExpr) (SchemeValue, error) {
	if len(args) != 2 {
		line, col := callExpr.Pos()
		return nil, &EvalError{Message: fmt.Sprintf("%d:%d: string>=?: requires exactly 2 arguments", line, col)}
	}
	a, ok1 := args[0].(*SchemeString)
	b, ok2 := args[1].(*SchemeString)
	if !ok1 || !ok2 {
		line, col := callExpr.Pos()
		return nil, &EvalError{Message: fmt.Sprintf("%d:%d: string>=?: expected strings", line, col)}
	}
	return &SchemeBool{Value: a.Value >= b.Value}, nil
}

// --- L21: memq, memv, member ---

func builtinMemq(args []SchemeValue, callExpr *ListExpr) (SchemeValue, error) {
	if len(args) != 2 {
		line, col := callExpr.Pos()
		return nil, &EvalError{Message: fmt.Sprintf("%d:%d: memq: requires exactly 2 arguments", line, col)}
	}
	obj := args[0]
	cur := args[1]
	for {
		switch c := cur.(type) {
		case *SchemePair:
			if schemeEq(obj, c.Car) {
				return c, nil
			}
			cur = c.Cdr
		case *SchemeEmpty:
			return &SchemeBool{Value: false}, nil
		default:
			return &SchemeBool{Value: false}, nil
		}
	}
}

func builtinMemv(args []SchemeValue, callExpr *ListExpr) (SchemeValue, error) {
	if len(args) != 2 {
		line, col := callExpr.Pos()
		return nil, &EvalError{Message: fmt.Sprintf("%d:%d: memv: requires exactly 2 arguments", line, col)}
	}
	obj := args[0]
	cur := args[1]
	for {
		switch c := cur.(type) {
		case *SchemePair:
			if schemeEqv(obj, c.Car) {
				return c, nil
			}
			cur = c.Cdr
		case *SchemeEmpty:
			return &SchemeBool{Value: false}, nil
		default:
			return &SchemeBool{Value: false}, nil
		}
	}
}

func builtinMember(args []SchemeValue, callExpr *ListExpr) (SchemeValue, error) {
	if len(args) != 2 {
		line, col := callExpr.Pos()
		return nil, &EvalError{Message: fmt.Sprintf("%d:%d: member: requires exactly 2 arguments", line, col)}
	}
	obj := args[0]
	cur := args[1]
	for {
		switch c := cur.(type) {
		case *SchemePair:
			if schemeEqual(obj, c.Car) {
				return c, nil
			}
			cur = c.Cdr
		case *SchemeEmpty:
			return &SchemeBool{Value: false}, nil
		default:
			return &SchemeBool{Value: false}, nil
		}
	}
}

// --- L21: assq, assv ---

func builtinAssq(args []SchemeValue, callExpr *ListExpr) (SchemeValue, error) {
	if len(args) != 2 {
		line, col := callExpr.Pos()
		return nil, &EvalError{Message: fmt.Sprintf("%d:%d: assq: requires exactly 2 arguments", line, col)}
	}
	key := args[0]
	cur := args[1]
	for {
		switch c := cur.(type) {
		case *SchemePair:
			pair, ok := c.Car.(*SchemePair)
			if ok && schemeEq(key, pair.Car) {
				return pair, nil
			}
			cur = c.Cdr
		case *SchemeEmpty:
			return &SchemeBool{Value: false}, nil
		default:
			return &SchemeBool{Value: false}, nil
		}
	}
}

func builtinAssv(args []SchemeValue, callExpr *ListExpr) (SchemeValue, error) {
	if len(args) != 2 {
		line, col := callExpr.Pos()
		return nil, &EvalError{Message: fmt.Sprintf("%d:%d: assv: requires exactly 2 arguments", line, col)}
	}
	key := args[0]
	cur := args[1]
	for {
		switch c := cur.(type) {
		case *SchemePair:
			pair, ok := c.Car.(*SchemePair)
			if ok && schemeEqv(key, pair.Car) {
				return pair, nil
			}
			cur = c.Cdr
		case *SchemeEmpty:
			return &SchemeBool{Value: false}, nil
		default:
			return &SchemeBool{Value: false}, nil
		}
	}
}
