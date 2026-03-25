package ming

import "fmt"

// ===================== CEK Machine Continuation Frames =====================

// WindEntry represents one level of dynamic-wind nesting.
type WindEntry struct {
	InThunk  *Value
	OutThunk *Value
	Next     *WindEntry
}

type KTag int

const (
	KHalt           KTag = iota
	KTopLevel
	KBody
	KDefine
	KSetBang
	KIfTest
	KEvFun
	KEvArgs
	KAnd
	KOr
	KCondClauses
	KLetBind
	KLetStarBind
	KLetrecBind
	KLetrecStarBind
	KCaseKey
	KDynWindIn
	KDynWindBody
	KDynWindOut
	KDynWindTransition
	KDynWindRestore
	KExcHandler
	KGuardHandler
	KRaiseCallHandler
	KRaiseGuardEval
	KGuardTest
)

type KontFrame struct {
	Tag  KTag
	Next *KontFrame

	Env        *Env
	Exprs      []*Expr
	Name       string
	Names      []string
	Vals       []*Value
	Fn         *Value
	Conseq     *Expr
	Alt        *Expr
	Line, Col  int
	OrigExpr   *Expr
	BodyExprs  []*Expr
	LetEnv     *Env
	NamedLet   string
	Clauses    []*Expr
	ClauseBody []*Expr
	Result     *Value
	Wind       *WindEntry
}

// ===================== Lambda/CaseLambda Binding =====================

func bindLambdaEnv(fn *Value, args []*Value, line, col int) (*Env, error) {
	if fn.RestParam != "" {
		if len(args) < len(fn.Params) {
			return nil, fmt.Errorf("%d:%d: wrong number of arguments: expected at least %d, got %d", line, col, len(fn.Params), len(args))
		}
		callEnv := NewEnv(fn.ClosureEnv)
		for i, param := range fn.Params {
			callEnv.Set(param, args[i])
		}
		rest := Null
		for i := len(args) - 1; i >= len(fn.Params); i-- {
			rest = PairValue(args[i], rest)
		}
		callEnv.Set(fn.RestParam, rest)
		return callEnv, nil
	}
	if len(args) != len(fn.Params) {
		return nil, fmt.Errorf("%d:%d: wrong number of arguments: expected %d, got %d", line, col, len(fn.Params), len(args))
	}
	callEnv := NewEnv(fn.ClosureEnv)
	for i, param := range fn.Params {
		callEnv.Set(param, args[i])
	}
	return callEnv, nil
}

func bindCaseLambdaEnv(fn *Value, args []*Value, line, col int) (*Env, []*Expr, error) {
	for _, clause := range fn.CaseClauses {
		if clause.RestParam != "" {
			if len(args) >= len(clause.Params) {
				callEnv := NewEnv(fn.ClosureEnv)
				for i, param := range clause.Params {
					callEnv.Set(param, args[i])
				}
				rest := Null
				for i := len(args) - 1; i >= len(clause.Params); i-- {
					rest = PairValue(args[i], rest)
				}
				callEnv.Set(clause.RestParam, rest)
				return callEnv, clause.Body, nil
			}
		} else if len(args) == len(clause.Params) {
			callEnv := NewEnv(fn.ClosureEnv)
			for i, param := range clause.Params {
				callEnv.Set(param, args[i])
			}
			return callEnv, clause.Body, nil
		}
	}
	return nil, nil, fmt.Errorf("%d:%d: no matching clause in case-lambda for %d arguments", line, col, len(args))
}

// ===================== Entry Points =====================

func Eval(expr *Expr, env *Env) (*Value, error) {
	return cekEval(expr, env, &KontFrame{Tag: KHalt})
}

func cekEvalAll(exprs []*Expr, env *Env) (*Value, error) {
	if len(exprs) == 0 {
		return Void, nil
	}
	kont := &KontFrame{Tag: KHalt}
	if len(exprs) > 1 {
		kont = &KontFrame{Tag: KTopLevel, Exprs: exprs[1:], Env: env, Next: &KontFrame{Tag: KHalt}}
	}
	return cekEval(exprs[0], env, kont)
}

// ===================== Main CEK Loop =====================

func cekEval(startExpr *Expr, startEnv *Env, startKont *KontFrame) (*Value, error) {
	expr := startExpr
	env := startEnv
	kont := startKont
	var val *Value
	var windStack *WindEntry
	evaluating := true

	for {
		if evaluating {
			// ==================== EVAL MODE ====================
			switch expr.Type {
			case ExprInteger:
				val = IntValue(expr.IntVal)
				evaluating = false
			case ExprBoolean:
				val = BoolValue(expr.BoolVal)
				evaluating = false
			case ExprString:
				val = StringValue(expr.StrVal)
				evaluating = false
			case ExprChar:
				val = CharValue([]rune(expr.StrVal)[0])
				evaluating = false
			case ExprFloat:
				val = FloatValue(expr.FloatVal)
				evaluating = false
			case ExprRational:
				val = RationalValue(expr.Num, expr.Den)
				evaluating = false
			case ExprLiteral:
				val = expr.LitVal
				evaluating = false
			case ExprSymbol:
				v, ok := env.Get(expr.StrVal)
				if !ok {
					return nil, fmt.Errorf("%d:%d: unbound variable '%s'", expr.Line, expr.Col, expr.StrVal)
				}
				val = v
				evaluating = false

			case ExprList:
				if len(expr.Elements) == 0 {
					return nil, fmt.Errorf("%d:%d: empty application", expr.Line, expr.Col)
				}
				head := expr.Elements[0]

				isSpecial := false
				if head.Type == ExprSymbol {
					switch head.StrVal {
					case "quote":
						v, err := evalQuote(expr)
						if err != nil {
							return nil, err
						}
						val = v
						evaluating = false
						isSpecial = true

					case "lambda":
						v, err := evalLambda(expr, env)
						if err != nil {
							return nil, err
						}
						val = v
						evaluating = false
						isSpecial = true

					case "case-lambda":
						v, err := evalCaseLambda(expr, env)
						if err != nil {
							return nil, err
						}
						val = v
						evaluating = false
						isSpecial = true

					case "define-syntax":
						v, err := evalDefineSyntax(expr, env)
						if err != nil {
							return nil, err
						}
						val = v
						evaluating = false
						isSpecial = true

					case "define-record-type":
						v, err := evalDefineRecordType(expr, env)
						if err != nil {
							return nil, err
						}
						val = v
						evaluating = false
						isSpecial = true

					case "do":
						v, err := evalDo(expr, env)
						if err != nil {
							return nil, err
						}
						val = v
						evaluating = false
						isSpecial = true

					case "define":
						if len(expr.Elements) < 3 {
							return nil, fmt.Errorf("%d:%d: 'define' requires at least 2 arguments", expr.Line, expr.Col)
						}
						target := expr.Elements[1]
						if target.Type == ExprList && len(target.Elements) > 0 {
							fname := target.Elements[0]
							if fname.Type != ExprSymbol {
								return nil, fmt.Errorf("%d:%d: expected symbol in define", expr.Line, expr.Col)
							}
							params, restParam, err := parseParams(target.Elements[1:])
							if err != nil {
								return nil, err
							}
							env.Set(fname.StrVal, &Value{
								Type: TypeLambda, Params: params, RestParam: restParam,
								Body: expr.Elements[2:], ClosureEnv: env,
							})
							val = Void
							evaluating = false
						} else {
							if target.Type != ExprSymbol {
								return nil, fmt.Errorf("%d:%d: expected symbol after define", expr.Line, expr.Col)
							}
							kont = &KontFrame{Tag: KDefine, Name: target.StrVal, Env: env, Next: kont}
							expr = expr.Elements[2]
						}
						isSpecial = true

					case "set!":
						if len(expr.Elements) != 3 {
							return nil, fmt.Errorf("%d:%d: 'set!' requires exactly 2 arguments", expr.Line, expr.Col)
						}
						target := expr.Elements[1]
						if target.Type != ExprSymbol {
							return nil, fmt.Errorf("%d:%d: 'set!' expects a symbol", expr.Line, expr.Col)
						}
						kont = &KontFrame{Tag: KSetBang, Name: target.StrVal, Env: env, Line: expr.Line, Col: expr.Col, Next: kont}
						expr = expr.Elements[2]
						isSpecial = true

					case "if":
						if len(expr.Elements) < 3 || len(expr.Elements) > 4 {
							return nil, fmt.Errorf("%d:%d: 'if' requires 2 or 3 arguments", expr.Line, expr.Col)
						}
						var alt *Expr
						if len(expr.Elements) == 4 {
							alt = expr.Elements[3]
						}
						kont = &KontFrame{Tag: KIfTest, Conseq: expr.Elements[2], Alt: alt, Env: env, Next: kont}
						expr = expr.Elements[1]
						isSpecial = true

					case "begin":
						if len(expr.Elements) < 2 {
							val = Void
							evaluating = false
						} else {
							body := expr.Elements[1:]
							if len(body) > 1 {
								kont = &KontFrame{Tag: KBody, Exprs: body[1:], Env: env, Next: kont}
							}
							expr = body[0]
						}
						isSpecial = true

					case "and":
						if len(expr.Elements) == 1 {
							val = True
							evaluating = false
						} else if len(expr.Elements) == 2 {
							expr = expr.Elements[1]
						} else {
							kont = &KontFrame{Tag: KAnd, Exprs: expr.Elements[2:], Env: env, Next: kont}
							expr = expr.Elements[1]
						}
						isSpecial = true

					case "or":
						if len(expr.Elements) == 1 {
							val = False
							evaluating = false
						} else if len(expr.Elements) == 2 {
							expr = expr.Elements[1]
						} else {
							kont = &KontFrame{Tag: KOr, Exprs: expr.Elements[2:], Env: env, Next: kont}
							expr = expr.Elements[1]
						}
						isSpecial = true

					case "cond":
						clauses := expr.Elements[1:]
						if len(clauses) == 0 {
							val = Void
							evaluating = false
						} else {
							cekStartCondClause(clauses[0], clauses[1:], env, kont,
								&expr, &env, &kont, &val, &evaluating)
						}
						isSpecial = true

					case "let":
						err := cekStartLet(expr, env, kont, &expr, &env, &kont, &val, &evaluating)
						if err != nil {
							return nil, err
						}
						isSpecial = true

					case "let*":
						err := cekStartLetStar(expr, env, kont, &expr, &env, &kont, &val, &evaluating)
						if err != nil {
							return nil, err
						}
						isSpecial = true

					case "letrec":
						err := cekStartLetrec(expr, env, kont, false, &expr, &env, &kont, &val, &evaluating)
						if err != nil {
							return nil, err
						}
						isSpecial = true

					case "letrec*":
						err := cekStartLetrec(expr, env, kont, true, &expr, &env, &kont, &val, &evaluating)
						if err != nil {
							return nil, err
						}
						isSpecial = true

					case "case":
						if len(expr.Elements) < 3 {
							return nil, fmt.Errorf("%d:%d: 'case' requires key and clauses", expr.Line, expr.Col)
						}
						kont = &KontFrame{
							Tag: KCaseKey, Clauses: expr.Elements[2:],
							Env: env, Line: expr.Line, Col: expr.Col, Next: kont,
						}
						expr = expr.Elements[1]
						isSpecial = true

					case "guard":
						if len(expr.Elements) < 3 {
							return nil, fmt.Errorf("%d:%d: 'guard' requires variable and body", expr.Line, expr.Col)
						}
						guardSpec := expr.Elements[1]
						if guardSpec.Type != ExprList || len(guardSpec.Elements) < 1 {
							return nil, fmt.Errorf("%d:%d: invalid guard specification", expr.Line, expr.Col)
						}
						if guardSpec.Elements[0].Type != ExprSymbol {
							return nil, fmt.Errorf("%d:%d: guard variable must be a symbol", expr.Line, expr.Col)
						}
						guardVar := guardSpec.Elements[0].StrVal
						guardClauses := guardSpec.Elements[1:]
						guardBody := expr.Elements[2:]
						guardKont := &KontFrame{
							Tag:     KGuardHandler,
							Name:    guardVar,
							Clauses: guardClauses,
							Env:     env,
							Wind:    windStack,
							Next:    kont,
						}
						if len(guardBody) > 1 {
							kont = &KontFrame{Tag: KBody, Exprs: guardBody[1:], Env: env, Next: guardKont}
						} else {
							kont = guardKont
						}
						expr = guardBody[0]
						isSpecial = true
					}

					if isSpecial {
						continue
					}

					// Not a special form — check for macro
					if macroVal, ok := env.Get(head.StrVal); ok && macroVal.Type == TypeMacro {
						expanded, err := expandMacro(macroVal.Macro, expr)
						if err != nil {
							return nil, err
						}
						expr = expanded
						continue
					}
				}

				// Function application: push KEvFun, evaluate operator
				kont = &KontFrame{
					Tag: KEvFun, Exprs: expr.Elements[1:], Env: env,
					Line: expr.Line, Col: expr.Col, OrigExpr: expr, Next: kont,
				}
				expr = head

			default:
				return nil, fmt.Errorf("%d:%d: unknown expression type", expr.Line, expr.Col)
			}
		} else {
			// ==================== APPLY CONTINUATION MODE ====================
			switch kont.Tag {
			case KHalt:
				return val, nil

			case KTopLevel:
				result := kont.Result
				if val.Type != TypeVoid {
					result = val
				}
				if len(kont.Exprs) == 0 {
					if result != nil {
						val = result
					}
					kont = kont.Next
					continue
				}
				nextExpr := kont.Exprs[0]
				topEnv := kont.Env
				kont = &KontFrame{
					Tag: KTopLevel, Exprs: kont.Exprs[1:], Env: topEnv,
					Result: result, Next: kont.Next,
				}
				expr = nextExpr
				env = topEnv
				evaluating = true

			case KBody:
				if len(kont.Exprs) == 1 {
					expr = kont.Exprs[0]
					env = kont.Env
					kont = kont.Next
					evaluating = true
				} else {
					nextExpr := kont.Exprs[0]
					kont = &KontFrame{Tag: KBody, Exprs: kont.Exprs[1:], Env: kont.Env, Next: kont.Next}
					expr = nextExpr
					env = kont.Env
					evaluating = true
				}

			case KDefine:
				kont.Env.Set(kont.Name, val)
				val = Void
				kont = kont.Next

			case KSetBang:
				if !kont.Env.Update(kont.Name, val) {
					return nil, fmt.Errorf("%d:%d: unbound variable '%s'", kont.Line, kont.Col, kont.Name)
				}
				val = Void
				kont = kont.Next

			case KIfTest:
				if isTruthy(val) {
					expr = kont.Conseq
				} else if kont.Alt != nil {
					expr = kont.Alt
				} else {
					val = Void
					kont = kont.Next
					continue
				}
				env = kont.Env
				kont = kont.Next
				evaluating = true

			case KEvFun:
				if val.Type == TypeMacro {
					expanded, err := expandMacro(val.Macro, kont.OrigExpr)
					if err != nil {
						return nil, err
					}
					expr = expanded
					env = kont.Env
					kont = kont.Next
					evaluating = true
					continue
				}
				argExprs := kont.Exprs
				if len(argExprs) == 0 {
					err := cekApplyFunction(val, nil, kont.Next, kont.Env, kont.Line, kont.Col,
						&expr, &env, &kont, &val, &evaluating, &windStack)
					if err != nil {
						return nil, err
					}
				} else {
					lastIdx := len(argExprs) - 1
					nextExpr := argExprs[lastIdx]
					evEnv := kont.Env
					kont = &KontFrame{
						Tag: KEvArgs, Fn: val, Exprs: argExprs[:lastIdx],
						Env: evEnv, Line: kont.Line, Col: kont.Col, Next: kont.Next,
					}
					expr = nextExpr
					env = evEnv
					evaluating = true
				}

			case KEvArgs:
				newVals := make([]*Value, len(kont.Vals)+1)
				newVals[0] = val
				copy(newVals[1:], kont.Vals)
				if len(kont.Exprs) == 0 {
					err := cekApplyFunction(kont.Fn, newVals, kont.Next, kont.Env, kont.Line, kont.Col,
						&expr, &env, &kont, &val, &evaluating, &windStack)
					if err != nil {
						return nil, err
					}
				} else {
					lastIdx := len(kont.Exprs) - 1
					nextExpr := kont.Exprs[lastIdx]
					evEnv := kont.Env
					kont = &KontFrame{
						Tag: KEvArgs, Fn: kont.Fn, Vals: newVals, Exprs: kont.Exprs[:lastIdx],
						Env: evEnv, Line: kont.Line, Col: kont.Col, Next: kont.Next,
					}
					expr = nextExpr
					env = evEnv
					evaluating = true
				}

			case KAnd:
				if !isTruthy(val) {
					kont = kont.Next
				} else if len(kont.Exprs) == 1 {
					expr = kont.Exprs[0]
					env = kont.Env
					kont = kont.Next
					evaluating = true
				} else {
					nextExpr := kont.Exprs[0]
					kont = &KontFrame{Tag: KAnd, Exprs: kont.Exprs[1:], Env: kont.Env, Next: kont.Next}
					expr = nextExpr
					env = kont.Env
					evaluating = true
				}

			case KOr:
				if isTruthy(val) {
					kont = kont.Next
				} else if len(kont.Exprs) == 1 {
					expr = kont.Exprs[0]
					env = kont.Env
					kont = kont.Next
					evaluating = true
				} else {
					nextExpr := kont.Exprs[0]
					kont = &KontFrame{Tag: KOr, Exprs: kont.Exprs[1:], Env: kont.Env, Next: kont.Next}
					expr = nextExpr
					env = kont.Env
					evaluating = true
				}

			case KCondClauses:
				if isTruthy(val) {
					body := kont.ClauseBody
					if len(body) == 0 {
						kont = kont.Next
					} else {
						cekEnterBody(body, kont.Env, kont.Next, &expr, &env, &kont, &val, &evaluating)
					}
				} else {
					clauses := kont.Clauses
					if len(clauses) == 0 {
						val = Void
						kont = kont.Next
					} else {
						cekStartCondClause(clauses[0], clauses[1:], kont.Env, kont.Next,
							&expr, &env, &kont, &val, &evaluating)
					}
				}

			case KLetBind:
				newVals := append(append([]*Value{}, kont.Vals...), val)
				if len(kont.Exprs) > 0 {
					nextExpr := kont.Exprs[0]
					kont = &KontFrame{
						Tag: KLetBind, Names: kont.Names, Vals: newVals,
						Exprs: kont.Exprs[1:], BodyExprs: kont.BodyExprs,
						Env: kont.Env, NamedLet: kont.NamedLet, Next: kont.Next,
					}
					expr = nextExpr
					env = kont.Env
					evaluating = true
				} else {
					letEnv := NewEnv(kont.Env)
					for i, name := range kont.Names {
						letEnv.Set(name, newVals[i])
					}
					if kont.NamedLet != "" {
						letEnv.Set(kont.NamedLet, &Value{
							Type: TypeLambda, Params: kont.Names,
							Body: kont.BodyExprs, ClosureEnv: letEnv,
						})
					}
					cekEnterBody(kont.BodyExprs, letEnv, kont.Next, &expr, &env, &kont, &val, &evaluating)
				}

			case KLetStarBind:
				kont.LetEnv.Set(kont.Name, val)
				if len(kont.Exprs) > 0 {
					nextBinding := kont.Exprs[0]
					if nextBinding.Type != ExprList || len(nextBinding.Elements) != 2 || nextBinding.Elements[0].Type != ExprSymbol {
						return nil, fmt.Errorf("invalid let* binding")
					}
					letEnv := kont.LetEnv
					kont = &KontFrame{
						Tag: KLetStarBind, Name: nextBinding.Elements[0].StrVal,
						Exprs: kont.Exprs[1:], BodyExprs: kont.BodyExprs,
						LetEnv: letEnv, Next: kont.Next,
					}
					expr = nextBinding.Elements[1]
					env = letEnv
					evaluating = true
				} else {
					cekEnterBody(kont.BodyExprs, kont.LetEnv, kont.Next, &expr, &env, &kont, &val, &evaluating)
				}

			case KLetrecBind:
				newVals := append(append([]*Value{}, kont.Vals...), val)
				if len(kont.Exprs) > 0 {
					nextExpr := kont.Exprs[0]
					kont = &KontFrame{
						Tag: KLetrecBind, Names: kont.Names, Vals: newVals,
						Exprs: kont.Exprs[1:], BodyExprs: kont.BodyExprs,
						LetEnv: kont.LetEnv, Next: kont.Next,
					}
					expr = nextExpr
					env = kont.LetEnv
					evaluating = true
				} else {
					for i, name := range kont.Names {
						kont.LetEnv.Set(name, newVals[i])
					}
					cekEnterBody(kont.BodyExprs, kont.LetEnv, kont.Next, &expr, &env, &kont, &val, &evaluating)
				}

			case KLetrecStarBind:
				kont.LetEnv.Set(kont.Name, val)
				if len(kont.Exprs) > 0 {
					nextBinding := kont.Exprs[0]
					if nextBinding.Type != ExprList || len(nextBinding.Elements) != 2 || nextBinding.Elements[0].Type != ExprSymbol {
						return nil, fmt.Errorf("invalid letrec* binding")
					}
					letEnv := kont.LetEnv
					kont = &KontFrame{
						Tag: KLetrecStarBind, Name: nextBinding.Elements[0].StrVal,
						Exprs: kont.Exprs[1:], BodyExprs: kont.BodyExprs,
						LetEnv: letEnv, Next: kont.Next,
					}
					expr = nextBinding.Elements[1]
					env = letEnv
					evaluating = true
				} else {
					cekEnterBody(kont.BodyExprs, kont.LetEnv, kont.Next, &expr, &env, &kont, &val, &evaluating)
				}

			case KCaseKey:
				key := val
				matched := false
				for _, clause := range kont.Clauses {
					if clause.Type != ExprList || len(clause.Elements) < 2 {
						return nil, fmt.Errorf("%d:%d: invalid case clause", kont.Line, kont.Col)
					}
					isElse := clause.Elements[0].Type == ExprSymbol && clause.Elements[0].StrVal == "else"
					match := isElse
					if !isElse {
						datums := clause.Elements[0]
						if datums.Type != ExprList {
							return nil, fmt.Errorf("%d:%d: case clause datums must be a list", kont.Line, kont.Col)
						}
						for _, d := range datums.Elements {
							if valuesEqv(key, exprToValue(d)) {
								match = true
								break
							}
						}
					}
					if match {
						body := clause.Elements[1:]
						cekEnterBody(body, kont.Env, kont.Next, &expr, &env, &kont, &val, &evaluating)
						matched = true
						break
					}
				}
				if !matched {
					val = Void
					kont = kont.Next
				}

			case KDynWindIn:
				// In-thunk completed. Push wind entry, call body-thunk.
				inThunk := kont.Vals[0]
				bodyThunk := kont.Vals[1]
				outThunk := kont.Vals[2]
				prevWind := kont.Wind
				windStack = &WindEntry{InThunk: inThunk, OutThunk: outThunk, Next: prevWind}
				bodyKont := &KontFrame{
					Tag:  KDynWindBody,
					Vals: []*Value{outThunk},
					Wind: prevWind,
					Next: kont.Next,
				}
				err := cekApplyFunction(bodyThunk, nil, bodyKont, env, 0, 0,
					&expr, &env, &kont, &val, &evaluating, &windStack)
				if err != nil {
					return nil, err
				}

			case KDynWindBody:
				// Body completed. Pop wind entry, call out-thunk.
				outThunk := kont.Vals[0]
				bodyResult := val
				windStack = kont.Wind // restore to pre-push
				outKont := &KontFrame{
					Tag:    KDynWindOut,
					Result: bodyResult,
					Next:   kont.Next,
				}
				err := cekApplyFunction(outThunk, nil, outKont, env, 0, 0,
					&expr, &env, &kont, &val, &evaluating, &windStack)
				if err != nil {
					return nil, err
				}

			case KDynWindOut:
				// Out-thunk completed. Return saved body result.
				val = kont.Result
				kont = kont.Next

			case KDynWindTransition:
				// Wind transition: update wind stack, call thunk.
				windStack = kont.Wind
				thunk := kont.Vals[0]
				err := cekApplyFunction(thunk, nil, kont.Next, env, 0, 0,
					&expr, &env, &kont, &val, &evaluating, &windStack)
				if err != nil {
					return nil, err
				}

			case KDynWindRestore:
				// All transitions done. Restore val and wind stack.
				val = kont.Result
				windStack = kont.Wind
				kont = kont.Next

			case KExcHandler:
				// Thunk completed normally, pass through
				kont = kont.Next

			case KGuardHandler:
				// Body completed normally, pass through
				kont = kont.Next

			case KRaiseCallHandler:
				handler := kont.Fn
				raisedVal := kont.Result
				err := cekApplyFunction(handler, []*Value{raisedVal}, kont.Next, env, 0, 0,
					&expr, &env, &kont, &val, &evaluating, &windStack)
				if err != nil {
					return nil, err
				}

			case KRaiseGuardEval:
				guardEnv := NewEnv(kont.Env)
				guardEnv.Set(kont.Name, kont.Result)
				clauses := kont.Clauses
				if len(clauses) == 0 {
					return nil, fmt.Errorf("unhandled exception: %s", kont.Result.Display())
				}
				cekStartGuardClause(clauses[0], clauses[1:], guardEnv, kont.Result, kont.Next,
					&expr, &env, &kont, &val, &evaluating)

			case KGuardTest:
				if isTruthy(val) {
					body := kont.ClauseBody
					if len(body) == 0 {
						kont = kont.Next
					} else {
						cekEnterBody(body, kont.Env, kont.Next, &expr, &env, &kont, &val, &evaluating)
					}
				} else {
					clauses := kont.Clauses
					if len(clauses) == 0 {
						return nil, fmt.Errorf("unhandled exception: %s", kont.Result.Display())
					}
					cekStartGuardClause(clauses[0], clauses[1:], kont.Env, kont.Result, kont.Next,
						&expr, &env, &kont, &val, &evaluating)
				}

			default:
				return nil, fmt.Errorf("unknown continuation tag: %d", kont.Tag)
			}
		}
	}
}

// ===================== Let Family Helpers =====================

func cekStartLet(expr *Expr, env *Env, kont *KontFrame,
	exprP **Expr, envP **Env, kontP **KontFrame, valP **Value, evaluatingP *bool) error {
	if len(expr.Elements) < 3 {
		return fmt.Errorf("%d:%d: 'let' requires bindings and body", expr.Line, expr.Col)
	}
	bindingsIdx := 1
	var loopName string
	if expr.Elements[1].Type == ExprSymbol {
		loopName = expr.Elements[1].StrVal
		bindingsIdx = 2
		if len(expr.Elements) < 4 {
			return fmt.Errorf("%d:%d: named 'let' requires bindings and body", expr.Line, expr.Col)
		}
	}
	bindingsExpr := expr.Elements[bindingsIdx]
	if bindingsExpr.Type != ExprList {
		return fmt.Errorf("%d:%d: 'let' bindings must be a list", expr.Line, expr.Col)
	}
	body := expr.Elements[bindingsIdx+1:]
	if len(bindingsExpr.Elements) == 0 {
		// No bindings — just evaluate body
		letEnv := NewEnv(env)
		if loopName != "" {
			letEnv.Set(loopName, &Value{Type: TypeLambda, Body: body, ClosureEnv: letEnv})
		}
		cekEnterBody(body, letEnv, kont, exprP, envP, kontP, valP, evaluatingP)
		return nil
	}
	// Parse binding names and init expressions
	names := make([]string, len(bindingsExpr.Elements))
	initExprs := make([]*Expr, len(bindingsExpr.Elements))
	for i, binding := range bindingsExpr.Elements {
		if binding.Type != ExprList || len(binding.Elements) != 2 {
			return fmt.Errorf("%d:%d: invalid let binding", expr.Line, expr.Col)
		}
		if binding.Elements[0].Type != ExprSymbol {
			return fmt.Errorf("%d:%d: let binding name must be a symbol", expr.Line, expr.Col)
		}
		names[i] = binding.Elements[0].StrVal
		initExprs[i] = binding.Elements[1]
	}
	// Push KLetBind, eval first init
	*kontP = &KontFrame{
		Tag: KLetBind, Names: names, Exprs: initExprs[1:],
		BodyExprs: body, Env: env, NamedLet: loopName, Next: kont,
	}
	*exprP = initExprs[0]
	*envP = env
	*evaluatingP = true
	return nil
}

func cekStartLetStar(expr *Expr, env *Env, kont *KontFrame,
	exprP **Expr, envP **Env, kontP **KontFrame, valP **Value, evaluatingP *bool) error {
	if len(expr.Elements) < 3 {
		return fmt.Errorf("%d:%d: 'let*' requires bindings and body", expr.Line, expr.Col)
	}
	bindingsExpr := expr.Elements[1]
	if bindingsExpr.Type != ExprList {
		return fmt.Errorf("%d:%d: 'let*' bindings must be a list", expr.Line, expr.Col)
	}
	body := expr.Elements[2:]
	letEnv := NewEnv(env)
	if len(bindingsExpr.Elements) == 0 {
		cekEnterBody(body, letEnv, kont, exprP, envP, kontP, valP, evaluatingP)
		return nil
	}
	first := bindingsExpr.Elements[0]
	if first.Type != ExprList || len(first.Elements) != 2 || first.Elements[0].Type != ExprSymbol {
		return fmt.Errorf("%d:%d: invalid let* binding", expr.Line, expr.Col)
	}
	*kontP = &KontFrame{
		Tag: KLetStarBind, Name: first.Elements[0].StrVal,
		Exprs: bindingsExpr.Elements[1:], BodyExprs: body,
		LetEnv: letEnv, Next: kont,
	}
	*exprP = first.Elements[1]
	*envP = letEnv
	*evaluatingP = true
	return nil
}

func cekStartLetrec(expr *Expr, env *Env, kont *KontFrame, isStar bool,
	exprP **Expr, envP **Env, kontP **KontFrame, valP **Value, evaluatingP *bool) error {
	keyword := "letrec"
	if isStar {
		keyword = "letrec*"
	}
	if len(expr.Elements) < 3 {
		return fmt.Errorf("%d:%d: '%s' requires bindings and body", expr.Line, expr.Col, keyword)
	}
	bindingsExpr := expr.Elements[1]
	if bindingsExpr.Type != ExprList {
		return fmt.Errorf("%d:%d: '%s' bindings must be a list", expr.Line, expr.Col, keyword)
	}
	body := expr.Elements[2:]
	letEnv := NewEnv(env)

	if len(bindingsExpr.Elements) == 0 {
		cekEnterBody(body, letEnv, kont, exprP, envP, kontP, valP, evaluatingP)
		return nil
	}

	// Pre-set all bindings to void
	for _, binding := range bindingsExpr.Elements {
		if binding.Type != ExprList || len(binding.Elements) != 2 || binding.Elements[0].Type != ExprSymbol {
			return fmt.Errorf("%d:%d: invalid %s binding", expr.Line, expr.Col, keyword)
		}
		letEnv.Set(binding.Elements[0].StrVal, Void)
	}

	if isStar {
		first := bindingsExpr.Elements[0]
		*kontP = &KontFrame{
			Tag: KLetrecStarBind, Name: first.Elements[0].StrVal,
			Exprs: bindingsExpr.Elements[1:], BodyExprs: body,
			LetEnv: letEnv, Next: kont,
		}
		*exprP = first.Elements[1]
	} else {
		names := make([]string, len(bindingsExpr.Elements))
		initExprs := make([]*Expr, len(bindingsExpr.Elements))
		for i, binding := range bindingsExpr.Elements {
			names[i] = binding.Elements[0].StrVal
			initExprs[i] = binding.Elements[1]
		}
		*kontP = &KontFrame{
			Tag: KLetrecBind, Names: names, Exprs: initExprs[1:],
			BodyExprs: body, LetEnv: letEnv, Next: kont,
		}
		*exprP = initExprs[0]
	}
	*envP = letEnv
	*evaluatingP = true
	return nil
}

// ===================== Helpers =====================

func cekEnterBody(body []*Expr, bodyEnv *Env, nextKont *KontFrame,
	exprP **Expr, envP **Env, kontP **KontFrame, valP **Value, evaluatingP *bool) {
	if len(body) == 0 {
		*valP = Void
		*kontP = nextKont
		*evaluatingP = false
		return
	}
	k := nextKont
	if len(body) > 1 {
		k = &KontFrame{Tag: KBody, Exprs: body[1:], Env: bodyEnv, Next: nextKont}
	}
	*exprP = body[0]
	*envP = bodyEnv
	*kontP = k
	*evaluatingP = true
}

func cekStartCondClause(clause *Expr, remaining []*Expr, condEnv *Env, nextKont *KontFrame,
	exprP **Expr, envP **Env, kontP **KontFrame, valP **Value, evaluatingP *bool) {
	if clause.Type != ExprList || len(clause.Elements) < 1 {
		*valP = Void
		*kontP = nextKont
		*evaluatingP = false
		return
	}
	isElse := clause.Elements[0].Type == ExprSymbol && clause.Elements[0].StrVal == "else"
	body := clause.Elements[1:]
	if isElse {
		cekEnterBody(body, condEnv, nextKont, exprP, envP, kontP, valP, evaluatingP)
		return
	}
	*kontP = &KontFrame{
		Tag: KCondClauses, ClauseBody: body, Clauses: remaining,
		Env: condEnv, Next: nextKont,
	}
	*exprP = clause.Elements[0]
	*envP = condEnv
	*evaluatingP = true
}

// cekApplyFunction handles applying a function to arguments within the CEK machine.
func cekApplyFunction(fn *Value, args []*Value, outerKont *KontFrame, env *Env, line, col int,
	exprP **Expr, envP **Env, kontP **KontFrame, valP **Value, evaluatingP *bool, windStackP **WindEntry) error {
	for {
		if fn.Type == TypeSymbol && len(fn.StrVal) > 8 && fn.StrVal[:8] == "builtin:" {
			switch fn.StrVal {
			case "builtin:call/cc":
				if len(args) != 1 {
					return fmt.Errorf("%d:%d: call/cc requires exactly 1 argument", line, col)
				}
				contVal := &Value{Type: TypeContinuation, ContKont: outerKont, ContWind: *windStackP}
				fn = args[0]
				args = []*Value{contVal}
				continue
			case "builtin:dynamic-wind":
				if len(args) != 3 {
					return fmt.Errorf("%d:%d: dynamic-wind requires exactly 3 arguments", line, col)
				}
				inThunk, bodyThunk, outThunk := args[0], args[1], args[2]
				windKont := &KontFrame{
					Tag:  KDynWindIn,
					Vals: []*Value{inThunk, bodyThunk, outThunk},
					Wind: *windStackP,
					Next: outerKont,
				}
				fn = inThunk
				args = nil
				outerKont = windKont
				continue
			case "builtin:raise":
				if len(args) != 1 {
					return fmt.Errorf("%d:%d: raise requires exactly 1 argument", line, col)
				}
				raisedVal := args[0]
				// Search continuation stack for exception handler
				k := outerKont
				for k != nil {
					if k.Tag == KExcHandler || k.Tag == KGuardHandler {
						break
					}
					k = k.Next
				}
				if k == nil {
					return fmt.Errorf("unhandled exception: %s", raisedVal.Display())
				}
				targetWind := k.Wind
				currentWind := *windStackP
				var finalK *KontFrame
				if k.Tag == KExcHandler {
					finalK = &KontFrame{
						Tag: KRaiseCallHandler, Fn: k.Fn,
						Result: raisedVal, Next: k.Next,
					}
				} else {
					finalK = &KontFrame{
						Tag: KRaiseGuardEval, Name: k.Name,
						Clauses: k.Clauses, Env: k.Env,
						Result: raisedVal, Next: k.Next,
					}
				}
				if currentWind != targetWind {
					finalK = buildWindTransitionChain(currentWind, targetWind, finalK)
				}
				*valP = Void
				*kontP = finalK
				*evaluatingP = false
				return nil
			case "builtin:with-exception-handler":
				if len(args) != 2 {
					return fmt.Errorf("%d:%d: with-exception-handler requires exactly 2 arguments", line, col)
				}
				handler := args[0]
				thunk := args[1]
				excKont := &KontFrame{
					Tag: KExcHandler, Fn: handler,
					Wind: *windStackP, Next: outerKont,
				}
				fn = thunk
				args = nil
				outerKont = excKont
				continue
			case "builtin:apply":
				if len(args) < 2 {
					return fmt.Errorf("%d:%d: 'apply' requires at least 2 arguments", line, col)
				}
				applyFn := args[0]
				lastArg := args[len(args)-1]
				var fullArgs []*Value
				for _, a := range args[1 : len(args)-1] {
					fullArgs = append(fullArgs, a)
				}
				cur := lastArg
				for cur.Type == TypePair {
					fullArgs = append(fullArgs, cur.Car)
					cur = cur.Cdr
				}
				fn = applyFn
				args = fullArgs
				continue
			default:
				result, err := callBuiltin(fn.StrVal, args, env, line, col)
				if err != nil {
					return err
				}
				*valP = result
				*kontP = outerKont
				*evaluatingP = false
				return nil
			}
		}
		break
	}
	if fn.Type == TypeGoFunc {
		result, err := fn.GoFunc(args)
		if err != nil {
			return err
		}
		*valP = result
		*kontP = outerKont
		*evaluatingP = false
		return nil
	}
	if fn.Type == TypeLambda {
		callEnv, err := bindLambdaEnv(fn, args, line, col)
		if err != nil {
			return err
		}
		if len(fn.Body) == 0 {
			*valP = Void
			*kontP = outerKont
			*evaluatingP = false
			return nil
		}
		k := outerKont
		if len(fn.Body) > 1 {
			k = &KontFrame{Tag: KBody, Exprs: fn.Body[1:], Env: callEnv, Next: outerKont}
		}
		*exprP = fn.Body[0]
		*envP = callEnv
		*kontP = k
		*evaluatingP = true
		return nil
	}
	if fn.Type == TypeCaseLambda {
		callEnv, body, err := bindCaseLambdaEnv(fn, args, line, col)
		if err != nil {
			return err
		}
		if len(body) == 0 {
			*valP = Void
			*kontP = outerKont
			*evaluatingP = false
			return nil
		}
		k := outerKont
		if len(body) > 1 {
			k = &KontFrame{Tag: KBody, Exprs: body[1:], Env: callEnv, Next: outerKont}
		}
		*exprP = body[0]
		*envP = callEnv
		*kontP = k
		*evaluatingP = true
		return nil
	}
	if fn.Type == TypeContinuation {
		if len(args) != 1 {
			return fmt.Errorf("%d:%d: continuation expects exactly 1 argument", line, col)
		}
		contArg := args[0]
		currentWind := *windStackP
		targetWind := fn.ContWind
		if currentWind == targetWind {
			*valP = contArg
			*kontP = fn.ContKont
			*evaluatingP = false
			return nil
		}
		// Build wind transition frames
		common := commonWindPrefix(currentWind, targetWind)
		type windOp struct {
			thunk   *Value
			newWind *WindEntry
		}
		var ops []windOp
		// Unwind: call out-thunks from innermost to outermost
		for w := currentWind; w != common; w = w.Next {
			ops = append(ops, windOp{thunk: w.OutThunk, newWind: w.Next})
		}
		// Rewind: call in-thunks from outermost to innermost
		var rewindEntries []*WindEntry
		for w := targetWind; w != common; w = w.Next {
			rewindEntries = append(rewindEntries, w)
		}
		for i := len(rewindEntries) - 1; i >= 0; i-- {
			ops = append(ops, windOp{thunk: rewindEntries[i].InThunk, newWind: rewindEntries[i]})
		}
		// Build frame chain: restore frame at bottom, then transition frames
		kont := &KontFrame{
			Tag:    KDynWindRestore,
			Result: contArg,
			Wind:   targetWind,
			Next:   fn.ContKont,
		}
		for i := len(ops) - 1; i >= 0; i-- {
			kont = &KontFrame{
				Tag:  KDynWindTransition,
				Vals: []*Value{ops[i].thunk},
				Wind: ops[i].newWind,
				Next: kont,
			}
		}
		// Kick off the transition chain
		*valP = Void
		*kontP = kont
		*evaluatingP = false
		return nil
	}
	return fmt.Errorf("%d:%d: not a procedure", line, col)
}

func commonWindPrefix(a, b *WindEntry) *WindEntry {
	da, db := windDepth(a), windDepth(b)
	for da > db {
		a = a.Next
		da--
	}
	for db > da {
		b = b.Next
		db--
	}
	for a != b {
		a = a.Next
		b = b.Next
	}
	return a
}

func windDepth(w *WindEntry) int {
	n := 0
	for w != nil {
		n++
		w = w.Next
	}
	return n
}

func buildWindTransitionChain(currentWind, targetWind *WindEntry, finalKont *KontFrame) *KontFrame {
	common := commonWindPrefix(currentWind, targetWind)
	type windOp struct {
		thunk   *Value
		newWind *WindEntry
	}
	var ops []windOp
	for w := currentWind; w != common; w = w.Next {
		ops = append(ops, windOp{thunk: w.OutThunk, newWind: w.Next})
	}
	var rewindEntries []*WindEntry
	for w := targetWind; w != common; w = w.Next {
		rewindEntries = append(rewindEntries, w)
	}
	for i := len(rewindEntries) - 1; i >= 0; i-- {
		ops = append(ops, windOp{thunk: rewindEntries[i].InThunk, newWind: rewindEntries[i]})
	}
	result := finalKont
	for i := len(ops) - 1; i >= 0; i-- {
		result = &KontFrame{
			Tag:  KDynWindTransition,
			Vals: []*Value{ops[i].thunk},
			Wind: ops[i].newWind,
			Next: result,
		}
	}
	return result
}

func cekStartGuardClause(clause *Expr, remaining []*Expr, guardEnv *Env, raisedVal *Value, nextKont *KontFrame,
	exprP **Expr, envP **Env, kontP **KontFrame, valP **Value, evaluatingP *bool) {
	if clause.Type != ExprList || len(clause.Elements) < 1 {
		*valP = Void
		*kontP = nextKont
		*evaluatingP = false
		return
	}
	isElse := clause.Elements[0].Type == ExprSymbol && clause.Elements[0].StrVal == "else"
	body := clause.Elements[1:]
	if isElse {
		cekEnterBody(body, guardEnv, nextKont, exprP, envP, kontP, valP, evaluatingP)
		return
	}
	*kontP = &KontFrame{
		Tag: KGuardTest, ClauseBody: body, Clauses: remaining,
		Env: guardEnv, Result: raisedVal, Next: nextKont,
	}
	*exprP = clause.Elements[0]
	*envP = guardEnv
	*evaluatingP = true
}

// ===================== Legacy Call Helpers (for builtins) =====================

func callLambda(fn *Value, args []*Value, line, col int) (*Value, error) {
	callEnv, err := bindLambdaEnv(fn, args, line, col)
	if err != nil {
		return nil, err
	}
	if len(fn.Body) == 0 {
		return Void, nil
	}
	kont := &KontFrame{Tag: KHalt}
	if len(fn.Body) > 1 {
		kont = &KontFrame{Tag: KBody, Exprs: fn.Body[1:], Env: callEnv, Next: kont}
	}
	return cekEval(fn.Body[0], callEnv, kont)
}

func callCaseLambda(fn *Value, args []*Value, line, col int) (*Value, error) {
	callEnv, body, err := bindCaseLambdaEnv(fn, args, line, col)
	if err != nil {
		return nil, err
	}
	if len(body) == 0 {
		return Void, nil
	}
	kont := &KontFrame{Tag: KHalt}
	if len(body) > 1 {
		kont = &KontFrame{Tag: KBody, Exprs: body[1:], Env: callEnv, Next: kont}
	}
	return cekEval(body[0], callEnv, kont)
}

// ===================== Expression Helpers =====================

func evalQuote(expr *Expr) (*Value, error) {
	if len(expr.Elements) != 2 {
		return nil, fmt.Errorf("%d:%d: 'quote' requires exactly 1 argument", expr.Line, expr.Col)
	}
	return exprToValue(expr.Elements[1]), nil
}

func exprToValue(expr *Expr) *Value {
	switch expr.Type {
	case ExprInteger:
		return IntValue(expr.IntVal)
	case ExprBoolean:
		return BoolValue(expr.BoolVal)
	case ExprString:
		return StringValue(expr.StrVal)
	case ExprSymbol:
		return SymbolValue(expr.StrVal)
	case ExprFloat:
		return FloatValue(expr.FloatVal)
	case ExprRational:
		return RationalValue(expr.Num, expr.Den)
	case ExprList:
		if len(expr.Elements) == 0 {
			return Null
		}
		result := Null
		for i := len(expr.Elements) - 1; i >= 0; i-- {
			result = PairValue(exprToValue(expr.Elements[i]), result)
		}
		return result
	}
	return Void
}

func parseParams(elements []*Expr) (params []string, restParam string, err error) {
	for i, p := range elements {
		if p.Type != ExprSymbol {
			return nil, "", fmt.Errorf("%d:%d: expected symbol as parameter", p.Line, p.Col)
		}
		if p.StrVal == "." {
			if i+1 != len(elements)-1 {
				return nil, "", fmt.Errorf("%d:%d: expected exactly one parameter after '.'", p.Line, p.Col)
			}
			rest := elements[i+1]
			if rest.Type != ExprSymbol {
				return nil, "", fmt.Errorf("%d:%d: expected symbol after '.'", rest.Line, rest.Col)
			}
			return params, rest.StrVal, nil
		}
		params = append(params, p.StrVal)
	}
	return params, "", nil
}

func evalLambda(expr *Expr, env *Env) (*Value, error) {
	if len(expr.Elements) < 3 {
		return nil, fmt.Errorf("%d:%d: 'lambda' requires parameters and body", expr.Line, expr.Col)
	}
	paramExpr := expr.Elements[1]
	if paramExpr.Type == ExprSymbol {
		return &Value{
			Type: TypeLambda, RestParam: paramExpr.StrVal,
			Body: expr.Elements[2:], ClosureEnv: env,
		}, nil
	}
	if paramExpr.Type != ExprList {
		return nil, fmt.Errorf("%d:%d: 'lambda' parameters must be a list", expr.Line, expr.Col)
	}
	params, restParam, err := parseParams(paramExpr.Elements)
	if err != nil {
		return nil, err
	}
	return &Value{
		Type: TypeLambda, Params: params, RestParam: restParam,
		Body: expr.Elements[2:], ClosureEnv: env,
	}, nil
}

func evalCaseLambda(expr *Expr, env *Env) (*Value, error) {
	clauses := make([]CaseLambdaClause, 0, len(expr.Elements)-1)
	for _, clauseExpr := range expr.Elements[1:] {
		if clauseExpr.Type != ExprList || len(clauseExpr.Elements) < 2 {
			return nil, fmt.Errorf("%d:%d: invalid case-lambda clause", expr.Line, expr.Col)
		}
		formalsExpr := clauseExpr.Elements[0]
		if formalsExpr.Type != ExprList {
			return nil, fmt.Errorf("%d:%d: case-lambda clause formals must be a list", expr.Line, expr.Col)
		}
		params, restParam, err := parseParams(formalsExpr.Elements)
		if err != nil {
			return nil, err
		}
		clauses = append(clauses, CaseLambdaClause{
			Params: params, RestParam: restParam,
			Body: clauseExpr.Elements[1:],
		})
	}
	return &Value{Type: TypeCaseLambda, CaseClauses: clauses, ClosureEnv: env}, nil
}

func evalDefineRecordType(expr *Expr, env *Env) (*Value, error) {
	if len(expr.Elements) < 5 {
		return nil, fmt.Errorf("%d:%d: define-record-type requires type name, constructor, predicate, and fields", expr.Line, expr.Col)
	}
	typeName := expr.Elements[1]
	if typeName.Type != ExprSymbol {
		return nil, fmt.Errorf("%d:%d: expected symbol for record type name", expr.Line, expr.Col)
	}
	ctorExpr := expr.Elements[2]
	if ctorExpr.Type != ExprList || len(ctorExpr.Elements) < 1 {
		return nil, fmt.Errorf("%d:%d: expected constructor specification", expr.Line, expr.Col)
	}
	ctorName := ctorExpr.Elements[0]
	if ctorName.Type != ExprSymbol {
		return nil, fmt.Errorf("%d:%d: expected symbol for constructor name", expr.Line, expr.Col)
	}
	ctorFields := make([]string, len(ctorExpr.Elements)-1)
	for i, f := range ctorExpr.Elements[1:] {
		if f.Type != ExprSymbol {
			return nil, fmt.Errorf("%d:%d: expected symbol for constructor field", expr.Line, expr.Col)
		}
		ctorFields[i] = f.StrVal
	}
	predExpr := expr.Elements[3]
	if predExpr.Type != ExprSymbol {
		return nil, fmt.Errorf("%d:%d: expected symbol for predicate name", expr.Line, expr.Col)
	}
	var fieldNames []string
	accessorMap := make(map[string]int)
	for _, fieldSpec := range expr.Elements[4:] {
		if fieldSpec.Type != ExprList || len(fieldSpec.Elements) < 2 {
			return nil, fmt.Errorf("%d:%d: expected (field accessor) specification", expr.Line, expr.Col)
		}
		fieldName := fieldSpec.Elements[0]
		accessor := fieldSpec.Elements[1]
		if fieldName.Type != ExprSymbol || accessor.Type != ExprSymbol {
			return nil, fmt.Errorf("%d:%d: expected symbols in field specification", expr.Line, expr.Col)
		}
		fieldNames = append(fieldNames, fieldName.StrVal)
		accessorMap[accessor.StrVal] = len(fieldNames) - 1
	}
	ctorFieldIdx := make([]int, len(ctorFields))
	for i, cf := range ctorFields {
		found := false
		for j, fn := range fieldNames {
			if cf == fn {
				ctorFieldIdx[i] = j
				found = true
				break
			}
		}
		if !found {
			return nil, fmt.Errorf("%d:%d: constructor field '%s' not in field specs", expr.Line, expr.Col, cf)
		}
	}
	rt := &RecordType{Name: typeName.StrVal, Fields: fieldNames}
	nf := len(fieldNames)
	env.Set(ctorName.StrVal, makeGoFunc(func(args []*Value) (*Value, error) {
		if len(args) != len(ctorFieldIdx) {
			return nil, fmt.Errorf("wrong number of arguments to constructor %s", ctorName.StrVal)
		}
		fields := make([]*Value, nf)
		for i, idx := range ctorFieldIdx {
			fields[idx] = args[i]
		}
		return &Value{Type: TypeRecord, RecordType: rt, RecordFields: fields}, nil
	}))
	env.Set(predExpr.StrVal, makeGoFunc(func(args []*Value) (*Value, error) {
		if len(args) != 1 {
			return nil, fmt.Errorf("wrong number of arguments to predicate %s", predExpr.StrVal)
		}
		if args[0].Type == TypeRecord && args[0].RecordType == rt {
			return True, nil
		}
		return False, nil
	}))
	for accessorName, fieldIdx := range accessorMap {
		idx := fieldIdx
		aName := accessorName
		env.Set(aName, makeGoFunc(func(args []*Value) (*Value, error) {
			if len(args) != 1 {
				return nil, fmt.Errorf("wrong number of arguments to accessor %s", aName)
			}
			if args[0].Type != TypeRecord || args[0].RecordType != rt {
				return nil, fmt.Errorf("accessor %s applied to wrong type", aName)
			}
			return args[0].RecordFields[idx], nil
		}))
	}
	return Void, nil
}

func evalDo(expr *Expr, env *Env) (*Value, error) {
	if len(expr.Elements) < 3 {
		return nil, fmt.Errorf("%d:%d: 'do' requires variable bindings and test", expr.Line, expr.Col)
	}
	varsExpr := expr.Elements[1]
	testExpr := expr.Elements[2]
	bodyExprs := expr.Elements[3:]
	if varsExpr.Type != ExprList {
		return nil, fmt.Errorf("%d:%d: 'do' variable bindings must be a list", expr.Line, expr.Col)
	}
	if testExpr.Type != ExprList || len(testExpr.Elements) < 1 {
		return nil, fmt.Errorf("%d:%d: 'do' test clause must be a list", expr.Line, expr.Col)
	}
	type doVar struct {
		name    string
		stepIdx int
	}
	vars := make([]doVar, len(varsExpr.Elements))
	doEnv := NewEnv(env)
	for i, v := range varsExpr.Elements {
		if v.Type != ExprList || len(v.Elements) < 2 || len(v.Elements) > 3 {
			return nil, fmt.Errorf("%d:%d: invalid do variable spec", expr.Line, expr.Col)
		}
		if v.Elements[0].Type != ExprSymbol {
			return nil, fmt.Errorf("%d:%d: do variable name must be a symbol", expr.Line, expr.Col)
		}
		vars[i].name = v.Elements[0].StrVal
		if len(v.Elements) == 3 {
			vars[i].stepIdx = i
		} else {
			vars[i].stepIdx = -1
		}
		initVal, err := Eval(v.Elements[1], env)
		if err != nil {
			return nil, err
		}
		doEnv.Set(vars[i].name, initVal)
	}
	for {
		testVal, err := Eval(testExpr.Elements[0], doEnv)
		if err != nil {
			return nil, err
		}
		if isTruthy(testVal) {
			if len(testExpr.Elements) == 1 {
				return Void, nil
			}
			var result *Value
			for _, e := range testExpr.Elements[1:] {
				result, err = Eval(e, doEnv)
				if err != nil {
					return nil, err
				}
			}
			return result, nil
		}
		for _, b := range bodyExprs {
			_, err := Eval(b, doEnv)
			if err != nil {
				return nil, err
			}
		}
		newVals := make([]*Value, len(vars))
		for i, v := range vars {
			if v.stepIdx >= 0 {
				stepExpr := varsExpr.Elements[i].Elements[2]
				newVals[i], err = Eval(stepExpr, doEnv)
				if err != nil {
					return nil, err
				}
			}
		}
		for i, v := range vars {
			if v.stepIdx >= 0 {
				doEnv.Set(v.name, newVals[i])
			}
		}
	}
}
