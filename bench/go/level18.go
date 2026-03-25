package ming

import "fmt"

type evalResult struct {
	thunk  func() evalResult
	values []value
	err    error
	raised *raisedSignal
	done   bool
}

type evalContinuation func([]value) evalResult

type valueListContinuation func([]value) evalResult

type continuationProc struct {
	k        evalContinuation
	winds    []*dynamicWindFrame
	handlers []*exceptionHandlerFrame
}

func doneEval(values []value, err error) evalResult {
	return evalResult{
		values: copyValueSlice(values),
		err:    err,
		done:   true,
	}
}

func doneValue(v value) evalResult {
	return doneEval([]value{v}, nil)
}

func doneValues(values []value) evalResult {
	return doneEval(values, nil)
}

func doneError(err error) evalResult {
	return doneEval(nil, err)
}

func doneRaised(sig *raisedSignal) evalResult {
	return evalResult{
		raised: sig,
		done:   true,
	}
}

func callEval(fn func() evalResult) evalResult {
	return evalResult{thunk: fn}
}

func continueEval(k evalContinuation, v value) evalResult {
	return callEval(func() evalResult {
		return k([]value{v})
	})
}

func continueValues(k evalContinuation, values []value) evalResult {
	copied := copyValueSlice(values)
	return callEval(func() evalResult {
		return k(copied)
	})
}

func (it *interpreter) runEval(result evalResult) ([]value, error) {
	for !result.done {
		result = result.thunk()
	}
	for result.raised != nil {
		result = it.dispatchRaised(result.raised)
		for !result.done {
			result = result.thunk()
		}
	}
	return result.values, result.err
}

func programNeedsContinuationEvaluator(exprs []expr) bool {
	for _, node := range exprs {
		if exprNeedsContinuationEvaluator(node) {
			return true
		}
	}
	return false
}

func exprNeedsContinuationEvaluator(node expr) bool {
	switch current := node.(type) {
	case *symbolExpr:
		return isContinuationEvaluatorSymbol(current.name)
	case *listExpr:
		if len(current.elements) == 2 {
			if head, ok := current.elements[0].(*symbolExpr); ok && head.name == "quote" {
				return false
			}
		}
		for _, element := range current.elements {
			if exprNeedsContinuationEvaluator(element) {
				return true
			}
		}
	}
	return false
}

func isContinuationEvaluatorSymbol(name string) bool {
	return isCallCCBuiltinName(name) || name == "guard" || name == "raise" || name == "with-exception-handler" || name == "values" || name == "call-with-values"
}

func isCallCCBuiltinName(name string) bool {
	return name == "call/cc" || name == "call-with-current-continuation"
}

func builtinCallCC(_ *interpreter, _ []value, callPos position) (value, error) {
	return nil, newEvalError(ErrSyntax, "call/cc requires continuation-aware evaluation", callPos)
}

func haltContinuation(values []value) evalResult {
	return doneValues(values)
}

func appendCopiedValue(values []value, v value) []value {
	result := make([]value, len(values)+1)
	copy(result, values)
	result[len(values)] = v
	return result
}

func (it *interpreter) evalProgramWithContinuations(exprs []expr) (value, error) {
	if len(exprs) == 0 {
		return nil, newEvalError(ErrSyntax, "expected expression", position{line: 1, column: 1})
	}

	values, err := it.runEval(it.evalSequenceWithContinuations(it.global, exprs, haltContinuation))
	if err != nil {
		return nil, err
	}
	return expectSingleValue(values, exprs[len(exprs)-1].pos())
}

func (it *interpreter) evalSequenceWithContinuations(scope *env, exprs []expr, k evalContinuation) evalResult {
	if len(exprs) == 0 {
		return continueEval(k, voidValue{})
	}
	if len(exprs) == 1 {
		return callEval(func() evalResult {
			return it.evalWithContinuations(exprs[0], scope, k)
		})
	}
	return callEval(func() evalResult {
		return it.evalWithContinuations(exprs[0], scope, func(_ []value) evalResult {
			return callEval(func() evalResult {
				return it.evalSequenceWithContinuations(scope, exprs[1:], k)
			})
		})
	})
}

func (it *interpreter) evalExprListRightToLeftWithContinuations(exprs []expr, scope *env, k valueListContinuation) evalResult {
	if len(exprs) == 0 {
		return callEval(func() evalResult {
			return k(nil)
		})
	}

	last := exprs[len(exprs)-1]
	return callEval(func() evalResult {
		return it.evalWithContinuations(last, scope, singleValueContinuation(last.pos(), func(lastValue value) evalResult {
			return callEval(func() evalResult {
				return it.evalExprListRightToLeftWithContinuations(exprs[:len(exprs)-1], scope, func(prefix []value) evalResult {
					return callEval(func() evalResult {
						return k(appendCopiedValue(prefix, lastValue))
					})
				})
			})
		}))
	})
}

func (it *interpreter) evalBindingValuesWithContinuations(bindings []letBinding, scope *env, k valueListContinuation) evalResult {
	if len(bindings) == 0 {
		return callEval(func() evalResult {
			return k(nil)
		})
	}

	last := bindings[len(bindings)-1]
	return callEval(func() evalResult {
		return it.evalWithContinuations(last.init, scope, singleValueContinuation(last.init.pos(), func(lastValue value) evalResult {
			return callEval(func() evalResult {
				return it.evalBindingValuesWithContinuations(bindings[:len(bindings)-1], scope, func(prefix []value) evalResult {
					return callEval(func() evalResult {
						return k(appendCopiedValue(prefix, lastValue))
					})
				})
			})
		}))
	})
}

func (it *interpreter) evalWithContinuations(node expr, scope *env, k evalContinuation) evalResult {
	if err := it.consumeEvalStep(node.pos()); err != nil {
		return doneError(err)
	}

	switch current := node.(type) {
	case *intExpr:
		return continueEval(k, current.value)
	case *rationalExpr:
		return continueEval(k, current.value)
	case *inexactExpr:
		return continueEval(k, current.value)
	case *boolExpr:
		return continueEval(k, current.value)
	case *stringExpr:
		return continueEval(k, newStringValue(current.value))
	case *charExpr:
		return continueEval(k, charValue(current.value))
	case *symbolExpr:
		resolved, ok := it.lookupSymbol(scope, current)
		if !ok {
			return doneError(newEvalError(ErrUnboundVariable, fmt.Sprintf("unbound variable: %s", current.name), current.at))
		}
		if pending, ok := resolved.(*uninitializedValue); ok {
			return doneError(newEvalError(ErrUnboundVariable, fmt.Sprintf("uninitialized variable: %s", pending.name), current.at))
		}
		if isSyntaxTransformer(resolved) {
			return doneError(newEvalError(ErrSyntax, fmt.Sprintf("cannot use syntax as value: %s", current.name), current.at))
		}
		return continueEval(k, resolved)
	case *listExpr:
		if len(current.elements) == 0 {
			return doneError(newEvalError(ErrSyntax, "cannot evaluate empty list", current.at))
		}

		if sym, ok := current.elements[0].(*symbolExpr); ok {
			switch sym.name {
			case "and":
				return it.evalAndWithContinuations(scope, current.elements[1:], k)
			case "or":
				return it.evalOrWithContinuations(scope, current.elements[1:], k)
			case "define":
				return it.evalDefineWithContinuations(scope, current, k)
			case "define-record-type":
				result, err := it.evalDefineRecordType(scope, current)
				if err != nil {
					return doneError(err)
				}
				return continueEval(k, result)
			case "define-syntax":
				result, err := it.evalDefineSyntax(scope, current)
				if err != nil {
					return doneError(err)
				}
				return continueEval(k, result)
			case "if":
				return it.evalIfWithContinuations(scope, current, k)
			case "quote":
				result, err := it.evalQuote(current)
				if err != nil {
					return doneError(err)
				}
				return continueEval(k, result)
			case "quasiquote":
				expanded, err := expandQuasiquoteForm(current)
				if err != nil {
					return doneError(err)
				}
				return callEval(func() evalResult {
					return it.evalWithContinuations(expanded, scope, k)
				})
			case "lambda":
				result, err := it.evalLambda(scope, current)
				if err != nil {
					return doneError(err)
				}
				return continueEval(k, result)
			case "case-lambda":
				result, err := it.evalCaseLambda(scope, current)
				if err != nil {
					return doneError(err)
				}
				return continueEval(k, result)
			case "set!":
				return it.evalSetWithContinuations(scope, current, k)
			case "begin":
				return it.evalSequenceWithContinuations(scope, current.elements[1:], k)
			case "cond":
				return it.evalCondWithContinuations(scope, current, k)
			case "let":
				return it.evalLetWithContinuations(scope, current, k)
			case "let*":
				return it.evalLetStarWithContinuations(scope, current, k)
			case "letrec":
				return it.evalLetRecWithContinuations(scope, current, false, k)
			case "letrec*":
				return it.evalLetRecWithContinuations(scope, current, true, k)
			case "case":
				return it.evalCaseWithContinuations(scope, current, k)
			case "do":
				return it.evalDoWithContinuations(scope, current, k)
			case "guard":
				return it.evalGuardWithContinuations(scope, current, k)
			}

			if expanded, ok, err := it.expandMacroCall(current, scope); ok || err != nil {
				if err != nil {
					return doneError(err)
				}
				return callEval(func() evalResult {
					return it.evalWithContinuations(expanded, scope, k)
				})
			}
		}

		return callEval(func() evalResult {
			return it.evalWithContinuations(current.elements[0], scope, singleValueContinuation(current.elements[0].pos(), func(operator value) evalResult {
				return callEval(func() evalResult {
					return it.evalExprListRightToLeftWithContinuations(current.elements[1:], scope, func(args []value) evalResult {
						return callEval(func() evalResult {
							return it.applyProcedureWithContinuations(operator, args, current.at, k)
						})
					})
				})
			}))
		})
	default:
		return doneError(newEvalError(ErrSyntax, "unknown expression", node.pos()))
	}
}

func (it *interpreter) evalAndWithContinuations(scope *env, args []expr, k evalContinuation) evalResult {
	if len(args) == 0 {
		return continueEval(k, true)
	}
	if len(args) == 1 {
		return callEval(func() evalResult {
			return it.evalWithContinuations(args[0], scope, k)
		})
	}
	return callEval(func() evalResult {
		return it.evalWithContinuations(args[0], scope, singleValueContinuation(args[0].pos(), func(result value) evalResult {
			if !isTruthy(result) {
				return continueEval(k, result)
			}
			return callEval(func() evalResult {
				return it.evalAndWithContinuations(scope, args[1:], k)
			})
		}))
	})
}

func (it *interpreter) evalOrWithContinuations(scope *env, args []expr, k evalContinuation) evalResult {
	if len(args) == 0 {
		return continueEval(k, false)
	}
	return callEval(func() evalResult {
		return it.evalWithContinuations(args[0], scope, singleValueContinuation(args[0].pos(), func(result value) evalResult {
			if isTruthy(result) {
				return continueEval(k, result)
			}
			return callEval(func() evalResult {
				return it.evalOrWithContinuations(scope, args[1:], k)
			})
		}))
	})
}

func (it *interpreter) evalDefineWithContinuations(scope *env, list *listExpr, k evalContinuation) evalResult {
	if len(list.elements) < 3 {
		return doneError(newEvalError(ErrSyntax, "define: expected a name and value", list.at))
	}

	switch target := list.elements[1].(type) {
	case *symbolExpr:
		if len(list.elements) != 3 {
			return doneError(newEvalError(ErrSyntax, "define: expected exactly one value expression", list.at))
		}
		return callEval(func() evalResult {
			return it.evalWithContinuations(list.elements[2], scope, singleValueContinuation(list.elements[2].pos(), func(result value) evalResult {
				it.defineSymbol(scope, target, result)
				return continueEval(k, voidValue{})
			}))
		})
	case *listExpr:
		if len(target.elements) == 0 {
			return doneError(newEvalError(ErrSyntax, "define: expected function name", target.at))
		}
		name, ok := target.elements[0].(*symbolExpr)
		if !ok {
			return doneError(newEvalError(ErrSyntax, "define: expected function name", target.elements[0].pos()))
		}
		params, rest, hasRest, err := parseParamNames(target.elements[1:])
		if err != nil {
			return doneError(err)
		}
		proc := &closureProc{
			name:    name.name,
			params:  params,
			rest:    rest,
			hasRest: hasRest,
			body:    list.elements[2:],
			env:     scope,
		}
		it.defineSymbol(scope, name, proc)
		return continueEval(k, voidValue{})
	default:
		return doneError(newEvalError(ErrSyntax, "define: expected a symbol or function signature", list.elements[1].pos()))
	}
}

func (it *interpreter) evalSetWithContinuations(scope *env, list *listExpr, k evalContinuation) evalResult {
	if len(list.elements) != 3 {
		return doneError(newEvalError(ErrSyntax, "set!: expected a name and value", list.at))
	}

	target, ok := list.elements[1].(*symbolExpr)
	if !ok {
		return doneError(newEvalError(ErrSyntax, "set!: expected variable name", list.elements[1].pos()))
	}

	binding, ok := it.lookupSymbolBinding(scope, target)
	if !ok {
		return doneError(newEvalError(ErrUnboundVariable, fmt.Sprintf("set!: unbound variable: %s", target.name), target.at))
	}

	return callEval(func() evalResult {
		return it.evalWithContinuations(list.elements[2], scope, singleValueContinuation(list.elements[2].pos(), func(result value) evalResult {
			binding.value = result
			return continueEval(k, voidValue{})
		}))
	})
}

func (it *interpreter) evalIfWithContinuations(scope *env, list *listExpr, k evalContinuation) evalResult {
	if len(list.elements) != 3 && len(list.elements) != 4 {
		return doneError(newEvalError(ErrSyntax, "if: expected a test, consequent, and optional alternate", list.at))
	}

	return callEval(func() evalResult {
		return it.evalWithContinuations(list.elements[1], scope, singleValueContinuation(list.elements[1].pos(), func(test value) evalResult {
			if isTruthy(test) {
				return callEval(func() evalResult {
					return it.evalWithContinuations(list.elements[2], scope, k)
				})
			}
			if len(list.elements) == 4 {
				return callEval(func() evalResult {
					return it.evalWithContinuations(list.elements[3], scope, k)
				})
			}
			return continueEval(k, voidValue{})
		}))
	})
}

func (it *interpreter) evalCondWithContinuations(scope *env, list *listExpr, k evalContinuation) evalResult {
	if len(list.elements) == 1 {
		return continueEval(k, voidValue{})
	}

	var evalClause func(int) evalResult
	evalClause = func(index int) evalResult {
		if index >= len(list.elements)-1 {
			return continueEval(k, voidValue{})
		}

		clauseExpr := list.elements[index+1]
		clause, ok := clauseExpr.(*listExpr)
		if !ok || len(clause.elements) == 0 {
			return doneError(newEvalError(ErrSyntax, "cond: expected non-empty clause", clauseExpr.pos()))
		}

		if sym, ok := clause.elements[0].(*symbolExpr); ok && sym.name == "else" {
			if index != len(list.elements)-2 {
				return doneError(newEvalError(ErrSyntax, "cond: else clause must be last", sym.at))
			}
			if len(clause.elements) == 1 {
				return continueEval(k, voidValue{})
			}
			return callEval(func() evalResult {
				return it.evalSequenceWithContinuations(scope, clause.elements[1:], k)
			})
		}

		recipient, hasArrow, err := condArrowRecipient(clause)
		if err != nil {
			return doneError(err)
		}

		return callEval(func() evalResult {
			return it.evalWithContinuations(clause.elements[0], scope, singleValueContinuation(clause.elements[0].pos(), func(test value) evalResult {
				if !isTruthy(test) {
					return callEval(func() evalResult {
						return evalClause(index + 1)
					})
				}
				if hasArrow {
					return callEval(func() evalResult {
						return it.evalWithContinuations(recipient, scope, singleValueContinuation(recipient.pos(), func(proc value) evalResult {
							return callEval(func() evalResult {
								return it.applyProcedureWithContinuations(proc, []value{test}, clause.at, k)
							})
						}))
					})
				}
				if len(clause.elements) == 1 {
					return continueEval(k, test)
				}
				return callEval(func() evalResult {
					return it.evalSequenceWithContinuations(scope, clause.elements[1:], k)
				})
			}))
		})
	}

	return evalClause(0)
}

func (it *interpreter) evalCaseWithContinuations(scope *env, list *listExpr, k evalContinuation) evalResult {
	if len(list.elements) < 2 {
		return doneError(newEvalError(ErrSyntax, "case: expected key and clauses", list.at))
	}

	return callEval(func() evalResult {
		return it.evalWithContinuations(list.elements[1], scope, singleValueContinuation(list.elements[1].pos(), func(key value) evalResult {
			clauses := list.elements[2:]
			for idx, clauseExpr := range clauses {
				clause, ok := clauseExpr.(*listExpr)
				if !ok || len(clause.elements) == 0 {
					return doneError(newEvalError(ErrSyntax, "case: expected non-empty clause", clauseExpr.pos()))
				}

				if sym, ok := clause.elements[0].(*symbolExpr); ok && sym.name == "else" {
					if idx != len(clauses)-1 {
						return doneError(newEvalError(ErrSyntax, "case: else clause must be last", sym.at))
					}
					if len(clause.elements) == 1 {
						return continueEval(k, voidValue{})
					}
					return callEval(func() evalResult {
						return it.evalSequenceWithContinuations(scope, clause.elements[1:], k)
					})
				}

				datums, ok := clause.elements[0].(*listExpr)
				if !ok {
					return doneError(newEvalError(ErrSyntax, "case: expected datum list", clause.elements[0].pos()))
				}

				matched := false
				for _, datumExpr := range datums.elements {
					datum, err := datumToValue(datumExpr)
					if err != nil {
						return doneError(err)
					}
					if caseDatumEqual(key, datum) {
						matched = true
						break
					}
				}
				if !matched {
					continue
				}

				if len(clause.elements) == 1 {
					return continueEval(k, voidValue{})
				}
				return callEval(func() evalResult {
					return it.evalSequenceWithContinuations(scope, clause.elements[1:], k)
				})
			}

			return continueEval(k, voidValue{})
		}))
	})
}

func (it *interpreter) evalLetWithContinuations(scope *env, list *listExpr, k evalContinuation) evalResult {
	if len(list.elements) < 3 {
		return doneError(newEvalError(ErrSyntax, "let: expected bindings and body", list.at))
	}

	bindingIndex := 1
	var name *symbolExpr
	if sym, ok := list.elements[1].(*symbolExpr); ok {
		name = sym
		bindingIndex = 2
		if len(list.elements) < 4 {
			return doneError(newEvalError(ErrSyntax, "let: expected named bindings and body", list.at))
		}
	}

	bindingList, ok := list.elements[bindingIndex].(*listExpr)
	if !ok {
		return doneError(newEvalError(ErrSyntax, "let: expected binding list", list.elements[bindingIndex].pos()))
	}

	bindings, err := parseLetBindings(bindingList)
	if err != nil {
		return doneError(err)
	}

	body := list.elements[bindingIndex+1:]
	if len(body) == 0 {
		return doneError(newEvalError(ErrSyntax, "let: expected body", list.at))
	}

	return callEval(func() evalResult {
		return it.evalBindingValuesWithContinuations(bindings, scope, func(args []value) evalResult {
			params := make([]bindingName, len(bindings))
			for i, binding := range bindings {
				params[i] = binding.name
			}

			if name == nil {
				letEnv := newEnv(scope)
				for i, param := range params {
					it.defineBindingName(letEnv, param, args[i])
				}
				return callEval(func() evalResult {
					return it.evalSequenceWithContinuations(letEnv, body, k)
				})
			}

			letEnv := newEnv(scope)
			proc := &closureProc{
				name:   name.name,
				params: params,
				body:   body,
				env:    letEnv,
			}
			it.defineSymbol(letEnv, name, proc)
			return callEval(func() evalResult {
				return it.applyProcedureWithContinuations(proc, args, list.at, k)
			})
		})
	})
}

func (it *interpreter) evalLetStarWithContinuations(scope *env, list *listExpr, k evalContinuation) evalResult {
	if len(list.elements) < 3 {
		return doneError(newEvalError(ErrSyntax, "let*: expected bindings and body", list.at))
	}

	bindingList, ok := list.elements[1].(*listExpr)
	if !ok {
		return doneError(newEvalError(ErrSyntax, "let*: expected binding list", list.elements[1].pos()))
	}

	bindings, err := parseLetBindings(bindingList)
	if err != nil {
		return doneError(err)
	}

	body := list.elements[2:]
	if len(body) == 0 {
		return doneError(newEvalError(ErrSyntax, "let*: expected body", list.at))
	}

	letEnv := newEnv(scope)
	var bind func(int) evalResult
	bind = func(index int) evalResult {
		if index >= len(bindings) {
			return callEval(func() evalResult {
				return it.evalSequenceWithContinuations(letEnv, body, k)
			})
		}

		binding := bindings[index]
		return callEval(func() evalResult {
			return it.evalWithContinuations(binding.init, letEnv, singleValueContinuation(binding.init.pos(), func(current value) evalResult {
				it.defineBindingName(letEnv, binding.name, current)
				return callEval(func() evalResult {
					return bind(index + 1)
				})
			}))
		})
	}

	return bind(0)
}

func (it *interpreter) evalLetRecWithContinuations(scope *env, list *listExpr, sequential bool, k evalContinuation) evalResult {
	if len(list.elements) < 3 {
		return doneError(newEvalError(ErrSyntax, "letrec: expected bindings and body", list.at))
	}

	bindingList, ok := list.elements[1].(*listExpr)
	if !ok {
		return doneError(newEvalError(ErrSyntax, "letrec: expected binding list", list.elements[1].pos()))
	}

	bindings, err := parseLetBindings(bindingList)
	if err != nil {
		return doneError(err)
	}

	body := list.elements[2:]
	if len(body) == 0 {
		return doneError(newEvalError(ErrSyntax, "letrec: expected body", list.at))
	}

	letEnv := newEnv(scope)
	defs := make([]*binding, len(bindings))
	for i, bindingSpec := range bindings {
		defs[i] = it.defineBindingName(letEnv, bindingSpec.name, &uninitializedValue{name: bindingSpec.name.name})
	}

	if sequential {
		var initialize func(int) evalResult
		initialize = func(index int) evalResult {
			if index >= len(bindings) {
				return callEval(func() evalResult {
					return it.evalSequenceWithContinuations(letEnv, body, k)
				})
			}

			bindingSpec := bindings[index]
			return callEval(func() evalResult {
				return it.evalWithContinuations(bindingSpec.init, letEnv, singleValueContinuation(bindingSpec.init.pos(), func(current value) evalResult {
					defs[index].value = current
					return callEval(func() evalResult {
						return initialize(index + 1)
					})
				}))
			})
		}
		return initialize(0)
	}

	return callEval(func() evalResult {
		return it.evalBindingValuesWithContinuations(bindings, letEnv, func(values []value) evalResult {
			for i, current := range values {
				defs[i].value = current
			}
			return callEval(func() evalResult {
				return it.evalSequenceWithContinuations(letEnv, body, k)
			})
		})
	})
}

func (it *interpreter) evalDoStepValuesWithContinuations(bindings []doBinding, defs []*binding, scope *env, index int, k valueListContinuation) evalResult {
	if index < 0 {
		return callEval(func() evalResult {
			return k(nil)
		})
	}

	currentStep := bindings[index].step
	if currentStep == nil {
		return callEval(func() evalResult {
			return it.evalDoStepValuesWithContinuations(bindings, defs, scope, index-1, func(prefix []value) evalResult {
				return callEval(func() evalResult {
					return k(appendCopiedValue(prefix, defs[index].value))
				})
			})
		})
	}

	return callEval(func() evalResult {
		return it.evalWithContinuations(currentStep, scope, singleValueContinuation(currentStep.pos(), func(nextValue value) evalResult {
			return callEval(func() evalResult {
				return it.evalDoStepValuesWithContinuations(bindings, defs, scope, index-1, func(prefix []value) evalResult {
					return callEval(func() evalResult {
						return k(appendCopiedValue(prefix, nextValue))
					})
				})
			})
		}))
	})
}

func (it *interpreter) evalDoWithContinuations(scope *env, list *listExpr, k evalContinuation) evalResult {
	if len(list.elements) < 3 {
		return doneError(newEvalError(ErrSyntax, "do: expected bindings, test, and body", list.at))
	}

	bindingList, ok := list.elements[1].(*listExpr)
	if !ok {
		return doneError(newEvalError(ErrSyntax, "do: expected binding list", list.elements[1].pos()))
	}

	testClause, ok := list.elements[2].(*listExpr)
	if !ok || len(testClause.elements) == 0 {
		return doneError(newEvalError(ErrSyntax, "do: expected test clause", list.elements[2].pos()))
	}

	bindings, err := parseDoBindings(bindingList)
	if err != nil {
		return doneError(err)
	}

	initExprs := make([]expr, len(bindings))
	for i, binding := range bindings {
		initExprs[i] = binding.init
	}

	commands := list.elements[3:]
	return callEval(func() evalResult {
		return it.evalExprListRightToLeftWithContinuations(initExprs, scope, func(values []value) evalResult {
			loopEnv := newEnv(scope)
			defs := make([]*binding, len(bindings))
			for i, binding := range bindings {
				defs[i] = it.defineBindingName(loopEnv, binding.name, values[i])
			}

			var loop func() evalResult
			loop = func() evalResult {
				return callEval(func() evalResult {
					return it.evalWithContinuations(testClause.elements[0], loopEnv, singleValueContinuation(testClause.elements[0].pos(), func(test value) evalResult {
						if isTruthy(test) {
							if len(testClause.elements) == 1 {
								return continueEval(k, voidValue{})
							}
							return callEval(func() evalResult {
								return it.evalSequenceWithContinuations(loopEnv, testClause.elements[1:], k)
							})
						}

						return callEval(func() evalResult {
							return it.evalSequenceWithContinuations(loopEnv, commands, func(_ []value) evalResult {
								return callEval(func() evalResult {
									return it.evalDoStepValuesWithContinuations(bindings, defs, loopEnv, len(bindings)-1, func(nextValues []value) evalResult {
										for i, next := range nextValues {
											defs[i].value = next
										}
										return callEval(func() evalResult {
											return loop()
										})
									})
								})
							})
						})
					}))
				})
			}

			return loop()
		})
	})
}

func (it *interpreter) applyProcedureWithContinuations(proc value, args []value, callPos position, k evalContinuation) evalResult {
	switch proc := proc.(type) {
	case *builtinProc:
		switch proc.name {
		case "apply":
			if len(args) < 2 {
				return doneError(wrongArgCount(callPos, "apply", "expected at least 2 arguments"))
			}

			tailArgs, err := listToSlice(args[len(args)-1], callPos)
			if err != nil {
				return doneError(err)
			}

			appliedArgs := make([]value, 0, len(args)-2+len(tailArgs))
			appliedArgs = append(appliedArgs, args[1:len(args)-1]...)
			appliedArgs = append(appliedArgs, tailArgs...)
			return callEval(func() evalResult {
				return it.applyProcedureWithContinuations(args[0], appliedArgs, callPos, k)
			})
		case "map":
			return it.applyMapWithContinuations(args, callPos, k)
		case "for-each":
			return it.applyForEachWithContinuations(args, callPos, k)
		case "dynamic-wind":
			return it.applyDynamicWindWithContinuations(args, callPos, k)
		case "raise":
			return it.applyRaiseWithContinuations(args, callPos)
		case "with-exception-handler":
			return it.applyWithExceptionHandlerWithContinuations(args, callPos, k)
		case "values":
			return continueValues(k, args)
		case "call-with-values":
			if len(args) != 2 {
				return doneError(wrongArgCount(callPos, "call-with-values", "expected exactly 2 arguments"))
			}
			return callEval(func() evalResult {
				return it.applyProcedureWithContinuations(args[0], nil, callPos, func(produced []value) evalResult {
					return callEval(func() evalResult {
						return it.applyProcedureWithContinuations(args[1], produced, callPos, k)
					})
				})
			})
		default:
			if isCallCCBuiltinName(proc.name) {
				if len(args) != 1 {
					return doneError(wrongArgCount(callPos, proc.name, "expected exactly 1 argument"))
				}
				return callEval(func() evalResult {
					return it.applyProcedureWithContinuations(args[0], []value{&continuationProc{
						k:        k,
						winds:    copyDynamicWindFrames(it.dynamicWinds),
						handlers: copyExceptionHandlerFrames(it.exceptionHandlers),
					}}, callPos, k)
				})
			}

			result, err := proc.fn(it, args, callPos)
			if err != nil {
				return doneError(err)
			}
			return continueEval(k, result)
		}
	case *closureProc:
		if err := validateProcedureCall(callPos, closureCallName(proc), proc.params, proc.hasRest, args); err != nil {
			return doneError(err)
		}
		callEnv := it.newProcedureCallEnv(proc.env, proc.params, proc.rest, proc.hasRest, args)
		return callEval(func() evalResult {
			return it.evalSequenceWithContinuations(callEnv, proc.body, k)
		})
	case *caseLambdaProc:
		for _, clause := range proc.clauses {
			if procedureArityMatches(len(clause.params), clause.hasRest, len(args)) {
				callEnv := it.newProcedureCallEnv(proc.env, clause.params, clause.rest, clause.hasRest, args)
				return callEval(func() evalResult {
					return it.evalSequenceWithContinuations(callEnv, clause.body, k)
				})
			}
		}

		name := proc.name
		if name == "" {
			name = "case-lambda"
		}
		return doneError(wrongArgCount(callPos, name, fmt.Sprintf("no matching clause for %d arguments", len(args))))
	case *continuationProc:
		targetHandlers := copyExceptionHandlerFrames(proc.handlers)
		return it.switchDynamicWinds(proc.winds, func() evalResult {
			it.exceptionHandlers = targetHandlers
			return continueValues(proc.k, args)
		})
	default:
		return doneError(newEvalError(ErrNotProcedure, "attempted to call a non-procedure", callPos))
	}
}

func (it *interpreter) applyMapWithContinuations(args []value, callPos position, k evalContinuation) evalResult {
	if len(args) < 2 {
		return doneError(wrongArgCount(callPos, "map", "expected at least 2 arguments"))
	}

	argLists := make([][]value, 0, len(args)-1)
	expectedLen := -1
	for _, arg := range args[1:] {
		items, err := listToSlice(arg, callPos)
		if err != nil {
			return doneError(err)
		}
		if expectedLen == -1 {
			expectedLen = len(items)
		} else if len(items) != expectedLen {
			return doneError(newEvalError(ErrWrongArgCount, "map: expected lists of equal length", callPos))
		}
		argLists = append(argLists, items)
	}

	var loop func(int, []value) evalResult
	loop = func(index int, results []value) evalResult {
		if index >= expectedLen {
			return continueEval(k, buildList(results))
		}

		callArgs := make([]value, len(argLists))
		for i, items := range argLists {
			callArgs[i] = items[index]
		}
		return callEval(func() evalResult {
			return it.applyProcedureWithContinuations(args[0], callArgs, callPos, singleValueContinuation(callPos, func(result value) evalResult {
				return callEval(func() evalResult {
					return loop(index+1, appendCopiedValue(results, result))
				})
			}))
		})
	}

	return loop(0, nil)
}

func (it *interpreter) applyForEachWithContinuations(args []value, callPos position, k evalContinuation) evalResult {
	if len(args) < 2 {
		return doneError(wrongArgCount(callPos, "for-each", "expected at least 2 arguments"))
	}

	argLists := make([][]value, 0, len(args)-1)
	expectedLen := -1
	for _, arg := range args[1:] {
		items, err := listToSlice(arg, callPos)
		if err != nil {
			return doneError(err)
		}
		if expectedLen == -1 {
			expectedLen = len(items)
		} else if len(items) != expectedLen {
			return doneError(newEvalError(ErrWrongArgCount, "for-each: expected lists of equal length", callPos))
		}
		argLists = append(argLists, items)
	}

	var loop func(int) evalResult
	loop = func(index int) evalResult {
		if index >= expectedLen {
			return continueEval(k, voidValue{})
		}

		callArgs := make([]value, len(argLists))
		for i, items := range argLists {
			callArgs[i] = items[index]
		}
		return callEval(func() evalResult {
			return it.applyProcedureWithContinuations(args[0], callArgs, callPos, func(_ []value) evalResult {
				return callEval(func() evalResult {
					return loop(index + 1)
				})
			})
		})
	}

	return loop(0)
}
