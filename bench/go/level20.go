package ming

type raisedSignal struct {
	value   value
	callPos position
}

type exceptionHandlerKind int

const (
	exceptionHandlerProcedure exceptionHandlerKind = iota
	exceptionHandlerGuard
)

type exceptionHandlerFrame struct {
	kind        exceptionHandlerKind
	winds       []*dynamicWindFrame
	k           evalContinuation
	handlerProc value
	scope       *env
	variable    bindingName
	clauses     []expr
	callPos     position
}

func copyExceptionHandlerFrames(frames []*exceptionHandlerFrame) []*exceptionHandlerFrame {
	if len(frames) == 0 {
		return nil
	}

	copied := make([]*exceptionHandlerFrame, len(frames))
	copy(copied, frames)
	return copied
}

func builtinRaise(_ *interpreter, _ []value, callPos position) (value, error) {
	return nil, newEvalError(ErrSyntax, "raise requires continuation-aware evaluation", callPos)
}

func builtinWithExceptionHandler(_ *interpreter, _ []value, callPos position) (value, error) {
	return nil, newEvalError(ErrSyntax, "with-exception-handler requires continuation-aware evaluation", callPos)
}

func (it *interpreter) applyRaiseWithContinuations(args []value, callPos position) evalResult {
	if len(args) != 1 {
		return doneError(wrongArgCount(callPos, "raise", "expected exactly 1 argument"))
	}
	return doneRaised(&raisedSignal{
		value:   args[0],
		callPos: callPos,
	})
}

func (it *interpreter) applyWithExceptionHandlerWithContinuations(args []value, callPos position, k evalContinuation) evalResult {
	if len(args) != 2 {
		return doneError(wrongArgCount(callPos, "with-exception-handler", "expected exactly 2 arguments"))
	}

	frame := &exceptionHandlerFrame{
		kind:        exceptionHandlerProcedure,
		winds:       copyDynamicWindFrames(it.dynamicWinds),
		k:           k,
		handlerProc: args[0],
		callPos:     callPos,
	}
	it.exceptionHandlers = append(it.exceptionHandlers, frame)

	return callEval(func() evalResult {
		return it.applyProcedureWithContinuations(args[1], nil, callPos, func(result value) evalResult {
			it.popExceptionHandlerFrame(frame)
			return continueEval(k, result)
		})
	})
}

func (it *interpreter) evalGuardWithContinuations(scope *env, list *listExpr, k evalContinuation) evalResult {
	if len(list.elements) < 3 {
		return doneError(newEvalError(ErrSyntax, "guard: expected variable, clauses, and body", list.at))
	}

	spec, ok := list.elements[1].(*listExpr)
	if !ok || len(spec.elements) == 0 {
		return doneError(newEvalError(ErrSyntax, "guard: expected variable and clauses", list.elements[1].pos()))
	}

	sym, ok := spec.elements[0].(*symbolExpr)
	if !ok || sym.name == "." {
		return doneError(newEvalError(ErrSyntax, "guard: expected exception variable", spec.elements[0].pos()))
	}

	frame := &exceptionHandlerFrame{
		kind:     exceptionHandlerGuard,
		winds:    copyDynamicWindFrames(it.dynamicWinds),
		k:        k,
		scope:    scope,
		variable: bindingName{name: sym.name, key: sym.key},
		clauses:  spec.elements[1:],
		callPos:  list.at,
	}
	it.exceptionHandlers = append(it.exceptionHandlers, frame)

	return callEval(func() evalResult {
		return it.evalSequenceWithContinuations(scope, list.elements[2:], func(result value) evalResult {
			it.popExceptionHandlerFrame(frame)
			return continueEval(k, result)
		})
	})
}

func (it *interpreter) popExceptionHandlerFrame(frame *exceptionHandlerFrame) {
	if len(it.exceptionHandlers) == 0 || it.exceptionHandlers[len(it.exceptionHandlers)-1] != frame {
		return
	}
	it.exceptionHandlers = it.exceptionHandlers[:len(it.exceptionHandlers)-1]
}

func (it *interpreter) dispatchRaised(sig *raisedSignal) evalResult {
	if len(it.exceptionHandlers) == 0 {
		return doneError(it.uncaughtRaisedError(sig))
	}

	frame := it.exceptionHandlers[len(it.exceptionHandlers)-1]
	it.exceptionHandlers = it.exceptionHandlers[:len(it.exceptionHandlers)-1]

	return it.switchDynamicWinds(frame.winds, func() evalResult {
		switch frame.kind {
		case exceptionHandlerProcedure:
			return callEval(func() evalResult {
				return it.applyProcedureWithContinuations(frame.handlerProc, []value{sig.value}, frame.callPos, func(result value) evalResult {
					return continueEval(frame.k, result)
				})
			})
		case exceptionHandlerGuard:
			handlerEnv := newEnv(frame.scope)
			it.defineBindingName(handlerEnv, frame.variable, sig.value)
			return it.evalGuardClausesWithContinuations(handlerEnv, frame.clauses, sig, frame.k)
		default:
			return doneError(newEvalError(ErrRaised, "unknown exception handler", frame.callPos))
		}
	})
}

func (it *interpreter) evalGuardClausesWithContinuations(scope *env, clauses []expr, original *raisedSignal, k evalContinuation) evalResult {
	if len(clauses) == 0 {
		return doneRaised(original)
	}

	var evalClause func(int) evalResult
	evalClause = func(index int) evalResult {
		if index >= len(clauses) {
			return doneRaised(original)
		}

		clauseExpr := clauses[index]
		clause, ok := clauseExpr.(*listExpr)
		if !ok || len(clause.elements) == 0 {
			return doneError(newEvalError(ErrSyntax, "guard: expected non-empty clause", clauseExpr.pos()))
		}

		if sym, ok := clause.elements[0].(*symbolExpr); ok && sym.name == "else" {
			if index != len(clauses)-1 {
				return doneError(newEvalError(ErrSyntax, "guard: else clause must be last", sym.at))
			}
			if len(clause.elements) == 1 {
				return continueEval(k, voidValue{})
			}
			return callEval(func() evalResult {
				return it.evalSequenceWithContinuations(scope, clause.elements[1:], k)
			})
		}

		return callEval(func() evalResult {
			return it.evalWithContinuations(clause.elements[0], scope, func(test value) evalResult {
				if !isTruthy(test) {
					return callEval(func() evalResult {
						return evalClause(index + 1)
					})
				}
				if len(clause.elements) == 1 {
					return continueEval(k, test)
				}
				return callEval(func() evalResult {
					return it.evalSequenceWithContinuations(scope, clause.elements[1:], k)
				})
			})
		})
	}

	return evalClause(0)
}

func (it *interpreter) uncaughtRaisedError(sig *raisedSignal) error {
	message, err := formatDisplayValue(sig.value)
	if err != nil {
		return err
	}
	return newEvalError(ErrRaised, "uncaught exception: "+message, sig.callPos)
}
