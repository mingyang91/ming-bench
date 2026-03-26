package ming

import "fmt"

const (
	level20RaiseKey                = "__level20_raise__"
	level20WithExceptionHandlerKey = "__level20_with_exception_handler__"
)

type raisedError struct {
	Value any
	Line  int
	Col   int
}

func (e *raisedError) Error() string {
	return fmt.Sprintf("uncaught exception: %s", formatValue(e.Value))
}

type exceptionHandlerFrame struct {
	parent  *exceptionHandlerFrame
	handler any
	target  any
	dynamic *dynamicWindFrame
}

type exceptionHandlerReturnProc struct {
	runtime *runtimeState
	frame   *exceptionHandlerFrame
}

type exceptionHandlerInvokeProc struct {
	runtime *runtimeState
	frame   *exceptionHandlerFrame
}

type uncaughtExceptionProc struct {
	raised *raisedError
}

func normalizeRaisedError(err error) error {
	raisedErr, ok := err.(*raisedError)
	if !ok {
		return err
	}

	message := fmt.Sprintf("uncaught exception: %s", formatValue(raisedErr.Value))
	if raisedErr.Line > 0 && raisedErr.Col > 0 {
		return &EvalError{
			Message: message,
			Line:    raisedErr.Line,
			Col:     raisedErr.Col,
		}
	}
	return &EvalError{Message: message}
}

func builtinRaise(args []any) (any, error) {
	if len(args) != 1 {
		return nil, &EvalError{Message: "raise expects exactly 1 argument"}
	}
	return nil, &raisedError{Value: args[0]}
}

func builtinWithExceptionHandler(args []any) (any, error) {
	if len(args) != 2 {
		return nil, &EvalError{Message: "with-exception-handler expects exactly 2 arguments"}
	}

	value, err := applyProcedure(args[1], nil)
	if err == nil {
		return value, nil
	}

	raisedErr, ok := err.(*raisedError)
	if !ok {
		return nil, err
	}

	return applyProcedure(args[0], []any{raisedErr.Value})
}

func desugarGuard(args []any, pos sourcePos) (any, error) {
	if len(args) < 2 {
		return nil, pos.errorf("guard expects a binding spec and a body")
	}

	bindingSpec, ok := args[0].(listExpr)
	if !ok || len(bindingSpec.elements) == 0 {
		return nil, exprSourcePos(args[0]).errorf("guard binding spec must be a non-empty list")
	}

	exnName, ok := bindingSpec.elements[0].(symbolExpr)
	if !ok {
		return nil, exprSourcePos(bindingSpec.elements[0]).errorf("guard variable must be a symbol")
	}

	handlerBody := any(listExpr{
		elements: []any{
			symbolExpr{name: "raise", key: level20RaiseKey, pos: pos},
			exnName,
		},
		pos: pos,
	})

	if len(bindingSpec.elements) > 1 {
		clauses := append([]any(nil), bindingSpec.elements[1:]...)
		if !guardHasElseClause(clauses) {
			clauses = append(clauses, listExpr{
				elements: []any{
					symbolExpr{name: "else", pos: pos},
					listExpr{
						elements: []any{
							symbolExpr{name: "raise", key: level20RaiseKey, pos: pos},
							exnName,
						},
						pos: pos,
					},
				},
				pos: pos,
			})
		}

		handlerElements := make([]any, 0, len(clauses)+1)
		handlerElements = append(handlerElements, symbolExpr{name: "cond", pos: pos})
		handlerElements = append(handlerElements, clauses...)
		handlerBody = listExpr{elements: handlerElements, pos: pos}
	}

	handlerExpr := listExpr{
		elements: []any{
			symbolExpr{name: "lambda", pos: pos},
			listExpr{elements: []any{exnName}, pos: pos},
			handlerBody,
		},
		pos: pos,
	}

	thunkElements := make([]any, 0, len(args)+2)
	thunkElements = append(thunkElements,
		symbolExpr{name: "lambda", pos: pos},
		listExpr{elements: nil, pos: pos},
	)
	thunkElements = append(thunkElements, args[1:]...)
	thunkExpr := listExpr{elements: thunkElements, pos: pos}

	return listExpr{
		elements: []any{
			symbolExpr{name: "with-exception-handler", key: level20WithExceptionHandlerKey, pos: pos},
			handlerExpr,
			thunkExpr,
		},
		pos: pos,
	}, nil
}

func guardHasElseClause(clauses []any) bool {
	if len(clauses) == 0 {
		return false
	}

	lastClause, ok := clauses[len(clauses)-1].(listExpr)
	if !ok || len(lastClause.elements) == 0 {
		return false
	}

	head, ok := lastClause.elements[0].(symbolExpr)
	return ok && head.name == "else"
}

func prepareRaiseCallCPS(args []any, runtime *runtimeState) (any, *tailEvalState, error) {
	if len(args) != 1 {
		return nil, nil, &EvalError{Message: "raise expects exactly 1 argument"}
	}
	return prepareRaisedValue(runtime, &raisedError{Value: args[0]})
}

func prepareWithExceptionHandlerCallCPS(args []any, k any, runtime *runtimeState) (any, *tailEvalState, error) {
	if len(args) != 2 {
		return nil, nil, &EvalError{Message: "with-exception-handler expects exactly 2 arguments"}
	}

	frame := &exceptionHandlerFrame{
		parent:  runtime.exceptionHandler,
		handler: args[0],
		target:  k,
		dynamic: runtime.dynamic,
	}
	runtime.exceptionHandler = frame

	value, next, err := prepareApplyCPSCall(args[1], nil, exceptionHandlerReturnProc{
		runtime: runtime,
		frame:   frame,
	}, runtime)
	if err != nil {
		runtime.exceptionHandler = frame.parent
		return nil, nil, err
	}
	return value, next, nil
}

func prepareExceptionHandlerReturnCall(callable exceptionHandlerReturnProc, args []any) (any, *tailEvalState, error) {
	if len(args) != 1 {
		return nil, nil, &EvalError{Message: "exception handler return continuation expects exactly 1 argument"}
	}

	if callable.runtime.exceptionHandler == callable.frame {
		callable.runtime.exceptionHandler = callable.frame.parent
	}
	return prepareInvokeContinuationTarget(callable.runtime, callable.frame.target, args[0])
}

func prepareExceptionHandlerInvokeCall(callable exceptionHandlerInvokeProc, args []any) (any, *tailEvalState, error) {
	if len(args) != 1 {
		return nil, nil, &EvalError{Message: "exception handler continuation expects exactly 1 argument"}
	}

	return prepareApplyCPSCall(callable.frame.handler, []any{args[0]}, callable.frame.target, callable.runtime)
}

func prepareUncaughtExceptionCall(callable uncaughtExceptionProc, args []any) (any, *tailEvalState, error) {
	if len(args) != 1 {
		return nil, nil, &EvalError{Message: "uncaught exception continuation expects exactly 1 argument"}
	}
	return nil, nil, callable.raised
}

func prepareRaisedValue(runtime *runtimeState, raisedErr *raisedError) (any, *tailEvalState, error) {
	frame := runtime.exceptionHandler
	if frame == nil {
		exits, enters := dynamicWindTransition(runtime.dynamic, nil)
		if len(exits) == 0 && len(enters) == 0 {
			return nil, nil, raisedErr
		}
		return prepareDynamicWindTransitionStep(runtime, exits, enters, nil, uncaughtExceptionProc{raised: raisedErr}, raisedErr.Value)
	}

	runtime.exceptionHandler = frame.parent
	exits, enters := dynamicWindTransition(runtime.dynamic, frame.dynamic)
	return prepareDynamicWindTransitionStep(runtime, exits, enters, frame.dynamic, exceptionHandlerInvokeProc{
		runtime: runtime,
		frame:   frame,
	}, raisedErr.Value)
}
