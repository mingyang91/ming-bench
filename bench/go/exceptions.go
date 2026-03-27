package ming

type raisedSignal struct {
	value any
	pos   position
}

func (s *raisedSignal) Error() string {
	return "uncaught exception: " + formatValue(s.value)
}

type exceptionHandler struct {
	handler  any
	stack    []continuationFrame
	winds    []*dynamicWind
	handlers []*exceptionHandler
	pos      position
}

type withExceptionHandlerPopFrame struct {
	handler *exceptionHandler
}

type exceptionRaiseState struct {
	exiting []*dynamicWind
	handler *exceptionHandler
	value   any
	pos     position
}

type exceptionRaiseFrame struct {
	state *exceptionRaiseState
}

func cloneExceptionHandlers(handlers []*exceptionHandler) []*exceptionHandler {
	return append([]*exceptionHandler(nil), handlers...)
}

func normalizeInterpreterError(err error) error {
	if signal, ok := err.(*raisedSignal); ok {
		return newEvalError(signal.pos, "uncaught exception: %s", formatValue(signal.value))
	}
	return err
}

func expandGuardForm(args []expr, pos position) (expr, error) {
	if len(args) < 2 {
		return nil, newEvalError(pos, "guard expects a clause list and a body")
	}

	spec, ok := args[0].(*listExpr)
	if !ok || len(spec.elements) == 0 {
		return nil, newEvalError(args[0].exprPos(), "guard requires a condition variable and clauses")
	}

	name, ok := spec.elements[0].(*symbolExpr)
	if !ok {
		return nil, newEvalError(spec.elements[0].exprPos(), "guard requires a symbol condition variable")
	}

	clauses := append([]expr(nil), spec.elements[1:]...)
	if !guardHasElseClause(clauses) {
		clauses = append(clauses, &listExpr{
			pos: pos,
			elements: []expr{
				&symbolExpr{value: "else", pos: pos},
				&listExpr{
					pos: pos,
					elements: []expr{
						&symbolExpr{value: "raise", pos: pos},
						&symbolExpr{value: name.value, pos: name.pos},
					},
				},
			},
		})
	}

	condElements := make([]expr, 0, len(clauses)+1)
	condElements = append(condElements, &symbolExpr{value: "cond", pos: pos})
	condElements = append(condElements, clauses...)

	handlerLambda := &listExpr{
		pos: pos,
		elements: []expr{
			&symbolExpr{value: "lambda", pos: pos},
			&listExpr{
				pos: name.pos,
				elements: []expr{
					&symbolExpr{value: name.value, pos: name.pos},
				},
			},
			&listExpr{pos: pos, elements: condElements},
		},
	}

	thunkElements := make([]expr, 0, len(args)+1)
	thunkElements = append(thunkElements,
		&symbolExpr{value: "lambda", pos: pos},
		&listExpr{pos: pos},
	)
	thunkElements = append(thunkElements, args[1:]...)

	return &listExpr{
		pos: pos,
		elements: []expr{
			&symbolExpr{value: "with-exception-handler", pos: pos},
			handlerLambda,
			&listExpr{pos: pos, elements: thunkElements},
		},
	}, nil
}

func guardHasElseClause(clauses []expr) bool {
	for _, clauseExpr := range clauses {
		clause, ok := clauseExpr.(*listExpr)
		if !ok || len(clause.elements) == 0 {
			continue
		}
		if symbol, ok := clause.elements[0].(*symbolExpr); ok && symbol.value == "else" {
			return true
		}
	}
	return false
}

func (i *interpreter) popCurrentHandler(expected *exceptionHandler) error {
	if len(i.currentHandlers) == 0 || i.currentHandlers[len(i.currentHandlers)-1] != expected {
		return newEvalError(expected.pos, "internal error: exception handler stack mismatch")
	}
	i.currentHandlers = i.currentHandlers[:len(i.currentHandlers)-1]
	return nil
}

func (i *interpreter) startWithExceptionHandler(handler any, thunk any, pos position, stack []continuationFrame) (continuationControl, []continuationFrame, error) {
	if !isCallableValue(handler) || !isCallableValue(thunk) {
		return continuationControl{}, nil, newEvalError(pos, "attempt to call non-procedure")
	}

	entry := &exceptionHandler{
		handler:  handler,
		stack:    cloneContinuationStack(stack),
		winds:    cloneDynamicWinds(i.currentWinds),
		handlers: cloneExceptionHandlers(i.currentHandlers),
		pos:      pos,
	}

	i.currentHandlers = append(i.currentHandlers, entry)
	stack = append(stack, withExceptionHandlerPopFrame{handler: entry})
	return i.applyContinuationProcedure(thunk, nil, pos, stack)
}

func (i *interpreter) raiseContinuation(value any, pos position) (continuationControl, []continuationFrame, error) {
	if len(i.currentHandlers) == 0 {
		return continuationControl{}, nil, newEvalError(pos, "uncaught exception: %s", formatValue(value))
	}

	handler := i.currentHandlers[len(i.currentHandlers)-1]
	return i.startExceptionRaise(handler, value, pos)
}

func (i *interpreter) startExceptionRaise(handler *exceptionHandler, value any, pos position) (continuationControl, []continuationFrame, error) {
	targetWinds := cloneDynamicWinds(handler.winds)
	commonPrefix := sharedDynamicWindPrefixLen(i.currentWinds, targetWinds)

	exiting := make([]*dynamicWind, 0, len(i.currentWinds)-commonPrefix)
	for index := len(i.currentWinds) - 1; index >= commonPrefix; index-- {
		exiting = append(exiting, i.currentWinds[index])
	}

	state := &exceptionRaiseState{
		exiting: exiting,
		handler: handler,
		value:   value,
		pos:     pos,
	}

	return i.advanceExceptionRaise(state)
}

func (i *interpreter) advanceExceptionRaise(state *exceptionRaiseState) (continuationControl, []continuationFrame, error) {
	if len(state.exiting) > 0 {
		wind := state.exiting[0]
		state.exiting = state.exiting[1:]

		if err := i.popCurrentWind(wind); err != nil {
			return continuationControl{}, nil, err
		}

		stack := []continuationFrame{
			exceptionRaiseFrame{state: state},
		}
		return i.applyContinuationProcedure(wind.after, nil, wind.pos, stack)
	}

	i.currentWinds = cloneDynamicWinds(state.handler.winds)
	i.currentHandlers = cloneExceptionHandlers(state.handler.handlers)
	return i.applyContinuationProcedure(
		state.handler.handler,
		[]any{state.value},
		state.pos,
		cloneContinuationStack(state.handler.stack),
	)
}
