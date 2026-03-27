package ming

type exceptionHandlerFrame struct {
	parent *exceptionHandlerFrame
	handler procedure
	wind    *windFrame
	target  continuation
}

type withExceptionHandlerProc struct {
	name string
}

type raiseProc struct {
	name string
}

type guardHandlerProc struct {
	variable string
	clauses  []locatedExpr
	env      *env
}

type exceptionHandlerReturnFrame struct {
	handler *exceptionHandlerFrame
	next    continuation
}

func (*exceptionHandlerReturnFrame) acceptsMultipleValues() bool {
	return true
}

type raiseInvokeHandlerFrame struct {
	handler procedure
	pos     SourcePos
	next    continuation
}

type restoreHandlersFrame struct {
	handlers *exceptionHandlerFrame
	next     continuation
}

func (*restoreHandlersFrame) acceptsMultipleValues() bool {
	return true
}

type guardClauseFrame struct {
	clauses   []locatedExpr
	index     int
	env       *env
	exception value
	next      continuation
}

func (p withExceptionHandlerProc) schemeString() string {
	return "#<procedure:" + p.name + ">"
}

func (withExceptionHandlerProc) isTruthy() bool {
	return true
}

func (p withExceptionHandlerProc) call(args []value) (value, error) {
	handler, thunk, err := parseWithExceptionHandlerArgs(args, p.name)
	if err != nil {
		return nil, err
	}

	m := machine{}
	if err := startWithExceptionHandler(&m, handler, thunk, currentEvalPos, nil); err != nil {
		return nil, err
	}
	return m.run()
}

func (p raiseProc) schemeString() string {
	return "#<procedure:" + p.name + ">"
}

func (raiseProc) isTruthy() bool {
	return true
}

func (p raiseProc) call(args []value) (value, error) {
	if len(args) != 1 {
		return nil, newCurrentEvalError("'%s' expects exactly 1 argument", p.name)
	}

	m := machine{}
	if err := startRaise(&m, args[0]); err != nil {
		return nil, err
	}
	return m.run()
}

func (guardHandlerProc) schemeString() string {
	return "#<procedure:guard-handler>"
}

func (guardHandlerProc) isTruthy() bool {
	return true
}

func (p guardHandlerProc) call(args []value) (value, error) {
	if len(args) != 1 {
		return nil, newCurrentEvalError("guard handler expects exactly 1 argument")
	}

	m := machine{}
	if err := startGuardHandler(&m, p, args[0], nil); err != nil {
		return nil, err
	}
	return m.run()
}

func (f *exceptionHandlerReturnFrame) resume(m *machine, v value) error {
	m.handlers = f.handler.parent
	m.setValue(v, f.next)
	return nil
}

func (f *raiseInvokeHandlerFrame) resume(m *machine, v value) error {
	return applyProcedureState(m, f.handler, []value{v}, f.pos, f.next)
}

func (f *restoreHandlersFrame) resume(m *machine, v value) error {
	m.handlers = f.handlers
	m.setValue(v, f.next)
	return nil
}

func (f *guardClauseFrame) resume(m *machine, v value) error {
	clauseExpr := f.clauses[f.index]
	clause := clauseExpr.form.(listExpr)
	if v.isTruthy() {
		if len(clause) == 1 {
			m.setValue(v, f.next)
			return nil
		}
		return startSequence(m, clause[1:], f.env, f.next)
	}

	return advanceGuardClauses(m, f.clauses, f.index+1, f.env, f.exception, f.next)
}

func parseWithExceptionHandlerArgs(args []value, name string) (procedure, procedure, error) {
	if len(args) != 2 {
		return nil, nil, newCurrentEvalError("'%s' expects exactly 2 arguments", name)
	}

	handler, ok := args[0].(procedure)
	if !ok {
		return nil, nil, newCurrentEvalError("'%s' expects a procedure for handler, got %s", name, args[0].schemeString())
	}

	thunk, ok := args[1].(procedure)
	if !ok {
		return nil, nil, newCurrentEvalError("'%s' expects a procedure for thunk, got %s", name, args[1].schemeString())
	}

	return handler, thunk, nil
}

func startWithExceptionHandler(m *machine, handler procedure, thunk procedure, pos SourcePos, cont continuation) error {
	frame := &exceptionHandlerFrame{
		parent: m.handlers,
		handler: handler,
		wind:   m.wind,
		target: cont,
	}

	m.handlers = frame
	return applyProcedureState(m, thunk, nil, pos, &exceptionHandlerReturnFrame{
		handler: frame,
		next:    cont,
	})
}

func evalValueWithControlTransition(v value, currentWind *windFrame, targetWind *windFrame, currentHandlers *exceptionHandlerFrame, targetHandlers *exceptionHandlerFrame, cont continuation) (value, error) {
	m := machine{
		wind:     currentWind,
		handlers: currentHandlers,
	}
	if err := startControlTransition(&m, targetWind, targetHandlers, cont, v); err != nil {
		return nil, err
	}
	return m.run()
}

func startControlTransition(m *machine, targetWind *windFrame, targetHandlers *exceptionHandlerFrame, targetCont continuation, result value) error {
	return startWindTransition(m, targetWind, &restoreHandlersFrame{
		handlers: targetHandlers,
		next:     targetCont,
	}, result)
}

func startRaise(m *machine, exception value) error {
	handler := m.handlers
	if handler == nil {
		return newCurrentEvalError("unhandled exception: %s", exception.schemeString())
	}

	m.handlers = handler.parent
	return startWindTransition(m, handler.wind, &raiseInvokeHandlerFrame{
		handler: handler.handler,
		pos:     currentEvalPos,
		next:    handler.target,
	}, exception)
}

func startGuard(m *machine, parts []locatedExpr, env *env, cont continuation) error {
	if len(parts) < 2 {
		return newCurrentEvalError("'guard' expects a specification and a body")
	}

	spec, ok := parts[0].form.(listExpr)
	if !ok || len(spec) == 0 {
		return newEvalError(parts[0].pos, "'guard' expects (variable clause ...)")
	}

	name, ok := spec[0].form.(symbolExpr)
	if !ok {
		return newEvalError(spec[0].pos, "'guard' variable must be a symbol")
	}

	frame := &exceptionHandlerFrame{
		parent: m.handlers,
		handler: guardHandlerProc{
			variable: string(name),
			clauses:  spec[1:],
			env:      env,
		},
		wind:   m.wind,
		target: cont,
	}

	m.handlers = frame
	return startSequence(m, parts[1:], env, &exceptionHandlerReturnFrame{
		handler: frame,
		next:    cont,
	})
}

func startGuardHandler(m *machine, handler guardHandlerProc, exception value, cont continuation) error {
	guardEnv := newEnv(handler.env)
	guardEnv.define(handler.variable, exception)
	return advanceGuardClauses(m, handler.clauses, 0, guardEnv, exception, cont)
}

func advanceGuardClauses(m *machine, clauses []locatedExpr, index int, env *env, exception value, cont continuation) error {
	if index >= len(clauses) {
		return startRaise(m, exception)
	}

	clauseExpr := clauses[index]
	clause, ok := clauseExpr.form.(listExpr)
	if !ok || len(clause) == 0 {
		return newEvalError(clauseExpr.pos, "'guard' clauses must be non-empty lists")
	}

	if keyword, ok := clause[0].form.(symbolExpr); ok && string(keyword) == "else" {
		if index != len(clauses)-1 {
			return newEvalError(clause[0].pos, "'guard' else clause must be last")
		}
		if len(clause) == 1 {
			m.setValue(voidValue{}, cont)
			return nil
		}
		return startSequence(m, clause[1:], env, cont)
	}

	m.setExpr(clause[0], env, &guardClauseFrame{
		clauses:   clauses,
		index:     index,
		env:       env,
		exception: exception,
		next:      cont,
	})
	return nil
}
