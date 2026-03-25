package ming

type raiseProcValue struct{}

var raiseBuiltin = &raiseProcValue{}

type withExceptionHandlerProcValue struct{}

var withExceptionHandlerBuiltin = &withExceptionHandlerProcValue{}

type exceptionHandlerKind int

const (
	exceptionHandlerProcedure exceptionHandlerKind = iota
	exceptionHandlerGuard
)

type exceptionHandlerFrame struct {
	parent      *exceptionHandlerFrame
	wind        *windFrame
	kind        exceptionHandlerKind
	handler     value
	next        continuation
	pos         sourcePos
	guardName   string
	guardClauses []node
	guardEnv    *environment
}

type handlerRestoreCont struct {
	frame *exceptionHandlerFrame
	next  continuation
}

type exceptionDispatchCont struct {
	frame *exceptionHandlerFrame
	pos   sourcePos
}

type guardTestCont struct {
	clause    listNode
	remaining []node
	env       *environment
	exn       value
	pos       sourcePos
	next      continuation
}

func (m *evalMachine) startGuard(args []node, env *environment, pos sourcePos, next continuation) error {
	if len(args) < 2 {
		return &EvalError{Message: "guard expects a variable list and a body"}
	}

	spec, ok := args[0].(listNode)
	if !ok || len(spec.elements) == 0 {
		return &EvalError{Message: "guard expects a variable list"}
	}

	nameNode, ok := spec.elements[0].(symbolNode)
	if !ok {
		return &EvalError{Message: "guard requires an exception variable"}
	}

	frame := &exceptionHandlerFrame{
		parent:       m.handler,
		wind:         m.wind,
		kind:         exceptionHandlerGuard,
		next:         next,
		pos:          pos,
		guardName:    nameNode.name,
		guardClauses: spec.elements[1:],
		guardEnv:     env,
	}
	m.handler = frame
	return m.startSequence(args[1:], env, &handlerRestoreCont{
		frame: frame,
		next:  next,
	})
}

func (m *evalMachine) startWithExceptionHandler(handlerProc value, thunk value, pos sourcePos, next continuation) error {
	frame := &exceptionHandlerFrame{
		parent:  m.handler,
		wind:    m.wind,
		kind:    exceptionHandlerProcedure,
		handler: handlerProc,
		next:    next,
		pos:     pos,
	}
	m.handler = frame
	return m.enterProcedure(thunk, nil, pos, &handlerRestoreCont{
		frame: frame,
		next:  next,
	})
}

func (m *evalMachine) raiseValue(exn value, pos sourcePos) error {
	frame := m.handler
	if frame == nil {
		return errorAt(pos, "unhandled exception: %s", formatExceptionValue(exn))
	}

	leave, enter := diffWindFrames(m.wind, frame.wind)
	return m.startWindTransition(
		leave,
		enter,
		exn,
		&exceptionDispatchCont{frame: frame, pos: pos},
		frame.wind,
		frame,
		pos,
	)
}

func (m *evalMachine) continueExceptionDispatch(cont *exceptionDispatchCont) error {
	frame := cont.frame
	exn := m.val

	m.handler = frame.parent

	switch frame.kind {
	case exceptionHandlerProcedure:
		return m.enterProcedure(frame.handler, []value{exn}, cont.pos, frame.next)
	case exceptionHandlerGuard:
		guardEnv := newEnvironment(frame.guardEnv)
		guardEnv.define(frame.guardName, exn)
		return m.startGuardClauses(frame.guardClauses, guardEnv, exn, frame.pos, frame.next)
	default:
		return &EvalError{Message: "invalid exception handler"}
	}
}

func (m *evalMachine) startGuardClauses(clauses []node, env *environment, exn value, pos sourcePos, next continuation) error {
	if len(clauses) == 0 {
		return m.raiseValue(exn, pos)
	}

	clause, ok := clauses[0].(listNode)
	if !ok || len(clause.elements) == 0 {
		return errorAt(pos, "guard clauses must be non-empty lists")
	}

	if name, ok := symbolName(clause.elements[0]); ok && name == "else" {
		if len(clauses) != 1 {
			return errorAt(pos, "else clause must be last")
		}
		if len(clause.elements) == 1 {
			m.setValue(voidValue{}, next)
			return nil
		}
		return m.startSequence(clause.elements[1:], env, next)
	}

	m.setEval(clause.elements[0], env, &guardTestCont{
		clause:    clause,
		remaining: clauses[1:],
		env:       env,
		exn:       exn,
		pos:       pos,
		next:      next,
	})
	return nil
}

func formatExceptionValue(v value) string {
	formatted, err := formatValue(v)
	if err != nil || formatted == "" {
		return "#<exception>"
	}
	return formatted
}
