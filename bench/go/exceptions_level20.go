package ming

import "fmt"

type exceptionHandlerKind int

const (
	exceptionHandlerLowLevel exceptionHandlerKind = iota
	exceptionHandlerGuard
)

type exceptionHandlerFrame struct {
	kind        exceptionHandlerKind
	wind        *dynamicWindFrame
	parent      *exceptionHandlerFrame
	handlerProc expr
	guardName   string
	guardExpr   expr
	guardEnv    *env
	next        level18Cont
}

type exceptionPopCont struct {
	frame *exceptionHandlerFrame
	next  level18Cont
}

type exceptionDispatchCont struct {
	frame *exceptionHandlerFrame
	value expr
}

type exceptionHandlerReturnedCont struct{}

func builtinRaiseSentinel(args []expr) (expr, error) {
	return nil, &EvalError{Message: "raise requires the level 20 evaluator"}
}

func builtinWithExceptionHandlerSentinel(args []expr) (expr, error) {
	return nil, &EvalError{Message: "with-exception-handler requires the level 20 evaluator"}
}

func (m *level18Machine) stepGuard(environment *env, forms []expr, pos sourcePos) error {
	if len(forms) < 1 {
		return attachPos(&EvalError{Message: "guard expects a clause list"}, pos)
	}
	if len(forms) < 2 {
		return attachPos(&EvalError{Message: "guard expects a body"}, pos)
	}

	header, ok := forms[0].(listExpr)
	if !ok || len(header.items) == 0 {
		return attachPos(&EvalError{Message: "guard expects a non-empty clause list"}, pos)
	}

	exceptionName, ok := header.items[0].(symbolExpr)
	if !ok {
		return attachPos(&EvalError{Message: "guard exception variable must be a symbol"}, pos)
	}

	guardExpr, err := expandGuardClauses(pos, exceptionName, header.items[1:])
	if err != nil {
		return attachPos(err, pos)
	}

	frame := &exceptionHandlerFrame{
		kind:      exceptionHandlerGuard,
		wind:      m.wind,
		parent:    m.handlers,
		guardName: exceptionName.name,
		guardExpr: guardExpr,
		guardEnv:  environment,
		next:      m.cont,
	}

	m.handlers = frame
	m.evalSequence(environment, forms[1:], &exceptionPopCont{
		frame: frame,
		next:  m.cont,
	})
	return nil
}

func (m *level18Machine) applyWithExceptionHandler(args []expr, cont level18Cont) error {
	if len(args) != 2 {
		return &EvalError{Message: "with-exception-handler expects exactly 2 arguments"}
	}

	frame := &exceptionHandlerFrame{
		kind:        exceptionHandlerLowLevel,
		wind:        m.wind,
		parent:      m.handlers,
		handlerProc: args[0],
	}

	m.handlers = frame
	return m.apply(args[1], nil, &exceptionPopCont{
		frame: frame,
		next:  cont,
	})
}

func (m *level18Machine) raise(value expr) error {
	if m.handlers == nil {
		return &EvalError{Message: fmt.Sprintf("uncaught exception: %s", renderExpr(value))}
	}

	frame := m.handlers
	m.handlers = frame.parent

	dispatch := &exceptionDispatchCont{
		frame: frame,
		value: value,
	}

	steps := buildWindTransition(m.wind, frame.wind)
	if len(steps) == 0 {
		m.wind = frame.wind
		m.returnValue(value, dispatch)
		return nil
	}

	return m.runWindTransition(steps, 0, dispatch, frame.wind, m.handlers, value)
}

func (m *level18Machine) dispatchException(frame *exceptionHandlerFrame, value expr) error {
	switch frame.kind {
	case exceptionHandlerLowLevel:
		return m.apply(frame.handlerProc, []expr{value}, &exceptionHandlerReturnedCont{})
	case exceptionHandlerGuard:
		guardEnv := &env{
			parent:   frame.guardEnv,
			bindings: map[string]expr{},
		}
		guardEnv.define(frame.guardName, value)
		m.eval(guardEnv, frame.guardExpr, frame.next)
		return nil
	default:
		return &EvalError{Message: "unsupported exception handler"}
	}
}

func expandGuardClauses(pos sourcePos, exceptionName symbolExpr, clauses []expr) (expr, error) {
	fallback := listExpr{
		pos: pos,
		items: []expr{
			symbolExpr{name: "raise", pos: pos},
			symbolExpr{name: exceptionName.name, pos: exceptionName.pos},
		},
	}

	return expandConditionalClauses(clauses, fallback, "%guard-value")
}

func expandConditionalClauses(clauses []expr, fallback expr, valueName string) (expr, error) {
	result := fallback

	for i := len(clauses) - 1; i >= 0; i-- {
		clause, ok := clauses[i].(listExpr)
		if !ok || len(clause.items) == 0 {
			return nil, &EvalError{Message: "guard clauses must be non-empty lists"}
		}

		if symbol, ok := clause.items[0].(symbolExpr); ok && symbol.name == "else" {
			if i != len(clauses)-1 {
				return nil, &EvalError{Message: "guard else clause must be last"}
			}
			result = level18SequenceForm(clause.pos, clause.items[1:])
			continue
		}

		if len(clause.items) == 1 {
			tmp := symbolExpr{name: valueName, pos: clause.pos}
			result = listExpr{
				pos: clause.pos,
				items: []expr{
					listExpr{
						pos: clause.pos,
						items: []expr{
							symbolExpr{name: "lambda", pos: clause.pos},
							listExpr{items: []expr{tmp}, pos: clause.pos},
							listExpr{
								pos: clause.pos,
								items: []expr{
									symbolExpr{name: "if", pos: clause.pos},
									tmp,
									tmp,
									result,
								},
							},
						},
					},
					clause.items[0],
				},
			}
			continue
		}

		result = listExpr{
			pos: clause.pos,
			items: []expr{
				symbolExpr{name: "if", pos: clause.pos},
				clause.items[0],
				level18SequenceForm(clause.pos, clause.items[1:]),
				result,
			},
		}
	}

	return result, nil
}
