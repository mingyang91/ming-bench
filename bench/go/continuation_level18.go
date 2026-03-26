package ming

import "fmt"

type continuationExpr struct {
	cont     level18Cont
	wind     *dynamicWindFrame
	handlers *exceptionHandlerFrame
}

type level18Cont interface{}

type level18SeqCont struct {
	env  *env
	rest []expr
	next level18Cont
}

type level18DefineCont struct {
	env  *env
	name string
	pos  sourcePos
	next level18Cont
}

type level18SetCont struct {
	env    *env
	target symbolExpr
	pos    sourcePos
	next   level18Cont
}

type level18IfCont struct {
	env          *env
	condPos      sourcePos
	thenForm     expr
	elseForm     expr
	hasAlternate bool
	next         level18Cont
}

type level18AndCont struct {
	env        *env
	currentPos sourcePos
	remaining  []expr
	next       level18Cont
}

type level18OrCont struct {
	env        *env
	currentPos sourcePos
	remaining  []expr
	next       level18Cont
}

type level18CallOpCont struct {
	env      *env
	argForms []expr
	pos      sourcePos
	opPos    sourcePos
	next     level18Cont
}

type level18CallArgsCont struct {
	proc     expr
	env      *env
	argForms []expr
	index    int
	values   []expr
	pos      sourcePos
	next     level18Cont
}

type level18CallWithValuesCont struct {
	consumer expr
	next     level18Cont
}

type dynamicWindFrame struct {
	in     expr
	out    expr
	parent *dynamicWindFrame
}

type dynamicWindAfterInCont struct {
	frame    *dynamicWindFrame
	bodyProc expr
	next     level18Cont
}

type dynamicWindAfterBodyCont struct {
	frame *dynamicWindFrame
	next  level18Cont
}

type dynamicWindAfterOutCont struct {
	frame  *dynamicWindFrame
	result expr
	next   level18Cont
}

type dynamicWindTransitionStep struct {
	frame    *dynamicWindFrame
	entering bool
}

type dynamicWindTransitionCont struct {
	steps          []dynamicWindTransitionStep
	index          int
	targetCont     level18Cont
	targetWind     *dynamicWindFrame
	targetHandlers *exceptionHandlerFrame
	value          expr
}

type level18Machine struct {
	evaluating bool
	env        *env
	form       expr
	value      expr
	cont       level18Cont
	wind       *dynamicWindFrame
	handlers   *exceptionHandlerFrame
}

func builtinContinuationSentinel(args []expr) (expr, error) {
	return nil, &EvalError{Message: "call/cc requires the level 18 evaluator"}
}

func builtinDynamicWindSentinel(args []expr) (expr, error) {
	return nil, &EvalError{Message: "dynamic-wind requires the level 19 evaluator"}
}

func evalSequenceLevel18(environment *env, forms []expr) (expr, error) {
	machine := &level18Machine{}
	machine.evalSequence(environment, forms, nil)
	return machine.run()
}

func applyCallableLevel18(proc expr, args []expr) (expr, error) {
	machine := &level18Machine{}
	if err := machine.apply(proc, args, nil); err != nil {
		return nil, err
	}
	return machine.run()
}

func (m *level18Machine) eval(environment *env, form expr, cont level18Cont) {
	m.evaluating = true
	m.env = environment
	m.form = form
	m.cont = cont
}

func (m *level18Machine) evalSequence(environment *env, forms []expr, cont level18Cont) {
	switch len(forms) {
	case 0:
		m.returnValue(voidExpr{}, cont)
	case 1:
		m.eval(environment, forms[0], cont)
	default:
		rest := append([]expr(nil), forms[1:]...)
		m.eval(environment, forms[0], &level18SeqCont{
			env:  environment,
			rest: rest,
			next: cont,
		})
	}
}

func (m *level18Machine) continueStep(step evalStep, cont level18Cont) {
	if step.tail {
		m.eval(step.nextEnv, step.nextForm, cont)
		return
	}
	m.returnValue(step.value, cont)
}

func (m *level18Machine) returnValue(value expr, cont level18Cont) {
	m.evaluating = false
	m.value = value
	m.cont = cont
}

func (m *level18Machine) run() (expr, error) {
	for {
		if m.evaluating {
			if err := consumeStepBudget(m.env); err != nil {
				return nil, err
			}
			if err := m.stepEval(); err != nil {
				return nil, err
			}
			continue
		}

		switch cont := m.cont.(type) {
		case nil:
			return m.value, nil
		case *level18SeqCont:
			if currentBenchLevel() >= 24 {
				if nextForm, ok := advanceSeqContCallCC(cont); ok {
					m.eval(cont.env, nextForm, cont)
					continue
				}
			}
			m.evalSequence(cont.env, cont.rest, cont.next)
		case *level18DefineCont:
			value, err := expectSingleValue(m.value, "define")
			if err != nil {
				return nil, attachPos(err, cont.pos)
			}
			cont.env.define(cont.name, value)
			m.returnValue(voidExpr{}, cont.next)
		case *level18SetCont:
			value, err := expectSingleValue(m.value, "set!")
			if err != nil {
				return nil, attachPos(err, cont.pos)
			}
			if !cont.env.assign(cont.target.name, value) {
				return nil, errorAt(cont.target.pos, fmt.Sprintf("unbound symbol: %s", cont.target.name))
			}
			m.returnValue(voidExpr{}, cont.next)
		case *level18IfCont:
			condition, err := expectSingleValue(m.value, "if")
			if err != nil {
				return nil, attachPos(err, cont.condPos)
			}
			if isTruthy(condition) {
				m.eval(cont.env, cont.thenForm, cont.next)
				continue
			}
			if cont.hasAlternate {
				m.eval(cont.env, cont.elseForm, cont.next)
				continue
			}
			m.returnValue(voidExpr{}, cont.next)
		case *level18AndCont:
			value, err := expectSingleValue(m.value, "and")
			if err != nil {
				return nil, attachPos(err, cont.currentPos)
			}
			if !isTruthy(value) {
				m.returnValue(value, cont.next)
				continue
			}
			if len(cont.remaining) == 1 {
				m.eval(cont.env, cont.remaining[0], cont.next)
				continue
			}

			nextForm := cont.remaining[0]
			m.eval(cont.env, nextForm, &level18AndCont{
				env:        cont.env,
				currentPos: formPos(nextForm),
				remaining:  append([]expr(nil), cont.remaining[1:]...),
				next:       cont.next,
			})
		case *level18OrCont:
			value, err := expectSingleValue(m.value, "or")
			if err != nil {
				return nil, attachPos(err, cont.currentPos)
			}
			if isTruthy(value) {
				m.returnValue(value, cont.next)
				continue
			}
			if len(cont.remaining) == 1 {
				m.eval(cont.env, cont.remaining[0], cont.next)
				continue
			}

			nextForm := cont.remaining[0]
			m.eval(cont.env, nextForm, &level18OrCont{
				env:        cont.env,
				currentPos: formPos(nextForm),
				remaining:  append([]expr(nil), cont.remaining[1:]...),
				next:       cont.next,
			})
		case *level18CallOpCont:
			proc, err := expectSingleValue(m.value, "procedure application")
			if err != nil {
				return nil, attachPos(err, cont.opPos)
			}
			if len(cont.argForms) == 0 {
				if err := m.apply(proc, nil, cont.next); err != nil {
					return nil, attachPos(err, cont.pos)
				}
				continue
			}

			values := make([]expr, len(cont.argForms))
			index := len(cont.argForms) - 1
			m.eval(cont.env, cont.argForms[index], &level18CallArgsCont{
				proc:     proc,
				env:      cont.env,
				argForms: cont.argForms,
				index:    index,
				values:   values,
				pos:      cont.pos,
				next:     cont.next,
			})
		case *level18CallArgsCont:
			value, err := expectSingleValue(m.value, "procedure application")
			if err != nil {
				return nil, attachPos(err, formPos(cont.argForms[cont.index]))
			}
			values := append([]expr(nil), cont.values...)
			values[cont.index] = value

			if cont.index > 0 {
				index := cont.index - 1
				m.eval(cont.env, cont.argForms[index], &level18CallArgsCont{
					proc:     cont.proc,
					env:      cont.env,
					argForms: cont.argForms,
					index:    index,
					values:   values,
					pos:      cont.pos,
					next:     cont.next,
				})
				continue
			}

			if err := m.apply(cont.proc, values, cont.next); err != nil {
				return nil, attachPos(err, cont.pos)
			}
		case *level18CallWithValuesCont:
			if err := m.apply(cont.consumer, valuesSlice(m.value), cont.next); err != nil {
				return nil, err
			}
		case *dynamicWindAfterInCont:
			m.wind = cont.frame
			if err := m.apply(cont.bodyProc, nil, &dynamicWindAfterBodyCont{
				frame: cont.frame,
				next:  cont.next,
			}); err != nil {
				return nil, err
			}
		case *dynamicWindAfterBodyCont:
			if err := m.apply(cont.frame.out, nil, &dynamicWindAfterOutCont{
				frame:  cont.frame,
				result: m.value,
				next:   cont.next,
			}); err != nil {
				return nil, err
			}
		case *dynamicWindAfterOutCont:
			m.wind = cont.frame.parent
			m.returnValue(cont.result, cont.next)
		case *dynamicWindTransitionCont:
			step := cont.steps[cont.index]
			if step.entering {
				m.wind = step.frame
			} else {
				m.wind = step.frame.parent
			}

			if cont.index+1 < len(cont.steps) {
				if err := m.runWindTransition(cont.steps, cont.index+1, cont.targetCont, cont.targetWind, cont.targetHandlers, cont.value); err != nil {
					return nil, err
				}
				continue
			}

			m.wind = cont.targetWind
			m.handlers = cont.targetHandlers
			m.returnValue(cont.value, cont.targetCont)
		case *exceptionPopCont:
			if m.handlers == cont.frame {
				m.handlers = cont.frame.parent
			}
			m.returnValue(m.value, cont.next)
		case *exceptionDispatchCont:
			if err := m.dispatchException(cont.frame, cont.value); err != nil {
				return nil, err
			}
		case *exceptionHandlerReturnedCont:
			return nil, &EvalError{Message: "exception handler returned"}
		default:
			return nil, &EvalError{Message: "unsupported continuation"}
		}
	}
}

func advanceSeqContCallCC(cont *level18SeqCont) (expr, bool) {
	if cont == nil || len(cont.rest) == 0 {
		return nil, false
	}

	call, ok := cont.rest[0].(listExpr)
	if !ok || len(call.items) == 0 {
		return nil, false
	}

	head, ok := call.items[0].(symbolExpr)
	if !ok {
		return nil, false
	}
	if head.name != "call/cc" && head.name != "call-with-current-continuation" {
		return nil, false
	}

	nextForm := cont.rest[0]
	cont.rest = append([]expr(nil), cont.rest[1:]...)
	return nextForm, true
}

func (m *level18Machine) stepEval() error {
	switch form := m.form.(type) {
	case symbolExpr:
		lookupEnv := m.env
		if form.lookupEnv != nil {
			lookupEnv = form.lookupEnv
		}
		value, ok := lookupEnv.lookup(form.name)
		if !ok {
			return errorAt(form.pos, fmt.Sprintf("unbound symbol: %s", form.name))
		}
		if _, ok := value.(uninitializedExpr); ok {
			return errorAt(form.pos, fmt.Sprintf("uninitialized binding: %s", form.name))
		}
		m.returnValue(value, m.cont)
		return nil
	case listExpr:
		return m.stepList(form)
	default:
		m.returnValue(form, m.cont)
		return nil
	}
}

func (m *level18Machine) stepList(items listExpr) error {
	if len(items.items) == 0 {
		return errorAt(items.pos, "cannot evaluate empty list")
	}

	environment := m.env

	if operator, ok := items.items[0].(symbolExpr); ok {
		switch operator.name {
		case "define":
			return m.stepDefine(environment, items.items[1:], operator.pos)
		case "define-record-type":
			value, err := evalDefineRecordType(environment, items.items[1:])
			if err != nil {
				return attachPos(err, operator.pos)
			}
			m.returnValue(value, m.cont)
			return nil
		case "define-syntax":
			value, err := evalDefineSyntax(environment, items.items[1:])
			if err != nil {
				return attachPos(err, operator.pos)
			}
			m.returnValue(value, m.cont)
			return nil
		case "set!":
			return m.stepSet(environment, items.items[1:], operator.pos)
		case "if":
			return m.stepIf(environment, items.items[1:], operator.pos)
		case "begin":
			m.evalSequence(environment, items.items[1:], m.cont)
			return nil
		case "and":
			return m.stepAnd(environment, items.items[1:])
		case "or":
			return m.stepOr(environment, items.items[1:])
		case "cond":
			expanded, err := level18ExpandCond(items.pos, items.items[1:])
			if err != nil {
				return attachPos(err, operator.pos)
			}
			m.eval(environment, expanded, m.cont)
			return nil
		case "do":
			step, err := evalDo(environment, items.items[1:])
			if err != nil {
				return attachPos(err, operator.pos)
			}
			m.continueStep(step, m.cont)
			return nil
		case "case":
			step, err := evalCase(environment, items.items[1:])
			if err != nil {
				return attachPos(err, operator.pos)
			}
			m.continueStep(step, m.cont)
			return nil
		case "guard":
			if currentBenchLevel() >= 20 {
				return m.stepGuard(environment, items.items[1:], operator.pos)
			}
		case "quote":
			if len(items.items) != 2 {
				return attachPos(&EvalError{Message: "quote expects exactly 1 argument"}, operator.pos)
			}
			m.returnValue(quoteDatum(items.items[1]), m.cont)
			return nil
		case "quasiquote":
			value, err := evalQuasiquote(environment, items.items[1:])
			if err != nil {
				return attachPos(err, operator.pos)
			}
			m.returnValue(value, m.cont)
			return nil
		case "syntax":
			value, err := evalSyntax(environment, items.items[1:])
			if err != nil {
				return attachPos(err, operator.pos)
			}
			m.returnValue(value, m.cont)
			return nil
		case "syntax-case":
			value, err := evalSyntaxCase(environment, items.items[1:])
			if err != nil {
				return attachPos(err, operator.pos)
			}
			m.returnValue(value, m.cont)
			return nil
		case "with-syntax":
			step, err := evalWithSyntax(environment, items.items[1:])
			if err != nil {
				return attachPos(err, operator.pos)
			}
			m.continueStep(step, m.cont)
			return nil
		case "lambda":
			value, err := evalLambda(environment, items.items[1:])
			if err != nil {
				return attachPos(err, operator.pos)
			}
			m.returnValue(value, m.cont)
			return nil
		case "case-lambda":
			value, err := evalCaseLambda(environment, items.items[1:])
			if err != nil {
				return attachPos(err, operator.pos)
			}
			m.returnValue(value, m.cont)
			return nil
		case "let":
			if len(items.items) > 1 {
				if _, ok := items.items[1].(symbolExpr); !ok {
					expanded, err := level18ExpandSimpleLet(items.pos, items.items[1:])
					if err != nil {
						return attachPos(err, operator.pos)
					}
					m.eval(environment, expanded, m.cont)
					return nil
				}
			}

			value, err := evalExpr(environment, items)
			if err != nil {
				return err
			}
			m.returnValue(value, m.cont)
			return nil
		case "let*":
			step, err := evalLetStar(environment, items.items[1:])
			if err != nil {
				return attachPos(err, operator.pos)
			}
			m.continueStep(step, m.cont)
			return nil
		case "letrec":
			step, err := evalLetrec(environment, items.items[1:], false)
			if err != nil {
				return attachPos(err, operator.pos)
			}
			m.continueStep(step, m.cont)
			return nil
		case "letrec*":
			step, err := evalLetrec(environment, items.items[1:], true)
			if err != nil {
				return attachPos(err, operator.pos)
			}
			m.continueStep(step, m.cont)
			return nil
		}

		lookupEnv := environment
		if operator.lookupEnv != nil {
			lookupEnv = operator.lookupEnv
		}
		if macroValue, ok := lookupEnv.lookup(operator.name); ok {
			if macro, ok := macroValue.(macroExpr); ok {
				expanded, err := expandMacro(macro, items)
				if err != nil {
					return attachPos(err, operator.pos)
				}
				m.eval(environment, expanded, m.cont)
				return nil
			}
		}
	}

	argForms := append([]expr(nil), items.items[1:]...)
	m.eval(environment, items.items[0], &level18CallOpCont{
		env:      environment,
		argForms: argForms,
		pos:      items.pos,
		opPos:    formPos(items.items[0]),
		next:     m.cont,
	})
	return nil
}

func (m *level18Machine) stepDefine(environment *env, forms []expr, pos sourcePos) error {
	if len(forms) < 2 {
		return attachPos(&EvalError{Message: "define expects a name and value"}, pos)
	}

	switch target := forms[0].(type) {
	case symbolExpr:
		if len(forms) != 2 {
			return attachPos(&EvalError{Message: "define expects exactly 2 arguments"}, pos)
		}
		m.eval(environment, forms[1], &level18DefineCont{
			env:  environment,
			name: target.name,
			pos:  formPos(forms[1]),
			next: m.cont,
		})
		return nil
	case listExpr:
		if len(target.items) == 0 {
			return attachPos(&EvalError{Message: "define function name cannot be empty"}, pos)
		}

		name, ok := target.items[0].(symbolExpr)
		if !ok {
			return attachPos(&EvalError{Message: "define function name must be a symbol"}, pos)
		}

		params, restParam, variadic, err := parseParamList(target.items[1:])
		if err != nil {
			return attachPos(err, pos)
		}

		environment.define(name.name, closureExpr{
			params:    params,
			restParam: restParam,
			variadic:  variadic,
			body:      append([]expr(nil), forms[1:]...),
			env:       environment,
		})
		m.returnValue(voidExpr{}, m.cont)
		return nil
	default:
		return attachPos(&EvalError{Message: "define target must be a symbol or parameter list"}, pos)
	}
}

func (m *level18Machine) stepSet(environment *env, forms []expr, pos sourcePos) error {
	if len(forms) != 2 {
		return attachPos(&EvalError{Message: "set! expects exactly 2 arguments"}, pos)
	}

	target, ok := forms[0].(symbolExpr)
	if !ok {
		return attachPos(&EvalError{Message: "set! target must be a symbol"}, pos)
	}

	m.eval(environment, forms[1], &level18SetCont{
		env:    environment,
		target: target,
		pos:    formPos(forms[1]),
		next:   m.cont,
	})
	return nil
}

func (m *level18Machine) stepIf(environment *env, forms []expr, pos sourcePos) error {
	if len(forms) != 2 && len(forms) != 3 {
		return attachPos(&EvalError{Message: "if expects 2 or 3 arguments"}, pos)
	}

	cont := &level18IfCont{
		env:      environment,
		condPos:  formPos(forms[0]),
		thenForm: forms[1],
		next:     m.cont,
	}
	if len(forms) == 3 {
		cont.elseForm = forms[2]
		cont.hasAlternate = true
	}

	m.eval(environment, forms[0], cont)
	return nil
}

func (m *level18Machine) stepAnd(environment *env, forms []expr) error {
	switch len(forms) {
	case 0:
		m.returnValue(boolExpr(true), m.cont)
		return nil
	case 1:
		m.eval(environment, forms[0], m.cont)
		return nil
	default:
		m.eval(environment, forms[0], &level18AndCont{
			env:        environment,
			currentPos: formPos(forms[0]),
			remaining:  append([]expr(nil), forms[1:]...),
			next:       m.cont,
		})
		return nil
	}
}

func (m *level18Machine) stepOr(environment *env, forms []expr) error {
	switch len(forms) {
	case 0:
		m.returnValue(boolExpr(false), m.cont)
		return nil
	case 1:
		m.eval(environment, forms[0], m.cont)
		return nil
	default:
		m.eval(environment, forms[0], &level18OrCont{
			env:        environment,
			currentPos: formPos(forms[0]),
			remaining:  append([]expr(nil), forms[1:]...),
			next:       m.cont,
		})
		return nil
	}
}

func (m *level18Machine) applyDynamicWind(args []expr, cont level18Cont) error {
	if len(args) != 3 {
		return &EvalError{Message: "dynamic-wind expects exactly 3 arguments"}
	}

	frame := &dynamicWindFrame{
		in:     args[0],
		out:    args[2],
		parent: m.wind,
	}

	return m.apply(args[0], nil, &dynamicWindAfterInCont{
		frame:    frame,
		bodyProc: args[1],
		next:     cont,
	})
}

func (m *level18Machine) invokeContinuation(target *continuationExpr, value expr) error {
	steps := buildWindTransition(m.wind, target.wind)
	if len(steps) == 0 {
		m.wind = target.wind
		m.handlers = target.handlers
		m.returnValue(value, target.cont)
		return nil
	}

	return m.runWindTransition(steps, 0, target.cont, target.wind, target.handlers, value)
}

func (m *level18Machine) runWindTransition(steps []dynamicWindTransitionStep, index int, targetCont level18Cont, targetWind *dynamicWindFrame, targetHandlers *exceptionHandlerFrame, value expr) error {
	step := steps[index]
	next := &dynamicWindTransitionCont{
		steps:          steps,
		index:          index,
		targetCont:     targetCont,
		targetWind:     targetWind,
		targetHandlers: targetHandlers,
		value:          value,
	}

	if step.entering {
		return m.apply(step.frame.in, nil, next)
	}
	return m.apply(step.frame.out, nil, next)
}

func buildWindTransition(from *dynamicWindFrame, to *dynamicWindFrame) []dynamicWindTransitionStep {
	fromPath := dynamicWindPath(from)
	toPath := dynamicWindPath(to)

	common := 0
	for common < len(fromPath) && common < len(toPath) && fromPath[common] == toPath[common] {
		common++
	}

	steps := make([]dynamicWindTransitionStep, 0, len(fromPath)-common+len(toPath)-common)
	for i := len(fromPath) - 1; i >= common; i-- {
		steps = append(steps, dynamicWindTransitionStep{frame: fromPath[i]})
	}
	for i := common; i < len(toPath); i++ {
		steps = append(steps, dynamicWindTransitionStep{
			frame:    toPath[i],
			entering: true,
		})
	}

	return steps
}

func dynamicWindPath(frame *dynamicWindFrame) []*dynamicWindFrame {
	if frame == nil {
		return nil
	}

	reversed := make([]*dynamicWindFrame, 0, 4)
	for current := frame; current != nil; current = current.parent {
		reversed = append(reversed, current)
	}

	path := make([]*dynamicWindFrame, len(reversed))
	for i := range reversed {
		path[len(reversed)-1-i] = reversed[i]
	}
	return path
}

func (m *level18Machine) apply(proc expr, args []expr, cont level18Cont) error {
applyLoop:
	for {
		switch callable := proc.(type) {
		case builtinProc:
			switch callable.name {
			case "call/cc", "call-with-current-continuation":
				if len(args) != 1 {
					return &EvalError{Message: fmt.Sprintf("%s expects exactly 1 argument", callable.name)}
				}
				proc = args[0]
				args = []expr{&continuationExpr{cont: cont, wind: m.wind, handlers: m.handlers}}
				continue
			case "dynamic-wind":
				return m.applyDynamicWind(args, cont)
			case "call-with-values":
				if len(args) != 2 {
					return &EvalError{Message: "call-with-values expects exactly 2 arguments"}
				}
				return m.apply(args[0], nil, &level18CallWithValuesCont{
					consumer: args[1],
					next:     cont,
				})
			case "raise":
				if len(args) != 1 {
					return &EvalError{Message: "raise expects exactly 1 argument"}
				}
				return m.raise(args[0])
			case "with-exception-handler":
				return m.applyWithExceptionHandler(args, cont)
			case "apply":
				if len(args) < 2 {
					return &EvalError{Message: "apply expects at least 2 arguments"}
				}

				tailList, ok := listElements(args[len(args)-1])
				if !ok {
					return &EvalError{Message: "apply expects a list as its final argument"}
				}

				callArgs := make([]expr, 0, len(args)-2+len(tailList))
				callArgs = append(callArgs, args[1:len(args)-1]...)
				callArgs = append(callArgs, tailList...)
				proc = args[0]
				args = callArgs
				continue
			default:
				value, err := callable.fn(args)
				if err != nil {
					return err
				}
				m.returnValue(value, cont)
				return nil
			}
		case *continuationExpr:
			return m.invokeContinuation(callable, makeValuesExpr(args))
		case closureExpr:
			if !callable.variadic && len(args) != len(callable.params) {
				return &EvalError{Message: fmt.Sprintf("expected %d arguments, got %d", len(callable.params), len(args))}
			}
			if callable.variadic && len(args) < len(callable.params) {
				return &EvalError{Message: fmt.Sprintf("expected at least %d arguments, got %d", len(callable.params), len(args))}
			}

			callEnv := &env{
				parent:   callable.env,
				bindings: map[string]expr{},
			}
			for i, name := range callable.params {
				callEnv.define(name, args[i])
			}
			if callable.variadic {
				rest := append([]expr(nil), args[len(callable.params):]...)
				callEnv.define(callable.restParam, properListFromSlice(rest))
			}
			m.evalSequence(callEnv, callable.body, cont)
			return nil
		case caseClosureExpr:
			for _, clause := range callable.clauses {
				if closureMatchesArity(clause, len(args)) {
					proc = clause
					continue applyLoop
				}
			}
			return &EvalError{Message: fmt.Sprintf("no matching case-lambda clause for %d arguments", len(args))}
		case recordConstructorProc:
			value, err := applyRecordConstructor(callable, args)
			if err != nil {
				return err
			}
			m.returnValue(value, cont)
			return nil
		case recordPredicateProc:
			value, err := applyRecordPredicate(callable, args)
			if err != nil {
				return err
			}
			m.returnValue(value, cont)
			return nil
		case recordAccessorProc:
			value, err := applyRecordAccessor(callable, args)
			if err != nil {
				return err
			}
			m.returnValue(value, cont)
			return nil
		case recordMutatorProc:
			value, err := applyRecordMutator(callable, args)
			if err != nil {
				return err
			}
			m.returnValue(value, cont)
			return nil
		default:
			return &EvalError{Message: "first list element is not a procedure"}
		}
	}
}

func level18ExpandSimpleLet(pos sourcePos, forms []expr) (expr, error) {
	if len(forms) < 2 {
		return nil, &EvalError{Message: "let expects bindings and a body"}
	}

	bindings, ok := forms[0].(listExpr)
	if !ok {
		return nil, &EvalError{Message: "let bindings must be a list"}
	}

	params := make([]expr, 0, len(bindings.items))
	args := make([]expr, 0, len(bindings.items))
	for _, rawBinding := range bindings.items {
		binding, ok := rawBinding.(listExpr)
		if !ok || len(binding.items) != 2 {
			return nil, &EvalError{Message: "let bindings must have the form (name value)"}
		}

		name, ok := binding.items[0].(symbolExpr)
		if !ok {
			return nil, &EvalError{Message: "let binding name must be a symbol"}
		}

		params = append(params, name)
		args = append(args, binding.items[1])
	}

	lambdaItems := make([]expr, 0, len(forms)+1)
	lambdaItems = append(lambdaItems, symbolExpr{name: "lambda", pos: pos})
	lambdaItems = append(lambdaItems, listExpr{items: params, pos: pos})
	lambdaItems = append(lambdaItems, forms[1:]...)

	appItems := make([]expr, 0, len(args)+1)
	appItems = append(appItems, listExpr{items: lambdaItems, pos: pos})
	appItems = append(appItems, args...)

	return listExpr{items: appItems, pos: pos}, nil
}

func level18ExpandCond(pos sourcePos, clauses []expr) (expr, error) {
	if len(clauses) == 0 {
		return voidExpr{}, nil
	}

	result := expr(voidExpr{})
	for i := len(clauses) - 1; i >= 0; i-- {
		clause, ok := clauses[i].(listExpr)
		if !ok || len(clause.items) == 0 {
			return nil, &EvalError{Message: "cond clauses must be non-empty lists"}
		}

		if symbol, ok := clause.items[0].(symbolExpr); ok && symbol.name == "else" {
			if i != len(clauses)-1 {
				return nil, &EvalError{Message: "cond else clause must be last"}
			}
			result = level18SequenceForm(clause.pos, clause.items[1:])
			continue
		}

		if isCondArrowClause(clause) {
			tmp := symbolExpr{name: freshMacroName("%cond_value"), pos: clause.pos}
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
									listExpr{
										pos:   clause.pos,
										items: []expr{clause.items[2], tmp},
									},
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

		if len(clause.items) == 1 {
			tmp := symbolExpr{name: "%cond-value", pos: clause.pos}
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

func level18SequenceForm(pos sourcePos, forms []expr) expr {
	switch len(forms) {
	case 0:
		return voidExpr{}
	case 1:
		return forms[0]
	default:
		items := make([]expr, 0, len(forms)+1)
		items = append(items, symbolExpr{name: "begin", pos: pos})
		items = append(items, forms...)
		return listExpr{items: items, pos: pos}
	}
}
