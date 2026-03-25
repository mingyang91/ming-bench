package ming

type continuation interface{}

type continuationValue struct {
	cont    continuation
	wind    *windFrame
	handler *exceptionHandlerFrame
	ctx     *evalContext
}

type callCCProcValue struct{}

var callCCBuiltin = &callCCProcValue{}

type dynamicWindProcValue struct{}

var dynamicWindBuiltin = &dynamicWindProcValue{}

type windFrame struct {
	inProc  value
	outProc value
	parent  *windFrame
}

type sequenceCont struct {
	exprs []node
	index int
	env   *environment
	next  continuation
}

type ifCont struct {
	consequent node
	alternate  node
	hasElse    bool
	env        *environment
	next       continuation
}

type defineValueCont struct {
	target symbolNode
	env    *environment
	next   continuation
}

type setValueCont struct {
	target symbolNode
	env    *environment
	next   continuation
}

type andCont struct {
	args  []node
	index int
	env   *environment
	next  continuation
}

type orCont struct {
	args  []node
	index int
	env   *environment
	next  continuation
}

type applicationOperatorCont struct {
	operands []node
	env      *environment
	pos      sourcePos
	next     continuation
}

type applicationArgCont struct {
	proc     value
	operands []node
	index    int
	values   []value
	env      *environment
	pos      sourcePos
	next     continuation
}

type callWithValuesProducerCont struct {
	consumer value
	pos      sourcePos
	next     continuation
}

type condTestCont struct {
	clauses []node
	index   int
	env     *environment
	pos     sourcePos
	next    continuation
}

type condArrowCont struct {
	arg  value
	pos  sourcePos
	next continuation
}

type caseKeyCont struct {
	clauses []node
	env     *environment
	pos     sourcePos
	next    continuation
}

type letValueCont struct {
	specs  []namedBindingSpec
	index  int
	values []value
	env    *environment
	body   []node
	next   continuation
}

type namedLetValueCont struct {
	name   string
	specs  []namedBindingSpec
	index  int
	values []value
	env    *environment
	body   []node
	next   continuation
}

type letStarValueCont struct {
	specs  []namedBindingSpec
	index  int
	letEnv *environment
	body   []node
	next   continuation
}

type letrecValueCont struct {
	specs      []namedBindingSpec
	index      int
	sequential bool
	letEnv     *environment
	cells      []*binding
	values     []value
	body       []node
	next       continuation
}

type dynamicWindAfterInCont struct {
	frame    *windFrame
	bodyProc value
	pos      sourcePos
	next     continuation
}

type dynamicWindAfterBodyCont struct {
	frame *windFrame
	pos   sourcePos
	next  continuation
}

type dynamicWindAfterOutCont struct {
	result value
	next   continuation
}

type windTransitionCont struct {
	leave         []*windFrame
	enter         []*windFrame
	setWind       *windFrame
	value         value
	targetCont    continuation
	targetWind    *windFrame
	targetHandler *exceptionHandlerFrame
	pos           sourcePos
}

type evalMachine struct {
	expr    node
	env     *environment
	val     value
	cont    continuation
	wind    *windFrame
	handler *exceptionHandlerFrame
	eval    bool
	ctx     *evalContext
}

func runEval(expr node, env *environment) (value, error) {
	m := &evalMachine{ctx: env.evalContext()}
	m.setEval(expr, env, nil)
	return m.run()
}

func runEvalSequence(exprs []node, env *environment) (value, error) {
	m := &evalMachine{ctx: env.evalContext()}
	if err := m.startSequence(exprs, env, nil); err != nil {
		return nil, err
	}
	return m.run()
}

func runProcedureCall(proc value, args []value, pos sourcePos, ctx *evalContext) (value, error) {
	if ctx == nil {
		ctx = procedureEvalContext(proc)
	}

	m := &evalMachine{ctx: ctx}
	if err := m.enterProcedure(proc, args, pos, nil); err != nil {
		return nil, err
	}
	return m.run()
}

func (m *evalMachine) run() (value, error) {
	for {
		if m.eval {
			if err := consumeEvalStep(m.ctx, m.expr); err != nil {
				return nil, err
			}
			if err := m.stepEval(); err != nil {
				return nil, err
			}
			continue
		}

		if m.cont == nil {
			return m.val, nil
		}

		if err := m.stepContinue(); err != nil {
			return nil, err
		}
	}
}

func (m *evalMachine) setEval(expr node, env *environment, cont continuation) {
	m.expr = expr
	m.env = env
	m.cont = cont
	m.eval = true
}

func (m *evalMachine) setValue(val value, cont continuation) {
	m.val = val
	m.cont = cont
	m.eval = false
}

func (m *evalMachine) stepEval() error {
	switch expr := m.expr.(type) {
	case listNode:
		if m.env == nil || m.env.macroState == nil || !m.env.macroState.hasMacros {
			return m.stepEvalExpanded(expr)
		}
		expanded, err := expandMacros(expr, m.env)
		if err != nil {
			return err
		}
		return m.stepEvalExpanded(expanded)
	default:
		return m.stepEvalExpanded(expr)
	}
}

func (m *evalMachine) stepEvalExpanded(expr node) error {
	switch expr := expr.(type) {
	case integerValue, rationalValue, inexactValue, booleanValue, stringValue, charValue:
		m.setValue(expr, m.cont)
		return nil
	case vectorNode:
		result, err := datumFromNode(expr)
		if err != nil {
			return err
		}
		m.setValue(result, m.cont)
		return nil
	case symbolNode:
		if expr.captured != nil {
			m.setValue(expr.captured.value, m.cont)
			return nil
		}

		val, ok := m.env.lookup(expr.name)
		if !ok {
			return errorAt(expr.pos, "unbound variable: %s", expr.name)
		}
		m.setValue(val, m.cont)
		return nil
	case listNode:
		if len(expr.elements) == 0 {
			return errorAt(expr.pos, "cannot evaluate empty list")
		}
		return m.stepEvalList(expr)
	default:
		return errorAt(nodePos(expr), "unknown expression")
	}
}

func (m *evalMachine) stepEvalList(list listNode) error {
	if name, ok := symbolName(list.elements[0]); ok {
		args := list.elements[1:]
		switch name {
		case "and":
			return m.startAnd(args, m.env, m.cont)
		case "or":
			return m.startOr(args, m.env, m.cont)
		case "define":
			return withErrorPos(m.startDefine(args, m.env, list.pos, m.cont), list.pos)
		case "define-syntax":
			result, err := evalDefineSyntax(args, m.env)
			if err != nil {
				return withErrorPos(err, list.pos)
			}
			m.setValue(result, m.cont)
			return nil
		case "define-record-type":
			result, err := evalDefineRecordType(args, m.env)
			if err != nil {
				return withErrorPos(err, list.pos)
			}
			m.setValue(result, m.cont)
			return nil
		case "set!":
			return withErrorPos(m.startSet(args, m.env, list.pos, m.cont), list.pos)
		case "if":
			return withErrorPos(m.startIf(args, m.env, m.cont), list.pos)
		case "quote":
			result, err := evalQuote(args)
			if err != nil {
				return withErrorPos(err, list.pos)
			}
			m.setValue(result, m.cont)
			return nil
		case "quasiquote":
			result, err := evalQuasiquote(args, m.env)
			if err != nil {
				return withErrorPos(err, list.pos)
			}
			m.setValue(result, m.cont)
			return nil
		case "syntax":
			result, err := evalSyntaxForm(args, m.env)
			if err != nil {
				return withErrorPos(err, list.pos)
			}
			m.setValue(result, m.cont)
			return nil
		case "syntax-case":
			result, err := evalSyntaxCaseForm(args, m.env)
			if err != nil {
				return withErrorPos(err, list.pos)
			}
			m.setValue(result, m.cont)
			return nil
		case "with-syntax":
			result, err := evalWithSyntaxForm(args, m.env)
			if err != nil {
				return withErrorPos(err, list.pos)
			}
			m.setValue(result, m.cont)
			return nil
		case "lambda":
			result, err := evalLambda(args, m.env)
			if err != nil {
				return withErrorPos(err, list.pos)
			}
			m.setValue(result, m.cont)
			return nil
		case "case-lambda":
			result, err := evalCaseLambda(args, m.env)
			if err != nil {
				return withErrorPos(err, list.pos)
			}
			m.setValue(result, m.cont)
			return nil
		case "begin":
			return m.startSequence(args, m.env, m.cont)
		case "cond":
			return m.startCond(args, m.env, list.pos, m.cont)
		case "case":
			return m.startCase(args, m.env, list.pos, m.cont)
		case "let":
			return withErrorPos(m.startLet(args, m.env, list.pos, m.cont), list.pos)
		case "let*":
			return withErrorPos(m.startLetStar(args, m.env, list.pos, m.cont), list.pos)
		case "letrec":
			return withErrorPos(m.startLetrec(args, m.env, list.pos, false, m.cont), list.pos)
		case "letrec*":
			return withErrorPos(m.startLetrec(args, m.env, list.pos, true, m.cont), list.pos)
		case "guard":
			return withErrorPos(m.startGuard(args, m.env, list.pos, m.cont), list.pos)
		case "do":
			result, err := evalDo(args, m.env)
			if err != nil {
				return withErrorPos(err, list.pos)
			}
			m.setValue(result, m.cont)
			return nil
		}
	}

	return m.startApplication(list, m.env, m.cont)
}

func (m *evalMachine) stepContinue() error {
	switch cont := m.cont.(type) {
	case *sequenceCont:
		cont.index++
		if cont.index >= len(cont.exprs) {
			m.setValue(voidValue{}, cont.next)
			return nil
		}
		if cont.index == len(cont.exprs)-1 {
			m.setEval(cont.exprs[cont.index], cont.env, cont.next)
			return nil
		}
		m.setEval(cont.exprs[cont.index], cont.env, cont)
		return nil
	case *ifCont:
		if isTruthy(m.val) {
			m.setEval(cont.consequent, cont.env, cont.next)
			return nil
		}
		if !cont.hasElse {
			m.setValue(voidValue{}, cont.next)
			return nil
		}
		m.setEval(cont.alternate, cont.env, cont.next)
		return nil
	case *defineValueCont:
		cont.env.define(cont.target.name, m.val)
		m.setValue(voidValue{}, cont.next)
		return nil
	case *setValueCont:
		if cont.target.captured != nil {
			cont.target.captured.value = m.val
			m.setValue(voidValue{}, cont.next)
			return nil
		}

		if !cont.env.set(cont.target.name, m.val) {
			return errorAt(cont.target.pos, "unbound variable: %s", cont.target.name)
		}
		m.setValue(voidValue{}, cont.next)
		return nil
	case *andCont:
		if !isTruthy(m.val) {
			m.setValue(m.val, cont.next)
			return nil
		}
		cont.index++
		if cont.index >= len(cont.args) {
			m.setValue(booleanValue(true), cont.next)
			return nil
		}
		if cont.index == len(cont.args)-1 {
			m.setEval(cont.args[cont.index], cont.env, cont.next)
			return nil
		}
		m.setEval(cont.args[cont.index], cont.env, cont)
		return nil
	case *orCont:
		if isTruthy(m.val) {
			m.setValue(m.val, cont.next)
			return nil
		}
		cont.index++
		if cont.index >= len(cont.args) {
			m.setValue(booleanValue(false), cont.next)
			return nil
		}
		if cont.index == len(cont.args)-1 {
			m.setEval(cont.args[cont.index], cont.env, cont.next)
			return nil
		}
		m.setEval(cont.args[cont.index], cont.env, cont)
		return nil
	case *applicationOperatorCont:
		if len(cont.operands) == 0 {
			return m.enterProcedure(m.val, nil, cont.pos, cont.next)
		}

		last := len(cont.operands) - 1
		m.setEval(cont.operands[last], cont.env, &applicationArgCont{
			proc:     m.val,
			operands: cont.operands,
			index:    last,
			values:   make([]value, len(cont.operands)),
			env:      cont.env,
			pos:      cont.pos,
			next:     cont.next,
		})
		return nil
	case *applicationArgCont:
		cont.values[cont.index] = m.val
		if cont.index == 0 {
			return m.enterProcedure(cont.proc, cont.values, cont.pos, cont.next)
		}

		cont.index--
		m.setEval(cont.operands[cont.index], cont.env, cont)
		return nil
	case *callWithValuesProducerCont:
		return m.enterProcedure(cont.consumer, unpackValues(m.val), cont.pos, cont.next)
	case *condTestCont:
		clause, ok := cont.clauses[cont.index].(listNode)
		if !ok || len(clause.elements) == 0 {
			return errorAt(cont.pos, "cond clauses must be non-empty lists")
		}
		if !isTruthy(m.val) {
			cont.index++
			return m.advanceCond(cont)
		}
		if len(clause.elements) == 1 {
			m.setValue(m.val, cont.next)
			return nil
		}
		if isCondArrowClause(clause) {
			m.setEval(clause.elements[2], cont.env, &condArrowCont{
				arg:  m.val,
				pos:  clause.pos,
				next: cont.next,
			})
			return nil
		}
		return m.startSequence(clause.elements[1:], cont.env, cont.next)
	case *condArrowCont:
		return m.enterProcedure(m.val, []value{cont.arg}, cont.pos, cont.next)
	case *caseKeyCont:
		return m.resumeCase(cont, m.val)
	case *letValueCont:
		cont.values[cont.index] = m.val
		if cont.index+1 < len(cont.specs) {
			cont.index++
			m.setEval(cont.specs[cont.index].expr, cont.env, cont)
			return nil
		}

		letEnv := newEnvironment(cont.env)
		for i, spec := range cont.specs {
			letEnv.define(spec.name, cont.values[i])
		}
		return m.startSequence(cont.body, letEnv, cont.next)
	case *namedLetValueCont:
		cont.values[cont.index] = m.val
		if cont.index+1 < len(cont.specs) {
			cont.index++
			m.setEval(cont.specs[cont.index].expr, cont.env, cont)
			return nil
		}

		letEnv := newEnvironment(cont.env)
		params := make([]string, len(cont.specs))
		for i, spec := range cont.specs {
			params[i] = spec.name
		}
		proc := &closureValue{
			params: params,
			body:   cont.body,
			env:    letEnv,
		}
		letEnv.define(cont.name, proc)
		for i, name := range params {
			letEnv.define(name, cont.values[i])
		}
		return m.startSequence(cont.body, letEnv, cont.next)
	case *letStarValueCont:
		cont.letEnv.define(cont.specs[cont.index].name, m.val)
		if cont.index+1 < len(cont.specs) {
			cont.index++
			m.setEval(cont.specs[cont.index].expr, cont.letEnv, cont)
			return nil
		}
		return m.startSequence(cont.body, cont.letEnv, cont.next)
	case *letrecValueCont:
		if cont.sequential {
			cont.cells[cont.index].value = m.val
			if cont.index+1 < len(cont.specs) {
				cont.index++
				m.setEval(cont.specs[cont.index].expr, cont.letEnv, cont)
				return nil
			}
			return m.startSequence(cont.body, cont.letEnv, cont.next)
		}

		cont.values[cont.index] = m.val
		if cont.index+1 < len(cont.specs) {
			cont.index++
			m.setEval(cont.specs[cont.index].expr, cont.letEnv, cont)
			return nil
		}

		for i, cell := range cont.cells {
			cell.value = cont.values[i]
		}
		return m.startSequence(cont.body, cont.letEnv, cont.next)
	case *dynamicWindAfterInCont:
		m.wind = cont.frame
		return m.enterProcedure(cont.bodyProc, nil, cont.pos, &dynamicWindAfterBodyCont{
			frame: cont.frame,
			pos:   cont.pos,
			next:  cont.next,
		})
	case *dynamicWindAfterBodyCont:
		result := m.val
		m.wind = cont.frame.parent
		return m.enterProcedure(cont.frame.outProc, nil, cont.pos, &dynamicWindAfterOutCont{
			result: result,
			next:   cont.next,
		})
	case *dynamicWindAfterOutCont:
		m.setValue(cont.result, cont.next)
		return nil
	case *handlerRestoreCont:
		if m.handler == cont.frame {
			m.handler = cont.frame.parent
		}
		m.setValue(m.val, cont.next)
		return nil
	case *exceptionDispatchCont:
		return m.continueExceptionDispatch(cont)
	case *guardTestCont:
		if !isTruthy(m.val) {
			return m.startGuardClauses(cont.remaining, cont.env, cont.exn, cont.pos, cont.next)
		}
		if len(cont.clause.elements) == 1 {
			m.setValue(m.val, cont.next)
			return nil
		}
		return m.startSequence(cont.clause.elements[1:], cont.env, cont.next)
	case *windTransitionCont:
		if cont.setWind != nil {
			m.wind = cont.setWind
		}
		return m.startWindTransition(cont.leave, cont.enter, cont.value, cont.targetCont, cont.targetWind, cont.targetHandler, cont.pos)
	default:
		return &EvalError{Message: "invalid continuation"}
	}
}

func (m *evalMachine) startApplication(list listNode, env *environment, next continuation) error {
	m.setEval(list.elements[0], env, &applicationOperatorCont{
		operands: list.elements[1:],
		env:      env,
		pos:      list.pos,
		next:     next,
	})
	return nil
}

func (m *evalMachine) startSequence(exprs []node, env *environment, next continuation) error {
	if len(exprs) == 0 {
		m.setValue(voidValue{}, next)
		return nil
	}
	if len(exprs) == 1 {
		m.setEval(exprs[0], env, next)
		return nil
	}

	m.setEval(exprs[0], env, &sequenceCont{
		exprs: exprs,
		env:   env,
		next:  next,
	})
	return nil
}

func (m *evalMachine) startAnd(args []node, env *environment, next continuation) error {
	if len(args) == 0 {
		m.setValue(booleanValue(true), next)
		return nil
	}
	if len(args) == 1 {
		m.setEval(args[0], env, next)
		return nil
	}

	m.setEval(args[0], env, &andCont{
		args: args,
		env:  env,
		next: next,
	})
	return nil
}

func (m *evalMachine) startOr(args []node, env *environment, next continuation) error {
	if len(args) == 0 {
		m.setValue(booleanValue(false), next)
		return nil
	}
	if len(args) == 1 {
		m.setEval(args[0], env, next)
		return nil
	}

	m.setEval(args[0], env, &orCont{
		args: args,
		env:  env,
		next: next,
	})
	return nil
}

func (m *evalMachine) startDefine(args []node, env *environment, pos sourcePos, next continuation) error {
	if len(args) < 2 {
		return &EvalError{Message: "define expects a name and value"}
	}

	switch target := args[0].(type) {
	case symbolNode:
		if len(args) != 2 {
			return &EvalError{Message: "define expects exactly 2 arguments"}
		}
		m.setEval(args[1], env, &defineValueCont{
			target: target,
			env:    env,
			next:   next,
		})
		return nil
	case listNode:
		if len(target.elements) == 0 {
			return &EvalError{Message: "define requires a function name"}
		}

		name, ok := symbolName(target.elements[0])
		if !ok {
			return &EvalError{Message: "define requires a function name"}
		}

		proc, err := makeClosure(target.elements[1:], args[1:], env)
		if err != nil {
			return withErrorPos(err, pos)
		}
		env.define(name, proc)
		m.setValue(voidValue{}, next)
		return nil
	default:
		return &EvalError{Message: "define requires a symbol"}
	}
}

func (m *evalMachine) startSet(args []node, env *environment, pos sourcePos, next continuation) error {
	if len(args) != 2 {
		return &EvalError{Message: "set! expects exactly 2 arguments"}
	}

	target, ok := args[0].(symbolNode)
	if !ok {
		return &EvalError{Message: "set! requires a symbol"}
	}

	m.setEval(args[1], env, &setValueCont{
		target: target,
		env:    env,
		next:   next,
	})
	return nil
}

func (m *evalMachine) startIf(args []node, env *environment, next continuation) error {
	if len(args) != 2 && len(args) != 3 {
		return &EvalError{Message: "if expects 2 or 3 arguments"}
	}

	frame := &ifCont{
		consequent: args[1],
		env:        env,
		next:       next,
	}
	if len(args) == 3 {
		frame.alternate = args[2]
		frame.hasElse = true
	}

	m.setEval(args[0], env, frame)
	return nil
}

func (m *evalMachine) startCond(clauses []node, env *environment, pos sourcePos, next continuation) error {
	return m.startCondAt(clauses, 0, env, pos, next)
}

func (m *evalMachine) startCondAt(clauses []node, index int, env *environment, pos sourcePos, next continuation) error {
	if len(clauses) == 0 {
		m.setValue(voidValue{}, next)
		return nil
	}

	frame := &condTestCont{
		clauses: clauses,
		index:   index,
		env:     env,
		pos:     pos,
		next:    next,
	}
	return m.advanceCond(frame)
}

func (m *evalMachine) advanceCond(frame *condTestCont) error {
	for {
		if frame.index >= len(frame.clauses) {
			m.setValue(voidValue{}, frame.next)
			return nil
		}

		clause, ok := frame.clauses[frame.index].(listNode)
		if !ok || len(clause.elements) == 0 {
			return errorAt(frame.pos, "cond clauses must be non-empty lists")
		}

		if name, ok := symbolName(clause.elements[0]); ok && name == "else" {
			if frame.index != len(frame.clauses)-1 {
				return errorAt(frame.pos, "else clause must be last")
			}
			if len(clause.elements) == 1 {
				m.setValue(voidValue{}, frame.next)
				return nil
			}
			return m.startSequence(clause.elements[1:], frame.env, frame.next)
		}

		m.setEval(clause.elements[0], frame.env, frame)
		return nil
	}
}

func (m *evalMachine) startCase(args []node, env *environment, pos sourcePos, next continuation) error {
	if len(args) < 2 {
		return errorAt(pos, "case expects a key and at least 1 clause")
	}

	m.setEval(args[0], env, &caseKeyCont{
		clauses: args[1:],
		env:     env,
		pos:     pos,
		next:    next,
	})
	return nil
}

func (m *evalMachine) resumeCase(cont *caseKeyCont, key value) error {
	for i, clauseExpr := range cont.clauses {
		clause, ok := clauseExpr.(listNode)
		if !ok || len(clause.elements) == 0 {
			return errorAt(cont.pos, "case clauses must be non-empty lists")
		}

		if name, ok := symbolName(clause.elements[0]); ok && name == "else" {
			if i != len(cont.clauses)-1 {
				return errorAt(cont.pos, "else clause must be last")
			}
			if len(clause.elements) == 1 {
				m.setValue(voidValue{}, cont.next)
				return nil
			}
			return m.startSequence(clause.elements[1:], cont.env, cont.next)
		}

		datums, ok := clause.elements[0].(listNode)
		if !ok {
			return errorAt(cont.pos, "case clause datums must be a list")
		}

		for _, datumExpr := range datums.elements {
			datum, err := datumFromNode(datumExpr)
			if err != nil {
				return err
			}
			if eqValues(key, datum) {
				if len(clause.elements) == 1 {
					m.setValue(voidValue{}, cont.next)
					return nil
				}
				return m.startSequence(clause.elements[1:], cont.env, cont.next)
			}
		}
	}

	m.setValue(voidValue{}, cont.next)
	return nil
}

func (m *evalMachine) startLet(args []node, env *environment, pos sourcePos, next continuation) error {
	if len(args) < 2 {
		return &EvalError{Message: "let expects bindings and a body"}
	}

	if name, ok := symbolName(args[0]); ok {
		return m.startNamedLet(name, args[1:], env, pos, next)
	}

	bindingList, ok := args[0].(listNode)
	if !ok {
		return &EvalError{Message: "let bindings must be a list"}
	}

	specs, err := parseNamedBindings(bindingList.elements, "let")
	if err != nil {
		return err
	}

	letEnv := newEnvironment(env)
	if len(specs) == 0 {
		return m.startSequence(args[1:], letEnv, next)
	}

	m.setEval(specs[0].expr, env, &letValueCont{
		specs:  specs,
		values: make([]value, len(specs)),
		env:    env,
		body:   args[1:],
		next:   next,
	})
	return nil
}

func (m *evalMachine) startNamedLet(name string, args []node, env *environment, pos sourcePos, next continuation) error {
	if len(args) < 2 {
		return &EvalError{Message: "named let expects bindings and a body"}
	}

	bindingList, ok := args[0].(listNode)
	if !ok {
		return &EvalError{Message: "named let bindings must be a list"}
	}

	specs, err := parseNamedBindings(bindingList.elements, "let")
	if err != nil {
		return err
	}

	if len(specs) == 0 {
		letEnv := newEnvironment(env)
		proc := &closureValue{
			body: args[1:],
			env:  letEnv,
		}
		letEnv.define(name, proc)
		return m.startSequence(args[1:], letEnv, next)
	}

	m.setEval(specs[0].expr, env, &namedLetValueCont{
		name:   name,
		specs:  specs,
		values: make([]value, len(specs)),
		env:    env,
		body:   args[1:],
		next:   next,
	})
	return nil
}

func (m *evalMachine) startLetStar(args []node, env *environment, pos sourcePos, next continuation) error {
	if len(args) < 2 {
		return &EvalError{Message: "let* expects bindings and a body"}
	}

	bindingList, ok := args[0].(listNode)
	if !ok {
		return &EvalError{Message: "let* bindings must be a list"}
	}

	specs, err := parseNamedBindingsAllowDuplicates(bindingList.elements, "let*")
	if err != nil {
		return err
	}

	letEnv := newEnvironment(env)
	if len(specs) == 0 {
		return m.startSequence(args[1:], letEnv, next)
	}

	m.setEval(specs[0].expr, letEnv, &letStarValueCont{
		specs:  specs,
		letEnv: letEnv,
		body:   args[1:],
		next:   next,
	})
	return nil
}

func (m *evalMachine) startLetrec(args []node, env *environment, pos sourcePos, sequential bool, next continuation) error {
	formName := "letrec"
	if sequential {
		formName = "letrec*"
	}

	if len(args) < 2 {
		return &EvalError{Message: formName + " expects bindings and a body"}
	}

	bindingList, ok := args[0].(listNode)
	if !ok {
		return &EvalError{Message: formName + " bindings must be a list"}
	}

	specs, err := parseNamedBindings(bindingList.elements, formName)
	if err != nil {
		return err
	}

	letEnv := newEnvironment(env)
	cells := make([]*binding, len(specs))
	for i, spec := range specs {
		cell := &binding{value: voidValue{}}
		letEnv.defineBinding(spec.name, cell)
		cells[i] = cell
	}

	if len(specs) == 0 {
		return m.startSequence(args[1:], letEnv, next)
	}

	m.setEval(specs[0].expr, letEnv, &letrecValueCont{
		specs:      specs,
		sequential: sequential,
		letEnv:     letEnv,
		cells:      cells,
		values:     make([]value, len(specs)),
		body:       args[1:],
		next:       next,
	})
	return nil
}

func (m *evalMachine) startDynamicWind(inProc value, bodyProc value, outProc value, pos sourcePos, next continuation) error {
	frame := &windFrame{
		inProc:  inProc,
		outProc: outProc,
		parent:  m.wind,
	}
	return m.enterProcedure(inProc, nil, pos, &dynamicWindAfterInCont{
		frame:    frame,
		bodyProc: bodyProc,
		pos:      pos,
		next:     next,
	})
}

func (m *evalMachine) resumeContinuation(val value, targetCont continuation, targetWind *windFrame, targetHandler *exceptionHandlerFrame, pos sourcePos) error {
	leave, enter := diffWindFrames(m.wind, targetWind)
	return m.startWindTransition(leave, enter, val, targetCont, targetWind, targetHandler, pos)
}

func (m *evalMachine) startWindTransition(leave []*windFrame, enter []*windFrame, val value, targetCont continuation, targetWind *windFrame, targetHandler *exceptionHandlerFrame, pos sourcePos) error {
	if len(leave) > 0 {
		frame := leave[0]
		m.wind = frame.parent
		return m.enterProcedure(frame.outProc, nil, pos, &windTransitionCont{
			leave:         leave[1:],
			enter:         enter,
			value:         val,
			targetCont:    targetCont,
			targetWind:    targetWind,
			targetHandler: targetHandler,
			pos:           pos,
		})
	}

	if len(enter) > 0 {
		frame := enter[0]
		return m.enterProcedure(frame.inProc, nil, pos, &windTransitionCont{
			leave:         leave,
			enter:         enter[1:],
			setWind:       frame,
			value:         val,
			targetCont:    targetCont,
			targetWind:    targetWind,
			targetHandler: targetHandler,
			pos:           pos,
		})
	}

	m.wind = targetWind
	m.handler = targetHandler
	m.setValue(val, targetCont)
	return nil
}

func (m *evalMachine) enterProcedure(proc value, args []value, pos sourcePos, next continuation) error {
	switch proc := proc.(type) {
	case builtinProc:
		result, err := proc(args)
		if err != nil {
			return withErrorPos(err, pos)
		}
		m.setValue(result, next)
		return nil
	case *closureValue:
		return m.startClosureCall(proc, args, pos, next)
	case *caseClosureValue:
		for _, clause := range proc.clauses {
			if closureAcceptsArgCount(clause, len(args)) {
				return m.startClosureCall(clause, args, pos, next)
			}
		}
		return errorAt(pos, "no matching case-lambda clause for %d arguments", len(args))
	case *continuationValue:
		return m.resumeContinuation(packValues(args), cloneContinuation(proc.cont), proc.wind, proc.handler, pos)
	case *callCCProcValue:
		if len(args) != 1 {
			return errorAt(pos, "call/cc expects exactly 1 argument")
		}
		return m.enterProcedure(args[0], []value{&continuationValue{
			cont:    cloneContinuation(next),
			wind:    m.wind,
			handler: m.handler,
			ctx:     m.ctx,
		}}, pos, next)
	case *callWithValuesProcValue:
		if len(args) != 2 {
			return errorAt(pos, "call-with-values expects exactly 2 arguments")
		}
		if !isProcedureValue(args[0]) || !isProcedureValue(args[1]) {
			return errorAt(pos, "call-with-values expects 2 procedures")
		}
		return m.enterProcedure(args[0], nil, pos, &callWithValuesProducerCont{
			consumer: args[1],
			pos:      pos,
			next:     next,
		})
	case *dynamicWindProcValue:
		if len(args) != 3 {
			return errorAt(pos, "dynamic-wind expects exactly 3 arguments")
		}
		if !isProcedureValue(args[0]) || !isProcedureValue(args[1]) || !isProcedureValue(args[2]) {
			return errorAt(pos, "dynamic-wind expects 3 procedures")
		}
		return m.startDynamicWind(args[0], args[1], args[2], pos, next)
	case *raiseProcValue:
		if len(args) != 1 {
			return errorAt(pos, "raise expects exactly 1 argument")
		}
		return m.raiseValue(args[0], pos)
	case *withExceptionHandlerProcValue:
		if len(args) != 2 {
			return errorAt(pos, "with-exception-handler expects exactly 2 arguments")
		}
		if !isProcedureValue(args[0]) || !isProcedureValue(args[1]) {
			return errorAt(pos, "with-exception-handler expects 2 procedures")
		}
		return m.startWithExceptionHandler(args[0], args[1], pos, next)
	default:
		return errorAt(pos, "not a procedure")
	}
}

func (m *evalMachine) startClosureCall(proc *closureValue, args []value, pos sourcePos, next continuation) error {
	if !proc.hasRest && len(args) != len(proc.params) {
		return errorAt(pos, "expected %d arguments, got %d", len(proc.params), len(args))
	}
	if proc.hasRest && len(args) < len(proc.params) {
		return errorAt(pos, "expected at least %d arguments, got %d", len(proc.params), len(args))
	}

	callEnv := newEnvironment(proc.env)
	for i, name := range proc.params {
		callEnv.define(name, args[i])
	}
	if proc.hasRest {
		callEnv.define(proc.restParam, makeList(copyValues(args[len(proc.params):])))
	}

	return m.startSequence(proc.body, callEnv, next)
}

func diffWindFrames(current *windFrame, target *windFrame) ([]*windFrame, []*windFrame) {
	currentFrames := collectWindFrames(current)
	targetFrames := collectWindFrames(target)

	i := len(currentFrames) - 1
	j := len(targetFrames) - 1
	for i >= 0 && j >= 0 && currentFrames[i] == targetFrames[j] {
		i--
		j--
	}

	leave := currentFrames[:i+1]
	enter := reverseWindFrames(targetFrames[:j+1])
	return leave, enter
}

func collectWindFrames(frame *windFrame) []*windFrame {
	var frames []*windFrame
	for current := frame; current != nil; current = current.parent {
		frames = append(frames, current)
	}
	return frames
}

func reverseWindFrames(frames []*windFrame) []*windFrame {
	result := make([]*windFrame, len(frames))
	for i, frame := range frames {
		result[len(frames)-1-i] = frame
	}
	return result
}

func cloneContinuation(cont continuation) continuation {
	switch cont := cont.(type) {
	case nil:
		return nil
	case *sequenceCont:
		return &sequenceCont{
			exprs: cont.exprs,
			index: cont.index,
			env:   cont.env,
			next:  cloneContinuation(cont.next),
		}
	case *ifCont:
		return &ifCont{
			consequent: cont.consequent,
			alternate:  cont.alternate,
			hasElse:    cont.hasElse,
			env:        cont.env,
			next:       cloneContinuation(cont.next),
		}
	case *defineValueCont:
		return &defineValueCont{
			target: cont.target,
			env:    cont.env,
			next:   cloneContinuation(cont.next),
		}
	case *setValueCont:
		return &setValueCont{
			target: cont.target,
			env:    cont.env,
			next:   cloneContinuation(cont.next),
		}
	case *andCont:
		return &andCont{
			args:  cont.args,
			index: cont.index,
			env:   cont.env,
			next:  cloneContinuation(cont.next),
		}
	case *orCont:
		return &orCont{
			args:  cont.args,
			index: cont.index,
			env:   cont.env,
			next:  cloneContinuation(cont.next),
		}
	case *applicationOperatorCont:
		return &applicationOperatorCont{
			operands: cont.operands,
			env:      cont.env,
			pos:      cont.pos,
			next:     cloneContinuation(cont.next),
		}
	case *applicationArgCont:
		return &applicationArgCont{
			proc:     cont.proc,
			operands: cont.operands,
			index:    cont.index,
			values:   append([]value(nil), cont.values...),
			env:      cont.env,
			pos:      cont.pos,
			next:     cloneContinuation(cont.next),
		}
	case *callWithValuesProducerCont:
		return &callWithValuesProducerCont{
			consumer: cont.consumer,
			pos:      cont.pos,
			next:     cloneContinuation(cont.next),
		}
	case *condTestCont:
		return &condTestCont{
			clauses: cont.clauses,
			index:   cont.index,
			env:     cont.env,
			pos:     cont.pos,
			next:    cloneContinuation(cont.next),
		}
	case *condArrowCont:
		return &condArrowCont{
			arg:  cont.arg,
			pos:  cont.pos,
			next: cloneContinuation(cont.next),
		}
	case *caseKeyCont:
		return &caseKeyCont{
			clauses: cont.clauses,
			env:     cont.env,
			pos:     cont.pos,
			next:    cloneContinuation(cont.next),
		}
	case *letValueCont:
		return &letValueCont{
			specs:  cont.specs,
			index:  cont.index,
			values: append([]value(nil), cont.values...),
			env:    cont.env,
			body:   cont.body,
			next:   cloneContinuation(cont.next),
		}
	case *namedLetValueCont:
		return &namedLetValueCont{
			name:   cont.name,
			specs:  cont.specs,
			index:  cont.index,
			values: append([]value(nil), cont.values...),
			env:    cont.env,
			body:   cont.body,
			next:   cloneContinuation(cont.next),
		}
	case *letStarValueCont:
		return &letStarValueCont{
			specs:  cont.specs,
			index:  cont.index,
			letEnv: cont.letEnv,
			body:   cont.body,
			next:   cloneContinuation(cont.next),
		}
	case *letrecValueCont:
		return &letrecValueCont{
			specs:      cont.specs,
			index:      cont.index,
			sequential: cont.sequential,
			letEnv:     cont.letEnv,
			cells:      cont.cells,
			values:     append([]value(nil), cont.values...),
			body:       cont.body,
			next:       cloneContinuation(cont.next),
		}
	case *dynamicWindAfterInCont:
		return &dynamicWindAfterInCont{
			frame:    cont.frame,
			bodyProc: cont.bodyProc,
			pos:      cont.pos,
			next:     cloneContinuation(cont.next),
		}
	case *dynamicWindAfterBodyCont:
		return &dynamicWindAfterBodyCont{
			frame: cont.frame,
			pos:   cont.pos,
			next:  cloneContinuation(cont.next),
		}
	case *dynamicWindAfterOutCont:
		return &dynamicWindAfterOutCont{
			result: cont.result,
			next:   cloneContinuation(cont.next),
		}
	case *windTransitionCont:
		return &windTransitionCont{
			leave:         cont.leave,
			enter:         cont.enter,
			setWind:       cont.setWind,
			value:         cont.value,
			targetCont:    cloneContinuation(cont.targetCont),
			targetWind:    cont.targetWind,
			targetHandler: cont.targetHandler,
			pos:           cont.pos,
		}
	case *handlerRestoreCont:
		return &handlerRestoreCont{
			frame: cont.frame,
			next:  cloneContinuation(cont.next),
		}
	case *exceptionDispatchCont:
		return &exceptionDispatchCont{
			frame: cont.frame,
			pos:   cont.pos,
		}
	case *guardTestCont:
		return &guardTestCont{
			clause:    cont.clause,
			remaining: cont.remaining,
			env:       cont.env,
			exn:       cont.exn,
			pos:       cont.pos,
			next:      cloneContinuation(cont.next),
		}
	default:
		return cont
	}
}
