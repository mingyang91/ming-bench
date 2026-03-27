package ming

type machineMode int

const (
	machineModeEvalExpr machineMode = iota
	machineModeHaveValue
)

type machine struct {
	mode machineMode
	expr locatedExpr
	env  *env
	val  value
	cont continuation
	wind *windFrame
}

type continuation interface {
	resume(m *machine, v value) error
}

type sequenceFrame struct {
	remaining []locatedExpr
	env       *env
	next      continuation
}

type andFrame struct {
	remaining []locatedExpr
	env       *env
	next      continuation
}

type orFrame struct {
	remaining []locatedExpr
	env       *env
	next      continuation
}

type ifFrame struct {
	consequent locatedExpr
	alternate  locatedExpr
	hasAlt     bool
	env        *env
	next       continuation
}

type defineValueFrame struct {
	name string
	env  *env
	next continuation
}

type setValueFrame struct {
	name      string
	targetPos SourcePos
	env       *env
	next      continuation
}

type applyOperatorFrame struct {
	items listExpr
	env   *env
	next  continuation
}

type applyArgsFrame struct {
	proc        procedure
	operatorPos SourcePos
	remaining   []locatedExpr
	suffix      []value
	env         *env
	next        continuation
}

type letFrame struct {
	bindings  []letBinding
	body      []locatedExpr
	outerEnv  *env
	nextIndex int
	values    []value
	next      continuation
}

type namedLetFrame struct {
	name      string
	namePos   SourcePos
	params    []string
	bindings  []letBinding
	body      []locatedExpr
	outerEnv  *env
	nextIndex int
	values    []value
	next      continuation
}

type condFrame struct {
	clauses []locatedExpr
	index   int
	env     *env
	next    continuation
}

type callCCProc struct {
	name string
}

type continuationProc struct {
	captured continuation
	wind     *windFrame
}

func (p callCCProc) schemeString() string {
	return "#<procedure:" + p.name + ">"
}

func (callCCProc) isTruthy() bool {
	return true
}

func (p callCCProc) call(args []value) (value, error) {
	if len(args) != 1 {
		return nil, newCurrentEvalError("'%s' expects exactly 1 argument", p.name)
	}

	target, ok := args[0].(procedure)
	if !ok {
		return nil, newCurrentEvalError("'%s' expects a procedure, got %s", p.name, args[0].schemeString())
	}

	return applyProcedureWithContinuation(
		target,
		[]value{continuationProc{}},
		currentEvalPos,
		nil,
	)
}

func (continuationProc) schemeString() string {
	return "#<continuation>"
}

func (continuationProc) isTruthy() bool {
	return true
}

func (p continuationProc) call(args []value) (value, error) {
	if len(args) != 1 {
		return nil, newCurrentEvalError("continuation expects exactly 1 argument")
	}

	return evalValueWithWindTransition(args[0], nil, p.wind, p.captured)
}

func (m *machine) run() (value, error) {
	for {
		switch m.mode {
		case machineModeEvalExpr:
			if err := m.stepExpr(); err != nil {
				return nil, err
			}
		case machineModeHaveValue:
			if m.cont == nil {
				return m.val, nil
			}
			if err := m.cont.resume(m, m.val); err != nil {
				return nil, err
			}
		default:
			return nil, newCurrentEvalError("unknown machine state")
		}
	}
}

func (m *machine) stepExpr() error {
	setCurrentEvalPos(m.expr.pos)

	switch expr := m.expr.form.(type) {
	case numberExpr:
		m.setValue(expr, m.cont)
		return nil
	case boolExpr:
		m.setValue(boolValue(expr), m.cont)
		return nil
	case stringExpr:
		m.setValue(newStringValue(string(expr)), m.cont)
		return nil
	case charExpr:
		m.setValue(charValue(expr), m.cont)
		return nil
	case symbolExpr:
		binding, ok := m.env.lookupBinding(string(expr))
		if !ok {
			return newCurrentEvalError("unbound variable: %s", string(expr))
		}
		if _, isUninitialized := binding.value.(uninitializedValue); isUninitialized {
			return newCurrentEvalError("uninitialized variable: %s", string(expr))
		}
		m.setValue(binding.value, m.cont)
		return nil
	case listExpr:
		return m.evalList(expr, m.env, m.cont)
	default:
		return newCurrentEvalError("unknown expression")
	}
}

func (m *machine) evalList(items listExpr, env *env, cont continuation) error {
	if len(items) == 0 {
		return newCurrentEvalError("cannot evaluate empty list")
	}

	if operator, ok := items[0].form.(symbolExpr); ok {
		switch string(operator) {
		case "and":
			return startAnd(m, items[1:], env, cont)
		case "or":
			return startOr(m, items[1:], env, cont)
		case "begin":
			return startSequence(m, items[1:], env, cont)
		case "if":
			return startIf(m, items[1:], env, cont)
		case "cond":
			return startCond(m, items[1:], env, cont)
		case "define":
			return startDefine(m, items[1:], env, cont)
		case "define-syntax":
			v, err := evalDefineSyntax(items[1:], env)
			if err != nil {
				return err
			}
			m.setValue(v, cont)
			return nil
		case "define-record-type":
			v, err := evalDefineRecordType(items[1:], env)
			if err != nil {
				return err
			}
			m.setValue(v, cont)
			return nil
		case "set!":
			return startSet(m, items[1:], env, cont)
		case "quote":
			v, err := evalQuote(items[1:])
			if err != nil {
				return err
			}
			m.setValue(v, cont)
			return nil
		case "let":
			return startLet(m, items[1:], env, cont)
		case "lambda":
			v, err := evalLambda(items[1:], env)
			if err != nil {
				return err
			}
			m.setValue(v, cont)
			return nil
		case "case-lambda":
			v, err := evalCaseLambda(items[1:], env)
			if err != nil {
				return err
			}
			m.setValue(v, cont)
			return nil
		case "let*":
			v, err := evalLetStar(items[1:], env)
			if err != nil {
				return err
			}
			m.setValue(v, cont)
			return nil
		case "letrec":
			v, err := evalLetrec(items[1:], env)
			if err != nil {
				return err
			}
			m.setValue(v, cont)
			return nil
		case "letrec*":
			v, err := evalLetrecStar(items[1:], env)
			if err != nil {
				return err
			}
			m.setValue(v, cont)
			return nil
		case "case":
			v, err := evalCase(items[1:], env)
			if err != nil {
				return err
			}
			m.setValue(v, cont)
			return nil
		case "do":
			v, err := evalDo(items[1:], env)
			if err != nil {
				return err
			}
			m.setValue(v, cont)
			return nil
		}

		if macro, found := env.lookupMacro(string(operator)); found {
			expanded, expansionEnv, err := expandMacroCall(items, macro, env)
			if err != nil {
				return err
			}
			m.setExpr(expanded, expansionEnv, cont)
			return nil
		}
	}

	return startApplication(m, items, env, cont)
}

func (m *machine) setExpr(expr locatedExpr, env *env, cont continuation) {
	m.mode = machineModeEvalExpr
	m.expr = expr
	m.env = env
	m.cont = cont
}

func (m *machine) setValue(v value, cont continuation) {
	m.mode = machineModeHaveValue
	m.val = v
	m.cont = cont
}

func (f *sequenceFrame) resume(m *machine, _ value) error {
	return startSequence(m, f.remaining, f.env, f.next)
}

func (f *andFrame) resume(m *machine, v value) error {
	if !v.isTruthy() {
		m.setValue(v, f.next)
		return nil
	}
	return startAnd(m, f.remaining, f.env, f.next)
}

func (f *orFrame) resume(m *machine, v value) error {
	if v.isTruthy() {
		m.setValue(v, f.next)
		return nil
	}
	return startOr(m, f.remaining, f.env, f.next)
}

func (f *ifFrame) resume(m *machine, v value) error {
	if v.isTruthy() {
		m.setExpr(f.consequent, f.env, f.next)
		return nil
	}
	if f.hasAlt {
		m.setExpr(f.alternate, f.env, f.next)
		return nil
	}
	m.setValue(voidValue{}, f.next)
	return nil
}

func (f *defineValueFrame) resume(m *machine, v value) error {
	f.env.define(f.name, v)
	m.setValue(voidValue{}, f.next)
	return nil
}

func (f *setValueFrame) resume(m *machine, v value) error {
	setCurrentEvalPos(f.targetPos)
	if !f.env.set(f.name, v) {
		return newCurrentEvalError("unbound variable: %s", f.name)
	}
	m.setValue(voidValue{}, f.next)
	return nil
}

func (f *applyOperatorFrame) resume(m *machine, v value) error {
	proc, ok := v.(procedure)
	if !ok {
		setCurrentEvalPos(f.items[0].pos)
		return newCurrentEvalError("attempt to call non-procedure: %s", v.schemeString())
	}

	if len(f.items) == 1 {
		return applyProcedureState(m, proc, nil, f.items[0].pos, f.next)
	}

	lastArg := f.items[len(f.items)-1]
	m.setExpr(lastArg, f.env, &applyArgsFrame{
		proc:        proc,
		operatorPos: f.items[0].pos,
		remaining:   f.items[1 : len(f.items)-1],
		env:         f.env,
		next:        f.next,
	})
	return nil
}

func (f *applyArgsFrame) resume(m *machine, v value) error {
	suffix := prependValue(v, f.suffix)
	if len(f.remaining) == 0 {
		return applyProcedureState(m, f.proc, suffix, f.operatorPos, f.next)
	}

	last := f.remaining[len(f.remaining)-1]
	m.setExpr(last, f.env, &applyArgsFrame{
		proc:        f.proc,
		operatorPos: f.operatorPos,
		remaining:   f.remaining[:len(f.remaining)-1],
		suffix:      suffix,
		env:         f.env,
		next:        f.next,
	})
	return nil
}

func (f *letFrame) resume(m *machine, v value) error {
	values := appendValue(f.values, v)
	if f.nextIndex < len(f.bindings) {
		nextBinding := f.bindings[f.nextIndex]
		m.setExpr(nextBinding.init, f.outerEnv, &letFrame{
			bindings:  f.bindings,
			body:      f.body,
			outerEnv:  f.outerEnv,
			nextIndex: f.nextIndex + 1,
			values:    values,
			next:      f.next,
		})
		return nil
	}

	letEnv := newEnv(f.outerEnv)
	for i, binding := range f.bindings {
		letEnv.define(binding.name, values[i])
	}
	return startSequence(m, f.body, letEnv, f.next)
}

func (f *namedLetFrame) resume(m *machine, v value) error {
	values := appendValue(f.values, v)
	if f.nextIndex < len(f.bindings) {
		nextBinding := f.bindings[f.nextIndex]
		m.setExpr(nextBinding.init, f.outerEnv, &namedLetFrame{
			name:      f.name,
			namePos:   f.namePos,
			params:    f.params,
			bindings:  f.bindings,
			body:      f.body,
			outerEnv:  f.outerEnv,
			nextIndex: f.nextIndex + 1,
			values:    values,
			next:      f.next,
		})
		return nil
	}

	letEnv := newEnv(f.outerEnv)
	proc := closureValue{
		params: f.params,
		body:   f.body,
		env:    letEnv,
	}
	letEnv.define(f.name, proc)
	return applyProcedureState(m, proc, values, f.namePos, f.next)
}

func (f *condFrame) resume(m *machine, v value) error {
	clauseExpr := f.clauses[f.index]
	clause := clauseExpr.form.(listExpr)
	if v.isTruthy() {
		if len(clause) == 1 {
			m.setValue(v, f.next)
			return nil
		}
		return startSequence(m, clause[1:], f.env, f.next)
	}
	return advanceCond(m, f.clauses, f.index+1, f.env, f.next)
}

func evalWithContinuation(expr locatedExpr, env *env, cont continuation) (value, error) {
	m := machine{}
	m.setExpr(expr, env, cont)
	return m.run()
}

func evalSequenceWithContinuation(exprs []locatedExpr, env *env, cont continuation) (value, error) {
	m := machine{}
	if err := startSequence(&m, exprs, env, cont); err != nil {
		return nil, err
	}
	return m.run()
}

func evalValueWithContinuation(v value, cont continuation) (value, error) {
	m := machine{}
	m.setValue(v, cont)
	return m.run()
}

func applyProcedureWithContinuation(proc procedure, args []value, pos SourcePos, cont continuation) (value, error) {
	m := machine{}
	if err := applyProcedureState(&m, proc, args, pos, cont); err != nil {
		return nil, err
	}
	return m.run()
}

func startSequence(m *machine, exprs []locatedExpr, env *env, cont continuation) error {
	switch len(exprs) {
	case 0:
		m.setValue(voidValue{}, cont)
	case 1:
		m.setExpr(exprs[0], env, cont)
	default:
		m.setExpr(exprs[0], env, &sequenceFrame{
			remaining: exprs[1:],
			env:       env,
			next:      cont,
		})
	}
	return nil
}

func startAnd(m *machine, items []locatedExpr, env *env, cont continuation) error {
	switch len(items) {
	case 0:
		m.setValue(boolValue(true), cont)
	case 1:
		m.setExpr(items[0], env, cont)
	default:
		m.setExpr(items[0], env, &andFrame{
			remaining: items[1:],
			env:       env,
			next:      cont,
		})
	}
	return nil
}

func startOr(m *machine, items []locatedExpr, env *env, cont continuation) error {
	switch len(items) {
	case 0:
		m.setValue(boolValue(false), cont)
	case 1:
		m.setExpr(items[0], env, cont)
	default:
		m.setExpr(items[0], env, &orFrame{
			remaining: items[1:],
			env:       env,
			next:      cont,
		})
	}
	return nil
}

func startIf(m *machine, parts []locatedExpr, env *env, cont continuation) error {
	if len(parts) != 2 && len(parts) != 3 {
		return newCurrentEvalError("'if' expects 2 or 3 arguments")
	}

	frame := &ifFrame{
		consequent: parts[1],
		env:        env,
		next:       cont,
	}
	if len(parts) == 3 {
		frame.alternate = parts[2]
		frame.hasAlt = true
	}

	m.setExpr(parts[0], env, frame)
	return nil
}

func startCond(m *machine, clauses []locatedExpr, env *env, cont continuation) error {
	return advanceCond(m, clauses, 0, env, cont)
}

func advanceCond(m *machine, clauses []locatedExpr, index int, env *env, cont continuation) error {
	if index >= len(clauses) {
		m.setValue(voidValue{}, cont)
		return nil
	}

	clauseExpr := clauses[index]
	clause, ok := clauseExpr.form.(listExpr)
	if !ok || len(clause) == 0 {
		return newEvalError(clauseExpr.pos, "'cond' clauses must be non-empty lists")
	}

	if keyword, ok := clause[0].form.(symbolExpr); ok && string(keyword) == "else" {
		if index != len(clauses)-1 {
			return newEvalError(clause[0].pos, "'cond' else clause must be last")
		}
		if len(clause) == 1 {
			m.setValue(voidValue{}, cont)
			return nil
		}
		return startSequence(m, clause[1:], env, cont)
	}

	m.setExpr(clause[0], env, &condFrame{
		clauses: clauses,
		index:   index,
		env:     env,
		next:    cont,
	})
	return nil
}

func startDefine(m *machine, parts []locatedExpr, env *env, cont continuation) error {
	if len(parts) < 2 {
		return newCurrentEvalError("'define' expects at least 2 arguments")
	}

	switch target := parts[0].form.(type) {
	case symbolExpr:
		if len(parts) != 2 {
			return newCurrentEvalError("'define' expects exactly 2 arguments for variable definitions")
		}
		m.setExpr(parts[1], env, &defineValueFrame{
			name: string(target),
			env:  env,
			next: cont,
		})
		return nil
	case listExpr:
		v, err := evalDefine(parts, env)
		if err != nil {
			return err
		}
		m.setValue(v, cont)
		return nil
	default:
		return newCurrentEvalError("invalid define target")
	}
}

func startSet(m *machine, parts []locatedExpr, env *env, cont continuation) error {
	if len(parts) != 2 {
		return newCurrentEvalError("'set!' expects exactly 2 arguments")
	}

	name, ok := parts[0].form.(symbolExpr)
	if !ok {
		return newEvalError(parts[0].pos, "'set!' target must be a symbol")
	}

	m.setExpr(parts[1], env, &setValueFrame{
		name:      string(name),
		targetPos: parts[0].pos,
		env:       env,
		next:      cont,
	})
	return nil
}

func startLet(m *machine, parts []locatedExpr, env *env, cont continuation) error {
	if len(parts) < 2 {
		return newCurrentEvalError("'let' expects bindings and a body")
	}

	if name, ok := parts[0].form.(symbolExpr); ok {
		if len(parts) < 3 {
			return newCurrentEvalError("named 'let' expects bindings and a body")
		}

		bindingExprs, ok := parts[1].form.(listExpr)
		if !ok {
			return newEvalError(parts[1].pos, "'let' bindings must be a list")
		}

		bindings, err := parseLetBindings(bindingExprs)
		if err != nil {
			return err
		}

		params := make([]string, len(bindings))
		for i, binding := range bindings {
			params[i] = binding.name
		}

		if len(bindings) == 0 {
			letEnv := newEnv(env)
			proc := closureValue{
				params: params,
				body:   parts[2:],
				env:    letEnv,
			}
			letEnv.define(string(name), proc)
			return applyProcedureState(m, proc, nil, parts[0].pos, cont)
		}

		m.setExpr(bindings[0].init, env, &namedLetFrame{
			name:      string(name),
			namePos:   parts[0].pos,
			params:    params,
			bindings:  bindings,
			body:      parts[2:],
			outerEnv:  env,
			nextIndex: 1,
			next:      cont,
		})
		return nil
	}

	bindingExprs, ok := parts[0].form.(listExpr)
	if !ok {
		return newEvalError(parts[0].pos, "'let' bindings must be a list")
	}

	bindings, err := parseLetBindings(bindingExprs)
	if err != nil {
		return err
	}

	if len(bindings) == 0 {
		return startSequence(m, parts[1:], newEnv(env), cont)
	}

	m.setExpr(bindings[0].init, env, &letFrame{
		bindings:  bindings,
		body:      parts[1:],
		outerEnv:  env,
		nextIndex: 1,
		next:      cont,
	})
	return nil
}

func startApplication(m *machine, items listExpr, env *env, cont continuation) error {
	m.setExpr(items[0], env, &applyOperatorFrame{
		items: items,
		env:   env,
		next:  cont,
	})
	return nil
}

func applyProcedureState(m *machine, proc procedure, args []value, pos SourcePos, cont continuation) error {
	setCurrentEvalPos(pos)

	switch p := proc.(type) {
	case callCCProc:
		if len(args) != 1 {
			return newCurrentEvalError("'%s' expects exactly 1 argument", p.name)
		}
		target, ok := args[0].(procedure)
		if !ok {
			return newCurrentEvalError("'%s' expects a procedure, got %s", p.name, args[0].schemeString())
		}
		return applyProcedureState(m, target, []value{continuationProc{captured: cont, wind: m.wind}}, pos, cont)
	case dynamicWindProc:
		inThunk, bodyThunk, outThunk, err := parseDynamicWindArgs(args, p.name)
		if err != nil {
			return err
		}
		return startDynamicWind(m, inThunk, bodyThunk, outThunk, pos, cont)
	case continuationProc:
		if len(args) != 1 {
			return newCurrentEvalError("continuation expects exactly 1 argument")
		}
		return startWindTransition(m, p.wind, p.captured, args[0])
	case closureValue:
		call, err := p.prepareTailCall(args)
		if err != nil {
			return err
		}
		return startSequence(m, call.body, call.env, cont)
	case caseClosureValue:
		call, err := p.prepareTailCall(args)
		if err != nil {
			return err
		}
		return startSequence(m, call.body, call.env, cont)
	default:
		v, err := proc.call(args)
		if err != nil {
			return err
		}
		m.setValue(v, cont)
		return nil
	}
}

func appendValue(items []value, item value) []value {
	result := make([]value, len(items)+1)
	copy(result, items)
	result[len(items)] = item
	return result
}

func prependValue(item value, items []value) []value {
	result := make([]value, len(items)+1)
	result[0] = item
	copy(result[1:], items)
	return result
}

func setCurrentEvalPos(pos SourcePos) {
	currentEvalPos = pos.normalized()
}
