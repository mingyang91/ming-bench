package ming

import "fmt"

type continuationExpr struct {
	cont level18Cont
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
	next level18Cont
}

type level18SetCont struct {
	env    *env
	target symbolExpr
	next   level18Cont
}

type level18IfCont struct {
	env          *env
	thenForm     expr
	elseForm     expr
	hasAlternate bool
	next         level18Cont
}

type level18CallOpCont struct {
	env      *env
	argForms []expr
	pos      sourcePos
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

type level18Machine struct {
	evaluating bool
	env        *env
	form       expr
	value      expr
	cont       level18Cont
}

func builtinContinuationSentinel(args []expr) (expr, error) {
	return nil, &EvalError{Message: "call/cc requires the level 18 evaluator"}
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

func (m *level18Machine) returnValue(value expr, cont level18Cont) {
	m.evaluating = false
	m.value = value
	m.cont = cont
}

func (m *level18Machine) run() (expr, error) {
	for {
		if m.evaluating {
			if err := m.stepEval(); err != nil {
				return nil, err
			}
			continue
		}

		switch cont := m.cont.(type) {
		case nil:
			return m.value, nil
		case *level18SeqCont:
			m.evalSequence(cont.env, cont.rest, cont.next)
		case *level18DefineCont:
			cont.env.define(cont.name, m.value)
			m.returnValue(voidExpr{}, cont.next)
		case *level18SetCont:
			if !cont.env.assign(cont.target.name, m.value) {
				return nil, errorAt(cont.target.pos, fmt.Sprintf("unbound symbol: %s", cont.target.name))
			}
			m.returnValue(voidExpr{}, cont.next)
		case *level18IfCont:
			if isTruthy(m.value) {
				m.eval(cont.env, cont.thenForm, cont.next)
				continue
			}
			if cont.hasAlternate {
				m.eval(cont.env, cont.elseForm, cont.next)
				continue
			}
			m.returnValue(voidExpr{}, cont.next)
		case *level18CallOpCont:
			if len(cont.argForms) == 0 {
				if err := m.apply(m.value, nil, cont.next); err != nil {
					return nil, attachPos(err, cont.pos)
				}
				continue
			}

			values := make([]expr, len(cont.argForms))
			index := len(cont.argForms) - 1
			m.eval(cont.env, cont.argForms[index], &level18CallArgsCont{
				proc:     m.value,
				env:      cont.env,
				argForms: cont.argForms,
				index:    index,
				values:   values,
				pos:      cont.pos,
				next:     cont.next,
			})
		case *level18CallArgsCont:
			values := append([]expr(nil), cont.values...)
			values[cont.index] = m.value

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
		default:
			return nil, &EvalError{Message: "unsupported continuation"}
		}
	}
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
		case "set!":
			return m.stepSet(environment, items.items[1:], operator.pos)
		case "if":
			return m.stepIf(environment, items.items[1:], operator.pos)
		case "begin":
			m.evalSequence(environment, items.items[1:], m.cont)
			return nil
		case "cond":
			expanded, err := level18ExpandCond(items.pos, items.items[1:])
			if err != nil {
				return attachPos(err, operator.pos)
			}
			m.eval(environment, expanded, m.cont)
			return nil
		case "quote":
			if len(items.items) != 2 {
				return attachPos(&EvalError{Message: "quote expects exactly 1 argument"}, operator.pos)
			}
			m.returnValue(quoteDatum(items.items[1]), m.cont)
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
				args = []expr{&continuationExpr{cont: cont}}
				continue
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
			if len(args) != 1 {
				return &EvalError{Message: "continuation expects exactly 1 argument"}
			}
			m.returnValue(args[0], callable.cont)
			return nil
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
