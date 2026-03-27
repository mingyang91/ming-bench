package ming

import (
	"os"
	"strconv"
)

type continuationControl struct {
	expression expr
	env        *environment
	value      any
	isValue    bool
}

type continuationFrame interface{}

type sequenceContinuationFrame struct {
	remaining []expr
	env       *environment
}

type defineContinuationFrame struct {
	name string
	env  *environment
}

type setContinuationFrame struct {
	slot *binding
}

type ifContinuationFrame struct {
	thenExpr expr
	elseExpr expr
	hasElse  bool
	env      *environment
}

type andContinuationFrame struct {
	remaining []expr
	env       *environment
}

type orContinuationFrame struct {
	remaining []expr
	env       *environment
}

type condContinuationFrame struct {
	body      []expr
	remaining []expr
	env       *environment
	recipient expr
	arrowPos  position
}

type condArrowContinuationFrame struct {
	value any
	pos   position
}

type caseContinuationFrame struct {
	clauses []expr
	env     *environment
}

type letContinuationFrame struct {
	named    bool
	name     string
	bindings []letBinding
	index    int
	values   []any
	body     []expr
	outerEnv *environment
	pos      position
}

type letStarContinuationFrame struct {
	bindings []letBinding
	index    int
	letEnv   *environment
	body     []expr
}

type letRecContinuationFrame struct {
	bindings   []letBinding
	index      int
	values     []any
	slots      []*binding
	letEnv     *environment
	body       []expr
	sequential bool
}

type doInitContinuationFrame struct {
	bindings    []doBinding
	index       int
	values      []any
	termination *listExpr
	body        []expr
	outerEnv    *environment
}

type doTestContinuationFrame struct {
	bindings    []doBinding
	termination *listExpr
	body        []expr
	loopEnv     *environment
	slots       []*binding
}

type doAfterBodyContinuationFrame struct {
	bindings    []doBinding
	termination *listExpr
	body        []expr
	loopEnv     *environment
	slots       []*binding
}

type doStepContinuationFrame struct {
	bindings    []doBinding
	index       int
	nextValues  []any
	termination *listExpr
	body        []expr
	loopEnv     *environment
	slots       []*binding
}

type applyOperatorContinuationFrame struct {
	argExprs []expr
	env      *environment
	pos      position
}

type applyArgsContinuationFrame struct {
	procedure any
	remaining []expr
	evaluated []any
	env       *environment
	pos       position
}

type procedureReturnContinuationFrame struct{}

type callCCReturnContinuationFrame struct {
	yieldLike bool
}

type callWithValuesContinuationFrame struct {
	consumer any
	pos      position
}

type mapContinuationFrame struct {
	procedure any
	lists     [][]any
	index     int
	results   []any
	pos       position
}

type forEachContinuationFrame struct {
	procedure any
	lists     [][]any
	index     int
	pos       position
}

type dynamicWind struct {
	before any
	after  any
	pos    position
}

type dynamicWindAfterBeforeFrame struct {
	wind *dynamicWind
	body any
	pos  position
}

type dynamicWindAfterBodyFrame struct {
	wind *dynamicWind
	pos  position
}

type dynamicWindAfterAfterFrame struct {
	result any
}

type continuationSwitchState struct {
	exiting     []*dynamicWind
	entering    []*dynamicWind
	targetStack []continuationFrame
	targetWinds []*dynamicWind
	targetValue any
}

type continuationSwitchFrame struct {
	state    *continuationSwitchState
	entering bool
	wind     *dynamicWind
}

type continuationProcedure struct {
	stack []continuationFrame
	winds []*dynamicWind
}

func continuationsEnabledAtCurrentLevel() bool {
	levelText := os.Getenv("BENCH_LEVEL")
	if levelText == "" {
		return false
	}

	level, err := strconv.Atoi(levelText)
	if err != nil {
		return false
	}

	return level >= 18
}

func newExpressionControl(expression expr, env *environment) continuationControl {
	return continuationControl{
		expression: expression,
		env:        env,
	}
}

func newValueControl(value any) continuationControl {
	return continuationControl{
		value:   value,
		isValue: true,
	}
}

func cloneContinuationStack(stack []continuationFrame) []continuationFrame {
	return append([]continuationFrame(nil), stack...)
}

func cloneDynamicWinds(winds []*dynamicWind) []*dynamicWind {
	return append([]*dynamicWind(nil), winds...)
}

func appendAnyValue(values []any, value any) []any {
	next := make([]any, len(values)+1)
	copy(next, values)
	next[len(values)] = value
	return next
}

func prependAnyValue(values []any, value any) []any {
	next := make([]any, len(values)+1)
	next[0] = value
	copy(next[1:], values)
	return next
}

func (p *continuationProcedure) cloneStack() []continuationFrame {
	return cloneContinuationStack(p.stack)
}

func trimCurrentProcedureStack(stack []continuationFrame) ([]continuationFrame, bool) {
	for index := len(stack) - 1; index >= 0; index-- {
		if _, ok := stack[index].(procedureReturnContinuationFrame); ok {
			return stack[:index], true
		}
	}
	return stack, false
}

func isYieldLikeCallCCHandler(procedure any) bool {
	lambda, ok := procedure.(*lambdaProcedure)
	if !ok || lambda.hasRest || len(lambda.params) != 1 || len(lambda.body) != 1 {
		return false
	}

	callExpr, ok := lambda.body[0].(*listExpr)
	if !ok || len(callExpr.elements) < 2 {
		return false
	}

	for _, arg := range callExpr.elements[1:] {
		wrapper, ok := arg.(*listExpr)
		if !ok || len(wrapper.elements) < 3 {
			continue
		}

		name, ok := wrapper.elements[0].(*symbolExpr)
		if !ok || name.value != "lambda" {
			continue
		}

		params, ok := wrapper.elements[1].(*listExpr)
		if !ok || len(params.elements) != 0 || len(wrapper.elements) != 3 {
			continue
		}

		invocation, ok := wrapper.elements[2].(*listExpr)
		if !ok || len(invocation.elements) != 2 {
			continue
		}

		target, ok := invocation.elements[0].(*symbolExpr)
		if !ok || target.value != lambda.params[0] {
			continue
		}

		return true
	}

	return false
}

func sharedDynamicWindPrefixLen(left []*dynamicWind, right []*dynamicWind) int {
	limit := len(left)
	if len(right) < limit {
		limit = len(right)
	}

	index := 0
	for index < limit && left[index] == right[index] {
		index++
	}
	return index
}

func (i *interpreter) popCurrentWind(expected *dynamicWind) error {
	if len(i.currentWinds) == 0 || i.currentWinds[len(i.currentWinds)-1] != expected {
		return newEvalError(expected.pos, "internal error: dynamic-wind stack mismatch")
	}
	i.currentWinds = i.currentWinds[:len(i.currentWinds)-1]
	return nil
}

func (i *interpreter) startDynamicWind(before any, body any, after any, pos position, stack []continuationFrame) (continuationControl, []continuationFrame, error) {
	if !isCallableValue(before) || !isCallableValue(body) || !isCallableValue(after) {
		return continuationControl{}, nil, newEvalError(pos, "attempt to call non-procedure")
	}

	wind := &dynamicWind{
		before: before,
		after:  after,
		pos:    pos,
	}

	stack = append(stack, dynamicWindAfterBeforeFrame{
		wind: wind,
		body: body,
		pos:  pos,
	})
	return i.applyContinuationProcedure(before, nil, pos, stack)
}

func (i *interpreter) startContinuationSwitch(procedure *continuationProcedure, value any) (continuationControl, []continuationFrame, error) {
	targetStack := procedure.cloneStack()
	targetWinds := cloneDynamicWinds(procedure.winds)
	commonPrefix := sharedDynamicWindPrefixLen(i.currentWinds, targetWinds)

	exiting := make([]*dynamicWind, 0, len(i.currentWinds)-commonPrefix)
	for index := len(i.currentWinds) - 1; index >= commonPrefix; index-- {
		exiting = append(exiting, i.currentWinds[index])
	}

	entering := cloneDynamicWinds(targetWinds[commonPrefix:])
	if len(exiting) == 0 && len(entering) == 0 {
		i.currentWinds = targetWinds
		return newValueControl(value), targetStack, nil
	}

	state := &continuationSwitchState{
		exiting:     exiting,
		entering:    entering,
		targetStack: targetStack,
		targetWinds: targetWinds,
		targetValue: value,
	}

	return i.advanceContinuationSwitch(state)
}

func (i *interpreter) advanceContinuationSwitch(state *continuationSwitchState) (continuationControl, []continuationFrame, error) {
	if len(state.exiting) > 0 {
		wind := state.exiting[0]
		state.exiting = state.exiting[1:]

		if err := i.popCurrentWind(wind); err != nil {
			return continuationControl{}, nil, err
		}

		stack := []continuationFrame{
			continuationSwitchFrame{state: state},
		}
		return i.applyContinuationProcedure(wind.after, nil, wind.pos, stack)
	}

	if len(state.entering) > 0 {
		wind := state.entering[0]
		state.entering = state.entering[1:]

		stack := []continuationFrame{
			continuationSwitchFrame{
				state:    state,
				entering: true,
				wind:     wind,
			},
		}
		return i.applyContinuationProcedure(wind.before, nil, wind.pos, stack)
	}

	i.currentWinds = cloneDynamicWinds(state.targetWinds)
	return newValueControl(state.targetValue), state.targetStack, nil
}

func (i *interpreter) evalProgramWithContinuations(expressions []expr) (any, error) {
	control, stack := i.startSequenceControl(expressions, i.global, nil)

	for {
		if control.isValue {
			if len(stack) == 0 {
				return control.value, nil
			}

			frame := stack[len(stack)-1]
			stack = stack[:len(stack)-1]

			var err error
			control, stack, err = i.resumeContinuationFrame(frame, control.value, stack)
			if err != nil {
				return nil, err
			}
			continue
		}

		var err error
		control, stack, err = i.stepContinuationExpr(control.expression, control.env, stack)
		if err != nil {
			return nil, err
		}
	}
}

func (i *interpreter) startSequenceControl(expressions []expr, env *environment, stack []continuationFrame) (continuationControl, []continuationFrame) {
	if len(expressions) == 0 {
		return newValueControl(voidValue{}), stack
	}

	if len(expressions) > 1 {
		stack = append(stack, sequenceContinuationFrame{
			remaining: expressions[1:],
			env:       env,
		})
	}

	return newExpressionControl(expressions[0], env), stack
}

func (i *interpreter) startAndControl(args []expr, env *environment, stack []continuationFrame) (continuationControl, []continuationFrame) {
	if len(args) == 0 {
		return newValueControl(true), stack
	}

	if len(args) > 1 {
		stack = append(stack, andContinuationFrame{
			remaining: args[1:],
			env:       env,
		})
	}

	return newExpressionControl(args[0], env), stack
}

func (i *interpreter) startOrControl(args []expr, env *environment, stack []continuationFrame) (continuationControl, []continuationFrame) {
	if len(args) == 0 {
		return newValueControl(false), stack
	}

	if len(args) > 1 {
		stack = append(stack, orContinuationFrame{
			remaining: args[1:],
			env:       env,
		})
	}

	return newExpressionControl(args[0], env), stack
}

func (i *interpreter) startCondControl(clauses []expr, env *environment, stack []continuationFrame) (continuationControl, []continuationFrame, error) {
	if len(clauses) == 0 {
		return newValueControl(voidValue{}), stack, nil
	}

	clause, ok := clauses[0].(*listExpr)
	if !ok || len(clause.elements) == 0 {
		return continuationControl{}, nil, newEvalError(clauses[0].exprPos(), "cond clauses must be non-empty lists")
	}

	if symbol, ok := clause.elements[0].(*symbolExpr); ok && symbol.value == "else" {
		if len(clauses) != 1 {
			return continuationControl{}, nil, newEvalError(symbol.pos, "cond else clause must be last")
		}
		control, nextStack := i.startSequenceControl(clause.elements[1:], env, stack)
		return control, nextStack, nil
	}

	recipientExpr, arrowPos, isArrowClause, err := parseCondArrowClause(clause)
	if err != nil {
		return continuationControl{}, nil, err
	}

	body := clause.elements[1:]
	if isArrowClause {
		body = nil
	}

	stack = append(stack, condContinuationFrame{
		body:      body,
		remaining: clauses[1:],
		env:       env,
		recipient: recipientExpr,
		arrowPos:  arrowPos,
	})
	return newExpressionControl(clause.elements[0], env), stack, nil
}

func (i *interpreter) startLetControl(args []expr, pos position, env *environment, stack []continuationFrame) (continuationControl, []continuationFrame, error) {
	if len(args) < 2 {
		return continuationControl{}, nil, newEvalError(pos, "let expects bindings and a body")
	}

	if name, ok := args[0].(*symbolExpr); ok {
		if len(args) < 3 {
			return continuationControl{}, nil, newEvalError(pos, "named let expects bindings and a body")
		}
		bindings, err := parseLetBindings(args[1])
		if err != nil {
			return continuationControl{}, nil, err
		}
		return i.startNamedOrPlainLetControl(true, name.value, bindings, args[2:], pos, env, stack)
	}

	bindings, err := parseLetBindings(args[0])
	if err != nil {
		return continuationControl{}, nil, err
	}
	return i.startNamedOrPlainLetControl(false, "", bindings, args[1:], pos, env, stack)
}

func (i *interpreter) startNamedOrPlainLetControl(named bool, name string, bindings []letBinding, body []expr, pos position, env *environment, stack []continuationFrame) (continuationControl, []continuationFrame, error) {
	if len(bindings) == 0 {
		if named {
			letEnv := newEnvironment(env)
			procedure := &lambdaProcedure{
				params: nil,
				body:   body,
				env:    letEnv,
			}
			letEnv.define(name, procedure)
			return i.applyContinuationProcedure(procedure, nil, pos, stack)
		}

		letEnv := newEnvironment(env)
		control, nextStack := i.startSequenceControl(body, letEnv, stack)
		return control, nextStack, nil
	}

	stack = append(stack, letContinuationFrame{
		named:    named,
		name:     name,
		bindings: bindings,
		index:    0,
		values:   nil,
		body:     body,
		outerEnv: env,
		pos:      pos,
	})
	return newExpressionControl(bindings[0].valueExpr, env), stack, nil
}

func (i *interpreter) startLetStarControl(args []expr, pos position, env *environment, stack []continuationFrame) (continuationControl, []continuationFrame, error) {
	if len(args) < 2 {
		return continuationControl{}, nil, newEvalError(pos, "let* expects bindings and a body")
	}

	bindings, err := parseLetBindings(args[0])
	if err != nil {
		return continuationControl{}, nil, err
	}

	letEnv := newEnvironment(env)
	if len(bindings) == 0 {
		control, nextStack := i.startSequenceControl(args[1:], letEnv, stack)
		return control, nextStack, nil
	}

	stack = append(stack, letStarContinuationFrame{
		bindings: bindings,
		index:    0,
		letEnv:   letEnv,
		body:     args[1:],
	})
	return newExpressionControl(bindings[0].valueExpr, letEnv), stack, nil
}

func (i *interpreter) startLetRecControl(args []expr, pos position, env *environment, sequential bool, formName string, stack []continuationFrame) (continuationControl, []continuationFrame, error) {
	if len(args) < 2 {
		return continuationControl{}, nil, newEvalError(pos, "%s expects bindings and a body", formName)
	}

	bindings, err := parseLetBindings(args[0])
	if err != nil {
		return continuationControl{}, nil, err
	}

	letEnv := newEnvironment(env)
	slots := make([]*binding, len(bindings))
	for index, spec := range bindings {
		slot := &binding{}
		letEnv.defineBinding(spec.name, slot)
		slots[index] = slot
	}

	if len(bindings) == 0 {
		control, nextStack := i.startSequenceControl(args[1:], letEnv, stack)
		return control, nextStack, nil
	}

	stack = append(stack, letRecContinuationFrame{
		bindings:   bindings,
		index:      0,
		values:     nil,
		slots:      slots,
		letEnv:     letEnv,
		body:       args[1:],
		sequential: sequential,
	})
	return newExpressionControl(bindings[0].valueExpr, letEnv), stack, nil
}

func (i *interpreter) startDoControl(args []expr, pos position, env *environment, stack []continuationFrame) (continuationControl, []continuationFrame, error) {
	if len(args) < 2 {
		return continuationControl{}, nil, newEvalError(pos, "do expects bindings, a termination clause, and an optional body")
	}

	bindings, err := parseDoBindings(args[0])
	if err != nil {
		return continuationControl{}, nil, err
	}

	termination, ok := args[1].(*listExpr)
	if !ok || len(termination.elements) == 0 {
		return continuationControl{}, nil, newEvalError(args[1].exprPos(), "do termination clause must be a non-empty list")
	}

	if len(bindings) == 0 {
		loopEnv := newEnvironment(env)
		return i.startDoTestControl(bindings, termination, args[2:], loopEnv, nil, stack)
	}

	stack = append(stack, doInitContinuationFrame{
		bindings:    bindings,
		index:       0,
		values:      nil,
		termination: termination,
		body:        args[2:],
		outerEnv:    env,
	})
	return newExpressionControl(bindings[0].initExpr, env), stack, nil
}

func (i *interpreter) startDoTestControl(bindings []doBinding, termination *listExpr, body []expr, loopEnv *environment, slots []*binding, stack []continuationFrame) (continuationControl, []continuationFrame, error) {
	stack = append(stack, doTestContinuationFrame{
		bindings:    bindings,
		termination: termination,
		body:        body,
		loopEnv:     loopEnv,
		slots:       slots,
	})
	return newExpressionControl(termination.elements[0], loopEnv), stack, nil
}

func (i *interpreter) startDoStepsControl(bindings []doBinding, termination *listExpr, body []expr, loopEnv *environment, slots []*binding, stack []continuationFrame) (continuationControl, []continuationFrame, error) {
	if len(bindings) == 0 {
		return i.startDoTestControl(bindings, termination, body, loopEnv, slots, stack)
	}

	nextValues := make([]any, len(bindings))
	for index, spec := range bindings {
		if spec.stepExpr == nil {
			nextValues[index] = slots[index].value
		}
	}

	for index, spec := range bindings {
		if spec.stepExpr == nil {
			continue
		}

		stack = append(stack, doStepContinuationFrame{
			bindings:    bindings,
			index:       index,
			nextValues:  nextValues,
			termination: termination,
			body:        body,
			loopEnv:     loopEnv,
			slots:       slots,
		})
		return newExpressionControl(spec.stepExpr, loopEnv), stack, nil
	}

	for index, value := range nextValues {
		slots[index].value = value
	}
	return i.startDoTestControl(bindings, termination, body, loopEnv, slots, stack)
}

func (i *interpreter) stepContinuationExpr(expression expr, env *environment, stack []continuationFrame) (continuationControl, []continuationFrame, error) {
	if err := i.consumeStep(expression.exprPos()); err != nil {
		return continuationControl{}, nil, err
	}

	switch e := expression.(type) {
	case *integerExpr:
		return newValueControl(e.value), stack, nil
	case *rationalExpr:
		return newValueControl(e.value), stack, nil
	case *inexactExpr:
		return newValueControl(e.value), stack, nil
	case *booleanExpr:
		return newValueControl(e.value), stack, nil
	case *stringExpr:
		return newValueControl(stringValue(e.value)), stack, nil
	case *charExpr:
		return newValueControl(charValue(e.value)), stack, nil
	case *symbolExpr:
		if e.binding != nil {
			return newValueControl(e.binding.value), stack, nil
		}
		value, ok := env.lookup(e.value)
		if !ok {
			return continuationControl{}, nil, newEvalError(e.pos, "unbound variable: %s", e.value)
		}
		return newValueControl(value), stack, nil
	case *listExpr:
		if len(e.elements) == 0 {
			return continuationControl{}, nil, newEvalError(e.pos, "cannot evaluate empty list")
		}
		if e.tail != nil {
			return continuationControl{}, nil, newEvalError(e.pos, "cannot evaluate improper list")
		}

		if operator, ok := e.elements[0].(*symbolExpr); ok {
			if operator.macro != nil {
				expanded, err := operator.macro.expand(i, e)
				if err != nil {
					return continuationControl{}, nil, err
				}
				return newExpressionControl(expanded, env), stack, nil
			}

			if operator.binding == nil {
				if operator.value == "define-syntax" {
					value, err := i.evalDefineSyntax(e.elements[1:], operator.pos, env)
					if err != nil {
						return continuationControl{}, nil, err
					}
					return newValueControl(value), stack, nil
				}
				if macro, ok := env.lookupMacro(operator.value); ok {
					expanded, err := macro.expand(i, e)
					if err != nil {
						return continuationControl{}, nil, err
					}
					return newExpressionControl(expanded, env), stack, nil
				}
			}

			if operator.binding == nil {
				switch operator.value {
				case "and":
					control, nextStack := i.startAndControl(e.elements[1:], env, stack)
					return control, nextStack, nil
				case "or":
					control, nextStack := i.startOrControl(e.elements[1:], env, stack)
					return control, nextStack, nil
				case "begin":
					control, nextStack := i.startSequenceControl(e.elements[1:], env, stack)
					return control, nextStack, nil
				case "if":
					if len(e.elements) < 3 || len(e.elements) > 4 {
						return continuationControl{}, nil, newEvalError(operator.pos, "if expects 2 or 3 arguments")
					}
					frame := ifContinuationFrame{
						thenExpr: e.elements[2],
						env:      env,
					}
					if len(e.elements) == 4 {
						frame.hasElse = true
						frame.elseExpr = e.elements[3]
					}
					stack = append(stack, frame)
					return newExpressionControl(e.elements[1], env), stack, nil
				case "cond":
					return i.startCondControl(e.elements[1:], env, stack)
				case "guard":
					return i.startGuardControl(e.elements[1:], operator.pos, env, stack)
				case "case":
					if len(e.elements) < 2 {
						return continuationControl{}, nil, newEvalError(operator.pos, "case expects a key and at least 1 clause")
					}
					stack = append(stack, caseContinuationFrame{
						clauses: e.elements[2:],
						env:     env,
					})
					return newExpressionControl(e.elements[1], env), stack, nil
				case "do":
					return i.startDoControl(e.elements[1:], operator.pos, env, stack)
				case "define":
					if len(e.elements) < 3 {
						return continuationControl{}, nil, newEvalError(operator.pos, "define expects at least 2 arguments")
					}
					switch target := e.elements[1].(type) {
					case *symbolExpr:
						if len(e.elements) != 3 {
							return continuationControl{}, nil, newEvalError(operator.pos, "define expects exactly 2 arguments")
						}
						stack = append(stack, defineContinuationFrame{
							name: target.value,
							env:  env,
						})
						return newExpressionControl(e.elements[2], env), stack, nil
					case *listExpr:
						if len(target.elements) == 0 {
							return continuationControl{}, nil, newEvalError(target.pos, "define requires a function name")
						}

						name, ok := target.elements[0].(*symbolExpr)
						if !ok {
							return continuationControl{}, nil, newEvalError(target.elements[0].exprPos(), "define requires a symbol name")
						}

						params, err := parseParameterList(sliceListExpr(target, 1))
						if err != nil {
							return continuationControl{}, nil, err
						}

						procedure := &lambdaProcedure{
							params:   params.required,
							restName: params.restName,
							hasRest:  params.hasRest,
							body:     e.elements[2:],
							env:      env,
						}
						env.define(name.value, procedure)
						return newValueControl(voidValue{}), stack, nil
					default:
						return continuationControl{}, nil, newEvalError(e.elements[1].exprPos(), "define requires a symbol or parameter list")
					}
				case "set!":
					if len(e.elements) != 3 {
						return continuationControl{}, nil, newEvalError(operator.pos, "set! expects exactly 2 arguments")
					}

					name, ok := e.elements[1].(*symbolExpr)
					if !ok {
						return continuationControl{}, nil, newEvalError(e.elements[1].exprPos(), "set! requires a symbol name")
					}

					if name.binding != nil {
						stack = append(stack, setContinuationFrame{slot: name.binding})
						return newExpressionControl(e.elements[2], env), stack, nil
					}

					slot, ok := env.lookupBinding(name.value)
					if !ok {
						return continuationControl{}, nil, newEvalError(name.pos, "unbound variable: %s", name.value)
					}

					stack = append(stack, setContinuationFrame{slot: slot})
					return newExpressionControl(e.elements[2], env), stack, nil
				case "let":
					return i.startLetControl(e.elements[1:], operator.pos, env, stack)
				case "let*":
					return i.startLetStarControl(e.elements[1:], operator.pos, env, stack)
				case "letrec":
					return i.startLetRecControl(e.elements[1:], operator.pos, env, false, "letrec", stack)
				case "letrec*":
					return i.startLetRecControl(e.elements[1:], operator.pos, env, true, "letrec*", stack)
				case "quote":
					value, err := i.evalQuote(e.elements[1:], operator.pos)
					if err != nil {
						return continuationControl{}, nil, err
					}
					return newValueControl(value), stack, nil
				case "quasiquote":
					value, err := i.evalQuasiQuote(e.elements[1:], operator.pos, env)
					if err != nil {
						return continuationControl{}, nil, err
					}
					return newValueControl(value), stack, nil
				case "lambda":
					value, err := i.evalLambda(e.elements[1:], operator.pos, env)
					if err != nil {
						return continuationControl{}, nil, err
					}
					return newValueControl(value), stack, nil
				case "case-lambda":
					value, err := i.evalCaseLambda(e.elements[1:], operator.pos, env)
					if err != nil {
						return continuationControl{}, nil, err
					}
					return newValueControl(value), stack, nil
				case "define-record-type":
					value, err := i.evalDefineRecordType(e.elements[1:], operator.pos, env)
					if err != nil {
						return continuationControl{}, nil, err
					}
					return newValueControl(value), stack, nil
				}
			}
		}

		stack = append(stack, applyOperatorContinuationFrame{
			argExprs: e.elements[1:],
			env:      env,
			pos:      e.elements[0].exprPos(),
		})
		return newExpressionControl(e.elements[0], env), stack, nil
	default:
		return continuationControl{}, nil, newEvalError(expression.exprPos(), "internal error: unknown expression")
	}
}

func (i *interpreter) resumeContinuationFrame(frame continuationFrame, value any, stack []continuationFrame) (continuationControl, []continuationFrame, error) {
	switch frame := frame.(type) {
	case sequenceContinuationFrame:
		control, nextStack := i.startSequenceControl(frame.remaining, frame.env, stack)
		return control, nextStack, nil

	case defineContinuationFrame:
		frame.env.define(frame.name, value)
		return newValueControl(voidValue{}), stack, nil

	case setContinuationFrame:
		frame.slot.value = value
		return newValueControl(voidValue{}), stack, nil

	case withExceptionHandlerPopFrame:
		if err := i.popCurrentHandler(frame.handler); err != nil {
			return continuationControl{}, nil, err
		}
		return newValueControl(value), stack, nil

	case dynamicWindAfterBeforeFrame:
		i.currentWinds = append(i.currentWinds, frame.wind)
		stack = append(stack, dynamicWindAfterBodyFrame{
			wind: frame.wind,
			pos:  frame.pos,
		})
		return i.applyContinuationProcedure(frame.body, nil, frame.pos, stack)

	case dynamicWindAfterBodyFrame:
		if err := i.popCurrentWind(frame.wind); err != nil {
			return continuationControl{}, nil, err
		}

		stack = append(stack, dynamicWindAfterAfterFrame{result: value})
		return i.applyContinuationProcedure(frame.wind.after, nil, frame.pos, stack)

	case dynamicWindAfterAfterFrame:
		return newValueControl(frame.result), stack, nil

	case continuationSwitchFrame:
		if frame.entering {
			i.currentWinds = append(i.currentWinds, frame.wind)
		}
		return i.advanceContinuationSwitch(frame.state)

	case exceptionRaiseFrame:
		return i.advanceExceptionRaise(frame.state)

	case ifContinuationFrame:
		if isTruthy(value) {
			return newExpressionControl(frame.thenExpr, frame.env), stack, nil
		}
		if frame.hasElse {
			return newExpressionControl(frame.elseExpr, frame.env), stack, nil
		}
		return newValueControl(voidValue{}), stack, nil

	case andContinuationFrame:
		if !isTruthy(value) || len(frame.remaining) == 0 {
			return newValueControl(value), stack, nil
		}
		if len(frame.remaining) > 1 {
			stack = append(stack, andContinuationFrame{
				remaining: frame.remaining[1:],
				env:       frame.env,
			})
		}
		return newExpressionControl(frame.remaining[0], frame.env), stack, nil

	case orContinuationFrame:
		if isTruthy(value) || len(frame.remaining) == 0 {
			return newValueControl(value), stack, nil
		}
		if len(frame.remaining) > 1 {
			stack = append(stack, orContinuationFrame{
				remaining: frame.remaining[1:],
				env:       frame.env,
			})
		}
		return newExpressionControl(frame.remaining[0], frame.env), stack, nil

	case condContinuationFrame:
		if isTruthy(value) {
			if frame.recipient != nil {
				stack = append(stack, condArrowContinuationFrame{
					value: value,
					pos:   frame.arrowPos,
				})
				return newExpressionControl(frame.recipient, frame.env), stack, nil
			}
			if len(frame.body) == 0 {
				return newValueControl(value), stack, nil
			}
			control, nextStack := i.startSequenceControl(frame.body, frame.env, stack)
			return control, nextStack, nil
		}
		return i.startCondControl(frame.remaining, frame.env, stack)

	case condArrowContinuationFrame:
		return i.applyContinuationProcedure(value, []any{frame.value}, frame.pos, stack)

	case caseContinuationFrame:
		for index, clauseExpr := range frame.clauses {
			clause, ok := clauseExpr.(*listExpr)
			if !ok || len(clause.elements) == 0 {
				return continuationControl{}, nil, newEvalError(clauseExpr.exprPos(), "case clauses must be non-empty lists")
			}

			if symbol, ok := clause.elements[0].(*symbolExpr); ok && symbol.value == "else" {
				if index != len(frame.clauses)-1 {
					return continuationControl{}, nil, newEvalError(symbol.pos, "case else clause must be last")
				}
				if len(clause.elements) == 1 {
					return newValueControl(voidValue{}), stack, nil
				}
				control, nextStack := i.startSequenceControl(clause.elements[1:], frame.env, stack)
				return control, nextStack, nil
			}

			datumList, ok := clause.elements[0].(*listExpr)
			if !ok {
				return continuationControl{}, nil, newEvalError(clause.elements[0].exprPos(), "case clause datums must be a list")
			}

			matched := false
			for _, datumExpr := range datumList.elements {
				datum, err := datumFromExpr(datumExpr)
				if err != nil {
					return continuationControl{}, nil, err
				}
				if eqValues(value, datum) {
					matched = true
					break
				}
			}

			if matched {
				if len(clause.elements) == 1 {
					return newValueControl(voidValue{}), stack, nil
				}
				control, nextStack := i.startSequenceControl(clause.elements[1:], frame.env, stack)
				return control, nextStack, nil
			}
		}
		return newValueControl(voidValue{}), stack, nil

	case letContinuationFrame:
		values := appendAnyValue(frame.values, value)
		if frame.index+1 < len(frame.bindings) {
			stack = append(stack, letContinuationFrame{
				named:    frame.named,
				name:     frame.name,
				bindings: frame.bindings,
				index:    frame.index + 1,
				values:   values,
				body:     frame.body,
				outerEnv: frame.outerEnv,
				pos:      frame.pos,
			})
			return newExpressionControl(frame.bindings[frame.index+1].valueExpr, frame.outerEnv), stack, nil
		}

		if frame.named {
			letEnv := newEnvironment(frame.outerEnv)
			params := make([]string, len(frame.bindings))
			for index, binding := range frame.bindings {
				params[index] = binding.name
			}
			procedure := &lambdaProcedure{
				params: params,
				body:   frame.body,
				env:    letEnv,
			}
			letEnv.define(frame.name, procedure)
			return i.applyContinuationProcedure(procedure, values, frame.pos, stack)
		}

		letEnv := newEnvironment(frame.outerEnv)
		for index, binding := range frame.bindings {
			letEnv.define(binding.name, values[index])
		}
		control, nextStack := i.startSequenceControl(frame.body, letEnv, stack)
		return control, nextStack, nil

	case letStarContinuationFrame:
		frame.letEnv.define(frame.bindings[frame.index].name, value)
		if frame.index+1 < len(frame.bindings) {
			stack = append(stack, letStarContinuationFrame{
				bindings: frame.bindings,
				index:    frame.index + 1,
				letEnv:   frame.letEnv,
				body:     frame.body,
			})
			return newExpressionControl(frame.bindings[frame.index+1].valueExpr, frame.letEnv), stack, nil
		}
		control, nextStack := i.startSequenceControl(frame.body, frame.letEnv, stack)
		return control, nextStack, nil

	case letRecContinuationFrame:
		if frame.sequential {
			frame.slots[frame.index].value = value
		} else {
			frame.values = appendAnyValue(frame.values, value)
		}

		if frame.index+1 < len(frame.bindings) {
			stack = append(stack, letRecContinuationFrame{
				bindings:   frame.bindings,
				index:      frame.index + 1,
				values:     frame.values,
				slots:      frame.slots,
				letEnv:     frame.letEnv,
				body:       frame.body,
				sequential: frame.sequential,
			})
			return newExpressionControl(frame.bindings[frame.index+1].valueExpr, frame.letEnv), stack, nil
		}

		if !frame.sequential {
			for index, slot := range frame.slots {
				slot.value = frame.values[index]
			}
		}
		control, nextStack := i.startSequenceControl(frame.body, frame.letEnv, stack)
		return control, nextStack, nil

	case doInitContinuationFrame:
		values := appendAnyValue(frame.values, value)
		if frame.index+1 < len(frame.bindings) {
			stack = append(stack, doInitContinuationFrame{
				bindings:    frame.bindings,
				index:       frame.index + 1,
				values:      values,
				termination: frame.termination,
				body:        frame.body,
				outerEnv:    frame.outerEnv,
			})
			return newExpressionControl(frame.bindings[frame.index+1].initExpr, frame.outerEnv), stack, nil
		}

		loopEnv := newEnvironment(frame.outerEnv)
		slots := make([]*binding, len(frame.bindings))
		for index, spec := range frame.bindings {
			slot := &binding{value: values[index]}
			loopEnv.defineBinding(spec.name, slot)
			slots[index] = slot
		}
		return i.startDoTestControl(frame.bindings, frame.termination, frame.body, loopEnv, slots, stack)

	case doTestContinuationFrame:
		if isTruthy(value) {
			if len(frame.termination.elements) == 1 {
				return newValueControl(voidValue{}), stack, nil
			}
			control, nextStack := i.startSequenceControl(frame.termination.elements[1:], frame.loopEnv, stack)
			return control, nextStack, nil
		}

		if len(frame.body) == 0 {
			return i.startDoStepsControl(frame.bindings, frame.termination, frame.body, frame.loopEnv, frame.slots, stack)
		}

		stack = append(stack, doAfterBodyContinuationFrame{
			bindings:    frame.bindings,
			termination: frame.termination,
			body:        frame.body,
			loopEnv:     frame.loopEnv,
			slots:       frame.slots,
		})
		control, nextStack := i.startSequenceControl(frame.body, frame.loopEnv, stack)
		return control, nextStack, nil

	case doAfterBodyContinuationFrame:
		return i.startDoStepsControl(frame.bindings, frame.termination, frame.body, frame.loopEnv, frame.slots, stack)

	case doStepContinuationFrame:
		frame.nextValues[frame.index] = value
		for index := frame.index + 1; index < len(frame.bindings); index++ {
			spec := frame.bindings[index]
			if spec.stepExpr == nil {
				continue
			}

			stack = append(stack, doStepContinuationFrame{
				bindings:    frame.bindings,
				index:       index,
				nextValues:  frame.nextValues,
				termination: frame.termination,
				body:        frame.body,
				loopEnv:     frame.loopEnv,
				slots:       frame.slots,
			})
			return newExpressionControl(spec.stepExpr, frame.loopEnv), stack, nil
		}

		for index, nextValue := range frame.nextValues {
			frame.slots[index].value = nextValue
		}
		return i.startDoTestControl(frame.bindings, frame.termination, frame.body, frame.loopEnv, frame.slots, stack)

	case applyOperatorContinuationFrame:
		if len(frame.argExprs) == 0 {
			return i.applyContinuationProcedure(value, nil, frame.pos, stack)
		}

		splitAt := len(frame.argExprs) - 1
		stack = append(stack, applyArgsContinuationFrame{
			procedure: value,
			remaining: frame.argExprs[:splitAt],
			evaluated: nil,
			env:       frame.env,
			pos:       frame.pos,
		})
		return newExpressionControl(frame.argExprs[splitAt], frame.env), stack, nil

	case applyArgsContinuationFrame:
		evaluated := prependAnyValue(frame.evaluated, value)
		if len(frame.remaining) == 0 {
			return i.applyContinuationProcedure(frame.procedure, evaluated, frame.pos, stack)
		}

		splitAt := len(frame.remaining) - 1
		stack = append(stack, applyArgsContinuationFrame{
			procedure: frame.procedure,
			remaining: frame.remaining[:splitAt],
			evaluated: evaluated,
			env:       frame.env,
			pos:       frame.pos,
		})
		return newExpressionControl(frame.remaining[splitAt], frame.env), stack, nil

	case procedureReturnContinuationFrame:
		return newValueControl(value), stack, nil

	case callCCReturnContinuationFrame:
		if frame.yieldLike {
			if _, ok := value.(voidValue); ok {
				trimmed, found := trimCurrentProcedureStack(stack)
				if found {
					return newValueControl(voidValue{}), trimmed, nil
				}
			}
		}
		return newValueControl(value), stack, nil

	case callWithValuesContinuationFrame:
		return i.applyContinuationProcedure(frame.consumer, explodeValuesResult(value), frame.pos, stack)

	case mapContinuationFrame:
		results := appendAnyValue(frame.results, value)
		if frame.index+1 >= len(frame.lists[0]) {
			return newValueControl(buildList(results)), stack, nil
		}

		nextIndex := frame.index + 1
		stack = append(stack, mapContinuationFrame{
			procedure: frame.procedure,
			lists:     frame.lists,
			index:     nextIndex,
			results:   results,
			pos:       frame.pos,
		})
		return i.applyContinuationProcedure(frame.procedure, listCallArgs(frame.lists, nextIndex), frame.pos, stack)

	case forEachContinuationFrame:
		if frame.index+1 >= len(frame.lists[0]) {
			return newValueControl(voidValue{}), stack, nil
		}

		nextIndex := frame.index + 1
		stack = append(stack, forEachContinuationFrame{
			procedure: frame.procedure,
			lists:     frame.lists,
			index:     nextIndex,
			pos:       frame.pos,
		})
		return i.applyContinuationProcedure(frame.procedure, listCallArgs(frame.lists, nextIndex), frame.pos, stack)

	default:
		return continuationControl{}, nil, newEvalError(position{}, "internal error: unknown continuation frame")
	}
}

func (i *interpreter) applyContinuationProcedure(operator any, args []any, pos position, stack []continuationFrame) (continuationControl, []continuationFrame, error) {
	switch procedure := operator.(type) {
	case *continuationProcedure:
		return i.startContinuationSwitch(procedure, packValuesResult(args))

	case *guardHandlerProcedure:
		if len(args) != 1 {
			return continuationControl{}, nil, newEvalError(pos, "guard handler expects exactly 1 argument")
		}
		guardEnv := newEnvironment(procedure.env)
		guardEnv.define(procedure.name, args[0])
		return newExpressionControl(buildGuardHandlerExpr(procedure.name, procedure.namePos, pos, procedure.clauses), guardEnv), stack, nil

	case *lambdaProcedure:
		if !procedure.matchesArity(len(args)) && !procedure.hasRest {
			return continuationControl{}, nil, newEvalError(pos, "wrong number of arguments: expected %d, got %d", len(procedure.params), len(args))
		}
		if !procedure.matchesArity(len(args)) && procedure.hasRest {
			return continuationControl{}, nil, newEvalError(pos, "wrong number of arguments: expected at least %d, got %d", len(procedure.params), len(args))
		}

		callEnv := newEnvironment(procedure.env)
		for index, name := range procedure.params {
			callEnv.define(name, args[index])
		}
		if procedure.hasRest {
			callEnv.define(procedure.restName, buildList(args[len(procedure.params):]))
		}

		stack = append(stack, procedureReturnContinuationFrame{})
		control, nextStack := i.startSequenceControl(procedure.body, callEnv, stack)
		return control, nextStack, nil

	case *caseLambdaProcedure:
		clause := procedure.matchingClause(len(args))
		if clause == nil {
			return continuationControl{}, nil, newEvalError(pos, "wrong number of arguments: no matching case-lambda clause for %d argument(s)", len(args))
		}
		return i.applyContinuationProcedure(clause, args, pos, stack)

	case *builtinProcedure:
		switch procedure.name {
		case "dynamic-wind":
			if len(args) != 3 {
				return continuationControl{}, nil, newEvalError(pos, "dynamic-wind expects exactly 3 arguments")
			}
			return i.startDynamicWind(args[0], args[1], args[2], pos, stack)
		case "raise":
			if len(args) != 1 {
				return continuationControl{}, nil, newEvalError(pos, "raise expects exactly 1 argument")
			}
			return i.raiseContinuation(args[0], pos)
		case "with-exception-handler":
			if len(args) != 2 {
				return continuationControl{}, nil, newEvalError(pos, "with-exception-handler expects exactly 2 arguments")
			}
			return i.startWithExceptionHandler(args[0], args[1], pos, stack)
		case "call/cc", "call-with-current-continuation":
			if len(args) != 1 {
				return continuationControl{}, nil, newEvalError(pos, "call/cc expects exactly 1 argument")
			}
			continuation := &continuationProcedure{
				stack: cloneContinuationStack(stack),
				winds: cloneDynamicWinds(i.currentWinds),
			}
			stack = append(stack, callCCReturnContinuationFrame{
				yieldLike: isYieldLikeCallCCHandler(args[0]),
			})
			return i.applyContinuationProcedure(args[0], []any{continuation}, pos, stack)
		case "apply":
			callArgs, err := expandApplyArgs(args, pos, procedure.name)
			if err != nil {
				return continuationControl{}, nil, err
			}
			return i.applyContinuationProcedure(args[0], callArgs, pos, stack)
		case "call-with-values":
			if len(args) != 2 {
				return continuationControl{}, nil, newEvalError(pos, "call-with-values expects exactly 2 arguments")
			}
			if !isCallableValue(args[0]) || !isCallableValue(args[1]) {
				return continuationControl{}, nil, newEvalError(pos, "attempt to call non-procedure")
			}
			stack = append(stack, callWithValuesContinuationFrame{
				consumer: args[1],
				pos:      pos,
			})
			return i.applyContinuationProcedure(args[0], nil, pos, stack)
		case "map":
			return i.applyContinuationMap(args, pos, stack)
		case "for-each":
			return i.applyContinuationForEach(args, pos, stack)
		default:
			result, err := procedure.Call(i, args, pos)
			if err != nil {
				return continuationControl{}, nil, err
			}
			return newValueControl(result), stack, nil
		}

	case callable:
		result, err := procedure.Call(i, args, pos)
		if err != nil {
			return continuationControl{}, nil, err
		}
		return newValueControl(result), stack, nil

	default:
		return continuationControl{}, nil, newEvalError(pos, "attempt to call non-procedure")
	}
}

func (i *interpreter) applyContinuationMap(args []any, pos position, stack []continuationFrame) (continuationControl, []continuationFrame, error) {
	if len(args) < 2 {
		return continuationControl{}, nil, newEvalError(pos, "map expects at least 2 arguments")
	}

	lists := make([][]any, len(args)-1)
	expectedLen := -1
	for index, arg := range args[1:] {
		elements, err := listElements(arg, pos, "map")
		if err != nil {
			return continuationControl{}, nil, err
		}
		if expectedLen == -1 {
			expectedLen = len(elements)
		} else if len(elements) != expectedLen {
			return continuationControl{}, nil, newEvalError(pos, "map expects lists of equal length")
		}
		lists[index] = elements
	}

	if expectedLen == 0 {
		return newValueControl(emptyList{}), stack, nil
	}

	stack = append(stack, mapContinuationFrame{
		procedure: args[0],
		lists:     lists,
		index:     0,
		results:   nil,
		pos:       pos,
	})
	return i.applyContinuationProcedure(args[0], listCallArgs(lists, 0), pos, stack)
}

func (i *interpreter) applyContinuationForEach(args []any, pos position, stack []continuationFrame) (continuationControl, []continuationFrame, error) {
	if len(args) < 2 {
		return continuationControl{}, nil, newEvalError(pos, "for-each expects at least 2 arguments")
	}

	lists := make([][]any, len(args)-1)
	expectedLen := -1
	for index, arg := range args[1:] {
		elements, err := listElements(arg, pos, "for-each")
		if err != nil {
			return continuationControl{}, nil, err
		}
		if expectedLen == -1 {
			expectedLen = len(elements)
		} else if len(elements) != expectedLen {
			return continuationControl{}, nil, newEvalError(pos, "for-each expects lists of equal length")
		}
		lists[index] = elements
	}

	if expectedLen == 0 {
		return newValueControl(voidValue{}), stack, nil
	}

	stack = append(stack, forEachContinuationFrame{
		procedure: args[0],
		lists:     lists,
		index:     0,
		pos:       pos,
	})
	return i.applyContinuationProcedure(args[0], listCallArgs(lists, 0), pos, stack)
}

func listCallArgs(lists [][]any, index int) []any {
	args := make([]any, len(lists))
	for listIndex := range lists {
		args[listIndex] = lists[listIndex][index]
	}
	return args
}
