package ming

const level19DynamicWindKey = "__level19_dynamic_wind__"

type runtimeState struct {
	dynamic *dynamicWindFrame
}

type dynamicWindFrame struct {
	parent *dynamicWindFrame
	before any
	after  any
}

type dynamicWindEnterProc struct {
	runtime *runtimeState
	frame   *dynamicWindFrame
	body    any
	target  any
}

type dynamicWindExitProc struct {
	runtime *runtimeState
	frame   *dynamicWindFrame
	target  any
}

type dynamicWindCompleteProc struct {
	runtime *runtimeState
	frame   *dynamicWindFrame
	target  any
	result  any
}

type dynamicWindPopProc struct {
	runtime         *runtimeState
	frame           *dynamicWindFrame
	remainingExits  []*dynamicWindFrame
	remainingEnters []*dynamicWindFrame
	targetDynamic   *dynamicWindFrame
	target          any
	value           any
}

type dynamicWindReenterProc struct {
	runtime         *runtimeState
	remainingExits  []*dynamicWindFrame
	remainingEnters []*dynamicWindFrame
	targetDynamic   *dynamicWindFrame
	target          any
	value           any
}

func evalLevel19DynamicWindTail(scope *env, args []any) (any, *tailEvalState, error) {
	if len(args) != 4 {
		return nil, nil, &EvalError{Message: "__dynamic_wind expects before, thunk, after, and continuation"}
	}

	before, err := eval(scope, args[0])
	if err != nil {
		return nil, nil, err
	}

	body, err := eval(scope, args[1])
	if err != nil {
		return nil, nil, err
	}

	after, err := eval(scope, args[2])
	if err != nil {
		return nil, nil, err
	}

	target, err := eval(scope, args[3])
	if err != nil {
		return nil, nil, err
	}

	frame := &dynamicWindFrame{
		parent: scope.runtime.dynamic,
		before: before,
		after:  after,
	}
	scope.runtime.dynamic = frame

	value, next, err := prepareApplyCPSCall(before, nil, dynamicWindEnterProc{
		runtime: scope.runtime,
		frame:   frame,
		body:    body,
		target:  target,
	}, scope.runtime)
	if err != nil {
		scope.runtime.dynamic = frame.parent
		return nil, nil, err
	}
	return value, next, nil
}

func prepareDynamicWindEnterCall(callable dynamicWindEnterProc, args []any) (any, *tailEvalState, error) {
	if len(args) != 1 {
		return nil, nil, &EvalError{Message: "dynamic-wind before continuation expects exactly 1 argument"}
	}

	value, next, err := prepareApplyCPSCall(callable.body, nil, dynamicWindExitProc{
		runtime: callable.runtime,
		frame:   callable.frame,
		target:  callable.target,
	}, callable.runtime)
	if err != nil {
		callable.runtime.dynamic = callable.frame.parent
		return nil, nil, err
	}
	return value, next, nil
}

func prepareDynamicWindExitCall(callable dynamicWindExitProc, args []any) (any, *tailEvalState, error) {
	if len(args) != 1 {
		return nil, nil, &EvalError{Message: "dynamic-wind body continuation expects exactly 1 argument"}
	}

	value, next, err := prepareApplyCPSCall(callable.frame.after, nil, dynamicWindCompleteProc{
		runtime: callable.runtime,
		frame:   callable.frame,
		target:  callable.target,
		result:  args[0],
	}, callable.runtime)
	if err != nil {
		callable.runtime.dynamic = callable.frame.parent
		return nil, nil, err
	}
	return value, next, nil
}

func prepareDynamicWindCompleteCall(callable dynamicWindCompleteProc, args []any) (any, *tailEvalState, error) {
	if len(args) != 1 {
		return nil, nil, &EvalError{Message: "dynamic-wind after continuation expects exactly 1 argument"}
	}

	callable.runtime.dynamic = callable.frame.parent
	return prepareInvokeContinuationTarget(callable.runtime, callable.target, callable.result)
}

func prepareDynamicWindPopCall(callable dynamicWindPopProc, args []any) (any, *tailEvalState, error) {
	if len(args) != 1 {
		return nil, nil, &EvalError{Message: "dynamic-wind exit continuation expects exactly 1 argument"}
	}

	callable.runtime.dynamic = callable.frame.parent
	return prepareDynamicWindTransitionStep(
		callable.runtime,
		callable.remainingExits,
		callable.remainingEnters,
		callable.targetDynamic,
		callable.target,
		callable.value,
	)
}

func prepareDynamicWindReenterCall(callable dynamicWindReenterProc, args []any) (any, *tailEvalState, error) {
	if len(args) != 1 {
		return nil, nil, &EvalError{Message: "dynamic-wind enter continuation expects exactly 1 argument"}
	}

	return prepareDynamicWindTransitionStep(
		callable.runtime,
		callable.remainingExits,
		callable.remainingEnters,
		callable.targetDynamic,
		callable.target,
		callable.value,
	)
}

func prepareContinuationJump(runtime *runtimeState, callable continuationProc, value any) (any, *tailEvalState, error) {
	exits, enters := dynamicWindTransition(runtime.dynamic, callable.dynamic)
	return prepareDynamicWindTransitionStep(runtime, exits, enters, callable.dynamic, callable.target, value)
}

func prepareDynamicWindTransitionStep(runtime *runtimeState, exits []*dynamicWindFrame, enters []*dynamicWindFrame, targetDynamic *dynamicWindFrame, target any, value any) (any, *tailEvalState, error) {
	if len(exits) > 0 {
		frame := exits[0]
		nextValue, nextState, err := prepareApplyCPSCall(frame.after, nil, dynamicWindPopProc{
			runtime:         runtime,
			frame:           frame,
			remainingExits:  exits[1:],
			remainingEnters: enters,
			targetDynamic:   targetDynamic,
			target:          target,
			value:           value,
		}, runtime)
		if err != nil {
			runtime.dynamic = frame.parent
			return nil, nil, err
		}
		return nextValue, nextState, nil
	}

	if len(enters) > 0 {
		frame := enters[0]
		runtime.dynamic = frame
		nextValue, nextState, err := prepareApplyCPSCall(frame.before, nil, dynamicWindReenterProc{
			runtime:         runtime,
			remainingExits:  exits,
			remainingEnters: enters[1:],
			targetDynamic:   targetDynamic,
			target:          target,
			value:           value,
		}, runtime)
		if err != nil {
			runtime.dynamic = frame.parent
			return nil, nil, err
		}
		return nextValue, nextState, nil
	}

	runtime.dynamic = targetDynamic
	return prepareInvokeContinuationTarget(runtime, target, value)
}

func prepareInvokeContinuationTarget(runtime *runtimeState, target any, value any) (any, *tailEvalState, error) {
	if continuation, ok := target.(continuationProc); ok {
		return prepareContinuationJump(runtime, continuation, value)
	}
	return prepareProcedureCall(target, []any{value})
}

func dynamicWindTransition(current *dynamicWindFrame, target *dynamicWindFrame) ([]*dynamicWindFrame, []*dynamicWindFrame) {
	currentFrames := dynamicWindFramesFromRoot(current)
	targetFrames := dynamicWindFramesFromRoot(target)

	common := 0
	for common < len(currentFrames) && common < len(targetFrames) && currentFrames[common] == targetFrames[common] {
		common++
	}

	exits := make([]*dynamicWindFrame, 0, len(currentFrames)-common)
	for i := len(currentFrames) - 1; i >= common; i-- {
		exits = append(exits, currentFrames[i])
	}

	enters := append([]*dynamicWindFrame(nil), targetFrames[common:]...)
	return exits, enters
}

func dynamicWindFramesFromRoot(frame *dynamicWindFrame) []*dynamicWindFrame {
	var reversed []*dynamicWindFrame
	for current := frame; current != nil; current = current.parent {
		reversed = append(reversed, current)
	}

	frames := make([]*dynamicWindFrame, len(reversed))
	for i := range reversed {
		frames[len(reversed)-1-i] = reversed[i]
	}
	return frames
}
