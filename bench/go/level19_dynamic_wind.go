package ming

type windFrame struct {
	parent   *windFrame
	inThunk  procedure
	outThunk procedure
	pos      SourcePos
}

type dynamicWindProc struct {
	name string
}

type dynamicWindEnterFrame struct {
	wind *windFrame
	body procedure
	next continuation
}

type dynamicWindBodyFrame struct {
	wind *windFrame
	next continuation
}

type windTransitionExitFrame struct {
	exits      []*windFrame
	entries    []*windFrame
	targetWind *windFrame
	targetCont continuation
	result     value
}

type windTransitionEnterFrame struct {
	entered    *windFrame
	entries    []*windFrame
	targetWind *windFrame
	targetCont continuation
	result     value
}

func (p dynamicWindProc) schemeString() string {
	return "#<procedure:" + p.name + ">"
}

func (dynamicWindProc) isTruthy() bool {
	return true
}

func (p dynamicWindProc) call(args []value) (value, error) {
	inThunk, bodyThunk, outThunk, err := parseDynamicWindArgs(args, p.name)
	if err != nil {
		return nil, err
	}

	return applyDynamicWindWithContinuation(inThunk, bodyThunk, outThunk, currentEvalPos, nil, nil)
}

func (f *dynamicWindEnterFrame) resume(m *machine, _ value) error {
	m.wind = f.wind
	return applyProcedureState(m, f.body, nil, f.wind.pos, &dynamicWindBodyFrame{
		wind: f.wind,
		next: f.next,
	})
}

func (f *dynamicWindBodyFrame) resume(m *machine, v value) error {
	return startWindTransition(m, f.wind.parent, f.next, v)
}

func (f *windTransitionExitFrame) resume(m *machine, _ value) error {
	return continueWindTransition(m, f.exits, f.entries, f.targetWind, f.targetCont, f.result)
}

func (f *windTransitionEnterFrame) resume(m *machine, _ value) error {
	m.wind = f.entered
	return continueWindTransition(m, nil, f.entries, f.targetWind, f.targetCont, f.result)
}

func parseDynamicWindArgs(args []value, name string) (procedure, procedure, procedure, error) {
	if len(args) != 3 {
		return nil, nil, nil, newCurrentEvalError("'%s' expects exactly 3 arguments", name)
	}

	inThunk, ok := args[0].(procedure)
	if !ok {
		return nil, nil, nil, newCurrentEvalError("'%s' expects a procedure for in-thunk, got %s", name, args[0].schemeString())
	}

	bodyThunk, ok := args[1].(procedure)
	if !ok {
		return nil, nil, nil, newCurrentEvalError("'%s' expects a procedure for body-thunk, got %s", name, args[1].schemeString())
	}

	outThunk, ok := args[2].(procedure)
	if !ok {
		return nil, nil, nil, newCurrentEvalError("'%s' expects a procedure for out-thunk, got %s", name, args[2].schemeString())
	}

	return inThunk, bodyThunk, outThunk, nil
}

func applyDynamicWindWithContinuation(inThunk procedure, bodyThunk procedure, outThunk procedure, pos SourcePos, cont continuation, wind *windFrame) (value, error) {
	m := machine{wind: wind}
	if err := startDynamicWind(&m, inThunk, bodyThunk, outThunk, pos, cont); err != nil {
		return nil, err
	}
	return m.run()
}

func startDynamicWind(m *machine, inThunk procedure, bodyThunk procedure, outThunk procedure, pos SourcePos, cont continuation) error {
	frame := &windFrame{
		parent:   m.wind,
		inThunk:  inThunk,
		outThunk: outThunk,
		pos:      pos.normalized(),
	}

	return applyProcedureState(m, inThunk, nil, frame.pos, &dynamicWindEnterFrame{
		wind: frame,
		body: bodyThunk,
		next: cont,
	})
}

func evalValueWithWindTransition(v value, currentWind *windFrame, targetWind *windFrame, cont continuation) (value, error) {
	m := machine{wind: currentWind}
	if err := startWindTransition(&m, targetWind, cont, v); err != nil {
		return nil, err
	}
	return m.run()
}

func startWindTransition(m *machine, targetWind *windFrame, targetCont continuation, result value) error {
	exits, entries := splitWindTransition(m.wind, targetWind)
	return continueWindTransition(m, exits, entries, targetWind, targetCont, result)
}

func continueWindTransition(m *machine, exits []*windFrame, entries []*windFrame, targetWind *windFrame, targetCont continuation, result value) error {
	if len(exits) > 0 {
		exiting := exits[0]
		m.wind = exiting.parent
		return applyProcedureState(m, exiting.outThunk, nil, exiting.pos, &windTransitionExitFrame{
			exits:      exits[1:],
			entries:    entries,
			targetWind: targetWind,
			targetCont: targetCont,
			result:     result,
		})
	}

	if len(entries) > 0 {
		entering := entries[0]
		return applyProcedureState(m, entering.inThunk, nil, entering.pos, &windTransitionEnterFrame{
			entered:    entering,
			entries:    entries[1:],
			targetWind: targetWind,
			targetCont: targetCont,
			result:     result,
		})
	}

	m.wind = targetWind
	m.setValue(result, targetCont)
	return nil
}

func splitWindTransition(current *windFrame, target *windFrame) ([]*windFrame, []*windFrame) {
	currentChain := windChain(current)
	targetChain := windChain(target)

	commonLength := 0
	for commonLength < len(currentChain) && commonLength < len(targetChain) && currentChain[commonLength] == targetChain[commonLength] {
		commonLength++
	}

	exits := make([]*windFrame, 0, len(currentChain)-commonLength)
	for i := len(currentChain) - 1; i >= commonLength; i-- {
		exits = append(exits, currentChain[i])
	}

	entries := append([]*windFrame(nil), targetChain[commonLength:]...)
	return exits, entries
}

func windChain(wind *windFrame) []*windFrame {
	if wind == nil {
		return nil
	}

	reversed := make([]*windFrame, 0, 4)
	for current := wind; current != nil; current = current.parent {
		reversed = append(reversed, current)
	}

	chain := make([]*windFrame, len(reversed))
	for i := range reversed {
		chain[len(reversed)-1-i] = reversed[i]
	}
	return chain
}
