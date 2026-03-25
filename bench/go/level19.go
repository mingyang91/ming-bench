package ming

type dynamicWindFrame struct {
	inProc  value
	outProc value
	callPos position
}

func copyDynamicWindFrames(frames []*dynamicWindFrame) []*dynamicWindFrame {
	if len(frames) == 0 {
		return nil
	}

	copied := make([]*dynamicWindFrame, len(frames))
	copy(copied, frames)
	return copied
}

func commonDynamicWindPrefix(left []*dynamicWindFrame, right []*dynamicWindFrame) int {
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

func builtinDynamicWind(it *interpreter, args []value, callPos position) (value, error) {
	if len(args) != 3 {
		return nil, wrongArgCount(callPos, "dynamic-wind", "expected exactly 3 arguments")
	}

	if _, err := it.applyProcedure(args[0], nil, callPos); err != nil {
		return nil, err
	}

	result, err := it.applyProcedure(args[1], nil, callPos)
	if err != nil {
		return nil, err
	}

	if _, err := it.applyProcedure(args[2], nil, callPos); err != nil {
		return nil, err
	}

	return result, nil
}

func (it *interpreter) applyDynamicWindWithContinuations(args []value, callPos position, k evalContinuation) evalResult {
	if len(args) != 3 {
		return doneError(wrongArgCount(callPos, "dynamic-wind", "expected exactly 3 arguments"))
	}

	frame := &dynamicWindFrame{
		inProc:  args[0],
		outProc: args[2],
		callPos: callPos,
	}

	return callEval(func() evalResult {
		return it.applyProcedureWithContinuations(args[0], nil, callPos, func(_ []value) evalResult {
			it.dynamicWinds = append(it.dynamicWinds, frame)
			return callEval(func() evalResult {
				return it.applyProcedureWithContinuations(args[1], nil, callPos, func(result []value) evalResult {
					return it.exitDynamicWindFrame(frame, func() evalResult {
						return continueValues(k, result)
					})
				})
			})
		})
	})
}

func (it *interpreter) exitDynamicWindFrame(frame *dynamicWindFrame, next func() evalResult) evalResult {
	if len(it.dynamicWinds) == 0 || it.dynamicWinds[len(it.dynamicWinds)-1] != frame {
		return callEval(next)
	}

	it.dynamicWinds = it.dynamicWinds[:len(it.dynamicWinds)-1]
	return callEval(func() evalResult {
		return it.applyProcedureWithContinuations(frame.outProc, nil, frame.callPos, func(_ []value) evalResult {
			return callEval(next)
		})
	})
}

func (it *interpreter) switchDynamicWinds(target []*dynamicWindFrame, next func() evalResult) evalResult {
	return it.unwindDynamicWinds(commonDynamicWindPrefix(it.dynamicWinds, target), target, next)
}

func (it *interpreter) unwindDynamicWinds(prefix int, target []*dynamicWindFrame, next func() evalResult) evalResult {
	if len(it.dynamicWinds) == prefix {
		return it.rewindDynamicWinds(target, prefix, next)
	}

	frame := it.dynamicWinds[len(it.dynamicWinds)-1]
	it.dynamicWinds = it.dynamicWinds[:len(it.dynamicWinds)-1]
	return callEval(func() evalResult {
		return it.applyProcedureWithContinuations(frame.outProc, nil, frame.callPos, func(_ []value) evalResult {
			return callEval(func() evalResult {
				return it.unwindDynamicWinds(prefix, target, next)
			})
		})
	})
}

func (it *interpreter) rewindDynamicWinds(target []*dynamicWindFrame, index int, next func() evalResult) evalResult {
	if index == len(target) {
		return callEval(next)
	}

	frame := target[index]
	return callEval(func() evalResult {
		return it.applyProcedureWithContinuations(frame.inProc, nil, frame.callPos, func(_ []value) evalResult {
			it.dynamicWinds = append(it.dynamicWinds, frame)
			return callEval(func() evalResult {
				return it.rewindDynamicWinds(target, index+1, next)
			})
		})
	})
}
