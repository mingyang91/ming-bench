package ming

import "fmt"

func builtinValues(_ *interpreter, _ []value, callPos position) (value, error) {
	return nil, newEvalError(ErrSyntax, "values requires continuation-aware evaluation", callPos)
}

func builtinCallWithValues(_ *interpreter, _ []value, callPos position) (value, error) {
	return nil, newEvalError(ErrSyntax, "call-with-values requires continuation-aware evaluation", callPos)
}

func copyValueSlice(values []value) []value {
	if len(values) == 0 {
		return nil
	}

	copied := make([]value, len(values))
	copy(copied, values)
	return copied
}

func wrongValueCount(pos position, expected int, got int) error {
	return newEvalError(ErrWrongValueCount, fmt.Sprintf("expected %d value(s), got %d", expected, got), pos)
}

func expectSingleValue(values []value, pos position) (value, error) {
	if len(values) != 1 {
		return nil, wrongValueCount(pos, 1, len(values))
	}
	return values[0], nil
}

func singleValueContinuation(pos position, next func(value) evalResult) evalContinuation {
	return func(values []value) evalResult {
		current, err := expectSingleValue(values, pos)
		if err != nil {
			return doneError(err)
		}
		return next(current)
	}
}
