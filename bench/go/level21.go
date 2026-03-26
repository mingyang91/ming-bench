package ming

import "fmt"

type multipleValues struct {
	values []any
}

type callWithValuesProducerProc struct {
	consumer any
	target   any
	runtime  *runtimeState
}

func builtinValues(args []any) (any, error) {
	return multipleValues{values: append([]any(nil), args...)}, nil
}

func builtinCallWithValues(args []any) (any, error) {
	if len(args) != 2 {
		return nil, &EvalError{Message: "call-with-values expects exactly 2 arguments"}
	}

	produced, err := applyProcedureRaw(args[0], nil)
	if err != nil {
		return nil, err
	}

	return applyProcedureRaw(args[1], expandValues(produced))
}

func consumeSingleValue(value any) (any, error) {
	values, ok := value.(multipleValues)
	if !ok {
		return value, nil
	}

	switch len(values.values) {
	case 1:
		return values.values[0], nil
	case 0:
		return nil, &EvalError{Message: "expected 1 value, got 0"}
	default:
		return nil, &EvalError{Message: fmt.Sprintf("expected 1 value, got %d", len(values.values))}
	}
}

func expandValues(value any) []any {
	values, ok := value.(multipleValues)
	if !ok {
		return []any{value}
	}
	return append([]any(nil), values.values...)
}

func applyProcedureRaw(proc any, args []any) (any, error) {
	value, next, err := prepareProcedureCall(proc, args)
	if err != nil {
		return nil, err
	}
	if next != nil {
		return evalRaw(next.scope, next.expr)
	}
	return value, nil
}

func prepareCallWithValuesCPS(args []any, k any, runtime *runtimeState) (any, *tailEvalState, error) {
	if len(args) != 2 {
		return nil, nil, &EvalError{Message: "call-with-values expects exactly 2 arguments"}
	}

	return prepareApplyCPSCall(args[0], nil, callWithValuesProducerProc{
		consumer: args[1],
		target:   k,
		runtime:  runtime,
	}, runtime)
}

func prepareCallWithValuesProducerCall(callable callWithValuesProducerProc, args []any) (any, *tailEvalState, error) {
	if len(args) != 1 {
		return nil, nil, &EvalError{Message: "call-with-values producer continuation expects exactly 1 argument"}
	}

	return prepareApplyCPSCall(callable.consumer, expandValues(args[0]), callable.target, callable.runtime)
}
