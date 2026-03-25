package ming

type multiValueValue struct {
	values []value
}

func builtinValues() builtinProc {
	return func(args []value) (value, error) {
		return packValues(args), nil
	}
}

func builtinCallWithValues() builtinProc {
	return func(args []value) (value, error) {
		if len(args) != 2 {
			return nil, &EvalError{Message: "call-with-values expects exactly 2 arguments"}
		}
		if !isProcedureValue(args[0]) || !isProcedureValue(args[1]) {
			return nil, &EvalError{Message: "call-with-values expects 2 procedures"}
		}

		produced, err := applyProcedure(args[0], nil, sourcePos{})
		if err != nil {
			return nil, err
		}

		return applyProcedure(args[1], unpackValues(produced), sourcePos{})
	}
}

func packValues(values []value) value {
	if len(values) == 1 {
		return values[0]
	}
	return multiValueValue{values: copyValues(values)}
}

func unpackValues(v value) []value {
	if multi, ok := v.(multiValueValue); ok {
		return copyValues(multi.values)
	}
	return []value{v}
}

func multiValueContextError(count int) error {
	return &EvalError{Message: "multiple values are not allowed in this context"}
}
