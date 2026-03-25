package ming

type multiValueValue struct {
	values []value
}

type callWithValuesProcValue struct{}

var callWithValuesBuiltin = &callWithValuesProcValue{}

func builtinValues() builtinProc {
	return func(args []value) (value, error) {
		return packValues(args), nil
	}
}

func builtinCallWithValues() value {
	return callWithValuesBuiltin
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
