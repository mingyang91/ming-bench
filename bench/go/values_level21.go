package ming

import "fmt"

type multiValueExpr struct {
	values []expr
}

func makeValuesExpr(values []expr) expr {
	if len(values) == 1 {
		return values[0]
	}

	return &multiValueExpr{
		values: append([]expr(nil), values...),
	}
}

func valuesSlice(value expr) []expr {
	if values, ok := value.(*multiValueExpr); ok {
		return append([]expr(nil), values.values...)
	}
	return []expr{value}
}

func expectSingleValue(value expr, context string) (expr, error) {
	if values, ok := value.(*multiValueExpr); ok {
		return nil, &EvalError{Message: fmt.Sprintf("%s expected 1 value, got %d", context, len(values.values))}
	}
	return value, nil
}

func evalSingleExpr(environment *env, form expr, context string) (expr, error) {
	value, err := evalExpr(environment, form)
	if err != nil {
		return nil, err
	}
	return expectSingleValue(value, context)
}

func builtinValues(args []expr) (expr, error) {
	return makeValuesExpr(args), nil
}

func builtinCallWithValues(args []expr) (expr, error) {
	if len(args) != 2 {
		return nil, &EvalError{Message: "call-with-values expects exactly 2 arguments"}
	}

	produced, err := applyCallable(args[0], nil)
	if err != nil {
		return nil, err
	}

	return applyCallable(args[1], valuesSlice(produced))
}
