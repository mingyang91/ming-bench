package ming

func registerLevel11Builtins(global *env) {
	global.define("exact?", builtinProc{name: "exact?", fn: evalExactPred})
	global.define("inexact?", builtinProc{name: "inexact?", fn: evalInexactPred})
	global.define("exact->inexact", builtinProc{name: "exact->inexact", fn: evalExactToInexact})
	global.define("inexact->exact", builtinProc{name: "inexact->exact", fn: evalInexactToExact})
	global.define("numerator", builtinProc{name: "numerator", fn: evalNumerator})
	global.define("denominator", builtinProc{name: "denominator", fn: evalDenominator})
	global.define("integer?", builtinProc{name: "integer?", fn: evalIntegerPred})
	global.define("rational?", builtinProc{name: "rational?", fn: evalRationalPred})
}

func evalExactPred(args []value) (value, error) {
	return evalNumericPredicate(args, "exact?", func(n numberValue) bool { return n.exact })
}

func evalInexactPred(args []value) (value, error) {
	return evalNumericPredicate(args, "inexact?", func(n numberValue) bool { return !n.exact })
}

func evalIntegerPred(args []value) (value, error) {
	return evalNumericPredicate(args, "integer?", func(n numberValue) bool { return n.isInteger() })
}

func evalRationalPred(args []value) (value, error) {
	return evalNumericPredicate(args, "rational?", func(numberValue) bool { return true })
}

func evalExactToInexact(args []value) (value, error) {
	if len(args) != 1 {
		return nil, newCurrentEvalError("'exact->inexact' expects exactly 1 argument")
	}

	n, err := expectNumber(args[0])
	if err != nil {
		return nil, err
	}
	return n.toInexact(), nil
}

func evalInexactToExact(args []value) (value, error) {
	if len(args) != 1 {
		return nil, newCurrentEvalError("'inexact->exact' expects exactly 1 argument")
	}

	n, err := expectNumber(args[0])
	if err != nil {
		return nil, err
	}
	return n.toExact(), nil
}

func evalNumerator(args []value) (value, error) {
	if len(args) != 1 {
		return nil, newCurrentEvalError("'numerator' expects exactly 1 argument")
	}

	n, err := expectNumber(args[0])
	if err != nil {
		return nil, err
	}
	return newExactInteger(n.numer), nil
}

func evalDenominator(args []value) (value, error) {
	if len(args) != 1 {
		return nil, newCurrentEvalError("'denominator' expects exactly 1 argument")
	}

	n, err := expectNumber(args[0])
	if err != nil {
		return nil, err
	}
	return newExactInteger(n.denom), nil
}

func evalNumericPredicate(args []value, name string, pred func(numberValue) bool) (value, error) {
	if len(args) != 1 {
		return nil, newCurrentEvalError("'%s' expects exactly 1 argument", name)
	}

	n, ok := args[0].(numberValue)
	if !ok {
		return boolValue(false), nil
	}
	return boolValue(pred(n)), nil
}
