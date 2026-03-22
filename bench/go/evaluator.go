package ming

// EvalStr evaluates one or more Scheme expressions and returns the string
// representation of the last result.
func EvalStr(input string) (string, error) {
	exprs, err := ParseAll(input)
	if err != nil {
		return "", &EvalError{Message: err.Error()}
	}
	if len(exprs) == 0 {
		return "", &EvalError{Message: "no expressions"}
	}

	env := makeDefaultEnv()
	var lastVal *Value
	for _, expr := range exprs {
		val, err := Eval(expr, env)
		if err != nil {
			return "", err
		}
		if val.Type != TypeVoid {
			lastVal = val
		}
	}
	if lastVal == nil {
		return "", nil
	}
	return lastVal.String(), nil
}

// EvalStrWithOutput evaluates Scheme expressions and returns both the result
// string and any captured output from display/write/newline.
func EvalStrWithOutput(input string) (result string, output string, err error) {
	return "", "", &EvalError{Message: "not implemented"}
}
