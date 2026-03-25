package ming

import "strings"

// EvalStr evaluates one or more Scheme expressions and returns the string
// representation of the last result.
func EvalStr(input string) (string, error) {
	exprs, err := parse(input)
	if err != nil {
		return "", &EvalError{Message: err.Error()}
	}
	if len(exprs) == 0 {
		return "", &EvalError{Message: "no expressions"}
	}

	env := makeGlobalEnv(nil)
	var lastVal Value
	for _, expr := range exprs {
		v, err := eval(expr, env)
		if err != nil {
			return "", err
		}
		lastVal = v
	}

	if _, ok := lastVal.(*VoidVal); ok {
		return "", nil
	}
	return lastVal.String(), nil
}

// EvalStrWithOutput evaluates Scheme expressions and returns both the result
// string and any captured output from display/write/newline.
func EvalStrWithOutput(input string) (result string, output string, err error) {
	exprs, parseErr := parse(input)
	if parseErr != nil {
		return "", "", &EvalError{Message: parseErr.Error()}
	}
	if len(exprs) == 0 {
		return "", "", &EvalError{Message: "no expressions"}
	}

	var out strings.Builder
	env := makeGlobalEnv(&out)
	var lastVal Value
	for _, expr := range exprs {
		v, evalErr := eval(expr, env)
		if evalErr != nil {
			return "", "", evalErr
		}
		lastVal = v
	}

	res := ""
	if _, ok := lastVal.(*VoidVal); !ok {
		res = lastVal.String()
	}
	return res, out.String(), nil
}
