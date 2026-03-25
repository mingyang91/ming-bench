package ming

import "strings"

// EvalStr evaluates one or more Scheme expressions and returns the string
// representation of the last result.
func EvalStr(input string) (string, error) {
	parser := NewParser(input)
	exprs, err := parser.ParseAll()
	if err != nil {
		return "", &EvalError{Message: err.Error()}
	}
	if len(exprs) == 0 {
		return "", &EvalError{Message: "no expressions"}
	}

	env := makeGlobalEnv()
	var result *Value
	for _, expr := range exprs {
		val, err := Eval(expr, env)
		if err != nil {
			return "", &EvalError{Message: err.Error()}
		}
		if val.Type != TypeVoid {
			result = val
		}
	}
	if result == nil {
		return "", nil
	}
	return result.Display(), nil
}

// EvalStrWithOutput evaluates Scheme expressions and returns both the result
// string and any captured output from display/write/newline.
func EvalStrWithOutput(input string) (result string, output string, err error) {
	parser := NewParser(input)
	exprs, parseErr := parser.ParseAll()
	if parseErr != nil {
		return "", "", &EvalError{Message: parseErr.Error()}
	}
	if len(exprs) == 0 {
		return "", "", &EvalError{Message: "no expressions"}
	}

	env := makeGlobalEnv()
	var buf strings.Builder
	env.output = &buf

	var res *Value
	for _, expr := range exprs {
		val, evalErr := Eval(expr, env)
		if evalErr != nil {
			return "", "", &EvalError{Message: evalErr.Error()}
		}
		if val.Type != TypeVoid {
			res = val
		}
	}
	resultStr := ""
	if res != nil {
		resultStr = res.Display()
	}
	return resultStr, buf.String(), nil
}
