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
	result, err := cekEvalAll(exprs, env)
	if err != nil {
		return "", &EvalError{Message: err.Error()}
	}
	if result == nil || result.Type == TypeVoid {
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

	res, evalErr := cekEvalAll(exprs, env)
	if evalErr != nil {
		return "", "", &EvalError{Message: evalErr.Error()}
	}
	resultStr := ""
	if res != nil && res.Type != TypeVoid {
		resultStr = res.Display()
	}
	return resultStr, buf.String(), nil
}
