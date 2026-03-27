package ming

import "strconv"

// EvalStr evaluates one or more Scheme expressions and returns the string
// representation of the last result.
func EvalStr(input string) (string, error) {
	result, _, err := evalInput(input)
	if err != nil {
		return "", err
	}
	return result, nil
}

// EvalStrWithLimit evaluates Scheme expressions with a step budget and returns
// the string representation of the last result.
func EvalStrWithLimit(input string, maxSteps int) (string, error) {
	result, _, err := evalInputWithLimit(input, maxSteps)
	if err != nil {
		return "", err
	}
	return result, nil
}

// EvalStrWithOutput evaluates Scheme expressions and returns both the result
// string and any captured output from display/write/newline.
func EvalStrWithOutput(input string) (result string, output string, err error) {
	result, output, err = evalInput(input)
	if err != nil {
		return "", output, err
	}

	if unquoted, unquoteErr := strconv.Unquote(result); unquoteErr == nil {
		result = unquoted
	}

	return result, output, nil
}
