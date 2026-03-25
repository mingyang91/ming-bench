package ming

// EvalStr evaluates one or more Scheme expressions and returns the string
// representation of the last result.
func EvalStr(input string) (string, error) {
	result, _, err := evalInput(input)
	if err != nil {
		return "", err
	}
	return formatValue(result)
}

// EvalStrWithLimit evaluates Scheme expressions with a step budget and returns
// the final result string if evaluation completes before the budget is
// exhausted.
func EvalStrWithLimit(input string, maxSteps int) (string, error) {
	result, _, err := evalInputWithLimit(input, maxSteps)
	if err != nil {
		return "", err
	}
	return formatValue(result)
}

// EvalStrWithOutput evaluates Scheme expressions and returns both the result
// string and any captured output from display/write/newline.
func EvalStrWithOutput(input string) (result string, output string, err error) {
	value, output, err := evalInput(input)
	if err != nil {
		return "", output, err
	}

	formatted, err := formatDisplayValue(value)
	if err != nil {
		return "", "", err
	}
	return formatted, output, nil
}
