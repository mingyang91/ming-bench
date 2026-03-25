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

// EvalStrWithOutput evaluates Scheme expressions and returns both the result
// string and any captured output from display/write/newline.
func EvalStrWithOutput(input string) (result string, output string, err error) {
	value, output, err := evalInput(input)
	if err != nil {
		return "", "", err
	}

	formatted, err := formatValue(value)
	if err != nil {
		return "", "", err
	}
	return formatted, output, nil
}
