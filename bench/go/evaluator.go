package ming

// EvalStr evaluates one or more Scheme expressions and returns the string
// representation of the last result.
func EvalStr(input string) (string, error) {
	return evalProgram(input)
}

// EvalStrWithLimit evaluates Scheme expressions with a step budget and returns
// the string representation of the last result.
func EvalStrWithLimit(input string, maxSteps int) (string, error) {
	return evalProgramWithLimit(input, maxSteps)
}

// EvalStrWithOutput evaluates Scheme expressions and returns both the result
// string and any captured output from display/write/newline.
func EvalStrWithOutput(input string) (result string, output string, err error) {
	return evalProgramWithOutput(input)
}
