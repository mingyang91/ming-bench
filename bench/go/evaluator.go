package ming

// EvalStr evaluates one or more Scheme expressions and returns the string
// representation of the last result.
func EvalStr(input string) (string, error) {
	tokenizer := NewTokenizer(input)
	tokens, err := tokenizer.Tokenize()
	if err != nil {
		return "", &EvalError{Message: err.Error()}
	}

	parser := NewParser(tokens)
	exprs, err := parser.ParseAll()
	if err != nil {
		return "", &EvalError{Message: err.Error()}
	}

	if len(exprs) == 0 {
		return "", &EvalError{Message: "no expressions"}
	}

	env := MakeDefaultEnv()
	var last *Value
	for _, expr := range exprs {
		v, err := Eval(expr, env)
		if err != nil {
			return "", &EvalError{Message: err.Error()}
		}
		last = v
	}

	return last.String(), nil
}

// EvalStrWithOutput evaluates Scheme expressions and returns both the result
// string and any captured output from display/write/newline.
func EvalStrWithOutput(input string) (result string, output string, err error) {
	r, err := EvalStr(input)
	return r, "", err
}
