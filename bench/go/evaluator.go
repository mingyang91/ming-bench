package ming

import "strings"

func evalInput(input string, env *Env) (*Value, error) {
	tokenizer := NewTokenizer(input)
	tokens, err := tokenizer.Tokenize()
	if err != nil {
		return nil, &EvalError{Message: err.Error()}
	}

	parser := NewParser(tokens)
	exprs, err := parser.ParseAll()
	if err != nil {
		return nil, &EvalError{Message: err.Error()}
	}

	if len(exprs) == 0 {
		return nil, &EvalError{Message: "no expressions"}
	}

	var last *Value
	for _, expr := range exprs {
		v, err := Eval(expr, env)
		if err != nil {
			return nil, &EvalError{Message: err.Error()}
		}
		last = v
	}
	return last, nil
}

// EvalStr evaluates one or more Scheme expressions and returns the string
// representation of the last result.
func EvalStr(input string) (string, error) {
	env := MakeDefaultEnv()
	v, err := evalInput(input, env)
	if err != nil {
		return "", err
	}
	return v.String(), nil
}

// EvalStrWithOutput evaluates Scheme expressions and returns both the result
// string and any captured output from display/write/newline.
func EvalStrWithOutput(input string) (result string, output string, err error) {
	env := MakeDefaultEnv()
	var buf strings.Builder
	env.output = &buf
	v, err := evalInput(input, env)
	if err != nil {
		return "", "", err
	}
	return v.String(), buf.String(), nil
}
