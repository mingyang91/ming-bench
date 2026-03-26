package ming

import "strings"

func evalInput(input string, env *Env) (*Value, error) {
	windStack = nil // reset dynamic-wind state
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

	result, err := evalSequenceWithCC(exprs, env)
	if err != nil {
		return nil, &EvalError{Message: err.Error()}
	}
	return result, nil
}

// evalSequenceWithCC evaluates a sequence of expressions, handling call/cc
// capture requests that propagate up via panic.
func evalSequenceWithCC(exprs []*Expr, env *Env) (result *Value, err error) {
	defer func() {
		if r := recover(); r != nil {
			if cr, ok := r.(*CaptureRequest); ok {
				result, err = processCallCC(cr)
				return
			}
			panic(r)
		}
	}()

	return evalSequence(exprs, env)
}

// evalSequence evaluates a list of expressions, returning the last value.
// It adds a continuation frame for the remaining expressions when a
// CaptureRequest propagates through.
func evalSequence(exprs []*Expr, env *Env) (*Value, error) {
	var last *Value
	for i, expr := range exprs {
		v, err := evalWithSeqCapture(expr, env, exprs, i)
		if err != nil {
			return nil, err
		}
		last = v
	}
	if last == nil {
		return Void, nil
	}
	return last, nil
}

// evalWithSeqCapture evaluates a single expression within a sequence,
// capturing the remaining sequence as a continuation frame if needed.
func evalWithSeqCapture(expr *Expr, env *Env, allExprs []*Expr, idx int) (result *Value, err error) {
	defer func() {
		if r := recover(); r != nil {
			if cr, ok := r.(*CaptureRequest); ok {
				remaining := allExprs[idx+1:]
				if len(remaining) > 0 {
					capturedRemaining := make([]*Expr, len(remaining))
					copy(capturedRemaining, remaining)
					capturedEnv := env
					cr.Frames = append(cr.Frames, ContFrame{
						Apply: func(val *Value) (*Value, error) {
							// Evaluate the remaining expressions in the sequence
							var last *Value = val
							for _, e := range capturedRemaining {
								v, err := Eval(e, capturedEnv)
								if err != nil {
									return nil, err
								}
								last = v
							}
							return last, nil
						},
					})
				}
				panic(cr) // Continue propagation
			}
			panic(r)
		}
	}()

	return Eval(expr, env)
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
