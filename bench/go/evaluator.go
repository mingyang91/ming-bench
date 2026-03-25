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
	env.evalState = newEvalState()
	lastVal, err := evalTopLevel(exprs, env)
	if err != nil {
		return "", err
	}

	if _, ok := lastVal.(*VoidVal); ok {
		return "", nil
	}
	return formatResult(lastVal), nil
}

// formatResult formats a value for EvalStr output.
// Top-level strings are returned without quotes (display format);
// all other values use their standard String() (write format).
func formatResult(v Value) string {
	if s, ok := v.(*StringVal); ok {
		return s.Val
	}
	return v.String()
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
	env.evalState = newEvalState()
	lastVal, evalErr := evalTopLevel(exprs, env)
	if evalErr != nil {
		return "", "", evalErr
	}

	res := ""
	if _, ok := lastVal.(*VoidVal); !ok {
		res = formatResult(lastVal)
	}
	return res, out.String(), nil
}

// EvalStrWithLimit evaluates Scheme expressions with a step budget.
// Each eval dispatch counts as one step. Returns an error if the budget is exhausted.
func EvalStrWithLimit(input string, maxSteps int) (string, error) {
	exprs, err := parse(input)
	if err != nil {
		return "", &EvalError{Message: err.Error()}
	}
	if len(exprs) == 0 {
		return "", &EvalError{Message: "no expressions"}
	}

	env := makeGlobalEnv(nil)
	env.evalState = newEvalState()
	env.evalState.stepLimit = int64(maxSteps)
	lastVal, err := evalTopLevel(exprs, env)
	if err != nil {
		return "", err
	}

	if _, ok := lastVal.(*VoidVal); ok {
		return "", nil
	}
	return formatResult(lastVal), nil
}
