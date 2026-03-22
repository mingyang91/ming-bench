package ming

import "strings"

// evalTopLevel evaluates a list of top-level expressions with trampoline
// support for re-entrant continuations (call/cc).
func evalTopLevel(exprs []*Expr, env *Env) (*Value, error) {
	ctx := &evalContext{exprs: exprs}
	env.evalCtx = ctx

	startIndex := 0

	for {
		var lastVal *Value
		var evalErr error
		var jump *continuationJump

		func() {
			defer func() {
				if r := recover(); r != nil {
					if j, ok := r.(continuationJump); ok {
						jump = &j
					} else {
						panic(r)
					}
				}
			}()

			for i := startIndex; i < len(exprs); i++ {
				ctx.currentIndex = i
				val, err := Eval(exprs[i], env)
				if err != nil {
					evalErr = err
					return
				}
				if val.Type != TypeVoid {
					lastVal = val
				}
			}
		}()

		if evalErr != nil {
			return nil, evalErr
		}
		if jump != nil {
			startIndex = jump.exprIndex
			ctx.resuming = true
			ctx.resumeValue = jump.value
			ctx.resumeExpr = jump.callExpr
			continue
		}

		return lastVal, nil
	}
}

// EvalStr evaluates one or more Scheme expressions and returns the string
// representation of the last result.
func EvalStr(input string) (string, error) {
	exprs, err := ParseAll(input)
	if err != nil {
		return "", &EvalError{Message: err.Error()}
	}
	if len(exprs) == 0 {
		return "", &EvalError{Message: "no expressions"}
	}

	env := makeDefaultEnv()
	lastVal, err := evalTopLevel(exprs, env)
	if err != nil {
		return "", err
	}
	if lastVal == nil {
		return "", nil
	}
	return lastVal.String(), nil
}

// EvalStrWithOutput evaluates Scheme expressions and returns both the result
// string and any captured output from display/write/newline.
func EvalStrWithOutput(input string) (result string, output string, err error) {
	exprs, parseErr := ParseAll(input)
	if parseErr != nil {
		return "", "", &EvalError{Message: parseErr.Error()}
	}
	if len(exprs) == 0 {
		return "", "", &EvalError{Message: "no expressions"}
	}

	env := makeDefaultEnv()
	var buf strings.Builder
	env.output = &buf

	lastVal, evalErr := evalTopLevel(exprs, env)
	if evalErr != nil {
		return "", "", evalErr
	}
	resultStr := ""
	if lastVal != nil {
		resultStr = lastVal.String()
	}
	return resultStr, buf.String(), nil
}
