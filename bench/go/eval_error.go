package ming

import "fmt"

// EvalError represents a Scheme evaluation error.
type EvalError struct {
	Message string
}

func (e *EvalError) Error() string {
	return e.Message
}

func posError(v *Value, msg string) *EvalError {
	if v != nil && v.Line > 0 {
		return &EvalError{Message: fmt.Sprintf("%d:%d: %s", v.Line, v.Col, msg)}
	}
	return &EvalError{Message: msg}
}

// wrapErrorPos adds position info to an error if it doesn't already have it.
func wrapErrorPos(err error, v *Value) error {
	if err == nil {
		return nil
	}
	ee, ok := err.(*EvalError)
	if !ok || v == nil || v.Line == 0 {
		return err
	}
	// Check if message already starts with position (digit...colon pattern)
	msg := ee.Message
	for i := 0; i < len(msg); i++ {
		if msg[i] == ':' {
			// Found a colon, check if everything before was digits
			if i > 0 {
				allDigits := true
				for j := 0; j < i; j++ {
					if msg[j] < '0' || msg[j] > '9' {
						allDigits = false
						break
					}
				}
				if allDigits {
					return err // already has position
				}
			}
			break
		}
		if msg[i] < '0' || msg[i] > '9' {
			break
		}
	}
	return posError(v, msg)
}
