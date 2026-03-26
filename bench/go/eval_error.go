package ming

import "fmt"

// EvalError represents a Scheme evaluation error.
type EvalError struct {
	Message string
	Line    int
	Column  int
}

func (e *EvalError) Error() string {
	if e.Line > 0 && e.Column > 0 {
		return fmt.Sprintf("%d:%d: %s", e.Line, e.Column, e.Message)
	}
	return e.Message
}
