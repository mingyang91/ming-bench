package ming

import "fmt"

// EvalError represents a Scheme evaluation error.
type EvalError struct {
	Message string
	Line    int
	Col     int
}

func (e *EvalError) Error() string {
	if e.Line > 0 && e.Col > 0 {
		return fmt.Sprintf("%d:%d: %s", e.Line, e.Col, e.Message)
	}
	return e.Message
}
