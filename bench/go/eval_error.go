package ming

import "fmt"

type sourcePos struct {
	Line int
	Col  int
}

func (p sourcePos) valid() bool {
	return p.Line > 0 && p.Col > 0
}

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

func errorAt(pos sourcePos, message string) *EvalError {
	return &EvalError{
		Message: message,
		Line:    pos.Line,
		Col:     pos.Col,
	}
}

func attachPos(err error, pos sourcePos) error {
	if err == nil || !pos.valid() {
		return err
	}

	evalErr, ok := err.(*EvalError)
	if !ok {
		return &EvalError{
			Message: err.Error(),
			Line:    pos.Line,
			Col:     pos.Col,
		}
	}
	if evalErr.Line > 0 && evalErr.Col > 0 {
		return err
	}
	return &EvalError{
		Message: evalErr.Message,
		Line:    pos.Line,
		Col:     pos.Col,
	}
}
