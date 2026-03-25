package ming

import (
	"errors"
	"fmt"
)

type sourcePos struct {
	Line int
	Col  int
}

func startPos() sourcePos {
	return sourcePos{Line: 1, Col: 1}
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

func errorAt(pos sourcePos, format string, args ...interface{}) *EvalError {
	message := format
	if len(args) > 0 {
		message = fmt.Sprintf(format, args...)
	}

	return &EvalError{
		Message: message,
		Line:    pos.Line,
		Col:     pos.Col,
	}
}

func withErrorPos(err error, pos sourcePos) error {
	if err == nil || !pos.valid() {
		return err
	}

	var evalErr *EvalError
	if errors.As(err, &evalErr) {
		if evalErr.Line > 0 && evalErr.Col > 0 {
			return err
		}

		copy := *evalErr
		copy.Line = pos.Line
		copy.Col = pos.Col
		return &copy
	}

	return errorAt(pos, err.Error())
}
