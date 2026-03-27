package ming

import "fmt"

// SourcePos represents a 1-based source location.
type SourcePos struct {
	Line int
	Col  int
}

func defaultSourcePos() SourcePos {
	return SourcePos{Line: 1, Col: 1}
}

func (p SourcePos) isValid() bool {
	return p.Line > 0 && p.Col > 0
}

func (p SourcePos) normalized() SourcePos {
	if !p.isValid() {
		return defaultSourcePos()
	}
	return p
}

// EvalError represents a Scheme evaluation error.
type EvalError struct {
	Message string
	Pos     SourcePos
}

func (e *EvalError) Error() string {
	pos := e.Pos.normalized()
	return fmt.Sprintf("%d:%d: %s", pos.Line, pos.Col, e.Message)
}

func newEvalError(pos SourcePos, format string, args ...interface{}) *EvalError {
	return &EvalError{
		Message: fmt.Sprintf(format, args...),
		Pos:     pos.normalized(),
	}
}

var currentEvalPos = defaultSourcePos()

func pushEvalPos(pos SourcePos) func() {
	prev := currentEvalPos
	if pos.isValid() {
		currentEvalPos = pos
	}
	return func() {
		currentEvalPos = prev
	}
}

func newCurrentEvalError(format string, args ...interface{}) *EvalError {
	return newEvalError(currentEvalPos, format, args...)
}
