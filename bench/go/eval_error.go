package ming

import "fmt"

// EvalErrorKind identifies a category of evaluation failure.
type EvalErrorKind string

const (
	ErrSyntax          EvalErrorKind = "syntax"
	ErrUnboundVariable EvalErrorKind = "unbound_variable"
	ErrTypeMismatch    EvalErrorKind = "type_mismatch"
	ErrWrongArgCount   EvalErrorKind = "wrong_arg_count"
	ErrDivisionByZero  EvalErrorKind = "division_by_zero"
	ErrNotProcedure    EvalErrorKind = "not_a_procedure"
	ErrOutOfRange      EvalErrorKind = "out_of_range"
	ErrImmutable       EvalErrorKind = "immutable"
	ErrRaised          EvalErrorKind = "raised"
)

// EvalError represents a Scheme evaluation error.
type EvalError struct {
	Message string
	Kind    EvalErrorKind
	Line    int
	Column  int
}

func (e *EvalError) Error() string {
	if e == nil {
		return "<nil>"
	}
	if e.Line > 0 && e.Column > 0 {
		return fmt.Sprintf("%d:%d: %s", e.Line, e.Column, e.Message)
	}
	return e.Message
}
