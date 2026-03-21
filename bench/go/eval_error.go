package ming

// EvalError represents a Scheme evaluation error.
type EvalError struct {
	Message string
}

func (e *EvalError) Error() string {
	return e.Message
}
