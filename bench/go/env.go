package ming

import "fmt"

// Env represents a Scheme environment (scope).
type Env struct {
	bindings map[string]*Value
	parent   *Env
}

func NewEnv(parent *Env) *Env {
	return &Env{bindings: make(map[string]*Value), parent: parent}
}

func (e *Env) Get(name string) (*Value, bool) {
	if v, ok := e.bindings[name]; ok {
		return v, true
	}
	if e.parent != nil {
		return e.parent.Get(name)
	}
	return nil, false
}

func (e *Env) Set(name string, val *Value) {
	e.bindings[name] = val
}

type BuiltinFunc func(args []*Value, callExpr *Expr) (*Value, error)

func DefaultEnv() *Env {
	env := NewEnv(nil)

	addBuiltin := func(name string, fn BuiltinFunc) {
		// Store builtins as symbols with a special convention
		env.bindings[name] = &Value{Type: TypeSymbol, StrVal: "__builtin:" + name}
	}
	_ = addBuiltin

	// We'll handle builtins directly in eval for now
	return env
}

func errAt(expr *Expr, msg string) error {
	return &EvalError{Message: fmt.Sprintf("%d:%d: %s", expr.Line, expr.Col, msg)}
}

func errAtf(expr *Expr, format string, args ...interface{}) error {
	return errAt(expr, fmt.Sprintf(format, args...))
}
