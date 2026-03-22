package ming

import (
	"fmt"
	"strings"
)

// evalContext tracks state for the top-level evaluation trampoline (call/cc support).
type evalContext struct {
	exprs        []*Expr
	currentIndex int
	resumeValue  *Value
	resuming     bool
}

// Env represents a Scheme environment (scope).
type Env struct {
	bindings map[string]*Value
	parent   *Env
	output   *strings.Builder // shared output buffer (only set on root)
	evalCtx  *evalContext     // set on root env for call/cc support
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

// SetExisting mutates an existing binding, walking up the scope chain.
// Returns false if the binding is not found in any scope.
func (e *Env) SetExisting(name string, val *Value) bool {
	if _, ok := e.bindings[name]; ok {
		e.bindings[name] = val
		return true
	}
	if e.parent != nil {
		return e.parent.SetExisting(name, val)
	}
	return false
}

// getEvalCtx returns the evalContext, walking up to root.
func (e *Env) getEvalCtx() *evalContext {
	for cur := e; cur != nil; cur = cur.parent {
		if cur.evalCtx != nil {
			return cur.evalCtx
		}
	}
	return nil
}

// Output returns the shared output buffer, walking up to root.
func (e *Env) Output() *strings.Builder {
	if e.output != nil {
		return e.output
	}
	if e.parent != nil {
		return e.parent.Output()
	}
	return nil
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
