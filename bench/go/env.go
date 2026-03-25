package ming

// Env is a lexical environment.
type Env struct {
	bindings  map[string]Value
	parent    *Env
	evalState *EvalState
}

func newEnv(parent *Env) *Env {
	e := &Env{bindings: make(map[string]Value), parent: parent}
	if parent != nil {
		e.evalState = parent.evalState
	}
	return e
}

func (e *Env) get(name string) (Value, bool) {
	if v, ok := e.bindings[name]; ok {
		return v, true
	}
	if e.parent != nil {
		return e.parent.get(name)
	}
	return nil, false
}

func (e *Env) set(name string, val Value) {
	e.bindings[name] = val
}

// setMut updates an existing binding in the nearest enclosing scope.
// Returns false if the variable is unbound in all scopes.
func (e *Env) setMut(name string, val Value) bool {
	if _, ok := e.bindings[name]; ok {
		e.bindings[name] = val
		return true
	}
	if e.parent != nil {
		return e.parent.setMut(name, val)
	}
	return false
}
