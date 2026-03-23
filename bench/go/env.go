package ming

// Env is a lexical environment (scope chain).
type Env struct {
	bindings map[string]SchemeValue
	parent   *Env
}

func NewEnv(parent *Env) *Env {
	return &Env{bindings: make(map[string]SchemeValue), parent: parent}
}

func (e *Env) Get(name string) (SchemeValue, bool) {
	if v, ok := e.bindings[name]; ok {
		return v, true
	}
	if e.parent != nil {
		return e.parent.Get(name)
	}
	return nil, false
}

func (e *Env) Set(name string, val SchemeValue) {
	e.bindings[name] = val
}

// SetExisting mutates an existing binding, walking up the scope chain.
// Returns false if the variable is not bound in any enclosing scope.
func (e *Env) SetExisting(name string, val SchemeValue) bool {
	if _, ok := e.bindings[name]; ok {
		e.bindings[name] = val
		return true
	}
	if e.parent != nil {
		return e.parent.SetExisting(name, val)
	}
	return false
}
