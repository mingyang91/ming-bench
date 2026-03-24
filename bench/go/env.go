package ming

// Env represents a Scheme environment (lexical scope chain).
type Env struct {
	bindings map[string]Value
	parent   *Env
}

func NewEnv(parent *Env) *Env {
	return &Env{bindings: make(map[string]Value), parent: parent}
}

func (e *Env) Get(name string) (Value, bool) {
	if v, ok := e.bindings[name]; ok {
		return v, true
	}
	if e.parent != nil {
		return e.parent.Get(name)
	}
	return nil, false
}

func (e *Env) Set(name string, val Value) {
	e.bindings[name] = val
}

// SetMut mutates an existing binding (walks up scope chain). Returns false if unbound.
func (e *Env) SetMut(name string, val Value) bool {
	if _, ok := e.bindings[name]; ok {
		e.bindings[name] = val
		return true
	}
	if e.parent != nil {
		return e.parent.SetMut(name, val)
	}
	return false
}
