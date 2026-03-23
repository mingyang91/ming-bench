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
