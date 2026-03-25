package ming

import "fmt"

// SyntaxRulesVal represents a syntax-rules macro transformer.
type SyntaxRulesVal struct {
	Name     string
	Literals []string
	Rules    []syntaxRule
	DefEnv   *Env
}

func (v *SyntaxRulesVal) String() string {
	return fmt.Sprintf("#<macro %s>", v.Name)
}

type syntaxRule struct {
	Pattern  []Expr // pattern elements (excluding the macro name)
	Template Expr
}

// EnvRefExpr is an expression that resolves a variable from a captured environment.
// Used for hygienic macro expansion to preserve definition-site bindings.
type EnvRefExpr struct {
	Name string
	Env  *Env
	Line int
	Col  int
}

func (e *EnvRefExpr) pos() (int, int) { return e.Line, e.Col }

var gensymCounter int

func gensym(base string) string {
	gensymCounter++
	return fmt.Sprintf("_gs_%s_%d", base, gensymCounter)
}

var specialForms = map[string]bool{
	"define": true, "if": true, "lambda": true, "quote": true,
	"begin": true, "let": true, "cond": true, "and": true, "or": true,
	"set!": true, "not": true, "define-syntax": true,
}

// evalDefineSyntax handles (define-syntax name (syntax-rules ...)).
func evalDefineSyntax(e *ListExpr, env *Env) (Value, error) {
	if len(e.Elems) != 3 {
		return nil, &EvalError{Message: fmt.Sprintf("%d:%d: define-syntax requires 2 arguments", e.Line, e.Col)}
	}
	nameSym, ok := e.Elems[1].(*SymbolExpr)
	if !ok {
		return nil, &EvalError{Message: fmt.Sprintf("%d:%d: define-syntax: expected symbol", e.Line, e.Col)}
	}
	srExpr, ok := e.Elems[2].(*ListExpr)
	if !ok || len(srExpr.Elems) < 2 {
		return nil, &EvalError{Message: fmt.Sprintf("%d:%d: define-syntax: expected syntax-rules", e.Line, e.Col)}
	}
	srHead, ok := srExpr.Elems[0].(*SymbolExpr)
	if !ok || srHead.Name != "syntax-rules" {
		return nil, &EvalError{Message: fmt.Sprintf("%d:%d: define-syntax: expected syntax-rules", e.Line, e.Col)}
	}
	litList, ok := srExpr.Elems[1].(*ListExpr)
	if !ok {
		return nil, &EvalError{Message: fmt.Sprintf("%d:%d: syntax-rules: expected literal list", e.Line, e.Col)}
	}
	var literals []string
	for _, l := range litList.Elems {
		ls, ok := l.(*SymbolExpr)
		if !ok {
			return nil, &EvalError{Message: fmt.Sprintf("%d:%d: syntax-rules: literal must be a symbol", e.Line, e.Col)}
		}
		literals = append(literals, ls.Name)
	}
	var rules []syntaxRule
	for _, r := range srExpr.Elems[2:] {
		rl, ok := r.(*ListExpr)
		if !ok || len(rl.Elems) != 2 {
			return nil, &EvalError{Message: fmt.Sprintf("%d:%d: syntax-rules: bad rule", e.Line, e.Col)}
		}
		patternExpr, ok := rl.Elems[0].(*ListExpr)
		if !ok {
			return nil, &EvalError{Message: fmt.Sprintf("%d:%d: syntax-rules: pattern must be a list", e.Line, e.Col)}
		}
		rules = append(rules, syntaxRule{
			Pattern:  patternExpr.Elems[1:], // skip macro name in pattern
			Template: rl.Elems[1],
		})
	}
	macro := &SyntaxRulesVal{
		Name:     nameSym.Name,
		Literals: literals,
		Rules:    rules,
		DefEnv:   env,
	}
	env.set(nameSym.Name, macro)
	return &VoidVal{}, nil
}

// expandMacro expands a macro application.
func expandMacro(macro *SyntaxRulesVal, form *ListExpr) (Expr, error) {
	formArgs := form.Elems[1:]
	for _, rule := range macro.Rules {
		bindings := make(map[string]interface{})
		if matchPatternElems(rule.Pattern, formArgs, macro.Literals, bindings) {
			gensyms := make(map[string]string)
			return instantiateTemplate(rule.Template, bindings, macro.DefEnv, gensyms), nil
		}
	}
	line, col := form.pos()
	return nil, &EvalError{Message: fmt.Sprintf("%d:%d: no matching pattern for macro %s", line, col, macro.Name)}
}

// matchPatternElems matches a list of pattern elements against form elements.
func matchPatternElems(patElems []Expr, formElems []Expr, literals []string, bindings map[string]interface{}) bool {
	// Find ellipsis position
	ellipsisIdx := -1
	for i, p := range patElems {
		if sym, ok := p.(*SymbolExpr); ok && sym.Name == "..." {
			ellipsisIdx = i
			break
		}
	}

	if ellipsisIdx == -1 {
		if len(patElems) != len(formElems) {
			return false
		}
		for i, p := range patElems {
			if !matchSinglePattern(p, formElems[i], literals, bindings) {
				return false
			}
		}
		return true
	}

	// Has ellipsis: repeated pattern is at ellipsisIdx-1
	beforeCount := ellipsisIdx - 1
	afterCount := len(patElems) - ellipsisIdx - 1

	if len(formElems) < beforeCount+afterCount {
		return false
	}

	// Match elements before the repeated pattern
	for i := 0; i < beforeCount; i++ {
		if !matchSinglePattern(patElems[i], formElems[i], literals, bindings) {
			return false
		}
	}

	// Match elements after the ellipsis
	for i := 0; i < afterCount; i++ {
		patIdx := ellipsisIdx + 1 + i
		formIdx := len(formElems) - afterCount + i
		if !matchSinglePattern(patElems[patIdx], formElems[formIdx], literals, bindings) {
			return false
		}
	}

	// Collect pattern variables from the repeated pattern
	repeatedPat := patElems[ellipsisIdx-1]
	patVars := collectPatternVars(repeatedPat, literals)
	for _, v := range patVars {
		bindings[v] = []Expr{}
	}

	// Match repeated elements
	middleStart := beforeCount
	middleEnd := len(formElems) - afterCount
	for i := middleStart; i < middleEnd; i++ {
		subBindings := make(map[string]interface{})
		if !matchSinglePattern(repeatedPat, formElems[i], literals, subBindings) {
			return false
		}
		for _, v := range patVars {
			if sub, ok := subBindings[v]; ok {
				bindings[v] = append(bindings[v].([]Expr), sub.(Expr))
			}
		}
	}

	return true
}

// matchSinglePattern matches a single pattern element against a form.
func matchSinglePattern(pattern Expr, form Expr, literals []string, bindings map[string]interface{}) bool {
	switch p := pattern.(type) {
	case *SymbolExpr:
		if p.Name == "_" {
			return true
		}
		for _, lit := range literals {
			if p.Name == lit {
				if fs, ok := form.(*SymbolExpr); ok {
					return fs.Name == p.Name
				}
				return false
			}
		}
		// Pattern variable
		bindings[p.Name] = form
		return true
	case *ListExpr:
		fl, ok := form.(*ListExpr)
		if !ok {
			return false
		}
		return matchPatternElems(p.Elems, fl.Elems, literals, bindings)
	case *NumberExpr:
		if fn, ok := form.(*NumberExpr); ok {
			return p.Val == fn.Val
		}
		return false
	case *BoolExpr:
		if fb, ok := form.(*BoolExpr); ok {
			return p.Val == fb.Val
		}
		return false
	case *StringExpr:
		if fs, ok := form.(*StringExpr); ok {
			return p.Val == fs.Val
		}
		return false
	default:
		return false
	}
}

// collectPatternVars returns all pattern variable names in a pattern.
func collectPatternVars(pattern Expr, literals []string) []string {
	var vars []string
	collectPatternVarsHelper(pattern, literals, &vars)
	return vars
}

func collectPatternVarsHelper(pattern Expr, literals []string, vars *[]string) {
	switch p := pattern.(type) {
	case *SymbolExpr:
		if p.Name == "_" || p.Name == "..." {
			return
		}
		for _, lit := range literals {
			if p.Name == lit {
				return
			}
		}
		*vars = append(*vars, p.Name)
	case *ListExpr:
		for _, e := range p.Elems {
			collectPatternVarsHelper(e, literals, vars)
		}
	}
}

// instantiateTemplate replaces pattern variables and handles hygiene.
func instantiateTemplate(tmpl Expr, bindings map[string]interface{}, defEnv *Env, gensyms map[string]string) Expr {
	switch t := tmpl.(type) {
	case *SymbolExpr:
		// Pattern variable?
		if val, ok := bindings[t.Name]; ok {
			if expr, ok := val.(Expr); ok {
				return expr
			}
			// []Expr in non-ellipsis context — shouldn't happen normally
		}
		// Special form: keep as-is
		if specialForms[t.Name] {
			return t
		}
		// Exists in definition-site env: resolve from there (hygiene)
		if _, ok := defEnv.get(t.Name); ok {
			return &EnvRefExpr{Name: t.Name, Env: defEnv, Line: t.Line, Col: t.Col}
		}
		// Introduced binding: gensym for hygiene
		if gs, ok := gensyms[t.Name]; ok {
			return &SymbolExpr{Name: gs, Line: t.Line, Col: t.Col}
		}
		gs := gensym(t.Name)
		gensyms[t.Name] = gs
		return &SymbolExpr{Name: gs, Line: t.Line, Col: t.Col}
	case *ListExpr:
		var newElems []Expr
		for i := 0; i < len(t.Elems); i++ {
			// Check if next element is ellipsis
			if i+1 < len(t.Elems) {
				if sym, ok := t.Elems[i+1].(*SymbolExpr); ok && sym.Name == "..." {
					ellipsisVars := findEllipsisVars(t.Elems[i], bindings)
					if len(ellipsisVars) > 0 {
						count := len(bindings[ellipsisVars[0]].([]Expr))
						for j := 0; j < count; j++ {
							subBindings := copyBindings(bindings)
							for _, ev := range ellipsisVars {
								subBindings[ev] = bindings[ev].([]Expr)[j]
							}
							newElems = append(newElems, instantiateTemplate(t.Elems[i], subBindings, defEnv, gensyms))
						}
					}
					i++ // skip the ...
					continue
				}
			}
			newElems = append(newElems, instantiateTemplate(t.Elems[i], bindings, defEnv, gensyms))
		}
		return &ListExpr{Elems: newElems, Line: t.Line, Col: t.Col}
	default:
		return tmpl
	}
}

func findEllipsisVars(tmpl Expr, bindings map[string]interface{}) []string {
	var vars []string
	findEllipsisVarsHelper(tmpl, bindings, &vars)
	return vars
}

func findEllipsisVarsHelper(tmpl Expr, bindings map[string]interface{}, vars *[]string) {
	switch t := tmpl.(type) {
	case *SymbolExpr:
		if val, ok := bindings[t.Name]; ok {
			if _, ok := val.([]Expr); ok {
				*vars = append(*vars, t.Name)
			}
		}
	case *ListExpr:
		for _, e := range t.Elems {
			findEllipsisVarsHelper(e, bindings, vars)
		}
	}
}

func copyBindings(b map[string]interface{}) map[string]interface{} {
	c := make(map[string]interface{})
	for k, v := range b {
		c[k] = v
	}
	return c
}
