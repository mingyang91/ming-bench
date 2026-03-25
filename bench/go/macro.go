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
	"guard": true, "raise": true, "dynamic-wind": true,
	"with-exception-handler": true, "define-record-type": true,
	"let*": true, "letrec": true, "letrec*": true, "do": true,
	"case": true, "when": true, "unless": true, "syntax-rules": true,
	"call/cc": true, "call-with-current-continuation": true,
	"case-lambda": true, "quasiquote": true, "let-values": true,
}

// evalDefineSyntax handles (define-syntax name (syntax-rules ...)) and
// (define-syntax name (lambda (stx) ...)) for syntax-case transformers.
func evalDefineSyntax(e *ListExpr, env *Env) (Value, error) {
	if len(e.Elems) != 3 {
		return nil, &EvalError{Message: fmt.Sprintf("%d:%d: define-syntax requires 2 arguments", e.Line, e.Col)}
	}
	nameSym, ok := e.Elems[1].(*SymbolExpr)
	if !ok {
		return nil, &EvalError{Message: fmt.Sprintf("%d:%d: define-syntax: expected symbol", e.Line, e.Col)}
	}

	// Check if it's a syntax-rules form
	if srExpr, ok := e.Elems[2].(*ListExpr); ok && len(srExpr.Elems) >= 2 {
		if srHead, ok := srExpr.Elems[0].(*SymbolExpr); ok && srHead.Name == "syntax-rules" {
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
	}

	// Otherwise, evaluate as expression (supports lambda transformers for syntax-case)
	val, err := evalExpr(e.Elems[2], env)
	if err != nil {
		return nil, err
	}
	if lambda, ok := val.(*LambdaVal); ok {
		transformer := &MacroTransformerVal{
			Name:   nameSym.Name,
			Proc:   lambda,
			DefEnv: env,
		}
		env.set(nameSym.Name, transformer)
		return &VoidVal{}, nil
	}
	return nil, &EvalError{Message: fmt.Sprintf("%d:%d: define-syntax: transformer must be syntax-rules or procedure", e.Line, e.Col)}
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

// instantiateSyntaxCaseTemplate replaces pattern variables (bound as SyntaxVal/SyntaxListVal
// in env) within a template Expr. Non-pattern-variable symbols are left as-is.
func instantiateSyntaxCaseTemplate(tmpl Expr, env *Env) Expr {
	switch t := tmpl.(type) {
	case *SymbolExpr:
		if val, ok := env.get(t.Name); ok {
			if sv, ok := val.(*SyntaxVal); ok {
				return sv.Expr
			}
		}
		return t
	case *ListExpr:
		var newElems []Expr
		for i := 0; i < len(t.Elems); i++ {
			// Check for ellipsis
			if i+1 < len(t.Elems) {
				if sym, ok := t.Elems[i+1].(*SymbolExpr); ok && sym.Name == "..." {
					listVars := findSyntaxCaseListVars(t.Elems[i], env)
					if len(listVars) > 0 {
						firstListVal, _ := env.get(listVars[0])
						firstList := firstListVal.(*SyntaxListVal)
						count := len(firstList.Elems)
						for j := 0; j < count; j++ {
							subEnv := newEnv(env)
							for _, lv := range listVars {
								lvVal, _ := env.get(lv)
								sl := lvVal.(*SyntaxListVal)
								subEnv.set(lv, sl.Elems[j])
							}
							newElems = append(newElems, instantiateSyntaxCaseTemplate(t.Elems[i], subEnv))
						}
					}
					i++ // skip ...
					continue
				}
			}
			newElems = append(newElems, instantiateSyntaxCaseTemplate(t.Elems[i], env))
		}
		return &ListExpr{Elems: newElems, Line: t.Line, Col: t.Col}
	default:
		return tmpl
	}
}

func findSyntaxCaseListVars(tmpl Expr, env *Env) []string {
	var vars []string
	findSyntaxCaseListVarsHelper(tmpl, env, &vars)
	return vars
}

func findSyntaxCaseListVarsHelper(tmpl Expr, env *Env, vars *[]string) {
	switch t := tmpl.(type) {
	case *SymbolExpr:
		if val, ok := env.get(t.Name); ok {
			if _, ok := val.(*SyntaxListVal); ok {
				*vars = append(*vars, t.Name)
			}
		}
	case *ListExpr:
		for _, e := range t.Elems {
			findSyntaxCaseListVarsHelper(e, env, vars)
		}
	}
}

// syntaxToDatum converts a parsed Expr to a Scheme Value.
func syntaxToDatum(e Expr) (Value, error) {
	switch ex := e.(type) {
	case *NumberExpr:
		return &IntVal{Val: ex.Val}, nil
	case *FloatExpr:
		return &FloatVal{Val: ex.Val}, nil
	case *RationalExpr:
		return makeRat(ex.Num, ex.Den), nil
	case *StringExpr:
		return &StringVal{Val: ex.Val}, nil
	case *BoolExpr:
		return &BoolVal{Val: ex.Val}, nil
	case *CharExpr:
		return &CharVal{Val: ex.Val}, nil
	case *SymbolExpr:
		return &SymbolVal{Val: ex.Name}, nil
	case *ListExpr:
		var result Value = &NilVal{}
		for i := len(ex.Elems) - 1; i >= 0; i-- {
			elem, err := syntaxToDatum(ex.Elems[i])
			if err != nil {
				return nil, err
			}
			result = &PairVal{Car: elem, Cdr: result}
		}
		return result, nil
	case *EnvRefExpr:
		return &SymbolVal{Val: ex.Name}, nil
	default:
		return &VoidVal{}, nil
	}
}

// datumToExpr converts a Scheme Value to a parsed Expr.
func datumToExpr(v Value) Expr {
	switch val := v.(type) {
	case *IntVal:
		return &NumberExpr{Val: val.Val}
	case *FloatVal:
		return &FloatExpr{Val: val.Val}
	case *BoolVal:
		return &BoolExpr{Val: val.Val}
	case *StringVal:
		return &StringExpr{Val: val.Val}
	case *CharVal:
		return &CharExpr{Val: val.Val}
	case *SymbolVal:
		return &SymbolExpr{Name: val.Val}
	case *PairVal:
		var elems []Expr
		cur := Value(val)
		for {
			if p, ok := cur.(*PairVal); ok {
				elems = append(elems, datumToExpr(p.Car))
				cur = p.Cdr
			} else {
				break
			}
		}
		return &ListExpr{Elems: elems}
	case *NilVal:
		return &ListExpr{Elems: nil}
	default:
		return &SymbolExpr{Name: "#<unknown>"}
	}
}
