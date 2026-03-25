package ming

import "fmt"

// SyntaxVal wraps an Expr as a first-class syntax object.
type SyntaxVal struct {
	Expr     Expr
	DefNames map[string]bool   // names defined at macro definition time (for hygiene)
	Bindings map[string]Value  // gensym → value, injected into env at expansion site
}

func (s *SyntaxVal) String() string { return "#<syntax>" }

// EllipsisSyntaxVal holds multiple exprs for ellipsis pattern variables.
type EllipsisSyntaxVal struct {
	Exprs []Expr
}

func (s *EllipsisSyntaxVal) String() string { return "#<syntax ...>" }

// MacroTransformerVal wraps a procedure used as a syntax transformer.
type MacroTransformerVal struct {
	Proc     Value
	DefNames map[string]bool // snapshot of env keys at definition time
}

func (m *MacroTransformerVal) String() string { return "#<macro>" }

// SyntaxCaseCtxVal holds context for the syntax form.
type SyntaxCaseCtxVal struct {
	Literals []string
	DefNames map[string]bool
}

func (s *SyntaxCaseCtxVal) String() string { return "" }

// captureEnvNames takes a snapshot of all defined names in the env chain.
func captureEnvNames(env *Env) map[string]bool {
	names := make(map[string]bool)
	for e := env; e != nil; e = e.parent {
		for k := range e.bindings {
			names[k] = true
		}
	}
	return names
}

// evalSyntaxCase implements (syntax-case stx (literal ...) clause ...).
func evalSyntaxCase(e *ListExpr, env *Env) (Value, error) {
	if len(e.Items) < 4 {
		return nil, &EvalError{Message: "syntax-case: bad syntax"}
	}

	// Evaluate stx expression
	stxVal, err := eval(e.Items[1], env)
	if err != nil {
		return nil, err
	}
	sv, ok := stxVal.(*SyntaxVal)
	if !ok {
		return nil, &EvalError{Message: "syntax-case: expected syntax object"}
	}

	// Parse literals
	litList, ok := e.Items[2].(*ListExpr)
	if !ok {
		return nil, &EvalError{Message: "syntax-case: expected literals list"}
	}
	var literals []string
	for _, l := range litList.Items {
		ls, ok := l.(*SymbolExpr)
		if !ok {
			return nil, &EvalError{Message: "syntax-case: literal must be a symbol"}
		}
		literals = append(literals, ls.Name)
	}

	stxExpr := sv.Expr

	// Try each clause
	for _, clause := range e.Items[3:] {
		cl, ok := clause.(*ListExpr)
		if !ok || len(cl.Items) < 2 || len(cl.Items) > 3 {
			return nil, &EvalError{Message: "syntax-case: bad clause"}
		}

		pattern := cl.Items[0]
		hasFender := len(cl.Items) == 3
		var fender, output Expr
		if hasFender {
			fender = cl.Items[1]
			output = cl.Items[2]
		} else {
			output = cl.Items[1]
		}

		// Pattern match
		bindings := make(map[string]*patternBinding)
		var matched bool
		if pat, ok := pattern.(*ListExpr); ok {
			if stxList, ok := stxExpr.(*ListExpr); ok {
				matched = matchPattern(pat.Items, stxList.Items, literals, bindings)
			}
		} else if sym, ok := pattern.(*SymbolExpr); ok {
			bindings[sym.Name] = &patternBinding{expr: stxExpr}
			matched = true
		}

		if !matched {
			continue
		}

		// Create new scope with pattern variable bindings as SyntaxVal
		childEnv := newEnv(env)
		childEnv.set("__sc_ctx__", &SyntaxCaseCtxVal{
			Literals: literals,
			DefNames: sv.DefNames,
		})

		for name, pb := range bindings {
			if pb.ellipsis {
				childEnv.set(name, &EllipsisSyntaxVal{Exprs: pb.exprs})
			} else {
				childEnv.set(name, &SyntaxVal{Expr: pb.expr})
			}
		}

		// Evaluate fender if present
		if hasFender {
			fenderVal, err := eval(fender, childEnv)
			if err != nil {
				return nil, err
			}
			if !isTruthy(fenderVal) {
				continue
			}
		}

		// Evaluate output expression (tail position)
		return &tailCallVal{expr: output, env: childEnv}, nil
	}

	return nil, &EvalError{Message: "syntax-case: no matching clause"}
}

// evalSyntax implements (syntax template) / #'template.
func evalSyntax(e *ListExpr, env *Env) (Value, error) {
	if len(e.Items) != 2 {
		return nil, &EvalError{Message: "syntax: requires exactly 1 argument"}
	}
	tmpl := e.Items[1]

	// If template is a single symbol bound to SyntaxVal, return it directly
	if sym, ok := tmpl.(*SymbolExpr); ok {
		if val, ok := env.get(sym.Name); ok {
			switch val.(type) {
			case *SyntaxVal:
				return val.(*SyntaxVal), nil
			case *EllipsisSyntaxVal:
				return val.(*EllipsisSyntaxVal), nil
			}
		}
	}

	// Collect pattern variables from env
	patVars := make(map[string]bool)
	collectSyntaxPatVars(env, patVars)

	// Get context (literals + DefNames)
	var literals []string
	var defNames map[string]bool
	if ctx, ok := env.get("__sc_ctx__"); ok {
		if sc, ok := ctx.(*SyntaxCaseCtxVal); ok {
			literals = sc.Literals
			defNames = sc.DefNames
		}
	}

	// Build rename map for hygiene — gensym symbols NOT in DefNames
	renameMap := make(map[string]string)
	collectSyntaxRenames(tmpl, patVars, literals, defNames, renameMap)

	// Expand template
	expanded := expandSyntaxTmpl(tmpl, patVars, env, renameMap, e.Ln, e.Cl)

	// Build injection bindings: for gensymed names that have current env values
	var bindings map[string]Value
	for origName, gensymName := range renameMap {
		if val, ok := env.get(origName); ok {
			// Skip syntax-related internal values
			switch val.(type) {
			case *SyntaxVal, *EllipsisSyntaxVal, *SyntaxCaseCtxVal:
				continue
			}
			if bindings == nil {
				bindings = make(map[string]Value)
			}
			bindings[gensymName] = val
		}
	}

	return &SyntaxVal{Expr: expanded, Bindings: bindings}, nil
}

// collectSyntaxPatVars collects names bound to SyntaxVal/EllipsisSyntaxVal in the env.
func collectSyntaxPatVars(env *Env, patVars map[string]bool) {
	for e := env; e != nil; e = e.parent {
		for name, val := range e.bindings {
			if name == "__sc_ctx__" {
				continue
			}
			switch val.(type) {
			case *SyntaxVal, *EllipsisSyntaxVal:
				patVars[name] = true
			}
		}
	}
}

// collectSyntaxRenames assigns gensyms to macro-introduced symbols.
// Uses defNames (snapshot at definition time) instead of live env.
func collectSyntaxRenames(tmpl Expr, patVars map[string]bool, literals []string, defNames map[string]bool, renameMap map[string]string) {
	switch t := tmpl.(type) {
	case *SymbolExpr:
		if t.Name == "..." || patVars[t.Name] || specialForms[t.Name] {
			return
		}
		for _, lit := range literals {
			if t.Name == lit {
				return
			}
		}
		// Only skip gensyming if the symbol was defined at macro definition time
		if defNames != nil && defNames[t.Name] {
			return
		}
		if _, ok := renameMap[t.Name]; !ok {
			renameMap[t.Name] = gensym(t.Name)
		}
	case *ListExpr:
		// Skip inside quote forms
		if len(t.Items) > 0 {
			if sym, ok := t.Items[0].(*SymbolExpr); ok && sym.Name == "quote" {
				return
			}
		}
		for _, item := range t.Items {
			collectSyntaxRenames(item, patVars, literals, defNames, renameMap)
		}
	}
}

// expandSyntaxTmpl expands a syntax template with substitution and hygiene.
func expandSyntaxTmpl(tmpl Expr, patVars map[string]bool, env *Env, renameMap map[string]string, ln, cl int) Expr {
	switch t := tmpl.(type) {
	case *SymbolExpr:
		if t.Name == "..." {
			return t
		}
		// Pattern variable?
		if patVars[t.Name] {
			if val, ok := env.get(t.Name); ok {
				if sv, ok := val.(*SyntaxVal); ok {
					return sv.Expr
				}
			}
			return &SymbolExpr{Name: t.Name, Ln: ln, Cl: cl}
		}
		// Renamed (macro-introduced)?
		if newName, ok := renameMap[t.Name]; ok {
			return &SymbolExpr{Name: newName, Ln: ln, Cl: cl}
		}
		// Resolve from env for hygiene (symbols that were in DefNames)
		if !specialForms[t.Name] {
			if val, ok := env.get(t.Name); ok {
				switch val.(type) {
				case *MacroVal, *MacroTransformerVal, *SyntaxCaseCtxVal, *SyntaxVal, *EllipsisSyntaxVal:
					return &SymbolExpr{Name: t.Name, Ln: ln, Cl: cl}
				default:
					return &ValueExpr{Val: val, Ln: ln, Cl: cl}
				}
			}
		}
		return &SymbolExpr{Name: t.Name, Ln: ln, Cl: cl}
	case *ListExpr:
		// Handle quote specially - don't modify inside quotes
		if len(t.Items) > 0 {
			if sym, ok := t.Items[0].(*SymbolExpr); ok && sym.Name == "quote" {
				return tmpl
			}
		}
		var items []Expr
		for i := 0; i < len(t.Items); i++ {
			if i+1 < len(t.Items) {
				if sym, ok := t.Items[i+1].(*SymbolExpr); ok && sym.Name == "..." {
					items = append(items, expandSyntaxEllipsis(t.Items[i], patVars, env, renameMap, ln, cl)...)
					i++ // skip ...
					continue
				}
			}
			items = append(items, expandSyntaxTmpl(t.Items[i], patVars, env, renameMap, ln, cl))
		}
		return &ListExpr{Items: items, Ln: ln, Cl: cl}
	default:
		return tmpl
	}
}

// expandSyntaxEllipsis expands a template element followed by ...
func expandSyntaxEllipsis(tmpl Expr, patVars map[string]bool, env *Env, renameMap map[string]string, ln, cl int) []Expr {
	var ellipsisVar string
	var ellipsisExprs []Expr
	findSyntaxEllipsisVar(tmpl, env, &ellipsisVar, &ellipsisExprs)

	if ellipsisVar == "" {
		return nil
	}

	var result []Expr
	for _, expr := range ellipsisExprs {
		iterEnv := newEnv(env)
		iterEnv.set(ellipsisVar, &SyntaxVal{Expr: expr})
		result = append(result, expandSyntaxTmpl(tmpl, patVars, iterEnv, renameMap, ln, cl))
	}
	return result
}

// findSyntaxEllipsisVar finds the ellipsis pattern variable in a template.
func findSyntaxEllipsisVar(tmpl Expr, env *Env, name *string, exprs *[]Expr) {
	switch t := tmpl.(type) {
	case *SymbolExpr:
		if val, ok := env.get(t.Name); ok {
			if esv, ok := val.(*EllipsisSyntaxVal); ok {
				*name = t.Name
				*exprs = esv.Exprs
			}
		}
	case *ListExpr:
		for _, item := range t.Items {
			findSyntaxEllipsisVar(item, env, name, exprs)
			if *name != "" {
				return
			}
		}
	}
}

// evalWithSyntax implements (with-syntax ((pattern expr) ...) body ...).
func evalWithSyntax(e *ListExpr, env *Env) (Value, error) {
	if len(e.Items) < 3 {
		return nil, &EvalError{Message: "with-syntax: bad syntax"}
	}
	bindingsList, ok := e.Items[1].(*ListExpr)
	if !ok {
		return nil, &EvalError{Message: "with-syntax: expected bindings list"}
	}

	childEnv := newEnv(env)
	for _, binding := range bindingsList.Items {
		bl, ok := binding.(*ListExpr)
		if !ok || len(bl.Items) != 2 {
			return nil, &EvalError{Message: "with-syntax: bad binding"}
		}
		nameSym, ok := bl.Items[0].(*SymbolExpr)
		if !ok {
			return nil, &EvalError{Message: "with-syntax: expected symbol in binding"}
		}
		val, err := eval(bl.Items[1], env)
		if err != nil {
			return nil, err
		}
		if sv, ok := val.(*SyntaxVal); ok {
			childEnv.set(nameSym.Name, sv)
		} else {
			return nil, &EvalError{Message: fmt.Sprintf("with-syntax: expected syntax object, got %T", val)}
		}
	}

	// Evaluate body
	for _, bodyExpr := range e.Items[2 : len(e.Items)-1] {
		_, err := eval(bodyExpr, childEnv)
		if err != nil {
			return nil, err
		}
	}
	return &tailCallVal{expr: e.Items[len(e.Items)-1], env: childEnv}, nil
}

// builtinSyntaxToDatum converts a syntax object to a datum.
func builtinSyntaxToDatum(args []Value) (Value, error) {
	if len(args) != 1 {
		return nil, &EvalError{Message: "syntax->datum: requires exactly 1 argument"}
	}
	sv, ok := args[0].(*SyntaxVal)
	if !ok {
		return nil, &EvalError{Message: "syntax->datum: expected syntax object"}
	}
	return quoteExpr(sv.Expr), nil
}

// builtinDatumToSyntax converts a datum to a syntax object.
func builtinDatumToSyntax(args []Value) (Value, error) {
	if len(args) != 2 {
		return nil, &EvalError{Message: "datum->syntax: requires exactly 2 arguments"}
	}
	datum := args[1]
	expr := valueToExpr(datum)
	return &SyntaxVal{Expr: expr}, nil
}

// valueToExpr converts a Value back to an Expr.
func valueToExpr(v Value) Expr {
	switch val := v.(type) {
	case *IntVal:
		return &NumberExpr{Val: val.Val}
	case *FloatVal:
		return &FloatExpr{Val: val.Val}
	case *BoolVal:
		return &BoolExpr{Val: val.Val}
	case *StringVal:
		return &StringExpr{Val: val.Val}
	case *SymbolVal:
		return &SymbolExpr{Name: val.Name}
	case *CharVal:
		return &CharExpr{Val: val.Val}
	case *NilVal:
		return &ListExpr{}
	case *PairVal:
		var items []Expr
		cur := Value(val)
		for {
			p, ok := cur.(*PairVal)
			if !ok {
				break
			}
			items = append(items, valueToExpr(p.Car))
			cur = p.Cdr
		}
		if _, ok := cur.(*NilVal); !ok {
			items = append(items, valueToExpr(cur))
		}
		return &ListExpr{Items: items}
	default:
		return &ValueExpr{Val: v}
	}
}
