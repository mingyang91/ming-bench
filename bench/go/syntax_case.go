package ming

import "fmt"

// applySyntaxCaseTransformer calls a syntax-case macro transformer with the input expression.
func applySyntaxCaseTransformer(transformer *Value, input *Expr, useEnv *Env) (*Expr, error) {
	stxVal := &Value{Type: TypeSyntax, SyntaxExpr: input}

	// Set up lambda call environment
	callEnv := NewEnv(transformer.Closure)
	if len(transformer.Params) > 0 {
		callEnv.Set(transformer.Params[0], stxVal)
	}
	// Store use-site and definition-site envs for hygiene in (syntax ...) expansion
	callEnv.Set("__sc_use_env__", &Value{EnvRef: useEnv})
	callEnv.Set("__sc_def_env__", &Value{EnvRef: transformer.Closure})

	// Evaluate lambda body
	var result *Value
	for _, bodyExpr := range transformer.Body {
		var err error
		result, err = Eval(bodyExpr, callEnv)
		if err != nil {
			return nil, err
		}
	}

	if result == nil || result.Type != TypeSyntax {
		return nil, fmt.Errorf("syntax-case transformer must return a syntax object, got %v", result)
	}
	return result.SyntaxExpr, nil
}

// evalSyntaxCase evaluates (syntax-case stx-expr (literals...) clause ...).
func evalSyntaxCase(expr *Expr, env *Env) (*Value, error) {
	if len(expr.List) < 4 {
		return nil, errAt(expr, "syntax-case: too few arguments")
	}

	// Evaluate the syntax expression
	stxVal, err := Eval(expr.List[1], env)
	if err != nil {
		return nil, err
	}
	if stxVal.Type != TypeSyntax {
		return nil, errAtf(expr, "syntax-case: first argument must be a syntax object, got type %d", stxVal.Type)
	}
	stxExpr := stxVal.SyntaxExpr

	// Parse literals
	litExpr := expr.List[2]
	if litExpr.Type != ExprList {
		return nil, errAt(litExpr, "syntax-case: literals must be a list")
	}
	litSet := make(map[string]bool)
	for _, l := range litExpr.List {
		if l.Type != ExprSymbol {
			return nil, errAt(l, "syntax-case: literal must be a symbol")
		}
		litSet[l.StrVal] = true
	}

	// Try each clause
	for _, clause := range expr.List[3:] {
		if clause.Type != ExprList || len(clause.List) < 2 || len(clause.List) > 3 {
			return nil, errAt(clause, "syntax-case: invalid clause")
		}
		pattern := clause.List[0]
		var guard, body *Expr
		if len(clause.List) == 3 {
			guard = clause.List[1]
			body = clause.List[2]
		} else {
			body = clause.List[1]
		}

		// Match pattern
		bindings := matchPattern(pattern, stxExpr, litSet)
		if bindings == nil {
			continue
		}

		// Create new env with pattern variable bindings as TypeSyntax
		clauseEnv := NewEnv(env)
		for name, val := range bindings {
			switch v := val.(type) {
			case *Expr:
				clauseEnv.Set(name, &Value{Type: TypeSyntax, SyntaxExpr: v})
			case []*Expr:
				clauseEnv.Set(name, &Value{Type: TypeSyntax, SyntaxExprs: v})
			}
		}

		// Evaluate guard if present
		if guard != nil {
			guardVal, err := Eval(guard, clauseEnv)
			if err != nil {
				return nil, err
			}
			if !guardVal.IsTruthy() {
				continue
			}
		}

		// Evaluate body
		return Eval(body, clauseEnv)
	}

	return nil, errAt(expr, "syntax-case: no matching pattern")
}

// evalSyntaxTemplate evaluates (syntax template) - expands a template with pattern variable bindings.
func evalSyntaxTemplate(expr *Expr, env *Env) (*Value, error) {
	if len(expr.List) != 2 {
		return nil, errAt(expr, "syntax: expected 1 argument")
	}
	tmpl := expr.List[1]

	// Collect pattern variable bindings (TypeSyntax values) from environment
	bindings := make(map[string]interface{})
	collectSyntaxBindings(env, bindings)

	patVars := make(map[string]bool)
	for k := range bindings {
		patVars[k] = true
	}

	// Collect free symbols for hygiene
	freeSyms := collectFreeSymbols(tmpl, patVars)

	// Create hygiene map
	hygieneMap := make(map[string]string)
	for sym := range freeSyms {
		hygieneMap[sym] = gensym(sym)
	}

	// Apply hygiene: bind gensyms in use-site env to definition-site values
	if useEnvVal, ok := env.Get("__sc_use_env__"); ok && useEnvVal.EnvRef != nil {
		useEnv := useEnvVal.EnvRef
		var defEnv *Env
		if defEnvVal, ok := env.Get("__sc_def_env__"); ok && defEnvVal.EnvRef != nil {
			defEnv = defEnvVal.EnvRef
		}
		for origName, newName := range hygieneMap {
			if defEnv != nil {
				if val, ok := defEnv.Get(origName); ok {
					useEnv.Set(newName, val)
				}
			}
		}
	}

	// Expand template
	expanded := expandTemplate(tmpl, bindings, hygieneMap)
	return &Value{Type: TypeSyntax, SyntaxExpr: expanded}, nil
}

// collectSyntaxBindings gathers TypeSyntax bindings from the env chain.
func collectSyntaxBindings(env *Env, bindings map[string]interface{}) {
	for cur := env; cur != nil; cur = cur.parent {
		for name, val := range cur.bindings {
			if val.Type == TypeSyntax && len(name) > 0 && name[0] != '_' {
				if _, exists := bindings[name]; !exists {
					if val.SyntaxExprs != nil {
						bindings[name] = val.SyntaxExprs
					} else if val.SyntaxExpr != nil {
						bindings[name] = val.SyntaxExpr
					}
				}
			}
		}
	}
}

// evalWithSyntax evaluates (with-syntax ((name expr) ...) body ...).
func evalWithSyntax(expr *Expr, env *Env) (*Value, error) {
	if len(expr.List) < 3 {
		return nil, errAt(expr, "with-syntax: too few arguments")
	}
	bindingsExpr := expr.List[1]
	if bindingsExpr.Type != ExprList {
		return nil, errAt(bindingsExpr, "with-syntax: bindings must be a list")
	}

	newEnv := NewEnv(env)
	for _, binding := range bindingsExpr.List {
		if binding.Type != ExprList || len(binding.List) != 2 {
			return nil, errAt(binding, "with-syntax: each binding must be (name expr)")
		}
		name := binding.List[0]
		if name.Type != ExprSymbol {
			return nil, errAt(name, "with-syntax: binding name must be a symbol")
		}
		val, err := Eval(binding.List[1], env)
		if err != nil {
			return nil, err
		}
		if val.Type != TypeSyntax {
			return nil, errAtf(binding, "with-syntax: binding value must be a syntax object")
		}
		newEnv.Set(name.StrVal, val)
	}

	// Evaluate body
	var result *Value
	for _, bodyExpr := range expr.List[2:] {
		var err error
		result, err = Eval(bodyExpr, newEnv)
		if err != nil {
			return nil, err
		}
	}
	return result, nil
}

// valueToExpr converts a runtime Value to an Expr.
func valueToExpr(v *Value) *Expr {
	switch v.Type {
	case TypeInteger:
		return &Expr{Type: ExprInteger, IntVal: v.IntVal}
	case TypeBoolean:
		return &Expr{Type: ExprBoolean, BoolVal: v.BoolVal}
	case TypeString:
		return &Expr{Type: ExprString, StrVal: v.StrContent()}
	case TypeSymbol:
		return &Expr{Type: ExprSymbol, StrVal: v.StrVal}
	case TypeChar:
		return &Expr{Type: ExprChar, IntVal: v.IntVal}
	case TypeFloat:
		return &Expr{Type: ExprFloat, FloatVal: v.FloatVal}
	case TypeRational:
		return &Expr{Type: ExprRational, Num: v.Num, Den: v.Den}
	case TypeNull:
		return &Expr{Type: ExprList, List: nil}
	case TypePair:
		var elems []*Expr
		cur := v
		for cur.Type == TypePair {
			elems = append(elems, valueToExpr(cur.Car))
			cur = cur.Cdr
		}
		if cur.Type != TypeNull {
			// Improper list - not supported as expr, best effort
			elems = append(elems, valueToExpr(cur))
		}
		return &Expr{Type: ExprList, List: elems}
	default:
		return &Expr{Type: ExprSymbol, StrVal: fmt.Sprintf("#<value:%d>", v.Type)}
	}
}
