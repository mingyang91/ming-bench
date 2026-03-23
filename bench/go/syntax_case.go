package ming

import "fmt"

// applySyntaxTransformer calls a lambda-based macro transformer with the input form as a syntax object.
func (ip *interp) applySyntaxTransformer(macro *value, inputExpr *expr) (*expr, error) {
	transformer := macro.macroTransformer
	if transformer.typ != valLambda || len(transformer.params) != 1 {
		return nil, &EvalError{Message: "syntax-case transformer must be a single-argument lambda"}
	}

	stxObj := &value{typ: valSyntax, syntaxExpr: inputExpr}
	localEnv := newEnv(transformer.closure)
	localEnv.set(transformer.params[0], stxObj)

	// Evaluate transformer body
	var result *value
	var err error
	for _, bodyExpr := range transformer.body {
		result, err = ip.eval(bodyExpr, localEnv)
		if err != nil {
			return nil, err
		}
	}

	if result == nil || result.typ != valSyntax {
		return nil, &EvalError{Message: "syntax-case transformer must return a syntax object"}
	}
	return result.syntaxExpr, nil
}

// evalSyntaxCase handles (syntax-case expr (literals) clause ...)
// Each clause is (pattern template) or (pattern fender template)
func (ip *interp) evalSyntaxCase(e *expr, envir *env) (*value, error) {
	if len(e.items) < 4 {
		return nil, &EvalError{Message: fmt.Sprintf("%d:%d: syntax-case: bad syntax", e.line, e.col)}
	}

	// Evaluate the expression to get a syntax object
	stxVal, err := ip.eval(e.items[1], envir)
	if err != nil {
		return nil, err
	}
	if stxVal.typ != valSyntax {
		return nil, &EvalError{Message: fmt.Sprintf("%d:%d: syntax-case: expected syntax object", e.line, e.col)}
	}
	inputExpr := stxVal.syntaxExpr

	// Parse literals
	literals := make(map[string]bool)
	if e.items[2].kind == "list" {
		for _, l := range e.items[2].items {
			if l.kind == "symbol" {
				literals[l.sval] = true
			}
		}
	}

	// Try each clause
	for _, clause := range e.items[3:] {
		if clause.kind != "list" || len(clause.items) < 2 {
			return nil, &EvalError{Message: fmt.Sprintf("%d:%d: syntax-case: bad clause", e.line, e.col)}
		}

		pattern := clause.items[0]
		var fender, template *expr
		if len(clause.items) == 2 {
			template = clause.items[1]
		} else if len(clause.items) == 3 {
			fender = clause.items[1]
			template = clause.items[2]
		}

		// Match pattern against the input expression
		bindings, ok := matchPattern(pattern, inputExpr, literals)
		if !ok {
			continue
		}

		patVars := collectPatternVars(pattern, literals)

		// Create clause environment with syntax bindings
		clauseEnv := newEnv(envir)
		clauseEnv.syntaxBindings = bindings
		clauseEnv.syntaxDefEnv = findMacroDefEnv(envir)
		clauseEnv.syntaxPatVars = patVars

		// Bind pattern variables as syntax objects in the environment
		for name, bound := range bindings.singles {
			clauseEnv.set(name, &value{typ: valSyntax, syntaxExpr: bound})
		}

		// Check fender if present
		if fender != nil {
			fVal, fErr := ip.eval(fender, clauseEnv)
			if fErr != nil {
				return nil, fErr
			}
			if !fVal.isTruthy() {
				continue
			}
		}

		// Evaluate template expression
		return ip.eval(template, clauseEnv)
	}

	return nil, &EvalError{Message: fmt.Sprintf("%d:%d: syntax-case: no matching clause", e.line, e.col)}
}

// evalSyntax handles (syntax template) — i.e., #'template
// Expands the template using current syntax-case bindings and returns a syntax object.
func (ip *interp) evalSyntax(e *expr, envir *env) (*value, error) {
	if len(e.items) != 2 {
		return nil, &EvalError{Message: fmt.Sprintf("%d:%d: syntax: bad syntax", e.line, e.col)}
	}

	template := e.items[1]

	// Find syntax bindings in the environment chain
	bindings, defEnv, patVars := findSyntaxBindings(envir)
	if bindings == nil {
		// No syntax-case context — just wrap the template as-is
		return &value{typ: valSyntax, syntaxExpr: template}, nil
	}

	// Compute introduced bindings for hygiene
	introduced := collectIntroducedBindings(template, patVars)
	renames := make(map[string]string)
	for name := range introduced {
		renames[name] = macroGensym(name)
	}

	expanded := expandTemplate(template, bindings, defEnv, renames)
	return &value{typ: valSyntax, syntaxExpr: expanded}, nil
}

// evalWithSyntax handles (with-syntax ((pattern expr) ...) body ...)
func (ip *interp) evalWithSyntax(e *expr, envir *env) (*value, error) {
	if len(e.items) < 3 {
		return nil, &EvalError{Message: fmt.Sprintf("%d:%d: with-syntax: bad syntax", e.line, e.col)}
	}

	bindingsList := e.items[1]
	if bindingsList.kind != "list" {
		return nil, &EvalError{Message: fmt.Sprintf("%d:%d: with-syntax: expected bindings list", e.line, e.col)}
	}

	// Start with existing syntax bindings (if any)
	existingBindings, existingDefEnv, existingPatVars := findSyntaxBindings(envir)

	newBindings := newMatchResult()
	newPatVars := make(map[string]bool)

	// Copy existing bindings
	if existingBindings != nil {
		for k, v := range existingBindings.singles {
			newBindings.singles[k] = v
		}
		for k, v := range existingBindings.ellipsis {
			newBindings.ellipsis[k] = v
		}
	}
	if existingPatVars != nil {
		for k := range existingPatVars {
			newPatVars[k] = true
		}
	}

	// Process each binding
	for _, binding := range bindingsList.items {
		if binding.kind != "list" || len(binding.items) != 2 {
			return nil, &EvalError{Message: fmt.Sprintf("%d:%d: with-syntax: bad binding", e.line, e.col)}
		}

		pattern := binding.items[0]
		exprVal, err := ip.eval(binding.items[1], envir)
		if err != nil {
			return nil, err
		}

		if exprVal.typ != valSyntax {
			return nil, &EvalError{Message: fmt.Sprintf("%d:%d: with-syntax: expected syntax object", e.line, e.col)}
		}

		// Simple case: pattern is just a symbol
		if pattern.kind == "symbol" {
			newBindings.singles[pattern.sval] = exprVal.syntaxExpr
			newPatVars[pattern.sval] = true
		} else {
			// Complex pattern — match against the syntax object's expression
			literals := make(map[string]bool)
			subBindings, ok := matchPattern(pattern, exprVal.syntaxExpr, literals)
			if !ok {
				return nil, &EvalError{Message: fmt.Sprintf("%d:%d: with-syntax: pattern match failed", e.line, e.col)}
			}
			subPatVars := collectPatternVars(pattern, literals)
			for k, v := range subBindings.singles {
				newBindings.singles[k] = v
			}
			for k, v := range subBindings.ellipsis {
				newBindings.ellipsis[k] = v
			}
			for k := range subPatVars {
				newPatVars[k] = true
			}
		}
	}

	// Create environment with merged syntax bindings
	bodyEnv := newEnv(envir)
	bodyEnv.syntaxBindings = newBindings
	bodyEnv.syntaxDefEnv = existingDefEnv
	bodyEnv.syntaxPatVars = newPatVars

	// Bind pattern variables as syntax objects
	for name, bound := range newBindings.singles {
		bodyEnv.set(name, &value{typ: valSyntax, syntaxExpr: bound})
	}

	// Evaluate body expressions
	var result *value
	for _, bodyExpr := range e.items[2:] {
		var err error
		result, err = ip.eval(bodyExpr, bodyEnv)
		if err != nil {
			return nil, err
		}
	}
	return result, nil
}

// findSyntaxBindings walks up the environment chain to find syntax-case bindings.
func findSyntaxBindings(envir *env) (*matchResult, *env, map[string]bool) {
	for e := envir; e != nil; e = e.parent {
		if e.syntaxBindings != nil {
			return e.syntaxBindings, e.syntaxDefEnv, e.syntaxPatVars
		}
	}
	return nil, nil, nil
}

// findMacroDefEnv finds the macro definition env from the environment chain.
func findMacroDefEnv(envir *env) *env {
	// Walk up to find the env that was the macro's definition env
	// This is set when the transformer lambda's closure was created
	for e := envir; e != nil; e = e.parent {
		if e.syntaxDefEnv != nil {
			return e.syntaxDefEnv
		}
	}
	return nil
}

// valueToExpr converts a Scheme value to an expression for use with datum->syntax.
func valueToExpr(v *value) *expr {
	switch v.typ {
	case valInt:
		return &expr{kind: "int", ival: v.ival}
	case valFloat:
		return &expr{kind: "float", fval: v.fval}
	case valBool:
		return &expr{kind: "bool", bval: v.bval}
	case valString:
		return &expr{kind: "string", sval: v.sval}
	case valSymbol:
		return &expr{kind: "symbol", sval: v.sval}
	case valChar:
		return &expr{kind: "char", sval: string(v.cval)}
	case valNil:
		return &expr{kind: "list", items: nil}
	case valPair:
		return pairToExpr(v)
	case valSyntax:
		return v.syntaxExpr
	default:
		return &expr{kind: "symbol", sval: fmt.Sprintf("#<datum:%d>", v.typ)}
	}
}

func pairToExpr(v *value) *expr {
	var items []*expr
	cur := v
	for cur.typ == valPair {
		items = append(items, valueToExpr(cur.car))
		cur = cur.cdr
	}
	if cur.typ != valNil {
		// Improper list
		items = append(items, valueToExpr(cur))
		return &expr{kind: "list", items: items, dotted: true}
	}
	return &expr{kind: "list", items: items}
}

// exprToValue converts an expression to a Scheme value for use with syntax->datum.
func exprToValue(e *expr) *value {
	switch e.kind {
	case "int":
		return intVal(e.ival)
	case "float":
		return floatVal(e.fval)
	case "rational":
		return ratVal(e.ival, e.ival2)
	case "bool":
		return boolVal(e.bval)
	case "string":
		return strVal(e.sval)
	case "symbol":
		return symVal(e.sval)
	case "char":
		runes := []rune(e.sval)
		if len(runes) > 0 {
			return charVal(runes[0])
		}
		return charVal(0)
	case "list":
		if len(e.items) == 0 {
			return nilVal
		}
		if e.dotted {
			result := exprToValue(e.items[len(e.items)-1])
			for i := len(e.items) - 2; i >= 0; i-- {
				result = &value{typ: valPair, car: exprToValue(e.items[i]), cdr: result}
			}
			return result
		}
		result := nilVal
		for i := len(e.items) - 1; i >= 0; i-- {
			result = &value{typ: valPair, car: exprToValue(e.items[i]), cdr: result}
		}
		return result
	default:
		return voidVal
	}
}
