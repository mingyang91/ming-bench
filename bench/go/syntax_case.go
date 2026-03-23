package ming

import "fmt"

// SchemeSyntaxTransformer wraps a lambda used as a macro transformer.
type SchemeSyntaxTransformer struct {
	Transformer *Lambda
	DefEnv      *Env
}

func (s *SchemeSyntaxTransformer) String() string { return "#<syntax-transformer>" }

// syntaxCaseBindings holds pattern variable bindings from syntax-case matching.
// Stored in the environment under a special key so (syntax ...) can find them.
const syntaxCaseBindingsKey = "$$syntax-case-bindings$$"

type SyntaxBindings struct {
	Vars     map[string]SchemeValue   // single-match pattern variables
	Ellipsis map[string][]SchemeValue // ellipsis-match pattern variables
}

func (s *SyntaxBindings) String() string { return "#<syntax-bindings>" }

// evalSyntaxCase evaluates (syntax-case expr (literals...) clause ...)
// Each clause is (pattern template) or (pattern fender template).
func evalSyntaxCase(e *ListExpr, env *Env) (SchemeValue, error) {
	if len(e.Elements) < 3 {
		line, col := e.Pos()
		return nil, &EvalError{Message: fmt.Sprintf("%d:%d: syntax-case: bad syntax", line, col)}
	}

	// Evaluate the expression being matched
	stxVal, err := Eval(e.Elements[1], env)
	if err != nil {
		return nil, err
	}

	// Parse literals list
	litExpr, ok := e.Elements[2].(*ListExpr)
	if !ok {
		line, col := e.Pos()
		return nil, &EvalError{Message: fmt.Sprintf("%d:%d: syntax-case: expected literal list", line, col)}
	}
	var literals []string
	for _, l := range litExpr.Elements {
		ls, ok := l.(*SymbolExpr)
		if !ok {
			line, col := e.Pos()
			return nil, &EvalError{Message: fmt.Sprintf("%d:%d: syntax-case: literals must be identifiers", line, col)}
		}
		literals = append(literals, ls.Name)
	}

	// Try each clause
	for _, clauseExpr := range e.Elements[3:] {
		clause, ok := clauseExpr.(*ListExpr)
		if !ok || len(clause.Elements) < 2 || len(clause.Elements) > 3 {
			line, col := e.Pos()
			return nil, &EvalError{Message: fmt.Sprintf("%d:%d: syntax-case: bad clause", line, col)}
		}

		pattern := clause.Elements[0]
		var fender Expr
		var body Expr
		if len(clause.Elements) == 3 {
			fender = clause.Elements[1]
			body = clause.Elements[2]
		} else {
			body = clause.Elements[1]
		}

		bindings := &SyntaxBindings{
			Vars:     make(map[string]SchemeValue),
			Ellipsis: make(map[string][]SchemeValue),
		}

		if matchSyntaxCasePattern(pattern, stxVal, literals, bindings) {
			// If there's a fender, check it
			if fender != nil {
				clauseEnv := NewEnv(env)
				clauseEnv.Set(syntaxCaseBindingsKey, bindings)
				// Bind pattern vars for fender evaluation
				for k, v := range bindings.Vars {
					clauseEnv.Set(k, v)
				}
				fenderVal, err := Eval(fender, clauseEnv)
				if err != nil {
					return nil, err
				}
				if b, ok := fenderVal.(*SchemeBool); ok && !b.Value {
					continue // fender failed, try next clause
				}
			}

			// Create environment with bindings and evaluate body
			clauseEnv := NewEnv(env)
			clauseEnv.Set(syntaxCaseBindingsKey, bindings)
			for k, v := range bindings.Vars {
				clauseEnv.Set(k, v)
			}
			return Eval(body, clauseEnv)
		}
	}

	line, col := e.Pos()
	return nil, &EvalError{Message: fmt.Sprintf("%d:%d: syntax-case: no matching pattern", line, col)}
}

// matchSyntaxCasePattern matches a SchemeValue against a syntax-case pattern (an Expr).
func matchSyntaxCasePattern(pattern Expr, input SchemeValue, literals []string, bindings *SyntaxBindings) bool {
	switch p := pattern.(type) {
	case *SymbolExpr:
		if p.Name == "_" {
			return true // wildcard
		}
		// Check if literal
		for _, lit := range literals {
			if p.Name == lit {
				if sym, ok := input.(*SchemeSymbol); ok && sym.Name == p.Name {
					return true
				}
				return false
			}
		}
		// Pattern variable
		bindings.Vars[p.Name] = input
		return true

	case *ListExpr:
		// Check for ellipsis in pattern
		hasEllipsis := false
		ellipsisIdx := -1
		for i, elem := range p.Elements {
			if sym, ok := elem.(*SymbolExpr); ok && sym.Name == "..." {
				hasEllipsis = true
				ellipsisIdx = i
				break
			}
		}

		inputElems := valueToList(input)
		if inputElems == nil && !isEmptyValue(input) {
			return false
		}

		if !hasEllipsis {
			// Exact match
			if len(p.Elements) != len(inputElems) {
				return false
			}
			for i, pat := range p.Elements {
				if !matchSyntaxCasePattern(pat, inputElems[i], literals, bindings) {
					return false
				}
			}
			return true
		}

		// Has ellipsis
		if ellipsisIdx == 0 {
			return false
		}
		beforeEllipsis := p.Elements[:ellipsisIdx-1]
		repeatedPat := p.Elements[ellipsisIdx-1]
		afterEllipsis := p.Elements[ellipsisIdx+1:]

		if len(inputElems) < len(beforeEllipsis)+len(afterEllipsis) {
			return false
		}

		// Match prefix
		for i, pat := range beforeEllipsis {
			if !matchSyntaxCasePattern(pat, inputElems[i], literals, bindings) {
				return false
			}
		}

		// Match suffix
		suffixStart := len(inputElems) - len(afterEllipsis)
		for i, pat := range afterEllipsis {
			if !matchSyntaxCasePattern(pat, inputElems[suffixStart+i], literals, bindings) {
				return false
			}
		}

		// Match repeated elements
		repSym, ok := repeatedPat.(*SymbolExpr)
		if !ok {
			return false
		}
		repeated := inputElems[len(beforeEllipsis):suffixStart]
		bindings.Ellipsis[repSym.Name] = repeated
		return true

	case *NumberExpr:
		if n, ok := input.(*SchemeInt); ok {
			return n.Value == p.Value
		}
		return false

	case *StringExpr:
		if s, ok := input.(*SchemeString); ok {
			return s.Value == p.Value
		}
		return false

	case *BoolExpr:
		if b, ok := input.(*SchemeBool); ok {
			return b.Value == p.Value
		}
		return false

	default:
		return false
	}
}

// valueToList converts a SchemeValue to a flat slice of values.
// Works for SchemePair chains, SchemeList, and SchemeEmpty.
func valueToList(v SchemeValue) []SchemeValue {
	switch val := v.(type) {
	case *SchemeList:
		return val.Elements
	case *SchemePair:
		var result []SchemeValue
		cur := SchemeValue(val)
		for {
			switch c := cur.(type) {
			case *SchemePair:
				result = append(result, c.Car)
				cur = c.Cdr
			case *SchemeEmpty:
				return result
			default:
				// improper list - include final element
				result = append(result, cur)
				return result
			}
		}
	case *SchemeEmpty:
		return []SchemeValue{}
	default:
		return nil
	}
}

func isEmptyValue(v SchemeValue) bool {
	switch v.(type) {
	case *SchemeEmpty:
		return true
	case *SchemeList:
		return len(v.(*SchemeList).Elements) == 0
	default:
		return false
	}
}

// evalSyntaxTemplate evaluates (syntax template) using syntax-case bindings.
func evalSyntaxTemplate(e *ListExpr, env *Env) (SchemeValue, error) {
	if len(e.Elements) != 2 {
		line, col := e.Pos()
		return nil, &EvalError{Message: fmt.Sprintf("%d:%d: syntax: bad syntax", line, col)}
	}

	// Find syntax-case bindings in environment
	bindings := &SyntaxBindings{
		Vars:     make(map[string]SchemeValue),
		Ellipsis: make(map[string][]SchemeValue),
	}
	if bVal, ok := env.Get(syntaxCaseBindingsKey); ok {
		if sb, ok := bVal.(*SyntaxBindings); ok {
			bindings = sb
		}
	}

	return expandSyntaxTemplate(e.Elements[1], bindings, env)
}

// expandSyntaxTemplate expands a syntax template with bindings.
func expandSyntaxTemplate(tmpl Expr, bindings *SyntaxBindings, env *Env) (SchemeValue, error) {
	switch t := tmpl.(type) {
	case *SymbolExpr:
		if t.Name == "..." {
			return &SchemeSymbol{Name: "..."}, nil
		}
		// Check pattern variable
		if val, ok := bindings.Vars[t.Name]; ok {
			return val, nil
		}
		// Return as symbol
		return &SchemeSymbol{Name: t.Name}, nil

	case *NumberExpr:
		return &SchemeInt{Value: t.Value}, nil

	case *FloatExpr:
		return &SchemeFloat{Value: t.Value}, nil

	case *StringExpr:
		return &SchemeString{Value: t.Value, Mutable: true}, nil

	case *BoolExpr:
		return &SchemeBool{Value: t.Value}, nil

	case *CharExpr:
		return &SchemeChar{Value: t.Value}, nil

	case *ListExpr:
		return expandSyntaxListTemplate(t, bindings, env)

	default:
		return &SchemeVoid{}, nil
	}
}

func expandSyntaxListTemplate(tmpl *ListExpr, bindings *SyntaxBindings, env *Env) (SchemeValue, error) {
	// Check if this is (quote ...) form
	if len(tmpl.Elements) == 2 {
		if sym, ok := tmpl.Elements[0].(*SymbolExpr); ok && sym.Name == "quote" {
			// For quoted forms, expand template but wrap in quote
			inner, err := expandSyntaxTemplate(tmpl.Elements[1], bindings, env)
			if err != nil {
				return nil, err
			}
			// Build a list: (quote <inner>)
			return buildPairList([]SchemeValue{&SchemeSymbol{Name: "quote"}, inner}), nil
		}
	}

	var result []SchemeValue
	for i := 0; i < len(tmpl.Elements); i++ {
		// Check if next element is ...
		if i+1 < len(tmpl.Elements) {
			if sym, ok := tmpl.Elements[i+1].(*SymbolExpr); ok && sym.Name == "..." {
				// Find the ellipsis variable in this sub-template
				ellVar := findSyntaxEllipsisVar(tmpl.Elements[i], bindings)
				if ellVar != "" {
					elems := bindings.Ellipsis[ellVar]
					for _, elem := range elems {
						// Create temporary bindings with ellipsis var bound to single element
						tmpBindings := &SyntaxBindings{
							Vars:     make(map[string]SchemeValue),
							Ellipsis: bindings.Ellipsis,
						}
						for k, v := range bindings.Vars {
							tmpBindings.Vars[k] = v
						}
						tmpBindings.Vars[ellVar] = elem
						expanded, err := expandSyntaxTemplate(tmpl.Elements[i], tmpBindings, env)
						if err != nil {
							return nil, err
						}
						result = append(result, expanded)
					}
				}
				i++ // skip ...
				continue
			}
		}
		expanded, err := expandSyntaxTemplate(tmpl.Elements[i], bindings, env)
		if err != nil {
			return nil, err
		}
		result = append(result, expanded)
	}

	return buildPairList(result), nil
}

func findSyntaxEllipsisVar(tmpl Expr, bindings *SyntaxBindings) string {
	switch t := tmpl.(type) {
	case *SymbolExpr:
		if _, ok := bindings.Ellipsis[t.Name]; ok {
			return t.Name
		}
	case *ListExpr:
		for _, elem := range t.Elements {
			if v := findSyntaxEllipsisVar(elem, bindings); v != "" {
				return v
			}
		}
	}
	return ""
}

// buildPairList builds a proper list from a slice of values.
func buildPairList(elems []SchemeValue) SchemeValue {
	if len(elems) == 0 {
		return &SchemeEmpty{}
	}
	var result SchemeValue = &SchemeEmpty{}
	for i := len(elems) - 1; i >= 0; i-- {
		result = &SchemePair{Car: elems[i], Cdr: result}
	}
	return result
}

// valueToExpr converts a SchemeValue back to an Expr for evaluation.
func valueToExpr(val SchemeValue) Expr {
	switch v := val.(type) {
	case *SchemeInt:
		return &NumberExpr{Value: v.Value, Line: 0, Col: 0}
	case *SchemeFloat:
		return &FloatExpr{Value: v.Value, Line: 0, Col: 0}
	case *SchemeRational:
		return &RationalExpr{Num: v.Num, Den: v.Den, Line: 0, Col: 0}
	case *SchemeBool:
		return &BoolExpr{Value: v.Value, Line: 0, Col: 0}
	case *SchemeString:
		return &StringExpr{Value: v.Value, Line: 0, Col: 0}
	case *SchemeChar:
		return &CharExpr{Value: v.Value, Line: 0, Col: 0}
	case *SchemeSymbol:
		return &SymbolExpr{Name: v.Name, Line: 0, Col: 0}
	case *SchemePair:
		elems := valueToList(val)
		if elems == nil {
			return &ListExpr{Elements: nil, Line: 0, Col: 0}
		}
		exprs := make([]Expr, len(elems))
		for i, e := range elems {
			exprs[i] = valueToExpr(e)
		}
		return &ListExpr{Elements: exprs, Line: 0, Col: 0}
	case *SchemeList:
		exprs := make([]Expr, len(v.Elements))
		for i, e := range v.Elements {
			exprs[i] = valueToExpr(e)
		}
		return &ListExpr{Elements: exprs, Line: 0, Col: 0}
	case *SchemeEmpty:
		return &ListExpr{Elements: nil, Line: 0, Col: 0}
	default:
		return &SymbolExpr{Name: fmt.Sprintf("%v", val), Line: 0, Col: 0}
	}
}

// exprToValue converts an Expr to a SchemeValue (for passing to transformer).
func exprToValue(expr Expr) SchemeValue {
	switch e := expr.(type) {
	case *NumberExpr:
		return &SchemeInt{Value: e.Value}
	case *FloatExpr:
		return &SchemeFloat{Value: e.Value}
	case *RationalExpr:
		return &SchemeRational{Num: e.Num, Den: e.Den}
	case *BoolExpr:
		return &SchemeBool{Value: e.Value}
	case *StringExpr:
		return &SchemeString{Value: e.Value, Mutable: true}
	case *CharExpr:
		return &SchemeChar{Value: e.Value}
	case *SymbolExpr:
		return &SchemeSymbol{Name: e.Name}
	case *ListExpr:
		elems := make([]SchemeValue, len(e.Elements))
		for i, el := range e.Elements {
			elems[i] = exprToValue(el)
		}
		return buildPairList(elems)
	default:
		return &SchemeVoid{}
	}
}

// evalWithSyntax evaluates (with-syntax ((pattern expr) ...) body ...)
func evalWithSyntax(e *ListExpr, env *Env) (SchemeValue, error) {
	if len(e.Elements) < 3 {
		line, col := e.Pos()
		return nil, &EvalError{Message: fmt.Sprintf("%d:%d: with-syntax: bad syntax", line, col)}
	}

	bindingsList, ok := e.Elements[1].(*ListExpr)
	if !ok {
		line, col := e.Pos()
		return nil, &EvalError{Message: fmt.Sprintf("%d:%d: with-syntax: expected binding list", line, col)}
	}

	// Get existing bindings or create new
	bindings := &SyntaxBindings{
		Vars:     make(map[string]SchemeValue),
		Ellipsis: make(map[string][]SchemeValue),
	}
	if bVal, ok := env.Get(syntaxCaseBindingsKey); ok {
		if sb, ok := bVal.(*SyntaxBindings); ok {
			// Copy existing bindings
			for k, v := range sb.Vars {
				bindings.Vars[k] = v
			}
			for k, v := range sb.Ellipsis {
				bindings.Ellipsis[k] = v
			}
		}
	}

	wsEnv := NewEnv(env)

	for _, binding := range bindingsList.Elements {
		bExpr, ok := binding.(*ListExpr)
		if !ok || len(bExpr.Elements) != 2 {
			line, col := e.Pos()
			return nil, &EvalError{Message: fmt.Sprintf("%d:%d: with-syntax: bad binding", line, col)}
		}

		pattern := bExpr.Elements[0]
		val, err := Eval(bExpr.Elements[1], wsEnv)
		if err != nil {
			return nil, err
		}

		// Match pattern against value
		matchSyntaxCasePattern(pattern, val, nil, bindings)
	}

	wsEnv.Set(syntaxCaseBindingsKey, bindings)
	for k, v := range bindings.Vars {
		wsEnv.Set(k, v)
	}

	// Evaluate body
	var result SchemeValue = &SchemeVoid{}
	for _, bodyExpr := range e.Elements[2:] {
		var err error
		result, err = Eval(bodyExpr, wsEnv)
		if err != nil {
			return nil, err
		}
	}
	return result, nil
}

// expandSyntaxTransformer applies a syntax-case transformer macro.
func expandSyntaxTransformer(transformer *SchemeSyntaxTransformer, callExpr *ListExpr, useEnv *Env) (Expr, *Env, error) {
	// Convert the call expression to a SchemeValue (list)
	stxVal := exprToValue(callExpr)

	// Call the transformer lambda with the syntax object
	callEnv := NewEnv(transformer.Transformer.Env)
	if len(transformer.Transformer.Params) != 1 {
		line, col := callExpr.Pos()
		return nil, nil, &EvalError{Message: fmt.Sprintf("%d:%d: syntax transformer: expected 1 parameter", line, col)}
	}
	callEnv.Set(transformer.Transformer.Params[0], stxVal)

	// Evaluate body
	var result SchemeValue
	var err error
	for _, bodyExpr := range transformer.Transformer.Body {
		result, err = Eval(bodyExpr, callEnv)
		if err != nil {
			return nil, nil, err
		}
	}

	// Convert result back to Expr
	expanded := valueToExpr(result)

	return expanded, useEnv, nil
}
