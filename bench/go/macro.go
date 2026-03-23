package ming

import (
	"fmt"
	"sync/atomic"
)

// SchemeMacro represents a syntax-rules macro.
type SchemeMacro struct {
	Name     string
	Literals []string
	Rules    []macroRule
	DefEnv   *Env
}

func (m *SchemeMacro) String() string {
	return fmt.Sprintf("#<macro %s>", m.Name)
}

type macroRule struct {
	Pattern  []Expr // pattern elements (excluding macro name)
	Template Expr
}

var gensymCounter uint64

func gensym(base string) string {
	id := atomic.AddUint64(&gensymCounter, 1)
	return fmt.Sprintf("%s$$%d", base, id)
}

// evalDefineSyntax handles (define-syntax name (syntax-rules ...)) or (define-syntax name (lambda (stx) ...))
func evalDefineSyntax(e *ListExpr, env *Env) (SchemeValue, error) {
	if len(e.Elements) != 3 {
		line, col := e.Pos()
		return nil, &EvalError{Message: fmt.Sprintf("%d:%d: define-syntax: bad syntax", line, col)}
	}

	nameSym, ok := e.Elements[1].(*SymbolExpr)
	if !ok {
		line, col := e.Pos()
		return nil, &EvalError{Message: fmt.Sprintf("%d:%d: define-syntax: expected identifier", line, col)}
	}

	srExpr, ok := e.Elements[2].(*ListExpr)
	if !ok || len(srExpr.Elements) < 2 {
		line, col := e.Pos()
		return nil, &EvalError{Message: fmt.Sprintf("%d:%d: define-syntax: expected syntax-rules or lambda", line, col)}
	}

	srSym, ok := srExpr.Elements[0].(*SymbolExpr)
	if !ok {
		line, col := e.Pos()
		return nil, &EvalError{Message: fmt.Sprintf("%d:%d: define-syntax: expected syntax-rules or lambda", line, col)}
	}

	// Handle lambda transformer
	if srSym.Name == "lambda" {
		val, err := Eval(srExpr, env)
		if err != nil {
			return nil, err
		}
		lam, ok := val.(*Lambda)
		if !ok {
			line, col := e.Pos()
			return nil, &EvalError{Message: fmt.Sprintf("%d:%d: define-syntax: lambda did not produce a procedure", line, col)}
		}
		transformer := &SchemeSyntaxTransformer{Transformer: lam, DefEnv: env}
		env.Set(nameSym.Name, transformer)
		return &SchemeVoid{}, nil
	}

	if srSym.Name != "syntax-rules" {
		line, col := e.Pos()
		return nil, &EvalError{Message: fmt.Sprintf("%d:%d: define-syntax: expected syntax-rules or lambda", line, col)}
	}

	// Parse literals
	litExpr, ok := srExpr.Elements[1].(*ListExpr)
	if !ok {
		line, col := e.Pos()
		return nil, &EvalError{Message: fmt.Sprintf("%d:%d: syntax-rules: expected literal list", line, col)}
	}
	var literals []string
	for _, l := range litExpr.Elements {
		ls, ok := l.(*SymbolExpr)
		if !ok {
			line, col := e.Pos()
			return nil, &EvalError{Message: fmt.Sprintf("%d:%d: syntax-rules: literals must be identifiers", line, col)}
		}
		literals = append(literals, ls.Name)
	}

	// Parse rules
	var rules []macroRule
	for _, ruleExpr := range srExpr.Elements[2:] {
		rule, ok := ruleExpr.(*ListExpr)
		if !ok || len(rule.Elements) != 2 {
			line, col := e.Pos()
			return nil, &EvalError{Message: fmt.Sprintf("%d:%d: syntax-rules: bad rule", line, col)}
		}
		patternExpr, ok := rule.Elements[0].(*ListExpr)
		if !ok {
			line, col := e.Pos()
			return nil, &EvalError{Message: fmt.Sprintf("%d:%d: syntax-rules: pattern must be a list", line, col)}
		}
		// Skip first element of pattern (macro name placeholder)
		rules = append(rules, macroRule{
			Pattern:  patternExpr.Elements[1:],
			Template: rule.Elements[1],
		})
	}

	macro := &SchemeMacro{
		Name:     nameSym.Name,
		Literals: literals,
		Rules:    rules,
		DefEnv:   env,
	}
	env.Set(nameSym.Name, macro)
	return &SchemeVoid{}, nil
}

// expandMacro expands a macro application and returns the expanded expression
// along with an enriched environment for hygiene.
func expandMacro(macro *SchemeMacro, callExpr *ListExpr, useEnv *Env) (Expr, *Env, error) {
	inputElements := callExpr.Elements[1:] // skip macro name

	for _, rule := range macro.Rules {
		bindings := &patternBindings{
			vars:     make(map[string]Expr),
			ellipsis: make(map[string][]Expr),
		}
		if matchPattern(rule.Pattern, inputElements, macro.Literals, bindings) {
			// Collect pattern variable names
			patVars := make(map[string]bool)
			for k := range bindings.vars {
				patVars[k] = true
			}
			for k := range bindings.ellipsis {
				patVars[k] = true
			}

			// Collect template-introduced free variables
			freeVars := collectFreeVars(rule.Template, patVars)

			// Generate gensyms for non-special-form free vars
			renaming := make(map[string]string)
			for _, name := range freeVars {
				if !isSpecialForm(name) {
					renaming[name] = gensym(name)
				}
			}

			// Expand template
			expanded := expandTemplate(rule.Template, bindings, renaming, callExpr)

			// Create enriched environment with definition-site bindings for gensyms
			enrichedEnv := NewEnv(useEnv)
			for origName, gensymName := range renaming {
				if val, ok := macro.DefEnv.Get(origName); ok {
					enrichedEnv.Set(gensymName, val)
				}
			}

			return expanded, enrichedEnv, nil
		}
	}

	line, col := callExpr.Pos()
	return nil, nil, &EvalError{Message: fmt.Sprintf("%d:%d: %s: no matching pattern", line, col, macro.Name)}
}

type patternBindings struct {
	vars     map[string]Expr
	ellipsis map[string][]Expr
}

// matchPattern matches input expressions against a pattern.
func matchPattern(pattern []Expr, input []Expr, literals []string, bindings *patternBindings) bool {
	// Find if pattern has ellipsis
	ellipsisIdx := -1
	for i, p := range pattern {
		if sym, ok := p.(*SymbolExpr); ok && sym.Name == "..." {
			ellipsisIdx = i
			break
		}
	}

	if ellipsisIdx == -1 {
		// No ellipsis - exact match on count
		if len(pattern) != len(input) {
			return false
		}
		for i, p := range pattern {
			if !matchSinglePattern(p, input[i], literals, bindings) {
				return false
			}
		}
		return true
	}

	// Has ellipsis at position ellipsisIdx
	if ellipsisIdx == 0 {
		return false
	}

	beforeEllipsis := pattern[:ellipsisIdx-1]
	repeatedPat := pattern[ellipsisIdx-1]
	afterEllipsis := pattern[ellipsisIdx+1:]

	if len(input) < len(beforeEllipsis)+len(afterEllipsis) {
		return false
	}

	// Match prefix
	for i, p := range beforeEllipsis {
		if !matchSinglePattern(p, input[i], literals, bindings) {
			return false
		}
	}

	// Match suffix
	suffixStart := len(input) - len(afterEllipsis)
	for i, p := range afterEllipsis {
		if !matchSinglePattern(p, input[suffixStart+i], literals, bindings) {
			return false
		}
	}

	// Match repeated elements
	repeatedSym, ok := repeatedPat.(*SymbolExpr)
	if !ok {
		return false
	}

	repeated := input[len(beforeEllipsis):suffixStart]
	bindings.ellipsis[repeatedSym.Name] = repeated
	return true
}

func matchSinglePattern(pattern Expr, input Expr, literals []string, bindings *patternBindings) bool {
	switch p := pattern.(type) {
	case *SymbolExpr:
		// Check if it's a literal
		for _, lit := range literals {
			if p.Name == lit {
				if inSym, ok := input.(*SymbolExpr); ok && inSym.Name == p.Name {
					return true
				}
				return false
			}
		}
		// Pattern variable - bind it
		bindings.vars[p.Name] = input
		return true
	case *ListExpr:
		inList, ok := input.(*ListExpr)
		if !ok {
			return false
		}
		return matchPattern(p.Elements, inList.Elements, literals, bindings)
	default:
		return false
	}
}

var specialForms = map[string]bool{
	"if": true, "let": true, "begin": true, "set!": true, "define": true,
	"lambda": true, "cond": true, "and": true, "or": true, "quote": true,
	"define-syntax": true, "syntax-rules": true, "let*": true, "letrec": true,
	"letrec*": true, "case": true, "do": true,
	"syntax-case": true, "syntax": true, "with-syntax": true,
}

func isSpecialForm(name string) bool {
	return specialForms[name]
}

// collectFreeVars collects symbols in a template that are not pattern variables.
func collectFreeVars(tmpl Expr, patVars map[string]bool) []string {
	seen := make(map[string]bool)
	var result []string
	collectFreeVarsHelper(tmpl, patVars, seen, &result)
	return result
}

func collectFreeVarsHelper(tmpl Expr, patVars map[string]bool, seen map[string]bool, result *[]string) {
	switch t := tmpl.(type) {
	case *SymbolExpr:
		if t.Name != "..." && !patVars[t.Name] && !seen[t.Name] {
			seen[t.Name] = true
			*result = append(*result, t.Name)
		}
	case *ListExpr:
		for _, elem := range t.Elements {
			collectFreeVarsHelper(elem, patVars, seen, result)
		}
	}
}

// expandTemplate expands a template with pattern bindings and renamings.
func expandTemplate(tmpl Expr, bindings *patternBindings, renaming map[string]string, refExpr *ListExpr) Expr {
	switch t := tmpl.(type) {
	case *SymbolExpr:
		if t.Name == "..." {
			return t
		}
		// Check if it's a pattern variable
		if expr, ok := bindings.vars[t.Name]; ok {
			return expr
		}
		// Check if it's an ellipsis variable (shouldn't appear outside ... context)
		// Check if it needs renaming
		if newName, ok := renaming[t.Name]; ok {
			return &SymbolExpr{Name: newName, Line: t.Line, Col: t.Col}
		}
		return t
	case *ListExpr:
		return expandListTemplate(t, bindings, renaming, refExpr)
	default:
		return tmpl
	}
}

func expandListTemplate(tmpl *ListExpr, bindings *patternBindings, renaming map[string]string, refExpr *ListExpr) Expr {
	var expanded []Expr

	for i := 0; i < len(tmpl.Elements); i++ {
		// Check if next element is ...
		if i+1 < len(tmpl.Elements) {
			if sym, ok := tmpl.Elements[i+1].(*SymbolExpr); ok && sym.Name == "..." {
				subTmpl := tmpl.Elements[i]

				// Find which ellipsis variable is used in this sub-template
				ellipsisVar := findEllipsisVar(subTmpl, bindings)
				if ellipsisVar != "" {
					elems := bindings.ellipsis[ellipsisVar]
					for _, elem := range elems {
						tmpBindings := &patternBindings{
							vars:     make(map[string]Expr),
							ellipsis: bindings.ellipsis,
						}
						for k, v := range bindings.vars {
							tmpBindings.vars[k] = v
						}
						tmpBindings.vars[ellipsisVar] = elem
						expanded = append(expanded, expandTemplate(subTmpl, tmpBindings, renaming, refExpr))
					}
				}
				i++ // skip the ...
				continue
			}
		}
		expanded = append(expanded, expandTemplate(tmpl.Elements[i], bindings, renaming, refExpr))
	}

	return &ListExpr{Elements: expanded, Line: tmpl.Line, Col: tmpl.Col}
}

// findEllipsisVar finds which ellipsis-bound variable is referenced in a template.
func findEllipsisVar(tmpl Expr, bindings *patternBindings) string {
	switch t := tmpl.(type) {
	case *SymbolExpr:
		if _, ok := bindings.ellipsis[t.Name]; ok {
			return t.Name
		}
	case *ListExpr:
		for _, elem := range t.Elements {
			if v := findEllipsisVar(elem, bindings); v != "" {
				return v
			}
		}
	}
	return ""
}
