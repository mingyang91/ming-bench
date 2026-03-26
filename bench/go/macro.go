package ming

import (
	"fmt"
	"sync/atomic"
)

// SyntaxRules represents a macro defined by syntax-rules.
type SyntaxRules struct {
	Literals []string
	Rules    []SyntaxRule
	DefEnv   *Env // environment at definition site (for hygiene)
}

// SyntaxRule is a single (pattern template) clause.
type SyntaxRule struct {
	Pattern  *Expr
	Template *Expr
}

var gensymCounter uint64

func gensym(base string) string {
	n := atomic.AddUint64(&gensymCounter, 1)
	return fmt.Sprintf("##%s.%d", base, n)
}

// evalDefineSyntax handles (define-syntax name (syntax-rules (literals...) clauses...))
func evalDefineSyntax(expr *Expr, env *Env) (*Value, error) {
	if len(expr.List) != 3 {
		return nil, fmt.Errorf("%d:%d: define-syntax: expected 2 arguments", expr.Line, expr.Col)
	}
	nameExpr := expr.List[1]
	if nameExpr.Type != ExprSymbol {
		return nil, fmt.Errorf("%d:%d: define-syntax: expected symbol", nameExpr.Line, nameExpr.Col)
	}
	srExpr := expr.List[2]
	if srExpr.Type != ExprList || len(srExpr.List) < 2 || srExpr.List[0].Type != ExprSymbol || srExpr.List[0].StrVal != "syntax-rules" {
		return nil, fmt.Errorf("%d:%d: define-syntax: expected syntax-rules", srExpr.Line, srExpr.Col)
	}

	// Parse literals
	litExpr := srExpr.List[1]
	if litExpr.Type != ExprList {
		return nil, fmt.Errorf("%d:%d: syntax-rules: expected literal list", litExpr.Line, litExpr.Col)
	}
	var literals []string
	for _, l := range litExpr.List {
		if l.Type != ExprSymbol {
			return nil, fmt.Errorf("%d:%d: syntax-rules: literal must be a symbol", l.Line, l.Col)
		}
		literals = append(literals, l.StrVal)
	}

	// Parse rules
	var rules []SyntaxRule
	for _, clause := range srExpr.List[2:] {
		if clause.Type != ExprList || len(clause.List) != 2 {
			return nil, fmt.Errorf("%d:%d: syntax-rules: bad rule", clause.Line, clause.Col)
		}
		rules = append(rules, SyntaxRule{Pattern: clause.List[0], Template: clause.List[1]})
	}

	sr := &SyntaxRules{Literals: literals, Rules: rules, DefEnv: env}
	env.Set(nameExpr.StrVal, &Value{Type: TypeSyntax, Syntax: sr})
	return Void, nil
}

// expandMacro tries to expand a macro application. Returns the expanded Expr or nil if no match.
func expandMacro(sr *SyntaxRules, expr *Expr, useEnv *Env) (*Expr, error) {
	for _, rule := range sr.Rules {
		bindings := make(map[string]interface{}) // string -> *Expr or []*Expr (for ellipsis)
		if matchPattern(rule.Pattern, expr, sr.Literals, bindings) {
			patVars := collectPatternVars(rule.Pattern, sr.Literals)
			bindingIds, freeRefIds := collectHygienicIds(rule.Template, patVars, sr.DefEnv)
			// Generate fresh names for hygienic renaming
			renames := make(map[string]string)
			for id := range bindingIds {
				renames[id] = gensym(id)
			}
			for id := range freeRefIds {
				if !bindingIds[id] {
					renames[id] = gensym(id)
				}
			}
			expanded := instantiate(rule.Template, bindings, renames, patVars)
			return expanded, nil
		}
	}
	return nil, fmt.Errorf("%d:%d: no matching syntax-rules pattern", expr.Line, expr.Col)
}

// matchPattern matches expr against pattern, filling bindings.
// Pattern is the (name args...) list from syntax-rules clause.
func matchPattern(pattern, expr *Expr, literals []string, bindings map[string]interface{}) bool {
	if pattern.Type == ExprSymbol {
		name := pattern.StrVal
		if name == "_" {
			return true
		}
		for _, lit := range literals {
			if name == lit {
				// Must match literally
				return expr.Type == ExprSymbol && expr.StrVal == name
			}
		}
		// Pattern variable - bind it
		bindings[name] = expr
		return true
	}
	if pattern.Type != ExprList || expr.Type != ExprList {
		// For non-symbol atoms, check equality
		if pattern.Type == ExprInt && expr.Type == ExprInt {
			return pattern.IntVal == expr.IntVal
		}
		if pattern.Type == ExprBool && expr.Type == ExprBool {
			return pattern.BoolVal == expr.BoolVal
		}
		if pattern.Type == ExprString && expr.Type == ExprString {
			return pattern.StrVal == expr.StrVal
		}
		return false
	}
	// Both are lists - check for ellipsis
	pats := pattern.List
	exprs := expr.List
	return matchListPattern(pats, exprs, literals, bindings)
}

func matchListPattern(pats []*Expr, exprs []*Expr, literals []string, bindings map[string]interface{}) bool {
	// Check if there's an ellipsis
	ellipsisIdx := -1
	for i, p := range pats {
		if p.Type == ExprSymbol && p.StrVal == "..." {
			ellipsisIdx = i
			break
		}
	}

	if ellipsisIdx == -1 {
		// No ellipsis - exact length match
		if len(pats) != len(exprs) {
			return false
		}
		for i := range pats {
			if !matchPattern(pats[i], exprs[i], literals, bindings) {
				return false
			}
		}
		return true
	}

	// Has ellipsis: pattern is [..., repeatedPat, ..., rest...]
	// The element before ... is the repeated pattern
	if ellipsisIdx < 1 {
		return false
	}
	repeatedPat := pats[ellipsisIdx-1]
	beforeCount := ellipsisIdx - 1
	afterCount := len(pats) - ellipsisIdx - 1

	if len(exprs) < beforeCount+afterCount {
		return false
	}

	// Match elements before the repeated pattern
	for i := 0; i < beforeCount; i++ {
		if !matchPattern(pats[i], exprs[i], literals, bindings) {
			return false
		}
	}

	// Match the repeated elements
	repeatCount := len(exprs) - beforeCount - afterCount
	// Collect pattern variables from the repeated pattern
	repVars := collectPatternVars(repeatedPat, literals)
	// Initialize list bindings
	for v := range repVars {
		bindings[v] = []*Expr{}
	}
	for i := 0; i < repeatCount; i++ {
		subBindings := make(map[string]interface{})
		if !matchPattern(repeatedPat, exprs[beforeCount+i], literals, subBindings) {
			return false
		}
		for v := range repVars {
			if sub, ok := subBindings[v]; ok {
				bindings[v] = append(bindings[v].([]*Expr), sub.(*Expr))
			}
		}
	}

	// Match elements after the ellipsis
	for i := 0; i < afterCount; i++ {
		if !matchPattern(pats[ellipsisIdx+1+i], exprs[len(exprs)-afterCount+i], literals, bindings) {
			return false
		}
	}

	return true
}

// collectPatternVars returns the set of pattern variable names in a pattern.
func collectPatternVars(pat *Expr, literals []string) map[string]bool {
	vars := make(map[string]bool)
	collectPatVarsHelper(pat, literals, vars)
	return vars
}

func collectPatVarsHelper(pat *Expr, literals []string, vars map[string]bool) {
	if pat.Type == ExprSymbol {
		name := pat.StrVal
		if name == "_" || name == "..." {
			return
		}
		for _, lit := range literals {
			if name == lit {
				return
			}
		}
		vars[name] = true
		return
	}
	if pat.Type == ExprList {
		for _, sub := range pat.List {
			collectPatVarsHelper(sub, literals, vars)
		}
	}
}

// specialForms are keywords handled by the evaluator's switch statement.
// These must never be renamed by hygiene.
var specialForms = map[string]bool{
	"and": true, "or": true, "define": true, "if": true, "quote": true,
	"lambda": true, "let": true, "begin": true, "cond": true, "set!": true,
	"define-syntax": true, "let*": true, "letrec": true,
}

// collectHygienicIds returns two sets of identifiers from the template:
// 1. binding ids: identifiers in binding positions (let/lambda) that need renaming to prevent capture
// 2. free ref ids: free references to definition-site vars that need renaming to preserve def-site binding
// Special forms and pattern variables are excluded from both.
func collectHygienicIds(tmpl *Expr, patVars map[string]bool, defEnv *Env) (bindingIds, freeRefIds map[string]bool) {
	bindingIds = make(map[string]bool)
	freeRefIds = make(map[string]bool)
	collectAllIds(tmpl, patVars, defEnv, bindingIds, freeRefIds)
	return
}

func collectAllIds(tmpl *Expr, patVars map[string]bool, defEnv *Env, bindingIds, freeRefIds map[string]bool) {
	if tmpl.Type == ExprSymbol {
		name := tmpl.StrVal
		if name == "..." || patVars[name] || specialForms[name] {
			return
		}
		// Check if it's bound at definition site — needs protection from shadowing
		if _, ok := defEnv.Get(name); ok {
			freeRefIds[name] = true
		}
		return
	}
	if tmpl.Type != ExprList || len(tmpl.List) == 0 {
		return
	}
	head := tmpl.List[0]
	if head.Type == ExprSymbol {
		switch head.StrVal {
		case "let", "let*", "letrec":
			if len(tmpl.List) >= 3 && tmpl.List[1].Type == ExprList {
				for _, binding := range tmpl.List[1].List {
					if binding.Type == ExprList && len(binding.List) >= 1 && binding.List[0].Type == ExprSymbol {
						name := binding.List[0].StrVal
						if !patVars[name] && !specialForms[name] {
							bindingIds[name] = true
						}
					}
					if binding.Type == ExprList {
						for _, sub := range binding.List[1:] {
							collectAllIds(sub, patVars, defEnv, bindingIds, freeRefIds)
						}
					}
				}
				for _, body := range tmpl.List[2:] {
					collectAllIds(body, patVars, defEnv, bindingIds, freeRefIds)
				}
				return
			}
		case "lambda":
			if len(tmpl.List) >= 3 && tmpl.List[1].Type == ExprList {
				for _, p := range tmpl.List[1].List {
					if p.Type == ExprSymbol && !patVars[p.StrVal] && !specialForms[p.StrVal] {
						bindingIds[p.StrVal] = true
					}
				}
				for _, body := range tmpl.List[2:] {
					collectAllIds(body, patVars, defEnv, bindingIds, freeRefIds)
				}
				return
			}
		}
	}
	for _, sub := range tmpl.List {
		collectAllIds(sub, patVars, defEnv, bindingIds, freeRefIds)
	}
}

// instantiate builds an Expr from a template, substituting bindings and applying renames.
func instantiate(tmpl *Expr, bindings map[string]interface{}, renames map[string]string, patVars map[string]bool) *Expr {
	if tmpl.Type == ExprSymbol {
		name := tmpl.StrVal
		if patVars[name] {
			if val, ok := bindings[name]; ok {
				if e, ok := val.(*Expr); ok {
					return e
				}
			}
			return tmpl
		}
		if newName, ok := renames[name]; ok {
			return &Expr{Type: ExprSymbol, StrVal: newName, Line: tmpl.Line, Col: tmpl.Col}
		}
		return tmpl
	}
	if tmpl.Type != ExprList {
		return tmpl
	}

	// Handle ellipsis in template
	var result []*Expr
	for i := 0; i < len(tmpl.List); i++ {
		// Check if next element is ...
		if i+1 < len(tmpl.List) && tmpl.List[i+1].Type == ExprSymbol && tmpl.List[i+1].StrVal == "..." {
			// Expand the repeated part
			subTmpl := tmpl.List[i]
			// Find an ellipsis-bound variable to determine count
			ellipsisVars := findEllipsisVars(subTmpl, patVars, bindings)
			count := 0
			for _, v := range ellipsisVars {
				if arr, ok := bindings[v].([]*Expr); ok {
					count = len(arr)
					break
				}
			}
			for j := 0; j < count; j++ {
				// Create per-iteration bindings
				iterBindings := make(map[string]interface{})
				for k, v := range bindings {
					iterBindings[k] = v
				}
				for _, v := range ellipsisVars {
					if arr, ok := bindings[v].([]*Expr); ok && j < len(arr) {
						iterBindings[v] = arr[j]
					}
				}
				result = append(result, instantiate(subTmpl, iterBindings, renames, patVars))
			}
			i++ // skip the ...
		} else {
			result = append(result, instantiate(tmpl.List[i], bindings, renames, patVars))
		}
	}
	return &Expr{Type: ExprList, List: result, Line: tmpl.Line, Col: tmpl.Col}
}

// findEllipsisVars returns pattern vars used in expr that have list bindings.
func findEllipsisVars(expr *Expr, patVars map[string]bool, bindings map[string]interface{}) []string {
	var result []string
	findEllipsisVarsHelper(expr, patVars, bindings, &result)
	return result
}

func findEllipsisVarsHelper(expr *Expr, patVars map[string]bool, bindings map[string]interface{}, result *[]string) {
	if expr.Type == ExprSymbol && patVars[expr.StrVal] {
		if _, ok := bindings[expr.StrVal].([]*Expr); ok {
			*result = append(*result, expr.StrVal)
		}
		return
	}
	if expr.Type == ExprList {
		for _, sub := range expr.List {
			findEllipsisVarsHelper(sub, patVars, bindings, result)
		}
	}
}
