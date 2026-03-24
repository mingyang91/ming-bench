package ming

import (
	"fmt"
	"sync/atomic"
)

var gensymCounter uint64

func gensym(base string) string {
	n := atomic.AddUint64(&gensymCounter, 1)
	return fmt.Sprintf("%s__m%d", base, n)
}

// SyntaxVal represents a syntax-rules macro transformer.
type SyntaxVal struct {
	Name     string
	Literals map[string]bool
	Rules    []syntaxRule
	DefEnv   *Env
}

func (v *SyntaxVal) String() string { return "#<syntax>" }

type syntaxRule struct {
	Pattern  *Expr
	Template *Expr
}

type macroBindings struct {
	singles map[string]*Expr
	lists   map[string][]*Expr
}

func newMacroBindings() *macroBindings {
	return &macroBindings{
		singles: make(map[string]*Expr),
		lists:   make(map[string][]*Expr),
	}
}

var specialForms = map[string]bool{
	"define": true, "if": true, "quote": true, "lambda": true,
	"let": true, "begin": true, "cond": true, "set!": true,
	"and": true, "or": true, "define-syntax": true, "syntax-rules": true,
	"else": true,
}

// evalDefineSyntax handles (define-syntax name (syntax-rules (lits...) rules...))
func evalDefineSyntax(expr *Expr, env *Env) (Value, error) {
	if len(expr.List) != 3 {
		return nil, &EvalError{Message: "define-syntax: bad syntax"}
	}
	nameExpr := expr.List[1]
	if nameExpr.Kind != ExprSymbol {
		return nil, &EvalError{Message: "define-syntax: expected symbol"}
	}
	srExpr := expr.List[2]
	if srExpr.Kind != ExprList || len(srExpr.List) < 2 ||
		srExpr.List[0].Kind != ExprSymbol || srExpr.List[0].SVal != "syntax-rules" {
		return nil, &EvalError{Message: "define-syntax: expected syntax-rules"}
	}
	litExpr := srExpr.List[1]
	if litExpr.Kind != ExprList {
		return nil, &EvalError{Message: "syntax-rules: expected literals list"}
	}
	literals := make(map[string]bool)
	for _, l := range litExpr.List {
		if l.Kind == ExprSymbol {
			literals[l.SVal] = true
		}
	}
	var rules []syntaxRule
	for _, r := range srExpr.List[2:] {
		if r.Kind != ExprList || len(r.List) != 2 {
			return nil, &EvalError{Message: "syntax-rules: bad rule"}
		}
		rules = append(rules, syntaxRule{Pattern: r.List[0], Template: r.List[1]})
	}
	env.Set(nameExpr.SVal, &SyntaxVal{
		Name:     nameExpr.SVal,
		Literals: literals,
		Rules:    rules,
		DefEnv:   env,
	})
	return &VoidVal{}, nil
}

// expandMacro expands a macro call and evaluates the result.
func expandMacro(sv *SyntaxVal, callExpr *Expr, useEnv *Env) (Value, error) {
	for _, rule := range sv.Rules {
		bindings := newMacroBindings()
		if matchPattern(callExpr, rule.Pattern, sv.Literals, sv.Name, bindings) {
			patVars := make(map[string]bool)
			for k := range bindings.singles {
				patVars[k] = true
			}
			for k := range bindings.lists {
				patVars[k] = true
			}

			gensyms := make(map[string]string)
			collectIntroduced(rule.Template, patVars, sv.Literals, gensyms)

			expanded := expandTemplate(rule.Template, bindings, gensyms)

			if len(gensyms) > 0 {
				evalEnv := NewEnv(useEnv)
				for orig, gs := range gensyms {
					if v, ok := sv.DefEnv.Get(orig); ok {
						evalEnv.Set(gs, v)
					}
				}
				return eval(expanded, evalEnv)
			}
			return eval(expanded, useEnv)
		}
	}
	return nil, &EvalError{Message: fmt.Sprintf("no matching pattern for macro %s", sv.Name)}
}

// Pattern matching

func matchPattern(call *Expr, pattern *Expr, literals map[string]bool, macroName string, bindings *macroBindings) bool {
	if call.Kind != ExprList || pattern.Kind != ExprList {
		return false
	}
	if len(pattern.List) == 0 {
		return len(call.List) == 0
	}
	return matchListElems(call.List[1:], pattern.List[1:], literals, macroName, bindings)
}

func matchListElems(callElems []*Expr, patElems []*Expr, literals map[string]bool, macroName string, bindings *macroBindings) bool {
	ci, pi := 0, 0
	for pi < len(patElems) {
		if pi+1 < len(patElems) && isEllipsis(patElems[pi+1]) {
			subPat := patElems[pi]
			remaining := 0
			for j := pi + 2; j < len(patElems); j++ {
				if !isEllipsis(patElems[j]) {
					remaining++
				}
			}
			pvars := collectPatVars(subPat, literals, macroName)
			for _, pv := range pvars {
				bindings.lists[pv] = nil
			}
			limit := len(callElems) - remaining
			for ci < limit {
				sub := newMacroBindings()
				if !matchExpr(callElems[ci], subPat, literals, macroName, sub) {
					return false
				}
				for _, pv := range pvars {
					if e, ok := sub.singles[pv]; ok {
						bindings.lists[pv] = append(bindings.lists[pv], e)
					}
				}
				ci++
			}
			pi += 2
			continue
		}
		if ci >= len(callElems) {
			return false
		}
		if !matchExpr(callElems[ci], patElems[pi], literals, macroName, bindings) {
			return false
		}
		ci++
		pi++
	}
	return ci == len(callElems)
}

func matchExpr(call *Expr, pat *Expr, literals map[string]bool, macroName string, bindings *macroBindings) bool {
	if pat.Kind == ExprSymbol && pat.SVal == "_" {
		return true
	}
	if pat.Kind == ExprSymbol && literals[pat.SVal] {
		return call.Kind == ExprSymbol && call.SVal == pat.SVal
	}
	if pat.Kind == ExprSymbol {
		bindings.singles[pat.SVal] = call
		return true
	}
	if pat.Kind == ExprBool {
		return call.Kind == ExprBool && call.BVal == pat.BVal
	}
	if pat.Kind == ExprInt {
		return call.Kind == ExprInt && call.IVal == pat.IVal
	}
	if pat.Kind == ExprString {
		return call.Kind == ExprString && call.SVal == pat.SVal
	}
	if pat.Kind == ExprList {
		if call.Kind != ExprList {
			return false
		}
		return matchListElems(call.List, pat.List, literals, macroName, bindings)
	}
	return false
}

func isEllipsis(e *Expr) bool {
	return e.Kind == ExprSymbol && e.SVal == "..."
}

func collectPatVars(pat *Expr, literals map[string]bool, macroName string) []string {
	var vars []string
	collectPatVarsRec(pat, literals, macroName, &vars)
	return vars
}

func collectPatVarsRec(pat *Expr, literals map[string]bool, macroName string, vars *[]string) {
	if pat.Kind == ExprSymbol && !literals[pat.SVal] && pat.SVal != "_" && pat.SVal != macroName && pat.SVal != "..." {
		*vars = append(*vars, pat.SVal)
	}
	if pat.Kind == ExprList {
		for _, e := range pat.List {
			if !isEllipsis(e) {
				collectPatVarsRec(e, literals, macroName, vars)
			}
		}
	}
}

// Template expansion

func expandTemplate(tmpl *Expr, bindings *macroBindings, gensyms map[string]string) *Expr {
	switch tmpl.Kind {
	case ExprSymbol:
		if e, ok := bindings.singles[tmpl.SVal]; ok {
			return e
		}
		if gs, ok := gensyms[tmpl.SVal]; ok {
			return &Expr{Kind: ExprSymbol, SVal: gs, Line: tmpl.Line, Col: tmpl.Col}
		}
		return tmpl
	case ExprList:
		var result []*Expr
		for i := 0; i < len(tmpl.List); i++ {
			elem := tmpl.List[i]
			if i+1 < len(tmpl.List) && isEllipsis(tmpl.List[i+1]) {
				evars := findEllipsisVars(elem, bindings)
				if len(evars) > 0 {
					count := len(bindings.lists[evars[0]])
					for j := 0; j < count; j++ {
						iter := &macroBindings{
							singles: make(map[string]*Expr),
							lists:   bindings.lists,
						}
						for k, v := range bindings.singles {
							iter.singles[k] = v
						}
						for _, ev := range evars {
							iter.singles[ev] = bindings.lists[ev][j]
						}
						result = append(result, expandTemplate(elem, iter, gensyms))
					}
				}
				i++ // skip ellipsis
				continue
			}
			result = append(result, expandTemplate(elem, bindings, gensyms))
		}
		return &Expr{Kind: ExprList, List: result, Line: tmpl.Line, Col: tmpl.Col}
	default:
		return tmpl
	}
}

func findEllipsisVars(tmpl *Expr, bindings *macroBindings) []string {
	var vars []string
	findEllipsisVarsRec(tmpl, bindings, &vars)
	return vars
}

func findEllipsisVarsRec(tmpl *Expr, bindings *macroBindings, vars *[]string) {
	if tmpl.Kind == ExprSymbol {
		if _, ok := bindings.lists[tmpl.SVal]; ok {
			*vars = append(*vars, tmpl.SVal)
		}
	}
	if tmpl.Kind == ExprList {
		for _, e := range tmpl.List {
			findEllipsisVarsRec(e, bindings, vars)
		}
	}
}

// Hygiene: collect introduced identifiers (not pattern vars, not special forms)

func collectIntroduced(tmpl *Expr, patVars map[string]bool, literals map[string]bool, gensyms map[string]string) {
	if tmpl.Kind == ExprSymbol {
		name := tmpl.SVal
		if !patVars[name] && !specialForms[name] && !literals[name] && name != "..." {
			if _, ok := gensyms[name]; !ok {
				gensyms[name] = gensym(name)
			}
		}
	}
	if tmpl.Kind == ExprList {
		for _, e := range tmpl.List {
			collectIntroduced(e, patVars, literals, gensyms)
		}
	}
}
