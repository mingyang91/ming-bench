package ming

import (
	"fmt"
	"sync/atomic"
)

var gensymCounter int64

func gensym(base string) string {
	n := atomic.AddInt64(&gensymCounter, 1)
	return fmt.Sprintf("%s__g%d", base, n)
}

// SyntaxRules represents a macro transformer created by syntax-rules.
type SyntaxRules struct {
	Literals []string
	Rules    []syntaxRule
	DefEnv   *Env
}

type syntaxRule struct {
	Pattern  *Expr
	Template *Expr
}

// Special forms that should not be renamed during hygiene.
var specialForms = map[string]bool{
	"if": true, "let": true, "begin": true, "set!": true,
	"define": true, "quote": true, "lambda": true, "cond": true,
	"and": true, "or": true, "define-syntax": true, "syntax-rules": true,
	"letrec": true, "let*": true, "do": true, "case": true,
	"guard": true,
}

// parseSyntaxRules parses (syntax-rules (literals...) (pattern template) ...).
func parseSyntaxRules(expr *Expr, defEnv *Env) (*SyntaxRules, error) {
	if len(expr.List) < 3 {
		return nil, errAt(expr, "syntax-rules: expected at least 2 arguments")
	}
	litExpr := expr.List[1]
	if litExpr.Type != ExprList {
		return nil, errAt(litExpr, "syntax-rules: literals must be a list")
	}
	var literals []string
	for _, l := range litExpr.List {
		if l.Type != ExprSymbol {
			return nil, errAt(l, "syntax-rules: literal must be a symbol")
		}
		literals = append(literals, l.StrVal)
	}
	var rules []syntaxRule
	for _, ruleExpr := range expr.List[2:] {
		if ruleExpr.Type != ExprList || len(ruleExpr.List) != 2 {
			return nil, errAt(ruleExpr, "syntax-rules: rule must be (pattern template)")
		}
		rules = append(rules, syntaxRule{
			Pattern:  ruleExpr.List[0],
			Template: ruleExpr.List[1],
		})
	}
	return &SyntaxRules{Literals: literals, Rules: rules, DefEnv: defEnv}, nil
}

// expandMacro expands a macro application in the given use-site environment.
func (sr *SyntaxRules) expandMacro(input *Expr, useEnv *Env) (*Expr, error) {
	litSet := make(map[string]bool)
	for _, l := range sr.Literals {
		litSet[l] = true
	}

	for _, rule := range sr.Rules {
		bindings := matchPattern(rule.Pattern, input, litSet)
		if bindings == nil {
			continue
		}

		// Collect pattern variable names
		patVars := make(map[string]bool)
		for k := range bindings {
			patVars[k] = true
		}

		// Collect free symbols for hygiene
		freeSyms := collectFreeSymbols(rule.Template, patVars)

		// Create hygiene map: original name -> gensym
		hygieneMap := make(map[string]string)
		for sym := range freeSyms {
			hygieneMap[sym] = gensym(sym)
		}

		// Bind gensyms in use-site env to definition-site values
		for origName, newName := range hygieneMap {
			if val, ok := sr.DefEnv.Get(origName); ok {
				useEnv.Set(newName, val)
			}
		}

		expanded := expandTemplate(rule.Template, bindings, hygieneMap)
		return expanded, nil
	}

	return nil, errAtf(input, "no matching pattern for macro")
}

// matchPattern tries to match input against a pattern, skipping the macro name (first element).
func matchPattern(pattern *Expr, input *Expr, literals map[string]bool) map[string]interface{} {
	if pattern.Type != ExprList || input.Type != ExprList {
		return nil
	}
	bindings := make(map[string]interface{})
	if matchElements(pattern.List[1:], input.List[1:], literals, bindings) {
		return bindings
	}
	return nil
}

// matchElements matches a sequence of pattern elements against input elements.
func matchElements(patterns []*Expr, inputs []*Expr, literals map[string]bool, bindings map[string]interface{}) bool {
	pi, ii := 0, 0
	for pi < len(patterns) {
		// Check for ellipsis following current pattern
		if pi+1 < len(patterns) && isEllipsis(patterns[pi+1]) {
			pat := patterns[pi]
			// Count remaining non-ellipsis patterns after this ellipsis
			remainingPats := len(patterns) - pi - 2
			endIdx := len(inputs) - remainingPats
			if endIdx < ii {
				return false
			}
			for ii < endIdx {
				sub := make(map[string]interface{})
				if !matchOne(pat, inputs[ii], literals, sub) {
					return false
				}
				// Merge sub-bindings as lists
				for k, v := range sub {
					if existing, ok := bindings[k]; ok {
						bindings[k] = append(existing.([]*Expr), v.(*Expr))
					} else {
						bindings[k] = []*Expr{v.(*Expr)}
					}
				}
				ii++
			}
			// Initialize empty lists for pattern vars with zero matches
			if endIdx == ii-(endIdx-ii) || true {
				initPatternVarsIfMissing(pat, literals, bindings)
			}
			pi += 2
			continue
		}
		if ii >= len(inputs) {
			return false
		}
		if !matchOne(patterns[pi], inputs[ii], literals, bindings) {
			return false
		}
		pi++
		ii++
	}
	return ii == len(inputs)
}

// matchOne matches a single pattern element against a single input expression.
func matchOne(pat *Expr, input *Expr, literals map[string]bool, bindings map[string]interface{}) bool {
	if pat.Type == ExprSymbol {
		if pat.StrVal == "_" {
			return true
		}
		if literals[pat.StrVal] {
			return input.Type == ExprSymbol && input.StrVal == pat.StrVal
		}
		// Pattern variable
		bindings[pat.StrVal] = input
		return true
	}
	if pat.Type == ExprList {
		if input.Type != ExprList {
			return false
		}
		return matchElements(pat.List, input.List, literals, bindings)
	}
	// Literal match
	if pat.Type != input.Type {
		return false
	}
	switch pat.Type {
	case ExprInteger:
		return pat.IntVal == input.IntVal
	case ExprBoolean:
		return pat.BoolVal == input.BoolVal
	case ExprString:
		return pat.StrVal == input.StrVal
	}
	return false
}

func isEllipsis(e *Expr) bool {
	return e.Type == ExprSymbol && e.StrVal == "..."
}

// initPatternVarsIfMissing ensures pattern variables have empty list entries for zero-match ellipsis.
func initPatternVarsIfMissing(pat *Expr, literals map[string]bool, bindings map[string]interface{}) {
	if pat.Type == ExprSymbol && !literals[pat.StrVal] && pat.StrVal != "_" && pat.StrVal != "..." {
		if _, ok := bindings[pat.StrVal]; !ok {
			bindings[pat.StrVal] = []*Expr{}
		}
	}
	if pat.Type == ExprList {
		for _, p := range pat.List {
			initPatternVarsIfMissing(p, literals, bindings)
		}
	}
}

// expandTemplate instantiates a template with pattern variable bindings and hygiene renaming.
func expandTemplate(tmpl *Expr, bindings map[string]interface{}, hygieneMap map[string]string) *Expr {
	switch tmpl.Type {
	case ExprSymbol:
		// Pattern variable substitution
		if val, ok := bindings[tmpl.StrVal]; ok {
			if expr, ok := val.(*Expr); ok {
				return expr
			}
			// []*Expr = ellipsis var used outside ellipsis context; leave as-is
		}
		// Hygiene renaming
		if newName, ok := hygieneMap[tmpl.StrVal]; ok {
			return &Expr{Type: ExprSymbol, StrVal: newName, Line: tmpl.Line, Col: tmpl.Col}
		}
		return tmpl
	case ExprList:
		return expandTemplateList(tmpl, bindings, hygieneMap)
	default:
		return tmpl
	}
}

func expandTemplateList(tmpl *Expr, bindings map[string]interface{}, hygieneMap map[string]string) *Expr {
	var result []*Expr
	for i := 0; i < len(tmpl.List); i++ {
		// Check if followed by ellipsis
		if i+1 < len(tmpl.List) && isEllipsis(tmpl.List[i+1]) {
			subTmpl := tmpl.List[i]
			ellipsisVars := findEllipsisVars(subTmpl, bindings)
			if len(ellipsisVars) > 0 {
				firstList := bindings[ellipsisVars[0]].([]*Expr)
				for j := 0; j < len(firstList); j++ {
					iterBindings := make(map[string]interface{})
					for k, v := range bindings {
						iterBindings[k] = v
					}
					for _, ev := range ellipsisVars {
						evList := bindings[ev].([]*Expr)
						if j < len(evList) {
							iterBindings[ev] = evList[j]
						}
					}
					result = append(result, expandTemplate(subTmpl, iterBindings, hygieneMap))
				}
			}
			i++ // skip the ...
			continue
		}
		result = append(result, expandTemplate(tmpl.List[i], bindings, hygieneMap))
	}
	return &Expr{Type: ExprList, List: result, Line: tmpl.Line, Col: tmpl.Col}
}

// findEllipsisVars finds pattern variables in a template that are bound to lists (from ellipsis matching).
func findEllipsisVars(tmpl *Expr, bindings map[string]interface{}) []string {
	var vars []string
	findEllipsisVarsInner(tmpl, bindings, &vars)
	return vars
}

func findEllipsisVarsInner(tmpl *Expr, bindings map[string]interface{}, vars *[]string) {
	if tmpl.Type == ExprSymbol {
		if val, ok := bindings[tmpl.StrVal]; ok {
			if _, isList := val.([]*Expr); isList {
				*vars = append(*vars, tmpl.StrVal)
			}
		}
	}
	if tmpl.Type == ExprList {
		for _, e := range tmpl.List {
			findEllipsisVarsInner(e, bindings, vars)
		}
	}
}

// collectFreeSymbols finds template symbols that are not pattern variables and not special forms.
func collectFreeSymbols(tmpl *Expr, patternVars map[string]bool) map[string]bool {
	free := make(map[string]bool)
	collectFreeSymbolsInner(tmpl, patternVars, free)
	return free
}

func collectFreeSymbolsInner(tmpl *Expr, patternVars map[string]bool, free map[string]bool) {
	if tmpl.Type == ExprSymbol {
		if !patternVars[tmpl.StrVal] && !specialForms[tmpl.StrVal] && tmpl.StrVal != "..." {
			free[tmpl.StrVal] = true
		}
	}
	if tmpl.Type == ExprList {
		for _, e := range tmpl.List {
			collectFreeSymbolsInner(e, patternVars, free)
		}
	}
}
