package ming

import "fmt"

// SyntaxRule represents one pattern/template pair in syntax-rules.
type SyntaxRule struct {
	Pattern  []*Expr // pattern elements (excluding macro name)
	Template *Expr
}

// SyntaxRulesMacro represents a syntax-rules macro transformer.
type SyntaxRulesMacro struct {
	Literals []string
	Rules    []SyntaxRule
	DefEnv   *Env
}

// PatternBindings maps pattern variable names to matched expressions.
type PatternBindings struct {
	singles map[string]*Expr
	lists   map[string][]*Expr
}

func newPatternBindings() *PatternBindings {
	return &PatternBindings{
		singles: make(map[string]*Expr),
		lists:   make(map[string][]*Expr),
	}
}

func (pb *PatternBindings) merge(other *PatternBindings) {
	for k, v := range other.singles {
		pb.singles[k] = v
	}
	for k, v := range other.lists {
		pb.lists[k] = v
	}
}

func isEllipsis(e *Expr) bool {
	return e.Type == ExprSymbol && e.StrVal == "..."
}

func isSpecialForm(name string) bool {
	switch name {
	case "if", "define", "lambda", "let", "begin", "cond", "set!", "quote",
		"and", "or", "define-syntax", "syntax-rules":
		return true
	}
	return false
}

// matchPattern tries to match input expressions against pattern elements.
func matchPattern(pattern []*Expr, input []*Expr, literals map[string]bool) *PatternBindings {
	bindings := newPatternBindings()
	pi, ii := 0, 0

	for pi < len(pattern) {
		// Check if current pattern element is followed by ellipsis
		if pi+1 < len(pattern) && isEllipsis(pattern[pi+1]) {
			pvar := pattern[pi]
			if pvar.Type != ExprSymbol {
				return nil
			}
			varName := pvar.StrVal
			// Count remaining required (non-ellipsis) patterns
			remainingRequired := 0
			for rpi := pi + 2; rpi < len(pattern); rpi++ {
				if rpi+1 < len(pattern) && isEllipsis(pattern[rpi+1]) {
					rpi++ // skip ellipsis
				} else {
					remainingRequired++
				}
			}
			available := len(input) - ii - remainingRequired
			if available < 0 {
				return nil
			}
			matched := make([]*Expr, available)
			for j := 0; j < available; j++ {
				matched[j] = input[ii+j]
			}
			bindings.lists[varName] = matched
			ii += available
			pi += 2 // skip pattern + ellipsis
			continue
		}

		if ii >= len(input) {
			return nil
		}

		pat := pattern[pi]
		inp := input[ii]

		if pat.Type == ExprSymbol {
			if literals[pat.StrVal] {
				// Literal — must match exactly
				if inp.Type != ExprSymbol || inp.StrVal != pat.StrVal {
					return nil
				}
			} else if pat.StrVal == "_" {
				// Wildcard
			} else {
				// Pattern variable
				bindings.singles[pat.StrVal] = inp
			}
		} else if pat.Type == ExprList {
			if inp.Type != ExprList {
				return nil
			}
			sub := matchPattern(pat.Elements, inp.Elements, literals)
			if sub == nil {
				return nil
			}
			bindings.merge(sub)
		} else {
			if !exprEqual(pat, inp) {
				return nil
			}
		}

		pi++
		ii++
	}

	if ii != len(input) {
		return nil
	}
	return bindings
}

func exprEqual(a, b *Expr) bool {
	if a.Type != b.Type {
		return false
	}
	switch a.Type {
	case ExprInteger:
		return a.IntVal == b.IntVal
	case ExprBoolean:
		return a.BoolVal == b.BoolVal
	case ExprString:
		return a.StrVal == b.StrVal
	case ExprSymbol:
		return a.StrVal == b.StrVal
	}
	return false
}

// collectPatternVars collects all pattern variable names from pattern elements.
func collectPatternVars(pattern []*Expr, literals map[string]bool) map[string]bool {
	vars := make(map[string]bool)
	for _, p := range pattern {
		collectPatternVarsExpr(p, literals, vars)
	}
	return vars
}

func collectPatternVarsExpr(expr *Expr, literals map[string]bool, vars map[string]bool) {
	switch expr.Type {
	case ExprSymbol:
		if expr.StrVal != "..." && expr.StrVal != "_" && !literals[expr.StrVal] {
			vars[expr.StrVal] = true
		}
	case ExprList:
		for _, e := range expr.Elements {
			collectPatternVarsExpr(e, literals, vars)
		}
	}
}

// expandTemplate instantiates a template with the given bindings.
func expandTemplate(tmpl *Expr, bindings *PatternBindings, defEnv *Env) *Expr {
	switch tmpl.Type {
	case ExprSymbol:
		name := tmpl.StrVal
		if e, ok := bindings.singles[name]; ok {
			return e
		}
		// Definition-site binding for hygiene
		if !isSpecialForm(name) && name != "..." {
			if val, ok := defEnv.Get(name); ok {
				return &Expr{Type: ExprLiteral, LitVal: val, Line: tmpl.Line, Col: tmpl.Col}
			}
		}
		return tmpl

	case ExprList:
		return expandTemplateList(tmpl, bindings, defEnv)

	default:
		return tmpl
	}
}

func expandTemplateList(tmpl *Expr, bindings *PatternBindings, defEnv *Env) *Expr {
	var elements []*Expr
	for i := 0; i < len(tmpl.Elements); i++ {
		elem := tmpl.Elements[i]
		if i+1 < len(tmpl.Elements) && isEllipsis(tmpl.Elements[i+1]) {
			vars := findEllipsisVars(elem, bindings)
			if len(vars) > 0 {
				count := len(bindings.lists[vars[0]])
				for j := 0; j < count; j++ {
					elements = append(elements, expandEllipsisIter(elem, bindings, j, defEnv))
				}
			}
			i++ // skip ellipsis
			continue
		}
		elements = append(elements, expandTemplate(elem, bindings, defEnv))
	}
	return &Expr{Type: ExprList, Elements: elements, Line: tmpl.Line, Col: tmpl.Col}
}

// findEllipsisVars finds pattern variables with list bindings in a template element.
func findEllipsisVars(tmpl *Expr, bindings *PatternBindings) []string {
	var vars []string
	switch tmpl.Type {
	case ExprSymbol:
		if _, ok := bindings.lists[tmpl.StrVal]; ok {
			vars = append(vars, tmpl.StrVal)
		}
	case ExprList:
		for _, e := range tmpl.Elements {
			vars = append(vars, findEllipsisVars(e, bindings)...)
		}
	}
	return vars
}

// expandEllipsisIter expands a template element for one iteration of an ellipsis.
func expandEllipsisIter(tmpl *Expr, bindings *PatternBindings, idx int, defEnv *Env) *Expr {
	switch tmpl.Type {
	case ExprSymbol:
		name := tmpl.StrVal
		if elems, ok := bindings.lists[name]; ok {
			if idx < len(elems) {
				return elems[idx]
			}
			return tmpl
		}
		if e, ok := bindings.singles[name]; ok {
			return e
		}
		if !isSpecialForm(name) && name != "..." {
			if val, ok := defEnv.Get(name); ok {
				return &Expr{Type: ExprLiteral, LitVal: val, Line: tmpl.Line, Col: tmpl.Col}
			}
		}
		return tmpl

	case ExprList:
		var elements []*Expr
		for i := 0; i < len(tmpl.Elements); i++ {
			elem := tmpl.Elements[i]
			if i+1 < len(tmpl.Elements) && isEllipsis(tmpl.Elements[i+1]) {
				vars := findEllipsisVars(elem, bindings)
				if len(vars) > 0 {
					count := len(bindings.lists[vars[0]])
					for j := 0; j < count; j++ {
						elements = append(elements, expandEllipsisIter(elem, bindings, j, defEnv))
					}
				}
				i++
				continue
			}
			elements = append(elements, expandEllipsisIter(elem, bindings, idx, defEnv))
		}
		return &Expr{Type: ExprList, Elements: elements, Line: tmpl.Line, Col: tmpl.Col}

	default:
		return tmpl
	}
}

// expandMacro tries each rule in the macro against the input expression.
func expandMacro(macro *SyntaxRulesMacro, input *Expr) (*Expr, error) {
	if input.Type != ExprList || len(input.Elements) == 0 {
		return nil, fmt.Errorf("invalid macro application")
	}

	literals := make(map[string]bool)
	for _, l := range macro.Literals {
		literals[l] = true
	}

	inputArgs := input.Elements[1:] // skip macro name

	for _, rule := range macro.Rules {
		bindings := matchPattern(rule.Pattern, inputArgs, literals)
		if bindings != nil {
			return expandTemplate(rule.Template, bindings, macro.DefEnv), nil
		}
	}

	return nil, fmt.Errorf("%d:%d: no matching pattern for macro", input.Line, input.Col)
}

// evalDefineSyntax handles (define-syntax name (syntax-rules ...)).
func evalDefineSyntax(expr *Expr, env *Env) (*Value, error) {
	if len(expr.Elements) != 3 {
		return nil, fmt.Errorf("%d:%d: define-syntax requires 2 arguments", expr.Line, expr.Col)
	}
	nameExpr := expr.Elements[1]
	if nameExpr.Type != ExprSymbol {
		return nil, fmt.Errorf("%d:%d: define-syntax expects a symbol", expr.Line, expr.Col)
	}

	srExpr := expr.Elements[2]
	if srExpr.Type != ExprList || len(srExpr.Elements) < 2 {
		return nil, fmt.Errorf("%d:%d: expected syntax-rules", expr.Line, expr.Col)
	}
	if srExpr.Elements[0].Type != ExprSymbol || srExpr.Elements[0].StrVal != "syntax-rules" {
		return nil, fmt.Errorf("%d:%d: expected syntax-rules", expr.Line, expr.Col)
	}

	// Parse literals list
	litExpr := srExpr.Elements[1]
	if litExpr.Type != ExprList {
		return nil, fmt.Errorf("%d:%d: syntax-rules literals must be a list", expr.Line, expr.Col)
	}
	var literals []string
	for _, l := range litExpr.Elements {
		if l.Type != ExprSymbol {
			return nil, fmt.Errorf("%d:%d: literal must be a symbol", l.Line, l.Col)
		}
		literals = append(literals, l.StrVal)
	}

	// Parse rules
	var rules []SyntaxRule
	for _, ruleExpr := range srExpr.Elements[2:] {
		if ruleExpr.Type != ExprList || len(ruleExpr.Elements) != 2 {
			return nil, fmt.Errorf("%d:%d: syntax rule must be (pattern template)", ruleExpr.Line, ruleExpr.Col)
		}
		patExpr := ruleExpr.Elements[0]
		tmplExpr := ruleExpr.Elements[1]

		if patExpr.Type != ExprList || len(patExpr.Elements) < 1 {
			return nil, fmt.Errorf("%d:%d: pattern must be a list starting with macro name", patExpr.Line, patExpr.Col)
		}

		rules = append(rules, SyntaxRule{
			Pattern:  patExpr.Elements[1:], // skip macro name
			Template: tmplExpr,
		})
	}

	macro := &SyntaxRulesMacro{
		Literals: literals,
		Rules:    rules,
		DefEnv:   env,
	}

	env.Set(nameExpr.StrVal, &Value{Type: TypeMacro, Macro: macro})
	return Void, nil
}
