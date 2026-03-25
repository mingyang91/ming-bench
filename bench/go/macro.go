package ming

import (
	"fmt"
	"sync/atomic"
)

var gensymCounter uint64

func gensym(base string) string {
	n := atomic.AddUint64(&gensymCounter, 1)
	return fmt.Sprintf("%s_g%d", base, n)
}

// MacroVal represents a syntax-rules macro transformer.
type MacroVal struct {
	Literals []string
	Rules    []macroRule
	DefEnv   *Env
}

func (m *MacroVal) String() string {
	return "#<macro>"
}

type macroRule struct {
	Pattern  Expr // ListExpr: (macro-name pattern...)
	Template Expr
}

// ValueExpr holds a pre-resolved Value, used for hygienic macro expansion.
type ValueExpr struct {
	Val    Value
	Ln, Cl int
}

func (e *ValueExpr) Line() int { return e.Ln }
func (e *ValueExpr) Col() int  { return e.Cl }

// Special form names that should not be renamed during macro expansion.
var specialForms = map[string]bool{
	"define": true, "if": true, "lambda": true, "begin": true,
	"cond": true, "let": true, "set!": true, "quote": true,
	"and": true, "or": true, "define-syntax": true, "syntax-rules": true,
	"else": true, "syntax-case": true, "syntax": true, "with-syntax": true,
}

type patternBinding struct {
	expr     Expr
	exprs    []Expr
	ellipsis bool
}

func evalDefineSyntax(e *ListExpr, env *Env) (Value, error) {
	if len(e.Items) != 3 {
		return nil, &EvalError{Message: "define-syntax: requires name and transformer"}
	}
	nameSym, ok := e.Items[1].(*SymbolExpr)
	if !ok {
		return nil, &EvalError{Message: "define-syntax: expected symbol"}
	}

	// Check if it's (syntax-rules ...)
	if srExpr, ok := e.Items[2].(*ListExpr); ok && len(srExpr.Items) >= 3 {
		if srSym, ok := srExpr.Items[0].(*SymbolExpr); ok && srSym.Name == "syntax-rules" {
			litList, ok := srExpr.Items[1].(*ListExpr)
			if !ok {
				return nil, &EvalError{Message: "define-syntax: expected literals list"}
			}
			var literals []string
			for _, l := range litList.Items {
				ls, ok := l.(*SymbolExpr)
				if !ok {
					return nil, &EvalError{Message: "define-syntax: literal must be a symbol"}
				}
				literals = append(literals, ls.Name)
			}
			var rules []macroRule
			for _, r := range srExpr.Items[2:] {
				rl, ok := r.(*ListExpr)
				if !ok || len(rl.Items) != 2 {
					return nil, &EvalError{Message: "define-syntax: bad rule"}
				}
				rules = append(rules, macroRule{Pattern: rl.Items[0], Template: rl.Items[1]})
			}
			macro := &MacroVal{Literals: literals, Rules: rules, DefEnv: env}
			env.set(nameSym.Name, macro)
			return &VoidVal{}, nil
		}
	}

	// Otherwise, evaluate as expression (e.g., lambda transformer)
	val, err := eval(e.Items[2], env)
	if err != nil {
		return nil, err
	}
	env.set(nameSym.Name, &MacroTransformerVal{Proc: val, DefNames: captureEnvNames(env)})
	return &VoidVal{}, nil
}

// expandMacro tries to expand a macro application.
func expandMacro(macro *MacroVal, form *ListExpr) (Expr, error) {
	for _, rule := range macro.Rules {
		bindings := make(map[string]*patternBinding)
		pat, ok := rule.Pattern.(*ListExpr)
		if !ok {
			continue
		}
		if matchPattern(pat.Items[1:], form.Items[1:], macro.Literals, bindings) {
			patVars := make(map[string]bool)
			for k := range bindings {
				patVars[k] = true
			}
			renameMap := make(map[string]string)
			collectTemplateSymbols(rule.Template, patVars, macro, renameMap)
			expanded := expandTemplate(rule.Template, bindings, patVars, macro, renameMap, form.Ln, form.Cl)
			return expanded, nil
		}
	}
	return nil, &EvalError{Message: fmt.Sprintf("%d:%d: no matching pattern for macro", form.Ln, form.Cl)}
}

// matchPattern matches pattern items against form items.
func matchPattern(pattern []Expr, form []Expr, literals []string, bindings map[string]*patternBinding) bool {
	ellipsisIdx := -1
	for i, p := range pattern {
		if sym, ok := p.(*SymbolExpr); ok && sym.Name == "..." {
			ellipsisIdx = i
			break
		}
	}

	if ellipsisIdx == -1 {
		if len(pattern) != len(form) {
			return false
		}
		for i, p := range pattern {
			if !matchOne(p, form[i], literals, bindings) {
				return false
			}
		}
		return true
	}

	if ellipsisIdx == 0 {
		return false
	}

	fixedBefore := ellipsisIdx - 1
	fixedAfter := pattern[ellipsisIdx+1:]

	if len(form) < fixedBefore+len(fixedAfter) {
		return false
	}

	for i := 0; i < fixedBefore; i++ {
		if !matchOne(pattern[i], form[i], literals, bindings) {
			return false
		}
	}

	afterStart := len(form) - len(fixedAfter)
	for i, p := range fixedAfter {
		if !matchOne(p, form[afterStart+i], literals, bindings) {
			return false
		}
	}

	ellipsisPattern := pattern[ellipsisIdx-1]
	sym, ok := ellipsisPattern.(*SymbolExpr)
	if !ok {
		return false
	}

	matched := make([]Expr, afterStart-fixedBefore)
	copy(matched, form[fixedBefore:afterStart])
	bindings[sym.Name] = &patternBinding{
		exprs:    matched,
		ellipsis: true,
	}
	return true
}

func matchOne(pattern Expr, form Expr, literals []string, bindings map[string]*patternBinding) bool {
	switch p := pattern.(type) {
	case *SymbolExpr:
		for _, lit := range literals {
			if p.Name == lit {
				fs, ok := form.(*SymbolExpr)
				return ok && fs.Name == p.Name
			}
		}
		bindings[p.Name] = &patternBinding{expr: form}
		return true
	case *ListExpr:
		fl, ok := form.(*ListExpr)
		if !ok {
			return false
		}
		return matchPattern(p.Items, fl.Items, literals, bindings)
	case *BoolExpr:
		fb, ok := form.(*BoolExpr)
		return ok && fb.Val == p.Val
	case *NumberExpr:
		fn, ok := form.(*NumberExpr)
		return ok && fn.Val == p.Val
	case *StringExpr:
		fs, ok := form.(*StringExpr)
		return ok && fs.Val == p.Val
	default:
		return false
	}
}

// collectTemplateSymbols assigns gensyms to macro-introduced symbols.
func collectTemplateSymbols(tmpl Expr, patVars map[string]bool, macro *MacroVal, renameMap map[string]string) {
	switch t := tmpl.(type) {
	case *SymbolExpr:
		if t.Name == "..." || patVars[t.Name] || specialForms[t.Name] {
			return
		}
		for _, lit := range macro.Literals {
			if t.Name == lit {
				return
			}
		}
		if _, ok := macro.DefEnv.get(t.Name); ok {
			return
		}
		if _, ok := renameMap[t.Name]; !ok {
			renameMap[t.Name] = gensym(t.Name)
		}
	case *ListExpr:
		for _, item := range t.Items {
			collectTemplateSymbols(item, patVars, macro, renameMap)
		}
	}
}

// expandTemplate expands a template with the given bindings.
func expandTemplate(tmpl Expr, bindings map[string]*patternBinding, patVars map[string]bool, macro *MacroVal, renameMap map[string]string, ln, cl int) Expr {
	switch t := tmpl.(type) {
	case *SymbolExpr:
		if t.Name == "..." {
			return t
		}
		if b, ok := bindings[t.Name]; ok && !b.ellipsis {
			return b.expr
		}
		if newName, ok := renameMap[t.Name]; ok {
			return &SymbolExpr{Name: newName, Ln: ln, Cl: cl}
		}
		if !specialForms[t.Name] && !patVars[t.Name] {
			if val, ok := macro.DefEnv.get(t.Name); ok {
				if _, isMacro := val.(*MacroVal); isMacro {
					return &SymbolExpr{Name: t.Name, Ln: ln, Cl: cl}
				}
				return &ValueExpr{Val: val, Ln: ln, Cl: cl}
			}
		}
		return &SymbolExpr{Name: t.Name, Ln: ln, Cl: cl}
	case *ListExpr:
		var items []Expr
		for i := 0; i < len(t.Items); i++ {
			if i+1 < len(t.Items) {
				if sym, ok := t.Items[i+1].(*SymbolExpr); ok && sym.Name == "..." {
					items = append(items, expandEllipsis(t.Items[i], bindings, patVars, macro, renameMap, ln, cl)...)
					i++
					continue
				}
			}
			items = append(items, expandTemplate(t.Items[i], bindings, patVars, macro, renameMap, ln, cl))
		}
		return &ListExpr{Items: items, Ln: ln, Cl: cl}
	default:
		return tmpl
	}
}

// expandEllipsis expands a template element that has ... after it.
func expandEllipsis(tmpl Expr, bindings map[string]*patternBinding, patVars map[string]bool, macro *MacroVal, renameMap map[string]string, ln, cl int) []Expr {
	var ellipsisVar string
	var ellipsisBinding *patternBinding
	findEllipsisVar(tmpl, bindings, &ellipsisVar, &ellipsisBinding)

	if ellipsisBinding == nil || !ellipsisBinding.ellipsis {
		return nil
	}

	var result []Expr
	for _, expr := range ellipsisBinding.exprs {
		newBindings := make(map[string]*patternBinding)
		for k, v := range bindings {
			newBindings[k] = v
		}
		newBindings[ellipsisVar] = &patternBinding{expr: expr}
		result = append(result, expandTemplate(tmpl, newBindings, patVars, macro, renameMap, ln, cl))
	}
	return result
}

func findEllipsisVar(tmpl Expr, bindings map[string]*patternBinding, name *string, binding **patternBinding) {
	switch t := tmpl.(type) {
	case *SymbolExpr:
		if b, ok := bindings[t.Name]; ok && b.ellipsis {
			*name = t.Name
			*binding = b
		}
	case *ListExpr:
		for _, item := range t.Items {
			findEllipsisVar(item, bindings, name, binding)
			if *binding != nil {
				return
			}
		}
	}
}
