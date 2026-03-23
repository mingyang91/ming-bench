package ming

import "fmt"

var macroGensymCounter int

func macroGensym(base string) string {
	macroGensymCounter++
	return fmt.Sprintf("%s__%d", base, macroGensymCounter)
}

// syntaxRule represents one pattern-template pair in syntax-rules.
type syntaxRule struct {
	pattern  *expr
	template *expr
}

// matchResult holds bindings from a successful pattern match.
type matchResult struct {
	singles  map[string]*expr
	ellipsis map[string][]*expr
}

func newMatchResult() *matchResult {
	return &matchResult{
		singles:  make(map[string]*expr),
		ellipsis: make(map[string][]*expr),
	}
}

// evalDefineSyntax handles (define-syntax name (syntax-rules ...))
func (ip *interp) evalDefineSyntax(e *expr, envir *env) (*value, error) {
	if len(e.items) != 3 {
		return nil, &EvalError{Message: fmt.Sprintf("%d:%d: define-syntax: bad syntax", e.line, e.col)}
	}
	name := e.items[1]
	if name.kind != "symbol" {
		return nil, &EvalError{Message: fmt.Sprintf("%d:%d: define-syntax: expected symbol", e.line, e.col)}
	}
	sr := e.items[2]
	if sr.kind != "list" || len(sr.items) < 2 ||
		sr.items[0].kind != "symbol" || sr.items[0].sval != "syntax-rules" {
		return nil, &EvalError{Message: fmt.Sprintf("%d:%d: define-syntax: expected syntax-rules", e.line, e.col)}
	}

	// Parse literals list
	var literals []string
	if sr.items[1].kind == "list" {
		for _, l := range sr.items[1].items {
			if l.kind == "symbol" {
				literals = append(literals, l.sval)
			}
		}
	}

	// Parse pattern-template rules
	var rules []syntaxRule
	for _, ruleExpr := range sr.items[2:] {
		if ruleExpr.kind != "list" || len(ruleExpr.items) != 2 {
			return nil, &EvalError{Message: fmt.Sprintf("%d:%d: define-syntax: bad rule", e.line, e.col)}
		}
		rules = append(rules, syntaxRule{
			pattern:  ruleExpr.items[0],
			template: ruleExpr.items[1],
		})
	}

	macro := &value{
		typ:           valMacro,
		macroRules:    rules,
		macroLiterals: literals,
		macroDefEnv:   envir,
	}
	envir.set(name.sval, macro)
	return voidVal, nil
}

// expandMacro tries each rule until one matches, then expands the template.
func expandMacro(macro *value, inputExpr *expr, macroName string) (*expr, error) {
	literals := make(map[string]bool)
	for _, l := range macro.macroLiterals {
		literals[l] = true
	}
	literals[macroName] = true // macro name is always a literal in patterns

	for _, rule := range macro.macroRules {
		if bindings, ok := matchPattern(rule.pattern, inputExpr, literals); ok {
			patVars := collectPatternVars(rule.pattern, literals)
			introduced := collectIntroducedBindings(rule.template, patVars)
			renames := make(map[string]string)
			for name := range introduced {
				renames[name] = macroGensym(name)
			}
			return expandTemplate(rule.template, bindings, macro.macroDefEnv, renames), nil
		}
	}
	return nil, &EvalError{Message: fmt.Sprintf("no matching pattern for macro %s", macroName)}
}

// --- Pattern matching ---

func matchPattern(pattern, input *expr, literals map[string]bool) (*matchResult, bool) {
	result := newMatchResult()
	if doMatch(pattern, input, literals, result) {
		return result, true
	}
	return nil, false
}

func doMatch(pattern, input *expr, literals map[string]bool, result *matchResult) bool {
	if pattern.kind == "symbol" {
		if pattern.sval == "_" {
			return true
		}
		if pattern.sval == "..." {
			return false
		}
		if literals[pattern.sval] {
			return input.kind == "symbol" && input.sval == pattern.sval
		}
		result.singles[pattern.sval] = input
		return true
	}
	if pattern.kind == "list" {
		if input.kind != "list" {
			return false
		}
		return matchList(pattern.items, input.items, literals, result)
	}
	if pattern.kind == input.kind {
		switch pattern.kind {
		case "int":
			return pattern.ival == input.ival
		case "bool":
			return pattern.bval == input.bval
		case "string":
			return pattern.sval == input.sval
		}
	}
	return false
}

func matchList(patItems, inputItems []*expr, literals map[string]bool, result *matchResult) bool {
	// Find ellipsis position
	ellipsisIdx := -1
	for i, p := range patItems {
		if p.kind == "symbol" && p.sval == "..." {
			ellipsisIdx = i
			break
		}
	}

	if ellipsisIdx == -1 {
		if len(patItems) != len(inputItems) {
			return false
		}
		for i, p := range patItems {
			if !doMatch(p, inputItems[i], literals, result) {
				return false
			}
		}
		return true
	}

	// The element before ... is the repeated pattern variable
	beforeEllipsis := ellipsisIdx - 1
	afterEllipsis := patItems[ellipsisIdx+1:]
	afterCount := len(afterEllipsis)

	// Match fixed elements before the ellipsis variable
	for i := 0; i < beforeEllipsis; i++ {
		if i >= len(inputItems) {
			return false
		}
		if !doMatch(patItems[i], inputItems[i], literals, result) {
			return false
		}
	}

	if len(inputItems) < beforeEllipsis+afterCount {
		return false
	}

	// Match fixed elements after ...
	for i := 0; i < afterCount; i++ {
		inputIdx := len(inputItems) - afterCount + i
		if !doMatch(afterEllipsis[i], inputItems[inputIdx], literals, result) {
			return false
		}
	}

	// The ellipsis variable captures all middle elements
	ellipsisVar := patItems[beforeEllipsis]
	middleStart := beforeEllipsis
	middleEnd := len(inputItems) - afterCount
	if ellipsisVar.kind == "symbol" && !literals[ellipsisVar.sval] {
		var matched []*expr
		for i := middleStart; i < middleEnd; i++ {
			matched = append(matched, inputItems[i])
		}
		result.ellipsis[ellipsisVar.sval] = matched
	}
	return true
}

// --- Template expansion ---

func expandTemplate(tmpl *expr, bindings *matchResult, defEnv *env, introduced map[string]string) *expr {
	switch tmpl.kind {
	case "symbol":
		name := tmpl.sval
		if e, ok := bindings.singles[name]; ok {
			return e
		}
		if newName, ok := introduced[name]; ok {
			return &expr{kind: "symbol", sval: newName, line: tmpl.line, col: tmpl.col}
		}
		if defEnv != nil && !isMacroSpecialForm(name) && name != "..." {
			if _, ok := defEnv.get(name); ok {
				return &expr{kind: "symbol", sval: name, line: tmpl.line, col: tmpl.col, envRef: defEnv}
			}
		}
		return tmpl
	case "list":
		return expandListTemplate(tmpl, bindings, defEnv, introduced)
	default:
		return tmpl
	}
}

func expandListTemplate(tmpl *expr, bindings *matchResult, defEnv *env, introduced map[string]string) *expr {
	var newItems []*expr

	for i := 0; i < len(tmpl.items); i++ {
		item := tmpl.items[i]

		// Check if next element is ...
		if i+1 < len(tmpl.items) && tmpl.items[i+1].kind == "symbol" && tmpl.items[i+1].sval == "..." {
			// Simple ellipsis variable
			if item.kind == "symbol" {
				if elems, ok := bindings.ellipsis[item.sval]; ok {
					newItems = append(newItems, elems...)
					i++ // skip ...
					continue
				}
			}
			// Complex sub-template with ellipsis — replicate for each element
			ellipsisVar := findEllipsisVar(item, bindings)
			if ellipsisVar != "" {
				elems := bindings.ellipsis[ellipsisVar]
				for _, elem := range elems {
					tmpBindings := &matchResult{
						singles:  make(map[string]*expr),
						ellipsis: bindings.ellipsis,
					}
					for k, v := range bindings.singles {
						tmpBindings.singles[k] = v
					}
					tmpBindings.singles[ellipsisVar] = elem
					newItems = append(newItems, expandTemplate(item, tmpBindings, defEnv, introduced))
				}
				i++ // skip ...
				continue
			}
		}

		newItems = append(newItems, expandTemplate(item, bindings, defEnv, introduced))
	}

	return &expr{kind: "list", items: newItems, line: tmpl.line, col: tmpl.col}
}

func findEllipsisVar(tmpl *expr, bindings *matchResult) string {
	if tmpl.kind == "symbol" {
		if _, ok := bindings.ellipsis[tmpl.sval]; ok {
			return tmpl.sval
		}
		return ""
	}
	if tmpl.kind == "list" {
		for _, item := range tmpl.items {
			if v := findEllipsisVar(item, bindings); v != "" {
				return v
			}
		}
	}
	return ""
}

// --- Hygiene helpers ---

func isMacroSpecialForm(name string) bool {
	switch name {
	case "if", "let", "begin", "set!", "define", "lambda", "quote",
		"cond", "and", "or", "define-syntax", "syntax-rules",
		"call/cc", "call-with-current-continuation":
		return true
	}
	return false
}

func collectPatternVars(pattern *expr, literals map[string]bool) map[string]bool {
	vars := make(map[string]bool)
	collectPVars(pattern, literals, vars)
	return vars
}

func collectPVars(pattern *expr, literals map[string]bool, vars map[string]bool) {
	switch pattern.kind {
	case "symbol":
		if !literals[pattern.sval] && pattern.sval != "_" && pattern.sval != "..." {
			vars[pattern.sval] = true
		}
	case "list":
		for _, item := range pattern.items {
			collectPVars(item, literals, vars)
		}
	}
}

// collectIntroducedBindings finds binding names in the template that aren't pattern variables.
func collectIntroducedBindings(tmpl *expr, patternVars map[string]bool) map[string]bool {
	result := make(map[string]bool)
	collectIntroduced(tmpl, patternVars, result)
	return result
}

func collectIntroduced(tmpl *expr, patternVars map[string]bool, result map[string]bool) {
	if tmpl.kind != "list" || len(tmpl.items) == 0 {
		return
	}
	head := tmpl.items[0]
	if head.kind == "symbol" {
		switch head.sval {
		case "let":
			if len(tmpl.items) >= 3 && tmpl.items[1].kind == "list" {
				for _, binding := range tmpl.items[1].items {
					if binding.kind == "list" && len(binding.items) >= 1 && binding.items[0].kind == "symbol" {
						name := binding.items[0].sval
						if !patternVars[name] {
							result[name] = true
						}
					}
				}
			}
		case "lambda":
			if len(tmpl.items) >= 3 && tmpl.items[1].kind == "list" {
				for _, p := range tmpl.items[1].items {
					if p.kind == "symbol" && !patternVars[p.sval] {
						result[p.sval] = true
					}
				}
			}
		}
	}
	for _, item := range tmpl.items {
		collectIntroduced(item, patternVars, result)
	}
}
