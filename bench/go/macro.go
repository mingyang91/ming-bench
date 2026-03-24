package ming

import "fmt"

var gensymCounter int

func gensym(base string) string {
	gensymCounter++
	return fmt.Sprintf("__%s_%d", base, gensymCounter)
}

type syntaxRulesMacro struct {
	literals    []string
	clauses     []macroClause
	defEnv      *env
	transformer *Value // lambda-based transformer for syntax-case macros
}

type macroClause struct {
	pattern  *astNode
	template *astNode
}

type patBinding struct {
	single     *astNode
	multi      []*astNode
	isEllipsis bool
}

var specialForms = map[string]bool{
	"and": true, "or": true, "define": true, "if": true, "quote": true,
	"lambda": true, "let": true, "let*": true, "letrec": true, "letrec*": true,
	"begin": true, "cond": true, "set!": true, "case": true, "do": true,
	"define-syntax": true, "syntax-rules": true, "syntax-case": true,
	"syntax": true, "with-syntax": true,
	"case-lambda": true, "define-record-type": true,
	"call/cc": true, "call-with-current-continuation": true, "guard": true,
}

func evalDefineSyntax(node *astNode, e *env, ip *interp) (*Value, error) {
	if len(node.children) != 3 {
		return nil, &EvalError{Message: fmt.Sprintf("%d:%d: define-syntax: bad syntax", node.line, node.col)}
	}
	nameNode := node.children[1]
	if !nameNode.isAtom || nameNode.tok.kind != tokSymbol {
		return nil, &EvalError{Message: fmt.Sprintf("%d:%d: define-syntax: expected identifier", node.line, node.col)}
	}
	name := nameNode.tok.sval

	srNode := node.children[2]
	if srNode.isAtom || len(srNode.children) < 2 {
		return nil, &EvalError{Message: fmt.Sprintf("%d:%d: define-syntax: expected syntax-rules or lambda", node.line, node.col)}
	}

	// Check if it's a lambda transformer (syntax-case style)
	if srNode.children[0].isAtom && srNode.children[0].tok.kind == tokSymbol && srNode.children[0].tok.sval == "lambda" {
		transformer, err := eval(srNode, e, ip)
		if err != nil {
			return nil, err
		}
		macro := &syntaxRulesMacro{defEnv: e, transformer: transformer}
		e.set(name, &Value{typ: valMacro, macro: macro})
		return voidVal(), nil
	}

	if !(srNode.children[0].isAtom && srNode.children[0].tok.kind == tokSymbol && srNode.children[0].tok.sval == "syntax-rules") {
		return nil, &EvalError{Message: fmt.Sprintf("%d:%d: define-syntax: expected syntax-rules or lambda", node.line, node.col)}
	}

	litNode := srNode.children[1]
	var literals []string
	if !litNode.isAtom {
		for _, lit := range litNode.children {
			if lit.isAtom && lit.tok.kind == tokSymbol {
				literals = append(literals, lit.tok.sval)
			}
		}
	}

	var clauses []macroClause
	for _, clauseNode := range srNode.children[2:] {
		if clauseNode.isAtom || len(clauseNode.children) != 2 {
			return nil, &EvalError{Message: fmt.Sprintf("%d:%d: define-syntax: bad clause", node.line, node.col)}
		}
		clauses = append(clauses, macroClause{
			pattern:  clauseNode.children[0],
			template: clauseNode.children[1],
		})
	}

	macro := &syntaxRulesMacro{
		literals: literals,
		clauses:  clauses,
		defEnv:   e,
	}

	e.set(name, &Value{typ: valMacro, macro: macro})
	return voidVal(), nil
}

func expandMacro(macro *syntaxRulesMacro, node *astNode, useEnv *env, ip *interp) (*astNode, *env, error) {
	if macro.transformer != nil {
		return expandTransformerMacro(macro, node, useEnv, ip)
	}
	for _, clause := range macro.clauses {
		bindings := make(map[string]*patBinding)
		if matchClause(clause.pattern, node, bindings, macro.literals) {
			// Collect template symbols that aren't pattern vars
			templateSyms := collectNonPatternSymbols(clause.template, bindings)

			// Build rename map (skip special forms and ...)
			renameMap := make(map[string]string)
			for _, sym := range templateSyms {
				if !specialForms[sym] {
					renameMap[sym] = gensym(sym)
				}
			}

			// Expand template with substitution and renaming
			expanded := expandTemplate(clause.template, bindings, renameMap)

			// Create wrapper env for hygiene: bind gensyms to definition-site values
			wrapperEnv := newEnv(useEnv)
			for origName, gsName := range renameMap {
				if v, ok := macro.defEnv.get(origName); ok {
					wrapperEnv.set(gsName, v)
				}
			}

			return expanded, wrapperEnv, nil
		}
	}
	return nil, nil, &EvalError{Message: fmt.Sprintf("%d:%d: no matching syntax-rules pattern", node.line, node.col)}
}

func matchClause(pattern *astNode, input *astNode, bindings map[string]*patBinding, literals []string) bool {
	if pattern.isAtom || input.isAtom {
		return false
	}
	// Skip first element (macro keyword) in both pattern and input
	patElems := pattern.children[1:]
	inputElems := input.children[1:]
	return matchElements(patElems, inputElems, bindings, literals)
}

func matchElements(patElems, inputElems []*astNode, bindings map[string]*patBinding, literals []string) bool {
	// Find ellipsis position
	ellipsisIdx := -1
	for i, pe := range patElems {
		if pe.isAtom && pe.tok.kind == tokSymbol && pe.tok.sval == "..." {
			ellipsisIdx = i
			break
		}
	}

	if ellipsisIdx < 0 {
		// No ellipsis: exact count match
		if len(patElems) != len(inputElems) {
			return false
		}
		for i, pe := range patElems {
			if !matchPatternNode(pe, inputElems[i], bindings, literals) {
				return false
			}
		}
		return true
	}

	// Ellipsis at ellipsisIdx; repeating pattern is at ellipsisIdx-1
	if ellipsisIdx == 0 {
		return false
	}
	repeatingIdx := ellipsisIdx - 1
	beforeCount := repeatingIdx
	afterElems := patElems[ellipsisIdx+1:]
	afterCount := len(afterElems)
	minInput := beforeCount + afterCount

	if len(inputElems) < minInput {
		return false
	}

	// Match elements before the ellipsis pattern
	for i := 0; i < beforeCount; i++ {
		if !matchPatternNode(patElems[i], inputElems[i], bindings, literals) {
			return false
		}
	}

	// Match repeating elements
	repeatCount := len(inputElems) - minInput
	repeatingPat := patElems[repeatingIdx]
	repVars := collectPatVars(repeatingPat, literals)
	for _, v := range repVars {
		bindings[v] = &patBinding{isEllipsis: true}
	}
	for i := 0; i < repeatCount; i++ {
		sub := make(map[string]*patBinding)
		if !matchPatternNode(repeatingPat, inputElems[beforeCount+i], sub, literals) {
			return false
		}
		for _, v := range repVars {
			if sb, ok := sub[v]; ok {
				bindings[v].multi = append(bindings[v].multi, sb.single)
			}
		}
	}

	// Match elements after ellipsis
	afterStart := beforeCount + repeatCount
	for i, pe := range afterElems {
		if !matchPatternNode(pe, inputElems[afterStart+i], bindings, literals) {
			return false
		}
	}

	return true
}

func matchPatternNode(pattern *astNode, input *astNode, bindings map[string]*patBinding, literals []string) bool {
	if pattern.isAtom {
		if pattern.tok.kind == tokSymbol {
			name := pattern.tok.sval
			if name == "_" {
				return true
			}
			// Check if it's a literal keyword
			for _, lit := range literals {
				if name == lit {
					return input.isAtom && input.tok.kind == tokSymbol && input.tok.sval == name
				}
			}
			// Pattern variable — binds to the input
			bindings[name] = &patBinding{single: input}
			return true
		}
		// Non-symbol atom: must match exactly
		if !input.isAtom {
			return false
		}
		return tokensMatch(pattern.tok, input.tok)
	}

	// Pattern is a list
	if input.isAtom {
		return false
	}
	return matchElements(pattern.children, input.children, bindings, literals)
}

func tokensMatch(a, b token) bool {
	if a.kind != b.kind {
		return false
	}
	switch a.kind {
	case tokNumber:
		return a.ival == b.ival
	case tokBool:
		return a.bval == b.bval
	case tokString:
		return a.sval == b.sval
	case tokSymbol:
		return a.sval == b.sval
	case tokChar:
		return a.ival == b.ival
	default:
		return false
	}
}

func expandTemplate(tmpl *astNode, bindings map[string]*patBinding, renameMap map[string]string) *astNode {
	if tmpl.isAtom {
		if tmpl.tok.kind == tokSymbol {
			name := tmpl.tok.sval
			// Pattern variable (single)?
			if b, ok := bindings[name]; ok && !b.isEllipsis {
				return b.single
			}
			// Rename for hygiene?
			if newName, ok := renameMap[name]; ok {
				return &astNode{
					isAtom: true,
					tok:    token{kind: tokSymbol, sval: newName, line: tmpl.tok.line, col: tmpl.tok.col},
					line:   tmpl.line,
					col:    tmpl.col,
				}
			}
		}
		return tmpl
	}

	// List template
	var children []*astNode
	for i := 0; i < len(tmpl.children); i++ {
		child := tmpl.children[i]
		// Check if next element is "..."
		if i+1 < len(tmpl.children) {
			next := tmpl.children[i+1]
			if next.isAtom && next.tok.kind == tokSymbol && next.tok.sval == "..." {
				// Ellipsis expansion
				evars := findEllipsisVars(child, bindings)
				if len(evars) > 0 {
					count := len(bindings[evars[0]].multi)
					for j := 0; j < count; j++ {
						sub := copyBindings(bindings)
						for _, ev := range evars {
							sub[ev] = &patBinding{single: bindings[ev].multi[j]}
						}
						children = append(children, expandTemplate(child, sub, renameMap))
					}
				}
				i++ // skip "..."
				continue
			}
		}
		children = append(children, expandTemplate(child, bindings, renameMap))
	}

	return &astNode{
		children: children,
		line:     tmpl.line,
		col:      tmpl.col,
	}
}

func copyBindings(bindings map[string]*patBinding) map[string]*patBinding {
	cp := make(map[string]*patBinding, len(bindings))
	for k, v := range bindings {
		cp[k] = v
	}
	return cp
}

func findEllipsisVars(node *astNode, bindings map[string]*patBinding) []string {
	var vars []string
	seen := make(map[string]bool)
	findEllipsisVarsHelper(node, bindings, seen, &vars)
	return vars
}

func findEllipsisVarsHelper(node *astNode, bindings map[string]*patBinding, seen map[string]bool, vars *[]string) {
	if node.isAtom {
		if node.tok.kind == tokSymbol {
			name := node.tok.sval
			if b, ok := bindings[name]; ok && b.isEllipsis && !seen[name] {
				seen[name] = true
				*vars = append(*vars, name)
			}
		}
		return
	}
	for _, child := range node.children {
		findEllipsisVarsHelper(child, bindings, seen, vars)
	}
}

func collectPatVars(node *astNode, literals []string) []string {
	var vars []string
	seen := make(map[string]bool)
	collectPatVarsHelper(node, literals, seen, &vars)
	return vars
}

func collectPatVarsHelper(node *astNode, literals []string, seen map[string]bool, vars *[]string) {
	if node.isAtom {
		if node.tok.kind == tokSymbol {
			name := node.tok.sval
			if name == "..." || name == "_" {
				return
			}
			for _, lit := range literals {
				if name == lit {
					return
				}
			}
			if !seen[name] {
				seen[name] = true
				*vars = append(*vars, name)
			}
		}
		return
	}
	for _, child := range node.children {
		collectPatVarsHelper(child, literals, seen, vars)
	}
}

func collectNonPatternSymbols(node *astNode, bindings map[string]*patBinding) []string {
	seen := make(map[string]bool)
	collectNonPatternSymbolsHelper(node, bindings, seen)
	var result []string
	for s := range seen {
		result = append(result, s)
	}
	return result
}

func collectNonPatternSymbolsHelper(node *astNode, bindings map[string]*patBinding, seen map[string]bool) {
	if node.isAtom {
		if node.tok.kind == tokSymbol {
			name := node.tok.sval
			if _, ok := bindings[name]; !ok && name != "..." {
				seen[name] = true
			}
		}
		return
	}
	// Skip non-pattern symbols inside (quote ...) — they're data, not code
	if len(node.children) >= 2 && node.children[0].isAtom &&
		node.children[0].tok.kind == tokSymbol && node.children[0].tok.sval == "quote" {
		return
	}
	for _, child := range node.children {
		collectNonPatternSymbolsHelper(child, bindings, seen)
	}
}
