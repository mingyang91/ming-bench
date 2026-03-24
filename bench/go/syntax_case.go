package ming

import "fmt"

// expandTransformerMacro expands a lambda-based (syntax-case) macro.
func expandTransformerMacro(macro *syntaxRulesMacro, node *astNode, useEnv *env, ip *interp) (*astNode, *env, error) {
	stx := &Value{typ: valSyntax, syntaxNode: node}

	// Save/restore macro definition env on interp for hygiene in syntax templates
	prevDefEnv := ip.macroDefEnv
	ip.macroDefEnv = macro.defEnv
	defer func() { ip.macroDefEnv = prevDefEnv }()

	result, err := applyLambdaFull(macro.transformer, []*Value{stx}, node, ip)
	if err != nil {
		return nil, nil, err
	}

	if result.typ != valSyntax {
		return nil, nil, &EvalError{Message: fmt.Sprintf("%d:%d: syntax-case transformer must return a syntax object", node.line, node.col)}
	}

	// Bind gensyms for hygiene directly in use-site env
	// (not a wrapper env, so top-level defines from macro expansions
	// are visible at the use site)
	if result.syntaxRenames != nil && result.syntaxDefEnv != nil {
		for origName, gsName := range result.syntaxRenames {
			if v, ok := result.syntaxDefEnv.get(origName); ok {
				useEnv.set(gsName, v)
			}
		}
	}

	return result.syntaxNode, useEnv, nil
}

// evalSyntaxCase implements (syntax-case expr (literals...) clause ...)
func evalSyntaxCase(node *astNode, e *env, ip *interp) (*Value, error) {
	if len(node.children) < 4 {
		return nil, &EvalError{Message: fmt.Sprintf("%d:%d: syntax-case: bad syntax", node.line, node.col)}
	}

	// Evaluate the expression to get a syntax object
	stxVal, err := eval(node.children[1], e, ip)
	if err != nil {
		return nil, err
	}

	var inputNode *astNode
	if stxVal.typ == valSyntax {
		inputNode = stxVal.syntaxNode
	} else {
		return nil, &EvalError{Message: fmt.Sprintf("%d:%d: syntax-case: expected syntax object", node.line, node.col)}
	}

	// Parse literals list
	litNode := node.children[2]
	var literals []string
	if !litNode.isAtom {
		for _, lit := range litNode.children {
			if lit.isAtom && lit.tok.kind == tokSymbol {
				literals = append(literals, lit.tok.sval)
			}
		}
	}

	// Try each clause
	for _, clauseNode := range node.children[3:] {
		if clauseNode.isAtom || len(clauseNode.children) < 2 || len(clauseNode.children) > 3 {
			return nil, &EvalError{Message: fmt.Sprintf("%d:%d: syntax-case: bad clause", node.line, node.col)}
		}

		pattern := clauseNode.children[0]
		var fender, body *astNode
		if len(clauseNode.children) == 3 {
			fender = clauseNode.children[1]
			body = clauseNode.children[2]
		} else {
			body = clauseNode.children[1]
		}

		bindings := make(map[string]*patBinding)
		if matchClause(pattern, inputNode, bindings, literals) {
			// Create new env with pattern variables bound as syntax objects
			caseEnv := newEnv(e)
			for name, binding := range bindings {
				if binding.isEllipsis {
					caseEnv.set(name, &Value{typ: valSyntax, syntaxMulti: binding.multi})
				} else {
					caseEnv.set(name, &Value{typ: valSyntax, syntaxNode: binding.single})
				}
			}

			// Check fender if present
			if fender != nil {
				fResult, err := eval(fender, caseEnv, ip)
				if err != nil {
					return nil, err
				}
				if !isTruthy(fResult) {
					continue
				}
			}

			// Evaluate body in the case env
			return eval(body, caseEnv, ip)
		}
	}

	return nil, &EvalError{Message: fmt.Sprintf("%d:%d: syntax-case: no matching pattern", node.line, node.col)}
}

// evalSyntaxTemplate implements (syntax template) / #'template
func evalSyntaxTemplate(node *astNode, e *env, ip *interp) (*Value, error) {
	if len(node.children) != 2 {
		return nil, &EvalError{Message: fmt.Sprintf("%d:%d: syntax: bad syntax", node.line, node.col)}
	}

	template := node.children[1]

	// If template is a single symbol that's a pattern variable, return it directly
	if template.isAtom && template.tok.kind == tokSymbol {
		if v, ok := e.get(template.tok.sval); ok && v.typ == valSyntax {
			return v, nil
		}
	}

	// Collect pattern variable bindings by scanning the template for symbols
	// that resolve to valSyntax in the environment
	bindings := make(map[string]*patBinding)
	collectSyntaxBindings(template, e, bindings)

	// Collect non-pattern symbols for hygiene renaming
	templateSyms := collectNonPatternSymbols(template, bindings)

	renameMap := make(map[string]string)
	for _, sym := range templateSyms {
		if !specialForms[sym] {
			renameMap[sym] = gensym(sym)
		}
	}

	// Expand template with substitution and renaming
	expanded := expandTemplate(template, bindings, renameMap)

	// Determine definition env for hygiene
	defEnv := ip.macroDefEnv
	if defEnv == nil {
		defEnv = e
	}

	return &Value{
		typ:           valSyntax,
		syntaxNode:    expanded,
		syntaxRenames: renameMap,
		syntaxDefEnv:  defEnv,
	}, nil
}

// evalWithSyntax implements (with-syntax ((pattern expr) ...) body ...)
func evalWithSyntax(node *astNode, e *env, ip *interp) (*Value, error) {
	if len(node.children) < 3 {
		return nil, &EvalError{Message: fmt.Sprintf("%d:%d: with-syntax: bad syntax", node.line, node.col)}
	}

	bindingsNode := node.children[1]
	if bindingsNode.isAtom {
		return nil, &EvalError{Message: fmt.Sprintf("%d:%d: with-syntax: expected bindings list", node.line, node.col)}
	}

	wsEnv := newEnv(e)

	for _, bindNode := range bindingsNode.children {
		if bindNode.isAtom || len(bindNode.children) != 2 {
			return nil, &EvalError{Message: fmt.Sprintf("%d:%d: with-syntax: bad binding", node.line, node.col)}
		}

		pattern := bindNode.children[0]
		exprNode := bindNode.children[1]

		// Evaluate the expression
		val, err := eval(exprNode, wsEnv, ip)
		if err != nil {
			return nil, err
		}

		if pattern.isAtom && pattern.tok.kind == tokSymbol {
			// Simple binding: pattern is just a name
			if val.typ == valSyntax {
				wsEnv.set(pattern.tok.sval, val)
			} else {
				// Wrap non-syntax value as syntax
				astN := valueToAstNode(val)
				wsEnv.set(pattern.tok.sval, &Value{typ: valSyntax, syntaxNode: astN})
			}
		} else {
			// Pattern matching binding
			if val.typ != valSyntax {
				return nil, &EvalError{Message: fmt.Sprintf("%d:%d: with-syntax: expected syntax object", node.line, node.col)}
			}
			bindings := make(map[string]*patBinding)
			if !matchPatternNode(pattern, val.syntaxNode, bindings, nil) {
				return nil, &EvalError{Message: fmt.Sprintf("%d:%d: with-syntax: pattern match failed", node.line, node.col)}
			}
			for name, binding := range bindings {
				if binding.isEllipsis {
					wsEnv.set(name, &Value{typ: valSyntax, syntaxMulti: binding.multi})
				} else {
					wsEnv.set(name, &Value{typ: valSyntax, syntaxNode: binding.single})
				}
			}
		}
	}

	// Evaluate body expressions
	body := node.children[2:]
	for i, expr := range body {
		if i == len(body)-1 {
			return eval(expr, wsEnv, ip)
		}
		_, err := eval(expr, wsEnv, ip)
		if err != nil {
			return nil, err
		}
	}

	return voidVal(), nil
}

// collectSyntaxBindings walks the template and finds all symbols that resolve
// to valSyntax in the environment, building a patBinding map for template expansion.
func collectSyntaxBindings(template *astNode, e *env, bindings map[string]*patBinding) {
	if template.isAtom {
		if template.tok.kind == tokSymbol {
			name := template.tok.sval
			if _, already := bindings[name]; already {
				return
			}
			if v, ok := e.get(name); ok && v.typ == valSyntax {
				if v.syntaxMulti != nil {
					bindings[name] = &patBinding{isEllipsis: true, multi: v.syntaxMulti}
				} else {
					bindings[name] = &patBinding{single: v.syntaxNode}
				}
			}
		}
		return
	}
	for _, child := range template.children {
		collectSyntaxBindings(child, e, bindings)
	}
}

// syntaxToDatumValue converts an AST node to a scheme Value.
func syntaxToDatumValue(node *astNode) *Value {
	if node.isAtom {
		switch node.tok.kind {
		case tokNumber:
			return intVal(node.tok.ival)
		case tokFloat:
			return floatVal(node.tok.fval)
		case tokRational:
			return makeRational(node.tok.ival, node.tok.dval)
		case tokBool:
			return boolVal(node.tok.bval)
		case tokString:
			return strVal(node.tok.sval)
		case tokChar:
			return charVal(rune(node.tok.ival))
		case tokSymbol:
			return symVal(node.tok.sval)
		}
	}
	// Vector literal
	if node.isVector {
		elems := make([]*Value, len(node.children))
		for i, c := range node.children {
			elems[i] = syntaxToDatumValue(c)
		}
		return &Value{typ: valVector, recordFields: elems}
	}
	// List: convert to proper list
	if len(node.children) == 0 {
		return nilVal()
	}
	result := nilVal()
	for i := len(node.children) - 1; i >= 0; i-- {
		result = &Value{typ: valPair, car: syntaxToDatumValue(node.children[i]), cdr: result}
	}
	return result
}

// valueToAstNode converts a scheme Value back to an AST node.
func valueToAstNode(v *Value) *astNode {
	switch v.typ {
	case valInt:
		return &astNode{isAtom: true, tok: token{kind: tokNumber, ival: v.ival}}
	case valFloat:
		return &astNode{isAtom: true, tok: token{kind: tokFloat, fval: v.fval}}
	case valRational:
		return &astNode{isAtom: true, tok: token{kind: tokRational, ival: v.ival, dval: v.dval}}
	case valBool:
		return &astNode{isAtom: true, tok: token{kind: tokBool, bval: v.bval}}
	case valString:
		return &astNode{isAtom: true, tok: token{kind: tokString, sval: v.sval}}
	case valChar:
		return &astNode{isAtom: true, tok: token{kind: tokChar, ival: v.ival}}
	case valSymbol:
		return &astNode{isAtom: true, tok: token{kind: tokSymbol, sval: v.sval}}
	case valNil:
		return &astNode{} // empty list
	case valPair:
		var children []*astNode
		cur := v
		for cur.typ == valPair {
			children = append(children, valueToAstNode(cur.car))
			cur = cur.cdr
		}
		if cur.typ != valNil {
			// Improper list
			children = append(children, valueToAstNode(cur))
			return &astNode{children: children, hasDot: true, dotPos: len(children) - 1}
		}
		return &astNode{children: children}
	case valVector:
		children := make([]*astNode, len(v.recordFields))
		for i, el := range v.recordFields {
			children[i] = valueToAstNode(el)
		}
		return &astNode{children: children, isVector: true}
	case valSyntax:
		return v.syntaxNode
	default:
		return &astNode{isAtom: true, tok: token{kind: tokSymbol, sval: fmt.Sprintf("#<value:%d>", v.typ)}}
	}
}
