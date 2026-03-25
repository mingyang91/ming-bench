package ming

type procedureMacro struct {
	keyword       string
	proc          value
	definitionEnv *environment
}

type syntaxValue struct {
	expr node
}

type repeatedSyntaxValue struct {
	exprs []node
}

type syntaxPatternMatcher struct {
	keyword  string
	literals map[string]struct{}
}

type syntaxTemplateContext struct {
	bindings      *environment
	definitionEnv *environment
	introduced    map[string]string
	locals        map[string]struct{}
}

func (m *procedureMacro) expand(call listNode) (node, error) {
	result, err := runMacroTransformer(m.proc, []value{&syntaxValue{expr: cloneNode(call)}}, call.pos, m.definitionEnv)
	if err != nil {
		return nil, err
	}

	syntax, ok := result.(*syntaxValue)
	if !ok {
		return nil, errorAt(call.pos, "macro transformer must return syntax")
	}
	return cloneNode(syntax.expr), nil
}

func runMacroTransformer(proc value, args []value, pos sourcePos, definitionEnv *environment) (value, error) {
	switch proc := proc.(type) {
	case *closureValue:
		return applyTransformerClosure(proc, args, pos, definitionEnv)
	case *caseClosureValue:
		for _, clause := range proc.clauses {
			if closureAcceptsArgCount(clause, len(args)) {
				return applyTransformerClosure(clause, args, pos, definitionEnv)
			}
		}
		return nil, errorAt(pos, "no matching case-lambda clause for %d arguments", len(args))
	default:
		return runProcedureCall(proc, args, pos, definitionEnv.evalContext())
	}
}

func applyTransformerClosure(proc *closureValue, args []value, pos sourcePos, definitionEnv *environment) (value, error) {
	if !proc.hasRest && len(args) != len(proc.params) {
		return nil, errorAt(pos, "expected %d arguments, got %d", len(proc.params), len(args))
	}
	if proc.hasRest && len(args) < len(proc.params) {
		return nil, errorAt(pos, "expected at least %d arguments, got %d", len(proc.params), len(args))
	}

	callEnv := newEnvironment(proc.env)
	if definitionEnv == nil {
		definitionEnv = proc.env.syntaxDefinitionEnv()
	}
	if definitionEnv == nil {
		definitionEnv = proc.env
	}
	callEnv.syntaxDefEnv = definitionEnv

	for i, name := range proc.params {
		callEnv.define(name, args[i])
	}
	if proc.hasRest {
		callEnv.define(proc.restParam, makeList(copyValues(args[len(proc.params):])))
	}

	return runEvalSequence(proc.body, callEnv)
}

func (e *environment) syntaxDefinitionEnv() *environment {
	for current := e; current != nil; current = current.parent {
		if current.syntaxDefEnv != nil {
			return current.syntaxDefEnv
		}
	}
	return nil
}

func evalSyntaxForm(args []node, env *environment) (value, error) {
	if len(args) != 1 {
		return nil, &EvalError{Message: "syntax expects exactly 1 argument"}
	}

	ctx := &syntaxTemplateContext{
		bindings:      env,
		definitionEnv: env.syntaxDefinitionEnv(),
		introduced:    map[string]string{},
		locals:        collectTemplateLocals(args[0]),
	}
	instantiated, err := ctx.instantiate(args[0], -1)
	if err != nil {
		return nil, err
	}
	return &syntaxValue{expr: instantiated}, nil
}

func evalSyntaxCaseForm(args []node, env *environment) (value, error) {
	if len(args) < 3 {
		return nil, &EvalError{Message: "syntax-case expects an input, literal list, and at least 1 clause"}
	}

	input, err := eval(args[0], env)
	if err != nil {
		return nil, err
	}
	subject, err := expectSingleSyntaxObject(input, "syntax-case")
	if err != nil {
		return nil, err
	}

	literalList, ok := args[1].(listNode)
	if !ok {
		return nil, &EvalError{Message: "syntax-case literals must be a list"}
	}

	literals := map[string]struct{}{}
	for _, literal := range literalList.elements {
		name, ok := symbolName(literal)
		if !ok {
			return nil, &EvalError{Message: "syntax-case literals must be symbols"}
		}
		literals[name] = struct{}{}
	}

	matcher := &syntaxPatternMatcher{literals: literals}
	for _, clauseExpr := range args[2:] {
		clause, ok := clauseExpr.(listNode)
		if !ok || len(clause.elements) < 2 || len(clause.elements) > 3 {
			return nil, &EvalError{Message: "syntax-case clauses must contain a pattern, optional fender, and a body"}
		}

		match := newMacroMatch()
		if !matcher.matchPattern(clause.elements[0], subject.expr, match, false) {
			continue
		}

		clauseEnv := newEnvironment(env)
		bindSyntaxMatch(clauseEnv, match)

		bodyIndex := 1
		if len(clause.elements) == 3 {
			fender, err := eval(clause.elements[1], clauseEnv)
			if err != nil {
				return nil, err
			}
			if !isTruthy(fender) {
				continue
			}
			bodyIndex = 2
		}

		return eval(clause.elements[bodyIndex], clauseEnv)
	}

	return nil, &EvalError{Message: "syntax-case: no matching clause"}
}

func evalWithSyntaxForm(args []node, env *environment) (value, error) {
	if len(args) < 2 {
		return nil, &EvalError{Message: "with-syntax expects bindings and a body"}
	}

	bindingList, ok := args[0].(listNode)
	if !ok {
		return nil, &EvalError{Message: "with-syntax bindings must be a list"}
	}

	scopeEnv := newEnvironment(env)
	matcher := &syntaxPatternMatcher{}
	for _, bindingExpr := range bindingList.elements {
		binding, ok := bindingExpr.(listNode)
		if !ok || len(binding.elements) != 2 {
			return nil, &EvalError{Message: "with-syntax bindings must contain a pattern and expression"}
		}

		rhs, err := eval(binding.elements[1], scopeEnv)
		if err != nil {
			return nil, err
		}
		syntax, err := expectSingleSyntaxObject(rhs, "with-syntax")
		if err != nil {
			return nil, err
		}

		match := newMacroMatch()
		if !matcher.matchPattern(binding.elements[0], syntax.expr, match, false) {
			return nil, &EvalError{Message: "with-syntax pattern did not match"}
		}
		bindSyntaxMatch(scopeEnv, match)
	}

	return evalSequence(args[1:], scopeEnv)
}

func (m *syntaxPatternMatcher) matchPattern(pattern node, expr node, captures *macroMatch, repeated bool) bool {
	switch pattern := pattern.(type) {
	case integerValue:
		other, ok := expr.(integerValue)
		return ok && other == pattern
	case rationalValue:
		other, ok := expr.(rationalValue)
		return ok && other == pattern
	case inexactValue:
		other, ok := expr.(inexactValue)
		return ok && other == pattern
	case booleanValue:
		other, ok := expr.(booleanValue)
		return ok && other == pattern
	case stringValue:
		other, ok := expr.(stringValue)
		return ok && other == pattern
	case charValue:
		other, ok := expr.(charValue)
		return ok && other == pattern
	case symbolNode:
		if pattern.name == "..." {
			return false
		}
		if pattern.name == "_" {
			return true
		}

		if m.isLiteral(pattern.name) {
			other, ok := expr.(symbolNode)
			return ok && freeIdentifierEqualNodes(pattern, other)
		}

		if repeated {
			captures.repeated[pattern.name] = append(captures.repeated[pattern.name], cloneNode(expr))
			return true
		}

		if existing, ok := captures.single[pattern.name]; ok {
			return syntaxEqual(existing, expr)
		}
		captures.single[pattern.name] = cloneNode(expr)
		return true
	case listNode:
		other, ok := expr.(listNode)
		if !ok {
			return false
		}
		return m.matchList(pattern.elements, other.elements, captures, repeated)
	case dottedListNode:
		other, ok := expr.(dottedListNode)
		if !ok || len(pattern.elements) != len(other.elements) {
			return false
		}
		for i := range pattern.elements {
			if !m.matchPattern(pattern.elements[i], other.elements[i], captures, repeated) {
				return false
			}
		}
		return m.matchPattern(pattern.tail, other.tail, captures, repeated)
	case vectorNode:
		other, ok := expr.(vectorNode)
		if !ok {
			return false
		}
		return m.matchList(pattern.elements, other.elements, captures, repeated)
	default:
		return false
	}
}

func (m *syntaxPatternMatcher) matchList(patternElems []node, exprElems []node, captures *macroMatch, repeated bool) bool {
	if len(patternElems) == 0 {
		return len(exprElems) == 0
	}

	if len(patternElems) >= 2 && isEllipsisNode(patternElems[1]) {
		subpattern := patternElems[0]
		tail := patternElems[2:]
		minTail := minPatternLength(tail)
		if len(exprElems) < minTail {
			return false
		}

		maxCount := len(exprElems) - minTail
		for count := 0; count <= maxCount; count++ {
			trial := captures.clone()
			m.seedRepeatedBindings(subpattern, trial)
			ok := true
			for i := 0; i < count; i++ {
				if !m.matchPattern(subpattern, exprElems[i], trial, true) {
					ok = false
					break
				}
			}
			if ok && m.matchList(tail, exprElems[count:], trial, repeated) {
				*captures = *trial
				return true
			}
		}
		return false
	}

	if len(exprElems) == 0 {
		return false
	}
	if !m.matchPattern(patternElems[0], exprElems[0], captures, repeated) {
		return false
	}
	return m.matchList(patternElems[1:], exprElems[1:], captures, repeated)
}

func (m *syntaxPatternMatcher) seedRepeatedBindings(pattern node, captures *macroMatch) {
	switch pattern := pattern.(type) {
	case symbolNode:
		if pattern.name == "..." || pattern.name == "_" || m.isLiteral(pattern.name) {
			return
		}
		if _, ok := captures.repeated[pattern.name]; !ok {
			captures.repeated[pattern.name] = nil
		}
	case listNode:
		for _, element := range pattern.elements {
			m.seedRepeatedBindings(element, captures)
		}
	case dottedListNode:
		for _, element := range pattern.elements {
			m.seedRepeatedBindings(element, captures)
		}
		m.seedRepeatedBindings(pattern.tail, captures)
	case vectorNode:
		for _, element := range pattern.elements {
			m.seedRepeatedBindings(element, captures)
		}
	}
}

func (m *syntaxPatternMatcher) isLiteral(name string) bool {
	if m.keyword != "" && name == m.keyword {
		return true
	}
	_, ok := m.literals[name]
	return ok
}

func bindSyntaxMatch(env *environment, match *macroMatch) {
	for name, expr := range match.single {
		env.define(name, &syntaxValue{expr: cloneNode(expr)})
	}
	for name, exprs := range match.repeated {
		bound := make([]node, len(exprs))
		for i, expr := range exprs {
			bound[i] = cloneNode(expr)
		}
		env.define(name, &repeatedSyntaxValue{exprs: bound})
	}
}

func (ctx *syntaxTemplateContext) instantiate(template node, repeatIndex int) (node, error) {
	switch template := template.(type) {
	case integerValue, rationalValue, inexactValue, booleanValue, stringValue, charValue:
		return template, nil
	case symbolNode:
		if template.name == "..." {
			return nil, &EvalError{Message: "invalid template ellipsis"}
		}

		if expr, ok := lookupSingleSyntaxBinding(ctx.bindings, template.name); ok {
			return cloneNode(expr), nil
		}

		if exprs, ok := lookupRepeatedSyntaxBinding(ctx.bindings, template.name); ok {
			if repeatIndex < 0 || repeatIndex >= len(exprs) {
				return nil, &EvalError{Message: "ellipsis variable used outside matching repetition"}
			}
			return cloneNode(exprs[repeatIndex]), nil
		}

		return ctx.introducedSymbol(template), nil
	case listNode:
		if isQuotedSyntaxForm(template) {
			return cloneNode(template), nil
		}

		elements := make([]node, 0, len(template.elements))
		for i := 0; i < len(template.elements); i++ {
			if i+1 < len(template.elements) && isEllipsisNode(template.elements[i+1]) {
				repeated, err := ctx.instantiateRepeated(template.elements[i])
				if err != nil {
					return nil, err
				}
				elements = append(elements, repeated...)
				i++
				continue
			}

			element, err := ctx.instantiate(template.elements[i], repeatIndex)
			if err != nil {
				return nil, err
			}
			elements = append(elements, element)
		}
		return listNode{elements: elements, pos: template.pos}, nil
	case dottedListNode:
		elements := make([]node, 0, len(template.elements))
		for i := 0; i < len(template.elements); i++ {
			if i+1 < len(template.elements) && isEllipsisNode(template.elements[i+1]) {
				repeated, err := ctx.instantiateRepeated(template.elements[i])
				if err != nil {
					return nil, err
				}
				elements = append(elements, repeated...)
				i++
				continue
			}

			element, err := ctx.instantiate(template.elements[i], repeatIndex)
			if err != nil {
				return nil, err
			}
			elements = append(elements, element)
		}
		tail, err := ctx.instantiate(template.tail, repeatIndex)
		if err != nil {
			return nil, err
		}
		return dottedListNode{elements: elements, tail: tail, pos: template.pos}, nil
	case vectorNode:
		elements := make([]node, 0, len(template.elements))
		for i := 0; i < len(template.elements); i++ {
			if i+1 < len(template.elements) && isEllipsisNode(template.elements[i+1]) {
				repeated, err := ctx.instantiateRepeated(template.elements[i])
				if err != nil {
					return nil, err
				}
				elements = append(elements, repeated...)
				i++
				continue
			}

			element, err := ctx.instantiate(template.elements[i], repeatIndex)
			if err != nil {
				return nil, err
			}
			elements = append(elements, element)
		}
		return vectorNode{elements: elements, pos: template.pos}, nil
	default:
		return nil, &EvalError{Message: "invalid syntax template"}
	}
}

func (ctx *syntaxTemplateContext) instantiateRepeated(template node) ([]node, error) {
	vars := map[string]struct{}{}
	collectRepeatedSyntaxVars(template, ctx.bindings, vars)
	if len(vars) == 0 {
		return nil, &EvalError{Message: "template ellipsis requires a repeated pattern variable"}
	}

	count := -1
	for name := range vars {
		current := len(mustLookupRepeatedSyntaxBinding(ctx.bindings, name))
		if count == -1 {
			count = current
			continue
		}
		if count != current {
			return nil, &EvalError{Message: "mismatched ellipsis lengths"}
		}
	}

	result := make([]node, 0, count)
	for i := 0; i < count; i++ {
		element, err := ctx.instantiate(template, i)
		if err != nil {
			return nil, err
		}
		result = append(result, element)
	}
	return result, nil
}

func (ctx *syntaxTemplateContext) introducedSymbol(sym symbolNode) symbolNode {
	if isCoreKeyword(sym.name) {
		return symbolNode{name: sym.name, pos: sym.pos}
	}

	if _, ok := ctx.locals[sym.name]; ok {
		renamed, ok := ctx.introduced[sym.name]
		if !ok {
			renamed = nextMacroName(ctx.bindings, sym.name)
			ctx.introduced[sym.name] = renamed
		}
		return symbolNode{name: renamed, pos: sym.pos}
	}

	definitionEnv := ctx.definitionEnv
	if definitionEnv == nil {
		definitionEnv = ctx.bindings
	}

	if _, ok := definitionEnv.lookupMacro(sym.name); ok {
		return symbolNode{name: sym.name, pos: sym.pos}
	}

	if binding, ok := definitionEnv.lookupBinding(sym.name); ok {
		switch binding.value.(type) {
		case *syntaxValue, *repeatedSyntaxValue:
		default:
			return symbolNode{name: sym.name, pos: sym.pos, captured: binding}
		}
	}

	return symbolNode{name: sym.name, pos: sym.pos}
}

func collectRepeatedSyntaxVars(expr node, env *environment, names map[string]struct{}) {
	switch expr := expr.(type) {
	case symbolNode:
		if _, ok := lookupRepeatedSyntaxBinding(env, expr.name); ok {
			names[expr.name] = struct{}{}
		}
	case listNode:
		for _, element := range expr.elements {
			collectRepeatedSyntaxVars(element, env, names)
		}
	case dottedListNode:
		for _, element := range expr.elements {
			collectRepeatedSyntaxVars(element, env, names)
		}
		collectRepeatedSyntaxVars(expr.tail, env, names)
	case vectorNode:
		for _, element := range expr.elements {
			collectRepeatedSyntaxVars(element, env, names)
		}
	}
}

func lookupSingleSyntaxBinding(env *environment, name string) (node, bool) {
	if env == nil {
		return nil, false
	}
	binding, ok := env.lookupBinding(name)
	if !ok {
		return nil, false
	}

	syntax, ok := binding.value.(*syntaxValue)
	if !ok {
		return nil, false
	}
	return syntax.expr, true
}

func lookupRepeatedSyntaxBinding(env *environment, name string) ([]node, bool) {
	if env == nil {
		return nil, false
	}
	binding, ok := env.lookupBinding(name)
	if !ok {
		return nil, false
	}

	repeated, ok := binding.value.(*repeatedSyntaxValue)
	if !ok {
		return nil, false
	}
	return repeated.exprs, true
}

func mustLookupRepeatedSyntaxBinding(env *environment, name string) []node {
	exprs, _ := lookupRepeatedSyntaxBinding(env, name)
	return exprs
}

func expectSingleSyntaxObject(v value, name string) (*syntaxValue, error) {
	syntax, ok := v.(*syntaxValue)
	if !ok {
		return nil, &EvalError{Message: name + " expects a syntax object"}
	}
	return syntax, nil
}

func expectSyntaxIdentifier(v value, name string) (symbolNode, error) {
	syntax, err := expectSingleSyntaxObject(v, name)
	if err != nil {
		return symbolNode{}, err
	}

	identifier, ok := syntax.expr.(symbolNode)
	if !ok {
		return symbolNode{}, &EvalError{Message: name + " expects identifier syntax"}
	}
	return identifier, nil
}

func builtinSyntaxToDatum() builtinProc {
	return func(args []value) (value, error) {
		if len(args) != 1 {
			return nil, &EvalError{Message: "syntax->datum expects exactly 1 argument"}
		}

		switch syntax := args[0].(type) {
		case *syntaxValue:
			return datumFromNode(syntax.expr)
		case *repeatedSyntaxValue:
			values := make([]value, len(syntax.exprs))
			for i, expr := range syntax.exprs {
				datum, err := datumFromNode(expr)
				if err != nil {
					return nil, err
				}
				values[i] = datum
			}
			return makeList(values), nil
		default:
			return nil, &EvalError{Message: "syntax->datum expects a syntax object"}
		}
	}
}

func builtinDatumToSyntax() builtinProc {
	return func(args []value) (value, error) {
		if len(args) != 2 {
			return nil, &EvalError{Message: "datum->syntax expects exactly 2 arguments"}
		}

		context, err := expectSingleSyntaxObject(args[0], "datum->syntax")
		if err != nil {
			return nil, err
		}

		expr, err := nodeFromDatum(args[1])
		if err != nil {
			return nil, err
		}
		return &syntaxValue{expr: applySyntaxContext(expr, context.expr)}, nil
	}
}

func builtinIdentifierPredicate() builtinProc {
	return func(args []value) (value, error) {
		if len(args) != 1 {
			return nil, &EvalError{Message: "identifier? expects exactly 1 argument"}
		}

		syntax, ok := args[0].(*syntaxValue)
		if !ok {
			return booleanValue(false), nil
		}
		_, ok = syntax.expr.(symbolNode)
		return booleanValue(ok), nil
	}
}

func builtinFreeIdentifierEqual() builtinProc {
	return func(args []value) (value, error) {
		if len(args) != 2 {
			return nil, &EvalError{Message: "free-identifier=? expects exactly 2 arguments"}
		}

		left, err := expectSyntaxIdentifier(args[0], "free-identifier=?")
		if err != nil {
			return nil, err
		}
		right, err := expectSyntaxIdentifier(args[1], "free-identifier=?")
		if err != nil {
			return nil, err
		}
		return booleanValue(freeIdentifierEqualNodes(left, right)), nil
	}
}

func freeIdentifierEqualNodes(left symbolNode, right symbolNode) bool {
	if left.name != right.name {
		return false
	}
	if left.captured != nil || right.captured != nil {
		return left.captured == right.captured
	}
	return true
}

func nodeFromDatum(v value) (node, error) {
	switch v := v.(type) {
	case integerValue, rationalValue, inexactValue, booleanValue, stringValue, charValue:
		return v, nil
	case *mutableStringValue:
		return stringValue(string(v.runes)), nil
	case symbolValue:
		return symbolNode{name: string(v)}, nil
	case *syntaxValue:
		return cloneNode(v.expr), nil
	case listValue:
		elements := make([]node, len(v.elements))
		for i, element := range v.elements {
			expr, err := nodeFromDatum(element)
			if err != nil {
				return nil, err
			}
			elements[i] = expr
		}
		return listNode{elements: elements}, nil
	case *pairValue:
		return nodeFromPairDatum(v)
	case *vectorValue:
		elements := make([]node, len(v.elements))
		for i, element := range v.elements {
			expr, err := nodeFromDatum(element)
			if err != nil {
				return nil, err
			}
			elements[i] = expr
		}
		return vectorNode{elements: elements}, nil
	default:
		return nil, &EvalError{Message: "datum->syntax expects a datum"}
	}
}

func nodeFromPairDatum(pair *pairValue) (node, error) {
	elements := []node{}
	current := value(pair)
	seen := map[*pairValue]struct{}{}

	for {
		switch pair := current.(type) {
		case *pairValue:
			if _, ok := seen[pair]; ok {
				return nil, &EvalError{Message: "datum->syntax cannot convert a cyclic pair"}
			}
			seen[pair] = struct{}{}

			expr, err := nodeFromDatum(pair.car)
			if err != nil {
				return nil, err
			}
			elements = append(elements, expr)
			current = pair.cdr
		case listValue:
			if len(pair.elements) == 0 {
				return listNode{elements: elements}, nil
			}
			for _, element := range pair.elements {
				expr, err := nodeFromDatum(element)
				if err != nil {
					return nil, err
				}
				elements = append(elements, expr)
			}
			return listNode{elements: elements}, nil
		default:
			tail, err := nodeFromDatum(current)
			if err != nil {
				return nil, err
			}
			return dottedListNode{elements: elements, tail: tail}, nil
		}
	}
}

func applySyntaxContext(expr node, context node) node {
	switch expr := expr.(type) {
	case symbolNode:
		if contextIdentifier, ok := context.(symbolNode); ok {
			expr.captured = contextIdentifier.captured
		}
		return expr
	case listNode:
		elements := make([]node, len(expr.elements))
		for i, element := range expr.elements {
			elements[i] = applySyntaxContext(element, context)
		}
		return listNode{elements: elements, pos: expr.pos}
	case dottedListNode:
		elements := make([]node, len(expr.elements))
		for i, element := range expr.elements {
			elements[i] = applySyntaxContext(element, context)
		}
		return dottedListNode{
			elements: elements,
			tail:     applySyntaxContext(expr.tail, context),
			pos:      expr.pos,
		}
	case vectorNode:
		elements := make([]node, len(expr.elements))
		for i, element := range expr.elements {
			elements[i] = applySyntaxContext(element, context)
		}
		return vectorNode{elements: elements, pos: expr.pos}
	default:
		return expr
	}
}
