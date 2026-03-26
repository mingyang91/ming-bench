package ming

import (
	"fmt"
)

const level18ApplyCPSKey = "__level18_apply_cps__"

type continuationProc struct {
	target  any
	dynamic *dynamicWindFrame
}

type callCCProc struct{}

type continuationTransfer struct {
	value any
	next  *tailEvalState
}

func level18UsesCPS() bool {
	level, ok := activeBenchLevel()
	return ok && level >= 18
}

func predeclareLevel18TopLevelDefines(scope *env, exprs []any) error {
	for _, expr := range exprs {
		list, ok := expr.(listExpr)
		if !ok || len(list.elements) == 0 {
			continue
		}

		head, ok := list.elements[0].(symbolExpr)
		if !ok || head.name != "define" {
			continue
		}

		if len(list.elements) < 3 {
			return nil
		}

		switch target := list.elements[1].(type) {
		case symbolExpr:
			scope.defineSymbol(target, uninitializedValue{})
		case listExpr:
			if len(target.elements) == 0 {
				return exprSourcePos(expr).errorf("define function form requires a name")
			}

			name, ok := target.elements[0].(symbolExpr)
			if !ok {
				return exprSourcePos(target.elements[0]).errorf("define function name must be a symbol")
			}
			scope.defineSymbol(name, uninitializedValue{})
		}
	}

	return nil
}

func normalizeLevel18TopLevelExprs(exprs []any) ([]any, error) {
	normalized := make([]any, len(exprs))
	for i, expr := range exprs {
		next, err := normalizeLevel18TopLevelExpr(expr)
		if err != nil {
			return nil, err
		}
		normalized[i] = next
	}
	return normalized, nil
}

func normalizeLevel18TopLevelExpr(expr any) (any, error) {
	list, ok := expr.(listExpr)
	if !ok || len(list.elements) == 0 {
		return expr, nil
	}

	head, ok := list.elements[0].(symbolExpr)
	if !ok || head.name != "define" {
		return expr, nil
	}

	args := list.elements[1:]
	if len(args) < 2 {
		return nil, exprSourcePos(expr).errorf("define expects a name and value")
	}

	switch target := args[0].(type) {
	case symbolExpr:
		if len(args) != 2 {
			return nil, exprSourcePos(expr).errorf("define variable form expects exactly 2 arguments")
		}
		return listExpr{
			elements: []any{
				symbolExpr{name: "set!", pos: head.pos},
				target,
				args[1],
			},
			pos: list.pos,
		}, nil
	case listExpr:
		if len(target.elements) == 0 {
			return nil, exprSourcePos(expr).errorf("define function form requires a name")
		}

		name, ok := target.elements[0].(symbolExpr)
		if !ok {
			return nil, exprSourcePos(target.elements[0]).errorf("define function name must be a symbol")
		}

		lambdaElements := make([]any, 0, len(args)+2)
		lambdaElements = append(lambdaElements, symbolExpr{name: "lambda", pos: head.pos})
		lambdaElements = append(lambdaElements, listExpr{elements: append([]any(nil), target.elements[1:]...), pos: target.pos})
		lambdaElements = append(lambdaElements, args[1:]...)

		return listExpr{
			elements: []any{
				symbolExpr{name: "set!", pos: head.pos},
				name,
				listExpr{elements: lambdaElements, pos: list.pos},
			},
			pos: list.pos,
		}, nil
	default:
		return nil, exprSourcePos(expr).errorf("define requires a symbol or function signature")
	}
}

func programUsesDynamicControl(exprs []any) bool {
	for _, expr := range exprs {
		if exprUsesDynamicControl(expr) {
			return true
		}
	}
	return false
}

func exprUsesDynamicControl(expr any) bool {
	switch node := expr.(type) {
	case symbolExpr:
		return node.name == "call/cc" || node.name == "call-with-current-continuation" || node.name == "dynamic-wind"
	case listExpr:
		if len(node.elements) == 0 {
			return false
		}

		if head, ok := node.elements[0].(symbolExpr); ok && head.name == "quote" {
			return false
		}

		for _, elem := range node.elements {
			if exprUsesDynamicControl(elem) {
				return true
			}
		}
	}

	return false
}

func builtinApplyCPS(runtime *runtimeState, args []any) (any, error) {
	if len(args) < 2 {
		return nil, &EvalError{Message: "__apply_cps expects a procedure and a continuation"}
	}

	proc := args[0]
	k := args[len(args)-1]
	callArgs := append([]any(nil), args[1:len(args)-1]...)
	return applyCPS(proc, callArgs, k, runtime)
}

func applyCPS(proc any, args []any, k any, runtime *runtimeState) (any, error) {
	value, next, err := prepareApplyCPSCall(proc, args, k, runtime)
	if err != nil {
		return nil, err
	}
	if next != nil {
		return eval(next.scope, next.expr)
	}
	return value, nil
}

func evalCPSProgram(scope *env, expr any) (result any, err error) {
	currentScope := scope
	currentExpr := expr

	for {
		transferred := false
		var transfer continuationTransfer

		func() {
			defer func() {
				if recovered := recover(); recovered != nil {
					signal, ok := recovered.(continuationTransfer)
					if !ok {
						panic(recovered)
					}
					transfer = signal
					transferred = true
				}
			}()

			result, err = eval(currentScope, currentExpr)
		}()

		if err != nil {
			return nil, err
		}
		if !transferred {
			return result, nil
		}
		if transfer.next == nil {
			return transfer.value, nil
		}

		currentScope = transfer.next.scope
		currentExpr = transfer.next.expr
	}
}

func prepareApplyCPSCall(proc any, args []any, k any, runtime *runtimeState) (any, *tailEvalState, error) {
	switch callable := proc.(type) {
	case builtinProc:
		if callable.name == "apply" {
			return prepareBuiltinApplyRuntimeCPS(args, k, runtime)
		}

		value, err := callable.fn(args)
		if err != nil {
			return nil, nil, err
		}
		return prepareProcedureCall(k, []any{value})
	case closure:
		callArgs := make([]any, 0, len(args)+1)
		callArgs = append(callArgs, k)
		callArgs = append(callArgs, args...)
		return prepareProcedureCall(callable, callArgs)
	case caseClosure:
		callArgs := make([]any, 0, len(args)+1)
		callArgs = append(callArgs, k)
		callArgs = append(callArgs, args...)
		return prepareProcedureCall(callable, callArgs)
	case continuationProc:
		if len(args) != 1 {
			return nil, nil, &EvalError{Message: "continuation expects exactly 1 argument"}
		}
		value, next, err := prepareContinuationJump(runtime, callable, args[0])
		if err != nil {
			return nil, nil, err
		}
		panic(continuationTransfer{value: value, next: next})
	case callCCProc:
		if len(args) != 1 {
			return nil, nil, &EvalError{Message: "call/cc expects exactly 1 argument"}
		}

		return prepareApplyCPSCall(args[0], []any{continuationProc{target: k, dynamic: runtime.dynamic}}, k, runtime)
	default:
		return nil, nil, &EvalError{Message: fmt.Sprintf("expected procedure, got %s", typeName(proc))}
	}
}

func builtinApplyRuntimeCPS(args []any, k any, runtime *runtimeState) (any, error) {
	value, next, err := prepareBuiltinApplyRuntimeCPS(args, k, runtime)
	if err != nil {
		return nil, err
	}
	if next != nil {
		return eval(next.scope, next.expr)
	}
	return value, nil
}

func prepareBuiltinApplyRuntimeCPS(args []any, k any, runtime *runtimeState) (any, *tailEvalState, error) {
	if len(args) < 2 {
		return nil, nil, &EvalError{Message: "apply expects at least 2 arguments"}
	}

	restArgs, err := properListElements(args[len(args)-1], "apply")
	if err != nil {
		return nil, nil, err
	}

	callArgs := make([]any, 0, len(args)-2+len(restArgs))
	callArgs = append(callArgs, args[1:len(args)-1]...)
	callArgs = append(callArgs, restArgs...)

	return prepareApplyCPSCall(args[0], callArgs, k, runtime)
}

func evalLevel18ApplyCPSTail(scope *env, args []any) (any, *tailEvalState, error) {
	if len(args) < 2 {
		return nil, nil, &EvalError{Message: "__apply_cps expects a procedure and a continuation"}
	}

	proc, err := eval(scope, args[0])
	if err != nil {
		return nil, nil, err
	}

	callArgs := make([]any, 0, len(args)-2)
	for _, argExpr := range args[1 : len(args)-1] {
		value, err := eval(scope, argExpr)
		if err != nil {
			return nil, nil, err
		}
		callArgs = append(callArgs, value)
	}

	k, err := eval(scope, args[len(args)-1])
	if err != nil {
		return nil, nil, err
	}

	return prepareApplyCPSCall(proc, callArgs, k, scope.runtime)
}

type level18CPSTransformer struct {
	counter int
}

func transformLevel18Program(exprs []any) (any, error) {
	transformer := &level18CPSTransformer{}
	resultParam := transformer.freshSymbol("result")
	return transformer.cpsSequence(exprs, transformer.directLambda([]symbolExpr{resultParam}, resultParam))
}

func (t *level18CPSTransformer) cpsSequence(exprs []any, k any) (any, error) {
	if len(exprs) == 0 {
		return t.applyCont(k, t.voidExpr()), nil
	}
	if len(exprs) == 1 {
		return t.cpsExpr(exprs[0], k)
	}

	ignored := t.freshSymbol("ignored")
	next, err := t.cpsSequence(exprs[1:], k)
	if err != nil {
		return nil, err
	}
	return t.cpsExpr(exprs[0], t.directLambda([]symbolExpr{ignored}, next))
}

func (t *level18CPSTransformer) cpsExpr(expr any, k any) (any, error) {
	switch node := expr.(type) {
	case int64, rationalValue, float64, bool, stringExpr, charValue, symbolExpr:
		return t.applyCont(k, node), nil
	case listExpr:
		if len(node.elements) == 0 {
			return nil, node.pos.errorf("cannot evaluate empty list")
		}

		if head, ok := node.elements[0].(symbolExpr); ok {
			args := node.elements[1:]
			switch head.name {
			case "quote":
				if len(args) != 1 {
					return nil, head.pos.errorf("quote expects exactly 1 argument")
				}
				return t.applyCont(k, node), nil
			case "lambda":
				lambdaExpr, err := t.transformLambdaExpr(args, node.pos)
				if err != nil {
					return nil, err
				}
				return t.applyCont(k, lambdaExpr), nil
			case "case-lambda":
				caseLambdaExpr, err := t.transformCaseLambdaExpr(args, node.pos)
				if err != nil {
					return nil, err
				}
				return t.applyCont(k, caseLambdaExpr), nil
			case "if":
				return t.cpsIf(args, k, node.pos)
			case "begin":
				return t.cpsSequence(args, k)
			case "dynamic-wind":
				return t.cpsDynamicWind(args, k, node.pos)
			case "define":
				return t.cpsDefine(args, k, node.pos)
			case "set!":
				return t.cpsSet(args, k, node.pos)
			case "let":
				desugared, err := t.desugarLet(args, node.pos)
				if err != nil {
					return nil, err
				}
				return t.cpsExpr(desugared, k)
			case "let*":
				desugared, err := t.desugarLetStar(args, node.pos)
				if err != nil {
					return nil, err
				}
				return t.cpsExpr(desugared, k)
			case "letrec":
				desugared, err := t.desugarLetrec(args, node.pos, false)
				if err != nil {
					return nil, err
				}
				return t.cpsExpr(desugared, k)
			case "letrec*":
				desugared, err := t.desugarLetrec(args, node.pos, true)
				if err != nil {
					return nil, err
				}
				return t.cpsExpr(desugared, k)
			case "cond":
				desugared, err := t.desugarCond(args, node.pos)
				if err != nil {
					return nil, err
				}
				return t.cpsExpr(desugared, k)
			case "and":
				desugared, err := t.desugarAnd(args, node.pos)
				if err != nil {
					return nil, err
				}
				return t.cpsExpr(desugared, k)
			case "or":
				desugared, err := t.desugarOr(args, node.pos)
				if err != nil {
					return nil, err
				}
				return t.cpsExpr(desugared, k)
			case "case":
				desugared, err := t.desugarCase(args, node.pos)
				if err != nil {
					return nil, err
				}
				return t.cpsExpr(desugared, k)
			case "do", "define-syntax", "define-record-type":
				return nil, head.pos.errorf("%s is not supported with level 18 continuations", head.name)
			}
		}

		return t.cpsApplication(node, k)
	default:
		return t.applyCont(k, node), nil
	}
}

func (t *level18CPSTransformer) cpsIf(args []any, k any, pos sourcePos) (any, error) {
	if len(args) != 2 && len(args) != 3 {
		return nil, pos.errorf("if expects 2 or 3 arguments")
	}

	testValue := t.freshSymbol("if_test")
	thenExpr, err := t.cpsExpr(args[1], k)
	if err != nil {
		return nil, err
	}

	elseSource := t.voidExpr()
	if len(args) == 3 {
		elseSource = args[2]
	}
	elseExpr, err := t.cpsExpr(elseSource, k)
	if err != nil {
		return nil, err
	}

	return t.cpsExpr(args[0], t.directLambda([]symbolExpr{testValue}, t.list(
		symbolExpr{name: "if", pos: pos},
		testValue,
		thenExpr,
		elseExpr,
	)))
}

func (t *level18CPSTransformer) cpsDefine(args []any, k any, pos sourcePos) (any, error) {
	if len(args) < 2 {
		return nil, pos.errorf("define expects a name and value")
	}

	switch target := args[0].(type) {
	case symbolExpr:
		if len(args) != 2 {
			return nil, pos.errorf("define variable form expects exactly 2 arguments")
		}

		valueSym := t.freshSymbol("define_value")
		return t.cpsExpr(args[1], t.directLambda([]symbolExpr{valueSym}, t.begin(
			t.list(symbolExpr{name: "define", pos: pos}, target, valueSym),
			t.applyCont(k, t.voidExpr()),
		)))
	case listExpr:
		if len(target.elements) == 0 {
			return nil, pos.errorf("define function form requires a name")
		}

		name, ok := target.elements[0].(symbolExpr)
		if !ok {
			return nil, pos.errorf("define function name must be a symbol")
		}

		lambdaExpr, err := t.transformLambdaValue(listExpr{elements: target.elements[1:], pos: target.pos}, args[1:], pos)
		if err != nil {
			return nil, err
		}

		return t.begin(
			t.list(symbolExpr{name: "define", pos: pos}, name, lambdaExpr),
			t.applyCont(k, t.voidExpr()),
		), nil
	default:
		return nil, pos.errorf("define requires a symbol or function signature")
	}
}

func (t *level18CPSTransformer) cpsSet(args []any, k any, pos sourcePos) (any, error) {
	if len(args) != 2 {
		return nil, pos.errorf("set! expects exactly 2 arguments")
	}

	target, ok := args[0].(symbolExpr)
	if !ok {
		return nil, pos.errorf("set! requires a symbol")
	}

	valueSym := t.freshSymbol("set_value")
	return t.cpsExpr(args[1], t.directLambda([]symbolExpr{valueSym}, t.begin(
		t.list(symbolExpr{name: "set!", pos: pos}, target, valueSym),
		t.applyCont(k, t.voidExpr()),
	)))
}

func (t *level18CPSTransformer) cpsApplication(expr listExpr, k any) (any, error) {
	procValue := t.freshSymbol("proc")
	values, err := t.cpsCollectOperandsReverse(expr.elements[1:], func(values []any) (any, error) {
		elems := make([]any, 0, len(values)+3)
		elems = append(elems, t.internalSymbol(level18ApplyCPSKey))
		elems = append(elems, procValue)
		elems = append(elems, values...)
		elems = append(elems, k)
		return t.list(elems...), nil
	})
	if err != nil {
		return nil, err
	}
	return t.cpsExpr(expr.elements[0], t.directLambda([]symbolExpr{procValue}, values))
}

func (t *level18CPSTransformer) cpsDynamicWind(args []any, k any, pos sourcePos) (any, error) {
	if len(args) != 3 {
		return nil, pos.errorf("dynamic-wind expects exactly 3 arguments")
	}

	beforeProc := t.freshSymbol("before")
	bodyProc := t.freshSymbol("body")
	afterProc := t.freshSymbol("after")

	bodyExpr := t.list(
		t.internalSymbol(level19DynamicWindKey),
		beforeProc,
		bodyProc,
		afterProc,
		k,
	)

	beforeExpr, err := t.cpsExpr(args[0], t.directLambda([]symbolExpr{beforeProc}, bodyExpr))
	if err != nil {
		return nil, err
	}

	bodyValueExpr, err := t.cpsExpr(args[1], t.directLambda([]symbolExpr{bodyProc}, beforeExpr))
	if err != nil {
		return nil, err
	}

	return t.cpsExpr(args[2], t.directLambda([]symbolExpr{afterProc}, bodyValueExpr))
}

func (t *level18CPSTransformer) cpsCollectOperandsReverse(exprs []any, build func([]any) (any, error)) (any, error) {
	var collect func(index int, values []any) (any, error)
	collect = func(index int, values []any) (any, error) {
		if index < 0 {
			return build(values)
		}

		valueSym := t.freshSymbol("value")
		nextValues := append([]any(nil), values...)
		nextValues[index] = valueSym
		next, err := collect(index-1, nextValues)
		if err != nil {
			return nil, err
		}
		return t.cpsExpr(exprs[index], t.directLambda([]symbolExpr{valueSym}, next))
	}

	return collect(len(exprs)-1, make([]any, len(exprs)))
}

func (t *level18CPSTransformer) transformLambdaExpr(args []any, pos sourcePos) (any, error) {
	if len(args) < 2 {
		return nil, pos.errorf("lambda expects parameters and a body")
	}
	return t.transformLambdaValue(args[0], args[1:], pos)
}

func (t *level18CPSTransformer) transformLambdaValue(formals any, body []any, pos sourcePos) (any, error) {
	kParam := t.freshSymbol("k")
	cpsBody, err := t.cpsSequence(body, kParam)
	if err != nil {
		return nil, err
	}

	cpsFormals, err := t.prependContinuationFormal(formals, kParam, pos)
	if err != nil {
		return nil, err
	}

	return t.list(symbolExpr{name: "lambda", pos: pos}, cpsFormals, cpsBody), nil
}

func (t *level18CPSTransformer) transformCaseLambdaExpr(args []any, pos sourcePos) (any, error) {
	if len(args) == 0 {
		return nil, pos.errorf("case-lambda expects at least 1 clause")
	}

	clauses := make([]any, 0, len(args))
	for _, clauseExpr := range args {
		clause, ok := clauseExpr.(listExpr)
		if !ok || len(clause.elements) < 2 {
			return nil, exprSourcePos(clauseExpr).errorf("case-lambda clauses must include parameters and a body")
		}

		transformedClause, err := t.transformLambdaValue(clause.elements[0], clause.elements[1:], clause.pos)
		if err != nil {
			return nil, err
		}

		lambdaExpr, ok := transformedClause.(listExpr)
		if !ok || len(lambdaExpr.elements) != 3 {
			return nil, clause.pos.errorf("invalid case-lambda transformation")
		}

		clauses = append(clauses, t.list(lambdaExpr.elements[1], lambdaExpr.elements[2]))
	}

	return t.list(append([]any{symbolExpr{name: "case-lambda", pos: pos}}, clauses...)...), nil
}

func (t *level18CPSTransformer) prependContinuationFormal(formals any, k symbolExpr, pos sourcePos) (any, error) {
	switch formals := formals.(type) {
	case symbolExpr:
		return listExpr{
			elements: []any{k, symbolExpr{name: ".", pos: pos}, formals},
			pos:      pos,
		}, nil
	case listExpr:
		elements := make([]any, 0, len(formals.elements)+1)
		elements = append(elements, k)
		elements = append(elements, formals.elements...)
		return listExpr{elements: elements, pos: pos}, nil
	default:
		return nil, pos.errorf("lambda parameters must be a list or symbol")
	}
}

func (t *level18CPSTransformer) desugarLet(args []any, pos sourcePos) (any, error) {
	if len(args) < 2 {
		return nil, pos.errorf("let expects bindings and a body")
	}

	if name, ok := args[0].(symbolExpr); ok {
		if len(args) < 3 {
			return nil, pos.errorf("named let expects bindings and a body")
		}

		bindingsExpr, ok := args[1].(listExpr)
		if !ok {
			return nil, pos.errorf("let bindings must be a list")
		}

		names, values, err := t.bindingNamesAndValues(bindingsExpr.elements, "let")
		if err != nil {
			return nil, err
		}

		formals := make([]any, len(names))
		for i, name := range names {
			formals[i] = name
		}

		lambdaExpr := t.list(
			symbolExpr{name: "lambda", pos: pos},
			listExpr{elements: formals, pos: pos},
		)
		lambdaExpr = appendListExpr(lambdaExpr, args[2:]...)

		letrecBinding := listExpr{
			elements: []any{name, lambdaExpr},
			pos:      pos,
		}
		callExpr := t.list(append([]any{name}, values...)...)
		return t.list(symbolExpr{name: "letrec", pos: pos}, listExpr{elements: []any{letrecBinding}, pos: pos}, callExpr), nil
	}

	bindingsExpr, ok := args[0].(listExpr)
	if !ok {
		return nil, pos.errorf("let bindings must be a list")
	}

	names, values, err := t.bindingNamesAndValues(bindingsExpr.elements, "let")
	if err != nil {
		return nil, err
	}

	return t.buildUnnamedLet(names, values, args[1:], pos), nil
}

func (t *level18CPSTransformer) desugarLetStar(args []any, pos sourcePos) (any, error) {
	if len(args) < 2 {
		return nil, pos.errorf("let* expects bindings and a body")
	}

	bindingsExpr, ok := args[0].(listExpr)
	if !ok {
		return nil, pos.errorf("let* bindings must be a list")
	}

	result := t.begin(args[1:]...)
	for i := len(bindingsExpr.elements) - 1; i >= 0; i-- {
		result = t.list(
			symbolExpr{name: "let", pos: pos},
			listExpr{elements: []any{bindingsExpr.elements[i]}, pos: pos},
			result,
		)
	}
	return result, nil
}

func (t *level18CPSTransformer) desugarLetrec(args []any, pos sourcePos, sequential bool) (any, error) {
	formName := "letrec"
	if sequential {
		formName = "letrec*"
	}

	if len(args) < 2 {
		return nil, pos.errorf("%s expects bindings and a body", formName)
	}

	bindingsExpr, ok := args[0].(listExpr)
	if !ok {
		return nil, pos.errorf("%s bindings must be a list", formName)
	}

	bindings, err := parseLetBindingSpecs(bindingsExpr.elements, formName)
	if err != nil {
		return nil, err
	}

	setup := make([]any, 0, len(bindings)*2+len(args[1:]))
	for _, binding := range bindings {
		setup = append(setup, t.list(
			symbolExpr{name: "define", pos: pos},
			binding.name,
			t.voidExpr(),
		))
	}
	for _, binding := range bindings {
		setup = append(setup, t.list(
			symbolExpr{name: "set!", pos: pos},
			binding.name,
			binding.value,
		))
		if sequential {
			continue
		}
	}
	setup = append(setup, args[1:]...)

	return t.list(
		t.list(
			symbolExpr{name: "lambda", pos: pos},
			listExpr{elements: nil, pos: pos},
			t.begin(setup...),
		),
	), nil
}

func (t *level18CPSTransformer) desugarCond(args []any, pos sourcePos) (any, error) {
	if len(args) == 0 {
		return t.voidExpr(), nil
	}
	return t.desugarCondClauses(args, pos)
}

func (t *level18CPSTransformer) desugarCondClauses(clauses []any, pos sourcePos) (any, error) {
	clauseExpr := clauses[0]
	clause, ok := clauseExpr.(listExpr)
	if !ok || len(clause.elements) == 0 {
		return nil, exprSourcePos(clauseExpr).errorf("cond clauses must be non-empty lists")
	}

	if symbol, ok := clause.elements[0].(symbolExpr); ok && symbol.name == "else" {
		if len(clauses) != 1 {
			return nil, symbol.pos.errorf("cond else clause must be last")
		}
		if len(clause.elements) == 1 {
			return t.voidExpr(), nil
		}
		return t.begin(clause.elements[1:]...), nil
	}

	next := any(t.voidExpr())
	var err error
	if len(clauses) > 1 {
		next, err = t.desugarCondClauses(clauses[1:], pos)
		if err != nil {
			return nil, err
		}
	}

	if len(clause.elements) == 1 {
		testValue := t.freshSymbol("cond_test")
		return t.buildUnnamedLet(
			[]symbolExpr{testValue},
			[]any{clause.elements[0]},
			[]any{
				t.list(symbolExpr{name: "if", pos: pos}, testValue, testValue, next),
			},
			pos,
		), nil
	}

	return t.list(
		symbolExpr{name: "if", pos: pos},
		clause.elements[0],
		t.begin(clause.elements[1:]...),
		next,
	), nil
}

func (t *level18CPSTransformer) desugarAnd(args []any, pos sourcePos) (any, error) {
	switch len(args) {
	case 0:
		return true, nil
	case 1:
		return args[0], nil
	}

	firstValue := t.freshSymbol("and_value")
	rest, err := t.desugarAnd(args[1:], pos)
	if err != nil {
		return nil, err
	}

	return t.buildUnnamedLet(
		[]symbolExpr{firstValue},
		[]any{args[0]},
		[]any{
			t.list(symbolExpr{name: "if", pos: pos}, firstValue, rest, firstValue),
		},
		pos,
	), nil
}

func (t *level18CPSTransformer) desugarOr(args []any, pos sourcePos) (any, error) {
	switch len(args) {
	case 0:
		return false, nil
	case 1:
		return args[0], nil
	}

	firstValue := t.freshSymbol("or_value")
	rest, err := t.desugarOr(args[1:], pos)
	if err != nil {
		return nil, err
	}

	return t.buildUnnamedLet(
		[]symbolExpr{firstValue},
		[]any{args[0]},
		[]any{
			t.list(symbolExpr{name: "if", pos: pos}, firstValue, firstValue, rest),
		},
		pos,
	), nil
}

func (t *level18CPSTransformer) desugarCase(args []any, pos sourcePos) (any, error) {
	if len(args) < 1 {
		return nil, pos.errorf("case expects a key and clauses")
	}

	keyValue := t.freshSymbol("case_key")
	body, err := t.desugarCaseClauses(keyValue, args[1:], pos)
	if err != nil {
		return nil, err
	}

	return t.buildUnnamedLet([]symbolExpr{keyValue}, []any{args[0]}, []any{body}, pos), nil
}

func (t *level18CPSTransformer) desugarCaseClauses(key symbolExpr, clauses []any, pos sourcePos) (any, error) {
	if len(clauses) == 0 {
		return t.voidExpr(), nil
	}

	clauseExpr := clauses[0]
	clause, ok := clauseExpr.(listExpr)
	if !ok || len(clause.elements) == 0 {
		return nil, exprSourcePos(clauseExpr).errorf("case clauses must be non-empty lists")
	}

	if symbol, ok := clause.elements[0].(symbolExpr); ok && symbol.name == "else" {
		if len(clauses) != 1 {
			return nil, symbol.pos.errorf("case else clause must be last")
		}
		if len(clause.elements) == 1 {
			return t.voidExpr(), nil
		}
		return t.begin(clause.elements[1:]...), nil
	}

	datums, ok := clause.elements[0].(listExpr)
	if !ok {
		return nil, exprSourcePos(clause.elements[0]).errorf("case clause datums must be a list")
	}

	conditions := make([]any, 0, len(datums.elements))
	for _, datum := range datums.elements {
		conditions = append(conditions, t.list(
			symbolExpr{name: "eqv?", pos: pos},
			key,
			t.list(symbolExpr{name: "quote", pos: pos}, datum),
		))
	}

	conditionExpr, err := t.desugarOr(conditions, pos)
	if err != nil {
		return nil, err
	}

	next, err := t.desugarCaseClauses(key, clauses[1:], pos)
	if err != nil {
		return nil, err
	}

	body := any(t.voidExpr())
	if len(clause.elements) > 1 {
		body = t.begin(clause.elements[1:]...)
	}

	return t.list(symbolExpr{name: "if", pos: pos}, conditionExpr, body, next), nil
}

func (t *level18CPSTransformer) bindingNamesAndValues(bindings []any, formName string) ([]symbolExpr, []any, error) {
	names := make([]symbolExpr, 0, len(bindings))
	values := make([]any, 0, len(bindings))
	for _, bindingExpr := range bindings {
		binding, ok := bindingExpr.(listExpr)
		if !ok || len(binding.elements) != 2 {
			return nil, nil, exprSourcePos(bindingExpr).errorf("%s bindings must be name/value pairs", formName)
		}

		name, ok := binding.elements[0].(symbolExpr)
		if !ok {
			return nil, nil, exprSourcePos(binding.elements[0]).errorf("%s binding name must be a symbol", formName)
		}

		names = append(names, name)
		values = append(values, binding.elements[1])
	}
	return names, values, nil
}

func (t *level18CPSTransformer) buildUnnamedLet(names []symbolExpr, values []any, body []any, pos sourcePos) any {
	formals := make([]any, len(names))
	for i, name := range names {
		formals[i] = name
	}

	lambdaExpr := t.list(
		symbolExpr{name: "lambda", pos: pos},
		listExpr{elements: formals, pos: pos},
	)
	lambdaExpr = appendListExpr(lambdaExpr, body...)

	elements := make([]any, 0, len(values)+1)
	elements = append(elements, lambdaExpr)
	elements = append(elements, values...)
	return t.list(elements...)
}

func (t *level18CPSTransformer) freshSymbol(prefix string) symbolExpr {
	t.counter++
	key := fmt.Sprintf("__l18_%s_%d", prefix, t.counter)
	return symbolExpr{name: key, key: key}
}

func (t *level18CPSTransformer) internalSymbol(key string) symbolExpr {
	return symbolExpr{name: key, key: key}
}

func (t *level18CPSTransformer) directLambda(params []symbolExpr, body any) any {
	formals := make([]any, len(params))
	for i, param := range params {
		formals[i] = param
	}
	return t.list(
		symbolExpr{name: "lambda"},
		listExpr{elements: formals},
		body,
	)
}

func (t *level18CPSTransformer) applyCont(k any, value any) any {
	return t.list(k, value)
}

func (t *level18CPSTransformer) begin(exprs ...any) any {
	if len(exprs) == 1 {
		return exprs[0]
	}
	elements := make([]any, 0, len(exprs)+1)
	elements = append(elements, symbolExpr{name: "begin"})
	elements = append(elements, exprs...)
	return t.list(elements...)
}

func (t *level18CPSTransformer) voidExpr() any {
	return voidValue{}
}

func (t *level18CPSTransformer) list(elements ...any) listExpr {
	return listExpr{elements: elements}
}

func appendListExpr(list listExpr, extra ...any) listExpr {
	elements := append([]any(nil), list.elements...)
	elements = append(elements, extra...)
	return listExpr{elements: elements, pos: list.pos}
}
