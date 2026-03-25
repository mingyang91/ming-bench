package ming

import "fmt"

type caseLambdaClause struct {
	params  []bindingName
	rest    bindingName
	hasRest bool
	body    []expr
}

type caseLambdaProc struct {
	name    string
	clauses []caseLambdaClause
	env     *env
}

func (it *interpreter) evalCaseLambda(scope *env, list *listExpr) (value, error) {
	if len(list.elements) < 2 {
		return nil, newEvalError(ErrSyntax, "case-lambda: expected at least one clause", list.at)
	}

	clauses := make([]caseLambdaClause, 0, len(list.elements)-1)
	for _, clauseExpr := range list.elements[1:] {
		clause, ok := clauseExpr.(*listExpr)
		if !ok || len(clause.elements) < 2 {
			return nil, newEvalError(ErrSyntax, "case-lambda: expected clause with formals and body", clauseExpr.pos())
		}

		params, rest, hasRest, err := parseLambdaParams(clause.elements[0])
		if err != nil {
			return nil, err
		}

		clauses = append(clauses, caseLambdaClause{
			params:  params,
			rest:    rest,
			hasRest: hasRest,
			body:    clause.elements[1:],
		})
	}

	return &caseLambdaProc{
		clauses: clauses,
		env:     scope,
	}, nil
}

func (it *interpreter) applyCaseLambda(proc *caseLambdaProc, args []value, callPos position) (value, error) {
	for _, clause := range proc.clauses {
		if procedureArityMatches(len(clause.params), clause.hasRest, len(args)) {
			return it.applyProcedureBody(proc.env, clause.params, clause.rest, clause.hasRest, clause.body, args)
		}
	}

	name := proc.name
	if name == "" {
		name = "case-lambda"
	}
	return nil, wrongArgCount(callPos, name, fmt.Sprintf("no matching clause for %d arguments", len(args)))
}

func procedureArityMatches(paramCount int, hasRest bool, argCount int) bool {
	if hasRest {
		return argCount >= paramCount
	}
	return argCount == paramCount
}

func (it *interpreter) applyProcedureBody(scope *env, params []bindingName, rest bindingName, hasRest bool, body []expr, args []value) (value, error) {
	callEnv := newEnv(scope)
	for i, param := range params {
		it.defineBindingName(callEnv, param, args[i])
	}
	if hasRest {
		it.defineBindingName(callEnv, rest, buildList(args[len(params):]))
	}
	return it.evalSequence(callEnv, body)
}

func isProcedureValue(v value) bool {
	switch v.(type) {
	case *builtinProc, *closureProc, *caseLambdaProc, *continuationProc:
		return true
	default:
		return false
	}
}

func builtinProcedurePred(_ *interpreter, args []value, callPos position) (value, error) {
	if len(args) != 1 {
		return nil, wrongArgCount(callPos, "procedure?", "expected exactly 1 argument")
	}
	return isProcedureValue(args[0]), nil
}
