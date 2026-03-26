package ming

import (
	"fmt"
	"strings"
)

type vectorExpr struct {
	items []expr
}

type uninitializedExpr struct {
	name string
}

type doBinding struct {
	name string
	step expr
}

func builtinEqv(args []expr) (expr, error) {
	if len(args) != 2 {
		return nil, &EvalError{Message: "eqv? expects exactly 2 arguments"}
	}
	return boolExpr(eqvExpr(args[0], args[1])), nil
}

func builtinVector(args []expr) (expr, error) {
	items := append([]expr(nil), args...)
	return &vectorExpr{items: items}, nil
}

func builtinMakeVector(args []expr) (expr, error) {
	if len(args) != 1 && len(args) != 2 {
		return nil, &EvalError{Message: "make-vector expects 1 or 2 arguments"}
	}

	length, ok := exactIntegerValue(args[0])
	if !ok {
		return nil, &EvalError{Message: "make-vector expects an exact integer length"}
	}
	if length < 0 {
		return nil, &EvalError{Message: "make-vector length must be non-negative"}
	}

	fill := expr(voidExpr{})
	if len(args) == 2 {
		fill = args[1]
	}

	items := make([]expr, length)
	for i := range items {
		items[i] = fill
	}

	return &vectorExpr{items: items}, nil
}

func builtinVectorPred(args []expr) (expr, error) {
	if len(args) != 1 {
		return nil, &EvalError{Message: "vector? expects exactly 1 argument"}
	}

	_, ok := args[0].(*vectorExpr)
	return boolExpr(ok), nil
}

func builtinVectorLength(args []expr) (expr, error) {
	vector, err := unaryVectorArg(args, "vector-length")
	if err != nil {
		return nil, err
	}
	return intExpr(len(vector.items)), nil
}

func builtinVectorRef(args []expr) (expr, error) {
	if len(args) != 2 {
		return nil, &EvalError{Message: "vector-ref expects exactly 2 arguments"}
	}

	vector, err := vectorArg(args[0], "vector-ref")
	if err != nil {
		return nil, err
	}

	index, ok := exactIntegerValue(args[1])
	if !ok {
		return nil, &EvalError{Message: "vector-ref expects an exact integer index"}
	}
	if index < 0 || index >= len(vector.items) {
		return nil, &EvalError{Message: "vector-ref index out of range"}
	}

	return vector.items[index], nil
}

func builtinVectorSet(args []expr) (expr, error) {
	if len(args) != 3 {
		return nil, &EvalError{Message: "vector-set! expects exactly 3 arguments"}
	}

	vector, err := vectorArg(args[0], "vector-set!")
	if err != nil {
		return nil, err
	}

	index, ok := exactIntegerValue(args[1])
	if !ok {
		return nil, &EvalError{Message: "vector-set! expects an exact integer index"}
	}
	if index < 0 || index >= len(vector.items) {
		return nil, &EvalError{Message: "vector-set! index out of range"}
	}

	vector.items[index] = args[2]
	return voidExpr{}, nil
}

func builtinVectorToList(args []expr) (expr, error) {
	vector, err := unaryVectorArg(args, "vector->list")
	if err != nil {
		return nil, err
	}

	items := append([]expr(nil), vector.items...)
	return listExpr{items: items}, nil
}

func builtinListToVector(args []expr) (expr, error) {
	if len(args) != 1 {
		return nil, &EvalError{Message: "list->vector expects exactly 1 argument"}
	}

	list, ok := args[0].(listExpr)
	if !ok {
		return nil, &EvalError{Message: "list->vector expects a list"}
	}

	items := append([]expr(nil), list.items...)
	return &vectorExpr{items: items}, nil
}

func unaryVectorArg(args []expr, name string) (*vectorExpr, error) {
	if len(args) != 1 {
		return nil, &EvalError{Message: fmt.Sprintf("%s expects exactly 1 argument", name)}
	}
	return vectorArg(args[0], name)
}

func vectorArg(value expr, name string) (*vectorExpr, error) {
	vector, ok := value.(*vectorExpr)
	if !ok {
		return nil, &EvalError{Message: fmt.Sprintf("%s expects a vector", name)}
	}
	return vector, nil
}

func eqvExpr(a, b expr) bool {
	return eqExpr(a, b)
}

func evalCase(environment *env, forms []expr) (evalStep, error) {
	if len(forms) < 2 {
		return evalStep{}, &EvalError{Message: "case expects a key and at least one clause"}
	}

	key, err := evalExpr(environment, forms[0])
	if err != nil {
		return evalStep{}, err
	}

	clauses := forms[1:]
	for i, form := range clauses {
		clause, ok := form.(listExpr)
		if !ok || len(clause.items) == 0 {
			return evalStep{}, &EvalError{Message: "case clauses must be non-empty lists"}
		}

		if symbol, ok := clause.items[0].(symbolExpr); ok && symbol.name == "else" {
			if i != len(clauses)-1 {
				return evalStep{}, &EvalError{Message: "case else clause must be last"}
			}
			if len(clause.items) == 1 {
				return doneStep(voidExpr{}), nil
			}
			return evalSequenceTail(environment, clause.items[1:])
		}

		datums, ok := clause.items[0].(listExpr)
		if !ok {
			return evalStep{}, &EvalError{Message: "case clause datums must be a list"}
		}

		matched := false
		for _, datum := range datums.items {
			if eqvExpr(key, datum) {
				matched = true
				break
			}
		}
		if !matched {
			continue
		}

		if len(clause.items) == 1 {
			return doneStep(voidExpr{}), nil
		}
		return evalSequenceTail(environment, clause.items[1:])
	}

	return doneStep(voidExpr{}), nil
}

func evalLetrec(environment *env, forms []expr, sequential bool) (evalStep, error) {
	formName := "letrec"
	if sequential {
		formName = "letrec*"
	}

	if len(forms) < 2 {
		return evalStep{}, &EvalError{Message: formName + " expects bindings and a body"}
	}

	bindings, ok := forms[0].(listExpr)
	if !ok {
		return evalStep{}, &EvalError{Message: formName + " bindings must be a list"}
	}

	names, initForms, err := parseNameValueBindings(bindings, formName)
	if err != nil {
		return evalStep{}, err
	}

	letEnv := &env{
		parent:   environment,
		bindings: map[string]expr{},
	}

	if sequential {
		for i, name := range names {
			letEnv.define(name, uninitializedExpr{name: name})

			value, err := evalExpr(letEnv, initForms[i])
			if err != nil {
				return evalStep{}, err
			}
			letEnv.bindings[name] = value
		}
	} else {
		for _, name := range names {
			letEnv.define(name, uninitializedExpr{name: name})
		}
		for i, name := range names {
			value, err := evalExpr(letEnv, initForms[i])
			if err != nil {
				return evalStep{}, err
			}
			letEnv.bindings[name] = value
		}
	}

	return evalSequenceTail(letEnv, forms[1:])
}

func parseNameValueBindings(bindings listExpr, formName string) ([]string, []expr, error) {
	names := make([]string, 0, len(bindings.items))
	values := make([]expr, 0, len(bindings.items))
	seen := map[string]struct{}{}

	for _, binding := range bindings.items {
		pair, ok := binding.(listExpr)
		if !ok || len(pair.items) != 2 {
			return nil, nil, &EvalError{Message: fmt.Sprintf("%s bindings must have the form (name value)", formName)}
		}

		name, ok := pair.items[0].(symbolExpr)
		if !ok {
			return nil, nil, &EvalError{Message: fmt.Sprintf("%s binding name must be a symbol", formName)}
		}
		if _, exists := seen[name.name]; exists {
			return nil, nil, &EvalError{Message: fmt.Sprintf("duplicate binding: %s", name.name)}
		}
		seen[name.name] = struct{}{}

		names = append(names, name.name)
		values = append(values, pair.items[1])
	}

	return names, values, nil
}

func evalDo(environment *env, forms []expr) (evalStep, error) {
	if len(forms) < 2 {
		return evalStep{}, &EvalError{Message: "do expects bindings, a test clause, and optional body expressions"}
	}

	bindingList, ok := forms[0].(listExpr)
	if !ok {
		return evalStep{}, &EvalError{Message: "do bindings must be a list"}
	}

	testClause, ok := forms[1].(listExpr)
	if !ok || len(testClause.items) == 0 {
		return evalStep{}, &EvalError{Message: "do test clause must be a non-empty list"}
	}

	loopEnv := &env{
		parent:   environment,
		bindings: map[string]expr{},
	}
	bindings := make([]doBinding, 0, len(bindingList.items))
	seen := map[string]struct{}{}

	for _, rawBinding := range bindingList.items {
		binding, ok := rawBinding.(listExpr)
		if !ok || len(binding.items) < 2 || len(binding.items) > 3 {
			return evalStep{}, &EvalError{Message: "do bindings must have the form (name init [step])"}
		}

		name, ok := binding.items[0].(symbolExpr)
		if !ok {
			return evalStep{}, &EvalError{Message: "do binding name must be a symbol"}
		}
		if _, exists := seen[name.name]; exists {
			return evalStep{}, &EvalError{Message: fmt.Sprintf("duplicate binding: %s", name.name)}
		}
		seen[name.name] = struct{}{}

		initValue, err := evalExpr(environment, binding.items[1])
		if err != nil {
			return evalStep{}, err
		}

		var step expr
		if len(binding.items) == 3 {
			step = binding.items[2]
		}

		loopEnv.define(name.name, initValue)
		bindings = append(bindings, doBinding{
			name: name.name,
			step: step,
		})
	}

	body := forms[2:]
	for {
		testValue, err := evalExpr(loopEnv, testClause.items[0])
		if err != nil {
			return evalStep{}, err
		}
		if isTruthy(testValue) {
			if len(testClause.items) == 1 {
				return doneStep(voidExpr{}), nil
			}
			return evalSequenceTail(loopEnv, testClause.items[1:])
		}

		if len(body) > 0 {
			if _, err := evalSequence(loopEnv, body); err != nil {
				return evalStep{}, err
			}
		}

		nextValues := make([]expr, len(bindings))
		for i, binding := range bindings {
			if binding.step == nil {
				nextValues[i] = loopEnv.bindings[binding.name]
				continue
			}

			value, err := evalExpr(loopEnv, binding.step)
			if err != nil {
				return evalStep{}, err
			}
			nextValues[i] = value
		}

		for i, binding := range bindings {
			loopEnv.bindings[binding.name] = nextValues[i]
		}
	}
}

func renderVector(vector *vectorExpr, render func(expr) string) string {
	parts := make([]string, len(vector.items))
	for i, item := range vector.items {
		parts[i] = render(item)
	}
	return "#(" + strings.Join(parts, " ") + ")"
}
