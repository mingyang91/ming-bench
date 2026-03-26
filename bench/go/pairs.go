package ming

import (
	"fmt"
	"strings"
)

type pairCompareKey struct {
	left  *pairExpr
	right *pairExpr
}

type vectorCompareKey struct {
	left  *vectorExpr
	right *vectorExpr
}

func isEmptyList(value expr) bool {
	list, ok := value.(listExpr)
	return ok && len(list.items) == 0
}

func isPairValue(value expr) bool {
	switch v := value.(type) {
	case *pairExpr:
		return true
	case listExpr:
		return len(v.items) > 0
	default:
		return false
	}
}

func properListFromSlice(items []expr) expr {
	var result expr = listExpr{}
	for i := len(items) - 1; i >= 0; i-- {
		result = &pairExpr{car: items[i], cdr: result}
	}
	return result
}

func quoteDatum(form expr) expr {
	switch v := form.(type) {
	case listExpr:
		items := make([]expr, len(v.items))
		for i, item := range v.items {
			items[i] = quoteDatum(item)
		}
		return properListFromSlice(items)
	case *pairExpr:
		return &pairExpr{
			car: quoteDatum(v.car),
			cdr: quoteDatum(v.cdr),
		}
	case *vectorExpr:
		items := make([]expr, len(v.items))
		for i, item := range v.items {
			items[i] = quoteDatum(item)
		}
		return &vectorExpr{items: items}
	default:
		return form
	}
}

func carValue(value expr) (expr, bool) {
	switch v := value.(type) {
	case *pairExpr:
		return v.car, true
	case listExpr:
		if len(v.items) == 0 {
			return nil, false
		}
		return v.items[0], true
	default:
		return nil, false
	}
}

func cdrValue(value expr) (expr, bool) {
	switch v := value.(type) {
	case *pairExpr:
		return v.cdr, true
	case listExpr:
		if len(v.items) == 0 {
			return nil, false
		}
		return properListFromSlice(v.items[1:]), true
	default:
		return nil, false
	}
}

func listElementsTail(value expr) ([]expr, expr, bool) {
	items := []expr{}
	seen := map[*pairExpr]struct{}{}
	current := value

	for {
		switch v := current.(type) {
		case listExpr:
			items = append(items, v.items...)
			return items, listExpr{}, false
		case *pairExpr:
			if _, ok := seen[v]; ok {
				return items, v, true
			}
			seen[v] = struct{}{}
			items = append(items, v.car)
			current = v.cdr
		default:
			return items, current, false
		}
	}
}

func listElements(value expr) ([]expr, bool) {
	items, tail, cycle := listElementsTail(value)
	return items, !cycle && isEmptyList(tail)
}

func isProperListValue(value expr) bool {
	_, ok := listElements(value)
	return ok
}

func builtinSetCar(args []expr) (expr, error) {
	if len(args) != 2 {
		return nil, &EvalError{Message: "set-car! expects exactly 2 arguments"}
	}

	pair, ok := args[0].(*pairExpr)
	if !ok {
		return nil, &EvalError{Message: "set-car! expects a pair"}
	}

	pair.car = args[1]
	return voidExpr{}, nil
}

func builtinSetCdr(args []expr) (expr, error) {
	if len(args) != 2 {
		return nil, &EvalError{Message: "set-cdr! expects exactly 2 arguments"}
	}

	pair, ok := args[0].(*pairExpr)
	if !ok {
		return nil, &EvalError{Message: "set-cdr! expects a pair"}
	}

	pair.cdr = args[1]
	return voidExpr{}, nil
}

func builtinForEach(args []expr) (expr, error) {
	if len(args) < 2 {
		return nil, &EvalError{Message: "for-each expects a procedure and at least one list"}
	}

	lists := make([][]expr, len(args)-1)
	expectedLen := -1
	for i, arg := range args[1:] {
		items, ok := listElements(arg)
		if !ok {
			return nil, &EvalError{Message: "for-each expects list arguments"}
		}
		if expectedLen == -1 {
			expectedLen = len(items)
		} else if len(items) != expectedLen {
			return nil, &EvalError{Message: "for-each expects lists of equal length"}
		}
		lists[i] = items
	}

	for i := 0; i < expectedLen; i++ {
		callArgs := make([]expr, len(lists))
		for j, items := range lists {
			callArgs[j] = items[i]
		}
		if _, err := applyCallable(args[0], callArgs); err != nil {
			return nil, err
		}
	}

	return voidExpr{}, nil
}

func builtinError(args []expr) (expr, error) {
	if len(args) == 0 {
		return nil, &EvalError{Message: "error"}
	}

	parts := make([]string, len(args))
	for i, arg := range args {
		parts[i] = displayExpr(arg)
	}
	return nil, &EvalError{Message: strings.Join(parts, " ")}
}

func evalLetStar(environment *env, forms []expr) (evalStep, error) {
	if len(forms) < 2 {
		return evalStep{}, &EvalError{Message: "let* expects bindings and a body"}
	}

	bindings, ok := forms[0].(listExpr)
	if !ok {
		return evalStep{}, &EvalError{Message: "let* bindings must be a list"}
	}

	letEnv := &env{
		parent:   environment,
		bindings: map[string]expr{},
	}

	for _, binding := range bindings.items {
		pair, ok := binding.(listExpr)
		if !ok || len(pair.items) != 2 {
			return evalStep{}, &EvalError{Message: "let* bindings must have the form (name value)"}
		}

		name, ok := pair.items[0].(symbolExpr)
		if !ok {
			return evalStep{}, &EvalError{Message: "let* binding name must be a symbol"}
		}

		value, err := evalSingleExpr(letEnv, pair.items[1], "let*")
		if err != nil {
			return evalStep{}, err
		}
		letEnv.define(name.name, value)
	}

	return evalSequenceTail(letEnv, forms[1:])
}

func equalExprSeen(a, b expr, pairSeen map[pairCompareKey]struct{}, vectorSeen map[vectorCompareKey]struct{}) bool {
	if equal, ok := numericEqualExpr(a, b); ok {
		return equal
	}

	switch left := a.(type) {
	case intExpr:
		right, ok := b.(intExpr)
		return ok && left == right
	case boolExpr:
		right, ok := b.(boolExpr)
		return ok && left == right
	case charExpr:
		right, ok := b.(charExpr)
		return ok && left == right
	case symbolExpr:
		right, ok := b.(symbolExpr)
		return ok && left.name == right.name
	case *stringExpr:
		right, ok := asString(b)
		return ok && left.text() == right.text()
	case listExpr:
		rightItems, ok := listElements(b)
		if !ok || len(left.items) != len(rightItems) {
			return false
		}
		for i := range left.items {
			if !equalExprSeen(left.items[i], rightItems[i], pairSeen, vectorSeen) {
				return false
			}
		}
		return true
	case *pairExpr:
		right, ok := b.(*pairExpr)
		if !ok {
			return false
		}
		key := pairCompareKey{left: left, right: right}
		if _, ok := pairSeen[key]; ok {
			return true
		}
		pairSeen[key] = struct{}{}
		pairSeen[pairCompareKey{left: right, right: left}] = struct{}{}
		return equalExprSeen(left.car, right.car, pairSeen, vectorSeen) &&
			equalExprSeen(left.cdr, right.cdr, pairSeen, vectorSeen)
	case *vectorExpr:
		right, ok := b.(*vectorExpr)
		if !ok || len(left.items) != len(right.items) {
			return false
		}
		key := vectorCompareKey{left: left, right: right}
		if _, ok := vectorSeen[key]; ok {
			return true
		}
		vectorSeen[key] = struct{}{}
		vectorSeen[vectorCompareKey{left: right, right: left}] = struct{}{}
		for i := range left.items {
			if !equalExprSeen(left.items[i], right.items[i], pairSeen, vectorSeen) {
				return false
			}
		}
		return true
	case builtinProc:
		right, ok := b.(builtinProc)
		return ok && left.name == right.name
	case voidExpr:
		_, ok := b.(voidExpr)
		return ok
	default:
		return false
	}
}

func pairMutationArg(value expr, name string) (*pairExpr, error) {
	pair, ok := value.(*pairExpr)
	if !ok {
		return nil, &EvalError{Message: fmt.Sprintf("%s expects a pair", name)}
	}
	return pair, nil
}
