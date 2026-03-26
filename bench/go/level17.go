package ming

import "fmt"

func newPair(car, cdr any) pairValue {
	return pairValue{&pairCell{car: car, cdr: cdr}}
}

func registerCxrBuiltins(scope *env) {
	for _, name := range []string{
		"caar", "cadr", "cdar", "cddr",
		"caaar", "caadr", "cadar", "caddr", "cdaar", "cdadr", "cddar", "cdddr",
		"caaaar", "caaadr", "caadar", "caaddr", "cadaar", "cadadr", "caddar", "cadddr",
		"cdaaar", "cdaadr", "cdadar", "cdaddr", "cddaar", "cddadr", "cdddar", "cddddr",
	} {
		scope.define(name, builtinProc{
			name: name,
			fn:   makeCxrBuiltin(name),
		})
	}
}

func makeCxrBuiltin(name string) builtinFunc {
	pattern := name[1 : len(name)-1]
	return func(args []any) (any, error) {
		if len(args) != 1 {
			return nil, &EvalError{Message: fmt.Sprintf("%s expects exactly 1 argument", name)}
		}

		current := args[0]
		for i := len(pattern) - 1; i >= 0; i-- {
			pair, err := expectPair(current, name)
			if err != nil {
				return nil, err
			}

			if pattern[i] == 'a' {
				current = pair.car
			} else {
				current = pair.cdr
			}
		}
		return current, nil
	}
}

func expectPair(value any, builtinName string) (pairValue, error) {
	pair, ok := value.(pairValue)
	if !ok {
		return pairValue{}, &EvalError{Message: fmt.Sprintf("%s expects a pair, got %s", builtinName, typeName(value))}
	}
	return pair, nil
}

func evalLetStar(scope *env, args []any) (any, error) {
	letScope, err := evalLetStarScope(scope, args)
	if err != nil {
		return nil, err
	}
	return evalSequence(letScope, args[1:])
}

func evalLetStarTail(scope *env, args []any) (any, *tailEvalState, error) {
	letScope, err := evalLetStarScope(scope, args)
	if err != nil {
		return nil, nil, err
	}
	return prepareTailSequence(letScope, args[1:])
}

func evalLetStarScope(scope *env, args []any) (*env, error) {
	if len(args) < 2 {
		return nil, &EvalError{Message: "let* expects bindings and a body"}
	}

	bindingsExpr, ok := args[0].(listExpr)
	if !ok {
		return nil, &EvalError{Message: "let* bindings must be a list"}
	}

	letScope := newEnv(scope)
	for _, bindingExpr := range bindingsExpr.elements {
		binding, ok := bindingExpr.(listExpr)
		if !ok || len(binding.elements) != 2 {
			return nil, &EvalError{Message: "let* bindings must be name/value pairs"}
		}

		name, ok := binding.elements[0].(symbolExpr)
		if !ok {
			return nil, &EvalError{Message: "let* binding name must be a symbol"}
		}

		value, err := eval(letScope, binding.elements[1])
		if err != nil {
			return nil, err
		}
		letScope.defineSymbol(name, value)
	}

	return letScope, nil
}

func builtinSetCar(args []any) (any, error) {
	if len(args) != 2 {
		return nil, &EvalError{Message: "set-car! expects exactly 2 arguments"}
	}

	pair, err := expectPair(args[0], "set-car!")
	if err != nil {
		return nil, err
	}

	pair.car = args[1]
	return voidValue{}, nil
}

func builtinSetCdr(args []any) (any, error) {
	if len(args) != 2 {
		return nil, &EvalError{Message: "set-cdr! expects exactly 2 arguments"}
	}

	pair, err := expectPair(args[0], "set-cdr!")
	if err != nil {
		return nil, err
	}

	pair.cdr = args[1]
	return voidValue{}, nil
}

func builtinReverse(args []any) (any, error) {
	if len(args) != 1 {
		return nil, &EvalError{Message: "reverse expects exactly 1 argument"}
	}

	elements, err := properListElements(args[0], "reverse")
	if err != nil {
		return nil, err
	}

	result := any(emptyListValue{})
	for _, elem := range elements {
		result = newPair(elem, result)
	}
	return result, nil
}

func builtinAssv(args []any) (any, error) {
	if len(args) != 2 {
		return nil, &EvalError{Message: "assv expects exactly 2 arguments"}
	}

	key := args[0]
	current := args[1]
	seen := map[*pairCell]struct{}{}
	for {
		switch list := current.(type) {
		case emptyListValue:
			return false, nil
		case pairValue:
			if _, ok := seen[list.pairCell]; ok {
				return nil, &EvalError{Message: "assv expects a proper list"}
			}
			seen[list.pairCell] = struct{}{}

			entry, ok := list.car.(pairValue)
			if !ok {
				return nil, &EvalError{Message: "assv expects an association list"}
			}
			if valuesEqv(key, entry.car) {
				return entry, nil
			}
			current = list.cdr
		default:
			return nil, &EvalError{Message: fmt.Sprintf("assv expects a list, got %s", typeName(args[1]))}
		}
	}
}

func builtinMember(args []any) (any, error) {
	if len(args) != 2 {
		return nil, &EvalError{Message: "member expects exactly 2 arguments"}
	}

	target := args[0]
	current := args[1]
	seen := map[*pairCell]struct{}{}
	for {
		switch list := current.(type) {
		case emptyListValue:
			return false, nil
		case pairValue:
			if _, ok := seen[list.pairCell]; ok {
				return nil, &EvalError{Message: "member expects a proper list"}
			}
			seen[list.pairCell] = struct{}{}

			if valuesEqual(target, list.car) {
				return list, nil
			}
			current = list.cdr
		default:
			return nil, &EvalError{Message: fmt.Sprintf("member expects a list, got %s", typeName(args[1]))}
		}
	}
}

func builtinForEach(args []any) (any, error) {
	if len(args) < 2 {
		return nil, &EvalError{Message: "for-each expects at least 2 arguments"}
	}

	proc := args[0]
	lists := make([][]any, len(args)-1)
	expectedLen := -1
	for i, listArg := range args[1:] {
		elements, err := properListElements(listArg, "for-each")
		if err != nil {
			return nil, err
		}
		if expectedLen < 0 {
			expectedLen = len(elements)
		} else if len(elements) != expectedLen {
			return nil, &EvalError{Message: "for-each expects lists of equal length"}
		}
		lists[i] = elements
	}

	callArgs := make([]any, len(lists))
	for i := 0; i < expectedLen; i++ {
		for j := range lists {
			callArgs[j] = lists[j][i]
		}
		if _, err := applyProcedure(proc, callArgs); err != nil {
			return nil, err
		}
	}

	return voidValue{}, nil
}

func builtinMakeString(args []any) (any, error) {
	if len(args) != 1 && len(args) != 2 {
		return nil, &EvalError{Message: "make-string expects 1 or 2 arguments"}
	}

	count, err := expectNonNegativeIndex(args[0], "make-string")
	if err != nil {
		return nil, err
	}

	fill := rune(0)
	if len(args) == 2 {
		ch, err := expectChar(args[1])
		if err != nil {
			return nil, err
		}
		fill = rune(ch)
	}

	runes := make([]rune, count)
	for i := range runes {
		runes[i] = fill
	}
	return string(runes), nil
}

func builtinString(args []any) (any, error) {
	runes := make([]rune, len(args))
	for i, arg := range args {
		ch, err := expectChar(arg)
		if err != nil {
			return nil, err
		}
		runes[i] = rune(ch)
	}
	return string(runes), nil
}

func builtinStringGreater(args []any) (any, error) {
	return builtinStringCompare("string>?", args, func(left, right string) bool {
		return left > right
	})
}

func builtinStringLessEqual(args []any) (any, error) {
	return builtinStringCompare("string<=?", args, func(left, right string) bool {
		return left <= right
	})
}

func builtinStringGreaterEqual(args []any) (any, error) {
	return builtinStringCompare("string>=?", args, func(left, right string) bool {
		return left >= right
	})
}
