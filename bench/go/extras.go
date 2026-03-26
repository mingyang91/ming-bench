package ming

import (
	"math"
	"math/big"
	"strings"
)

func registerCxrBuiltins(root *env) {
	var build func(prefix string, depth int)
	build = func(prefix string, depth int) {
		if depth == 0 {
			name := "c" + prefix + "r"
			root.define(name, builtinProc{name: name, fn: makeCxrBuiltin(name, prefix)})
			return
		}
		build(prefix+"a", depth-1)
		build(prefix+"d", depth-1)
	}

	for depth := 2; depth <= 4; depth++ {
		build("", depth)
	}
}

func makeCxrBuiltin(name, pattern string) builtinFunc {
	return func(args []expr) (expr, error) {
		if len(args) != 1 {
			return nil, &EvalError{Message: name + " expects exactly 1 argument"}
		}

		current := args[0]
		for i := len(pattern) - 1; i >= 0; i-- {
			var ok bool
			switch pattern[i] {
			case 'a':
				current, ok = carValue(current)
			case 'd':
				current, ok = cdrValue(current)
			default:
				ok = false
			}
			if !ok {
				return nil, &EvalError{Message: name + " expects a non-empty list"}
			}
		}

		return current, nil
	}
}

func builtinReverse(args []expr) (expr, error) {
	if len(args) != 1 {
		return nil, &EvalError{Message: "reverse expects exactly 1 argument"}
	}

	items, ok := listElements(args[0])
	if !ok {
		return nil, &EvalError{Message: "reverse expects a list"}
	}

	reversed := make([]expr, len(items))
	for i := range items {
		reversed[len(items)-1-i] = items[i]
	}
	return properListFromSlice(reversed), nil
}

func builtinMemq(args []expr) (expr, error) {
	return memberBy(args, "memq", eqExpr)
}

func builtinMemv(args []expr) (expr, error) {
	return memberBy(args, "memv", eqvExpr)
}

func builtinMember(args []expr) (expr, error) {
	return memberBy(args, "member", equalExpr)
}

func memberBy(args []expr, name string, cmp func(expr, expr) bool) (expr, error) {
	if len(args) != 2 {
		return nil, &EvalError{Message: name + " expects exactly 2 arguments"}
	}

	seen := map[*pairExpr]struct{}{}
	current := args[1]
	for {
		switch v := current.(type) {
		case listExpr:
			for i, item := range v.items {
				if cmp(args[0], item) {
					return properListFromSlice(v.items[i:]), nil
				}
			}
			return boolExpr(false), nil
		case *pairExpr:
			if _, ok := seen[v]; ok {
				return nil, &EvalError{Message: name + " expects a list"}
			}
			seen[v] = struct{}{}
			if cmp(args[0], v.car) {
				return current, nil
			}
			current = v.cdr
		default:
			return nil, &EvalError{Message: name + " expects a list"}
		}
	}
}

func builtinAssq(args []expr) (expr, error) {
	return assocBy(args, "assq", eqExpr)
}

func builtinAssv(args []expr) (expr, error) {
	return assocBy(args, "assv", eqvExpr)
}

func assocBy(args []expr, name string, cmp func(expr, expr) bool) (expr, error) {
	if len(args) != 2 {
		return nil, &EvalError{Message: name + " expects exactly 2 arguments"}
	}

	seen := map[*pairExpr]struct{}{}
	current := args[1]
	for {
		switch v := current.(type) {
		case listExpr:
			for _, entry := range v.items {
				key, ok := carValue(entry)
				if !ok {
					return nil, &EvalError{Message: name + " expects association entries to be pairs"}
				}
				if cmp(args[0], key) {
					return entry, nil
				}
			}
			return boolExpr(false), nil
		case *pairExpr:
			if _, ok := seen[v]; ok {
				return nil, &EvalError{Message: name + " expects a list"}
			}
			seen[v] = struct{}{}
			key, ok := carValue(v.car)
			if !ok {
				return nil, &EvalError{Message: name + " expects association entries to be pairs"}
			}
			if cmp(args[0], key) {
				return v.car, nil
			}
			current = v.cdr
		default:
			return nil, &EvalError{Message: name + " expects a list"}
		}
	}
}

func builtinGCD(args []expr) (expr, error) {
	result := 0
	for _, arg := range args {
		value, ok := exactIntegerValue(arg)
		if !ok {
			return nil, &EvalError{Message: "gcd expects integer arguments"}
		}
		result = gcdInt(result, value)
	}
	return intExpr(result), nil
}

func builtinLCM(args []expr) (expr, error) {
	if len(args) == 0 {
		return intExpr(1), nil
	}

	result := 1
	for _, arg := range args {
		value, ok := exactIntegerValue(arg)
		if !ok {
			return nil, &EvalError{Message: "lcm expects integer arguments"}
		}
		result = lcmInt(result, value)
	}
	return intExpr(result), nil
}

func gcdInt(a, b int) int {
	if a < 0 {
		a = -a
	}
	if b < 0 {
		b = -b
	}
	for b != 0 {
		a, b = b, a%b
	}
	return a
}

func lcmInt(a, b int) int {
	if a == 0 || b == 0 {
		return 0
	}
	result := a / gcdInt(a, b) * b
	if result < 0 {
		return -result
	}
	return result
}

func builtinTruncate(args []expr) (expr, error) {
	number, err := unaryNumberArg(args, "truncate")
	if err != nil {
		return nil, err
	}

	if number.isInexact {
		return newInexactExpr(math.Trunc(number.inexact)), nil
	}

	result := new(big.Int).Quo(number.exact.Num(), number.exact.Denom())
	return exprFromRat(new(big.Rat).SetInt(result)), nil
}

func builtinRound(args []expr) (expr, error) {
	number, err := unaryNumberArg(args, "round")
	if err != nil {
		return nil, err
	}

	rounded := math.Round(numberToFloat(number))
	if number.isInexact {
		return newInexactExpr(rounded), nil
	}
	return exprFromRat(new(big.Rat).SetFloat64(rounded)), nil
}

func builtinMakeString(args []expr) (expr, error) {
	if len(args) != 1 && len(args) != 2 {
		return nil, &EvalError{Message: "make-string expects 1 or 2 arguments"}
	}

	length, ok := exactIntegerValue(args[0])
	if !ok {
		return nil, &EvalError{Message: "make-string expects an exact integer length"}
	}
	if length < 0 {
		return nil, &EvalError{Message: "make-string length must be non-negative"}
	}

	fill := rune(0)
	if len(args) == 2 {
		ch, ok := args[1].(charExpr)
		if !ok {
			return nil, &EvalError{Message: "make-string expects a character fill value"}
		}
		fill = rune(ch)
	}

	return newAllocatedString(strings.Repeat(string(fill), length)), nil
}

func builtinString(args []expr) (expr, error) {
	runes := make([]rune, len(args))
	for i, arg := range args {
		ch, ok := arg.(charExpr)
		if !ok {
			return nil, &EvalError{Message: "string expects character arguments"}
		}
		runes[i] = rune(ch)
	}
	return newAllocatedString(string(runes)), nil
}
