package ming

import (
	"fmt"
	"math"
)

// cXXr compositions - generate all 2, 3, and 4 level compositions of car/cdr
func makeCxr(pattern string) func([]Value) (Value, error) {
	name := "c" + pattern + "r"
	return func(args []Value) (Value, error) {
		if len(args) != 1 {
			return nil, &EvalError{Message: fmt.Sprintf("%s: requires exactly 1 argument", name)}
		}
		v := args[0]
		// Apply pattern right to left: caddr = car(cdr(cdr(x)))
		for i := len(pattern) - 1; i >= 0; i-- {
			p, ok := v.(*PairVal)
			if !ok {
				return nil, &EvalError{Message: fmt.Sprintf("%s: not a pair", name)}
			}
			if pattern[i] == 'a' {
				v = p.Car
			} else {
				v = p.Cdr
			}
		}
		return v, nil
	}
}

func builtinForEach(args []Value) (Value, error) {
	if len(args) < 2 {
		return nil, &EvalError{Message: "for-each: requires at least 2 arguments"}
	}
	fn := args[0]
	lists := args[1:]

	cursors := make([]Value, len(lists))
	copy(cursors, lists)

	for {
		allDone := false
		for _, c := range cursors {
			if _, ok := c.(*NilVal); ok {
				allDone = true
				break
			}
		}
		if allDone {
			break
		}

		callArgs := make([]Value, len(cursors))
		for i, c := range cursors {
			p, ok := c.(*PairVal)
			if !ok {
				return nil, &EvalError{Message: "for-each: not a proper list"}
			}
			callArgs[i] = p.Car
			cursors[i] = p.Cdr
		}

		var err error
		switch f := fn.(type) {
		case *BuiltinFunc:
			_, err = f.Fn(callArgs)
		case *LambdaVal:
			_, err = resolveTC(applyLambda(f, callArgs, 0, 0))
		case *CaseLambdaVal:
			_, err = resolveTC(applyCaseLambda(f, callArgs, 0, 0))
		default:
			return nil, &EvalError{Message: fmt.Sprintf("for-each: not a procedure: %s", fn.String())}
		}
		if err != nil {
			return nil, err
		}
	}
	return &VoidVal{}, nil
}

func builtinReverse(args []Value) (Value, error) {
	if len(args) != 1 {
		return nil, &EvalError{Message: "reverse: requires exactly 1 argument"}
	}
	var result Value = &NilVal{}
	cur := args[0]
	for {
		switch v := cur.(type) {
		case *NilVal:
			return result, nil
		case *PairVal:
			result = &PairVal{Car: v.Car, Cdr: result}
			cur = v.Cdr
		default:
			return nil, &EvalError{Message: "reverse: not a proper list"}
		}
	}
}

func builtinAssq(args []Value) (Value, error) {
	if len(args) != 2 {
		return nil, &EvalError{Message: "assq: requires exactly 2 arguments"}
	}
	key := args[0]
	cur := args[1]
	for {
		switch v := cur.(type) {
		case *NilVal:
			return &BoolVal{Val: false}, nil
		case *PairVal:
			p, ok := v.Car.(*PairVal)
			if !ok {
				return nil, &EvalError{Message: "assq: not an alist"}
			}
			if isEq(key, p.Car) {
				return v.Car, nil
			}
			cur = v.Cdr
		default:
			return nil, &EvalError{Message: "assq: not a proper list"}
		}
	}
}

func builtinMemq(args []Value) (Value, error) {
	if len(args) != 2 {
		return nil, &EvalError{Message: "memq: requires exactly 2 arguments"}
	}
	key := args[0]
	cur := args[1]
	for {
		switch v := cur.(type) {
		case *NilVal:
			return &BoolVal{Val: false}, nil
		case *PairVal:
			if isEq(key, v.Car) {
				return v, nil
			}
			cur = v.Cdr
		default:
			return nil, &EvalError{Message: "memq: not a proper list"}
		}
	}
}

func builtinMember(args []Value) (Value, error) {
	if len(args) != 2 {
		return nil, &EvalError{Message: "member: requires exactly 2 arguments"}
	}
	key := args[0]
	cur := args[1]
	for {
		switch v := cur.(type) {
		case *NilVal:
			return &BoolVal{Val: false}, nil
		case *PairVal:
			if valuesEqual(key, v.Car) {
				return v, nil
			}
			cur = v.Cdr
		default:
			return nil, &EvalError{Message: "member: not a proper list"}
		}
	}
}

func builtinAssv(args []Value) (Value, error) {
	if len(args) != 2 {
		return nil, &EvalError{Message: "assv: requires exactly 2 arguments"}
	}
	key := args[0]
	cur := args[1]
	for {
		switch v := cur.(type) {
		case *NilVal:
			return &BoolVal{Val: false}, nil
		case *PairVal:
			p, ok := v.Car.(*PairVal)
			if !ok {
				return nil, &EvalError{Message: "assv: not an alist"}
			}
			if isEqv(key, p.Car) {
				return v.Car, nil
			}
			cur = v.Cdr
		default:
			return nil, &EvalError{Message: "assv: not a proper list"}
		}
	}
}

func builtinError(args []Value) (Value, error) {
	if len(args) == 0 {
		return nil, &EvalError{Message: "error"}
	}
	msg := displayValue(args[0])
	for _, a := range args[1:] {
		msg += " " + displayValue(a)
	}
	return nil, &EvalError{Message: msg}
}

func builtinGcd(args []Value) (Value, error) {
	if len(args) == 0 {
		return &IntVal{Val: 0}, nil
	}
	result := int64(0)
	for _, a := range args {
		n, ok := a.(*IntVal)
		if !ok {
			return nil, &EvalError{Message: "gcd: not an integer"}
		}
		v := n.Val
		if v < 0 {
			v = -v
		}
		result = gcd64(result, v)
	}
	return &IntVal{Val: result}, nil
}

func gcd64(a, b int64) int64 {
	for b != 0 {
		a, b = b, a%b
	}
	return a
}

func builtinLcm(args []Value) (Value, error) {
	if len(args) == 0 {
		return &IntVal{Val: 1}, nil
	}
	result := int64(1)
	for _, a := range args {
		n, ok := a.(*IntVal)
		if !ok {
			return nil, &EvalError{Message: "lcm: not an integer"}
		}
		v := n.Val
		if v < 0 {
			v = -v
		}
		if v == 0 {
			return &IntVal{Val: 0}, nil
		}
		result = result / gcd64(result, v) * v
	}
	return &IntVal{Val: result}, nil
}

func builtinMemv(args []Value) (Value, error) {
	if len(args) != 2 {
		return nil, &EvalError{Message: "memv: requires exactly 2 arguments"}
	}
	key := args[0]
	cur := args[1]
	for {
		switch v := cur.(type) {
		case *NilVal:
			return &BoolVal{Val: false}, nil
		case *PairVal:
			if isEqv(key, v.Car) {
				return v, nil
			}
			cur = v.Cdr
		default:
			return nil, &EvalError{Message: "memv: not a proper list"}
		}
	}
}

func builtinMakeString(args []Value) (Value, error) {
	if len(args) < 1 || len(args) > 2 {
		return nil, &EvalError{Message: "make-string: requires 1 or 2 arguments"}
	}
	n, ok := args[0].(*IntVal)
	if !ok {
		return nil, &EvalError{Message: "make-string: first argument must be integer"}
	}
	ch := rune(' ')
	if len(args) == 2 {
		c, ok := args[1].(*CharVal)
		if !ok {
			return nil, &EvalError{Message: "make-string: second argument must be char"}
		}
		ch = c.Val
	}
	s := make([]rune, n.Val)
	for i := range s {
		s[i] = ch
	}
	return &StringVal{Val: string(s), Mutable: true}, nil
}

func builtinString(args []Value) (Value, error) {
	runes := make([]rune, len(args))
	for i, a := range args {
		c, ok := a.(*CharVal)
		if !ok {
			return nil, &EvalError{Message: "string: arguments must be chars"}
		}
		runes[i] = c.Val
	}
	return &StringVal{Val: string(runes), Mutable: true}, nil
}

func builtinStringGtQ(args []Value) (Value, error) {
	if len(args) != 2 {
		return nil, &EvalError{Message: "string>?: requires exactly 2 arguments"}
	}
	a, ok1 := args[0].(*StringVal)
	b, ok2 := args[1].(*StringVal)
	if !ok1 || !ok2 {
		return nil, &EvalError{Message: "string>?: arguments must be strings"}
	}
	return &BoolVal{Val: a.Val > b.Val}, nil
}

func builtinStringLeQ(args []Value) (Value, error) {
	if len(args) != 2 {
		return nil, &EvalError{Message: "string<=?: requires exactly 2 arguments"}
	}
	a, ok1 := args[0].(*StringVal)
	b, ok2 := args[1].(*StringVal)
	if !ok1 || !ok2 {
		return nil, &EvalError{Message: "string<=?: arguments must be strings"}
	}
	return &BoolVal{Val: a.Val <= b.Val}, nil
}

func builtinStringGeQ(args []Value) (Value, error) {
	if len(args) != 2 {
		return nil, &EvalError{Message: "string>=?: requires exactly 2 arguments"}
	}
	a, ok1 := args[0].(*StringVal)
	b, ok2 := args[1].(*StringVal)
	if !ok1 || !ok2 {
		return nil, &EvalError{Message: "string>=?: arguments must be strings"}
	}
	return &BoolVal{Val: a.Val >= b.Val}, nil
}

func isEqv(a, b Value) bool {
	if a == b {
		return true
	}
	switch av := a.(type) {
	case *IntVal:
		if bv, ok := b.(*IntVal); ok {
			return av.Val == bv.Val
		}
	case *FloatVal:
		if bv, ok := b.(*FloatVal); ok {
			return av.Val == bv.Val
		}
	case *BoolVal:
		if bv, ok := b.(*BoolVal); ok {
			return av.Val == bv.Val
		}
	case *CharVal:
		if bv, ok := b.(*CharVal); ok {
			return av.Val == bv.Val
		}
	case *SymbolVal:
		if bv, ok := b.(*SymbolVal); ok {
			return av.Name == bv.Name
		}
	case *NilVal:
		_, ok := b.(*NilVal)
		return ok
	}
	return false
}

func toFloat(v Value) (float64, bool) {
	switch n := v.(type) {
	case *IntVal:
		return float64(n.Val), true
	case *FloatVal:
		return n.Val, true
	case *RationalVal:
		return float64(n.Num) / float64(n.Den), true
	}
	return 0, false
}

func builtinTruncate(args []Value) (Value, error) {
	if len(args) != 1 {
		return nil, &EvalError{Message: "truncate: requires exactly 1 argument"}
	}
	switch n := args[0].(type) {
	case *IntVal:
		return n, nil
	case *FloatVal:
		return &FloatVal{Val: math.Trunc(n.Val)}, nil
	case *RationalVal:
		return &IntVal{Val: n.Num / n.Den}, nil
	}
	return nil, &EvalError{Message: "truncate: not a number"}
}

func builtinRound(args []Value) (Value, error) {
	if len(args) != 1 {
		return nil, &EvalError{Message: "round: requires exactly 1 argument"}
	}
	switch n := args[0].(type) {
	case *IntVal:
		return n, nil
	case *FloatVal:
		return &FloatVal{Val: math.RoundToEven(n.Val)}, nil
	case *RationalVal:
		return &IntVal{Val: int64(math.RoundToEven(float64(n.Num) / float64(n.Den)))}, nil
	}
	return nil, &EvalError{Message: "round: not a number"}
}

func builtinFloor(args []Value) (Value, error) {
	if len(args) != 1 {
		return nil, &EvalError{Message: "floor: requires exactly 1 argument"}
	}
	switch n := args[0].(type) {
	case *IntVal:
		return n, nil
	case *FloatVal:
		return &FloatVal{Val: math.Floor(n.Val)}, nil
	case *RationalVal:
		q := n.Num / n.Den
		if (n.Num < 0) != (n.Den < 0) && n.Num%n.Den != 0 {
			q--
		}
		return &IntVal{Val: q}, nil
	}
	return nil, &EvalError{Message: "floor: not a number"}
}

func builtinCeiling(args []Value) (Value, error) {
	if len(args) != 1 {
		return nil, &EvalError{Message: "ceiling: requires exactly 1 argument"}
	}
	switch n := args[0].(type) {
	case *IntVal:
		return n, nil
	case *FloatVal:
		return &FloatVal{Val: math.Ceil(n.Val)}, nil
	case *RationalVal:
		q := n.Num / n.Den
		if (n.Num > 0 || (n.Num < 0) == (n.Den < 0)) && n.Num%n.Den != 0 {
			q++
		}
		return &IntVal{Val: q}, nil
	}
	return nil, &EvalError{Message: "ceiling: not a number"}
}

func builtinExact(args []Value) (Value, error) {
	if len(args) != 1 {
		return nil, &EvalError{Message: "exact: requires exactly 1 argument"}
	}
	switch n := args[0].(type) {
	case *IntVal:
		return n, nil
	case *FloatVal:
		return &IntVal{Val: int64(n.Val)}, nil
	case *RationalVal:
		return n, nil
	}
	return nil, &EvalError{Message: "exact: not a number"}
}

func builtinInexact(args []Value) (Value, error) {
	if len(args) != 1 {
		return nil, &EvalError{Message: "inexact: requires exactly 1 argument"}
	}
	f, ok := toFloat(args[0])
	if !ok {
		return nil, &EvalError{Message: "inexact: not a number"}
	}
	return &FloatVal{Val: f}, nil
}

func isEq(a, b Value) bool {
	if a == b {
		return true
	}
	switch av := a.(type) {
	case *IntVal:
		if bv, ok := b.(*IntVal); ok {
			return av.Val == bv.Val
		}
	case *BoolVal:
		if bv, ok := b.(*BoolVal); ok {
			return av.Val == bv.Val
		}
	case *CharVal:
		if bv, ok := b.(*CharVal); ok {
			return av.Val == bv.Val
		}
	case *SymbolVal:
		if bv, ok := b.(*SymbolVal); ok {
			return av.Name == bv.Name
		}
	case *NilVal:
		_, ok := b.(*NilVal)
		return ok
	}
	return false
}

func registerL17Builtins(env *Env) {
	// cXXr compositions (2-level)
	for _, a := range "ad" {
		for _, b := range "ad" {
			pat := string([]rune{a, b})
			name := "c" + pat + "r"
			env.set(name, &BuiltinFunc{Name: name, Fn: makeCxr(pat)})
		}
	}
	// cXXXr compositions (3-level)
	for _, a := range "ad" {
		for _, b := range "ad" {
			for _, c := range "ad" {
				pat := string([]rune{a, b, c})
				name := "c" + pat + "r"
				env.set(name, &BuiltinFunc{Name: name, Fn: makeCxr(pat)})
			}
		}
	}
	// cXXXXr compositions (4-level)
	for _, a := range "ad" {
		for _, b := range "ad" {
			for _, c := range "ad" {
				for _, d := range "ad" {
					pat := string([]rune{a, b, c, d})
					name := "c" + pat + "r"
					env.set(name, &BuiltinFunc{Name: name, Fn: makeCxr(pat)})
				}
			}
		}
	}

	env.set("for-each", &BuiltinFunc{Name: "for-each", Fn: builtinForEach})
	env.set("reverse", &BuiltinFunc{Name: "reverse", Fn: builtinReverse})
	env.set("assq", &BuiltinFunc{Name: "assq", Fn: builtinAssq})
	env.set("assv", &BuiltinFunc{Name: "assv", Fn: builtinAssv})
	env.set("memq", &BuiltinFunc{Name: "memq", Fn: builtinMemq})
	env.set("member", &BuiltinFunc{Name: "member", Fn: builtinMember})
	env.set("error", &BuiltinFunc{Name: "error", Fn: builtinError})
	env.set("gcd", &BuiltinFunc{Name: "gcd", Fn: builtinGcd})
	env.set("lcm", &BuiltinFunc{Name: "lcm", Fn: builtinLcm})
	env.set("memv", &BuiltinFunc{Name: "memv", Fn: builtinMemv})
	env.set("make-string", &BuiltinFunc{Name: "make-string", Fn: builtinMakeString})
	env.set("string", &BuiltinFunc{Name: "string", Fn: builtinString})
	env.set("string>?", &BuiltinFunc{Name: "string>?", Fn: builtinStringGtQ})
	env.set("string<=?", &BuiltinFunc{Name: "string<=?", Fn: builtinStringLeQ})
	env.set("string>=?", &BuiltinFunc{Name: "string>=?", Fn: builtinStringGeQ})
	env.set("truncate", &BuiltinFunc{Name: "truncate", Fn: builtinTruncate})
	env.set("round", &BuiltinFunc{Name: "round", Fn: builtinRound})
	env.set("floor", &BuiltinFunc{Name: "floor", Fn: builtinFloor})
	env.set("ceiling", &BuiltinFunc{Name: "ceiling", Fn: builtinCeiling})
	env.set("exact", &BuiltinFunc{Name: "exact", Fn: builtinExact})
	env.set("inexact", &BuiltinFunc{Name: "inexact", Fn: builtinInexact})
}
