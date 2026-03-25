package ming

import (
	"fmt"
	"strings"
	"unicode"
)

func builtinAbs(args []Value) (Value, error) {
	if len(args) != 1 {
		return nil, &EvalError{Message: "abs: requires exactly 1 argument"}
	}
	n, ok := args[0].(*IntVal)
	if !ok {
		return nil, &EvalError{Message: "abs: not a number"}
	}
	v := n.Val
	if v < 0 {
		v = -v
	}
	return &IntVal{Val: v}, nil
}

func builtinModulo(args []Value) (Value, error) {
	if len(args) != 2 {
		return nil, &EvalError{Message: "modulo: requires exactly 2 arguments"}
	}
	a, ok := args[0].(*IntVal)
	if !ok {
		return nil, &EvalError{Message: "modulo: not a number"}
	}
	b, ok2 := args[1].(*IntVal)
	if !ok2 {
		return nil, &EvalError{Message: "modulo: not a number"}
	}
	if b.Val == 0 {
		return nil, &EvalError{Message: "modulo: division by zero"}
	}
	// modulo takes the sign of the divisor
	r := a.Val % b.Val
	if r != 0 && (r > 0) != (b.Val > 0) {
		r += b.Val
	}
	return &IntVal{Val: r}, nil
}

func builtinRemainder(args []Value) (Value, error) {
	if len(args) != 2 {
		return nil, &EvalError{Message: "remainder: requires exactly 2 arguments"}
	}
	a, ok := args[0].(*IntVal)
	if !ok {
		return nil, &EvalError{Message: "remainder: not a number"}
	}
	b, ok2 := args[1].(*IntVal)
	if !ok2 {
		return nil, &EvalError{Message: "remainder: not a number"}
	}
	if b.Val == 0 {
		return nil, &EvalError{Message: "remainder: division by zero"}
	}
	// remainder takes the sign of the dividend (Go's % does this)
	return &IntVal{Val: a.Val % b.Val}, nil
}

func builtinQuotient(args []Value) (Value, error) {
	if len(args) != 2 {
		return nil, &EvalError{Message: "quotient: requires exactly 2 arguments"}
	}
	a, ok := args[0].(*IntVal)
	if !ok {
		return nil, &EvalError{Message: "quotient: not a number"}
	}
	b, ok2 := args[1].(*IntVal)
	if !ok2 {
		return nil, &EvalError{Message: "quotient: not a number"}
	}
	if b.Val == 0 {
		return nil, &EvalError{Message: "quotient: division by zero"}
	}
	return &IntVal{Val: a.Val / b.Val}, nil
}

func builtinMin(args []Value) (Value, error) {
	if len(args) == 0 {
		return nil, &EvalError{Message: "min: requires at least 1 argument"}
	}
	best, ok := args[0].(*IntVal)
	if !ok {
		return nil, &EvalError{Message: "min: not a number"}
	}
	for _, a := range args[1:] {
		n, ok := a.(*IntVal)
		if !ok {
			return nil, &EvalError{Message: "min: not a number"}
		}
		if n.Val < best.Val {
			best = n
		}
	}
	return best, nil
}

func builtinMax(args []Value) (Value, error) {
	if len(args) == 0 {
		return nil, &EvalError{Message: "max: requires at least 1 argument"}
	}
	best, ok := args[0].(*IntVal)
	if !ok {
		return nil, &EvalError{Message: "max: not a number"}
	}
	for _, a := range args[1:] {
		n, ok := a.(*IntVal)
		if !ok {
			return nil, &EvalError{Message: "max: not a number"}
		}
		if n.Val > best.Val {
			best = n
		}
	}
	return best, nil
}

func builtinExpt(args []Value) (Value, error) {
	if len(args) != 2 {
		return nil, &EvalError{Message: "expt: requires exactly 2 arguments"}
	}
	base, ok := args[0].(*IntVal)
	if !ok {
		return nil, &EvalError{Message: "expt: not a number"}
	}
	exp, ok2 := args[1].(*IntVal)
	if !ok2 {
		return nil, &EvalError{Message: "expt: not a number"}
	}
	result := int64(1)
	b := base.Val
	e := exp.Val
	if e < 0 {
		return &IntVal{Val: 0}, nil
	}
	for e > 0 {
		if e%2 == 1 {
			result *= b
		}
		b *= b
		e /= 2
	}
	return &IntVal{Val: result}, nil
}

func builtinZeroQ(args []Value) (Value, error) {
	if len(args) != 1 {
		return nil, &EvalError{Message: "zero?: requires exactly 1 argument"}
	}
	n, ok := args[0].(*IntVal)
	if !ok {
		return nil, &EvalError{Message: "zero?: not a number"}
	}
	return &BoolVal{Val: n.Val == 0}, nil
}

func builtinPositiveQ(args []Value) (Value, error) {
	if len(args) != 1 {
		return nil, &EvalError{Message: "positive?: requires exactly 1 argument"}
	}
	n, ok := args[0].(*IntVal)
	if !ok {
		return nil, &EvalError{Message: "positive?: not a number"}
	}
	return &BoolVal{Val: n.Val > 0}, nil
}

func builtinNegativeQ(args []Value) (Value, error) {
	if len(args) != 1 {
		return nil, &EvalError{Message: "negative?: requires exactly 1 argument"}
	}
	n, ok := args[0].(*IntVal)
	if !ok {
		return nil, &EvalError{Message: "negative?: not a number"}
	}
	return &BoolVal{Val: n.Val < 0}, nil
}

func builtinOddQ(args []Value) (Value, error) {
	if len(args) != 1 {
		return nil, &EvalError{Message: "odd?: requires exactly 1 argument"}
	}
	n, ok := args[0].(*IntVal)
	if !ok {
		return nil, &EvalError{Message: "odd?: not a number"}
	}
	return &BoolVal{Val: n.Val%2 != 0}, nil
}

func builtinEvenQ(args []Value) (Value, error) {
	if len(args) != 1 {
		return nil, &EvalError{Message: "even?: requires exactly 1 argument"}
	}
	n, ok := args[0].(*IntVal)
	if !ok {
		return nil, &EvalError{Message: "even?: not a number"}
	}
	return &BoolVal{Val: n.Val%2 == 0}, nil
}

func builtinListRef(args []Value) (Value, error) {
	if len(args) != 2 {
		return nil, &EvalError{Message: "list-ref: requires exactly 2 arguments"}
	}
	idx, ok := args[1].(*IntVal)
	if !ok {
		return nil, &EvalError{Message: "list-ref: index not a number"}
	}
	cur := args[0]
	for i := int64(0); i < idx.Val; i++ {
		p, ok := cur.(*PairVal)
		if !ok {
			return nil, &EvalError{Message: "list-ref: index out of range"}
		}
		cur = p.Cdr
	}
	p, ok := cur.(*PairVal)
	if !ok {
		return nil, &EvalError{Message: "list-ref: index out of range"}
	}
	return p.Car, nil
}

func builtinListTail(args []Value) (Value, error) {
	if len(args) != 2 {
		return nil, &EvalError{Message: "list-tail: requires exactly 2 arguments"}
	}
	idx, ok := args[1].(*IntVal)
	if !ok {
		return nil, &EvalError{Message: "list-tail: index not a number"}
	}
	cur := args[0]
	for i := int64(0); i < idx.Val; i++ {
		p, ok := cur.(*PairVal)
		if !ok {
			return nil, &EvalError{Message: "list-tail: index out of range"}
		}
		cur = p.Cdr
	}
	return cur, nil
}

func builtinListQ(args []Value) (Value, error) {
	if len(args) != 1 {
		return nil, &EvalError{Message: "list?: requires exactly 1 argument"}
	}
	cur := args[0]
	for {
		switch v := cur.(type) {
		case *NilVal:
			return &BoolVal{Val: true}, nil
		case *PairVal:
			cur = v.Cdr
		default:
			return &BoolVal{Val: false}, nil
		}
	}
}

func valuesEqual(a, b Value) bool {
	switch av := a.(type) {
	case *IntVal:
		if bv, ok := b.(*IntVal); ok {
			return av.Val == bv.Val
		}
		if bv, ok := b.(*RationalVal); ok {
			return bv.Den == 1 && av.Val == bv.Num
		}
	case *RationalVal:
		if bv, ok := b.(*RationalVal); ok {
			return av.Num == bv.Num && av.Den == bv.Den
		}
		if bv, ok := b.(*IntVal); ok {
			return av.Den == 1 && av.Num == bv.Val
		}
	case *FloatVal:
		if bv, ok := b.(*FloatVal); ok {
			return av.Val == bv.Val
		}
	case *BoolVal:
		if bv, ok := b.(*BoolVal); ok {
			return av.Val == bv.Val
		}
	case *StringVal:
		if bv, ok := b.(*StringVal); ok {
			return av.Val == bv.Val
		}
	case *SymbolVal:
		if bv, ok := b.(*SymbolVal); ok {
			return av.Name == bv.Name
		}
	case *CharVal:
		if bv, ok := b.(*CharVal); ok {
			return av.Val == bv.Val
		}
	case *NilVal:
		_, ok := b.(*NilVal)
		return ok
	case *PairVal:
		if bv, ok := b.(*PairVal); ok {
			return valuesEqual(av.Car, bv.Car) && valuesEqual(av.Cdr, bv.Cdr)
		}
	case *VectorVal:
		if bv, ok := b.(*VectorVal); ok {
			if len(av.Elems) != len(bv.Elems) {
				return false
			}
			for i := range av.Elems {
				if !valuesEqual(av.Elems[i], bv.Elems[i]) {
					return false
				}
			}
			return true
		}
	case *VoidVal:
		_, ok := b.(*VoidVal)
		return ok
	}
	return a == b
}

func builtinEqualQ(args []Value) (Value, error) {
	if len(args) != 2 {
		return nil, &EvalError{Message: "equal?: requires exactly 2 arguments"}
	}
	return &BoolVal{Val: valuesEqual(args[0], args[1])}, nil
}

func builtinEqQ(args []Value) (Value, error) {
	if len(args) != 2 {
		return nil, &EvalError{Message: "eq?: requires exactly 2 arguments"}
	}
	a, b := args[0], args[1]
	// eq? is identity comparison - same object or same simple value
	switch av := a.(type) {
	case *BoolVal:
		if bv, ok := b.(*BoolVal); ok {
			return &BoolVal{Val: av.Val == bv.Val}, nil
		}
	case *IntVal:
		if bv, ok := b.(*IntVal); ok {
			return &BoolVal{Val: av.Val == bv.Val}, nil
		}
	case *SymbolVal:
		if bv, ok := b.(*SymbolVal); ok {
			return &BoolVal{Val: av.Name == bv.Name}, nil
		}
	case *CharVal:
		if bv, ok := b.(*CharVal); ok {
			return &BoolVal{Val: av.Val == bv.Val}, nil
		}
	case *NilVal:
		_, ok := b.(*NilVal)
		return &BoolVal{Val: ok}, nil
	case *VoidVal:
		_, ok := b.(*VoidVal)
		return &BoolVal{Val: ok}, nil
	}
	return &BoolVal{Val: a == b}, nil
}

func builtinEqvQ(args []Value) (Value, error) {
	if len(args) != 2 {
		return nil, &EvalError{Message: "eqv?: requires exactly 2 arguments"}
	}
	a, b := args[0], args[1]
	switch av := a.(type) {
	case *BoolVal:
		if bv, ok := b.(*BoolVal); ok {
			return &BoolVal{Val: av.Val == bv.Val}, nil
		}
	case *IntVal:
		if bv, ok := b.(*IntVal); ok {
			return &BoolVal{Val: av.Val == bv.Val}, nil
		}
	case *FloatVal:
		if bv, ok := b.(*FloatVal); ok {
			return &BoolVal{Val: av.Val == bv.Val}, nil
		}
	case *RationalVal:
		if bv, ok := b.(*RationalVal); ok {
			return &BoolVal{Val: av.Num == bv.Num && av.Den == bv.Den}, nil
		}
	case *SymbolVal:
		if bv, ok := b.(*SymbolVal); ok {
			return &BoolVal{Val: av.Name == bv.Name}, nil
		}
	case *CharVal:
		if bv, ok := b.(*CharVal); ok {
			return &BoolVal{Val: av.Val == bv.Val}, nil
		}
	case *StringVal:
		if bv, ok := b.(*StringVal); ok {
			return &BoolVal{Val: av.Val == bv.Val}, nil
		}
	case *NilVal:
		_, ok := b.(*NilVal)
		return &BoolVal{Val: ok}, nil
	case *VoidVal:
		_, ok := b.(*VoidVal)
		return &BoolVal{Val: ok}, nil
	}
	return &BoolVal{Val: a == b}, nil
}

func builtinAssoc(args []Value) (Value, error) {
	if len(args) != 2 {
		return nil, &EvalError{Message: "assoc: requires exactly 2 arguments"}
	}
	key := args[0]
	cur := args[1]
	for {
		if _, ok := cur.(*NilVal); ok {
			return &BoolVal{Val: false}, nil
		}
		p, ok := cur.(*PairVal)
		if !ok {
			return nil, &EvalError{Message: "assoc: not a proper list"}
		}
		entry, ok := p.Car.(*PairVal)
		if !ok {
			return nil, &EvalError{Message: "assoc: entry is not a pair"}
		}
		if valuesEqual(key, entry.Car) {
			return entry, nil
		}
		cur = p.Cdr
	}
}

// builtinMap supports multiple list arguments: (map f list1 list2 ...)
func builtinMap(args []Value) (Value, error) {
	if len(args) < 2 {
		return nil, &EvalError{Message: "map: requires at least 2 arguments"}
	}
	fn := args[0]
	lists := args[1:]

	var results []Value
	cursors := make([]Value, len(lists))
	copy(cursors, lists)

	for {
		// Check if any list is exhausted
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

		// Extract car of each list
		callArgs := make([]Value, len(cursors))
		for i, c := range cursors {
			p, ok := c.(*PairVal)
			if !ok {
				return nil, &EvalError{Message: "map: not a proper list"}
			}
			callArgs[i] = p.Car
			cursors[i] = p.Cdr
		}

		// Apply function
		var result Value
		var err error
		switch f := fn.(type) {
		case *BuiltinFunc:
			result, err = f.Fn(callArgs)
		case *LambdaVal:
			result, err = applyLambda(f, callArgs, 0, 0)
		default:
			return nil, &EvalError{Message: fmt.Sprintf("map: not a procedure: %s", fn.String())}
		}
		if err != nil {
			return nil, err
		}
		results = append(results, result)
	}

	// Build result list
	res := Value(&NilVal{})
	for i := len(results) - 1; i >= 0; i-- {
		res = &PairVal{Car: results[i], Cdr: res}
	}
	return res, nil
}

// Character builtins

func builtinCharAlphabeticQ(args []Value) (Value, error) {
	if len(args) != 1 {
		return nil, &EvalError{Message: "char-alphabetic?: requires exactly 1 argument"}
	}
	c, ok := args[0].(*CharVal)
	if !ok {
		return nil, &EvalError{Message: "char-alphabetic?: not a character"}
	}
	return &BoolVal{Val: unicode.IsLetter(c.Val)}, nil
}

func builtinCharNumericQ(args []Value) (Value, error) {
	if len(args) != 1 {
		return nil, &EvalError{Message: "char-numeric?: requires exactly 1 argument"}
	}
	c, ok := args[0].(*CharVal)
	if !ok {
		return nil, &EvalError{Message: "char-numeric?: not a character"}
	}
	return &BoolVal{Val: unicode.IsDigit(c.Val)}, nil
}

func builtinCharUpcase(args []Value) (Value, error) {
	if len(args) != 1 {
		return nil, &EvalError{Message: "char-upcase: requires exactly 1 argument"}
	}
	c, ok := args[0].(*CharVal)
	if !ok {
		return nil, &EvalError{Message: "char-upcase: not a character"}
	}
	return &CharVal{Val: unicode.ToUpper(c.Val)}, nil
}

func builtinCharDowncase(args []Value) (Value, error) {
	if len(args) != 1 {
		return nil, &EvalError{Message: "char-downcase: requires exactly 1 argument"}
	}
	c, ok := args[0].(*CharVal)
	if !ok {
		return nil, &EvalError{Message: "char-downcase: not a character"}
	}
	return &CharVal{Val: unicode.ToLower(c.Val)}, nil
}

func builtinCharEqQ(args []Value) (Value, error) {
	if len(args) != 2 {
		return nil, &EvalError{Message: "char=?: requires exactly 2 arguments"}
	}
	a, ok := args[0].(*CharVal)
	if !ok {
		return nil, &EvalError{Message: "char=?: not a character"}
	}
	b, ok2 := args[1].(*CharVal)
	if !ok2 {
		return nil, &EvalError{Message: "char=?: not a character"}
	}
	return &BoolVal{Val: a.Val == b.Val}, nil
}

func builtinCharLtQ(args []Value) (Value, error) {
	if len(args) != 2 {
		return nil, &EvalError{Message: "char<?: requires exactly 2 arguments"}
	}
	a, ok := args[0].(*CharVal)
	if !ok {
		return nil, &EvalError{Message: "char<?: not a character"}
	}
	b, ok2 := args[1].(*CharVal)
	if !ok2 {
		return nil, &EvalError{Message: "char<?: not a character"}
	}
	return &BoolVal{Val: a.Val < b.Val}, nil
}

// String comparison builtins

func builtinStringEqQ(args []Value) (Value, error) {
	if len(args) != 2 {
		return nil, &EvalError{Message: "string=?: requires exactly 2 arguments"}
	}
	a, ok := args[0].(*StringVal)
	if !ok {
		return nil, &EvalError{Message: "string=?: not a string"}
	}
	b, ok2 := args[1].(*StringVal)
	if !ok2 {
		return nil, &EvalError{Message: "string=?: not a string"}
	}
	return &BoolVal{Val: a.Val == b.Val}, nil
}

func builtinStringLtQ(args []Value) (Value, error) {
	if len(args) != 2 {
		return nil, &EvalError{Message: "string<?: requires exactly 2 arguments"}
	}
	a, ok := args[0].(*StringVal)
	if !ok {
		return nil, &EvalError{Message: "string<?: not a string"}
	}
	b, ok2 := args[1].(*StringVal)
	if !ok2 {
		return nil, &EvalError{Message: "string<?: not a string"}
	}
	return &BoolVal{Val: a.Val < b.Val}, nil
}

func builtinStringCiEqQ(args []Value) (Value, error) {
	if len(args) != 2 {
		return nil, &EvalError{Message: "string-ci=?: requires exactly 2 arguments"}
	}
	a, ok := args[0].(*StringVal)
	if !ok {
		return nil, &EvalError{Message: "string-ci=?: not a string"}
	}
	b, ok2 := args[1].(*StringVal)
	if !ok2 {
		return nil, &EvalError{Message: "string-ci=?: not a string"}
	}
	return &BoolVal{Val: strings.EqualFold(a.Val, b.Val)}, nil
}

func builtinStringUpcase(args []Value) (Value, error) {
	if len(args) != 1 {
		return nil, &EvalError{Message: "string-upcase: requires exactly 1 argument"}
	}
	s, ok := args[0].(*StringVal)
	if !ok {
		return nil, &EvalError{Message: "string-upcase: not a string"}
	}
	return &StringVal{Val: strings.ToUpper(s.Val), Mutable: true}, nil
}

func builtinStringDowncase(args []Value) (Value, error) {
	if len(args) != 1 {
		return nil, &EvalError{Message: "string-downcase: requires exactly 1 argument"}
	}
	s, ok := args[0].(*StringVal)
	if !ok {
		return nil, &EvalError{Message: "string-downcase: not a string"}
	}
	return &StringVal{Val: strings.ToLower(s.Val), Mutable: true}, nil
}

func registerL09Builtins(env *Env) {
	env.set("abs", &BuiltinFunc{Name: "abs", Fn: builtinAbs})
	env.set("modulo", &BuiltinFunc{Name: "modulo", Fn: builtinModulo})
	env.set("remainder", &BuiltinFunc{Name: "remainder", Fn: builtinRemainder})
	env.set("quotient", &BuiltinFunc{Name: "quotient", Fn: builtinQuotient})
	env.set("min", &BuiltinFunc{Name: "min", Fn: builtinMin})
	env.set("max", &BuiltinFunc{Name: "max", Fn: builtinMax})
	env.set("expt", &BuiltinFunc{Name: "expt", Fn: builtinExpt})
	env.set("zero?", &BuiltinFunc{Name: "zero?", Fn: builtinZeroQ})
	env.set("positive?", &BuiltinFunc{Name: "positive?", Fn: builtinPositiveQ})
	env.set("negative?", &BuiltinFunc{Name: "negative?", Fn: builtinNegativeQ})
	env.set("odd?", &BuiltinFunc{Name: "odd?", Fn: builtinOddQ})
	env.set("even?", &BuiltinFunc{Name: "even?", Fn: builtinEvenQ})
	env.set("list-ref", &BuiltinFunc{Name: "list-ref", Fn: builtinListRef})
	env.set("list-tail", &BuiltinFunc{Name: "list-tail", Fn: builtinListTail})
	env.set("list?", &BuiltinFunc{Name: "list?", Fn: builtinListQ})
	env.set("assoc", &BuiltinFunc{Name: "assoc", Fn: builtinAssoc})
	env.set("map", &BuiltinFunc{Name: "map", Fn: builtinMap})
	env.set("eq?", &BuiltinFunc{Name: "eq?", Fn: builtinEqQ})
	env.set("eqv?", &BuiltinFunc{Name: "eqv?", Fn: builtinEqvQ})
	env.set("equal?", &BuiltinFunc{Name: "equal?", Fn: builtinEqualQ})

	// Character builtins
	env.set("char-alphabetic?", &BuiltinFunc{Name: "char-alphabetic?", Fn: builtinCharAlphabeticQ})
	env.set("char-numeric?", &BuiltinFunc{Name: "char-numeric?", Fn: builtinCharNumericQ})
	env.set("char-upcase", &BuiltinFunc{Name: "char-upcase", Fn: builtinCharUpcase})
	env.set("char-downcase", &BuiltinFunc{Name: "char-downcase", Fn: builtinCharDowncase})
	env.set("char=?", &BuiltinFunc{Name: "char=?", Fn: builtinCharEqQ})
	env.set("char<?", &BuiltinFunc{Name: "char<?", Fn: builtinCharLtQ})

	// String comparison builtins
	env.set("string=?", &BuiltinFunc{Name: "string=?", Fn: builtinStringEqQ})
	env.set("string<?", &BuiltinFunc{Name: "string<?", Fn: builtinStringLtQ})
	env.set("string-ci=?", &BuiltinFunc{Name: "string-ci=?", Fn: builtinStringCiEqQ})
	env.set("string-upcase", &BuiltinFunc{Name: "string-upcase", Fn: builtinStringUpcase})
	env.set("string-downcase", &BuiltinFunc{Name: "string-downcase", Fn: builtinStringDowncase})
}
