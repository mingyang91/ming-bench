package ming

import "fmt"

func registerL14Builtins(env *Env) {
	env.set("vector", &BuiltinFunc{Name: "vector", Fn: builtinVector})
	env.set("make-vector", &BuiltinFunc{Name: "make-vector", Fn: builtinMakeVector})
	env.set("vector-ref", &BuiltinFunc{Name: "vector-ref", Fn: builtinVectorRef})
	env.set("vector-set!", &BuiltinFunc{Name: "vector-set!", Fn: builtinVectorSet})
	env.set("vector-length", &BuiltinFunc{Name: "vector-length", Fn: builtinVectorLength})
	env.set("vector?", &BuiltinFunc{Name: "vector?", Fn: builtinVectorQ})
	env.set("vector->list", &BuiltinFunc{Name: "vector->list", Fn: builtinVectorToList})
	env.set("list->vector", &BuiltinFunc{Name: "list->vector", Fn: builtinListToVector})
}

func builtinVector(args []Value) (Value, error) {
	elems := make([]Value, len(args))
	copy(elems, args)
	return &VectorVal{Elems: elems}, nil
}

func builtinMakeVector(args []Value) (Value, error) {
	if len(args) < 1 || len(args) > 2 {
		return nil, &EvalError{Message: "make-vector: requires 1 or 2 arguments"}
	}
	n, ok := args[0].(*IntVal)
	if !ok {
		return nil, &EvalError{Message: "make-vector: size not a number"}
	}
	var fill Value = &IntVal{Val: 0}
	if len(args) == 2 {
		fill = args[1]
	}
	elems := make([]Value, n.Val)
	for i := range elems {
		elems[i] = fill
	}
	return &VectorVal{Elems: elems}, nil
}

func builtinVectorRef(args []Value) (Value, error) {
	if len(args) != 2 {
		return nil, &EvalError{Message: "vector-ref: requires exactly 2 arguments"}
	}
	v, ok := args[0].(*VectorVal)
	if !ok {
		return nil, &EvalError{Message: "vector-ref: not a vector"}
	}
	idx, ok := args[1].(*IntVal)
	if !ok {
		return nil, &EvalError{Message: "vector-ref: index not a number"}
	}
	if idx.Val < 0 || idx.Val >= int64(len(v.Elems)) {
		return nil, &EvalError{Message: fmt.Sprintf("vector-ref: index %d out of range [0, %d)", idx.Val, len(v.Elems))}
	}
	return v.Elems[idx.Val], nil
}

func builtinVectorSet(args []Value) (Value, error) {
	if len(args) != 3 {
		return nil, &EvalError{Message: "vector-set!: requires exactly 3 arguments"}
	}
	v, ok := args[0].(*VectorVal)
	if !ok {
		return nil, &EvalError{Message: "vector-set!: not a vector"}
	}
	idx, ok := args[1].(*IntVal)
	if !ok {
		return nil, &EvalError{Message: "vector-set!: index not a number"}
	}
	if idx.Val < 0 || idx.Val >= int64(len(v.Elems)) {
		return nil, &EvalError{Message: fmt.Sprintf("vector-set!: index %d out of range [0, %d)", idx.Val, len(v.Elems))}
	}
	v.Elems[idx.Val] = args[2]
	return &VoidVal{}, nil
}

func builtinVectorLength(args []Value) (Value, error) {
	if len(args) != 1 {
		return nil, &EvalError{Message: "vector-length: requires exactly 1 argument"}
	}
	v, ok := args[0].(*VectorVal)
	if !ok {
		return nil, &EvalError{Message: "vector-length: not a vector"}
	}
	return &IntVal{Val: int64(len(v.Elems))}, nil
}

func builtinVectorQ(args []Value) (Value, error) {
	if len(args) != 1 {
		return nil, &EvalError{Message: "vector?: requires exactly 1 argument"}
	}
	_, ok := args[0].(*VectorVal)
	return &BoolVal{Val: ok}, nil
}

func builtinVectorToList(args []Value) (Value, error) {
	if len(args) != 1 {
		return nil, &EvalError{Message: "vector->list: requires exactly 1 argument"}
	}
	v, ok := args[0].(*VectorVal)
	if !ok {
		return nil, &EvalError{Message: "vector->list: not a vector"}
	}
	result := Value(&NilVal{})
	for i := len(v.Elems) - 1; i >= 0; i-- {
		result = &PairVal{Car: v.Elems[i], Cdr: result}
	}
	return result, nil
}

func builtinListToVector(args []Value) (Value, error) {
	if len(args) != 1 {
		return nil, &EvalError{Message: "list->vector: requires exactly 1 argument"}
	}
	var elems []Value
	cur := args[0]
	for {
		if _, ok := cur.(*NilVal); ok {
			break
		}
		p, ok := cur.(*PairVal)
		if !ok {
			return nil, &EvalError{Message: "list->vector: not a proper list"}
		}
		elems = append(elems, p.Car)
		cur = p.Cdr
	}
	return &VectorVal{Elems: elems}, nil
}
