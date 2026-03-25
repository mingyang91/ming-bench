package ming

import "fmt"

// RecordType describes a record type created by define-record-type.
type RecordType struct {
	Name   string
	Fields []string
}

// RecordVal is an instance of a RecordType.
type RecordVal struct {
	Type   *RecordType
	Fields map[string]Value
}

func (v *RecordVal) String() string {
	return fmt.Sprintf("#<%s>", v.Type.Name)
}

// evalDefineRecordType handles (define-record-type <name> (constructor field ...) predicate (field accessor) ...)
func evalDefineRecordType(e *ListExpr, env *Env) (Value, error) {
	// (define-record-type <name> (constructor field...) predicate (field accessor) ...)
	if len(e.Items) < 4 {
		return nil, &EvalError{Message: fmt.Sprintf("%d:%d: define-record-type: bad syntax", e.Ln, e.Cl)}
	}

	// 1. Type name
	nameSym, ok := e.Items[1].(*SymbolExpr)
	if !ok {
		return nil, &EvalError{Message: fmt.Sprintf("%d:%d: define-record-type: expected type name", e.Ln, e.Cl)}
	}

	// 2. Constructor: (constructor-name field ...)
	consExpr, ok := e.Items[2].(*ListExpr)
	if !ok || len(consExpr.Items) < 1 {
		return nil, &EvalError{Message: fmt.Sprintf("%d:%d: define-record-type: expected constructor", e.Ln, e.Cl)}
	}
	consSym, ok := consExpr.Items[0].(*SymbolExpr)
	if !ok {
		return nil, &EvalError{Message: fmt.Sprintf("%d:%d: define-record-type: expected constructor name", e.Ln, e.Cl)}
	}
	consName := consSym.Name
	consFields := make([]string, 0, len(consExpr.Items)-1)
	for _, item := range consExpr.Items[1:] {
		s, ok := item.(*SymbolExpr)
		if !ok {
			return nil, &EvalError{Message: fmt.Sprintf("%d:%d: define-record-type: expected field name in constructor", e.Ln, e.Cl)}
		}
		consFields = append(consFields, s.Name)
	}

	// 3. Predicate
	predSym, ok := e.Items[3].(*SymbolExpr)
	if !ok {
		return nil, &EvalError{Message: fmt.Sprintf("%d:%d: define-record-type: expected predicate name", e.Ln, e.Cl)}
	}
	predName := predSym.Name

	// 4. Field clauses: (field accessor)
	type fieldClause struct {
		field    string
		accessor string
	}
	var clauses []fieldClause
	for _, item := range e.Items[4:] {
		fc, ok := item.(*ListExpr)
		if !ok || len(fc.Items) < 2 {
			return nil, &EvalError{Message: fmt.Sprintf("%d:%d: define-record-type: bad field clause", e.Ln, e.Cl)}
		}
		fieldSym, ok := fc.Items[0].(*SymbolExpr)
		if !ok {
			return nil, &EvalError{Message: fmt.Sprintf("%d:%d: define-record-type: expected field name", e.Ln, e.Cl)}
		}
		accSym, ok := fc.Items[1].(*SymbolExpr)
		if !ok {
			return nil, &EvalError{Message: fmt.Sprintf("%d:%d: define-record-type: expected accessor name", e.Ln, e.Cl)}
		}
		clauses = append(clauses, fieldClause{field: fieldSym.Name, accessor: accSym.Name})
	}

	// Create the record type
	rt := &RecordType{
		Name:   nameSym.Name,
		Fields: consFields,
	}

	// Define constructor
	env.set(consName, &BuiltinFunc{
		Name: consName,
		Fn: func(args []Value) (Value, error) {
			if len(args) != len(consFields) {
				return nil, &EvalError{Message: fmt.Sprintf("%s: expected %d arguments, got %d", consName, len(consFields), len(args))}
			}
			fields := make(map[string]Value, len(consFields))
			for i, name := range consFields {
				fields[name] = args[i]
			}
			return &RecordVal{Type: rt, Fields: fields}, nil
		},
	})

	// Define predicate
	env.set(predName, &BuiltinFunc{
		Name: predName,
		Fn: func(args []Value) (Value, error) {
			if len(args) != 1 {
				return nil, &EvalError{Message: fmt.Sprintf("%s: expected 1 argument", predName)}
			}
			rec, ok := args[0].(*RecordVal)
			return &BoolVal{Val: ok && rec.Type == rt}, nil
		},
	})

	// Define accessors
	for _, c := range clauses {
		fieldName := c.field
		accName := c.accessor
		env.set(accName, &BuiltinFunc{
			Name: accName,
			Fn: func(args []Value) (Value, error) {
				if len(args) != 1 {
					return nil, &EvalError{Message: fmt.Sprintf("%s: expected 1 argument", accName)}
				}
				rec, ok := args[0].(*RecordVal)
				if !ok || rec.Type != rt {
					return nil, &EvalError{Message: fmt.Sprintf("%s: not a %s", accName, rt.Name)}
				}
				return rec.Fields[fieldName], nil
			},
		})
	}

	return &VoidVal{}, nil
}
