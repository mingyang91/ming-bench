package ming

import "fmt"

type recordType struct {
	name   string
	fields []recordFieldSpec
}

type recordFieldSpec struct {
	name        symbolExpr
	accessor    symbolExpr
	hasAccessor bool
	mutator     symbolExpr
	hasMutator  bool
}

type recordValue struct {
	typ    *recordType
	fields []any
}

func evalDefineRecordType(scope *env, args []any) (any, error) {
	if len(args) < 3 {
		return nil, &EvalError{Message: "define-record-type expects a type name, constructor, predicate, and fields"}
	}

	typeName, ok := args[0].(symbolExpr)
	if !ok {
		return nil, exprSourcePos(args[0]).errorf("define-record-type requires a type name")
	}

	constructorForm, ok := args[1].(listExpr)
	if !ok || len(constructorForm.elements) == 0 {
		return nil, exprSourcePos(args[1]).errorf("define-record-type constructor must be a list")
	}

	constructorName, ok := constructorForm.elements[0].(symbolExpr)
	if !ok {
		return nil, exprSourcePos(constructorForm.elements[0]).errorf("define-record-type constructor name must be a symbol")
	}

	constructorParams := constructorForm.elements[1:]
	for _, paramExpr := range constructorParams {
		if _, ok := paramExpr.(symbolExpr); !ok {
			return nil, exprSourcePos(paramExpr).errorf("define-record-type constructor parameters must be symbols")
		}
	}

	predicateName, ok := args[2].(symbolExpr)
	if !ok {
		return nil, exprSourcePos(args[2]).errorf("define-record-type predicate must be a symbol")
	}

	fields := make([]recordFieldSpec, len(args)-3)
	for i, fieldExpr := range args[3:] {
		field, err := parseRecordFieldSpec(fieldExpr)
		if err != nil {
			return nil, err
		}
		fields[i] = field
	}

	if len(constructorParams) != len(fields) {
		return nil, constructorForm.pos.errorf("define-record-type constructor arity does not match field count")
	}

	recordType := &recordType{
		name:   typeName.name,
		fields: fields,
	}

	fieldCount := len(fields)
	scope.defineSymbol(constructorName, builtinProc{
		name: constructorName.name,
		fn: func(args []any) (any, error) {
			if len(args) != fieldCount {
				return nil, &EvalError{Message: fmt.Sprintf("%s expects exactly %d arguments", constructorName.name, fieldCount)}
			}

			values := append([]any(nil), args...)
			return &recordValue{
				typ:    recordType,
				fields: values,
			}, nil
		},
	})

	scope.defineSymbol(predicateName, builtinProc{
		name: predicateName.name,
		fn: func(args []any) (any, error) {
			if len(args) != 1 {
				return nil, &EvalError{Message: fmt.Sprintf("%s expects exactly 1 argument", predicateName.name)}
			}

			record, ok := args[0].(*recordValue)
			return ok && record.typ == recordType, nil
		},
	})

	for index, field := range fields {
		index := index
		field := field

		if field.hasAccessor {
			scope.defineSymbol(field.accessor, builtinProc{
				name: field.accessor.name,
				fn: func(args []any) (any, error) {
					if len(args) != 1 {
						return nil, &EvalError{Message: fmt.Sprintf("%s expects exactly 1 argument", field.accessor.name)}
					}

					record, err := expectRecordOfType(args[0], recordType, field.accessor.name)
					if err != nil {
						return nil, err
					}
					return record.fields[index], nil
				},
			})
		}

		if field.hasMutator {
			scope.defineSymbol(field.mutator, builtinProc{
				name: field.mutator.name,
				fn: func(args []any) (any, error) {
					if len(args) != 2 {
						return nil, &EvalError{Message: fmt.Sprintf("%s expects exactly 2 arguments", field.mutator.name)}
					}

					record, err := expectRecordOfType(args[0], recordType, field.mutator.name)
					if err != nil {
						return nil, err
					}

					record.fields[index] = args[1]
					return voidValue{}, nil
				},
			})
		}
	}

	return voidValue{}, nil
}

func parseRecordFieldSpec(expr any) (recordFieldSpec, error) {
	switch field := expr.(type) {
	case symbolExpr:
		return recordFieldSpec{name: field}, nil
	case listExpr:
		if len(field.elements) == 0 || len(field.elements) > 3 {
			return recordFieldSpec{}, field.pos.errorf("record field spec must contain 1 to 3 identifiers")
		}

		name, ok := field.elements[0].(symbolExpr)
		if !ok {
			return recordFieldSpec{}, exprSourcePos(field.elements[0]).errorf("record field name must be a symbol")
		}

		spec := recordFieldSpec{name: name}
		if len(field.elements) >= 2 {
			accessor, ok := field.elements[1].(symbolExpr)
			if !ok {
				return recordFieldSpec{}, exprSourcePos(field.elements[1]).errorf("record accessor name must be a symbol")
			}
			spec.accessor = accessor
			spec.hasAccessor = true
		}

		if len(field.elements) == 3 {
			mutator, ok := field.elements[2].(symbolExpr)
			if !ok {
				return recordFieldSpec{}, exprSourcePos(field.elements[2]).errorf("record mutator name must be a symbol")
			}
			spec.mutator = mutator
			spec.hasMutator = true
		}

		return spec, nil
	default:
		return recordFieldSpec{}, exprSourcePos(expr).errorf("record field spec must be a symbol or list")
	}
}

func expectRecordOfType(value any, recordType *recordType, who string) (*recordValue, error) {
	record, ok := value.(*recordValue)
	if !ok || record.typ != recordType {
		return nil, &EvalError{Message: fmt.Sprintf("%s expects a %s record, got %s", who, recordType.name, typeName(value))}
	}
	return record, nil
}
