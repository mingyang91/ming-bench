package ming

import "fmt"

type recordType struct {
	name       string
	fieldCount int
}

type recordValue struct {
	typ    *recordType
	fields []value
}

type recordFieldSpec struct {
	accessor *symbolExpr
	mutator  *symbolExpr
}

func (it *interpreter) evalDefineRecordType(scope *env, list *listExpr) (value, error) {
	if len(list.elements) < 4 {
		return nil, newEvalError(ErrSyntax, "define-record-type: expected type, constructor, and predicate", list.at)
	}

	typeName, ok := list.elements[1].(*symbolExpr)
	if !ok {
		return nil, newEvalError(ErrSyntax, "define-record-type: expected type name", list.elements[1].pos())
	}

	constructorForm, ok := list.elements[2].(*listExpr)
	if !ok || len(constructorForm.elements) == 0 {
		return nil, newEvalError(ErrSyntax, "define-record-type: expected constructor specification", list.elements[2].pos())
	}

	constructorName, ok := constructorForm.elements[0].(*symbolExpr)
	if !ok {
		return nil, newEvalError(ErrSyntax, "define-record-type: expected constructor name", constructorForm.elements[0].pos())
	}

	predicateName, ok := list.elements[3].(*symbolExpr)
	if !ok {
		return nil, newEvalError(ErrSyntax, "define-record-type: expected predicate name", list.elements[3].pos())
	}

	fields, fieldIndex, err := parseRecordFieldSpecs(list.elements[4:])
	if err != nil {
		return nil, err
	}

	constructorFields := make([]int, 0, len(constructorForm.elements)-1)
	for _, fieldExpr := range constructorForm.elements[1:] {
		fieldName, ok := fieldExpr.(*symbolExpr)
		if !ok {
			return nil, newEvalError(ErrSyntax, "define-record-type: expected constructor field name", fieldExpr.pos())
		}

		index, ok := fieldIndex[identifierName(fieldName)]
		if !ok {
			return nil, newEvalError(ErrSyntax, fmt.Sprintf("define-record-type: unknown field %s", fieldName.name), fieldName.at)
		}
		constructorFields = append(constructorFields, index)
	}

	recordType := &recordType{
		name:       typeName.name,
		fieldCount: len(fields),
	}

	it.defineSymbol(scope, constructorName, newRecordConstructorProc(recordType, constructorName.name, constructorFields))
	it.defineSymbol(scope, predicateName, newRecordPredicateProc(recordType, predicateName.name))
	for index, field := range fields {
		it.defineSymbol(scope, field.accessor, newRecordAccessorProc(recordType, field.accessor.name, index))
		if field.mutator != nil {
			it.defineSymbol(scope, field.mutator, newRecordMutatorProc(recordType, field.mutator.name, index))
		}
	}

	return voidValue{}, nil
}

func parseRecordFieldSpecs(fieldExprs []expr) ([]recordFieldSpec, map[string]int, error) {
	fields := make([]recordFieldSpec, 0, len(fieldExprs))
	fieldIndex := make(map[string]int, len(fieldExprs))
	for _, fieldExpr := range fieldExprs {
		fieldForm, ok := fieldExpr.(*listExpr)
		if !ok || len(fieldForm.elements) < 2 || len(fieldForm.elements) > 3 {
			return nil, nil, newEvalError(ErrSyntax, "define-record-type: expected field specification", fieldExpr.pos())
		}

		fieldName, ok := fieldForm.elements[0].(*symbolExpr)
		if !ok {
			return nil, nil, newEvalError(ErrSyntax, "define-record-type: expected field name", fieldForm.elements[0].pos())
		}

		accessorName, ok := fieldForm.elements[1].(*symbolExpr)
		if !ok {
			return nil, nil, newEvalError(ErrSyntax, "define-record-type: expected accessor name", fieldForm.elements[1].pos())
		}

		var mutatorName *symbolExpr
		if len(fieldForm.elements) == 3 {
			var ok bool
			mutatorName, ok = fieldForm.elements[2].(*symbolExpr)
			if !ok {
				return nil, nil, newEvalError(ErrSyntax, "define-record-type: expected mutator name", fieldForm.elements[2].pos())
			}
		}

		id := identifierName(fieldName)
		if _, exists := fieldIndex[id]; exists {
			return nil, nil, newEvalError(ErrSyntax, fmt.Sprintf("define-record-type: duplicate field %s", fieldName.name), fieldName.at)
		}

		fieldIndex[id] = len(fields)
		fields = append(fields, recordFieldSpec{
			accessor: accessorName,
			mutator:  mutatorName,
		})
	}

	return fields, fieldIndex, nil
}

func identifierName(sym *symbolExpr) string {
	if sym.key != "" {
		return sym.key
	}
	return sym.name
}

func newRecordConstructorProc(recordType *recordType, name string, fieldOrder []int) *builtinProc {
	return &builtinProc{
		name: name,
		fn: func(_ *interpreter, args []value, callPos position) (value, error) {
			if len(args) != len(fieldOrder) {
				return nil, wrongArgCount(callPos, name, fmt.Sprintf("expected %d arguments, got %d", len(fieldOrder), len(args)))
			}

			fields := make([]value, recordType.fieldCount)
			for i := range fields {
				fields[i] = voidValue{}
			}
			for argIndex, fieldIndex := range fieldOrder {
				fields[fieldIndex] = args[argIndex]
			}

			return &recordValue{
				typ:    recordType,
				fields: fields,
			}, nil
		},
	}
}

func newRecordPredicateProc(recordType *recordType, name string) *builtinProc {
	return &builtinProc{
		name: name,
		fn: func(_ *interpreter, args []value, callPos position) (value, error) {
			if len(args) != 1 {
				return nil, wrongArgCount(callPos, name, "expected exactly 1 argument")
			}

			record, ok := args[0].(*recordValue)
			return ok && record.typ == recordType, nil
		},
	}
}

func newRecordAccessorProc(recordType *recordType, name string, fieldIndex int) *builtinProc {
	return &builtinProc{
		name: name,
		fn: func(_ *interpreter, args []value, callPos position) (value, error) {
			if len(args) != 1 {
				return nil, wrongArgCount(callPos, name, "expected exactly 1 argument")
			}

			record, err := expectRecord(args[0], recordType, callPos, name)
			if err != nil {
				return nil, err
			}
			return record.fields[fieldIndex], nil
		},
	}
}

func newRecordMutatorProc(recordType *recordType, name string, fieldIndex int) *builtinProc {
	return &builtinProc{
		name: name,
		fn: func(_ *interpreter, args []value, callPos position) (value, error) {
			if len(args) != 2 {
				return nil, wrongArgCount(callPos, name, "expected exactly 2 arguments")
			}

			record, err := expectRecord(args[0], recordType, callPos, name)
			if err != nil {
				return nil, err
			}
			record.fields[fieldIndex] = args[1]
			return voidValue{}, nil
		},
	}
}

func expectRecord(v value, recordType *recordType, callPos position, name string) (*recordValue, error) {
	record, ok := v.(*recordValue)
	if !ok || record.typ != recordType {
		return nil, newEvalError(ErrTypeMismatch, fmt.Sprintf("%s: expected %s record", name, recordType.name), callPos)
	}
	return record, nil
}
