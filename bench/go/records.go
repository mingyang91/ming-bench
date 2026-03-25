package ming

import "fmt"

type recordType struct {
	name       string
	fieldNames []string
	fieldIndex map[string]int
}

type recordValue struct {
	recordType *recordType
	fields     []value
}

func evalDefineRecordType(args []node, env *environment) (value, error) {
	if len(args) < 3 {
		return nil, &EvalError{Message: "define-record-type expects a type name, constructor, and predicate"}
	}

	typeName, ok := symbolName(args[0])
	if !ok {
		return nil, &EvalError{Message: "define-record-type requires a type name symbol"}
	}

	constructorSpec, ok := args[1].(listNode)
	if !ok || len(constructorSpec.elements) == 0 {
		return nil, &EvalError{Message: "define-record-type constructor must be a non-empty list"}
	}

	constructorName, ok := symbolName(constructorSpec.elements[0])
	if !ok {
		return nil, &EvalError{Message: "define-record-type constructor requires a name"}
	}

	fieldNames := make([]string, len(constructorSpec.elements)-1)
	fieldIndex := make(map[string]int, len(fieldNames))
	for i, fieldExpr := range constructorSpec.elements[1:] {
		fieldName, ok := symbolName(fieldExpr)
		if !ok {
			return nil, &EvalError{Message: "record constructor fields must be symbols"}
		}
		if _, exists := fieldIndex[fieldName]; exists {
			return nil, &EvalError{Message: fmt.Sprintf("duplicate record field: %s", fieldName)}
		}
		fieldNames[i] = fieldName
		fieldIndex[fieldName] = i
	}

	predicateName, ok := symbolName(args[2])
	if !ok {
		return nil, &EvalError{Message: "define-record-type requires a predicate name"}
	}

	recType := &recordType{
		name:       typeName,
		fieldNames: fieldNames,
		fieldIndex: fieldIndex,
	}

	env.define(constructorName, makeRecordConstructor(recType, constructorName))
	env.define(predicateName, makeRecordPredicate(recType, predicateName))

	seenFields := map[string]struct{}{}
	for _, fieldExpr := range args[3:] {
		fieldSpec, ok := fieldExpr.(listNode)
		if !ok || len(fieldSpec.elements) < 2 || len(fieldSpec.elements) > 3 {
			return nil, &EvalError{Message: "record field specs must contain a field name, accessor, and optional mutator"}
		}

		fieldName, ok := symbolName(fieldSpec.elements[0])
		if !ok {
			return nil, &EvalError{Message: "record field names must be symbols"}
		}
		if _, exists := seenFields[fieldName]; exists {
			return nil, &EvalError{Message: fmt.Sprintf("duplicate record field: %s", fieldName)}
		}

		index, exists := recType.fieldIndex[fieldName]
		if !exists {
			return nil, &EvalError{Message: fmt.Sprintf("unknown record field: %s", fieldName)}
		}

		accessorName, ok := symbolName(fieldSpec.elements[1])
		if !ok {
			return nil, &EvalError{Message: "record accessors must be symbols"}
		}
		env.define(accessorName, makeRecordAccessor(recType, accessorName, index))

		if len(fieldSpec.elements) == 3 {
			mutatorName, ok := symbolName(fieldSpec.elements[2])
			if !ok {
				return nil, &EvalError{Message: "record mutators must be symbols"}
			}
			env.define(mutatorName, makeRecordMutator(recType, mutatorName, index))
		}

		seenFields[fieldName] = struct{}{}
	}

	return voidValue{}, nil
}

func makeRecordConstructor(recType *recordType, name string) builtinProc {
	return func(args []value) (value, error) {
		if len(args) != len(recType.fieldNames) {
			return nil, &EvalError{Message: fmt.Sprintf("%s expects exactly %d arguments", name, len(recType.fieldNames))}
		}
		return &recordValue{
			recordType: recType,
			fields:     copyValues(args),
		}, nil
	}
}

func makeRecordPredicate(recType *recordType, name string) builtinProc {
	return func(args []value) (value, error) {
		if len(args) != 1 {
			return nil, &EvalError{Message: fmt.Sprintf("%s expects exactly 1 argument", name)}
		}

		record, ok := args[0].(*recordValue)
		return booleanValue(ok && record.recordType == recType), nil
	}
}

func makeRecordAccessor(recType *recordType, name string, index int) builtinProc {
	return func(args []value) (value, error) {
		record, err := expectRecordValue(args, recType, name)
		if err != nil {
			return nil, err
		}
		return record.fields[index], nil
	}
}

func makeRecordMutator(recType *recordType, name string, index int) builtinProc {
	return func(args []value) (value, error) {
		if len(args) != 2 {
			return nil, &EvalError{Message: fmt.Sprintf("%s expects exactly 2 arguments", name)}
		}

		record, ok := args[0].(*recordValue)
		if !ok || record.recordType != recType {
			return nil, &EvalError{Message: fmt.Sprintf("%s expects a %s record", name, recType.name)}
		}

		record.fields[index] = args[1]
		return voidValue{}, nil
	}
}

func expectRecordValue(args []value, recType *recordType, name string) (*recordValue, error) {
	if len(args) != 1 {
		return nil, &EvalError{Message: fmt.Sprintf("%s expects exactly 1 argument", name)}
	}

	record, ok := args[0].(*recordValue)
	if !ok || record.recordType != recType {
		return nil, &EvalError{Message: fmt.Sprintf("%s expects a %s record", name, recType.name)}
	}
	return record, nil
}
