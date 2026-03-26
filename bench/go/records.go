package ming

import "fmt"

type recordTypeSpec struct {
	name               string
	fields             []recordFieldSpec
	constructorName    string
	constructorIndexes []int
}

type recordFieldSpec struct {
	name         string
	accessorName string
	mutatorName  string
}

type recordExpr struct {
	recordType *recordTypeSpec
	fields     []expr
}

type recordConstructorProc struct {
	recordType *recordTypeSpec
}

type recordPredicateProc struct {
	name       string
	recordType *recordTypeSpec
}

type recordAccessorProc struct {
	name       string
	recordType *recordTypeSpec
	fieldIndex int
}

type recordMutatorProc struct {
	name       string
	recordType *recordTypeSpec
	fieldIndex int
}

func evalDefineRecordType(environment *env, forms []expr) (expr, error) {
	if len(forms) < 3 {
		return nil, &EvalError{Message: "define-record-type expects a type name, constructor, predicate, and field specs"}
	}

	typeName, ok := forms[0].(symbolExpr)
	if !ok {
		return nil, &EvalError{Message: "define-record-type type name must be a symbol"}
	}

	constructorName, constructorFields, err := parseRecordConstructor(forms[1])
	if err != nil {
		return nil, err
	}

	predicateName, ok := forms[2].(symbolExpr)
	if !ok {
		return nil, &EvalError{Message: "define-record-type predicate name must be a symbol"}
	}

	fieldSpecs, fieldIndexes, err := parseRecordFields(forms[3:])
	if err != nil {
		return nil, err
	}

	constructorIndexes := make([]int, 0, len(constructorFields))
	seenConstructorFields := map[string]struct{}{}
	for _, fieldName := range constructorFields {
		if _, seen := seenConstructorFields[fieldName]; seen {
			return nil, &EvalError{Message: fmt.Sprintf("duplicate constructor field: %s", fieldName)}
		}
		seenConstructorFields[fieldName] = struct{}{}

		index, ok := fieldIndexes[fieldName]
		if !ok {
			return nil, &EvalError{Message: fmt.Sprintf("unknown constructor field: %s", fieldName)}
		}
		constructorIndexes = append(constructorIndexes, index)
	}

	recordType := &recordTypeSpec{
		name:               typeName.name,
		fields:             fieldSpecs,
		constructorName:    constructorName,
		constructorIndexes: constructorIndexes,
	}

	environment.define(constructorName, recordConstructorProc{recordType: recordType})
	environment.define(predicateName.name, recordPredicateProc{
		name:       predicateName.name,
		recordType: recordType,
	})

	for index, field := range fieldSpecs {
		environment.define(field.accessorName, recordAccessorProc{
			name:       field.accessorName,
			recordType: recordType,
			fieldIndex: index,
		})
		if field.mutatorName != "" {
			environment.define(field.mutatorName, recordMutatorProc{
				name:       field.mutatorName,
				recordType: recordType,
				fieldIndex: index,
			})
		}
	}

	return voidExpr{}, nil
}

func parseRecordConstructor(form expr) (string, []string, error) {
	constructor, ok := form.(listExpr)
	if !ok || len(constructor.items) == 0 {
		return "", nil, &EvalError{Message: "define-record-type constructor spec must be a non-empty list"}
	}

	name, ok := constructor.items[0].(symbolExpr)
	if !ok {
		return "", nil, &EvalError{Message: "define-record-type constructor name must be a symbol"}
	}

	fields := make([]string, 0, len(constructor.items)-1)
	for _, item := range constructor.items[1:] {
		field, ok := item.(symbolExpr)
		if !ok {
			return "", nil, &EvalError{Message: "define-record-type constructor fields must be symbols"}
		}
		fields = append(fields, field.name)
	}

	return name.name, fields, nil
}

func parseRecordFields(forms []expr) ([]recordFieldSpec, map[string]int, error) {
	fields := make([]recordFieldSpec, 0, len(forms))
	fieldIndexes := make(map[string]int, len(forms))

	for index, form := range forms {
		fieldForm, ok := form.(listExpr)
		if !ok || len(fieldForm.items) < 2 || len(fieldForm.items) > 3 {
			return nil, nil, &EvalError{Message: "record field specs must have the form (field accessor) or (field accessor mutator)"}
		}

		name, ok := fieldForm.items[0].(symbolExpr)
		if !ok {
			return nil, nil, &EvalError{Message: "record field name must be a symbol"}
		}
		if _, exists := fieldIndexes[name.name]; exists {
			return nil, nil, &EvalError{Message: fmt.Sprintf("duplicate record field: %s", name.name)}
		}

		accessor, ok := fieldForm.items[1].(symbolExpr)
		if !ok {
			return nil, nil, &EvalError{Message: "record accessor name must be a symbol"}
		}

		spec := recordFieldSpec{
			name:         name.name,
			accessorName: accessor.name,
		}

		if len(fieldForm.items) == 3 {
			mutator, ok := fieldForm.items[2].(symbolExpr)
			if !ok {
				return nil, nil, &EvalError{Message: "record mutator name must be a symbol"}
			}
			spec.mutatorName = mutator.name
		}

		fields = append(fields, spec)
		fieldIndexes[spec.name] = index
	}

	return fields, fieldIndexes, nil
}

func applyRecordConstructor(proc recordConstructorProc, args []expr) (expr, error) {
	expected := len(proc.recordType.constructorIndexes)
	if len(args) != expected {
		return nil, &EvalError{Message: fmt.Sprintf("expected %d arguments, got %d", expected, len(args))}
	}

	fields := make([]expr, len(proc.recordType.fields))
	for i := range fields {
		fields[i] = voidExpr{}
	}
	for i, fieldIndex := range proc.recordType.constructorIndexes {
		fields[fieldIndex] = args[i]
	}

	return &recordExpr{
		recordType: proc.recordType,
		fields:     fields,
	}, nil
}

func applyRecordPredicate(proc recordPredicateProc, args []expr) (expr, error) {
	if len(args) != 1 {
		return nil, &EvalError{Message: fmt.Sprintf("%s expects exactly 1 argument", proc.name)}
	}

	record, ok := args[0].(*recordExpr)
	return boolExpr(ok && record.recordType == proc.recordType), nil
}

func applyRecordAccessor(proc recordAccessorProc, args []expr) (expr, error) {
	if len(args) != 1 {
		return nil, &EvalError{Message: fmt.Sprintf("%s expects exactly 1 argument", proc.name)}
	}

	record, err := recordArgument(args[0], proc.recordType, proc.name)
	if err != nil {
		return nil, err
	}
	return record.fields[proc.fieldIndex], nil
}

func applyRecordMutator(proc recordMutatorProc, args []expr) (expr, error) {
	if len(args) != 2 {
		return nil, &EvalError{Message: fmt.Sprintf("%s expects exactly 2 arguments", proc.name)}
	}

	record, err := recordArgument(args[0], proc.recordType, proc.name)
	if err != nil {
		return nil, err
	}
	record.fields[proc.fieldIndex] = args[1]
	return voidExpr{}, nil
}

func recordArgument(value expr, expectedType *recordTypeSpec, procName string) (*recordExpr, error) {
	record, ok := value.(*recordExpr)
	if !ok || record.recordType != expectedType {
		return nil, &EvalError{Message: fmt.Sprintf("%s expects a %s record", procName, expectedType.name)}
	}
	return record, nil
}
