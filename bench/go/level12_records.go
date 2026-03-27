package ming

type recordType struct {
	name   string
	fields []recordField
}

type recordField struct {
	name         string
	accessorName string
}

type recordValue struct {
	recordType *recordType
	fields     []value
}

type recordConstructorProc struct {
	name       string
	recordType *recordType
	fieldOrder []int
}

type recordPredicateProc struct {
	name       string
	recordType *recordType
}

type recordAccessorProc struct {
	name       string
	recordType *recordType
	fieldIndex int
}

func (r *recordValue) schemeString() string {
	return "#<record " + r.recordType.name + ">"
}

func (*recordValue) isTruthy() bool {
	return true
}

func (p recordConstructorProc) schemeString() string {
	return "#<procedure:" + p.name + ">"
}

func (recordConstructorProc) isTruthy() bool {
	return true
}

func (p recordConstructorProc) call(args []value) (value, error) {
	if len(args) != len(p.fieldOrder) {
		return nil, newCurrentEvalError("'%s' expects %d arguments, got %d", p.name, len(p.fieldOrder), len(args))
	}

	fields := make([]value, len(p.recordType.fields))
	for i, fieldIndex := range p.fieldOrder {
		fields[fieldIndex] = args[i]
	}

	return &recordValue{
		recordType: p.recordType,
		fields:     fields,
	}, nil
}

func (p recordPredicateProc) schemeString() string {
	return "#<procedure:" + p.name + ">"
}

func (recordPredicateProc) isTruthy() bool {
	return true
}

func (p recordPredicateProc) call(args []value) (value, error) {
	if len(args) != 1 {
		return nil, newCurrentEvalError("'%s' expects exactly 1 argument", p.name)
	}

	record, ok := args[0].(*recordValue)
	return boolValue(ok && record.recordType == p.recordType), nil
}

func (p recordAccessorProc) schemeString() string {
	return "#<procedure:" + p.name + ">"
}

func (recordAccessorProc) isTruthy() bool {
	return true
}

func (p recordAccessorProc) call(args []value) (value, error) {
	if len(args) != 1 {
		return nil, newCurrentEvalError("'%s' expects exactly 1 argument", p.name)
	}

	record, ok := args[0].(*recordValue)
	if !ok || record.recordType != p.recordType {
		return nil, newCurrentEvalError("'%s' expects a %s record", p.name, p.recordType.name)
	}

	return record.fields[p.fieldIndex], nil
}

func evalDefineRecordType(parts []locatedExpr, env *env) (value, error) {
	if len(parts) < 3 {
		return nil, newCurrentEvalError("'define-record-type' expects a type name, constructor, and predicate")
	}

	typeName, err := parseRecordSymbol(parts[0], "'define-record-type' type name must be a symbol")
	if err != nil {
		return nil, err
	}

	constructorSpec, err := parseRecordConstructorSpec(parts[1])
	if err != nil {
		return nil, err
	}

	predicateName, err := parseRecordSymbol(parts[2], "'define-record-type' predicate name must be a symbol")
	if err != nil {
		return nil, err
	}

	fields, fieldIndexes, err := parseRecordFields(parts[3:])
	if err != nil {
		return nil, err
	}

	constructorOrder := make([]int, len(constructorSpec.fieldNames))
	seenConstructorFields := make(map[string]struct{}, len(constructorSpec.fieldNames))
	for i, fieldName := range constructorSpec.fieldNames {
		if _, exists := seenConstructorFields[fieldName]; exists {
			return nil, newEvalError(constructorSpec.fieldExprs[i].pos, "duplicate constructor field: %s", fieldName)
		}
		seenConstructorFields[fieldName] = struct{}{}

		fieldIndex, ok := fieldIndexes[fieldName]
		if !ok {
			return nil, newEvalError(constructorSpec.fieldExprs[i].pos, "unknown record field: %s", fieldName)
		}
		constructorOrder[i] = fieldIndex
	}

	if len(constructorOrder) != len(fields) {
		return nil, newEvalError(parts[1].pos, "'define-record-type' constructor must initialize every field exactly once")
	}

	recordType := &recordType{
		name:   typeName,
		fields: fields,
	}

	env.define(constructorSpec.name, recordConstructorProc{
		name:       constructorSpec.name,
		recordType: recordType,
		fieldOrder: constructorOrder,
	})
	env.define(predicateName, recordPredicateProc{
		name:       predicateName,
		recordType: recordType,
	})
	for i, field := range fields {
		env.define(field.accessorName, recordAccessorProc{
			name:       field.accessorName,
			recordType: recordType,
			fieldIndex: i,
		})
	}

	return voidValue{}, nil
}

type recordConstructorSpec struct {
	name       string
	fieldNames []string
	fieldExprs []locatedExpr
}

func parseRecordConstructorSpec(expr locatedExpr) (recordConstructorSpec, error) {
	items, ok := expr.form.(listExpr)
	if !ok || len(items) == 0 {
		return recordConstructorSpec{}, newEvalError(expr.pos, "'define-record-type' constructor spec must be a non-empty list")
	}

	name, err := parseRecordSymbol(items[0], "'define-record-type' constructor name must be a symbol")
	if err != nil {
		return recordConstructorSpec{}, err
	}

	fieldNames := make([]string, 0, len(items)-1)
	fieldExprs := make([]locatedExpr, 0, len(items)-1)
	for _, item := range items[1:] {
		fieldName, err := parseRecordSymbol(item, "'define-record-type' constructor fields must be symbols")
		if err != nil {
			return recordConstructorSpec{}, err
		}
		fieldNames = append(fieldNames, fieldName)
		fieldExprs = append(fieldExprs, item)
	}

	return recordConstructorSpec{
		name:       name,
		fieldNames: fieldNames,
		fieldExprs: fieldExprs,
	}, nil
}

func parseRecordFields(fieldExprs []locatedExpr) ([]recordField, map[string]int, error) {
	fields := make([]recordField, 0, len(fieldExprs))
	fieldIndexes := make(map[string]int, len(fieldExprs))
	accessorNames := make(map[string]struct{}, len(fieldExprs))

	for _, fieldExpr := range fieldExprs {
		items, ok := fieldExpr.form.(listExpr)
		if !ok || len(items) != 2 {
			return nil, nil, newEvalError(fieldExpr.pos, "'define-record-type' field specs must be (field-name accessor-name) pairs")
		}

		fieldName, err := parseRecordSymbol(items[0], "'define-record-type' field names must be symbols")
		if err != nil {
			return nil, nil, err
		}
		if _, exists := fieldIndexes[fieldName]; exists {
			return nil, nil, newEvalError(items[0].pos, "duplicate record field: %s", fieldName)
		}

		accessorName, err := parseRecordSymbol(items[1], "'define-record-type' accessor names must be symbols")
		if err != nil {
			return nil, nil, err
		}
		if _, exists := accessorNames[accessorName]; exists {
			return nil, nil, newEvalError(items[1].pos, "duplicate record accessor: %s", accessorName)
		}
		accessorNames[accessorName] = struct{}{}

		fieldIndexes[fieldName] = len(fields)
		fields = append(fields, recordField{
			name:         fieldName,
			accessorName: accessorName,
		})
	}

	return fields, fieldIndexes, nil
}

func parseRecordSymbol(expr locatedExpr, message string) (string, error) {
	name, ok := expr.form.(symbolExpr)
	if !ok {
		return "", newEvalError(expr.pos, message)
	}
	return string(name), nil
}
