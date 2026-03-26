package ming;

import static ming.EvaluatorSupport.requireExactArgs;
import static ming.EvaluatorSupport.requireMinArgs;
import static ming.RuntimeConstants.VOID;
import static ming.RuntimeConstants.boolValue;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

final class RecordTypeSupport {
    private RecordTypeSupport() {
    }

    static Value evalDefineRecordType(List<Expr> arguments, Env env) throws EvalError {
        requireMinArgs("define-record-type", arguments, 3);

        Expr typeExpr = arguments.get(0);
        if (!(typeExpr instanceof SymbolExpr typeSymbol)) {
            throw new EvalError("define-record-type type name must be a symbol");
        }

        Expr constructorExpr = arguments.get(1);
        if (!(constructorExpr instanceof ListExpr constructorList)
                || constructorList.elements().isEmpty()) {
            throw new EvalError("define-record-type constructor spec must be a list");
        }

        Expr constructorNameExpr = constructorList.elements().get(0);
        if (!(constructorNameExpr instanceof SymbolExpr constructorNameSymbol)) {
            throw new EvalError("define-record-type constructor name must be a symbol");
        }

        Expr predicateExpr = arguments.get(2);
        if (!(predicateExpr instanceof SymbolExpr predicateSymbol)) {
            throw new EvalError("define-record-type predicate name must be a symbol");
        }

        List<RecordFieldSpec> fieldSpecs = new ArrayList<>();
        Map<String, Integer> fieldIndexes = new HashMap<>();
        for (int index = 3; index < arguments.size(); index++) {
            Expr fieldExpr = arguments.get(index);
            if (!(fieldExpr instanceof ListExpr fieldList) || fieldList.elements().size() != 2) {
                throw new EvalError(
                        "define-record-type field spec must contain a field and accessor");
            }

            Expr fieldNameExpr = fieldList.elements().get(0);
            if (!(fieldNameExpr instanceof SymbolExpr fieldNameSymbol)) {
                throw new EvalError("define-record-type field name must be a symbol");
            }

            Expr accessorExpr = fieldList.elements().get(1);
            if (!(accessorExpr instanceof SymbolExpr accessorSymbol)) {
                throw new EvalError("define-record-type accessor name must be a symbol");
            }

            String fieldName = fieldNameSymbol.name();
            if (fieldIndexes.containsKey(fieldName)) {
                throw new EvalError("define-record-type field names must be unique");
            }

            fieldIndexes.put(fieldName, fieldSpecs.size());
            fieldSpecs.add(new RecordFieldSpec(fieldName, accessorSymbol.name()));
        }

        List<Expr> constructorFields = constructorList.elements().subList(1,
                constructorList.elements().size());
        if (constructorFields.size() != fieldSpecs.size()) {
            throw new EvalError(
                    "define-record-type constructor field count must match field specs");
        }

        int[] constructorOrder = new int[constructorFields.size()];
        boolean[] assignedFields = new boolean[fieldSpecs.size()];
        for (int index = 0; index < constructorFields.size(); index++) {
            Expr fieldExpr = constructorFields.get(index);
            if (!(fieldExpr instanceof SymbolExpr fieldSymbol)) {
                throw new EvalError("define-record-type constructor fields must be symbols");
            }

            Integer fieldIndex = fieldIndexes.get(fieldSymbol.name());
            if (fieldIndex == null) {
                throw new EvalError("define-record-type constructor references an unknown field");
            }
            if (assignedFields[fieldIndex]) {
                throw new EvalError("define-record-type constructor fields must be unique");
            }

            constructorOrder[index] = fieldIndex;
            assignedFields[fieldIndex] = true;
        }

        RecordTypeValue type = new RecordTypeValue(typeSymbol.name());
        env.define(constructorNameSymbol.name(),
                new BuiltinValue(constructorNameSymbol.name(),
                        values -> constructRecord(type, constructorOrder, values,
                                constructorNameSymbol.name())));
        env.define(predicateSymbol.name(),
                new BuiltinValue(predicateSymbol.name(),
                        values -> recordPredicate(type, values, predicateSymbol.name())));
        for (int index = 0; index < fieldSpecs.size(); index++) {
            RecordFieldSpec fieldSpec = fieldSpecs.get(index);
            int fieldIndex = index;
            env.define(fieldSpec.accessorName(),
                    new BuiltinValue(fieldSpec.accessorName(),
                            values -> recordAccessor(type, fieldIndex, values,
                                    fieldSpec.accessorName())));
        }
        return VOID;
    }

    private static Value constructRecord(RecordTypeValue type, int[] constructorOrder,
            List<Value> arguments, String constructorName) throws EvalError {
        requireExactArgs(constructorName, arguments, constructorOrder.length);

        Value[] fields = new Value[constructorOrder.length];
        for (int index = 0; index < constructorOrder.length; index++) {
            fields[constructorOrder[index]] = arguments.get(index);
        }
        return new RecordInstanceValue(type, List.of(fields));
    }

    private static Value recordPredicate(RecordTypeValue type, List<Value> arguments, String name)
            throws EvalError {
        requireExactArgs(name, arguments, 1);
        return boolValue(arguments.get(0) instanceof RecordInstanceValue recordInstance
                && recordInstance.type() == type);
    }

    private static Value recordAccessor(RecordTypeValue type, int fieldIndex,
            List<Value> arguments, String accessorName) throws EvalError {
        requireExactArgs(accessorName, arguments, 1);
        Value value = arguments.get(0);
        if (!(value instanceof RecordInstanceValue recordInstance)
                || recordInstance.type() != type) {
            throw new EvalError(accessorName + " expects a " + type.name() + " record");
        }
        return recordInstance.field(fieldIndex);
    }

    private record RecordFieldSpec(String name, String accessorName) {
    }
}
