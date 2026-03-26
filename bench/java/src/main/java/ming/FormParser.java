package ming;

import java.util.ArrayList;
import java.util.List;

final class FormParser {
    private FormParser() {
    }

    static ParameterSpec parseLambdaParameterSpec(Expr paramsExpr) throws EvalError {
        if (paramsExpr instanceof ListExpr paramsList) {
            return parseParameterSpec(paramsList.elements());
        }
        if (paramsExpr instanceof SymbolExpr symbolExpr) {
            if (symbolExpr.name().equals(".")) {
                throw new EvalError("invalid parameter list");
            }
            return new ParameterSpec(List.of(), symbolExpr.name());
        }
        throw new EvalError("lambda parameters must be a list or symbol");
    }

    static ProcedureClause parseCaseLambdaClause(Expr clauseExpr) throws EvalError {
        if (!(clauseExpr instanceof ListExpr clauseList)) {
            throw new EvalError("case-lambda clause must be a list");
        }

        List<Expr> parts = clauseList.elements();
        if (parts.size() < 2) {
            throw new EvalError("case-lambda clause requires parameters and a body");
        }

        return new ProcedureClause(parseLambdaParameterSpec(parts.getFirst()),
                parseBody("case-lambda", parts.subList(1, parts.size())));
    }

    static ParameterSpec parseParameterSpec(List<Expr> params) throws EvalError {
        List<String> requiredParameters = new ArrayList<>(params.size());
        String restParameter = null;

        for (int index = 0; index < params.size(); index++) {
            Expr param = params.get(index);
            if (param instanceof SymbolExpr symbolExpr && symbolExpr.name().equals(".")) {
                if (restParameter != null || index != params.size() - 2) {
                    throw new EvalError("invalid parameter list");
                }

                Expr restExpr = params.get(index + 1);
                if (!(restExpr instanceof SymbolExpr restSymbol) || restSymbol.name().equals(".")) {
                    throw new EvalError("rest parameter must be a symbol");
                }
                restParameter = restSymbol.name();
                index++;
                continue;
            }

            if (!(param instanceof SymbolExpr symbolExpr)) {
                throw new EvalError("parameter must be a symbol");
            }
            requiredParameters.add(symbolExpr.name());
        }

        return new ParameterSpec(requiredParameters, restParameter);
    }

    static List<LetBinding> parseBindings(List<Expr> bindingExprs) throws EvalError {
        List<LetBinding> bindings = new ArrayList<>(bindingExprs.size());
        for (Expr bindingExpr : bindingExprs) {
            if (!(bindingExpr instanceof ListExpr bindingList)) {
                throw new EvalError("let binding must be a list");
            }

            List<Expr> parts = bindingList.elements();
            if (parts.size() != 2) {
                throw new EvalError("let binding must contain a name and value");
            }
            if (!(parts.getFirst() instanceof SymbolExpr symbolExpr)) {
                throw new EvalError("let binding name must be a symbol");
            }

            bindings.add(new LetBinding(symbolExpr.name(), parts.get(1)));
        }
        return bindings;
    }

    static List<DoBinding> parseDoBindings(List<Expr> bindingExprs) throws EvalError {
        List<DoBinding> bindings = new ArrayList<>(bindingExprs.size());
        for (Expr bindingExpr : bindingExprs) {
            if (!(bindingExpr instanceof ListExpr bindingList)) {
                throw new EvalError("do binding must be a list");
            }

            List<Expr> parts = bindingList.elements();
            if (parts.size() < 2 || parts.size() > 3) {
                throw new EvalError("do binding must contain a name, init, and optional step");
            }
            if (!(parts.getFirst() instanceof SymbolExpr symbolExpr)) {
                throw new EvalError("do binding name must be a symbol");
            }

            Expr stepExpr = parts.size() == 3 ? parts.get(2) : null;
            bindings.add(new DoBinding(symbolExpr.name(), parts.get(1), stepExpr));
        }
        return List.copyOf(bindings);
    }

    static RecordConstructorSpec parseRecordConstructorSpec(Expr constructorExpr)
            throws EvalError {
        if (!(constructorExpr instanceof ListExpr constructorList)) {
            throw new EvalError("record constructor spec must be a list");
        }

        List<Expr> parts = constructorList.elements();
        if (parts.isEmpty()) {
            throw new EvalError("record constructor spec cannot be empty");
        }
        if (!(parts.getFirst() instanceof SymbolExpr nameExpr)) {
            throw new EvalError("record constructor name must be a symbol");
        }

        List<String> fieldNames = new ArrayList<>(parts.size() - 1);
        for (int index = 1; index < parts.size(); index++) {
            fieldNames.add(expectExprSymbol(parts.get(index),
                    "record constructor field must be a symbol"));
        }
        return new RecordConstructorSpec(nameExpr.name(), fieldNames);
    }

    static List<RecordFieldSpec> parseRecordFieldSpecs(List<Expr> fieldExprs) throws EvalError {
        List<RecordFieldSpec> fields = new ArrayList<>(fieldExprs.size());
        for (Expr fieldExpr : fieldExprs) {
            if (!(fieldExpr instanceof ListExpr fieldList)) {
                throw new EvalError("record field spec must be a list");
            }

            List<Expr> parts = fieldList.elements();
            if (parts.size() < 2 || parts.size() > 3) {
                throw new EvalError("record field spec must contain a field name, accessor,"
                        + " and optional mutator");
            }

            String mutatorName = null;
            if (parts.size() == 3) {
                mutatorName = expectExprSymbol(parts.get(2),
                        "record mutator name must be a symbol");
            }
            fields.add(new RecordFieldSpec(
                    expectExprSymbol(parts.get(0), "record field name must be a symbol"),
                    expectExprSymbol(parts.get(1), "record accessor name must be a symbol"),
                    mutatorName
            ));
        }
        return List.copyOf(fields);
    }

    static List<String> recordFieldNames(List<RecordFieldSpec> fields) {
        List<String> fieldNames = new ArrayList<>(fields.size());
        for (RecordFieldSpec field : fields) {
            fieldNames.add(field.fieldName());
        }
        return fieldNames;
    }

    static List<Integer> resolveRecordFieldIndexes(List<String> fieldNames, RecordType recordType)
            throws EvalError {
        List<Integer> indexes = new ArrayList<>(fieldNames.size());
        boolean[] usedIndexes = new boolean[recordType.fieldCount()];

        for (String fieldName : fieldNames) {
            int index = recordType.fieldIndex(fieldName);
            if (index < 0) {
                throw new EvalError("unknown record field: " + fieldName);
            }
            if (usedIndexes[index]) {
                throw new EvalError("duplicate record field: " + fieldName);
            }

            usedIndexes[index] = true;
            indexes.add(index);
        }

        return List.copyOf(indexes);
    }

    static List<Expr> parseBody(String formName, List<Expr> body) throws EvalError {
        if (body.isEmpty()) {
            throw new EvalError(formName + " requires a body");
        }
        return List.copyOf(body);
    }

    private static String expectExprSymbol(Expr expr, String errorMessage) throws EvalError {
        if (expr instanceof SymbolExpr symbolExpr) {
            return symbolExpr.name();
        }
        throw new EvalError(errorMessage);
    }

    record DoBinding(String name, Expr initExpr, Expr stepExpr) {
    }
}
