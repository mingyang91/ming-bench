package ming;

import java.util.ArrayList;
import java.util.List;
import java.util.Optional;

@FunctionalInterface
interface ExpressionEvaluator {
    Value eval(Expr expression, Environment env) throws EvalError;
}

@FunctionalInterface
interface ParameterSpecParser {
    ParameterSpec parse(List<Expr> parameterExprs, String formName) throws EvalError;
}

@FunctionalInterface
interface EquivalenceChecker {
    boolean test(Value left, Value right);
}

@FunctionalInterface
interface StringFactory {
    StringValue create(String value);
}

record SpecialFormRuntime(
        ExpressionEvaluator expressionEvaluator,
        ParameterSpecParser parameterSpecParser,
        EquivalenceChecker equivalenceChecker,
        ProcedureRuntime procedureRuntime,
        StringFactory stringFactory
) {
    Value eval(Expr expression, Environment env) throws EvalError {
        return expressionEvaluator.eval(expression, env);
    }

    ParameterSpec parseParameters(List<Expr> parameterExprs, String formName) throws EvalError {
        return parameterSpecParser.parse(parameterExprs, formName);
    }

    boolean eqv(Value left, Value right) {
        return equivalenceChecker.test(left, right);
    }

    Value evalSequence(List<Expr> expressions, Environment env) throws EvalError {
        return procedureRuntime.evalSequence(expressions, env);
    }

    Value buildList(List<Value> values) {
        return procedureRuntime.buildList(values);
    }

    StringValue createString(String value) {
        return stringFactory.create(value);
    }

    EvalError error(SourceLoc loc, String message) {
        return procedureRuntime.error(loc, message);
    }
}

final class SpecialFormEvaluator {
    private static final BooleanValue TRUE = new BooleanValue(true);
    private static final BooleanValue FALSE = new BooleanValue(false);
    private static final VoidValue VOID = VoidValue.INSTANCE;

    private final SpecialFormRuntime runtime;

    SpecialFormEvaluator(SpecialFormRuntime runtime) {
        this.runtime = runtime;
    }

    Optional<Value> tryEval(String symbolName, ListExpr listExpr, Environment env) throws EvalError {
        return switch (symbolName) {
            case "if" -> Optional.of(evalIf(listExpr, env));
            case "quote" -> Optional.of(evalQuote(listExpr));
            case "lambda" -> Optional.of(evalLambda(listExpr, env));
            case "case-lambda" -> Optional.of(evalCaseLambda(listExpr, env));
            case "set!" -> Optional.of(evalSet(listExpr, env));
            case "begin" -> Optional.of(evalBegin(listExpr, env));
            case "let" -> Optional.of(evalLet(listExpr, env));
            case "letrec" -> Optional.of(evalLetrec(listExpr, env, false));
            case "letrec*" -> Optional.of(evalLetrec(listExpr, env, true));
            case "cond" -> Optional.of(evalCond(listExpr, env));
            case "case" -> Optional.of(evalCase(listExpr, env));
            case "and" -> Optional.of(evalAnd(listExpr.elements().subList(1, listExpr.elements().size()), env));
            case "or" -> Optional.of(evalOr(listExpr.elements().subList(1, listExpr.elements().size()), env));
            case "do" -> Optional.of(evalDo(listExpr, env));
            default -> Optional.empty();
        };
    }

    private Value evalIf(ListExpr listExpr, Environment env) throws EvalError {
        if (listExpr.elements().size() != 3 && listExpr.elements().size() != 4) {
            throw runtime.error(
                    listExpr.loc(),
                    "if expected 2 or 3 arguments but got " + (listExpr.elements().size() - 1)
            );
        }

        Value condition = runtime.eval(listExpr.elements().get(1), env);
        if (condition.isTruthy()) {
            return runtime.eval(listExpr.elements().get(2), env);
        }
        if (listExpr.elements().size() == 4) {
            return runtime.eval(listExpr.elements().get(3), env);
        }
        return VOID;
    }

    private Value evalQuote(ListExpr listExpr) throws EvalError {
        ensureExactlyExpressions("quote", listExpr, 2);
        return quoteToValue(listExpr.elements().get(1));
    }

    private Value evalLambda(ListExpr listExpr, Environment env) throws EvalError {
        ensureAtLeastExpressions("lambda", listExpr, 3);
        Expr parametersExpr = listExpr.elements().get(1);
        if (!(parametersExpr instanceof ListExpr parametersList)) {
            throw runtime.error(parametersExpr.loc(), "lambda requires a parameter list");
        }

        ParameterSpec parameters = runtime.parseParameters(parametersList.elements(), "lambda");
        List<Expr> body = List.copyOf(listExpr.elements().subList(2, listExpr.elements().size()));
        return new UserProcedure(
                "lambda",
                parameters.requiredParameters(),
                parameters.restParameter(),
                body,
                env,
                runtime.procedureRuntime()
        );
    }

    private Value evalCaseLambda(ListExpr listExpr, Environment env) throws EvalError {
        ensureAtLeastExpressions("case-lambda", listExpr, 2);

        List<CaseLambdaClause> clauses = new ArrayList<>(listExpr.elements().size() - 1);
        for (int index = 1; index < listExpr.elements().size(); index++) {
            Expr clauseExpr = listExpr.elements().get(index);
            if (!(clauseExpr instanceof ListExpr clauseList)) {
                throw runtime.error(clauseExpr.loc(), "case-lambda clauses must be lists");
            }
            if (clauseList.elements().isEmpty()) {
                throw runtime.error(clauseExpr.loc(), "case-lambda clauses cannot be empty");
            }

            Expr parametersExpr = clauseList.elements().getFirst();
            if (!(parametersExpr instanceof ListExpr parametersList)) {
                throw runtime.error(
                        parametersExpr.loc(),
                        "case-lambda clauses require a parameter list"
                );
            }

            ParameterSpec parameters = runtime.parseParameters(
                    parametersList.elements(),
                    "case-lambda"
            );
            List<Expr> body = List.copyOf(clauseList.elements().subList(1, clauseList.elements().size()));
            if (body.isEmpty()) {
                throw runtime.error(clauseExpr.loc(), "case-lambda clauses must have a body");
            }

            clauses.add(new CaseLambdaClause(
                    parameters.requiredParameters(),
                    parameters.restParameter(),
                    body
            ));
        }

        return new CaseLambdaProcedure(clauses, env, runtime.procedureRuntime());
    }

    private Value evalSet(ListExpr listExpr, Environment env) throws EvalError {
        ensureExactlyExpressions("set!", listExpr, 3);

        Expr targetExpr = listExpr.elements().get(1);
        if (!(targetExpr instanceof SymbolExpr symbolExpr)) {
            throw runtime.error(targetExpr.loc(), "set! requires a symbol");
        }

        Value value = runtime.eval(listExpr.elements().get(2), env);
        env.set(symbolExpr.name(), value, symbolExpr.loc());
        return VOID;
    }

    private Value evalBegin(ListExpr listExpr, Environment env) throws EvalError {
        return runtime.evalSequence(listExpr.elements().subList(1, listExpr.elements().size()), env);
    }

    private Value evalLet(ListExpr listExpr, Environment env) throws EvalError {
        ensureAtLeastExpressions("let", listExpr, 3);

        Expr secondExpr = listExpr.elements().get(1);
        if (secondExpr instanceof SymbolExpr nameSymbol) {
            return evalNamedLet(listExpr, env, nameSymbol);
        }
        if (!(secondExpr instanceof ListExpr bindingList)) {
            throw runtime.error(secondExpr.loc(), "let requires a binding list");
        }

        BindingParseResult bindings = parseBindings(bindingList, env);
        Environment letEnv = new Environment(env);
        for (int index = 0; index < bindings.names().size(); index++) {
            letEnv.define(bindings.names().get(index), bindings.values().get(index));
        }
        return runtime.evalSequence(listExpr.elements().subList(2, listExpr.elements().size()), letEnv);
    }

    private Value evalNamedLet(ListExpr listExpr, Environment env, SymbolExpr nameSymbol)
            throws EvalError {
        if (listExpr.elements().size() < 4) {
            throw runtime.error(
                    listExpr.loc(),
                    "let expected at least 2 arguments but got " + (listExpr.elements().size() - 1)
            );
        }

        Expr bindingExpr = listExpr.elements().get(2);
        if (!(bindingExpr instanceof ListExpr bindingList)) {
            throw runtime.error(bindingExpr.loc(), "let requires a binding list");
        }

        BindingParseResult bindings = parseBindings(bindingList, env);
        List<Expr> body = List.copyOf(listExpr.elements().subList(3, listExpr.elements().size()));

        Environment namedLetEnv = new Environment(env);
        UserProcedure procedure = new UserProcedure(
                nameSymbol.name(),
                bindings.names(),
                null,
                body,
                namedLetEnv,
                runtime.procedureRuntime()
        );
        namedLetEnv.define(nameSymbol.name(), procedure);
        return procedure.apply(bindings.values(), listExpr.loc());
    }

    private Value evalLetrec(ListExpr listExpr, Environment env, boolean sequential)
            throws EvalError {
        String formName = sequential ? "letrec*" : "letrec";
        ensureAtLeastExpressions(formName, listExpr, 3);

        Expr bindingsExpr = listExpr.elements().get(1);
        if (!(bindingsExpr instanceof ListExpr bindingsList)) {
            throw runtime.error(bindingsExpr.loc(), formName + " requires a binding list");
        }

        List<BindingSpec> bindings = parseBindingSpecs(bindingsList, formName);
        Environment letrecEnv = new Environment(env);
        List<BindingCell> cells = new ArrayList<>(bindings.size());
        for (BindingSpec binding : bindings) {
            BindingCell cell = new BindingCell(VOID);
            letrecEnv.defineCell(binding.name(), cell);
            cells.add(cell);
        }

        if (sequential) {
            for (int index = 0; index < bindings.size(); index++) {
                cells.get(index).set(runtime.eval(bindings.get(index).valueExpr(), letrecEnv));
            }
        } else {
            List<Value> values = new ArrayList<>(bindings.size());
            for (BindingSpec binding : bindings) {
                values.add(runtime.eval(binding.valueExpr(), letrecEnv));
            }
            for (int index = 0; index < values.size(); index++) {
                cells.get(index).set(values.get(index));
            }
        }

        return runtime.evalSequence(listExpr.elements().subList(2, listExpr.elements().size()), letrecEnv);
    }

    private Value evalCond(ListExpr listExpr, Environment env) throws EvalError {
        ensureAtLeastExpressions("cond", listExpr, 2);

        List<Expr> clauses = listExpr.elements().subList(1, listExpr.elements().size());
        for (int clauseIndex = 0; clauseIndex < clauses.size(); clauseIndex++) {
            Expr clauseExpr = clauses.get(clauseIndex);
            if (!(clauseExpr instanceof ListExpr clauseList)) {
                throw runtime.error(clauseExpr.loc(), "cond clauses must be lists");
            }
            if (clauseList.elements().isEmpty()) {
                throw runtime.error(clauseExpr.loc(), "cond clauses cannot be empty");
            }

            Expr testExpr = clauseList.elements().getFirst();
            boolean isElseClause = testExpr instanceof SymbolExpr symbolExpr
                    && "else".equals(symbolExpr.name());
            if (isElseClause) {
                if (clauseIndex != clauses.size() - 1) {
                    throw runtime.error(testExpr.loc(), "cond else clause must be last");
                }
                return runtime.evalSequence(
                        clauseList.elements().subList(1, clauseList.elements().size()),
                        env
                );
            }

            Value testValue = runtime.eval(testExpr, env);
            if (!testValue.isTruthy()) {
                continue;
            }
            if (clauseList.elements().size() == 1) {
                return testValue;
            }
            return runtime.evalSequence(
                    clauseList.elements().subList(1, clauseList.elements().size()),
                    env
            );
        }

        return VOID;
    }

    private Value evalCase(ListExpr listExpr, Environment env) throws EvalError {
        ensureAtLeastExpressions("case", listExpr, 2);

        Value key = runtime.eval(listExpr.elements().get(1), env);
        List<Expr> clauses = listExpr.elements().subList(2, listExpr.elements().size());
        for (int clauseIndex = 0; clauseIndex < clauses.size(); clauseIndex++) {
            Expr clauseExpr = clauses.get(clauseIndex);
            if (!(clauseExpr instanceof ListExpr clauseList)) {
                throw runtime.error(clauseExpr.loc(), "case clauses must be lists");
            }
            if (clauseList.elements().isEmpty()) {
                throw runtime.error(clauseExpr.loc(), "case clauses cannot be empty");
            }

            Expr firstExpr = clauseList.elements().getFirst();
            boolean isElseClause = firstExpr instanceof SymbolExpr symbolExpr
                    && "else".equals(symbolExpr.name());
            if (isElseClause) {
                if (clauseIndex != clauses.size() - 1) {
                    throw runtime.error(firstExpr.loc(), "case else clause must be last");
                }
                return runtime.evalSequence(
                        clauseList.elements().subList(1, clauseList.elements().size()),
                        env
                );
            }

            if (!(firstExpr instanceof ListExpr datumList)) {
                throw runtime.error(firstExpr.loc(), "case clauses must start with a datum list");
            }

            for (Expr datumExpr : datumList.elements()) {
                if (runtime.eqv(key, quoteToValue(datumExpr))) {
                    return runtime.evalSequence(
                            clauseList.elements().subList(1, clauseList.elements().size()),
                            env
                    );
                }
            }
        }

        return VOID;
    }

    private Value evalAnd(List<Expr> expressions, Environment env) throws EvalError {
        Value lastValue = TRUE;
        for (Expr expression : expressions) {
            Value value = runtime.eval(expression, env);
            if (!value.isTruthy()) {
                return value;
            }
            lastValue = value;
        }
        return lastValue;
    }

    private Value evalOr(List<Expr> expressions, Environment env) throws EvalError {
        for (Expr expression : expressions) {
            Value value = runtime.eval(expression, env);
            if (value.isTruthy()) {
                return value;
            }
        }
        return FALSE;
    }

    private Value evalDo(ListExpr listExpr, Environment env) throws EvalError {
        ensureAtLeastExpressions("do", listExpr, 3);

        Expr bindingsExpr = listExpr.elements().get(1);
        if (!(bindingsExpr instanceof ListExpr bindingsList)) {
            throw runtime.error(bindingsExpr.loc(), "do requires a binding list");
        }

        Expr testClauseExpr = listExpr.elements().get(2);
        if (!(testClauseExpr instanceof ListExpr testClause) || testClause.elements().isEmpty()) {
            throw runtime.error(testClauseExpr.loc(), "do requires a test clause");
        }

        List<DoBindingSpec> bindings = parseDoBindings(bindingsList);
        List<Value> initValues = new ArrayList<>(bindings.size());
        for (DoBindingSpec binding : bindings) {
            initValues.add(runtime.eval(binding.initExpr(), env));
        }

        Environment loopEnv = new Environment(env);
        List<BindingCell> cells = new ArrayList<>(bindings.size());
        for (int index = 0; index < bindings.size(); index++) {
            BindingCell cell = new BindingCell(initValues.get(index));
            loopEnv.defineCell(bindings.get(index).name(), cell);
            cells.add(cell);
        }

        while (true) {
            Value testValue = runtime.eval(testClause.elements().getFirst(), loopEnv);
            if (testValue.isTruthy()) {
                return runtime.evalSequence(
                        testClause.elements().subList(1, testClause.elements().size()),
                        loopEnv
                );
            }

            runtime.evalSequence(listExpr.elements().subList(3, listExpr.elements().size()), loopEnv);

            List<Value> nextValues = new ArrayList<>(bindings.size());
            for (int index = 0; index < bindings.size(); index++) {
                Expr stepExpr = bindings.get(index).stepExpr();
                if (stepExpr == null) {
                    nextValues.add(cells.get(index).value());
                } else {
                    nextValues.add(runtime.eval(stepExpr, loopEnv));
                }
            }
            for (int index = 0; index < bindings.size(); index++) {
                cells.get(index).set(nextValues.get(index));
            }
        }
    }

    private Value quoteToValue(Expr expression) throws EvalError {
        return switch (expression) {
            case NumberExpr numberExpr -> new NumberValue(numberExpr.value());
            case BooleanExpr booleanExpr -> booleanExpr.value() ? TRUE : FALSE;
            case StringExpr stringExpr -> runtime.createString(stringExpr.value());
            case CharExpr charExpr -> new CharValue(charExpr.codePoint());
            case SymbolExpr symbolExpr -> new SymbolValue(symbolExpr.name());
            case ListExpr listExpr -> quoteList(listExpr.elements());
        };
    }

    private Value quoteList(List<Expr> expressions) throws EvalError {
        List<Value> values = new ArrayList<>(expressions.size());
        for (Expr expression : expressions) {
            values.add(quoteToValue(expression));
        }
        return runtime.buildList(values);
    }

    private BindingParseResult parseBindings(ListExpr bindingsList, Environment env) throws EvalError {
        List<String> names = new ArrayList<>(bindingsList.elements().size());
        List<Value> values = new ArrayList<>(bindingsList.elements().size());
        for (Expr bindingExpr : bindingsList.elements()) {
            if (!(bindingExpr instanceof ListExpr bindingList)) {
                throw runtime.error(bindingExpr.loc(), "let bindings must be lists");
            }
            if (bindingList.elements().size() != 2) {
                throw runtime.error(bindingExpr.loc(), "let bindings must contain a name and value");
            }

            Expr nameExpr = bindingList.elements().getFirst();
            if (!(nameExpr instanceof SymbolExpr symbolExpr)) {
                throw runtime.error(nameExpr.loc(), "let binding names must be symbols");
            }

            names.add(symbolExpr.name());
            values.add(runtime.eval(bindingList.elements().get(1), env));
        }
        return new BindingParseResult(List.copyOf(names), List.copyOf(values));
    }

    private List<BindingSpec> parseBindingSpecs(ListExpr bindingsList, String formName)
            throws EvalError {
        List<BindingSpec> bindings = new ArrayList<>(bindingsList.elements().size());
        for (Expr bindingExpr : bindingsList.elements()) {
            if (!(bindingExpr instanceof ListExpr bindingList)) {
                throw runtime.error(bindingExpr.loc(), formName + " bindings must be lists");
            }
            if (bindingList.elements().size() != 2) {
                throw runtime.error(
                        bindingExpr.loc(),
                        formName + " bindings must contain a name and value"
                );
            }

            Expr nameExpr = bindingList.elements().getFirst();
            if (!(nameExpr instanceof SymbolExpr symbolExpr)) {
                throw runtime.error(nameExpr.loc(), formName + " binding names must be symbols");
            }

            bindings.add(new BindingSpec(symbolExpr.name(), bindingList.elements().get(1)));
        }
        return List.copyOf(bindings);
    }

    private List<DoBindingSpec> parseDoBindings(ListExpr bindingsList) throws EvalError {
        List<DoBindingSpec> bindings = new ArrayList<>(bindingsList.elements().size());
        for (Expr bindingExpr : bindingsList.elements()) {
            if (!(bindingExpr instanceof ListExpr bindingList)) {
                throw runtime.error(bindingExpr.loc(), "do bindings must be lists");
            }
            if (bindingList.elements().size() != 2 && bindingList.elements().size() != 3) {
                throw runtime.error(
                        bindingExpr.loc(),
                        "do bindings must contain a name, init, and optional step"
                );
            }

            Expr nameExpr = bindingList.elements().getFirst();
            if (!(nameExpr instanceof SymbolExpr symbolExpr)) {
                throw runtime.error(nameExpr.loc(), "do binding names must be symbols");
            }

            Expr stepExpr = bindingList.elements().size() == 3
                    ? bindingList.elements().get(2)
                    : null;
            bindings.add(new DoBindingSpec(symbolExpr.name(), bindingList.elements().get(1), stepExpr));
        }
        return List.copyOf(bindings);
    }

    private static void ensureExactlyExpressions(String formName, ListExpr listExpr, int expectedSize)
            throws EvalError {
        if (listExpr.elements().size() != expectedSize) {
            throw SchemeErrors.at(
                    listExpr.loc(),
                    formName + " expected " + (expectedSize - 1) + " arguments but got "
                            + (listExpr.elements().size() - 1)
            );
        }
    }

    private static void ensureAtLeastExpressions(String formName, ListExpr listExpr, int minimumSize)
            throws EvalError {
        if (listExpr.elements().size() < minimumSize) {
            throw SchemeErrors.at(
                    listExpr.loc(),
                    formName + " expected at least " + (minimumSize - 1) + " arguments but got "
                            + (listExpr.elements().size() - 1)
            );
        }
    }

    private record BindingSpec(String name, Expr valueExpr) {
    }

    private record DoBindingSpec(String name, Expr initExpr, Expr stepExpr) {
    }
}
