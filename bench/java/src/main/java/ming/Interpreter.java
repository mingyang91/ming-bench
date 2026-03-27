package ming;

import java.util.ArrayList;
import java.util.List;

final class Interpreter {
    private static final BooleanValue TRUE = new BooleanValue(true);
    private static final BooleanValue FALSE = new BooleanValue(false);
    private static final EmptyListValue EMPTY_LIST = EmptyListValue.INSTANCE;
    private static final VoidValue VOID = VoidValue.INSTANCE;

    private final Environment globalEnv;
    private final StringBuilder output;
    private final ProcedureRuntime procedureRuntime;
    private final SpecialFormEvaluator specialFormEvaluator;
    private final boolean immutableStringLiterals;

    Interpreter() {
        this.output = new StringBuilder();
        this.procedureRuntime = new ProcedureRuntime(this::evalSequence, this::buildList);
        this.immutableStringLiterals = shouldUseImmutableStrings();

        ValueEquality valueEquality = new ValueEquality();
        this.specialFormEvaluator = new SpecialFormEvaluator(new SpecialFormRuntime(
                this::eval,
                this::parseParameters,
                valueEquality::eqv,
                procedureRuntime,
                this::createLiteralStringValue
        ));
        this.globalEnv = new Builtins(this::createFreshStringValue, output, valueEquality)
                .createGlobalEnvironment();
    }

    private static boolean shouldUseImmutableStrings() {
        int benchLevel = readBenchLevel();
        return benchLevel == 0 || benchLevel >= 15;
    }

    private static int readBenchLevel() {
        String level = System.getProperty("bench.level", "");
        if (level.isEmpty()) {
            level = System.getenv("BENCH_LEVEL");
        }
        if (level == null || level.isEmpty()) {
            return 0;
        }
        try {
            return Integer.parseInt(level);
        } catch (NumberFormatException ignored) {
            return 0;
        }
    }

    private StringValue createLiteralStringValue(String value) {
        return new StringValue(value, !immutableStringLiterals);
    }

    private StringValue createFreshStringValue(String value) {
        return new StringValue(value, true);
    }

    EvalResult evalProgram(String input) throws EvalError {
        Parser parser = new Parser(input);
        List<Expr> expressions = parser.parseProgram();
        if (expressions.isEmpty()) {
            throw new EvalError("input is empty");
        }

        Value lastValue = FALSE;
        for (Expr expression : expressions) {
            lastValue = eval(expression, globalEnv);
        }
        return new EvalResult(lastValue.render(), output.toString());
    }

    private Value eval(Expr expression, Environment env) throws EvalError {
        return executeTask(new ExpressionTask(expression, env));
    }

    private Value executeTask(EvaluationTask task) throws EvalError {
        EvaluationTask currentTask = task;
        while (true) {
            try {
                return switch (currentTask) {
                    case ExpressionTask expressionTask ->
                            evalInternal(expressionTask.expression(), expressionTask.env(), true);
                    case SequenceTask sequenceTask ->
                            evalSequenceInternal(sequenceTask.expressions(), sequenceTask.env());
                };
            } catch (TailCall tailCall) {
                currentTask = tailCall.task();
            }
        }
    }

    private Value evalInternal(Expr expression, Environment env, boolean tailPosition)
            throws EvalError {
        return switch (expression) {
            case NumberExpr numberExpr -> new NumberValue(numberExpr.value());
            case BooleanExpr booleanExpr -> booleanExpr.value() ? TRUE : FALSE;
            case StringExpr stringExpr -> createLiteralStringValue(stringExpr.value());
            case CharExpr charExpr -> new CharValue(charExpr.codePoint());
            case SymbolExpr symbolExpr -> env.lookup(symbolExpr.name(), symbolExpr.loc());
            case ListExpr listExpr -> evalList(listExpr, env, tailPosition);
        };
    }

    private Value evalList(ListExpr listExpr, Environment env, boolean tailPosition)
            throws EvalError {
        if (listExpr.elements().isEmpty()) {
            throw error(listExpr.loc(), "cannot evaluate empty list");
        }

        Expr head = listExpr.elements().getFirst();
        if (head instanceof SymbolExpr symbolExpr) {
            String symbolName = symbolExpr.name();
            if ("define".equals(symbolName)) {
                return evalDefine(listExpr, env);
            }
            if ("define-syntax".equals(symbolName)) {
                return evalDefineSyntax(listExpr, env);
            }
            if ("define-record-type".equals(symbolName)) {
                return evalDefineRecordType(listExpr, env);
            }

            var specialFormValue = specialFormEvaluator.tryEval(
                    symbolName,
                    listExpr,
                    env,
                    tailPosition
            );
            if (specialFormValue.isPresent()) {
                return specialFormValue.get();
            }

            SyntaxRulesMacro definition = env.lookupMacro(symbolName);
            if (definition != null) {
                MacroExpansion expansion = SyntaxRulesSupport.expandMacroCall(
                        definition,
                        listExpr.elements().subList(1, listExpr.elements().size()),
                        env,
                        listExpr.loc()
                );
                if (tailPosition) {
                    throw new TailCall(new ExpressionTask(
                            expansion.expression(),
                            expansion.environment()
                    ));
                }
                return eval(expansion.expression(), expansion.environment());
            }
        }

        Value procedureValue = eval(head, env);
        if (!(procedureValue instanceof Procedure procedure)) {
            throw error(head.loc(), "attempted to call a non-procedure");
        }

        List<Value> arguments = new ArrayList<>();
        for (int index = 1; index < listExpr.elements().size(); index++) {
            arguments.add(eval(listExpr.elements().get(index), env));
        }
        if (tailPosition && procedureValue instanceof TailCallable tailCallable) {
            throw new TailCall(tailCallable.prepareTailCall(arguments, listExpr.loc()));
        }
        return procedure.apply(arguments, listExpr.loc());
    }

    private Value evalDefine(ListExpr listExpr, Environment env) throws EvalError {
        ensureAtLeastExpressions("define", listExpr, 3);

        Expr target = listExpr.elements().get(1);
        if (target instanceof SymbolExpr symbolExpr) {
            if (listExpr.elements().size() != 3) {
                throw error(listExpr.loc(),
                        "define expected 2 arguments but got " + (listExpr.elements().size() - 1));
            }
            Value value = eval(listExpr.elements().get(2), env);
            env.define(symbolExpr.name(), value);
            return VOID;
        }

        if (target instanceof ListExpr signature) {
            if (signature.elements().isEmpty()) {
                throw error(target.loc(), "define requires a function name");
            }
            Expr nameExpr = signature.elements().getFirst();
            if (!(nameExpr instanceof SymbolExpr nameSymbol)) {
                throw error(nameExpr.loc(), "define requires a function name");
            }

            ParameterSpec parameters = parseParameters(
                    signature.elements().subList(1, signature.elements().size()),
                    "define"
            );
            List<Expr> body = List.copyOf(listExpr.elements().subList(2, listExpr.elements().size()));
            UserProcedure procedure = new UserProcedure(
                    nameSymbol.name(),
                    parameters.requiredParameters(),
                    parameters.restParameter(),
                    body,
                    env,
                    procedureRuntime
            );
            env.define(nameSymbol.name(), procedure);
            return VOID;
        }

        throw error(target.loc(), "define requires a symbol or parameter list");
    }

    private Value evalDefineSyntax(ListExpr listExpr, Environment env) throws EvalError {
        ensureExactlyExpressions("define-syntax", listExpr, 3);

        Expr nameExpr = listExpr.elements().get(1);
        if (!(nameExpr instanceof SymbolExpr nameSymbol)) {
            throw error(nameExpr.loc(), "define-syntax name must be a symbol");
        }

        SyntaxRulesMacro definition = SyntaxRulesSupport.parseSyntaxRules(
                nameSymbol.name(),
                listExpr.elements().get(2),
                env
        );
        env.defineMacro(nameSymbol.name(), definition);
        return VOID;
    }

    private Value evalDefineRecordType(ListExpr listExpr, Environment env) throws EvalError {
        ensureAtLeastExpressions("define-record-type", listExpr, 4);

        String typeName = requireSymbolExpr(
                listExpr.elements().get(1),
                "define-record-type type name must be a symbol"
        );

        Expr constructorExpr = listExpr.elements().get(2);
        if (!(constructorExpr instanceof ListExpr constructorList)
                || constructorList.elements().isEmpty()) {
            throw error(constructorExpr.loc(),
                    "define-record-type constructor spec must be a non-empty list");
        }

        String constructorName = requireSymbolExpr(
                constructorList.elements().getFirst(),
                "define-record-type constructor name must be a symbol"
        );
        List<String> constructorFields = new ArrayList<>(Math.max(
                constructorList.elements().size() - 1,
                0
        ));
        for (int index = 1; index < constructorList.elements().size(); index++) {
            constructorFields.add(requireSymbolExpr(
                    constructorList.elements().get(index),
                    "define-record-type constructor fields must be symbols"
            ));
        }

        String predicateName = requireSymbolExpr(
                listExpr.elements().get(3),
                "define-record-type predicate name must be a symbol"
        );

        int fieldCount = Math.max(listExpr.elements().size() - 4, 0);
        List<String> fieldNames = new ArrayList<>(fieldCount);
        List<String> accessorNames = new ArrayList<>(fieldCount);
        for (int index = 4; index < listExpr.elements().size(); index++) {
            Expr fieldExpr = listExpr.elements().get(index);
            if (!(fieldExpr instanceof ListExpr fieldList)) {
                throw error(fieldExpr.loc(), "define-record-type field specs must be lists");
            }
            if (fieldList.elements().size() != 2) {
                throw error(fieldExpr.loc(),
                        "define-record-type field specs must contain a field name and accessor");
            }

            fieldNames.add(requireSymbolExpr(
                    fieldList.elements().get(0),
                    "define-record-type field names must be symbols"
            ));
            accessorNames.add(requireSymbolExpr(
                    fieldList.elements().get(1),
                    "define-record-type accessor names must be symbols"
            ));
        }

        if (constructorFields.size() != fieldNames.size()) {
            throw error(constructorExpr.loc(),
                    "define-record-type constructor field count must match record fields");
        }
        if (!constructorFields.equals(fieldNames)) {
            throw error(constructorExpr.loc(),
                    "define-record-type constructor fields must match record fields");
        }

        RecordType recordType = new RecordType(typeName, fieldNames);
        env.define(constructorName, new RecordConstructorProcedure(constructorName, recordType));
        env.define(predicateName, new RecordPredicateProcedure(predicateName, recordType));
        for (int index = 0; index < accessorNames.size(); index++) {
            env.define(
                    accessorNames.get(index),
                    new RecordAccessorProcedure(accessorNames.get(index), recordType, index)
            );
        }

        return VOID;
    }

    private Value evalSequence(List<Expr> expressions, Environment env) throws EvalError {
        return executeTask(new SequenceTask(expressions, env));
    }

    private Value evalSequenceInternal(List<Expr> expressions, Environment env) throws EvalError {
        if (expressions.isEmpty()) {
            return VOID;
        }

        for (int index = 0; index < expressions.size() - 1; index++) {
            eval(expressions.get(index), env);
        }
        return evalInternal(expressions.getLast(), env, true);
    }

    private ParameterSpec parseParameters(List<Expr> parameterExprs, String formName)
            throws EvalError {
        List<String> parameters = new ArrayList<>(parameterExprs.size());
        String restParameter = null;

        for (int index = 0; index < parameterExprs.size(); index++) {
            Expr parameterExpr = parameterExprs.get(index);
            if (!(parameterExpr instanceof SymbolExpr symbolExpr)) {
                throw error(parameterExpr.loc(), formName + " parameters must be symbols");
            }

            if (".".equals(symbolExpr.name())) {
                if (restParameter != null || index != parameterExprs.size() - 2) {
                    throw error(symbolExpr.loc(), formName + " has invalid dotted parameter list");
                }

                Expr restExpr = parameterExprs.get(index + 1);
                if (!(restExpr instanceof SymbolExpr restSymbol)
                        || ".".equals(restSymbol.name())) {
                    throw error(restExpr.loc(), formName + " has invalid dotted parameter list");
                }
                restParameter = restSymbol.name();
                break;
            }

            parameters.add(symbolExpr.name());
        }
        return new ParameterSpec(List.copyOf(parameters), restParameter);
    }

    private String requireSymbolExpr(Expr expression, String message) throws EvalError {
        if (expression instanceof SymbolExpr symbolExpr) {
            return symbolExpr.name();
        }
        throw error(expression.loc(), message);
    }

    private Value buildList(List<Value> elements) {
        Value result = EMPTY_LIST;
        for (int index = elements.size() - 1; index >= 0; index--) {
            result = new PairValue(elements.get(index), result);
        }
        return result;
    }

    private void ensureExactlyExpressions(String formName, ListExpr listExpr, int expectedSize)
            throws EvalError {
        if (listExpr.elements().size() != expectedSize) {
            throw error(listExpr.loc(),
                    formName + " expected " + (expectedSize - 1) + " arguments but got "
                            + (listExpr.elements().size() - 1));
        }
    }

    private void ensureAtLeastExpressions(String formName, ListExpr listExpr, int minimumSize)
            throws EvalError {
        if (listExpr.elements().size() < minimumSize) {
            throw error(listExpr.loc(),
                    formName + " expected at least " + (minimumSize - 1) + " arguments but got "
                            + (listExpr.elements().size() - 1));
        }
    }

    private static EvalError error(SourceLoc loc, String message) {
        return SchemeErrors.at(loc, message);
    }
}
