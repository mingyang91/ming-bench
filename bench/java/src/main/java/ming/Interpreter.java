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
    private final ValueEquality valueEquality;
    private final boolean immutableStringLiterals;

    @FunctionalInterface
    private interface CollectedValuesHandler {
        MachineState accept(Interpreter interpreter, List<Value> values, Continuation cont)
                throws EvalError;
    }

    private record BindingSpec(String name, Expr valueExpr) {
    }

    private record DoBindingSpec(String name, Expr initExpr, Expr stepExpr) {
    }

    Interpreter() {
        this.output = new StringBuilder();
        this.procedureRuntime = new ProcedureRuntime(this::evalSequenceDirect, this::buildList);
        this.valueEquality = new ValueEquality();
        this.immutableStringLiterals = shouldUseImmutableStrings();
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

        Value lastValue = run(new SequenceState(
                List.copyOf(expressions),
                globalEnv,
                DoneContinuation.INSTANCE
        ));
        return new EvalResult(lastValue.render(), output.toString());
    }

    MachineState deliver(Value value, Continuation cont) {
        return new ReturnState(value, cont);
    }

    MachineState applyProcedure(
            Procedure procedure,
            List<Value> arguments,
            SourceLoc callLoc,
            Continuation cont
    ) {
        return new ApplyState(procedure, List.copyOf(arguments), callLoc, cont);
    }

    private Value evalSequenceDirect(List<Expr> expressions, Environment env) throws EvalError {
        return run(new SequenceState(List.copyOf(expressions), env, DoneContinuation.INSTANCE));
    }

    private Value run(MachineState initialState) throws EvalError {
        MachineState state = initialState;
        while (true) {
            switch (state) {
                case EvalState evalState ->
                        state = evalExpression(evalState.expression(), evalState.env(),
                                evalState.cont());
                case SequenceState sequenceState ->
                        state = evalSequence(sequenceState.expressions(), sequenceState.env(),
                                sequenceState.cont());
                case ReturnState returnState -> {
                    if (returnState.cont() instanceof DoneContinuation) {
                        return returnState.value();
                    }
                    PendingContinuation pending = (PendingContinuation) returnState.cont();
                    state = pending.frame().resume(this, returnState.value(), pending.next());
                }
                case ApplyState applyState ->
                        state = invokeProcedure(
                                applyState.procedure(),
                                applyState.arguments(),
                                applyState.callLoc(),
                                applyState.cont()
                        );
            }
        }
    }

    private MachineState invokeProcedure(
            Procedure procedure,
            List<Value> arguments,
            SourceLoc callLoc,
            Continuation cont
    ) throws EvalError {
        if (procedure instanceof UserProcedure userProcedure) {
            return userProcedure.invoke(arguments, callLoc, cont);
        }
        if (procedure instanceof CaseLambdaProcedure caseLambdaProcedure) {
            return caseLambdaProcedure.invoke(arguments, callLoc, cont);
        }
        if (procedure instanceof ContinuationProcedure continuationProcedure) {
            return continuationProcedure.invoke(arguments, callLoc);
        }
        if (procedure instanceof BuiltinProcedure builtinProcedure) {
            return builtinProcedure.invoke(this, arguments, callLoc, cont);
        }
        return deliver(procedure.apply(arguments, callLoc), cont);
    }

    private MachineState evalExpression(
            Expr expression,
            Environment env,
            Continuation cont
    ) throws EvalError {
        return switch (expression) {
            case NumberExpr numberExpr -> deliver(new NumberValue(numberExpr.value()), cont);
            case BooleanExpr booleanExpr -> deliver(booleanExpr.value() ? TRUE : FALSE, cont);
            case StringExpr stringExpr -> deliver(createLiteralStringValue(stringExpr.value()), cont);
            case CharExpr charExpr -> deliver(new CharValue(charExpr.codePoint()), cont);
            case SymbolExpr symbolExpr -> deliver(env.lookup(symbolExpr.name(), symbolExpr.loc()), cont);
            case ListExpr listExpr -> evalList(listExpr, env, cont);
        };
    }

    private MachineState evalSequence(
            List<Expr> expressions,
            Environment env,
            Continuation cont
    ) throws EvalError {
        if (expressions.isEmpty()) {
            return deliver(VOID, cont);
        }
        if (expressions.size() == 1) {
            return new EvalState(expressions.getFirst(), env, cont);
        }

        List<Expr> remaining = copyTail(expressions, 1);
        return new EvalState(
                expressions.getFirst(),
                env,
                withFrame(cont, (interpreter, ignored, next) ->
                        interpreter.evalSequence(remaining, env, next))
        );
    }

    private MachineState evalList(
            ListExpr listExpr,
            Environment env,
            Continuation cont
    ) throws EvalError {
        if (listExpr.elements().isEmpty()) {
            throw error(listExpr.loc(), "cannot evaluate empty list");
        }

        Expr head = listExpr.elements().getFirst();
        if (head instanceof SymbolExpr symbolExpr) {
            String symbolName = symbolExpr.name();
            switch (symbolName) {
                case "define":
                    return evalDefine(listExpr, env, cont);
                case "define-syntax":
                    return evalDefineSyntax(listExpr, env, cont);
                case "define-record-type":
                    return evalDefineRecordType(listExpr, env, cont);
                case "if":
                    return evalIf(listExpr, env, cont);
                case "quote":
                    return evalQuote(listExpr, cont);
                case "lambda":
                    return evalLambda(listExpr, env, cont);
                case "case-lambda":
                    return evalCaseLambda(listExpr, env, cont);
                case "set!":
                    return evalSet(listExpr, env, cont);
                case "begin":
                    return evalSequence(copyTail(listExpr.elements(), 1), env, cont);
                case "let":
                    return evalLet(listExpr, env, cont);
                case "let*":
                    return evalLetStar(listExpr, env, cont);
                case "letrec":
                    return evalLetrec(listExpr, env, false, cont);
                case "letrec*":
                    return evalLetrec(listExpr, env, true, cont);
                case "cond":
                    return evalCond(listExpr, env, cont);
                case "case":
                    return evalCase(listExpr, env, cont);
                case "and":
                    return evalAnd(copyTail(listExpr.elements(), 1), env, cont);
                case "or":
                    return evalOr(copyTail(listExpr.elements(), 1), env, cont);
                case "do":
                    return evalDo(listExpr, env, cont);
                default:
                    break;
            }

            SyntaxRulesMacro definition = env.lookupMacro(symbolName);
            if (definition != null) {
                MacroExpansion expansion = SyntaxRulesSupport.expandMacroCall(
                        definition,
                        listExpr.elements().subList(1, listExpr.elements().size()),
                        env,
                        listExpr.loc()
                );
                return new EvalState(expansion.expression(), expansion.environment(), cont);
            }
        }

        return evalApplication(listExpr, env, cont);
    }

    private MachineState evalDefine(
            ListExpr listExpr,
            Environment env,
            Continuation cont
    ) throws EvalError {
        ensureAtLeastExpressions("define", listExpr, 3);

        Expr target = listExpr.elements().get(1);
        if (target instanceof SymbolExpr symbolExpr) {
            if (listExpr.elements().size() != 3) {
                throw error(
                        listExpr.loc(),
                        "define expected 2 arguments but got " + (listExpr.elements().size() - 1)
                );
            }

            return new EvalState(
                    listExpr.elements().get(2),
                    env,
                    withFrame(cont, (interpreter, value, next) -> {
                        env.define(symbolExpr.name(), value);
                        return interpreter.deliver(VOID, next);
                    })
            );
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
            List<Expr> body = copyTail(listExpr.elements(), 2);
            UserProcedure procedure = new UserProcedure(
                    nameSymbol.name(),
                    parameters.requiredParameters(),
                    parameters.restParameter(),
                    body,
                    env,
                    procedureRuntime
            );
            env.define(nameSymbol.name(), procedure);
            return deliver(VOID, cont);
        }

        throw error(target.loc(), "define requires a symbol or parameter list");
    }

    private MachineState evalDefineSyntax(
            ListExpr listExpr,
            Environment env,
            Continuation cont
    ) throws EvalError {
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
        return deliver(VOID, cont);
    }

    private MachineState evalDefineRecordType(
            ListExpr listExpr,
            Environment env,
            Continuation cont
    ) throws EvalError {
        ensureAtLeastExpressions("define-record-type", listExpr, 4);

        String typeName = requireSymbolExpr(
                listExpr.elements().get(1),
                "define-record-type type name must be a symbol"
        );

        Expr constructorExpr = listExpr.elements().get(2);
        if (!(constructorExpr instanceof ListExpr constructorList)
                || constructorList.elements().isEmpty()) {
            throw error(
                    constructorExpr.loc(),
                    "define-record-type constructor spec must be a non-empty list"
            );
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
                throw error(
                        fieldExpr.loc(),
                        "define-record-type field specs must contain a field name and accessor"
                );
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
            throw error(
                    constructorExpr.loc(),
                    "define-record-type constructor field count must match record fields"
            );
        }
        if (!constructorFields.equals(fieldNames)) {
            throw error(
                    constructorExpr.loc(),
                    "define-record-type constructor fields must match record fields"
            );
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

        return deliver(VOID, cont);
    }

    private MachineState evalIf(
            ListExpr listExpr,
            Environment env,
            Continuation cont
    ) throws EvalError {
        if (listExpr.elements().size() != 3 && listExpr.elements().size() != 4) {
            throw error(
                    listExpr.loc(),
                    "if expected 2 or 3 arguments but got " + (listExpr.elements().size() - 1)
            );
        }

        Expr consequentExpr = listExpr.elements().get(2);
        Expr alternateExpr = listExpr.elements().size() == 4 ? listExpr.elements().get(3) : null;
        return new EvalState(
                listExpr.elements().get(1),
                env,
                withFrame(cont, (interpreter, value, next) -> {
                    if (value.isTruthy()) {
                        return new EvalState(consequentExpr, env, next);
                    }
                    if (alternateExpr != null) {
                        return new EvalState(alternateExpr, env, next);
                    }
                    return interpreter.deliver(VOID, next);
                })
        );
    }

    private MachineState evalQuote(ListExpr listExpr, Continuation cont) throws EvalError {
        ensureExactlyExpressions("quote", listExpr, 2);
        return deliver(quoteToValue(listExpr.elements().get(1)), cont);
    }

    private MachineState evalLambda(
            ListExpr listExpr,
            Environment env,
            Continuation cont
    ) throws EvalError {
        ensureAtLeastExpressions("lambda", listExpr, 3);

        Expr parametersExpr = listExpr.elements().get(1);
        if (!(parametersExpr instanceof ListExpr parametersList)) {
            throw error(parametersExpr.loc(), "lambda requires a parameter list");
        }

        ParameterSpec parameters = parseParameters(parametersList.elements(), "lambda");
        List<Expr> body = copyTail(listExpr.elements(), 2);
        return deliver(new UserProcedure(
                "lambda",
                parameters.requiredParameters(),
                parameters.restParameter(),
                body,
                env,
                procedureRuntime
        ), cont);
    }

    private MachineState evalCaseLambda(
            ListExpr listExpr,
            Environment env,
            Continuation cont
    ) throws EvalError {
        ensureAtLeastExpressions("case-lambda", listExpr, 2);

        List<CaseLambdaClause> clauses = new ArrayList<>(listExpr.elements().size() - 1);
        for (int index = 1; index < listExpr.elements().size(); index++) {
            Expr clauseExpr = listExpr.elements().get(index);
            if (!(clauseExpr instanceof ListExpr clauseList)) {
                throw error(clauseExpr.loc(), "case-lambda clauses must be lists");
            }
            if (clauseList.elements().isEmpty()) {
                throw error(clauseExpr.loc(), "case-lambda clauses cannot be empty");
            }

            Expr parametersExpr = clauseList.elements().getFirst();
            if (!(parametersExpr instanceof ListExpr parametersList)) {
                throw error(parametersExpr.loc(), "case-lambda clauses require a parameter list");
            }

            ParameterSpec parameters = parseParameters(parametersList.elements(), "case-lambda");
            List<Expr> body = copyTail(clauseList.elements(), 1);
            if (body.isEmpty()) {
                throw error(clauseExpr.loc(), "case-lambda clauses must have a body");
            }

            clauses.add(new CaseLambdaClause(
                    parameters.requiredParameters(),
                    parameters.restParameter(),
                    body
            ));
        }

        return deliver(new CaseLambdaProcedure(clauses, env, procedureRuntime), cont);
    }

    private MachineState evalSet(
            ListExpr listExpr,
            Environment env,
            Continuation cont
    ) throws EvalError {
        ensureExactlyExpressions("set!", listExpr, 3);

        Expr targetExpr = listExpr.elements().get(1);
        if (!(targetExpr instanceof SymbolExpr symbolExpr)) {
            throw error(targetExpr.loc(), "set! requires a symbol");
        }

        return new EvalState(
                listExpr.elements().get(2),
                env,
                withFrame(cont, (interpreter, value, next) -> {
                    env.set(symbolExpr.name(), value, symbolExpr.loc());
                    return interpreter.deliver(VOID, next);
                })
        );
    }

    private MachineState evalLet(
            ListExpr listExpr,
            Environment env,
            Continuation cont
    ) throws EvalError {
        ensureAtLeastExpressions("let", listExpr, 3);

        Expr secondExpr = listExpr.elements().get(1);
        if (secondExpr instanceof SymbolExpr nameSymbol) {
            return evalNamedLet(listExpr, env, nameSymbol, cont);
        }
        if (!(secondExpr instanceof ListExpr bindingList)) {
            throw error(secondExpr.loc(), "let requires a binding list");
        }

        List<BindingSpec> bindings = parseBindingSpecs(bindingList, "let");
        List<Expr> body = copyTail(listExpr.elements(), 2);
        return evalExpressions(
                bindingValueExpressions(bindings),
                env,
                (interpreter, values, next) -> {
                    Environment letEnv = new Environment(env);
                    for (int index = 0; index < bindings.size(); index++) {
                        letEnv.define(bindings.get(index).name(), values.get(index));
                    }
                    return interpreter.evalSequence(body, letEnv, next);
                },
                cont
        );
    }

    private MachineState evalNamedLet(
            ListExpr listExpr,
            Environment env,
            SymbolExpr nameSymbol,
            Continuation cont
    ) throws EvalError {
        if (listExpr.elements().size() < 4) {
            throw error(
                    listExpr.loc(),
                    "let expected at least 2 arguments but got " + (listExpr.elements().size() - 1)
            );
        }

        Expr bindingExpr = listExpr.elements().get(2);
        if (!(bindingExpr instanceof ListExpr bindingList)) {
            throw error(bindingExpr.loc(), "let requires a binding list");
        }

        List<BindingSpec> bindings = parseBindingSpecs(bindingList, "let");
        List<Expr> body = copyTail(listExpr.elements(), 3);
        return evalExpressions(
                bindingValueExpressions(bindings),
                env,
                (interpreter, values, next) -> {
                    Environment namedLetEnv = new Environment(env);
                    UserProcedure procedure = new UserProcedure(
                            nameSymbol.name(),
                            bindingNames(bindings),
                            null,
                            body,
                            namedLetEnv,
                            procedureRuntime
                    );
                    namedLetEnv.define(nameSymbol.name(), procedure);
                    return interpreter.applyProcedure(procedure, values, listExpr.loc(), next);
                },
                cont
        );
    }

    private MachineState evalLetStar(
            ListExpr listExpr,
            Environment env,
            Continuation cont
    ) throws EvalError {
        ensureAtLeastExpressions("let*", listExpr, 3);

        Expr bindingsExpr = listExpr.elements().get(1);
        if (!(bindingsExpr instanceof ListExpr bindingsList)) {
            throw error(bindingsExpr.loc(), "let* requires a binding list");
        }

        Environment letStarEnv = new Environment(env);
        List<BindingSpec> bindings = parseBindingSpecs(bindingsList, "let*");
        return evalLetStarBindings(
                bindings,
                0,
                letStarEnv,
                copyTail(listExpr.elements(), 2),
                cont
        );
    }

    private MachineState evalLetStarBindings(
            List<BindingSpec> bindings,
            int index,
            Environment letStarEnv,
            List<Expr> body,
            Continuation cont
    ) throws EvalError {
        if (index >= bindings.size()) {
            return evalSequence(body, letStarEnv, cont);
        }

        BindingSpec binding = bindings.get(index);
        return new EvalState(
                binding.valueExpr(),
                letStarEnv,
                withFrame(cont, (interpreter, value, next) -> {
                    letStarEnv.define(binding.name(), value);
                    return interpreter.evalLetStarBindings(
                            bindings,
                            index + 1,
                            letStarEnv,
                            body,
                            next
                    );
                })
        );
    }

    private MachineState evalLetrec(
            ListExpr listExpr,
            Environment env,
            boolean sequential,
            Continuation cont
    ) throws EvalError {
        String formName = sequential ? "letrec*" : "letrec";
        ensureAtLeastExpressions(formName, listExpr, 3);

        Expr bindingsExpr = listExpr.elements().get(1);
        if (!(bindingsExpr instanceof ListExpr bindingsList)) {
            throw error(bindingsExpr.loc(), formName + " requires a binding list");
        }

        List<BindingSpec> bindings = parseBindingSpecs(bindingsList, formName);
        Environment letrecEnv = new Environment(env);
        List<BindingCell> cells = new ArrayList<>(bindings.size());
        for (BindingSpec binding : bindings) {
            BindingCell cell = new BindingCell(VOID);
            letrecEnv.defineCell(binding.name(), cell);
            cells.add(cell);
        }

        List<Expr> body = copyTail(listExpr.elements(), 2);
        if (sequential) {
            return evalLetrecSequential(bindings, cells, 0, letrecEnv, body, cont);
        }

        return evalExpressions(
                bindingValueExpressions(bindings),
                letrecEnv,
                (interpreter, values, next) -> {
                    for (int index = 0; index < values.size(); index++) {
                        cells.get(index).set(values.get(index));
                    }
                    return interpreter.evalSequence(body, letrecEnv, next);
                },
                cont
        );
    }

    private MachineState evalLetrecSequential(
            List<BindingSpec> bindings,
            List<BindingCell> cells,
            int index,
            Environment letrecEnv,
            List<Expr> body,
            Continuation cont
    ) throws EvalError {
        if (index >= bindings.size()) {
            return evalSequence(body, letrecEnv, cont);
        }

        BindingSpec binding = bindings.get(index);
        return new EvalState(
                binding.valueExpr(),
                letrecEnv,
                withFrame(cont, (interpreter, value, next) -> {
                    cells.get(index).set(value);
                    return interpreter.evalLetrecSequential(
                            bindings,
                            cells,
                            index + 1,
                            letrecEnv,
                            body,
                            next
                    );
                })
        );
    }

    private MachineState evalCond(
            ListExpr listExpr,
            Environment env,
            Continuation cont
    ) throws EvalError {
        ensureAtLeastExpressions("cond", listExpr, 2);
        return evalCondClauses(copyTail(listExpr.elements(), 1), env, cont);
    }

    private MachineState evalCondClauses(
            List<Expr> clauses,
            Environment env,
            Continuation cont
    ) throws EvalError {
        if (clauses.isEmpty()) {
            return deliver(VOID, cont);
        }

        Expr clauseExpr = clauses.getFirst();
        if (!(clauseExpr instanceof ListExpr clauseList)) {
            throw error(clauseExpr.loc(), "cond clauses must be lists");
        }
        if (clauseList.elements().isEmpty()) {
            throw error(clauseExpr.loc(), "cond clauses cannot be empty");
        }

        Expr testExpr = clauseList.elements().getFirst();
        if (isElseSymbol(testExpr)) {
            if (clauses.size() != 1) {
                throw error(testExpr.loc(), "cond else clause must be last");
            }
            return evalSequence(copyTail(clauseList.elements(), 1), env, cont);
        }

        List<Expr> remainingClauses = copyTail(clauses, 1);
        return new EvalState(
                testExpr,
                env,
                withFrame(cont, (interpreter, value, next) -> {
                    if (!value.isTruthy()) {
                        return interpreter.evalCondClauses(remainingClauses, env, next);
                    }
                    if (clauseList.elements().size() == 1) {
                        return interpreter.deliver(value, next);
                    }
                    return interpreter.evalSequence(copyTail(clauseList.elements(), 1), env, next);
                })
        );
    }

    private MachineState evalCase(
            ListExpr listExpr,
            Environment env,
            Continuation cont
    ) throws EvalError {
        ensureAtLeastExpressions("case", listExpr, 2);

        List<Expr> clauses = copyTail(listExpr.elements(), 2);
        return new EvalState(
                listExpr.elements().get(1),
                env,
                withFrame(cont, (interpreter, value, next) ->
                        interpreter.evalCaseClauses(value, clauses, env, next))
        );
    }

    private MachineState evalCaseClauses(
            Value key,
            List<Expr> clauses,
            Environment env,
            Continuation cont
    ) throws EvalError {
        for (int index = 0; index < clauses.size(); index++) {
            Expr clauseExpr = clauses.get(index);
            if (!(clauseExpr instanceof ListExpr clauseList)) {
                throw error(clauseExpr.loc(), "case clauses must be lists");
            }
            if (clauseList.elements().isEmpty()) {
                throw error(clauseExpr.loc(), "case clauses cannot be empty");
            }

            Expr firstExpr = clauseList.elements().getFirst();
            if (isElseSymbol(firstExpr)) {
                if (index != clauses.size() - 1) {
                    throw error(firstExpr.loc(), "case else clause must be last");
                }
                return evalSequence(copyTail(clauseList.elements(), 1), env, cont);
            }

            if (!(firstExpr instanceof ListExpr datumList)) {
                throw error(firstExpr.loc(), "case clauses must start with a datum list");
            }

            for (Expr datumExpr : datumList.elements()) {
                if (valueEquality.eqv(key, quoteToValue(datumExpr))) {
                    return evalSequence(copyTail(clauseList.elements(), 1), env, cont);
                }
            }
        }
        return deliver(VOID, cont);
    }

    private MachineState evalAnd(
            List<Expr> expressions,
            Environment env,
            Continuation cont
    ) throws EvalError {
        if (expressions.isEmpty()) {
            return deliver(TRUE, cont);
        }
        if (expressions.size() == 1) {
            return new EvalState(expressions.getFirst(), env, cont);
        }

        List<Expr> remaining = copyTail(expressions, 1);
        return new EvalState(
                expressions.getFirst(),
                env,
                withFrame(cont, (interpreter, value, next) -> {
                    if (!value.isTruthy()) {
                        return interpreter.deliver(value, next);
                    }
                    return interpreter.evalAnd(remaining, env, next);
                })
        );
    }

    private MachineState evalOr(
            List<Expr> expressions,
            Environment env,
            Continuation cont
    ) throws EvalError {
        if (expressions.isEmpty()) {
            return deliver(FALSE, cont);
        }
        if (expressions.size() == 1) {
            return new EvalState(expressions.getFirst(), env, cont);
        }

        List<Expr> remaining = copyTail(expressions, 1);
        return new EvalState(
                expressions.getFirst(),
                env,
                withFrame(cont, (interpreter, value, next) -> {
                    if (value.isTruthy()) {
                        return interpreter.deliver(value, next);
                    }
                    return interpreter.evalOr(remaining, env, next);
                })
        );
    }

    private MachineState evalDo(
            ListExpr listExpr,
            Environment env,
            Continuation cont
    ) throws EvalError {
        ensureAtLeastExpressions("do", listExpr, 3);

        Expr bindingsExpr = listExpr.elements().get(1);
        if (!(bindingsExpr instanceof ListExpr bindingsList)) {
            throw error(bindingsExpr.loc(), "do requires a binding list");
        }

        Expr testClauseExpr = listExpr.elements().get(2);
        if (!(testClauseExpr instanceof ListExpr testClause) || testClause.elements().isEmpty()) {
            throw error(testClauseExpr.loc(), "do requires a test clause");
        }

        List<DoBindingSpec> bindings = parseDoBindings(bindingsList);
        List<Expr> initExpressions = new ArrayList<>(bindings.size());
        for (DoBindingSpec binding : bindings) {
            initExpressions.add(binding.initExpr());
        }

        List<Expr> bodyExpressions = copyTail(listExpr.elements(), 3);
        return evalExpressions(
                List.copyOf(initExpressions),
                env,
                (interpreter, values, next) -> {
                    Environment loopEnv = new Environment(env);
                    List<BindingCell> cells = new ArrayList<>(bindings.size());
                    for (int index = 0; index < bindings.size(); index++) {
                        BindingCell cell = new BindingCell(values.get(index));
                        loopEnv.defineCell(bindings.get(index).name(), cell);
                        cells.add(cell);
                    }
                    return interpreter.evalDoLoop(
                            bindings,
                            List.copyOf(cells),
                            testClause,
                            bodyExpressions,
                            loopEnv,
                            next
                    );
                },
                cont
        );
    }

    private MachineState evalDoLoop(
            List<DoBindingSpec> bindings,
            List<BindingCell> cells,
            ListExpr testClause,
            List<Expr> bodyExpressions,
            Environment loopEnv,
            Continuation cont
    ) throws EvalError {
        return new EvalState(
                testClause.elements().getFirst(),
                loopEnv,
                withFrame(cont, (interpreter, value, next) -> {
                    if (value.isTruthy()) {
                        return interpreter.evalSequence(copyTail(testClause.elements(), 1), loopEnv, next);
                    }

                    Continuation bodyCont = withFrame(next, (afterBodyInterpreter, ignored, afterBody) ->
                            afterBodyInterpreter.evalDoSteps(
                                    bindings,
                                    cells,
                                    0,
                                    List.of(),
                                    testClause,
                                    bodyExpressions,
                                    loopEnv,
                                    afterBody
                            ));
                    return interpreter.evalSequence(bodyExpressions, loopEnv, bodyCont);
                })
        );
    }

    private MachineState evalDoSteps(
            List<DoBindingSpec> bindings,
            List<BindingCell> cells,
            int index,
            List<Value> nextValues,
            ListExpr testClause,
            List<Expr> bodyExpressions,
            Environment loopEnv,
            Continuation cont
    ) throws EvalError {
        if (index >= bindings.size()) {
            for (int cellIndex = 0; cellIndex < cells.size(); cellIndex++) {
                cells.get(cellIndex).set(nextValues.get(cellIndex));
            }
            return evalDoLoop(bindings, cells, testClause, bodyExpressions, loopEnv, cont);
        }

        DoBindingSpec binding = bindings.get(index);
        if (binding.stepExpr() == null) {
            return evalDoSteps(
                    bindings,
                    cells,
                    index + 1,
                    appendValue(nextValues, cells.get(index).value()),
                    testClause,
                    bodyExpressions,
                    loopEnv,
                    cont
            );
        }

        List<Value> valueSnapshot = List.copyOf(nextValues);
        return new EvalState(
                binding.stepExpr(),
                loopEnv,
                withFrame(cont, (interpreter, value, next) ->
                        interpreter.evalDoSteps(
                                bindings,
                                cells,
                                index + 1,
                                appendValue(valueSnapshot, value),
                                testClause,
                                bodyExpressions,
                                loopEnv,
                                next
                        ))
        );
    }

    private MachineState evalApplication(
            ListExpr listExpr,
            Environment env,
            Continuation cont
    ) throws EvalError {
        Expr operatorExpr = listExpr.elements().getFirst();
        List<Expr> argumentExprs = copyTail(listExpr.elements(), 1);
        return new EvalState(
                operatorExpr,
                env,
                withFrame(cont, (interpreter, value, next) -> {
                    if (!(value instanceof Procedure procedure)) {
                        throw error(operatorExpr.loc(), "attempted to call a non-procedure");
                    }
                    return interpreter.evalApplicationArguments(
                            procedure,
                            argumentExprs,
                            List.of(),
                            env,
                            listExpr.loc(),
                            next
                    );
                })
        );
    }

    private MachineState evalApplicationArguments(
            Procedure procedure,
            List<Expr> remainingArgumentExprs,
            List<Value> collectedArguments,
            Environment env,
            SourceLoc callLoc,
            Continuation cont
    ) throws EvalError {
        if (remainingArgumentExprs.isEmpty()) {
            return applyProcedure(procedure, collectedArguments, callLoc, cont);
        }

        Expr nextArgumentExpr = remainingArgumentExprs.getLast();
        List<Expr> restArguments = copyWithoutLast(remainingArgumentExprs);
        List<Value> argumentSnapshot = List.copyOf(collectedArguments);
        return new EvalState(
                nextArgumentExpr,
                env,
                withFrame(cont, (interpreter, value, next) ->
                        interpreter.evalApplicationArguments(
                                procedure,
                                restArguments,
                                prependValue(argumentSnapshot, value),
                                env,
                                callLoc,
                                next
                        ))
        );
    }

    private MachineState evalExpressions(
            List<Expr> expressions,
            Environment env,
            CollectedValuesHandler handler,
            Continuation cont
    ) throws EvalError {
        return evalExpressions(expressions, env, List.of(), handler, cont);
    }

    private MachineState evalExpressions(
            List<Expr> expressions,
            Environment env,
            List<Value> collectedValues,
            CollectedValuesHandler handler,
            Continuation cont
    ) throws EvalError {
        if (expressions.isEmpty()) {
            return handler.accept(this, collectedValues, cont);
        }

        Expr nextExpression = expressions.getFirst();
        List<Expr> remainingExpressions = copyTail(expressions, 1);
        List<Value> collectedSnapshot = List.copyOf(collectedValues);
        return new EvalState(
                nextExpression,
                env,
                withFrame(cont, (interpreter, value, next) ->
                        interpreter.evalExpressions(
                                remainingExpressions,
                                env,
                                appendValue(collectedSnapshot, value),
                                handler,
                                next
                        ))
        );
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

    private List<BindingSpec> parseBindingSpecs(ListExpr bindingsList, String formName)
            throws EvalError {
        List<BindingSpec> bindings = new ArrayList<>(bindingsList.elements().size());
        for (Expr bindingExpr : bindingsList.elements()) {
            if (!(bindingExpr instanceof ListExpr bindingList)) {
                throw error(bindingExpr.loc(), formName + " bindings must be lists");
            }
            if (bindingList.elements().size() != 2) {
                throw error(
                        bindingExpr.loc(),
                        formName + " bindings must contain a name and value"
                );
            }

            Expr nameExpr = bindingList.elements().getFirst();
            if (!(nameExpr instanceof SymbolExpr symbolExpr)) {
                throw error(nameExpr.loc(), formName + " binding names must be symbols");
            }

            bindings.add(new BindingSpec(symbolExpr.name(), bindingList.elements().get(1)));
        }
        return List.copyOf(bindings);
    }

    private List<DoBindingSpec> parseDoBindings(ListExpr bindingsList) throws EvalError {
        List<DoBindingSpec> bindings = new ArrayList<>(bindingsList.elements().size());
        for (Expr bindingExpr : bindingsList.elements()) {
            if (!(bindingExpr instanceof ListExpr bindingList)) {
                throw error(bindingExpr.loc(), "do bindings must be lists");
            }
            if (bindingList.elements().size() != 2 && bindingList.elements().size() != 3) {
                throw error(
                        bindingExpr.loc(),
                        "do bindings must contain a name, init, and optional step"
                );
            }

            Expr nameExpr = bindingList.elements().getFirst();
            if (!(nameExpr instanceof SymbolExpr symbolExpr)) {
                throw error(nameExpr.loc(), "do binding names must be symbols");
            }

            Expr stepExpr = bindingList.elements().size() == 3
                    ? bindingList.elements().get(2)
                    : null;
            bindings.add(new DoBindingSpec(symbolExpr.name(), bindingList.elements().get(1), stepExpr));
        }
        return List.copyOf(bindings);
    }

    private Value quoteToValue(Expr expression) throws EvalError {
        return switch (expression) {
            case NumberExpr numberExpr -> new NumberValue(numberExpr.value());
            case BooleanExpr booleanExpr -> booleanExpr.value() ? TRUE : FALSE;
            case StringExpr stringExpr -> createLiteralStringValue(stringExpr.value());
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
        return buildList(values);
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

    private List<String> bindingNames(List<BindingSpec> bindings) {
        List<String> names = new ArrayList<>(bindings.size());
        for (BindingSpec binding : bindings) {
            names.add(binding.name());
        }
        return List.copyOf(names);
    }

    private List<Expr> bindingValueExpressions(List<BindingSpec> bindings) {
        List<Expr> expressions = new ArrayList<>(bindings.size());
        for (BindingSpec binding : bindings) {
            expressions.add(binding.valueExpr());
        }
        return List.copyOf(expressions);
    }

    private boolean isElseSymbol(Expr expression) {
        return expression instanceof SymbolExpr symbolExpr && "else".equals(symbolExpr.name());
    }

    private Continuation withFrame(Continuation cont, ContinuationFrame frame) {
        return new PendingContinuation(frame, cont);
    }

    private List<Value> appendValue(List<Value> values, Value value) {
        List<Value> updated = new ArrayList<>(values.size() + 1);
        updated.addAll(values);
        updated.add(value);
        return List.copyOf(updated);
    }

    private List<Value> prependValue(List<Value> values, Value value) {
        List<Value> updated = new ArrayList<>(values.size() + 1);
        updated.add(value);
        updated.addAll(values);
        return List.copyOf(updated);
    }

    private static <T> List<T> copyTail(List<T> values, int startIndex) {
        if (startIndex >= values.size()) {
            return List.of();
        }
        return List.copyOf(values.subList(startIndex, values.size()));
    }

    private static <T> List<T> copyWithoutLast(List<T> values) {
        if (values.isEmpty()) {
            return List.of();
        }
        return List.copyOf(values.subList(0, values.size() - 1));
    }

    private void ensureExactlyExpressions(String formName, ListExpr listExpr, int expectedSize)
            throws EvalError {
        if (listExpr.elements().size() != expectedSize) {
            throw error(
                    listExpr.loc(),
                    formName + " expected " + (expectedSize - 1) + " arguments but got "
                            + (listExpr.elements().size() - 1)
            );
        }
    }

    private void ensureAtLeastExpressions(String formName, ListExpr listExpr, int minimumSize)
            throws EvalError {
        if (listExpr.elements().size() < minimumSize) {
            throw error(
                    listExpr.loc(),
                    formName + " expected at least " + (minimumSize - 1) + " arguments but got "
                            + (listExpr.elements().size() - 1)
            );
        }
    }

    private static EvalError error(SourceLoc loc, String message) {
        return SchemeErrors.at(loc, message);
    }
}
