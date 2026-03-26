package ming;

import java.math.BigInteger;
import java.util.ArrayList;
import java.util.List;

/**
 * Scheme interpreter entry point.
 * Agents implement this class.
 */
public class Evaluator {
    private sealed interface EvalStep permits ValueStep, TailStep {
    }

    private record ValueStep(Value value) implements EvalStep {
    }

    private record TailStep(Expr expr, Environment env) implements EvalStep {
    }

    private final Environment globalEnv;
    private StringBuilder activeOutput;
    private long syntheticCounter;
    private final RecordProcedureSupport recordProcedureSupport = new RecordProcedureSupport() {
        @Override
        public void requireArity(String name, int actual, int expected) throws EvalError {
            Evaluator.this.requireArity(name, actual, expected);
        }

        @Override
        public RecordValue expectRecord(Value value, RecordType recordType) throws EvalError {
            return Evaluator.this.expectRecord(value, recordType);
        }
    };

    public Evaluator() {
        globalEnv = GlobalEnvironmentFactory.create(this);
    }

    /**
     * Evaluate one or more Scheme expressions and return the string
     * representation of the last result.
     */
    public String evalStr(String input) throws EvalError {
        return evalProgram(input, null).render();
    }

    private Value evalProgram(String input, StringBuilder output) throws EvalError {
        StringBuilder previousOutput = activeOutput;
        activeOutput = output;

        Parser parser = new Parser(input);
        Value lastValue = null;

        try {
            while (parser.hasMore()) {
                lastValue = eval(parser.parseExpr(), globalEnv);
            }

            if (lastValue == null) {
                throw new EvalError("empty input", 1, 1);
            }

            return lastValue;
        } finally {
            activeOutput = previousOutput;
        }
    }

    /**
     * Evaluate Scheme expressions and return both the result string
     * and any captured output from display/write/newline.
     */
    public EvalResult evalStrWithOutput(String input) throws EvalError {
        StringBuilder output = new StringBuilder();
        Value result = evalProgram(input, output);
        return new EvalResult(result.render(), output.toString());
    }

    ProcedureValue builtin(String name, BuiltinAction action) {
        return new BuiltinProcedure(name, action);
    }

    void appendOutput(String text) {
        if (activeOutput != null) {
            activeOutput.append(text);
        }
    }

    String renderForDisplay(Value value) {
        if (value instanceof StringValue stringValue) {
            return stringValue.value();
        }
        if (value instanceof CharValue charValue) {
            return Character.toString(charValue.value());
        }
        return value.render();
    }

    private Value eval(Expr expr, Environment env) throws EvalError {
        Expr currentExpr = expr;
        Environment currentEnv = env;

        while (true) {
            try {
                switch (currentExpr) {
                    case IntExpr intExpr -> {
                        return new IntValue(intExpr.value());
                    }
                    case RationalExpr rationalExpr -> {
                        return NumericSupport.exactToValue(
                                new ExactFraction(rationalExpr.numerator(),
                                        rationalExpr.denominator()));
                    }
                    case InexactExpr inexactExpr -> {
                        return new InexactValue(inexactExpr.value());
                    }
                    case BoolExpr boolExpr -> {
                        return BoolValue.of(boolExpr.value());
                    }
                    case StringExpr stringExpr -> {
                        return new StringValue(stringExpr.value());
                    }
                    case CharExpr charExpr -> {
                        return new CharValue(charExpr.value());
                    }
                    case SymbolExpr symbolExpr -> {
                        return currentEnv.lookup(symbolExpr.name());
                    }
                    case ListExpr listExpr -> {
                        EvalStep step = evalList(listExpr, currentEnv);
                        if (step instanceof ValueStep valueStep) {
                            return valueStep.value();
                        }

                        TailStep tailStep = (TailStep) step;
                        currentExpr = tailStep.expr();
                        currentEnv = tailStep.env();
                    }
                }
            } catch (EvalError error) {
                throw error.withPosition(currentExpr.position().line(),
                        currentExpr.position().column());
            }
        }
    }

    private EvalStep evalList(ListExpr listExpr, Environment env) throws EvalError {
        List<Expr> elements = listExpr.elements();
        if (elements.isEmpty()) {
            throw new EvalError("cannot evaluate empty list");
        }

        Expr head = elements.getFirst();
        List<Expr> argExprs = elements.subList(1, elements.size());

        if (head instanceof SymbolExpr symbolExpr) {
            return switch (symbolExpr.name()) {
                case "define" -> valueStep(evalDefine(argExprs, env));
                case "define-syntax" -> valueStep(evalDefineSyntax(argExprs, env));
                case "define-record-type" -> valueStep(evalDefineRecordType(argExprs, env));
                case "set!" -> valueStep(evalSet(argExprs, env));
                case "if" -> evalIf(argExprs, env);
                case "quote" -> valueStep(evalQuote(argExprs));
                case "lambda" -> valueStep(evalLambda(argExprs, env));
                case "case-lambda" -> valueStep(evalCaseLambda(argExprs, env));
                case "begin" -> evalBegin(argExprs, env);
                case "let" -> evalLet(argExprs, env);
                case "letrec" -> evalLetrec(argExprs, env, false);
                case "letrec*" -> evalLetrec(argExprs, env, true);
                case "cond" -> evalCond(argExprs, env);
                case "case" -> evalCase(argExprs, env);
                case "and" -> evalAnd(argExprs, env);
                case "or" -> evalOr(argExprs, env);
                case "do" -> evalDo(argExprs, env);
                default -> {
                    MacroBinding macro = env.lookupSyntax(symbolExpr.name());
                    if (macro != null) {
                        yield tailStep(macro.expand(listExpr), env);
                    }
                    yield applyProcedureStep(eval(head, env), evalArgs(argExprs, env));
                }
            };
        }

        return applyProcedureStep(eval(head, env), evalArgs(argExprs, env));
    }

    private Value evalDefine(List<Expr> argExprs, Environment env) throws EvalError {
        if (argExprs.size() < 2) {
            throw new EvalError("define requires a name and a value");
        }

        Expr target = argExprs.getFirst();
        if (target instanceof SymbolExpr symbolExpr) {
            requireArity("define", argExprs.size(), 2);
            Value value = eval(argExprs.get(1), env);
            env.define(symbolExpr.name(), value);
            return VoidValue.INSTANCE;
        }

        if (target instanceof ListExpr signatureExpr) {
            List<Expr> signature = signatureExpr.elements();
            if (signature.isEmpty()) {
                throw new EvalError("define requires a function name");
            }
            if (!(signature.getFirst() instanceof SymbolExpr nameExpr)) {
                throw new EvalError("function name must be a symbol");
            }

            ParameterSpec parameters = FormParser.parseParameterSpec(
                    signature.subList(1, signature.size()));
            List<Expr> body = FormParser.parseBody("define",
                    argExprs.subList(1, argExprs.size()));
            ProcedureValue procedure = new UserProcedure(this, nameExpr.name(), parameters, body,
                    env);
            env.define(nameExpr.name(), procedure);
            return VoidValue.INSTANCE;
        }

        throw new EvalError("invalid define");
    }

    private Value evalSet(List<Expr> argExprs, Environment env) throws EvalError {
        requireArity("set!", argExprs.size(), 2);
        if (!(argExprs.getFirst() instanceof SymbolExpr symbolExpr)) {
            throw new EvalError("set! target must be a symbol");
        }

        Value value = eval(argExprs.get(1), env);
        env.set(symbolExpr.name(), value);
        return VoidValue.INSTANCE;
    }

    private EvalStep evalIf(List<Expr> argExprs, Environment env) throws EvalError {
        if (argExprs.size() < 2 || argExprs.size() > 3) {
            throw new EvalError(
                    "wrong number of arguments for if: expected 2 or 3, got " + argExprs.size());
        }
        Value condition = eval(argExprs.get(0), env);
        if (isTruthy(condition)) {
            return tailStep(argExprs.get(1), env);
        }
        if (argExprs.size() == 2) {
            return valueStep(VoidValue.INSTANCE);
        }
        return tailStep(argExprs.get(2), env);
    }

    private Value evalQuote(List<Expr> argExprs) throws EvalError {
        requireArity("quote", argExprs.size(), 1);
        return quoteToValue(argExprs.getFirst());
    }

    private Value evalDefineSyntax(List<Expr> argExprs, Environment env) throws EvalError {
        requireArity("define-syntax", argExprs.size(), 2);

        if (!(argExprs.getFirst() instanceof SymbolExpr nameExpr)) {
            throw new EvalError("define-syntax name must be a symbol");
        }

        env.defineSyntax(nameExpr.name(),
                SyntaxRulesMacro.compile(nameExpr.name(), argExprs.get(1), env,
                        this::freshSyntheticName));
        return VoidValue.INSTANCE;
    }

    private Value evalDefineRecordType(List<Expr> argExprs, Environment env) throws EvalError {
        if (argExprs.size() < 3) {
            throw new EvalError(
                    "define-record-type requires a type name, constructor, and predicate");
        }
        if (!(argExprs.get(0) instanceof SymbolExpr typeExpr)) {
            throw new EvalError("record type name must be a symbol");
        }
        if (!(argExprs.get(2) instanceof SymbolExpr predicateExpr)) {
            throw new EvalError("record predicate name must be a symbol");
        }

        RecordConstructorSpec constructor = FormParser.parseRecordConstructorSpec(argExprs.get(1));
        List<RecordFieldSpec> fields = FormParser.parseRecordFieldSpecs(
                argExprs.subList(3, argExprs.size()));

        RecordType recordType;
        try {
            recordType = new RecordType(typeExpr.name(), FormParser.recordFieldNames(fields));
        } catch (IllegalArgumentException error) {
            throw new EvalError(error.getMessage());
        }

        List<Integer> constructorFieldIndexes = FormParser.resolveRecordFieldIndexes(
                constructor.fieldNames(), recordType);
        env.define(constructor.name(), new RecordConstructorProcedure(constructor.name(),
                recordType, constructorFieldIndexes, recordProcedureSupport));
        env.define(predicateExpr.name(), new RecordPredicateProcedure(predicateExpr.name(),
                recordType, recordProcedureSupport));

        for (int index = 0; index < fields.size(); index++) {
            RecordFieldSpec field = fields.get(index);
            env.define(field.accessorName(), new RecordAccessorProcedure(field.accessorName(),
                    recordType, index, recordProcedureSupport));
            if (field.mutatorName() != null) {
                env.define(field.mutatorName(), new RecordMutatorProcedure(field.mutatorName(),
                        recordType, index, recordProcedureSupport));
            }
        }

        return VoidValue.INSTANCE;
    }

    private Value evalLambda(List<Expr> argExprs, Environment env) throws EvalError {
        if (argExprs.size() < 2) {
            throw new EvalError("lambda requires parameters and a body");
        }

        ParameterSpec parameters = FormParser.parseLambdaParameterSpec(argExprs.getFirst());
        List<Expr> body = FormParser.parseBody("lambda", argExprs.subList(1, argExprs.size()));
        return new UserProcedure(this, null, parameters, body, env);
    }

    private Value evalCaseLambda(List<Expr> argExprs, Environment env) throws EvalError {
        if (argExprs.isEmpty()) {
            throw new EvalError("case-lambda requires at least one clause");
        }

        List<ProcedureClause> clauses = new ArrayList<>(argExprs.size());
        for (Expr clauseExpr : argExprs) {
            clauses.add(FormParser.parseCaseLambdaClause(clauseExpr));
        }
        return new CaseLambdaProcedure(this, clauses, env);
    }

    private EvalStep evalBegin(List<Expr> argExprs, Environment env) throws EvalError {
        return evalSequenceStep(argExprs, env);
    }

    private EvalStep evalLet(List<Expr> argExprs, Environment env) throws EvalError {
        if (argExprs.isEmpty()) {
            throw new EvalError("let requires bindings and a body");
        }

        Expr firstArg = argExprs.getFirst();
        if (firstArg instanceof SymbolExpr nameExpr) {
            if (argExprs.size() < 2) {
                throw new EvalError("let requires bindings and a body");
            }
            if (!(argExprs.get(1) instanceof ListExpr bindingsExpr)) {
                throw new EvalError("let bindings must be a list");
            }

            List<LetBinding> bindings = FormParser.parseBindings(bindingsExpr.elements());
            List<Expr> body = FormParser.parseBody("let", argExprs.subList(2, argExprs.size()));
            return evalNamedLet(nameExpr.name(), bindings, body, env);
        }

        if (!(firstArg instanceof ListExpr bindingsExpr)) {
            throw new EvalError("let bindings must be a list");
        }

        List<LetBinding> bindings = FormParser.parseBindings(bindingsExpr.elements());
        List<Expr> body = FormParser.parseBody("let", argExprs.subList(1, argExprs.size()));
        return evalSimpleLet(bindings, body, env);
    }

    private EvalStep evalLetrec(List<Expr> argExprs, Environment env, boolean sequential)
            throws EvalError {
        String formName = sequential ? "letrec*" : "letrec";
        if (argExprs.isEmpty()) {
            throw new EvalError(formName + " requires bindings and a body");
        }
        if (!(argExprs.getFirst() instanceof ListExpr bindingsExpr)) {
            throw new EvalError(formName + " bindings must be a list");
        }

        List<LetBinding> bindings = FormParser.parseBindings(bindingsExpr.elements());
        List<Expr> body = FormParser.parseBody(formName, argExprs.subList(1, argExprs.size()));

        Environment letrecEnv = new Environment(env);
        List<Cell> bindingCells = new ArrayList<>(bindings.size());
        for (LetBinding binding : bindings) {
            Cell cell = new Cell(UninitializedValue.INSTANCE);
            letrecEnv.defineAlias(binding.name(), cell);
            bindingCells.add(cell);
        }

        if (sequential) {
            for (int index = 0; index < bindings.size(); index++) {
                bindingCells.get(index).set(eval(bindings.get(index).valueExpr(), letrecEnv));
            }
        } else {
            List<Value> values = new ArrayList<>(bindings.size());
            for (LetBinding binding : bindings) {
                values.add(eval(binding.valueExpr(), letrecEnv));
            }
            for (int index = 0; index < values.size(); index++) {
                bindingCells.get(index).set(values.get(index));
            }
        }

        return evalSequenceStep(body, letrecEnv);
    }

    private EvalStep evalSimpleLet(List<LetBinding> bindings, List<Expr> body, Environment env)
            throws EvalError {
        Environment letEnv = new Environment(env);
        for (LetBinding binding : bindings) {
            letEnv.define(binding.name(), eval(binding.valueExpr(), env));
        }
        return evalSequenceStep(body, letEnv);
    }

    private EvalStep evalNamedLet(String name, List<LetBinding> bindings, List<Expr> body,
                                  Environment env) throws EvalError {
        List<String> parameterNames = new ArrayList<>(bindings.size());
        List<Value> arguments = new ArrayList<>(bindings.size());
        for (LetBinding binding : bindings) {
            parameterNames.add(binding.name());
            arguments.add(eval(binding.valueExpr(), env));
        }

        Environment letEnv = new Environment(env);
        ProcedureValue procedure = new UserProcedure(this, name,
                new ParameterSpec(parameterNames, null), body, letEnv);
        letEnv.define(name, procedure);
        return applyProcedureStep(procedure, arguments);
    }

    private EvalStep evalCase(List<Expr> argExprs, Environment env) throws EvalError {
        if (argExprs.isEmpty()) {
            throw new EvalError("case requires a key and at least zero clauses");
        }

        Value key = eval(argExprs.getFirst(), env);
        for (int index = 1; index < argExprs.size(); index++) {
            Expr clauseExpr = argExprs.get(index);
            if (!(clauseExpr instanceof ListExpr clauseList)) {
                throw new EvalError("case clause must be a list");
            }

            List<Expr> clause = clauseList.elements();
            if (clause.isEmpty()) {
                throw new EvalError("case clause cannot be empty");
            }

            Expr head = clause.getFirst();
            if (head instanceof SymbolExpr symbolExpr && symbolExpr.name().equals("else")) {
                if (index != argExprs.size() - 1) {
                    throw new EvalError("case else clause must be last");
                }
                return evalSequenceOrVoidStep(clause.subList(1, clause.size()), env);
            }

            if (!(head instanceof ListExpr datumList)) {
                throw new EvalError("case clause datums must be a list");
            }

            for (Expr datumExpr : datumList.elements()) {
                if (isEqv(key, quoteToValue(datumExpr))) {
                    return evalSequenceOrVoidStep(clause.subList(1, clause.size()), env);
                }
            }
        }

        return valueStep(VoidValue.INSTANCE);
    }

    private EvalStep evalDo(List<Expr> argExprs, Environment env) throws EvalError {
        if (argExprs.size() < 2) {
            throw new EvalError("do requires bindings and a termination clause");
        }
        if (!(argExprs.get(0) instanceof ListExpr bindingList)) {
            throw new EvalError("do bindings must be a list");
        }
        if (!(argExprs.get(1) instanceof ListExpr terminationClause)) {
            throw new EvalError("do termination clause must be a list");
        }

        List<FormParser.DoBinding> bindings = FormParser.parseDoBindings(bindingList.elements());
        List<Expr> terminationParts = terminationClause.elements();
        if (terminationParts.isEmpty()) {
            throw new EvalError("do termination clause requires a test");
        }

        Environment loopEnv = new Environment(env);
        List<Cell> bindingCells = new ArrayList<>(bindings.size());
        for (FormParser.DoBinding binding : bindings) {
            Cell cell = new Cell(eval(binding.initExpr(), env));
            loopEnv.defineAlias(binding.name(), cell);
            bindingCells.add(cell);
        }

        List<Expr> body = argExprs.subList(2, argExprs.size());
        while (true) {
            if (isTruthy(eval(terminationParts.getFirst(), loopEnv))) {
                return evalSequenceOrVoidStep(terminationParts.subList(1, terminationParts.size()),
                        loopEnv);
            }

            resolveStep(evalSequenceOrVoidStep(body, loopEnv));

            List<Value> nextValues = new ArrayList<>(bindings.size());
            for (int index = 0; index < bindings.size(); index++) {
                Expr stepExpr = bindings.get(index).stepExpr();
                if (stepExpr == null) {
                    nextValues.add(bindingCells.get(index).value());
                } else {
                    nextValues.add(eval(stepExpr, loopEnv));
                }
            }
            for (int index = 0; index < nextValues.size(); index++) {
                bindingCells.get(index).set(nextValues.get(index));
            }
        }
    }

    private EvalStep evalCond(List<Expr> argExprs, Environment env) throws EvalError {
        for (int index = 0; index < argExprs.size(); index++) {
            Expr clauseExpr = argExprs.get(index);
            if (!(clauseExpr instanceof ListExpr clauseList)) {
                throw new EvalError("cond clause must be a list");
            }

            List<Expr> clause = clauseList.elements();
            if (clause.isEmpty()) {
                throw new EvalError("cond clause cannot be empty");
            }

            Expr testExpr = clause.getFirst();
            if (testExpr instanceof SymbolExpr symbolExpr && symbolExpr.name().equals("else")) {
                if (index != argExprs.size() - 1) {
                    throw new EvalError("cond else clause must be last");
                }
                return evalClauseBody("cond", clause.subList(1, clause.size()), env, null);
            }

            Value testValue = eval(testExpr, env);
            if (isTruthy(testValue)) {
                return evalClauseBody("cond", clause.subList(1, clause.size()), env, testValue);
            }
        }

        return valueStep(VoidValue.INSTANCE);
    }

    private EvalStep evalClauseBody(String formName, List<Expr> body, Environment env,
                                    Value defaultValue) throws EvalError {
        if (body.isEmpty()) {
            if (defaultValue != null) {
                return valueStep(defaultValue);
            }
            throw new EvalError(formName + " clause requires a body");
        }
        return evalSequenceStep(body, env);
    }

    private List<Value> evalArgs(List<Expr> argExprs, Environment env) throws EvalError {
        List<Value> values = new ArrayList<>(argExprs.size());
        for (Expr argExpr : argExprs) {
            values.add(eval(argExpr, env));
        }
        return values;
    }

    private EvalStep evalAnd(List<Expr> argExprs, Environment env) throws EvalError {
        if (argExprs.isEmpty()) {
            return valueStep(BoolValue.TRUE);
        }

        for (int index = 0; index < argExprs.size() - 1; index++) {
            Value value = eval(argExprs.get(index), env);
            if (!isTruthy(value)) {
                return valueStep(value);
            }
        }
        return tailStep(argExprs.get(argExprs.size() - 1), env);
    }

    private EvalStep evalOr(List<Expr> argExprs, Environment env) throws EvalError {
        if (argExprs.isEmpty()) {
            return valueStep(BoolValue.FALSE);
        }

        for (int index = 0; index < argExprs.size() - 1; index++) {
            Value value = eval(argExprs.get(index), env);
            if (isTruthy(value)) {
                return valueStep(value);
            }
        }
        return tailStep(argExprs.get(argExprs.size() - 1), env);
    }

    private Value evalSequence(List<Expr> exprs, Environment env) throws EvalError {
        return resolveStep(evalSequenceStep(exprs, env));
    }

    private Value evalSequenceOrVoid(List<Expr> exprs, Environment env) throws EvalError {
        return resolveStep(evalSequenceOrVoidStep(exprs, env));
    }

    private EvalStep evalSequenceStep(List<Expr> exprs, Environment env) throws EvalError {
        if (exprs.isEmpty()) {
            return valueStep(VoidValue.INSTANCE);
        }

        for (int index = 0; index < exprs.size() - 1; index++) {
            eval(exprs.get(index), env);
        }
        return tailStep(exprs.get(exprs.size() - 1), env);
    }

    private EvalStep evalSequenceOrVoidStep(List<Expr> exprs, Environment env) throws EvalError {
        if (exprs.isEmpty()) {
            return valueStep(VoidValue.INSTANCE);
        }
        return evalSequenceStep(exprs, env);
    }

    Value applyUserProcedure(String displayName, ParameterSpec parameters,
                             List<Expr> body, Environment closureEnv, List<Value> args)
            throws EvalError {
        return resolveStep(applyUserProcedureStep(displayName, parameters, body, closureEnv, args));
    }

    private EvalStep applyUserProcedureStep(String displayName, ParameterSpec parameters,
                                            List<Expr> body, Environment closureEnv,
                                            List<Value> args) throws EvalError {
        Environment callEnv = createCallEnv(displayName, parameters, closureEnv, args);
        return evalSequenceStep(body, callEnv);
    }

    private Environment createCallEnv(String displayName, ParameterSpec parameters,
                                      Environment closureEnv, List<Value> args) throws EvalError {
        validateArity(displayName, parameters, args.size());

        int requiredCount = parameters.requiredParameters().size();
        Environment callEnv = new Environment(closureEnv);
        for (int index = 0; index < requiredCount; index++) {
            callEnv.define(parameters.requiredParameters().get(index), args.get(index));
        }
        if (parameters.restParameter() != null) {
            callEnv.define(parameters.restParameter(), makeList(args.subList(requiredCount,
                    args.size())));
        }
        return callEnv;
    }

    private void validateArity(String displayName, ParameterSpec parameters, int actual)
            throws EvalError {
        int requiredCount = parameters.requiredParameters().size();
        if (parameters.restParameter() == null) {
            requireArity(displayName, actual, requiredCount);
            return;
        }
        if (actual < requiredCount) {
            requireAtLeast(displayName, actual, requiredCount);
        }
    }

    boolean matchesArity(ParameterSpec parameters, int actual) {
        int requiredCount = parameters.requiredParameters().size();
        if (parameters.restParameter() == null) {
            return actual == requiredCount;
        }
        return actual >= requiredCount;
    }

    private String freshSyntheticName(String kind, String base) {
        syntheticCounter++;
        return "__ming$" + kind + "$" + syntheticCounter + "$" + base;
    }

    private EvalStep applyProcedureStep(Value procedureValue, List<Value> argumentValues)
            throws EvalError {
        if (!(procedureValue instanceof ProcedureValue procedure)) {
            throw new EvalError("not a procedure");
        }

        if (procedure instanceof UserProcedure userProcedure) {
            return applyUserProcedureStep(userProcedure.displayName(),
                    userProcedure.parameters(), userProcedure.body(),
                    userProcedure.closureEnv(), argumentValues);
        }

        if (procedure instanceof CaseLambdaProcedure caseLambdaProcedure) {
            return applyCaseLambdaProcedureStep(caseLambdaProcedure, argumentValues);
        }

        return valueStep(procedure.apply(argumentValues));
    }

    private Value applyProcedure(Value procedureValue, List<Value> argumentValues)
            throws EvalError {
        return resolveStep(applyProcedureStep(procedureValue, argumentValues));
    }

    private EvalStep applyCaseLambdaProcedureStep(CaseLambdaProcedure procedure, List<Value> args)
            throws EvalError {
        for (ProcedureClause clause : procedure.clauses()) {
            if (matchesArity(clause.parameters(), args.size())) {
                return applyUserProcedureStep("case-lambda", clause.parameters(), clause.body(),
                        procedure.closureEnv(), args);
            }
        }
        throw new EvalError("wrong number of arguments for case-lambda: got " + args.size());
    }

    private EvalStep valueStep(Value value) {
        return new ValueStep(value);
    }

    private EvalStep tailStep(Expr expr, Environment env) {
        return new TailStep(expr, env);
    }

    private Value resolveStep(EvalStep step) throws EvalError {
        if (step instanceof ValueStep valueStep) {
            return valueStep.value();
        }

        TailStep tailStep = (TailStep) step;
        return eval(tailStep.expr(), tailStep.env());
    }

    private Value quoteToValue(Expr expr) throws EvalError {
        return switch (expr) {
            case IntExpr intExpr -> new IntValue(intExpr.value());
            case RationalExpr rationalExpr -> NumericSupport.exactToValue(
                    new ExactFraction(rationalExpr.numerator(), rationalExpr.denominator()));
            case InexactExpr inexactExpr -> new InexactValue(inexactExpr.value());
            case BoolExpr boolExpr -> BoolValue.of(boolExpr.value());
            case StringExpr stringExpr -> new StringValue(stringExpr.value());
            case CharExpr charExpr -> new CharValue(charExpr.value());
            case SymbolExpr symbolExpr -> new SymbolValue(symbolExpr.name());
            case ListExpr listExpr -> quoteListToValue(listExpr.elements());
        };
    }

    private Value quoteListToValue(List<Expr> elements) throws EvalError {
        Value result = EmptyListValue.INSTANCE;
        for (int index = elements.size() - 1; index >= 0; index--) {
            result = new PairValue(quoteToValue(elements.get(index)), result);
        }
        return result;
    }

    Value addNumbers(List<Value> args) throws EvalError {
        if (containsInexact(args)) {
            double total = 0.0;
            for (Value arg : args) {
                total += NumericSupport.toDouble(arg);
            }
            return new InexactValue(total);
        }

        ExactFraction total = ExactFraction.of(0);
        for (Value arg : args) {
            total = total.add(NumericSupport.toExactFraction(arg));
        }
        return NumericSupport.exactToValue(total);
    }

    Value subtractNumbers(List<Value> args) throws EvalError {
        requireAtLeast("-", args.size(), 1);

        if (containsInexact(args)) {
            double result = NumericSupport.toDouble(args.getFirst());
            if (args.size() == 1) {
                return new InexactValue(-result);
            }

            for (int index = 1; index < args.size(); index++) {
                result -= NumericSupport.toDouble(args.get(index));
            }
            return new InexactValue(result);
        }

        ExactFraction result = NumericSupport.toExactFraction(args.getFirst());
        if (args.size() == 1) {
            return NumericSupport.exactToValue(result.negate());
        }

        for (int index = 1; index < args.size(); index++) {
            result = result.subtract(NumericSupport.toExactFraction(args.get(index)));
        }
        return NumericSupport.exactToValue(result);
    }

    Value multiplyNumbers(List<Value> args) throws EvalError {
        if (containsInexact(args)) {
            double total = 1.0;
            for (Value arg : args) {
                total *= NumericSupport.toDouble(arg);
            }
            return new InexactValue(total);
        }

        ExactFraction total = ExactFraction.of(1);
        for (Value arg : args) {
            total = total.multiply(NumericSupport.toExactFraction(arg));
        }
        return NumericSupport.exactToValue(total);
    }

    Value divideNumbers(List<Value> args) throws EvalError {
        requireAtLeast("/", args.size(), 2);

        if (containsInexact(args)) {
            double result = NumericSupport.toDouble(args.getFirst());
            for (int index = 1; index < args.size(); index++) {
                double divisor = NumericSupport.toDouble(args.get(index));
                if (divisor == 0.0) {
                    throw new EvalError("division by zero");
                }
                result /= divisor;
            }
            return new InexactValue(result);
        }

        ExactFraction result = NumericSupport.toExactFraction(args.getFirst());
        for (int index = 1; index < args.size(); index++) {
            result = result.divide(NumericSupport.toExactFraction(args.get(index)));
        }
        return NumericSupport.exactToValue(result);
    }

    Value absBuiltin(List<Value> args) throws EvalError {
        requireArity("abs", args.size(), 1);

        Value value = expectNumber(args.getFirst());
        if (value instanceof InexactValue inexactValue) {
            return new InexactValue(Math.abs(inexactValue.value()));
        }

        ExactFraction fraction = NumericSupport.toExactFraction(value);
        if (fraction.signum() < 0) {
            fraction = fraction.negate();
        }
        return NumericSupport.exactToValue(fraction);
    }

    int quotient(List<Value> args) throws EvalError {
        requireArity("quotient", args.size(), 2);

        int dividend = expectInt(args.get(0));
        int divisor = expectInt(args.get(1));
        if (divisor == 0) {
            throw new EvalError("division by zero");
        }
        return dividend / divisor;
    }

    int remainder(List<Value> args) throws EvalError {
        requireArity("remainder", args.size(), 2);

        int dividend = expectInt(args.get(0));
        int divisor = expectInt(args.get(1));
        if (divisor == 0) {
            throw new EvalError("division by zero");
        }
        return dividend % divisor;
    }

    int modulo(List<Value> args) throws EvalError {
        requireArity("modulo", args.size(), 2);

        int dividend = expectInt(args.get(0));
        int divisor = expectInt(args.get(1));
        if (divisor == 0) {
            throw new EvalError("division by zero");
        }
        return Math.floorMod(dividend, divisor);
    }

    Value minBuiltin(List<Value> args) throws EvalError {
        requireAtLeast("min", args.size(), 1);

        Value result = expectNumber(args.getFirst());
        boolean sawInexact = NumericSupport.isInexact(result);
        for (int index = 1; index < args.size(); index++) {
            Value current = expectNumber(args.get(index));
            if (NumericSupport.compare(current, result) < 0) {
                result = current;
            }
            sawInexact |= NumericSupport.isInexact(current);
        }
        if (sawInexact && NumericSupport.isExact(result)) {
            return exactToInexact(result);
        }
        return result;
    }

    Value maxBuiltin(List<Value> args) throws EvalError {
        requireAtLeast("max", args.size(), 1);

        Value result = expectNumber(args.getFirst());
        boolean sawInexact = NumericSupport.isInexact(result);
        for (int index = 1; index < args.size(); index++) {
            Value current = expectNumber(args.get(index));
            if (NumericSupport.compare(current, result) > 0) {
                result = current;
            }
            sawInexact |= NumericSupport.isInexact(current);
        }
        if (sawInexact && NumericSupport.isExact(result)) {
            return exactToInexact(result);
        }
        return result;
    }

    int expt(List<Value> args) throws EvalError {
        requireArity("expt", args.size(), 2);

        int base = expectInt(args.get(0));
        int exponent = expectInt(args.get(1));
        if (exponent < 0) {
            throw new EvalError("expt exponent must be non-negative");
        }

        int result = 1;
        for (int index = 0; index < exponent; index++) {
            result *= base;
        }
        return result;
    }

    boolean compareIncreasing(List<Value> args, Comparison comparison)
            throws EvalError {
        requireAtLeast(comparison.symbol(), args.size(), 2);

        Value previous = expectNumber(args.getFirst());
        for (int index = 1; index < args.size(); index++) {
            Value current = expectNumber(args.get(index));
            if (!comparison.matches(NumericSupport.compare(previous, current))) {
                return false;
            }
            previous = current;
        }
        return true;
    }

    private boolean containsInexact(List<Value> args) throws EvalError {
        for (Value arg : args) {
            expectNumber(arg);
            if (NumericSupport.isInexact(arg)) {
                return true;
            }
        }
        return false;
    }

    Value expectNumber(Value value) throws EvalError {
        if (NumericSupport.isNumber(value)) {
            return value;
        }
        throw new EvalError("expected number");
    }

    int expectInt(Value value) throws EvalError {
        BigInteger integer = NumericSupport.expectExactInteger(value);
        try {
            return integer.intValueExact();
        } catch (ArithmeticException error) {
            throw new EvalError("integer out of range");
        }
    }

    int expectIndex(Value value, String operationName) throws EvalError {
        int index = expectInt(value);
        if (index < 0) {
            throw new EvalError(operationName + " index out of range");
        }
        return index;
    }

    String expectString(Value value) throws EvalError {
        return expectStringValue(value).value();
    }

    StringValue expectStringValue(Value value) throws EvalError {
        if (value instanceof StringValue stringValue) {
            return stringValue;
        }
        throw new EvalError("expected string");
    }

    char expectChar(Value value) throws EvalError {
        if (value instanceof CharValue charValue) {
            return charValue.value();
        }
        throw new EvalError("expected character");
    }

    String expectSymbol(Value value) throws EvalError {
        if (value instanceof SymbolValue symbolValue) {
            return symbolValue.name();
        }
        throw new EvalError("expected symbol");
    }

    PairValue expectPair(Value value) throws EvalError {
        if (value instanceof PairValue pairValue) {
            return pairValue;
        }
        throw new EvalError("expected pair");
    }

    VectorValue expectVectorValue(Value value) throws EvalError {
        if (value instanceof VectorValue vectorValue) {
            return vectorValue;
        }
        throw new EvalError("expected vector");
    }

    private RecordValue expectRecord(Value value, RecordType recordType) throws EvalError {
        if (value instanceof RecordValue recordValue && recordValue.type() == recordType) {
            return recordValue;
        }
        throw new EvalError("expected record of type " + recordType.name());
    }

    int lengthOfList(Value value) throws EvalError {
        int length = 0;
        Value current = value;
        while (current instanceof PairValue pairValue) {
            length++;
            current = pairValue.cdr();
        }
        if (current instanceof EmptyListValue) {
            return length;
        }
        throw new EvalError("expected list");
    }

    List<Value> listElements(Value value) throws EvalError {
        List<Value> elements = new ArrayList<>();
        Value current = value;
        while (current instanceof PairValue pairValue) {
            elements.add(pairValue.car());
            current = pairValue.cdr();
        }
        if (current instanceof EmptyListValue) {
            return elements;
        }
        throw new EvalError("expected list");
    }

    Value listRef(List<Value> args) throws EvalError {
        requireArity("list-ref", args.size(), 2);

        int index = expectIndex(args.get(1), "list-ref");
        Value current = args.getFirst();
        for (int remaining = index; remaining >= 0; remaining--) {
            if (!(current instanceof PairValue pairValue)) {
                throw new EvalError("list-ref index out of range");
            }
            if (remaining == 0) {
                return pairValue.car();
            }
            current = pairValue.cdr();
        }

        throw new EvalError("list-ref index out of range");
    }

    Value listTailBuiltin(List<Value> args) throws EvalError {
        requireArity("list-tail", args.size(), 2);
        return listTail(args.getFirst(), expectIndex(args.get(1), "list-tail"));
    }

    private Value listTail(Value value, int index) throws EvalError {
        Value current = value;
        for (int remaining = index; remaining > 0; remaining--) {
            if (!(current instanceof PairValue pairValue)) {
                throw new EvalError("list-tail index out of range");
            }
            current = pairValue.cdr();
        }
        if (current instanceof PairValue || current instanceof EmptyListValue) {
            return current;
        }
        throw new EvalError("list-tail index out of range");
    }

    boolean isProperList(Value value) {
        Value current = value;
        while (current instanceof PairValue pairValue) {
            current = pairValue.cdr();
        }
        return current instanceof EmptyListValue;
    }

    Value makeVectorBuiltin(List<Value> args) throws EvalError {
        if (args.size() < 1 || args.size() > 2) {
            throw new EvalError("wrong number of arguments for make-vector: expected 1 or 2, got "
                    + args.size());
        }

        int length = expectIndex(args.getFirst(), "make-vector");
        Value fill = args.size() == 2 ? args.get(1) : VoidValue.INSTANCE;
        List<Value> elements = new ArrayList<>(length);
        for (int index = 0; index < length; index++) {
            elements.add(fill);
        }
        return new VectorValue(elements);
    }

    Value vectorRefBuiltin(List<Value> args) throws EvalError {
        requireArity("vector-ref", args.size(), 2);

        VectorValue vector = expectVectorValue(args.getFirst());
        int index = expectIndex(args.get(1), "vector-ref");
        if (index >= vector.length()) {
            throw new EvalError("vector-ref index out of range");
        }
        return vector.element(index);
    }

    Value vectorSetBuiltin(List<Value> args) throws EvalError {
        requireArity("vector-set!", args.size(), 3);

        VectorValue vector = expectVectorValue(args.getFirst());
        int index = expectIndex(args.get(1), "vector-set!");
        if (index >= vector.length()) {
            throw new EvalError("vector-set! index out of range");
        }
        vector.setElement(index, args.get(2));
        return VoidValue.INSTANCE;
    }

    Value appendLists(List<Value> args) throws EvalError {
        if (args.isEmpty()) {
            return EmptyListValue.INSTANCE;
        }

        Value result = args.get(args.size() - 1);
        for (int argIndex = args.size() - 2; argIndex >= 0; argIndex--) {
            List<Value> elements = listElements(args.get(argIndex));
            for (int elementIndex = elements.size() - 1; elementIndex >= 0; elementIndex--) {
                result = new PairValue(elements.get(elementIndex), result);
            }
        }
        return result;
    }

    Value applyBuiltin(List<Value> args) throws EvalError {
        requireAtLeast("apply", args.size(), 2);

        List<Value> expandedArgs = new ArrayList<>();
        for (int index = 1; index < args.size() - 1; index++) {
            expandedArgs.add(args.get(index));
        }
        expandedArgs.addAll(listElements(args.get(args.size() - 1)));

        return applyProcedure(args.getFirst(), expandedArgs);
    }

    Value mapBuiltin(List<Value> args) throws EvalError {
        requireAtLeast("map", args.size(), 2);

        Value procedure = args.getFirst();
        List<List<Value>> listArguments = new ArrayList<>(args.size() - 1);
        Integer expectedLength = null;

        for (int index = 1; index < args.size(); index++) {
            List<Value> elements = listElements(args.get(index));
            if (expectedLength == null) {
                expectedLength = elements.size();
            } else if (elements.size() != expectedLength) {
                throw new EvalError("map lists must have the same length");
            }
            listArguments.add(elements);
        }

        List<Value> results = new ArrayList<>(expectedLength == null ? 0 : expectedLength);
        for (int elementIndex = 0; elementIndex < expectedLength; elementIndex++) {
            List<Value> invocationArgs = new ArrayList<>(listArguments.size());
            for (List<Value> listArgument : listArguments) {
                invocationArgs.add(listArgument.get(elementIndex));
            }
            results.add(applyProcedure(procedure, invocationArgs));
        }
        return makeList(results);
    }

    Value assocBuiltin(List<Value> args) throws EvalError {
        requireArity("assoc", args.size(), 2);

        Value key = args.get(0);
        Value current = args.get(1);
        while (current instanceof PairValue pairValue) {
            Value entry = pairValue.car();
            PairValue association = expectPair(entry);
            if (isEqual(key, association.car())) {
                return entry;
            }
            current = pairValue.cdr();
        }
        if (current instanceof EmptyListValue) {
            return BoolValue.FALSE;
        }
        throw new EvalError("expected list");
    }

    String stringAppend(List<Value> args) throws EvalError {
        StringBuilder builder = new StringBuilder();
        for (Value arg : args) {
            builder.append(expectString(arg));
        }
        return builder.toString();
    }

    Value stringToNumber(String token) {
        ParsedNumber parsedNumber;
        try {
            parsedNumber = NumericSupport.parseLiteral(token);
        } catch (IllegalArgumentException error) {
            return BoolValue.FALSE;
        }

        if (parsedNumber == null) {
            return BoolValue.FALSE;
        }
        return parsedNumberToValue(parsedNumber);
    }

    private Value parsedNumberToValue(ParsedNumber parsedNumber) {
        return switch (parsedNumber) {
            case ParsedInteger parsedInteger -> new IntValue(parsedInteger.value());
            case ParsedRational parsedRational -> NumericSupport.exactToValue(
                    new ExactFraction(parsedRational.numerator(), parsedRational.denominator()));
            case ParsedInexact parsedInexact -> new InexactValue(parsedInexact.value());
        };
    }

    Value exactToInexact(Value value) throws EvalError {
        return new InexactValue(NumericSupport.toDouble(value));
    }

    Value numeratorBuiltin(List<Value> args) throws EvalError {
        requireArity("numerator", args.size(), 1);
        ExactFraction fraction = NumericSupport.toExactFraction(args.getFirst());
        return NumericSupport.integerToValue(fraction.numerator());
    }

    Value denominatorBuiltin(List<Value> args) throws EvalError {
        requireArity("denominator", args.size(), 1);
        ExactFraction fraction = NumericSupport.toExactFraction(args.getFirst());
        return NumericSupport.integerToValue(fraction.denominator());
    }

    BoolValue signPredicate(String name, List<Value> args, int expectedSign)
            throws EvalError {
        requireArity(name, args.size(), 1);

        Value value = expectNumber(args.getFirst());
        int sign;
        if (NumericSupport.isExact(value)) {
            sign = NumericSupport.toExactFraction(value).signum();
        } else {
            sign = Double.compare(((InexactValue) value).value(), 0.0);
        }
        return BoolValue.of(sign == expectedSign || (expectedSign == 1 && sign > 0)
                || (expectedSign == -1 && sign < 0));
    }

    boolean compareChars(List<Value> args, String name, CharComparison comparison)
            throws EvalError {
        requireAtLeast(name, args.size(), 2);

        char previous = expectChar(args.getFirst());
        for (int index = 1; index < args.size(); index++) {
            char current = expectChar(args.get(index));
            if (!comparison.matches(previous, current)) {
                return false;
            }
            previous = current;
        }
        return true;
    }

    boolean compareStrings(List<Value> args, String name, StringComparison comparison)
            throws EvalError {
        requireAtLeast(name, args.size(), 2);

        String previous = expectString(args.getFirst());
        for (int index = 1; index < args.size(); index++) {
            String current = expectString(args.get(index));
            if (!comparison.matches(previous, current)) {
                return false;
            }
            previous = current;
        }
        return true;
    }

    boolean isEq(Value left, Value right) throws EvalError {
        if (left == right) {
            return true;
        }
        if (NumericSupport.isNumber(left) && NumericSupport.isNumber(right)) {
            return NumericSupport.compare(left, right) == 0;
        }
        if (left instanceof BoolValue leftBool && right instanceof BoolValue rightBool) {
            return leftBool.value() == rightBool.value();
        }
        if (left instanceof CharValue leftChar && right instanceof CharValue rightChar) {
            return leftChar.value() == rightChar.value();
        }
        if (left instanceof SymbolValue leftSymbol && right instanceof SymbolValue rightSymbol) {
            return leftSymbol.name().equals(rightSymbol.name());
        }
        return false;
    }

    boolean isEqv(Value left, Value right) throws EvalError {
        return isEq(left, right);
    }

    boolean isEqual(Value left, Value right) throws EvalError {
        if (left == right) {
            return true;
        }
        if (NumericSupport.isNumber(left) && NumericSupport.isNumber(right)) {
            return NumericSupport.compare(left, right) == 0;
        }
        if (left instanceof BoolValue leftBool && right instanceof BoolValue rightBool) {
            return leftBool.value() == rightBool.value();
        }
        if (left instanceof StringValue leftString && right instanceof StringValue rightString) {
            return leftString.value().equals(rightString.value());
        }
        if (left instanceof CharValue leftChar && right instanceof CharValue rightChar) {
            return leftChar.value() == rightChar.value();
        }
        if (left instanceof SymbolValue leftSymbol && right instanceof SymbolValue rightSymbol) {
            return leftSymbol.name().equals(rightSymbol.name());
        }
        if (left instanceof PairValue leftPair && right instanceof PairValue rightPair) {
            return isEqual(leftPair.car(), rightPair.car())
                    && isEqual(leftPair.cdr(), rightPair.cdr());
        }
        if (left instanceof VectorValue leftVector && right instanceof VectorValue rightVector) {
            if (leftVector.length() != rightVector.length()) {
                return false;
            }
            for (int index = 0; index < leftVector.length(); index++) {
                if (!isEqual(leftVector.element(index), rightVector.element(index))) {
                    return false;
                }
            }
            return true;
        }
        return false;
    }

    Value makeList(List<Value> args) {
        Value result = EmptyListValue.INSTANCE;
        for (int index = args.size() - 1; index >= 0; index--) {
            result = new PairValue(args.get(index), result);
        }
        return result;
    }

    BoolValue typePredicate(String name, List<Value> args, ValuePredicate predicate)
            throws EvalError {
        requireArity(name, args.size(), 1);
        return BoolValue.of(predicate.matches(args.getFirst()));
    }

    void requireArity(String name, int actual, int expected)
            throws EvalError {
        if (actual != expected) {
            throw new EvalError(
                    "wrong number of arguments for " + name + ": expected " + expected
                            + ", got " + actual
            );
        }
    }

    void requireAtLeast(String name, int actual, int minimum)
            throws EvalError {
        if (actual < minimum) {
            throw new EvalError(
                    "wrong number of arguments for " + name + ": expected at least "
                            + minimum + ", got " + actual
            );
        }
    }

    boolean isTruthy(Value value) {
        return !(value instanceof BoolValue boolValue) || boolValue.value();
    }

}
