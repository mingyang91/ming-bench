package ming;

import java.math.BigInteger;
import java.util.ArrayList;
import java.util.IdentityHashMap;
import java.util.List;

/**
 * Scheme interpreter entry point.
 * Agents implement this class.
 */
public class Evaluator {
    @FunctionalInterface
    private interface Bounce {
        Bounce run() throws EvalError;
    }

    @FunctionalInterface
    interface Continuation {
        Bounce resume(Value value) throws EvalError;
    }

    @FunctionalInterface
    private interface ValueListContinuation {
        Bounce resume(List<Value> values) throws EvalError;
    }

    @FunctionalInterface
    private interface BounceFactory {
        Bounce create(Continuation halt) throws EvalError;
    }

    private static final class ResultBox {
        private Value value;
    }

    private final CollectionProcedures collectionProcedures;
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
        collectionProcedures = new CollectionProcedures(this);
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
        List<Expr> exprs = new ArrayList<>();

        try {
            while (parser.hasMore()) {
                exprs.add(parser.parseExpr());
            }

            if (exprs.isEmpty()) {
                throw new EvalError("empty input", 1, 1);
            }

            return run(halt -> evalSequenceBounce(exprs, globalEnv, halt));
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

    CollectionProcedures collectionProcedures() {
        return collectionProcedures;
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

    private Value run(BounceFactory factory) throws EvalError {
        ResultBox result = new ResultBox();
        Bounce current = factory.create(value -> {
            result.value = value;
            return null;
        });

        while (current != null) {
            current = current.run();
        }
        return result.value;
    }

    private Bounce deliver(Continuation cont, Value value) {
        return () -> cont.resume(value);
    }

    private Bounce deliverValues(ValueListContinuation cont, List<Value> values) {
        return () -> cont.resume(values);
    }

    private Bounce withPosition(SourcePos position, Bounce bounce) {
        return () -> {
            try {
                return bounce.run();
            } catch (EvalError error) {
                throw error.withPosition(position.line(), position.column());
            }
        };
    }

    private Continuation positionedCont(SourcePos position, Continuation cont) {
        return value -> withPosition(position, () -> cont.resume(value));
    }

    private ValueListContinuation positionedValues(SourcePos position, ValueListContinuation cont) {
        return values -> withPosition(position, () -> cont.resume(values));
    }

    private List<Value> appendValue(List<Value> values, Value value) {
        List<Value> next = new ArrayList<>(values.size() + 1);
        next.addAll(values);
        next.add(value);
        return List.copyOf(next);
    }

    private List<Value> prependValue(Value value, List<Value> values) {
        List<Value> next = new ArrayList<>(values.size() + 1);
        next.add(value);
        next.addAll(values);
        return List.copyOf(next);
    }

    private Value eval(Expr expr, Environment env) throws EvalError {
        return run(halt -> evalExpr(expr, env, halt));
    }

    private Bounce evalExpr(Expr expr, Environment env, Continuation cont) {
        return withPosition(expr.position(), () -> switch (expr) {
            case IntExpr intExpr -> deliver(cont, new IntValue(intExpr.value()));
            case RationalExpr rationalExpr -> deliver(cont, NumericSupport.exactToValue(
                    new ExactFraction(rationalExpr.numerator(), rationalExpr.denominator())));
            case InexactExpr inexactExpr -> deliver(cont, new InexactValue(inexactExpr.value()));
            case BoolExpr boolExpr -> deliver(cont, BoolValue.of(boolExpr.value()));
            case StringExpr stringExpr -> deliver(cont, new StringValue(stringExpr.value()));
            case CharExpr charExpr -> deliver(cont, new CharValue(charExpr.value()));
            case SymbolExpr symbolExpr -> deliver(cont, env.lookup(symbolExpr.name()));
            case ListExpr listExpr -> evalList(listExpr, env, cont);
        });
    }

    private Bounce evalList(ListExpr listExpr, Environment env, Continuation cont)
            throws EvalError {
        List<Expr> elements = listExpr.elements();
        if (elements.isEmpty()) {
            throw new EvalError("cannot evaluate empty list");
        }

        Expr head = elements.getFirst();
        List<Expr> argExprs = elements.subList(1, elements.size());
        SourcePos position = listExpr.position();

        if (head instanceof SymbolExpr symbolExpr) {
            return switch (symbolExpr.name()) {
                case "define" -> evalDefine(position, argExprs, env, cont);
                case "define-syntax" -> evalDefineSyntax(argExprs, env, cont);
                case "define-record-type" -> evalDefineRecordType(argExprs, env, cont);
                case "set!" -> evalSet(position, argExprs, env, cont);
                case "if" -> evalIf(position, argExprs, env, cont);
                case "quote" -> evalQuote(argExprs, cont);
                case "lambda" -> evalLambda(argExprs, env, cont);
                case "case-lambda" -> evalCaseLambda(argExprs, env, cont);
                case "begin" -> evalSequenceBounce(argExprs, env, cont);
                case "let" -> evalLet(position, argExprs, env, cont);
                case "let*" -> evalLetStar(position, argExprs, env, cont);
                case "letrec" -> evalLetrec(position, argExprs, env, false, cont);
                case "letrec*" -> evalLetrec(position, argExprs, env, true, cont);
                case "cond" -> evalCond(position, argExprs, 0, env, cont);
                case "case" -> evalCase(position, argExprs, env, cont);
                case "and" -> evalAnd(argExprs, 0, env, cont);
                case "or" -> evalOr(argExprs, 0, env, cont);
                case "do" -> evalDo(position, argExprs, env, cont);
                default -> {
                    MacroBinding macro = env.lookupSyntax(symbolExpr.name());
                    if (macro != null) {
                        yield evalExpr(macro.expand(listExpr), env, cont);
                    }
                    yield evalApplication(position, head, argExprs, env, cont);
                }
            };
        }

        return evalApplication(position, head, argExprs, env, cont);
    }

    private Bounce evalDefine(SourcePos position, List<Expr> argExprs, Environment env,
                              Continuation cont) throws EvalError {
        if (argExprs.size() < 2) {
            throw new EvalError("define requires a name and a value");
        }

        Expr target = argExprs.getFirst();
        if (target instanceof SymbolExpr symbolExpr) {
            requireArity("define", argExprs.size(), 2);
            return evalExpr(argExprs.get(1), env, positionedCont(position, value -> {
                env.define(symbolExpr.name(), value);
                return deliver(cont, VoidValue.INSTANCE);
            }));
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
            env.define(nameExpr.name(), new UserProcedure(this, nameExpr.name(), parameters, body,
                    env));
            return deliver(cont, VoidValue.INSTANCE);
        }

        throw new EvalError("invalid define");
    }

    private Bounce evalSet(SourcePos position, List<Expr> argExprs, Environment env,
                           Continuation cont) throws EvalError {
        requireArity("set!", argExprs.size(), 2);
        if (!(argExprs.getFirst() instanceof SymbolExpr symbolExpr)) {
            throw new EvalError("set! target must be a symbol");
        }

        return evalExpr(argExprs.get(1), env, positionedCont(position, value -> {
            env.set(symbolExpr.name(), value);
            return deliver(cont, VoidValue.INSTANCE);
        }));
    }

    private Bounce evalIf(SourcePos position, List<Expr> argExprs, Environment env,
                          Continuation cont) throws EvalError {
        if (argExprs.size() < 2 || argExprs.size() > 3) {
            throw new EvalError(
                    "wrong number of arguments for if: expected 2 or 3, got " + argExprs.size());
        }

        return evalExpr(argExprs.get(0), env, positionedCont(position, condition -> {
            if (isTruthy(condition)) {
                return evalExpr(argExprs.get(1), env, cont);
            }
            if (argExprs.size() == 2) {
                return deliver(cont, VoidValue.INSTANCE);
            }
            return evalExpr(argExprs.get(2), env, cont);
        }));
    }

    private Bounce evalQuote(List<Expr> argExprs, Continuation cont) throws EvalError {
        requireArity("quote", argExprs.size(), 1);
        return deliver(cont, quoteToValue(argExprs.getFirst()));
    }

    private Bounce evalDefineSyntax(List<Expr> argExprs, Environment env, Continuation cont)
            throws EvalError {
        requireArity("define-syntax", argExprs.size(), 2);

        if (!(argExprs.getFirst() instanceof SymbolExpr nameExpr)) {
            throw new EvalError("define-syntax name must be a symbol");
        }

        env.defineSyntax(nameExpr.name(),
                SyntaxRulesMacro.compile(nameExpr.name(), argExprs.get(1), env,
                        this::freshSyntheticName));
        return deliver(cont, VoidValue.INSTANCE);
    }

    private Bounce evalDefineRecordType(List<Expr> argExprs, Environment env, Continuation cont)
            throws EvalError {
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

        return deliver(cont, VoidValue.INSTANCE);
    }

    private Bounce evalLambda(List<Expr> argExprs, Environment env, Continuation cont)
            throws EvalError {
        if (argExprs.size() < 2) {
            throw new EvalError("lambda requires parameters and a body");
        }

        ParameterSpec parameters = FormParser.parseLambdaParameterSpec(argExprs.getFirst());
        List<Expr> body = FormParser.parseBody("lambda", argExprs.subList(1, argExprs.size()));
        return deliver(cont, new UserProcedure(this, null, parameters, body, env));
    }

    private Bounce evalCaseLambda(List<Expr> argExprs, Environment env, Continuation cont)
            throws EvalError {
        if (argExprs.isEmpty()) {
            throw new EvalError("case-lambda requires at least one clause");
        }

        List<ProcedureClause> clauses = new ArrayList<>(argExprs.size());
        for (Expr clauseExpr : argExprs) {
            clauses.add(FormParser.parseCaseLambdaClause(clauseExpr));
        }
        return deliver(cont, new CaseLambdaProcedure(this, clauses, env));
    }

    private Bounce evalLet(SourcePos position, List<Expr> argExprs, Environment env,
                           Continuation cont) throws EvalError {
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
            return evalNamedLet(position, nameExpr.name(), bindings, body, env, cont);
        }

        if (!(firstArg instanceof ListExpr bindingsExpr)) {
            throw new EvalError("let bindings must be a list");
        }

        List<LetBinding> bindings = FormParser.parseBindings(bindingsExpr.elements());
        List<Expr> body = FormParser.parseBody("let", argExprs.subList(1, argExprs.size()));
        return evalSimpleLet(position, bindings, body, env, cont);
    }

    private Bounce evalLetStar(SourcePos position, List<Expr> argExprs, Environment env,
                               Continuation cont) throws EvalError {
        if (argExprs.isEmpty()) {
            throw new EvalError("let* requires bindings and a body");
        }
        if (!(argExprs.getFirst() instanceof ListExpr bindingsExpr)) {
            throw new EvalError("let* bindings must be a list");
        }

        List<LetBinding> bindings = FormParser.parseBindings(bindingsExpr.elements());
        List<Expr> body = FormParser.parseBody("let*", argExprs.subList(1, argExprs.size()));
        Environment letEnv = new Environment(env);
        return evalLetStarBindings(position, bindings, 0, letEnv, body, cont);
    }

    private Bounce evalLetrec(SourcePos position, List<Expr> argExprs, Environment env,
                              boolean sequential, Continuation cont) throws EvalError {
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
            return evalLetrecSequential(position, bindings, 0, letrecEnv, bindingCells, body, cont);
        }

        return evalBindingValues(bindings, 0, List.of(), letrecEnv, position,
                positionedValues(position, values -> {
                    for (int index = 0; index < values.size(); index++) {
                        bindingCells.get(index).set(values.get(index));
                    }
                    return evalSequenceBounce(body, letrecEnv, cont);
                }));
    }

    private Bounce evalSimpleLet(SourcePos position, List<LetBinding> bindings, List<Expr> body,
                                 Environment env, Continuation cont) throws EvalError {
        return evalBindingValues(bindings, 0, List.of(), env, position,
                positionedValues(position, values -> {
                    Environment letEnv = new Environment(env);
                    for (int index = 0; index < bindings.size(); index++) {
                        letEnv.define(bindings.get(index).name(), values.get(index));
                    }
                    return evalSequenceBounce(body, letEnv, cont);
                }));
    }

    private Bounce evalNamedLet(SourcePos position, String name, List<LetBinding> bindings,
                                List<Expr> body, Environment env, Continuation cont)
            throws EvalError {
        List<String> parameterNames = new ArrayList<>(bindings.size());
        for (LetBinding binding : bindings) {
            parameterNames.add(binding.name());
        }

        return evalBindingValues(bindings, 0, List.of(), env, position,
                positionedValues(position, values -> {
                    Environment letEnv = new Environment(env);
                    ProcedureValue procedure = new UserProcedure(this, name,
                            new ParameterSpec(parameterNames, null), body, letEnv);
                    letEnv.define(name, procedure);
                    return applyProcedureCps(procedure, values, cont);
                }));
    }

    private Bounce evalBindingValues(List<LetBinding> bindings, int index, List<Value> values,
                                     Environment valueEnv, SourcePos position,
                                     ValueListContinuation cont) throws EvalError {
        if (index >= bindings.size()) {
            return deliverValues(cont, values);
        }

        return evalExpr(bindings.get(index).valueExpr(), valueEnv,
                positionedCont(position, value -> evalBindingValues(bindings, index + 1,
                        appendValue(values, value), valueEnv, position, cont)));
    }

    private Bounce evalLetStarBindings(SourcePos position, List<LetBinding> bindings, int index,
                                       Environment letEnv, List<Expr> body, Continuation cont)
            throws EvalError {
        if (index >= bindings.size()) {
            return evalSequenceBounce(body, letEnv, cont);
        }

        LetBinding binding = bindings.get(index);
        return evalExpr(binding.valueExpr(), letEnv, positionedCont(position, value -> {
            letEnv.define(binding.name(), value);
            return evalLetStarBindings(position, bindings, index + 1, letEnv, body, cont);
        }));
    }

    private Bounce evalLetrecSequential(SourcePos position, List<LetBinding> bindings, int index,
                                        Environment letrecEnv, List<Cell> bindingCells,
                                        List<Expr> body, Continuation cont) throws EvalError {
        if (index >= bindings.size()) {
            return evalSequenceBounce(body, letrecEnv, cont);
        }

        return evalExpr(bindings.get(index).valueExpr(), letrecEnv,
                positionedCont(position, value -> {
                    bindingCells.get(index).set(value);
                    return evalLetrecSequential(position, bindings, index + 1, letrecEnv,
                            bindingCells, body, cont);
                }));
    }

    private Bounce evalCase(SourcePos position, List<Expr> argExprs, Environment env,
                            Continuation cont) throws EvalError {
        if (argExprs.isEmpty()) {
            throw new EvalError("case requires a key and at least zero clauses");
        }

        return evalExpr(argExprs.getFirst(), env, positionedCont(position,
                key -> evalCaseClauses(argExprs.subList(1, argExprs.size()), key, env, cont)));
    }

    private Bounce evalCaseClauses(List<Expr> clauseExprs, Value key, Environment env,
                                   Continuation cont) throws EvalError {
        for (int index = 0; index < clauseExprs.size(); index++) {
            Expr clauseExpr = clauseExprs.get(index);
            if (!(clauseExpr instanceof ListExpr clauseList)) {
                throw new EvalError("case clause must be a list");
            }

            List<Expr> clause = clauseList.elements();
            if (clause.isEmpty()) {
                throw new EvalError("case clause cannot be empty");
            }

            Expr head = clause.getFirst();
            if (head instanceof SymbolExpr symbolExpr && symbolExpr.name().equals("else")) {
                if (index != clauseExprs.size() - 1) {
                    throw new EvalError("case else clause must be last");
                }
                return evalSequenceBounce(clause.subList(1, clause.size()), env, cont);
            }

            if (!(head instanceof ListExpr datumList)) {
                throw new EvalError("case clause datums must be a list");
            }

            for (Expr datumExpr : datumList.elements()) {
                if (isEqv(key, quoteToValue(datumExpr))) {
                    return evalSequenceBounce(clause.subList(1, clause.size()), env, cont);
                }
            }
        }

        return deliver(cont, VoidValue.INSTANCE);
    }

    private Bounce evalDo(SourcePos position, List<Expr> argExprs, Environment env,
                          Continuation cont) throws EvalError {
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

        return evalDoInitialBindings(bindings, 0, List.of(), env, position,
                positionedValues(position, values -> {
                    Environment loopEnv = new Environment(env);
                    List<Cell> bindingCells = new ArrayList<>(bindings.size());
                    for (int index = 0; index < bindings.size(); index++) {
                        Cell cell = new Cell(values.get(index));
                        loopEnv.defineAlias(bindings.get(index).name(), cell);
                        bindingCells.add(cell);
                    }
                    return evalDoIteration(position, bindings, terminationParts,
                            argExprs.subList(2, argExprs.size()), loopEnv, bindingCells, cont);
                }));
    }

    private Bounce evalDoInitialBindings(List<FormParser.DoBinding> bindings, int index,
                                         List<Value> values, Environment env, SourcePos position,
                                         ValueListContinuation cont) throws EvalError {
        if (index >= bindings.size()) {
            return deliverValues(cont, values);
        }

        return evalExpr(bindings.get(index).initExpr(), env,
                positionedCont(position, value -> evalDoInitialBindings(bindings, index + 1,
                        appendValue(values, value), env, position, cont)));
    }

    private Bounce evalDoIteration(SourcePos position, List<FormParser.DoBinding> bindings,
                                   List<Expr> terminationParts, List<Expr> body,
                                   Environment loopEnv, List<Cell> bindingCells,
                                   Continuation cont) throws EvalError {
        return evalExpr(terminationParts.getFirst(), loopEnv,
                positionedCont(position, testValue -> {
                    if (isTruthy(testValue)) {
                        return evalSequenceBounce(
                                terminationParts.subList(1, terminationParts.size()), loopEnv,
                                cont);
                    }

                    return evalSequenceBounce(body, loopEnv, positionedCont(position,
                            ignored -> evalDoStepValues(position, bindings, 0, List.of(), loopEnv,
                                    bindingCells, terminationParts, body, cont)));
                }));
    }

    private Bounce evalDoStepValues(SourcePos position, List<FormParser.DoBinding> bindings,
                                    int index, List<Value> nextValues, Environment loopEnv,
                                    List<Cell> bindingCells, List<Expr> terminationParts,
                                    List<Expr> body, Continuation cont) throws EvalError {
        if (index >= bindings.size()) {
            for (int valueIndex = 0; valueIndex < nextValues.size(); valueIndex++) {
                bindingCells.get(valueIndex).set(nextValues.get(valueIndex));
            }
            return evalDoIteration(position, bindings, terminationParts, body, loopEnv,
                    bindingCells, cont);
        }

        Expr stepExpr = bindings.get(index).stepExpr();
        if (stepExpr == null) {
            return evalDoStepValues(position, bindings, index + 1,
                    appendValue(nextValues, bindingCells.get(index).value()), loopEnv,
                    bindingCells, terminationParts, body, cont);
        }

        return evalExpr(stepExpr, loopEnv, positionedCont(position,
                value -> evalDoStepValues(position, bindings, index + 1,
                        appendValue(nextValues, value), loopEnv, bindingCells,
                        terminationParts, body, cont)));
    }

    private Bounce evalCond(SourcePos position, List<Expr> clauses, int index, Environment env,
                            Continuation cont) throws EvalError {
        if (index >= clauses.size()) {
            return deliver(cont, VoidValue.INSTANCE);
        }

        Expr clauseExpr = clauses.get(index);
        if (!(clauseExpr instanceof ListExpr clauseList)) {
            throw new EvalError("cond clause must be a list");
        }

        List<Expr> clause = clauseList.elements();
        if (clause.isEmpty()) {
            throw new EvalError("cond clause cannot be empty");
        }

        Expr testExpr = clause.getFirst();
        if (testExpr instanceof SymbolExpr symbolExpr && symbolExpr.name().equals("else")) {
            if (index != clauses.size() - 1) {
                throw new EvalError("cond else clause must be last");
            }
            return evalClauseBody("cond", clause.subList(1, clause.size()), env, null, cont);
        }

        return evalExpr(testExpr, env, positionedCont(position, testValue -> {
            if (isTruthy(testValue)) {
                return evalClauseBody("cond", clause.subList(1, clause.size()), env, testValue,
                        cont);
            }
            return evalCond(position, clauses, index + 1, env, cont);
        }));
    }

    private Bounce evalClauseBody(String formName, List<Expr> body, Environment env,
                                  Value defaultValue, Continuation cont) throws EvalError {
        if (body.isEmpty()) {
            if (defaultValue != null) {
                return deliver(cont, defaultValue);
            }
            throw new EvalError(formName + " clause requires a body");
        }
        return evalSequenceBounce(body, env, cont);
    }

    private Bounce evalAnd(List<Expr> argExprs, int index, Environment env, Continuation cont)
            throws EvalError {
        if (argExprs.isEmpty()) {
            return deliver(cont, BoolValue.TRUE);
        }
        if (index == argExprs.size() - 1) {
            return evalExpr(argExprs.get(index), env, cont);
        }

        return evalExpr(argExprs.get(index), env, value -> {
            if (!isTruthy(value)) {
                return deliver(cont, value);
            }
            return evalAnd(argExprs, index + 1, env, cont);
        });
    }

    private Bounce evalOr(List<Expr> argExprs, int index, Environment env, Continuation cont)
            throws EvalError {
        if (argExprs.isEmpty()) {
            return deliver(cont, BoolValue.FALSE);
        }
        if (index == argExprs.size() - 1) {
            return evalExpr(argExprs.get(index), env, cont);
        }

        return evalExpr(argExprs.get(index), env, value -> {
            if (isTruthy(value)) {
                return deliver(cont, value);
            }
            return evalOr(argExprs, index + 1, env, cont);
        });
    }

    private Bounce evalApplication(SourcePos position, Expr head, List<Expr> argExprs,
                                   Environment env, Continuation cont) {
        return evalExpr(head, env, positionedCont(position, procedureValue ->
                evalArgumentValues(argExprs, argExprs.size() - 1, List.of(), env, position,
                        positionedValues(position,
                                values -> applyProcedureCps(procedureValue, values, cont)))));
    }

    private Bounce evalArgumentValues(List<Expr> argExprs, int index, List<Value> values,
                                      Environment env, SourcePos position,
                                      ValueListContinuation cont) throws EvalError {
        if (index < 0) {
            return deliverValues(cont, values);
        }

        return evalExpr(argExprs.get(index), env, positionedCont(position,
                value -> evalArgumentValues(argExprs, index - 1,
                        prependValue(value, values), env, position, cont)));
    }

    private Value evalSequence(List<Expr> exprs, Environment env) throws EvalError {
        return run(halt -> evalSequenceBounce(exprs, env, halt));
    }

    private Value evalSequenceOrVoid(List<Expr> exprs, Environment env) throws EvalError {
        return run(halt -> evalSequenceBounce(exprs, env, halt));
    }

    private Bounce evalSequenceBounce(List<Expr> exprs, Environment env, Continuation cont)
            throws EvalError {
        if (exprs.isEmpty()) {
            return deliver(cont, VoidValue.INSTANCE);
        }
        if (exprs.size() == 1) {
            return evalExpr(exprs.getFirst(), env, cont);
        }

        return evalExpr(exprs.getFirst(), env,
                ignored -> evalSequenceBounce(exprs.subList(1, exprs.size()), env, cont));
    }

    Value applyUserProcedure(String displayName, ParameterSpec parameters,
                             List<Expr> body, Environment closureEnv, List<Value> args)
            throws EvalError {
        return run(halt -> applyUserProcedureCps(displayName, parameters, body, closureEnv,
                args, halt));
    }

    private Bounce applyUserProcedureCps(String displayName, ParameterSpec parameters,
                                         List<Expr> body, Environment closureEnv,
                                         List<Value> args, Continuation cont) throws EvalError {
        Environment callEnv = createCallEnv(displayName, parameters, closureEnv, args);
        return evalSequenceBounce(body, callEnv, cont);
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
            callEnv.define(parameters.restParameter(),
                    collectionProcedures.makeList(args.subList(requiredCount, args.size())));
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

    Value applyProcedure(Value procedureValue, List<Value> argumentValues)
            throws EvalError {
        return run(halt -> applyProcedureCps(procedureValue, argumentValues, halt));
    }

    private Bounce applyProcedureCps(Value procedureValue, List<Value> argumentValues,
                                     Continuation cont) throws EvalError {
        if (!(procedureValue instanceof ProcedureValue procedure)) {
            throw new EvalError("not a procedure");
        }

        if (procedure instanceof ContinuationProcedure continuationProcedure) {
            requireArity("continuation", argumentValues.size(), 1);
            return deliver(continuationProcedure.continuation(), argumentValues.getFirst());
        }

        if (procedure instanceof CallCcProcedure callCcProcedure) {
            requireArity(callCcProcedure.name(), argumentValues.size(), 1);
            return applyProcedureCps(argumentValues.getFirst(),
                    List.of(new ContinuationProcedure(cont)), cont);
        }

        if (procedure instanceof UserProcedure userProcedure) {
            return applyUserProcedureCps(userProcedure.displayName(), userProcedure.parameters(),
                    userProcedure.body(), userProcedure.closureEnv(), argumentValues, cont);
        }

        if (procedure instanceof CaseLambdaProcedure caseLambdaProcedure) {
            return applyCaseLambdaProcedureCps(caseLambdaProcedure, argumentValues, cont);
        }

        return deliver(cont, procedure.apply(argumentValues));
    }

    private Bounce applyCaseLambdaProcedureCps(CaseLambdaProcedure procedure, List<Value> args,
                                               Continuation cont) throws EvalError {
        for (ProcedureClause clause : procedure.clauses()) {
            if (matchesArity(clause.parameters(), args.size())) {
                return applyUserProcedureCps("case-lambda", clause.parameters(), clause.body(),
                        procedure.closureEnv(), args, cont);
            }
        }
        throw new EvalError("wrong number of arguments for case-lambda: got " + args.size());
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

    Value gcdBuiltin(List<Value> args) throws EvalError {
        BigInteger result = BigInteger.ZERO;
        for (Value arg : args) {
            result = result.gcd(NumericSupport.expectExactInteger(arg).abs());
        }
        return NumericSupport.integerToValue(result);
    }

    Value lcmBuiltin(List<Value> args) throws EvalError {
        BigInteger result = BigInteger.ONE;
        boolean sawArgument = false;

        for (Value arg : args) {
            BigInteger value = NumericSupport.expectExactInteger(arg).abs();
            sawArgument = true;
            if (value.signum() == 0) {
                result = BigInteger.ZERO;
                break;
            }
            result = result.divide(result.gcd(value)).multiply(value);
        }

        if (!sawArgument) {
            return new IntValue(1);
        }
        return NumericSupport.integerToValue(result);
    }

    Value truncateBuiltin(List<Value> args) throws EvalError {
        requireArity("truncate", args.size(), 1);

        Value value = expectNumber(args.getFirst());
        if (value instanceof InexactValue inexactValue) {
            double raw = inexactValue.value();
            return new InexactValue(raw < 0.0 ? Math.ceil(raw) : Math.floor(raw));
        }

        ExactFraction fraction = NumericSupport.toExactFraction(value);
        return NumericSupport.integerToValue(
                fraction.numerator().divide(fraction.denominator()));
    }

    Value roundBuiltin(List<Value> args) throws EvalError {
        requireArity("round", args.size(), 1);

        Value value = expectNumber(args.getFirst());
        if (value instanceof InexactValue inexactValue) {
            return new InexactValue(Math.rint(inexactValue.value()));
        }

        ExactFraction fraction = NumericSupport.toExactFraction(value);
        BigInteger[] quotientAndRemainder = fraction.numerator().divideAndRemainder(
                fraction.denominator());
        BigInteger quotient = quotientAndRemainder[0];
        BigInteger doubledRemainder = quotientAndRemainder[1].abs().multiply(BigInteger.TWO);
        int relation = doubledRemainder.compareTo(fraction.denominator());

        if (relation > 0 || (relation == 0 && quotient.testBit(0))) {
            quotient = quotient.add(BigInteger.valueOf(fraction.signum()));
        }
        return NumericSupport.integerToValue(quotient);
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
        return isEqual(left, right, new IdentityHashMap<>());
    }

    private boolean isEqual(Value left, Value right,
                            IdentityHashMap<Value, IdentityHashMap<Value, Boolean>> seenPairs)
            throws EvalError {
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
            if (alreadyCompared(leftPair, rightPair, seenPairs)) {
                return true;
            }
            return isEqual(leftPair.car(), rightPair.car(), seenPairs)
                    && isEqual(leftPair.cdr(), rightPair.cdr(), seenPairs);
        }
        if (left instanceof VectorValue leftVector && right instanceof VectorValue rightVector) {
            if (leftVector.length() != rightVector.length()) {
                return false;
            }
            if (alreadyCompared(leftVector, rightVector, seenPairs)) {
                return true;
            }
            for (int index = 0; index < leftVector.length(); index++) {
                if (!isEqual(leftVector.element(index), rightVector.element(index), seenPairs)) {
                    return false;
                }
            }
            return true;
        }
        return false;
    }

    Value errorBuiltin(List<Value> args) throws EvalError {
        requireAtLeast("error", args.size(), 1);

        Value messageValue = args.getFirst();
        StringBuilder builder = new StringBuilder();
        if (messageValue instanceof StringValue stringValue) {
            builder.append(stringValue.value());
        } else {
            builder.append(messageValue.render());
        }

        for (int index = 1; index < args.size(); index++) {
            if (index == 1) {
                builder.append(':');
            }
            builder.append(' ');
            builder.append(args.get(index).render());
        }

        throw new EvalError(builder.toString());
    }

    BoolValue typePredicate(String name, List<Value> args, ValuePredicate predicate)
            throws EvalError {
        requireArity(name, args.size(), 1);
        return BoolValue.of(predicate.matches(args.getFirst()));
    }

    private boolean alreadyCompared(Value left, Value right,
                                    IdentityHashMap<Value,
                                            IdentityHashMap<Value, Boolean>> seenPairs) {
        IdentityHashMap<Value, Boolean> rightValues = seenPairs.get(left);
        if (rightValues == null) {
            rightValues = new IdentityHashMap<>();
            seenPairs.put(left, rightValues);
        } else if (rightValues.containsKey(right)) {
            return true;
        }

        rightValues.put(right, Boolean.TRUE);
        return false;
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
