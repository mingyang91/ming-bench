package ming;

import java.util.ArrayList;
import java.util.List;
import java.util.Set;

/**
 * Scheme interpreter entry point.
 * Agents implement this class.
 */
public class Evaluator {
    private final CollectionProcedures collectionProcedures;
    private final DynamicWindSupport dynamicWindSupport;
    private final ContinuationValueSupport continuationValueSupport =
            new ContinuationValueSupport();
    private final ValueSupport valueSupport = new ValueSupport();
    private final NumericProcedures numericProcedures = new NumericProcedures(valueSupport);
    private final Environment globalEnv;
    private ExceptionHandlerFrame currentExceptionHandler;
    private StringBuilder activeOutput;
    private Environment activeTransformerDefinitionEnv;
    private StepBudget activeStepBudget;
    private long syntheticCounter;
    private final RecordProcedureSupport recordProcedureSupport;

    public Evaluator() {
        collectionProcedures = new CollectionProcedures(this);
        dynamicWindSupport = new DynamicWindSupport(this::invokeThunk);
        recordProcedureSupport = new EvaluatorRecordProcedureSupport(this);
        globalEnv = GlobalEnvironmentFactory.create(this);
    }

    /**
     * Evaluate one or more Scheme expressions and return the string
     * representation of the last result.
     */
    public String evalStr(String input) throws EvalError {
        return renderResult(evalProgram(input, null));
    }

    public String evalStrWithLimit(String input, long maxSteps) throws EvalError {
        StepBudget previousStepBudget = activeStepBudget;
        activeStepBudget = StepBudget.limited(maxSteps);
        try {
            return renderResult(evalProgram(input, null));
        } finally {
            activeStepBudget = previousStepBudget;
        }
    }

    private Value evalProgram(String input, StringBuilder output) throws EvalError {
        StringBuilder previousOutput = activeOutput;
        WindFrame previousWind = dynamicWindSupport.currentWind();
        ExceptionHandlerFrame previousExceptionHandler = currentExceptionHandler;
        activeOutput = output;
        dynamicWindSupport.restore(null);
        currentExceptionHandler = null;

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
            dynamicWindSupport.restore(previousWind);
            currentExceptionHandler = previousExceptionHandler;
        }
    }

    /**
     * Evaluate Scheme expressions and return both the result string
     * and any captured output from display/write/newline.
     */
    public EvalResult evalStrWithOutput(String input) throws EvalError {
        StringBuilder output = new StringBuilder();
        Value result = evalProgram(input, output);
        return new EvalResult(renderResult(result), output.toString());
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

    private String renderResult(Value value) throws EvalError {
        if (value instanceof MultiValueValue multiValue) {
            throw new EvalError("top-level expression returned " + multiValue.values().size()
                    + " values");
        }
        return value.render();
    }

    private Value run(BounceFactory factory) throws EvalError {
        Value[] result = new Value[1];
        Bounce current = factory.create(values -> {
            result[0] = continuationValueSupport.pack(values);
            return null;
        });

        while (current != null) {
            current = current.run();
        }
        return result[0];
    }

    Bounce deliver(Continuation cont, Value value) {
        return deliverValues(cont, List.of(value));
    }

    private Bounce deliverValues(Continuation cont, List<Value> values) {
        return () -> cont.resume(values);
    }

    private Bounce deliverValues(ValueListContinuation cont, List<Value> values) {
        return () -> cont.resume(values);
    }

    private Bounce invokeThunk(Value thunk, Continuation cont) throws EvalError {
        return applyProcedureCps(thunk, List.of(), cont);
    }

    Bounce withPosition(SourcePos position, Bounce bounce) {
        return () -> {
            try {
                return bounce.run();
            } catch (EvalError error) {
                throw error.withPosition(position.line(), position.column());
            }
        };
    }

    Continuation positionedCont(SourcePos position, SingleValueContinuation cont) {
        return values -> withPosition(position,
                () -> cont.resume(continuationValueSupport.requireSingle(values)));
    }

    private ValueListContinuation positionedValues(SourcePos position, ValueListContinuation cont) {
        return values -> withPosition(position, () -> cont.resume(values));
    }

    private Value eval(Expr expr, Environment env) throws EvalError {
        return run(halt -> evalExpr(expr, env, halt));
    }

    Bounce evalExpr(Expr expr, Environment env, Continuation cont) {
        return withPosition(expr.position(), () -> {
            if (activeStepBudget != null) {
                activeStepBudget.consume();
            }
            return switch (expr) {
            case IntExpr intExpr -> deliver(cont, new IntValue(intExpr.value()));
            case RationalExpr rationalExpr -> deliver(cont, NumericSupport.exactToValue(
                    new ExactFraction(rationalExpr.numerator(), rationalExpr.denominator())));
            case InexactExpr inexactExpr -> deliver(cont, new InexactValue(inexactExpr.value()));
            case BoolExpr boolExpr -> deliver(cont, BoolValue.of(boolExpr.value()));
            case StringExpr stringExpr -> deliver(cont, new StringValue(stringExpr.value()));
            case CharExpr charExpr -> deliver(cont, new CharValue(charExpr.value()));
            case SymbolExpr symbolExpr -> deliver(cont, env.lookup(symbolExpr.name()));
            case ListExpr listExpr -> evalList(listExpr, env, cont);
            };
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
                case "quasiquote" -> evalQuasiquote(argExprs, env, cont);
                case "syntax" -> evalSyntax(argExprs, env, cont);
                case "syntax-case" -> evalSyntaxCase(position, argExprs, env, cont);
                case "with-syntax" -> evalWithSyntax(position, argExprs, env, cont);
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
                case "guard" -> evalGuard(position, argExprs, env, cont);
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

    private Bounce evalQuasiquote(List<Expr> argExprs, Environment env, Continuation cont)
            throws EvalError {
        requireArity("quasiquote", argExprs.size(), 1);
        return deliver(cont, quasiquoteToValue(argExprs.getFirst(), env, 1));
    }

    private Bounce evalSyntax(List<Expr> argExprs, Environment env, Continuation cont)
            throws EvalError {
        requireArity("syntax", argExprs.size(), 1);

        Environment definitionEnv = activeTransformerDefinitionEnv == null
                ? env
                : activeTransformerDefinitionEnv;
        Expr expanded = SyntaxCaseSupport.expandTemplate(argExprs.getFirst(), env, definitionEnv,
                this::freshSyntheticName);
        return deliver(cont, new SyntaxValue(expanded));
    }

    private Bounce evalSyntaxCase(SourcePos position, List<Expr> argExprs, Environment env,
                                  Continuation cont) throws EvalError {
        if (argExprs.size() < 3) {
            throw new EvalError(
                    "syntax-case requires an input, literals, and at least one clause");
        }

        Set<String> literals = SyntaxCaseSupport.parseLiteralIdentifiers(argExprs.get(1));
        return evalExpr(argExprs.getFirst(), env, positionedCont(position, inputValue ->
                evalSyntaxCaseClauses(position, expectSyntaxValue(inputValue).expr(), literals,
                        argExprs.subList(2, argExprs.size()), 0, env, cont)));
    }

    private Bounce evalSyntaxCaseClauses(SourcePos position, Expr inputExpr, Set<String> literals,
                                         List<Expr> clauseExprs, int index, Environment env,
                                         Continuation cont) throws EvalError {
        if (index >= clauseExprs.size()) {
            throw new EvalError("syntax-case: no matching clause");
        }

        Expr clauseExpr = clauseExprs.get(index);
        if (!(clauseExpr instanceof ListExpr clauseList)) {
            throw new EvalError("syntax-case clause must be a list");
        }

        List<Expr> clauseParts = clauseList.elements();
        if (clauseParts.size() < 2 || clauseParts.size() > 3) {
            throw new EvalError(
                    "syntax-case clause must contain a pattern, optional fender, and expression");
        }

        SyntaxCaseSupport.SyntaxMatch match = SyntaxCaseSupport.match(clauseParts.getFirst(),
                inputExpr, literals);
        if (match == null) {
            return evalSyntaxCaseClauses(position, inputExpr, literals, clauseExprs, index + 1,
                    env, cont);
        }

        Environment clauseEnv = new Environment(env);
        SyntaxCaseSupport.bindPatternVariables(clauseEnv, match);

        if (clauseParts.size() == 2) {
            return evalExpr(clauseParts.get(1), clauseEnv, cont);
        }

        return evalExpr(clauseParts.get(1), clauseEnv, positionedCont(position, testValue -> {
            if (isTruthy(testValue)) {
                return evalExpr(clauseParts.get(2), clauseEnv, cont);
            }
            return evalSyntaxCaseClauses(position, inputExpr, literals, clauseExprs, index + 1,
                    env, cont);
        }));
    }

    private Bounce evalWithSyntax(SourcePos position, List<Expr> argExprs, Environment env,
                                  Continuation cont) throws EvalError {
        if (argExprs.isEmpty()) {
            throw new EvalError("with-syntax requires bindings and a body");
        }
        if (!(argExprs.getFirst() instanceof ListExpr bindingList)) {
            throw new EvalError("with-syntax bindings must be a list");
        }

        List<Expr> body = FormParser.parseBody("with-syntax", argExprs.subList(1, argExprs.size()));
        return evalWithSyntaxBindings(position, bindingList.elements(), 0, env, body, cont);
    }

    private Bounce evalWithSyntaxBindings(SourcePos position, List<Expr> bindingExprs, int index,
                                          Environment env, List<Expr> body, Continuation cont)
            throws EvalError {
        if (index >= bindingExprs.size()) {
            return evalSequenceBounce(body, env, cont);
        }

        Expr bindingExpr = bindingExprs.get(index);
        if (!(bindingExpr instanceof ListExpr bindingList)) {
            throw new EvalError("with-syntax binding must be a list");
        }

        List<Expr> bindingParts = bindingList.elements();
        if (bindingParts.size() != 2) {
            throw new EvalError("with-syntax binding must contain a pattern and value");
        }

        Expr pattern = bindingParts.getFirst();
        Expr valueExpr = bindingParts.get(1);
        return evalExpr(valueExpr, env, positionedCont(position, value -> {
            SyntaxCaseSupport.SyntaxMatch match = SyntaxCaseSupport.match(pattern,
                    expectSyntaxValue(value).expr(), Set.of());
            if (match == null) {
                throw new EvalError("with-syntax pattern did not match");
            }

            Environment nextEnv = new Environment(env);
            SyntaxCaseSupport.bindPatternVariables(nextEnv, match);
            return evalWithSyntaxBindings(position, bindingExprs, index + 1, nextEnv, body, cont);
        }));
    }

    private Bounce evalDefineSyntax(List<Expr> argExprs, Environment env, Continuation cont)
            throws EvalError {
        requireArity("define-syntax", argExprs.size(), 2);

        if (!(argExprs.getFirst() instanceof SymbolExpr nameExpr)) {
            throw new EvalError("define-syntax name must be a symbol");
        }

        Expr transformerExpr = argExprs.get(1);
        if (transformerExpr instanceof ListExpr syntaxRulesExpr
                && !syntaxRulesExpr.elements().isEmpty()
                && syntaxRulesExpr.elements().getFirst() instanceof SymbolExpr head
                && head.name().equals("syntax-rules")) {
            env.defineSyntax(nameExpr.name(),
                    SyntaxRulesMacro.compile(nameExpr.name(), transformerExpr, env,
                            this::freshSyntheticName));
            return deliver(cont, VoidValue.INSTANCE);
        }

        return evalExpr(transformerExpr, env, positionedCont(transformerExpr.position(), value -> {
            if (!(value instanceof ProcedureValue procedureValue)) {
                throw new EvalError(
                        "define-syntax transformer must be a syntax-rules form or procedure");
            }
            env.defineSyntax(nameExpr.name(),
                    new TransformerProcedureMacro(this, procedureValue, env));
            return deliver(cont, VoidValue.INSTANCE);
        }));
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
                        continuationValueSupport.append(values, value), valueEnv, position,
                        cont)));
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
                        continuationValueSupport.append(values, value), env, position, cont)));
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
                    continuationValueSupport.append(nextValues, bindingCells.get(index).value()),
                    loopEnv, bindingCells, terminationParts, body, cont);
        }

        return evalExpr(stepExpr, loopEnv, positionedCont(position,
                value -> evalDoStepValues(position, bindings, index + 1,
                        continuationValueSupport.append(nextValues, value), loopEnv, bindingCells,
                        terminationParts, body, cont)));
    }

    private Bounce evalGuard(SourcePos position, List<Expr> argExprs, Environment env,
                             Continuation cont) throws EvalError {
        if (argExprs.size() < 2) {
            throw new EvalError("guard requires a clause list and a body");
        }
        if (!(argExprs.getFirst() instanceof ListExpr guardSpec)) {
            throw new EvalError("guard requires a clause list");
        }

        List<Expr> guardParts = guardSpec.elements();
        if (guardParts.isEmpty()) {
            throw new EvalError("guard clause list cannot be empty");
        }
        if (!(guardParts.getFirst() instanceof SymbolExpr variableExpr)) {
            throw new EvalError("guard variable must be a symbol");
        }

        ExceptionHandlerFrame previousHandler = currentExceptionHandler;
        ExceptionHandlerFrame guardHandler = new ExceptionHandlerFrame(
                previousHandler,
                exceptionValue -> evalGuardClauses(position,
                        variableExpr.name(),
                        guardParts.subList(1, guardParts.size()),
                        exceptionValue,
                        env,
                        cont),
                dynamicWindSupport.currentWind()
        );
        currentExceptionHandler = guardHandler;

        try {
            return evalSequenceBounce(argExprs.subList(1, argExprs.size()), env, values -> {
                currentExceptionHandler = previousHandler;
                return deliverValues(cont, values);
            });
        } catch (EvalError error) {
            currentExceptionHandler = previousHandler;
            throw error;
        }
    }

    private Bounce evalGuardClauses(SourcePos position, String variableName, List<Expr> clauses,
                                    Value exceptionValue, Environment env, Continuation cont)
            throws EvalError {
        Environment guardEnv = new Environment(env);
        guardEnv.define(variableName, exceptionValue);
        return evalGuardClause(position, clauses, 0, exceptionValue, guardEnv, cont);
    }

    private Bounce evalGuardClause(SourcePos position, List<Expr> clauses, int index,
                                   Value exceptionValue, Environment guardEnv,
                                   Continuation cont) throws EvalError {
        if (index >= clauses.size()) {
            return signalException(exceptionValue);
        }

        Expr clauseExpr = clauses.get(index);
        if (!(clauseExpr instanceof ListExpr clauseList)) {
            throw new EvalError("guard clause must be a list");
        }

        List<Expr> clause = clauseList.elements();
        if (clause.isEmpty()) {
            throw new EvalError("guard clause cannot be empty");
        }

        Expr testExpr = clause.getFirst();
        if (testExpr instanceof SymbolExpr symbolExpr && symbolExpr.name().equals("else")) {
            if (index != clauses.size() - 1) {
                throw new EvalError("guard else clause must be last");
            }
            return evalClauseBody("guard", clause.subList(1, clause.size()), guardEnv, null,
                    cont);
        }

        return evalExpr(testExpr, guardEnv, positionedCont(position, testValue -> {
            if (isTruthy(testValue)) {
                return evalClauseBody("guard", clause.subList(1, clause.size()), guardEnv,
                        testValue, cont);
            }
            return evalGuardClause(position, clauses, index + 1, exceptionValue, guardEnv, cont);
        }));
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
        if (isCondArrowClause(clause) && clause.size() != 3) {
            throw new EvalError("cond => clause requires exactly one recipient expression");
        }

        return evalExpr(testExpr, env, positionedCont(position, testValue -> {
            if (isTruthy(testValue)) {
                if (isCondArrowClause(clause)) {
                    Expr recipientExpr = clause.get(2);
                    return evalExpr(recipientExpr, env,
                            positionedCont(recipientExpr.position(),
                                    recipient -> applyProcedureCps(recipient,
                                            List.of(testValue), cont)));
                }
                return evalClauseBody("cond", clause.subList(1, clause.size()), env, testValue,
                        cont);
            }
            return evalCond(position, clauses, index + 1, env, cont);
        }));
    }

    private boolean isCondArrowClause(List<Expr> clause) {
        return clause.size() >= 2
                && clause.get(1) instanceof SymbolExpr symbolExpr
                && symbolExpr.name().equals("=>");
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

        Expr expr = argExprs.get(index);
        return evalExpr(expr, env, positionedCont(expr.position(), value -> {
            if (!isTruthy(value)) {
                return deliver(cont, value);
            }
            return evalAnd(argExprs, index + 1, env, cont);
        }));
    }

    private Bounce evalOr(List<Expr> argExprs, int index, Environment env, Continuation cont)
            throws EvalError {
        if (argExprs.isEmpty()) {
            return deliver(cont, BoolValue.FALSE);
        }
        if (index == argExprs.size() - 1) {
            return evalExpr(argExprs.get(index), env, cont);
        }

        Expr expr = argExprs.get(index);
        return evalExpr(expr, env, positionedCont(expr.position(), value -> {
            if (isTruthy(value)) {
                return deliver(cont, value);
            }
            return evalOr(argExprs, index + 1, env, cont);
        }));
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
                        continuationValueSupport.prepend(value, values), env, position, cont)));
    }

    private Value evalSequence(List<Expr> exprs, Environment env) throws EvalError {
        return run(halt -> evalSequenceBounce(exprs, env, halt));
    }

    private Value evalSequenceOrVoid(List<Expr> exprs, Environment env) throws EvalError {
        return run(halt -> evalSequenceBounce(exprs, env, halt));
    }

    private Value quasiquoteToValue(Expr expr, Environment env, int depth) throws EvalError {
        if (expr instanceof ListExpr listExpr) {
            List<Expr> elements = listExpr.elements();
            if (matchesQuasiquoteForm(elements, "unquote")) {
                return evalNestedQuasiquoteForm("unquote", elements, env, depth - 1);
            }
            if (matchesQuasiquoteForm(elements, "unquote-splicing")) {
                if (depth == 1) {
                    throw new EvalError("unquote-splicing must appear within a list");
                }
                return evalNestedQuasiquoteForm("unquote-splicing", elements, env, depth - 1);
            }
            if (matchesQuasiquoteForm(elements, "quasiquote")) {
                return evalNestedQuasiquoteForm("quasiquote", elements, env, depth + 1);
            }
            return quasiquoteListToValue(elements, env, depth);
        }
        return quoteToValue(expr);
    }

    private Value evalNestedQuasiquoteForm(String name, List<Expr> elements, Environment env,
                                           int nestedDepth) throws EvalError {
        requireQuasiquoteFormArity(name, elements);
        if (name.equals("unquote") && nestedDepth == 0) {
            return evalSingleValue(elements.get(1), env);
        }
        Value argument = quasiquoteToValue(elements.get(1), env, nestedDepth);
        return new PairValue(new SymbolValue(name),
                new PairValue(argument, EmptyListValue.INSTANCE));
    }

    private Value quasiquoteListToValue(List<Expr> elements, Environment env, int depth)
            throws EvalError {
        int dotIndex = dottedTailIndex(elements);
        int prefixEnd = dotIndex >= 0 ? dotIndex : elements.size();
        Value result = dotIndex >= 0
                ? quasiquoteToValue(elements.get(dotIndex + 1), env, depth)
                : EmptyListValue.INSTANCE;

        for (int index = prefixEnd - 1; index >= 0; index--) {
            Expr element = elements.get(index);
            if (depth == 1 && element instanceof ListExpr spliceExpr
                    && matchesQuasiquoteForm(spliceExpr.elements(), "unquote-splicing")) {
                requireQuasiquoteFormArity("unquote-splicing", spliceExpr.elements());
                List<Value> splicedValues = collectionProcedures.listElements(
                        evalSingleValue(spliceExpr.elements().get(1), env));
                for (int spliceIndex = splicedValues.size() - 1; spliceIndex >= 0; spliceIndex--) {
                    result = new PairValue(splicedValues.get(spliceIndex), result);
                }
                continue;
            }
            result = new PairValue(quasiquoteToValue(element, env, depth), result);
        }

        return result;
    }

    private Value evalSingleValue(Expr expr, Environment env) throws EvalError {
        Value value = eval(expr, env);
        if (value instanceof MultiValueValue multiValue) {
            throw new EvalError("expected single value, got " + multiValue.values().size());
        }
        return value;
    }

    private boolean matchesQuasiquoteForm(List<Expr> elements, String name) {
        return elements.size() == 2
                && elements.getFirst() instanceof SymbolExpr symbolExpr
                && symbolExpr.name().equals(name);
    }

    private void requireQuasiquoteFormArity(String name, List<Expr> elements) throws EvalError {
        if (elements.size() != 2) {
            throw new EvalError(name + " requires exactly one argument");
        }
    }

    private int dottedTailIndex(List<Expr> elements) throws EvalError {
        int dotIndex = -1;
        for (int index = 0; index < elements.size(); index++) {
            if (!(elements.get(index) instanceof SymbolExpr symbolExpr)
                    || !symbolExpr.name().equals(".")) {
                continue;
            }
            if (dotIndex >= 0 || index == 0 || index != elements.size() - 2) {
                throw new EvalError("invalid dotted list");
            }
            dotIndex = index;
        }
        return dotIndex;
    }

    private Bounce evalSequenceBounce(List<Expr> exprs, Environment env, Continuation cont)
            throws EvalError {
        return SequenceEvaluator.evaluate(this, exprs, env, cont);
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

    Value applyTransformerProcedure(ProcedureValue transformer, SyntaxValue input,
                                    Environment definitionEnv) throws EvalError {
        Environment previousDefinitionEnv = activeTransformerDefinitionEnv;
        activeTransformerDefinitionEnv = definitionEnv;
        try {
            return applyProcedure(transformer, List.of(input));
        } finally {
            activeTransformerDefinitionEnv = previousDefinitionEnv;
        }
    }

    private Bounce applyProcedureCps(Value procedureValue, List<Value> argumentValues,
                                     Continuation cont) throws EvalError {
        if (!(procedureValue instanceof ProcedureValue procedure)) {
            throw new EvalError("not a procedure");
        }

        if (procedure instanceof ContinuationProcedure continuationProcedure) {
            return dynamicWindSupport.transferTo(continuationProcedure.windContext(), () -> {
                currentExceptionHandler = continuationProcedure.exceptionHandlerContext();
                return deliverValues(continuationProcedure.continuation(), argumentValues);
            });
        }

        if (procedure instanceof CallCcProcedure callCcProcedure) {
            requireArity(callCcProcedure.name(), argumentValues.size(), 1);
            return applyProcedureCps(argumentValues.getFirst(),
                    List.of(new ContinuationProcedure(cont, dynamicWindSupport.currentWind(),
                            currentExceptionHandler)),
                    cont);
        }

        if (procedure instanceof ValuesProcedure) {
            return deliverValues(cont, argumentValues);
        }

        if (procedure instanceof CallWithValuesProcedure callWithValuesProcedure) {
            requireArity(callWithValuesProcedure.name(), argumentValues.size(), 2);
            Value producer = argumentValues.get(0);
            Value consumer = argumentValues.get(1);
            return applyProcedureCps(producer, List.of(),
                    producedValues -> applyProcedureCps(consumer, producedValues, cont));
        }

        if (procedure instanceof DynamicWindProcedure dynamicWindProcedure) {
            requireArity(dynamicWindProcedure.name(), argumentValues.size(), 3);
            return dynamicWindSupport.apply(argumentValues.get(0), argumentValues.get(1),
                    argumentValues.get(2), cont);
        }

        if (procedure instanceof RaiseProcedure raiseProcedure) {
            requireArity(raiseProcedure.name(), argumentValues.size(), 1);
            return signalException(argumentValues.getFirst());
        }

        if (procedure instanceof WithExceptionHandlerProcedure withExceptionHandlerProcedure) {
            requireArity(withExceptionHandlerProcedure.name(), argumentValues.size(), 2);
            return applyWithExceptionHandler(argumentValues.get(0), argumentValues.get(1), cont);
        }

        if (procedure instanceof UserProcedure userProcedure) {
            return applyUserProcedureCps(userProcedure.displayName(), userProcedure.parameters(),
                    userProcedure.body(), userProcedure.closureEnv(), argumentValues, cont);
        }

        if (procedure instanceof CaseLambdaProcedure caseLambdaProcedure) {
            return applyCaseLambdaProcedureCps(caseLambdaProcedure, argumentValues, cont);
        }

        Value result = procedure.apply(argumentValues);
        if (result instanceof MultiValueValue multiValue) {
            return deliverValues(cont, multiValue.values());
        }
        return deliver(cont, result);
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

    private Bounce applyWithExceptionHandler(Value handlerProcedure, Value thunk,
                                             Continuation cont) throws EvalError {
        ExceptionHandlerFrame previousHandler = currentExceptionHandler;
        ExceptionHandlerFrame handlerFrame = new ExceptionHandlerFrame(
                previousHandler,
                exceptionValue -> applyProcedureCps(handlerProcedure,
                        List.of(exceptionValue),
                        ignoredValues -> signalException(exceptionValue)),
                dynamicWindSupport.currentWind()
        );
        currentExceptionHandler = handlerFrame;

        try {
            return applyProcedureCps(thunk, List.of(), values -> {
                currentExceptionHandler = previousHandler;
                return deliverValues(cont, values);
            });
        } catch (EvalError error) {
            currentExceptionHandler = previousHandler;
            throw error;
        }
    }

    private Bounce signalException(Value exceptionValue) throws EvalError {
        ExceptionHandlerFrame handlerFrame = currentExceptionHandler;
        if (handlerFrame == null) {
            throw new EvalError("uncaught exception: " + exceptionValue.render());
        }

        currentExceptionHandler = handlerFrame.parent();
        return dynamicWindSupport.transferTo(handlerFrame.windContext(),
                () -> handlerFrame.action().handle(exceptionValue));
    }

    private Value quoteToValue(Expr expr) throws EvalError {
        return valueSupport.quoteToValue(expr);
    }

    Value addNumbers(List<Value> args) throws EvalError {
        return numericProcedures.addNumbers(args);
    }

    Value subtractNumbers(List<Value> args) throws EvalError {
        return numericProcedures.subtractNumbers(args);
    }

    Value multiplyNumbers(List<Value> args) throws EvalError {
        return numericProcedures.multiplyNumbers(args);
    }

    Value divideNumbers(List<Value> args) throws EvalError {
        return numericProcedures.divideNumbers(args);
    }

    Value absBuiltin(List<Value> args) throws EvalError {
        return numericProcedures.absBuiltin(args);
    }

    int quotient(List<Value> args) throws EvalError {
        return numericProcedures.quotient(args);
    }

    int remainder(List<Value> args) throws EvalError {
        return numericProcedures.remainder(args);
    }

    int modulo(List<Value> args) throws EvalError {
        return numericProcedures.modulo(args);
    }

    Value minBuiltin(List<Value> args) throws EvalError {
        return numericProcedures.minBuiltin(args);
    }

    Value maxBuiltin(List<Value> args) throws EvalError {
        return numericProcedures.maxBuiltin(args);
    }

    int expt(List<Value> args) throws EvalError {
        return numericProcedures.expt(args);
    }

    boolean compareIncreasing(List<Value> args, Comparison comparison) throws EvalError {
        return numericProcedures.compareIncreasing(args, comparison);
    }

    Value expectNumber(Value value) throws EvalError {
        return valueSupport.expectNumber(value);
    }

    int expectInt(Value value) throws EvalError {
        return valueSupport.expectInt(value);
    }

    int expectIndex(Value value, String operationName) throws EvalError {
        return valueSupport.expectIndex(value, operationName);
    }

    String expectString(Value value) throws EvalError {
        return valueSupport.expectString(value);
    }

    StringValue expectStringValue(Value value) throws EvalError {
        return valueSupport.expectStringValue(value);
    }

    char expectChar(Value value) throws EvalError {
        return valueSupport.expectChar(value);
    }

    String expectSymbol(Value value) throws EvalError {
        return valueSupport.expectSymbol(value);
    }

    SyntaxValue expectSyntaxValue(Value value) throws EvalError {
        return valueSupport.expectSyntax(value);
    }

    PairValue expectPair(Value value) throws EvalError {
        return valueSupport.expectPair(value);
    }

    VectorValue expectVectorValue(Value value) throws EvalError {
        return valueSupport.expectVectorValue(value);
    }

    RecordValue expectRecord(Value value, RecordType recordType) throws EvalError {
        return valueSupport.expectRecord(value, recordType);
    }

    Value gcdBuiltin(List<Value> args) throws EvalError {
        return numericProcedures.gcdBuiltin(args);
    }

    Value lcmBuiltin(List<Value> args) throws EvalError {
        return numericProcedures.lcmBuiltin(args);
    }

    Value truncateBuiltin(List<Value> args) throws EvalError {
        return numericProcedures.truncateBuiltin(args);
    }

    Value roundBuiltin(List<Value> args) throws EvalError {
        return numericProcedures.roundBuiltin(args);
    }

    String stringAppend(List<Value> args) throws EvalError {
        return valueSupport.stringAppend(args);
    }

    Value syntaxToDatum(Value value) throws EvalError {
        return valueSupport.syntaxToDatum(value);
    }

    Expr datumToExpr(Value value, SourcePos position) throws EvalError {
        return valueSupport.datumToExpr(value, position);
    }

    Value stringToNumber(String token) {
        return numericProcedures.stringToNumber(token);
    }

    Value exactToInexact(Value value) throws EvalError {
        return numericProcedures.exactToInexact(value);
    }

    Value numeratorBuiltin(List<Value> args) throws EvalError {
        return numericProcedures.numeratorBuiltin(args);
    }

    Value denominatorBuiltin(List<Value> args) throws EvalError {
        return numericProcedures.denominatorBuiltin(args);
    }

    BoolValue signPredicate(String name, List<Value> args, int expectedSign) throws EvalError {
        return numericProcedures.signPredicate(name, args, expectedSign);
    }

    boolean compareChars(List<Value> args, String name, CharComparison comparison)
            throws EvalError {
        return valueSupport.compareChars(args, name, comparison);
    }

    boolean compareStrings(List<Value> args, String name, StringComparison comparison)
            throws EvalError {
        return valueSupport.compareStrings(args, name, comparison);
    }

    boolean isEq(Value left, Value right) throws EvalError {
        return valueSupport.isEq(left, right);
    }

    boolean isEqv(Value left, Value right) throws EvalError {
        return valueSupport.isEqv(left, right);
    }

    boolean isEqual(Value left, Value right) throws EvalError {
        return valueSupport.isEqual(left, right);
    }

    Value errorBuiltin(List<Value> args) throws EvalError {
        return valueSupport.errorBuiltin(args);
    }

    BoolValue typePredicate(String name, List<Value> args, ValuePredicate predicate)
            throws EvalError {
        return valueSupport.typePredicate(name, args, predicate);
    }

    void requireArity(String name, int actual, int expected) throws EvalError {
        valueSupport.requireArity(name, actual, expected);
    }

    void requireAtLeast(String name, int actual, int minimum) throws EvalError {
        valueSupport.requireAtLeast(name, actual, minimum);
    }

    boolean isTruthy(Value value) {
        return valueSupport.isTruthy(value);
    }

}
