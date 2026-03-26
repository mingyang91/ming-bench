package ming;

import static ming.EvaluatorSupport.*;
import static ming.RuntimeConstants.*;
import static ming.ValueSupport.*;

import java.util.ArrayList;
import java.util.List;
import java.util.Optional;

/**
 * Scheme interpreter entry point.
 * Agents implement this class.
 */
public class Evaluator {
    private static final int STRING_IMMUTABILITY_LEVEL = 15;

    private final Env globalEnv;
    private final MacroExpander macroExpander;
    private final boolean immutableStringsEnabled;
    private List<DynamicWindFrame> dynamicWindStack;
    private ExceptionHandlerFrame exceptionHandlerStack;
    private StringBuilder outputBuffer;

    @FunctionalInterface
    private interface ValueListHandler {
        Step apply(List<Value> values) throws EvalError;
    }

    private static final Kont DONE = new Kont(0, 0, DoneStep::new);

    public Evaluator() {
        this.immutableStringsEnabled = currentBenchLevel() >= STRING_IMMUTABILITY_LEVEL;
        this.globalEnv = createGlobalEnv();
        this.macroExpander = new MacroExpander(this::applyProcedure);
        this.dynamicWindStack = new ArrayList<>();
        this.exceptionHandlerStack = null;
    }

    /**
     * Evaluate one or more Scheme expressions and return the string
     * representation of the last result.
     */
    public String evalStr(String input) throws EvalError {
        return evalProgram(input, false).result();
    }

    /**
     * Evaluate Scheme expressions and return both the result string
     * and any captured output from display/write/newline.
     */
    public EvalResult evalStrWithOutput(String input) throws EvalError {
        return evalProgram(input, true);
    }

    private static int currentBenchLevel() {
        String level = System.getProperty("bench.level", "");
        if (level == null || level.isEmpty()) {
            level = System.getenv("BENCH_LEVEL");
        }
        if (level == null || level.isEmpty()) {
            return Integer.MAX_VALUE;
        }

        try {
            return Integer.parseInt(level);
        } catch (NumberFormatException ignored) {
            return Integer.MAX_VALUE;
        }
    }

    private EvalResult evalProgram(String input, boolean captureOutput) throws EvalError {
        List<Expr> expressions = new Parser(input).parseProgram();
        if (expressions.isEmpty()) {
            throw new EvalError("input did not contain any expressions");
        }

        StringBuilder previousOutput = outputBuffer;
        outputBuffer = captureOutput ? new StringBuilder() : null;
        try {
            Value result = run(evalSequence(expressions, globalEnv, DONE));
            String output = outputBuffer == null ? "" : outputBuffer.toString();
            return new EvalResult(ValueRenderer.render(result), output);
        } finally {
            outputBuffer = previousOutput;
        }
    }

    private Env createGlobalEnv() {
        return EvaluatorBuiltins.createGlobalEnv(
                immutableStringsEnabled,
                this::applyProcedure,
                this::appendOutput,
                QuotationSupport::quoteToValue);
    }

    private Value run(Step initialStep) throws EvalError {
        Step currentStep = initialStep;
        while (true) {
            try {
                switch (currentStep) {
                    case EvalExprStep evalStep -> {
                        try {
                            currentStep = evalExpr(evalStep.expr(), evalStep.env(), evalStep.kont());
                        } catch (EvalError error) {
                            throw error.withPosition(evalStep.expr().line(), evalStep.expr().column());
                        }
                    }
                    case ReturnStep returnStep -> currentStep = returnStep.kont().apply(
                            returnStep.value());
                    case ApplyStep applyStep -> {
                        try {
                            currentStep = applyProcedureStep(
                                    applyStep.operator(),
                                    applyStep.arguments(),
                                    applyStep.kont(),
                                    applyStep.line(),
                                    applyStep.column());
                        } catch (EvalError error) {
                            if (applyStep.line() <= 0 || applyStep.column() <= 0) {
                                throw error;
                            }
                            throw error.withPosition(applyStep.line(), applyStep.column());
                        }
                    }
                    case ThunkStep thunkStep -> currentStep = thunkStep.supplier().get();
                    case DoneStep doneStep -> {
                        return doneStep.value();
                    }
                }
            } catch (ContinuationJump jump) {
                currentStep = jumpToContinuation(jump.target(), jump.value());
            } catch (RaisedException raised) {
                currentStep = handleRaisedException(raised);
            }
        }
    }

    private Value eval(Expr expr, Env env) throws EvalError {
        return run(new EvalExprStep(expr, env, DONE));
    }

    private Step evalExpr(Expr expr, Env env, Kont kont) throws EvalError {
        return switch (expr) {
            case IntExpr intExpr -> new ReturnStep(new IntValue(intExpr.value()), kont);
            case RationalExpr rationalExpr ->
                    new ReturnStep(
                            exactValue(rationalExpr.numerator(), rationalExpr.denominator()),
                            kont);
            case InexactExpr inexactExpr -> new ReturnStep(
                    new InexactValue(inexactExpr.value()), kont);
            case BoolExpr boolExpr -> new ReturnStep(boolValue(boolExpr.value()), kont);
            case CharExpr charExpr -> new ReturnStep(new CharValue(charExpr.value()), kont);
            case StringExpr stringExpr -> new ReturnStep(immutableString(stringExpr.value()), kont);
            case SymbolExpr symbolExpr -> new ReturnStep(
                    macroExpander.lookupSymbol(symbolExpr.name(), env), kont);
            case VectorExpr vectorExpr -> new ReturnStep(QuotationSupport.quoteToValue(vectorExpr),
                    kont);
            case ListExpr listExpr -> evalList(listExpr, env, kont);
        };
    }

    private Step evalList(ListExpr listExpr, Env env, Kont kont) throws EvalError {
        List<Expr> elements = listExpr.elements();
        if (elements.isEmpty()) {
            throw new EvalError("cannot evaluate an empty list");
        }

        Expr operatorExpr = elements.get(0);
        List<Expr> arguments = elements.subList(1, elements.size());
        String operatorName = symbolName(operatorExpr);
        if (operatorName != null) {
            if ("define-syntax".equals(operatorName)) {
                macroExpander.defineSyntax(arguments, env);
                return new ReturnStep(VOID, kont);
            }

            Optional<Expr> expandedMacro = macroExpander.expandInvocation(operatorName, listExpr);
            if (expandedMacro.isPresent()) {
                return new EvalExprStep(expandedMacro.get(), env, kont);
            }

            return switch (operatorName) {
                case "define" -> evalDefine(arguments, env, kont, listExpr);
                case "define-record-type" ->
                        new ReturnStep(RecordTypeSupport.evalDefineRecordType(arguments, env), kont);
                case "set!" -> evalSet(arguments, env, kont, listExpr);
                case "if" -> evalIf(arguments, env, kont, listExpr);
                case "quote" -> new ReturnStep(evalQuote(arguments), kont);
                case "quasiquote" -> new ReturnStep(evalQuasiquote(arguments, env), kont);
                case "lambda" -> new ReturnStep(evalLambda(arguments, env), kont);
                case "case-lambda" -> new ReturnStep(evalCaseLambda(arguments, env), kont);
                case "and" -> evalAnd(arguments, 0, env, kont, listExpr);
                case "or" -> evalOr(arguments, 0, env, kont, listExpr);
                case "begin" -> evalSequence(arguments, env, kont);
                case "guard" -> evalGuard(arguments, env, kont, listExpr);
                case "let" -> evalLet(arguments, env, kont, listExpr);
                case "let*" -> evalLetStar(arguments, env, kont, listExpr);
                case "letrec" -> evalLetRec(arguments, env, false, kont, listExpr);
                case "letrec*" -> evalLetRec(arguments, env, true, kont, listExpr);
                case "cond" -> evalCond(arguments, 0, env, kont, listExpr);
                case "case" -> evalCase(arguments, env, kont, listExpr);
                case "do" -> evalDo(arguments, env, kont, listExpr);
                case "unquote" -> throw new EvalError("unquote can only appear within quasiquote");
                case "unquote-splicing" ->
                        throw new EvalError("unquote-splicing can only appear within quasiquote");
                default -> evalApplication(listExpr, operatorExpr, arguments, env, kont);
            };
        }

        return evalApplication(listExpr, operatorExpr, arguments, env, kont);
    }

    private Step evalApplication(ListExpr context, Expr operatorExpr, List<Expr> arguments, Env env,
            Kont kont) {
        return new EvalExprStep(operatorExpr, env,
                continuation(context, operator -> evalApplicationArguments(
                        operator,
                        arguments,
                        arguments.size() - 1,
                        List.of(),
                        env,
                        kont,
                        context)));
    }

    private Step evalApplicationArguments(Value operator, List<Expr> arguments, int index,
            List<Value> suffixValues, Env env, Kont kont, ListExpr context) {
        if (index < 0) {
            return new ApplyStep(operator, suffixValues, kont, context.line(), context.column());
        }

        return new EvalExprStep(arguments.get(index), env,
                continuation(context, value -> evalApplicationArguments(
                        operator,
                        arguments,
                        index - 1,
                        prependValue(value, suffixValues),
                        env,
                        kont,
                        context)));
    }

    private Step evalDefine(List<Expr> arguments, Env env, Kont kont, Expr context)
            throws EvalError {
        if (arguments.size() < 2) {
            throw new EvalError("define expects a name and a value");
        }

        Expr target = arguments.get(0);
        if (target instanceof SymbolExpr symbolExpr) {
            if (arguments.size() != 2) {
                throw new EvalError("define expects exactly one value expression");
            }

            Cell binding = env.definePlaceholder(symbolExpr.name());
            return new EvalExprStep(arguments.get(1), env,
                    continuation(context, value -> {
                        binding.set(value);
                        return new ReturnStep(VOID, kont);
                    }));
        }

        if (target instanceof ListExpr signatureExpr) {
            List<Expr> signature = signatureExpr.elements();
            if (signature.isEmpty() || !(signature.get(0) instanceof SymbolExpr nameExpr)) {
                throw new EvalError("define function name must be a symbol");
            }

            if (arguments.size() < 2) {
                throw new EvalError("define requires a function body");
            }

            Cell binding = env.definePlaceholder(nameExpr.name());
            binding.set(new ClosureValue(
                    parseFormals(signature.subList(1, signature.size())),
                    List.copyOf(arguments.subList(1, arguments.size())),
                    env));
            return new ReturnStep(VOID, kont);
        }

        throw new EvalError("define target must be a symbol or parameter list");
    }

    private Step evalSet(List<Expr> arguments, Env env, Kont kont, Expr context) throws EvalError {
        requireExactArgs("set!", arguments, 2);

        Expr target = arguments.get(0);
        if (!(target instanceof SymbolExpr symbolExpr)) {
            throw new EvalError("set! target must be a symbol");
        }

        return new EvalExprStep(arguments.get(1), env,
                continuation(context, value -> {
                    macroExpander.setSymbol(symbolExpr.name(), value, env);
                    return new ReturnStep(VOID, kont);
                }));
    }

    private String symbolName(Expr expr) {
        if (expr instanceof SymbolExpr symbolExpr) {
            return symbolExpr.name();
        }
        return null;
    }

    private Step evalIf(List<Expr> arguments, Env env, Kont kont, Expr context) throws EvalError {
        if (arguments.size() < 2 || arguments.size() > 3) {
            throw new EvalError("if expected 2 or 3 argument(s)");
        }

        Expr alternate = arguments.size() == 3 ? arguments.get(2) : null;
        return new EvalExprStep(arguments.get(0), env,
                continuation(context, condition -> {
                    if (isTruthy(condition)) {
                        return new EvalExprStep(arguments.get(1), env, kont);
                    }
                    if (alternate != null) {
                        return new EvalExprStep(alternate, env, kont);
                    }
                    return new ReturnStep(VOID, kont);
                }));
    }

    private Value evalQuote(List<Expr> arguments) throws EvalError {
        return QuotationSupport.evalQuote(arguments);
    }

    private Value evalQuasiquote(List<Expr> arguments, Env env) throws EvalError {
        return QuotationSupport.evalQuasiquote(arguments, env, this::eval);
    }

    private Value evalLambda(List<Expr> arguments, Env env) throws EvalError {
        if (arguments.size() < 2) {
            throw new EvalError("lambda requires parameters and a body");
        }

        return new ClosureValue(
                parseFormals(arguments.get(0)),
                List.copyOf(arguments.subList(1, arguments.size())),
                env);
    }

    private Value evalCaseLambda(List<Expr> arguments, Env env) throws EvalError {
        if (arguments.isEmpty()) {
            throw new EvalError("case-lambda requires at least one clause");
        }

        List<ProcedureClause> clauses = new ArrayList<>(arguments.size());
        for (Expr argument : arguments) {
            if (!(argument instanceof ListExpr clauseExpr) || clauseExpr.elements().size() < 2) {
                throw new EvalError("case-lambda clause must include parameters and a body");
            }

            List<Expr> clauseElements = clauseExpr.elements();
            clauses.add(new ProcedureClause(
                    parseFormals(clauseElements.get(0)),
                    List.copyOf(clauseElements.subList(1, clauseElements.size()))));
        }
        return new CaseLambdaValue(List.copyOf(clauses), env);
    }

    private Formals parseFormals(Expr parameterExpr) throws EvalError {
        if (parameterExpr instanceof SymbolExpr symbolExpr) {
            return new Formals(List.of(), symbolExpr.name());
        }
        if (!(parameterExpr instanceof ListExpr parameterList)) {
            throw new EvalError("lambda parameters must be a list or symbol");
        }
        return parseFormals(parameterList.elements());
    }

    private Formals parseFormals(List<Expr> parameterExprs) throws EvalError {
        List<String> parameters = new ArrayList<>(parameterExprs.size());
        String restParameter = null;

        for (int index = 0; index < parameterExprs.size(); index++) {
            Expr parameterExpr = parameterExprs.get(index);
            if (parameterExpr instanceof SymbolExpr symbolExpr
                    && ".".equals(symbolExpr.name())) {
                if (index == parameterExprs.size() - 1) {
                    throw new EvalError("lambda rest parameter name is missing");
                }

                Expr restExpr = parameterExprs.get(index + 1);
                if (!(restExpr instanceof SymbolExpr restSymbol)
                        || ".".equals(restSymbol.name())) {
                    throw new EvalError("lambda rest parameter must be a symbol");
                }
                if (index + 2 != parameterExprs.size()) {
                    throw new EvalError("lambda rest parameter must be last");
                }
                restParameter = restSymbol.name();
                break;
            }
            if (!(parameterExpr instanceof SymbolExpr symbolExpr)) {
                throw new EvalError("lambda parameter must be a symbol");
            }
            parameters.add(symbolExpr.name());
        }
        return new Formals(List.copyOf(parameters), restParameter);
    }

    private Kont continuation(Expr context, ContinuationBody body) {
        return new Kont(context.line(), context.column(), body);
    }

    private Kont continuation(int line, int column, ContinuationBody body) {
        return new Kont(line, column, body);
    }

    private Kont continuation(ContinuationBody body) {
        return new Kont(0, 0, body);
    }

    private Step evalSequence(List<Expr> expressions, Env env, Kont kont) {
        return evalSequence(new SequenceState(expressions), 0, env, kont);
    }

    private Step evalSequence(SequenceState state, int index, Env env, Kont kont) {
        List<Expr> expressions = state.expressions();
        if (index >= expressions.size()) {
            return new ReturnStep(VOID, kont);
        }
        if (state.hasCachedCheckpoint(index)) {
            if (index == expressions.size() - 1) {
                return new ReturnStep(state.cachedCheckpointValue(index), kont);
            }
            return evalSequence(state, index + 1, env, kont);
        }
        if (index == expressions.size() - 1) {
            return evalSequenceExpression(state, index, env, kont);
        }

        return evalSequenceExpression(state, index, env,
                continuation(value -> evalSequence(state, index + 1, env, kont)));
    }

    private Step evalSequenceExpression(SequenceState state, int index, Env env, Kont kont) {
        Expr expression = state.expressions().get(index);
        if (!isSequenceCheckpointExpression(expression)) {
            return new EvalExprStep(expression, env, kont);
        }

        return new EvalExprStep(expression, env,
                continuation(value -> {
                    state.cacheCheckpoint(index, value);
                    return kont.apply(value);
                }));
    }

    private boolean isSequenceCheckpointExpression(Expr expression) {
        if (!(expression instanceof ListExpr listExpr) || listExpr.elements().isEmpty()) {
            return false;
        }

        String operatorName = symbolName(listExpr.elements().get(0));
        return "call/cc".equals(operatorName)
                || "call-with-current-continuation".equals(operatorName);
    }

    private List<Value> appendValue(List<Value> values, Value value) {
        List<Value> result = new ArrayList<>(values.size() + 1);
        result.addAll(values);
        result.add(value);
        return List.copyOf(result);
    }

    private List<Value> prependValue(Value value, List<Value> values) {
        List<Value> result = new ArrayList<>(values.size() + 1);
        result.add(value);
        result.addAll(values);
        return List.copyOf(result);
    }

    private Value packValues(List<Value> values) {
        if (values.size() == 1) {
            return values.get(0);
        }
        return new MultiValue(values);
    }

    private List<Value> unpackValues(Value value) {
        if (value instanceof MultiValue multiValue) {
            return multiValue.values();
        }
        return List.of(value);
    }

    private Step evalAnd(List<Expr> arguments, int index, Env env, Kont kont, Expr context) {
        if (index >= arguments.size()) {
            return new ReturnStep(TRUE, kont);
        }

        return new EvalExprStep(arguments.get(index), env,
                continuation(context, value -> {
                    if (!isTruthy(value) || index == arguments.size() - 1) {
                        return new ReturnStep(value, kont);
                    }
                    return evalAnd(arguments, index + 1, env, kont, context);
                }));
    }

    private Step evalOr(List<Expr> arguments, int index, Env env, Kont kont, Expr context) {
        if (index >= arguments.size()) {
            return new ReturnStep(FALSE, kont);
        }

        return new EvalExprStep(arguments.get(index), env,
                continuation(context, value -> {
                    if (isTruthy(value) || index == arguments.size() - 1) {
                        return new ReturnStep(value, kont);
                    }
                    return evalOr(arguments, index + 1, env, kont, context);
                }));
    }

    private Step evalLet(List<Expr> arguments, Env env, Kont kont, Expr context)
            throws EvalError {
        if (arguments.isEmpty()) {
            throw new EvalError("let requires bindings and a body");
        }

        Expr firstArgument = arguments.get(0);
        if (firstArgument instanceof SymbolExpr nameExpr) {
            if (arguments.size() < 3) {
                throw new EvalError("named let requires bindings and a body");
            }
            return evalNamedLet(nameExpr.name(), arguments.get(1),
                    arguments.subList(2, arguments.size()), env, kont, context);
        }

        if (arguments.size() < 2) {
            throw new EvalError("let requires bindings and a body");
        }

        List<BindingSpec> bindings = parseBindings(firstArgument);
        return evalBindingValues(bindings, 0, env, List.of(),
                values -> {
                    Env letEnv = new Env(env);
                    bindValues(letEnv, bindings, values);
                    return evalSequence(arguments.subList(1, arguments.size()), letEnv, kont);
                },
                context);
    }

    private Step evalLetStar(List<Expr> arguments, Env env, Kont kont, Expr context)
            throws EvalError {
        if (arguments.size() < 2) {
            throw new EvalError("let* requires bindings and a body");
        }

        return evalLetStarBindings(
                parseBindings(arguments.get(0)),
                0,
                new Env(env),
                arguments.subList(1, arguments.size()),
                kont,
                context);
    }

    private Step evalLetStarBindings(List<BindingSpec> bindings, int index, Env currentEnv,
            List<Expr> body, Kont kont, Expr context) {
        if (index >= bindings.size()) {
            return evalSequence(body, currentEnv, kont);
        }

        BindingSpec binding = bindings.get(index);
        return new EvalExprStep(binding.initExpr(), currentEnv,
                continuation(context, value -> {
                    Env nextEnv = new Env(currentEnv);
                    nextEnv.define(binding.name(), value);
                    return evalLetStarBindings(bindings, index + 1, nextEnv, body, kont, context);
                }));
    }

    private Step evalLetRec(List<Expr> arguments, Env env, boolean sequential, Kont kont,
            Expr context) throws EvalError {
        if (arguments.size() < 2) {
            throw new EvalError((sequential ? "letrec*" : "letrec")
                    + " requires bindings and a body");
        }

        List<BindingSpec> bindings = parseBindings(arguments.get(0));
        Env letrecEnv = new Env(env);
        List<Cell> cells = new ArrayList<>(bindings.size());
        for (BindingSpec binding : bindings) {
            cells.add(letrecEnv.definePlaceholder(binding.name()));
        }

        if (sequential) {
            return evalLetRecSequential(bindings, cells, 0, letrecEnv,
                    arguments.subList(1, arguments.size()), kont, context);
        }

        return evalBindingValues(bindings, 0, letrecEnv, List.of(),
                values -> {
                    for (int index = 0; index < values.size(); index++) {
                        cells.get(index).set(values.get(index));
                    }
                    return evalSequence(arguments.subList(1, arguments.size()), letrecEnv, kont);
                },
                context);
    }

    private Step evalLetRecSequential(List<BindingSpec> bindings, List<Cell> cells, int index,
            Env letrecEnv, List<Expr> body, Kont kont, Expr context) {
        if (index >= bindings.size()) {
            return evalSequence(body, letrecEnv, kont);
        }

        return new EvalExprStep(bindings.get(index).initExpr(), letrecEnv,
                continuation(context, value -> {
                    cells.get(index).set(value);
                    return evalLetRecSequential(
                            bindings, cells, index + 1, letrecEnv, body, kont, context);
                }));
    }

    private Step evalNamedLet(String name, Expr bindingExpr, List<Expr> body, Env env, Kont kont,
            Expr context) throws EvalError {
        List<BindingSpec> bindings = parseBindings(bindingExpr);
        Env letEnv = new Env(env);
        Cell binding = letEnv.definePlaceholder(name);
        ClosureValue closure = new ClosureValue(
                new Formals(bindingNames(bindings), null),
                List.copyOf(body),
                letEnv);
        binding.set(closure);
        return evalBindingValues(bindings, 0, env, List.of(),
                values -> new ApplyStep(
                        closure,
                        values,
                        kont,
                        context.line(),
                        context.column()),
                context);
    }

    private List<String> bindingNames(List<BindingSpec> bindings) {
        List<String> names = new ArrayList<>(bindings.size());
        for (BindingSpec binding : bindings) {
            names.add(binding.name());
        }
        return List.copyOf(names);
    }

    private List<BindingSpec> parseBindings(Expr bindingExpr) throws EvalError {
        if (!(bindingExpr instanceof ListExpr bindingList)) {
            throw new EvalError("let bindings must be a list");
        }

        List<BindingSpec> bindings = new ArrayList<>(bindingList.elements().size());
        for (Expr entryExpr : bindingList.elements()) {
            if (!(entryExpr instanceof ListExpr entry) || entry.elements().size() != 2) {
                throw new EvalError("let binding must contain a name and value");
            }
            Expr nameExpr = entry.elements().get(0);
            if (!(nameExpr instanceof SymbolExpr symbolExpr)) {
                throw new EvalError("let binding name must be a symbol");
            }
            bindings.add(new BindingSpec(symbolExpr.name(), entry.elements().get(1)));
        }
        return List.copyOf(bindings);
    }

    private Step evalBindingValues(List<BindingSpec> bindings, int index, Env env,
            List<Value> values, ValueListHandler handler, Expr context) throws EvalError {
        if (index >= bindings.size()) {
            return handler.apply(values);
        }

        return new EvalExprStep(bindings.get(index).initExpr(), env,
                continuation(context, value -> evalBindingValues(
                        bindings,
                        index + 1,
                        env,
                        appendValue(values, value),
                        handler,
                        context)));
    }

    private void bindValues(Env env, List<BindingSpec> bindings, List<Value> values) {
        for (int i = 0; i < bindings.size(); i++) {
            env.define(bindings.get(i).name(), values.get(i));
        }
    }

    private Step evalCond(List<Expr> arguments, int index, Env env, Kont kont, Expr context)
            throws EvalError {
        if (index >= arguments.size()) {
            return new ReturnStep(VOID, kont);
        }

        Expr clauseExpr = arguments.get(index);
        if (!(clauseExpr instanceof ListExpr clause) || clause.elements().isEmpty()) {
            throw new EvalError("cond clause must be a non-empty list");
        }

        List<Expr> elements = clause.elements();
        Expr testExpr = elements.get(0);
        if (testExpr instanceof SymbolExpr symbolExpr && symbolExpr.name().equals("else")) {
            if (index != arguments.size() - 1) {
                throw new EvalError("cond else clause must be last");
            }
            if (elements.size() == 1) {
                return new ReturnStep(TRUE, kont);
            }
            return evalSequence(elements.subList(1, elements.size()), env, kont);
        }

        return new EvalExprStep(testExpr, env,
                continuation(context, testValue -> {
                    if (isTruthy(testValue)) {
                        if (isCondArrowClause(elements)) {
                            return evalCondArrowClause(elements.get(2), testValue, env, kont,
                                    clause);
                        }
                        if (elements.size() == 1) {
                            return new ReturnStep(testValue, kont);
                        }
                        return evalSequence(elements.subList(1, elements.size()), env, kont);
                    }
                    return evalCond(arguments, index + 1, env, kont, context);
                }));
    }

    private boolean isCondArrowClause(List<Expr> elements) {
        return elements.size() == 3
                && elements.get(1) instanceof SymbolExpr symbolExpr
                && "=>".equals(symbolExpr.name());
    }

    private Step evalCondArrowClause(Expr procedureExpr, Value testValue, Env env, Kont kont,
            Expr context) {
        return new EvalExprStep(procedureExpr, env,
                continuation(context, procedure -> new ApplyStep(
                        procedure,
                        List.of(testValue),
                        kont,
                        context.line(),
                        context.column())));
    }

    private Step evalCase(List<Expr> arguments, Env env, Kont kont, Expr context)
            throws EvalError {
        requireMinArgs("case", arguments, 1);
        return new EvalExprStep(arguments.get(0), env,
                continuation(context, key -> evalCaseClauses(
                        key,
                        arguments,
                        1,
                        env,
                        kont,
                        context)));
    }

    private Step evalCaseClauses(Value key, List<Expr> arguments, int index, Env env, Kont kont,
            Expr context) throws EvalError {
        if (index >= arguments.size()) {
            return new ReturnStep(VOID, kont);
        }

        Expr clauseExpr = arguments.get(index);
        if (!(clauseExpr instanceof ListExpr clause) || clause.elements().isEmpty()) {
            throw new EvalError("case clause must be a non-empty list");
        }

        List<Expr> clauseElements = clause.elements();
        Expr headExpr = clauseElements.get(0);
        if (headExpr instanceof SymbolExpr symbolExpr && "else".equals(symbolExpr.name())) {
            if (index != arguments.size() - 1) {
                throw new EvalError("case else clause must be last");
            }
            return evalSequence(clauseElements.subList(1, clauseElements.size()), env, kont);
        }

        if (!(headExpr instanceof ListExpr datumList)) {
            throw new EvalError("case clause datums must be a list");
        }

        for (Expr datumExpr : datumList.elements()) {
            if (eqValues(key, QuotationSupport.quoteToValue(datumExpr))) {
                return evalSequence(clauseElements.subList(1, clauseElements.size()), env, kont);
            }
        }
        return evalCaseClauses(key, arguments, index + 1, env, kont, context);
    }

    private Step evalDo(List<Expr> arguments, Env env, Kont kont, Expr context) throws EvalError {
        if (arguments.size() < 2) {
            throw new EvalError("do requires bindings and a test clause");
        }

        List<DoBindingSpec> bindings = parseDoBindings(arguments.get(0));
        if (!(arguments.get(1) instanceof ListExpr testClause) || testClause.elements().isEmpty()) {
            throw new EvalError("do test clause must be a non-empty list");
        }

        Expr testExpr = testClause.elements().get(0);
        List<Expr> resultExprs = testClause.elements().subList(1, testClause.elements().size());
        List<Expr> bodyExprs = arguments.subList(2, arguments.size());
        return evalDoInitValues(bindings, 0, env, List.of(),
                initValues -> {
                    Env loopEnv = new Env(env);
                    List<Cell> cells = new ArrayList<>(bindings.size());
                    for (int index = 0; index < bindings.size(); index++) {
                        Cell cell = loopEnv.definePlaceholder(bindings.get(index).name());
                        cell.set(initValues.get(index));
                        cells.add(cell);
                    }
                    return evalDoLoop(bindings, cells, loopEnv, testExpr, resultExprs, bodyExprs,
                            kont, context);
                },
                context);
    }

    private Step evalDoInitValues(List<DoBindingSpec> bindings, int index, Env env,
            List<Value> values, ValueListHandler handler, Expr context) throws EvalError {
        if (index >= bindings.size()) {
            return handler.apply(values);
        }

        return new EvalExprStep(bindings.get(index).initExpr(), env,
                continuation(context, value -> evalDoInitValues(
                        bindings,
                        index + 1,
                        env,
                        appendValue(values, value),
                        handler,
                        context)));
    }

    private Step evalDoLoop(List<DoBindingSpec> bindings, List<Cell> cells, Env loopEnv,
            Expr testExpr, List<Expr> resultExprs, List<Expr> bodyExprs, Kont kont, Expr context) {
        return new EvalExprStep(testExpr, loopEnv,
                continuation(context, testValue -> {
                    if (isTruthy(testValue)) {
                        return evalSequence(resultExprs, loopEnv, kont);
                    }
                    return evalSequence(bodyExprs, loopEnv,
                            continuation(context, ignored -> evalDoSteps(
                                    bindings,
                                    cells,
                                    0,
                                    loopEnv,
                                    List.of(),
                                    testExpr,
                                    resultExprs,
                                    bodyExprs,
                                    kont,
                                    context)));
                }));
    }

    private Step evalDoSteps(List<DoBindingSpec> bindings, List<Cell> cells, int index, Env loopEnv,
            List<Value> stepValues, Expr testExpr, List<Expr> resultExprs, List<Expr> bodyExprs,
            Kont kont, Expr context) {
        if (index >= bindings.size()) {
            for (int stepIndex = 0; stepIndex < cells.size(); stepIndex++) {
                cells.get(stepIndex).set(stepValues.get(stepIndex));
            }
            return evalDoLoop(bindings, cells, loopEnv, testExpr, resultExprs, bodyExprs, kont,
                    context);
        }

        DoBindingSpec binding = bindings.get(index);
        if (binding.stepExpr() == null) {
            return evalDoSteps(
                    bindings,
                    cells,
                    index + 1,
                    loopEnv,
                    appendValue(stepValues, cells.get(index).get()),
                    testExpr,
                    resultExprs,
                    bodyExprs,
                    kont,
                    context);
        }

        return new EvalExprStep(binding.stepExpr(), loopEnv,
                continuation(context, value -> evalDoSteps(
                        bindings,
                        cells,
                        index + 1,
                        loopEnv,
                        appendValue(stepValues, value),
                        testExpr,
                        resultExprs,
                        bodyExprs,
                        kont,
                        context)));
    }

    private List<DoBindingSpec> parseDoBindings(Expr bindingExpr) throws EvalError {
        if (!(bindingExpr instanceof ListExpr bindingList)) {
            throw new EvalError("do bindings must be a list");
        }

        List<DoBindingSpec> bindings = new ArrayList<>(bindingList.elements().size());
        for (Expr entryExpr : bindingList.elements()) {
            if (!(entryExpr instanceof ListExpr entry)
                    || entry.elements().size() < 2
                    || entry.elements().size() > 3) {
                throw new EvalError("do binding must contain a name, init, and optional step");
            }

            Expr nameExpr = entry.elements().get(0);
            if (!(nameExpr instanceof SymbolExpr symbolExpr)) {
                throw new EvalError("do binding name must be a symbol");
            }

            Expr stepExpr = entry.elements().size() == 3 ? entry.elements().get(2) : null;
            bindings.add(new DoBindingSpec(symbolExpr.name(), entry.elements().get(1), stepExpr));
        }
        return List.copyOf(bindings);
    }

    private Step evalGuard(List<Expr> arguments, Env env, Kont kont, Expr context)
            throws EvalError {
        if (arguments.size() < 2) {
            throw new EvalError("guard requires clauses and a body");
        }

        Expr guardExpr = arguments.get(0);
        if (!(guardExpr instanceof ListExpr guardList) || guardList.elements().isEmpty()) {
            throw new EvalError("guard expects an exception variable and clauses");
        }

        Expr variableExpr = guardList.elements().get(0);
        if (!(variableExpr instanceof SymbolExpr variableSymbol)) {
            throw new EvalError("guard exception variable must be a symbol");
        }

        List<Expr> clauses = List.copyOf(guardList.elements().subList(1,
                guardList.elements().size()));
        ExceptionHandlerFrame frame = new ExceptionHandlerFrame(
                List.copyOf(dynamicWindStack),
                exceptionHandlerStack,
                exception -> evalGuardClauses(
                        clauses,
                        0,
                        createGuardEnv(env, variableSymbol.name(), exception.value()),
                        exception,
                        kont,
                        context));
        pushExceptionHandlerFrame(frame);
        return evalSequence(arguments.subList(1, arguments.size()), env,
                continuation(context, value -> exitExceptionHandler(frame, value, kont)));
    }

    private Env createGuardEnv(Env env, String variableName, Value exceptionValue) {
        Env guardEnv = new Env(env);
        guardEnv.define(variableName, exceptionValue);
        return guardEnv;
    }

    private Step evalGuardClauses(List<Expr> clauses, int index, Env guardEnv,
            RaisedException exception, Kont kont, Expr context) throws EvalError {
        if (index >= clauses.size()) {
            throw exception;
        }

        Expr clauseExpr = clauses.get(index);
        if (!(clauseExpr instanceof ListExpr clause) || clause.elements().isEmpty()) {
            throw new EvalError("guard clause must be a non-empty list");
        }

        List<Expr> elements = clause.elements();
        Expr testExpr = elements.get(0);
        if (testExpr instanceof SymbolExpr symbolExpr && "else".equals(symbolExpr.name())) {
            if (index != clauses.size() - 1) {
                throw new EvalError("guard else clause must be last");
            }
            if (elements.size() == 1) {
                return new ReturnStep(TRUE, kont);
            }
            return evalSequence(elements.subList(1, elements.size()), guardEnv, kont);
        }

        return new EvalExprStep(testExpr, guardEnv,
                continuation(context, testValue -> {
                    if (isTruthy(testValue)) {
                        if (elements.size() == 1) {
                            return new ReturnStep(testValue, kont);
                        }
                        return evalSequence(elements.subList(1, elements.size()), guardEnv, kont);
                    }
                    return evalGuardClauses(
                            clauses,
                            index + 1,
                            guardEnv,
                            exception,
                            kont,
                            context);
                }));
    }

    private Value applyProcedure(Value operator, List<Value> arguments) throws EvalError {
        return run(new ApplyStep(operator, List.copyOf(arguments), DONE, 0, 0));
    }

    private Step applyProcedureStep(Value operator, List<Value> arguments, Kont kont, int line,
            int column) throws EvalError {
        return switch (operator) {
            case BuiltinValue builtinValue ->
                    applyBuiltin(builtinValue, arguments, kont, line, column);
            case ClosureValue closureValue -> applyClosure(closureValue, arguments, kont);
            case CaseLambdaValue caseLambdaValue -> applyCaseLambda(caseLambdaValue, arguments, kont);
            default -> throw new EvalError("attempted to call a non-procedure");
        };
    }

    private Step applyBuiltin(BuiltinValue builtinValue, List<Value> arguments, Kont kont, int line,
            int column) throws EvalError {
        return switch (builtinValue.name()) {
            case "values" -> new ReturnStep(packValues(arguments), kont);
            case "call-with-values" -> applyCallWithValues(arguments, kont, line, column);
            case "call/cc", "call-with-current-continuation" ->
                    applyCallWithCurrentContinuation(builtinValue.name(), arguments, kont, line,
                            column);
            case "dynamic-wind" -> applyDynamicWind(arguments, kont, line, column);
            case "raise" -> applyRaise(arguments, line, column);
            case "error" -> applyError(arguments, line, column);
            case "with-exception-handler" ->
                    applyWithExceptionHandler(arguments, kont, line, column);
            case "apply" -> applyBuiltinApply(arguments, kont, line, column);
            case "map" -> applyBuiltinMap(arguments, kont, line, column);
            case "for-each" -> applyBuiltinForEach(arguments, kont, line, column);
            default -> new ReturnStep(builtinValue.implementation().apply(arguments), kont);
        };
    }

    private Step applyCallWithValues(List<Value> arguments, Kont kont, int line, int column)
            throws EvalError {
        requireExactArgs("call-with-values", arguments, 2);
        return new ApplyStep(arguments.get(0), List.of(),
                continuation(line, column, produced -> new ApplyStep(arguments.get(1),
                        unpackValues(produced), kont, line, column)),
                line,
                column);
    }

    private Step applyCallWithCurrentContinuation(String name, List<Value> arguments, Kont kont,
            int line, int column) throws EvalError {
        requireExactArgs(name, arguments, 1);
        return new ApplyStep(arguments.get(0), List.of(makeContinuationValue(captureContinuation(kont))), kont, line,
                column);
    }

    private CapturedContinuation captureContinuation(Kont kont) {
        return new CapturedContinuation(kont,
                List.copyOf(dynamicWindStack),
                exceptionHandlerStack);
    }

    private Value makeContinuationValue(CapturedContinuation continuation) {
        return new BuiltinValue("continuation", arguments -> {
            throw new ContinuationJump(continuation, packValues(arguments));
        });
    }

    private Step applyDynamicWind(List<Value> arguments, Kont kont, int line, int column)
            throws EvalError {
        requireExactArgs("dynamic-wind", arguments, 3);

        DynamicWindFrame frame = new DynamicWindFrame(arguments.get(0), arguments.get(2));
        Value bodyThunk = arguments.get(1);
        return new ApplyStep(frame.inThunk(), List.of(),
                continuation(line, column, ignored -> {
                    pushDynamicWindFrame(frame);
                    return new ApplyStep(bodyThunk, List.of(),
                            continuation(line, column, bodyValue -> exitDynamicWind(
                                    frame, bodyValue, kont, line, column)),
                            line,
                            column);
                }),
                line,
                column);
    }

    private Step exitDynamicWind(DynamicWindFrame frame, Value bodyValue, Kont kont, int line,
            int column) {
        popDynamicWindFrame(frame);
        return new ApplyStep(frame.outThunk(), List.of(),
                continuation(line, column, ignored -> new ReturnStep(bodyValue, kont)),
                line,
                column);
    }

    private Step jumpToContinuation(CapturedContinuation continuation, Value value) {
        exceptionHandlerStack = continuation.exceptionHandlerStack();
        return transitionDynamicWind(continuation.dynamicStack(),
                new ReturnStep(value, continuation.target()));
    }

    private Step handleRaisedException(RaisedException exception) throws EvalError {
        if (exceptionHandlerStack == null) {
            EvalError error = new EvalError(
                    "uncaught exception: " + ValueRenderer.render(exception.value()));
            if (exception.hasPosition()) {
                throw error.withPosition(exception.line(), exception.column());
            }
            throw error;
        }

        ExceptionHandlerFrame frame = exceptionHandlerStack;
        exceptionHandlerStack = frame.parent();
        return transitionDynamicWind(frame.dynamicStack(),
                new ThunkStep(() -> frame.body().apply(exception)));
    }

    private Step transitionDynamicWind(List<DynamicWindFrame> targetStack, Step nextStep) {
        int sharedDepth = commonDynamicWindDepth(dynamicWindStack, targetStack);
        return unwindDynamicWind(targetStack, sharedDepth, nextStep);
    }

    private Step unwindDynamicWind(List<DynamicWindFrame> targetStack, int sharedDepth,
            Step nextStep) {
        if (dynamicWindStack.size() <= sharedDepth) {
            return rewindDynamicWind(targetStack, sharedDepth, nextStep);
        }

        DynamicWindFrame frame = dynamicWindStack.get(dynamicWindStack.size() - 1);
        popDynamicWindFrame(frame);
        return new ApplyStep(frame.outThunk(), List.of(),
                continuation(ignored -> unwindDynamicWind(targetStack, sharedDepth, nextStep)),
                0,
                0);
    }

    private Step rewindDynamicWind(List<DynamicWindFrame> targetStack, int index, Step nextStep) {
        if (index >= targetStack.size()) {
            return nextStep;
        }

        DynamicWindFrame frame = targetStack.get(index);
        return new ApplyStep(frame.inThunk(), List.of(),
                continuation(ignored -> {
                    pushDynamicWindFrame(frame);
                    return rewindDynamicWind(targetStack, index + 1, nextStep);
                }),
                0,
                0);
    }

    private int commonDynamicWindDepth(List<DynamicWindFrame> left, List<DynamicWindFrame> right) {
        int maxShared = Math.min(left.size(), right.size());
        int index = 0;
        while (index < maxShared && left.get(index) == right.get(index)) {
            index++;
        }
        return index;
    }

    private void pushDynamicWindFrame(DynamicWindFrame frame) {
        dynamicWindStack.add(frame);
    }

    private void popDynamicWindFrame(DynamicWindFrame frame) {
        int lastIndex = dynamicWindStack.size() - 1;
        if (lastIndex < 0 || dynamicWindStack.get(lastIndex) != frame) {
            throw new IllegalStateException("dynamic-wind stack out of sync");
        }
        dynamicWindStack.remove(lastIndex);
    }

    private void pushExceptionHandlerFrame(ExceptionHandlerFrame frame) {
        exceptionHandlerStack = frame;
    }

    private void popExceptionHandlerFrame(ExceptionHandlerFrame frame) {
        if (exceptionHandlerStack != frame) {
            throw new IllegalStateException("exception handler stack out of sync");
        }
        exceptionHandlerStack = frame.parent();
    }

    private Step exitExceptionHandler(ExceptionHandlerFrame frame, Value value, Kont kont) {
        popExceptionHandlerFrame(frame);
        return new ReturnStep(value, kont);
    }

    private Step applyRaise(List<Value> arguments, int line, int column) throws EvalError {
        requireExactArgs("raise", arguments, 1);
        throw new RaisedException(arguments.get(0), line, column);
    }

    private Step applyError(List<Value> arguments, int line, int column) throws EvalError {
        requireMinArgs("error", arguments, 1);
        Value payload = arguments.size() == 1 ? arguments.get(0) : listValue(arguments);
        throw new RaisedException(payload, line, column);
    }

    private Step applyWithExceptionHandler(List<Value> arguments, Kont kont, int line, int column)
            throws EvalError {
        requireExactArgs("with-exception-handler", arguments, 2);

        ExceptionHandlerFrame frame = new ExceptionHandlerFrame(
                List.copyOf(dynamicWindStack),
                exceptionHandlerStack,
                exception -> new ApplyStep(arguments.get(0), List.of(exception.value()), kont, line,
                        column));
        pushExceptionHandlerFrame(frame);
        return new ApplyStep(arguments.get(1), List.of(),
                continuation(line, column, value -> exitExceptionHandler(frame, value, kont)),
                line,
                column);
    }

    private Step applyBuiltinApply(List<Value> arguments, Kont kont, int line, int column)
            throws EvalError {
        requireMinArgs("apply", arguments, 2);

        Value operator = arguments.get(0);
        List<Value> appliedArguments = new ArrayList<>();
        for (int i = 1; i < arguments.size() - 1; i++) {
            appliedArguments.add(arguments.get(i));
        }
        appliedArguments.addAll(requireProperList(arguments.get(arguments.size() - 1), "apply"));
        return new ApplyStep(operator, List.copyOf(appliedArguments), kont, line, column);
    }

    private List<List<Value>> requireEqualLengthLists(List<Value> arguments, String name)
            throws EvalError {
        List<List<Value>> lists = new ArrayList<>(arguments.size());
        int expectedLength = -1;
        for (Value argument : arguments) {
            List<Value> list = requireProperList(argument, name);
            if (expectedLength == -1) {
                expectedLength = list.size();
            } else if (list.size() != expectedLength) {
                throw new EvalError(name + " expects lists of equal length");
            }
            lists.add(list);
        }
        return List.copyOf(lists);
    }

    private Step applyBuiltinMap(List<Value> arguments, Kont kont, int line, int column)
            throws EvalError {
        requireMinArgs("map", arguments, 2);
        return applyMapItem(
                arguments.get(0),
                requireEqualLengthLists(arguments.subList(1, arguments.size()), "map"),
                0,
                EMPTY_LIST,
                kont,
                line,
                column);
    }

    private Step applyMapItem(Value procedure, List<List<Value>> lists, int index,
            Value reversedResults, Kont kont, int line, int column) {
        int length = lists.isEmpty() ? 0 : lists.get(0).size();
        if (index >= length) {
            return new ReturnStep(reverseList(reversedResults), kont);
        }

        List<Value> callArguments = new ArrayList<>(lists.size());
        for (List<Value> list : lists) {
            callArguments.add(list.get(index));
        }
        return new ApplyStep(procedure, List.copyOf(callArguments),
                continuation(line, column, value -> applyMapItem(
                        procedure,
                        lists,
                        index + 1,
                        new PairValue(value, reversedResults),
                        kont,
                        line,
                        column)),
                line,
                column);
    }

    private Step applyBuiltinForEach(List<Value> arguments, Kont kont, int line, int column)
            throws EvalError {
        requireMinArgs("for-each", arguments, 2);
        return applyForEachItem(
                arguments.get(0),
                requireEqualLengthLists(arguments.subList(1, arguments.size()), "for-each"),
                0,
                kont,
                line,
                column);
    }

    private Step applyForEachItem(Value procedure, List<List<Value>> lists, int index, Kont kont,
            int line, int column) {
        int length = lists.isEmpty() ? 0 : lists.get(0).size();
        if (index >= length) {
            return new ReturnStep(VOID, kont);
        }

        List<Value> callArguments = new ArrayList<>(lists.size());
        for (List<Value> list : lists) {
            callArguments.add(list.get(index));
        }
        return new ApplyStep(procedure, List.copyOf(callArguments),
                continuation(line, column, ignored -> applyForEachItem(
                        procedure,
                        lists,
                        index + 1,
                        kont,
                        line,
                        column)),
                line,
                column);
    }

    private Value reverseList(Value list) {
        Value result = EMPTY_LIST;
        Value current = list;
        while (current instanceof PairValue pairValue) {
            result = new PairValue(pairValue.car(), result);
            current = pairValue.cdr();
        }
        return result;
    }

    private Step applyClosure(ClosureValue closure, List<Value> arguments, Kont kont)
            throws EvalError {
        return applyProcedureClause(
                closure.formals(),
                closure.body(),
                closure.env(),
                arguments,
                "lambda",
                kont);
    }

    private Step applyCaseLambda(CaseLambdaValue caseLambda, List<Value> arguments, Kont kont)
            throws EvalError {
        for (ProcedureClause clause : caseLambda.clauses()) {
            if (clause.formals().matchesArity(arguments.size())) {
                return applyProcedureClause(
                        clause.formals(),
                        clause.body(),
                        caseLambda.env(),
                        arguments,
                        "case-lambda",
                        kont);
            }
        }

        throw new EvalError("case-lambda has no matching clause for "
                + arguments.size() + " argument(s)");
    }

    private Step applyProcedureClause(Formals formals, List<Expr> body, Env definitionEnv,
            List<Value> arguments, String procedureName, Kont kont) throws EvalError {
        int fixedCount = formals.fixedCount();
        if (formals.restParameter() == null) {
            requireExactArgs(procedureName, arguments, fixedCount);
        } else if (arguments.size() < fixedCount) {
            throw new EvalError(procedureName + " expected at least "
                    + fixedCount + " argument(s)");
        }

        Env callEnv = new Env(definitionEnv);
        for (int i = 0; i < fixedCount; i++) {
            callEnv.define(formals.parameters().get(i), arguments.get(i));
        }
        if (formals.restParameter() != null) {
            callEnv.define(formals.restParameter(),
                    listValue(arguments.subList(fixedCount, arguments.size())));
        }
        return evalSequence(body, callEnv, kont);
    }

    private void appendOutput(String value) {
        if (outputBuffer != null) {
            outputBuffer.append(value);
        }
    }

    private record DoBindingSpec(String name, Expr initExpr, Expr stepExpr) {
    }
}
