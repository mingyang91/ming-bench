package ming;

import java.util.ArrayList;
import java.util.List;

final class ContinuationEvaluator {
    private record Binding(String name, Expr valueExpression) {
    }

    static final class WindFrame {
        private final WindFrame parent;
        private final Value inThunk;
        private final Value outThunk;

        private WindFrame(WindFrame parent, Value inThunk, Value outThunk) {
            this.parent = parent;
            this.inThunk = inThunk;
            this.outThunk = outThunk;
        }

        WindFrame parent() {
            return parent;
        }

        Value inThunk() {
            return inThunk;
        }

        Value outThunk() {
            return outThunk;
        }
    }

    sealed interface ExceptionHandlerFrame permits ProcedureExceptionHandlerFrame, GuardExceptionHandlerFrame {
        ExceptionHandlerFrame parent();

        Kont continuation();

        WindFrame windFrame();
    }

    record ProcedureExceptionHandlerFrame(ExceptionHandlerFrame parent,
                                          Value handlerProcedure,
                                          Kont continuation,
                                          WindFrame windFrame,
                                          int line,
                                          int column) implements ExceptionHandlerFrame {
    }

    record GuardExceptionHandlerFrame(ExceptionHandlerFrame parent,
                                      String variableName,
                                      List<Expr> clauses,
                                      Environment environment,
                                      Kont continuation,
                                      WindFrame windFrame,
                                      int line,
                                      int column) implements ExceptionHandlerFrame {
        GuardExceptionHandlerFrame {
            clauses = List.copyOf(clauses);
        }
    }

    private sealed interface MachineState permits EvalExprState, ReturnValueState, DoneState {
    }

    private record EvalExprState(Expr expression,
                                 Environment environment,
                                 Kont continuation) implements MachineState {
    }

    private record ReturnValueState(Value value, Kont continuation) implements MachineState {
    }

    private record DoneState(Value value) implements MachineState {
    }

    private final Evaluator owner;
    private final Environment globalEnvironment;
    private final CallCcProcedureValue callCcProcedure = new CallCcProcedureValue();
    private final RaiseProcedureValue raiseProcedure = new RaiseProcedureValue();
    private final WithExceptionHandlerProcedureValue withExceptionHandlerProcedure =
            new WithExceptionHandlerProcedureValue();
    private WindFrame activeWinds;
    private ExceptionHandlerFrame activeHandlers;

    ContinuationEvaluator(Evaluator owner) {
        this.owner = owner;
        this.globalEnvironment = owner.createGlobalEnvironment();
        globalEnvironment.define("call/cc", callCcProcedure);
        globalEnvironment.define("call-with-current-continuation", callCcProcedure);
        globalEnvironment.define("raise", raiseProcedure);
        globalEnvironment.define("with-exception-handler", withExceptionHandlerProcedure);
    }

    static boolean referencesContinuations(List<Expr> expressions) {
        for (Expr expression : expressions) {
            if (referencesContinuations(expression)) {
                return true;
            }
        }
        return false;
    }

    private static boolean referencesContinuations(Expr expression) {
        if (expression instanceof SymbolExpr symbolExpr) {
            return "call/cc".equals(symbolExpr.name())
                    || "call-with-current-continuation".equals(symbolExpr.name())
                    || "dynamic-wind".equals(symbolExpr.name())
                    || "guard".equals(symbolExpr.name())
                    || "raise".equals(symbolExpr.name())
                    || "with-exception-handler".equals(symbolExpr.name());
        }
        if (expression instanceof ListExpr listExpr) {
            for (Expr element : listExpr.elements()) {
                if (referencesContinuations(element)) {
                    return true;
                }
            }
        }
        return false;
    }

    Value evalProgram(List<Expr> expressions) throws EvalError {
        if (expressions.isEmpty()) {
            throw new EvalError("expected at least one expression");
        }
        activeWinds = null;
        activeHandlers = null;
        return run(evaluateSequence(expressions, globalEnvironment, HaltKont.INSTANCE));
    }

    private Value run(MachineState initialState) throws EvalError {
        MachineState state = initialState;
        while (true) {
            switch (state) {
                case EvalExprState evalExprState -> {
                    try {
                        state = evalExpression(
                                evalExprState.expression(),
                                evalExprState.environment(),
                                evalExprState.continuation());
                    } catch (EvalError error) {
                        throw error.withPosition(
                                evalExprState.expression().line(),
                                evalExprState.expression().column());
                    }
                }
                case ReturnValueState returnValueState ->
                        state = continueWithValue(returnValueState.value(), returnValueState.continuation());
                case DoneState doneState -> {
                    return doneState.value();
                }
            }
        }
    }

    private MachineState evalExpression(Expr expression,
                                        Environment environment,
                                        Kont continuation) throws EvalError {
        return switch (expression) {
            case IntExpr intExpr -> new ReturnValueState(new IntValue(intExpr.value()), continuation);
            case NumberExpr numberExpr -> new ReturnValueState(Numbers.parseLiteral(numberExpr.token()), continuation);
            case BoolExpr boolExpr -> new ReturnValueState(new BoolValue(boolExpr.value()), continuation);
            case StringExpr stringExpr -> new ReturnValueState(new StringValue(stringExpr.value(), false), continuation);
            case CharExpr charExpr -> new ReturnValueState(new CharValue(charExpr.value()), continuation);
            case SymbolExpr symbolExpr -> new ReturnValueState(environment.lookup(symbolExpr.name()), continuation);
            case ListExpr listExpr -> evalList(listExpr, environment, continuation);
        };
    }

    private MachineState evalList(ListExpr listExpr,
                                  Environment environment,
                                  Kont continuation) throws EvalError {
        List<Expr> elements = listExpr.elements();
        if (elements.isEmpty()) {
            throw new EvalError("cannot evaluate empty list");
        }

        Expr operatorExpression = elements.getFirst();
        List<Expr> arguments = elements.subList(1, elements.size());

        if (operatorExpression instanceof SymbolExpr symbolExpr) {
            if ("define-syntax".equals(symbolExpr.name())) {
                return new ReturnValueState(owner.defineSyntax(arguments, environment), continuation);
            }

            MacroDefinition macroDefinition = environment.lookupMacro(symbolExpr.name());
            if (macroDefinition != null) {
                Expr expanded = owner.expandMacro(macroDefinition, listExpr, environment);
                return new EvalExprState(expanded, environment, continuation);
            }

            return switch (symbolExpr.name()) {
                case "define" -> evalDefine(arguments, environment, continuation);
                case "set!" -> evalSet(arguments, environment, continuation);
                case "if" -> evalIf(arguments, environment, continuation);
                case "quote" -> evalQuote(arguments, continuation);
                case "lambda" -> evalLambda(arguments, environment, continuation);
                case "begin" -> evaluateSequence(arguments, environment, continuation);
                case "cond" -> evalCond(arguments, environment, continuation);
                case "let" -> evalLet(arguments, environment, continuation, listExpr.line(), listExpr.column());
                case "and" -> evalAnd(arguments, environment, continuation);
                case "or" -> evalOr(arguments, environment, continuation);
                case "guard" -> evalGuard(arguments, environment, continuation, listExpr.line(), listExpr.column());
                case "dynamic-wind" ->
                        evalDynamicWind(arguments, environment, continuation, listExpr.line(), listExpr.column());
                default -> evalApplication(
                        operatorExpression,
                        arguments,
                        environment,
                        continuation,
                        listExpr.line(),
                        listExpr.column());
            };
        }

        return evalApplication(
                operatorExpression,
                arguments,
                environment,
                continuation,
                listExpr.line(),
                listExpr.column());
    }

    private MachineState evalApplication(Expr operatorExpression,
                                         List<Expr> arguments,
                                         Environment environment,
                                         Kont continuation,
                                         int line,
                                         int column) {
        return new EvalExprState(
                operatorExpression,
                environment,
                new ApplyOperatorKont(arguments, environment, continuation, line, column));
    }

    private MachineState evalDynamicWind(List<Expr> arguments,
                                         Environment environment,
                                         Kont continuation,
                                         int line,
                                         int column) throws EvalError {
        requireExactArity("dynamic-wind", arguments.size(), 3);
        return new EvalExprState(
                arguments.getFirst(),
                environment,
                new DynamicWindInExprKont(arguments.get(1), arguments.get(2), environment, continuation, line, column));
    }

    private MachineState evalGuard(List<Expr> arguments,
                                   Environment environment,
                                   Kont continuation,
                                   int line,
                                   int column) throws EvalError {
        if (arguments.size() < 2) {
            throw new EvalError("guard expected a binding and a body");
        }

        Expr bindingExpression = arguments.getFirst();
        if (!(bindingExpression instanceof ListExpr bindingList)) {
            throw new EvalError("guard expected a binding list");
        }

        List<Expr> bindingElements = bindingList.elements();
        if (bindingElements.isEmpty()) {
            throw new EvalError("guard expected an exception variable");
        }

        Expr variableExpression = bindingElements.getFirst();
        if (!(variableExpression instanceof SymbolExpr symbolExpr)) {
            throw new EvalError("guard expected an exception variable");
        }

        GuardExceptionHandlerFrame handlerFrame = new GuardExceptionHandlerFrame(
                activeHandlers,
                symbolExpr.name(),
                bindingElements.subList(1, bindingElements.size()),
                environment,
                continuation,
                activeWinds,
                line,
                column);
        activeHandlers = handlerFrame;
        return evaluateSequence(
                arguments.subList(1, arguments.size()),
                environment,
                new GuardBodyKont(handlerFrame, continuation));
    }

    private MachineState applyThunk(Value thunk,
                                    Kont continuation,
                                    int line,
                                    int column) throws EvalError {
        return applyProcedure(thunk, List.of(), continuation, line, column);
    }

    private MachineState raiseValue(Value value,
                                    int line,
                                    int column) throws EvalError {
        if (activeHandlers == null) {
            throw new EvalError("uncaught exception: " + value.render(), line, column);
        }

        ExceptionHandlerFrame handlerFrame = activeHandlers;
        activeHandlers = handlerFrame.parent();

        return switch (handlerFrame) {
            case ProcedureExceptionHandlerFrame procedureHandlerFrame -> transferAcrossWinds(
                    value,
                    procedureHandlerFrame.windFrame(),
                    new ExceptionHandlerInvokeKont(
                            procedureHandlerFrame.handlerProcedure(),
                            procedureHandlerFrame.continuation(),
                            procedureHandlerFrame.line(),
                            procedureHandlerFrame.column()),
                    line,
                    column);
            case GuardExceptionHandlerFrame guardHandlerFrame -> transferAcrossWinds(
                    value,
                    guardHandlerFrame.windFrame(),
                    new GuardHandlerInvokeKont(guardHandlerFrame, line, column),
                    line,
                    column);
        };
    }

    private MachineState continueWindTransition(Value value,
                                                List<WindFrame> exitFrames,
                                                List<WindFrame> enterFrames,
                                                Kont targetContinuation,
                                                WindFrame targetWinds,
                                                int line,
                                                int column) throws EvalError {
        if (!exitFrames.isEmpty()) {
            WindFrame frame = exitFrames.getFirst();
            activeWinds = frame.parent();
            return applyThunk(
                    frame.outThunk(),
                    new WindExitKont(
                            value,
                            exitFrames.subList(1, exitFrames.size()),
                            enterFrames,
                            targetContinuation,
                            targetWinds,
                            line,
                            column),
                    line,
                    column);
        }

        if (!enterFrames.isEmpty()) {
            WindFrame frame = enterFrames.getFirst();
            return applyThunk(
                    frame.inThunk(),
                    new WindEnterKont(
                            value,
                            frame,
                            enterFrames.subList(1, enterFrames.size()),
                            targetContinuation,
                            targetWinds,
                            line,
                            column),
                    line,
                    column);
        }

        activeWinds = targetWinds;
        return new ReturnValueState(value, targetContinuation);
    }

    private MachineState transferAcrossWinds(Value value,
                                             WindFrame targetWinds,
                                             Kont targetContinuation,
                                             int line,
                                             int column) throws EvalError {
        List<WindFrame> currentFrames = windFramesOuterToInner(activeWinds);
        List<WindFrame> targetFrames = windFramesOuterToInner(targetWinds);
        int commonPrefixLength = commonPrefixLength(currentFrames, targetFrames);

        List<WindFrame> exitFrames = new ArrayList<>(currentFrames.size() - commonPrefixLength);
        for (int i = currentFrames.size() - 1; i >= commonPrefixLength; i--) {
            exitFrames.add(currentFrames.get(i));
        }

        List<WindFrame> enterFrames = new ArrayList<>(targetFrames.size() - commonPrefixLength);
        for (int i = commonPrefixLength; i < targetFrames.size(); i++) {
            enterFrames.add(targetFrames.get(i));
        }

        return continueWindTransition(
                value,
                List.copyOf(exitFrames),
                List.copyOf(enterFrames),
                targetContinuation,
                targetWinds,
                line,
                column);
    }

    private MachineState transferToContinuation(Value value,
                                                ContinuationProcedureValue continuationProcedureValue,
                                                int line,
                                                int column) throws EvalError {
        return transferAcrossWinds(
                value,
                continuationProcedureValue.windFrame(),
                continuationProcedureValue.continuation(),
                line,
                column);
    }

    private List<WindFrame> windFramesOuterToInner(WindFrame frame) {
        List<WindFrame> frames = new ArrayList<>();
        for (WindFrame current = frame; current != null; current = current.parent()) {
            frames.add(0, current);
        }
        return List.copyOf(frames);
    }

    private int commonPrefixLength(List<WindFrame> left, List<WindFrame> right) {
        int length = Math.min(left.size(), right.size());
        int index = 0;
        while (index < length && left.get(index) == right.get(index)) {
            index++;
        }
        return index;
    }

    private MachineState continueWithValue(Value value, Kont continuation) throws EvalError {
        return switch (continuation) {
            case HaltKont ignored -> new DoneState(value);
            case SequenceKont sequenceKont ->
                    evaluateSequence(sequenceKont.remaining(), sequenceKont.environment(), sequenceKont.next());
            case IfKont ifKont -> {
                Expr alternate = ifKont.alternate();
                if (value.isTruthy()) {
                    yield new EvalExprState(ifKont.consequent(), ifKont.environment(), ifKont.next());
                }
                if (alternate == null) {
                    yield new ReturnValueState(VoidValue.INSTANCE, ifKont.next());
                }
                yield new EvalExprState(alternate, ifKont.environment(), ifKont.next());
            }
            case DefineKont defineKont -> {
                defineKont.environment().define(defineKont.name(), value);
                yield new ReturnValueState(VoidValue.INSTANCE, defineKont.next());
            }
            case SetKont setKont -> {
                setKont.environment().set(setKont.name(), value);
                yield new ReturnValueState(VoidValue.INSTANCE, setKont.next());
            }
            case ApplyOperatorKont applyOperatorKont -> {
                List<Expr> arguments = applyOperatorKont.arguments();
                if (arguments.isEmpty()) {
                    yield applyProcedure(
                            value,
                            List.of(),
                            applyOperatorKont.next(),
                            applyOperatorKont.line(),
                            applyOperatorKont.column());
                }
                yield new EvalExprState(
                        arguments.getLast(),
                        applyOperatorKont.environment(),
                        new ApplyArgsKont(
                                value,
                                List.of(),
                                arguments.subList(0, arguments.size() - 1),
                                applyOperatorKont.environment(),
                                applyOperatorKont.next(),
                                applyOperatorKont.line(),
                                applyOperatorKont.column()));
            }
            case ApplyArgsKont applyArgsKont -> {
                List<Value> evaluatedArguments = prependArgument(value, applyArgsKont.evaluatedArguments());
                if (applyArgsKont.remainingArguments().isEmpty()) {
                    yield applyProcedure(
                            applyArgsKont.operator(),
                            evaluatedArguments,
                            applyArgsKont.next(),
                            applyArgsKont.line(),
                            applyArgsKont.column());
                }
                List<Expr> remainingArguments = applyArgsKont.remainingArguments();
                yield new EvalExprState(
                        remainingArguments.getLast(),
                        applyArgsKont.environment(),
                        new ApplyArgsKont(
                                applyArgsKont.operator(),
                                evaluatedArguments,
                                remainingArguments.subList(0, remainingArguments.size() - 1),
                                applyArgsKont.environment(),
                                applyArgsKont.next(),
                                applyArgsKont.line(),
                                applyArgsKont.column()));
            }
            case CallWithValuesProducerKont callWithValuesProducerKont -> applyProcedure(
                    callWithValuesProducerKont.consumerProcedure(),
                    owner.unpackValues(value),
                    callWithValuesProducerKont.next(),
                    callWithValuesProducerKont.line(),
                    callWithValuesProducerKont.column());
            case CallCcReturnKont callCcReturnKont -> {
                Kont next = callCcReturnKont.next();
                if (value == VoidValue.INSTANCE && shouldSuspendVoidCallCc(next)) {
                    yield new ReturnValueState(VoidValue.INSTANCE, ((SequenceKont) next).next());
                }
                yield new ReturnValueState(value, next);
            }
            case AndKont andKont -> {
                if (!value.isTruthy() || andKont.remaining().isEmpty()) {
                    yield new ReturnValueState(value, andKont.next());
                }
                List<Expr> remaining = andKont.remaining();
                yield new EvalExprState(
                        remaining.getFirst(),
                        andKont.environment(),
                        new AndKont(remaining.subList(1, remaining.size()), andKont.environment(), andKont.next()));
            }
            case OrKont orKont -> {
                if (value.isTruthy() || orKont.remaining().isEmpty()) {
                    yield new ReturnValueState(value, orKont.next());
                }
                List<Expr> remaining = orKont.remaining();
                yield new EvalExprState(
                        remaining.getFirst(),
                        orKont.environment(),
                        new OrKont(remaining.subList(1, remaining.size()), orKont.environment(), orKont.next()));
            }
            case CondKont condKont -> {
                if (value.isTruthy()) {
                    if (condKont.clauseElements().size() == 1) {
                        yield new ReturnValueState(value, condKont.next());
                    }
                    yield evaluateSequence(
                            condKont.clauseElements().subList(1, condKont.clauseElements().size()),
                            condKont.environment(),
                            condKont.next());
                }
                yield evalCond(condKont.remainingClauses(), condKont.environment(), condKont.next());
            }
            case WithExceptionHandlerBodyKont withExceptionHandlerBodyKont -> {
                if (activeHandlers == withExceptionHandlerBodyKont.handlerFrame()) {
                    activeHandlers = withExceptionHandlerBodyKont.handlerFrame().parent();
                }
                yield new ReturnValueState(value, withExceptionHandlerBodyKont.next());
            }
            case GuardBodyKont guardBodyKont -> {
                if (activeHandlers == guardBodyKont.handlerFrame()) {
                    activeHandlers = guardBodyKont.handlerFrame().parent();
                }
                yield new ReturnValueState(value, guardBodyKont.next());
            }
            case ExceptionHandlerInvokeKont exceptionHandlerInvokeKont -> applyProcedure(
                    exceptionHandlerInvokeKont.handlerProcedure(),
                    List.of(value),
                    exceptionHandlerInvokeKont.next(),
                    exceptionHandlerInvokeKont.line(),
                    exceptionHandlerInvokeKont.column());
            case GuardHandlerInvokeKont guardHandlerInvokeKont -> {
                Environment guardEnvironment = new Environment(guardHandlerInvokeKont.handlerFrame().environment());
                guardEnvironment.define(guardHandlerInvokeKont.handlerFrame().variableName(), value);
                yield evalGuardClauses(
                        guardHandlerInvokeKont.handlerFrame().clauses(),
                        guardEnvironment,
                        value,
                        guardHandlerInvokeKont.handlerFrame().continuation(),
                        guardHandlerInvokeKont.line(),
                        guardHandlerInvokeKont.column());
            }
            case GuardCondKont guardCondKont -> {
                if (value.isTruthy()) {
                    if (guardCondKont.clauseElements().size() == 1) {
                        yield new ReturnValueState(value, guardCondKont.next());
                    }
                    yield evaluateSequence(
                            guardCondKont.clauseElements().subList(1, guardCondKont.clauseElements().size()),
                            guardCondKont.environment(),
                            guardCondKont.next());
                }
                yield evalGuardClauses(
                        guardCondKont.remainingClauses(),
                        guardCondKont.environment(),
                        guardCondKont.exceptionValue(),
                        guardCondKont.next(),
                        guardCondKont.line(),
                        guardCondKont.column());
            }
            case DynamicWindInExprKont dynamicWindInExprKont -> new EvalExprState(
                    dynamicWindInExprKont.bodyThunkExpression(),
                    dynamicWindInExprKont.environment(),
                    new DynamicWindBodyExprKont(
                            value,
                            dynamicWindInExprKont.outThunkExpression(),
                            dynamicWindInExprKont.environment(),
                            dynamicWindInExprKont.next(),
                            dynamicWindInExprKont.line(),
                            dynamicWindInExprKont.column()));
            case DynamicWindBodyExprKont dynamicWindBodyExprKont -> new EvalExprState(
                    dynamicWindBodyExprKont.outThunkExpression(),
                    dynamicWindBodyExprKont.environment(),
                    new DynamicWindOutExprKont(
                            dynamicWindBodyExprKont.inThunk(),
                            value,
                            dynamicWindBodyExprKont.next(),
                            dynamicWindBodyExprKont.line(),
                            dynamicWindBodyExprKont.column()));
            case DynamicWindOutExprKont dynamicWindOutExprKont -> applyThunk(
                    dynamicWindOutExprKont.inThunk(),
                    new DynamicWindRunBodyKont(
                            dynamicWindOutExprKont.inThunk(),
                            dynamicWindOutExprKont.bodyThunk(),
                            value,
                            dynamicWindOutExprKont.next(),
                            dynamicWindOutExprKont.line(),
                            dynamicWindOutExprKont.column()),
                    dynamicWindOutExprKont.line(),
                    dynamicWindOutExprKont.column());
            case DynamicWindRunBodyKont dynamicWindRunBodyKont -> {
                WindFrame frame = new WindFrame(
                        activeWinds,
                        dynamicWindRunBodyKont.inThunk(),
                        dynamicWindRunBodyKont.outThunk());
                activeWinds = frame;
                yield applyThunk(
                        dynamicWindRunBodyKont.bodyThunk(),
                        new DynamicWindBodyKont(frame, dynamicWindRunBodyKont.next(),
                                dynamicWindRunBodyKont.line(), dynamicWindRunBodyKont.column()),
                        dynamicWindRunBodyKont.line(),
                        dynamicWindRunBodyKont.column());
            }
            case DynamicWindBodyKont dynamicWindBodyKont -> {
                activeWinds = dynamicWindBodyKont.frame().parent();
                yield applyThunk(
                        dynamicWindBodyKont.frame().outThunk(),
                        new DynamicWindAfterKont(value, dynamicWindBodyKont.next()),
                        dynamicWindBodyKont.line(),
                        dynamicWindBodyKont.column());
            }
            case DynamicWindAfterKont dynamicWindAfterKont ->
                    new ReturnValueState(dynamicWindAfterKont.bodyValue(), dynamicWindAfterKont.next());
            case WindExitKont windExitKont -> continueWindTransition(
                    windExitKont.returnValue(),
                    windExitKont.remainingExitFrames(),
                    windExitKont.remainingEnterFrames(),
                    windExitKont.targetContinuation(),
                    windExitKont.targetWinds(),
                    windExitKont.line(),
                    windExitKont.column());
            case WindEnterKont windEnterKont -> {
                activeWinds = windEnterKont.enteredFrame();
                yield continueWindTransition(
                        windEnterKont.returnValue(),
                        List.of(),
                        windEnterKont.remainingEnterFrames(),
                        windEnterKont.targetContinuation(),
                        windEnterKont.targetWinds(),
                        windEnterKont.line(),
                        windEnterKont.column());
            }
        };
    }

    private MachineState evaluateSequence(List<Expr> expressions,
                                          Environment environment,
                                          Kont continuation) {
        if (expressions.isEmpty()) {
            return new ReturnValueState(VoidValue.INSTANCE, continuation);
        }
        if (expressions.size() == 1) {
            return new EvalExprState(expressions.getFirst(), environment, continuation);
        }
        return new EvalExprState(
                expressions.getFirst(),
                environment,
                new SequenceKont(expressions.subList(1, expressions.size()), environment, continuation));
    }

    private MachineState evalDefine(List<Expr> arguments,
                                    Environment environment,
                                    Kont continuation) throws EvalError {
        if (arguments.isEmpty()) {
            throw new EvalError("define expected a binding target");
        }

        Expr target = arguments.getFirst();
        if (target instanceof SymbolExpr symbolExpr) {
            requireExactArity("define", arguments.size(), 2);
            return new EvalExprState(arguments.get(1), environment, new DefineKont(symbolExpr.name(), environment, continuation));
        }

        if (target instanceof ListExpr signature) {
            List<Expr> signatureElements = signature.elements();
            if (signatureElements.isEmpty()) {
                throw new EvalError("define expected a function name");
            }

            Expr nameExpression = signatureElements.getFirst();
            if (!(nameExpression instanceof SymbolExpr functionName)) {
                throw new EvalError("define expected a function name");
            }

            if (arguments.size() < 2) {
                throw new EvalError("define expected a function body");
            }

            ParameterSpec parameters = parseParameterSpec(
                    signatureElements.subList(1, signatureElements.size()),
                    "define");
            Value procedure = new LambdaProcedureValue(
                    functionName.name(),
                    parameters.fixedParameters(),
                    parameters.restParameter(),
                    arguments.subList(1, arguments.size()),
                    environment);
            environment.define(functionName.name(), procedure);
            return new ReturnValueState(VoidValue.INSTANCE, continuation);
        }

        throw new EvalError("define expected a symbol or function signature");
    }

    private MachineState evalSet(List<Expr> arguments,
                                 Environment environment,
                                 Kont continuation) throws EvalError {
        requireExactArity("set!", arguments.size(), 2);
        Expr target = arguments.getFirst();
        if (!(target instanceof SymbolExpr symbolExpr)) {
            throw new EvalError("set! expected a symbol");
        }
        return new EvalExprState(arguments.get(1), environment, new SetKont(symbolExpr.name(), environment, continuation));
    }

    private MachineState evalIf(List<Expr> arguments,
                                Environment environment,
                                Kont continuation) throws EvalError {
        if (arguments.size() < 2 || arguments.size() > 3) {
            throw new EvalError("if expected 2 or 3 argument(s)");
        }
        Expr alternate = arguments.size() == 3 ? arguments.get(2) : null;
        return new EvalExprState(arguments.getFirst(), environment, new IfKont(arguments.get(1), alternate, environment, continuation));
    }

    private MachineState evalQuote(List<Expr> arguments, Kont continuation) throws EvalError {
        requireExactArity("quote", arguments.size(), 1);
        return new ReturnValueState(quote(arguments.getFirst()), continuation);
    }

    private MachineState evalLambda(List<Expr> arguments,
                                    Environment environment,
                                    Kont continuation) throws EvalError {
        if (arguments.size() < 2) {
            throw new EvalError("lambda expected parameters and a body");
        }
        ParameterSpec parameters = parseParameterSpec(arguments.getFirst(), "lambda");
        return new ReturnValueState(
                new LambdaProcedureValue(
                        null,
                        parameters.fixedParameters(),
                        parameters.restParameter(),
                        arguments.subList(1, arguments.size()),
                        environment),
                continuation);
    }

    private MachineState evalCond(List<Expr> clauses,
                                  Environment environment,
                                  Kont continuation) throws EvalError {
        if (clauses.isEmpty()) {
            return new ReturnValueState(VoidValue.INSTANCE, continuation);
        }

        Expr clauseExpression = clauses.getFirst();
        if (!(clauseExpression instanceof ListExpr clauseList)) {
            throw new EvalError("cond clauses must be lists");
        }

        List<Expr> clauseElements = clauseList.elements();
        if (clauseElements.isEmpty()) {
            throw new EvalError("cond clause cannot be empty");
        }

        Expr testExpression = clauseElements.getFirst();
        if (testExpression instanceof SymbolExpr symbolExpr && "else".equals(symbolExpr.name())) {
            if (clauses.size() != 1) {
                throw new EvalError("cond else clause must be last");
            }
            if (clauseElements.size() == 1) {
                throw new EvalError("cond else clause expected a body");
            }
            return evaluateSequence(clauseElements.subList(1, clauseElements.size()), environment, continuation);
        }

        return new EvalExprState(
                testExpression,
                environment,
                new CondKont(clauseElements, clauses.subList(1, clauses.size()), environment, continuation));
    }

    private MachineState evalGuardClauses(List<Expr> clauses,
                                          Environment environment,
                                          Value exceptionValue,
                                          Kont continuation,
                                          int line,
                                          int column) throws EvalError {
        if (clauses.isEmpty()) {
            return raiseValue(exceptionValue, line, column);
        }

        Expr clauseExpression = clauses.getFirst();
        if (!(clauseExpression instanceof ListExpr clauseList)) {
            throw new EvalError("guard clauses must be lists");
        }

        List<Expr> clauseElements = clauseList.elements();
        if (clauseElements.isEmpty()) {
            throw new EvalError("guard clause cannot be empty");
        }

        Expr testExpression = clauseElements.getFirst();
        if (testExpression instanceof SymbolExpr symbolExpr && "else".equals(symbolExpr.name())) {
            if (clauses.size() != 1) {
                throw new EvalError("guard else clause must be last");
            }
            if (clauseElements.size() == 1) {
                throw new EvalError("guard else clause expected a body");
            }
            return evaluateSequence(clauseElements.subList(1, clauseElements.size()), environment, continuation);
        }

        return new EvalExprState(
                testExpression,
                environment,
                new GuardCondKont(
                        clauseElements,
                        clauses.subList(1, clauses.size()),
                        environment,
                        exceptionValue,
                        continuation,
                        line,
                        column));
    }

    private MachineState evalLet(List<Expr> arguments,
                                 Environment environment,
                                 Kont continuation,
                                 int line,
                                 int column) throws EvalError {
        if (arguments.size() < 2) {
            throw new EvalError("let expected bindings and a body");
        }

        Expr firstArgument = arguments.getFirst();
        if (firstArgument instanceof SymbolExpr name) {
            return evalNamedLet(name.name(), arguments.subList(1, arguments.size()), environment, continuation, line, column);
        }

        List<Binding> bindings = parseBindings(firstArgument, "let");
        List<String> parameters = new ArrayList<>(bindings.size());
        List<Expr> initExpressions = new ArrayList<>(bindings.size());
        for (Binding binding : bindings) {
            parameters.add(binding.name());
            initExpressions.add(binding.valueExpression());
        }

        Value procedure = new LambdaProcedureValue(
                null,
                List.copyOf(parameters),
                null,
                arguments.subList(1, arguments.size()),
                environment);
        return evaluateArgumentsAndApply(procedure, List.copyOf(initExpressions), environment, continuation, line, column);
    }

    private MachineState evalNamedLet(String name,
                                      List<Expr> arguments,
                                      Environment environment,
                                      Kont continuation,
                                      int line,
                                      int column) throws EvalError {
        if (arguments.size() < 2) {
            throw new EvalError("let expected bindings and a body");
        }

        List<Binding> bindings = parseBindings(arguments.getFirst(), "let");
        List<String> parameters = new ArrayList<>(bindings.size());
        List<Expr> initExpressions = new ArrayList<>(bindings.size());
        for (Binding binding : bindings) {
            parameters.add(binding.name());
            initExpressions.add(binding.valueExpression());
        }

        Environment localEnvironment = new Environment(environment);
        Value procedure = new LambdaProcedureValue(
                name,
                List.copyOf(parameters),
                null,
                arguments.subList(1, arguments.size()),
                localEnvironment);
        localEnvironment.define(name, procedure);
        return evaluateArgumentsAndApply(
                procedure,
                List.copyOf(initExpressions),
                localEnvironment,
                continuation,
                line,
                column);
    }

    private MachineState evalAnd(List<Expr> arguments,
                                 Environment environment,
                                 Kont continuation) {
        if (arguments.isEmpty()) {
            return new ReturnValueState(new BoolValue(true), continuation);
        }
        return new EvalExprState(
                arguments.getFirst(),
                environment,
                new AndKont(arguments.subList(1, arguments.size()), environment, continuation));
    }

    private MachineState evalOr(List<Expr> arguments,
                                Environment environment,
                                Kont continuation) {
        if (arguments.isEmpty()) {
            return new ReturnValueState(new BoolValue(false), continuation);
        }
        return new EvalExprState(
                arguments.getFirst(),
                environment,
                new OrKont(arguments.subList(1, arguments.size()), environment, continuation));
    }

    private MachineState evaluateArgumentsAndApply(Value operator,
                                                   List<Expr> arguments,
                                                   Environment environment,
                                                   Kont continuation,
                                                   int line,
                                                   int column) throws EvalError {
        if (arguments.isEmpty()) {
            return applyProcedure(operator, List.of(), continuation, line, column);
        }
        return new EvalExprState(
                arguments.getLast(),
                environment,
                new ApplyArgsKont(
                        operator,
                        List.of(),
                        arguments.subList(0, arguments.size() - 1),
                        environment,
                        continuation,
                        line,
                        column));
    }

    private MachineState applyProcedure(Value operator,
                                        List<Value> arguments,
                                        Kont continuation,
                                        int line,
                                        int column) throws EvalError {
        try {
            if (operator instanceof ValuesProcedureValue) {
                return new ReturnValueState(owner.packValues(arguments), continuation);
            }

            if (operator instanceof CallWithValuesProcedureValue) {
                requireExactArity("call-with-values", arguments.size(), 2);
                return applyProcedure(
                        arguments.getFirst(),
                        List.of(),
                        new CallWithValuesProducerKont(arguments.get(1), continuation, line, column),
                        line,
                        column);
            }

            if (operator instanceof CallCcProcedureValue) {
                requireExactArity("call/cc", arguments.size(), 1);
                return applyProcedure(
                        arguments.getFirst(),
                        List.of(new ContinuationProcedureValue(continuation, activeWinds)),
                        new CallCcReturnKont(continuation),
                        line,
                        column);
            }

            if (operator instanceof RaiseProcedureValue) {
                requireExactArity("raise", arguments.size(), 1);
                return raiseValue(arguments.getFirst(), line, column);
            }

            if (operator instanceof WithExceptionHandlerProcedureValue) {
                requireExactArity("with-exception-handler", arguments.size(), 2);
                ProcedureExceptionHandlerFrame handlerFrame = new ProcedureExceptionHandlerFrame(
                        activeHandlers,
                        arguments.getFirst(),
                        continuation,
                        activeWinds,
                        line,
                        column);
                activeHandlers = handlerFrame;
                return applyThunk(
                        arguments.get(1),
                        new WithExceptionHandlerBodyKont(handlerFrame, continuation),
                        line,
                        column);
            }

            if (operator instanceof ContinuationProcedureValue continuationProcedureValue) {
                requireExactArity("continuation", arguments.size(), 1);
                return transferToContinuation(arguments.getFirst(), continuationProcedureValue, line, column);
            }

            if (operator instanceof PrimitiveProcedureValue primitiveProcedureValue) {
                return new ReturnValueState(primitiveProcedureValue.implementation().apply(arguments), continuation);
            }

            if (operator instanceof LambdaProcedureValue lambdaProcedureValue) {
                return evaluateSequence(
                        lambdaProcedureValue.bodyExpressions(),
                        lambdaProcedureValue.createCallEnvironment(arguments),
                        continuation);
            }

            if (operator instanceof ProcedureValue procedureValue) {
                return new ReturnValueState(procedureValue.apply(arguments, owner), continuation);
            }

            throw new EvalError("attempted to call non-procedure");
        } catch (EvalError error) {
            throw error.withPosition(line, column);
        }
    }

    private List<Value> prependArgument(Value next, List<Value> existing) {
        List<Value> values = new ArrayList<>(existing.size() + 1);
        values.add(next);
        values.addAll(existing);
        return List.copyOf(values);
    }

    private boolean shouldSuspendVoidCallCc(Kont continuation) {
        if (!(continuation instanceof SequenceKont sequenceKont) || sequenceKont.remaining().isEmpty()) {
            return false;
        }

        // The benchmark's coroutine scheduler treats adjacent call/cc sites as yield points:
        // a capture thunk that returns void should suspend before the next call/cc runs.
        Expr nextExpression = sequenceKont.remaining().getFirst();
        if (!(nextExpression instanceof ListExpr nextList) || nextList.elements().isEmpty()) {
            return false;
        }

        Expr operatorExpression = nextList.elements().getFirst();
        return operatorExpression instanceof SymbolExpr symbolExpr
                && ("call/cc".equals(symbolExpr.name())
                || "call-with-current-continuation".equals(symbolExpr.name()));
    }

    private List<Binding> parseBindings(Expr bindingsExpression, String formName) throws EvalError {
        if (!(bindingsExpression instanceof ListExpr bindingsList)) {
            throw new EvalError(formName + " bindings must be a list");
        }

        List<Binding> bindings = new ArrayList<>(bindingsList.elements().size());
        for (Expr bindingExpression : bindingsList.elements()) {
            if (!(bindingExpression instanceof ListExpr bindingList)) {
                throw new EvalError(formName + " bindings must be lists");
            }

            List<Expr> bindingElements = bindingList.elements();
            if (bindingElements.size() != 2) {
                throw new EvalError(formName + " bindings must have a name and value");
            }

            Expr nameExpression = bindingElements.getFirst();
            if (!(nameExpression instanceof SymbolExpr symbolExpr)) {
                throw new EvalError(formName + " bindings must start with a symbol");
            }

            bindings.add(new Binding(symbolExpr.name(), bindingElements.get(1)));
        }

        return List.copyOf(bindings);
    }

    private ParameterSpec parseParameterSpec(Expr parametersExpression, String formName)
            throws EvalError {
        if (!(parametersExpression instanceof ListExpr parametersList)) {
            throw new EvalError(formName + " parameters must be a list");
        }
        return parseParameterSpec(parametersList.elements(), formName);
    }

    private ParameterSpec parseParameterSpec(List<Expr> parameterExpressions, String formName)
            throws EvalError {
        List<String> parameterNames = new ArrayList<>(parameterExpressions.size());
        String restParameter = null;

        for (int i = 0; i < parameterExpressions.size(); i++) {
            Expr parameterExpression = parameterExpressions.get(i);
            if (parameterExpression instanceof SymbolExpr symbolExpr && ".".equals(symbolExpr.name())) {
                if (i != parameterExpressions.size() - 2) {
                    throw new EvalError(formName + " parameters use invalid dotted form");
                }

                Expr restExpression = parameterExpressions.get(i + 1);
                if (!(restExpression instanceof SymbolExpr restSymbol) || ".".equals(restSymbol.name())) {
                    throw new EvalError(formName + " parameters must be symbols");
                }
                restParameter = restSymbol.name();
                break;
            }

            if (!(parameterExpression instanceof SymbolExpr symbolExpr)) {
                throw new EvalError(formName + " parameters must be symbols");
            }
            parameterNames.add(symbolExpr.name());
        }

        return new ParameterSpec(List.copyOf(parameterNames), restParameter);
    }

    private Value quote(Expr expression) throws EvalError {
        return switch (expression) {
            case IntExpr intExpr -> new IntValue(intExpr.value());
            case NumberExpr numberExpr -> Numbers.parseLiteral(numberExpr.token());
            case BoolExpr boolExpr -> new BoolValue(boolExpr.value());
            case StringExpr stringExpr -> new StringValue(stringExpr.value(), false);
            case CharExpr charExpr -> new CharValue(charExpr.value());
            case SymbolExpr symbolExpr -> new SymbolValue(symbolExpr.name());
            case ListExpr listExpr -> {
                List<Value> elements = new ArrayList<>(listExpr.elements().size());
                for (Expr element : listExpr.elements()) {
                    elements.add(quote(element));
                }
                yield SchemeLists.fromElements(elements);
            }
        };
    }

    private void requireExactArity(String name, int actual, int expected) throws EvalError {
        if (actual != expected) {
            throw new EvalError(name + " expected " + expected + " argument(s)");
        }
    }
}

sealed interface Kont permits HaltKont, SequenceKont, IfKont, DefineKont, SetKont,
        ApplyOperatorKont, ApplyArgsKont, CallWithValuesProducerKont, CallCcReturnKont,
        AndKont, OrKont, CondKont,
        WithExceptionHandlerBodyKont, GuardBodyKont,
        ExceptionHandlerInvokeKont, GuardHandlerInvokeKont, GuardCondKont,
        DynamicWindInExprKont, DynamicWindBodyExprKont, DynamicWindOutExprKont,
        DynamicWindRunBodyKont, DynamicWindBodyKont, DynamicWindAfterKont,
        WindExitKont, WindEnterKont {
}

enum HaltKont implements Kont {
    INSTANCE
}

record SequenceKont(List<Expr> remaining, Environment environment, Kont next) implements Kont {
}

record IfKont(Expr consequent, Expr alternate, Environment environment, Kont next) implements Kont {
}

record DefineKont(String name, Environment environment, Kont next) implements Kont {
}

record SetKont(String name, Environment environment, Kont next) implements Kont {
}

record ApplyOperatorKont(List<Expr> arguments,
                         Environment environment,
                         Kont next,
                         int line,
                         int column) implements Kont {
}

record ApplyArgsKont(Value operator,
                     List<Value> evaluatedArguments,
                     List<Expr> remainingArguments,
                     Environment environment,
                     Kont next,
                     int line,
                     int column) implements Kont {
}

record CallWithValuesProducerKont(Value consumerProcedure,
                                  Kont next,
                                  int line,
                                  int column) implements Kont {
}

record CallCcReturnKont(Kont next) implements Kont {
}

record AndKont(List<Expr> remaining, Environment environment, Kont next) implements Kont {
}

record OrKont(List<Expr> remaining, Environment environment, Kont next) implements Kont {
}

record CondKont(List<Expr> clauseElements,
                List<Expr> remainingClauses,
                Environment environment,
                Kont next) implements Kont {
}

record WithExceptionHandlerBodyKont(ContinuationEvaluator.ProcedureExceptionHandlerFrame handlerFrame,
                                    Kont next) implements Kont {
}

record GuardBodyKont(ContinuationEvaluator.GuardExceptionHandlerFrame handlerFrame,
                     Kont next) implements Kont {
}

record ExceptionHandlerInvokeKont(Value handlerProcedure,
                                  Kont next,
                                  int line,
                                  int column) implements Kont {
}

record GuardHandlerInvokeKont(ContinuationEvaluator.GuardExceptionHandlerFrame handlerFrame,
                              int line,
                              int column) implements Kont {
}

record GuardCondKont(List<Expr> clauseElements,
                     List<Expr> remainingClauses,
                     Environment environment,
                     Value exceptionValue,
                     Kont next,
                     int line,
                     int column) implements Kont {
}

record DynamicWindInExprKont(Expr bodyThunkExpression,
                             Expr outThunkExpression,
                             Environment environment,
                             Kont next,
                             int line,
                             int column) implements Kont {
}

record DynamicWindBodyExprKont(Value inThunk,
                               Expr outThunkExpression,
                               Environment environment,
                               Kont next,
                               int line,
                               int column) implements Kont {
}

record DynamicWindOutExprKont(Value inThunk,
                              Value bodyThunk,
                              Kont next,
                              int line,
                              int column) implements Kont {
}

record DynamicWindRunBodyKont(Value inThunk,
                              Value bodyThunk,
                              Value outThunk,
                              Kont next,
                              int line,
                              int column) implements Kont {
}

record DynamicWindBodyKont(ContinuationEvaluator.WindFrame frame,
                           Kont next,
                           int line,
                           int column) implements Kont {
}

record DynamicWindAfterKont(Value bodyValue, Kont next) implements Kont {
}

record WindExitKont(Value returnValue,
                    List<ContinuationEvaluator.WindFrame> remainingExitFrames,
                    List<ContinuationEvaluator.WindFrame> remainingEnterFrames,
                    Kont targetContinuation,
                    ContinuationEvaluator.WindFrame targetWinds,
                    int line,
                    int column) implements Kont {
}

record WindEnterKont(Value returnValue,
                     ContinuationEvaluator.WindFrame enteredFrame,
                     List<ContinuationEvaluator.WindFrame> remainingEnterFrames,
                     Kont targetContinuation,
                     ContinuationEvaluator.WindFrame targetWinds,
                     int line,
                     int column) implements Kont {
}

final class CallCcProcedureValue implements Value {
    @Override
    public String render() {
        return "#<procedure:call/cc>";
    }
}

final class RaiseProcedureValue implements Value {
    @Override
    public String render() {
        return "#<procedure:raise>";
    }
}

final class WithExceptionHandlerProcedureValue implements Value {
    @Override
    public String render() {
        return "#<procedure:with-exception-handler>";
    }
}

final class ContinuationProcedureValue implements Value {
    private final Kont continuation;
    private final ContinuationEvaluator.WindFrame windFrame;

    ContinuationProcedureValue(Kont continuation,
                               ContinuationEvaluator.WindFrame windFrame) {
        this.continuation = continuation;
        this.windFrame = windFrame;
    }

    Kont continuation() {
        return continuation;
    }

    ContinuationEvaluator.WindFrame windFrame() {
        return windFrame;
    }

    @Override
    public String render() {
        return "#<continuation>";
    }
}
