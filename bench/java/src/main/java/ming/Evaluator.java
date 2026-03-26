package ming;

import java.util.ArrayList;
import java.util.HashSet;
import java.util.HashMap;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.Set;
import java.util.function.Predicate;

/**
 * Scheme interpreter entry point.
 * Agents implement this class.
 */
public class Evaluator {
    private final Map<String, BuiltinProcedure> builtins = createBuiltins();
    private StringBuilder outputBuffer;
    private ContinuationContext currentContinuation;
    private List<WindFrame> currentWinds = List.of();

    /**
     * Evaluate one or more Scheme expressions and return the string
     * representation of the last result.
     */
    public String evalStr(String input) throws EvalError {
        return evaluateProgram(input, false).result();
    }

    /**
     * Evaluate Scheme expressions and return both the result string
     * and any captured output from display/write/newline.
     */
    public EvalResult evalStrWithOutput(String input) throws EvalError {
        return evaluateProgram(input, true);
    }

    private EvalResult evaluateProgram(String input, boolean captureOutput) throws EvalError {
        List<SchemeExpression> expressions = new SchemeParser(input).parseProgram();
        if (expressions.isEmpty()) {
            throw new EvalError("empty input");
        }

        StringBuilder previousOutputBuffer = outputBuffer;
        outputBuffer = captureOutput ? new StringBuilder() : null;
        try {
            Environment environment = createTopLevelEnvironment();
            SchemeValue result;
            try {
                result = runWithContinuations(
                        () -> evalSequenceToValue(expressions, 0, environment, VoidValue.INSTANCE)
                );
            } catch (RaisedException raised) {
                throw new EvalError("uncaught exception: " + raised.value().render());
            }
            result = requireSingleValue(result);
            String output = outputBuffer == null ? "" : outputBuffer.toString();
            return new EvalResult(result.render(), output);
        } finally {
            outputBuffer = previousOutputBuffer;
        }
    }

    private SchemeValue eval(SchemeExpression expression, Environment environment) throws EvalError {
        SchemeExpression currentExpression = expression;
        Environment currentEnvironment = environment;
        while (true) {
            try {
                if (currentExpression instanceof LiteralExpression literal) {
                    return literal.value();
                }

                if (currentExpression instanceof SymbolExpression symbol) {
                    return currentEnvironment.lookup(symbol.name());
                }

                TailStep step = evalListTail((ListExpression) currentExpression, currentEnvironment);
                if (step.isDone()) {
                    return step.value();
                }

                currentExpression = step.nextExpression();
                currentEnvironment = step.nextEnvironment();
            } catch (EvalError error) {
                throw error.withPosition(currentExpression.position());
            }
        }
    }

    private SchemeValue evalNonTail(SchemeExpression expression, Environment environment) throws EvalError {
        try {
            SchemeValue value;
            if (expression instanceof LiteralExpression literal) {
                value = literal.value();
            } else if (expression instanceof SymbolExpression symbol) {
                value = environment.lookup(symbol.name());
            } else {
                value = evalListNonTail((ListExpression) expression, environment);
            }
            return requireSingleValue(value);
        } catch (EvalError error) {
            throw error.withPosition(expression.position());
        }
    }

    private SchemeValue runWithContinuations(RootComputation computation) throws EvalError {
        RootComputation currentComputation = computation;
        ContinuationContext previousContinuation = currentContinuation;
        List<WindFrame> previousWinds = currentWinds;
        currentContinuation = null;
        currentWinds = List.of();
        try {
            while (true) {
                try {
                    return currentComputation.run();
                } catch (ContinuationJump jump) {
                    currentComputation = () -> resumeContinuation(jump);
                }
            }
        } finally {
            currentContinuation = previousContinuation;
            currentWinds = previousWinds;
        }
    }

    private SchemeValue resumeContinuation(ContinuationJump jump) throws EvalError {
        transitionWinds(jump.sourceWinds(), jump.targetWinds());

        SchemeValue resumedValue = jump.value();
        ContinuationContext context = jump.continuation();
        while (context != null) {
            ContinuationContext activeContext = context;
            SchemeValue inputValue = resumedValue;
            resumedValue = withActiveContinuation(
                    activeContext.parent(),
                    () -> activeContext.frame().resume(inputValue)
            );
            context = activeContext.parent();
        }
        return resumedValue;
    }

    private SchemeValue evalNonTailWithContinuation(
            SchemeExpression expression,
            Environment environment,
            ContinuationFrame frame
    ) throws EvalError {
        ContinuationContext savedContinuation = currentContinuation;
        currentContinuation = new ContinuationContext(
                value -> frame.resume(requireSingleValue(value)),
                savedContinuation
        );
        try {
            return evalNonTail(expression, environment);
        } finally {
            currentContinuation = savedContinuation;
        }
    }

    private SchemeValue withActiveContinuation(ContinuationContext continuation, RootComputation computation)
            throws EvalError {
        ContinuationContext previousContinuation = currentContinuation;
        currentContinuation = continuation;
        try {
            return computation.run();
        } finally {
            currentContinuation = previousContinuation;
        }
    }

    private SchemeValue withActiveWinds(List<WindFrame> winds, RootComputation computation) throws EvalError {
        List<WindFrame> previousWinds = currentWinds;
        currentWinds = winds;
        try {
            return computation.run();
        } finally {
            currentWinds = previousWinds;
        }
    }

    private void transitionWinds(List<WindFrame> source, List<WindFrame> target) throws EvalError {
        int shared = sharedWindPrefix(source, target);

        for (int index = source.size() - 1; index >= shared; index--) {
            WindFrame frame = source.get(index);
            List<WindFrame> activeWinds = copyWindPrefix(source, index);
            withActiveWinds(activeWinds, () -> applyThunk(frame.outThunk()));
        }

        for (int index = shared; index < target.size(); index++) {
            WindFrame frame = target.get(index);
            List<WindFrame> activeWinds = copyWindPrefix(target, index);
            withActiveWinds(activeWinds, () -> applyThunk(frame.inThunk()));
        }

        currentWinds = List.copyOf(target);
    }

    private TailStep evalListTail(ListExpression expression, Environment environment) throws EvalError {
        List<SchemeExpression> elements = expression.elements();
        if (elements.isEmpty()) {
            throw new EvalError("cannot evaluate an empty list");
        }

        SchemeExpression head = elements.getFirst();
        if (head instanceof SymbolExpression symbol) {
            String name = symbol.name();
            if ("and".equals(name)) {
                return evalAndTail(elements.subList(1, elements.size()), environment);
            }
            if ("or".equals(name)) {
                return evalOrTail(elements.subList(1, elements.size()), environment);
            }
            if ("define".equals(name)) {
                return TailStep.done(evalDefine(elements, environment));
            }
            if ("define-syntax".equals(name)) {
                return TailStep.done(evalDefineSyntax(elements, environment));
            }
            if ("define-record-type".equals(name)) {
                return TailStep.done(evalDefineRecordType(elements, environment));
            }
            if ("set!".equals(name)) {
                return TailStep.done(evalSet(elements, environment));
            }
            if ("if".equals(name)) {
                return evalIfTail(elements, environment);
            }
            if ("begin".equals(name)) {
                return tailSequence(elements.subList(1, elements.size()), environment, VoidValue.INSTANCE);
            }
            if ("cond".equals(name)) {
                return evalCondTail(elements.subList(1, elements.size()), environment);
            }
            if ("let".equals(name)) {
                return evalLetTail(elements, environment);
            }
            if ("let*".equals(name)) {
                return evalLetStarTail(elements, environment);
            }
            if ("letrec".equals(name)) {
                return evalLetrecTail(elements, environment, false);
            }
            if ("letrec*".equals(name)) {
                return evalLetrecTail(elements, environment, true);
            }
            if ("quote".equals(name)) {
                return TailStep.done(evalQuote(elements));
            }
            if ("case".equals(name)) {
                return evalCaseTail(elements, environment);
            }
            if ("lambda".equals(name)) {
                return TailStep.done(evalLambda(elements, environment));
            }
            if ("case-lambda".equals(name)) {
                return TailStep.done(evalCaseLambda(elements, environment));
            }
            if ("guard".equals(name)) {
                return TailStep.done(evalGuard(elements, environment));
            }
            if ("do".equals(name)) {
                return TailStep.done(evalDo(elements, environment));
            }

            SyntaxRulesMacro macro = environment.lookupMacro(name);
            if (macro != null) {
                return TailStep.next(macro.expand(expression), environment);
            }
        }

        return evalTailApplication(elements, environment);
    }

    private SchemeValue evalListNonTail(ListExpression expression, Environment environment) throws EvalError {
        List<SchemeExpression> elements = expression.elements();
        if (elements.isEmpty()) {
            throw new EvalError("cannot evaluate an empty list");
        }

        SchemeExpression head = elements.getFirst();
        if (head instanceof SymbolExpression symbol) {
            String name = symbol.name();
            if ("and".equals(name)) {
                return evalAnd(elements.subList(1, elements.size()), environment);
            }
            if ("or".equals(name)) {
                return evalOr(elements.subList(1, elements.size()), environment);
            }
            if ("define".equals(name)) {
                return evalDefine(elements, environment);
            }
            if ("define-syntax".equals(name)) {
                return evalDefineSyntax(elements, environment);
            }
            if ("define-record-type".equals(name)) {
                return evalDefineRecordType(elements, environment);
            }
            if ("set!".equals(name)) {
                return evalSet(elements, environment);
            }
            if ("if".equals(name)) {
                return evalIf(elements, environment);
            }
            if ("begin".equals(name)) {
                return evalBegin(elements.subList(1, elements.size()), environment);
            }
            if ("cond".equals(name)) {
                return evalCond(elements.subList(1, elements.size()), environment);
            }
            if ("let".equals(name)) {
                return evalLet(elements, environment);
            }
            if ("let*".equals(name)) {
                return evalLetStar(elements, environment);
            }
            if ("letrec".equals(name)) {
                return evalLetrec(elements, environment, false);
            }
            if ("letrec*".equals(name)) {
                return evalLetrec(elements, environment, true);
            }
            if ("quote".equals(name)) {
                return evalQuote(elements);
            }
            if ("case".equals(name)) {
                return evalCase(elements, environment);
            }
            if ("lambda".equals(name)) {
                return evalLambda(elements, environment);
            }
            if ("case-lambda".equals(name)) {
                return evalCaseLambda(elements, environment);
            }
            if ("guard".equals(name)) {
                return evalGuard(elements, environment);
            }
            if ("do".equals(name)) {
                return evalDo(elements, environment);
            }

            SyntaxRulesMacro macro = environment.lookupMacro(name);
            if (macro != null) {
                return evalNonTail(macro.expand(expression), environment);
            }
        }

        return evalApplication(elements, environment);
    }

    private SchemeValue evalGuard(List<SchemeExpression> elements, Environment environment) throws EvalError {
        if (elements.size() < 3) {
            throw new EvalError("guard: expected clauses and body");
        }
        if (!(elements.get(1) instanceof ListExpression guardExpression)) {
            throw new EvalError("guard: expected exception variable and clauses");
        }

        List<SchemeExpression> guardElements = guardExpression.elements();
        if (guardElements.isEmpty()) {
            throw new EvalError("guard: expected exception variable");
        }
        if (!(guardElements.getFirst() instanceof SymbolExpression exceptionVariable)) {
            throw new EvalError("guard: expected exception variable");
        }

        try {
            return evalSequence(elements.subList(2, elements.size()), environment);
        } catch (RaisedException raised) {
            return evalGuardClauses(
                    exceptionVariable.name(),
                    guardElements.subList(1, guardElements.size()),
                    environment,
                    raised.value()
            );
        }
    }

    private SchemeValue evalAnd(List<SchemeExpression> expressions, Environment environment) throws EvalError {
        SchemeValue result = BoolValue.TRUE;
        for (SchemeExpression expression : expressions) {
            result = evalNonTail(expression, environment);
            if (!isTruthy(result)) {
                return result;
            }
        }
        return result;
    }

    private SchemeValue evalOr(List<SchemeExpression> expressions, Environment environment) throws EvalError {
        SchemeValue result = BoolValue.FALSE;
        for (SchemeExpression expression : expressions) {
            result = evalNonTail(expression, environment);
            if (isTruthy(result)) {
                return result;
            }
        }
        return result;
    }

    private SchemeValue evalApplication(List<SchemeExpression> elements, Environment environment) throws EvalError {
        SchemeValue callee = evalNonTailWithContinuation(
                elements.getFirst(),
                environment,
                value -> continueApplication(elements, environment, value, elements.size() - 1, List.of())
        );
        return continueApplication(elements, environment, callee, elements.size() - 1, List.of());
    }

    private SchemeValue continueApplication(
            List<SchemeExpression> elements,
            Environment environment,
            SchemeValue callee,
            int argumentIndex,
            List<SchemeValue> evaluatedSuffix
    ) throws EvalError {
        if (!(callee instanceof ProcedureValue)) {
            throw new EvalError("not a procedure");
        }

        List<SchemeValue> arguments = new ArrayList<>(evaluatedSuffix);
        for (int index = argumentIndex; index >= 1; index--) {
            int nextIndex = index - 1;
            List<SchemeValue> suffix = List.copyOf(arguments);
            SchemeValue argument = evalNonTailWithContinuation(
                    elements.get(index),
                    environment,
                    value -> continueApplication(elements, environment, callee, nextIndex, prependArgument(value, suffix))
            );
            arguments.add(0, argument);
        }
        return applyProcedure(callee, arguments);
    }

    private TailStep evalTailApplication(List<SchemeExpression> elements, Environment environment) throws EvalError {
        SchemeValue callee = evalNonTailWithContinuation(
                elements.getFirst(),
                environment,
                value -> continueTailApplicationToValue(elements, environment, value, elements.size() - 1, List.of())
        );
        return continueTailApplication(elements, environment, callee, elements.size() - 1, List.of());
    }

    private TailStep continueTailApplication(
            List<SchemeExpression> elements,
            Environment environment,
            SchemeValue callee,
            int argumentIndex,
            List<SchemeValue> evaluatedSuffix
    ) throws EvalError {
        if (!(callee instanceof ProcedureValue)) {
            throw new EvalError("not a procedure");
        }

        List<SchemeValue> arguments = new ArrayList<>(evaluatedSuffix);
        for (int index = argumentIndex; index >= 1; index--) {
            int nextIndex = index - 1;
            List<SchemeValue> suffix = List.copyOf(arguments);
            SchemeValue argument = evalNonTailWithContinuation(
                    elements.get(index),
                    environment,
                    value -> continueTailApplicationToValue(
                            elements,
                            environment,
                            callee,
                            nextIndex,
                            prependArgument(value, suffix)
                    )
            );
            arguments.add(0, argument);
        }
        return applyProcedureTail(callee, arguments);
    }

    private SchemeValue continueTailApplicationToValue(
            List<SchemeExpression> elements,
            Environment environment,
            SchemeValue callee,
            int argumentIndex,
            List<SchemeValue> evaluatedSuffix
    ) throws EvalError {
        if (!(callee instanceof ProcedureValue)) {
            throw new EvalError("not a procedure");
        }

        List<SchemeValue> arguments = new ArrayList<>(evaluatedSuffix);
        for (int index = argumentIndex; index >= 1; index--) {
            int nextIndex = index - 1;
            List<SchemeValue> suffix = List.copyOf(arguments);
            SchemeValue argument = evalNonTailWithContinuation(
                    elements.get(index),
                    environment,
                    value -> continueTailApplicationToValue(
                            elements,
                            environment,
                            callee,
                            nextIndex,
                            prependArgument(value, suffix)
                    )
            );
            arguments.add(0, argument);
        }
        return applyProcedureTailToValue(callee, arguments);
    }

    private static List<SchemeValue> prependArgument(SchemeValue value, List<SchemeValue> arguments) {
        List<SchemeValue> values = new ArrayList<>(arguments.size() + 1);
        values.add(value);
        values.addAll(arguments);
        return values;
    }

    private SchemeValue evalDefine(List<SchemeExpression> elements, Environment environment) throws EvalError {
        if (elements.size() < 3) {
            throw new EvalError("define: expected a name and value");
        }

        SchemeExpression target = elements.get(1);
        if (target instanceof SymbolExpression symbol) {
            if (elements.size() != 3) {
                throw new EvalError("define: expected exactly 2 arguments for variable definition");
            }

            SchemeValue value = evalNonTailWithContinuation(
                    elements.get(2),
                    environment,
                    result -> defineVariable(environment, symbol.name(), result)
            );
            return defineVariable(environment, symbol.name(), value);
        }

        if (target instanceof ListExpression signature) {
            return evalFunctionDefine(signature, elements.subList(2, elements.size()), environment);
        }

        throw new EvalError("define: invalid definition target");
    }

    private SchemeValue evalSet(List<SchemeExpression> elements, Environment environment) throws EvalError {
        if (elements.size() != 3) {
            throw new EvalError("set!: expected a variable and value");
        }
        if (!(elements.get(1) instanceof SymbolExpression symbol)) {
            throw new EvalError("set!: expected variable name");
        }

        SchemeValue value = evalNonTailWithContinuation(
                elements.get(2),
                environment,
                result -> setVariable(environment, symbol.name(), result)
        );
        return setVariable(environment, symbol.name(), value);
    }

    private SchemeValue defineVariable(Environment environment, String name, SchemeValue value) {
        environment.define(name, value);
        return VoidValue.INSTANCE;
    }

    private SchemeValue setVariable(Environment environment, String name, SchemeValue value) throws EvalError {
        environment.set(name, value);
        return VoidValue.INSTANCE;
    }

    private SchemeValue evalDefineSyntax(List<SchemeExpression> elements, Environment environment) throws EvalError {
        if (elements.size() != 3) {
            throw new EvalError("define-syntax: expected a name and transformer");
        }
        if (!(elements.get(1) instanceof SymbolExpression symbol)) {
            throw new EvalError("define-syntax: expected macro name");
        }

        environment.defineMacro(symbol.name(), parseSyntaxRules(symbol.name(), elements.get(2), environment));
        return VoidValue.INSTANCE;
    }

    private SchemeValue evalDefineRecordType(List<SchemeExpression> elements, Environment environment) throws EvalError {
        if (elements.size() < 4) {
            throw new EvalError("define-record-type: expected type name, constructor, predicate, and fields");
        }
        if (!(elements.get(1) instanceof SymbolExpression typeName)) {
            throw new EvalError("define-record-type: expected type name");
        }
        if (!(elements.get(2) instanceof ListExpression constructorExpression)) {
            throw new EvalError("define-record-type: expected constructor clause");
        }
        if (!(elements.get(3) instanceof SymbolExpression predicateName)) {
            throw new EvalError("define-record-type: expected predicate name");
        }

        List<String> fieldNames = new ArrayList<>(Math.max(0, elements.size() - 4));
        List<RecordAccessorSpec> accessors = new ArrayList<>(Math.max(0, elements.size() - 4));
        Set<String> seenFields = new HashSet<>();
        for (int index = 4; index < elements.size(); index++) {
            SchemeExpression fieldExpression = elements.get(index);
            if (!(fieldExpression instanceof ListExpression fieldClause)) {
                throw new EvalError("define-record-type: expected field clause");
            }

            List<SchemeExpression> fieldElements = fieldClause.elements();
            if (fieldElements.size() != 2) {
                throw new EvalError("define-record-type: expected field and accessor names");
            }
            if (!(fieldElements.get(0) instanceof SymbolExpression fieldName)) {
                throw new EvalError("define-record-type: expected field name");
            }
            if (!(fieldElements.get(1) instanceof SymbolExpression accessorName)) {
                throw new EvalError("define-record-type: expected accessor name");
            }
            if (!seenFields.add(fieldName.name())) {
                throw new EvalError("define-record-type: duplicate field " + fieldName.name());
            }

            fieldNames.add(fieldName.name());
            accessors.add(new RecordAccessorSpec(fieldName.name(), accessorName.name()));
        }

        RecordType recordType = new RecordType(typeName.name(), fieldNames);

        List<SchemeExpression> constructorElements = constructorExpression.elements();
        if (constructorElements.isEmpty()) {
            throw new EvalError("define-record-type: expected constructor name");
        }
        if (!(constructorElements.getFirst() instanceof SymbolExpression constructorName)) {
            throw new EvalError("define-record-type: expected constructor name");
        }

        List<Integer> constructorFieldIndexes = new ArrayList<>(Math.max(0, constructorElements.size() - 1));
        Set<String> seenConstructorFields = new HashSet<>();
        for (int index = 1; index < constructorElements.size(); index++) {
            if (!(constructorElements.get(index) instanceof SymbolExpression constructorField)) {
                throw new EvalError("define-record-type: expected constructor field name");
            }
            if (!seenConstructorFields.add(constructorField.name())) {
                throw new EvalError("define-record-type: duplicate constructor field " + constructorField.name());
            }
            constructorFieldIndexes.add(recordType.requireFieldIndex(constructorField.name()));
        }
        if (constructorFieldIndexes.size() != recordType.fieldCount()) {
            throw new EvalError("define-record-type: constructor must initialize all fields");
        }

        environment.define(
                constructorName.name(),
                new BuiltinProcedure(
                        constructorName.name(),
                        arguments -> applyRecordConstructor(recordType, constructorFieldIndexes, arguments, constructorName.name())
                )
        );
        environment.define(
                predicateName.name(),
                new BuiltinProcedure(
                        predicateName.name(),
                        arguments -> applyRecordPredicate(recordType, arguments, predicateName.name())
                )
        );
        for (RecordAccessorSpec accessor : accessors) {
            int fieldIndex = recordType.requireFieldIndex(accessor.fieldName());
            environment.define(
                    accessor.accessorName(),
                    new BuiltinProcedure(
                            accessor.accessorName(),
                            arguments -> applyRecordAccessor(recordType, fieldIndex, arguments, accessor.accessorName())
                    )
            );
        }
        return VoidValue.INSTANCE;
    }

    private Map<String, BuiltinProcedure> createBuiltins() {
        Map<String, BuiltinProcedure> builtins = new HashMap<>();
        builtins.put("+", new BuiltinProcedure("+", Evaluator::applyAdd));
        builtins.put("-", new BuiltinProcedure("-", Evaluator::applySubtract));
        builtins.put("*", new BuiltinProcedure("*", Evaluator::applyMultiply));
        builtins.put("/", new BuiltinProcedure("/", Evaluator::applyDivide));
        builtins.put("<", new BuiltinProcedure("<", args -> compare(args, "<", ordering -> ordering < 0)));
        builtins.put(">", new BuiltinProcedure(">", args -> compare(args, ">", ordering -> ordering > 0)));
        builtins.put("=", new BuiltinProcedure("=", args -> compare(args, "=", ordering -> ordering == 0)));
        builtins.put("<=", new BuiltinProcedure("<=", args -> compare(args, "<=", ordering -> ordering <= 0)));
        builtins.put(">=", new BuiltinProcedure(">=", args -> compare(args, ">=", ordering -> ordering >= 0)));
        builtins.put("not", new BuiltinProcedure("not", Evaluator::applyNot));
        builtins.put("cons", new BuiltinProcedure("cons", Evaluator::applyCons));
        builtins.put("car", new BuiltinProcedure("car", Evaluator::applyCar));
        builtins.put("cdr", new BuiltinProcedure("cdr", Evaluator::applyCdr));
        builtins.put("cddr", new BuiltinProcedure("cddr", Evaluator::applyCddr));
        builtins.put("set-car!", new BuiltinProcedure("set-car!", Evaluator::applySetCar));
        builtins.put("set-cdr!", new BuiltinProcedure("set-cdr!", Evaluator::applySetCdr));
        builtins.put("null?", new BuiltinProcedure("null?", args -> applyPredicate(args, "null?",
                value -> value instanceof EmptyListValue)));
        builtins.put("list", new BuiltinProcedure("list", Evaluator::applyList));
        builtins.put("length", new BuiltinProcedure("length", Evaluator::applyLength));
        builtins.put("append", new BuiltinProcedure("append", Evaluator::applyAppend));
        builtins.put("reverse", new BuiltinProcedure("reverse", Evaluator::applyReverse));
        builtins.put("display", new BuiltinProcedure("display", this::applyDisplay));
        builtins.put("write", new BuiltinProcedure("write", this::applyWrite));
        builtins.put("newline", new BuiltinProcedure("newline", this::applyNewline));
        builtins.put("apply", new BuiltinProcedure("apply", this::applyApply));
        builtins.put("map", new BuiltinProcedure("map", this::applyMap));
        builtins.put("for-each", new BuiltinProcedure("for-each", this::applyForEach));
        builtins.put("values", new BuiltinProcedure("values", Evaluator::applyValues));
        builtins.put("call-with-values", new BuiltinProcedure("call-with-values", this::applyCallWithValues));
        BuiltinProcedure callWithCurrentContinuation = new BuiltinProcedure(
                "call/cc",
                this::applyCallWithCurrentContinuation
        );
        builtins.put("call/cc", callWithCurrentContinuation);
        builtins.put("call-with-current-continuation", callWithCurrentContinuation);
        builtins.put("dynamic-wind", new BuiltinProcedure("dynamic-wind", this::applyDynamicWind));
        builtins.put("raise", new BuiltinProcedure("raise", Evaluator::applyRaise));
        builtins.put("with-exception-handler",
                new BuiltinProcedure("with-exception-handler", this::applyWithExceptionHandler));
        builtins.put("string-append", new BuiltinProcedure("string-append", Evaluator::applyStringAppend));
        builtins.put("make-string", new BuiltinProcedure("make-string", Evaluator::applyMakeString));
        builtins.put("string", new BuiltinProcedure("string", Evaluator::applyString));
        builtins.put("string-length", new BuiltinProcedure("string-length", Evaluator::applyStringLength));
        builtins.put("substring", new BuiltinProcedure("substring", Evaluator::applySubstring));
        builtins.put("string->number", new BuiltinProcedure("string->number", Evaluator::applyStringToNumber));
        builtins.put("number->string", new BuiltinProcedure("number->string", Evaluator::applyNumberToString));
        builtins.put("symbol->string", new BuiltinProcedure("symbol->string", Evaluator::applySymbolToString));
        builtins.put("string->symbol", new BuiltinProcedure("string->symbol", Evaluator::applyStringToSymbol));
        builtins.put("string-ref", new BuiltinProcedure("string-ref", Evaluator::applyStringRef));
        builtins.put("string-copy", new BuiltinProcedure("string-copy", Evaluator::applyStringCopy));
        builtins.put("string-set!", new BuiltinProcedure("string-set!", Evaluator::applyStringSet));
        builtins.put("string->list", new BuiltinProcedure("string->list", Evaluator::applyStringToList));
        builtins.put("list->string", new BuiltinProcedure("list->string", Evaluator::applyListToString));
        builtins.put("string=?", new BuiltinProcedure("string=?", Evaluator::applyStringEqual));
        builtins.put("string<?", new BuiltinProcedure("string<?", Evaluator::applyStringLess));
        builtins.put("string>?", new BuiltinProcedure("string>?",
                args -> compareStrings(args, "string>?", (left, right) -> left.compareTo(right) > 0)));
        builtins.put("string<=?", new BuiltinProcedure("string<=?",
                args -> compareStrings(args, "string<=?", (left, right) -> left.compareTo(right) <= 0)));
        builtins.put("string>=?", new BuiltinProcedure("string>=?",
                args -> compareStrings(args, "string>=?", (left, right) -> left.compareTo(right) >= 0)));
        builtins.put("string-ci=?", new BuiltinProcedure("string-ci=?", Evaluator::applyStringCiEqual));
        builtins.put("string-upcase", new BuiltinProcedure("string-upcase", Evaluator::applyStringUpcase));
        builtins.put("string-downcase", new BuiltinProcedure("string-downcase", Evaluator::applyStringDowncase));
        builtins.put("string?", new BuiltinProcedure("string?", args -> applyPredicate(args, "string?",
                value -> value instanceof StringValue)));
        builtins.put("number?", new BuiltinProcedure("number?", args -> applyPredicate(args, "number?",
                Numbers::isNumber)));
        builtins.put("integer?", new BuiltinProcedure("integer?", args -> applyPredicate(args, "integer?",
                Numbers::isIntegerValue)));
        builtins.put("rational?", new BuiltinProcedure("rational?", args -> applyPredicate(args, "rational?",
                Numbers::isRationalValue)));
        builtins.put("exact?", new BuiltinProcedure("exact?", args -> applyPredicate(args, "exact?",
                Numbers::isExactNumber)));
        builtins.put("inexact?", new BuiltinProcedure("inexact?", args -> applyPredicate(args, "inexact?",
                Numbers::isInexactNumber)));
        builtins.put("exact->inexact", new BuiltinProcedure("exact->inexact", Evaluator::applyExactToInexact));
        builtins.put("inexact->exact", new BuiltinProcedure("inexact->exact", Evaluator::applyInexactToExact));
        builtins.put("numerator", new BuiltinProcedure("numerator", Evaluator::applyNumerator));
        builtins.put("denominator", new BuiltinProcedure("denominator", Evaluator::applyDenominator));
        builtins.put("boolean?", new BuiltinProcedure("boolean?", args -> applyPredicate(args, "boolean?",
                value -> value instanceof BoolValue)));
        builtins.put("pair?", new BuiltinProcedure("pair?", args -> applyPredicate(args, "pair?",
                value -> value instanceof PairValue)));
        builtins.put("symbol?", new BuiltinProcedure("symbol?", args -> applyPredicate(args, "symbol?",
                value -> value instanceof SymbolValue)));
        builtins.put("procedure?", new BuiltinProcedure("procedure?", args -> applyPredicate(args, "procedure?",
                Evaluator::isProcedureValue)));
        builtins.put("char?", new BuiltinProcedure("char?", args -> applyPredicate(args, "char?",
                value -> value instanceof CharValue)));
        builtins.put("char-alphabetic?", new BuiltinProcedure("char-alphabetic?",
                args -> applyPredicate(args, "char-alphabetic?",
                        value -> value instanceof CharValue charValue && Character.isLetter(charValue.value()))));
        builtins.put("char-numeric?", new BuiltinProcedure("char-numeric?",
                args -> applyPredicate(args, "char-numeric?",
                        value -> value instanceof CharValue charValue && Character.isDigit(charValue.value()))));
        builtins.put("char->integer", new BuiltinProcedure("char->integer", Evaluator::applyCharToInteger));
        builtins.put("integer->char", new BuiltinProcedure("integer->char", Evaluator::applyIntegerToChar));
        builtins.put("char-upcase", new BuiltinProcedure("char-upcase", Evaluator::applyCharUpcase));
        builtins.put("char-downcase", new BuiltinProcedure("char-downcase", Evaluator::applyCharDowncase));
        builtins.put("char=?", new BuiltinProcedure("char=?", args -> compareChars(args, "char=?",
                (left, right) -> left == right)));
        builtins.put("char<?", new BuiltinProcedure("char<?", args -> compareChars(args, "char<?",
                (left, right) -> left < right)));
        builtins.put("abs", new BuiltinProcedure("abs", Evaluator::applyAbs));
        builtins.put("gcd", new BuiltinProcedure("gcd", Evaluator::applyGcd));
        builtins.put("lcm", new BuiltinProcedure("lcm", Evaluator::applyLcm));
        builtins.put("modulo", new BuiltinProcedure("modulo", Evaluator::applyModulo));
        builtins.put("remainder", new BuiltinProcedure("remainder", Evaluator::applyRemainder));
        builtins.put("quotient", new BuiltinProcedure("quotient", Evaluator::applyQuotient));
        builtins.put("min", new BuiltinProcedure("min", Evaluator::applyMin));
        builtins.put("max", new BuiltinProcedure("max", Evaluator::applyMax));
        builtins.put("expt", new BuiltinProcedure("expt", Evaluator::applyExpt));
        builtins.put("truncate", new BuiltinProcedure("truncate", Evaluator::applyTruncate));
        builtins.put("round", new BuiltinProcedure("round", Evaluator::applyRound));
        builtins.put("zero?", new BuiltinProcedure("zero?", args -> applyNumericPredicate(args, "zero?",
                value -> value == 0L)));
        builtins.put("positive?", new BuiltinProcedure("positive?", args -> applyNumericPredicate(args, "positive?",
                value -> value > 0L)));
        builtins.put("negative?", new BuiltinProcedure("negative?", args -> applyNumericPredicate(args, "negative?",
                value -> value < 0L)));
        builtins.put("odd?", new BuiltinProcedure("odd?", args -> applyNumericPredicate(args, "odd?",
                value -> value % 2L != 0L)));
        builtins.put("even?", new BuiltinProcedure("even?", args -> applyNumericPredicate(args, "even?",
                value -> value % 2L == 0L)));
        builtins.put("list-ref", new BuiltinProcedure("list-ref", Evaluator::applyListRef));
        builtins.put("list-tail", new BuiltinProcedure("list-tail", Evaluator::applyListTail));
        builtins.put("list?", new BuiltinProcedure("list?", Evaluator::applyListPredicate));
        builtins.put("assoc", new BuiltinProcedure("assoc", Evaluator::applyAssoc));
        builtins.put("assv", new BuiltinProcedure("assv", Evaluator::applyAssv));
        builtins.put("member", new BuiltinProcedure("member", Evaluator::applyMember));
        builtins.put("eq?", new BuiltinProcedure("eq?", Evaluator::applyEq));
        builtins.put("eqv?", new BuiltinProcedure("eqv?", Evaluator::applyEqv));
        builtins.put("equal?", new BuiltinProcedure("equal?", Evaluator::applyEqual));
        builtins.put("error", new BuiltinProcedure("error", Evaluator::applyError));
        builtins.put("vector", new BuiltinProcedure("vector", Evaluator::applyVector));
        builtins.put("make-vector", new BuiltinProcedure("make-vector", Evaluator::applyMakeVector));
        builtins.put("vector-ref", new BuiltinProcedure("vector-ref", Evaluator::applyVectorRef));
        builtins.put("vector-set!", new BuiltinProcedure("vector-set!", Evaluator::applyVectorSet));
        builtins.put("vector-length", new BuiltinProcedure("vector-length", Evaluator::applyVectorLength));
        builtins.put("vector?", new BuiltinProcedure("vector?", args -> applyPredicate(args, "vector?",
                value -> value instanceof VectorValue)));
        builtins.put("vector->list", new BuiltinProcedure("vector->list", Evaluator::applyVectorToList));
        builtins.put("list->vector", new BuiltinProcedure("list->vector", Evaluator::applyListToVector));
        return Map.copyOf(builtins);
    }

    private Environment createTopLevelEnvironment() {
        Environment environment = new Environment(null);
        for (Map.Entry<String, BuiltinProcedure> entry : builtins.entrySet()) {
            environment.define(entry.getKey(), entry.getValue());
        }
        return environment;
    }

    private SchemeValue evalFunctionDefine(
            ListExpression signature,
            List<SchemeExpression> body,
            Environment environment
    ) throws EvalError {
        List<SchemeExpression> signatureElements = signature.elements();
        if (signatureElements.isEmpty()) {
            throw new EvalError("define: expected function name");
        }
        if (body.isEmpty()) {
            throw new EvalError("define: expected function body");
        }

        SchemeExpression nameExpression = signatureElements.getFirst();
        if (!(nameExpression instanceof SymbolExpression nameSymbol)) {
            throw new EvalError("define: expected function name");
        }

        ParameterSpec parameters = parseParameters(signatureElements.subList(1, signatureElements.size()), "define");
        LambdaProcedure procedure = new LambdaProcedure(
                nameSymbol.name(),
                parameters.fixedParameters(),
                parameters.restParameter(),
                List.copyOf(body),
                environment
        );
        environment.define(nameSymbol.name(), procedure);
        return VoidValue.INSTANCE;
    }

    private SchemeValue evalIf(List<SchemeExpression> elements, Environment environment) throws EvalError {
        if (elements.size() != 3 && elements.size() != 4) {
            throw new EvalError("if: expected 2 or 3 arguments");
        }

        SchemeValue condition = evalNonTail(elements.get(1), environment);
        if (isTruthy(condition)) {
            return evalNonTail(elements.get(2), environment);
        }
        if (elements.size() == 4) {
            return evalNonTail(elements.get(3), environment);
        }
        return VoidValue.INSTANCE;
    }

    private SchemeValue evalBegin(List<SchemeExpression> expressions, Environment environment) throws EvalError {
        return evalSequence(expressions, environment);
    }

    private SchemeValue evalCond(List<SchemeExpression> clauses, Environment environment) throws EvalError {
        for (int index = 0; index < clauses.size(); index++) {
            if (!(clauses.get(index) instanceof ListExpression clauseExpression)) {
                throw new EvalError("cond: expected clause");
            }

            List<SchemeExpression> clause = clauseExpression.elements();
            if (clause.isEmpty()) {
                throw new EvalError("cond: expected non-empty clause");
            }

            SchemeExpression testExpression = clause.getFirst();
            if (testExpression instanceof SymbolExpression symbol && "else".equals(symbol.name())) {
                return evalClauseBody(clause.subList(1, clause.size()), environment, BoolValue.TRUE);
            }

            SchemeValue testValue = evalNonTail(testExpression, environment);
            if (isTruthy(testValue)) {
                return evalClauseBody(clause.subList(1, clause.size()), environment, testValue);
            }
        }
        return VoidValue.INSTANCE;
    }

    private SchemeValue evalLet(List<SchemeExpression> elements, Environment environment) throws EvalError {
        if (elements.size() < 3) {
            throw new EvalError("let: expected bindings and body");
        }

        SchemeExpression second = elements.get(1);
        if (second instanceof SymbolExpression nameSymbol) {
            return evalNamedLet(nameSymbol.name(), elements, environment);
        }
        if (!(second instanceof ListExpression bindingsExpression)) {
            throw new EvalError("let: expected bindings");
        }

        List<SchemeExpression> body = elements.subList(2, elements.size());
        if (body.isEmpty()) {
            throw new EvalError("let: expected body");
        }

        List<Binding> bindings = parseBindings(bindingsExpression.elements(), "let");
        List<SchemeValue> values = new ArrayList<>(bindings.size());
        for (Binding binding : bindings) {
            values.add(evalNonTail(binding.valueExpression(), environment));
        }

        Environment letEnvironment = new Environment(environment);
        for (int index = 0; index < bindings.size(); index++) {
            letEnvironment.define(bindings.get(index).name(), values.get(index));
        }
        return evalSequence(body, letEnvironment);
    }

    private SchemeValue evalLetStar(List<SchemeExpression> elements, Environment environment) throws EvalError {
        if (elements.size() < 3) {
            throw new EvalError("let*: expected bindings and body");
        }
        if (!(elements.get(1) instanceof ListExpression bindingsExpression)) {
            throw new EvalError("let*: expected bindings");
        }

        List<SchemeExpression> body = elements.subList(2, elements.size());
        if (body.isEmpty()) {
            throw new EvalError("let*: expected body");
        }

        List<Binding> bindings = parseBindings(bindingsExpression.elements(), "let*");
        Environment letStarEnvironment = new Environment(environment);
        for (Binding binding : bindings) {
            letStarEnvironment.define(binding.name(), evalNonTail(binding.valueExpression(), letStarEnvironment));
        }
        return evalSequence(body, letStarEnvironment);
    }

    private SchemeValue evalLetrec(List<SchemeExpression> elements, Environment environment, boolean sequential)
            throws EvalError {
        String formName = sequential ? "letrec*" : "letrec";
        if (elements.size() < 3) {
            throw new EvalError(formName + ": expected bindings and body");
        }
        if (!(elements.get(1) instanceof ListExpression bindingsExpression)) {
            throw new EvalError(formName + ": expected bindings");
        }

        List<SchemeExpression> body = elements.subList(2, elements.size());
        if (body.isEmpty()) {
            throw new EvalError(formName + ": expected body");
        }

        List<Binding> bindings = parseBindings(bindingsExpression.elements(), formName);
        Environment letrecEnvironment = new Environment(environment);
        for (Binding binding : bindings) {
            letrecEnvironment.defineUninitialized(binding.name());
        }

        if (sequential) {
            for (Binding binding : bindings) {
                letrecEnvironment.set(binding.name(), evalNonTail(binding.valueExpression(), letrecEnvironment));
            }
        } else {
            List<SchemeValue> values = new ArrayList<>(bindings.size());
            for (Binding binding : bindings) {
                values.add(evalNonTail(binding.valueExpression(), letrecEnvironment));
            }
            for (int index = 0; index < bindings.size(); index++) {
                letrecEnvironment.set(bindings.get(index).name(), values.get(index));
            }
        }

        return evalSequence(body, letrecEnvironment);
    }

    private SchemeValue evalNamedLet(String name, List<SchemeExpression> elements, Environment environment)
            throws EvalError {
        if (elements.size() < 4) {
            throw new EvalError("let: expected named let bindings and body");
        }
        if (!(elements.get(2) instanceof ListExpression bindingsExpression)) {
            throw new EvalError("let: expected bindings");
        }

        List<Binding> bindings = parseBindings(bindingsExpression.elements(), "let");
        List<String> parameters = new ArrayList<>(bindings.size());
        List<SchemeValue> arguments = new ArrayList<>(bindings.size());
        for (Binding binding : bindings) {
            parameters.add(binding.name());
            arguments.add(evalNonTail(binding.valueExpression(), environment));
        }

        Environment letEnvironment = new Environment(environment);
        LambdaProcedure procedure = new LambdaProcedure(
                name,
                List.copyOf(parameters),
                null,
                List.copyOf(elements.subList(3, elements.size())),
                letEnvironment
        );
        letEnvironment.define(name, procedure);
        return applyLambda(procedure, arguments);
    }

    private SchemeValue evalCase(List<SchemeExpression> elements, Environment environment) throws EvalError {
        if (elements.size() < 3) {
            throw new EvalError("case: expected key and clauses");
        }

        SchemeValue key = evalNonTail(elements.get(1), environment);
        for (int index = 2; index < elements.size(); index++) {
            if (!(elements.get(index) instanceof ListExpression clauseExpression)) {
                throw new EvalError("case: expected clause");
            }

            List<SchemeExpression> clause = clauseExpression.elements();
            if (clause.isEmpty()) {
                throw new EvalError("case: expected non-empty clause");
            }

            SchemeExpression head = clause.getFirst();
            if (head instanceof SymbolExpression symbol && "else".equals(symbol.name())) {
                if (index != elements.size() - 1) {
                    throw new EvalError("case: else clause must be last");
                }
                return evalClauseBody(clause.subList(1, clause.size()), environment, VoidValue.INSTANCE);
            }

            if (!(head instanceof ListExpression datumList)) {
                throw new EvalError("case: expected datum list");
            }

            for (SchemeExpression datumExpression : datumList.elements()) {
                if (eqvValues(key, quote(datumExpression))) {
                    return evalClauseBody(clause.subList(1, clause.size()), environment, VoidValue.INSTANCE);
                }
            }
        }
        return VoidValue.INSTANCE;
    }

    private SchemeValue evalQuote(List<SchemeExpression> elements) throws EvalError {
        if (elements.size() != 2) {
            throw new EvalError("quote: expected 1 argument");
        }
        return quote(elements.get(1));
    }

    private SchemeValue evalLambda(List<SchemeExpression> elements, Environment environment) throws EvalError {
        if (elements.size() < 3) {
            throw new EvalError("lambda: expected parameters and body");
        }
        ParameterSpec parameters = parseParameters(elements.get(1), "lambda");
        List<SchemeExpression> body = List.copyOf(elements.subList(2, elements.size()));
        return new LambdaProcedure(
                null,
                parameters.fixedParameters(),
                parameters.restParameter(),
                body,
                environment
        );
    }

    private SchemeValue evalDo(List<SchemeExpression> elements, Environment environment) throws EvalError {
        if (elements.size() < 3) {
            throw new EvalError("do: expected bindings and test clause");
        }
        if (!(elements.get(1) instanceof ListExpression bindingsExpression)) {
            throw new EvalError("do: expected binding list");
        }
        if (!(elements.get(2) instanceof ListExpression testClause)) {
            throw new EvalError("do: expected test clause");
        }
        if (testClause.elements().isEmpty()) {
            throw new EvalError("do: expected non-empty test clause");
        }

        List<DoBinding> bindings = parseDoBindings(bindingsExpression.elements());
        List<SchemeValue> initialValues = new ArrayList<>(bindings.size());
        for (DoBinding binding : bindings) {
            initialValues.add(evalNonTail(binding.initExpression(), environment));
        }

        Environment doEnvironment = new Environment(environment);
        for (int index = 0; index < bindings.size(); index++) {
            doEnvironment.define(bindings.get(index).name(), initialValues.get(index));
        }

        List<SchemeExpression> terminationClause = testClause.elements();
        List<SchemeExpression> body = elements.subList(3, elements.size());

        while (true) {
            if (isTruthy(evalNonTail(terminationClause.getFirst(), doEnvironment))) {
                return evalClauseBody(terminationClause.subList(1, terminationClause.size()),
                        doEnvironment, VoidValue.INSTANCE);
            }

            evalSequence(body, doEnvironment);

            List<SchemeValue> nextValues = new ArrayList<>(bindings.size());
            for (DoBinding binding : bindings) {
                if (binding.stepExpression() == null) {
                    nextValues.add(doEnvironment.lookup(binding.name()));
                } else {
                    nextValues.add(evalNonTail(binding.stepExpression(), doEnvironment));
                }
            }
            for (int index = 0; index < bindings.size(); index++) {
                doEnvironment.set(bindings.get(index).name(), nextValues.get(index));
            }
        }
    }

    private SchemeValue evalCaseLambda(List<SchemeExpression> elements, Environment environment) throws EvalError {
        if (elements.size() < 2) {
            throw new EvalError("case-lambda: expected at least 1 clause");
        }

        List<CaseLambdaClause> clauses = new ArrayList<>(elements.size() - 1);
        for (int index = 1; index < elements.size(); index++) {
            SchemeExpression clauseExpression = elements.get(index);
            if (!(clauseExpression instanceof ListExpression clauseList)) {
                throw new EvalError("case-lambda: expected clause");
            }

            List<SchemeExpression> clauseElements = clauseList.elements();
            if (clauseElements.size() < 2) {
                throw new EvalError("case-lambda: expected clause parameters and body");
            }

            ParameterSpec parameters = parseParameters(clauseElements.getFirst(), "case-lambda");
            clauses.add(new CaseLambdaClause(
                    parameters.fixedParameters(),
                    parameters.restParameter(),
                    List.copyOf(clauseElements.subList(1, clauseElements.size()))
            ));
        }

        return new CaseLambdaProcedure(null, List.copyOf(clauses), environment);
    }

    private SyntaxRulesMacro parseSyntaxRules(
            String macroName,
            SchemeExpression transformerExpression,
            Environment environment
    ) throws EvalError {
        if (!(transformerExpression instanceof ListExpression rulesExpression)) {
            throw new EvalError("define-syntax: expected syntax-rules transformer");
        }

        List<SchemeExpression> rulesElements = rulesExpression.elements();
        if (rulesElements.size() < 3) {
            throw new EvalError("syntax-rules: expected literals and at least one rule");
        }
        if (!(rulesElements.getFirst() instanceof SymbolExpression symbol)
                || !"syntax-rules".equals(symbol.name())) {
            throw new EvalError("define-syntax: expected syntax-rules transformer");
        }
        if (!(rulesElements.get(1) instanceof ListExpression literalsExpression)) {
            throw new EvalError("syntax-rules: expected literal identifier list");
        }

        Set<String> literalIdentifiers = new HashSet<>();
        for (SchemeExpression literal : literalsExpression.elements()) {
            if (!(literal instanceof SymbolExpression literalSymbol)) {
                throw new EvalError("syntax-rules: expected literal identifier");
            }
            literalIdentifiers.add(literalSymbol.name());
        }

        List<SyntaxRulesMacro.SyntaxRule> rules = new ArrayList<>(rulesElements.size() - 2);
        for (int index = 2; index < rulesElements.size(); index++) {
            SchemeExpression ruleExpression = rulesElements.get(index);
            if (!(ruleExpression instanceof ListExpression ruleList)) {
                throw new EvalError("syntax-rules: expected rule");
            }
            List<SchemeExpression> ruleElements = ruleList.elements();
            if (ruleElements.size() != 2) {
                throw new EvalError("syntax-rules: expected pattern and template");
            }
            rules.add(new SyntaxRulesMacro.SyntaxRule(ruleElements.get(0), ruleElements.get(1)));
        }

        return new SyntaxRulesMacro(
                macroName,
                Set.copyOf(literalIdentifiers),
                List.copyOf(rules),
                environment
        );
    }

    private SchemeValue evalSequenceToValue(
            List<SchemeExpression> expressions,
            int startIndex,
            Environment environment,
            SchemeValue defaultValue
    ) throws EvalError {
        if (startIndex >= expressions.size()) {
            return defaultValue;
        }

        for (int index = startIndex; index < expressions.size() - 1; index++) {
            int nextIndex = index + 1;
            evalNonTailWithContinuation(
                    expressions.get(index),
                    environment,
                    value -> evalSequenceToValue(expressions, nextIndex, environment, defaultValue)
            );
        }
        return eval(expressions.getLast(), environment);
    }

    private SchemeValue evalSequence(List<SchemeExpression> expressions, Environment environment) throws EvalError {
        return evalSequenceToValue(expressions, 0, environment, VoidValue.INSTANCE);
    }

    private SchemeValue evalClauseBody(
            List<SchemeExpression> expressions,
            Environment environment,
            SchemeValue defaultValue
    ) throws EvalError {
        if (expressions.isEmpty()) {
            return defaultValue;
        }
        return evalSequence(expressions, environment);
    }

    private SchemeValue evalGuardClauses(
            String exceptionVariable,
            List<SchemeExpression> clauses,
            Environment environment,
            SchemeValue exceptionValue
    ) throws EvalError {
        Environment guardEnvironment = new Environment(environment);
        guardEnvironment.define(exceptionVariable, exceptionValue);

        for (int index = 0; index < clauses.size(); index++) {
            if (!(clauses.get(index) instanceof ListExpression clauseExpression)) {
                throw new EvalError("guard: expected clause");
            }

            List<SchemeExpression> clause = clauseExpression.elements();
            if (clause.isEmpty()) {
                throw new EvalError("guard: expected non-empty clause");
            }

            SchemeExpression testExpression = clause.getFirst();
            if (testExpression instanceof SymbolExpression symbol && "else".equals(symbol.name())) {
                return evalClauseBody(clause.subList(1, clause.size()), guardEnvironment, BoolValue.TRUE);
            }

            SchemeValue testValue = evalNonTail(testExpression, guardEnvironment);
            if (isTruthy(testValue)) {
                return evalClauseBody(clause.subList(1, clause.size()), guardEnvironment, testValue);
            }
        }

        throw new RaisedException(exceptionValue);
    }

    private TailStep evalAndTail(List<SchemeExpression> expressions, Environment environment) throws EvalError {
        if (expressions.isEmpty()) {
            return TailStep.done(BoolValue.TRUE);
        }

        for (int index = 0; index < expressions.size() - 1; index++) {
            SchemeValue result = evalNonTail(expressions.get(index), environment);
            if (!isTruthy(result)) {
                return TailStep.done(result);
            }
        }

        return TailStep.next(expressions.getLast(), environment);
    }

    private TailStep evalOrTail(List<SchemeExpression> expressions, Environment environment) throws EvalError {
        if (expressions.isEmpty()) {
            return TailStep.done(BoolValue.FALSE);
        }

        for (int index = 0; index < expressions.size() - 1; index++) {
            SchemeValue result = evalNonTail(expressions.get(index), environment);
            if (isTruthy(result)) {
                return TailStep.done(result);
            }
        }

        return TailStep.next(expressions.getLast(), environment);
    }

    private TailStep evalIfTail(List<SchemeExpression> elements, Environment environment) throws EvalError {
        if (elements.size() != 3 && elements.size() != 4) {
            throw new EvalError("if: expected 2 or 3 arguments");
        }

        SchemeValue condition = evalNonTail(elements.get(1), environment);
        if (isTruthy(condition)) {
            return TailStep.next(elements.get(2), environment);
        }
        if (elements.size() == 4) {
            return TailStep.next(elements.get(3), environment);
        }
        return TailStep.done(VoidValue.INSTANCE);
    }

    private TailStep evalCondTail(List<SchemeExpression> clauses, Environment environment) throws EvalError {
        for (int index = 0; index < clauses.size(); index++) {
            if (!(clauses.get(index) instanceof ListExpression clauseExpression)) {
                throw new EvalError("cond: expected clause");
            }

            List<SchemeExpression> clause = clauseExpression.elements();
            if (clause.isEmpty()) {
                throw new EvalError("cond: expected non-empty clause");
            }

            SchemeExpression testExpression = clause.getFirst();
            if (testExpression instanceof SymbolExpression symbol && "else".equals(symbol.name())) {
                return evalClauseBodyTail(clause.subList(1, clause.size()), environment, BoolValue.TRUE);
            }

            SchemeValue testValue = evalNonTail(testExpression, environment);
            if (isTruthy(testValue)) {
                return evalClauseBodyTail(clause.subList(1, clause.size()), environment, testValue);
            }
        }
        return TailStep.done(VoidValue.INSTANCE);
    }

    private TailStep evalLetTail(List<SchemeExpression> elements, Environment environment) throws EvalError {
        if (elements.size() < 3) {
            throw new EvalError("let: expected bindings and body");
        }

        SchemeExpression second = elements.get(1);
        if (second instanceof SymbolExpression nameSymbol) {
            return evalNamedLetTail(nameSymbol.name(), elements, environment);
        }
        if (!(second instanceof ListExpression bindingsExpression)) {
            throw new EvalError("let: expected bindings");
        }

        List<SchemeExpression> body = elements.subList(2, elements.size());
        if (body.isEmpty()) {
            throw new EvalError("let: expected body");
        }

        List<Binding> bindings = parseBindings(bindingsExpression.elements(), "let");
        List<SchemeValue> values = new ArrayList<>(bindings.size());
        for (Binding binding : bindings) {
            values.add(evalNonTail(binding.valueExpression(), environment));
        }

        Environment letEnvironment = new Environment(environment);
        for (int index = 0; index < bindings.size(); index++) {
            letEnvironment.define(bindings.get(index).name(), values.get(index));
        }
        return tailSequence(body, letEnvironment, VoidValue.INSTANCE);
    }

    private TailStep evalLetStarTail(List<SchemeExpression> elements, Environment environment) throws EvalError {
        if (elements.size() < 3) {
            throw new EvalError("let*: expected bindings and body");
        }
        if (!(elements.get(1) instanceof ListExpression bindingsExpression)) {
            throw new EvalError("let*: expected bindings");
        }

        List<SchemeExpression> body = elements.subList(2, elements.size());
        if (body.isEmpty()) {
            throw new EvalError("let*: expected body");
        }

        List<Binding> bindings = parseBindings(bindingsExpression.elements(), "let*");
        Environment letStarEnvironment = new Environment(environment);
        for (Binding binding : bindings) {
            letStarEnvironment.define(binding.name(), evalNonTail(binding.valueExpression(), letStarEnvironment));
        }
        return tailSequence(body, letStarEnvironment, VoidValue.INSTANCE);
    }

    private TailStep evalLetrecTail(List<SchemeExpression> elements, Environment environment, boolean sequential)
            throws EvalError {
        String formName = sequential ? "letrec*" : "letrec";
        if (elements.size() < 3) {
            throw new EvalError(formName + ": expected bindings and body");
        }
        if (!(elements.get(1) instanceof ListExpression bindingsExpression)) {
            throw new EvalError(formName + ": expected bindings");
        }

        List<SchemeExpression> body = elements.subList(2, elements.size());
        if (body.isEmpty()) {
            throw new EvalError(formName + ": expected body");
        }

        List<Binding> bindings = parseBindings(bindingsExpression.elements(), formName);
        Environment letrecEnvironment = new Environment(environment);
        for (Binding binding : bindings) {
            letrecEnvironment.defineUninitialized(binding.name());
        }

        if (sequential) {
            for (Binding binding : bindings) {
                letrecEnvironment.set(binding.name(), evalNonTail(binding.valueExpression(), letrecEnvironment));
            }
        } else {
            List<SchemeValue> values = new ArrayList<>(bindings.size());
            for (Binding binding : bindings) {
                values.add(evalNonTail(binding.valueExpression(), letrecEnvironment));
            }
            for (int index = 0; index < bindings.size(); index++) {
                letrecEnvironment.set(bindings.get(index).name(), values.get(index));
            }
        }

        return tailSequence(body, letrecEnvironment, VoidValue.INSTANCE);
    }

    private TailStep evalNamedLetTail(String name, List<SchemeExpression> elements, Environment environment)
            throws EvalError {
        if (elements.size() < 4) {
            throw new EvalError("let: expected named let bindings and body");
        }
        if (!(elements.get(2) instanceof ListExpression bindingsExpression)) {
            throw new EvalError("let: expected bindings");
        }

        List<Binding> bindings = parseBindings(bindingsExpression.elements(), "let");
        List<String> parameters = new ArrayList<>(bindings.size());
        List<SchemeValue> arguments = new ArrayList<>(bindings.size());
        for (Binding binding : bindings) {
            parameters.add(binding.name());
            arguments.add(evalNonTail(binding.valueExpression(), environment));
        }

        Environment letEnvironment = new Environment(environment);
        LambdaProcedure procedure = new LambdaProcedure(
                name,
                List.copyOf(parameters),
                null,
                List.copyOf(elements.subList(3, elements.size())),
                letEnvironment
        );
        letEnvironment.define(name, procedure);
        return applyLambdaTail(procedure, arguments);
    }

    private TailStep evalCaseTail(List<SchemeExpression> elements, Environment environment) throws EvalError {
        if (elements.size() < 3) {
            throw new EvalError("case: expected key and clauses");
        }

        SchemeValue key = evalNonTail(elements.get(1), environment);
        for (int index = 2; index < elements.size(); index++) {
            if (!(elements.get(index) instanceof ListExpression clauseExpression)) {
                throw new EvalError("case: expected clause");
            }

            List<SchemeExpression> clause = clauseExpression.elements();
            if (clause.isEmpty()) {
                throw new EvalError("case: expected non-empty clause");
            }

            SchemeExpression clauseHead = clause.getFirst();
            if (clauseHead instanceof SymbolExpression symbol && "else".equals(symbol.name())) {
                if (index != elements.size() - 1) {
                    throw new EvalError("case: else clause must be last");
                }
                return evalClauseBodyTail(clause.subList(1, clause.size()), environment, VoidValue.INSTANCE);
            }

            if (!(clauseHead instanceof ListExpression datumList)) {
                throw new EvalError("case: expected datum list");
            }

            for (SchemeExpression datumExpression : datumList.elements()) {
                if (eqvValues(key, quote(datumExpression))) {
                    return evalClauseBodyTail(clause.subList(1, clause.size()), environment, VoidValue.INSTANCE);
                }
            }
        }
        return TailStep.done(VoidValue.INSTANCE);
    }

    private TailStep tailSequence(
            List<SchemeExpression> expressions,
            Environment environment,
            SchemeValue defaultValue
    ) throws EvalError {
        if (expressions.isEmpty()) {
            return TailStep.done(defaultValue);
        }

        for (int index = 0; index < expressions.size() - 1; index++) {
            int nextIndex = index + 1;
            evalNonTailWithContinuation(
                    expressions.get(index),
                    environment,
                    value -> evalSequenceToValue(expressions, nextIndex, environment, defaultValue)
            );
        }
        return TailStep.next(expressions.getLast(), environment);
    }

    private TailStep evalClauseBodyTail(
            List<SchemeExpression> expressions,
            Environment environment,
            SchemeValue defaultValue
    ) throws EvalError {
        return tailSequence(expressions, environment, defaultValue);
    }

    private SchemeValue evalProcedureBody(List<SchemeExpression> body, Environment environment) throws EvalError {
        return evalSequenceToValue(body, 0, environment, VoidValue.INSTANCE);
    }

    private SchemeValue applyProcedure(SchemeValue callee, List<SchemeValue> arguments) throws EvalError {
        if (callee instanceof BuiltinProcedure builtinProcedure) {
            return builtinProcedure.apply(arguments);
        }
        if (callee instanceof LambdaProcedure lambdaProcedure) {
            return applyLambda(lambdaProcedure, arguments);
        }
        if (callee instanceof CaseLambdaProcedure caseLambdaProcedure) {
            return applyCaseLambda(caseLambdaProcedure, arguments);
        }
        if (callee instanceof ContinuationProcedure continuationProcedure) {
            return applyContinuation(continuationProcedure, arguments);
        }
        throw new EvalError("not a procedure");
    }

    private TailStep applyProcedureTail(SchemeValue callee, List<SchemeValue> arguments) throws EvalError {
        if (callee instanceof BuiltinProcedure builtinProcedure) {
            return TailStep.done(builtinProcedure.apply(arguments));
        }
        if (callee instanceof LambdaProcedure lambdaProcedure) {
            return applyLambdaTail(lambdaProcedure, arguments);
        }
        if (callee instanceof CaseLambdaProcedure caseLambdaProcedure) {
            return applyCaseLambdaTail(caseLambdaProcedure, arguments);
        }
        if (callee instanceof ContinuationProcedure continuationProcedure) {
            return TailStep.done(applyContinuation(continuationProcedure, arguments));
        }
        throw new EvalError("not a procedure");
    }

    private SchemeValue applyProcedureTailToValue(SchemeValue callee, List<SchemeValue> arguments) throws EvalError {
        TailStep step = applyProcedureTail(callee, arguments);
        if (step.isDone()) {
            return step.value();
        }
        return eval(step.nextExpression(), step.nextEnvironment());
    }

    private SchemeValue applyLambda(LambdaProcedure procedure, List<SchemeValue> arguments) throws EvalError {
        return applyClosure(
                procedure.parameters(),
                procedure.restParameter(),
                procedure.body(),
                procedure.closureEnvironment(),
                arguments,
                procedure.render()
        );
    }

    private SchemeValue applyCaseLambda(CaseLambdaProcedure procedure, List<SchemeValue> arguments) throws EvalError {
        for (CaseLambdaClause clause : procedure.clauses()) {
            if (matchesArity(clause.parameters().size(), clause.restParameter(), arguments.size())) {
                return applyClosure(
                        clause.parameters(),
                        clause.restParameter(),
                        clause.body(),
                        procedure.closureEnvironment(),
                        arguments,
                        procedure.render()
                );
            }
        }
        throw new EvalError(procedure.render() + ": expected a matching clause for " + arguments.size()
                + " arguments");
    }

    private TailStep applyLambdaTail(LambdaProcedure procedure, List<SchemeValue> arguments) throws EvalError {
        return applyClosureTail(
                procedure.parameters(),
                procedure.restParameter(),
                procedure.body(),
                procedure.closureEnvironment(),
                arguments,
                procedure.render()
        );
    }

    private TailStep applyCaseLambdaTail(CaseLambdaProcedure procedure, List<SchemeValue> arguments)
            throws EvalError {
        for (CaseLambdaClause clause : procedure.clauses()) {
            if (matchesArity(clause.parameters().size(), clause.restParameter(), arguments.size())) {
                return applyClosureTail(
                        clause.parameters(),
                        clause.restParameter(),
                        clause.body(),
                        procedure.closureEnvironment(),
                        arguments,
                        procedure.render()
                );
            }
        }
        throw new EvalError(procedure.render() + ": expected a matching clause for " + arguments.size()
                + " arguments");
    }

    private SchemeValue applyClosure(
            List<String> parameters,
            String restParameter,
            List<SchemeExpression> body,
            Environment closureEnvironment,
            List<SchemeValue> arguments,
            String procedureName
    ) throws EvalError {
        Environment invocationEnvironment = createInvocationEnvironment(
                parameters,
                restParameter,
                closureEnvironment,
                arguments,
                procedureName
        );
        return evalProcedureBody(body, invocationEnvironment);
    }

    private TailStep applyClosureTail(
            List<String> parameters,
            String restParameter,
            List<SchemeExpression> body,
            Environment closureEnvironment,
            List<SchemeValue> arguments,
            String procedureName
    ) throws EvalError {
        Environment invocationEnvironment = createInvocationEnvironment(
                parameters,
                restParameter,
                closureEnvironment,
                arguments,
                procedureName
        );
        return tailSequence(body, invocationEnvironment, VoidValue.INSTANCE);
    }

    private Environment createInvocationEnvironment(
            List<String> parameters,
            String restParameter,
            Environment closureEnvironment,
            List<SchemeValue> arguments,
            String procedureName
    ) throws EvalError {
        int requiredCount = parameters.size();
        if (restParameter == null) {
            if (arguments.size() != requiredCount) {
                throw new EvalError(procedureName + ": expected " + requiredCount + " arguments");
            }
        } else if (arguments.size() < requiredCount) {
            throw new EvalError(procedureName + ": expected at least " + requiredCount + " arguments");
        }

        Environment invocationEnvironment = new Environment(closureEnvironment);
        for (int index = 0; index < requiredCount; index++) {
            invocationEnvironment.define(parameters.get(index), arguments.get(index));
        }
        if (restParameter != null) {
            invocationEnvironment.define(
                    restParameter,
                    buildList(arguments.subList(requiredCount, arguments.size()))
            );
        }
        return invocationEnvironment;
    }

    private ParameterSpec parseParameters(SchemeExpression parameterExpression, String formName)
            throws EvalError {
        if (parameterExpression instanceof SymbolExpression symbol) {
            return new ParameterSpec(List.of(), symbol.name());
        }
        if (parameterExpression instanceof ListExpression listExpression) {
            return parseParameters(listExpression.elements(), formName);
        }
        throw new EvalError(formName + ": expected parameter list");
    }

    private ParameterSpec parseParameters(List<SchemeExpression> parameterExpressions, String formName)
            throws EvalError {
        List<String> parameters = new ArrayList<>(parameterExpressions.size());
        String restParameter = null;
        for (int index = 0; index < parameterExpressions.size(); index++) {
            SchemeExpression parameterExpression = parameterExpressions.get(index);
            if (!(parameterExpression instanceof SymbolExpression symbol)) {
                throw new EvalError(formName + ": expected symbol parameter");
            }
            String name = symbol.name();
            if (".".equals(name)) {
                if (index != parameterExpressions.size() - 2 || restParameter != null) {
                    throw new EvalError(formName + ": invalid dotted parameter list");
                }

                SchemeExpression restExpression = parameterExpressions.get(index + 1);
                if (!(restExpression instanceof SymbolExpression restSymbol)) {
                    throw new EvalError(formName + ": expected symbol parameter");
                }
                restParameter = restSymbol.name();
                break;
            }
            parameters.add(name);
        }
        return new ParameterSpec(List.copyOf(parameters), restParameter);
    }

    private List<Binding> parseBindings(List<SchemeExpression> bindingExpressions, String formName)
            throws EvalError {
        List<Binding> bindings = new ArrayList<>(bindingExpressions.size());
        for (SchemeExpression bindingExpression : bindingExpressions) {
            if (!(bindingExpression instanceof ListExpression bindingList)) {
                throw new EvalError(formName + ": expected binding");
            }

            List<SchemeExpression> binding = bindingList.elements();
            if (binding.size() != 2) {
                throw new EvalError(formName + ": expected binding pair");
            }
            if (!(binding.getFirst() instanceof SymbolExpression symbol)) {
                throw new EvalError(formName + ": expected binding name");
            }
            bindings.add(new Binding(symbol.name(), binding.get(1)));
        }
        return bindings;
    }

    private List<DoBinding> parseDoBindings(List<SchemeExpression> bindingExpressions) throws EvalError {
        List<DoBinding> bindings = new ArrayList<>(bindingExpressions.size());
        for (SchemeExpression bindingExpression : bindingExpressions) {
            if (!(bindingExpression instanceof ListExpression bindingList)) {
                throw new EvalError("do: expected binding");
            }

            List<SchemeExpression> binding = bindingList.elements();
            if (binding.size() != 2 && binding.size() != 3) {
                throw new EvalError("do: expected binding with init and optional step");
            }
            if (!(binding.getFirst() instanceof SymbolExpression symbol)) {
                throw new EvalError("do: expected binding name");
            }

            SchemeExpression stepExpression = null;
            if (binding.size() == 3) {
                stepExpression = binding.get(2);
            }
            bindings.add(new DoBinding(symbol.name(), binding.get(1), stepExpression));
        }
        return bindings;
    }

    private SchemeValue quote(SchemeExpression expression) throws EvalError {
        if (expression instanceof LiteralExpression literal) {
            return literal.value();
        }
        if (expression instanceof SymbolExpression symbol) {
            return new SymbolValue(symbol.name());
        }
        List<SchemeExpression> elements = ((ListExpression) expression).elements();
        SchemeValue result = EmptyListValue.INSTANCE;
        for (int index = elements.size() - 1; index >= 0; index--) {
            result = new PairValue(quote(elements.get(index)), result);
        }
        return result;
    }

    private static SchemeValue applyAdd(List<SchemeValue> arguments) throws EvalError {
        return Numbers.add(arguments, "+");
    }

    private static SchemeValue applySubtract(List<SchemeValue> arguments) throws EvalError {
        return Numbers.subtract(arguments, "-");
    }

    private static SchemeValue applyMultiply(List<SchemeValue> arguments) throws EvalError {
        return Numbers.multiply(arguments, "*");
    }

    private static SchemeValue applyDivide(List<SchemeValue> arguments) throws EvalError {
        return Numbers.divide(arguments, "/");
    }

    private static SchemeValue applyNot(List<SchemeValue> arguments) throws EvalError {
        if (arguments.size() != 1) {
            throw new EvalError("not: expected 1 argument");
        }

        return SchemeValue.booleanValue(!isTruthy(arguments.getFirst()));
    }

    private static SchemeValue applyCons(List<SchemeValue> arguments) throws EvalError {
        requireArgumentCount(arguments, 2, "cons");
        return new PairValue(arguments.get(0), arguments.get(1));
    }

    private static SchemeValue applyCar(List<SchemeValue> arguments) throws EvalError {
        requireArgumentCount(arguments, 1, "car");
        return requirePair(arguments.getFirst(), "car").car();
    }

    private static SchemeValue applyCdr(List<SchemeValue> arguments) throws EvalError {
        requireArgumentCount(arguments, 1, "cdr");
        return requirePair(arguments.getFirst(), "cdr").cdr();
    }

    private static SchemeValue applyCddr(List<SchemeValue> arguments) throws EvalError {
        requireArgumentCount(arguments, 1, "cddr");
        return requirePair(requirePair(arguments.getFirst(), "cddr").cdr(), "cddr").cdr();
    }

    private static SchemeValue applySetCar(List<SchemeValue> arguments) throws EvalError {
        requireArgumentCount(arguments, 2, "set-car!");
        requirePair(arguments.get(0), "set-car!").setCar(arguments.get(1));
        return VoidValue.INSTANCE;
    }

    private static SchemeValue applySetCdr(List<SchemeValue> arguments) throws EvalError {
        requireArgumentCount(arguments, 2, "set-cdr!");
        requirePair(arguments.get(0), "set-cdr!").setCdr(arguments.get(1));
        return VoidValue.INSTANCE;
    }

    private static SchemeValue applyList(List<SchemeValue> arguments) {
        return buildList(arguments);
    }

    private static SchemeValue applyLength(List<SchemeValue> arguments) throws EvalError {
        requireArgumentCount(arguments, 1, "length");
        return new IntValue(requireProperList(arguments.getFirst(), "length").size());
    }

    private static SchemeValue applyAppend(List<SchemeValue> arguments) throws EvalError {
        if (arguments.isEmpty()) {
            return EmptyListValue.INSTANCE;
        }

        SchemeValue result = arguments.getLast();
        for (int index = arguments.size() - 2; index >= 0; index--) {
            List<SchemeValue> elements = requireProperList(arguments.get(index), "append");
            for (int elementIndex = elements.size() - 1; elementIndex >= 0; elementIndex--) {
                result = new PairValue(elements.get(elementIndex), result);
            }
        }

        if (arguments.size() == 1) {
            requireProperList(arguments.getFirst(), "append");
        }
        return result;
    }

    private static SchemeValue applyReverse(List<SchemeValue> arguments) throws EvalError {
        requireArgumentCount(arguments, 1, "reverse");
        SchemeValue result = EmptyListValue.INSTANCE;
        for (SchemeValue element : requireProperList(arguments.getFirst(), "reverse")) {
            result = new PairValue(element, result);
        }
        return result;
    }

    private SchemeValue applyDisplay(List<SchemeValue> arguments) throws EvalError {
        requireArgumentCount(arguments, 1, "display");
        emit(arguments.getFirst().display());
        return VoidValue.INSTANCE;
    }

    private SchemeValue applyWrite(List<SchemeValue> arguments) throws EvalError {
        requireArgumentCount(arguments, 1, "write");
        emit(arguments.getFirst().render());
        return VoidValue.INSTANCE;
    }

    private SchemeValue applyNewline(List<SchemeValue> arguments) throws EvalError {
        requireArgumentCount(arguments, 0, "newline");
        emit("\n");
        return VoidValue.INSTANCE;
    }

    private SchemeValue applyApply(List<SchemeValue> arguments) throws EvalError {
        if (arguments.size() < 2) {
            throw new EvalError("apply: expected at least 2 arguments");
        }

        List<SchemeValue> expandedArguments = new ArrayList<>(arguments.size());
        for (int index = 1; index < arguments.size() - 1; index++) {
            expandedArguments.add(arguments.get(index));
        }
        expandedArguments.addAll(requireProperList(arguments.getLast(), "apply"));
        return applyProcedure(arguments.getFirst(), expandedArguments);
    }

    private SchemeValue applyMap(List<SchemeValue> arguments) throws EvalError {
        if (arguments.size() < 2) {
            throw new EvalError("map: expected at least 2 arguments");
        }

        SchemeValue procedure = arguments.getFirst();
        List<List<SchemeValue>> lists = new ArrayList<>(arguments.size() - 1);
        for (int index = 1; index < arguments.size(); index++) {
            lists.add(requireProperList(arguments.get(index), "map"));
        }

        int length = lists.getFirst().size();
        for (int index = 1; index < lists.size(); index++) {
            if (lists.get(index).size() != length) {
                throw new EvalError("map: expected lists of equal length");
            }
        }

        List<SchemeValue> results = new ArrayList<>(length);
        for (int index = 0; index < length; index++) {
            List<SchemeValue> callArguments = new ArrayList<>(lists.size());
            for (List<SchemeValue> list : lists) {
                callArguments.add(list.get(index));
            }
            results.add(applyProcedure(procedure, callArguments));
        }
        return buildList(results);
    }

    private SchemeValue applyForEach(List<SchemeValue> arguments) throws EvalError {
        if (arguments.size() < 2) {
            throw new EvalError("for-each: expected at least 2 arguments");
        }

        SchemeValue procedure = arguments.getFirst();
        List<List<SchemeValue>> lists = new ArrayList<>(arguments.size() - 1);
        for (int index = 1; index < arguments.size(); index++) {
            lists.add(requireProperList(arguments.get(index), "for-each"));
        }

        int length = lists.getFirst().size();
        for (int index = 1; index < lists.size(); index++) {
            if (lists.get(index).size() != length) {
                throw new EvalError("for-each: expected lists of equal length");
            }
        }

        for (int index = 0; index < length; index++) {
            List<SchemeValue> callArguments = new ArrayList<>(lists.size());
            for (List<SchemeValue> list : lists) {
                callArguments.add(list.get(index));
            }
            applyProcedure(procedure, callArguments);
        }
        return VoidValue.INSTANCE;
    }

    private SchemeValue applyThunk(SchemeValue thunk) throws EvalError {
        return applyProcedure(thunk, List.of());
    }

    private static SchemeValue applyValues(List<SchemeValue> arguments) {
        return packValues(arguments);
    }

    private SchemeValue applyCallWithValues(List<SchemeValue> arguments) throws EvalError {
        requireArgumentCount(arguments, 2, "call-with-values");

        SchemeValue produced = applyThunk(arguments.getFirst());
        return applyProcedure(arguments.get(1), unpackValues(produced));
    }

    private SchemeValue applyCallWithCurrentContinuation(List<SchemeValue> arguments) throws EvalError {
        requireArgumentCount(arguments, 1, "call/cc");
        return applyProcedure(
                arguments.getFirst(),
                List.of(new ContinuationProcedure(
                        new CapturedContinuation(currentContinuation, List.copyOf(currentWinds))
                ))
        );
    }

    private static SchemeValue applyRaise(List<SchemeValue> arguments) throws EvalError {
        requireArgumentCount(arguments, 1, "raise");
        throw new RaisedException(arguments.getFirst());
    }

    private SchemeValue applyWithExceptionHandler(List<SchemeValue> arguments) throws EvalError {
        requireArgumentCount(arguments, 2, "with-exception-handler");

        try {
            return applyThunk(arguments.get(1));
        } catch (RaisedException raised) {
            return applyProcedure(arguments.get(0), List.of(raised.value()));
        }
    }

    private SchemeValue applyDynamicWind(List<SchemeValue> arguments) throws EvalError {
        requireArgumentCount(arguments, 3, "dynamic-wind");

        SchemeValue inThunk = arguments.get(0);
        SchemeValue bodyThunk = arguments.get(1);
        SchemeValue outThunk = arguments.get(2);

        applyThunk(inThunk);

        WindFrame wind = new WindFrame(inThunk, outThunk);
        pushWind(wind);

        ContinuationContext bodyContinuation = new ContinuationContext(
                value -> finishDynamicWind(wind, value),
                currentContinuation
        );

        try {
            SchemeValue value = withActiveContinuation(bodyContinuation, () -> applyThunk(bodyThunk));
            return finishDynamicWind(wind, value);
        } catch (EvalError error) {
            popWind(wind);
            try {
                applyThunk(outThunk);
            } catch (EvalError outError) {
                throw outError;
            }
            throw error;
        } catch (RaisedException raised) {
            popWind(wind);
            applyThunk(outThunk);
            throw raised;
        } catch (ContinuationJump jump) {
            popWind(wind);
            throw jump;
        }
    }

    private SchemeValue finishDynamicWind(WindFrame wind, SchemeValue value) throws EvalError {
        popWind(wind);
        applyThunk(wind.outThunk());
        return value;
    }

    private void pushWind(WindFrame wind) {
        List<WindFrame> winds = new ArrayList<>(currentWinds.size() + 1);
        winds.addAll(currentWinds);
        winds.add(wind);
        currentWinds = List.copyOf(winds);
    }

    private void popWind(WindFrame wind) {
        if (currentWinds.isEmpty() || currentWinds.getLast() != wind) {
            return;
        }
        currentWinds = copyWindPrefix(currentWinds, currentWinds.size() - 1);
    }

    private SchemeValue applyContinuation(ContinuationProcedure continuationProcedure, List<SchemeValue> arguments)
            throws EvalError {
        requireArgumentCount(arguments, 1, "continuation");
        CapturedContinuation continuation = (CapturedContinuation) continuationProcedure.continuation();
        throw new ContinuationJump(
                continuation.context(),
                arguments.getFirst(),
                List.copyOf(currentWinds),
                continuation.winds()
        );
    }

    private static SchemeValue applyStringAppend(List<SchemeValue> arguments) throws EvalError {
        StringBuilder builder = new StringBuilder();
        for (SchemeValue argument : arguments) {
            builder.append(requireString(argument, "string-append"));
        }
        return new StringValue(builder.toString());
    }

    private static SchemeValue applyMakeString(List<SchemeValue> arguments) throws EvalError {
        if (arguments.size() != 1 && arguments.size() != 2) {
            throw new EvalError("make-string: expected 1 or 2 arguments");
        }

        long length = requireIndex(arguments.getFirst(), "make-string");
        if (length > Integer.MAX_VALUE) {
            throw new EvalError("make-string: length too large");
        }

        char fill = arguments.size() == 2 ? requireChar(arguments.get(1), "make-string") : ' ';
        return new StringValue(String.valueOf(fill).repeat((int) length));
    }

    private static SchemeValue applyString(List<SchemeValue> arguments) throws EvalError {
        StringBuilder builder = new StringBuilder(arguments.size());
        for (SchemeValue argument : arguments) {
            builder.append(requireChar(argument, "string"));
        }
        return new StringValue(builder.toString());
    }

    private static SchemeValue applyStringLength(List<SchemeValue> arguments) throws EvalError {
        requireArgumentCount(arguments, 1, "string-length");
        return new IntValue(requireString(arguments.getFirst(), "string-length").length());
    }

    private static SchemeValue applySubstring(List<SchemeValue> arguments) throws EvalError {
        requireArgumentCount(arguments, 3, "substring");
        String value = requireString(arguments.get(0), "substring");
        long start = requireInteger(arguments.get(1), "substring");
        long end = requireInteger(arguments.get(2), "substring");
        if (start < 0 || end < start || end > value.length()) {
            throw new EvalError("substring: index out of bounds");
        }
        return new StringValue(value.substring((int) start, (int) end));
    }

    private static SchemeValue applyStringToNumber(List<SchemeValue> arguments) throws EvalError {
        requireArgumentCount(arguments, 1, "string->number");
        String value = requireString(arguments.getFirst(), "string->number");
        SchemeValue parsed = Numbers.tryParseNumber(value);
        if (parsed == null) {
            return BoolValue.FALSE;
        }
        return parsed;
    }

    private static SchemeValue applyNumberToString(List<SchemeValue> arguments) throws EvalError {
        requireArgumentCount(arguments, 1, "number->string");
        return new StringValue(Numbers.requireNumeric(arguments.getFirst(), "number->string").render());
    }

    private static SchemeValue applySymbolToString(List<SchemeValue> arguments) throws EvalError {
        requireArgumentCount(arguments, 1, "symbol->string");
        return new StringValue(requireSymbol(arguments.getFirst(), "symbol->string"));
    }

    private static SchemeValue applyStringToSymbol(List<SchemeValue> arguments) throws EvalError {
        requireArgumentCount(arguments, 1, "string->symbol");
        return new SymbolValue(requireString(arguments.getFirst(), "string->symbol"));
    }

    private static SchemeValue applyStringRef(List<SchemeValue> arguments) throws EvalError {
        requireArgumentCount(arguments, 2, "string-ref");
        StringValue value = requireStringValue(arguments.getFirst(), "string-ref");
        long index = requireInteger(arguments.get(1), "string-ref");
        if (index < 0 || index >= value.length()) {
            throw new EvalError("string-ref: index out of bounds");
        }
        return new CharValue(value.charAt((int) index));
    }

    private static SchemeValue applyStringCopy(List<SchemeValue> arguments) throws EvalError {
        requireArgumentCount(arguments, 1, "string-copy");
        return requireStringValue(arguments.getFirst(), "string-copy").copy(true);
    }

    private static SchemeValue applyStringSet(List<SchemeValue> arguments) throws EvalError {
        requireArgumentCount(arguments, 3, "string-set!");
        StringValue value = requireStringValue(arguments.get(0), "string-set!");
        if (!value.isMutable()) {
            throw new EvalError("string-set!: expected mutable string");
        }

        long index = requireInteger(arguments.get(1), "string-set!");
        if (index < 0 || index >= value.length()) {
            throw new EvalError("string-set!: index out of bounds");
        }

        value.setCharAt((int) index, requireChar(arguments.get(2), "string-set!"));
        return VoidValue.INSTANCE;
    }

    private static SchemeValue applyStringToList(List<SchemeValue> arguments) throws EvalError {
        requireArgumentCount(arguments, 1, "string->list");
        String value = requireString(arguments.getFirst(), "string->list");
        List<SchemeValue> elements = new ArrayList<>(value.length());
        for (int index = 0; index < value.length(); index++) {
            elements.add(new CharValue(value.charAt(index)));
        }
        return buildList(elements);
    }

    private static SchemeValue applyListToString(List<SchemeValue> arguments) throws EvalError {
        requireArgumentCount(arguments, 1, "list->string");
        List<SchemeValue> elements = requireProperList(arguments.getFirst(), "list->string");
        StringBuilder builder = new StringBuilder(elements.size());
        for (SchemeValue element : elements) {
            builder.append(requireChar(element, "list->string"));
        }
        return new StringValue(builder.toString());
    }

    private static SchemeValue applyStringEqual(List<SchemeValue> arguments) throws EvalError {
        return compareStrings(arguments, "string=?", String::equals);
    }

    private static SchemeValue applyStringLess(List<SchemeValue> arguments) throws EvalError {
        return compareStrings(arguments, "string<?", (left, right) -> left.compareTo(right) < 0);
    }

    private static SchemeValue applyStringCiEqual(List<SchemeValue> arguments) throws EvalError {
        return compareStrings(arguments, "string-ci=?",
                (left, right) -> left.equalsIgnoreCase(right));
    }

    private static SchemeValue applyStringUpcase(List<SchemeValue> arguments) throws EvalError {
        requireArgumentCount(arguments, 1, "string-upcase");
        return new StringValue(requireString(arguments.getFirst(), "string-upcase").toUpperCase(Locale.ROOT));
    }

    private static SchemeValue applyStringDowncase(List<SchemeValue> arguments) throws EvalError {
        requireArgumentCount(arguments, 1, "string-downcase");
        return new StringValue(requireString(arguments.getFirst(), "string-downcase").toLowerCase(Locale.ROOT));
    }

    private static SchemeValue applyCharUpcase(List<SchemeValue> arguments) throws EvalError {
        requireArgumentCount(arguments, 1, "char-upcase");
        return new CharValue(Character.toUpperCase(requireChar(arguments.getFirst(), "char-upcase")));
    }

    private static SchemeValue applyCharToInteger(List<SchemeValue> arguments) throws EvalError {
        requireArgumentCount(arguments, 1, "char->integer");
        return new IntValue(requireChar(arguments.getFirst(), "char->integer"));
    }

    private static SchemeValue applyIntegerToChar(List<SchemeValue> arguments) throws EvalError {
        requireArgumentCount(arguments, 1, "integer->char");
        long value = requireInteger(arguments.getFirst(), "integer->char");
        if (value < Character.MIN_VALUE || value > Character.MAX_VALUE) {
            throw new EvalError("integer->char: invalid character code");
        }
        return new CharValue((char) value);
    }

    private static SchemeValue applyCharDowncase(List<SchemeValue> arguments) throws EvalError {
        requireArgumentCount(arguments, 1, "char-downcase");
        return new CharValue(Character.toLowerCase(requireChar(arguments.getFirst(), "char-downcase")));
    }

    private static SchemeValue applyAbs(List<SchemeValue> arguments) throws EvalError {
        requireArgumentCount(arguments, 1, "abs");
        return Numbers.abs(arguments.getFirst(), "abs");
    }

    private static SchemeValue applyGcd(List<SchemeValue> arguments) throws EvalError {
        long result = 0L;
        for (SchemeValue argument : arguments) {
            result = gcd(result, requireInteger(argument, "gcd"));
        }
        return new IntValue(Math.abs(result));
    }

    private static SchemeValue applyLcm(List<SchemeValue> arguments) throws EvalError {
        long result = 1L;
        for (SchemeValue argument : arguments) {
            long value = requireInteger(argument, "lcm");
            if (result == 0L || value == 0L) {
                result = 0L;
                continue;
            }
            result = Math.abs((result / gcd(result, value)) * value);
        }
        return new IntValue(result);
    }

    private static SchemeValue applyExactToInexact(List<SchemeValue> arguments) throws EvalError {
        requireArgumentCount(arguments, 1, "exact->inexact");
        return Numbers.exactToInexact(arguments.getFirst(), "exact->inexact");
    }

    private static SchemeValue applyInexactToExact(List<SchemeValue> arguments) throws EvalError {
        requireArgumentCount(arguments, 1, "inexact->exact");
        return Numbers.inexactToExact(arguments.getFirst(), "inexact->exact");
    }

    private static SchemeValue applyNumerator(List<SchemeValue> arguments) throws EvalError {
        requireArgumentCount(arguments, 1, "numerator");
        return Numbers.numerator(arguments.getFirst(), "numerator");
    }

    private static SchemeValue applyDenominator(List<SchemeValue> arguments) throws EvalError {
        requireArgumentCount(arguments, 1, "denominator");
        return Numbers.denominator(arguments.getFirst(), "denominator");
    }

    private static SchemeValue applyModulo(List<SchemeValue> arguments) throws EvalError {
        requireArgumentCount(arguments, 2, "modulo");
        long dividend = requireInteger(arguments.get(0), "modulo");
        long divisor = requireInteger(arguments.get(1), "modulo");
        if (divisor == 0L) {
            throw new EvalError("division by zero");
        }

        long remainder = dividend % divisor;
        if (remainder != 0L && ((divisor < 0L && remainder > 0L) || (divisor > 0L && remainder < 0L))) {
            remainder += divisor;
        }
        return new IntValue(remainder);
    }

    private static SchemeValue applyRemainder(List<SchemeValue> arguments) throws EvalError {
        requireArgumentCount(arguments, 2, "remainder");
        long dividend = requireInteger(arguments.get(0), "remainder");
        long divisor = requireInteger(arguments.get(1), "remainder");
        if (divisor == 0L) {
            throw new EvalError("division by zero");
        }
        return new IntValue(dividend % divisor);
    }

    private static SchemeValue applyQuotient(List<SchemeValue> arguments) throws EvalError {
        requireArgumentCount(arguments, 2, "quotient");
        long dividend = requireInteger(arguments.get(0), "quotient");
        long divisor = requireInteger(arguments.get(1), "quotient");
        if (divisor == 0L) {
            throw new EvalError("division by zero");
        }
        return new IntValue(dividend / divisor);
    }

    private static SchemeValue applyMin(List<SchemeValue> arguments) throws EvalError {
        if (arguments.isEmpty()) {
            throw new EvalError("min: expected at least 1 argument");
        }

        long result = requireInteger(arguments.getFirst(), "min");
        for (int index = 1; index < arguments.size(); index++) {
            result = Math.min(result, requireInteger(arguments.get(index), "min"));
        }
        return new IntValue(result);
    }

    private static SchemeValue applyMax(List<SchemeValue> arguments) throws EvalError {
        if (arguments.isEmpty()) {
            throw new EvalError("max: expected at least 1 argument");
        }

        long result = requireInteger(arguments.getFirst(), "max");
        for (int index = 1; index < arguments.size(); index++) {
            result = Math.max(result, requireInteger(arguments.get(index), "max"));
        }
        return new IntValue(result);
    }

    private static SchemeValue applyExpt(List<SchemeValue> arguments) throws EvalError {
        requireArgumentCount(arguments, 2, "expt");
        long base = requireInteger(arguments.get(0), "expt");
        long exponent = requireInteger(arguments.get(1), "expt");
        if (exponent < 0L) {
            throw new EvalError("expt: expected non-negative exponent");
        }

        long result = 1L;
        long factor = base;
        long power = exponent;
        while (power > 0L) {
            if ((power & 1L) != 0L) {
                result *= factor;
            }
            power >>= 1;
            if (power > 0L) {
                factor *= factor;
            }
        }
        return new IntValue(result);
    }

    private static SchemeValue applyTruncate(List<SchemeValue> arguments) throws EvalError {
        requireArgumentCount(arguments, 1, "truncate");
        return new IntValue(truncateToLong(arguments.getFirst(), "truncate"));
    }

    private static SchemeValue applyRound(List<SchemeValue> arguments) throws EvalError {
        requireArgumentCount(arguments, 1, "round");
        NumericValue numeric = Numbers.requireNumeric(arguments.getFirst(), "round");
        if (numeric instanceof IntValue intValue) {
            return intValue;
        }
        if (numeric instanceof RationalValue rationalValue) {
            return new IntValue(Math.round((double) rationalValue.numerator() / (double) rationalValue.denominator()));
        }
        return new IntValue(Math.round(numeric.doubleValue()));
    }

    private static SchemeValue applyListRef(List<SchemeValue> arguments) throws EvalError {
        requireArgumentCount(arguments, 2, "list-ref");
        SchemeValue tail = nthTail(arguments.getFirst(), requireIndex(arguments.get(1), "list-ref"), "list-ref");
        if (tail instanceof PairValue pair) {
            return pair.car();
        }
        throw new EvalError("list-ref: index out of bounds");
    }

    private static SchemeValue applyListTail(List<SchemeValue> arguments) throws EvalError {
        requireArgumentCount(arguments, 2, "list-tail");
        return nthTail(arguments.getFirst(), requireIndex(arguments.get(1), "list-tail"), "list-tail");
    }

    private static SchemeValue applyListPredicate(List<SchemeValue> arguments) throws EvalError {
        requireArgumentCount(arguments, 1, "list?");
        return SchemeValue.booleanValue(isProperList(arguments.getFirst()));
    }

    private static SchemeValue applyAssoc(List<SchemeValue> arguments) throws EvalError {
        requireArgumentCount(arguments, 2, "assoc");
        SchemeValue key = arguments.get(0);
        SchemeValue current = arguments.get(1);
        while (current instanceof PairValue pair) {
            SchemeValue candidate = pair.car();
            PairValue association = requirePair(candidate, "assoc");
            if (equalValues(key, association.car())) {
                return candidate;
            }
            current = pair.cdr();
        }
        if (current instanceof EmptyListValue) {
            return BoolValue.FALSE;
        }
        throw new EvalError("assoc: expected list");
    }

    private static SchemeValue applyAssv(List<SchemeValue> arguments) throws EvalError {
        requireArgumentCount(arguments, 2, "assv");
        SchemeValue key = arguments.get(0);
        SchemeValue current = arguments.get(1);
        Set<PairValue> seen = new HashSet<>();
        while (current instanceof PairValue pair) {
            if (!seen.add(pair)) {
                throw new EvalError("assv: expected list");
            }
            SchemeValue candidate = pair.car();
            PairValue association = requirePair(candidate, "assv");
            if (eqvValues(key, association.car())) {
                return candidate;
            }
            current = pair.cdr();
        }
        if (current instanceof EmptyListValue) {
            return BoolValue.FALSE;
        }
        throw new EvalError("assv: expected list");
    }

    private static SchemeValue applyMember(List<SchemeValue> arguments) throws EvalError {
        requireArgumentCount(arguments, 2, "member");
        SchemeValue key = arguments.get(0);
        SchemeValue current = arguments.get(1);
        Set<PairValue> seen = new HashSet<>();
        while (current instanceof PairValue pair) {
            if (!seen.add(pair)) {
                throw new EvalError("member: expected list");
            }
            if (equalValues(key, pair.car())) {
                return current;
            }
            current = pair.cdr();
        }
        if (current instanceof EmptyListValue) {
            return BoolValue.FALSE;
        }
        throw new EvalError("member: expected list");
    }

    private static SchemeValue applyEq(List<SchemeValue> arguments) throws EvalError {
        requireArgumentCount(arguments, 2, "eq?");
        return SchemeValue.booleanValue(eqValues(arguments.get(0), arguments.get(1)));
    }

    private static SchemeValue applyEqv(List<SchemeValue> arguments) throws EvalError {
        requireArgumentCount(arguments, 2, "eqv?");
        return SchemeValue.booleanValue(eqvValues(arguments.get(0), arguments.get(1)));
    }

    private static SchemeValue applyEqual(List<SchemeValue> arguments) throws EvalError {
        requireArgumentCount(arguments, 2, "equal?");
        return SchemeValue.booleanValue(equalValues(arguments.get(0), arguments.get(1)));
    }

    private static SchemeValue applyError(List<SchemeValue> arguments) throws EvalError {
        if (arguments.isEmpty()) {
            throw new EvalError("error");
        }

        StringBuilder builder = new StringBuilder();
        builder.append(arguments.getFirst() instanceof StringValue
                ? arguments.getFirst().display()
                : arguments.getFirst().render());
        for (int index = 1; index < arguments.size(); index++) {
            builder.append(' ');
            builder.append(arguments.get(index).render());
        }
        throw new EvalError(builder.toString());
    }

    private static SchemeValue applyVector(List<SchemeValue> arguments) {
        return new VectorValue(arguments);
    }

    private static SchemeValue applyMakeVector(List<SchemeValue> arguments) throws EvalError {
        if (arguments.size() != 1 && arguments.size() != 2) {
            throw new EvalError("make-vector: expected 1 or 2 arguments");
        }

        long length = requireIndex(arguments.getFirst(), "make-vector");
        if (length > Integer.MAX_VALUE) {
            throw new EvalError("make-vector: length too large");
        }

        SchemeValue fill = arguments.size() == 2 ? arguments.get(1) : VoidValue.INSTANCE;
        List<SchemeValue> elements = new ArrayList<>((int) length);
        for (int index = 0; index < length; index++) {
            elements.add(fill);
        }
        return new VectorValue(elements);
    }

    private static SchemeValue applyVectorRef(List<SchemeValue> arguments) throws EvalError {
        requireArgumentCount(arguments, 2, "vector-ref");
        VectorValue vector = requireVector(arguments.getFirst(), "vector-ref");
        long index = requireIndex(arguments.get(1), "vector-ref");
        if (index >= vector.length()) {
            throw new EvalError("vector-ref: index out of bounds");
        }
        return vector.ref((int) index);
    }

    private static SchemeValue applyVectorSet(List<SchemeValue> arguments) throws EvalError {
        requireArgumentCount(arguments, 3, "vector-set!");
        VectorValue vector = requireVector(arguments.get(0), "vector-set!");
        long index = requireIndex(arguments.get(1), "vector-set!");
        if (index >= vector.length()) {
            throw new EvalError("vector-set!: index out of bounds");
        }
        vector.set((int) index, arguments.get(2));
        return VoidValue.INSTANCE;
    }

    private static SchemeValue applyVectorLength(List<SchemeValue> arguments) throws EvalError {
        requireArgumentCount(arguments, 1, "vector-length");
        return new IntValue(requireVector(arguments.getFirst(), "vector-length").length());
    }

    private static SchemeValue applyVectorToList(List<SchemeValue> arguments) throws EvalError {
        requireArgumentCount(arguments, 1, "vector->list");
        return buildList(requireVector(arguments.getFirst(), "vector->list").elements());
    }

    private static SchemeValue applyListToVector(List<SchemeValue> arguments) throws EvalError {
        requireArgumentCount(arguments, 1, "list->vector");
        return new VectorValue(requireProperList(arguments.getFirst(), "list->vector"));
    }

    private static SchemeValue applyRecordConstructor(
            RecordType recordType,
            List<Integer> constructorFieldIndexes,
            List<SchemeValue> arguments,
            String constructorName
    ) throws EvalError {
        if (arguments.size() != constructorFieldIndexes.size()) {
            throw new EvalError(constructorName + ": expected " + constructorFieldIndexes.size() + " arguments");
        }

        List<SchemeValue> fields = new ArrayList<>(recordType.fieldCount());
        for (int index = 0; index < recordType.fieldCount(); index++) {
            fields.add(VoidValue.INSTANCE);
        }
        for (int index = 0; index < constructorFieldIndexes.size(); index++) {
            fields.set(constructorFieldIndexes.get(index), arguments.get(index));
        }
        return new RecordValue(recordType, fields);
    }

    private static SchemeValue applyRecordPredicate(
            RecordType recordType,
            List<SchemeValue> arguments,
            String predicateName
    ) throws EvalError {
        requireArgumentCount(arguments, 1, predicateName);
        return SchemeValue.booleanValue(arguments.getFirst() instanceof RecordValue recordValue
                && recordValue.hasType(recordType));
    }

    private static SchemeValue applyRecordAccessor(
            RecordType recordType,
            int fieldIndex,
            List<SchemeValue> arguments,
            String accessorName
    ) throws EvalError {
        requireArgumentCount(arguments, 1, accessorName);
        if (!(arguments.getFirst() instanceof RecordValue recordValue) || !recordValue.hasType(recordType)) {
            throw new EvalError(accessorName + ": expected " + recordType.name());
        }
        return recordValue.field(fieldIndex);
    }

    private static SchemeValue applyPredicate(
            List<SchemeValue> arguments,
            String name,
            Predicate<SchemeValue> predicate
    ) throws EvalError {
        requireArgumentCount(arguments, 1, name);
        return SchemeValue.booleanValue(predicate.test(arguments.getFirst()));
    }

    private static SchemeValue compare(
            List<SchemeValue> arguments,
            String name,
            NumericComparator comparator
    ) throws EvalError {
        if (arguments.size() < 2) {
            throw new EvalError(name + ": expected at least 2 arguments");
        }

        SchemeValue previous = arguments.getFirst();
        Numbers.requireNumeric(previous, name);
        for (int index = 1; index < arguments.size(); index++) {
            SchemeValue current = arguments.get(index);
            int ordering = Numbers.compareValues(previous, current, name);
            if (!comparator.test(ordering)) {
                return BoolValue.FALSE;
            }
            previous = current;
        }
        return BoolValue.TRUE;
    }

    private static SchemeValue compareChars(
            List<SchemeValue> arguments,
            String name,
            CharacterComparator comparator
    ) throws EvalError {
        if (arguments.size() < 2) {
            throw new EvalError(name + ": expected at least 2 arguments");
        }

        char previous = requireChar(arguments.getFirst(), name);
        for (int index = 1; index < arguments.size(); index++) {
            char current = requireChar(arguments.get(index), name);
            if (!comparator.test(previous, current)) {
                return BoolValue.FALSE;
            }
            previous = current;
        }
        return BoolValue.TRUE;
    }

    private static SchemeValue compareStrings(
            List<SchemeValue> arguments,
            String name,
            StringComparator comparator
    ) throws EvalError {
        if (arguments.size() < 2) {
            throw new EvalError(name + ": expected at least 2 arguments");
        }

        String previous = requireString(arguments.getFirst(), name);
        for (int index = 1; index < arguments.size(); index++) {
            String current = requireString(arguments.get(index), name);
            if (!comparator.test(previous, current)) {
                return BoolValue.FALSE;
            }
            previous = current;
        }
        return BoolValue.TRUE;
    }

    private static SchemeValue applyNumericPredicate(
            List<SchemeValue> arguments,
            String name,
            NumericPredicate predicate
    ) throws EvalError {
        requireArgumentCount(arguments, 1, name);
        return SchemeValue.booleanValue(predicate.test(requireInteger(arguments.getFirst(), name)));
    }

    private static long requireInteger(SchemeValue value, String procedure) throws EvalError {
        return Numbers.requireInteger(value, procedure);
    }

    private static String requireString(SchemeValue value, String procedure) throws EvalError {
        return requireStringValue(value, procedure).value();
    }

    private static StringValue requireStringValue(SchemeValue value, String procedure) throws EvalError {
        if (value instanceof StringValue stringValue) {
            return stringValue;
        }
        throw new EvalError(procedure + ": expected string");
    }

    private static String requireSymbol(SchemeValue value, String procedure) throws EvalError {
        if (value instanceof SymbolValue symbolValue) {
            return symbolValue.name();
        }
        throw new EvalError(procedure + ": expected symbol");
    }

    private static char requireChar(SchemeValue value, String procedure) throws EvalError {
        if (value instanceof CharValue charValue) {
            return charValue.value();
        }
        throw new EvalError(procedure + ": expected char");
    }

    private static PairValue requirePair(SchemeValue value, String procedure) throws EvalError {
        if (value instanceof PairValue pairValue) {
            return pairValue;
        }
        throw new EvalError(procedure + ": expected pair");
    }

    private static VectorValue requireVector(SchemeValue value, String procedure) throws EvalError {
        if (value instanceof VectorValue vectorValue) {
            return vectorValue;
        }
        throw new EvalError(procedure + ": expected vector");
    }

    private static long requireIndex(SchemeValue value, String procedure) throws EvalError {
        long index = requireInteger(value, procedure);
        if (index < 0L) {
            throw new EvalError(procedure + ": index out of bounds");
        }
        return index;
    }

    private static List<SchemeValue> requireProperList(SchemeValue value, String procedure) throws EvalError {
        List<SchemeValue> elements = new ArrayList<>();
        SchemeValue current = value;
        Set<PairValue> seen = new HashSet<>();
        while (current instanceof PairValue pair) {
            if (!seen.add(pair)) {
                throw new EvalError(procedure + ": expected list");
            }
            elements.add(pair.car());
            current = pair.cdr();
        }
        if (!(current instanceof EmptyListValue)) {
            throw new EvalError(procedure + ": expected list");
        }
        return elements;
    }

    private static SchemeValue nthTail(SchemeValue value, long index, String procedure) throws EvalError {
        SchemeValue current = value;
        for (long currentIndex = 0L; currentIndex < index; currentIndex++) {
            if (current instanceof PairValue pair) {
                current = pair.cdr();
                continue;
            }
            if (current instanceof EmptyListValue) {
                throw new EvalError(procedure + ": index out of bounds");
            }
            throw new EvalError(procedure + ": expected list");
        }
        if (current instanceof PairValue || current instanceof EmptyListValue) {
            return current;
        }
        throw new EvalError(procedure + ": expected list");
    }

    private static boolean isProperList(SchemeValue value) {
        SchemeValue slow = value;
        SchemeValue fast = value;
        while (true) {
            if (fast instanceof EmptyListValue) {
                return true;
            }
            if (!(fast instanceof PairValue fastPair)) {
                return false;
            }
            fast = fastPair.cdr();
            if (fast instanceof EmptyListValue) {
                return true;
            }
            if (!(fast instanceof PairValue fastPair2)) {
                return false;
            }
            fast = fastPair2.cdr();
            if (!(slow instanceof PairValue slowPair)) {
                return slow instanceof EmptyListValue;
            }
            slow = slowPair.cdr();
            if (slow == fast) {
                return false;
            }
        }
    }

    private static boolean eqValues(SchemeValue left, SchemeValue right) {
        if (left == right) {
            return true;
        }
        if (Numbers.numericEquals(left, right)) {
            return true;
        }
        if (left == null || right == null || left.getClass() != right.getClass()) {
            return false;
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

    private static boolean eqvValues(SchemeValue left, SchemeValue right) {
        return eqValues(left, right);
    }

    private static boolean equalValues(SchemeValue left, SchemeValue right) {
        return equalValues(left, right, new HashSet<>(), new HashSet<>());
    }

    private static boolean equalValues(
            SchemeValue left,
            SchemeValue right,
            Set<PairComparison> seenPairs,
            Set<VectorComparison> seenVectors
    ) {
        if (eqvValues(left, right)) {
            return true;
        }
        if (left instanceof StringValue leftString && right instanceof StringValue rightString) {
            return leftString.value().equals(rightString.value());
        }
        if (left instanceof VectorValue leftVector && right instanceof VectorValue rightVector) {
            if (!seenVectors.add(new VectorComparison(leftVector, rightVector))) {
                return true;
            }
            if (leftVector.length() != rightVector.length()) {
                return false;
            }
            for (int index = 0; index < leftVector.length(); index++) {
                if (!equalValues(leftVector.ref(index), rightVector.ref(index), seenPairs, seenVectors)) {
                    return false;
                }
            }
            return true;
        }
        if (left instanceof PairValue leftPair && right instanceof PairValue rightPair) {
            if (!seenPairs.add(new PairComparison(leftPair, rightPair))) {
                return true;
            }
            return equalValues(leftPair.car(), rightPair.car(), seenPairs, seenVectors)
                    && equalValues(leftPair.cdr(), rightPair.cdr(), seenPairs, seenVectors);
        }
        return false;
    }

    private static SchemeValue buildList(List<SchemeValue> elements) {
        SchemeValue result = EmptyListValue.INSTANCE;
        for (int index = elements.size() - 1; index >= 0; index--) {
            result = new PairValue(elements.get(index), result);
        }
        return result;
    }

    private static void requireArgumentCount(List<SchemeValue> arguments, int expected, String name)
            throws EvalError {
        if (arguments.size() != expected) {
            throw new EvalError(name + ": expected " + expected + " arguments");
        }
    }

    private static boolean isTruthy(SchemeValue value) {
        if (value instanceof BoolValue boolValue) {
            return boolValue.value();
        }
        return true;
    }

    private static boolean isProcedureValue(SchemeValue value) {
        return value instanceof ProcedureValue;
    }

    private static long gcd(long left, long right) {
        long a = Math.abs(left);
        long b = Math.abs(right);
        while (b != 0L) {
            long remainder = a % b;
            a = b;
            b = remainder;
        }
        return a;
    }

    private static long truncateToLong(SchemeValue value, String procedure) throws EvalError {
        if (value instanceof IntValue intValue) {
            return intValue.value();
        }
        if (value instanceof RationalValue rationalValue) {
            return rationalValue.numerator() / rationalValue.denominator();
        }

        NumericValue numeric = Numbers.requireNumeric(value, procedure);
        double raw = numeric.doubleValue();
        if (Double.isNaN(raw) || Double.isInfinite(raw)) {
            throw new EvalError(procedure + ": expected finite number");
        }
        return (long) (raw < 0.0 ? Math.ceil(raw) : Math.floor(raw));
    }

    private static boolean matchesArity(int requiredCount, String restParameter, int actualCount) {
        if (restParameter == null) {
            return actualCount == requiredCount;
        }
        return actualCount >= requiredCount;
    }

    private static SchemeValue packValues(List<SchemeValue> values) {
        if (values.size() == 1) {
            return values.getFirst();
        }
        return new MultiValueValue(values);
    }

    private static List<SchemeValue> unpackValues(SchemeValue value) {
        if (value instanceof MultiValueValue multiValue) {
            return multiValue.values();
        }
        return List.of(value);
    }

    private static SchemeValue requireSingleValue(SchemeValue value) throws EvalError {
        if (value instanceof MultiValueValue multiValue) {
            throw new EvalError("expected 1 value, got " + multiValue.values().size());
        }
        return value;
    }

    private void emit(String text) {
        if (outputBuffer != null) {
            outputBuffer.append(text);
        }
    }

    private record Binding(String name, SchemeExpression valueExpression) {
    }

    private record DoBinding(String name, SchemeExpression initExpression, SchemeExpression stepExpression) {
    }

    private record TailStep(SchemeExpression nextExpression, Environment nextEnvironment, SchemeValue value) {
        private static TailStep next(SchemeExpression expression, Environment environment) {
            return new TailStep(expression, environment, null);
        }

        private static TailStep done(SchemeValue value) {
            return new TailStep(null, null, value);
        }

        private boolean isDone() {
            return nextExpression == null;
        }
    }

    private record ParameterSpec(List<String> fixedParameters, String restParameter) {
    }

    private record RecordAccessorSpec(String fieldName, String accessorName) {
    }

    private record PairComparison(PairValue left, PairValue right) {
    }

    private record VectorComparison(VectorValue left, VectorValue right) {
    }

    private static List<WindFrame> copyWindPrefix(List<WindFrame> winds, int size) {
        if (size == 0) {
            return List.of();
        }
        return List.copyOf(winds.subList(0, size));
    }

    private static int sharedWindPrefix(List<WindFrame> left, List<WindFrame> right) {
        int shared = 0;
        int limit = Math.min(left.size(), right.size());
        while (shared < limit && left.get(shared) == right.get(shared)) {
            shared++;
        }
        return shared;
    }

    @FunctionalInterface
    private interface RootComputation {
        SchemeValue run() throws EvalError;
    }

    @FunctionalInterface
    private interface ContinuationFrame {
        SchemeValue resume(SchemeValue value) throws EvalError;
    }

    private record ContinuationContext(ContinuationFrame frame, ContinuationContext parent) {
    }

    private record WindFrame(SchemeValue inThunk, SchemeValue outThunk) {
    }

    private record CapturedContinuation(ContinuationContext context, List<WindFrame> winds) {
    }

    private static final class ContinuationJump extends RuntimeException {
        private final ContinuationContext continuation;
        private final SchemeValue value;
        private final List<WindFrame> sourceWinds;
        private final List<WindFrame> targetWinds;

        private ContinuationJump(
                ContinuationContext continuation,
                SchemeValue value,
                List<WindFrame> sourceWinds,
                List<WindFrame> targetWinds
        ) {
            super(null, null, false, false);
            this.continuation = continuation;
            this.value = value;
            this.sourceWinds = sourceWinds;
            this.targetWinds = targetWinds;
        }

        private ContinuationContext continuation() {
            return continuation;
        }

        private SchemeValue value() {
            return value;
        }

        private List<WindFrame> sourceWinds() {
            return sourceWinds;
        }

        private List<WindFrame> targetWinds() {
            return targetWinds;
        }
    }

    private static final class RaisedException extends RuntimeException {
        private final SchemeValue value;

        private RaisedException(SchemeValue value) {
            super(null, null, false, false);
            this.value = value;
        }

        private SchemeValue value() {
            return value;
        }
    }

    @FunctionalInterface
    private interface NumericComparator {
        boolean test(int ordering);
    }

    @FunctionalInterface
    private interface NumericPredicate {
        boolean test(long value);
    }

    @FunctionalInterface
    private interface CharacterComparator {
        boolean test(char left, char right);
    }

    @FunctionalInterface
    private interface StringComparator {
        boolean test(String left, String right);
    }
}
