package ming;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.function.Predicate;

/**
 * Scheme interpreter entry point.
 * Agents implement this class.
 */
public class Evaluator {
    private static final Map<String, BuiltinProcedure> BUILTINS = createBuiltins();

    /**
     * Evaluate one or more Scheme expressions and return the string
     * representation of the last result.
     */
    public String evalStr(String input) throws EvalError {
        List<SchemeExpression> expressions = new SchemeParser(input).parseProgram();
        if (expressions.isEmpty()) {
            throw new EvalError("empty input");
        }

        Environment environment = createTopLevelEnvironment();
        SchemeValue result = VoidValue.INSTANCE;
        for (SchemeExpression expression : expressions) {
            result = eval(expression, environment);
        }

        return result.render();
    }

    /**
     * Evaluate Scheme expressions and return both the result string
     * and any captured output from display/write/newline.
     */
    public EvalResult evalStrWithOutput(String input) throws EvalError {
        return new EvalResult(evalStr(input), "");
    }

    private SchemeValue eval(SchemeExpression expression, Environment environment) throws EvalError {
        try {
            if (expression instanceof LiteralExpression literal) {
                return literal.value();
            }

            if (expression instanceof SymbolExpression symbol) {
                return environment.lookup(symbol.name());
            }

            return evalList((ListExpression) expression, environment);
        } catch (EvalError error) {
            throw error.withPosition(expression.position());
        }
    }

    private SchemeValue evalList(ListExpression expression, Environment environment) throws EvalError {
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
            if ("quote".equals(name)) {
                return evalQuote(elements);
            }
            if ("lambda".equals(name)) {
                return evalLambda(elements, environment);
            }
        }

        SchemeValue callee = eval(head, environment);
        if (!(callee instanceof BuiltinProcedure) && !(callee instanceof LambdaProcedure)) {
            throw new EvalError("not a procedure");
        }

        List<SchemeValue> arguments = new ArrayList<>(elements.size() - 1);
        for (int index = 1; index < elements.size(); index++) {
            arguments.add(eval(elements.get(index), environment));
        }
        return applyProcedure(callee, arguments);
    }

    private SchemeValue evalAnd(List<SchemeExpression> expressions, Environment environment) throws EvalError {
        SchemeValue result = BoolValue.TRUE;
        for (SchemeExpression expression : expressions) {
            result = eval(expression, environment);
            if (!isTruthy(result)) {
                return result;
            }
        }
        return result;
    }

    private SchemeValue evalOr(List<SchemeExpression> expressions, Environment environment) throws EvalError {
        SchemeValue result = BoolValue.FALSE;
        for (SchemeExpression expression : expressions) {
            result = eval(expression, environment);
            if (isTruthy(result)) {
                return result;
            }
        }
        return result;
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

            SchemeValue value = eval(elements.get(2), environment);
            environment.define(symbol.name(), value);
            return VoidValue.INSTANCE;
        }

        if (target instanceof ListExpression signature) {
            return evalFunctionDefine(signature, elements.subList(2, elements.size()), environment);
        }

        throw new EvalError("define: invalid definition target");
    }

    private static Map<String, BuiltinProcedure> createBuiltins() {
        Map<String, BuiltinProcedure> builtins = new HashMap<>();
        builtins.put("+", new BuiltinProcedure("+", Evaluator::applyAdd));
        builtins.put("-", new BuiltinProcedure("-", Evaluator::applySubtract));
        builtins.put("*", new BuiltinProcedure("*", Evaluator::applyMultiply));
        builtins.put("/", new BuiltinProcedure("/", Evaluator::applyDivide));
        builtins.put("<", new BuiltinProcedure("<", args -> compare(args, "<",
                (left, right) -> left < right)));
        builtins.put(">", new BuiltinProcedure(">", args -> compare(args, ">",
                (left, right) -> left > right)));
        builtins.put("=", new BuiltinProcedure("=", args -> compare(args, "=",
                (left, right) -> left == right)));
        builtins.put("<=", new BuiltinProcedure("<=", args -> compare(args, "<=",
                (left, right) -> left <= right)));
        builtins.put("not", new BuiltinProcedure("not", Evaluator::applyNot));
        builtins.put("cons", new BuiltinProcedure("cons", Evaluator::applyCons));
        builtins.put("car", new BuiltinProcedure("car", Evaluator::applyCar));
        builtins.put("cdr", new BuiltinProcedure("cdr", Evaluator::applyCdr));
        builtins.put("null?", new BuiltinProcedure("null?", args -> applyPredicate(args, "null?",
                value -> value instanceof EmptyListValue)));
        builtins.put("list", new BuiltinProcedure("list", Evaluator::applyList));
        builtins.put("length", new BuiltinProcedure("length", Evaluator::applyLength));
        builtins.put("append", new BuiltinProcedure("append", Evaluator::applyAppend));
        builtins.put("string?", new BuiltinProcedure("string?", args -> applyPredicate(args, "string?",
                value -> value instanceof StringValue)));
        builtins.put("number?", new BuiltinProcedure("number?", args -> applyPredicate(args, "number?",
                value -> value instanceof IntValue)));
        builtins.put("boolean?", new BuiltinProcedure("boolean?", args -> applyPredicate(args, "boolean?",
                value -> value instanceof BoolValue)));
        builtins.put("pair?", new BuiltinProcedure("pair?", args -> applyPredicate(args, "pair?",
                value -> value instanceof PairValue)));
        builtins.put("symbol?", new BuiltinProcedure("symbol?", args -> applyPredicate(args, "symbol?",
                value -> value instanceof SymbolValue)));
        return Map.copyOf(builtins);
    }

    private static Environment createTopLevelEnvironment() {
        Environment environment = new Environment(null);
        for (Map.Entry<String, BuiltinProcedure> entry : BUILTINS.entrySet()) {
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

        List<String> parameters = parseParameters(
                signatureElements.subList(1, signatureElements.size()),
                "define"
        );
        LambdaProcedure procedure = new LambdaProcedure(
                nameSymbol.name(),
                List.copyOf(parameters),
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

        SchemeValue condition = eval(elements.get(1), environment);
        if (isTruthy(condition)) {
            return eval(elements.get(2), environment);
        }
        if (elements.size() == 4) {
            return eval(elements.get(3), environment);
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

            SchemeValue testValue = eval(testExpression, environment);
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
            values.add(eval(binding.valueExpression(), environment));
        }

        Environment letEnvironment = new Environment(environment);
        for (int index = 0; index < bindings.size(); index++) {
            letEnvironment.define(bindings.get(index).name(), values.get(index));
        }
        return evalSequence(body, letEnvironment);
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
            arguments.add(eval(binding.valueExpression(), environment));
        }

        Environment letEnvironment = new Environment(environment);
        LambdaProcedure procedure = new LambdaProcedure(
                name,
                List.copyOf(parameters),
                List.copyOf(elements.subList(3, elements.size())),
                letEnvironment
        );
        letEnvironment.define(name, procedure);
        return applyLambda(procedure, arguments);
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
        if (!(elements.get(1) instanceof ListExpression parametersExpression)) {
            throw new EvalError("lambda: expected parameter list");
        }

        List<String> parameters = parseParameters(parametersExpression.elements(), "lambda");
        List<SchemeExpression> body = List.copyOf(elements.subList(2, elements.size()));
        return new LambdaProcedure(null, parameters, body, environment);
    }

    private SchemeValue evalSequence(List<SchemeExpression> expressions, Environment environment) throws EvalError {
        SchemeValue result = VoidValue.INSTANCE;
        for (SchemeExpression expression : expressions) {
            result = eval(expression, environment);
        }
        return result;
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

    private SchemeValue applyProcedure(SchemeValue callee, List<SchemeValue> arguments) throws EvalError {
        if (callee instanceof BuiltinProcedure builtinProcedure) {
            return builtinProcedure.apply(arguments);
        }
        if (callee instanceof LambdaProcedure lambdaProcedure) {
            return applyLambda(lambdaProcedure, arguments);
        }
        throw new EvalError("not a procedure");
    }

    private SchemeValue applyLambda(LambdaProcedure procedure, List<SchemeValue> arguments) throws EvalError {
        if (arguments.size() != procedure.parameters().size()) {
            throw new EvalError(procedure.render() + ": expected " + procedure.parameters().size()
                    + " arguments");
        }

        Environment invocationEnvironment = new Environment(procedure.closureEnvironment());
        for (int index = 0; index < procedure.parameters().size(); index++) {
            invocationEnvironment.define(procedure.parameters().get(index), arguments.get(index));
        }

        return evalSequence(procedure.body(), invocationEnvironment);
    }

    private List<String> parseParameters(List<SchemeExpression> parameterExpressions, String formName)
            throws EvalError {
        List<String> parameters = new ArrayList<>(parameterExpressions.size());
        for (SchemeExpression parameterExpression : parameterExpressions) {
            if (!(parameterExpression instanceof SymbolExpression symbol)) {
                throw new EvalError(formName + ": expected symbol parameter");
            }
            parameters.add(symbol.name());
        }
        return parameters;
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
        long total = 0L;
        for (SchemeValue argument : arguments) {
            total += requireInteger(argument, "+");
        }
        return new IntValue(total);
    }

    private static SchemeValue applySubtract(List<SchemeValue> arguments) throws EvalError {
        if (arguments.isEmpty()) {
            throw new EvalError("-: expected at least 1 argument");
        }

        long result = requireInteger(arguments.getFirst(), "-");
        if (arguments.size() == 1) {
            return new IntValue(-result);
        }

        for (int index = 1; index < arguments.size(); index++) {
            result -= requireInteger(arguments.get(index), "-");
        }
        return new IntValue(result);
    }

    private static SchemeValue applyMultiply(List<SchemeValue> arguments) throws EvalError {
        long total = 1L;
        for (SchemeValue argument : arguments) {
            total *= requireInteger(argument, "*");
        }
        return new IntValue(total);
    }

    private static SchemeValue applyDivide(List<SchemeValue> arguments) throws EvalError {
        if (arguments.size() < 2) {
            throw new EvalError("/: expected at least 2 arguments");
        }

        long result = requireInteger(arguments.getFirst(), "/");
        for (int index = 1; index < arguments.size(); index++) {
            long divisor = requireInteger(arguments.get(index), "/");
            if (divisor == 0L) {
                throw new EvalError("division by zero");
            }
            result /= divisor;
        }
        return new IntValue(result);
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

        long previous = requireInteger(arguments.getFirst(), name);
        for (int index = 1; index < arguments.size(); index++) {
            long current = requireInteger(arguments.get(index), name);
            if (!comparator.test(previous, current)) {
                return BoolValue.FALSE;
            }
            previous = current;
        }
        return BoolValue.TRUE;
    }

    private static long requireInteger(SchemeValue value, String procedure) throws EvalError {
        if (value instanceof IntValue intValue) {
            return intValue.value();
        }

        throw new EvalError(procedure + ": expected integer");
    }

    private static PairValue requirePair(SchemeValue value, String procedure) throws EvalError {
        if (value instanceof PairValue pairValue) {
            return pairValue;
        }
        throw new EvalError(procedure + ": expected pair");
    }

    private static List<SchemeValue> requireProperList(SchemeValue value, String procedure) throws EvalError {
        List<SchemeValue> elements = new ArrayList<>();
        SchemeValue current = value;
        while (current instanceof PairValue pair) {
            elements.add(pair.car());
            current = pair.cdr();
        }
        if (!(current instanceof EmptyListValue)) {
            throw new EvalError(procedure + ": expected list");
        }
        return elements;
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

    private record Binding(String name, SchemeExpression valueExpression) {
    }

    @FunctionalInterface
    private interface NumericComparator {
        boolean test(long left, long right);
    }
}
