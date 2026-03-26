package ming;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

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
        if (expression instanceof LiteralExpression literal) {
            return literal.value();
        }

        if (expression instanceof SymbolExpression symbol) {
            return environment.lookup(symbol.name());
        }

        return evalList((ListExpression) expression, environment);
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

        SchemeValue result = VoidValue.INSTANCE;
        for (SchemeExpression expression : procedure.body()) {
            result = eval(expression, invocationEnvironment);
        }
        return result;
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

    private static boolean isTruthy(SchemeValue value) {
        if (value instanceof BoolValue boolValue) {
            return boolValue.value();
        }
        return true;
    }

    @FunctionalInterface
    private interface NumericComparator {
        boolean test(long left, long right);
    }
}
