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

        SchemeValue result = null;
        for (SchemeExpression expression : expressions) {
            result = eval(expression);
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

    private SchemeValue eval(SchemeExpression expression) throws EvalError {
        if (expression instanceof LiteralExpression literal) {
            return literal.value();
        }

        if (expression instanceof SymbolExpression symbol) {
            return lookup(symbol.name());
        }

        return evalList((ListExpression) expression);
    }

    private SchemeValue evalList(ListExpression expression) throws EvalError {
        List<SchemeExpression> elements = expression.elements();
        if (elements.isEmpty()) {
            throw new EvalError("cannot evaluate an empty list");
        }

        SchemeExpression head = elements.getFirst();
        if (head instanceof SymbolExpression symbol) {
            String name = symbol.name();
            if ("and".equals(name)) {
                return evalAnd(elements.subList(1, elements.size()));
            }
            if ("or".equals(name)) {
                return evalOr(elements.subList(1, elements.size()));
            }
        }

        SchemeValue callee = eval(head);
        if (!(callee instanceof BuiltinProcedure procedure)) {
            throw new EvalError("not a procedure");
        }

        List<SchemeValue> arguments = new ArrayList<>(elements.size() - 1);
        for (int index = 1; index < elements.size(); index++) {
            arguments.add(eval(elements.get(index)));
        }
        return procedure.apply(arguments);
    }

    private SchemeValue evalAnd(List<SchemeExpression> expressions) throws EvalError {
        SchemeValue result = BoolValue.TRUE;
        for (SchemeExpression expression : expressions) {
            result = eval(expression);
            if (!isTruthy(result)) {
                return result;
            }
        }
        return result;
    }

    private SchemeValue evalOr(List<SchemeExpression> expressions) throws EvalError {
        SchemeValue result = BoolValue.FALSE;
        for (SchemeExpression expression : expressions) {
            result = eval(expression);
            if (isTruthy(result)) {
                return result;
            }
        }
        return result;
    }

    private SchemeValue lookup(String name) throws EvalError {
        BuiltinProcedure procedure = BUILTINS.get(name);
        if (procedure != null) {
            return procedure;
        }

        throw new EvalError("unbound variable: " + name);
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
