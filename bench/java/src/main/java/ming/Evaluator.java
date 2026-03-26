package ming;

import java.util.List;

/**
 * Scheme interpreter entry point.
 * Agents implement this class.
 */
public class Evaluator {
    /**
     * Evaluate one or more Scheme expressions and return the string
     * representation of the last result.
     */
    public String evalStr(String input) throws EvalError {
        return evalStrWithOutput(input).result();
    }

    /**
     * Evaluate Scheme expressions and return both the result string
     * and any captured output from display/write/newline.
     */
    public EvalResult evalStrWithOutput(String input) throws EvalError {
        List<Expr> expressions = new Parser(input).parseProgram();
        if (expressions.isEmpty()) {
            throw new EvalError("expected at least one expression");
        }

        Value lastValue = null;
        for (Expr expression : expressions) {
            lastValue = eval(expression);
        }

        return new EvalResult(lastValue.render(), "");
    }

    private Value eval(Expr expression) throws EvalError {
        return switch (expression) {
            case IntExpr intExpr -> new IntValue(intExpr.value());
            case BoolExpr boolExpr -> new BoolValue(boolExpr.value());
            case StringExpr stringExpr -> new StringValue(stringExpr.value());
            case SymbolExpr symbolExpr -> throw new EvalError("unbound symbol: " + symbolExpr.name());
            case ListExpr listExpr -> evalList(listExpr);
        };
    }

    private Value evalList(ListExpr listExpr) throws EvalError {
        List<Expr> elements = listExpr.elements();
        if (elements.isEmpty()) {
            throw new EvalError("cannot evaluate empty list");
        }

        Expr operator = elements.getFirst();
        if (!(operator instanceof SymbolExpr symbolExpr)) {
            throw new EvalError("operator must be a symbol");
        }

        List<Expr> arguments = elements.subList(1, elements.size());
        return switch (symbolExpr.name()) {
            case "and" -> evalAnd(arguments);
            case "or" -> evalOr(arguments);
            case "not" -> evalNot(arguments);
            case "+" -> evalAdd(arguments);
            case "-" -> evalSubtract(arguments);
            case "*" -> evalMultiply(arguments);
            case "/" -> evalDivide(arguments);
            case "<" -> evalComparison("<", arguments);
            case ">" -> evalComparison(">", arguments);
            case "=" -> evalComparison("=", arguments);
            case "<=" -> evalComparison("<=", arguments);
            default -> throw new EvalError("unknown operator: " + symbolExpr.name());
        };
    }

    private Value evalAnd(List<Expr> arguments) throws EvalError {
        Value last = new BoolValue(true);
        for (Expr argument : arguments) {
            last = eval(argument);
            if (!last.isTruthy()) {
                return last;
            }
        }
        return last;
    }

    private Value evalOr(List<Expr> arguments) throws EvalError {
        Value last = new BoolValue(false);
        for (Expr argument : arguments) {
            last = eval(argument);
            if (last.isTruthy()) {
                return last;
            }
        }
        return last;
    }

    private Value evalNot(List<Expr> arguments) throws EvalError {
        requireExactArity("not", arguments, 1);
        return new BoolValue(!eval(arguments.getFirst()).isTruthy());
    }

    private Value evalAdd(List<Expr> arguments) throws EvalError {
        long total = 0L;
        for (Expr argument : arguments) {
            total += expectInt(eval(argument), "+");
        }
        return new IntValue(total);
    }

    private Value evalSubtract(List<Expr> arguments) throws EvalError {
        requireMinimumArity("-", arguments, 1);
        long result = expectInt(eval(arguments.getFirst()), "-");
        if (arguments.size() == 1) {
            return new IntValue(-result);
        }

        for (int i = 1; i < arguments.size(); i++) {
            result -= expectInt(eval(arguments.get(i)), "-");
        }
        return new IntValue(result);
    }

    private Value evalMultiply(List<Expr> arguments) throws EvalError {
        long product = 1L;
        for (Expr argument : arguments) {
            product *= expectInt(eval(argument), "*");
        }
        return new IntValue(product);
    }

    private Value evalDivide(List<Expr> arguments) throws EvalError {
        requireMinimumArity("/", arguments, 2);
        long result = expectInt(eval(arguments.getFirst()), "/");
        for (int i = 1; i < arguments.size(); i++) {
            long divisor = expectInt(eval(arguments.get(i)), "/");
            if (divisor == 0L) {
                throw new EvalError("division by zero");
            }
            result /= divisor;
        }
        return new IntValue(result);
    }

    private Value evalComparison(String operator, List<Expr> arguments) throws EvalError {
        requireMinimumArity(operator, arguments, 2);

        long previous = expectInt(eval(arguments.getFirst()), operator);
        for (int i = 1; i < arguments.size(); i++) {
            long current = expectInt(eval(arguments.get(i)), operator);
            if (!compare(operator, previous, current)) {
                return new BoolValue(false);
            }
            previous = current;
        }

        return new BoolValue(true);
    }

    private boolean compare(String operator, long left, long right) {
        return switch (operator) {
            case "<" -> left < right;
            case ">" -> left > right;
            case "=" -> left == right;
            case "<=" -> left <= right;
            default -> false;
        };
    }

    private long expectInt(Value value, String operator) throws EvalError {
        if (value instanceof IntValue intValue) {
            return intValue.value();
        }
        throw new EvalError(operator + " expects integer arguments");
    }

    private void requireExactArity(String name, List<Expr> arguments, int expected)
            throws EvalError {
        if (arguments.size() != expected) {
            throw new EvalError(name + " expected " + expected + " argument(s)");
        }
    }

    private void requireMinimumArity(String name, List<Expr> arguments, int minimum)
            throws EvalError {
        if (arguments.size() < minimum) {
            throw new EvalError(name + " expected at least " + minimum + " argument(s)");
        }
    }
}
