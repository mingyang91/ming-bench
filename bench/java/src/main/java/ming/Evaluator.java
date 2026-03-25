package ming;

import java.util.ArrayList;
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
        List<Expr> expressions = new Parser(input).parseProgram();
        if (expressions.isEmpty()) {
            throw new EvalError("empty input");
        }

        Value result = null;
        for (Expr expression : expressions) {
            result = eval(expression);
        }

        if (result == null) {
            throw new EvalError("empty input");
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

    private Value eval(Expr expression) throws EvalError {
        return switch (expression) {
            case IntExpr(long value) -> new IntValue(value);
            case BoolExpr(boolean value) -> boolValue(value);
            case StringExpr(String value) -> new StringValue(value);
            case SymbolExpr(String name) -> throw new EvalError("unbound symbol: " + name);
            case ListExpr(List<Expr> elements) -> evalCall(elements);
        };
    }

    private Value evalCall(List<Expr> elements) throws EvalError {
        if (elements.isEmpty()) {
            throw new EvalError("cannot evaluate empty list");
        }
        if (!(elements.getFirst() instanceof SymbolExpr(String name))) {
            throw new EvalError("operator must be a symbol");
        }

        List<Expr> arguments = elements.subList(1, elements.size());
        return switch (name) {
            case "and" -> evalAnd(arguments);
            case "or" -> evalOr(arguments);
            case "not" -> evalNot(arguments);
            case "+" -> evalAdd(arguments);
            case "-" -> evalSubtract(arguments);
            case "*" -> evalMultiply(arguments);
            case "/" -> evalDivide(arguments);
            case "<" -> evalComparison(arguments, Comparison.LESS_THAN);
            case ">" -> evalComparison(arguments, Comparison.GREATER_THAN);
            case "=" -> evalComparison(arguments, Comparison.EQUAL);
            case "<=" -> evalComparison(arguments, Comparison.LESS_EQUAL);
            default -> throw new EvalError("unknown procedure: " + name);
        };
    }

    private Value evalAnd(List<Expr> arguments) throws EvalError {
        Value result = TRUE_VALUE;
        for (Expr argument : arguments) {
            result = eval(argument);
            if (!isTruthy(result)) {
                return result;
            }
        }
        return result;
    }

    private Value evalOr(List<Expr> arguments) throws EvalError {
        Value result = FALSE_VALUE;
        for (Expr argument : arguments) {
            result = eval(argument);
            if (isTruthy(result)) {
                return result;
            }
        }
        return result;
    }

    private Value evalNot(List<Expr> arguments) throws EvalError {
        expectArgumentCount(arguments, 1, "not");
        return boolValue(!isTruthy(eval(arguments.getFirst())));
    }

    private Value evalAdd(List<Expr> arguments) throws EvalError {
        long total = 0L;
        for (Expr argument : arguments) {
            total += requireInt(eval(argument), "+");
        }
        return new IntValue(total);
    }

    private Value evalSubtract(List<Expr> arguments) throws EvalError {
        if (arguments.isEmpty()) {
            throw new EvalError("wrong argument count for -");
        }

        long result = requireInt(eval(arguments.getFirst()), "-");
        if (arguments.size() == 1) {
            return new IntValue(-result);
        }

        for (int i = 1; i < arguments.size(); i++) {
            result -= requireInt(eval(arguments.get(i)), "-");
        }
        return new IntValue(result);
    }

    private Value evalMultiply(List<Expr> arguments) throws EvalError {
        long total = 1L;
        for (Expr argument : arguments) {
            total *= requireInt(eval(argument), "*");
        }
        return new IntValue(total);
    }

    private Value evalDivide(List<Expr> arguments) throws EvalError {
        if (arguments.size() < 2) {
            throw new EvalError("wrong argument count for /");
        }

        long result = requireInt(eval(arguments.getFirst()), "/");
        for (int i = 1; i < arguments.size(); i++) {
            long divisor = requireInt(eval(arguments.get(i)), "/");
            if (divisor == 0L) {
                throw new EvalError("division by zero");
            }
            result /= divisor;
        }
        return new IntValue(result);
    }

    private Value evalComparison(List<Expr> arguments, Comparison comparison) throws EvalError {
        if (arguments.size() < 2) {
            throw new EvalError("wrong argument count for " + comparison.name);
        }

        long left = requireInt(eval(arguments.getFirst()), comparison.name);
        for (int i = 1; i < arguments.size(); i++) {
            long right = requireInt(eval(arguments.get(i)), comparison.name);
            if (!comparison.test(left, right)) {
                return FALSE_VALUE;
            }
            left = right;
        }
        return TRUE_VALUE;
    }

    private void expectArgumentCount(List<Expr> arguments, int expected, String name)
            throws EvalError {
        if (arguments.size() != expected) {
            throw new EvalError("wrong argument count for " + name);
        }
    }

    private long requireInt(Value value, String name) throws EvalError {
        if (value instanceof IntValue(long number)) {
            return number;
        }
        throw new EvalError("expected number for " + name);
    }

    private boolean isTruthy(Value value) {
        return !(value instanceof BoolValue(boolean bool) && !bool);
    }

    private BoolValue boolValue(boolean value) {
        return value ? TRUE_VALUE : FALSE_VALUE;
    }

    private sealed interface Expr permits IntExpr, BoolExpr, StringExpr, SymbolExpr, ListExpr {}

    private record IntExpr(long value) implements Expr {}

    private record BoolExpr(boolean value) implements Expr {}

    private record StringExpr(String value) implements Expr {}

    private record SymbolExpr(String name) implements Expr {}

    private record ListExpr(List<Expr> elements) implements Expr {}

    private sealed interface Value permits IntValue, BoolValue, StringValue {
        String render();
    }

    private record IntValue(long value) implements Value {
        @Override
        public String render() {
            return Long.toString(value);
        }
    }

    private record BoolValue(boolean value) implements Value {
        @Override
        public String render() {
            return value ? "#t" : "#f";
        }
    }

    private record StringValue(String value) implements Value {
        @Override
        public String render() {
            String escaped = value
                    .replace("\\", "\\\\")
                    .replace("\"", "\\\"")
                    .replace("\n", "\\n")
                    .replace("\t", "\\t");
            return "\"" + escaped + "\"";
        }
    }

    private record Comparison(String name, Comparator comparator) {
        private static final Comparison LESS_THAN = new Comparison("<", (a, b) -> a < b);
        private static final Comparison GREATER_THAN = new Comparison(">", (a, b) -> a > b);
        private static final Comparison EQUAL = new Comparison("=", (a, b) -> a == b);
        private static final Comparison LESS_EQUAL = new Comparison("<=", (a, b) -> a <= b);

        private boolean test(long left, long right) {
            return comparator.test(left, right);
        }
    }

    @FunctionalInterface
    private interface Comparator {
        boolean test(long left, long right);
    }

    private static final BoolValue TRUE_VALUE = new BoolValue(true);
    private static final BoolValue FALSE_VALUE = new BoolValue(false);

    private static final class Parser {
        private final String input;
        private int index;

        private Parser(String input) {
            this.input = input;
        }

        private List<Expr> parseProgram() throws EvalError {
            List<Expr> expressions = new ArrayList<>();
            skipWhitespace();
            while (!isAtEnd()) {
                expressions.add(parseExpr());
                skipWhitespace();
            }
            return expressions;
        }

        private Expr parseExpr() throws EvalError {
            skipWhitespace();
            if (isAtEnd()) {
                throw new EvalError("unexpected end of input");
            }

            char ch = input.charAt(index);
            if (ch == '(') {
                return parseList();
            }
            if (ch == ')') {
                throw new EvalError("unexpected )");
            }
            if (ch == '"') {
                return parseString();
            }
            if (ch == '#') {
                return parseBoolean();
            }
            return parseAtom();
        }

        private Expr parseList() throws EvalError {
            index++;
            List<Expr> elements = new ArrayList<>();
            skipWhitespace();

            while (true) {
                if (isAtEnd()) {
                    throw new EvalError("unterminated list");
                }
                if (input.charAt(index) == ')') {
                    index++;
                    return new ListExpr(elements);
                }
                elements.add(parseExpr());
                skipWhitespace();
            }
        }

        private Expr parseString() throws EvalError {
            index++;
            StringBuilder value = new StringBuilder();
            while (!isAtEnd()) {
                char ch = input.charAt(index++);
                if (ch == '"') {
                    return new StringExpr(value.toString());
                }
                if (ch == '\\') {
                    if (isAtEnd()) {
                        throw new EvalError("unterminated string");
                    }
                    char escaped = input.charAt(index++);
                    value.append(switch (escaped) {
                        case 'n' -> '\n';
                        case 't' -> '\t';
                        case '"' -> '"';
                        case '\\' -> '\\';
                        default -> escaped;
                    });
                } else {
                    value.append(ch);
                }
            }
            throw new EvalError("unterminated string");
        }

        private Expr parseBoolean() throws EvalError {
            if (matchesToken("#t")) {
                index += 2;
                return new BoolExpr(true);
            }
            if (matchesToken("#f")) {
                index += 2;
                return new BoolExpr(false);
            }
            throw new EvalError("invalid boolean literal");
        }

        private boolean matchesToken(String token) {
            int end = index + token.length();
            if (end > input.length()) {
                return false;
            }
            if (!input.startsWith(token, index)) {
                return false;
            }
            return end == input.length() || isDelimiter(input.charAt(end));
        }

        private Expr parseAtom() throws EvalError {
            int start = index;
            while (!isAtEnd() && !isDelimiter(input.charAt(index))) {
                index++;
            }

            String token = input.substring(start, index);
            if (token.isEmpty()) {
                throw new EvalError("unexpected token");
            }

            if (isIntegerToken(token)) {
                try {
                    return new IntExpr(Long.parseLong(token));
                } catch (NumberFormatException e) {
                    throw new EvalError("invalid integer literal: " + token);
                }
            }
            return new SymbolExpr(token);
        }

        private boolean isIntegerToken(String token) {
            if (token.isEmpty()) {
                return false;
            }

            int start = 0;
            if (token.charAt(0) == '-' || token.charAt(0) == '+') {
                if (token.length() == 1) {
                    return false;
                }
                start = 1;
            }

            for (int i = start; i < token.length(); i++) {
                if (!Character.isDigit(token.charAt(i))) {
                    return false;
                }
            }
            return true;
        }

        private void skipWhitespace() {
            while (!isAtEnd() && Character.isWhitespace(input.charAt(index))) {
                index++;
            }
        }

        private boolean isAtEnd() {
            return index >= input.length();
        }

        private boolean isDelimiter(char ch) {
            return Character.isWhitespace(ch) || ch == '(' || ch == ')';
        }
    }
}
