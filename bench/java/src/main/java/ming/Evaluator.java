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
        Parser parser = new Parser(input);
        Value lastValue = null;

        while (parser.hasMore()) {
            lastValue = eval(parser.parseExpr());
        }

        if (lastValue == null) {
            throw new EvalError("empty input");
        }

        return lastValue.render();
    }

    /**
     * Evaluate Scheme expressions and return both the result string
     * and any captured output from display/write/newline.
     */
    public EvalResult evalStrWithOutput(String input) throws EvalError {
        return new EvalResult(evalStr(input), "");
    }

    private Value eval(Expr expr) throws EvalError {
        return switch (expr) {
            case IntExpr intExpr -> new IntValue(intExpr.value());
            case BoolExpr boolExpr -> BoolValue.of(boolExpr.value());
            case StringExpr stringExpr -> new StringValue(stringExpr.value());
            case SymbolExpr symbolExpr ->
                    throw new EvalError("unbound variable: " + symbolExpr.name());
            case ListExpr listExpr -> evalList(listExpr);
        };
    }

    private Value evalList(ListExpr listExpr) throws EvalError {
        List<Expr> elements = listExpr.elements();
        if (elements.isEmpty()) {
            throw new EvalError("cannot evaluate empty list");
        }

        Expr head = elements.getFirst();
        if (!(head instanceof SymbolExpr symbolExpr)) {
            throw new EvalError("operator must be a symbol");
        }

        String name = symbolExpr.name();
        List<Expr> argExprs = elements.subList(1, elements.size());

        return switch (name) {
            case "and" -> evalAnd(argExprs);
            case "or" -> evalOr(argExprs);
            default -> applyBuiltin(name, evalArgs(argExprs));
        };
    }

    private List<Value> evalArgs(List<Expr> argExprs) throws EvalError {
        List<Value> values = new ArrayList<>(argExprs.size());
        for (Expr argExpr : argExprs) {
            values.add(eval(argExpr));
        }
        return values;
    }

    private Value evalAnd(List<Expr> argExprs) throws EvalError {
        Value result = BoolValue.TRUE;
        for (Expr argExpr : argExprs) {
            result = eval(argExpr);
            if (!isTruthy(result)) {
                return result;
            }
        }
        return result;
    }

    private Value evalOr(List<Expr> argExprs) throws EvalError {
        Value lastValue = BoolValue.FALSE;
        for (Expr argExpr : argExprs) {
            Value value = eval(argExpr);
            if (isTruthy(value)) {
                return value;
            }
            lastValue = value;
        }
        return lastValue;
    }

    private Value applyBuiltin(String name, List<Value> args) throws EvalError {
        return switch (name) {
            case "+" -> new IntValue(sum(args));
            case "-" -> new IntValue(subtract(args));
            case "*" -> new IntValue(multiply(args));
            case "/" -> new IntValue(divide(args));
            case "<" -> BoolValue.of(compareIncreasing(args, Comparison.STRICTLY_LESS));
            case ">" -> BoolValue.of(compareIncreasing(args, Comparison.STRICTLY_GREATER));
            case "=" -> BoolValue.of(compareIncreasing(args, Comparison.EQUAL));
            case "<=" -> BoolValue.of(compareIncreasing(args, Comparison.LESS_OR_EQUAL));
            case "not" -> {
                requireArity(name, args, 1);
                yield BoolValue.of(!isTruthy(args.getFirst()));
            }
            default -> throw new EvalError("unknown builtin: " + name);
        };
    }

    private int sum(List<Value> args) throws EvalError {
        int total = 0;
        for (Value arg : args) {
            total += expectInt(arg);
        }
        return total;
    }

    private int subtract(List<Value> args) throws EvalError {
        requireAtLeast("-", args, 1);

        if (args.size() == 1) {
            return -expectInt(args.getFirst());
        }

        int result = expectInt(args.getFirst());
        for (int index = 1; index < args.size(); index++) {
            result -= expectInt(args.get(index));
        }
        return result;
    }

    private int multiply(List<Value> args) throws EvalError {
        int total = 1;
        for (Value arg : args) {
            total *= expectInt(arg);
        }
        return total;
    }

    private int divide(List<Value> args) throws EvalError {
        requireAtLeast("/", args, 2);

        int result = expectInt(args.getFirst());
        for (int index = 1; index < args.size(); index++) {
            int divisor = expectInt(args.get(index));
            if (divisor == 0) {
                throw new EvalError("division by zero");
            }
            result /= divisor;
        }
        return result;
    }

    private boolean compareIncreasing(List<Value> args, Comparison comparison)
            throws EvalError {
        requireAtLeast(comparison.symbol, args, 2);

        int previous = expectInt(args.getFirst());
        for (int index = 1; index < args.size(); index++) {
            int current = expectInt(args.get(index));
            if (!comparison.matches(previous, current)) {
                return false;
            }
            previous = current;
        }
        return true;
    }

    private int expectInt(Value value) throws EvalError {
        if (value instanceof IntValue intValue) {
            return intValue.value();
        }
        throw new EvalError("expected number");
    }

    private void requireArity(String name, List<Value> args, int expected)
            throws EvalError {
        if (args.size() != expected) {
            throw new EvalError(
                    "wrong number of arguments for " + name + ": expected " + expected
                            + ", got " + args.size()
            );
        }
    }

    private void requireAtLeast(String name, List<Value> args, int minimum)
            throws EvalError {
        if (args.size() < minimum) {
            throw new EvalError(
                    "wrong number of arguments for " + name + ": expected at least "
                            + minimum + ", got " + args.size()
            );
        }
    }

    private boolean isTruthy(Value value) {
        return !(value instanceof BoolValue boolValue) || boolValue.value();
    }

    private sealed interface Expr permits IntExpr, BoolExpr, StringExpr, SymbolExpr, ListExpr {
    }

    private record IntExpr(int value) implements Expr {
    }

    private record BoolExpr(boolean value) implements Expr {
    }

    private record StringExpr(String value) implements Expr {
    }

    private record SymbolExpr(String name) implements Expr {
    }

    private record ListExpr(List<Expr> elements) implements Expr {
    }

    private sealed interface Value permits IntValue, BoolValue, StringValue {
        String render();
    }

    private record IntValue(int value) implements Value {
        @Override
        public String render() {
            return Integer.toString(value);
        }
    }

    private record BoolValue(boolean value) implements Value {
        private static final BoolValue TRUE = new BoolValue(true);
        private static final BoolValue FALSE = new BoolValue(false);

        private static BoolValue of(boolean value) {
            return value ? TRUE : FALSE;
        }

        @Override
        public String render() {
            return value ? "#t" : "#f";
        }
    }

    private record StringValue(String value) implements Value {
        @Override
        public String render() {
            return "\"" + escapeString(value) + "\"";
        }
    }

    private enum Comparison {
        STRICTLY_LESS("<") {
            @Override
            boolean matches(int left, int right) {
                return left < right;
            }
        },
        STRICTLY_GREATER(">") {
            @Override
            boolean matches(int left, int right) {
                return left > right;
            }
        },
        EQUAL("=") {
            @Override
            boolean matches(int left, int right) {
                return left == right;
            }
        },
        LESS_OR_EQUAL("<=") {
            @Override
            boolean matches(int left, int right) {
                return left <= right;
            }
        };

        private final String symbol;

        Comparison(String symbol) {
            this.symbol = symbol;
        }

        abstract boolean matches(int left, int right);
    }

    private static String escapeString(String value) {
        StringBuilder builder = new StringBuilder(value.length());
        for (int index = 0; index < value.length(); index++) {
            char ch = value.charAt(index);
            switch (ch) {
                case '\\' -> builder.append("\\\\");
                case '"' -> builder.append("\\\"");
                case '\n' -> builder.append("\\n");
                case '\t' -> builder.append("\\t");
                case '\r' -> builder.append("\\r");
                default -> builder.append(ch);
            }
        }
        return builder.toString();
    }

    private static final class Parser {
        private final String input;
        private int index;

        private Parser(String input) {
            this.input = input;
        }

        private boolean hasMore() {
            skipWhitespace();
            return index < input.length();
        }

        private Expr parseExpr() throws EvalError {
            skipWhitespace();
            if (index >= input.length()) {
                throw new EvalError("unexpected end of input");
            }

            char ch = input.charAt(index);
            return switch (ch) {
                case '(' -> parseList();
                case '"' -> parseString();
                case ')' -> throw new EvalError("unexpected ')'");
                default -> parseAtom();
            };
        }

        private Expr parseList() throws EvalError {
            index++;
            List<Expr> elements = new ArrayList<>();

            while (true) {
                skipWhitespace();
                if (index >= input.length()) {
                    throw new EvalError("unterminated list");
                }
                if (input.charAt(index) == ')') {
                    index++;
                    return new ListExpr(elements);
                }
                elements.add(parseExpr());
            }
        }

        private Expr parseString() throws EvalError {
            index++;
            StringBuilder builder = new StringBuilder();

            while (index < input.length()) {
                char ch = input.charAt(index++);
                if (ch == '"') {
                    return new StringExpr(builder.toString());
                }
                if (ch == '\\') {
                    builder.append(parseEscape());
                    continue;
                }
                builder.append(ch);
            }

            throw new EvalError("unterminated string");
        }

        private char parseEscape() throws EvalError {
            if (index >= input.length()) {
                throw new EvalError("unterminated string escape");
            }

            char ch = input.charAt(index++);
            return switch (ch) {
                case '\\' -> '\\';
                case '"' -> '"';
                case 'n' -> '\n';
                case 't' -> '\t';
                case 'r' -> '\r';
                default -> ch;
            };
        }

        private Expr parseAtom() {
            int start = index;
            while (index < input.length()) {
                char ch = input.charAt(index);
                if (Character.isWhitespace(ch) || ch == '(' || ch == ')') {
                    break;
                }
                index++;
            }

            String token = input.substring(start, index);
            if (token.equals("#t")) {
                return new BoolExpr(true);
            }
            if (token.equals("#f")) {
                return new BoolExpr(false);
            }
            if (token.matches("[+-]?\\d+")) {
                return new IntExpr(Integer.parseInt(token));
            }
            return new SymbolExpr(token);
        }

        private void skipWhitespace() {
            while (index < input.length() && Character.isWhitespace(input.charAt(index))) {
                index++;
            }
        }
    }
}
