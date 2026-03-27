package ming;

import java.util.ArrayList;
import java.util.List;

/**
 * Scheme interpreter entry point.
 */
public class Evaluator {
    public String evalStr(String input) throws EvalError {
        Parser parser = new Parser(input);
        List<Expr> program = parser.parseProgram();
        if (program.isEmpty()) {
            throw new EvalError("empty input");
        }

        Value last = null;
        for (Expr expr : program) {
            last = eval(expr);
        }

        if (last == null) {
            throw new EvalError("empty input");
        }
        return last.toSchemeString();
    }

    public EvalResult evalStrWithOutput(String input) throws EvalError {
        return new EvalResult(evalStr(input), "");
    }

    private Value eval(Expr expr) throws EvalError {
        return switch (expr) {
            case IntExpr intExpr -> new IntValue(intExpr.value());
            case BoolExpr boolExpr -> BoolValue.of(boolExpr.value());
            case StringExpr stringExpr -> new StringValue(stringExpr.value());
            case SymbolExpr symbolExpr -> throw new EvalError("unbound variable: " + symbolExpr.name());
            case ListExpr listExpr -> evalList(listExpr);
        };
    }

    private Value evalList(ListExpr expr) throws EvalError {
        List<Expr> elements = expr.elements();
        if (elements.isEmpty()) {
            throw new EvalError("cannot evaluate empty list");
        }

        Expr head = elements.getFirst();
        if (!(head instanceof SymbolExpr symbol)) {
            throw new EvalError("first list element must be a procedure name");
        }

        List<Expr> args = elements.subList(1, elements.size());
        return switch (symbol.name()) {
            case "+" -> evalAdd(args);
            case "-" -> evalSub(args);
            case "*" -> evalMul(args);
            case "/" -> evalDiv(args);
            case "<" -> evalComparison(args, Comparison.LT);
            case ">" -> evalComparison(args, Comparison.GT);
            case "=" -> evalComparison(args, Comparison.EQ);
            case "<=" -> evalComparison(args, Comparison.LE);
            case "not" -> evalNot(args);
            case "and" -> evalAnd(args);
            case "or" -> evalOr(args);
            default -> throw new EvalError("unknown procedure: " + symbol.name());
        };
    }

    private Value evalAdd(List<Expr> args) throws EvalError {
        long sum = 0;
        for (Expr arg : args) {
            sum += requireInt(eval(arg));
        }
        return new IntValue(sum);
    }

    private Value evalSub(List<Expr> args) throws EvalError {
        if (args.isEmpty()) {
            throw new EvalError("'-' expects at least 1 argument");
        }

        long result = requireInt(eval(args.getFirst()));
        if (args.size() == 1) {
            return new IntValue(-result);
        }

        for (int i = 1; i < args.size(); i++) {
            result -= requireInt(eval(args.get(i)));
        }
        return new IntValue(result);
    }

    private Value evalMul(List<Expr> args) throws EvalError {
        long product = 1;
        for (Expr arg : args) {
            product *= requireInt(eval(arg));
        }
        return new IntValue(product);
    }

    private Value evalDiv(List<Expr> args) throws EvalError {
        if (args.size() < 2) {
            throw new EvalError("'/' expects at least 2 arguments");
        }

        long result = requireInt(eval(args.getFirst()));
        for (int i = 1; i < args.size(); i++) {
            long divisor = requireInt(eval(args.get(i)));
            if (divisor == 0) {
                throw new EvalError("division by zero");
            }
            result /= divisor;
        }
        return new IntValue(result);
    }

    private Value evalComparison(List<Expr> args, Comparison comparison) throws EvalError {
        if (args.size() < 2) {
            throw new EvalError("comparison expects at least 2 arguments");
        }

        long previous = requireInt(eval(args.getFirst()));
        for (int i = 1; i < args.size(); i++) {
            long current = requireInt(eval(args.get(i)));
            if (!comparison.test(previous, current)) {
                return BoolValue.FALSE;
            }
            previous = current;
        }
        return BoolValue.TRUE;
    }

    private Value evalNot(List<Expr> args) throws EvalError {
        if (args.size() != 1) {
            throw new EvalError("'not' expects exactly 1 argument");
        }
        return BoolValue.of(!isTruthy(eval(args.getFirst())));
    }

    private Value evalAnd(List<Expr> args) throws EvalError {
        Value result = BoolValue.TRUE;
        for (Expr arg : args) {
            result = eval(arg);
            if (!isTruthy(result)) {
                return result;
            }
        }
        return result;
    }

    private Value evalOr(List<Expr> args) throws EvalError {
        for (Expr arg : args) {
            Value result = eval(arg);
            if (isTruthy(result)) {
                return result;
            }
        }
        return BoolValue.FALSE;
    }

    private long requireInt(Value value) throws EvalError {
        if (value instanceof IntValue intValue) {
            return intValue.value();
        }
        throw new EvalError("expected number");
    }

    private boolean isTruthy(Value value) {
        return !(value instanceof BoolValue boolValue) || boolValue.value();
    }

    private enum Comparison {
        LT {
            @Override
            boolean test(long left, long right) {
                return left < right;
            }
        },
        GT {
            @Override
            boolean test(long left, long right) {
                return left > right;
            }
        },
        EQ {
            @Override
            boolean test(long left, long right) {
                return left == right;
            }
        },
        LE {
            @Override
            boolean test(long left, long right) {
                return left <= right;
            }
        };

        abstract boolean test(long left, long right);
    }

    private sealed interface Expr permits IntExpr, BoolExpr, StringExpr, SymbolExpr, ListExpr {
    }

    private record IntExpr(long value) implements Expr {
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
        String toSchemeString();
    }

    private record IntValue(long value) implements Value {
        @Override
        public String toSchemeString() {
            return Long.toString(value);
        }
    }

    private record BoolValue(boolean value) implements Value {
        private static final BoolValue TRUE = new BoolValue(true);
        private static final BoolValue FALSE = new BoolValue(false);

        private static BoolValue of(boolean value) {
            return value ? TRUE : FALSE;
        }

        @Override
        public String toSchemeString() {
            return value ? "#t" : "#f";
        }
    }

    private record StringValue(String value) implements Value {
        @Override
        public String toSchemeString() {
            return quoteString(value);
        }
    }

    private static String quoteString(String value) {
        StringBuilder builder = new StringBuilder();
        builder.append('"');
        for (int i = 0; i < value.length(); i++) {
            char ch = value.charAt(i);
            switch (ch) {
                case '\\' -> builder.append("\\\\");
                case '"' -> builder.append("\\\"");
                case '\n' -> builder.append("\\n");
                case '\r' -> builder.append("\\r");
                case '\t' -> builder.append("\\t");
                default -> builder.append(ch);
            }
        }
        builder.append('"');
        return builder.toString();
    }

    private static final class Parser {
        private final String input;
        private int index;

        private Parser(String input) {
            this.input = input;
        }

        private List<Expr> parseProgram() throws EvalError {
            List<Expr> expressions = new ArrayList<>();
            skipIgnored();
            while (!isAtEnd()) {
                expressions.add(parseExpr());
                skipIgnored();
            }
            return expressions;
        }

        private Expr parseExpr() throws EvalError {
            skipIgnored();
            if (isAtEnd()) {
                throw new EvalError("unexpected end of input");
            }

            char ch = currentChar();
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
            skipIgnored();
            while (!isAtEnd() && currentChar() != ')') {
                elements.add(parseExpr());
                skipIgnored();
            }
            if (isAtEnd()) {
                throw new EvalError("unterminated list");
            }
            index++;
            return new ListExpr(List.copyOf(elements));
        }

        private Expr parseString() throws EvalError {
            index++;
            StringBuilder builder = new StringBuilder();
            while (!isAtEnd()) {
                char ch = input.charAt(index++);
                if (ch == '"') {
                    return new StringExpr(builder.toString());
                }
                if (ch == '\\') {
                    if (isAtEnd()) {
                        throw new EvalError("unterminated string");
                    }
                    char escaped = input.charAt(index++);
                    switch (escaped) {
                        case 'n' -> builder.append('\n');
                        case 'r' -> builder.append('\r');
                        case 't' -> builder.append('\t');
                        case '\\' -> builder.append('\\');
                        case '"' -> builder.append('"');
                        default -> builder.append(escaped);
                    }
                } else {
                    builder.append(ch);
                }
            }
            throw new EvalError("unterminated string");
        }

        private Expr parseAtom() throws EvalError {
            int start = index;
            while (!isAtEnd()) {
                char ch = currentChar();
                if (Character.isWhitespace(ch) || ch == '(' || ch == ')' || ch == ';') {
                    break;
                }
                index++;
            }

            String atom = input.substring(start, index);
            if (atom.isEmpty()) {
                throw new EvalError("unexpected token");
            }
            if (atom.equals("#t")) {
                return new BoolExpr(true);
            }
            if (atom.equals("#f")) {
                return new BoolExpr(false);
            }
            if (isInteger(atom)) {
                try {
                    return new IntExpr(Long.parseLong(atom));
                } catch (NumberFormatException ex) {
                    throw new EvalError("invalid integer literal: " + atom);
                }
            }
            return new SymbolExpr(atom);
        }

        private boolean isInteger(String text) {
            if (text.isEmpty()) {
                return false;
            }
            int start = (text.charAt(0) == '+' || text.charAt(0) == '-') ? 1 : 0;
            if (start == text.length()) {
                return false;
            }
            for (int i = start; i < text.length(); i++) {
                if (!Character.isDigit(text.charAt(i))) {
                    return false;
                }
            }
            return true;
        }

        private void skipIgnored() {
            while (!isAtEnd()) {
                char ch = currentChar();
                if (Character.isWhitespace(ch)) {
                    index++;
                    continue;
                }
                if (ch == ';') {
                    while (!isAtEnd() && currentChar() != '\n') {
                        index++;
                    }
                    continue;
                }
                break;
            }
        }

        private boolean isAtEnd() {
            return index >= input.length();
        }

        private char currentChar() {
            return input.charAt(index);
        }
    }
}
