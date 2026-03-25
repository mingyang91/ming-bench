package ming;

import java.math.BigInteger;
import java.util.ArrayList;
import java.util.List;

/**
 * Scheme interpreter entry point.
 * This implementation is intentionally scoped to level 01.
 */
public class Evaluator {
    /**
     * Evaluate one or more Scheme expressions and return the string
     * representation of the last result.
     */
    public String evalStr(String input) throws EvalError {
        Parser parser = new Parser(input);
        List<Expr> expressions = parser.parseProgram();
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
        return format(result);
    }

    /**
     * Evaluate Scheme expressions and return both the result string
     * and any captured output from display/write/newline.
     */
    public EvalResult evalStrWithOutput(String input) throws EvalError {
        return new EvalResult(evalStr(input), "");
    }

    private Value eval(Expr expression) throws EvalError {
        if (expression instanceof IntExpr intExpr) {
            return new IntValue(intExpr.value());
        }
        if (expression instanceof BoolExpr boolExpr) {
            return BoolValue.of(boolExpr.value());
        }
        if (expression instanceof StringExpr stringExpr) {
            return new StringValue(stringExpr.value());
        }
        if (expression instanceof SymbolExpr symbolExpr) {
            throw error("unbound variable: " + symbolExpr.name(), symbolExpr.pos());
        }
        if (expression instanceof ListExpr listExpr) {
            return evalList(listExpr);
        }
        throw new EvalError("unsupported expression");
    }

    private Value evalList(ListExpr expression) throws EvalError {
        List<Expr> elements = expression.elements();
        if (elements.isEmpty()) {
            throw error("cannot evaluate empty list", expression.pos());
        }

        Expr head = elements.getFirst();
        if (!(head instanceof SymbolExpr symbolExpr)) {
            throw error("first list element must be a procedure name", head.pos());
        }

        String name = symbolExpr.name();
        List<Expr> arguments = elements.subList(1, elements.size());
        return switch (name) {
            case "and" -> evalAnd(arguments);
            case "or" -> evalOr(arguments);
            default -> applyBuiltin(name, evalArguments(arguments), symbolExpr.pos());
        };
    }

    private List<Value> evalArguments(List<Expr> arguments) throws EvalError {
        List<Value> values = new ArrayList<>(arguments.size());
        for (Expr argument : arguments) {
            values.add(eval(argument));
        }
        return values;
    }

    private Value evalAnd(List<Expr> arguments) throws EvalError {
        Value result = BoolValue.TRUE;
        for (Expr argument : arguments) {
            result = eval(argument);
            if (!isTruthy(result)) {
                return result;
            }
        }
        return result;
    }

    private Value evalOr(List<Expr> arguments) throws EvalError {
        for (Expr argument : arguments) {
            Value result = eval(argument);
            if (isTruthy(result)) {
                return result;
            }
        }
        return BoolValue.FALSE;
    }

    private Value applyBuiltin(String name, List<Value> arguments, SourcePos pos)
            throws EvalError {
        return switch (name) {
            case "+" -> new IntValue(sum(arguments));
            case "-" -> new IntValue(subtract(arguments, pos));
            case "*" -> new IntValue(product(arguments));
            case "/" -> new IntValue(divide(arguments, pos));
            case "<" -> BoolValue.of(compare(arguments, pos, Comparison.LESS_THAN));
            case ">" -> BoolValue.of(compare(arguments, pos, Comparison.GREATER_THAN));
            case "=" -> BoolValue.of(compare(arguments, pos, Comparison.EQUAL));
            case "<=" -> BoolValue.of(compare(arguments, pos, Comparison.LESS_EQUAL));
            case "not" -> BoolValue.of(not(arguments, pos));
            default -> throw error("unknown procedure: " + name, pos);
        };
    }

    private BigInteger sum(List<Value> arguments) throws EvalError {
        BigInteger result = BigInteger.ZERO;
        for (Value argument : arguments) {
            result = result.add(asNumber(argument, "+"));
        }
        return result;
    }

    private BigInteger subtract(List<Value> arguments, SourcePos pos) throws EvalError {
        if (arguments.isEmpty()) {
            throw error("'-' expects at least 1 argument", pos);
        }

        BigInteger result = asNumber(arguments.getFirst(), "-");
        if (arguments.size() == 1) {
            return result.negate();
        }

        for (int i = 1; i < arguments.size(); i++) {
            result = result.subtract(asNumber(arguments.get(i), "-"));
        }
        return result;
    }

    private BigInteger product(List<Value> arguments) throws EvalError {
        BigInteger result = BigInteger.ONE;
        for (Value argument : arguments) {
            result = result.multiply(asNumber(argument, "*"));
        }
        return result;
    }

    private BigInteger divide(List<Value> arguments, SourcePos pos) throws EvalError {
        if (arguments.size() < 2) {
            throw error("'/' expects at least 2 arguments", pos);
        }

        BigInteger result = asNumber(arguments.getFirst(), "/");
        for (int i = 1; i < arguments.size(); i++) {
            BigInteger divisor = asNumber(arguments.get(i), "/");
            if (BigInteger.ZERO.equals(divisor)) {
                throw error("division by zero", pos);
            }
            result = result.divide(divisor);
        }
        return result;
    }

    private boolean compare(List<Value> arguments, SourcePos pos, Comparison comparison)
            throws EvalError {
        if (arguments.size() < 2) {
            throw error("comparison expects at least 2 arguments", pos);
        }

        BigInteger left = asNumber(arguments.getFirst(), comparison.name);
        for (int i = 1; i < arguments.size(); i++) {
            BigInteger right = asNumber(arguments.get(i), comparison.name);
            if (!comparison.matches(left.compareTo(right))) {
                return false;
            }
            left = right;
        }
        return true;
    }

    private boolean not(List<Value> arguments, SourcePos pos) throws EvalError {
        if (arguments.size() != 1) {
            throw error("'not' expects exactly 1 argument", pos);
        }
        return !isTruthy(arguments.getFirst());
    }

    private BigInteger asNumber(Value value, String operator) throws EvalError {
        if (value instanceof IntValue intValue) {
            return intValue.value();
        }
        throw new EvalError("'" + operator + "' expects numeric arguments");
    }

    private boolean isTruthy(Value value) {
        return !(value instanceof BoolValue boolValue) || boolValue.value();
    }

    private String format(Value value) {
        if (value instanceof IntValue intValue) {
            return intValue.value().toString();
        }
        if (value instanceof BoolValue boolValue) {
            return boolValue.value() ? "#t" : "#f";
        }
        if (value instanceof StringValue stringValue) {
            return "\"" + escapeString(stringValue.value()) + "\"";
        }
        throw new IllegalStateException("unsupported runtime value");
    }

    private String escapeString(String value) {
        StringBuilder builder = new StringBuilder(value.length());
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
        return builder.toString();
    }

    private EvalError error(String message, SourcePos pos) {
        return new EvalError(message + " at " + pos);
    }

    private enum Comparison {
        LESS_THAN("<") {
            @Override
            boolean matches(int value) {
                return value < 0;
            }
        },
        GREATER_THAN(">") {
            @Override
            boolean matches(int value) {
                return value > 0;
            }
        },
        EQUAL("=") {
            @Override
            boolean matches(int value) {
                return value == 0;
            }
        },
        LESS_EQUAL("<=") {
            @Override
            boolean matches(int value) {
                return value <= 0;
            }
        };

        private final String name;

        Comparison(String name) {
            this.name = name;
        }

        abstract boolean matches(int value);
    }

    private sealed interface Expr permits IntExpr, BoolExpr, StringExpr, SymbolExpr, ListExpr {
        SourcePos pos();
    }

    private sealed interface Value permits IntValue, BoolValue, StringValue {
    }

    private record IntExpr(BigInteger value, SourcePos pos) implements Expr {
    }

    private record BoolExpr(boolean value, SourcePos pos) implements Expr {
    }

    private record StringExpr(String value, SourcePos pos) implements Expr {
    }

    private record SymbolExpr(String name, SourcePos pos) implements Expr {
    }

    private record ListExpr(List<Expr> elements, SourcePos pos) implements Expr {
    }

    private record IntValue(BigInteger value) implements Value {
    }

    private record BoolValue(boolean value) implements Value {
        private static final BoolValue TRUE = new BoolValue(true);
        private static final BoolValue FALSE = new BoolValue(false);

        private static BoolValue of(boolean value) {
            return value ? TRUE : FALSE;
        }
    }

    private record StringValue(String value) implements Value {
    }

    private record SourcePos(int line, int column) {
        @Override
        public String toString() {
            return line + ":" + column;
        }
    }

    private static final class Parser {
        private final String input;
        private int index;
        private int line;
        private int column;

        private Parser(String input) {
            this.input = input;
            this.index = 0;
            this.line = 1;
            this.column = 1;
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
                throw error("unexpected end of input", currentPos());
            }

            char current = peek();
            if (current == '(') {
                return parseList();
            }
            if (current == '"') {
                return parseString();
            }
            if (current == '#') {
                return parseBoolean();
            }
            if (current == ')') {
                throw error("unexpected ')'", currentPos());
            }
            if (isNumberStart()) {
                return parseNumber();
            }
            return parseSymbol();
        }

        private Expr parseList() throws EvalError {
            SourcePos pos = currentPos();
            advance();

            List<Expr> elements = new ArrayList<>();
            skipIgnored();
            while (!isAtEnd() && peek() != ')') {
                elements.add(parseExpr());
                skipIgnored();
            }

            if (isAtEnd()) {
                throw error("unterminated list", pos);
            }

            advance();
            return new ListExpr(elements, pos);
        }

        private Expr parseString() throws EvalError {
            SourcePos pos = currentPos();
            advance();

            StringBuilder builder = new StringBuilder();
            while (!isAtEnd()) {
                char current = advance();
                if (current == '"') {
                    return new StringExpr(builder.toString(), pos);
                }
                if (current == '\\') {
                    if (isAtEnd()) {
                        throw error("unterminated string escape", pos);
                    }
                    builder.append(readEscape(pos));
                } else {
                    builder.append(current);
                }
            }

            throw error("unterminated string", pos);
        }

        private char readEscape(SourcePos pos) throws EvalError {
            char escaped = advance();
            return switch (escaped) {
                case 'n' -> '\n';
                case 'r' -> '\r';
                case 't' -> '\t';
                case '\\' -> '\\';
                case '"' -> '"';
                default -> throw error("unsupported string escape: \\" + escaped, pos);
            };
        }

        private Expr parseBoolean() throws EvalError {
            SourcePos pos = currentPos();
            advance();
            if (isAtEnd()) {
                throw error("incomplete boolean literal", pos);
            }

            char value = advance();
            return switch (value) {
                case 't' -> new BoolExpr(true, pos);
                case 'f' -> new BoolExpr(false, pos);
                default -> throw error("unknown boolean literal '#" + value + "'", pos);
            };
        }

        private Expr parseNumber() {
            SourcePos pos = currentPos();
            int start = index;
            if (peek() == '+' || peek() == '-') {
                advance();
            }
            while (!isAtEnd() && Character.isDigit(peek())) {
                advance();
            }
            return new IntExpr(new BigInteger(input.substring(start, index)), pos);
        }

        private Expr parseSymbol() {
            SourcePos pos = currentPos();
            int start = index;
            while (!isAtEnd() && !isDelimiter(peek())) {
                advance();
            }
            return new SymbolExpr(input.substring(start, index), pos);
        }

        private boolean isNumberStart() {
            if (isAtEnd()) {
                return false;
            }

            char current = peek();
            if (Character.isDigit(current)) {
                return true;
            }
            if ((current == '+' || current == '-') && index + 1 < input.length()) {
                return Character.isDigit(input.charAt(index + 1));
            }
            return false;
        }

        private void skipIgnored() {
            while (!isAtEnd()) {
                char current = peek();
                if (Character.isWhitespace(current)) {
                    advance();
                    continue;
                }
                if (current == ';') {
                    skipComment();
                    continue;
                }
                return;
            }
        }

        private void skipComment() {
            while (!isAtEnd() && peek() != '\n') {
                advance();
            }
        }

        private boolean isDelimiter(char value) {
            return Character.isWhitespace(value) || value == '(' || value == ')'
                    || value == '"' || value == ';';
        }

        private boolean isAtEnd() {
            return index >= input.length();
        }

        private char peek() {
            return input.charAt(index);
        }

        private char advance() {
            char value = input.charAt(index);
            index++;
            if (value == '\n') {
                line++;
                column = 1;
            } else {
                column++;
            }
            return value;
        }

        private SourcePos currentPos() {
            return new SourcePos(line, column);
        }

        private EvalError error(String message, SourcePos pos) {
            return new EvalError(message + " at " + pos);
        }
    }
}
