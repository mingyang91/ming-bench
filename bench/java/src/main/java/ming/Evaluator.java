package ming;

import java.util.ArrayList;
import java.util.List;

/**
 * Scheme interpreter entry point.
 * Agents implement this class.
 */
public class Evaluator {
    private static final BoolValue TRUE = new BoolValue(true);
    private static final BoolValue FALSE = new BoolValue(false);

    /**
     * Evaluate one or more Scheme expressions and return the string
     * representation of the last result.
     */
    public String evalStr(String input) throws EvalError {
        List<Expr> expressions = new Parser(input).parseProgram();
        if (expressions.isEmpty()) {
            throw new EvalError("input did not contain any expressions");
        }

        Value result = FALSE;
        for (Expr expression : expressions) {
            result = eval(expression);
        }
        return render(result);
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
            case BoolExpr boolExpr -> boolValue(boolExpr.value());
            case StringExpr stringExpr -> new StringValue(stringExpr.value());
            case SymbolExpr symbolExpr -> throw new EvalError("unbound variable: "
                    + symbolExpr.name());
            case ListExpr listExpr -> evalList(listExpr);
        };
    }

    private Value evalList(ListExpr listExpr) throws EvalError {
        List<Expr> elements = listExpr.elements();
        if (elements.isEmpty()) {
            throw new EvalError("cannot evaluate an empty list");
        }

        Expr operatorExpr = elements.get(0);
        if (!(operatorExpr instanceof SymbolExpr symbolExpr)) {
            throw new EvalError("first list element must name a procedure");
        }

        List<Expr> arguments = elements.subList(1, elements.size());
        return switch (symbolExpr.name()) {
            case "and" -> evalAnd(arguments);
            case "or" -> evalOr(arguments);
            default -> applyBuiltin(symbolExpr.name(), evalAll(arguments));
        };
    }

    private List<Value> evalAll(List<Expr> arguments) throws EvalError {
        List<Value> values = new ArrayList<>(arguments.size());
        for (Expr argument : arguments) {
            values.add(eval(argument));
        }
        return values;
    }

    private Value evalAnd(List<Expr> arguments) throws EvalError {
        Value last = TRUE;
        for (Expr argument : arguments) {
            last = eval(argument);
            if (!isTruthy(last)) {
                return last;
            }
        }
        return last;
    }

    private Value evalOr(List<Expr> arguments) throws EvalError {
        Value last = FALSE;
        for (Expr argument : arguments) {
            last = eval(argument);
            if (isTruthy(last)) {
                return last;
            }
        }
        return last;
    }

    private Value applyBuiltin(String name, List<Value> arguments) throws EvalError {
        return switch (name) {
            case "+" -> add(arguments);
            case "-" -> subtract(arguments);
            case "*" -> multiply(arguments);
            case "/" -> divide(arguments);
            case "<" -> compare(arguments, name);
            case ">" -> compare(arguments, name);
            case "=" -> compare(arguments, name);
            case "<=" -> compare(arguments, name);
            case "not" -> not(arguments);
            default -> throw new EvalError("unbound variable: " + name);
        };
    }

    private Value add(List<Value> arguments) throws EvalError {
        long total = 0L;
        for (Value argument : arguments) {
            total += requireInt(argument, "+");
        }
        return new IntValue(total);
    }

    private Value subtract(List<Value> arguments) throws EvalError {
        requireMinArgs("-", arguments, 1);
        long result = requireInt(arguments.get(0), "-");
        if (arguments.size() == 1) {
            return new IntValue(-result);
        }

        for (int index = 1; index < arguments.size(); index++) {
            result -= requireInt(arguments.get(index), "-");
        }
        return new IntValue(result);
    }

    private Value multiply(List<Value> arguments) throws EvalError {
        long total = 1L;
        for (Value argument : arguments) {
            total *= requireInt(argument, "*");
        }
        return new IntValue(total);
    }

    private Value divide(List<Value> arguments) throws EvalError {
        requireMinArgs("/", arguments, 2);
        long result = requireInt(arguments.get(0), "/");
        for (int index = 1; index < arguments.size(); index++) {
            long divisor = requireInt(arguments.get(index), "/");
            if (divisor == 0L) {
                throw new EvalError("division by zero");
            }
            result /= divisor;
        }
        return new IntValue(result);
    }

    private Value compare(List<Value> arguments, String operator) throws EvalError {
        requireMinArgs(operator, arguments, 2);
        for (int index = 0; index < arguments.size() - 1; index++) {
            long left = requireInt(arguments.get(index), operator);
            long right = requireInt(arguments.get(index + 1), operator);
            if (!comparePair(left, right, operator)) {
                return FALSE;
            }
        }
        return TRUE;
    }

    private boolean comparePair(long left, long right, String operator)
            throws EvalError {
        return switch (operator) {
            case "<" -> left < right;
            case ">" -> left > right;
            case "=" -> left == right;
            case "<=" -> left <= right;
            default -> throw new EvalError("unknown comparison operator: " + operator);
        };
    }

    private Value not(List<Value> arguments) throws EvalError {
        requireExactArgs("not", arguments, 1);
        return boolValue(!isTruthy(arguments.get(0)));
    }

    private void requireMinArgs(String name, List<Value> arguments, int min)
            throws EvalError {
        if (arguments.size() < min) {
            throw new EvalError(name + " expected at least "
                    + min + " argument(s)");
        }
    }

    private void requireExactArgs(String name, List<Value> arguments, int exact)
            throws EvalError {
        if (arguments.size() != exact) {
            throw new EvalError(name + " expected exactly "
                    + exact + " argument(s)");
        }
    }

    private long requireInt(Value value, String operator) throws EvalError {
        if (value instanceof IntValue intValue) {
            return intValue.value();
        }
        throw new EvalError(operator + " expects numeric arguments");
    }

    private boolean isTruthy(Value value) {
        return !(value instanceof BoolValue boolValue) || boolValue.value();
    }

    private BoolValue boolValue(boolean value) {
        return value ? TRUE : FALSE;
    }

    private String render(Value value) {
        return switch (value) {
            case IntValue intValue -> Long.toString(intValue.value());
            case BoolValue boolValue -> boolValue.value() ? "#t" : "#f";
            case StringValue stringValue -> quote(stringValue.value());
        };
    }

    private String quote(String value) {
        StringBuilder builder = new StringBuilder();
        builder.append('"');
        for (int index = 0; index < value.length(); index++) {
            char ch = value.charAt(index);
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

    private sealed interface Expr permits IntExpr, BoolExpr, StringExpr, SymbolExpr,
            ListExpr {
    }

    private sealed interface Value permits IntValue, BoolValue, StringValue {
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

    private record IntValue(long value) implements Value {
    }

    private record BoolValue(boolean value) implements Value {
    }

    private record StringValue(String value) implements Value {
    }

    private static final class Parser {
        private final String input;
        private int index;
        private int line = 1;
        private int column = 1;

        private Parser(String input) {
            this.input = input;
        }

        private List<Expr> parseProgram() throws EvalError {
            List<Expr> expressions = new ArrayList<>();
            skipTrivia();
            while (!isAtEnd()) {
                expressions.add(parseExpression());
                skipTrivia();
            }
            return expressions;
        }

        private Expr parseExpression() throws EvalError {
            skipTrivia();
            if (isAtEnd()) {
                throw error("unexpected end of input");
            }

            char current = currentChar();
            if (current == '(') {
                return parseList();
            }
            if (current == '"') {
                return parseString();
            }
            if (current == ')') {
                throw error("unexpected ')'");
            }
            return parseAtom();
        }

        private Expr parseList() throws EvalError {
            consume('(');
            List<Expr> elements = new ArrayList<>();
            skipTrivia();
            while (!isAtEnd() && currentChar() != ')') {
                elements.add(parseExpression());
                skipTrivia();
            }

            if (isAtEnd()) {
                throw error("unterminated list");
            }

            consume(')');
            return new ListExpr(List.copyOf(elements));
        }

        private Expr parseString() throws EvalError {
            consume('"');
            StringBuilder builder = new StringBuilder();
            while (!isAtEnd()) {
                char current = advance();
                if (current == '"') {
                    return new StringExpr(builder.toString());
                }
                if (current == '\\') {
                    if (isAtEnd()) {
                        throw error("unterminated string escape");
                    }
                    builder.append(unescape(advance()));
                    continue;
                }
                builder.append(current);
            }
            throw error("unterminated string literal");
        }

        private char unescape(char escaped) {
            return switch (escaped) {
                case 'n' -> '\n';
                case 'r' -> '\r';
                case 't' -> '\t';
                case '"' -> '"';
                case '\\' -> '\\';
                default -> escaped;
            };
        }

        private Expr parseAtom() {
            int start = index;
            while (!isAtEnd() && !isDelimiter(currentChar())) {
                advance();
            }
            String token = input.substring(start, index);
            return parseAtomToken(token);
        }

        private Expr parseAtomToken(String token) {
            return switch (token) {
                case "#t" -> new BoolExpr(true);
                case "#f" -> new BoolExpr(false);
                default -> parseNumberOrSymbol(token);
            };
        }

        private Expr parseNumberOrSymbol(String token) {
            if (isIntegerToken(token)) {
                return new IntExpr(Long.parseLong(token));
            }
            return new SymbolExpr(token);
        }

        private boolean isIntegerToken(String token) {
            if (token.isEmpty()) {
                return false;
            }

            int start = 0;
            char first = token.charAt(0);
            if (first == '+' || first == '-') {
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

        private void skipTrivia() {
            while (!isAtEnd()) {
                char current = currentChar();
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
            while (!isAtEnd() && currentChar() != '\n') {
                advance();
            }
        }

        private boolean isDelimiter(char ch) {
            return Character.isWhitespace(ch) || ch == '(' || ch == ')' || ch == ';';
        }

        private void consume(char expected) throws EvalError {
            if (isAtEnd() || currentChar() != expected) {
                throw error("expected '" + expected + "'");
            }
            advance();
        }

        private char currentChar() {
            return input.charAt(index);
        }

        private boolean isAtEnd() {
            return index >= input.length();
        }

        private char advance() {
            char current = input.charAt(index);
            index++;
            if (current == '\n') {
                line++;
                column = 1;
            } else {
                column++;
            }
            return current;
        }

        private EvalError error(String message) {
            return new EvalError(message + " at " + line + ":" + column);
        }
    }
}
