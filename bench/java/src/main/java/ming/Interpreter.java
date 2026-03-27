package ming;

import java.math.BigInteger;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

final class Interpreter {
    private static final BooleanValue TRUE = new BooleanValue(true);
    private static final BooleanValue FALSE = new BooleanValue(false);

    private final Environment globalEnv;
    private final StringBuilder output;

    Interpreter() {
        this.output = new StringBuilder();
        this.globalEnv = createGlobalEnv();
    }

    EvalResult evalProgram(String input) throws EvalError {
        Parser parser = new Parser(input);
        List<Expr> expressions = parser.parseProgram();
        if (expressions.isEmpty()) {
            throw new EvalError("input is empty");
        }

        Value lastValue = FALSE;
        for (Expr expression : expressions) {
            lastValue = eval(expression, globalEnv);
        }
        return new EvalResult(lastValue.render(), output.toString());
    }

    private Environment createGlobalEnv() {
        Environment env = new Environment(null);
        env.define("+", new BuiltinProcedure("+", this::applyAdd));
        env.define("-", new BuiltinProcedure("-", this::applySubtract));
        env.define("*", new BuiltinProcedure("*", this::applyMultiply));
        env.define("/", new BuiltinProcedure("/", this::applyDivide));
        env.define("<", new BuiltinProcedure("<", this::applyLessThan));
        env.define(">", new BuiltinProcedure(">", this::applyGreaterThan));
        env.define("=", new BuiltinProcedure("=", this::applyNumericEquals));
        env.define("<=", new BuiltinProcedure("<=", this::applyLessEqual));
        env.define("not", new BuiltinProcedure("not", this::applyNot));
        return env;
    }

    private Value eval(Expr expression, Environment env) throws EvalError {
        return switch (expression) {
            case NumberExpr numberExpr -> new NumberValue(numberExpr.value());
            case BooleanExpr booleanExpr -> booleanExpr.value() ? TRUE : FALSE;
            case StringExpr stringExpr -> new StringValue(stringExpr.value());
            case SymbolExpr symbolExpr -> env.lookup(symbolExpr.name(), symbolExpr.loc());
            case ListExpr listExpr -> evalList(listExpr, env);
        };
    }

    private Value evalList(ListExpr listExpr, Environment env) throws EvalError {
        if (listExpr.elements().isEmpty()) {
            throw error(listExpr.loc(), "cannot evaluate empty list");
        }

        Expr head = listExpr.elements().getFirst();
        if (head instanceof SymbolExpr symbolExpr) {
            String symbolName = symbolExpr.name();
            if ("and".equals(symbolName)) {
                return evalAnd(listExpr.elements().subList(1, listExpr.elements().size()), env);
            }
            if ("or".equals(symbolName)) {
                return evalOr(listExpr.elements().subList(1, listExpr.elements().size()), env);
            }
        }

        Value procedureValue = eval(head, env);
        if (!(procedureValue instanceof Procedure procedure)) {
            throw error(head.loc(), "attempted to call a non-procedure");
        }

        List<Value> arguments = new ArrayList<>();
        for (int index = 1; index < listExpr.elements().size(); index++) {
            arguments.add(eval(listExpr.elements().get(index), env));
        }
        return procedure.apply(arguments, listExpr.loc());
    }

    private Value evalAnd(List<Expr> expressions, Environment env) throws EvalError {
        Value lastValue = TRUE;
        for (Expr expression : expressions) {
            Value value = eval(expression, env);
            if (!value.isTruthy()) {
                return value;
            }
            lastValue = value;
        }
        return lastValue;
    }

    private Value evalOr(List<Expr> expressions, Environment env) throws EvalError {
        for (Expr expression : expressions) {
            Value value = eval(expression, env);
            if (value.isTruthy()) {
                return value;
            }
        }
        return FALSE;
    }

    private Value applyAdd(List<Value> arguments, SourceLoc callLoc) throws EvalError {
        Rational result = Rational.ZERO;
        for (Value argument : arguments) {
            result = result.add(requireNumber(argument, "+", callLoc));
        }
        return new NumberValue(result);
    }

    private Value applySubtract(List<Value> arguments, SourceLoc callLoc) throws EvalError {
        ensureAtLeast("-", arguments, 1, callLoc);
        Rational result = requireNumber(arguments.getFirst(), "-", callLoc);
        if (arguments.size() == 1) {
            return new NumberValue(result.negate());
        }

        for (int index = 1; index < arguments.size(); index++) {
            result = result.subtract(requireNumber(arguments.get(index), "-", callLoc));
        }
        return new NumberValue(result);
    }

    private Value applyMultiply(List<Value> arguments, SourceLoc callLoc) throws EvalError {
        Rational result = Rational.ONE;
        for (Value argument : arguments) {
            result = result.multiply(requireNumber(argument, "*", callLoc));
        }
        return new NumberValue(result);
    }

    private Value applyDivide(List<Value> arguments, SourceLoc callLoc) throws EvalError {
        ensureAtLeast("/", arguments, 1, callLoc);
        Rational result = arguments.size() == 1
                ? Rational.ONE.divide(requireNumber(arguments.getFirst(), "/", callLoc), callLoc)
                : requireNumber(arguments.getFirst(), "/", callLoc);

        int startIndex = arguments.size() == 1 ? 1 : 1;
        for (int index = startIndex; index < arguments.size(); index++) {
            result = result.divide(requireNumber(arguments.get(index), "/", callLoc), callLoc);
        }
        return new NumberValue(result);
    }

    private Value applyLessThan(List<Value> arguments, SourceLoc callLoc) throws EvalError {
        return applyComparison(arguments, callLoc, "<", (left, right) -> left.compareTo(right) < 0);
    }

    private Value applyGreaterThan(List<Value> arguments, SourceLoc callLoc) throws EvalError {
        return applyComparison(arguments, callLoc, ">", (left, right) -> left.compareTo(right) > 0);
    }

    private Value applyNumericEquals(List<Value> arguments, SourceLoc callLoc) throws EvalError {
        return applyComparison(arguments, callLoc, "=", Rational::equals);
    }

    private Value applyLessEqual(List<Value> arguments, SourceLoc callLoc) throws EvalError {
        return applyComparison(arguments, callLoc, "<=", (left, right) -> left.compareTo(right) <= 0);
    }

    private Value applyNot(List<Value> arguments, SourceLoc callLoc) throws EvalError {
        ensureExactly("not", arguments, 1, callLoc);
        return arguments.getFirst().isTruthy() ? FALSE : TRUE;
    }

    private Value applyComparison(
            List<Value> arguments,
            SourceLoc callLoc,
            String name,
            RationalComparison comparison
    ) throws EvalError {
        ensureAtLeast(name, arguments, 2, callLoc);
        Rational previous = requireNumber(arguments.getFirst(), name, callLoc);
        for (int index = 1; index < arguments.size(); index++) {
            Rational current = requireNumber(arguments.get(index), name, callLoc);
            if (!comparison.test(previous, current)) {
                return FALSE;
            }
            previous = current;
        }
        return TRUE;
    }

    private Rational requireNumber(Value value, String procedureName, SourceLoc callLoc)
            throws EvalError {
        if (value instanceof NumberValue numberValue) {
            return numberValue.value();
        }
        throw error(callLoc, procedureName + " expects numeric arguments");
    }

    private void ensureExactly(
            String procedureName,
            List<Value> arguments,
            int expected,
            SourceLoc callLoc
    ) throws EvalError {
        if (arguments.size() != expected) {
            throw error(callLoc,
                    procedureName + " expected " + expected + " arguments but got "
                            + arguments.size());
        }
    }

    private void ensureAtLeast(
            String procedureName,
            List<Value> arguments,
            int minimum,
            SourceLoc callLoc
    ) throws EvalError {
        if (arguments.size() < minimum) {
            throw error(callLoc,
                    procedureName + " expected at least " + minimum + " arguments but got "
                            + arguments.size());
        }
    }

    private static EvalError error(SourceLoc loc, String message) {
        return new EvalError(message + " at " + loc.line() + ":" + loc.column());
    }

    @FunctionalInterface
    private interface BuiltinInvoker {
        Value apply(List<Value> arguments, SourceLoc callLoc) throws EvalError;
    }

    @FunctionalInterface
    private interface RationalComparison {
        boolean test(Rational left, Rational right);
    }

    private interface Procedure {
        Value apply(List<Value> arguments, SourceLoc callLoc) throws EvalError;
    }

    private sealed interface Expr permits NumberExpr, BooleanExpr, StringExpr, SymbolExpr, ListExpr {
        SourceLoc loc();
    }

    private sealed interface Value permits NumberValue, BooleanValue, StringValue, BuiltinProcedure {
        String render();

        default boolean isTruthy() {
            return true;
        }
    }

    private record SourceLoc(int line, int column) {
    }

    private record NumberExpr(SourceLoc loc, Rational value) implements Expr {
    }

    private record BooleanExpr(SourceLoc loc, boolean value) implements Expr {
    }

    private record StringExpr(SourceLoc loc, String value) implements Expr {
    }

    private record SymbolExpr(SourceLoc loc, String name) implements Expr {
    }

    private record ListExpr(SourceLoc loc, List<Expr> elements) implements Expr {
    }

    private record NumberValue(Rational value) implements Value {
        @Override
        public String render() {
            return value.render();
        }
    }

    private record BooleanValue(boolean value) implements Value {
        @Override
        public String render() {
            return value ? "#t" : "#f";
        }

        @Override
        public boolean isTruthy() {
            return value;
        }
    }

    private record StringValue(String value) implements Value {
        @Override
        public String render() {
            return renderString(value);
        }
    }

    private static final class BuiltinProcedure implements Value, Procedure {
        private final String name;
        private final BuiltinInvoker invoker;

        private BuiltinProcedure(String name, BuiltinInvoker invoker) {
            this.name = name;
            this.invoker = invoker;
        }

        @Override
        public String render() {
            return "#<procedure:" + name + ">";
        }

        @Override
        public Value apply(List<Value> arguments, SourceLoc callLoc) throws EvalError {
            return invoker.apply(arguments, callLoc);
        }
    }

    private static final class Environment {
        private final Environment parent;
        private final Map<String, Value> bindings;

        private Environment(Environment parent) {
            this.parent = parent;
            this.bindings = new HashMap<>();
        }

        private void define(String name, Value value) {
            bindings.put(name, value);
        }

        private Value lookup(String name, SourceLoc loc) throws EvalError {
            Value value = bindings.get(name);
            if (value != null) {
                return value;
            }
            if (parent != null) {
                return parent.lookup(name, loc);
            }
            throw error(loc, "unbound variable: " + name);
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
                expressions.add(parseExpression());
                skipIgnored();
            }
            return List.copyOf(expressions);
        }

        private Expr parseExpression() throws EvalError {
            skipIgnored();
            if (isAtEnd()) {
                throw new EvalError("unexpected end of input");
            }

            SourceLoc loc = currentLoc();
            char ch = peek();
            if (ch == '(') {
                return parseList();
            }
            if (ch == ')') {
                throw error(loc, "unexpected ')'");
            }
            if (ch == '"') {
                return parseString();
            }
            return parseAtom();
        }

        private Expr parseList() throws EvalError {
            SourceLoc start = currentLoc();
            advance();

            List<Expr> elements = new ArrayList<>();
            skipIgnored();
            while (!isAtEnd() && peek() != ')') {
                elements.add(parseExpression());
                skipIgnored();
            }

            if (isAtEnd()) {
                throw error(start, "unterminated list");
            }
            advance();
            return new ListExpr(start, List.copyOf(elements));
        }

        private Expr parseString() throws EvalError {
            SourceLoc start = currentLoc();
            advance();

            StringBuilder builder = new StringBuilder();
            while (!isAtEnd()) {
                char ch = advance();
                if (ch == '"') {
                    return new StringExpr(start, builder.toString());
                }
                if (ch == '\\') {
                    if (isAtEnd()) {
                        throw error(start, "unterminated string literal");
                    }
                    builder.append(parseEscape(advance()));
                } else {
                    builder.append(ch);
                }
            }

            throw error(start, "unterminated string literal");
        }

        private char parseEscape(char escaped) {
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
            SourceLoc start = currentLoc();
            StringBuilder builder = new StringBuilder();
            while (!isAtEnd() && !isDelimiter(peek())) {
                builder.append(advance());
            }

            String token = builder.toString();
            if ("#t".equals(token)) {
                return new BooleanExpr(start, true);
            }
            if ("#f".equals(token)) {
                return new BooleanExpr(start, false);
            }
            if (isIntegerToken(token)) {
                return new NumberExpr(start, Rational.integer(new BigInteger(token)));
            }
            return new SymbolExpr(start, token);
        }

        private void skipIgnored() {
            while (!isAtEnd()) {
                char ch = peek();
                if (Character.isWhitespace(ch)) {
                    advance();
                    continue;
                }
                if (ch == ';') {
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

        private boolean isDelimiter(char ch) {
            return Character.isWhitespace(ch) || ch == '(' || ch == ')' || ch == ';';
        }

        private boolean isAtEnd() {
            return index >= input.length();
        }

        private char peek() {
            return input.charAt(index);
        }

        private char advance() {
            char ch = input.charAt(index);
            index++;
            if (ch == '\n') {
                line++;
                column = 1;
            } else {
                column++;
            }
            return ch;
        }

        private SourceLoc currentLoc() {
            return new SourceLoc(line, column);
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

            for (int index = start; index < token.length(); index++) {
                if (!Character.isDigit(token.charAt(index))) {
                    return false;
                }
            }
            return true;
        }
    }

    private record Rational(BigInteger numerator, BigInteger denominator) implements Comparable<Rational> {
        private static final Rational ZERO = integer(BigInteger.ZERO);
        private static final Rational ONE = integer(BigInteger.ONE);

        private Rational {
            if (denominator.signum() == 0) {
                throw new IllegalArgumentException("denominator cannot be zero");
            }
        }

        private static Rational integer(BigInteger value) {
            return new Rational(value, BigInteger.ONE);
        }

        private static Rational of(BigInteger numerator, BigInteger denominator) {
            if (denominator.signum() == 0) {
                throw new IllegalArgumentException("denominator cannot be zero");
            }

            BigInteger normalizedNumerator = numerator;
            BigInteger normalizedDenominator = denominator;
            if (normalizedDenominator.signum() < 0) {
                normalizedNumerator = normalizedNumerator.negate();
                normalizedDenominator = normalizedDenominator.negate();
            }

            BigInteger gcd = normalizedNumerator.gcd(normalizedDenominator);
            return new Rational(
                    normalizedNumerator.divide(gcd),
                    normalizedDenominator.divide(gcd)
            );
        }

        private Rational add(Rational other) {
            return of(
                    numerator.multiply(other.denominator).add(other.numerator.multiply(denominator)),
                    denominator.multiply(other.denominator)
            );
        }

        private Rational subtract(Rational other) {
            return of(
                    numerator.multiply(other.denominator).subtract(other.numerator.multiply(denominator)),
                    denominator.multiply(other.denominator)
            );
        }

        private Rational multiply(Rational other) {
            return of(numerator.multiply(other.numerator), denominator.multiply(other.denominator));
        }

        private Rational divide(Rational other, SourceLoc callLoc) throws EvalError {
            if (other.numerator.signum() == 0) {
                throw error(callLoc, "division by zero");
            }
            return of(numerator.multiply(other.denominator), denominator.multiply(other.numerator));
        }

        private Rational negate() {
            return new Rational(numerator.negate(), denominator);
        }

        private String render() {
            if (denominator.equals(BigInteger.ONE)) {
                return numerator.toString();
            }
            return numerator + "/" + denominator;
        }

        @Override
        public int compareTo(Rational other) {
            return numerator.multiply(other.denominator)
                    .compareTo(other.numerator.multiply(denominator));
        }
    }

    private static String renderString(String value) {
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
}
