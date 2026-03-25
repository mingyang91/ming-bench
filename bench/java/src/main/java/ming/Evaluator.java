package ming;

import java.math.BigInteger;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

/**
 * Scheme interpreter entry point.
 */
public class Evaluator {
    /**
     * Evaluate one or more Scheme expressions and return the string
     * representation of the last result.
     */
    public String evalStr(String input) throws EvalError {
        return format(evalProgram(input));
    }

    /**
     * Evaluate Scheme expressions and return both the result string
     * and any captured output from display/write/newline.
     */
    public EvalResult evalStrWithOutput(String input) throws EvalError {
        return new EvalResult(format(evalProgram(input)), "");
    }

    private Value evalProgram(String input) throws EvalError {
        Parser parser = new Parser(input);
        List<Expr> expressions = parser.parseProgram();
        if (expressions.isEmpty()) {
            throw new EvalError("empty input");
        }

        Environment environment = createGlobalEnvironment();
        Value result = VoidValue.INSTANCE;
        for (Expr expression : expressions) {
            result = eval(expression, environment);
        }
        return result;
    }

    private Environment createGlobalEnvironment() {
        Environment environment = new Environment(null);
        environment.define("+", new BuiltinProcedure("+", this::applyAdd));
        environment.define("-", new BuiltinProcedure("-", this::applySubtract));
        environment.define("*", new BuiltinProcedure("*", this::applyMultiply));
        environment.define("/", new BuiltinProcedure("/", this::applyDivide));
        environment.define("<", new BuiltinProcedure("<", (args, pos) ->
                BoolValue.of(compare(args, pos, Comparison.LESS_THAN))));
        environment.define(">", new BuiltinProcedure(">", (args, pos) ->
                BoolValue.of(compare(args, pos, Comparison.GREATER_THAN))));
        environment.define("=", new BuiltinProcedure("=", (args, pos) ->
                BoolValue.of(compare(args, pos, Comparison.EQUAL))));
        environment.define("<=", new BuiltinProcedure("<=", (args, pos) ->
                BoolValue.of(compare(args, pos, Comparison.LESS_EQUAL))));
        environment.define("not", new BuiltinProcedure("not", (args, pos) ->
                BoolValue.of(not(args, pos))));
        return environment;
    }

    private Value eval(Expr expression, Environment environment) throws EvalError {
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
            return environment.lookup(symbolExpr.name(), symbolExpr.pos());
        }
        if (expression instanceof ListExpr listExpr) {
            return evalList(listExpr, environment);
        }
        throw new EvalError("unsupported expression");
    }

    private Value evalList(ListExpr expression, Environment environment) throws EvalError {
        List<Expr> elements = expression.elements();
        if (elements.isEmpty()) {
            throw error("cannot evaluate empty list", expression.pos());
        }

        Expr head = elements.getFirst();
        if (head instanceof SymbolExpr symbolExpr) {
            List<Expr> arguments = elements.subList(1, elements.size());
            return switch (symbolExpr.name()) {
                case "define" -> evalDefine(arguments, environment, symbolExpr.pos());
                case "if" -> evalIf(arguments, environment, symbolExpr.pos());
                case "quote" -> evalQuote(arguments, symbolExpr.pos());
                case "lambda" -> evalLambda(arguments, environment, symbolExpr.pos());
                case "and" -> evalAnd(arguments, environment);
                case "or" -> evalOr(arguments, environment);
                default -> apply(eval(head, environment),
                        evalArguments(arguments, environment), expression.pos());
            };
        }

        return apply(eval(head, environment),
                evalArguments(elements.subList(1, elements.size()), environment),
                expression.pos());
    }

    private Value evalDefine(List<Expr> arguments, Environment environment, SourcePos pos)
            throws EvalError {
        if (arguments.isEmpty()) {
            throw error("'define' expects a target and a value", pos);
        }

        Expr target = arguments.getFirst();
        if (target instanceof SymbolExpr symbolExpr) {
            if (arguments.size() != 2) {
                throw error("'define' expects exactly 2 arguments", pos);
            }
            Value value = eval(arguments.get(1), environment);
            environment.define(symbolExpr.name(), value);
            return VoidValue.INSTANCE;
        }

        if (target instanceof ListExpr signatureExpr) {
            List<Expr> signature = signatureExpr.elements();
            if (signature.isEmpty()) {
                throw error("function definition requires a name", target.pos());
            }
            if (!(signature.getFirst() instanceof SymbolExpr nameExpr)) {
                throw error("function definition requires a symbol name", signatureExpr.pos());
            }
            if (arguments.size() < 2) {
                throw error("function definition requires a body", pos);
            }

            List<String> parameters = parseParameterNames(
                    signature.subList(1, signature.size()), signatureExpr.pos());
            List<Expr> body = new ArrayList<>(arguments.subList(1, arguments.size()));
            ClosureProcedure procedure = new ClosureProcedure(
                    nameExpr.name(), parameters, body, environment);
            environment.define(nameExpr.name(), procedure);
            return VoidValue.INSTANCE;
        }

        throw error("'define' target must be a symbol or parameter list", target.pos());
    }

    private Value evalIf(List<Expr> arguments, Environment environment, SourcePos pos)
            throws EvalError {
        if (arguments.size() != 3) {
            throw error("'if' expects exactly 3 arguments", pos);
        }

        Value condition = eval(arguments.get(0), environment);
        if (isTruthy(condition)) {
            return eval(arguments.get(1), environment);
        }
        return eval(arguments.get(2), environment);
    }

    private Value evalQuote(List<Expr> arguments, SourcePos pos) throws EvalError {
        if (arguments.size() != 1) {
            throw error("'quote' expects exactly 1 argument", pos);
        }
        return quote(arguments.getFirst());
    }

    private Value evalLambda(List<Expr> arguments, Environment environment, SourcePos pos)
            throws EvalError {
        if (arguments.size() < 2) {
            throw error("'lambda' expects a parameter list and a body", pos);
        }
        if (!(arguments.getFirst() instanceof ListExpr parameterExpr)) {
            throw error("'lambda' parameters must be a list", arguments.getFirst().pos());
        }

        List<String> parameters = parseParameterNames(parameterExpr.elements(), parameterExpr.pos());
        List<Expr> body = new ArrayList<>(arguments.subList(1, arguments.size()));
        return new ClosureProcedure(null, parameters, body, environment);
    }

    private List<String> parseParameterNames(List<Expr> parameterExprs, SourcePos pos)
            throws EvalError {
        List<String> parameters = new ArrayList<>(parameterExprs.size());
        for (Expr parameterExpr : parameterExprs) {
            if (!(parameterExpr instanceof SymbolExpr symbolExpr)) {
                throw error("parameters must be symbols", pos);
            }
            parameters.add(symbolExpr.name());
        }
        return parameters;
    }

    private List<Value> evalArguments(List<Expr> arguments, Environment environment)
            throws EvalError {
        List<Value> values = new ArrayList<>(arguments.size());
        for (Expr argument : arguments) {
            values.add(eval(argument, environment));
        }
        return values;
    }

    private Value evalAnd(List<Expr> arguments, Environment environment) throws EvalError {
        Value result = BoolValue.TRUE;
        for (Expr argument : arguments) {
            result = eval(argument, environment);
            if (!isTruthy(result)) {
                return result;
            }
        }
        return result;
    }

    private Value evalOr(List<Expr> arguments, Environment environment) throws EvalError {
        for (Expr argument : arguments) {
            Value result = eval(argument, environment);
            if (isTruthy(result)) {
                return result;
            }
        }
        return BoolValue.FALSE;
    }

    private Value apply(Value procedure, List<Value> arguments, SourcePos pos)
            throws EvalError {
        if (procedure instanceof BuiltinProcedure builtinProcedure) {
            return builtinProcedure.implementation().apply(arguments, pos);
        }
        if (procedure instanceof ClosureProcedure closureProcedure) {
            if (arguments.size() != closureProcedure.parameters().size()) {
                throw error("wrong number of arguments", pos);
            }

            Environment callEnvironment = new Environment(closureProcedure.environment());
            for (int i = 0; i < closureProcedure.parameters().size(); i++) {
                callEnvironment.define(closureProcedure.parameters().get(i), arguments.get(i));
            }

            Value result = VoidValue.INSTANCE;
            for (Expr bodyExpr : closureProcedure.body()) {
                result = eval(bodyExpr, callEnvironment);
            }
            return result;
        }
        throw error("not a procedure", pos);
    }

    private Value applyAdd(List<Value> arguments, SourcePos pos) throws EvalError {
        return new IntValue(sum(arguments, pos));
    }

    private Value applySubtract(List<Value> arguments, SourcePos pos) throws EvalError {
        return new IntValue(subtract(arguments, pos));
    }

    private Value applyMultiply(List<Value> arguments, SourcePos pos) throws EvalError {
        return new IntValue(product(arguments, pos));
    }

    private Value applyDivide(List<Value> arguments, SourcePos pos) throws EvalError {
        return new IntValue(divide(arguments, pos));
    }

    private BigInteger sum(List<Value> arguments, SourcePos pos) throws EvalError {
        BigInteger result = BigInteger.ZERO;
        for (Value argument : arguments) {
            result = result.add(asNumber(argument, "+", pos));
        }
        return result;
    }

    private BigInteger subtract(List<Value> arguments, SourcePos pos) throws EvalError {
        if (arguments.isEmpty()) {
            throw error("'-' expects at least 1 argument", pos);
        }

        BigInteger result = asNumber(arguments.getFirst(), "-", pos);
        if (arguments.size() == 1) {
            return result.negate();
        }

        for (int i = 1; i < arguments.size(); i++) {
            result = result.subtract(asNumber(arguments.get(i), "-", pos));
        }
        return result;
    }

    private BigInteger product(List<Value> arguments, SourcePos pos) throws EvalError {
        BigInteger result = BigInteger.ONE;
        for (Value argument : arguments) {
            result = result.multiply(asNumber(argument, "*", pos));
        }
        return result;
    }

    private BigInteger divide(List<Value> arguments, SourcePos pos) throws EvalError {
        if (arguments.size() < 2) {
            throw error("'/' expects at least 2 arguments", pos);
        }

        BigInteger result = asNumber(arguments.getFirst(), "/", pos);
        for (int i = 1; i < arguments.size(); i++) {
            BigInteger divisor = asNumber(arguments.get(i), "/", pos);
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

        BigInteger left = asNumber(arguments.getFirst(), comparison.name, pos);
        for (int i = 1; i < arguments.size(); i++) {
            BigInteger right = asNumber(arguments.get(i), comparison.name, pos);
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

    private BigInteger asNumber(Value value, String operator, SourcePos pos) throws EvalError {
        if (value instanceof IntValue intValue) {
            return intValue.value();
        }
        throw error("'" + operator + "' expects numeric arguments", pos);
    }

    private Value quote(Expr expression) throws EvalError {
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
            return new SymbolValue(symbolExpr.name());
        }
        if (expression instanceof ListExpr listExpr) {
            return quoteList(listExpr.elements());
        }
        throw new EvalError("unsupported quoted expression");
    }

    private Value quoteList(List<Expr> expressions) throws EvalError {
        Value result = EmptyListValue.INSTANCE;
        for (int i = expressions.size() - 1; i >= 0; i--) {
            result = new PairValue(quote(expressions.get(i)), result);
        }
        return result;
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
        if (value instanceof SymbolValue symbolValue) {
            return symbolValue.name();
        }
        if (value instanceof EmptyListValue) {
            return "()";
        }
        if (value instanceof PairValue pairValue) {
            return formatPair(pairValue);
        }
        if (value instanceof VoidValue) {
            return "#<void>";
        }
        if (value instanceof ProcedureValue) {
            return "#<procedure>";
        }
        throw new IllegalStateException("unsupported runtime value");
    }

    private String formatPair(PairValue pairValue) {
        StringBuilder builder = new StringBuilder("(");
        Value current = pairValue;
        boolean first = true;
        while (current instanceof PairValue pair) {
            if (!first) {
                builder.append(' ');
            }
            builder.append(format(pair.car()));
            current = pair.cdr();
            first = false;
        }

        if (current instanceof EmptyListValue) {
            builder.append(')');
        } else {
            builder.append(" . ").append(format(current)).append(')');
        }
        return builder.toString();
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

    private sealed interface Value permits IntValue, BoolValue, StringValue, SymbolValue,
            EmptyListValue, PairValue, ProcedureValue, VoidValue {
    }

    private sealed interface ProcedureValue extends Value permits BuiltinProcedure,
            ClosureProcedure {
    }

    @FunctionalInterface
    private interface BuiltinImplementation {
        Value apply(List<Value> arguments, SourcePos pos) throws EvalError;
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

    private record SymbolValue(String name) implements Value {
    }

    private record EmptyListValue() implements Value {
        private static final EmptyListValue INSTANCE = new EmptyListValue();
    }

    private record PairValue(Value car, Value cdr) implements Value {
    }

    private record BuiltinProcedure(String name, BuiltinImplementation implementation)
            implements ProcedureValue {
    }

    private record ClosureProcedure(String name, List<String> parameters, List<Expr> body,
                                    Environment environment) implements ProcedureValue {
    }

    private record VoidValue() implements Value {
        private static final VoidValue INSTANCE = new VoidValue();
    }

    private record SourcePos(int line, int column) {
        @Override
        public String toString() {
            return line + ":" + column;
        }
    }

    private static final class Environment {
        private final Environment parent;
        private final Map<String, Value> bindings = new HashMap<>();

        private Environment(Environment parent) {
            this.parent = parent;
        }

        private void define(String name, Value value) {
            bindings.put(name, value);
        }

        private Value lookup(String name, SourcePos pos) throws EvalError {
            if (bindings.containsKey(name)) {
                return bindings.get(name);
            }
            if (parent != null) {
                return parent.lookup(name, pos);
            }
            throw new EvalError("unbound variable: " + name + " at " + pos);
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
