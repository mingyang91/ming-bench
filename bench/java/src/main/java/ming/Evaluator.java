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
    /**
     * Evaluate one or more Scheme expressions and return the string
     * representation of the last result.
     */
    public String evalStr(String input) throws EvalError {
        return evaluateProgram(input).render();
    }

    /**
     * Evaluate Scheme expressions and return both the result string
     * and any captured output from display/write/newline.
     */
    public EvalResult evalStrWithOutput(String input) throws EvalError {
        Value result = evaluateProgram(input);
        return new EvalResult(result.render(), "");
    }

    private Value evaluateProgram(String input) throws EvalError {
        List<Expr> expressions = new Parser(input).parseProgram();
        if (expressions.isEmpty()) {
            throw new EvalError("empty input");
        }

        Environment global = createGlobalEnvironment();
        Value result = VOID_VALUE;
        for (Expr expression : expressions) {
            result = eval(expression, global);
        }
        return result;
    }

    private Environment createGlobalEnvironment() {
        Environment env = new Environment(null);
        installBuiltin(env, "+", this::builtinAdd);
        installBuiltin(env, "-", this::builtinSubtract);
        installBuiltin(env, "*", this::builtinMultiply);
        installBuiltin(env, "/", this::builtinDivide);
        installBuiltin(env, "<", args -> builtinComparison(args, Comparison.LESS_THAN));
        installBuiltin(env, ">", args -> builtinComparison(args, Comparison.GREATER_THAN));
        installBuiltin(env, "=", args -> builtinComparison(args, Comparison.EQUAL));
        installBuiltin(env, "<=", args -> builtinComparison(args, Comparison.LESS_EQUAL));
        installBuiltin(env, "not", this::builtinNot);
        return env;
    }

    private void installBuiltin(Environment env, String name, BuiltinImplementation implementation) {
        env.define(name, new BuiltinProcedure(name, implementation));
    }

    private Value eval(Expr expression, Environment env) throws EvalError {
        return switch (expression) {
            case IntExpr(long value) -> new IntValue(value);
            case BoolExpr(boolean value) -> boolValue(value);
            case StringExpr(String value) -> new StringValue(value);
            case SymbolExpr(String name) -> env.lookup(name);
            case ListExpr(List<Expr> elements) -> evalList(elements, env);
        };
    }

    private Value evalList(List<Expr> elements, Environment env) throws EvalError {
        if (elements.isEmpty()) {
            throw new EvalError("cannot evaluate empty list");
        }

        Expr operatorExpr = elements.get(0);
        List<Expr> arguments = elements.subList(1, elements.size());
        if (operatorExpr instanceof SymbolExpr(String name)) {
            return switch (name) {
                case "and" -> evalAnd(arguments, env);
                case "or" -> evalOr(arguments, env);
                case "define" -> evalDefine(arguments, env);
                case "if" -> evalIf(arguments, env);
                case "lambda" -> evalLambda(arguments, env);
                case "quote" -> evalQuote(arguments);
                default -> apply(eval(operatorExpr, env), evalArguments(arguments, env));
            };
        }
        return apply(eval(operatorExpr, env), evalArguments(arguments, env));
    }

    private List<Value> evalArguments(List<Expr> arguments, Environment env) throws EvalError {
        List<Value> values = new ArrayList<>(arguments.size());
        for (Expr argument : arguments) {
            values.add(eval(argument, env));
        }
        return values;
    }

    private Value evalAnd(List<Expr> arguments, Environment env) throws EvalError {
        Value result = TRUE_VALUE;
        for (Expr argument : arguments) {
            result = eval(argument, env);
            if (!isTruthy(result)) {
                return result;
            }
        }
        return result;
    }

    private Value evalOr(List<Expr> arguments, Environment env) throws EvalError {
        Value result = FALSE_VALUE;
        for (Expr argument : arguments) {
            result = eval(argument, env);
            if (isTruthy(result)) {
                return result;
            }
        }
        return result;
    }

    private Value evalDefine(List<Expr> arguments, Environment env) throws EvalError {
        if (arguments.isEmpty()) {
            throw new EvalError("invalid define");
        }

        Expr target = arguments.get(0);
        if (target instanceof SymbolExpr(String name)) {
            if (arguments.size() != 2) {
                throw new EvalError("invalid define");
            }
            env.define(name, eval(arguments.get(1), env));
            return VOID_VALUE;
        }

        if (target instanceof ListExpr(List<Expr> signature)) {
            if (signature.isEmpty() || !(signature.get(0) instanceof SymbolExpr(String name))) {
                throw new EvalError("invalid define");
            }
            if (arguments.size() < 2) {
                throw new EvalError("invalid define");
            }
            List<String> parameters = parseParameters(signature.subList(1, signature.size()));
            env.define(name, new LambdaProcedure(name, parameters, copyExprs(arguments.subList(1, arguments.size())), env));
            return VOID_VALUE;
        }

        throw new EvalError("invalid define");
    }

    private Value evalIf(List<Expr> arguments, Environment env) throws EvalError {
        if (arguments.size() < 2 || arguments.size() > 3) {
            throw new EvalError("wrong argument count for if");
        }

        if (isTruthy(eval(arguments.get(0), env))) {
            return eval(arguments.get(1), env);
        }
        if (arguments.size() == 3) {
            return eval(arguments.get(2), env);
        }
        return VOID_VALUE;
    }

    private Value evalLambda(List<Expr> arguments, Environment env) throws EvalError {
        if (arguments.size() < 2) {
            throw new EvalError("invalid lambda");
        }
        List<String> parameters = parseParameters(arguments.get(0));
        return new LambdaProcedure(null, parameters, copyExprs(arguments.subList(1, arguments.size())), env);
    }

    private Value evalQuote(List<Expr> arguments) throws EvalError {
        expectArgumentCount(arguments, 1, "quote");
        return quote(arguments.get(0));
    }

    private List<Expr> copyExprs(List<Expr> expressions) {
        return new ArrayList<>(expressions);
    }

    private List<String> parseParameters(Expr parametersExpr) throws EvalError {
        if (!(parametersExpr instanceof ListExpr(List<Expr> parameters))) {
            throw new EvalError("invalid parameter list");
        }
        return parseParameters(parameters);
    }

    private List<String> parseParameters(List<Expr> parameters) throws EvalError {
        List<String> names = new ArrayList<>(parameters.size());
        for (Expr parameter : parameters) {
            if (!(parameter instanceof SymbolExpr(String name))) {
                throw new EvalError("invalid parameter list");
            }
            names.add(name);
        }
        return names;
    }

    private Value quote(Expr expression) {
        return switch (expression) {
            case IntExpr(long value) -> new IntValue(value);
            case BoolExpr(boolean value) -> boolValue(value);
            case StringExpr(String value) -> new StringValue(value);
            case SymbolExpr(String name) -> new SymbolValue(name);
            case ListExpr(List<Expr> elements) -> quoteList(elements);
        };
    }

    private Value quoteList(List<Expr> elements) {
        Value value = EMPTY_LIST;
        for (int i = elements.size() - 1; i >= 0; i--) {
            value = new PairValue(quote(elements.get(i)), value);
        }
        return value;
    }

    private Value apply(Value operator, List<Value> arguments) throws EvalError {
        if (operator instanceof BuiltinProcedure builtin) {
            return builtin.apply(arguments);
        }
        if (operator instanceof LambdaProcedure lambda) {
            return applyLambda(lambda, arguments);
        }
        throw new EvalError("attempted to call non-procedure");
    }

    private Value applyLambda(LambdaProcedure lambda, List<Value> arguments) throws EvalError {
        if (arguments.size() != lambda.parameters().size()) {
            throw new EvalError("wrong argument count for " + lambda.displayName());
        }

        Environment callEnv = new Environment(lambda.closure());
        for (int i = 0; i < lambda.parameters().size(); i++) {
            callEnv.define(lambda.parameters().get(i), arguments.get(i));
        }

        Value result = VOID_VALUE;
        for (Expr bodyExpr : lambda.body()) {
            result = eval(bodyExpr, callEnv);
        }
        return result;
    }

    private Value builtinNot(List<Value> arguments) throws EvalError {
        expectArgumentCount(arguments, 1, "not");
        return boolValue(!isTruthy(arguments.get(0)));
    }

    private Value builtinAdd(List<Value> arguments) throws EvalError {
        long total = 0L;
        for (Value argument : arguments) {
            total += requireInt(argument, "+");
        }
        return new IntValue(total);
    }

    private Value builtinSubtract(List<Value> arguments) throws EvalError {
        if (arguments.isEmpty()) {
            throw new EvalError("wrong argument count for -");
        }

        long result = requireInt(arguments.get(0), "-");
        if (arguments.size() == 1) {
            return new IntValue(-result);
        }

        for (int i = 1; i < arguments.size(); i++) {
            result -= requireInt(arguments.get(i), "-");
        }
        return new IntValue(result);
    }

    private Value builtinMultiply(List<Value> arguments) throws EvalError {
        long total = 1L;
        for (Value argument : arguments) {
            total *= requireInt(argument, "*");
        }
        return new IntValue(total);
    }

    private Value builtinDivide(List<Value> arguments) throws EvalError {
        if (arguments.size() < 2) {
            throw new EvalError("wrong argument count for /");
        }

        long result = requireInt(arguments.get(0), "/");
        for (int i = 1; i < arguments.size(); i++) {
            long divisor = requireInt(arguments.get(i), "/");
            if (divisor == 0L) {
                throw new EvalError("division by zero");
            }
            result /= divisor;
        }
        return new IntValue(result);
    }

    private Value builtinComparison(List<Value> arguments, Comparison comparison) throws EvalError {
        if (arguments.size() < 2) {
            throw new EvalError("wrong argument count for " + comparison.name);
        }

        long left = requireInt(arguments.get(0), comparison.name);
        for (int i = 1; i < arguments.size(); i++) {
            long right = requireInt(arguments.get(i), comparison.name);
            if (!comparison.test(left, right)) {
                return FALSE_VALUE;
            }
            left = right;
        }
        return TRUE_VALUE;
    }

    private void expectArgumentCount(List<?> arguments, int expected, String name)
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

    private sealed interface Value permits IntValue, BoolValue, StringValue, SymbolValue,
            PairValue, EmptyListValue, BuiltinProcedure, LambdaProcedure, VoidValue {
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

    private record SymbolValue(String name) implements Value {
        @Override
        public String render() {
            return name;
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

    private record PairValue(Value car, Value cdr) implements Value {
        @Override
        public String render() {
            return renderPair(this);
        }
    }

    private record EmptyListValue() implements Value {
        @Override
        public String render() {
            return "()";
        }
    }

    private record VoidValue() implements Value {
        @Override
        public String render() {
            return "";
        }
    }

    private static final class BuiltinProcedure implements Value {
        private final String name;
        private final BuiltinImplementation implementation;

        private BuiltinProcedure(String name, BuiltinImplementation implementation) {
            this.name = name;
            this.implementation = implementation;
        }

        private Value apply(List<Value> arguments) throws EvalError {
            return implementation.apply(arguments);
        }

        @Override
        public String render() {
            return "#<procedure " + name + ">";
        }
    }

    private record LambdaProcedure(String name, List<String> parameters, List<Expr> body,
                                   Environment closure) implements Value {
        private String displayName() {
            return name == null ? "lambda" : name;
        }

        @Override
        public String render() {
            return "#<procedure " + displayName() + ">";
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

    @FunctionalInterface
    private interface BuiltinImplementation {
        Value apply(List<Value> arguments) throws EvalError;
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

        private Value lookup(String name) throws EvalError {
            if (bindings.containsKey(name)) {
                return bindings.get(name);
            }
            if (parent != null) {
                return parent.lookup(name);
            }
            throw new EvalError("unbound symbol: " + name);
        }
    }

    private static final BoolValue TRUE_VALUE = new BoolValue(true);
    private static final BoolValue FALSE_VALUE = new BoolValue(false);
    private static final EmptyListValue EMPTY_LIST = new EmptyListValue();
    private static final VoidValue VOID_VALUE = new VoidValue();

    private static String renderPair(PairValue pair) {
        StringBuilder builder = new StringBuilder("(");
        Value current = pair;
        boolean first = true;

        while (current instanceof PairValue(Value car, Value cdr)) {
            if (!first) {
                builder.append(' ');
            }
            builder.append(car.render());
            current = cdr;
            first = false;
        }

        if (current instanceof EmptyListValue) {
            builder.append(')');
            return builder.toString();
        }

        builder.append(" . ").append(current.render()).append(')');
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
            if (ch == '\'') {
                return parseQuoted();
            }
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

        private Expr parseQuoted() throws EvalError {
            index++;
            return new ListExpr(List.of(new SymbolExpr("quote"), parseExpr()));
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
