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
    private static final BoolValue TRUE = new BoolValue(true);
    private static final BoolValue FALSE = new BoolValue(false);
    private static final EmptyListValue EMPTY_LIST = new EmptyListValue();
    private static final VoidValue VOID = new VoidValue();
    private static final UninitializedValue UNINITIALIZED = new UninitializedValue();

    private final Env globalEnv;

    public Evaluator() {
        this.globalEnv = createGlobalEnv();
    }

    /**
     * Evaluate one or more Scheme expressions and return the string
     * representation of the last result.
     */
    public String evalStr(String input) throws EvalError {
        List<Expr> expressions = new Parser(input).parseProgram();
        if (expressions.isEmpty()) {
            throw new EvalError("input did not contain any expressions");
        }

        Value result = VOID;
        for (Expr expression : expressions) {
            result = eval(expression, globalEnv);
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

    private Env createGlobalEnv() {
        Env env = new Env(null);
        env.define("+", new BuiltinValue("+", this::add));
        env.define("-", new BuiltinValue("-", this::subtract));
        env.define("*", new BuiltinValue("*", this::multiply));
        env.define("/", new BuiltinValue("/", this::divide));
        env.define("<", new BuiltinValue("<", arguments -> compare(arguments, "<")));
        env.define(">", new BuiltinValue(">", arguments -> compare(arguments, ">")));
        env.define("=", new BuiltinValue("=", arguments -> compare(arguments, "=")));
        env.define("<=", new BuiltinValue("<=", arguments -> compare(arguments, "<=")));
        env.define("not", new BuiltinValue("not", this::not));
        return env;
    }

    private Value eval(Expr expr, Env env) throws EvalError {
        return switch (expr) {
            case IntExpr intExpr -> new IntValue(intExpr.value());
            case BoolExpr boolExpr -> boolValue(boolExpr.value());
            case StringExpr stringExpr -> new StringValue(stringExpr.value());
            case SymbolExpr symbolExpr -> env.lookup(symbolExpr.name());
            case ListExpr listExpr -> evalList(listExpr, env);
        };
    }

    private Value evalList(ListExpr listExpr, Env env) throws EvalError {
        List<Expr> elements = listExpr.elements();
        if (elements.isEmpty()) {
            throw new EvalError("cannot evaluate an empty list");
        }

        Expr operatorExpr = elements.get(0);
        List<Expr> arguments = elements.subList(1, elements.size());
        if (operatorExpr instanceof SymbolExpr symbolExpr) {
            return switch (symbolExpr.name()) {
                case "define" -> evalDefine(arguments, env);
                case "if" -> evalIf(arguments, env);
                case "quote" -> evalQuote(arguments);
                case "lambda" -> evalLambda(arguments, env);
                case "and" -> evalAnd(arguments, env);
                case "or" -> evalOr(arguments, env);
                default -> applyProcedure(eval(operatorExpr, env), evalAll(arguments, env));
            };
        }

        return applyProcedure(eval(operatorExpr, env), evalAll(arguments, env));
    }

    private Value evalDefine(List<Expr> arguments, Env env) throws EvalError {
        if (arguments.size() < 2) {
            throw new EvalError("define expects a name and a value");
        }

        Expr target = arguments.get(0);
        if (target instanceof SymbolExpr symbolExpr) {
            if (arguments.size() != 2) {
                throw new EvalError("define expects exactly one value expression");
            }
            Cell binding = env.definePlaceholder(symbolExpr.name());
            binding.value = eval(arguments.get(1), env);
            return VOID;
        }

        if (target instanceof ListExpr signatureExpr) {
            List<Expr> signature = signatureExpr.elements();
            if (signature.isEmpty() || !(signature.get(0) instanceof SymbolExpr nameExpr)) {
                throw new EvalError("define function name must be a symbol");
            }

            if (arguments.size() < 2) {
                throw new EvalError("define requires a function body");
            }

            Cell binding = env.definePlaceholder(nameExpr.name());
            binding.value = new ClosureValue(
                    parseParameterNames(signature.subList(1, signature.size())),
                    List.copyOf(arguments.subList(1, arguments.size())),
                    env);
            return VOID;
        }

        throw new EvalError("define target must be a symbol or parameter list");
    }

    private Value evalIf(List<Expr> arguments, Env env) throws EvalError {
        requireExactArgs("if", arguments, 3);
        Value condition = eval(arguments.get(0), env);
        if (isTruthy(condition)) {
            return eval(arguments.get(1), env);
        }
        return eval(arguments.get(2), env);
    }

    private Value evalQuote(List<Expr> arguments) throws EvalError {
        requireExactArgs("quote", arguments, 1);
        return quoteToValue(arguments.get(0));
    }

    private Value evalLambda(List<Expr> arguments, Env env) throws EvalError {
        if (arguments.size() < 2) {
            throw new EvalError("lambda requires parameters and a body");
        }

        return new ClosureValue(
                parseParameterNames(arguments.get(0)),
                List.copyOf(arguments.subList(1, arguments.size())),
                env);
    }

    private List<String> parseParameterNames(Expr parameterExpr) throws EvalError {
        if (!(parameterExpr instanceof ListExpr parameterList)) {
            throw new EvalError("lambda parameters must be a list");
        }
        return parseParameterNames(parameterList.elements());
    }

    private List<String> parseParameterNames(List<Expr> parameterExprs) throws EvalError {
        List<String> parameters = new ArrayList<>(parameterExprs.size());
        for (Expr parameterExpr : parameterExprs) {
            if (!(parameterExpr instanceof SymbolExpr symbolExpr)) {
                throw new EvalError("lambda parameter must be a symbol");
            }
            parameters.add(symbolExpr.name());
        }
        return List.copyOf(parameters);
    }

    private List<Value> evalAll(List<Expr> arguments, Env env) throws EvalError {
        List<Value> values = new ArrayList<>(arguments.size());
        for (Expr argument : arguments) {
            values.add(eval(argument, env));
        }
        return values;
    }

    private Value evalAnd(List<Expr> arguments, Env env) throws EvalError {
        Value last = TRUE;
        for (Expr argument : arguments) {
            last = eval(argument, env);
            if (!isTruthy(last)) {
                return last;
            }
        }
        return last;
    }

    private Value evalOr(List<Expr> arguments, Env env) throws EvalError {
        Value last = FALSE;
        for (Expr argument : arguments) {
            last = eval(argument, env);
            if (isTruthy(last)) {
                return last;
            }
        }
        return last;
    }

    private Value applyProcedure(Value operator, List<Value> arguments) throws EvalError {
        return switch (operator) {
            case BuiltinValue builtinValue -> builtinValue.implementation().apply(arguments);
            case ClosureValue closureValue -> applyClosure(closureValue, arguments);
            default -> throw new EvalError("attempted to call a non-procedure");
        };
    }

    private Value applyClosure(ClosureValue closure, List<Value> arguments) throws EvalError {
        requireExactArgs("lambda", arguments, closure.parameters().size());

        Env callEnv = new Env(closure.env());
        for (int i = 0; i < closure.parameters().size(); i++) {
            callEnv.define(closure.parameters().get(i), arguments.get(i));
        }
        return evalSequence(closure.body(), callEnv);
    }

    private Value evalSequence(List<Expr> expressions, Env env) throws EvalError {
        Value result = VOID;
        for (Expr expression : expressions) {
            result = eval(expression, env);
        }
        return result;
    }

    private Value quoteToValue(Expr expr) {
        return switch (expr) {
            case IntExpr intExpr -> new IntValue(intExpr.value());
            case BoolExpr boolExpr -> boolValue(boolExpr.value());
            case StringExpr stringExpr -> new StringValue(stringExpr.value());
            case SymbolExpr symbolExpr -> new SymbolValue(symbolExpr.name());
            case ListExpr listExpr -> listValue(quoteElements(listExpr.elements()));
        };
    }

    private List<Value> quoteElements(List<Expr> expressions) {
        List<Value> values = new ArrayList<>(expressions.size());
        for (Expr expression : expressions) {
            values.add(quoteToValue(expression));
        }
        return values;
    }

    private Value listValue(List<Value> values) {
        Value result = EMPTY_LIST;
        for (int index = values.size() - 1; index >= 0; index--) {
            result = new PairValue(values.get(index), result);
        }
        return result;
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

    private void requireExactArgs(String name, List<?> arguments, int exact)
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
            case SymbolValue symbolValue -> symbolValue.name();
            case EmptyListValue ignored -> "()";
            case PairValue pairValue -> renderPair(pairValue);
            case BuiltinValue ignored -> "#<procedure>";
            case ClosureValue ignored -> "#<procedure>";
            case VoidValue ignored -> "#<void>";
            case UninitializedValue ignored -> "#<uninitialized>";
        };
    }

    private String renderPair(PairValue pairValue) {
        StringBuilder builder = new StringBuilder();
        builder.append('(');

        Value current = pairValue;
        boolean first = true;
        while (current instanceof PairValue pair) {
            if (!first) {
                builder.append(' ');
            }
            builder.append(render(pair.car()));
            current = pair.cdr();
            first = false;
        }

        if (!(current instanceof EmptyListValue)) {
            builder.append(" . ");
            builder.append(render(current));
        }

        builder.append(')');
        return builder.toString();
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

    private sealed interface Value permits IntValue, BoolValue, StringValue, SymbolValue,
            EmptyListValue, PairValue, BuiltinValue, ClosureValue, VoidValue,
            UninitializedValue {
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

    private record SymbolValue(String name) implements Value {
    }

    private record EmptyListValue() implements Value {
    }

    private record PairValue(Value car, Value cdr) implements Value {
    }

    private record BuiltinValue(String name, BuiltinFunction implementation) implements Value {
    }

    private record ClosureValue(List<String> parameters, List<Expr> body, Env env)
            implements Value {
    }

    private record VoidValue() implements Value {
    }

    private record UninitializedValue() implements Value {
    }

    @FunctionalInterface
    private interface BuiltinFunction {
        Value apply(List<Value> arguments) throws EvalError;
    }

    private static final class Cell {
        private Value value;

        private Cell(Value value) {
            this.value = value;
        }
    }

    private static final class Env {
        private final Env parent;
        private final Map<String, Cell> bindings = new HashMap<>();

        private Env(Env parent) {
            this.parent = parent;
        }

        private void define(String name, Value value) {
            bindings.put(name, new Cell(value));
        }

        private Cell definePlaceholder(String name) {
            Cell cell = new Cell(UNINITIALIZED);
            bindings.put(name, cell);
            return cell;
        }

        private Value lookup(String name) throws EvalError {
            Cell cell = lookupCell(name);
            if (cell == null || cell.value == UNINITIALIZED) {
                throw new EvalError("unbound variable: " + name);
            }
            return cell.value;
        }

        private Cell lookupCell(String name) {
            if (bindings.containsKey(name)) {
                return bindings.get(name);
            }
            if (parent != null) {
                return parent.lookupCell(name);
            }
            return null;
        }
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
            if (current == '\'') {
                advance();
                return new ListExpr(List.of(new SymbolExpr("quote"), parseExpression()));
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
            return Character.isWhitespace(ch) || ch == '(' || ch == ')' || ch == ';'
                    || ch == '\'';
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
