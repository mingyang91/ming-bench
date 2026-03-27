package ming;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

/**
 * Scheme interpreter entry point.
 */
public class Evaluator {
    private final Environment globalEnv = createGlobalEnv();

    public String evalStr(String input) throws EvalError {
        Parser parser = new Parser(input);
        List<Expr> program = parser.parseProgram();
        if (program.isEmpty()) {
            throw new EvalError("empty input");
        }

        Value last = VoidValue.INSTANCE;
        for (Expr expr : program) {
            last = eval(expr, globalEnv);
        }

        return last.toSchemeString();
    }

    public EvalResult evalStrWithOutput(String input) throws EvalError {
        return new EvalResult(evalStr(input), "");
    }

    private Environment createGlobalEnv() {
        Environment env = new Environment(null);
        env.define("+", new BuiltinProcedure("+", this::builtinAdd));
        env.define("-", new BuiltinProcedure("-", this::builtinSub));
        env.define("*", new BuiltinProcedure("*", this::builtinMul));
        env.define("/", new BuiltinProcedure("/", this::builtinDiv));
        env.define("<", new BuiltinProcedure("<", args -> builtinComparison(args, Comparison.LT, "<")));
        env.define(">", new BuiltinProcedure(">", args -> builtinComparison(args, Comparison.GT, ">")));
        env.define("=", new BuiltinProcedure("=", args -> builtinComparison(args, Comparison.EQ, "=")));
        env.define("<=", new BuiltinProcedure("<=", args -> builtinComparison(args, Comparison.LE, "<=")));
        env.define("not", new BuiltinProcedure("not", this::builtinNot));
        env.define("cons", new BuiltinProcedure("cons", this::builtinCons));
        env.define("car", new BuiltinProcedure("car", this::builtinCar));
        env.define("cdr", new BuiltinProcedure("cdr", this::builtinCdr));
        env.define("null?", new BuiltinProcedure("null?", this::builtinNull));
        env.define("list", new BuiltinProcedure("list", this::builtinList));
        env.define("length", new BuiltinProcedure("length", this::builtinLength));
        env.define("append", new BuiltinProcedure("append", this::builtinAppend));
        env.define("string?", new BuiltinProcedure("string?", this::builtinStringPredicate));
        env.define("number?", new BuiltinProcedure("number?", this::builtinNumberPredicate));
        env.define("boolean?", new BuiltinProcedure("boolean?", this::builtinBooleanPredicate));
        env.define("pair?", new BuiltinProcedure("pair?", this::builtinPairPredicate));
        env.define("symbol?", new BuiltinProcedure("symbol?", this::builtinSymbolPredicate));
        return env;
    }

    private Value eval(Expr expr, Environment env) throws EvalError {
        return switch (expr) {
            case IntExpr intExpr -> new IntValue(intExpr.value());
            case BoolExpr boolExpr -> BoolValue.of(boolExpr.value());
            case StringExpr stringExpr -> new StringValue(stringExpr.value());
            case SymbolExpr symbolExpr -> env.lookup(symbolExpr.name());
            case ListExpr listExpr -> evalList(listExpr, env);
        };
    }

    private Value evalList(ListExpr expr, Environment env) throws EvalError {
        List<Expr> elements = expr.elements();
        if (elements.isEmpty()) {
            throw new EvalError("cannot evaluate empty list");
        }

        Expr head = elements.getFirst();
        List<Expr> args = elements.subList(1, elements.size());
        if (head instanceof SymbolExpr symbol) {
            return switch (symbol.name()) {
                case "define" -> evalDefine(args, env);
                case "if" -> evalIf(args, env);
                case "quote" -> evalQuote(args);
                case "lambda" -> evalLambda(args, env);
                case "begin" -> evalBegin(args, env);
                case "let" -> evalLet(args, env);
                case "cond" -> evalCond(args, env);
                case "and" -> evalAnd(args, env);
                case "or" -> evalOr(args, env);
                default -> evalApplication(head, args, env);
            };
        }

        return evalApplication(head, args, env);
    }

    private Value evalApplication(Expr operatorExpr, List<Expr> argExprs, Environment env)
            throws EvalError {
        Value operator = eval(operatorExpr, env);
        List<Value> args = new ArrayList<>(argExprs.size());
        for (Expr argExpr : argExprs) {
            args.add(eval(argExpr, env));
        }
        return apply(operator, args);
    }

    private Value evalDefine(List<Expr> args, Environment env) throws EvalError {
        if (args.size() < 2) {
            throw new EvalError("'define' expects a target and a value");
        }

        Expr target = args.getFirst();
        if (target instanceof SymbolExpr symbol) {
            if (args.size() != 2) {
                throw new EvalError("variable define expects exactly 2 arguments");
            }
            Value value = eval(args.get(1), env);
            env.define(symbol.name(), value);
            return VoidValue.INSTANCE;
        }

        if (target instanceof ListExpr signature) {
            List<Expr> signatureElements = signature.elements();
            if (signatureElements.isEmpty()) {
                throw new EvalError("function define requires a function name");
            }

            Expr nameExpr = signatureElements.getFirst();
            if (!(nameExpr instanceof SymbolExpr functionName)) {
                throw new EvalError("function define requires a symbol name");
            }

            List<String> params = parseParameters(signatureElements.subList(1, signatureElements.size()));
            List<Expr> body = List.copyOf(args.subList(1, args.size()));
            ClosureValue closure = new ClosureValue(functionName.name(), params, body, env);
            env.define(functionName.name(), closure);
            return VoidValue.INSTANCE;
        }

        throw new EvalError("invalid define target");
    }

    private Value evalIf(List<Expr> args, Environment env) throws EvalError {
        requireArgCount(args.size(), 3, "if");
        Value condition = eval(args.get(0), env);
        if (isTruthy(condition)) {
            return eval(args.get(1), env);
        }
        return eval(args.get(2), env);
    }

    private Value evalQuote(List<Expr> args) throws EvalError {
        requireArgCount(args.size(), 1, "quote");
        return quoteToValue(args.getFirst());
    }

    private Value evalLambda(List<Expr> args, Environment env) throws EvalError {
        if (args.size() < 2) {
            throw new EvalError("'lambda' expects a parameter list and a body");
        }

        Expr paramsExpr = args.getFirst();
        if (!(paramsExpr instanceof ListExpr paramsList)) {
            throw new EvalError("'lambda' expects a parameter list");
        }

        List<String> params = parseParameters(paramsList.elements());
        List<Expr> body = List.copyOf(args.subList(1, args.size()));
        return new ClosureValue(null, params, body, env);
    }

    private Value evalBegin(List<Expr> args, Environment env) throws EvalError {
        return evalSequence(args, env);
    }

    private Value evalLet(List<Expr> args, Environment env) throws EvalError {
        if (args.size() < 2) {
            throw new EvalError("'let' expects bindings and a body");
        }

        Expr head = args.getFirst();
        if (head instanceof SymbolExpr name) {
            return evalNamedLet(name.name(), args.subList(1, args.size()), env);
        }

        List<Binding> bindings = parseBindings(head);
        List<Value> values = new ArrayList<>(bindings.size());
        for (Binding binding : bindings) {
            values.add(eval(binding.valueExpr(), env));
        }

        Environment letEnv = new Environment(env);
        for (int i = 0; i < bindings.size(); i++) {
            letEnv.define(bindings.get(i).name(), values.get(i));
        }

        return evalSequence(args.subList(1, args.size()), letEnv);
    }

    private Value evalNamedLet(String name, List<Expr> args, Environment env) throws EvalError {
        if (args.size() < 2) {
            throw new EvalError("'let' expects bindings and a body");
        }

        List<Binding> bindings = parseBindings(args.getFirst());
        List<String> params = new ArrayList<>(bindings.size());
        List<Value> values = new ArrayList<>(bindings.size());
        for (Binding binding : bindings) {
            params.add(binding.name());
            values.add(eval(binding.valueExpr(), env));
        }

        Environment closureEnv = new Environment(env);
        ClosureValue closure = new ClosureValue(
                name,
                List.copyOf(params),
                List.copyOf(args.subList(1, args.size())),
                closureEnv);
        closureEnv.define(name, closure);
        return applyClosure(closure, values);
    }

    private List<Binding> parseBindings(Expr bindingsExpr) throws EvalError {
        if (!(bindingsExpr instanceof ListExpr bindingsList)) {
            throw new EvalError("'let' expects a binding list");
        }

        List<Binding> bindings = new ArrayList<>(bindingsList.elements().size());
        for (Expr bindingExpr : bindingsList.elements()) {
            if (!(bindingExpr instanceof ListExpr bindingList)) {
                throw new EvalError("binding must be a list");
            }

            List<Expr> bindingElements = bindingList.elements();
            if (bindingElements.size() != 2) {
                throw new EvalError("binding must contain a name and a value");
            }
            if (!(bindingElements.getFirst() instanceof SymbolExpr name)) {
                throw new EvalError("binding name must be a symbol");
            }

            bindings.add(new Binding(name.name(), bindingElements.get(1)));
        }
        return List.copyOf(bindings);
    }

    private Value evalCond(List<Expr> args, Environment env) throws EvalError {
        for (int i = 0; i < args.size(); i++) {
            Expr clauseExpr = args.get(i);
            if (!(clauseExpr instanceof ListExpr clause)) {
                throw new EvalError("'cond' clauses must be lists");
            }

            List<Expr> clauseElements = clause.elements();
            if (clauseElements.isEmpty()) {
                throw new EvalError("'cond' clause cannot be empty");
            }

            Expr testExpr = clauseElements.getFirst();
            boolean isElseClause = testExpr instanceof SymbolExpr symbol
                    && symbol.name().equals("else");
            if (isElseClause && i != args.size() - 1) {
                throw new EvalError("'cond' else clause must be last");
            }

            Value testResult = isElseClause ? BoolValue.TRUE : eval(testExpr, env);
            if (isTruthy(testResult)) {
                if (clauseElements.size() == 1) {
                    return testResult;
                }
                return evalSequence(clauseElements.subList(1, clauseElements.size()), env);
            }
        }

        return VoidValue.INSTANCE;
    }

    private Value evalSequence(List<Expr> expressions, Environment env) throws EvalError {
        Value result = VoidValue.INSTANCE;
        for (Expr expression : expressions) {
            result = eval(expression, env);
        }
        return result;
    }

    private List<String> parseParameters(List<Expr> paramExprs) throws EvalError {
        List<String> params = new ArrayList<>(paramExprs.size());
        for (Expr paramExpr : paramExprs) {
            if (!(paramExpr instanceof SymbolExpr symbol)) {
                throw new EvalError("parameter names must be symbols");
            }
            params.add(symbol.name());
        }
        return List.copyOf(params);
    }

    private Value evalAnd(List<Expr> args, Environment env) throws EvalError {
        Value result = BoolValue.TRUE;
        for (Expr arg : args) {
            result = eval(arg, env);
            if (!isTruthy(result)) {
                return result;
            }
        }
        return result;
    }

    private Value evalOr(List<Expr> args, Environment env) throws EvalError {
        for (Expr arg : args) {
            Value result = eval(arg, env);
            if (isTruthy(result)) {
                return result;
            }
        }
        return BoolValue.FALSE;
    }

    private Value quoteToValue(Expr expr) throws EvalError {
        return switch (expr) {
            case IntExpr intExpr -> new IntValue(intExpr.value());
            case BoolExpr boolExpr -> BoolValue.of(boolExpr.value());
            case StringExpr stringExpr -> new StringValue(stringExpr.value());
            case SymbolExpr symbolExpr -> new SymbolValue(symbolExpr.name());
            case ListExpr listExpr -> {
                List<Value> values = new ArrayList<>(listExpr.elements().size());
                for (Expr element : listExpr.elements()) {
                    values.add(quoteToValue(element));
                }
                yield new ListValue(List.copyOf(values));
            }
        };
    }

    private Value apply(Value operator, List<Value> args) throws EvalError {
        if (!(operator instanceof ProcedureValue procedure)) {
            throw new EvalError("attempted to call non-procedure");
        }

        return switch (procedure) {
            case BuiltinProcedure builtin -> builtin.fn().apply(args);
            case ClosureValue closure -> applyClosure(closure, args);
        };
    }

    private Value applyClosure(ClosureValue closure, List<Value> args) throws EvalError {
        if (args.size() != closure.params().size()) {
            throw new EvalError("wrong number of arguments");
        }

        Environment callEnv = new Environment(closure.env());
        for (int i = 0; i < closure.params().size(); i++) {
            callEnv.define(closure.params().get(i), args.get(i));
        }

        Value result = VoidValue.INSTANCE;
        for (Expr bodyExpr : closure.body()) {
            result = eval(bodyExpr, callEnv);
        }
        return result;
    }

    private Value builtinAdd(List<Value> args) throws EvalError {
        long sum = 0;
        for (Value arg : args) {
            sum += requireInt(arg);
        }
        return new IntValue(sum);
    }

    private Value builtinSub(List<Value> args) throws EvalError {
        if (args.isEmpty()) {
            throw new EvalError("'-' expects at least 1 argument");
        }

        long result = requireInt(args.getFirst());
        if (args.size() == 1) {
            return new IntValue(-result);
        }

        for (int i = 1; i < args.size(); i++) {
            result -= requireInt(args.get(i));
        }
        return new IntValue(result);
    }

    private Value builtinMul(List<Value> args) throws EvalError {
        long product = 1;
        for (Value arg : args) {
            product *= requireInt(arg);
        }
        return new IntValue(product);
    }

    private Value builtinDiv(List<Value> args) throws EvalError {
        if (args.size() < 2) {
            throw new EvalError("'/' expects at least 2 arguments");
        }

        long result = requireInt(args.getFirst());
        for (int i = 1; i < args.size(); i++) {
            long divisor = requireInt(args.get(i));
            if (divisor == 0) {
                throw new EvalError("division by zero");
            }
            result /= divisor;
        }
        return new IntValue(result);
    }

    private Value builtinComparison(List<Value> args, Comparison comparison, String name)
            throws EvalError {
        if (args.size() < 2) {
            throw new EvalError("'" + name + "' expects at least 2 arguments");
        }

        long previous = requireInt(args.getFirst());
        for (int i = 1; i < args.size(); i++) {
            long current = requireInt(args.get(i));
            if (!comparison.test(previous, current)) {
                return BoolValue.FALSE;
            }
            previous = current;
        }
        return BoolValue.TRUE;
    }

    private Value builtinNot(List<Value> args) throws EvalError {
        requireArgCount(args.size(), 1, "not");
        return BoolValue.of(!isTruthy(args.getFirst()));
    }

    private Value builtinCons(List<Value> args) throws EvalError {
        requireArgCount(args.size(), 2, "cons");
        List<Value> tail = requireList(args.get(1), "cons");
        List<Value> result = new ArrayList<>(tail.size() + 1);
        result.add(args.getFirst());
        result.addAll(tail);
        return new ListValue(List.copyOf(result));
    }

    private Value builtinCar(List<Value> args) throws EvalError {
        requireArgCount(args.size(), 1, "car");
        return requireNonEmptyList(args.getFirst(), "car").getFirst();
    }

    private Value builtinCdr(List<Value> args) throws EvalError {
        requireArgCount(args.size(), 1, "cdr");
        List<Value> elements = requireNonEmptyList(args.getFirst(), "cdr");
        return new ListValue(List.copyOf(elements.subList(1, elements.size())));
    }

    private Value builtinNull(List<Value> args) throws EvalError {
        requireArgCount(args.size(), 1, "null?");
        return BoolValue.of(args.getFirst() instanceof ListValue listValue
                && listValue.elements().isEmpty());
    }

    private Value builtinList(List<Value> args) {
        return new ListValue(List.copyOf(args));
    }

    private Value builtinLength(List<Value> args) throws EvalError {
        requireArgCount(args.size(), 1, "length");
        return new IntValue(requireList(args.getFirst(), "length").size());
    }

    private Value builtinAppend(List<Value> args) throws EvalError {
        List<Value> result = new ArrayList<>();
        for (Value arg : args) {
            result.addAll(requireList(arg, "append"));
        }
        return new ListValue(List.copyOf(result));
    }

    private Value builtinStringPredicate(List<Value> args) throws EvalError {
        requireArgCount(args.size(), 1, "string?");
        return BoolValue.of(args.getFirst() instanceof StringValue);
    }

    private Value builtinNumberPredicate(List<Value> args) throws EvalError {
        requireArgCount(args.size(), 1, "number?");
        return BoolValue.of(args.getFirst() instanceof IntValue);
    }

    private Value builtinBooleanPredicate(List<Value> args) throws EvalError {
        requireArgCount(args.size(), 1, "boolean?");
        return BoolValue.of(args.getFirst() instanceof BoolValue);
    }

    private Value builtinPairPredicate(List<Value> args) throws EvalError {
        requireArgCount(args.size(), 1, "pair?");
        return BoolValue.of(args.getFirst() instanceof ListValue listValue
                && !listValue.elements().isEmpty());
    }

    private Value builtinSymbolPredicate(List<Value> args) throws EvalError {
        requireArgCount(args.size(), 1, "symbol?");
        return BoolValue.of(args.getFirst() instanceof SymbolValue);
    }

    private void requireArgCount(int actual, int expected, String procedure) throws EvalError {
        if (actual != expected) {
            throw new EvalError("'" + procedure + "' expects exactly "
                    + expected + " argument" + (expected == 1 ? "" : "s"));
        }
    }

    private long requireInt(Value value) throws EvalError {
        if (value instanceof IntValue intValue) {
            return intValue.value();
        }
        throw new EvalError("expected number");
    }

    private List<Value> requireList(Value value, String procedure) throws EvalError {
        if (value instanceof ListValue listValue) {
            return listValue.elements();
        }
        throw new EvalError("'" + procedure + "' expects a list");
    }

    private List<Value> requireNonEmptyList(Value value, String procedure) throws EvalError {
        List<Value> elements = requireList(value, procedure);
        if (!elements.isEmpty()) {
            return elements;
        }
        throw new EvalError("'" + procedure + "' expects a non-empty list");
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

    private record Binding(String name, Expr valueExpr) {
    }

    private sealed interface Value permits IntValue, BoolValue, StringValue, SymbolValue,
            ListValue, ProcedureValue, VoidValue {
        String toSchemeString();
    }

    private sealed interface ProcedureValue extends Value permits BuiltinProcedure, ClosureValue {
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

    private record SymbolValue(String name) implements Value {
        @Override
        public String toSchemeString() {
            return name;
        }
    }

    private record ListValue(List<Value> elements) implements Value {
        @Override
        public String toSchemeString() {
            if (elements.isEmpty()) {
                return "()";
            }

            StringBuilder builder = new StringBuilder();
            builder.append('(');
            for (int i = 0; i < elements.size(); i++) {
                if (i > 0) {
                    builder.append(' ');
                }
                builder.append(elements.get(i).toSchemeString());
            }
            builder.append(')');
            return builder.toString();
        }
    }

    private record BuiltinProcedure(String name, BuiltinFn fn) implements ProcedureValue {
        @Override
        public String toSchemeString() {
            return "#<procedure:" + name + ">";
        }
    }

    private record ClosureValue(String name, List<String> params, List<Expr> body, Environment env)
            implements ProcedureValue {
        @Override
        public String toSchemeString() {
            if (name == null) {
                return "#<procedure>";
            }
            return "#<procedure:" + name + ">";
        }
    }

    private enum VoidValue implements Value {
        INSTANCE;

        @Override
        public String toSchemeString() {
            return "";
        }
    }

    @FunctionalInterface
    private interface BuiltinFn {
        Value apply(List<Value> args) throws EvalError;
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
            throw new EvalError("unbound variable: " + name);
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
                case '\'' -> parseQuoteAbbreviation();
                case '"' -> parseString();
                case ')' -> throw new EvalError("unexpected ')'");
                default -> parseAtom();
            };
        }

        private Expr parseQuoteAbbreviation() throws EvalError {
            index++;
            return new ListExpr(List.of(new SymbolExpr("quote"), parseExpr()));
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
                if (Character.isWhitespace(ch) || ch == '(' || ch == ')' || ch == '\'' || ch == ';') {
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
