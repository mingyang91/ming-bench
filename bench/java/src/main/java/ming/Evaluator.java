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
        installBuiltin(env, "<", (callPos, args) -> builtinComparison(callPos, args, Comparison.LESS_THAN));
        installBuiltin(env, ">", (callPos, args) -> builtinComparison(callPos, args, Comparison.GREATER_THAN));
        installBuiltin(env, "=", (callPos, args) -> builtinComparison(callPos, args, Comparison.EQUAL));
        installBuiltin(env, "<=", (callPos, args) -> builtinComparison(callPos, args, Comparison.LESS_EQUAL));
        installBuiltin(env, "cons", this::builtinCons);
        installBuiltin(env, "car", this::builtinCar);
        installBuiltin(env, "cdr", this::builtinCdr);
        installBuiltin(env, "null?", this::builtinNull);
        installBuiltin(env, "list", this::builtinList);
        installBuiltin(env, "length", this::builtinLength);
        installBuiltin(env, "append", this::builtinAppend);
        installBuiltin(env, "string?", this::builtinStringPredicate);
        installBuiltin(env, "number?", this::builtinNumberPredicate);
        installBuiltin(env, "boolean?", this::builtinBooleanPredicate);
        installBuiltin(env, "pair?", this::builtinPairPredicate);
        installBuiltin(env, "symbol?", this::builtinSymbolPredicate);
        installBuiltin(env, "not", this::builtinNot);
        return env;
    }

    private void installBuiltin(Environment env, String name, BuiltinImplementation implementation) {
        env.define(name, new BuiltinProcedure(name, implementation));
    }

    private Value eval(Expr expression, Environment env) throws EvalError {
        try {
            if (expression instanceof IntExpr intExpr) {
                return new IntValue(intExpr.value());
            }
            if (expression instanceof BoolExpr boolExpr) {
                return boolValue(boolExpr.value());
            }
            if (expression instanceof StringExpr stringExpr) {
                return new StringValue(stringExpr.value());
            }
            if (expression instanceof SymbolExpr symbolExpr) {
                return env.lookup(symbolExpr.name());
            }
            if (expression instanceof ListExpr listExpr) {
                return evalList(listExpr, env);
            }
            throw new IllegalStateException("unknown expression type");
        } catch (EvalError error) {
            throw withPosition(error, expression.pos());
        }
    }

    private Value evalList(ListExpr listExpr, Environment env) throws EvalError {
        List<Expr> elements = listExpr.elements();
        if (elements.isEmpty()) {
            throw new EvalError("cannot evaluate empty list");
        }

        Expr operatorExpr = elements.get(0);
        List<Expr> arguments = elements.subList(1, elements.size());
        if (operatorExpr instanceof SymbolExpr symbolExpr) {
            String name = symbolExpr.name();
            return switch (name) {
                case "and" -> evalAnd(arguments, env);
                case "begin" -> evalBegin(arguments, env);
                case "cond" -> evalCond(arguments, env);
                case "or" -> evalOr(arguments, env);
                case "define" -> evalDefine(arguments, env);
                case "if" -> evalIf(arguments, env);
                case "let" -> evalLet(arguments, env);
                case "lambda" -> evalLambda(arguments, env);
                case "quote" -> evalQuote(arguments);
                default -> apply(
                        eval(operatorExpr, env),
                        operatorExpr.pos(),
                        evalArguments(arguments, env),
                        listExpr.pos());
            };
        }
        return apply(
                eval(operatorExpr, env),
                operatorExpr.pos(),
                evalArguments(arguments, env),
                listExpr.pos());
    }

    private List<LocatedValue> evalArguments(List<Expr> arguments, Environment env) throws EvalError {
        List<LocatedValue> values = new ArrayList<>(arguments.size());
        for (Expr argument : arguments) {
            values.add(new LocatedValue(eval(argument, env), argument.pos()));
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

    private Value evalBegin(List<Expr> arguments, Environment env) throws EvalError {
        return evalSequence(arguments, env);
    }

    private Value evalCond(List<Expr> arguments, Environment env) throws EvalError {
        for (int i = 0; i < arguments.size(); i++) {
            Expr clauseExpr = arguments.get(i);
            if (!(clauseExpr instanceof ListExpr clauseExprList) || clauseExprList.elements().isEmpty()) {
                throw new EvalError("invalid cond");
            }
            List<Expr> clause = clauseExprList.elements();

            Expr testExpr = clause.get(0);
            boolean isElseClause = testExpr instanceof SymbolExpr symbolExpr
                    && symbolExpr.name().equals("else");
            if (isElseClause) {
                if (i != arguments.size() - 1) {
                    throw new EvalError("invalid cond");
                }
                return clause.size() == 1
                        ? TRUE_VALUE
                        : evalSequence(clause.subList(1, clause.size()), env);
            }

            Value testValue = eval(testExpr, env);
            if (isTruthy(testValue)) {
                return clause.size() == 1
                        ? testValue
                        : evalSequence(clause.subList(1, clause.size()), env);
            }
        }
        return VOID_VALUE;
    }

    private Value evalDefine(List<Expr> arguments, Environment env) throws EvalError {
        if (arguments.isEmpty()) {
            throw new EvalError("invalid define");
        }

        Expr target = arguments.get(0);
        if (target instanceof SymbolExpr symbolExpr) {
            String name = symbolExpr.name();
            if (arguments.size() != 2) {
                throw new EvalError("invalid define");
            }
            env.define(name, eval(arguments.get(1), env));
            return VOID_VALUE;
        }

        if (target instanceof ListExpr signatureExpr) {
            List<Expr> signature = signatureExpr.elements();
            if (signature.isEmpty() || !(signature.get(0) instanceof SymbolExpr nameExpr)) {
                throw new EvalError("invalid define");
            }
            if (arguments.size() < 2) {
                throw new EvalError("invalid define");
            }
            List<String> parameters = parseParameters(signature.subList(1, signature.size()));
            String name = nameExpr.name();
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

    private Value evalLet(List<Expr> arguments, Environment env) throws EvalError {
        if (arguments.size() < 2) {
            throw new EvalError("invalid let");
        }

        if (arguments.get(0) instanceof SymbolExpr nameExpr) {
            String name = nameExpr.name();
            if (arguments.size() < 3) {
                throw new EvalError("invalid let");
            }
            List<Binding> bindings = parseBindings(arguments.get(1));
            List<Value> values = evalBindingValues(bindings, env);
            List<String> parameters = bindingNames(bindings);

            Environment loopEnv = new Environment(env);
            LambdaProcedure procedure = new LambdaProcedure(
                    name,
                    parameters,
                    copyExprs(arguments.subList(2, arguments.size())),
                    loopEnv);
            loopEnv.define(name, procedure);
            List<LocatedValue> locatedValues = new ArrayList<>(values.size());
            for (int i = 0; i < values.size(); i++) {
                locatedValues.add(new LocatedValue(values.get(i), bindings.get(i).valueExpr().pos()));
            }
            return applyLambda(procedure, locatedValues, arguments.get(1).pos());
        }

        List<Binding> bindings = parseBindings(arguments.get(0));
        List<Value> values = evalBindingValues(bindings, env);
        Environment letEnv = new Environment(env);
        for (int i = 0; i < bindings.size(); i++) {
            letEnv.define(bindings.get(i).name(), values.get(i));
        }
        return evalSequence(arguments.subList(1, arguments.size()), letEnv);
    }

    private Value evalQuote(List<Expr> arguments) throws EvalError {
        if (arguments.size() != 1) {
            throw new EvalError("wrong argument count for quote");
        }
        return quote(arguments.get(0));
    }

    private Value evalSequence(List<Expr> expressions, Environment env) throws EvalError {
        Value result = VOID_VALUE;
        for (Expr expression : expressions) {
            result = eval(expression, env);
        }
        return result;
    }

    private List<Expr> copyExprs(List<Expr> expressions) {
        return new ArrayList<>(expressions);
    }

    private List<Binding> parseBindings(Expr bindingsExpr) throws EvalError {
        if (!(bindingsExpr instanceof ListExpr bindingsList)) {
            throw new EvalError("invalid let");
        }
        List<Expr> bindings = bindingsList.elements();

        List<Binding> parsed = new ArrayList<>(bindings.size());
        for (Expr bindingExpr : bindings) {
            if (!(bindingExpr instanceof ListExpr bindingList)) {
                throw new EvalError("invalid let");
            }
            List<Expr> binding = bindingList.elements();
            if (binding.size() != 2 || !(binding.get(0) instanceof SymbolExpr nameExpr)) {
                throw new EvalError("invalid let");
            }
            parsed.add(new Binding(nameExpr.name(), binding.get(1)));
        }
        return parsed;
    }

    private List<Value> evalBindingValues(List<Binding> bindings, Environment env) throws EvalError {
        List<Value> values = new ArrayList<>(bindings.size());
        for (Binding binding : bindings) {
            values.add(eval(binding.valueExpr(), env));
        }
        return values;
    }

    private List<String> bindingNames(List<Binding> bindings) {
        List<String> names = new ArrayList<>(bindings.size());
        for (Binding binding : bindings) {
            names.add(binding.name());
        }
        return names;
    }

    private List<String> parseParameters(Expr parametersExpr) throws EvalError {
        if (!(parametersExpr instanceof ListExpr parametersList)) {
            throw new EvalError("invalid parameter list");
        }
        List<Expr> parameters = parametersList.elements();
        return parseParameters(parameters);
    }

    private List<String> parseParameters(List<Expr> parameters) throws EvalError {
        List<String> names = new ArrayList<>(parameters.size());
        for (Expr parameter : parameters) {
            if (!(parameter instanceof SymbolExpr symbolExpr)) {
                throw new EvalError("invalid parameter list");
            }
            names.add(symbolExpr.name());
        }
        return names;
    }

    private Value quote(Expr expression) {
        if (expression instanceof IntExpr intExpr) {
            return new IntValue(intExpr.value());
        }
        if (expression instanceof BoolExpr boolExpr) {
            return boolValue(boolExpr.value());
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
        throw new IllegalStateException("unknown quoted expression type");
    }

    private Value quoteList(List<Expr> elements) {
        Value value = EMPTY_LIST;
        for (int i = elements.size() - 1; i >= 0; i--) {
            value = new PairValue(quote(elements.get(i)), value);
        }
        return value;
    }

    private Value apply(Value operator, SourcePos operatorPos, List<LocatedValue> arguments,
                        SourcePos callPos) throws EvalError {
        if (operator instanceof BuiltinProcedure builtin) {
            return builtin.apply(callPos, arguments);
        }
        if (operator instanceof LambdaProcedure lambda) {
            return applyLambda(lambda, arguments, callPos);
        }
        throw errorAt(operatorPos, "attempted to call non-procedure");
    }

    private Value applyLambda(LambdaProcedure lambda, List<LocatedValue> arguments, SourcePos callPos)
            throws EvalError {
        if (arguments.size() != lambda.parameters().size()) {
            throw errorAt(callPos, "wrong argument count for " + lambda.displayName());
        }

        Environment callEnv = new Environment(lambda.closure());
        for (int i = 0; i < lambda.parameters().size(); i++) {
            callEnv.define(lambda.parameters().get(i), arguments.get(i).value());
        }

        Value result = VOID_VALUE;
        for (Expr bodyExpr : lambda.body()) {
            result = eval(bodyExpr, callEnv);
        }
        return result;
    }

    private Value builtinNot(SourcePos callPos, List<LocatedValue> arguments) throws EvalError {
        expectArgumentCount(arguments, 1, "not", callPos);
        return boolValue(!isTruthy(arguments.get(0).value()));
    }

    private Value builtinCons(SourcePos callPos, List<LocatedValue> arguments) throws EvalError {
        expectArgumentCount(arguments, 2, "cons", callPos);
        return new PairValue(arguments.get(0).value(), arguments.get(1).value());
    }

    private Value builtinCar(SourcePos callPos, List<LocatedValue> arguments) throws EvalError {
        expectArgumentCount(arguments, 1, "car", callPos);
        return requirePair(arguments.get(0), "car").car();
    }

    private Value builtinCdr(SourcePos callPos, List<LocatedValue> arguments) throws EvalError {
        expectArgumentCount(arguments, 1, "cdr", callPos);
        return requirePair(arguments.get(0), "cdr").cdr();
    }

    private Value builtinNull(SourcePos callPos, List<LocatedValue> arguments) throws EvalError {
        expectArgumentCount(arguments, 1, "null?", callPos);
        return boolValue(arguments.get(0).value() instanceof EmptyListValue);
    }

    private Value builtinList(SourcePos callPos, List<LocatedValue> arguments) {
        Value result = EMPTY_LIST;
        for (int i = arguments.size() - 1; i >= 0; i--) {
            result = new PairValue(arguments.get(i).value(), result);
        }
        return result;
    }

    private Value builtinLength(SourcePos callPos, List<LocatedValue> arguments) throws EvalError {
        expectArgumentCount(arguments, 1, "length", callPos);
        return new IntValue(requireProperListLength(arguments.get(0), "length"));
    }

    private Value builtinAppend(SourcePos callPos, List<LocatedValue> arguments) throws EvalError {
        if (arguments.isEmpty()) {
            return EMPTY_LIST;
        }

        Value result = arguments.get(arguments.size() - 1).value();
        for (int i = arguments.size() - 2; i >= 0; i--) {
            result = appendListOnto(arguments.get(i), result);
        }
        return result;
    }

    private Value builtinStringPredicate(SourcePos callPos, List<LocatedValue> arguments)
            throws EvalError {
        expectArgumentCount(arguments, 1, "string?", callPos);
        return boolValue(arguments.get(0).value() instanceof StringValue);
    }

    private Value builtinNumberPredicate(SourcePos callPos, List<LocatedValue> arguments)
            throws EvalError {
        expectArgumentCount(arguments, 1, "number?", callPos);
        return boolValue(arguments.get(0).value() instanceof IntValue);
    }

    private Value builtinBooleanPredicate(SourcePos callPos, List<LocatedValue> arguments)
            throws EvalError {
        expectArgumentCount(arguments, 1, "boolean?", callPos);
        return boolValue(arguments.get(0).value() instanceof BoolValue);
    }

    private Value builtinPairPredicate(SourcePos callPos, List<LocatedValue> arguments)
            throws EvalError {
        expectArgumentCount(arguments, 1, "pair?", callPos);
        return boolValue(arguments.get(0).value() instanceof PairValue);
    }

    private Value builtinSymbolPredicate(SourcePos callPos, List<LocatedValue> arguments)
            throws EvalError {
        expectArgumentCount(arguments, 1, "symbol?", callPos);
        return boolValue(arguments.get(0).value() instanceof SymbolValue);
    }

    private Value builtinAdd(SourcePos callPos, List<LocatedValue> arguments) throws EvalError {
        long total = 0L;
        for (LocatedValue argument : arguments) {
            total += requireInt(argument, "+");
        }
        return new IntValue(total);
    }

    private Value builtinSubtract(SourcePos callPos, List<LocatedValue> arguments) throws EvalError {
        if (arguments.isEmpty()) {
            throw errorAt(callPos, "wrong argument count for -");
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

    private Value builtinMultiply(SourcePos callPos, List<LocatedValue> arguments) throws EvalError {
        long total = 1L;
        for (LocatedValue argument : arguments) {
            total *= requireInt(argument, "*");
        }
        return new IntValue(total);
    }

    private Value builtinDivide(SourcePos callPos, List<LocatedValue> arguments) throws EvalError {
        if (arguments.size() < 2) {
            throw errorAt(callPos, "wrong argument count for /");
        }

        long result = requireInt(arguments.get(0), "/");
        for (int i = 1; i < arguments.size(); i++) {
            long divisor = requireInt(arguments.get(i), "/");
            if (divisor == 0L) {
                throw errorAt(arguments.get(i).pos(), "division by zero");
            }
            result /= divisor;
        }
        return new IntValue(result);
    }

    private Value builtinComparison(SourcePos callPos, List<LocatedValue> arguments,
                                    Comparison comparison) throws EvalError {
        if (arguments.size() < 2) {
            throw errorAt(callPos, "wrong argument count for " + comparison.name);
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

    private void expectArgumentCount(List<?> arguments, int expected, String name, SourcePos pos)
            throws EvalError {
        if (arguments.size() != expected) {
            throw errorAt(pos, "wrong argument count for " + name);
        }
    }

    private long requireInt(LocatedValue value, String name) throws EvalError {
        if (value.value() instanceof IntValue(long number)) {
            return number;
        }
        throw errorAt(value.pos(), "expected number for " + name);
    }

    private PairValue requirePair(LocatedValue value, String name) throws EvalError {
        if (value.value() instanceof PairValue pair) {
            return pair;
        }
        throw errorAt(value.pos(), "expected pair for " + name);
    }

    private long requireProperListLength(LocatedValue value, String name) throws EvalError {
        long length = 0L;
        Value current = value.value();
        while (current instanceof PairValue(Value ignoredCar, Value cdr)) {
            length++;
            current = cdr;
        }
        if (current instanceof EmptyListValue) {
            return length;
        }
        throw errorAt(value.pos(), "expected list for " + name);
    }

    private Value appendListOnto(LocatedValue list, Value tail) throws EvalError {
        List<Value> elements = new ArrayList<>();
        Value current = list.value();
        while (current instanceof PairValue(Value car, Value cdr)) {
            elements.add(car);
            current = cdr;
        }
        if (!(current instanceof EmptyListValue)) {
            throw errorAt(list.pos(), "expected list for append");
        }

        Value result = tail;
        for (int i = elements.size() - 1; i >= 0; i--) {
            result = new PairValue(elements.get(i), result);
        }
        return result;
    }

    private boolean isTruthy(Value value) {
        return !(value instanceof BoolValue(boolean bool) && !bool);
    }

    private BoolValue boolValue(boolean value) {
        return value ? TRUE_VALUE : FALSE_VALUE;
    }

    private EvalError errorAt(SourcePos pos, String message) {
        return new EvalError(message, pos.line(), pos.column());
    }

    private EvalError withPosition(EvalError error, SourcePos pos) {
        return error.hasPosition() ? error : error.withPosition(pos.line(), pos.column());
    }

    private sealed interface Expr permits IntExpr, BoolExpr, StringExpr, SymbolExpr, ListExpr {
        SourcePos pos();
    }

    private record SourcePos(int line, int column) {}

    private record IntExpr(long value, SourcePos pos) implements Expr {}

    private record BoolExpr(boolean value, SourcePos pos) implements Expr {}

    private record StringExpr(String value, SourcePos pos) implements Expr {}

    private record SymbolExpr(String name, SourcePos pos) implements Expr {}

    private record ListExpr(List<Expr> elements, SourcePos pos) implements Expr {}

    private record LocatedValue(Value value, SourcePos pos) {}

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

        private Value apply(SourcePos callPos, List<LocatedValue> arguments) throws EvalError {
            return implementation.apply(callPos, arguments);
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
        Value apply(SourcePos callPos, List<LocatedValue> arguments) throws EvalError;
    }

    private record Binding(String name, Expr valueExpr) {}

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
        private final List<Integer> lineStarts;
        private int index;

        private Parser(String input) {
            this.input = input;
            this.lineStarts = computeLineStarts(input);
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
                throw errorAtCurrent("unexpected end of input");
            }

            SourcePos pos = positionAt(index);
            char ch = input.charAt(index);
            if (ch == '\'') {
                return parseQuoted(pos);
            }
            if (ch == '(') {
                return parseList(pos);
            }
            if (ch == ')') {
                throw errorAtCurrent("unexpected )");
            }
            if (ch == '"') {
                return parseString(pos);
            }
            if (ch == '#') {
                return parseBoolean(pos);
            }
            return parseAtom(pos);
        }

        private Expr parseQuoted(SourcePos pos) throws EvalError {
            index++;
            return new ListExpr(List.of(new SymbolExpr("quote", pos), parseExpr()), pos);
        }

        private Expr parseList(SourcePos pos) throws EvalError {
            index++;
            List<Expr> elements = new ArrayList<>();
            skipWhitespace();

            while (true) {
                if (isAtEnd()) {
                    throw errorAt(pos, "unterminated list");
                }
                if (input.charAt(index) == ')') {
                    index++;
                    return new ListExpr(elements, pos);
                }
                elements.add(parseExpr());
                skipWhitespace();
            }
        }

        private Expr parseString(SourcePos pos) throws EvalError {
            index++;
            StringBuilder value = new StringBuilder();
            while (!isAtEnd()) {
                char ch = input.charAt(index++);
                if (ch == '"') {
                    return new StringExpr(value.toString(), pos);
                }
                if (ch == '\\') {
                    if (isAtEnd()) {
                        throw errorAt(pos, "unterminated string");
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
            throw errorAt(pos, "unterminated string");
        }

        private Expr parseBoolean(SourcePos pos) throws EvalError {
            if (matchesToken("#t")) {
                index += 2;
                return new BoolExpr(true, pos);
            }
            if (matchesToken("#f")) {
                index += 2;
                return new BoolExpr(false, pos);
            }
            throw errorAt(pos, "invalid boolean literal");
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

        private Expr parseAtom(SourcePos pos) throws EvalError {
            int start = index;
            while (!isAtEnd() && !isDelimiter(input.charAt(index))) {
                index++;
            }

            String token = input.substring(start, index);
            if (token.isEmpty()) {
                throw errorAt(pos, "unexpected token");
            }

            if (isIntegerToken(token)) {
                try {
                    return new IntExpr(Long.parseLong(token), pos);
                } catch (NumberFormatException e) {
                    throw errorAt(pos, "invalid integer literal: " + token);
                }
            }
            return new SymbolExpr(token, pos);
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
            while (!isAtEnd()) {
                char ch = input.charAt(index);
                if (Character.isWhitespace(ch)) {
                    index++;
                    continue;
                }
                if (ch == ';') {
                    while (!isAtEnd() && input.charAt(index) != '\n') {
                        index++;
                    }
                    continue;
                }
                return;
            }
        }

        private boolean isAtEnd() {
            return index >= input.length();
        }

        private boolean isDelimiter(char ch) {
            return Character.isWhitespace(ch) || ch == '(' || ch == ')' || ch == ';';
        }

        private EvalError errorAtCurrent(String message) {
            return errorAt(positionAt(index), message);
        }

        private EvalError errorAt(SourcePos pos, String message) {
            return new EvalError(message, pos.line(), pos.column());
        }

        private SourcePos positionAt(int absoluteIndex) {
            int cappedIndex = Math.max(0, Math.min(absoluteIndex, input.length()));
            int low = 0;
            int high = lineStarts.size() - 1;
            int lineIndex = 0;

            while (low <= high) {
                int mid = (low + high) >>> 1;
                int lineStart = lineStarts.get(mid);
                if (lineStart <= cappedIndex) {
                    lineIndex = mid;
                    low = mid + 1;
                } else {
                    high = mid - 1;
                }
            }

            int lineStart = lineStarts.get(lineIndex);
            return new SourcePos(lineIndex + 1, cappedIndex - lineStart + 1);
        }

        private List<Integer> computeLineStarts(String source) {
            List<Integer> starts = new ArrayList<>();
            starts.add(0);
            for (int i = 0; i < source.length(); i++) {
                if (source.charAt(i) == '\n') {
                    starts.add(i + 1);
                }
            }
            return starts;
        }
    }
}
