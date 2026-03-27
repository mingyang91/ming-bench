package ming;

import java.math.BigInteger;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

final class Interpreter {
    private static final BooleanValue TRUE = new BooleanValue(true);
    private static final BooleanValue FALSE = new BooleanValue(false);
    private static final EmptyListValue EMPTY_LIST = EmptyListValue.INSTANCE;
    private static final VoidValue VOID = VoidValue.INSTANCE;

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
        env.define("cons", new BuiltinProcedure("cons", this::applyCons));
        env.define("car", new BuiltinProcedure("car", this::applyCar));
        env.define("cdr", new BuiltinProcedure("cdr", this::applyCdr));
        env.define("null?", new BuiltinProcedure("null?", this::applyNullPredicate));
        env.define("list", new BuiltinProcedure("list", this::applyList));
        env.define("length", new BuiltinProcedure("length", this::applyLength));
        env.define("append", new BuiltinProcedure("append", this::applyAppend));
        env.define("string?", new BuiltinProcedure("string?", this::applyStringPredicate));
        env.define("number?", new BuiltinProcedure("number?", this::applyNumberPredicate));
        env.define("boolean?", new BuiltinProcedure("boolean?", this::applyBooleanPredicate));
        env.define("pair?", new BuiltinProcedure("pair?", this::applyPairPredicate));
        env.define("symbol?", new BuiltinProcedure("symbol?", this::applySymbolPredicate));
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
            if ("define".equals(symbolName)) {
                return evalDefine(listExpr, env);
            }
            if ("if".equals(symbolName)) {
                return evalIf(listExpr, env);
            }
            if ("quote".equals(symbolName)) {
                return evalQuote(listExpr);
            }
            if ("lambda".equals(symbolName)) {
                return evalLambda(listExpr, env);
            }
            if ("begin".equals(symbolName)) {
                return evalBegin(listExpr, env);
            }
            if ("let".equals(symbolName)) {
                return evalLet(listExpr, env);
            }
            if ("cond".equals(symbolName)) {
                return evalCond(listExpr, env);
            }
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

    private Value evalDefine(ListExpr listExpr, Environment env) throws EvalError {
        ensureAtLeastExpressions("define", listExpr, 3);

        Expr target = listExpr.elements().get(1);
        if (target instanceof SymbolExpr symbolExpr) {
            if (listExpr.elements().size() != 3) {
                throw error(listExpr.loc(),
                        "define expected 2 arguments but got " + (listExpr.elements().size() - 1));
            }
            Value value = eval(listExpr.elements().get(2), env);
            env.define(symbolExpr.name(), value);
            return VOID;
        }

        if (target instanceof ListExpr signature) {
            if (signature.elements().isEmpty()) {
                throw error(target.loc(), "define requires a function name");
            }
            Expr nameExpr = signature.elements().getFirst();
            if (!(nameExpr instanceof SymbolExpr nameSymbol)) {
                throw error(nameExpr.loc(), "define requires a function name");
            }

            List<String> parameters = parseParameters(
                    signature.elements().subList(1, signature.elements().size()),
                    "define"
            );
            List<Expr> body = List.copyOf(listExpr.elements().subList(2, listExpr.elements().size()));
            UserProcedure procedure = new UserProcedure(nameSymbol.name(), parameters, body, env);
            env.define(nameSymbol.name(), procedure);
            return VOID;
        }

        throw error(target.loc(), "define requires a symbol or parameter list");
    }

    private Value evalIf(ListExpr listExpr, Environment env) throws EvalError {
        ensureExactlyExpressions("if", listExpr, 4);
        Value condition = eval(listExpr.elements().get(1), env);
        Expr branch = condition.isTruthy() ? listExpr.elements().get(2) : listExpr.elements().get(3);
        return eval(branch, env);
    }

    private Value evalQuote(ListExpr listExpr) throws EvalError {
        ensureExactlyExpressions("quote", listExpr, 2);
        return quoteToValue(listExpr.elements().get(1));
    }

    private Value evalLambda(ListExpr listExpr, Environment env) throws EvalError {
        ensureAtLeastExpressions("lambda", listExpr, 3);
        Expr parametersExpr = listExpr.elements().get(1);
        if (!(parametersExpr instanceof ListExpr parametersList)) {
            throw error(parametersExpr.loc(), "lambda requires a parameter list");
        }

        List<String> parameters = parseParameters(parametersList.elements(), "lambda");
        List<Expr> body = List.copyOf(listExpr.elements().subList(2, listExpr.elements().size()));
        return new UserProcedure("lambda", parameters, body, env);
    }

    private Value evalBegin(ListExpr listExpr, Environment env) throws EvalError {
        return evalSequence(listExpr.elements().subList(1, listExpr.elements().size()), env);
    }

    private Value evalLet(ListExpr listExpr, Environment env) throws EvalError {
        ensureAtLeastExpressions("let", listExpr, 3);

        Expr secondExpr = listExpr.elements().get(1);
        if (secondExpr instanceof SymbolExpr nameSymbol) {
            if (listExpr.elements().size() < 4) {
                throw error(listExpr.loc(),
                        "let expected at least 2 arguments but got "
                                + (listExpr.elements().size() - 1));
            }

            Expr bindingExpr = listExpr.elements().get(2);
            if (!(bindingExpr instanceof ListExpr bindingList)) {
                throw error(bindingExpr.loc(), "let requires a binding list");
            }

            BindingParseResult bindings = parseBindings(bindingList, env);
            List<Expr> body = List.copyOf(listExpr.elements().subList(3, listExpr.elements().size()));

            Environment namedLetEnv = new Environment(env);
            UserProcedure procedure = new UserProcedure(
                    nameSymbol.name(),
                    bindings.names(),
                    body,
                    namedLetEnv
            );
            namedLetEnv.define(nameSymbol.name(), procedure);
            return procedure.apply(bindings.values(), listExpr.loc());
        }

        if (!(secondExpr instanceof ListExpr bindingList)) {
            throw error(secondExpr.loc(), "let requires a binding list");
        }

        BindingParseResult bindings = parseBindings(bindingList, env);
        Environment letEnv = new Environment(env);
        for (int index = 0; index < bindings.names().size(); index++) {
            letEnv.define(bindings.names().get(index), bindings.values().get(index));
        }
        return evalSequence(listExpr.elements().subList(2, listExpr.elements().size()), letEnv);
    }

    private Value evalCond(ListExpr listExpr, Environment env) throws EvalError {
        ensureAtLeastExpressions("cond", listExpr, 2);

        List<Expr> clauses = listExpr.elements().subList(1, listExpr.elements().size());
        for (int clauseIndex = 0; clauseIndex < clauses.size(); clauseIndex++) {
            Expr clauseExpr = clauses.get(clauseIndex);
            if (!(clauseExpr instanceof ListExpr clauseList)) {
                throw error(clauseExpr.loc(), "cond clauses must be lists");
            }
            if (clauseList.elements().isEmpty()) {
                throw error(clauseExpr.loc(), "cond clauses cannot be empty");
            }

            Expr testExpr = clauseList.elements().getFirst();
            boolean isElseClause = testExpr instanceof SymbolExpr symbolExpr
                    && "else".equals(symbolExpr.name());
            if (isElseClause) {
                if (clauseIndex != clauses.size() - 1) {
                    throw error(testExpr.loc(), "cond else clause must be last");
                }
                return evalSequence(clauseList.elements().subList(1, clauseList.elements().size()), env);
            }

            Value testValue = eval(testExpr, env);
            if (!testValue.isTruthy()) {
                continue;
            }
            if (clauseList.elements().size() == 1) {
                return testValue;
            }
            return evalSequence(clauseList.elements().subList(1, clauseList.elements().size()), env);
        }

        return VOID;
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

    private Value applyCons(List<Value> arguments, SourceLoc callLoc) throws EvalError {
        ensureExactly("cons", arguments, 2, callLoc);
        return new PairValue(arguments.get(0), arguments.get(1));
    }

    private Value applyCar(List<Value> arguments, SourceLoc callLoc) throws EvalError {
        ensureExactly("car", arguments, 1, callLoc);
        return requirePair(arguments.getFirst(), "car", callLoc).car();
    }

    private Value applyCdr(List<Value> arguments, SourceLoc callLoc) throws EvalError {
        ensureExactly("cdr", arguments, 1, callLoc);
        return requirePair(arguments.getFirst(), "cdr", callLoc).cdr();
    }

    private Value applyNullPredicate(List<Value> arguments, SourceLoc callLoc) throws EvalError {
        ensureExactly("null?", arguments, 1, callLoc);
        return arguments.getFirst() instanceof EmptyListValue ? TRUE : FALSE;
    }

    private Value applyList(List<Value> arguments, SourceLoc callLoc) {
        return buildList(arguments);
    }

    private Value applyLength(List<Value> arguments, SourceLoc callLoc) throws EvalError {
        ensureExactly("length", arguments, 1, callLoc);
        List<Value> elements = requireProperList(arguments.getFirst(), "length", callLoc);
        return new NumberValue(Rational.integer(BigInteger.valueOf(elements.size())));
    }

    private Value applyAppend(List<Value> arguments, SourceLoc callLoc) throws EvalError {
        if (arguments.isEmpty()) {
            return EMPTY_LIST;
        }

        Value result = arguments.get(arguments.size() - 1);
        for (int index = arguments.size() - 2; index >= 0; index--) {
            List<Value> elements = requireProperList(arguments.get(index), "append", callLoc);
            for (int elementIndex = elements.size() - 1; elementIndex >= 0; elementIndex--) {
                result = new PairValue(elements.get(elementIndex), result);
            }
        }
        return result;
    }

    private Value applyStringPredicate(List<Value> arguments, SourceLoc callLoc) throws EvalError {
        ensureExactly("string?", arguments, 1, callLoc);
        return arguments.getFirst() instanceof StringValue ? TRUE : FALSE;
    }

    private Value applyNumberPredicate(List<Value> arguments, SourceLoc callLoc) throws EvalError {
        ensureExactly("number?", arguments, 1, callLoc);
        return arguments.getFirst() instanceof NumberValue ? TRUE : FALSE;
    }

    private Value applyBooleanPredicate(List<Value> arguments, SourceLoc callLoc) throws EvalError {
        ensureExactly("boolean?", arguments, 1, callLoc);
        return arguments.getFirst() instanceof BooleanValue ? TRUE : FALSE;
    }

    private Value applyPairPredicate(List<Value> arguments, SourceLoc callLoc) throws EvalError {
        ensureExactly("pair?", arguments, 1, callLoc);
        return arguments.getFirst() instanceof PairValue ? TRUE : FALSE;
    }

    private Value applySymbolPredicate(List<Value> arguments, SourceLoc callLoc) throws EvalError {
        ensureExactly("symbol?", arguments, 1, callLoc);
        return arguments.getFirst() instanceof SymbolValue ? TRUE : FALSE;
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

    private Value evalSequence(List<Expr> expressions, Environment env) throws EvalError {
        if (expressions.isEmpty()) {
            return VOID;
        }

        Value lastValue = VOID;
        for (Expr expression : expressions) {
            lastValue = eval(expression, env);
        }
        return lastValue;
    }

    private Value quoteToValue(Expr expression) throws EvalError {
        return switch (expression) {
            case NumberExpr numberExpr -> new NumberValue(numberExpr.value());
            case BooleanExpr booleanExpr -> booleanExpr.value() ? TRUE : FALSE;
            case StringExpr stringExpr -> new StringValue(stringExpr.value());
            case SymbolExpr symbolExpr -> new SymbolValue(symbolExpr.name());
            case ListExpr listExpr -> quoteList(listExpr.elements());
        };
    }

    private Value quoteList(List<Expr> expressions) throws EvalError {
        Value result = EMPTY_LIST;
        for (int index = expressions.size() - 1; index >= 0; index--) {
            result = new PairValue(quoteToValue(expressions.get(index)), result);
        }
        return result;
    }

    private BindingParseResult parseBindings(ListExpr bindingsList, Environment env) throws EvalError {
        List<String> names = new ArrayList<>(bindingsList.elements().size());
        List<Value> values = new ArrayList<>(bindingsList.elements().size());

        for (Expr bindingExpr : bindingsList.elements()) {
            if (!(bindingExpr instanceof ListExpr bindingList)) {
                throw error(bindingExpr.loc(), "let bindings must be lists");
            }
            if (bindingList.elements().size() != 2) {
                throw error(bindingExpr.loc(), "let bindings must contain a name and value");
            }

            Expr nameExpr = bindingList.elements().getFirst();
            if (!(nameExpr instanceof SymbolExpr symbolExpr)) {
                throw error(nameExpr.loc(), "let binding names must be symbols");
            }

            names.add(symbolExpr.name());
            values.add(eval(bindingList.elements().get(1), env));
        }

        return new BindingParseResult(List.copyOf(names), List.copyOf(values));
    }

    private List<String> parseParameters(List<Expr> parameterExprs, String formName)
            throws EvalError {
        List<String> parameters = new ArrayList<>(parameterExprs.size());
        for (Expr parameterExpr : parameterExprs) {
            if (!(parameterExpr instanceof SymbolExpr symbolExpr)) {
                throw error(parameterExpr.loc(), formName + " parameters must be symbols");
            }
            parameters.add(symbolExpr.name());
        }
        return List.copyOf(parameters);
    }

    private Rational requireNumber(Value value, String procedureName, SourceLoc callLoc)
            throws EvalError {
        if (value instanceof NumberValue numberValue) {
            return numberValue.value();
        }
        throw error(callLoc, procedureName + " expects numeric arguments");
    }

    private PairValue requirePair(Value value, String procedureName, SourceLoc callLoc)
            throws EvalError {
        if (value instanceof PairValue pairValue) {
            return pairValue;
        }
        throw error(callLoc, procedureName + " expects a pair");
    }

    private List<Value> requireProperList(Value value, String procedureName, SourceLoc callLoc)
            throws EvalError {
        List<Value> elements = new ArrayList<>();
        Value current = value;
        while (current instanceof PairValue pairValue) {
            elements.add(pairValue.car());
            current = pairValue.cdr();
        }
        if (current instanceof EmptyListValue) {
            return elements;
        }
        throw error(callLoc, procedureName + " expects a proper list");
    }

    private Value buildList(List<Value> elements) {
        Value result = EMPTY_LIST;
        for (int index = elements.size() - 1; index >= 0; index--) {
            result = new PairValue(elements.get(index), result);
        }
        return result;
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

    private void ensureExactlyExpressions(String formName, ListExpr listExpr, int expectedSize)
            throws EvalError {
        if (listExpr.elements().size() != expectedSize) {
            throw error(listExpr.loc(),
                    formName + " expected " + (expectedSize - 1) + " arguments but got "
                            + (listExpr.elements().size() - 1));
        }
    }

    private void ensureAtLeastExpressions(String formName, ListExpr listExpr, int minimumSize)
            throws EvalError {
        if (listExpr.elements().size() < minimumSize) {
            throw error(listExpr.loc(),
                    formName + " expected at least " + (minimumSize - 1) + " arguments but got "
                            + (listExpr.elements().size() - 1));
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

    private sealed interface Value permits NumberValue,
            BooleanValue,
            StringValue,
            SymbolValue,
            PairValue,
            EmptyListValue,
            BuiltinProcedure,
            UserProcedure,
            VoidValue {
        String render();

        default boolean isTruthy() {
            return true;
        }
    }

    private record SourceLoc(int line, int column) {
    }

    private record BindingParseResult(List<String> names, List<Value> values) {
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

    private record SymbolValue(String name) implements Value {
        @Override
        public String render() {
            return name;
        }
    }

    private record PairValue(Value car, Value cdr) implements Value {
        @Override
        public String render() {
            StringBuilder builder = new StringBuilder();
            builder.append('(');
            appendPairContents(builder, this);
            builder.append(')');
            return builder.toString();
        }

        private static void appendPairContents(StringBuilder builder, PairValue pair) {
            builder.append(pair.car.render());
            if (pair.cdr instanceof EmptyListValue) {
                return;
            }
            if (pair.cdr instanceof PairValue nextPair) {
                builder.append(' ');
                appendPairContents(builder, nextPair);
                return;
            }
            builder.append(" . ");
            builder.append(pair.cdr.render());
        }
    }

    private enum EmptyListValue implements Value {
        INSTANCE;

        @Override
        public String render() {
            return "()";
        }
    }

    private enum VoidValue implements Value {
        INSTANCE;

        @Override
        public String render() {
            return "";
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

    private final class UserProcedure implements Value, Procedure {
        private final String name;
        private final List<String> parameters;
        private final List<Expr> body;
        private final Environment closureEnv;

        private UserProcedure(String name, List<String> parameters, List<Expr> body, Environment closureEnv) {
            this.name = name;
            this.parameters = parameters;
            this.body = body;
            this.closureEnv = closureEnv;
        }

        @Override
        public String render() {
            return "#<procedure:" + name + ">";
        }

        @Override
        public Value apply(List<Value> arguments, SourceLoc callLoc) throws EvalError {
            if (arguments.size() != parameters.size()) {
                throw error(callLoc,
                        name + " expected " + parameters.size() + " arguments but got "
                                + arguments.size());
            }

            Environment callEnv = new Environment(closureEnv);
            for (int index = 0; index < parameters.size(); index++) {
                callEnv.define(parameters.get(index), arguments.get(index));
            }
            return evalSequence(body, callEnv);
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
            if (ch == '\'') {
                return parseQuoteShorthand();
            }
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

        private Expr parseQuoteShorthand() throws EvalError {
            SourceLoc start = currentLoc();
            advance();
            Expr quotedExpr = parseExpression();
            return new ListExpr(
                    start,
                    List.of(
                            new SymbolExpr(start, "quote"),
                            quotedExpr
                    )
            );
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
