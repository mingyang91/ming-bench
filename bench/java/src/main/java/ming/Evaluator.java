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
    private final Environment globalEnv;
    private StringBuilder activeOutput;

    public Evaluator() {
        globalEnv = createGlobalEnv();
    }

    /**
     * Evaluate one or more Scheme expressions and return the string
     * representation of the last result.
     */
    public String evalStr(String input) throws EvalError {
        return evalProgram(input, null).render();
    }

    private Value evalProgram(String input, StringBuilder output) throws EvalError {
        StringBuilder previousOutput = activeOutput;
        activeOutput = output;

        Parser parser = new Parser(input);
        Value lastValue = null;

        try {
            while (parser.hasMore()) {
                lastValue = eval(parser.parseExpr(), globalEnv);
            }

            if (lastValue == null) {
                throw new EvalError("empty input", 1, 1);
            }

            return lastValue;
        } finally {
            activeOutput = previousOutput;
        }
    }

    /**
     * Evaluate Scheme expressions and return both the result string
     * and any captured output from display/write/newline.
     */
    public EvalResult evalStrWithOutput(String input) throws EvalError {
        StringBuilder output = new StringBuilder();
        Value result = evalProgram(input, output);
        return new EvalResult(result.render(), output.toString());
    }

    private Environment createGlobalEnv() {
        Environment env = new Environment(null);

        env.define("+", builtin("+", args -> new IntValue(sum(args))));
        env.define("-", builtin("-", args -> new IntValue(subtract(args))));
        env.define("*", builtin("*", args -> new IntValue(multiply(args))));
        env.define("/", builtin("/", args -> new IntValue(divide(args))));
        env.define("<", builtin("<",
                args -> BoolValue.of(compareIncreasing(args, Comparison.STRICTLY_LESS))));
        env.define(">", builtin(">",
                args -> BoolValue.of(compareIncreasing(args, Comparison.STRICTLY_GREATER))));
        env.define("=", builtin("=",
                args -> BoolValue.of(compareIncreasing(args, Comparison.EQUAL))));
        env.define("<=", builtin("<=",
                args -> BoolValue.of(compareIncreasing(args, Comparison.LESS_OR_EQUAL))));
        env.define("not", builtin("not", args -> {
            requireArity("not", args.size(), 1);
            return BoolValue.of(!isTruthy(args.getFirst()));
        }));
        env.define("cons", builtin("cons", args -> {
            requireArity("cons", args.size(), 2);
            return new PairValue(args.get(0), args.get(1));
        }));
        env.define("car", builtin("car", args -> {
            requireArity("car", args.size(), 1);
            return expectPair(args.getFirst()).car();
        }));
        env.define("cdr", builtin("cdr", args -> {
            requireArity("cdr", args.size(), 1);
            return expectPair(args.getFirst()).cdr();
        }));
        env.define("null?", builtin("null?", args -> {
            requireArity("null?", args.size(), 1);
            return BoolValue.of(args.getFirst() instanceof EmptyListValue);
        }));
        env.define("list", builtin("list", this::makeList));
        env.define("length", builtin("length", args -> {
            requireArity("length", args.size(), 1);
            return new IntValue(lengthOfList(args.getFirst()));
        }));
        env.define("append", builtin("append", this::appendLists));
        env.define("string?", builtin("string?",
                args -> typePredicate("string?", args, value -> value instanceof StringValue)));
        env.define("number?", builtin("number?",
                args -> typePredicate("number?", args, value -> value instanceof IntValue)));
        env.define("boolean?", builtin("boolean?",
                args -> typePredicate("boolean?", args, value -> value instanceof BoolValue)));
        env.define("pair?", builtin("pair?",
                args -> typePredicate("pair?", args, value -> value instanceof PairValue)));
        env.define("symbol?", builtin("symbol?",
                args -> typePredicate("symbol?", args, value -> value instanceof SymbolValue)));
        env.define("display", builtin("display", args -> {
            requireArity("display", args.size(), 1);
            appendOutput(renderForDisplay(args.getFirst()));
            return VoidValue.INSTANCE;
        }));
        env.define("write", builtin("write", args -> {
            requireArity("write", args.size(), 1);
            appendOutput(args.getFirst().render());
            return VoidValue.INSTANCE;
        }));
        env.define("newline", builtin("newline", args -> {
            requireArity("newline", args.size(), 0);
            appendOutput("\n");
            return VoidValue.INSTANCE;
        }));
        env.define("string-append", builtin("string-append",
                args -> new StringValue(stringAppend(args))));
        env.define("string-length", builtin("string-length", args -> {
            requireArity("string-length", args.size(), 1);
            return new IntValue(expectString(args.getFirst()).length());
        }));
        env.define("substring", builtin("substring", args -> {
            requireArity("substring", args.size(), 3);
            String value = expectString(args.get(0));
            int start = expectIndex(args.get(1), "substring");
            int end = expectIndex(args.get(2), "substring");
            if (start > end || end > value.length()) {
                throw new EvalError("substring indices out of range");
            }
            return new StringValue(value.substring(start, end));
        }));
        env.define("string->number", builtin("string->number", args -> {
            requireArity("string->number", args.size(), 1);
            String value = expectString(args.getFirst());
            try {
                return new IntValue(Integer.parseInt(value));
            } catch (NumberFormatException error) {
                return BoolValue.FALSE;
            }
        }));
        env.define("number->string", builtin("number->string", args -> {
            requireArity("number->string", args.size(), 1);
            return new StringValue(Integer.toString(expectInt(args.getFirst())));
        }));
        env.define("symbol->string", builtin("symbol->string", args -> {
            requireArity("symbol->string", args.size(), 1);
            return new StringValue(expectSymbol(args.getFirst()));
        }));
        env.define("string->symbol", builtin("string->symbol", args -> {
            requireArity("string->symbol", args.size(), 1);
            return new SymbolValue(expectString(args.getFirst()));
        }));
        env.define("string-ref", builtin("string-ref", args -> {
            requireArity("string-ref", args.size(), 2);
            StringValue value = expectStringValue(args.get(0));
            int index = expectIndex(args.get(1), "string-ref");
            if (index >= value.length()) {
                throw new EvalError("string-ref index out of range");
            }
            return new CharValue(value.charAt(index));
        }));
        env.define("string-copy", builtin("string-copy", args -> {
            requireArity("string-copy", args.size(), 1);
            return expectStringValue(args.getFirst()).copy(true);
        }));
        env.define("string-set!", builtin("string-set!", args -> {
            requireArity("string-set!", args.size(), 3);
            StringValue value = expectStringValue(args.get(0));
            int index = expectIndex(args.get(1), "string-set!");
            if (index >= value.length()) {
                throw new EvalError("string-set! index out of range");
            }
            value.setCharAt(index, expectChar(args.get(2)));
            return VoidValue.INSTANCE;
        }));
        env.define("char?", builtin("char?",
                args -> typePredicate("char?", args, value -> value instanceof CharValue)));

        return env;
    }

    private ProcedureValue builtin(String name, BuiltinAction action) {
        return new BuiltinProcedure(name, action);
    }

    private void appendOutput(String text) {
        if (activeOutput != null) {
            activeOutput.append(text);
        }
    }

    private String renderForDisplay(Value value) {
        if (value instanceof StringValue stringValue) {
            return stringValue.value();
        }
        if (value instanceof CharValue charValue) {
            return Character.toString(charValue.value());
        }
        return value.render();
    }

    private Value eval(Expr expr, Environment env) throws EvalError {
        try {
            return switch (expr) {
                case IntExpr intExpr -> new IntValue(intExpr.value());
                case BoolExpr boolExpr -> BoolValue.of(boolExpr.value());
                case StringExpr stringExpr -> new StringValue(stringExpr.value());
                case CharExpr charExpr -> new CharValue(charExpr.value());
                case SymbolExpr symbolExpr -> env.lookup(symbolExpr.name());
                case ListExpr listExpr -> evalList(listExpr, env);
            };
        } catch (EvalError error) {
            throw error.withPosition(expr.position().line(), expr.position().column());
        }
    }

    private Value evalList(ListExpr listExpr, Environment env) throws EvalError {
        List<Expr> elements = listExpr.elements();
        if (elements.isEmpty()) {
            throw new EvalError("cannot evaluate empty list");
        }

        Expr head = elements.getFirst();
        List<Expr> argExprs = elements.subList(1, elements.size());

        if (head instanceof SymbolExpr symbolExpr) {
            return switch (symbolExpr.name()) {
                case "define" -> evalDefine(argExprs, env);
                case "if" -> evalIf(argExprs, env);
                case "quote" -> evalQuote(argExprs);
                case "lambda" -> evalLambda(argExprs, env);
                case "begin" -> evalBegin(argExprs, env);
                case "let" -> evalLet(argExprs, env);
                case "cond" -> evalCond(argExprs, env);
                case "and" -> evalAnd(argExprs, env);
                case "or" -> evalOr(argExprs, env);
                default -> applyProcedure(eval(head, env), evalArgs(argExprs, env));
            };
        }

        return applyProcedure(eval(head, env), evalArgs(argExprs, env));
    }

    private Value evalDefine(List<Expr> argExprs, Environment env) throws EvalError {
        if (argExprs.size() < 2) {
            throw new EvalError("define requires a name and a value");
        }

        Expr target = argExprs.getFirst();
        if (target instanceof SymbolExpr symbolExpr) {
            requireArity("define", argExprs.size(), 2);
            Value value = eval(argExprs.get(1), env);
            env.define(symbolExpr.name(), value);
            return VoidValue.INSTANCE;
        }

        if (target instanceof ListExpr signatureExpr) {
            List<Expr> signature = signatureExpr.elements();
            if (signature.isEmpty()) {
                throw new EvalError("define requires a function name");
            }
            if (!(signature.getFirst() instanceof SymbolExpr nameExpr)) {
                throw new EvalError("function name must be a symbol");
            }

            List<String> parameterNames = parseParameterNames(
                    signature.subList(1, signature.size()));
            List<Expr> body = parseBody("define", argExprs.subList(1, argExprs.size()));
            ProcedureValue procedure =
                    new UserProcedure(nameExpr.name(), parameterNames, body, env);
            env.define(nameExpr.name(), procedure);
            return VoidValue.INSTANCE;
        }

        throw new EvalError("invalid define");
    }

    private Value evalIf(List<Expr> argExprs, Environment env) throws EvalError {
        requireArity("if", argExprs.size(), 3);
        Value condition = eval(argExprs.get(0), env);
        if (isTruthy(condition)) {
            return eval(argExprs.get(1), env);
        }
        return eval(argExprs.get(2), env);
    }

    private Value evalQuote(List<Expr> argExprs) throws EvalError {
        requireArity("quote", argExprs.size(), 1);
        return quoteToValue(argExprs.getFirst());
    }

    private Value evalLambda(List<Expr> argExprs, Environment env) throws EvalError {
        if (argExprs.size() < 2) {
            throw new EvalError("lambda requires parameters and a body");
        }
        if (!(argExprs.getFirst() instanceof ListExpr paramsExpr)) {
            throw new EvalError("lambda parameters must be a list");
        }

        List<String> parameterNames = parseParameterNames(paramsExpr.elements());
        List<Expr> body = parseBody("lambda", argExprs.subList(1, argExprs.size()));
        return new UserProcedure(null, parameterNames, body, env);
    }

    private Value evalBegin(List<Expr> argExprs, Environment env) throws EvalError {
        return evalSequence(argExprs, env);
    }

    private Value evalLet(List<Expr> argExprs, Environment env) throws EvalError {
        if (argExprs.isEmpty()) {
            throw new EvalError("let requires bindings and a body");
        }

        Expr firstArg = argExprs.getFirst();
        if (firstArg instanceof SymbolExpr nameExpr) {
            if (argExprs.size() < 2) {
                throw new EvalError("let requires bindings and a body");
            }
            if (!(argExprs.get(1) instanceof ListExpr bindingsExpr)) {
                throw new EvalError("let bindings must be a list");
            }

            List<LetBinding> bindings = parseBindings(bindingsExpr.elements());
            List<Expr> body = parseBody("let", argExprs.subList(2, argExprs.size()));
            return evalNamedLet(nameExpr.name(), bindings, body, env);
        }

        if (!(firstArg instanceof ListExpr bindingsExpr)) {
            throw new EvalError("let bindings must be a list");
        }

        List<LetBinding> bindings = parseBindings(bindingsExpr.elements());
        List<Expr> body = parseBody("let", argExprs.subList(1, argExprs.size()));
        return evalSimpleLet(bindings, body, env);
    }

    private Value evalSimpleLet(List<LetBinding> bindings, List<Expr> body, Environment env)
            throws EvalError {
        Environment letEnv = new Environment(env);
        for (LetBinding binding : bindings) {
            letEnv.define(binding.name(), eval(binding.valueExpr(), env));
        }
        return evalSequence(body, letEnv);
    }

    private Value evalNamedLet(String name, List<LetBinding> bindings, List<Expr> body,
                               Environment env) throws EvalError {
        List<String> parameterNames = new ArrayList<>(bindings.size());
        List<Value> arguments = new ArrayList<>(bindings.size());
        for (LetBinding binding : bindings) {
            parameterNames.add(binding.name());
            arguments.add(eval(binding.valueExpr(), env));
        }

        Environment letEnv = new Environment(env);
        ProcedureValue procedure = new UserProcedure(name, parameterNames, body, letEnv);
        letEnv.define(name, procedure);
        return procedure.apply(arguments);
    }

    private Value evalCond(List<Expr> argExprs, Environment env) throws EvalError {
        for (int index = 0; index < argExprs.size(); index++) {
            Expr clauseExpr = argExprs.get(index);
            if (!(clauseExpr instanceof ListExpr clauseList)) {
                throw new EvalError("cond clause must be a list");
            }

            List<Expr> clause = clauseList.elements();
            if (clause.isEmpty()) {
                throw new EvalError("cond clause cannot be empty");
            }

            Expr testExpr = clause.getFirst();
            if (testExpr instanceof SymbolExpr symbolExpr && symbolExpr.name().equals("else")) {
                if (index != argExprs.size() - 1) {
                    throw new EvalError("cond else clause must be last");
                }
                return evalClauseBody("cond", clause.subList(1, clause.size()), env, null);
            }

            Value testValue = eval(testExpr, env);
            if (isTruthy(testValue)) {
                return evalClauseBody("cond", clause.subList(1, clause.size()), env, testValue);
            }
        }

        return VoidValue.INSTANCE;
    }

    private Value evalClauseBody(String formName, List<Expr> body, Environment env,
                                 Value defaultValue) throws EvalError {
        if (body.isEmpty()) {
            if (defaultValue != null) {
                return defaultValue;
            }
            throw new EvalError(formName + " clause requires a body");
        }
        return evalSequence(body, env);
    }

    private List<String> parseParameterNames(List<Expr> params) throws EvalError {
        List<String> parameterNames = new ArrayList<>(params.size());
        for (Expr param : params) {
            if (!(param instanceof SymbolExpr symbolExpr)) {
                throw new EvalError("parameter must be a symbol");
            }
            parameterNames.add(symbolExpr.name());
        }
        return parameterNames;
    }

    private List<LetBinding> parseBindings(List<Expr> bindingExprs) throws EvalError {
        List<LetBinding> bindings = new ArrayList<>(bindingExprs.size());
        for (Expr bindingExpr : bindingExprs) {
            if (!(bindingExpr instanceof ListExpr bindingList)) {
                throw new EvalError("let binding must be a list");
            }

            List<Expr> parts = bindingList.elements();
            if (parts.size() != 2) {
                throw new EvalError("let binding must contain a name and value");
            }
            if (!(parts.getFirst() instanceof SymbolExpr symbolExpr)) {
                throw new EvalError("let binding name must be a symbol");
            }

            bindings.add(new LetBinding(symbolExpr.name(), parts.get(1)));
        }
        return bindings;
    }

    private List<Expr> parseBody(String formName, List<Expr> body) throws EvalError {
        if (body.isEmpty()) {
            throw new EvalError(formName + " requires a body");
        }
        return List.copyOf(body);
    }

    private List<Value> evalArgs(List<Expr> argExprs, Environment env) throws EvalError {
        List<Value> values = new ArrayList<>(argExprs.size());
        for (Expr argExpr : argExprs) {
            values.add(eval(argExpr, env));
        }
        return values;
    }

    private Value evalAnd(List<Expr> argExprs, Environment env) throws EvalError {
        Value result = BoolValue.TRUE;
        for (Expr argExpr : argExprs) {
            result = eval(argExpr, env);
            if (!isTruthy(result)) {
                return result;
            }
        }
        return result;
    }

    private Value evalOr(List<Expr> argExprs, Environment env) throws EvalError {
        Value lastValue = BoolValue.FALSE;
        for (Expr argExpr : argExprs) {
            Value value = eval(argExpr, env);
            if (isTruthy(value)) {
                return value;
            }
            lastValue = value;
        }
        return lastValue;
    }

    private Value evalSequence(List<Expr> exprs, Environment env) throws EvalError {
        Value result = VoidValue.INSTANCE;
        for (Expr expr : exprs) {
            result = eval(expr, env);
        }
        return result;
    }

    private Value applyProcedure(Value procedureValue, List<Value> argumentValues)
            throws EvalError {
        if (!(procedureValue instanceof ProcedureValue procedure)) {
            throw new EvalError("not a procedure");
        }
        return procedure.apply(argumentValues);
    }

    private Value quoteToValue(Expr expr) throws EvalError {
        return switch (expr) {
            case IntExpr intExpr -> new IntValue(intExpr.value());
            case BoolExpr boolExpr -> BoolValue.of(boolExpr.value());
            case StringExpr stringExpr -> new StringValue(stringExpr.value());
            case CharExpr charExpr -> new CharValue(charExpr.value());
            case SymbolExpr symbolExpr -> new SymbolValue(symbolExpr.name());
            case ListExpr listExpr -> quoteListToValue(listExpr.elements());
        };
    }

    private Value quoteListToValue(List<Expr> elements) throws EvalError {
        Value result = EmptyListValue.INSTANCE;
        for (int index = elements.size() - 1; index >= 0; index--) {
            result = new PairValue(quoteToValue(elements.get(index)), result);
        }
        return result;
    }

    private int sum(List<Value> args) throws EvalError {
        int total = 0;
        for (Value arg : args) {
            total += expectInt(arg);
        }
        return total;
    }

    private int subtract(List<Value> args) throws EvalError {
        requireAtLeast("-", args.size(), 1);

        if (args.size() == 1) {
            return -expectInt(args.getFirst());
        }

        int result = expectInt(args.getFirst());
        for (int index = 1; index < args.size(); index++) {
            result -= expectInt(args.get(index));
        }
        return result;
    }

    private int multiply(List<Value> args) throws EvalError {
        int total = 1;
        for (Value arg : args) {
            total *= expectInt(arg);
        }
        return total;
    }

    private int divide(List<Value> args) throws EvalError {
        requireAtLeast("/", args.size(), 2);

        int result = expectInt(args.getFirst());
        for (int index = 1; index < args.size(); index++) {
            int divisor = expectInt(args.get(index));
            if (divisor == 0) {
                throw new EvalError("division by zero");
            }
            result /= divisor;
        }
        return result;
    }

    private boolean compareIncreasing(List<Value> args, Comparison comparison)
            throws EvalError {
        requireAtLeast(comparison.symbol, args.size(), 2);

        int previous = expectInt(args.getFirst());
        for (int index = 1; index < args.size(); index++) {
            int current = expectInt(args.get(index));
            if (!comparison.matches(previous, current)) {
                return false;
            }
            previous = current;
        }
        return true;
    }

    private int expectInt(Value value) throws EvalError {
        if (value instanceof IntValue intValue) {
            return intValue.value();
        }
        throw new EvalError("expected number");
    }

    private int expectIndex(Value value, String operationName) throws EvalError {
        int index = expectInt(value);
        if (index < 0) {
            throw new EvalError(operationName + " index out of range");
        }
        return index;
    }

    private String expectString(Value value) throws EvalError {
        return expectStringValue(value).value();
    }

    private StringValue expectStringValue(Value value) throws EvalError {
        if (value instanceof StringValue stringValue) {
            return stringValue;
        }
        throw new EvalError("expected string");
    }

    private char expectChar(Value value) throws EvalError {
        if (value instanceof CharValue charValue) {
            return charValue.value();
        }
        throw new EvalError("expected character");
    }

    private String expectSymbol(Value value) throws EvalError {
        if (value instanceof SymbolValue symbolValue) {
            return symbolValue.name();
        }
        throw new EvalError("expected symbol");
    }

    private PairValue expectPair(Value value) throws EvalError {
        if (value instanceof PairValue pairValue) {
            return pairValue;
        }
        throw new EvalError("expected pair");
    }

    private int lengthOfList(Value value) throws EvalError {
        int length = 0;
        Value current = value;
        while (current instanceof PairValue pairValue) {
            length++;
            current = pairValue.cdr();
        }
        if (current instanceof EmptyListValue) {
            return length;
        }
        throw new EvalError("expected list");
    }

    private List<Value> listElements(Value value) throws EvalError {
        List<Value> elements = new ArrayList<>();
        Value current = value;
        while (current instanceof PairValue pairValue) {
            elements.add(pairValue.car());
            current = pairValue.cdr();
        }
        if (current instanceof EmptyListValue) {
            return elements;
        }
        throw new EvalError("expected list");
    }

    private Value appendLists(List<Value> args) throws EvalError {
        if (args.isEmpty()) {
            return EmptyListValue.INSTANCE;
        }

        Value result = args.get(args.size() - 1);
        for (int argIndex = args.size() - 2; argIndex >= 0; argIndex--) {
            List<Value> elements = listElements(args.get(argIndex));
            for (int elementIndex = elements.size() - 1; elementIndex >= 0; elementIndex--) {
                result = new PairValue(elements.get(elementIndex), result);
            }
        }
        return result;
    }

    private String stringAppend(List<Value> args) throws EvalError {
        StringBuilder builder = new StringBuilder();
        for (Value arg : args) {
            builder.append(expectString(arg));
        }
        return builder.toString();
    }

    private Value makeList(List<Value> args) {
        Value result = EmptyListValue.INSTANCE;
        for (int index = args.size() - 1; index >= 0; index--) {
            result = new PairValue(args.get(index), result);
        }
        return result;
    }

    private BoolValue typePredicate(String name, List<Value> args, ValuePredicate predicate)
            throws EvalError {
        requireArity(name, args.size(), 1);
        return BoolValue.of(predicate.matches(args.getFirst()));
    }

    private void requireArity(String name, int actual, int expected)
            throws EvalError {
        if (actual != expected) {
            throw new EvalError(
                    "wrong number of arguments for " + name + ": expected " + expected
                            + ", got " + actual
            );
        }
    }

    private void requireAtLeast(String name, int actual, int minimum)
            throws EvalError {
        if (actual < minimum) {
            throw new EvalError(
                    "wrong number of arguments for " + name + ": expected at least "
                            + minimum + ", got " + actual
            );
        }
    }

    private boolean isTruthy(Value value) {
        return !(value instanceof BoolValue boolValue) || boolValue.value();
    }

    private sealed interface Expr permits IntExpr, BoolExpr, StringExpr, CharExpr,
            SymbolExpr, ListExpr {
        SourcePos position();
    }

    private record IntExpr(int value, SourcePos position) implements Expr {
    }

    private record BoolExpr(boolean value, SourcePos position) implements Expr {
    }

    private record StringExpr(String value, SourcePos position) implements Expr {
    }

    private record CharExpr(char value, SourcePos position) implements Expr {
    }

    private record SymbolExpr(String name, SourcePos position) implements Expr {
    }

    private record ListExpr(List<Expr> elements, SourcePos position) implements Expr {
    }

    private record LetBinding(String name, Expr valueExpr) {
    }

    private record SourcePos(int line, int column) {
    }

    private interface Value {
        String render();
    }

    private record IntValue(int value) implements Value {
        @Override
        public String render() {
            return Integer.toString(value);
        }
    }

    private record BoolValue(boolean value) implements Value {
        private static final BoolValue TRUE = new BoolValue(true);
        private static final BoolValue FALSE = new BoolValue(false);

        private static BoolValue of(boolean value) {
            return value ? TRUE : FALSE;
        }

        @Override
        public String render() {
            return value ? "#t" : "#f";
        }
    }

    private static final class StringValue implements Value {
        private final StringBuilder value;
        private final boolean mutable;

        private StringValue(String value) {
            this(value, false);
        }

        private StringValue(String value, boolean mutable) {
            this.value = new StringBuilder(value);
            this.mutable = mutable;
        }

        private String value() {
            return value.toString();
        }

        private int length() {
            return value.length();
        }

        private char charAt(int index) {
            return value.charAt(index);
        }

        private void setCharAt(int index, char ch) throws EvalError {
            if (!mutable) {
                throw new EvalError("string is immutable");
            }
            value.setCharAt(index, ch);
        }

        private StringValue copy(boolean mutable) {
            return new StringValue(value(), mutable);
        }

        @Override
        public String render() {
            return "\"" + escapeString(value()) + "\"";
        }
    }

    private record CharValue(char value) implements Value {
        @Override
        public String render() {
            return switch (value) {
                case ' ' -> "#\\space";
                case '\n' -> "#\\newline";
                default -> "#\\" + value;
            };
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
            appendListContents(builder, this);
            builder.append(')');
            return builder.toString();
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
            return "#<void>";
        }
    }

    private abstract class ProcedureValue implements Value {
        @Override
        public String render() {
            return "#<procedure>";
        }

        abstract Value apply(List<Value> args) throws EvalError;
    }

    @FunctionalInterface
    private interface BuiltinAction {
        Value apply(List<Value> args) throws EvalError;
    }

    @FunctionalInterface
    private interface ValuePredicate {
        boolean matches(Value value);
    }

    private final class BuiltinProcedure extends ProcedureValue {
        private final String name;
        private final BuiltinAction action;

        private BuiltinProcedure(String name, BuiltinAction action) {
            this.name = name;
            this.action = action;
        }

        @Override
        Value apply(List<Value> args) throws EvalError {
            return action.apply(args);
        }

        @Override
        public String render() {
            return "#<procedure:" + name + ">";
        }
    }

    private final class UserProcedure extends ProcedureValue {
        private final String name;
        private final List<String> parameterNames;
        private final List<Expr> body;
        private final Environment closureEnv;

        private UserProcedure(String name, List<String> parameterNames,
                              List<Expr> body, Environment closureEnv) {
            this.name = name;
            this.parameterNames = List.copyOf(parameterNames);
            this.body = List.copyOf(body);
            this.closureEnv = closureEnv;
        }

        @Override
        Value apply(List<Value> args) throws EvalError {
            requireArity(displayName(), args.size(), parameterNames.size());

            Environment callEnv = new Environment(closureEnv);
            for (int index = 0; index < parameterNames.size(); index++) {
                callEnv.define(parameterNames.get(index), args.get(index));
            }
            return evalSequence(body, callEnv);
        }

        private String displayName() {
            return name == null ? "lambda" : name;
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

    private enum Comparison {
        STRICTLY_LESS("<") {
            @Override
            boolean matches(int left, int right) {
                return left < right;
            }
        },
        STRICTLY_GREATER(">") {
            @Override
            boolean matches(int left, int right) {
                return left > right;
            }
        },
        EQUAL("=") {
            @Override
            boolean matches(int left, int right) {
                return left == right;
            }
        },
        LESS_OR_EQUAL("<=") {
            @Override
            boolean matches(int left, int right) {
                return left <= right;
            }
        };

        private final String symbol;

        Comparison(String symbol) {
            this.symbol = symbol;
        }

        abstract boolean matches(int left, int right);
    }

    private static void appendListContents(StringBuilder builder, Value value) {
        Value current = value;
        boolean first = true;

        while (current instanceof PairValue pairValue) {
            if (!first) {
                builder.append(' ');
            }
            builder.append(pairValue.car().render());
            current = pairValue.cdr();
            first = false;
        }

        if (!(current instanceof EmptyListValue)) {
            if (!first) {
                builder.append(" . ");
            }
            builder.append(current.render());
        }
    }

    private static String escapeString(String value) {
        StringBuilder builder = new StringBuilder(value.length());
        for (int index = 0; index < value.length(); index++) {
            char ch = value.charAt(index);
            switch (ch) {
                case '\\' -> builder.append("\\\\");
                case '"' -> builder.append("\\\"");
                case '\n' -> builder.append("\\n");
                case '\t' -> builder.append("\\t");
                case '\r' -> builder.append("\\r");
                default -> builder.append(ch);
            }
        }
        return builder.toString();
    }

    private static final class Parser {
        private final String input;
        private int index;
        private int line = 1;
        private int column = 1;

        private Parser(String input) {
            this.input = input;
        }

        private boolean hasMore() {
            skipWhitespace();
            return index < input.length();
        }

        private Expr parseExpr() throws EvalError {
            skipWhitespace();
            if (index >= input.length()) {
                throw errorAtCurrent("unexpected end of input");
            }

            SourcePos position = currentPosition();
            char ch = input.charAt(index);
            return switch (ch) {
                case '(' -> parseList(position);
                case '\'' -> parseQuoted(position);
                case '"' -> parseString(position);
                case ')' -> throw errorAt(position, "unexpected ')'");
                default -> {
                    if (ch == '#' && index + 1 < input.length() && input.charAt(index + 1) == '\\') {
                        yield parseCharLiteral(position);
                    }
                    yield parseAtom(position);
                }
            };
        }

        private Expr parseQuoted(SourcePos position) throws EvalError {
            advance();
            return new ListExpr(List.of(new SymbolExpr("quote", position), parseExpr()), position);
        }

        private Expr parseList(SourcePos position) throws EvalError {
            advance();
            List<Expr> elements = new ArrayList<>();

            while (true) {
                skipWhitespace();
                if (index >= input.length()) {
                    throw errorAt(position, "unterminated list");
                }
                if (input.charAt(index) == ')') {
                    advance();
                    return new ListExpr(elements, position);
                }
                elements.add(parseExpr());
            }
        }

        private Expr parseString(SourcePos position) throws EvalError {
            advance();
            StringBuilder builder = new StringBuilder();

            while (index < input.length()) {
                char ch = readChar();
                if (ch == '"') {
                    return new StringExpr(builder.toString(), position);
                }
                if (ch == '\\') {
                    builder.append(parseEscape(position));
                    continue;
                }
                builder.append(ch);
            }

            throw errorAt(position, "unterminated string");
        }

        private Expr parseCharLiteral(SourcePos position) throws EvalError {
            advance();
            advance();

            if (index >= input.length()) {
                throw errorAt(position, "invalid character literal");
            }

            char next = input.charAt(index);
            if (isTokenDelimiter(next)) {
                advance();
                return new CharExpr(next, position);
            }

            int start = index;
            while (index < input.length() && !isTokenDelimiter(input.charAt(index))) {
                advance();
            }

            String literal = input.substring(start, index);
            return switch (literal) {
                case "space" -> new CharExpr(' ', position);
                case "newline" -> new CharExpr('\n', position);
                default -> {
                    if (literal.length() == 1) {
                        yield new CharExpr(literal.charAt(0), position);
                    }
                    throw errorAt(position, "invalid character literal");
                }
            };
        }

        private char parseEscape(SourcePos position) throws EvalError {
            if (index >= input.length()) {
                throw errorAt(position, "unterminated string escape");
            }

            char ch = readChar();
            return switch (ch) {
                case '\\' -> '\\';
                case '"' -> '"';
                case 'n' -> '\n';
                case 't' -> '\t';
                case 'r' -> '\r';
                default -> ch;
            };
        }

        private Expr parseAtom(SourcePos position) {
            int start = index;
            while (index < input.length()) {
                char ch = input.charAt(index);
                if (isTokenDelimiter(ch)) {
                    break;
                }
                advance();
            }

            String token = input.substring(start, index);
            if (token.equals("#t")) {
                return new BoolExpr(true, position);
            }
            if (token.equals("#f")) {
                return new BoolExpr(false, position);
            }
            if (token.matches("[+-]?\\d+")) {
                return new IntExpr(Integer.parseInt(token), position);
            }
            return new SymbolExpr(token, position);
        }

        private boolean isTokenDelimiter(char ch) {
            return Character.isWhitespace(ch) || ch == '(' || ch == ')' || ch == ';';
        }

        private void skipWhitespace() {
            while (index < input.length()) {
                char ch = input.charAt(index);
                if (Character.isWhitespace(ch)) {
                    advance();
                    continue;
                }
                if (ch == ';') {
                    advance();
                    while (index < input.length() && input.charAt(index) != '\n') {
                        advance();
                    }
                    continue;
                }
                break;
            }
        }

        private SourcePos currentPosition() {
            return new SourcePos(line, column);
        }

        private EvalError errorAtCurrent(String message) {
            return errorAt(currentPosition(), message);
        }

        private EvalError errorAt(SourcePos position, String message) {
            return new EvalError(message, position.line(), position.column());
        }

        private char readChar() {
            char ch = input.charAt(index);
            advance();
            return ch;
        }

        private void advance() {
            char ch = input.charAt(index++);
            if (ch == '\n') {
                line++;
                column = 1;
                return;
            }
            column++;
        }
    }
}
