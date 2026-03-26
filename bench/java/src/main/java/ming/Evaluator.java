package ming;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.function.Predicate;

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
    private StringBuilder outputBuffer;

    public Evaluator() {
        this.globalEnv = createGlobalEnv();
    }

    /**
     * Evaluate one or more Scheme expressions and return the string
     * representation of the last result.
     */
    public String evalStr(String input) throws EvalError {
        return evalProgram(input, false).result();
    }

    /**
     * Evaluate Scheme expressions and return both the result string
     * and any captured output from display/write/newline.
     */
    public EvalResult evalStrWithOutput(String input) throws EvalError {
        return evalProgram(input, true);
    }

    private EvalResult evalProgram(String input, boolean captureOutput) throws EvalError {
        List<Expr> expressions = new Parser(input).parseProgram();
        if (expressions.isEmpty()) {
            throw new EvalError("input did not contain any expressions");
        }

        StringBuilder previousOutput = outputBuffer;
        outputBuffer = captureOutput ? new StringBuilder() : null;
        try {
            Value result = VOID;
            for (Expr expression : expressions) {
                result = eval(expression, globalEnv);
            }
            String output = outputBuffer == null ? "" : outputBuffer.toString();
            return new EvalResult(render(result), output);
        } finally {
            outputBuffer = previousOutput;
        }
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
        env.define("cons", new BuiltinValue("cons", this::cons));
        env.define("car", new BuiltinValue("car", this::car));
        env.define("cdr", new BuiltinValue("cdr", this::cdr));
        env.define("null?", new BuiltinValue("null?", arguments ->
                typePredicate("null?", arguments, value -> value instanceof EmptyListValue)));
        env.define("list", new BuiltinValue("list", this::list));
        env.define("length", new BuiltinValue("length", this::length));
        env.define("append", new BuiltinValue("append", this::append));
        env.define("string?", new BuiltinValue("string?", arguments ->
                typePredicate("string?", arguments, value -> value instanceof StringValue)));
        env.define("number?", new BuiltinValue("number?", arguments ->
                typePredicate("number?", arguments, value -> value instanceof IntValue)));
        env.define("boolean?", new BuiltinValue("boolean?", arguments ->
                typePredicate("boolean?", arguments, value -> value instanceof BoolValue)));
        env.define("pair?", new BuiltinValue("pair?", arguments ->
                typePredicate("pair?", arguments, value -> value instanceof PairValue)));
        env.define("symbol?", new BuiltinValue("symbol?", arguments ->
                typePredicate("symbol?", arguments, value -> value instanceof SymbolValue)));
        env.define("display", new BuiltinValue("display", this::display));
        env.define("write", new BuiltinValue("write", this::write));
        env.define("newline", new BuiltinValue("newline", this::newline));
        env.define("string-append", new BuiltinValue("string-append", this::stringAppend));
        env.define("string-length", new BuiltinValue("string-length", this::stringLength));
        env.define("substring", new BuiltinValue("substring", this::substring));
        env.define("string->number", new BuiltinValue("string->number", this::stringToNumber));
        env.define("number->string", new BuiltinValue("number->string", this::numberToString));
        env.define("symbol->string", new BuiltinValue("symbol->string", this::symbolToString));
        env.define("string->symbol", new BuiltinValue("string->symbol", this::stringToSymbol));
        env.define("string-ref", new BuiltinValue("string-ref", this::stringRef));
        env.define("char?", new BuiltinValue("char?", arguments ->
                typePredicate("char?", arguments, value -> value instanceof CharValue)));
        return env;
    }

    private Value eval(Expr expr, Env env) throws EvalError {
        try {
            return switch (expr) {
                case IntExpr intExpr -> new IntValue(intExpr.value());
                case BoolExpr boolExpr -> boolValue(boolExpr.value());
                case StringExpr stringExpr -> new StringValue(stringExpr.value());
                case SymbolExpr symbolExpr -> env.lookup(symbolExpr.name());
                case ListExpr listExpr -> evalList(listExpr, env);
            };
        } catch (EvalError error) {
            throw error.withPosition(expr.line(), expr.column());
        }
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
                case "begin" -> evalBegin(arguments, env);
                case "let" -> evalLet(arguments, env);
                case "cond" -> evalCond(arguments, env);
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

    private Value evalBegin(List<Expr> arguments, Env env) throws EvalError {
        return evalSequence(arguments, env);
    }

    private Value evalLet(List<Expr> arguments, Env env) throws EvalError {
        if (arguments.isEmpty()) {
            throw new EvalError("let requires bindings and a body");
        }

        Expr firstArgument = arguments.get(0);
        if (firstArgument instanceof SymbolExpr nameExpr) {
            if (arguments.size() < 3) {
                throw new EvalError("named let requires bindings and a body");
            }
            return evalNamedLet(nameExpr.name(), arguments.get(1),
                    arguments.subList(2, arguments.size()), env);
        }

        if (arguments.size() < 2) {
            throw new EvalError("let requires bindings and a body");
        }

        List<BindingSpec> bindings = parseBindings(firstArgument);
        Env letEnv = new Env(env);
        bindValues(letEnv, bindings, evalBindingValues(bindings, env));
        return evalSequence(arguments.subList(1, arguments.size()), letEnv);
    }

    private Value evalNamedLet(String name, Expr bindingExpr, List<Expr> body, Env env)
            throws EvalError {
        List<BindingSpec> bindings = parseBindings(bindingExpr);
        Env letEnv = new Env(env);
        Cell binding = letEnv.definePlaceholder(name);
        ClosureValue closure = new ClosureValue(bindingNames(bindings), List.copyOf(body), letEnv);
        binding.value = closure;
        return applyClosure(closure, evalBindingValues(bindings, env));
    }

    private List<String> bindingNames(List<BindingSpec> bindings) {
        List<String> names = new ArrayList<>(bindings.size());
        for (BindingSpec binding : bindings) {
            names.add(binding.name());
        }
        return List.copyOf(names);
    }

    private List<BindingSpec> parseBindings(Expr bindingExpr) throws EvalError {
        if (!(bindingExpr instanceof ListExpr bindingList)) {
            throw new EvalError("let bindings must be a list");
        }

        List<BindingSpec> bindings = new ArrayList<>(bindingList.elements().size());
        for (Expr entryExpr : bindingList.elements()) {
            if (!(entryExpr instanceof ListExpr entry) || entry.elements().size() != 2) {
                throw new EvalError("let binding must contain a name and value");
            }
            Expr nameExpr = entry.elements().get(0);
            if (!(nameExpr instanceof SymbolExpr symbolExpr)) {
                throw new EvalError("let binding name must be a symbol");
            }
            bindings.add(new BindingSpec(symbolExpr.name(), entry.elements().get(1)));
        }
        return List.copyOf(bindings);
    }

    private List<Value> evalBindingValues(List<BindingSpec> bindings, Env env) throws EvalError {
        List<Value> values = new ArrayList<>(bindings.size());
        for (BindingSpec binding : bindings) {
            values.add(eval(binding.initExpr(), env));
        }
        return values;
    }

    private void bindValues(Env env, List<BindingSpec> bindings, List<Value> values) {
        for (int i = 0; i < bindings.size(); i++) {
            env.define(bindings.get(i).name(), values.get(i));
        }
    }

    private Value evalCond(List<Expr> arguments, Env env) throws EvalError {
        for (int index = 0; index < arguments.size(); index++) {
            Expr clauseExpr = arguments.get(index);
            if (!(clauseExpr instanceof ListExpr clause) || clause.elements().isEmpty()) {
                throw new EvalError("cond clause must be a non-empty list");
            }

            List<Expr> elements = clause.elements();
            Expr testExpr = elements.get(0);
            if (testExpr instanceof SymbolExpr symbolExpr && symbolExpr.name().equals("else")) {
                if (index != arguments.size() - 1) {
                    throw new EvalError("cond else clause must be last");
                }
                if (elements.size() == 1) {
                    return TRUE;
                }
                return evalSequence(elements.subList(1, elements.size()), env);
            }

            Value testValue = eval(testExpr, env);
            if (isTruthy(testValue)) {
                if (elements.size() == 1) {
                    return testValue;
                }
                return evalSequence(elements.subList(1, elements.size()), env);
            }
        }
        return VOID;
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

    private Value display(List<Value> arguments) throws EvalError {
        requireExactArgs("display", arguments, 1);
        appendOutput(renderDisplay(arguments.get(0)));
        return VOID;
    }

    private Value write(List<Value> arguments) throws EvalError {
        requireExactArgs("write", arguments, 1);
        appendOutput(render(arguments.get(0)));
        return VOID;
    }

    private Value newline(List<Value> arguments) throws EvalError {
        requireExactArgs("newline", arguments, 0);
        appendOutput("\n");
        return VOID;
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

    private Value cons(List<Value> arguments) throws EvalError {
        requireExactArgs("cons", arguments, 2);
        return new PairValue(arguments.get(0), arguments.get(1));
    }

    private Value car(List<Value> arguments) throws EvalError {
        requireExactArgs("car", arguments, 1);
        return requirePair(arguments.get(0), "car").car();
    }

    private Value cdr(List<Value> arguments) throws EvalError {
        requireExactArgs("cdr", arguments, 1);
        return requirePair(arguments.get(0), "cdr").cdr();
    }

    private Value list(List<Value> arguments) {
        return listValue(arguments);
    }

    private Value stringAppend(List<Value> arguments) throws EvalError {
        StringBuilder builder = new StringBuilder();
        for (Value argument : arguments) {
            builder.append(requireString(argument, "string-append"));
        }
        return new StringValue(builder.toString());
    }

    private Value stringLength(List<Value> arguments) throws EvalError {
        requireExactArgs("string-length", arguments, 1);
        return new IntValue(requireString(arguments.get(0), "string-length").length());
    }

    private Value substring(List<Value> arguments) throws EvalError {
        requireExactArgs("substring", arguments, 3);
        String value = requireString(arguments.get(0), "substring");
        int start = requireIndex(arguments.get(1), "substring");
        int end = requireIndex(arguments.get(2), "substring");
        if (start > end || end > value.length()) {
            throw new EvalError("substring index out of bounds");
        }
        return new StringValue(value.substring(start, end));
    }

    private Value stringToNumber(List<Value> arguments) throws EvalError {
        requireExactArgs("string->number", arguments, 1);
        String value = requireString(arguments.get(0), "string->number");
        try {
            return new IntValue(Long.parseLong(value));
        } catch (NumberFormatException error) {
            return FALSE;
        }
    }

    private Value numberToString(List<Value> arguments) throws EvalError {
        requireExactArgs("number->string", arguments, 1);
        return new StringValue(Long.toString(requireInt(arguments.get(0), "number->string")));
    }

    private Value symbolToString(List<Value> arguments) throws EvalError {
        requireExactArgs("symbol->string", arguments, 1);
        return new StringValue(requireSymbol(arguments.get(0), "symbol->string"));
    }

    private Value stringToSymbol(List<Value> arguments) throws EvalError {
        requireExactArgs("string->symbol", arguments, 1);
        return new SymbolValue(requireString(arguments.get(0), "string->symbol"));
    }

    private Value stringRef(List<Value> arguments) throws EvalError {
        requireExactArgs("string-ref", arguments, 2);
        String value = requireString(arguments.get(0), "string-ref");
        int index = requireIndex(arguments.get(1), "string-ref");
        if (index >= value.length()) {
            throw new EvalError("string-ref index out of bounds");
        }
        return new CharValue(value.charAt(index));
    }

    private Value length(List<Value> arguments) throws EvalError {
        requireExactArgs("length", arguments, 1);
        return new IntValue(requireProperList(arguments.get(0), "length").size());
    }

    private Value append(List<Value> arguments) throws EvalError {
        if (arguments.isEmpty()) {
            return EMPTY_LIST;
        }

        Value result = arguments.get(arguments.size() - 1);
        for (int index = arguments.size() - 2; index >= 0; index--) {
            List<Value> prefix = requireProperList(arguments.get(index), "append");
            for (int item = prefix.size() - 1; item >= 0; item--) {
                result = new PairValue(prefix.get(item), result);
            }
        }
        return result;
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

    private Value typePredicate(String name, List<Value> arguments, Predicate<Value> predicate)
            throws EvalError {
        requireExactArgs(name, arguments, 1);
        return boolValue(predicate.test(arguments.get(0)));
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

    private String requireString(Value value, String operator) throws EvalError {
        if (value instanceof StringValue stringValue) {
            return stringValue.value();
        }
        throw new EvalError(operator + " expects string arguments");
    }

    private String requireSymbol(Value value, String operator) throws EvalError {
        if (value instanceof SymbolValue symbolValue) {
            return symbolValue.name();
        }
        throw new EvalError(operator + " expects symbol arguments");
    }

    private int requireIndex(Value value, String operator) throws EvalError {
        long index = requireInt(value, operator);
        if (index < 0L || index > Integer.MAX_VALUE) {
            throw new EvalError(operator + " expects a valid index");
        }
        return (int) index;
    }

    private PairValue requirePair(Value value, String operator) throws EvalError {
        if (value instanceof PairValue pairValue) {
            return pairValue;
        }
        throw new EvalError(operator + " expects a pair");
    }

    private List<Value> requireProperList(Value value, String operator) throws EvalError {
        List<Value> elements = new ArrayList<>();
        Value current = value;
        while (current instanceof PairValue pairValue) {
            elements.add(pairValue.car());
            current = pairValue.cdr();
        }
        if (!(current instanceof EmptyListValue)) {
            throw new EvalError(operator + " expects a proper list");
        }
        return elements;
    }

    private boolean isTruthy(Value value) {
        return !(value instanceof BoolValue boolValue) || boolValue.value();
    }

    private BoolValue boolValue(boolean value) {
        return value ? TRUE : FALSE;
    }

    private String render(Value value) {
        return renderValue(value, false);
    }

    private String renderDisplay(Value value) {
        return renderValue(value, true);
    }

    private String renderValue(Value value, boolean displayMode) {
        return switch (value) {
            case IntValue intValue -> Long.toString(intValue.value());
            case BoolValue boolValue -> boolValue.value() ? "#t" : "#f";
            case StringValue stringValue ->
                    displayMode ? stringValue.value() : quote(stringValue.value());
            case SymbolValue symbolValue -> symbolValue.name();
            case EmptyListValue ignored -> "()";
            case CharValue charValue -> displayMode
                    ? Character.toString(charValue.value())
                    : renderChar(charValue.value());
            case PairValue pairValue -> renderPair(pairValue, displayMode);
            case BuiltinValue ignored -> "#<procedure>";
            case ClosureValue ignored -> "#<procedure>";
            case VoidValue ignored -> "#<void>";
            case UninitializedValue ignored -> "#<uninitialized>";
        };
    }

    private String renderPair(PairValue pairValue, boolean displayMode) {
        StringBuilder builder = new StringBuilder();
        builder.append('(');

        Value current = pairValue;
        boolean first = true;
        while (current instanceof PairValue pair) {
            if (!first) {
                builder.append(' ');
            }
            builder.append(renderValue(pair.car(), displayMode));
            current = pair.cdr();
            first = false;
        }

        if (!(current instanceof EmptyListValue)) {
            builder.append(" . ");
            builder.append(renderValue(current, displayMode));
        }

        builder.append(')');
        return builder.toString();
    }

    private String renderChar(char value) {
        return switch (value) {
            case ' ' -> "#\\space";
            case '\n' -> "#\\newline";
            default -> "#\\" + value;
        };
    }

    private void appendOutput(String value) {
        if (outputBuffer != null) {
            outputBuffer.append(value);
        }
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
        int line();

        int column();
    }

    private sealed interface Value permits IntValue, BoolValue, StringValue, SymbolValue,
            CharValue, EmptyListValue, PairValue, BuiltinValue, ClosureValue, VoidValue,
            UninitializedValue {
    }

    private record IntExpr(long value, int line, int column) implements Expr {
    }

    private record BoolExpr(boolean value, int line, int column) implements Expr {
    }

    private record StringExpr(String value, int line, int column) implements Expr {
    }

    private record SymbolExpr(String name, int line, int column) implements Expr {
    }

    private record ListExpr(List<Expr> elements, int line, int column) implements Expr {
    }

    private record IntValue(long value) implements Value {
    }

    private record BoolValue(boolean value) implements Value {
    }

    private record StringValue(String value) implements Value {
    }

    private record SymbolValue(String name) implements Value {
    }

    private record CharValue(char value) implements Value {
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

    private record BindingSpec(String name, Expr initExpr) {
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

            int startLine = line;
            int startColumn = column;
            char current = currentChar();
            if (current == '(') {
                return parseList(startLine, startColumn);
            }
            if (current == '\'') {
                advance();
                Expr quoted = parseExpression();
                return new ListExpr(
                        List.of(new SymbolExpr("quote", startLine, startColumn), quoted),
                        startLine,
                        startColumn);
            }
            if (current == '"') {
                return parseString(startLine, startColumn);
            }
            if (current == ')') {
                throw error("unexpected ')'");
            }
            return parseAtom(startLine, startColumn);
        }

        private Expr parseList(int startLine, int startColumn) throws EvalError {
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
            return new ListExpr(List.copyOf(elements), startLine, startColumn);
        }

        private Expr parseString(int startLine, int startColumn) throws EvalError {
            consume('"');
            StringBuilder builder = new StringBuilder();
            while (!isAtEnd()) {
                char current = advance();
                if (current == '"') {
                    return new StringExpr(builder.toString(), startLine, startColumn);
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

        private Expr parseAtom(int startLine, int startColumn) {
            int start = index;
            while (!isAtEnd() && !isDelimiter(currentChar())) {
                advance();
            }
            String token = input.substring(start, index);
            return parseAtomToken(token, startLine, startColumn);
        }

        private Expr parseAtomToken(String token, int startLine, int startColumn) {
            return switch (token) {
                case "#t" -> new BoolExpr(true, startLine, startColumn);
                case "#f" -> new BoolExpr(false, startLine, startColumn);
                default -> parseNumberOrSymbol(token, startLine, startColumn);
            };
        }

        private Expr parseNumberOrSymbol(String token, int startLine, int startColumn) {
            if (isIntegerToken(token)) {
                return new IntExpr(Long.parseLong(token), startLine, startColumn);
            }
            return new SymbolExpr(token, startLine, startColumn);
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
            return new EvalError(message, line, column);
        }
    }
}
