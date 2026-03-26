package ming;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Optional;
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
    private final MacroExpander macroExpander;
    private StringBuilder outputBuffer;

    public Evaluator() {
        this.globalEnv = createGlobalEnv();
        this.macroExpander = new MacroExpander();
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
        env.define("abs", new BuiltinValue("abs", this::abs));
        env.define("modulo", new BuiltinValue("modulo", this::modulo));
        env.define("remainder", new BuiltinValue("remainder", this::remainder));
        env.define("quotient", new BuiltinValue("quotient", this::quotient));
        env.define("min", new BuiltinValue("min", arguments -> extremum(arguments, "min")));
        env.define("max", new BuiltinValue("max", arguments -> extremum(arguments, "max")));
        env.define("expt", new BuiltinValue("expt", this::expt));
        env.define("<", new BuiltinValue("<", arguments -> compare(arguments, "<")));
        env.define(">", new BuiltinValue(">", arguments -> compare(arguments, ">")));
        env.define("=", new BuiltinValue("=", arguments -> compare(arguments, "=")));
        env.define("<=", new BuiltinValue("<=", arguments -> compare(arguments, "<=")));
        env.define("not", new BuiltinValue("not", this::not));
        env.define("zero?", new BuiltinValue("zero?",
                arguments -> numericPredicate("zero?", arguments, value -> value == 0L)));
        env.define("positive?", new BuiltinValue("positive?",
                arguments -> numericPredicate("positive?", arguments, value -> value > 0L)));
        env.define("negative?", new BuiltinValue("negative?",
                arguments -> numericPredicate("negative?", arguments, value -> value < 0L)));
        env.define("odd?", new BuiltinValue("odd?",
                arguments -> numericPredicate("odd?", arguments, value -> value % 2L != 0L)));
        env.define("even?", new BuiltinValue("even?",
                arguments -> numericPredicate("even?", arguments, value -> value % 2L == 0L)));
        env.define("cons", new BuiltinValue("cons", this::cons));
        env.define("car", new BuiltinValue("car", this::car));
        env.define("cdr", new BuiltinValue("cdr", this::cdr));
        env.define("null?", new BuiltinValue("null?", arguments ->
                typePredicate("null?", arguments, value -> value instanceof EmptyListValue)));
        env.define("list", new BuiltinValue("list", this::list));
        env.define("map", new BuiltinValue("map", this::map));
        env.define("length", new BuiltinValue("length", this::length));
        env.define("list-ref", new BuiltinValue("list-ref", this::listRef));
        env.define("list-tail", new BuiltinValue("list-tail", this::listTail));
        env.define("list?", new BuiltinValue("list?", this::listPredicate));
        env.define("assoc", new BuiltinValue("assoc", this::assoc));
        env.define("append", new BuiltinValue("append", this::append));
        env.define("apply", new BuiltinValue("apply", this::apply));
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
        env.define("eq?", new BuiltinValue("eq?", this::eq));
        env.define("equal?", new BuiltinValue("equal?", this::equal));
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
        env.define("string-set!", new BuiltinValue("string-set!", this::stringSet));
        env.define("string-copy", new BuiltinValue("string-copy", this::stringCopy));
        env.define("string=?", new BuiltinValue("string=?",
                arguments -> compareStrings(arguments, "string=?")));
        env.define("string<?", new BuiltinValue("string<?",
                arguments -> compareStrings(arguments, "string<?")));
        env.define("string-ci=?", new BuiltinValue("string-ci=?",
                arguments -> compareStrings(arguments, "string-ci=?")));
        env.define("string-upcase", new BuiltinValue("string-upcase", this::stringUpcase));
        env.define("string-downcase", new BuiltinValue("string-downcase", this::stringDowncase));
        env.define("char?", new BuiltinValue("char?", arguments ->
                typePredicate("char?", arguments, value -> value instanceof CharValue)));
        env.define("char-alphabetic?", new BuiltinValue("char-alphabetic?",
                this::charAlphabetic));
        env.define("char-numeric?", new BuiltinValue("char-numeric?", this::charNumeric));
        env.define("char-upcase", new BuiltinValue("char-upcase", this::charUpcase));
        env.define("char-downcase", new BuiltinValue("char-downcase", this::charDowncase));
        env.define("char=?", new BuiltinValue("char=?",
                arguments -> compareChars(arguments, "char=?")));
        env.define("char<?", new BuiltinValue("char<?",
                arguments -> compareChars(arguments, "char<?")));
        return env;
    }

    private Value eval(Expr expr, Env env) throws EvalError {
        try {
            return switch (expr) {
                case IntExpr intExpr -> new IntValue(intExpr.value());
                case BoolExpr boolExpr -> boolValue(boolExpr.value());
                case CharExpr charExpr -> new CharValue(charExpr.value());
                case StringExpr stringExpr -> immutableString(stringExpr.value());
                case SymbolExpr symbolExpr -> macroExpander.lookupSymbol(symbolExpr.name(), env);
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
        String operatorName = symbolName(operatorExpr);
        if (operatorName != null) {
            if ("define-syntax".equals(operatorName)) {
                macroExpander.defineSyntax(arguments, env);
                return VOID;
            }

            Optional<Expr> expandedMacro = macroExpander.expandInvocation(operatorName, listExpr);
            if (expandedMacro.isPresent()) {
                return eval(expandedMacro.get(), env);
            }

            return switch (operatorName) {
                case "define" -> evalDefine(arguments, env);
                case "set!" -> evalSet(arguments, env);
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
                    parseFormals(signature.subList(1, signature.size())),
                    List.copyOf(arguments.subList(1, arguments.size())),
                    env);
            return VOID;
        }

        throw new EvalError("define target must be a symbol or parameter list");
    }

    private Value evalSet(List<Expr> arguments, Env env) throws EvalError {
        requireExactArgs("set!", arguments, 2);

        Expr target = arguments.get(0);
        if (!(target instanceof SymbolExpr symbolExpr)) {
            throw new EvalError("set! target must be a symbol");
        }

        macroExpander.setSymbol(symbolExpr.name(), eval(arguments.get(1), env), env);
        return VOID;
    }

    private String symbolName(Expr expr) {
        if (expr instanceof SymbolExpr symbolExpr) {
            return symbolExpr.name();
        }
        return null;
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
                parseFormals(arguments.get(0)),
                List.copyOf(arguments.subList(1, arguments.size())),
                env);
    }

    private Formals parseFormals(Expr parameterExpr) throws EvalError {
        if (parameterExpr instanceof SymbolExpr symbolExpr) {
            return new Formals(List.of(), symbolExpr.name());
        }
        if (!(parameterExpr instanceof ListExpr parameterList)) {
            throw new EvalError("lambda parameters must be a list or symbol");
        }
        return parseFormals(parameterList.elements());
    }

    private Formals parseFormals(List<Expr> parameterExprs) throws EvalError {
        List<String> parameters = new ArrayList<>(parameterExprs.size());
        String restParameter = null;

        for (int index = 0; index < parameterExprs.size(); index++) {
            Expr parameterExpr = parameterExprs.get(index);
            if (parameterExpr instanceof SymbolExpr symbolExpr
                    && ".".equals(symbolExpr.name())) {
                if (index == parameterExprs.size() - 1) {
                    throw new EvalError("lambda rest parameter name is missing");
                }

                Expr restExpr = parameterExprs.get(index + 1);
                if (!(restExpr instanceof SymbolExpr restSymbol)
                        || ".".equals(restSymbol.name())) {
                    throw new EvalError("lambda rest parameter must be a symbol");
                }
                if (index + 2 != parameterExprs.size()) {
                    throw new EvalError("lambda rest parameter must be last");
                }
                restParameter = restSymbol.name();
                break;
            }
            if (!(parameterExpr instanceof SymbolExpr symbolExpr)) {
                throw new EvalError("lambda parameter must be a symbol");
            }
            parameters.add(symbolExpr.name());
        }
        return new Formals(List.copyOf(parameters), restParameter);
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
        ClosureValue closure = new ClosureValue(
                new Formals(bindingNames(bindings), null),
                List.copyOf(body),
                letEnv);
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
        Formals formals = closure.formals();
        int fixedCount = formals.parameters().size();
        if (formals.restParameter() == null) {
            requireExactArgs("lambda", arguments, fixedCount);
        } else if (arguments.size() < fixedCount) {
            throw new EvalError("lambda expected at least "
                    + fixedCount + " argument(s)");
        }

        Env callEnv = new Env(closure.env());
        for (int i = 0; i < fixedCount; i++) {
            callEnv.define(formals.parameters().get(i), arguments.get(i));
        }
        if (formals.restParameter() != null) {
            callEnv.define(formals.restParameter(),
                    listValue(arguments.subList(fixedCount, arguments.size())));
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
            case CharExpr charExpr -> new CharValue(charExpr.value());
            case StringExpr stringExpr -> immutableString(stringExpr.value());
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

    private Value abs(List<Value> arguments) throws EvalError {
        requireExactArgs("abs", arguments, 1);
        return new IntValue(Math.abs(requireInt(arguments.get(0), "abs")));
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

    private Value map(List<Value> arguments) throws EvalError {
        requireMinArgs("map", arguments, 2);

        Value procedure = arguments.get(0);
        List<List<Value>> lists = new ArrayList<>(arguments.size() - 1);
        int expectedLength = -1;
        for (int index = 1; index < arguments.size(); index++) {
            List<Value> list = requireProperList(arguments.get(index), "map");
            if (expectedLength == -1) {
                expectedLength = list.size();
            } else if (list.size() != expectedLength) {
                throw new EvalError("map expects lists of equal length");
            }
            lists.add(list);
        }

        List<Value> results = new ArrayList<>(expectedLength);
        for (int item = 0; item < expectedLength; item++) {
            List<Value> callArguments = new ArrayList<>(lists.size());
            for (List<Value> list : lists) {
                callArguments.add(list.get(item));
            }
            results.add(applyProcedure(procedure, callArguments));
        }
        return listValue(results);
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
        return immutableString(builder.toString());
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
        return immutableString(value.substring(start, end));
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
        return immutableString(Long.toString(requireInt(arguments.get(0), "number->string")));
    }

    private Value symbolToString(List<Value> arguments) throws EvalError {
        requireExactArgs("symbol->string", arguments, 1);
        return immutableString(requireSymbol(arguments.get(0), "symbol->string"));
    }

    private Value stringToSymbol(List<Value> arguments) throws EvalError {
        requireExactArgs("string->symbol", arguments, 1);
        return new SymbolValue(requireString(arguments.get(0), "string->symbol"));
    }

    private Value stringRef(List<Value> arguments) throws EvalError {
        requireExactArgs("string-ref", arguments, 2);
        StringValue value = requireStringValue(arguments.get(0), "string-ref");
        int index = requireIndex(arguments.get(1), "string-ref");
        if (index >= value.length()) {
            throw new EvalError("string-ref index out of bounds");
        }
        return new CharValue(value.charAt(index));
    }

    private Value stringSet(List<Value> arguments) throws EvalError {
        requireExactArgs("string-set!", arguments, 3);
        StringValue value = requireStringValue(arguments.get(0), "string-set!");
        if (!value.mutable()) {
            throw new EvalError("string-set! expects a mutable string");
        }

        int index = requireIndex(arguments.get(1), "string-set!");
        if (index >= value.length()) {
            throw new EvalError("string-set! index out of bounds");
        }

        value.setCharAt(index, requireChar(arguments.get(2), "string-set!"));
        return VOID;
    }

    private Value stringCopy(List<Value> arguments) throws EvalError {
        requireExactArgs("string-copy", arguments, 1);
        return requireStringValue(arguments.get(0), "string-copy").copy(true);
    }

    private Value stringUpcase(List<Value> arguments) throws EvalError {
        requireExactArgs("string-upcase", arguments, 1);
        return immutableString(requireString(arguments.get(0), "string-upcase").toUpperCase());
    }

    private Value stringDowncase(List<Value> arguments) throws EvalError {
        requireExactArgs("string-downcase", arguments, 1);
        return immutableString(requireString(arguments.get(0), "string-downcase").toLowerCase());
    }

    private Value length(List<Value> arguments) throws EvalError {
        requireExactArgs("length", arguments, 1);
        return new IntValue(requireProperList(arguments.get(0), "length").size());
    }

    private Value listRef(List<Value> arguments) throws EvalError {
        requireExactArgs("list-ref", arguments, 2);
        List<Value> elements = requireProperList(arguments.get(0), "list-ref");
        int index = requireIndex(arguments.get(1), "list-ref");
        if (index >= elements.size()) {
            throw new EvalError("list-ref index out of bounds");
        }
        return elements.get(index);
    }

    private Value listTail(List<Value> arguments) throws EvalError {
        requireExactArgs("list-tail", arguments, 2);
        int index = requireIndex(arguments.get(1), "list-tail");

        Value current = arguments.get(0);
        for (int i = 0; i < index; i++) {
            if (current instanceof PairValue pairValue) {
                current = pairValue.cdr();
            } else if (current instanceof EmptyListValue) {
                throw new EvalError("list-tail index out of bounds");
            } else {
                throw new EvalError("list-tail expects a proper list");
            }
        }

        if (!isProperListValue(current)) {
            throw new EvalError("list-tail expects a proper list");
        }
        return current;
    }

    private Value listPredicate(List<Value> arguments) throws EvalError {
        requireExactArgs("list?", arguments, 1);
        return boolValue(isProperListValue(arguments.get(0)));
    }

    private Value assoc(List<Value> arguments) throws EvalError {
        requireExactArgs("assoc", arguments, 2);

        Value key = arguments.get(0);
        Value current = arguments.get(1);
        while (current instanceof PairValue pairValue) {
            Value entry = pairValue.car();
            if (!(entry instanceof PairValue entryPair)) {
                throw new EvalError("assoc expects an association list");
            }
            if (equalValues(key, entryPair.car())) {
                return entry;
            }
            current = pairValue.cdr();
        }

        if (!(current instanceof EmptyListValue)) {
            throw new EvalError("assoc expects a proper list");
        }
        return FALSE;
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

    private Value apply(List<Value> arguments) throws EvalError {
        requireMinArgs("apply", arguments, 2);

        Value operator = arguments.get(0);
        List<Value> appliedArguments = new ArrayList<>();
        for (int i = 1; i < arguments.size() - 1; i++) {
            appliedArguments.add(arguments.get(i));
        }
        appliedArguments.addAll(requireProperList(arguments.get(arguments.size() - 1), "apply"));
        return applyProcedure(operator, appliedArguments);
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

    private Value modulo(List<Value> arguments) throws EvalError {
        requireExactArgs("modulo", arguments, 2);
        long dividend = requireInt(arguments.get(0), "modulo");
        long divisor = requireInt(arguments.get(1), "modulo");
        if (divisor == 0L) {
            throw new EvalError("division by zero");
        }

        long result = dividend % divisor;
        if (result != 0L && ((result > 0L) != (divisor > 0L))) {
            result += divisor;
        }
        return new IntValue(result);
    }

    private Value remainder(List<Value> arguments) throws EvalError {
        requireExactArgs("remainder", arguments, 2);
        long dividend = requireInt(arguments.get(0), "remainder");
        long divisor = requireInt(arguments.get(1), "remainder");
        if (divisor == 0L) {
            throw new EvalError("division by zero");
        }
        return new IntValue(dividend % divisor);
    }

    private Value quotient(List<Value> arguments) throws EvalError {
        requireExactArgs("quotient", arguments, 2);
        long dividend = requireInt(arguments.get(0), "quotient");
        long divisor = requireInt(arguments.get(1), "quotient");
        if (divisor == 0L) {
            throw new EvalError("division by zero");
        }
        return new IntValue(dividend / divisor);
    }

    private Value extremum(List<Value> arguments, String name) throws EvalError {
        requireMinArgs(name, arguments, 1);
        long result = requireInt(arguments.get(0), name);
        for (int index = 1; index < arguments.size(); index++) {
            long candidate = requireInt(arguments.get(index), name);
            if ("min".equals(name)) {
                result = Math.min(result, candidate);
            } else {
                result = Math.max(result, candidate);
            }
        }
        return new IntValue(result);
    }

    private Value expt(List<Value> arguments) throws EvalError {
        requireExactArgs("expt", arguments, 2);
        long base = requireInt(arguments.get(0), "expt");
        long exponent = requireInt(arguments.get(1), "expt");
        if (exponent < 0L) {
            throw new EvalError("expt expects a non-negative exponent");
        }

        long result = 1L;
        long factor = base;
        long remaining = exponent;
        while (remaining > 0L) {
            if ((remaining & 1L) != 0L) {
                result *= factor;
            }
            remaining >>= 1;
            if (remaining > 0L) {
                factor *= factor;
            }
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

    private Value compareChars(List<Value> arguments, String operator) throws EvalError {
        requireMinArgs(operator, arguments, 2);
        for (int index = 0; index < arguments.size() - 1; index++) {
            char left = requireChar(arguments.get(index), operator);
            char right = requireChar(arguments.get(index + 1), operator);
            if ("char=?".equals(operator) && left != right) {
                return FALSE;
            }
            if ("char<?".equals(operator) && left >= right) {
                return FALSE;
            }
        }
        return TRUE;
    }

    private Value compareStrings(List<Value> arguments, String operator) throws EvalError {
        requireMinArgs(operator, arguments, 2);
        for (int index = 0; index < arguments.size() - 1; index++) {
            String left = requireString(arguments.get(index), operator);
            String right = requireString(arguments.get(index + 1), operator);
            boolean matches = switch (operator) {
                case "string=?" -> left.equals(right);
                case "string<?" -> left.compareTo(right) < 0;
                case "string-ci=?" -> left.equalsIgnoreCase(right);
                default -> throw new EvalError("unknown string comparison operator: " + operator);
            };
            if (!matches) {
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

    private Value numericPredicate(String name, List<Value> arguments,
            java.util.function.LongPredicate predicate) throws EvalError {
        requireExactArgs(name, arguments, 1);
        return boolValue(predicate.test(requireInt(arguments.get(0), name)));
    }

    private Value charAlphabetic(List<Value> arguments) throws EvalError {
        requireExactArgs("char-alphabetic?", arguments, 1);
        return boolValue(Character.isLetter(requireChar(arguments.get(0), "char-alphabetic?")));
    }

    private Value charNumeric(List<Value> arguments) throws EvalError {
        requireExactArgs("char-numeric?", arguments, 1);
        return boolValue(Character.isDigit(requireChar(arguments.get(0), "char-numeric?")));
    }

    private Value charUpcase(List<Value> arguments) throws EvalError {
        requireExactArgs("char-upcase", arguments, 1);
        return new CharValue(Character.toUpperCase(requireChar(arguments.get(0), "char-upcase")));
    }

    private Value charDowncase(List<Value> arguments) throws EvalError {
        requireExactArgs("char-downcase", arguments, 1);
        return new CharValue(Character.toLowerCase(requireChar(arguments.get(0), "char-downcase")));
    }

    private Value eq(List<Value> arguments) throws EvalError {
        requireExactArgs("eq?", arguments, 2);
        return boolValue(eqValues(arguments.get(0), arguments.get(1)));
    }

    private Value equal(List<Value> arguments) throws EvalError {
        requireExactArgs("equal?", arguments, 2);
        return boolValue(equalValues(arguments.get(0), arguments.get(1)));
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
        return requireStringValue(value, operator).text();
    }

    private StringValue requireStringValue(Value value, String operator) throws EvalError {
        if (value instanceof StringValue stringValue) {
            return stringValue;
        }
        throw new EvalError(operator + " expects string arguments");
    }

    private String requireSymbol(Value value, String operator) throws EvalError {
        if (value instanceof SymbolValue symbolValue) {
            return symbolValue.name();
        }
        throw new EvalError(operator + " expects symbol arguments");
    }

    private char requireChar(Value value, String operator) throws EvalError {
        if (value instanceof CharValue charValue) {
            return charValue.value();
        }
        throw new EvalError(operator + " expects character arguments");
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

    private boolean isProperListValue(Value value) {
        Value current = value;
        while (current instanceof PairValue pairValue) {
            current = pairValue.cdr();
        }
        return current instanceof EmptyListValue;
    }

    private boolean eqValues(Value left, Value right) {
        if (left == right) {
            return true;
        }
        if (left instanceof IntValue leftInt && right instanceof IntValue rightInt) {
            return leftInt.value() == rightInt.value();
        }
        if (left instanceof BoolValue leftBool && right instanceof BoolValue rightBool) {
            return leftBool.value() == rightBool.value();
        }
        if (left instanceof CharValue leftChar && right instanceof CharValue rightChar) {
            return leftChar.value() == rightChar.value();
        }
        if (left instanceof SymbolValue leftSymbol && right instanceof SymbolValue rightSymbol) {
            return leftSymbol.name().equals(rightSymbol.name());
        }
        return left instanceof EmptyListValue && right instanceof EmptyListValue;
    }

    private boolean equalValues(Value left, Value right) {
        if (eqValues(left, right)) {
            return true;
        }
        if (left instanceof StringValue leftString && right instanceof StringValue rightString) {
            return leftString.text().equals(rightString.text());
        }
        if (left instanceof PairValue leftPair && right instanceof PairValue rightPair) {
            return equalValues(leftPair.car(), rightPair.car())
                    && equalValues(leftPair.cdr(), rightPair.cdr());
        }
        return false;
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
                    displayMode ? stringValue.text() : quote(stringValue.text());
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

    private StringValue immutableString(String value) {
        return new StringValue(new StringBuilder(value), false);
    }

    sealed interface Value permits IntValue, BoolValue, StringValue, SymbolValue,
            CharValue, EmptyListValue, PairValue, BuiltinValue, ClosureValue, VoidValue,
            UninitializedValue {
    }

    private record IntValue(long value) implements Value {
    }

    private record BoolValue(boolean value) implements Value {
    }

    private record StringValue(StringBuilder contents, boolean mutable) implements Value {
        private String text() {
            return contents.toString();
        }

        private int length() {
            return contents.length();
        }

        private char charAt(int index) {
            return contents.charAt(index);
        }

        private void setCharAt(int index, char value) {
            contents.setCharAt(index, value);
        }

        private StringValue copy(boolean mutableCopy) {
            return new StringValue(new StringBuilder(text()), mutableCopy);
        }
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

    private record ClosureValue(Formals formals, List<Expr> body, Env env)
            implements Value {
    }

    private record Formals(List<String> parameters, String restParameter) {
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

    static final class Cell {
        private Value value;

        private Cell(Value value) {
            this.value = value;
        }

        Value get() {
            return value;
        }

        void set(Value newValue) {
            value = newValue;
        }

        boolean isUninitialized() {
            return value == UNINITIALIZED;
        }
    }

    static final class Env {
        private final Env parent;
        private final Map<String, Cell> bindings = new HashMap<>();

        private Env(Env parent) {
            this.parent = parent;
        }

        void define(String name, Value value) {
            bindings.put(name, new Cell(value));
        }

        Cell definePlaceholder(String name) {
            Cell cell = new Cell(UNINITIALIZED);
            bindings.put(name, cell);
            return cell;
        }

        Value lookup(String name) throws EvalError {
            Cell cell = lookupCell(name);
            if (cell == null || cell.isUninitialized()) {
                throw new EvalError("unbound variable: " + name);
            }
            return cell.get();
        }

        void set(String name, Value value) throws EvalError {
            Cell cell = lookupCell(name);
            if (cell == null || cell.isUninitialized()) {
                throw new EvalError("unbound variable: " + name);
            }
            cell.set(value);
        }

        Cell lookupCell(String name) {
            if (bindings.containsKey(name)) {
                return bindings.get(name);
            }
            if (parent != null) {
                return parent.lookupCell(name);
            }
            return null;
        }
    }
}
