package ming;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Locale;
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
        env.define("abs", builtin("abs", args -> {
            requireArity("abs", args.size(), 1);
            return new IntValue(Math.abs(expectInt(args.getFirst())));
        }));
        env.define("quotient", builtin("quotient", args -> new IntValue(quotient(args))));
        env.define("remainder", builtin("remainder", args -> new IntValue(remainder(args))));
        env.define("modulo", builtin("modulo", args -> new IntValue(modulo(args))));
        env.define("min", builtin("min", args -> new IntValue(min(args))));
        env.define("max", builtin("max", args -> new IntValue(max(args))));
        env.define("expt", builtin("expt", args -> new IntValue(expt(args))));
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
        env.define("zero?", builtin("zero?", args -> {
            requireArity("zero?", args.size(), 1);
            return BoolValue.of(expectInt(args.getFirst()) == 0);
        }));
        env.define("positive?", builtin("positive?", args -> {
            requireArity("positive?", args.size(), 1);
            return BoolValue.of(expectInt(args.getFirst()) > 0);
        }));
        env.define("negative?", builtin("negative?", args -> {
            requireArity("negative?", args.size(), 1);
            return BoolValue.of(expectInt(args.getFirst()) < 0);
        }));
        env.define("odd?", builtin("odd?", args -> {
            requireArity("odd?", args.size(), 1);
            return BoolValue.of(expectInt(args.getFirst()) % 2 != 0);
        }));
        env.define("even?", builtin("even?", args -> {
            requireArity("even?", args.size(), 1);
            return BoolValue.of(expectInt(args.getFirst()) % 2 == 0);
        }));
        env.define("list", builtin("list", this::makeList));
        env.define("length", builtin("length", args -> {
            requireArity("length", args.size(), 1);
            return new IntValue(lengthOfList(args.getFirst()));
        }));
        env.define("list-ref", builtin("list-ref", this::listRef));
        env.define("list-tail", builtin("list-tail", this::listTailBuiltin));
        env.define("list?", builtin("list?", args -> {
            requireArity("list?", args.size(), 1);
            return BoolValue.of(isProperList(args.getFirst()));
        }));
        env.define("append", builtin("append", this::appendLists));
        env.define("apply", builtin("apply", this::applyBuiltin));
        env.define("map", builtin("map", this::mapBuiltin));
        env.define("eq?", builtin("eq?", args -> {
            requireArity("eq?", args.size(), 2);
            return BoolValue.of(isEq(args.get(0), args.get(1)));
        }));
        env.define("equal?", builtin("equal?", args -> {
            requireArity("equal?", args.size(), 2);
            return BoolValue.of(isEqual(args.get(0), args.get(1)));
        }));
        env.define("assoc", builtin("assoc", this::assocBuiltin));
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
        env.define("string=?", builtin("string=?",
                args -> BoolValue.of(compareStrings(args, "string=?", StringComparison.EQUAL))));
        env.define("string<?", builtin("string<?",
                args -> BoolValue.of(compareStrings(args, "string<?", StringComparison.LESS))));
        env.define("string-ci=?", builtin("string-ci=?", args -> {
            requireAtLeast("string-ci=?", args.size(), 2);

            String previous = expectString(args.getFirst());
            for (int index = 1; index < args.size(); index++) {
                String current = expectString(args.get(index));
                if (!previous.equalsIgnoreCase(current)) {
                    return BoolValue.FALSE;
                }
                previous = current;
            }
            return BoolValue.TRUE;
        }));
        env.define("string-upcase", builtin("string-upcase", args -> {
            requireArity("string-upcase", args.size(), 1);
            return new StringValue(expectString(args.getFirst()).toUpperCase(Locale.ROOT));
        }));
        env.define("string-downcase", builtin("string-downcase", args -> {
            requireArity("string-downcase", args.size(), 1);
            return new StringValue(expectString(args.getFirst()).toLowerCase(Locale.ROOT));
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
        env.define("char-alphabetic?", builtin("char-alphabetic?", args -> {
            requireArity("char-alphabetic?", args.size(), 1);
            return BoolValue.of(Character.isLetter(expectChar(args.getFirst())));
        }));
        env.define("char-numeric?", builtin("char-numeric?", args -> {
            requireArity("char-numeric?", args.size(), 1);
            return BoolValue.of(Character.isDigit(expectChar(args.getFirst())));
        }));
        env.define("char-upcase", builtin("char-upcase", args -> {
            requireArity("char-upcase", args.size(), 1);
            return new CharValue(Character.toUpperCase(expectChar(args.getFirst())));
        }));
        env.define("char-downcase", builtin("char-downcase", args -> {
            requireArity("char-downcase", args.size(), 1);
            return new CharValue(Character.toLowerCase(expectChar(args.getFirst())));
        }));
        env.define("char=?", builtin("char=?",
                args -> BoolValue.of(compareChars(args, "char=?", CharComparison.EQUAL))));
        env.define("char<?", builtin("char<?",
                args -> BoolValue.of(compareChars(args, "char<?", CharComparison.LESS))));

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
                case "set!" -> evalSet(argExprs, env);
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

            ParameterSpec parameters = parseParameterSpec(signature.subList(1, signature.size()));
            List<Expr> body = parseBody("define", argExprs.subList(1, argExprs.size()));
            ProcedureValue procedure = new UserProcedure(nameExpr.name(), parameters, body, env);
            env.define(nameExpr.name(), procedure);
            return VoidValue.INSTANCE;
        }

        throw new EvalError("invalid define");
    }

    private Value evalSet(List<Expr> argExprs, Environment env) throws EvalError {
        requireArity("set!", argExprs.size(), 2);
        if (!(argExprs.getFirst() instanceof SymbolExpr symbolExpr)) {
            throw new EvalError("set! target must be a symbol");
        }

        Value value = eval(argExprs.get(1), env);
        env.set(symbolExpr.name(), value);
        return VoidValue.INSTANCE;
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

        ParameterSpec parameters = parseLambdaParameterSpec(argExprs.getFirst());
        List<Expr> body = parseBody("lambda", argExprs.subList(1, argExprs.size()));
        return new UserProcedure(null, parameters, body, env);
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
        ProcedureValue procedure = new UserProcedure(name,
                new ParameterSpec(parameterNames, null), body, letEnv);
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

    private ParameterSpec parseLambdaParameterSpec(Expr paramsExpr) throws EvalError {
        if (paramsExpr instanceof ListExpr paramsList) {
            return parseParameterSpec(paramsList.elements());
        }
        if (paramsExpr instanceof SymbolExpr symbolExpr) {
            if (symbolExpr.name().equals(".")) {
                throw new EvalError("invalid parameter list");
            }
            return new ParameterSpec(List.of(), symbolExpr.name());
        }
        throw new EvalError("lambda parameters must be a list or symbol");
    }

    private ParameterSpec parseParameterSpec(List<Expr> params) throws EvalError {
        List<String> requiredParameters = new ArrayList<>(params.size());
        String restParameter = null;

        for (int index = 0; index < params.size(); index++) {
            Expr param = params.get(index);
            if (param instanceof SymbolExpr symbolExpr && symbolExpr.name().equals(".")) {
                if (restParameter != null || index != params.size() - 2) {
                    throw new EvalError("invalid parameter list");
                }

                Expr restExpr = params.get(index + 1);
                if (!(restExpr instanceof SymbolExpr restSymbol) || restSymbol.name().equals(".")) {
                    throw new EvalError("rest parameter must be a symbol");
                }
                restParameter = restSymbol.name();
                index++;
                continue;
            }

            if (!(param instanceof SymbolExpr symbolExpr)) {
                throw new EvalError("parameter must be a symbol");
            }
            requiredParameters.add(symbolExpr.name());
        }

        return new ParameterSpec(requiredParameters, restParameter);
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

    private int quotient(List<Value> args) throws EvalError {
        requireArity("quotient", args.size(), 2);

        int dividend = expectInt(args.get(0));
        int divisor = expectInt(args.get(1));
        if (divisor == 0) {
            throw new EvalError("division by zero");
        }
        return dividend / divisor;
    }

    private int remainder(List<Value> args) throws EvalError {
        requireArity("remainder", args.size(), 2);

        int dividend = expectInt(args.get(0));
        int divisor = expectInt(args.get(1));
        if (divisor == 0) {
            throw new EvalError("division by zero");
        }
        return dividend % divisor;
    }

    private int modulo(List<Value> args) throws EvalError {
        requireArity("modulo", args.size(), 2);

        int dividend = expectInt(args.get(0));
        int divisor = expectInt(args.get(1));
        if (divisor == 0) {
            throw new EvalError("division by zero");
        }
        return Math.floorMod(dividend, divisor);
    }

    private int min(List<Value> args) throws EvalError {
        requireAtLeast("min", args.size(), 1);

        int result = expectInt(args.getFirst());
        for (int index = 1; index < args.size(); index++) {
            result = Math.min(result, expectInt(args.get(index)));
        }
        return result;
    }

    private int max(List<Value> args) throws EvalError {
        requireAtLeast("max", args.size(), 1);

        int result = expectInt(args.getFirst());
        for (int index = 1; index < args.size(); index++) {
            result = Math.max(result, expectInt(args.get(index)));
        }
        return result;
    }

    private int expt(List<Value> args) throws EvalError {
        requireArity("expt", args.size(), 2);

        int base = expectInt(args.get(0));
        int exponent = expectInt(args.get(1));
        if (exponent < 0) {
            throw new EvalError("expt exponent must be non-negative");
        }

        int result = 1;
        for (int index = 0; index < exponent; index++) {
            result *= base;
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

    private Value listRef(List<Value> args) throws EvalError {
        requireArity("list-ref", args.size(), 2);

        int index = expectIndex(args.get(1), "list-ref");
        Value current = args.getFirst();
        for (int remaining = index; remaining >= 0; remaining--) {
            if (!(current instanceof PairValue pairValue)) {
                throw new EvalError("list-ref index out of range");
            }
            if (remaining == 0) {
                return pairValue.car();
            }
            current = pairValue.cdr();
        }

        throw new EvalError("list-ref index out of range");
    }

    private Value listTailBuiltin(List<Value> args) throws EvalError {
        requireArity("list-tail", args.size(), 2);
        return listTail(args.getFirst(), expectIndex(args.get(1), "list-tail"));
    }

    private Value listTail(Value value, int index) throws EvalError {
        Value current = value;
        for (int remaining = index; remaining > 0; remaining--) {
            if (!(current instanceof PairValue pairValue)) {
                throw new EvalError("list-tail index out of range");
            }
            current = pairValue.cdr();
        }
        if (current instanceof PairValue || current instanceof EmptyListValue) {
            return current;
        }
        throw new EvalError("list-tail index out of range");
    }

    private boolean isProperList(Value value) {
        Value current = value;
        while (current instanceof PairValue pairValue) {
            current = pairValue.cdr();
        }
        return current instanceof EmptyListValue;
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

    private Value applyBuiltin(List<Value> args) throws EvalError {
        requireAtLeast("apply", args.size(), 2);

        List<Value> expandedArgs = new ArrayList<>();
        for (int index = 1; index < args.size() - 1; index++) {
            expandedArgs.add(args.get(index));
        }
        expandedArgs.addAll(listElements(args.get(args.size() - 1)));

        return applyProcedure(args.getFirst(), expandedArgs);
    }

    private Value mapBuiltin(List<Value> args) throws EvalError {
        requireAtLeast("map", args.size(), 2);

        Value procedure = args.getFirst();
        List<List<Value>> listArguments = new ArrayList<>(args.size() - 1);
        Integer expectedLength = null;

        for (int index = 1; index < args.size(); index++) {
            List<Value> elements = listElements(args.get(index));
            if (expectedLength == null) {
                expectedLength = elements.size();
            } else if (elements.size() != expectedLength) {
                throw new EvalError("map lists must have the same length");
            }
            listArguments.add(elements);
        }

        List<Value> results = new ArrayList<>(expectedLength == null ? 0 : expectedLength);
        for (int elementIndex = 0; elementIndex < expectedLength; elementIndex++) {
            List<Value> invocationArgs = new ArrayList<>(listArguments.size());
            for (List<Value> listArgument : listArguments) {
                invocationArgs.add(listArgument.get(elementIndex));
            }
            results.add(applyProcedure(procedure, invocationArgs));
        }
        return makeList(results);
    }

    private Value assocBuiltin(List<Value> args) throws EvalError {
        requireArity("assoc", args.size(), 2);

        Value key = args.get(0);
        Value current = args.get(1);
        while (current instanceof PairValue pairValue) {
            Value entry = pairValue.car();
            PairValue association = expectPair(entry);
            if (isEqual(key, association.car())) {
                return entry;
            }
            current = pairValue.cdr();
        }
        if (current instanceof EmptyListValue) {
            return BoolValue.FALSE;
        }
        throw new EvalError("expected list");
    }

    private String stringAppend(List<Value> args) throws EvalError {
        StringBuilder builder = new StringBuilder();
        for (Value arg : args) {
            builder.append(expectString(arg));
        }
        return builder.toString();
    }

    private boolean compareChars(List<Value> args, String name, CharComparison comparison)
            throws EvalError {
        requireAtLeast(name, args.size(), 2);

        char previous = expectChar(args.getFirst());
        for (int index = 1; index < args.size(); index++) {
            char current = expectChar(args.get(index));
            if (!comparison.matches(previous, current)) {
                return false;
            }
            previous = current;
        }
        return true;
    }

    private boolean compareStrings(List<Value> args, String name, StringComparison comparison)
            throws EvalError {
        requireAtLeast(name, args.size(), 2);

        String previous = expectString(args.getFirst());
        for (int index = 1; index < args.size(); index++) {
            String current = expectString(args.get(index));
            if (!comparison.matches(previous, current)) {
                return false;
            }
            previous = current;
        }
        return true;
    }

    private boolean isEq(Value left, Value right) {
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
        return false;
    }

    private boolean isEqual(Value left, Value right) {
        if (left == right) {
            return true;
        }
        if (left instanceof IntValue leftInt && right instanceof IntValue rightInt) {
            return leftInt.value() == rightInt.value();
        }
        if (left instanceof BoolValue leftBool && right instanceof BoolValue rightBool) {
            return leftBool.value() == rightBool.value();
        }
        if (left instanceof StringValue leftString && right instanceof StringValue rightString) {
            return leftString.value().equals(rightString.value());
        }
        if (left instanceof CharValue leftChar && right instanceof CharValue rightChar) {
            return leftChar.value() == rightChar.value();
        }
        if (left instanceof SymbolValue leftSymbol && right instanceof SymbolValue rightSymbol) {
            return leftSymbol.name().equals(rightSymbol.name());
        }
        if (left instanceof PairValue leftPair && right instanceof PairValue rightPair) {
            return isEqual(leftPair.car(), rightPair.car())
                    && isEqual(leftPair.cdr(), rightPair.cdr());
        }
        return false;
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
        private final ParameterSpec parameters;
        private final List<Expr> body;
        private final Environment closureEnv;

        private UserProcedure(String name, ParameterSpec parameters,
                              List<Expr> body, Environment closureEnv) {
            this.name = name;
            this.parameters = parameters;
            this.body = List.copyOf(body);
            this.closureEnv = closureEnv;
        }

        @Override
        Value apply(List<Value> args) throws EvalError {
            int requiredCount = parameters.requiredParameters().size();
            if (parameters.restParameter() == null) {
                requireArity(displayName(), args.size(), requiredCount);
            } else if (args.size() < requiredCount) {
                requireAtLeast(displayName(), args.size(), requiredCount);
            }

            Environment callEnv = new Environment(closureEnv);
            for (int index = 0; index < requiredCount; index++) {
                callEnv.define(parameters.requiredParameters().get(index), args.get(index));
            }
            if (parameters.restParameter() != null) {
                callEnv.define(parameters.restParameter(), makeList(args.subList(requiredCount,
                        args.size())));
            }
            return evalSequence(body, callEnv);
        }

        private String displayName() {
            return name == null ? "lambda" : name;
        }
    }

    private static final class Environment {
        private final Environment parent;
        private final Map<String, Cell> bindings = new HashMap<>();

        private Environment(Environment parent) {
            this.parent = parent;
        }

        private void define(String name, Value value) {
            bindings.put(name, new Cell(value));
        }

        private Value lookup(String name) throws EvalError {
            return lookupCell(name).value();
        }

        private void set(String name, Value value) throws EvalError {
            lookupCell(name).set(value);
        }

        private Cell lookupCell(String name) throws EvalError {
            Cell binding = bindings.get(name);
            if (binding != null) {
                return binding;
            }
            if (parent != null) {
                return parent.lookupCell(name);
            }
            throw new EvalError("unbound variable: " + name);
        }
    }

    private static final class Cell {
        private Value value;

        private Cell(Value value) {
            this.value = value;
        }

        private Value value() {
            return value;
        }

        private void set(Value value) {
            this.value = value;
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

    private enum CharComparison {
        EQUAL {
            @Override
            boolean matches(char left, char right) {
                return left == right;
            }
        },
        LESS {
            @Override
            boolean matches(char left, char right) {
                return left < right;
            }
        };

        abstract boolean matches(char left, char right);
    }

    private enum StringComparison {
        EQUAL {
            @Override
            boolean matches(String left, String right) {
                return left.equals(right);
            }
        },
        LESS {
            @Override
            boolean matches(String left, String right) {
                return left.compareTo(right) < 0;
            }
        };

        abstract boolean matches(String left, String right);
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

}
