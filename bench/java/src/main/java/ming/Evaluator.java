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
        return format(evalProgram(input).value());
    }

    /**
     * Evaluate Scheme expressions and return both the result string
     * and any captured output from display/write/newline.
     */
    public EvalResult evalStrWithOutput(String input) throws EvalError {
        ProgramResult result = evalProgram(input);
        return new EvalResult(format(result.value()), result.output());
    }

    private ProgramResult evalProgram(String input) throws EvalError {
        Parser parser = new Parser(input);
        List<Expr> expressions = parser.parseProgram();
        if (expressions.isEmpty()) {
            throw new EvalError("empty input", 1, 1);
        }

        StringBuilder output = new StringBuilder();
        Environment environment = createGlobalEnvironment(output);
        Value result = VoidValue.INSTANCE;
        for (Expr expression : expressions) {
            result = eval(expression, environment);
        }
        return new ProgramResult(result, output.toString());
    }

    private Environment createGlobalEnvironment(StringBuilder output) {
        Environment environment = new Environment(null);
        environment.define("+", new BuiltinProcedure("+", this::applyAdd));
        environment.define("-", new BuiltinProcedure("-", this::applySubtract));
        environment.define("*", new BuiltinProcedure("*", this::applyMultiply));
        environment.define("/", new BuiltinProcedure("/", this::applyDivide));
        environment.define("cons", new BuiltinProcedure("cons", this::applyCons));
        environment.define("car", new BuiltinProcedure("car", this::applyCar));
        environment.define("cdr", new BuiltinProcedure("cdr", this::applyCdr));
        environment.define("null?", new BuiltinProcedure("null?", (args, pos) ->
                BoolValue.of(isNull(args, pos))));
        environment.define("list", new BuiltinProcedure("list", (args, pos) -> buildList(args)));
        environment.define("length", new BuiltinProcedure("length", (args, pos) ->
                new IntValue(BigInteger.valueOf(length(args, pos)))));
        environment.define("append", new BuiltinProcedure("append", this::applyAppend));
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
        environment.define("string?", new BuiltinProcedure("string?", (args, pos) ->
                BoolValue.of(isType(args, pos, StringValue.class))));
        environment.define("number?", new BuiltinProcedure("number?", (args, pos) ->
                BoolValue.of(isType(args, pos, IntValue.class))));
        environment.define("boolean?", new BuiltinProcedure("boolean?", (args, pos) ->
                BoolValue.of(isType(args, pos, BoolValue.class))));
        environment.define("pair?", new BuiltinProcedure("pair?", (args, pos) ->
                BoolValue.of(isType(args, pos, PairValue.class))));
        environment.define("symbol?", new BuiltinProcedure("symbol?", (args, pos) ->
                BoolValue.of(isType(args, pos, SymbolValue.class))));
        environment.define("display", new BuiltinProcedure("display", (args, pos) ->
                applyDisplay(args, pos, output)));
        environment.define("write", new BuiltinProcedure("write", (args, pos) ->
                applyWrite(args, pos, output)));
        environment.define("newline", new BuiltinProcedure("newline", (args, pos) ->
                applyNewline(args, pos, output)));
        environment.define("string-append", new BuiltinProcedure("string-append",
                this::applyStringAppend));
        environment.define("string-length", new BuiltinProcedure("string-length",
                this::applyStringLength));
        environment.define("substring", new BuiltinProcedure("substring", this::applySubstring));
        environment.define("string->number", new BuiltinProcedure("string->number",
                this::applyStringToNumber));
        environment.define("number->string", new BuiltinProcedure("number->string",
                this::applyNumberToString));
        environment.define("symbol->string", new BuiltinProcedure("symbol->string",
                this::applySymbolToString));
        environment.define("string->symbol", new BuiltinProcedure("string->symbol",
                this::applyStringToSymbol));
        environment.define("string-ref", new BuiltinProcedure("string-ref", this::applyStringRef));
        environment.define("string-set!", new BuiltinProcedure("string-set!", this::applyStringSet));
        environment.define("string-copy", new BuiltinProcedure("string-copy", this::applyStringCopy));
        environment.define("char?", new BuiltinProcedure("char?", (args, pos) ->
                BoolValue.of(isType(args, pos, CharValue.class))));
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
        if (expression instanceof CharExpr charExpr) {
            return new CharValue(charExpr.value());
        }
        if (expression instanceof SymbolExpr symbolExpr) {
            return environment.lookup(symbolExpr.name(), symbolExpr.pos());
        }
        if (expression instanceof ListExpr listExpr) {
            return evalList(listExpr, environment);
        }
        throw new EvalError("unsupported expression", expression.pos().line(),
                expression.pos().column());
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
                case "begin" -> evalBegin(arguments, environment);
                case "cond" -> evalCond(arguments, environment, symbolExpr.pos());
                case "let" -> evalLet(arguments, environment, symbolExpr.pos());
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

    private Value evalBegin(List<Expr> arguments, Environment environment) throws EvalError {
        return evalSequence(arguments, environment);
    }

    private Value evalCond(List<Expr> arguments, Environment environment, SourcePos pos)
            throws EvalError {
        for (int i = 0; i < arguments.size(); i++) {
            Expr clauseExpr = arguments.get(i);
            if (!(clauseExpr instanceof ListExpr clause) || clause.elements().isEmpty()) {
                throw error("'cond' clauses must be non-empty lists", pos);
            }

            List<Expr> clauseElements = clause.elements();
            Expr testExpr = clauseElements.getFirst();
            if (testExpr instanceof SymbolExpr symbolExpr && "else".equals(symbolExpr.name())) {
                if (i != arguments.size() - 1) {
                    throw error("'cond' else clause must be last", symbolExpr.pos());
                }
                return evalSequence(clauseElements.subList(1, clauseElements.size()), environment);
            }

            Value testValue = eval(testExpr, environment);
            if (isTruthy(testValue)) {
                if (clauseElements.size() == 1) {
                    return testValue;
                }
                return evalSequence(clauseElements.subList(1, clauseElements.size()), environment);
            }
        }

        return VoidValue.INSTANCE;
    }

    private Value evalLet(List<Expr> arguments, Environment environment, SourcePos pos)
            throws EvalError {
        if (arguments.size() < 2) {
            throw error("'let' expects bindings and a body", pos);
        }

        Expr head = arguments.getFirst();
        if (head instanceof SymbolExpr nameExpr) {
            return evalNamedLet(nameExpr, arguments.subList(1, arguments.size()), environment, pos);
        }
        if (head instanceof ListExpr bindingsExpr) {
            return evalSimpleLet(bindingsExpr, arguments.subList(1, arguments.size()), environment,
                    pos);
        }

        throw error("'let' expects a binding list", head.pos());
    }

    private Value evalSimpleLet(ListExpr bindingsExpr, List<Expr> body, Environment environment,
                                SourcePos pos) throws EvalError {
        if (body.isEmpty()) {
            throw error("'let' expects a body", pos);
        }

        List<LetBinding> bindings = parseBindings(bindingsExpr, pos);
        List<Value> values = evalBindingValues(bindings, environment);
        Environment letEnvironment = new Environment(environment);
        for (int i = 0; i < bindings.size(); i++) {
            letEnvironment.define(bindings.get(i).name(), values.get(i));
        }
        return evalSequence(body, letEnvironment);
    }

    private Value evalNamedLet(SymbolExpr nameExpr, List<Expr> arguments, Environment environment,
                               SourcePos pos) throws EvalError {
        if (arguments.isEmpty() || !(arguments.getFirst() instanceof ListExpr bindingsExpr)) {
            throw error("named 'let' expects a binding list", pos);
        }
        List<Expr> body = arguments.subList(1, arguments.size());
        if (body.isEmpty()) {
            throw error("named 'let' expects a body", pos);
        }

        List<LetBinding> bindings = parseBindings(bindingsExpr, pos);
        List<Value> values = evalBindingValues(bindings, environment);
        List<String> parameters = new ArrayList<>(bindings.size());
        for (LetBinding binding : bindings) {
            parameters.add(binding.name());
        }

        Environment closureEnvironment = new Environment(environment);
        ClosureProcedure procedure = new ClosureProcedure(nameExpr.name(), parameters,
                new ArrayList<>(body), closureEnvironment);
        closureEnvironment.define(nameExpr.name(), procedure);
        return apply(procedure, values, pos);
    }

    private List<LetBinding> parseBindings(ListExpr bindingsExpr, SourcePos pos) throws EvalError {
        List<LetBinding> bindings = new ArrayList<>(bindingsExpr.elements().size());
        for (Expr bindingExpr : bindingsExpr.elements()) {
            if (!(bindingExpr instanceof ListExpr bindingList)) {
                throw error("'let' bindings must be lists", pos);
            }
            List<Expr> bindingElements = bindingList.elements();
            if (bindingElements.size() != 2) {
                throw error("'let' bindings must contain a name and a value", bindingList.pos());
            }
            if (!(bindingElements.getFirst() instanceof SymbolExpr symbolExpr)) {
                throw error("'let' binding names must be symbols", bindingList.pos());
            }
            bindings.add(new LetBinding(symbolExpr.name(), bindingElements.get(1)));
        }
        return bindings;
    }

    private List<Value> evalBindingValues(List<LetBinding> bindings, Environment environment)
            throws EvalError {
        List<Value> values = new ArrayList<>(bindings.size());
        for (LetBinding binding : bindings) {
            values.add(eval(binding.initializer(), environment));
        }
        return values;
    }

    private Value evalSequence(List<Expr> expressions, Environment environment) throws EvalError {
        Value result = VoidValue.INSTANCE;
        for (Expr expression : expressions) {
            result = eval(expression, environment);
        }
        return result;
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

            return evalSequence(closureProcedure.body(), callEnvironment);
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

    private Value applyCons(List<Value> arguments, SourcePos pos) throws EvalError {
        requireArgCount(arguments, 2, "cons", pos);
        return new PairValue(arguments.get(0), arguments.get(1));
    }

    private Value applyCar(List<Value> arguments, SourcePos pos) throws EvalError {
        requireArgCount(arguments, 1, "car", pos);
        return asPair(arguments.getFirst(), "car", pos).car();
    }

    private Value applyCdr(List<Value> arguments, SourcePos pos) throws EvalError {
        requireArgCount(arguments, 1, "cdr", pos);
        return asPair(arguments.getFirst(), "cdr", pos).cdr();
    }

    private Value applyAppend(List<Value> arguments, SourcePos pos) throws EvalError {
        if (arguments.isEmpty()) {
            return EmptyListValue.INSTANCE;
        }

        Value result = arguments.get(arguments.size() - 1);
        for (int i = arguments.size() - 2; i >= 0; i--) {
            result = copyListOnto(arguments.get(i), result, pos);
        }
        return result;
    }

    private Value applyDisplay(List<Value> arguments, SourcePos pos, StringBuilder output)
            throws EvalError {
        requireArgCount(arguments, 1, "display", pos);
        output.append(formatDisplay(arguments.getFirst()));
        return VoidValue.INSTANCE;
    }

    private Value applyWrite(List<Value> arguments, SourcePos pos, StringBuilder output)
            throws EvalError {
        requireArgCount(arguments, 1, "write", pos);
        output.append(format(arguments.getFirst()));
        return VoidValue.INSTANCE;
    }

    private Value applyNewline(List<Value> arguments, SourcePos pos, StringBuilder output)
            throws EvalError {
        requireArgCount(arguments, 0, "newline", pos);
        output.append('\n');
        return VoidValue.INSTANCE;
    }

    private Value applyStringAppend(List<Value> arguments, SourcePos pos) throws EvalError {
        StringBuilder builder = new StringBuilder();
        for (Value argument : arguments) {
            builder.append(asString(argument, "string-append", pos));
        }
        return new StringValue(builder.toString());
    }

    private Value applyStringLength(List<Value> arguments, SourcePos pos) throws EvalError {
        requireArgCount(arguments, 1, "string-length", pos);
        return new IntValue(BigInteger.valueOf(
                asString(arguments.getFirst(), "string-length", pos).length()));
    }

    private Value applySubstring(List<Value> arguments, SourcePos pos) throws EvalError {
        requireArgCount(arguments, 3, "substring", pos);
        String value = asString(arguments.get(0), "substring", pos);
        int start = asIndex(arguments.get(1), "substring", pos);
        int end = asIndex(arguments.get(2), "substring", pos);
        if (start > end || end > value.length()) {
            throw error("'substring' index out of range", pos);
        }
        return new StringValue(value.substring(start, end));
    }

    private Value applyStringToNumber(List<Value> arguments, SourcePos pos) throws EvalError {
        requireArgCount(arguments, 1, "string->number", pos);
        String value = asString(arguments.getFirst(), "string->number", pos);
        try {
            return new IntValue(new BigInteger(value));
        } catch (NumberFormatException ignored) {
            return BoolValue.FALSE;
        }
    }

    private Value applyNumberToString(List<Value> arguments, SourcePos pos) throws EvalError {
        requireArgCount(arguments, 1, "number->string", pos);
        return new StringValue(asNumber(arguments.getFirst(), "number->string", pos).toString());
    }

    private Value applySymbolToString(List<Value> arguments, SourcePos pos) throws EvalError {
        requireArgCount(arguments, 1, "symbol->string", pos);
        return new StringValue(asSymbol(arguments.getFirst(), "symbol->string", pos));
    }

    private Value applyStringToSymbol(List<Value> arguments, SourcePos pos) throws EvalError {
        requireArgCount(arguments, 1, "string->symbol", pos);
        return new SymbolValue(asString(arguments.getFirst(), "string->symbol", pos));
    }

    private Value applyStringRef(List<Value> arguments, SourcePos pos) throws EvalError {
        requireArgCount(arguments, 2, "string-ref", pos);
        StringValue value = asStringValue(arguments.get(0), "string-ref", pos);
        int index = asIndex(arguments.get(1), "string-ref", pos);
        if (index >= value.length()) {
            throw error("'string-ref' index out of range", pos);
        }
        return new CharValue(value.charAt(index));
    }

    private Value applyStringSet(List<Value> arguments, SourcePos pos) throws EvalError {
        requireArgCount(arguments, 3, "string-set!", pos);
        StringValue value = asStringValue(arguments.get(0), "string-set!", pos);
        int index = asIndex(arguments.get(1), "string-set!", pos);
        if (index >= value.length()) {
            throw error("'string-set!' index out of range", pos);
        }
        if (!value.mutable()) {
            throw error("'string-set!' cannot modify an immutable string", pos);
        }
        value.setCharAt(index, asChar(arguments.get(2), "string-set!", pos));
        return VoidValue.INSTANCE;
    }

    private Value applyStringCopy(List<Value> arguments, SourcePos pos) throws EvalError {
        requireArgCount(arguments, 1, "string-copy", pos);
        return asStringValue(arguments.getFirst(), "string-copy", pos).copy(true);
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
        requireArgCount(arguments, 1, "not", pos);
        return !isTruthy(arguments.getFirst());
    }

    private boolean isNull(List<Value> arguments, SourcePos pos) throws EvalError {
        requireArgCount(arguments, 1, "null?", pos);
        return arguments.getFirst() instanceof EmptyListValue;
    }

    private long length(List<Value> arguments, SourcePos pos) throws EvalError {
        requireArgCount(arguments, 1, "length", pos);
        return listLength(arguments.getFirst(), "length", pos);
    }

    private boolean isType(List<Value> arguments, SourcePos pos,
                           Class<? extends Value> expectedType) throws EvalError {
        requireArgCount(arguments, 1, expectedType.getSimpleName(), pos);
        return expectedType.isInstance(arguments.getFirst());
    }

    private void requireArgCount(List<Value> arguments, int expected, String name, SourcePos pos)
            throws EvalError {
        if (arguments.size() != expected) {
            throw error("'" + name + "' expects exactly " + expected + " argument"
                    + (expected == 1 ? "" : "s"), pos);
        }
    }

    private BigInteger asNumber(Value value, String operator, SourcePos pos) throws EvalError {
        if (value instanceof IntValue intValue) {
            return intValue.value();
        }
        throw error("'" + operator + "' expects numeric arguments", pos);
    }

    private PairValue asPair(Value value, String operator, SourcePos pos) throws EvalError {
        if (value instanceof PairValue pairValue) {
            return pairValue;
        }
        throw error("'" + operator + "' expects a pair", pos);
    }

    private String asString(Value value, String operator, SourcePos pos) throws EvalError {
        if (value instanceof StringValue stringValue) {
            return stringValue.value();
        }
        throw error("'" + operator + "' expects string arguments", pos);
    }

    private StringValue asStringValue(Value value, String operator, SourcePos pos)
            throws EvalError {
        if (value instanceof StringValue stringValue) {
            return stringValue;
        }
        throw error("'" + operator + "' expects string arguments", pos);
    }

    private String asSymbol(Value value, String operator, SourcePos pos) throws EvalError {
        if (value instanceof SymbolValue symbolValue) {
            return symbolValue.name();
        }
        throw error("'" + operator + "' expects a symbol", pos);
    }

    private char asChar(Value value, String operator, SourcePos pos) throws EvalError {
        if (value instanceof CharValue charValue) {
            return charValue.value();
        }
        throw error("'" + operator + "' expects a character", pos);
    }

    private int asIndex(Value value, String operator, SourcePos pos) throws EvalError {
        BigInteger index = asNumber(value, operator, pos);
        if (index.signum() < 0 || index.compareTo(BigInteger.valueOf(Integer.MAX_VALUE)) > 0) {
            throw error("'" + operator + "' expects a non-negative integer index", pos);
        }
        return index.intValue();
    }

    private Value buildList(List<Value> values) {
        Value result = EmptyListValue.INSTANCE;
        for (int i = values.size() - 1; i >= 0; i--) {
            result = new PairValue(values.get(i), result);
        }
        return result;
    }

    private long listLength(Value value, String operator, SourcePos pos) throws EvalError {
        long length = 0;
        Value current = value;
        while (current instanceof PairValue pairValue) {
            length++;
            current = pairValue.cdr();
        }
        if (current instanceof EmptyListValue) {
            return length;
        }
        throw error("'" + operator + "' expects a proper list", pos);
    }

    private Value copyListOnto(Value list, Value tail, SourcePos pos) throws EvalError {
        List<Value> prefix = new ArrayList<>();
        Value current = list;
        while (current instanceof PairValue pairValue) {
            prefix.add(pairValue.car());
            current = pairValue.cdr();
        }
        if (!(current instanceof EmptyListValue)) {
            throw error("'append' expects list arguments", pos);
        }

        Value result = tail;
        for (int i = prefix.size() - 1; i >= 0; i--) {
            result = new PairValue(prefix.get(i), result);
        }
        return result;
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
        if (expression instanceof CharExpr charExpr) {
            return new CharValue(charExpr.value());
        }
        if (expression instanceof SymbolExpr symbolExpr) {
            return new SymbolValue(symbolExpr.name());
        }
        if (expression instanceof ListExpr listExpr) {
            return quoteList(listExpr.elements());
        }
        throw new EvalError("unsupported quoted expression", expression.pos().line(),
                expression.pos().column());
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
        return format(value, false);
    }

    private String formatDisplay(Value value) {
        return format(value, true);
    }

    private String format(Value value, boolean displayMode) {
        if (value instanceof IntValue intValue) {
            return intValue.value().toString();
        }
        if (value instanceof BoolValue boolValue) {
            return boolValue.value() ? "#t" : "#f";
        }
        if (value instanceof StringValue stringValue) {
            if (displayMode) {
                return stringValue.value();
            }
            return "\"" + escapeString(stringValue.value()) + "\"";
        }
        if (value instanceof CharValue charValue) {
            if (displayMode) {
                return Character.toString(charValue.value());
            }
            return formatCharacter(charValue.value());
        }
        if (value instanceof SymbolValue symbolValue) {
            return symbolValue.name();
        }
        if (value instanceof EmptyListValue) {
            return "()";
        }
        if (value instanceof PairValue pairValue) {
            return formatPair(pairValue, displayMode);
        }
        if (value instanceof VoidValue) {
            return "#<void>";
        }
        if (value instanceof ProcedureValue) {
            return "#<procedure>";
        }
        throw new IllegalStateException("unsupported runtime value");
    }

    private String formatPair(PairValue pairValue, boolean displayMode) {
        StringBuilder builder = new StringBuilder("(");
        Value current = pairValue;
        boolean first = true;
        while (current instanceof PairValue pair) {
            if (!first) {
                builder.append(' ');
            }
            builder.append(format(pair.car(), displayMode));
            current = pair.cdr();
            first = false;
        }

        if (current instanceof EmptyListValue) {
            builder.append(')');
        } else {
            builder.append(" . ").append(format(current, displayMode)).append(')');
        }
        return builder.toString();
    }

    private String formatCharacter(char value) {
        return switch (value) {
            case ' ' -> "#\\space";
            case '\n' -> "#\\newline";
            default -> "#\\" + value;
        };
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
        return new EvalError(message, pos.line(), pos.column());
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

    private sealed interface Expr permits IntExpr, BoolExpr, StringExpr, CharExpr, SymbolExpr,
            ListExpr {
        SourcePos pos();
    }

    private sealed interface Value permits IntValue, BoolValue, StringValue, CharValue,
            SymbolValue, EmptyListValue, PairValue, ProcedureValue, VoidValue {
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

    private record CharExpr(char value, SourcePos pos) implements Expr {
    }

    private record SymbolExpr(String name, SourcePos pos) implements Expr {
    }

    private record ListExpr(List<Expr> elements, SourcePos pos) implements Expr {
    }

    private record LetBinding(String name, Expr initializer) {
    }

    private record ProgramResult(Value value, String output) {
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

    private static final class StringValue implements Value {
        private final StringBuilder value;
        private final boolean mutable;

        private StringValue(String value) {
            this(value, true);
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

        private boolean mutable() {
            return mutable;
        }

        private void setCharAt(int index, char updatedValue) {
            value.setCharAt(index, updatedValue);
        }

        private StringValue copy(boolean mutable) {
            return new StringValue(value(), mutable);
        }
    }

    private record CharValue(char value) implements Value {
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
            throw new EvalError("unbound variable: " + name, pos.line(), pos.column());
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
            if (current == '\'') {
                return parseQuote();
            }
            if (current == '"') {
                return parseString();
            }
            if (current == '#') {
                return parseHashLiteral();
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

        private Expr parseQuote() throws EvalError {
            SourcePos pos = currentPos();
            advance();

            List<Expr> elements = new ArrayList<>(2);
            elements.add(new SymbolExpr("quote", pos));
            elements.add(parseExpr());
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

        private Expr parseHashLiteral() throws EvalError {
            SourcePos pos = currentPos();
            advance();
            if (isAtEnd()) {
                throw error("incomplete hash literal", pos);
            }

            char value = advance();
            return switch (value) {
                case 't' -> new BoolExpr(true, pos);
                case 'f' -> new BoolExpr(false, pos);
                case '\\' -> parseCharacterLiteral(pos);
                default -> throw error("unknown hash literal '#" + value + "'", pos);
            };
        }

        private Expr parseCharacterLiteral(SourcePos pos) throws EvalError {
            if (isAtEnd()) {
                throw error("incomplete character literal", pos);
            }

            int start = index;
            while (!isAtEnd() && !isDelimiter(peek())) {
                advance();
            }

            String token = input.substring(start, index);
            if (token.isEmpty()) {
                throw error("incomplete character literal", pos);
            }
            return switch (token) {
                case "space" -> new CharExpr(' ', pos);
                case "newline" -> new CharExpr('\n', pos);
                default -> {
                    if (token.length() != 1) {
                        throw error("unknown character literal '#\\"
                                + token + "'", pos);
                    }
                    yield new CharExpr(token.charAt(0), pos);
                }
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
            return new EvalError(message, pos.line(), pos.column());
        }
    }
}
