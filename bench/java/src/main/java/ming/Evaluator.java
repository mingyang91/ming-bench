package ming;

import java.util.ArrayList;
import java.util.List;

/**
 * Scheme interpreter entry point.
 * Agents implement this class.
 */
public class Evaluator {
    private record Binding(String name, Expr valueExpression) {
    }

    private StringBuilder outputBuffer;

    /**
     * Evaluate one or more Scheme expressions and return the string
     * representation of the last result.
     */
    public String evalStr(String input) throws EvalError {
        return evalStrWithOutput(input).result();
    }

    /**
     * Evaluate Scheme expressions and return both the result string
     * and any captured output from display/write/newline.
     */
    public EvalResult evalStrWithOutput(String input) throws EvalError {
        StringBuilder previousOutputBuffer = outputBuffer;
        outputBuffer = new StringBuilder();
        try {
            List<Expr> expressions = new Parser(input).parseProgram();
            if (expressions.isEmpty()) {
                throw new EvalError("expected at least one expression");
            }

            Environment environment = createGlobalEnvironment();
            Value lastValue = VoidValue.INSTANCE;
            for (Expr expression : expressions) {
                lastValue = eval(expression, environment);
            }

            return new EvalResult(lastValue.render(), outputBuffer.toString());
        } finally {
            outputBuffer = previousOutputBuffer;
        }
    }

    Value evalSequence(List<Expr> expressions, Environment environment) throws EvalError {
        Value lastValue = VoidValue.INSTANCE;
        for (Expr expression : expressions) {
            lastValue = eval(expression, environment);
        }
        return lastValue;
    }

    private Environment createGlobalEnvironment() {
        Environment environment = new Environment(null);
        environment.define("+", new PrimitiveProcedureValue("+", this::applyAdd));
        environment.define("-", new PrimitiveProcedureValue("-", this::applySubtract));
        environment.define("*", new PrimitiveProcedureValue("*", this::applyMultiply));
        environment.define("/", new PrimitiveProcedureValue("/", this::applyDivide));
        environment.define("<", new PrimitiveProcedureValue("<", arguments -> applyComparison("<", arguments)));
        environment.define(">", new PrimitiveProcedureValue(">", arguments -> applyComparison(">", arguments)));
        environment.define("=", new PrimitiveProcedureValue("=", arguments -> applyComparison("=", arguments)));
        environment.define("<=", new PrimitiveProcedureValue("<=", arguments -> applyComparison("<=", arguments)));
        environment.define("not", new PrimitiveProcedureValue("not", this::applyNot));
        environment.define("cons", new PrimitiveProcedureValue("cons", this::applyCons));
        environment.define("car", new PrimitiveProcedureValue("car", this::applyCar));
        environment.define("cdr", new PrimitiveProcedureValue("cdr", this::applyCdr));
        environment.define("list", new PrimitiveProcedureValue("list", this::applyList));
        environment.define("append", new PrimitiveProcedureValue("append", this::applyAppend));
        environment.define("length", new PrimitiveProcedureValue("length", this::applyLength));
        environment.define("null?", new PrimitiveProcedureValue("null?", this::applyNullPredicate));
        environment.define("pair?", new PrimitiveProcedureValue("pair?", this::applyPairPredicate));
        environment.define("number?", new PrimitiveProcedureValue("number?", this::applyNumberPredicate));
        environment.define("string?", new PrimitiveProcedureValue("string?", this::applyStringPredicate));
        environment.define("boolean?", new PrimitiveProcedureValue("boolean?", this::applyBooleanPredicate));
        environment.define("symbol?", new PrimitiveProcedureValue("symbol?", this::applySymbolPredicate));
        environment.define("display", new PrimitiveProcedureValue("display", this::applyDisplay));
        environment.define("write", new PrimitiveProcedureValue("write", this::applyWrite));
        environment.define("newline", new PrimitiveProcedureValue("newline", this::applyNewline));
        environment.define("string-append", new PrimitiveProcedureValue("string-append", this::applyStringAppend));
        environment.define("string-length", new PrimitiveProcedureValue("string-length", this::applyStringLength));
        environment.define("substring", new PrimitiveProcedureValue("substring", this::applySubstring));
        environment.define("string->number", new PrimitiveProcedureValue("string->number", this::applyStringToNumber));
        environment.define("number->string", new PrimitiveProcedureValue("number->string", this::applyNumberToString));
        environment.define("symbol->string", new PrimitiveProcedureValue("symbol->string", this::applySymbolToString));
        environment.define("string->symbol", new PrimitiveProcedureValue("string->symbol", this::applyStringToSymbol));
        environment.define("string-ref", new PrimitiveProcedureValue("string-ref", this::applyStringRef));
        environment.define("char?", new PrimitiveProcedureValue("char?", this::applyCharPredicate));
        return environment;
    }

    private Value eval(Expr expression, Environment environment) throws EvalError {
        try {
            return switch (expression) {
                case IntExpr intExpr -> new IntValue(intExpr.value());
                case BoolExpr boolExpr -> new BoolValue(boolExpr.value());
                case StringExpr stringExpr -> new StringValue(stringExpr.value());
                case SymbolExpr symbolExpr -> environment.lookup(symbolExpr.name());
                case ListExpr listExpr -> evalList(listExpr, environment);
            };
        } catch (EvalError error) {
            throw error.withPosition(expression.line(), expression.column());
        }
    }

    private Value evalList(ListExpr listExpr, Environment environment) throws EvalError {
        List<Expr> elements = listExpr.elements();
        if (elements.isEmpty()) {
            throw new EvalError("cannot evaluate empty list");
        }

        Expr operatorExpression = elements.getFirst();
        List<Expr> arguments = elements.subList(1, elements.size());

        if (operatorExpression instanceof SymbolExpr symbolExpr) {
            return switch (symbolExpr.name()) {
                case "define" -> evalDefine(arguments, environment);
                case "if" -> evalIf(arguments, environment);
                case "quote" -> evalQuote(arguments);
                case "lambda" -> evalLambda(arguments, environment);
                case "begin" -> evalBegin(arguments, environment);
                case "cond" -> evalCond(arguments, environment);
                case "let" -> evalLet(arguments, environment);
                case "and" -> evalAnd(arguments, environment);
                case "or" -> evalOr(arguments, environment);
                default -> applyProcedure(operatorExpression, arguments, environment);
            };
        }

        return applyProcedure(operatorExpression, arguments, environment);
    }

    private Value applyProcedure(Expr operatorExpression,
                                 List<Expr> argumentExpressions,
                                 Environment environment) throws EvalError {
        Value operator = eval(operatorExpression, environment);
        if (!(operator instanceof ProcedureValue procedure)) {
            throw new EvalError("attempted to call non-procedure");
        }

        List<Value> arguments = new ArrayList<>(argumentExpressions.size());
        for (Expr argumentExpression : argumentExpressions) {
            arguments.add(eval(argumentExpression, environment));
        }
        return procedure.apply(List.copyOf(arguments), this);
    }

    private Value evalDefine(List<Expr> arguments, Environment environment) throws EvalError {
        if (arguments.isEmpty()) {
            throw new EvalError("define expected a binding target");
        }

        Expr target = arguments.getFirst();
        if (target instanceof SymbolExpr symbolExpr) {
            requireExactArity("define", arguments.size(), 2);
            Value value = eval(arguments.get(1), environment);
            environment.define(symbolExpr.name(), value);
            return VoidValue.INSTANCE;
        }

        if (target instanceof ListExpr signature) {
            List<Expr> signatureElements = signature.elements();
            if (signatureElements.isEmpty()) {
                throw new EvalError("define expected a function name");
            }

            Expr nameExpression = signatureElements.getFirst();
            if (!(nameExpression instanceof SymbolExpr functionName)) {
                throw new EvalError("define expected a function name");
            }

            if (arguments.size() < 2) {
                throw new EvalError("define expected a function body");
            }

            List<String> parameters = parseParameterNames(
                    signatureElements.subList(1, signatureElements.size()),
                    "define");
            List<Expr> body = arguments.subList(1, arguments.size());
            Value procedure = new LambdaProcedureValue(
                    functionName.name(),
                    parameters,
                    body,
                    environment);
            environment.define(functionName.name(), procedure);
            return VoidValue.INSTANCE;
        }

        throw new EvalError("define expected a symbol or function signature");
    }

    private Value evalIf(List<Expr> arguments, Environment environment) throws EvalError {
        requireExactArity("if", arguments.size(), 3);
        Value condition = eval(arguments.get(0), environment);
        Expr branch = condition.isTruthy() ? arguments.get(1) : arguments.get(2);
        return eval(branch, environment);
    }

    private Value evalQuote(List<Expr> arguments) throws EvalError {
        requireExactArity("quote", arguments.size(), 1);
        return quote(arguments.getFirst());
    }

    private Value evalLambda(List<Expr> arguments, Environment environment) throws EvalError {
        if (arguments.size() < 2) {
            throw new EvalError("lambda expected parameters and a body");
        }

        Expr parametersExpression = arguments.getFirst();
        if (!(parametersExpression instanceof ListExpr parametersList)) {
            throw new EvalError("lambda parameters must be a list");
        }

        List<String> parameters = parseParameterNames(parametersList.elements(), "lambda");
        List<Expr> body = arguments.subList(1, arguments.size());
        return new LambdaProcedureValue(null, parameters, body, environment);
    }

    private Value evalBegin(List<Expr> arguments, Environment environment) throws EvalError {
        return evalSequence(arguments, environment);
    }

    private Value evalCond(List<Expr> clauses, Environment environment) throws EvalError {
        for (int i = 0; i < clauses.size(); i++) {
            Expr clauseExpression = clauses.get(i);
            if (!(clauseExpression instanceof ListExpr clauseList)) {
                throw new EvalError("cond clauses must be lists");
            }

            List<Expr> clauseElements = clauseList.elements();
            if (clauseElements.isEmpty()) {
                throw new EvalError("cond clause cannot be empty");
            }

            Expr testExpression = clauseElements.getFirst();
            if (testExpression instanceof SymbolExpr symbolExpr && "else".equals(symbolExpr.name())) {
                if (i != clauses.size() - 1) {
                    throw new EvalError("cond else clause must be last");
                }
                if (clauseElements.size() == 1) {
                    throw new EvalError("cond else clause expected a body");
                }
                return evalSequence(clauseElements.subList(1, clauseElements.size()), environment);
            }

            Value testValue = eval(testExpression, environment);
            if (testValue.isTruthy()) {
                if (clauseElements.size() == 1) {
                    return testValue;
                }
                return evalSequence(clauseElements.subList(1, clauseElements.size()), environment);
            }
        }

        return VoidValue.INSTANCE;
    }

    private Value evalLet(List<Expr> arguments, Environment environment) throws EvalError {
        if (arguments.size() < 2) {
            throw new EvalError("let expected bindings and a body");
        }

        Expr firstArgument = arguments.getFirst();
        if (firstArgument instanceof SymbolExpr name) {
            return evalNamedLet(name.name(), arguments.subList(1, arguments.size()), environment);
        }

        List<Binding> bindings = parseBindings(firstArgument, "let");
        List<Expr> body = arguments.subList(1, arguments.size());
        Environment localEnvironment = new Environment(environment);
        for (Binding binding : bindings) {
            localEnvironment.define(binding.name(), eval(binding.valueExpression(), environment));
        }
        return evalSequence(body, localEnvironment);
    }

    private Value evalNamedLet(String name,
                               List<Expr> arguments,
                               Environment environment) throws EvalError {
        if (arguments.size() < 2) {
            throw new EvalError("let expected bindings and a body");
        }

        List<Binding> bindings = parseBindings(arguments.getFirst(), "let");
        List<String> parameters = new ArrayList<>(bindings.size());
        for (Binding binding : bindings) {
            parameters.add(binding.name());
        }

        List<Expr> body = arguments.subList(1, arguments.size());
        Environment localEnvironment = new Environment(environment);
        LambdaProcedureValue procedure = new LambdaProcedureValue(
                name,
                List.copyOf(parameters),
                body,
                localEnvironment);
        localEnvironment.define(name, procedure);

        List<Value> initialValues = new ArrayList<>(bindings.size());
        for (Binding binding : bindings) {
            initialValues.add(eval(binding.valueExpression(), localEnvironment));
        }

        return procedure.apply(List.copyOf(initialValues), this);
    }

    private List<Binding> parseBindings(Expr bindingsExpression, String formName) throws EvalError {
        if (!(bindingsExpression instanceof ListExpr bindingsList)) {
            throw new EvalError(formName + " bindings must be a list");
        }

        List<Binding> bindings = new ArrayList<>(bindingsList.elements().size());
        for (Expr bindingExpression : bindingsList.elements()) {
            if (!(bindingExpression instanceof ListExpr bindingList)) {
                throw new EvalError(formName + " bindings must be lists");
            }

            List<Expr> bindingElements = bindingList.elements();
            if (bindingElements.size() != 2) {
                throw new EvalError(formName + " bindings must have a name and value");
            }

            Expr nameExpression = bindingElements.getFirst();
            if (!(nameExpression instanceof SymbolExpr symbolExpr)) {
                throw new EvalError(formName + " bindings must start with a symbol");
            }

            bindings.add(new Binding(symbolExpr.name(), bindingElements.get(1)));
        }

        return List.copyOf(bindings);
    }

    private List<String> parseParameterNames(List<Expr> parameterExpressions, String formName)
            throws EvalError {
        List<String> parameterNames = new ArrayList<>(parameterExpressions.size());
        for (Expr parameterExpression : parameterExpressions) {
            if (!(parameterExpression instanceof SymbolExpr symbolExpr)) {
                throw new EvalError(formName + " parameters must be symbols");
            }
            parameterNames.add(symbolExpr.name());
        }
        return List.copyOf(parameterNames);
    }

    private Value quote(Expr expression) throws EvalError {
        return switch (expression) {
            case IntExpr intExpr -> new IntValue(intExpr.value());
            case BoolExpr boolExpr -> new BoolValue(boolExpr.value());
            case StringExpr stringExpr -> new StringValue(stringExpr.value());
            case SymbolExpr symbolExpr -> new SymbolValue(symbolExpr.name());
            case ListExpr listExpr -> {
                List<Value> elements = new ArrayList<>(listExpr.elements().size());
                for (Expr element : listExpr.elements()) {
                    elements.add(quote(element));
                }
                yield new ListValue(List.copyOf(elements));
            }
        };
    }

    private Value evalAnd(List<Expr> arguments, Environment environment) throws EvalError {
        Value last = new BoolValue(true);
        for (Expr argument : arguments) {
            last = eval(argument, environment);
            if (!last.isTruthy()) {
                return last;
            }
        }
        return last;
    }

    private Value evalOr(List<Expr> arguments, Environment environment) throws EvalError {
        Value last = new BoolValue(false);
        for (Expr argument : arguments) {
            last = eval(argument, environment);
            if (last.isTruthy()) {
                return last;
            }
        }
        return last;
    }

    private Value applyNot(List<Value> arguments) throws EvalError {
        requireExactArity("not", arguments.size(), 1);
        return new BoolValue(!arguments.getFirst().isTruthy());
    }

    private Value applyCons(List<Value> arguments) throws EvalError {
        requireExactArity("cons", arguments.size(), 2);
        List<Value> tail = expectList(arguments.get(1), "cons");
        List<Value> elements = new ArrayList<>(tail.size() + 1);
        elements.add(arguments.getFirst());
        elements.addAll(tail);
        return new ListValue(List.copyOf(elements));
    }

    private Value applyCar(List<Value> arguments) throws EvalError {
        requireExactArity("car", arguments.size(), 1);
        List<Value> elements = expectNonEmptyList(arguments.getFirst(), "car");
        return elements.getFirst();
    }

    private Value applyCdr(List<Value> arguments) throws EvalError {
        requireExactArity("cdr", arguments.size(), 1);
        List<Value> elements = expectNonEmptyList(arguments.getFirst(), "cdr");
        return new ListValue(List.copyOf(elements.subList(1, elements.size())));
    }

    private Value applyList(List<Value> arguments) {
        return new ListValue(List.copyOf(arguments));
    }

    private Value applyAppend(List<Value> arguments) throws EvalError {
        List<Value> appended = new ArrayList<>();
        for (Value argument : arguments) {
            appended.addAll(expectList(argument, "append"));
        }
        return new ListValue(List.copyOf(appended));
    }

    private Value applyLength(List<Value> arguments) throws EvalError {
        requireExactArity("length", arguments.size(), 1);
        return new IntValue(expectList(arguments.getFirst(), "length").size());
    }

    private Value applyNullPredicate(List<Value> arguments) throws EvalError {
        requireExactArity("null?", arguments.size(), 1);
        return new BoolValue(arguments.getFirst() instanceof ListValue listValue
                && listValue.elements().isEmpty());
    }

    private Value applyPairPredicate(List<Value> arguments) throws EvalError {
        requireExactArity("pair?", arguments.size(), 1);
        return new BoolValue(arguments.getFirst() instanceof ListValue listValue
                && !listValue.elements().isEmpty());
    }

    private Value applyNumberPredicate(List<Value> arguments) throws EvalError {
        requireExactArity("number?", arguments.size(), 1);
        return new BoolValue(arguments.getFirst() instanceof IntValue);
    }

    private Value applyStringPredicate(List<Value> arguments) throws EvalError {
        requireExactArity("string?", arguments.size(), 1);
        return new BoolValue(arguments.getFirst() instanceof StringValue);
    }

    private Value applyBooleanPredicate(List<Value> arguments) throws EvalError {
        requireExactArity("boolean?", arguments.size(), 1);
        return new BoolValue(arguments.getFirst() instanceof BoolValue);
    }

    private Value applySymbolPredicate(List<Value> arguments) throws EvalError {
        requireExactArity("symbol?", arguments.size(), 1);
        return new BoolValue(arguments.getFirst() instanceof SymbolValue);
    }

    private Value applyDisplay(List<Value> arguments) throws EvalError {
        requireExactArity("display", arguments.size(), 1);
        appendOutput(renderForDisplay(arguments.getFirst()));
        return VoidValue.INSTANCE;
    }

    private Value applyWrite(List<Value> arguments) throws EvalError {
        requireExactArity("write", arguments.size(), 1);
        appendOutput(arguments.getFirst().render());
        return VoidValue.INSTANCE;
    }

    private Value applyNewline(List<Value> arguments) throws EvalError {
        requireExactArity("newline", arguments.size(), 0);
        appendOutput("\n");
        return VoidValue.INSTANCE;
    }

    private Value applyStringAppend(List<Value> arguments) throws EvalError {
        StringBuilder builder = new StringBuilder();
        for (Value argument : arguments) {
            builder.append(expectString(argument, "string-append"));
        }
        return new StringValue(builder.toString());
    }

    private Value applyStringLength(List<Value> arguments) throws EvalError {
        requireExactArity("string-length", arguments.size(), 1);
        return new IntValue(expectString(arguments.getFirst(), "string-length").length());
    }

    private Value applySubstring(List<Value> arguments) throws EvalError {
        requireExactArity("substring", arguments.size(), 3);
        String value = expectString(arguments.getFirst(), "substring");
        int start = expectIndex(arguments.get(1), "substring");
        int end = expectIndex(arguments.get(2), "substring");
        if (start > end || end > value.length()) {
            throw new EvalError("substring indices out of range");
        }
        return new StringValue(value.substring(start, end));
    }

    private Value applyStringToNumber(List<Value> arguments) throws EvalError {
        requireExactArity("string->number", arguments.size(), 1);
        String value = expectString(arguments.getFirst(), "string->number");
        try {
            return new IntValue(Long.parseLong(value));
        } catch (NumberFormatException error) {
            return new BoolValue(false);
        }
    }

    private Value applyNumberToString(List<Value> arguments) throws EvalError {
        requireExactArity("number->string", arguments.size(), 1);
        return new StringValue(Long.toString(expectInt(arguments.getFirst(), "number->string")));
    }

    private Value applySymbolToString(List<Value> arguments) throws EvalError {
        requireExactArity("symbol->string", arguments.size(), 1);
        if (arguments.getFirst() instanceof SymbolValue symbolValue) {
            return new StringValue(symbolValue.name());
        }
        throw new EvalError("symbol->string expects symbol arguments");
    }

    private Value applyStringToSymbol(List<Value> arguments) throws EvalError {
        requireExactArity("string->symbol", arguments.size(), 1);
        return new SymbolValue(expectString(arguments.getFirst(), "string->symbol"));
    }

    private Value applyStringRef(List<Value> arguments) throws EvalError {
        requireExactArity("string-ref", arguments.size(), 2);
        String value = expectString(arguments.getFirst(), "string-ref");
        int index = expectIndex(arguments.get(1), "string-ref");
        if (index >= value.length()) {
            throw new EvalError("string-ref index out of range");
        }
        return new CharValue(value.charAt(index));
    }

    private Value applyCharPredicate(List<Value> arguments) throws EvalError {
        requireExactArity("char?", arguments.size(), 1);
        return new BoolValue(arguments.getFirst() instanceof CharValue);
    }

    private Value applyAdd(List<Value> arguments) throws EvalError {
        long total = 0L;
        for (Value argument : arguments) {
            total += expectInt(argument, "+");
        }
        return new IntValue(total);
    }

    private Value applySubtract(List<Value> arguments) throws EvalError {
        requireMinimumArity("-", arguments.size(), 1);
        long result = expectInt(arguments.getFirst(), "-");
        if (arguments.size() == 1) {
            return new IntValue(-result);
        }

        for (int i = 1; i < arguments.size(); i++) {
            result -= expectInt(arguments.get(i), "-");
        }
        return new IntValue(result);
    }

    private Value applyMultiply(List<Value> arguments) throws EvalError {
        long product = 1L;
        for (Value argument : arguments) {
            product *= expectInt(argument, "*");
        }
        return new IntValue(product);
    }

    private Value applyDivide(List<Value> arguments) throws EvalError {
        requireMinimumArity("/", arguments.size(), 2);
        long result = expectInt(arguments.getFirst(), "/");
        for (int i = 1; i < arguments.size(); i++) {
            long divisor = expectInt(arguments.get(i), "/");
            if (divisor == 0L) {
                throw new EvalError("division by zero");
            }
            result /= divisor;
        }
        return new IntValue(result);
    }

    private Value applyComparison(String operator, List<Value> arguments) throws EvalError {
        requireMinimumArity(operator, arguments.size(), 2);

        long previous = expectInt(arguments.getFirst(), operator);
        for (int i = 1; i < arguments.size(); i++) {
            long current = expectInt(arguments.get(i), operator);
            if (!compare(operator, previous, current)) {
                return new BoolValue(false);
            }
            previous = current;
        }

        return new BoolValue(true);
    }

    private boolean compare(String operator, long left, long right) {
        return switch (operator) {
            case "<" -> left < right;
            case ">" -> left > right;
            case "=" -> left == right;
            case "<=" -> left <= right;
            default -> false;
        };
    }

    private long expectInt(Value value, String operator) throws EvalError {
        if (value instanceof IntValue intValue) {
            return intValue.value();
        }
        throw new EvalError(operator + " expects integer arguments");
    }

    private int expectIndex(Value value, String operator) throws EvalError {
        long index = expectInt(value, operator);
        if (index < 0L || index > Integer.MAX_VALUE) {
            throw new EvalError(operator + " index out of range");
        }
        return (int) index;
    }

    private String expectString(Value value, String operator) throws EvalError {
        if (value instanceof StringValue stringValue) {
            return stringValue.value();
        }
        throw new EvalError(operator + " expects string arguments");
    }

    private List<Value> expectList(Value value, String operator) throws EvalError {
        if (value instanceof ListValue listValue) {
            return listValue.elements();
        }
        throw new EvalError(operator + " expects list arguments");
    }

    private void appendOutput(String output) {
        if (outputBuffer != null) {
            outputBuffer.append(output);
        }
    }

    private String renderForDisplay(Value value) {
        return switch (value) {
            case StringValue stringValue -> stringValue.value();
            case CharValue charValue -> Character.toString(charValue.value());
            case ListValue listValue -> renderListForDisplay(listValue.elements());
            default -> value.render();
        };
    }

    private String renderListForDisplay(List<Value> elements) {
        StringBuilder builder = new StringBuilder();
        builder.append('(');
        for (int i = 0; i < elements.size(); i++) {
            if (i > 0) {
                builder.append(' ');
            }
            builder.append(renderForDisplay(elements.get(i)));
        }
        builder.append(')');
        return builder.toString();
    }

    private List<Value> expectNonEmptyList(Value value, String operator) throws EvalError {
        List<Value> elements = expectList(value, operator);
        if (elements.isEmpty()) {
            throw new EvalError(operator + " expected a non-empty list");
        }
        return elements;
    }

    private void requireExactArity(String name, int actual, int expected) throws EvalError {
        if (actual != expected) {
            throw new EvalError(name + " expected " + expected + " argument(s)");
        }
    }

    private void requireMinimumArity(String name, int actual, int minimum) throws EvalError {
        if (actual < minimum) {
            throw new EvalError(name + " expected at least " + minimum + " argument(s)");
        }
    }
}
