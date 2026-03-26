package ming;

import java.util.ArrayList;
import java.util.List;

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
        return evalStrWithOutput(input).result();
    }

    /**
     * Evaluate Scheme expressions and return both the result string
     * and any captured output from display/write/newline.
     */
    public EvalResult evalStrWithOutput(String input) throws EvalError {
        List<Expr> expressions = new Parser(input).parseProgram();
        if (expressions.isEmpty()) {
            throw new EvalError("expected at least one expression");
        }

        Environment environment = createGlobalEnvironment();
        Value lastValue = VoidValue.INSTANCE;
        for (Expr expression : expressions) {
            lastValue = eval(expression, environment);
        }

        return new EvalResult(lastValue.render(), "");
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
        return environment;
    }

    private Value eval(Expr expression, Environment environment) throws EvalError {
        return switch (expression) {
            case IntExpr intExpr -> new IntValue(intExpr.value());
            case BoolExpr boolExpr -> new BoolValue(boolExpr.value());
            case StringExpr stringExpr -> new StringValue(stringExpr.value());
            case SymbolExpr symbolExpr -> environment.lookup(symbolExpr.name());
            case ListExpr listExpr -> evalList(listExpr, environment);
        };
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
