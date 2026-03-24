package ming;

import java.util.ArrayList;
import java.util.List;

final class Interpreter {
    private final Environment global = new Environment(null);

    Value evalProgram(List<Expr> program) throws EvalError {
        Value last = null;
        for (Expr expr : program) {
            last = eval(expr, global);
        }
        if (last == null) {
            throw new EvalError("empty program");
        }
        return last;
    }

    private Value eval(Expr expr, Environment env) throws EvalError {
        return switch (expr) {
            case Expr.IntegerExpr integerExpr -> new Value.IntegerValue(integerExpr.value());
            case Expr.BooleanExpr booleanExpr -> new Value.BooleanValue(booleanExpr.value());
            case Expr.StringExpr stringExpr -> new Value.StringValue(stringExpr.value());
            case Expr.SymbolExpr symbolExpr -> lookupSymbol(symbolExpr.name(), symbolExpr.pos(), env);
            case Expr.ListExpr listExpr -> evalList(listExpr, env);
        };
    }

    private Value evalList(Expr.ListExpr listExpr, Environment env) throws EvalError {
        List<Expr> elements = listExpr.elements();
        if (elements.isEmpty()) {
            throw EvalError.syntax(listExpr.pos(), "cannot evaluate empty list");
        }

        Expr operator = elements.get(0);
        if (operator instanceof Expr.SymbolExpr symbolExpr) {
            List<Expr> arguments = elements.subList(1, elements.size());
            return switch (symbolExpr.name()) {
                case "define" -> evalDefine(arguments, env, listExpr.pos());
                case "if" -> evalIf(arguments, env, listExpr.pos());
                case "quote" -> evalQuote(arguments, listExpr.pos());
                case "lambda" -> evalLambda(arguments, env, listExpr.pos());
                case "and" -> evalAnd(arguments, env);
                case "or" -> evalOr(arguments, env);
                default -> apply(eval(operator, env), arguments, env, listExpr.pos());
            };
        }

        return apply(eval(operator, env), elements.subList(1, elements.size()), env, listExpr.pos());
    }

    private Value evalDefine(List<Expr> arguments, Environment env, SourcePos pos) throws EvalError {
        if (arguments.size() < 2) {
            throw EvalError.syntax(pos, "define expects a target and value");
        }

        Expr target = arguments.get(0);
        if (target instanceof Expr.SymbolExpr symbolExpr) {
            if (arguments.size() != 2) {
                throw EvalError.syntax(pos, "define variable form expects exactly one value expression");
            }
            Value value = eval(arguments.get(1), env);
            env.define(symbolExpr.name(), value);
            return new Value.VoidValue();
        }

        if (target instanceof Expr.ListExpr signatureExpr) {
            List<Expr> signature = signatureExpr.elements();
            if (signature.isEmpty()) {
                throw EvalError.syntax(signatureExpr.pos(), "define function form requires a function name");
            }
            if (!(signature.get(0) instanceof Expr.SymbolExpr nameExpr)) {
                throw EvalError.syntax(signature.get(0).pos(), "function name must be a symbol");
            }

            List<String> parameters = parseParameters(signature.subList(1, signature.size()), signatureExpr.pos());
            List<Expr> body = arguments.subList(1, arguments.size());
            if (body.isEmpty()) {
                throw EvalError.syntax(pos, "define function form requires a body");
            }

            Value.ClosureValue closure = new Value.ClosureValue(parameters, body, env);
            env.define(nameExpr.name(), closure);
            return new Value.VoidValue();
        }

        throw EvalError.syntax(target.pos(), "invalid define target");
    }

    private Value evalIf(List<Expr> arguments, Environment env, SourcePos pos) throws EvalError {
        if (arguments.size() != 3) {
            throw EvalError.syntax(pos, "if expects exactly 3 arguments");
        }

        Value condition = eval(arguments.get(0), env);
        if (condition.isTruthy()) {
            return eval(arguments.get(1), env);
        }
        return eval(arguments.get(2), env);
    }

    private Value evalQuote(List<Expr> arguments, SourcePos pos) throws EvalError {
        if (arguments.size() != 1) {
            throw EvalError.syntax(pos, "quote expects exactly 1 argument");
        }
        return quote(arguments.get(0));
    }

    private Value evalLambda(List<Expr> arguments, Environment env, SourcePos pos) throws EvalError {
        if (arguments.size() < 2) {
            throw EvalError.syntax(pos, "lambda expects a parameter list and body");
        }
        if (!(arguments.get(0) instanceof Expr.ListExpr parameterList)) {
            throw EvalError.syntax(arguments.get(0).pos(), "lambda parameters must be a list");
        }

        List<String> parameters = parseParameters(parameterList.elements(), parameterList.pos());
        List<Expr> body = arguments.subList(1, arguments.size());
        return new Value.ClosureValue(parameters, body, env);
    }

    private List<String> parseParameters(List<Expr> parameterExprs, SourcePos pos) throws EvalError {
        List<String> parameters = new ArrayList<>(parameterExprs.size());
        for (Expr parameterExpr : parameterExprs) {
            if (!(parameterExpr instanceof Expr.SymbolExpr symbolExpr)) {
                throw EvalError.syntax(pos, "parameters must be symbols");
            }
            parameters.add(symbolExpr.name());
        }
        return List.copyOf(parameters);
    }

    private Value quote(Expr expr) throws EvalError {
        return switch (expr) {
            case Expr.IntegerExpr integerExpr -> new Value.IntegerValue(integerExpr.value());
            case Expr.BooleanExpr booleanExpr -> new Value.BooleanValue(booleanExpr.value());
            case Expr.StringExpr stringExpr -> new Value.StringValue(stringExpr.value());
            case Expr.SymbolExpr symbolExpr -> new Value.SymbolValue(symbolExpr.name());
            case Expr.ListExpr listExpr -> {
                List<Value> values = new ArrayList<>(listExpr.elements().size());
                for (Expr element : listExpr.elements()) {
                    values.add(quote(element));
                }
                yield new Value.ListValue(values);
            }
        };
    }

    private Value evalAnd(List<Expr> arguments, Environment env) throws EvalError {
        Value result = new Value.BooleanValue(true);
        for (Expr argument : arguments) {
            result = eval(argument, env);
            if (!result.isTruthy()) {
                return result;
            }
        }
        return result;
    }

    private Value evalOr(List<Expr> arguments, Environment env) throws EvalError {
        Value result = new Value.BooleanValue(false);
        for (Expr argument : arguments) {
            result = eval(argument, env);
            if (result.isTruthy()) {
                return result;
            }
        }
        return result;
    }

    private Value lookupSymbol(String name, SourcePos pos, Environment env) throws EvalError {
        Value value = env.lookup(name);
        if (value != null) {
            return value;
        }

        Value builtin = builtinValue(name);
        if (builtin != null) {
            return builtin;
        }

        throw new EvalError(pos, "unbound variable: " + name);
    }

    private Value builtinValue(String name) {
        return switch (name) {
            case "+", "-", "*", "/", "<", ">", "=", "<=", "not" -> new Value.BuiltinProcedure(name);
            default -> null;
        };
    }

    private Value apply(Value operator, List<Expr> argumentExprs, Environment env, SourcePos pos) throws EvalError {
        return switch (operator) {
            case Value.BuiltinProcedure builtinProcedure -> applyBuiltin(builtinProcedure.name(), argumentExprs, env, pos);
            case Value.ClosureValue closureValue -> applyClosure(closureValue, argumentExprs, env, pos);
            default -> throw new EvalError(pos, "attempted to call non-procedure");
        };
    }

    private Value applyClosure(Value.ClosureValue closure, List<Expr> argumentExprs, Environment env, SourcePos pos) throws EvalError {
        List<Value> arguments = evalArguments(argumentExprs, env);
        if (arguments.size() != closure.parameters().size()) {
            throw EvalError.arity(pos, "lambda", "expected " + closure.parameters().size() + " arguments");
        }

        Environment callEnv = new Environment(closure.env());
        for (int i = 0; i < closure.parameters().size(); i++) {
            callEnv.define(closure.parameters().get(i), arguments.get(i));
        }
        return evalSequence(closure.body(), callEnv);
    }

    private Value evalSequence(List<Expr> expressions, Environment env) throws EvalError {
        Value last = new Value.VoidValue();
        for (Expr expression : expressions) {
            last = eval(expression, env);
        }
        return last;
    }

    private List<Value> evalArguments(List<Expr> argumentExprs, Environment env) throws EvalError {
        List<Value> arguments = new ArrayList<>(argumentExprs.size());
        for (Expr argumentExpr : argumentExprs) {
            arguments.add(eval(argumentExpr, env));
        }
        return arguments;
    }

    private Value applyBuiltin(String name, List<Expr> argumentExprs, Environment env, SourcePos pos) throws EvalError {
        List<Value> arguments = evalArguments(argumentExprs, env);

        return switch (name) {
            case "+" -> add(arguments, pos);
            case "-" -> subtract(arguments, pos);
            case "*" -> multiply(arguments, pos);
            case "/" -> divide(arguments, pos);
            case "<" -> compare(arguments, pos, "<", (left, right) -> left < right);
            case ">" -> compare(arguments, pos, ">", (left, right) -> left > right);
            case "=" -> compare(arguments, pos, "=", (left, right) -> left == right);
            case "<=" -> compare(arguments, pos, "<=", (left, right) -> left <= right);
            case "not" -> not(arguments, pos);
            default -> throw new EvalError(pos, "unknown procedure: " + name);
        };
    }

    private Value add(List<Value> arguments, SourcePos pos) throws EvalError {
        long total = 0;
        for (Value argument : arguments) {
            total += expectInteger(argument, pos, "+");
        }
        return new Value.IntegerValue(total);
    }

    private Value subtract(List<Value> arguments, SourcePos pos) throws EvalError {
        if (arguments.isEmpty()) {
            throw EvalError.arity(pos, "-", "expected at least 1 argument");
        }

        long result = expectInteger(arguments.get(0), pos, "-");
        if (arguments.size() == 1) {
            return new Value.IntegerValue(-result);
        }

        for (int i = 1; i < arguments.size(); i++) {
            result -= expectInteger(arguments.get(i), pos, "-");
        }
        return new Value.IntegerValue(result);
    }

    private Value multiply(List<Value> arguments, SourcePos pos) throws EvalError {
        long product = 1;
        for (Value argument : arguments) {
            product *= expectInteger(argument, pos, "*");
        }
        return new Value.IntegerValue(product);
    }

    private Value divide(List<Value> arguments, SourcePos pos) throws EvalError {
        if (arguments.size() < 2) {
            throw EvalError.arity(pos, "/", "expected at least 2 arguments");
        }

        long result = expectInteger(arguments.get(0), pos, "/");
        for (int i = 1; i < arguments.size(); i++) {
            long divisor = expectInteger(arguments.get(i), pos, "/");
            if (divisor == 0) {
                throw new EvalError(pos, "division by zero");
            }
            result /= divisor;
        }
        return new Value.IntegerValue(result);
    }

    private Value compare(List<Value> arguments, SourcePos pos, String name, LongComparison comparison) throws EvalError {
        if (arguments.size() <= 1) {
            return new Value.BooleanValue(true);
        }

        long previous = expectInteger(arguments.get(0), pos, name);
        for (int i = 1; i < arguments.size(); i++) {
            long current = expectInteger(arguments.get(i), pos, name);
            if (!comparison.test(previous, current)) {
                return new Value.BooleanValue(false);
            }
            previous = current;
        }
        return new Value.BooleanValue(true);
    }

    private Value not(List<Value> arguments, SourcePos pos) throws EvalError {
        if (arguments.size() != 1) {
            throw EvalError.arity(pos, "not", "expected exactly 1 argument");
        }
        return new Value.BooleanValue(!arguments.get(0).isTruthy());
    }

    private long expectInteger(Value value, SourcePos pos, String name) throws EvalError {
        if (value instanceof Value.IntegerValue integerValue) {
            return integerValue.value();
        }
        throw EvalError.type(pos, name + " expects integer arguments");
    }

    @FunctionalInterface
    private interface LongComparison {
        boolean test(long left, long right);
    }
}
