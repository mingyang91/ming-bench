package ming;

import java.util.ArrayList;
import java.util.List;

final class Interpreter {
    Value evalProgram(List<Expr> program) throws EvalError {
        Value last = null;
        for (Expr expr : program) {
            last = eval(expr);
        }
        if (last == null) {
            throw new EvalError("empty program");
        }
        return last;
    }

    private Value eval(Expr expr) throws EvalError {
        return switch (expr) {
            case Expr.IntegerExpr integerExpr -> new Value.IntegerValue(integerExpr.value());
            case Expr.BooleanExpr booleanExpr -> new Value.BooleanValue(booleanExpr.value());
            case Expr.StringExpr stringExpr -> new Value.StringValue(stringExpr.value());
            case Expr.SymbolExpr symbolExpr -> throw new EvalError(symbolExpr.pos(), "unbound variable: " + symbolExpr.name());
            case Expr.ListExpr listExpr -> evalList(listExpr);
        };
    }

    private Value evalList(Expr.ListExpr listExpr) throws EvalError {
        List<Expr> elements = listExpr.elements();
        if (elements.isEmpty()) {
            throw EvalError.syntax(listExpr.pos(), "cannot evaluate empty list");
        }

        Expr operator = elements.get(0);
        if (!(operator instanceof Expr.SymbolExpr symbolExpr)) {
            throw new EvalError(operator.pos(), "first list element must be a procedure name");
        }

        List<Expr> arguments = elements.subList(1, elements.size());
        return switch (symbolExpr.name()) {
            case "and" -> evalAnd(arguments);
            case "or" -> evalOr(arguments);
            default -> applyBuiltin(symbolExpr.name(), arguments, listExpr.pos());
        };
    }

    private Value evalAnd(List<Expr> arguments) throws EvalError {
        Value result = new Value.BooleanValue(true);
        for (Expr argument : arguments) {
            result = eval(argument);
            if (!result.isTruthy()) {
                return result;
            }
        }
        return result;
    }

    private Value evalOr(List<Expr> arguments) throws EvalError {
        Value result = new Value.BooleanValue(false);
        for (Expr argument : arguments) {
            result = eval(argument);
            if (result.isTruthy()) {
                return result;
            }
        }
        return result;
    }

    private Value applyBuiltin(String name, List<Expr> argumentExprs, SourcePos pos) throws EvalError {
        List<Value> arguments = new ArrayList<>(argumentExprs.size());
        for (Expr argumentExpr : argumentExprs) {
            arguments.add(eval(argumentExpr));
        }

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
