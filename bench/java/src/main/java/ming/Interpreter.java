package ming;

import java.util.List;

public class Interpreter {

    public SchemeValue eval(SchemeValue expr) throws EvalError {
        return switch (expr) {
            case SchemeValue.IntVal v -> v;
            case SchemeValue.BoolVal v -> v;
            case SchemeValue.StringVal v -> v;
            case SchemeValue.SymbolVal v -> throw new EvalError("unbound variable: " + v.name());
            case SchemeValue.ListVal v -> evalList(v.elements());
        };
    }

    private SchemeValue evalList(List<SchemeValue> elements) throws EvalError {
        if (elements.isEmpty()) {
            throw new EvalError("empty application");
        }
        SchemeValue head = elements.getFirst();
        if (head instanceof SchemeValue.SymbolVal sym) {
            String name = sym.name();
            return switch (name) {
                case "and" -> evalAnd(elements);
                case "or" -> evalOr(elements);
                default -> evalProcCall(name, elements);
            };
        }
        throw new EvalError("not a procedure: " + head.display());
    }

    private SchemeValue evalProcCall(String name, List<SchemeValue> elements) throws EvalError {
        var args = new java.util.ArrayList<SchemeValue>();
        for (int i = 1; i < elements.size(); i++) {
            args.add(eval(elements.get(i)));
        }
        return switch (name) {
            case "+" -> arithPlus(args);
            case "-" -> arithMinus(args);
            case "*" -> arithMul(args);
            case "/" -> arithDiv(args);
            case "<" -> compare(args, (a, b) -> a < b);
            case ">" -> compare(args, (a, b) -> a > b);
            case "=" -> compare(args, (a, b) -> a == b);
            case "<=" -> compare(args, (a, b) -> a <= b);
            case ">=" -> compare(args, (a, b) -> a >= b);
            case "not" -> {
                if (args.size() != 1) throw new EvalError("not: expected 1 argument, got " + args.size());
                yield new SchemeValue.BoolVal(!args.getFirst().isTruthy());
            }
            default -> throw new EvalError("unbound variable: " + name);
        };
    }

    private SchemeValue arithPlus(List<SchemeValue> args) throws EvalError {
        long result = 0;
        for (var arg : args) {
            result += requireInt(arg);
        }
        return new SchemeValue.IntVal(result);
    }

    private SchemeValue arithMinus(List<SchemeValue> args) throws EvalError {
        if (args.isEmpty()) throw new EvalError("-: expected at least 1 argument");
        if (args.size() == 1) {
            return new SchemeValue.IntVal(-requireInt(args.getFirst()));
        }
        long result = requireInt(args.getFirst());
        for (int i = 1; i < args.size(); i++) {
            result -= requireInt(args.get(i));
        }
        return new SchemeValue.IntVal(result);
    }

    private SchemeValue arithMul(List<SchemeValue> args) throws EvalError {
        long result = 1;
        for (var arg : args) {
            result *= requireInt(arg);
        }
        return new SchemeValue.IntVal(result);
    }

    private SchemeValue arithDiv(List<SchemeValue> args) throws EvalError {
        if (args.isEmpty()) throw new EvalError("/: expected at least 1 argument");
        long result = requireInt(args.getFirst());
        for (int i = 1; i < args.size(); i++) {
            long divisor = requireInt(args.get(i));
            if (divisor == 0) throw new EvalError("division by zero");
            result /= divisor;
        }
        return new SchemeValue.IntVal(result);
    }

    @FunctionalInterface
    interface LongBiPredicate {
        boolean test(long a, long b);
    }

    private SchemeValue compare(List<SchemeValue> args, LongBiPredicate pred) throws EvalError {
        if (args.size() < 2) throw new EvalError("comparison: expected at least 2 arguments");
        for (int i = 0; i < args.size() - 1; i++) {
            if (!pred.test(requireInt(args.get(i)), requireInt(args.get(i + 1)))) {
                return new SchemeValue.BoolVal(false);
            }
        }
        return new SchemeValue.BoolVal(true);
    }

    private SchemeValue evalAnd(List<SchemeValue> elements) throws EvalError {
        SchemeValue result = new SchemeValue.BoolVal(true);
        for (int i = 1; i < elements.size(); i++) {
            result = eval(elements.get(i));
            if (!result.isTruthy()) return result;
        }
        return result;
    }

    private SchemeValue evalOr(List<SchemeValue> elements) throws EvalError {
        SchemeValue result = new SchemeValue.BoolVal(false);
        for (int i = 1; i < elements.size(); i++) {
            result = eval(elements.get(i));
            if (result.isTruthy()) return result;
        }
        return result;
    }

    private long requireInt(SchemeValue v) throws EvalError {
        if (v instanceof SchemeValue.IntVal iv) return iv.value();
        throw new EvalError("expected number, got: " + v.display());
    }
}
