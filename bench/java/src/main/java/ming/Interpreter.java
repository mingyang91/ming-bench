package ming;

import java.util.HashMap;
import java.util.List;
import java.util.Map;

public class Interpreter {
    private final Map<String, SchemeValue> globals = new HashMap<>();

    public Interpreter() {
    }

    public SchemeValue eval(SchemeValue expr) throws EvalError {
        return switch (expr) {
            case SchemeValue.IntVal v -> v;
            case SchemeValue.BoolVal v -> v;
            case SchemeValue.StringVal v -> v;
            case SchemeValue.SymbolVal v -> {
                var val = globals.get(v.name());
                if (val == null) throw new EvalError("unbound variable: " + v.name());
                yield val;
            }
            case SchemeValue.ListVal v -> evalList(v);
        };
    }

    private SchemeValue evalList(SchemeValue.ListVal list) throws EvalError {
        if (list.elements().isEmpty()) throw new EvalError("empty application");

        var first = list.elements().get(0);
        if (first instanceof SchemeValue.SymbolVal sym) {
            String name = sym.name();
            return switch (name) {
                case "and" -> evalAnd(list.elements());
                case "or" -> evalOr(list.elements());
                default -> evalApplication(list.elements());
            };
        }
        return evalApplication(list.elements());
    }

    private SchemeValue evalAnd(List<SchemeValue> elements) throws EvalError {
        if (elements.size() == 1) return new SchemeValue.BoolVal(true);
        SchemeValue result = new SchemeValue.BoolVal(true);
        for (int i = 1; i < elements.size(); i++) {
            result = eval(elements.get(i));
            if (!result.isTruthy()) return result;
        }
        return result;
    }

    private SchemeValue evalOr(List<SchemeValue> elements) throws EvalError {
        if (elements.size() == 1) return new SchemeValue.BoolVal(false);
        for (int i = 1; i < elements.size(); i++) {
            var result = eval(elements.get(i));
            if (result.isTruthy()) return result;
        }
        return new SchemeValue.BoolVal(false);
    }

    private SchemeValue evalApplication(List<SchemeValue> elements) throws EvalError {
        var first = elements.get(0);
        if (!(first instanceof SchemeValue.SymbolVal sym)) {
            throw new EvalError("not a procedure: " + first.display());
        }

        String op = sym.name();
        var args = new SchemeValue[elements.size() - 1];
        for (int i = 1; i < elements.size(); i++) {
            args[i - 1] = eval(elements.get(i));
        }

        return switch (op) {
            case "+" -> arith(args, 0, Long::sum);
            case "-" -> {
                if (args.length == 0) throw new EvalError("-: need at least one argument");
                if (args.length == 1) yield new SchemeValue.IntVal(-asLong(args[0]));
                long result = asLong(args[0]);
                for (int i = 1; i < args.length; i++) result -= asLong(args[i]);
                yield new SchemeValue.IntVal(result);
            }
            case "*" -> arith(args, 1, (a, b) -> a * b);
            case "/" -> {
                if (args.length == 0) throw new EvalError("/: need at least one argument");
                long result = asLong(args[0]);
                for (int i = 1; i < args.length; i++) {
                    long divisor = asLong(args[i]);
                    if (divisor == 0) throw new EvalError("division by zero");
                    result /= divisor;
                }
                yield new SchemeValue.IntVal(result);
            }
            case "<" -> compare(args, (a, b) -> a < b);
            case ">" -> compare(args, (a, b) -> a > b);
            case "=" -> compare(args, (a, b) -> a == b);
            case "<=" -> compare(args, (a, b) -> a <= b);
            case ">=" -> compare(args, (a, b) -> a >= b);
            case "not" -> {
                if (args.length != 1) throw new EvalError("not: expected 1 argument");
                yield new SchemeValue.BoolVal(!args[0].isTruthy());
            }
            default -> throw new EvalError("unbound variable: " + op);
        };
    }

    @FunctionalInterface
    interface LongBinOp { long apply(long a, long b); }

    @FunctionalInterface
    interface LongCmp { boolean test(long a, long b); }

    private SchemeValue arith(SchemeValue[] args, long identity, LongBinOp op) throws EvalError {
        long result = identity;
        for (var arg : args) result = op.apply(result, asLong(arg));
        return new SchemeValue.IntVal(result);
    }

    private SchemeValue compare(SchemeValue[] args, LongCmp cmp) throws EvalError {
        if (args.length < 2) throw new EvalError("comparison needs at least 2 arguments");
        for (int i = 0; i < args.length - 1; i++) {
            if (!cmp.test(asLong(args[i]), asLong(args[i + 1]))) {
                return new SchemeValue.BoolVal(false);
            }
        }
        return new SchemeValue.BoolVal(true);
    }

    private long asLong(SchemeValue v) throws EvalError {
        if (v instanceof SchemeValue.IntVal iv) return iv.value();
        throw new EvalError("expected number, got: " + v.display());
    }
}
