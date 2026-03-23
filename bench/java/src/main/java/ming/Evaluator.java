package ming;

import java.util.List;

public class Evaluator {

    public String evalStr(String input) throws EvalError {
        var parser = new Parser(input);
        List<SchemeValue> exprs = parser.parseAll();
        if (exprs.isEmpty()) throw new EvalError("empty input");

        SchemeValue result = null;
        for (var expr : exprs) {
            result = eval(expr);
        }
        return result.display();
    }

    public EvalResult evalStrWithOutput(String input) throws EvalError {
        throw new EvalError("not implemented");
    }

    private SchemeValue eval(SchemeValue expr) throws EvalError {
        return switch (expr) {
            case SchemeValue.IntVal v -> v;
            case SchemeValue.BoolVal v -> v;
            case SchemeValue.StringVal v -> v;
            case SchemeValue.VoidVal v -> v;
            case SchemeValue.SymbolVal v -> throw new EvalError("unbound variable: " + v.name());
            case SchemeValue.ListVal list -> evalList(list);
        };
    }

    private SchemeValue evalList(SchemeValue.ListVal list) throws EvalError {
        if (list.elements().isEmpty()) throw new EvalError("empty application");

        var first = list.elements().getFirst();
        if (first instanceof SchemeValue.SymbolVal sym) {
            String name = sym.name();
            var args = list.elements().subList(1, list.elements().size());

            return switch (name) {
                case "+" -> arithOp(args, 0, Long::sum);
                case "-" -> minusOp(args);
                case "*" -> arithOp(args, 1, (a, b) -> a * b);
                case "/" -> divOp(args);
                case "<" -> cmpOp(args, (a, b) -> a < b);
                case ">" -> cmpOp(args, (a, b) -> a > b);
                case "=" -> cmpOp(args, (a, b) -> a == b);
                case "<=" -> cmpOp(args, (a, b) -> a <= b);
                case ">=" -> cmpOp(args, (a, b) -> a >= b);
                case "not" -> notOp(args);
                case "and" -> andOp(args);
                case "or" -> orOp(args);
                default -> throw new EvalError("unknown procedure: " + name);
            };
        }
        throw new EvalError("not a procedure");
    }

    private long asInt(SchemeValue v) throws EvalError {
        if (v instanceof SchemeValue.IntVal i) return i.value();
        throw new EvalError("expected number, got " + v.display());
    }

    @FunctionalInterface
    interface LongBinOp { long apply(long a, long b); }

    @FunctionalInterface
    interface LongCmp { boolean test(long a, long b); }

    private SchemeValue arithOp(List<SchemeValue> args, long identity, LongBinOp op) throws EvalError {
        long result = identity;
        for (var arg : args) {
            result = op.apply(result, asInt(eval(arg)));
        }
        return new SchemeValue.IntVal(result);
    }

    private SchemeValue minusOp(List<SchemeValue> args) throws EvalError {
        if (args.isEmpty()) throw new EvalError("-: needs at least 1 argument");
        if (args.size() == 1) return new SchemeValue.IntVal(-asInt(eval(args.getFirst())));
        long result = asInt(eval(args.getFirst()));
        for (int i = 1; i < args.size(); i++) {
            result -= asInt(eval(args.get(i)));
        }
        return new SchemeValue.IntVal(result);
    }

    private SchemeValue divOp(List<SchemeValue> args) throws EvalError {
        if (args.isEmpty()) throw new EvalError("/: needs at least 1 argument");
        long result = asInt(eval(args.getFirst()));
        for (int i = 1; i < args.size(); i++) {
            long divisor = asInt(eval(args.get(i)));
            if (divisor == 0) throw new EvalError("division by zero");
            result /= divisor;
        }
        return new SchemeValue.IntVal(result);
    }

    private SchemeValue cmpOp(List<SchemeValue> args, LongCmp cmp) throws EvalError {
        if (args.size() < 2) throw new EvalError("comparison needs at least 2 arguments");
        long prev = asInt(eval(args.getFirst()));
        for (int i = 1; i < args.size(); i++) {
            long cur = asInt(eval(args.get(i)));
            if (!cmp.test(prev, cur)) return new SchemeValue.BoolVal(false);
            prev = cur;
        }
        return new SchemeValue.BoolVal(true);
    }

    private SchemeValue notOp(List<SchemeValue> args) throws EvalError {
        if (args.size() != 1) throw new EvalError("not: needs exactly 1 argument");
        return new SchemeValue.BoolVal(!eval(args.getFirst()).isTruthy());
    }

    private SchemeValue andOp(List<SchemeValue> args) throws EvalError {
        SchemeValue result = new SchemeValue.BoolVal(true);
        for (var arg : args) {
            result = eval(arg);
            if (!result.isTruthy()) return result;
        }
        return result;
    }

    private SchemeValue orOp(List<SchemeValue> args) throws EvalError {
        SchemeValue result = new SchemeValue.BoolVal(false);
        for (var arg : args) {
            result = eval(arg);
            if (result.isTruthy()) return result;
        }
        return result;
    }
}
