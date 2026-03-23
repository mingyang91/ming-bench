package ming;

import java.util.List;

public class Evaluator {

    public String evalStr(String input) throws EvalError {
        var parser = new Parser(input);
        List<SchemeValue> exprs = parser.parseAll();
        if (exprs.isEmpty()) throw new EvalError("no expressions");
        SchemeValue result = null;
        for (SchemeValue expr : exprs) {
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
            case SchemeValue.SymbolVal v -> throw new EvalError("unbound variable: " + v.name());
            case SchemeValue.ListVal v -> evalList(v.elements());
        };
    }

    private SchemeValue evalList(List<SchemeValue> elements) throws EvalError {
        if (elements.isEmpty()) throw new EvalError("empty application");

        SchemeValue head = elements.getFirst();
        if (head instanceof SchemeValue.SymbolVal sym) {
            String op = sym.name();
            List<SchemeValue> args = elements.subList(1, elements.size());

            return switch (op) {
                case "+" -> arith(args, 0, Long::sum);
                case "-" -> minus(args);
                case "*" -> arith(args, 1, (a, b) -> a * b);
                case "/" -> divide(args);
                case "<" -> compare(args, (a, b) -> a < b);
                case ">" -> compare(args, (a, b) -> a > b);
                case "=" -> compare(args, (a, b) -> a == b);
                case "<=" -> compare(args, (a, b) -> a <= b);
                case ">=" -> compare(args, (a, b) -> a >= b);
                case "not" -> not(args);
                case "and" -> and(args);
                case "or" -> or(args);
                default -> throw new EvalError("unknown procedure: " + op);
            };
        }
        throw new EvalError("not a procedure");
    }

    @FunctionalInterface
    private interface LongBinOp {
        long apply(long a, long b);
    }

    @FunctionalInterface
    private interface LongPred {
        boolean test(long a, long b);
    }

    private SchemeValue arith(List<SchemeValue> args, long identity, LongBinOp op) throws EvalError {
        long result = identity;
        for (SchemeValue arg : args) {
            result = op.apply(result, requireInt(eval(arg)));
        }
        return new SchemeValue.IntVal(result);
    }

    private SchemeValue minus(List<SchemeValue> args) throws EvalError {
        if (args.isEmpty()) throw new EvalError("-: need at least one argument");
        if (args.size() == 1) {
            return new SchemeValue.IntVal(-requireInt(eval(args.getFirst())));
        }
        long result = requireInt(eval(args.getFirst()));
        for (int i = 1; i < args.size(); i++) {
            result -= requireInt(eval(args.get(i)));
        }
        return new SchemeValue.IntVal(result);
    }

    private SchemeValue divide(List<SchemeValue> args) throws EvalError {
        if (args.isEmpty()) throw new EvalError("/: need at least one argument");
        long result = requireInt(eval(args.getFirst()));
        for (int i = 1; i < args.size(); i++) {
            long divisor = requireInt(eval(args.get(i)));
            if (divisor == 0) throw new EvalError("division by zero");
            result /= divisor;
        }
        return new SchemeValue.IntVal(result);
    }

    private SchemeValue compare(List<SchemeValue> args, LongPred pred) throws EvalError {
        if (args.size() < 2) throw new EvalError("comparison needs at least 2 arguments");
        long prev = requireInt(eval(args.getFirst()));
        for (int i = 1; i < args.size(); i++) {
            long curr = requireInt(eval(args.get(i)));
            if (!pred.test(prev, curr)) return new SchemeValue.BoolVal(false);
            prev = curr;
        }
        return new SchemeValue.BoolVal(true);
    }

    private SchemeValue not(List<SchemeValue> args) throws EvalError {
        if (args.size() != 1) throw new EvalError("not: need exactly one argument");
        return new SchemeValue.BoolVal(!eval(args.getFirst()).isTruthy());
    }

    private SchemeValue and(List<SchemeValue> args) throws EvalError {
        SchemeValue result = new SchemeValue.BoolVal(true);
        for (SchemeValue arg : args) {
            result = eval(arg);
            if (!result.isTruthy()) return result;
        }
        return result;
    }

    private SchemeValue or(List<SchemeValue> args) throws EvalError {
        SchemeValue result = new SchemeValue.BoolVal(false);
        for (SchemeValue arg : args) {
            result = eval(arg);
            if (result.isTruthy()) return result;
        }
        return result;
    }

    private long requireInt(SchemeValue v) throws EvalError {
        if (v instanceof SchemeValue.IntVal i) return i.value();
        throw new EvalError("expected integer, got: " + v.display());
    }
}
