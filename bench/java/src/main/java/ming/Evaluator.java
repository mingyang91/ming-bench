package ming;

import java.util.List;

public class Evaluator {
    public String evalStr(String input) throws EvalError {
        Parser parser = new Parser(input);
        List<SchemeValue> exprs = parser.parseAll();
        if (exprs.isEmpty()) throw new EvalError("No expressions");

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
            case SchemeValue.SymbolVal v -> throw new EvalError("Unbound variable: " + v.name());
            case SchemeValue.Void v -> v;
            case SchemeValue.ListVal list -> evalList(list);
        };
    }

    private SchemeValue evalList(SchemeValue.ListVal list) throws EvalError {
        List<SchemeValue> elems = list.elements();
        if (elems.isEmpty()) throw new EvalError("Empty application");

        SchemeValue head = elems.getFirst();
        if (head instanceof SchemeValue.SymbolVal sym) {
            String name = sym.name();
            return switch (name) {
                case "+" -> arith(elems, name);
                case "-" -> arith(elems, name);
                case "*" -> arith(elems, name);
                case "/" -> arith(elems, name);
                case "<" -> compare(elems, name);
                case ">" -> compare(elems, name);
                case "=" -> compare(elems, name);
                case "<=" -> compare(elems, name);
                case "not" -> evalNot(elems);
                case "and" -> evalAnd(elems);
                case "or" -> evalOr(elems);
                default -> throw new EvalError("Unknown procedure: " + name);
            };
        }
        throw new EvalError("Not a procedure");
    }

    private SchemeValue arith(List<SchemeValue> elems, String op) throws EvalError {
        if (elems.size() < 2) throw new EvalError("Wrong number of arguments for " + op);

        if (op.equals("-") && elems.size() == 2) {
            // Unary minus
            long val = asInt(eval(elems.get(1)));
            return new SchemeValue.IntVal(-val);
        }

        long result = asInt(eval(elems.get(1)));
        if (elems.size() == 2 && (op.equals("+") || op.equals("*"))) {
            // Single argument for + and *
            return new SchemeValue.IntVal(result);
        }

        for (int i = 2; i < elems.size(); i++) {
            long val = asInt(eval(elems.get(i)));
            result = switch (op) {
                case "+" -> result + val;
                case "-" -> result - val;
                case "*" -> result * val;
                case "/" -> {
                    if (val == 0) throw new EvalError("Division by zero");
                    yield result / val;
                }
                default -> throw new EvalError("Unknown op");
            };
        }
        return new SchemeValue.IntVal(result);
    }

    private SchemeValue compare(List<SchemeValue> elems, String op) throws EvalError {
        if (elems.size() != 3) throw new EvalError("Wrong number of arguments for " + op);
        long a = asInt(eval(elems.get(1)));
        long b = asInt(eval(elems.get(2)));
        boolean result = switch (op) {
            case "<" -> a < b;
            case ">" -> a > b;
            case "=" -> a == b;
            case "<=" -> a <= b;
            default -> throw new EvalError("Unknown comparator");
        };
        return new SchemeValue.BoolVal(result);
    }

    private SchemeValue evalNot(List<SchemeValue> elems) throws EvalError {
        if (elems.size() != 2) throw new EvalError("Wrong number of arguments for not");
        SchemeValue val = eval(elems.get(1));
        return new SchemeValue.BoolVal(!val.isTruthy());
    }

    private SchemeValue evalAnd(List<SchemeValue> elems) throws EvalError {
        SchemeValue result = new SchemeValue.BoolVal(true);
        for (int i = 1; i < elems.size(); i++) {
            result = eval(elems.get(i));
            if (!result.isTruthy()) return result;
        }
        return result;
    }

    private SchemeValue evalOr(List<SchemeValue> elems) throws EvalError {
        SchemeValue result = new SchemeValue.BoolVal(false);
        for (int i = 1; i < elems.size(); i++) {
            result = eval(elems.get(i));
            if (result.isTruthy()) return result;
        }
        return result;
    }

    private long asInt(SchemeValue val) throws EvalError {
        if (val instanceof SchemeValue.IntVal v) return v.value();
        throw new EvalError("Expected integer, got: " + val.display());
    }
}
