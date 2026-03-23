package ming;

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
        var tokens = new Tokenizer(input).tokenize();
        var exprs = new Parser(tokens).parseAll();
        if (exprs.isEmpty()) throw new EvalError("No expressions");
        SchemeValue result = null;
        for (SchemeValue expr : exprs) {
            result = eval(expr);
        }
        return result.display();
    }

    /**
     * Evaluate Scheme expressions and return both the result string
     * and any captured output from display/write/newline.
     */
    public EvalResult evalStrWithOutput(String input) throws EvalError {
        throw new EvalError("not implemented");
    }

    private SchemeValue eval(SchemeValue expr) throws EvalError {
        return switch (expr) {
            case SchemeValue.IntVal v -> v;
            case SchemeValue.BoolVal v -> v;
            case SchemeValue.StringVal v -> v;
            case SchemeValue.SymbolVal v -> throw new EvalError("Unbound variable: " + v.name());
            case SchemeValue.ListVal v -> evalList(v.elements());
        };
    }

    private SchemeValue evalList(List<SchemeValue> elems) throws EvalError {
        if (elems.isEmpty()) throw new EvalError("Empty application");
        SchemeValue head = elems.getFirst();
        if (head instanceof SchemeValue.SymbolVal sym) {
            String name = sym.name();
            // Special forms
            return switch (name) {
                case "and" -> evalAnd(elems);
                case "or" -> evalOr(elems);
                default -> evalProcCall(name, elems);
            };
        }
        throw new EvalError("Not a procedure: " + head.display());
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

    private SchemeValue evalProcCall(String name, List<SchemeValue> elems) throws EvalError {
        return switch (name) {
            case "+" -> {
                long sum = 0;
                for (int i = 1; i < elems.size(); i++) sum += requireInt(eval(elems.get(i)));
                yield new SchemeValue.IntVal(sum);
            }
            case "-" -> {
                if (elems.size() < 2) throw new EvalError("- requires at least 1 argument");
                if (elems.size() == 2) {
                    yield new SchemeValue.IntVal(-requireInt(eval(elems.get(1))));
                }
                long result = requireInt(eval(elems.get(1)));
                for (int i = 2; i < elems.size(); i++) result -= requireInt(eval(elems.get(i)));
                yield new SchemeValue.IntVal(result);
            }
            case "*" -> {
                long product = 1;
                for (int i = 1; i < elems.size(); i++) product *= requireInt(eval(elems.get(i)));
                yield new SchemeValue.IntVal(product);
            }
            case "/" -> {
                if (elems.size() < 3) throw new EvalError("/ requires at least 2 arguments");
                long result = requireInt(eval(elems.get(1)));
                for (int i = 2; i < elems.size(); i++) {
                    long divisor = requireInt(eval(elems.get(i)));
                    if (divisor == 0) throw new EvalError("Division by zero");
                    result /= divisor;
                }
                yield new SchemeValue.IntVal(result);
            }
            case "<" -> {
                long a = requireInt(eval(elems.get(1)));
                long b = requireInt(eval(elems.get(2)));
                yield new SchemeValue.BoolVal(a < b);
            }
            case ">" -> {
                long a = requireInt(eval(elems.get(1)));
                long b = requireInt(eval(elems.get(2)));
                yield new SchemeValue.BoolVal(a > b);
            }
            case "=" -> {
                long a = requireInt(eval(elems.get(1)));
                long b = requireInt(eval(elems.get(2)));
                yield new SchemeValue.BoolVal(a == b);
            }
            case "<=" -> {
                long a = requireInt(eval(elems.get(1)));
                long b = requireInt(eval(elems.get(2)));
                yield new SchemeValue.BoolVal(a <= b);
            }
            case "not" -> {
                SchemeValue val = eval(elems.get(1));
                yield new SchemeValue.BoolVal(!val.isTruthy());
            }
            default -> throw new EvalError("Unbound variable: " + name);
        };
    }

    private long requireInt(SchemeValue val) throws EvalError {
        if (val instanceof SchemeValue.IntVal iv) return iv.value();
        throw new EvalError("Expected integer, got: " + val.display());
    }
}
