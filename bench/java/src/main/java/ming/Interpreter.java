package ming;

import java.util.ArrayList;
import java.util.List;

public class Interpreter {
    private final Environment globalEnv = new Environment();
    private final StringBuilder outputBuffer = new StringBuilder();

    public Interpreter() {
        registerBuiltins();
    }

    public String getOutput() {
        return outputBuffer.toString();
    }

    private void registerBuiltins() {
        builtin("+", args -> arithPlus(args));
        builtin("-", args -> arithMinus(args));
        builtin("*", args -> arithMul(args));
        builtin("/", args -> arithDiv(args));
        builtin("<", args -> compare(args, (a, b) -> a < b));
        builtin(">", args -> compare(args, (a, b) -> a > b));
        builtin("=", args -> compare(args, (a, b) -> a == b));
        builtin("<=", args -> compare(args, (a, b) -> a <= b));
        builtin(">=", args -> compare(args, (a, b) -> a >= b));
        builtin("not", args -> {
            if (args.size() != 1) throw new EvalError("not: expected 1 argument, got " + args.size());
            return new SchemeValue.BoolVal(!args.getFirst().isTruthy());
        });
        builtin("cons", args -> {
            if (args.size() != 2) throw new EvalError("cons: expected 2 arguments");
            return new SchemeValue.PairVal(args.get(0), args.get(1));
        });
        builtin("car", args -> {
            if (args.size() != 1) throw new EvalError("car: expected 1 argument");
            SchemeValue v = args.getFirst();
            if (v instanceof SchemeValue.PairVal p) return p.car();
            if (v instanceof SchemeValue.ListVal l && !l.elements().isEmpty()) return l.elements().getFirst();
            throw new EvalError("car: expected pair, got: " + v.display());
        });
        builtin("cdr", args -> {
            if (args.size() != 1) throw new EvalError("cdr: expected 1 argument");
            SchemeValue v = args.getFirst();
            if (v instanceof SchemeValue.PairVal p) return p.cdr();
            if (v instanceof SchemeValue.ListVal l && !l.elements().isEmpty()) {
                return new SchemeValue.ListVal(l.elements().subList(1, l.elements().size()));
            }
            throw new EvalError("cdr: expected pair, got: " + v.display());
        });
        builtin("null?", args -> {
            if (args.size() != 1) throw new EvalError("null?: expected 1 argument");
            SchemeValue v = args.getFirst();
            return new SchemeValue.BoolVal(v instanceof SchemeValue.ListVal l && l.elements().isEmpty());
        });
        builtin("list", args -> {
            if (args.isEmpty()) return new SchemeValue.ListVal(List.of());
            SchemeValue result = new SchemeValue.ListVal(List.of());
            for (int i = args.size() - 1; i >= 0; i--) {
                result = new SchemeValue.PairVal(args.get(i), result);
            }
            return result;
        });
        builtin("length", args -> {
            if (args.size() != 1) throw new EvalError("length: expected 1 argument");
            SchemeValue v = args.getFirst();
            long count = 0;
            while (true) {
                if (v instanceof SchemeValue.ListVal l) {
                    count += l.elements().size();
                    break;
                } else if (v instanceof SchemeValue.PairVal p) {
                    count++;
                    v = p.cdr();
                } else {
                    throw new EvalError("length: expected list");
                }
            }
            return new SchemeValue.IntVal(count);
        });
        builtin("append", args -> {
            SchemeValue result = new SchemeValue.ListVal(List.of());
            for (int i = args.size() - 1; i >= 0; i--) {
                SchemeValue lst = args.get(i);
                var elems = new ArrayList<SchemeValue>();
                while (true) {
                    if (lst instanceof SchemeValue.PairVal p) {
                        elems.add(p.car());
                        lst = p.cdr();
                    } else if (lst instanceof SchemeValue.ListVal l) {
                        elems.addAll(l.elements());
                        break;
                    } else {
                        break;
                    }
                }
                for (int j = elems.size() - 1; j >= 0; j--) {
                    result = new SchemeValue.PairVal(elems.get(j), result);
                }
            }
            return result;
        });
        builtin("string?", args -> {
            if (args.size() != 1) throw new EvalError("string?: expected 1 argument");
            return new SchemeValue.BoolVal(args.getFirst() instanceof SchemeValue.StringVal || args.getFirst() instanceof SchemeValue.MutableStringVal);
        });
        builtin("number?", args -> {
            if (args.size() != 1) throw new EvalError("number?: expected 1 argument");
            return new SchemeValue.BoolVal(args.getFirst() instanceof SchemeValue.IntVal);
        });
        builtin("boolean?", args -> {
            if (args.size() != 1) throw new EvalError("boolean?: expected 1 argument");
            return new SchemeValue.BoolVal(args.getFirst() instanceof SchemeValue.BoolVal);
        });
        builtin("pair?", args -> {
            if (args.size() != 1) throw new EvalError("pair?: expected 1 argument");
            SchemeValue v = args.getFirst();
            return new SchemeValue.BoolVal(v instanceof SchemeValue.PairVal ||
                (v instanceof SchemeValue.ListVal l && !l.elements().isEmpty()));
        });
        builtin("symbol?", args -> {
            if (args.size() != 1) throw new EvalError("symbol?: expected 1 argument");
            return new SchemeValue.BoolVal(args.getFirst() instanceof SchemeValue.SymbolVal);
        });
        builtin("char?", args -> {
            if (args.size() != 1) throw new EvalError("char?: expected 1 argument");
            return new SchemeValue.BoolVal(args.getFirst() instanceof SchemeValue.CharVal);
        });
        // Display/Write/Newline
        builtin("display", args -> {
            if (args.size() != 1) throw new EvalError("display: expected 1 argument");
            outputBuffer.append(args.getFirst().displayStr());
            return new SchemeValue.VoidVal();
        });
        builtin("write", args -> {
            if (args.size() != 1) throw new EvalError("write: expected 1 argument");
            outputBuffer.append(args.getFirst().display());
            return new SchemeValue.VoidVal();
        });
        builtin("newline", args -> {
            if (!args.isEmpty()) throw new EvalError("newline: expected 0 arguments");
            outputBuffer.append('\n');
            return new SchemeValue.VoidVal();
        });
        // String operations
        builtin("string-append", args -> {
            var sb = new StringBuilder();
            for (var arg : args) {
                sb.append(requireString(arg, "string-append"));
            }
            return new SchemeValue.StringVal(sb.toString());
        });
        builtin("string-length", args -> {
            if (args.size() != 1) throw new EvalError("string-length: expected 1 argument");
            return new SchemeValue.IntVal(requireString(args.getFirst(), "string-length").length());
        });
        builtin("substring", args -> {
            if (args.size() != 3) throw new EvalError("substring: expected 3 arguments");
            String str = requireString(args.get(0), "substring");
            long start = requireInt(args.get(1));
            long end = requireInt(args.get(2));
            return new SchemeValue.StringVal(str.substring((int) start, (int) end));
        });
        builtin("string->number", args -> {
            if (args.size() != 1) throw new EvalError("string->number: expected 1 argument");
            String str = requireString(args.getFirst(), "string->number");
            try {
                return new SchemeValue.IntVal(Long.parseLong(str));
            } catch (NumberFormatException e) {
                return new SchemeValue.BoolVal(false);
            }
        });
        builtin("number->string", args -> {
            if (args.size() != 1) throw new EvalError("number->string: expected 1 argument");
            return new SchemeValue.StringVal(String.valueOf(requireInt(args.getFirst())));
        });
        builtin("symbol->string", args -> {
            if (args.size() != 1) throw new EvalError("symbol->string: expected 1 argument");
            if (!(args.getFirst() instanceof SchemeValue.SymbolVal s)) throw new EvalError("symbol->string: expected symbol");
            return new SchemeValue.StringVal(s.name());
        });
        builtin("string->symbol", args -> {
            if (args.size() != 1) throw new EvalError("string->symbol: expected 1 argument");
            return new SchemeValue.SymbolVal(requireString(args.getFirst(), "string->symbol"));
        });
        builtin("string-ref", args -> {
            if (args.size() != 2) throw new EvalError("string-ref: expected 2 arguments");
            String str = requireString(args.get(0), "string-ref");
            long idx = requireInt(args.get(1));
            return new SchemeValue.CharVal(str.charAt((int) idx));
        });
        builtin("string-copy", args -> {
            if (args.size() != 1) throw new EvalError("string-copy: expected 1 argument");
            String str = requireString(args.getFirst(), "string-copy");
            return new SchemeValue.MutableStringVal(new StringBuilder(str));
        });
        builtin("string-set!", args -> {
            if (args.size() != 3) throw new EvalError("string-set!: expected 3 arguments");
            if (!(args.get(0) instanceof SchemeValue.MutableStringVal ms)) {
                throw new EvalError("string-set!: expected mutable string");
            }
            long idx = requireInt(args.get(1));
            if (!(args.get(2) instanceof SchemeValue.CharVal ch)) {
                throw new EvalError("string-set!: expected char as third argument");
            }
            ms.chars().setCharAt((int) idx, ch.value());
            return new SchemeValue.VoidVal();
        });
    }

    @FunctionalInterface
    interface CheckedFunction {
        SchemeValue apply(List<SchemeValue> args) throws EvalError;
    }

    private void builtin(String name, CheckedFunction fn) {
        globalEnv.define(name, new SchemeValue.BuiltinVal(name, args -> {
            try {
                return fn.apply(args);
            } catch (EvalError e) {
                throw new RuntimeException(e);
            }
        }));
    }

    private static String posPrefix(SchemeValue expr) {
        SourcePos p = expr.pos();
        return p != null ? p + ": " : "";
    }

    public SchemeValue eval(SchemeValue expr) throws EvalError {
        return eval(expr, globalEnv);
    }

    public SchemeValue eval(SchemeValue expr, Environment env) throws EvalError {
        return switch (expr) {
            case SchemeValue.IntVal v -> v;
            case SchemeValue.BoolVal v -> v;
            case SchemeValue.StringVal v -> v;
            case SchemeValue.LambdaVal v -> v;
            case SchemeValue.BuiltinVal v -> v;
            case SchemeValue.PairVal v -> v;
            case SchemeValue.VoidVal v -> v;
            case SchemeValue.CharVal v -> v;
            case SchemeValue.MutableStringVal v -> v;
            case SchemeValue.SymbolVal v -> {
                try {
                    yield env.get(v.name());
                } catch (EvalError e) {
                    throw new EvalError(posPrefix(expr) + e.getMessage());
                }
            }
            case SchemeValue.ListVal v -> evalList(v, env);
        };
    }

    private SchemeValue evalList(SchemeValue.ListVal listVal, Environment env) throws EvalError {
        List<SchemeValue> elements = listVal.elements();
        if (elements.isEmpty()) {
            throw new EvalError(posPrefix(listVal) + "empty application");
        }
        SchemeValue head = elements.getFirst();
        if (head instanceof SchemeValue.SymbolVal sym) {
            String name = sym.name();
            switch (name) {
                case "and" -> { return evalAnd(elements, env); }
                case "or" -> { return evalOr(elements, env); }
                case "define" -> { return evalDefine(listVal, elements, env); }
                case "if" -> { return evalIf(listVal, elements, env); }
                case "quote" -> {
                    if (elements.size() != 2) throw new EvalError(posPrefix(listVal) + "quote: expected 1 argument");
                    return elements.get(1);
                }
                case "lambda" -> { return evalLambda(listVal, elements, env); }
                case "let" -> { return evalLet(elements, env); }
                case "begin" -> { return evalBegin(elements, env); }
                case "cond" -> { return evalCond(elements, env); }
            }
        }
        // Evaluate head and call as procedure
        SchemeValue proc = eval(head, env);
        var args = new ArrayList<SchemeValue>();
        for (int i = 1; i < elements.size(); i++) {
            args.add(eval(elements.get(i), env));
        }
        return apply(proc, args, listVal);
    }

    private SchemeValue apply(SchemeValue proc, List<SchemeValue> args, SchemeValue callSite) throws EvalError {
        if (proc instanceof SchemeValue.LambdaVal lambda) {
            if (lambda.params().size() != args.size()) {
                throw new EvalError(posPrefix(callSite) + "expected " + lambda.params().size() + " arguments, got " + args.size());
            }
            var localEnv = new Environment(lambda.env());
            for (int i = 0; i < lambda.params().size(); i++) {
                localEnv.define(lambda.params().get(i), args.get(i));
            }
            SchemeValue result = null;
            for (var bodyExpr : lambda.body()) {
                result = eval(bodyExpr, localEnv);
            }
            return result;
        }
        if (proc instanceof SchemeValue.BuiltinVal builtin) {
            try {
                return builtin.fn().apply(args);
            } catch (RuntimeException e) {
                if (e.getCause() instanceof EvalError ee) {
                    // Re-throw with position if not already present
                    String msg = ee.getMessage();
                    if (callSite != null && callSite.pos() != null && !msg.matches(".*\\d+:\\d+.*")) {
                        throw new EvalError(posPrefix(callSite) + msg);
                    }
                    throw ee;
                }
                throw e;
            }
        }
        throw new EvalError(posPrefix(callSite) + "not a procedure: " + proc.display());
    }

    private SchemeValue evalDefine(SchemeValue.ListVal listVal, List<SchemeValue> elements, Environment env) throws EvalError {
        if (elements.size() < 3) throw new EvalError(posPrefix(listVal) + "define: bad syntax");
        SchemeValue target = elements.get(1);
        if (target instanceof SchemeValue.SymbolVal sym) {
            SchemeValue val = eval(elements.get(2), env);
            env.define(sym.name(), val);
            return new SchemeValue.VoidVal();
        } else if (target instanceof SchemeValue.ListVal nameAndParams) {
            if (nameAndParams.elements().isEmpty()) throw new EvalError(posPrefix(listVal) + "define: bad syntax");
            SchemeValue nameVal = nameAndParams.elements().getFirst();
            if (!(nameVal instanceof SchemeValue.SymbolVal nameSym)) {
                throw new EvalError(posPrefix(listVal) + "define: expected symbol as function name");
            }
            var params = new ArrayList<String>();
            for (int i = 1; i < nameAndParams.elements().size(); i++) {
                if (!(nameAndParams.elements().get(i) instanceof SchemeValue.SymbolVal p)) {
                    throw new EvalError(posPrefix(listVal) + "define: expected symbol as parameter");
                }
                params.add(p.name());
            }
            var body = elements.subList(2, elements.size());
            var lambda = new SchemeValue.LambdaVal(params, body, env);
            env.define(nameSym.name(), lambda);
            return new SchemeValue.VoidVal();
        }
        throw new EvalError(posPrefix(listVal) + "define: bad syntax");
    }

    private SchemeValue evalIf(SchemeValue.ListVal listVal, List<SchemeValue> elements, Environment env) throws EvalError {
        if (elements.size() < 3 || elements.size() > 4) throw new EvalError(posPrefix(listVal) + "if: bad syntax");
        SchemeValue cond = eval(elements.get(1), env);
        if (cond.isTruthy()) {
            return eval(elements.get(2), env);
        } else if (elements.size() == 4) {
            return eval(elements.get(3), env);
        }
        return new SchemeValue.BoolVal(false);
    }

    private SchemeValue evalLambda(SchemeValue.ListVal listVal, List<SchemeValue> elements, Environment env) throws EvalError {
        if (elements.size() < 3) throw new EvalError(posPrefix(listVal) + "lambda: bad syntax");
        SchemeValue paramList = elements.get(1);
        if (!(paramList instanceof SchemeValue.ListVal pList)) {
            throw new EvalError(posPrefix(listVal) + "lambda: expected parameter list");
        }
        var params = new ArrayList<String>();
        for (var p : pList.elements()) {
            if (!(p instanceof SchemeValue.SymbolVal sym)) {
                throw new EvalError(posPrefix(listVal) + "lambda: expected symbol as parameter");
            }
            params.add(sym.name());
        }
        var body = elements.subList(2, elements.size());
        return new SchemeValue.LambdaVal(params, body, env);
    }

    private SchemeValue evalAnd(List<SchemeValue> elements, Environment env) throws EvalError {
        SchemeValue result = new SchemeValue.BoolVal(true);
        for (int i = 1; i < elements.size(); i++) {
            result = eval(elements.get(i), env);
            if (!result.isTruthy()) return result;
        }
        return result;
    }

    private SchemeValue evalOr(List<SchemeValue> elements, Environment env) throws EvalError {
        SchemeValue result = new SchemeValue.BoolVal(false);
        for (int i = 1; i < elements.size(); i++) {
            result = eval(elements.get(i), env);
            if (result.isTruthy()) return result;
        }
        return result;
    }

    private SchemeValue evalLet(List<SchemeValue> elements, Environment env) throws EvalError {
        if (elements.size() < 3) throw new EvalError("let: bad syntax");
        // Named let: (let name ((var init) ...) body ...)
        if (elements.get(1) instanceof SchemeValue.SymbolVal nameSym) {
            if (elements.size() < 4) throw new EvalError("let: bad syntax");
            if (!(elements.get(2) instanceof SchemeValue.ListVal bl)) throw new EvalError("let: expected bindings list");
            var params = new ArrayList<String>();
            var inits = new ArrayList<SchemeValue>();
            for (var binding : bl.elements()) {
                if (!(binding instanceof SchemeValue.ListVal b) || b.elements().size() != 2) {
                    throw new EvalError("let: bad binding");
                }
                if (!(b.elements().getFirst() instanceof SchemeValue.SymbolVal sym)) {
                    throw new EvalError("let: expected symbol in binding");
                }
                params.add(sym.name());
                inits.add(eval(b.elements().get(1), env));
            }
            var body = elements.subList(3, elements.size());
            var localEnv = new Environment(env);
            var lambda = new SchemeValue.LambdaVal(params, body, localEnv);
            localEnv.define(nameSym.name(), lambda);
            return apply(lambda, inits, null);
        }
        // Regular let
        if (!(elements.get(1) instanceof SchemeValue.ListVal bl)) throw new EvalError("let: expected bindings list");
        var localEnv = new Environment(env);
        for (var binding : bl.elements()) {
            if (!(binding instanceof SchemeValue.ListVal b) || b.elements().size() != 2) {
                throw new EvalError("let: bad binding");
            }
            if (!(b.elements().getFirst() instanceof SchemeValue.SymbolVal sym)) {
                throw new EvalError("let: expected symbol in binding");
            }
            localEnv.define(sym.name(), eval(b.elements().get(1), env));
        }
        SchemeValue result = null;
        for (int i = 2; i < elements.size(); i++) {
            result = eval(elements.get(i), localEnv);
        }
        return result;
    }

    private SchemeValue evalBegin(List<SchemeValue> elements, Environment env) throws EvalError {
        SchemeValue result = null;
        for (int i = 1; i < elements.size(); i++) {
            result = eval(elements.get(i), env);
        }
        if (result == null) throw new EvalError("begin: empty body");
        return result;
    }

    private SchemeValue evalCond(List<SchemeValue> elements, Environment env) throws EvalError {
        for (int i = 1; i < elements.size(); i++) {
            if (!(elements.get(i) instanceof SchemeValue.ListVal clause) || clause.elements().isEmpty()) {
                throw new EvalError("cond: bad clause");
            }
            SchemeValue test = clause.elements().getFirst();
            if (test instanceof SchemeValue.SymbolVal sym && sym.name().equals("else")) {
                SchemeValue result = null;
                for (int j = 1; j < clause.elements().size(); j++) {
                    result = eval(clause.elements().get(j), env);
                }
                return result;
            }
            SchemeValue testResult = eval(test, env);
            if (testResult.isTruthy()) {
                SchemeValue result = testResult;
                for (int j = 1; j < clause.elements().size(); j++) {
                    result = eval(clause.elements().get(j), env);
                }
                return result;
            }
        }
        return new SchemeValue.BoolVal(false);
    }

    private SchemeValue arithPlus(List<SchemeValue> args) throws EvalError {
        long result = 0;
        for (var arg : args) result += requireInt(arg);
        return new SchemeValue.IntVal(result);
    }

    private SchemeValue arithMinus(List<SchemeValue> args) throws EvalError {
        if (args.isEmpty()) throw new EvalError("-: expected at least 1 argument");
        if (args.size() == 1) return new SchemeValue.IntVal(-requireInt(args.getFirst()));
        long result = requireInt(args.getFirst());
        for (int i = 1; i < args.size(); i++) result -= requireInt(args.get(i));
        return new SchemeValue.IntVal(result);
    }

    private SchemeValue arithMul(List<SchemeValue> args) throws EvalError {
        long result = 1;
        for (var arg : args) result *= requireInt(arg);
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

    private long requireInt(SchemeValue v) throws EvalError {
        if (v instanceof SchemeValue.IntVal iv) return iv.value();
        throw new EvalError("expected number, got: " + v.display());
    }

    private String requireString(SchemeValue v, String caller) throws EvalError {
        if (v instanceof SchemeValue.StringVal s) return s.value();
        if (v instanceof SchemeValue.MutableStringVal s) return s.value();
        throw new EvalError(caller + ": expected string");
    }
}
