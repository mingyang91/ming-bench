package ming;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

public class Evaluator {

    // --- Environment ---

    private static class Env {
        final Map<String, Object> bindings = new HashMap<>();
        final Env parent;

        Env(Env parent) {
            this.parent = parent;
        }

        Object lookup(String name) throws EvalError {
            if (bindings.containsKey(name)) return bindings.get(name);
            if (parent != null) return parent.lookup(name);
            throw new EvalError("unbound variable: " + name);
        }

        void define(String name, Object val) {
            bindings.put(name, val);
        }
    }

    // --- Lambda (closure) ---

    private static class Lambda {
        final List<String> params;
        final Object body;
        final Env closureEnv;

        Lambda(List<String> params, Object body, Env closureEnv) {
            this.params = params;
            this.body = body;
            this.closureEnv = closureEnv;
        }
    }

    // --- Public API ---

    public String evalStr(String input) throws EvalError {
        List<Object> exprs = parse(input);
        Env env = makeGlobalEnv();
        Object result = null;
        for (Object expr : exprs) {
            result = eval(expr, env);
        }
        return schemeToString(result);
    }

    public EvalResult evalStrWithOutput(String input) throws EvalError {
        String result = evalStr(input);
        return new EvalResult(result, "");
    }

    private Env makeGlobalEnv() {
        Env env = new Env(null);
        registerBuiltins(env);
        return env;
    }

    // --- Parser ---

    private int pos;
    private String src;

    private List<Object> parse(String input) throws EvalError {
        this.src = input;
        this.pos = 0;
        List<Object> exprs = new ArrayList<>();
        while (true) {
            skipWhitespace();
            if (pos >= src.length()) break;
            exprs.add(readExpr());
        }
        return exprs;
    }

    private void skipWhitespace() {
        while (pos < src.length()) {
            char c = src.charAt(pos);
            if (c == ';') {
                while (pos < src.length() && src.charAt(pos) != '\n') pos++;
            } else if (Character.isWhitespace(c)) {
                pos++;
            } else {
                break;
            }
        }
    }

    private Object readExpr() throws EvalError {
        skipWhitespace();
        if (pos >= src.length()) throw new EvalError("unexpected end of input");
        char c = src.charAt(pos);
        if (c == '\'') {
            pos++;
            Object datum = readExpr();
            List<Object> quoted = new ArrayList<>();
            quoted.add(new SchemeSymbol("quote"));
            quoted.add(datum);
            return new SchemeList(quoted);
        }
        if (c == '(') {
            return readList();
        } else if (c == '"') {
            return readString();
        } else if (c == '#') {
            return readHash();
        } else {
            return readAtom();
        }
    }

    private SchemeList readList() throws EvalError {
        pos++; // skip '('
        List<Object> elems = new ArrayList<>();
        while (true) {
            skipWhitespace();
            if (pos >= src.length()) throw new EvalError("unexpected end of input");
            if (src.charAt(pos) == ')') {
                pos++;
                return new SchemeList(elems);
            }
            elems.add(readExpr());
        }
    }

    private SchemeString readString() throws EvalError {
        pos++; // skip opening "
        StringBuilder sb = new StringBuilder();
        while (pos < src.length()) {
            char c = src.charAt(pos);
            if (c == '\\') {
                pos++;
                if (pos >= src.length()) throw new EvalError("unexpected end of string");
                char esc = src.charAt(pos);
                switch (esc) {
                    case 'n' -> sb.append('\n');
                    case 't' -> sb.append('\t');
                    case '\\' -> sb.append('\\');
                    case '"' -> sb.append('"');
                    default -> { sb.append('\\'); sb.append(esc); }
                }
                pos++;
            } else if (c == '"') {
                pos++;
                return new SchemeString(sb.toString());
            } else {
                sb.append(c);
                pos++;
            }
        }
        throw new EvalError("unterminated string");
    }

    private Object readHash() throws EvalError {
        pos++; // skip '#'
        if (pos >= src.length()) throw new EvalError("unexpected end of input after #");
        char c = src.charAt(pos);
        if (c == 't') {
            pos++;
            return Boolean.TRUE;
        } else if (c == 'f') {
            pos++;
            return Boolean.FALSE;
        }
        throw new EvalError("unknown hash literal: #" + c);
    }

    private Object readAtom() throws EvalError {
        int start = pos;
        while (pos < src.length()) {
            char c = src.charAt(pos);
            if (Character.isWhitespace(c) || c == '(' || c == ')' || c == '"' || c == ';') break;
            pos++;
        }
        String token = src.substring(start, pos);
        if (token.isEmpty()) throw new EvalError("empty token");
        try {
            return Long.parseLong(token);
        } catch (NumberFormatException e) {
            return new SchemeSymbol(token);
        }
    }

    // --- Eval ---

    private Object eval(Object expr, Env env) throws EvalError {
        if (expr instanceof Long || expr instanceof Boolean || expr instanceof SchemeString) {
            return expr;
        }
        if (expr instanceof SchemeSymbol sym) {
            return env.lookup(sym.name);
        }
        if (expr instanceof SchemeList list) {
            if (list.elems.isEmpty()) {
                throw new EvalError("empty application");
            }
            Object first = list.elems.get(0);
            if (first instanceof SchemeSymbol sym) {
                switch (sym.name) {
                    case "define" -> { return evalDefine(list.elems, env); }
                    case "if" -> { return evalIf(list.elems, env); }
                    case "quote" -> {
                        if (list.elems.size() != 2) throw new EvalError("quote: expected 1 argument");
                        return list.elems.get(1);
                    }
                    case "lambda" -> { return evalLambda(list.elems, env); }
                    case "and" -> { return evalAnd(list.elems, env); }
                    case "or" -> { return evalOr(list.elems, env); }
                }
            }
            // Function call: evaluate operator and arguments
            Object proc = eval(first, env);
            List<Object> args = new ArrayList<>();
            for (int i = 1; i < list.elems.size(); i++) {
                args.add(eval(list.elems.get(i), env));
            }
            return apply(proc, args);
        }
        throw new EvalError("cannot eval: " + expr);
    }

    private Object evalDefine(List<Object> elems, Env env) throws EvalError {
        if (elems.size() < 3) throw new EvalError("define: bad syntax");
        Object target = elems.get(1);
        if (target instanceof SchemeSymbol sym) {
            // (define x expr)
            Object val = eval(elems.get(2), env);
            env.define(sym.name, val);
            return null; // void
        }
        if (target instanceof SchemeList nameAndParams) {
            // (define (f x y) body)
            if (nameAndParams.elems.isEmpty()) throw new EvalError("define: bad syntax");
            String fname = ((SchemeSymbol) nameAndParams.elems.get(0)).name;
            List<String> params = new ArrayList<>();
            for (int i = 1; i < nameAndParams.elems.size(); i++) {
                params.add(((SchemeSymbol) nameAndParams.elems.get(i)).name);
            }
            Object body = elems.get(2);
            Lambda lambda = new Lambda(params, body, env);
            env.define(fname, lambda);
            return null; // void
        }
        throw new EvalError("define: bad syntax");
    }

    private Object evalIf(List<Object> elems, Env env) throws EvalError {
        if (elems.size() < 3 || elems.size() > 4) throw new EvalError("if: bad syntax");
        Object cond = eval(elems.get(1), env);
        if (isTruthy(cond)) {
            return eval(elems.get(2), env);
        } else {
            if (elems.size() == 4) {
                return eval(elems.get(3), env);
            }
            return null; // void
        }
    }

    private Lambda evalLambda(List<Object> elems, Env env) throws EvalError {
        if (elems.size() < 3) throw new EvalError("lambda: bad syntax");
        SchemeList paramList = (SchemeList) elems.get(1);
        List<String> params = new ArrayList<>();
        for (Object p : paramList.elems) {
            params.add(((SchemeSymbol) p).name);
        }
        Object body = elems.get(2);
        return new Lambda(params, body, env);
    }

    private Object evalAnd(List<Object> elems, Env env) throws EvalError {
        Object result = Boolean.TRUE;
        for (int i = 1; i < elems.size(); i++) {
            result = eval(elems.get(i), env);
            if (!isTruthy(result)) return result;
        }
        return result;
    }

    private Object evalOr(List<Object> elems, Env env) throws EvalError {
        Object result = Boolean.FALSE;
        for (int i = 1; i < elems.size(); i++) {
            result = eval(elems.get(i), env);
            if (isTruthy(result)) return result;
        }
        return result;
    }

    private boolean isTruthy(Object val) {
        return !(val instanceof Boolean b && !b);
    }

    private Object apply(Object proc, List<Object> args) throws EvalError {
        if (proc instanceof Lambda lam) {
            if (args.size() != lam.params.size()) {
                throw new EvalError("lambda: expected " + lam.params.size() + " arguments, got " + args.size());
            }
            Env callEnv = new Env(lam.closureEnv);
            for (int i = 0; i < lam.params.size(); i++) {
                callEnv.define(lam.params.get(i), args.get(i));
            }
            return eval(lam.body, callEnv);
        }
        if (proc instanceof BuiltinProc bp) {
            return bp.apply(args);
        }
        throw new EvalError("not a procedure: " + schemeToString(proc));
    }

    // --- Builtins ---

    @FunctionalInterface
    private interface BuiltinFn {
        Object apply(List<Object> args) throws EvalError;
    }

    private record BuiltinProc(String name, BuiltinFn fn) {
        Object apply(List<Object> args) throws EvalError {
            return fn.apply(args);
        }
    }

    private void registerBuiltins(Env env) {
        env.define("+", new BuiltinProc("+", args -> {
            long sum = 0;
            for (Object a : args) sum += requireLong(a, "+");
            return sum;
        }));
        env.define("-", new BuiltinProc("-", args -> {
            if (args.isEmpty()) throw new EvalError("-: need at least 1 argument");
            if (args.size() == 1) return -requireLong(args.get(0), "-");
            long result = requireLong(args.get(0), "-");
            for (int i = 1; i < args.size(); i++) result -= requireLong(args.get(i), "-");
            return result;
        }));
        env.define("*", new BuiltinProc("*", args -> {
            long product = 1;
            for (Object a : args) product *= requireLong(a, "*");
            return product;
        }));
        env.define("/", new BuiltinProc("/", args -> {
            if (args.isEmpty()) throw new EvalError("/: need at least 1 argument");
            long result = requireLong(args.get(0), "/");
            for (int i = 1; i < args.size(); i++) {
                long divisor = requireLong(args.get(i), "/");
                if (divisor == 0) throw new EvalError("division by zero");
                result /= divisor;
            }
            return result;
        }));
        env.define("<", new BuiltinProc("<", args -> {
            requireArgCount("<", args, 2);
            return requireLong(args.get(0), "<") < requireLong(args.get(1), "<");
        }));
        env.define(">", new BuiltinProc(">", args -> {
            requireArgCount(">", args, 2);
            return requireLong(args.get(0), ">") > requireLong(args.get(1), ">");
        }));
        env.define("=", new BuiltinProc("=", args -> {
            requireArgCount("=", args, 2);
            return requireLong(args.get(0), "=") == requireLong(args.get(1), "=");
        }));
        env.define("<=", new BuiltinProc("<=", args -> {
            requireArgCount("<=", args, 2);
            return requireLong(args.get(0), "<=") <= requireLong(args.get(1), "<=");
        }));
        env.define("not", new BuiltinProc("not", args -> {
            requireArgCount("not", args, 1);
            return !isTruthy(args.get(0));
        }));
    }

    private long requireLong(Object val, String op) throws EvalError {
        if (val instanceof Long l) return l;
        throw new EvalError(op + ": not a number: " + schemeToString(val));
    }

    private void requireArgCount(String name, List<Object> args, int expected) throws EvalError {
        if (args.size() != expected)
            throw new EvalError(name + ": expected " + expected + " arguments, got " + args.size());
    }

    // --- Output ---

    private String schemeToString(Object val) {
        if (val == null) return "void";
        if (val instanceof Long l) return l.toString();
        if (val instanceof Boolean b) return b ? "#t" : "#f";
        if (val instanceof SchemeString s) return "\"" + s.value + "\"";
        if (val instanceof SchemeSymbol s) return s.name;
        if (val instanceof SchemeList list) {
            StringBuilder sb = new StringBuilder("(");
            for (int i = 0; i < list.elems.size(); i++) {
                if (i > 0) sb.append(" ");
                sb.append(schemeToString(list.elems.get(i)));
            }
            sb.append(")");
            return sb.toString();
        }
        if (val instanceof Lambda) return "#<procedure>";
        if (val instanceof BuiltinProc) return "#<procedure>";
        return val.toString();
    }

    // --- Data types ---

    record SchemeSymbol(String name) {}
    record SchemeString(String value) {}
    record SchemeList(List<Object> elems) {}
}
