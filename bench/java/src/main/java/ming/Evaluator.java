package ming;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

public class Evaluator {

    private static final Object VOID = new Object() {
        @Override public String toString() { return "#<void>"; }
    };

    // Scheme empty list
    static final Object NIL = new Object() {
        @Override public String toString() { return "()"; }
    };

    // Scheme pair (cons cell)
    static class Pair {
        Object car;
        Object cdr;
        Pair(Object car, Object cdr) {
            this.car = car;
            this.cdr = cdr;
        }
    }

    public String evalStr(String input) throws EvalError {
        List<Object> tokens = tokenize(input);
        int[] pos = {0};
        Env env = createGlobalEnv();
        Object lastResult = null;
        while (pos[0] < tokens.size()) {
            Object expr = parse(tokens, pos);
            lastResult = eval(expr, env);
        }
        if (lastResult == null) {
            throw new EvalError("no expression");
        }
        if (lastResult == VOID) {
            throw new EvalError("no expression");
        }
        return schemeToString(lastResult);
    }

    public EvalResult evalStrWithOutput(String input) throws EvalError {
        throw new EvalError("not implemented");
    }

    private Env createGlobalEnv() {
        return new Env(null);
    }

    // --- Environment ---

    private static class Env {
        final Env parent;
        final Map<String, Object> bindings = new HashMap<>();

        Env(Env parent) {
            this.parent = parent;
        }

        Object lookup(String name) throws EvalError {
            if (bindings.containsKey(name)) return bindings.get(name);
            if (parent != null) return parent.lookup(name);
            throw new EvalError("unbound variable: " + name);
        }

        void define(String name, Object value) {
            bindings.put(name, value);
        }
    }

    // --- Lambda ---

    private static class Lambda {
        final List<String> params;
        final Object body;
        final Env closure;

        Lambda(List<String> params, Object body, Env closure) {
            this.params = params;
            this.body = body;
            this.closure = closure;
        }
    }

    // --- Tokenizer ---

    private List<Object> tokenize(String input) throws EvalError {
        List<Object> tokens = new ArrayList<>();
        int i = 0;
        int len = input.length();
        while (i < len) {
            char c = input.charAt(i);
            if (Character.isWhitespace(c)) {
                i++;
            } else if (c == ';') {
                while (i < len && input.charAt(i) != '\n') i++;
            } else if (c == '(') {
                tokens.add("(");
                i++;
            } else if (c == ')') {
                tokens.add(")");
                i++;
            } else if (c == '\'') {
                tokens.add("'");
                i++;
            } else if (c == '"') {
                StringBuilder sb = new StringBuilder();
                i++; // skip opening quote
                while (i < len && input.charAt(i) != '"') {
                    if (input.charAt(i) == '\\') {
                        i++;
                        if (i < len) {
                            char esc = input.charAt(i);
                            switch (esc) {
                                case 'n' -> sb.append('\n');
                                case 't' -> sb.append('\t');
                                case '\\' -> sb.append('\\');
                                case '"' -> sb.append('"');
                                default -> { sb.append('\\'); sb.append(esc); }
                            }
                        }
                    } else {
                        sb.append(input.charAt(i));
                    }
                    i++;
                }
                if (i >= len) throw new EvalError("unterminated string");
                i++; // skip closing quote
                tokens.add(new SchemeString(sb.toString()));
            } else if (c == '#') {
                if (i + 1 < len) {
                    char next = input.charAt(i + 1);
                    if (next == 't') {
                        tokens.add(Boolean.TRUE);
                        i += 2;
                    } else if (next == 'f') {
                        tokens.add(Boolean.FALSE);
                        i += 2;
                    } else {
                        throw new EvalError("unknown token: #" + next);
                    }
                } else {
                    throw new EvalError("unexpected end of input after #");
                }
            } else {
                // symbol or number
                StringBuilder sb = new StringBuilder();
                while (i < len && !Character.isWhitespace(input.charAt(i))
                        && input.charAt(i) != '(' && input.charAt(i) != ')'
                        && input.charAt(i) != '"' && input.charAt(i) != ';'
                        && input.charAt(i) != '\'') {
                    sb.append(input.charAt(i));
                    i++;
                }
                String tok = sb.toString();
                try {
                    tokens.add(Long.parseLong(tok));
                } catch (NumberFormatException e) {
                    tokens.add(tok); // symbol
                }
            }
        }
        return tokens;
    }

    // --- Parser ---

    private Object parse(List<Object> tokens, int[] pos) throws EvalError {
        if (pos[0] >= tokens.size()) {
            throw new EvalError("unexpected end of input");
        }
        Object tok = tokens.get(pos[0]);
        pos[0]++;

        if (tok.equals("'")) {
            Object quoted = parse(tokens, pos);
            List<Object> quoteExpr = new ArrayList<>();
            quoteExpr.add("quote");
            quoteExpr.add(quoted);
            return quoteExpr;
        }

        if (tok.equals("(")) {
            List<Object> list = new ArrayList<>();
            while (pos[0] < tokens.size() && !tokens.get(pos[0]).equals(")")) {
                list.add(parse(tokens, pos));
            }
            if (pos[0] >= tokens.size()) {
                throw new EvalError("missing closing parenthesis");
            }
            pos[0]++; // skip ')'
            return list;
        } else if (tok.equals(")")) {
            throw new EvalError("unexpected )");
        } else {
            return tok;
        }
    }

    // Convert a parsed List<Object> to Scheme pair structure (for quote)
    private Object listToScheme(Object parsed) {
        if (parsed instanceof List<?> list) {
            Object result = NIL;
            for (int i = list.size() - 1; i >= 0; i--) {
                result = new Pair(listToScheme(list.get(i)), result);
            }
            return result;
        }
        return parsed;
    }

    // --- Evaluator ---

    @SuppressWarnings("unchecked")
    private Object eval(Object expr, Env env) throws EvalError {
        if (expr instanceof Long || expr instanceof Boolean || expr instanceof SchemeString) {
            return expr;
        }
        if (expr instanceof String sym) {
            if (isPrimitive(sym)) {
                return sym;
            }
            return env.lookup(sym);
        }
        if (expr instanceof List<?> list) {
            if (list.isEmpty()) {
                throw new EvalError("empty application");
            }
            Object head = list.get(0);

            // Special forms
            if (head instanceof String s) {
                switch (s) {
                    case "define" -> {
                        if (list.size() < 3) throw new EvalError("define: bad syntax");
                        Object target = list.get(1);
                        if (target instanceof String name) {
                            Object val = eval(list.get(2), env);
                            env.define(name, val);
                            return VOID;
                        } else if (target instanceof List<?> sig) {
                            if (sig.isEmpty() || !(sig.get(0) instanceof String fname)) {
                                throw new EvalError("define: bad syntax");
                            }
                            List<String> params = new ArrayList<>();
                            for (int i = 1; i < sig.size(); i++) {
                                if (!(sig.get(i) instanceof String p)) {
                                    throw new EvalError("define: parameter must be a symbol");
                                }
                                params.add(p);
                            }
                            Object body;
                            if (list.size() == 3) {
                                body = list.get(2);
                            } else {
                                List<Object> beginBody = new ArrayList<>();
                                beginBody.add("begin");
                                for (int i = 2; i < list.size(); i++) {
                                    beginBody.add(list.get(i));
                                }
                                body = beginBody;
                            }
                            Lambda lambda = new Lambda(params, body, env);
                            env.define(fname, lambda);
                            return VOID;
                        }
                        throw new EvalError("define: bad syntax");
                    }
                    case "if" -> {
                        if (list.size() < 3 || list.size() > 4) {
                            throw new EvalError("if: bad syntax");
                        }
                        Object cond = eval(list.get(1), env);
                        if (!isFalse(cond)) {
                            return eval(list.get(2), env);
                        } else if (list.size() == 4) {
                            return eval(list.get(3), env);
                        }
                        return VOID;
                    }
                    case "quote" -> {
                        if (list.size() != 2) throw new EvalError("quote: expected 1 argument");
                        return listToScheme(list.get(1));
                    }
                    case "lambda" -> {
                        if (list.size() < 3) throw new EvalError("lambda: bad syntax");
                        Object paramsExpr = list.get(1);
                        if (!(paramsExpr instanceof List<?> paramList)) {
                            throw new EvalError("lambda: parameters must be a list");
                        }
                        List<String> params = new ArrayList<>();
                        for (Object p : paramList) {
                            if (!(p instanceof String ps)) {
                                throw new EvalError("lambda: parameter must be a symbol");
                            }
                            params.add(ps);
                        }
                        Object body;
                        if (list.size() == 3) {
                            body = list.get(2);
                        } else {
                            List<Object> beginBody = new ArrayList<>();
                            beginBody.add("begin");
                            for (int i = 2; i < list.size(); i++) {
                                beginBody.add(list.get(i));
                            }
                            body = beginBody;
                        }
                        return new Lambda(params, body, env);
                    }
                    case "and" -> {
                        Object result = Boolean.TRUE;
                        for (int i = 1; i < list.size(); i++) {
                            result = eval(list.get(i), env);
                            if (isFalse(result)) return result;
                        }
                        return result;
                    }
                    case "or" -> {
                        Object result = Boolean.FALSE;
                        for (int i = 1; i < list.size(); i++) {
                            result = eval(list.get(i), env);
                            if (!isFalse(result)) return result;
                        }
                        return result;
                    }
                    case "let" -> {
                        return evalLet(list, env);
                    }
                    case "begin" -> {
                        Object result = VOID;
                        for (int i = 1; i < list.size(); i++) {
                            result = eval(list.get(i), env);
                        }
                        return result;
                    }
                    case "cond" -> {
                        return evalCond(list, env);
                    }
                }
            }

            // Procedure call
            Object proc = eval(head, env);
            List<Object> args = new ArrayList<>();
            for (int i = 1; i < list.size(); i++) {
                args.add(eval(list.get(i), env));
            }
            return apply(proc, args);
        }
        throw new EvalError("cannot evaluate: " + expr);
    }

    private Object apply(Object proc, List<Object> args) throws EvalError {
        if (proc instanceof Lambda lambda) {
            if (args.size() != lambda.params.size()) {
                throw new EvalError("wrong number of arguments: expected " + lambda.params.size() + ", got " + args.size());
            }
            Env callEnv = new Env(lambda.closure);
            for (int i = 0; i < lambda.params.size(); i++) {
                callEnv.define(lambda.params.get(i), args.get(i));
            }
            return eval(lambda.body, callEnv);
        }
        if (proc instanceof String p && isPrimitive(p)) {
            return applyPrimitive(p, args);
        }
        throw new EvalError("not a procedure: " + schemeToString(proc));
    }

    private Object evalLet(List<?> list, Env env) throws EvalError {
        // (let ((x 1) (y 2)) body...)
        // (let name ((x 1) (y 2)) body...)  -- named let
        int idx = 1;
        String name = null;
        if (list.get(1) instanceof String n) {
            name = n;
            idx = 2;
        }
        if (!(list.get(idx) instanceof List<?> bindings)) {
            throw new EvalError("let: bad syntax");
        }
        List<String> paramNames = new ArrayList<>();
        List<Object> initExprs = new ArrayList<>();
        for (Object b : bindings) {
            if (!(b instanceof List<?> binding) || binding.size() != 2) {
                throw new EvalError("let: bad binding");
            }
            if (!(binding.get(0) instanceof String pname)) {
                throw new EvalError("let: binding name must be a symbol");
            }
            paramNames.add(pname);
            initExprs.add(binding.get(1));
        }
        // Evaluate init expressions in outer env
        List<Object> initVals = new ArrayList<>();
        for (Object e : initExprs) {
            initVals.add(eval(e, env));
        }
        // Build body
        Object body;
        if (list.size() - idx - 1 == 1) {
            body = list.get(idx + 1);
        } else {
            List<Object> beginBody = new ArrayList<>();
            beginBody.add("begin");
            for (int i = idx + 1; i < list.size(); i++) {
                beginBody.add(list.get(i));
            }
            body = beginBody;
        }
        if (name != null) {
            // Named let: create a lambda and bind it to name, then call it
            Env letEnv = new Env(env);
            Lambda lambda = new Lambda(paramNames, body, letEnv);
            letEnv.define(name, lambda);
            for (int i = 0; i < paramNames.size(); i++) {
                letEnv.define(paramNames.get(i), initVals.get(i));
            }
            return eval(body, letEnv);
        } else {
            Env letEnv = new Env(env);
            for (int i = 0; i < paramNames.size(); i++) {
                letEnv.define(paramNames.get(i), initVals.get(i));
            }
            return eval(body, letEnv);
        }
    }

    private Object evalCond(List<?> list, Env env) throws EvalError {
        for (int i = 1; i < list.size(); i++) {
            if (!(list.get(i) instanceof List<?> clause) || clause.isEmpty()) {
                throw new EvalError("cond: bad clause");
            }
            Object test = clause.get(0);
            if (test instanceof String s && s.equals("else")) {
                // else clause - evaluate body
                Object result = VOID;
                for (int j = 1; j < clause.size(); j++) {
                    result = eval(clause.get(j), env);
                }
                return result;
            }
            Object testVal = eval(test, env);
            if (!isFalse(testVal)) {
                if (clause.size() == 1) return testVal;
                Object result = VOID;
                for (int j = 1; j < clause.size(); j++) {
                    result = eval(clause.get(j), env);
                }
                return result;
            }
        }
        return VOID;
    }

    private static final java.util.Set<String> PRIMITIVES = java.util.Set.of(
            "+", "-", "*", "/", "<", ">", "=", "<=", ">=",
            "cons", "car", "cdr", "null?", "list", "length", "append",
            "not", "string?", "number?", "boolean?", "pair?", "symbol?"
    );

    private boolean isPrimitive(String name) {
        return PRIMITIVES.contains(name);
    }

    private Object applyPrimitive(String proc, List<Object> args) throws EvalError {
        return switch (proc) {
            case "+" -> {
                long sum = 0;
                for (Object a : args) sum += requireLong(a, "+");
                yield sum;
            }
            case "-" -> {
                if (args.isEmpty()) throw new EvalError("-: expected at least 1 argument");
                if (args.size() == 1) yield -requireLong(args.get(0), "-");
                long result = requireLong(args.get(0), "-");
                for (int i = 1; i < args.size(); i++) result -= requireLong(args.get(i), "-");
                yield result;
            }
            case "*" -> {
                long product = 1;
                for (Object a : args) product *= requireLong(a, "*");
                yield product;
            }
            case "/" -> {
                if (args.isEmpty()) throw new EvalError("/: expected at least 1 argument");
                long result = requireLong(args.get(0), "/");
                for (int i = 1; i < args.size(); i++) {
                    long divisor = requireLong(args.get(i), "/");
                    if (divisor == 0) throw new EvalError("division by zero");
                    result /= divisor;
                }
                yield result;
            }
            case "<" -> {
                requireArgCount(args, 2, "<");
                yield requireLong(args.get(0), "<") < requireLong(args.get(1), "<");
            }
            case ">" -> {
                requireArgCount(args, 2, ">");
                yield requireLong(args.get(0), ">") > requireLong(args.get(1), ">");
            }
            case "=" -> {
                requireArgCount(args, 2, "=");
                yield requireLong(args.get(0), "=") == requireLong(args.get(1), "=");
            }
            case "<=" -> {
                requireArgCount(args, 2, "<=");
                yield requireLong(args.get(0), "<=") <= requireLong(args.get(1), "<=");
            }
            case ">=" -> {
                requireArgCount(args, 2, ">=");
                yield requireLong(args.get(0), ">=") >= requireLong(args.get(1), ">=");
            }
            case "cons" -> {
                requireArgCount(args, 2, "cons");
                yield new Pair(args.get(0), args.get(1));
            }
            case "car" -> {
                requireArgCount(args, 1, "car");
                if (!(args.get(0) instanceof Pair p)) throw new EvalError("car: not a pair");
                yield p.car;
            }
            case "cdr" -> {
                requireArgCount(args, 1, "cdr");
                if (!(args.get(0) instanceof Pair p)) throw new EvalError("cdr: not a pair");
                yield p.cdr;
            }
            case "null?" -> {
                requireArgCount(args, 1, "null?");
                yield args.get(0) == NIL;
            }
            case "list" -> {
                Object result = NIL;
                for (int i = args.size() - 1; i >= 0; i--) {
                    result = new Pair(args.get(i), result);
                }
                yield result;
            }
            case "length" -> {
                requireArgCount(args, 1, "length");
                Object lst = args.get(0);
                long len = 0;
                while (lst instanceof Pair p) {
                    len++;
                    lst = p.cdr;
                }
                if (lst != NIL) throw new EvalError("length: not a proper list");
                yield len;
            }
            case "append" -> {
                if (args.isEmpty()) yield NIL;
                // Append all lists
                Object result = args.get(args.size() - 1);
                for (int i = args.size() - 2; i >= 0; i--) {
                    result = appendTwo(args.get(i), result);
                }
                yield result;
            }
            case "not" -> {
                requireArgCount(args, 1, "not");
                yield isFalse(args.get(0)) ? Boolean.TRUE : Boolean.FALSE;
            }
            case "string?" -> {
                requireArgCount(args, 1, "string?");
                yield args.get(0) instanceof SchemeString;
            }
            case "number?" -> {
                requireArgCount(args, 1, "number?");
                yield args.get(0) instanceof Long;
            }
            case "boolean?" -> {
                requireArgCount(args, 1, "boolean?");
                yield args.get(0) instanceof Boolean;
            }
            case "pair?" -> {
                requireArgCount(args, 1, "pair?");
                yield args.get(0) instanceof Pair;
            }
            case "symbol?" -> {
                requireArgCount(args, 1, "symbol?");
                yield args.get(0) instanceof String;
            }
            default -> throw new EvalError("unbound variable: " + proc);
        };
    }

    private Object appendTwo(Object a, Object b) throws EvalError {
        if (a == NIL) return b;
        if (!(a instanceof Pair p)) throw new EvalError("append: not a proper list");
        return new Pair(p.car, appendTwo(p.cdr, b));
    }

    private boolean isFalse(Object val) {
        return val instanceof Boolean b && !b;
    }

    private long requireLong(Object val, String context) throws EvalError {
        if (val instanceof Long l) return l;
        throw new EvalError(context + ": expected number, got " + schemeToString(val));
    }

    private void requireArgCount(List<Object> args, int expected, String context) throws EvalError {
        if (args.size() != expected) {
            throw new EvalError(context + ": expected " + expected + " arguments, got " + args.size());
        }
    }

    // --- Output formatting ---

    private String schemeToString(Object val) {
        if (val instanceof Long l) return l.toString();
        if (val instanceof Boolean b) return b ? "#t" : "#f";
        if (val instanceof SchemeString s) return "\"" + s.value() + "\"";
        if (val == NIL) return "()";
        if (val instanceof Pair) {
            StringBuilder sb = new StringBuilder("(");
            Object cur = val;
            boolean first = true;
            while (cur instanceof Pair p) {
                if (!first) sb.append(" ");
                first = false;
                sb.append(schemeToString(p.car));
                cur = p.cdr;
            }
            if (cur != NIL) {
                sb.append(" . ");
                sb.append(schemeToString(cur));
            }
            sb.append(")");
            return sb.toString();
        }
        if (val instanceof List<?> list) {
            StringBuilder sb = new StringBuilder("(");
            for (int i = 0; i < list.size(); i++) {
                if (i > 0) sb.append(" ");
                sb.append(schemeToString(list.get(i)));
            }
            sb.append(")");
            return sb.toString();
        }
        if (val instanceof Lambda) return "#<procedure>";
        if (val == VOID) return "#<void>";
        return val.toString();
    }

    // Internal type for Scheme strings (to distinguish from symbols which are Java Strings)
    record SchemeString(String value) {}
}
