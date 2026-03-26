package ming;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

public class Evaluator {

    private final Env globalEnv = new Env(null);

    // Current source position for error reporting
    private int currentLine = 1;
    private int currentCol = 1;

    // Output buffer for display/write/newline
    private StringBuilder outputBuffer = new StringBuilder();

    public Evaluator() {
        // Register builtins as procedures in the global environment
        for (String name : new String[]{"+", "-", "*", "/", "<", ">", "=", "<=", ">=", "not",
                "cons", "car", "cdr", "null?", "list", "length",
                "string?", "number?", "boolean?", "pair?", "symbol?", "append",
                "display", "write", "newline",
                "string-append", "string-length", "substring",
                "string->number", "number->string",
                "symbol->string", "string->symbol",
                "string-ref", "char?",
                "string-copy", "string-set!"}) {
            globalEnv.define(name, new BuiltinProc(name));
        }
    }

    public String evalStr(String input) throws EvalError {
        List<Object> exprs = parse(input);
        Object result = null;
        for (Object expr : exprs) {
            result = eval(expr, globalEnv);
        }
        return schemeToString(result);
    }

    public EvalResult evalStrWithOutput(String input) throws EvalError {
        outputBuffer.setLength(0);
        List<Object> exprs = parse(input);
        Object result = null;
        for (Object expr : exprs) {
            result = eval(expr, globalEnv);
        }
        return new EvalResult(schemeToString(result), outputBuffer.toString());
    }

    private EvalError posError(String msg) {
        return new EvalError(currentLine + ":" + currentCol + ": " + msg);
    }

    // ---- Representation ----
    static final Object NIL = new Object() {
        @Override public String toString() { return "()"; }
    };

    static final class SchemeString {
        String value;
        SchemeString(String value) { this.value = value; }
    }

    static final class SchemeChar {
        final char value;
        SchemeChar(char value) { this.value = value; }
    }

    static final class Pair {
        Object car;
        Object cdr;
        Pair(Object car, Object cdr) { this.car = car; this.cdr = cdr; }
    }

    // Source location wrapper
    static final class Located {
        final Object datum;
        final int line;
        final int col;
        Located(Object datum, int line, int col) {
            this.datum = datum;
            this.line = line;
            this.col = col;
        }
    }

    // ---- Environment ----
    static class Env {
        private final Map<String, Object> bindings = new HashMap<>();
        private final Env parent;

        Env(Env parent) { this.parent = parent; }

        void define(String name, Object value) { bindings.put(name, value); }

        Object lookup(String name) throws EvalError {
            if (bindings.containsKey(name)) return bindings.get(name);
            if (parent != null) return parent.lookup(name);
            throw new EvalError("unbound variable: " + name);
        }
    }

    // ---- Procedure types ----
    static final class BuiltinProc {
        final String name;
        BuiltinProc(String name) { this.name = name; }
    }

    static final class Lambda {
        final List<String> params;
        final List<Object> body;
        final Env closureEnv;
        Lambda(List<String> params, List<Object> body, Env closureEnv) {
            this.params = params;
            this.body = body;
            this.closureEnv = closureEnv;
        }
    }

    // ---- Token with position ----
    private static final class Token {
        final String value;
        final int line;
        final int col;
        Token(String value, int line, int col) {
            this.value = value;
            this.line = line;
            this.col = col;
        }
    }

    // ---- Tokenizer ----

    private List<Token> tokenize(String input) {
        List<Token> tokens = new ArrayList<>();
        int i = 0;
        int len = input.length();
        int line = 1;
        int col = 1;
        while (i < len) {
            char c = input.charAt(i);
            if (c == '\n') { i++; line++; col = 1; continue; }
            if (Character.isWhitespace(c)) { i++; col++; continue; }
            if (c == ';') {
                while (i < len && input.charAt(i) != '\n') { i++; col++; }
                continue;
            }
            int startLine = line;
            int startCol = col;
            if (c == '(') { tokens.add(new Token("(", startLine, startCol)); i++; col++; continue; }
            if (c == ')') { tokens.add(new Token(")", startLine, startCol)); i++; col++; continue; }
            if (c == '\'') { tokens.add(new Token("'", startLine, startCol)); i++; col++; continue; }
            if (c == '"') {
                StringBuilder sb = new StringBuilder();
                sb.append('"');
                i++; col++;
                while (i < len && input.charAt(i) != '"') {
                    if (input.charAt(i) == '\\') {
                        sb.append(input.charAt(i)); i++; col++;
                        if (i < len) { sb.append(input.charAt(i)); i++; col++; }
                    } else {
                        if (input.charAt(i) == '\n') { line++; col = 1; } else { col++; }
                        sb.append(input.charAt(i)); i++;
                    }
                }
                if (i < len) { sb.append('"'); i++; col++; }
                tokens.add(new Token(sb.toString(), startLine, startCol));
                continue;
            }
            StringBuilder sb = new StringBuilder();
            while (i < len) {
                char ch = input.charAt(i);
                if (Character.isWhitespace(ch) || ch == '(' || ch == ')' || ch == '"' || ch == ';') break;
                sb.append(ch);
                i++; col++;
            }
            tokens.add(new Token(sb.toString(), startLine, startCol));
        }
        return tokens;
    }

    // ---- Parser ----

    private int pos;

    private List<Object> parse(String input) throws EvalError {
        List<Token> tokens = tokenize(input);
        pos = 0;
        List<Object> exprs = new ArrayList<>();
        while (pos < tokens.size()) {
            exprs.add(parseExpr(tokens));
        }
        return exprs;
    }

    private Object parseExpr(List<Token> tokens) throws EvalError {
        if (pos >= tokens.size()) throw new EvalError("unexpected end of input");
        Token token = tokens.get(pos++);

        if (token.value.equals("(")) {
            List<Object> list = new ArrayList<>();
            while (pos < tokens.size() && !tokens.get(pos).value.equals(")")) {
                list.add(parseExpr(tokens));
            }
            if (pos >= tokens.size()) throw new EvalError("missing closing parenthesis");
            pos++;
            return new Located(list, token.line, token.col);
        }
        if (token.value.equals(")")) throw new EvalError("unexpected )");
        if (token.value.equals("'")) {
            Object quoted = parseExpr(tokens);
            List<Object> q = new ArrayList<>();
            q.add("quote");
            q.add(quoted);
            return new Located(q, token.line, token.col);
        }
        Object atom = parseAtom(token.value);
        return new Located(atom, token.line, token.col);
    }

    private Object parseAtom(String token) {
        if (token.equals("#t")) return Boolean.TRUE;
        if (token.equals("#f")) return Boolean.FALSE;
        if (token.startsWith("#\\")) {
            String charName = token.substring(2);
            if (charName.equals("space")) return new SchemeChar(' ');
            if (charName.equals("newline")) return new SchemeChar('\n');
            if (charName.equals("tab")) return new SchemeChar('\t');
            if (charName.length() == 1) return new SchemeChar(charName.charAt(0));
            throw new RuntimeException("unknown character literal: " + token);
        }
        if (token.startsWith("\"") && token.endsWith("\"")) {
            String s = token.substring(1, token.length() - 1);
            s = s.replace("\\n", "\n").replace("\\t", "\t").replace("\\\\", "\\").replace("\\\"", "\"");
            return new SchemeString(s);
        }
        try { return Long.parseLong(token); } catch (NumberFormatException ignored) {}
        return token; // symbol
    }

    // Unwrap Located to get raw datum
    private Object unwrap(Object expr) {
        if (expr instanceof Located loc) return loc.datum;
        return expr;
    }

    // ---- Evaluator ----

    @SuppressWarnings("unchecked")
    private Object eval(Object expr, Env env) throws EvalError {
        // Unwrap Located and update current position
        if (expr instanceof Located loc) {
            currentLine = loc.line;
            currentCol = loc.col;
            expr = loc.datum;
        }

        if (expr instanceof Long || expr instanceof Boolean || expr instanceof SchemeString || expr instanceof SchemeChar) {
            return expr;
        }
        if (expr instanceof String sym) {
            try {
                return env.lookup(sym);
            } catch (EvalError e) {
                throw posError("unbound variable: " + sym);
            }
        }
        if (expr instanceof List<?> list) {
            if (list.isEmpty()) throw posError("empty application");
            Object head = list.get(0);
            Object rawHead = unwrap(head);

            if (rawHead instanceof String op) {
                switch (op) {
                    case "quote" -> {
                        if (list.size() != 2) throw posError("quote: expected 1 argument");
                        return quoteDatum(list.get(1));
                    }
                    case "if" -> {
                        if (list.size() < 3 || list.size() > 4)
                            throw posError("if: expected 2 or 3 arguments");
                        Object cond = eval(list.get(1), env);
                        if (!isFalse(cond)) {
                            return eval(list.get(2), env);
                        } else if (list.size() == 4) {
                            return eval(list.get(3), env);
                        }
                        return null; // void
                    }
                    case "define" -> {
                        if (list.size() < 3) throw posError("define: bad syntax");
                        Object target = unwrap(list.get(1));
                        if (target instanceof String name) {
                            Object val = eval(list.get(2), env);
                            env.define(name, val);
                            return null;
                        }
                        if (target instanceof List<?> sig) {
                            // (define (f params...) body...)
                            if (sig.isEmpty()) throw posError("define: bad syntax");
                            String name = (String) unwrap(sig.get(0));
                            List<String> params = new ArrayList<>();
                            for (int i = 1; i < sig.size(); i++) {
                                params.add((String) unwrap(sig.get(i)));
                            }
                            List<Object> body = new ArrayList<>();
                            for (int i = 2; i < list.size(); i++) {
                                body.add(list.get(i));
                            }
                            env.define(name, new Lambda(params, body, env));
                            return null;
                        }
                        throw posError("define: bad syntax");
                    }
                    case "lambda" -> {
                        if (list.size() < 3) throw posError("lambda: bad syntax");
                        Object paramListRaw = unwrap(list.get(1));
                        List<?> paramList = (List<?>) paramListRaw;
                        List<String> params = new ArrayList<>();
                        for (Object p : paramList) params.add((String) unwrap(p));
                        List<Object> body = new ArrayList<>();
                        for (int i = 2; i < list.size(); i++) body.add(list.get(i));
                        return new Lambda(params, body, env);
                    }
                    case "and" -> { return evalAnd((List<Object>) list, env); }
                    case "or" -> { return evalOr((List<Object>) list, env); }
                    case "begin" -> {
                        Object result2 = null;
                        for (int i = 1; i < list.size(); i++) {
                            result2 = eval(list.get(i), env);
                        }
                        return result2;
                    }
                    case "let" -> {
                        if (list.size() < 3) throw posError("let: bad syntax");
                        Object second = unwrap(list.get(1));
                        if (second instanceof String namedLetName) {
                            // Named let: (let name ((var init) ...) body...)
                            if (list.size() < 4) throw posError("let: bad syntax");
                            Object bindingsRaw = unwrap(list.get(2));
                            List<?> bindings = (List<?>) bindingsRaw;
                            List<String> params = new ArrayList<>();
                            List<Object> inits = new ArrayList<>();
                            for (Object b : bindings) {
                                List<?> binding = (List<?>) unwrap(b);
                                params.add((String) unwrap(binding.get(0)));
                                inits.add(binding.get(1));
                            }
                            List<Object> body2 = new ArrayList<>();
                            for (int i = 3; i < list.size(); i++) body2.add(list.get(i));
                            Env letEnv = new Env(env);
                            Lambda loopLam = new Lambda(params, body2, letEnv);
                            letEnv.define(namedLetName, loopLam);
                            List<Object> args2 = new ArrayList<>();
                            for (Object init : inits) args2.add(eval(init, env));
                            return apply(loopLam, args2);
                        }
                        List<?> bindings = (List<?>) second;
                        Env letEnv = new Env(env);
                        for (Object b : bindings) {
                            List<?> binding = (List<?>) unwrap(b);
                            String bname = (String) unwrap(binding.get(0));
                            Object bval = eval(binding.get(1), env);
                            letEnv.define(bname, bval);
                        }
                        Object result2 = null;
                        for (int i = 2; i < list.size(); i++) {
                            result2 = eval(list.get(i), letEnv);
                        }
                        return result2;
                    }
                    case "cond" -> {
                        for (int i = 1; i < list.size(); i++) {
                            List<?> clause = (List<?>) unwrap(list.get(i));
                            Object clauseHead = unwrap(clause.get(0));
                            if (clauseHead instanceof String s && s.equals("else")) {
                                Object result2 = null;
                                for (int j = 1; j < clause.size(); j++) {
                                    result2 = eval(clause.get(j), env);
                                }
                                return result2;
                            }
                            Object test = eval(clause.get(0), env);
                            if (!isFalse(test)) {
                                if (clause.size() == 1) return test;
                                Object result2 = null;
                                for (int j = 1; j < clause.size(); j++) {
                                    result2 = eval(clause.get(j), env);
                                }
                                return result2;
                            }
                        }
                        return null; // void if no clause matches
                    }
                }
            }

            // General application
            // Save position of the call expression before evaluating subexpressions
            int callLine = currentLine;
            int callCol = currentCol;
            Object proc = eval(head, env);
            List<Object> args = new ArrayList<>();
            for (int i = 1; i < list.size(); i++) {
                args.add(eval(list.get(i), env));
            }
            // Restore call position for error reporting in apply
            currentLine = callLine;
            currentCol = callCol;
            return apply(proc, args);
        }
        throw posError("unknown expression type");
    }

    private Object quoteDatum(Object datum) {
        datum = unwrap(datum);
        if (datum instanceof List<?> list) {
            if (list.isEmpty()) return NIL;
            Object result = NIL;
            for (int i = list.size() - 1; i >= 0; i--) {
                result = new Pair(quoteDatum(list.get(i)), result);
            }
            return result;
        }
        return datum;
    }

    private Object apply(Object proc, List<Object> args) throws EvalError {
        if (proc instanceof BuiltinProc bp) {
            return applyBuiltin(bp.name, args);
        }
        if (proc instanceof Lambda lam) {
            if (args.size() != lam.params.size()) {
                throw posError("wrong number of arguments: expected " + lam.params.size() + ", got " + args.size());
            }
            Env callEnv = new Env(lam.closureEnv);
            for (int i = 0; i < lam.params.size(); i++) {
                callEnv.define(lam.params.get(i), args.get(i));
            }
            Object result = null;
            for (Object bodyExpr : lam.body) {
                result = eval(bodyExpr, callEnv);
            }
            return result;
        }
        throw posError("not a procedure: " + schemeToString(proc));
    }

    private Object evalAnd(List<Object> expr, Env env) throws EvalError {
        Object result = Boolean.TRUE;
        for (int i = 1; i < expr.size(); i++) {
            result = eval(expr.get(i), env);
            if (isFalse(result)) return result;
        }
        return result;
    }

    private Object evalOr(List<Object> expr, Env env) throws EvalError {
        Object result = Boolean.FALSE;
        for (int i = 1; i < expr.size(); i++) {
            result = eval(expr.get(i), env);
            if (!isFalse(result)) return result;
        }
        return result;
    }

    private boolean isFalse(Object val) {
        return val instanceof Boolean b && !b;
    }

    private Object applyBuiltin(String name, List<Object> args) throws EvalError {
        return switch (name) {
            case "+" -> {
                long sum = 0;
                for (Object a : args) sum += requireLong(a, "+");
                yield sum;
            }
            case "-" -> {
                if (args.isEmpty()) throw posError("-: need at least 1 argument");
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
                if (args.isEmpty()) throw posError("/: need at least 1 argument");
                long result = requireLong(args.get(0), "/");
                for (int i = 1; i < args.size(); i++) {
                    long divisor = requireLong(args.get(i), "/");
                    if (divisor == 0) throw posError("division by zero");
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
            case "not" -> {
                requireArgCount(args, 1, "not");
                yield isFalse(args.get(0));
            }
            case "cons" -> {
                requireArgCount(args, 2, "cons");
                yield new Pair(args.get(0), args.get(1));
            }
            case "car" -> {
                requireArgCount(args, 1, "car");
                if (!(args.get(0) instanceof Pair p)) throw posError("car: expected pair");
                yield p.car;
            }
            case "cdr" -> {
                requireArgCount(args, 1, "cdr");
                if (!(args.get(0) instanceof Pair p)) throw posError("cdr: expected pair");
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
                long count = 0;
                Object cur = args.get(0);
                while (cur instanceof Pair p) {
                    count++;
                    cur = p.cdr;
                }
                if (cur != NIL) throw posError("length: not a proper list");
                yield count;
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
            case "append" -> {
                if (args.isEmpty()) yield NIL;
                Object result = args.get(args.size() - 1);
                for (int i = args.size() - 2; i >= 0; i--) {
                    Object lst = args.get(i);
                    List<Object> elems = new ArrayList<>();
                    Object cur = lst;
                    while (cur instanceof Pair p) {
                        elems.add(p.car);
                        cur = p.cdr;
                    }
                    for (int j = elems.size() - 1; j >= 0; j--) {
                        result = new Pair(elems.get(j), result);
                    }
                }
                yield result;
            }
            case "display" -> {
                requireArgCount(args, 1, "display");
                outputBuffer.append(displayString(args.get(0)));
                yield null; // void
            }
            case "write" -> {
                requireArgCount(args, 1, "write");
                outputBuffer.append(schemeToString(args.get(0)));
                yield null;
            }
            case "newline" -> {
                outputBuffer.append("\n");
                yield null;
            }
            case "string-append" -> {
                StringBuilder sb = new StringBuilder();
                for (Object a : args) {
                    if (!(a instanceof SchemeString s)) throw posError("string-append: expected string");
                    sb.append(s.value);
                }
                yield new SchemeString(sb.toString());
            }
            case "string-length" -> {
                requireArgCount(args, 1, "string-length");
                if (!(args.get(0) instanceof SchemeString s)) throw posError("string-length: expected string");
                yield (long) s.value.length();
            }
            case "substring" -> {
                if (args.size() < 2 || args.size() > 3) throw posError("substring: expected 2 or 3 arguments");
                if (!(args.get(0) instanceof SchemeString s)) throw posError("substring: expected string");
                int start = (int) requireLong(args.get(1), "substring");
                int end = args.size() == 3 ? (int) requireLong(args.get(2), "substring") : s.value.length();
                yield new SchemeString(s.value.substring(start, end));
            }
            case "string->number" -> {
                requireArgCount(args, 1, "string->number");
                if (!(args.get(0) instanceof SchemeString s)) throw posError("string->number: expected string");
                try {
                    yield Long.parseLong(s.value);
                } catch (NumberFormatException e) {
                    yield Boolean.FALSE;
                }
            }
            case "number->string" -> {
                requireArgCount(args, 1, "number->string");
                yield new SchemeString(String.valueOf(requireLong(args.get(0), "number->string")));
            }
            case "symbol->string" -> {
                requireArgCount(args, 1, "symbol->string");
                if (!(args.get(0) instanceof String s)) throw posError("symbol->string: expected symbol");
                yield new SchemeString(s);
            }
            case "string->symbol" -> {
                requireArgCount(args, 1, "string->symbol");
                if (!(args.get(0) instanceof SchemeString s)) throw posError("string->symbol: expected string");
                yield s.value; // symbols are plain strings
            }
            case "string-ref" -> {
                requireArgCount(args, 2, "string-ref");
                if (!(args.get(0) instanceof SchemeString s)) throw posError("string-ref: expected string");
                int idx = (int) requireLong(args.get(1), "string-ref");
                yield new SchemeChar(s.value.charAt(idx));
            }
            case "char?" -> {
                requireArgCount(args, 1, "char?");
                yield args.get(0) instanceof SchemeChar;
            }
            case "string-copy" -> {
                requireArgCount(args, 1, "string-copy");
                if (!(args.get(0) instanceof SchemeString s)) throw posError("string-copy: expected string");
                yield new SchemeString(s.value);
            }
            case "string-set!" -> {
                requireArgCount(args, 3, "string-set!");
                if (!(args.get(0) instanceof SchemeString s)) throw posError("string-set!: expected string");
                int idx = (int) requireLong(args.get(1), "string-set!");
                if (!(args.get(2) instanceof SchemeChar c)) throw posError("string-set!: expected char");
                char[] chars = s.value.toCharArray();
                chars[idx] = c.value;
                s.value = new String(chars);
                yield null; // void
            }
            default -> throw posError("unbound variable: " + name);
        };
    }

    private long requireLong(Object val, String context) throws EvalError {
        if (val instanceof Long l) return l;
        throw posError(context + ": expected number, got " + schemeToString(val));
    }

    private void requireArgCount(List<Object> args, int expected, String name) throws EvalError {
        if (args.size() != expected) {
            throw posError(name + ": expected " + expected + " arguments, got " + args.size());
        }
    }

    // ---- Output formatting ----

    // display format: strings without quotes
    private String displayString(Object val) {
        if (val instanceof SchemeString s) return s.value;
        if (val instanceof SchemeChar c) return String.valueOf(c.value);
        return schemeToString(val);
    }

    @SuppressWarnings("unchecked")
    private String schemeToString(Object val) {
        if (val == null) return ""; // void
        if (val == NIL) return "()";
        if (val instanceof Long l) return l.toString();
        if (val instanceof Boolean b) return b ? "#t" : "#f";
        if (val instanceof SchemeString s) return "\"" + s.value + "\"";
        if (val instanceof SchemeChar c) return "#\\" + c.value;
        if (val instanceof String s) return s;
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
                sb.append(" . ").append(schemeToString(cur));
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
        return val.toString();
    }
}
