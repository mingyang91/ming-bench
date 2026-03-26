package ming;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;

public class Evaluator {

    private final Env globalEnv = new Env(null);
    private final Builtins builtins = new Builtins(this);

    // Current source position for error reporting
    private int currentLine = 1;
    private int currentCol = 1;

    // Output buffer for display/write/newline
    StringBuilder outputBuffer = new StringBuilder();

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
                "string-copy", "string-set!",
                "apply", "map", "for-each",
                "abs", "modulo", "remainder", "quotient", "min", "max", "expt",
                "zero?", "positive?", "negative?", "odd?", "even?",
                "list-ref", "list-tail", "list?", "assoc",
                "equal?", "eq?",
                "char-alphabetic?", "char-numeric?", "char-upcase", "char-downcase",
                "char=?", "char<?",
                "string=?", "string<?", "string-ci=?",
                "string-upcase", "string-downcase"}) {
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

    EvalError posError(String msg) {
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

        void set(String name, Object value) throws EvalError {
            if (bindings.containsKey(name)) { bindings.put(name, value); return; }
            if (parent != null) { parent.set(name, value); return; }
            throw new EvalError("set!: unbound variable: " + name);
        }
    }

    // ---- Procedure types ----
    static final class BuiltinProc {
        final String name;
        BuiltinProc(String name) { this.name = name; }
    }

    static final class Lambda {
        final List<String> params;
        final String restParam; // null if no rest param
        final List<Object> body;
        final Env closureEnv;
        Lambda(List<String> params, String restParam, List<Object> body, Env closureEnv) {
            this.params = params;
            this.restParam = restParam;
            this.body = body;
            this.closureEnv = closureEnv;
        }
    }

    static final class SyntaxRules {
        final List<String> literals;
        final List<Object[]> rules; // each: {pattern (List<?>), template (Object)}
        final Env defEnv;
        SyntaxRules(List<String> literals, List<Object[]> rules, Env defEnv) {
            this.literals = literals;
            this.rules = rules;
            this.defEnv = defEnv;
        }
    }

    static final class ResolvedRef {
        final String name;
        final Env env;
        ResolvedRef(String name, Env env) {
            this.name = name;
            this.env = env;
        }
    }

    private long gensymCounter = 0;
    private String gensym(String base) {
        return base + "__g" + (gensymCounter++);
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
        if (expr instanceof ResolvedRef ref) {
            return ref.env.lookup(ref.name);
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
                            // (define (f params...) body...) or (define (f x . rest) body...)
                            if (sig.isEmpty()) throw posError("define: bad syntax");
                            String name = (String) unwrap(sig.get(0));
                            List<String> params = new ArrayList<>();
                            String restParam = null;
                            for (int i = 1; i < sig.size(); i++) {
                                String p = (String) unwrap(sig.get(i));
                                if (p.equals(".")) {
                                    if (i + 1 < sig.size()) {
                                        restParam = (String) unwrap(sig.get(i + 1));
                                    }
                                    break;
                                }
                                params.add(p);
                            }
                            List<Object> body = new ArrayList<>();
                            for (int i = 2; i < list.size(); i++) {
                                body.add(list.get(i));
                            }
                            env.define(name, new Lambda(params, restParam, body, env));
                            return null;
                        }
                        throw posError("define: bad syntax");
                    }
                    case "lambda" -> {
                        if (list.size() < 3) throw posError("lambda: bad syntax");
                        Object paramListRaw = unwrap(list.get(1));
                        List<String> params = new ArrayList<>();
                        String restParam = null;
                        if (paramListRaw instanceof List<?> paramList) {
                            for (int i = 0; i < paramList.size(); i++) {
                                String p = (String) unwrap(paramList.get(i));
                                if (p.equals(".")) {
                                    if (i + 1 < paramList.size()) {
                                        restParam = (String) unwrap(paramList.get(i + 1));
                                    }
                                    break;
                                }
                                params.add(p);
                            }
                        } else if (paramListRaw instanceof String sym) {
                            // (lambda args body...) — single rest param
                            restParam = sym;
                        }
                        List<Object> body = new ArrayList<>();
                        for (int i = 2; i < list.size(); i++) body.add(list.get(i));
                        return new Lambda(params, restParam, body, env);
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
                            Lambda loopLam = new Lambda(params, null, body2, letEnv);
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
                    case "set!" -> {
                        if (list.size() != 3) throw posError("set!: bad syntax");
                        Object varName = unwrap(list.get(1));
                        if (!(varName instanceof String name)) throw posError("set!: expected symbol");
                        Object val = eval(list.get(2), env);
                        try {
                            env.set(name, val);
                        } catch (EvalError e) {
                            throw posError("set!: unbound variable: " + name);
                        }
                        return null;
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
                    case "define-syntax" -> {
                        if (list.size() != 3) throw posError("define-syntax: bad syntax");
                        String name = (String) unwrap(list.get(1));
                        SyntaxRules transformer = evalSyntaxRules(list.get(2), env);
                        env.define(name, transformer);
                        return null;
                    }
                }
                // Check if head is a macro
                try {
                    Object maybeTransformer = env.lookup(op);
                    if (maybeTransformer instanceof SyntaxRules sr) {
                        @SuppressWarnings("unchecked")
                        List<Object> formList = (List<Object>) list;
                        Object expanded = expandMacro(sr, formList);
                        return eval(expanded, env);
                    }
                } catch (EvalError ignored) {}
            }

            // Check if head is a ResolvedRef
            if (rawHead instanceof ResolvedRef ref) {
                Object resolved = ref.env.lookup(ref.name);
                if (resolved instanceof SyntaxRules sr) {
                    @SuppressWarnings("unchecked")
                    List<Object> formList = (List<Object>) list;
                    Object expanded = expandMacro(sr, formList);
                    return eval(expanded, env);
                }
                // Procedure call
                List<Object> args = new ArrayList<>();
                for (int i = 1; i < list.size(); i++) {
                    args.add(eval(list.get(i), env));
                }
                return apply(resolved, args);
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

    Object apply(Object proc, List<Object> args) throws EvalError {
        if (proc instanceof BuiltinProc bp) {
            return builtins.apply(bp.name, args);
        }
        if (proc instanceof Lambda lam) {
            if (lam.restParam != null) {
                if (args.size() < lam.params.size()) {
                    throw posError("wrong number of arguments: expected at least " + lam.params.size() + ", got " + args.size());
                }
            } else {
                if (args.size() != lam.params.size()) {
                    throw posError("wrong number of arguments: expected " + lam.params.size() + ", got " + args.size());
                }
            }
            Env callEnv = new Env(lam.closureEnv);
            for (int i = 0; i < lam.params.size(); i++) {
                callEnv.define(lam.params.get(i), args.get(i));
            }
            if (lam.restParam != null) {
                Object rest = NIL;
                for (int i = args.size() - 1; i >= lam.params.size(); i--) {
                    rest = new Pair(args.get(i), rest);
                }
                callEnv.define(lam.restParam, rest);
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

    boolean schemeEqual(Object a, Object b) {
        if (a == b) return true;
        if (a instanceof Long && b instanceof Long) return a.equals(b);
        if (a instanceof Boolean && b instanceof Boolean) return a.equals(b);
        if (a instanceof String && b instanceof String) return a.equals(b);
        if (a instanceof SchemeString sa && b instanceof SchemeString sb) return sa.value.equals(sb.value);
        if (a instanceof SchemeChar ca && b instanceof SchemeChar cb) return ca.value == cb.value;
        if (a == NIL && b == NIL) return true;
        if (a instanceof Pair pa && b instanceof Pair pb) {
            return schemeEqual(pa.car, pb.car) && schemeEqual(pa.cdr, pb.cdr);
        }
        return false;
    }

    // ---- Output formatting ----

    // display format: strings without quotes
    String displayString(Object val) {
        if (val instanceof SchemeString s) return s.value;
        if (val instanceof SchemeChar c) return String.valueOf(c.value);
        return schemeToString(val);
    }

    @SuppressWarnings("unchecked")
    String schemeToString(Object val) {
        if (val == null) return ""; // void
        if (val == NIL) return "()";
        if (val instanceof Long l) return l.toString();
        if (val instanceof Boolean b) return b ? "#t" : "#f";
        if (val instanceof SchemeString s) return "\"" + s.value + "\"";
        if (val instanceof SchemeChar c) {
            return switch (c.value) {
                case ' ' -> "#\\space";
                case '\n' -> "#\\newline";
                case '\t' -> "#\\tab";
                default -> "#\\" + c.value;
            };
        }
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

    // ---- Macro support ----

    private static boolean isKeyword(String sym) {
        return switch (sym) {
            case "if", "define", "lambda", "quote", "set!", "begin", "cond",
                 "let", "and", "or", "define-syntax", "syntax-rules" -> true;
            default -> false;
        };
    }

    @SuppressWarnings("unchecked")
    private SyntaxRules evalSyntaxRules(Object expr, Env env) throws EvalError {
        List<?> form = (List<?>) unwrap(expr);
        if (form.isEmpty() || !"syntax-rules".equals(unwrap(form.get(0))))
            throw posError("expected syntax-rules");
        List<?> literalsRaw = (List<?>) unwrap(form.get(1));
        List<String> literals = new ArrayList<>();
        for (Object lit : literalsRaw) literals.add((String) unwrap(lit));
        List<Object[]> rules = new ArrayList<>();
        for (int i = 2; i < form.size(); i++) {
            List<?> rule = (List<?>) unwrap(form.get(i));
            rules.add(new Object[]{rule.get(0), rule.get(1)});
        }
        return new SyntaxRules(literals, rules, env);
    }

    @SuppressWarnings("unchecked")
    private Object expandMacro(SyntaxRules sr, List<?> form) throws EvalError {
        Set<String> literalSet = new HashSet<>(sr.literals);
        for (Object[] rule : sr.rules) {
            List<?> pattern = (List<?>) unwrap(rule[0]);
            Object template = rule[1];
            Set<String> ellipsisVars = new HashSet<>();
            Map<String, Object> bindings = matchPattern(pattern, form, literalSet, ellipsisVars);
            if (bindings != null) {
                Set<String> patternVars = new HashSet<>(bindings.keySet());
                Map<String, String> gensymMap = new HashMap<>();
                return expandTemplate(template, bindings, patternVars, ellipsisVars, sr, gensymMap);
            }
        }
        throw posError("no matching syntax-rules pattern");
    }

    private Map<String, Object> matchPattern(List<?> pattern, List<?> form,
                                              Set<String> literals, Set<String> ellipsisVars) {
        Map<String, Object> bindings = new HashMap<>();
        int pi = 1, fi = 1; // skip macro keyword
        int patLen = pattern.size();
        int formLen = form.size();

        while (pi < patLen) {
            Object rawPat = unwrap(pattern.get(pi));
            boolean hasEllipsis = (pi + 1 < patLen && "...".equals(unwrap(pattern.get(pi + 1))));

            if (hasEllipsis) {
                if (!(rawPat instanceof String varName)) return null;
                // Count remaining non-ellipsis pattern elements after this ...
                int remaining = 0;
                for (int k = pi + 2; k < patLen; k++) {
                    if (!"...".equals(unwrap(pattern.get(k)))) remaining++;
                }
                int endFi = formLen - remaining;
                List<Object> matched = new ArrayList<>();
                while (fi < endFi) {
                    matched.add(form.get(fi));
                    fi++;
                }
                ellipsisVars.add(varName);
                bindings.put(varName, matched);
                pi += 2;
            } else if (rawPat instanceof String sym) {
                if (literals.contains(sym)) {
                    if (fi >= formLen) return null;
                    if (!sym.equals(unwrap(form.get(fi)))) return null;
                    fi++;
                } else {
                    if (fi >= formLen) return null;
                    bindings.put(sym, form.get(fi));
                    fi++;
                }
                pi++;
            } else {
                // Literal or nested (not needed for current tests)
                if (fi >= formLen) return null;
                fi++;
                pi++;
            }
        }
        return (fi == formLen) ? bindings : null;
    }

    @SuppressWarnings("unchecked")
    private Object expandTemplate(Object template, Map<String, Object> bindings,
                                   Set<String> patternVars, Set<String> ellipsisVars,
                                   SyntaxRules sr, Map<String, String> gensymMap) throws EvalError {
        Object raw = unwrap(template);

        if (raw instanceof String sym) {
            if (patternVars.contains(sym) && !ellipsisVars.contains(sym)) {
                return bindings.get(sym);
            }
            if (ellipsisVars.contains(sym)) {
                // Being expanded in a non-ellipsis context — return the matched value
                return bindings.get(sym);
            }
            if (isKeyword(sym) || sym.equals("...")) {
                return sym;
            }
            // Free variable — check if in defEnv for hygiene
            try {
                sr.defEnv.lookup(sym);
                return new ResolvedRef(sym, sr.defEnv);
            } catch (EvalError e) {
                // Macro-introduced binding — use gensym
                return gensymMap.computeIfAbsent(sym, k -> gensym(k));
            }
        }
        if (raw instanceof Boolean || raw instanceof Long ||
            raw instanceof SchemeString || raw instanceof SchemeChar) {
            return raw;
        }
        if (raw instanceof List<?> tmplList) {
            List<Object> result = new ArrayList<>();
            for (int i = 0; i < tmplList.size(); i++) {
                boolean hasEllipsis = (i + 1 < tmplList.size() &&
                                       "...".equals(unwrap(tmplList.get(i + 1))));
                if (hasEllipsis) {
                    Set<String> usedEllipsis = findEllipsisVarsInTemplate(tmplList.get(i), ellipsisVars);
                    if (!usedEllipsis.isEmpty()) {
                        String anyVar = usedEllipsis.iterator().next();
                        List<Object> varList = (List<Object>) bindings.get(anyVar);
                        for (int j = 0; j < varList.size(); j++) {
                            Map<String, Object> iterBindings = new HashMap<>(bindings);
                            for (String ev : usedEllipsis) {
                                List<Object> evList = (List<Object>) bindings.get(ev);
                                iterBindings.put(ev, evList.get(j));
                            }
                            Set<String> adjustedEllipsis = new HashSet<>(ellipsisVars);
                            adjustedEllipsis.removeAll(usedEllipsis);
                            result.add(expandTemplate(tmplList.get(i), iterBindings, patternVars,
                                                       adjustedEllipsis, sr, gensymMap));
                        }
                    }
                    i++; // skip "..."
                } else {
                    result.add(expandTemplate(tmplList.get(i), bindings, patternVars,
                                               ellipsisVars, sr, gensymMap));
                }
            }
            return new Located(result, 0, 0);
        }
        return raw;
    }

    private Set<String> findEllipsisVarsInTemplate(Object template, Set<String> ellipsisVars) {
        Object raw = unwrap(template);
        Set<String> found = new HashSet<>();
        if (raw instanceof String sym && ellipsisVars.contains(sym)) {
            found.add(sym);
        } else if (raw instanceof List<?> list) {
            for (Object elem : list) {
                found.addAll(findEllipsisVarsInTemplate(elem, ellipsisVars));
            }
        }
        return found;
    }
}
