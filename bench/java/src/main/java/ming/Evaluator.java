package ming;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;

public class Evaluator {

    // Top-level environment, persisted across evalStr calls
    private final Env globalEnv = createGlobalEnv();
    private StringBuilder outputBuffer = new StringBuilder();

    public String evalStr(String input) throws EvalError {
        outputBuffer.setLength(0);
        List<Object> exprs = parse(input);
        if (exprs.isEmpty()) return schemeToString(VOID);
        Object result = trampoline(evalSeqK(exprs, 0, globalEnv, BounceValue::new));
        return schemeToString(result);
    }

    public EvalResult evalStrWithOutput(String input) throws EvalError {
        outputBuffer.setLength(0);
        List<Object> exprs = parse(input);
        if (exprs.isEmpty()) return new EvalResult(schemeToString(VOID), outputBuffer.toString());
        Object result = trampoline(evalSeqK(exprs, 0, globalEnv, BounceValue::new));
        return new EvalResult(schemeToString(result), outputBuffer.toString());
    }

    // --- Trampoline ---

    private Object trampoline(Bounce b) throws EvalError {
        while (b instanceof BounceThunk bt) {
            b = bt.thunk().get();
        }
        return ((BounceValue) b).value();
    }

    private static Bounce bounce(BounceSupplier s) { return new BounceThunk(s); }

    // --- Environment ---

    static class Env {
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

        void define(String name, Object value) {
            bindings.put(name, value);
        }
    }

    // --- Data types ---

    static final Object VOID = new Object() {
        @Override public String toString() { return "#<void>"; }
    };

    static class SchemeString {
        private char[] chars;
        private boolean immutable;
        SchemeString(String value) { this.chars = value.toCharArray(); this.immutable = true; }
        SchemeString(String value, boolean immutable) { this.chars = value.toCharArray(); this.immutable = immutable; }
        String value() { return new String(chars); }
        char charAt(int i) { return chars[i]; }
        void setChar(int i, char c) { chars[i] = c; }
        boolean isImmutable() { return immutable; }
        int length() { return chars.length; }
    }

    record SchemeChar(char value) {}

    record SchemeRational(long num, long den) {}

    private static long gcd(long a, long b) {
        a = Math.abs(a); b = Math.abs(b);
        while (b != 0) { long t = b; b = a % b; a = t; }
        return a;
    }

    private static Object makeRational(long num, long den) throws EvalError {
        if (den == 0) throw new EvalError("division by zero");
        if (den < 0) { num = -num; den = -den; }
        long g = gcd(num, den);
        num /= g; den /= g;
        if (den == 1) return num;
        return new SchemeRational(num, den);
    }

    sealed interface SchemeList permits Pair, Empty {}

    static final class Pair implements SchemeList {
        private Object car;
        private Object cdr;
        Pair(Object car, Object cdr) { this.car = car; this.cdr = cdr; }
        Object car() { return car; }
        Object cdr() { return cdr; }
        void setCar(Object v) { this.car = v; }
        void setCdr(Object v) { this.cdr = v; }
    }

    enum Empty implements SchemeList { NIL }

    // Lambda (closure) — restParam is non-null for variadic (dot notation)
    record Lambda(List<String> params, String restParam, List<Object> body, Env env) {}

    // case-lambda: multiple clauses with different arities
    record CaseLambda(List<Lambda> clauses) {}

    // Record types
    static class RecordType {
        final String name;
        final List<String> fieldNames;
        RecordType(String name, List<String> fieldNames) {
            this.name = name;
            this.fieldNames = fieldNames;
        }
    }

    static class SchemeRecord {
        final RecordType type;
        final Object[] fields;
        SchemeRecord(RecordType type, Object[] fields) {
            this.type = type;
            this.fields = fields;
        }
    }

    // Mutable vector type
    static class SchemeVector {
        Object[] data;
        SchemeVector(Object[] data) { this.data = data; }
    }

    // Macro transformer from syntax-rules
    @SuppressWarnings("unchecked")
    record SyntaxRules(List<String> literals, List<List<Object>> patterns, List<Object> templates, Env defEnv) {}

    private static final Set<String> MACRO_SPECIAL_FORMS = Set.of(
        "quote", "if", "define", "lambda", "case-lambda", "and", "begin", "let", "let*", "cond", "set!", "or",
        "define-syntax", "syntax-rules", "letrec", "letrec*", "case", "do",
        "call/cc", "call-with-current-continuation"
    );

    // Source position-aware types
    record LocatedSymbol(String name, int line, int col) {}

    static class LocatedList extends ArrayList<Object> {
        final int line, col;
        LocatedList(int line, int col) { super(); this.line = line; this.col = col; }
    }

    // --- CPS types for call/cc ---

    @FunctionalInterface
    interface Cont {
        Bounce apply(Object value) throws EvalError;
    }

    sealed interface Bounce permits BounceValue, BounceThunk {}
    record BounceValue(Object value) implements Bounce {}
    record BounceThunk(BounceSupplier thunk) implements Bounce {}

    @FunctionalInterface
    interface BounceSupplier {
        Bounce get() throws EvalError;
    }

    static class SchemeContinuation {
        final Cont k;
        SchemeContinuation(Cont k) { this.k = k; }
    }

    static final Object CALLCC_PROC = new Object() {
        @Override public String toString() { return "#<procedure>"; }
    };

    // --- Global environment ---

    private Env createGlobalEnv() {
        Env env = new Env(null);
        for (String name : List.of("+", "-", "*", "/", "<", ">", "=", "<=", ">=", "not",
                "cons", "car", "cdr", "null?", "list", "length",
                "string?", "number?", "boolean?", "pair?", "symbol?", "append",
                "display", "write", "newline",
                "string-append", "string-length", "substring",
                "string->number", "number->string",
                "symbol->string", "string->symbol",
                "string-ref", "char?",
                "string-copy", "string-set!",
                "string->list", "list->string", "char->integer", "integer->char",
                "apply",
                "abs", "modulo", "remainder", "quotient", "min", "max", "expt",
                "zero?", "positive?", "negative?", "odd?", "even?",
                "list-ref", "list-tail", "list?", "assoc", "map",
                "equal?", "eq?",
                "char-alphabetic?", "char-numeric?", "char-upcase", "char-downcase",
                "char=?", "char<?",
                "string=?", "string<?", "string-ci=?",
                "string-upcase", "string-downcase",
                "exact?", "inexact?", "exact->inexact", "inexact->exact",
                "numerator", "denominator", "integer?", "rational?",
                "procedure?",
                "eqv?",
                "vector", "make-vector", "vector-ref", "vector-set!", "vector-length", "vector?",
                "vector->list", "list->vector",
                "set-car!", "set-cdr!",
                "caar", "cadr", "cdar", "cddr", "caddr", "cdddr", "caddar",
                "for-each", "reverse", "memq", "memv", "member", "assq", "assv", "gcd", "lcm",
                "truncate", "round", "make-string", "string",
                "string>?", "string<=?", "string>=?",
                "caaar", "caadr", "cadar", "cdaar", "cdadr", "cddar",
                "caaaar", "caaadr", "caadar", "caaddr", "cadaar", "cadadr", "cadddr",
                "cdaaar", "cdaadr", "cdadar", "cdaddr", "cddaar", "cddadr", "cdddar", "cddddr")) {
            env.define(name, "builtin:" + name);
        }
        env.define("call/cc", CALLCC_PROC);
        env.define("call-with-current-continuation", CALLCC_PROC);
        return env;
    }

    // --- Printing ---

    private String schemeToString(Object val) {
        if (val == VOID) return "#<void>";
        if (val instanceof Long n) return n.toString();
        if (val instanceof Double d) return doubleToString(d);
        if (val instanceof SchemeRational r) return r.num() + "/" + r.den();
        if (val instanceof Boolean b) return b ? "#t" : "#f";
        if (val instanceof SchemeChar c) return formatChar(c.value());
        if (val instanceof SchemeString s) return "\"" + s.value() + "\"";
        if (val instanceof String sym) return sym;
        if (val == Empty.NIL) return "()";
        if (val instanceof Pair p) return pairToString(p, true);
        if (val instanceof Lambda) return "#<procedure>";
        if (val instanceof CaseLambda) return "#<procedure>";
        if (val instanceof SyntaxRules) return "#<syntax>";
        if (val instanceof SchemeVector v) return vectorToString(v, true);
        if (val instanceof SchemeRecord) return "#<record>";
        if (val instanceof SchemeContinuation) return "#<procedure>";
        if (val == CALLCC_PROC) return "#<procedure>";
        if (val instanceof java.util.function.Function) return "#<procedure>";
        return val.toString();
    }

    private String vectorToString(SchemeVector v, boolean writeMode) {
        StringBuilder sb = new StringBuilder("#(");
        for (int i = 0; i < v.data.length; i++) {
            if (i > 0) sb.append(" ");
            sb.append(writeMode ? schemeToString(v.data[i]) : displayToString(v.data[i]));
        }
        sb.append(")");
        return sb.toString();
    }

    private String displayToString(Object val) {
        if (val == VOID) return "#<void>";
        if (val instanceof Long n) return n.toString();
        if (val instanceof Double d) return doubleToString(d);
        if (val instanceof SchemeRational r) return r.num() + "/" + r.den();
        if (val instanceof Boolean b) return b ? "#t" : "#f";
        if (val instanceof SchemeChar c) return String.valueOf(c.value());
        if (val instanceof SchemeString s) return s.value();
        if (val instanceof String sym) return sym;
        if (val == Empty.NIL) return "()";
        if (val instanceof Pair p) return pairToString(p, false);
        if (val instanceof Lambda) return "#<procedure>";
        if (val instanceof CaseLambda) return "#<procedure>";
        if (val instanceof SchemeVector v) return vectorToString(v, false);
        if (val instanceof SchemeRecord) return "#<record>";
        if (val instanceof SchemeContinuation) return "#<procedure>";
        if (val == CALLCC_PROC) return "#<procedure>";
        if (val instanceof java.util.function.Function) return "#<procedure>";
        return val.toString();
    }

    private String pairToString(Pair p, boolean writeMode) {
        StringBuilder sb = new StringBuilder("(");
        sb.append(writeMode ? schemeToString(p.car()) : displayToString(p.car()));
        Object rest = p.cdr();
        Set<Pair> seen = new HashSet<>(Set.of(p));
        while (rest instanceof Pair pr) {
            if (!seen.add(pr)) {
                sb.append(" ...");
                break;
            }
            sb.append(" ").append(writeMode ? schemeToString(pr.car()) : displayToString(pr.car()));
            rest = pr.cdr();
        }
        if (rest != Empty.NIL && !(rest instanceof Pair)) {
            sb.append(" . ").append(writeMode ? schemeToString(rest) : displayToString(rest));
        }
        sb.append(")");
        return sb.toString();
    }

    private String formatChar(char c) {
        return switch (c) {
            case ' ' -> "#\\space";
            case '\n' -> "#\\newline";
            case '\t' -> "#\\tab";
            default -> "#\\" + c;
        };
    }

    private String doubleToString(double d) {
        if (d == Math.floor(d) && !Double.isInfinite(d)) {
            long l = (long) d;
            return l + ".0";
        }
        String s = Double.toString(d);
        return s;
    }

    // --- Parser ---

    private List<Object> parse(String input) throws EvalError {
        List<Token> tokens = tokenize(input);
        List<Object> exprs = new ArrayList<>();
        int[] pos = {0};
        while (pos[0] < tokens.size()) {
            exprs.add(parseExpr(tokens, pos));
        }
        return exprs;
    }

    enum TokenType { LPAREN, RPAREN, QUOTE, STRING, ATOM }

    record Token(TokenType type, String value, int line, int col) {}

    private List<Token> tokenize(String input) throws EvalError {
        List<Token> tokens = new ArrayList<>();
        int i = 0;
        int len = input.length();
        int line = 1, col = 1;
        while (i < len) {
            char c = input.charAt(i);
            if (c == '\n') { i++; line++; col = 1; continue; }
            if (Character.isWhitespace(c)) { i++; col++; continue; }
            if (c == ';') {
                while (i < len && input.charAt(i) != '\n') { i++; col++; }
                continue;
            }
            if (c == '(') { tokens.add(new Token(TokenType.LPAREN, "(", line, col)); i++; col++; continue; }
            if (c == ')') { tokens.add(new Token(TokenType.RPAREN, ")", line, col)); i++; col++; continue; }
            if (c == '\'') { tokens.add(new Token(TokenType.QUOTE, "'", line, col)); i++; col++; continue; }
            if (c == '"') {
                int startCol = col;
                StringBuilder sb = new StringBuilder();
                i++; col++;
                while (i < len && input.charAt(i) != '"') {
                    if (input.charAt(i) == '\\') {
                        i++; col++;
                        if (i < len) {
                            char esc = input.charAt(i);
                            switch (esc) {
                                case 'n' -> sb.append('\n');
                                case 't' -> sb.append('\t');
                                case '"' -> sb.append('"');
                                case '\\' -> sb.append('\\');
                                default -> { sb.append('\\'); sb.append(esc); }
                            }
                        }
                    } else {
                        sb.append(input.charAt(i));
                    }
                    if (input.charAt(i) == '\n') { line++; col = 1; } else { col++; }
                    i++;
                }
                if (i < len) { i++; col++; } // skip closing quote
                tokens.add(new Token(TokenType.STRING, sb.toString(), line, startCol));
                continue;
            }
            // Atom
            int startCol = col;
            StringBuilder sb = new StringBuilder();
            while (i < len) {
                char ch = input.charAt(i);
                if (Character.isWhitespace(ch) || ch == '(' || ch == ')' || ch == '"' || ch == ';') break;
                sb.append(ch);
                i++; col++;
            }
            tokens.add(new Token(TokenType.ATOM, sb.toString(), line, startCol));
        }
        return tokens;
    }

    private Object parseExpr(List<Token> tokens, int[] pos) throws EvalError {
        if (pos[0] >= tokens.size()) throw new EvalError("unexpected end of input");
        Token tok = tokens.get(pos[0]);
        pos[0]++;

        return switch (tok.type()) {
            case LPAREN -> {
                LocatedList elems = new LocatedList(tok.line(), tok.col());
                while (pos[0] < tokens.size() && tokens.get(pos[0]).type() != TokenType.RPAREN) {
                    elems.add(parseExpr(tokens, pos));
                }
                if (pos[0] >= tokens.size()) throw new EvalError(tok.line() + ":" + tok.col() + ": missing closing parenthesis");
                pos[0]++; // skip )
                yield elems;
            }
            case RPAREN -> throw new EvalError(tok.line() + ":" + tok.col() + ": unexpected )");
            case QUOTE -> {
                Object quoted = parseExpr(tokens, pos);
                LocatedList q = new LocatedList(tok.line(), tok.col());
                q.add("quote");
                q.add(quoted);
                yield q;
            }
            case STRING -> new SchemeString(tok.value());
            case ATOM -> parseAtom(tok.value(), tok.line(), tok.col());
        };
    }

    private Object parseAtom(String s, int line, int col) {
        if (s.equals("#t")) return Boolean.TRUE;
        if (s.equals("#f")) return Boolean.FALSE;
        if (s.startsWith("#\\")) {
            String charName = s.substring(2);
            return switch (charName) {
                case "space" -> new SchemeChar(' ');
                case "newline" -> new SchemeChar('\n');
                case "tab" -> new SchemeChar('\t');
                default -> {
                    if (charName.length() == 1) yield new SchemeChar(charName.charAt(0));
                    yield new LocatedSymbol(s, line, col);
                }
            };
        }
        // Try integer
        try {
            return Long.parseLong(s);
        } catch (NumberFormatException e) { /* fall through */ }
        // Try rational N/D
        int slashIdx = s.indexOf('/');
        if (slashIdx > 0 && slashIdx < s.length() - 1) {
            try {
                long num = Long.parseLong(s.substring(0, slashIdx));
                long den = Long.parseLong(s.substring(slashIdx + 1));
                return makeRational(num, den);
            } catch (NumberFormatException | EvalError e) { /* fall through */ }
        }
        // Try floating point
        try {
            if (s.contains(".") || s.contains("e") || s.contains("E")) {
                return Double.parseDouble(s);
            }
        } catch (NumberFormatException e) { /* fall through */ }
        return new LocatedSymbol(s, line, col); // symbol with position
    }

    // --- Error helper ---

    private static EvalError errAt(int line, int col, String msg) {
        return new EvalError(line + ":" + col + ": " + msg);
    }

    private static boolean hasPosition(String msg) {
        return msg.matches(".*\\d+:\\d+.*");
    }

    // Helper to get symbol name from either String or LocatedSymbol
    private static String symName(Object o) {
        if (o instanceof String s) return s;
        if (o instanceof LocatedSymbol ls) return ls.name();
        return null;
    }

    // --- Synchronous eval (for use from builtins) ---

    private Object eval(Object expr, Env env) throws EvalError {
        return trampoline(evalK(expr, env, BounceValue::new));
    }

    // --- CPS Evaluator ---

    @SuppressWarnings("unchecked")
    private Bounce evalK(Object expr, Env env, Cont k) throws EvalError {
        if (expr instanceof Long || expr instanceof Double || expr instanceof SchemeRational
                || expr instanceof Boolean || expr instanceof SchemeString || expr instanceof SchemeChar
                || expr instanceof SchemeVector) {
            return k.apply(expr);
        }
        if (expr instanceof LocatedSymbol ls) {
            try {
                return k.apply(env.lookup(ls.name()));
            } catch (EvalError e) {
                throw errAt(ls.line(), ls.col(), e.getMessage());
            }
        }
        if (expr instanceof String sym) {
            return k.apply(env.lookup(sym));
        }
        if (expr instanceof List<?> list) {
            int eline = 0, ecol = 0;
            if (list instanceof LocatedList ll) { eline = ll.line; ecol = ll.col; }

            if (list.isEmpty()) throw errAt(eline, ecol, "empty application");
            Object first = list.get(0);
            String sym = symName(first);
            final int el = eline, ec = ecol;

            if (sym != null) {
                switch (sym) {
                    case "quote" -> {
                        if (list.size() != 2) throw errAt(eline, ecol, "quote: expected 1 argument");
                        return k.apply(toSchemeValue(list.get(1)));
                    }
                    case "if" -> {
                        if (list.size() < 3 || list.size() > 4) throw errAt(eline, ecol, "if: bad syntax");
                        return bounce(() -> evalK(list.get(1), env, cond -> {
                            if (!isFalse(cond)) return bounce(() -> evalK(list.get(2), env, k));
                            else if (list.size() == 4) return bounce(() -> evalK(list.get(3), env, k));
                            return k.apply(VOID);
                        }));
                    }
                    case "define" -> {
                        if (list.size() < 3) throw errAt(eline, ecol, "define: bad syntax");
                        Object target = list.get(1);
                        String targetName = symName(target);
                        if (targetName != null) {
                            return bounce(() -> evalK(list.get(2), env, val -> {
                                env.define(targetName, val);
                                return k.apply(VOID);
                            }));
                        } else if (target instanceof List<?> sig) {
                            String fname = symName(sig.isEmpty() ? null : sig.get(0));
                            if (sig.isEmpty() || fname == null)
                                throw errAt(eline, ecol, "define: bad syntax");
                            List<String> params = new ArrayList<>();
                            String restParam = null;
                            for (int i = 1; i < sig.size(); i++) {
                                String pname = symName(sig.get(i));
                                if (pname == null)
                                    throw errAt(eline, ecol, "define: parameter must be a symbol");
                                if (".".equals(pname)) {
                                    if (i + 2 != sig.size())
                                        throw errAt(eline, ecol, "define: bad dot syntax");
                                    restParam = symName(sig.get(i + 1));
                                    if (restParam == null)
                                        throw errAt(eline, ecol, "define: parameter must be a symbol");
                                    break;
                                }
                                params.add(pname);
                            }
                            List<Object> body = new ArrayList<>();
                            for (int i = 2; i < list.size(); i++) body.add(list.get(i));
                            env.define(fname, new Lambda(params, restParam, body, env));
                            return k.apply(VOID);
                        } else {
                            throw errAt(eline, ecol, "define: bad syntax");
                        }
                    }
                    case "lambda" -> {
                        if (list.size() < 3) throw errAt(eline, ecol, "lambda: bad syntax");
                        if (!(list.get(1) instanceof List<?> paramList))
                            throw errAt(eline, ecol, "lambda: bad syntax");
                        List<String> params = new ArrayList<>();
                        String restParam = null;
                        for (int i = 0; i < paramList.size(); i++) {
                            String pname = symName(paramList.get(i));
                            if (pname == null)
                                throw errAt(eline, ecol, "lambda: parameter must be a symbol");
                            if (".".equals(pname)) {
                                if (i + 2 != paramList.size())
                                    throw errAt(eline, ecol, "lambda: bad dot syntax");
                                restParam = symName(paramList.get(i + 1));
                                if (restParam == null)
                                    throw errAt(eline, ecol, "lambda: parameter must be a symbol");
                                break;
                            }
                            params.add(pname);
                        }
                        List<Object> body = new ArrayList<>();
                        for (int i = 2; i < list.size(); i++) body.add(list.get(i));
                        return k.apply(new Lambda(params, restParam, body, env));
                    }
                    case "case-lambda" -> {
                        if (list.size() < 2) throw errAt(eline, ecol, "case-lambda: bad syntax");
                        List<Lambda> clauses = new ArrayList<>();
                        for (int ci = 1; ci < list.size(); ci++) {
                            if (!(list.get(ci) instanceof List<?> clause) || clause.size() < 2)
                                throw errAt(eline, ecol, "case-lambda: bad clause");
                            if (!(clause.get(0) instanceof List<?> cparamList))
                                throw errAt(eline, ecol, "case-lambda: bad clause");
                            List<String> cparams = new ArrayList<>();
                            String crest = null;
                            for (int pi = 0; pi < cparamList.size(); pi++) {
                                String pname = symName(cparamList.get(pi));
                                if (pname == null)
                                    throw errAt(eline, ecol, "case-lambda: parameter must be a symbol");
                                if (".".equals(pname)) {
                                    if (pi + 2 != cparamList.size())
                                        throw errAt(eline, ecol, "case-lambda: bad dot syntax");
                                    crest = symName(cparamList.get(pi + 1));
                                    if (crest == null)
                                        throw errAt(eline, ecol, "case-lambda: parameter must be a symbol");
                                    break;
                                }
                                cparams.add(pname);
                            }
                            List<Object> cbody = new ArrayList<>();
                            for (int bi = 1; bi < clause.size(); bi++) cbody.add(clause.get(bi));
                            clauses.add(new Lambda(cparams, crest, cbody, env));
                        }
                        return k.apply(new CaseLambda(clauses));
                    }
                    case "and" -> {
                        if (list.size() == 1) return k.apply(Boolean.TRUE);
                        return evalAndK(list, 1, env, k);
                    }
                    case "begin" -> {
                        if (list.size() == 1) return k.apply(VOID);
                        return evalSeqK(list, 1, env, k);
                    }
                    case "let" -> {
                        if (list.size() < 3) throw errAt(eline, ecol, "let: bad syntax");
                        Object second = list.get(1);
                        String secondName = symName(second);
                        if (secondName != null) {
                            // Named let
                            if (list.size() < 4) throw errAt(eline, ecol, "let: bad syntax");
                            List<?> bindings = (List<?>) list.get(2);
                            List<String> params = new ArrayList<>();
                            List<Object> initExprs = new ArrayList<>();
                            for (Object b : bindings) {
                                List<?> binding = (List<?>) b;
                                String pname = symName(binding.size() >= 1 ? binding.get(0) : null);
                                if (binding.size() != 2 || pname == null)
                                    throw errAt(eline, ecol, "let: bad binding");
                                params.add(pname);
                                initExprs.add(binding.get(1));
                            }
                            List<Object> body = new ArrayList<>();
                            for (int i = 3; i < list.size(); i++) body.add(list.get(i));
                            Env letEnv = new Env(env);
                            Lambda loopLam = new Lambda(params, null, body, letEnv);
                            letEnv.define(secondName, loopLam);
                            return evalListK(initExprs, 0, env, new ArrayList<>(), initsObj -> {
                                List<Object> inits = (List<Object>) initsObj;
                                Env callEnv = new Env(letEnv);
                                for (int i = 0; i < params.size(); i++) {
                                    callEnv.define(params.get(i), inits.get(i));
                                }
                                return evalSeqK(body, 0, callEnv, k);
                            });
                        }
                        // Regular let
                        List<?> bindings = (List<?>) second;
                        Env letEnv = new Env(env);
                        return evalLetBindsK(bindings, 0, env, letEnv, el, ec, () ->
                            evalSeqK(list, 2, letEnv, k));
                    }
                    case "let*" -> {
                        if (list.size() < 3) throw errAt(eline, ecol, "let*: bad syntax");
                        List<?> bindings = (List<?>) list.get(1);
                        Env letEnv = new Env(env);
                        return evalLetStarBindsK(bindings, 0, letEnv, el, ec, () ->
                            evalSeqK(list, 2, letEnv, k));
                    }
                    case "cond" -> {
                        return evalCondK(list, 1, env, el, ec, k);
                    }
                    case "set!" -> {
                        if (list.size() != 3) throw errAt(eline, ecol, "set!: bad syntax");
                        Object target = list.get(1);
                        String tname = symName(target);
                        if (tname == null) throw errAt(eline, ecol, "set!: expected symbol");
                        int tline = eline, tcol = ecol;
                        if (target instanceof LocatedSymbol ls) { tline = ls.line(); tcol = ls.col(); }
                        final int tl = tline, tc = tcol;
                        return bounce(() -> evalK(list.get(2), env, val -> {
                            Env e = env;
                            while (e != null) {
                                if (e.bindings.containsKey(tname)) {
                                    e.bindings.put(tname, val);
                                    return k.apply(VOID);
                                }
                                e = e.parent;
                            }
                            throw errAt(tl, tc, "set!: unbound variable: " + tname);
                        }));
                    }
                    case "or" -> {
                        if (list.size() == 1) return k.apply(Boolean.FALSE);
                        return evalOrK(list, 1, env, k);
                    }
                    case "define-record-type" -> {
                        if (list.size() < 4) throw errAt(eline, ecol, "define-record-type: bad syntax");
                        String typeName = symName(list.get(1));
                        if (typeName == null) throw errAt(eline, ecol, "define-record-type: expected type name");
                        if (!(list.get(2) instanceof List<?> ctorSpec) || ctorSpec.size() < 1)
                            throw errAt(eline, ecol, "define-record-type: bad constructor");
                        String ctorName = symName(ctorSpec.get(0));
                        if (ctorName == null) throw errAt(eline, ecol, "define-record-type: bad constructor name");
                        List<String> ctorFields = new ArrayList<>();
                        for (int i = 1; i < ctorSpec.size(); i++) {
                            String fn = symName(ctorSpec.get(i));
                            if (fn == null) throw errAt(eline, ecol, "define-record-type: bad field name");
                            ctorFields.add(fn);
                        }
                        String predName = symName(list.get(3));
                        if (predName == null) throw errAt(eline, ecol, "define-record-type: bad predicate name");

                        RecordType rt = new RecordType(typeName, ctorFields);
                        final RecordType rtFinal = rt;
                        env.define(ctorName, (java.util.function.Function<List<Object>, Object>) args2 -> {
                            if (args2.size() != rtFinal.fieldNames.size())
                                throw new RuntimeException("wrong number of arguments");
                            return new SchemeRecord(rtFinal, args2.toArray());
                        });
                        env.define(predName, (java.util.function.Function<List<Object>, Object>) args2 -> {
                            if (args2.size() != 1) throw new RuntimeException("wrong number of arguments");
                            return args2.get(0) instanceof SchemeRecord sr && sr.type == rtFinal;
                        });
                        for (int i = 4; i < list.size(); i++) {
                            if (!(list.get(i) instanceof List<?> fieldSpec) || fieldSpec.size() < 2)
                                throw errAt(eline, ecol, "define-record-type: bad field spec");
                            String fieldName = symName(fieldSpec.get(0));
                            String accessorName = symName(fieldSpec.get(1));
                            if (fieldName == null || accessorName == null)
                                throw errAt(eline, ecol, "define-record-type: bad field spec");
                            int fieldIdx = ctorFields.indexOf(fieldName);
                            if (fieldIdx < 0)
                                throw errAt(eline, ecol, "define-record-type: unknown field " + fieldName);
                            final int idx = fieldIdx;
                            env.define(accessorName, (java.util.function.Function<List<Object>, Object>) args2 -> {
                                if (args2.size() != 1) throw new RuntimeException("wrong number of arguments");
                                if (!(args2.get(0) instanceof SchemeRecord sr) || sr.type != rtFinal)
                                    throw new RuntimeException(accessorName + ": not a " + typeName);
                                return sr.fields[idx];
                            });
                        }
                        return k.apply(VOID);
                    }
                    case "letrec" -> {
                        if (list.size() < 3) throw errAt(eline, ecol, "letrec: bad syntax");
                        if (!(list.get(1) instanceof List<?> bindings))
                            throw errAt(eline, ecol, "letrec: bad syntax");
                        Env letrecEnv = new Env(env);
                        List<String> names = new ArrayList<>();
                        for (Object b : bindings) {
                            List<?> binding = (List<?>) b;
                            String bname = symName(binding.size() >= 1 ? binding.get(0) : null);
                            if (binding.size() != 2 || bname == null)
                                throw errAt(eline, ecol, "letrec: bad binding");
                            names.add(bname);
                            letrecEnv.define(bname, VOID);
                        }
                        return evalLetrecBindsK(bindings, 0, names, letrecEnv, el, ec,
                            () -> evalSeqK(list, 2, letrecEnv, k));
                    }
                    case "letrec*" -> {
                        if (list.size() < 3) throw errAt(eline, ecol, "letrec*: bad syntax");
                        if (!(list.get(1) instanceof List<?> bindings))
                            throw errAt(eline, ecol, "letrec*: bad syntax");
                        Env letrecEnv = new Env(env);
                        return evalLetrecStarBindsK(bindings, 0, letrecEnv, el, ec,
                            () -> evalSeqK(list, 2, letrecEnv, k));
                    }
                    case "case" -> {
                        if (list.size() < 2) throw errAt(eline, ecol, "case: bad syntax");
                        return bounce(() -> evalK(list.get(1), env, key ->
                            evalCaseClausesK(list, 2, key, env, el, ec, k)));
                    }
                    case "do" -> {
                        return evalDoK(list, env, el, ec, k);
                    }
                    case "define-syntax" -> {
                        if (list.size() != 3) throw errAt(eline, ecol, "define-syntax: bad syntax");
                        String macroName = symName(list.get(1));
                        if (macroName == null) throw errAt(eline, ecol, "define-syntax: expected symbol");
                        Object transformer = list.get(2);
                        if (!(transformer instanceof List<?> tlist) || tlist.size() < 2)
                            throw errAt(eline, ecol, "define-syntax: expected syntax-rules");
                        String trSym = symName(tlist.get(0));
                        if (!"syntax-rules".equals(trSym))
                            throw errAt(eline, ecol, "define-syntax: expected syntax-rules");
                        if (!(tlist.get(1) instanceof List<?> litList))
                            throw errAt(eline, ecol, "syntax-rules: expected literal list");
                        List<String> lits = new ArrayList<>();
                        for (Object l : litList) {
                            String ln = symName(l);
                            if (ln != null) lits.add(ln);
                        }
                        List<List<Object>> pats = new ArrayList<>();
                        List<Object> tmpls = new ArrayList<>();
                        for (int i = 2; i < tlist.size(); i++) {
                            if (!(tlist.get(i) instanceof List<?> rule) || rule.size() != 2)
                                throw errAt(eline, ecol, "syntax-rules: bad rule");
                            if (!(rule.get(0) instanceof List<?> pattern))
                                throw errAt(eline, ecol, "syntax-rules: pattern must be a list");
                            List<Object> pat = (List<Object>) pattern;
                            pats.add(pat);
                            tmpls.add(rule.get(1));
                        }
                        env.define(macroName, new SyntaxRules(lits, pats, tmpls, env));
                        return k.apply(VOID);
                    }
                    case "call/cc", "call-with-current-continuation" -> {
                        if (list.size() != 2) throw errAt(eline, ecol, "call/cc: expected 1 argument");
                        return bounce(() -> evalK(list.get(1), env, proc -> {
                            SchemeContinuation cont = new SchemeContinuation(k);
                            return applyK(proc, List.of(cont), el, ec, k);
                        }));
                    }
                }

                // Check for macro expansion
                {
                    Object maybeMacro = null;
                    try { maybeMacro = env.lookup(sym); } catch (EvalError ignored) {}
                    if (maybeMacro instanceof SyntaxRules sr) {
                        return applyMacroK(sr, list, env, k);
                    }
                }
            }

            // Function application: eval func, then args (right-to-left for call/cc compat), then apply
            return bounce(() -> evalK(first, env, proc -> {
                int nargs = list.size() - 1;
                if (nargs == 0) {
                    try {
                        return applyK(proc, List.of(), el, ec, k);
                    } catch (EvalError e) {
                        if (el > 0 && !hasPosition(e.getMessage())) throw errAt(el, ec, e.getMessage());
                        throw e;
                    }
                }
                return evalArgsRtoLK(list, list.size() - 1, 1, env, new Object[nargs], argsArr -> {
                    Object[] arr = (Object[]) argsArr;
                    List<Object> args = new ArrayList<>(arr.length);
                    for (Object a : arr) args.add(a);
                    try {
                        return applyK(proc, args, el, ec, k);
                    } catch (EvalError e) {
                        if (el > 0 && !hasPosition(e.getMessage())) throw errAt(el, ec, e.getMessage());
                        throw e;
                    }
                });
            }));
        }
        throw new EvalError("cannot evaluate: " + expr);
    }

    // --- CPS Helper Methods ---

    private Bounce evalSeqK(List<?> exprs, int start, Env env, Cont k) throws EvalError {
        if (start >= exprs.size()) return k.apply(VOID);
        if (start == exprs.size() - 1) return bounce(() -> evalK(exprs.get(start), env, k));
        return bounce(() -> evalK(exprs.get(start), env, _v -> evalSeqK(exprs, start + 1, env, k)));
    }

    // Right-to-left argument evaluation for function application (needed for call/cc compatibility)
    private Bounce evalArgsRtoLK(List<?> list, int idx, int start, Env env, Object[] results, Cont k) throws EvalError {
        if (idx < start) return k.apply(results);
        return bounce(() -> evalK(list.get(idx), env, val -> {
            Object[] newResults = results.clone();
            newResults[idx - start] = val;
            return evalArgsRtoLK(list, idx - 1, start, env, newResults, k);
        }));
    }

    private Bounce evalListK(List<?> exprs, int start, Env env, List<Object> acc, Cont k) throws EvalError {
        if (start >= exprs.size()) return k.apply(acc);
        return bounce(() -> evalK(exprs.get(start), env, val -> {
            List<Object> newAcc = new ArrayList<>(acc);
            newAcc.add(val);
            return evalListK(exprs, start + 1, env, newAcc, k);
        }));
    }

    private Bounce evalAndK(List<?> list, int idx, Env env, Cont k) throws EvalError {
        if (idx == list.size() - 1) return bounce(() -> evalK(list.get(idx), env, k));
        return bounce(() -> evalK(list.get(idx), env, val -> {
            if (isFalse(val)) return k.apply(val);
            return evalAndK(list, idx + 1, env, k);
        }));
    }

    private Bounce evalOrK(List<?> list, int idx, Env env, Cont k) throws EvalError {
        if (idx == list.size() - 1) return bounce(() -> evalK(list.get(idx), env, k));
        return bounce(() -> evalK(list.get(idx), env, val -> {
            if (!isFalse(val)) return k.apply(val);
            return evalOrK(list, idx + 1, env, k);
        }));
    }

    private Bounce evalLetBindsK(List<?> bindings, int idx, Env outerEnv, Env letEnv, int el, int ec, BounceSupplier after) throws EvalError {
        if (idx >= bindings.size()) return after.get();
        List<?> binding = (List<?>) bindings.get(idx);
        String bname = symName(binding.size() >= 1 ? binding.get(0) : null);
        if (binding.size() != 2 || bname == null) throw errAt(el, ec, "let: bad binding");
        return bounce(() -> evalK(binding.get(1), outerEnv, val -> {
            letEnv.define(bname, val);
            return evalLetBindsK(bindings, idx + 1, outerEnv, letEnv, el, ec, after);
        }));
    }

    private Bounce evalLetStarBindsK(List<?> bindings, int idx, Env letEnv, int el, int ec, BounceSupplier after) throws EvalError {
        if (idx >= bindings.size()) return after.get();
        List<?> binding = (List<?>) bindings.get(idx);
        String bname = symName(binding.size() >= 1 ? binding.get(0) : null);
        if (binding.size() != 2 || bname == null) throw errAt(el, ec, "let*: bad binding");
        return bounce(() -> evalK(binding.get(1), letEnv, val -> {
            letEnv.define(bname, val);
            return evalLetStarBindsK(bindings, idx + 1, letEnv, el, ec, after);
        }));
    }

    private Bounce evalLetrecBindsK(List<?> bindings, int idx, List<String> names, Env letrecEnv, int el, int ec, BounceSupplier after) throws EvalError {
        if (idx >= bindings.size()) return after.get();
        List<?> binding = (List<?>) bindings.get(idx);
        return bounce(() -> evalK(binding.get(1), letrecEnv, val -> {
            letrecEnv.define(names.get(idx), val);
            return evalLetrecBindsK(bindings, idx + 1, names, letrecEnv, el, ec, after);
        }));
    }

    private Bounce evalLetrecStarBindsK(List<?> bindings, int idx, Env letrecEnv, int el, int ec, BounceSupplier after) throws EvalError {
        if (idx >= bindings.size()) return after.get();
        List<?> binding = (List<?>) bindings.get(idx);
        String bname = symName(binding.size() >= 1 ? binding.get(0) : null);
        if (binding.size() != 2 || bname == null) throw errAt(el, ec, "letrec*: bad binding");
        return bounce(() -> evalK(binding.get(1), letrecEnv, val -> {
            letrecEnv.define(bname, val);
            return evalLetrecStarBindsK(bindings, idx + 1, letrecEnv, el, ec, after);
        }));
    }

    private Bounce evalCondK(List<?> list, int idx, Env env, int el, int ec, Cont k) throws EvalError {
        if (idx >= list.size()) return k.apply(VOID);
        if (!(list.get(idx) instanceof List<?> clause) || clause.isEmpty())
            throw errAt(el, ec, "cond: empty clause");
        Object test = clause.get(0);
        String testSym = symName(test);
        if ("else".equals(testSym)) {
            if (clause.size() > 1) return evalSeqK(clause, 1, env, k);
            return k.apply(VOID);
        }
        return bounce(() -> evalK(test, env, val -> {
            if (!isFalse(val)) {
                if (clause.size() == 1) return k.apply(val);
                return evalSeqK(clause, 1, env, k);
            }
            return evalCondK(list, idx + 1, env, el, ec, k);
        }));
    }

    private Bounce evalCaseClausesK(List<?> list, int idx, Object key, Env env, int el, int ec, Cont k) throws EvalError {
        if (idx >= list.size()) return k.apply(VOID);
        if (!(list.get(idx) instanceof List<?> clause) || clause.isEmpty())
            throw errAt(el, ec, "case: bad clause");
        Object datums = clause.get(0);
        String dSym = symName(datums);
        if ("else".equals(dSym)) {
            if (clause.size() > 1) return evalSeqK(clause, 1, env, k);
            return k.apply(VOID);
        }
        if (!(datums instanceof List<?> datumList))
            throw errAt(el, ec, "case: bad clause");
        for (Object d : datumList) {
            Object datum = toSchemeValue(d);
            if (schemeEqv(key, datum)) {
                if (clause.size() > 1) return evalSeqK(clause, 1, env, k);
                return k.apply(VOID);
            }
        }
        return evalCaseClausesK(list, idx + 1, key, env, el, ec, k);
    }

    private Bounce evalDoK(List<?> list, Env outerEnv, int el, int ec, Cont k) throws EvalError {
        if (list.size() < 3) throw errAt(el, ec, "do: bad syntax");
        if (!(list.get(1) instanceof List<?> varSpecs))
            throw errAt(el, ec, "do: bad syntax");
        if (!(list.get(2) instanceof List<?> testClause) || testClause.isEmpty())
            throw errAt(el, ec, "do: bad syntax");

        List<String> varNames = new ArrayList<>();
        List<Object> initExprs = new ArrayList<>();
        List<Object> stepExprs = new ArrayList<>();
        for (Object vs : varSpecs) {
            if (!(vs instanceof List<?> spec) || spec.size() < 2)
                throw errAt(el, ec, "do: bad variable spec");
            String vname = symName(spec.get(0));
            if (vname == null) throw errAt(el, ec, "do: expected symbol");
            varNames.add(vname);
            initExprs.add(spec.get(1));
            stepExprs.add(spec.size() >= 3 ? spec.get(2) : null);
        }

        Env doEnv = new Env(outerEnv);
        return evalDoInitsK(initExprs, 0, outerEnv, doEnv, varNames, () ->
            doLoopK(varNames, stepExprs, testClause, list, doEnv, k));
    }

    private Bounce evalDoInitsK(List<Object> initExprs, int idx, Env outerEnv, Env doEnv, List<String> varNames, BounceSupplier after) throws EvalError {
        if (idx >= initExprs.size()) return after.get();
        return bounce(() -> evalK(initExprs.get(idx), outerEnv, val -> {
            doEnv.define(varNames.get(idx), val);
            return evalDoInitsK(initExprs, idx + 1, outerEnv, doEnv, varNames, after);
        }));
    }

    private Bounce doLoopK(List<String> varNames, List<Object> stepExprs, List<?> testClause, List<?> list, Env doEnv, Cont k) throws EvalError {
        return bounce(() -> evalK(testClause.get(0), doEnv, testVal -> {
            if (!isFalse(testVal)) {
                if (testClause.size() == 1) return k.apply(VOID);
                Object result = VOID;
                for (int j = 1; j < testClause.size(); j++) {
                    result = eval(testClause.get(j), doEnv);
                }
                return k.apply(result);
            }
            // Execute body
            for (int j = 3; j < list.size(); j++) {
                eval(list.get(j), doEnv);
            }
            // Step: evaluate all steps, then update in parallel
            Object[] newVals = new Object[varNames.size()];
            for (int j = 0; j < varNames.size(); j++) {
                if (stepExprs.get(j) != null) {
                    newVals[j] = eval(stepExprs.get(j), doEnv);
                } else {
                    newVals[j] = doEnv.bindings.get(varNames.get(j));
                }
            }
            for (int j = 0; j < varNames.size(); j++) {
                doEnv.define(varNames.get(j), newVals[j]);
            }
            return doLoopK(varNames, stepExprs, testClause, list, doEnv, k);
        }));
    }

    // --- CPS Apply ---

    @SuppressWarnings("unchecked")
    private Bounce applyK(Object proc, List<Object> args, int eline, int ecol, Cont k) throws EvalError {
        // call/cc as first-class value
        if (proc == CALLCC_PROC) {
            if (args.size() != 1) throw new EvalError("call/cc: expected 1 argument");
            Object fn = args.get(0);
            SchemeContinuation cont = new SchemeContinuation(k);
            return applyK(fn, List.of(cont), eline, ecol, k);
        }
        // Continuation invocation
        if (proc instanceof SchemeContinuation sc) {
            if (args.isEmpty()) throw new EvalError("continuation: expected 1 argument");
            return sc.k.apply(args.get(0));
        }
        // CaseLambda dispatch
        if (proc instanceof CaseLambda cl) {
            Lambda matched = null;
            for (Lambda clause : cl.clauses()) {
                if (clause.restParam() != null) {
                    if (args.size() >= clause.params().size()) { matched = clause; break; }
                } else {
                    if (args.size() == clause.params().size()) { matched = clause; break; }
                }
            }
            if (matched == null) throw new EvalError("case-lambda: no matching clause for " + args.size() + " arguments");
            proc = matched;
        }
        // Lambda application
        if (proc instanceof Lambda lam) {
            if (lam.restParam() != null) {
                if (args.size() < lam.params().size())
                    throw new EvalError("wrong number of arguments: expected at least " + lam.params().size() + ", got " + args.size());
            } else {
                if (args.size() != lam.params().size())
                    throw new EvalError("wrong number of arguments: expected " + lam.params().size() + ", got " + args.size());
            }
            Env callEnv = new Env(lam.env());
            for (int i = 0; i < lam.params().size(); i++) {
                callEnv.define(lam.params().get(i), args.get(i));
            }
            if (lam.restParam() != null) {
                Object rest = Empty.NIL;
                for (int i = args.size() - 1; i >= lam.params().size(); i--) {
                    rest = new Pair(args.get(i), rest);
                }
                callEnv.define(lam.restParam(), rest);
            }
            return evalSeqK(lam.body(), 0, callEnv, k);
        }
        // Builtin
        if (proc instanceof String sym && sym.startsWith("builtin:")) {
            String name = sym.substring(8);
            // CPS builtins that call procedures
            if ("apply".equals(name)) {
                if (args.size() < 2) throw new EvalError("apply: expected at least 2 arguments");
                Object fn = args.get(0);
                Object lastArg = args.get(args.size() - 1);
                List<Object> allArgs = new ArrayList<>();
                for (int i = 1; i < args.size() - 1; i++) allArgs.add(args.get(i));
                Object lst = lastArg;
                while (lst instanceof Pair p) { allArgs.add(p.car()); lst = p.cdr(); }
                return applyK(fn, allArgs, eline, ecol, k);
            }
            if ("map".equals(name)) {
                return mapCpsK(args, k);
            }
            if ("for-each".equals(name)) {
                return forEachCpsK(args, k);
            }
            Object result = applyBuiltin(name, args);
            return k.apply(result);
        }
        // java.util.function.Function (from define-record-type)
        if (proc instanceof java.util.function.Function) {
            java.util.function.Function<List<Object>, Object> fn = (java.util.function.Function<List<Object>, Object>) proc;
            try {
                Object result = fn.apply(args);
                return k.apply(result);
            } catch (RuntimeException e) {
                throw new EvalError(e.getMessage());
            }
        }
        throw new EvalError("not a procedure: " + schemeToString(proc));
    }

    // CPS map
    private Bounce mapCpsK(List<Object> args, Cont k) throws EvalError {
        if (args.size() < 2) throw new EvalError("map: expected at least 2 arguments");
        Object fn = args.get(0);
        List<Object> lists = new ArrayList<>();
        for (int i = 1; i < args.size(); i++) lists.add(args.get(i));
        return mapLoopK(fn, lists, new ArrayList<>(), k);
    }

    private Bounce mapLoopK(Object fn, List<Object> lists, List<Object> results, Cont k) throws EvalError {
        boolean done = false;
        for (Object l : lists) { if (!(l instanceof Pair)) { done = true; break; } }
        if (done) {
            Object result = Empty.NIL;
            for (int i = results.size() - 1; i >= 0; i--) result = new Pair(results.get(i), result);
            return k.apply(result);
        }
        List<Object> mapArgs = new ArrayList<>();
        List<Object> nextLists = new ArrayList<>();
        for (Object l : lists) {
            Pair p = (Pair) l;
            mapArgs.add(p.car());
            nextLists.add(p.cdr());
        }
        return applyK(fn, mapArgs, 0, 0, val -> {
            results.add(val);
            return mapLoopK(fn, nextLists, results, k);
        });
    }

    // CPS for-each
    private Bounce forEachCpsK(List<Object> args, Cont k) throws EvalError {
        if (args.size() < 2) throw new EvalError("for-each: expected at least 2 arguments");
        Object fn = args.get(0);
        Object lst = args.get(1);
        return forEachLoopK(fn, lst, k);
    }

    private Bounce forEachLoopK(Object fn, Object lst, Cont k) throws EvalError {
        if (!(lst instanceof Pair p)) return k.apply(VOID);
        return applyK(fn, List.of(p.car()), 0, 0, _v -> forEachLoopK(fn, p.cdr(), k));
    }

    // --- CPS Macro expansion ---

    @SuppressWarnings("unchecked")
    private Bounce applyMacroK(SyntaxRules sr, List<?> input, Env useEnv, Cont k) throws EvalError {
        for (int r = 0; r < sr.patterns().size(); r++) {
            Map<String, Object> bindings = matchPattern(sr.patterns().get(r), input, sr.literals());
            if (bindings != null) {
                Set<String> ellipsisVars = new HashSet<>();
                collectEllipsisVars(sr.patterns().get(r), ellipsisVars);
                Object expanded = expandTemplate(sr.templates().get(r), bindings, ellipsisVars);

                // Hygiene: overlay def-site bindings for free variables in template
                Set<String> freeVars = new HashSet<>();
                collectFreeVars(sr.templates().get(r), bindings.keySet(), freeVars);
                Env evalEnv = useEnv;
                boolean overlayCreated = false;
                for (String fv : freeVars) {
                    try {
                        Object val = sr.defEnv().lookup(fv);
                        if (!overlayCreated) {
                            evalEnv = new Env(useEnv);
                            overlayCreated = true;
                        }
                        evalEnv.define(fv, val);
                    } catch (EvalError ignored) {}
                }
                final Env finalEvalEnv = evalEnv;
                return bounce(() -> evalK(expanded, finalEvalEnv, k));
            }
        }
        throw new EvalError("no matching pattern for macro");
    }

    // Convert parsed data to Scheme values (for quote)
    @SuppressWarnings("unchecked")
    private Object toSchemeValue(Object parsed) {
        if (parsed instanceof List<?> list) {
            Object result = Empty.NIL;
            for (int i = list.size() - 1; i >= 0; i--) {
                result = new Pair(toSchemeValue(list.get(i)), result);
            }
            return result;
        }
        if (parsed instanceof LocatedSymbol ls) return ls.name();
        return parsed;
    }

    private boolean isFalse(Object val) {
        return val instanceof Boolean b && !b;
    }

    private boolean schemeEqv(Object a, Object b) {
        if (a == b) return true;
        if (isNumber(a) && isNumber(b)) {
            try { return compareNumbers(a, b, "eqv?") == 0; } catch (EvalError e) { return false; }
        }
        if (a instanceof Boolean && b instanceof Boolean) return a.equals(b);
        if (a instanceof String && b instanceof String) return a.equals(b);
        if (a instanceof SchemeChar ca && b instanceof SchemeChar cb) return ca.value() == cb.value();
        return false;
    }

    private boolean schemeEqual(Object a, Object b) {
        return schemeEqualRec(a, b, java.util.Collections.newSetFromMap(new java.util.IdentityHashMap<>()));
    }

    private boolean schemeEqualRec(Object a, Object b, Set<Object> seen) {
        if (a == b) return true;
        if (isNumber(a) && isNumber(b)) {
            try { return compareNumbers(a, b, "equal?") == 0; } catch (EvalError e) { return false; }
        }
        if (a instanceof Boolean && b instanceof Boolean) return a.equals(b);
        if (a instanceof String && b instanceof String) return a.equals(b);
        if (a instanceof SchemeString sa && b instanceof SchemeString sb) return sa.value().equals(sb.value());
        if (a instanceof SchemeChar ca && b instanceof SchemeChar cb) return ca.value() == cb.value();
        if (a == Empty.NIL && b == Empty.NIL) return true;
        if (a instanceof Pair pa && b instanceof Pair pb) {
            if (!seen.add(pa)) return true;
            return schemeEqualRec(pa.car(), pb.car(), seen) && schemeEqualRec(pa.cdr(), pb.cdr(), seen);
        }
        if (a instanceof SchemeVector va && b instanceof SchemeVector vb) {
            if (va.data.length != vb.data.length) return false;
            for (int i = 0; i < va.data.length; i++) {
                if (!schemeEqualRec(va.data[i], vb.data[i], seen)) return false;
            }
            return true;
        }
        return false;
    }

    // Synchronous apply (for builtins that need it)
    private Object apply(Object proc, List<Object> args) throws EvalError {
        return trampoline(applyK(proc, args, 0, 0, BounceValue::new));
    }

    // --- Macro expansion helpers ---

    private Map<String, Object> matchPattern(List<?> pattern, List<?> input, List<String> literals) {
        Map<String, Object> bindings = new HashMap<>();
        int pi = 1, ii = 1; // skip macro name
        while (pi < pattern.size()) {
            String ps = symName(pattern.get(pi));
            if (pi + 1 < pattern.size() && "...".equals(symName(pattern.get(pi + 1)))) {
                if (ps == null) return null;
                List<Object> matches = new ArrayList<>();
                while (ii < input.size()) {
                    matches.add(input.get(ii));
                    ii++;
                }
                bindings.put(ps, matches);
                pi += 2;
            } else {
                if (ii >= input.size()) return null;
                if (ps != null && literals.contains(ps)) {
                    String is = symName(input.get(ii));
                    if (!ps.equals(is)) return null;
                } else if (ps != null) {
                    bindings.put(ps, input.get(ii));
                } else {
                    return null;
                }
                pi++;
                ii++;
            }
        }
        return ii == input.size() ? bindings : null;
    }

    private void collectEllipsisVars(List<?> pattern, Set<String> ellipsisVars) {
        for (int i = 1; i < pattern.size(); i++) {
            if (i + 1 < pattern.size() && "...".equals(symName(pattern.get(i + 1)))) {
                String v = symName(pattern.get(i));
                if (v != null) ellipsisVars.add(v);
            }
        }
    }

    @SuppressWarnings("unchecked")
    private Object expandTemplate(Object template, Map<String, Object> bindings, Set<String> ellipsisVars) {
        String s = symName(template);
        if (s != null) {
            if (bindings.containsKey(s) && !ellipsisVars.contains(s)) {
                return bindings.get(s);
            }
            return s;
        }
        if (template instanceof List<?> list) {
            List<Object> result = new ArrayList<>();
            for (int i = 0; i < list.size(); i++) {
                if (i + 1 < list.size() && "...".equals(symName(list.get(i + 1)))) {
                    Object subTemplate = list.get(i);
                    Set<String> usedEV = new HashSet<>();
                    findEllipsisVarsInTemplate(subTemplate, ellipsisVars, usedEV);
                    if (!usedEV.isEmpty()) {
                        String mainVar = usedEV.iterator().next();
                        List<Object> varList = (List<Object>) bindings.get(mainVar);
                        if (varList != null) {
                            for (int j = 0; j < varList.size(); j++) {
                                Map<String, Object> iterBindings = new HashMap<>(bindings);
                                for (String ev : usedEV) {
                                    List<Object> evList = (List<Object>) bindings.get(ev);
                                    if (evList != null && j < evList.size()) {
                                        iterBindings.put(ev, evList.get(j));
                                    }
                                }
                                Set<String> remainingEV = new HashSet<>(ellipsisVars);
                                remainingEV.removeAll(usedEV);
                                result.add(expandTemplate(subTemplate, iterBindings, remainingEV));
                            }
                        }
                    }
                    i++; // skip "..."
                } else {
                    result.add(expandTemplate(list.get(i), bindings, ellipsisVars));
                }
            }
            return result;
        }
        return template;
    }

    private void findEllipsisVarsInTemplate(Object template, Set<String> ellipsisVars, Set<String> found) {
        String s = symName(template);
        if (s != null && ellipsisVars.contains(s)) {
            found.add(s);
            return;
        }
        if (template instanceof List<?> list) {
            for (Object elem : list) {
                findEllipsisVarsInTemplate(elem, ellipsisVars, found);
            }
        }
    }

    private void collectFreeVars(Object template, Set<String> patVars, Set<String> freeVars) {
        String s = symName(template);
        if (s != null) {
            if (!patVars.contains(s) && !MACRO_SPECIAL_FORMS.contains(s) && !"...".equals(s)) {
                freeVars.add(s);
            }
            return;
        }
        if (template instanceof List<?> list) {
            for (Object elem : list) {
                collectFreeVars(elem, patVars, freeVars);
            }
        }
    }

    // --- Number helpers ---

    private boolean isNumber(Object o) {
        return o instanceof Long || o instanceof Double || o instanceof SchemeRational;
    }

    private boolean isExact(Object o) {
        return o instanceof Long || o instanceof SchemeRational;
    }

    private double toDouble(Object o) throws EvalError {
        if (o instanceof Long n) return (double) n;
        if (o instanceof Double d) return d;
        if (o instanceof SchemeRational r) return (double) r.num() / r.den();
        throw new EvalError("expected number");
    }

    private long[] toRational(Object o) {
        if (o instanceof Long n) return new long[]{n, 1};
        if (o instanceof SchemeRational r) return new long[]{r.num(), r.den()};
        return null;
    }

    private boolean hasInexact(List<Object> args) {
        for (Object a : args) if (a instanceof Double) return true;
        return false;
    }

    private Object addExact(Object a, Object b) throws EvalError {
        long[] ra = toRational(a), rb = toRational(b);
        return makeRational(ra[0] * rb[1] + rb[0] * ra[1], ra[1] * rb[1]);
    }

    private Object subExact(Object a, Object b) throws EvalError {
        long[] ra = toRational(a), rb = toRational(b);
        return makeRational(ra[0] * rb[1] - rb[0] * ra[1], ra[1] * rb[1]);
    }

    private Object mulExact(Object a, Object b) throws EvalError {
        long[] ra = toRational(a), rb = toRational(b);
        return makeRational(ra[0] * rb[0], ra[1] * rb[1]);
    }

    private Object divExact(Object a, Object b) throws EvalError {
        long[] ra = toRational(a), rb = toRational(b);
        return makeRational(ra[0] * rb[1], ra[1] * rb[0]);
    }

    private void checkNumber(Object o, String ctx) throws EvalError {
        if (!isNumber(o)) throw new EvalError(ctx + ": expected number, got " + schemeToString(o));
    }

    // --- Builtins ---

    private Object applyBuiltin(String name, List<Object> args) throws EvalError {
        return switch (name) {
            case "+" -> {
                for (Object a : args) checkNumber(a, "+");
                if (hasInexact(args)) {
                    double sum = 0;
                    for (Object a : args) sum += toDouble(a);
                    yield sum;
                }
                Object sum = 0L;
                for (Object a : args) sum = addExact(sum, a);
                yield sum;
            }
            case "-" -> {
                if (args.isEmpty()) throw new EvalError("-: need at least 1 argument");
                for (Object a : args) checkNumber(a, "-");
                if (hasInexact(args)) {
                    if (args.size() == 1) yield -toDouble(args.get(0));
                    double result = toDouble(args.get(0));
                    for (int i = 1; i < args.size(); i++) result -= toDouble(args.get(i));
                    yield result;
                }
                if (args.size() == 1) {
                    long[] r = toRational(args.get(0));
                    yield makeRational(-r[0], r[1]);
                }
                Object result = args.get(0);
                for (int i = 1; i < args.size(); i++) result = subExact(result, args.get(i));
                yield result;
            }
            case "*" -> {
                for (Object a : args) checkNumber(a, "*");
                if (hasInexact(args)) {
                    double prod = 1;
                    for (Object a : args) prod *= toDouble(a);
                    yield prod;
                }
                Object prod = 1L;
                for (Object a : args) prod = mulExact(prod, a);
                yield prod;
            }
            case "/" -> {
                if (args.isEmpty()) throw new EvalError("/: need at least 1 argument");
                for (Object a : args) checkNumber(a, "/");
                if (hasInexact(args)) {
                    double result = toDouble(args.get(0));
                    for (int i = 1; i < args.size(); i++) {
                        double d = toDouble(args.get(i));
                        if (d == 0) throw new EvalError("division by zero");
                        result /= d;
                    }
                    yield result;
                }
                if (args.size() == 1) {
                    yield divExact(1L, args.get(0));
                }
                Object result = args.get(0);
                for (int i = 1; i < args.size(); i++) result = divExact(result, args.get(i));
                yield result;
            }
            case "<" -> {
                checkMinArgs(args, 2, "<");
                yield compareNumbers(args.get(0), args.get(1), "<") < 0;
            }
            case ">" -> {
                checkMinArgs(args, 2, ">");
                yield compareNumbers(args.get(0), args.get(1), ">") > 0;
            }
            case "=" -> {
                checkMinArgs(args, 2, "=");
                yield compareNumbers(args.get(0), args.get(1), "=") == 0;
            }
            case "<=" -> {
                checkMinArgs(args, 2, "<=");
                yield compareNumbers(args.get(0), args.get(1), "<=") <= 0;
            }
            case ">=" -> {
                checkMinArgs(args, 2, ">=");
                yield compareNumbers(args.get(0), args.get(1), ">=") >= 0;
            }
            case "not" -> {
                checkMinArgs(args, 1, "not");
                yield isFalse(args.get(0));
            }
            case "cons" -> {
                checkMinArgs(args, 2, "cons");
                yield new Pair(args.get(0), args.get(1));
            }
            case "car" -> {
                checkMinArgs(args, 1, "car");
                if (!(args.get(0) instanceof Pair p)) throw new EvalError("car: expected pair");
                yield p.car();
            }
            case "cdr" -> {
                checkMinArgs(args, 1, "cdr");
                if (!(args.get(0) instanceof Pair p)) throw new EvalError("cdr: expected pair");
                yield p.cdr();
            }
            case "set-car!" -> {
                checkMinArgs(args, 2, "set-car!");
                if (!(args.get(0) instanceof Pair p)) throw new EvalError("set-car!: expected pair");
                p.setCar(args.get(1));
                yield VOID;
            }
            case "set-cdr!" -> {
                checkMinArgs(args, 2, "set-cdr!");
                if (!(args.get(0) instanceof Pair p)) throw new EvalError("set-cdr!: expected pair");
                p.setCdr(args.get(1));
                yield VOID;
            }
            case "caar", "cadr", "cdar", "cddr", "caddr", "cdddr", "caddar",
                 "caaar", "caadr", "cadar", "cdaar", "cdadr", "cddar",
                 "caaaar", "caaadr", "caadar", "caaddr", "cadaar", "cadadr", "cadddr",
                 "cdaaar", "cdaadr", "cdadar", "cdaddr", "cddaar", "cddadr", "cdddar", "cddddr" -> {
                checkMinArgs(args, 1, name);
                yield applyCxr(args.get(0), name);
            }
            case "for-each" -> {
                checkMinArgs(args, 2, "for-each");
                Object fn = args.get(0);
                Object lst = args.get(1);
                while (lst instanceof Pair p) {
                    List<Object> fnArgs = new ArrayList<>();
                    fnArgs.add(p.car());
                    apply(fn, fnArgs);
                    lst = p.cdr();
                }
                yield VOID;
            }
            case "reverse" -> {
                checkMinArgs(args, 1, "reverse");
                Object lst = args.get(0);
                Object result = Empty.NIL;
                while (lst instanceof Pair p) {
                    result = new Pair(p.car(), result);
                    lst = p.cdr();
                }
                yield result;
            }
            case "memq" -> {
                checkMinArgs(args, 2, "memq");
                Object obj = args.get(0);
                Object lst = args.get(1);
                while (lst instanceof Pair p) {
                    if (obj == p.car() || (obj instanceof String && obj.equals(p.car()))
                        || (obj instanceof Long && obj.equals(p.car()))
                        || (obj instanceof Boolean && obj.equals(p.car()))) {
                        yield lst;
                    }
                    lst = p.cdr();
                }
                yield false;
            }
            case "gcd" -> {
                checkMinArgs(args, 2, "gcd");
                if (!(args.get(0) instanceof Long a)) throw new EvalError("gcd: expected integer");
                if (!(args.get(1) instanceof Long b)) throw new EvalError("gcd: expected integer");
                yield gcd(a, b);
            }
            case "lcm" -> {
                checkMinArgs(args, 2, "lcm");
                if (!(args.get(0) instanceof Long a)) throw new EvalError("lcm: expected integer");
                if (!(args.get(1) instanceof Long b)) throw new EvalError("lcm: expected integer");
                long g = gcd(a, b);
                yield g == 0 ? 0L : Math.abs(a / g * b);
            }
            case "truncate" -> {
                checkMinArgs(args, 1, "truncate");
                Object v = args.get(0);
                if (v instanceof Long) yield v;
                if (v instanceof Double d) yield Long.valueOf((long) d.doubleValue());
                throw new EvalError("truncate: expected number");
            }
            case "round" -> {
                checkMinArgs(args, 1, "round");
                Object v = args.get(0);
                if (v instanceof Long) yield v;
                if (v instanceof Double d) yield Math.round(d);
                throw new EvalError("round: expected number");
            }
            case "make-string" -> {
                checkMinArgs(args, 1, "make-string");
                if (!(args.get(0) instanceof Long n)) throw new EvalError("make-string: expected integer");
                char fill = args.size() > 1 && args.get(1) instanceof SchemeChar c ? c.value() : ' ';
                char[] chars = new char[(int)(long)n];
                java.util.Arrays.fill(chars, fill);
                yield new SchemeString(new String(chars), false);
            }
            case "string" -> {
                StringBuilder sb = new StringBuilder();
                for (Object a : args) {
                    if (!(a instanceof SchemeChar c)) throw new EvalError("string: expected char");
                    sb.append(c.value());
                }
                yield new SchemeString(sb.toString(), false);
            }
            case "string>?" -> {
                checkMinArgs(args, 2, "string>?");
                if (!(args.get(0) instanceof SchemeString a)) throw new EvalError("string>?: expected string");
                if (!(args.get(1) instanceof SchemeString b)) throw new EvalError("string>?: expected string");
                yield a.value().compareTo(b.value()) > 0;
            }
            case "string<=?" -> {
                checkMinArgs(args, 2, "string<=?");
                if (!(args.get(0) instanceof SchemeString a)) throw new EvalError("string<=?: expected string");
                if (!(args.get(1) instanceof SchemeString b)) throw new EvalError("string<=?: expected string");
                yield a.value().compareTo(b.value()) <= 0;
            }
            case "string>=?" -> {
                checkMinArgs(args, 2, "string>=?");
                if (!(args.get(0) instanceof SchemeString a)) throw new EvalError("string>=?: expected string");
                if (!(args.get(1) instanceof SchemeString b)) throw new EvalError("string>=?: expected string");
                yield a.value().compareTo(b.value()) >= 0;
            }
            case "memv" -> {
                checkMinArgs(args, 2, "memv");
                Object obj = args.get(0);
                Object lst = args.get(1);
                while (lst instanceof Pair p) {
                    if (schemeEqv(obj, p.car())) yield lst;
                    lst = p.cdr();
                }
                yield false;
            }
            case "assq" -> {
                checkMinArgs(args, 2, "assq");
                Object key = args.get(0);
                Object lst = args.get(1);
                while (lst instanceof Pair p) {
                    if (p.car() instanceof Pair entry) {
                        Object ek = entry.car();
                        if (key == ek || (key instanceof String && key.equals(ek))
                            || (key instanceof Long && key.equals(ek))
                            || (key instanceof Boolean && key.equals(ek))) {
                            yield entry;
                        }
                    }
                    lst = p.cdr();
                }
                yield false;
            }
            case "assv" -> {
                checkMinArgs(args, 2, "assv");
                Object key = args.get(0);
                Object lst = args.get(1);
                while (lst instanceof Pair p) {
                    if (p.car() instanceof Pair entry) {
                        if (schemeEqv(key, entry.car())) yield entry;
                    }
                    lst = p.cdr();
                }
                yield false;
            }
            case "member" -> {
                checkMinArgs(args, 2, "member");
                Object obj = args.get(0);
                Object lst = args.get(1);
                while (lst instanceof Pair p) {
                    if (schemeEqual(obj, p.car())) {
                        yield lst;
                    }
                    lst = p.cdr();
                }
                yield false;
            }
            case "null?" -> {
                checkMinArgs(args, 1, "null?");
                yield args.get(0) == Empty.NIL;
            }
            case "list" -> {
                Object result = Empty.NIL;
                for (int i = args.size() - 1; i >= 0; i--) {
                    result = new Pair(args.get(i), result);
                }
                yield result;
            }
            case "length" -> {
                checkMinArgs(args, 1, "length");
                Object lst = args.get(0);
                long len = 0;
                while (lst instanceof Pair p) {
                    len++;
                    lst = p.cdr();
                }
                if (lst != Empty.NIL) throw new EvalError("length: not a proper list");
                yield len;
            }
            case "string?" -> {
                checkMinArgs(args, 1, "string?");
                yield args.get(0) instanceof SchemeString;
            }
            case "number?" -> {
                checkMinArgs(args, 1, "number?");
                yield isNumber(args.get(0));
            }
            case "boolean?" -> {
                checkMinArgs(args, 1, "boolean?");
                yield args.get(0) instanceof Boolean;
            }
            case "pair?" -> {
                checkMinArgs(args, 1, "pair?");
                yield args.get(0) instanceof Pair;
            }
            case "symbol?" -> {
                checkMinArgs(args, 1, "symbol?");
                yield args.get(0) instanceof String && !((String) args.get(0)).startsWith("builtin:");
            }
            case "append" -> {
                if (args.isEmpty()) yield Empty.NIL;
                Object result = args.get(args.size() - 1);
                for (int i = args.size() - 2; i >= 0; i--) {
                    Object lst = args.get(i);
                    List<Object> elems = new ArrayList<>();
                    while (lst instanceof Pair p) {
                        elems.add(p.car());
                        lst = p.cdr();
                    }
                    for (int j = elems.size() - 1; j >= 0; j--) {
                        result = new Pair(elems.get(j), result);
                    }
                }
                yield result;
            }
            case "display" -> {
                checkMinArgs(args, 1, "display");
                outputBuffer.append(displayToString(args.get(0)));
                yield VOID;
            }
            case "write" -> {
                checkMinArgs(args, 1, "write");
                outputBuffer.append(schemeToString(args.get(0)));
                yield VOID;
            }
            case "newline" -> {
                outputBuffer.append("\n");
                yield VOID;
            }
            case "string-append" -> {
                StringBuilder sb = new StringBuilder();
                for (Object a : args) {
                    if (!(a instanceof SchemeString s)) throw new EvalError("string-append: expected string");
                    sb.append(s.value());
                }
                yield new SchemeString(sb.toString());
            }
            case "string-length" -> {
                checkMinArgs(args, 1, "string-length");
                if (!(args.get(0) instanceof SchemeString s)) throw new EvalError("string-length: expected string");
                yield (long) s.value().length();
            }
            case "substring" -> {
                checkMinArgs(args, 3, "substring");
                if (!(args.get(0) instanceof SchemeString s)) throw new EvalError("substring: expected string");
                long start = asLong(args.get(1), "substring");
                long end = asLong(args.get(2), "substring");
                yield new SchemeString(s.value().substring((int) start, (int) end));
            }
            case "string->number" -> {
                checkMinArgs(args, 1, "string->number");
                if (!(args.get(0) instanceof SchemeString s)) throw new EvalError("string->number: expected string");
                String sv = s.value();
                try { yield Long.parseLong(sv); } catch (NumberFormatException e) { /* fall through */ }
                try {
                    if (sv.contains(".") || sv.contains("e") || sv.contains("E")) {
                        yield Double.parseDouble(sv);
                    }
                } catch (NumberFormatException e) { /* fall through */ }
                yield Boolean.FALSE;
            }
            case "number->string" -> {
                checkMinArgs(args, 1, "number->string");
                Object a = args.get(0);
                if (a instanceof Long n) yield new SchemeString(n.toString());
                if (a instanceof Double d) yield new SchemeString(doubleToString(d));
                if (a instanceof SchemeRational r) yield new SchemeString(r.num() + "/" + r.den());
                throw new EvalError("number->string: expected number");
            }
            case "symbol->string" -> {
                checkMinArgs(args, 1, "symbol->string");
                if (!(args.get(0) instanceof String s)) throw new EvalError("symbol->string: expected symbol");
                yield new SchemeString(s);
            }
            case "string->symbol" -> {
                checkMinArgs(args, 1, "string->symbol");
                if (!(args.get(0) instanceof SchemeString s)) throw new EvalError("string->symbol: expected string");
                yield s.value();
            }
            case "string-ref" -> {
                checkMinArgs(args, 2, "string-ref");
                if (!(args.get(0) instanceof SchemeString s)) throw new EvalError("string-ref: expected string");
                long idx = asLong(args.get(1), "string-ref");
                yield new SchemeChar(s.value().charAt((int) idx));
            }
            case "char?" -> {
                checkMinArgs(args, 1, "char?");
                yield args.get(0) instanceof SchemeChar;
            }
            case "string-copy" -> {
                checkMinArgs(args, 1, "string-copy");
                if (!(args.get(0) instanceof SchemeString s)) throw new EvalError("string-copy: expected string");
                yield new SchemeString(s.value(), false);
            }
            case "apply" -> {
                if (args.size() < 2) throw new EvalError("apply: expected at least 2 arguments");
                Object fn = args.get(0);
                Object lastArg = args.get(args.size() - 1);
                List<Object> allArgs = new ArrayList<>();
                for (int i = 1; i < args.size() - 1; i++) {
                    allArgs.add(args.get(i));
                }
                Object lst = lastArg;
                while (lst instanceof Pair p) {
                    allArgs.add(p.car());
                    lst = p.cdr();
                }
                yield apply(fn, allArgs);
            }
            case "string-set!" -> {
                checkMinArgs(args, 3, "string-set!");
                if (!(args.get(0) instanceof SchemeString s)) throw new EvalError("string-set!: expected string");
                if (s.isImmutable()) throw new EvalError("string-set!: strings are immutable");
                if (!(args.get(1) instanceof Long idx)) throw new EvalError("string-set!: expected integer index");
                if (!(args.get(2) instanceof SchemeChar c)) throw new EvalError("string-set!: expected char");
                int i = idx.intValue();
                if (i < 0 || i >= s.length()) throw new EvalError("string-set!: index out of range");
                s.setChar(i, c.value());
                yield Empty.NIL;
            }
            case "string->list" -> {
                checkMinArgs(args, 1, "string->list");
                if (!(args.get(0) instanceof SchemeString s)) throw new EvalError("string->list: expected string");
                Object result = Empty.NIL;
                for (int i = s.length() - 1; i >= 0; i--) {
                    result = new Pair(new SchemeChar(s.charAt(i)), result);
                }
                yield result;
            }
            case "list->string" -> {
                checkMinArgs(args, 1, "list->string");
                StringBuilder sb = new StringBuilder();
                Object lst = args.get(0);
                while (lst instanceof Pair p) {
                    if (!(p.car() instanceof SchemeChar c)) throw new EvalError("list->string: expected char in list");
                    sb.append(c.value());
                    lst = p.cdr();
                }
                yield new SchemeString(sb.toString());
            }
            case "char->integer" -> {
                checkMinArgs(args, 1, "char->integer");
                if (!(args.get(0) instanceof SchemeChar c)) throw new EvalError("char->integer: expected char");
                yield (long) c.value();
            }
            case "integer->char" -> {
                checkMinArgs(args, 1, "integer->char");
                long code = asLong(args.get(0), "integer->char");
                yield new SchemeChar((char) code);
            }
            case "abs" -> {
                checkMinArgs(args, 1, "abs");
                yield Math.abs(asLong(args.get(0), "abs"));
            }
            case "modulo" -> {
                checkMinArgs(args, 2, "modulo");
                long a = asLong(args.get(0), "modulo");
                long b = asLong(args.get(1), "modulo");
                yield Math.floorMod(a, b);
            }
            case "remainder" -> {
                checkMinArgs(args, 2, "remainder");
                long a = asLong(args.get(0), "remainder");
                long b = asLong(args.get(1), "remainder");
                yield a % b;
            }
            case "quotient" -> {
                checkMinArgs(args, 2, "quotient");
                long a = asLong(args.get(0), "quotient");
                long b = asLong(args.get(1), "quotient");
                yield a / b;
            }
            case "min" -> {
                checkMinArgs(args, 1, "min");
                long result = asLong(args.get(0), "min");
                for (int i = 1; i < args.size(); i++) {
                    long v = asLong(args.get(i), "min");
                    if (v < result) result = v;
                }
                yield result;
            }
            case "max" -> {
                checkMinArgs(args, 1, "max");
                long result = asLong(args.get(0), "max");
                for (int i = 1; i < args.size(); i++) {
                    long v = asLong(args.get(i), "max");
                    if (v > result) result = v;
                }
                yield result;
            }
            case "expt" -> {
                checkMinArgs(args, 2, "expt");
                long base = asLong(args.get(0), "expt");
                long exp = asLong(args.get(1), "expt");
                long result = 1;
                for (long i = 0; i < exp; i++) result *= base;
                yield result;
            }
            case "zero?" -> {
                checkMinArgs(args, 1, "zero?");
                yield asLong(args.get(0), "zero?") == 0;
            }
            case "positive?" -> {
                checkMinArgs(args, 1, "positive?");
                yield asLong(args.get(0), "positive?") > 0;
            }
            case "negative?" -> {
                checkMinArgs(args, 1, "negative?");
                yield asLong(args.get(0), "negative?") < 0;
            }
            case "odd?" -> {
                checkMinArgs(args, 1, "odd?");
                yield asLong(args.get(0), "odd?") % 2 != 0;
            }
            case "even?" -> {
                checkMinArgs(args, 1, "even?");
                yield asLong(args.get(0), "even?") % 2 == 0;
            }
            case "list-ref" -> {
                checkMinArgs(args, 2, "list-ref");
                Object lst = args.get(0);
                long idx = asLong(args.get(1), "list-ref");
                for (long i = 0; i < idx; i++) {
                    if (!(lst instanceof Pair p)) throw new EvalError("list-ref: index out of range");
                    lst = p.cdr();
                }
                if (!(lst instanceof Pair p)) throw new EvalError("list-ref: index out of range");
                yield p.car();
            }
            case "list-tail" -> {
                checkMinArgs(args, 2, "list-tail");
                Object lst = args.get(0);
                long idx = asLong(args.get(1), "list-tail");
                for (long i = 0; i < idx; i++) {
                    if (!(lst instanceof Pair p)) throw new EvalError("list-tail: index out of range");
                    lst = p.cdr();
                }
                yield lst;
            }
            case "list?" -> {
                checkMinArgs(args, 1, "list?");
                Object slow = args.get(0);
                Object fast = slow;
                while (fast instanceof Pair fp) {
                    fast = fp.cdr();
                    if (!(fast instanceof Pair fp2)) break;
                    fast = fp2.cdr();
                    slow = ((Pair) slow).cdr();
                    if (slow == fast) yield false; // cycle detected
                }
                yield fast == Empty.NIL;
            }
            case "assoc" -> {
                checkMinArgs(args, 2, "assoc");
                Object key = args.get(0);
                Object lst = args.get(1);
                while (lst instanceof Pair p) {
                    if (p.car() instanceof Pair entry) {
                        if (schemeEqual(key, entry.car())) yield entry;
                    }
                    lst = p.cdr();
                }
                yield Boolean.FALSE;
            }
            case "map" -> {
                checkMinArgs(args, 2, "map");
                Object fn = args.get(0);
                List<Object> lists = new ArrayList<>();
                for (int i = 1; i < args.size(); i++) lists.add(args.get(i));
                List<Object> results = new ArrayList<>();
                while (true) {
                    boolean done = false;
                    for (Object l : lists) {
                        if (!(l instanceof Pair)) { done = true; break; }
                    }
                    if (done) break;
                    List<Object> mapArgs = new ArrayList<>();
                    List<Object> nextLists = new ArrayList<>();
                    for (Object l : lists) {
                        Pair p = (Pair) l;
                        mapArgs.add(p.car());
                        nextLists.add(p.cdr());
                    }
                    results.add(apply(fn, mapArgs));
                    lists = nextLists;
                }
                Object result = Empty.NIL;
                for (int i = results.size() - 1; i >= 0; i--) {
                    result = new Pair(results.get(i), result);
                }
                yield result;
            }
            case "equal?" -> {
                checkMinArgs(args, 2, "equal?");
                yield schemeEqual(args.get(0), args.get(1));
            }
            case "eq?" -> {
                checkMinArgs(args, 2, "eq?");
                Object a = args.get(0), b = args.get(1);
                yield a == b || a.equals(b);
            }
            case "char-alphabetic?" -> {
                checkMinArgs(args, 1, "char-alphabetic?");
                if (!(args.get(0) instanceof SchemeChar c)) throw new EvalError("char-alphabetic?: expected char");
                yield Character.isLetter(c.value());
            }
            case "char-numeric?" -> {
                checkMinArgs(args, 1, "char-numeric?");
                if (!(args.get(0) instanceof SchemeChar c)) throw new EvalError("char-numeric?: expected char");
                yield Character.isDigit(c.value());
            }
            case "char-upcase" -> {
                checkMinArgs(args, 1, "char-upcase");
                if (!(args.get(0) instanceof SchemeChar c)) throw new EvalError("char-upcase: expected char");
                yield new SchemeChar(Character.toUpperCase(c.value()));
            }
            case "char-downcase" -> {
                checkMinArgs(args, 1, "char-downcase");
                if (!(args.get(0) instanceof SchemeChar c)) throw new EvalError("char-downcase: expected char");
                yield new SchemeChar(Character.toLowerCase(c.value()));
            }
            case "char=?" -> {
                checkMinArgs(args, 2, "char=?");
                if (!(args.get(0) instanceof SchemeChar a)) throw new EvalError("char=?: expected char");
                if (!(args.get(1) instanceof SchemeChar b)) throw new EvalError("char=?: expected char");
                yield a.value() == b.value();
            }
            case "char<?" -> {
                checkMinArgs(args, 2, "char<?");
                if (!(args.get(0) instanceof SchemeChar a)) throw new EvalError("char<?: expected char");
                if (!(args.get(1) instanceof SchemeChar b)) throw new EvalError("char<?: expected char");
                yield a.value() < b.value();
            }
            case "string=?" -> {
                checkMinArgs(args, 2, "string=?");
                if (!(args.get(0) instanceof SchemeString a)) throw new EvalError("string=?: expected string");
                if (!(args.get(1) instanceof SchemeString b)) throw new EvalError("string=?: expected string");
                yield a.value().equals(b.value());
            }
            case "string<?" -> {
                checkMinArgs(args, 2, "string<?");
                if (!(args.get(0) instanceof SchemeString a)) throw new EvalError("string<?: expected string");
                if (!(args.get(1) instanceof SchemeString b)) throw new EvalError("string<?: expected string");
                yield a.value().compareTo(b.value()) < 0;
            }
            case "string-ci=?" -> {
                checkMinArgs(args, 2, "string-ci=?");
                if (!(args.get(0) instanceof SchemeString a)) throw new EvalError("string-ci=?: expected string");
                if (!(args.get(1) instanceof SchemeString b)) throw new EvalError("string-ci=?: expected string");
                yield a.value().equalsIgnoreCase(b.value());
            }
            case "string-upcase" -> {
                checkMinArgs(args, 1, "string-upcase");
                if (!(args.get(0) instanceof SchemeString s)) throw new EvalError("string-upcase: expected string");
                yield new SchemeString(s.value().toUpperCase());
            }
            case "string-downcase" -> {
                checkMinArgs(args, 1, "string-downcase");
                if (!(args.get(0) instanceof SchemeString s)) throw new EvalError("string-downcase: expected string");
                yield new SchemeString(s.value().toLowerCase());
            }
            case "exact?" -> {
                checkMinArgs(args, 1, "exact?");
                yield isExact(args.get(0));
            }
            case "inexact?" -> {
                checkMinArgs(args, 1, "inexact?");
                yield args.get(0) instanceof Double;
            }
            case "exact->inexact" -> {
                checkMinArgs(args, 1, "exact->inexact");
                yield toDouble(args.get(0));
            }
            case "inexact->exact" -> {
                checkMinArgs(args, 1, "inexact->exact");
                Object a = args.get(0);
                if (isExact(a)) yield a;
                double d = toDouble(a);
                if (d == Math.floor(d) && !Double.isInfinite(d)) yield (long) d;
                long den = 1;
                double v = d;
                while (v != Math.floor(v) && den < 1_000_000_000L) {
                    v *= 10;
                    den *= 10;
                }
                yield makeRational((long) v, den);
            }
            case "numerator" -> {
                checkMinArgs(args, 1, "numerator");
                Object a = args.get(0);
                if (a instanceof Long n) yield n;
                if (a instanceof SchemeRational r) yield r.num();
                throw new EvalError("numerator: expected rational");
            }
            case "denominator" -> {
                checkMinArgs(args, 1, "denominator");
                Object a = args.get(0);
                if (a instanceof Long) yield 1L;
                if (a instanceof SchemeRational r) yield r.den();
                throw new EvalError("denominator: expected rational");
            }
            case "integer?" -> {
                checkMinArgs(args, 1, "integer?");
                Object a = args.get(0);
                if (a instanceof Long) yield true;
                if (a instanceof SchemeRational) yield false;
                if (a instanceof Double d) yield d == Math.floor(d) && !Double.isInfinite(d);
                yield false;
            }
            case "rational?" -> {
                checkMinArgs(args, 1, "rational?");
                yield isExact(args.get(0));
            }
            case "procedure?" -> {
                checkMinArgs(args, 1, "procedure?");
                Object a = args.get(0);
                yield a instanceof Lambda || a instanceof CaseLambda
                    || (a instanceof String s && s.startsWith("builtin:"))
                    || a instanceof java.util.function.Function
                    || a instanceof SchemeContinuation
                    || a == CALLCC_PROC;
            }
            case "eqv?" -> {
                checkMinArgs(args, 2, "eqv?");
                yield schemeEqv(args.get(0), args.get(1));
            }
            case "vector" -> {
                yield new SchemeVector(args.toArray());
            }
            case "make-vector" -> {
                checkMinArgs(args, 1, "make-vector");
                int len = (int) asLong(args.get(0), "make-vector");
                Object fill = args.size() >= 2 ? args.get(1) : 0L;
                Object[] data = new Object[len];
                java.util.Arrays.fill(data, fill);
                yield new SchemeVector(data);
            }
            case "vector-ref" -> {
                checkMinArgs(args, 2, "vector-ref");
                if (!(args.get(0) instanceof SchemeVector v)) throw new EvalError("vector-ref: expected vector");
                int idx = (int) asLong(args.get(1), "vector-ref");
                yield v.data[idx];
            }
            case "vector-set!" -> {
                checkMinArgs(args, 3, "vector-set!");
                if (!(args.get(0) instanceof SchemeVector v)) throw new EvalError("vector-set!: expected vector");
                int idx = (int) asLong(args.get(1), "vector-set!");
                v.data[idx] = args.get(2);
                yield VOID;
            }
            case "vector-length" -> {
                checkMinArgs(args, 1, "vector-length");
                if (!(args.get(0) instanceof SchemeVector v)) throw new EvalError("vector-length: expected vector");
                yield (long) v.data.length;
            }
            case "vector?" -> {
                checkMinArgs(args, 1, "vector?");
                yield args.get(0) instanceof SchemeVector;
            }
            case "vector->list" -> {
                checkMinArgs(args, 1, "vector->list");
                if (!(args.get(0) instanceof SchemeVector v)) throw new EvalError("vector->list: expected vector");
                Object result = Empty.NIL;
                for (int i = v.data.length - 1; i >= 0; i--) {
                    result = new Pair(v.data[i], result);
                }
                yield result;
            }
            case "list->vector" -> {
                checkMinArgs(args, 1, "list->vector");
                List<Object> elems = new ArrayList<>();
                Object lst = args.get(0);
                while (lst instanceof Pair p) {
                    elems.add(p.car());
                    lst = p.cdr();
                }
                yield new SchemeVector(elems.toArray());
            }
            default -> throw new EvalError("unbound variable: " + name);
        };
    }

    private long asLong(Object val, String context) throws EvalError {
        if (val instanceof Long n) return n;
        if (val instanceof SchemeRational r) {
            if (r.den() == 1) return r.num();
            throw new EvalError(context + ": expected integer, got " + schemeToString(val));
        }
        if (val instanceof Double d) {
            if (d == Math.floor(d) && !Double.isInfinite(d)) return (long) d.doubleValue();
            throw new EvalError(context + ": expected integer, got " + schemeToString(val));
        }
        throw new EvalError(context + ": expected number, got " + schemeToString(val));
    }

    private Object applyCxr(Object val, String name) throws EvalError {
        for (int i = name.length() - 2; i >= 1; i--) {
            if (!(val instanceof Pair p)) throw new EvalError(name + ": expected pair");
            val = (name.charAt(i) == 'a') ? p.car() : p.cdr();
        }
        return val;
    }

    private void checkMinArgs(List<Object> args, int min, String name) throws EvalError {
        if (args.size() < min) throw new EvalError(name + ": expected at least " + min + " arguments");
    }

    private int compareNumbers(Object a, Object b, String ctx) throws EvalError {
        checkNumber(a, ctx);
        checkNumber(b, ctx);
        if (isExact(a) && isExact(b)) {
            long[] ra = toRational(a), rb = toRational(b);
            long lhs = ra[0] * rb[1], rhs = rb[0] * ra[1];
            return Long.compare(lhs, rhs);
        }
        return Double.compare(toDouble(a), toDouble(b));
    }
}
