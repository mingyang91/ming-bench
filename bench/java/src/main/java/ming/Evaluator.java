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
    private int stepLimit = -1; // -1 means unlimited

    public String evalStr(String input) throws EvalError {
        outputBuffer.setLength(0);
        List<Object> exprs = parse(input);
        if (exprs.isEmpty()) return schemeToString(VOID);
        Object result = trampoline(evalSeqK(exprs, 0, globalEnv, BounceValue::new));
        return schemeToString(result);
    }

    public String evalStrWithLimit(String input, int maxSteps) throws EvalError {
        outputBuffer.setLength(0);
        stepLimit = maxSteps;
        try {
            List<Object> exprs = parse(input);
            if (exprs.isEmpty()) return schemeToString(VOID);
            Object result = trampoline(evalSeqK(exprs, 0, globalEnv, BounceValue::new));
            return schemeToString(result);
        } finally {
            stepLimit = -1;
        }
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
        while (true) {
            if (b instanceof BounceValue bv) {
                return bv.value();
            }
            if (stepLimit >= 0) {
                if (--stepLimit < 0) {
                    throw new EvalError("step limit exceeded");
                }
            }
            BounceThunk bt = (BounceThunk) b;
            try {
                b = bt.thunk().get();
            } catch (SchemeRaisedException sre) {
                if (exceptionHandlerStack.isEmpty()) {
                    throw new EvalError("unhandled exception: " + schemeToString(sre.value));
                }
                ExceptionHandler eh = exceptionHandlerStack.remove(exceptionHandlerStack.size() - 1);
                List<WindEntry> currentWinding = new ArrayList<>(windingStack);
                // Wrap in bounce so any SchemeRaisedException thrown during
                // wind transition or handler dispatch is caught by the next
                // iteration of this loop, not lost in this catch block.
                b = bounce(() -> doWindTransition(currentWinding, eh.savedWinding, sre.value, eh.onException));
            }
        }
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

    // Macro transformer from syntax-case (lambda-based)
    record MacroTransformer(Lambda lambda, Env defEnv) {}

    private static final Set<String> MACRO_SPECIAL_FORMS = Set.of(
        "quote", "quasiquote", "if", "define", "lambda", "case-lambda", "and", "begin", "let", "let*", "cond", "set!", "or",
        "define-syntax", "syntax-rules", "syntax-case", "syntax", "with-syntax",
        "letrec", "letrec*", "case", "do",
        "call/cc", "call-with-current-continuation",
        "dynamic-wind",
        "guard", "with-exception-handler"
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

    // dynamic-wind support
    record WindEntry(Object inThunk, Object outThunk) {}
    private final List<WindEntry> windingStack = new ArrayList<>();

    // Exception handler stack for raise/guard/with-exception-handler
    private final List<ExceptionHandler> exceptionHandlerStack = new ArrayList<>();

    // Stack of syntax-case pattern variable names (for syntax template expansion)
    private final List<Set<String>> syntaxCasePatVarStack = new ArrayList<>();

    static class ExceptionHandler {
        final List<WindEntry> savedWinding;
        final Cont onException; // called with exception value after unwinding
        ExceptionHandler(List<WindEntry> savedWinding, Cont onException) {
            this.savedWinding = savedWinding;
            this.onException = onException;
        }
    }

    // Java exception used to propagate Scheme raise through the trampoline
    static class SchemeRaisedException extends RuntimeException {
        final Object value;
        SchemeRaisedException(Object value) {
            super("scheme raise", null, true, false);
            this.value = value;
        }
    }

    static class SchemeContinuation {
        final Cont k;
        final List<WindEntry> savedWinding;
        SchemeContinuation(Cont k, List<WindEntry> savedWinding) {
            this.k = k;
            this.savedWinding = savedWinding;
        }
    }

    static final Object CALLCC_PROC = new Object() {
        @Override public String toString() { return "#<procedure>"; }
    };

    // Multiple values wrapper
    record SchemeValues(List<Object> values) {}

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
                "cdaaar", "cdaadr", "cdadar", "cdaddr", "cddaar", "cddadr", "cdddar", "cddddr",
                "raise", "error",
                "values", "call-with-values",
                "syntax->datum", "datum->syntax")) {
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
        if (val instanceof MacroTransformer) return "#<syntax>";
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

    enum TokenType { LPAREN, RPAREN, QUOTE, SYNTAX_QUOTE, QUASIQUOTE, UNQUOTE, UNQUOTE_SPLICING, STRING, ATOM }

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
            if (c == '`') { tokens.add(new Token(TokenType.QUASIQUOTE, "`", line, col)); i++; col++; continue; }
            if (c == ',') {
                if (i + 1 < len && input.charAt(i + 1) == '@') {
                    tokens.add(new Token(TokenType.UNQUOTE_SPLICING, ",@", line, col));
                    i += 2; col += 2;
                } else {
                    tokens.add(new Token(TokenType.UNQUOTE, ",", line, col));
                    i++; col++;
                }
                continue;
            }
            if (c == '#' && i + 1 < len && input.charAt(i + 1) == '\'') {
                tokens.add(new Token(TokenType.SYNTAX_QUOTE, "#'", line, col));
                i += 2; col += 2; continue;
            }
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
            case SYNTAX_QUOTE -> {
                Object quoted = parseExpr(tokens, pos);
                LocatedList q = new LocatedList(tok.line(), tok.col());
                q.add("syntax");
                q.add(quoted);
                yield q;
            }
            case QUASIQUOTE -> {
                Object quoted = parseExpr(tokens, pos);
                LocatedList q = new LocatedList(tok.line(), tok.col());
                q.add("quasiquote");
                q.add(quoted);
                yield q;
            }
            case UNQUOTE -> {
                Object quoted = parseExpr(tokens, pos);
                LocatedList q = new LocatedList(tok.line(), tok.col());
                q.add("unquote");
                q.add(quoted);
                yield q;
            }
            case UNQUOTE_SPLICING -> {
                Object quoted = parseExpr(tokens, pos);
                LocatedList q = new LocatedList(tok.line(), tok.col());
                q.add("unquote-splicing");
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
                    case "quasiquote" -> {
                        if (list.size() != 2) throw errAt(eline, ecol, "quasiquote: expected 1 argument");
                        return expandQuasiquote(list.get(1), env, k);
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
                            throw errAt(eline, ecol, "define-syntax: bad syntax");
                        String trSym = symName(tlist.get(0));
                        // syntax-rules transformer
                        if ("syntax-rules".equals(trSym)) {
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
                        // lambda transformer (syntax-case based)
                        if ("lambda".equals(trSym)) {
                            final Env defEnv = env;
                            return bounce(() -> evalK(transformer, env, val -> {
                                if (!(val instanceof Lambda lam))
                                    throw errAt(el, ec, "define-syntax: transformer must be a procedure");
                                defEnv.define(macroName, new MacroTransformer(lam, defEnv));
                                return k.apply(VOID);
                            }));
                        }
                        throw errAt(eline, ecol, "define-syntax: expected syntax-rules or lambda");
                    }
                    case "syntax-case" -> {
                        // (syntax-case expr (literals) clause ...)
                        // clause = (pattern body) or (pattern fender body)
                        if (list.size() < 4) throw errAt(eline, ecol, "syntax-case: bad syntax");
                        if (!(list.get(2) instanceof List<?> litList))
                            throw errAt(eline, ecol, "syntax-case: expected literal list");
                        List<String> lits = new ArrayList<>();
                        for (Object l : litList) {
                            String ln = symName(l);
                            if (ln != null) lits.add(ln);
                        }
                        return bounce(() -> evalK(list.get(1), env, stxVal -> {
                            // Convert the syntax object to a parsed form for matching
                            List<Object> inputForm = schemeValueToParseForm(stxVal);
                            return evalSyntaxCaseClausesK(inputForm, stxVal, lits, list, 3, env, el, ec, k);
                        }));
                    }
                    case "syntax" -> {
                        // (syntax template) — a.k.a. #'template
                        if (list.size() != 2) throw errAt(eline, ecol, "syntax: expected 1 argument");
                        Object template = list.get(1);
                        // Collect all pattern variables from the syntax-case stack
                        Set<String> patVars = new HashSet<>();
                        for (Set<String> s : syntaxCasePatVarStack) patVars.addAll(s);
                        // Expand the template using pattern variables from environment
                        Object expanded = expandSyntaxTemplate(template, env, patVars);
                        return k.apply(expanded);
                    }
                    case "with-syntax" -> {
                        // (with-syntax ((pat expr) ...) body ...)
                        if (list.size() < 3) throw errAt(eline, ecol, "with-syntax: bad syntax");
                        if (!(list.get(1) instanceof List<?> bindings))
                            throw errAt(eline, ecol, "with-syntax: expected bindings");
                        List<Object> body = new ArrayList<>();
                        for (int i = 2; i < list.size(); i++) body.add(list.get(i));
                        return evalWithSyntaxBindingsK(bindings, 0, env, new HashSet<>(), el, ec, body, k);
                    }
                    case "call/cc", "call-with-current-continuation" -> {
                        if (list.size() != 2) throw errAt(eline, ecol, "call/cc: expected 1 argument");
                        return bounce(() -> evalK(list.get(1), env, proc -> {
                            SchemeContinuation cont = new SchemeContinuation(k, new ArrayList<>(windingStack));
                            return applyK(proc, List.of(cont), el, ec, k);
                        }));
                    }
                    case "dynamic-wind" -> {
                        if (list.size() != 4) throw errAt(eline, ecol, "dynamic-wind: expected 3 arguments");
                        return bounce(() -> evalK(list.get(1), env, inThunk ->
                            bounce(() -> evalK(list.get(2), env, bodyThunk ->
                                bounce(() -> evalK(list.get(3), env, outThunk ->
                                    dynamicWindK(inThunk, bodyThunk, outThunk, el, ec, k)
                                ))
                            ))
                        ));
                    }
                    case "with-exception-handler" -> {
                        if (list.size() != 3) throw errAt(eline, ecol, "with-exception-handler: expected 2 arguments");
                        return bounce(() -> evalK(list.get(1), env, handler ->
                            bounce(() -> evalK(list.get(2), env, thunk ->
                                withExceptionHandlerK(handler, thunk, el, ec, k)
                            ))
                        ));
                    }
                    case "guard" -> {
                        // (guard (var clause ...) body ...)
                        if (list.size() < 3) throw errAt(eline, ecol, "guard: bad syntax");
                        if (!(list.get(1) instanceof List<?> clauseList) || clauseList.isEmpty())
                            throw errAt(eline, ecol, "guard: bad syntax");
                        String guardVar = symName(clauseList.get(0));
                        if (guardVar == null) throw errAt(eline, ecol, "guard: expected variable name");
                        // clauses are clauseList[1..]
                        List<Object> clauses = new ArrayList<>();
                        for (int i = 1; i < clauseList.size(); i++) clauses.add(clauseList.get(i));
                        // body is list[2..]
                        List<Object> body = new ArrayList<>();
                        for (int i = 2; i < list.size(); i++) body.add(list.get(i));
                        return evalGuardK(guardVar, clauses, body, env, el, ec, k);
                    }
                }

                // Check for macro expansion
                {
                    Object maybeMacro = null;
                    try { maybeMacro = env.lookup(sym); } catch (EvalError ignored) {}
                    if (maybeMacro instanceof SyntaxRules sr) {
                        return applyMacroK(sr, list, env, k);
                    }
                    if (maybeMacro instanceof MacroTransformer mt) {
                        // Convert the input form to a Scheme value (proper list) and call the transformer
                        Object stxObj = toSchemeValue(list);
                        return applyK(mt.lambda(), List.of(stxObj), el, ec, expanded -> {
                            // The transformer returns a syntax object; convert back to parsed form and eval
                            Object parsedForm = schemeValueToEvalForm(expanded);
                            return bounce(() -> evalK(parsedForm, env, k));
                        });
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

    // --- dynamic-wind support ---

    private Bounce dynamicWindK(Object inThunk, Object bodyThunk, Object outThunk, int el, int ec, Cont k) throws EvalError {
        // Call in-thunk
        return applyK(inThunk, List.of(), el, ec, _in -> {
            // Push wind entry, call body
            WindEntry entry = new WindEntry(inThunk, outThunk);
            windingStack.add(entry);
            return applyK(bodyThunk, List.of(), el, ec, bodyResult -> {
                // Pop wind entry, call out-thunk, return body result
                windingStack.remove(windingStack.size() - 1);
                return applyK(outThunk, List.of(), el, ec, _out -> k.apply(bodyResult));
            });
        });
    }

    // Transition from current winding to target winding (for continuation invocation)
    private Bounce doWindTransition(List<WindEntry> from, List<WindEntry> to, Object val, Cont k) throws EvalError {
        // Find common prefix length (by identity)
        int common = 0;
        int minLen = Math.min(from.size(), to.size());
        for (int i = 0; i < minLen; i++) {
            if (from.get(i) == to.get(i)) common++;
            else break;
        }
        // Unwind: call out-thunks from innermost to common prefix
        // Then rewind: call in-thunks from common prefix to target
        return doUnwind(from, common, from.size() - 1, to, val, k);
    }

    private Bounce doUnwind(List<WindEntry> from, int common, int idx, List<WindEntry> to, Object val, Cont k) throws EvalError {
        if (idx < common) {
            // Done unwinding, now rewind
            windingStack.clear();
            windingStack.addAll(to.subList(0, common));
            return doRewind(to, common, val, k);
        }
        WindEntry entry = from.get(idx);
        windingStack.remove(windingStack.size() - 1);
        return applyK(entry.outThunk(), List.of(), 0, 0, _out ->
            doUnwind(from, common, idx - 1, to, val, k)
        );
    }

    private Bounce doRewind(List<WindEntry> to, int idx, Object val, Cont k) throws EvalError {
        if (idx >= to.size()) {
            // Done rewinding, invoke continuation
            return k.apply(val);
        }
        WindEntry entry = to.get(idx);
        return applyK(entry.inThunk(), List.of(), 0, 0, _in -> {
            windingStack.add(entry);
            return doRewind(to, idx + 1, val, k);
        });
    }

    // --- Exception handling (raise/guard/with-exception-handler) ---

    private Bounce withExceptionHandlerK(Object handler, Object thunk, int el, int ec, Cont k) throws EvalError {
        int handlerStackSize = exceptionHandlerStack.size();
        Cont onException = unwoundVal -> applyK(handler, List.of(unwoundVal), el, ec, handlerResult -> k.apply(handlerResult));
        ExceptionHandler eh = new ExceptionHandler(new ArrayList<>(windingStack), onException);
        exceptionHandlerStack.add(eh);
        return applyK(thunk, List.of(), el, ec, bodyResult -> {
            if (exceptionHandlerStack.size() > handlerStackSize) {
                exceptionHandlerStack.remove(handlerStackSize);
            }
            return k.apply(bodyResult);
        });
    }

    @SuppressWarnings("unchecked")
    private Bounce evalGuardK(String guardVar, List<Object> clauses, List<Object> body, Env env, int el, int ec, Cont k) throws EvalError {
        int handlerStackSize = exceptionHandlerStack.size();

        Cont onException = unwoundVal -> evalGuardClausesK(guardVar, unwoundVal, clauses, 0, env, el, ec, k);
        ExceptionHandler eh = new ExceptionHandler(new ArrayList<>(windingStack), onException);
        exceptionHandlerStack.add(eh);

        return evalSeqK(body, 0, env, bodyResult -> {
            // Normal completion — remove our handler and return result
            if (exceptionHandlerStack.size() > handlerStackSize) {
                exceptionHandlerStack.remove(handlerStackSize);
            }
            return bounce(() -> k.apply(bodyResult));
        });
    }

    private Bounce evalGuardClausesK(String guardVar, Object exnVal, List<Object> clauses, int idx, Env env, int el, int ec, Cont k) throws EvalError {
        if (idx >= clauses.size()) {
            // No clause matched, re-raise
            throw new SchemeRaisedException(exnVal);
        }
        Object clause = clauses.get(idx);
        if (!(clause instanceof List<?> clauseList) || clauseList.isEmpty())
            throw errAt(el, ec, "guard: bad clause");

        // Check for else clause
        String testSym = symName(clauseList.get(0));
        if ("else".equals(testSym)) {
            Env clauseEnv = new Env(env);
            clauseEnv.define(guardVar, exnVal);
            if (clauseList.size() == 1) return k.apply(VOID);
            return evalSeqK(clauseList, 1, clauseEnv, k);
        }

        // Evaluate test with guardVar bound to exception value
        Env testEnv = new Env(env);
        testEnv.define(guardVar, exnVal);
        return bounce(() -> evalK(clauseList.get(0), testEnv, testResult -> {
            if (!isFalse(testResult)) {
                // Clause matched — if there's a body, evaluate it; otherwise return test result
                if (clauseList.size() == 1) return k.apply(testResult);
                return evalSeqK(clauseList, 1, testEnv, k);
            }
            // Try next clause
            return evalGuardClausesK(guardVar, exnVal, clauses, idx + 1, env, el, ec, k);
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

    // Quasiquote expansion
    private Bounce expandQuasiquote(Object template, Env env, Cont k) throws EvalError {
        if (!(template instanceof List<?> list)) {
            // Atom — just quote it
            return k.apply(toSchemeValue(template));
        }
        if (list.size() == 2 && "unquote".equals(symName(list.get(0)))) {
            return bounce(() -> evalK(list.get(1), env, k));
        }
        // Check for dot notation: (a b . c) where second-to-last is "."
        int dotIdx = -1;
        for (int i = 0; i < list.size(); i++) {
            if (".".equals(symName(list.get(i)))) { dotIdx = i; break; }
        }
        if (dotIdx >= 0 && dotIdx == list.size() - 2) {
            // Dotted list: expand elements before dot, then expand the tail
            return expandQQDotted(list, 0, dotIdx, env, k);
        }
        // Regular list: expand each element, handling unquote-splicing
        return expandQQList(list, 0, env, k);
    }

    private Bounce expandQQList(List<?> list, int idx, Env env, Cont k) throws EvalError {
        if (idx >= list.size()) return k.apply(Empty.NIL);
        Object elem = list.get(idx);
        if (elem instanceof List<?> sub && sub.size() == 2 && "unquote-splicing".equals(symName(sub.get(0)))) {
            return bounce(() -> evalK(sub.get(1), env, spliced -> {
                return expandQQList(list, idx + 1, env, rest -> {
                    // Append spliced to rest
                    return k.apply(schemeAppend(spliced, rest));
                });
            }));
        }
        return bounce(() -> expandQuasiquote(elem, env, val -> {
            return expandQQList(list, idx + 1, env, rest -> {
                return k.apply(new Pair(val, rest));
            });
        }));
    }

    private Bounce expandQQDotted(List<?> list, int idx, int dotIdx, Env env, Cont k) throws EvalError {
        if (idx >= dotIdx) {
            // Expand the tail (element after the dot)
            Object tail = list.get(dotIdx + 1);
            if (tail instanceof List<?> sub && sub.size() == 2 && "unquote".equals(symName(sub.get(0)))) {
                return bounce(() -> evalK(sub.get(1), env, k));
            }
            return expandQuasiquote(tail, env, k);
        }
        Object elem = list.get(idx);
        if (elem instanceof List<?> sub && sub.size() == 2 && "unquote-splicing".equals(symName(sub.get(0)))) {
            return bounce(() -> evalK(sub.get(1), env, spliced -> {
                return expandQQDotted(list, idx + 1, dotIdx, env, rest -> {
                    return k.apply(schemeAppend(spliced, rest));
                });
            }));
        }
        return bounce(() -> expandQuasiquote(elem, env, val -> {
            return expandQQDotted(list, idx + 1, dotIdx, env, rest -> {
                return k.apply(new Pair(val, rest));
            });
        }));
    }

    private Object schemeAppend(Object a, Object b) {
        if (a instanceof Empty) return b;
        if (a instanceof Pair p) {
            return new Pair(p.car(), schemeAppend(p.cdr(), b));
        }
        return b; // shouldn't happen for well-formed lists
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
                if (clause.size() == 3 && "=>".equals(symName(clause.get(1)))) {
                    return bounce(() -> evalK(clause.get(2), env, proc ->
                        applyK(proc, List.of(val), el, ec, k)));
                }
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
            SchemeContinuation cont = new SchemeContinuation(k, new ArrayList<>(windingStack));
            return applyK(fn, List.of(cont), eline, ecol, k);
        }
        // Continuation invocation — with dynamic-wind unwind/rewind
        if (proc instanceof SchemeContinuation sc) {
            if (args.isEmpty()) throw new EvalError("continuation: expected 1 argument");
            Object val = args.size() == 1 ? args.get(0) : new SchemeValues(args);
            return doWindTransition(windingStack, sc.savedWinding, val, sc.k);
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
            if ("raise".equals(name)) {
                if (args.size() != 1) throw new EvalError("raise: expected 1 argument");
                throw new SchemeRaisedException(args.get(0));
            }
            if ("error".equals(name)) {
                if (args.isEmpty()) throw new EvalError("error: expected at least 1 argument");
                StringBuilder sb = new StringBuilder();
                sb.append(schemeToString(args.get(0)));
                for (int i = 1; i < args.size(); i++) {
                    sb.append(" ").append(schemeToString(args.get(i)));
                }
                throw new SchemeRaisedException(sb.toString());
            }
            if ("values".equals(name)) {
                if (args.size() == 1) return k.apply(args.get(0));
                return k.apply(new SchemeValues(args));
            }
            if ("call-with-values".equals(name)) {
                if (args.size() != 2) throw new EvalError("call-with-values: expected 2 arguments");
                Object producer = args.get(0);
                Object consumer = args.get(1);
                return applyK(producer, List.of(), eline, ecol, produced -> {
                    List<Object> consumerArgs;
                    if (produced instanceof SchemeValues sv) {
                        consumerArgs = sv.values();
                    } else {
                        consumerArgs = List.of(produced);
                    }
                    return applyK(consumer, consumerArgs, eline, ecol, k);
                });
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
            } catch (SchemeRaisedException sre) {
                throw sre;
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
                            // Transparent overlay: lookups see overlay bindings, but
                            // define goes to the use-site env so macros that expand
                            // to (define ...) define in the correct scope.
                            evalEnv = new Env(useEnv) {
                                @Override
                                void define(String name, Object value) {
                                    parent.define(name, value);
                                }
                            };
                            overlayCreated = true;
                        }
                        evalEnv.bindings.put(fv, val);
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
            // Handle dotted pair notation: (a b . c) -> improper list
            int dotIdx = -1;
            for (int i = 0; i < list.size(); i++) {
                if (".".equals(symName(list.get(i)))) { dotIdx = i; break; }
            }
            if (dotIdx >= 0 && dotIdx == list.size() - 2) {
                Object result = toSchemeValue(list.get(list.size() - 1));
                for (int i = dotIdx - 1; i >= 0; i--) {
                    result = new Pair(toSchemeValue(list.get(i)), result);
                }
                return result;
            }
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

    // --- syntax-case helpers ---

    // Convert a Scheme value (proper list) back to a parsed form suitable for evalK
    @SuppressWarnings("unchecked")
    private Object schemeValueToEvalForm(Object val) {
        if (val instanceof Pair) {
            List<Object> result = new ArrayList<>();
            Object cur = val;
            while (cur instanceof Pair p) {
                result.add(schemeValueToEvalForm(p.car()));
                cur = p.cdr();
            }
            return result;
        }
        if (val == Empty.NIL) {
            return new ArrayList<>();
        }
        // Symbols, numbers, booleans, strings etc. pass through
        return val;
    }

    // Convert a Scheme value (proper list) to a list form for pattern matching
    @SuppressWarnings("unchecked")
    private List<Object> schemeValueToParseForm(Object val) {
        if (val instanceof Pair) {
            List<Object> result = new ArrayList<>();
            Object cur = val;
            while (cur instanceof Pair p) {
                result.add(p.car());
                cur = p.cdr();
            }
            return result;
        }
        if (val == Empty.NIL) {
            return new ArrayList<>();
        }
        // Wrap non-list in a single-element list
        List<Object> result = new ArrayList<>();
        result.add(val);
        return result;
    }

    // Evaluate syntax-case clauses one by one
    @SuppressWarnings("unchecked")
    private Bounce evalSyntaxCaseClausesK(List<Object> inputForm, Object stxVal, List<String> lits,
                                           List<?> list, int clauseIdx, Env env, int el, int ec, Cont k) throws EvalError {
        if (clauseIdx >= list.size()) throw errAt(el, ec, "syntax-case: no matching pattern");
        if (!(list.get(clauseIdx) instanceof List<?> clause) || clause.size() < 2)
            throw errAt(el, ec, "syntax-case: bad clause");

        List<Object> pattern;
        if (clause.get(0) instanceof List<?> patList) {
            pattern = (List<Object>) patList;
        } else {
            // Single symbol pattern (matches anything)
            String patSym = symName(clause.get(0));
            if (patSym != null) {
                // Bind the whole form to this variable
                Set<String> patVarNames = new HashSet<>();
                patVarNames.add(patSym);
                Env clauseEnv = new Env(env);
                clauseEnv.define(patSym, stxVal);
                syntaxCasePatVarStack.add(patVarNames);
                Object body = clause.size() == 2 ? clause.get(1) : clause.get(2);
                try {
                    return bounce(() -> evalK(body, clauseEnv, val -> {
                        syntaxCasePatVarStack.remove(syntaxCasePatVarStack.size() - 1);
                        return k.apply(val);
                    }));
                } catch (Exception e) {
                    syntaxCasePatVarStack.remove(syntaxCasePatVarStack.size() - 1);
                    throw e;
                }
            }
            return evalSyntaxCaseClausesK(inputForm, stxVal, lits, list, clauseIdx + 1, env, el, ec, k);
        }

        // Try to match the pattern against the input form
        Map<String, Object> bindings = matchSyntaxCasePattern(pattern, inputForm, lits);
        if (bindings == null) {
            return evalSyntaxCaseClausesK(inputForm, stxVal, lits, list, clauseIdx + 1, env, el, ec, k);
        }

        // Create environment with pattern variable bindings
        Set<String> patVarNames = new HashSet<>(bindings.keySet());
        Env clauseEnv = new Env(env);
        for (Map.Entry<String, Object> e : bindings.entrySet()) {
            clauseEnv.define(e.getKey(), e.getValue());
        }

        // Push pattern variables for syntax template expansion
        syntaxCasePatVarStack.add(patVarNames);

        // Handle fender (guard) if present: (pattern fender body) vs (pattern body)
        if (clause.size() == 3) {
            Object fender = clause.get(1);
            Object body = clause.get(2);
            return bounce(() -> evalK(fender, clauseEnv, fenderVal -> {
                if (!isFalse(fenderVal)) {
                    return bounce(() -> evalK(body, clauseEnv, val -> {
                        syntaxCasePatVarStack.remove(syntaxCasePatVarStack.size() - 1);
                        return k.apply(val);
                    }));
                }
                // Fender failed, try next clause
                syntaxCasePatVarStack.remove(syntaxCasePatVarStack.size() - 1);
                return evalSyntaxCaseClausesK(inputForm, stxVal, lits, list, clauseIdx + 1, env, el, ec, k);
            }));
        } else {
            // No fender
            Object body = clause.get(1);
            return bounce(() -> evalK(body, clauseEnv, val -> {
                syntaxCasePatVarStack.remove(syntaxCasePatVarStack.size() - 1);
                return k.apply(val);
            }));
        }
    }

    // Match a syntax-case pattern against input, returning bindings or null
    private Map<String, Object> matchSyntaxCasePattern(List<?> pattern, List<Object> input, List<String> lits) {
        Map<String, Object> bindings = new HashMap<>();
        int pi = 0, ii = 0;
        while (pi < pattern.size()) {
            String ps = symName(pattern.get(pi));
            // Check for ellipsis following this element
            if (pi + 1 < pattern.size() && "...".equals(symName(pattern.get(pi + 1)))) {
                if (ps == null) return null;
                // Collect remaining input into a Scheme list
                Object rest = Empty.NIL;
                List<Object> collected = new ArrayList<>();
                while (ii < input.size()) {
                    collected.add(input.get(ii));
                    ii++;
                }
                // Store as Scheme list for syntax template expansion
                for (int i = collected.size() - 1; i >= 0; i--) {
                    rest = new Pair(collected.get(i), rest);
                }
                bindings.put(ps, rest);
                pi += 2;
            } else {
                if (ii >= input.size()) return null;
                if ("_".equals(ps)) {
                    // Wildcard - matches anything, no binding
                    pi++; ii++;
                } else if (ps != null && lits.contains(ps)) {
                    // Literal - must match exactly
                    String is = null;
                    Object inputEl = input.get(ii);
                    if (inputEl instanceof String s) is = s;
                    else if (inputEl instanceof LocatedSymbol ls) is = ls.name();
                    if (!ps.equals(is)) return null;
                    pi++; ii++;
                } else if (ps != null) {
                    // Pattern variable - bind to the input element
                    bindings.put(ps, input.get(ii));
                    pi++; ii++;
                } else {
                    // Sub-pattern (nested list)
                    if (pattern.get(pi) instanceof List<?> subPat) {
                        List<Object> subInput = schemeValueToParseForm(input.get(ii));
                        Map<String, Object> subBindings = matchSyntaxCasePattern((List<Object>) subPat, subInput, lits);
                        if (subBindings == null) return null;
                        bindings.putAll(subBindings);
                    } else {
                        return null;
                    }
                    pi++; ii++;
                }
            }
        }
        return ii == input.size() ? bindings : null;
    }

    // Expand a syntax template (#'...) using pattern variables from the environment
    @SuppressWarnings("unchecked")
    private Object expandSyntaxTemplate(Object template, Env env, Set<String> patVars) {
        String s = symName(template);
        if (s != null) {
            if (patVars.contains(s)) {
                try {
                    return env.lookup(s);
                } catch (EvalError e) {
                    return s;
                }
            }
            return s;
        }
        if (template instanceof List<?> tmplList) {
            List<Object> result = new ArrayList<>();
            for (int i = 0; i < tmplList.size(); i++) {
                if (i + 1 < tmplList.size() && "...".equals(symName(tmplList.get(i + 1)))) {
                    // Ellipsis expansion
                    Object subTemplate = tmplList.get(i);
                    Set<String> usedEV = new HashSet<>();
                    findPatVarsInTemplate(subTemplate, patVars, usedEV);
                    if (!usedEV.isEmpty()) {
                        // Get the list-valued pattern variable
                        String mainVar = usedEV.iterator().next();
                        Object varVal;
                        try { varVal = env.lookup(mainVar); } catch (EvalError e) { varVal = Empty.NIL; }
                        // Iterate over the list
                        List<Object> elements = new ArrayList<>();
                        Object cur = varVal;
                        while (cur instanceof Pair p) {
                            elements.add(p.car());
                            cur = p.cdr();
                        }
                        for (int j = 0; j < elements.size(); j++) {
                            Env iterEnv = new Env(env);
                            for (String ev : usedEV) {
                                Object evVal;
                                try { evVal = env.lookup(ev); } catch (EvalError e) { continue; }
                                List<Object> evElements = new ArrayList<>();
                                Object evCur = evVal;
                                while (evCur instanceof Pair p) {
                                    evElements.add(p.car());
                                    evCur = p.cdr();
                                }
                                if (j < evElements.size()) {
                                    iterEnv.define(ev, evElements.get(j));
                                }
                            }
                            Set<String> remainingPV = new HashSet<>(patVars);
                            // For iteration, the ellipsis vars are now single values
                            result.add(expandSyntaxTemplate(subTemplate, iterEnv, remainingPV));
                        }
                    }
                    i++; // skip "..."
                } else {
                    result.add(expandSyntaxTemplate(tmplList.get(i), env, patVars));
                }
            }
            // Convert to Scheme list (proper list)
            Object schemeList = Empty.NIL;
            for (int i = result.size() - 1; i >= 0; i--) {
                schemeList = new Pair(result.get(i), schemeList);
            }
            return schemeList;
        }
        return template;
    }

    private void findPatVarsInTemplate(Object template, Set<String> patVars, Set<String> found) {
        String s = symName(template);
        if (s != null && patVars.contains(s)) {
            found.add(s);
            return;
        }
        if (template instanceof List<?> list) {
            for (Object elem : list) {
                findPatVarsInTemplate(elem, patVars, found);
            }
        }
    }

    // Evaluate with-syntax bindings sequentially
    @SuppressWarnings("unchecked")
    private Bounce evalWithSyntaxBindingsK(List<?> bindings, int idx, Env env, Set<String> accPatVars,
                                            int el, int ec, List<Object> body, Cont k) throws EvalError {
        if (idx >= bindings.size()) {
            // All bindings done, push pat vars and eval body
            syntaxCasePatVarStack.add(accPatVars);
            return evalSeqK(body, 0, env, val -> {
                syntaxCasePatVarStack.remove(syntaxCasePatVarStack.size() - 1);
                return k.apply(val);
            });
        }
        if (!(bindings.get(idx) instanceof List<?> binding) || binding.size() != 2)
            throw errAt(el, ec, "with-syntax: bad binding");
        String patName = symName(binding.get(0));
        if (patName == null) throw errAt(el, ec, "with-syntax: expected pattern variable name");
        return bounce(() -> evalK(binding.get(1), env, val -> {
            env.define(patName, val);
            accPatVars.add(patName);
            return evalWithSyntaxBindingsK(bindings, idx + 1, env, accPatVars, el, ec, body, k);
        }));
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
            case "syntax->datum" -> {
                if (args.size() != 1) throw new EvalError("syntax->datum: expected 1 argument");
                // In our representation, syntax objects are just values - identity operation
                yield args.get(0);
            }
            case "datum->syntax" -> {
                if (args.size() != 2) throw new EvalError("datum->syntax: expected 2 arguments");
                // First arg is template-id (context), second is datum
                // In our simple representation, just return the datum
                yield args.get(1);
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
