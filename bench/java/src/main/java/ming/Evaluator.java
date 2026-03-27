package ming;

import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;
import java.util.IdentityHashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;

public class Evaluator {

    static class SchemeString {
        private char[] chars;
        private boolean immutable;
        SchemeString(String value) { this.chars = value.toCharArray(); this.immutable = true; }
        SchemeString(char[] chars) { this.chars = chars.clone(); this.immutable = false; }
        String value() { return new String(chars); }
        int length() { return chars.length; }
        char charAt(int i) { return chars[i]; }
        boolean isImmutable() { return immutable; }
        void setChar(int i, char c) { chars[i] = c; }
    }
    static class Pair {
        Object car;
        Object cdr;
        Pair(Object car, Object cdr) { this.car = car; this.cdr = cdr; }
        Object car() { return car; }
        Object cdr() { return cdr; }
    }
    static final Object NIL = new Object() {
        @Override public String toString() { return "()"; }
    };
    record Lambda(List<String> params, String restParam, List<Object> body, Env closure) {}
    record CaseLambda(List<Lambda> clauses) {}
    record SchemeChar(char value) {}

    // Exact rational number: always in reduced form, denominator > 0
    record Rational(long num, long den) {
        Rational {
            if (den == 0) throw new ArithmeticException("division by zero");
            if (den < 0) { num = -num; den = -den; }
            long g = gcd(Math.abs(num), den);
            num /= g;
            den /= g;
        }
        boolean isInteger() { return den == 1; }
        Object simplify() { return den == 1 ? (Object) num : this; }
        double toDouble() { return (double) num / den; }
        private static long gcd(long a, long b) {
            while (b != 0) { long t = b; b = a % b; a = t; }
            return a;
        }
        static Object add(Object a, Object b) {
            long an = numerOf(a), ad = denomOf(a), bn = numerOf(b), bd = denomOf(b);
            return new Rational(an * bd + bn * ad, ad * bd).simplify();
        }
        static Object sub(Object a, Object b) {
            long an = numerOf(a), ad = denomOf(a), bn = numerOf(b), bd = denomOf(b);
            return new Rational(an * bd - bn * ad, ad * bd).simplify();
        }
        static Object mul(Object a, Object b) {
            long an = numerOf(a), ad = denomOf(a), bn = numerOf(b), bd = denomOf(b);
            return new Rational(an * bn, ad * bd).simplify();
        }
        static Object div(Object a, Object b) {
            long an = numerOf(a), ad = denomOf(a), bn = numerOf(b), bd = denomOf(b);
            if (bn == 0) throw new ArithmeticException("division by zero");
            return new Rational(an * bd, ad * bn).simplify();
        }
        static long numerOf(Object o) { return o instanceof Rational r ? r.num : (Long) o; }
        static long denomOf(Object o) { return o instanceof Rational r ? r.den : 1L; }
    }
    static class SchemeRecord {
        final String typeName;
        final Map<String, Object> fields;
        SchemeRecord(String typeName, Map<String, Object> fields) {
            this.typeName = typeName;
            this.fields = fields;
        }
    }
    static class SchemeVector {
        final Object[] data;
        SchemeVector(int size, Object fill) { data = new Object[size]; java.util.Arrays.fill(data, fill); }
        SchemeVector(Object[] data) { this.data = data; }
        int length() { return data.length; }
        Object ref(int i) { return data[i]; }
        void set(int i, Object v) { data[i] = v; }
    }
    record Builtin(String name) {}
    record SyntaxRules(List<String> literals, List<Object> patterns, List<Object> templates, Env defEnv) {}
    // Multiple return values from (values ...)
    record MultipleValues(List<Object> vals) {}
    // Syntax objects for syntax-case macros
    record SyntaxObject(Object datum, Env context) {}
    record SyntaxCaseTransformer(Object proc, Env defEnv) {}
    // Trampoline sentinel for tail call optimization
    record TailCall(Object expr, Env env) {}

    // First-class continuation captured by call/cc
    static class Continuation {
        boolean inExtent = true;
        // For body-level replay:
        List<?> capturedBody;   // the body list containing call/cc
        int capturedBodyIndex;  // index in the body where call/cc was
        int topLevelExprIndex;  // which top-level expression
    }

    // Thrown when a continuation is invoked within its dynamic extent (escape)
    static class ContinuationEscape extends RuntimeException {
        final Continuation cont;
        final Object value;
        ContinuationEscape(Continuation cont, Object value) {
            super(null, null, true, false);
            this.cont = cont;
            this.value = value;
        }
    }

    // Thrown when a continuation is invoked outside its dynamic extent (reentrant)
    static class ContinuationResume extends RuntimeException {
        final Continuation cont;
        final Object value;
        ContinuationResume(Continuation cont, Object value) {
            super(null, null, true, false);
            this.cont = cont;
            this.value = value;
        }
    }

    // Thrown by (raise value)
    static class SchemeRaise extends RuntimeException {
        final Object value;
        SchemeRaise(Object value) {
            super(null, null, true, false);
            this.value = value;
        }
    }

    // Resolve a TailCall chain to a final value
    private Object resolve(Object result) throws EvalError {
        while (result instanceof TailCall tc) {
            result = eval(tc.expr(), tc.env());
        }
        return result;
    }
    record Token(Object value, int line, int col) {}
    record Located(Object expr, int line, int col) {}

    static class Env {
        final Map<String, Object> bindings = new HashMap<>();
        final Env parent;
        Env(Env parent) { this.parent = parent; }
        Object lookup(String name) throws EvalError {
            if (bindings.containsKey(name)) return bindings.get(name);
            if (parent != null) return parent.lookup(name);
            throw new EvalError("unbound variable: " + name);
        }
        void define(String name, Object val) { bindings.put(name, val); }
        void set(String name, Object val) throws EvalError {
            if (bindings.containsKey(name)) { bindings.put(name, val); return; }
            if (parent != null) { parent.set(name, val); return; }
            throw new EvalError("set!: unbound variable: " + name);
        }
    }

    private static final String[] BUILTIN_NAMES = {
        "+", "-", "*", "/", "<", ">", "=", "<=", ">=",
        "cons", "car", "cdr", "null?", "list", "length", "append",
        "not",
        "string?", "number?", "boolean?", "pair?", "symbol?", "char?",
        "display", "write", "newline",
        "string-append", "string-length", "substring",
        "string->number", "number->string",
        "symbol->string", "string->symbol",
        "string-ref", "string-set!", "string-copy",
        "string->list", "list->string", "char->integer", "integer->char",
        "apply",
        "eq?", "equal?", "map",
        "abs", "modulo", "remainder", "quotient", "min", "max", "expt",
        "zero?", "positive?", "negative?", "odd?", "even?",
        "list-ref", "list-tail", "list?", "assoc",
        "char-alphabetic?", "char-numeric?", "char-upcase", "char-downcase",
        "char=?", "char<?",
        "string=?", "string<?", "string-ci=?", "string-upcase", "string-downcase",
        "exact?", "inexact?", "exact->inexact", "inexact->exact",
        "numerator", "denominator", "integer?", "rational?",
        "procedure?",
        "eqv?",
        "vector", "make-vector", "vector-ref", "vector-set!", "vector-length", "vector?",
        "vector->list", "list->vector",
        "for-each",
        "set-car!", "set-cdr!",
        "reverse", "member", "memq", "memv", "assq", "assv",
        "cddr", "cadr", "caar", "cdar", "caddr", "cdddr", "cadadr",
        "gcd", "lcm", "truncate", "round",
        "make-string", "string",
        "string>?", "string<=?", "string>=?",
        "call/cc", "call-with-current-continuation",
        "dynamic-wind",
        "raise", "with-exception-handler",
        "values", "call-with-values",
        "syntax->datum", "datum->syntax"
    };

    private static final Set<String> SPECIAL_FORMS = Set.of(
        "define", "set!", "if", "quote", "lambda", "case-lambda", "and", "or", "begin", "let", "cond", "define-syntax", "define-record-type",
        "letrec", "letrec*", "case", "do", "let*", "when", "unless",
        "call/cc", "call-with-current-continuation",
        "guard", "syntax-case", "syntax", "with-syntax"
    );

    record RecordType(String typeName, List<String> fields) {}
    private final Map<String, RecordType> recordTypes = new HashMap<>();
    private final Map<String, String> recordPredicates = new HashMap<>();
    private final Map<String, String> recordAccessors = new HashMap<>();

    private final Env globalEnv;
    private StringBuilder outputBuffer;
    private int gensymCounter = 0;
    private Continuation lastCapturedCont = null;
    private boolean hasPendingCallccValue = false;
    private Object pendingCallccValue = null;
    // syntax-case context: set during transformer invocation
    private Env currentMacroDefEnv = null;
    private Map<String, Object> syntaxPatternBindings = null;
    private Set<String> syntaxPatternEllipsis = null;
    private Map<String, String> lastSyntaxRenameMap = null;

    public Evaluator() {
        globalEnv = new Env(null);
        for (String name : BUILTIN_NAMES) {
            globalEnv.define(name, new Builtin(name));
        }
    }

    public String evalStr(String input) throws EvalError {
        List<Token> tokens = tokenize(input);
        int[] pos = {0};
        List<Object> exprs = new ArrayList<>();
        while (pos[0] < tokens.size()) {
            exprs.add(parse(tokens, pos));
        }
        Object result = null;
        int startIdx = 0;
        outer:
        while (true) {
            try {
                for (int i = startIdx; i < exprs.size(); i++) {
                    result = eval(exprs.get(i), globalEnv);
                    if (lastCapturedCont != null) {
                        lastCapturedCont.topLevelExprIndex = i;
                        if (lastCapturedCont.capturedBody == null) {
                            lastCapturedCont.capturedBody = exprs;
                            lastCapturedCont.capturedBodyIndex = i;
                        }
                        lastCapturedCont = null;
                    }
                }
                break;
            } catch (ContinuationResume cr) {
                hasPendingCallccValue = true;
                pendingCallccValue = cr.value;
                startIdx = cr.cont.topLevelExprIndex;
                continue outer;
            }
        }
        if (result == null) {
            throw new EvalError("no expression");
        }
        return schemeToString(result);
    }

    public EvalResult evalStrWithOutput(String input) throws EvalError {
        outputBuffer = new StringBuilder();
        List<Token> tokens = tokenize(input);
        int[] pos = {0};
        List<Object> exprs = new ArrayList<>();
        while (pos[0] < tokens.size()) {
            exprs.add(parse(tokens, pos));
        }
        Object result = null;
        int startIdx = 0;
        outer:
        while (true) {
            try {
                for (int i = startIdx; i < exprs.size(); i++) {
                    result = eval(exprs.get(i), globalEnv);
                    if (lastCapturedCont != null) {
                        lastCapturedCont.topLevelExprIndex = i;
                        if (lastCapturedCont.capturedBody == null) {
                            lastCapturedCont.capturedBody = exprs;
                            lastCapturedCont.capturedBodyIndex = i;
                        }
                        lastCapturedCont = null;
                    }
                }
                break;
            } catch (ContinuationResume cr) {
                hasPendingCallccValue = true;
                pendingCallccValue = cr.value;
                startIdx = cr.cont.topLevelExprIndex;
                continue outer;
            }
        }
        String output = outputBuffer.toString();
        outputBuffer = null;
        if (result == null) {
            return new EvalResult("", output);
        }
        return new EvalResult(schemeToString(result), output);
    }

    private List<Token> tokenize(String input) throws EvalError {
        List<Token> tokens = new ArrayList<>();
        int i = 0;
        int len = input.length();
        int line = 1, col = 1;
        while (i < len) {
            char c = input.charAt(i);
            if (c == '\n') {
                i++; line++; col = 1;
            } else if (Character.isWhitespace(c)) {
                i++; col++;
            } else if (c == ';') {
                while (i < len && input.charAt(i) != '\n') { i++; col++; }
            } else if (c == '(') {
                tokens.add(new Token("(", line, col));
                i++; col++;
            } else if (c == ')') {
                tokens.add(new Token(")", line, col));
                i++; col++;
            } else if (c == '\'') {
                tokens.add(new Token("'", line, col));
                i++; col++;
            } else if (c == '#') {
                int startCol = col;
                if (i + 1 < len && input.charAt(i + 1) == '\\') {
                    // Character literal: #\x, #\space, #\newline, #\tab
                    i += 2; col += 2;
                    if (i >= len) throw new EvalError(line + ":" + startCol + ": incomplete character literal");
                    // Check for named characters
                    int nameStart = i;
                    while (i < len && !Character.isWhitespace(input.charAt(i)) && input.charAt(i) != ')' && input.charAt(i) != '(') {
                        i++; col++;
                    }
                    String name = input.substring(nameStart, i);
                    SchemeChar sc = switch (name) {
                        case "space" -> new SchemeChar(' ');
                        case "newline" -> new SchemeChar('\n');
                        case "tab" -> new SchemeChar('\t');
                        default -> {
                            if (name.length() == 1) yield new SchemeChar(name.charAt(0));
                            throw new EvalError(line + ":" + startCol + ": unknown character name: " + name);
                        }
                    };
                    tokens.add(new Token(sc, line, startCol));
                } else if (i + 1 < len && input.charAt(i + 1) == '\'') {
                    // Syntax quote: #'
                    tokens.add(new Token("#'", line, startCol));
                    i += 2; col += 2;
                } else if (i + 1 < len && (input.charAt(i + 1) == 't' || input.charAt(i + 1) == 'f')) {
                    tokens.add(new Token(input.charAt(i + 1) == 't' ? Boolean.TRUE : Boolean.FALSE, line, startCol));
                    i += 2; col += 2;
                } else {
                    throw new EvalError(line + ":" + startCol + ": unexpected character after #");
                }
            } else if (c == '"') {
                int startCol = col;
                StringBuilder sb = new StringBuilder();
                i++; col++;
                while (i < len && input.charAt(i) != '"') {
                    if (input.charAt(i) == '\\' && i + 1 < len) {
                        i++; col++;
                        char esc = input.charAt(i);
                        switch (esc) {
                            case 'n' -> sb.append('\n');
                            case 't' -> sb.append('\t');
                            case '"' -> sb.append('"');
                            case '\\' -> sb.append('\\');
                            default -> sb.append(esc);
                        }
                    } else {
                        sb.append(input.charAt(i));
                    }
                    if (input.charAt(i) == '\n') { line++; col = 1; } else { col++; }
                    i++;
                }
                if (i >= len) throw new EvalError(line + ":" + startCol + ": unterminated string");
                i++; col++;
                tokens.add(new Token(new SchemeString(sb.toString()), line, startCol));
            } else if (c == '-' && i + 1 < len && Character.isDigit(input.charAt(i + 1))
                    && (tokens.isEmpty() || tokens.getLast().value().equals("("))) {
                int startCol = col;
                StringBuilder sb = new StringBuilder();
                sb.append('-');
                i++; col++;
                while (i < len && (Character.isDigit(input.charAt(i)) || input.charAt(i) == '.' || input.charAt(i) == '/')) {
                    sb.append(input.charAt(i));
                    i++; col++;
                }
                tokens.add(new Token(parseNumber(sb.toString()), line, startCol));
            } else {
                int startCol = col;
                StringBuilder sb = new StringBuilder();
                while (i < len && !Character.isWhitespace(input.charAt(i))
                        && input.charAt(i) != '(' && input.charAt(i) != ')'
                        && input.charAt(i) != '"' && input.charAt(i) != ';') {
                    sb.append(input.charAt(i));
                    i++; col++;
                }
                String tok = sb.toString();
                tokens.add(new Token(parseNumberOrSymbol(tok), line, startCol));
            }
        }
        return tokens;
    }

    private static Object parseNumber(String tok) {
        if (tok.contains("/")) {
            int slash = tok.indexOf('/');
            long num = Long.parseLong(tok.substring(0, slash));
            long den = Long.parseLong(tok.substring(slash + 1));
            return new Rational(num, den).simplify();
        }
        if (tok.contains(".")) {
            return Double.parseDouble(tok);
        }
        return Long.parseLong(tok);
    }

    private static Object parseNumberOrSymbol(String tok) {
        // Try rational: digits/digits
        if (tok.matches("-?\\d+/\\d+")) {
            return parseNumber(tok);
        }
        try { return Long.parseLong(tok); }
        catch (NumberFormatException e) {}
        // Try floating point
        if (tok.matches("-?\\d+\\.\\d*") || tok.matches("-?\\.\\d+")) {
            try { return Double.parseDouble(tok); }
            catch (NumberFormatException e) {}
        }
        return tok;
    }

    private Object parse(List<Token> tokens, int[] pos) throws EvalError {
        if (pos[0] >= tokens.size()) {
            throw new EvalError("unexpected end of input");
        }
        Token token = tokens.get(pos[0]);
        int tLine = token.line(), tCol = token.col();
        if (token.value().equals("'")) {
            pos[0]++;
            Object datum = parse(tokens, pos);
            List<Object> quoted = new ArrayList<>();
            quoted.add("quote");
            quoted.add(datum);
            return new Located(quoted, tLine, tCol);
        }
        if (token.value().equals("#'")) {
            pos[0]++;
            Object datum = parse(tokens, pos);
            List<Object> syntaxExpr = new ArrayList<>();
            syntaxExpr.add("syntax");
            syntaxExpr.add(datum);
            return new Located(syntaxExpr, tLine, tCol);
        }
        if (token.value().equals("(")) {
            pos[0]++;
            List<Object> list = new ArrayList<>();
            while (pos[0] < tokens.size() && !tokens.get(pos[0]).value().equals(")")) {
                list.add(parse(tokens, pos));
            }
            if (pos[0] >= tokens.size()) {
                throw new EvalError(tLine + ":" + tCol + ": missing closing parenthesis");
            }
            pos[0]++;
            return new Located(list, tLine, tCol);
        } else if (token.value().equals(")")) {
            throw new EvalError(tLine + ":" + tCol + ": unexpected )");
        } else {
            pos[0]++;
            return new Located(token.value(), tLine, tCol);
        }
    }

    private Object eval(Object expr, Env env) throws EvalError {
        while (true) {
            // Unwrap Located to get position info
            int eLine = 0, eCol = 0;
            if (expr instanceof Located loc) {
                eLine = loc.line();
                eCol = loc.col();
                expr = loc.expr();
            }

            try {
                Object result = evalInner(expr, env, eLine, eCol);
                if (result instanceof TailCall tc) {
                    expr = tc.expr();
                    env = tc.env();
                    continue;
                }
                return result;
            } catch (EvalError e) {
                String msg = e.getMessage();
                if (eLine > 0 && !msg.matches(".*\\d+:\\d+.*")) {
                    throw new EvalError(eLine + ":" + eCol + ": " + msg);
                }
                throw e;
            }
        }
    }

    private Object evalInner(Object expr, Env env, int posLine, int posCol) throws EvalError {
        if (expr instanceof Long || expr instanceof Double || expr instanceof Rational || expr instanceof Boolean || expr instanceof SchemeString || expr instanceof SchemeChar) {
            return expr;
        }
        if (expr instanceof String symbol) {
            return env.lookup(symbol);
        }
        if (expr instanceof List<?> list) {
            if (list.isEmpty()) {
                throw new EvalError("empty application");
            }
            Object head = list.getFirst();
            // Unwrap Located head to get the raw value for switch matching
            Object rawHead = head;
            if (rawHead instanceof Located lh) rawHead = lh.expr();
            if (rawHead instanceof String s) {
                switch (s) {
                    case "define" -> {
                        if (list.size() < 3) throw new EvalError("define: bad syntax");
                        Object target = list.get(1);
                        if (target instanceof Located lt) target = lt.expr();
                        if (target instanceof String name) {
                            Object val = eval(list.get(2), env);
                            env.define(name, val);
                            return val;
                        } else if (target instanceof List<?> sig) {
                            // (define (f params...) body...) or (define (f params... . rest) body...)
                            Object rawFirst = sig.getFirst();
                            if (rawFirst instanceof Located lf) rawFirst = lf.expr();
                            String name = (String) rawFirst;
                            List<String> params = new ArrayList<>();
                            String restParam = null;
                            for (int i = 1; i < sig.size(); i++) {
                                Object p = sig.get(i);
                                if (p instanceof Located lp) p = lp.expr();
                                if (".".equals(p)) {
                                    Object rp = sig.get(i + 1);
                                    if (rp instanceof Located lrp) rp = lrp.expr();
                                    restParam = (String) rp;
                                    break;
                                }
                                params.add((String) p);
                            }
                            List<Object> body = new ArrayList<>();
                            for (int i = 2; i < list.size(); i++) {
                                body.add(list.get(i));
                            }
                            Lambda lambda = new Lambda(params, restParam, body, env);
                            env.define(name, lambda);
                            return lambda;
                        }
                        throw new EvalError("define: bad syntax");
                    }
                    case "set!" -> {
                        if (list.size() != 3) throw new EvalError("set!: bad syntax");
                        Object target = list.get(1);
                        if (target instanceof Located lt) target = lt.expr();
                        if (!(target instanceof String name)) throw new EvalError("set!: not a variable");
                        Object val = eval(list.get(2), env);
                        env.set(name, val);
                        return null;
                    }
                    case "if" -> {
                        if (list.size() < 3) throw new EvalError("if: bad syntax");
                        Object cond = eval(list.get(1), env);
                        if (!isFalse(cond)) {
                            return new TailCall(list.get(2), env);
                        } else if (list.size() > 3) {
                            return new TailCall(list.get(3), env);
                        }
                        return null; // unspecified
                    }
                    case "quote" -> {
                        if (list.size() != 2) throw new EvalError("quote: bad syntax");
                        return quote(list.get(1));
                    }
                    case "lambda" -> {
                        if (list.size() < 3) throw new EvalError("lambda: bad syntax");
                        Object paramSpec = list.get(1);
                        if (paramSpec instanceof Located lp) paramSpec = lp.expr();
                        List<String> params = new ArrayList<>();
                        String restParam = null;
                        if (paramSpec instanceof List<?> plist) {
                            for (int pi = 0; pi < plist.size(); pi++) {
                                Object p = plist.get(pi);
                                if (p instanceof Located lpp) p = lpp.expr();
                                if (".".equals(p)) {
                                    Object rp = plist.get(pi + 1);
                                    if (rp instanceof Located lrp) rp = lrp.expr();
                                    restParam = (String) rp;
                                    break;
                                }
                                params.add((String) p);
                            }
                        } else if (paramSpec instanceof String singleRest) {
                            // (lambda args body) - single rest param
                            restParam = singleRest;
                        } else {
                            throw new EvalError("lambda: bad parameter list");
                        }
                        List<Object> body = new ArrayList<>();
                        for (int i = 2; i < list.size(); i++) {
                            body.add(list.get(i));
                        }
                        return new Lambda(params, restParam, body, env);
                    }
                    case "case-lambda" -> {
                        List<Lambda> clauses = new ArrayList<>();
                        for (int ci = 1; ci < list.size(); ci++) {
                            Object clauseObj = list.get(ci);
                            if (clauseObj instanceof Located lc) clauseObj = lc.expr();
                            @SuppressWarnings("unchecked")
                            List<Object> clause = (List<Object>) clauseObj;
                            Object cParamSpec = clause.get(0);
                            if (cParamSpec instanceof Located lp) cParamSpec = lp.expr();
                            List<String> cParams = new ArrayList<>();
                            String cRestParam = null;
                            if (cParamSpec instanceof List<?> plist) {
                                for (int pi = 0; pi < plist.size(); pi++) {
                                    Object p = plist.get(pi);
                                    if (p instanceof Located lpp) p = lpp.expr();
                                    if (".".equals(p)) {
                                        Object rp = plist.get(pi + 1);
                                        if (rp instanceof Located lrp) rp = lrp.expr();
                                        cRestParam = (String) rp;
                                        break;
                                    }
                                    cParams.add((String) p);
                                }
                            } else if (cParamSpec instanceof String singleRest) {
                                cRestParam = singleRest;
                            }
                            List<Object> cBody = new ArrayList<>();
                            for (int i = 1; i < clause.size(); i++) {
                                cBody.add(clause.get(i));
                            }
                            clauses.add(new Lambda(cParams, cRestParam, cBody, env));
                        }
                        return new CaseLambda(clauses);
                    }
                    case "and" -> {
                        if (list.size() == 1) return Boolean.TRUE;
                        for (int i = 1; i < list.size() - 1; i++) {
                            Object result = eval(list.get(i), env);
                            if (isFalse(result)) return result;
                        }
                        return new TailCall(list.getLast(), env);
                    }
                    case "or" -> {
                        if (list.size() == 1) return Boolean.FALSE;
                        for (int i = 1; i < list.size() - 1; i++) {
                            Object result = eval(list.get(i), env);
                            if (!isFalse(result)) return result;
                        }
                        return new TailCall(list.getLast(), env);
                    }
                    case "begin" -> {
                        if (list.size() == 1) return null;
                        for (int i = 1; i < list.size() - 1; i++) {
                            eval(list.get(i), env);
                        }
                        return new TailCall(list.getLast(), env);
                    }
                    case "let" -> {
                        if (list.size() < 3) throw new EvalError("let: bad syntax");
                        Object item1 = list.get(1);
                        if (item1 instanceof Located li) item1 = li.expr();
                        // Named let: (let name ((var init) ...) body ...)
                        if (item1 instanceof String loopName) {
                            if (list.size() < 4) throw new EvalError("let: bad syntax");
                            Object bindingsRaw = list.get(2);
                            if (bindingsRaw instanceof Located lb) bindingsRaw = lb.expr();
                            List<?> bindingsList = (List<?>) bindingsRaw;
                            List<String> params = new ArrayList<>();
                            List<Object> inits = new ArrayList<>();
                            for (Object b : bindingsList) {
                                if (b instanceof Located lbb) b = lbb.expr();
                                List<?> binding = (List<?>) b;
                                Object bname = binding.get(0);
                                if (bname instanceof Located lbn) bname = lbn.expr();
                                params.add((String) bname);
                                inits.add(eval(binding.get(1), env));
                            }
                            List<Object> body = new ArrayList<>();
                            for (int i = 3; i < list.size(); i++) body.add(list.get(i));
                            Env letEnv = new Env(env);
                            Lambda loopLambda = new Lambda(params, null, body, letEnv);
                            letEnv.define(loopName, loopLambda);
                            return apply(loopLambda, inits); // TailCall from apply is handled by eval trampoline
                        }
                        // Regular let
                        List<?> bindingsList2 = (List<?>) item1;
                        Env letEnv = new Env(env);
                        for (Object b : bindingsList2) {
                            if (b instanceof Located lbb) b = lbb.expr();
                            List<?> binding = (List<?>) b;
                            Object bname = binding.get(0);
                            if (bname instanceof Located lbn) bname = lbn.expr();
                            String name = (String) bname;
                            Object val = eval(binding.get(1), env);
                            letEnv.define(name, val);
                        }
                        boolean contCapturedInLet = false;
                        for (int i = 2; i < list.size() - 1; i++) {
                            eval(list.get(i), letEnv);
                            if (lastCapturedCont != null && lastCapturedCont.capturedBody == null) {
                                lastCapturedCont.capturedBody = list;
                                lastCapturedCont.capturedBodyIndex = i;
                                contCapturedInLet = true;
                            }
                        }
                        if (contCapturedInLet) {
                            // Stay in scope to catch ContinuationResume from the tail expression
                            while (true) {
                                try {
                                    return resolve(eval(list.getLast(), letEnv));
                                } catch (ContinuationResume cr) {
                                    if (cr.cont.capturedBody == list) {
                                        hasPendingCallccValue = true;
                                        pendingCallccValue = cr.value;
                                        for (int j = cr.cont.capturedBodyIndex; j < list.size() - 1; j++) {
                                            eval(list.get(j), letEnv);
                                        }
                                        continue;
                                    }
                                    throw cr;
                                }
                            }
                        }
                        return new TailCall(list.getLast(), letEnv);
                    }
                    case "cond" -> {
                        for (int i = 1; i < list.size(); i++) {
                            Object clauseRaw = list.get(i);
                            if (clauseRaw instanceof Located lc) clauseRaw = lc.expr();
                            List<?> clause = (List<?>) clauseRaw;
                            Object test = clause.getFirst();
                            Object rawTest = test;
                            if (rawTest instanceof Located lt) rawTest = lt.expr();
                            if (rawTest instanceof String st && st.equals("else")) {
                                for (int j = 1; j < clause.size() - 1; j++) eval(clause.get(j), env);
                                return new TailCall(clause.getLast(), env);
                            }
                            Object testVal = eval(test, env);
                            if (!isFalse(testVal)) {
                                if (clause.size() == 1) return testVal;
                                for (int j = 1; j < clause.size() - 1; j++) eval(clause.get(j), env);
                                return new TailCall(clause.getLast(), env);
                            }
                        }
                        return null;
                    }
                    case "define-syntax" -> {
                        if (list.size() != 3) throw new EvalError("define-syntax: bad syntax");
                        Object nameObj = list.get(1);
                        if (nameObj instanceof Located ln) nameObj = ln.expr();
                        String macroName = (String) nameObj;
                        Object transRaw = list.get(2);
                        if (transRaw instanceof Located lt) transRaw = lt.expr();
                        boolean isSyntaxRules = false;
                        if (transRaw instanceof List<?> trans) {
                            Object transHead = trans.get(0);
                            if (transHead instanceof Located lh) transHead = lh.expr();
                            if ("syntax-rules".equals(transHead)) {
                                isSyntaxRules = true;
                                Object litRaw = trans.get(1);
                                if (litRaw instanceof Located ll) litRaw = ll.expr();
                                List<?> litList = (List<?>) litRaw;
                                List<String> literals = new ArrayList<>();
                                for (Object lit : litList) {
                                    if (lit instanceof Located llit) lit = llit.expr();
                                    literals.add((String) lit);
                                }
                                List<Object> patterns = new ArrayList<>();
                                List<Object> templates = new ArrayList<>();
                                for (int i = 2; i < trans.size(); i++) {
                                    Object clauseRaw = trans.get(i);
                                    if (clauseRaw instanceof Located lc) clauseRaw = lc.expr();
                                    List<?> clause = (List<?>) clauseRaw;
                                    patterns.add(stripLocated(clause.get(0)));
                                    templates.add(stripLocated(clause.get(1)));
                                }
                                env.define(macroName, new SyntaxRules(literals, patterns, templates, env));
                            }
                        }
                        if (!isSyntaxRules) {
                            // General transformer (e.g., lambda)
                            Object transformer = eval(list.get(2), env);
                            env.define(macroName, new SyntaxCaseTransformer(transformer, env));
                        }
                        return null;
                    }
                    case "letrec" -> {
                        if (list.size() < 3) throw new EvalError("letrec: bad syntax");
                        Object bindingsRaw = list.get(1);
                        if (bindingsRaw instanceof Located lb) bindingsRaw = lb.expr();
                        List<?> bindingsList = (List<?>) bindingsRaw;
                        Env letrecEnv = new Env(env);
                        List<String> names = new ArrayList<>();
                        List<Object> initExprs = new ArrayList<>();
                        for (Object b : bindingsList) {
                            if (b instanceof Located lbb) b = lbb.expr();
                            List<?> binding = (List<?>) b;
                            Object bname = binding.get(0);
                            if (bname instanceof Located lbn) bname = lbn.expr();
                            names.add((String) bname);
                            initExprs.add(binding.get(1));
                            letrecEnv.define((String) bname, null);
                        }
                        for (int i = 0; i < names.size(); i++) {
                            letrecEnv.define(names.get(i), eval(initExprs.get(i), letrecEnv));
                        }
                        for (int i = 2; i < list.size() - 1; i++) eval(list.get(i), letrecEnv);
                        return new TailCall(list.getLast(), letrecEnv);
                    }
                    case "letrec*" -> {
                        if (list.size() < 3) throw new EvalError("letrec*: bad syntax");
                        Object bindingsRaw = list.get(1);
                        if (bindingsRaw instanceof Located lb) bindingsRaw = lb.expr();
                        List<?> bindingsList = (List<?>) bindingsRaw;
                        Env letrecEnv = new Env(env);
                        for (Object b : bindingsList) {
                            if (b instanceof Located lbb) b = lbb.expr();
                            List<?> binding = (List<?>) b;
                            Object bname = binding.get(0);
                            if (bname instanceof Located lbn) bname = lbn.expr();
                            letrecEnv.define((String) bname, eval(binding.get(1), letrecEnv));
                        }
                        for (int i = 2; i < list.size() - 1; i++) eval(list.get(i), letrecEnv);
                        return new TailCall(list.getLast(), letrecEnv);
                    }
                    case "case" -> {
                        Object key = eval(list.get(1), env);
                        for (int i = 2; i < list.size(); i++) {
                            Object clauseRaw = list.get(i);
                            if (clauseRaw instanceof Located lc) clauseRaw = lc.expr();
                            List<?> clause = (List<?>) clauseRaw;
                            Object datumsRaw = clause.get(0);
                            if (datumsRaw instanceof Located ld) datumsRaw = ld.expr();
                            if (datumsRaw instanceof String ds && ds.equals("else")) {
                                Object r = null;
                                for (int j = 1; j < clause.size(); j++) r = eval(clause.get(j), env);
                                return r;
                            }
                            List<?> datums = (List<?>) datumsRaw;
                            for (Object d : datums) {
                                Object datum = quote(d);
                                if (schemeEqv(key, datum)) {
                                    Object r = null;
                                    for (int j = 1; j < clause.size(); j++) r = eval(clause.get(j), env);
                                    return r;
                                }
                            }
                        }
                        return null; // no match, unspecified
                    }
                    case "do" -> {
                        // (do ((var init step) ...) (test expr ...) body ...)
                        if (list.size() < 3) throw new EvalError("do: bad syntax");
                        Object varsRaw = list.get(1);
                        if (varsRaw instanceof Located lv) varsRaw = lv.expr();
                        List<?> varSpecs = (List<?>) varsRaw;
                        Object testRaw = list.get(2);
                        if (testRaw instanceof Located lt) testRaw = lt.expr();
                        List<?> testClause = (List<?>) testRaw;

                        // Parse variable specs
                        List<String> varNames = new ArrayList<>();
                        List<Object> stepExprs = new ArrayList<>(); // null if no step
                        Env doEnv = new Env(env);
                        for (Object vs : varSpecs) {
                            if (vs instanceof Located lvs) vs = lvs.expr();
                            List<?> spec = (List<?>) vs;
                            Object vname = spec.get(0);
                            if (vname instanceof Located lvn) vname = lvn.expr();
                            varNames.add((String) vname);
                            doEnv.define((String) vname, eval(spec.get(1), env));
                            stepExprs.add(spec.size() > 2 ? spec.get(2) : null);
                        }

                        // Loop
                        while (true) {
                            // Test
                            Object testVal = eval(testClause.get(0), doEnv);
                            if (!isFalse(testVal)) {
                                // Test passed - evaluate result exprs
                                if (testClause.size() > 1) {
                                    Object r = null;
                                    for (int j = 1; j < testClause.size(); j++) r = eval(testClause.get(j), doEnv);
                                    return r;
                                }
                                return null;
                            }
                            // Evaluate body
                            for (int i = 3; i < list.size(); i++) eval(list.get(i), doEnv);
                            // Parallel step: evaluate all step exprs with current values
                            Object[] newVals = new Object[varNames.size()];
                            for (int i = 0; i < varNames.size(); i++) {
                                if (stepExprs.get(i) != null) {
                                    newVals[i] = eval(stepExprs.get(i), doEnv);
                                } else {
                                    newVals[i] = doEnv.lookup(varNames.get(i));
                                }
                            }
                            for (int i = 0; i < varNames.size(); i++) {
                                doEnv.define(varNames.get(i), newVals[i]);
                            }
                        }
                    }
                    case "let*" -> {
                        if (list.size() < 3) throw new EvalError("let*: bad syntax");
                        Object bindingsRaw = list.get(1);
                        if (bindingsRaw instanceof Located lb) bindingsRaw = lb.expr();
                        List<?> bindingsList = (List<?>) bindingsRaw;
                        Env letEnv = env;
                        for (Object b : bindingsList) {
                            if (b instanceof Located lbb) b = lbb.expr();
                            List<?> binding = (List<?>) b;
                            Object bname = binding.get(0);
                            if (bname instanceof Located lbn) bname = lbn.expr();
                            String name = (String) bname;
                            Object val = eval(binding.get(1), letEnv);
                            Env nextEnv = new Env(letEnv);
                            nextEnv.define(name, val);
                            letEnv = nextEnv;
                        }
                        for (int i = 2; i < list.size() - 1; i++) {
                            eval(list.get(i), letEnv);
                        }
                        return new TailCall(list.getLast(), letEnv);
                    }
                    case "when" -> {
                        if (list.size() < 3) throw new EvalError("when: bad syntax");
                        Object testVal = eval(list.get(1), env);
                        if (!isFalse(testVal)) {
                            for (int i = 2; i < list.size() - 1; i++) eval(list.get(i), env);
                            return new TailCall(list.getLast(), env);
                        }
                        return null;
                    }
                    case "unless" -> {
                        if (list.size() < 3) throw new EvalError("unless: bad syntax");
                        Object testVal = eval(list.get(1), env);
                        if (isFalse(testVal)) {
                            for (int i = 2; i < list.size() - 1; i++) eval(list.get(i), env);
                            return new TailCall(list.getLast(), env);
                        }
                        return null;
                    }
                    case "define-record-type" -> {
                        // (define-record-type <name> (constructor field...) predicate (field accessor)...)
                        Object typeNameRaw = list.get(1);
                        if (typeNameRaw instanceof Located lt) typeNameRaw = lt.expr();
                        String typeName = (String) typeNameRaw;

                        Object ctorRaw = list.get(2);
                        if (ctorRaw instanceof Located lc) ctorRaw = lc.expr();
                        List<?> ctorSpec = (List<?>) ctorRaw;
                        Object ctorNameRaw = ctorSpec.getFirst();
                        if (ctorNameRaw instanceof Located ln) ctorNameRaw = ln.expr();
                        String ctorName = (String) ctorNameRaw;
                        List<String> ctorFields = new ArrayList<>();
                        for (int i = 1; i < ctorSpec.size(); i++) {
                            Object f = ctorSpec.get(i);
                            if (f instanceof Located lf) f = lf.expr();
                            ctorFields.add((String) f);
                        }

                        Object predRaw = list.get(3);
                        if (predRaw instanceof Located lp) predRaw = lp.expr();
                        String predName = (String) predRaw;

                        // Field accessors: (field accessor) from index 4 onward
                        Map<String, String> fieldToAccessor = new HashMap<>();
                        for (int i = 4; i < list.size(); i++) {
                            Object fieldSpecRaw = list.get(i);
                            if (fieldSpecRaw instanceof Located lfs) fieldSpecRaw = lfs.expr();
                            List<?> fieldSpec = (List<?>) fieldSpecRaw;
                            Object fn = fieldSpec.get(0);
                            if (fn instanceof Located lfn) fn = lfn.expr();
                            Object an = fieldSpec.get(1);
                            if (an instanceof Located lan) an = lan.expr();
                            fieldToAccessor.put((String) fn, (String) an);
                        }

                        // Define constructor
                        final List<String> cFields = ctorFields;
                        final String tName = typeName;
                        env.define(ctorName, new Builtin(ctorName));

                        // Define predicate
                        env.define(predName, new Builtin(predName));

                        // Define accessors
                        for (var entry : fieldToAccessor.entrySet()) {
                            env.define(entry.getValue(), new Builtin(entry.getValue()));
                        }

                        // Store record type info for use in apply
                        recordTypes.put(ctorName, new RecordType(tName, cFields));
                        recordPredicates.put(predName, tName);
                        for (var entry : fieldToAccessor.entrySet()) {
                            recordAccessors.put(entry.getValue(), entry.getKey());
                        }
                        return null;
                    }
                    case "guard" -> {
                        // (guard (var clause ...) body ...)
                        Object guardSpecRaw = list.get(1);
                        if (guardSpecRaw instanceof Located lg) guardSpecRaw = lg.expr();
                        @SuppressWarnings("unchecked")
                        List<Object> guardSpec = (List<Object>) guardSpecRaw;
                        Object varRaw = guardSpec.getFirst();
                        if (varRaw instanceof Located lv) varRaw = lv.expr();
                        String var = (String) varRaw;
                        try {
                            Object bodyResult = null;
                            for (int i = 2; i < list.size(); i++) {
                                bodyResult = eval(list.get(i), env);
                            }
                            return bodyResult;
                        } catch (SchemeRaise sr) {
                            Env guardEnv = new Env(env);
                            guardEnv.define(var, sr.value);
                            for (int c = 1; c < guardSpec.size(); c++) {
                                Object clauseRaw = guardSpec.get(c);
                                if (clauseRaw instanceof Located lc) clauseRaw = lc.expr();
                                List<?> clause = (List<?>) clauseRaw;
                                Object test = clause.getFirst();
                                Object rawTest = test;
                                if (rawTest instanceof Located lt) rawTest = lt.expr();
                                if (rawTest instanceof String st && st.equals("else")) {
                                    for (int j = 1; j < clause.size() - 1; j++) eval(clause.get(j), guardEnv);
                                    return new TailCall(clause.getLast(), guardEnv);
                                }
                                Object testVal = eval(test, guardEnv);
                                if (!isFalse(testVal)) {
                                    if (clause.size() == 1) return testVal;
                                    for (int j = 1; j < clause.size() - 1; j++) eval(clause.get(j), guardEnv);
                                    return new TailCall(clause.getLast(), guardEnv);
                                }
                            }
                            throw sr; // no clause matched, re-raise
                        }
                    }
                    case "call/cc", "call-with-current-continuation" -> {
                        if (hasPendingCallccValue) {
                            Object v = pendingCallccValue;
                            hasPendingCallccValue = false;
                            pendingCallccValue = null;
                            return v;
                        }
                        if (list.size() != 2) throw new EvalError("call/cc: expected 1 argument");
                        Object func = eval(list.get(1), env);
                        return doCallCC(func);
                    }
                    case "syntax-case" -> {
                        // (syntax-case expr (literals) clause ...)
                        Object stxObj = eval(list.get(1), env);
                        Object litRaw = list.get(2);
                        if (litRaw instanceof Located ll) litRaw = ll.expr();
                        List<?> litList = (List<?>) litRaw;
                        List<String> literals = new ArrayList<>();
                        for (Object lit : litList) {
                            if (lit instanceof Located llit) lit = llit.expr();
                            literals.add((String) lit);
                        }
                        // Get the datum to match against
                        Object datum = stxObj instanceof SyntaxObject so ? so.datum() : stxObj;
                        @SuppressWarnings("unchecked")
                        List<Object> input = (List<Object>) datum;

                        for (int ci = 3; ci < list.size(); ci++) {
                            Object clauseRaw = list.get(ci);
                            if (clauseRaw instanceof Located lc) clauseRaw = lc.expr();
                            List<?> clause = (List<?>) clauseRaw;
                            Object patternRaw = clause.get(0);
                            if (patternRaw instanceof Located lp) patternRaw = lp.expr();
                            List<?> pattern = (List<?>) patternRaw;
                            Object strippedPattern = stripLocated(patternRaw);
                            @SuppressWarnings("unchecked")
                            List<?> patList = (List<?>) strippedPattern;

                            Map<String, Object> bindings = new HashMap<>();
                            Set<String> ellipsisVars = new HashSet<>();
                            if (matchPattern(patList, input, literals, bindings, ellipsisVars, 0)) {
                                // Check for fender (guard): clause size > 2 means (pattern fender body)
                                Object bodyExpr;
                                if (clause.size() > 2) {
                                    // Save context for fender evaluation
                                    var prevBindings = syntaxPatternBindings;
                                    var prevEllipsis = syntaxPatternEllipsis;
                                    syntaxPatternBindings = new HashMap<>(bindings);
                                    if (prevBindings != null) {
                                        for (var e : prevBindings.entrySet()) {
                                            syntaxPatternBindings.putIfAbsent(e.getKey(), e.getValue());
                                        }
                                    }
                                    syntaxPatternEllipsis = new HashSet<>(ellipsisVars);
                                    if (prevEllipsis != null) syntaxPatternEllipsis.addAll(prevEllipsis);
                                    Object fenderResult;
                                    try {
                                        fenderResult = eval(clause.get(1), env);
                                    } finally {
                                        syntaxPatternBindings = prevBindings;
                                        syntaxPatternEllipsis = prevEllipsis;
                                    }
                                    if (isFalse(fenderResult)) continue;
                                    bodyExpr = clause.get(2);
                                } else {
                                    bodyExpr = clause.get(1);
                                }

                                // Set syntax context and eval body
                                var prevBindings = syntaxPatternBindings;
                                var prevEllipsis = syntaxPatternEllipsis;
                                syntaxPatternBindings = new HashMap<>(bindings);
                                if (prevBindings != null) {
                                    for (var e : prevBindings.entrySet()) {
                                        syntaxPatternBindings.putIfAbsent(e.getKey(), e.getValue());
                                    }
                                }
                                syntaxPatternEllipsis = new HashSet<>(ellipsisVars);
                                if (prevEllipsis != null) syntaxPatternEllipsis.addAll(prevEllipsis);
                                try {
                                    return eval(bodyExpr, env);
                                } finally {
                                    syntaxPatternBindings = prevBindings;
                                    syntaxPatternEllipsis = prevEllipsis;
                                }
                            }
                        }
                        throw new EvalError("syntax-case: no matching pattern");
                    }
                    case "syntax" -> {
                        // (syntax template) — template expansion using current syntax-case bindings
                        Object template = list.get(1);
                        if (template instanceof Located lt) template = lt.expr();
                        if (syntaxPatternBindings != null) {
                            // Simple pattern variable reference: return SyntaxObject
                            if (template instanceof String ts && syntaxPatternBindings.containsKey(ts)
                                    && !syntaxPatternEllipsis.contains(ts)) {
                                Object val = syntaxPatternBindings.get(ts);
                                return new SyntaxObject(val, currentMacroDefEnv);
                            }
                            // Full template expansion
                            Object stripped = stripLocated(template);
                            Set<String> patVars = syntaxPatternBindings.keySet();
                            Map<String, String> renameMap = new HashMap<>();
                            collectFreeSymbols(stripped, patVars, renameMap);
                            Object expanded = expandTemplate(stripped, syntaxPatternBindings,
                                    syntaxPatternEllipsis, renameMap);
                            lastSyntaxRenameMap = renameMap;
                            return new SyntaxObject(expanded, currentMacroDefEnv);
                        }
                        // Outside syntax-case context, just wrap
                        return new SyntaxObject(stripLocated(template), env);
                    }
                    case "with-syntax" -> {
                        // (with-syntax ((name expr) ...) body ...)
                        Object bindingsRaw = list.get(1);
                        if (bindingsRaw instanceof Located lb) bindingsRaw = lb.expr();
                        List<?> bindingsList = (List<?>) bindingsRaw;

                        var prevBindings = syntaxPatternBindings;
                        var prevEllipsis = syntaxPatternEllipsis;
                        Map<String, Object> newBindings = new HashMap<>(
                                prevBindings != null ? prevBindings : Map.of());
                        Set<String> newEllipsis = new HashSet<>(
                                prevEllipsis != null ? prevEllipsis : Set.of());

                        for (Object b : bindingsList) {
                            if (b instanceof Located lbb) b = lbb.expr();
                            List<?> binding = (List<?>) b;
                            Object bname = binding.get(0);
                            if (bname instanceof Located lbn) bname = lbn.expr();
                            String name = (String) bname;
                            Object val = eval(binding.get(1), env);
                            if (val instanceof SyntaxObject so) {
                                newBindings.put(name, so.datum());
                            } else {
                                newBindings.put(name, val);
                            }
                        }

                        syntaxPatternBindings = newBindings;
                        syntaxPatternEllipsis = newEllipsis;
                        try {
                            Object result = null;
                            for (int i = 2; i < list.size(); i++) {
                                result = eval(list.get(i), env);
                            }
                            return result;
                        } finally {
                            syntaxPatternBindings = prevBindings;
                            syntaxPatternEllipsis = prevEllipsis;
                        }
                    }
                }
            }
            // Function application
            Object proc = eval(head, env);
            if (proc instanceof SyntaxRules sr) {
                return expandAndEvalMacro(sr, list, env);
            }
            if (proc instanceof SyntaxCaseTransformer sct) {
                return expandSyntaxCaseTransformer(sct, list, env);
            }
            List<Object> args = new ArrayList<>();
            for (int i = 1; i < list.size(); i++) {
                args.add(eval(list.get(i), env));
            }
            return apply(proc, args);
        }
        throw new EvalError("cannot eval: " + expr);
    }

    private Object doCallCC(Object func) throws EvalError {
        Continuation cont = new Continuation();
        lastCapturedCont = cont;
        try {
            cont.inExtent = true;
            Object result = resolve(apply(func, List.of(cont)));
            cont.inExtent = false;
            // Reset capturedBody so the OUTER body loop sets it to the correct enclosing body
            cont.capturedBody = null;
            return result;
        } catch (ContinuationEscape ce) {
            if (ce.cont == cont) {
                cont.inExtent = false;
                lastCapturedCont = null; // escape: no reentrant use needed
                return ce.value;
            }
            throw ce;
        }
    }

    private Object expandSyntaxCaseTransformer(SyntaxCaseTransformer sct, List<?> form, Env useEnv) throws EvalError {
        List<Object> stripped = new ArrayList<>();
        for (Object elem : form) stripped.add(stripLocated(elem));
        SyntaxObject stx = new SyntaxObject(stripped, useEnv);

        Env prevDefEnv = currentMacroDefEnv;
        var prevRenameMap = lastSyntaxRenameMap;
        currentMacroDefEnv = sct.defEnv();
        lastSyntaxRenameMap = null;
        try {
            Object result = resolve(apply(sct.proc(), List.of(stx)));

            Object expandedCode;
            Env hygieneSrcEnv;
            if (result instanceof SyntaxObject so) {
                expandedCode = so.datum();
                hygieneSrcEnv = so.context() != null ? so.context() : sct.defEnv();
            } else {
                expandedCode = result;
                hygieneSrcEnv = sct.defEnv();
            }

            // Define hygiene bindings in useEnv (gensyms are unique, no collision risk)
            if (lastSyntaxRenameMap != null) {
                for (Map.Entry<String, String> entry : lastSyntaxRenameMap.entrySet()) {
                    String origName = entry.getKey();
                    String gensymName = entry.getValue();
                    try {
                        Object val = hygieneSrcEnv.lookup(origName);
                        useEnv.define(gensymName, val);
                    } catch (EvalError e) {
                        // macro-introduced binding, will be bound by let/lambda/define
                    }
                }
            }

            return eval(expandedCode, useEnv);
        } finally {
            currentMacroDefEnv = prevDefEnv;
            lastSyntaxRenameMap = prevRenameMap;
        }
    }

    private Object doDynamicWind(Object inThunk, Object bodyThunk, Object outThunk) throws EvalError {
        resolve(apply(inThunk, List.of()));
        Object result;
        try {
            result = resolve(apply(bodyThunk, List.of()));
        } catch (ContinuationEscape ce) {
            resolve(apply(outThunk, List.of()));
            throw ce;
        } catch (ContinuationResume cr) {
            resolve(apply(outThunk, List.of()));
            throw cr;
        } catch (SchemeRaise sr) {
            resolve(apply(outThunk, List.of()));
            throw sr;
        }
        resolve(apply(outThunk, List.of()));
        return result;
    }

    private Object apply(Object proc, List<Object> args) throws EvalError {
        if (proc instanceof Continuation cont) {
            if (args.size() != 1) throw new EvalError("continuation: expected 1 argument");
            Object value = args.get(0);
            if (cont.inExtent) {
                throw new ContinuationEscape(cont, value);
            } else {
                throw new ContinuationResume(cont, value);
            }
        }
        if (proc instanceof Lambda lambda) {
            int nParams = lambda.params().size();
            if (lambda.restParam() != null) {
                if (args.size() < nParams) {
                    throw new EvalError("wrong number of arguments: expected at least " + nParams + ", got " + args.size());
                }
            } else {
                if (args.size() != nParams) {
                    throw new EvalError("wrong number of arguments: expected " + nParams + ", got " + args.size());
                }
            }
            Env callEnv = new Env(lambda.closure());
            for (int i = 0; i < nParams; i++) {
                callEnv.define(lambda.params().get(i), args.get(i));
            }
            if (lambda.restParam() != null) {
                Object rest = NIL;
                for (int i = args.size() - 1; i >= nParams; i--) {
                    rest = new Pair(args.get(i), rest);
                }
                callEnv.define(lambda.restParam(), rest);
            }
            for (int i = 0; i < lambda.body().size() - 1; i++) {
                eval(lambda.body().get(i), callEnv);
                if (lastCapturedCont != null && lastCapturedCont.capturedBody == null) {
                    lastCapturedCont.capturedBody = lambda.body();
                    lastCapturedCont.capturedBodyIndex = i;
                }
            }
            return new TailCall(lambda.body().getLast(), callEnv);
        }
        if (proc instanceof CaseLambda cl) {
            for (Lambda clause : cl.clauses()) {
                int nParams = clause.params().size();
                if (clause.restParam() != null) {
                    if (args.size() >= nParams) return apply(clause, args);
                } else {
                    if (args.size() == nParams) return apply(clause, args);
                }
            }
            throw new EvalError("case-lambda: no matching clause for " + args.size() + " arguments");
        }
        if (proc instanceof Builtin b) {
            String name = b.name();
            if (name.equals("call/cc") || name.equals("call-with-current-continuation")) {
                if (args.size() != 1) throw new EvalError("call/cc: expected 1 argument");
                if (hasPendingCallccValue) {
                    Object v = pendingCallccValue;
                    hasPendingCallccValue = false;
                    pendingCallccValue = null;
                    return v;
                }
                return doCallCC(args.get(0));
            }
            if (name.equals("dynamic-wind")) {
                if (args.size() != 3) throw new EvalError("dynamic-wind: expected 3 arguments");
                return doDynamicWind(args.get(0), args.get(1), args.get(2));
            }
            if (name.equals("raise")) {
                if (args.size() != 1) throw new EvalError("raise: expected 1 argument");
                throw new SchemeRaise(args.get(0));
            }
            if (name.equals("with-exception-handler")) {
                if (args.size() != 2) throw new EvalError("with-exception-handler: expected 2 arguments");
                Object handler = args.get(0);
                Object thunk = args.get(1);
                try {
                    return resolve(apply(thunk, List.of()));
                } catch (SchemeRaise sr) {
                    return resolve(apply(handler, List.of(sr.value)));
                }
            }
            if (name.equals("values")) {
                if (args.size() == 1) return args.get(0);
                return new MultipleValues(args);
            }
            if (name.equals("call-with-values")) {
                if (args.size() != 2) throw new EvalError("call-with-values: expected 2 arguments");
                Object producer = args.get(0);
                Object consumer = args.get(1);
                Object produced = resolve(apply(producer, List.of()));
                List<Object> consumerArgs;
                if (produced instanceof MultipleValues mv) {
                    consumerArgs = mv.vals();
                } else {
                    consumerArgs = List.of(produced);
                }
                return apply(consumer, consumerArgs);
            }
            return applyBuiltin(name, args);
        }
        throw new EvalError("not a procedure: " + schemeToString(proc));
    }

    private Object quote(Object datum) {
        if (datum instanceof Located loc) datum = loc.expr();
        if (datum instanceof List<?> list) {
            Object result = NIL;
            for (int i = list.size() - 1; i >= 0; i--) {
                result = new Pair(quote(list.get(i)), result);
            }
            return result;
        }
        return datum;
    }

    private Object applyBuiltin(String op, List<Object> args) throws EvalError {
        return switch (op) {
            case "+" -> {
                if (anyInexact(args)) {
                    double sum = 0;
                    for (Object a : args) sum += toDouble(a);
                    yield sum;
                }
                Object sum = 0L;
                for (Object a : args) { requireNumeric(a); sum = exactAdd(sum, a); }
                yield sum;
            }
            case "-" -> {
                if (args.isEmpty()) throw new EvalError("-: need at least one argument");
                for (Object a : args) requireNumeric(a);
                if (anyInexact(args)) {
                    if (args.size() == 1) yield -toDouble(args.getFirst());
                    double r = toDouble(args.getFirst());
                    for (int i = 1; i < args.size(); i++) r -= toDouble(args.get(i));
                    yield r;
                }
                if (args.size() == 1) {
                    Object a = args.getFirst();
                    if (a instanceof Long l) yield -l;
                    if (a instanceof Rational rat) yield new Rational(-rat.num(), rat.den()).simplify();
                }
                Object r = args.getFirst();
                for (int i = 1; i < args.size(); i++) r = exactSub(r, args.get(i));
                yield r;
            }
            case "*" -> {
                if (anyInexact(args)) {
                    double p = 1;
                    for (Object a : args) p *= toDouble(a);
                    yield p;
                }
                Object p = 1L;
                for (Object a : args) { requireNumeric(a); p = exactMul(p, a); }
                yield p;
            }
            case "/" -> {
                if (args.size() < 2) throw new EvalError("/: need at least two arguments");
                for (Object a : args) requireNumeric(a);
                if (anyInexact(args)) {
                    double r2 = toDouble(args.getFirst());
                    for (int i = 1; i < args.size(); i++) {
                        double d = toDouble(args.get(i));
                        if (d == 0) throw new EvalError("division by zero");
                        r2 /= d;
                    }
                    yield r2;
                }
                try {
                    Object r2 = args.getFirst();
                    for (int i = 1; i < args.size(); i++) {
                        r2 = exactDiv(r2, args.get(i));
                    }
                    yield r2;
                } catch (ArithmeticException e) {
                    throw new EvalError("division by zero");
                }
            }
            case "<" -> {
                requireArgs(op, args, 2);
                yield toDouble(args.get(0)) < toDouble(args.get(1)) ? Boolean.TRUE : Boolean.FALSE;
            }
            case ">" -> {
                requireArgs(op, args, 2);
                yield toDouble(args.get(0)) > toDouble(args.get(1)) ? Boolean.TRUE : Boolean.FALSE;
            }
            case "=" -> {
                requireArgs(op, args, 2);
                yield toDouble(args.get(0)) == toDouble(args.get(1)) ? Boolean.TRUE : Boolean.FALSE;
            }
            case "<=" -> {
                requireArgs(op, args, 2);
                yield toDouble(args.get(0)) <= toDouble(args.get(1)) ? Boolean.TRUE : Boolean.FALSE;
            }
            case ">=" -> {
                requireArgs(op, args, 2);
                yield toDouble(args.get(0)) >= toDouble(args.get(1)) ? Boolean.TRUE : Boolean.FALSE;
            }
            case "cons" -> {
                requireArgs(op, args, 2);
                yield new Pair(args.get(0), args.get(1));
            }
            case "car" -> {
                requireArgs(op, args, 1);
                if (args.get(0) instanceof Pair p) yield p.car();
                throw new EvalError("car: not a pair");
            }
            case "cdr" -> {
                requireArgs(op, args, 1);
                if (args.get(0) instanceof Pair p) yield p.cdr();
                throw new EvalError("cdr: not a pair");
            }
            case "set-car!" -> {
                requireArgs(op, args, 2);
                if (!(args.get(0) instanceof Pair p)) throw new EvalError("set-car!: not a pair");
                p.car = args.get(1);
                yield Boolean.TRUE; // unspecified
            }
            case "set-cdr!" -> {
                requireArgs(op, args, 2);
                if (!(args.get(0) instanceof Pair p)) throw new EvalError("set-cdr!: not a pair");
                p.cdr = args.get(1);
                yield Boolean.TRUE; // unspecified
            }
            case "reverse" -> {
                requireArgs(op, args, 1);
                Object lst = args.get(0);
                Object result = NIL;
                while (lst instanceof Pair p) {
                    result = new Pair(p.car(), result);
                    lst = p.cdr();
                }
                yield result;
            }
            case "member" -> {
                requireArgs(op, args, 2);
                Object key = args.get(0);
                Object lst = args.get(1);
                while (lst instanceof Pair p) {
                    if (schemeEqual(key, p.car())) yield lst;
                    lst = p.cdr();
                }
                yield Boolean.FALSE;
            }
            case "memq" -> {
                requireArgs(op, args, 2);
                Object key = args.get(0);
                Object lst = args.get(1);
                while (lst instanceof Pair p) {
                    if (schemeEq(key, p.car())) yield lst;
                    lst = p.cdr();
                }
                yield Boolean.FALSE;
            }
            case "memv" -> {
                requireArgs(op, args, 2);
                Object key = args.get(0);
                Object lst = args.get(1);
                while (lst instanceof Pair p) {
                    if (schemeEqv(key, p.car())) yield lst;
                    lst = p.cdr();
                }
                yield Boolean.FALSE;
            }
            case "assq" -> {
                requireArgs(op, args, 2);
                Object key = args.get(0);
                Object alist = args.get(1);
                while (alist instanceof Pair p) {
                    if (p.car() instanceof Pair entry && schemeEq(key, entry.car())) {
                        yield entry;
                    }
                    alist = p.cdr();
                }
                yield Boolean.FALSE;
            }
            case "assv" -> {
                requireArgs(op, args, 2);
                Object key = args.get(0);
                Object alist = args.get(1);
                while (alist instanceof Pair p) {
                    if (p.car() instanceof Pair entry && schemeEqv(key, entry.car())) {
                        yield entry;
                    }
                    alist = p.cdr();
                }
                yield Boolean.FALSE;
            }
            case "cddr" -> {
                requireArgs(op, args, 1);
                if (!(args.get(0) instanceof Pair p1)) throw new EvalError("cddr: not a pair");
                if (!(p1.cdr() instanceof Pair p2)) throw new EvalError("cddr: cdr is not a pair");
                yield p2.cdr();
            }
            case "cadr" -> {
                requireArgs(op, args, 1);
                if (!(args.get(0) instanceof Pair p1)) throw new EvalError("cadr: not a pair");
                if (!(p1.cdr() instanceof Pair p2)) throw new EvalError("cadr: cdr is not a pair");
                yield p2.car();
            }
            case "caar" -> {
                requireArgs(op, args, 1);
                if (!(args.get(0) instanceof Pair p1)) throw new EvalError("caar: not a pair");
                if (!(p1.car() instanceof Pair p2)) throw new EvalError("caar: car is not a pair");
                yield p2.car();
            }
            case "cdar" -> {
                requireArgs(op, args, 1);
                if (!(args.get(0) instanceof Pair p1)) throw new EvalError("cdar: not a pair");
                if (!(p1.car() instanceof Pair p2)) throw new EvalError("cdar: car is not a pair");
                yield p2.cdr();
            }
            case "caddr" -> {
                requireArgs(op, args, 1);
                if (!(args.get(0) instanceof Pair p1)) throw new EvalError("caddr: not a pair");
                if (!(p1.cdr() instanceof Pair p2)) throw new EvalError("caddr: not a pair");
                if (!(p2.cdr() instanceof Pair p3)) throw new EvalError("caddr: not a pair");
                yield p3.car();
            }
            case "cdddr" -> {
                requireArgs(op, args, 1);
                if (!(args.get(0) instanceof Pair p1)) throw new EvalError("cdddr: not a pair");
                if (!(p1.cdr() instanceof Pair p2)) throw new EvalError("cdddr: not a pair");
                if (!(p2.cdr() instanceof Pair p3)) throw new EvalError("cdddr: not a pair");
                yield p3.cdr();
            }
            case "cadadr" -> {
                requireArgs(op, args, 1);
                if (!(args.get(0) instanceof Pair p1)) throw new EvalError("cadadr: not a pair");
                if (!(p1.cdr() instanceof Pair p2)) throw new EvalError("cadadr: not a pair");
                if (!(p2.car() instanceof Pair p3)) throw new EvalError("cadadr: not a pair");
                if (!(p3.cdr() instanceof Pair p4)) throw new EvalError("cadadr: not a pair");
                yield p4.car();
            }
            case "gcd" -> {
                if (args.isEmpty()) yield 0L;
                long result = Math.abs(asLong(args.get(0)));
                for (int i = 1; i < args.size(); i++) {
                    long b = Math.abs(asLong(args.get(i)));
                    while (b != 0) { long t = b; b = result % b; result = t; }
                }
                yield result;
            }
            case "lcm" -> {
                if (args.isEmpty()) yield 1L;
                long result = Math.abs(asLong(args.get(0)));
                for (int i = 1; i < args.size(); i++) {
                    long b = Math.abs(asLong(args.get(i)));
                    if (result == 0 || b == 0) { result = 0; continue; }
                    long g = result; long tmp = b;
                    while (tmp != 0) { long t = tmp; tmp = g % tmp; g = t; }
                    result = result / g * b;
                }
                yield result;
            }
            case "truncate" -> {
                requireArgs(op, args, 1);
                Object v = args.get(0);
                if (v instanceof Long) yield v;
                if (v instanceof Double d) yield (Long) (long) d.doubleValue();
                if (v instanceof Rational r) yield r.num() / r.den();
                throw new EvalError("truncate: not a number");
            }
            case "round" -> {
                requireArgs(op, args, 1);
                Object v = args.get(0);
                if (v instanceof Long) yield v;
                if (v instanceof Double d) yield (Long) Math.round(d);
                if (v instanceof Rational r) {
                    long q = r.num() / r.den();
                    long rem = Math.abs(r.num() % r.den());
                    long half = r.den();
                    if (2 * rem > half) yield r.num() > 0 ? q + 1 : q - 1;
                    if (2 * rem == half) yield (q % 2 == 0) ? q : (r.num() > 0 ? q + 1 : q - 1);
                    yield q;
                }
                throw new EvalError("round: not a number");
            }
            case "make-string" -> {
                if (args.size() < 1 || args.size() > 2) throw new EvalError("make-string: expected 1-2 arguments");
                int len = (int) asLong(args.get(0));
                char fill = args.size() > 1 ? ((SchemeChar) args.get(1)).value() : ' ';
                char[] chars = new char[len];
                java.util.Arrays.fill(chars, fill);
                yield new SchemeString(chars);
            }
            case "string" -> {
                char[] chars = new char[args.size()];
                for (int i = 0; i < args.size(); i++) {
                    if (!(args.get(i) instanceof SchemeChar sc)) throw new EvalError("string: not a char");
                    chars[i] = sc.value();
                }
                yield new SchemeString(chars);
            }
            case "string>?" -> {
                requireArgs(op, args, 2);
                yield stringVal(args.get(0)).compareTo(stringVal(args.get(1))) > 0 ? Boolean.TRUE : Boolean.FALSE;
            }
            case "string<=?" -> {
                requireArgs(op, args, 2);
                yield stringVal(args.get(0)).compareTo(stringVal(args.get(1))) <= 0 ? Boolean.TRUE : Boolean.FALSE;
            }
            case "string>=?" -> {
                requireArgs(op, args, 2);
                yield stringVal(args.get(0)).compareTo(stringVal(args.get(1))) >= 0 ? Boolean.TRUE : Boolean.FALSE;
            }
            case "null?" -> {
                requireArgs(op, args, 1);
                yield args.get(0) == NIL ? Boolean.TRUE : Boolean.FALSE;
            }
            case "list" -> {
                Object result = NIL;
                for (int i = args.size() - 1; i >= 0; i--) {
                    result = new Pair(args.get(i), result);
                }
                yield result;
            }
            case "length" -> {
                Object obj = args.get(0);
                Object slow = obj, fast = obj;
                long len = 0;
                while (obj instanceof Pair p) {
                    len++;
                    obj = p.cdr();
                    // cycle detection with tortoise/hare
                    if (len % 2 == 0 && slow instanceof Pair ps) slow = ps.cdr();
                    if (obj instanceof Pair && obj == slow && len > 1) throw new EvalError("length: circular list");
                }
                if (obj != NIL) throw new EvalError("length: not a proper list");
                yield len;
            }
            case "append" -> {
                Object result = NIL;
                for (int i = args.size() - 1; i >= 0; i--) {
                    Object lst = args.get(i);
                    if (i == args.size() - 1) {
                        result = lst;
                    } else {
                        // Prepend elements of lst onto result
                        List<Object> elems = new ArrayList<>();
                        Object cur = lst;
                        while (cur instanceof Pair p) {
                            elems.add(p.car());
                            cur = p.cdr();
                        }
                        for (int j = elems.size() - 1; j >= 0; j--) {
                            result = new Pair(elems.get(j), result);
                        }
                    }
                }
                yield result;
            }
            case "not" -> {
                requireArgs(op, args, 1);
                yield isFalse(args.get(0)) ? Boolean.TRUE : Boolean.FALSE;
            }
            case "string?" -> {
                requireArgs(op, args, 1);
                yield args.get(0) instanceof SchemeString ? Boolean.TRUE : Boolean.FALSE;
            }
            case "number?" -> {
                requireArgs(op, args, 1);
                yield isNumber(args.get(0)) ? Boolean.TRUE : Boolean.FALSE;
            }
            case "boolean?" -> {
                requireArgs(op, args, 1);
                yield args.get(0) instanceof Boolean ? Boolean.TRUE : Boolean.FALSE;
            }
            case "pair?" -> {
                requireArgs(op, args, 1);
                yield args.get(0) instanceof Pair ? Boolean.TRUE : Boolean.FALSE;
            }
            case "symbol?" -> {
                requireArgs(op, args, 1);
                yield args.get(0) instanceof String ? Boolean.TRUE : Boolean.FALSE;
            }
            case "char?" -> {
                requireArgs(op, args, 1);
                yield args.get(0) instanceof SchemeChar ? Boolean.TRUE : Boolean.FALSE;
            }
            case "display" -> {
                requireArgs(op, args, 1);
                if (outputBuffer != null) outputBuffer.append(displayString(args.get(0)));
                yield null;
            }
            case "write" -> {
                requireArgs(op, args, 1);
                if (outputBuffer != null) outputBuffer.append(schemeToString(args.get(0)));
                yield null;
            }
            case "newline" -> {
                if (outputBuffer != null) outputBuffer.append('\n');
                yield null;
            }
            case "string-append" -> {
                StringBuilder sb = new StringBuilder();
                for (Object a : args) {
                    if (a instanceof SchemeString ss) sb.append(ss.value());
                    else throw new EvalError("string-append: not a string");
                }
                yield new SchemeString(sb.toString());
            }
            case "string-length" -> {
                requireArgs(op, args, 1);
                if (args.get(0) instanceof SchemeString ss) yield (long) ss.value().length();
                throw new EvalError("string-length: not a string");
            }
            case "substring" -> {
                requireArgs(op, args, 3);
                if (args.get(0) instanceof SchemeString ss) {
                    int start = (int) asLong(args.get(1));
                    int end = (int) asLong(args.get(2));
                    yield new SchemeString(ss.value().substring(start, end));
                }
                throw new EvalError("substring: not a string");
            }
            case "string->number" -> {
                requireArgs(op, args, 1);
                if (args.get(0) instanceof SchemeString ss) {
                    try { yield Long.parseLong(ss.value()); }
                    catch (NumberFormatException e) { yield Boolean.FALSE; }
                }
                throw new EvalError("string->number: not a string");
            }
            case "number->string" -> {
                requireArgs(op, args, 1);
                yield new SchemeString(schemeToString(args.get(0)));
            }
            case "symbol->string" -> {
                requireArgs(op, args, 1);
                if (args.get(0) instanceof String s) yield new SchemeString(s);
                throw new EvalError("symbol->string: not a symbol");
            }
            case "string->symbol" -> {
                requireArgs(op, args, 1);
                if (args.get(0) instanceof SchemeString ss) yield ss.value();
                throw new EvalError("string->symbol: not a string");
            }
            case "string-ref" -> {
                requireArgs(op, args, 2);
                if (args.get(0) instanceof SchemeString ss) {
                    int idx = (int) asLong(args.get(1));
                    yield new SchemeChar(ss.charAt(idx));
                }
                throw new EvalError("string-ref: not a string");
            }
            case "string-set!" -> {
                requireArgs(op, args, 3);
                if (args.get(0) instanceof SchemeString ss) {
                    if (ss.isImmutable())
                        throw new EvalError("string-set!: strings are immutable");
                    int idx = (int) asLong(args.get(1));
                    if (!(args.get(2) instanceof SchemeChar sc))
                        throw new EvalError("string-set!: third argument must be a character");
                    ss.setChar(idx, sc.value());
                    yield null;
                }
                throw new EvalError("string-set!: not a string");
            }
            case "string-copy" -> {
                requireArgs(op, args, 1);
                if (args.get(0) instanceof SchemeString ss) {
                    yield new SchemeString(ss.value().toCharArray());
                }
                throw new EvalError("string-copy: not a string");
            }
            case "string->list" -> {
                requireArgs(op, args, 1);
                if (args.get(0) instanceof SchemeString ss) {
                    String s = ss.value();
                    Object result = NIL;
                    for (int i = s.length() - 1; i >= 0; i--) {
                        result = new Pair(new SchemeChar(s.charAt(i)), result);
                    }
                    yield result;
                }
                throw new EvalError("string->list: not a string");
            }
            case "list->string" -> {
                requireArgs(op, args, 1);
                Object cur = args.get(0);
                StringBuilder sb = new StringBuilder();
                while (cur instanceof Pair p) {
                    if (!(p.car() instanceof SchemeChar sc))
                        throw new EvalError("list->string: not a char");
                    sb.append(sc.value());
                    cur = p.cdr();
                }
                yield new SchemeString(sb.toString());
            }
            case "char->integer" -> {
                requireArgs(op, args, 1);
                if (args.get(0) instanceof SchemeChar sc) {
                    yield (long) sc.value();
                }
                throw new EvalError("char->integer: not a char");
            }
            case "integer->char" -> {
                requireArgs(op, args, 1);
                long n = asLong(args.get(0));
                yield new SchemeChar((char) n);
            }
            case "apply" -> {
                if (args.size() < 2) throw new EvalError("apply: need at least two arguments");
                Object proc = args.get(0);
                // Last argument must be a list; prefix args are prepended
                Object lastArg = args.get(args.size() - 1);
                List<Object> callArgs = new ArrayList<>();
                for (int i = 1; i < args.size() - 1; i++) {
                    callArgs.add(args.get(i));
                }
                // Unpack the last argument (a list)
                Object cur = lastArg;
                while (cur instanceof Pair p) {
                    callArgs.add(p.car());
                    cur = p.cdr();
                }
                yield apply(proc, callArgs);
            }
            case "eq?" -> {
                requireArgs(op, args, 2);
                Object a = args.get(0), b = args.get(1);
                yield (a == b || a.equals(b)) ? Boolean.TRUE : Boolean.FALSE;
            }
            case "equal?" -> {
                requireArgs(op, args, 2);
                yield schemeEqual(args.get(0), args.get(1)) ? Boolean.TRUE : Boolean.FALSE;
            }
            case "eqv?" -> {
                requireArgs(op, args, 2);
                yield schemeEqv(args.get(0), args.get(1)) ? Boolean.TRUE : Boolean.FALSE;
            }
            case "vector" -> {
                Object[] data = new Object[args.size()];
                for (int i = 0; i < args.size(); i++) data[i] = args.get(i);
                yield new SchemeVector(data);
            }
            case "make-vector" -> {
                if (args.size() < 1 || args.size() > 2) throw new EvalError("make-vector: expected 1-2 arguments");
                int size = (int) asLong(args.get(0));
                Object fill = args.size() > 1 ? args.get(1) : 0L;
                yield new SchemeVector(size, fill);
            }
            case "vector-ref" -> {
                requireArgs(op, args, 2);
                if (!(args.get(0) instanceof SchemeVector v)) throw new EvalError("vector-ref: not a vector");
                yield v.ref((int) asLong(args.get(1)));
            }
            case "vector-set!" -> {
                requireArgs(op, args, 3);
                if (!(args.get(0) instanceof SchemeVector v)) throw new EvalError("vector-set!: not a vector");
                v.set((int) asLong(args.get(1)), args.get(2));
                yield null;
            }
            case "vector-length" -> {
                requireArgs(op, args, 1);
                if (!(args.get(0) instanceof SchemeVector v)) throw new EvalError("vector-length: not a vector");
                yield (long) v.length();
            }
            case "vector?" -> {
                requireArgs(op, args, 1);
                yield args.get(0) instanceof SchemeVector ? Boolean.TRUE : Boolean.FALSE;
            }
            case "vector->list" -> {
                requireArgs(op, args, 1);
                if (!(args.get(0) instanceof SchemeVector v)) throw new EvalError("vector->list: not a vector");
                Object result = NIL;
                for (int i = v.length() - 1; i >= 0; i--) result = new Pair(v.ref(i), result);
                yield result;
            }
            case "list->vector" -> {
                requireArgs(op, args, 1);
                List<Object> elems = new ArrayList<>();
                Object cur = args.get(0);
                while (cur instanceof Pair p) { elems.add(p.car()); cur = p.cdr(); }
                yield new SchemeVector(elems.toArray());
            }
            case "for-each" -> {
                if (args.size() < 2) throw new EvalError("for-each: need at least two arguments");
                Object proc = args.get(0);
                List<Object> lists = new ArrayList<>();
                for (int i = 1; i < args.size(); i++) lists.add(args.get(i));
                while (true) {
                    boolean allPairs = true;
                    for (Object l : lists) {
                        if (!(l instanceof Pair)) { allPairs = false; break; }
                    }
                    if (!allPairs) break;
                    List<Object> callArgs = new ArrayList<>();
                    for (int i = 0; i < lists.size(); i++) {
                        Pair p = (Pair) lists.get(i);
                        callArgs.add(p.car());
                        lists.set(i, p.cdr());
                    }
                    resolve(apply(proc, callArgs));
                }
                yield null;
            }
            case "map" -> {
                if (args.size() < 2) throw new EvalError("map: need at least two arguments");
                Object proc = args.get(0);
                List<Object> lists = new ArrayList<>();
                for (int i = 1; i < args.size(); i++) lists.add(args.get(i));
                List<Object> resultElems = new ArrayList<>();
                while (true) {
                    boolean allPairs = true;
                    for (Object l : lists) {
                        if (!(l instanceof Pair)) { allPairs = false; break; }
                    }
                    if (!allPairs) break;
                    List<Object> callArgs = new ArrayList<>();
                    for (int i = 0; i < lists.size(); i++) {
                        Pair p = (Pair) lists.get(i);
                        callArgs.add(p.car());
                        lists.set(i, p.cdr());
                    }
                    resultElems.add(resolve(apply(proc, callArgs)));
                }
                Object result = NIL;
                for (int i = resultElems.size() - 1; i >= 0; i--) {
                    result = new Pair(resultElems.get(i), result);
                }
                yield result;
            }
            case "abs" -> {
                requireArgs(op, args, 1);
                yield Math.abs(asLong(args.get(0)));
            }
            case "quotient" -> {
                requireArgs(op, args, 2);
                long dividend = asLong(args.get(0)), divisor = asLong(args.get(1));
                if (divisor == 0) throw new EvalError("quotient: division by zero");
                yield dividend / divisor;
            }
            case "remainder" -> {
                requireArgs(op, args, 2);
                long dividend = asLong(args.get(0)), divisor = asLong(args.get(1));
                if (divisor == 0) throw new EvalError("remainder: division by zero");
                yield dividend % divisor;
            }
            case "modulo" -> {
                requireArgs(op, args, 2);
                long dividend = asLong(args.get(0)), divisor = asLong(args.get(1));
                if (divisor == 0) throw new EvalError("modulo: division by zero");
                long rem = dividend % divisor;
                if (rem != 0 && ((rem > 0) != (divisor > 0))) rem += divisor;
                yield rem;
            }
            case "min" -> {
                if (args.isEmpty()) throw new EvalError("min: need at least one argument");
                long m = asLong(args.get(0));
                for (int i = 1; i < args.size(); i++) m = Math.min(m, asLong(args.get(i)));
                yield m;
            }
            case "max" -> {
                if (args.isEmpty()) throw new EvalError("max: need at least one argument");
                long m = asLong(args.get(0));
                for (int i = 1; i < args.size(); i++) m = Math.max(m, asLong(args.get(i)));
                yield m;
            }
            case "expt" -> {
                requireArgs(op, args, 2);
                long base = asLong(args.get(0)), exp = asLong(args.get(1));
                long result = 1;
                for (long i = 0; i < exp; i++) result *= base;
                yield result;
            }
            case "zero?" -> {
                requireArgs(op, args, 1);
                yield asLong(args.get(0)) == 0 ? Boolean.TRUE : Boolean.FALSE;
            }
            case "positive?" -> {
                requireArgs(op, args, 1);
                yield asLong(args.get(0)) > 0 ? Boolean.TRUE : Boolean.FALSE;
            }
            case "negative?" -> {
                requireArgs(op, args, 1);
                yield asLong(args.get(0)) < 0 ? Boolean.TRUE : Boolean.FALSE;
            }
            case "odd?" -> {
                requireArgs(op, args, 1);
                yield asLong(args.get(0)) % 2 != 0 ? Boolean.TRUE : Boolean.FALSE;
            }
            case "even?" -> {
                requireArgs(op, args, 1);
                yield asLong(args.get(0)) % 2 == 0 ? Boolean.TRUE : Boolean.FALSE;
            }
            case "list-ref" -> {
                requireArgs(op, args, 2);
                Object lst = args.get(0);
                int idx = (int) asLong(args.get(1));
                for (int i = 0; i < idx; i++) {
                    if (!(lst instanceof Pair p)) throw new EvalError("list-ref: index out of range");
                    lst = p.cdr();
                }
                if (!(lst instanceof Pair p)) throw new EvalError("list-ref: index out of range");
                yield p.car();
            }
            case "list-tail" -> {
                requireArgs(op, args, 2);
                Object lst = args.get(0);
                int idx = (int) asLong(args.get(1));
                for (int i = 0; i < idx; i++) {
                    if (!(lst instanceof Pair p)) throw new EvalError("list-tail: index out of range");
                    lst = p.cdr();
                }
                yield lst;
            }
            case "list?" -> {
                requireArgs(op, args, 1);
                Object slow = args.get(0);
                Object fast = args.get(0);
                while (fast instanceof Pair pf) {
                    fast = pf.cdr();
                    if (!(fast instanceof Pair pf2)) break;
                    fast = pf2.cdr();
                    slow = ((Pair) slow).cdr();
                    if (slow == fast) yield Boolean.FALSE; // cycle
                }
                yield fast == NIL ? Boolean.TRUE : Boolean.FALSE;
            }
            case "assoc" -> {
                requireArgs(op, args, 2);
                Object key = args.get(0);
                Object alist = args.get(1);
                while (alist instanceof Pair p) {
                    if (p.car() instanceof Pair entry && schemeEqual(entry.car(), key)) {
                        yield entry;
                    }
                    alist = p.cdr();
                }
                yield Boolean.FALSE;
            }
            case "char-alphabetic?" -> {
                requireArgs(op, args, 1);
                if (!(args.get(0) instanceof SchemeChar sc)) throw new EvalError("char-alphabetic?: not a char");
                yield Character.isLetter(sc.value()) ? Boolean.TRUE : Boolean.FALSE;
            }
            case "char-numeric?" -> {
                requireArgs(op, args, 1);
                if (!(args.get(0) instanceof SchemeChar sc)) throw new EvalError("char-numeric?: not a char");
                yield Character.isDigit(sc.value()) ? Boolean.TRUE : Boolean.FALSE;
            }
            case "char-upcase" -> {
                requireArgs(op, args, 1);
                if (!(args.get(0) instanceof SchemeChar sc)) throw new EvalError("char-upcase: not a char");
                yield new SchemeChar(Character.toUpperCase(sc.value()));
            }
            case "char-downcase" -> {
                requireArgs(op, args, 1);
                if (!(args.get(0) instanceof SchemeChar sc)) throw new EvalError("char-downcase: not a char");
                yield new SchemeChar(Character.toLowerCase(sc.value()));
            }
            case "char=?" -> {
                requireArgs(op, args, 2);
                if (!(args.get(0) instanceof SchemeChar a)) throw new EvalError("char=?: not a char");
                if (!(args.get(1) instanceof SchemeChar b)) throw new EvalError("char=?: not a char");
                yield a.value() == b.value() ? Boolean.TRUE : Boolean.FALSE;
            }
            case "char<?" -> {
                requireArgs(op, args, 2);
                if (!(args.get(0) instanceof SchemeChar a)) throw new EvalError("char<?: not a char");
                if (!(args.get(1) instanceof SchemeChar b)) throw new EvalError("char<?: not a char");
                yield a.value() < b.value() ? Boolean.TRUE : Boolean.FALSE;
            }
            case "string=?" -> {
                requireArgs(op, args, 2);
                if (!(args.get(0) instanceof SchemeString a)) throw new EvalError("string=?: not a string");
                if (!(args.get(1) instanceof SchemeString b)) throw new EvalError("string=?: not a string");
                yield a.value().equals(b.value()) ? Boolean.TRUE : Boolean.FALSE;
            }
            case "string<?" -> {
                requireArgs(op, args, 2);
                if (!(args.get(0) instanceof SchemeString a)) throw new EvalError("string<?: not a string");
                if (!(args.get(1) instanceof SchemeString b)) throw new EvalError("string<?: not a string");
                yield a.value().compareTo(b.value()) < 0 ? Boolean.TRUE : Boolean.FALSE;
            }
            case "string-ci=?" -> {
                requireArgs(op, args, 2);
                if (!(args.get(0) instanceof SchemeString a)) throw new EvalError("string-ci=?: not a string");
                if (!(args.get(1) instanceof SchemeString b)) throw new EvalError("string-ci=?: not a string");
                yield a.value().equalsIgnoreCase(b.value()) ? Boolean.TRUE : Boolean.FALSE;
            }
            case "string-upcase" -> {
                requireArgs(op, args, 1);
                if (!(args.get(0) instanceof SchemeString ss)) throw new EvalError("string-upcase: not a string");
                yield new SchemeString(ss.value().toUpperCase());
            }
            case "string-downcase" -> {
                requireArgs(op, args, 1);
                if (!(args.get(0) instanceof SchemeString ss)) throw new EvalError("string-downcase: not a string");
                yield new SchemeString(ss.value().toLowerCase());
            }
            case "exact?" -> {
                requireArgs(op, args, 1);
                yield isExact(args.get(0)) ? Boolean.TRUE : Boolean.FALSE;
            }
            case "inexact?" -> {
                requireArgs(op, args, 1);
                yield (args.get(0) instanceof Double) ? Boolean.TRUE : Boolean.FALSE;
            }
            case "exact->inexact" -> {
                requireArgs(op, args, 1);
                yield toDouble(args.get(0));
            }
            case "inexact->exact" -> {
                requireArgs(op, args, 1);
                Object a = args.get(0);
                if (a instanceof Long) yield a;
                if (a instanceof Rational) yield a;
                if (a instanceof Double d) {
                    // Convert double to rational via continued fraction / simple approach
                    // 0.5 -> 1/2, 0.333... -> 1/3 etc.
                    // Use the standard approach: represent as p/q where q is power of 2, then simplify
                    long bits = Double.doubleToLongBits(d);
                    boolean negative = (bits & (1L << 63)) != 0;
                    int exp = (int)((bits >> 52) & 0x7FFL) - 1023;
                    long mantissa = (bits & 0x000FFFFFFFFFFFFFL) | 0x0010000000000000L;
                    // value = mantissa * 2^(exp - 52)
                    int shift = exp - 52;
                    long num, den;
                    if (shift >= 0) {
                        num = mantissa << shift;
                        den = 1;
                    } else {
                        num = mantissa;
                        den = 1L << (-shift);
                    }
                    if (negative) num = -num;
                    yield new Rational(num, den).simplify();
                }
                throw new EvalError("inexact->exact: not a number");
            }
            case "numerator" -> {
                requireArgs(op, args, 1);
                Object a = args.get(0);
                if (a instanceof Long l) yield l;
                if (a instanceof Rational rat) yield rat.num();
                throw new EvalError("numerator: not a rational number");
            }
            case "denominator" -> {
                requireArgs(op, args, 1);
                Object a = args.get(0);
                if (a instanceof Long) yield 1L;
                if (a instanceof Rational rat) yield rat.den();
                throw new EvalError("denominator: not a rational number");
            }
            case "integer?" -> {
                requireArgs(op, args, 1);
                Object a = args.get(0);
                if (a instanceof Long) yield Boolean.TRUE;
                if (a instanceof Rational rat) yield rat.isInteger() ? Boolean.TRUE : Boolean.FALSE;
                if (a instanceof Double d) yield (d == Math.floor(d) && !Double.isInfinite(d)) ? Boolean.TRUE : Boolean.FALSE;
                yield Boolean.FALSE;
            }
            case "rational?" -> {
                requireArgs(op, args, 1);
                Object a = args.get(0);
                yield (a instanceof Long || a instanceof Rational) ? Boolean.TRUE : Boolean.FALSE;
            }
            case "procedure?" -> {
                requireArgs(op, args, 1);
                Object a = args.get(0);
                yield (a instanceof Lambda || a instanceof CaseLambda || a instanceof Builtin || a instanceof Continuation) ? Boolean.TRUE : Boolean.FALSE;
            }
            case "syntax->datum" -> {
                requireArgs(op, args, 1);
                if (args.get(0) instanceof SyntaxObject so) yield so.datum();
                yield args.get(0);
            }
            case "datum->syntax" -> {
                requireArgs(op, args, 2);
                Object templateId = args.get(0);
                Object datum = args.get(1);
                Env ctx = templateId instanceof SyntaxObject so ? so.context() : null;
                yield new SyntaxObject(datum, ctx);
            }
            default -> {
                // Check for record type operations
                if (recordTypes.containsKey(op)) {
                    RecordType rt = recordTypes.get(op);
                    if (args.size() != rt.fields().size())
                        throw new EvalError(op + ": expected " + rt.fields().size() + " arguments, got " + args.size());
                    Map<String, Object> fields = new HashMap<>();
                    for (int i = 0; i < rt.fields().size(); i++) {
                        fields.put(rt.fields().get(i), args.get(i));
                    }
                    yield new SchemeRecord(rt.typeName(), fields);
                }
                if (recordPredicates.containsKey(op)) {
                    String typeName = recordPredicates.get(op);
                    if (args.size() != 1) throw new EvalError(op + ": expected 1 argument");
                    Object a = args.getFirst();
                    yield (a instanceof SchemeRecord sr && sr.typeName.equals(typeName)) ? Boolean.TRUE : Boolean.FALSE;
                }
                if (recordAccessors.containsKey(op)) {
                    String fieldName = recordAccessors.get(op);
                    if (args.size() != 1) throw new EvalError(op + ": expected 1 argument");
                    Object a = args.getFirst();
                    if (!(a instanceof SchemeRecord sr))
                        throw new EvalError(op + ": not a record");
                    if (!sr.fields.containsKey(fieldName))
                        throw new EvalError(op + ": no such field: " + fieldName);
                    yield sr.fields.get(fieldName);
                }
                throw new EvalError("unbound variable: " + op);
            }
        };
    }

    // --- Macro expansion ---

    private String gensym(String base) {
        return base + "$" + (gensymCounter++);
    }

    private Object stripLocated(Object obj) {
        if (obj instanceof Located loc) obj = loc.expr();
        if (obj instanceof List<?> list) {
            List<Object> result = new ArrayList<>();
            for (Object elem : list) result.add(stripLocated(elem));
            return result;
        }
        return obj;
    }

    private Object expandAndEvalMacro(SyntaxRules sr, List<?> form, Env useEnv) throws EvalError {
        List<Object> input = new ArrayList<>();
        for (Object elem : form) input.add(stripLocated(elem));

        for (int i = 0; i < sr.patterns().size(); i++) {
            List<?> pattern = (List<?>) sr.patterns().get(i);
            Object template = sr.templates().get(i);

            Map<String, Object> bindings = new HashMap<>();
            Set<String> ellipsisVars = new HashSet<>();
            if (matchPattern(pattern, input, sr.literals(), bindings, ellipsisVars, 1)) {
                // Collect free symbols and generate rename mappings
                Set<String> patVars = bindings.keySet();
                Map<String, String> renameMap = new HashMap<>();
                collectFreeSymbols(template, patVars, renameMap);

                // Expand template
                Object expanded = expandTemplate(template, bindings, ellipsisVars, renameMap);

                // Create wrapper env with hygienic bindings
                Env wrapperEnv = new Env(useEnv);
                for (Map.Entry<String, String> entry : renameMap.entrySet()) {
                    String origName = entry.getKey();
                    String gensymName = entry.getValue();
                    try {
                        Object val = sr.defEnv().lookup(origName);
                        wrapperEnv.define(gensymName, val);
                    } catch (EvalError e) {
                        // Not in def env (macro-introduced binding) - let/lambda will bind it
                    }
                }

                return eval(expanded, wrapperEnv);
            }
        }
        throw new EvalError("no matching syntax-rules pattern");
    }

    private boolean matchPattern(List<?> pattern, List<Object> input, List<String> literals,
                                  Map<String, Object> bindings, Set<String> ellipsisVars, int startIndex) {
        int pi = startIndex;
        int ii = startIndex;

        while (pi < pattern.size()) {
            Object pat = pattern.get(pi);
            boolean hasEllipsis = (pi + 1 < pattern.size()) && "...".equals(pattern.get(pi + 1));

            if (hasEllipsis) {
                String varName = (String) pat;
                List<Object> matches = new ArrayList<>();
                while (ii < input.size()) {
                    matches.add(input.get(ii));
                    ii++;
                }
                bindings.put(varName, matches);
                ellipsisVars.add(varName);
                pi += 2; // skip var and ...
            } else {
                if (ii >= input.size()) return false;
                if (pat instanceof String s) {
                    if ("_".equals(s)) {
                        // wildcard, don't bind
                    } else if (literals.contains(s)) {
                        if (!s.equals(input.get(ii))) return false;
                    } else {
                        bindings.put(s, input.get(ii));
                    }
                }
                pi++;
                ii++;
            }
        }
        return ii == input.size();
    }

    private void collectFreeSymbols(Object template, Set<String> patVars, Map<String, String> renameMap) {
        if (template instanceof String s) {
            if (!patVars.contains(s) && !"...".equals(s) && !SPECIAL_FORMS.contains(s)
                    && !renameMap.containsKey(s)) {
                renameMap.put(s, gensym(s));
            }
        } else if (template instanceof List<?> list) {
            // Don't rename symbols inside quoted forms
            if (!list.isEmpty() && "quote".equals(list.get(0))) return;
            for (Object elem : list) {
                collectFreeSymbols(elem, patVars, renameMap);
            }
        }
    }

    @SuppressWarnings("unchecked")
    private Object expandTemplate(Object template, Map<String, Object> bindings,
                                   Set<String> ellipsisVars, Map<String, String> renameMap) {
        if (template instanceof String s) {
            if (bindings.containsKey(s) && !ellipsisVars.contains(s)) {
                return bindings.get(s);
            }
            if (renameMap.containsKey(s)) {
                return renameMap.get(s);
            }
            return s;
        }
        if (template instanceof List<?> list) {
            // Don't expand inside quoted forms
            if (!list.isEmpty() && "quote".equals(list.get(0))) return template;
            List<Object> result = new ArrayList<>();
            for (int i = 0; i < list.size(); i++) {
                Object elem = list.get(i);
                boolean hasEllipsis = (i + 1 < list.size()) && "...".equals(list.get(i + 1));
                if (hasEllipsis) {
                    result.addAll(expandEllipsis(elem, bindings, ellipsisVars, renameMap));
                    i++; // skip ...
                } else if ("...".equals(elem)) {
                    // already handled
                } else {
                    result.add(expandTemplate(elem, bindings, ellipsisVars, renameMap));
                }
            }
            return result;
        }
        return template;
    }

    @SuppressWarnings("unchecked")
    private List<Object> expandEllipsis(Object subTemplate, Map<String, Object> bindings,
                                         Set<String> ellipsisVars, Map<String, String> renameMap) {
        // Find which ellipsis variables appear in this sub-template
        Set<String> usedEllipsisVars = new HashSet<>();
        findUsedEllipsisVars(subTemplate, ellipsisVars, usedEllipsisVars);

        if (usedEllipsisVars.isEmpty()) return List.of();

        String firstVar = usedEllipsisVars.iterator().next();
        List<?> values = (List<?>) bindings.get(firstVar);
        int count = values.size();

        List<Object> results = new ArrayList<>();
        for (int idx = 0; idx < count; idx++) {
            Map<String, Object> localBindings = new HashMap<>(bindings);
            for (String var : usedEllipsisVars) {
                List<?> vals = (List<?>) bindings.get(var);
                localBindings.put(var, vals.get(idx));
            }
            // Remove these from ellipsisVars for the recursive call so they're treated as single values
            Set<String> remainingEllipsis = new HashSet<>(ellipsisVars);
            remainingEllipsis.removeAll(usedEllipsisVars);
            results.add(expandTemplate(subTemplate, localBindings, remainingEllipsis, renameMap));
        }
        return results;
    }

    private void findUsedEllipsisVars(Object template, Set<String> ellipsisVars, Set<String> result) {
        if (template instanceof String s) {
            if (ellipsisVars.contains(s)) result.add(s);
        } else if (template instanceof List<?> list) {
            for (Object elem : list) findUsedEllipsisVars(elem, ellipsisVars, result);
        }
    }

    private boolean schemeEq(Object a, Object b) {
        if (a == b) return true;
        if (a == null || b == null) return false;
        // Symbols (String) and booleans use equals due to Java boxing
        if (a instanceof String && b instanceof String) return a.equals(b);
        if (a instanceof Boolean && b instanceof Boolean) return a.equals(b);
        // Numbers: eq? compares identity for exact, but Java boxes Long so use equals
        if (a instanceof Long && b instanceof Long) return a.equals(b);
        return false;
    }

    private boolean schemeEqv(Object a, Object b) {
        if (a == b) return true;
        if (a == null || b == null) return a == b;
        if (isNumber(a) && isNumber(b)) return toDouble(a) == toDouble(b);
        if (a instanceof Boolean && b instanceof Boolean) return a.equals(b);
        if (a instanceof String && b instanceof String) return a.equals(b);
        if (a instanceof SchemeChar ca && b instanceof SchemeChar cb) return ca.value() == cb.value();
        return false;
    }

    private boolean schemeEqual(Object a, Object b) {
        return schemeEqualRec(a, b, new HashSet<>());
    }

    private boolean schemeEqualRec(Object a, Object b, Set<Long> seen) {
        if (a == b) return true;
        if (a == null || b == null) return a == b;
        if (isNumber(a) && isNumber(b)) return toDouble(a) == toDouble(b);
        if (a instanceof Boolean && b instanceof Boolean) return a.equals(b);
        if (a instanceof String && b instanceof String) return a.equals(b);
        if (a instanceof SchemeString sa && b instanceof SchemeString sb) return sa.value().equals(sb.value());
        if (a instanceof SchemeChar ca && b instanceof SchemeChar cb) return ca.value() == cb.value();
        if (a == NIL && b == NIL) return true;
        if (a instanceof Pair pa && b instanceof Pair pb) {
            long key = ((long) System.identityHashCode(a) << 32) | (System.identityHashCode(b) & 0xFFFFFFFFL);
            if (!seen.add(key)) return true; // assume equal for cycles
            return schemeEqualRec(pa.car(), pb.car(), seen) && schemeEqualRec(pa.cdr(), pb.cdr(), seen);
        }
        if (a instanceof SchemeVector va && b instanceof SchemeVector vb) {
            if (va.length() != vb.length()) return false;
            for (int i = 0; i < va.length(); i++) {
                if (!schemeEqualRec(va.ref(i), vb.ref(i), seen)) return false;
            }
            return true;
        }
        return false;
    }

    private boolean isFalse(Object val) {
        return val instanceof Boolean b && !b;
    }

    private long asLong(Object val) throws EvalError {
        if (val instanceof Long l) return l;
        if (val instanceof Rational r && r.isInteger()) return r.num();
        throw new EvalError("expected number, got: " + schemeToString(val));
    }

    private String stringVal(Object val) throws EvalError {
        if (val instanceof SchemeString s) return s.value();
        throw new EvalError("expected string");
    }

    private static boolean isNumber(Object val) {
        return val instanceof Long || val instanceof Double || val instanceof Rational;
    }

    private static boolean isExact(Object val) {
        return val instanceof Long || val instanceof Rational;
    }

    private static double toDouble(Object val) {
        if (val instanceof Long l) return l.doubleValue();
        if (val instanceof Double d) return d;
        if (val instanceof Rational r) return r.toDouble();
        throw new IllegalArgumentException("not a number");
    }

    // Exact arithmetic on Long/Rational operands
    private static Object exactAdd(Object a, Object b) {
        return Rational.add(a, b);
    }
    private static Object exactSub(Object a, Object b) {
        return Rational.sub(a, b);
    }
    private static Object exactMul(Object a, Object b) {
        return Rational.mul(a, b);
    }
    private static Object exactDiv(Object a, Object b) {
        return Rational.div(a, b);
    }

    private static boolean anyInexact(List<Object> args) {
        for (Object a : args) if (a instanceof Double) return true;
        return false;
    }

    private void requireNumeric(Object val) throws EvalError {
        if (!isNumber(val)) throw new EvalError("expected number, got: " + schemeToString(val));
    }

    private void requireArgs(String op, List<Object> args, int n) throws EvalError {
        if (args.size() != n)
            throw new EvalError(op + ": expected " + n + " arguments, got " + args.size());
    }

    private String displayString(Object val) {
        if (val instanceof SchemeString s) return s.value();
        if (val instanceof SchemeChar c) return String.valueOf(c.value());
        return schemeToString(val);
    }

    private String schemeToString(Object val) {
        if (val == null) return "";
        if (val instanceof Long l) return l.toString();
        if (val instanceof Double d) {
            if (d == Math.floor(d) && !Double.isInfinite(d) && !Double.isNaN(d)) {
                // Print as e.g. "5.0" not "5"
                long l = (long) d.doubleValue();
                return l + ".0";
            }
            return Double.toString(d);
        }
        if (val instanceof Rational r) return r.num() + "/" + r.den();
        if (val instanceof Boolean b) return b ? "#t" : "#f";
        if (val instanceof SchemeString s) return "\"" + s.value() + "\"";
        if (val instanceof SchemeChar c) {
            return switch (c.value()) {
                case ' ' -> "#\\space";
                case '\n' -> "#\\newline";
                case '\t' -> "#\\tab";
                default -> "#\\" + c.value();
            };
        }
        if (val == NIL) return "()";
        if (val instanceof Pair) {
            StringBuilder sb = new StringBuilder("(");
            Object cur = val;
            boolean first = true;
            Set<Object> seen = Collections.newSetFromMap(new IdentityHashMap<>());
            seen.add(cur);
            while (cur instanceof Pair p) {
                if (!first) sb.append(" ");
                first = false;
                sb.append(schemeToString(p.car()));
                cur = p.cdr();
                if (cur instanceof Pair && seen.contains(cur)) {
                    sb.append(" . ...");
                    cur = NIL; // break cycle
                    break;
                }
                seen.add(cur);
            }
            if (cur != NIL) {
                sb.append(" . ");
                sb.append(schemeToString(cur));
            }
            sb.append(")");
            return sb.toString();
        }
        if (val instanceof SchemeVector v) {
            StringBuilder sb = new StringBuilder("#(");
            for (int i = 0; i < v.length(); i++) {
                if (i > 0) sb.append(" ");
                sb.append(schemeToString(v.ref(i)));
            }
            sb.append(")");
            return sb.toString();
        }
        if (val instanceof SchemeRecord sr) return "#<record:" + sr.typeName + ">";
        if (val instanceof Lambda) return "#<procedure>";
        if (val instanceof CaseLambda) return "#<procedure>";
        if (val instanceof Continuation) return "#<continuation>";
        if (val instanceof SyntaxRules) return "#<macro>";
        if (val instanceof SyntaxCaseTransformer) return "#<macro>";
        if (val instanceof SyntaxObject so) return schemeToString(so.datum());
        return val.toString();
    }
}
