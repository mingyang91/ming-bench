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
    private int currentLine = 1;
    private int currentCol = 1;
    StringBuilder outputBuffer = new StringBuilder();
    private int trampolineDepth = 0;

    public Evaluator() {
        for (String name : new String[]{"+", "-", "*", "/", "<", ">", "=", "<=", ">=", "not",
                "cons", "car", "cdr", "null?", "list", "length",
                "string?", "number?", "boolean?", "pair?", "symbol?", "append",
                "display", "write", "newline",
                "string-append", "string-length", "substring",
                "string->number", "number->string",
                "symbol->string", "string->symbol",
                "string-ref", "char?",
                "string-copy", "string-set!",
                "string->list", "list->string",
                "char->integer", "integer->char",
                "apply", "map", "for-each",
                "abs", "modulo", "remainder", "quotient", "min", "max", "expt",
                "zero?", "positive?", "negative?", "odd?", "even?",
                "list-ref", "list-tail", "list?", "assoc",
                "equal?", "eq?", "eqv?",
                "vector", "make-vector", "vector-ref", "vector-set!",
                "vector-length", "vector?", "vector->list", "list->vector",
                "char-alphabetic?", "char-numeric?", "char-upcase", "char-downcase",
                "char=?", "char<?",
                "string=?", "string<?", "string-ci=?",
                "string-upcase", "string-downcase",
                "exact?", "inexact?", "exact->inexact", "inexact->exact",
                "numerator", "denominator", "integer?", "rational?",
                "set-car!", "set-cdr!",
                "caar", "cadr", "cdar", "cddr",
                "caaar", "caadr", "cadar", "caddr",
                "cdaar", "cdadr", "cddar", "cdddr",
                "caaaar", "caaadr", "caadar", "caaddr",
                "cadaar", "cadadr", "caddar", "cadddr",
                "cdaaar", "cdaadr", "cdadar", "cdaddr",
                "cddaar", "cddadr", "cdddar", "cddddr",
                "assq", "assv", "memq", "memv", "member", "reverse",
                "gcd", "lcm", "truncate", "round",
                "make-string", "string",
                "string>?", "string<=?", "string>=?",
                "procedure?"}) {
            globalEnv.define(name, new BuiltinProc(name));
        }
        globalEnv.define("call/cc", new CallccProc());
        globalEnv.define("call-with-current-continuation", new CallccProc());
    }

    public String evalStr(String input) throws EvalError {
        List<Object> exprs = parse(input);
        if (exprs.isEmpty()) return "";
        Cont haltK = val -> new BounceVal(val);
        Object result = trampoline(evalBody(exprs, globalEnv, haltK));
        return schemeToString(result);
    }

    public EvalResult evalStrWithOutput(String input) throws EvalError {
        outputBuffer.setLength(0);
        List<Object> exprs = parse(input);
        if (exprs.isEmpty()) return new EvalResult("", "");
        Cont haltK = val -> new BounceVal(val);
        Object result = trampoline(evalBody(exprs, globalEnv, haltK));
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
        boolean mutable;
        SchemeString(String value) { this.value = value; this.mutable = false; }
        SchemeString(String value, boolean mutable) { this.value = value; this.mutable = mutable; }
    }

    static final class SchemeChar {
        final char value;
        SchemeChar(char value) { this.value = value; }
    }

    static final class SchemeRational {
        final long numer;
        final long denom;
        SchemeRational(long numer, long denom) { this.numer = numer; this.denom = denom; }
        double toDouble() { return (double) numer / denom; }
    }

    static long gcd(long a, long b) {
        a = Math.abs(a); b = Math.abs(b);
        while (b != 0) { long t = b; b = a % b; a = t; }
        return a;
    }

    static Object makeRational(long numer, long denom) {
        if (denom < 0) { numer = -numer; denom = -denom; }
        long g = gcd(numer, denom);
        numer /= g; denom /= g;
        if (denom == 1) return numer;
        return new SchemeRational(numer, denom);
    }

    static Object doubleToExact(double d) {
        if (d == Math.floor(d) && !Double.isInfinite(d)) return (long) d;
        long denom = 1;
        double val = d;
        while (val != Math.floor(val) && denom <= (1L << 53)) {
            val *= 2; denom *= 2;
        }
        return makeRational(Math.round(val), denom);
    }

    static final class Pair {
        Object car;
        Object cdr;
        Pair(Object car, Object cdr) { this.car = car; this.cdr = cdr; }
    }

    static final class SchemeVector {
        final Object[] data;
        SchemeVector(Object[] data) { this.data = data; }
    }

    static final class Located {
        final Object datum;
        final int line;
        final int col;
        Located(Object datum, int line, int col) {
            this.datum = datum; this.line = line; this.col = col;
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
        final String restParam;
        final List<Object> body;
        final Env closureEnv;
        Lambda(List<String> params, String restParam, List<Object> body, Env closureEnv) {
            this.params = params; this.restParam = restParam;
            this.body = body; this.closureEnv = closureEnv;
        }
    }

    static final class CaseLambda {
        final List<Lambda> clauses;
        CaseLambda(List<Lambda> clauses) { this.clauses = clauses; }
    }

    static final class RecordType {
        final String name;
        final List<String> fieldNames;
        RecordType(String name, List<String> fieldNames) { this.name = name; this.fieldNames = fieldNames; }
    }

    static final class SchemeRecord {
        final RecordType type;
        final Object[] fields;
        SchemeRecord(RecordType type, Object[] fields) { this.type = type; this.fields = fields; }
    }

    static final class RecordConstructor {
        final RecordType type;
        final List<String> fieldOrder;
        RecordConstructor(RecordType type, List<String> fieldOrder) { this.type = type; this.fieldOrder = fieldOrder; }
    }

    static final class RecordPredicate {
        final RecordType type;
        RecordPredicate(RecordType type) { this.type = type; }
    }

    static final class RecordAccessor {
        final RecordType type;
        final int fieldIndex;
        RecordAccessor(RecordType type, int fieldIndex) { this.type = type; this.fieldIndex = fieldIndex; }
    }

    static final class SyntaxRules {
        final List<String> literals;
        final List<Object[]> rules;
        final Env defEnv;
        SyntaxRules(List<String> literals, List<Object[]> rules, Env defEnv) {
            this.literals = literals; this.rules = rules; this.defEnv = defEnv;
        }
    }

    static final class ResolvedRef {
        final String name;
        final Env env;
        ResolvedRef(String name, Env env) { this.name = name; this.env = env; }
    }

    // ---- Dynamic wind ----
    static final class WindFrame {
        final Object inThunk;
        final Object outThunk;
        WindFrame(Object inThunk, Object outThunk) { this.inThunk = inThunk; this.outThunk = outThunk; }
    }

    final List<WindFrame> windStack = new ArrayList<>();

    // ---- Continuation / trampoline types ----
    static final class SchemeCont {
        final Cont k;
        final List<WindFrame> capturedWind;
        SchemeCont(Cont k, List<WindFrame> capturedWind) { this.k = k; this.capturedWind = capturedWind; }
    }

    static final class CallccProc {}

    static final class TailCall {
        final Object proc;
        final List<Object> args;
        TailCall(Object proc, List<Object> args) { this.proc = proc; this.args = args; }
    }

    @FunctionalInterface
    interface Cont {
        Object apply(Object value) throws EvalError;
    }

    static final class BounceStep {
        final Object expr;
        final Env env;
        final Cont k;
        BounceStep(Object expr, Env env, Cont k) { this.expr = expr; this.env = env; this.k = k; }
    }

    static final class BounceVal {
        final Object val;
        BounceVal(Object val) { this.val = val; }
    }

    static final class BounceApplyK {
        final Cont k;
        final Object value;
        BounceApplyK(Cont k, Object value) { this.k = k; this.value = value; }
    }

    static final class ContinuationInvoked extends RuntimeException {
        final Cont k;
        final Object value;
        ContinuationInvoked(Cont k, Object value) {
            super(null, null, true, false);
            this.k = k; this.value = value;
        }
    }

    private long gensymCounter = 0;
    private String gensym(String base) { return base + "__g" + (gensymCounter++); }

    // ---- Token ----
    private static final class Token {
        final String value;
        final int line;
        final int col;
        Token(String value, int line, int col) { this.value = value; this.line = line; this.col = col; }
    }

    // ---- Tokenizer ----
    private List<Token> tokenize(String input) {
        List<Token> tokens = new ArrayList<>();
        int i = 0, len = input.length(), line = 1, col = 1;
        while (i < len) {
            char c = input.charAt(i);
            if (c == '\n') { i++; line++; col = 1; continue; }
            if (Character.isWhitespace(c)) { i++; col++; continue; }
            if (c == ';') { while (i < len && input.charAt(i) != '\n') { i++; col++; } continue; }
            int startLine = line, startCol = col;
            if (c == '#' && i + 1 < len && input.charAt(i + 1) == '(') {
                tokens.add(new Token("#(", startLine, startCol)); i += 2; col += 2; continue;
            }
            if (c == '(') { tokens.add(new Token("(", startLine, startCol)); i++; col++; continue; }
            if (c == ')') { tokens.add(new Token(")", startLine, startCol)); i++; col++; continue; }
            if (c == '\'') { tokens.add(new Token("'", startLine, startCol)); i++; col++; continue; }
            if (c == '"') {
                StringBuilder sb = new StringBuilder();
                sb.append('"'); i++; col++;
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
                sb.append(ch); i++; col++;
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
        while (pos < tokens.size()) exprs.add(parseExpr(tokens));
        return exprs;
    }

    private Object parseExpr(List<Token> tokens) throws EvalError {
        if (pos >= tokens.size()) throw new EvalError("unexpected end of input");
        Token token = tokens.get(pos++);
        if (token.value.equals("#(")) {
            List<Object> elems = new ArrayList<>();
            while (pos < tokens.size() && !tokens.get(pos).value.equals(")")) elems.add(parseExpr(tokens));
            if (pos >= tokens.size()) throw new EvalError("missing closing parenthesis");
            pos++;
            List<Object> list = new ArrayList<>();
            list.add("vector");
            list.addAll(elems);
            return new Located(list, token.line, token.col);
        }
        if (token.value.equals("(")) {
            List<Object> list = new ArrayList<>();
            while (pos < tokens.size() && !tokens.get(pos).value.equals(")")) list.add(parseExpr(tokens));
            if (pos >= tokens.size()) throw new EvalError("missing closing parenthesis");
            pos++;
            return new Located(list, token.line, token.col);
        }
        if (token.value.equals(")")) throw new EvalError("unexpected )");
        if (token.value.equals("'")) {
            Object quoted = parseExpr(tokens);
            List<Object> q = new ArrayList<>();
            q.add("quote"); q.add(quoted);
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
        int slashIdx = token.indexOf('/');
        if (slashIdx > 0 && slashIdx < token.length() - 1) {
            try {
                long numer = Long.parseLong(token.substring(0, slashIdx));
                long denom = Long.parseLong(token.substring(slashIdx + 1));
                if (denom != 0) return makeRational(numer, denom);
            } catch (NumberFormatException ignored) {}
        }
        try { return Double.parseDouble(token); } catch (NumberFormatException ignored) {}
        return token;
    }

    private Object unwrap(Object expr) {
        if (expr instanceof Located loc) return loc.datum;
        return expr;
    }

    // ---- Trampoline ----
    private Object trampoline(Object bounce) throws EvalError {
        trampolineDepth++;
        try {
            while (true) {
                try {
                    if (bounce instanceof BounceStep s) {
                        bounce = evalStep(s.expr, s.env, s.k);
                    } else if (bounce instanceof BounceApplyK ak) {
                        bounce = ak.k.apply(ak.value);
                    } else {
                        return ((BounceVal) bounce).val;
                    }
                } catch (ContinuationInvoked ci) {
                    bounce = new BounceApplyK(ci.k, ci.value);
                }
            }
        } finally {
            trampolineDepth--;
        }
    }

    // ---- CEK Evaluator ----
    @SuppressWarnings("unchecked")
    private Object evalStep(Object expr, Env env, Cont k) throws EvalError {
        if (expr instanceof Located loc) {
            currentLine = loc.line;
            currentCol = loc.col;
            expr = loc.datum;
        }

        if (expr instanceof Long || expr instanceof Double || expr instanceof Boolean
            || expr instanceof SchemeString || expr instanceof SchemeChar
            || expr instanceof SchemeRational) {
            return new BounceApplyK(k, expr);
        }
        if (expr instanceof ResolvedRef ref) {
            return new BounceApplyK(k, ref.env.lookup(ref.name));
        }
        if (expr instanceof String sym) {
            try { return new BounceApplyK(k, env.lookup(sym)); }
            catch (EvalError e) { throw posError("unbound variable: " + sym); }
        }
        if (expr instanceof List<?> list) {
            if (list.isEmpty()) throw posError("empty application");
            Object rawHead = unwrap(list.get(0));

            if (rawHead instanceof String op) {
                switch (op) {
                    case "quote": {
                        if (list.size() != 2) throw posError("quote: expected 1 argument");
                        return new BounceApplyK(k, quoteDatum(list.get(1)));
                    }
                    case "if": {
                        if (list.size() < 3 || list.size() > 4) throw posError("if: expected 2 or 3 arguments");
                        Object thenE = list.get(2);
                        Object elseE = list.size() > 3 ? list.get(3) : null;
                        Env ifEnv = env;
                        return new BounceStep(list.get(1), env, testVal -> {
                            if (!isFalse(testVal)) return new BounceStep(thenE, ifEnv, k);
                            else if (elseE != null) return new BounceStep(elseE, ifEnv, k);
                            else return new BounceApplyK(k, null);
                        });
                    }
                    case "define": {
                        if (list.size() < 3) throw posError("define: bad syntax");
                        Object target = unwrap(list.get(1));
                        if (target instanceof String name) {
                            Env defEnv = env;
                            return new BounceStep(list.get(2), env, defVal -> {
                                defEnv.define(name, defVal);
                                return new BounceApplyK(k, null);
                            });
                        }
                        if (target instanceof List<?> sig) {
                            if (sig.isEmpty()) throw posError("define: bad syntax");
                            String name = (String) unwrap(sig.get(0));
                            ParamSpec ps = parseParamList(sig.subList(1, sig.size()));
                            List<Object> body = new ArrayList<>();
                            for (int i = 2; i < list.size(); i++) body.add(list.get(i));
                            env.define(name, new Lambda(ps.params, ps.restParam, body, env));
                            return new BounceApplyK(k, null);
                        }
                        throw posError("define: bad syntax");
                    }
                    case "set!": {
                        if (list.size() != 3) throw posError("set!: bad syntax");
                        Object varName = unwrap(list.get(1));
                        if (!(varName instanceof String name)) throw posError("set!: expected symbol");
                        Env setEnv = env;
                        return new BounceStep(list.get(2), env, setVal -> {
                            try { setEnv.set(name, setVal); }
                            catch (EvalError e) { throw posError("set!: unbound variable: " + name); }
                            return new BounceApplyK(k, null);
                        });
                    }
                    case "lambda": {
                        return new BounceApplyK(k, evalLambda(list, env));
                    }
                    case "case-lambda": {
                        return new BounceApplyK(k, evalCaseLambda(list, env));
                    }
                    case "begin": {
                        if (list.size() == 1) return new BounceApplyK(k, null);
                        List<Object> body = new ArrayList<>();
                        for (int i = 1; i < list.size(); i++) body.add(list.get(i));
                        return evalBody(body, env, k);
                    }
                    case "and": {
                        if (list.size() == 1) return new BounceApplyK(k, Boolean.TRUE);
                        if (list.size() == 2) return new BounceStep(list.get(1), env, k);
                        return evalAnd(list, 1, env, k);
                    }
                    case "or": {
                        if (list.size() == 1) return new BounceApplyK(k, Boolean.FALSE);
                        if (list.size() == 2) return new BounceStep(list.get(1), env, k);
                        return evalOr(list, 1, env, k);
                    }
                    case "let": {
                        if (list.size() < 3) throw posError("let: bad syntax");
                        Object second = unwrap(list.get(1));
                        if (second instanceof String namedName) {
                            if (list.size() < 4) throw posError("let: bad syntax");
                            List<?> bindings = (List<?>) unwrap(list.get(2));
                            List<String> params = new ArrayList<>();
                            List<Object> inits = new ArrayList<>();
                            for (Object b : bindings) {
                                List<?> binding = (List<?>) unwrap(b);
                                params.add((String) unwrap(binding.get(0)));
                                inits.add(binding.get(1));
                            }
                            List<Object> body = new ArrayList<>();
                            for (int i = 3; i < list.size(); i++) body.add(list.get(i));
                            return evalNamedLet(namedName, params, inits, 0, new ArrayList<>(), env, body, k);
                        }
                        List<?> bindings = (List<?>) second;
                        List<Object> body = new ArrayList<>();
                        for (int i = 2; i < list.size(); i++) body.add(list.get(i));
                        if (bindings.isEmpty()) return evalBody(body, new Env(env), k);
                        return evalLetInits(bindings, 0, new ArrayList<>(), env, body, k);
                    }
                    case "let*": {
                        if (list.size() < 3) throw posError("let*: bad syntax");
                        List<?> bindings = (List<?>) unwrap(list.get(1));
                        List<Object> body = new ArrayList<>();
                        for (int i = 2; i < list.size(); i++) body.add(list.get(i));
                        Env letEnv = new Env(env);
                        if (bindings.isEmpty()) return evalBody(body, letEnv, k);
                        return evalLetStarInits(bindings, 0, letEnv, body, k);
                    }
                    case "letrec", "letrec*": {
                        if (list.size() < 3) throw posError(op + ": bad syntax");
                        List<?> bindings = (List<?>) unwrap(list.get(1));
                        List<Object> body = new ArrayList<>();
                        for (int i = 2; i < list.size(); i++) body.add(list.get(i));
                        Env letrecEnv = new Env(env);
                        for (Object b : bindings) {
                            List<?> binding = (List<?>) unwrap(b);
                            letrecEnv.define((String) unwrap(binding.get(0)), null);
                        }
                        if (bindings.isEmpty()) return evalBody(body, letrecEnv, k);
                        return evalLetrecInits(bindings, 0, letrecEnv, body, k);
                    }
                    case "cond": {
                        return evalCond(list, 1, env, k);
                    }
                    case "case": {
                        if (list.size() < 2) throw posError("case: bad syntax");
                        List<?> caseForm = list;
                        Env caseEnv = env;
                        return new BounceStep(list.get(1), env, keyVal -> evalCaseClauses(caseForm, 2, keyVal, caseEnv, k));
                    }
                    case "do": {
                        return evalDoStart(list, env, k);
                    }
                    case "define-syntax": {
                        if (list.size() != 3) throw posError("define-syntax: bad syntax");
                        String name = (String) unwrap(list.get(1));
                        SyntaxRules transformer = evalSyntaxRules(list.get(2), env);
                        env.define(name, transformer);
                        return new BounceApplyK(k, null);
                    }
                    case "define-record-type": {
                        Object result = evalDefineRecordType(list, env);
                        return new BounceApplyK(k, result);
                    }
                    case "call/cc", "call-with-current-continuation": {
                        Env ccEnv = env;
                        return new BounceStep(list.get(1), env, procVal -> applyProc(procVal, List.of(new SchemeCont(k, new ArrayList<>(windStack))), k));
                    }
                    case "dynamic-wind": {
                        if (list.size() != 4) throw posError("dynamic-wind: expected 3 arguments");
                        Env dwEnv = env;
                        return new BounceStep(list.get(1), env, inThunk ->
                            new BounceStep(list.get(2), dwEnv, bodyThunk ->
                                new BounceStep(list.get(3), dwEnv, outThunk -> {
                                    WindFrame frame = new WindFrame(inThunk, outThunk);
                                    return applyProc(inThunk, List.of(), ignored1 -> {
                                        windStack.add(frame);
                                        return applyProc(bodyThunk, List.of(), bodyVal -> {
                                            windStack.remove(windStack.size() - 1);
                                            return applyProc(outThunk, List.of(), ignored2 ->
                                                k.apply(bodyVal));
                                        });
                                    });
                                })));
                    }
                }
                // Check for macros
                try {
                    Object maybeMacro = env.lookup(op);
                    if (maybeMacro instanceof SyntaxRules sr) {
                        return new BounceStep(expandMacro(sr, (List<Object>) list), env, k);
                    }
                } catch (EvalError ignored) {}
            }

            // ResolvedRef head
            if (rawHead instanceof ResolvedRef ref) {
                Object resolved = ref.env.lookup(ref.name);
                if (resolved instanceof SyntaxRules sr) {
                    return new BounceStep(expandMacro(sr, (List<Object>) list), env, k);
                }
                return evalArgsAndApply(resolved, list, 1, env, k);
            }

            // General application: evaluate head then args
            Env appEnv = env;
            int callLine = currentLine, callCol = currentCol;
            return new BounceStep(list.get(0), env, procVal -> {
                currentLine = callLine; currentCol = callCol;
                return evalArgsAndApply(procVal, list, 1, appEnv, k);
            });
        }
        throw posError("unknown expression type");
    }

    // ---- Eval helpers ----

    private Object evalBody(List<?> body, Env env, Cont k) throws EvalError {
        if (body.isEmpty()) return new BounceApplyK(k, null);
        if (body.size() == 1) return new BounceStep(body.get(0), env, k);
        return new BounceStep(body.get(0), env, makeSeqCont(body, 1, env, k));
    }

    private Cont makeSeqCont(List<?> exprs, int nextIdx, Env env, Cont k) {
        return ignored -> {
            if (nextIdx == exprs.size() - 1) return new BounceStep(exprs.get(nextIdx), env, k);
            return new BounceStep(exprs.get(nextIdx), env, makeSeqCont(exprs, nextIdx + 1, env, k));
        };
    }

    private Object evalAnd(List<?> form, int idx, Env env, Cont k) throws EvalError {
        if (idx == form.size() - 1) return new BounceStep(form.get(idx), env, k);
        return new BounceStep(form.get(idx), env, andVal -> {
            if (isFalse(andVal)) return new BounceApplyK(k, andVal);
            return evalAnd(form, idx + 1, env, k);
        });
    }

    private Object evalOr(List<?> form, int idx, Env env, Cont k) throws EvalError {
        if (idx == form.size() - 1) return new BounceStep(form.get(idx), env, k);
        return new BounceStep(form.get(idx), env, orVal -> {
            if (!isFalse(orVal)) return new BounceApplyK(k, orVal);
            return evalOr(form, idx + 1, env, k);
        });
    }

    private Object evalLetInits(List<?> bindings, int idx, List<Object> vals,
                                 Env origEnv, List<Object> body, Cont k) throws EvalError {
        if (idx >= bindings.size()) {
            Env letEnv = new Env(origEnv);
            for (int i = 0; i < bindings.size(); i++) {
                List<?> b = (List<?>) unwrap(bindings.get(i));
                letEnv.define((String) unwrap(b.get(0)), vals.get(i));
            }
            return evalBody(body, letEnv, k);
        }
        List<?> binding = (List<?>) unwrap(bindings.get(idx));
        return new BounceStep(binding.get(1), origEnv, initVal -> {
            List<Object> newVals = new ArrayList<>(vals);
            newVals.add(initVal);
            return evalLetInits(bindings, idx + 1, newVals, origEnv, body, k);
        });
    }

    private Object evalLetStarInits(List<?> bindings, int idx, Env letEnv,
                                     List<Object> body, Cont k) throws EvalError {
        if (idx >= bindings.size()) return evalBody(body, letEnv, k);
        List<?> binding = (List<?>) unwrap(bindings.get(idx));
        String name = (String) unwrap(binding.get(0));
        return new BounceStep(binding.get(1), letEnv, initVal -> {
            letEnv.define(name, initVal);
            return evalLetStarInits(bindings, idx + 1, letEnv, body, k);
        });
    }

    private Object evalLetrecInits(List<?> bindings, int idx, Env letrecEnv,
                                    List<Object> body, Cont k) throws EvalError {
        if (idx >= bindings.size()) return evalBody(body, letrecEnv, k);
        List<?> binding = (List<?>) unwrap(bindings.get(idx));
        String name = (String) unwrap(binding.get(0));
        return new BounceStep(binding.get(1), letrecEnv, initVal -> {
            letrecEnv.set(name, initVal);
            return evalLetrecInits(bindings, idx + 1, letrecEnv, body, k);
        });
    }

    private Object evalNamedLet(String name, List<String> params, List<Object> inits,
                                 int idx, List<Object> vals, Env outerEnv,
                                 List<Object> body, Cont k) throws EvalError {
        if (idx >= inits.size()) {
            Env letEnv = new Env(outerEnv);
            Lambda loopLam = new Lambda(params, null, body, letEnv);
            letEnv.define(name, loopLam);
            Env callEnv = new Env(letEnv);
            for (int i = 0; i < params.size(); i++) callEnv.define(params.get(i), vals.get(i));
            return evalBody(body, callEnv, k);
        }
        return new BounceStep(inits.get(idx), outerEnv, initVal -> {
            List<Object> newVals = new ArrayList<>(vals);
            newVals.add(initVal);
            return evalNamedLet(name, params, inits, idx + 1, newVals, outerEnv, body, k);
        });
    }

    private Object evalCond(List<?> form, int clauseIdx, Env env, Cont k) throws EvalError {
        if (clauseIdx >= form.size()) return new BounceApplyK(k, null);
        List<?> clause = (List<?>) unwrap(form.get(clauseIdx));
        Object head = unwrap(clause.get(0));
        if (head instanceof String s && s.equals("else")) {
            return evalClauseBody(clause, 1, env, k);
        }
        return new BounceStep(clause.get(0), env, testVal -> {
            if (!isFalse(testVal)) {
                if (clause.size() == 1) return new BounceApplyK(k, testVal);
                return evalClauseBody(clause, 1, env, k);
            }
            return evalCond(form, clauseIdx + 1, env, k);
        });
    }

    private Object evalClauseBody(List<?> clause, int startIdx, Env env, Cont k) throws EvalError {
        if (startIdx >= clause.size()) return new BounceApplyK(k, null);
        List<Object> body = new ArrayList<>();
        for (int i = startIdx; i < clause.size(); i++) body.add(clause.get(i));
        return evalBody(body, env, k);
    }

    private Object evalCaseClauses(List<?> form, int idx, Object keyVal, Env env, Cont k) throws EvalError {
        if (idx >= form.size()) return new BounceApplyK(k, null);
        List<?> clause = (List<?>) unwrap(form.get(idx));
        Object datumHead = unwrap(clause.get(0));
        if (datumHead instanceof String s && s.equals("else")) {
            return evalClauseBody(clause, 1, env, k);
        }
        List<?> datums = (List<?>) datumHead;
        for (Object d : datums) {
            if (schemeEqv(keyVal, quoteDatum(d))) {
                return evalClauseBody(clause, 1, env, k);
            }
        }
        return evalCaseClauses(form, idx + 1, keyVal, env, k);
    }

    private Object evalDoStart(List<?> list, Env env, Cont k) throws EvalError {
        if (list.size() < 3) throw posError("do: bad syntax");
        List<?> varSpecs = (List<?>) unwrap(list.get(1));
        int nVars = varSpecs.size();
        String[] varNames = new String[nVars];
        Object[] initExprs = new Object[nVars];
        Object[] stepExprs = new Object[nVars];
        for (int i = 0; i < nVars; i++) {
            List<?> spec = (List<?>) unwrap(varSpecs.get(i));
            varNames[i] = (String) unwrap(spec.get(0));
            initExprs[i] = spec.get(1);
            stepExprs[i] = spec.size() > 2 ? spec.get(2) : null;
        }
        List<?> testClause = (List<?>) unwrap(list.get(2));
        if (nVars == 0) {
            Env doEnv = new Env(env);
            return doLoop(varNames, stepExprs, doEnv, testClause, list, k);
        }
        return evalDoInits(varNames, initExprs, stepExprs, 0, new Object[nVars], env, testClause, list, k);
    }

    private Object evalDoInits(String[] varNames, Object[] initExprs, Object[] stepExprs,
                                int idx, Object[] vals, Env outerEnv,
                                List<?> testClause, List<?> fullForm, Cont k) throws EvalError {
        if (idx >= varNames.length) {
            Env doEnv = new Env(outerEnv);
            for (int i = 0; i < varNames.length; i++) doEnv.define(varNames[i], vals[i]);
            return doLoop(varNames, stepExprs, doEnv, testClause, fullForm, k);
        }
        return new BounceStep(initExprs[idx], outerEnv, initVal -> {
            Object[] newVals = vals.clone();
            newVals[idx] = initVal;
            return evalDoInits(varNames, initExprs, stepExprs, idx + 1, newVals, outerEnv, testClause, fullForm, k);
        });
    }

    private Object doLoop(String[] varNames, Object[] stepExprs, Env doEnv,
                           List<?> testClause, List<?> fullForm, Cont k) throws EvalError {
        return new BounceStep(testClause.get(0), doEnv, testVal -> {
            if (!isFalse(testVal)) {
                if (testClause.size() <= 1) return new BounceApplyK(k, null);
                List<Object> exitBody = new ArrayList<>();
                for (int j = 1; j < testClause.size(); j++) exitBody.add(testClause.get(j));
                return evalBody(exitBody, doEnv, k);
            }
            // Body commands then steps
            Cont afterBody = ignored -> evalDoSteps(varNames, stepExprs, 0, new Object[varNames.length], doEnv, testClause, fullForm, k);
            int bodyStart = 3;
            if (bodyStart >= fullForm.size()) return afterBody.apply(null);
            List<Object> bodyExprs = new ArrayList<>();
            for (int j = bodyStart; j < fullForm.size(); j++) bodyExprs.add(fullForm.get(j));
            // Evaluate body, all non-tail (results discarded), then afterBody
            return evalBodyForEffect(bodyExprs, 0, doEnv, afterBody);
        });
    }

    private Object evalBodyForEffect(List<Object> exprs, int idx, Env env, Cont afterK) throws EvalError {
        if (idx >= exprs.size()) return afterK.apply(null);
        return new BounceStep(exprs.get(idx), env, ignored -> evalBodyForEffect(exprs, idx + 1, env, afterK));
    }

    private Object evalDoSteps(String[] varNames, Object[] stepExprs, int idx, Object[] newVals,
                                Env doEnv, List<?> testClause, List<?> fullForm, Cont k) throws EvalError {
        // Fill in values for vars without step exprs, evaluate step exprs for others
        while (idx < varNames.length && stepExprs[idx] == null) {
            newVals[idx] = doEnv.lookup(varNames[idx]);
            idx++;
        }
        if (idx >= varNames.length) {
            for (int i = 0; i < varNames.length; i++) doEnv.set(varNames[i], newVals[i]);
            return doLoop(varNames, stepExprs, doEnv, testClause, fullForm, k);
        }
        int curIdx = idx;
        return new BounceStep(stepExprs[idx], doEnv, stepVal -> {
            Object[] nv = newVals.clone();
            nv[curIdx] = stepVal;
            return evalDoSteps(varNames, stepExprs, curIdx + 1, nv, doEnv, testClause, fullForm, k);
        });
    }

    // ---- Application helpers ----

    private Object evalArgsAndApply(Object proc, List<?> form, int startIdx,
                                     Env env, Cont k) throws EvalError {
        int nArgs = form.size() - startIdx;
        if (nArgs == 0) return applyProc(proc, new ArrayList<>(), k);
        // Evaluate arguments right-to-left so continuations re-evaluate earlier args
        Object[] results = new Object[nArgs];
        return evalArgRtoL(proc, form, startIdx, form.size() - 1, results, env, k);
    }

    private Object evalArgRtoL(Object proc, List<?> form, int startIdx, int curIdx,
                                Object[] results, Env env, Cont k) throws EvalError {
        if (curIdx < startIdx) {
            List<Object> args = new ArrayList<>(results.length);
            for (Object r : results) args.add(r);
            return applyProc(proc, args, k);
        }
        return new BounceStep(form.get(curIdx), env, argVal -> {
            Object[] nr = results.clone();
            nr[curIdx - startIdx] = argVal;
            return evalArgRtoL(proc, form, startIdx, curIdx - 1, nr, env, k);
        });
    }

    private Object applyProc(Object proc, List<Object> args, Cont k) throws EvalError {
        while (true) {
            if (proc instanceof Lambda lam) {
                Env callEnv = applyLambdaEnv(lam, args);
                return evalBody(lam.body, callEnv, k);
            }
            if (proc instanceof CaseLambda cl) {
                Lambda matched = null;
                for (Lambda lam : cl.clauses) {
                    if (lam.restParam != null) {
                        if (args.size() >= lam.params.size()) { matched = lam; break; }
                    } else {
                        if (args.size() == lam.params.size()) { matched = lam; break; }
                    }
                }
                if (matched == null) throw posError("no matching clause for " + args.size() + " arguments");
                proc = matched; continue;
            }
            if (proc instanceof BuiltinProc bp) {
                try {
                    Object result = builtins.apply(bp.name, args);
                    if (result instanceof TailCall tc) { proc = tc.proc; args = tc.args; continue; }
                    return new BounceApplyK(k, result);
                } catch (ContinuationInvoked ci) {
                    return new BounceApplyK(ci.k, ci.value);
                }
            }
            if (proc instanceof SchemeCont sc) {
                Object value = args.isEmpty() ? null : args.get(0);
                return performWindTransition(sc.capturedWind, value, v -> {
                    if (trampolineDepth > 1) throw new ContinuationInvoked(sc.k, v);
                    return new BounceApplyK(sc.k, v);
                });
            }
            if (proc instanceof CallccProc) {
                Object theProc = args.get(0);
                SchemeCont captured = new SchemeCont(k, new ArrayList<>(windStack));
                args = List.of(captured);
                proc = theProc;
                continue;
            }
            if (proc instanceof RecordConstructor rc) {
                if (args.size() != rc.fieldOrder.size())
                    throw posError("wrong number of arguments to constructor: expected " + rc.fieldOrder.size() + ", got " + args.size());
                return new BounceApplyK(k, new SchemeRecord(rc.type, args.toArray()));
            }
            if (proc instanceof RecordPredicate rp) {
                if (args.size() != 1) throw posError("wrong number of arguments to predicate");
                return new BounceApplyK(k, args.get(0) instanceof SchemeRecord sr && sr.type == rp.type);
            }
            if (proc instanceof RecordAccessor ra) {
                if (args.size() != 1) throw posError("wrong number of arguments to accessor");
                Object arg = args.get(0);
                if (!(arg instanceof SchemeRecord sr) || sr.type != ra.type) throw posError("accessor applied to wrong type");
                return new BounceApplyK(k, sr.fields[ra.fieldIndex]);
            }
            throw posError("not a procedure: " + schemeToString(proc));
        }
    }

    // ---- Wind transition for dynamic-wind + call/cc ----

    private Object performWindTransition(List<WindFrame> target, Object value, Cont finalK) throws EvalError {
        int common = 0;
        int maxCommon = Math.min(windStack.size(), target.size());
        while (common < maxCommon && windStack.get(common) == target.get(common)) {
            common++;
        }
        return unwindTo(common, target, value, finalK);
    }

    private Object unwindTo(int targetSize, List<WindFrame> target, Object value, Cont finalK) throws EvalError {
        if (windStack.size() <= targetSize) {
            return rewindFrom(target, windStack.size(), value, finalK);
        }
        WindFrame frame = windStack.remove(windStack.size() - 1);
        return applyProc(frame.outThunk, List.of(), ignored -> unwindTo(targetSize, target, value, finalK));
    }

    private Object rewindFrom(List<WindFrame> target, int idx, Object value, Cont finalK) throws EvalError {
        if (idx >= target.size()) {
            return finalK.apply(value);
        }
        WindFrame frame = target.get(idx);
        windStack.add(frame);
        return applyProc(frame.inThunk, List.of(), ignored -> rewindFrom(target, idx + 1, value, finalK));
    }

    // Synchronous apply for builtins (map/for-each callbacks)
    Object apply(Object proc, List<Object> args) throws EvalError {
        while (true) {
            if (proc instanceof Lambda lam) {
                Env callEnv = applyLambdaEnv(lam, args);
                Cont haltK = val -> new BounceVal(val);
                return trampoline(evalBody(lam.body, callEnv, haltK));
            }
            if (proc instanceof CaseLambda cl) {
                Lambda matched = null;
                for (Lambda lam : cl.clauses) {
                    if (lam.restParam != null) {
                        if (args.size() >= lam.params.size()) { matched = lam; break; }
                    } else {
                        if (args.size() == lam.params.size()) { matched = lam; break; }
                    }
                }
                if (matched == null) throw posError("no matching clause for " + args.size() + " arguments");
                proc = matched; continue;
            }
            if (proc instanceof BuiltinProc bp) {
                Object result = builtins.apply(bp.name, args);
                if (result instanceof TailCall tc) { proc = tc.proc; args = tc.args; continue; }
                return result;
            }
            if (proc instanceof SchemeCont sc) {
                Object value = args.isEmpty() ? null : args.get(0);
                throw new ContinuationInvoked(sc.k, value);
            }
            if (proc instanceof CallccProc) {
                // Limited call/cc in sync context: only escape continuations work
                Object theProc = args.get(0);
                Cont haltK = val -> new BounceVal(val);
                SchemeCont captured = new SchemeCont(haltK, new ArrayList<>(windStack));
                proc = theProc;
                args = List.of(captured);
                continue;
            }
            if (proc instanceof RecordConstructor rc) {
                if (args.size() != rc.fieldOrder.size()) throw posError("wrong arity");
                return new SchemeRecord(rc.type, args.toArray());
            }
            if (proc instanceof RecordPredicate rp) {
                if (args.size() != 1) throw posError("wrong arity");
                return (args.get(0) instanceof SchemeRecord sr && sr.type == rp.type);
            }
            if (proc instanceof RecordAccessor ra) {
                if (args.size() != 1) throw posError("wrong arity");
                Object arg = args.get(0);
                if (!(arg instanceof SchemeRecord sr) || sr.type != ra.type) throw posError("wrong type");
                return sr.fields[ra.fieldIndex];
            }
            throw posError("not a procedure: " + schemeToString(proc));
        }
    }

    // ---- Lambda helpers ----

    private record ParamSpec(List<String> params, String restParam) {}

    private ParamSpec parseParamList(Object paramListRaw) {
        List<String> params = new ArrayList<>();
        String restParam = null;
        if (paramListRaw instanceof List<?> paramList) {
            for (int i = 0; i < paramList.size(); i++) {
                String p = (String) unwrap(paramList.get(i));
                if (p.equals(".")) {
                    if (i + 1 < paramList.size()) restParam = (String) unwrap(paramList.get(i + 1));
                    break;
                }
                params.add(p);
            }
        } else if (paramListRaw instanceof String sym) {
            restParam = sym;
        }
        return new ParamSpec(params, restParam);
    }

    private Lambda evalLambda(List<?> list, Env env) throws EvalError {
        if (list.size() < 3) throw posError("lambda: bad syntax");
        ParamSpec ps = parseParamList(unwrap(list.get(1)));
        List<Object> body = new ArrayList<>();
        for (int i = 2; i < list.size(); i++) body.add(list.get(i));
        return new Lambda(ps.params, ps.restParam, body, env);
    }

    private CaseLambda evalCaseLambda(List<?> list, Env env) throws EvalError {
        if (list.size() < 2) throw posError("case-lambda: bad syntax");
        List<Lambda> clauses = new ArrayList<>();
        for (int ci = 1; ci < list.size(); ci++) {
            Object clauseRaw = unwrap(list.get(ci));
            if (!(clauseRaw instanceof List<?> clause) || clause.size() < 2)
                throw posError("case-lambda: bad clause");
            ParamSpec ps = parseParamList(unwrap(clause.get(0)));
            List<Object> body = new ArrayList<>();
            for (int i = 1; i < clause.size(); i++) body.add(clause.get(i));
            clauses.add(new Lambda(ps.params, ps.restParam, body, env));
        }
        return new CaseLambda(clauses);
    }

    private Env applyLambdaEnv(Lambda lam, List<Object> args) throws EvalError {
        if (lam.restParam != null) {
            if (args.size() < lam.params.size())
                throw posError("wrong number of arguments: expected at least " + lam.params.size() + ", got " + args.size());
        } else {
            if (args.size() != lam.params.size())
                throw posError("wrong number of arguments: expected " + lam.params.size() + ", got " + args.size());
        }
        Env callEnv = new Env(lam.closureEnv);
        for (int i = 0; i < lam.params.size(); i++) callEnv.define(lam.params.get(i), args.get(i));
        if (lam.restParam != null) {
            Object rest = NIL;
            for (int i = args.size() - 1; i >= lam.params.size(); i--) rest = new Pair(args.get(i), rest);
            callEnv.define(lam.restParam, rest);
        }
        return callEnv;
    }

    private Object evalDefineRecordType(List<?> list, Env env) throws EvalError {
        if (list.size() < 4) throw posError("define-record-type: bad syntax");
        String typeName = (String) unwrap(list.get(1));
        List<?> ctorSpec = (List<?>) unwrap(list.get(2));
        String ctorName = (String) unwrap(ctorSpec.get(0));
        List<String> ctorFields = new ArrayList<>();
        for (int i = 1; i < ctorSpec.size(); i++) ctorFields.add((String) unwrap(ctorSpec.get(i)));
        String predName = (String) unwrap(list.get(3));
        List<String> allFieldNames = new ArrayList<>(ctorFields);
        Map<String, String> fieldAccessors = new HashMap<>();
        for (int i = 4; i < list.size(); i++) {
            List<?> fieldSpec = (List<?>) unwrap(list.get(i));
            String fieldName = (String) unwrap(fieldSpec.get(0));
            String accessorName = (String) unwrap(fieldSpec.get(1));
            fieldAccessors.put(fieldName, accessorName);
        }
        RecordType rt = new RecordType(typeName, allFieldNames);
        env.define(ctorName, new RecordConstructor(rt, ctorFields));
        env.define(predName, new RecordPredicate(rt));
        for (int i = 0; i < allFieldNames.size(); i++) {
            String fn = allFieldNames.get(i);
            String accName = fieldAccessors.get(fn);
            if (accName != null) env.define(accName, new RecordAccessor(rt, i));
        }
        return null;
    }

    private Object quoteDatum(Object datum) {
        datum = unwrap(datum);
        if (datum instanceof List<?> list) {
            if (list.isEmpty()) return NIL;
            Object result = NIL;
            for (int i = list.size() - 1; i >= 0; i--) result = new Pair(quoteDatum(list.get(i)), result);
            return result;
        }
        return datum;
    }

    private boolean isFalse(Object val) {
        return val instanceof Boolean b && !b;
    }

    boolean schemeEqual(Object a, Object b) {
        return schemeEqualImpl(a, b, new java.util.IdentityHashMap<>());
    }

    private boolean schemeEqualImpl(Object a, Object b, java.util.IdentityHashMap<Object, Set<Object>> seen) {
        if (a == b) return true;
        if (isNumber(a) && isNumber(b)) return numToDouble(a) == numToDouble(b);
        if (a instanceof Boolean && b instanceof Boolean) return a.equals(b);
        if (a instanceof String && b instanceof String) return a.equals(b);
        if (a instanceof SchemeString sa && b instanceof SchemeString sb) return sa.value.equals(sb.value);
        if (a instanceof SchemeChar ca && b instanceof SchemeChar cb) return ca.value == cb.value;
        if (a == NIL && b == NIL) return true;
        if (a instanceof Pair pa && b instanceof Pair pb) {
            Set<Object> aSet = seen.get(a);
            if (aSet != null && aSet.contains(b)) return true;
            if (aSet == null) { aSet = java.util.Collections.newSetFromMap(new java.util.IdentityHashMap<>()); seen.put(a, aSet); }
            aSet.add(b);
            return schemeEqualImpl(pa.car, pb.car, seen) && schemeEqualImpl(pa.cdr, pb.cdr, seen);
        }
        if (a instanceof SchemeVector va && b instanceof SchemeVector vb) {
            if (va.data.length != vb.data.length) return false;
            Set<Object> aSet = seen.get(a);
            if (aSet != null && aSet.contains(b)) return true;
            if (aSet == null) { aSet = java.util.Collections.newSetFromMap(new java.util.IdentityHashMap<>()); seen.put(a, aSet); }
            aSet.add(b);
            for (int i = 0; i < va.data.length; i++) {
                if (!schemeEqualImpl(va.data[i], vb.data[i], seen)) return false;
            }
            return true;
        }
        return false;
    }

    boolean schemeEqv(Object a, Object b) {
        if (a == b) return true;
        if (isNumber(a) && isNumber(b)) return numToDouble(a) == numToDouble(b);
        if (a instanceof Boolean && b instanceof Boolean) return a.equals(b);
        if (a instanceof String && b instanceof String) return a.equals(b);
        if (a instanceof SchemeChar ca && b instanceof SchemeChar cb) return ca.value == cb.value;
        return false;
    }

    boolean isNumber(Object val) {
        return val instanceof Long || val instanceof Double || val instanceof SchemeRational;
    }

    double numToDouble(Object val) {
        if (val instanceof Long l) return l.doubleValue();
        if (val instanceof Double d) return d;
        if (val instanceof SchemeRational r) return r.toDouble();
        return 0;
    }

    // ---- Output formatting ----

    String displayString(Object val) {
        if (val instanceof SchemeString s) return s.value;
        if (val instanceof SchemeChar c) return String.valueOf(c.value);
        return schemeToStringImpl(val, new java.util.IdentityHashMap<>());
    }

    @SuppressWarnings("unchecked")
    String schemeToString(Object val) {
        return schemeToStringImpl(val, new java.util.IdentityHashMap<>());
    }

    private String schemeToStringImpl(Object val, java.util.IdentityHashMap<Object, Boolean> seen) {
        if (val == null) return "";
        if (val == NIL) return "()";
        if (val instanceof Long l) return l.toString();
        if (val instanceof Double d) return Double.toString(d);
        if (val instanceof SchemeRational r) return r.numer + "/" + r.denom;
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
        if (val instanceof SchemeVector v) {
            if (seen.containsKey(v)) return "#<cycle>";
            seen.put(v, Boolean.TRUE);
            StringBuilder sb = new StringBuilder("#(");
            for (int i = 0; i < v.data.length; i++) {
                if (i > 0) sb.append(" ");
                sb.append(schemeToStringImpl(v.data[i], seen));
            }
            sb.append(")");
            seen.remove(v);
            return sb.toString();
        }
        if (val instanceof Pair) {
            if (seen.containsKey(val)) return "#<cycle>";
            seen.put(val, Boolean.TRUE);
            StringBuilder sb = new StringBuilder("(");
            Object cur = val;
            boolean first = true;
            while (cur instanceof Pair p) {
                if (!first) {
                    if (seen.containsKey(cur)) { sb.append(" . #<cycle>"); break; }
                    seen.put(cur, Boolean.TRUE);
                }
                if (!first) sb.append(" ");
                first = false;
                sb.append(schemeToStringImpl(p.car, seen));
                cur = p.cdr;
            }
            if (cur != NIL && !(cur instanceof Pair)) {
                sb.append(" . ").append(schemeToStringImpl(cur, seen));
            }
            sb.append(")");
            return sb.toString();
        }
        if (val instanceof List<?> list) {
            StringBuilder sb = new StringBuilder("(");
            for (int i = 0; i < list.size(); i++) {
                if (i > 0) sb.append(" ");
                sb.append(schemeToStringImpl(list.get(i), seen));
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
                 "let", "let*", "and", "or", "define-syntax", "syntax-rules",
                 "define-record-type", "case-lambda",
                 "letrec", "letrec*", "case", "do", "dynamic-wind" -> true;
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
        int pi = 1, fi = 1;
        int patLen = pattern.size(), formLen = form.size();
        while (pi < patLen) {
            Object rawPat = unwrap(pattern.get(pi));
            boolean hasEllipsis = (pi + 1 < patLen && "...".equals(unwrap(pattern.get(pi + 1))));
            if (hasEllipsis) {
                if (!(rawPat instanceof String varName)) return null;
                int remaining = 0;
                for (int k = pi + 2; k < patLen; k++) {
                    if (!"...".equals(unwrap(pattern.get(k)))) remaining++;
                }
                int endFi = formLen - remaining;
                List<Object> matched = new ArrayList<>();
                while (fi < endFi) { matched.add(form.get(fi)); fi++; }
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
                if (fi >= formLen) return null;
                fi++; pi++;
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
            if (patternVars.contains(sym) && !ellipsisVars.contains(sym)) return bindings.get(sym);
            if (ellipsisVars.contains(sym)) return bindings.get(sym);
            if (isKeyword(sym) || sym.equals("...")) return sym;
            try {
                sr.defEnv.lookup(sym);
                return new ResolvedRef(sym, sr.defEnv);
            } catch (EvalError e) {
                return gensymMap.computeIfAbsent(sym, k -> gensym(k));
            }
        }
        if (raw instanceof Boolean || raw instanceof Long ||
            raw instanceof SchemeString || raw instanceof SchemeChar) return raw;
        if (raw instanceof List<?> tmplList) {
            List<Object> result = new ArrayList<>();
            for (int i = 0; i < tmplList.size(); i++) {
                boolean hasEllipsis = (i + 1 < tmplList.size() && "...".equals(unwrap(tmplList.get(i + 1))));
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
                    i++;
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
            for (Object elem : list) found.addAll(findEllipsisVarsInTemplate(elem, ellipsisVars));
        }
        return found;
    }
}
