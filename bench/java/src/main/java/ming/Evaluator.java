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
        SchemeRational(long numer, long denom) {
            this.numer = numer;
            this.denom = denom;
        }
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

    static final class CaseLambda {
        final List<Lambda> clauses;
        CaseLambda(List<Lambda> clauses) { this.clauses = clauses; }
    }

    // ---- Record types ----
    static final class RecordType {
        final String name;
        final List<String> fieldNames;
        RecordType(String name, List<String> fieldNames) {
            this.name = name;
            this.fieldNames = fieldNames;
        }
    }

    static final class SchemeRecord {
        final RecordType type;
        final Object[] fields;
        SchemeRecord(RecordType type, Object[] fields) {
            this.type = type;
            this.fields = fields;
        }
    }

    static final class RecordConstructor {
        final RecordType type;
        final List<String> fieldOrder;
        RecordConstructor(RecordType type, List<String> fieldOrder) {
            this.type = type;
            this.fieldOrder = fieldOrder;
        }
    }

    static final class RecordPredicate {
        final RecordType type;
        RecordPredicate(RecordType type) { this.type = type; }
    }

    static final class RecordAccessor {
        final RecordType type;
        final int fieldIndex;
        RecordAccessor(RecordType type, int fieldIndex) {
            this.type = type;
            this.fieldIndex = fieldIndex;
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
            if (c == '#' && i + 1 < len && input.charAt(i + 1) == '(') {
                tokens.add(new Token("#(", startLine, startCol)); i += 2; col += 2; continue;
            }
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

        if (token.value.equals("#(")) {
            List<Object> elems = new ArrayList<>();
            while (pos < tokens.size() && !tokens.get(pos).value.equals(")")) {
                elems.add(parseExpr(tokens));
            }
            if (pos >= tokens.size()) throw new EvalError("missing closing parenthesis");
            pos++;
            // Desugar #(a b c) to (vector a b c)
            List<Object> list = new ArrayList<>();
            list.add("vector");
            list.addAll(elems);
            return new Located(list, token.line, token.col);
        }
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
        // Rational literal: digits/digits
        int slashIdx = token.indexOf('/');
        if (slashIdx > 0 && slashIdx < token.length() - 1) {
            try {
                long numer = Long.parseLong(token.substring(0, slashIdx));
                long denom = Long.parseLong(token.substring(slashIdx + 1));
                if (denom != 0) return makeRational(numer, denom);
            } catch (NumberFormatException ignored) {}
        }
        // Float literal
        try { return Double.parseDouble(token); } catch (NumberFormatException ignored) {}
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
      while (true) {
        // Unwrap Located and update current position
        if (expr instanceof Located loc) {
            currentLine = loc.line;
            currentCol = loc.col;
            expr = loc.datum;
        }

        if (expr instanceof Long || expr instanceof Double || expr instanceof Boolean || expr instanceof SchemeString || expr instanceof SchemeChar || expr instanceof SchemeRational) {
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
                            expr = list.get(2); continue;
                        } else if (list.size() == 4) {
                            expr = list.get(3); continue;
                        }
                        return null; // void
                    }
                    case "define" -> { return evalDefine(list, env); }
                    case "lambda" -> { return evalLambda(list, env); }
                    case "case-lambda" -> { return evalCaseLambda(list, env); }
                    case "and" -> {
                        if (list.size() == 1) return Boolean.TRUE;
                        for (int i = 1; i < list.size() - 1; i++) {
                            Object val = eval(list.get(i), env);
                            if (isFalse(val)) return val;
                        }
                        expr = list.get(list.size() - 1); continue;
                    }
                    case "or" -> {
                        if (list.size() == 1) return Boolean.FALSE;
                        for (int i = 1; i < list.size() - 1; i++) {
                            Object val = eval(list.get(i), env);
                            if (!isFalse(val)) return val;
                        }
                        expr = list.get(list.size() - 1); continue;
                    }
                    case "begin" -> {
                        if (list.size() == 1) return null;
                        for (int i = 1; i < list.size() - 1; i++) {
                            eval(list.get(i), env);
                        }
                        expr = list.get(list.size() - 1); continue;
                    }
                    case "let" -> {
                        if (list.size() < 3) throw posError("let: bad syntax");
                        Object second = unwrap(list.get(1));
                        if (second instanceof String namedLetName) {
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
                            Env letEnv = new Env(env);
                            Lambda loopLam = new Lambda(params, null, body, letEnv);
                            letEnv.define(namedLetName, loopLam);
                            Env callEnv = new Env(letEnv);
                            for (int i = 0; i < params.size(); i++) {
                                callEnv.define(params.get(i), eval(inits.get(i), env));
                            }
                            for (int i = 0; i < body.size() - 1; i++) {
                                eval(body.get(i), callEnv);
                            }
                            expr = body.get(body.size() - 1);
                            env = callEnv;
                            continue;
                        }
                        List<?> bindings = (List<?>) second;
                        Env letEnv = new Env(env);
                        for (Object b : bindings) {
                            List<?> binding = (List<?>) unwrap(b);
                            String bname = (String) unwrap(binding.get(0));
                            Object bval = eval(binding.get(1), env);
                            letEnv.define(bname, bval);
                        }
                        for (int i = 2; i < list.size() - 1; i++) {
                            eval(list.get(i), letEnv);
                        }
                        expr = list.get(list.size() - 1);
                        env = letEnv;
                        continue;
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
                    case "let*" -> {
                        if (list.size() < 3) throw posError("let*: bad syntax");
                        List<?> bindings = (List<?>) unwrap(list.get(1));
                        Env letEnv = new Env(env);
                        for (Object b : bindings) {
                            List<?> binding = (List<?>) unwrap(b);
                            String bname = (String) unwrap(binding.get(0));
                            Object bval = eval(binding.get(1), letEnv);
                            letEnv.define(bname, bval);
                        }
                        for (int i = 2; i < list.size() - 1; i++) {
                            eval(list.get(i), letEnv);
                        }
                        expr = list.get(list.size() - 1);
                        env = letEnv;
                        continue;
                    }
                    case "letrec" -> {
                        if (list.size() < 3) throw posError("letrec: bad syntax");
                        List<?> bindings = (List<?>) unwrap(list.get(1));
                        Env letrecEnv = new Env(env);
                        List<String> names = new ArrayList<>();
                        for (Object b : bindings) {
                            List<?> binding = (List<?>) unwrap(b);
                            String bname = (String) unwrap(binding.get(0));
                            names.add(bname);
                            letrecEnv.define(bname, null);
                        }
                        for (int i = 0; i < bindings.size(); i++) {
                            List<?> binding = (List<?>) unwrap(bindings.get(i));
                            Object bval = eval(binding.get(1), letrecEnv);
                            letrecEnv.set(names.get(i), bval);
                        }
                        for (int i = 2; i < list.size() - 1; i++) {
                            eval(list.get(i), letrecEnv);
                        }
                        expr = list.get(list.size() - 1);
                        env = letrecEnv;
                        continue;
                    }
                    case "letrec*" -> {
                        if (list.size() < 3) throw posError("letrec*: bad syntax");
                        List<?> bindings = (List<?>) unwrap(list.get(1));
                        Env letrecEnv = new Env(env);
                        for (Object b : bindings) {
                            List<?> binding = (List<?>) unwrap(b);
                            String bname = (String) unwrap(binding.get(0));
                            letrecEnv.define(bname, null);
                        }
                        for (Object b : bindings) {
                            List<?> binding = (List<?>) unwrap(b);
                            String bname = (String) unwrap(binding.get(0));
                            Object bval = eval(binding.get(1), letrecEnv);
                            letrecEnv.set(bname, bval);
                        }
                        for (int i = 2; i < list.size() - 1; i++) {
                            eval(list.get(i), letrecEnv);
                        }
                        expr = list.get(list.size() - 1);
                        env = letrecEnv;
                        continue;
                    }
                    case "case" -> { return evalCase(list, env); }
                    case "do" -> { return evalDo(list, env); }
                    case "cond" -> {
                        boolean matched = false;
                        for (int i = 1; i < list.size(); i++) {
                            List<?> clause = (List<?>) unwrap(list.get(i));
                            Object clauseHead = unwrap(clause.get(0));
                            if (clauseHead instanceof String s && s.equals("else")) {
                                for (int j = 1; j < clause.size() - 1; j++) {
                                    eval(clause.get(j), env);
                                }
                                if (clause.size() > 1) {
                                    expr = clause.get(clause.size() - 1); matched = true; break;
                                }
                                return null;
                            }
                            Object test = eval(clause.get(0), env);
                            if (!isFalse(test)) {
                                if (clause.size() == 1) return test;
                                for (int j = 1; j < clause.size() - 1; j++) {
                                    eval(clause.get(j), env);
                                }
                                expr = clause.get(clause.size() - 1); matched = true; break;
                            }
                        }
                        if (matched) continue;
                        return null;
                    }
                    case "define-syntax" -> {
                        if (list.size() != 3) throw posError("define-syntax: bad syntax");
                        String name = (String) unwrap(list.get(1));
                        SyntaxRules transformer = evalSyntaxRules(list.get(2), env);
                        env.define(name, transformer);
                        return null;
                    }
                    case "define-record-type" -> {
                        return evalDefineRecordType(list, env);
                    }
                }
                // Check if head is a macro
                try {
                    Object maybeTransformer = env.lookup(op);
                    if (maybeTransformer instanceof SyntaxRules sr) {
                        @SuppressWarnings("unchecked")
                        List<Object> formList = (List<Object>) list;
                        Object expanded = expandMacro(sr, formList);
                        expr = expanded; continue;
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
                    expr = expanded; continue;
                }
                List<Object> args = new ArrayList<>();
                for (int i = 1; i < list.size(); i++) {
                    args.add(eval(list.get(i), env));
                }
                if (resolved instanceof Lambda lam) {
                    env = applyLambdaEnv(lam, args);
                    for (int i = 0; i < lam.body.size() - 1; i++) eval(lam.body.get(i), env);
                    expr = lam.body.get(lam.body.size() - 1); continue;
                }
                return apply(resolved, args);
            }

            // General application
            int callLine = currentLine;
            int callCol = currentCol;
            Object proc = eval(head, env);
            List<Object> args = new ArrayList<>();
            for (int i = 1; i < list.size(); i++) {
                args.add(eval(list.get(i), env));
            }
            currentLine = callLine;
            currentCol = callCol;
            if (proc instanceof Lambda lam) {
                env = applyLambdaEnv(lam, args);
                for (int i = 0; i < lam.body.size() - 1; i++) eval(lam.body.get(i), env);
                expr = lam.body.get(lam.body.size() - 1); continue;
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
                env = applyLambdaEnv(matched, args);
                for (int i = 0; i < matched.body.size() - 1; i++) eval(matched.body.get(i), env);
                expr = matched.body.get(matched.body.size() - 1); continue;
            }
            return apply(proc, args);
        }
        throw posError("unknown expression type");
      } // end while
    }

    // Helper: parse parameter list with optional dot-rest notation
    private record ParamSpec(List<String> params, String restParam) {}

    private ParamSpec parseParamList(Object paramListRaw) {
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
            restParam = sym;
        }
        return new ParamSpec(params, restParam);
    }

    private Object evalDefine(List<?> list, Env env) throws EvalError {
        if (list.size() < 3) throw posError("define: bad syntax");
        Object target = unwrap(list.get(1));
        if (target instanceof String name) {
            Object val = eval(list.get(2), env);
            env.define(name, val);
            return null;
        }
        if (target instanceof List<?> sig) {
            if (sig.isEmpty()) throw posError("define: bad syntax");
            String name = (String) unwrap(sig.get(0));
            ParamSpec ps = parseParamList(sig.subList(1, sig.size()));
            List<Object> body = new ArrayList<>();
            for (int i = 2; i < list.size(); i++) body.add(list.get(i));
            env.define(name, new Lambda(ps.params, ps.restParam, body, env));
            return null;
        }
        throw posError("define: bad syntax");
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
        return callEnv;
    }

    private Object evalDo(List<?> list, Env env) throws EvalError {
        // (do ((var init step) ...) (test expr ...) body ...)
        if (list.size() < 3) throw posError("do: bad syntax");
        List<?> varSpecs = (List<?>) unwrap(list.get(1));
        List<?> testClause = (List<?>) unwrap(list.get(2));

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

        Env doEnv = new Env(env);
        for (int i = 0; i < nVars; i++) {
            doEnv.define(varNames[i], eval(initExprs[i], env));
        }

        while (true) {
            Object testResult = eval(testClause.get(0), doEnv);
            if (!isFalse(testResult)) {
                if (testClause.size() > 1) {
                    Object result = null;
                    for (int j = 1; j < testClause.size(); j++) {
                        result = eval(testClause.get(j), doEnv);
                    }
                    return result;
                }
                return null;
            }
            for (int j = 3; j < list.size(); j++) {
                eval(list.get(j), doEnv);
            }
            Object[] newVals = new Object[nVars];
            for (int i = 0; i < nVars; i++) {
                if (stepExprs[i] != null) {
                    newVals[i] = eval(stepExprs[i], doEnv);
                } else {
                    newVals[i] = doEnv.lookup(varNames[i]);
                }
            }
            for (int i = 0; i < nVars; i++) {
                doEnv.set(varNames[i], newVals[i]);
            }
        }
    }


    private Object evalCase(List<?> list, Env env) throws EvalError {
        if (list.size() < 2) throw posError("case: bad syntax");
        Object keyVal = eval(list.get(1), env);
        for (int i = 2; i < list.size(); i++) {
            List<?> clause = (List<?>) unwrap(list.get(i));
            Object datumHead = unwrap(clause.get(0));
            if (datumHead instanceof String s && s.equals("else")) {
                Object result = null;
                for (int j = 1; j < clause.size(); j++) {
                    result = eval(clause.get(j), env);
                }
                return result;
            }
            List<?> datums = (List<?>) datumHead;
            for (Object d : datums) {
                Object datum = quoteDatum(d);
                if (schemeEqv(keyVal, datum)) {
                    Object result = null;
                    for (int j = 1; j < clause.size(); j++) {
                        result = eval(clause.get(j), env);
                    }
                    return result;
                }
            }
        }
        return null;
    }

    private Object evalDefineRecordType(List<?> list, Env env) throws EvalError {
        // (define-record-type <name> (constructor field...) predicate (field accessor)...)
        if (list.size() < 4) throw posError("define-record-type: bad syntax");
        String typeName = (String) unwrap(list.get(1));
        List<?> ctorSpec = (List<?>) unwrap(list.get(2));
        String ctorName = (String) unwrap(ctorSpec.get(0));
        List<String> ctorFields = new ArrayList<>();
        for (int i = 1; i < ctorSpec.size(); i++) {
            ctorFields.add((String) unwrap(ctorSpec.get(i)));
        }
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
            if (accName != null) {
                env.define(accName, new RecordAccessor(rt, i));
            }
        }
        return null;
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
        if (proc instanceof CaseLambda cl) {
            for (Lambda lam : cl.clauses) {
                if (lam.restParam != null) {
                    if (args.size() >= lam.params.size()) return apply(lam, args);
                } else {
                    if (args.size() == lam.params.size()) return apply(lam, args);
                }
            }
            throw posError("no matching clause for " + args.size() + " arguments");
        }
        if (proc instanceof RecordConstructor rc) {
            if (args.size() != rc.fieldOrder.size()) {
                throw posError("wrong number of arguments to constructor: expected " + rc.fieldOrder.size() + ", got " + args.size());
            }
            return new SchemeRecord(rc.type, args.toArray());
        }
        if (proc instanceof RecordPredicate rp) {
            if (args.size() != 1) throw posError("wrong number of arguments to predicate");
            Object arg = args.get(0);
            return (arg instanceof SchemeRecord sr && sr.type == rp.type);
        }
        if (proc instanceof RecordAccessor ra) {
            if (args.size() != 1) throw posError("wrong number of arguments to accessor");
            Object arg = args.get(0);
            if (!(arg instanceof SchemeRecord sr) || sr.type != ra.type) {
                throw posError("accessor applied to wrong type");
            }
            return sr.fields[ra.fieldIndex];
        }
        throw posError("not a procedure: " + schemeToString(proc));
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
            if (aSet != null && aSet.contains(b)) return true; // already comparing these
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
        if (a instanceof String && b instanceof String) return a.equals(b); // symbols
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

    // display format: strings without quotes
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
        if (val == null) return ""; // void
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
                 "letrec", "letrec*", "case", "do" -> true;
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
