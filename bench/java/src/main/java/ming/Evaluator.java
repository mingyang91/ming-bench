package ming;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;

public class Evaluator {

    // --- Position tracking ---

    record Pos(int line, int col) {
        @Override public String toString() { return line + ":" + col; }
    }

    private record Located(Object value, Pos pos) {}

    private Pos currentPos = new Pos(1, 1);
    private List<Pos> tokenPositions = new ArrayList<>();

    private String posStr() {
        return currentPos + ": ";
    }

    private static Object unwrap(Object obj) {
        return obj instanceof Located loc ? loc.value() : obj;
    }

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

        void define(String name, Object value) {
            bindings.put(name, value);
        }

        void set(String name, Object value) throws EvalError {
            if (bindings.containsKey(name)) { bindings.put(name, value); return; }
            if (parent != null) { parent.set(name, value); return; }
            throw new EvalError("unbound variable: " + name);
        }
    }

    // --- Lambda (closure) ---

    private record Lambda(List<String> params, String restParam, List<Object> body, Env closureEnv) {}

    private record CaseLambda(List<Lambda> clauses) {}

    // Builtin procedure wrapper
    private record Builtin(String name) {}

    // Macro transformer (syntax-rules)
    private record SyntaxTransformer(List<String> literals, List<Object[]> clauses, Env defEnv) {}
    private record EllipsisList(List<Object> elements) {}

    private int gensymCounter = 0;
    private String gensym(String base) { return "__" + base + "_" + (gensymCounter++); }

    // --- Record types (define-record-type) ---

    private static class RecordType {
        final String name;
        final List<String> fieldNames;
        RecordType(String name, List<String> fieldNames) { this.name = name; this.fieldNames = fieldNames; }
    }

    private static class SchemeRecord {
        final RecordType type;
        final Object[] fields;
        SchemeRecord(RecordType type, Object[] fields) { this.type = type; this.fields = fields; }
    }

    private record RecordConstructor(RecordType type) {}
    private record RecordPredicate(RecordType type) {}
    private record RecordAccessor(RecordType type, int fieldIndex) {}

    // --- Vectors ---

    static class SchemeVector {
        Object[] elements;
        SchemeVector(Object[] elements) { this.elements = elements; }
        int length() { return elements.length; }
        Object ref(int i) { return elements[i]; }
        void set(int i, Object val) { elements[i] = val; }
    }

    private static final Set<String> SPECIAL_FORMS = Set.of(
        "define", "define-syntax", "define-record-type", "set!", "if", "quote", "lambda",
        "case-lambda", "and", "or", "begin", "cond", "let", "letrec", "letrec*", "case", "do"
    );

    // Internal string wrapper to distinguish from symbols (mutable for string-set!)
    static class SchemeString {
        private char[] chars;
        private boolean immutable;
        SchemeString(String value) { this.chars = value.toCharArray(); }
        SchemeString(String value, boolean immutable) { this.chars = value.toCharArray(); this.immutable = immutable; }
        String value() { return new String(chars); }
        char charAt(int i) { return chars[i]; }
        int length() { return chars.length; }
        boolean isImmutable() { return immutable; }
        void setChar(int i, char c) { chars[i] = c; }
        SchemeString copy() { return new SchemeString(value()); }
    }

    // Character wrapper
    record SchemeChar(char value) {}

    // Exact rational number (always simplified, positive denominator)
    static class SchemeRational {
        final long num;
        final long den;
        SchemeRational(long num, long den) { this.num = num; this.den = den; }
    }

    private static long gcd(long a, long b) {
        a = Math.abs(a); b = Math.abs(b);
        while (b != 0) { long t = b; b = a % b; a = t; }
        return a;
    }

    // Returns Long if den==1, else SchemeRational
    private static Object makeExact(long num, long den) {
        if (den < 0) { num = -num; den = -den; }
        long g = gcd(num, den);
        if (g > 0) { num /= g; den /= g; }
        if (den == 1) return num;
        return new SchemeRational(num, den);
    }

    private static boolean isNumber(Object val) {
        return val instanceof Long || val instanceof Double || val instanceof SchemeRational;
    }

    private double toDouble(Object val) throws EvalError {
        if (val instanceof Long l) return (double) l;
        if (val instanceof Double d) return d;
        if (val instanceof SchemeRational r) return (double) r.num / r.den;
        throw new EvalError(posStr() + "expected number");
    }

    // Returns [num, den] for exact numbers
    private long[] toRational(Object val) throws EvalError {
        if (val instanceof Long l) return new long[]{l, 1};
        if (val instanceof SchemeRational r) return new long[]{r.num, r.den};
        throw new EvalError(posStr() + "expected exact number");
    }

    // Cons pair
    record Pair(Object car, Object cdr) {}

    // Empty list sentinel
    static final Object NIL = new Object() {
        @Override public String toString() { return "()"; }
    };

    // Void sentinel for define
    private static final Object VOID = new Object() {
        @Override public String toString() { return "#<void>"; }
    };

    // Output buffer for display/write/newline
    private StringBuilder outputBuffer = new StringBuilder();

    public String evalStr(String input) throws EvalError {
        currentPos = new Pos(1, 1);
        List<Object> tokens = tokenize(input);
        int[] pos = {0};
        Env env = makeTopLevelEnv();
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
        currentPos = new Pos(1, 1);
        outputBuffer = new StringBuilder();
        List<Object> tokens = tokenize(input);
        int[] pos = {0};
        Env env = makeTopLevelEnv();
        Object lastResult = null;
        while (pos[0] < tokens.size()) {
            Object expr = parse(tokens, pos);
            lastResult = eval(expr, env);
        }
        String output = outputBuffer.toString();
        String result = (lastResult == null || lastResult == VOID) ? null : schemeToString(lastResult);
        return new EvalResult(result, output);
    }

    private static final String[] BUILTIN_NAMES = {
        "+", "-", "*", "/", "<", ">", "=", "<=", ">=", "not",
        "cons", "car", "cdr", "null?", "list", "length", "append",
        "string?", "number?", "boolean?", "pair?", "symbol?",
        "display", "write", "newline",
        "string-append", "string-length", "substring",
        "string->number", "number->string",
        "symbol->string", "string->symbol",
        "string-ref", "char?",
        "string-set!", "string-copy",
        "string->list", "list->string", "char->integer", "integer->char",
        "apply",
        // L09
        "abs", "modulo", "remainder", "quotient", "min", "max", "expt",
        "zero?", "positive?", "negative?", "odd?", "even?",
        "list-ref", "list-tail", "list?", "assoc", "map",
        "eq?", "equal?",
        "char-alphabetic?", "char-numeric?", "char-upcase", "char-downcase",
        "char=?", "char<?",
        "string=?", "string<?", "string-ci=?",
        "string-upcase", "string-downcase",
        // L11
        "exact?", "inexact?", "exact->inexact", "inexact->exact",
        "numerator", "denominator", "integer?", "rational?",
        // L13
        "procedure?",
        // L14
        "eqv?", "vector", "make-vector", "vector-ref", "vector-set!",
        "vector-length", "vector?", "vector->list", "list->vector"
    };

    private Env makeTopLevelEnv() {
        Env env = new Env(null);
        for (String name : BUILTIN_NAMES) {
            env.define(name, new Builtin(name));
        }
        return env;
    }

    // --- Tokenizer ---

    private List<Object> tokenize(String input) throws EvalError {
        List<Object> tokens = new ArrayList<>();
        tokenPositions = new ArrayList<>();
        int i = 0;
        int line = 1, col = 1;
        while (i < input.length()) {
            char c = input.charAt(i);
            if (Character.isWhitespace(c)) {
                if (c == '\n') { line++; col = 1; } else { col++; }
                i++;
            } else if (c == ';') {
                while (i < input.length() && input.charAt(i) != '\n') { i++; col++; }
            } else if (c == '(') {
                tokens.add("(");
                tokenPositions.add(new Pos(line, col));
                i++; col++;
            } else if (c == ')') {
                tokens.add(")");
                tokenPositions.add(new Pos(line, col));
                i++; col++;
            } else if (c == '\'') {
                tokens.add("'");
                tokenPositions.add(new Pos(line, col));
                i++; col++;
            } else if (c == '"') {
                Pos startPos = new Pos(line, col);
                StringBuilder sb = new StringBuilder();
                i++; col++; // skip opening quote
                while (i < input.length() && input.charAt(i) != '"') {
                    if (input.charAt(i) == '\\') {
                        i++; col++;
                        if (i < input.length()) {
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
                        if (input.charAt(i) == '\n') { line++; col = 0; }
                        sb.append(input.charAt(i));
                    }
                    i++; col++;
                }
                if (i < input.length()) { i++; col++; } // skip closing quote
                tokens.add(new SchemeString(sb.toString(), true));
                tokenPositions.add(startPos);
            } else if (c == '#') {
                Pos startPos = new Pos(line, col);
                if (i + 1 < input.length()) {
                    char next = input.charAt(i + 1);
                    if (next == '(') {
                        tokens.add("#(");
                        tokenPositions.add(startPos);
                        i += 2; col += 2;
                    } else if (next == 't') {
                        tokens.add(Boolean.TRUE);
                        tokenPositions.add(startPos);
                        i += 2; col += 2;
                    } else if (next == 'f') {
                        tokens.add(Boolean.FALSE);
                        tokenPositions.add(startPos);
                        i += 2; col += 2;
                    } else if (next == '\\') {
                        // Character literal: #\x or #\space, #\newline, #\tab
                        i += 2; col += 2;
                        if (i >= input.length()) throw new EvalError(startPos + ": unexpected end after #\\");
                        // Try to read a named character
                        int charStart = i;
                        while (i < input.length() && !Character.isWhitespace(input.charAt(i))
                                && input.charAt(i) != '(' && input.charAt(i) != ')'
                                && input.charAt(i) != '"' && input.charAt(i) != ';') {
                            i++; col++;
                        }
                        String charName = input.substring(charStart, i);
                        char ch;
                        if (charName.length() == 1) {
                            ch = charName.charAt(0);
                        } else {
                            ch = switch (charName.toLowerCase()) {
                                case "space" -> ' ';
                                case "newline" -> '\n';
                                case "tab" -> '\t';
                                default -> throw new EvalError(startPos + ": unknown character name: " + charName);
                            };
                        }
                        tokens.add(new SchemeChar(ch));
                        tokenPositions.add(startPos);
                    } else {
                        throw new EvalError(startPos + ": unexpected character after #: " + next);
                    }
                } else {
                    throw new EvalError(startPos + ": unexpected end after #");
                }
            } else {
                // symbol or number
                Pos startPos = new Pos(line, col);
                StringBuilder sb = new StringBuilder();
                while (i < input.length() && !Character.isWhitespace(input.charAt(i))
                        && input.charAt(i) != '(' && input.charAt(i) != ')'
                        && input.charAt(i) != '"' && input.charAt(i) != ';') {
                    sb.append(input.charAt(i));
                    i++; col++;
                }
                String tok = sb.toString();
                Object parsed = parseNumber(tok);
                tokens.add(parsed != null ? parsed : tok);
                tokenPositions.add(startPos);
            }
        }
        return tokens;
    }

    // Parse a token as a number: integer, rational (N/D), or float. Returns null if not a number.
    private static Object parseNumber(String tok) {
        // Try integer
        try { return Long.parseLong(tok); } catch (NumberFormatException e) {}
        // Try rational N/D
        int slash = tok.indexOf('/');
        if (slash > 0 && slash < tok.length() - 1) {
            try {
                long num = Long.parseLong(tok.substring(0, slash));
                long den = Long.parseLong(tok.substring(slash + 1));
                if (den != 0) return makeExact(num, den);
            } catch (NumberFormatException e) {}
        }
        // Try float
        try {
            double d = Double.parseDouble(tok);
            if (!Double.isInfinite(d) && !Double.isNaN(d)) return d;
        } catch (NumberFormatException e) {}
        return null;
    }

    // --- Parser ---

    private Object parse(List<Object> tokens, int[] pos) throws EvalError {
        if (pos[0] >= tokens.size()) {
            throw new EvalError(currentPos + ": unexpected end of input");
        }
        int tokenIdx = pos[0];
        Pos tokenPos = tokenPositions.get(tokenIdx);
        Object token = tokens.get(pos[0]);
        pos[0]++;
        if (token.equals("'")) {
            // 'expr -> (quote expr)
            Object quoted = parse(tokens, pos);
            List<Object> quoteExpr = new ArrayList<>();
            quoteExpr.add(new Located("quote", tokenPos));
            quoteExpr.add(quoted);
            return new Located(quoteExpr, tokenPos);
        }
        if (token.equals("#(")) {
            List<Object> elems = new ArrayList<>();
            while (pos[0] < tokens.size() && !tokens.get(pos[0]).equals(")")) {
                elems.add(parse(tokens, pos));
            }
            if (pos[0] >= tokens.size()) {
                throw new EvalError(tokenPos + ": missing closing parenthesis for vector");
            }
            pos[0]++; // skip )
            // Build (vector e1 e2 ...) form
            List<Object> vecForm = new ArrayList<>();
            vecForm.add(new Located("vector", tokenPos));
            vecForm.addAll(elems);
            return new Located(vecForm, tokenPos);
        }
        if (token.equals("(")) {
            List<Object> list = new ArrayList<>();
            while (pos[0] < tokens.size() && !tokens.get(pos[0]).equals(")")) {
                list.add(parse(tokens, pos));
            }
            if (pos[0] >= tokens.size()) {
                throw new EvalError(tokenPos + ": missing closing parenthesis");
            }
            pos[0]++; // skip )
            return new Located(list, tokenPos);
        } else if (token.equals(")")) {
            throw new EvalError(tokenPos + ": unexpected )");
        } else {
            return new Located(token, tokenPos);
        }
    }

    // --- Evaluator ---

    @SuppressWarnings("unchecked")
    private Object eval(Object expr, Env env) throws EvalError {
        if (expr instanceof Located loc) {
            currentPos = loc.pos();
            expr = loc.value();
        }

        if (expr instanceof Long || expr instanceof Double || expr instanceof SchemeRational || expr instanceof Boolean || expr instanceof SchemeString || expr instanceof SchemeChar || expr instanceof SchemeVector) {
            return expr;
        }
        if (expr instanceof String sym) {
            try {
                return env.lookup(sym);
            } catch (EvalError e) {
                throw new EvalError(posStr() + e.getMessage());
            }
        }
        if (expr instanceof List<?> list) {
            if (list.isEmpty()) {
                throw new EvalError(posStr() + "empty application");
            }
            Object head = list.get(0);
            Object rawHead = unwrap(head);

            // Special forms
            if (rawHead instanceof String op) {
                switch (op) {
                    case "define" -> {
                        if (list.size() < 3) throw new EvalError(posStr() + "define: bad syntax");
                        Object target = unwrap(list.get(1));
                        if (target instanceof String name) {
                            env.define(name, eval(list.get(2), env));
                        } else if (target instanceof List<?> sig) {
                            // (define (f params... . rest) body...)
                            Object rawFirst = sig.isEmpty() ? null : unwrap(sig.get(0));
                            if (sig.isEmpty() || !(rawFirst instanceof String fname)) {
                                throw new EvalError(posStr() + "define: bad syntax");
                            }
                            List<String> params = new ArrayList<>();
                            String restParam = null;
                            for (int i = 1; i < sig.size(); i++) {
                                Object p = unwrap(sig.get(i));
                                if (p instanceof String s && s.equals(".")) {
                                    if (i + 1 < sig.size()) {
                                        Object rp = unwrap(sig.get(i + 1));
                                        if (!(rp instanceof String rpName)) throw new EvalError(posStr() + "define: rest parameter must be a symbol");
                                        restParam = rpName;
                                        break;
                                    } else {
                                        throw new EvalError(posStr() + "define: bad syntax after dot");
                                    }
                                }
                                if (!(p instanceof String s)) {
                                    throw new EvalError(posStr() + "define: parameter must be a symbol");
                                }
                                params.add(s);
                            }
                            List<Object> body = new ArrayList<>(list.subList(2, list.size()));
                            env.define(fname, new Lambda(params, restParam, body, env));
                        } else {
                            throw new EvalError(posStr() + "define: bad syntax");
                        }
                        return VOID;
                    }
                    case "define-syntax" -> {
                        if (list.size() != 3) throw new EvalError(posStr() + "define-syntax: bad syntax");
                        Object macroNameObj = unwrap(list.get(1));
                        if (!(macroNameObj instanceof String macroName)) throw new EvalError(posStr() + "define-syntax: expected symbol");
                        Object srFormObj = unwrap(list.get(2));
                        if (!(srFormObj instanceof List<?> srForm) || srForm.isEmpty()) throw new EvalError(posStr() + "define-syntax: expected syntax-rules");
                        Object srHead = unwrap(srForm.get(0));
                        if (!(srHead instanceof String srStr) || !srStr.equals("syntax-rules")) throw new EvalError(posStr() + "define-syntax: expected syntax-rules");
                        if (srForm.size() < 2) throw new EvalError(posStr() + "syntax-rules: bad syntax");
                        Object litsObj = unwrap(srForm.get(1));
                        List<String> literals = new ArrayList<>();
                        if (litsObj instanceof List<?> litsList) {
                            for (Object l : litsList) {
                                Object ul = unwrap(l);
                                if (ul instanceof String s) literals.add(s);
                            }
                        }
                        List<Object[]> clauses = new ArrayList<>();
                        for (int i = 2; i < srForm.size(); i++) {
                            Object clauseObj = unwrap(srForm.get(i));
                            if (!(clauseObj instanceof List<?> clause) || clause.size() != 2) throw new EvalError(posStr() + "syntax-rules: bad clause");
                            clauses.add(new Object[]{unwrapDeep(clause.get(0)), unwrapDeep(clause.get(1))});
                        }
                        env.define(macroName, new SyntaxTransformer(literals, clauses, env));
                        return VOID;
                    }
                    case "define-record-type" -> {
                        // (define-record-type <name> (constructor field...) predicate (field accessor)...)
                        if (list.size() < 4) throw new EvalError(posStr() + "define-record-type: bad syntax");
                        Object typeName = unwrap(list.get(1));
                        if (!(typeName instanceof String)) throw new EvalError(posStr() + "define-record-type: expected type name");
                        Object ctorSpec = unwrap(list.get(2));
                        if (!(ctorSpec instanceof List<?> ctorList) || ctorList.isEmpty())
                            throw new EvalError(posStr() + "define-record-type: bad constructor spec");
                        String ctorName = (String) unwrap(ctorList.get(0));
                        List<String> ctorFields = new ArrayList<>();
                        for (int i = 1; i < ctorList.size(); i++) {
                            ctorFields.add((String) unwrap(ctorList.get(i)));
                        }
                        Object predObj = unwrap(list.get(3));
                        if (!(predObj instanceof String predName)) throw new EvalError(posStr() + "define-record-type: expected predicate name");
                        // Collect field specs
                        List<String> fieldNames = new ArrayList<>(ctorFields);
                        RecordType rt = new RecordType((String) typeName, fieldNames);
                        // Define constructor
                        env.define(ctorName, new RecordConstructor(rt));
                        // Define predicate
                        env.define(predName, new RecordPredicate(rt));
                        // Define accessors
                        for (int i = 4; i < list.size(); i++) {
                            Object fieldSpec = unwrap(list.get(i));
                            if (!(fieldSpec instanceof List<?> fsList) || fsList.size() < 2)
                                throw new EvalError(posStr() + "define-record-type: bad field spec");
                            String fieldName = (String) unwrap(fsList.get(0));
                            String accessorName = (String) unwrap(fsList.get(1));
                            int idx = fieldNames.indexOf(fieldName);
                            if (idx < 0) throw new EvalError(posStr() + "define-record-type: unknown field " + fieldName);
                            env.define(accessorName, new RecordAccessor(rt, idx));
                        }
                        return VOID;
                    }
                    case "set!" -> {
                        if (list.size() != 3) throw new EvalError(posStr() + "set!: bad syntax");
                        Object nameObj = unwrap(list.get(1));
                        if (!(nameObj instanceof String name)) throw new EvalError(posStr() + "set!: not a symbol");
                        Object val = eval(list.get(2), env);
                        try {
                            env.set(name, val);
                        } catch (EvalError e) {
                            throw new EvalError(posStr() + e.getMessage());
                        }
                        return VOID;
                    }
                    case "if" -> {
                        if (list.size() < 3) throw new EvalError(posStr() + "if: bad syntax");
                        Object cond = eval(list.get(1), env);
                        if (!isFalse(cond)) {
                            return eval(list.get(2), env);
                        } else if (list.size() > 3) {
                            return eval(list.get(3), env);
                        } else {
                            return VOID;
                        }
                    }
                    case "quote" -> {
                        if (list.size() != 2) throw new EvalError(posStr() + "quote: bad syntax");
                        return quoteDatum(list.get(1));
                    }
                    case "lambda" -> {
                        if (list.size() < 3) throw new EvalError(posStr() + "lambda: bad syntax");
                        Object paramSpec = unwrap(list.get(1));
                        if (!(paramSpec instanceof List<?> paramList)) {
                            throw new EvalError(posStr() + "lambda: parameters must be a list");
                        }
                        List<String> params = new ArrayList<>();
                        String restParam = null;
                        for (int pi = 0; pi < paramList.size(); pi++) {
                            Object rawP = unwrap(paramList.get(pi));
                            if (rawP instanceof String s && s.equals(".")) {
                                if (pi + 1 < paramList.size()) {
                                    Object rp = unwrap(paramList.get(pi + 1));
                                    if (!(rp instanceof String rpName)) throw new EvalError(posStr() + "lambda: rest parameter must be a symbol");
                                    restParam = rpName;
                                    break;
                                } else {
                                    throw new EvalError(posStr() + "lambda: bad syntax after dot");
                                }
                            }
                            if (!(rawP instanceof String s)) {
                                throw new EvalError(posStr() + "lambda: parameter must be a symbol");
                            }
                            params.add(s);
                        }
                        List<Object> body = new ArrayList<>(list.subList(2, list.size()));
                        return new Lambda(params, restParam, body, env);
                    }
                    case "case-lambda" -> {
                        if (list.size() < 2) throw new EvalError(posStr() + "case-lambda: bad syntax");
                        List<Lambda> clauses = new ArrayList<>();
                        for (int ci = 1; ci < list.size(); ci++) {
                            Object clauseRaw = unwrap(list.get(ci));
                            if (!(clauseRaw instanceof List<?> clause) || clause.size() < 2)
                                throw new EvalError(posStr() + "case-lambda: bad clause");
                            Object paramSpec = unwrap(clause.get(0));
                            if (!(paramSpec instanceof List<?> paramList))
                                throw new EvalError(posStr() + "case-lambda: parameters must be a list");
                            List<String> params = new ArrayList<>();
                            String restParam = null;
                            for (int pi = 0; pi < paramList.size(); pi++) {
                                Object rawP = unwrap(paramList.get(pi));
                                if (rawP instanceof String s && s.equals(".")) {
                                    if (pi + 1 < paramList.size()) {
                                        Object rp = unwrap(paramList.get(pi + 1));
                                        if (!(rp instanceof String rpName))
                                            throw new EvalError(posStr() + "case-lambda: rest parameter must be a symbol");
                                        restParam = rpName;
                                        break;
                                    } else {
                                        throw new EvalError(posStr() + "case-lambda: bad syntax after dot");
                                    }
                                }
                                if (!(rawP instanceof String s))
                                    throw new EvalError(posStr() + "case-lambda: parameter must be a symbol");
                                params.add(s);
                            }
                            List<Object> body = new ArrayList<>(clause.subList(1, clause.size()));
                            clauses.add(new Lambda(params, restParam, body, env));
                        }
                        return new CaseLambda(clauses);
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
                    case "begin" -> {
                        Object result = VOID;
                        for (int i = 1; i < list.size(); i++) {
                            result = eval(list.get(i), env);
                        }
                        return result;
                    }
                    case "cond" -> {
                        for (int i = 1; i < list.size(); i++) {
                            Object clauseObj = unwrap(list.get(i));
                            if (!(clauseObj instanceof List<?> clause) || clause.isEmpty()) {
                                throw new EvalError(posStr() + "cond: bad clause");
                            }
                            Object clauseHead = clause.get(0);
                            Object rawClauseHead = unwrap(clauseHead);
                            if (rawClauseHead instanceof String s && s.equals("else")) {
                                Object result = VOID;
                                for (int j = 1; j < clause.size(); j++) {
                                    result = eval(clause.get(j), env);
                                }
                                return result;
                            }
                            Object test = eval(clauseHead, env);
                            if (!isFalse(test)) {
                                if (clause.size() == 1) return test;
                                Object result = VOID;
                                for (int j = 1; j < clause.size(); j++) {
                                    result = eval(clause.get(j), env);
                                }
                                return result;
                            }
                        }
                        return VOID;
                    }
                    case "let" -> {
                        if (list.size() < 3) throw new EvalError(posStr() + "let: bad syntax");
                        Object second = unwrap(list.get(1));

                        // Named let: (let name ((var val) ...) body...)
                        if (second instanceof String loopName) {
                            if (list.size() < 4) throw new EvalError(posStr() + "let: bad syntax");
                            Object bindsObj = unwrap(list.get(2));
                            if (!(bindsObj instanceof List<?> bindingsList)) {
                                throw new EvalError(posStr() + "let: bindings must be a list");
                            }
                            List<String> params = new ArrayList<>();
                            List<Object> initVals = new ArrayList<>();
                            for (Object binding : bindingsList) {
                                Object rawBinding = unwrap(binding);
                                if (!(rawBinding instanceof List<?> bp) || bp.size() != 2) {
                                    throw new EvalError(posStr() + "let: bad binding");
                                }
                                Object pnameObj = unwrap(bp.get(0));
                                if (!(pnameObj instanceof String pname)) {
                                    throw new EvalError(posStr() + "let: binding name must be a symbol");
                                }
                                params.add(pname);
                                initVals.add(eval(bp.get(1), env));
                            }
                            List<Object> body = new ArrayList<>(list.subList(3, list.size()));
                            Env letEnv = new Env(env);
                            Lambda loopLam = new Lambda(params, null, body, letEnv);
                            letEnv.define(loopName, loopLam);
                            Env callEnv = new Env(letEnv);
                            for (int i = 0; i < params.size(); i++) {
                                callEnv.define(params.get(i), initVals.get(i));
                            }
                            Object result = VOID;
                            for (Object bodyExpr : body) {
                                result = eval(bodyExpr, callEnv);
                            }
                            return result;
                        }

                        // Regular let
                        if (!(second instanceof List<?> bindings)) {
                            throw new EvalError(posStr() + "let: bindings must be a list");
                        }
                        Env letEnv = new Env(env);
                        for (Object binding : bindings) {
                            Object rawBinding = unwrap(binding);
                            if (!(rawBinding instanceof List<?> pair) || pair.size() != 2) {
                                throw new EvalError(posStr() + "let: bad binding");
                            }
                            Object nameObj = unwrap(pair.get(0));
                            if (!(nameObj instanceof String name)) {
                                throw new EvalError(posStr() + "let: binding name must be a symbol");
                            }
                            letEnv.define(name, eval(pair.get(1), env));
                        }
                        Object result = VOID;
                        for (int i = 2; i < list.size(); i++) {
                            result = eval(list.get(i), letEnv);
                        }
                        return result;
                    }
                    case "letrec" -> {
                        if (list.size() < 3) throw new EvalError(posStr() + "letrec: bad syntax");
                        Object bindsObj = unwrap(list.get(1));
                        if (!(bindsObj instanceof List<?> bindingsList)) throw new EvalError(posStr() + "letrec: bindings must be a list");
                        Env letrecEnv = new Env(env);
                        // First define all vars as uninitialized
                        List<String> names = new ArrayList<>();
                        List<Object> initExprs = new ArrayList<>();
                        for (Object binding : bindingsList) {
                            Object rawBinding = unwrap(binding);
                            if (!(rawBinding instanceof List<?> bp) || bp.size() != 2) throw new EvalError(posStr() + "letrec: bad binding");
                            Object nameObj = unwrap(bp.get(0));
                            if (!(nameObj instanceof String name)) throw new EvalError(posStr() + "letrec: binding name must be a symbol");
                            names.add(name);
                            initExprs.add(bp.get(1));
                            letrecEnv.define(name, VOID);
                        }
                        // Evaluate inits in letrec env, then assign
                        for (int i = 0; i < names.size(); i++) {
                            letrecEnv.define(names.get(i), eval(initExprs.get(i), letrecEnv));
                        }
                        Object result = VOID;
                        for (int i = 2; i < list.size(); i++) {
                            result = eval(list.get(i), letrecEnv);
                        }
                        return result;
                    }
                    case "letrec*" -> {
                        if (list.size() < 3) throw new EvalError(posStr() + "letrec*: bad syntax");
                        Object bindsObj = unwrap(list.get(1));
                        if (!(bindsObj instanceof List<?> bindingsList)) throw new EvalError(posStr() + "letrec*: bindings must be a list");
                        Env letrecEnv = new Env(env);
                        for (Object binding : bindingsList) {
                            Object rawBinding = unwrap(binding);
                            if (!(rawBinding instanceof List<?> bp) || bp.size() != 2) throw new EvalError(posStr() + "letrec*: bad binding");
                            Object nameObj = unwrap(bp.get(0));
                            if (!(nameObj instanceof String name)) throw new EvalError(posStr() + "letrec*: binding name must be a symbol");
                            letrecEnv.define(name, eval(bp.get(1), letrecEnv));
                        }
                        Object result = VOID;
                        for (int i = 2; i < list.size(); i++) {
                            result = eval(list.get(i), letrecEnv);
                        }
                        return result;
                    }
                    case "case" -> {
                        if (list.size() < 2) throw new EvalError(posStr() + "case: bad syntax");
                        Object key = eval(list.get(1), env);
                        for (int i = 2; i < list.size(); i++) {
                            Object clauseObj = unwrap(list.get(i));
                            if (!(clauseObj instanceof List<?> clause) || clause.isEmpty())
                                throw new EvalError(posStr() + "case: bad clause");
                            Object datums = unwrap(clause.get(0));
                            if (datums instanceof String s && s.equals("else")) {
                                Object result = VOID;
                                for (int j = 1; j < clause.size(); j++) {
                                    result = eval(clause.get(j), env);
                                }
                                return result;
                            }
                            if (!(datums instanceof List<?> datumList))
                                throw new EvalError(posStr() + "case: expected datum list");
                            for (Object d : datumList) {
                                Object datum = quoteDatum(d);
                                if (schemeEqv(key, datum)) {
                                    Object result = VOID;
                                    for (int j = 1; j < clause.size(); j++) {
                                        result = eval(clause.get(j), env);
                                    }
                                    return result;
                                }
                            }
                        }
                        return VOID;
                    }
                    case "do" -> {
                        // (do ((var init step) ...) (test expr ...) body ...)
                        if (list.size() < 3) throw new EvalError(posStr() + "do: bad syntax");
                        Object varsObj = unwrap(list.get(1));
                        if (!(varsObj instanceof List<?> varSpecs)) throw new EvalError(posStr() + "do: bad variable specs");
                        Object testObj = unwrap(list.get(2));
                        if (!(testObj instanceof List<?> testClause) || testClause.isEmpty())
                            throw new EvalError(posStr() + "do: bad test clause");

                        // Parse variable specs
                        List<String> varNames = new ArrayList<>();
                        List<Object> stepExprs = new ArrayList<>(); // null means no step
                        Env doEnv = new Env(env);
                        for (Object spec : varSpecs) {
                            Object rawSpec = unwrap(spec);
                            if (!(rawSpec instanceof List<?> specList) || specList.size() < 2)
                                throw new EvalError(posStr() + "do: bad variable spec");
                            Object nameObj = unwrap(specList.get(0));
                            if (!(nameObj instanceof String name)) throw new EvalError(posStr() + "do: variable must be a symbol");
                            varNames.add(name);
                            doEnv.define(name, eval(specList.get(1), env));
                            stepExprs.add(specList.size() >= 3 ? specList.get(2) : null);
                        }

                        // Iterate
                        while (true) {
                            Object test = eval(testClause.get(0), doEnv);
                            if (!isFalse(test)) {
                                // Test is true — evaluate result expressions
                                if (testClause.size() == 1) return VOID;
                                Object result = VOID;
                                for (int j = 1; j < testClause.size(); j++) {
                                    result = eval(testClause.get(j), doEnv);
                                }
                                return result;
                            }
                            // Evaluate body
                            for (int j = 3; j < list.size(); j++) {
                                eval(list.get(j), doEnv);
                            }
                            // Parallel step: evaluate all steps with current values, then update
                            List<Object> newVals = new ArrayList<>();
                            for (int j = 0; j < varNames.size(); j++) {
                                if (stepExprs.get(j) != null) {
                                    newVals.add(eval(stepExprs.get(j), doEnv));
                                } else {
                                    newVals.add(doEnv.lookup(varNames.get(j)));
                                }
                            }
                            for (int j = 0; j < varNames.size(); j++) {
                                doEnv.define(varNames.get(j), newVals.get(j));
                            }
                        }
                    }
                }
            }

            // Check for macro application
            if (rawHead instanceof String macroSym) {
                try {
                    Object macroVal = env.lookup(macroSym);
                    if (macroVal instanceof SyntaxTransformer st) {
                        return eval(expandMacro(st, list, env), env);
                    }
                } catch (EvalError e) { /* not bound, fall through */ }
            }

            // Function application
            Object proc = eval(head, env);

            // Evaluate arguments
            List<Object> args = new ArrayList<>();
            for (int i = 1; i < list.size(); i++) {
                args.add(eval(list.get(i), env));
            }

            if (proc instanceof Lambda lam) {
                return applyLambda(lam, args);
            }

            if (proc instanceof CaseLambda cl) {
                return applyCaseLambda(cl, args);
            }

            if (proc instanceof Builtin b) {
                return applyBuiltin(b.name(), args);
            }

            if (proc instanceof RecordConstructor rc) {
                if (args.size() != rc.type().fieldNames.size())
                    throw new EvalError(posStr() + "wrong number of arguments to record constructor");
                return new SchemeRecord(rc.type(), args.toArray());
            }
            if (proc instanceof RecordPredicate rp) {
                if (args.size() != 1) throw new EvalError(posStr() + "record predicate expects 1 argument");
                return (args.get(0) instanceof SchemeRecord sr && sr.type == rp.type());
            }
            if (proc instanceof RecordAccessor ra) {
                if (args.size() != 1) throw new EvalError(posStr() + "record accessor expects 1 argument");
                if (!(args.get(0) instanceof SchemeRecord sr) || sr.type != ra.type())
                    throw new EvalError(posStr() + "record accessor: wrong record type");
                return sr.fields[ra.fieldIndex()];
            }

            throw new EvalError(posStr() + "not a procedure: " + schemeToString(proc));
        }
        throw new EvalError(posStr() + "cannot eval: " + expr);
    }

    private Object applyLambda(Lambda lam, List<Object> args) throws EvalError {
        int required = lam.params().size();
        if (lam.restParam() != null) {
            if (args.size() < required) {
                throw new EvalError(posStr() + "wrong number of arguments: expected at least " + required + ", got " + args.size());
            }
        } else {
            if (args.size() != required) {
                throw new EvalError(posStr() + "wrong number of arguments: expected " + required + ", got " + args.size());
            }
        }
        Env callEnv = new Env(lam.closureEnv());
        for (int i = 0; i < required; i++) {
            callEnv.define(lam.params().get(i), args.get(i));
        }
        if (lam.restParam() != null) {
            Object rest = NIL;
            for (int i = args.size() - 1; i >= required; i--) {
                rest = new Pair(args.get(i), rest);
            }
            callEnv.define(lam.restParam(), rest);
        }
        Object result = VOID;
        for (Object bodyExpr : lam.body()) {
            result = eval(bodyExpr, callEnv);
        }
        return result;
    }

    private Object applyCaseLambda(CaseLambda cl, List<Object> args) throws EvalError {
        for (Lambda lam : cl.clauses()) {
            int required = lam.params().size();
            if (lam.restParam() != null) {
                if (args.size() >= required) return applyLambda(lam, args);
            } else {
                if (args.size() == required) return applyLambda(lam, args);
            }
        }
        throw new EvalError(posStr() + "case-lambda: no matching clause for " + args.size() + " arguments");
    }

    private Object applyProcedure(Object proc, List<Object> args) throws EvalError {
        if (proc instanceof Lambda lam) {
            return applyLambda(lam, args);
        }
        if (proc instanceof CaseLambda cl) {
            return applyCaseLambda(cl, args);
        }
        if (proc instanceof Builtin b) {
            return applyBuiltin(b.name(), args);
        }
        if (proc instanceof RecordConstructor rc) {
            if (args.size() != rc.type().fieldNames.size())
                throw new EvalError(posStr() + "wrong number of arguments to record constructor");
            return new SchemeRecord(rc.type(), args.toArray());
        }
        if (proc instanceof RecordPredicate rp) {
            if (args.size() != 1) throw new EvalError(posStr() + "record predicate expects 1 argument");
            return (args.get(0) instanceof SchemeRecord sr && sr.type == rp.type());
        }
        if (proc instanceof RecordAccessor ra) {
            if (args.size() != 1) throw new EvalError(posStr() + "record accessor expects 1 argument");
            if (!(args.get(0) instanceof SchemeRecord sr) || sr.type != ra.type())
                throw new EvalError(posStr() + "record accessor: wrong record type");
            return sr.fields[ra.fieldIndex()];
        }
        throw new EvalError(posStr() + "not a procedure: " + schemeToString(proc));
    }

    private Object applyBuiltin(String op, List<Object> args) throws EvalError {
        switch (op) {
            case "+" -> {
                if (args.isEmpty()) return 0L;
                boolean inexact = false;
                for (Object a : args) { if (!isNumber(a)) throw new EvalError(posStr() + "+: not a number"); if (a instanceof Double) inexact = true; }
                if (inexact) { double s = 0; for (Object a : args) s += toDouble(a); return s; }
                long num = 0, den = 1;
                for (Object a : args) { long[] r = toRational(a); num = num * r[1] + r[0] * den; den *= r[1]; long g = gcd(Math.abs(num), Math.abs(den)); if (g > 0) { num /= g; den /= g; } }
                return makeExact(num, den);
            }
            case "-" -> {
                if (args.isEmpty()) throw new EvalError(posStr() + "- requires at least 1 argument");
                boolean inexact = false;
                for (Object a : args) { if (!isNumber(a)) throw new EvalError(posStr() + "-: not a number"); if (a instanceof Double) inexact = true; }
                if (inexact) {
                    double r = toDouble(args.get(0));
                    if (args.size() == 1) return -r;
                    for (int i = 1; i < args.size(); i++) r -= toDouble(args.get(i));
                    return r;
                }
                long[] first = toRational(args.get(0));
                long num = first[0], den = first[1];
                if (args.size() == 1) return makeExact(-num, den);
                for (int i = 1; i < args.size(); i++) { long[] r = toRational(args.get(i)); num = num * r[1] - r[0] * den; den *= r[1]; long g = gcd(Math.abs(num), Math.abs(den)); if (g > 0) { num /= g; den /= g; } }
                return makeExact(num, den);
            }
            case "*" -> {
                boolean inexact = false;
                for (Object a : args) { if (!isNumber(a)) throw new EvalError(posStr() + "*: not a number"); if (a instanceof Double) inexact = true; }
                if (inexact) { double p = 1; for (Object a : args) p *= toDouble(a); return p; }
                long num = 1, den = 1;
                for (Object a : args) { long[] r = toRational(a); num *= r[0]; den *= r[1]; long g = gcd(Math.abs(num), Math.abs(den)); if (g > 0) { num /= g; den /= g; } }
                return makeExact(num, den);
            }
            case "/" -> {
                if (args.size() < 2) throw new EvalError(posStr() + "/ requires at least 2 arguments");
                boolean inexact = false;
                for (Object a : args) { if (!isNumber(a)) throw new EvalError(posStr() + "/: not a number"); if (a instanceof Double) inexact = true; }
                if (inexact) { double r = toDouble(args.get(0)); for (int i = 1; i < args.size(); i++) { double d = toDouble(args.get(i)); if (d == 0) throw new EvalError(posStr() + "division by zero"); r /= d; } return r; }
                long[] first = toRational(args.get(0));
                long num = first[0], den = first[1];
                for (int i = 1; i < args.size(); i++) { long[] r = toRational(args.get(i)); if (r[0] == 0) throw new EvalError(posStr() + "division by zero"); num *= r[1]; den *= r[0]; long g = gcd(Math.abs(num), Math.abs(den)); if (g > 0) { num /= g; den /= g; } }
                return makeExact(num, den);
            }
            case "<" -> {
                requireArgCount(op, args, 2);
                return numCompare(args.get(0), args.get(1)) < 0;
            }
            case ">" -> {
                requireArgCount(op, args, 2);
                return numCompare(args.get(0), args.get(1)) > 0;
            }
            case "=" -> {
                requireArgCount(op, args, 2);
                return numCompare(args.get(0), args.get(1)) == 0;
            }
            case "<=" -> {
                requireArgCount(op, args, 2);
                return numCompare(args.get(0), args.get(1)) <= 0;
            }
            case ">=" -> {
                requireArgCount(op, args, 2);
                return numCompare(args.get(0), args.get(1)) >= 0;
            }
            case "not" -> {
                requireArgCount(op, args, 1);
                return isFalse(args.get(0));
            }
            case "cons" -> {
                requireArgCount(op, args, 2);
                return new Pair(args.get(0), args.get(1));
            }
            case "car" -> {
                requireArgCount(op, args, 1);
                if (!(args.get(0) instanceof Pair p)) throw new EvalError(posStr() + "car: not a pair");
                return p.car();
            }
            case "cdr" -> {
                requireArgCount(op, args, 1);
                if (!(args.get(0) instanceof Pair p)) throw new EvalError(posStr() + "cdr: not a pair");
                return p.cdr();
            }
            case "null?" -> {
                requireArgCount(op, args, 1);
                return args.get(0) == NIL;
            }
            case "list" -> {
                Object result = NIL;
                for (int i = args.size() - 1; i >= 0; i--) {
                    result = new Pair(args.get(i), result);
                }
                return result;
            }
            case "length" -> {
                requireArgCount(op, args, 1);
                Object obj = args.get(0);
                if (obj == NIL) return 0L;
                int count = 0;
                while (obj instanceof Pair p) {
                    count++;
                    obj = p.cdr();
                }
                if (obj != NIL) throw new EvalError(posStr() + "length: not a proper list");
                return (long) count;
            }
            case "append" -> {
                Object result = NIL;
                for (int i = args.size() - 1; i >= 0; i--) {
                    Object lst = args.get(i);
                    if (lst == NIL) continue;
                    if (i == args.size() - 1) {
                        result = lst;
                    } else {
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
                return result;
            }
            case "string?" -> {
                requireArgCount(op, args, 1);
                return args.get(0) instanceof SchemeString;
            }
            case "number?" -> {
                requireArgCount(op, args, 1);
                return isNumber(args.get(0));
            }
            case "boolean?" -> {
                requireArgCount(op, args, 1);
                return args.get(0) instanceof Boolean;
            }
            case "pair?" -> {
                requireArgCount(op, args, 1);
                return args.get(0) instanceof Pair;
            }
            case "symbol?" -> {
                requireArgCount(op, args, 1);
                return args.get(0) instanceof String;
            }
            case "char?" -> {
                requireArgCount(op, args, 1);
                return args.get(0) instanceof SchemeChar;
            }
            case "display" -> {
                requireArgCount(op, args, 1);
                outputBuffer.append(displayString(args.get(0)));
                return VOID;
            }
            case "write" -> {
                requireArgCount(op, args, 1);
                outputBuffer.append(schemeToString(args.get(0)));
                return VOID;
            }
            case "newline" -> {
                requireArgCount(op, args, 0);
                outputBuffer.append('\n');
                return VOID;
            }
            case "string-append" -> {
                StringBuilder sb = new StringBuilder();
                for (Object a : args) {
                    if (!(a instanceof SchemeString s)) throw new EvalError(posStr() + "string-append: not a string");
                    sb.append(s.value());
                }
                return new SchemeString(sb.toString());
            }
            case "string-length" -> {
                requireArgCount(op, args, 1);
                if (!(args.get(0) instanceof SchemeString s)) throw new EvalError(posStr() + "string-length: not a string");
                return (long) s.length();
            }
            case "substring" -> {
                requireArgCount(op, args, 3);
                if (!(args.get(0) instanceof SchemeString s)) throw new EvalError(posStr() + "substring: not a string");
                int start = (int) requireLong(args.get(1));
                int end = (int) requireLong(args.get(2));
                return new SchemeString(s.value().substring(start, end));
            }
            case "string->number" -> {
                requireArgCount(op, args, 1);
                if (!(args.get(0) instanceof SchemeString s)) throw new EvalError(posStr() + "string->number: not a string");
                try {
                    return Long.parseLong(s.value());
                } catch (NumberFormatException e) {
                    return Boolean.FALSE;
                }
            }
            case "number->string" -> {
                requireArgCount(op, args, 1);
                if (!isNumber(args.get(0))) throw new EvalError(posStr() + "number->string: not a number");
                return new SchemeString(schemeToString(args.get(0)));
            }
            case "symbol->string" -> {
                requireArgCount(op, args, 1);
                if (!(args.get(0) instanceof String sym)) throw new EvalError(posStr() + "symbol->string: not a symbol");
                return new SchemeString(sym, true);
            }
            case "string->symbol" -> {
                requireArgCount(op, args, 1);
                if (!(args.get(0) instanceof SchemeString s)) throw new EvalError(posStr() + "string->symbol: not a string");
                return s.value();
            }
            case "string-ref" -> {
                requireArgCount(op, args, 2);
                if (!(args.get(0) instanceof SchemeString s)) throw new EvalError(posStr() + "string-ref: not a string");
                int idx = (int) requireLong(args.get(1));
                return new SchemeChar(s.charAt(idx));
            }
            case "string-set!" -> {
                requireArgCount(op, args, 3);
                if (!(args.get(0) instanceof SchemeString s)) throw new EvalError(posStr() + "string-set!: not a string");
                if (s.isImmutable()) throw new EvalError(posStr() + "string-set!: string is immutable");
                int idx = (int) requireLong(args.get(1));
                if (!(args.get(2) instanceof SchemeChar c)) throw new EvalError(posStr() + "string-set!: not a character");
                if (idx < 0 || idx >= s.length()) throw new EvalError(posStr() + "string-set!: index out of range");
                s.setChar(idx, c.value());
                return VOID;
            }
            case "string-copy" -> {
                requireArgCount(op, args, 1);
                if (!(args.get(0) instanceof SchemeString s)) throw new EvalError(posStr() + "string-copy: not a string");
                return s.copy();
            }
            case "string->list" -> {
                requireArgCount(op, args, 1);
                if (!(args.get(0) instanceof SchemeString s)) throw new EvalError(posStr() + "string->list: not a string");
                Object result = NIL;
                for (int i = s.length() - 1; i >= 0; i--) {
                    result = new Pair(new SchemeChar(s.charAt(i)), result);
                }
                return result;
            }
            case "list->string" -> {
                requireArgCount(op, args, 1);
                StringBuilder sb = new StringBuilder();
                Object cur = args.get(0);
                while (cur instanceof Pair pair) {
                    if (!(pair.car() instanceof SchemeChar c)) throw new EvalError(posStr() + "list->string: not a character");
                    sb.append(c.value());
                    cur = pair.cdr();
                }
                if (cur != NIL) throw new EvalError(posStr() + "list->string: not a proper list");
                return new SchemeString(sb.toString());
            }
            case "char->integer" -> {
                requireArgCount(op, args, 1);
                if (!(args.get(0) instanceof SchemeChar c)) throw new EvalError(posStr() + "char->integer: not a character");
                return (long) c.value();
            }
            case "integer->char" -> {
                requireArgCount(op, args, 1);
                long n = requireLong(args.get(0));
                return new SchemeChar((char) n);
            }
            case "apply" -> {
                if (args.size() < 2) throw new EvalError(posStr() + "apply: requires at least 2 arguments");
                Object proc = args.get(0);
                // Last arg must be a list; preceding args are prepended
                Object lastArg = args.get(args.size() - 1);
                List<Object> finalArgs = new ArrayList<>();
                for (int i = 1; i < args.size() - 1; i++) {
                    finalArgs.add(args.get(i));
                }
                // Unpack the last argument (a list)
                Object cur = lastArg;
                while (cur instanceof Pair p) {
                    finalArgs.add(p.car());
                    cur = p.cdr();
                }
                if (cur != NIL && cur != null) {
                    throw new EvalError(posStr() + "apply: last argument is not a proper list");
                }
                return applyProcedure(proc, finalArgs);
            }
            // L09 — Numeric utilities
            case "abs" -> {
                requireArgCount(op, args, 1);
                return Math.abs(requireLong(args.get(0)));
            }
            case "modulo" -> {
                requireArgCount(op, args, 2);
                long a = requireLong(args.get(0));
                long b = requireLong(args.get(1));
                if (b == 0) throw new EvalError(posStr() + "modulo: division by zero");
                long r = a % b;
                if (r != 0 && ((r > 0) != (b > 0))) r += b;
                return r;
            }
            case "remainder" -> {
                requireArgCount(op, args, 2);
                long a = requireLong(args.get(0));
                long b = requireLong(args.get(1));
                if (b == 0) throw new EvalError(posStr() + "remainder: division by zero");
                return a % b;
            }
            case "quotient" -> {
                requireArgCount(op, args, 2);
                long a = requireLong(args.get(0));
                long b = requireLong(args.get(1));
                if (b == 0) throw new EvalError(posStr() + "quotient: division by zero");
                // Truncate toward zero (Java default for long division)
                return a / b;
            }
            case "min" -> {
                if (args.isEmpty()) throw new EvalError(posStr() + "min: requires at least 1 argument");
                long result = requireLong(args.get(0));
                for (int i = 1; i < args.size(); i++) {
                    long v = requireLong(args.get(i));
                    if (v < result) result = v;
                }
                return result;
            }
            case "max" -> {
                if (args.isEmpty()) throw new EvalError(posStr() + "max: requires at least 1 argument");
                long result = requireLong(args.get(0));
                for (int i = 1; i < args.size(); i++) {
                    long v = requireLong(args.get(i));
                    if (v > result) result = v;
                }
                return result;
            }
            case "expt" -> {
                requireArgCount(op, args, 2);
                long base = requireLong(args.get(0));
                long exp = requireLong(args.get(1));
                long result = 1;
                for (long i = 0; i < exp; i++) result *= base;
                return result;
            }
            case "zero?" -> {
                requireArgCount(op, args, 1);
                return requireLong(args.get(0)) == 0;
            }
            case "positive?" -> {
                requireArgCount(op, args, 1);
                return requireLong(args.get(0)) > 0;
            }
            case "negative?" -> {
                requireArgCount(op, args, 1);
                return requireLong(args.get(0)) < 0;
            }
            case "odd?" -> {
                requireArgCount(op, args, 1);
                return requireLong(args.get(0)) % 2 != 0;
            }
            case "even?" -> {
                requireArgCount(op, args, 1);
                return requireLong(args.get(0)) % 2 == 0;
            }
            // L09 — List utilities
            case "list-ref" -> {
                requireArgCount(op, args, 2);
                Object lst = args.get(0);
                int idx = (int) requireLong(args.get(1));
                for (int i = 0; i < idx; i++) {
                    if (!(lst instanceof Pair p)) throw new EvalError(posStr() + "list-ref: index out of range");
                    lst = p.cdr();
                }
                if (!(lst instanceof Pair p)) throw new EvalError(posStr() + "list-ref: index out of range");
                return p.car();
            }
            case "list-tail" -> {
                requireArgCount(op, args, 2);
                Object lst = args.get(0);
                int idx = (int) requireLong(args.get(1));
                for (int i = 0; i < idx; i++) {
                    if (!(lst instanceof Pair p)) throw new EvalError(posStr() + "list-tail: index out of range");
                    lst = p.cdr();
                }
                return lst;
            }
            case "list?" -> {
                requireArgCount(op, args, 1);
                Object obj = args.get(0);
                while (obj instanceof Pair p) {
                    obj = p.cdr();
                }
                return obj == NIL;
            }
            case "assoc" -> {
                requireArgCount(op, args, 2);
                Object key = args.get(0);
                Object alist = args.get(1);
                while (alist instanceof Pair p) {
                    if (p.car() instanceof Pair entry) {
                        if (schemeEqual(key, entry.car())) return entry;
                    }
                    alist = p.cdr();
                }
                return Boolean.FALSE;
            }
            case "map" -> {
                if (args.size() < 2) throw new EvalError(posStr() + "map: requires at least 2 arguments");
                Object proc = args.get(0);
                List<Object> lists = new ArrayList<>();
                for (int i = 1; i < args.size(); i++) lists.add(args.get(i));
                List<Object> results = new ArrayList<>();
                while (true) {
                    // Check if any list is exhausted
                    boolean done = false;
                    for (Object l : lists) {
                        if (!(l instanceof Pair)) { done = true; break; }
                    }
                    if (done) break;
                    List<Object> callArgs = new ArrayList<>();
                    for (int i = 0; i < lists.size(); i++) {
                        Pair p = (Pair) lists.get(i);
                        callArgs.add(p.car());
                        lists.set(i, p.cdr());
                    }
                    results.add(applyProcedure(proc, callArgs));
                }
                Object result = NIL;
                for (int i = results.size() - 1; i >= 0; i--) {
                    result = new Pair(results.get(i), result);
                }
                return result;
            }
            // L09 — Equality
            case "eq?" -> {
                requireArgCount(op, args, 2);
                return schemeEq(args.get(0), args.get(1));
            }
            case "equal?" -> {
                requireArgCount(op, args, 2);
                return schemeEqual(args.get(0), args.get(1));
            }
            // L09 — Character utilities
            case "char-alphabetic?" -> {
                requireArgCount(op, args, 1);
                if (!(args.get(0) instanceof SchemeChar c)) throw new EvalError(posStr() + "char-alphabetic?: not a character");
                return Character.isLetter(c.value());
            }
            case "char-numeric?" -> {
                requireArgCount(op, args, 1);
                if (!(args.get(0) instanceof SchemeChar c)) throw new EvalError(posStr() + "char-numeric?: not a character");
                return Character.isDigit(c.value());
            }
            case "char-upcase" -> {
                requireArgCount(op, args, 1);
                if (!(args.get(0) instanceof SchemeChar c)) throw new EvalError(posStr() + "char-upcase: not a character");
                return new SchemeChar(Character.toUpperCase(c.value()));
            }
            case "char-downcase" -> {
                requireArgCount(op, args, 1);
                if (!(args.get(0) instanceof SchemeChar c)) throw new EvalError(posStr() + "char-downcase: not a character");
                return new SchemeChar(Character.toLowerCase(c.value()));
            }
            case "char=?" -> {
                requireArgCount(op, args, 2);
                if (!(args.get(0) instanceof SchemeChar a)) throw new EvalError(posStr() + "char=?: not a character");
                if (!(args.get(1) instanceof SchemeChar b)) throw new EvalError(posStr() + "char=?: not a character");
                return a.value() == b.value();
            }
            case "char<?" -> {
                requireArgCount(op, args, 2);
                if (!(args.get(0) instanceof SchemeChar a)) throw new EvalError(posStr() + "char<?: not a character");
                if (!(args.get(1) instanceof SchemeChar b)) throw new EvalError(posStr() + "char<?: not a character");
                return a.value() < b.value();
            }
            // L09 — String comparison and case
            case "string=?" -> {
                requireArgCount(op, args, 2);
                if (!(args.get(0) instanceof SchemeString a)) throw new EvalError(posStr() + "string=?: not a string");
                if (!(args.get(1) instanceof SchemeString b)) throw new EvalError(posStr() + "string=?: not a string");
                return a.value().equals(b.value());
            }
            case "string<?" -> {
                requireArgCount(op, args, 2);
                if (!(args.get(0) instanceof SchemeString a)) throw new EvalError(posStr() + "string<?: not a string");
                if (!(args.get(1) instanceof SchemeString b)) throw new EvalError(posStr() + "string<?: not a string");
                return a.value().compareTo(b.value()) < 0;
            }
            case "string-ci=?" -> {
                requireArgCount(op, args, 2);
                if (!(args.get(0) instanceof SchemeString a)) throw new EvalError(posStr() + "string-ci=?: not a string");
                if (!(args.get(1) instanceof SchemeString b)) throw new EvalError(posStr() + "string-ci=?: not a string");
                return a.value().equalsIgnoreCase(b.value());
            }
            case "string-upcase" -> {
                requireArgCount(op, args, 1);
                if (!(args.get(0) instanceof SchemeString s)) throw new EvalError(posStr() + "string-upcase: not a string");
                return new SchemeString(s.value().toUpperCase());
            }
            case "string-downcase" -> {
                requireArgCount(op, args, 1);
                if (!(args.get(0) instanceof SchemeString s)) throw new EvalError(posStr() + "string-downcase: not a string");
                return new SchemeString(s.value().toLowerCase());
            }
            // L11 — Exact arithmetic & rationals
            case "exact?" -> {
                requireArgCount(op, args, 1);
                Object a = args.get(0);
                return a instanceof Long || a instanceof SchemeRational;
            }
            case "inexact?" -> {
                requireArgCount(op, args, 1);
                return args.get(0) instanceof Double;
            }
            case "exact->inexact" -> {
                requireArgCount(op, args, 1);
                return toDouble(args.get(0));
            }
            case "inexact->exact" -> {
                requireArgCount(op, args, 1);
                Object a = args.get(0);
                if (a instanceof Long || a instanceof SchemeRational) return a;
                if (a instanceof Double dd) {
                    double d = dd;
                    // Convert double to exact rational
                    if (d == Math.floor(d) && !Double.isInfinite(d)) return (long) d;
                    // Use continued fraction / multiply out
                    long denom = 1;
                    double val = d;
                    while (val != Math.floor(val) && denom < 1000000000L) { val *= 2; denom *= 2; }
                    return makeExact(Math.round(val), denom);
                }
                throw new EvalError(posStr() + "inexact->exact: not a number");
            }
            case "numerator" -> {
                requireArgCount(op, args, 1);
                Object a = args.get(0);
                if (a instanceof Long l) return l;
                if (a instanceof SchemeRational r) return r.num;
                throw new EvalError(posStr() + "numerator: not a rational");
            }
            case "denominator" -> {
                requireArgCount(op, args, 1);
                Object a = args.get(0);
                if (a instanceof Long) return 1L;
                if (a instanceof SchemeRational r) return r.den;
                throw new EvalError(posStr() + "denominator: not a rational");
            }
            case "integer?" -> {
                requireArgCount(op, args, 1);
                Object a = args.get(0);
                if (a instanceof Long) return true;
                if (a instanceof SchemeRational) return false; // already simplified, so den != 1
                if (a instanceof Double d) return d == Math.floor(d) && !Double.isInfinite(d);
                return false;
            }
            case "rational?" -> {
                requireArgCount(op, args, 1);
                Object a = args.get(0);
                return a instanceof Long || a instanceof SchemeRational;
            }
            // L13
            case "procedure?" -> {
                requireArgCount(op, args, 1);
                Object a = args.get(0);
                return a instanceof Lambda || a instanceof CaseLambda || a instanceof Builtin
                    || a instanceof RecordConstructor || a instanceof RecordPredicate || a instanceof RecordAccessor;
            }
            // L14
            case "eqv?" -> {
                requireArgCount(op, args, 2);
                return schemeEqv(args.get(0), args.get(1));
            }
            case "vector" -> {
                return new SchemeVector(args.toArray());
            }
            case "make-vector" -> {
                if (args.size() < 1 || args.size() > 2) throw new EvalError(posStr() + "make-vector: expected 1-2 arguments");
                int len = (int) requireLong(args.get(0));
                Object fill = args.size() > 1 ? args.get(1) : 0L;
                Object[] elems = new Object[len];
                for (int i = 0; i < len; i++) elems[i] = fill;
                return new SchemeVector(elems);
            }
            case "vector-ref" -> {
                requireArgCount(op, args, 2);
                if (!(args.get(0) instanceof SchemeVector v)) throw new EvalError(posStr() + "vector-ref: not a vector");
                int idx = (int) requireLong(args.get(1));
                return v.ref(idx);
            }
            case "vector-set!" -> {
                requireArgCount(op, args, 3);
                if (!(args.get(0) instanceof SchemeVector v)) throw new EvalError(posStr() + "vector-set!: not a vector");
                int idx = (int) requireLong(args.get(1));
                v.set(idx, args.get(2));
                return VOID;
            }
            case "vector-length" -> {
                requireArgCount(op, args, 1);
                if (!(args.get(0) instanceof SchemeVector v)) throw new EvalError(posStr() + "vector-length: not a vector");
                return (long) v.length();
            }
            case "vector?" -> {
                requireArgCount(op, args, 1);
                return args.get(0) instanceof SchemeVector;
            }
            case "vector->list" -> {
                requireArgCount(op, args, 1);
                if (!(args.get(0) instanceof SchemeVector v)) throw new EvalError(posStr() + "vector->list: not a vector");
                Object result = NIL;
                for (int i = v.length() - 1; i >= 0; i--) {
                    result = new Pair(v.ref(i), result);
                }
                return result;
            }
            case "list->vector" -> {
                requireArgCount(op, args, 1);
                List<Object> elems = new ArrayList<>();
                Object cur = args.get(0);
                while (cur instanceof Pair p) {
                    elems.add(p.car());
                    cur = p.cdr();
                }
                return new SchemeVector(elems.toArray());
            }
            default -> throw new EvalError(posStr() + "unbound variable: " + op);
        }
    }

    // --- Macro expansion ---

    private Object unwrapDeep(Object obj) {
        if (obj instanceof Located loc) obj = loc.value();
        if (obj instanceof List<?> list) {
            List<Object> result = new ArrayList<>();
            for (Object item : list) result.add(unwrapDeep(item));
            return result;
        }
        return obj;
    }

    @SuppressWarnings("unchecked")
    private Object expandMacro(SyntaxTransformer st, List<?> form, Env useEnv) throws EvalError {
        List<Object> input = new ArrayList<>();
        for (Object o : form) input.add(unwrapDeep(o));

        for (Object[] clause : st.clauses) {
            List<Object> pattern = (List<Object>) clause[0];
            Object template = clause[1];
            Map<String, Object> bindings = new HashMap<>();
            if (matchPattern(pattern, input, st.literals, bindings)) {
                Set<String> patVars = new HashSet<>(bindings.keySet());
                Map<String, String> gensymMap = new HashMap<>();
                collectTemplateSymbols(template, patVars, st.literals, gensymMap);
                Object expanded = expandTemplate(template, bindings, gensymMap);
                // Inject definition-site bindings for gensym'd symbols
                for (Map.Entry<String, String> entry : gensymMap.entrySet()) {
                    try {
                        Object val = st.defEnv.lookup(entry.getKey());
                        useEnv.define(entry.getValue(), val);
                    } catch (EvalError e) { /* template-introduced var, skip */ }
                }
                return expanded;
            }
        }
        throw new EvalError(posStr() + "no matching pattern for macro");
    }

    @SuppressWarnings("unchecked")
    private boolean matchPattern(Object pattern, Object input, List<String> literals, Map<String, Object> bindings) {
        if (pattern instanceof String sym) {
            if (sym.equals("_")) return true;
            if (sym.equals("...")) return false;
            if (literals.contains(sym)) {
                return input instanceof String && input.equals(sym);
            }
            bindings.put(sym, input);
            return true;
        }
        if (pattern instanceof List<?> patList && input instanceof List<?> inList) {
            int ellipsisIdx = -1;
            for (int i = 0; i < patList.size(); i++) {
                if (patList.get(i) instanceof String s && s.equals("...")) {
                    ellipsisIdx = i;
                    break;
                }
            }
            if (ellipsisIdx == -1) {
                if (patList.size() != inList.size()) return false;
                for (int i = 0; i < patList.size(); i++) {
                    if (!matchPattern(patList.get(i), inList.get(i), literals, bindings)) return false;
                }
                return true;
            } else {
                int beforeEllipsis = ellipsisIdx - 1;
                int afterEllipsis = patList.size() - ellipsisIdx - 1;
                if (inList.size() < beforeEllipsis + afterEllipsis) return false;
                for (int i = 0; i < beforeEllipsis; i++) {
                    if (!matchPattern(patList.get(i), inList.get(i), literals, bindings)) return false;
                }
                int repeatCount = inList.size() - beforeEllipsis - afterEllipsis;
                Object repeatedPat = patList.get(beforeEllipsis);
                if (repeatedPat instanceof String sym) {
                    List<Object> repeated = new ArrayList<>();
                    for (int i = 0; i < repeatCount; i++) repeated.add(inList.get(beforeEllipsis + i));
                    bindings.put(sym, new EllipsisList(repeated));
                }
                for (int i = 0; i < afterEllipsis; i++) {
                    if (!matchPattern(patList.get(ellipsisIdx + 1 + i), inList.get(inList.size() - afterEllipsis + i), literals, bindings)) return false;
                }
                return true;
            }
        }
        if (pattern instanceof Long || pattern instanceof Boolean) {
            return pattern.equals(input);
        }
        return false;
    }

    private void collectTemplateSymbols(Object template, Set<String> patVars, List<String> literals, Map<String, String> gensymMap) {
        if (template instanceof String sym) {
            if (!patVars.contains(sym) && !SPECIAL_FORMS.contains(sym)
                    && !sym.equals("...") && !literals.contains(sym) && !gensymMap.containsKey(sym)) {
                gensymMap.put(sym, gensym(sym));
            }
        } else if (template instanceof List<?> list) {
            for (Object item : list) collectTemplateSymbols(item, patVars, literals, gensymMap);
        }
    }

    @SuppressWarnings("unchecked")
    private Object expandTemplate(Object template, Map<String, Object> bindings, Map<String, String> gensymMap) {
        if (template instanceof String sym) {
            if (bindings.containsKey(sym) && !(bindings.get(sym) instanceof EllipsisList)) {
                return bindings.get(sym);
            }
            if (gensymMap.containsKey(sym)) return gensymMap.get(sym);
            return sym;
        }
        if (template instanceof List<?> list) {
            List<Object> result = new ArrayList<>();
            for (int i = 0; i < list.size(); i++) {
                Object elem = list.get(i);
                if (i + 1 < list.size() && list.get(i + 1) instanceof String s && s.equals("...")) {
                    String varName = findEllipsisVar(elem, bindings);
                    if (varName != null && bindings.get(varName) instanceof EllipsisList el) {
                        for (Object val : el.elements()) {
                            Map<String, Object> newBindings = new HashMap<>(bindings);
                            newBindings.put(varName, val);
                            result.add(expandTemplate(elem, newBindings, gensymMap));
                        }
                    }
                    i++; // skip "..."
                } else {
                    result.add(expandTemplate(elem, bindings, gensymMap));
                }
            }
            return result;
        }
        return template;
    }

    private String findEllipsisVar(Object template, Map<String, Object> bindings) {
        if (template instanceof String sym) {
            if (bindings.containsKey(sym) && bindings.get(sym) instanceof EllipsisList) return sym;
        } else if (template instanceof List<?> list) {
            for (Object item : list) {
                String found = findEllipsisVar(item, bindings);
                if (found != null) return found;
            }
        }
        return null;
    }

    // --- Helpers ---

    private Object quoteDatum(Object datum) {
        if (datum instanceof Located loc) {
            datum = loc.value();
        }
        if (datum instanceof List<?> list) {
            Object result = NIL;
            for (int i = list.size() - 1; i >= 0; i--) {
                result = new Pair(quoteDatum(list.get(i)), result);
            }
            return result;
        }
        return datum;
    }

    private boolean schemeEq(Object a, Object b) {
        if (a == b) return true;
        if (a instanceof Long && b instanceof Long) return a.equals(b);
        if (a instanceof Double && b instanceof Double) return a.equals(b);
        if (a instanceof SchemeRational ra && b instanceof SchemeRational rb) return ra.num == rb.num && ra.den == rb.den;
        if (a instanceof Boolean && b instanceof Boolean) return a.equals(b);
        if (a instanceof SchemeChar ca && b instanceof SchemeChar cb) return ca.value() == cb.value();
        // Symbols are Java Strings — use equals
        if (a instanceof String && b instanceof String) return a.equals(b);
        return false;
    }

    private boolean schemeEqv(Object a, Object b) {
        if (a == b) return true;
        if (a instanceof Long && b instanceof Long) return a.equals(b);
        if (a instanceof Double && b instanceof Double) return a.equals(b);
        if (a instanceof SchemeRational ra && b instanceof SchemeRational rb) return ra.num == rb.num && ra.den == rb.den;
        if (a instanceof Boolean && b instanceof Boolean) return a.equals(b);
        if (a instanceof SchemeChar ca && b instanceof SchemeChar cb) return ca.value() == cb.value();
        if (a instanceof String && b instanceof String) return a.equals(b);
        return false;
    }

    private boolean schemeEqual(Object a, Object b) {
        if (schemeEq(a, b)) return true;
        if (a instanceof SchemeString sa && b instanceof SchemeString sb) return sa.value().equals(sb.value());
        if (a instanceof Pair pa && b instanceof Pair pb) {
            return schemeEqual(pa.car(), pb.car()) && schemeEqual(pa.cdr(), pb.cdr());
        }
        if (a instanceof SchemeVector va && b instanceof SchemeVector vb) {
            if (va.length() != vb.length()) return false;
            for (int i = 0; i < va.length(); i++) {
                if (!schemeEqual(va.ref(i), vb.ref(i))) return false;
            }
            return true;
        }
        return false;
    }

    private int numCompare(Object a, Object b) throws EvalError {
        if (!isNumber(a) || !isNumber(b)) throw new EvalError(posStr() + "comparison: not a number");
        // If both exact, compare as rationals to avoid precision loss
        if ((a instanceof Long || a instanceof SchemeRational) && (b instanceof Long || b instanceof SchemeRational)) {
            long[] ra = toRational(a);
            long[] rb = toRational(b);
            return Long.compare(ra[0] * rb[1], rb[0] * ra[1]);
        }
        return Double.compare(toDouble(a), toDouble(b));
    }

    private boolean isFalse(Object val) {
        return val instanceof Boolean b && !b;
    }

    private long requireLong(Object val) throws EvalError {
        if (val instanceof Long l) return l;
        throw new EvalError(posStr() + "expected number, got: " + schemeToString(val));
    }

    private void requireArgCount(String op, List<Object> args, int expected) throws EvalError {
        if (args.size() != expected) {
            throw new EvalError(posStr() + op + ": expected " + expected + " arguments, got " + args.size());
        }
    }

    private String displayString(Object val) {
        if (val instanceof SchemeString s) return s.value();
        if (val instanceof SchemeChar c) return String.valueOf(c.value());
        if (val instanceof SchemeVector v) {
            StringBuilder sb = new StringBuilder("#(");
            for (int i = 0; i < v.length(); i++) {
                if (i > 0) sb.append(" ");
                sb.append(displayString(v.ref(i)));
            }
            sb.append(")");
            return sb.toString();
        }
        return schemeToString(val);
    }

    private String schemeToString(Object val) {
        if (val instanceof Long l) return l.toString();
        if (val instanceof Double d) {
            if (d == Math.floor(d) && !Double.isInfinite(d) && Math.abs(d) < 1e15) {
                // Format as integer-like: 5.0
                return String.valueOf(d);
            }
            return String.valueOf(d);
        }
        if (val instanceof SchemeRational r) return r.num + "/" + r.den;
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
        if (val instanceof SchemeVector v) {
            StringBuilder sb = new StringBuilder("#(");
            for (int i = 0; i < v.length(); i++) {
                if (i > 0) sb.append(" ");
                sb.append(schemeToString(v.ref(i)));
            }
            sb.append(")");
            return sb.toString();
        }
        if (val == NIL) return "()";
        if (val instanceof Pair) {
            StringBuilder sb = new StringBuilder("(");
            Object cur = val;
            boolean first = true;
            while (cur instanceof Pair p) {
                if (!first) sb.append(" ");
                first = false;
                sb.append(schemeToString(p.car()));
                cur = p.cdr();
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
        return String.valueOf(val);
    }
}
