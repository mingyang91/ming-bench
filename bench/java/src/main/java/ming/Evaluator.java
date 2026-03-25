package ming;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

public class Evaluator {

    @FunctionalInterface
    interface BuiltinFn {
        Object apply(List<Object> args) throws EvalError;
    }

    record Builtin(String name, BuiltinFn fn) {
        Object apply(List<Object> args) throws EvalError {
            return fn.apply(args);
        }
    }

    static class SchemeString {
        private final char[] chars;
        SchemeString(String value) { this.chars = value.toCharArray(); }
        SchemeString(char[] chars) { this.chars = chars; }
        String value() { return new String(chars); }
        int length() { return chars.length; }
        char charAt(int i) { return chars[i]; }
        void setChar(int i, char c) { chars[i] = c; }
    }

    record SchemeChar(char value) {}

    record Lambda(List<String> params, String restParam, List<Object> body, Env env) {}

    // Source position tracking
    record Pos(int line, int col) {
        @Override public String toString() { return line + ":" + col; }
    }

    // Parsed list that carries source position
    static class SExpr extends ArrayList<Object> {
        final Pos pos;
        SExpr(Pos pos) { super(); this.pos = pos; }
    }

    // Token with position
    record Token(Object value, Pos pos) {}

    static class Cons {
        Object car;
        Object cdr;
        Cons(Object car, Object cdr) { this.car = car; this.cdr = cdr; }
    }

    static class SyntaxRules {
        final List<String> literals;
        final List<List<Object>> patterns;
        final List<Object> templates;
        final Env defEnv;
        SyntaxRules(List<String> literals, List<List<Object>> patterns, List<Object> templates, Env defEnv) {
            this.literals = literals;
            this.patterns = patterns;
            this.templates = templates;
            this.defEnv = defEnv;
        }
    }

    static final Object NIL = new Object() {
        @Override public String toString() { return "()"; }
    };

    static final Object VOID = new Object() {
        @Override public String toString() { return "#<void>"; }
    };

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

    private static final java.util.Set<String> SPECIAL_FORMS = java.util.Set.of(
        "quote", "if", "define", "lambda", "and", "or", "let", "set!", "begin", "cond",
        "define-syntax", "syntax-rules", "else", "let*", "letrec"
    );

    private int gensymCounter = 0;
    private String gensym(String base) {
        return "##" + base + "_" + (gensymCounter++);
    }

    private final Env globalEnv = new Env(null);
    private StringBuilder outputBuffer = new StringBuilder();

    public Evaluator() {
        new Builtins(globalEnv, this).registerAll();
    }

    void appendOutput(String s) {
        outputBuffer.append(s);
    }

    public String evalStr(String input) throws EvalError {
        List<Token> tokens = tokenize(input);
        int[] pos = {0};
        Object lastResult = null;
        while (pos[0] < tokens.size()) {
            Object expr = parse(tokens, pos);
            lastResult = eval(expr, globalEnv);
        }
        if (lastResult == null) {
            throw new EvalError("no expression");
        }
        if (lastResult == VOID) return "#<void>";
        return schemeToString(lastResult);
    }

    public EvalResult evalStrWithOutput(String input) throws EvalError {
        outputBuffer = new StringBuilder();
        List<Token> tokens = tokenize(input);
        int[] pos = {0};
        Object lastResult = null;
        while (pos[0] < tokens.size()) {
            Object expr = parse(tokens, pos);
            lastResult = eval(expr, globalEnv);
        }
        String result = "";
        if (lastResult != null && lastResult != VOID) {
            result = schemeToString(lastResult);
        }
        return new EvalResult(result, outputBuffer.toString());
    }

    // --- Tokenizer ---

    private int lineNum, colNum;

    private Pos posAt(int line, int col) { return new Pos(line, col); }

    private boolean isDelimiter(char c) {
        return Character.isWhitespace(c) || c == '(' || c == ')' || c == '"' || c == ';' || c == '\'';
    }

    private List<Token> tokenize(String input) throws EvalError {
        List<Token> tokens = new ArrayList<>();
        int i = 0;
        lineNum = 1;
        colNum = 1;
        while (i < input.length()) {
            char c = input.charAt(i);
            if (c == '\n') {
                lineNum++;
                colNum = 1;
                i++;
            } else if (Character.isWhitespace(c)) {
                colNum++;
                i++;
            } else if (c == ';') {
                while (i < input.length() && input.charAt(i) != '\n') { i++; colNum++; }
            } else if (c == '\'') {
                tokens.add(new Token("'", posAt(lineNum, colNum)));
                i++; colNum++;
            } else if (c == '(') {
                tokens.add(new Token("(", posAt(lineNum, colNum)));
                i++; colNum++;
            } else if (c == ')') {
                tokens.add(new Token(")", posAt(lineNum, colNum)));
                i++; colNum++;
            } else if (c == '"') {
                Pos strPos = posAt(lineNum, colNum);
                StringBuilder sb = new StringBuilder();
                i++; colNum++;
                while (i < input.length() && input.charAt(i) != '"') {
                    if (input.charAt(i) == '\\') {
                        i++; colNum++;
                        if (i < input.length()) {
                            switch (input.charAt(i)) {
                                case 'n' -> sb.append('\n');
                                case 't' -> sb.append('\t');
                                case '\\' -> sb.append('\\');
                                case '"' -> sb.append('"');
                                default -> { sb.append('\\'); sb.append(input.charAt(i)); }
                            }
                        }
                    } else {
                        sb.append(input.charAt(i));
                    }
                    if (input.charAt(i) == '\n') { lineNum++; colNum = 1; } else { colNum++; }
                    i++;
                }
                if (i < input.length()) { i++; colNum++; }
                tokens.add(new Token(new SchemeString(sb.toString()), strPos));
            } else if (c == '#') {
                Pos hPos = posAt(lineNum, colNum);
                if (i + 1 < input.length()) {
                    char next = input.charAt(i + 1);
                    if (next == 't') {
                        // Check it's not followed by an identifier char
                        if (i + 2 >= input.length() || isDelimiter(input.charAt(i + 2))) {
                            tokens.add(new Token(Boolean.TRUE, hPos));
                            i += 2; colNum += 2;
                        } else {
                            throw new EvalError("unexpected #" + next + " at " + hPos);
                        }
                    } else if (next == 'f') {
                        if (i + 2 >= input.length() || isDelimiter(input.charAt(i + 2))) {
                            tokens.add(new Token(Boolean.FALSE, hPos));
                            i += 2; colNum += 2;
                        } else {
                            throw new EvalError("unexpected #" + next + " at " + hPos);
                        }
                    } else if (next == '\\') {
                        // Character literal
                        i += 2; colNum += 2;
                        if (i >= input.length()) throw new EvalError("unexpected end after #\\ at " + hPos);
                        // Try named characters first
                        if (i + 4 < input.length() && input.substring(i, i + 5).equals("space") &&
                                (i + 5 >= input.length() || isDelimiter(input.charAt(i + 5)))) {
                            tokens.add(new Token(new SchemeChar(' '), hPos));
                            i += 5; colNum += 5;
                        } else if (i + 6 < input.length() && input.substring(i, i + 7).equals("newline") &&
                                (i + 7 >= input.length() || isDelimiter(input.charAt(i + 7)))) {
                            tokens.add(new Token(new SchemeChar('\n'), hPos));
                            i += 7; colNum += 7;
                        } else if (i + 2 < input.length() && input.substring(i, i + 3).equals("tab") &&
                                (i + 3 >= input.length() || isDelimiter(input.charAt(i + 3)))) {
                            tokens.add(new Token(new SchemeChar('\t'), hPos));
                            i += 3; colNum += 3;
                        } else {
                            tokens.add(new Token(new SchemeChar(input.charAt(i)), hPos));
                            i++; colNum++;
                        }
                    } else {
                        throw new EvalError("unexpected #" + next + " at " + hPos);
                    }
                } else {
                    throw new EvalError("unexpected end after # at " + hPos);
                }
            } else {
                Pos tokPos = posAt(lineNum, colNum);
                StringBuilder sb = new StringBuilder();
                while (i < input.length()) {
                    char ch = input.charAt(i);
                    if (Character.isWhitespace(ch) || ch == '(' || ch == ')' || ch == '"' || ch == ';' || ch == '\'') break;
                    sb.append(ch);
                    i++; colNum++;
                }
                String tok = sb.toString();
                try {
                    tokens.add(new Token(Long.parseLong(tok), tokPos));
                } catch (NumberFormatException e) {
                    tokens.add(new Token(tok, tokPos));
                }
            }
        }
        return tokens;
    }

    // --- Parser ---

    private Object parse(List<Token> tokens, int[] pos) throws EvalError {
        if (pos[0] >= tokens.size()) {
            throw new EvalError("unexpected end of input");
        }
        Token token = tokens.get(pos[0]);
        if ("'".equals(token.value())) {
            pos[0]++;
            Object quoted = parse(tokens, pos);
            SExpr quoteExpr = new SExpr(token.pos());
            quoteExpr.add("quote");
            quoteExpr.add(quoted);
            return quoteExpr;
        }
        if ("(".equals(token.value())) {
            pos[0]++;
            SExpr list = new SExpr(token.pos());
            while (pos[0] < tokens.size() && !")".equals(tokens.get(pos[0]).value())) {
                list.add(parse(tokens, pos));
            }
            if (pos[0] >= tokens.size()) {
                throw new EvalError("missing closing paren at " + token.pos());
            }
            pos[0]++;
            return list;
        } else if (")".equals(token.value())) {
            throw new EvalError("unexpected ) at " + token.pos());
        } else {
            pos[0]++;
            return token;
        }
    }

    // --- Evaluator ---

    // Extract position from an expression (Token or SExpr)
    private Pos posOf(Object expr) {
        if (expr instanceof Token t) return t.pos();
        if (expr instanceof SExpr s) return s.pos;
        return null;
    }

    private String posStr(Pos p) {
        return p != null ? " at " + p : "";
    }

    // Unwrap Token to get the raw value (for comparisons)
    private Object unwrap(Object expr) {
        if (expr instanceof Token t) return t.value();
        return expr;
    }

    @SuppressWarnings("unchecked")
    private Object eval(Object expr, Env env) throws EvalError {
        Object raw = unwrap(expr);
        Pos pos = posOf(expr);

        if (raw instanceof Long || raw instanceof Boolean || raw instanceof SchemeString || raw instanceof SchemeChar) {
            return raw;
        }
        if (raw instanceof String sym) {
            try {
                return env.lookup(sym);
            } catch (EvalError e) {
                throw new EvalError(e.getMessage() + posStr(pos));
            }
        }
        if (raw instanceof List<?> list) {
            if (list.isEmpty()) {
                throw new EvalError("empty application" + posStr(pos));
            }
            Object headExpr = list.get(0);
            Object head = unwrap(headExpr);

            if (head instanceof String sym) {
                switch (sym) {
                    case "quote" -> {
                        if (list.size() != 2) throw new EvalError("quote requires 1 argument" + posStr(pos));
                        return javaToScheme(list.get(1));
                    }
                    case "if" -> {
                        if (list.size() < 3 || list.size() > 4)
                            throw new EvalError("if requires 2 or 3 arguments" + posStr(pos));
                        Object cond = eval(list.get(1), env);
                        if (!isFalse(cond)) {
                            return eval(list.get(2), env);
                        } else if (list.size() == 4) {
                            return eval(list.get(3), env);
                        }
                        return VOID;
                    }
                    case "define" -> {
                        if (list.size() < 3) throw new EvalError("define requires at least 2 arguments" + posStr(pos));
                        Object target = unwrap(list.get(1));
                        if (target instanceof String name) {
                            Object val = eval(list.get(2), env);
                            env.define(name, val);
                            return VOID;
                        } else if (target instanceof List<?> sig) {
                            if (sig.isEmpty() || !(unwrap(sig.get(0)) instanceof String name))
                                throw new EvalError("invalid define" + posStr(pos));
                            List<String> params = new ArrayList<>();
                            String restParam = null;
                            for (int i = 1; i < sig.size(); i++) {
                                String p = unwrap(sig.get(i)) instanceof String s ? s : null;
                                if (p == null) throw new EvalError("parameter must be a symbol" + posStr(pos));
                                if (".".equals(p)) {
                                    if (i + 1 >= sig.size()) throw new EvalError("missing rest parameter after ." + posStr(pos));
                                    restParam = unwrap(sig.get(i + 1)) instanceof String rp ? rp : null;
                                    if (restParam == null) throw new EvalError("rest parameter must be a symbol" + posStr(pos));
                                    break;
                                }
                                params.add(p);
                            }
                            List<Object> body = new ArrayList<>(list.subList(2, list.size()));
                            env.define(name, new Lambda(params, restParam, body, env));
                            return VOID;
                        }
                        throw new EvalError("invalid define" + posStr(pos));
                    }
                    case "lambda" -> {
                        if (list.size() < 3) throw new EvalError("lambda requires params and body" + posStr(pos));
                        Object paramSpec = unwrap(list.get(1));
                        if (!(paramSpec instanceof List<?> paramList))
                            throw new EvalError("lambda params must be a list" + posStr(pos));
                        List<String> params = new ArrayList<>();
                        String restParam = null;
                        for (int pi = 0; pi < paramList.size(); pi++) {
                            String s = unwrap(paramList.get(pi)) instanceof String str ? str : null;
                            if (s == null) throw new EvalError("parameter must be a symbol" + posStr(pos));
                            if (".".equals(s)) {
                                if (pi + 1 >= paramList.size()) throw new EvalError("missing rest parameter after ." + posStr(pos));
                                restParam = unwrap(paramList.get(pi + 1)) instanceof String rp ? rp : null;
                                if (restParam == null) throw new EvalError("rest parameter must be a symbol" + posStr(pos));
                                break;
                            }
                            params.add(s);
                        }
                        List<Object> body = new ArrayList<>(list.subList(2, list.size()));
                        return new Lambda(params, restParam, body, env);
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
                        if (list.size() < 3) throw new EvalError("let requires bindings and body" + posStr(pos));
                        Object second = unwrap(list.get(1));
                        if (second instanceof String loopName) {
                            if (list.size() < 4) throw new EvalError("named let requires bindings and body" + posStr(pos));
                            Object bindingsRaw = unwrap(list.get(2));
                            if (!(bindingsRaw instanceof List<?> bindings))
                                throw new EvalError("let bindings must be a list" + posStr(pos));
                            List<String> params = new ArrayList<>();
                            List<Object> inits = new ArrayList<>();
                            for (Object b : bindings) {
                                Object bRaw = unwrap(b);
                                if (!(bRaw instanceof List<?> binding) || binding.size() != 2)
                                    throw new EvalError("invalid let binding" + posStr(pos));
                                if (!(unwrap(binding.get(0)) instanceof String pname))
                                    throw new EvalError("let binding name must be a symbol" + posStr(pos));
                                params.add(pname);
                                inits.add(eval(binding.get(1), env));
                            }
                            List<Object> body = new ArrayList<>(list.subList(3, list.size()));
                            Env letEnv = new Env(env);
                            Lambda loopFn = new Lambda(params, null, body, letEnv);
                            letEnv.define(loopName, loopFn);
                            return applyProc(loopFn, inits, pos);
                        }
                        if (!(second instanceof List<?> bindings))
                            throw new EvalError("let bindings must be a list" + posStr(pos));
                        Env letEnv = new Env(env);
                        for (Object b : bindings) {
                            Object bRaw = unwrap(b);
                            if (!(bRaw instanceof List<?> binding) || binding.size() != 2)
                                throw new EvalError("invalid let binding" + posStr(pos));
                            if (!(unwrap(binding.get(0)) instanceof String name))
                                throw new EvalError("let binding name must be a symbol" + posStr(pos));
                            letEnv.define(name, eval(binding.get(1), env));
                        }
                        Object result = VOID;
                        for (int i = 2; i < list.size(); i++) {
                            result = eval(list.get(i), letEnv);
                        }
                        return result;
                    }
                    case "set!" -> {
                        if (list.size() != 3) throw new EvalError("set! requires 2 arguments" + posStr(pos));
                        Object nameRaw = unwrap(list.get(1));
                        if (!(nameRaw instanceof String name))
                            throw new EvalError("set!: first argument must be a symbol" + posStr(pos));
                        Object val = eval(list.get(2), env);
                        env.set(name, val);
                        return VOID;
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
                            Object clauseRaw = unwrap(list.get(i));
                            if (!(clauseRaw instanceof List<?> clause) || clause.isEmpty())
                                throw new EvalError("invalid cond clause" + posStr(pos));
                            if ("else".equals(unwrap(clause.get(0)))) {
                                Object result = VOID;
                                for (int j = 1; j < clause.size(); j++) {
                                    result = eval(clause.get(j), env);
                                }
                                return result;
                            }
                            Object test = eval(clause.get(0), env);
                            if (!isFalse(test)) {
                                Object result = test;
                                for (int j = 1; j < clause.size(); j++) {
                                    result = eval(clause.get(j), env);
                                }
                                return result;
                            }
                        }
                        return VOID;
                    }
                    case "define-syntax" -> {
                        if (list.size() != 3) throw new EvalError("define-syntax requires 2 arguments" + posStr(pos));
                        Object nameRaw = unwrap(list.get(1));
                        if (!(nameRaw instanceof String macroName))
                            throw new EvalError("define-syntax: name must be a symbol" + posStr(pos));
                        Object rulesExpr = unwrap(list.get(2));
                        if (!(rulesExpr instanceof List<?> rulesList) || rulesList.size() < 2 || !"syntax-rules".equals(unwrap(rulesList.get(0))))
                            throw new EvalError("define-syntax: expected syntax-rules" + posStr(pos));
                        Object literalsExpr = unwrap(rulesList.get(1));
                        List<String> literals = new ArrayList<>();
                        if (literalsExpr instanceof List<?> litList) {
                            for (Object lit : litList) {
                                if (unwrap(lit) instanceof String s) literals.add(s);
                            }
                        }
                        List<List<Object>> patterns = new ArrayList<>();
                        List<Object> templates = new ArrayList<>();
                        for (int i = 2; i < rulesList.size(); i++) {
                            Object rule = unwrap(rulesList.get(i));
                            if (!(rule instanceof List<?> rulePair) || rulePair.size() != 2)
                                throw new EvalError("syntax-rules: each rule must be (pattern template)" + posStr(pos));
                            Object pat = unwrap(rulePair.get(0));
                            if (!(pat instanceof List<?> patList))
                                throw new EvalError("syntax-rules: pattern must be a list" + posStr(pos));
                            @SuppressWarnings("unchecked")
                            List<Object> patObjList = (List<Object>) patList;
                            patterns.add(patObjList);
                            templates.add(rulePair.get(1));
                        }
                        env.define(macroName, new SyntaxRules(literals, patterns, templates, env));
                        return VOID;
                    }
                }
            }

            // Check for macro application
            if (head instanceof String macroSym) {
                try {
                    Object val = env.lookup(macroSym);
                    if (val instanceof SyntaxRules macro) {
                        return expandAndEvalMacro(macro, list, env, pos);
                    }
                } catch (EvalError ignore) {}
            }

            // Procedure call
            Object proc = eval(headExpr, env);
            List<Object> args = new ArrayList<>();
            for (int i = 1; i < list.size(); i++) {
                args.add(eval(list.get(i), env));
            }
            return applyProc(proc, args, pos);
        }
        throw new EvalError("cannot eval: " + expr);
    }

    boolean isFalse(Object val) {
        return val instanceof Boolean b && !b;
    }

    boolean schemeEqual(Object a, Object b) {
        if (a == b) return true;
        if (a instanceof Long la && b instanceof Long lb) return la.equals(lb);
        if (a instanceof Boolean ba && b instanceof Boolean bb) return ba.equals(bb);
        if (a instanceof String sa && b instanceof String sb) return sa.equals(sb);
        if (a instanceof SchemeString sa && b instanceof SchemeString sb) return sa.value().equals(sb.value());
        if (a instanceof SchemeChar ca && b instanceof SchemeChar cb) return ca.value() == cb.value();
        if (a == NIL && b == NIL) return true;
        if (a instanceof Cons ca && b instanceof Cons cb) {
            return schemeEqual(ca.car, cb.car) && schemeEqual(ca.cdr, cb.cdr);
        }
        return false;
    }

    Object applyProc(Object proc, List<Object> args, Pos pos) throws EvalError {
        if (proc instanceof Builtin b) {
            try {
                return b.apply(args);
            } catch (EvalError e) {
                // Add position if not already present
                String msg = e.getMessage();
                if (pos != null && !msg.matches(".*\\d+:\\d+.*")) {
                    throw new EvalError(msg + " at " + pos);
                }
                throw e;
            }
        }
        if (proc instanceof Lambda lam) {
            if (lam.restParam() != null) {
                if (args.size() < lam.params().size()) {
                    throw new EvalError("expected at least " + lam.params().size() + " arguments, got " + args.size() + posStr(pos));
                }
            } else if (args.size() != lam.params().size()) {
                throw new EvalError("expected " + lam.params().size() + " arguments, got " + args.size() + posStr(pos));
            }
            Env callEnv = new Env(lam.env());
            for (int i = 0; i < lam.params().size(); i++) {
                callEnv.define(lam.params().get(i), args.get(i));
            }
            if (lam.restParam() != null) {
                Object rest = NIL;
                for (int i = args.size() - 1; i >= lam.params().size(); i--) {
                    rest = new Cons(args.get(i), rest);
                }
                callEnv.define(lam.restParam(), rest);
            }
            Object result = VOID;
            for (Object bodyExpr : lam.body()) {
                result = eval(bodyExpr, callEnv);
            }
            return result;
        }
        throw new EvalError("not a procedure: " + schemeToString(proc) + posStr(pos));
    }

    long requireLong(Object val) throws EvalError {
        if (val instanceof Long l) return l;
        throw new EvalError("expected number, got: " + schemeToString(val));
    }

    void requireArgCount(List<Object> args, int n, String name) throws EvalError {
        if (args.size() != n) {
            throw new EvalError(name + " requires " + n + " arguments, got " + args.size());
        }
    }

    // --- Macro expansion ---

    @SuppressWarnings("unchecked")
    private Object expandAndEvalMacro(SyntaxRules macro, List<?> form, Env env, Pos pos) throws EvalError {
        for (int i = 0; i < macro.patterns.size(); i++) {
            List<Object> pattern = macro.patterns.get(i);
            Map<String, Object> bindings = new HashMap<>();
            java.util.Set<String> patVars = new java.util.HashSet<>();
            java.util.Set<String> ellipsisVars = new java.util.HashSet<>();
            collectPatternVars(pattern, 1, macro.literals, patVars, ellipsisVars);
            if (matchElements(pattern, 1, form, 1, macro.literals, patVars, bindings)) {
                Map<String, String> renames = new HashMap<>();
                Object expanded = expandTemplate(macro.templates.get(i), bindings, ellipsisVars, patVars, renames);
                Env macroEnv = new Env(env);
                for (Map.Entry<String, String> entry : renames.entrySet()) {
                    try {
                        macroEnv.define(entry.getValue(), macro.defEnv.lookup(entry.getKey()));
                    } catch (EvalError ignore) {
                        // Not in defEnv - it's a fresh binding introduced by the macro
                    }
                }
                return eval(expanded, macroEnv);
            }
        }
        throw new EvalError("no matching syntax-rules pattern" + posStr(pos));
    }

    private void collectPatternVars(List<Object> pattern, int start, List<String> literals,
                                     java.util.Set<String> patVars, java.util.Set<String> ellipsisVars) {
        for (int i = start; i < pattern.size(); i++) {
            Object raw = unwrap(pattern.get(i));
            if (raw instanceof String sym && !literals.contains(sym) && !"...".equals(sym)) {
                patVars.add(sym);
                if (i + 1 < pattern.size() && "...".equals(unwrap(pattern.get(i + 1)))) {
                    ellipsisVars.add(sym);
                }
            } else if (raw instanceof List<?> nested) {
                collectPatternVars((List<Object>) nested, 0, literals, patVars, ellipsisVars);
            }
        }
    }

    private boolean matchElements(List<Object> pattern, int pi, List<?> input, int ii,
                                   List<String> literals, java.util.Set<String> patVars,
                                   Map<String, Object> bindings) {
        while (pi < pattern.size()) {
            Object patRaw = unwrap(pattern.get(pi));
            if (pi + 1 < pattern.size() && "...".equals(unwrap(pattern.get(pi + 1)))) {
                if (!(patRaw instanceof String var)) return false;
                int remaining = 0;
                for (int k = pi + 2; k < pattern.size(); k++) {
                    if (!"...".equals(unwrap(pattern.get(k)))) remaining++;
                }
                int available = input.size() - ii - remaining;
                if (available < 0) return false;
                List<Object> collected = new ArrayList<>();
                for (int j = 0; j < available; j++) {
                    collected.add(input.get(ii + j));
                }
                bindings.put(var, collected);
                ii += available;
                pi += 2;
            } else {
                if (ii >= input.size()) return false;
                if (patRaw instanceof String sym && patVars.contains(sym)) {
                    bindings.put(sym, input.get(ii));
                } else {
                    Object inRaw = unwrap(input.get(ii));
                    if (!java.util.Objects.equals(patRaw, inRaw)) return false;
                }
                pi++;
                ii++;
            }
        }
        return ii == input.size();
    }

    @SuppressWarnings("unchecked")
    private Object expandTemplate(Object template, Map<String, Object> bindings,
                                   java.util.Set<String> ellipsisVars, java.util.Set<String> patVars,
                                   Map<String, String> renames) {
        Object raw = unwrap(template);
        if (raw instanceof String sym) {
            if (patVars.contains(sym)) {
                return bindings.get(sym);
            }
            if ("...".equals(sym)) return template;
            if (!SPECIAL_FORMS.contains(sym)) {
                return renames.computeIfAbsent(sym, k -> gensym(k));
            }
            return sym;
        }
        if (raw instanceof List<?> list) {
            List<Object> result = new ArrayList<>();
            for (int i = 0; i < list.size(); i++) {
                if (i + 1 < list.size() && "...".equals(unwrap(list.get(i + 1)))) {
                    String var = findEllipsisVar(list.get(i), ellipsisVars);
                    if (var != null && bindings.get(var) instanceof List<?> vals) {
                        for (Object v : vals) {
                            Map<String, Object> singleBindings = new HashMap<>(bindings);
                            singleBindings.put(var, v);
                            result.add(expandTemplate(list.get(i), singleBindings,
                                    java.util.Set.of(), patVars, renames));
                        }
                    }
                    i++; // skip ...
                } else {
                    result.add(expandTemplate(list.get(i), bindings, ellipsisVars, patVars, renames));
                }
            }
            SExpr sexpr = new SExpr(template instanceof SExpr s ? s.pos : null);
            sexpr.addAll(result);
            return sexpr;
        }
        return raw;
    }

    private String findEllipsisVar(Object template, java.util.Set<String> ellipsisVars) {
        Object raw = unwrap(template);
        if (raw instanceof String sym && ellipsisVars.contains(sym)) return sym;
        if (raw instanceof List<?> list) {
            for (Object elem : list) {
                String found = findEllipsisVar(elem, ellipsisVars);
                if (found != null) return found;
            }
        }
        return null;
    }

    // --- Conversion ---

    private Object javaToScheme(Object val) {
        if (val instanceof Token t) return javaToScheme(t.value());
        if (val instanceof List<?> list) {
            Object result = NIL;
            for (int i = list.size() - 1; i >= 0; i--) {
                result = new Cons(javaToScheme(list.get(i)), result);
            }
            return result;
        }
        return val;
    }

    // --- Output formatting ---

    String displayString(Object val) {
        if (val instanceof SchemeString s) return s.value();
        return schemeToString(val);
    }

    String schemeToString(Object val) {
        if (val == NIL) return "()";
        if (val instanceof Long l) return l.toString();
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
        if (val instanceof Builtin b) return "#<procedure " + b.name() + ">";
        if (val instanceof Lambda) return "#<procedure>";
        if (val instanceof Cons) {
            StringBuilder sb = new StringBuilder("(");
            Object cur = val;
            boolean first = true;
            while (cur instanceof Cons c) {
                if (!first) sb.append(" ");
                first = false;
                sb.append(schemeToString(c.car));
                cur = c.cdr;
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
        if (val instanceof String s) return s;
        return String.valueOf(val);
    }
}
