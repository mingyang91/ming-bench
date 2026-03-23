package ming;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;

public class Evaluator {

    private static final Object VOID = new Object() {
        @Override public String toString() { return "#<void>"; }
    };

    static final Object NIL = new Object() {
        @Override public String toString() { return "()"; }
    };

    static class Pair {
        Object car;
        Object cdr;
        Pair(Object car, Object cdr) {
            this.car = car;
            this.cdr = cdr;
        }
    }

    record Located(Object value, int line, int col) {}
    record Token(Object value, int line, int col) {}

    private int errLine = 1;
    private int errCol = 1;
    private StringBuilder outputBuf;

    // --- CPS infrastructure ---

    private interface Bounce {}
    private record Done(Object value) implements Bounce {}
    private record More(Thunk thunk) implements Bounce {}
    @FunctionalInterface private interface Thunk { Bounce run() throws EvalError; }
    @FunctionalInterface private interface Cont { Bounce apply(Object value) throws EvalError; }
    @FunctionalInterface private interface ArgsCont { Bounce apply(List<Object> args) throws EvalError; }

    private static class Continuation {
        final Cont k;
        Continuation(Cont k) { this.k = k; }
    }

    private static final Object CALL_CC = new Object() {
        @Override public String toString() { return "#<procedure:call/cc>"; }
    };

    private Object trampoline(Bounce b) throws EvalError {
        while (b instanceof More m) {
            b = m.thunk().run();
        }
        return ((Done) b).value();
    }

    // --- Public API ---

    private EvalError posError(String msg) {
        return new EvalError(errLine + ":" + errCol + ": " + msg);
    }

    public String evalStr(String input) throws EvalError {
        List<Token> tokens = tokenize(input);
        int[] pos = {0};
        List<Object> exprs = new ArrayList<>();
        while (pos[0] < tokens.size()) exprs.add(parse(tokens, pos));
        if (exprs.isEmpty()) throw new EvalError("no expression");
        Env env = createGlobalEnv();
        Object lastResult = trampoline(evalTopSequence(exprs, 0, env));
        if (lastResult == VOID) throw new EvalError("no expression");
        return schemeToString(lastResult);
    }

    public EvalResult evalStrWithOutput(String input) throws EvalError {
        List<Token> tokens = tokenize(input);
        int[] pos = {0};
        List<Object> exprs = new ArrayList<>();
        while (pos[0] < tokens.size()) exprs.add(parse(tokens, pos));
        Env env = createGlobalEnv();
        this.outputBuf = new StringBuilder();
        Object lastResult = exprs.isEmpty() ? VOID : trampoline(evalTopSequence(exprs, 0, env));
        String resultStr = (lastResult == VOID) ? "" : schemeToString(lastResult);
        return new EvalResult(resultStr, this.outputBuf.toString());
    }

    private Bounce evalTopSequence(List<Object> exprs, int idx, Env env) throws EvalError {
        if (idx == exprs.size() - 1) {
            return new More(() -> eval(exprs.get(idx), env, v -> new Done(v)));
        }
        return new More(() -> eval(exprs.get(idx), env, ignored ->
            evalTopSequence(exprs, idx + 1, env)));
    }

    private Env createGlobalEnv() {
        return new Env(null);
    }

    // --- Environment ---

    private static class Env {
        final Env parent;
        final Map<String, Object> bindings = new HashMap<>();

        Env(Env parent) { this.parent = parent; }

        Object lookup(String name) throws EvalError {
            if (bindings.containsKey(name)) return bindings.get(name);
            if (parent != null) return parent.lookup(name);
            return null;
        }

        void define(String name, Object value) { bindings.put(name, value); }

        boolean set(String name, Object value) {
            if (bindings.containsKey(name)) { bindings.put(name, value); return true; }
            if (parent != null) return parent.set(name, value);
            return false;
        }
    }

    // --- Lambda ---

    private static class Lambda {
        final List<String> params;
        final String restParam;
        final Object body;
        final Env closure;

        Lambda(List<String> params, String restParam, Object body, Env closure) {
            this.params = params;
            this.restParam = restParam;
            this.body = body;
            this.closure = closure;
        }
    }

    // --- Macro support ---

    private static class SyntaxRulesMacro {
        final List<String> literals;
        final List<List<Object>> patterns;
        final List<Object> templates;
        final Env defEnv;
        SyntaxRulesMacro(List<String> literals, List<List<Object>> patterns,
                         List<Object> templates, Env defEnv) {
            this.literals = literals;
            this.patterns = patterns;
            this.templates = templates;
            this.defEnv = defEnv;
        }
    }

    private static class EnvRef {
        final String name;
        final Env env;
        EnvRef(String name, Env env) { this.name = name; this.env = env; }
    }

    private int gensymCounter = 0;
    private String gensym(String base) { return base + "##" + (gensymCounter++); }

    private static final java.util.Set<String> SPECIAL_FORMS = java.util.Set.of(
        "define", "if", "quote", "lambda", "and", "or", "let", "begin",
        "set!", "cond", "define-syntax", "syntax-rules"
    );

    private Object unwrapLoc(Object o) {
        while (o instanceof Located l) o = l.value();
        return o;
    }

    // --- Tokenizer ---

    private List<Token> tokenize(String input) throws EvalError {
        List<Token> tokens = new ArrayList<>();
        int i = 0;
        int len = input.length();
        int line = 1;
        int col = 1;
        while (i < len) {
            char c = input.charAt(i);
            if (c == '\n') {
                i++; line++; col = 1;
            } else if (Character.isWhitespace(c)) {
                i++; col++;
            } else if (c == ';') {
                while (i < len && input.charAt(i) != '\n') { i++; col++; }
            } else if (c == '(') {
                tokens.add(new Token("(", line, col)); i++; col++;
            } else if (c == ')') {
                tokens.add(new Token(")", line, col)); i++; col++;
            } else if (c == '\'') {
                tokens.add(new Token("'", line, col)); i++; col++;
            } else if (c == '"') {
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
                                case '\\' -> sb.append('\\');
                                case '"' -> sb.append('"');
                                default -> { sb.append('\\'); sb.append(esc); }
                            }
                        }
                    } else {
                        if (input.charAt(i) == '\n') {
                            sb.append('\n'); i++; line++; col = 1; continue;
                        }
                        sb.append(input.charAt(i));
                    }
                    i++; col++;
                }
                if (i >= len) throw new EvalError("unterminated string");
                i++; col++;
                tokens.add(new Token(new SchemeString(sb.toString()), line, startCol));
            } else if (c == '#') {
                int startCol = col;
                if (i + 1 < len) {
                    char next = input.charAt(i + 1);
                    if (next == 't') {
                        if (i + 2 >= len || isDelimiter(input.charAt(i + 2))) {
                            tokens.add(new Token(Boolean.TRUE, line, startCol));
                            i += 2; col += 2;
                        } else {
                            throw new EvalError(line + ":" + col + ": unknown token: #" + next);
                        }
                    } else if (next == 'f') {
                        if (i + 2 >= len || isDelimiter(input.charAt(i + 2))) {
                            tokens.add(new Token(Boolean.FALSE, line, startCol));
                            i += 2; col += 2;
                        } else {
                            throw new EvalError(line + ":" + col + ": unknown token: #" + next);
                        }
                    } else if (next == '\\') {
                        if (i + 2 >= len) throw new EvalError(line + ":" + col + ": unexpected end of input in character literal");
                        int start = i + 2;
                        int ci = start;
                        if (Character.isLetter(input.charAt(ci))) {
                            while (ci < len && Character.isLetter(input.charAt(ci))) ci++;
                            String charName = input.substring(start, ci);
                            if (charName.length() == 1) {
                                tokens.add(new Token(new SchemeChar(charName.charAt(0)), line, startCol));
                            } else {
                                tokens.add(new Token(switch (charName) {
                                    case "space" -> new SchemeChar(' ');
                                    case "newline" -> new SchemeChar('\n');
                                    case "tab" -> new SchemeChar('\t');
                                    default -> throw new EvalError(line + ":" + col + ": unknown character name: " + charName);
                                }, line, startCol));
                            }
                        } else {
                            tokens.add(new Token(new SchemeChar(input.charAt(ci)), line, startCol));
                            ci++;
                        }
                        col += (ci - i);
                        i = ci;
                    } else {
                        throw new EvalError(line + ":" + col + ": unknown token: #" + next);
                    }
                } else {
                    throw new EvalError(line + ":" + col + ": unexpected end of input after #");
                }
            } else {
                int startCol = col;
                StringBuilder sb = new StringBuilder();
                while (i < len && !Character.isWhitespace(input.charAt(i))
                        && input.charAt(i) != '(' && input.charAt(i) != ')'
                        && input.charAt(i) != '"' && input.charAt(i) != ';'
                        && input.charAt(i) != '\'') {
                    sb.append(input.charAt(i)); i++; col++;
                }
                String tok = sb.toString();
                try {
                    tokens.add(new Token(Long.parseLong(tok), line, startCol));
                } catch (NumberFormatException e) {
                    tokens.add(new Token(tok, line, startCol));
                }
            }
        }
        return tokens;
    }

    private boolean isDelimiter(char c) {
        return Character.isWhitespace(c) || c == '(' || c == ')' || c == '"' || c == ';';
    }

    // --- Parser ---

    private Object parse(List<Token> tokens, int[] pos) throws EvalError {
        if (pos[0] >= tokens.size()) throw new EvalError("unexpected end of input");
        Token tok = tokens.get(pos[0]);
        pos[0]++;
        if (tok.value().equals("'")) {
            Object quoted = parse(tokens, pos);
            List<Object> quoteExpr = new ArrayList<>();
            quoteExpr.add("quote");
            quoteExpr.add(quoted);
            return new Located(quoteExpr, tok.line(), tok.col());
        }
        if (tok.value().equals("(")) {
            List<Object> list = new ArrayList<>();
            while (pos[0] < tokens.size() && !tokens.get(pos[0]).value().equals(")")) {
                list.add(parse(tokens, pos));
            }
            if (pos[0] >= tokens.size()) {
                throw new EvalError(tok.line() + ":" + tok.col() + ": missing closing parenthesis");
            }
            pos[0]++;
            return new Located(list, tok.line(), tok.col());
        } else if (tok.value().equals(")")) {
            throw new EvalError(tok.line() + ":" + tok.col() + ": unexpected )");
        } else {
            return new Located(tok.value(), tok.line(), tok.col());
        }
    }

    private Object listToScheme(Object parsed) {
        if (parsed instanceof Located loc) return listToScheme(loc.value());
        if (parsed instanceof List<?> list) {
            Object result = NIL;
            for (int i = list.size() - 1; i >= 0; i--) {
                result = new Pair(listToScheme(list.get(i)), result);
            }
            return result;
        }
        return parsed;
    }

    // --- CPS Evaluator ---

    @SuppressWarnings("unchecked")
    private Bounce eval(Object expr, Env env, Cont k) throws EvalError {
        if (expr instanceof Located loc) {
            errLine = loc.line();
            errCol = loc.col();
            return new More(() -> eval(loc.value(), env, k));
        }

        if (expr instanceof Long || expr instanceof Boolean || expr instanceof SchemeString || expr instanceof SchemeChar) {
            return k.apply(expr);
        }

        if (expr instanceof String sym) {
            if (sym.equals("call/cc") || sym.equals("call-with-current-continuation")) {
                return k.apply(CALL_CC);
            }
            if (isPrimitive(sym)) return k.apply(sym);
            Object val = env.lookup(sym);
            if (val == null) throw posError("unbound variable: " + sym);
            return k.apply(val);
        }

        if (expr instanceof EnvRef ref) {
            Object val = ref.env.lookup(ref.name);
            if (val == null) throw posError("unbound variable: " + ref.name);
            return k.apply(val);
        }

        if (expr instanceof List<?> list) {
            if (list.isEmpty()) throw posError("empty application");

            Object head = list.get(0);
            String formName = null;
            if (head instanceof Located locHead) {
                if (locHead.value() instanceof String s) formName = s;
            } else if (head instanceof String s) {
                formName = s;
            }

            if (formName != null) {
                switch (formName) {
                    case "define" -> {
                        if (list.size() < 3) throw posError("define: bad syntax");
                        Object target = list.get(1);
                        if (target instanceof Located lt) target = lt.value();
                        if (target instanceof String name) {
                            return new More(() -> eval(list.get(2), env, val -> {
                                env.define(name, val);
                                return k.apply(VOID);
                            }));
                        } else if (target instanceof List<?> sig) {
                            String fname = null;
                            Object first = sig.isEmpty() ? null : sig.get(0);
                            if (first instanceof Located lf) first = lf.value();
                            if (first instanceof String s) fname = s;
                            if (fname == null) throw posError("define: bad syntax");
                            List<String> params = new ArrayList<>();
                            String restParam = null;
                            for (int i = 1; i < sig.size(); i++) {
                                Object p = sig.get(i);
                                if (p instanceof Located lp) p = lp.value();
                                if (!(p instanceof String ps)) throw posError("define: parameter must be a symbol");
                                if (ps.equals(".")) {
                                    if (i + 1 >= sig.size()) throw posError("define: missing rest parameter after dot");
                                    Object rp = sig.get(i + 1);
                                    if (rp instanceof Located lrp) rp = lrp.value();
                                    if (!(rp instanceof String rps)) throw posError("define: rest parameter must be a symbol");
                                    restParam = rps;
                                    break;
                                }
                                params.add(ps);
                            }
                            Object body;
                            if (list.size() == 3) {
                                body = list.get(2);
                            } else {
                                List<Object> beginBody = new ArrayList<>();
                                beginBody.add("begin");
                                for (int i = 2; i < list.size(); i++) beginBody.add(list.get(i));
                                body = beginBody;
                            }
                            Lambda lambda = new Lambda(params, restParam, body, env);
                            env.define(fname, lambda);
                            return k.apply(VOID);
                        }
                        throw posError("define: bad syntax");
                    }
                    case "if" -> {
                        if (list.size() < 3 || list.size() > 4) throw posError("if: bad syntax");
                        return new More(() -> eval(list.get(1), env, cond -> {
                            if (!isFalse(cond)) {
                                return new More(() -> eval(list.get(2), env, k));
                            } else if (list.size() == 4) {
                                return new More(() -> eval(list.get(3), env, k));
                            }
                            return k.apply(VOID);
                        }));
                    }
                    case "quote" -> {
                        if (list.size() != 2) throw posError("quote: expected 1 argument");
                        return k.apply(listToScheme(list.get(1)));
                    }
                    case "lambda" -> {
                        if (list.size() < 3) throw posError("lambda: bad syntax");
                        Object paramsExpr = list.get(1);
                        if (paramsExpr instanceof Located lp) paramsExpr = lp.value();
                        List<String> params = new ArrayList<>();
                        String restParam = null;
                        if (paramsExpr instanceof List<?> paramList) {
                            for (int pi = 0; pi < paramList.size(); pi++) {
                                Object p = paramList.get(pi);
                                if (p instanceof Located lpp) p = lpp.value();
                                if (!(p instanceof String ps)) throw posError("lambda: parameter must be a symbol");
                                if (ps.equals(".")) {
                                    if (pi + 1 >= paramList.size()) throw posError("lambda: missing rest parameter after dot");
                                    Object rp = paramList.get(pi + 1);
                                    if (rp instanceof Located lrp) rp = lrp.value();
                                    if (!(rp instanceof String rps)) throw posError("lambda: rest parameter must be a symbol");
                                    restParam = rps;
                                    break;
                                }
                                params.add(ps);
                            }
                        } else if (paramsExpr instanceof String restOnly) {
                            restParam = restOnly;
                        } else {
                            throw posError("lambda: parameters must be a list or symbol");
                        }
                        Object body;
                        if (list.size() == 3) {
                            body = list.get(2);
                        } else {
                            List<Object> beginBody = new ArrayList<>();
                            beginBody.add("begin");
                            for (int i = 2; i < list.size(); i++) beginBody.add(list.get(i));
                            body = beginBody;
                        }
                        return k.apply(new Lambda(params, restParam, body, env));
                    }
                    case "and" -> {
                        if (list.size() == 1) return k.apply(Boolean.TRUE);
                        return evalAnd(list, 1, env, k);
                    }
                    case "or" -> {
                        if (list.size() == 1) return k.apply(Boolean.FALSE);
                        return evalOr(list, 1, env, k);
                    }
                    case "let" -> {
                        return evalLet(list, env, k);
                    }
                    case "begin" -> {
                        if (list.size() == 1) return k.apply(VOID);
                        return evalBegin(list, 1, env, k);
                    }
                    case "set!" -> {
                        if (list.size() != 3) throw posError("set!: bad syntax");
                        Object nameObj = list.get(1);
                        if (nameObj instanceof Located ln) nameObj = ln.value();
                        if (!(nameObj instanceof String setName)) throw posError("set!: expected symbol");
                        final String finalSetName = setName;
                        return new More(() -> eval(list.get(2), env, val -> {
                            if (!env.set(finalSetName, val)) throw posError("set!: unbound variable: " + finalSetName);
                            return k.apply(VOID);
                        }));
                    }
                    case "cond" -> {
                        return evalCond(list, 1, env, k);
                    }
                    case "define-syntax" -> {
                        if (list.size() != 3) throw posError("define-syntax: bad syntax");
                        Object dsName = list.get(1);
                        if (dsName instanceof Located ln) dsName = ln.value();
                        if (!(dsName instanceof String macroName)) throw posError("define-syntax: expected symbol");
                        Object transformer = list.get(2);
                        if (transformer instanceof Located lt) transformer = lt.value();
                        if (!(transformer instanceof List<?> transExpr) || transExpr.size() < 2)
                            throw posError("define-syntax: expected syntax-rules");
                        Object transHead = transExpr.get(0);
                        if (transHead instanceof Located lth) transHead = lth.value();
                        if (!(transHead instanceof String th && th.equals("syntax-rules")))
                            throw posError("define-syntax: expected syntax-rules");
                        Object literalsObj = transExpr.get(1);
                        if (literalsObj instanceof Located ll) literalsObj = ll.value();
                        if (!(literalsObj instanceof List<?> literalsList))
                            throw posError("syntax-rules: expected literals list");
                        List<String> literals = new ArrayList<>();
                        for (Object l : literalsList) {
                            if (l instanceof Located ll2) l = ll2.value();
                            if (!(l instanceof String ls)) throw posError("syntax-rules: literal must be a symbol");
                            literals.add(ls);
                        }
                        List<List<Object>> patterns = new ArrayList<>();
                        List<Object> templates = new ArrayList<>();
                        for (int i = 2; i < transExpr.size(); i++) {
                            Object clause = transExpr.get(i);
                            if (clause instanceof Located lc) clause = lc.value();
                            if (!(clause instanceof List<?> clauseList) || clauseList.size() != 2)
                                throw posError("syntax-rules: bad clause");
                            Object pattern = clauseList.get(0);
                            if (pattern instanceof Located lp) pattern = lp.value();
                            if (!(pattern instanceof List<?> patList))
                                throw posError("syntax-rules: pattern must be a list");
                            @SuppressWarnings("unchecked")
                            List<Object> typedPatList = (List<Object>) patList;
                            patterns.add(typedPatList);
                            templates.add(clauseList.get(1));
                        }
                        final String fMacroName = macroName;
                        env.define(fMacroName, new SyntaxRulesMacro(literals, patterns, templates, env));
                        return k.apply(VOID);
                    }
                }

                // Check for macro application
                Object macroVal = env.lookup(formName);
                if (macroVal instanceof SyntaxRulesMacro macro) {
                    Object expanded = expandMacro(macro, list);
                    return new More(() -> eval(expanded, env, k));
                }
            }

            // Procedure call
            return new More(() -> eval(head, env, proc -> {
                return evalArgs(list, 1, env, args -> {
                    if (head instanceof Located lh) {
                        errLine = lh.line();
                        errCol = lh.col();
                    }
                    if (proc instanceof String p && p.equals("apply")) {
                        Object[] applyResult = handleApply(args);
                        @SuppressWarnings("unchecked")
                        List<Object> newArgs = (List<Object>) applyResult[1];
                        return applyProc(applyResult[0], newArgs, k);
                    }
                    return applyProc(proc, args, k);
                });
            }));
        }

        throw posError("cannot evaluate: " + expr);
    }

    // --- CPS helpers ---

    private Bounce evalBegin(List<?> list, int idx, Env env, Cont k) throws EvalError {
        if (idx == list.size() - 1) {
            return new More(() -> eval(list.get(idx), env, k));
        }
        return new More(() -> eval(list.get(idx), env, ignored -> evalBegin(list, idx + 1, env, k)));
    }

    private Bounce evalAnd(List<?> list, int idx, Env env, Cont k) throws EvalError {
        if (idx == list.size() - 1) {
            return new More(() -> eval(list.get(idx), env, k));
        }
        return new More(() -> eval(list.get(idx), env, val -> {
            if (isFalse(val)) return k.apply(val);
            return evalAnd(list, idx + 1, env, k);
        }));
    }

    private Bounce evalOr(List<?> list, int idx, Env env, Cont k) throws EvalError {
        if (idx == list.size() - 1) {
            return new More(() -> eval(list.get(idx), env, k));
        }
        return new More(() -> eval(list.get(idx), env, val -> {
            if (!isFalse(val)) return k.apply(val);
            return evalOr(list, idx + 1, env, k);
        }));
    }

    // Evaluate arguments right-to-left (matches Guile's evaluation order)
    private Bounce evalArgs(List<?> list, int start, Env env, ArgsCont k) throws EvalError {
        return evalArgsRTL(list, list.size() - 1, start, env, new ArrayList<>(), k);
    }

    private Bounce evalArgsRTL(List<?> list, int idx, int start, Env env, List<Object> acc, ArgsCont k) throws EvalError {
        if (idx < start) return k.apply(acc);
        return new More(() -> eval(list.get(idx), env, val -> {
            List<Object> newAcc = new ArrayList<>(acc.size() + 1);
            newAcc.add(val);
            newAcc.addAll(acc);
            return evalArgsRTL(list, idx - 1, start, env, newAcc, k);
        }));
    }

    private Bounce evalLet(List<?> list, Env env, Cont k) throws EvalError {
        int idx = 1;
        String name = null;
        Object nameCandidate = list.get(1);
        if (nameCandidate instanceof Located ln) nameCandidate = ln.value();
        if (nameCandidate instanceof String n) {
            name = n;
            idx = 2;
        }
        Object bindingsObj = list.get(idx);
        if (bindingsObj instanceof Located lb) bindingsObj = lb.value();
        if (!(bindingsObj instanceof List<?> bindings)) throw posError("let: bad syntax");

        List<String> paramNames = new ArrayList<>();
        List<Object> initExprs = new ArrayList<>();
        for (Object b : bindings) {
            if (b instanceof Located lbb) b = lbb.value();
            if (!(b instanceof List<?> binding) || binding.size() != 2) throw posError("let: bad binding");
            Object pnameObj = binding.get(0);
            if (pnameObj instanceof Located lp) pnameObj = lp.value();
            if (!(pnameObj instanceof String pname)) throw posError("let: binding name must be a symbol");
            paramNames.add(pname);
            initExprs.add(binding.get(1));
        }

        Object body;
        int bodyStart = idx + 1;
        if (list.size() - bodyStart == 1) {
            body = list.get(bodyStart);
        } else {
            List<Object> beginBody = new ArrayList<>();
            beginBody.add("begin");
            for (int i = bodyStart; i < list.size(); i++) beginBody.add(list.get(i));
            body = beginBody;
        }

        final String finalName = name;
        final Object finalBody = body;
        return evalLetInits(initExprs, 0, env, new ArrayList<>(), initVals -> {
            Env letEnv = new Env(env);
            if (finalName != null) {
                Lambda lambda = new Lambda(paramNames, null, finalBody, letEnv);
                letEnv.define(finalName, lambda);
            }
            for (int i = 0; i < paramNames.size(); i++) {
                letEnv.define(paramNames.get(i), initVals.get(i));
            }
            return new More(() -> eval(finalBody, letEnv, k));
        });
    }

    private Bounce evalLetInits(List<Object> exprs, int idx, Env env, List<Object> acc, ArgsCont k) throws EvalError {
        if (idx >= exprs.size()) return k.apply(acc);
        return new More(() -> eval(exprs.get(idx), env, val -> {
            List<Object> newAcc = new ArrayList<>(acc);
            newAcc.add(val);
            return evalLetInits(exprs, idx + 1, env, newAcc, k);
        }));
    }

    private Bounce evalCond(List<?> list, int clauseIdx, Env env, Cont k) throws EvalError {
        if (clauseIdx >= list.size()) return k.apply(VOID);
        Object clauseObj = list.get(clauseIdx);
        if (clauseObj instanceof Located lc) clauseObj = lc.value();
        if (!(clauseObj instanceof List<?> clause) || clause.isEmpty()) throw posError("cond: bad clause");

        Object test = clause.get(0);
        Object testUnwrapped = test;
        if (testUnwrapped instanceof Located lt) testUnwrapped = lt.value();
        if (testUnwrapped instanceof String s && s.equals("else")) {
            if (clause.size() == 1) return k.apply(Boolean.TRUE);
            return evalBegin(clause, 1, env, k);
        }

        final List<?> finalClause = clause;
        return new More(() -> eval(test, env, testResult -> {
            if (!isFalse(testResult)) {
                if (finalClause.size() == 1) return k.apply(testResult);
                return evalBegin(finalClause, 1, env, k);
            }
            return evalCond(list, clauseIdx + 1, env, k);
        }));
    }

    // --- Macro expansion ---

    private Object expandMacro(SyntaxRulesMacro macro, List<?> input) throws EvalError {
        for (int r = 0; r < macro.patterns.size(); r++) {
            Map<String, Object> bindings = matchPattern(macro.patterns.get(r), input, macro.literals);
            if (bindings != null) {
                Set<String> patVars = bindings.keySet();
                Map<String, Object> renaming = new HashMap<>();
                collectHygieneRenames(macro.templates.get(r), patVars, macro.literals,
                                      macro.defEnv, renaming);
                return expandTemplate(macro.templates.get(r), bindings, renaming);
            }
        }
        throw posError("no matching pattern for macro");
    }

    private Map<String, Object> matchPattern(List<Object> pattern, List<?> input,
                                              List<String> literals) throws EvalError {
        Map<String, Object> bindings = new HashMap<>();
        int pi = 1, ii = 1;
        while (pi < pattern.size()) {
            Object patElem = unwrapLoc(pattern.get(pi));
            boolean isEll = (pi + 1 < pattern.size()) && isEllipsis(pattern.get(pi + 1));
            if (isEll) {
                if (!(patElem instanceof String pvar)) return null;
                List<Object> collected = new ArrayList<>();
                while (ii < input.size()) { collected.add(input.get(ii)); ii++; }
                bindings.put(pvar, collected);
                pi += 2;
            } else {
                if (ii >= input.size()) return null;
                if (patElem instanceof String pvar) {
                    if (literals.contains(pvar)) {
                        Object inVal = unwrapLoc(input.get(ii));
                        if (!(inVal instanceof String s && s.equals(pvar))) return null;
                    } else {
                        bindings.put(pvar, input.get(ii));
                    }
                } else {
                    return null;
                }
                pi++; ii++;
            }
        }
        return (ii == input.size()) ? bindings : null;
    }

    private boolean isEllipsis(Object o) {
        o = unwrapLoc(o);
        return o instanceof String s && s.equals("...");
    }

    private void collectHygieneRenames(Object template, Set<String> patVars,
                                        List<String> literals, Env defEnv,
                                        Map<String, Object> renaming) throws EvalError {
        template = unwrapLoc(template);
        if (template instanceof String sym) {
            if (patVars.contains(sym) || SPECIAL_FORMS.contains(sym) ||
                literals.contains(sym) || sym.equals("...") ||
                renaming.containsKey(sym) || PRIMITIVES.contains(sym)) return;
            Object val = defEnv.lookup(sym);
            if (val instanceof SyntaxRulesMacro) {
                return; // keep macro names as-is for recognition
            } else if (val != null) {
                renaming.put(sym, new EnvRef(sym, defEnv));
            } else {
                renaming.put(sym, gensym(sym));
            }
        } else if (template instanceof List<?> list) {
            for (Object elem : list) {
                collectHygieneRenames(elem, patVars, literals, defEnv, renaming);
            }
        }
    }

    private Object expandTemplate(Object template, Map<String, Object> bindings,
                                   Map<String, Object> renaming) {
        template = unwrapLoc(template);
        if (template instanceof String sym) {
            if (bindings.containsKey(sym)) return bindings.get(sym);
            if (renaming.containsKey(sym)) return renaming.get(sym);
            return sym;
        }
        if (template instanceof List<?> list) {
            List<Object> result = new ArrayList<>();
            for (int i = 0; i < list.size(); i++) {
                if (i + 1 < list.size() && isEllipsis(list.get(i + 1))) {
                    Object subTemplate = list.get(i);
                    Set<String> ellipsisVars = findEllipsisVars(subTemplate, bindings);
                    if (!ellipsisVars.isEmpty()) {
                        String firstVar = ellipsisVars.iterator().next();
                        @SuppressWarnings("unchecked")
                        List<Object> vals = (List<Object>) bindings.get(firstVar);
                        for (int j = 0; j < vals.size(); j++) {
                            Map<String, Object> iterBindings = new HashMap<>(bindings);
                            for (String ev : ellipsisVars) {
                                @SuppressWarnings("unchecked")
                                List<Object> evVals = (List<Object>) bindings.get(ev);
                                iterBindings.put(ev, evVals.get(j));
                            }
                            result.add(expandTemplate(subTemplate, iterBindings, renaming));
                        }
                    }
                    i++; // skip ...
                } else {
                    result.add(expandTemplate(list.get(i), bindings, renaming));
                }
            }
            return result;
        }
        return template;
    }

    private Set<String> findEllipsisVars(Object template, Map<String, Object> bindings) {
        template = unwrapLoc(template);
        Set<String> vars = new HashSet<>();
        if (template instanceof String sym) {
            if (bindings.containsKey(sym) && bindings.get(sym) instanceof List) {
                vars.add(sym);
            }
        } else if (template instanceof List<?> list) {
            for (Object elem : list) vars.addAll(findEllipsisVars(elem, bindings));
        }
        return vars;
    }

    // --- Procedure application (CPS) ---

    private Bounce applyProc(Object proc, List<Object> args, Cont k) throws EvalError {
        if (proc == CALL_CC) {
            if (args.size() != 1) throw posError("call/cc: expected 1 argument");
            Continuation captured = new Continuation(k);
            return applyProc(args.get(0), List.of(captured), k);
        }
        if (proc instanceof Continuation cont) {
            if (args.size() != 1) throw posError("continuation: expected 1 argument");
            return cont.k.apply(args.get(0));
        }
        if (proc instanceof Lambda lambda) {
            if (lambda.restParam != null) {
                if (args.size() < lambda.params.size())
                    throw posError("wrong number of arguments: expected at least " + lambda.params.size() + ", got " + args.size());
            } else {
                if (args.size() != lambda.params.size())
                    throw posError("wrong number of arguments: expected " + lambda.params.size() + ", got " + args.size());
            }
            Env callEnv = new Env(lambda.closure);
            for (int i = 0; i < lambda.params.size(); i++) {
                callEnv.define(lambda.params.get(i), args.get(i));
            }
            if (lambda.restParam != null) {
                Object rest = NIL;
                for (int i = args.size() - 1; i >= lambda.params.size(); i--) {
                    rest = new Pair(args.get(i), rest);
                }
                callEnv.define(lambda.restParam, rest);
            }
            return new More(() -> eval(lambda.body, callEnv, k));
        }
        if (proc instanceof String p && isPrimitive(p)) {
            return k.apply(applyPrimitive(p, args));
        }
        throw posError("not a procedure: " + schemeToString(proc));
    }

    // --- Primitives ---

    private static final java.util.Set<String> PRIMITIVES = java.util.Set.of(
            "+", "-", "*", "/", "<", ">", "=", "<=", ">=",
            "cons", "car", "cdr", "null?", "list", "length", "append",
            "not", "string?", "number?", "boolean?", "pair?", "symbol?",
            "display", "write", "newline",
            "string-append", "string-length", "substring",
            "string->number", "number->string",
            "symbol->string", "string->symbol",
            "string-ref", "char?",
            "string-set!", "string-copy",
            "apply", "procedure?"
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
                if (args.isEmpty()) throw posError("-: expected at least 1 argument");
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
                if (args.isEmpty()) throw posError("/: expected at least 1 argument");
                long result = requireLong(args.get(0), "/");
                for (int i = 1; i < args.size(); i++) {
                    long divisor = requireLong(args.get(i), "/");
                    if (divisor == 0) throw posError("division by zero");
                    result /= divisor;
                }
                yield result;
            }
            case "<" -> { requireArgCount(args, 2, "<"); yield requireLong(args.get(0), "<") < requireLong(args.get(1), "<"); }
            case ">" -> { requireArgCount(args, 2, ">"); yield requireLong(args.get(0), ">") > requireLong(args.get(1), ">"); }
            case "=" -> { requireArgCount(args, 2, "="); yield requireLong(args.get(0), "=") == requireLong(args.get(1), "="); }
            case "<=" -> { requireArgCount(args, 2, "<="); yield requireLong(args.get(0), "<=") <= requireLong(args.get(1), "<="); }
            case ">=" -> { requireArgCount(args, 2, ">="); yield requireLong(args.get(0), ">=") >= requireLong(args.get(1), ">="); }
            case "cons" -> { requireArgCount(args, 2, "cons"); yield new Pair(args.get(0), args.get(1)); }
            case "car" -> {
                requireArgCount(args, 1, "car");
                if (!(args.get(0) instanceof Pair p)) throw posError("car: not a pair");
                yield p.car;
            }
            case "cdr" -> {
                requireArgCount(args, 1, "cdr");
                if (!(args.get(0) instanceof Pair p)) throw posError("cdr: not a pair");
                yield p.cdr;
            }
            case "null?" -> { requireArgCount(args, 1, "null?"); yield args.get(0) == NIL; }
            case "list" -> {
                Object result = NIL;
                for (int i = args.size() - 1; i >= 0; i--) result = new Pair(args.get(i), result);
                yield result;
            }
            case "length" -> {
                requireArgCount(args, 1, "length");
                Object lst = args.get(0);
                long len = 0;
                while (lst instanceof Pair p) { len++; lst = p.cdr; }
                if (lst != NIL) throw posError("length: not a proper list");
                yield len;
            }
            case "append" -> {
                if (args.isEmpty()) yield NIL;
                Object result = args.get(args.size() - 1);
                for (int i = args.size() - 2; i >= 0; i--) result = appendTwo(args.get(i), result);
                yield result;
            }
            case "not" -> { requireArgCount(args, 1, "not"); yield isFalse(args.get(0)) ? Boolean.TRUE : Boolean.FALSE; }
            case "string?" -> { requireArgCount(args, 1, "string?"); yield args.get(0) instanceof SchemeString; }
            case "number?" -> { requireArgCount(args, 1, "number?"); yield args.get(0) instanceof Long; }
            case "boolean?" -> { requireArgCount(args, 1, "boolean?"); yield args.get(0) instanceof Boolean; }
            case "pair?" -> { requireArgCount(args, 1, "pair?"); yield args.get(0) instanceof Pair; }
            case "symbol?" -> { requireArgCount(args, 1, "symbol?"); yield args.get(0) instanceof String; }
            case "char?" -> { requireArgCount(args, 1, "char?"); yield args.get(0) instanceof SchemeChar; }
            case "procedure?" -> {
                requireArgCount(args, 1, "procedure?");
                Object a = args.get(0);
                yield a instanceof Lambda || a instanceof Continuation || a == CALL_CC || (a instanceof String s && isPrimitive(s));
            }
            case "display" -> {
                requireArgCount(args, 1, "display");
                if (outputBuf != null) outputBuf.append(displayString(args.get(0)));
                yield VOID;
            }
            case "write" -> {
                requireArgCount(args, 1, "write");
                if (outputBuf != null) outputBuf.append(schemeToString(args.get(0)));
                yield VOID;
            }
            case "newline" -> {
                requireArgCount(args, 0, "newline");
                if (outputBuf != null) outputBuf.append('\n');
                yield VOID;
            }
            case "string-append" -> {
                StringBuilder sb = new StringBuilder();
                for (Object a : args) {
                    if (!(a instanceof SchemeString s)) throw posError("string-append: expected string");
                    sb.append(s.value());
                }
                yield new SchemeString(sb.toString());
            }
            case "string-length" -> {
                requireArgCount(args, 1, "string-length");
                if (!(args.get(0) instanceof SchemeString s)) throw posError("string-length: expected string");
                yield (long) s.value().length();
            }
            case "substring" -> {
                if (args.size() < 2 || args.size() > 3) throw posError("substring: expected 2-3 arguments");
                if (!(args.get(0) instanceof SchemeString s)) throw posError("substring: expected string");
                int start = (int) requireLong(args.get(1), "substring");
                int end = args.size() == 3 ? (int) requireLong(args.get(2), "substring") : s.value().length();
                yield new SchemeString(s.value().substring(start, end));
            }
            case "string->number" -> {
                requireArgCount(args, 1, "string->number");
                if (!(args.get(0) instanceof SchemeString s)) throw posError("string->number: expected string");
                try { yield Long.parseLong(s.value()); } catch (NumberFormatException e) { yield Boolean.FALSE; }
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
                yield s.value();
            }
            case "string-ref" -> {
                requireArgCount(args, 2, "string-ref");
                if (!(args.get(0) instanceof SchemeString s)) throw posError("string-ref: expected string");
                int idx = (int) requireLong(args.get(1), "string-ref");
                yield new SchemeChar(s.charAt(idx));
            }
            case "string-set!" -> {
                requireArgCount(args, 3, "string-set!");
                if (!(args.get(0) instanceof SchemeString s)) throw posError("string-set!: expected string");
                int idx = (int) requireLong(args.get(1), "string-set!");
                if (!(args.get(2) instanceof SchemeChar c)) throw posError("string-set!: expected char");
                s.setChar(idx, c.value());
                yield VOID;
            }
            case "string-copy" -> {
                requireArgCount(args, 1, "string-copy");
                if (!(args.get(0) instanceof SchemeString s)) throw posError("string-copy: expected string");
                yield new SchemeString(s.value());
            }
            default -> throw posError("unbound variable: " + proc);
        };
    }

    private Object[] handleApply(List<Object> args) throws EvalError {
        if (args.size() < 2) throw posError("apply: expected at least 2 arguments");
        Object proc = args.get(0);
        Object lastArg = args.get(args.size() - 1);
        List<Object> newArgs = new ArrayList<>();
        for (int i = 1; i < args.size() - 1; i++) newArgs.add(args.get(i));
        Object cur = lastArg;
        while (cur instanceof Pair p) { newArgs.add(p.car); cur = p.cdr; }
        if (cur != NIL) throw posError("apply: last argument must be a proper list");
        return new Object[]{proc, newArgs};
    }

    private Object appendTwo(Object a, Object b) throws EvalError {
        if (a == NIL) return b;
        if (!(a instanceof Pair p)) throw posError("append: not a proper list");
        return new Pair(p.car, appendTwo(p.cdr, b));
    }

    private boolean isFalse(Object val) {
        return val instanceof Boolean b && !b;
    }

    private long requireLong(Object val, String context) throws EvalError {
        if (val instanceof Long l) return l;
        throw posError(context + ": expected number, got " + schemeToString(val));
    }

    private void requireArgCount(List<Object> args, int expected, String context) throws EvalError {
        if (args.size() != expected) {
            throw posError(context + ": expected " + expected + " arguments, got " + args.size());
        }
    }

    // --- Output formatting ---

    private String displayString(Object val) {
        if (val instanceof SchemeString s) return s.value();
        if (val instanceof SchemeChar c) return String.valueOf(c.value());
        return schemeToString(val);
    }

    private String schemeToString(Object val) {
        if (val instanceof Long l) return l.toString();
        if (val instanceof Boolean b) return b ? "#t" : "#f";
        if (val instanceof SchemeString s) return "\"" + s.value() + "\"";
        if (val instanceof SchemeChar c) return "#\\" + c.value();
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
            if (cur != NIL) { sb.append(" . "); sb.append(schemeToString(cur)); }
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
        if (val instanceof Continuation) return "#<continuation>";
        if (val == CALL_CC) return "#<procedure:call/cc>";
        if (val == VOID) return "#<void>";
        return val.toString();
    }

    static class SchemeString {
        private char[] chars;
        SchemeString(String value) { this.chars = value.toCharArray(); }
        String value() { return new String(chars); }
        int length() { return chars.length; }
        char charAt(int i) { return chars[i]; }
        void setChar(int i, char c) { chars[i] = c; }
        @Override public boolean equals(Object o) {
            return o instanceof SchemeString s && java.util.Arrays.equals(chars, s.chars);
        }
        @Override public int hashCode() { return java.util.Arrays.hashCode(chars); }
    }

    record SchemeChar(char value) {}
}
