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
        "define", "if", "quote", "lambda", "and", "or", "let", "let*", "begin",
        "set!", "cond", "define-syntax", "syntax-rules",
        "letrec", "letrec*", "case", "do"
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
                    case "let*" -> {
                        return evalLetStar(list, env, k);
                    }
                    case "letrec" -> {
                        return evalLetrec(list, env, k);
                    }
                    case "letrec*" -> {
                        return evalLetrecStar(list, env, k);
                    }
                    case "case" -> {
                        return evalCase(list, env, k);
                    }
                    case "do" -> {
                        return evalDo(list, env, k);
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
                    if (proc instanceof String p && p.equals("map")) {
                        if (args.size() < 2) throw posError("map: expected at least 2 arguments");
                        Object mapFn = args.get(0);
                        List<Object> lists = new ArrayList<>(args.subList(1, args.size()));
                        return cpsMap(mapFn, lists, k);
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

    private Bounce evalLetStar(List<?> list, Env env, Cont k) throws EvalError {
        if (list.size() < 3) throw posError("let*: bad syntax");
        Object bindingsObj = list.get(1);
        if (bindingsObj instanceof Located lb) bindingsObj = lb.value();
        if (!(bindingsObj instanceof List<?> bindings)) throw posError("let*: bad syntax");
        Object body;
        if (list.size() == 3) {
            body = list.get(2);
        } else {
            List<Object> beginBody = new ArrayList<>();
            beginBody.add("begin");
            for (int i = 2; i < list.size(); i++) beginBody.add(list.get(i));
            body = beginBody;
        }
        Env letEnv = new Env(env);
        return evalLetStarBindings(bindings, 0, letEnv, body, k);
    }

    private Bounce evalLetStarBindings(List<?> bindings, int idx, Env env, Object body, Cont k) throws EvalError {
        if (idx >= bindings.size()) {
            return new More(() -> eval(body, env, k));
        }
        Object b = bindings.get(idx);
        if (b instanceof Located lb) b = lb.value();
        if (!(b instanceof List<?> binding) || binding.size() != 2) throw posError("let*: bad binding");
        Object nameObj = binding.get(0);
        if (nameObj instanceof Located ln) nameObj = ln.value();
        if (!(nameObj instanceof String name)) throw posError("let*: binding name must be a symbol");
        final String finalName = name;
        return new More(() -> eval(binding.get(1), env, val -> {
            env.define(finalName, val);
            return evalLetStarBindings(bindings, idx + 1, env, body, k);
        }));
    }

    private Bounce evalLetrec(List<?> list, Env env, Cont k) throws EvalError {
        if (list.size() < 3) throw posError("letrec: bad syntax");
        Object bindingsObj = list.get(1);
        if (bindingsObj instanceof Located lb) bindingsObj = lb.value();
        if (!(bindingsObj instanceof List<?> bindings)) throw posError("letrec: bad syntax");

        Env letrecEnv = new Env(env);
        List<String> names = new ArrayList<>();
        List<Object> initExprs = new ArrayList<>();
        for (Object b : bindings) {
            if (b instanceof Located lbb) b = lbb.value();
            if (!(b instanceof List<?> binding) || binding.size() != 2) throw posError("letrec: bad binding");
            Object nameObj = binding.get(0);
            if (nameObj instanceof Located ln) nameObj = ln.value();
            if (!(nameObj instanceof String name)) throw posError("letrec: binding name must be a symbol");
            names.add(name);
            initExprs.add(binding.get(1));
            letrecEnv.define(name, VOID); // placeholder
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

        final Object finalBody = body;
        return evalLetInits(initExprs, 0, letrecEnv, new ArrayList<>(), initVals -> {
            for (int i = 0; i < names.size(); i++) {
                letrecEnv.define(names.get(i), initVals.get(i));
            }
            return new More(() -> eval(finalBody, letrecEnv, k));
        });
    }

    private Bounce evalLetrecStar(List<?> list, Env env, Cont k) throws EvalError {
        if (list.size() < 3) throw posError("letrec*: bad syntax");
        Object bindingsObj = list.get(1);
        if (bindingsObj instanceof Located lb) bindingsObj = lb.value();
        if (!(bindingsObj instanceof List<?> bindings)) throw posError("letrec*: bad syntax");

        Env letrecEnv = new Env(env);
        List<String> names = new ArrayList<>();
        List<Object> initExprs = new ArrayList<>();
        for (Object b : bindings) {
            if (b instanceof Located lbb) b = lbb.value();
            if (!(b instanceof List<?> binding) || binding.size() != 2) throw posError("letrec*: bad binding");
            Object nameObj = binding.get(0);
            if (nameObj instanceof Located ln) nameObj = ln.value();
            if (!(nameObj instanceof String name)) throw posError("letrec*: binding name must be a symbol");
            names.add(name);
            initExprs.add(binding.get(1));
            letrecEnv.define(name, VOID);
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

        final Object finalBody = body;
        return evalLetrecStarBindings(names, initExprs, 0, letrecEnv, finalBody, k);
    }

    private Bounce evalLetrecStarBindings(List<String> names, List<Object> initExprs, int idx,
                                           Env env, Object body, Cont k) throws EvalError {
        if (idx >= names.size()) {
            return new More(() -> eval(body, env, k));
        }
        return new More(() -> eval(initExprs.get(idx), env, val -> {
            env.define(names.get(idx), val);
            return evalLetrecStarBindings(names, initExprs, idx + 1, env, body, k);
        }));
    }

    private Bounce evalCase(List<?> list, Env env, Cont k) throws EvalError {
        if (list.size() < 2) throw posError("case: bad syntax");
        return new More(() -> eval(list.get(1), env, keyVal ->
            evalCaseClauses(list, 2, keyVal, env, k)));
    }

    private Bounce evalCaseClauses(List<?> list, int idx, Object keyVal, Env env, Cont k) throws EvalError {
        if (idx >= list.size()) return k.apply(VOID);
        Object clauseObj = list.get(idx);
        if (clauseObj instanceof Located lc) clauseObj = lc.value();
        if (!(clauseObj instanceof List<?> clause) || clause.isEmpty()) throw posError("case: bad clause");

        Object datums = clause.get(0);
        if (datums instanceof Located ld) datums = ld.value();

        if (datums instanceof String s && s.equals("else")) {
            if (clause.size() == 1) return k.apply(VOID);
            return evalBegin(clause, 1, env, k);
        }

        if (!(datums instanceof List<?> datumList)) throw posError("case: expected datum list");
        for (Object d : datumList) {
            Object datum = listToScheme(d);
            if (schemeEqv(keyVal, datum)) {
                if (clause.size() == 1) return k.apply(VOID);
                return evalBegin(clause, 1, env, k);
            }
        }
        return evalCaseClauses(list, idx + 1, keyVal, env, k);
    }

    private boolean schemeEqv(Object a, Object b) {
        if (a == b) return true;
        if (a instanceof Long la && b instanceof Long lb) return la.equals(lb);
        if (a instanceof Boolean ba && b instanceof Boolean bb) return ba.equals(bb);
        if (a instanceof String sa && b instanceof String sb) return sa.equals(sb);
        if (a instanceof SchemeChar ca && b instanceof SchemeChar cb) return ca.value() == cb.value();
        return false;
    }

    @SuppressWarnings("unchecked")
    private Bounce evalDo(List<?> list, Env env, Cont k) throws EvalError {
        if (list.size() < 3) throw posError("do: bad syntax");
        Object bindingsObj = list.get(1);
        if (bindingsObj instanceof Located lb) bindingsObj = lb.value();
        if (!(bindingsObj instanceof List<?> bindings)) throw posError("do: bad syntax");

        Object testClauseObj = list.get(2);
        if (testClauseObj instanceof Located lt) testClauseObj = lt.value();
        if (!(testClauseObj instanceof List<?> testClause) || testClause.isEmpty())
            throw posError("do: bad test clause");

        List<String> varNames = new ArrayList<>();
        List<Object> initExprs = new ArrayList<>();
        List<Object> stepExprs = new ArrayList<>(); // null means no step
        for (Object b : bindings) {
            if (b instanceof Located lbb) b = lbb.value();
            if (!(b instanceof List<?> binding) || binding.size() < 2 || binding.size() > 3)
                throw posError("do: bad variable clause");
            Object nameObj = binding.get(0);
            if (nameObj instanceof Located ln) nameObj = ln.value();
            if (!(nameObj instanceof String name)) throw posError("do: variable must be a symbol");
            varNames.add(name);
            initExprs.add(binding.get(1));
            stepExprs.add(binding.size() == 3 ? binding.get(2) : null);
        }

        Object testExpr = testClause.get(0);
        List<Object> resultExprs = new ArrayList<>();
        for (int i = 1; i < testClause.size(); i++) resultExprs.add(testClause.get(i));

        List<Object> bodyExprs = new ArrayList<>();
        for (int i = 3; i < list.size(); i++) bodyExprs.add(list.get(i));

        // Evaluate init expressions in the outer env
        return evalLetInits(initExprs, 0, env, new ArrayList<>(), initVals -> {
            Env doEnv = new Env(env);
            for (int i = 0; i < varNames.size(); i++) {
                doEnv.define(varNames.get(i), initVals.get(i));
            }
            return doLoop(varNames, stepExprs, testExpr, resultExprs, bodyExprs, doEnv, k);
        });
    }

    private Bounce doLoop(List<String> varNames, List<Object> stepExprs,
                           Object testExpr, List<Object> resultExprs,
                           List<Object> bodyExprs, Env doEnv, Cont k) throws EvalError {
        return new More(() -> eval(testExpr, doEnv, testResult -> {
            if (!isFalse(testResult)) {
                // Test passed - evaluate result expressions
                if (resultExprs.isEmpty()) return k.apply(VOID);
                return evalDoResults(resultExprs, 0, doEnv, k);
            }
            // Execute body, then step
            return doBodyThenStep(varNames, stepExprs, testExpr, resultExprs, bodyExprs, 0, doEnv, k);
        }));
    }

    private Bounce evalDoResults(List<Object> exprs, int idx, Env env, Cont k) throws EvalError {
        if (idx == exprs.size() - 1) {
            return new More(() -> eval(exprs.get(idx), env, k));
        }
        return new More(() -> eval(exprs.get(idx), env, ignored ->
            evalDoResults(exprs, idx + 1, env, k)));
    }

    private Bounce doBodyThenStep(List<String> varNames, List<Object> stepExprs,
                                   Object testExpr, List<Object> resultExprs,
                                   List<Object> bodyExprs, int bodyIdx,
                                   Env doEnv, Cont k) throws EvalError {
        if (bodyIdx < bodyExprs.size()) {
            return new More(() -> eval(bodyExprs.get(bodyIdx), doEnv, ignored ->
                doBodyThenStep(varNames, stepExprs, testExpr, resultExprs, bodyExprs, bodyIdx + 1, doEnv, k)));
        }
        // Evaluate all step expressions using current values (parallel update)
        return evalDoSteps(varNames, stepExprs, 0, doEnv, new ArrayList<>(), stepVals -> {
            // Update all variables with new values
            for (int i = 0; i < varNames.size(); i++) {
                if (stepExprs.get(i) != null) {
                    doEnv.define(varNames.get(i), stepVals.get(i));
                }
            }
            return doLoop(varNames, stepExprs, testExpr, resultExprs, bodyExprs, doEnv, k);
        });
    }

    private Bounce evalDoSteps(List<String> varNames, List<Object> stepExprs, int idx,
                                Env env, List<Object> acc, ArgsCont k) throws EvalError {
        if (idx >= varNames.size()) return k.apply(acc);
        if (stepExprs.get(idx) == null) {
            List<Object> newAcc = new ArrayList<>(acc);
            newAcc.add(null); // placeholder, won't be used
            return evalDoSteps(varNames, stepExprs, idx + 1, env, newAcc, k);
        }
        return new More(() -> eval(stepExprs.get(idx), env, val -> {
            List<Object> newAcc = new ArrayList<>(acc);
            newAcc.add(val);
            return evalDoSteps(varNames, stepExprs, idx + 1, env, newAcc, k);
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
            "string->list", "list->string",
            "char->integer", "integer->char",
            "apply", "procedure?",
            "call/cc", "call-with-current-continuation",
            "abs", "modulo", "remainder", "quotient", "min", "max", "expt",
            "zero?", "positive?", "negative?", "odd?", "even?",
            "list-ref", "list-tail", "list?", "assoc",
            "equal?", "eq?", "eqv?",
            "vector", "make-vector", "vector-ref", "vector-set!",
            "vector-length", "vector?", "vector->list", "list->vector",
            "char-alphabetic?", "char-numeric?", "char-upcase", "char-downcase",
            "char=?", "char<?",
            "string=?", "string<?", "string-ci=?", "string-upcase", "string-downcase",
            "map"
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
                if (idx < 0 || idx >= s.length()) throw posError("string-set!: index out of range");
                s.setChar(idx, c.value());
                yield VOID;
            }
            case "string-copy" -> {
                requireArgCount(args, 1, "string-copy");
                if (!(args.get(0) instanceof SchemeString s)) throw posError("string-copy: expected string");
                yield new SchemeString(s.value());
            }
            case "string->list" -> {
                requireArgCount(args, 1, "string->list");
                if (!(args.get(0) instanceof SchemeString s)) throw posError("string->list: expected string");
                Object result = NIL;
                String str = s.value();
                for (int i = str.length() - 1; i >= 0; i--) {
                    result = new Pair(new SchemeChar(str.charAt(i)), result);
                }
                yield result;
            }
            case "list->string" -> {
                requireArgCount(args, 1, "list->string");
                StringBuilder sb = new StringBuilder();
                Object lst = args.get(0);
                while (lst instanceof Pair p) {
                    if (!(p.car instanceof SchemeChar c)) throw posError("list->string: expected char in list");
                    sb.append(c.value());
                    lst = p.cdr;
                }
                yield new SchemeString(sb.toString());
            }
            case "char->integer" -> {
                requireArgCount(args, 1, "char->integer");
                if (!(args.get(0) instanceof SchemeChar c)) throw posError("char->integer: expected char");
                yield (long) c.value();
            }
            case "integer->char" -> {
                requireArgCount(args, 1, "integer->char");
                long n = requireLong(args.get(0), "integer->char");
                yield new SchemeChar((char) n);
            }
            case "abs" -> {
                requireArgCount(args, 1, "abs");
                yield Math.abs(requireLong(args.get(0), "abs"));
            }
            case "modulo" -> {
                requireArgCount(args, 2, "modulo");
                long a = requireLong(args.get(0), "modulo");
                long b = requireLong(args.get(1), "modulo");
                yield Math.floorMod(a, b);
            }
            case "remainder" -> {
                requireArgCount(args, 2, "remainder");
                long a = requireLong(args.get(0), "remainder");
                long b = requireLong(args.get(1), "remainder");
                yield a % b;
            }
            case "quotient" -> {
                requireArgCount(args, 2, "quotient");
                long a = requireLong(args.get(0), "quotient");
                long b = requireLong(args.get(1), "quotient");
                long q = a / b;
                // truncate toward zero (Java default for long division)
                yield q;
            }
            case "min" -> {
                if (args.isEmpty()) throw posError("min: expected at least 1 argument");
                long result = requireLong(args.get(0), "min");
                for (int i = 1; i < args.size(); i++) result = Math.min(result, requireLong(args.get(i), "min"));
                yield result;
            }
            case "max" -> {
                if (args.isEmpty()) throw posError("max: expected at least 1 argument");
                long result = requireLong(args.get(0), "max");
                for (int i = 1; i < args.size(); i++) result = Math.max(result, requireLong(args.get(i), "max"));
                yield result;
            }
            case "expt" -> {
                requireArgCount(args, 2, "expt");
                long base = requireLong(args.get(0), "expt");
                long exp = requireLong(args.get(1), "expt");
                long result = 1;
                for (long i = 0; i < exp; i++) result *= base;
                yield result;
            }
            case "zero?" -> { requireArgCount(args, 1, "zero?"); yield requireLong(args.get(0), "zero?") == 0; }
            case "positive?" -> { requireArgCount(args, 1, "positive?"); yield requireLong(args.get(0), "positive?") > 0; }
            case "negative?" -> { requireArgCount(args, 1, "negative?"); yield requireLong(args.get(0), "negative?") < 0; }
            case "odd?" -> { requireArgCount(args, 1, "odd?"); yield requireLong(args.get(0), "odd?") % 2 != 0; }
            case "even?" -> { requireArgCount(args, 1, "even?"); yield requireLong(args.get(0), "even?") % 2 == 0; }
            case "list-ref" -> {
                requireArgCount(args, 2, "list-ref");
                Object lst = args.get(0);
                int idx = (int) requireLong(args.get(1), "list-ref");
                for (int i = 0; i < idx; i++) {
                    if (!(lst instanceof Pair p)) throw posError("list-ref: index out of range");
                    lst = p.cdr;
                }
                if (!(lst instanceof Pair p)) throw posError("list-ref: index out of range");
                yield p.car;
            }
            case "list-tail" -> {
                requireArgCount(args, 2, "list-tail");
                Object lst = args.get(0);
                int idx = (int) requireLong(args.get(1), "list-tail");
                for (int i = 0; i < idx; i++) {
                    if (!(lst instanceof Pair p)) throw posError("list-tail: index out of range");
                    lst = p.cdr;
                }
                yield lst;
            }
            case "list?" -> {
                requireArgCount(args, 1, "list?");
                Object lst = args.get(0);
                while (lst instanceof Pair p) lst = p.cdr;
                yield lst == NIL;
            }
            case "assoc" -> {
                requireArgCount(args, 2, "assoc");
                Object key = args.get(0);
                Object alist = args.get(1);
                while (alist instanceof Pair p) {
                    if (!(p.car instanceof Pair entry)) throw posError("assoc: not an alist");
                    if (schemeEqual(key, entry.car)) yield (Object) p.car;
                    alist = p.cdr;
                }
                yield Boolean.FALSE;
            }
            case "equal?" -> {
                requireArgCount(args, 2, "equal?");
                yield schemeEqual(args.get(0), args.get(1));
            }
            case "eq?" -> {
                requireArgCount(args, 2, "eq?");
                Object a = args.get(0), b = args.get(1);
                if (a instanceof Long la && b instanceof Long lb) yield la.equals(lb);
                if (a instanceof String && b instanceof String) yield a.equals(b);
                yield a == b;
            }
            case "eqv?" -> {
                requireArgCount(args, 2, "eqv?");
                yield schemeEqv(args.get(0), args.get(1));
            }
            case "vector" -> {
                Object[] data = args.toArray();
                yield new SchemeVector(data);
            }
            case "make-vector" -> {
                if (args.size() < 1 || args.size() > 2) throw posError("make-vector: expected 1-2 arguments");
                int size = (int) requireLong(args.get(0), "make-vector");
                Object fill = args.size() == 2 ? args.get(1) : 0L;
                Object[] data = new Object[size];
                java.util.Arrays.fill(data, fill);
                yield new SchemeVector(data);
            }
            case "vector-ref" -> {
                requireArgCount(args, 2, "vector-ref");
                if (!(args.get(0) instanceof SchemeVector v)) throw posError("vector-ref: expected vector");
                int idx = (int) requireLong(args.get(1), "vector-ref");
                yield v.ref(idx);
            }
            case "vector-set!" -> {
                requireArgCount(args, 3, "vector-set!");
                if (!(args.get(0) instanceof SchemeVector v)) throw posError("vector-set!: expected vector");
                int idx = (int) requireLong(args.get(1), "vector-set!");
                v.set(idx, args.get(2));
                yield VOID;
            }
            case "vector-length" -> {
                requireArgCount(args, 1, "vector-length");
                if (!(args.get(0) instanceof SchemeVector v)) throw posError("vector-length: expected vector");
                yield (long) v.length();
            }
            case "vector?" -> {
                requireArgCount(args, 1, "vector?");
                yield args.get(0) instanceof SchemeVector;
            }
            case "vector->list" -> {
                requireArgCount(args, 1, "vector->list");
                if (!(args.get(0) instanceof SchemeVector v)) throw posError("vector->list: expected vector");
                Object result = NIL;
                for (int i = v.length() - 1; i >= 0; i--) result = new Pair(v.ref(i), result);
                yield result;
            }
            case "list->vector" -> {
                requireArgCount(args, 1, "list->vector");
                List<Object> elems = new ArrayList<>();
                Object lst = args.get(0);
                while (lst instanceof Pair p) { elems.add(p.car); lst = p.cdr; }
                yield new SchemeVector(elems.toArray());
            }
            case "char-alphabetic?" -> {
                requireArgCount(args, 1, "char-alphabetic?");
                if (!(args.get(0) instanceof SchemeChar c)) throw posError("char-alphabetic?: expected char");
                yield Character.isLetter(c.value());
            }
            case "char-numeric?" -> {
                requireArgCount(args, 1, "char-numeric?");
                if (!(args.get(0) instanceof SchemeChar c)) throw posError("char-numeric?: expected char");
                yield Character.isDigit(c.value());
            }
            case "char-upcase" -> {
                requireArgCount(args, 1, "char-upcase");
                if (!(args.get(0) instanceof SchemeChar c)) throw posError("char-upcase: expected char");
                yield new SchemeChar(Character.toUpperCase(c.value()));
            }
            case "char-downcase" -> {
                requireArgCount(args, 1, "char-downcase");
                if (!(args.get(0) instanceof SchemeChar c)) throw posError("char-downcase: expected char");
                yield new SchemeChar(Character.toLowerCase(c.value()));
            }
            case "char=?" -> {
                requireArgCount(args, 2, "char=?");
                if (!(args.get(0) instanceof SchemeChar a)) throw posError("char=?: expected char");
                if (!(args.get(1) instanceof SchemeChar b)) throw posError("char=?: expected char");
                yield a.value() == b.value();
            }
            case "char<?" -> {
                requireArgCount(args, 2, "char<?");
                if (!(args.get(0) instanceof SchemeChar a)) throw posError("char<?: expected char");
                if (!(args.get(1) instanceof SchemeChar b)) throw posError("char<?: expected char");
                yield a.value() < b.value();
            }
            case "string=?" -> {
                requireArgCount(args, 2, "string=?");
                if (!(args.get(0) instanceof SchemeString a)) throw posError("string=?: expected string");
                if (!(args.get(1) instanceof SchemeString b)) throw posError("string=?: expected string");
                yield a.value().equals(b.value());
            }
            case "string<?" -> {
                requireArgCount(args, 2, "string<?");
                if (!(args.get(0) instanceof SchemeString a)) throw posError("string<?: expected string");
                if (!(args.get(1) instanceof SchemeString b)) throw posError("string<?: expected string");
                yield a.value().compareTo(b.value()) < 0;
            }
            case "string-ci=?" -> {
                requireArgCount(args, 2, "string-ci=?");
                if (!(args.get(0) instanceof SchemeString a)) throw posError("string-ci=?: expected string");
                if (!(args.get(1) instanceof SchemeString b)) throw posError("string-ci=?: expected string");
                yield a.value().equalsIgnoreCase(b.value());
            }
            case "string-upcase" -> {
                requireArgCount(args, 1, "string-upcase");
                if (!(args.get(0) instanceof SchemeString s)) throw posError("string-upcase: expected string");
                yield new SchemeString(s.value().toUpperCase());
            }
            case "string-downcase" -> {
                requireArgCount(args, 1, "string-downcase");
                if (!(args.get(0) instanceof SchemeString s)) throw posError("string-downcase: expected string");
                yield new SchemeString(s.value().toLowerCase());
            }
            case "map" -> throw posError("map: handled in eval");
            default -> throw posError("unbound variable: " + proc);
        };
    }

    private Bounce cpsMap(Object fn, List<Object> lists, Cont k) throws EvalError {
        // Check if any list is empty
        for (Object lst : lists) {
            if (lst == NIL) return k.apply(NIL);
        }
        // Extract cars and cdrs
        List<Object> cars = new ArrayList<>();
        List<Object> cdrs = new ArrayList<>();
        for (Object lst : lists) {
            if (!(lst instanceof Pair p)) throw posError("map: expected list");
            cars.add(p.car);
            cdrs.add(p.cdr);
        }
        return new More(() -> applyProc(fn, cars, headVal ->
            new More(() -> cpsMap(fn, cdrs, tailVal ->
                k.apply(new Pair(headVal, tailVal))
            ))
        ));
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

    private boolean schemeEqual(Object a, Object b) {
        if (a == b) return true;
        if (a instanceof Long la && b instanceof Long lb) return la.equals(lb);
        if (a instanceof Boolean ba && b instanceof Boolean bb) return ba.equals(bb);
        if (a instanceof String sa && b instanceof String sb) return sa.equals(sb);
        if (a instanceof SchemeString sa && b instanceof SchemeString sb) return sa.value().equals(sb.value());
        if (a instanceof SchemeChar ca && b instanceof SchemeChar cb) return ca.value() == cb.value();
        if (a instanceof Pair pa && b instanceof Pair pb) return schemeEqual(pa.car, pb.car) && schemeEqual(pa.cdr, pb.cdr);
        if (a instanceof SchemeVector va && b instanceof SchemeVector vb) {
            if (va.length() != vb.length()) return false;
            for (int i = 0; i < va.length(); i++) {
                if (!schemeEqual(va.ref(i), vb.ref(i))) return false;
            }
            return true;
        }
        return false;
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

    static class SchemeVector {
        final Object[] data;
        SchemeVector(Object[] data) { this.data = data; }
        int length() { return data.length; }
        Object ref(int i) { return data[i]; }
        void set(int i, Object v) { data[i] = v; }
    }
}
