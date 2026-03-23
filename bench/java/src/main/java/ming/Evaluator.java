package ming;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

public class Evaluator {

    private static final Object VOID = new Object() {
        @Override public String toString() { return "#<void>"; }
    };

    // Scheme empty list
    static final Object NIL = new Object() {
        @Override public String toString() { return "()"; }
    };

    // Scheme pair (cons cell)
    static class Pair {
        Object car;
        Object cdr;
        Pair(Object car, Object cdr) {
            this.car = car;
            this.cdr = cdr;
        }
    }

    // Source position wrapper for parsed AST nodes
    record Located(Object value, int line, int col) {}

    // Token with source position
    record Token(Object value, int line, int col) {}

    // Current error position context
    private int errLine = 1;
    private int errCol = 1;

    // Output capture buffer (null when not capturing)
    private StringBuilder outputBuf;

    private EvalError posError(String msg) {
        return new EvalError(errLine + ":" + errCol + ": " + msg);
    }

    public String evalStr(String input) throws EvalError {
        List<Token> tokens = tokenize(input);
        int[] pos = {0};
        Env env = createGlobalEnv();
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
        List<Token> tokens = tokenize(input);
        int[] pos = {0};
        Env env = createGlobalEnv();
        this.outputBuf = new StringBuilder();
        Object lastResult = null;
        while (pos[0] < tokens.size()) {
            Object expr = parse(tokens, pos);
            lastResult = eval(expr, env);
        }
        String resultStr = (lastResult == null || lastResult == VOID) ? "" : schemeToString(lastResult);
        return new EvalResult(resultStr, this.outputBuf.toString());
    }

    private Env createGlobalEnv() {
        return new Env(null);
    }

    // --- Environment ---

    private static class Env {
        final Env parent;
        final Map<String, Object> bindings = new HashMap<>();

        Env(Env parent) {
            this.parent = parent;
        }

        Object lookup(String name) throws EvalError {
            if (bindings.containsKey(name)) return bindings.get(name);
            if (parent != null) return parent.lookup(name);
            return null; // signal not found
        }

        void define(String name, Object value) {
            bindings.put(name, value);
        }

        boolean set(String name, Object value) {
            if (bindings.containsKey(name)) {
                bindings.put(name, value);
                return true;
            }
            if (parent != null) return parent.set(name, value);
            return false;
        }
    }

    // --- Lambda ---

    private static class Lambda {
        final List<String> params;
        final String restParam; // null if no rest parameter
        final Object body;
        final Env closure;

        Lambda(List<String> params, String restParam, Object body, Env closure) {
            this.params = params;
            this.restParam = restParam;
            this.body = body;
            this.closure = closure;
        }
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
                i++;
                line++;
                col = 1;
            } else if (Character.isWhitespace(c)) {
                i++;
                col++;
            } else if (c == ';') {
                while (i < len && input.charAt(i) != '\n') { i++; col++; }
            } else if (c == '(') {
                tokens.add(new Token("(", line, col));
                i++;
                col++;
            } else if (c == ')') {
                tokens.add(new Token(")", line, col));
                i++;
                col++;
            } else if (c == '\'') {
                tokens.add(new Token("'", line, col));
                i++;
                col++;
            } else if (c == '"') {
                int startCol = col;
                StringBuilder sb = new StringBuilder();
                i++; col++; // skip opening quote
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
                            sb.append('\n');
                            i++; line++; col = 1;
                            continue;
                        }
                        sb.append(input.charAt(i));
                    }
                    i++; col++;
                }
                if (i >= len) throw new EvalError("unterminated string");
                i++; col++; // skip closing quote
                tokens.add(new Token(new SchemeString(sb.toString()), line, startCol));
            } else if (c == '#') {
                int startCol = col;
                if (i + 1 < len) {
                    char next = input.charAt(i + 1);
                    if (next == 't') {
                        // Check it's not a longer token like #true
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
                        // Character literal: #\<char> or #\space, #\newline, etc.
                        if (i + 2 >= len) throw new EvalError(line + ":" + col + ": unexpected end of input in character literal");
                        // Read the character name
                        int start = i + 2;
                        int ci = start;
                        // If next char is a letter, read a full word (for named chars like #\space)
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
                            // Single non-letter char like #\( or #\)
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
                // symbol or number
                int startCol = col;
                StringBuilder sb = new StringBuilder();
                while (i < len && !Character.isWhitespace(input.charAt(i))
                        && input.charAt(i) != '(' && input.charAt(i) != ')'
                        && input.charAt(i) != '"' && input.charAt(i) != ';'
                        && input.charAt(i) != '\'') {
                    sb.append(input.charAt(i));
                    i++; col++;
                }
                String tok = sb.toString();
                try {
                    tokens.add(new Token(Long.parseLong(tok), line, startCol));
                } catch (NumberFormatException e) {
                    tokens.add(new Token(tok, line, startCol)); // symbol
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
        if (pos[0] >= tokens.size()) {
            throw new EvalError("unexpected end of input");
        }
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
            pos[0]++; // skip ')'
            return new Located(list, tok.line(), tok.col());
        } else if (tok.value().equals(")")) {
            throw new EvalError(tok.line() + ":" + tok.col() + ": unexpected )");
        } else {
            return new Located(tok.value(), tok.line(), tok.col());
        }
    }

    // Convert a parsed List<Object> to Scheme pair structure (for quote)
    private Object listToScheme(Object parsed) {
        if (parsed instanceof Located loc) {
            return listToScheme(loc.value());
        }
        if (parsed instanceof List<?> list) {
            Object result = NIL;
            for (int i = list.size() - 1; i >= 0; i--) {
                result = new Pair(listToScheme(list.get(i)), result);
            }
            return result;
        }
        return parsed;
    }

    // --- Evaluator ---

    @SuppressWarnings("unchecked")
    private Object eval(Object expr, Env env) throws EvalError {
        while (true) {
            // Unwrap Located to get position context
            if (expr instanceof Located loc) {
                errLine = loc.line();
                errCol = loc.col();
                expr = loc.value();
                continue;
            }

            if (expr instanceof Long || expr instanceof Boolean || expr instanceof SchemeString || expr instanceof SchemeChar) {
                return expr;
            }
            if (expr instanceof String sym) {
                if (isPrimitive(sym)) {
                    return sym;
                }
                Object val = env.lookup(sym);
                if (val == null) {
                    throw posError("unbound variable: " + sym);
                }
                return val;
            }
            if (expr instanceof List<?> list) {
                if (list.isEmpty()) {
                    throw posError("empty application");
                }
                Object head = list.get(0);

                // Check for special form names (unwrap Located if needed)
                String formName = null;
                if (head instanceof Located locHead) {
                    if (locHead.value() instanceof String s) formName = s;
                } else if (head instanceof String s) {
                    formName = s;
                }

                // Special forms
                if (formName != null) {
                    switch (formName) {
                        case "define" -> {
                            if (list.size() < 3) throw posError("define: bad syntax");
                            Object target = list.get(1);
                            if (target instanceof Located lt) target = lt.value();
                            if (target instanceof String name) {
                                Object val = eval(list.get(2), env);
                                env.define(name, val);
                                return VOID;
                            } else if (target instanceof List<?> sig) {
                                String fname = null;
                                Object first = sig.isEmpty() ? null : sig.get(0);
                                if (first instanceof Located lf) first = lf.value();
                                if (first instanceof String s) fname = s;
                                if (fname == null) {
                                    throw posError("define: bad syntax");
                                }
                                List<String> params = new ArrayList<>();
                                String restParam = null;
                                for (int i = 1; i < sig.size(); i++) {
                                    Object p = sig.get(i);
                                    if (p instanceof Located lp) p = lp.value();
                                    if (!(p instanceof String ps)) {
                                        throw posError("define: parameter must be a symbol");
                                    }
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
                                    for (int i = 2; i < list.size(); i++) {
                                        beginBody.add(list.get(i));
                                    }
                                    body = beginBody;
                                }
                                Lambda lambda = new Lambda(params, restParam, body, env);
                                env.define(fname, lambda);
                                return VOID;
                            }
                            throw posError("define: bad syntax");
                        }
                        case "if" -> {
                            if (list.size() < 3 || list.size() > 4) {
                                throw posError("if: bad syntax");
                            }
                            Object cond = eval(list.get(1), env);
                            if (!isFalse(cond)) {
                                expr = list.get(2);
                                continue; // TCO
                            } else if (list.size() == 4) {
                                expr = list.get(3);
                                continue; // TCO
                            }
                            return VOID;
                        }
                        case "quote" -> {
                            if (list.size() != 2) throw posError("quote: expected 1 argument");
                            return listToScheme(list.get(1));
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
                                    if (!(p instanceof String ps)) {
                                        throw posError("lambda: parameter must be a symbol");
                                    }
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
                                // (lambda args body) - single symbol means all args go to rest
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
                                for (int i = 2; i < list.size(); i++) {
                                    beginBody.add(list.get(i));
                                }
                                body = beginBody;
                            }
                            return new Lambda(params, restParam, body, env);
                        }
                        case "and" -> {
                            if (list.size() == 1) return Boolean.TRUE;
                            for (int i = 1; i < list.size() - 1; i++) {
                                Object result = eval(list.get(i), env);
                                if (isFalse(result)) return result;
                            }
                            expr = list.get(list.size() - 1);
                            continue; // TCO for last expression
                        }
                        case "or" -> {
                            if (list.size() == 1) return Boolean.FALSE;
                            for (int i = 1; i < list.size() - 1; i++) {
                                Object result = eval(list.get(i), env);
                                if (!isFalse(result)) return result;
                            }
                            expr = list.get(list.size() - 1);
                            continue; // TCO for last expression
                        }
                        case "let" -> {
                            Object[] letResult = prepareLet(list, env);
                            expr = letResult[0];
                            env = (Env) letResult[1];
                            continue; // TCO
                        }
                        case "begin" -> {
                            if (list.size() == 1) return VOID;
                            for (int i = 1; i < list.size() - 1; i++) {
                                eval(list.get(i), env);
                            }
                            expr = list.get(list.size() - 1);
                            continue; // TCO for last expression
                        }
                        case "set!" -> {
                            if (list.size() != 3) throw posError("set!: bad syntax");
                            Object nameObj = list.get(1);
                            if (nameObj instanceof Located ln) nameObj = ln.value();
                            if (!(nameObj instanceof String name)) throw posError("set!: expected symbol");
                            Object val = eval(list.get(2), env);
                            if (!env.set(name, val)) throw posError("set!: unbound variable: " + name);
                            return VOID;
                        }
                        case "cond" -> {
                            Object condTail = evalCondTail(list, env);
                            if (condTail == null) return VOID;
                            if (condTail instanceof Evaluated ev) return ev.value;
                            expr = condTail;
                            continue; // TCO
                        }
                    }
                }

                // Procedure call
                Object proc = eval(head, env);
                List<Object> args = new ArrayList<>();
                for (int i = 1; i < list.size(); i++) {
                    args.add(eval(list.get(i), env));
                }
                // Restore position to the call site for error reporting
                if (head instanceof Located lh) {
                    errLine = lh.line();
                    errCol = lh.col();
                }
                // Handle apply
                if (proc instanceof String p && p.equals("apply")) {
                    Object[] applyResult = handleApply(args);
                    proc = applyResult[0];
                    @SuppressWarnings("unchecked")
                    List<Object> newArgs = (List<Object>) applyResult[1];
                    args = newArgs;
                }
                // TCO for lambda calls
                if (proc instanceof Lambda lambda) {
                    if (lambda.restParam != null) {
                        if (args.size() < lambda.params.size()) {
                            throw posError("wrong number of arguments: expected at least " + lambda.params.size() + ", got " + args.size());
                        }
                    } else {
                        if (args.size() != lambda.params.size()) {
                            throw posError("wrong number of arguments: expected " + lambda.params.size() + ", got " + args.size());
                        }
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
                    expr = lambda.body;
                    env = callEnv;
                    continue; // TCO
                }
                if (proc instanceof String p && isPrimitive(p)) {
                    return applyPrimitive(p, args);
                }
                throw posError("not a procedure: " + schemeToString(proc));
            }
            throw posError("cannot evaluate: " + expr);
        }
    }

    // Returns [body, env] for TCO continuation, sets up let bindings
    private Object[] prepareLet(List<?> list, Env env) throws EvalError {
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
        if (!(bindingsObj instanceof List<?> bindings)) {
            throw posError("let: bad syntax");
        }
        List<String> paramNames = new ArrayList<>();
        List<Object> initExprs = new ArrayList<>();
        for (Object b : bindings) {
            if (b instanceof Located lbb) b = lbb.value();
            if (!(b instanceof List<?> binding) || binding.size() != 2) {
                throw posError("let: bad binding");
            }
            Object pnameObj = binding.get(0);
            if (pnameObj instanceof Located lp) pnameObj = lp.value();
            if (!(pnameObj instanceof String pname)) {
                throw posError("let: binding name must be a symbol");
            }
            paramNames.add(pname);
            initExprs.add(binding.get(1));
        }
        List<Object> initVals = new ArrayList<>();
        for (Object e : initExprs) {
            initVals.add(eval(e, env));
        }
        Object body;
        if (list.size() - idx - 1 == 1) {
            body = list.get(idx + 1);
        } else {
            List<Object> beginBody = new ArrayList<>();
            beginBody.add("begin");
            for (int i = idx + 1; i < list.size(); i++) {
                beginBody.add(list.get(i));
            }
            body = beginBody;
        }
        if (name != null) {
            Env letEnv = new Env(env);
            Lambda lambda = new Lambda(paramNames, null, body, letEnv);
            letEnv.define(name, lambda);
            for (int i = 0; i < paramNames.size(); i++) {
                letEnv.define(paramNames.get(i), initVals.get(i));
            }
            return new Object[]{body, letEnv};
        } else {
            Env letEnv = new Env(env);
            for (int i = 0; i < paramNames.size(); i++) {
                letEnv.define(paramNames.get(i), initVals.get(i));
            }
            return new Object[]{body, letEnv};
        }
    }

    // Sentinel wrapper for already-evaluated values returned from cond
    private static class Evaluated {
        final Object value;
        Evaluated(Object value) { this.value = value; }
    }

    // Returns tail expression for TCO, Evaluated for already-computed values, or null for VOID
    private Object evalCondTail(List<?> list, Env env) throws EvalError {
        for (int i = 1; i < list.size(); i++) {
            Object clauseObj = list.get(i);
            if (clauseObj instanceof Located lc) clauseObj = lc.value();
            if (!(clauseObj instanceof List<?> clause) || clause.isEmpty()) {
                throw posError("cond: bad clause");
            }
            Object test = clause.get(0);
            Object testVal2 = test;
            if (testVal2 instanceof Located lt) testVal2 = lt.value();
            if (testVal2 instanceof String s && s.equals("else")) {
                if (clause.size() == 1) return new Evaluated(Boolean.TRUE);
                for (int j = 1; j < clause.size() - 1; j++) {
                    eval(clause.get(j), env);
                }
                return clause.get(clause.size() - 1);
            }
            Object testVal = eval(test, env);
            if (!isFalse(testVal)) {
                if (clause.size() == 1) return new Evaluated(testVal);
                for (int j = 1; j < clause.size() - 1; j++) {
                    eval(clause.get(j), env);
                }
                return clause.get(clause.size() - 1);
            }
        }
        return null;
    }

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
            "apply"
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
            case "<" -> {
                requireArgCount(args, 2, "<");
                yield requireLong(args.get(0), "<") < requireLong(args.get(1), "<");
            }
            case ">" -> {
                requireArgCount(args, 2, ">");
                yield requireLong(args.get(0), ">") > requireLong(args.get(1), ">");
            }
            case "=" -> {
                requireArgCount(args, 2, "=");
                yield requireLong(args.get(0), "=") == requireLong(args.get(1), "=");
            }
            case "<=" -> {
                requireArgCount(args, 2, "<=");
                yield requireLong(args.get(0), "<=") <= requireLong(args.get(1), "<=");
            }
            case ">=" -> {
                requireArgCount(args, 2, ">=");
                yield requireLong(args.get(0), ">=") >= requireLong(args.get(1), ">=");
            }
            case "cons" -> {
                requireArgCount(args, 2, "cons");
                yield new Pair(args.get(0), args.get(1));
            }
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
            case "null?" -> {
                requireArgCount(args, 1, "null?");
                yield args.get(0) == NIL;
            }
            case "list" -> {
                Object result = NIL;
                for (int i = args.size() - 1; i >= 0; i--) {
                    result = new Pair(args.get(i), result);
                }
                yield result;
            }
            case "length" -> {
                requireArgCount(args, 1, "length");
                Object lst = args.get(0);
                long len = 0;
                while (lst instanceof Pair p) {
                    len++;
                    lst = p.cdr;
                }
                if (lst != NIL) throw posError("length: not a proper list");
                yield len;
            }
            case "append" -> {
                if (args.isEmpty()) yield NIL;
                // Append all lists
                Object result = args.get(args.size() - 1);
                for (int i = args.size() - 2; i >= 0; i--) {
                    result = appendTwo(args.get(i), result);
                }
                yield result;
            }
            case "not" -> {
                requireArgCount(args, 1, "not");
                yield isFalse(args.get(0)) ? Boolean.TRUE : Boolean.FALSE;
            }
            case "string?" -> {
                requireArgCount(args, 1, "string?");
                yield args.get(0) instanceof SchemeString;
            }
            case "number?" -> {
                requireArgCount(args, 1, "number?");
                yield args.get(0) instanceof Long;
            }
            case "boolean?" -> {
                requireArgCount(args, 1, "boolean?");
                yield args.get(0) instanceof Boolean;
            }
            case "pair?" -> {
                requireArgCount(args, 1, "pair?");
                yield args.get(0) instanceof Pair;
            }
            case "symbol?" -> {
                requireArgCount(args, 1, "symbol?");
                yield args.get(0) instanceof String;
            }
            case "char?" -> {
                requireArgCount(args, 1, "char?");
                yield args.get(0) instanceof SchemeChar;
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
                try {
                    yield Long.parseLong(s.value());
                } catch (NumberFormatException e) {
                    yield Boolean.FALSE;
                }
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

    // Returns [proc, argsList] for apply
    private Object[] handleApply(List<Object> args) throws EvalError {
        if (args.size() < 2) throw posError("apply: expected at least 2 arguments");
        Object proc = args.get(0);
        // Last arg must be a list; prefix args are prepended
        Object lastArg = args.get(args.size() - 1);
        List<Object> newArgs = new ArrayList<>();
        for (int i = 1; i < args.size() - 1; i++) {
            newArgs.add(args.get(i));
        }
        // Flatten the last argument (a Scheme list) into newArgs
        Object cur = lastArg;
        while (cur instanceof Pair p) {
            newArgs.add(p.car);
            cur = p.cdr;
        }
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
        if (val instanceof Lambda) return "#<procedure>";
        if (val == VOID) return "#<void>";
        return val.toString();
    }

    // Internal type for Scheme strings (mutable for string-set!)
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

    // Internal type for Scheme characters
    record SchemeChar(char value) {}
}
