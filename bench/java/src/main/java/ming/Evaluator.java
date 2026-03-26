package ming;

import java.util.ArrayList;
import java.util.List;

public class Evaluator {

    private final Environment globalEnv = new Environment(null);
    private StringBuilder outputBuffer = null;

    private static final String[] BUILTIN_NAMES = {
        "+", "-", "*", "/", "<", ">", "=", "<=", ">=",
        "not",
        "cons", "car", "cdr", "null?", "list", "length", "append",
        "number?", "string?", "boolean?", "pair?", "symbol?", "char?",
        "display", "write", "newline",
        "string-append", "string-length", "substring", "string-ref",
        "string->number", "number->string",
        "symbol->string", "string->symbol",
        "string-copy", "string-set!",
        "apply"
    };

    {
        for (String name : BUILTIN_NAMES) {
            globalEnv.define(name, new BuiltinProcedure(name));
        }
    }

    // Source position tracking
    private record Token(String value, int line, int col) {}
    private record Located(Object expr, int line, int col) {}

    private static Object unwrap(Object o) {
        return o instanceof Located loc ? loc.expr() : o;
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
        return schemeToString(lastResult);
    }

    public EvalResult evalStrWithOutput(String input) throws EvalError {
        outputBuffer = new StringBuilder();
        try {
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
            return new EvalResult(schemeToString(lastResult), outputBuffer.toString());
        } finally {
            outputBuffer = null;
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
                line++;
                col = 1;
                i++;
            } else if (Character.isWhitespace(c)) {
                col++;
                i++;
            } else if (c == ';') {
                while (i < len && input.charAt(i) != '\n') {
                    i++;
                    col++;
                }
            } else if (c == '\'') {
                tokens.add(new Token("'", line, col));
                i++;
                col++;
            } else if (c == '(') {
                tokens.add(new Token("(", line, col));
                i++;
                col++;
            } else if (c == ')') {
                tokens.add(new Token(")", line, col));
                i++;
                col++;
            } else if (c == '"') {
                int startLine = line;
                int startCol = col;
                StringBuilder sb = new StringBuilder();
                sb.append('"');
                i++;
                col++;
                while (i < len && input.charAt(i) != '"') {
                    if (input.charAt(i) == '\\') {
                        sb.append(input.charAt(i));
                        i++;
                        col++;
                        if (i < len) {
                            sb.append(input.charAt(i));
                            i++;
                            col++;
                        }
                    } else {
                        if (input.charAt(i) == '\n') {
                            line++;
                            col = 1;
                        } else {
                            col++;
                        }
                        sb.append(input.charAt(i));
                        i++;
                    }
                }
                if (i < len) {
                    sb.append('"');
                    i++;
                    col++;
                }
                tokens.add(new Token(sb.toString(), startLine, startCol));
            } else if (c == '#') {
                int startCol = col;
                if (i + 1 < len) {
                    char next = input.charAt(i + 1);
                    if (next == 't') {
                        tokens.add(new Token("#t", line, startCol));
                        i += 2;
                        col += 2;
                    } else if (next == 'f') {
                        tokens.add(new Token("#f", line, startCol));
                        i += 2;
                        col += 2;
                    } else if (next == '\\') {
                        // Character literal: #\x or #\space, #\newline, etc.
                        if (i + 2 < len) {
                            // Try to read a named character or single char
                            int charStart = i + 2;
                            int charEnd = charStart;
                            while (charEnd < len && !Character.isWhitespace(input.charAt(charEnd))
                                    && input.charAt(charEnd) != ')' && input.charAt(charEnd) != '('
                                    && input.charAt(charEnd) != '"' && input.charAt(charEnd) != ';') {
                                charEnd++;
                            }
                            String charName = input.substring(charStart, charEnd);
                            String tok = "#\\" + charName;
                            tokens.add(new Token(tok, line, startCol));
                            int tokLen = tok.length();
                            i += tokLen;
                            col += tokLen;
                        } else {
                            tokens.add(new Token("#\\", line, startCol));
                            i += 2;
                            col += 2;
                        }
                    } else {
                        String sym = readSymbol(input, i);
                        tokens.add(new Token(sym, line, startCol));
                        i += sym.length();
                        col += sym.length();
                    }
                } else {
                    tokens.add(new Token("#", line, startCol));
                    i++;
                    col++;
                }
            } else {
                int startCol = col;
                String sym = readSymbol(input, i);
                tokens.add(new Token(sym, line, startCol));
                i += sym.length();
                col += sym.length();
            }
        }
        return tokens;
    }

    private String readSymbol(String input, int start) {
        int i = start;
        int len = input.length();
        while (i < len) {
            char c = input.charAt(i);
            if (Character.isWhitespace(c) || c == '(' || c == ')' || c == '"' || c == ';' || c == '\'') {
                break;
            }
            i++;
        }
        return input.substring(start, i);
    }

    // --- Parser ---

    private Object parse(List<Token> tokens, int[] pos) throws EvalError {
        if (pos[0] >= tokens.size()) {
            throw new EvalError("unexpected end of input");
        }
        Token token = tokens.get(pos[0]);
        pos[0]++;

        if (token.value().equals("'")) {
            Object quoted = parse(tokens, pos);
            List<Object> quoteExpr = new ArrayList<>();
            quoteExpr.add(new SchemeSymbol("quote"));
            quoteExpr.add(quoted);
            return new Located(quoteExpr, token.line(), token.col());
        }

        if (token.value().equals("(")) {
            List<Object> list = new ArrayList<>();
            while (pos[0] < tokens.size() && !tokens.get(pos[0]).value().equals(")")) {
                list.add(parse(tokens, pos));
            }
            if (pos[0] >= tokens.size()) {
                throw new EvalError("missing closing parenthesis");
            }
            pos[0]++; // skip ')'
            return new Located(list, token.line(), token.col());
        } else if (token.value().equals(")")) {
            throw new EvalError("unexpected )");
        } else {
            return new Located(parseAtom(token.value()), token.line(), token.col());
        }
    }

    private Object parseAtom(String token) {
        if (token.equals("#t")) {
            return Boolean.TRUE;
        }
        if (token.equals("#f")) {
            return Boolean.FALSE;
        }
        if (token.startsWith("#\\")) {
            String charName = token.substring(2);
            if (charName.equals("space")) return new SchemeChar(' ');
            if (charName.equals("newline")) return new SchemeChar('\n');
            if (charName.equals("tab")) return new SchemeChar('\t');
            if (charName.length() == 1) return new SchemeChar(charName.charAt(0));
            return new SchemeChar(charName.charAt(0)); // fallback
        }
        if (token.startsWith("\"") && token.endsWith("\"")) {
            return token; // keep as quoted string
        }
        try {
            return Long.parseLong(token);
        } catch (NumberFormatException e) {
            return new SchemeSymbol(token);
        }
    }

    // --- Evaluator ---

    private static final Object VOID = new Object() {
        @Override public String toString() { return "#<void>"; }
    };

    // Convert a parsed Java List to a Scheme list (SchemePair chain ending in NIL)
    private Object javaListToSchemeList(List<?> javaList) {
        Object result = SchemeNil.INSTANCE;
        for (int i = javaList.size() - 1; i >= 0; i--) {
            Object elem = unwrap(javaList.get(i));
            if (elem instanceof List<?> subList) {
                elem = javaListToSchemeList(subList);
            }
            result = new SchemePair(elem, result);
        }
        return result;
    }

    @SuppressWarnings("unchecked")
    private Object eval(Object expr, Environment env) throws EvalError {
        // Unwrap Located and add position to any errors
        if (expr instanceof Located loc) {
            try {
                return eval(loc.expr(), env);
            } catch (EvalError e) {
                if (e.getMessage() != null && !e.getMessage().matches(".*\\d+:\\d+.*")) {
                    throw new EvalError(e.getMessage() + " [" + loc.line() + ":" + loc.col() + "]");
                }
                throw e;
            }
        }

        if (expr instanceof Long || expr instanceof Boolean || expr instanceof SchemeChar) {
            return expr;
        }
        if (expr instanceof String s) {
            return s;
        }
        if (expr instanceof SchemeString ss) {
            return ss;
        }
        if (expr instanceof SchemeSymbol sym) {
            return env.lookup(sym.name());
        }
        if (expr instanceof List<?> rawList) {
            List<Object> list = (List<Object>) rawList;
            if (list.isEmpty()) {
                throw new EvalError("empty application");
            }
            Object first = list.get(0);
            Object rawFirst = unwrap(first);

            // Special forms
            if (rawFirst instanceof SchemeSymbol sym) {
                String name = sym.name();
                List<Object> args = list.subList(1, list.size());

                switch (name) {
                    case "quote" -> {
                        if (args.size() != 1) throw new EvalError("quote: expected 1 argument");
                        Object quoted = unwrap(args.get(0));
                        if (quoted instanceof List<?> ql) {
                            return javaListToSchemeList(ql);
                        }
                        return quoted;
                    }
                    case "if" -> {
                        if (args.size() < 2 || args.size() > 3)
                            throw new EvalError("if: expected 2 or 3 arguments");
                        Object cond = eval(args.get(0), env);
                        if (!cond.equals(Boolean.FALSE)) {
                            return eval(args.get(1), env);
                        } else if (args.size() == 3) {
                            return eval(args.get(2), env);
                        }
                        return VOID;
                    }
                    case "define" -> {
                        if (args.size() < 2) throw new EvalError("define: bad syntax");
                        Object target = unwrap(args.get(0));
                        if (target instanceof SchemeSymbol s) {
                            Object val = eval(args.get(1), env);
                            env.define(s.name(), val);
                            return VOID;
                        }
                        if (target instanceof List<?> sig) {
                            // (define (f params...) body) or (define (f x . rest) body)
                            if (sig.isEmpty()) throw new EvalError("define: bad syntax");
                            String fname = ((SchemeSymbol) unwrap(sig.get(0))).name();
                            List<String> params = new ArrayList<>();
                            String restParam = null;
                            for (int i = 1; i < sig.size(); i++) {
                                String pname = ((SchemeSymbol) unwrap(sig.get(i))).name();
                                if (pname.equals(".")) {
                                    if (i + 1 < sig.size()) {
                                        restParam = ((SchemeSymbol) unwrap(sig.get(i + 1))).name();
                                    }
                                    break;
                                }
                                params.add(pname);
                            }
                            Object body;
                            if (args.size() == 2) {
                                body = args.get(1);
                            } else {
                                List<Object> beginList = new ArrayList<>();
                                beginList.add(new SchemeSymbol("begin"));
                                beginList.addAll(args.subList(1, args.size()));
                                body = beginList;
                            }
                            env.define(fname, new SchemeLambda(params, restParam, body, env));
                            return VOID;
                        }
                        throw new EvalError("define: bad syntax");
                    }
                    case "set!" -> {
                        if (args.size() != 2) throw new EvalError("set!: bad syntax");
                        Object target = unwrap(args.get(0));
                        if (!(target instanceof SchemeSymbol s))
                            throw new EvalError("set!: not a variable");
                        Object val = eval(args.get(1), env);
                        env.set(s.name(), val);
                        return VOID;
                    }
                    case "lambda" -> {
                        if (args.size() < 2) throw new EvalError("lambda: bad syntax");
                        Object paramObj = unwrap(args.get(0));
                        List<?> paramList = (List<?>) paramObj;
                        List<String> params = new ArrayList<>();
                        String restParam = null;
                        for (int i = 0; i < paramList.size(); i++) {
                            String pname = ((SchemeSymbol) unwrap(paramList.get(i))).name();
                            if (pname.equals(".")) {
                                if (i + 1 < paramList.size()) {
                                    restParam = ((SchemeSymbol) unwrap(paramList.get(i + 1))).name();
                                }
                                break;
                            }
                            params.add(pname);
                        }
                        Object body;
                        if (args.size() == 2) {
                            body = args.get(1);
                        } else {
                            List<Object> beginList = new ArrayList<>();
                            beginList.add(new SchemeSymbol("begin"));
                            beginList.addAll(args.subList(1, args.size()));
                            body = beginList;
                        }
                        return new SchemeLambda(params, restParam, body, env);
                    }
                    case "begin" -> {
                        Object result = VOID;
                        for (Object a : args) {
                            result = eval(a, env);
                        }
                        return result;
                    }
                    case "let" -> {
                        if (args.size() < 2) throw new EvalError("let: bad syntax");
                        Object first2 = unwrap(args.get(0));
                        if (first2 instanceof SchemeSymbol loopName) {
                            // Named let: (let name ((var init) ...) body ...)
                            if (args.size() < 3) throw new EvalError("let: bad syntax");
                            Object bindingsObj = unwrap(args.get(1));
                            List<?> bindings = (List<?>) bindingsObj;
                            List<String> params = new ArrayList<>();
                            List<Object> inits = new ArrayList<>();
                            for (Object binding : bindings) {
                                List<?> b = (List<?>) unwrap(binding);
                                params.add(((SchemeSymbol) unwrap(b.get(0))).name());
                                inits.add(eval(b.get(1), env));
                            }
                            Object body;
                            if (args.size() == 3) {
                                body = args.get(2);
                            } else {
                                List<Object> beginList = new ArrayList<>();
                                beginList.add(new SchemeSymbol("begin"));
                                beginList.addAll(args.subList(2, args.size()));
                                body = beginList;
                            }
                            Environment letEnv = new Environment(env);
                            SchemeLambda loopLam = new SchemeLambda(params, body, letEnv);
                            letEnv.define(loopName.name(), loopLam);
                            return apply(loopLam, inits);
                        }
                        // Regular let: (let ((var val) ...) body ...)
                        List<?> bindings = (List<?>) first2;
                        Environment letEnv = new Environment(env);
                        for (Object binding : bindings) {
                            List<?> b = (List<?>) unwrap(binding);
                            String varName = ((SchemeSymbol) unwrap(b.get(0))).name();
                            Object val = eval(b.get(1), env);
                            letEnv.define(varName, val);
                        }
                        Object result = VOID;
                        for (int i = 1; i < args.size(); i++) {
                            result = eval(args.get(i), letEnv);
                        }
                        return result;
                    }
                    case "cond" -> {
                        for (Object clause : args) {
                            List<?> cl = (List<?>) unwrap(clause);
                            if (cl.isEmpty()) throw new EvalError("cond: empty clause");
                            Object test = cl.get(0);
                            Object rawTest = unwrap(test);
                            if (rawTest instanceof SchemeSymbol s && s.name().equals("else")) {
                                Object result = VOID;
                                for (int i = 1; i < cl.size(); i++) {
                                    result = eval(cl.get(i), env);
                                }
                                return result;
                            }
                            Object testVal = eval(test, env);
                            if (!testVal.equals(Boolean.FALSE)) {
                                if (cl.size() == 1) return testVal;
                                Object result = VOID;
                                for (int i = 1; i < cl.size(); i++) {
                                    result = eval(cl.get(i), env);
                                }
                                return result;
                            }
                        }
                        return VOID;
                    }
                    // Builtins handled as special forms (unevaluated args for and/or)
                    case "and" -> {
                        Object result = Boolean.TRUE;
                        for (Object arg : args) {
                            result = eval(arg, env);
                            if (result.equals(Boolean.FALSE)) return Boolean.FALSE;
                        }
                        return result;
                    }
                    case "or" -> {
                        Object result = Boolean.FALSE;
                        for (Object arg : args) {
                            result = eval(arg, env);
                            if (!result.equals(Boolean.FALSE)) return result;
                        }
                        return result;
                    }
                    case "+", "-", "*", "/", "<", ">", "=", "<=", ">=",
                         "not",
                         "cons", "car", "cdr", "null?", "list", "length", "append",
                         "number?", "string?", "boolean?", "pair?", "symbol?", "char?",
                         "display", "write", "newline",
                         "string-append", "string-length", "substring", "string-ref",
                         "string->number", "number->string",
                         "symbol->string", "string->symbol",
                         "string-copy", "string-set!",
                         "apply" -> {
                        return evalBuiltin(name, args, env);
                    }
                    default -> {
                        // Fall through to procedure call
                    }
                }
            }

            // Procedure call: evaluate all, then apply
            Object proc = eval(first, env);
            List<Object> evaledArgs = new ArrayList<>();
            for (int i = 1; i < list.size(); i++) {
                evaledArgs.add(eval(list.get(i), env));
            }
            return apply(proc, evaledArgs);
        }
        throw new EvalError("cannot evaluate: " + expr);
    }

    private Object apply(Object proc, List<Object> args) throws EvalError {
        if (proc instanceof SchemeLambda lam) {
            if (lam.restParam != null) {
                if (args.size() < lam.params.size()) {
                    throw new EvalError("expected at least " + lam.params.size() + " arguments, got " + args.size());
                }
            } else {
                if (args.size() != lam.params.size()) {
                    throw new EvalError("expected " + lam.params.size() + " arguments, got " + args.size());
                }
            }
            Environment callEnv = new Environment(lam.closure);
            for (int i = 0; i < lam.params.size(); i++) {
                callEnv.define(lam.params.get(i), args.get(i));
            }
            if (lam.restParam != null) {
                Object rest = SchemeNil.INSTANCE;
                for (int i = args.size() - 1; i >= lam.params.size(); i--) {
                    rest = new SchemePair(args.get(i), rest);
                }
                callEnv.define(lam.restParam, rest);
            }
            return eval(lam.body, callEnv);
        }
        if (proc instanceof BuiltinProcedure bp) {
            return applyBuiltin(bp.name(), args);
        }
        throw new EvalError("not a procedure");
    }

    private Object applyBuiltin(String name, List<Object> args) throws EvalError {
        switch (name) {
            case "+" -> {
                long result = 0;
                for (Object arg : args) result += requireLong(arg);
                return result;
            }
            case "-" -> {
                if (args.isEmpty()) throw new EvalError("- requires at least one argument");
                if (args.size() == 1) return -requireLong(args.get(0));
                long result = requireLong(args.get(0));
                for (int i = 1; i < args.size(); i++) result -= requireLong(args.get(i));
                return result;
            }
            case "*" -> {
                long result = 1;
                for (Object arg : args) result *= requireLong(arg);
                return result;
            }
            case "/" -> {
                if (args.isEmpty()) throw new EvalError("/ requires at least one argument");
                long result = requireLong(args.get(0));
                for (int i = 1; i < args.size(); i++) {
                    long divisor = requireLong(args.get(i));
                    if (divisor == 0) throw new EvalError("division by zero");
                    result /= divisor;
                }
                return result;
            }
            case "<" -> { return requireLong(args.get(0)) < requireLong(args.get(1)); }
            case ">" -> { return requireLong(args.get(0)) > requireLong(args.get(1)); }
            case "=" -> { return requireLong(args.get(0)) == requireLong(args.get(1)); }
            case "<=" -> { return requireLong(args.get(0)) <= requireLong(args.get(1)); }
            case ">=" -> { return requireLong(args.get(0)) >= requireLong(args.get(1)); }
            case "not" -> { return args.get(0).equals(Boolean.FALSE); }
            case "cons" -> { return new SchemePair(args.get(0), args.get(1)); }
            case "car" -> {
                if (args.get(0) instanceof SchemePair p) return p.car;
                throw new EvalError("car: not a pair");
            }
            case "cdr" -> {
                if (args.get(0) instanceof SchemePair p) return p.cdr;
                throw new EvalError("cdr: not a pair");
            }
            case "null?" -> { return args.get(0) instanceof SchemeNil; }
            case "list" -> {
                Object result = SchemeNil.INSTANCE;
                for (int i = args.size() - 1; i >= 0; i--) result = new SchemePair(args.get(i), result);
                return result;
            }
            case "length" -> {
                Object val = args.get(0);
                long len = 0;
                while (val instanceof SchemePair p) { len++; val = p.cdr; }
                if (!(val instanceof SchemeNil)) throw new EvalError("length: not a proper list");
                return len;
            }
            case "append" -> {
                Object result = SchemeNil.INSTANCE;
                for (int i = args.size() - 1; i >= 0; i--) {
                    Object lst = args.get(i);
                    if (lst instanceof SchemeNil) continue;
                    if (i == args.size() - 1) {
                        result = lst;
                    } else {
                        List<Object> elems = new ArrayList<>();
                        Object cur = lst;
                        while (cur instanceof SchemePair p) { elems.add(p.car); cur = p.cdr; }
                        for (int j = elems.size() - 1; j >= 0; j--) result = new SchemePair(elems.get(j), result);
                    }
                }
                return result;
            }
            case "number?" -> { return args.get(0) instanceof Long; }
            case "string?" -> { Object sv = args.get(0); return sv instanceof String || sv instanceof SchemeString; }
            case "boolean?" -> { return args.get(0) instanceof Boolean; }
            case "pair?" -> { return args.get(0) instanceof SchemePair; }
            case "symbol?" -> { return args.get(0) instanceof SchemeSymbol; }
            case "char?" -> { return args.get(0) instanceof SchemeChar; }
            case "display" -> {
                if (outputBuffer != null) outputBuffer.append(displayString(args.get(0)));
                return VOID;
            }
            case "write" -> {
                if (outputBuffer != null) outputBuffer.append(schemeToString(args.get(0)));
                return VOID;
            }
            case "newline" -> {
                if (outputBuffer != null) outputBuffer.append("\n");
                return VOID;
            }
            case "string-append" -> {
                StringBuilder sb = new StringBuilder();
                for (Object arg : args) sb.append(requireString(arg));
                return "\"" + sb + "\"";
            }
            case "string-length" -> { return (long) requireString(args.get(0)).length(); }
            case "substring" -> {
                String s = requireString(args.get(0));
                int start = (int) requireLong(args.get(1));
                int end = args.size() == 3 ? (int) requireLong(args.get(2)) : s.length();
                return "\"" + s.substring(start, end) + "\"";
            }
            case "string-ref" -> {
                return new SchemeChar(requireString(args.get(0)).charAt((int) requireLong(args.get(1))));
            }
            case "string->number" -> {
                try { return Long.parseLong(requireString(args.get(0))); }
                catch (NumberFormatException e) { return Boolean.FALSE; }
            }
            case "number->string" -> { return "\"" + requireLong(args.get(0)) + "\""; }
            case "symbol->string" -> {
                if (!(args.get(0) instanceof SchemeSymbol sym)) throw new EvalError("symbol->string: not a symbol");
                return "\"" + sym.name() + "\"";
            }
            case "string->symbol" -> { return new SchemeSymbol(requireString(args.get(0))); }
            case "string-copy" -> { return new SchemeString(requireString(args.get(0))); }
            case "string-set!" -> {
                Object target = args.get(0);
                int idx = (int) requireLong(args.get(1));
                Object charVal = args.get(2);
                if (!(charVal instanceof SchemeChar ch)) throw new EvalError("string-set!: expected char");
                if (!(target instanceof SchemeString ss)) throw new EvalError("string-set!: string is immutable");
                ss.setCharAt(idx, ch.value());
                return VOID;
            }
            case "apply" -> {
                if (args.size() < 2) throw new EvalError("apply: expected at least 2 arguments");
                Object applyProc = args.get(0);
                Object lastArg = args.get(args.size() - 1);
                List<Object> allArgs = new ArrayList<>();
                for (int i = 1; i < args.size() - 1; i++) allArgs.add(args.get(i));
                Object cur = lastArg;
                while (cur instanceof SchemePair p) { allArgs.add(p.car); cur = p.cdr; }
                return apply(applyProc, allArgs);
            }
            default -> throw new EvalError("unknown procedure: " + name);
        }
    }

    private Object evalBuiltin(String name, List<Object> args, Environment env) throws EvalError {
        List<Object> evaluated = new ArrayList<>();
        for (Object arg : args) {
            evaluated.add(eval(arg, env));
        }
        return applyBuiltin(name, evaluated);
    }

    private long requireLong(Object val) throws EvalError {
        if (val instanceof Long l) return l;
        throw new EvalError("expected number, got: " + schemeToString(val));
    }

    private String requireString(Object val) throws EvalError {
        if (val instanceof String s) {
            if (s.startsWith("\"") && s.endsWith("\"")) {
                return s.substring(1, s.length() - 1);
            }
            return s;
        }
        if (val instanceof SchemeString ss) {
            return ss.value();
        }
        throw new EvalError("expected string, got: " + schemeToString(val));
    }

    /** display format: strings without quotes, everything else like schemeToString */
    private String displayString(Object val) {
        if (val instanceof String s) {
            if (s.startsWith("\"") && s.endsWith("\"")) {
                return s.substring(1, s.length() - 1);
            }
            return s;
        }
        if (val instanceof SchemeString ss) {
            return ss.value();
        }
        return schemeToString(val);
    }

    private void requireArgCount(String name, List<?> args, int expected) throws EvalError {
        if (args.size() != expected) {
            throw new EvalError(name + ": expected " + expected + " arguments, got " + args.size());
        }
    }

    @SuppressWarnings("unchecked")
    static String schemeToString(Object val) {
        if (val instanceof Long l) return l.toString();
        if (val instanceof Boolean b) return b ? "#t" : "#f";
        if (val instanceof String s) return s;
        if (val instanceof SchemeString ss) return ss.toString();
        if (val instanceof SchemeSymbol sym) return sym.name();
        if (val instanceof SchemeChar ch) return "#\\" + ch.value();
        if (val instanceof SchemeNil) return "()";
        if (val instanceof SchemePair p) {
            StringBuilder sb = new StringBuilder("(");
            sb.append(schemeToString(p.car));
            Object rest = p.cdr;
            while (rest instanceof SchemePair rp) {
                sb.append(" ");
                sb.append(schemeToString(rp.car));
                rest = rp.cdr;
            }
            if (!(rest instanceof SchemeNil)) {
                sb.append(" . ");
                sb.append(schemeToString(rest));
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
        return val.toString();
    }
}
