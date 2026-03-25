package ming;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

public class Evaluator {

    // --- Source position tracking ---

    private record Token(Object value, int line, int col) {}

    private record SourceExpr(Object expr, int line, int col) {}

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
            if (bindings.containsKey(name)) {
                bindings.put(name, value);
                return;
            }
            if (parent != null) {
                parent.set(name, value);
                return;
            }
            throw new EvalError("unbound variable: " + name);
        }
    }

    // --- Lambda (closure) ---

    private record Lambda(List<String> params, String restParam, List<Object> body, Env closureEnv) {}

    // Sentinel for void (define returns this)
    private static final Object VOID = new Object() {
        @Override public String toString() { return "#<void>"; }
    };

    // --- Scheme Pair (cons cell) ---

    private static class Pair {
        Object car;
        Object cdr;
        Pair(Object car, Object cdr) { this.car = car; this.cdr = cdr; }
    }

    // Sentinel for empty list '()
    private static final Object NIL = new Object() {
        @Override public String toString() { return "()"; }
    };

    // --- Builtin procedure ---

    @FunctionalInterface
    private interface BuiltinProc {
        Object apply(List<Object> args) throws EvalError;
    }

    private final Env globalEnv = new Env(null);
    private StringBuilder outputBuffer = new StringBuilder();

    public Evaluator() {
        // Arithmetic
        globalEnv.define("+", (BuiltinProc) args -> {
            long result = 0;
            for (Object a : args) result += asLong(a);
            return result;
        });
        globalEnv.define("-", (BuiltinProc) args -> {
            if (args.isEmpty()) throw new EvalError("- requires at least 1 argument");
            if (args.size() == 1) return -asLong(args.get(0));
            long result = asLong(args.get(0));
            for (int i = 1; i < args.size(); i++) result -= asLong(args.get(i));
            return result;
        });
        globalEnv.define("*", (BuiltinProc) args -> {
            long result = 1;
            for (Object a : args) result *= asLong(a);
            return result;
        });
        globalEnv.define("/", (BuiltinProc) args -> {
            if (args.size() < 2) throw new EvalError("/ requires at least 2 arguments");
            long result = asLong(args.get(0));
            for (int i = 1; i < args.size(); i++) {
                long d = asLong(args.get(i));
                if (d == 0) throw new EvalError("division by zero");
                result /= d;
            }
            return result;
        });

        // Comparisons
        globalEnv.define("<", (BuiltinProc) args -> asLong(args.get(0)) < asLong(args.get(1)));
        globalEnv.define(">", (BuiltinProc) args -> asLong(args.get(0)) > asLong(args.get(1)));
        globalEnv.define("=", (BuiltinProc) args -> asLong(args.get(0)) == asLong(args.get(1)));
        globalEnv.define("<=", (BuiltinProc) args -> asLong(args.get(0)) <= asLong(args.get(1)));
        globalEnv.define(">=", (BuiltinProc) args -> asLong(args.get(0)) >= asLong(args.get(1)));
        globalEnv.define("not", (BuiltinProc) args -> isFalse(args.get(0)));

        // List operations
        globalEnv.define("cons", (BuiltinProc) args -> new Pair(args.get(0), args.get(1)));
        globalEnv.define("car", (BuiltinProc) args -> {
            if (args.get(0) instanceof Pair p) return p.car;
            throw new EvalError("car: not a pair");
        });
        globalEnv.define("cdr", (BuiltinProc) args -> {
            if (args.get(0) instanceof Pair p) return p.cdr;
            throw new EvalError("cdr: not a pair");
        });
        globalEnv.define("null?", (BuiltinProc) args -> args.get(0) == NIL);
        globalEnv.define("list", (BuiltinProc) args -> {
            Object result = NIL;
            for (int i = args.size() - 1; i >= 0; i--) {
                result = new Pair(args.get(i), result);
            }
            return result;
        });
        globalEnv.define("length", (BuiltinProc) args -> {
            long count = 0;
            Object curr = args.get(0);
            while (curr instanceof Pair p) {
                count++;
                curr = p.cdr;
            }
            return count;
        });
        globalEnv.define("append", (BuiltinProc) args -> {
            if (args.isEmpty()) return NIL;
            if (args.size() == 1) return args.get(0);
            Object result = args.get(args.size() - 1);
            for (int i = args.size() - 2; i >= 0; i--) {
                result = appendTwo(args.get(i), result);
            }
            return result;
        });

        // Type predicates
        globalEnv.define("boolean?", (BuiltinProc) args -> args.get(0) instanceof Boolean);
        globalEnv.define("number?", (BuiltinProc) args -> args.get(0) instanceof Long);
        globalEnv.define("pair?", (BuiltinProc) args -> args.get(0) instanceof Pair);
        globalEnv.define("string?", (BuiltinProc) args -> args.get(0) instanceof SchemeString);
        globalEnv.define("symbol?", (BuiltinProc) args -> args.get(0) instanceof String);
        globalEnv.define("char?", (BuiltinProc) args -> args.get(0) instanceof SchemeChar);

        // I/O
        globalEnv.define("display", (BuiltinProc) args -> {
            outputBuffer.append(displayString(args.get(0)));
            return VOID;
        });
        globalEnv.define("write", (BuiltinProc) args -> {
            outputBuffer.append(schemeToString(args.get(0)));
            return VOID;
        });
        globalEnv.define("newline", (BuiltinProc) args -> {
            outputBuffer.append('\n');
            return VOID;
        });

        // String operations
        globalEnv.define("string-append", (BuiltinProc) args -> {
            StringBuilder sb = new StringBuilder();
            for (Object a : args) sb.append(asSchemeString(a).value());
            return new SchemeString(sb.toString());
        });
        globalEnv.define("string-length", (BuiltinProc) args -> (long) asSchemeString(args.get(0)).length());
        globalEnv.define("substring", (BuiltinProc) args -> {
            String s = asSchemeString(args.get(0)).value();
            int start = (int) asLong(args.get(1));
            int end = (int) asLong(args.get(2));
            return new SchemeString(s.substring(start, end));
        });
        globalEnv.define("string->number", (BuiltinProc) args -> {
            String s = asSchemeString(args.get(0)).value();
            try {
                return Long.parseLong(s);
            } catch (NumberFormatException e) {
                return Boolean.FALSE;
            }
        });
        globalEnv.define("number->string", (BuiltinProc) args -> new SchemeString(String.valueOf(asLong(args.get(0)))));
        globalEnv.define("symbol->string", (BuiltinProc) args -> {
            if (args.get(0) instanceof String s) return new SchemeString(s);
            throw new EvalError("symbol->string: not a symbol");
        });
        globalEnv.define("string->symbol", (BuiltinProc) args -> asSchemeString(args.get(0)).value());
        globalEnv.define("string-ref", (BuiltinProc) args -> {
            SchemeString s = asSchemeString(args.get(0));
            int idx = (int) asLong(args.get(1));
            return new SchemeChar(s.charAt(idx));
        });
        globalEnv.define("string-copy", (BuiltinProc) args -> {
            return new SchemeString(asSchemeString(args.get(0)).value());
        });
        globalEnv.define("string-set!", (BuiltinProc) args -> {
            SchemeString s = asSchemeString(args.get(0));
            int idx = (int) asLong(args.get(1));
            if (!(args.get(2) instanceof SchemeChar ch)) throw new EvalError("string-set!: expected char");
            s.setChar(idx, ch.value());
            return VOID;
        });
        globalEnv.define("apply", (BuiltinProc) args -> {
            if (args.size() < 2) throw new EvalError("apply: requires at least 2 arguments");
            Object proc = args.get(0);
            // Last argument must be a list; preceding args are prepended
            Object lastArg = args.get(args.size() - 1);
            List<Object> callArgs = new ArrayList<>();
            for (int i = 1; i < args.size() - 1; i++) {
                callArgs.add(args.get(i));
            }
            // Convert last arg (scheme list) to java list
            Object curr = lastArg;
            while (curr instanceof Pair p) {
                callArgs.add(p.car);
                curr = p.cdr;
            }
            if (proc instanceof BuiltinProc builtin) {
                return builtin.apply(callArgs);
            }
            if (proc instanceof Lambda lambda) {
                return applyLambda(lambda, callArgs);
            }
            throw new EvalError("apply: not a procedure");
        });
    }

    private Object appendTwo(Object a, Object b) {
        if (a == NIL) return b;
        if (a instanceof Pair p) {
            return new Pair(p.car, appendTwo(p.cdr, b));
        }
        return b;
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
        if (lastResult == VOID) {
            throw new EvalError("no expression");
        }
        return schemeToString(lastResult);
    }

    public EvalResult evalStrWithOutput(String input) throws EvalError {
        outputBuffer.setLength(0);
        List<Token> tokens = tokenize(input);
        int[] pos = {0};
        Object lastResult = null;
        while (pos[0] < tokens.size()) {
            Object expr = parse(tokens, pos);
            lastResult = eval(expr, globalEnv);
        }
        String result = (lastResult == null || lastResult == VOID) ? "" : schemeToString(lastResult);
        return new EvalResult(result, outputBuffer.toString());
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
                int startCol = col;
                StringBuilder sb = new StringBuilder();
                i++;
                col++;
                while (i < len && input.charAt(i) != '"') {
                    if (input.charAt(i) == '\\') {
                        i++;
                        col++;
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
                            line++;
                            col = 0;
                        } else {
                            sb.append(input.charAt(i));
                        }
                    }
                    i++;
                    col++;
                }
                if (i >= len) throw new EvalError("unterminated string at " + line + ":" + startCol);
                i++;
                col++;
                tokens.add(new Token(new SchemeString(sb.toString()), line, startCol));
            } else if (c == '#') {
                int startCol = col;
                if (i + 1 < len) {
                    char next = input.charAt(i + 1);
                    if (next == 't') {
                        tokens.add(new Token(Boolean.TRUE, line, startCol));
                        i += 2;
                        col += 2;
                    } else if (next == 'f') {
                        tokens.add(new Token(Boolean.FALSE, line, startCol));
                        i += 2;
                        col += 2;
                    } else if (next == '\\') {
                        i += 2;
                        col += 2;
                        if (i >= len) throw new EvalError("unexpected end after #\\ at " + line + ":" + col);
                        // Read character name or single char
                        int nameStart = i;
                        while (i < len && !Character.isWhitespace(input.charAt(i))
                                && input.charAt(i) != '(' && input.charAt(i) != ')'
                                && input.charAt(i) != '"' && input.charAt(i) != ';') {
                            i++;
                            col++;
                        }
                        String charName = input.substring(nameStart, i);
                        char ch;
                        switch (charName) {
                            case "space" -> ch = ' ';
                            case "newline" -> ch = '\n';
                            case "tab" -> ch = '\t';
                            default -> {
                                if (charName.length() == 1) ch = charName.charAt(0);
                                else throw new EvalError("unknown character name: " + charName + " at " + line + ":" + startCol);
                            }
                        }
                        tokens.add(new Token(new SchemeChar(ch), line, startCol));
                    } else {
                        throw new EvalError("unexpected token: #" + next + " at " + line + ":" + col);
                    }
                } else {
                    throw new EvalError("unexpected end after # at " + line + ":" + col);
                }
            } else {
                int startCol = col;
                StringBuilder sb = new StringBuilder();
                while (i < len && !Character.isWhitespace(input.charAt(i))
                        && input.charAt(i) != '(' && input.charAt(i) != ')'
                        && input.charAt(i) != '"' && input.charAt(i) != ';'
                        && input.charAt(i) != '\'') {
                    sb.append(input.charAt(i));
                    i++;
                    col++;
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

    // --- Parser ---

    private Object parse(List<Token> tokens, int[] pos) throws EvalError {
        if (pos[0] >= tokens.size()) {
            throw new EvalError("unexpected end of input");
        }
        Token token = tokens.get(pos[0]);
        if ("'".equals(token.value)) {
            pos[0]++;
            Object quoted = parse(tokens, pos);
            List<Object> quoteExpr = new ArrayList<>();
            quoteExpr.add("quote");
            quoteExpr.add(quoted);
            return new SourceExpr(quoteExpr, token.line, token.col);
        }
        if ("(".equals(token.value)) {
            pos[0]++;
            List<Object> list = new ArrayList<>();
            while (pos[0] < tokens.size() && !")".equals(tokens.get(pos[0]).value)) {
                list.add(parse(tokens, pos));
            }
            if (pos[0] >= tokens.size()) {
                throw new EvalError("missing closing parenthesis at " + token.line + ":" + token.col);
            }
            pos[0]++;
            return new SourceExpr(list, token.line, token.col);
        } else if (")".equals(token.value)) {
            throw new EvalError("unexpected ) at " + token.line + ":" + token.col);
        } else {
            pos[0]++;
            return new SourceExpr(token.value, token.line, token.col);
        }
    }

    // Convert parsed AST list to Scheme cons-cell list (for quote)
    private Object astToScheme(Object ast) {
        if (ast instanceof SourceExpr se) {
            return astToScheme(se.expr);
        }
        if (ast instanceof List<?> list) {
            Object result = NIL;
            for (int i = list.size() - 1; i >= 0; i--) {
                result = new Pair(astToScheme(list.get(i)), result);
            }
            return result;
        }
        return ast;
    }

    // --- Evaluator ---

    @SuppressWarnings("unchecked")
    private Object eval(Object expr, Env env) throws EvalError {
        // Unwrap SourceExpr and add position to any errors
        if (expr instanceof SourceExpr se) {
            try {
                return eval(se.expr, env);
            } catch (EvalError e) {
                if (!e.getMessage().matches(".*\\d+:\\d+.*")) {
                    throw new EvalError(e.getMessage() + " at " + se.line + ":" + se.col);
                }
                throw e;
            }
        }

        if (expr instanceof Long || expr instanceof Boolean || expr instanceof SchemeString || expr instanceof SchemeChar) {
            return expr;
        }
        if (expr instanceof String sym) {
            return env.lookup(sym);
        }
        if (expr instanceof List<?> rawList) {
            List<Object> list = (List<Object>) rawList;
            if (list.isEmpty()) {
                throw new EvalError("empty application");
            }
            Object head = list.get(0);

            // Unwrap SourceExpr for head to check special forms
            Object rawHead = head;
            if (rawHead instanceof SourceExpr she) {
                rawHead = she.expr;
            }

            // Special forms
            if (rawHead instanceof String op) {
                switch (op) {
                    case "quote" -> {
                        if (list.size() < 2) throw new EvalError("bad syntax: quote");
                        return astToScheme(list.get(1));
                    }
                    case "if" -> {
                        if (list.size() < 3) throw new EvalError("bad syntax: if requires at least 2 parts");
                        Object cond = eval(list.get(1), env);
                        if (!isFalse(cond)) {
                            return eval(list.get(2), env);
                        } else if (list.size() > 3) {
                            return eval(list.get(3), env);
                        }
                        return VOID;
                    }
                    case "define" -> {
                        if (list.size() < 2) throw new EvalError("bad syntax: define");
                        Object target = list.get(1);
                        if (target instanceof SourceExpr se) target = se.expr;
                        if (target instanceof String name) {
                            if (list.size() < 3) throw new EvalError("bad syntax: define");
                            env.define(name, eval(list.get(2), env));
                        } else if (target instanceof List<?> sig) {
                            if (sig.isEmpty()) throw new EvalError("bad syntax: define");
                            Object nameObj = sig.get(0);
                            if (nameObj instanceof SourceExpr se2) nameObj = se2.expr;
                            String name = (String) nameObj;
                            List<String> params = new ArrayList<>();
                            String restParam = null;
                            for (int i = 1; i < sig.size(); i++) {
                                Object p = sig.get(i);
                                if (p instanceof SourceExpr se2) p = se2.expr;
                                if (".".equals(p)) {
                                    if (i + 1 >= sig.size()) throw new EvalError("bad syntax: define");
                                    Object rp = sig.get(i + 1);
                                    if (rp instanceof SourceExpr se3) rp = se3.expr;
                                    restParam = (String) rp;
                                    break;
                                }
                                params.add((String) p);
                            }
                            List<Object> body = new ArrayList<>(list.subList(2, list.size()));
                            env.define(name, new Lambda(params, restParam, body, env));
                        } else {
                            throw new EvalError("bad syntax: define");
                        }
                        return VOID;
                    }
                    case "lambda" -> {
                        if (list.size() < 3) throw new EvalError("bad syntax: lambda");
                        Object paramObj = list.get(1);
                        if (paramObj instanceof SourceExpr se) paramObj = se.expr;
                        List<Object> body = new ArrayList<>(list.subList(2, list.size()));
                        if (paramObj instanceof String restOnly) {
                            // (lambda args body) - single rest param
                            return new Lambda(List.of(), restOnly, body, env);
                        }
                        List<?> paramList = (List<?>) paramObj;
                        List<String> params = new ArrayList<>();
                        String restParam = null;
                        for (int i = 0; i < paramList.size(); i++) {
                            Object p = paramList.get(i);
                            if (p instanceof SourceExpr se) p = se.expr;
                            if (".".equals(p)) {
                                if (i + 1 >= paramList.size()) throw new EvalError("bad syntax: lambda");
                                Object rp = paramList.get(i + 1);
                                if (rp instanceof SourceExpr se2) rp = se2.expr;
                                restParam = (String) rp;
                                break;
                            }
                            params.add((String) p);
                        }
                        return new Lambda(params, restParam, body, env);
                    }
                    case "let" -> {
                        int offset;
                        String loopName = null;
                        Object second = list.get(1);
                        if (second instanceof SourceExpr se) second = se.expr;
                        if (second instanceof String name) {
                            loopName = name;
                            offset = 2;
                        } else {
                            offset = 1;
                        }
                        Object bindingsObj = list.get(offset);
                        if (bindingsObj instanceof SourceExpr se) bindingsObj = se.expr;
                        List<?> bindings = (List<?>) bindingsObj;
                        List<Object> body = new ArrayList<>(list.subList(offset + 1, list.size()));

                        List<String> params = new ArrayList<>();
                        List<Object> inits = new ArrayList<>();
                        for (Object b : bindings) {
                            if (b instanceof SourceExpr se) b = se.expr;
                            List<?> binding = (List<?>) b;
                            Object pname = binding.get(0);
                            if (pname instanceof SourceExpr se) pname = se.expr;
                            params.add((String) pname);
                            inits.add(binding.get(1));
                        }

                        if (loopName != null) {
                            Env letEnv = new Env(env);
                            Lambda loopLambda = new Lambda(params, null, body, letEnv);
                            letEnv.define(loopName, loopLambda);
                            List<Object> args = new ArrayList<>();
                            for (Object init : inits) {
                                args.add(eval(init, env));
                            }
                            return applyLambda(loopLambda, args);
                        } else {
                            Env letEnv = new Env(env);
                            for (int i = 0; i < params.size(); i++) {
                                letEnv.define(params.get(i), eval(inits.get(i), env));
                            }
                            Object result = VOID;
                            for (Object bodyExpr : body) {
                                result = eval(bodyExpr, letEnv);
                            }
                            return result;
                        }
                    }
                    case "set!" -> {
                        if (list.size() != 3) throw new EvalError("bad syntax: set!");
                        Object varObj = list.get(1);
                        if (varObj instanceof SourceExpr se) varObj = se.expr;
                        if (!(varObj instanceof String varName)) throw new EvalError("bad syntax: set!");
                        Object val = eval(list.get(2), env);
                        env.set(varName, val);
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
                            Object clauseObj = list.get(i);
                            if (clauseObj instanceof SourceExpr se) clauseObj = se.expr;
                            List<Object> clause = (List<Object>) clauseObj;
                            Object test = clause.get(0);
                            Object rawTest = test;
                            if (rawTest instanceof SourceExpr se) rawTest = se.expr;
                            if ("else".equals(rawTest)) {
                                Object result = VOID;
                                for (int j = 1; j < clause.size(); j++) {
                                    result = eval(clause.get(j), env);
                                }
                                return result;
                            }
                            Object condVal = eval(test, env);
                            if (!isFalse(condVal)) {
                                if (clause.size() == 1) return condVal;
                                Object result = VOID;
                                for (int j = 1; j < clause.size(); j++) {
                                    result = eval(clause.get(j), env);
                                }
                                return result;
                            }
                        }
                        return VOID;
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
                }
            }

            // Function application
            Object proc = eval(head, env);
            List<Object> args = new ArrayList<>();
            for (int i = 1; i < list.size(); i++) {
                args.add(eval(list.get(i), env));
            }

            if (proc instanceof BuiltinProc builtin) {
                return builtin.apply(args);
            }
            if (proc instanceof Lambda lambda) {
                return applyLambda(lambda, args);
            }
            throw new EvalError("cannot apply: " + schemeToString(proc));
        }
        throw new EvalError("unknown expression type");
    }

    private Object applyLambda(Lambda lambda, List<Object> args) throws EvalError {
        int required = lambda.params.size();
        if (lambda.restParam != null) {
            if (args.size() < required) {
                throw new EvalError("wrong number of arguments: expected at least " + required + ", got " + args.size());
            }
        } else {
            if (args.size() != required) {
                throw new EvalError("wrong number of arguments: expected " + required + ", got " + args.size());
            }
        }
        Env callEnv = new Env(lambda.closureEnv);
        for (int i = 0; i < required; i++) {
            callEnv.define(lambda.params.get(i), args.get(i));
        }
        if (lambda.restParam != null) {
            callEnv.define(lambda.restParam, javaListToScheme(args.subList(required, args.size())));
        }
        Object result = VOID;
        for (Object bodyExpr : lambda.body) {
            result = eval(bodyExpr, callEnv);
        }
        return result;
    }

    private Object javaListToScheme(List<Object> items) {
        Object result = NIL;
        for (int i = items.size() - 1; i >= 0; i--) {
            result = new Pair(items.get(i), result);
        }
        return result;
    }

    private boolean isFalse(Object val) {
        return Boolean.FALSE.equals(val);
    }

    private long asLong(Object val) throws EvalError {
        if (val instanceof Long l) return l;
        throw new EvalError("expected number, got: " + schemeToString(val));
    }

    private SchemeString asSchemeString(Object val) throws EvalError {
        if (val instanceof SchemeString s) return s;
        throw new EvalError("expected string, got: " + schemeToString(val));
    }

    private String displayString(Object val) {
        if (val instanceof SchemeString s) return s.value();
        if (val instanceof SchemeChar c) return String.valueOf(c.value());
        return schemeToString(val);
    }

    // --- Display ---

    private String schemeToString(Object val) {
        if (val instanceof Long l) return l.toString();
        if (val instanceof Boolean b) return b ? "#t" : "#f";
        if (val instanceof SchemeString s) return "\"" + s.value() + "\"";
        if (val instanceof SchemeChar c) return "#\\" + c.value();
        if (val instanceof String s) return s;
        if (val == NIL) return "()";
        if (val instanceof Pair) {
            StringBuilder sb = new StringBuilder("(");
            Object curr = val;
            boolean first = true;
            while (curr instanceof Pair p) {
                if (!first) sb.append(" ");
                first = false;
                sb.append(schemeToString(p.car));
                curr = p.cdr;
            }
            if (curr != NIL) {
                sb.append(" . ");
                sb.append(schemeToString(curr));
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
        return val.toString();
    }

    // Internal wrapper to distinguish strings from symbols (mutable for string-set!)
    static class SchemeString {
        private char[] chars;
        SchemeString(String value) { this.chars = value.toCharArray(); }
        String value() { return new String(chars); }
        char charAt(int i) { return chars[i]; }
        void setChar(int i, char c) { chars[i] = c; }
        int length() { return chars.length; }
        @Override public boolean equals(Object o) {
            return o instanceof SchemeString s && java.util.Arrays.equals(chars, s.chars);
        }
        @Override public int hashCode() { return java.util.Arrays.hashCode(chars); }
    }

    // Internal wrapper for characters
    record SchemeChar(char value) {}
}
