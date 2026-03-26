package ming;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

public class Evaluator {

    static class SchemeString {
        private char[] chars;
        SchemeString(String value) { this.chars = value.toCharArray(); }
        SchemeString(char[] chars) { this.chars = chars.clone(); }
        String value() { return new String(chars); }
        int length() { return chars.length; }
        char charAt(int i) { return chars[i]; }
        void setChar(int i, char c) { chars[i] = c; }
    }
    record Pair(Object car, Object cdr) {}
    static final Object NIL = new Object() {
        @Override public String toString() { return "()"; }
    };
    record Lambda(List<String> params, String restParam, List<Object> body, Env closure) {}
    record SchemeChar(char value) {}
    record Builtin(String name) {}
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
        "apply",
        "eq?", "equal?", "map",
        "abs", "modulo", "remainder", "quotient", "min", "max", "expt",
        "zero?", "positive?", "negative?", "odd?", "even?",
        "list-ref", "list-tail", "list?", "assoc",
        "char-alphabetic?", "char-numeric?", "char-upcase", "char-downcase",
        "char=?", "char<?",
        "string=?", "string<?", "string-ci=?", "string-upcase", "string-downcase"
    };

    private final Env globalEnv;
    private StringBuilder outputBuffer;

    public Evaluator() {
        globalEnv = new Env(null);
        for (String name : BUILTIN_NAMES) {
            globalEnv.define(name, new Builtin(name));
        }
    }

    public String evalStr(String input) throws EvalError {
        List<Token> tokens = tokenize(input);
        int[] pos = {0};
        Object result = null;
        while (pos[0] < tokens.size()) {
            Object expr = parse(tokens, pos);
            result = eval(expr, globalEnv);
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
        Object result = null;
        while (pos[0] < tokens.size()) {
            Object expr = parse(tokens, pos);
            result = eval(expr, globalEnv);
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
                while (i < len && Character.isDigit(input.charAt(i))) {
                    sb.append(input.charAt(i));
                    i++; col++;
                }
                tokens.add(new Token(Long.parseLong(sb.toString()), line, startCol));
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
                try {
                    tokens.add(new Token(Long.parseLong(tok), line, startCol));
                } catch (NumberFormatException e) {
                    tokens.add(new Token(tok, line, startCol));
                }
            }
        }
        return tokens;
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
        // Unwrap Located to get position info
        int eLine = 0, eCol = 0;
        if (expr instanceof Located loc) {
            eLine = loc.line();
            eCol = loc.col();
            expr = loc.expr();
        }
        final int posLine = eLine, posCol = eCol;

        try {
            return evalInner(expr, env, posLine, posCol);
        } catch (EvalError e) {
            // If the error doesn't already have position info, add it
            String msg = e.getMessage();
            if (posLine > 0 && !msg.matches(".*\\d+:\\d+.*")) {
                throw new EvalError(posLine + ":" + posCol + ": " + msg);
            }
            throw e;
        }
    }

    private Object evalInner(Object expr, Env env, int posLine, int posCol) throws EvalError {
        if (expr instanceof Long || expr instanceof Boolean || expr instanceof SchemeString || expr instanceof SchemeChar) {
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
                            return eval(list.get(2), env);
                        } else if (list.size() > 3) {
                            return eval(list.get(3), env);
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
                        Object result2 = null;
                        for (int i = 1; i < list.size(); i++) {
                            result2 = eval(list.get(i), env);
                        }
                        return result2;
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
                            return apply(loopLambda, inits);
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
                        Object result3 = null;
                        for (int i = 2; i < list.size(); i++) {
                            result3 = eval(list.get(i), letEnv);
                        }
                        return result3;
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
                                Object r = null;
                                for (int j = 1; j < clause.size(); j++) r = eval(clause.get(j), env);
                                return r;
                            }
                            Object testVal = eval(test, env);
                            if (!isFalse(testVal)) {
                                if (clause.size() == 1) return testVal;
                                Object r = null;
                                for (int j = 1; j < clause.size(); j++) r = eval(clause.get(j), env);
                                return r;
                            }
                        }
                        return null;
                    }
                }
            }
            // Function application
            Object proc = eval(head, env);
            List<Object> args = new ArrayList<>();
            for (int i = 1; i < list.size(); i++) {
                args.add(eval(list.get(i), env));
            }
            return apply(proc, args);
        }
        throw new EvalError("cannot eval: " + expr);
    }

    private Object apply(Object proc, List<Object> args) throws EvalError {
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
            Object result = null;
            for (Object bodyExpr : lambda.body()) {
                result = eval(bodyExpr, callEnv);
            }
            return result;
        }
        if (proc instanceof Builtin b) {
            return applyBuiltin(b.name(), args);
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
                long sum = 0;
                for (Object a : args) sum += asLong(a);
                yield sum;
            }
            case "-" -> {
                if (args.isEmpty()) throw new EvalError("-: need at least one argument");
                if (args.size() == 1) yield -asLong(args.getFirst());
                long r = asLong(args.getFirst());
                for (int i = 1; i < args.size(); i++) r -= asLong(args.get(i));
                yield r;
            }
            case "*" -> {
                long p = 1;
                for (Object a : args) p *= asLong(a);
                yield p;
            }
            case "/" -> {
                if (args.size() < 2) throw new EvalError("/: need at least two arguments");
                long r2 = asLong(args.getFirst());
                for (int i = 1; i < args.size(); i++) {
                    long d = asLong(args.get(i));
                    if (d == 0) throw new EvalError("division by zero");
                    r2 /= d;
                }
                yield r2;
            }
            case "<" -> {
                requireArgs(op, args, 2);
                yield asLong(args.get(0)) < asLong(args.get(1)) ? Boolean.TRUE : Boolean.FALSE;
            }
            case ">" -> {
                requireArgs(op, args, 2);
                yield asLong(args.get(0)) > asLong(args.get(1)) ? Boolean.TRUE : Boolean.FALSE;
            }
            case "=" -> {
                requireArgs(op, args, 2);
                yield asLong(args.get(0)) == asLong(args.get(1)) ? Boolean.TRUE : Boolean.FALSE;
            }
            case "<=" -> {
                requireArgs(op, args, 2);
                yield asLong(args.get(0)) <= asLong(args.get(1)) ? Boolean.TRUE : Boolean.FALSE;
            }
            case ">=" -> {
                requireArgs(op, args, 2);
                yield asLong(args.get(0)) >= asLong(args.get(1)) ? Boolean.TRUE : Boolean.FALSE;
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
                long len = 0;
                while (obj instanceof Pair p) {
                    len++;
                    obj = p.cdr();
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
                yield args.get(0) instanceof Long ? Boolean.TRUE : Boolean.FALSE;
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
                yield new SchemeString(Long.toString(asLong(args.get(0))));
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
                    int idx = (int) asLong(args.get(1));
                    if (!(args.get(2) instanceof SchemeChar sc))
                        throw new EvalError("string-set!: not a char");
                    ss.setChar(idx, sc.value());
                    yield null;
                }
                throw new EvalError("string-set!: not a string");
            }
            case "string-copy" -> {
                requireArgs(op, args, 1);
                if (args.get(0) instanceof SchemeString ss) {
                    yield new SchemeString(ss.value());
                }
                throw new EvalError("string-copy: not a string");
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
                    resultElems.add(apply(proc, callArgs));
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
                Object obj = args.get(0);
                while (obj instanceof Pair p) obj = p.cdr();
                yield obj == NIL ? Boolean.TRUE : Boolean.FALSE;
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
            default -> throw new EvalError("unbound variable: " + op);
        };
    }

    private boolean schemeEqual(Object a, Object b) {
        if (a == b) return true;
        if (a instanceof Long && b instanceof Long) return a.equals(b);
        if (a instanceof Boolean && b instanceof Boolean) return a.equals(b);
        if (a instanceof String && b instanceof String) return a.equals(b);
        if (a instanceof SchemeString sa && b instanceof SchemeString sb) return sa.value().equals(sb.value());
        if (a instanceof SchemeChar ca && b instanceof SchemeChar cb) return ca.value() == cb.value();
        if (a == NIL && b == NIL) return true;
        if (a instanceof Pair pa && b instanceof Pair pb) {
            return schemeEqual(pa.car(), pb.car()) && schemeEqual(pa.cdr(), pb.cdr());
        }
        return false;
    }

    private boolean isFalse(Object val) {
        return val instanceof Boolean b && !b;
    }

    private long asLong(Object val) throws EvalError {
        if (val instanceof Long l) return l;
        throw new EvalError("expected number, got: " + schemeToString(val));
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
        if (val instanceof Lambda) return "#<procedure>";
        return val.toString();
    }
}
