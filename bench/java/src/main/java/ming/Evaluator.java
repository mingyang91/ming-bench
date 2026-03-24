package ming;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

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
    }

    // --- Lambda (closure) ---

    private record Lambda(List<String> params, List<Object> body, Env closureEnv) {}

    // Builtin procedure wrapper
    private record Builtin(String name) {}

    // Internal string wrapper to distinguish from symbols
    record SchemeString(String value) {}

    // Character wrapper
    record SchemeChar(char value) {}

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
        "string-ref", "char?"
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
                tokens.add(new SchemeString(sb.toString()));
                tokenPositions.add(startPos);
            } else if (c == '#') {
                Pos startPos = new Pos(line, col);
                if (i + 1 < input.length()) {
                    char next = input.charAt(i + 1);
                    if (next == 't') {
                        tokens.add(Boolean.TRUE);
                        tokenPositions.add(startPos);
                        i += 2; col += 2;
                    } else if (next == 'f') {
                        tokens.add(Boolean.FALSE);
                        tokenPositions.add(startPos);
                        i += 2; col += 2;
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
                try {
                    tokens.add(Long.parseLong(tok));
                } catch (NumberFormatException e) {
                    tokens.add(tok); // symbol
                }
                tokenPositions.add(startPos);
            }
        }
        return tokens;
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

        if (expr instanceof Long || expr instanceof Boolean || expr instanceof SchemeString) {
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
                            // (define (f params...) body...)
                            Object rawFirst = sig.isEmpty() ? null : unwrap(sig.get(0));
                            if (sig.isEmpty() || !(rawFirst instanceof String fname)) {
                                throw new EvalError(posStr() + "define: bad syntax");
                            }
                            List<String> params = new ArrayList<>();
                            for (int i = 1; i < sig.size(); i++) {
                                Object p = unwrap(sig.get(i));
                                if (!(p instanceof String s)) {
                                    throw new EvalError(posStr() + "define: parameter must be a symbol");
                                }
                                params.add(s);
                            }
                            List<Object> body = new ArrayList<>(list.subList(2, list.size()));
                            env.define(fname, new Lambda(params, body, env));
                        } else {
                            throw new EvalError(posStr() + "define: bad syntax");
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
                        for (Object p : paramList) {
                            Object rawP = unwrap(p);
                            if (!(rawP instanceof String s)) {
                                throw new EvalError(posStr() + "lambda: parameter must be a symbol");
                            }
                            params.add(s);
                        }
                        List<Object> body = new ArrayList<>(list.subList(2, list.size()));
                        return new Lambda(params, body, env);
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
                            Lambda loopLam = new Lambda(params, body, letEnv);
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
                }
            }

            // Function application
            Object proc = eval(head, env);

            // Evaluate arguments
            List<Object> args = new ArrayList<>();
            for (int i = 1; i < list.size(); i++) {
                args.add(eval(list.get(i), env));
            }

            if (proc instanceof Lambda lam) {
                if (args.size() != lam.params().size()) {
                    throw new EvalError(posStr() + "wrong number of arguments: expected " + lam.params().size() + ", got " + args.size());
                }
                Env callEnv = new Env(lam.closureEnv());
                for (int i = 0; i < lam.params().size(); i++) {
                    callEnv.define(lam.params().get(i), args.get(i));
                }
                Object result = VOID;
                for (Object bodyExpr : lam.body()) {
                    result = eval(bodyExpr, callEnv);
                }
                return result;
            }

            if (proc instanceof Builtin b) {
                return applyBuiltin(b.name(), args);
            }

            throw new EvalError(posStr() + "not a procedure: " + schemeToString(proc));
        }
        throw new EvalError(posStr() + "cannot eval: " + expr);
    }

    private Object applyBuiltin(String op, List<Object> args) throws EvalError {
        switch (op) {
            case "+" -> {
                long sum = 0;
                for (Object a : args) sum += requireLong(a);
                return sum;
            }
            case "-" -> {
                if (args.isEmpty()) throw new EvalError(posStr() + "- requires at least 1 argument");
                if (args.size() == 1) return -requireLong(args.get(0));
                long result = requireLong(args.get(0));
                for (int i = 1; i < args.size(); i++) result -= requireLong(args.get(i));
                return result;
            }
            case "*" -> {
                long product = 1;
                for (Object a : args) product *= requireLong(a);
                return product;
            }
            case "/" -> {
                if (args.size() < 2) throw new EvalError(posStr() + "/ requires at least 2 arguments");
                long result = requireLong(args.get(0));
                for (int i = 1; i < args.size(); i++) {
                    long divisor = requireLong(args.get(i));
                    if (divisor == 0) throw new EvalError(posStr() + "division by zero");
                    result /= divisor;
                }
                return result;
            }
            case "<" -> {
                requireArgCount(op, args, 2);
                return requireLong(args.get(0)) < requireLong(args.get(1));
            }
            case ">" -> {
                requireArgCount(op, args, 2);
                return requireLong(args.get(0)) > requireLong(args.get(1));
            }
            case "=" -> {
                requireArgCount(op, args, 2);
                return requireLong(args.get(0)) == requireLong(args.get(1));
            }
            case "<=" -> {
                requireArgCount(op, args, 2);
                return requireLong(args.get(0)) <= requireLong(args.get(1));
            }
            case ">=" -> {
                requireArgCount(op, args, 2);
                return requireLong(args.get(0)) >= requireLong(args.get(1));
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
                return args.get(0) instanceof Long;
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
                return (long) s.value().length();
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
                return new SchemeString(String.valueOf(requireLong(args.get(0))));
            }
            case "symbol->string" -> {
                requireArgCount(op, args, 1);
                if (!(args.get(0) instanceof String sym)) throw new EvalError(posStr() + "symbol->string: not a symbol");
                return new SchemeString(sym);
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
                return new SchemeChar(s.value().charAt(idx));
            }
            default -> throw new EvalError(posStr() + "unbound variable: " + op);
        }
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
