package ming;

import java.util.ArrayList;
import java.util.List;

public class Evaluator {

    private final Environment globalEnv = new Environment(null);

    public String evalStr(String input) throws EvalError {
        List<Object> tokens = tokenize(input);
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
        String result = evalStr(input);
        return new EvalResult(result, "");
    }

    // --- Tokenizer ---

    private List<Object> tokenize(String input) throws EvalError {
        List<Object> tokens = new ArrayList<>();
        int i = 0;
        int len = input.length();
        while (i < len) {
            char c = input.charAt(i);
            if (Character.isWhitespace(c)) {
                i++;
            } else if (c == ';') {
                while (i < len && input.charAt(i) != '\n') {
                    i++;
                }
            } else if (c == '\'') {
                tokens.add("'");
                i++;
            } else if (c == '(') {
                tokens.add("(");
                i++;
            } else if (c == ')') {
                tokens.add(")");
                i++;
            } else if (c == '"') {
                StringBuilder sb = new StringBuilder();
                sb.append('"');
                i++;
                while (i < len && input.charAt(i) != '"') {
                    if (input.charAt(i) == '\\') {
                        sb.append(input.charAt(i));
                        i++;
                        if (i < len) {
                            sb.append(input.charAt(i));
                            i++;
                        }
                    } else {
                        sb.append(input.charAt(i));
                        i++;
                    }
                }
                if (i < len) {
                    sb.append('"');
                    i++;
                }
                tokens.add(sb.toString());
            } else if (c == '#') {
                if (i + 1 < len) {
                    char next = input.charAt(i + 1);
                    if (next == 't') {
                        tokens.add("#t");
                        i += 2;
                    } else if (next == 'f') {
                        tokens.add("#f");
                        i += 2;
                    } else {
                        tokens.add(readSymbol(input, i));
                        i += ((String) tokens.get(tokens.size() - 1)).length();
                    }
                } else {
                    tokens.add("#");
                    i++;
                }
            } else {
                String sym = readSymbol(input, i);
                tokens.add(sym);
                i += sym.length();
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

    private Object parse(List<Object> tokens, int[] pos) throws EvalError {
        if (pos[0] >= tokens.size()) {
            throw new EvalError("unexpected end of input");
        }
        String token = (String) tokens.get(pos[0]);
        pos[0]++;

        if (token.equals("'")) {
            Object quoted = parse(tokens, pos);
            List<Object> quoteExpr = new ArrayList<>();
            quoteExpr.add(new SchemeSymbol("quote"));
            quoteExpr.add(quoted);
            return quoteExpr;
        }

        if (token.equals("(")) {
            List<Object> list = new ArrayList<>();
            while (pos[0] < tokens.size() && !tokens.get(pos[0]).equals(")")) {
                list.add(parse(tokens, pos));
            }
            if (pos[0] >= tokens.size()) {
                throw new EvalError("missing closing parenthesis");
            }
            pos[0]++; // skip ')'
            return list;
        } else if (token.equals(")")) {
            throw new EvalError("unexpected )");
        } else {
            return parseAtom(token);
        }
    }

    private Object parseAtom(String token) {
        if (token.equals("#t")) {
            return Boolean.TRUE;
        }
        if (token.equals("#f")) {
            return Boolean.FALSE;
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

    @SuppressWarnings("unchecked")
    private Object eval(Object expr, Environment env) throws EvalError {
        if (expr instanceof Long || expr instanceof Boolean) {
            return expr;
        }
        if (expr instanceof String s) {
            return s;
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

            // Special forms
            if (first instanceof SchemeSymbol sym) {
                String name = sym.name();
                List<Object> args = list.subList(1, list.size());

                switch (name) {
                    case "quote" -> {
                        if (args.size() != 1) throw new EvalError("quote: expected 1 argument");
                        return args.get(0);
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
                        Object target = args.get(0);
                        if (target instanceof SchemeSymbol s) {
                            Object val = eval(args.get(1), env);
                            env.define(s.name(), val);
                            return VOID;
                        }
                        if (target instanceof List<?> sig) {
                            // (define (f params...) body)
                            if (sig.isEmpty()) throw new EvalError("define: bad syntax");
                            String fname = ((SchemeSymbol) sig.get(0)).name();
                            List<String> params = new ArrayList<>();
                            for (int i = 1; i < sig.size(); i++) {
                                params.add(((SchemeSymbol) sig.get(i)).name());
                            }
                            Object body;
                            if (args.size() == 2) {
                                body = args.get(1);
                            } else {
                                // implicit begin for multiple body exprs
                                List<Object> beginList = new ArrayList<>();
                                beginList.add(new SchemeSymbol("begin"));
                                beginList.addAll(args.subList(1, args.size()));
                                body = beginList;
                            }
                            env.define(fname, new SchemeLambda(params, body, env));
                            return VOID;
                        }
                        throw new EvalError("define: bad syntax");
                    }
                    case "lambda" -> {
                        if (args.size() < 2) throw new EvalError("lambda: bad syntax");
                        List<?> paramList = (List<?>) args.get(0);
                        List<String> params = new ArrayList<>();
                        for (Object p : paramList) {
                            params.add(((SchemeSymbol) p).name());
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
                        return new SchemeLambda(params, body, env);
                    }
                    case "begin" -> {
                        Object result = VOID;
                        for (Object a : args) {
                            result = eval(a, env);
                        }
                        return result;
                    }
                    // Builtins handled as procedures below
                    case "+", "-", "*", "/", "<", ">", "=", "<=", ">=",
                         "not", "and", "or" -> {
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
            if (args.size() != lam.params.size()) {
                throw new EvalError("expected " + lam.params.size() + " arguments, got " + args.size());
            }
            Environment callEnv = new Environment(lam.closure);
            for (int i = 0; i < lam.params.size(); i++) {
                callEnv.define(lam.params.get(i), args.get(i));
            }
            return eval(lam.body, callEnv);
        }
        throw new EvalError("not a procedure");
    }

    @SuppressWarnings("unchecked")
    private Object evalBuiltin(String name, List<Object> args, Environment env) throws EvalError {
        switch (name) {
            case "+" -> {
                long result = 0;
                for (Object arg : args) {
                    result += requireLong(eval(arg, env));
                }
                return result;
            }
            case "-" -> {
                if (args.isEmpty()) throw new EvalError("- requires at least one argument");
                if (args.size() == 1) return -requireLong(eval(args.get(0), env));
                long result = requireLong(eval(args.get(0), env));
                for (int i = 1; i < args.size(); i++) {
                    result -= requireLong(eval(args.get(i), env));
                }
                return result;
            }
            case "*" -> {
                long result = 1;
                for (Object arg : args) {
                    result *= requireLong(eval(arg, env));
                }
                return result;
            }
            case "/" -> {
                if (args.isEmpty()) throw new EvalError("/ requires at least one argument");
                long result = requireLong(eval(args.get(0), env));
                for (int i = 1; i < args.size(); i++) {
                    long divisor = requireLong(eval(args.get(i), env));
                    if (divisor == 0) throw new EvalError("division by zero");
                    result /= divisor;
                }
                return result;
            }
            case "<" -> {
                requireArgCount(name, args, 2);
                return requireLong(eval(args.get(0), env)) < requireLong(eval(args.get(1), env));
            }
            case ">" -> {
                requireArgCount(name, args, 2);
                return requireLong(eval(args.get(0), env)) > requireLong(eval(args.get(1), env));
            }
            case "=" -> {
                requireArgCount(name, args, 2);
                return requireLong(eval(args.get(0), env)) == requireLong(eval(args.get(1), env));
            }
            case "<=" -> {
                requireArgCount(name, args, 2);
                return requireLong(eval(args.get(0), env)) <= requireLong(eval(args.get(1), env));
            }
            case ">=" -> {
                requireArgCount(name, args, 2);
                return requireLong(eval(args.get(0), env)) >= requireLong(eval(args.get(1), env));
            }
            case "not" -> {
                requireArgCount(name, args, 1);
                Object val = eval(args.get(0), env);
                return val.equals(Boolean.FALSE);
            }
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
            default -> throw new EvalError("unknown procedure: " + name);
        }
    }

    private long requireLong(Object val) throws EvalError {
        if (val instanceof Long l) return l;
        throw new EvalError("expected number, got: " + schemeToString(val));
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
        if (val instanceof SchemeSymbol sym) return sym.name();
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
