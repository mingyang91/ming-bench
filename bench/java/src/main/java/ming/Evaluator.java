package ming;

import java.util.ArrayList;
import java.util.List;

public class Evaluator {

    private static final Object VOID = new Object() {
        @Override public String toString() { return "#<void>"; }
    };

    private final Environment globalEnv = new Environment();

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
        throw new EvalError("not implemented");
    }

    // --- Tokenizer ---

    private List<Object> tokenize(String input) throws EvalError {
        List<Object> tokens = new ArrayList<>();
        int i = 0;
        while (i < input.length()) {
            char c = input.charAt(i);
            if (Character.isWhitespace(c)) {
                i++;
            } else if (c == ';') {
                while (i < input.length() && input.charAt(i) != '\n') i++;
            } else if (c == '(') {
                tokens.add("(");
                i++;
            } else if (c == ')') {
                tokens.add(")");
                i++;
            } else if (c == '"') {
                StringBuilder sb = new StringBuilder();
                i++; // skip opening quote
                while (i < input.length() && input.charAt(i) != '"') {
                    if (input.charAt(i) == '\\') {
                        i++;
                        if (i < input.length()) {
                            char esc = input.charAt(i);
                            switch (esc) {
                                case 'n' -> sb.append('\n');
                                case 't' -> sb.append('\t');
                                case '"' -> sb.append('"');
                                case '\\' -> sb.append('\\');
                                default -> sb.append(esc);
                            }
                        }
                    } else {
                        sb.append(input.charAt(i));
                    }
                    i++;
                }
                if (i < input.length()) i++; // skip closing quote
                tokens.add(new SchemeString(sb.toString()));
            } else if (c == '#') {
                if (i + 1 < input.length()) {
                    char next = input.charAt(i + 1);
                    if (next == 't') {
                        tokens.add(Boolean.TRUE);
                        i += 2;
                    } else if (next == 'f') {
                        tokens.add(Boolean.FALSE);
                        i += 2;
                    } else {
                        throw new EvalError("unexpected character after #: " + next);
                    }
                } else {
                    throw new EvalError("unexpected end after #");
                }
            } else {
                // Symbol or number
                StringBuilder sb = new StringBuilder();
                while (i < input.length()) {
                    char ch = input.charAt(i);
                    if (Character.isWhitespace(ch) || ch == '(' || ch == ')' || ch == '"' || ch == ';') break;
                    sb.append(ch);
                    i++;
                }
                String tok = sb.toString();
                try {
                    tokens.add(Long.parseLong(tok));
                } catch (NumberFormatException e) {
                    tokens.add(tok); // symbol
                }
            }
        }
        return tokens;
    }

    // --- Parser ---

    private Object parse(List<Object> tokens, int[] pos) throws EvalError {
        if (pos[0] >= tokens.size()) {
            throw new EvalError("unexpected end of input");
        }
        Object token = tokens.get(pos[0]);
        if (token.equals("(")) {
            pos[0]++;
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
            pos[0]++;
            return token;
        }
    }

    // --- Evaluator ---

    @SuppressWarnings("unchecked")
    private Object eval(Object expr, Environment env) throws EvalError {
        if (expr instanceof Long || expr instanceof Boolean || expr instanceof SchemeString) {
            return expr;
        }
        if (expr instanceof String symbol) {
            return env.lookup(symbol);
        }
        if (expr instanceof List<?> list) {
            if (list.isEmpty()) {
                throw new EvalError("empty application");
            }
            Object head = list.get(0);

            // Special forms
            if (head instanceof String op) {
                switch (op) {
                    case "define" -> { return evalDefine(list, env); }
                    case "if" -> { return evalIf(list, env); }
                    case "quote" -> {
                        if (list.size() != 2) throw new EvalError("quote: expected 1 argument");
                        return list.get(1);
                    }
                    case "lambda" -> { return evalLambda(list, env); }
                    case "and" -> {
                        Object result = Boolean.TRUE;
                        for (int i = 1; i < list.size(); i++) {
                            result = eval(list.get(i), env);
                            if (Boolean.FALSE.equals(result)) return Boolean.FALSE;
                        }
                        return result;
                    }
                    case "or" -> {
                        Object result = Boolean.FALSE;
                        for (int i = 1; i < list.size(); i++) {
                            result = eval(list.get(i), env);
                            if (!Boolean.FALSE.equals(result)) return result;
                        }
                        return result;
                    }
                    case "not" -> {
                        checkArgs(list, 1, "not");
                        Object val = eval(list.get(1), env);
                        return Boolean.FALSE.equals(val) ? Boolean.TRUE : Boolean.FALSE;
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
        throw new EvalError("cannot evaluate: " + expr);
    }

    @SuppressWarnings("unchecked")
    private Object evalDefine(List<?> list, Environment env) throws EvalError {
        if (list.size() < 3) throw new EvalError("define: bad syntax");
        Object target = list.get(1);
        if (target instanceof String name) {
            // (define x expr)
            Object val = eval(list.get(2), env);
            env.define(name, val);
            return VOID;
        }
        if (target instanceof List<?> sig) {
            // (define (f params...) body...)
            if (sig.isEmpty() || !(sig.get(0) instanceof String name)) {
                throw new EvalError("define: bad syntax");
            }
            List<String> params = new ArrayList<>();
            for (int i = 1; i < sig.size(); i++) {
                if (!(sig.get(i) instanceof String p)) throw new EvalError("define: bad parameter");
                params.add(p);
            }
            List<Object> body = new ArrayList<>();
            for (int i = 2; i < list.size(); i++) {
                body.add(list.get(i));
            }
            Lambda lambda = new Lambda(params, body, env);
            env.define(name, lambda);
            return VOID;
        }
        throw new EvalError("define: bad syntax");
    }

    private Object evalIf(List<?> list, Environment env) throws EvalError {
        if (list.size() < 3 || list.size() > 4) throw new EvalError("if: bad syntax");
        Object cond = eval(list.get(1), env);
        if (!Boolean.FALSE.equals(cond)) {
            return eval(list.get(2), env);
        } else if (list.size() == 4) {
            return eval(list.get(3), env);
        }
        return VOID;
    }

    private Object evalLambda(List<?> list, Environment env) throws EvalError {
        if (list.size() < 3) throw new EvalError("lambda: bad syntax");
        Object paramList = list.get(1);
        if (!(paramList instanceof List<?> plist)) throw new EvalError("lambda: bad parameters");
        List<String> params = new ArrayList<>();
        for (Object p : plist) {
            if (!(p instanceof String s)) throw new EvalError("lambda: bad parameter");
            params.add(s);
        }
        List<Object> body = new ArrayList<>();
        for (int i = 2; i < list.size(); i++) {
            body.add(list.get(i));
        }
        return new Lambda(params, body, env);
    }

    private Object apply(Object proc, List<Object> args) throws EvalError {
        if (proc instanceof Lambda lambda) {
            if (args.size() != lambda.params.size()) {
                throw new EvalError("wrong number of arguments: expected " + lambda.params.size() + ", got " + args.size());
            }
            Environment callEnv = new Environment(lambda.closure);
            for (int i = 0; i < lambda.params.size(); i++) {
                callEnv.define(lambda.params.get(i), args.get(i));
            }
            Object result = VOID;
            for (Object bodyExpr : lambda.body) {
                result = eval(bodyExpr, callEnv);
            }
            return result;
        }
        if (proc instanceof BuiltinProc bp) {
            return bp.apply(args);
        }
        throw new EvalError("not a procedure: " + schemeToString(proc));
    }

    // Builtins are registered as BuiltinProc in the global env
    @FunctionalInterface
    interface BuiltinProc {
        Object apply(List<Object> args) throws EvalError;
    }

    {
        // Register arithmetic and comparison builtins
        globalEnv.define("+", (BuiltinProc) args -> arith(args, "+"));
        globalEnv.define("-", (BuiltinProc) args -> arith(args, "-"));
        globalEnv.define("*", (BuiltinProc) args -> arith(args, "*"));
        globalEnv.define("/", (BuiltinProc) args -> arith(args, "/"));
        globalEnv.define("<", (BuiltinProc) args -> cmp(args, "<"));
        globalEnv.define(">", (BuiltinProc) args -> cmp(args, ">"));
        globalEnv.define("=", (BuiltinProc) args -> cmp(args, "="));
        globalEnv.define("<=", (BuiltinProc) args -> cmp(args, "<="));
        globalEnv.define(">=", (BuiltinProc) args -> cmp(args, ">="));
    }

    // Arithmetic on already-evaluated args
    private Object arith(List<Object> args, String op) throws EvalError {
        if (args.isEmpty()) {
            if (op.equals("+")) return 0L;
            if (op.equals("*")) return 1L;
            throw new EvalError(op + ": need at least 1 argument");
        }
        if (op.equals("-") && args.size() == 1) {
            Object val = args.get(0);
            if (!(val instanceof Long)) throw new EvalError("-: not a number");
            return -((Long) val);
        }
        Object first = args.get(0);
        if (!(first instanceof Long)) throw new EvalError(op + ": not a number");
        long result = (Long) first;
        for (int i = 1; i < args.size(); i++) {
            Object val = args.get(i);
            if (!(val instanceof Long)) throw new EvalError(op + ": not a number");
            long v = (Long) val;
            switch (op) {
                case "+" -> result += v;
                case "-" -> result -= v;
                case "*" -> result *= v;
                case "/" -> {
                    if (v == 0) throw new EvalError("division by zero");
                    result /= v;
                }
            }
        }
        return result;
    }

    private Object cmp(List<Object> args, String op) throws EvalError {
        if (args.size() != 2) throw new EvalError(op + ": expected 2 arguments");
        Object a = args.get(0);
        Object b = args.get(1);
        if (!(a instanceof Long la) || !(b instanceof Long lb)) {
            throw new EvalError(op + ": not a number");
        }
        boolean result = switch (op) {
            case "<" -> la < lb;
            case ">" -> la > lb;
            case "=" -> la.equals(lb);
            case "<=" -> la <= lb;
            case ">=" -> la >= lb;
            default -> false;
        };
        return result;
    }

    private void checkArgs(List<?> list, int expected, String name) throws EvalError {
        if (list.size() - 1 != expected) {
            throw new EvalError(name + ": expected " + expected + " arguments, got " + (list.size() - 1));
        }
    }

    // --- Output formatting ---

    @SuppressWarnings("unchecked")
    static String schemeToString(Object val) {
        if (val instanceof Long) return val.toString();
        if (val instanceof Boolean b) return b ? "#t" : "#f";
        if (val instanceof SchemeString s) return "\"" + s.value() + "\"";
        if (val instanceof Lambda) return "#<procedure>";
        if (val instanceof List<?> list) {
            StringBuilder sb = new StringBuilder("(");
            for (int i = 0; i < list.size(); i++) {
                if (i > 0) sb.append(" ");
                sb.append(schemeToString(list.get(i)));
            }
            sb.append(")");
            return sb.toString();
        }
        if (val instanceof String s) return s; // symbol
        return val.toString();
    }
}
