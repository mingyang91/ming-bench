package ming;

import java.util.ArrayList;
import java.util.List;

public class Evaluator {

    private static final Object VOID = new Object() {
        @Override public String toString() { return "#<void>"; }
    };

    static final Object NIL = new Object() {
        @Override public String toString() { return "()"; }
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
                    if (Character.isWhitespace(ch) || ch == '(' || ch == ')' || ch == '"' || ch == ';' || ch == '\'') break;
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
        if (token.equals("'")) {
            pos[0]++;
            Object quoted = parse(tokens, pos);
            List<Object> quoteExpr = new ArrayList<>();
            quoteExpr.add("quote");
            quoteExpr.add(quoted);
            return quoteExpr;
        }
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

    // --- Convert parsed list data to cons cells ---

    private Object listToConsCells(Object datum) {
        if (datum instanceof List<?> list) {
            Object result = NIL;
            for (int i = list.size() - 1; i >= 0; i--) {
                result = new Pair(listToConsCells(list.get(i)), result);
            }
            return result;
        }
        return datum;
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
                        return listToConsCells(list.get(1));
                    }
                    case "lambda" -> { return evalLambda(list, env); }
                    case "begin" -> { return evalBegin(list, env); }
                    case "cond" -> { return evalCond(list, env); }
                    case "let" -> { return evalLet(list, env); }
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

    private Object evalBegin(List<?> list, Environment env) throws EvalError {
        Object result = VOID;
        for (int i = 1; i < list.size(); i++) {
            result = eval(list.get(i), env);
        }
        return result;
    }

    private Object evalCond(List<?> list, Environment env) throws EvalError {
        for (int i = 1; i < list.size(); i++) {
            Object clause = list.get(i);
            if (!(clause instanceof List<?> cl) || cl.isEmpty()) {
                throw new EvalError("cond: bad clause");
            }
            Object test = cl.get(0);
            if (test instanceof String s && s.equals("else")) {
                Object result = VOID;
                for (int j = 1; j < cl.size(); j++) {
                    result = eval(cl.get(j), env);
                }
                return result;
            }
            Object testVal = eval(test, env);
            if (!Boolean.FALSE.equals(testVal)) {
                Object result = testVal;
                for (int j = 1; j < cl.size(); j++) {
                    result = eval(cl.get(j), env);
                }
                return result;
            }
        }
        return VOID;
    }

    @SuppressWarnings("unchecked")
    private Object evalLet(List<?> list, Environment env) throws EvalError {
        if (list.size() < 3) throw new EvalError("let: bad syntax");
        Object second = list.get(1);

        // Named let: (let name ((var init) ...) body...)
        if (second instanceof String name) {
            if (list.size() < 4) throw new EvalError("let: bad syntax");
            Object bindingsList = list.get(2);
            if (!(bindingsList instanceof List<?> bindings)) throw new EvalError("let: bad bindings");
            List<String> params = new ArrayList<>();
            List<Object> inits = new ArrayList<>();
            for (Object b : bindings) {
                if (!(b instanceof List<?> binding) || binding.size() != 2)
                    throw new EvalError("let: bad binding");
                if (!(binding.get(0) instanceof String varName))
                    throw new EvalError("let: bad binding variable");
                params.add(varName);
                inits.add(eval(binding.get(1), env));
            }
            List<Object> body = new ArrayList<>();
            for (int i = 3; i < list.size(); i++) {
                body.add(list.get(i));
            }
            Environment letEnv = new Environment(env);
            Lambda lambda = new Lambda(params, body, letEnv);
            letEnv.define(name, lambda);
            Environment callEnv = new Environment(letEnv);
            for (int i = 0; i < params.size(); i++) {
                callEnv.define(params.get(i), inits.get(i));
            }
            Object result = VOID;
            for (Object bodyExpr : body) {
                result = eval(bodyExpr, callEnv);
            }
            return result;
        }

        // Regular let: (let ((var init) ...) body...)
        if (!(second instanceof List<?> bindings)) throw new EvalError("let: bad bindings");
        Environment letEnv = new Environment(env);
        for (Object b : bindings) {
            if (!(b instanceof List<?> binding) || binding.size() != 2)
                throw new EvalError("let: bad binding");
            if (!(binding.get(0) instanceof String varName))
                throw new EvalError("let: bad binding variable");
            Object val = eval(binding.get(1), env);
            letEnv.define(varName, val);
        }
        Object result = VOID;
        for (int i = 2; i < list.size(); i++) {
            result = eval(list.get(i), letEnv);
        }
        return result;
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

        // List operations
        globalEnv.define("cons", (BuiltinProc) args -> {
            if (args.size() != 2) throw new EvalError("cons: expected 2 arguments");
            return new Pair(args.get(0), args.get(1));
        });
        globalEnv.define("car", (BuiltinProc) args -> {
            if (args.size() != 1) throw new EvalError("car: expected 1 argument");
            if (!(args.get(0) instanceof Pair p)) throw new EvalError("car: not a pair");
            return p.car;
        });
        globalEnv.define("cdr", (BuiltinProc) args -> {
            if (args.size() != 1) throw new EvalError("cdr: expected 1 argument");
            if (!(args.get(0) instanceof Pair p)) throw new EvalError("cdr: not a pair");
            return p.cdr;
        });
        globalEnv.define("null?", (BuiltinProc) args -> {
            if (args.size() != 1) throw new EvalError("null?: expected 1 argument");
            return args.get(0) == NIL;
        });
        globalEnv.define("list", (BuiltinProc) args -> {
            Object result = NIL;
            for (int i = args.size() - 1; i >= 0; i--) {
                result = new Pair(args.get(i), result);
            }
            return result;
        });
        globalEnv.define("length", (BuiltinProc) args -> {
            if (args.size() != 1) throw new EvalError("length: expected 1 argument");
            Object obj = args.get(0);
            long count = 0;
            while (obj instanceof Pair p) {
                count++;
                obj = p.cdr;
            }
            if (obj != NIL) throw new EvalError("length: not a proper list");
            return count;
        });
        globalEnv.define("append", (BuiltinProc) args -> {
            if (args.isEmpty()) return NIL;
            if (args.size() == 1) return args.get(0);
            // Append all lists
            Object result = args.get(args.size() - 1);
            for (int i = args.size() - 2; i >= 0; i--) {
                Object lst = args.get(i);
                // Collect elements of lst, then prepend to result
                List<Object> elems = new ArrayList<>();
                Object cur = lst;
                while (cur instanceof Pair p) {
                    elems.add(p.car);
                    cur = p.cdr;
                }
                for (int j = elems.size() - 1; j >= 0; j--) {
                    result = new Pair(elems.get(j), result);
                }
            }
            return result;
        });

        // Type predicates
        globalEnv.define("number?", (BuiltinProc) args -> {
            if (args.size() != 1) throw new EvalError("number?: expected 1 argument");
            return args.get(0) instanceof Long;
        });
        globalEnv.define("string?", (BuiltinProc) args -> {
            if (args.size() != 1) throw new EvalError("string?: expected 1 argument");
            return args.get(0) instanceof SchemeString;
        });
        globalEnv.define("boolean?", (BuiltinProc) args -> {
            if (args.size() != 1) throw new EvalError("boolean?: expected 1 argument");
            return args.get(0) instanceof Boolean;
        });
        globalEnv.define("pair?", (BuiltinProc) args -> {
            if (args.size() != 1) throw new EvalError("pair?: expected 1 argument");
            return args.get(0) instanceof Pair;
        });
        globalEnv.define("symbol?", (BuiltinProc) args -> {
            if (args.size() != 1) throw new EvalError("symbol?: expected 1 argument");
            return args.get(0) instanceof String;
        });
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
        if (val instanceof String s) return s; // symbol
        return val.toString();
    }
}
