package ming;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

public class Evaluator {

    @FunctionalInterface
    interface BuiltinFn {
        Object apply(List<Object> args) throws EvalError;
    }

    record Builtin(String name, BuiltinFn fn) {
        Object apply(List<Object> args) throws EvalError {
            return fn.apply(args);
        }
    }

    record SchemeString(String value) {}

    private final Map<String, Object> globals = new HashMap<>();

    public Evaluator() {
        globals.put("+", new Builtin("+", args -> {
            long sum = 0;
            for (Object a : args) sum += requireLong(a);
            return sum;
        }));
        globals.put("-", new Builtin("-", args -> {
            if (args.isEmpty()) throw new EvalError("- requires at least 1 argument");
            if (args.size() == 1) return -requireLong(args.get(0));
            long result = requireLong(args.get(0));
            for (int i = 1; i < args.size(); i++) result -= requireLong(args.get(i));
            return result;
        }));
        globals.put("*", new Builtin("*", args -> {
            long product = 1;
            for (Object a : args) product *= requireLong(a);
            return product;
        }));
        globals.put("/", new Builtin("/", args -> {
            if (args.size() < 2) throw new EvalError("/ requires at least 2 arguments");
            long result = requireLong(args.get(0));
            for (int i = 1; i < args.size(); i++) {
                long divisor = requireLong(args.get(i));
                if (divisor == 0) throw new EvalError("division by zero");
                result /= divisor;
            }
            return result;
        }));
        globals.put("<", new Builtin("<", args -> {
            requireArgCount(args, 2, "<");
            return requireLong(args.get(0)) < requireLong(args.get(1));
        }));
        globals.put(">", new Builtin(">", args -> {
            requireArgCount(args, 2, ">");
            return requireLong(args.get(0)) > requireLong(args.get(1));
        }));
        globals.put("=", new Builtin("=", args -> {
            requireArgCount(args, 2, "=");
            return requireLong(args.get(0)) == requireLong(args.get(1));
        }));
        globals.put("<=", new Builtin("<=", args -> {
            requireArgCount(args, 2, "<=");
            return requireLong(args.get(0)) <= requireLong(args.get(1));
        }));
        globals.put(">=", new Builtin(">=", args -> {
            requireArgCount(args, 2, ">=");
            return requireLong(args.get(0)) >= requireLong(args.get(1));
        }));
        globals.put("not", new Builtin("not", args -> {
            requireArgCount(args, 1, "not");
            return isFalse(args.get(0));
        }));
    }

    public String evalStr(String input) throws EvalError {
        List<Object> tokens = tokenize(input);
        int[] pos = {0};
        Object lastResult = null;
        while (pos[0] < tokens.size()) {
            Object expr = parse(tokens, pos);
            lastResult = eval(expr);
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
                i++;
                while (i < input.length() && input.charAt(i) != '"') {
                    if (input.charAt(i) == '\\') {
                        i++;
                        if (i < input.length()) {
                            switch (input.charAt(i)) {
                                case 'n' -> sb.append('\n');
                                case 't' -> sb.append('\t');
                                case '\\' -> sb.append('\\');
                                case '"' -> sb.append('"');
                                default -> { sb.append('\\'); sb.append(input.charAt(i)); }
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
                        throw new EvalError("unexpected #" + next);
                    }
                } else {
                    throw new EvalError("unexpected end after #");
                }
            } else {
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
                    tokens.add(tok);
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
        if ("(".equals(token)) {
            pos[0]++;
            List<Object> list = new ArrayList<>();
            while (pos[0] < tokens.size() && !")".equals(tokens.get(pos[0]))) {
                list.add(parse(tokens, pos));
            }
            if (pos[0] >= tokens.size()) {
                throw new EvalError("missing closing paren");
            }
            pos[0]++;
            return list;
        } else if (")".equals(token)) {
            throw new EvalError("unexpected )");
        } else {
            pos[0]++;
            return token;
        }
    }

    // --- Evaluator ---

    @SuppressWarnings("unchecked")
    private Object eval(Object expr) throws EvalError {
        if (expr instanceof Long || expr instanceof Boolean || expr instanceof SchemeString) {
            return expr;
        }
        if (expr instanceof String sym) {
            Object val = globals.get(sym);
            if (val == null && !globals.containsKey(sym)) {
                throw new EvalError("unbound variable: " + sym);
            }
            return val;
        }
        if (expr instanceof List<?> list) {
            if (list.isEmpty()) {
                throw new EvalError("empty application");
            }
            Object head = list.get(0);

            // Special forms: and, or
            if (head instanceof String sym) {
                switch (sym) {
                    case "and" -> {
                        Object result = Boolean.TRUE;
                        for (int i = 1; i < list.size(); i++) {
                            result = eval(list.get(i));
                            if (isFalse(result)) return result;
                        }
                        return result;
                    }
                    case "or" -> {
                        Object result = Boolean.FALSE;
                        for (int i = 1; i < list.size(); i++) {
                            result = eval(list.get(i));
                            if (!isFalse(result)) return result;
                        }
                        return result;
                    }
                }
            }

            // Procedure call
            Object proc = eval(head);
            List<Object> args = new ArrayList<>();
            for (int i = 1; i < list.size(); i++) {
                args.add(eval(list.get(i)));
            }
            return applyProc(proc, args);
        }
        throw new EvalError("cannot eval: " + expr);
    }

    private boolean isFalse(Object val) {
        return val instanceof Boolean b && !b;
    }

    private Object applyProc(Object proc, List<Object> args) throws EvalError {
        if (proc instanceof Builtin b) {
            return b.apply(args);
        }
        throw new EvalError("not a procedure: " + schemeToString(proc));
    }

    private long requireLong(Object val) throws EvalError {
        if (val instanceof Long l) return l;
        throw new EvalError("expected number, got: " + schemeToString(val));
    }

    private void requireArgCount(List<Object> args, int n, String name) throws EvalError {
        if (args.size() != n) {
            throw new EvalError(name + " requires " + n + " arguments, got " + args.size());
        }
    }

    // --- Output formatting ---

    private String schemeToString(Object val) {
        if (val instanceof Long l) return l.toString();
        if (val instanceof Boolean b) return b ? "#t" : "#f";
        if (val instanceof SchemeString s) return "\"" + s.value() + "\"";
        if (val instanceof Builtin b) return "#<procedure " + b.name() + ">";
        return String.valueOf(val);
    }
}
