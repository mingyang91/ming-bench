package ming;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

public class Evaluator {

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

        void define(String name, Object val) {
            bindings.put(name, val);
        }
    }

    // --- Lambda (closure) ---

    private record Lambda(List<String> params, Object body, Env closureEnv) {}

    // --- Quoted list wrapper ---

    private record SchemeList(List<Object> elements) {}

    public String evalStr(String input) throws EvalError {
        List<Object> tokens = tokenize(input);
        int[] pos = {0};
        Object lastResult = null;
        Env env = new Env(null);
        while (pos[0] < tokens.size()) {
            Object expr = parse(tokens, pos);
            lastResult = eval(expr, env);
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
        int len = input.length();
        while (i < len) {
            char c = input.charAt(i);
            if (Character.isWhitespace(c)) {
                i++;
            } else if (c == ';') {
                while (i < len && input.charAt(i) != '\n') i++;
            } else if (c == '(') {
                tokens.add("(");
                i++;
            } else if (c == ')') {
                tokens.add(")");
                i++;
            } else if (c == '\'') {
                tokens.add("'");
                i++;
            } else if (c == '"') {
                StringBuilder sb = new StringBuilder();
                i++; // skip opening quote
                while (i < len && input.charAt(i) != '"') {
                    if (input.charAt(i) == '\\') {
                        i++;
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
                        sb.append(input.charAt(i));
                    }
                    i++;
                }
                if (i < len) i++; // skip closing quote
                tokens.add(new SchemeString(sb.toString()));
            } else if (c == '#') {
                if (i + 1 < len) {
                    char next = input.charAt(i + 1);
                    if (next == 't') {
                        tokens.add(Boolean.TRUE);
                        i += 2;
                    } else if (next == 'f') {
                        tokens.add(Boolean.FALSE);
                        i += 2;
                    } else {
                        throw new EvalError("unexpected token: #" + next);
                    }
                } else {
                    throw new EvalError("unexpected end after #");
                }
            } else {
                // symbol or number
                StringBuilder sb = new StringBuilder();
                while (i < len && !Character.isWhitespace(input.charAt(i))
                        && input.charAt(i) != '(' && input.charAt(i) != ')'
                        && input.charAt(i) != '"' && input.charAt(i) != ';'
                        && input.charAt(i) != '\'') {
                    sb.append(input.charAt(i));
                    i++;
                }
                String tok = sb.toString();
                // Try parsing as integer
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
            Object datum = parse(tokens, pos);
            List<Object> quoted = new ArrayList<>();
            quoted.add("quote");
            quoted.add(datum);
            return quoted;
        }
        if (token.equals("(")) {
            pos[0]++;
            List<Object> list = new ArrayList<>();
            while (pos[0] < tokens.size() && !tokens.get(pos[0]).equals(")")) {
                list.add(parse(tokens, pos));
            }
            if (pos[0] >= tokens.size()) {
                throw new EvalError("missing closing paren");
            }
            pos[0]++; // skip )
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
    private Object eval(Object expr, Env env) throws EvalError {
        if (expr instanceof Long || expr instanceof Boolean || expr instanceof SchemeString) {
            return expr;
        }
        if (expr instanceof String sym) {
            // Check for built-in operator symbols
            return env.lookup(sym);
        }
        if (expr instanceof List<?> list) {
            if (list.isEmpty()) {
                throw new EvalError("empty application");
            }
            Object head = list.get(0);

            // Special forms
            if (head instanceof String op) {
                switch (op) {
                    case "quote" -> {
                        if (list.size() != 2) throw new EvalError("quote: expected 1 argument");
                        return quoteValue(list.get(1));
                    }
                    case "if" -> {
                        if (list.size() < 3 || list.size() > 4)
                            throw new EvalError("if: expected 2-3 arguments");
                        Object cond = eval(list.get(1), env);
                        if (isTruthy(cond)) {
                            return eval(list.get(2), env);
                        } else if (list.size() == 4) {
                            return eval(list.get(3), env);
                        } else {
                            return Boolean.FALSE; // unspecified
                        }
                    }
                    case "define" -> {
                        if (list.size() < 3) throw new EvalError("define: too few arguments");
                        Object target = list.get(1);
                        if (target instanceof String name) {
                            // (define x expr)
                            Object val = eval(list.get(2), env);
                            env.define(name, val);
                            return val;
                        } else if (target instanceof List<?> sig) {
                            // (define (f params...) body)
                            if (sig.isEmpty() || !(sig.get(0) instanceof String fname))
                                throw new EvalError("define: invalid function signature");
                            List<String> params = new ArrayList<>();
                            for (int i = 1; i < sig.size(); i++) {
                                if (!(sig.get(i) instanceof String pname))
                                    throw new EvalError("define: parameter must be symbol");
                                params.add(pname);
                            }
                            Object body = list.get(2);
                            Lambda lambda = new Lambda(params, body, env);
                            env.define(fname, lambda);
                            return lambda;
                        } else {
                            throw new EvalError("define: invalid syntax");
                        }
                    }
                    case "lambda" -> {
                        if (list.size() < 3) throw new EvalError("lambda: too few arguments");
                        Object paramSpec = list.get(1);
                        if (!(paramSpec instanceof List<?> paramList))
                            throw new EvalError("lambda: params must be a list");
                        List<String> params = new ArrayList<>();
                        for (Object p : paramList) {
                            if (!(p instanceof String pname))
                                throw new EvalError("lambda: parameter must be symbol");
                            params.add(pname);
                        }
                        Object body = list.get(2);
                        return new Lambda(params, body, env);
                    }
                    case "+" , "-", "*", "/" -> { return evalArith(list, op, env); }
                    case "<", ">", "=", "<=", ">=" -> { return evalCompare(list, op, env); }
                    case "not" -> {
                        checkArgs(list, 1, "not");
                        Object val = eval(list.get(1), env);
                        return isTruthy(val) ? Boolean.FALSE : Boolean.TRUE;
                    }
                    case "and" -> {
                        Object result = Boolean.TRUE;
                        for (int i = 1; i < list.size(); i++) {
                            result = eval(list.get(i), env);
                            if (!isTruthy(result)) return result;
                        }
                        return result;
                    }
                    case "or" -> {
                        Object result = Boolean.FALSE;
                        for (int i = 1; i < list.size(); i++) {
                            result = eval(list.get(i), env);
                            if (isTruthy(result)) return result;
                        }
                        return result;
                    }
                }
            }

            // Procedure application
            Object proc = eval(head, env);
            if (proc instanceof Lambda lambda) {
                List<Object> args = new ArrayList<>();
                for (int i = 1; i < list.size(); i++) {
                    args.add(eval(list.get(i), env));
                }
                if (args.size() != lambda.params.size()) {
                    throw new EvalError("wrong number of arguments: expected " +
                            lambda.params.size() + ", got " + args.size());
                }
                Env callEnv = new Env(lambda.closureEnv);
                for (int i = 0; i < lambda.params.size(); i++) {
                    callEnv.define(lambda.params.get(i), args.get(i));
                }
                return eval(lambda.body, callEnv);
            }
            throw new EvalError("not a procedure: " + schemeToString(proc));
        }
        throw new EvalError("cannot evaluate: " + expr);
    }

    private Object quoteValue(Object datum) {
        if (datum instanceof List<?> list) {
            List<Object> elements = new ArrayList<>();
            for (Object item : list) {
                elements.add(quoteValue(item));
            }
            return new SchemeList(elements);
        }
        return datum;
    }

    private Object evalArith(List<?> list, String op, Env env) throws EvalError {
        if (op.equals("-") && list.size() == 2) {
            long val = asLong(eval(list.get(1), env));
            return -val;
        }
        if (list.size() < 3 && !op.equals("+") && !op.equals("*")) {
            throw new EvalError(op + ": need at least 2 arguments");
        }
        long result;
        if (op.equals("+")) {
            result = 0;
            for (int i = 1; i < list.size(); i++) {
                result += asLong(eval(list.get(i), env));
            }
        } else if (op.equals("*")) {
            result = 1;
            for (int i = 1; i < list.size(); i++) {
                result *= asLong(eval(list.get(i), env));
            }
        } else if (op.equals("-")) {
            result = asLong(eval(list.get(1), env));
            for (int i = 2; i < list.size(); i++) {
                result -= asLong(eval(list.get(i), env));
            }
        } else { // "/"
            result = asLong(eval(list.get(1), env));
            for (int i = 2; i < list.size(); i++) {
                long divisor = asLong(eval(list.get(i), env));
                if (divisor == 0) throw new EvalError("division by zero");
                result /= divisor;
            }
        }
        return result;
    }

    private Object evalCompare(List<?> list, String op, Env env) throws EvalError {
        if (list.size() < 3) {
            throw new EvalError(op + ": need at least 2 arguments");
        }
        long prev = asLong(eval(list.get(1), env));
        for (int i = 2; i < list.size(); i++) {
            long curr = asLong(eval(list.get(i), env));
            boolean ok = switch (op) {
                case "<" -> prev < curr;
                case ">" -> prev > curr;
                case "=" -> prev == curr;
                case "<=" -> prev <= curr;
                case ">=" -> prev >= curr;
                default -> throw new EvalError("unknown comparison: " + op);
            };
            if (!ok) return Boolean.FALSE;
            prev = curr;
        }
        return Boolean.TRUE;
    }

    private boolean isTruthy(Object val) {
        return !(val instanceof Boolean b && !b);
    }

    private long asLong(Object val) throws EvalError {
        if (val instanceof Long l) return l;
        throw new EvalError("expected number, got: " + schemeToString(val));
    }

    private void checkArgs(List<?> list, int expected, String name) throws EvalError {
        if (list.size() - 1 != expected) {
            throw new EvalError(name + ": expected " + expected + " args, got " + (list.size() - 1));
        }
    }

    private String schemeToString(Object val) {
        if (val instanceof Long l) return l.toString();
        if (val instanceof Boolean b) return b ? "#t" : "#f";
        if (val instanceof SchemeString s) return "\"" + s.value() + "\"";
        if (val instanceof SchemeList sl) {
            StringBuilder sb = new StringBuilder("(");
            for (int i = 0; i < sl.elements().size(); i++) {
                if (i > 0) sb.append(" ");
                sb.append(schemeToString(sl.elements().get(i)));
            }
            sb.append(")");
            return sb.toString();
        }
        if (val instanceof Lambda) return "#<procedure>";
        if (val instanceof String s) return s;
        return val.toString();
    }

    // Internal type to distinguish Scheme strings from symbols (Java Strings)
    record SchemeString(String value) {}
}
