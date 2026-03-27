package ming;

import java.util.ArrayList;
import java.util.List;

public class Evaluator {

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
                        && input.charAt(i) != '"' && input.charAt(i) != ';') {
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
    private Object eval(Object expr) throws EvalError {
        if (expr instanceof Long || expr instanceof Boolean || expr instanceof SchemeString) {
            return expr;
        }
        if (expr instanceof String sym) {
            throw new EvalError("unbound variable: " + sym);
        }
        if (expr instanceof List<?> list) {
            if (list.isEmpty()) {
                throw new EvalError("empty application");
            }
            Object head = list.get(0);
            if (head instanceof String op) {
                switch (op) {
                    case "+" -> { return evalArith(list, op); }
                    case "-" -> { return evalArith(list, op); }
                    case "*" -> { return evalArith(list, op); }
                    case "/" -> { return evalArith(list, op); }
                    case "<" -> { return evalCompare(list, op); }
                    case ">" -> { return evalCompare(list, op); }
                    case "=" -> { return evalCompare(list, op); }
                    case "<=" -> { return evalCompare(list, op); }
                    case ">=" -> { return evalCompare(list, op); }
                    case "not" -> {
                        checkArgs(list, 1, "not");
                        Object val = eval(list.get(1));
                        return isTruthy(val) ? Boolean.FALSE : Boolean.TRUE;
                    }
                    case "and" -> {
                        Object result = Boolean.TRUE;
                        for (int i = 1; i < list.size(); i++) {
                            result = eval(list.get(i));
                            if (!isTruthy(result)) return result;
                        }
                        return result;
                    }
                    case "or" -> {
                        Object result = Boolean.FALSE;
                        for (int i = 1; i < list.size(); i++) {
                            result = eval(list.get(i));
                            if (isTruthy(result)) return result;
                        }
                        return result;
                    }
                }
            }
            throw new EvalError("not a procedure: " + schemeToString(head));
        }
        throw new EvalError("cannot evaluate: " + expr);
    }

    private Object evalArith(List<?> list, String op) throws EvalError {
        if (op.equals("-") && list.size() == 2) {
            // unary minus
            long val = asLong(eval(list.get(1)));
            return -val;
        }
        if (list.size() < 3 && !op.equals("+") && !op.equals("*")) {
            throw new EvalError(op + ": need at least 2 arguments");
        }
        long result;
        if (op.equals("+")) {
            result = 0;
            for (int i = 1; i < list.size(); i++) {
                result += asLong(eval(list.get(i)));
            }
        } else if (op.equals("*")) {
            result = 1;
            for (int i = 1; i < list.size(); i++) {
                result *= asLong(eval(list.get(i)));
            }
        } else if (op.equals("-")) {
            result = asLong(eval(list.get(1)));
            for (int i = 2; i < list.size(); i++) {
                result -= asLong(eval(list.get(i)));
            }
        } else { // "/"
            result = asLong(eval(list.get(1)));
            for (int i = 2; i < list.size(); i++) {
                long divisor = asLong(eval(list.get(i)));
                if (divisor == 0) throw new EvalError("division by zero");
                result /= divisor;
            }
        }
        return result;
    }

    private Object evalCompare(List<?> list, String op) throws EvalError {
        if (list.size() < 3) {
            throw new EvalError(op + ": need at least 2 arguments");
        }
        long prev = asLong(eval(list.get(1)));
        for (int i = 2; i < list.size(); i++) {
            long curr = asLong(eval(list.get(i)));
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
        if (val instanceof String s) return s;
        return val.toString();
    }

    // Internal type to distinguish Scheme strings from symbols (Java Strings)
    record SchemeString(String value) {}
}
