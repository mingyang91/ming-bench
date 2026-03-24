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
    private Object eval(Object expr) throws EvalError {
        if (expr instanceof Long || expr instanceof Boolean || expr instanceof SchemeString) {
            return expr;
        }
        if (expr instanceof String symbol) {
            throw new EvalError("unbound variable: " + symbol);
        }
        if (expr instanceof List<?> list) {
            if (list.isEmpty()) {
                throw new EvalError("empty application");
            }
            Object head = list.get(0);
            if (head instanceof String op) {
                switch (op) {
                    case "+" -> { return arith(list, op); }
                    case "-" -> { return arith(list, op); }
                    case "*" -> { return arith(list, op); }
                    case "/" -> { return arith(list, op); }
                    case "<" -> { return compare(list, op); }
                    case ">" -> { return compare(list, op); }
                    case "=" -> { return compare(list, op); }
                    case "<=" -> { return compare(list, op); }
                    case ">=" -> { return compare(list, op); }
                    case "not" -> {
                        checkArgs(list, 1, "not");
                        Object val = eval(list.get(1));
                        return Boolean.FALSE.equals(val) ? Boolean.TRUE : Boolean.FALSE;
                    }
                    case "and" -> {
                        Object result = Boolean.TRUE;
                        for (int i = 1; i < list.size(); i++) {
                            result = eval(list.get(i));
                            if (Boolean.FALSE.equals(result)) return Boolean.FALSE;
                        }
                        return result;
                    }
                    case "or" -> {
                        Object result = Boolean.FALSE;
                        for (int i = 1; i < list.size(); i++) {
                            result = eval(list.get(i));
                            if (!Boolean.FALSE.equals(result)) return result;
                        }
                        return result;
                    }
                }
            }
            throw new EvalError("not a procedure: " + schemeToString(head));
        }
        throw new EvalError("cannot evaluate: " + expr);
    }

    private Object arith(List<?> list, String op) throws EvalError {
        if (list.size() == 1) {
            // Zero args
            if (op.equals("+")) return 0L;
            if (op.equals("*")) return 1L;
            throw new EvalError(op + ": need at least 1 argument");
        }
        if (op.equals("-") && list.size() == 2) {
            // Unary minus
            Object val = eval(list.get(1));
            if (!(val instanceof Long)) throw new EvalError("-: not a number");
            return -((Long) val);
        }
        long result;
        Object first = eval(list.get(1));
        if (!(first instanceof Long)) throw new EvalError(op + ": not a number");
        result = (Long) first;
        for (int i = 2; i < list.size(); i++) {
            Object val = eval(list.get(i));
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

    private Object compare(List<?> list, String op) throws EvalError {
        checkArgs(list, 2, op);
        Object a = eval(list.get(1));
        Object b = eval(list.get(2));
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

    static String schemeToString(Object val) {
        if (val instanceof Long) return val.toString();
        if (val instanceof Boolean b) return b ? "#t" : "#f";
        if (val instanceof SchemeString s) return "\"" + s.value() + "\"";
        return val.toString();
    }
}
