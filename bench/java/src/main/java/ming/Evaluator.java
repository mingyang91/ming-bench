package ming;

import java.util.ArrayList;
import java.util.List;

public class Evaluator {

    public String evalStr(String input) throws EvalError {
        List<Object> tokens = tokenize(input);
        int[] pos = {0};
        Object result = null;
        while (pos[0] < tokens.size()) {
            Object expr = parse(tokens, pos);
            result = eval(expr);
        }
        if (result == null) {
            throw new EvalError("no expression");
        }
        return schemeToString(result);
    }

    public EvalResult evalStrWithOutput(String input) throws EvalError {
        throw new EvalError("not implemented");
    }

    record SchemeString(String value) {}

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
            } else if (c == '#') {
                if (i + 1 < len && (input.charAt(i + 1) == 't' || input.charAt(i + 1) == 'f')) {
                    tokens.add(input.charAt(i + 1) == 't' ? Boolean.TRUE : Boolean.FALSE);
                    i += 2;
                } else {
                    throw new EvalError("unexpected character after #");
                }
            } else if (c == '"') {
                StringBuilder sb = new StringBuilder();
                i++;
                while (i < len && input.charAt(i) != '"') {
                    if (input.charAt(i) == '\\' && i + 1 < len) {
                        i++;
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
                    i++;
                }
                if (i >= len) throw new EvalError("unterminated string");
                i++;
                tokens.add(new SchemeString(sb.toString()));
            } else if (c == '-' && i + 1 < len && Character.isDigit(input.charAt(i + 1))
                    && (tokens.isEmpty() || tokens.getLast().equals("("))) {
                StringBuilder sb = new StringBuilder();
                sb.append('-');
                i++;
                while (i < len && Character.isDigit(input.charAt(i))) {
                    sb.append(input.charAt(i));
                    i++;
                }
                tokens.add(Long.parseLong(sb.toString()));
            } else {
                StringBuilder sb = new StringBuilder();
                while (i < len && !Character.isWhitespace(input.charAt(i))
                        && input.charAt(i) != '(' && input.charAt(i) != ')'
                        && input.charAt(i) != '"' && input.charAt(i) != ';') {
                    sb.append(input.charAt(i));
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
            pos[0]++;
            return list;
        } else if (token.equals(")")) {
            throw new EvalError("unexpected )");
        } else {
            pos[0]++;
            return token;
        }
    }

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
            Object head = list.getFirst();
            if (head instanceof String s) {
                switch (s) {
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
                    case "not" -> {
                        if (list.size() != 2) throw new EvalError("not: wrong argument count");
                        Object val = eval(list.get(1));
                        return isFalse(val) ? Boolean.TRUE : Boolean.FALSE;
                    }
                }
            }
            String op = head instanceof String ? (String) head : null;
            if (op == null) throw new EvalError("not a procedure");
            List<Object> args = new ArrayList<>();
            for (int i = 1; i < list.size(); i++) {
                args.add(eval(list.get(i)));
            }
            return applyBuiltin(op, args);
        }
        throw new EvalError("cannot eval: " + expr);
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
            default -> throw new EvalError("unbound variable: " + op);
        };
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

    private String schemeToString(Object val) {
        if (val instanceof Long l) return l.toString();
        if (val instanceof Boolean b) return b ? "#t" : "#f";
        if (val instanceof SchemeString s) return "\"" + s.value() + "\"";
        return val.toString();
    }
}
