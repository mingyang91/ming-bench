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
            if (Character.isWhitespace(c) || c == '(' || c == ')' || c == '"' || c == ';') {
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

    @SuppressWarnings("unchecked")
    private Object eval(Object expr) throws EvalError {
        if (expr instanceof Long || expr instanceof Boolean) {
            return expr;
        }
        if (expr instanceof String s) {
            // String literal
            return s;
        }
        if (expr instanceof SchemeSymbol) {
            throw new EvalError("unbound variable: " + ((SchemeSymbol) expr).name());
        }
        if (expr instanceof List<?> rawList) {
            List<Object> list = new ArrayList<>(rawList);
            if (list.isEmpty()) {
                throw new EvalError("empty application");
            }
            Object first = list.get(0);
            if (first instanceof SchemeSymbol sym) {
                String name = sym.name();
                List<Object> args = list.subList(1, list.size());

                switch (name) {
                    case "+" -> {
                        long result = 0;
                        for (Object arg : args) {
                            result += requireLong(eval(arg));
                        }
                        return result;
                    }
                    case "-" -> {
                        if (args.isEmpty()) {
                            throw new EvalError("- requires at least one argument");
                        }
                        if (args.size() == 1) {
                            return -requireLong(eval(args.get(0)));
                        }
                        long result = requireLong(eval(args.get(0)));
                        for (int i = 1; i < args.size(); i++) {
                            result -= requireLong(eval(args.get(i)));
                        }
                        return result;
                    }
                    case "*" -> {
                        long result = 1;
                        for (Object arg : args) {
                            result *= requireLong(eval(arg));
                        }
                        return result;
                    }
                    case "/" -> {
                        if (args.isEmpty()) {
                            throw new EvalError("/ requires at least one argument");
                        }
                        long result = requireLong(eval(args.get(0)));
                        for (int i = 1; i < args.size(); i++) {
                            long divisor = requireLong(eval(args.get(i)));
                            if (divisor == 0) {
                                throw new EvalError("division by zero");
                            }
                            result /= divisor;
                        }
                        return result;
                    }
                    case "<" -> {
                        requireArgCount(name, args, 2);
                        return requireLong(eval(args.get(0))) < requireLong(eval(args.get(1)));
                    }
                    case ">" -> {
                        requireArgCount(name, args, 2);
                        return requireLong(eval(args.get(0))) > requireLong(eval(args.get(1)));
                    }
                    case "=" -> {
                        requireArgCount(name, args, 2);
                        return requireLong(eval(args.get(0))) == requireLong(eval(args.get(1)));
                    }
                    case "<=" -> {
                        requireArgCount(name, args, 2);
                        return requireLong(eval(args.get(0))) <= requireLong(eval(args.get(1)));
                    }
                    case "not" -> {
                        requireArgCount(name, args, 1);
                        Object val = eval(args.get(0));
                        return val.equals(Boolean.FALSE);
                    }
                    case "and" -> {
                        Object result = Boolean.TRUE;
                        for (Object arg : args) {
                            result = eval(arg);
                            if (result.equals(Boolean.FALSE)) {
                                return Boolean.FALSE;
                            }
                        }
                        return result;
                    }
                    case "or" -> {
                        Object result = Boolean.FALSE;
                        for (Object arg : args) {
                            result = eval(arg);
                            if (!result.equals(Boolean.FALSE)) {
                                return result;
                            }
                        }
                        return result;
                    }
                    default -> throw new EvalError("unknown procedure: " + name);
                }
            }
            throw new EvalError("not a procedure");
        }
        throw new EvalError("cannot evaluate: " + expr);
    }

    private long requireLong(Object val) throws EvalError {
        if (val instanceof Long l) {
            return l;
        }
        throw new EvalError("expected number, got: " + schemeToString(val));
    }

    private void requireArgCount(String name, List<?> args, int expected) throws EvalError {
        if (args.size() != expected) {
            throw new EvalError(name + ": expected " + expected + " arguments, got " + args.size());
        }
    }

    static String schemeToString(Object val) {
        if (val instanceof Long l) {
            return l.toString();
        }
        if (val instanceof Boolean b) {
            return b ? "#t" : "#f";
        }
        if (val instanceof String s) {
            return s; // already includes quotes
        }
        return val.toString();
    }
}
