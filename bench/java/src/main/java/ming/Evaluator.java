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
                                default -> { sb.append('\\'); sb.append(esc); }
                            }
                        }
                    } else {
                        sb.append(input.charAt(i));
                    }
                    i++;
                }
                i++; // skip closing quote
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
                // symbol or number
                StringBuilder sb = new StringBuilder();
                while (i < input.length() && !Character.isWhitespace(input.charAt(i))
                        && input.charAt(i) != '(' && input.charAt(i) != ')'
                        && input.charAt(i) != '"' && input.charAt(i) != ';') {
                    sb.append(input.charAt(i));
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
        pos[0]++;
        if (token.equals("(")) {
            List<Object> list = new ArrayList<>();
            while (pos[0] < tokens.size() && !tokens.get(pos[0]).equals(")")) {
                list.add(parse(tokens, pos));
            }
            if (pos[0] >= tokens.size()) {
                throw new EvalError("missing closing parenthesis");
            }
            pos[0]++; // skip )
            return list;
        } else if (token.equals(")")) {
            throw new EvalError("unexpected )");
        } else {
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

            // Special forms
            if (head instanceof String op) {
                switch (op) {
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

                // Builtin procedures
                List<Object> args = new ArrayList<>();
                for (int i = 1; i < list.size(); i++) {
                    args.add(eval(list.get(i)));
                }
                return applyBuiltin(op, args);
            }
            throw new EvalError("not a procedure: " + schemeToString(head));
        }
        throw new EvalError("cannot eval: " + expr);
    }

    private Object applyBuiltin(String op, List<Object> args) throws EvalError {
        switch (op) {
            case "+" -> {
                long sum = 0;
                for (Object a : args) sum += requireLong(a);
                return sum;
            }
            case "-" -> {
                if (args.isEmpty()) throw new EvalError("- requires at least 1 argument");
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
                if (args.size() < 2) throw new EvalError("/ requires at least 2 arguments");
                long result = requireLong(args.get(0));
                for (int i = 1; i < args.size(); i++) {
                    long divisor = requireLong(args.get(i));
                    if (divisor == 0) throw new EvalError("division by zero");
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
            default -> throw new EvalError("unbound variable: " + op);
        }
    }

    // --- Helpers ---

    private boolean isFalse(Object val) {
        return val instanceof Boolean b && !b;
    }

    private long requireLong(Object val) throws EvalError {
        if (val instanceof Long l) return l;
        throw new EvalError("expected number, got: " + schemeToString(val));
    }

    private void requireArgCount(String op, List<Object> args, int expected) throws EvalError {
        if (args.size() != expected) {
            throw new EvalError(op + ": expected " + expected + " arguments, got " + args.size());
        }
    }

    private String schemeToString(Object val) {
        if (val instanceof Long l) return l.toString();
        if (val instanceof Boolean b) return b ? "#t" : "#f";
        if (val instanceof SchemeString s) return "\"" + s.value() + "\"";
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

    // Internal string wrapper to distinguish from symbols
    record SchemeString(String value) {}
}
