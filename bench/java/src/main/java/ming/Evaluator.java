package ming;

import java.util.ArrayList;
import java.util.List;
import java.util.Set;

public class Evaluator {

    private static final Set<String> BUILTINS = Set.of(
        "+", "-", "*", "/", "<", ">", "=", "<=", ">=", "not"
    );

    public String evalStr(String input) throws EvalError {
        List<Object> exprs = parse(input);
        Object result = null;
        for (Object expr : exprs) {
            result = eval(expr);
        }
        return schemeToString(result);
    }

    public EvalResult evalStrWithOutput(String input) throws EvalError {
        throw new EvalError("not implemented");
    }

    // --- Data types ---
    // Integer -> Long
    // Boolean -> Boolean
    // String -> SchemeString (wrapper to distinguish from symbols)
    // Symbol -> String
    // List -> SchemeList (linked list)

    static final Object VOID = new Object() {
        @Override public String toString() { return "#<void>"; }
    };

    record SchemeString(String value) {}

    sealed interface SchemeList permits Pair, Empty {
    }

    record Pair(Object car, Object cdr) implements SchemeList {}

    enum Empty implements SchemeList { NIL }

    // --- Printing ---

    private String schemeToString(Object val) {
        if (val == VOID) return "#<void>";
        if (val instanceof Long n) return n.toString();
        if (val instanceof Boolean b) return b ? "#t" : "#f";
        if (val instanceof SchemeString s) return "\"" + s.value() + "\"";
        if (val instanceof String sym) return sym;
        if (val == Empty.NIL) return "()";
        if (val instanceof Pair p) {
            StringBuilder sb = new StringBuilder("(");
            sb.append(schemeToString(p.car()));
            Object rest = p.cdr();
            while (rest instanceof Pair pr) {
                sb.append(" ").append(schemeToString(pr.car()));
                rest = pr.cdr();
            }
            if (rest != Empty.NIL) {
                sb.append(" . ").append(schemeToString(rest));
            }
            sb.append(")");
            return sb.toString();
        }
        return val.toString();
    }

    // --- Parser ---

    private List<Object> parse(String input) throws EvalError {
        List<Token> tokens = tokenize(input);
        List<Object> exprs = new ArrayList<>();
        int[] pos = {0};
        while (pos[0] < tokens.size()) {
            exprs.add(parseExpr(tokens, pos));
        }
        return exprs;
    }

    enum TokenType { LPAREN, RPAREN, QUOTE, STRING, ATOM }

    record Token(TokenType type, String value) {}

    private List<Token> tokenize(String input) throws EvalError {
        List<Token> tokens = new ArrayList<>();
        int i = 0;
        int len = input.length();
        while (i < len) {
            char c = input.charAt(i);
            if (Character.isWhitespace(c)) { i++; continue; }
            if (c == ';') {
                while (i < len && input.charAt(i) != '\n') i++;
                continue;
            }
            if (c == '(') { tokens.add(new Token(TokenType.LPAREN, "(")); i++; continue; }
            if (c == ')') { tokens.add(new Token(TokenType.RPAREN, ")")); i++; continue; }
            if (c == '\'') { tokens.add(new Token(TokenType.QUOTE, "'")); i++; continue; }
            if (c == '"') {
                StringBuilder sb = new StringBuilder();
                i++;
                while (i < len && input.charAt(i) != '"') {
                    if (input.charAt(i) == '\\') {
                        i++;
                        if (i < len) {
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
                if (i < len) i++; // skip closing quote
                tokens.add(new Token(TokenType.STRING, sb.toString()));
                continue;
            }
            // Atom
            StringBuilder sb = new StringBuilder();
            while (i < len) {
                char ch = input.charAt(i);
                if (Character.isWhitespace(ch) || ch == '(' || ch == ')' || ch == '"' || ch == ';') break;
                sb.append(ch);
                i++;
            }
            tokens.add(new Token(TokenType.ATOM, sb.toString()));
        }
        return tokens;
    }

    private Object parseExpr(List<Token> tokens, int[] pos) throws EvalError {
        if (pos[0] >= tokens.size()) throw new EvalError("unexpected end of input");
        Token tok = tokens.get(pos[0]);
        pos[0]++;

        return switch (tok.type()) {
            case LPAREN -> {
                List<Object> elems = new ArrayList<>();
                while (pos[0] < tokens.size() && tokens.get(pos[0]).type() != TokenType.RPAREN) {
                    elems.add(parseExpr(tokens, pos));
                }
                if (pos[0] >= tokens.size()) throw new EvalError("missing closing parenthesis");
                pos[0]++; // skip )
                yield elems;
            }
            case RPAREN -> throw new EvalError("unexpected )");
            case QUOTE -> {
                Object quoted = parseExpr(tokens, pos);
                yield List.of("quote", quoted);
            }
            case STRING -> new SchemeString(tok.value());
            case ATOM -> parseAtom(tok.value());
        };
    }

    private Object parseAtom(String s) {
        if (s.equals("#t")) return Boolean.TRUE;
        if (s.equals("#f")) return Boolean.FALSE;
        try {
            return Long.parseLong(s);
        } catch (NumberFormatException e) {
            return s; // symbol
        }
    }

    // --- Evaluator ---

    @SuppressWarnings("unchecked")
    private Object eval(Object expr) throws EvalError {
        if (expr instanceof Long || expr instanceof Boolean || expr instanceof SchemeString) {
            return expr;
        }
        if (expr instanceof String sym) {
            if (BUILTINS.contains(sym)) return sym;
            throw new EvalError("unbound variable: " + sym);
        }
        if (expr instanceof List<?> list) {
            if (list.isEmpty()) throw new EvalError("empty application");
            Object first = list.get(0);

            // Special forms
            if (first instanceof String sym) {
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

            // Function application
            Object proc = eval(first);
            List<Object> args = new ArrayList<>();
            for (int i = 1; i < list.size(); i++) {
                args.add(eval(list.get(i)));
            }
            return apply(proc, args);
        }
        throw new EvalError("cannot evaluate: " + expr);
    }

    private boolean isFalse(Object val) {
        return val instanceof Boolean b && !b;
    }

    private Object apply(Object proc, List<Object> args) throws EvalError {
        if (proc instanceof String sym) {
            return applyBuiltin(sym, args);
        }
        throw new EvalError("not a procedure: " + schemeToString(proc));
    }

    private Object applyBuiltin(String name, List<Object> args) throws EvalError {
        return switch (name) {
            case "+" -> {
                long sum = 0;
                for (Object a : args) sum += asLong(a, "+");
                yield sum;
            }
            case "-" -> {
                if (args.isEmpty()) throw new EvalError("-: need at least 1 argument");
                if (args.size() == 1) yield -asLong(args.get(0), "-");
                long result = asLong(args.get(0), "-");
                for (int i = 1; i < args.size(); i++) result -= asLong(args.get(i), "-");
                yield result;
            }
            case "*" -> {
                long prod = 1;
                for (Object a : args) prod *= asLong(a, "*");
                yield prod;
            }
            case "/" -> {
                if (args.isEmpty()) throw new EvalError("/: need at least 1 argument");
                long result = asLong(args.get(0), "/");
                for (int i = 1; i < args.size(); i++) {
                    long d = asLong(args.get(i), "/");
                    if (d == 0) throw new EvalError("division by zero");
                    result /= d;
                }
                yield result;
            }
            case "<" -> {
                checkMinArgs(args, 2, "<");
                yield asLong(args.get(0), "<") < asLong(args.get(1), "<");
            }
            case ">" -> {
                checkMinArgs(args, 2, ">");
                yield asLong(args.get(0), ">") > asLong(args.get(1), ">");
            }
            case "=" -> {
                checkMinArgs(args, 2, "=");
                yield asLong(args.get(0), "=") == asLong(args.get(1), "=");
            }
            case "<=" -> {
                checkMinArgs(args, 2, "<=");
                yield asLong(args.get(0), "<=") <= asLong(args.get(1), "<=");
            }
            case ">=" -> {
                checkMinArgs(args, 2, ">=");
                yield asLong(args.get(0), ">=") >= asLong(args.get(1), ">=");
            }
            case "not" -> {
                checkMinArgs(args, 1, "not");
                yield isFalse(args.get(0));
            }
            default -> throw new EvalError("unbound variable: " + name);
        };
    }

    private long asLong(Object val, String context) throws EvalError {
        if (val instanceof Long n) return n;
        throw new EvalError(context + ": expected number, got " + schemeToString(val));
    }

    private void checkMinArgs(List<Object> args, int min, String name) throws EvalError {
        if (args.size() < min) throw new EvalError(name + ": expected at least " + min + " arguments");
    }
}
