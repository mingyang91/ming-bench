package ming;

import java.util.ArrayList;
import java.util.List;

public class Evaluator {

    // ---- Value types ----
    sealed interface SchemeVal permits IntVal, BoolVal, StrVal, ListVal, SymbolVal {}
    record IntVal(long value) implements SchemeVal {}
    record BoolVal(boolean value) implements SchemeVal {}
    record StrVal(String value) implements SchemeVal {}
    record ListVal(List<SchemeVal> elements) implements SchemeVal {}

    // ---- Tokenizer ----
    private static List<String> tokenize(String input) {
        List<String> tokens = new ArrayList<>();
        int i = 0;
        while (i < input.length()) {
            char c = input.charAt(i);
            if (Character.isWhitespace(c)) {
                i++;
            } else if (c == ';') {
                // Skip line comments
                while (i < input.length() && input.charAt(i) != '\n') i++;
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
                // String literal
                StringBuilder sb = new StringBuilder();
                sb.append('"');
                i++;
                while (i < input.length() && input.charAt(i) != '"') {
                    if (input.charAt(i) == '\\') {
                        sb.append(input.charAt(i));
                        i++;
                        if (i < input.length()) {
                            sb.append(input.charAt(i));
                            i++;
                        }
                    } else {
                        sb.append(input.charAt(i));
                        i++;
                    }
                }
                if (i < input.length()) {
                    sb.append('"');
                    i++;
                }
                tokens.add(sb.toString());
            } else {
                // Atom (number, symbol, boolean)
                StringBuilder sb = new StringBuilder();
                while (i < input.length() && !Character.isWhitespace(input.charAt(i))
                        && input.charAt(i) != '(' && input.charAt(i) != ')'
                        && input.charAt(i) != '"' && input.charAt(i) != ';') {
                    sb.append(input.charAt(i));
                    i++;
                }
                tokens.add(sb.toString());
            }
        }
        return tokens;
    }

    // ---- Parser ----
    private static int[] parsePos = {0}; // thread-local hack avoided; use instance method

    private static SchemeVal parse(List<String> tokens, int[] pos) throws EvalError {
        if (pos[0] >= tokens.size()) {
            throw new EvalError("unexpected end of input");
        }
        String token = tokens.get(pos[0]);
        if (token.equals("(")) {
            pos[0]++;
            List<SchemeVal> elems = new ArrayList<>();
            while (pos[0] < tokens.size() && !tokens.get(pos[0]).equals(")")) {
                elems.add(parse(tokens, pos));
            }
            if (pos[0] >= tokens.size()) {
                throw new EvalError("missing closing parenthesis");
            }
            pos[0]++; // skip ')'
            return new ListVal(elems);
        } else if (token.equals(")")) {
            throw new EvalError("unexpected )");
        } else {
            pos[0]++;
            return parseAtom(token);
        }
    }

    private static SchemeVal parseAtom(String token) {
        if (token.equals("#t")) return new BoolVal(true);
        if (token.equals("#f")) return new BoolVal(false);
        if (token.startsWith("\"")) {
            // Strip surrounding quotes and handle escapes
            String inner = token.substring(1, token.length() - 1);
            inner = inner.replace("\\n", "\n").replace("\\t", "\t")
                         .replace("\\\\", "\\").replace("\\\"", "\"");
            return new StrVal(inner);
        }
        try {
            return new IntVal(Long.parseLong(token));
        } catch (NumberFormatException e) {
            // It's a symbol - represent as a ListVal with special marker? No, use a string-based symbol.
            // For L01, symbols are only operator names used in function position.
            // We'll represent them as StrVal with a special prefix, but actually let's just
            // handle them inline in eval. We need a SymbolVal.
            return new SymbolVal(token);
        }
    }

    // Add Symbol type
    record SymbolVal(String name) implements SchemeVal {}

    // ---- Evaluator ----
    private SchemeVal eval(SchemeVal expr) throws EvalError {
        return switch (expr) {
            case IntVal v -> v;
            case BoolVal v -> v;
            case StrVal v -> v;
            case SymbolVal v -> throw new EvalError("unbound variable: " + v.name());
            case ListVal v -> {
                List<SchemeVal> elems = v.elements();
                if (elems.isEmpty()) throw new EvalError("empty application");

                SchemeVal head = elems.getFirst();

                // Special forms: and, or
                if (head instanceof SymbolVal sym) {
                    switch (sym.name()) {
                        case "and" -> {
                            SchemeVal result = new BoolVal(true);
                            for (int i = 1; i < elems.size(); i++) {
                                result = eval(elems.get(i));
                                if (isFalse(result)) yield result;
                            }
                            yield result;
                        }
                        case "or" -> {
                            SchemeVal result = new BoolVal(false);
                            for (int i = 1; i < elems.size(); i++) {
                                result = eval(elems.get(i));
                                if (!isFalse(result)) yield result;
                            }
                            yield result;
                        }
                        default -> {}
                    }
                }

                // Evaluate head and args
                if (head instanceof SymbolVal sym) {
                    List<SchemeVal> args = new ArrayList<>();
                    for (int i = 1; i < elems.size(); i++) {
                        args.add(eval(elems.get(i)));
                    }
                    yield applyBuiltin(sym.name(), args);
                }

                throw new EvalError("not a procedure: " + display(head));
            }
        };
    }

    private boolean isFalse(SchemeVal val) {
        return val instanceof BoolVal b && !b.value();
    }

    private SchemeVal applyBuiltin(String name, List<SchemeVal> args) throws EvalError {
        return switch (name) {
            case "+" -> {
                long sum = 0;
                for (SchemeVal a : args) sum += asLong(a);
                yield new IntVal(sum);
            }
            case "-" -> {
                if (args.isEmpty()) throw new EvalError("- requires at least 1 argument");
                if (args.size() == 1) yield new IntVal(-asLong(args.getFirst()));
                long result = asLong(args.getFirst());
                for (int i = 1; i < args.size(); i++) result -= asLong(args.get(i));
                yield new IntVal(result);
            }
            case "*" -> {
                long product = 1;
                for (SchemeVal a : args) product *= asLong(a);
                yield new IntVal(product);
            }
            case "/" -> {
                if (args.size() < 2) throw new EvalError("/ requires at least 2 arguments");
                long result = asLong(args.getFirst());
                for (int i = 1; i < args.size(); i++) {
                    long divisor = asLong(args.get(i));
                    if (divisor == 0) throw new EvalError("division by zero");
                    result /= divisor;
                }
                yield new IntVal(result);
            }
            case "<" -> {
                if (args.size() < 2) throw new EvalError("< requires at least 2 arguments");
                boolean res = true;
                for (int i = 0; i < args.size() - 1; i++) {
                    if (asLong(args.get(i)) >= asLong(args.get(i + 1))) { res = false; break; }
                }
                yield new BoolVal(res);
            }
            case ">" -> {
                if (args.size() < 2) throw new EvalError("> requires at least 2 arguments");
                boolean res = true;
                for (int i = 0; i < args.size() - 1; i++) {
                    if (asLong(args.get(i)) <= asLong(args.get(i + 1))) { res = false; break; }
                }
                yield new BoolVal(res);
            }
            case "=" -> {
                if (args.size() < 2) throw new EvalError("= requires at least 2 arguments");
                boolean res = true;
                long first = asLong(args.getFirst());
                for (int i = 1; i < args.size(); i++) {
                    if (asLong(args.get(i)) != first) { res = false; break; }
                }
                yield new BoolVal(res);
            }
            case "<=" -> {
                if (args.size() < 2) throw new EvalError("<= requires at least 2 arguments");
                boolean res = true;
                for (int i = 0; i < args.size() - 1; i++) {
                    if (asLong(args.get(i)) > asLong(args.get(i + 1))) { res = false; break; }
                }
                yield new BoolVal(res);
            }
            case ">=" -> {
                if (args.size() < 2) throw new EvalError(">= requires at least 2 arguments");
                boolean res = true;
                for (int i = 0; i < args.size() - 1; i++) {
                    if (asLong(args.get(i)) < asLong(args.get(i + 1))) { res = false; break; }
                }
                yield new BoolVal(res);
            }
            case "not" -> {
                if (args.size() != 1) throw new EvalError("not requires exactly 1 argument");
                yield new BoolVal(isFalse(args.getFirst()));
            }
            default -> throw new EvalError("unbound variable: " + name);
        };
    }

    private long asLong(SchemeVal val) throws EvalError {
        if (val instanceof IntVal iv) return iv.value();
        throw new EvalError("expected number, got: " + display(val));
    }

    // ---- Display ----
    private String display(SchemeVal val) {
        return switch (val) {
            case IntVal v -> String.valueOf(v.value());
            case BoolVal v -> v.value() ? "#t" : "#f";
            case StrVal v -> "\"" + v.value() + "\"";
            case SymbolVal v -> v.name();
            case ListVal v -> {
                StringBuilder sb = new StringBuilder("(");
                for (int i = 0; i < v.elements().size(); i++) {
                    if (i > 0) sb.append(" ");
                    sb.append(display(v.elements().get(i)));
                }
                sb.append(")");
                yield sb.toString();
            }
        };
    }

    // ---- Public API ----
    public String evalStr(String input) throws EvalError {
        List<String> tokens = tokenize(input);
        if (tokens.isEmpty()) throw new EvalError("empty input");

        int[] pos = {0};
        SchemeVal result = null;
        while (pos[0] < tokens.size()) {
            result = eval(parse(tokens, pos));
        }
        if (result == null) throw new EvalError("empty input");
        return display(result);
    }

    public EvalResult evalStrWithOutput(String input) throws EvalError {
        throw new EvalError("not implemented");
    }
}
