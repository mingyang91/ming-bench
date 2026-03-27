package ming;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

public class Evaluator {

    // Top-level environment, persisted across evalStr calls
    private final Env globalEnv = createGlobalEnv();
    private StringBuilder outputBuffer = new StringBuilder();

    public String evalStr(String input) throws EvalError {
        outputBuffer.setLength(0);
        List<Object> exprs = parse(input);
        Object result = null;
        for (Object expr : exprs) {
            result = eval(expr, globalEnv);
        }
        return schemeToString(result);
    }

    public EvalResult evalStrWithOutput(String input) throws EvalError {
        outputBuffer.setLength(0);
        List<Object> exprs = parse(input);
        Object result = null;
        for (Object expr : exprs) {
            result = eval(expr, globalEnv);
        }
        return new EvalResult(schemeToString(result), outputBuffer.toString());
    }

    // --- Environment ---

    static class Env {
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

        void define(String name, Object value) {
            bindings.put(name, value);
        }
    }

    // --- Data types ---

    static final Object VOID = new Object() {
        @Override public String toString() { return "#<void>"; }
    };

    static class SchemeString {
        private char[] chars;
        SchemeString(String value) { this.chars = value.toCharArray(); }
        String value() { return new String(chars); }
        char charAt(int i) { return chars[i]; }
        void setChar(int i, char c) { chars[i] = c; }
        int length() { return chars.length; }
    }

    record SchemeChar(char value) {}

    sealed interface SchemeList permits Pair, Empty {}

    record Pair(Object car, Object cdr) implements SchemeList {}

    enum Empty implements SchemeList { NIL }

    // Lambda (closure)
    record Lambda(List<String> params, List<Object> body, Env env) {}

    // Source position-aware types
    record LocatedSymbol(String name, int line, int col) {}

    static class LocatedList extends ArrayList<Object> {
        final int line, col;
        LocatedList(int line, int col) { super(); this.line = line; this.col = col; }
    }

    // --- Global environment ---

    private Env createGlobalEnv() {
        Env env = new Env(null);
        for (String name : List.of("+", "-", "*", "/", "<", ">", "=", "<=", ">=", "not",
                "cons", "car", "cdr", "null?", "list", "length",
                "string?", "number?", "boolean?", "pair?", "symbol?", "append",
                "display", "write", "newline",
                "string-append", "string-length", "substring",
                "string->number", "number->string",
                "symbol->string", "string->symbol",
                "string-ref", "char?",
                "string-copy", "string-set!")) {
            env.define(name, "builtin:" + name);
        }
        return env;
    }

    // --- Printing ---

    private String schemeToString(Object val) {
        if (val == VOID) return "#<void>";
        if (val instanceof Long n) return n.toString();
        if (val instanceof Boolean b) return b ? "#t" : "#f";
        if (val instanceof SchemeChar c) return formatChar(c.value());
        if (val instanceof SchemeString s) return "\"" + s.value() + "\"";
        if (val instanceof String sym) return sym;
        if (val == Empty.NIL) return "()";
        if (val instanceof Pair p) return pairToString(p, true);
        if (val instanceof Lambda) return "#<procedure>";
        return val.toString();
    }

    private String displayToString(Object val) {
        if (val == VOID) return "#<void>";
        if (val instanceof Long n) return n.toString();
        if (val instanceof Boolean b) return b ? "#t" : "#f";
        if (val instanceof SchemeChar c) return String.valueOf(c.value());
        if (val instanceof SchemeString s) return s.value();
        if (val instanceof String sym) return sym;
        if (val == Empty.NIL) return "()";
        if (val instanceof Pair p) return pairToString(p, false);
        if (val instanceof Lambda) return "#<procedure>";
        return val.toString();
    }

    private String pairToString(Pair p, boolean writeMode) {
        StringBuilder sb = new StringBuilder("(");
        sb.append(writeMode ? schemeToString(p.car()) : displayToString(p.car()));
        Object rest = p.cdr();
        while (rest instanceof Pair pr) {
            sb.append(" ").append(writeMode ? schemeToString(pr.car()) : displayToString(pr.car()));
            rest = pr.cdr();
        }
        if (rest != Empty.NIL) {
            sb.append(" . ").append(writeMode ? schemeToString(rest) : displayToString(rest));
        }
        sb.append(")");
        return sb.toString();
    }

    private String formatChar(char c) {
        return switch (c) {
            case ' ' -> "#\\space";
            case '\n' -> "#\\newline";
            case '\t' -> "#\\tab";
            default -> "#\\" + c;
        };
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

    record Token(TokenType type, String value, int line, int col) {}

    private List<Token> tokenize(String input) throws EvalError {
        List<Token> tokens = new ArrayList<>();
        int i = 0;
        int len = input.length();
        int line = 1, col = 1;
        while (i < len) {
            char c = input.charAt(i);
            if (c == '\n') { i++; line++; col = 1; continue; }
            if (Character.isWhitespace(c)) { i++; col++; continue; }
            if (c == ';') {
                while (i < len && input.charAt(i) != '\n') { i++; col++; }
                continue;
            }
            if (c == '(') { tokens.add(new Token(TokenType.LPAREN, "(", line, col)); i++; col++; continue; }
            if (c == ')') { tokens.add(new Token(TokenType.RPAREN, ")", line, col)); i++; col++; continue; }
            if (c == '\'') { tokens.add(new Token(TokenType.QUOTE, "'", line, col)); i++; col++; continue; }
            if (c == '"') {
                int startCol = col;
                StringBuilder sb = new StringBuilder();
                i++; col++;
                while (i < len && input.charAt(i) != '"') {
                    if (input.charAt(i) == '\\') {
                        i++; col++;
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
                    if (input.charAt(i) == '\n') { line++; col = 1; } else { col++; }
                    i++;
                }
                if (i < len) { i++; col++; } // skip closing quote
                tokens.add(new Token(TokenType.STRING, sb.toString(), line, startCol));
                continue;
            }
            // Atom
            int startCol = col;
            StringBuilder sb = new StringBuilder();
            while (i < len) {
                char ch = input.charAt(i);
                if (Character.isWhitespace(ch) || ch == '(' || ch == ')' || ch == '"' || ch == ';') break;
                sb.append(ch);
                i++; col++;
            }
            tokens.add(new Token(TokenType.ATOM, sb.toString(), line, startCol));
        }
        return tokens;
    }

    private Object parseExpr(List<Token> tokens, int[] pos) throws EvalError {
        if (pos[0] >= tokens.size()) throw new EvalError("unexpected end of input");
        Token tok = tokens.get(pos[0]);
        pos[0]++;

        return switch (tok.type()) {
            case LPAREN -> {
                LocatedList elems = new LocatedList(tok.line(), tok.col());
                while (pos[0] < tokens.size() && tokens.get(pos[0]).type() != TokenType.RPAREN) {
                    elems.add(parseExpr(tokens, pos));
                }
                if (pos[0] >= tokens.size()) throw new EvalError(tok.line() + ":" + tok.col() + ": missing closing parenthesis");
                pos[0]++; // skip )
                yield elems;
            }
            case RPAREN -> throw new EvalError(tok.line() + ":" + tok.col() + ": unexpected )");
            case QUOTE -> {
                Object quoted = parseExpr(tokens, pos);
                LocatedList q = new LocatedList(tok.line(), tok.col());
                q.add("quote");
                q.add(quoted);
                yield q;
            }
            case STRING -> new SchemeString(tok.value());
            case ATOM -> parseAtom(tok.value(), tok.line(), tok.col());
        };
    }

    private Object parseAtom(String s, int line, int col) {
        if (s.equals("#t")) return Boolean.TRUE;
        if (s.equals("#f")) return Boolean.FALSE;
        if (s.startsWith("#\\")) {
            String charName = s.substring(2);
            return switch (charName) {
                case "space" -> new SchemeChar(' ');
                case "newline" -> new SchemeChar('\n');
                case "tab" -> new SchemeChar('\t');
                default -> {
                    if (charName.length() == 1) yield new SchemeChar(charName.charAt(0));
                    yield new LocatedSymbol(s, line, col);
                }
            };
        }
        try {
            return Long.parseLong(s);
        } catch (NumberFormatException e) {
            return new LocatedSymbol(s, line, col); // symbol with position
        }
    }

    // --- Error helper ---

    private static EvalError errAt(int line, int col, String msg) {
        return new EvalError(line + ":" + col + ": " + msg);
    }

    private static boolean hasPosition(String msg) {
        return msg.matches(".*\\d+:\\d+.*");
    }

    // Helper to get symbol name from either String or LocatedSymbol
    private static String symName(Object o) {
        if (o instanceof String s) return s;
        if (o instanceof LocatedSymbol ls) return ls.name();
        return null;
    }

    // --- Evaluator ---

    @SuppressWarnings("unchecked")
    private Object eval(Object expr, Env env) throws EvalError {
        if (expr instanceof Long || expr instanceof Boolean || expr instanceof SchemeString || expr instanceof SchemeChar) {
            return expr;
        }
        if (expr instanceof LocatedSymbol ls) {
            try {
                return env.lookup(ls.name());
            } catch (EvalError e) {
                throw errAt(ls.line(), ls.col(), e.getMessage());
            }
        }
        if (expr instanceof String sym) {
            return env.lookup(sym);
        }
        if (expr instanceof List<?> list) {
            int eline = 0, ecol = 0;
            if (list instanceof LocatedList ll) { eline = ll.line; ecol = ll.col; }

            if (list.isEmpty()) throw errAt(eline, ecol, "empty application");
            Object first = list.get(0);
            String sym = symName(first);

            // Special forms
            if (sym != null) {
                switch (sym) {
                    case "quote" -> {
                        if (list.size() != 2) throw errAt(eline, ecol, "quote: expected 1 argument");
                        return toSchemeValue(list.get(1));
                    }
                    case "if" -> {
                        if (list.size() < 3 || list.size() > 4) throw errAt(eline, ecol, "if: bad syntax");
                        Object cond = eval(list.get(1), env);
                        if (!isFalse(cond)) {
                            return eval(list.get(2), env);
                        } else if (list.size() == 4) {
                            return eval(list.get(3), env);
                        }
                        return VOID;
                    }
                    case "define" -> {
                        if (list.size() < 3) throw errAt(eline, ecol, "define: bad syntax");
                        Object target = list.get(1);
                        String targetName = symName(target);
                        if (targetName != null) {
                            // (define x expr)
                            env.define(targetName, eval(list.get(2), env));
                        } else if (target instanceof List<?> sig) {
                            // (define (f params...) body...)
                            String fname = symName(sig.isEmpty() ? null : sig.get(0));
                            if (sig.isEmpty() || fname == null)
                                throw errAt(eline, ecol, "define: bad syntax");
                            List<String> params = new ArrayList<>();
                            for (int i = 1; i < sig.size(); i++) {
                                String pname = symName(sig.get(i));
                                if (pname == null)
                                    throw errAt(eline, ecol, "define: parameter must be a symbol");
                                params.add(pname);
                            }
                            List<Object> body = new ArrayList<>();
                            for (int i = 2; i < list.size(); i++) body.add(list.get(i));
                            env.define(fname, new Lambda(params, body, env));
                        } else {
                            throw errAt(eline, ecol, "define: bad syntax");
                        }
                        return VOID;
                    }
                    case "lambda" -> {
                        if (list.size() < 3) throw errAt(eline, ecol, "lambda: bad syntax");
                        if (!(list.get(1) instanceof List<?> paramList))
                            throw errAt(eline, ecol, "lambda: bad syntax");
                        List<String> params = new ArrayList<>();
                        for (Object p : paramList) {
                            String pname = symName(p);
                            if (pname == null)
                                throw errAt(eline, ecol, "lambda: parameter must be a symbol");
                            params.add(pname);
                        }
                        List<Object> body = new ArrayList<>();
                        for (int i = 2; i < list.size(); i++) body.add(list.get(i));
                        return new Lambda(params, body, env);
                    }
                    case "and" -> {
                        Object result = Boolean.TRUE;
                        for (int i = 1; i < list.size(); i++) {
                            result = eval(list.get(i), env);
                            if (isFalse(result)) return result;
                        }
                        return result;
                    }
                    case "begin" -> {
                        Object result = VOID;
                        for (int i = 1; i < list.size(); i++) {
                            result = eval(list.get(i), env);
                        }
                        return result;
                    }
                    case "let" -> {
                        if (list.size() < 3) throw errAt(eline, ecol, "let: bad syntax");
                        Object second = list.get(1);
                        String secondName = symName(second);
                        // Named let: (let name ((var init) ...) body ...)
                        if (secondName != null) {
                            if (list.size() < 4) throw errAt(eline, ecol, "let: bad syntax");
                            List<?> bindings = (List<?>) list.get(2);
                            List<String> params = new ArrayList<>();
                            List<Object> inits = new ArrayList<>();
                            for (Object b : bindings) {
                                List<?> binding = (List<?>) b;
                                String pname = symName(binding.size() >= 1 ? binding.get(0) : null);
                                if (binding.size() != 2 || pname == null)
                                    throw errAt(eline, ecol, "let: bad binding");
                                params.add(pname);
                                inits.add(eval(binding.get(1), env));
                            }
                            List<Object> body = new ArrayList<>();
                            for (int i = 3; i < list.size(); i++) body.add(list.get(i));
                            Env letEnv = new Env(env);
                            Lambda loopLam = new Lambda(params, body, letEnv);
                            letEnv.define(secondName, loopLam);
                            List<Object> args = new ArrayList<>(inits);
                            return apply(loopLam, args);
                        }
                        // Regular let
                        List<?> bindings = (List<?>) second;
                        Env letEnv = new Env(env);
                        for (Object b : bindings) {
                            List<?> binding = (List<?>) b;
                            String bname = symName(binding.size() >= 1 ? binding.get(0) : null);
                            if (binding.size() != 2 || bname == null)
                                throw errAt(eline, ecol, "let: bad binding");
                            letEnv.define(bname, eval(binding.get(1), env));
                        }
                        Object result = VOID;
                        for (int i = 2; i < list.size(); i++) {
                            result = eval(list.get(i), letEnv);
                        }
                        return result;
                    }
                    case "cond" -> {
                        for (int i = 1; i < list.size(); i++) {
                            List<?> clause = (List<?>) list.get(i);
                            if (clause.isEmpty()) throw errAt(eline, ecol, "cond: empty clause");
                            Object test = clause.get(0);
                            String testSym = symName(test);
                            if ("else".equals(testSym)) {
                                Object result = VOID;
                                for (int j = 1; j < clause.size(); j++) {
                                    result = eval(clause.get(j), env);
                                }
                                return result;
                            }
                            Object val = eval(test, env);
                            if (!isFalse(val)) {
                                if (clause.size() == 1) return val;
                                Object result = VOID;
                                for (int j = 1; j < clause.size(); j++) {
                                    result = eval(clause.get(j), env);
                                }
                                return result;
                            }
                        }
                        return VOID;
                    }
                    case "set!" -> {
                        if (list.size() != 3) throw errAt(eline, ecol, "set!: bad syntax");
                        Object target = list.get(1);
                        String tname = symName(target);
                        if (tname == null) throw errAt(eline, ecol, "set!: expected symbol");
                        int tline = eline, tcol = ecol;
                        if (target instanceof LocatedSymbol ls) { tline = ls.line(); tcol = ls.col(); }
                        // Find the env that owns this binding
                        Env e = env;
                        while (e != null) {
                            if (e.bindings.containsKey(tname)) {
                                e.bindings.put(tname, eval(list.get(2), env));
                                return VOID;
                            }
                            e = e.parent;
                        }
                        throw errAt(tline, tcol, "set!: unbound variable: " + tname);
                    }
                    case "or" -> {
                        Object result = Boolean.FALSE;
                        for (int i = 1; i < list.size(); i++) {
                            result = eval(list.get(i), env);
                            if (!isFalse(result)) return result;
                        }
                        return result;
                    }
                }
            }

            // Function application
            Object proc = eval(first, env);
            List<Object> args = new ArrayList<>();
            for (int i = 1; i < list.size(); i++) {
                args.add(eval(list.get(i), env));
            }
            try {
                return apply(proc, args);
            } catch (EvalError e) {
                if (eline > 0 && !hasPosition(e.getMessage())) {
                    throw errAt(eline, ecol, e.getMessage());
                }
                throw e;
            }
        }
        throw new EvalError("cannot evaluate: " + expr);
    }

    // Convert parsed data to Scheme values (for quote)
    @SuppressWarnings("unchecked")
    private Object toSchemeValue(Object parsed) {
        if (parsed instanceof List<?> list) {
            Object result = Empty.NIL;
            for (int i = list.size() - 1; i >= 0; i--) {
                result = new Pair(toSchemeValue(list.get(i)), result);
            }
            return result;
        }
        if (parsed instanceof LocatedSymbol ls) return ls.name();
        // Atoms (Long, Boolean, String/symbol, SchemeString) are already fine
        return parsed;
    }

    private boolean isFalse(Object val) {
        return val instanceof Boolean b && !b;
    }

    private Object apply(Object proc, List<Object> args) throws EvalError {
        if (proc instanceof String sym && sym.startsWith("builtin:")) {
            return applyBuiltin(sym.substring(8), args);
        }
        if (proc instanceof Lambda lam) {
            if (args.size() != lam.params().size())
                throw new EvalError("wrong number of arguments: expected " + lam.params().size() + ", got " + args.size());
            Env callEnv = new Env(lam.env());
            for (int i = 0; i < lam.params().size(); i++) {
                callEnv.define(lam.params().get(i), args.get(i));
            }
            Object result = VOID;
            for (Object bodyExpr : lam.body()) {
                result = eval(bodyExpr, callEnv);
            }
            return result;
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
            case "cons" -> {
                checkMinArgs(args, 2, "cons");
                yield new Pair(args.get(0), args.get(1));
            }
            case "car" -> {
                checkMinArgs(args, 1, "car");
                if (!(args.get(0) instanceof Pair p)) throw new EvalError("car: expected pair");
                yield p.car();
            }
            case "cdr" -> {
                checkMinArgs(args, 1, "cdr");
                if (!(args.get(0) instanceof Pair p)) throw new EvalError("cdr: expected pair");
                yield p.cdr();
            }
            case "null?" -> {
                checkMinArgs(args, 1, "null?");
                yield args.get(0) == Empty.NIL;
            }
            case "list" -> {
                Object result = Empty.NIL;
                for (int i = args.size() - 1; i >= 0; i--) {
                    result = new Pair(args.get(i), result);
                }
                yield result;
            }
            case "length" -> {
                checkMinArgs(args, 1, "length");
                Object lst = args.get(0);
                long len = 0;
                while (lst instanceof Pair p) {
                    len++;
                    lst = p.cdr();
                }
                if (lst != Empty.NIL) throw new EvalError("length: not a proper list");
                yield len;
            }
            case "string?" -> {
                checkMinArgs(args, 1, "string?");
                yield args.get(0) instanceof SchemeString;
            }
            case "number?" -> {
                checkMinArgs(args, 1, "number?");
                yield args.get(0) instanceof Long;
            }
            case "boolean?" -> {
                checkMinArgs(args, 1, "boolean?");
                yield args.get(0) instanceof Boolean;
            }
            case "pair?" -> {
                checkMinArgs(args, 1, "pair?");
                yield args.get(0) instanceof Pair;
            }
            case "symbol?" -> {
                checkMinArgs(args, 1, "symbol?");
                yield args.get(0) instanceof String && !((String) args.get(0)).startsWith("builtin:");
            }
            case "append" -> {
                if (args.isEmpty()) yield Empty.NIL;
                Object result = args.get(args.size() - 1);
                for (int i = args.size() - 2; i >= 0; i--) {
                    Object lst = args.get(i);
                    List<Object> elems = new ArrayList<>();
                    while (lst instanceof Pair p) {
                        elems.add(p.car());
                        lst = p.cdr();
                    }
                    for (int j = elems.size() - 1; j >= 0; j--) {
                        result = new Pair(elems.get(j), result);
                    }
                }
                yield result;
            }
            case "display" -> {
                checkMinArgs(args, 1, "display");
                outputBuffer.append(displayToString(args.get(0)));
                yield VOID;
            }
            case "write" -> {
                checkMinArgs(args, 1, "write");
                outputBuffer.append(schemeToString(args.get(0)));
                yield VOID;
            }
            case "newline" -> {
                outputBuffer.append("\n");
                yield VOID;
            }
            case "string-append" -> {
                StringBuilder sb = new StringBuilder();
                for (Object a : args) {
                    if (!(a instanceof SchemeString s)) throw new EvalError("string-append: expected string");
                    sb.append(s.value());
                }
                yield new SchemeString(sb.toString());
            }
            case "string-length" -> {
                checkMinArgs(args, 1, "string-length");
                if (!(args.get(0) instanceof SchemeString s)) throw new EvalError("string-length: expected string");
                yield (long) s.value().length();
            }
            case "substring" -> {
                checkMinArgs(args, 3, "substring");
                if (!(args.get(0) instanceof SchemeString s)) throw new EvalError("substring: expected string");
                long start = asLong(args.get(1), "substring");
                long end = asLong(args.get(2), "substring");
                yield new SchemeString(s.value().substring((int) start, (int) end));
            }
            case "string->number" -> {
                checkMinArgs(args, 1, "string->number");
                if (!(args.get(0) instanceof SchemeString s)) throw new EvalError("string->number: expected string");
                try {
                    yield Long.parseLong(s.value());
                } catch (NumberFormatException e) {
                    yield Boolean.FALSE;
                }
            }
            case "number->string" -> {
                checkMinArgs(args, 1, "number->string");
                yield new SchemeString(String.valueOf(asLong(args.get(0), "number->string")));
            }
            case "symbol->string" -> {
                checkMinArgs(args, 1, "symbol->string");
                if (!(args.get(0) instanceof String s)) throw new EvalError("symbol->string: expected symbol");
                yield new SchemeString(s);
            }
            case "string->symbol" -> {
                checkMinArgs(args, 1, "string->symbol");
                if (!(args.get(0) instanceof SchemeString s)) throw new EvalError("string->symbol: expected string");
                yield s.value();
            }
            case "string-ref" -> {
                checkMinArgs(args, 2, "string-ref");
                if (!(args.get(0) instanceof SchemeString s)) throw new EvalError("string-ref: expected string");
                long idx = asLong(args.get(1), "string-ref");
                yield new SchemeChar(s.value().charAt((int) idx));
            }
            case "char?" -> {
                checkMinArgs(args, 1, "char?");
                yield args.get(0) instanceof SchemeChar;
            }
            case "string-copy" -> {
                checkMinArgs(args, 1, "string-copy");
                if (!(args.get(0) instanceof SchemeString s)) throw new EvalError("string-copy: expected string");
                yield new SchemeString(s.value());
            }
            case "string-set!" -> {
                checkMinArgs(args, 3, "string-set!");
                if (!(args.get(0) instanceof SchemeString s)) throw new EvalError("string-set!: expected string");
                long idx = asLong(args.get(1), "string-set!");
                if (!(args.get(2) instanceof SchemeChar c)) throw new EvalError("string-set!: expected char");
                s.setChar((int) idx, c.value());
                yield VOID;
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
