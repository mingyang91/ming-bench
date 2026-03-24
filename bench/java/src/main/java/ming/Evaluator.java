package ming;

import java.util.ArrayList;
import java.util.List;

public class Evaluator {

    private static final Object VOID = new Object() {
        @Override public String toString() { return "#<void>"; }
    };

    static final Object NIL = new Object() {
        @Override public String toString() { return "()"; }
    };

    private final Environment globalEnv = new Environment();

    // Position tracking for error messages
    private int currentLine = 1;
    private int currentCol = 1;

    // Output buffer for display/write/newline
    private StringBuilder outputBuffer = new StringBuilder();

    private EvalError error(String msg) {
        return new EvalError(currentLine + ":" + currentCol + " " + msg);
    }

    // Token with source position
    private record Token(Object value, int line, int col) {}

    // Expression wrapper with source position
    record Located(Object value, int line, int col) {}

    public String evalStr(String input) throws EvalError {
        List<Token> tokens = tokenize(input);
        int[] pos = {0};
        Object lastResult = null;
        while (pos[0] < tokens.size()) {
            Object expr = parse(tokens, pos);
            lastResult = eval(expr, globalEnv);
        }
        if (lastResult == null) {
            throw new EvalError("1:1 no expression");
        }
        return schemeToString(lastResult);
    }

    public EvalResult evalStrWithOutput(String input) throws EvalError {
        outputBuffer = new StringBuilder();
        List<Token> tokens = tokenize(input);
        int[] pos = {0};
        Object lastResult = null;
        while (pos[0] < tokens.size()) {
            Object expr = parse(tokens, pos);
            lastResult = eval(expr, globalEnv);
        }
        String output = outputBuffer.toString();
        if (lastResult == null) {
            throw new EvalError("1:1 no expression");
        }
        return new EvalResult(schemeToString(lastResult), output);
    }

    // --- Tokenizer ---

    private List<Token> tokenize(String input) throws EvalError {
        List<Token> tokens = new ArrayList<>();
        int i = 0;
        int line = 1;
        int col = 1;
        while (i < input.length()) {
            char c = input.charAt(i);
            if (c == '\n') {
                i++;
                line++;
                col = 1;
            } else if (Character.isWhitespace(c)) {
                i++;
                col++;
            } else if (c == ';') {
                while (i < input.length() && input.charAt(i) != '\n') { i++; col++; }
            } else if (c == '\'') {
                tokens.add(new Token("'", line, col));
                i++;
                col++;
            } else if (c == '(') {
                tokens.add(new Token("(", line, col));
                i++;
                col++;
            } else if (c == ')') {
                tokens.add(new Token(")", line, col));
                i++;
                col++;
            } else if (c == '"') {
                int startLine = line;
                int startCol = col;
                StringBuilder sb = new StringBuilder();
                i++; col++;
                while (i < input.length() && input.charAt(i) != '"') {
                    if (input.charAt(i) == '\\') {
                        i++; col++;
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
                        if (input.charAt(i) == '\n') {
                            sb.append('\n');
                            line++;
                            col = 0;
                        } else {
                            sb.append(input.charAt(i));
                        }
                    }
                    i++; col++;
                }
                if (i < input.length()) { i++; col++; }
                tokens.add(new Token(new SchemeString(sb.toString()), startLine, startCol));
            } else if (c == '#') {
                int startCol = col;
                if (i + 1 < input.length()) {
                    char next = input.charAt(i + 1);
                    if (next == 't') {
                        tokens.add(new Token(Boolean.TRUE, line, startCol));
                        i += 2; col += 2;
                    } else if (next == 'f') {
                        tokens.add(new Token(Boolean.FALSE, line, startCol));
                        i += 2; col += 2;
                    } else if (next == '\\') {
                        // Character literal: #\x, #\space, #\newline, #\tab
                        i += 2; col += 2;
                        if (i >= input.length()) throw new EvalError(line + ":" + startCol + " unexpected end in character literal");
                        // Try to read a named character or single character
                        int nameStart = i;
                        while (i < input.length()) {
                            char ch = input.charAt(i);
                            if (Character.isWhitespace(ch) || ch == '(' || ch == ')' || ch == '"' || ch == ';' || ch == '\'') break;
                            i++; col++;
                        }
                        String charName = input.substring(nameStart, i);
                        char charVal;
                        if (charName.length() == 1) {
                            charVal = charName.charAt(0);
                        } else if (charName.equalsIgnoreCase("space")) {
                            charVal = ' ';
                        } else if (charName.equalsIgnoreCase("newline")) {
                            charVal = '\n';
                        } else if (charName.equalsIgnoreCase("tab")) {
                            charVal = '\t';
                        } else {
                            throw new EvalError(line + ":" + startCol + " unknown character name: " + charName);
                        }
                        tokens.add(new Token(new SchemeChar(charVal), line, startCol));
                    } else {
                        throw new EvalError(line + ":" + col + " unexpected character after #: " + next);
                    }
                } else {
                    throw new EvalError(line + ":" + col + " unexpected end after #");
                }
            } else {
                int startCol = col;
                StringBuilder sb = new StringBuilder();
                while (i < input.length()) {
                    char ch = input.charAt(i);
                    if (Character.isWhitespace(ch) || ch == '(' || ch == ')' || ch == '"' || ch == ';' || ch == '\'') break;
                    sb.append(ch);
                    i++; col++;
                }
                String tok = sb.toString();
                try {
                    tokens.add(new Token(Long.parseLong(tok), line, startCol));
                } catch (NumberFormatException e) {
                    tokens.add(new Token(tok, line, startCol));
                }
            }
        }
        return tokens;
    }

    // --- Parser ---

    private Object parse(List<Token> tokens, int[] pos) throws EvalError {
        if (pos[0] >= tokens.size()) {
            throw new EvalError("1:1 unexpected end of input");
        }
        Token token = tokens.get(pos[0]);
        if (token.value().equals("'")) {
            pos[0]++;
            Object quoted = parse(tokens, pos);
            Object rawQuoted = quoted instanceof Located loc ? loc.value() : quoted;
            List<Object> quoteExpr = new ArrayList<>();
            quoteExpr.add("quote");
            quoteExpr.add(rawQuoted);
            return new Located(quoteExpr, token.line(), token.col());
        }
        if (token.value().equals("(")) {
            pos[0]++;
            List<Object> list = new ArrayList<>();
            while (pos[0] < tokens.size() && !tokens.get(pos[0]).value().equals(")")) {
                list.add(parse(tokens, pos));
            }
            if (pos[0] >= tokens.size()) {
                throw new EvalError(token.line() + ":" + token.col() + " missing closing parenthesis");
            }
            pos[0]++;
            return new Located(list, token.line(), token.col());
        } else if (token.value().equals(")")) {
            throw new EvalError(token.line() + ":" + token.col() + " unexpected )");
        } else {
            pos[0]++;
            return new Located(token.value(), token.line(), token.col());
        }
    }

    // --- Convert parsed list data to cons cells ---

    private Object listToConsCells(Object datum) {
        if (datum instanceof Located loc) {
            return listToConsCells(loc.value());
        }
        if (datum instanceof List<?> list) {
            Object result = NIL;
            for (int i = list.size() - 1; i >= 0; i--) {
                result = new Pair(listToConsCells(list.get(i)), result);
            }
            return result;
        }
        return datum;
    }

    // --- Evaluator ---

    @SuppressWarnings("unchecked")
    private Object eval(Object expr, Environment env) throws EvalError {
        if (expr instanceof Located loc) {
            currentLine = loc.line();
            currentCol = loc.col();
            return eval(loc.value(), env);
        }

        if (expr instanceof Long || expr instanceof Boolean || expr instanceof SchemeString || expr instanceof SchemeChar) {
            return expr;
        }
        if (expr instanceof String symbol) {
            try {
                return env.lookup(symbol);
            } catch (EvalError e) {
                throw error("unbound variable: " + symbol);
            }
        }
        if (expr instanceof List<?> list) {
            if (list.isEmpty()) {
                throw error("empty application");
            }
            // Save call-site position (set by Located wrapper of this list)
            int callLine = currentLine;
            int callCol = currentCol;

            Object head = list.get(0);

            // Unwrap Located from head to check for special forms
            Object rawHead = head instanceof Located loc ? loc.value() : head;

            // Special forms
            if (rawHead instanceof String op) {
                switch (op) {
                    case "define" -> { return evalDefine(list, env); }
                    case "if" -> { return evalIf(list, env); }
                    case "quote" -> {
                        if (list.size() != 2) throw error("quote: expected 1 argument");
                        return listToConsCells(list.get(1));
                    }
                    case "lambda" -> { return evalLambda(list, env); }
                    case "begin" -> { return evalBegin(list, env); }
                    case "cond" -> { return evalCond(list, env); }
                    case "let" -> { return evalLet(list, env); }
                    case "and" -> {
                        Object result = Boolean.TRUE;
                        for (int i = 1; i < list.size(); i++) {
                            result = eval(list.get(i), env);
                            if (Boolean.FALSE.equals(result)) return Boolean.FALSE;
                        }
                        return result;
                    }
                    case "or" -> {
                        Object result = Boolean.FALSE;
                        for (int i = 1; i < list.size(); i++) {
                            result = eval(list.get(i), env);
                            if (!Boolean.FALSE.equals(result)) return result;
                        }
                        return result;
                    }
                    case "not" -> {
                        checkArgs(list, 1, "not");
                        Object val = eval(list.get(1), env);
                        return Boolean.FALSE.equals(val) ? Boolean.TRUE : Boolean.FALSE;
                    }
                }
            }

            // Function application
            Object proc = eval(head, env);
            List<Object> args = new ArrayList<>();
            for (int i = 1; i < list.size(); i++) {
                args.add(eval(list.get(i), env));
            }
            // Restore call-site position for apply errors
            currentLine = callLine;
            currentCol = callCol;
            return apply(proc, args);
        }
        throw error("cannot evaluate: " + expr);
    }

    @SuppressWarnings("unchecked")
    private Object evalDefine(List<?> list, Environment env) throws EvalError {
        if (list.size() < 3) throw error("define: bad syntax");
        Object target = list.get(1);
        // Unwrap Located
        if (target instanceof Located loc) target = loc.value();
        if (target instanceof String name) {
            Object val = eval(list.get(2), env);
            env.define(name, val);
            return VOID;
        }
        if (target instanceof List<?> sig) {
            // Unwrap Located elements in signature
            Object first = sig.isEmpty() ? null : sig.get(0);
            if (first instanceof Located loc) first = loc.value();
            if (sig.isEmpty() || !(first instanceof String name)) {
                throw error("define: bad syntax");
            }
            List<String> params = new ArrayList<>();
            for (int i = 1; i < sig.size(); i++) {
                Object p = sig.get(i);
                if (p instanceof Located loc) p = loc.value();
                if (!(p instanceof String s)) throw error("define: bad parameter");
                params.add(s);
            }
            List<Object> body = new ArrayList<>();
            for (int i = 2; i < list.size(); i++) {
                body.add(list.get(i));
            }
            Lambda lambda = new Lambda(params, body, env);
            env.define(name, lambda);
            return VOID;
        }
        throw error("define: bad syntax");
    }

    private Object evalIf(List<?> list, Environment env) throws EvalError {
        if (list.size() < 3 || list.size() > 4) throw error("if: bad syntax");
        Object cond = eval(list.get(1), env);
        if (!Boolean.FALSE.equals(cond)) {
            return eval(list.get(2), env);
        } else if (list.size() == 4) {
            return eval(list.get(3), env);
        }
        return VOID;
    }

    private Object evalLambda(List<?> list, Environment env) throws EvalError {
        if (list.size() < 3) throw error("lambda: bad syntax");
        Object paramList = list.get(1);
        if (paramList instanceof Located loc) paramList = loc.value();
        if (!(paramList instanceof List<?> plist)) throw error("lambda: bad parameters");
        List<String> params = new ArrayList<>();
        for (Object p : plist) {
            if (p instanceof Located loc) p = loc.value();
            if (!(p instanceof String s)) throw error("lambda: bad parameter");
            params.add(s);
        }
        List<Object> body = new ArrayList<>();
        for (int i = 2; i < list.size(); i++) {
            body.add(list.get(i));
        }
        return new Lambda(params, body, env);
    }

    private Object evalBegin(List<?> list, Environment env) throws EvalError {
        Object result = VOID;
        for (int i = 1; i < list.size(); i++) {
            result = eval(list.get(i), env);
        }
        return result;
    }

    private Object evalCond(List<?> list, Environment env) throws EvalError {
        for (int i = 1; i < list.size(); i++) {
            Object clause = list.get(i);
            if (clause instanceof Located loc) clause = loc.value();
            if (!(clause instanceof List<?> cl) || cl.isEmpty()) {
                throw error("cond: bad clause");
            }
            Object test = cl.get(0);
            Object rawTest = test instanceof Located loc ? loc.value() : test;
            if (rawTest instanceof String s && s.equals("else")) {
                Object result = VOID;
                for (int j = 1; j < cl.size(); j++) {
                    result = eval(cl.get(j), env);
                }
                return result;
            }
            Object testVal = eval(test, env);
            if (!Boolean.FALSE.equals(testVal)) {
                Object result = testVal;
                for (int j = 1; j < cl.size(); j++) {
                    result = eval(cl.get(j), env);
                }
                return result;
            }
        }
        return VOID;
    }

    @SuppressWarnings("unchecked")
    private Object evalLet(List<?> list, Environment env) throws EvalError {
        if (list.size() < 3) throw error("let: bad syntax");
        Object second = list.get(1);
        if (second instanceof Located loc) second = loc.value();

        // Named let: (let name ((var init) ...) body...)
        if (second instanceof String name) {
            if (list.size() < 4) throw error("let: bad syntax");
            Object bindingsList = list.get(2);
            if (bindingsList instanceof Located loc) bindingsList = loc.value();
            if (!(bindingsList instanceof List<?> bindings)) throw error("let: bad bindings");
            List<String> params = new ArrayList<>();
            List<Object> inits = new ArrayList<>();
            for (Object b : bindings) {
                if (b instanceof Located loc) b = loc.value();
                if (!(b instanceof List<?> binding) || binding.size() != 2)
                    throw error("let: bad binding");
                Object varObj = binding.get(0);
                if (varObj instanceof Located loc) varObj = loc.value();
                if (!(varObj instanceof String varName))
                    throw error("let: bad binding variable");
                params.add(varName);
                inits.add(eval(binding.get(1), env));
            }
            List<Object> body = new ArrayList<>();
            for (int i = 3; i < list.size(); i++) {
                body.add(list.get(i));
            }
            Environment letEnv = new Environment(env);
            Lambda lambda = new Lambda(params, body, letEnv);
            letEnv.define(name, lambda);
            Environment callEnv = new Environment(letEnv);
            for (int i = 0; i < params.size(); i++) {
                callEnv.define(params.get(i), inits.get(i));
            }
            Object result = VOID;
            for (Object bodyExpr : body) {
                result = eval(bodyExpr, callEnv);
            }
            return result;
        }

        // Regular let: (let ((var init) ...) body...)
        if (!(second instanceof List<?> bindings)) throw error("let: bad bindings");
        Environment letEnv = new Environment(env);
        for (Object b : bindings) {
            if (b instanceof Located loc) b = loc.value();
            if (!(b instanceof List<?> binding) || binding.size() != 2)
                throw error("let: bad binding");
            Object varObj = binding.get(0);
            if (varObj instanceof Located loc) varObj = loc.value();
            if (!(varObj instanceof String varName))
                throw error("let: bad binding variable");
            Object val = eval(binding.get(1), env);
            letEnv.define(varName, val);
        }
        Object result = VOID;
        for (int i = 2; i < list.size(); i++) {
            result = eval(list.get(i), letEnv);
        }
        return result;
    }

    private Object apply(Object proc, List<Object> args) throws EvalError {
        if (proc instanceof Lambda lambda) {
            if (args.size() != lambda.params.size()) {
                throw error("wrong number of arguments: expected " + lambda.params.size() + ", got " + args.size());
            }
            Environment callEnv = new Environment(lambda.closure);
            for (int i = 0; i < lambda.params.size(); i++) {
                callEnv.define(lambda.params.get(i), args.get(i));
            }
            Object result = VOID;
            for (Object bodyExpr : lambda.body) {
                result = eval(bodyExpr, callEnv);
            }
            return result;
        }
        if (proc instanceof BuiltinProc bp) {
            return bp.apply(args);
        }
        throw error("not a procedure: " + schemeToString(proc));
    }

    // Builtins are registered as BuiltinProc in the global env
    @FunctionalInterface
    interface BuiltinProc {
        Object apply(List<Object> args) throws EvalError;
    }

    {
        // Register arithmetic and comparison builtins
        globalEnv.define("+", (BuiltinProc) args -> arith(args, "+"));
        globalEnv.define("-", (BuiltinProc) args -> arith(args, "-"));
        globalEnv.define("*", (BuiltinProc) args -> arith(args, "*"));
        globalEnv.define("/", (BuiltinProc) args -> arith(args, "/"));
        globalEnv.define("<", (BuiltinProc) args -> cmp(args, "<"));
        globalEnv.define(">", (BuiltinProc) args -> cmp(args, ">"));
        globalEnv.define("=", (BuiltinProc) args -> cmp(args, "="));
        globalEnv.define("<=", (BuiltinProc) args -> cmp(args, "<="));
        globalEnv.define(">=", (BuiltinProc) args -> cmp(args, ">="));

        // List operations
        globalEnv.define("cons", (BuiltinProc) args -> {
            if (args.size() != 2) throw error("cons: expected 2 arguments");
            return new Pair(args.get(0), args.get(1));
        });
        globalEnv.define("car", (BuiltinProc) args -> {
            if (args.size() != 1) throw error("car: expected 1 argument");
            if (!(args.get(0) instanceof Pair p)) throw error("car: not a pair");
            return p.car;
        });
        globalEnv.define("cdr", (BuiltinProc) args -> {
            if (args.size() != 1) throw error("cdr: expected 1 argument");
            if (!(args.get(0) instanceof Pair p)) throw error("cdr: not a pair");
            return p.cdr;
        });
        globalEnv.define("null?", (BuiltinProc) args -> {
            if (args.size() != 1) throw error("null?: expected 1 argument");
            return args.get(0) == NIL;
        });
        globalEnv.define("list", (BuiltinProc) args -> {
            Object result = NIL;
            for (int i = args.size() - 1; i >= 0; i--) {
                result = new Pair(args.get(i), result);
            }
            return result;
        });
        globalEnv.define("length", (BuiltinProc) args -> {
            if (args.size() != 1) throw error("length: expected 1 argument");
            Object obj = args.get(0);
            long count = 0;
            while (obj instanceof Pair p) {
                count++;
                obj = p.cdr;
            }
            if (obj != NIL) throw error("length: not a proper list");
            return count;
        });
        globalEnv.define("append", (BuiltinProc) args -> {
            if (args.isEmpty()) return NIL;
            if (args.size() == 1) return args.get(0);
            Object result = args.get(args.size() - 1);
            for (int i = args.size() - 2; i >= 0; i--) {
                Object lst = args.get(i);
                List<Object> elems = new ArrayList<>();
                Object cur = lst;
                while (cur instanceof Pair p) {
                    elems.add(p.car);
                    cur = p.cdr;
                }
                for (int j = elems.size() - 1; j >= 0; j--) {
                    result = new Pair(elems.get(j), result);
                }
            }
            return result;
        });

        // Type predicates
        globalEnv.define("number?", (BuiltinProc) args -> {
            if (args.size() != 1) throw error("number?: expected 1 argument");
            return args.get(0) instanceof Long;
        });
        globalEnv.define("string?", (BuiltinProc) args -> {
            if (args.size() != 1) throw error("string?: expected 1 argument");
            return args.get(0) instanceof SchemeString;
        });
        globalEnv.define("boolean?", (BuiltinProc) args -> {
            if (args.size() != 1) throw error("boolean?: expected 1 argument");
            return args.get(0) instanceof Boolean;
        });
        globalEnv.define("pair?", (BuiltinProc) args -> {
            if (args.size() != 1) throw error("pair?: expected 1 argument");
            return args.get(0) instanceof Pair;
        });
        globalEnv.define("symbol?", (BuiltinProc) args -> {
            if (args.size() != 1) throw error("symbol?: expected 1 argument");
            return args.get(0) instanceof String;
        });

        // Output
        globalEnv.define("display", (BuiltinProc) args -> {
            if (args.size() != 1) throw error("display: expected 1 argument");
            outputBuffer.append(displayString(args.get(0)));
            return VOID;
        });
        globalEnv.define("write", (BuiltinProc) args -> {
            if (args.size() != 1) throw error("write: expected 1 argument");
            outputBuffer.append(schemeToString(args.get(0)));
            return VOID;
        });
        globalEnv.define("newline", (BuiltinProc) args -> {
            if (args.size() != 0) throw error("newline: expected 0 arguments");
            outputBuffer.append('\n');
            return VOID;
        });

        // String operations
        globalEnv.define("string-append", (BuiltinProc) args -> {
            StringBuilder sb = new StringBuilder();
            for (Object a : args) {
                if (!(a instanceof SchemeString s)) throw error("string-append: not a string");
                sb.append(s.value());
            }
            return new SchemeString(sb.toString());
        });
        globalEnv.define("string-length", (BuiltinProc) args -> {
            if (args.size() != 1) throw error("string-length: expected 1 argument");
            if (!(args.get(0) instanceof SchemeString s)) throw error("string-length: not a string");
            return (long) s.value().length();
        });
        globalEnv.define("substring", (BuiltinProc) args -> {
            if (args.size() != 3) throw error("substring: expected 3 arguments");
            if (!(args.get(0) instanceof SchemeString s)) throw error("substring: not a string");
            if (!(args.get(1) instanceof Long start)) throw error("substring: not a number");
            if (!(args.get(2) instanceof Long end)) throw error("substring: not a number");
            return new SchemeString(s.value().substring(start.intValue(), end.intValue()));
        });
        globalEnv.define("string->number", (BuiltinProc) args -> {
            if (args.size() != 1) throw error("string->number: expected 1 argument");
            if (!(args.get(0) instanceof SchemeString s)) throw error("string->number: not a string");
            try {
                return Long.parseLong(s.value());
            } catch (NumberFormatException e) {
                return Boolean.FALSE;
            }
        });
        globalEnv.define("number->string", (BuiltinProc) args -> {
            if (args.size() != 1) throw error("number->string: expected 1 argument");
            if (!(args.get(0) instanceof Long n)) throw error("number->string: not a number");
            return new SchemeString(n.toString());
        });

        // Symbol/String conversion
        globalEnv.define("symbol->string", (BuiltinProc) args -> {
            if (args.size() != 1) throw error("symbol->string: expected 1 argument");
            if (!(args.get(0) instanceof String s)) throw error("symbol->string: not a symbol");
            return new SchemeString(s);
        });
        globalEnv.define("string->symbol", (BuiltinProc) args -> {
            if (args.size() != 1) throw error("string->symbol: expected 1 argument");
            if (!(args.get(0) instanceof SchemeString s)) throw error("string->symbol: not a string");
            return s.value();
        });

        globalEnv.define("string-copy", (BuiltinProc) args -> {
            if (args.size() != 1) throw error("string-copy: expected 1 argument");
            if (!(args.get(0) instanceof SchemeString s)) throw error("string-copy: not a string");
            return new SchemeString(s.value());
        });
        globalEnv.define("string-set!", (BuiltinProc) args -> {
            if (args.size() != 3) throw error("string-set!: expected 3 arguments");
            if (!(args.get(0) instanceof SchemeString s)) throw error("string-set!: not a string");
            if (!(args.get(1) instanceof Long idx)) throw error("string-set!: not a number");
            if (!(args.get(2) instanceof SchemeChar ch)) throw error("string-set!: not a character");
            s.setChar(idx.intValue(), ch.value());
            return VOID;
        });

        // Character operations
        globalEnv.define("string-ref", (BuiltinProc) args -> {
            if (args.size() != 2) throw error("string-ref: expected 2 arguments");
            if (!(args.get(0) instanceof SchemeString s)) throw error("string-ref: not a string");
            if (!(args.get(1) instanceof Long idx)) throw error("string-ref: not a number");
            return new SchemeChar(s.value().charAt(idx.intValue()));
        });
        globalEnv.define("char?", (BuiltinProc) args -> {
            if (args.size() != 1) throw error("char?: expected 1 argument");
            return args.get(0) instanceof SchemeChar;
        });
    }

    // Arithmetic on already-evaluated args
    private Object arith(List<Object> args, String op) throws EvalError {
        if (args.isEmpty()) {
            if (op.equals("+")) return 0L;
            if (op.equals("*")) return 1L;
            throw error(op + ": need at least 1 argument");
        }
        if (op.equals("-") && args.size() == 1) {
            Object val = args.get(0);
            if (!(val instanceof Long)) throw error("-: not a number");
            return -((Long) val);
        }
        Object first = args.get(0);
        if (!(first instanceof Long)) throw error(op + ": not a number");
        long result = (Long) first;
        for (int i = 1; i < args.size(); i++) {
            Object val = args.get(i);
            if (!(val instanceof Long)) throw error(op + ": not a number");
            long v = (Long) val;
            switch (op) {
                case "+" -> result += v;
                case "-" -> result -= v;
                case "*" -> result *= v;
                case "/" -> {
                    if (v == 0) throw error("division by zero");
                    result /= v;
                }
            }
        }
        return result;
    }

    private Object cmp(List<Object> args, String op) throws EvalError {
        if (args.size() != 2) throw error(op + ": expected 2 arguments");
        Object a = args.get(0);
        Object b = args.get(1);
        if (!(a instanceof Long la) || !(b instanceof Long lb)) {
            throw error(op + ": not a number");
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
            throw error(name + ": expected " + expected + " arguments, got " + (list.size() - 1));
        }
    }

    // --- Output formatting ---

    @SuppressWarnings("unchecked")
    static String schemeToString(Object val) {
        if (val instanceof Long) return val.toString();
        if (val instanceof Boolean b) return b ? "#t" : "#f";
        if (val instanceof SchemeString s) return "\"" + s.value() + "\"";
        if (val instanceof SchemeChar ch) return "#\\" + ch.value();
        if (val instanceof Lambda) return "#<procedure>";
        if (val == NIL) return "()";
        if (val instanceof Pair) {
            StringBuilder sb = new StringBuilder("(");
            Object cur = val;
            boolean first = true;
            while (cur instanceof Pair p) {
                if (!first) sb.append(" ");
                first = false;
                sb.append(schemeToString(p.car));
                cur = p.cdr;
            }
            if (cur != NIL) {
                sb.append(" . ");
                sb.append(schemeToString(cur));
            }
            sb.append(")");
            return sb.toString();
        }
        if (val instanceof List<?> list) {
            StringBuilder sb = new StringBuilder("(");
            for (int i = 0; i < list.size(); i++) {
                if (i > 0) sb.append(" ");
                sb.append(schemeToString(list.get(i)));
            }
            sb.append(")");
            return sb.toString();
        }
        if (val instanceof String s) return s; // symbol
        return val.toString();
    }

    static String displayString(Object val) {
        if (val instanceof SchemeString s) return s.value();
        if (val instanceof SchemeChar ch) return String.valueOf(ch.value());
        if (val instanceof Pair) {
            StringBuilder sb = new StringBuilder("(");
            Object cur = val;
            boolean first = true;
            while (cur instanceof Pair p) {
                if (!first) sb.append(" ");
                first = false;
                sb.append(displayString(p.car));
                cur = p.cdr;
            }
            if (cur != NIL) {
                sb.append(" . ");
                sb.append(displayString(cur));
            }
            sb.append(")");
            return sb.toString();
        }
        return schemeToString(val);
    }
}
