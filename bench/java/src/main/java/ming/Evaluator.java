package ming;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

public class Evaluator {

    // --- Sentinel for empty list ---
    static final Object NIL = new Object() {
        @Override public String toString() { return "()"; }
    };

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

        void set(String name, Object val) throws EvalError {
            if (bindings.containsKey(name)) { bindings.put(name, val); return; }
            if (parent != null) { parent.set(name, val); return; }
            throw new EvalError("set!: unbound variable: " + name);
        }
    }

    // --- Lambda (closure) ---

    private static class Lambda {
        final List<String> params;
        final String restParam; // null if no rest arg
        final List<Object> body; // implicit begin
        Env closureEnv;

        Lambda(List<String> params, String restParam, List<Object> body, Env closureEnv) {
            this.params = params;
            this.restParam = restParam;
            this.body = body;
            this.closureEnv = closureEnv;
        }
    }

    // --- Syntax Rules Macro ---

    private static class SyntaxRulesMacro {
        final List<String> literals;
        final List<Object[]> clauses; // each: [pattern (SchemeList), template]
        final Env defEnv;

        SyntaxRulesMacro(List<String> literals, List<Object[]> clauses, Env defEnv) {
            this.literals = literals;
            this.clauses = clauses;
            this.defEnv = defEnv;
        }
    }

    private int gensymCounter = 0;
    private String gensym(String base) {
        return base + "_$" + (gensymCounter++);
    }

    private static final java.util.Set<String> SPECIAL_FORMS = java.util.Set.of(
        "define", "if", "quote", "lambda", "and", "or", "set!", "begin", "let", "cond",
        "define-syntax", "syntax-rules"
    );

    // --- Pair (cons cell) ---

    static class SchemePair {
        Object car;
        Object cdr;

        SchemePair(Object car, Object cdr) {
            this.car = car;
            this.cdr = cdr;
        }
    }

    // --- Output buffer ---
    private StringBuilder outputBuffer = new StringBuilder();

    // --- Public API ---

    public String evalStr(String input) throws EvalError {
        List<Object> exprs = parse(input);
        Env env = makeGlobalEnv();
        Object result = null;
        for (Object expr : exprs) {
            result = eval(expr, env);
        }
        return schemeToString(result);
    }

    public EvalResult evalStrWithOutput(String input) throws EvalError {
        outputBuffer = new StringBuilder();
        List<Object> exprs = parse(input);
        Env env = makeGlobalEnv();
        Object result = null;
        for (Object expr : exprs) {
            result = eval(expr, env);
        }
        return new EvalResult(schemeToString(result), outputBuffer.toString());
    }

    private Env makeGlobalEnv() {
        Env env = new Env(null);
        registerBuiltins(env);
        return env;
    }

    // --- Parser ---

    private int pos;
    private String src;

    private int[] lineCol(int offset) {
        int line = 1, col = 1;
        for (int i = 0; i < offset && i < src.length(); i++) {
            if (src.charAt(i) == '\n') { line++; col = 1; }
            else col++;
        }
        return new int[]{line, col};
    }

    private List<Object> parse(String input) throws EvalError {
        this.src = input;
        this.pos = 0;
        List<Object> exprs = new ArrayList<>();
        while (true) {
            skipWhitespace();
            if (pos >= src.length()) break;
            exprs.add(readExpr());
        }
        return exprs;
    }

    private void skipWhitespace() {
        while (pos < src.length()) {
            char c = src.charAt(pos);
            if (c == ';') {
                while (pos < src.length() && src.charAt(pos) != '\n') pos++;
            } else if (Character.isWhitespace(c)) {
                pos++;
            } else {
                break;
            }
        }
    }

    private Object readExpr() throws EvalError {
        skipWhitespace();
        if (pos >= src.length()) throw new EvalError("unexpected end of input");
        char c = src.charAt(pos);
        if (c == '\'') {
            int[] lc = lineCol(pos);
            pos++;
            Object datum = readExpr();
            List<Object> quoted = new ArrayList<>();
            quoted.add(new SchemeSymbol("quote", lc[0], lc[1]));
            quoted.add(datum);
            return new SchemeList(quoted, lc[0], lc[1]);
        }
        if (c == '(') {
            return readList();
        } else if (c == '"') {
            return readString();
        } else if (c == '#') {
            return readHash();
        } else {
            return readAtom();
        }
    }

    private SchemeList readList() throws EvalError {
        int[] lc = lineCol(pos);
        pos++; // skip '('
        List<Object> elems = new ArrayList<>();
        while (true) {
            skipWhitespace();
            if (pos >= src.length()) throw new EvalError("unexpected end of input");
            if (src.charAt(pos) == ')') {
                pos++;
                return new SchemeList(elems, lc[0], lc[1]);
            }
            elems.add(readExpr());
        }
    }

    private SchemeString readString() throws EvalError {
        pos++; // skip opening "
        StringBuilder sb = new StringBuilder();
        while (pos < src.length()) {
            char c = src.charAt(pos);
            if (c == '\\') {
                pos++;
                if (pos >= src.length()) throw new EvalError("unexpected end of string");
                char esc = src.charAt(pos);
                switch (esc) {
                    case 'n' -> sb.append('\n');
                    case 't' -> sb.append('\t');
                    case '\\' -> sb.append('\\');
                    case '"' -> sb.append('"');
                    default -> { sb.append('\\'); sb.append(esc); }
                }
                pos++;
            } else if (c == '"') {
                pos++;
                return new SchemeString(sb.toString());
            } else {
                sb.append(c);
                pos++;
            }
        }
        throw new EvalError("unterminated string");
    }

    private Object readHash() throws EvalError {
        pos++; // skip '#'
        if (pos >= src.length()) throw new EvalError("unexpected end of input after #");
        char c = src.charAt(pos);
        if (c == 't') {
            pos++;
            return Boolean.TRUE;
        } else if (c == 'f') {
            pos++;
            return Boolean.FALSE;
        } else if (c == '\\') {
            pos++; // skip backslash
            if (pos >= src.length()) throw new EvalError("unexpected end of character literal");
            // Check for named characters
            int start = pos;
            while (pos < src.length() && !Character.isWhitespace(src.charAt(pos)) && src.charAt(pos) != ')' && src.charAt(pos) != '(') {
                pos++;
            }
            String name = src.substring(start, pos);
            if (name.length() == 1) return new SchemeChar(name.charAt(0));
            return switch (name) {
                case "space" -> new SchemeChar(' ');
                case "newline" -> new SchemeChar('\n');
                case "tab" -> new SchemeChar('\t');
                default -> throw new EvalError("unknown character name: " + name);
            };
        }
        throw new EvalError("unknown hash literal: #" + c);
    }

    private Object readAtom() throws EvalError {
        int[] lc = lineCol(pos);
        int start = pos;
        while (pos < src.length()) {
            char c = src.charAt(pos);
            if (Character.isWhitespace(c) || c == '(' || c == ')' || c == '"' || c == ';') break;
            pos++;
        }
        String token = src.substring(start, pos);
        if (token.isEmpty()) throw new EvalError("empty token");
        try {
            return Long.parseLong(token);
        } catch (NumberFormatException e) {
            return new SchemeSymbol(token, lc[0], lc[1]);
        }
    }

    // --- Quote: convert parsed SchemeList to runtime cons pairs ---

    private Object quoteDatum(Object datum) {
        if (datum instanceof SchemeList list) {
            Object result = NIL;
            for (int i = list.elems.size() - 1; i >= 0; i--) {
                result = new SchemePair(quoteDatum(list.elems.get(i)), result);
            }
            return result;
        }
        return datum;
    }

    // --- Eval ---

    private Object eval(Object expr, Env env) throws EvalError {
        if (expr instanceof Long || expr instanceof Boolean || expr instanceof SchemeString || expr instanceof SchemeChar) {
            return expr;
        }
        if (expr instanceof SchemeSymbol sym) {
            try {
                return env.lookup(sym.name);
            } catch (EvalError e) {
                throw addPosition(e, sym.line, sym.col);
            }
        }
        if (expr instanceof SchemeList list) {
            if (list.elems.isEmpty()) {
                throw new EvalError("empty application");
            }
            Object first = list.elems.get(0);
            if (first instanceof SchemeSymbol sym) {
                try {
                    switch (sym.name) {
                        case "define" -> { return evalDefine(list.elems, env); }
                        case "if" -> { return evalIf(list.elems, env); }
                        case "quote" -> {
                            if (list.elems.size() != 2) throw new EvalError("quote: expected 1 argument");
                            return quoteDatum(list.elems.get(1));
                        }
                        case "lambda" -> { return evalLambda(list.elems, env); }
                        case "and" -> { return evalAnd(list.elems, env); }
                        case "or" -> { return evalOr(list.elems, env); }
                        case "set!" -> {
                            if (list.elems.size() != 3) throw new EvalError("set!: bad syntax");
                            String varName = ((SchemeSymbol) list.elems.get(1)).name;
                            Object val = eval(list.elems.get(2), env);
                            env.set(varName, val);
                            return null; // void
                        }
                        case "begin" -> { return evalBegin(list.elems, env); }
                        case "let" -> { return evalLet(list.elems, env); }
                        case "cond" -> { return evalCond(list.elems, env); }
                        case "define-syntax" -> { return evalDefineSyntax(list.elems, env); }
                    }
                } catch (EvalError e) {
                    throw addPosition(e, list.line, list.col);
                }
                // Check for macro application
                try {
                    Object headVal = env.lookup(sym.name);
                    if (headVal instanceof SyntaxRulesMacro macro) {
                        return eval(expandMacro(macro, list, env), env);
                    }
                } catch (EvalError ignored) {}
            }
            // Function call
            try {
                Object proc = eval(first, env);
                List<Object> args = new ArrayList<>();
                for (int i = 1; i < list.elems.size(); i++) {
                    args.add(eval(list.elems.get(i), env));
                }
                return apply(proc, args);
            } catch (EvalError e) {
                throw addPosition(e, list.line, list.col);
            }
        }
        throw new EvalError("cannot eval: " + expr);
    }

    private Object evalDefine(List<Object> elems, Env env) throws EvalError {
        if (elems.size() < 3) throw new EvalError("define: bad syntax");
        Object target = elems.get(1);
        if (target instanceof SchemeSymbol sym) {
            Object val = eval(elems.get(2), env);
            env.define(sym.name, val);
            return null; // void
        }
        if (target instanceof SchemeList nameAndParams) {
            if (nameAndParams.elems.isEmpty()) throw new EvalError("define: bad syntax");
            String fname = ((SchemeSymbol) nameAndParams.elems.get(0)).name;
            List<String> params = new ArrayList<>();
            String restParam = null;
            for (int i = 1; i < nameAndParams.elems.size(); i++) {
                Object p = nameAndParams.elems.get(i);
                if (p instanceof SchemeSymbol s && s.name.equals(".")) {
                    if (i != nameAndParams.elems.size() - 2) throw new EvalError("define: bad dotted syntax");
                    restParam = ((SchemeSymbol) nameAndParams.elems.get(i + 1)).name;
                    break;
                }
                params.add(((SchemeSymbol) p).name);
            }
            List<Object> body = elems.subList(2, elems.size());
            Lambda lambda = new Lambda(params, restParam, body, env);
            env.define(fname, lambda);
            return null; // void
        }
        throw new EvalError("define: bad syntax");
    }

    private Object evalIf(List<Object> elems, Env env) throws EvalError {
        if (elems.size() < 3 || elems.size() > 4) throw new EvalError("if: bad syntax");
        Object cond = eval(elems.get(1), env);
        if (isTruthy(cond)) {
            return eval(elems.get(2), env);
        } else {
            if (elems.size() == 4) {
                return eval(elems.get(3), env);
            }
            return null; // void
        }
    }

    private Lambda evalLambda(List<Object> elems, Env env) throws EvalError {
        if (elems.size() < 3) throw new EvalError("lambda: bad syntax");
        Object paramSpec = elems.get(1);
        List<String> params = new ArrayList<>();
        String restParam = null;
        if (paramSpec instanceof SchemeSymbol sym) {
            // (lambda args body...) — single rest param
            restParam = sym.name;
        } else if (paramSpec instanceof SchemeList paramList) {
            // Check for dotted pair: (x y . rest)
            List<Object> pelems = paramList.elems;
            for (int i = 0; i < pelems.size(); i++) {
                Object p = pelems.get(i);
                if (p instanceof SchemeSymbol s && s.name.equals(".")) {
                    if (i != pelems.size() - 2) throw new EvalError("lambda: bad dotted syntax");
                    restParam = ((SchemeSymbol) pelems.get(i + 1)).name;
                    break;
                }
                params.add(((SchemeSymbol) p).name);
            }
        } else {
            throw new EvalError("lambda: bad syntax");
        }
        List<Object> body = elems.subList(2, elems.size());
        return new Lambda(params, restParam, body, env);
    }

    private Object evalAnd(List<Object> elems, Env env) throws EvalError {
        Object result = Boolean.TRUE;
        for (int i = 1; i < elems.size(); i++) {
            result = eval(elems.get(i), env);
            if (!isTruthy(result)) return result;
        }
        return result;
    }

    private Object evalOr(List<Object> elems, Env env) throws EvalError {
        Object result = Boolean.FALSE;
        for (int i = 1; i < elems.size(); i++) {
            result = eval(elems.get(i), env);
            if (isTruthy(result)) return result;
        }
        return result;
    }

    private Object evalBegin(List<Object> elems, Env env) throws EvalError {
        Object result = null;
        for (int i = 1; i < elems.size(); i++) {
            result = eval(elems.get(i), env);
        }
        return result;
    }

    private Object evalLet(List<Object> elems, Env env) throws EvalError {
        // (let ((x 1) (y 2)) body...) or named let: (let loop ((x 1)) body...)
        if (elems.size() < 3) throw new EvalError("let: bad syntax");
        int bindingsIdx = 1;
        String name = null;
        if (elems.get(1) instanceof SchemeSymbol sym) {
            // Named let
            name = sym.name;
            bindingsIdx = 2;
            if (elems.size() < 4) throw new EvalError("let: bad syntax");
        }
        SchemeList bindingsList = (SchemeList) elems.get(bindingsIdx);
        List<String> paramNames = new ArrayList<>();
        List<Object> initExprs = new ArrayList<>();
        for (Object b : bindingsList.elems) {
            SchemeList binding = (SchemeList) b;
            paramNames.add(((SchemeSymbol) binding.elems.get(0)).name);
            initExprs.add(binding.elems.get(1));
        }

        List<Object> body = elems.subList(bindingsIdx + 1, elems.size());

        if (name != null) {
            // Named let: create a lambda and call it
            Lambda loopLam = new Lambda(paramNames, null, body, env);
            Env letEnv = new Env(env);
            letEnv.define(name, loopLam);
            loopLam.closureEnv = letEnv; // self-reference
            List<Object> initVals = new ArrayList<>();
            for (Object e : initExprs) initVals.add(eval(e, env));
            return apply(loopLam, initVals);
        } else {
            Env letEnv = new Env(env);
            for (int i = 0; i < paramNames.size(); i++) {
                letEnv.define(paramNames.get(i), eval(initExprs.get(i), env));
            }
            return evalBody(body, letEnv);
        }
    }

    private Object evalCond(List<Object> elems, Env env) throws EvalError {
        for (int i = 1; i < elems.size(); i++) {
            SchemeList clause = (SchemeList) elems.get(i);
            if (clause.elems.isEmpty()) throw new EvalError("cond: empty clause");
            Object test = clause.elems.get(0);
            if (test instanceof SchemeSymbol sym && sym.name.equals("else")) {
                Object result = null;
                for (int j = 1; j < clause.elems.size(); j++) {
                    result = eval(clause.elems.get(j), env);
                }
                return result;
            }
            Object testVal = eval(test, env);
            if (isTruthy(testVal)) {
                if (clause.elems.size() == 1) return testVal;
                Object result = null;
                for (int j = 1; j < clause.elems.size(); j++) {
                    result = eval(clause.elems.get(j), env);
                }
                return result;
            }
        }
        return null; // void - no clause matched
    }

    private Object evalBody(List<Object> body, Env env) throws EvalError {
        Object result = null;
        for (Object expr : body) {
            result = eval(expr, env);
        }
        return result;
    }

    private EvalError addPosition(EvalError e, int line, int col) {
        String msg = e.getMessage();
        if (msg != null && msg.matches(".*\\d+:\\d+.*")) return e;
        return new EvalError(line + ":" + col + ": " + msg);
    }

    private boolean isTruthy(Object val) {
        return !(val instanceof Boolean b && !b);
    }

    private Object apply(Object proc, List<Object> args) throws EvalError {
        if (proc instanceof Lambda lam) {
            if (lam.restParam != null) {
                if (args.size() < lam.params.size()) {
                    throw new EvalError("lambda: expected at least " + lam.params.size() + " arguments, got " + args.size());
                }
            } else {
                if (args.size() != lam.params.size()) {
                    throw new EvalError("lambda: expected " + lam.params.size() + " arguments, got " + args.size());
                }
            }
            Env callEnv = new Env(lam.closureEnv);
            for (int i = 0; i < lam.params.size(); i++) {
                callEnv.define(lam.params.get(i), args.get(i));
            }
            if (lam.restParam != null) {
                // Build rest list from remaining args
                Object rest = NIL;
                for (int i = args.size() - 1; i >= lam.params.size(); i--) {
                    rest = new SchemePair(args.get(i), rest);
                }
                callEnv.define(lam.restParam, rest);
            }
            return evalBody(lam.body, callEnv);
        }
        if (proc instanceof BuiltinProc bp) {
            return bp.apply(args);
        }
        throw new EvalError("not a procedure: " + schemeToString(proc));
    }

    // --- Builtins ---

    @FunctionalInterface
    private interface BuiltinFn {
        Object apply(List<Object> args) throws EvalError;
    }

    private record BuiltinProc(String name, BuiltinFn fn) {
        Object apply(List<Object> args) throws EvalError {
            return fn.apply(args);
        }
    }

    private void registerBuiltins(Env env) {
        env.define("+", new BuiltinProc("+", args -> {
            long sum = 0;
            for (Object a : args) sum += requireLong(a, "+");
            return sum;
        }));
        env.define("-", new BuiltinProc("-", args -> {
            if (args.isEmpty()) throw new EvalError("-: need at least 1 argument");
            if (args.size() == 1) return -requireLong(args.get(0), "-");
            long result = requireLong(args.get(0), "-");
            for (int i = 1; i < args.size(); i++) result -= requireLong(args.get(i), "-");
            return result;
        }));
        env.define("*", new BuiltinProc("*", args -> {
            long product = 1;
            for (Object a : args) product *= requireLong(a, "*");
            return product;
        }));
        env.define("/", new BuiltinProc("/", args -> {
            if (args.isEmpty()) throw new EvalError("/: need at least 1 argument");
            long result = requireLong(args.get(0), "/");
            for (int i = 1; i < args.size(); i++) {
                long divisor = requireLong(args.get(i), "/");
                if (divisor == 0) throw new EvalError("division by zero");
                result /= divisor;
            }
            return result;
        }));
        env.define("<", new BuiltinProc("<", args -> {
            requireArgCount("<", args, 2);
            return requireLong(args.get(0), "<") < requireLong(args.get(1), "<");
        }));
        env.define(">", new BuiltinProc(">", args -> {
            requireArgCount(">", args, 2);
            return requireLong(args.get(0), ">") > requireLong(args.get(1), ">");
        }));
        env.define("=", new BuiltinProc("=", args -> {
            requireArgCount("=", args, 2);
            return requireLong(args.get(0), "=") == requireLong(args.get(1), "=");
        }));
        env.define("<=", new BuiltinProc("<=", args -> {
            requireArgCount("<=", args, 2);
            return requireLong(args.get(0), "<=") <= requireLong(args.get(1), "<=");
        }));
        env.define("not", new BuiltinProc("not", args -> {
            requireArgCount("not", args, 1);
            return !isTruthy(args.get(0));
        }));

        // L03: List operations
        env.define("cons", new BuiltinProc("cons", args -> {
            requireArgCount("cons", args, 2);
            return new SchemePair(args.get(0), args.get(1));
        }));
        env.define("car", new BuiltinProc("car", args -> {
            requireArgCount("car", args, 1);
            if (args.get(0) instanceof SchemePair p) return p.car;
            throw new EvalError("car: not a pair: " + schemeToString(args.get(0)));
        }));
        env.define("cdr", new BuiltinProc("cdr", args -> {
            requireArgCount("cdr", args, 1);
            if (args.get(0) instanceof SchemePair p) return p.cdr;
            throw new EvalError("cdr: not a pair: " + schemeToString(args.get(0)));
        }));
        env.define("null?", new BuiltinProc("null?", args -> {
            requireArgCount("null?", args, 1);
            return args.get(0) == NIL;
        }));
        env.define("list", new BuiltinProc("list", args -> {
            Object result = NIL;
            for (int i = args.size() - 1; i >= 0; i--) {
                result = new SchemePair(args.get(i), result);
            }
            return result;
        }));
        env.define("length", new BuiltinProc("length", args -> {
            requireArgCount("length", args, 1);
            long count = 0;
            Object lst = args.get(0);
            while (lst instanceof SchemePair p) {
                count++;
                lst = p.cdr;
            }
            if (lst != NIL) throw new EvalError("length: not a proper list");
            return count;
        }));
        env.define("append", new BuiltinProc("append", args -> {
            if (args.isEmpty()) return NIL;
            if (args.size() == 1) return args.get(0);
            // append two lists
            Object result = args.get(args.size() - 1);
            for (int i = args.size() - 2; i >= 0; i--) {
                Object lst = args.get(i);
                // collect elements, then prepend
                List<Object> elems = new ArrayList<>();
                Object cur = lst;
                while (cur instanceof SchemePair p) {
                    elems.add(p.car);
                    cur = p.cdr;
                }
                for (int j = elems.size() - 1; j >= 0; j--) {
                    result = new SchemePair(elems.get(j), result);
                }
            }
            return result;
        }));

        // L03: Type predicates
        env.define("number?", new BuiltinProc("number?", args -> {
            requireArgCount("number?", args, 1);
            return args.get(0) instanceof Long;
        }));
        env.define("string?", new BuiltinProc("string?", args -> {
            requireArgCount("string?", args, 1);
            return args.get(0) instanceof SchemeString;
        }));
        env.define("boolean?", new BuiltinProc("boolean?", args -> {
            requireArgCount("boolean?", args, 1);
            return args.get(0) instanceof Boolean;
        }));
        env.define("symbol?", new BuiltinProc("symbol?", args -> {
            requireArgCount("symbol?", args, 1);
            return args.get(0) instanceof SchemeSymbol;
        }));
        env.define("pair?", new BuiltinProc("pair?", args -> {
            requireArgCount("pair?", args, 1);
            return args.get(0) instanceof SchemePair;
        }));

        // L05: Display, Write, Newline
        env.define("display", new BuiltinProc("display", args -> {
            requireArgCount("display", args, 1);
            outputBuffer.append(displayValue(args.get(0)));
            return null; // void
        }));
        env.define("write", new BuiltinProc("write", args -> {
            requireArgCount("write", args, 1);
            outputBuffer.append(schemeToString(args.get(0)));
            return null; // void
        }));
        env.define("newline", new BuiltinProc("newline", args -> {
            requireArgCount("newline", args, 0);
            outputBuffer.append("\n");
            return null; // void
        }));

        // L05: String operations
        env.define("string-append", new BuiltinProc("string-append", args -> {
            StringBuilder sb = new StringBuilder();
            for (Object a : args) {
                if (!(a instanceof SchemeString s)) throw new EvalError("string-append: not a string: " + schemeToString(a));
                sb.append(s.value);
            }
            return new SchemeString(sb.toString());
        }));
        env.define("string-length", new BuiltinProc("string-length", args -> {
            requireArgCount("string-length", args, 1);
            if (!(args.get(0) instanceof SchemeString s)) throw new EvalError("string-length: not a string");
            return (long) s.value.length();
        }));
        env.define("substring", new BuiltinProc("substring", args -> {
            if (args.size() < 2 || args.size() > 3) throw new EvalError("substring: expected 2 or 3 arguments");
            if (!(args.get(0) instanceof SchemeString s)) throw new EvalError("substring: not a string");
            int start = (int) requireLong(args.get(1), "substring");
            int end = args.size() == 3 ? (int) requireLong(args.get(2), "substring") : s.value.length();
            return new SchemeString(s.value.substring(start, end));
        }));
        env.define("string->number", new BuiltinProc("string->number", args -> {
            requireArgCount("string->number", args, 1);
            if (!(args.get(0) instanceof SchemeString s)) throw new EvalError("string->number: not a string");
            try {
                return Long.parseLong(s.value);
            } catch (NumberFormatException e) {
                return Boolean.FALSE;
            }
        }));
        env.define("number->string", new BuiltinProc("number->string", args -> {
            requireArgCount("number->string", args, 1);
            return new SchemeString(String.valueOf(requireLong(args.get(0), "number->string")));
        }));
        env.define("symbol->string", new BuiltinProc("symbol->string", args -> {
            requireArgCount("symbol->string", args, 1);
            if (!(args.get(0) instanceof SchemeSymbol s)) throw new EvalError("symbol->string: not a symbol");
            return new SchemeString(s.name);
        }));
        env.define("string->symbol", new BuiltinProc("string->symbol", args -> {
            requireArgCount("string->symbol", args, 1);
            if (!(args.get(0) instanceof SchemeString s)) throw new EvalError("string->symbol: not a string");
            return new SchemeSymbol(s.value, 0, 0);
        }));
        env.define("string-ref", new BuiltinProc("string-ref", args -> {
            requireArgCount("string-ref", args, 2);
            if (!(args.get(0) instanceof SchemeString s)) throw new EvalError("string-ref: not a string");
            int idx = (int) requireLong(args.get(1), "string-ref");
            return new SchemeChar(s.value.charAt(idx));
        }));
        // L06: Mutable strings
        env.define("string-copy", new BuiltinProc("string-copy", args -> {
            requireArgCount("string-copy", args, 1);
            if (!(args.get(0) instanceof SchemeString s)) throw new EvalError("string-copy: not a string");
            return new SchemeString(s.value);
        }));
        env.define("string-set!", new BuiltinProc("string-set!", args -> {
            if (args.size() != 3) throw new EvalError("string-set!: expected 3 arguments, got " + args.size());
            if (!(args.get(0) instanceof SchemeString s)) throw new EvalError("string-set!: not a string");
            int idx = (int) requireLong(args.get(1), "string-set!");
            if (!(args.get(2) instanceof SchemeChar c)) throw new EvalError("string-set!: not a character");
            char[] chars = s.value.toCharArray();
            chars[idx] = c.value;
            s.value = new String(chars);
            return null; // void
        }));
        env.define("char?", new BuiltinProc("char?", args -> {
            requireArgCount("char?", args, 1);
            return args.get(0) instanceof SchemeChar;
        }));

        // L08: apply
        env.define("apply", new BuiltinProc("apply", args -> {
            if (args.size() < 2) throw new EvalError("apply: expected at least 2 arguments");
            Object proc = args.get(0);
            // Last arg must be a list; preceding args are prepended
            Object lastArg = args.get(args.size() - 1);
            List<Object> callArgs = new ArrayList<>();
            for (int i = 1; i < args.size() - 1; i++) {
                callArgs.add(args.get(i));
            }
            // Unpack the last arg (a scheme list) into callArgs
            Object cur = lastArg;
            while (cur instanceof SchemePair p) {
                callArgs.add(p.car);
                cur = p.cdr;
            }
            return apply(proc, callArgs);
        }));

        // L09: eq? and equal?
        env.define("eq?", new BuiltinProc("eq?", args -> {
            requireArgCount("eq?", args, 2);
            Object a = args.get(0), b = args.get(1);
            if (a == b) return true;
            if (a instanceof Long && b instanceof Long) return a.equals(b);
            if (a instanceof Boolean && b instanceof Boolean) return a.equals(b);
            if (a instanceof SchemeSymbol sa && b instanceof SchemeSymbol sb) return sa.name.equals(sb.name);
            if (a instanceof SchemeChar ca && b instanceof SchemeChar cb) return ca.value == cb.value;
            return false;
        }));
        env.define("equal?", new BuiltinProc("equal?", args -> {
            requireArgCount("equal?", args, 2);
            return schemeEqual(args.get(0), args.get(1));
        }));

        // L09: map (multi-list)
        env.define("map", new BuiltinProc("map", args -> {
            if (args.size() < 2) throw new EvalError("map: expected at least 2 arguments");
            Object proc = args.get(0);
            List<Object> lists = new ArrayList<>();
            for (int i = 1; i < args.size(); i++) lists.add(args.get(i));
            List<Object> results = new ArrayList<>();
            while (true) {
                // Check if any list is exhausted
                boolean done = false;
                for (Object lst : lists) {
                    if (!(lst instanceof SchemePair)) { done = true; break; }
                }
                if (done) break;
                List<Object> callArgs = new ArrayList<>();
                List<Object> newLists = new ArrayList<>();
                for (Object lst : lists) {
                    SchemePair p = (SchemePair) lst;
                    callArgs.add(p.car);
                    newLists.add(p.cdr);
                }
                results.add(apply(proc, callArgs));
                lists = newLists;
            }
            Object result = NIL;
            for (int i = results.size() - 1; i >= 0; i--) {
                result = new SchemePair(results.get(i), result);
            }
            return result;
        }));

        // L09: Numeric utilities
        env.define("abs", new BuiltinProc("abs", args -> {
            requireArgCount("abs", args, 1);
            return Math.abs(requireLong(args.get(0), "abs"));
        }));
        env.define("modulo", new BuiltinProc("modulo", args -> {
            requireArgCount("modulo", args, 2);
            long a = requireLong(args.get(0), "modulo");
            long b = requireLong(args.get(1), "modulo");
            return Math.floorMod(a, b);
        }));
        env.define("remainder", new BuiltinProc("remainder", args -> {
            requireArgCount("remainder", args, 2);
            long a = requireLong(args.get(0), "remainder");
            long b = requireLong(args.get(1), "remainder");
            return a % b;
        }));
        env.define("quotient", new BuiltinProc("quotient", args -> {
            requireArgCount("quotient", args, 2);
            long a = requireLong(args.get(0), "quotient");
            long b = requireLong(args.get(1), "quotient");
            // Truncate toward zero (Java's default behavior)
            return a / b;
        }));
        env.define("min", new BuiltinProc("min", args -> {
            if (args.isEmpty()) throw new EvalError("min: need at least 1 argument");
            long result = requireLong(args.get(0), "min");
            for (int i = 1; i < args.size(); i++) {
                long v = requireLong(args.get(i), "min");
                if (v < result) result = v;
            }
            return result;
        }));
        env.define("max", new BuiltinProc("max", args -> {
            if (args.isEmpty()) throw new EvalError("max: need at least 1 argument");
            long result = requireLong(args.get(0), "max");
            for (int i = 1; i < args.size(); i++) {
                long v = requireLong(args.get(i), "max");
                if (v > result) result = v;
            }
            return result;
        }));
        env.define("expt", new BuiltinProc("expt", args -> {
            requireArgCount("expt", args, 2);
            long base = requireLong(args.get(0), "expt");
            long exp = requireLong(args.get(1), "expt");
            long result = 1;
            for (long i = 0; i < exp; i++) result *= base;
            return result;
        }));

        // L09: Numeric predicates
        env.define("zero?", new BuiltinProc("zero?", args -> {
            requireArgCount("zero?", args, 1);
            return requireLong(args.get(0), "zero?") == 0;
        }));
        env.define("positive?", new BuiltinProc("positive?", args -> {
            requireArgCount("positive?", args, 1);
            return requireLong(args.get(0), "positive?") > 0;
        }));
        env.define("negative?", new BuiltinProc("negative?", args -> {
            requireArgCount("negative?", args, 1);
            return requireLong(args.get(0), "negative?") < 0;
        }));
        env.define("odd?", new BuiltinProc("odd?", args -> {
            requireArgCount("odd?", args, 1);
            return requireLong(args.get(0), "odd?") % 2 != 0;
        }));
        env.define("even?", new BuiltinProc("even?", args -> {
            requireArgCount("even?", args, 1);
            return requireLong(args.get(0), "even?") % 2 == 0;
        }));

        // L09: List utilities
        env.define("list-ref", new BuiltinProc("list-ref", args -> {
            requireArgCount("list-ref", args, 2);
            int idx = (int) requireLong(args.get(1), "list-ref");
            Object lst = args.get(0);
            for (int i = 0; i < idx; i++) {
                if (!(lst instanceof SchemePair p)) throw new EvalError("list-ref: index out of range");
                lst = p.cdr;
            }
            if (!(lst instanceof SchemePair p)) throw new EvalError("list-ref: index out of range");
            return p.car;
        }));
        env.define("list-tail", new BuiltinProc("list-tail", args -> {
            requireArgCount("list-tail", args, 2);
            int idx = (int) requireLong(args.get(1), "list-tail");
            Object lst = args.get(0);
            for (int i = 0; i < idx; i++) {
                if (!(lst instanceof SchemePair p)) throw new EvalError("list-tail: index out of range");
                lst = p.cdr;
            }
            return lst;
        }));
        env.define("list?", new BuiltinProc("list?", args -> {
            requireArgCount("list?", args, 1);
            Object lst = args.get(0);
            while (lst instanceof SchemePair p) {
                lst = p.cdr;
            }
            return lst == NIL;
        }));
        env.define("assoc", new BuiltinProc("assoc", args -> {
            requireArgCount("assoc", args, 2);
            Object key = args.get(0);
            Object lst = args.get(1);
            while (lst instanceof SchemePair p) {
                if (p.car instanceof SchemePair entry) {
                    if (schemeEqual(key, entry.car)) return entry;
                }
                lst = p.cdr;
            }
            return false;
        }));

        // L09: Character operations
        env.define("char-alphabetic?", new BuiltinProc("char-alphabetic?", args -> {
            requireArgCount("char-alphabetic?", args, 1);
            if (!(args.get(0) instanceof SchemeChar c)) throw new EvalError("char-alphabetic?: not a character");
            return Character.isLetter(c.value);
        }));
        env.define("char-numeric?", new BuiltinProc("char-numeric?", args -> {
            requireArgCount("char-numeric?", args, 1);
            if (!(args.get(0) instanceof SchemeChar c)) throw new EvalError("char-numeric?: not a character");
            return Character.isDigit(c.value);
        }));
        env.define("char-upcase", new BuiltinProc("char-upcase", args -> {
            requireArgCount("char-upcase", args, 1);
            if (!(args.get(0) instanceof SchemeChar c)) throw new EvalError("char-upcase: not a character");
            return new SchemeChar(Character.toUpperCase(c.value));
        }));
        env.define("char-downcase", new BuiltinProc("char-downcase", args -> {
            requireArgCount("char-downcase", args, 1);
            if (!(args.get(0) instanceof SchemeChar c)) throw new EvalError("char-downcase: not a character");
            return new SchemeChar(Character.toLowerCase(c.value));
        }));
        env.define("char=?", new BuiltinProc("char=?", args -> {
            requireArgCount("char=?", args, 2);
            if (!(args.get(0) instanceof SchemeChar a)) throw new EvalError("char=?: not a character");
            if (!(args.get(1) instanceof SchemeChar b)) throw new EvalError("char=?: not a character");
            return a.value == b.value;
        }));
        env.define("char<?", new BuiltinProc("char<?", args -> {
            requireArgCount("char<?", args, 2);
            if (!(args.get(0) instanceof SchemeChar a)) throw new EvalError("char<?: not a character");
            if (!(args.get(1) instanceof SchemeChar b)) throw new EvalError("char<?: not a character");
            return a.value < b.value;
        }));

        // L09: String comparison/case operations
        env.define("string=?", new BuiltinProc("string=?", args -> {
            requireArgCount("string=?", args, 2);
            if (!(args.get(0) instanceof SchemeString a)) throw new EvalError("string=?: not a string");
            if (!(args.get(1) instanceof SchemeString b)) throw new EvalError("string=?: not a string");
            return a.value.equals(b.value);
        }));
        env.define("string<?", new BuiltinProc("string<?", args -> {
            requireArgCount("string<?", args, 2);
            if (!(args.get(0) instanceof SchemeString a)) throw new EvalError("string<?: not a string");
            if (!(args.get(1) instanceof SchemeString b)) throw new EvalError("string<?: not a string");
            return a.value.compareTo(b.value) < 0;
        }));
        env.define("string-ci=?", new BuiltinProc("string-ci=?", args -> {
            requireArgCount("string-ci=?", args, 2);
            if (!(args.get(0) instanceof SchemeString a)) throw new EvalError("string-ci=?: not a string");
            if (!(args.get(1) instanceof SchemeString b)) throw new EvalError("string-ci=?: not a string");
            return a.value.equalsIgnoreCase(b.value);
        }));
        env.define("string-upcase", new BuiltinProc("string-upcase", args -> {
            requireArgCount("string-upcase", args, 1);
            if (!(args.get(0) instanceof SchemeString s)) throw new EvalError("string-upcase: not a string");
            return new SchemeString(s.value.toUpperCase());
        }));
        env.define("string-downcase", new BuiltinProc("string-downcase", args -> {
            requireArgCount("string-downcase", args, 1);
            if (!(args.get(0) instanceof SchemeString s)) throw new EvalError("string-downcase: not a string");
            return new SchemeString(s.value.toLowerCase());
        }));

        // L09: >=
        env.define(">=", new BuiltinProc(">=", args -> {
            requireArgCount(">=", args, 2);
            return requireLong(args.get(0), ">=") >= requireLong(args.get(1), ">=");
        }));
    }

    // --- Macro support ---

    private Object evalDefineSyntax(List<Object> elems, Env env) throws EvalError {
        if (elems.size() != 3) throw new EvalError("define-syntax: bad syntax");
        String macroName = ((SchemeSymbol) elems.get(1)).name;
        Object transformer = elems.get(2);
        if (!(transformer instanceof SchemeList srList)) throw new EvalError("define-syntax: expected syntax-rules");
        if (srList.elems.isEmpty() || !(srList.elems.get(0) instanceof SchemeSymbol srSym)
                || !srSym.name.equals("syntax-rules"))
            throw new EvalError("define-syntax: expected syntax-rules");
        List<String> literals = new ArrayList<>();
        SchemeList litList = (SchemeList) srList.elems.get(1);
        for (Object lit : litList.elems) {
            literals.add(((SchemeSymbol) lit).name);
        }
        List<Object[]> clauses = new ArrayList<>();
        for (int i = 2; i < srList.elems.size(); i++) {
            SchemeList clause = (SchemeList) srList.elems.get(i);
            clauses.add(new Object[]{clause.elems.get(0), clause.elems.get(1)});
        }
        env.define(macroName, new SyntaxRulesMacro(literals, clauses, env));
        return null;
    }

    private Object expandMacro(SyntaxRulesMacro macro, SchemeList form, Env useEnv) throws EvalError {
        for (Object[] clause : macro.clauses) {
            SchemeList pattern = (SchemeList) clause[0];
            Object template = clause[1];
            java.util.Set<String> patVars = new java.util.HashSet<>();
            collectPatternVars(pattern, macro.literals, patVars, true);
            Map<String, Object> bindings = new HashMap<>();
            Map<String, List<Object>> ellipsisBindings = new HashMap<>();
            if (matchElems(pattern.elems, 1, form.elems, 1, macro.literals, bindings, ellipsisBindings)) {
                Map<String, String> renames = new HashMap<>();
                Object hygienic = renameTemplate(template, patVars, macro.literals, renames);
                Object expanded = expandTemplate(hygienic, bindings, ellipsisBindings, form.line, form.col);
                for (Map.Entry<String, String> entry : renames.entrySet()) {
                    try {
                        Object val = macro.defEnv.lookup(entry.getKey());
                        useEnv.define(entry.getValue(), val);
                    } catch (EvalError ignored) {}
                }
                return expanded;
            }
        }
        throw new EvalError("syntax-rules: no matching pattern");
    }

    private void collectPatternVars(Object pattern, List<String> literals,
                                     java.util.Set<String> patVars, boolean isTopLevel) {
        if (pattern instanceof SchemeSymbol sym) {
            if (!sym.name.equals("...") && !sym.name.equals("_") && !literals.contains(sym.name)) {
                patVars.add(sym.name);
            }
        } else if (pattern instanceof SchemeList list) {
            int start = isTopLevel ? 1 : 0;
            for (int i = start; i < list.elems.size(); i++) {
                Object elem = list.elems.get(i);
                if (elem instanceof SchemeSymbol s && s.name.equals("...")) continue;
                collectPatternVars(elem, literals, patVars, false);
            }
        }
    }

    private boolean matchElems(List<Object> pattern, int pStart, List<Object> input, int iStart,
                                List<String> literals, Map<String, Object> bindings,
                                Map<String, List<Object>> ellipsisBindings) {
        int pi = pStart, ii = iStart;
        while (pi < pattern.size()) {
            Object pat = pattern.get(pi);
            boolean hasEllipsis = (pi + 1 < pattern.size() &&
                pattern.get(pi + 1) instanceof SchemeSymbol s && s.name.equals("..."));
            if (hasEllipsis) {
                int remainingPats = pattern.size() - pi - 2;
                int available = input.size() - ii - remainingPats;
                if (available < 0) return false;
                if (pat instanceof SchemeSymbol sym && !literals.contains(sym.name) && !sym.name.equals("_")) {
                    List<Object> matched = new ArrayList<>();
                    for (int j = 0; j < available; j++) matched.add(input.get(ii + j));
                    ellipsisBindings.put(sym.name, matched);
                }
                ii += available;
                pi += 2;
            } else {
                if (ii >= input.size()) return false;
                if (!matchOne(pat, input.get(ii), literals, bindings, ellipsisBindings)) return false;
                pi++;
                ii++;
            }
        }
        return ii == input.size();
    }

    private boolean matchOne(Object pat, Object input, List<String> literals,
                              Map<String, Object> bindings, Map<String, List<Object>> ellipsisBindings) {
        if (pat instanceof SchemeSymbol sym) {
            if (literals.contains(sym.name)) {
                return input instanceof SchemeSymbol inSym && inSym.name.equals(sym.name);
            }
            if (sym.name.equals("_")) return true;
            bindings.put(sym.name, input);
            return true;
        }
        if (pat instanceof SchemeList patList) {
            if (!(input instanceof SchemeList inList)) return false;
            return matchElems(patList.elems, 0, inList.elems, 0, literals, bindings, ellipsisBindings);
        }
        if (pat instanceof Long && input instanceof Long) return pat.equals(input);
        if (pat instanceof Boolean && input instanceof Boolean) return pat.equals(input);
        return false;
    }

    private Object renameTemplate(Object template, java.util.Set<String> patVars,
                                   List<String> literals, Map<String, String> renames) {
        if (template instanceof SchemeSymbol sym) {
            if (patVars.contains(sym.name) || sym.name.equals("...") ||
                SPECIAL_FORMS.contains(sym.name) || literals.contains(sym.name)) {
                return template;
            }
            String renamed = renames.computeIfAbsent(sym.name, k -> gensym(k));
            return new SchemeSymbol(renamed, sym.line, sym.col);
        }
        if (template instanceof SchemeList list) {
            List<Object> newElems = new ArrayList<>();
            for (Object elem : list.elems) {
                newElems.add(renameTemplate(elem, patVars, literals, renames));
            }
            return new SchemeList(newElems, list.line, list.col);
        }
        return template;
    }

    private Object expandTemplate(Object template, Map<String, Object> bindings,
                                   Map<String, List<Object>> ellipsisBindings,
                                   int line, int col) throws EvalError {
        if (template instanceof SchemeSymbol sym) {
            if (bindings.containsKey(sym.name)) return bindings.get(sym.name);
            return template;
        }
        if (template instanceof SchemeList list) {
            List<Object> result = new ArrayList<>();
            for (int i = 0; i < list.elems.size(); i++) {
                Object elem = list.elems.get(i);
                boolean hasEllipsis = (i + 1 < list.elems.size() &&
                    list.elems.get(i + 1) instanceof SchemeSymbol s && s.name.equals("..."));
                if (hasEllipsis) {
                    java.util.Set<String> usedEVars = new java.util.HashSet<>();
                    findEllipsisVars(elem, ellipsisBindings, usedEVars);
                    if (!usedEVars.isEmpty()) {
                        String eVar = usedEVars.iterator().next();
                        List<Object> eList = ellipsisBindings.get(eVar);
                        for (int j = 0; j < eList.size(); j++) {
                            Map<String, Object> newBindings = new HashMap<>(bindings);
                            for (String v : usedEVars) {
                                List<Object> vList = ellipsisBindings.get(v);
                                if (j < vList.size()) newBindings.put(v, vList.get(j));
                            }
                            Map<String, List<Object>> newEllipsis = new HashMap<>(ellipsisBindings);
                            for (String v : usedEVars) newEllipsis.remove(v);
                            result.add(expandTemplate(elem, newBindings, newEllipsis, line, col));
                        }
                    }
                    i++; // skip "..."
                } else {
                    result.add(expandTemplate(elem, bindings, ellipsisBindings, line, col));
                }
            }
            return new SchemeList(result, line, col);
        }
        return template;
    }

    private void findEllipsisVars(Object template, Map<String, List<Object>> ellipsisBindings,
                                   java.util.Set<String> found) {
        if (template instanceof SchemeSymbol sym) {
            if (ellipsisBindings.containsKey(sym.name)) found.add(sym.name);
        } else if (template instanceof SchemeList list) {
            for (Object elem : list.elems) findEllipsisVars(elem, ellipsisBindings, found);
        }
    }

    private boolean schemeEqual(Object a, Object b) {
        if (a == b) return true;
        if (a == null || b == null) return a == b;
        if (a instanceof Long && b instanceof Long) return a.equals(b);
        if (a instanceof Boolean && b instanceof Boolean) return a.equals(b);
        if (a instanceof SchemeString sa && b instanceof SchemeString sb) return sa.value.equals(sb.value);
        if (a instanceof SchemeSymbol sa && b instanceof SchemeSymbol sb) return sa.name.equals(sb.name);
        if (a instanceof SchemeChar ca && b instanceof SchemeChar cb) return ca.value == cb.value;
        if (a instanceof SchemePair pa && b instanceof SchemePair pb) {
            return schemeEqual(pa.car, pb.car) && schemeEqual(pa.cdr, pb.cdr);
        }
        return false;
    }

    private long requireLong(Object val, String op) throws EvalError {
        if (val instanceof Long l) return l;
        throw new EvalError(op + ": not a number: " + schemeToString(val));
    }

    private void requireArgCount(String name, List<Object> args, int expected) throws EvalError {
        if (args.size() != expected)
            throw new EvalError(name + ": expected " + expected + " arguments, got " + args.size());
    }

    // --- Output ---

    private String schemeToString(Object val) {
        if (val == null) return "void";
        if (val == NIL) return "()";
        if (val instanceof Long l) return l.toString();
        if (val instanceof Boolean b) return b ? "#t" : "#f";
        if (val instanceof SchemeString s) return "\"" + s.value + "\"";
        if (val instanceof SchemeSymbol s) return s.name;
        if (val instanceof SchemePair) {
            StringBuilder sb = new StringBuilder("(");
            Object cur = val;
            boolean first = true;
            while (cur instanceof SchemePair p) {
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
        if (val instanceof SchemeList list) {
            StringBuilder sb = new StringBuilder("(");
            for (int i = 0; i < list.elems.size(); i++) {
                if (i > 0) sb.append(" ");
                sb.append(schemeToString(list.elems.get(i)));
            }
            sb.append(")");
            return sb.toString();
        }
        if (val instanceof SchemeChar c) {
            return switch (c.value) {
                case ' ' -> "#\\space";
                case '\n' -> "#\\newline";
                case '\t' -> "#\\tab";
                default -> "#\\" + c.value;
            };
        }
        if (val instanceof Lambda) return "#<procedure>";
        if (val instanceof BuiltinProc) return "#<procedure>";
        return val.toString();
    }

    private String displayValue(Object val) {
        if (val == null) return "void";
        if (val == NIL) return "()";
        if (val instanceof SchemeString s) return s.value;
        if (val instanceof SchemeChar c) return String.valueOf(c.value);
        if (val instanceof SchemePair) {
            StringBuilder sb = new StringBuilder("(");
            Object cur = val;
            boolean first = true;
            while (cur instanceof SchemePair p) {
                if (!first) sb.append(" ");
                first = false;
                sb.append(displayValue(p.car));
                cur = p.cdr;
            }
            if (cur != NIL) {
                sb.append(" . ");
                sb.append(displayValue(cur));
            }
            sb.append(")");
            return sb.toString();
        }
        return schemeToString(val);
    }

    // --- Data types ---

    record SchemeSymbol(String name, int line, int col) {}
    static class SchemeString {
        String value;
        SchemeString(String value) { this.value = value; }
        @Override public boolean equals(Object o) { return o instanceof SchemeString s && value.equals(s.value); }
        @Override public int hashCode() { return value.hashCode(); }
    }
    record SchemeList(List<Object> elems, int line, int col) {}
    record SchemeChar(char value) {}
}
