package ming;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

public class Evaluator {

    private final Environment globalEnv = new Environment(null);
    private final Map<String, String[]> recordTypes = new HashMap<>();
    private StringBuilder outputBuffer = null;

    private static final String[] BUILTIN_NAMES = {
        "+", "-", "*", "/", "<", ">", "=", "<=", ">=",
        "not",
        "cons", "car", "cdr", "null?", "list", "length", "append",
        "number?", "string?", "boolean?", "pair?", "symbol?", "char?",
        "integer?", "rational?", "exact?", "inexact?",
        "exact->inexact", "inexact->exact",
        "numerator", "denominator",
        "display", "write", "newline",
        "string-append", "string-length", "substring", "string-ref",
        "string->number", "number->string",
        "symbol->string", "string->symbol",
        "string-copy", "string-set!",
        "apply",
        "eq?", "equal?",
        "abs", "modulo", "remainder", "quotient", "min", "max", "expt",
        "zero?", "positive?", "negative?", "odd?", "even?",
        "list-ref", "list-tail", "list?", "assoc", "map",
        "char-alphabetic?", "char-numeric?", "char=?", "char<?",
        "char-upcase", "char-downcase",
        "string=?", "string<?", "string-ci=?", "string-upcase", "string-downcase",
        "procedure?"
    };

    {
        for (String name : BUILTIN_NAMES) {
            globalEnv.define(name, new BuiltinProcedure(name));
        }
    }

    // Source position tracking
    private record Token(String value, int line, int col) {}
    private record Located(Object expr, int line, int col) {}

    private static Object unwrap(Object o) {
        return o instanceof Located loc ? loc.expr() : o;
    }

    // Deep-unwrap: remove all Located wrappers recursively
    @SuppressWarnings("unchecked")
    static Object deepUnwrap(Object o) {
        o = unwrap(o);
        if (o instanceof List<?> list) {
            List<Object> result = new ArrayList<>(list.size());
            for (Object elem : list) result.add(deepUnwrap(elem));
            return result;
        }
        return o;
    }

    public String evalStr(String input) throws EvalError {
        List<Token> tokens = tokenize(input);
        int[] pos = {0};
        Object lastResult = null;
        while (pos[0] < tokens.size()) {
            Object expr = parse(tokens, pos);
            lastResult = eval(expr, globalEnv);
        }
        if (lastResult == null) {
            throw new EvalError("no expression");
        }
        return schemeToString(lastResult);
    }

    public EvalResult evalStrWithOutput(String input) throws EvalError {
        outputBuffer = new StringBuilder();
        try {
            List<Token> tokens = tokenize(input);
            int[] pos = {0};
            Object lastResult = null;
            while (pos[0] < tokens.size()) {
                Object expr = parse(tokens, pos);
                lastResult = eval(expr, globalEnv);
            }
            if (lastResult == null) {
                throw new EvalError("no expression");
            }
            return new EvalResult(schemeToString(lastResult), outputBuffer.toString());
        } finally {
            outputBuffer = null;
        }
    }

    // --- Tokenizer ---

    private List<Token> tokenize(String input) throws EvalError {
        List<Token> tokens = new ArrayList<>();
        int i = 0;
        int len = input.length();
        int line = 1;
        int col = 1;
        while (i < len) {
            char c = input.charAt(i);
            if (c == '\n') {
                line++;
                col = 1;
                i++;
            } else if (Character.isWhitespace(c)) {
                col++;
                i++;
            } else if (c == ';') {
                while (i < len && input.charAt(i) != '\n') {
                    i++;
                    col++;
                }
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
                sb.append('"');
                i++;
                col++;
                while (i < len && input.charAt(i) != '"') {
                    if (input.charAt(i) == '\\') {
                        sb.append(input.charAt(i));
                        i++;
                        col++;
                        if (i < len) {
                            sb.append(input.charAt(i));
                            i++;
                            col++;
                        }
                    } else {
                        if (input.charAt(i) == '\n') {
                            line++;
                            col = 1;
                        } else {
                            col++;
                        }
                        sb.append(input.charAt(i));
                        i++;
                    }
                }
                if (i < len) {
                    sb.append('"');
                    i++;
                    col++;
                }
                tokens.add(new Token(sb.toString(), startLine, startCol));
            } else if (c == '#') {
                int startCol = col;
                if (i + 1 < len) {
                    char next = input.charAt(i + 1);
                    if (next == 't') {
                        tokens.add(new Token("#t", line, startCol));
                        i += 2;
                        col += 2;
                    } else if (next == 'f') {
                        tokens.add(new Token("#f", line, startCol));
                        i += 2;
                        col += 2;
                    } else if (next == '\\') {
                        // Character literal: #\x or #\space, #\newline, etc.
                        if (i + 2 < len) {
                            // Try to read a named character or single char
                            int charStart = i + 2;
                            int charEnd = charStart;
                            while (charEnd < len && !Character.isWhitespace(input.charAt(charEnd))
                                    && input.charAt(charEnd) != ')' && input.charAt(charEnd) != '('
                                    && input.charAt(charEnd) != '"' && input.charAt(charEnd) != ';') {
                                charEnd++;
                            }
                            String charName = input.substring(charStart, charEnd);
                            String tok = "#\\" + charName;
                            tokens.add(new Token(tok, line, startCol));
                            int tokLen = tok.length();
                            i += tokLen;
                            col += tokLen;
                        } else {
                            tokens.add(new Token("#\\", line, startCol));
                            i += 2;
                            col += 2;
                        }
                    } else {
                        String sym = readSymbol(input, i);
                        tokens.add(new Token(sym, line, startCol));
                        i += sym.length();
                        col += sym.length();
                    }
                } else {
                    tokens.add(new Token("#", line, startCol));
                    i++;
                    col++;
                }
            } else {
                int startCol = col;
                String sym = readSymbol(input, i);
                tokens.add(new Token(sym, line, startCol));
                i += sym.length();
                col += sym.length();
            }
        }
        return tokens;
    }

    private String readSymbol(String input, int start) {
        int i = start;
        int len = input.length();
        while (i < len) {
            char c = input.charAt(i);
            if (Character.isWhitespace(c) || c == '(' || c == ')' || c == '"' || c == ';' || c == '\'') {
                break;
            }
            i++;
        }
        return input.substring(start, i);
    }

    // --- Parser ---

    private Object parse(List<Token> tokens, int[] pos) throws EvalError {
        if (pos[0] >= tokens.size()) {
            throw new EvalError("unexpected end of input");
        }
        Token token = tokens.get(pos[0]);
        pos[0]++;

        if (token.value().equals("'")) {
            Object quoted = parse(tokens, pos);
            List<Object> quoteExpr = new ArrayList<>();
            quoteExpr.add(new SchemeSymbol("quote"));
            quoteExpr.add(quoted);
            return new Located(quoteExpr, token.line(), token.col());
        }

        if (token.value().equals("(")) {
            List<Object> list = new ArrayList<>();
            while (pos[0] < tokens.size() && !tokens.get(pos[0]).value().equals(")")) {
                list.add(parse(tokens, pos));
            }
            if (pos[0] >= tokens.size()) {
                throw new EvalError("missing closing parenthesis");
            }
            pos[0]++; // skip ')'
            return new Located(list, token.line(), token.col());
        } else if (token.value().equals(")")) {
            throw new EvalError("unexpected )");
        } else {
            return new Located(parseAtom(token.value()), token.line(), token.col());
        }
    }

    private Object parseAtom(String token) {
        if (token.equals("#t")) {
            return Boolean.TRUE;
        }
        if (token.equals("#f")) {
            return Boolean.FALSE;
        }
        if (token.startsWith("#\\")) {
            String charName = token.substring(2);
            if (charName.equals("space")) return new SchemeChar(' ');
            if (charName.equals("newline")) return new SchemeChar('\n');
            if (charName.equals("tab")) return new SchemeChar('\t');
            if (charName.length() == 1) return new SchemeChar(charName.charAt(0));
            return new SchemeChar(charName.charAt(0)); // fallback
        }
        if (token.startsWith("\"") && token.endsWith("\"")) {
            return token; // keep as quoted string
        }
        // Try integer
        try {
            return Long.parseLong(token);
        } catch (NumberFormatException e) {}
        // Try rational literal: num/den
        int slash = token.indexOf('/');
        if (slash > 0 && slash < token.length() - 1) {
            try {
                long num = Long.parseLong(token.substring(0, slash));
                long den = Long.parseLong(token.substring(slash + 1));
                return SchemeRational.make(num, den);
            } catch (NumberFormatException e) {}
        }
        // Try floating point (inexact)
        try {
            return Double.parseDouble(token);
        } catch (NumberFormatException e) {}
        return new SchemeSymbol(token);
    }

    // --- Evaluator ---

    private static final Object VOID = new Object() {
        @Override public String toString() { return "#<void>"; }
    };

    // Convert a parsed Java List to a Scheme list (SchemePair chain ending in NIL)
    private Object javaListToSchemeList(List<?> javaList) {
        Object result = SchemeNil.INSTANCE;
        for (int i = javaList.size() - 1; i >= 0; i--) {
            Object elem = unwrap(javaList.get(i));
            if (elem instanceof List<?> subList) {
                elem = javaListToSchemeList(subList);
            }
            result = new SchemePair(elem, result);
        }
        return result;
    }

    @SuppressWarnings("unchecked")
    private Object eval(Object expr, Environment env) throws EvalError {
        // Unwrap Located and add position to any errors
        if (expr instanceof Located loc) {
            try {
                return eval(loc.expr(), env);
            } catch (EvalError e) {
                if (e.getMessage() != null && !e.getMessage().matches(".*\\d+:\\d+.*")) {
                    throw new EvalError(e.getMessage() + " [" + loc.line() + ":" + loc.col() + "]");
                }
                throw e;
            }
        }

        if (expr instanceof Long || expr instanceof Double || expr instanceof SchemeRational || expr instanceof Boolean || expr instanceof SchemeChar) {
            return expr;
        }
        if (expr instanceof String s) {
            return s;
        }
        if (expr instanceof SchemeString ss) {
            return ss;
        }
        if (expr instanceof SchemeSymbol sym) {
            return env.lookup(sym.name());
        }
        if (expr instanceof List<?> rawList) {
            List<Object> list = (List<Object>) rawList;
            if (list.isEmpty()) {
                throw new EvalError("empty application");
            }
            Object first = list.get(0);
            Object rawFirst = unwrap(first);

            // Special forms
            if (rawFirst instanceof SchemeSymbol sym) {
                String name = sym.name();
                List<Object> args = list.subList(1, list.size());

                switch (name) {
                    case "quote" -> {
                        if (args.size() != 1) throw new EvalError("quote: expected 1 argument");
                        Object quoted = unwrap(args.get(0));
                        if (quoted instanceof List<?> ql) {
                            return javaListToSchemeList(ql);
                        }
                        return quoted;
                    }
                    case "if" -> {
                        if (args.size() < 2 || args.size() > 3)
                            throw new EvalError("if: expected 2 or 3 arguments");
                        Object cond = eval(args.get(0), env);
                        if (!cond.equals(Boolean.FALSE)) {
                            return eval(args.get(1), env);
                        } else if (args.size() == 3) {
                            return eval(args.get(2), env);
                        }
                        return VOID;
                    }
                    case "define" -> {
                        return evalDefine(args, env);
                    }
                    case "set!" -> {
                        if (args.size() != 2) throw new EvalError("set!: bad syntax");
                        Object target = unwrap(args.get(0));
                        if (!(target instanceof SchemeSymbol s))
                            throw new EvalError("set!: not a variable");
                        Object val = eval(args.get(1), env);
                        env.set(s.name(), val);
                        return VOID;
                    }
                    case "lambda" -> {
                        if (args.size() < 2) throw new EvalError("lambda: bad syntax");
                        return parseLambda(args, env);
                    }
                    case "case-lambda" -> {
                        if (args.isEmpty()) throw new EvalError("case-lambda: bad syntax");
                        List<SchemeLambda> clauses = new ArrayList<>();
                        for (Object clause : args) {
                            List<?> clauseList = (List<?>) unwrap(clause);
                            if (clauseList.size() < 2) throw new EvalError("case-lambda: bad clause");
                            List<Object> clauseArgs = new ArrayList<>(clauseList);
                            clauses.add(parseLambda(clauseArgs, env));
                        }
                        return new SchemeCaseLambda(clauses);
                    }
                    case "begin" -> {
                        Object result = VOID;
                        for (Object a : args) {
                            result = eval(a, env);
                        }
                        return result;
                    }
                    case "let" -> {
                        if (args.size() < 2) throw new EvalError("let: bad syntax");
                        Object first2 = unwrap(args.get(0));
                        if (first2 instanceof SchemeSymbol loopName) {
                            // Named let: (let name ((var init) ...) body ...)
                            if (args.size() < 3) throw new EvalError("let: bad syntax");
                            Object bindingsObj = unwrap(args.get(1));
                            List<?> bindings = (List<?>) bindingsObj;
                            List<String> params = new ArrayList<>();
                            List<Object> inits = new ArrayList<>();
                            for (Object binding : bindings) {
                                List<?> b = (List<?>) unwrap(binding);
                                params.add(((SchemeSymbol) unwrap(b.get(0))).name());
                                inits.add(eval(b.get(1), env));
                            }
                            Object body = wrapBodyInBegin(args, 2);
                            Environment letEnv = new Environment(env);
                            SchemeLambda loopLam = new SchemeLambda(params, body, letEnv);
                            letEnv.define(loopName.name(), loopLam);
                            return apply(loopLam, inits);
                        }
                        // Regular let: (let ((var val) ...) body ...)
                        List<?> bindings = (List<?>) first2;
                        Environment letEnv = new Environment(env);
                        for (Object binding : bindings) {
                            List<?> b = (List<?>) unwrap(binding);
                            String varName = ((SchemeSymbol) unwrap(b.get(0))).name();
                            Object val = eval(b.get(1), env);
                            letEnv.define(varName, val);
                        }
                        Object result = VOID;
                        for (int i = 1; i < args.size(); i++) {
                            result = eval(args.get(i), letEnv);
                        }
                        return result;
                    }
                    case "cond" -> {
                        for (Object clause : args) {
                            List<?> cl = (List<?>) unwrap(clause);
                            if (cl.isEmpty()) throw new EvalError("cond: empty clause");
                            Object test = cl.get(0);
                            Object rawTest = unwrap(test);
                            if (rawTest instanceof SchemeSymbol s && s.name().equals("else")) {
                                Object result = VOID;
                                for (int i = 1; i < cl.size(); i++) {
                                    result = eval(cl.get(i), env);
                                }
                                return result;
                            }
                            Object testVal = eval(test, env);
                            if (!testVal.equals(Boolean.FALSE)) {
                                if (cl.size() == 1) return testVal;
                                Object result = VOID;
                                for (int i = 1; i < cl.size(); i++) {
                                    result = eval(cl.get(i), env);
                                }
                                return result;
                            }
                        }
                        return VOID;
                    }
                    // Builtins handled as special forms (unevaluated args for and/or)
                    case "and" -> {
                        Object result = Boolean.TRUE;
                        for (Object arg : args) {
                            result = eval(arg, env);
                            if (result.equals(Boolean.FALSE)) return Boolean.FALSE;
                        }
                        return result;
                    }
                    case "or" -> {
                        Object result = Boolean.FALSE;
                        for (Object arg : args) {
                            result = eval(arg, env);
                            if (!result.equals(Boolean.FALSE)) return result;
                        }
                        return result;
                    }
                    case "+", "-", "*", "/", "<", ">", "=", "<=", ">=",
                         "not",
                         "cons", "car", "cdr", "null?", "list", "length", "append",
                         "number?", "string?", "boolean?", "pair?", "symbol?", "char?",
                         "integer?", "rational?", "exact?", "inexact?",
                         "exact->inexact", "inexact->exact",
                         "numerator", "denominator",
                         "display", "write", "newline",
                         "string-append", "string-length", "substring", "string-ref",
                         "string->number", "number->string",
                         "symbol->string", "string->symbol",
                         "string-copy", "string-set!",
                         "apply",
                         "eq?", "equal?",
                         "abs", "modulo", "remainder", "quotient", "min", "max", "expt",
                         "zero?", "positive?", "negative?", "odd?", "even?",
                         "list-ref", "list-tail", "list?", "assoc", "map",
                         "char-alphabetic?", "char-numeric?", "char=?", "char<?",
                         "char-upcase", "char-downcase",
                         "string=?", "string<?", "string-ci=?", "string-upcase", "string-downcase",
                         "procedure?" -> {
                        return evalBuiltin(name, args, env);
                    }
                    case "define-record-type" -> {
                        return evalDefineRecordType(args, env);
                    }
                    case "define-syntax" -> {
                        if (args.size() != 2) throw new EvalError("define-syntax: bad syntax");
                        String macroName = ((SchemeSymbol) unwrap(args.get(0))).name();
                        List<?> srForm = (List<?>) unwrap(args.get(1));
                        Object srHead = unwrap(srForm.get(0));
                        if (!(srHead instanceof SchemeSymbol ss) || !ss.name().equals("syntax-rules"))
                            throw new EvalError("define-syntax: expected syntax-rules");
                        List<?> litList = (List<?>) unwrap(srForm.get(1));
                        List<String> literals = new ArrayList<>();
                        for (Object lit : litList)
                            literals.add(((SchemeSymbol) unwrap(lit)).name());
                        List<Object[]> rules = new ArrayList<>();
                        for (int i = 2; i < srForm.size(); i++) {
                            List<?> rule = (List<?>) unwrap(srForm.get(i));
                            rules.add(new Object[]{deepUnwrap(rule.get(0)), deepUnwrap(rule.get(1))});
                        }
                        env.define(macroName, new SyntaxRules(literals, rules, env));
                        return VOID;
                    }
                    default -> {
                        // Check if this is a macro call
                        try {
                            Object val = env.lookup(name);
                            if (val instanceof SyntaxRules sr) {
                                @SuppressWarnings("unchecked")
                                List<Object> deepForm = (List<Object>) deepUnwrap(list);
                                Object expanded = sr.expand(deepForm, env);
                                return eval(expanded, env);
                            }
                        } catch (EvalError ignored) {}
                        // Fall through to procedure call
                    }
                }
            }

            // Procedure call: evaluate all, then apply
            Object proc = eval(first, env);
            List<Object> evaledArgs = new ArrayList<>();
            for (int i = 1; i < list.size(); i++) {
                evaledArgs.add(eval(list.get(i), env));
            }
            return apply(proc, evaledArgs);
        }
        throw new EvalError("cannot evaluate: " + expr);
    }

    @SuppressWarnings("unchecked")
    private Object evalDefine(List<Object> args, Environment env) throws EvalError {
        if (args.size() < 2) throw new EvalError("define: bad syntax");
        Object target = unwrap(args.get(0));
        if (target instanceof SchemeSymbol s) {
            Object val = eval(args.get(1), env);
            env.define(s.name(), val);
            return VOID;
        }
        if (target instanceof List<?> sig) {
            if (sig.isEmpty()) throw new EvalError("define: bad syntax");
            String fname = ((SchemeSymbol) unwrap(sig.get(0))).name();
            List<String> params = new ArrayList<>();
            String restParam = null;
            for (int i = 1; i < sig.size(); i++) {
                String pname = ((SchemeSymbol) unwrap(sig.get(i))).name();
                if (pname.equals(".")) {
                    if (i + 1 < sig.size()) {
                        restParam = ((SchemeSymbol) unwrap(sig.get(i + 1))).name();
                    }
                    break;
                }
                params.add(pname);
            }
            Object body = wrapBodyInBegin(args, 1);
            env.define(fname, new SchemeLambda(params, restParam, body, env));
            return VOID;
        }
        throw new EvalError("define: bad syntax");
    }

    private Object evalDefineRecordType(List<Object> args, Environment env) throws EvalError {
        if (args.size() < 3) throw new EvalError("define-record-type: bad syntax");
        String typeName = ((SchemeSymbol) unwrap(args.get(0))).name();
        List<?> ctorSpec = (List<?>) unwrap(args.get(1));
        String ctorName = ((SchemeSymbol) unwrap(ctorSpec.get(0))).name();
        List<String> ctorFields = new ArrayList<>();
        for (int i = 1; i < ctorSpec.size(); i++)
            ctorFields.add(((SchemeSymbol) unwrap(ctorSpec.get(i))).name());
        String predName = ((SchemeSymbol) unwrap(args.get(2))).name();
        List<String> fieldNames = new ArrayList<>();
        List<String> accessorNames = new ArrayList<>();
        for (int i = 3; i < args.size(); i++) {
            List<?> fieldSpec = (List<?>) unwrap(args.get(i));
            fieldNames.add(((SchemeSymbol) unwrap(fieldSpec.get(0))).name());
            accessorNames.add(((SchemeSymbol) unwrap(fieldSpec.get(1))).name());
        }
        recordTypes.put(typeName, ctorFields.toArray(new String[0]));
        env.define(ctorName, new BuiltinProcedure("record-ctor:" + typeName));
        env.define(predName, new BuiltinProcedure("record-pred:" + typeName));
        for (int i = 0; i < fieldNames.size(); i++) {
            env.define(accessorNames.get(i), new BuiltinProcedure("record-acc:" + typeName + ":" + fieldNames.get(i)));
        }
        return VOID;
    }

    private Object wrapBodyInBegin(List<Object> args, int bodyStart) {
        if (args.size() == bodyStart + 1) {
            return args.get(bodyStart);
        }
        List<Object> beginList = new ArrayList<>();
        beginList.add(new SchemeSymbol("begin"));
        beginList.addAll(args.subList(bodyStart, args.size()));
        return beginList;
    }

    private SchemeLambda parseLambda(List<?> args, Environment env) throws EvalError {
        Object paramObj = unwrap(args.get(0));
        List<?> paramList = (List<?>) paramObj;
        List<String> params = new ArrayList<>();
        String restParam = null;
        for (int i = 0; i < paramList.size(); i++) {
            String pname = ((SchemeSymbol) unwrap(paramList.get(i))).name();
            if (pname.equals(".")) {
                if (i + 1 < paramList.size()) {
                    restParam = ((SchemeSymbol) unwrap(paramList.get(i + 1))).name();
                }
                break;
            }
            params.add(pname);
        }
        List<Object> bodyArgs = new ArrayList<>();
        for (int i = 1; i < args.size(); i++) bodyArgs.add(args.get(i));
        return new SchemeLambda(params, restParam, wrapBodyInBegin(bodyArgs, 0), env);
    }

    private Object apply(Object proc, List<Object> args) throws EvalError {
        if (proc instanceof SchemeCaseLambda cl) {
            for (SchemeLambda clause : cl.clauses) {
                if (clause.restParam != null) {
                    if (args.size() >= clause.params.size()) {
                        return apply(clause, args);
                    }
                } else {
                    if (args.size() == clause.params.size()) {
                        return apply(clause, args);
                    }
                }
            }
            throw new EvalError("no matching clause for " + args.size() + " arguments");
        }
        if (proc instanceof SchemeLambda lam) {
            if (lam.restParam != null) {
                if (args.size() < lam.params.size()) {
                    throw new EvalError("expected at least " + lam.params.size() + " arguments, got " + args.size());
                }
            } else {
                if (args.size() != lam.params.size()) {
                    throw new EvalError("expected " + lam.params.size() + " arguments, got " + args.size());
                }
            }
            Environment callEnv = new Environment(lam.closure);
            for (int i = 0; i < lam.params.size(); i++) {
                callEnv.define(lam.params.get(i), args.get(i));
            }
            if (lam.restParam != null) {
                Object rest = SchemeNil.INSTANCE;
                for (int i = args.size() - 1; i >= lam.params.size(); i--) {
                    rest = new SchemePair(args.get(i), rest);
                }
                callEnv.define(lam.restParam, rest);
            }
            return eval(lam.body, callEnv);
        }
        if (proc instanceof BuiltinProcedure bp) {
            return applyBuiltin(bp.name(), args);
        }
        throw new EvalError("not a procedure");
    }

    private static boolean isNumber(Object o) {
        return o instanceof Long || o instanceof Double || o instanceof SchemeRational;
    }

    private static double toDouble(Object o) throws EvalError {
        if (o instanceof Long l) return l.doubleValue();
        if (o instanceof Double d) return d;
        if (o instanceof SchemeRational r) return r.toDouble();
        throw new EvalError("expected number, got: " + schemeToString(o));
    }

    private static boolean hasInexact(List<Object> args) {
        for (Object a : args) if (a instanceof Double) return true;
        return false;
    }

    private static long[] toRational(Object a) throws EvalError {
        if (a instanceof Long l) return new long[]{l, 1};
        if (a instanceof SchemeRational r) return new long[]{r.numerator, r.denominator};
        throw new EvalError("expected number, got: " + schemeToString(a));
    }

    private static Object exactAdd(Object a, Object b) throws EvalError {
        long[] ra = toRational(a), rb = toRational(b);
        return SchemeRational.make(ra[0] * rb[1] + rb[0] * ra[1], ra[1] * rb[1]);
    }

    private static Object exactSub(Object a, Object b) throws EvalError {
        long[] ra = toRational(a), rb = toRational(b);
        return SchemeRational.make(ra[0] * rb[1] - rb[0] * ra[1], ra[1] * rb[1]);
    }

    private static Object exactMul(Object a, Object b) throws EvalError {
        long[] ra = toRational(a), rb = toRational(b);
        return SchemeRational.make(ra[0] * rb[0], ra[1] * rb[1]);
    }

    private static Object exactDiv(Object a, Object b) throws EvalError {
        long[] ra = toRational(a), rb = toRational(b);
        if (rb[0] == 0) throw new EvalError("division by zero");
        return SchemeRational.make(ra[0] * rb[1], ra[1] * rb[0]);
    }

    private static Object exactNeg(Object a) throws EvalError {
        if (a instanceof Long l) return -l;
        if (a instanceof SchemeRational r) return SchemeRational.make(-r.numerator, r.denominator);
        throw new EvalError("expected number, got: " + schemeToString(a));
    }

    private Object applyBuiltin(String name, List<Object> args) throws EvalError {
        return switch (name) {
            case "+", "-", "*", "/", "abs", "modulo", "remainder", "quotient",
                 "min", "max", "expt", "zero?", "positive?", "negative?", "odd?", "even?" ->
                applyArithmeticBuiltin(name, args);
            case "<" -> toDouble(args.get(0)) < toDouble(args.get(1));
            case ">" -> toDouble(args.get(0)) > toDouble(args.get(1));
            case "=" -> toDouble(args.get(0)) == toDouble(args.get(1));
            case "<=" -> toDouble(args.get(0)) <= toDouble(args.get(1));
            case ">=" -> toDouble(args.get(0)) >= toDouble(args.get(1));
            case "not" -> args.get(0).equals(Boolean.FALSE);
            case "cons", "car", "cdr", "null?", "list", "length", "append",
                 "list-ref", "list-tail", "list?", "assoc", "map" ->
                applyListBuiltin(name, args);
            case "number?", "integer?", "rational?", "exact?", "inexact?",
                 "exact->inexact", "inexact->exact", "numerator", "denominator" ->
                applyNumericTypeBuiltin(name, args);
            case "string?", "boolean?", "pair?", "symbol?", "char?", "procedure?" ->
                applyTypePredicateBuiltin(name, args);
            case "display", "write", "newline" ->
                applyIoBuiltin(name, args);
            case "string-append", "string-length", "substring", "string-ref",
                 "string->number", "number->string", "symbol->string", "string->symbol",
                 "string-copy", "string-set!", "string=?", "string<?", "string-ci=?",
                 "string-upcase", "string-downcase" ->
                applyStringBuiltin(name, args);
            case "char-alphabetic?", "char-numeric?", "char=?", "char<?",
                 "char-upcase", "char-downcase" ->
                applyCharBuiltin(name, args);
            case "apply" -> {
                if (args.size() < 2) throw new EvalError("apply: expected at least 2 arguments");
                Object applyProc = args.get(0);
                Object lastArg = args.get(args.size() - 1);
                List<Object> allArgs = new ArrayList<>();
                for (int i = 1; i < args.size() - 1; i++) allArgs.add(args.get(i));
                Object cur = lastArg;
                while (cur instanceof SchemePair p) { allArgs.add(p.car); cur = p.cdr; }
                yield apply(applyProc, allArgs);
            }
            case "eq?" -> {
                Object a = args.get(0), b = args.get(1);
                if (a instanceof SchemeSymbol sa && b instanceof SchemeSymbol sb) yield sa.name().equals(sb.name());
                if (a instanceof SchemeChar ca && b instanceof SchemeChar cb) yield ca.value() == cb.value();
                yield a == b || a.equals(b);
            }
            case "equal?" -> schemeEqual(args.get(0), args.get(1));
            default -> {
                if (name.startsWith("record-ctor:")) {
                    String recType = name.substring("record-ctor:".length());
                    String[] fieldNames = recordTypes.get(recType);
                    if (fieldNames == null) throw new EvalError("unknown record type: " + recType);
                    if (args.size() != fieldNames.length)
                        throw new EvalError(recType + " constructor: expected " + fieldNames.length + " arguments, got " + args.size());
                    yield new SchemeRecord(recType, fieldNames, args.toArray());
                } else if (name.startsWith("record-pred:")) {
                    String recType = name.substring("record-pred:".length());
                    yield args.get(0) instanceof SchemeRecord r && r.typeName.equals(recType);
                } else if (name.startsWith("record-acc:")) {
                    String rest = name.substring("record-acc:".length());
                    int colonIdx = rest.indexOf(':');
                    String recType = rest.substring(0, colonIdx);
                    String fieldName = rest.substring(colonIdx + 1);
                    if (!(args.get(0) instanceof SchemeRecord r) || !r.typeName.equals(recType))
                        throw new EvalError(name + ": not a " + recType);
                    Object val = r.getField(fieldName);
                    if (val == null) throw new EvalError(name + ": no such field " + fieldName);
                    yield val;
                } else {
                    throw new EvalError("unknown procedure: " + name);
                }
            }
        };
    }

    private Object applyArithmeticBuiltin(String name, List<Object> args) throws EvalError {
        return switch (name) {
            case "+" -> {
                if (hasInexact(args)) {
                    double result = 0;
                    for (Object arg : args) result += toDouble(arg);
                    yield result;
                }
                Object result = 0L;
                for (Object arg : args) result = exactAdd(result, arg);
                yield result;
            }
            case "-" -> {
                if (args.isEmpty()) throw new EvalError("- requires at least one argument");
                if (hasInexact(args)) {
                    if (args.size() == 1) yield -toDouble(args.get(0));
                    double result = toDouble(args.get(0));
                    for (int i = 1; i < args.size(); i++) result -= toDouble(args.get(i));
                    yield result;
                }
                if (args.size() == 1) yield exactNeg(args.get(0));
                Object result = args.get(0);
                for (int i = 1; i < args.size(); i++) result = exactSub(result, args.get(i));
                yield result;
            }
            case "*" -> {
                if (hasInexact(args)) {
                    double result = 1;
                    for (Object arg : args) result *= toDouble(arg);
                    yield result;
                }
                Object result = 1L;
                for (Object arg : args) result = exactMul(result, arg);
                yield result;
            }
            case "/" -> {
                if (args.isEmpty()) throw new EvalError("/ requires at least one argument");
                if (hasInexact(args)) {
                    double result = toDouble(args.get(0));
                    if (args.size() == 1) yield 1.0 / result;
                    for (int i = 1; i < args.size(); i++) {
                        double d = toDouble(args.get(i));
                        if (d == 0) throw new EvalError("division by zero");
                        result /= d;
                    }
                    yield result;
                }
                Object result = args.get(0);
                if (args.size() == 1) yield exactDiv(1L, result);
                for (int i = 1; i < args.size(); i++) result = exactDiv(result, args.get(i));
                yield result;
            }
            case "abs" -> Math.abs(requireLong(args.get(0)));
            case "modulo" -> Math.floorMod(requireLong(args.get(0)), requireLong(args.get(1)));
            case "remainder" -> requireLong(args.get(0)) % requireLong(args.get(1));
            case "quotient" -> requireLong(args.get(0)) / requireLong(args.get(1));
            case "min" -> {
                if (args.isEmpty()) throw new EvalError("min: expected at least 1 argument");
                long result = requireLong(args.get(0));
                for (int i = 1; i < args.size(); i++) result = Math.min(result, requireLong(args.get(i)));
                yield result;
            }
            case "max" -> {
                if (args.isEmpty()) throw new EvalError("max: expected at least 1 argument");
                long result = requireLong(args.get(0));
                for (int i = 1; i < args.size(); i++) result = Math.max(result, requireLong(args.get(i)));
                yield result;
            }
            case "expt" -> {
                long base = requireLong(args.get(0)), exp = requireLong(args.get(1));
                long result = 1;
                for (long i = 0; i < exp; i++) result *= base;
                yield result;
            }
            case "zero?" -> requireLong(args.get(0)) == 0;
            case "positive?" -> requireLong(args.get(0)) > 0;
            case "negative?" -> requireLong(args.get(0)) < 0;
            case "odd?" -> Math.abs(requireLong(args.get(0))) % 2 == 1;
            case "even?" -> requireLong(args.get(0)) % 2 == 0;
            default -> throw new EvalError("unknown arithmetic procedure: " + name);
        };
    }

    private Object applyListBuiltin(String name, List<Object> args) throws EvalError {
        return switch (name) {
            case "cons" -> new SchemePair(args.get(0), args.get(1));
            case "car" -> {
                if (args.get(0) instanceof SchemePair p) yield p.car;
                throw new EvalError("car: not a pair");
            }
            case "cdr" -> {
                if (args.get(0) instanceof SchemePair p) yield p.cdr;
                throw new EvalError("cdr: not a pair");
            }
            case "null?" -> args.get(0) instanceof SchemeNil;
            case "list" -> {
                Object result = SchemeNil.INSTANCE;
                for (int i = args.size() - 1; i >= 0; i--) result = new SchemePair(args.get(i), result);
                yield result;
            }
            case "length" -> {
                Object val = args.get(0);
                long len = 0;
                while (val instanceof SchemePair p) { len++; val = p.cdr; }
                if (!(val instanceof SchemeNil)) throw new EvalError("length: not a proper list");
                yield len;
            }
            case "append" -> {
                Object result = SchemeNil.INSTANCE;
                for (int i = args.size() - 1; i >= 0; i--) {
                    Object lst = args.get(i);
                    if (lst instanceof SchemeNil) continue;
                    if (i == args.size() - 1) {
                        result = lst;
                    } else {
                        List<Object> elems = new ArrayList<>();
                        Object cur = lst;
                        while (cur instanceof SchemePair p) { elems.add(p.car); cur = p.cdr; }
                        for (int j = elems.size() - 1; j >= 0; j--) result = new SchemePair(elems.get(j), result);
                    }
                }
                yield result;
            }
            case "list-ref" -> {
                Object lst = args.get(0);
                int idx = (int) requireLong(args.get(1));
                for (int i = 0; i < idx; i++) {
                    if (!(lst instanceof SchemePair p)) throw new EvalError("list-ref: index out of range");
                    lst = p.cdr;
                }
                if (!(lst instanceof SchemePair p)) throw new EvalError("list-ref: index out of range");
                yield p.car;
            }
            case "list-tail" -> {
                Object lst = args.get(0);
                int idx = (int) requireLong(args.get(1));
                for (int i = 0; i < idx; i++) {
                    if (!(lst instanceof SchemePair p)) throw new EvalError("list-tail: index out of range");
                    lst = p.cdr;
                }
                yield lst;
            }
            case "list?" -> {
                Object val = args.get(0);
                while (val instanceof SchemePair p) val = p.cdr;
                yield val instanceof SchemeNil;
            }
            case "assoc" -> {
                Object key = args.get(0);
                Object lst = args.get(1);
                while (lst instanceof SchemePair p) {
                    if (p.car instanceof SchemePair entry) {
                        if (schemeEqual(key, entry.car)) yield entry;
                    }
                    lst = p.cdr;
                }
                yield Boolean.FALSE;
            }
            case "map" -> {
                if (args.size() < 2) throw new EvalError("map: expected at least 2 arguments");
                Object proc = args.get(0);
                List<List<Object>> lists = new ArrayList<>();
                for (int i = 1; i < args.size(); i++) {
                    List<Object> elems = new ArrayList<>();
                    Object cur = args.get(i);
                    while (cur instanceof SchemePair p) { elems.add(p.car); cur = p.cdr; }
                    lists.add(elems);
                }
                int len = lists.get(0).size();
                List<Object> results = new ArrayList<>();
                for (int i = 0; i < len; i++) {
                    List<Object> callArgs = new ArrayList<>();
                    for (List<Object> l : lists) callArgs.add(l.get(i));
                    results.add(apply(proc, callArgs));
                }
                Object result = SchemeNil.INSTANCE;
                for (int i = results.size() - 1; i >= 0; i--) result = new SchemePair(results.get(i), result);
                yield result;
            }
            default -> throw new EvalError("unknown list procedure: " + name);
        };
    }

    private Object applyNumericTypeBuiltin(String name, List<Object> args) throws EvalError {
        return switch (name) {
            case "number?" -> isNumber(args.get(0));
            case "integer?" -> {
                Object v = args.get(0);
                if (v instanceof Long) yield true;
                if (v instanceof Double d) yield d == Math.floor(d) && !Double.isInfinite(d);
                yield false;
            }
            case "rational?" -> args.get(0) instanceof Long || args.get(0) instanceof SchemeRational;
            case "exact?" -> args.get(0) instanceof Long || args.get(0) instanceof SchemeRational;
            case "inexact?" -> args.get(0) instanceof Double;
            case "exact->inexact" -> toDouble(args.get(0));
            case "inexact->exact" -> {
                Object v = args.get(0);
                if (v instanceof Long || v instanceof SchemeRational) yield v;
                if (v instanceof Double d) {
                    if (d == Math.floor(d) && !Double.isInfinite(d)) yield d.longValue();
                    long bits = Double.doubleToLongBits(d);
                    long mantissa = bits & 0x000fffffffffffffL;
                    int exponent = (int) ((bits >> 52) & 0x7ffL) - 1023 - 52;
                    mantissa |= 0x0010000000000000L;
                    if ((bits & 0x8000000000000000L) != 0) mantissa = -mantissa;
                    if (exponent >= 0) yield mantissa * (1L << exponent);
                    else yield SchemeRational.make(mantissa, 1L << (-exponent));
                }
                throw new EvalError("inexact->exact: expected number");
            }
            case "numerator" -> {
                Object v = args.get(0);
                if (v instanceof Long l) yield l;
                if (v instanceof SchemeRational r) yield r.numerator;
                throw new EvalError("numerator: expected rational");
            }
            case "denominator" -> {
                Object v = args.get(0);
                if (v instanceof Long) yield 1L;
                if (v instanceof SchemeRational r) yield r.denominator;
                throw new EvalError("denominator: expected rational");
            }
            default -> throw new EvalError("unknown numeric type procedure: " + name);
        };
    }

    private Object applyTypePredicateBuiltin(String name, List<Object> args) {
        return switch (name) {
            case "string?" -> { Object sv = args.get(0); yield sv instanceof String || sv instanceof SchemeString; }
            case "boolean?" -> args.get(0) instanceof Boolean;
            case "pair?" -> args.get(0) instanceof SchemePair;
            case "symbol?" -> args.get(0) instanceof SchemeSymbol;
            case "char?" -> args.get(0) instanceof SchemeChar;
            case "procedure?" -> args.get(0) instanceof SchemeLambda || args.get(0) instanceof SchemeCaseLambda || args.get(0) instanceof BuiltinProcedure;
            default -> false;
        };
    }

    private Object applyIoBuiltin(String name, List<Object> args) {
        switch (name) {
            case "display" -> { if (outputBuffer != null) outputBuffer.append(displayString(args.get(0))); }
            case "write" -> { if (outputBuffer != null) outputBuffer.append(schemeToString(args.get(0))); }
            case "newline" -> { if (outputBuffer != null) outputBuffer.append("\n"); }
            default -> { }
        }
        return VOID;
    }

    private Object applyStringBuiltin(String name, List<Object> args) throws EvalError {
        return switch (name) {
            case "string-append" -> {
                StringBuilder sb = new StringBuilder();
                for (Object arg : args) sb.append(requireString(arg));
                yield "\"" + sb + "\"";
            }
            case "string-length" -> (long) requireString(args.get(0)).length();
            case "substring" -> {
                String s = requireString(args.get(0));
                int start = (int) requireLong(args.get(1));
                int end = args.size() == 3 ? (int) requireLong(args.get(2)) : s.length();
                yield "\"" + s.substring(start, end) + "\"";
            }
            case "string-ref" -> new SchemeChar(requireString(args.get(0)).charAt((int) requireLong(args.get(1))));
            case "string->number" -> {
                try { yield Long.parseLong(requireString(args.get(0))); }
                catch (NumberFormatException e) { yield Boolean.FALSE; }
            }
            case "number->string" -> "\"" + schemeToString(args.get(0)) + "\"";
            case "symbol->string" -> {
                if (!(args.get(0) instanceof SchemeSymbol sym)) throw new EvalError("symbol->string: not a symbol");
                yield "\"" + sym.name() + "\"";
            }
            case "string->symbol" -> new SchemeSymbol(requireString(args.get(0)));
            case "string-copy" -> new SchemeString(requireString(args.get(0)));
            case "string-set!" -> {
                Object target = args.get(0);
                int idx = (int) requireLong(args.get(1));
                Object charVal = args.get(2);
                if (!(charVal instanceof SchemeChar ch)) throw new EvalError("string-set!: expected char");
                if (!(target instanceof SchemeString ss)) throw new EvalError("string-set!: string is immutable");
                ss.setCharAt(idx, ch.value());
                yield VOID;
            }
            case "string=?" -> requireString(args.get(0)).equals(requireString(args.get(1)));
            case "string<?" -> requireString(args.get(0)).compareTo(requireString(args.get(1))) < 0;
            case "string-ci=?" -> requireString(args.get(0)).equalsIgnoreCase(requireString(args.get(1)));
            case "string-upcase" -> "\"" + requireString(args.get(0)).toUpperCase() + "\"";
            case "string-downcase" -> "\"" + requireString(args.get(0)).toLowerCase() + "\"";
            default -> throw new EvalError("unknown string procedure: " + name);
        };
    }

    private Object applyCharBuiltin(String name, List<Object> args) throws EvalError {
        return switch (name) {
            case "char-alphabetic?" -> {
                if (!(args.get(0) instanceof SchemeChar ch)) throw new EvalError("char-alphabetic?: expected char");
                yield Character.isLetter(ch.value());
            }
            case "char-numeric?" -> {
                if (!(args.get(0) instanceof SchemeChar ch)) throw new EvalError("char-numeric?: expected char");
                yield Character.isDigit(ch.value());
            }
            case "char=?" -> {
                if (!(args.get(0) instanceof SchemeChar a) || !(args.get(1) instanceof SchemeChar b))
                    throw new EvalError("char=?: expected chars");
                yield a.value() == b.value();
            }
            case "char<?" -> {
                if (!(args.get(0) instanceof SchemeChar a) || !(args.get(1) instanceof SchemeChar b))
                    throw new EvalError("char<?: expected chars");
                yield a.value() < b.value();
            }
            case "char-upcase" -> {
                if (!(args.get(0) instanceof SchemeChar ch)) throw new EvalError("char-upcase: expected char");
                yield new SchemeChar(Character.toUpperCase(ch.value()));
            }
            case "char-downcase" -> {
                if (!(args.get(0) instanceof SchemeChar ch)) throw new EvalError("char-downcase: expected char");
                yield new SchemeChar(Character.toLowerCase(ch.value()));
            }
            default -> throw new EvalError("unknown char procedure: " + name);
        };
    }

    private Object evalBuiltin(String name, List<Object> args, Environment env) throws EvalError {
        List<Object> evaluated = new ArrayList<>();
        for (Object arg : args) {
            evaluated.add(eval(arg, env));
        }
        return applyBuiltin(name, evaluated);
    }

    private long requireLong(Object val) throws EvalError {
        if (val instanceof Long l) return l;
        if (val instanceof Double d) return d.longValue();
        throw new EvalError("expected integer, got: " + schemeToString(val));
    }

    private String requireString(Object val) throws EvalError {
        if (val instanceof String s) {
            if (s.startsWith("\"") && s.endsWith("\"")) {
                return s.substring(1, s.length() - 1);
            }
            return s;
        }
        if (val instanceof SchemeString ss) {
            return ss.value();
        }
        throw new EvalError("expected string, got: " + schemeToString(val));
    }

    /** display format: strings without quotes, everything else like schemeToString */
    private String displayString(Object val) {
        if (val instanceof String s) {
            if (s.startsWith("\"") && s.endsWith("\"")) {
                return s.substring(1, s.length() - 1);
            }
            return s;
        }
        if (val instanceof SchemeString ss) {
            return ss.value();
        }
        return schemeToString(val);
    }

    private boolean schemeEqual(Object a, Object b) {
        if (a instanceof SchemePair pa && b instanceof SchemePair pb) {
            return schemeEqual(pa.car, pb.car) && schemeEqual(pa.cdr, pb.cdr);
        }
        if (a instanceof SchemeNil && b instanceof SchemeNil) return true;
        if (a instanceof SchemeSymbol sa && b instanceof SchemeSymbol sb) return sa.name().equals(sb.name());
        if (a instanceof SchemeChar ca && b instanceof SchemeChar cb) return ca.value() == cb.value();
        if (isNumber(a) && isNumber(b)) {
            try { return toDouble(a) == toDouble(b); } catch (EvalError e) { return false; }
        }
        if ((a instanceof String || a instanceof SchemeString) && (b instanceof String || b instanceof SchemeString)) {
            try { return requireString(a).equals(requireString(b)); } catch (EvalError e) { return false; }
        }
        if (a == null || b == null) return a == b;
        return a.equals(b);
    }

    private void requireArgCount(String name, List<?> args, int expected) throws EvalError {
        if (args.size() != expected) {
            throw new EvalError(name + ": expected " + expected + " arguments, got " + args.size());
        }
    }

    @SuppressWarnings("unchecked")
    static String schemeToString(Object val) {
        if (val instanceof Long l) return l.toString();
        if (val instanceof Double d) {
            if (d == Math.floor(d) && !Double.isInfinite(d) && Math.abs(d) < 1e15) {
                // Format as e.g. "5.0" not "5"
                return String.valueOf(d);
            }
            return String.valueOf(d);
        }
        if (val instanceof SchemeRational r) return r.toString();
        if (val instanceof Boolean b) return b ? "#t" : "#f";
        if (val instanceof String s) return s;
        if (val instanceof SchemeString ss) return ss.toString();
        if (val instanceof SchemeSymbol sym) return sym.name();
        if (val instanceof SchemeChar ch) return "#\\" + ch.value();
        if (val instanceof SchemeNil) return "()";
        if (val instanceof SchemePair p) {
            StringBuilder sb = new StringBuilder("(");
            sb.append(schemeToString(p.car));
            Object rest = p.cdr;
            while (rest instanceof SchemePair rp) {
                sb.append(" ");
                sb.append(schemeToString(rp.car));
                rest = rp.cdr;
            }
            if (!(rest instanceof SchemeNil)) {
                sb.append(" . ");
                sb.append(schemeToString(rest));
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
        return val.toString();
    }
}
