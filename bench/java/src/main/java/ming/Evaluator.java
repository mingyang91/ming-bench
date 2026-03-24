package ming;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;

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

    // Gensym counter for hygienic macros
    private int gensymCounter = 0;
    private String gensym(String prefix) { return prefix + "$" + (gensymCounter++); }

    // Macro representation
    record SyntaxRule(Object pattern, Object template) {}
    record SyntaxRulesDef(List<String> literals, List<SyntaxRule> rules, Environment defEnv) {}
    record SyntaxCaseTransformer(Lambda transformer, Environment defEnv) {}

    // Syntax context for syntax-case pattern bindings
    record SyntaxContext(Map<String, Object> bindings, Set<String> ellipsisVars,
                         Set<String> patternVars, Environment defEnv) {}
    private final List<SyntaxContext> syntaxContextStack = new ArrayList<>();
    private final List<Environment> macroDefEnvStack = new ArrayList<>();
    private final List<Environment> macroUseEnvStack = new ArrayList<>();

    private static final Set<String> SPECIAL_FORMS = Set.of(
        "if", "let", "let*", "begin", "set!", "define", "lambda", "quote", "cond",
        "and", "or", "not", "define-syntax", "syntax-rules", "define-record-type",
        "letrec", "letrec*", "case", "do", "when", "unless",
        "syntax-case", "syntax", "with-syntax",
        "guard", "raise", "with-exception-handler",
        "call/cc", "call-with-current-continuation", "dynamic-wind", "case-lambda"
    );

    // Output buffer for display/write/newline
    private StringBuilder outputBuffer = new StringBuilder();

    // Continuation support
    private Object pendingCallCCValue = null;
    private boolean hasPendingCallCC = false;
    private Environment replayEnv = null;
    private int currentTopLevelIndex = 0;
    private List<Object> currentBodyExprs = null;
    private Environment currentBodyEnv = null;

    // dynamic-wind support
    record WindEntry(Object inThunk, Object outThunk) {}
    private List<WindEntry> windStack = new ArrayList<>();

    // Frame stack for continuation capture
    private final List<ContinuationFrame> frameStack = new ArrayList<>();

    // Exception handler stack for with-exception-handler
    private List<Object> exceptionHandlerStack = new ArrayList<>();

    // Guard handler stack for TCO-compatible guard
    record GuardHandler(String var, List<?> clauseList, Environment env) {}
    private final List<GuardHandler> guardHandlerStack = new ArrayList<>();

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
        List<Object> exprs = new ArrayList<>();
        while (pos[0] < tokens.size()) {
            exprs.add(parse(tokens, pos));
        }
        Object lastResult = null;
        int i = 0;
        while (i < exprs.size()) {
            try {
                currentTopLevelIndex = i;
                currentBodyExprs = null;
                currentBodyEnv = null;
                frameStack.clear();
                lastResult = eval(exprs.get(i), globalEnv);
                i++;
            } catch (ContinuationInvoked ci) {
                SchemeContinuation k = ci.continuation;
                hasPendingCallCC = true;
                pendingCallCCValue = ci.value;
                replayEnv = (k.bodyEnv != null && k.bodyEnv != globalEnv) ? k.bodyEnv : null;
                i = k.topLevelIndex;
            }
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
        List<Object> exprs = new ArrayList<>();
        while (pos[0] < tokens.size()) {
            exprs.add(parse(tokens, pos));
        }
        Object lastResult = null;
        int i = 0;
        while (i < exprs.size()) {
            try {
                currentTopLevelIndex = i;
                currentBodyExprs = null;
                currentBodyEnv = null;
                frameStack.clear();
                lastResult = eval(exprs.get(i), globalEnv);
                i++;
            } catch (ContinuationInvoked ci) {
                SchemeContinuation k = ci.continuation;
                hasPendingCallCC = true;
                pendingCallCCValue = ci.value;
                replayEnv = (k.bodyEnv != null && k.bodyEnv != globalEnv) ? k.bodyEnv : null;
                i = k.topLevelIndex;
            }
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
                tokens.add(new Token(new SchemeString(sb.toString(), true), startLine, startCol));
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
                    } else if (next == '\'') {
                        tokens.add(new Token("#'", line, startCol));
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
                Object parsed = parseNumber(tok);
                if (parsed != null) {
                    tokens.add(new Token(parsed, line, startCol));
                } else {
                    tokens.add(new Token(tok, line, startCol));
                }
            }
        }
        return tokens;
    }

    private static Object parseNumber(String tok) {
        // Try integer
        try { return Long.parseLong(tok); } catch (NumberFormatException ignored) {}
        // Try rational N/D
        int slash = tok.indexOf('/');
        if (slash > 0 && slash < tok.length() - 1) {
            try {
                long num = Long.parseLong(tok.substring(0, slash));
                long den = Long.parseLong(tok.substring(slash + 1));
                if (den == 0) return null;
                Rational r = Rational.of(num, den);
                if (r.isInteger()) return r.toLong();
                return r;
            } catch (NumberFormatException ignored) {}
        }
        // Try floating point
        try { return Double.parseDouble(tok); } catch (NumberFormatException ignored) {}
        return null;
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
        if (token.value().equals("#'")) {
            pos[0]++;
            Object synExpr = parse(tokens, pos);
            Object rawSynExpr = synExpr instanceof Located loc ? loc.value() : synExpr;
            List<Object> syntaxExpr = new ArrayList<>();
            syntaxExpr.add("syntax");
            syntaxExpr.add(rawSynExpr);
            return new Located(syntaxExpr, token.line(), token.col());
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

    // --- Evaluator (with trampoline TCO) ---

    @SuppressWarnings("unchecked")
    private Object eval(Object expr, Environment env) throws EvalError {
        int guardBase = guardHandlerStack.size();
        try {
        while (true) {  // trampoline loop for TCO
        try {
        if (expr instanceof Located loc) {
            currentLine = loc.line();
            currentCol = loc.col();
            expr = loc.value();
            continue;
        }

        if (expr instanceof MacroExpansion me) {
            expr = me.form();
            env = me.env();
            continue;
        }

        if (expr instanceof Long || expr instanceof Double || expr instanceof Rational || expr instanceof Boolean || expr instanceof SchemeString || expr instanceof SchemeChar) {
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
                    case "set!" -> {
                        if (list.size() != 3) throw error("set!: bad syntax");
                        Object setTarget = list.get(1);
                        if (setTarget instanceof Located loc) setTarget = loc.value();
                        if (!(setTarget instanceof String setName)) throw error("set!: not a variable");
                        Object setVal = eval(list.get(2), env);
                        try {
                            env.set(setName, setVal);
                        } catch (EvalError e) {
                            throw error("set!: unbound variable: " + setName);
                        }
                        return VOID;
                    }
                    case "define" -> { return evalDefine(list, env); }
                    case "if" -> {
                        // TCO: tail-call the chosen branch
                        if (list.size() < 3 || list.size() > 4) throw error("if: bad syntax");
                        Object cond = eval(list.get(1), env);
                        if (!Boolean.FALSE.equals(cond)) {
                            expr = list.get(2);
                            continue;
                        } else if (list.size() == 4) {
                            expr = list.get(3);
                            continue;
                        }
                        return VOID;
                    }
                    case "quote" -> {
                        if (list.size() != 2) throw error("quote: expected 1 argument");
                        return listToConsCells(list.get(1));
                    }
                    case "lambda" -> { return evalLambda(list, env); }
                    case "begin" -> {
                        // TCO: eval all but last, tail-call last
                        if (list.size() <= 1) return VOID;
                        List<?> beginBody = list.subList(1, list.size());
                        currentBodyExprs = new ArrayList<>(beginBody);
                        currentBodyEnv = env;
                        if (beginBody.size() > 1) {
                            ContinuationFrame _f = new ContinuationFrame(beginBody, env, 0);
                            frameStack.add(_f);
                            for (int i = 0; i < beginBody.size() - 1; i++) {
                                _f.currentIdx = i;
                                eval((Object) beginBody.get(i), env);
                            }
                            frameStack.remove(frameStack.size() - 1);
                        }
                        expr = list.get(list.size() - 1);
                        continue;
                    }
                    case "cond" -> {
                        // TCO: tail-call the last expr in matched clause
                        Object condResult = VOID;
                        boolean matched = false;
                        for (int i = 1; i < list.size(); i++) {
                            Object clause = list.get(i);
                            if (clause instanceof Located loc) clause = loc.value();
                            if (!(clause instanceof List<?> cl) || cl.isEmpty()) {
                                throw error("cond: bad clause");
                            }
                            Object test = cl.get(0);
                            Object rawTest = test instanceof Located loc ? loc.value() : test;
                            if (rawTest instanceof String s && s.equals("else")) {
                                for (int j = 1; j < cl.size() - 1; j++) {
                                    eval(cl.get(j), env);
                                }
                                if (cl.size() > 1) {
                                    expr = cl.get(cl.size() - 1);
                                    matched = true;
                                    break;
                                }
                                return VOID;
                            }
                            Object testVal = eval(test, env);
                            if (!Boolean.FALSE.equals(testVal)) {
                                if (cl.size() == 1) return testVal;
                                for (int j = 1; j < cl.size() - 1; j++) {
                                    eval(cl.get(j), env);
                                }
                                expr = cl.get(cl.size() - 1);
                                matched = true;
                                break;
                            }
                        }
                        if (matched) continue;
                        return VOID;
                    }
                    case "let" -> {
                        // TCO: tail-call the last body expr
                        if (list.size() < 3) throw error("let: bad syntax");
                        Object second = list.get(1);
                        if (second instanceof Located loc) second = loc.value();

                        if (second instanceof String name) {
                            // Named let
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
                            Lambda lambda = new Lambda(params, null, body, letEnv);
                            letEnv.define(name, lambda);
                            Environment callEnv = new Environment(letEnv);
                            for (int i = 0; i < params.size(); i++) {
                                callEnv.define(params.get(i), inits.get(i));
                            }
                            if (body.size() > 1) {
                                ContinuationFrame _f = new ContinuationFrame(body, callEnv, 0);
                                frameStack.add(_f);
                                for (int i = 0; i < body.size() - 1; i++) {
                                    _f.currentIdx = i;
                                    eval(body.get(i), callEnv);
                                }
                                frameStack.remove(frameStack.size() - 1);
                            }
                            expr = body.get(body.size() - 1);
                            env = callEnv;
                            continue;
                        }

                        // Regular let
                        if (!(second instanceof List<?> bindings)) throw error("let: bad bindings");
                        Environment letEnv;
                        if (replayEnv != null && replayEnv.getParent() == env) {
                            letEnv = replayEnv;
                            replayEnv = null;
                        } else {
                            letEnv = new Environment(env);
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
                        }
                        List<?> letBody = list.subList(2, list.size());
                        currentBodyExprs = new ArrayList<>(letBody);
                        currentBodyEnv = letEnv;
                        if (letBody.size() > 1) {
                            ContinuationFrame _f = new ContinuationFrame(letBody, letEnv, 0);
                            frameStack.add(_f);
                            for (int i = 0; i < letBody.size() - 1; i++) {
                                _f.currentIdx = i;
                                eval((Object) letBody.get(i), letEnv);
                            }
                            frameStack.remove(frameStack.size() - 1);
                        }
                        expr = list.get(list.size() - 1);
                        env = letEnv;
                        continue;
                    }
                    case "and" -> {
                        // TCO: tail-call the last expression
                        if (list.size() == 1) return Boolean.TRUE;
                        for (int i = 1; i < list.size() - 1; i++) {
                            Object result = eval(list.get(i), env);
                            if (Boolean.FALSE.equals(result)) return Boolean.FALSE;
                        }
                        expr = list.get(list.size() - 1);
                        continue;
                    }
                    case "or" -> {
                        // TCO: tail-call the last expression
                        if (list.size() == 1) return Boolean.FALSE;
                        for (int i = 1; i < list.size() - 1; i++) {
                            Object result = eval(list.get(i), env);
                            if (!Boolean.FALSE.equals(result)) return result;
                        }
                        expr = list.get(list.size() - 1);
                        continue;
                    }
                    case "not" -> {
                        checkArgs(list, 1, "not");
                        Object val = eval(list.get(1), env);
                        return Boolean.FALSE.equals(val) ? Boolean.TRUE : Boolean.FALSE;
                    }
                    case "define-syntax" -> { return evalDefineSyntax(list, env); }
                    case "syntax-case" -> {
                        return evalSyntaxCaseForm(list, env);
                    }
                    case "syntax" -> {
                        return evalSyntaxForm(list);
                    }
                    case "with-syntax" -> {
                        return evalWithSyntax(list, env);
                    }
                    case "define-record-type" -> { return evalDefineRecordType(list, env); }
                    case "case-lambda" -> { return evalCaseLambda(list, env); }
                    case "letrec" -> {
                        // TCO: tail-call last body expr
                        Object letrecResult = evalLetrecTco(list, env, false);
                        if (letrecResult instanceof TailCall tc) {
                            expr = tc.expr; env = tc.env; continue;
                        }
                        return letrecResult;
                    }
                    case "letrec*" -> {
                        Object letrecResult = evalLetrecTco(list, env, true);
                        if (letrecResult instanceof TailCall tc) {
                            expr = tc.expr; env = tc.env; continue;
                        }
                        return letrecResult;
                    }
                    case "let*" -> {
                        if (list.size() < 3) throw error("let*: bad syntax");
                        Object bindingsObj = list.get(1);
                        if (bindingsObj instanceof Located loc) bindingsObj = loc.value();
                        if (!(bindingsObj instanceof List<?> bindings)) throw error("let*: bad bindings");
                        Environment letStarEnv = new Environment(env);
                        for (Object b : bindings) {
                            if (b instanceof Located loc) b = loc.value();
                            if (!(b instanceof List<?> binding) || binding.size() != 2)
                                throw error("let*: bad binding");
                            Object varObj = binding.get(0);
                            if (varObj instanceof Located loc) varObj = loc.value();
                            if (!(varObj instanceof String varName))
                                throw error("let*: bad binding variable");
                            Object val = eval(binding.get(1), letStarEnv);
                            letStarEnv.define(varName, val);
                        }
                        List<?> letStarBody = list.subList(2, list.size());
                        if (letStarBody.size() > 1) {
                            ContinuationFrame _f = new ContinuationFrame(letStarBody, letStarEnv, 0);
                            frameStack.add(_f);
                            for (int i = 0; i < letStarBody.size() - 1; i++) {
                                _f.currentIdx = i;
                                eval((Object) letStarBody.get(i), letStarEnv);
                            }
                            frameStack.remove(frameStack.size() - 1);
                        }
                        expr = list.get(list.size() - 1);
                        env = letStarEnv;
                        continue;
                    }
                    case "when" -> {
                        if (list.size() < 3) throw error("when: bad syntax");
                        Object test = eval(list.get(1), env);
                        if (!Boolean.FALSE.equals(test)) {
                            List<?> whenBody = list.subList(2, list.size());
                            if (whenBody.size() > 1) {
                                ContinuationFrame _f = new ContinuationFrame(whenBody, env, 0);
                                frameStack.add(_f);
                                for (int i = 0; i < whenBody.size() - 1; i++) {
                                    _f.currentIdx = i;
                                    eval((Object) whenBody.get(i), env);
                                }
                                frameStack.remove(frameStack.size() - 1);
                            }
                            expr = list.get(list.size() - 1);
                            continue;
                        }
                        return VOID;
                    }
                    case "unless" -> {
                        if (list.size() < 3) throw error("unless: bad syntax");
                        Object test = eval(list.get(1), env);
                        if (Boolean.FALSE.equals(test)) {
                            List<?> unlessBody = list.subList(2, list.size());
                            if (unlessBody.size() > 1) {
                                ContinuationFrame _f = new ContinuationFrame(unlessBody, env, 0);
                                frameStack.add(_f);
                                for (int i = 0; i < unlessBody.size() - 1; i++) {
                                    _f.currentIdx = i;
                                    eval((Object) unlessBody.get(i), env);
                                }
                                frameStack.remove(frameStack.size() - 1);
                            }
                            expr = list.get(list.size() - 1);
                            continue;
                        }
                        return VOID;
                    }
                    case "case" -> { return evalCase(list, env); }
                    case "do" -> { return evalDo(list, env); }
                    case "dynamic-wind" -> {
                        if (list.size() != 4) throw error("dynamic-wind: expected 3 arguments");
                        Object dwIn = eval(list.get(1), env);
                        Object dwBody = eval(list.get(2), env);
                        Object dwOut = eval(list.get(3), env);
                        // Protect replay state from in/out thunk side effects
                        var savedReplay = replayEnv;
                        replayEnv = null;
                        apply(dwIn, List.of());
                        replayEnv = savedReplay;
                        WindEntry wEntry = new WindEntry(dwIn, dwOut);
                        windStack.add(wEntry);
                        Object dwResult;
                        try {
                            dwResult = apply(dwBody, List.of());
                        } catch (ContinuationInvoked ci) {
                            windStack.remove(windStack.size() - 1);
                            apply(dwOut, List.of());
                            throw ci;
                        } catch (SchemeRaise sr) {
                            windStack.remove(windStack.size() - 1);
                            apply(dwOut, List.of());
                            throw sr;
                        }
                        windStack.remove(windStack.size() - 1);
                        apply(dwOut, List.of());
                        return dwResult;
                    }
                    case "guard" -> {
                        // (guard (var clause ...) body ...)
                        if (list.size() < 3) throw error("guard: bad syntax");
                        Object clauseSpec = list.get(1);
                        if (clauseSpec instanceof Located loc) clauseSpec = loc.value();
                        if (!(clauseSpec instanceof List<?> clauseList) || clauseList.isEmpty())
                            throw error("guard: bad syntax");
                        Object varObj = clauseList.get(0);
                        if (varObj instanceof Located vl) varObj = vl.value();
                        if (!(varObj instanceof String)) throw error("guard: variable must be a symbol");
                        String guardVar = (String) varObj;
                        // Evaluate non-tail body expressions with try/catch
                        try {
                            for (int bi = 2; bi < list.size() - 1; bi++) {
                                eval(list.get(bi), env);
                            }
                        } catch (SchemeRaise sr) {
                            return evalGuardClauses(guardVar, clauseList, sr.value, env);
                        }
                        // Push handler for tail expression (enables TCO through guard)
                        guardHandlerStack.add(new GuardHandler(guardVar, clauseList, env));
                        expr = list.get(list.size() - 1);
                        continue;
                    }
                    case "with-exception-handler" -> {
                        if (list.size() != 3) throw error("with-exception-handler: expected 2 arguments");
                        Object handler = eval(list.get(1), env);
                        Object thunk = eval(list.get(2), env);
                        exceptionHandlerStack.add(handler);
                        try {
                            Object result = apply(thunk, List.of());
                            exceptionHandlerStack.remove(exceptionHandlerStack.size() - 1);
                            return result;
                        } catch (SchemeRaise sr) {
                            exceptionHandlerStack.remove(exceptionHandlerStack.size() - 1);
                            throw sr;
                        }
                    }
                    case "call/cc", "call-with-current-continuation" -> {
                        if (list.size() != 2) throw error("call/cc: expected 1 argument");
                        if (hasPendingCallCC) {
                            hasPendingCallCC = false;
                            Object val = pendingCallCCValue;
                            pendingCallCCValue = null;
                            replayEnv = null;
                            return val;
                        }
                        Object ccProc = eval(list.get(1), env);
                        SchemeContinuation k = new SchemeContinuation();
                        k.topLevelIndex = currentTopLevelIndex;
                        k.bodyExprs = currentBodyExprs;
                        k.bodyEnv = currentBodyEnv;
                        k.savedWindStack = new ArrayList<>(windStack);
                        k.savedFrameStack = copyFrameStack();
                        try {
                            Object ccResult = apply(ccProc, List.of(k));
                            k.active = false;
                            return ccResult;
                        } catch (ContinuationInvoked ci) {
                            k.active = false;
                            if (ci.continuation == k) return ci.value;
                            throw ci;
                        }
                    }
                }
                // Check for macro usage
                try {
                    Object headVal = env.lookup(op);
                    if (headVal instanceof SyntaxRulesDef sr) {
                        Object expanded = expandMacro(sr, list, env);
                        expr = expanded;
                        continue;
                    }
                    if (headVal instanceof SyntaxCaseTransformer sct) {
                        Object expanded = expandSyntaxCaseTransformer(sct, list, env);
                        expr = expanded;
                        continue;
                    }
                } catch (EvalError ignored) {}
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

            // TCO: for Lambda/CaseLambda, set up env and tail-call last body expr
            if (proc instanceof Lambda lambda) {
                env = applyLambdaEnv(lambda, args);
                currentBodyExprs = lambda.body;
                currentBodyEnv = env;
                if (lambda.body.size() > 1) {
                    ContinuationFrame _f = new ContinuationFrame(lambda.body, env, 0);
                    frameStack.add(_f);
                    for (int i = 0; i < lambda.body.size() - 1; i++) {
                        _f.currentIdx = i;
                        eval(lambda.body.get(i), env);
                    }
                    frameStack.remove(frameStack.size() - 1);
                }
                expr = lambda.body.get(lambda.body.size() - 1);
                continue;
            }
            if (proc instanceof CaseLambda cl) {
                Lambda matched = null;
                for (Lambda clause : cl.clauses) {
                    if (clause.restParam != null) {
                        if (args.size() >= clause.params.size()) { matched = clause; break; }
                    } else {
                        if (args.size() == clause.params.size()) { matched = clause; break; }
                    }
                }
                if (matched == null) throw error("case-lambda: no matching clause for " + args.size() + " arguments");
                env = applyLambdaEnv(matched, args);
                if (matched.body.size() > 1) {
                    ContinuationFrame _f = new ContinuationFrame(matched.body, env, 0);
                    frameStack.add(_f);
                    for (int i = 0; i < matched.body.size() - 1; i++) {
                        _f.currentIdx = i;
                        eval(matched.body.get(i), env);
                    }
                    frameStack.remove(frameStack.size() - 1);
                }
                expr = matched.body.get(matched.body.size() - 1);
                continue;
            }
            if (proc instanceof BuiltinProc bp) {
                return bp.apply(args);
            }
            if (proc instanceof SchemeContinuation cont) {
                Object val;
                if (args.size() == 1) {
                    val = args.get(0);
                } else {
                    val = new SchemeValues(new ArrayList<>(args));
                }
                if (cont.active) {
                    // Escape: call/cc is on the stack
                    throw new ContinuationInvoked(cont, val);
                }
                // Re-entrant: check if replay is possible
                if (cont.bodyEnv == null || cont.bodyEnv == globalEnv
                        || cont.bodyEnv.getParent() == globalEnv) {
                    throw new ContinuationInvoked(cont, val);
                }
                // Deep nested continuation - can't replay, return value
                return val;
            }
            throw error("not a procedure: " + schemeToString(proc));
        }
        throw error("cannot evaluate: " + expr);
        } catch (SchemeRaise sr) {
            if (guardHandlerStack.size() > guardBase) {
                GuardHandler gh = guardHandlerStack.remove(guardHandlerStack.size() - 1);
                // Clean up any remaining handlers from this eval depth
                while (guardHandlerStack.size() > guardBase)
                    guardHandlerStack.remove(guardHandlerStack.size() - 1);
                return evalGuardClauses(gh.var, gh.clauseList, sr.value, gh.env);
            }
            throw sr;
        }
        } // end trampoline while
        } finally {
            // Clean up any guard handlers pushed during this eval call
            while (guardHandlerStack.size() > guardBase)
                guardHandlerStack.remove(guardHandlerStack.size() - 1);
        }
    }

    // Helper: set up a Lambda call environment (for TCO in eval loop)
    private Environment applyLambdaEnv(Lambda lambda, List<Object> args) throws EvalError {
        if (lambda.restParam != null) {
            if (args.size() < lambda.params.size()) {
                throw error("wrong number of arguments: expected at least " + lambda.params.size() + ", got " + args.size());
            }
        } else {
            if (args.size() != lambda.params.size()) {
                throw error("wrong number of arguments: expected " + lambda.params.size() + ", got " + args.size());
            }
        }
        if (replayEnv != null && replayEnv.getParent() == lambda.closure) {
            Environment restored = replayEnv;
            replayEnv = null;
            return restored;
        }
        Environment callEnv = new Environment(lambda.closure);
        for (int i = 0; i < lambda.params.size(); i++) {
            callEnv.define(lambda.params.get(i), args.get(i));
        }
        if (lambda.restParam != null) {
            Object rest = NIL;
            for (int i = args.size() - 1; i >= lambda.params.size(); i--) {
                rest = new Pair(args.get(i), rest);
            }
            callEnv.define(lambda.restParam, rest);
        }
        return callEnv;
    }

    // Evaluate guard clauses (cond-like) against a raised exception
    private Object evalGuardClauses(String var, List<?> clauseList, Object raised, Environment env) throws EvalError {
        Environment guardEnv = new Environment(env);
        guardEnv.define(var, raised);
        for (int ci = 1; ci < clauseList.size(); ci++) {
            Object clause = clauseList.get(ci);
            if (clause instanceof Located cl) clause = cl.value();
            if (!(clause instanceof List<?> clist) || clist.isEmpty())
                throw error("guard: bad clause");
            Object test = clist.get(0);
            if (test instanceof Located tl) test = tl.value();
            if ("else".equals(test) || (test instanceof String && "else".equals(test))) {
                Object result = VOID;
                for (int ei = 1; ei < clist.size(); ei++) {
                    result = eval(clist.get(ei), guardEnv);
                }
                return result;
            }
            Object testResult = eval(clist.get(0), guardEnv);
            if (!Boolean.FALSE.equals(testResult)) {
                if (clist.size() == 1) return testResult;
                Object result = VOID;
                for (int ei = 1; ei < clist.size(); ei++) {
                    result = eval(clist.get(ei), guardEnv);
                }
                return result;
            }
        }
        // No clause matched — re-raise
        throw new SchemeRaise(raised);
    }

    // Marker for tail call from helper methods
    private record TailCall(Object expr, Environment env) {}

    // Multiple values wrapper
    static record SchemeValues(List<Object> values) {}

    // Continuation frame for precise resumption
    static class ContinuationFrame {
        final List<?> bodyExprs;
        final Environment env;
        int currentIdx;

        ContinuationFrame(List<?> bodyExprs, Environment env, int currentIdx) {
            this.bodyExprs = bodyExprs;
            this.env = env;
            this.currentIdx = currentIdx;
        }

        ContinuationFrame copy() {
            return new ContinuationFrame(bodyExprs, env, currentIdx);
        }
    }

    // Continuation support for call/cc
    static class SchemeContinuation {
        List<Object> bodyExprs;
        Environment bodyEnv;
        int topLevelIndex;
        List<WindEntry> savedWindStack;
        List<ContinuationFrame> savedFrameStack;
        boolean active = true; // true while call/cc is on the stack
    }

    static class ContinuationInvoked extends RuntimeException {
        final SchemeContinuation continuation;
        final Object value;
        ContinuationInvoked(SchemeContinuation k, Object v) {
            super(null, null, true, false);
            this.continuation = k;
            this.value = v;
        }
    }

    // Exception support for raise/guard/with-exception-handler
    static class SchemeRaise extends RuntimeException {
        final Object value;
        SchemeRaise(Object v) {
            super(null, null, true, false);
            this.value = v;
        }
    }

    private void doWindTransitions(List<WindEntry> targetStack) throws EvalError {
        int commonLen = 0;
        int minLen = Math.min(windStack.size(), targetStack.size());
        while (commonLen < minLen && windStack.get(commonLen) == targetStack.get(commonLen)) {
            commonLen++;
        }
        // Unwind: call out-thunks from top down to common prefix
        for (int i = windStack.size() - 1; i >= commonLen; i--) {
            WindEntry entry = windStack.remove(i);
            apply(entry.outThunk(), List.of());
        }
        // Rewind: call in-thunks from common prefix up to target
        for (int i = commonLen; i < targetStack.size(); i++) {
            WindEntry entry = targetStack.get(i);
            windStack.add(entry);
            apply(entry.inThunk(), List.of());
        }
    }

    private List<ContinuationFrame> copyFrameStack() {
        List<ContinuationFrame> copy = new ArrayList<>(frameStack.size());
        for (ContinuationFrame f : frameStack) copy.add(f.copy());
        return copy;
    }

    @SuppressWarnings("unchecked")
    private Object resumeContinuation(SchemeContinuation k, Object value) throws EvalError {
        // Wind transitions
        doWindTransitions(k.savedWindStack);

        List<ContinuationFrame> frames = k.savedFrameStack;

        hasPendingCallCC = true;
        pendingCallCCValue = value;
        replayEnv = null;

        Object result = VOID;

        for (int f = frames.size() - 1; f >= 0; f--) {
            ContinuationFrame frame = frames.get(f);
            int startIdx;

            if (f == frames.size() - 1) {
                // Innermost frame: start from the call/cc expression
                startIdx = frame.currentIdx;
            } else {
                // Outer frame: the call at frame.currentIdx completed via inner frames
                startIdx = frame.currentIdx + 1;
            }

            // Set up frame stack for potential nested continuation captures
            frameStack.clear();
            for (int i = 0; i < f; i++) {
                frameStack.add(frames.get(i).copy());
            }

            // Evaluate remaining body expressions
            for (int i = startIdx; i < frame.bodyExprs.size(); i++) {
                ContinuationFrame trackFrame = new ContinuationFrame(frame.bodyExprs, frame.env, i);
                frameStack.add(trackFrame);
                result = eval((Object) frame.bodyExprs.get(i), frame.env);
                if (!frameStack.isEmpty()) {
                    frameStack.remove(frameStack.size() - 1);
                }
            }
        }

        return result;
    }

    // letrec/letrec* with TCO support - returns TailCall for last body expr
    @SuppressWarnings("unchecked")
    private Object evalLetrecTco(List<?> list, Environment env, boolean star) throws EvalError {
        if (list.size() < 3) throw error("letrec: bad syntax");
        Object bindingsObj = list.get(1);
        if (bindingsObj instanceof Located loc) bindingsObj = loc.value();
        if (!(bindingsObj instanceof List<?> bindings)) throw error("letrec: bad bindings");
        Environment letEnv = new Environment(env);
        List<String> names = new ArrayList<>();
        List<Object> initExprs = new ArrayList<>();
        for (Object b : bindings) {
            if (b instanceof Located loc) b = loc.value();
            if (!(b instanceof List<?> binding) || binding.size() != 2)
                throw error("letrec: bad binding");
            Object varObj = binding.get(0);
            if (varObj instanceof Located loc) varObj = loc.value();
            if (!(varObj instanceof String varName)) throw error("letrec: bad binding variable");
            names.add(varName);
            initExprs.add(binding.get(1));
            letEnv.define(varName, VOID);
        }
        if (star) {
            for (int i = 0; i < names.size(); i++) {
                Object val = eval(initExprs.get(i), letEnv);
                letEnv.define(names.get(i), val);
            }
        } else {
            List<Object> vals = new ArrayList<>();
            for (int i = 0; i < names.size(); i++) {
                vals.add(eval(initExprs.get(i), letEnv));
            }
            for (int i = 0; i < names.size(); i++) {
                letEnv.define(names.get(i), vals.get(i));
            }
        }
        List<?> letrecBody = list.subList(2, list.size());
        if (letrecBody.size() > 1) {
            ContinuationFrame _f = new ContinuationFrame(letrecBody, letEnv, 0);
            frameStack.add(_f);
            for (int i = 0; i < letrecBody.size() - 1; i++) {
                _f.currentIdx = i;
                eval((Object) letrecBody.get(i), letEnv);
            }
            frameStack.remove(frameStack.size() - 1);
        }
        return new TailCall(list.get(list.size() - 1), letEnv);
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
            String restParam = null;
            for (int i = 1; i < sig.size(); i++) {
                Object p = sig.get(i);
                if (p instanceof Located loc) p = loc.value();
                if (!(p instanceof String s)) throw error("define: bad parameter");
                if (s.equals(".")) {
                    if (i + 1 >= sig.size()) throw error("define: missing rest parameter after dot");
                    Object rp = sig.get(i + 1);
                    if (rp instanceof Located loc) rp = loc.value();
                    if (!(rp instanceof String rest)) throw error("define: bad rest parameter");
                    restParam = rest;
                    break;
                }
                params.add(s);
            }
            List<Object> body = new ArrayList<>();
            for (int i = 2; i < list.size(); i++) {
                body.add(list.get(i));
            }
            Lambda lambda = new Lambda(params, restParam, body, env);
            env.define(name, lambda);
            return VOID;
        }
        throw error("define: bad syntax");
    }

    private Object evalLambda(List<?> list, Environment env) throws EvalError {
        if (list.size() < 3) throw error("lambda: bad syntax");
        Object paramList = list.get(1);
        if (paramList instanceof Located loc) paramList = loc.value();
        if (!(paramList instanceof List<?> plist)) throw error("lambda: bad parameters");
        List<String> params = new ArrayList<>();
        String restParam = null;
        for (int i = 0; i < plist.size(); i++) {
            Object p = plist.get(i);
            if (p instanceof Located loc) p = loc.value();
            if (!(p instanceof String s)) throw error("lambda: bad parameter");
            if (s.equals(".")) {
                if (i + 1 >= plist.size()) throw error("lambda: missing rest parameter after dot");
                Object rp = plist.get(i + 1);
                if (rp instanceof Located loc) rp = loc.value();
                if (!(rp instanceof String rest)) throw error("lambda: bad rest parameter");
                restParam = rest;
                break;
            }
            params.add(s);
        }
        List<Object> body = new ArrayList<>();
        for (int i = 2; i < list.size(); i++) {
            body.add(list.get(i));
        }
        return new Lambda(params, restParam, body, env);
    }

    private Object evalCaseLambda(List<?> list, Environment env) throws EvalError {
        if (list.size() < 2) throw error("case-lambda: bad syntax");
        List<Lambda> clauses = new ArrayList<>();
        for (int i = 1; i < list.size(); i++) {
            Object clause = list.get(i);
            if (clause instanceof Located loc) clause = loc.value();
            if (!(clause instanceof List<?> cl) || cl.size() < 2) {
                throw error("case-lambda: bad clause");
            }
            Object paramList = cl.get(0);
            if (paramList instanceof Located loc) paramList = loc.value();
            if (!(paramList instanceof List<?> plist)) throw error("case-lambda: bad parameters");
            List<String> params = new ArrayList<>();
            String restParam = null;
            for (int j = 0; j < plist.size(); j++) {
                Object p = plist.get(j);
                if (p instanceof Located loc) p = loc.value();
                if (!(p instanceof String s)) throw error("case-lambda: bad parameter");
                if (s.equals(".")) {
                    if (j + 1 >= plist.size()) throw error("case-lambda: missing rest parameter after dot");
                    Object rp = plist.get(j + 1);
                    if (rp instanceof Located loc) rp = loc.value();
                    if (!(rp instanceof String rest)) throw error("case-lambda: bad rest parameter");
                    restParam = rest;
                    break;
                }
                params.add(s);
            }
            List<Object> body = new ArrayList<>();
            for (int j = 1; j < cl.size(); j++) {
                body.add(cl.get(j));
            }
            clauses.add(new Lambda(params, restParam, body, env));
        }
        return new CaseLambda(clauses);
    }

    @SuppressWarnings("unchecked")
    private Object evalCase(List<?> list, Environment env) throws EvalError {
        if (list.size() < 2) throw error("case: bad syntax");
        Object key = eval(list.get(1), env);
        for (int i = 2; i < list.size(); i++) {
            Object clause = list.get(i);
            if (clause instanceof Located loc) clause = loc.value();
            if (!(clause instanceof List<?> cl) || cl.isEmpty())
                throw error("case: bad clause");
            Object datums = cl.get(0);
            if (datums instanceof Located loc) datums = loc.value();
            // else clause
            if (datums instanceof String s && s.equals("else")) {
                Object result = VOID;
                for (int j = 1; j < cl.size(); j++) {
                    result = eval(cl.get(j), env);
                }
                return result;
            }
            // datums is a list of values
            if (!(datums instanceof List<?> datumList)) throw error("case: bad clause");
            for (Object d : datumList) {
                Object datum = d;
                if (datum instanceof Located loc) datum = loc.value();
                // Convert to cons-cell data for quoted symbols etc
                datum = listToConsCells(datum);
                if (eqv(key, datum)) {
                    Object result = VOID;
                    for (int j = 1; j < cl.size(); j++) {
                        result = eval(cl.get(j), env);
                    }
                    return result;
                }
            }
        }
        return VOID;
    }

    private boolean eqv(Object a, Object b) {
        if (a == b) return true;
        if (a instanceof Long && b instanceof Long) return a.equals(b);
        if (a instanceof Rational && b instanceof Rational) return a.equals(b);
        if (a instanceof Double && b instanceof Double) return a.equals(b);
        if (a instanceof Boolean && b instanceof Boolean) return a.equals(b);
        if (a instanceof SchemeChar && b instanceof SchemeChar) return a.equals(b);
        if (a instanceof String && b instanceof String) return a.equals(b);
        return false;
    }

    @SuppressWarnings("unchecked")
    private Object evalDo(List<?> list, Environment env) throws EvalError {
        // (do ((var init step) ...) (test expr ...) body ...)
        if (list.size() < 3) throw error("do: bad syntax");
        Object varsObj = list.get(1);
        if (varsObj instanceof Located loc) varsObj = loc.value();
        if (!(varsObj instanceof List<?> varSpecs)) throw error("do: bad variable specs");

        Object testObj = list.get(2);
        if (testObj instanceof Located loc) testObj = loc.value();
        if (!(testObj instanceof List<?> testClause) || testClause.isEmpty())
            throw error("do: bad test clause");

        // Parse variable specs
        List<String> varNames = new ArrayList<>();
        List<Object> stepExprs = new ArrayList<>(); // null means no step
        Environment doEnv = new Environment(env);
        for (Object spec : varSpecs) {
            if (spec instanceof Located loc) spec = loc.value();
            if (!(spec instanceof List<?> s) || s.size() < 2 || s.size() > 3)
                throw error("do: bad variable spec");
            Object nameObj = s.get(0);
            if (nameObj instanceof Located loc) nameObj = loc.value();
            if (!(nameObj instanceof String name)) throw error("do: bad variable name");
            varNames.add(name);
            Object initVal = eval(s.get(1), env);
            doEnv.define(name, initVal);
            if (s.size() == 3) {
                stepExprs.add(s.get(2));
            } else {
                stepExprs.add(null);
            }
        }

        // Iteration
        while (true) {
            // Evaluate test
            Object testVal = eval(testClause.get(0), doEnv);
            if (!Boolean.FALSE.equals(testVal)) {
                // Test passed - evaluate result expressions
                Object result = VOID;
                for (int j = 1; j < testClause.size(); j++) {
                    result = eval(testClause.get(j), doEnv);
                }
                return result;
            }
            // Execute body
            for (int j = 3; j < list.size(); j++) {
                eval(list.get(j), doEnv);
            }
            // Parallel step: evaluate all steps with current values, then update
            List<Object> newVals = new ArrayList<>();
            for (int j = 0; j < varNames.size(); j++) {
                Object stepExpr = stepExprs.get(j);
                if (stepExpr != null) {
                    newVals.add(eval(stepExpr, doEnv));
                } else {
                    newVals.add(doEnv.lookup(varNames.get(j)));
                }
            }
            for (int j = 0; j < varNames.size(); j++) {
                doEnv.define(varNames.get(j), newVals.get(j));
            }
        }
    }

    // --- Macros ---

    @SuppressWarnings("unchecked")
    private Object evalDefineRecordType(List<?> list, Environment env) throws EvalError {
        // (define-record-type <name> (constructor field...) predicate (field accessor) ...)
        if (list.size() < 4) throw error("define-record-type: bad syntax");

        // Type name
        Object typeNameObj = list.get(1);
        if (typeNameObj instanceof Located loc) typeNameObj = loc.value();
        if (!(typeNameObj instanceof String typeName)) throw error("define-record-type: expected type name");

        // Constructor: (constructor-name field ...)
        Object ctorObj = list.get(2);
        if (ctorObj instanceof Located loc) ctorObj = loc.value();
        if (!(ctorObj instanceof List<?> ctorList) || ctorList.size() < 1)
            throw error("define-record-type: expected constructor");
        Object ctorNameObj = ctorList.get(0);
        if (ctorNameObj instanceof Located loc) ctorNameObj = loc.value();
        String ctorName = (String) ctorNameObj;
        List<String> ctorFields = new ArrayList<>();
        for (int i = 1; i < ctorList.size(); i++) {
            Object f = ctorList.get(i);
            if (f instanceof Located loc) f = loc.value();
            ctorFields.add((String) f);
        }

        // Predicate name
        Object predObj = list.get(3);
        if (predObj instanceof Located loc) predObj = loc.value();
        String predName = (String) predObj;

        // Field accessors: (field-name accessor-name) ...
        Map<String, String> fieldAccessors = new HashMap<>(); // accessor-name -> field-name
        for (int i = 4; i < list.size(); i++) {
            Object fieldSpec = list.get(i);
            if (fieldSpec instanceof Located loc) fieldSpec = loc.value();
            List<?> spec = (List<?>) fieldSpec;
            Object fname = spec.get(0);
            if (fname instanceof Located loc) fname = loc.value();
            Object aname = spec.get(1);
            if (aname instanceof Located loc) aname = loc.value();
            fieldAccessors.put((String) aname, (String) fname);
        }

        // Define constructor
        env.define(ctorName, (BuiltinProc) args -> {
            if (args.size() != ctorFields.size())
                throw new EvalError("1:1 " + ctorName + ": expected " + ctorFields.size() + " args");
            Map<String, Object> fields = new HashMap<>();
            for (int i = 0; i < ctorFields.size(); i++) {
                fields.put(ctorFields.get(i), args.get(i));
            }
            return new SchemeRecord(typeName, fields);
        });

        // Define predicate
        env.define(predName, (BuiltinProc) args -> {
            if (args.size() != 1) throw new EvalError("1:1 " + predName + ": expected 1 arg");
            return (args.get(0) instanceof SchemeRecord r && r.typeName().equals(typeName))
                ? Boolean.TRUE : Boolean.FALSE;
        });

        // Define accessors
        for (var entry : fieldAccessors.entrySet()) {
            String accessorName = entry.getKey();
            String fieldName = entry.getValue();
            env.define(accessorName, (BuiltinProc) args -> {
                if (args.size() != 1) throw new EvalError("1:1 " + accessorName + ": expected 1 arg");
                if (!(args.get(0) instanceof SchemeRecord r) || !r.typeName().equals(typeName))
                    throw new EvalError("1:1 " + accessorName + ": not a " + typeName);
                return r.getField(fieldName);
            });
        }

        return VOID;
    }

    private Object evalDefineSyntax(List<?> list, Environment env) throws EvalError {
        if (list.size() != 3) throw error("define-syntax: bad syntax");
        Object nameObj = list.get(1);
        if (nameObj instanceof Located loc) nameObj = loc.value();
        if (!(nameObj instanceof String name)) throw error("define-syntax: expected name");

        Object transformer = list.get(2);
        if (transformer instanceof Located loc) transformer = loc.value();
        if (!(transformer instanceof List<?> tlist) || tlist.size() < 2)
            throw error("define-syntax: expected syntax-rules or lambda");

        Object srHead = tlist.get(0);
        if (srHead instanceof Located loc) srHead = loc.value();

        if ("lambda".equals(srHead)) {
            Object lambdaVal = eval(transformer, env);
            if (lambdaVal instanceof Lambda lam) {
                env.define(name, new SyntaxCaseTransformer(lam, env));
                return VOID;
            }
            throw error("define-syntax: lambda did not evaluate to procedure");
        }

        if (!"syntax-rules".equals(srHead)) throw error("define-syntax: expected syntax-rules or lambda");

        Object litsObj = tlist.get(1);
        if (litsObj instanceof Located loc) litsObj = loc.value();
        if (!(litsObj instanceof List<?> litsList)) throw error("syntax-rules: expected literals list");
        List<String> literals = new ArrayList<>();
        for (Object lit : litsList) {
            if (lit instanceof Located loc) lit = loc.value();
            if (lit instanceof String s) literals.add(s);
        }

        List<SyntaxRule> rules = new ArrayList<>();
        for (int i = 2; i < tlist.size(); i++) {
            Object rule = tlist.get(i);
            if (rule instanceof Located loc) rule = loc.value();
            if (!(rule instanceof List<?> rlist) || rlist.size() != 2)
                throw error("syntax-rules: bad rule");
            Object pattern = unwrapDeep(rlist.get(0));
            Object template = unwrapDeep(rlist.get(1));
            rules.add(new SyntaxRule(pattern, template));
        }

        env.define(name, new SyntaxRulesDef(literals, rules, env));
        return VOID;
    }

    private Object unwrapDeep(Object obj) {
        if (obj instanceof Located loc) return unwrapDeep(loc.value());
        if (obj instanceof List<?> list) {
            List<Object> result = new ArrayList<>();
            for (Object item : list) result.add(unwrapDeep(item));
            return result;
        }
        return obj;
    }

    @SuppressWarnings("unchecked")
    private Object expandMacro(SyntaxRulesDef sr, List<?> inputForm, Environment useEnv) throws EvalError {
        List<Object> input = new ArrayList<>();
        for (Object item : inputForm) input.add(unwrapDeep(item));

        for (SyntaxRule rule : sr.rules) {
            if (!(rule.pattern() instanceof List<?> patList)) continue;
            Map<String, Object> bindings = new HashMap<>();
            Set<String> ellipsisVars = new HashSet<>();

            // Skip position 0 (macro keyword) in both pattern and input
            List<Object> patArgs = new ArrayList<>(patList.subList(1, patList.size()));
            List<Object> inArgs = new ArrayList<>(input.subList(1, input.size()));

            if (matchPattern(patArgs, inArgs, bindings, sr.literals, ellipsisVars)) {
                // Collect pattern variable names
                Set<String> patVars = new HashSet<>();
                collectPatternVars(patArgs, patVars, sr.literals);

                // Apply hygiene: rename introduced symbols
                Map<String, String> renameMap = new HashMap<>();
                Set<String> templateSyms = new HashSet<>();
                collectSymbols(rule.template(), templateSyms);
                for (String sym : templateSyms) {
                    if (patVars.contains(sym) || SPECIAL_FORMS.contains(sym) || "...".equals(sym))
                        continue;
                    // This is an introduced symbol
                    String gs = gensym(sym);
                    renameMap.put(sym, gs);
                }

                Object expanded = expandTemplate(rule.template(), bindings, ellipsisVars, renameMap);

                // Bind gensyms directly in use env (not a wrapper scope, to avoid trapping defines)
                for (Map.Entry<String, String> entry : renameMap.entrySet()) {
                    try {
                        Object val = sr.defEnv.lookup(entry.getKey());
                        useEnv.define(entry.getValue(), val);
                    } catch (EvalError ignored) {
                        // Symbol doesn't exist in def env (e.g., tmp in swap!)
                        // Leave unbound - will be bound by the expansion itself (e.g., let)
                    }
                }
                return expanded;
            }
        }
        throw error("no matching macro pattern");
    }

    record MacroExpansion(Object form, Environment env) {}

    @SuppressWarnings("unchecked")
    private boolean matchPattern(List<Object> pattern, List<Object> input,
            Map<String, Object> bindings, List<String> literals, Set<String> ellipsisVars) {
        // Check for ellipsis in pattern
        int ellipsisIdx = -1;
        for (int i = 0; i < pattern.size(); i++) {
            if ("...".equals(pattern.get(i))) {
                ellipsisIdx = i;
                break;
            }
        }

        if (ellipsisIdx == -1) {
            // No ellipsis - exact length match
            if (pattern.size() != input.size()) return false;
            for (int i = 0; i < pattern.size(); i++) {
                if (!matchOne(pattern.get(i), input.get(i), bindings, literals, ellipsisVars))
                    return false;
            }
            return true;
        }

        // Has ellipsis
        int repeatedIdx = ellipsisIdx - 1;
        int afterCount = pattern.size() - ellipsisIdx - 1;
        int minRequired = repeatedIdx + afterCount;
        if (input.size() < minRequired) return false;

        // Match elements before the repeated pattern
        for (int i = 0; i < repeatedIdx; i++) {
            if (!matchOne(pattern.get(i), input.get(i), bindings, literals, ellipsisVars))
                return false;
        }

        // Collect vars in repeated pattern
        Object repeatedPat = pattern.get(repeatedIdx);
        Set<String> repeatVars = new HashSet<>();
        collectPatternVars(List.of(repeatedPat), repeatVars, literals);
        for (String v : repeatVars) {
            bindings.put(v, new ArrayList<>());
            ellipsisVars.add(v);
        }

        int repeatCount = input.size() - minRequired;
        for (int i = 0; i < repeatCount; i++) {
            Map<String, Object> sub = new HashMap<>();
            Set<String> subEllipsis = new HashSet<>();
            if (!matchOne(repeatedPat, input.get(repeatedIdx + i), sub, literals, subEllipsis))
                return false;
            for (String v : repeatVars) {
                ((List<Object>) bindings.get(v)).add(sub.get(v));
            }
        }

        // Match elements after ellipsis
        for (int i = 0; i < afterCount; i++) {
            if (!matchOne(pattern.get(ellipsisIdx + 1 + i),
                    input.get(input.size() - afterCount + i), bindings, literals, ellipsisVars))
                return false;
        }
        return true;
    }

    private boolean matchOne(Object pattern, Object input,
            Map<String, Object> bindings, List<String> literals, Set<String> ellipsisVars) {
        if (pattern instanceof String sym) {
            if ("_".equals(sym)) return true;
            if (literals.contains(sym)) return sym.equals(input);
            bindings.put(sym, input);
            return true;
        }
        if (pattern instanceof List<?> patList) {
            if (!(input instanceof List<?> inList)) return false;
            return matchPattern((List<Object>) patList, (List<Object>) inList, bindings, literals, ellipsisVars);
        }
        if (pattern instanceof Long || pattern instanceof Boolean) return pattern.equals(input);
        return false;
    }

    private void collectPatternVars(Object pattern, Set<String> vars, List<String> literals) {
        if (pattern instanceof String sym) {
            if (!"_".equals(sym) && !"...".equals(sym) && !literals.contains(sym))
                vars.add(sym);
        } else if (pattern instanceof List<?> list) {
            for (Object item : list) collectPatternVars(item, vars, literals);
        }
    }

    private void collectSymbols(Object template, Set<String> syms) {
        if (template instanceof String s) { syms.add(s); }
        else if (template instanceof List<?> list) {
            // Don't collect symbols inside (quote ...) — they're literal data
            if (!list.isEmpty() && "quote".equals(list.get(0))) return;
            for (Object item : list) collectSymbols(item, syms);
        }
    }

    @SuppressWarnings("unchecked")
    private Object expandTemplate(Object template, Map<String, Object> bindings,
            Set<String> ellipsisVars, Map<String, String> renameMap) throws EvalError {
        if (template instanceof String sym) {
            if (bindings.containsKey(sym) && !ellipsisVars.contains(sym)) return bindings.get(sym);
            if (renameMap.containsKey(sym)) return renameMap.get(sym);
            return sym;
        }
        if (template instanceof List<?> list) {
            List<Object> result = new ArrayList<>();
            for (int i = 0; i < list.size(); i++) {
                // Check if next element is ellipsis
                if (i + 1 < list.size() && "...".equals(list.get(i + 1))) {
                    // Find ellipsis vars in this sub-template
                    Set<String> subEllipsis = new HashSet<>();
                    collectSymbols(list.get(i), subEllipsis);
                    subEllipsis.retainAll(ellipsisVars);

                    if (!subEllipsis.isEmpty()) {
                        String firstVar = subEllipsis.iterator().next();
                        List<Object> varValues = (List<Object>) bindings.get(firstVar);
                        int count = varValues.size();
                        for (int j = 0; j < count; j++) {
                            // Create single-value bindings for this iteration
                            Map<String, Object> iterBindings = new HashMap<>(bindings);
                            for (String ev : subEllipsis) {
                                iterBindings.put(ev, ((List<Object>) bindings.get(ev)).get(j));
                            }
                            Set<String> noEllipsis = new HashSet<>(ellipsisVars);
                            noEllipsis.removeAll(subEllipsis);
                            result.add(expandTemplate(list.get(i), iterBindings, noEllipsis, renameMap));
                        }
                    }
                    i++; // skip the ...
                    continue;
                }
                result.add(expandTemplate(list.get(i), bindings, ellipsisVars, renameMap));
            }
            return result;
        }
        return template;
    }

    // --- syntax-case support ---

    @SuppressWarnings("unchecked")
    private Object expandSyntaxCaseTransformer(SyntaxCaseTransformer sct, List<?> inputForm, Environment useEnv) throws EvalError {
        List<Object> input = new ArrayList<>();
        for (Object item : inputForm) input.add(unwrapDeep(item));

        macroDefEnvStack.add(sct.defEnv());
        macroUseEnvStack.add(useEnv);
        try {
            Object result = apply(sct.transformer(), List.of(input));
            return result;
        } finally {
            macroDefEnvStack.remove(macroDefEnvStack.size() - 1);
            macroUseEnvStack.remove(macroUseEnvStack.size() - 1);
        }
    }

    @SuppressWarnings("unchecked")
    private Object evalSyntaxCaseForm(List<?> list, Environment env) throws EvalError {
        // (syntax-case expr (literals) clause ...)
        if (list.size() < 4) throw error("syntax-case: bad syntax");
        Object inputVal = eval(list.get(1), env);

        Object litsObj = list.get(2);
        if (litsObj instanceof Located loc) litsObj = loc.value();
        List<String> literals = new ArrayList<>();
        if (litsObj instanceof List<?> litsList) {
            for (Object lit : litsList) {
                if (lit instanceof Located l) lit = l.value();
                if (lit instanceof String s) literals.add(s);
            }
        }

        // Unwrap input to a list
        List<Object> inputList;
        if (inputVal instanceof List<?> il) {
            inputList = new ArrayList<>();
            for (Object item : il) inputList.add(unwrapDeep(item));
        } else {
            throw error("syntax-case: expected list, got: " + schemeToString(inputVal));
        }

        for (int ci = 3; ci < list.size(); ci++) {
            Object clause = list.get(ci);
            if (clause instanceof Located loc) clause = loc.value();
            if (!(clause instanceof List<?> clauseList) || clauseList.size() < 2)
                throw error("syntax-case: bad clause");

            Object pattern = unwrapDeep(clauseList.get(0));
            Object output = clauseList.get(clauseList.size() - 1);
            // Optional fender: (pattern fender output) has size 3
            Object fender = clauseList.size() == 3 ? clauseList.get(1) : null;

            if (!(pattern instanceof List<?> patList)) continue;
            Map<String, Object> bindings = new HashMap<>();
            Set<String> ellipsisVars = new HashSet<>();

            // Match the full form (including keyword at position 0)
            List<Object> patArgs = new ArrayList<>((List<Object>) patList);

            if (matchPattern(patArgs, inputList, bindings, literals, ellipsisVars)) {
                // Check fender if present
                if (fender != null) {
                    // Create env with pattern bindings for fender eval
                    Environment fenderEnv = new Environment(env);
                    for (var entry : bindings.entrySet()) {
                        fenderEnv.define(entry.getKey(), entry.getValue());
                    }
                    Object fenderResult = eval(fender, fenderEnv);
                    if (Boolean.FALSE.equals(fenderResult)) continue;
                }

                Set<String> patVars = new HashSet<>();
                collectPatternVars(patArgs, patVars, literals);

                Environment defEnv = macroDefEnvStack.isEmpty() ? env :
                    macroDefEnvStack.get(macroDefEnvStack.size() - 1);

                SyntaxContext ctx = new SyntaxContext(bindings, ellipsisVars, patVars, defEnv);
                syntaxContextStack.add(ctx);
                try {
                    return eval(output, env);
                } finally {
                    syntaxContextStack.remove(syntaxContextStack.size() - 1);
                }
            }
        }
        throw error("syntax-case: no matching pattern");
    }

    private Object evalSyntaxForm(List<?> list) throws EvalError {
        // (syntax template)
        if (list.size() != 2) throw error("syntax: bad syntax");
        Object template = unwrapDeep(list.get(1));

        if (syntaxContextStack.isEmpty()) return template;

        SyntaxContext ctx = syntaxContextStack.get(syntaxContextStack.size() - 1);

        // Simple variable lookup
        if (template instanceof String sym && ctx.bindings.containsKey(sym)
                && !ctx.ellipsisVars.contains(sym)) {
            return ctx.bindings.get(sym);
        }

        // Apply hygiene: rename introduced symbols
        Map<String, String> renameMap = new HashMap<>();
        Set<String> templateSyms = new HashSet<>();
        collectSymbols(template, templateSyms);
        for (String sym : templateSyms) {
            if (ctx.patternVars.contains(sym) || SPECIAL_FORMS.contains(sym) || "...".equals(sym))
                continue;
            if (ctx.bindings.containsKey(sym)) continue;
            String gs = gensym(sym);
            renameMap.put(sym, gs);
        }

        Object expanded = expandTemplate(template, ctx.bindings, ctx.ellipsisVars, renameMap);

        Environment useEnv = macroUseEnvStack.isEmpty() ? null :
            macroUseEnvStack.get(macroUseEnvStack.size() - 1);

        // Bind gensyms in use env (not a wrapper scope, to avoid trapping defines)
        if (!renameMap.isEmpty() && useEnv != null) {
            for (Map.Entry<String, String> entry : renameMap.entrySet()) {
                try {
                    Object val = ctx.defEnv.lookup(entry.getKey());
                    useEnv.define(entry.getValue(), val);
                } catch (EvalError ignored) {}
            }
        }
        return expanded;
    }

    private Object evalWithSyntax(List<?> list, Environment env) throws EvalError {
        // (with-syntax ((pattern expr) ...) body ...)
        if (list.size() < 3) throw error("with-syntax: bad syntax");
        Object bObj = list.get(1);
        if (bObj instanceof Located loc) bObj = loc.value();
        if (!(bObj instanceof List<?> bindList)) throw error("with-syntax: bad bindings");

        if (syntaxContextStack.isEmpty()) throw error("with-syntax: not inside syntax-case");
        SyntaxContext parentCtx = syntaxContextStack.get(syntaxContextStack.size() - 1);

        Map<String, Object> newBindings = new HashMap<>(parentCtx.bindings());
        Set<String> newEllipsis = new HashSet<>(parentCtx.ellipsisVars());
        Set<String> newPatVars = new HashSet<>(parentCtx.patternVars());

        for (Object b : bindList) {
            if (b instanceof Located loc) b = loc.value();
            if (!(b instanceof List<?> binding) || binding.size() != 2)
                throw error("with-syntax: bad binding");
            Object pat = binding.get(0);
            if (pat instanceof Located loc) pat = loc.value();
            if (!(pat instanceof String varName)) throw error("with-syntax: expected symbol");
            Object val = eval(binding.get(1), env);
            newBindings.put(varName, val);
            newPatVars.add(varName);
        }

        SyntaxContext newCtx = new SyntaxContext(newBindings, newEllipsis, newPatVars, parentCtx.defEnv());
        // Replace top of stack
        syntaxContextStack.set(syntaxContextStack.size() - 1, newCtx);
        try {
            Object result = VOID;
            for (int i = 2; i < list.size(); i++) {
                result = eval(list.get(i), env);
            }
            return result;
        } finally {
            syntaxContextStack.set(syntaxContextStack.size() - 1, parentCtx);
        }
    }

    private Object apply(Object proc, List<Object> args) throws EvalError {
        if (proc instanceof Lambda lambda) {
            if (lambda.restParam != null) {
                if (args.size() < lambda.params.size()) {
                    throw error("wrong number of arguments: expected at least " + lambda.params.size() + ", got " + args.size());
                }
            } else {
                if (args.size() != lambda.params.size()) {
                    throw error("wrong number of arguments: expected " + lambda.params.size() + ", got " + args.size());
                }
            }
            Environment callEnv = new Environment(lambda.closure);
            for (int i = 0; i < lambda.params.size(); i++) {
                callEnv.define(lambda.params.get(i), args.get(i));
            }
            if (lambda.restParam != null) {
                Object rest = NIL;
                for (int i = args.size() - 1; i >= lambda.params.size(); i--) {
                    rest = new Pair(args.get(i), rest);
                }
                callEnv.define(lambda.restParam, rest);
            }
            currentBodyExprs = lambda.body;
            currentBodyEnv = callEnv;
            Object result = VOID;
            for (Object bodyExpr : lambda.body) {
                result = eval(bodyExpr, callEnv);
            }
            return result;
        }
        if (proc instanceof CaseLambda cl) {
            for (Lambda clause : cl.clauses) {
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
            throw error("case-lambda: no matching clause for " + args.size() + " arguments");
        }
        if (proc instanceof BuiltinProc bp) {
            return bp.apply(args);
        }
        if (proc instanceof SchemeContinuation cont) {
            if (args.size() != 1) throw error("continuation: expected 1 argument");
            if (cont.active) {
                throw new ContinuationInvoked(cont, args.get(0));
            }
            if (cont.bodyEnv == null || cont.bodyEnv == globalEnv
                    || cont.bodyEnv.getParent() == globalEnv) {
                throw new ContinuationInvoked(cont, args.get(0));
            }
            return args.get(0);
        }
        throw error("not a procedure: " + schemeToString(proc));
    }

    // Builtins are registered as BuiltinProc in the global env
    @FunctionalInterface
    interface BuiltinProc {
        Object apply(List<Object> args) throws EvalError;
    }

    {
        // raise as a builtin procedure (not special form, so user code can shadow it)
        globalEnv.define("raise", (BuiltinProc) args -> {
            if (args.size() != 1) throw error("raise: expected 1 argument");
            Object raised = args.get(0);
            if (!exceptionHandlerStack.isEmpty()) {
                Object handler = exceptionHandlerStack.remove(exceptionHandlerStack.size() - 1);
                apply(handler, List.of(raised));
                // Handler returned without escaping — re-raise
                exceptionHandlerStack.add(handler);
            }
            throw new SchemeRaise(raised);
        });

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
            return isNumber(args.get(0));
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
        globalEnv.define("procedure?", (BuiltinProc) args -> {
            if (args.size() != 1) throw error("procedure?: expected 1 argument");
            Object val = args.get(0);
            return val instanceof Lambda || val instanceof CaseLambda || val instanceof BuiltinProc || val instanceof SchemeContinuation;
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
            Object parsed = parseNumber(s.value());
            return parsed != null ? parsed : Boolean.FALSE;
        });
        globalEnv.define("number->string", (BuiltinProc) args -> {
            if (args.size() != 1) throw error("number->string: expected 1 argument");
            Object val = args.get(0);
            if (val instanceof Long n) return new SchemeString(n.toString());
            if (val instanceof Rational r) return new SchemeString(r.toString());
            if (val instanceof Double d) return new SchemeString(String.valueOf(d));
            throw error("number->string: not a number");
        });

        // Symbol/String conversion
        globalEnv.define("symbol->string", (BuiltinProc) args -> {
            if (args.size() != 1) throw error("symbol->string: expected 1 argument");
            if (!(args.get(0) instanceof String s)) throw error("symbol->string: not a symbol");
            return new SchemeString(s);
        });
        globalEnv.define("syntax->datum", (BuiltinProc) args -> {
            if (args.size() != 1) throw error("syntax->datum: expected 1 argument");
            return args.get(0);
        });
        globalEnv.define("datum->syntax", (BuiltinProc) args -> {
            if (args.size() != 2) throw error("datum->syntax: expected 2 arguments");
            return args.get(1);
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
            if (s.isImmutable()) throw error("string-set!: string is immutable");
            if (!(args.get(1) instanceof Number n)) throw error("string-set!: index not a number");
            if (!(args.get(2) instanceof SchemeChar c)) throw error("string-set!: not a character");
            int idx = n.intValue();
            if (idx < 0 || idx >= s.length()) throw error("string-set!: index out of range");
            s.setChar(idx, c.value());
            return VOID;
        });

        globalEnv.define("string->list", (BuiltinProc) args -> {
            if (args.size() != 1) throw error("string->list: expected 1 argument");
            if (!(args.get(0) instanceof SchemeString s)) throw error("string->list: not a string");
            Object result = NIL;
            String v = s.value();
            for (int i = v.length() - 1; i >= 0; i--) {
                result = new Pair(new SchemeChar(v.charAt(i)), result);
            }
            return result;
        });

        globalEnv.define("list->string", (BuiltinProc) args -> {
            if (args.size() != 1) throw error("list->string: expected 1 argument");
            StringBuilder sb = new StringBuilder();
            Object cur = args.get(0);
            while (cur instanceof Pair p) {
                if (!(p.car instanceof SchemeChar ch)) throw error("list->string: not a character");
                sb.append(ch.value());
                cur = p.cdr;
            }
            return new SchemeString(sb.toString());
        });

        globalEnv.define("char->integer", (BuiltinProc) args -> {
            if (args.size() != 1) throw error("char->integer: expected 1 argument");
            if (!(args.get(0) instanceof SchemeChar ch)) throw error("char->integer: not a character");
            return (long) ch.value();
        });

        globalEnv.define("integer->char", (BuiltinProc) args -> {
            if (args.size() != 1) throw error("integer->char: expected 1 argument");
            if (!(args.get(0) instanceof Long n)) throw error("integer->char: not an integer");
            return new SchemeChar((char) n.intValue());
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

        // eq? (identity/value equality for symbols, numbers, booleans, chars)
        globalEnv.define("eq?", (BuiltinProc) args -> {
            if (args.size() != 2) throw error("eq?: expected 2 arguments");
            Object a = args.get(0), b = args.get(1);
            if (a == b) return true;
            if (a instanceof Long && b instanceof Long) return a.equals(b);
            if (a instanceof Rational && b instanceof Rational) return a.equals(b);
            if (a instanceof Double && b instanceof Double) return a.equals(b);
            if (a instanceof Boolean && b instanceof Boolean) return a.equals(b);
            if (a instanceof SchemeChar && b instanceof SchemeChar) return a.equals(b);
            if (a instanceof String && b instanceof String) return a.equals(b);
            return false;
        });

        // equal? (deep structural equality)
        globalEnv.define("equal?", (BuiltinProc) args -> {
            if (args.size() != 2) throw error("equal?: expected 2 arguments");
            return schemeEqual(args.get(0), args.get(1));
        });

        // eqv?
        globalEnv.define("eqv?", (BuiltinProc) args -> {
            if (args.size() != 2) throw error("eqv?: expected 2 arguments");
            return eqv(args.get(0), args.get(1));
        });

        // Vector operations
        globalEnv.define("vector", (BuiltinProc) args -> {
            return new SchemeVector(args.toArray());
        });
        globalEnv.define("make-vector", (BuiltinProc) args -> {
            if (args.size() < 1 || args.size() > 2) throw error("make-vector: expected 1-2 arguments");
            if (!(args.get(0) instanceof Long size)) throw error("make-vector: not a number");
            Object fill = args.size() == 2 ? args.get(1) : 0L;
            return new SchemeVector(size.intValue(), fill);
        });
        globalEnv.define("vector-ref", (BuiltinProc) args -> {
            if (args.size() != 2) throw error("vector-ref: expected 2 arguments");
            if (!(args.get(0) instanceof SchemeVector v)) throw error("vector-ref: not a vector");
            if (!(args.get(1) instanceof Long idx)) throw error("vector-ref: not a number");
            return v.ref(idx.intValue());
        });
        globalEnv.define("vector-set!", (BuiltinProc) args -> {
            if (args.size() != 3) throw error("vector-set!: expected 3 arguments");
            if (!(args.get(0) instanceof SchemeVector v)) throw error("vector-set!: not a vector");
            if (!(args.get(1) instanceof Long idx)) throw error("vector-set!: not a number");
            v.set(idx.intValue(), args.get(2));
            return VOID;
        });
        globalEnv.define("vector-length", (BuiltinProc) args -> {
            if (args.size() != 1) throw error("vector-length: expected 1 argument");
            if (!(args.get(0) instanceof SchemeVector v)) throw error("vector-length: not a vector");
            return (long) v.length();
        });
        globalEnv.define("vector?", (BuiltinProc) args -> {
            if (args.size() != 1) throw error("vector?: expected 1 argument");
            return args.get(0) instanceof SchemeVector;
        });
        globalEnv.define("vector->list", (BuiltinProc) args -> {
            if (args.size() != 1) throw error("vector->list: expected 1 argument");
            if (!(args.get(0) instanceof SchemeVector v)) throw error("vector->list: not a vector");
            Object result = NIL;
            for (int i = v.length() - 1; i >= 0; i--) {
                result = new Pair(v.ref(i), result);
            }
            return result;
        });
        globalEnv.define("list->vector", (BuiltinProc) args -> {
            if (args.size() != 1) throw error("list->vector: expected 1 argument");
            List<Object> elems = new ArrayList<>();
            Object cur = args.get(0);
            while (cur instanceof Pair p) {
                elems.add(p.car);
                cur = p.cdr;
            }
            return new SchemeVector(elems.toArray());
        });

        // Numeric utilities
        globalEnv.define("abs", (BuiltinProc) args -> {
            if (args.size() != 1) throw error("abs: expected 1 argument");
            Object val = args.get(0);
            if (val instanceof Long n) return Math.abs(n);
            if (val instanceof Rational r) return normalizeExact(Rational.of(Math.abs(r.numerator()), r.denominator()));
            if (val instanceof Double d) return Math.abs(d);
            throw error("abs: not a number");
        });
        globalEnv.define("modulo", (BuiltinProc) args -> {
            if (args.size() != 2) throw error("modulo: expected 2 arguments");
            if (!(args.get(0) instanceof Long a)) throw error("modulo: not a number");
            if (!(args.get(1) instanceof Long b)) throw error("modulo: not a number");
            if (b == 0) throw error("modulo: division by zero");
            long r = a % b;
            if (r != 0 && ((r > 0) != (b > 0))) r += b;
            return r;
        });
        globalEnv.define("remainder", (BuiltinProc) args -> {
            if (args.size() != 2) throw error("remainder: expected 2 arguments");
            if (!(args.get(0) instanceof Long a)) throw error("remainder: not a number");
            if (!(args.get(1) instanceof Long b)) throw error("remainder: not a number");
            if (b == 0) throw error("remainder: division by zero");
            return a % b;
        });
        globalEnv.define("quotient", (BuiltinProc) args -> {
            if (args.size() != 2) throw error("quotient: expected 2 arguments");
            if (!(args.get(0) instanceof Long a)) throw error("quotient: not a number");
            if (!(args.get(1) instanceof Long b)) throw error("quotient: not a number");
            if (b == 0) throw error("quotient: division by zero");
            // Truncation toward zero (Java default behavior)
            return a / b;
        });
        globalEnv.define("min", (BuiltinProc) args -> {
            if (args.isEmpty()) throw error("min: expected at least 1 argument");
            if (!(args.get(0) instanceof Long result)) throw error("min: not a number");
            long r = result;
            for (int i = 1; i < args.size(); i++) {
                if (!(args.get(i) instanceof Long v)) throw error("min: not a number");
                if (v < r) r = v;
            }
            return r;
        });
        globalEnv.define("max", (BuiltinProc) args -> {
            if (args.isEmpty()) throw error("max: expected at least 1 argument");
            if (!(args.get(0) instanceof Long result)) throw error("max: not a number");
            long r = result;
            for (int i = 1; i < args.size(); i++) {
                if (!(args.get(i) instanceof Long v)) throw error("max: not a number");
                if (v > r) r = v;
            }
            return r;
        });
        globalEnv.define("expt", (BuiltinProc) args -> {
            if (args.size() != 2) throw error("expt: expected 2 arguments");
            if (!(args.get(0) instanceof Long base)) throw error("expt: not a number");
            if (!(args.get(1) instanceof Long exp)) throw error("expt: not a number");
            long result = 1;
            long b = base;
            long e = exp;
            if (e < 0) return 0L; // integer expt with negative exp → 0
            while (e > 0) {
                if ((e & 1) == 1) result *= b;
                b *= b;
                e >>= 1;
            }
            return result;
        });

        // Number predicates
        globalEnv.define("zero?", (BuiltinProc) args -> {
            if (args.size() != 1) throw error("zero?: expected 1 argument");
            Object val = args.get(0);
            if (val instanceof Long n) return n == 0L;
            if (val instanceof Rational r) return r.numerator() == 0;
            if (val instanceof Double d) return d == 0.0;
            throw error("zero?: not a number");
        });
        globalEnv.define("positive?", (BuiltinProc) args -> {
            if (args.size() != 1) throw error("positive?: expected 1 argument");
            Object val = args.get(0);
            if (val instanceof Long n) return n > 0L;
            if (val instanceof Rational r) return r.numerator() > 0;
            if (val instanceof Double d) return d > 0.0;
            throw error("positive?: not a number");
        });
        globalEnv.define("negative?", (BuiltinProc) args -> {
            if (args.size() != 1) throw error("negative?: expected 1 argument");
            Object val = args.get(0);
            if (val instanceof Long n) return n < 0L;
            if (val instanceof Rational r) return r.numerator() < 0;
            if (val instanceof Double d) return d < 0.0;
            throw error("negative?: not a number");
        });
        globalEnv.define("odd?", (BuiltinProc) args -> {
            if (args.size() != 1) throw error("odd?: expected 1 argument");
            if (!(args.get(0) instanceof Long n)) throw error("odd?: not a number");
            return n % 2 != 0;
        });
        globalEnv.define("even?", (BuiltinProc) args -> {
            if (args.size() != 1) throw error("even?: expected 1 argument");
            if (!(args.get(0) instanceof Long n)) throw error("even?: not a number");
            return n % 2 == 0;
        });

        // List utilities
        globalEnv.define("list-ref", (BuiltinProc) args -> {
            if (args.size() != 2) throw error("list-ref: expected 2 arguments");
            if (!(args.get(1) instanceof Long idx)) throw error("list-ref: not a number");
            Object cur = args.get(0);
            for (long i = 0; i < idx; i++) {
                if (!(cur instanceof Pair p)) throw error("list-ref: index out of range");
                cur = p.cdr;
            }
            if (!(cur instanceof Pair p)) throw error("list-ref: index out of range");
            return p.car;
        });
        globalEnv.define("list-tail", (BuiltinProc) args -> {
            if (args.size() != 2) throw error("list-tail: expected 2 arguments");
            if (!(args.get(1) instanceof Long idx)) throw error("list-tail: not a number");
            Object cur = args.get(0);
            for (long i = 0; i < idx; i++) {
                if (!(cur instanceof Pair p)) throw error("list-tail: index out of range");
                cur = p.cdr;
            }
            return cur;
        });
        globalEnv.define("list?", (BuiltinProc) args -> {
            if (args.size() != 1) throw error("list?: expected 1 argument");
            // Tortoise-and-hare cycle detection
            Object slow = args.get(0);
            Object fast = args.get(0);
            while (fast instanceof Pair fp) {
                fast = fp.cdr;
                if (!(fast instanceof Pair fp2)) {
                    return fast == NIL;
                }
                fast = fp2.cdr;
                slow = ((Pair) slow).cdr;
                if (slow == fast) return false; // cycle detected
            }
            return fast == NIL;
        });
        globalEnv.define("assoc", (BuiltinProc) args -> {
            if (args.size() != 2) throw error("assoc: expected 2 arguments");
            Object key = args.get(0);
            Object alist = args.get(1);
            while (alist instanceof Pair p) {
                if (p.car instanceof Pair entry) {
                    if (schemeEqual(key, entry.car)) return entry;
                }
                alist = p.cdr;
            }
            return false;
        });

        // map (supports multiple lists)
        globalEnv.define("map", (BuiltinProc) args -> {
            if (args.size() < 2) throw error("map: expected at least 2 arguments");
            Object proc = args.get(0);
            List<Object> lists = new ArrayList<>();
            for (int i = 1; i < args.size(); i++) {
                lists.add(args.get(i));
            }
            List<Object> results = new ArrayList<>();
            while (true) {
                // Check if any list is exhausted
                boolean done = false;
                for (Object lst : lists) {
                    if (!(lst instanceof Pair)) { done = true; break; }
                }
                if (done) break;
                List<Object> callArgs = new ArrayList<>();
                for (int i = 0; i < lists.size(); i++) {
                    Pair p = (Pair) lists.get(i);
                    callArgs.add(p.car);
                    lists.set(i, p.cdr);
                }
                results.add(apply(proc, callArgs));
            }
            Object result = NIL;
            for (int i = results.size() - 1; i >= 0; i--) {
                result = new Pair(results.get(i), result);
            }
            return result;
        });

        // cxr shortcuts
        globalEnv.define("caar", (BuiltinProc) args -> {
            if (args.size() != 1) throw error("caar: expected 1 argument");
            if (!(args.get(0) instanceof Pair p)) throw error("caar: not a pair");
            if (!(p.car instanceof Pair p2)) throw error("caar: car is not a pair");
            return p2.car;
        });
        globalEnv.define("cadr", (BuiltinProc) args -> {
            if (args.size() != 1) throw error("cadr: expected 1 argument");
            if (!(args.get(0) instanceof Pair p)) throw error("cadr: not a pair");
            if (!(p.cdr instanceof Pair p2)) throw error("cadr: cdr is not a pair");
            return p2.car;
        });
        globalEnv.define("cdar", (BuiltinProc) args -> {
            if (args.size() != 1) throw error("cdar: expected 1 argument");
            if (!(args.get(0) instanceof Pair p)) throw error("cdar: not a pair");
            if (!(p.car instanceof Pair p2)) throw error("cdar: car is not a pair");
            return p2.cdr;
        });
        globalEnv.define("cddr", (BuiltinProc) args -> {
            if (args.size() != 1) throw error("cddr: expected 1 argument");
            if (!(args.get(0) instanceof Pair p)) throw error("cddr: not a pair");
            if (!(p.cdr instanceof Pair p2)) throw error("cddr: cdr is not a pair");
            return p2.cdr;
        });

        // member (equal?-based)
        globalEnv.define("member", (BuiltinProc) args -> {
            if (args.size() != 2) throw error("member: expected 2 arguments");
            Object obj = args.get(0);
            Object lst = args.get(1);
            while (lst instanceof Pair p) {
                if (schemeEqual(obj, p.car)) return lst;
                lst = p.cdr;
            }
            return false;
        });
        // memv (eqv?-based)
        globalEnv.define("memv", (BuiltinProc) args -> {
            if (args.size() != 2) throw error("memv: expected 2 arguments");
            Object obj = args.get(0);
            Object lst = args.get(1);
            while (lst instanceof Pair p) {
                if (eqv(obj, p.car)) return lst;
                lst = p.cdr;
            }
            return false;
        });
        // assq (eq?-based)
        globalEnv.define("assq", (BuiltinProc) args -> {
            if (args.size() != 2) throw error("assq: expected 2 arguments");
            Object key = args.get(0);
            Object alist = args.get(1);
            while (alist instanceof Pair p) {
                if (p.car instanceof Pair entry) {
                    Object entryKey = entry.car;
                    if (key == entryKey || (key instanceof Long && key.equals(entryKey))
                        || (key instanceof String && key.equals(entryKey))) return entry;
                }
                alist = p.cdr;
            }
            return false;
        });
        // assv (eqv?-based)
        globalEnv.define("assv", (BuiltinProc) args -> {
            if (args.size() != 2) throw error("assv: expected 2 arguments");
            Object key = args.get(0);
            Object alist = args.get(1);
            while (alist instanceof Pair p) {
                if (p.car instanceof Pair entry) {
                    if (eqv(key, entry.car)) return entry;
                }
                alist = p.cdr;
            }
            return false;
        });
        // memq (eq?-based)
        globalEnv.define("memq", (BuiltinProc) args -> {
            if (args.size() != 2) throw error("memq: expected 2 arguments");
            Object obj = args.get(0);
            Object lst = args.get(1);
            while (lst instanceof Pair p) {
                if (obj == p.car || (obj instanceof Long && obj.equals(p.car))
                    || (obj instanceof String && obj.equals(p.car))) return lst;
                lst = p.cdr;
            }
            return false;
        });
        // gcd
        globalEnv.define("gcd", (BuiltinProc) args -> {
            if (args.isEmpty()) return 0L;
            long result = numToLong(args.get(0));
            if (result < 0) result = -result;
            for (int i = 1; i < args.size(); i++) {
                long b = numToLong(args.get(i));
                if (b < 0) b = -b;
                while (b != 0) { long t = b; b = result % b; result = t; }
            }
            return result;
        });
        // lcm
        globalEnv.define("lcm", (BuiltinProc) args -> {
            if (args.isEmpty()) return 1L;
            long result = numToLong(args.get(0));
            if (result < 0) result = -result;
            for (int i = 1; i < args.size(); i++) {
                long b = numToLong(args.get(i));
                if (b < 0) b = -b;
                if (result == 0 || b == 0) { result = 0; } else {
                    long g = result; long tmp = b;
                    while (tmp != 0) { long t = tmp; tmp = g % tmp; g = t; }
                    result = result / g * b;
                }
            }
            return result;
        });
        // truncate
        globalEnv.define("truncate", (BuiltinProc) args -> {
            if (args.size() != 1) throw error("truncate: expected 1 argument");
            Object val = args.get(0);
            if (val instanceof Long) return val;
            if (val instanceof Double d) return (long) d.doubleValue();
            if (val instanceof Rational r) return r.toLong();
            throw error("truncate: not a number");
        });
        // round
        globalEnv.define("round", (BuiltinProc) args -> {
            if (args.size() != 1) throw error("round: expected 1 argument");
            Object val = args.get(0);
            if (val instanceof Long) return val;
            if (val instanceof Double d) return Math.round(d);
            if (val instanceof Rational r) return Math.round(r.toDouble());
            throw error("round: not a number");
        });
        // make-string
        globalEnv.define("make-string", (BuiltinProc) args -> {
            if (args.size() < 1 || args.size() > 2) throw error("make-string: expected 1-2 arguments");
            if (!(args.get(0) instanceof Long len)) throw error("make-string: not a number");
            char fill = args.size() == 2 && args.get(1) instanceof SchemeChar c ? c.value() : '\0';
            char[] chars = new char[len.intValue()];
            java.util.Arrays.fill(chars, fill);
            return new SchemeString(new String(chars));
        });
        // string (from chars)
        globalEnv.define("string", (BuiltinProc) args -> {
            StringBuilder sb = new StringBuilder();
            for (Object a : args) {
                if (!(a instanceof SchemeChar c)) throw error("string: not a character");
                sb.append(c.value());
            }
            return new SchemeString(sb.toString());
        });
        // string>?
        globalEnv.define("string>?", (BuiltinProc) args -> {
            if (args.size() != 2) throw error("string>?: expected 2 arguments");
            if (!(args.get(0) instanceof SchemeString a)) throw error("string>?: not a string");
            if (!(args.get(1) instanceof SchemeString b)) throw error("string>?: not a string");
            return a.value().compareTo(b.value()) > 0;
        });
        // string<=?
        globalEnv.define("string<=?", (BuiltinProc) args -> {
            if (args.size() != 2) throw error("string<=?: expected 2 arguments");
            if (!(args.get(0) instanceof SchemeString a)) throw error("string<=?: not a string");
            if (!(args.get(1) instanceof SchemeString b)) throw error("string<=?: not a string");
            return a.value().compareTo(b.value()) <= 0;
        });
        // string>=?
        globalEnv.define("string>=?", (BuiltinProc) args -> {
            if (args.size() != 2) throw error("string>=?: expected 2 arguments");
            if (!(args.get(0) instanceof SchemeString a)) throw error("string>=?: not a string");
            if (!(args.get(1) instanceof SchemeString b)) throw error("string>=?: not a string");
            return a.value().compareTo(b.value()) >= 0;
        });

        // reverse
        globalEnv.define("reverse", (BuiltinProc) args -> {
            if (args.size() != 1) throw error("reverse: expected 1 argument");
            Object result = NIL;
            Object cur = args.get(0);
            while (cur instanceof Pair p) {
                result = new Pair(p.car, result);
                cur = p.cdr;
            }
            return result;
        });

        // for-each (supports multiple lists)
        globalEnv.define("for-each", (BuiltinProc) args -> {
            if (args.size() < 2) throw error("for-each: expected at least 2 arguments");
            Object proc = args.get(0);
            List<Object> lists = new ArrayList<>();
            for (int i = 1; i < args.size(); i++) {
                lists.add(args.get(i));
            }
            while (true) {
                boolean done = false;
                for (Object lst : lists) {
                    if (!(lst instanceof Pair)) { done = true; break; }
                }
                if (done) break;
                List<Object> callArgs = new ArrayList<>();
                for (int i = 0; i < lists.size(); i++) {
                    Pair p = (Pair) lists.get(i);
                    callArgs.add(p.car);
                    lists.set(i, p.cdr);
                }
                apply(proc, callArgs);
            }
            return VOID;
        });

        // Pair mutation
        globalEnv.define("set-car!", (BuiltinProc) args -> {
            if (args.size() != 2) throw error("set-car!: expected 2 arguments");
            if (!(args.get(0) instanceof Pair p)) throw error("set-car!: not a pair");
            p.car = args.get(1);
            return VOID;
        });
        globalEnv.define("set-cdr!", (BuiltinProc) args -> {
            if (args.size() != 2) throw error("set-cdr!: expected 2 arguments");
            if (!(args.get(0) instanceof Pair p)) throw error("set-cdr!: not a pair");
            p.cdr = args.get(1);
            return VOID;
        });

        // Character operations
        globalEnv.define("char-alphabetic?", (BuiltinProc) args -> {
            if (args.size() != 1) throw error("char-alphabetic?: expected 1 argument");
            if (!(args.get(0) instanceof SchemeChar c)) throw error("char-alphabetic?: not a character");
            return Character.isLetter(c.value());
        });
        globalEnv.define("char-numeric?", (BuiltinProc) args -> {
            if (args.size() != 1) throw error("char-numeric?: expected 1 argument");
            if (!(args.get(0) instanceof SchemeChar c)) throw error("char-numeric?: not a character");
            return Character.isDigit(c.value());
        });
        globalEnv.define("char-upcase", (BuiltinProc) args -> {
            if (args.size() != 1) throw error("char-upcase: expected 1 argument");
            if (!(args.get(0) instanceof SchemeChar c)) throw error("char-upcase: not a character");
            return new SchemeChar(Character.toUpperCase(c.value()));
        });
        globalEnv.define("char-downcase", (BuiltinProc) args -> {
            if (args.size() != 1) throw error("char-downcase: expected 1 argument");
            if (!(args.get(0) instanceof SchemeChar c)) throw error("char-downcase: not a character");
            return new SchemeChar(Character.toLowerCase(c.value()));
        });
        globalEnv.define("char=?", (BuiltinProc) args -> {
            if (args.size() != 2) throw error("char=?: expected 2 arguments");
            if (!(args.get(0) instanceof SchemeChar a)) throw error("char=?: not a character");
            if (!(args.get(1) instanceof SchemeChar b)) throw error("char=?: not a character");
            return a.value() == b.value();
        });
        globalEnv.define("char<?", (BuiltinProc) args -> {
            if (args.size() != 2) throw error("char<?: expected 2 arguments");
            if (!(args.get(0) instanceof SchemeChar a)) throw error("char<?: not a character");
            if (!(args.get(1) instanceof SchemeChar b)) throw error("char<?: not a character");
            return a.value() < b.value();
        });

        // String comparison
        globalEnv.define("string=?", (BuiltinProc) args -> {
            if (args.size() != 2) throw error("string=?: expected 2 arguments");
            if (!(args.get(0) instanceof SchemeString a)) throw error("string=?: not a string");
            if (!(args.get(1) instanceof SchemeString b)) throw error("string=?: not a string");
            return a.value().equals(b.value());
        });
        globalEnv.define("string<?", (BuiltinProc) args -> {
            if (args.size() != 2) throw error("string<?: expected 2 arguments");
            if (!(args.get(0) instanceof SchemeString a)) throw error("string<?: not a string");
            if (!(args.get(1) instanceof SchemeString b)) throw error("string<?: not a string");
            return a.value().compareTo(b.value()) < 0;
        });
        globalEnv.define("string-ci=?", (BuiltinProc) args -> {
            if (args.size() != 2) throw error("string-ci=?: expected 2 arguments");
            if (!(args.get(0) instanceof SchemeString a)) throw error("string-ci=?: not a string");
            if (!(args.get(1) instanceof SchemeString b)) throw error("string-ci=?: not a string");
            return a.value().equalsIgnoreCase(b.value());
        });
        globalEnv.define("string-upcase", (BuiltinProc) args -> {
            if (args.size() != 1) throw error("string-upcase: expected 1 argument");
            if (!(args.get(0) instanceof SchemeString s)) throw error("string-upcase: not a string");
            return new SchemeString(s.value().toUpperCase());
        });
        globalEnv.define("string-downcase", (BuiltinProc) args -> {
            if (args.size() != 1) throw error("string-downcase: expected 1 argument");
            if (!(args.get(0) instanceof SchemeString s)) throw error("string-downcase: not a string");
            return new SchemeString(s.value().toLowerCase());
        });

        // apply
        globalEnv.define("apply", (BuiltinProc) args -> {
            if (args.size() < 2) throw error("apply: expected at least 2 arguments");
            Object proc = args.get(0);
            // Last argument must be a list; prefix arguments are prepended
            Object lastArg = args.get(args.size() - 1);
            List<Object> callArgs = new ArrayList<>();
            for (int i = 1; i < args.size() - 1; i++) {
                callArgs.add(args.get(i));
            }
            // Unpack the last argument (a list) into callArgs
            Object cur = lastArg;
            while (cur instanceof Pair p) {
                callArgs.add(p.car);
                cur = p.cdr;
            }
            return apply(proc, callArgs);
        });

        // L11: Exact/Inexact predicates and conversions
        globalEnv.define("exact?", (BuiltinProc) args -> {
            if (args.size() != 1) throw error("exact?: expected 1 argument");
            return isExact(args.get(0));
        });
        globalEnv.define("inexact?", (BuiltinProc) args -> {
            if (args.size() != 1) throw error("inexact?: expected 1 argument");
            return args.get(0) instanceof Double;
        });
        globalEnv.define("exact->inexact", (BuiltinProc) args -> {
            if (args.size() != 1) throw error("exact->inexact: expected 1 argument");
            if (!isNumber(args.get(0))) throw error("exact->inexact: not a number");
            return toDouble(args.get(0));
        });
        globalEnv.define("inexact->exact", (BuiltinProc) args -> {
            if (args.size() != 1) throw error("inexact->exact: expected 1 argument");
            Object val = args.get(0);
            if (val instanceof Long || val instanceof Rational) return val;
            if (val instanceof Double d) {
                // Convert double to exact rational
                if (d == Math.floor(d) && !Double.isInfinite(d)) return (long) d.doubleValue();
                // Use BigDecimal-like approach: find rational representation
                long bits = Double.doubleToLongBits(d);
                long mantissa = bits & 0x000fffffffffffffL;
                int exponent = (int) ((bits >> 52) & 0x7ffL) - 1023 - 52;
                mantissa |= 0x0010000000000000L; // implicit leading 1
                if ((bits & 0x8000000000000000L) != 0) mantissa = -mantissa;
                if (exponent >= 0) {
                    return normalizeExact(Rational.of(mantissa << exponent, 1));
                } else {
                    return normalizeExact(Rational.of(mantissa, 1L << (-exponent)));
                }
            }
            throw error("inexact->exact: not a number");
        });
        globalEnv.define("numerator", (BuiltinProc) args -> {
            if (args.size() != 1) throw error("numerator: expected 1 argument");
            Object val = args.get(0);
            if (val instanceof Long l) return l;
            if (val instanceof Rational r) return r.numerator();
            throw error("numerator: not an exact number");
        });
        globalEnv.define("denominator", (BuiltinProc) args -> {
            if (args.size() != 1) throw error("denominator: expected 1 argument");
            Object val = args.get(0);
            if (val instanceof Long) return 1L;
            if (val instanceof Rational r) return r.denominator();
            throw error("denominator: not an exact number");
        });
        globalEnv.define("integer?", (BuiltinProc) args -> {
            if (args.size() != 1) throw error("integer?: expected 1 argument");
            Object val = args.get(0);
            if (val instanceof Long) return true;
            if (val instanceof Rational r) return r.isInteger();
            if (val instanceof Double d) return d == Math.floor(d) && !Double.isInfinite(d);
            return false;
        });
        globalEnv.define("rational?", (BuiltinProc) args -> {
            if (args.size() != 1) throw error("rational?: expected 1 argument");
            return isExact(args.get(0));
        });

        // call/cc as first-class value
        BuiltinProc callCCBuiltin = args -> {
            if (args.size() != 1) throw error("call/cc: expected 1 argument");
            if (hasPendingCallCC) {
                hasPendingCallCC = false;
                Object val = pendingCallCCValue;
                pendingCallCCValue = null;
                replayEnv = null;
                return val;
            }
            Object proc = args.get(0);
            SchemeContinuation k = new SchemeContinuation();
            k.topLevelIndex = currentTopLevelIndex;
            k.bodyExprs = currentBodyExprs;
            k.bodyEnv = currentBodyEnv;
            k.savedWindStack = new ArrayList<>(windStack);
            k.savedFrameStack = copyFrameStack();
            try {
                Object ccResult = apply(proc, List.of(k));
                k.active = false;
                return ccResult;
            } catch (ContinuationInvoked ci) {
                k.active = false;
                if (ci.continuation == k) return ci.value;
                throw ci;
            }
        };
        globalEnv.define("call/cc", callCCBuiltin);
        globalEnv.define("call-with-current-continuation", callCCBuiltin);

        // values: return multiple values (single value is transparent)
        globalEnv.define("values", (BuiltinProc) args -> {
            if (args.size() == 1) return args.get(0);
            return new SchemeValues(args);
        });

        // call-with-values: (call-with-values producer consumer)
        globalEnv.define("call-with-values", (BuiltinProc) args -> {
            if (args.size() != 2) throw error("call-with-values: expected 2 arguments");
            Object producer = args.get(0);
            Object consumer = args.get(1);
            Object produced = apply(producer, List.of());
            List<Object> vals;
            if (produced instanceof SchemeValues sv) {
                vals = sv.values;
            } else {
                vals = List.of(produced);
            }
            return apply(consumer, vals);
        });
    }

    // Convert any numeric value to Rational
    private Rational toRational(Object val) throws EvalError {
        if (val instanceof Long l) return Rational.of(l, 1);
        if (val instanceof Rational r) return r;
        throw error("not an exact number");
    }

    private boolean isExact(Object val) {
        return val instanceof Long || val instanceof Rational;
    }

    private boolean isNumber(Object val) {
        return val instanceof Long || val instanceof Double || val instanceof Rational;
    }

    private double toDouble(Object val) throws EvalError {
        if (val instanceof Long l) return l.doubleValue();
        if (val instanceof Double d) return d;
        if (val instanceof Rational r) return r.toDouble();
        throw error("not a number");
    }

    private long numToLong(Object val) throws EvalError {
        if (val instanceof Long l) return l;
        if (val instanceof Double d) return (long) d.doubleValue();
        if (val instanceof Rational r) return r.toLong();
        throw error("not a number");
    }

    // Normalize: if Rational with denom==1, return Long
    private Object normalizeExact(Rational r) {
        return r.isInteger() ? r.toLong() : r;
    }

    // Arithmetic on already-evaluated args
    private Object arith(List<Object> args, String op) throws EvalError {
        if (args.isEmpty()) {
            if (op.equals("+")) return 0L;
            if (op.equals("*")) return 1L;
            throw error(op + ": need at least 1 argument");
        }
        // Check all args are numbers
        for (Object a : args) {
            if (!isNumber(a)) throw error(op + ": not a number");
        }
        // Unary minus
        if (op.equals("-") && args.size() == 1) {
            Object val = args.get(0);
            if (val instanceof Long l) return -l;
            if (val instanceof Rational r) return normalizeExact(r.negate());
            return -((Double) val);
        }
        // Check if any arg is inexact (Double)
        boolean inexact = false;
        for (Object a : args) { if (a instanceof Double) { inexact = true; break; } }
        if (inexact) {
            double result = toDouble(args.get(0));
            for (int i = 1; i < args.size(); i++) {
                double v = toDouble(args.get(i));
                switch (op) {
                    case "+" -> result += v;
                    case "-" -> result -= v;
                    case "*" -> result *= v;
                    case "/" -> { if (v == 0) throw error("division by zero"); result /= v; }
                }
            }
            return result;
        }
        // All exact: use Rational arithmetic
        Rational result = toRational(args.get(0));
        for (int i = 1; i < args.size(); i++) {
            Rational v = toRational(args.get(i));
            switch (op) {
                case "+" -> result = result.add(v);
                case "-" -> result = result.subtract(v);
                case "*" -> result = result.multiply(v);
                case "/" -> {
                    if (v.numerator() == 0) throw error("division by zero");
                    result = result.divide(v);
                }
            }
        }
        return normalizeExact(result);
    }

    private Object cmp(List<Object> args, String op) throws EvalError {
        if (args.size() != 2) throw error(op + ": expected 2 arguments");
        Object a = args.get(0), b = args.get(1);
        if (!isNumber(a) || !isNumber(b)) throw error(op + ": not a number");
        // If both exact, compare as rationals to avoid floating point issues
        if (isExact(a) && isExact(b)) {
            Rational ra = toRational(a), rb = toRational(b);
            long diff = ra.numerator() * rb.denominator() - rb.numerator() * ra.denominator();
            return switch (op) {
                case "<" -> diff < 0;
                case ">" -> diff > 0;
                case "=" -> diff == 0;
                case "<=" -> diff <= 0;
                case ">=" -> diff >= 0;
                default -> false;
            };
        }
        double da = toDouble(a), db = toDouble(b);
        return switch (op) {
            case "<" -> da < db;
            case ">" -> da > db;
            case "=" -> da == db;
            case "<=" -> da <= db;
            case ">=" -> da >= db;
            default -> false;
        };
    }

    private void checkArgs(List<?> list, int expected, String name) throws EvalError {
        if (list.size() - 1 != expected) {
            throw error(name + ": expected " + expected + " arguments, got " + (list.size() - 1));
        }
    }

    private boolean schemeEqual(Object a, Object b) {
        return schemeEqualImpl(a, b, java.util.Collections.newSetFromMap(new java.util.IdentityHashMap<>()));
    }

    private boolean schemeEqualImpl(Object a, Object b, Set<Object> seen) {
        if (a == b) return true;
        if (a instanceof Long && b instanceof Long) return a.equals(b);
        if (a instanceof Rational && b instanceof Rational) return a.equals(b);
        if (a instanceof Double && b instanceof Double) return a.equals(b);
        if (a instanceof Boolean && b instanceof Boolean) return a.equals(b);
        if (a instanceof String && b instanceof String) return a.equals(b);
        if (a instanceof SchemeString sa && b instanceof SchemeString sb) return sa.value().equals(sb.value());
        if (a instanceof SchemeChar ca && b instanceof SchemeChar cb) return ca.value() == cb.value();
        if (a instanceof Pair pa && b instanceof Pair pb) {
            if (!seen.add(pa)) return true; // cycle — assume equal
            boolean result = schemeEqualImpl(pa.car, pb.car, seen) && schemeEqualImpl(pa.cdr, pb.cdr, seen);
            return result;
        }
        if (a == NIL && b == NIL) return true;
        if (a instanceof SchemeVector va && b instanceof SchemeVector vb) {
            if (va.length() != vb.length()) return false;
            for (int i = 0; i < va.length(); i++) {
                if (!schemeEqualImpl(va.ref(i), vb.ref(i), seen)) return false;
            }
            return true;
        }
        return false;
    }

    // --- Output formatting ---

    @SuppressWarnings("unchecked")
    static String schemeToString(Object val) {
        return schemeToStringImpl(val, java.util.Collections.newSetFromMap(new java.util.IdentityHashMap<>()));
    }

    private static String schemeToStringImpl(Object val, Set<Object> seen) {
        if (val instanceof Long) return val.toString();
        if (val instanceof Rational r) return r.toString();
        if (val instanceof Double d) {
            if (d == Math.floor(d) && !Double.isInfinite(d) && Math.abs(d) < 1e15) {
                return String.valueOf(d);
            }
            return String.valueOf(d);
        }
        if (val instanceof Boolean b) return b ? "#t" : "#f";
        if (val instanceof SchemeString s) return "\"" + s.value() + "\"";
        if (val instanceof SchemeChar ch) return "#\\" + ch.value();
        if (val instanceof Lambda) return "#<procedure>";
        if (val instanceof CaseLambda) return "#<procedure>";
        if (val instanceof SchemeContinuation) return "#<continuation>";
        if (val instanceof SchemeVector v) {
            if (!seen.add(v)) return "#<circular>";
            StringBuilder sb = new StringBuilder("#(");
            for (int i = 0; i < v.length(); i++) {
                if (i > 0) sb.append(" ");
                sb.append(schemeToStringImpl(v.ref(i), seen));
            }
            sb.append(")");
            return sb.toString();
        }
        if (val == NIL) return "()";
        if (val instanceof Pair) {
            if (!seen.add(val)) return "#<circular>";
            StringBuilder sb = new StringBuilder("(");
            Object cur = val;
            boolean first = true;
            while (cur instanceof Pair p) {
                if (!first) {
                    if (!seen.add(cur)) { sb.append(" . #<circular>"); break; }
                    sb.append(" ");
                }
                first = false;
                sb.append(schemeToStringImpl(p.car, seen));
                cur = p.cdr;
            }
            if (cur != NIL && !(cur instanceof Pair)) {
                sb.append(" . ");
                sb.append(schemeToStringImpl(cur, seen));
            }
            sb.append(")");
            return sb.toString();
        }
        if (val instanceof List<?> list) {
            StringBuilder sb = new StringBuilder("(");
            for (int i = 0; i < list.size(); i++) {
                if (i > 0) sb.append(" ");
                sb.append(schemeToStringImpl(list.get(i), seen));
            }
            sb.append(")");
            return sb.toString();
        }
        if (val instanceof String s) return s; // symbol
        return val.toString();
    }

    static String displayString(Object val) {
        return displayStringImpl(val, java.util.Collections.newSetFromMap(new java.util.IdentityHashMap<>()));
    }

    private static String displayStringImpl(Object val, Set<Object> seen) {
        if (val instanceof SchemeString s) return s.value();
        if (val instanceof SchemeChar ch) return String.valueOf(ch.value());
        if (val instanceof SchemeVector v) {
            if (!seen.add(v)) return "#<circular>";
            StringBuilder sb = new StringBuilder("#(");
            for (int i = 0; i < v.length(); i++) {
                if (i > 0) sb.append(" ");
                sb.append(displayStringImpl(v.ref(i), seen));
            }
            sb.append(")");
            return sb.toString();
        }
        if (val instanceof Pair) {
            if (!seen.add(val)) return "#<circular>";
            StringBuilder sb = new StringBuilder("(");
            Object cur = val;
            boolean first = true;
            while (cur instanceof Pair p) {
                if (!first) {
                    if (!seen.add(cur)) { sb.append(" . #<circular>"); break; }
                    sb.append(" ");
                }
                first = false;
                sb.append(displayStringImpl(p.car, seen));
                cur = p.cdr;
            }
            if (cur != NIL && !(cur instanceof Pair)) {
                sb.append(" . ");
                sb.append(displayStringImpl(cur, seen));
            }
            sb.append(")");
            return sb.toString();
        }
        return schemeToStringImpl(val, seen);
    }
}
