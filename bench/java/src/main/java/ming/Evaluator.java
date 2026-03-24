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

    private static final Set<String> SPECIAL_FORMS = Set.of(
        "if", "let", "begin", "set!", "define", "lambda", "quote", "cond",
        "and", "or", "not", "define-syntax", "syntax-rules"
    );

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

        if (expr instanceof MacroExpansion me) {
            return eval(me.form(), me.env());
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
                    case "define-syntax" -> { return evalDefineSyntax(list, env); }
                }
                // Check for macro usage
                try {
                    Object headVal = env.lookup(op);
                    if (headVal instanceof SyntaxRulesDef sr) {
                        Object expanded = expandMacro(sr, list, env);
                        return eval(expanded, env);
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
            Lambda lambda = new Lambda(params, null, body, letEnv);
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

    // --- Macros ---

    @SuppressWarnings("unchecked")
    private Object evalDefineSyntax(List<?> list, Environment env) throws EvalError {
        if (list.size() != 3) throw error("define-syntax: bad syntax");
        Object nameObj = list.get(1);
        if (nameObj instanceof Located loc) nameObj = loc.value();
        if (!(nameObj instanceof String name)) throw error("define-syntax: expected name");

        Object transformer = list.get(2);
        if (transformer instanceof Located loc) transformer = loc.value();
        if (!(transformer instanceof List<?> tlist) || tlist.size() < 2)
            throw error("define-syntax: expected syntax-rules");

        Object srHead = tlist.get(0);
        if (srHead instanceof Located loc) srHead = loc.value();
        if (!"syntax-rules".equals(srHead)) throw error("define-syntax: expected syntax-rules");

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

                // Bind gensyms for symbols that exist in definition env
                Environment wrapEnv = new Environment(useEnv);
                for (Map.Entry<String, String> entry : renameMap.entrySet()) {
                    try {
                        Object val = sr.defEnv.lookup(entry.getKey());
                        wrapEnv.define(entry.getValue(), val);
                    } catch (EvalError ignored) {
                        // Symbol doesn't exist in def env (e.g., tmp in swap!)
                        // Leave unbound - will be bound by the expansion itself (e.g., let)
                    }
                }

                // If we created any gensym bindings, wrap the form to eval in that env
                if (!renameMap.isEmpty()) {
                    // Return a special wrapper that eval handles
                    return new MacroExpansion(expanded, wrapEnv);
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

        // eq? (identity/value equality for symbols, numbers, booleans, chars)
        globalEnv.define("eq?", (BuiltinProc) args -> {
            if (args.size() != 2) throw error("eq?: expected 2 arguments");
            Object a = args.get(0), b = args.get(1);
            if (a == b) return true;
            if (a instanceof Long && b instanceof Long) return a.equals(b);
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

        // Numeric utilities
        globalEnv.define("abs", (BuiltinProc) args -> {
            if (args.size() != 1) throw error("abs: expected 1 argument");
            if (!(args.get(0) instanceof Long n)) throw error("abs: not a number");
            return Math.abs(n);
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
            if (!(args.get(0) instanceof Long n)) throw error("zero?: not a number");
            return n == 0L;
        });
        globalEnv.define("positive?", (BuiltinProc) args -> {
            if (args.size() != 1) throw error("positive?: expected 1 argument");
            if (!(args.get(0) instanceof Long n)) throw error("positive?: not a number");
            return n > 0L;
        });
        globalEnv.define("negative?", (BuiltinProc) args -> {
            if (args.size() != 1) throw error("negative?: expected 1 argument");
            if (!(args.get(0) instanceof Long n)) throw error("negative?: not a number");
            return n < 0L;
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
            Object cur = args.get(0);
            while (cur instanceof Pair p) {
                cur = p.cdr;
            }
            return cur == NIL;
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

    private boolean schemeEqual(Object a, Object b) {
        if (a == b) return true;
        if (a instanceof Long && b instanceof Long) return a.equals(b);
        if (a instanceof Boolean && b instanceof Boolean) return a.equals(b);
        if (a instanceof String && b instanceof String) return a.equals(b);
        if (a instanceof SchemeString sa && b instanceof SchemeString sb) return sa.value().equals(sb.value());
        if (a instanceof SchemeChar ca && b instanceof SchemeChar cb) return ca.value() == cb.value();
        if (a instanceof Pair pa && b instanceof Pair pb) {
            return schemeEqual(pa.car, pb.car) && schemeEqual(pa.cdr, pb.cdr);
        }
        if (a == NIL && b == NIL) return true;
        return false;
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
