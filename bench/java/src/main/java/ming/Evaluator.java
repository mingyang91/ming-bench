package ming;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.IdentityHashMap;
import java.util.List;
import java.util.HashSet;
import java.util.Map;
import java.util.Set;

public class Evaluator {

    // ---- Value types ----
    sealed interface SchemeVal permits IntVal, BoolVal, StrVal, CharVal, ListVal, SymbolVal, LambdaVal, VoidVal, BuiltinVal, ContinuationVal, MacroVal {}
    record IntVal(long value) implements SchemeVal {}
    record BoolVal(boolean value) implements SchemeVal {}
    static final class StrVal implements SchemeVal {
        private char[] chars;
        StrVal(String value) { this.chars = value.toCharArray(); }
        String value() { return new String(chars); }
        char charAt(int i) { return chars[i]; }
        void setChar(int i, char c) { chars[i] = c; }
        int length() { return chars.length; }
    }
    record CharVal(char value) implements SchemeVal {}
    static final class ListVal implements SchemeVal {
        final List<SchemeVal> elements;
        final boolean improper;
        ListVal(List<SchemeVal> elements) { this(elements, false); }
        ListVal(List<SchemeVal> elements, boolean improper) { this.elements = elements; this.improper = improper; }
        List<SchemeVal> elements() { return elements; }
    }
    record SymbolVal(String name) implements SchemeVal {}
    record VoidVal() implements SchemeVal {}
    record LambdaVal(List<String> params, String restParam, List<SchemeVal> body, Env env) implements SchemeVal {}
    record BuiltinVal(String name) implements SchemeVal {}
    static final class ContinuationVal implements SchemeVal {
        final Object tag = new Object();
        final int topLevelIndex;
        final List<BodyFrame> frames;
        ContinuationVal(int topLevelIndex, List<BodyFrame> frames) {
            this.topLevelIndex = topLevelIndex;
            this.frames = frames;
        }
    }

    static class BodyFrame {
        final List<SchemeVal> exprs;
        int idx;
        final Env env;
        BodyFrame(List<SchemeVal> exprs, int idx, Env env) {
            this.exprs = exprs; this.idx = idx; this.env = env;
        }
        BodyFrame copy() { return new BodyFrame(exprs, idx, env); }
    }

    record MacroVal(String name, List<List<SchemeVal>> rules, Env defEnv, List<String> literals) implements SchemeVal {}

    private static final SchemeVal VOID = new VoidVal();

    // ---- Continuation support ----
    static class ContinuationReturn extends RuntimeException {
        final Object tag;
        final SchemeVal value;
        final int topLevelIndex;
        final List<BodyFrame> frames;
        ContinuationReturn(ContinuationVal cont, SchemeVal value) {
            super(null, null, true, false);
            this.tag = cont.tag;
            this.value = value;
            this.topLevelIndex = cont.topLevelIndex;
            this.frames = cont.frames;
        }
    }
    private SchemeVal pendingReturn = null;
    private int currentTopLevelIndex = 0;
    // Body frame stack for continuation capture
    private final List<BodyFrame> bodyFrameStack = new ArrayList<>();

    // ---- Gensym for macro hygiene ----
    private int gensymCounter = 0;
    private String gensym(String base) { return base + "##" + (gensymCounter++); }

    // ---- Output capture ----
    private StringBuilder outputBuffer;

    // ---- Source positions ----
    record SourcePos(int line, int col) {}
    record Token(String text, int line, int col) {}

    private IdentityHashMap<SchemeVal, SourcePos> posMap;
    private SourcePos currentPos;

    // ---- Environment ----
    static class Env {
        final Map<String, SchemeVal> bindings = new HashMap<>();
        final Env parent;
        Env(Env parent) { this.parent = parent; }

        SchemeVal get(String name) throws EvalError {
            if (bindings.containsKey(name)) return bindings.get(name);
            if (parent != null) return parent.get(name);
            throw new EvalError("unbound variable: " + name);
        }

        void define(String name, SchemeVal val) {
            bindings.put(name, val);
        }

        void set(String name, SchemeVal val) throws EvalError {
            if (bindings.containsKey(name)) { bindings.put(name, val); return; }
            if (parent != null) { parent.set(name, val); return; }
            throw new EvalError("unbound variable: " + name);
        }
    }

    private static Env makeGlobalEnv() {
        Env env = new Env(null);
        String[] builtins = {"+", "-", "*", "/", "<", ">", "=", "<=", ">=", "not",
            "cons", "car", "cdr", "null?", "list", "length", "append",
            "string?", "number?", "boolean?", "pair?", "symbol?", "procedure?", "integer?",
            "display", "write", "newline",
            "string-append", "string-length", "substring", "string->number", "number->string",
            "symbol->string", "string->symbol", "string-ref", "char?",
            "string-copy", "string-set!", "apply",
            "call/cc", "call-with-current-continuation",
            "abs", "modulo", "remainder", "quotient", "min", "max", "expt",
            "zero?", "positive?", "negative?", "odd?", "even?",
            "list-ref", "list-tail", "list?", "assoc", "map",
            "eq?", "equal?",
            "char-alphabetic?", "char-numeric?", "char-upcase", "char-downcase", "char=?", "char<?",
            "string=?", "string<?", "string-ci=?", "string-upcase", "string-downcase"};
        for (String b : builtins) {
            env.define(b, new BuiltinVal(b));
        }
        return env;
    }

    // ---- Tokenizer ----
    private static List<Token> tokenize(String input) {
        List<Token> tokens = new ArrayList<>();
        int i = 0;
        int line = 1, col = 1;
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
            } else if (c == '(') {
                tokens.add(new Token("(", line, col));
                i++;
                col++;
            } else if (c == ')') {
                tokens.add(new Token(")", line, col));
                i++;
                col++;
            } else if (c == '\'') {
                tokens.add(new Token("'", line, col));
                i++;
                col++;
            } else if (c == '"') {
                int startCol = col;
                StringBuilder sb = new StringBuilder();
                sb.append('"');
                i++;
                col++;
                while (i < input.length() && input.charAt(i) != '"') {
                    if (input.charAt(i) == '\\') {
                        sb.append(input.charAt(i));
                        i++;
                        col++;
                        if (i < input.length()) {
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
                if (i < input.length()) {
                    sb.append('"');
                    i++;
                    col++;
                }
                tokens.add(new Token(sb.toString(), line, startCol));
            } else {
                int startCol = col;
                StringBuilder sb = new StringBuilder();
                while (i < input.length() && !Character.isWhitespace(input.charAt(i))
                        && input.charAt(i) != '(' && input.charAt(i) != ')'
                        && input.charAt(i) != '"' && input.charAt(i) != ';') {
                    sb.append(input.charAt(i));
                    i++;
                    col++;
                }
                tokens.add(new Token(sb.toString(), line, startCol));
            }
        }
        return tokens;
    }

    // ---- Parser ----
    private SchemeVal parse(List<Token> tokens, int[] pos) throws EvalError {
        if (pos[0] >= tokens.size()) {
            throw new EvalError("unexpected end of input");
        }
        Token token = tokens.get(pos[0]);
        if (token.text().equals("'")) {
            pos[0]++;
            SchemeVal quoted = parse(tokens, pos);
            List<SchemeVal> quoteExpr = new ArrayList<>();
            SymbolVal quoteSym = new SymbolVal("quote");
            quoteExpr.add(quoteSym);
            quoteExpr.add(quoted);
            ListVal result = new ListVal(quoteExpr);
            posMap.put(quoteSym, new SourcePos(token.line(), token.col()));
            posMap.put(result, new SourcePos(token.line(), token.col()));
            return result;
        } else if (token.text().equals("(")) {
            pos[0]++;
            List<SchemeVal> elems = new ArrayList<>();
            while (pos[0] < tokens.size() && !tokens.get(pos[0]).text().equals(")")) {
                elems.add(parse(tokens, pos));
            }
            if (pos[0] >= tokens.size()) {
                throw new EvalError("missing closing parenthesis");
            }
            pos[0]++;
            ListVal result = new ListVal(elems);
            posMap.put(result, new SourcePos(token.line(), token.col()));
            return result;
        } else if (token.text().equals(")")) {
            throw new EvalError("unexpected )");
        } else {
            pos[0]++;
            SchemeVal result = parseAtom(token.text());
            posMap.put(result, new SourcePos(token.line(), token.col()));
            return result;
        }
    }

    private static SchemeVal parseAtom(String token) {
        if (token.equals("#t")) return new BoolVal(true);
        if (token.equals("#f")) return new BoolVal(false);
        if (token.startsWith("#\\")) {
            String charName = token.substring(2);
            if (charName.length() == 1) return new CharVal(charName.charAt(0));
            return switch (charName.toLowerCase()) {
                case "space" -> new CharVal(' ');
                case "newline" -> new CharVal('\n');
                case "tab" -> new CharVal('\t');
                default -> new CharVal(charName.charAt(0));
            };
        }
        if (token.startsWith("\"")) {
            String inner = token.substring(1, token.length() - 1);
            inner = inner.replace("\\n", "\n").replace("\\t", "\t")
                         .replace("\\\\", "\\").replace("\\\"", "\"");
            return new StrVal(inner);
        }
        try {
            return new IntVal(Long.parseLong(token));
        } catch (NumberFormatException e) {
            return new SymbolVal(token);
        }
    }

    // ---- Error helper ----
    private EvalError posError(String msg, SourcePos pos) {
        if (pos != null) {
            return new EvalError(msg + " at " + pos.line() + ":" + pos.col());
        }
        return new EvalError(msg);
    }

    private static boolean hasPosition(EvalError e) {
        return e.getMessage().matches(".*\\d+:\\d+.*");
    }

    // ---- Evaluator (trampolined for TCO) ----
    private SchemeVal eval(SchemeVal expr, Env env) throws EvalError {
        int frameBase = bodyFrameStack.size();
        try { return evalInner(expr, env); }
        finally { while (bodyFrameStack.size() > frameBase) bodyFrameStack.removeLast(); }
    }

    private SchemeVal evalInner(SchemeVal expr, Env env) throws EvalError {
        trampoline:
        while (true) {
            SourcePos pos = posMap != null ? posMap.get(expr) : null;
            if (pos != null) currentPos = pos;
            SourcePos savedPos = currentPos;
            try {
                if (expr instanceof IntVal || expr instanceof BoolVal || expr instanceof StrVal
                        || expr instanceof CharVal || expr instanceof VoidVal || expr instanceof LambdaVal
                        || expr instanceof BuiltinVal) {
                    return expr;
                }
                if (expr instanceof SymbolVal sym) {
                    return env.get(sym.name());
                }
                if (!(expr instanceof ListVal listExpr)) {
                    throw new EvalError("unexpected expression type");
                }

                List<SchemeVal> elems = listExpr.elements();
                if (elems.isEmpty()) throw new EvalError("empty application");
                SchemeVal head = elems.getFirst();

                // Special forms
                if (head instanceof SymbolVal sym) {
                    switch (sym.name()) {
                        case "quote": {
                            if (elems.size() != 2) throw new EvalError("quote requires 1 argument");
                            return elems.get(1);
                        }
                        case "if": {
                            if (elems.size() < 3 || elems.size() > 4)
                                throw new EvalError("if requires 2 or 3 arguments");
                            SchemeVal cond = eval(elems.get(1), env);
                            if (!isFalse(cond)) {
                                expr = elems.get(2);
                                continue trampoline;
                            } else if (elems.size() == 4) {
                                expr = elems.get(3);
                                continue trampoline;
                            } else {
                                return VOID;
                            }
                        }
                        case "define": {
                            if (elems.size() < 3) throw new EvalError("define requires at least 2 arguments");
                            SchemeVal target = elems.get(1);
                            if (target instanceof SymbolVal name) {
                                SchemeVal val = eval(elems.get(2), env);
                                env.define(name.name(), val);
                                return VOID;
                            } else if (target instanceof ListVal nameAndParams) {
                                if (nameAndParams.elements().isEmpty())
                                    throw new EvalError("define: empty name list");
                                String fname = ((SymbolVal) nameAndParams.elements().getFirst()).name();
                                List<String> params = new ArrayList<>();
                                String restParam = null;
                                List<SchemeVal> pElems = nameAndParams.elements();
                                for (int i = 1; i < pElems.size(); i++) {
                                    String pName = ((SymbolVal) pElems.get(i)).name();
                                    if (pName.equals(".")) {
                                        if (i + 1 < pElems.size()) {
                                            restParam = ((SymbolVal) pElems.get(i + 1)).name();
                                        }
                                        break;
                                    }
                                    params.add(pName);
                                }
                                List<SchemeVal> body = new ArrayList<>(elems.subList(2, elems.size()));
                                env.define(fname, new LambdaVal(params, restParam, body, env));
                                return VOID;
                            } else {
                                throw new EvalError("define: invalid syntax");
                            }
                        }
                        case "lambda": {
                            if (elems.size() < 3) throw new EvalError("lambda requires params and body");
                            SchemeVal paramSpec = elems.get(1);
                            List<String> params = new ArrayList<>();
                            String restParam = null;
                            if (paramSpec instanceof ListVal pl) {
                                for (int i = 0; i < pl.elements().size(); i++) {
                                    String pName = ((SymbolVal) pl.elements().get(i)).name();
                                    if (pName.equals(".")) {
                                        if (i + 1 < pl.elements().size()) {
                                            restParam = ((SymbolVal) pl.elements().get(i + 1)).name();
                                        }
                                        break;
                                    }
                                    params.add(pName);
                                }
                            } else if (paramSpec instanceof SymbolVal restOnly) {
                                restParam = restOnly.name();
                            } else {
                                throw new EvalError("lambda: invalid parameter list");
                            }
                            List<SchemeVal> body = new ArrayList<>(elems.subList(2, elems.size()));
                            return new LambdaVal(params, restParam, body, env);
                        }
                        case "and": {
                            if (elems.size() == 1) return new BoolVal(true);
                            for (int i = 1; i < elems.size() - 1; i++) {
                                SchemeVal result = eval(elems.get(i), env);
                                if (isFalse(result)) return result;
                            }
                            expr = elems.getLast();
                            continue trampoline;
                        }
                        case "or": {
                            if (elems.size() == 1) return new BoolVal(false);
                            for (int i = 1; i < elems.size() - 1; i++) {
                                SchemeVal result = eval(elems.get(i), env);
                                if (!isFalse(result)) return result;
                            }
                            expr = elems.getLast();
                            continue trampoline;
                        }
                        case "let": {
                            if (elems.size() < 3) throw new EvalError("let requires bindings and body");
                            SchemeVal second = elems.get(1);
                            // Named let: (let name ((var init) ...) body...)
                            if (second instanceof SymbolVal loopName) {
                                if (elems.size() < 4) throw new EvalError("named let requires bindings and body");
                                SchemeVal bindingsVal = elems.get(2);
                                if (!(bindingsVal instanceof ListVal bl)) throw new EvalError("let: invalid bindings");
                                List<String> params = new ArrayList<>();
                                List<SchemeVal> inits = new ArrayList<>();
                                for (SchemeVal binding : bl.elements()) {
                                    if (!(binding instanceof ListVal bpair) || bpair.elements().size() != 2)
                                        throw new EvalError("let: invalid binding");
                                    params.add(((SymbolVal) bpair.elements().get(0)).name());
                                    inits.add(bpair.elements().get(1));
                                }
                                List<SchemeVal> body = new ArrayList<>(elems.subList(3, elems.size()));
                                Env letEnv = new Env(env);
                                LambdaVal loopFn = new LambdaVal(params, null, body, letEnv);
                                letEnv.define(loopName.name(), loopFn);
                                // Apply the loop function with TCO
                                Env callEnv = new Env(letEnv);
                                for (int i = 0; i < params.size(); i++) {
                                    callEnv.define(params.get(i), eval(inits.get(i), env));
                                }
                                bodyFrameStack.add(new BodyFrame(body, 0, callEnv));
                                for (int j = 0; j < body.size() - 1; j++) {
                                    bodyFrameStack.getLast().idx = j;
                                    eval(body.get(j), callEnv);
                                }
                                bodyFrameStack.getLast().idx = body.size() - 1;
                                // Don't pop - eval's try-finally handles cleanup
                                expr = body.getLast();
                                env = callEnv;
                                continue trampoline;
                            }
                            // Regular let: (let ((var init) ...) body...)
                            if (!(second instanceof ListVal bl)) throw new EvalError("let: invalid bindings");
                            Env letEnv = new Env(env);
                            for (SchemeVal binding : bl.elements()) {
                                if (!(binding instanceof ListVal bpair) || bpair.elements().size() != 2)
                                    throw new EvalError("let: invalid binding");
                                String varName = ((SymbolVal) bpair.elements().get(0)).name();
                                SchemeVal val = eval(bpair.elements().get(1), env);
                                letEnv.define(varName, val);
                            }
                            List<SchemeVal> letBody = new ArrayList<>(elems.subList(2, elems.size()));
                            bodyFrameStack.add(new BodyFrame(letBody, 0, letEnv));
                            for (int i = 0; i < letBody.size() - 1; i++) {
                                bodyFrameStack.getLast().idx = i;
                                eval(letBody.get(i), letEnv);
                            }
                            bodyFrameStack.getLast().idx = letBody.size() - 1;
                            // Don't pop - eval's try-finally handles cleanup
                            expr = letBody.getLast();
                            env = letEnv;
                            continue trampoline;
                        }
                        case "set!": {
                            if (elems.size() != 3) throw new EvalError("set! requires 2 arguments");
                            if (!(elems.get(1) instanceof SymbolVal target))
                                throw new EvalError("set!: not a symbol");
                            SchemeVal val = eval(elems.get(2), env);
                            env.set(target.name(), val);
                            return VOID;
                        }
                        case "begin": {
                            if (elems.size() == 1) return VOID;
                            for (int i = 1; i < elems.size() - 1; i++) {
                                eval(elems.get(i), env);
                            }
                            expr = elems.getLast();
                            continue trampoline;
                        }
                        case "cond": {
                            SchemeVal condResult = VOID;
                            for (int i = 1; i < elems.size(); i++) {
                                if (!(elems.get(i) instanceof ListVal clause) || clause.elements().isEmpty())
                                    throw new EvalError("cond: invalid clause");
                                SchemeVal test = clause.elements().get(0);
                                boolean isElse = test instanceof SymbolVal s && s.name().equals("else");
                                SchemeVal testResult = isElse ? new BoolVal(true) : eval(test, env);
                                if (!isFalse(testResult)) {
                                    if (clause.elements().size() == 1) return testResult;
                                    for (int j = 1; j < clause.elements().size() - 1; j++) {
                                        eval(clause.elements().get(j), env);
                                    }
                                    expr = clause.elements().getLast();
                                    continue trampoline;
                                }
                            }
                            return condResult;
                        }
                        case "call/cc":
                        case "call-with-current-continuation": {
                            if (elems.size() != 2) throw new EvalError("call/cc requires 1 argument");
                            SchemeVal proc = eval(elems.get(1), env);
                            return doCallCC(proc);
                        }
                        case "define-syntax": {
                            if (elems.size() != 3) throw new EvalError("define-syntax requires 2 arguments");
                            String macroName = ((SymbolVal) elems.get(1)).name();
                            SchemeVal syntaxRulesExpr = elems.get(2);
                            if (!(syntaxRulesExpr instanceof ListVal srList))
                                throw new EvalError("define-syntax: expected syntax-rules");
                            List<SchemeVal> srElems = srList.elements();
                            if (srElems.isEmpty() || !(srElems.get(0) instanceof SymbolVal srSym)
                                    || !srSym.name().equals("syntax-rules"))
                                throw new EvalError("define-syntax: expected syntax-rules");
                            List<String> literals = new ArrayList<>();
                            if (srElems.get(1) instanceof ListVal litList) {
                                for (SchemeVal lit : litList.elements())
                                    literals.add(((SymbolVal) lit).name());
                            }
                            List<List<SchemeVal>> rules = new ArrayList<>();
                            for (int i = 2; i < srElems.size(); i++) {
                                ListVal rule = (ListVal) srElems.get(i);
                                rules.add(rule.elements());
                            }
                            env.define(macroName, new MacroVal(macroName, rules, env, literals));
                            return VOID;
                        }
                        default: {
                            // Check if it's a macro
                            try {
                                SchemeVal maybeM = env.get(sym.name());
                                if (maybeM instanceof MacroVal macro) {
                                    expr = expandMacro(macro, listExpr, env);
                                    continue trampoline;
                                }
                            } catch (EvalError ignored) {}
                            // fall through to procedure call
                            break;
                        }
                    }
                }

                // Procedure call
                SchemeVal proc = eval(head, env);
                List<SchemeVal> args = new ArrayList<>();
                for (int i = 1; i < elems.size(); i++) {
                    args.add(eval(elems.get(i), env));
                }
                if (proc instanceof LambdaVal lambda) {
                    Env callEnv = bindLambdaArgs(lambda, args);
                    boolean pushFrame = lambda.body().size() > 1 && !bodyFrameStack.isEmpty();
                    if (pushFrame) {
                        bodyFrameStack.add(new BodyFrame(lambda.body(), 0, callEnv));
                    }
                    for (int j = 0; j < lambda.body().size() - 1; j++) {
                        if (pushFrame) bodyFrameStack.getLast().idx = j;
                        eval(lambda.body().get(j), callEnv);
                    }
                    if (pushFrame) bodyFrameStack.getLast().idx = lambda.body().size() - 1;
                    // Don't pop frame - eval's try-finally handles cleanup
                    expr = lambda.body().getLast();
                    env = callEnv;
                    continue trampoline;
                } else if (proc instanceof ContinuationVal cont) {
                    if (args.size() != 1) throw new EvalError("continuation requires exactly 1 argument");
                    throw new ContinuationReturn(cont, args.get(0));
                } else if (proc instanceof BuiltinVal b) {
                    String bname = b.name();
                    if (bname.equals("call/cc") || bname.equals("call-with-current-continuation")) {
                        if (args.size() != 1) throw new EvalError("call/cc requires 1 argument");
                        return doCallCC(args.get(0));
                    }
                    if (bname.equals("apply")) {
                        return doApply(args);
                    }
                    return applyBuiltin(bname, args);
                }
                throw new EvalError("not a procedure: " + display(proc));
            } catch (EvalError e) {
                if (savedPos != null && !hasPosition(e)) {
                    throw posError(e.getMessage(), savedPos);
                }
                throw e;
            }
        }
    }

    private Env bindLambdaArgs(LambdaVal lambda, List<SchemeVal> args) throws EvalError {
        if (lambda.restParam() != null) {
            if (args.size() < lambda.params().size()) {
                throw new EvalError("wrong number of arguments: expected at least " + lambda.params().size() + ", got " + args.size());
            }
        } else {
            if (args.size() != lambda.params().size()) {
                throw new EvalError("wrong number of arguments: expected " + lambda.params().size() + ", got " + args.size());
            }
        }
        Env callEnv = new Env(lambda.env());
        for (int i = 0; i < lambda.params().size(); i++) {
            callEnv.define(lambda.params().get(i), args.get(i));
        }
        if (lambda.restParam() != null) {
            List<SchemeVal> rest = new ArrayList<>(args.subList(lambda.params().size(), args.size()));
            callEnv.define(lambda.restParam(), new ListVal(rest));
        }
        return callEnv;
    }

    private static boolean isDirectCallCC(SchemeVal expr) {
        if (expr instanceof ListVal list && !list.elements().isEmpty()) {
            SchemeVal head = list.elements().getFirst();
            if (head instanceof SymbolVal sym) {
                return sym.name().equals("call/cc") || sym.name().equals("call-with-current-continuation");
            }
        }
        return false;
    }

    private SchemeVal resumeFrames(List<BodyFrame> frames, SchemeVal value) throws EvalError {
        pendingReturn = value;
        SchemeVal result = VOID;
        // frames[0] = outermost, frames[last] = innermost
        int stackBase = bodyFrameStack.size();
        // Push all frames with their original captured indices
        for (BodyFrame frame : frames) {
            bodyFrameStack.add(frame.copy());
        }
        // Resume from innermost to outermost
        for (int f = frames.size() - 1; f >= 0; f--) {
            BodyFrame frame = frames.get(f);
            int startIdx;
            if (f == frames.size() - 1) {
                // Innermost: if the captured expression IS a direct call/cc, skip it;
                // otherwise re-evaluate it (call/cc is nested inside and pendingReturn handles it)
                boolean direct = frame.idx < frame.exprs.size() && isDirectCallCC(frame.exprs.get(frame.idx));
                startIdx = direct ? frame.idx + 1 : frame.idx;
            } else {
                startIdx = frame.idx + 1;
            }
            for (int i = startIdx; i < frame.exprs.size(); i++) {
                bodyFrameStack.get(stackBase + f).idx = i;
                result = eval(frame.exprs.get(i), frame.env);
            }
            if (f == frames.size() - 1) {
                pendingReturn = null; // Clear unconsumed pendingReturn after innermost frame
            }
            // Pop this processed frame
            bodyFrameStack.remove(stackBase + f);
        }
        return result;
    }

    private SchemeVal doCallCC(SchemeVal proc) throws EvalError {
        if (pendingReturn != null) {
            SchemeVal val = pendingReturn;
            pendingReturn = null;
            return val;
        }
        List<BodyFrame> framesCopy = new ArrayList<>();
        for (BodyFrame f : bodyFrameStack) framesCopy.add(f.copy());
        ContinuationVal cont = new ContinuationVal(currentTopLevelIndex, framesCopy);
        try {
            return applyProc(proc, List.of(cont));
        } catch (ContinuationReturn cr) {
            if (cr.tag == cont.tag) {
                return cr.value;
            }
            throw cr;
        }
    }

    private SchemeVal applyProc(SchemeVal proc, List<SchemeVal> args) throws EvalError {
        if (proc instanceof ContinuationVal cont) {
            if (args.size() != 1) throw new EvalError("continuation requires exactly 1 argument");
            throw new ContinuationReturn(cont, args.get(0));
        } else if (proc instanceof BuiltinVal b) {
            String bname = b.name();
            if (bname.equals("call/cc") || bname.equals("call-with-current-continuation")) {
                if (args.size() != 1) throw new EvalError("call/cc requires 1 argument");
                return doCallCC(args.get(0));
            }
            if (bname.equals("apply")) {
                return doApply(args);
            }
            return applyBuiltin(bname, args);
        } else if (proc instanceof LambdaVal lambda) {
            Env callEnv = bindLambdaArgs(lambda, args);
            SchemeVal result = VOID;
            boolean pushFrame = lambda.body().size() > 1;
            if (pushFrame) bodyFrameStack.add(new BodyFrame(lambda.body(), 0, callEnv));
            try {
                for (int i = 0; i < lambda.body().size(); i++) {
                    if (pushFrame) bodyFrameStack.getLast().idx = i;
                    result = eval(lambda.body().get(i), callEnv);
                }
            } finally {
                if (pushFrame) bodyFrameStack.removeLast();
            }
            return result;
        }
        throw new EvalError("not a procedure: " + display(proc));
    }

    // ---- Macro expansion (define-syntax / syntax-rules) ----
    private static final Set<String> SPECIAL_FORMS = Set.of(
        "quote", "if", "define", "lambda", "and", "or", "let", "set!", "begin",
        "cond", "call/cc", "call-with-current-continuation", "define-syntax",
        "let*", "letrec", "letrec*", "do", "case", "when", "unless", "syntax-rules"
    );

    private SchemeVal expandMacro(MacroVal macro, ListVal form, Env useEnv) throws EvalError {
        for (List<SchemeVal> rule : macro.rules()) {
            SchemeVal pattern = rule.get(0);
            SchemeVal template = rule.get(1);
            Map<String, Object> bindings = matchPattern(pattern, form, macro.name(), macro.literals());
            if (bindings != null) {
                Map<String, String> renameMap = new HashMap<>();
                return expandTemplate(template, bindings, macro.defEnv(), useEnv, renameMap, macro.name());
            }
        }
        throw new EvalError("no matching pattern for macro " + macro.name());
    }

    @SuppressWarnings("unchecked")
    private Map<String, Object> matchPattern(SchemeVal pattern, SchemeVal form,
                                              String macroName, List<String> literals) {
        if (pattern instanceof SymbolVal sym) {
            String name = sym.name();
            if (name.equals("_") || name.equals(macroName))
                return new HashMap<>();
            if (literals.contains(name)) {
                if (form instanceof SymbolVal fs && fs.name().equals(name))
                    return new HashMap<>();
                return null;
            }
            Map<String, Object> b = new HashMap<>();
            b.put(name, form);
            return b;
        }
        if (pattern instanceof ListVal patList && form instanceof ListVal formList) {
            List<SchemeVal> pats = patList.elements();
            List<SchemeVal> forms = formList.elements();
            Map<String, Object> bindings = new HashMap<>();
            int pi = 0, fi = 0;
            while (pi < pats.size()) {
                if (pi + 1 < pats.size() && pats.get(pi + 1) instanceof SymbolVal s
                        && s.name().equals("...")) {
                    SchemeVal ellipPat = pats.get(pi);
                    int remainingPats = 0;
                    for (int k = pi + 2; k < pats.size(); k++) {
                        if (k + 1 < pats.size() && pats.get(k + 1) instanceof SymbolVal es
                                && es.name().equals("...")) {
                            k++;
                        } else {
                            remainingPats++;
                        }
                    }
                    int available = forms.size() - fi - remainingPats;
                    if (available < 0) return null;
                    Set<String> ellipVars = patternVars(ellipPat, macroName, literals);
                    Map<String, List<SchemeVal>> ellipBindings = new HashMap<>();
                    for (String v : ellipVars) ellipBindings.put(v, new ArrayList<>());
                    for (int j = 0; j < available; j++) {
                        Map<String, Object> sub = matchPattern(ellipPat, forms.get(fi + j), macroName, literals);
                        if (sub == null) return null;
                        for (String v : ellipVars) {
                            Object val = sub.get(v);
                            if (val instanceof SchemeVal sv) ellipBindings.get(v).add(sv);
                        }
                    }
                    for (var e : ellipBindings.entrySet()) bindings.put(e.getKey(), e.getValue());
                    fi += available;
                    pi += 2;
                } else {
                    if (fi >= forms.size()) return null;
                    Map<String, Object> sub = matchPattern(pats.get(pi), forms.get(fi), macroName, literals);
                    if (sub == null) return null;
                    bindings.putAll(sub);
                    pi++;
                    fi++;
                }
            }
            if (fi != forms.size()) return null;
            return bindings;
        }
        // Literal match
        if (pattern instanceof BoolVal pb && form instanceof BoolVal fb && pb.value() == fb.value())
            return new HashMap<>();
        if (pattern instanceof IntVal pi && form instanceof IntVal fi && pi.value() == fi.value())
            return new HashMap<>();
        return null;
    }

    private Set<String> patternVars(SchemeVal pattern, String macroName, List<String> literals) {
        Set<String> vars = new HashSet<>();
        if (pattern instanceof SymbolVal sym) {
            String name = sym.name();
            if (!name.equals("_") && !name.equals(macroName) && !name.equals("...") && !literals.contains(name))
                vars.add(name);
        } else if (pattern instanceof ListVal list) {
            for (SchemeVal e : list.elements()) vars.addAll(patternVars(e, macroName, literals));
        }
        return vars;
    }

    @SuppressWarnings("unchecked")
    private SchemeVal expandTemplate(SchemeVal template, Map<String, Object> bindings,
                                     Env defEnv, Env useEnv, Map<String, String> renameMap,
                                     String macroName) throws EvalError {
        if (template instanceof SymbolVal sym) {
            String name = sym.name();
            if (bindings.containsKey(name) && bindings.get(name) instanceof SchemeVal sv) return sv;
            if (name.equals("...")) return template;
            if (SPECIAL_FORMS.contains(name) || name.equals(macroName)) return template;
            if (renameMap.containsKey(name)) return new SymbolVal(renameMap.get(name));
            String gs = gensym(name);
            renameMap.put(name, gs);
            try {
                SchemeVal defVal = defEnv.get(name);
                useEnv.define(gs, defVal);
            } catch (EvalError ignored) {}
            return new SymbolVal(gs);
        }
        if (template instanceof ListVal list) {
            List<SchemeVal> expanded = new ArrayList<>();
            List<SchemeVal> elems = list.elements();
            for (int i = 0; i < elems.size(); i++) {
                if (i + 1 < elems.size() && elems.get(i + 1) instanceof SymbolVal s
                        && s.name().equals("...")) {
                    SchemeVal tmpl = elems.get(i);
                    Set<String> ellipVars = findEllipsisVars(tmpl, bindings);
                    if (!ellipVars.isEmpty()) {
                        String firstVar = ellipVars.iterator().next();
                        List<SchemeVal> vals = (List<SchemeVal>) bindings.get(firstVar);
                        for (int j = 0; j < vals.size(); j++) {
                            Map<String, Object> subBindings = new HashMap<>(bindings);
                            for (String v : ellipVars) {
                                List<SchemeVal> vList = (List<SchemeVal>) bindings.get(v);
                                subBindings.put(v, vList.get(j));
                            }
                            expanded.add(expandTemplate(tmpl, subBindings, defEnv, useEnv, renameMap, macroName));
                        }
                    }
                    i++; // skip ...
                } else {
                    expanded.add(expandTemplate(elems.get(i), bindings, defEnv, useEnv, renameMap, macroName));
                }
            }
            return new ListVal(expanded);
        }
        return template;
    }

    @SuppressWarnings("unchecked")
    private Set<String> findEllipsisVars(SchemeVal template, Map<String, Object> bindings) {
        Set<String> vars = new HashSet<>();
        if (template instanceof SymbolVal sym) {
            String name = sym.name();
            if (bindings.containsKey(name) && bindings.get(name) instanceof List)
                vars.add(name);
        } else if (template instanceof ListVal list) {
            for (SchemeVal e : list.elements()) vars.addAll(findEllipsisVars(e, bindings));
        }
        return vars;
    }

    private SchemeVal doApply(List<SchemeVal> args) throws EvalError {
        if (args.size() < 2) throw new EvalError("apply requires at least 2 arguments");
        SchemeVal proc = args.get(0);
        SchemeVal lastArg = args.get(args.size() - 1);
        if (!(lastArg instanceof ListVal lastList)) throw new EvalError("apply: last argument must be a list");
        List<SchemeVal> allArgs = new ArrayList<>();
        for (int i = 1; i < args.size() - 1; i++) {
            allArgs.add(args.get(i));
        }
        allArgs.addAll(lastList.elements());
        return applyProc(proc, allArgs);
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
            case "cons" -> {
                if (args.size() != 2) throw new EvalError("cons requires exactly 2 arguments");
                SchemeVal carVal = args.get(0);
                SchemeVal cdrVal = args.get(1);
                if (cdrVal instanceof ListVal lst) {
                    List<SchemeVal> newElems = new ArrayList<>();
                    newElems.add(carVal);
                    newElems.addAll(lst.elements());
                    yield new ListVal(newElems, lst.improper);
                }
                List<SchemeVal> pair = new ArrayList<>();
                pair.add(carVal);
                pair.add(cdrVal);
                yield new ListVal(pair, true);
            }
            case "car" -> {
                if (args.size() != 1) throw new EvalError("car requires exactly 1 argument");
                if (args.getFirst() instanceof ListVal lst && !lst.elements().isEmpty()) {
                    yield lst.elements().getFirst();
                }
                throw new EvalError("car: not a pair");
            }
            case "cdr" -> {
                if (args.size() != 1) throw new EvalError("cdr requires exactly 1 argument");
                if (args.getFirst() instanceof ListVal lst && !lst.elements().isEmpty()) {
                    if (lst.improper && lst.elements().size() == 2) {
                        yield lst.elements().get(1);
                    }
                    yield new ListVal(new ArrayList<>(lst.elements().subList(1, lst.elements().size())), lst.improper);
                }
                throw new EvalError("cdr: not a pair");
            }
            case "null?" -> {
                if (args.size() != 1) throw new EvalError("null? requires exactly 1 argument");
                yield new BoolVal(args.getFirst() instanceof ListVal lst && lst.elements().isEmpty());
            }
            case "list" -> {
                yield new ListVal(new ArrayList<>(args));
            }
            case "length" -> {
                if (args.size() != 1) throw new EvalError("length requires exactly 1 argument");
                if (args.getFirst() instanceof ListVal lst) {
                    yield new IntVal(lst.elements().size());
                }
                throw new EvalError("length: not a list");
            }
            case "append" -> {
                List<SchemeVal> result = new ArrayList<>();
                boolean improper = false;
                for (int i = 0; i < args.size(); i++) {
                    if (i < args.size() - 1) {
                        if (args.get(i) instanceof ListVal lst) {
                            result.addAll(lst.elements());
                        } else {
                            throw new EvalError("append: not a list");
                        }
                    } else {
                        if (args.get(i) instanceof ListVal lst) {
                            result.addAll(lst.elements());
                            improper = lst.improper;
                        } else {
                            result.add(args.get(i));
                            improper = true;
                        }
                    }
                }
                yield new ListVal(result, improper);
            }
            case "string?" -> {
                if (args.size() != 1) throw new EvalError("string? requires exactly 1 argument");
                yield new BoolVal(args.getFirst() instanceof StrVal);
            }
            case "number?", "integer?" -> {
                if (args.size() != 1) throw new EvalError(name + " requires exactly 1 argument");
                yield new BoolVal(args.getFirst() instanceof IntVal);
            }
            case "boolean?" -> {
                if (args.size() != 1) throw new EvalError("boolean? requires exactly 1 argument");
                yield new BoolVal(args.getFirst() instanceof BoolVal);
            }
            case "pair?" -> {
                if (args.size() != 1) throw new EvalError("pair? requires exactly 1 argument");
                yield new BoolVal(args.getFirst() instanceof ListVal lst && !lst.elements().isEmpty());
            }
            case "symbol?" -> {
                if (args.size() != 1) throw new EvalError("symbol? requires exactly 1 argument");
                yield new BoolVal(args.getFirst() instanceof SymbolVal);
            }
            case "procedure?" -> {
                if (args.size() != 1) throw new EvalError("procedure? requires exactly 1 argument");
                yield new BoolVal(args.getFirst() instanceof LambdaVal || args.getFirst() instanceof BuiltinVal || args.getFirst() instanceof ContinuationVal);
            }
            case "display" -> {
                if (args.size() != 1) throw new EvalError("display requires exactly 1 argument");
                if (outputBuffer != null) outputBuffer.append(displayVal(args.getFirst()));
                yield VOID;
            }
            case "write" -> {
                if (args.size() != 1) throw new EvalError("write requires exactly 1 argument");
                if (outputBuffer != null) outputBuffer.append(display(args.getFirst()));
                yield VOID;
            }
            case "newline" -> {
                if (args.size() != 0) throw new EvalError("newline requires exactly 0 arguments");
                if (outputBuffer != null) outputBuffer.append("\n");
                yield VOID;
            }
            case "string-append" -> {
                StringBuilder sb = new StringBuilder();
                for (SchemeVal a : args) {
                    if (!(a instanceof StrVal s)) throw new EvalError("string-append: not a string");
                    sb.append(s.value());
                }
                yield new StrVal(sb.toString());
            }
            case "string-length" -> {
                if (args.size() != 1) throw new EvalError("string-length requires exactly 1 argument");
                if (!(args.getFirst() instanceof StrVal s)) throw new EvalError("string-length: not a string");
                yield new IntVal(s.value().length());
            }
            case "substring" -> {
                if (args.size() != 3) throw new EvalError("substring requires exactly 3 arguments");
                if (!(args.get(0) instanceof StrVal s)) throw new EvalError("substring: not a string");
                int start = (int) asLong(args.get(1));
                int end = (int) asLong(args.get(2));
                yield new StrVal(s.value().substring(start, end));
            }
            case "string->number" -> {
                if (args.size() != 1) throw new EvalError("string->number requires exactly 1 argument");
                if (!(args.getFirst() instanceof StrVal s)) throw new EvalError("string->number: not a string");
                try {
                    yield new IntVal(Long.parseLong(s.value()));
                } catch (NumberFormatException e) {
                    yield new BoolVal(false);
                }
            }
            case "number->string" -> {
                if (args.size() != 1) throw new EvalError("number->string requires exactly 1 argument");
                yield new StrVal(String.valueOf(asLong(args.getFirst())));
            }
            case "symbol->string" -> {
                if (args.size() != 1) throw new EvalError("symbol->string requires exactly 1 argument");
                if (!(args.getFirst() instanceof SymbolVal s)) throw new EvalError("symbol->string: not a symbol");
                yield new StrVal(s.name());
            }
            case "string->symbol" -> {
                if (args.size() != 1) throw new EvalError("string->symbol requires exactly 1 argument");
                if (!(args.getFirst() instanceof StrVal s)) throw new EvalError("string->symbol: not a string");
                yield new SymbolVal(s.value());
            }
            case "string-ref" -> {
                if (args.size() != 2) throw new EvalError("string-ref requires exactly 2 arguments");
                if (!(args.get(0) instanceof StrVal s)) throw new EvalError("string-ref: not a string");
                int idx = (int) asLong(args.get(1));
                yield new CharVal(s.charAt(idx));
            }
            case "char?" -> {
                if (args.size() != 1) throw new EvalError("char? requires exactly 1 argument");
                yield new BoolVal(args.getFirst() instanceof CharVal);
            }
            case "string-copy" -> {
                if (args.size() != 1) throw new EvalError("string-copy requires exactly 1 argument");
                if (!(args.getFirst() instanceof StrVal s)) throw new EvalError("string-copy: not a string");
                yield new StrVal(s.value());
            }
            case "string-set!" -> {
                if (args.size() != 3) throw new EvalError("string-set! requires exactly 3 arguments");
                if (!(args.get(0) instanceof StrVal s)) throw new EvalError("string-set!: not a string");
                int idx = (int) asLong(args.get(1));
                if (!(args.get(2) instanceof CharVal c)) throw new EvalError("string-set!: not a character");
                s.setChar(idx, c.value());
                yield VOID;
            }
            case "abs" -> {
                if (args.size() != 1) throw new EvalError("abs requires exactly 1 argument");
                yield new IntVal(Math.abs(asLong(args.getFirst())));
            }
            case "modulo" -> {
                if (args.size() != 2) throw new EvalError("modulo requires exactly 2 arguments");
                long a = asLong(args.get(0)), b = asLong(args.get(1));
                yield new IntVal(Math.floorMod(a, b));
            }
            case "remainder" -> {
                if (args.size() != 2) throw new EvalError("remainder requires exactly 2 arguments");
                yield new IntVal(asLong(args.get(0)) % asLong(args.get(1)));
            }
            case "quotient" -> {
                if (args.size() != 2) throw new EvalError("quotient requires exactly 2 arguments");
                long a = asLong(args.get(0)), b = asLong(args.get(1));
                if (b == 0) throw new EvalError("division by zero");
                long q = a / b;
                // truncate toward zero (Java's default for long division)
                yield new IntVal(q);
            }
            case "min" -> {
                if (args.isEmpty()) throw new EvalError("min requires at least 1 argument");
                long m = asLong(args.getFirst());
                for (int i = 1; i < args.size(); i++) m = Math.min(m, asLong(args.get(i)));
                yield new IntVal(m);
            }
            case "max" -> {
                if (args.isEmpty()) throw new EvalError("max requires at least 1 argument");
                long m = asLong(args.getFirst());
                for (int i = 1; i < args.size(); i++) m = Math.max(m, asLong(args.get(i)));
                yield new IntVal(m);
            }
            case "expt" -> {
                if (args.size() != 2) throw new EvalError("expt requires exactly 2 arguments");
                long base = asLong(args.get(0)), exp = asLong(args.get(1));
                long result = 1;
                for (long i = 0; i < exp; i++) result *= base;
                yield new IntVal(result);
            }
            case "zero?" -> {
                if (args.size() != 1) throw new EvalError("zero? requires exactly 1 argument");
                yield new BoolVal(asLong(args.getFirst()) == 0);
            }
            case "positive?" -> {
                if (args.size() != 1) throw new EvalError("positive? requires exactly 1 argument");
                yield new BoolVal(asLong(args.getFirst()) > 0);
            }
            case "negative?" -> {
                if (args.size() != 1) throw new EvalError("negative? requires exactly 1 argument");
                yield new BoolVal(asLong(args.getFirst()) < 0);
            }
            case "odd?" -> {
                if (args.size() != 1) throw new EvalError("odd? requires exactly 1 argument");
                yield new BoolVal(asLong(args.getFirst()) % 2 != 0);
            }
            case "even?" -> {
                if (args.size() != 1) throw new EvalError("even? requires exactly 1 argument");
                yield new BoolVal(asLong(args.getFirst()) % 2 == 0);
            }
            case "list-ref" -> {
                if (args.size() != 2) throw new EvalError("list-ref requires exactly 2 arguments");
                if (!(args.get(0) instanceof ListVal lst)) throw new EvalError("list-ref: not a list");
                int idx = (int) asLong(args.get(1));
                yield lst.elements().get(idx);
            }
            case "list-tail" -> {
                if (args.size() != 2) throw new EvalError("list-tail requires exactly 2 arguments");
                if (!(args.get(0) instanceof ListVal lst)) throw new EvalError("list-tail: not a list");
                int idx = (int) asLong(args.get(1));
                yield new ListVal(new ArrayList<>(lst.elements().subList(idx, lst.elements().size())));
            }
            case "list?" -> {
                if (args.size() != 1) throw new EvalError("list? requires exactly 1 argument");
                yield new BoolVal(args.getFirst() instanceof ListVal lst && !lst.improper);
            }
            case "assoc" -> {
                if (args.size() != 2) throw new EvalError("assoc requires exactly 2 arguments");
                SchemeVal key = args.get(0);
                if (!(args.get(1) instanceof ListVal alist)) throw new EvalError("assoc: not a list");
                for (SchemeVal entry : alist.elements()) {
                    if (entry instanceof ListVal pair && !pair.elements().isEmpty()) {
                        if (schemeEqual(key, pair.elements().getFirst())) {
                            yield pair;
                        }
                    }
                }
                yield new BoolVal(false);
            }
            case "map" -> {
                if (args.size() < 2) throw new EvalError("map requires at least 2 arguments");
                SchemeVal proc = args.get(0);
                List<ListVal> lists = new ArrayList<>();
                for (int i = 1; i < args.size(); i++) {
                    if (!(args.get(i) instanceof ListVal l)) throw new EvalError("map: not a list");
                    lists.add(l);
                }
                int len = lists.getFirst().elements().size();
                List<SchemeVal> result = new ArrayList<>();
                for (int i = 0; i < len; i++) {
                    List<SchemeVal> callArgs = new ArrayList<>();
                    for (ListVal l : lists) callArgs.add(l.elements().get(i));
                    result.add(applyProc(proc, callArgs));
                }
                yield new ListVal(result);
            }
            case "eq?" -> {
                if (args.size() != 2) throw new EvalError("eq? requires exactly 2 arguments");
                yield new BoolVal(schemeEq(args.get(0), args.get(1)));
            }
            case "equal?" -> {
                if (args.size() != 2) throw new EvalError("equal? requires exactly 2 arguments");
                yield new BoolVal(schemeEqual(args.get(0), args.get(1)));
            }
            case "char-alphabetic?" -> {
                if (args.size() != 1) throw new EvalError("char-alphabetic? requires exactly 1 argument");
                if (!(args.getFirst() instanceof CharVal c)) throw new EvalError("char-alphabetic?: not a character");
                yield new BoolVal(Character.isLetter(c.value()));
            }
            case "char-numeric?" -> {
                if (args.size() != 1) throw new EvalError("char-numeric? requires exactly 1 argument");
                if (!(args.getFirst() instanceof CharVal c)) throw new EvalError("char-numeric?: not a character");
                yield new BoolVal(Character.isDigit(c.value()));
            }
            case "char-upcase" -> {
                if (args.size() != 1) throw new EvalError("char-upcase requires exactly 1 argument");
                if (!(args.getFirst() instanceof CharVal c)) throw new EvalError("char-upcase: not a character");
                yield new CharVal(Character.toUpperCase(c.value()));
            }
            case "char-downcase" -> {
                if (args.size() != 1) throw new EvalError("char-downcase requires exactly 1 argument");
                if (!(args.getFirst() instanceof CharVal c)) throw new EvalError("char-downcase: not a character");
                yield new CharVal(Character.toLowerCase(c.value()));
            }
            case "char=?" -> {
                if (args.size() != 2) throw new EvalError("char=? requires exactly 2 arguments");
                if (!(args.get(0) instanceof CharVal a) || !(args.get(1) instanceof CharVal b))
                    throw new EvalError("char=?: not a character");
                yield new BoolVal(a.value() == b.value());
            }
            case "char<?" -> {
                if (args.size() != 2) throw new EvalError("char<? requires exactly 2 arguments");
                if (!(args.get(0) instanceof CharVal a) || !(args.get(1) instanceof CharVal b))
                    throw new EvalError("char<?: not a character");
                yield new BoolVal(a.value() < b.value());
            }
            case "string=?" -> {
                if (args.size() != 2) throw new EvalError("string=? requires exactly 2 arguments");
                if (!(args.get(0) instanceof StrVal a) || !(args.get(1) instanceof StrVal b))
                    throw new EvalError("string=?: not a string");
                yield new BoolVal(a.value().equals(b.value()));
            }
            case "string<?" -> {
                if (args.size() != 2) throw new EvalError("string<? requires exactly 2 arguments");
                if (!(args.get(0) instanceof StrVal a) || !(args.get(1) instanceof StrVal b))
                    throw new EvalError("string<?: not a string");
                yield new BoolVal(a.value().compareTo(b.value()) < 0);
            }
            case "string-ci=?" -> {
                if (args.size() != 2) throw new EvalError("string-ci=? requires exactly 2 arguments");
                if (!(args.get(0) instanceof StrVal a) || !(args.get(1) instanceof StrVal b))
                    throw new EvalError("string-ci=?: not a string");
                yield new BoolVal(a.value().equalsIgnoreCase(b.value()));
            }
            case "string-upcase" -> {
                if (args.size() != 1) throw new EvalError("string-upcase requires exactly 1 argument");
                if (!(args.getFirst() instanceof StrVal s)) throw new EvalError("string-upcase: not a string");
                yield new StrVal(s.value().toUpperCase());
            }
            case "string-downcase" -> {
                if (args.size() != 1) throw new EvalError("string-downcase requires exactly 1 argument");
                if (!(args.getFirst() instanceof StrVal s)) throw new EvalError("string-downcase: not a string");
                yield new StrVal(s.value().toLowerCase());
            }
            default -> throw new EvalError("unbound variable: " + name);
        };
    }

    private long asLong(SchemeVal val) throws EvalError {
        if (val instanceof IntVal iv) return iv.value();
        throw new EvalError("expected number, got: " + display(val));
    }

    private boolean schemeEqual(SchemeVal a, SchemeVal b) {
        if (a instanceof IntVal ai && b instanceof IntVal bi) return ai.value() == bi.value();
        if (a instanceof BoolVal ab && b instanceof BoolVal bb) return ab.value() == bb.value();
        if (a instanceof StrVal as && b instanceof StrVal bs) return as.value().equals(bs.value());
        if (a instanceof CharVal ac && b instanceof CharVal bc) return ac.value() == bc.value();
        if (a instanceof SymbolVal as && b instanceof SymbolVal bs) return as.name().equals(bs.name());
        if (a instanceof ListVal al && b instanceof ListVal bl) {
            if (al.elements().size() != bl.elements().size()) return false;
            if (al.improper != bl.improper) return false;
            for (int i = 0; i < al.elements().size(); i++) {
                if (!schemeEqual(al.elements().get(i), bl.elements().get(i))) return false;
            }
            return true;
        }
        return a == b;
    }

    private boolean schemeEq(SchemeVal a, SchemeVal b) {
        if (a instanceof SymbolVal as && b instanceof SymbolVal bs) return as.name().equals(bs.name());
        if (a instanceof IntVal ai && b instanceof IntVal bi) return ai.value() == bi.value();
        if (a instanceof BoolVal ab && b instanceof BoolVal bb) return ab.value() == bb.value();
        if (a instanceof CharVal ac && b instanceof CharVal bc) return ac.value() == bc.value();
        return a == b;
    }

    // ---- Display (write-style, with quotes) ----
    private String display(SchemeVal val) {
        return switch (val) {
            case IntVal v -> String.valueOf(v.value());
            case BoolVal v -> v.value() ? "#t" : "#f";
            case StrVal v -> "\"" + v.value() + "\"";
            case CharVal v -> {
                char c = v.value();
                if (c == ' ') yield "#\\space";
                if (c == '\n') yield "#\\newline";
                if (c == '\t') yield "#\\tab";
                yield "#\\" + c;
            }
            case SymbolVal v -> v.name();
            case VoidVal v -> "";
            case LambdaVal v -> "#<procedure>";
            case BuiltinVal v -> "#<procedure:" + v.name() + ">";
            case ContinuationVal v -> "#<continuation>";
            case MacroVal v -> "#<macro:" + v.name() + ">";
            case ListVal v -> {
                StringBuilder sb = new StringBuilder("(");
                int size = v.elements().size();
                for (int i = 0; i < size; i++) {
                    if (i > 0) sb.append(" ");
                    if (v.improper && i == size - 1 && size > 1) sb.append(". ");
                    sb.append(display(v.elements().get(i)));
                }
                sb.append(")");
                yield sb.toString();
            }
        };
    }

    // ---- Display (display-style, no quotes on strings) ----
    private String displayVal(SchemeVal val) {
        return switch (val) {
            case StrVal v -> v.value();
            case CharVal v -> String.valueOf(v.value());
            case ListVal v -> {
                StringBuilder sb = new StringBuilder("(");
                int size = v.elements().size();
                for (int i = 0; i < size; i++) {
                    if (i > 0) sb.append(" ");
                    if (v.improper && i == size - 1 && size > 1) sb.append(". ");
                    sb.append(displayVal(v.elements().get(i)));
                }
                sb.append(")");
                yield sb.toString();
            }
            default -> display(val);
        };
    }

    // ---- Public API ----
    private SchemeVal evalTopLevel(String input, Env env) throws EvalError {
        List<Token> tokens = tokenize(input);
        if (tokens.isEmpty()) throw new EvalError("empty input");

        posMap = new IdentityHashMap<>();
        currentPos = null;

        // Parse all top-level expressions first
        List<SchemeVal> exprs = new ArrayList<>();
        int[] pos = {0};
        while (pos[0] < tokens.size()) {
            exprs.add(parse(tokens, pos));
        }
        if (exprs.isEmpty()) throw new EvalError("empty input");

        // Evaluate with continuation re-entry support
        SchemeVal result = null;
        int idx = 0;
        while (idx < exprs.size()) {
            currentTopLevelIndex = idx;
            try {
                result = eval(exprs.get(idx), env);
                idx++;
            } catch (ContinuationReturn cr) {
                // Resume from the continuation's frame stack
                ContinuationReturn current = cr;
                boolean resolved = false;
                while (!resolved) {
                    if (current.frames.isEmpty()) {
                        // Top-level continuation: re-evaluate from topLevelIndex with pendingReturn
                        pendingReturn = current.value;
                        idx = current.topLevelIndex;
                        resolved = true;
                    } else {
                        try {
                            result = resumeFrames(current.frames, current.value);
                            idx = current.topLevelIndex + 1;
                            resolved = true;
                        } catch (ContinuationReturn cr2) {
                            current = cr2;
                        }
                    }
                }
            }
        }
        return result;
    }

    public String evalStr(String input) throws EvalError {
        Env env = makeGlobalEnv();
        SchemeVal result = evalTopLevel(input, env);
        if (result == null) throw new EvalError("empty input");
        return display(result);
    }

    public EvalResult evalStrWithOutput(String input) throws EvalError {
        outputBuffer = new StringBuilder();
        Env env = makeGlobalEnv();
        SchemeVal result = evalTopLevel(input, env);
        if (result == null) throw new EvalError("empty input");
        String output = outputBuffer.toString();
        outputBuffer = null;
        String resultStr = (result instanceof VoidVal) ? "" : display(result);
        return new EvalResult(resultStr, output);
    }
}
