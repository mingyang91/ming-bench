package ming;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.IdentityHashMap;
import java.util.List;
import java.util.Map;

public class Evaluator {

    // ---- Value types ----
    sealed interface SchemeVal permits IntVal, BoolVal, StrVal, CharVal, ListVal, SymbolVal, LambdaVal, VoidVal, BuiltinVal {}
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
    record ListVal(List<SchemeVal> elements) implements SchemeVal {}
    record SymbolVal(String name) implements SchemeVal {}
    record VoidVal() implements SchemeVal {}
    record LambdaVal(List<String> params, String restParam, List<SchemeVal> body, Env env) implements SchemeVal {}
    record BuiltinVal(String name) implements SchemeVal {}

    private static final SchemeVal VOID = new VoidVal();

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
            "string-copy", "string-set!", "apply"};
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
                                for (int j = 0; j < body.size() - 1; j++) {
                                    eval(body.get(j), callEnv);
                                }
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
                            for (int i = 2; i < elems.size() - 1; i++) {
                                eval(elems.get(i), letEnv);
                            }
                            expr = elems.getLast();
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
                        default: {
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
                    for (int j = 0; j < lambda.body().size() - 1; j++) {
                        eval(lambda.body().get(j), callEnv);
                    }
                    expr = lambda.body().getLast();
                    env = callEnv;
                    continue trampoline;
                } else if (proc instanceof BuiltinVal b) {
                    if (b.name().equals("apply")) {
                        return doApply(args);
                    }
                    return applyBuiltin(b.name(), args);
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

    private SchemeVal applyProc(SchemeVal proc, List<SchemeVal> args) throws EvalError {
        if (proc instanceof BuiltinVal b) {
            if (b.name().equals("apply")) {
                return doApply(args);
            }
            return applyBuiltin(b.name(), args);
        } else if (proc instanceof LambdaVal lambda) {
            Env callEnv = bindLambdaArgs(lambda, args);
            SchemeVal result = VOID;
            for (SchemeVal bodyExpr : lambda.body()) {
                result = eval(bodyExpr, callEnv);
            }
            return result;
        }
        throw new EvalError("not a procedure: " + display(proc));
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
                    yield new ListVal(newElems);
                }
                // Improper pair - for now just store as 2-element list with dot notation later
                List<SchemeVal> pair = new ArrayList<>();
                pair.add(carVal);
                pair.add(cdrVal);
                yield new ListVal(pair); // simplified for L03
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
                    yield new ListVal(new ArrayList<>(lst.elements().subList(1, lst.elements().size())));
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
                        } else {
                            // last arg can be non-list for improper lists
                            result.add(args.get(i));
                        }
                    }
                }
                yield new ListVal(result);
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
                yield new BoolVal(args.getFirst() instanceof LambdaVal || args.getFirst() instanceof BuiltinVal);
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
            default -> throw new EvalError("unbound variable: " + name);
        };
    }

    private long asLong(SchemeVal val) throws EvalError {
        if (val instanceof IntVal iv) return iv.value();
        throw new EvalError("expected number, got: " + display(val));
    }

    // ---- Display (write-style, with quotes) ----
    private String display(SchemeVal val) {
        return switch (val) {
            case IntVal v -> String.valueOf(v.value());
            case BoolVal v -> v.value() ? "#t" : "#f";
            case StrVal v -> "\"" + v.value() + "\"";
            case CharVal v -> "#\\" + v.value();
            case SymbolVal v -> v.name();
            case VoidVal v -> "";
            case LambdaVal v -> "#<procedure>";
            case BuiltinVal v -> "#<procedure:" + v.name() + ">";
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

    // ---- Display (display-style, no quotes on strings) ----
    private String displayVal(SchemeVal val) {
        return switch (val) {
            case StrVal v -> v.value();
            case ListVal v -> {
                StringBuilder sb = new StringBuilder("(");
                for (int i = 0; i < v.elements().size(); i++) {
                    if (i > 0) sb.append(" ");
                    sb.append(displayVal(v.elements().get(i)));
                }
                sb.append(")");
                yield sb.toString();
            }
            default -> display(val);
        };
    }

    // ---- Public API ----
    public String evalStr(String input) throws EvalError {
        List<Token> tokens = tokenize(input);
        if (tokens.isEmpty()) throw new EvalError("empty input");

        posMap = new IdentityHashMap<>();
        currentPos = null;
        Env env = makeGlobalEnv();
        int[] pos = {0};
        SchemeVal result = null;
        while (pos[0] < tokens.size()) {
            result = eval(parse(tokens, pos), env);
        }
        if (result == null) throw new EvalError("empty input");
        return display(result);
    }

    public EvalResult evalStrWithOutput(String input) throws EvalError {
        List<Token> tokens = tokenize(input);
        if (tokens.isEmpty()) throw new EvalError("empty input");

        posMap = new IdentityHashMap<>();
        currentPos = null;
        outputBuffer = new StringBuilder();
        Env env = makeGlobalEnv();
        int[] pos = {0};
        SchemeVal result = null;
        while (pos[0] < tokens.size()) {
            result = eval(parse(tokens, pos), env);
        }
        if (result == null) throw new EvalError("empty input");
        String output = outputBuffer.toString();
        outputBuffer = null;
        String resultStr = (result instanceof VoidVal) ? "" : display(result);
        return new EvalResult(resultStr, output);
    }
}
