package ming;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

public class Evaluator {

    // ---- Value types ----
    sealed interface SchemeVal permits IntVal, BoolVal, StrVal, ListVal, SymbolVal, LambdaVal, VoidVal, BuiltinVal {}
    record IntVal(long value) implements SchemeVal {}
    record BoolVal(boolean value) implements SchemeVal {}
    record StrVal(String value) implements SchemeVal {}
    record ListVal(List<SchemeVal> elements) implements SchemeVal {}
    record SymbolVal(String name) implements SchemeVal {}
    record VoidVal() implements SchemeVal {}
    record LambdaVal(List<String> params, List<SchemeVal> body, Env env) implements SchemeVal {}
    record BuiltinVal(String name) implements SchemeVal {}

    private static final SchemeVal VOID = new VoidVal();

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
    }

    private static Env makeGlobalEnv() {
        Env env = new Env(null);
        String[] builtins = {"+", "-", "*", "/", "<", ">", "=", "<=", ">=", "not",
            "cons", "car", "cdr", "null?", "list", "length", "append",
            "string?", "number?", "boolean?", "pair?", "symbol?", "procedure?", "integer?"};
        for (String b : builtins) {
            env.define(b, new BuiltinVal(b));
        }
        return env;
    }

    // ---- Tokenizer ----
    private static List<String> tokenize(String input) {
        List<String> tokens = new ArrayList<>();
        int i = 0;
        while (i < input.length()) {
            char c = input.charAt(i);
            if (Character.isWhitespace(c)) {
                i++;
            } else if (c == ';') {
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
    private static SchemeVal parse(List<String> tokens, int[] pos) throws EvalError {
        if (pos[0] >= tokens.size()) {
            throw new EvalError("unexpected end of input");
        }
        String token = tokens.get(pos[0]);
        if (token.equals("'")) {
            pos[0]++;
            SchemeVal quoted = parse(tokens, pos);
            List<SchemeVal> quoteExpr = new ArrayList<>();
            quoteExpr.add(new SymbolVal("quote"));
            quoteExpr.add(quoted);
            return new ListVal(quoteExpr);
        } else if (token.equals("(")) {
            pos[0]++;
            List<SchemeVal> elems = new ArrayList<>();
            while (pos[0] < tokens.size() && !tokens.get(pos[0]).equals(")")) {
                elems.add(parse(tokens, pos));
            }
            if (pos[0] >= tokens.size()) {
                throw new EvalError("missing closing parenthesis");
            }
            pos[0]++;
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

    // ---- Evaluator ----
    private SchemeVal eval(SchemeVal expr, Env env) throws EvalError {
        return switch (expr) {
            case IntVal v -> v;
            case BoolVal v -> v;
            case StrVal v -> v;
            case VoidVal v -> v;
            case LambdaVal v -> v;
            case BuiltinVal v -> v;
            case SymbolVal v -> env.get(v.name());
            case ListVal v -> {
                List<SchemeVal> elems = v.elements();
                if (elems.isEmpty()) throw new EvalError("empty application");

                SchemeVal head = elems.getFirst();

                // Special forms
                if (head instanceof SymbolVal sym) {
                    switch (sym.name()) {
                        case "quote" -> {
                            if (elems.size() != 2) throw new EvalError("quote requires 1 argument");
                            yield elems.get(1);
                        }
                        case "if" -> {
                            if (elems.size() < 3 || elems.size() > 4)
                                throw new EvalError("if requires 2 or 3 arguments");
                            SchemeVal cond = eval(elems.get(1), env);
                            if (!isFalse(cond)) {
                                yield eval(elems.get(2), env);
                            } else if (elems.size() == 4) {
                                yield eval(elems.get(3), env);
                            } else {
                                yield VOID;
                            }
                        }
                        case "define" -> {
                            if (elems.size() < 3) throw new EvalError("define requires at least 2 arguments");
                            SchemeVal target = elems.get(1);
                            if (target instanceof SymbolVal name) {
                                SchemeVal val = eval(elems.get(2), env);
                                env.define(name.name(), val);
                                yield VOID;
                            } else if (target instanceof ListVal nameAndParams) {
                                // (define (f x y) body...)
                                if (nameAndParams.elements().isEmpty())
                                    throw new EvalError("define: empty name list");
                                String fname = ((SymbolVal) nameAndParams.elements().getFirst()).name();
                                List<String> params = new ArrayList<>();
                                for (int i = 1; i < nameAndParams.elements().size(); i++) {
                                    params.add(((SymbolVal) nameAndParams.elements().get(i)).name());
                                }
                                List<SchemeVal> body = new ArrayList<>(elems.subList(2, elems.size()));
                                env.define(fname, new LambdaVal(params, body, env));
                                yield VOID;
                            } else {
                                throw new EvalError("define: invalid syntax");
                            }
                        }
                        case "lambda" -> {
                            if (elems.size() < 3) throw new EvalError("lambda requires params and body");
                            SchemeVal paramSpec = elems.get(1);
                            List<String> params = new ArrayList<>();
                            if (paramSpec instanceof ListVal pl) {
                                for (SchemeVal p : pl.elements()) {
                                    params.add(((SymbolVal) p).name());
                                }
                            } else {
                                throw new EvalError("lambda: invalid parameter list");
                            }
                            List<SchemeVal> body = new ArrayList<>(elems.subList(2, elems.size()));
                            yield new LambdaVal(params, body, env);
                        }
                        case "and" -> {
                            SchemeVal result = new BoolVal(true);
                            for (int i = 1; i < elems.size(); i++) {
                                result = eval(elems.get(i), env);
                                if (isFalse(result)) yield result;
                            }
                            yield result;
                        }
                        case "or" -> {
                            SchemeVal result = new BoolVal(false);
                            for (int i = 1; i < elems.size(); i++) {
                                result = eval(elems.get(i), env);
                                if (!isFalse(result)) yield result;
                            }
                            yield result;
                        }
                        case "let" -> {
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
                                LambdaVal loopFn = new LambdaVal(params, body, letEnv);
                                letEnv.define(loopName.name(), loopFn);
                                List<SchemeVal> evaledInits = new ArrayList<>();
                                for (SchemeVal init : inits) evaledInits.add(eval(init, env));
                                yield applyProc(loopFn, evaledInits);
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
                            SchemeVal letResult = VOID;
                            for (int i = 2; i < elems.size(); i++) {
                                letResult = eval(elems.get(i), letEnv);
                            }
                            yield letResult;
                        }
                        case "begin" -> {
                            SchemeVal beginResult = VOID;
                            for (int i = 1; i < elems.size(); i++) {
                                beginResult = eval(elems.get(i), env);
                            }
                            yield beginResult;
                        }
                        case "cond" -> {
                            SchemeVal condResult = VOID;
                            boolean matched = false;
                            for (int i = 1; i < elems.size(); i++) {
                                if (!(elems.get(i) instanceof ListVal clause) || clause.elements().isEmpty())
                                    throw new EvalError("cond: invalid clause");
                                SchemeVal test = clause.elements().get(0);
                                if (test instanceof SymbolVal s && s.name().equals("else")) {
                                    condResult = VOID;
                                    for (int j = 1; j < clause.elements().size(); j++) {
                                        condResult = eval(clause.elements().get(j), env);
                                    }
                                    matched = true;
                                    break;
                                }
                                SchemeVal testResult = eval(test, env);
                                if (!isFalse(testResult)) {
                                    condResult = testResult;
                                    for (int j = 1; j < clause.elements().size(); j++) {
                                        condResult = eval(clause.elements().get(j), env);
                                    }
                                    matched = true;
                                    break;
                                }
                            }
                            yield condResult;
                        }
                        default -> {
                            // fall through to procedure call
                        }
                    }
                }

                // Procedure call
                SchemeVal proc = eval(head, env);
                List<SchemeVal> args = new ArrayList<>();
                for (int i = 1; i < elems.size(); i++) {
                    args.add(eval(elems.get(i), env));
                }
                yield applyProc(proc, args);
            }
        };
    }

    private SchemeVal applyProc(SchemeVal proc, List<SchemeVal> args) throws EvalError {
        if (proc instanceof BuiltinVal b) {
            return applyBuiltin(b.name(), args);
        } else if (proc instanceof LambdaVal lambda) {
            if (args.size() != lambda.params().size()) {
                throw new EvalError("wrong number of arguments: expected " + lambda.params().size() + ", got " + args.size());
            }
            Env callEnv = new Env(lambda.env());
            for (int i = 0; i < lambda.params().size(); i++) {
                callEnv.define(lambda.params().get(i), args.get(i));
            }
            SchemeVal result = VOID;
            for (SchemeVal bodyExpr : lambda.body()) {
                result = eval(bodyExpr, callEnv);
            }
            return result;
        }
        throw new EvalError("not a procedure: " + display(proc));
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

    // ---- Public API ----
    public String evalStr(String input) throws EvalError {
        List<String> tokens = tokenize(input);
        if (tokens.isEmpty()) throw new EvalError("empty input");

        Env env = makeGlobalEnv();
        int[] pos = {0};
        SchemeVal result = null;
        SchemeVal lastNonVoid = null;
        while (pos[0] < tokens.size()) {
            result = eval(parse(tokens, pos), env);
            if (!(result instanceof VoidVal)) {
                lastNonVoid = result;
            }
        }
        if (result == null) throw new EvalError("empty input");
        // If the last expression was void but there was a non-void before, return that?
        // No — spec says return last result. But void define followed by expr returns expr.
        // Actually: multiple exprs, return last. If last is void, still return void display.
        // But "(define x 5) x" → "5" means we eval both, last is x=5.
        // The result var already tracks the last expression result correctly.
        if (lastNonVoid != null && result instanceof VoidVal) {
            // This shouldn't happen for well-formed programs — if define is last, return void
        }
        return display(result);
    }

    public EvalResult evalStrWithOutput(String input) throws EvalError {
        throw new EvalError("not implemented");
    }
}
