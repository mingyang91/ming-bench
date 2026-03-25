package ming;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

public class Evaluator {

    // ── Value types ──────────────────────────────────────────────
    private sealed interface Val permits Val.Int, Val.Bool, Val.Str, Val.Sym, Val.PairV, Val.Nil, Val.Void, Val.Builtin, Val.Lambda {
        record Int(long value) implements Val {}
        record Bool(boolean value) implements Val {}
        record Str(String value) implements Val {}
        record Sym(String name) implements Val {}
        record PairV(Val car, Val cdr) implements Val {}
        record Nil() implements Val {}
        record Void() implements Val {}
        record Builtin(String name, java.util.function.Function<List<Val>, Val> fn) implements Val {}
        record Lambda(List<String> params, List<Val> body, Env closure) implements Val {}
    }

    // ── Environment ─────────────────────────────────────────────
    private static class Env {
        final Map<String, Val> bindings = new HashMap<>();
        final Env parent;

        Env(Env parent) { this.parent = parent; }

        Val lookup(String name) throws EvalError {
            if (bindings.containsKey(name)) return bindings.get(name);
            if (parent != null) return parent.lookup(name);
            throw new EvalError("unbound variable: " + name);
        }

        void define(String name, Val val) {
            bindings.put(name, val);
        }
    }

    // ── Display ──────────────────────────────────────────────────
    private static String display(Val v) {
        return switch (v) {
            case Val.Int i -> String.valueOf(i.value());
            case Val.Bool b -> b.value() ? "#t" : "#f";
            case Val.Str s -> "\"" + s.value() + "\"";
            case Val.Sym s -> s.name();
            case Val.Nil ignored -> "()";
            case Val.Void ignored -> "#<void>";
            case Val.PairV p -> displayPair(p);
            case Val.Builtin b -> "#<procedure:" + b.name() + ">";
            case Val.Lambda ignored -> "#<procedure>";
        };
    }

    private static String displayPair(Val.PairV p) {
        StringBuilder sb = new StringBuilder("(");
        Val cur = p;
        boolean first = true;
        while (cur instanceof Val.PairV pair) {
            if (!first) sb.append(' ');
            first = false;
            sb.append(display(pair.car()));
            cur = pair.cdr();
        }
        if (!(cur instanceof Val.Nil)) {
            sb.append(" . ").append(display(cur));
        }
        sb.append(')');
        return sb.toString();
    }

    private static boolean isTruthy(Val v) {
        return !(v instanceof Val.Bool b && !b.value());
    }

    // ── Tokenizer ────────────────────────────────────────────────
    private static List<String> tokenize(String input) {
        List<String> tokens = new ArrayList<>();
        int i = 0;
        int len = input.length();
        while (i < len) {
            char c = input.charAt(i);
            if (Character.isWhitespace(c)) { i++; continue; }
            if (c == ';') { while (i < len && input.charAt(i) != '\n') i++; continue; }
            if (c == '(') { tokens.add("("); i++; continue; }
            if (c == ')') { tokens.add(")"); i++; continue; }
            if (c == '\'') { tokens.add("'"); i++; continue; }
            if (c == '"') {
                StringBuilder sb = new StringBuilder("\"");
                i++;
                while (i < len && input.charAt(i) != '"') {
                    if (input.charAt(i) == '\\') { sb.append(input.charAt(i++)); }
                    sb.append(input.charAt(i++));
                }
                sb.append('"');
                if (i < len) i++; // skip closing "
                tokens.add(sb.toString());
                continue;
            }
            // atom
            StringBuilder sb = new StringBuilder();
            while (i < len && !Character.isWhitespace(input.charAt(i))
                    && input.charAt(i) != '(' && input.charAt(i) != ')'
                    && input.charAt(i) != '"' && input.charAt(i) != ';') {
                sb.append(input.charAt(i++));
            }
            tokens.add(sb.toString());
        }
        return tokens;
    }

    // ── Parser ───────────────────────────────────────────────────
    private Val parse(List<String> tokens, int[] idx) throws EvalError {
        if (idx[0] >= tokens.size()) throw new EvalError("unexpected EOF");
        String tok = tokens.get(idx[0]++);
        if (tok.equals("(")) {
            List<Val> elems = new ArrayList<>();
            while (idx[0] < tokens.size() && !tokens.get(idx[0]).equals(")")) {
                elems.add(parse(tokens, idx));
            }
            if (idx[0] >= tokens.size()) throw new EvalError("missing )");
            idx[0]++; // skip )
            Val list = new Val.Nil();
            for (int i = elems.size() - 1; i >= 0; i--) {
                list = new Val.PairV(elems.get(i), list);
            }
            return list;
        }
        if (tok.equals(")")) throw new EvalError("unexpected )");
        if (tok.equals("'")) {
            Val quoted = parse(tokens, idx);
            return new Val.PairV(new Val.Sym("quote"), new Val.PairV(quoted, new Val.Nil()));
        }
        return parseAtom(tok);
    }

    private Val parseAtom(String tok) {
        if (tok.equals("#t")) return new Val.Bool(true);
        if (tok.equals("#f")) return new Val.Bool(false);
        if (tok.startsWith("\"")) return new Val.Str(tok.substring(1, tok.length() - 1));
        try {
            return new Val.Int(Long.parseLong(tok));
        } catch (NumberFormatException e) {
            return new Val.Sym(tok);
        }
    }

    // ── Eval ─────────────────────────────────────────────────────
    private Val eval(Val expr, Env env) throws EvalError {
        return switch (expr) {
            case Val.Int i -> i;
            case Val.Bool b -> b;
            case Val.Str s -> s;
            case Val.Nil n -> n;
            case Val.Void v -> v;
            case Val.Builtin b -> b;
            case Val.Lambda l -> l;
            case Val.Sym sym -> env.lookup(sym.name());
            case Val.PairV pair -> evalList(pair, env);
        };
    }

    private Val evalList(Val.PairV pair, Env env) throws EvalError {
        Val head = pair.car();

        // Special forms
        if (head instanceof Val.Sym sym) {
            switch (sym.name()) {
                case "quote" -> {
                    if (!(pair.cdr() instanceof Val.PairV q))
                        throw new EvalError("quote requires 1 argument");
                    return q.car();
                }
                case "if" -> {
                    return evalIf(pair.cdr(), env);
                }
                case "define" -> {
                    return evalDefine(pair.cdr(), env);
                }
                case "lambda" -> {
                    return evalLambda(pair.cdr(), env);
                }
                case "begin" -> { return evalBegin(pair.cdr(), env); }
                case "let" -> { return evalLet(pair.cdr(), env); }
                case "cond" -> { return evalCond(pair.cdr(), env); }
                case "and" -> { return evalAnd(pair.cdr(), env); }
                case "or" -> { return evalOr(pair.cdr(), env); }
            }
        }

        // Function call
        Val fn = eval(head, env);
        List<Val> args = evalArgs(pair.cdr(), env);
        return applyFn(fn, args);
    }

    private Val applyFn(Val fn, List<Val> args) throws EvalError {
        if (fn instanceof Val.Builtin builtin) {
            try {
                return builtin.fn().apply(args);
            } catch (RuntimeException e) {
                throw new EvalError(e.getMessage());
            }
        }
        if (fn instanceof Val.Lambda lambda) {
            if (args.size() != lambda.params().size())
                throw new EvalError("expected " + lambda.params().size() + " arguments, got " + args.size());
            Env callEnv = new Env(lambda.closure());
            for (int i = 0; i < lambda.params().size(); i++) {
                callEnv.define(lambda.params().get(i), args.get(i));
            }
            return evalBody(lambda.body(), callEnv);
        }
        throw new EvalError("not a procedure: " + display(fn));
    }

    private Val evalIf(Val args, Env env) throws EvalError {
        if (!(args instanceof Val.PairV p1)) throw new EvalError("if requires a condition");
        Val cond = eval(p1.car(), env);
        Val rest = p1.cdr();
        if (!(rest instanceof Val.PairV p2)) throw new EvalError("if requires a consequent");
        if (isTruthy(cond)) {
            return eval(p2.car(), env);
        }
        Val elseRest = p2.cdr();
        if (elseRest instanceof Val.PairV p3) {
            return eval(p3.car(), env);
        }
        return new Val.Void();
    }

    private Val evalDefine(Val args, Env env) throws EvalError {
        if (!(args instanceof Val.PairV p)) throw new EvalError("define requires arguments");
        Val target = p.car();
        if (target instanceof Val.Sym sym) {
            // (define x expr)
            if (!(p.cdr() instanceof Val.PairV valPair)) throw new EvalError("define requires a value");
            Val val = eval(valPair.car(), env);
            env.define(sym.name(), val);
            return new Val.Void();
        }
        if (target instanceof Val.PairV namePair) {
            // (define (f params...) body)
            if (!(namePair.car() instanceof Val.Sym fnName))
                throw new EvalError("define: expected function name");
            List<String> params = new ArrayList<>();
            Val paramList = namePair.cdr();
            while (paramList instanceof Val.PairV pp) {
                if (!(pp.car() instanceof Val.Sym paramSym))
                    throw new EvalError("define: expected parameter name");
                params.add(paramSym.name());
                paramList = pp.cdr();
            }
            List<Val> body = collectList(p.cdr());
            if (body.isEmpty()) throw new EvalError("define: missing body");
            Val.Lambda lambda = new Val.Lambda(params, body, env);
            env.define(fnName.name(), lambda);
            return new Val.Void();
        }
        throw new EvalError("define: invalid syntax");
    }

    private Val evalLambda(Val args, Env env) throws EvalError {
        if (!(args instanceof Val.PairV p)) throw new EvalError("lambda requires arguments");
        List<String> params = new ArrayList<>();
        Val paramList = p.car();
        while (paramList instanceof Val.PairV pp) {
            if (!(pp.car() instanceof Val.Sym paramSym))
                throw new EvalError("lambda: expected parameter name");
            params.add(paramSym.name());
            paramList = pp.cdr();
        }
        List<Val> body = collectList(p.cdr());
        if (body.isEmpty()) throw new EvalError("lambda: missing body");
        return new Val.Lambda(params, body, env);
    }

    private Val evalAnd(Val args, Env env) throws EvalError {
        Val result = new Val.Bool(true);
        Val cur = args;
        while (cur instanceof Val.PairV p) {
            result = eval(p.car(), env);
            if (!isTruthy(result)) return result;
            cur = p.cdr();
        }
        return result;
    }

    private Val evalOr(Val args, Env env) throws EvalError {
        Val result = new Val.Bool(false);
        Val cur = args;
        while (cur instanceof Val.PairV p) {
            result = eval(p.car(), env);
            if (isTruthy(result)) return result;
            cur = p.cdr();
        }
        return result;
    }

    private Val evalBody(List<Val> body, Env env) throws EvalError {
        Val result = new Val.Void();
        for (Val expr : body) {
            result = eval(expr, env);
        }
        return result;
    }

    private List<Val> collectList(Val v) {
        List<Val> result = new ArrayList<>();
        Val cur = v;
        while (cur instanceof Val.PairV p) {
            result.add(p.car());
            cur = p.cdr();
        }
        return result;
    }

    private Val evalBegin(Val args, Env env) throws EvalError {
        Val result = new Val.Void();
        Val cur = args;
        while (cur instanceof Val.PairV p) {
            result = eval(p.car(), env);
            cur = p.cdr();
        }
        return result;
    }

    private Val evalLet(Val args, Env env) throws EvalError {
        if (!(args instanceof Val.PairV p)) throw new EvalError("let: invalid syntax");
        // Named let: (let name ((var init) ...) body ...)
        if (p.car() instanceof Val.Sym nameSym) {
            if (!(p.cdr() instanceof Val.PairV rest)) throw new EvalError("let: invalid syntax");
            List<String> params = new ArrayList<>();
            List<Val> inits = new ArrayList<>();
            Val bindings = rest.car();
            while (bindings instanceof Val.PairV bp) {
                if (!(bp.car() instanceof Val.PairV binding)) throw new EvalError("let: invalid binding");
                if (!(binding.car() instanceof Val.Sym varSym)) throw new EvalError("let: expected variable name");
                params.add(varSym.name());
                if (!(binding.cdr() instanceof Val.PairV valPair)) throw new EvalError("let: missing init");
                inits.add(eval(valPair.car(), env));
                bindings = bp.cdr();
            }
            List<Val> body = collectList(rest.cdr());
            if (body.isEmpty()) throw new EvalError("let: missing body");
            // Create lambda and bind it in its own closure
            Env letEnv = new Env(env);
            Val.Lambda lambda = new Val.Lambda(params, body, letEnv);
            letEnv.define(nameSym.name(), lambda);
            return applyFn(lambda, inits);
        }
        // Regular let: (let ((var init) ...) body ...)
        Env letEnv = new Env(env);
        Val bindings = p.car();
        while (bindings instanceof Val.PairV bp) {
            if (!(bp.car() instanceof Val.PairV binding)) throw new EvalError("let: invalid binding");
            if (!(binding.car() instanceof Val.Sym varSym)) throw new EvalError("let: expected variable name");
            if (!(binding.cdr() instanceof Val.PairV valPair)) throw new EvalError("let: missing init");
            Val val = eval(valPair.car(), env);
            letEnv.define(varSym.name(), val);
            bindings = bp.cdr();
        }
        List<Val> body = collectList(p.cdr());
        if (body.isEmpty()) throw new EvalError("let: missing body");
        return evalBody(body, letEnv);
    }

    private Val evalCond(Val args, Env env) throws EvalError {
        Val cur = args;
        while (cur instanceof Val.PairV p) {
            Val clause = p.car();
            if (!(clause instanceof Val.PairV cp)) throw new EvalError("cond: invalid clause");
            // Check for else clause
            if (cp.car() instanceof Val.Sym s && s.name().equals("else")) {
                return evalBegin(cp.cdr(), env);
            }
            Val test = eval(cp.car(), env);
            if (isTruthy(test)) {
                if (cp.cdr() instanceof Val.Nil) return test;
                return evalBegin(cp.cdr(), env);
            }
            cur = p.cdr();
        }
        return new Val.Void();
    }

    private List<Val> evalArgs(Val args, Env env) throws EvalError {
        List<Val> result = new ArrayList<>();
        Val cur = args;
        while (cur instanceof Val.PairV p) {
            result.add(eval(p.car(), env));
            cur = p.cdr();
        }
        return result;
    }

    // ── Builtins ────────────────────────────────────────────────
    private static long asInt(Val v) {
        if (v instanceof Val.Int i) return i.value();
        throw new RuntimeException("expected integer, got: " + display(v));
    }

    private static void checkArgCount(List<Val> args, int expected, String name) {
        if (args.size() != expected)
            throw new RuntimeException(name + " requires " + expected + " arguments, got " + args.size());
    }

    private Env createGlobalEnv() {
        Env env = new Env(null);
        env.define("+", new Val.Builtin("+", args -> {
            long sum = 0;
            for (Val a : args) sum += asInt(a);
            return new Val.Int(sum);
        }));
        env.define("-", new Val.Builtin("-", args -> {
            if (args.isEmpty()) throw new RuntimeException("- requires at least 1 argument");
            if (args.size() == 1) return new Val.Int(-asInt(args.get(0)));
            long result = asInt(args.get(0));
            for (int i = 1; i < args.size(); i++) result -= asInt(args.get(i));
            return new Val.Int(result);
        }));
        env.define("*", new Val.Builtin("*", args -> {
            long product = 1;
            for (Val a : args) product *= asInt(a);
            return new Val.Int(product);
        }));
        env.define("/", new Val.Builtin("/", args -> {
            if (args.size() < 2) throw new RuntimeException("/ requires at least 2 arguments");
            long result = asInt(args.get(0));
            for (int i = 1; i < args.size(); i++) {
                long divisor = asInt(args.get(i));
                if (divisor == 0) throw new RuntimeException("division by zero");
                result /= divisor;
            }
            return new Val.Int(result);
        }));
        env.define("<", new Val.Builtin("<", args -> {
            checkArgCount(args, 2, "<");
            return new Val.Bool(asInt(args.get(0)) < asInt(args.get(1)));
        }));
        env.define(">", new Val.Builtin(">", args -> {
            checkArgCount(args, 2, ">");
            return new Val.Bool(asInt(args.get(0)) > asInt(args.get(1)));
        }));
        env.define("=", new Val.Builtin("=", args -> {
            checkArgCount(args, 2, "=");
            return new Val.Bool(asInt(args.get(0)) == asInt(args.get(1)));
        }));
        env.define("<=", new Val.Builtin("<=", args -> {
            checkArgCount(args, 2, "<=");
            return new Val.Bool(asInt(args.get(0)) <= asInt(args.get(1)));
        }));
        env.define(">=", new Val.Builtin(">=", args -> {
            checkArgCount(args, 2, ">=");
            return new Val.Bool(asInt(args.get(0)) >= asInt(args.get(1)));
        }));
        env.define("not", new Val.Builtin("not", args -> {
            checkArgCount(args, 1, "not");
            return new Val.Bool(!isTruthy(args.get(0)));
        }));
        env.define("cons", new Val.Builtin("cons", args -> {
            checkArgCount(args, 2, "cons");
            return new Val.PairV(args.get(0), args.get(1));
        }));
        env.define("car", new Val.Builtin("car", args -> {
            checkArgCount(args, 1, "car");
            if (!(args.get(0) instanceof Val.PairV p)) throw new RuntimeException("car: not a pair");
            return p.car();
        }));
        env.define("cdr", new Val.Builtin("cdr", args -> {
            checkArgCount(args, 1, "cdr");
            if (!(args.get(0) instanceof Val.PairV p)) throw new RuntimeException("cdr: not a pair");
            return p.cdr();
        }));
        env.define("null?", new Val.Builtin("null?", args -> {
            checkArgCount(args, 1, "null?");
            return new Val.Bool(args.get(0) instanceof Val.Nil);
        }));
        env.define("list", new Val.Builtin("list", args -> {
            Val result = new Val.Nil();
            for (int i = args.size() - 1; i >= 0; i--) {
                result = new Val.PairV(args.get(i), result);
            }
            return result;
        }));
        env.define("length", new Val.Builtin("length", args -> {
            checkArgCount(args, 1, "length");
            long count = 0;
            Val cur = args.get(0);
            while (cur instanceof Val.PairV p) { count++; cur = p.cdr(); }
            return new Val.Int(count);
        }));
        env.define("append", new Val.Builtin("append", args -> {
            if (args.isEmpty()) return new Val.Nil();
            Val result = args.get(args.size() - 1);
            for (int i = args.size() - 2; i >= 0; i--) {
                Val lst = args.get(i);
                // Collect elements then prepend in reverse
                List<Val> elems = new ArrayList<>();
                Val cur = lst;
                while (cur instanceof Val.PairV p) { elems.add(p.car()); cur = p.cdr(); }
                for (int j = elems.size() - 1; j >= 0; j--) {
                    result = new Val.PairV(elems.get(j), result);
                }
            }
            return result;
        }));
        env.define("number?", new Val.Builtin("number?", args -> {
            checkArgCount(args, 1, "number?");
            return new Val.Bool(args.get(0) instanceof Val.Int);
        }));
        env.define("string?", new Val.Builtin("string?", args -> {
            checkArgCount(args, 1, "string?");
            return new Val.Bool(args.get(0) instanceof Val.Str);
        }));
        env.define("boolean?", new Val.Builtin("boolean?", args -> {
            checkArgCount(args, 1, "boolean?");
            return new Val.Bool(args.get(0) instanceof Val.Bool);
        }));
        env.define("pair?", new Val.Builtin("pair?", args -> {
            checkArgCount(args, 1, "pair?");
            return new Val.Bool(args.get(0) instanceof Val.PairV);
        }));
        env.define("symbol?", new Val.Builtin("symbol?", args -> {
            checkArgCount(args, 1, "symbol?");
            return new Val.Bool(args.get(0) instanceof Val.Sym);
        }));
        return env;
    }

    // ── Public API ───────────────────────────────────────────────
    public String evalStr(String input) throws EvalError {
        List<String> tokens = tokenize(input);
        if (tokens.isEmpty()) throw new EvalError("empty input");
        int[] idx = {0};
        Env env = createGlobalEnv();
        Val result = null;
        while (idx[0] < tokens.size()) {
            result = parse(tokens, idx);
            result = eval(result, env);
        }
        if (result instanceof Val.Void) return "#<void>";
        return display(result);
    }

    public EvalResult evalStrWithOutput(String input) throws EvalError {
        String result = evalStr(input);
        return new EvalResult(result, "");
    }
}
