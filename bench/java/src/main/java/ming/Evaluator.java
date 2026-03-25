package ming;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.IdentityHashMap;
import java.util.List;
import java.util.Map;

public class Evaluator {

    // ── Value types ──────────────────────────────────────────────
    private sealed interface Val permits Val.Int, Val.Bool, Val.Str, Val.Sym, Val.Chr, Val.PairV, Val.Nil, Val.Void, Val.Builtin, Val.Lambda {
        record Int(long value) implements Val {}
        record Bool(boolean value) implements Val {}
        final class Str implements Val {
            private final char[] chars;
            Str(String value) { this.chars = value.toCharArray(); }
            String value() { return new String(chars); }
            void setChar(int idx, char c) { chars[idx] = c; }
            int length() { return chars.length; }
        }
        record Sym(String name) implements Val {}
        record Chr(char value) implements Val {}
        record PairV(Val car, Val cdr) implements Val {}
        record Nil() implements Val {}
        record Void() implements Val {}
        record Builtin(String name, java.util.function.Function<List<Val>, Val> fn) implements Val {}
        record Lambda(List<String> params, List<Val> body, Env closure) implements Val {}
    }

    // ── Token with position ─────────────────────────────────────
    private record Token(String text, int line, int col) {}

    // ── Position tracking ───────────────────────────────────────
    private final IdentityHashMap<Val, int[]> positions = new IdentityHashMap<>();

    private void setPos(Val v, int line, int col) {
        positions.put(v, new int[]{line, col});
    }

    private void copyPos(Val from, Val to) {
        int[] pos = positions.get(from);
        if (pos != null) positions.put(to, pos);
    }

    private String posPrefix(Val v) {
        int[] pos = positions.get(v);
        if (pos != null) return pos[0] + ":" + pos[1] + ": ";
        return "";
    }

    private EvalError posError(Val v, String msg) {
        return new EvalError(posPrefix(v) + msg);
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

        void set(String name, Val val) throws EvalError {
            if (bindings.containsKey(name)) { bindings.put(name, val); return; }
            if (parent != null) { parent.set(name, val); return; }
            throw new EvalError("unbound variable: " + name);
        }
    }

    // ── Output capture ──────────────────────────────────────────
    private StringBuilder output = new StringBuilder();

    // ── Write representation (with quotes) ──────────────────────
    private static String writeVal(Val v) {
        return switch (v) {
            case Val.Int i -> String.valueOf(i.value());
            case Val.Bool b -> b.value() ? "#t" : "#f";
            case Val.Str s -> "\"" + s.value() + "\"";
            case Val.Sym s -> s.name();
            case Val.Chr c -> "#\\" + c.value();
            case Val.Nil ignored -> "()";
            case Val.Void ignored -> "#<void>";
            case Val.PairV p -> writePair(p);
            case Val.Builtin b -> "#<procedure:" + b.name() + ">";
            case Val.Lambda ignored -> "#<procedure>";
        };
    }

    private static String writePair(Val.PairV p) {
        StringBuilder sb = new StringBuilder("(");
        Val cur = p;
        boolean first = true;
        while (cur instanceof Val.PairV pair) {
            if (!first) sb.append(' ');
            first = false;
            sb.append(writeVal(pair.car()));
            cur = pair.cdr();
        }
        if (!(cur instanceof Val.Nil)) {
            sb.append(" . ").append(writeVal(cur));
        }
        sb.append(')');
        return sb.toString();
    }

    // ── Display representation (no quotes on strings) ───────────
    private static String displayVal(Val v) {
        return switch (v) {
            case Val.Str s -> s.value();
            case Val.Chr c -> String.valueOf(c.value());
            case Val.PairV p -> displayPairVal(p);
            default -> writeVal(v);
        };
    }

    private static String displayPairVal(Val.PairV p) {
        StringBuilder sb = new StringBuilder("(");
        Val cur = p;
        boolean first = true;
        while (cur instanceof Val.PairV pair) {
            if (!first) sb.append(' ');
            first = false;
            sb.append(displayVal(pair.car()));
            cur = pair.cdr();
        }
        if (!(cur instanceof Val.Nil)) {
            sb.append(" . ").append(displayVal(cur));
        }
        sb.append(')');
        return sb.toString();
    }

    private static boolean isTruthy(Val v) {
        return !(v instanceof Val.Bool b && !b.value());
    }

    // ── Tokenizer ────────────────────────────────────────────────
    private static List<Token> tokenize(String input) {
        List<Token> tokens = new ArrayList<>();
        int i = 0;
        int len = input.length();
        int line = 1;
        int col = 1;
        while (i < len) {
            char c = input.charAt(i);
            if (c == '\n') { i++; line++; col = 1; continue; }
            if (Character.isWhitespace(c)) { i++; col++; continue; }
            if (c == ';') {
                while (i < len && input.charAt(i) != '\n') { i++; col++; }
                continue;
            }
            if (c == '(') { tokens.add(new Token("(", line, col)); i++; col++; continue; }
            if (c == ')') { tokens.add(new Token(")", line, col)); i++; col++; continue; }
            if (c == '\'') { tokens.add(new Token("'", line, col)); i++; col++; continue; }
            if (c == '"') {
                int startCol = col;
                StringBuilder sb = new StringBuilder("\"");
                i++; col++;
                while (i < len && input.charAt(i) != '"') {
                    if (input.charAt(i) == '\\') { sb.append(input.charAt(i)); i++; col++; }
                    sb.append(input.charAt(i)); i++; col++;
                }
                sb.append('"');
                if (i < len) { i++; col++; } // skip closing "
                tokens.add(new Token(sb.toString(), line, startCol));
                continue;
            }
            // atom
            int startCol = col;
            StringBuilder sb = new StringBuilder();
            while (i < len && !Character.isWhitespace(input.charAt(i))
                    && input.charAt(i) != '(' && input.charAt(i) != ')'
                    && input.charAt(i) != '"' && input.charAt(i) != ';') {
                sb.append(input.charAt(i)); i++; col++;
            }
            tokens.add(new Token(sb.toString(), line, startCol));
        }
        return tokens;
    }

    // ── Parser ───────────────────────────────────────────────────
    private Val parse(List<Token> tokens, int[] idx) throws EvalError {
        if (idx[0] >= tokens.size()) throw new EvalError("unexpected EOF");
        Token tok = tokens.get(idx[0]++);
        if (tok.text().equals("(")) {
            List<Val> elems = new ArrayList<>();
            while (idx[0] < tokens.size() && !tokens.get(idx[0]).text().equals(")")) {
                elems.add(parse(tokens, idx));
            }
            if (idx[0] >= tokens.size()) throw new EvalError(tok.line() + ":" + tok.col() + ": missing )");
            idx[0]++; // skip )
            Val list = new Val.Nil();
            for (int i = elems.size() - 1; i >= 0; i--) {
                Val pair = new Val.PairV(elems.get(i), list);
                copyPos(elems.get(i), pair);
                list = pair;
            }
            // Set position of the whole list to the opening paren
            if (list instanceof Val.PairV) {
                setPos(list, tok.line(), tok.col());
            } else {
                // empty list ()
                setPos(list, tok.line(), tok.col());
            }
            return list;
        }
        if (tok.text().equals(")")) throw new EvalError(tok.line() + ":" + tok.col() + ": unexpected )");
        if (tok.text().equals("'")) {
            Val quoted = parse(tokens, idx);
            Val inner = new Val.PairV(quoted, new Val.Nil());
            Val quoteSym = new Val.Sym("quote");
            setPos(quoteSym, tok.line(), tok.col());
            Val result = new Val.PairV(quoteSym, inner);
            setPos(result, tok.line(), tok.col());
            return result;
        }
        Val atom = parseAtom(tok.text());
        setPos(atom, tok.line(), tok.col());
        return atom;
    }

    private Val parseAtom(String tok) {
        if (tok.equals("#t")) return new Val.Bool(true);
        if (tok.equals("#f")) return new Val.Bool(false);
        if (tok.startsWith("#\\")) {
            String rest = tok.substring(2);
            if (rest.equals("space")) return new Val.Chr(' ');
            if (rest.equals("newline")) return new Val.Chr('\n');
            if (rest.equals("tab")) return new Val.Chr('\t');
            if (rest.length() == 1) return new Val.Chr(rest.charAt(0));
            throw new RuntimeException("unknown character literal: " + tok);
        }
        if (tok.startsWith("\"")) {
            String raw = tok.substring(1, tok.length() - 1);
            StringBuilder sb = new StringBuilder();
            for (int i = 0; i < raw.length(); i++) {
                if (raw.charAt(i) == '\\' && i + 1 < raw.length()) {
                    char next = raw.charAt(++i);
                    switch (next) {
                        case 'n' -> sb.append('\n');
                        case 't' -> sb.append('\t');
                        case '\\' -> sb.append('\\');
                        case '"' -> sb.append('"');
                        default -> { sb.append('\\'); sb.append(next); }
                    }
                } else {
                    sb.append(raw.charAt(i));
                }
            }
            return new Val.Str(sb.toString());
        }
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
            case Val.Chr c -> c;
            case Val.Builtin b -> b;
            case Val.Lambda l -> l;
            case Val.Sym sym -> {
                try {
                    yield env.lookup(sym.name());
                } catch (EvalError e) {
                    throw posError(expr, e.getMessage());
                }
            }
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
                        throw posError(pair, "quote requires 1 argument");
                    return q.car();
                }
                case "if" -> {
                    return evalIf(pair.cdr(), env, pair);
                }
                case "define" -> {
                    return evalDefine(pair.cdr(), env, pair);
                }
                case "lambda" -> {
                    return evalLambda(pair.cdr(), env, pair);
                }
                case "set!" -> {
                    Val setCdr = pair.cdr();
                    if (!(setCdr instanceof Val.PairV sp)) throw posError(pair, "set! requires 2 arguments");
                    if (!(sp.car() instanceof Val.Sym setSym)) throw posError(pair, "set!: expected variable name");
                    if (!(sp.cdr() instanceof Val.PairV svp)) throw posError(pair, "set! requires a value");
                    Val setVal = eval(svp.car(), env);
                    env.set(setSym.name(), setVal);
                    return new Val.Void();
                }
                case "begin" -> { return evalBegin(pair.cdr(), env); }
                case "let" -> { return evalLet(pair.cdr(), env, pair); }
                case "cond" -> { return evalCond(pair.cdr(), env); }
                case "and" -> { return evalAnd(pair.cdr(), env); }
                case "or" -> { return evalOr(pair.cdr(), env); }
            }
        }

        // Function call
        Val fn = eval(head, env);
        List<Val> args = evalArgs(pair.cdr(), env);
        return applyFn(fn, args, pair);
    }

    private Val applyFn(Val fn, List<Val> args, Val callSite) throws EvalError {
        if (fn instanceof Val.Builtin builtin) {
            try {
                return builtin.fn().apply(args);
            } catch (RuntimeException e) {
                throw posError(callSite, e.getMessage());
            }
        }
        if (fn instanceof Val.Lambda lambda) {
            if (args.size() != lambda.params().size())
                throw posError(callSite, "expected " + lambda.params().size() + " arguments, got " + args.size());
            Env callEnv = new Env(lambda.closure());
            for (int i = 0; i < lambda.params().size(); i++) {
                callEnv.define(lambda.params().get(i), args.get(i));
            }
            return evalBody(lambda.body(), callEnv);
        }
        throw posError(callSite, "not a procedure: " + writeVal(fn));
    }

    private Val evalIf(Val args, Env env, Val form) throws EvalError {
        if (!(args instanceof Val.PairV p1)) throw posError(form, "if requires a condition");
        Val cond = eval(p1.car(), env);
        Val rest = p1.cdr();
        if (!(rest instanceof Val.PairV p2)) throw posError(form, "if requires a consequent");
        if (isTruthy(cond)) {
            return eval(p2.car(), env);
        }
        Val elseRest = p2.cdr();
        if (elseRest instanceof Val.PairV p3) {
            return eval(p3.car(), env);
        }
        return new Val.Void();
    }

    private Val evalDefine(Val args, Env env, Val form) throws EvalError {
        if (!(args instanceof Val.PairV p)) throw posError(form, "define requires arguments");
        Val target = p.car();
        if (target instanceof Val.Sym sym) {
            // (define x expr)
            if (!(p.cdr() instanceof Val.PairV valPair)) throw posError(form, "define requires a value");
            Val val = eval(valPair.car(), env);
            env.define(sym.name(), val);
            return new Val.Void();
        }
        if (target instanceof Val.PairV namePair) {
            // (define (f params...) body)
            if (!(namePair.car() instanceof Val.Sym fnName))
                throw posError(form, "define: expected function name");
            List<String> params = new ArrayList<>();
            Val paramList = namePair.cdr();
            while (paramList instanceof Val.PairV pp) {
                if (!(pp.car() instanceof Val.Sym paramSym))
                    throw posError(form, "define: expected parameter name");
                params.add(paramSym.name());
                paramList = pp.cdr();
            }
            List<Val> body = collectList(p.cdr());
            if (body.isEmpty()) throw posError(form, "define: missing body");
            Val.Lambda lambda = new Val.Lambda(params, body, env);
            env.define(fnName.name(), lambda);
            return new Val.Void();
        }
        throw posError(form, "define: invalid syntax");
    }

    private Val evalLambda(Val args, Env env, Val form) throws EvalError {
        if (!(args instanceof Val.PairV p)) throw posError(form, "lambda requires arguments");
        List<String> params = new ArrayList<>();
        Val paramList = p.car();
        while (paramList instanceof Val.PairV pp) {
            if (!(pp.car() instanceof Val.Sym paramSym))
                throw posError(form, "lambda: expected parameter name");
            params.add(paramSym.name());
            paramList = pp.cdr();
        }
        List<Val> body = collectList(p.cdr());
        if (body.isEmpty()) throw posError(form, "lambda: missing body");
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

    private Val evalLet(Val args, Env env, Val form) throws EvalError {
        if (!(args instanceof Val.PairV p)) throw posError(form, "let: invalid syntax");
        // Named let: (let name ((var init) ...) body ...)
        if (p.car() instanceof Val.Sym nameSym) {
            if (!(p.cdr() instanceof Val.PairV rest)) throw posError(form, "let: invalid syntax");
            List<String> params = new ArrayList<>();
            List<Val> inits = new ArrayList<>();
            Val bindings = rest.car();
            while (bindings instanceof Val.PairV bp) {
                if (!(bp.car() instanceof Val.PairV binding)) throw posError(form, "let: invalid binding");
                if (!(binding.car() instanceof Val.Sym varSym)) throw posError(form, "let: expected variable name");
                params.add(varSym.name());
                if (!(binding.cdr() instanceof Val.PairV valPair)) throw posError(form, "let: missing init");
                inits.add(eval(valPair.car(), env));
                bindings = bp.cdr();
            }
            List<Val> body = collectList(rest.cdr());
            if (body.isEmpty()) throw posError(form, "let: missing body");
            Env letEnv = new Env(env);
            Val.Lambda lambda = new Val.Lambda(params, body, letEnv);
            letEnv.define(nameSym.name(), lambda);
            return applyFn(lambda, inits, form);
        }
        // Regular let: (let ((var init) ...) body ...)
        Env letEnv = new Env(env);
        Val bindings = p.car();
        while (bindings instanceof Val.PairV bp) {
            if (!(bp.car() instanceof Val.PairV binding)) throw posError(form, "let: invalid binding");
            if (!(binding.car() instanceof Val.Sym varSym)) throw posError(form, "let: expected variable name");
            if (!(binding.cdr() instanceof Val.PairV valPair)) throw posError(form, "let: missing init");
            Val val = eval(valPair.car(), env);
            letEnv.define(varSym.name(), val);
            bindings = bp.cdr();
        }
        List<Val> body = collectList(p.cdr());
        if (body.isEmpty()) throw posError(form, "let: missing body");
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
        throw new RuntimeException("expected integer, got: " + writeVal(v));
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
        // L05 — display, write, newline
        env.define("display", new Val.Builtin("display", args -> {
            checkArgCount(args, 1, "display");
            output.append(displayVal(args.get(0)));
            return new Val.Void();
        }));
        env.define("write", new Val.Builtin("write", args -> {
            checkArgCount(args, 1, "write");
            output.append(writeVal(args.get(0)));
            return new Val.Void();
        }));
        env.define("newline", new Val.Builtin("newline", args -> {
            checkArgCount(args, 0, "newline");
            output.append("\n");
            return new Val.Void();
        }));
        // L05 — string operations
        env.define("string-append", new Val.Builtin("string-append", args -> {
            StringBuilder sb = new StringBuilder();
            for (Val a : args) {
                if (!(a instanceof Val.Str s)) throw new RuntimeException("string-append: not a string: " + writeVal(a));
                sb.append(s.value());
            }
            return new Val.Str(sb.toString());
        }));
        env.define("string-length", new Val.Builtin("string-length", args -> {
            checkArgCount(args, 1, "string-length");
            if (!(args.get(0) instanceof Val.Str s)) throw new RuntimeException("string-length: not a string");
            return new Val.Int(s.value().length());
        }));
        env.define("substring", new Val.Builtin("substring", args -> {
            checkArgCount(args, 3, "substring");
            if (!(args.get(0) instanceof Val.Str s)) throw new RuntimeException("substring: not a string");
            int start = (int) asInt(args.get(1));
            int end = (int) asInt(args.get(2));
            return new Val.Str(s.value().substring(start, end));
        }));
        env.define("string->number", new Val.Builtin("string->number", args -> {
            checkArgCount(args, 1, "string->number");
            if (!(args.get(0) instanceof Val.Str s)) throw new RuntimeException("string->number: not a string");
            try {
                return new Val.Int(Long.parseLong(s.value()));
            } catch (NumberFormatException e) {
                return new Val.Bool(false);
            }
        }));
        env.define("number->string", new Val.Builtin("number->string", args -> {
            checkArgCount(args, 1, "number->string");
            return new Val.Str(String.valueOf(asInt(args.get(0))));
        }));
        env.define("symbol->string", new Val.Builtin("symbol->string", args -> {
            checkArgCount(args, 1, "symbol->string");
            if (!(args.get(0) instanceof Val.Sym s)) throw new RuntimeException("symbol->string: not a symbol");
            return new Val.Str(s.name());
        }));
        env.define("string->symbol", new Val.Builtin("string->symbol", args -> {
            checkArgCount(args, 1, "string->symbol");
            if (!(args.get(0) instanceof Val.Str s)) throw new RuntimeException("string->symbol: not a string");
            return new Val.Sym(s.value());
        }));
        env.define("string-ref", new Val.Builtin("string-ref", args -> {
            checkArgCount(args, 2, "string-ref");
            if (!(args.get(0) instanceof Val.Str s)) throw new RuntimeException("string-ref: not a string");
            int idx = (int) asInt(args.get(1));
            return new Val.Chr(s.value().charAt(idx));
        }));
        env.define("char?", new Val.Builtin("char?", args -> {
            checkArgCount(args, 1, "char?");
            return new Val.Bool(args.get(0) instanceof Val.Chr);
        }));
        // L06 — mutable strings
        env.define("string-copy", new Val.Builtin("string-copy", args -> {
            checkArgCount(args, 1, "string-copy");
            if (!(args.get(0) instanceof Val.Str s)) throw new RuntimeException("string-copy: not a string");
            return new Val.Str(s.value());
        }));
        env.define("string-set!", new Val.Builtin("string-set!", args -> {
            checkArgCount(args, 3, "string-set!");
            if (!(args.get(0) instanceof Val.Str s)) throw new RuntimeException("string-set!: not a string");
            int idx = (int) asInt(args.get(1));
            if (!(args.get(2) instanceof Val.Chr c)) throw new RuntimeException("string-set!: not a character");
            s.setChar(idx, c.value());
            return new Val.Void();
        }));
        return env;
    }

    // ── Public API ───────────────────────────────────────────────
    public String evalStr(String input) throws EvalError {
        positions.clear();
        output = new StringBuilder();
        List<Token> tokens = tokenize(input);
        if (tokens.isEmpty()) throw new EvalError("empty input");
        int[] idx = {0};
        Env env = createGlobalEnv();
        Val result = null;
        while (idx[0] < tokens.size()) {
            result = parse(tokens, idx);
            result = eval(result, env);
        }
        if (result instanceof Val.Void) return "#<void>";
        return writeVal(result);
    }

    public EvalResult evalStrWithOutput(String input) throws EvalError {
        positions.clear();
        output = new StringBuilder();
        List<Token> tokens = tokenize(input);
        if (tokens.isEmpty()) throw new EvalError("empty input");
        int[] idx = {0};
        Env env = createGlobalEnv();
        Val result = null;
        while (idx[0] < tokens.size()) {
            result = parse(tokens, idx);
            result = eval(result, env);
        }
        String resultStr = (result instanceof Val.Void) ? "#<void>" : writeVal(result);
        return new EvalResult(resultStr, output.toString());
    }
}
