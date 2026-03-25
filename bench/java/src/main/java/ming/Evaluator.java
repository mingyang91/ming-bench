package ming;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.HashSet;
import java.util.IdentityHashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;

public class Evaluator {

    // ── Value types ──────────────────────────────────────────────
    private sealed interface Val permits Val.Int, Val.Rat, Val.Flo, Val.Bool, Val.Str, Val.Sym, Val.Chr, Val.PairV, Val.Nil, Val.Void, Val.Builtin, Val.Lambda, Val.CaseLambda, Val.Macro, Val.RecordInstance, Val.Vec {
        record Int(long value) implements Val {}
        record Rat(long num, long den) implements Val {}
        record Flo(double value) implements Val {}
        record Bool(boolean value) implements Val {}
        final class Str implements Val {
            private final char[] chars;
            private boolean immutable;
            Str(String value) { this.chars = value.toCharArray(); this.immutable = false; }
            static Str literal(String value) { Str s = new Str(value); s.immutable = true; return s; }
            String value() { return new String(chars); }
            void setChar(int idx, char c) {
                if (immutable) throw new RuntimeException("string-set!: string is immutable");
                chars[idx] = c;
            }
            int length() { return chars.length; }
            boolean isImmutable() { return immutable; }
        }
        record Sym(String name) implements Val {}
        record Chr(char value) implements Val {}
        record PairV(Val car, Val cdr) implements Val {}
        record Nil() implements Val {}
        record Void() implements Val {}
        record Builtin(String name, java.util.function.Function<List<Val>, Val> fn) implements Val {}
        record Lambda(List<String> params, String restParam, List<Val> body, Env closure) implements Val {}
        record CaseLambda(List<Lambda> clauses) implements Val {}
        final class RecordInstance implements Val {
            final Object tag; // identity object for type distinction
            final String typeName;
            final String[] fieldNames;
            final Val[] fields;
            RecordInstance(Object tag, String typeName, String[] fieldNames, Val[] fields) {
                this.tag = tag; this.typeName = typeName; this.fieldNames = fieldNames; this.fields = fields;
            }
        }
        final class Vec implements Val {
            final Val[] elements;
            Vec(Val[] elements) { this.elements = elements; }
        }
        final class Macro implements Val {
            final String name;
            final List<String> literals;
            final List<Val> patterns;
            final List<Val> templates;
            final Env defEnv;
            Macro(String name, List<String> literals, List<Val> patterns, List<Val> templates, Env defEnv) {
                this.name = name;
                this.literals = literals;
                this.patterns = patterns;
                this.templates = templates;
                this.defEnv = defEnv;
            }
        }
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
    private int gensymCounter = 0;
    private String gensym(String base) { return base + "_g" + (gensymCounter++); }
    private static final Set<String> SPECIAL_FORMS = Set.of(
        "quote", "if", "define", "lambda", "set!", "begin", "let", "let*", "cond", "and", "or",
        "define-syntax", "define-record-type", "letrec", "letrec*", "case", "do"
    );

    // ── Write representation (with quotes) ──────────────────────
    private static String writeVal(Val v) {
        return switch (v) {
            case Val.Int i -> String.valueOf(i.value());
            case Val.Rat r -> r.num() + "/" + r.den();
            case Val.Flo f -> String.valueOf(f.value());
            case Val.Bool b -> b.value() ? "#t" : "#f";
            case Val.Str s -> "\"" + s.value() + "\"";
            case Val.Sym s -> s.name();
            case Val.Chr c -> "#\\" + switch (c.value()) {
                case ' ' -> "space";
                case '\n' -> "newline";
                case '\t' -> "tab";
                default -> String.valueOf(c.value());
            };
            case Val.Nil ignored -> "()";
            case Val.Void ignored -> "#<void>";
            case Val.PairV p -> writePair(p);
            case Val.Builtin b -> "#<procedure:" + b.name() + ">";
            case Val.Lambda ignored -> "#<procedure>";
            case Val.CaseLambda ignored -> "#<procedure>";
            case Val.RecordInstance r -> "#<record:" + r.typeName + ">";
            case Val.Macro m -> "#<macro:" + m.name + ">";
            case Val.Vec vec -> {
                StringBuilder sb = new StringBuilder("#(");
                for (int i = 0; i < vec.elements.length; i++) {
                    if (i > 0) sb.append(' ');
                    sb.append(writeVal(vec.elements[i]));
                }
                sb.append(')');
                yield sb.toString();
            }
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
            case Val.Vec vec -> {
                StringBuilder sb = new StringBuilder("#(");
                for (int i = 0; i < vec.elements.length; i++) {
                    if (i > 0) sb.append(' ');
                    sb.append(displayVal(vec.elements[i]));
                }
                sb.append(')');
                yield sb.toString();
            }
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

    private static boolean isEq(Val a, Val b) {
        if (a == b) return true;
        if (a instanceof Val.Int ai && b instanceof Val.Int bi) return ai.value() == bi.value();
        if (a instanceof Val.Rat ar && b instanceof Val.Rat br) return ar.num() == br.num() && ar.den() == br.den();
        if (a instanceof Val.Flo af && b instanceof Val.Flo bf) return af.value() == bf.value();
        if (a instanceof Val.Bool ab && b instanceof Val.Bool bb) return ab.value() == bb.value();
        if (a instanceof Val.Sym as && b instanceof Val.Sym bs) return as.name().equals(bs.name());
        if (a instanceof Val.Chr ac && b instanceof Val.Chr bc) return ac.value() == bc.value();
        if (a instanceof Val.Nil && b instanceof Val.Nil) return true;
        if (a instanceof Val.Void && b instanceof Val.Void) return true;
        return false;
    }

    private static boolean isEqual(Val a, Val b) {
        if (a instanceof Val.PairV pa && b instanceof Val.PairV pb) {
            return isEqual(pa.car(), pb.car()) && isEqual(pa.cdr(), pb.cdr());
        }
        if (a instanceof Val.Str sa && b instanceof Val.Str sb) return sa.value().equals(sb.value());
        if (a instanceof Val.Vec va && b instanceof Val.Vec vb) {
            if (va.elements.length != vb.elements.length) return false;
            for (int i = 0; i < va.elements.length; i++) {
                if (!isEqual(va.elements[i], vb.elements[i])) return false;
            }
            return true;
        }
        return isEq(a, b);
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
            Val dotTail = null;
            while (idx[0] < tokens.size() && !tokens.get(idx[0]).text().equals(")")) {
                if (tokens.get(idx[0]).text().equals(".")) {
                    idx[0]++; // skip dot
                    dotTail = parse(tokens, idx);
                    break;
                }
                elems.add(parse(tokens, idx));
            }
            if (idx[0] >= tokens.size()) throw new EvalError(tok.line() + ":" + tok.col() + ": missing )");
            idx[0]++; // skip )
            Val list = (dotTail != null) ? dotTail : new Val.Nil();
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
            return Val.Str.literal(sb.toString());
        }
        try {
            return new Val.Int(Long.parseLong(tok));
        } catch (NumberFormatException e) {
            // Try rational literal: e.g. 3/4, -1/3
            int slashIdx = tok.indexOf('/');
            if (slashIdx > 0 && slashIdx < tok.length() - 1) {
                try {
                    long num = Long.parseLong(tok.substring(0, slashIdx));
                    long den = Long.parseLong(tok.substring(slashIdx + 1));
                    return makeRat(num, den);
                } catch (NumberFormatException e2) { /* fall through */ }
            }
            // Try float literal
            if (tok.contains(".")) {
                try {
                    return new Val.Flo(Double.parseDouble(tok));
                } catch (NumberFormatException e2) { /* fall through */ }
            }
            return new Val.Sym(tok);
        }
    }

    // ── Eval ─────────────────────────────────────────────────────
    private Val eval(Val expr, Env env) throws EvalError {
        return switch (expr) {
            case Val.Int i -> i;
            case Val.Rat r -> r;
            case Val.Flo f -> f;
            case Val.Bool b -> b;
            case Val.Str s -> s;
            case Val.Nil n -> n;
            case Val.Void v -> v;
            case Val.Chr c -> c;
            case Val.Builtin b -> b;
            case Val.Lambda l -> l;
            case Val.CaseLambda cl -> cl;
            case Val.Macro m -> m;
            case Val.RecordInstance r -> r;
            case Val.Vec v -> v;
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
                case "define-syntax" -> { return evalDefineSyntax(pair, env); }
                case "define-record-type" -> { return evalDefineRecordType(pair.cdr(), env, pair); }
                case "case-lambda" -> { return evalCaseLambda(pair.cdr(), env, pair); }
                case "let*" -> { return evalLetStar(pair.cdr(), env, pair); }
                case "letrec" -> { return evalLetrec(pair.cdr(), env, pair); }
                case "letrec*" -> { return evalLetrecStar(pair.cdr(), env, pair); }
                case "case" -> { return evalCase(pair.cdr(), env, pair); }
                case "do" -> { return evalDo(pair.cdr(), env, pair); }
            }
        }

        // Function call
        Val fn = eval(head, env);
        if (fn instanceof Val.Macro macro) {
            return expandAndEvalMacro(macro, pair, env);
        }
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
        if (fn instanceof Val.CaseLambda cl) {
            for (Val.Lambda clause : cl.clauses()) {
                int required = clause.params().size();
                if (clause.restParam() != null) {
                    if (args.size() >= required) return applyFn(clause, args, callSite);
                } else {
                    if (args.size() == required) return applyFn(clause, args, callSite);
                }
            }
            throw posError(callSite, "no matching clause for " + args.size() + " arguments");
        }
        if (fn instanceof Val.Lambda lambda) {
            int required = lambda.params().size();
            if (lambda.restParam() != null) {
                if (args.size() < required)
                    throw posError(callSite, "expected at least " + required + " arguments, got " + args.size());
            } else {
                if (args.size() != required)
                    throw posError(callSite, "expected " + required + " arguments, got " + args.size());
            }
            Env callEnv = new Env(lambda.closure());
            for (int i = 0; i < required; i++) {
                callEnv.define(lambda.params().get(i), args.get(i));
            }
            if (lambda.restParam() != null) {
                Val rest = new Val.Nil();
                for (int i = args.size() - 1; i >= required; i--) {
                    rest = new Val.PairV(args.get(i), rest);
                }
                callEnv.define(lambda.restParam(), rest);
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
            String restParam = null;
            Val paramList = namePair.cdr();
            while (paramList instanceof Val.PairV pp) {
                if (!(pp.car() instanceof Val.Sym paramSym))
                    throw posError(form, "define: expected parameter name");
                params.add(paramSym.name());
                paramList = pp.cdr();
            }
            if (paramList instanceof Val.Sym restSym) {
                restParam = restSym.name();
            }
            List<Val> body = collectList(p.cdr());
            if (body.isEmpty()) throw posError(form, "define: missing body");
            Val.Lambda lambda = new Val.Lambda(params, restParam, body, env);
            env.define(fnName.name(), lambda);
            return new Val.Void();
        }
        throw posError(form, "define: invalid syntax");
    }

    private Val evalLambda(Val args, Env env, Val form) throws EvalError {
        if (!(args instanceof Val.PairV p)) throw posError(form, "lambda requires arguments");
        List<String> params = new ArrayList<>();
        String restParam = null;
        Val paramList = p.car();
        while (paramList instanceof Val.PairV pp) {
            if (!(pp.car() instanceof Val.Sym paramSym))
                throw posError(form, "lambda: expected parameter name");
            params.add(paramSym.name());
            paramList = pp.cdr();
        }
        if (paramList instanceof Val.Sym restSym) {
            restParam = restSym.name();
        }
        List<Val> body = collectList(p.cdr());
        if (body.isEmpty()) throw posError(form, "lambda: missing body");
        return new Val.Lambda(params, restParam, body, env);
    }

    private Val evalCaseLambda(Val args, Env env, Val form) throws EvalError {
        List<Val.Lambda> clauses = new ArrayList<>();
        Val cur = args;
        while (cur instanceof Val.PairV p) {
            Val clauseVal = p.car();
            if (!(clauseVal instanceof Val.PairV clause))
                throw posError(form, "case-lambda: invalid clause");
            List<String> params = new ArrayList<>();
            String restParam = null;
            Val paramList = clause.car();
            while (paramList instanceof Val.PairV pp) {
                if (!(pp.car() instanceof Val.Sym paramSym))
                    throw posError(form, "case-lambda: expected parameter name");
                params.add(paramSym.name());
                paramList = pp.cdr();
            }
            if (paramList instanceof Val.Sym restSym) {
                restParam = restSym.name();
            }
            List<Val> body = collectList(clause.cdr());
            if (body.isEmpty()) throw posError(form, "case-lambda: clause missing body");
            clauses.add(new Val.Lambda(params, restParam, body, env));
            cur = p.cdr();
        }
        if (clauses.isEmpty()) throw posError(form, "case-lambda: no clauses");
        return new Val.CaseLambda(clauses);
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
            Val.Lambda lambda = new Val.Lambda(params, null, body, letEnv);
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

    private Val evalLetStar(Val args, Env env, Val form) throws EvalError {
        if (!(args instanceof Val.PairV p)) throw posError(form, "let*: invalid syntax");
        Env letEnv = new Env(env);
        Val bindings = p.car();
        while (bindings instanceof Val.PairV bp) {
            if (!(bp.car() instanceof Val.PairV binding)) throw posError(form, "let*: invalid binding");
            if (!(binding.car() instanceof Val.Sym varSym)) throw posError(form, "let*: expected variable name");
            if (!(binding.cdr() instanceof Val.PairV valPair)) throw posError(form, "let*: missing init");
            Val val = eval(valPair.car(), letEnv);
            letEnv.define(varSym.name(), val);
            bindings = bp.cdr();
        }
        List<Val> body = collectList(p.cdr());
        if (body.isEmpty()) throw posError(form, "let*: missing body");
        return evalBody(body, letEnv);
    }

    private Val evalLetrec(Val args, Env env, Val form) throws EvalError {
        if (!(args instanceof Val.PairV p)) throw posError(form, "letrec: invalid syntax");
        Env letEnv = new Env(env);
        // First pass: define all variables as void
        List<String> names = new ArrayList<>();
        List<Val> initExprs = new ArrayList<>();
        Val bindings = p.car();
        while (bindings instanceof Val.PairV bp) {
            if (!(bp.car() instanceof Val.PairV binding)) throw posError(form, "letrec: invalid binding");
            if (!(binding.car() instanceof Val.Sym varSym)) throw posError(form, "letrec: expected variable name");
            names.add(varSym.name());
            letEnv.define(varSym.name(), new Val.Void());
            if (!(binding.cdr() instanceof Val.PairV valPair)) throw posError(form, "letrec: missing init");
            initExprs.add(valPair.car());
            bindings = bp.cdr();
        }
        // Second pass: evaluate inits in the letrec env and assign
        for (int i = 0; i < names.size(); i++) {
            Val val = eval(initExprs.get(i), letEnv);
            letEnv.define(names.get(i), val);
        }
        List<Val> body = collectList(p.cdr());
        if (body.isEmpty()) throw posError(form, "letrec: missing body");
        return evalBody(body, letEnv);
    }

    private Val evalLetrecStar(Val args, Env env, Val form) throws EvalError {
        if (!(args instanceof Val.PairV p)) throw posError(form, "letrec*: invalid syntax");
        Env letEnv = new Env(env);
        Val bindings = p.car();
        while (bindings instanceof Val.PairV bp) {
            if (!(bp.car() instanceof Val.PairV binding)) throw posError(form, "letrec*: invalid binding");
            if (!(binding.car() instanceof Val.Sym varSym)) throw posError(form, "letrec*: expected variable name");
            if (!(binding.cdr() instanceof Val.PairV valPair)) throw posError(form, "letrec*: missing init");
            Val val = eval(valPair.car(), letEnv);
            letEnv.define(varSym.name(), val);
            bindings = bp.cdr();
        }
        List<Val> body = collectList(p.cdr());
        if (body.isEmpty()) throw posError(form, "letrec*: missing body");
        return evalBody(body, letEnv);
    }

    private Val evalCase(Val args, Env env, Val form) throws EvalError {
        if (!(args instanceof Val.PairV p)) throw posError(form, "case: invalid syntax");
        Val key = eval(p.car(), env);
        Val clauses = p.cdr();
        while (clauses instanceof Val.PairV cp) {
            Val clause = cp.car();
            if (!(clause instanceof Val.PairV clausePair)) throw posError(form, "case: invalid clause");
            // Check for else clause
            if (clausePair.car() instanceof Val.Sym s && s.name().equals("else")) {
                return evalBegin(clausePair.cdr(), env);
            }
            // Match datums: (datum ...) => body
            Val datums = clausePair.car();
            if (!(datums instanceof Val.PairV)) throw posError(form, "case: expected datum list");
            boolean matched = false;
            Val d = datums;
            while (d instanceof Val.PairV dp) {
                if (isEqv(key, dp.car())) { matched = true; break; }
                d = dp.cdr();
            }
            if (matched) {
                if (clausePair.cdr() instanceof Val.Nil) return new Val.Void();
                return evalBegin(clausePair.cdr(), env);
            }
            clauses = cp.cdr();
        }
        // No match, no else — return void
        return new Val.Void();
    }

    private static boolean isEqv(Val a, Val b) {
        // eqv? is like eq? but also compares characters and exact numbers by value
        return isEq(a, b);
    }

    private Val evalDo(Val args, Env env, Val form) throws EvalError {
        if (!(args instanceof Val.PairV p1)) throw posError(form, "do: invalid syntax");
        // Parse variable clauses: ((var init step) ...)
        List<String> varNames = new ArrayList<>();
        List<Val> stepExprs = new ArrayList<>();  // null if no step
        List<Val> initVals = new ArrayList<>();
        Val varClauses = p1.car();
        while (varClauses instanceof Val.PairV vp) {
            if (!(vp.car() instanceof Val.PairV clause)) throw posError(form, "do: invalid var clause");
            if (!(clause.car() instanceof Val.Sym varSym)) throw posError(form, "do: expected variable name");
            varNames.add(varSym.name());
            if (!(clause.cdr() instanceof Val.PairV initPair)) throw posError(form, "do: missing init");
            initVals.add(eval(initPair.car(), env));
            // Step is optional
            if (initPair.cdr() instanceof Val.PairV stepPair) {
                stepExprs.add(stepPair.car());
            } else {
                stepExprs.add(null);
            }
            varClauses = vp.cdr();
        }
        // Parse test clause: (test expr ...)
        if (!(p1.cdr() instanceof Val.PairV p2)) throw posError(form, "do: missing test clause");
        Val testClause = p2.car();
        if (!(testClause instanceof Val.PairV testPair)) throw posError(form, "do: invalid test clause");
        Val testExpr = testPair.car();
        List<Val> resultExprs = collectList(testPair.cdr());
        // Body expressions
        List<Val> bodyExprs = collectList(p2.cdr());

        // Initialize environment
        Env doEnv = new Env(env);
        for (int i = 0; i < varNames.size(); i++) {
            doEnv.define(varNames.get(i), initVals.get(i));
        }

        // Iteration loop
        while (true) {
            Val testResult = eval(testExpr, doEnv);
            if (isTruthy(testResult)) {
                // Test is true — evaluate result expressions
                if (resultExprs.isEmpty()) return new Val.Void();
                Val result = new Val.Void();
                for (Val expr : resultExprs) result = eval(expr, doEnv);
                return result;
            }
            // Execute body
            for (Val expr : bodyExprs) eval(expr, doEnv);
            // Parallel step: evaluate all steps using current values, then update
            Val[] newVals = new Val[varNames.size()];
            for (int i = 0; i < varNames.size(); i++) {
                if (stepExprs.get(i) != null) {
                    newVals[i] = eval(stepExprs.get(i), doEnv);
                } else {
                    newVals[i] = doEnv.lookup(varNames.get(i));
                }
            }
            for (int i = 0; i < varNames.size(); i++) {
                doEnv.define(varNames.get(i), newVals[i]);
            }
        }
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

    // ── Macros (L10) ────────────────────────────────────────────
    private static class MatchResult {
        final Map<String, Val> singles = new HashMap<>();
        final Map<String, List<Val>> ellipsis = new HashMap<>();
    }

    private Val evalDefineRecordType(Val args, Env env, Val form) throws EvalError {
        // (define-record-type <name> (constructor field ...) predicate (field accessor) ...)
        List<Val> parts = new ArrayList<>();
        Val cur = args;
        while (cur instanceof Val.PairV p) { parts.add(p.car()); cur = p.cdr(); }
        if (parts.size() < 3) throw posError(form, "define-record-type: invalid syntax");

        // type name
        if (!(parts.get(0) instanceof Val.Sym typeSym)) throw posError(form, "define-record-type: expected type name");
        String typeName = typeSym.name();

        // constructor: (make-xxx field1 field2 ...)
        if (!(parts.get(1) instanceof Val.PairV consPair)) throw posError(form, "define-record-type: expected constructor");
        List<Val> consElems = new ArrayList<>();
        Val cc = parts.get(1);
        while (cc instanceof Val.PairV cp) { consElems.add(cp.car()); cc = cp.cdr(); }
        if (consElems.isEmpty() || !(consElems.get(0) instanceof Val.Sym consNameSym))
            throw posError(form, "define-record-type: expected constructor name");
        String consName = consNameSym.name();
        String[] consFields = new String[consElems.size() - 1];
        for (int i = 1; i < consElems.size(); i++) {
            if (!(consElems.get(i) instanceof Val.Sym fs)) throw posError(form, "define-record-type: expected field name");
            consFields[i - 1] = fs.name();
        }

        // predicate
        if (!(parts.get(2) instanceof Val.Sym predSym)) throw posError(form, "define-record-type: expected predicate name");
        String predName = predSym.name();

        // field specs: (field accessor) ...
        String[] fieldNames = new String[parts.size() - 3];
        String[] accessorNames = new String[parts.size() - 3];
        for (int i = 3; i < parts.size(); i++) {
            if (!(parts.get(i) instanceof Val.PairV fp)) throw posError(form, "define-record-type: expected field spec");
            List<Val> fspec = new ArrayList<>();
            Val fc = parts.get(i);
            while (fc instanceof Val.PairV fcp) { fspec.add(fcp.car()); fc = fcp.cdr(); }
            if (fspec.size() < 2) throw posError(form, "define-record-type: field spec needs name and accessor");
            if (!(fspec.get(0) instanceof Val.Sym fnSym)) throw posError(form, "define-record-type: expected field name");
            if (!(fspec.get(1) instanceof Val.Sym anSym)) throw posError(form, "define-record-type: expected accessor name");
            fieldNames[i - 3] = fnSym.name();
            accessorNames[i - 3] = anSym.name();
        }

        // Build field index map (field name -> index in fieldNames array)
        Map<String, Integer> fieldIndex = new HashMap<>();
        for (int i = 0; i < fieldNames.length; i++) fieldIndex.put(fieldNames[i], i);

        // Build constructor field order -> field index mapping
        int[] consFieldIdx = new int[consFields.length];
        for (int i = 0; i < consFields.length; i++) {
            Integer idx = fieldIndex.get(consFields[i]);
            if (idx == null) throw posError(form, "define-record-type: constructor field " + consFields[i] + " not in field specs");
            consFieldIdx[i] = idx;
        }

        // Use a unique tag object for this record type
        final Object tag = new Object();
        final int fieldCount = fieldNames.length;

        // Define constructor
        env.define(consName, new Val.Builtin(consName, cargs -> {
            if (cargs.size() != consFields.length)
                throw new RuntimeException(consName + ": expected " + consFields.length + " arguments, got " + cargs.size());
            Val[] fields = new Val[fieldCount];
            for (int i = 0; i < consFields.length; i++) {
                fields[consFieldIdx[i]] = cargs.get(i);
            }
            return new Val.RecordInstance(tag, typeName, fieldNames, fields);
        }));

        // Define predicate
        env.define(predName, new Val.Builtin(predName, pargs -> {
            if (pargs.size() != 1)
                throw new RuntimeException(predName + ": expected 1 argument, got " + pargs.size());
            return new Val.Bool(pargs.get(0) instanceof Val.RecordInstance ri && ri.tag == tag);
        }));

        // Define accessors
        for (int i = 0; i < fieldNames.length; i++) {
            final int fi = i;
            final String accName = accessorNames[i];
            env.define(accName, new Val.Builtin(accName, aargs -> {
                if (aargs.size() != 1)
                    throw new RuntimeException(accName + ": expected 1 argument, got " + aargs.size());
                if (!(aargs.get(0) instanceof Val.RecordInstance ri) || ri.tag != tag)
                    throw new RuntimeException(accName + ": not a " + typeName);
                return ri.fields[fi];
            }));
        }

        return new Val.Void();
    }

    private Val evalDefineSyntax(Val.PairV form, Env env) throws EvalError {
        Val args = form.cdr();
        if (!(args instanceof Val.PairV dp)) throw posError(form, "define-syntax: invalid syntax");
        if (!(dp.car() instanceof Val.Sym macroName)) throw posError(form, "define-syntax: expected name");
        if (!(dp.cdr() instanceof Val.PairV dp2)) throw posError(form, "define-syntax: expected syntax-rules");
        Val srForm = dp2.car();
        if (!(srForm instanceof Val.PairV srPair)) throw posError(form, "define-syntax: expected syntax-rules form");
        if (!(srPair.car() instanceof Val.Sym srSym) || !srSym.name().equals("syntax-rules"))
            throw posError(form, "define-syntax: expected syntax-rules");
        Val srArgs = srPair.cdr();
        if (!(srArgs instanceof Val.PairV srArgs1)) throw posError(form, "syntax-rules: expected literals list");
        List<String> literals = new ArrayList<>();
        Val litList = srArgs1.car();
        while (litList instanceof Val.PairV lp) {
            if (lp.car() instanceof Val.Sym ls) literals.add(ls.name());
            litList = lp.cdr();
        }
        List<Val> patterns = new ArrayList<>();
        List<Val> templates = new ArrayList<>();
        Val clauses = srArgs1.cdr();
        while (clauses instanceof Val.PairV cp) {
            if (!(cp.car() instanceof Val.PairV clause)) throw posError(form, "syntax-rules: invalid clause");
            patterns.add(clause.car());
            if (!(clause.cdr() instanceof Val.PairV templatePair)) throw posError(form, "syntax-rules: missing template");
            templates.add(templatePair.car());
            clauses = cp.cdr();
        }
        env.define(macroName.name(), new Val.Macro(macroName.name(), literals, patterns, templates, env));
        return new Val.Void();
    }

    private Val expandAndEvalMacro(Val.Macro macro, Val.PairV form, Env env) throws EvalError {
        Val inputArgs = form.cdr();
        for (int i = 0; i < macro.patterns.size(); i++) {
            Val pattern = macro.patterns.get(i);
            Val patternArgs = (pattern instanceof Val.PairV pp) ? pp.cdr() : new Val.Nil();
            MatchResult match = new MatchResult();
            if (doMatch(patternArgs, inputArgs, macro.literals, match)) {
                Val template = macro.templates.get(i);
                Set<String> patternVars = new HashSet<>();
                patternVars.addAll(match.singles.keySet());
                patternVars.addAll(match.ellipsis.keySet());
                Map<String, String> renames = new HashMap<>();
                Set<String> templateIds = new HashSet<>();
                collectIdentifiers(template, templateIds);
                for (String id : templateIds) {
                    if (!patternVars.contains(id) && !SPECIAL_FORMS.contains(id)
                            && !id.equals(macro.name) && !id.equals("...")) {
                        renames.put(id, gensym(id));
                    }
                }
                Val expanded = expandTemplate(template, match, renames);
                for (Map.Entry<String, String> entry : renames.entrySet()) {
                    try {
                        Val defVal = macro.defEnv.lookup(entry.getKey());
                        env.define(entry.getValue(), defVal);
                    } catch (EvalError e) {
                        // Not in defEnv — truly introduced identifier, no pre-binding needed
                    }
                }
                return eval(expanded, env);
            }
        }
        throw posError(form, "no matching pattern for macro " + macro.name);
    }

    private boolean doMatch(Val pattern, Val input, List<String> literals, MatchResult result) {
        if (pattern instanceof Val.Sym sym) {
            String name = sym.name();
            if (name.equals("...")) return false;
            if (literals.contains(name)) {
                return input instanceof Val.Sym is && is.name().equals(name);
            }
            if (name.equals("_")) return true;
            result.singles.put(name, input);
            return true;
        }
        if (pattern instanceof Val.Nil) return input instanceof Val.Nil;
        if (pattern instanceof Val.Bool pb) return input instanceof Val.Bool ib && pb.value() == ib.value();
        if (pattern instanceof Val.Int pi) return input instanceof Val.Int ii && pi.value() == ii.value();
        if (pattern instanceof Val.PairV) {
            List<Val> patElems = collectList(pattern);
            int ellipsisIdx = -1;
            for (int j = 0; j < patElems.size(); j++) {
                if (patElems.get(j) instanceof Val.Sym s && s.name().equals("...")) {
                    ellipsisIdx = j;
                    break;
                }
            }
            if (ellipsisIdx >= 0) {
                int fixedBefore = ellipsisIdx - 1;
                Val cur = input;
                for (int j = 0; j < fixedBefore; j++) {
                    if (!(cur instanceof Val.PairV p)) return false;
                    if (!doMatch(patElems.get(j), p.car(), literals, result)) return false;
                    cur = p.cdr();
                }
                Val ellipsisPat = patElems.get(ellipsisIdx - 1);
                if (ellipsisPat instanceof Val.Sym sym) {
                    List<Val> matches = new ArrayList<>();
                    while (cur instanceof Val.PairV p) {
                        matches.add(p.car());
                        cur = p.cdr();
                    }
                    if (!(cur instanceof Val.Nil)) return false;
                    result.ellipsis.put(sym.name(), matches);
                    return true;
                }
                return false;
            } else {
                Val patCur = pattern;
                Val inpCur = input;
                while (patCur instanceof Val.PairV patP) {
                    if (!(inpCur instanceof Val.PairV inpP)) return false;
                    if (!doMatch(patP.car(), inpP.car(), literals, result)) return false;
                    patCur = patP.cdr();
                    inpCur = inpP.cdr();
                }
                if (patCur instanceof Val.Nil) return inpCur instanceof Val.Nil;
                return doMatch(patCur, inpCur, literals, result);
            }
        }
        return false;
    }

    private Val expandTemplate(Val template, MatchResult bindings, Map<String, String> renames) {
        if (template instanceof Val.Sym sym) {
            String name = sym.name();
            if (bindings.singles.containsKey(name)) return bindings.singles.get(name);
            if (renames.containsKey(name)) return new Val.Sym(renames.get(name));
            return template;
        }
        if (template instanceof Val.PairV) {
            List<Val> elems = collectList(template);
            List<Val> expanded = new ArrayList<>();
            for (int i = 0; i < elems.size(); i++) {
                if (i + 1 < elems.size() && elems.get(i + 1) instanceof Val.Sym s && s.name().equals("...")) {
                    Val subTemplate = elems.get(i);
                    Set<String> ellipsisVars = new HashSet<>();
                    collectEllipsisVars(subTemplate, bindings, ellipsisVars);
                    if (!ellipsisVars.isEmpty()) {
                        String firstVar = ellipsisVars.iterator().next();
                        List<Val> vals = bindings.ellipsis.get(firstVar);
                        int count = vals != null ? vals.size() : 0;
                        for (int j = 0; j < count; j++) {
                            MatchResult sub = new MatchResult();
                            sub.singles.putAll(bindings.singles);
                            sub.ellipsis.putAll(bindings.ellipsis);
                            for (String ev : ellipsisVars) {
                                sub.singles.put(ev, bindings.ellipsis.get(ev).get(j));
                            }
                            expanded.add(expandTemplate(subTemplate, sub, renames));
                        }
                    }
                    i++; // skip ...
                } else {
                    expanded.add(expandTemplate(elems.get(i), bindings, renames));
                }
            }
            Val result = new Val.Nil();
            for (int j = expanded.size() - 1; j >= 0; j--) {
                result = new Val.PairV(expanded.get(j), result);
            }
            return result;
        }
        return template;
    }

    private void collectIdentifiers(Val template, Set<String> ids) {
        if (template instanceof Val.Sym sym) {
            ids.add(sym.name());
        } else if (template instanceof Val.PairV pair) {
            collectIdentifiers(pair.car(), ids);
            collectIdentifiers(pair.cdr(), ids);
        }
    }

    private void collectEllipsisVars(Val template, MatchResult bindings, Set<String> vars) {
        if (template instanceof Val.Sym sym && bindings.ellipsis.containsKey(sym.name())) {
            vars.add(sym.name());
        } else if (template instanceof Val.PairV pair) {
            collectEllipsisVars(pair.car(), bindings, vars);
            collectEllipsisVars(pair.cdr(), bindings, vars);
        }
    }

    // ── Numeric helpers ───────────────────────────────────────────
    private static long gcd(long a, long b) {
        a = Math.abs(a); b = Math.abs(b);
        while (b != 0) { long t = b; b = a % b; a = t; }
        return a;
    }

    private static Val makeRat(long num, long den) {
        if (den == 0) throw new RuntimeException("division by zero");
        if (num == 0) return new Val.Int(0);
        if (den < 0) { num = -num; den = -den; }
        long g = gcd(Math.abs(num), den);
        num /= g; den /= g;
        if (den == 1) return new Val.Int(num);
        return new Val.Rat(num, den);
    }

    private static long[] toRat(Val v) {
        if (v instanceof Val.Int i) return new long[]{i.value(), 1};
        if (v instanceof Val.Rat r) return new long[]{r.num(), r.den()};
        throw new RuntimeException("expected exact number, got: " + writeVal(v));
    }

    private static double toDouble(Val v) {
        if (v instanceof Val.Int i) return (double) i.value();
        if (v instanceof Val.Rat r) return (double) r.num() / r.den();
        if (v instanceof Val.Flo f) return f.value();
        throw new RuntimeException("expected number, got: " + writeVal(v));
    }

    private static boolean isNum(Val v) {
        return v instanceof Val.Int || v instanceof Val.Rat || v instanceof Val.Flo;
    }

    private static boolean isInexact(Val v) {
        return v instanceof Val.Flo;
    }

    private static Val numAdd(Val a, Val b) {
        if (a instanceof Val.Flo || b instanceof Val.Flo)
            return new Val.Flo(toDouble(a) + toDouble(b));
        long[] ar = toRat(a), br = toRat(b);
        return makeRat(ar[0] * br[1] + br[0] * ar[1], ar[1] * br[1]);
    }

    private static Val numSub(Val a, Val b) {
        if (a instanceof Val.Flo || b instanceof Val.Flo)
            return new Val.Flo(toDouble(a) - toDouble(b));
        long[] ar = toRat(a), br = toRat(b);
        return makeRat(ar[0] * br[1] - br[0] * ar[1], ar[1] * br[1]);
    }

    private static Val numMul(Val a, Val b) {
        if (a instanceof Val.Flo || b instanceof Val.Flo)
            return new Val.Flo(toDouble(a) * toDouble(b));
        long[] ar = toRat(a), br = toRat(b);
        return makeRat(ar[0] * br[0], ar[1] * br[1]);
    }

    private static Val numDiv(Val a, Val b) {
        if (a instanceof Val.Flo || b instanceof Val.Flo) {
            double bd = toDouble(b);
            if (bd == 0) throw new RuntimeException("division by zero");
            return new Val.Flo(toDouble(a) / bd);
        }
        long[] ar = toRat(a), br = toRat(b);
        if (br[0] == 0) throw new RuntimeException("division by zero");
        return makeRat(ar[0] * br[1], ar[1] * br[0]);
    }

    private static int numCompare(Val a, Val b) {
        if (a instanceof Val.Flo || b instanceof Val.Flo)
            return Double.compare(toDouble(a), toDouble(b));
        long[] ar = toRat(a), br = toRat(b);
        return Long.compare(ar[0] * br[1], br[0] * ar[1]);
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
            Val result = new Val.Int(0);
            for (Val a : args) result = numAdd(result, a);
            return result;
        }));
        env.define("-", new Val.Builtin("-", args -> {
            if (args.isEmpty()) throw new RuntimeException("- requires at least 1 argument");
            if (args.size() == 1) return numSub(new Val.Int(0), args.get(0));
            Val result = args.get(0);
            for (int i = 1; i < args.size(); i++) result = numSub(result, args.get(i));
            return result;
        }));
        env.define("*", new Val.Builtin("*", args -> {
            Val result = new Val.Int(1);
            for (Val a : args) result = numMul(result, a);
            return result;
        }));
        env.define("/", new Val.Builtin("/", args -> {
            if (args.size() < 2) throw new RuntimeException("/ requires at least 2 arguments");
            Val result = args.get(0);
            for (int i = 1; i < args.size(); i++) result = numDiv(result, args.get(i));
            return result;
        }));
        env.define("<", new Val.Builtin("<", args -> {
            checkArgCount(args, 2, "<");
            return new Val.Bool(numCompare(args.get(0), args.get(1)) < 0);
        }));
        env.define(">", new Val.Builtin(">", args -> {
            checkArgCount(args, 2, ">");
            return new Val.Bool(numCompare(args.get(0), args.get(1)) > 0);
        }));
        env.define("=", new Val.Builtin("=", args -> {
            checkArgCount(args, 2, "=");
            return new Val.Bool(numCompare(args.get(0), args.get(1)) == 0);
        }));
        env.define("<=", new Val.Builtin("<=", args -> {
            checkArgCount(args, 2, "<=");
            return new Val.Bool(numCompare(args.get(0), args.get(1)) <= 0);
        }));
        env.define(">=", new Val.Builtin(">=", args -> {
            checkArgCount(args, 2, ">=");
            return new Val.Bool(numCompare(args.get(0), args.get(1)) >= 0);
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
        env.define("procedure?", new Val.Builtin("procedure?", args -> {
            checkArgCount(args, 1, "procedure?");
            Val v = args.get(0);
            return new Val.Bool(v instanceof Val.Lambda || v instanceof Val.CaseLambda || v instanceof Val.Builtin);
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
            return new Val.Bool(isNum(args.get(0)));
        }));
        env.define("integer?", new Val.Builtin("integer?", args -> {
            checkArgCount(args, 1, "integer?");
            return new Val.Bool(args.get(0) instanceof Val.Int);
        }));
        env.define("rational?", new Val.Builtin("rational?", args -> {
            checkArgCount(args, 1, "rational?");
            Val v = args.get(0);
            return new Val.Bool(v instanceof Val.Int || v instanceof Val.Rat);
        }));
        env.define("exact?", new Val.Builtin("exact?", args -> {
            checkArgCount(args, 1, "exact?");
            Val v = args.get(0);
            return new Val.Bool(v instanceof Val.Int || v instanceof Val.Rat);
        }));
        env.define("inexact?", new Val.Builtin("inexact?", args -> {
            checkArgCount(args, 1, "inexact?");
            return new Val.Bool(args.get(0) instanceof Val.Flo);
        }));
        env.define("exact->inexact", new Val.Builtin("exact->inexact", args -> {
            checkArgCount(args, 1, "exact->inexact");
            return new Val.Flo(toDouble(args.get(0)));
        }));
        env.define("inexact->exact", new Val.Builtin("inexact->exact", args -> {
            checkArgCount(args, 1, "inexact->exact");
            Val v = args.get(0);
            if (v instanceof Val.Int || v instanceof Val.Rat) return v;
            if (v instanceof Val.Flo f) {
                // Convert double to exact rational
                double d = f.value();
                if (d == Math.floor(d) && !Double.isInfinite(d)) return new Val.Int((long) d);
                // Use the rational approximation via bit representation
                long bits = Double.doubleToLongBits(d);
                long sign = (bits >> 63) == 0 ? 1 : -1;
                int exp = (int)((bits >> 52) & 0x7ffL) - 1023;
                long mantissa = (bits & 0x000fffffffffffffL) | 0x0010000000000000L;
                // d = sign * mantissa * 2^(exp - 52)
                int shift = exp - 52;
                if (shift >= 0) {
                    return new Val.Int(sign * mantissa * (1L << shift));
                } else {
                    return makeRat(sign * mantissa, 1L << (-shift));
                }
            }
            throw new RuntimeException("inexact->exact: not a number");
        }));
        env.define("numerator", new Val.Builtin("numerator", args -> {
            checkArgCount(args, 1, "numerator");
            Val v = args.get(0);
            if (v instanceof Val.Int i) return new Val.Int(i.value());
            if (v instanceof Val.Rat r) return new Val.Int(r.num());
            throw new RuntimeException("numerator: not a rational number");
        }));
        env.define("denominator", new Val.Builtin("denominator", args -> {
            checkArgCount(args, 1, "denominator");
            Val v = args.get(0);
            if (v instanceof Val.Int) return new Val.Int(1);
            if (v instanceof Val.Rat r) return new Val.Int(r.den());
            throw new RuntimeException("denominator: not a rational number");
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
            if (!(args.get(1) instanceof Val.Int idx)) throw new RuntimeException("string-set!: not an integer");
            if (!(args.get(2) instanceof Val.Chr c)) throw new RuntimeException("string-set!: not a character");
            int i = (int) idx.value();
            if (i < 0 || i >= s.length()) throw new RuntimeException("string-set!: index out of range");
            s.setChar(i, c.value());
            return new Val.Void();
        }));
        // L15 — string immutability helpers
        env.define("string->list", new Val.Builtin("string->list", args -> {
            checkArgCount(args, 1, "string->list");
            if (!(args.get(0) instanceof Val.Str s)) throw new RuntimeException("string->list: not a string");
            Val result = new Val.Nil();
            String v = s.value();
            for (int i = v.length() - 1; i >= 0; i--) {
                result = new Val.PairV(new Val.Chr(v.charAt(i)), result);
            }
            return result;
        }));
        env.define("list->string", new Val.Builtin("list->string", args -> {
            checkArgCount(args, 1, "list->string");
            StringBuilder sb = new StringBuilder();
            Val cur = args.get(0);
            while (cur instanceof Val.PairV p) {
                if (!(p.car() instanceof Val.Chr c)) throw new RuntimeException("list->string: not a character");
                sb.append(c.value());
                cur = p.cdr();
            }
            return new Val.Str(sb.toString());
        }));
        env.define("char->integer", new Val.Builtin("char->integer", args -> {
            checkArgCount(args, 1, "char->integer");
            if (!(args.get(0) instanceof Val.Chr c)) throw new RuntimeException("char->integer: not a character");
            return new Val.Int((long) c.value());
        }));
        env.define("integer->char", new Val.Builtin("integer->char", args -> {
            checkArgCount(args, 1, "integer->char");
            return new Val.Chr((char) asInt(args.get(0)));
        }));
        // L09 — numeric utilities
        env.define("abs", new Val.Builtin("abs", args -> {
            checkArgCount(args, 1, "abs");
            return new Val.Int(Math.abs(asInt(args.get(0))));
        }));
        env.define("modulo", new Val.Builtin("modulo", args -> {
            checkArgCount(args, 2, "modulo");
            long a = asInt(args.get(0)), b = asInt(args.get(1));
            if (b == 0) throw new RuntimeException("modulo: division by zero");
            return new Val.Int(Math.floorMod(a, b));
        }));
        env.define("remainder", new Val.Builtin("remainder", args -> {
            checkArgCount(args, 2, "remainder");
            long a = asInt(args.get(0)), b = asInt(args.get(1));
            if (b == 0) throw new RuntimeException("remainder: division by zero");
            return new Val.Int(a % b);
        }));
        env.define("quotient", new Val.Builtin("quotient", args -> {
            checkArgCount(args, 2, "quotient");
            long a = asInt(args.get(0)), b = asInt(args.get(1));
            if (b == 0) throw new RuntimeException("quotient: division by zero");
            long q = a / b;
            // truncate toward zero (Java default for long division)
            return new Val.Int(q);
        }));
        env.define("min", new Val.Builtin("min", args -> {
            if (args.isEmpty()) throw new RuntimeException("min requires at least 1 argument");
            long result = asInt(args.get(0));
            for (int i = 1; i < args.size(); i++) result = Math.min(result, asInt(args.get(i)));
            return new Val.Int(result);
        }));
        env.define("max", new Val.Builtin("max", args -> {
            if (args.isEmpty()) throw new RuntimeException("max requires at least 1 argument");
            long result = asInt(args.get(0));
            for (int i = 1; i < args.size(); i++) result = Math.max(result, asInt(args.get(i)));
            return new Val.Int(result);
        }));
        env.define("expt", new Val.Builtin("expt", args -> {
            checkArgCount(args, 2, "expt");
            long base = asInt(args.get(0)), exp = asInt(args.get(1));
            long result = 1;
            boolean neg = exp < 0;
            long e = Math.abs(exp);
            for (long i = 0; i < e; i++) result *= base;
            if (neg) return new Val.Int(0); // integer exponentiation truncates
            return new Val.Int(result);
        }));
        env.define("zero?", new Val.Builtin("zero?", args -> {
            checkArgCount(args, 1, "zero?");
            return new Val.Bool(asInt(args.get(0)) == 0);
        }));
        env.define("positive?", new Val.Builtin("positive?", args -> {
            checkArgCount(args, 1, "positive?");
            return new Val.Bool(asInt(args.get(0)) > 0);
        }));
        env.define("negative?", new Val.Builtin("negative?", args -> {
            checkArgCount(args, 1, "negative?");
            return new Val.Bool(asInt(args.get(0)) < 0);
        }));
        env.define("odd?", new Val.Builtin("odd?", args -> {
            checkArgCount(args, 1, "odd?");
            return new Val.Bool(asInt(args.get(0)) % 2 != 0);
        }));
        env.define("even?", new Val.Builtin("even?", args -> {
            checkArgCount(args, 1, "even?");
            return new Val.Bool(asInt(args.get(0)) % 2 == 0);
        }));
        // L09 — list utilities
        env.define("list-ref", new Val.Builtin("list-ref", args -> {
            checkArgCount(args, 2, "list-ref");
            Val cur = args.get(0);
            long idx = asInt(args.get(1));
            for (long i = 0; i < idx; i++) {
                if (!(cur instanceof Val.PairV p)) throw new RuntimeException("list-ref: index out of range");
                cur = p.cdr();
            }
            if (!(cur instanceof Val.PairV p)) throw new RuntimeException("list-ref: index out of range");
            return p.car();
        }));
        env.define("list-tail", new Val.Builtin("list-tail", args -> {
            checkArgCount(args, 2, "list-tail");
            Val cur = args.get(0);
            long idx = asInt(args.get(1));
            for (long i = 0; i < idx; i++) {
                if (!(cur instanceof Val.PairV p)) throw new RuntimeException("list-tail: index out of range");
                cur = p.cdr();
            }
            return cur;
        }));
        env.define("list?", new Val.Builtin("list?", args -> {
            checkArgCount(args, 1, "list?");
            Val cur = args.get(0);
            while (cur instanceof Val.PairV p) {
                cur = p.cdr();
            }
            return new Val.Bool(cur instanceof Val.Nil);
        }));
        // L09 — equality
        env.define("eq?", new Val.Builtin("eq?", args -> {
            checkArgCount(args, 2, "eq?");
            return new Val.Bool(isEq(args.get(0), args.get(1)));
        }));
        env.define("equal?", new Val.Builtin("equal?", args -> {
            checkArgCount(args, 2, "equal?");
            return new Val.Bool(isEqual(args.get(0), args.get(1)));
        }));
        // L09 — assoc
        env.define("assoc", new Val.Builtin("assoc", args -> {
            checkArgCount(args, 2, "assoc");
            Val key = args.get(0);
            Val lst = args.get(1);
            while (lst instanceof Val.PairV p) {
                if (p.car() instanceof Val.PairV entry && isEqual(entry.car(), key)) {
                    return entry;
                }
                lst = p.cdr();
            }
            return new Val.Bool(false);
        }));
        // L09 — built-in map (supports multiple lists)
        env.define("map", new Val.Builtin("map", args -> {
            if (args.size() < 2) throw new RuntimeException("map requires at least 2 arguments");
            Val fn = args.get(0);
            int numLists = args.size() - 1;
            Val[] cursors = new Val[numLists];
            for (int i = 0; i < numLists; i++) cursors[i] = args.get(i + 1);
            List<Val> results = new ArrayList<>();
            while (true) {
                // Check if any list is exhausted
                boolean done = false;
                for (Val c : cursors) {
                    if (!(c instanceof Val.PairV)) { done = true; break; }
                }
                if (done) break;
                List<Val> callArgs = new ArrayList<>();
                for (int i = 0; i < numLists; i++) {
                    callArgs.add(((Val.PairV) cursors[i]).car());
                    cursors[i] = ((Val.PairV) cursors[i]).cdr();
                }
                try {
                    results.add(applyFn(fn, callArgs, null));
                } catch (EvalError e) {
                    throw new RuntimeException(e.getMessage());
                }
            }
            Val result = new Val.Nil();
            for (int i = results.size() - 1; i >= 0; i--) result = new Val.PairV(results.get(i), result);
            return result;
        }));
        // L09 — character utilities
        env.define("char-alphabetic?", new Val.Builtin("char-alphabetic?", args -> {
            checkArgCount(args, 1, "char-alphabetic?");
            if (!(args.get(0) instanceof Val.Chr c)) throw new RuntimeException("char-alphabetic?: not a character");
            return new Val.Bool(Character.isLetter(c.value()));
        }));
        env.define("char-numeric?", new Val.Builtin("char-numeric?", args -> {
            checkArgCount(args, 1, "char-numeric?");
            if (!(args.get(0) instanceof Val.Chr c)) throw new RuntimeException("char-numeric?: not a character");
            return new Val.Bool(Character.isDigit(c.value()));
        }));
        env.define("char-upcase", new Val.Builtin("char-upcase", args -> {
            checkArgCount(args, 1, "char-upcase");
            if (!(args.get(0) instanceof Val.Chr c)) throw new RuntimeException("char-upcase: not a character");
            return new Val.Chr(Character.toUpperCase(c.value()));
        }));
        env.define("char-downcase", new Val.Builtin("char-downcase", args -> {
            checkArgCount(args, 1, "char-downcase");
            if (!(args.get(0) instanceof Val.Chr c)) throw new RuntimeException("char-downcase: not a character");
            return new Val.Chr(Character.toLowerCase(c.value()));
        }));
        env.define("char=?", new Val.Builtin("char=?", args -> {
            checkArgCount(args, 2, "char=?");
            if (!(args.get(0) instanceof Val.Chr a)) throw new RuntimeException("char=?: not a character");
            if (!(args.get(1) instanceof Val.Chr b)) throw new RuntimeException("char=?: not a character");
            return new Val.Bool(a.value() == b.value());
        }));
        env.define("char<?", new Val.Builtin("char<?", args -> {
            checkArgCount(args, 2, "char<?");
            if (!(args.get(0) instanceof Val.Chr a)) throw new RuntimeException("char<?: not a character");
            if (!(args.get(1) instanceof Val.Chr b)) throw new RuntimeException("char<?: not a character");
            return new Val.Bool(a.value() < b.value());
        }));
        // L09 — string comparison/case
        env.define("string=?", new Val.Builtin("string=?", args -> {
            checkArgCount(args, 2, "string=?");
            if (!(args.get(0) instanceof Val.Str a)) throw new RuntimeException("string=?: not a string");
            if (!(args.get(1) instanceof Val.Str b)) throw new RuntimeException("string=?: not a string");
            return new Val.Bool(a.value().equals(b.value()));
        }));
        env.define("string<?", new Val.Builtin("string<?", args -> {
            checkArgCount(args, 2, "string<?");
            if (!(args.get(0) instanceof Val.Str a)) throw new RuntimeException("string<?: not a string");
            if (!(args.get(1) instanceof Val.Str b)) throw new RuntimeException("string<?: not a string");
            return new Val.Bool(a.value().compareTo(b.value()) < 0);
        }));
        env.define("string-ci=?", new Val.Builtin("string-ci=?", args -> {
            checkArgCount(args, 2, "string-ci=?");
            if (!(args.get(0) instanceof Val.Str a)) throw new RuntimeException("string-ci=?: not a string");
            if (!(args.get(1) instanceof Val.Str b)) throw new RuntimeException("string-ci=?: not a string");
            return new Val.Bool(a.value().equalsIgnoreCase(b.value()));
        }));
        env.define("string-upcase", new Val.Builtin("string-upcase", args -> {
            checkArgCount(args, 1, "string-upcase");
            if (!(args.get(0) instanceof Val.Str s)) throw new RuntimeException("string-upcase: not a string");
            return new Val.Str(s.value().toUpperCase());
        }));
        env.define("string-downcase", new Val.Builtin("string-downcase", args -> {
            checkArgCount(args, 1, "string-downcase");
            if (!(args.get(0) instanceof Val.Str s)) throw new RuntimeException("string-downcase: not a string");
            return new Val.Str(s.value().toLowerCase());
        }));
        // L14 — eqv?
        env.define("eqv?", new Val.Builtin("eqv?", args -> {
            checkArgCount(args, 2, "eqv?");
            return new Val.Bool(isEq(args.get(0), args.get(1)));
        }));
        // L14 — vectors
        env.define("vector", new Val.Builtin("vector", args -> {
            return new Val.Vec(args.toArray(new Val[0]));
        }));
        env.define("make-vector", new Val.Builtin("make-vector", args -> {
            if (args.size() < 1 || args.size() > 2) throw new RuntimeException("make-vector requires 1-2 arguments");
            int len = (int) asInt(args.get(0));
            Val fill = args.size() > 1 ? args.get(1) : new Val.Int(0);
            Val[] elems = new Val[len];
            for (int i = 0; i < len; i++) elems[i] = fill;
            return new Val.Vec(elems);
        }));
        env.define("vector-ref", new Val.Builtin("vector-ref", args -> {
            checkArgCount(args, 2, "vector-ref");
            if (!(args.get(0) instanceof Val.Vec v)) throw new RuntimeException("vector-ref: not a vector");
            int idx = (int) asInt(args.get(1));
            return v.elements[idx];
        }));
        env.define("vector-set!", new Val.Builtin("vector-set!", args -> {
            checkArgCount(args, 3, "vector-set!");
            if (!(args.get(0) instanceof Val.Vec v)) throw new RuntimeException("vector-set!: not a vector");
            int idx = (int) asInt(args.get(1));
            v.elements[idx] = args.get(2);
            return new Val.Void();
        }));
        env.define("vector-length", new Val.Builtin("vector-length", args -> {
            checkArgCount(args, 1, "vector-length");
            if (!(args.get(0) instanceof Val.Vec v)) throw new RuntimeException("vector-length: not a vector");
            return new Val.Int(v.elements.length);
        }));
        env.define("vector?", new Val.Builtin("vector?", args -> {
            checkArgCount(args, 1, "vector?");
            return new Val.Bool(args.get(0) instanceof Val.Vec);
        }));
        env.define("vector->list", new Val.Builtin("vector->list", args -> {
            checkArgCount(args, 1, "vector->list");
            if (!(args.get(0) instanceof Val.Vec v)) throw new RuntimeException("vector->list: not a vector");
            Val result = new Val.Nil();
            for (int i = v.elements.length - 1; i >= 0; i--) result = new Val.PairV(v.elements[i], result);
            return result;
        }));
        env.define("list->vector", new Val.Builtin("list->vector", args -> {
            checkArgCount(args, 1, "list->vector");
            List<Val> elems = new ArrayList<>();
            Val cur = args.get(0);
            while (cur instanceof Val.PairV p) { elems.add(p.car()); cur = p.cdr(); }
            return new Val.Vec(elems.toArray(new Val[0]));
        }));
        // L14 — error
        env.define("error", new Val.Builtin("error", args -> {
            if (args.isEmpty()) throw new RuntimeException("error");
            StringBuilder sb = new StringBuilder();
            // First arg is who (or #f), rest are message parts
            if (args.size() > 1) {
                for (int i = 1; i < args.size(); i++) sb.append(displayVal(args.get(i)));
            } else {
                sb.append(displayVal(args.get(0)));
            }
            throw new RuntimeException(sb.toString());
        }));
        // L08 — apply
        env.define("apply", new Val.Builtin("apply", args -> {
            if (args.size() < 2) throw new RuntimeException("apply requires at least 2 arguments");
            Val fn = args.get(0);
            Val lastArg = args.get(args.size() - 1);
            List<Val> callArgs = new ArrayList<>();
            for (int i = 1; i < args.size() - 1; i++) {
                callArgs.add(args.get(i));
            }
            Val cur = lastArg;
            while (cur instanceof Val.PairV p) {
                callArgs.add(p.car());
                cur = p.cdr();
            }
            try {
                return applyFn(fn, callArgs, null);
            } catch (EvalError e) {
                throw new RuntimeException(e.getMessage());
            }
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
