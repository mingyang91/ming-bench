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
    private sealed interface Val permits Val.Int, Val.Rat, Val.Flo, Val.Bool, Val.Str, Val.Sym, Val.Chr, Val.PairV, Val.Nil, Val.Void, Val.Builtin, Val.Lambda, Val.CaseLambda, Val.Macro, Val.RecordInstance, Val.Vec, Val.ContVal, Val.CallccVal, Val.DynamicWindVal {
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
        final class PairV implements Val {
            Val car;
            Val cdr;
            PairV(Val car, Val cdr) { this.car = car; this.cdr = cdr; }
            Val car() { return car; }
            Val cdr() { return cdr; }
        }
        record Nil() implements Val {}
        record Void() implements Val {}
        record Builtin(String name, java.util.function.Function<List<Val>, Val> fn) implements Val {}
        record Lambda(List<String> params, String restParam, List<Val> body, Env closure) implements Val {}
        record CaseLambda(List<Lambda> clauses) implements Val {}
        final class RecordInstance implements Val {
            final Object tag;
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
                this.name = name; this.literals = literals; this.patterns = patterns;
                this.templates = templates; this.defEnv = defEnv;
            }
        }
        // Continuation value (captured by call/cc)
        final class ContVal implements Val {
            final Kont savedK;
            final List<WindEntry> savedWinds;
            ContVal(Kont savedK, List<WindEntry> savedWinds) { this.savedK = savedK; this.savedWinds = savedWinds; }
        }
        // call/cc as a first-class value
        final class CallccVal implements Val {}
        // dynamic-wind as a first-class value
        final class DynamicWindVal implements Val {}
    }

    // ── Token with position ─────────────────────────────────────
    private record Token(String text, int line, int col) {}

    // ── Position tracking ───────────────────────────────────────
    private final IdentityHashMap<Val, int[]> positions = new IdentityHashMap<>();

    private void setPos(Val v, int line, int col) { positions.put(v, new int[]{line, col}); }
    private void copyPos(Val from, Val to) {
        int[] pos = positions.get(from);
        if (pos != null) positions.put(to, pos);
    }
    private String posPrefix(Val v) {
        int[] pos = positions.get(v);
        if (pos != null) return pos[0] + ":" + pos[1] + ": ";
        return "";
    }
    private EvalError posError(Val v, String msg) { return new EvalError(posPrefix(v) + msg); }

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
        void define(String name, Val val) { bindings.put(name, val); }
        void set(String name, Val val) throws EvalError {
            if (bindings.containsKey(name)) { bindings.put(name, val); return; }
            if (parent != null) { parent.set(name, val); return; }
            throw new EvalError("unbound variable: " + name);
        }
    }

    // ── Output capture ──────────────────────────────────────────
    private StringBuilder output = new StringBuilder();
    private int gensymCounter = 0;

    // ── Dynamic wind ─────────────────────────────────────────────
    private record WindEntry(Val inThunk, Val outThunk) {}
    private final List<WindEntry> windStack = new ArrayList<>();
    private String gensym(String base) { return base + "_g" + (gensymCounter++); }
    private static final Set<String> SPECIAL_FORMS = Set.of(
        "quote", "if", "define", "lambda", "set!", "begin", "let", "let*", "cond", "and", "or",
        "define-syntax", "define-record-type", "letrec", "letrec*", "case", "do", "case-lambda"
    );

    // ── Write representation ────────────────────────────────────
    private static String writeVal(Val v) {
        return switch (v) {
            case Val.Int i -> String.valueOf(i.value());
            case Val.Rat r -> r.num() + "/" + r.den();
            case Val.Flo f -> String.valueOf(f.value());
            case Val.Bool b -> b.value() ? "#t" : "#f";
            case Val.Str s -> "\"" + s.value() + "\"";
            case Val.Sym s -> s.name();
            case Val.Chr c -> "#\\" + switch (c.value()) {
                case ' ' -> "space"; case '\n' -> "newline"; case '\t' -> "tab";
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
            case Val.ContVal ignored -> "#<continuation>";
            case Val.CallccVal ignored -> "#<procedure:call/cc>";
            case Val.DynamicWindVal ignored -> "#<procedure:dynamic-wind>";
        };
    }

    private static String writePair(Val.PairV p) {
        Set<Val> seen = java.util.Collections.newSetFromMap(new IdentityHashMap<>());
        StringBuilder sb = new StringBuilder("(");
        Val cur = p;
        boolean first = true;
        while (cur instanceof Val.PairV pair) {
            if (!seen.add(pair)) { sb.append(" ..."); break; }
            if (!first) sb.append(' ');
            first = false;
            sb.append(writeVal(pair.car()));
            cur = pair.cdr();
        }
        if (!(cur instanceof Val.Nil) && !(cur instanceof Val.PairV)) {
            sb.append(" . ").append(writeVal(cur));
        }
        sb.append(')');
        return sb.toString();
    }

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
        Set<Val> seen = java.util.Collections.newSetFromMap(new IdentityHashMap<>());
        StringBuilder sb = new StringBuilder("(");
        Val cur = p;
        boolean first = true;
        while (cur instanceof Val.PairV pair) {
            if (!seen.add(pair)) { sb.append(" ..."); break; }
            if (!first) sb.append(' ');
            first = false;
            sb.append(displayVal(pair.car()));
            cur = pair.cdr();
        }
        if (!(cur instanceof Val.Nil) && !(cur instanceof Val.PairV)) {
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

    private static boolean isEqual(Val a, Val b) { return isEqualImpl(a, b, new HashSet<>()); }
    private static boolean isEqualImpl(Val a, Val b, Set<Long> seen) {
        if (a == b) return true;
        if (a instanceof Val.PairV pa && b instanceof Val.PairV pb) {
            long key = ((long) System.identityHashCode(pa) << 32) | (System.identityHashCode(pb) & 0xFFFFFFFFL);
            if (!seen.add(key)) return true;
            return isEqualImpl(pa.car(), pb.car(), seen) && isEqualImpl(pa.cdr(), pb.cdr(), seen);
        }
        if (a instanceof Val.Str sa && b instanceof Val.Str sb) return sa.value().equals(sb.value());
        if (a instanceof Val.Vec va && b instanceof Val.Vec vb) {
            if (va.elements.length != vb.elements.length) return false;
            for (int i = 0; i < va.elements.length; i++) {
                if (!isEqualImpl(va.elements[i], vb.elements[i], seen)) return false;
            }
            return true;
        }
        return isEq(a, b);
    }

    private static boolean isEqv(Val a, Val b) { return isEq(a, b); }
    private static boolean isTruthy(Val v) { return !(v instanceof Val.Bool b && !b.value()); }

    // ── Tokenizer ───────────────────────────────────────────────
    private static List<Token> tokenize(String input) {
        List<Token> tokens = new ArrayList<>();
        int i = 0, len = input.length(), line = 1, col = 1;
        while (i < len) {
            char c = input.charAt(i);
            if (c == '\n') { i++; line++; col = 1; continue; }
            if (Character.isWhitespace(c)) { i++; col++; continue; }
            if (c == ';') { while (i < len && input.charAt(i) != '\n') { i++; col++; } continue; }
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
                if (i < len) { i++; col++; }
                tokens.add(new Token(sb.toString(), line, startCol));
                continue;
            }
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

    // ── Parser ──────────────────────────────────────────────────
    private Val parse(List<Token> tokens, int[] idx) throws EvalError {
        if (idx[0] >= tokens.size()) throw new EvalError("unexpected EOF");
        Token tok = tokens.get(idx[0]++);
        if (tok.text().equals("(")) {
            List<Val> elems = new ArrayList<>();
            Val dotTail = null;
            while (idx[0] < tokens.size() && !tokens.get(idx[0]).text().equals(")")) {
                if (tokens.get(idx[0]).text().equals(".")) {
                    idx[0]++;
                    dotTail = parse(tokens, idx);
                    break;
                }
                elems.add(parse(tokens, idx));
            }
            if (idx[0] >= tokens.size()) throw new EvalError(tok.line() + ":" + tok.col() + ": missing )");
            idx[0]++;
            Val list = (dotTail != null) ? dotTail : new Val.Nil();
            for (int i = elems.size() - 1; i >= 0; i--) {
                Val pair = new Val.PairV(elems.get(i), list);
                copyPos(elems.get(i), pair);
                list = pair;
            }
            if (list instanceof Val.PairV) setPos(list, tok.line(), tok.col());
            else setPos(list, tok.line(), tok.col());
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
                        case 'n' -> sb.append('\n'); case 't' -> sb.append('\t');
                        case '\\' -> sb.append('\\'); case '"' -> sb.append('"');
                        default -> { sb.append('\\'); sb.append(next); }
                    }
                } else sb.append(raw.charAt(i));
            }
            return Val.Str.literal(sb.toString());
        }
        try { return new Val.Int(Long.parseLong(tok)); }
        catch (NumberFormatException e) {
            int slashIdx = tok.indexOf('/');
            if (slashIdx > 0 && slashIdx < tok.length() - 1) {
                try {
                    long num = Long.parseLong(tok.substring(0, slashIdx));
                    long den = Long.parseLong(tok.substring(slashIdx + 1));
                    return makeRat(num, den);
                } catch (NumberFormatException e2) { /* fall through */ }
            }
            if (tok.contains(".")) {
                try { return new Val.Flo(Double.parseDouble(tok)); }
                catch (NumberFormatException e2) { /* fall through */ }
            }
            return new Val.Sym(tok);
        }
    }

    // ── CEK Machine ─────────────────────────────────────────────

    // Continuation frames
    private sealed interface Kont {
        record HaltK() implements Kont {}
        record SeqK(List<Val> body, int next, Env env, Kont k) implements Kont {}
        record IfK(Val thenExpr, Val elseExpr, Env env, Kont k) implements Kont {}
        record DefineK(String name, Env env, Kont k) implements Kont {}
        record SetK(String name, Env env, Kont k) implements Kont {}
        record OpK(Val argsList, Env env, Kont k, Val form) implements Kont {}
        record ArgK(Val fn, List<Val> done, List<Val> allExprs, int nextIdx, Env env, Kont k, Val form) implements Kont {}
        record AndK(Val remaining, Env env, Kont k) implements Kont {}
        record OrK(Val remaining, Env env, Kont k) implements Kont {}
        record CondK(Val clauseBody, Val remaining, Env env, Kont k) implements Kont {}
        record CaseK(Val clauses, Env env, Kont k, Val form) implements Kont {}
        record LetK(List<String> names, int idx, List<Val> initExprs, List<Val> initVals, Env evalEnv, Env letEnv, List<Val> body, Kont k) implements Kont {}
        record LetStarK(String varName, Val remainingBindings, Env letEnv, List<Val> body, Kont k, Val form) implements Kont {}
        record LetrecK(List<String> names, int idx, List<Val> initExprs, Env letEnv, List<Val> body, Kont k) implements Kont {}
        record NamedLetK(Val.Lambda lambda, int idx, List<Val> initExprs, List<Val> initVals, Env evalEnv, Kont k, Val form) implements Kont {}
        record CallccK(Kont k) implements Kont {}
        // dynamic-wind continuation frames
        record DynWindCallInK(Val inThunk, Val bodyThunk, Val outThunk, Kont k) implements Kont {}
        record DynWindCallBodyK(Val inThunk, Val outThunk, Kont k) implements Kont {}
        record DynWindCallOutK(Val bodyVal, Kont k) implements Kont {}
        // wind transfer frames for continuation invocation
        record DynWindUnwindK(List<Val> remainingOuts, List<WindEntry> toRewind, Val value, Kont targetK, List<WindEntry> targetWinds) implements Kont {}
        record DynWindRewindK(List<WindEntry> remaining, Val value, Kont targetK, List<WindEntry> targetWinds) implements Kont {}
    }

    // CEK step: either evaluate an expression or apply a continuation
    private sealed interface Step {
        record Eval(Val expr, Env env, Kont k) implements Step {}
        record Apply(Val value, Kont k) implements Step {}
    }

    // Exception for continuation invocation from within builtins
    private static class ContinuationInvoke extends RuntimeException {
        final Val.ContVal cont;
        final Val value;
        ContinuationInvoke(Val.ContVal cont, Val value) {
            super(null, null, true, false);
            this.cont = cont;
            this.value = value;
        }
    }

    // ── Trampoline ──────────────────────────────────────────────
    private Val run(Step step) throws EvalError {
        while (true) {
            if (step instanceof Step.Apply a && a.k() instanceof Kont.HaltK) {
                return a.value();
            }
            try {
                step = advance(step);
            } catch (ContinuationInvoke ci) {
                step = windTransfer(ci.value, ci.cont.savedK, ci.cont.savedWinds);
            }
        }
    }

    private Step advance(Step step) throws EvalError {
        return switch (step) {
            case Step.Eval e -> evalStep(e.expr(), e.env(), e.k());
            case Step.Apply a -> applyKont(a.k(), a.value());
        };
    }

    // ── Eval Step ───────────────────────────────────────────────
    private Step evalStep(Val expr, Env env, Kont k) throws EvalError {
        if (expr instanceof Val.Sym sym) {
            try { return new Step.Apply(env.lookup(sym.name()), k); }
            catch (EvalError e) { throw posError(expr, e.getMessage()); }
        }
        if (!(expr instanceof Val.PairV pair)) return new Step.Apply(expr, k);

        Val head = pair.car();
        if (head instanceof Val.Sym sym) {
            String fn = sym.name();
            if (SPECIAL_FORMS.contains(fn)) {
                return evalSpecialForm(fn, pair, env, k);
            }
        }
        // Function application: eval operator first
        return new Step.Eval(head, env, new Kont.OpK(pair.cdr(), env, k, pair));
    }

    private Step evalSpecialForm(String fn, Val.PairV pair, Env env, Kont k) throws EvalError {
        return switch (fn) {
            case "quote" -> {
                if (!(pair.cdr() instanceof Val.PairV q)) throw posError(pair, "quote requires 1 argument");
                yield new Step.Apply(q.car(), k);
            }
            case "if" -> {
                Val args = pair.cdr();
                if (!(args instanceof Val.PairV p1)) throw posError(pair, "if requires a condition");
                Val rest = p1.cdr();
                if (!(rest instanceof Val.PairV p2)) throw posError(pair, "if requires a consequent");
                Val thenE = p2.car();
                Val elseE = (p2.cdr() instanceof Val.PairV p3) ? p3.car() : null;
                yield new Step.Eval(p1.car(), env, new Kont.IfK(thenE, elseE, env, k));
            }
            case "define" -> evalDefineStep(pair.cdr(), env, k, pair);
            case "lambda" -> new Step.Apply(buildLambda(pair.cdr(), env, pair), k);
            case "set!" -> {
                Val sc = pair.cdr();
                if (!(sc instanceof Val.PairV sp)) throw posError(pair, "set! requires 2 arguments");
                if (!(sp.car() instanceof Val.Sym ss)) throw posError(pair, "set!: expected variable name");
                if (!(sp.cdr() instanceof Val.PairV sv)) throw posError(pair, "set! requires a value");
                yield new Step.Eval(sv.car(), env, new Kont.SetK(ss.name(), env, k));
            }
            case "begin" -> evalBeginStep(pair.cdr(), env, k);
            case "let" -> evalLetStep(pair.cdr(), env, k, pair);
            case "let*" -> evalLetStarStep(pair.cdr(), env, k, pair);
            case "letrec" -> evalLetrecStep(pair.cdr(), env, k, pair, false);
            case "letrec*" -> evalLetrecStep(pair.cdr(), env, k, pair, true);
            case "cond" -> evalCondStep(pair.cdr(), env, k);
            case "and" -> evalAndStep(pair.cdr(), env, k);
            case "or" -> evalOrStep(pair.cdr(), env, k);
            case "case" -> {
                Val ca = pair.cdr();
                if (!(ca instanceof Val.PairV cp)) throw posError(pair, "case: invalid syntax");
                yield new Step.Eval(cp.car(), env, new Kont.CaseK(cp.cdr(), env, k, pair));
            }
            case "define-syntax" -> {
                Val result = evalDefineSyntax(pair, env);
                yield new Step.Apply(result, k);
            }
            case "define-record-type" -> {
                Val result = evalDefineRecordType(pair.cdr(), env, pair);
                yield new Step.Apply(result, k);
            }
            case "do" -> evalDoStep(pair.cdr(), env, k, pair);
            case "case-lambda" -> new Step.Apply(buildCaseLambda(pair.cdr(), env, pair), k);
            default -> throw posError(pair, "unknown special form: " + fn);
        };
    }

    // ── Apply Continuation ──────────────────────────────────────
    private Step applyKont(Kont kont, Val value) throws EvalError {
        return switch (kont) {
            case Kont.HaltK h -> throw new IllegalStateException("halt in applyKont");

            case Kont.SeqK(var body, var next, var env, var k) -> {
                if (next >= body.size()) yield new Step.Apply(value, k);
                if (next == body.size() - 1) yield new Step.Eval(body.get(next), env, k);
                yield new Step.Eval(body.get(next), env, new Kont.SeqK(body, next + 1, env, k));
            }

            case Kont.IfK(var thenE, var elseE, var env, var k) -> {
                if (isTruthy(value)) yield new Step.Eval(thenE, env, k);
                if (elseE != null) yield new Step.Eval(elseE, env, k);
                yield new Step.Apply(new Val.Void(), k);
            }

            case Kont.DefineK(var name, var env, var k) -> {
                env.define(name, value);
                yield new Step.Apply(new Val.Void(), k);
            }

            case Kont.SetK(var name, var env, var k) -> {
                env.set(name, value);
                yield new Step.Apply(new Val.Void(), k);
            }

            case Kont.OpK(var argsList, var env, var k, var form) -> {
                Val fn = value;
                if (fn instanceof Val.Macro macro) {
                    Val expanded = expandMacro(macro, (Val.PairV) form, env);
                    yield new Step.Eval(expanded, env, k);
                }
                if (fn instanceof Val.CallccVal) {
                    if (!(argsList instanceof Val.PairV ap)) throw posError(form, "call/cc requires 1 argument");
                    if (ap.cdr() instanceof Val.PairV) throw posError(form, "call/cc requires 1 argument");
                    yield new Step.Eval(ap.car(), env, new Kont.CallccK(k));
                }
                if (fn instanceof Val.DynamicWindVal) {
                    List<Val> dwArgs = collectList(argsList);
                    if (dwArgs.size() != 3) throw posError(form, "dynamic-wind requires 3 arguments");
                    // Evaluate all 3 thunk expressions; use ArgK to collect them
                    int last = dwArgs.size() - 1;
                    yield new Step.Eval(dwArgs.get(last), env,
                        new Kont.ArgK(fn, new ArrayList<>(), dwArgs, last - 1, env, k, form));
                }
                List<Val> argExprs = collectList(argsList);
                if (argExprs.isEmpty()) {
                    yield applyFunctionStep(fn, new ArrayList<>(), k, form);
                }
                // Evaluate arguments right-to-left for proper call/cc semantics
                int last = argExprs.size() - 1;
                yield new Step.Eval(argExprs.get(last), env,
                    new Kont.ArgK(fn, new ArrayList<>(), argExprs, last - 1, env, k, form));
            }

            case Kont.ArgK(var fn, var done, var allExprs, var nextIdx, var env, var k, var form) -> {
                List<Val> newDone = new ArrayList<>(done);
                newDone.add(value);
                if (nextIdx < 0) {
                    // All args evaluated (in reverse order). Reverse to get correct order.
                    java.util.Collections.reverse(newDone);
                    yield applyFunctionStep(fn, newDone, k, form);
                }
                yield new Step.Eval(allExprs.get(nextIdx), env,
                    new Kont.ArgK(fn, newDone, allExprs, nextIdx - 1, env, k, form));
            }

            case Kont.AndK(var remaining, var env, var k) -> {
                if (!isTruthy(value)) yield new Step.Apply(value, k);
                if (!(remaining instanceof Val.PairV rp)) yield new Step.Apply(value, k);
                if (!(rp.cdr() instanceof Val.PairV)) yield new Step.Eval(rp.car(), env, k);
                yield new Step.Eval(rp.car(), env, new Kont.AndK(rp.cdr(), env, k));
            }

            case Kont.OrK(var remaining, var env, var k) -> {
                if (isTruthy(value)) yield new Step.Apply(value, k);
                if (!(remaining instanceof Val.PairV rp)) yield new Step.Apply(value, k);
                if (!(rp.cdr() instanceof Val.PairV)) yield new Step.Eval(rp.car(), env, k);
                yield new Step.Eval(rp.car(), env, new Kont.OrK(rp.cdr(), env, k));
            }

            case Kont.CondK(var clauseBody, var remaining, var env, var k) -> {
                if (isTruthy(value)) {
                    if (clauseBody instanceof Val.Nil) yield new Step.Apply(value, k);
                    yield evalBeginStep(clauseBody, env, k);
                }
                yield evalCondStep(remaining, env, k);
            }

            case Kont.CaseK(var clauses, var env, var k, var form) -> {
                Val key = value;
                Val cur = clauses;
                while (cur instanceof Val.PairV clp) {
                    Val clause = clp.car();
                    if (!(clause instanceof Val.PairV cp)) throw posError(form, "case: invalid clause");
                    if (cp.car() instanceof Val.Sym s && s.name().equals("else")) {
                        yield evalBeginStep(cp.cdr(), env, k);
                    }
                    Val datums = cp.car();
                    if (!(datums instanceof Val.PairV)) throw posError(form, "case: expected datum list");
                    boolean matched = false;
                    Val d = datums;
                    while (d instanceof Val.PairV dp) {
                        if (isEqv(key, dp.car())) { matched = true; break; }
                        d = dp.cdr();
                    }
                    if (matched) {
                        if (cp.cdr() instanceof Val.Nil) yield new Step.Apply(new Val.Void(), k);
                        yield evalBeginStep(cp.cdr(), env, k);
                    }
                    cur = clp.cdr();
                }
                yield new Step.Apply(new Val.Void(), k);
            }

            case Kont.LetK(var names, var idx, var initExprs, var initVals, var evalEnv, var letEnv, var body, var k) -> {
                List<Val> newVals = new ArrayList<>(initVals);
                newVals.add(value);
                int next = idx + 1;
                if (next >= names.size()) {
                    for (int i = 0; i < names.size(); i++) letEnv.define(names.get(i), newVals.get(i));
                    yield evalBodyStep(body, letEnv, k);
                }
                yield new Step.Eval(initExprs.get(next), evalEnv,
                    new Kont.LetK(names, next, initExprs, newVals, evalEnv, letEnv, body, k));
            }

            case Kont.LetStarK(var varName, var remaining, var letEnv, var body, var k, var form) -> {
                letEnv.define(varName, value);
                if (!(remaining instanceof Val.PairV bp)) {
                    yield evalBodyStep(body, letEnv, k);
                }
                Val binding = bp.car();
                if (!(binding instanceof Val.PairV bindPair)) throw posError(form, "let*: invalid binding");
                if (!(bindPair.car() instanceof Val.Sym varSym)) throw posError(form, "let*: expected variable name");
                if (!(bindPair.cdr() instanceof Val.PairV valPair)) throw posError(form, "let*: missing init");
                yield new Step.Eval(valPair.car(), letEnv,
                    new Kont.LetStarK(varSym.name(), bp.cdr(), letEnv, body, k, form));
            }

            case Kont.LetrecK(var names, var idx, var initExprs, var letEnv, var body, var k) -> {
                letEnv.define(names.get(idx), value);
                int next = idx + 1;
                if (next >= names.size()) {
                    yield evalBodyStep(body, letEnv, k);
                }
                yield new Step.Eval(initExprs.get(next), letEnv,
                    new Kont.LetrecK(names, next, initExprs, letEnv, body, k));
            }

            case Kont.NamedLetK(var lambda, var idx, var initExprs, var initVals, var evalEnv, var k, var form) -> {
                List<Val> newVals = new ArrayList<>(initVals);
                newVals.add(value);
                int next = idx + 1;
                if (next >= initExprs.size()) {
                    yield applyFunctionStep(lambda, newVals, k, form);
                }
                yield new Step.Eval(initExprs.get(next), evalEnv,
                    new Kont.NamedLetK(lambda, next, initExprs, newVals, evalEnv, k, form));
            }

            case Kont.CallccK(var k) -> {
                // value is the proc, k is the continuation to capture
                Val proc = value;
                Val.ContVal contVal = new Val.ContVal(k, new ArrayList<>(windStack));
                yield applyFunctionStep(proc, List.of(contVal), k, null);
            }

            // dynamic-wind continuation frames
            case Kont.DynWindCallInK(var inThunk, var bodyThunk, var outThunk, var k) -> {
                // in-thunk done; push wind entry, call body-thunk
                windStack.add(new WindEntry(inThunk, outThunk));
                yield applyFunctionStep(bodyThunk, new ArrayList<>(),
                    new Kont.DynWindCallBodyK(inThunk, outThunk, k), null);
            }

            case Kont.DynWindCallBodyK(var inThunk, var outThunk, var k) -> {
                // body done; pop wind entry, call out-thunk, save body value
                windStack.remove(windStack.size() - 1);
                yield applyFunctionStep(outThunk, new ArrayList<>(),
                    new Kont.DynWindCallOutK(value, k), null);
            }

            case Kont.DynWindCallOutK(var bodyVal, var k) -> {
                // out-thunk done; return body value
                yield new Step.Apply(bodyVal, k);
            }

            // wind transfer frames for continuation invocation
            case Kont.DynWindUnwindK(var remainingOuts, var toRewind, var val, var targetK, var targetWinds) -> {
                // An out-thunk just finished; pop wind stack
                windStack.remove(windStack.size() - 1);
                if (!remainingOuts.isEmpty()) {
                    List<Val> rest = new ArrayList<>(remainingOuts);
                    Val nextOut = rest.remove(0);
                    yield applyFunctionStep(nextOut, new ArrayList<>(),
                        new Kont.DynWindUnwindK(rest, toRewind, val, targetK, targetWinds), null);
                }
                // Done unwinding; start rewinding
                if (!toRewind.isEmpty()) {
                    List<WindEntry> rest = new ArrayList<>(toRewind);
                    WindEntry next = rest.remove(0);
                    yield applyFunctionStep(next.inThunk(), new ArrayList<>(),
                        new Kont.DynWindRewindK(rest, val, targetK, targetWinds), null);
                }
                // Nothing to rewind
                yield new Step.Apply(val, targetK);
            }

            case Kont.DynWindRewindK(var remaining, var val, var targetK, var targetWinds) -> {
                // An in-thunk just finished; push the wind entry
                // Figure out which entry was just rewound based on targetWinds and remaining count
                int idx = targetWinds.size() - remaining.size() - 1;
                // Actually we need to track which entry. Let me compute from targetWinds.
                // The entries being rewound start at (targetWinds.size() - remaining.size() - 1)
                // Wait: toRewind was built from targetWinds starting at commonLen.
                // remaining has the rest, the one we just ran is targetWinds[targetWinds.size() - remaining.size() - 1]
                // Simpler: just push the correct entry from targetWinds
                // At this point, windStack should be at commonLen + (number already rewound)
                windStack.add(targetWinds.get(windStack.size()));
                if (!remaining.isEmpty()) {
                    List<WindEntry> rest = new ArrayList<>(remaining);
                    WindEntry next = rest.remove(0);
                    yield applyFunctionStep(next.inThunk(), new ArrayList<>(),
                        new Kont.DynWindRewindK(rest, val, targetK, targetWinds), null);
                }
                // Done rewinding
                yield new Step.Apply(val, targetK);
            }
        };
    }

    // ── Apply Function ──────────────────────────────────────────
    private Step applyFunctionStep(Val fn, List<Val> args, Kont k, Val form) throws EvalError {
        if (fn instanceof Val.ContVal cont) {
            if (args.size() != 1) throw posError(form, "continuation expects 1 argument, got " + args.size());
            return windTransfer(args.get(0), cont.savedK, cont.savedWinds);
        }
        if (fn instanceof Val.CallccVal) {
            if (args.size() != 1) throw posError(form, "call/cc requires 1 argument");
            Val proc = args.get(0);
            Val.ContVal contVal = new Val.ContVal(k, new ArrayList<>(windStack));
            return applyFunctionStep(proc, List.of(contVal), k, form);
        }
        if (fn instanceof Val.DynamicWindVal) {
            if (args.size() != 3) throw posError(form, "dynamic-wind requires 3 arguments");
            Val inThunk = args.get(0), bodyThunk = args.get(1), outThunk = args.get(2);
            // Call in-thunk first
            return applyFunctionStep(inThunk, new ArrayList<>(),
                new Kont.DynWindCallInK(inThunk, bodyThunk, outThunk, k), form);
        }

        Val.Lambda lambda = null;
        if (fn instanceof Val.CaseLambda cl) {
            for (Val.Lambda clause : cl.clauses()) {
                int req = clause.params().size();
                if (clause.restParam() != null) { if (args.size() >= req) { lambda = clause; break; } }
                else { if (args.size() == req) { lambda = clause; break; } }
            }
            if (lambda == null) throw posError(form, "no matching clause for " + args.size() + " arguments");
        } else if (fn instanceof Val.Lambda l) {
            lambda = l;
        }

        if (lambda != null) {
            int req = lambda.params().size();
            if (lambda.restParam() != null) {
                if (args.size() < req) throw posError(form, "expected at least " + req + " arguments, got " + args.size());
            } else {
                if (args.size() != req) throw posError(form, "expected " + req + " arguments, got " + args.size());
            }
            Env callEnv = new Env(lambda.closure());
            for (int i = 0; i < req; i++) callEnv.define(lambda.params().get(i), args.get(i));
            if (lambda.restParam() != null) {
                Val rest = new Val.Nil();
                for (int i = args.size() - 1; i >= req; i--) rest = new Val.PairV(args.get(i), rest);
                callEnv.define(lambda.restParam(), rest);
            }
            return evalBodyStep(lambda.body(), callEnv, k);
        }

        if (fn instanceof Val.Builtin builtin) {
            try {
                Val result = builtin.fn().apply(args);
                return new Step.Apply(result, k);
            } catch (ContinuationInvoke ci) { throw ci; }
            catch (RuntimeException e) { throw posError(form, e.getMessage()); }
        }
        throw posError(form, "not a procedure: " + writeVal(fn));
    }

    // ── Wind transfer for continuation invocation ────────────────
    private Step windTransfer(Val value, Kont targetK, List<WindEntry> targetWinds) throws EvalError {
        // Find common prefix length
        int commonLen = 0;
        int minLen = Math.min(windStack.size(), targetWinds.size());
        while (commonLen < minLen && windStack.get(commonLen) == targetWinds.get(commonLen)) {
            commonLen++;
        }
        // Collect out-thunks to unwind (current[commonLen..end], innermost first)
        List<Val> outsToRun = new ArrayList<>();
        for (int i = windStack.size() - 1; i >= commonLen; i--) {
            outsToRun.add(windStack.get(i).outThunk());
        }
        // Collect wind entries to rewind (target[commonLen..end], outermost first)
        List<WindEntry> toRewind = new ArrayList<>();
        for (int i = commonLen; i < targetWinds.size(); i++) {
            toRewind.add(targetWinds.get(i));
        }
        // If nothing to unwind/rewind, jump directly
        if (outsToRun.isEmpty() && toRewind.isEmpty()) {
            return new Step.Apply(value, targetK);
        }
        // Start unwinding
        if (!outsToRun.isEmpty()) {
            Val firstOut = outsToRun.remove(0);
            return applyFunctionStep(firstOut, new ArrayList<>(),
                new Kont.DynWindUnwindK(outsToRun, toRewind, value, targetK, targetWinds), null);
        }
        // Nothing to unwind, start rewinding
        WindEntry first = toRewind.remove(0);
        return applyFunctionStep(first.inThunk(), new ArrayList<>(),
            new Kont.DynWindRewindK(toRewind, value, targetK, targetWinds), null);
    }

    // ── applyFn for builtins (runs mini-trampoline) ─────────────
    private Val applyFn(Val fn, List<Val> args, Val callSite) throws EvalError {
        if (fn instanceof Val.ContVal cont) {
            if (args.size() != 1) throw posError(callSite, "continuation expects 1 argument");
            throw new ContinuationInvoke(cont, args.get(0));
        }
        Step step = applyFunctionStep(fn, args, new Kont.HaltK(), callSite);
        while (true) {
            if (step instanceof Step.Apply a && a.k() instanceof Kont.HaltK) return a.value();
            try { step = advance(step); }
            catch (ContinuationInvoke ci) { throw ci; } // propagate to outer trampoline
        }
    }

    // ── Step Helpers ────────────────────────────────────────────
    private Step evalBodyStep(List<Val> body, Env env, Kont k) {
        if (body.isEmpty()) return new Step.Apply(new Val.Void(), k);
        if (body.size() == 1) return new Step.Eval(body.get(0), env, k);
        return new Step.Eval(body.get(0), env, new Kont.SeqK(body, 1, env, k));
    }

    private Step evalBeginStep(Val body, Env env, Kont k) {
        if (!(body instanceof Val.PairV p)) return new Step.Apply(new Val.Void(), k);
        if (!(p.cdr() instanceof Val.PairV)) return new Step.Eval(p.car(), env, k);
        List<Val> exprs = collectList(body);
        return evalBodyStep(exprs, env, k);
    }

    private Step evalDefineStep(Val args, Env env, Kont k, Val form) throws EvalError {
        if (!(args instanceof Val.PairV p)) throw posError(form, "define requires arguments");
        Val target = p.car();
        if (target instanceof Val.Sym sym) {
            if (!(p.cdr() instanceof Val.PairV vp)) throw posError(form, "define requires a value");
            return new Step.Eval(vp.car(), env, new Kont.DefineK(sym.name(), env, k));
        }
        if (target instanceof Val.PairV namePair) {
            if (!(namePair.car() instanceof Val.Sym fnName)) throw posError(form, "define: expected function name");
            List<String> params = new ArrayList<>();
            String restParam = null;
            Val paramList = namePair.cdr();
            while (paramList instanceof Val.PairV pp) {
                if (!(pp.car() instanceof Val.Sym ps)) throw posError(form, "define: expected parameter name");
                params.add(ps.name());
                paramList = pp.cdr();
            }
            if (paramList instanceof Val.Sym rs) restParam = rs.name();
            List<Val> body = collectList(p.cdr());
            if (body.isEmpty()) throw posError(form, "define: missing body");
            Val.Lambda lambda = new Val.Lambda(params, restParam, body, env);
            env.define(fnName.name(), lambda);
            return new Step.Apply(new Val.Void(), k);
        }
        throw posError(form, "define: invalid syntax");
    }

    private Step evalLetStep(Val args, Env env, Kont k, Val form) throws EvalError {
        if (!(args instanceof Val.PairV lp)) throw posError(form, "let: invalid syntax");
        // Named let
        if (lp.car() instanceof Val.Sym nameSym) {
            if (!(lp.cdr() instanceof Val.PairV rest)) throw posError(form, "let: invalid syntax");
            List<String> params = new ArrayList<>();
            List<Val> initExprs = new ArrayList<>();
            Val bindings = rest.car();
            while (bindings instanceof Val.PairV bp) {
                if (!(bp.car() instanceof Val.PairV binding)) throw posError(form, "let: invalid binding");
                if (!(binding.car() instanceof Val.Sym vs)) throw posError(form, "let: expected variable name");
                params.add(vs.name());
                if (!(binding.cdr() instanceof Val.PairV vp)) throw posError(form, "let: missing init");
                initExprs.add(vp.car());
                bindings = bp.cdr();
            }
            List<Val> body = collectList(rest.cdr());
            if (body.isEmpty()) throw posError(form, "let: missing body");
            Env letEnv = new Env(env);
            Val.Lambda lambda = new Val.Lambda(params, null, body, letEnv);
            letEnv.define(nameSym.name(), lambda);
            if (initExprs.isEmpty()) {
                return applyFunctionStep(lambda, new ArrayList<>(), k, form);
            }
            return new Step.Eval(initExprs.get(0), env,
                new Kont.NamedLetK(lambda, 0, initExprs, new ArrayList<>(), env, k, form));
        }
        // Regular let
        Val bindings = lp.car();
        List<Val> body = collectList(lp.cdr());
        if (body.isEmpty()) throw posError(form, "let: missing body");
        List<String> names = new ArrayList<>();
        List<Val> initExprs = new ArrayList<>();
        Val cur = bindings;
        while (cur instanceof Val.PairV bp) {
            if (!(bp.car() instanceof Val.PairV binding)) throw posError(form, "let: invalid binding");
            if (!(binding.car() instanceof Val.Sym vs)) throw posError(form, "let: expected variable name");
            names.add(vs.name());
            if (!(binding.cdr() instanceof Val.PairV vp)) throw posError(form, "let: missing init");
            initExprs.add(vp.car());
            cur = bp.cdr();
        }
        Env letEnv = new Env(env);
        if (names.isEmpty()) return evalBodyStep(body, letEnv, k);
        return new Step.Eval(initExprs.get(0), env,
            new Kont.LetK(names, 0, initExprs, new ArrayList<>(), env, letEnv, body, k));
    }

    private Step evalLetStarStep(Val args, Env env, Kont k, Val form) throws EvalError {
        if (!(args instanceof Val.PairV lp)) throw posError(form, "let*: invalid syntax");
        Env letEnv = new Env(env);
        Val bindings = lp.car();
        List<Val> body = collectList(lp.cdr());
        if (body.isEmpty()) throw posError(form, "let*: missing body");
        if (!(bindings instanceof Val.PairV bp)) return evalBodyStep(body, letEnv, k);
        Val binding = bp.car();
        if (!(binding instanceof Val.PairV bindPair)) throw posError(form, "let*: invalid binding");
        if (!(bindPair.car() instanceof Val.Sym vs)) throw posError(form, "let*: expected variable name");
        if (!(bindPair.cdr() instanceof Val.PairV vp)) throw posError(form, "let*: missing init");
        return new Step.Eval(vp.car(), letEnv,
            new Kont.LetStarK(vs.name(), bp.cdr(), letEnv, body, k, form));
    }

    private Step evalLetrecStep(Val args, Env env, Kont k, Val form, boolean isStar) throws EvalError {
        if (!(args instanceof Val.PairV lp)) throw posError(form, (isStar ? "letrec*" : "letrec") + ": invalid syntax");
        Env letEnv = new Env(env);
        List<String> names = new ArrayList<>();
        List<Val> initExprs = new ArrayList<>();
        Val bindings = lp.car();
        while (bindings instanceof Val.PairV bp) {
            if (!(bp.car() instanceof Val.PairV binding)) throw posError(form, "letrec: invalid binding");
            if (!(binding.car() instanceof Val.Sym vs)) throw posError(form, "letrec: expected variable name");
            names.add(vs.name());
            letEnv.define(vs.name(), new Val.Void());
            if (!(binding.cdr() instanceof Val.PairV vp)) throw posError(form, "letrec: missing init");
            initExprs.add(vp.car());
            bindings = bp.cdr();
        }
        List<Val> body = collectList(lp.cdr());
        if (body.isEmpty()) throw posError(form, "letrec: missing body");
        if (names.isEmpty()) return evalBodyStep(body, letEnv, k);
        return new Step.Eval(initExprs.get(0), letEnv,
            new Kont.LetrecK(names, 0, initExprs, letEnv, body, k));
    }

    private Step evalCondStep(Val clauses, Env env, Kont k) throws EvalError {
        if (!(clauses instanceof Val.PairV cp)) return new Step.Apply(new Val.Void(), k);
        Val clause = cp.car();
        if (!(clause instanceof Val.PairV clausePair)) throw new EvalError("cond: invalid clause");
        if (clausePair.car() instanceof Val.Sym s && s.name().equals("else")) {
            return evalBeginStep(clausePair.cdr(), env, k);
        }
        return new Step.Eval(clausePair.car(), env,
            new Kont.CondK(clausePair.cdr(), cp.cdr(), env, k));
    }

    private Step evalAndStep(Val args, Env env, Kont k) {
        if (!(args instanceof Val.PairV p)) return new Step.Apply(new Val.Bool(true), k);
        if (!(p.cdr() instanceof Val.PairV)) return new Step.Eval(p.car(), env, k);
        return new Step.Eval(p.car(), env, new Kont.AndK(p.cdr(), env, k));
    }

    private Step evalOrStep(Val args, Env env, Kont k) {
        if (!(args instanceof Val.PairV p)) return new Step.Apply(new Val.Bool(false), k);
        if (!(p.cdr() instanceof Val.PairV)) return new Step.Eval(p.car(), env, k);
        return new Step.Eval(p.car(), env, new Kont.OrK(p.cdr(), env, k));
    }

    private Step evalDoStep(Val args, Env env, Kont k, Val form) throws EvalError {
        // Desugar do into named let
        if (!(args instanceof Val.PairV p1)) throw posError(form, "do: invalid syntax");
        List<String> varNames = new ArrayList<>();
        List<Val> initExprs = new ArrayList<>();
        List<Val> stepExprs = new ArrayList<>();
        Val varClauses = p1.car();
        while (varClauses instanceof Val.PairV vp) {
            if (!(vp.car() instanceof Val.PairV clause)) throw posError(form, "do: invalid var clause");
            if (!(clause.car() instanceof Val.Sym vs)) throw posError(form, "do: expected variable name");
            varNames.add(vs.name());
            if (!(clause.cdr() instanceof Val.PairV initPair)) throw posError(form, "do: missing init");
            initExprs.add(initPair.car());
            if (initPair.cdr() instanceof Val.PairV stepPair) stepExprs.add(stepPair.car());
            else stepExprs.add(new Val.Sym(vs.name()));
            varClauses = vp.cdr();
        }
        if (!(p1.cdr() instanceof Val.PairV p2)) throw posError(form, "do: missing test clause");
        Val testClause = p2.car();
        if (!(testClause instanceof Val.PairV testPair)) throw posError(form, "do: invalid test clause");
        Val testExpr = testPair.car();
        List<Val> resultExprs = collectList(testPair.cdr());
        List<Val> bodyExprs = collectList(p2.cdr());

        // Build loop call: (_loop step1 step2 ...)
        String loopName = gensym("do");
        Val loopSym = new Val.Sym(loopName);
        Val loopCall = new Val.Nil();
        for (int i = stepExprs.size() - 1; i >= 0; i--) loopCall = new Val.PairV(stepExprs.get(i), loopCall);
        loopCall = new Val.PairV(loopSym, loopCall);

        // Build else branch
        Val elseBranch;
        if (bodyExprs.isEmpty()) { elseBranch = loopCall; }
        else {
            Val elseBody = new Val.PairV(loopCall, new Val.Nil());
            for (int i = bodyExprs.size() - 1; i >= 0; i--) elseBody = new Val.PairV(bodyExprs.get(i), elseBody);
            elseBranch = new Val.PairV(new Val.Sym("begin"), elseBody);
        }

        // Build then branch
        Val thenBranch;
        if (resultExprs.isEmpty()) thenBranch = new Val.PairV(new Val.Sym("begin"), new Val.Nil());
        else if (resultExprs.size() == 1) thenBranch = resultExprs.get(0);
        else {
            Val tb = new Val.Nil();
            for (int i = resultExprs.size() - 1; i >= 0; i--) tb = new Val.PairV(resultExprs.get(i), tb);
            thenBranch = new Val.PairV(new Val.Sym("begin"), tb);
        }

        // Build if
        Val ifExpr = new Val.PairV(new Val.Sym("if"),
            new Val.PairV(testExpr, new Val.PairV(thenBranch, new Val.PairV(elseBranch, new Val.Nil()))));

        // Build named let
        Val bindingsList = new Val.Nil();
        for (int i = varNames.size() - 1; i >= 0; i--) {
            Val b = new Val.PairV(new Val.Sym(varNames.get(i)), new Val.PairV(initExprs.get(i), new Val.Nil()));
            bindingsList = new Val.PairV(b, bindingsList);
        }
        Val letExpr = new Val.PairV(new Val.Sym("let"),
            new Val.PairV(loopSym, new Val.PairV(bindingsList, new Val.PairV(ifExpr, new Val.Nil()))));

        return new Step.Eval(letExpr, env, k);
    }

    // ── Lambda / CaseLambda builders ────────────────────────────
    private Val buildLambda(Val args, Env env, Val form) throws EvalError {
        if (!(args instanceof Val.PairV p)) throw posError(form, "lambda requires arguments");
        List<String> params = new ArrayList<>();
        String restParam = null;
        Val paramList = p.car();
        while (paramList instanceof Val.PairV pp) {
            if (!(pp.car() instanceof Val.Sym ps)) throw posError(form, "lambda: expected parameter name");
            params.add(ps.name());
            paramList = pp.cdr();
        }
        if (paramList instanceof Val.Sym rs) restParam = rs.name();
        List<Val> body = collectList(p.cdr());
        if (body.isEmpty()) throw posError(form, "lambda: missing body");
        return new Val.Lambda(params, restParam, body, env);
    }

    private Val buildCaseLambda(Val args, Env env, Val form) throws EvalError {
        List<Val.Lambda> clauses = new ArrayList<>();
        Val cur = args;
        while (cur instanceof Val.PairV p) {
            Val clauseVal = p.car();
            if (!(clauseVal instanceof Val.PairV clause)) throw posError(form, "case-lambda: invalid clause");
            List<String> params = new ArrayList<>();
            String restParam = null;
            Val paramList = clause.car();
            while (paramList instanceof Val.PairV pp) {
                if (!(pp.car() instanceof Val.Sym ps)) throw posError(form, "case-lambda: expected parameter name");
                params.add(ps.name());
                paramList = pp.cdr();
            }
            if (paramList instanceof Val.Sym rs) restParam = rs.name();
            List<Val> body = collectList(clause.cdr());
            if (body.isEmpty()) throw posError(form, "case-lambda: clause missing body");
            clauses.add(new Val.Lambda(params, restParam, body, env));
            cur = p.cdr();
        }
        if (clauses.isEmpty()) throw posError(form, "case-lambda: no clauses");
        return new Val.CaseLambda(clauses);
    }

    private List<Val> collectList(Val v) {
        List<Val> result = new ArrayList<>();
        Val cur = v;
        while (cur instanceof Val.PairV p) { result.add(p.car()); cur = p.cdr(); }
        return result;
    }

    // ── Macros ──────────────────────────────────────────────────
    private static class MatchResult {
        final Map<String, Val> singles = new HashMap<>();
        final Map<String, List<Val>> ellipsis = new HashMap<>();
    }

    private Val evalDefineRecordType(Val args, Env env, Val form) throws EvalError {
        List<Val> parts = new ArrayList<>();
        Val cur = args;
        while (cur instanceof Val.PairV p) { parts.add(p.car()); cur = p.cdr(); }
        if (parts.size() < 3) throw posError(form, "define-record-type: invalid syntax");
        if (!(parts.get(0) instanceof Val.Sym typeSym)) throw posError(form, "define-record-type: expected type name");
        String typeName = typeSym.name();
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
        if (!(parts.get(2) instanceof Val.Sym predSym)) throw posError(form, "define-record-type: expected predicate name");
        String predName = predSym.name();
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
        Map<String, Integer> fieldIndex = new HashMap<>();
        for (int i = 0; i < fieldNames.length; i++) fieldIndex.put(fieldNames[i], i);
        int[] consFieldIdx = new int[consFields.length];
        for (int i = 0; i < consFields.length; i++) {
            Integer idx = fieldIndex.get(consFields[i]);
            if (idx == null) throw posError(form, "define-record-type: constructor field " + consFields[i] + " not in field specs");
            consFieldIdx[i] = idx;
        }
        final Object tag = new Object();
        final int fieldCount = fieldNames.length;
        env.define(consName, new Val.Builtin(consName, cargs -> {
            if (cargs.size() != consFields.length)
                throw new RuntimeException(consName + ": expected " + consFields.length + " arguments, got " + cargs.size());
            Val[] fields = new Val[fieldCount];
            for (int i = 0; i < consFields.length; i++) fields[consFieldIdx[i]] = cargs.get(i);
            return new Val.RecordInstance(tag, typeName, fieldNames, fields);
        }));
        env.define(predName, new Val.Builtin(predName, pargs -> {
            if (pargs.size() != 1) throw new RuntimeException(predName + ": expected 1 argument, got " + pargs.size());
            return new Val.Bool(pargs.get(0) instanceof Val.RecordInstance ri && ri.tag == tag);
        }));
        for (int i = 0; i < fieldNames.length; i++) {
            final int fi = i;
            final String accName = accessorNames[i];
            env.define(accName, new Val.Builtin(accName, aargs -> {
                if (aargs.size() != 1) throw new RuntimeException(accName + ": expected 1 argument, got " + aargs.size());
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

    private Val expandMacro(Val.Macro macro, Val.PairV form, Env env) throws EvalError {
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
                    } catch (EvalError e) { /* not in defEnv */ }
                }
                return expanded;
            }
        }
        throw posError(form, "no matching pattern for macro " + macro.name);
    }

    private boolean doMatch(Val pattern, Val input, List<String> literals, MatchResult result) {
        if (pattern instanceof Val.Sym sym) {
            String name = sym.name();
            if (name.equals("...")) return false;
            if (literals.contains(name)) return input instanceof Val.Sym is && is.name().equals(name);
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
                if (patElems.get(j) instanceof Val.Sym s && s.name().equals("...")) { ellipsisIdx = j; break; }
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
                    while (cur instanceof Val.PairV p) { matches.add(p.car()); cur = p.cdr(); }
                    if (!(cur instanceof Val.Nil)) return false;
                    result.ellipsis.put(sym.name(), matches);
                    return true;
                }
                return false;
            } else {
                Val patCur = pattern, inpCur = input;
                while (patCur instanceof Val.PairV patP) {
                    if (!(inpCur instanceof Val.PairV inpP)) return false;
                    if (!doMatch(patP.car(), inpP.car(), literals, result)) return false;
                    patCur = patP.cdr(); inpCur = inpP.cdr();
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
                            for (String ev : ellipsisVars) sub.singles.put(ev, bindings.ellipsis.get(ev).get(j));
                            expanded.add(expandTemplate(subTemplate, sub, renames));
                        }
                    }
                    i++;
                } else {
                    expanded.add(expandTemplate(elems.get(i), bindings, renames));
                }
            }
            Val result = new Val.Nil();
            for (int j = expanded.size() - 1; j >= 0; j--) result = new Val.PairV(expanded.get(j), result);
            return result;
        }
        return template;
    }

    private void collectIdentifiers(Val template, Set<String> ids) {
        if (template instanceof Val.Sym sym) ids.add(sym.name());
        else if (template instanceof Val.PairV pair) {
            collectIdentifiers(pair.car(), ids);
            collectIdentifiers(pair.cdr(), ids);
        }
    }

    private void collectEllipsisVars(Val template, MatchResult bindings, Set<String> vars) {
        if (template instanceof Val.Sym sym && bindings.ellipsis.containsKey(sym.name())) vars.add(sym.name());
        else if (template instanceof Val.PairV pair) {
            collectEllipsisVars(pair.car(), bindings, vars);
            collectEllipsisVars(pair.cdr(), bindings, vars);
        }
    }

    // ── Numeric helpers ─────────────────────────────────────────
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
    private static boolean isNum(Val v) { return v instanceof Val.Int || v instanceof Val.Rat || v instanceof Val.Flo; }
    private static Val numAdd(Val a, Val b) {
        if (a instanceof Val.Flo || b instanceof Val.Flo) return new Val.Flo(toDouble(a) + toDouble(b));
        long[] ar = toRat(a), br = toRat(b);
        return makeRat(ar[0] * br[1] + br[0] * ar[1], ar[1] * br[1]);
    }
    private static Val numSub(Val a, Val b) {
        if (a instanceof Val.Flo || b instanceof Val.Flo) return new Val.Flo(toDouble(a) - toDouble(b));
        long[] ar = toRat(a), br = toRat(b);
        return makeRat(ar[0] * br[1] - br[0] * ar[1], ar[1] * br[1]);
    }
    private static Val numMul(Val a, Val b) {
        if (a instanceof Val.Flo || b instanceof Val.Flo) return new Val.Flo(toDouble(a) * toDouble(b));
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
        if (a instanceof Val.Flo || b instanceof Val.Flo) return Double.compare(toDouble(a), toDouble(b));
        long[] ar = toRat(a), br = toRat(b);
        return Long.compare(ar[0] * br[1], br[0] * ar[1]);
    }
    private static long asInt(Val v) {
        if (v instanceof Val.Int i) return i.value();
        throw new RuntimeException("expected integer, got: " + writeVal(v));
    }
    private static double asDouble(Val v) {
        if (v instanceof Val.Int i) return (double) i.value();
        if (v instanceof Val.Flo f) return f.value();
        if (v instanceof Val.Rat r) return (double) r.num() / r.den();
        throw new RuntimeException("expected number, got: " + writeVal(v));
    }
    private static void checkArgCount(List<Val> args, int expected, String name) {
        if (args.size() != expected)
            throw new RuntimeException(name + " requires " + expected + " arguments, got " + args.size());
    }

    // ── Builtins ────────────────────────────────────────────────
    private Env createGlobalEnv() {
        Env env = new Env(null);
        // call/cc as first-class values
        env.define("call/cc", new Val.CallccVal());
        env.define("call-with-current-continuation", new Val.CallccVal());
        env.define("dynamic-wind", new Val.DynamicWindVal());
        // Arithmetic
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
        env.define("<", new Val.Builtin("<", args -> { checkArgCount(args, 2, "<"); return new Val.Bool(numCompare(args.get(0), args.get(1)) < 0); }));
        env.define(">", new Val.Builtin(">", args -> { checkArgCount(args, 2, ">"); return new Val.Bool(numCompare(args.get(0), args.get(1)) > 0); }));
        env.define("=", new Val.Builtin("=", args -> { checkArgCount(args, 2, "="); return new Val.Bool(numCompare(args.get(0), args.get(1)) == 0); }));
        env.define("<=", new Val.Builtin("<=", args -> { checkArgCount(args, 2, "<="); return new Val.Bool(numCompare(args.get(0), args.get(1)) <= 0); }));
        env.define(">=", new Val.Builtin(">=", args -> { checkArgCount(args, 2, ">="); return new Val.Bool(numCompare(args.get(0), args.get(1)) >= 0); }));
        env.define("not", new Val.Builtin("not", args -> { checkArgCount(args, 1, "not"); return new Val.Bool(!isTruthy(args.get(0))); }));
        // Pairs & lists
        env.define("cons", new Val.Builtin("cons", args -> { checkArgCount(args, 2, "cons"); return new Val.PairV(args.get(0), args.get(1)); }));
        env.define("car", new Val.Builtin("car", args -> { checkArgCount(args, 1, "car"); if (!(args.get(0) instanceof Val.PairV p)) throw new RuntimeException("car: not a pair"); return p.car(); }));
        env.define("cdr", new Val.Builtin("cdr", args -> { checkArgCount(args, 1, "cdr"); if (!(args.get(0) instanceof Val.PairV p)) throw new RuntimeException("cdr: not a pair"); return p.cdr(); }));
        env.define("set-car!", new Val.Builtin("set-car!", args -> { checkArgCount(args, 2, "set-car!"); if (!(args.get(0) instanceof Val.PairV p)) throw new RuntimeException("set-car!: not a pair"); p.car = args.get(1); return new Val.Void(); }));
        env.define("set-cdr!", new Val.Builtin("set-cdr!", args -> { checkArgCount(args, 2, "set-cdr!"); if (!(args.get(0) instanceof Val.PairV p)) throw new RuntimeException("set-cdr!: not a pair"); p.cdr = args.get(1); return new Val.Void(); }));
        env.define("caar", new Val.Builtin("caar", args -> { checkArgCount(args, 1, "caar"); if (!(args.get(0) instanceof Val.PairV p) || !(p.car() instanceof Val.PairV p2)) throw new RuntimeException("caar: not valid"); return p2.car(); }));
        env.define("cadr", new Val.Builtin("cadr", args -> { checkArgCount(args, 1, "cadr"); if (!(args.get(0) instanceof Val.PairV p) || !(p.cdr() instanceof Val.PairV p2)) throw new RuntimeException("cadr: not valid"); return p2.car(); }));
        env.define("cdar", new Val.Builtin("cdar", args -> { checkArgCount(args, 1, "cdar"); if (!(args.get(0) instanceof Val.PairV p) || !(p.car() instanceof Val.PairV p2)) throw new RuntimeException("cdar: not valid"); return p2.cdr(); }));
        env.define("cddr", new Val.Builtin("cddr", args -> { checkArgCount(args, 1, "cddr"); if (!(args.get(0) instanceof Val.PairV p) || !(p.cdr() instanceof Val.PairV p2)) throw new RuntimeException("cddr: not valid"); return p2.cdr(); }));
        env.define("reverse", new Val.Builtin("reverse", args -> {
            checkArgCount(args, 1, "reverse");
            Val result = new Val.Nil(); Val cur = args.get(0);
            while (cur instanceof Val.PairV p) { result = new Val.PairV(p.car(), result); cur = p.cdr(); }
            return result;
        }));
        env.define("assv", new Val.Builtin("assv", args -> {
            checkArgCount(args, 2, "assv");
            Val key = args.get(0), lst = args.get(1);
            while (lst instanceof Val.PairV p) { if (p.car() instanceof Val.PairV entry && isEq(entry.car(), key)) return entry; lst = p.cdr(); }
            return new Val.Bool(false);
        }));
        env.define("member", new Val.Builtin("member", args -> {
            checkArgCount(args, 2, "member");
            Val key = args.get(0), lst = args.get(1);
            while (lst instanceof Val.PairV p) { if (isEqual(p.car(), key)) return p; lst = p.cdr(); }
            return new Val.Bool(false);
        }));
        env.define("null?", new Val.Builtin("null?", args -> { checkArgCount(args, 1, "null?"); return new Val.Bool(args.get(0) instanceof Val.Nil); }));
        env.define("procedure?", new Val.Builtin("procedure?", args -> {
            checkArgCount(args, 1, "procedure?");
            Val v = args.get(0);
            return new Val.Bool(v instanceof Val.Lambda || v instanceof Val.CaseLambda || v instanceof Val.Builtin || v instanceof Val.ContVal || v instanceof Val.CallccVal || v instanceof Val.DynamicWindVal);
        }));
        env.define("list", new Val.Builtin("list", args -> {
            Val result = new Val.Nil();
            for (int i = args.size() - 1; i >= 0; i--) result = new Val.PairV(args.get(i), result);
            return result;
        }));
        env.define("length", new Val.Builtin("length", args -> {
            checkArgCount(args, 1, "length");
            long count = 0; Val cur = args.get(0);
            while (cur instanceof Val.PairV p) { count++; cur = p.cdr(); }
            return new Val.Int(count);
        }));
        env.define("append", new Val.Builtin("append", args -> {
            if (args.isEmpty()) return new Val.Nil();
            Val result = args.get(args.size() - 1);
            for (int i = args.size() - 2; i >= 0; i--) {
                Val lst = args.get(i);
                List<Val> elems = new ArrayList<>();
                Val cur = lst;
                while (cur instanceof Val.PairV p) { elems.add(p.car()); cur = p.cdr(); }
                for (int j = elems.size() - 1; j >= 0; j--) result = new Val.PairV(elems.get(j), result);
            }
            return result;
        }));
        // Type predicates
        env.define("number?", new Val.Builtin("number?", args -> { checkArgCount(args, 1, "number?"); return new Val.Bool(isNum(args.get(0))); }));
        env.define("integer?", new Val.Builtin("integer?", args -> { checkArgCount(args, 1, "integer?"); return new Val.Bool(args.get(0) instanceof Val.Int); }));
        env.define("rational?", new Val.Builtin("rational?", args -> { checkArgCount(args, 1, "rational?"); Val v = args.get(0); return new Val.Bool(v instanceof Val.Int || v instanceof Val.Rat); }));
        env.define("exact?", new Val.Builtin("exact?", args -> { checkArgCount(args, 1, "exact?"); Val v = args.get(0); return new Val.Bool(v instanceof Val.Int || v instanceof Val.Rat); }));
        env.define("inexact?", new Val.Builtin("inexact?", args -> { checkArgCount(args, 1, "inexact?"); return new Val.Bool(args.get(0) instanceof Val.Flo); }));
        env.define("exact->inexact", new Val.Builtin("exact->inexact", args -> { checkArgCount(args, 1, "exact->inexact"); return new Val.Flo(toDouble(args.get(0))); }));
        env.define("inexact->exact", new Val.Builtin("inexact->exact", args -> {
            checkArgCount(args, 1, "inexact->exact");
            Val v = args.get(0);
            if (v instanceof Val.Int || v instanceof Val.Rat) return v;
            if (v instanceof Val.Flo f) {
                double d = f.value();
                if (d == Math.floor(d) && !Double.isInfinite(d)) return new Val.Int((long) d);
                long bits = Double.doubleToLongBits(d);
                long sign = (bits >> 63) == 0 ? 1 : -1;
                int exp = (int)((bits >> 52) & 0x7ffL) - 1023;
                long mantissa = (bits & 0x000fffffffffffffL) | 0x0010000000000000L;
                int shift = exp - 52;
                if (shift >= 0) return new Val.Int(sign * mantissa * (1L << shift));
                else return makeRat(sign * mantissa, 1L << (-shift));
            }
            throw new RuntimeException("inexact->exact: not a number");
        }));
        env.define("numerator", new Val.Builtin("numerator", args -> { checkArgCount(args, 1, "numerator"); Val v = args.get(0); if (v instanceof Val.Int i) return new Val.Int(i.value()); if (v instanceof Val.Rat r) return new Val.Int(r.num()); throw new RuntimeException("numerator: not a rational number"); }));
        env.define("denominator", new Val.Builtin("denominator", args -> { checkArgCount(args, 1, "denominator"); Val v = args.get(0); if (v instanceof Val.Int) return new Val.Int(1); if (v instanceof Val.Rat r) return new Val.Int(r.den()); throw new RuntimeException("denominator: not a rational number"); }));
        env.define("string?", new Val.Builtin("string?", args -> { checkArgCount(args, 1, "string?"); return new Val.Bool(args.get(0) instanceof Val.Str); }));
        env.define("boolean?", new Val.Builtin("boolean?", args -> { checkArgCount(args, 1, "boolean?"); return new Val.Bool(args.get(0) instanceof Val.Bool); }));
        env.define("pair?", new Val.Builtin("pair?", args -> { checkArgCount(args, 1, "pair?"); return new Val.Bool(args.get(0) instanceof Val.PairV); }));
        env.define("symbol?", new Val.Builtin("symbol?", args -> { checkArgCount(args, 1, "symbol?"); return new Val.Bool(args.get(0) instanceof Val.Sym); }));
        // I/O
        env.define("display", new Val.Builtin("display", args -> { checkArgCount(args, 1, "display"); output.append(displayVal(args.get(0))); return new Val.Void(); }));
        env.define("write", new Val.Builtin("write", args -> { checkArgCount(args, 1, "write"); output.append(writeVal(args.get(0))); return new Val.Void(); }));
        env.define("newline", new Val.Builtin("newline", args -> { checkArgCount(args, 0, "newline"); output.append("\n"); return new Val.Void(); }));
        // Strings
        env.define("string-append", new Val.Builtin("string-append", args -> { StringBuilder sb = new StringBuilder(); for (Val a : args) { if (!(a instanceof Val.Str s)) throw new RuntimeException("string-append: not a string: " + writeVal(a)); sb.append(s.value()); } return new Val.Str(sb.toString()); }));
        env.define("string-length", new Val.Builtin("string-length", args -> { checkArgCount(args, 1, "string-length"); if (!(args.get(0) instanceof Val.Str s)) throw new RuntimeException("string-length: not a string"); return new Val.Int(s.value().length()); }));
        env.define("substring", new Val.Builtin("substring", args -> { checkArgCount(args, 3, "substring"); if (!(args.get(0) instanceof Val.Str s)) throw new RuntimeException("substring: not a string"); return new Val.Str(s.value().substring((int) asInt(args.get(1)), (int) asInt(args.get(2)))); }));
        env.define("string->number", new Val.Builtin("string->number", args -> { checkArgCount(args, 1, "string->number"); if (!(args.get(0) instanceof Val.Str s)) throw new RuntimeException("string->number: not a string"); try { return new Val.Int(Long.parseLong(s.value())); } catch (NumberFormatException e) { return new Val.Bool(false); } }));
        env.define("number->string", new Val.Builtin("number->string", args -> { checkArgCount(args, 1, "number->string"); return new Val.Str(String.valueOf(asInt(args.get(0)))); }));
        env.define("symbol->string", new Val.Builtin("symbol->string", args -> { checkArgCount(args, 1, "symbol->string"); if (!(args.get(0) instanceof Val.Sym s)) throw new RuntimeException("symbol->string: not a symbol"); return new Val.Str(s.name()); }));
        env.define("string->symbol", new Val.Builtin("string->symbol", args -> { checkArgCount(args, 1, "string->symbol"); if (!(args.get(0) instanceof Val.Str s)) throw new RuntimeException("string->symbol: not a string"); return new Val.Sym(s.value()); }));
        env.define("string-ref", new Val.Builtin("string-ref", args -> { checkArgCount(args, 2, "string-ref"); if (!(args.get(0) instanceof Val.Str s)) throw new RuntimeException("string-ref: not a string"); return new Val.Chr(s.value().charAt((int) asInt(args.get(1)))); }));
        env.define("char?", new Val.Builtin("char?", args -> { checkArgCount(args, 1, "char?"); return new Val.Bool(args.get(0) instanceof Val.Chr); }));
        env.define("string-copy", new Val.Builtin("string-copy", args -> { checkArgCount(args, 1, "string-copy"); if (!(args.get(0) instanceof Val.Str s)) throw new RuntimeException("string-copy: not a string"); return new Val.Str(s.value()); }));
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
        env.define("string->list", new Val.Builtin("string->list", args -> {
            checkArgCount(args, 1, "string->list");
            if (!(args.get(0) instanceof Val.Str s)) throw new RuntimeException("string->list: not a string");
            Val result = new Val.Nil(); String v = s.value();
            for (int i = v.length() - 1; i >= 0; i--) result = new Val.PairV(new Val.Chr(v.charAt(i)), result);
            return result;
        }));
        env.define("list->string", new Val.Builtin("list->string", args -> {
            checkArgCount(args, 1, "list->string");
            StringBuilder sb = new StringBuilder(); Val cur = args.get(0);
            while (cur instanceof Val.PairV p) { if (!(p.car() instanceof Val.Chr c)) throw new RuntimeException("list->string: not a character"); sb.append(c.value()); cur = p.cdr(); }
            return new Val.Str(sb.toString());
        }));
        env.define("char->integer", new Val.Builtin("char->integer", args -> { checkArgCount(args, 1, "char->integer"); if (!(args.get(0) instanceof Val.Chr c)) throw new RuntimeException("char->integer: not a character"); return new Val.Int((long) c.value()); }));
        env.define("integer->char", new Val.Builtin("integer->char", args -> { checkArgCount(args, 1, "integer->char"); return new Val.Chr((char) asInt(args.get(0))); }));
        // Numeric utilities
        env.define("abs", new Val.Builtin("abs", args -> { checkArgCount(args, 1, "abs"); return new Val.Int(Math.abs(asInt(args.get(0)))); }));
        env.define("modulo", new Val.Builtin("modulo", args -> { checkArgCount(args, 2, "modulo"); long a = asInt(args.get(0)), b = asInt(args.get(1)); if (b == 0) throw new RuntimeException("modulo: division by zero"); return new Val.Int(Math.floorMod(a, b)); }));
        env.define("remainder", new Val.Builtin("remainder", args -> { checkArgCount(args, 2, "remainder"); long a = asInt(args.get(0)), b = asInt(args.get(1)); if (b == 0) throw new RuntimeException("remainder: division by zero"); return new Val.Int(a % b); }));
        env.define("quotient", new Val.Builtin("quotient", args -> { checkArgCount(args, 2, "quotient"); long a = asInt(args.get(0)), b = asInt(args.get(1)); if (b == 0) throw new RuntimeException("quotient: division by zero"); return new Val.Int(a / b); }));
        env.define("min", new Val.Builtin("min", args -> { if (args.isEmpty()) throw new RuntimeException("min requires at least 1 argument"); long r = asInt(args.get(0)); for (int i = 1; i < args.size(); i++) r = Math.min(r, asInt(args.get(i))); return new Val.Int(r); }));
        env.define("max", new Val.Builtin("max", args -> { if (args.isEmpty()) throw new RuntimeException("max requires at least 1 argument"); long r = asInt(args.get(0)); for (int i = 1; i < args.size(); i++) r = Math.max(r, asInt(args.get(i))); return new Val.Int(r); }));
        env.define("expt", new Val.Builtin("expt", args -> { checkArgCount(args, 2, "expt"); long base = asInt(args.get(0)), exp = asInt(args.get(1)); long r = 1; boolean neg = exp < 0; long e = Math.abs(exp); for (long i = 0; i < e; i++) r *= base; if (neg) return new Val.Int(0); return new Val.Int(r); }));
        env.define("zero?", new Val.Builtin("zero?", args -> { checkArgCount(args, 1, "zero?"); return new Val.Bool(asInt(args.get(0)) == 0); }));
        env.define("positive?", new Val.Builtin("positive?", args -> { checkArgCount(args, 1, "positive?"); return new Val.Bool(asInt(args.get(0)) > 0); }));
        env.define("negative?", new Val.Builtin("negative?", args -> { checkArgCount(args, 1, "negative?"); return new Val.Bool(asInt(args.get(0)) < 0); }));
        env.define("odd?", new Val.Builtin("odd?", args -> { checkArgCount(args, 1, "odd?"); return new Val.Bool(asInt(args.get(0)) % 2 != 0); }));
        env.define("even?", new Val.Builtin("even?", args -> { checkArgCount(args, 1, "even?"); return new Val.Bool(asInt(args.get(0)) % 2 == 0); }));
        env.define("gcd", new Val.Builtin("gcd", args -> { if (args.isEmpty()) return new Val.Int(0); long r = Math.abs(asInt(args.get(0))); for (int i = 1; i < args.size(); i++) { long b = Math.abs(asInt(args.get(i))); while (b != 0) { long t = b; b = r % b; r = t; } } return new Val.Int(r); }));
        env.define("lcm", new Val.Builtin("lcm", args -> { if (args.isEmpty()) return new Val.Int(1); long r = Math.abs(asInt(args.get(0))); for (int i = 1; i < args.size(); i++) { long b = Math.abs(asInt(args.get(i))); if (r == 0 && b == 0) { r = 0; continue; } long g = r; long t = b; while (t != 0) { long tmp = t; t = g % t; g = tmp; } r = r / g * b; } return new Val.Int(r); }));
        env.define("floor", new Val.Builtin("floor", args -> { checkArgCount(args, 1, "floor"); Val v = args.get(0); if (v instanceof Val.Int) return v; if (v instanceof Val.Flo f) return new Val.Int((long) Math.floor(f.value())); if (v instanceof Val.Rat r) { long q = r.num() / r.den(); if (r.num() < 0 && r.num() % r.den() != 0) q--; return new Val.Int(q); } throw new RuntimeException("floor: not a number"); }));
        env.define("ceiling", new Val.Builtin("ceiling", args -> { checkArgCount(args, 1, "ceiling"); Val v = args.get(0); if (v instanceof Val.Int) return v; if (v instanceof Val.Flo f) return new Val.Int((long) Math.ceil(f.value())); if (v instanceof Val.Rat r) { long q = r.num() / r.den(); if (r.num() > 0 && r.num() % r.den() != 0) q++; return new Val.Int(q); } throw new RuntimeException("ceiling: not a number"); }));
        env.define("truncate", new Val.Builtin("truncate", args -> { checkArgCount(args, 1, "truncate"); Val v = args.get(0); if (v instanceof Val.Int) return v; if (v instanceof Val.Flo f) return new Val.Int((long) f.value()); if (v instanceof Val.Rat r) return new Val.Int(r.num() / r.den()); throw new RuntimeException("truncate: not a number"); }));
        env.define("round", new Val.Builtin("round", args -> { checkArgCount(args, 1, "round"); Val v = args.get(0); if (v instanceof Val.Int) return v; if (v instanceof Val.Flo f) return new Val.Int(Math.round(f.value())); if (v instanceof Val.Rat r) { double d = (double) r.num() / r.den(); return new Val.Int(Math.round(d)); } throw new RuntimeException("round: not a number"); }));
        env.define("sqrt", new Val.Builtin("sqrt", args -> { checkArgCount(args, 1, "sqrt"); double d = asDouble(args.get(0)); double r = Math.sqrt(d); if (r == Math.floor(r) && !Double.isInfinite(r)) return new Val.Int((long) r); return new Val.Flo(r); }));
        env.define("make-string", new Val.Builtin("make-string", args -> { if (args.size() < 1 || args.size() > 2) throw new RuntimeException("make-string: 1-2 args"); int n = (int) asInt(args.get(0)); char c = args.size() == 2 ? ((Val.Chr) args.get(1)).value() : '\0'; char[] chars = new char[n]; java.util.Arrays.fill(chars, c); return new Val.Str(new String(chars)); }));
        // List utilities
        env.define("list-ref", new Val.Builtin("list-ref", args -> { checkArgCount(args, 2, "list-ref"); Val cur = args.get(0); long idx = asInt(args.get(1)); for (long i = 0; i < idx; i++) { if (!(cur instanceof Val.PairV p)) throw new RuntimeException("list-ref: index out of range"); cur = p.cdr(); } if (!(cur instanceof Val.PairV p)) throw new RuntimeException("list-ref: index out of range"); return p.car(); }));
        env.define("list-tail", new Val.Builtin("list-tail", args -> { checkArgCount(args, 2, "list-tail"); Val cur = args.get(0); long idx = asInt(args.get(1)); for (long i = 0; i < idx; i++) { if (!(cur instanceof Val.PairV p)) throw new RuntimeException("list-tail: index out of range"); cur = p.cdr(); } return cur; }));
        env.define("list?", new Val.Builtin("list?", args -> { checkArgCount(args, 1, "list?"); Val slow = args.get(0), fast = args.get(0); while (true) { if (!(fast instanceof Val.PairV fp)) return new Val.Bool(fast instanceof Val.Nil); fast = fp.cdr(); if (!(fast instanceof Val.PairV fp2)) return new Val.Bool(fast instanceof Val.Nil); fast = fp2.cdr(); slow = ((Val.PairV) slow).cdr(); if (slow == fast) return new Val.Bool(false); } }));
        // Equality
        env.define("eq?", new Val.Builtin("eq?", args -> { checkArgCount(args, 2, "eq?"); return new Val.Bool(isEq(args.get(0), args.get(1))); }));
        env.define("equal?", new Val.Builtin("equal?", args -> { checkArgCount(args, 2, "equal?"); return new Val.Bool(isEqual(args.get(0), args.get(1))); }));
        env.define("eqv?", new Val.Builtin("eqv?", args -> { checkArgCount(args, 2, "eqv?"); return new Val.Bool(isEq(args.get(0), args.get(1))); }));
        env.define("assoc", new Val.Builtin("assoc", args -> { checkArgCount(args, 2, "assoc"); Val key = args.get(0), lst = args.get(1); while (lst instanceof Val.PairV p) { if (p.car() instanceof Val.PairV entry && isEqual(entry.car(), key)) return entry; lst = p.cdr(); } return new Val.Bool(false); }));
        // Higher-order
        env.define("map", new Val.Builtin("map", args -> {
            if (args.size() < 2) throw new RuntimeException("map requires at least 2 arguments");
            Val fn = args.get(0); int numLists = args.size() - 1;
            Val[] cursors = new Val[numLists];
            for (int i = 0; i < numLists; i++) cursors[i] = args.get(i + 1);
            List<Val> results = new ArrayList<>();
            while (true) {
                boolean done = false;
                for (Val c : cursors) { if (!(c instanceof Val.PairV)) { done = true; break; } }
                if (done) break;
                List<Val> callArgs = new ArrayList<>();
                for (int i = 0; i < numLists; i++) { callArgs.add(((Val.PairV) cursors[i]).car()); cursors[i] = ((Val.PairV) cursors[i]).cdr(); }
                try { results.add(applyFn(fn, callArgs, null)); }
                catch (EvalError e) { throw new RuntimeException(e.getMessage()); }
            }
            Val result = new Val.Nil();
            for (int i = results.size() - 1; i >= 0; i--) result = new Val.PairV(results.get(i), result);
            return result;
        }));
        env.define("for-each", new Val.Builtin("for-each", args -> {
            if (args.size() < 2) throw new RuntimeException("for-each requires at least 2 arguments");
            Val fn = args.get(0); int numLists = args.size() - 1;
            Val[] cursors = new Val[numLists];
            for (int i = 0; i < numLists; i++) cursors[i] = args.get(i + 1);
            while (true) {
                boolean done = false;
                for (Val c : cursors) { if (!(c instanceof Val.PairV)) { done = true; break; } }
                if (done) break;
                List<Val> callArgs = new ArrayList<>();
                for (int i = 0; i < numLists; i++) { callArgs.add(((Val.PairV) cursors[i]).car()); cursors[i] = ((Val.PairV) cursors[i]).cdr(); }
                try { applyFn(fn, callArgs, null); }
                catch (EvalError e) { throw new RuntimeException(e.getMessage()); }
            }
            return new Val.Void();
        }));
        // Char utilities
        env.define("char-alphabetic?", new Val.Builtin("char-alphabetic?", args -> { checkArgCount(args, 1, "char-alphabetic?"); if (!(args.get(0) instanceof Val.Chr c)) throw new RuntimeException("not a character"); return new Val.Bool(Character.isLetter(c.value())); }));
        env.define("char-numeric?", new Val.Builtin("char-numeric?", args -> { checkArgCount(args, 1, "char-numeric?"); if (!(args.get(0) instanceof Val.Chr c)) throw new RuntimeException("not a character"); return new Val.Bool(Character.isDigit(c.value())); }));
        env.define("char-upcase", new Val.Builtin("char-upcase", args -> { checkArgCount(args, 1, "char-upcase"); if (!(args.get(0) instanceof Val.Chr c)) throw new RuntimeException("not a character"); return new Val.Chr(Character.toUpperCase(c.value())); }));
        env.define("char-downcase", new Val.Builtin("char-downcase", args -> { checkArgCount(args, 1, "char-downcase"); if (!(args.get(0) instanceof Val.Chr c)) throw new RuntimeException("not a character"); return new Val.Chr(Character.toLowerCase(c.value())); }));
        env.define("char=?", new Val.Builtin("char=?", args -> { checkArgCount(args, 2, "char=?"); if (!(args.get(0) instanceof Val.Chr a) || !(args.get(1) instanceof Val.Chr b)) throw new RuntimeException("not characters"); return new Val.Bool(a.value() == b.value()); }));
        env.define("char<?", new Val.Builtin("char<?", args -> { checkArgCount(args, 2, "char<?"); if (!(args.get(0) instanceof Val.Chr a) || !(args.get(1) instanceof Val.Chr b)) throw new RuntimeException("not characters"); return new Val.Bool(a.value() < b.value()); }));
        // String comparison/case
        env.define("string=?", new Val.Builtin("string=?", args -> { checkArgCount(args, 2, "string=?"); if (!(args.get(0) instanceof Val.Str a) || !(args.get(1) instanceof Val.Str b)) throw new RuntimeException("not strings"); return new Val.Bool(a.value().equals(b.value())); }));
        env.define("string<?", new Val.Builtin("string<?", args -> { checkArgCount(args, 2, "string<?"); if (!(args.get(0) instanceof Val.Str a) || !(args.get(1) instanceof Val.Str b)) throw new RuntimeException("not strings"); return new Val.Bool(a.value().compareTo(b.value()) < 0); }));
        env.define("string>?", new Val.Builtin("string>?", args -> { checkArgCount(args, 2, "string>?"); if (!(args.get(0) instanceof Val.Str a) || !(args.get(1) instanceof Val.Str b)) throw new RuntimeException("not strings"); return new Val.Bool(a.value().compareTo(b.value()) > 0); }));
        env.define("string<=?", new Val.Builtin("string<=?", args -> { checkArgCount(args, 2, "string<=?"); if (!(args.get(0) instanceof Val.Str a) || !(args.get(1) instanceof Val.Str b)) throw new RuntimeException("not strings"); return new Val.Bool(a.value().compareTo(b.value()) <= 0); }));
        env.define("string>=?", new Val.Builtin("string>=?", args -> { checkArgCount(args, 2, "string>=?"); if (!(args.get(0) instanceof Val.Str a) || !(args.get(1) instanceof Val.Str b)) throw new RuntimeException("not strings"); return new Val.Bool(a.value().compareTo(b.value()) >= 0); }));
        env.define("string", new Val.Builtin("string", args -> { char[] chars = new char[args.size()]; for (int i = 0; i < args.size(); i++) { if (!(args.get(i) instanceof Val.Chr c)) throw new RuntimeException("string: not a character"); chars[i] = c.value(); } return new Val.Str(new String(chars)); }));
        env.define("string-ci=?", new Val.Builtin("string-ci=?", args -> { checkArgCount(args, 2, "string-ci=?"); if (!(args.get(0) instanceof Val.Str a) || !(args.get(1) instanceof Val.Str b)) throw new RuntimeException("not strings"); return new Val.Bool(a.value().equalsIgnoreCase(b.value())); }));
        env.define("string-upcase", new Val.Builtin("string-upcase", args -> { checkArgCount(args, 1, "string-upcase"); if (!(args.get(0) instanceof Val.Str s)) throw new RuntimeException("not a string"); return new Val.Str(s.value().toUpperCase()); }));
        env.define("string-downcase", new Val.Builtin("string-downcase", args -> { checkArgCount(args, 1, "string-downcase"); if (!(args.get(0) instanceof Val.Str s)) throw new RuntimeException("not a string"); return new Val.Str(s.value().toLowerCase()); }));
        // Vectors
        env.define("vector", new Val.Builtin("vector", args -> new Val.Vec(args.toArray(new Val[0]))));
        env.define("make-vector", new Val.Builtin("make-vector", args -> { if (args.size() < 1 || args.size() > 2) throw new RuntimeException("make-vector requires 1-2 arguments"); int len = (int) asInt(args.get(0)); Val fill = args.size() > 1 ? args.get(1) : new Val.Int(0); Val[] elems = new Val[len]; for (int i = 0; i < len; i++) elems[i] = fill; return new Val.Vec(elems); }));
        env.define("vector-ref", new Val.Builtin("vector-ref", args -> { checkArgCount(args, 2, "vector-ref"); if (!(args.get(0) instanceof Val.Vec v)) throw new RuntimeException("vector-ref: not a vector"); return v.elements[(int) asInt(args.get(1))]; }));
        env.define("vector-set!", new Val.Builtin("vector-set!", args -> { checkArgCount(args, 3, "vector-set!"); if (!(args.get(0) instanceof Val.Vec v)) throw new RuntimeException("vector-set!: not a vector"); v.elements[(int) asInt(args.get(1))] = args.get(2); return new Val.Void(); }));
        env.define("vector-length", new Val.Builtin("vector-length", args -> { checkArgCount(args, 1, "vector-length"); if (!(args.get(0) instanceof Val.Vec v)) throw new RuntimeException("vector-length: not a vector"); return new Val.Int(v.elements.length); }));
        env.define("vector?", new Val.Builtin("vector?", args -> { checkArgCount(args, 1, "vector?"); return new Val.Bool(args.get(0) instanceof Val.Vec); }));
        env.define("vector->list", new Val.Builtin("vector->list", args -> { checkArgCount(args, 1, "vector->list"); if (!(args.get(0) instanceof Val.Vec v)) throw new RuntimeException("vector->list: not a vector"); Val r = new Val.Nil(); for (int i = v.elements.length - 1; i >= 0; i--) r = new Val.PairV(v.elements[i], r); return r; }));
        env.define("list->vector", new Val.Builtin("list->vector", args -> { checkArgCount(args, 1, "list->vector"); List<Val> elems = new ArrayList<>(); Val cur = args.get(0); while (cur instanceof Val.PairV p) { elems.add(p.car()); cur = p.cdr(); } return new Val.Vec(elems.toArray(new Val[0])); }));
        // Error
        env.define("error", new Val.Builtin("error", args -> { if (args.isEmpty()) throw new RuntimeException("error"); StringBuilder sb = new StringBuilder(); if (args.size() > 1) { for (int i = 1; i < args.size(); i++) sb.append(displayVal(args.get(i))); } else sb.append(displayVal(args.get(0))); throw new RuntimeException(sb.toString()); }));
        // Apply
        env.define("apply", new Val.Builtin("apply", args -> {
            if (args.size() < 2) throw new RuntimeException("apply requires at least 2 arguments");
            Val fn = args.get(0); Val lastArg = args.get(args.size() - 1);
            List<Val> callArgs = new ArrayList<>();
            for (int i = 1; i < args.size() - 1; i++) callArgs.add(args.get(i));
            Val cur = lastArg;
            while (cur instanceof Val.PairV p) { callArgs.add(p.car()); cur = p.cdr(); }
            try { return applyFn(fn, callArgs, null); }
            catch (EvalError e) { throw new RuntimeException(e.getMessage()); }
        }));
        return env;
    }

    // ── Public API ──────────────────────────────────────────────
    public String evalStr(String input) throws EvalError {
        positions.clear();
        output = new StringBuilder();
        List<Token> tokens = tokenize(input);
        if (tokens.isEmpty()) throw new EvalError("empty input");
        int[] idx = {0};
        Env env = createGlobalEnv();
        List<Val> exprs = new ArrayList<>();
        while (idx[0] < tokens.size()) exprs.add(parse(tokens, idx));
        Val result = run(evalBodyStep(exprs, env, new Kont.HaltK()));
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
        List<Val> exprs = new ArrayList<>();
        while (idx[0] < tokens.size()) exprs.add(parse(tokens, idx));
        Val result = run(evalBodyStep(exprs, env, new Kont.HaltK()));
        String resultStr = (result instanceof Val.Void) ? "#<void>" : writeVal(result);
        return new EvalResult(resultStr, output.toString());
    }
}
