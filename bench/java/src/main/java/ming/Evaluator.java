package ming;

import java.util.ArrayList;
import java.util.ArrayDeque;
import java.util.Collections;
import java.util.Deque;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.concurrent.atomic.AtomicLong;

/**
 * Scheme interpreter — CEK machine with first-class continuations.
 */
public class Evaluator {

    public String evalStr(String input) throws EvalError {
        List<Object> exprs = Parser.parse(input);
        Env env = Env.global();
        WIND_STACK.get().clear();
        Object result = runCEK(exprs, env);
        return SchemeValue.toStr(result);
    }

    public EvalResult evalStrWithOutput(String input) throws EvalError {
        List<Object> exprs = Parser.parse(input);
        Env env = Env.global();
        StringBuilder outputBuf = new StringBuilder();
        OUTPUT.set(outputBuf);
        WIND_STACK.get().clear();
        try {
            Object result = runCEK(exprs, env);
            return new EvalResult(SchemeValue.toStr(result), outputBuf.toString());
        } finally {
            OUTPUT.remove();
            WIND_STACK.get().clear();
        }
    }

    public String evalStrWithLimit(String input, int maxSteps) throws EvalError {
        List<Object> exprs = Parser.parse(input);
        Env env = Env.global();
        WIND_STACK.get().clear();
        STEP_LIMIT.set(new long[]{maxSteps});
        try {
            Object result = runCEK(exprs, env);
            return SchemeValue.toStr(result);
        } finally {
            STEP_LIMIT.remove();
        }
    }

    static final ThreadLocal<StringBuilder> OUTPUT = new ThreadLocal<>();

    static void emitOutput(String s) {
        StringBuilder buf = OUTPUT.get();
        if (buf != null) buf.append(s);
    }

    // ===== Call/cc support =====

    static final Object CALLCC = new Object() {
        @Override public String toString() { return "#<call/cc>"; }
    };

    static final class ContinuationObj {
        final Kont kont;
        final List<DynamicWindEntry> windStack;
        ContinuationObj(Kont kont, List<DynamicWindEntry> windStack) {
            this.kont = kont;
            this.windStack = windStack;
        }
    }

    static final class ContinuationReturn extends RuntimeException {
        final Object value;
        final Kont kont;
        ContinuationReturn(Object value, Kont kont) {
            super(null, null, true, false);
            this.value = value;
            this.kont = kont;
        }
    }

    // ===== Exception handling (raise/guard/with-exception-handler) =====

    static final class SchemeException extends RuntimeException {
        final Object value;
        SchemeException(Object value) {
            super(null, null, true, false);
            this.value = value;
        }
    }

    // ===== dynamic-wind support =====

    static final Object DYNAMIC_WIND = new Object() {
        @Override public String toString() { return "#<dynamic-wind>"; }
    };

    // ===== values & call-with-values support =====

    static final Object CALL_WITH_VALUES = new Object() {
        @Override public String toString() { return "#<call-with-values>"; }
    };

    record MultipleValues(List<Object> values) {}

    record DynamicWindEntry(Object inThunk, Object outThunk) {}

    static final ThreadLocal<List<DynamicWindEntry>> WIND_STACK = ThreadLocal.withInitial(ArrayList::new);

    // Step-limited evaluation (L27)
    static final ThreadLocal<long[]> STEP_LIMIT = ThreadLocal.withInitial(() -> null);

    // ===== syntax-case support =====

    record SyntaxResult(Object form, Map<String, String> renames, Env defEnv) {}

    static class SyntaxCaseContext {
        final Map<String, Object> bindings;
        final Map<String, List<Object>> ellipsisBindings;
        final Env defEnv;
        SyntaxCaseContext(Map<String, Object> bindings, Map<String, List<Object>> ellipsisBindings, Env defEnv) {
            this.bindings = bindings;
            this.ellipsisBindings = ellipsisBindings;
            this.defEnv = defEnv;
        }
    }

    static final ThreadLocal<Deque<SyntaxCaseContext>> SYNTAX_CASE_STACK =
        ThreadLocal.withInitial(ArrayDeque::new);

    private static final AtomicLong syntaxCounter = new AtomicLong(0);

    private static final Set<String> SYNTAX_SPECIAL_FORMS = Set.of(
        "define", "if", "quote", "lambda", "set!", "and", "or", "begin",
        "let", "cond", "define-syntax", "syntax-rules", "let*", "letrec",
        "letrec*", "case-lambda", "do", "case", "guard", "define-record-type",
        "with-syntax", "syntax-case", "syntax", "quasiquote", "unquote"
    );

    /** Merge all syntax-case contexts (innermost first) into combined bindings. */
    private static SyntaxCaseContext mergedSyntaxContext() {
        Deque<SyntaxCaseContext> stack = SYNTAX_CASE_STACK.get();
        Map<String, Object> merged = new HashMap<>();
        Map<String, List<Object>> mergedEllipsis = new HashMap<>();
        Env defEnv = null;
        // Iterate from bottom to top so inner shadows outer
        for (SyntaxCaseContext ctx : stack) {
            merged.putAll(ctx.bindings);
            mergedEllipsis.putAll(ctx.ellipsisBindings);
            defEnv = ctx.defEnv;
        }
        return new SyntaxCaseContext(merged, mergedEllipsis, defEnv);
    }

    /** Expand a syntax template using the given context. */
    private static Object expandSyntaxTemplate(Object tmpl, SyntaxCaseContext ctx,
            Map<String, String> renames) throws EvalError {
        if (tmpl instanceof String s) {
            if (s.startsWith("\"")) return s;
            if (ctx.bindings.containsKey(s)) return ctx.bindings.get(s);
            if (ctx.ellipsisBindings.containsKey(s)) return ctx.bindings.get(s); // shouldn't be bare
            if (!SYNTAX_SPECIAL_FORMS.contains(s) && !ctx.bindings.containsKey(s)
                    && !ctx.ellipsisBindings.containsKey(s) && !"...".equals(s) && !"_".equals(s)) {
                return renames.computeIfAbsent(s, k -> k + "$" + syntaxCounter.incrementAndGet());
            }
            return s;
        }
        if (tmpl instanceof List<?> list) {
            // Don't expand inside (quote ...)
            if (!list.isEmpty() && list.get(0) instanceof String qs && "quote".equals(qs)) {
                return tmpl;
            }
            List<Object> result = new ArrayList<>();
            for (int i = 0; i < list.size(); i++) {
                boolean hasEllipsis = i + 1 < list.size()
                        && list.get(i + 1) instanceof String ds && "...".equals(ds);
                if (hasEllipsis) {
                    String eVar = findEllipsisVar(list.get(i), ctx.ellipsisBindings);
                    if (eVar != null) {
                        for (Object val : ctx.ellipsisBindings.get(eVar)) {
                            Map<String, Object> lb = new HashMap<>(ctx.bindings);
                            lb.put(eVar, val);
                            SyntaxCaseContext localCtx = new SyntaxCaseContext(lb, ctx.ellipsisBindings, ctx.defEnv);
                            result.add(expandSyntaxTemplate(list.get(i), localCtx, renames));
                        }
                    }
                    i++; // skip ...
                } else {
                    result.add(expandSyntaxTemplate(list.get(i), ctx, renames));
                }
            }
            return result;
        }
        return tmpl;
    }

    private static String findEllipsisVar(Object tmpl, Map<String, List<Object>> ellipsisBindings) {
        if (tmpl instanceof String s && ellipsisBindings.containsKey(s)) return s;
        if (tmpl instanceof List<?> list) {
            for (Object e : list) {
                String found = findEllipsisVar(e, ellipsisBindings);
                if (found != null) return found;
            }
        }
        return null;
    }

    /** Match a syntax-case pattern against an input form. */
    @SuppressWarnings("unchecked")
    private static boolean matchSyntaxCasePattern(Object pattern, Object input,
            List<String> literals, Map<String, Object> bindings,
            Map<String, List<Object>> ellipsisBindings) {
        if (pattern instanceof String s) {
            if (s.equals("_")) return true;
            if (s.startsWith("\"")) return s.equals(input);
            if (literals.contains(s)) return s.equals(input);
            bindings.put(s, input);
            return true;
        }
        if (pattern instanceof List<?> patList) {
            if (!(input instanceof List<?> form)) return false;
            int pi = 0, fi = 0;
            while (pi < patList.size()) {
                boolean hasEllipsis = pi + 1 < patList.size() && "...".equals(patList.get(pi + 1));
                if (hasEllipsis) {
                    Object pe = patList.get(pi);
                    if (!(pe instanceof String varName)) return false;
                    int remaining = 0;
                    for (int j = pi + 2; j < patList.size(); j++) {
                        if (!"...".equals(patList.get(j))) remaining++;
                    }
                    int available = form.size() - fi - remaining;
                    if (available < 0) return false;
                    List<Object> collected = new ArrayList<>();
                    for (int i = 0; i < available; i++) collected.add(form.get(fi + i));
                    ellipsisBindings.put(varName, collected);
                    fi += available;
                    pi += 2;
                } else {
                    if (fi >= form.size()) return false;
                    Object pe = patList.get(pi);
                    if (!matchSyntaxCasePattern(pe, form.get(fi), literals, bindings, ellipsisBindings))
                        return false;
                    pi++;
                    fi++;
                }
            }
            return fi == form.size();
        }
        // literal value
        if (pattern instanceof Long || pattern instanceof Boolean) return pattern.equals(input);
        return false;
    }

    /** Evaluate syntax-case form. */
    @SuppressWarnings("unchecked")
    private static Object evalSyntaxCase(List<?> list, Env env) throws EvalError {
        // (syntax-case expr (literals) clause ...)
        if (list.size() < 4) throw new EvalError("syntax-case: bad syntax");
        Object input = eval(list.get(1), env);
        if (!(list.get(2) instanceof List<?> litList))
            throw new EvalError("syntax-case: literals must be a list");
        List<String> literals = new ArrayList<>();
        for (Object l : litList) {
            if (l instanceof String s) literals.add(s);
        }
        for (int i = 3; i < list.size(); i++) {
            if (!(list.get(i) instanceof List<?> clause) || clause.size() < 2 || clause.size() > 3)
                throw new EvalError("syntax-case: bad clause");
            Object pattern = clause.get(0);
            boolean hasFender = clause.size() == 3;
            Object fender = hasFender ? clause.get(1) : null;
            Object body = clause.get(hasFender ? 2 : 1);

            Map<String, Object> bindings = new HashMap<>();
            Map<String, List<Object>> ellipsisBindings = new HashMap<>();
            if (matchSyntaxCasePattern(pattern, input, literals, bindings, ellipsisBindings)) {
                // Create env with pattern variable bindings
                Env matchEnv = new Env(env);
                for (var entry : bindings.entrySet()) matchEnv.define(entry.getKey(), entry.getValue());

                if (hasFender) {
                    Object fenderResult = eval(fender, matchEnv);
                    if (isFalse(fenderResult)) continue;
                }

                // Push syntax-case context
                Deque<SyntaxCaseContext> stack = SYNTAX_CASE_STACK.get();
                stack.push(new SyntaxCaseContext(bindings, ellipsisBindings, env));
                try {
                    return eval(body, matchEnv);
                } finally {
                    stack.pop();
                }
            }
        }
        throw new EvalError("syntax-case: no matching pattern");
    }

    /** Evaluate syntax (aka #') form. */
    private static Object evalSyntax(List<?> list, Env env) throws EvalError {
        if (list.size() != 2) throw new EvalError("syntax: bad syntax");
        Object template = list.get(1);
        SyntaxCaseContext ctx = mergedSyntaxContext();
        if (ctx.defEnv == null) throw new EvalError("syntax: not in a syntax-case context");

        // Single pattern variable reference
        if (template instanceof String s && !s.startsWith("\"")) {
            if (ctx.bindings.containsKey(s)) return ctx.bindings.get(s);
        }

        // Full template expansion
        Map<String, String> renames = new HashMap<>();
        Object expanded = expandSyntaxTemplate(template, ctx, renames);
        return new SyntaxResult(expanded, renames, ctx.defEnv);
    }

    /** Evaluate with-syntax form. */
    @SuppressWarnings("unchecked")
    private static Object evalWithSyntax(List<?> list, Env env) throws EvalError {
        // (with-syntax ((pattern expr) ...) body ...)
        if (list.size() < 3) throw new EvalError("with-syntax: bad syntax");
        if (!(list.get(1) instanceof List<?> bindingsList))
            throw new EvalError("with-syntax: bindings must be a list");

        Map<String, Object> bindings = new HashMap<>();
        Map<String, List<Object>> ellipsisBindings = new HashMap<>();
        for (Object b : bindingsList) {
            if (!(b instanceof List<?> binding) || binding.size() != 2)
                throw new EvalError("with-syntax: bad binding");
            Object pattern = binding.get(0);
            Object val = eval(binding.get(1), env);
            if (pattern instanceof String s && !s.startsWith("\"")) {
                bindings.put(s, val);
            } else {
                matchSyntaxCasePattern(pattern, val, List.of(), bindings, ellipsisBindings);
            }
        }

        Env wsEnv = new Env(env);
        for (var entry : bindings.entrySet()) wsEnv.define(entry.getKey(), entry.getValue());

        Deque<SyntaxCaseContext> stack = SYNTAX_CASE_STACK.get();
        stack.push(new SyntaxCaseContext(bindings, ellipsisBindings, env));
        try {
            Object result = null;
            for (int i = 2; i < list.size(); i++) result = eval(list.get(i), wsEnv);
            return result;
        } finally {
            stack.pop();
        }
    }

    /** Handle macro expansion result (SyntaxResult or raw form). */
    private static Object[] handleMacroResult(Object result, Env callEnv) throws EvalError {
        if (result instanceof SyntaxResult sr) {
            // Add renames directly to call env (unique names won't conflict)
            for (var entry : sr.renames.entrySet()) {
                try {
                    Object val = sr.defEnv.lookup(entry.getKey());
                    callEnv.define(entry.getValue(), val);
                } catch (EvalError e) { /* not bound at def site, skip */ }
            }
            return new Object[] { sr.form, callEnv };
        }
        // Raw form (no hygiene renames needed)
        return new Object[] { result, callEnv };
    }

    static void windTo(List<DynamicWindEntry> target) throws EvalError {
        List<DynamicWindEntry> current = WIND_STACK.get();
        int common = 0;
        int minLen = Math.min(current.size(), target.size());
        while (common < minLen && current.get(common) == target.get(common)) common++;
        // Unwind: out-thunks from innermost to outermost
        for (int i = current.size() - 1; i >= common; i--) {
            DynamicWindEntry e = current.remove(i);
            applyProc(e.outThunk, List.of());
        }
        // Rewind: in-thunks from outermost to innermost
        for (int i = common; i < target.size(); i++) {
            current.add(target.get(i));
            applyProc(target.get(i).inThunk, List.of());
        }
    }

    // ===== Continuation frames (immutable linked list via 'next' pointer) =====

    sealed interface Kont {
        record Halt() implements Kont {}
        record Seq(List<Object> exprs, int idx, Env env, Kont next) implements Kont {}
        record Def(String name, Env env, Kont next) implements Kont {}
        record Set(String name, Env env, Kont next) implements Kont {}
        record If(Object thenE, Object elseE, boolean hasElse, Env env, Kont next) implements Kont {}
        record Arg(List<Object> evald, List<Object> todo, List<?> form, Env env, Kont next) implements Kont {}
        record And(List<?> form, int idx, Env env, Kont next) implements Kont {}
        record Or(List<?> form, int idx, Env env, Kont next) implements Kont {}
        record CondTest(List<?> clause, int nextCI, List<?> form, Env env, Kont next) implements Kont {}
        record LetStar(List<?> bindings, int idx, Env letEnv, List<Object> body, Kont next) implements Kont {}
        record Letrec(List<String> names, int idx, List<Object> inits, List<Object> body, Env letEnv, Kont next) implements Kont {}
        record DynWindBody(Object inThunk, Object outThunk, Kont next) implements Kont {}
        record CallWithValues(Object consumer, Kont next) implements Kont {}
        record Guard(String varName, List<List<?>> clauses, Env env, Kont next) implements Kont {}
    }

    private static Kont nextKont(Kont k) {
        return switch (k) {
            case Kont.Halt h -> null;
            case Kont.Seq s -> s.next();
            case Kont.Def d -> d.next();
            case Kont.Set s -> s.next();
            case Kont.If i -> i.next();
            case Kont.Arg a -> a.next();
            case Kont.And a -> a.next();
            case Kont.Or o -> o.next();
            case Kont.CondTest c -> c.next();
            case Kont.LetStar l -> l.next();
            case Kont.Letrec l -> l.next();
            case Kont.DynWindBody d -> d.next();
            case Kont.CallWithValues c -> c.next();
            case Kont.Guard g -> g.next();
        };
    }

    // ===== CEK Machine =====

    @SuppressWarnings("unchecked")
    static Object runCEK(List<Object> program, Env env) throws EvalError {
        if (program.isEmpty()) return null;

        Object ctrl = program.get(0);
        Kont k = program.size() > 1
            ? new Kont.Seq(program, 1, env, new Kont.Halt())
            : new Kont.Halt();
        boolean ev = true;   // true = evaluate ctrl; false = apply k to val
        Object val = null;
        Object fn = null;     // non-null = apply fn to fa
        List<Object> fa = null;
        SourceList lastSrc = null;

        try { mainLoop: while (true) { try {

            // Step limit check
            long[] stepLim = STEP_LIMIT.get();
            if (stepLim != null) {
                if (stepLim[0] <= 0) throw new EvalError("step limit exceeded");
                stepLim[0]--;
            }

            // ---- function application ----
            if (fn != null) {
                if (fn instanceof ContinuationObj co) {
                    if (fa == null || fa.isEmpty()) { val = null; }
                    else if (fa.size() == 1) { val = fa.get(0); }
                    else { val = new MultipleValues(new ArrayList<>(fa)); }
                    windTo(co.windStack);
                    k = co.kont; fn = null; fa = null; ev = false; continue;
                }
                if (fn == CALLCC) {
                    ContinuationObj co = new ContinuationObj(k, new ArrayList<>(WIND_STACK.get()));
                    fn = fa.get(0); fa = List.of(co); continue;
                }
                if (fn == DYNAMIC_WIND) {
                    if (fa.size() != 3) throw new EvalError("dynamic-wind: expected 3 arguments");
                    Object inThunk = fa.get(0), bodyThunk = fa.get(1), outThunk = fa.get(2);
                    try { applyProc(inThunk, List.of()); }
                    catch (ContinuationReturn cr) { val = cr.value; k = cr.kont; fn = null; fa = null; ev = false; continue; }
                    WIND_STACK.get().add(new DynamicWindEntry(inThunk, outThunk));
                    fn = bodyThunk; fa = List.of();
                    k = new Kont.DynWindBody(inThunk, outThunk, k);
                    continue;
                }
                if (fn == CALL_WITH_VALUES) {
                    if (fa.size() != 2) throw new EvalError("call-with-values: expected 2 arguments");
                    Object producer = fa.get(0), consumer = fa.get(1);
                    k = new Kont.CallWithValues(consumer, k);
                    fn = producer; fa = List.of(); continue;
                }
                if (fn instanceof CaseLambda cl) {
                    Lambda matched = null;
                    for (Lambda l : cl.clauses) {
                        if (l.restParam != null ? fa.size() >= l.params.size() : fa.size() == l.params.size())
                        { matched = l; break; }
                    }
                    if (matched == null)
                        throw new EvalError("case-lambda: no matching clause for " + fa.size() + " arguments");
                    fn = matched; continue;
                }
                if (fn instanceof Lambda lam) {
                    if (lam.restParam == null) {
                        if (fa.size() != lam.params.size())
                            throw new EvalError("lambda: expected " + lam.params.size() + " arguments, got " + fa.size());
                    } else {
                        if (fa.size() < lam.params.size())
                            throw new EvalError("lambda: expected at least " + lam.params.size() + " arguments, got " + fa.size());
                    }
                    Env le = new Env(lam.closure);
                    for (int i = 0; i < lam.params.size(); i++) le.define(lam.params.get(i), fa.get(i));
                    if (lam.restParam != null) {
                        Object rest = SchemeValue.NIL;
                        for (int i = fa.size() - 1; i >= lam.params.size(); i--) rest = new Pair(fa.get(i), rest);
                        le.define(lam.restParam, rest);
                    }
                    ctrl = lam.body.get(0);
                    if (lam.body.size() > 1) k = new Kont.Seq(lam.body, 1, le, k);
                    env = le; fn = null; fa = null; ev = true; continue;
                }
                if (fn instanceof Builtin b) {
                    try { val = b.apply(fa); }
                    catch (ContinuationReturn cr) { val = cr.value; k = cr.kont; }
                    fn = null; fa = null; ev = false; continue;
                }
                throw new EvalError("not a procedure: " + SchemeValue.toStr(fn));
            }

            // ---- evaluate ctrl ----
            if (ev) {
                if (ctrl instanceof Long || ctrl instanceof Boolean || ctrl instanceof Character
                        || ctrl instanceof Double || ctrl instanceof Rational || ctrl instanceof MutableString) {
                    val = ctrl; ev = false; continue;
                }
                if (ctrl instanceof String s) {
                    val = s.startsWith("\"") ? s : env.lookup(s);
                    ev = false; continue;
                }
                if (!(ctrl instanceof List<?> list)) throw new EvalError("cannot evaluate: " + ctrl);
                if (list.isEmpty()) throw new EvalError("empty application");
                if (list instanceof SourceList sl) lastSrc = sl;

                Object first = list.get(0);
                if (first instanceof String op) { switch (op) {
                    case "quote" -> {
                        if (list.size() != 2) throw new EvalError("quote: expected 1 argument");
                        val = SchemeValue.quotedToScheme(list.get(1));
                        ev = false; continue mainLoop;
                    }
                    case "quasiquote" -> {
                        if (list.size() != 2) throw new EvalError("quasiquote: expected 1 argument");
                        val = evalQuasiquote(list.get(1), env, 0);
                        ev = false; continue mainLoop;
                    }
                    case "lambda" -> {
                        val = evalLambda(list, env);
                        ev = false; continue mainLoop;
                    }
                    case "define" -> {
                        if (list.size() < 3) throw new EvalError("define: bad syntax");
                        Object tgt = list.get(1);
                        if (tgt instanceof String name) {
                            ctrl = list.get(2); k = new Kont.Def(name, env, k); continue mainLoop;
                        }
                        if (tgt instanceof List<?> || tgt instanceof Pair) {
                            evalDefine(list, env); val = null; ev = false; continue mainLoop;
                        }
                        throw new EvalError("define: bad syntax");
                    }
                    case "set!" -> {
                        if (list.size() != 3) throw new EvalError("set!: bad syntax");
                        if (!(list.get(1) instanceof String name) || name.startsWith("\""))
                            throw new EvalError("set!: expected variable name");
                        ctrl = list.get(2); k = new Kont.Set(name, env, k); continue mainLoop;
                    }
                    case "if" -> {
                        if (list.size() < 3 || list.size() > 4) throw new EvalError("if: bad syntax");
                        ctrl = list.get(1);
                        k = new Kont.If(list.get(2),
                            list.size() == 4 ? list.get(3) : null,
                            list.size() == 4, env, k);
                        continue mainLoop;
                    }
                    case "begin" -> {
                        if (list.size() == 1) { val = null; ev = false; continue mainLoop; }
                        ctrl = list.get(1);
                        if (list.size() > 2) {
                            List<Object> r = new ArrayList<>(list.size() - 2);
                            for (int i = 2; i < list.size(); i++) r.add(list.get(i));
                            k = new Kont.Seq(r, 0, env, k);
                        }
                        continue mainLoop;
                    }
                    case "and" -> {
                        if (list.size() == 1) { val = Boolean.TRUE; ev = false; continue mainLoop; }
                        ctrl = list.get(1);
                        if (list.size() > 2) k = new Kont.And(list, 2, env, k);
                        continue mainLoop;
                    }
                    case "or" -> {
                        if (list.size() == 1) { val = Boolean.FALSE; ev = false; continue mainLoop; }
                        ctrl = list.get(1);
                        if (list.size() > 2) k = new Kont.Or(list, 2, env, k);
                        continue mainLoop;
                    }
                    case "cond" -> {
                        if (list.size() < 2) { val = null; ev = false; continue mainLoop; }
                        List<?> cl = (List<?>) list.get(1);
                        if (cl.isEmpty()) throw new EvalError("cond: bad clause");
                        if (cl.get(0) instanceof String cs && cs.equals("else")) {
                            if (cl.size() == 1) { val = null; ev = false; continue mainLoop; }
                            ctrl = cl.get(1);
                            if (cl.size() > 2) {
                                List<Object> r = new ArrayList<>();
                                for (int i = 2; i < cl.size(); i++) r.add(cl.get(i));
                                k = new Kont.Seq(r, 0, env, k);
                            }
                            continue mainLoop;
                        }
                        ctrl = cl.get(0);
                        k = new Kont.CondTest(cl, 2, list, env, k);
                        continue mainLoop;
                    }
                    case "let" -> {
                        if (list.size() < 3) throw new EvalError("let: bad syntax");
                        int off = 1; String letName = null;
                        if (list.get(1) instanceof String ls && !ls.startsWith("\"")) { letName = ls; off = 2; }
                        if (off >= list.size()) throw new EvalError("let: bad syntax");
                        if (!(list.get(off) instanceof List<?> bindings))
                            throw new EvalError("let: bindings must be a list");
                        List<String> params = new ArrayList<>();
                        List<Object> initExprs = new ArrayList<>();
                        for (Object b : bindings) {
                            if (!(b instanceof List<?> bd) || bd.size() != 2)
                                throw new EvalError("let: bad binding");
                            if (!(bd.get(0) instanceof String p))
                                throw new EvalError("let: binding name must be symbol");
                            params.add(p); initExprs.add(bd.get(1));
                        }
                        List<Object> body = new ArrayList<>();
                        for (int i = off + 1; i < list.size(); i++) body.add(list.get(i));
                        Lambda lam;
                        if (letName != null) {
                            Env le = new Env(env);
                            lam = new Lambda(params, null, body, le);
                            le.define(letName, lam);
                        } else {
                            lam = new Lambda(params, null, body, env);
                        }
                        if (initExprs.isEmpty()) { fn = lam; fa = List.of(); continue mainLoop; }
                        // Reverse init expressions for right-to-left evaluation (matches Arg frame convention)
                        List<Object> revInits = new ArrayList<>(initExprs);
                        Collections.reverse(revInits);
                        ctrl = revInits.get(0);
                        k = new Kont.Arg(List.of(lam),
                            revInits.size() > 1 ? new ArrayList<>(revInits.subList(1, revInits.size())) : List.of(),
                            null, env, k);
                        continue mainLoop;
                    }
                    case "let*" -> {
                        if (list.size() < 3) throw new EvalError("let*: bad syntax");
                        if (!(list.get(1) instanceof List<?> bindings))
                            throw new EvalError("let*: bindings must be a list");
                        List<Object> body = new ArrayList<>();
                        for (int i = 2; i < list.size(); i++) body.add(list.get(i));
                        if (bindings.isEmpty()) {
                            ctrl = body.get(0);
                            if (body.size() > 1) k = new Kont.Seq(body, 1, env, k);
                            continue mainLoop;
                        }
                        Env le = new Env(env);
                        List<?> fb = (List<?>) bindings.get(0);
                        if (fb.size() != 2 || !(fb.get(0) instanceof String))
                            throw new EvalError("let*: bad binding");
                        ctrl = fb.get(1);
                        k = new Kont.LetStar(bindings, 0, le, body, k);
                        continue mainLoop;
                    }
                    case "letrec", "letrec*" -> {
                        if (list.size() < 3) throw new EvalError(op + ": bad syntax");
                        if (!(list.get(1) instanceof List<?> bindings))
                            throw new EvalError(op + ": bindings must be a list");
                        List<String> names = new ArrayList<>();
                        List<Object> inits = new ArrayList<>();
                        Env le = new Env(env);
                        for (Object b : bindings) {
                            if (!(b instanceof List<?> bd) || bd.size() != 2)
                                throw new EvalError(op + ": bad binding");
                            if (!(bd.get(0) instanceof String n))
                                throw new EvalError(op + ": binding name must be symbol");
                            names.add(n); inits.add(bd.get(1)); le.define(n, null);
                        }
                        List<Object> body = new ArrayList<>();
                        for (int i = 2; i < list.size(); i++) body.add(list.get(i));
                        if (names.isEmpty()) {
                            ctrl = body.get(0); env = le;
                            if (body.size() > 1) k = new Kont.Seq(body, 1, le, k);
                            continue mainLoop;
                        }
                        ctrl = inits.get(0); env = le;
                        k = new Kont.Letrec(names, 0, inits, body, le, k);
                        continue mainLoop;
                    }
                    case "define-syntax" -> {
                        if (list.size() != 3) throw new EvalError("define-syntax: bad syntax");
                        if (!(list.get(1) instanceof String name) || name.startsWith("\""))
                            throw new EvalError("define-syntax: expected name");
                        Object tr = list.get(2);
                        if (tr instanceof List<?> tl && !tl.isEmpty() && "syntax-rules".equals(tl.get(0))) {
                            env.define(name, SyntaxRules.parse(tl, env));
                        } else {
                            Object transformer = eval(tr, env);
                            env.define(name, new MacroTransformer(transformer));
                        }
                        val = null; ev = false; continue mainLoop;
                    }
                    case "define-record-type" -> {
                        evalDefineRecordType(list, env);
                        val = null; ev = false; continue mainLoop;
                    }
                    case "case-lambda" -> {
                        val = evalCaseLambda(list, env);
                        ev = false; continue mainLoop;
                    }
                    case "do" -> {
                        try { val = evalDo(list, env); }
                        catch (ContinuationReturn cr) { val = cr.value; k = cr.kont; }
                        ev = false; continue mainLoop;
                    }
                    case "case" -> {
                        try { val = evalCase(list, env); }
                        catch (ContinuationReturn cr) { val = cr.value; k = cr.kont; }
                        ev = false; continue mainLoop;
                    }
                    case "guard" -> {
                        if (list.size() < 3) throw new EvalError("guard: bad syntax");
                        if (!(list.get(1) instanceof List<?> guardSpec) || guardSpec.isEmpty())
                            throw new EvalError("guard: bad syntax");
                        if (!(guardSpec.get(0) instanceof String gVarName))
                            throw new EvalError("guard: expected variable name");
                        List<List<?>> gClauses = new ArrayList<>();
                        for (int gi = 1; gi < guardSpec.size(); gi++) {
                            if (!(guardSpec.get(gi) instanceof List<?> gClause))
                                throw new EvalError("guard: bad clause");
                            gClauses.add(gClause);
                        }
                        List<Object> gBody = new ArrayList<>();
                        for (int gi = 2; gi < list.size(); gi++) gBody.add(list.get(gi));
                        Kont guardK = new Kont.Guard(gVarName, gClauses, env, k);
                        if (gBody.size() > 1) {
                            k = new Kont.Seq(gBody, 1, env, guardK);
                        } else {
                            k = guardK;
                        }
                        ctrl = gBody.get(0);
                        ev = true; continue mainLoop;
                    }
                    default -> { /* fall through to application */ }
                }}

                // ---- function application (args evaluated right-to-left) ----
                ctrl = first;
                List<Object> remaining = new ArrayList<>(list.size() - 1);
                for (int i = list.size() - 1; i >= 1; i--) remaining.add(list.get(i));
                k = new Kont.Arg(List.of(), remaining, list, env, k);
                continue;
            }

            // ---- apply continuation to val ----
            switch (k) {
                case Kont.Halt() -> { return val; }

                case Kont.Seq(var exprs, var idx, var e, var next) -> {
                    ctrl = exprs.get(idx);
                    k = (idx + 1 < exprs.size()) ? new Kont.Seq(exprs, idx + 1, e, next) : next;
                    env = e; ev = true;
                }

                case Kont.Def(var name, var e, var next) -> {
                    e.define(name, val); val = null; k = next;
                }

                case Kont.Set(var name, var e, var next) -> {
                    e.set(name, val); val = null; k = next;
                }

                case Kont.If(var th, var el, var hasEl, var e, var next) -> {
                    if (!isFalse(val)) { ctrl = th; k = next; env = e; ev = true; }
                    else if (hasEl) { ctrl = el; k = next; env = e; ev = true; }
                    else { val = null; k = next; }
                }

                case Kont.Arg(var evald, var todo, var form, var e, var next) -> {
                    List<Object> ne = new ArrayList<>(evald);
                    ne.add(val);
                    // macro expansion: operator is SyntaxRules
                    if (evald.isEmpty() && val instanceof SyntaxRules sr && form != null) {
                        Object[] expanded = sr.expandToForm(form, e);
                        ctrl = expanded[0]; env = (Env) expanded[1]; k = next; ev = true;
                        continue mainLoop;
                    }
                    // macro expansion: operator is MacroTransformer (syntax-case lambda)
                    if (evald.isEmpty() && val instanceof MacroTransformer mt && form != null) {
                        Object result = applyProc(mt.procedure, List.of(form));
                        Object[] expanded = handleMacroResult(result, e);
                        ctrl = expanded[0]; env = (Env) expanded[1]; k = next; ev = true;
                        continue mainLoop;
                    }
                    if (todo.isEmpty()) {
                        fn = ne.get(0);
                        // args were collected in reverse order; reverse back
                        List<Object> args = new ArrayList<>(ne.subList(1, ne.size()));
                        Collections.reverse(args);
                        fa = args;
                        k = next;
                    } else {
                        ctrl = todo.get(0);
                        k = new Kont.Arg(new ArrayList<>(ne),
                            todo.size() > 1 ? new ArrayList<>(todo.subList(1, todo.size())) : List.of(),
                            form, e, next);
                        env = e; ev = true;
                    }
                }

                case Kont.And(var form, var idx, var e, var next) -> {
                    if (isFalse(val)) { k = next; }
                    else if (idx >= form.size() - 1) { ctrl = form.get(idx); k = next; env = e; ev = true; }
                    else { ctrl = form.get(idx); k = new Kont.And(form, idx + 1, e, next); env = e; ev = true; }
                }

                case Kont.Or(var form, var idx, var e, var next) -> {
                    if (!isFalse(val)) { k = next; }
                    else if (idx >= form.size() - 1) { ctrl = form.get(idx); k = next; env = e; ev = true; }
                    else { ctrl = form.get(idx); k = new Kont.Or(form, idx + 1, e, next); env = e; ev = true; }
                }

                case Kont.CondTest(var clause, var nextCI, var form, var e, var next) -> {
                    if (!isFalse(val)) {
                        if (clause.size() == 1) { k = next; /* return test value */ }
                        else if (clause.size() == 3 && clause.get(1) instanceof String s && s.equals("=>")) {
                            // (test => proc) — apply proc to test result
                            Object testVal = val;
                            List<Object> quoted = new ArrayList<>();
                            quoted.add("quote");
                            quoted.add(testVal);
                            ctrl = clause.get(2);
                            k = new Kont.Arg(new ArrayList<>(), List.of(quoted), null, e, next);
                            env = e; ev = true;
                        }
                        else {
                            ctrl = clause.get(1);
                            if (clause.size() > 2) {
                                List<Object> r = new ArrayList<>();
                                for (int i = 2; i < clause.size(); i++) r.add(clause.get(i));
                                k = new Kont.Seq(r, 0, e, next);
                            } else { k = next; }
                            env = e; ev = true;
                        }
                    } else {
                        if (nextCI >= form.size()) { val = null; k = next; }
                        else {
                            List<?> nc = (List<?>) form.get(nextCI);
                            if (nc.isEmpty()) throw new EvalError("cond: bad clause");
                            if (nc.get(0) instanceof String cs && cs.equals("else")) {
                                if (nc.size() == 1) { val = null; k = next; }
                                else {
                                    ctrl = nc.get(1);
                                    if (nc.size() > 2) {
                                        List<Object> r = new ArrayList<>();
                                        for (int i = 2; i < nc.size(); i++) r.add(nc.get(i));
                                        k = new Kont.Seq(r, 0, e, next);
                                    } else { k = next; }
                                    env = e; ev = true;
                                }
                            } else {
                                ctrl = nc.get(0);
                                k = new Kont.CondTest(nc, nextCI + 1, form, e, next);
                                env = e; ev = true;
                            }
                        }
                    }
                }

                case Kont.LetStar(var bindings, var idx, var le, var body, var next) -> {
                    String n = (String) ((List<?>) bindings.get(idx)).get(0);
                    le.define(n, val);
                    if (idx + 1 < bindings.size()) {
                        List<?> nb = (List<?>) bindings.get(idx + 1);
                        if (nb.size() != 2 || !(nb.get(0) instanceof String))
                            throw new EvalError("let*: bad binding");
                        ctrl = nb.get(1);
                        k = new Kont.LetStar(bindings, idx + 1, le, body, next);
                        env = le; ev = true;
                    } else {
                        ctrl = body.get(0);
                        k = body.size() > 1 ? new Kont.Seq(body, 1, le, next) : next;
                        env = le; ev = true;
                    }
                }

                case Kont.Letrec(var names, var idx, var inits, var body, var le, var next) -> {
                    le.set(names.get(idx), val);
                    if (idx + 1 < names.size()) {
                        ctrl = inits.get(idx + 1);
                        k = new Kont.Letrec(names, idx + 1, inits, body, le, next);
                        env = le; ev = true;
                    } else {
                        ctrl = body.get(0);
                        k = body.size() > 1 ? new Kont.Seq(body, 1, le, next) : next;
                        env = le; ev = true;
                    }
                }

                case Kont.DynWindBody(var inThunk, var outThunk, var next) -> {
                    var ws = WIND_STACK.get();
                    if (!ws.isEmpty()) ws.remove(ws.size() - 1);
                    try { applyProc(outThunk, List.of()); }
                    catch (ContinuationReturn cr) { val = cr.value; k = cr.kont; continue mainLoop; }
                    k = next;
                }

                case Kont.CallWithValues(var consumer, var next) -> {
                    if (val instanceof MultipleValues mv) {
                        fn = consumer; fa = mv.values; k = next;
                    } else {
                        fn = consumer; fa = List.of(val); k = next;
                    }
                }

                case Kont.Guard(var vn, var cl, var ge, var next) -> {
                    // Body completed normally — skip through consecutive Guard frames
                    k = next;
                    while (k instanceof Kont.Guard g2) k = g2.next();
                }
            }

        } catch (SchemeException se) {
            // Search continuation chain for nearest Guard frame
            Kont searchK = k;
            boolean guardHandled = false;
            while (searchK != null) {
                if (searchK instanceof Kont.Guard g) {
                    // Unwind DynWindBody frames between k and this guard
                    Kont uw = k;
                    while (uw != g) {
                        if (uw instanceof Kont.DynWindBody dwb) {
                            var ws = WIND_STACK.get();
                            if (!ws.isEmpty()) ws.remove(ws.size() - 1);
                            applyProc(dwb.outThunk(), List.of());
                        }
                        uw = nextKont(uw);
                    }
                    // Evaluate guard clauses
                    Env guardEnv = new Env(g.env());
                    guardEnv.define(g.varName(), se.value);
                    boolean matched = false;
                    for (List<?> clause : g.clauses()) {
                        if (clause.isEmpty()) throw new EvalError("guard: bad clause");
                        Object test = clause.get(0);
                        if (test instanceof String cs && cs.equals("else")) {
                            if (clause.size() == 1) { val = null; }
                            else {
                                Object result = null;
                                for (int j = 1; j < clause.size(); j++) result = eval(clause.get(j), guardEnv);
                                val = result;
                            }
                            matched = true; break;
                        }
                        Object testVal = eval(test, guardEnv);
                        if (!isFalse(testVal)) {
                            if (clause.size() == 1) { val = testVal; }
                            else {
                                Object result = null;
                                for (int j = 1; j < clause.size(); j++) result = eval(clause.get(j), guardEnv);
                                val = result;
                            }
                            matched = true; break;
                        }
                    }
                    if (!matched) { searchK = nextKont(searchK); continue; }
                    k = g.next(); fn = null; fa = null; ev = false;
                    guardHandled = true; break;
                }
                searchK = nextKont(searchK);
            }
            if (guardHandled) continue mainLoop;
            throw new EvalError("unhandled exception: " + SchemeValue.toStr(se.value));
        }

        }} catch (EvalError e) {
            if (lastSrc != null && !e.getMessage().matches(".*\\d+:\\d+.*"))
                throw new EvalError(e.getMessage() + " [" + lastSrc.line + ":" + lastSrc.col + "]");
            throw e;
        }
    }

    // ===== Legacy eval (used by SyntaxRules.expand, evalCase, evalDo, builtins) =====

    @SuppressWarnings("unchecked")
    static Object eval(Object expr, Env env) throws EvalError {
        while (true) {
            if (expr instanceof Long || expr instanceof Boolean || expr instanceof Character
                    || expr instanceof Double || expr instanceof Rational) {
                return expr;
            }
            if (expr instanceof String s) {
                if (s.startsWith("\"")) return s;
                return env.lookup(s);
            }
            if (expr instanceof MutableString) return expr;
            if (!(expr instanceof List<?> list)) {
                throw new EvalError("cannot evaluate: " + expr);
            }
            if (list.isEmpty()) throw new EvalError("empty application");

            try {

            Object first = list.get(0);

            if (first instanceof String op) {
                switch (op) {
                    case "define" -> { return evalDefine(list, env); }
                    case "quote" -> {
                        if (list.size() != 2) throw new EvalError("quote: expected 1 argument");
                        return SchemeValue.quotedToScheme(list.get(1));
                    }
                    case "quasiquote" -> {
                        if (list.size() != 2) throw new EvalError("quasiquote: expected 1 argument");
                        return evalQuasiquote(list.get(1), env, 0);
                    }
                    case "lambda" -> { return evalLambda(list, env); }
                    case "set!" -> {
                        if (list.size() != 3) throw new EvalError("set!: bad syntax");
                        Object target = list.get(1);
                        if (!(target instanceof String name) || name.startsWith("\""))
                            throw new EvalError("set!: expected variable name");
                        Object val = eval(list.get(2), env);
                        env.set(name, val);
                        return null;
                    }
                    case "define-record-type" -> { return evalDefineRecordType(list, env); }
                    case "case-lambda" -> { return evalCaseLambda(list, env); }
                    case "define-syntax" -> {
                        if (list.size() != 3) throw new EvalError("define-syntax: bad syntax");
                        if (!(list.get(1) instanceof String name) || name.startsWith("\""))
                            throw new EvalError("define-syntax: expected name");
                        Object transformer = list.get(2);
                        if (transformer instanceof List<?> trList && !trList.isEmpty()
                                && "syntax-rules".equals(trList.get(0))) {
                            env.define(name, SyntaxRules.parse(trList, env));
                        } else {
                            Object tr = eval(transformer, env);
                            env.define(name, new MacroTransformer(tr));
                        }
                        return null;
                    }
                    case "syntax-case" -> { return evalSyntaxCase(list, env); }
                    case "syntax" -> { return evalSyntax(list, env); }
                    case "with-syntax" -> { return evalWithSyntax(list, env); }
                    case "do" -> { return evalDo(list, env); }

                    case "if" -> {
                        if (list.size() < 3 || list.size() > 4) throw new EvalError("if: bad syntax");
                        Object cond = eval(list.get(1), env);
                        if (!isFalse(cond)) { expr = list.get(2); continue; }
                        else if (list.size() == 4) { expr = list.get(3); continue; }
                        return null;
                    }
                    case "and" -> {
                        if (list.size() == 1) return Boolean.TRUE;
                        for (int i = 1; i < list.size() - 1; i++) {
                            Object result = eval(list.get(i), env);
                            if (isFalse(result)) return result;
                        }
                        expr = list.get(list.size() - 1); continue;
                    }
                    case "or" -> {
                        if (list.size() == 1) return Boolean.FALSE;
                        for (int i = 1; i < list.size() - 1; i++) {
                            Object result = eval(list.get(i), env);
                            if (!isFalse(result)) return result;
                        }
                        expr = list.get(list.size() - 1); continue;
                    }
                    case "begin" -> {
                        if (list.size() == 1) return null;
                        for (int i = 1; i < list.size() - 1; i++) eval(list.get(i), env);
                        expr = list.get(list.size() - 1); continue;
                    }
                    case "let" -> {
                        if (list.size() < 3) throw new EvalError("let: bad syntax");
                        int offset = 1; String letName = null;
                        if (list.get(1) instanceof String ls && !ls.startsWith("\"")) { letName = ls; offset = 2; }
                        if (offset >= list.size()) throw new EvalError("let: bad syntax");
                        Object bindingsObj = list.get(offset);
                        if (!(bindingsObj instanceof List<?> bindings)) throw new EvalError("let: bindings must be a list");
                        List<String> params = new ArrayList<>(); List<Object> inits = new ArrayList<>();
                        for (Object b : bindings) {
                            if (!(b instanceof List<?> binding) || binding.size() != 2) throw new EvalError("let: bad binding");
                            if (!(binding.get(0) instanceof String p)) throw new EvalError("let: binding name must be symbol");
                            params.add(p); inits.add(eval(binding.get(1), env));
                        }
                        List<Object> body = new ArrayList<>();
                        for (int i = offset + 1; i < list.size(); i++) body.add(list.get(i));
                        if (letName != null) {
                            Env letEnv = new Env(env);
                            Lambda lambda = new Lambda(params, null, body, letEnv);
                            letEnv.define(letName, lambda);
                            Env localEnv = new Env(letEnv);
                            for (int i = 0; i < params.size(); i++) localEnv.define(params.get(i), inits.get(i));
                            for (int i = 0; i < body.size() - 1; i++) eval(body.get(i), localEnv);
                            expr = body.get(body.size() - 1); env = localEnv; continue;
                        } else {
                            Env letEnv = new Env(env);
                            for (int i = 0; i < params.size(); i++) letEnv.define(params.get(i), inits.get(i));
                            for (int i = 0; i < body.size() - 1; i++) eval(body.get(i), letEnv);
                            expr = body.get(body.size() - 1); env = letEnv; continue;
                        }
                    }
                    case "let*" -> {
                        if (list.size() < 3) throw new EvalError("let*: bad syntax");
                        if (!(list.get(1) instanceof List<?> bindings)) throw new EvalError("let*: bindings must be a list");
                        Env letStarEnv = new Env(env);
                        for (Object b : bindings) {
                            if (!(b instanceof List<?> binding) || binding.size() != 2) throw new EvalError("let*: bad binding");
                            if (!(binding.get(0) instanceof String name)) throw new EvalError("let*: binding name must be symbol");
                            letStarEnv.define(name, eval(binding.get(1), letStarEnv));
                        }
                        for (int i = 2; i < list.size() - 1; i++) eval(list.get(i), letStarEnv);
                        expr = list.get(list.size() - 1); env = letStarEnv; continue;
                    }
                    case "cond" -> {
                        boolean matched = false;
                        for (int i = 1; i < list.size(); i++) {
                            Object clause = list.get(i);
                            if (!(clause instanceof List<?> c) || c.isEmpty()) throw new EvalError("cond: bad clause");
                            Object test = c.get(0);
                            if (test instanceof String cs && cs.equals("else")) {
                                if (c.size() == 1) return null;
                                for (int j = 1; j < c.size() - 1; j++) eval(c.get(j), env);
                                expr = c.get(c.size() - 1); matched = true; break;
                            }
                            Object val = eval(test, env);
                            if (!isFalse(val)) {
                                if (c.size() == 1) return val;
                                if (c.size() == 3 && c.get(1) instanceof String s && s.equals("=>")) {
                                    Object proc = eval(c.get(2), env);
                                    return applyProc(proc, List.of(val));
                                }
                                for (int j = 1; j < c.size() - 1; j++) eval(c.get(j), env);
                                expr = c.get(c.size() - 1); matched = true; break;
                            }
                        }
                        if (matched) continue;
                        return null;
                    }
                    case "letrec" -> {
                        if (list.size() < 3) throw new EvalError("letrec: bad syntax");
                        if (!(list.get(1) instanceof List<?> bindings)) throw new EvalError("letrec: bindings must be a list");
                        Env letEnv = new Env(env);
                        List<String> names = new ArrayList<>(); List<Object> initExprs = new ArrayList<>();
                        for (Object b : bindings) {
                            if (!(b instanceof List<?> binding) || binding.size() != 2) throw new EvalError("letrec: bad binding");
                            if (!(binding.get(0) instanceof String name)) throw new EvalError("letrec: binding name must be symbol");
                            names.add(name); initExprs.add(binding.get(1)); letEnv.define(name, null);
                        }
                        for (int i = 0; i < names.size(); i++) letEnv.set(names.get(i), eval(initExprs.get(i), letEnv));
                        for (int i = 2; i < list.size() - 1; i++) eval(list.get(i), letEnv);
                        expr = list.get(list.size() - 1); env = letEnv; continue;
                    }
                    case "letrec*" -> {
                        if (list.size() < 3) throw new EvalError("letrec*: bad syntax");
                        if (!(list.get(1) instanceof List<?> bindings)) throw new EvalError("letrec*: bindings must be a list");
                        Env letEnv = new Env(env);
                        for (Object b : bindings) {
                            if (!(b instanceof List<?> binding) || binding.size() != 2) throw new EvalError("letrec*: bad binding");
                            if (!(binding.get(0) instanceof String name)) throw new EvalError("letrec*: binding name must be symbol");
                            letEnv.define(name, eval(binding.get(1), letEnv));
                        }
                        for (int i = 2; i < list.size() - 1; i++) eval(list.get(i), letEnv);
                        expr = list.get(list.size() - 1); env = letEnv; continue;
                    }
                    case "case" -> {
                        return evalCase(list, env);
                    }
                    case "guard" -> {
                        return evalGuard(list, env);
                    }
                }
            }

            // Function application
            Object func = eval(first, env);
            if (func instanceof SyntaxRules sr) {
                Object[] expanded = sr.expandToForm(list, env);
                expr = expanded[0]; env = (Env) expanded[1]; continue;
            }
            if (func instanceof MacroTransformer mt) {
                Object result = applyProc(mt.procedure, List.of(list));
                Object[] expanded = handleMacroResult(result, env);
                expr = expanded[0]; env = (Env) expanded[1]; continue;
            }
            List<Object> args = new ArrayList<>();
            for (int i = 1; i < list.size(); i++) args.add(eval(list.get(i), env));
            if (func instanceof Builtin b) { return b.apply(args); }
            if (func instanceof CaseLambda cl) {
                Lambda matched = null;
                for (Lambda lam : cl.clauses) {
                    if (lam.restParam != null ? args.size() >= lam.params.size() : args.size() == lam.params.size())
                    { matched = lam; break; }
                }
                if (matched == null) throw new EvalError("case-lambda: no matching clause for " + args.size() + " arguments");
                func = matched;
            }
            if (func instanceof Lambda lam) {
                if (lam.restParam == null) {
                    if (args.size() != lam.params.size()) throw new EvalError("lambda: expected " + lam.params.size() + " arguments, got " + args.size());
                } else {
                    if (args.size() < lam.params.size()) throw new EvalError("lambda: expected at least " + lam.params.size() + " arguments, got " + args.size());
                }
                Env localEnv = new Env(lam.closure);
                for (int i = 0; i < lam.params.size(); i++) localEnv.define(lam.params.get(i), args.get(i));
                if (lam.restParam != null) {
                    Object rest = SchemeValue.NIL;
                    for (int i = args.size() - 1; i >= lam.params.size(); i--) rest = new Pair(args.get(i), rest);
                    localEnv.define(lam.restParam, rest);
                }
                for (int i = 0; i < lam.body.size() - 1; i++) eval(lam.body.get(i), localEnv);
                expr = lam.body.get(lam.body.size() - 1); env = localEnv; continue;
            }
            if (func instanceof ContinuationObj co) {
                windTo(co.windStack);
                throw new ContinuationReturn(args.isEmpty() ? null : args.get(0), co.kont);
            }
            if (func == DYNAMIC_WIND) {
                if (args.size() != 3) throw new EvalError("dynamic-wind: expected 3 arguments");
                Object inTh = args.get(0), bodyTh = args.get(1), outTh = args.get(2);
                applyProc(inTh, List.of());
                DynamicWindEntry entry = new DynamicWindEntry(inTh, outTh);
                WIND_STACK.get().add(entry);
                Object result;
                try { result = applyProc(bodyTh, List.of()); }
                catch (ContinuationReturn cr) {
                    var ws = WIND_STACK.get();
                    int idx2 = ws.lastIndexOf(entry);
                    if (idx2 >= 0) { ws.remove(idx2); applyProc(outTh, List.of()); }
                    throw cr;
                }
                catch (SchemeException se) {
                    var ws = WIND_STACK.get();
                    int idx2 = ws.lastIndexOf(entry);
                    if (idx2 >= 0) { ws.remove(idx2); applyProc(outTh, List.of()); }
                    throw se;
                }
                WIND_STACK.get().remove(WIND_STACK.get().size() - 1);
                applyProc(outTh, List.of());
                return result;
            }
            if (func == CALL_WITH_VALUES) {
                if (args.size() != 2) throw new EvalError("call-with-values: expected 2 arguments");
                Object producer = args.get(0), consumer = args.get(1);
                Object result = applyProc(producer, List.of());
                List<Object> vals;
                if (result instanceof MultipleValues mv) {
                    vals = mv.values;
                } else {
                    vals = List.of(result);
                }
                return applyProc(consumer, vals);
            }
            throw new EvalError("not a procedure: " + SchemeValue.toStr(func));

            } catch (EvalError e) {
                if (list instanceof SourceList sl && !e.getMessage().matches(".*\\d+:\\d+.*")) {
                    throw new EvalError(e.getMessage() + " [" + sl.line + ":" + sl.col + "]");
                }
                throw e;
            }
        }
    }

    // ===== guard special form =====

    @SuppressWarnings("unchecked")
    private static Object evalGuard(List<?> list, Env env) throws EvalError {
        // (guard (var clause ...) body ...)
        if (list.size() < 3) throw new EvalError("guard: bad syntax");
        if (!(list.get(1) instanceof List<?> guardSpec) || guardSpec.isEmpty())
            throw new EvalError("guard: bad syntax");
        if (!(guardSpec.get(0) instanceof String varName))
            throw new EvalError("guard: expected variable name");
        List<List<?>> clauses = new ArrayList<>();
        for (int i = 1; i < guardSpec.size(); i++) {
            if (!(guardSpec.get(i) instanceof List<?> clause))
                throw new EvalError("guard: bad clause");
            clauses.add(clause);
        }
        List<Object> body = new ArrayList<>();
        for (int i = 2; i < list.size(); i++) body.add(list.get(i));
        try {
            Object result = null;
            for (Object expr : body) result = eval(expr, env);
            return result;
        } catch (SchemeException se) {
            Env guardEnv = new Env(env);
            guardEnv.define(varName, se.value);
            for (List<?> clause : clauses) {
                if (clause.isEmpty()) throw new EvalError("guard: bad clause");
                Object test = clause.get(0);
                if (test instanceof String cs && cs.equals("else")) {
                    if (clause.size() == 1) return null;
                    Object result = null;
                    for (int j = 1; j < clause.size(); j++) result = eval(clause.get(j), guardEnv);
                    return result;
                }
                Object testVal = eval(test, guardEnv);
                if (!isFalse(testVal)) {
                    if (clause.size() == 1) return testVal;
                    Object result = null;
                    for (int j = 1; j < clause.size(); j++) result = eval(clause.get(j), guardEnv);
                    return result;
                }
            }
            throw se; // no clause matched, re-raise
        }
    }

    // ===== case special form (used by both CEK and legacy eval) =====

    @SuppressWarnings("unchecked")
    private static Object evalCase(List<?> list, Env env) throws EvalError {
        if (list.size() < 2) throw new EvalError("case: bad syntax");
        Object key = eval(list.get(1), env);
        for (int i = 2; i < list.size(); i++) {
            if (!(list.get(i) instanceof List<?> clause) || clause.isEmpty())
                throw new EvalError("case: bad clause");
            Object datums = clause.get(0);
            if (datums instanceof String cs && cs.equals("else")) {
                if (clause.size() == 1) return null;
                Object result = null;
                for (int j = 1; j < clause.size(); j++) result = eval(clause.get(j), env);
                return result;
            }
            if (!(datums instanceof List<?> datumList)) throw new EvalError("case: bad clause");
            boolean found = false;
            for (Object datum : datumList) {
                Object d = SchemeValue.quotedToScheme(datum);
                if (Env.schemeEqv(key, d)) { found = true; break; }
            }
            if (found) {
                if (clause.size() == 1) return null;
                Object result = null;
                for (int j = 1; j < clause.size(); j++) result = eval(clause.get(j), env);
                return result;
            }
        }
        return null;
    }

    // ===== applyProc (used by builtins: map, for-each, apply) =====

    static Object applyProc(Object func, List<Object> args) throws EvalError {
        if (func instanceof ContinuationObj co) {
            windTo(co.windStack);
            throw new ContinuationReturn(args.isEmpty() ? null : args.get(0), co.kont);
        }
        if (func == DYNAMIC_WIND) {
            if (args.size() != 3) throw new EvalError("dynamic-wind: expected 3 arguments");
            Object inTh = args.get(0), bodyTh = args.get(1), outTh = args.get(2);
            applyProc(inTh, List.of());
            DynamicWindEntry entry = new DynamicWindEntry(inTh, outTh);
            WIND_STACK.get().add(entry);
            Object result;
            try { result = applyProc(bodyTh, List.of()); }
            catch (ContinuationReturn cr) {
                var ws = WIND_STACK.get();
                int idx = ws.lastIndexOf(entry);
                if (idx >= 0) { ws.remove(idx); applyProc(outTh, List.of()); }
                throw cr;
            }
            catch (SchemeException se) {
                var ws = WIND_STACK.get();
                int idx = ws.lastIndexOf(entry);
                if (idx >= 0) { ws.remove(idx); applyProc(outTh, List.of()); }
                throw se;
            }
            WIND_STACK.get().remove(WIND_STACK.get().size() - 1);
            applyProc(outTh, List.of());
            return result;
        }
        if (func == CALL_WITH_VALUES) {
            if (args.size() != 2) throw new EvalError("call-with-values: expected 2 arguments");
            Object producer = args.get(0), consumer = args.get(1);
            Object result = applyProc(producer, List.of());
            List<Object> vals;
            if (result instanceof MultipleValues mv) {
                vals = mv.values;
            } else {
                vals = List.of(result);
            }
            return applyProc(consumer, vals);
        }
        if (func instanceof Builtin b) {
            return b.apply(args);
        }
        if (func instanceof CaseLambda cl) {
            for (Lambda lam : cl.clauses) {
                if (lam.restParam != null ? args.size() >= lam.params.size() : args.size() == lam.params.size())
                    return applyProc(lam, args);
            }
            throw new EvalError("case-lambda: no matching clause for " + args.size() + " arguments");
        }
        if (func instanceof Lambda lam) {
            if (lam.restParam == null) {
                if (args.size() != lam.params.size())
                    throw new EvalError("lambda: expected " + lam.params.size() + " arguments, got " + args.size());
            } else {
                if (args.size() < lam.params.size())
                    throw new EvalError("lambda: expected at least " + lam.params.size() + " arguments, got " + args.size());
            }
            Env localEnv = new Env(lam.closure);
            for (int i = 0; i < lam.params.size(); i++) localEnv.define(lam.params.get(i), args.get(i));
            if (lam.restParam != null) {
                Object rest = SchemeValue.NIL;
                for (int i = args.size() - 1; i >= lam.params.size(); i--) rest = new Pair(args.get(i), rest);
                localEnv.define(lam.restParam, rest);
            }
            Object result = null;
            for (Object bodyExpr : lam.body) result = eval(bodyExpr, localEnv);
            return result;
        }
        throw new EvalError("not a procedure: " + SchemeValue.toStr(func));
    }

    // ===== Helpers =====

    static boolean isFalse(Object val) {
        return val instanceof Boolean b && !b;
    }

    private static Object evalDefine(List<?> list, Env env) throws EvalError {
        if (list.size() < 3) throw new EvalError("define: bad syntax");
        Object target = list.get(1);
        if (target instanceof String name) {
            Object val = eval(list.get(2), env);
            env.define(name, val);
            return null;
        }
        // Function shorthand: (define (name params...) body) or (define (name . rest) body)
        String name = null;
        Object paramSpec = null;
        if (target instanceof List<?> sig) {
            if (sig.isEmpty() || !(sig.get(0) instanceof String n))
                throw new EvalError("define: bad syntax");
            name = n;
            // Build param spec from remaining elements
            List<Object> plist = new ArrayList<>();
            for (int i = 1; i < sig.size(); i++) plist.add(sig.get(i));
            paramSpec = plist;
        } else if (target instanceof Pair p) {
            if (!(p.car instanceof String n)) throw new EvalError("define: bad syntax");
            name = n;
            paramSpec = p.cdr;
        } else {
            throw new EvalError("define: bad syntax");
        }
        String[] parsed = parseParamSpec(paramSpec, "define");
        String restParam = parsed[0].isEmpty() ? null : parsed[0];
        List<String> params = new ArrayList<>();
        for (int i = 1; i < parsed.length; i++) params.add(parsed[i]);
        List<Object> body = new ArrayList<>();
        for (int i = 2; i < list.size(); i++) body.add(list.get(i));
        Lambda lambda = new Lambda(params, restParam, body, env);
        env.define(name, lambda);
        return null;
    }

    // Extract params and restParam from a parameter spec (List or Pair chain)
    private static String[] parseParamSpec(Object paramSpec, String context) throws EvalError {
        List<String> params = new ArrayList<>();
        String restParam = null;
        if (paramSpec instanceof List<?> paramList) {
            for (int j = 0; j < paramList.size(); j++) {
                if (!(paramList.get(j) instanceof String s)) throw new EvalError(context + ": parameter must be symbol");
                if (s.equals(".")) {
                    if (j + 2 != paramList.size()) throw new EvalError(context + ": bad dot syntax");
                    if (!(paramList.get(j + 1) instanceof String rp)) throw new EvalError(context + ": parameter must be symbol");
                    restParam = rp; break;
                }
                params.add(s);
            }
        } else if (paramSpec instanceof Pair) {
            Object cur = paramSpec;
            while (cur instanceof Pair p) {
                if (!(p.car instanceof String s)) throw new EvalError(context + ": parameter must be symbol");
                params.add(s);
                cur = p.cdr;
            }
            if (cur != SchemeValue.NIL && cur != null) {
                if (!(cur instanceof String s)) throw new EvalError(context + ": rest parameter must be symbol");
                restParam = s;
            }
        } else if (paramSpec instanceof String s && !s.startsWith("\"")) {
            // (lambda args body) — single symbol matches all args
            restParam = s;
        } else {
            throw new EvalError(context + ": parameters must be a list");
        }
        // Encode as: [restParam, param1, param2, ...] where restParam may be ""
        String[] result = new String[params.size() + 1];
        result[0] = restParam != null ? restParam : "";
        for (int i = 0; i < params.size(); i++) result[i + 1] = params.get(i);
        return result;
    }

    @SuppressWarnings("unchecked")
    private static Object evalQuasiquote(Object tmpl, Env env, int depth) throws EvalError {
        if (tmpl instanceof List<?> list) {
            if (!list.isEmpty() && list.get(0) instanceof String s) {
                if ("unquote".equals(s)) {
                    if (depth == 0) return eval(list.get(1), env);
                    // Nested unquote — decrease depth
                    List<Object> r = new ArrayList<>();
                    r.add("unquote");
                    r.add(evalQuasiquote(list.get(1), env, depth - 1));
                    return SchemeValue.quotedToScheme(r);
                }
                if ("quasiquote".equals(s)) {
                    List<Object> r = new ArrayList<>();
                    r.add("quasiquote");
                    r.add(evalQuasiquote(list.get(1), env, depth + 1));
                    return SchemeValue.quotedToScheme(r);
                }
            }
            // Process each element, handling unquote-splicing
            List<Object> result = new ArrayList<>();
            for (int i = 0; i < list.size(); i++) {
                Object elem = list.get(i);
                if (elem instanceof List<?> el && el.size() == 2 && "unquote-splicing".equals(el.get(0))) {
                    if (depth == 0) {
                        Object spliced = eval(el.get(1), env);
                        // Splice the result into the list
                        Object cur = spliced;
                        while (cur instanceof Pair p) {
                            result.add(p.car);
                            cur = p.cdr;
                        }
                    } else {
                        List<Object> r = new ArrayList<>();
                        r.add("unquote-splicing");
                        r.add(evalQuasiquote(el.get(1), env, depth - 1));
                        result.add(SchemeValue.quotedToScheme(r));
                    }
                } else {
                    result.add(evalQuasiquote(elem, env, depth));
                }
            }
            // Convert to Scheme list (Pair chain)
            Object schemeList = SchemeValue.NIL;
            for (int i = result.size() - 1; i >= 0; i--) {
                schemeList = new Pair(result.get(i), schemeList);
            }
            return schemeList;
        }
        if (tmpl instanceof Pair p) {
            // Dotted pair in quasiquote template
            if (p.car instanceof String s && "unquote".equals(s) && p.cdr instanceof Pair pc && pc.cdr == SchemeValue.NIL) {
                if (depth == 0) return eval(pc.car, env);
            }
            Object car = evalQuasiquote(p.car, env, depth);
            Object cdr = evalQuasiquote(p.cdr, env, depth);
            // Handle unquote-splicing in car position
            if (p.car instanceof List<?> el && el.size() == 2 && "unquote-splicing".equals(el.get(0)) && depth == 0) {
                Object spliced = eval(el.get(1), env);
                // Append spliced list with cdr
                if (spliced == SchemeValue.NIL) return cdr;
                // Find tail of spliced and attach cdr
                Object result = spliced;
                Object tail = spliced;
                while (tail instanceof Pair tp && tp.cdr instanceof Pair) tail = tp.cdr;
                if (tail instanceof Pair tp) tp.cdr = cdr;
                return result;
            }
            return new Pair(car, cdr);
        }
        // Atom — return as Scheme value
        return SchemeValue.quotedToScheme(tmpl);
    }

    private static Object evalLambda(List<?> list, Env env) throws EvalError {
        if (list.size() < 3) throw new EvalError("lambda: bad syntax");
        String[] parsed = parseParamSpec(list.get(1), "lambda");
        String restParam = parsed[0].isEmpty() ? null : parsed[0];
        List<String> params = new ArrayList<>();
        for (int i = 1; i < parsed.length; i++) params.add(parsed[i]);
        List<Object> body = new ArrayList<>();
        for (int i = 2; i < list.size(); i++) body.add(list.get(i));
        return new Lambda(params, restParam, body, env);
    }

    @SuppressWarnings("unchecked")
    private static Object evalDefineRecordType(List<?> list, Env env) throws EvalError {
        if (list.size() < 4) throw new EvalError("define-record-type: bad syntax");
        if (!(list.get(1) instanceof String typeName))
            throw new EvalError("define-record-type: expected type name");
        if (!(list.get(2) instanceof List<?> ctorSpec) || ctorSpec.isEmpty())
            throw new EvalError("define-record-type: expected constructor spec");
        String ctorName = (String) ctorSpec.get(0);
        List<String> ctorFields = new ArrayList<>();
        for (int i = 1; i < ctorSpec.size(); i++) ctorFields.add((String) ctorSpec.get(i));
        if (!(list.get(3) instanceof String predName))
            throw new EvalError("define-record-type: expected predicate name");
        List<String> fieldNames = new ArrayList<>();
        List<String> accessorNames = new ArrayList<>();
        for (int i = 4; i < list.size(); i++) {
            if (!(list.get(i) instanceof List<?> fieldSpec) || fieldSpec.size() != 2)
                throw new EvalError("define-record-type: bad field spec");
            fieldNames.add((String) fieldSpec.get(0));
            accessorNames.add((String) fieldSpec.get(1));
        }
        Record.RecordType recordType = new Record.RecordType(typeName, ctorFields);
        env.define(ctorName, Builtin.named(ctorName, args -> {
            if (args.size() != ctorFields.size())
                throw new EvalError(ctorName + ": expected " + ctorFields.size() + " arguments, got " + args.size());
            return new Record(recordType, args.toArray());
        }));
        env.define(predName, Builtin.named(predName, args -> {
            if (args.size() != 1) throw new EvalError(predName + ": expected 1 argument");
            return args.get(0) instanceof Record r && r.type == recordType;
        }));
        for (int i = 0; i < fieldNames.size(); i++) {
            String fieldName = fieldNames.get(i);
            String accessorName = accessorNames.get(i);
            int fieldIndex = ctorFields.indexOf(fieldName);
            if (fieldIndex < 0) throw new EvalError("define-record-type: field " + fieldName + " not in constructor");
            env.define(accessorName, Builtin.named(accessorName, args -> {
                if (args.size() != 1) throw new EvalError(accessorName + ": expected 1 argument");
                if (!(args.get(0) instanceof Record r) || r.type != recordType)
                    throw new EvalError(accessorName + ": not a " + typeName);
                return r.fields[fieldIndex];
            }));
        }
        return null;
    }

    private static Object evalCaseLambda(List<?> list, Env env) throws EvalError {
        List<Lambda> clauses = new ArrayList<>();
        for (int i = 1; i < list.size(); i++) {
            if (!(list.get(i) instanceof List<?> clause) || clause.size() < 2)
                throw new EvalError("case-lambda: bad clause");
            String[] parsed = parseParamSpec(clause.get(0), "case-lambda");
            String restParam = parsed[0].isEmpty() ? null : parsed[0];
            List<String> params = new ArrayList<>();
            for (int j = 1; j < parsed.length; j++) params.add(parsed[j]);
            List<Object> body = new ArrayList<>();
            for (int j = 1; j < clause.size(); j++) body.add(clause.get(j));
            clauses.add(new Lambda(params, restParam, body, env));
        }
        return new CaseLambda(clauses);
    }

    @SuppressWarnings("unchecked")
    private static Object evalDo(List<?> list, Env env) throws EvalError {
        if (list.size() < 3) throw new EvalError("do: bad syntax");
        if (!(list.get(1) instanceof List<?> varSpecs))
            throw new EvalError("do: variable specs must be a list");
        if (!(list.get(2) instanceof List<?> testClause) || testClause.isEmpty())
            throw new EvalError("do: test clause must be a non-empty list");
        int numVars = varSpecs.size();
        String[] names = new String[numVars];
        Object[] stepExprs = new Object[numVars];
        boolean[] hasStep = new boolean[numVars];
        Env doEnv = new Env(env);
        for (int i = 0; i < numVars; i++) {
            if (!(varSpecs.get(i) instanceof List<?> spec) || spec.size() < 2 || spec.size() > 3)
                throw new EvalError("do: bad variable spec");
            if (!(spec.get(0) instanceof String name)) throw new EvalError("do: variable name must be symbol");
            names[i] = name;
            doEnv.define(name, eval(spec.get(1), env));
            if (spec.size() == 3) { stepExprs[i] = spec.get(2); hasStep[i] = true; }
        }
        while (true) {
            Object testResult = eval(testClause.get(0), doEnv);
            if (!isFalse(testResult)) {
                if (testClause.size() == 1) return null;
                Object result = null;
                for (int j = 1; j < testClause.size(); j++) result = eval(testClause.get(j), doEnv);
                return result;
            }
            for (int i = 3; i < list.size(); i++) eval(list.get(i), doEnv);
            Object[] newVals = new Object[numVars];
            for (int i = 0; i < numVars; i++) {
                if (hasStep[i]) newVals[i] = eval(stepExprs[i], doEnv);
            }
            for (int i = 0; i < numVars; i++) {
                if (hasStep[i]) doEnv.set(names[i], newVals[i]);
            }
        }
    }
}
