package ming;

import static ming.Numbers.*;
import static ming.SchemeFormatter.*;

import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;

public class Evaluator {

    static final Object EMPTY_LIST = new Object() {
        @Override public String toString() { return "()"; }
    };

    static class Env {
        final Map<String, Object> bindings = new HashMap<>();
        final Env parent;
        Env(Env parent) { this.parent = parent; }
        Object lookup(String name, SchemeParser.Pos pos) throws EvalError {
            if (bindings.containsKey(name)) return bindings.get(name);
            if (parent != null) return parent.lookup(name, pos);
            throw new EvalError("unbound variable: " + name + " at " + pos.fmt());
        }
        void define(String name, Object val) { bindings.put(name, val); }
        void set(String name, Object val, SchemeParser.Pos pos) throws EvalError {
            if (bindings.containsKey(name)) { bindings.put(name, val); return; }
            if (parent != null) { parent.set(name, val, pos); return; }
            throw new EvalError("set!: unbound variable: " + name + " at " + pos.fmt());
        }
    }

    static class Pair {
        Object car, cdr;
        Pair(Object car, Object cdr) { this.car = car; this.cdr = cdr; }
    }

    record Lambda(List<String> params, String restParam, List<Object> body, Env closureEnv) {}
    record CaseLambda(List<Lambda> clauses) {}
    @FunctionalInterface
    interface Builtin { Object apply(List<Object> args) throws EvalError; }
    record BuiltinProc(String name, Builtin fn) {}
    record DynamicWindEntry(Object inThunk, Object outThunk) {}

    private static class CekState {
        Object current;
        Env env;
        boolean evaluating;
        CekState(Object current, Env env, boolean evaluating) {
            this.current = current; this.env = env; this.evaluating = evaluating;
        }
    }

    // --- Continuation support ---
    static final Object CALLCC_PROC = new Object() {
        @Override public String toString() { return "#<procedure call/cc>"; }
    };
    static final Object DYNAMIC_WIND_PROC = new Object() {
        @Override public String toString() { return "#<procedure dynamic-wind>"; }
    };
    static final Object RAISE_PROC = new Object() {
        @Override public String toString() { return "#<procedure raise>"; }
    };
    static final Object WITH_EXCEPTION_HANDLER_PROC = new Object() {
        @Override public String toString() { return "#<procedure with-exception-handler>"; }
    };
    static final Object VALUES_PROC = new Object() {
        @Override public String toString() { return "#<procedure values>"; }
    };
    static final Object CALL_WITH_VALUES_PROC = new Object() {
        @Override public String toString() { return "#<procedure call-with-values>"; }
    };

    static class MultipleValues {
        final List<Object> values;
        MultipleValues(List<Object> values) { this.values = values; }
    }

    static class SchemeRaise extends RuntimeException {
        final Object value;
        SchemeRaise(Object v) { super(null, null, true, false); value = v; }
    }

    static class GuardHandler {
        final List<Object> savedKont;
        final List<DynamicWindEntry> savedWindStack;
        final List<Object> clauses;
        final String varName;
        final Env guardEnv;
        final List<Object> savedExceptionHandlers;
        GuardHandler(List<Object> kont, List<DynamicWindEntry> ws, List<Object> clauses,
                     String var, Env env, List<Object> exHandlers) {
            this.savedKont = new ArrayList<>(kont);
            this.savedWindStack = new ArrayList<>(ws);
            this.clauses = clauses;
            this.varName = var;
            this.guardEnv = env;
            this.savedExceptionHandlers = new ArrayList<>(exHandlers);
        }
    }

    static class Continuation {
        final List<Object> savedKont;
        final int evalId;
        final List<DynamicWindEntry> savedWindStack;
        final List<Object> savedExceptionHandlers;
        Continuation(List<Object> kont, int evalId, List<DynamicWindEntry> windStack, List<Object> exHandlers) {
            this.savedKont = new ArrayList<>(kont);
            this.evalId = evalId;
            this.savedWindStack = new ArrayList<>(windStack);
            this.savedExceptionHandlers = new ArrayList<>(exHandlers);
        }
    }

    static class ContinuationInvoked extends RuntimeException {
        final Continuation cont;
        final Object value;
        ContinuationInvoked(Continuation c, Object v) {
            super(null, null, true, false);
            cont = c; value = v;
        }
    }

    // --- CEK Continuation Frames ---
    private record IfFrame(List<?> list, Env env) {}
    private record DefineFrame(String name, Env env) {}
    private record SetFrame(String name, Env env, SchemeParser.Pos pos) {}
    private record BeginFrame(List<?> exprs, int nextIdx, Env env) {}
    private record EvalOpFrame(List<?> list, Env env, int eLine, int eCol) {}
    private record EvalArgsFrame(Object proc, List<?> list, int nextArgIdx,
                                  List<Object> evaluated, Env env, int eLine, int eCol) {}
    private record AndFrame(List<?> list, int nextIdx, Env env) {}
    private record OrFrame(List<?> list, int nextIdx, Env env) {}
    private record CondFrame(List<?> list, int clauseIdx, Env env) {}
    private record CaseFrame(List<?> list, Env env) {}
    private record LetBindFrame(List<String> names, List<Object> initExprs, int bindingIdx,
                                 List<Object> values, List<Object> bodyExprs,
                                 Env outerEnv, String namedLetName) {}
    private record LetStarBindFrame(List<?> bindingList, int bindingIdx,
                                     List<Object> bodyExprs, Env letEnv) {}
    private record LetrecBindFrame(List<String> names, List<Object> initExprs, int bindingIdx,
                                    List<Object> values, List<Object> bodyExprs,
                                    Env letrecEnv, boolean isStar) {}
    private record DWAfterInFrame(Object inThunk, Object bodyThunk, Object outThunk) {}
    private record DWAfterBodyFrame(Object inThunk, Object outThunk) {}
    private record DWAfterOutFrame(Object bodyValue) {}
    private record GuardAfterFrame() {}
    private record WEHAfterFrame() {}
    private record RaiseReturnFrame() {}
    private record CallWithValuesFrame(Object consumer) {}
    private record SyntaxCaseFrame(List<String> literals, List<Object> clauses, Env env) {}
    private record WithSyntaxFrame(List<?> bindings, int index, List<Object> values,
                                    List<Object> bodyExprs, Env env) {}

    record SyntaxCaseTransformer(Object proc, Env defEnv) {}
    record SyntaxTemplate(Object form, Map<String, Object> hygieneBindings) {}

    private static final Set<String> SPECIAL_FORMS = Set.of(
        "if", "begin", "let", "let*", "set!", "define", "quote", "lambda", "case-lambda",
        "and", "or", "cond", "case", "do", "letrec", "letrec*",
        "define-syntax", "syntax-rules", "syntax-case", "syntax", "with-syntax",
        "define-record-type", "guard"
    );

    private int gensymCounter = 0;
    private int nextEvalId = 0;
    private StringBuilder outputBuffer;
    private Env syntaxDefEnv;
    private final List<DynamicWindEntry> windStack = new ArrayList<>();
    private final List<Object> exceptionHandlers = new ArrayList<>();

    public String evalStr(String input) throws EvalError {
        outputBuffer = new StringBuilder();
        List<SchemeParser.Token> tokens = SchemeParser.tokenize(input);
        int[] pos = {0};
        List<Object> forms = new ArrayList<>();
        while (pos[0] < tokens.size()) forms.add(SchemeParser.parse(tokens, pos));
        if (forms.isEmpty()) throw new EvalError("no expression");
        Env env = createGlobalEnv();
        Object result;
        if (forms.size() == 1) { result = eval(forms.get(0), env); }
        else {
            List<Object> bf = new ArrayList<>(); bf.add("begin"); bf.addAll(forms);
            result = eval(bf, env);
        }
        return schemeToString(result);
    }

    public EvalResult evalStrWithOutput(String input) throws EvalError {
        outputBuffer = new StringBuilder();
        List<SchemeParser.Token> tokens = SchemeParser.tokenize(input);
        int[] pos = {0};
        List<Object> forms = new ArrayList<>();
        while (pos[0] < tokens.size()) forms.add(SchemeParser.parse(tokens, pos));
        if (forms.isEmpty()) throw new EvalError("no expression");
        Env env = createGlobalEnv();
        Object result;
        if (forms.size() == 1) { result = eval(forms.get(0), env); }
        else {
            List<Object> bf = new ArrayList<>(); bf.add("begin"); bf.addAll(forms);
            result = eval(bf, env);
        }
        return new EvalResult(schemeToString(result), outputBuffer.toString());
    }

    private Env createGlobalEnv() {
        Env env = new Env(null);
        Builtins.registerAll(env, outputBuffer, this::applyProc);
        env.define("call/cc", CALLCC_PROC);
        env.define("call-with-current-continuation", CALLCC_PROC);
        env.define("dynamic-wind", DYNAMIC_WIND_PROC);
        env.define("raise", RAISE_PROC);
        env.define("with-exception-handler", WITH_EXCEPTION_HANDLER_PROC);
        env.define("values", VALUES_PROC);
        env.define("call-with-values", CALL_WITH_VALUES_PROC);
        env.define("syntax->datum", new BuiltinProc("syntax->datum", args -> {
            if (args.size() != 1) throw new EvalError("syntax->datum: expected 1 argument");
            Object v = args.get(0);
            if (v instanceof SyntaxObject so) {
                Object d = so.datum;
                if (d instanceof SchemeParser.Located loc) d = loc.expr;
                return d;
            }
            return v;
        }));
        env.define("datum->syntax", new BuiltinProc("datum->syntax", args -> {
            if (args.size() != 2) throw new EvalError("datum->syntax: expected 2 arguments");
            return new SyntaxObject(args.get(1));
        }));
        env.define("procedure?", new BuiltinProc("procedure?", args -> {
            if (args.size() != 1) throw new EvalError("procedure?: expected 1 arg");
            Object v = args.get(0);
            return (v instanceof Lambda || v instanceof CaseLambda || v instanceof BuiltinProc
                    || v instanceof Continuation || v == CALLCC_PROC
                    || v == DYNAMIC_WIND_PROC || v == RAISE_PROC
                    || v == WITH_EXCEPTION_HANDLER_PROC
                    || v == VALUES_PROC || v == CALL_WITH_VALUES_PROC) ? Boolean.TRUE : Boolean.FALSE;
        }));
        return env;
    }

    // --- CEK Machine ---

    @SuppressWarnings("unchecked")
    private Object eval(Object startExpr, Env startEnv) throws EvalError {
        final int evalId = nextEvalId++;
        boolean evaluating = true;
        Object current = startExpr;
        Env env = startEnv;
        List<Object> kont = new ArrayList<>();

        while (true) {
          try {
            if (evaluating) {
                int eLine = 0, eCol = 0;
                if (current instanceof SchemeParser.Located loc) {
                    eLine = loc.line; eCol = loc.col; current = loc.expr;
                }
                String posStr = eLine > 0 ? " at " + eLine + ":" + eCol : "";

                if (current instanceof Long || current instanceof Double || current instanceof Rational
                        || current instanceof Boolean || current instanceof SchemeString
                        || current instanceof SchemeChar) {
                    evaluating = false; continue;
                }
                if (current instanceof String sym) {
                    current = env.lookup(sym, new SchemeParser.Pos(eLine, eCol));
                    evaluating = false; continue;
                }
                if (!(current instanceof List<?> list)) throw new EvalError("cannot evaluate: " + current + posStr);
                if (list.isEmpty()) throw new EvalError("empty application" + posStr);

                Object rawHead = list.get(0);
                if (rawHead instanceof SchemeParser.Located loc) rawHead = loc.expr;

                if (rawHead instanceof String op) {
                    CekState sf = dispatchSpecialForm(op, list, env, kont, eLine, eCol, posStr);
                    if (sf != null) {
                        current = sf.current; env = sf.env; evaluating = sf.evaluating;
                        continue;
                    }
                }
                // Application
                kont.add(new EvalOpFrame(list, env, eLine, eCol));
                current = list.get(0); continue;

            } else {
                // --- CONTINUE phase ---
                if (kont.isEmpty()) return current;
                Object frame = kont.remove(kont.size() - 1);
                CekState state = processFrame(frame, current, env, kont, evalId);
                current = state.current;
                env = state.env;
                evaluating = state.evaluating;
                continue;
            }
          } catch (ContinuationInvoked ci) {
              if (ci.cont.evalId == evalId) {
                  performWindTransition(ci.cont.savedWindStack);
                  kont.clear(); kont.addAll(ci.cont.savedKont);
                  exceptionHandlers.clear(); exceptionHandlers.addAll(ci.cont.savedExceptionHandlers);
                  current = ci.value; evaluating = false;
              } else throw ci;
          }
        }
    }

    @SuppressWarnings("unchecked")
    private CekState dispatchSpecialForm(String op, List<?> list, Env env,
                                          List<Object> kont, int eLine, int eCol,
                                          String posStr) throws EvalError {
        switch (op) {
        case "quote": {
            if (list.size() != 2) throw new EvalError("quote: expected 1 argument" + posStr);
            Object d = list.get(1);
            if (d instanceof SchemeParser.Located loc) d = loc.expr;
            return new CekState(quoteValue(d), env, false);
        }
        case "if": {
            if (list.size() < 3 || list.size() > 4) throw new EvalError("if: expected 2-3 arguments" + posStr);
            kont.add(new IfFrame(list, env));
            return new CekState(list.get(1), env, true);
        }
        case "define": {
            if (list.size() < 3) throw new EvalError("define: too few arguments" + posStr);
            Object target = list.get(1);
            if (target instanceof SchemeParser.Located loc) target = loc.expr;
            if (target instanceof String name) {
                kont.add(new DefineFrame(name, env));
                return new CekState(list.get(2), env, true);
            } else if (target instanceof List<?> sig) {
                if (sig.isEmpty()) throw new EvalError("define: invalid function signature");
                Object first = sig.get(0);
                if (first instanceof SchemeParser.Located loc) first = loc.expr;
                if (!(first instanceof String fname)) throw new EvalError("define: invalid function signature");
                List<String> params = new ArrayList<>();
                String restParam = parseParamList(sig, 1, params, "define");
                List<Object> body = new ArrayList<>(list.subList(2, list.size()));
                Lambda lambda = new Lambda(params, restParam, body, env);
                env.define(fname, lambda);
                return new CekState(lambda, env, false);
            } else throw new EvalError("define: invalid syntax");
        }
        case "set!": {
            if (list.size() != 3) throw new EvalError("set!: expected 2 arguments" + posStr);
            Object tgt = list.get(1);
            if (tgt instanceof SchemeParser.Located loc) tgt = loc.expr;
            if (!(tgt instanceof String name)) throw new EvalError("set!: target must be a symbol");
            kont.add(new SetFrame(name, env, new SchemeParser.Pos(eLine, eCol)));
            return new CekState(list.get(2), env, true);
        }
        case "lambda":
            return new CekState(makeLambda(list, env), env, false);
        case "case-lambda":
            return new CekState(makeCaseLambda(list, env), env, false);
        case "begin": {
            if (list.size() == 1) return new CekState(Boolean.FALSE, env, false);
            if (list.size() > 2) kont.add(new BeginFrame(list, 2, env));
            return new CekState(list.get(1), env, true);
        }
        case "and": {
            if (list.size() == 1) return new CekState(Boolean.TRUE, env, false);
            if (list.size() == 2) return new CekState(list.get(1), env, true);
            kont.add(new AndFrame(list, 2, env));
            return new CekState(list.get(1), env, true);
        }
        case "or": {
            if (list.size() == 1) return new CekState(Boolean.FALSE, env, false);
            if (list.size() == 2) return new CekState(list.get(1), env, true);
            kont.add(new OrFrame(list, 2, env));
            return new CekState(list.get(1), env, true);
        }
        case "cond": {
            for (int i = 1; i < list.size(); i++) {
                Object co = unwrap(list.get(i));
                if (!(co instanceof List<?> clause) || clause.isEmpty())
                    throw new EvalError("cond: invalid clause");
                Object test = unwrap(clause.get(0));
                if (test instanceof String s && s.equals("else")) {
                    if (clause.size() == 1) return new CekState(Boolean.TRUE, env, false);
                    Object first = pushBodyFrames(clause, 1, kont, env);
                    return new CekState(first, env, true);
                }
                kont.add(new CondFrame(list, i, env));
                return new CekState(clause.get(0), env, true);
            }
            return new CekState(Boolean.FALSE, env, false);
        }
        case "case": {
            if (list.size() < 3) throw new EvalError("case: too few arguments");
            kont.add(new CaseFrame(list, env));
            return new CekState(list.get(1), env, true);
        }
        case "let": {
            int bidx = 1; String namedLetName = null;
            Object fa = list.get(1);
            if (fa instanceof SchemeParser.Located loc) fa = loc.expr;
            if (fa instanceof String nm) { namedLetName = nm; bidx = 2; }
            Object bo = list.get(bidx);
            if (bo instanceof SchemeParser.Located loc) bo = loc.expr;
            if (!(bo instanceof List<?> bl)) throw new EvalError("let: bindings must be a list");
            List<String> names = new ArrayList<>(); List<Object> inits = new ArrayList<>();
            parseBindings(bl, names, inits, "let");
            List<Object> bodyExprs = new ArrayList<>(list.subList(bidx + 1, list.size()));
            if (inits.isEmpty()) {
                Env le = new Env(env);
                if (namedLetName != null) {
                    Lambda loop = new Lambda(names, null, bodyExprs, le);
                    le.define(namedLetName, loop);
                }
                Object first = pushBodyFrames(bodyExprs, kont, le);
                if (first == null) return new CekState(Boolean.FALSE, le, false);
                return new CekState(first, le, true);
            } else {
                kont.add(new LetBindFrame(names, inits, 0, new ArrayList<>(),
                        bodyExprs, env, namedLetName));
                return new CekState(inits.get(0), env, true);
            }
        }
        case "let*": {
            Object bo = list.get(1);
            if (bo instanceof SchemeParser.Located loc) bo = loc.expr;
            if (!(bo instanceof List<?> bl)) throw new EvalError("let*: bindings must be a list");
            List<Object> bodyExprs = new ArrayList<>(list.subList(2, list.size()));
            Env le = new Env(env);
            if (bl.isEmpty()) {
                Object first = pushBodyFrames(bodyExprs, kont, le);
                if (first == null) return new CekState(Boolean.FALSE, le, false);
                return new CekState(first, le, true);
            } else {
                kont.add(new LetStarBindFrame(bl, 0, bodyExprs, le));
                Object fb = unwrap(bl.get(0));
                return new CekState(((List<?>) fb).get(1), le, true);
            }
        }
        case "letrec": case "letrec*": {
            boolean isStar = op.equals("letrec*");
            Object bo = list.get(1);
            if (bo instanceof SchemeParser.Located loc) bo = loc.expr;
            if (!(bo instanceof List<?> bl)) throw new EvalError(op + ": bindings must be a list");
            List<String> names = new ArrayList<>(); List<Object> inits = new ArrayList<>();
            parseBindings(bl, names, inits, op);
            List<Object> bodyExprs = new ArrayList<>(list.subList(2, list.size()));
            Env le = new Env(env);
            for (String n : names) le.define(n, Boolean.FALSE);
            if (inits.isEmpty()) {
                Object first = pushBodyFrames(bodyExprs, kont, le);
                if (first == null) return new CekState(Boolean.FALSE, le, false);
                return new CekState(first, le, true);
            } else {
                kont.add(new LetrecBindFrame(names, inits, 0, new ArrayList<>(),
                        bodyExprs, le, isStar));
                return new CekState(inits.get(0), le, true);
            }
        }
        case "do":
            return new CekState(desugarDo(list), env, true);
        case "define-syntax":
            return new CekState(handleDefineSyntax(list, env), env, false);
        case "define-record-type":
            return new CekState(handleDefineRecordType(list, env), env, false);
        case "guard": {
            Object gs = unwrap(list.get(1));
            if (!(gs instanceof List<?> guardSpec) || guardSpec.isEmpty())
                throw new EvalError("guard: invalid syntax");
            String var = (String) unwrap(guardSpec.get(0));
            List<Object> clauses = new ArrayList<>();
            boolean hasElse = false;
            for (int ci = 1; ci < guardSpec.size(); ci++) {
                clauses.add(guardSpec.get(ci));
                Object cl = unwrap(guardSpec.get(ci));
                if (cl instanceof List<?> clList && !clList.isEmpty()) {
                    Object test = unwrap(clList.get(0));
                    if (test instanceof String s && s.equals("else")) hasElse = true;
                }
            }
            if (!hasElse) {
                List<Object> raiseExpr = List.of("raise", var);
                clauses.add(List.of("else", raiseExpr));
            }
            GuardHandler gh = new GuardHandler(kont, windStack, clauses, var, env, exceptionHandlers);
            exceptionHandlers.add(gh);
            kont.add(new GuardAfterFrame());
            if (list.size() == 2) return new CekState(Boolean.FALSE, env, false);
            else if (list.size() == 3) return new CekState(list.get(2), env, true);
            else {
                List<Object> bodyExprs = new ArrayList<>(list.subList(2, list.size()));
                return new CekState(pushBodyFrames(bodyExprs, kont, env), env, true);
            }
        }
        case "syntax-case": {
            if (list.size() < 4) throw new EvalError("syntax-case: too few arguments" + posStr);
            Object litsObj = unwrap(list.get(2));
            List<String> scLits = new ArrayList<>();
            if (litsObj instanceof List<?> ll) {
                for (Object l : ll) { l = unwrap(l); if (l instanceof String s) scLits.add(s); }
            }
            List<Object> scClauses = new ArrayList<>();
            for (int ci = 3; ci < list.size(); ci++) scClauses.add(list.get(ci));
            kont.add(new SyntaxCaseFrame(scLits, scClauses, env));
            return new CekState(list.get(1), env, true);
        }
        case "syntax": {
            if (list.size() != 2) throw new EvalError("syntax: expected 1 argument" + posStr);
            Object rawTmpl = unwrap(list.get(1));
            if (rawTmpl instanceof String id) {
                Object val = null;
                try { val = env.lookup(id, new SchemeParser.Pos(eLine, eCol)); } catch (EvalError ignored) {}
                if (val instanceof SyntaxObject) return new CekState(val, env, false);
            }
            Map<String, String> renameMap = new HashMap<>();
            Map<String, Object> hygieneBindings = new HashMap<>();
            Object expanded = expandSyntaxTemplate(list.get(1), env, renameMap, hygieneBindings);
            return new CekState(new SyntaxTemplate(expanded, hygieneBindings), env, false);
        }
        case "with-syntax": {
            if (list.size() < 3) throw new EvalError("with-syntax: too few arguments" + posStr);
            Object bindingsObj = unwrap(list.get(1));
            if (!(bindingsObj instanceof List<?> wsbl)) throw new EvalError("with-syntax: bindings must be a list");
            List<Object> wsBody = new ArrayList<>(list.subList(2, list.size()));
            if (wsbl.isEmpty()) {
                Object first = pushBodyFrames(wsBody, kont, env);
                if (first == null) return new CekState(Boolean.FALSE, env, false);
                return new CekState(first, env, true);
            }
            kont.add(new WithSyntaxFrame(wsbl, 0, new ArrayList<>(), wsBody, env));
            List<?> fb = (List<?>) unwrap(wsbl.get(0));
            return new CekState(fb.get(1), env, true);
        }
        default: {
            Object mv = null;
            try { mv = env.lookup(op, new SchemeParser.Pos(eLine, eCol)); } catch (EvalError ignored) {}
            if (mv instanceof SyntaxRulesMacro macro) {
                Object[] expanded = expandMacro(macro, list, env);
                return new CekState(expanded[0], (Env) expanded[1], true);
            }
            if (mv instanceof SyntaxCaseTransformer sct) {
                Env oldDefEnv = syntaxDefEnv;
                syntaxDefEnv = sct.defEnv;
                SyntaxObject stx = new SyntaxObject(list);
                Object result = applyProc(sct.proc, List.of(stx));
                syntaxDefEnv = oldDefEnv;
                if (result instanceof SyntaxTemplate st) {
                    if (!st.hygieneBindings.isEmpty()) {
                        for (var e : st.hygieneBindings.entrySet()) env.define(e.getKey(), e.getValue());
                    }
                    return new CekState(st.form, env, true);
                }
                if (result instanceof SyntaxObject so) return new CekState(so.datum, env, true);
                return new CekState(result, env, true);
            }
            return null;
        }
        }
    }

    @SuppressWarnings("unchecked")
    private CekState processFrame(Object frame, Object current, Env env,
                                   List<Object> kont, int evalId) throws EvalError {
        if (frame instanceof IfFrame f) {
            if (isTruthy(current)) return new CekState(f.list.get(2), f.env, true);
            else if (f.list.size() == 4) return new CekState(f.list.get(3), f.env, true);
            else return new CekState(Boolean.FALSE, env, false);
        }
        if (frame instanceof DefineFrame f) {
            f.env.define(f.name, current);
            return new CekState(current, env, false);
        }
        if (frame instanceof SetFrame f) {
            f.env.set(f.name, current, f.pos);
            return new CekState(current, env, false);
        }
        if (frame instanceof BeginFrame f) {
            if (f.nextIdx >= f.exprs.size() - 1) {
                return new CekState(f.exprs.get(f.exprs.size() - 1), f.env, true);
            } else {
                kont.add(new BeginFrame(f.exprs, f.nextIdx + 1, f.env));
                return new CekState(f.exprs.get(f.nextIdx), f.env, true);
            }
        }
        if (frame instanceof EvalOpFrame f) {
            Object proc = current;
            if (f.list.size() == 1) {
                doApply(proc, new ArrayList<>(), kont, f.eLine, f.eCol, evalId);
                Env newEnv = applyResult[1] != null ? (Env) applyResult[1] : env;
                return new CekState(applyResult[0], newEnv, (Boolean) applyResult[2]);
            }
            int last = f.list.size() - 1;
            kont.add(new EvalArgsFrame(proc, f.list, last - 1, new ArrayList<>(), f.env, f.eLine, f.eCol));
            return new CekState(f.list.get(last), f.env, true);
        }
        if (frame instanceof EvalArgsFrame f) {
            List<Object> newEval = new ArrayList<>(f.evaluated);
            newEval.add(current);
            if (f.nextArgIdx >= 1) {
                kont.add(new EvalArgsFrame(f.proc, f.list, f.nextArgIdx - 1,
                        newEval, f.env, f.eLine, f.eCol));
                return new CekState(f.list.get(f.nextArgIdx), f.env, true);
            } else {
                Collections.reverse(newEval);
                doApply(f.proc, newEval, kont, f.eLine, f.eCol, evalId);
                Env newEnv = applyResult[1] != null ? (Env) applyResult[1] : env;
                return new CekState(applyResult[0], newEnv, (Boolean) applyResult[2]);
            }
        }
        if (frame instanceof AndFrame f) {
            if (!isTruthy(current)) return new CekState(current, env, false);
            else if (f.nextIdx >= f.list.size() - 1) {
                return new CekState(f.list.get(f.list.size() - 1), f.env, true);
            } else {
                kont.add(new AndFrame(f.list, f.nextIdx + 1, f.env));
                return new CekState(f.list.get(f.nextIdx), f.env, true);
            }
        }
        if (frame instanceof OrFrame f) {
            if (isTruthy(current)) return new CekState(current, env, false);
            else if (f.nextIdx >= f.list.size() - 1) {
                return new CekState(f.list.get(f.list.size() - 1), f.env, true);
            } else {
                kont.add(new OrFrame(f.list, f.nextIdx + 1, f.env));
                return new CekState(f.list.get(f.nextIdx), f.env, true);
            }
        }
        if (frame instanceof CondFrame f) {
            return processCondFrame(f, current, env, kont);
        }
        if (frame instanceof CaseFrame f) {
            return processCaseFrame(f, current, kont);
        }
        if (frame instanceof LetBindFrame f) {
            return processLetBindFrame(f, current, kont);
        }
        if (frame instanceof LetStarBindFrame f) {
            return processLetStarBindFrame(f, current, kont);
        }
        if (frame instanceof LetrecBindFrame f) {
            return processLetrecBindFrame(f, current, kont);
        }
        if (frame instanceof DWAfterInFrame f) {
            windStack.add(new DynamicWindEntry(f.inThunk, f.outThunk));
            kont.add(new DWAfterBodyFrame(f.inThunk, f.outThunk));
            doApply(f.bodyThunk, List.of(), kont, 0, 0, evalId);
            Env newEnv = applyResult[1] != null ? (Env) applyResult[1] : env;
            return new CekState(applyResult[0], newEnv, (Boolean) applyResult[2]);
        }
        if (frame instanceof DWAfterBodyFrame f) {
            windStack.remove(windStack.size() - 1);
            kont.add(new DWAfterOutFrame(current));
            doApply(f.outThunk, List.of(), kont, 0, 0, evalId);
            Env newEnv = applyResult[1] != null ? (Env) applyResult[1] : env;
            return new CekState(applyResult[0], newEnv, (Boolean) applyResult[2]);
        }
        if (frame instanceof DWAfterOutFrame f) {
            return new CekState(f.bodyValue, env, false);
        }
        if (frame instanceof GuardAfterFrame) {
            // Body completed normally, pop the guard handler
            exceptionHandlers.remove(exceptionHandlers.size() - 1);
            return new CekState(current, env, false);
        }
        if (frame instanceof WEHAfterFrame) {
            // Thunk completed normally, pop the exception handler
            exceptionHandlers.remove(exceptionHandlers.size() - 1);
            return new CekState(current, env, false);
        }
        if (frame instanceof CallWithValuesFrame f) {
            List<Object> vals;
            if (current instanceof MultipleValues mv) {
                vals = mv.values;
            } else {
                vals = List.of(current);
            }
            doApply(f.consumer, vals, kont, 0, 0, evalId);
            Env newEnv = applyResult[1] != null ? (Env) applyResult[1] : env;
            return new CekState(applyResult[0], newEnv, (Boolean) applyResult[2]);
        }
        if (frame instanceof SyntaxCaseFrame f) {
            return processSyntaxCaseFrame(f, current, env);
        }
        if (frame instanceof WithSyntaxFrame f) {
            return processWithSyntaxFrame(f, current, env, kont);
        }
        if (frame instanceof RaiseReturnFrame) {
            throw new EvalError("raise: handler returned");
        }
        throw new EvalError("unknown frame: " + frame.getClass().getSimpleName());
    }

    private CekState processCondFrame(CondFrame f, Object current, Env env,
                                       List<Object> kont) throws EvalError {
        List<?> clause = (List<?>) unwrap(f.list.get(f.clauseIdx));
        if (isTruthy(current)) {
            if (clause.size() == 1) return new CekState(current, env, false);
            Object first = pushBodyFrames(clause, 1, kont, f.env);
            return new CekState(first, f.env, true);
        }
        for (int i = f.clauseIdx + 1; i < f.list.size(); i++) {
            List<?> nc = (List<?>) unwrap(f.list.get(i));
            Object nt = unwrap(nc.get(0));
            if (nt instanceof String s && s.equals("else")) {
                if (nc.size() == 1) return new CekState(Boolean.TRUE, f.env, false);
                Object first = pushBodyFrames(nc, 1, kont, f.env);
                return new CekState(first, f.env, true);
            }
            kont.add(new CondFrame(f.list, i, f.env));
            return new CekState(nc.get(0), f.env, true);
        }
        return new CekState(Boolean.FALSE, env, false);
    }

    private CekState processCaseFrame(CaseFrame f, Object current, List<Object> kont) {
        Env caseEnv = f.env;
        for (int i = 2; i < f.list.size(); i++) {
            List<?> cl = (List<?>) unwrap(f.list.get(i));
            Object datums = unwrap(cl.get(0));
            if (datums instanceof String s && s.equals("else")) {
                Object first = pushBodyFrames(cl, 1, kont, caseEnv);
                return new CekState(first, caseEnv, true);
            }
            if (datums instanceof List<?> dl) {
                for (Object d : dl) {
                    d = unwrap(d);
                    if (schemeEqv(current, quoteValue(d))) {
                        Object first = pushBodyFrames(cl, 1, kont, caseEnv);
                        return new CekState(first, caseEnv, true);
                    }
                }
            }
        }
        return new CekState(Boolean.FALSE, caseEnv, false);
    }

    private CekState processLetBindFrame(LetBindFrame f, Object current,
                                          List<Object> kont) {
        List<Object> nv = new ArrayList<>(f.values); nv.add(current);
        int next = f.bindingIdx + 1;
        if (next < f.initExprs.size()) {
            kont.add(new LetBindFrame(f.names, f.initExprs, next, nv,
                    f.bodyExprs, f.outerEnv, f.namedLetName));
            return new CekState(f.initExprs.get(next), f.outerEnv, true);
        }
        Env le = new Env(f.outerEnv);
        for (int i = 0; i < f.names.size(); i++) le.define(f.names.get(i), nv.get(i));
        if (f.namedLetName != null) {
            Lambda loop = new Lambda(f.names, null, f.bodyExprs, le);
            le.define(f.namedLetName, loop);
        }
        Object first = pushBodyFrames(f.bodyExprs, kont, le);
        return new CekState(first != null ? first : Boolean.FALSE, le, first != null);
    }

    private CekState processLetStarBindFrame(LetStarBindFrame f, Object current,
                                              List<Object> kont) {
        Object bobj = unwrap(f.bindingList.get(f.bindingIdx));
        String nm = (String) unwrap(((List<?>) bobj).get(0));
        f.letEnv.define(nm, current);
        int next = f.bindingIdx + 1;
        if (next < f.bindingList.size()) {
            kont.add(new LetStarBindFrame(f.bindingList, next, f.bodyExprs, f.letEnv));
            Object nb = unwrap(f.bindingList.get(next));
            return new CekState(((List<?>) nb).get(1), f.letEnv, true);
        }
        Object first = pushBodyFrames(f.bodyExprs, kont, f.letEnv);
        return new CekState(first != null ? first : Boolean.FALSE, f.letEnv, first != null);
    }

    private CekState processLetrecBindFrame(LetrecBindFrame f, Object current,
                                             List<Object> kont) {
        List<Object> nv = new ArrayList<>(f.values); nv.add(current);
        if (f.isStar) f.letrecEnv.bindings.put(f.names.get(f.bindingIdx), current);
        int next = f.bindingIdx + 1;
        if (next < f.initExprs.size()) {
            kont.add(new LetrecBindFrame(f.names, f.initExprs, next, nv,
                    f.bodyExprs, f.letrecEnv, f.isStar));
            return new CekState(f.initExprs.get(next), f.letrecEnv, true);
        }
        if (!f.isStar) {
            for (int i = 0; i < f.names.size(); i++)
                f.letrecEnv.bindings.put(f.names.get(i), nv.get(i));
        }
        Object first = pushBodyFrames(f.bodyExprs, kont, f.letrecEnv);
        return new CekState(first != null ? first : Boolean.FALSE, f.letrecEnv, first != null);
    }

    // Apply proc to args. Sets current/env/evaluating via the kont and returns.
    // The caller should `continue` after this.
    // This method modifies the outer loop state indirectly: it pushes frames,
    // and the caller re-enters the loop which will pop them.
    // To communicate the new current/env/evaluating, we use a hack: we push a special
    // one-shot frame, or we inline the logic.
    // Actually, we return the result state as an Object[] = {current, env, evaluating(Boolean)}
    private Object[] applyResult;

    private void doApply(Object proc, List<Object> args, List<Object> kont,
                         int eLine, int eCol, int evalId) throws EvalError {
        String posStr = eLine > 0 ? " at " + eLine + ":" + eCol : "";
        if (proc == DYNAMIC_WIND_PROC) {
            if (args.size() != 3) throw new EvalError("dynamic-wind: expected 3 arguments" + posStr);
            Object inThunk = args.get(0), bodyThunk = args.get(1), outThunk = args.get(2);
            kont.add(new DWAfterInFrame(inThunk, bodyThunk, outThunk));
            doApply(inThunk, List.of(), kont, eLine, eCol, evalId);
            return;
        }
        if (proc == RAISE_PROC) {
            if (args.size() != 1) throw new EvalError("raise: expected 1 argument" + posStr);
            Object value = args.get(0);
            if (exceptionHandlers.isEmpty())
                throw new EvalError("unhandled exception: " + schemeToString(value));
            Object handler = exceptionHandlers.remove(exceptionHandlers.size() - 1);
            if (handler instanceof GuardHandler gh) {
                performWindTransition(gh.savedWindStack);
                kont.clear(); kont.addAll(gh.savedKont);
                exceptionHandlers.clear(); exceptionHandlers.addAll(gh.savedExceptionHandlers);
                Env clauseEnv = new Env(gh.guardEnv);
                clauseEnv.define(gh.varName, value);
                List<Object> condExpr = new ArrayList<>();
                condExpr.add("cond");
                condExpr.addAll(gh.clauses);
                applyResult = new Object[]{condExpr, clauseEnv, true};
                return;
            } else {
                // Procedure handler from with-exception-handler
                kont.add(new RaiseReturnFrame());
                proc = handler; args = List.of(value);
                // Fall through to apply handler
            }
        }
        if (proc == WITH_EXCEPTION_HANDLER_PROC) {
            if (args.size() != 2) throw new EvalError("with-exception-handler: expected 2 arguments" + posStr);
            Object handler = args.get(0);
            Object thunk = args.get(1);
            exceptionHandlers.add(handler);
            kont.add(new WEHAfterFrame());
            proc = thunk; args = List.of();
            // Fall through to apply thunk
        }
        if (proc == VALUES_PROC) {
            if (args.size() == 1) {
                applyResult = new Object[]{args.get(0), null, false};
            } else {
                applyResult = new Object[]{new MultipleValues(new ArrayList<>(args)), null, false};
            }
            return;
        }
        if (proc == CALL_WITH_VALUES_PROC) {
            if (args.size() != 2) throw new EvalError("call-with-values: expected 2 arguments" + posStr);
            Object producer = args.get(0), consumer = args.get(1);
            kont.add(new CallWithValuesFrame(consumer));
            proc = producer; args = List.of();
            // Fall through to apply producer
        }
        if (proc == CALLCC_PROC) {
            if (args.size() != 1) throw new EvalError("call/cc: expected 1 argument" + posStr);
            Continuation k = new Continuation(kont, evalId, windStack, exceptionHandlers);
            proc = args.get(0); args = List.of(k);
            // Fall through to apply proc
        }
        if (proc instanceof Continuation cont) {
            throw new ContinuationInvoked(cont, args.isEmpty() ? Boolean.FALSE : args.get(0));
        }
        if (proc instanceof Lambda lambda) {
            Env callEnv = bindLambdaArgs(lambda, args);
            if (lambda.body.isEmpty()) {
                applyResult = new Object[]{Boolean.FALSE, callEnv, false};
            } else {
                Object first = pushBodyFrames(lambda.body, kont, callEnv);
                applyResult = new Object[]{first != null ? first : Boolean.FALSE, callEnv,
                        first != null};
            }
            return;
        }
        if (proc instanceof CaseLambda cl) {
            Lambda matched = findMatchingClause(cl, args);
            Env callEnv = bindLambdaArgs(matched, args);
            if (matched.body.isEmpty()) {
                applyResult = new Object[]{Boolean.FALSE, callEnv, false};
            } else {
                Object first = pushBodyFrames(matched.body, kont, callEnv);
                applyResult = new Object[]{first != null ? first : Boolean.FALSE, callEnv,
                        first != null};
            }
            return;
        }
        if (proc instanceof BuiltinProc bp) {
            try {
                applyResult = new Object[]{bp.fn.apply(args), null, false};
            } catch (EvalError e) {
                if (eLine > 0 && !e.getMessage().matches(".*\\d+:\\d+.*"))
                    throw new EvalError(e.getMessage() + posStr);
                throw e;
            }
            return;
        }
        throw new EvalError("not a procedure: " + schemeToString(proc) + posStr);
    }

    // Pushes BeginFrame if body has >1 expressions. Returns the first expression to eval.
    // Returns null if body is empty.
    private Object pushBodyFrames(List<Object> body, List<Object> kont, Env env) {
        if (body.isEmpty()) return null;
        if (body.size() > 1) {
            List<Object> bl = new ArrayList<>(); bl.add("begin"); bl.addAll(body);
            kont.add(new BeginFrame(bl, 2, env));
        }
        return body.get(0);
    }

    // Overload for clause body starting at startIdx within a clause list
    private Object pushBodyFrames(List<?> clause, int startIdx, List<Object> kont, Env env) {
        int count = clause.size() - startIdx;
        if (count <= 0) return null;
        if (count > 1) {
            List<Object> bl = new ArrayList<>(); bl.add("begin");
            for (int i = startIdx; i < clause.size(); i++) bl.add(clause.get(i));
            kont.add(new BeginFrame(bl, 2, env));
        }
        return clause.get(startIdx);
    }

    // --- Binding parsing ---

    private void parseBindings(List<?> bindingList, List<String> names, List<Object> inits, String form)
            throws EvalError {
        for (Object b : bindingList) {
            if (b instanceof SchemeParser.Located loc) b = loc.expr;
            if (!(b instanceof List<?> binding) || binding.size() != 2)
                throw new EvalError(form + ": invalid binding");
            Object bn = binding.get(0);
            if (bn instanceof SchemeParser.Located loc) bn = loc.expr;
            if (!(bn instanceof String vn)) throw new EvalError(form + ": binding name must be symbol");
            names.add(vn);
            inits.add(binding.get(1));
        }
    }

    private String parseParamList(List<?> sig, int startIdx, List<String> params, String formName) throws EvalError {
        String restParam = null;
        for (int pi = startIdx; pi < sig.size(); pi++) {
            Object p = sig.get(pi);
            if (p instanceof SchemeParser.Located loc) p = loc.expr;
            if (p instanceof String pname && pname.equals(".")) {
                if (pi + 1 >= sig.size()) throw new EvalError(formName + ": missing rest parameter after dot");
                Object rp = sig.get(pi + 1);
                if (rp instanceof SchemeParser.Located loc) rp = loc.expr;
                if (!(rp instanceof String rpname)) throw new EvalError(formName + ": rest parameter must be symbol");
                restParam = rpname; break;
            }
            if (!(p instanceof String pname)) throw new EvalError(formName + ": parameter must be symbol");
            params.add(pname);
        }
        return restParam;
    }

    // --- Lambda creation ---

    private Lambda makeLambda(List<?> list, Env env) throws EvalError {
        if (list.size() < 3) throw new EvalError("lambda: too few arguments");
        Object ps = list.get(1);
        if (ps instanceof SchemeParser.Located loc) ps = loc.expr;
        if (!(ps instanceof List<?> pl)) throw new EvalError("lambda: params must be a list");
        List<String> params = new ArrayList<>();
        String rest = parseParamList(pl, 0, params, "lambda");
        return new Lambda(params, rest, new ArrayList<>(list.subList(2, list.size())), env);
    }

    private CaseLambda makeCaseLambda(List<?> list, Env env) throws EvalError {
        List<Lambda> clauses = new ArrayList<>();
        for (int ci = 1; ci < list.size(); ci++) {
            Object co = list.get(ci);
            if (co instanceof SchemeParser.Located loc) co = loc.expr;
            if (!(co instanceof List<?> cl) || cl.size() < 2) throw new EvalError("case-lambda: bad clause");
            Object ps = cl.get(0);
            if (ps instanceof SchemeParser.Located loc) ps = loc.expr;
            if (!(ps instanceof List<?> pl)) throw new EvalError("case-lambda: params must be a list");
            List<String> params = new ArrayList<>();
            String rest = parseParamList(pl, 0, params, "case-lambda");
            clauses.add(new Lambda(params, rest, new ArrayList<>(cl.subList(1, cl.size())), env));
        }
        return new CaseLambda(clauses);
    }

    // --- Binding and application ---

    private Env bindLambdaArgs(Lambda lambda, List<Object> args) throws EvalError {
        if (lambda.restParam != null) {
            if (args.size() < lambda.params.size())
                throw new EvalError("wrong number of arguments: expected at least " + lambda.params.size() + ", got " + args.size());
        } else {
            if (args.size() != lambda.params.size())
                throw new EvalError("wrong number of arguments: expected " + lambda.params.size() + ", got " + args.size());
        }
        Env callEnv = new Env(lambda.closureEnv);
        for (int i = 0; i < lambda.params.size(); i++) callEnv.define(lambda.params.get(i), args.get(i));
        if (lambda.restParam != null) {
            Object rest = EMPTY_LIST;
            for (int i = args.size() - 1; i >= lambda.params.size(); i--) rest = new Pair(args.get(i), rest);
            callEnv.define(lambda.restParam, rest);
        }
        return callEnv;
    }

    private Lambda findMatchingClause(CaseLambda cl, List<Object> args) throws EvalError {
        for (Lambda c : cl.clauses) {
            if (c.restParam != null) { if (args.size() >= c.params.size()) return c; }
            else { if (args.size() == c.params.size()) return c; }
        }
        throw new EvalError("case-lambda: no matching clause for " + args.size() + " arguments");
    }

    private void performWindTransition(List<DynamicWindEntry> target) throws EvalError {
        int common = 0;
        int minLen = Math.min(windStack.size(), target.size());
        while (common < minLen && windStack.get(common) == target.get(common)) common++;
        // Unwind: out-thunks from innermost to outermost
        for (int i = windStack.size() - 1; i >= common; i--) {
            applyProc(windStack.get(i).outThunk(), List.of());
        }
        while (windStack.size() > common) windStack.remove(windStack.size() - 1);
        // Rewind: in-thunks from outermost to innermost
        for (int i = common; i < target.size(); i++) {
            windStack.add(target.get(i));
            applyProc(target.get(i).inThunk(), List.of());
        }
    }

    // Used by builtins (map, for-each, apply) via callback
    private Object applyProc(Object proc, List<Object> args) throws EvalError {
        if (proc instanceof Continuation cont)
            throw new ContinuationInvoked(cont, args.isEmpty() ? Boolean.FALSE : args.get(0));
        if (proc == CALLCC_PROC)
            throw new EvalError("call/cc: cannot be invoked via apply in this context");
        if (proc instanceof Lambda lambda) {
            Env callEnv = bindLambdaArgs(lambda, args);
            return evalBody(lambda.body, callEnv);
        }
        if (proc instanceof CaseLambda cl) {
            Lambda m = findMatchingClause(cl, args);
            return evalBody(m.body, bindLambdaArgs(m, args));
        }
        if (proc instanceof BuiltinProc bp) return bp.fn.apply(args);
        throw new EvalError("not a procedure: " + schemeToString(proc));
    }

    private Object evalBody(List<Object> body, Env env) throws EvalError {
        if (body.isEmpty()) return Boolean.FALSE;
        if (body.size() == 1) return eval(body.get(0), env);
        List<Object> bf = new ArrayList<>(); bf.add("begin"); bf.addAll(body);
        return eval(bf, env);
    }

    // --- Do desugaring ---

    @SuppressWarnings("unchecked")
    private Object desugarDo(List<?> list) throws EvalError {
        if (list.size() < 3) throw new EvalError("do: too few arguments");
        Object vo = list.get(1); if (vo instanceof SchemeParser.Located loc) vo = loc.expr;
        if (!(vo instanceof List<?> varSpecs)) throw new EvalError("do: variable specs must be a list");
        Object to = list.get(2); if (to instanceof SchemeParser.Located loc) to = loc.expr;
        if (!(to instanceof List<?> testClause) || testClause.isEmpty())
            throw new EvalError("do: test clause must be a list");

        String loopName = "__do_" + (gensymCounter++);
        List<Object> bindings = new ArrayList<>(), stepArgs = new ArrayList<>();
        for (Object vs : varSpecs) {
            if (vs instanceof SchemeParser.Located loc) vs = loc.expr;
            if (!(vs instanceof List<?> spec) || spec.size() < 2)
                throw new EvalError("do: invalid variable spec");
            Object vn = spec.get(0); if (vn instanceof SchemeParser.Located loc) vn = loc.expr;
            if (!(vn instanceof String name)) throw new EvalError("do: variable name must be symbol");
            List<Object> b = new ArrayList<>(); b.add(name); b.add(spec.get(1)); bindings.add(b);
            stepArgs.add(spec.size() >= 3 ? spec.get(2) : name);
        }
        Object exitExpr;
        if (testClause.size() == 1) exitExpr = Boolean.FALSE;
        else if (testClause.size() == 2) exitExpr = testClause.get(1);
        else {
            List<Object> eb = new ArrayList<>(); eb.add("begin");
            for (int i = 1; i < testClause.size(); i++) eb.add(testClause.get(i));
            exitExpr = eb;
        }
        List<Object> loopCall = new ArrayList<>(); loopCall.add(loopName); loopCall.addAll(stepArgs);
        List<Object> elseBody = new ArrayList<>(); elseBody.add("begin");
        for (int i = 3; i < list.size(); i++) elseBody.add(list.get(i));
        elseBody.add(loopCall);
        List<Object> ifForm = new ArrayList<>();
        ifForm.add("if"); ifForm.add(testClause.get(0)); ifForm.add(exitExpr); ifForm.add(elseBody);
        List<Object> namedLet = new ArrayList<>();
        namedLet.add("let"); namedLet.add(loopName); namedLet.add(bindings); namedLet.add(ifForm);
        return namedLet;
    }

    // --- Syntax/macro ---

    private Object handleDefineSyntax(List<?> list, Env env) throws EvalError {
        if (list.size() != 3) throw new EvalError("define-syntax: expected 2 arguments");
        Object no = list.get(1); if (no instanceof SchemeParser.Located loc) no = loc.expr;
        if (!(no instanceof String macroName)) throw new EvalError("define-syntax: name must be symbol");
        Object te = list.get(2); if (te instanceof SchemeParser.Located loc) te = loc.expr;
        // Check if it's syntax-rules
        if (te instanceof List<?> tr && !tr.isEmpty()) {
            Object sh = tr.get(0); if (sh instanceof SchemeParser.Located loc) sh = loc.expr;
            if (sh instanceof String ss && ss.equals("syntax-rules")) {
                Object lo = tr.get(1); if (lo instanceof SchemeParser.Located loc) lo = loc.expr;
                List<String> lits = new ArrayList<>();
                if (lo instanceof List<?> ll) { for (Object l : ll) { l = unwrap(l); if (l instanceof String s) lits.add(s); } }
                List<Object[]> rules = new ArrayList<>();
                for (int ri = 2; ri < tr.size(); ri++) {
                    Object ro = tr.get(ri); if (ro instanceof SchemeParser.Located loc) ro = loc.expr;
                    if (!(ro instanceof List<?> rule) || rule.size() != 2) throw new EvalError("define-syntax: invalid rule");
                    rules.add(new Object[]{rule.get(0), rule.get(1)});
                }
                env.define(macroName, new SyntaxRulesMacro(macroName, lits, rules, env));
                return Boolean.FALSE;
            }
        }
        // Not syntax-rules — evaluate to get a transformer procedure
        Object transformer = eval(te, env);
        env.define(macroName, new SyntaxCaseTransformer(transformer, env));
        return Boolean.FALSE;
    }

    private Object handleDefineRecordType(List<?> list, Env env) throws EvalError {
        if (list.size() < 4) throw new EvalError("define-record-type: too few arguments");
        String typeName = (String) unwrap(list.get(1));
        List<?> ctorSpec = (List<?>) unwrap(list.get(2));
        String ctorName = (String) unwrap(ctorSpec.get(0));
        List<String> ctorFields = new ArrayList<>();
        for (int ci = 1; ci < ctorSpec.size(); ci++) ctorFields.add((String) unwrap(ctorSpec.get(ci)));
        String predName = (String) unwrap(list.get(3));
        Map<String, Integer> fieldIndex = new HashMap<>();
        for (int fi = 0; fi < ctorFields.size(); fi++) fieldIndex.put(ctorFields.get(fi), fi);
        Map<String, Integer> accessorMap = new HashMap<>();
        for (int fi = 4; fi < list.size(); fi++) {
            List<?> fs = (List<?>) unwrap(list.get(fi));
            String fn = (String) unwrap(fs.get(0));
            String an = (String) unwrap(fs.get(1));
            accessorMap.put(an, fieldIndex.get(fn));
        }
        RecordType rt = new RecordType(typeName, ctorFields);
        env.define(ctorName, new BuiltinProc(ctorName, args -> {
            if (args.size() != ctorFields.size())
                throw new EvalError(ctorName + ": expected " + ctorFields.size() + " arguments, got " + args.size());
            return new SchemeRecord(rt, args.toArray());
        }));
        env.define(predName, new BuiltinProc(predName, args -> {
            if (args.size() != 1) throw new EvalError(predName + ": expected 1 argument");
            return args.get(0) instanceof SchemeRecord sr && sr.type == rt;
        }));
        for (var e : accessorMap.entrySet()) {
            String an = e.getKey(); int idx = e.getValue();
            env.define(an, new BuiltinProc(an, args -> {
                if (args.size() != 1) throw new EvalError(an + ": expected 1 argument");
                if (!(args.get(0) instanceof SchemeRecord sr) || sr.type != rt)
                    throw new EvalError(an + ": not a " + typeName);
                return sr.fields[idx];
            }));
        }
        return Boolean.FALSE;
    }

    // --- Macro expansion ---

    @SuppressWarnings("unchecked")
    private Object[] expandMacro(SyntaxRulesMacro macro, List<?> form, Env useEnv) throws EvalError {
        for (Object[] rule : macro.rules) {
            Map<String, Object> bindings = matchPattern(rule[0], form, macro.literals);
            if (bindings != null) {
                Map<String, String> renameMap = new HashMap<>();
                Object expanded = expandTemplate(rule[1], bindings, renameMap);
                Env evalEnv = useEnv;
                if (!renameMap.isEmpty()) {
                    Map<String, Object> hb = new HashMap<>();
                    for (var e : renameMap.entrySet()) {
                        try { hb.put(e.getValue(), macro.defEnv.lookup(e.getKey(), new SchemeParser.Pos(0, 0))); }
                        catch (EvalError ignored) {}
                    }
                    if (!hb.isEmpty()) { evalEnv = new Env(useEnv); for (var e : hb.entrySet()) evalEnv.define(e.getKey(), e.getValue()); }
                }
                return new Object[]{expanded, evalEnv};
            }
        }
        throw new EvalError("no matching pattern for macro " + macro.name);
    }

    private Map<String, Object> matchPattern(Object pattern, List<?> input, List<String> literals) {
        if (pattern instanceof SchemeParser.Located loc) pattern = loc.expr;
        if (!(pattern instanceof List<?> patList)) return null;
        Map<String, Object> bindings = new HashMap<>();
        int pi = 1, ii = 1;
        while (pi < patList.size()) {
            Object pe = patList.get(pi); if (pe instanceof SchemeParser.Located loc) pe = loc.expr;
            boolean hasE = false;
            if (pi + 1 < patList.size()) {
                Object nx = patList.get(pi + 1); if (nx instanceof SchemeParser.Located loc) nx = loc.expr;
                if ("...".equals(nx)) hasE = true;
            }
            if (hasE) {
                if (!(pe instanceof String vn)) return null;
                List<Object> coll = new ArrayList<>();
                while (ii < input.size()) { coll.add(input.get(ii)); ii++; }
                bindings.put(vn, coll); pi += 2;
            } else if (pe instanceof String s && literals.contains(s)) {
                if (ii >= input.size()) return null;
                Object ie = input.get(ii); if (ie instanceof SchemeParser.Located loc) ie = loc.expr;
                if (!s.equals(ie)) return null;
                pi++; ii++;
            } else if (pe instanceof String vn) {
                if (ii >= input.size()) return null;
                bindings.put(vn, input.get(ii)); pi++; ii++;
            } else return null;
        }
        return ii == input.size() ? bindings : null;
    }

    @SuppressWarnings("unchecked")
    private Object expandTemplate(Object template, Map<String, Object> bindings, Map<String, String> renameMap) {
        if (template instanceof SchemeParser.Located loc) template = loc.expr;
        if (template instanceof String id) {
            if (bindings.containsKey(id)) return bindings.get(id);
            if ("...".equals(id) || SPECIAL_FORMS.contains(id)) return id;
            if (!renameMap.containsKey(id)) renameMap.put(id, "__" + id + "_" + (gensymCounter++));
            return renameMap.get(id);
        }
        if (template instanceof List<?> tl) {
            List<Object> result = new ArrayList<>();
            for (int i = 0; i < tl.size(); i++) {
                Object elem = tl.get(i);
                Object raw = elem; if (raw instanceof SchemeParser.Located loc) raw = loc.expr;
                boolean nxt = false;
                if (i + 1 < tl.size()) { Object n = tl.get(i + 1); if (n instanceof SchemeParser.Located loc) n = loc.expr; if ("...".equals(n)) nxt = true; }
                if (nxt) {
                    String ev = findEllipsisVar(elem, bindings);
                    if (ev != null) {
                        for (Object e : (List<Object>) bindings.get(ev)) {
                            Map<String, Object> sb = new HashMap<>(bindings); sb.put(ev, e);
                            result.add(expandTemplate(elem, sb, renameMap));
                        }
                    }
                    i++;
                } else if (raw instanceof String s && "...".equals(s)) { /* skip */ }
                else result.add(expandTemplate(elem, bindings, renameMap));
            }
            return result;
        }
        return template;
    }

    private String findEllipsisVar(Object template, Map<String, Object> bindings) {
        if (template instanceof SchemeParser.Located loc) template = loc.expr;
        if (template instanceof String s && bindings.get(s) instanceof List) return s;
        if (template instanceof List<?> list) {
            for (Object e : list) { String f = findEllipsisVar(e, bindings); if (f != null) return f; }
        }
        return null;
    }

    // --- syntax-case support ---

    @SuppressWarnings("unchecked")
    private CekState processSyntaxCaseFrame(SyntaxCaseFrame f, Object current, Env env) throws EvalError {
        Object scrutinee;
        if (current instanceof SyntaxObject so) scrutinee = so.datum;
        else scrutinee = current;
        if (!(scrutinee instanceof List<?> inputList))
            throw new EvalError("syntax-case: scrutinee must be a list");
        for (Object clauseObj : f.clauses) {
            List<?> clause = (List<?>) unwrap(clauseObj);
            Object pattern = clause.get(0);
            Map<String, Object> bindings = matchSyntaxCasePattern(pattern, inputList, f.literals);
            if (bindings != null) {
                Env clauseEnv = new Env(f.env);
                for (var e : bindings.entrySet()) {
                    clauseEnv.define(e.getKey(), new SyntaxObject(e.getValue()));
                }
                Object body = clause.size() == 3 ? clause.get(2) : clause.get(1);
                return new CekState(body, clauseEnv, true);
            }
        }
        throw new EvalError("syntax-case: no matching pattern");
    }

    private CekState processWithSyntaxFrame(WithSyntaxFrame f, Object current, Env env,
                                             List<Object> kont) {
        List<Object> values = new ArrayList<>(f.values);
        Object val = current instanceof SyntaxObject ? current : new SyntaxObject(current);
        values.add(val);
        int next = f.index + 1;
        if (next < f.bindings.size()) {
            kont.add(new WithSyntaxFrame(f.bindings, next, values, f.bodyExprs, f.env));
            List<?> nb = (List<?>) unwrap(f.bindings.get(next));
            return new CekState(nb.get(1), f.env, true);
        }
        Env wsEnv = new Env(f.env);
        for (int i = 0; i < f.bindings.size(); i++) {
            List<?> binding = (List<?>) unwrap(f.bindings.get(i));
            String name = (String) unwrap(binding.get(0));
            wsEnv.define(name, values.get(i));
        }
        Object first = pushBodyFrames(f.bodyExprs, kont, wsEnv);
        return new CekState(first != null ? first : Boolean.FALSE, wsEnv, first != null);
    }

    private Map<String, Object> matchSyntaxCasePattern(Object pattern, List<?> input, List<String> literals) {
        if (pattern instanceof SchemeParser.Located loc) pattern = loc.expr;
        if (!(pattern instanceof List<?> patList)) return null;
        Map<String, Object> bindings = new HashMap<>();
        int pi = 0, ii = 0;
        while (pi < patList.size()) {
            Object pe = patList.get(pi);
            if (pe instanceof SchemeParser.Located loc) pe = loc.expr;
            boolean hasE = false;
            if (pi + 1 < patList.size()) {
                Object nx = patList.get(pi + 1);
                if (nx instanceof SchemeParser.Located loc) nx = loc.expr;
                if ("...".equals(nx)) hasE = true;
            }
            if (hasE) {
                if (!(pe instanceof String vn)) return null;
                List<Object> coll = new ArrayList<>();
                while (ii < input.size()) { coll.add(input.get(ii)); ii++; }
                bindings.put(vn, coll); pi += 2;
            } else if (pe instanceof String s && s.equals("_")) {
                if (ii >= input.size()) return null;
                pi++; ii++;
            } else if (pe instanceof String s && literals.contains(s)) {
                if (ii >= input.size()) return null;
                Object ie = input.get(ii);
                if (ie instanceof SchemeParser.Located loc) ie = loc.expr;
                if (!s.equals(ie)) return null;
                pi++; ii++;
            } else if (pe instanceof String vn) {
                if (ii >= input.size()) return null;
                bindings.put(vn, input.get(ii)); pi++; ii++;
            } else return null;
        }
        return ii == input.size() ? bindings : null;
    }

    @SuppressWarnings("unchecked")
    private Object expandSyntaxTemplate(Object template, Env env,
                                         Map<String, String> renameMap,
                                         Map<String, Object> hygieneBindings) {
        template = unwrap(template);
        if (template instanceof String id) {
            Object val = null;
            try { val = env.lookup(id, new SchemeParser.Pos(0, 0)); } catch (EvalError ignored) {}
            if (val instanceof SyntaxObject so) return so.datum;
            if ("...".equals(id) || SPECIAL_FORMS.contains(id)) return id;
            if (!renameMap.containsKey(id)) renameMap.put(id, "__" + id + "_" + (gensymCounter++));
            String renamed = renameMap.get(id);
            if (syntaxDefEnv != null && !hygieneBindings.containsKey(renamed)) {
                try {
                    hygieneBindings.put(renamed, syntaxDefEnv.lookup(id, new SchemeParser.Pos(0, 0)));
                } catch (EvalError ignored) {}
            }
            return renamed;
        }
        if (template instanceof List<?> tl) {
            if (!tl.isEmpty()) {
                Object head = unwrap(tl.get(0));
                if ("quote".equals(head)) return tl;
            }
            List<Object> result = new ArrayList<>();
            for (int i = 0; i < tl.size(); i++) {
                Object elem = tl.get(i);
                Object raw = unwrap(elem);
                boolean hasEllipsis = false;
                if (i + 1 < tl.size()) {
                    Object nx = unwrap(tl.get(i + 1));
                    if ("...".equals(nx)) hasEllipsis = true;
                }
                if (hasEllipsis) {
                    String ev = findSyntaxEllipsisVar(elem, env);
                    if (ev != null) {
                        Object evVal = null;
                        try { evVal = env.lookup(ev, new SchemeParser.Pos(0, 0)); } catch (EvalError ignored) {}
                        if (evVal instanceof SyntaxObject so && so.datum instanceof List<?> items) {
                            for (Object item : items) {
                                Env subEnv = new Env(env);
                                subEnv.define(ev, new SyntaxObject(item));
                                result.add(expandSyntaxTemplate(elem, subEnv, renameMap, hygieneBindings));
                            }
                        }
                    }
                    i++;
                } else if ("...".equals(raw)) {
                    // skip
                } else {
                    result.add(expandSyntaxTemplate(elem, env, renameMap, hygieneBindings));
                }
            }
            return result;
        }
        return template;
    }

    private String findSyntaxEllipsisVar(Object template, Env env) {
        template = unwrap(template);
        if (template instanceof String id) {
            Object val = null;
            try { val = env.lookup(id, new SchemeParser.Pos(0, 0)); } catch (EvalError ignored) {}
            if (val instanceof SyntaxObject so && so.datum instanceof List) return id;
            return null;
        }
        if (template instanceof List<?> tl) {
            for (Object e : tl) {
                String found = findSyntaxEllipsisVar(e, env);
                if (found != null) return found;
            }
        }
        return null;
    }

    // --- Utility ---

    private static Object unwrap(Object obj) {
        if (obj instanceof SchemeParser.Located loc) return loc.expr;
        return obj;
    }

    private Object quoteValue(Object datum) {
        if (datum instanceof SchemeParser.Located loc) datum = loc.expr;
        if (datum instanceof List<?> list) {
            Object result = EMPTY_LIST;
            for (int i = list.size() - 1; i >= 0; i--) result = new Pair(quoteValue(list.get(i)), result);
            return result;
        }
        return datum;
    }

    private boolean schemeEqv(Object a, Object b) {
        if (a == b) return true;
        if (a instanceof Long && b instanceof Long) return a.equals(b);
        if (a instanceof Double && b instanceof Double) return a.equals(b);
        if (a instanceof Rational && b instanceof Rational) return a.equals(b);
        if (a instanceof Boolean && b instanceof Boolean) return a.equals(b);
        if (a instanceof SchemeChar && b instanceof SchemeChar) return a.equals(b);
        if (a instanceof String && b instanceof String) return a.equals(b);
        return false;
    }

    static boolean schemeEqual(Object a, Object b) {
        return schemeEqualRec(a, b, new java.util.IdentityHashMap<>());
    }

    private static boolean schemeEqualRec(Object a, Object b, java.util.IdentityHashMap<Object, Object> seen) {
        if (a == b) return true;
        if (isNumber(a) && isNumber(b)) { try { return toDouble(a) == toDouble(b); } catch (EvalError e) { return false; } }
        if (a instanceof Boolean && b instanceof Boolean) return a.equals(b);
        if (a instanceof String && b instanceof String) return a.equals(b);
        if (a instanceof SchemeString sa && b instanceof SchemeString sb) return sa.value().equals(sb.value());
        if (a instanceof SchemeChar ca && b instanceof SchemeChar cb) return ca.value() == cb.value();
        if (a == EMPTY_LIST && b == EMPTY_LIST) return true;
        if (a instanceof Pair pa && b instanceof Pair pb) {
            if (seen.containsKey(pa)) return true; seen.put(pa, pb);
            return schemeEqualRec(pa.car, pb.car, seen) && schemeEqualRec(pa.cdr, pb.cdr, seen);
        }
        if (a instanceof SchemeVector va && b instanceof SchemeVector vb) {
            if (va.length() != vb.length()) return false;
            for (int i = 0; i < va.length(); i++) if (!schemeEqualRec(va.ref(i), vb.ref(i), seen)) return false;
            return true;
        }
        return false;
    }

    static boolean schemeEqvStatic(Object a, Object b) {
        if (a == b) return true;
        if (a instanceof Long && b instanceof Long) return a.equals(b);
        if (a instanceof Double && b instanceof Double) return a.equals(b);
        if (a instanceof Rational && b instanceof Rational) return a.equals(b);
        if (a instanceof Boolean && b instanceof Boolean) return a.equals(b);
        if (a instanceof SchemeChar && b instanceof SchemeChar) return a.equals(b);
        if (a instanceof String && b instanceof String) return a.equals(b);
        return false;
    }

    static boolean isTruthy(Object val) {
        return !(val instanceof Boolean b && !b);
    }
}
