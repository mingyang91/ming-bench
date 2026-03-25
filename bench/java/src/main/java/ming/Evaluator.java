package ming;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

public class Evaluator {

    @FunctionalInterface
    interface BuiltinFn {
        Object apply(List<Object> args) throws EvalError;
    }

    record Builtin(String name, BuiltinFn fn) {
        Object apply(List<Object> args) throws EvalError {
            return fn.apply(args);
        }
    }

    static class SchemeString {
        private final char[] chars;
        private final boolean immutable;
        SchemeString(String value) { this.chars = value.toCharArray(); this.immutable = false; }
        SchemeString(String value, boolean immutable) { this.chars = value.toCharArray(); this.immutable = immutable; }
        SchemeString(char[] chars) { this.chars = chars; this.immutable = false; }
        String value() { return new String(chars); }
        int length() { return chars.length; }
        char charAt(int i) { return chars[i]; }
        boolean isImmutable() { return immutable; }
        void setChar(int i, char c) throws EvalError {
            if (immutable) throw new EvalError("string-set!: strings are immutable");
            chars[i] = c;
        }
    }

    record SchemeChar(char value) {}

    static class SchemeRational {
        final long num;
        final long den;
        SchemeRational(long num, long den) {
            if (den == 0) throw new ArithmeticException("division by zero");
            if (den < 0) { num = -num; den = -den; }
            long g = gcd(Math.abs(num), den);
            this.num = num / g;
            this.den = den / g;
        }
        boolean isInteger() { return den == 1; }
        long toLong() { return num / den; }
        double toDouble() { return (double) num / den; }
        private static long gcd(long a, long b) { while (b != 0) { long t = b; b = a % b; a = t; } return a; }
        @Override public boolean equals(Object o) {
            if (this == o) return true;
            if (!(o instanceof SchemeRational r)) return false;
            return num == r.num && den == r.den;
        }
        @Override public int hashCode() { return Long.hashCode(num) * 31 + Long.hashCode(den); }
    }

    record Lambda(List<String> params, String restParam, List<Object> body, Env env) {}

    record CaseLambda(List<Lambda> clauses) {}

    record Pos(int line, int col) {
        @Override public String toString() { return line + ":" + col; }
    }

    static class SExpr extends ArrayList<Object> {
        final Pos pos;
        SExpr(Pos pos) { super(); this.pos = pos; }
    }

    record Token(Object value, Pos pos) {}

    static class Cons {
        Object car;
        Object cdr;
        Cons(Object car, Object cdr) { this.car = car; this.cdr = cdr; }
    }

    static class SyntaxRules {
        final List<String> literals;
        final List<List<Object>> patterns;
        final List<Object> templates;
        final Env defEnv;
        SyntaxRules(List<String> literals, List<List<Object>> patterns, List<Object> templates, Env defEnv) {
            this.literals = literals;
            this.patterns = patterns;
            this.templates = templates;
            this.defEnv = defEnv;
        }
    }

    static class RecordType {
        final String name;
        final List<String> fieldNames;
        RecordType(String name, List<String> fieldNames) {
            this.name = name;
            this.fieldNames = fieldNames;
        }
    }

    static class SchemeRecord {
        final RecordType type;
        final Object[] fields;
        SchemeRecord(RecordType type, Object[] fields) {
            this.type = type;
            this.fields = fields;
        }
    }

    static class SchemeVector {
        final Object[] data;
        SchemeVector(Object[] data) { this.data = data; }
        int length() { return data.length; }
        Object ref(int i) { return data[i]; }
        void set(int i, Object v) { data[i] = v; }
    }

    static final Object NIL = new Object() {
        @Override public String toString() { return "()"; }
    };

    static final Object VOID = new Object() {
        @Override public String toString() { return "#<void>"; }
    };

    static class Env {
        final Map<String, Object> bindings = new HashMap<>();
        final Env parent;
        Env(Env parent) { this.parent = parent; }
        Object lookup(String name) throws EvalError {
            if (bindings.containsKey(name)) return bindings.get(name);
            if (parent != null) return parent.lookup(name);
            throw new EvalError("unbound variable: " + name);
        }
        void define(String name, Object val) { bindings.put(name, val); }
        void set(String name, Object val) throws EvalError {
            if (bindings.containsKey(name)) { bindings.put(name, val); return; }
            if (parent != null) { parent.set(name, val); return; }
            throw new EvalError("set!: unbound variable: " + name);
        }
    }

    // ===== Continuation types for CEK machine =====

    static abstract class Kont {}

    static final class HaltK extends Kont {
        static final HaltK INST = new HaltK();
    }

    static final class IfK extends Kont {
        final Object thenE, elseE;
        final Env env; final Kont k;
        IfK(Object t, Object e, Env env, Kont k) { thenE=t; elseE=e; this.env=env; this.k=k; }
    }

    // Sequence: evaluate exprs[idx], then exprs[idx+1], ..., with last in tail position
    static final class SeqK extends Kont {
        final List<?> exprs; final int idx;
        final Env env; final Kont k;
        SeqK(List<?> es, int i, Env env, Kont k) { exprs=es; idx=i; this.env=env; this.k=k; }
    }

    static final class SetK extends Kont {
        final String name; final Env env; final Pos pos; final Kont k;
        SetK(String n, Env env, Pos p, Kont k) { name=n; this.env=env; pos=p; this.k=k; }
    }

    static final class DefK extends Kont {
        final String name; final Env env; final Kont k;
        DefK(String n, Env env, Kont k) { name=n; this.env=env; this.k=k; }
    }

    // Evaluated operator, now start evaluating args
    static final class EvArgsK extends Kont {
        final List<Object> argExprs;
        final Env env; final Pos pos; final Kont k;
        EvArgsK(List<Object> ae, Env env, Pos pos, Kont k) { argExprs=ae; this.env=env; this.pos=pos; this.k=k; }
    }

    // Accumulating evaluated args
    static final class AccArgsK extends Kont {
        final Object fun;
        final List<Object> evaled;
        final List<Object> argExprs;
        final int nextIdx;
        final Env env; final Pos pos; final Kont k;
        AccArgsK(Object f, List<Object> ev, List<Object> ae, int ni, Env env, Pos pos, Kont k) {
            fun=f; evaled=ev; argExprs=ae; nextIdx=ni; this.env=env; this.pos=pos; this.k=k;
        }
    }

    static final class AndK extends Kont {
        final List<?> form; final int nextIdx;
        final Env env; final Kont k;
        AndK(List<?> f, int i, Env env, Kont k) { form=f; nextIdx=i; this.env=env; this.k=k; }
    }

    static final class OrK extends Kont {
        final List<?> form; final int nextIdx;
        final Env env; final Kont k;
        OrK(List<?> f, int i, Env env, Kont k) { form=f; nextIdx=i; this.env=env; this.k=k; }
    }

    // Let/let*/letrec bindings
    static final class LetBindK extends Kont {
        final String name;
        final List<?> bindings; final int nextIdx;
        final Env evalEnv; final Env letEnv;
        final List<?> form; final int bodyStart;
        final Kont k;
        LetBindK(String n, List<?> b, int ni, Env ee, Env le, List<?> f, int bs, Kont k) {
            name=n; bindings=b; nextIdx=ni; evalEnv=ee; letEnv=le; form=f; bodyStart=bs; this.k=k;
        }
    }

    // Cond: evaluated a test
    static final class CondK extends Kont {
        final List<?> clause;
        final List<?> form; final int nextClauseIdx;
        final Env env; final Kont k;
        CondK(List<?> c, List<?> f, int ni, Env env, Kont k) { clause=c; form=f; nextClauseIdx=ni; this.env=env; this.k=k; }
    }

    // Case: evaluated the key
    static final class CaseKeyK extends Kont {
        final List<?> form; final Env env; final Kont k;
        CaseKeyK(List<?> f, Env env, Kont k) { form=f; this.env=env; this.k=k; }
    }

    // Do: init evaluation
    static final class DoInitK extends Kont {
        final int idx;
        final String[] varNames; final Object[] initExprs;
        final Object[] stepExprs; final boolean[] hasStep;
        final Env doEnv; final List<?> testClause; final List<?> form;
        final Env outerEnv; final Kont k;
        DoInitK(int i, String[] vn, Object[] ie, Object[] se, boolean[] hs, Env de, List<?> tc, List<?> f, Env oe, Kont k) {
            idx=i; varNames=vn; initExprs=ie; stepExprs=se; hasStep=hs; doEnv=de; testClause=tc; form=f; outerEnv=oe; this.k=k;
        }
    }

    // Do: test evaluated
    static final class DoTestK extends Kont {
        final String[] varNames; final Object[] stepExprs; final boolean[] hasStep;
        final List<?> testClause; final List<?> form;
        final Env doEnv; final Kont k;
        DoTestK(String[] vn, Object[] se, boolean[] hs, List<?> tc, List<?> f, Env de, Kont k) {
            varNames=vn; stepExprs=se; hasStep=hs; testClause=tc; form=f; doEnv=de; this.k=k;
        }
    }

    // Do: after commands, start step evaluation
    static final class DoStepStartK extends Kont {
        final String[] varNames; final Object[] stepExprs; final boolean[] hasStep;
        final List<?> testClause; final List<?> form;
        final Env doEnv; final Kont k;
        DoStepStartK(String[] vn, Object[] se, boolean[] hs, List<?> tc, List<?> f, Env de, Kont k) {
            varNames=vn; stepExprs=se; hasStep=hs; testClause=tc; form=f; doEnv=de; this.k=k;
        }
    }

    // Do: evaluating step expressions
    static final class DoStepK extends Kont {
        final int idx; final Object[] newVals;
        final String[] varNames; final Object[] stepExprs; final boolean[] hasStep;
        final List<?> testClause; final List<?> form;
        final Env doEnv; final Kont k;
        DoStepK(int i, Object[] nv, String[] vn, Object[] se, boolean[] hs, List<?> tc, List<?> f, Env de, Kont k) {
            idx=i; newVals=nv; varNames=vn; stepExprs=se; hasStep=hs; testClause=tc; form=f; doEnv=de; this.k=k;
        }
    }

    // Map builtin continuation
    static final class MapK extends Kont {
        final Object fun; final List<Object> remainingLists;
        final List<Object> results; final Pos pos; final Kont k;
        MapK(Object f, List<Object> rl, List<Object> r, Pos p, Kont k) {
            fun=f; remainingLists=rl; results=r; pos=p; this.k=k;
        }
    }

    // For-each builtin continuation
    static final class ForEachK extends Kont {
        final Object fun; final List<Object> remainingLists;
        final Pos pos; final Kont k;
        ForEachK(Object f, List<Object> rl, Pos p, Kont k) {
            fun=f; remainingLists=rl; pos=p; this.k=k;
        }
    }

    // First-class continuation value
    static final class SchemeContinuation {
        final Kont k;
        SchemeContinuation(Kont k) { this.k = k; }
    }

    // ===== Constants =====

    static final java.util.Set<String> SPECIAL_FORMS = java.util.Set.of(
        "quote", "if", "define", "lambda", "and", "or", "let", "set!", "begin", "cond",
        "define-syntax", "syntax-rules", "else", "let*", "letrec", "letrec*", "case",
        "do", "define-record-type", "case-lambda"
    );

    private int gensymCounter = 0;
    String gensym(String base) {
        return "##" + base + "_" + (gensymCounter++);
    }

    private final Env globalEnv = new Env(null);
    private StringBuilder outputBuffer = new StringBuilder();
    private final SchemeReader reader = new SchemeReader();
    private final MacroExpander macroExpander = new MacroExpander(this);

    // CEK machine state
    private Object mExpr;
    private Env mEnv;
    private Kont mK;
    private Object mVal;
    private boolean mApply;

    public Evaluator() {
        new Builtins(globalEnv, this).registerAll();
    }

    void appendOutput(String s) {
        outputBuffer.append(s);
    }

    public String evalStr(String input) throws EvalError {
        List<Token> tokens = tokenize(input);
        int[] pos = {0};
        List<Object> program = new ArrayList<>();
        while (pos[0] < tokens.size()) {
            program.add(parse(tokens, pos));
        }
        if (program.isEmpty()) throw new EvalError("no expression");
        Object result = cekRun(program, globalEnv);
        if (result == VOID) return "#<void>";
        return schemeToString(result);
    }

    public EvalResult evalStrWithOutput(String input) throws EvalError {
        outputBuffer = new StringBuilder();
        List<Token> tokens = tokenize(input);
        int[] pos = {0};
        List<Object> program = new ArrayList<>();
        while (pos[0] < tokens.size()) {
            program.add(parse(tokens, pos));
        }
        Object lastResult = null;
        if (!program.isEmpty()) {
            lastResult = cekRun(program, globalEnv);
        }
        String result = "";
        if (lastResult != null && lastResult != VOID) {
            result = schemeToString(lastResult);
        }
        return new EvalResult(result, outputBuffer.toString());
    }

    private List<Token> tokenize(String input) throws EvalError {
        return reader.tokenize(input);
    }

    private Object parse(List<Token> tokens, int[] pos) throws EvalError {
        return reader.parse(tokens, pos);
    }

    // ===== CEK Machine =====

    private Object cekRun(List<Object> program, Env env) throws EvalError {
        mK = HaltK.INST;
        if (program.size() == 1) {
            mExpr = program.get(0);
        } else {
            mK = new SeqK(program, 1, env, mK);
            mExpr = program.get(0);
        }
        mEnv = env;
        mApply = false;
        mVal = null;

        while (true) {
            if (mApply) {
                if (mK instanceof HaltK) return mVal;
                applyStep();
            } else {
                evalStep();
            }
        }
    }

    private Object unwrap(Object expr) {
        if (expr instanceof Token t) return t.value();
        return expr;
    }

    private Pos posOf(Object expr) {
        if (expr instanceof Token t) return t.pos();
        if (expr instanceof SExpr s) return s.pos;
        return null;
    }

    private String posStr(Pos p) {
        return p != null ? " at " + p : "";
    }

    @SuppressWarnings("unchecked")
    private void evalStep() throws EvalError {
        Object raw = unwrap(mExpr);
        Pos pos = posOf(mExpr);

        // Self-evaluating
        if (raw instanceof Long || raw instanceof Boolean || raw instanceof SchemeString
                || raw instanceof SchemeChar || raw instanceof SchemeRational || raw instanceof Double) {
            mVal = raw; mApply = true; return;
        }

        // Symbol lookup
        if (raw instanceof String sym) {
            try {
                mVal = mEnv.lookup(sym);
            } catch (EvalError e) {
                throw new EvalError(e.getMessage() + posStr(pos));
            }
            mApply = true; return;
        }

        // List (special form or application)
        if (raw instanceof List<?> list) {
            if (list.isEmpty()) throw new EvalError("empty application" + posStr(pos));
            Object headExpr = list.get(0);
            Object head = unwrap(headExpr);

            if (head instanceof String sym) {
                switch (sym) {
                    case "quote" -> {
                        if (list.size() != 2) throw new EvalError("quote requires 1 argument" + posStr(pos));
                        mVal = javaToScheme(list.get(1)); mApply = true; return;
                    }
                    case "if" -> {
                        if (list.size() < 3 || list.size() > 4)
                            throw new EvalError("if requires 2 or 3 arguments" + posStr(pos));
                        mK = new IfK(list.get(2), list.size() > 3 ? list.get(3) : null, mEnv, mK);
                        mExpr = list.get(1); return;
                    }
                    case "and" -> {
                        if (list.size() == 1) { mVal = Boolean.TRUE; mApply = true; return; }
                        if (list.size() == 2) { mExpr = list.get(1); return; }
                        mK = new AndK(list, 2, mEnv, mK);
                        mExpr = list.get(1); return;
                    }
                    case "or" -> {
                        if (list.size() == 1) { mVal = Boolean.FALSE; mApply = true; return; }
                        if (list.size() == 2) { mExpr = list.get(1); return; }
                        mK = new OrK(list, 2, mEnv, mK);
                        mExpr = list.get(1); return;
                    }
                    case "set!" -> {
                        if (list.size() != 3) throw new EvalError("set! requires 2 arguments" + posStr(pos));
                        Object nameRaw = unwrap(list.get(1));
                        if (!(nameRaw instanceof String name))
                            throw new EvalError("set!: first argument must be a symbol" + posStr(pos));
                        mK = new SetK(name, mEnv, pos, mK);
                        mExpr = list.get(2); return;
                    }
                    case "begin" -> {
                        if (list.size() == 1) { mVal = VOID; mApply = true; return; }
                        if (list.size() == 2) { mExpr = list.get(1); return; }
                        mK = new SeqK(list, 2, mEnv, mK);
                        mExpr = list.get(1); return;
                    }
                    case "define" -> { evalDefineStep(list, pos); return; }
                    case "lambda" -> { mVal = makeLambda(list, mEnv, pos); mApply = true; return; }
                    case "case-lambda" -> { mVal = makeCaseLambda(list, mEnv, pos); mApply = true; return; }
                    case "let" -> { evalLetStep(list, pos); return; }
                    case "let*" -> { evalLetStarStep(list, pos); return; }
                    case "letrec" -> { evalLetrecStep(list, pos, false); return; }
                    case "letrec*" -> { evalLetrecStep(list, pos, true); return; }
                    case "cond" -> { evalCondStep(list, 1); return; }
                    case "case" -> {
                        if (list.size() < 2) throw new EvalError("case requires at least a key" + posStr(pos));
                        mK = new CaseKeyK(list, mEnv, mK);
                        mExpr = list.get(1); return;
                    }
                    case "do" -> { evalDoStep(list, pos); return; }
                    case "define-syntax" -> { evalDefineSyntax(list, mEnv, pos); mVal = VOID; mApply = true; return; }
                    case "define-record-type" -> { evalDefineRecordType(list, mEnv, pos); mVal = VOID; mApply = true; return; }
                }
            }

            // Check for macro
            if (head instanceof String macroSym) {
                try {
                    Object val = mEnv.lookup(macroSym);
                    if (val instanceof SyntaxRules macro) {
                        Object[] result = expandMacroForm(macro, list, mEnv, pos);
                        mExpr = result[0];
                        mEnv = (Env) result[1];
                        return; // stay in eval mode
                    }
                } catch (EvalError ignore) {}
            }

            // Function application
            List<Object> argExprs = new ArrayList<>();
            for (int i = 1; i < list.size(); i++) argExprs.add(list.get(i));
            mK = new EvArgsK(argExprs, mEnv, pos, mK);
            mExpr = headExpr; return;
        }

        throw new EvalError("cannot eval: " + mExpr);
    }

    @SuppressWarnings("unchecked")
    private void applyStep() throws EvalError {
        if (mK instanceof IfK ik) {
            if (!isFalse(mVal)) {
                mExpr = ik.thenE; mEnv = ik.env; mK = ik.k; mApply = false;
            } else if (ik.elseE != null) {
                mExpr = ik.elseE; mEnv = ik.env; mK = ik.k; mApply = false;
            } else {
                mVal = VOID; mK = ik.k;
            }
            return;
        }

        if (mK instanceof SeqK sk) {
            if (sk.idx == sk.exprs.size() - 1) {
                // Last expr: tail position
                mExpr = sk.exprs.get(sk.idx); mEnv = sk.env; mK = sk.k; mApply = false;
            } else {
                mK = new SeqK(sk.exprs, sk.idx + 1, sk.env, sk.k);
                mExpr = sk.exprs.get(sk.idx); mEnv = sk.env; mApply = false;
            }
            return;
        }

        if (mK instanceof SetK sk) {
            try {
                sk.env.set(sk.name, mVal);
            } catch (EvalError e) {
                throw new EvalError(e.getMessage() + posStr(sk.pos));
            }
            mVal = VOID; mK = sk.k; return;
        }

        if (mK instanceof DefK dk) {
            dk.env.define(dk.name, mVal);
            mVal = VOID; mK = dk.k; return;
        }

        if (mK instanceof EvArgsK eak) {
            Object fun = mVal;
            if (eak.argExprs.isEmpty()) {
                mK = eak.k;
                cekApplyFun(fun, new ArrayList<>(), eak.pos);
            } else {
                // Evaluate arguments right-to-left (standard in Chez Scheme, required for reentrant continuations)
                int lastIdx = eak.argExprs.size() - 1;
                mK = new AccArgsK(fun, new ArrayList<>(), eak.argExprs, lastIdx - 1, eak.env, eak.pos, eak.k);
                mExpr = eak.argExprs.get(lastIdx); mEnv = eak.env; mApply = false;
            }
            return;
        }

        if (mK instanceof AccArgsK aak) {
            // Create a new list to avoid mutating captured continuations
            List<Object> newEvaled = new ArrayList<>(aak.evaled);
            newEvaled.add(mVal);
            if (aak.nextIdx < 0) {
                // All args evaluated (right-to-left), reverse to get correct order
                java.util.Collections.reverse(newEvaled);
                mK = aak.k;
                cekApplyFun(aak.fun, newEvaled, aak.pos);
            } else {
                mK = new AccArgsK(aak.fun, newEvaled, aak.argExprs, aak.nextIdx - 1, aak.env, aak.pos, aak.k);
                mExpr = aak.argExprs.get(aak.nextIdx); mEnv = aak.env; mApply = false;
            }
            return;
        }

        if (mK instanceof AndK ak) {
            if (isFalse(mVal)) {
                mK = ak.k; // return false value
            } else if (ak.nextIdx >= ak.form.size()) {
                mK = ak.k; // return last value
            } else if (ak.nextIdx == ak.form.size() - 1) {
                mExpr = ak.form.get(ak.nextIdx); mEnv = ak.env; mK = ak.k; mApply = false;
            } else {
                mK = new AndK(ak.form, ak.nextIdx + 1, ak.env, ak.k);
                mExpr = ak.form.get(ak.nextIdx); mEnv = ak.env; mApply = false;
            }
            return;
        }

        if (mK instanceof OrK ok) {
            if (!isFalse(mVal)) {
                mK = ok.k; // return true-ish value
            } else if (ok.nextIdx >= ok.form.size()) {
                mK = ok.k; // return last value
            } else if (ok.nextIdx == ok.form.size() - 1) {
                mExpr = ok.form.get(ok.nextIdx); mEnv = ok.env; mK = ok.k; mApply = false;
            } else {
                mK = new OrK(ok.form, ok.nextIdx + 1, ok.env, ok.k);
                mExpr = ok.form.get(ok.nextIdx); mEnv = ok.env; mApply = false;
            }
            return;
        }

        if (mK instanceof LetBindK lk) {
            lk.letEnv.define(lk.name, mVal);
            if (lk.nextIdx < lk.bindings.size()) {
                Object bRaw = unwrap(lk.bindings.get(lk.nextIdx));
                if (!(bRaw instanceof List<?> binding) || binding.size() != 2)
                    throw new EvalError("invalid let binding");
                String nextName = (String) unwrap(binding.get(0));
                mK = new LetBindK(nextName, lk.bindings, lk.nextIdx + 1, lk.evalEnv, lk.letEnv, lk.form, lk.bodyStart, lk.k);
                mExpr = binding.get(1); mEnv = lk.evalEnv; mApply = false;
            } else {
                startBody(lk.form, lk.bodyStart, lk.letEnv, lk.k);
            }
            return;
        }

        if (mK instanceof CondK ck) {
            if (!isFalse(mVal)) {
                if (ck.clause.size() == 1) {
                    mK = ck.k; // return test value
                } else {
                    mK = ck.k;
                    startBody(ck.clause, 1, ck.env, ck.k);
                }
            } else {
                mK = ck.k;
                mEnv = ck.env;
                evalCondStep(ck.form, ck.nextClauseIdx);
            }
            return;
        }

        if (mK instanceof CaseKeyK ck) {
            Object key = mVal;
            for (int i = 2; i < ck.form.size(); i++) {
                Object clauseRaw = unwrap(ck.form.get(i));
                if (!(clauseRaw instanceof List<?> clause) || clause.isEmpty()) continue;
                Object datums = unwrap(clause.get(0));
                if ("else".equals(datums)) {
                    mK = ck.k;
                    startBody(clause, 1, ck.env, ck.k);
                    return;
                }
                if (datums instanceof List<?> datumList && matchesCaseDatum(key, datumList)) {
                    mK = ck.k;
                    startBody(clause, 1, ck.env, ck.k);
                    return;
                }
            }
            mVal = VOID; mK = ck.k; return;
        }

        if (mK instanceof DoInitK dk) {
            dk.doEnv.define(dk.varNames[dk.idx], mVal);
            int next = dk.idx + 1;
            if (next < dk.varNames.length) {
                mK = new DoInitK(next, dk.varNames, dk.initExprs, dk.stepExprs, dk.hasStep, dk.doEnv, dk.testClause, dk.form, dk.outerEnv, dk.k);
                mExpr = dk.initExprs[next]; mEnv = dk.outerEnv; mApply = false;
            } else {
                // All inits done, start loop
                mK = new DoTestK(dk.varNames, dk.stepExprs, dk.hasStep, dk.testClause, dk.form, dk.doEnv, dk.k);
                mExpr = dk.testClause.get(0); mEnv = dk.doEnv; mApply = false;
            }
            return;
        }

        if (mK instanceof DoTestK dt) {
            if (!isFalse(mVal)) {
                // Test passed, evaluate result body
                mK = dt.k;
                startBody(dt.testClause, 1, dt.doEnv, dt.k);
            } else {
                // Test failed, evaluate commands then step
                int numCommands = dt.form.size() - 3;
                DoStepStartK dss = new DoStepStartK(dt.varNames, dt.stepExprs, dt.hasStep, dt.testClause, dt.form, dt.doEnv, dt.k);
                if (numCommands > 0) {
                    mK = dss;
                    if (numCommands == 1) {
                        mExpr = dt.form.get(3); mEnv = dt.doEnv; mApply = false;
                    } else {
                        mK = new SeqK(dt.form, 4, dt.doEnv, dss);
                        mExpr = dt.form.get(3); mEnv = dt.doEnv; mApply = false;
                    }
                } else {
                    mK = dss;
                    mVal = VOID; // trigger DoStepStartK immediately
                }
            }
            return;
        }

        if (mK instanceof DoStepStartK ds) {
            // Start evaluating step expressions
            int firstStep = -1;
            for (int i = 0; i < ds.varNames.length; i++) {
                if (ds.hasStep[i]) { firstStep = i; break; }
            }
            if (firstStep == -1) {
                // No step expressions, loop back to test
                mK = new DoTestK(ds.varNames, ds.stepExprs, ds.hasStep, ds.testClause, ds.form, ds.doEnv, ds.k);
                mExpr = ds.testClause.get(0); mEnv = ds.doEnv; mApply = false;
            } else {
                Object[] newVals = new Object[ds.varNames.length];
                mK = new DoStepK(firstStep, newVals, ds.varNames, ds.stepExprs, ds.hasStep, ds.testClause, ds.form, ds.doEnv, ds.k);
                mExpr = ds.stepExprs[firstStep]; mEnv = ds.doEnv; mApply = false;
            }
            return;
        }

        if (mK instanceof DoStepK dsk) {
            dsk.newVals[dsk.idx] = mVal;
            int nextStep = -1;
            for (int i = dsk.idx + 1; i < dsk.varNames.length; i++) {
                if (dsk.hasStep[i]) { nextStep = i; break; }
            }
            if (nextStep == -1) {
                // All steps evaluated, update vars and loop
                for (int i = 0; i < dsk.varNames.length; i++) {
                    if (dsk.hasStep[i]) dsk.doEnv.define(dsk.varNames[i], dsk.newVals[i]);
                }
                mK = new DoTestK(dsk.varNames, dsk.stepExprs, dsk.hasStep, dsk.testClause, dsk.form, dsk.doEnv, dsk.k);
                mExpr = dsk.testClause.get(0); mEnv = dsk.doEnv; mApply = false;
            } else {
                mK = new DoStepK(nextStep, dsk.newVals, dsk.varNames, dsk.stepExprs, dsk.hasStep, dsk.testClause, dsk.form, dsk.doEnv, dsk.k);
                mExpr = dsk.stepExprs[nextStep]; mEnv = dsk.doEnv; mApply = false;
            }
            return;
        }

        if (mK instanceof MapK mk) {
            List<Object> newResults = new ArrayList<>(mk.results);
            newResults.add(mVal);
            boolean done = false;
            for (Object lst : mk.remainingLists) {
                if (!(lst instanceof Cons)) { done = true; break; }
            }
            if (done) {
                Object result = NIL;
                for (int i = newResults.size() - 1; i >= 0; i--) result = new Cons(newResults.get(i), result);
                mVal = result; mK = mk.k;
            } else {
                List<Object> nextArgs = new ArrayList<>();
                List<Object> nextRemaining = new ArrayList<>();
                for (Object lst : mk.remainingLists) {
                    Cons c = (Cons) lst;
                    nextArgs.add(c.car);
                    nextRemaining.add(c.cdr);
                }
                mK = new MapK(mk.fun, nextRemaining, newResults, mk.pos, mk.k);
                cekApplyFun(mk.fun, nextArgs, mk.pos);
            }
            return;
        }

        if (mK instanceof ForEachK fk) {
            boolean done = false;
            for (Object lst : fk.remainingLists) {
                if (!(lst instanceof Cons)) { done = true; break; }
            }
            if (done) {
                mVal = VOID; mK = fk.k;
            } else {
                List<Object> nextArgs = new ArrayList<>();
                List<Object> nextRemaining = new ArrayList<>();
                for (Object lst : fk.remainingLists) {
                    Cons c = (Cons) lst;
                    nextArgs.add(c.car);
                    nextRemaining.add(c.cdr);
                }
                mK = new ForEachK(fk.fun, nextRemaining, fk.pos, fk.k);
                cekApplyFun(fk.fun, nextArgs, fk.pos);
            }
            return;
        }

        throw new EvalError("unknown continuation type: " + mK.getClass().getSimpleName());
    }

    private void cekApplyFun(Object fun, List<Object> args, Pos pos) throws EvalError {
        if (fun instanceof Lambda lam) {
            Env callEnv = bindLambdaArgs(lam, args, pos);
            if (lam.body().size() == 1) {
                mExpr = lam.body().get(0); mEnv = callEnv; mApply = false;
            } else {
                mK = new SeqK(lam.body(), 1, callEnv, mK);
                mExpr = lam.body().get(0); mEnv = callEnv; mApply = false;
            }
            return;
        }

        if (fun instanceof CaseLambda cl) {
            Lambda lam = findMatchingClause(cl, args, pos);
            Env callEnv = bindLambdaArgs(lam, args, pos);
            if (lam.body().size() == 1) {
                mExpr = lam.body().get(0); mEnv = callEnv; mApply = false;
            } else {
                mK = new SeqK(lam.body(), 1, callEnv, mK);
                mExpr = lam.body().get(0); mEnv = callEnv; mApply = false;
            }
            return;
        }

        if (fun instanceof SchemeContinuation cont) {
            if (args.size() != 1) throw new EvalError("continuation requires exactly 1 argument" + posStr(pos));
            mVal = args.get(0);
            mK = cont.k;
            mApply = true;
            return;
        }

        if (fun instanceof Builtin b) {
            String name = b.name();

            // call/cc
            if ("call/cc".equals(name) || "call-with-current-continuation".equals(name)) {
                if (args.size() != 1) throw new EvalError("call/cc requires 1 argument" + posStr(pos));
                SchemeContinuation captured = new SchemeContinuation(mK);
                cekApplyFun(args.get(0), List.of(captured), pos);
                return;
            }

            // apply
            if ("apply".equals(name)) {
                if (args.size() < 2) throw new EvalError("apply requires at least 2 arguments" + posStr(pos));
                Object proc = args.get(0);
                Object lastArg = args.get(args.size() - 1);
                List<Object> callArgs = new ArrayList<>();
                for (int i = 1; i < args.size() - 1; i++) callArgs.add(args.get(i));
                Object cur = lastArg;
                while (cur instanceof Cons c) { callArgs.add(c.car); cur = c.cdr; }
                if (cur != NIL) throw new EvalError("apply: last argument must be a proper list" + posStr(pos));
                cekApplyFun(proc, callArgs, pos);
                return;
            }

            // map
            if ("map".equals(name)) {
                if (args.size() < 2) throw new EvalError("map requires at least 2 arguments" + posStr(pos));
                Object f = args.get(0);
                List<Object> lists = new ArrayList<>(args.subList(1, args.size()));
                boolean anyNull = false;
                for (Object lst : lists) { if (!(lst instanceof Cons)) { anyNull = true; break; } }
                if (anyNull) {
                    mVal = NIL; mApply = true;
                } else {
                    List<Object> fArgs = new ArrayList<>();
                    List<Object> remainingLists = new ArrayList<>();
                    for (Object lst : lists) {
                        Cons c = (Cons) lst;
                        fArgs.add(c.car);
                        remainingLists.add(c.cdr);
                    }
                    mK = new MapK(f, remainingLists, new ArrayList<>(), pos, mK);
                    cekApplyFun(f, fArgs, pos);
                }
                return;
            }

            // for-each
            if ("for-each".equals(name)) {
                if (args.size() < 2) throw new EvalError("for-each requires at least 2 arguments" + posStr(pos));
                Object f = args.get(0);
                List<Object> lists = new ArrayList<>(args.subList(1, args.size()));
                boolean anyNull = false;
                for (Object lst : lists) { if (!(lst instanceof Cons)) { anyNull = true; break; } }
                if (anyNull) {
                    mVal = VOID; mApply = true;
                } else {
                    List<Object> fArgs = new ArrayList<>();
                    List<Object> remainingLists = new ArrayList<>();
                    for (Object lst : lists) {
                        Cons c = (Cons) lst;
                        fArgs.add(c.car);
                        remainingLists.add(c.cdr);
                    }
                    mK = new ForEachK(f, remainingLists, pos, mK);
                    cekApplyFun(f, fArgs, pos);
                }
                return;
            }

            // Regular builtin
            try {
                mVal = b.apply(args);
            } catch (EvalError e) {
                String msg = e.getMessage();
                if (pos != null && !msg.matches(".*\\d+:\\d+.*")) {
                    throw new EvalError(msg + " at " + pos);
                }
                throw e;
            }
            mApply = true;
            return;
        }

        throw new EvalError("not a procedure: " + schemeToString(fun) + posStr(pos));
    }

    // Helper: set up evaluation of body expressions
    private void startBody(List<?> form, int start, Env env, Kont k) {
        int bodyLen = form.size() - start;
        if (bodyLen <= 0) {
            mVal = VOID; mK = k; mApply = true;
        } else if (bodyLen == 1) {
            mExpr = form.get(start); mEnv = env; mK = k; mApply = false;
        } else {
            mK = new SeqK(form, start + 1, env, k);
            mExpr = form.get(start); mEnv = env; mApply = false;
        }
    }

    // ===== Special form step handlers =====

    private void evalDefineStep(List<?> list, Pos pos) throws EvalError {
        if (list.size() < 3) throw new EvalError("define requires at least 2 arguments" + posStr(pos));
        Object target = unwrap(list.get(1));
        if (target instanceof String name) {
            mK = new DefK(name, mEnv, mK);
            mExpr = list.get(2); mApply = false;
            return;
        }
        if (target instanceof List<?> sig) {
            if (sig.isEmpty() || !(unwrap(sig.get(0)) instanceof String name))
                throw new EvalError("invalid define" + posStr(pos));
            List<String> params = new ArrayList<>();
            String restParam = parseParamList(sig, 1, pos);
            for (int i = 1; i < sig.size(); i++) {
                String p = unwrap(sig.get(i)) instanceof String s ? s : null;
                if (p == null) throw new EvalError("parameter must be a symbol" + posStr(pos));
                if (".".equals(p)) break;
                params.add(p);
            }
            List<Object> body = new ArrayList<>(list.subList(2, list.size()));
            mEnv.define(name, new Lambda(params, restParam, body, mEnv));
            mVal = VOID; mApply = true;
            return;
        }
        throw new EvalError("invalid define" + posStr(pos));
    }

    @SuppressWarnings("unchecked")
    private void evalLetStep(List<?> list, Pos pos) throws EvalError {
        if (list.size() < 3) throw new EvalError("let requires bindings and body" + posStr(pos));
        Object second = unwrap(list.get(1));

        if (second instanceof String loopName) {
            // Named let
            if (list.size() < 4) throw new EvalError("named let requires bindings and body" + posStr(pos));
            Object bindingsRaw = unwrap(list.get(2));
            if (!(bindingsRaw instanceof List<?> bindings))
                throw new EvalError("let bindings must be a list" + posStr(pos));
            List<String> params = new ArrayList<>();
            List<Object> initExprs = new ArrayList<>();
            for (Object b : bindings) {
                Object bRaw = unwrap(b);
                if (!(bRaw instanceof List<?> binding) || binding.size() != 2)
                    throw new EvalError("invalid let binding" + posStr(pos));
                if (!(unwrap(binding.get(0)) instanceof String pname))
                    throw new EvalError("let binding name must be a symbol" + posStr(pos));
                params.add(pname);
                initExprs.add(binding.get(1));
            }
            List<Object> body = new ArrayList<>(list.subList(3, list.size()));
            Env letEnv = new Env(mEnv);
            Lambda loopFn = new Lambda(params, null, body, letEnv);
            letEnv.define(loopName, loopFn);

            if (initExprs.isEmpty()) {
                cekApplyFun(loopFn, new ArrayList<>(), pos);
            } else {
                // Evaluate init expressions right-to-left (consistent with function args)
                int lastIdx = initExprs.size() - 1;
                mK = new AccArgsK(loopFn, new ArrayList<>(), initExprs, lastIdx - 1, mEnv, pos, mK);
                mExpr = initExprs.get(lastIdx); mApply = false;
                // mEnv stays as outer env for evaluating inits
            }
            return;
        }

        // Regular let
        if (!(second instanceof List<?> bindingsList))
            throw new EvalError("let bindings must be a list" + posStr(pos));
        Env letEnv = new Env(mEnv);
        if (bindingsList.isEmpty()) {
            startBody(list, 2, letEnv, mK);
            return;
        }
        Object bRaw = unwrap(bindingsList.get(0));
        if (!(bRaw instanceof List<?> binding) || binding.size() != 2)
            throw new EvalError("invalid let binding" + posStr(pos));
        String firstName = (String) unwrap(binding.get(0));
        mK = new LetBindK(firstName, bindingsList, 1, mEnv, letEnv, list, 2, mK);
        mExpr = binding.get(1); mApply = false;
        // mEnv stays as outer env
    }

    @SuppressWarnings("unchecked")
    private void evalLetStarStep(List<?> list, Pos pos) throws EvalError {
        if (list.size() < 3) throw new EvalError("let* requires bindings and body" + posStr(pos));
        Object bindingsRaw = unwrap(list.get(1));
        if (!(bindingsRaw instanceof List<?> bindingsList))
            throw new EvalError("let* bindings must be a list" + posStr(pos));
        Env letEnv = new Env(mEnv);
        if (bindingsList.isEmpty()) {
            startBody(list, 2, letEnv, mK);
            return;
        }
        Object bRaw = unwrap(bindingsList.get(0));
        if (!(bRaw instanceof List<?> binding) || binding.size() != 2)
            throw new EvalError("invalid let* binding" + posStr(pos));
        String firstName = (String) unwrap(binding.get(0));
        // For let*, evalEnv == letEnv (each binding sees previous)
        mK = new LetBindK(firstName, bindingsList, 1, letEnv, letEnv, list, 2, mK);
        mExpr = binding.get(1); mEnv = letEnv; mApply = false;
    }

    @SuppressWarnings("unchecked")
    private void evalLetrecStep(List<?> list, Pos pos, boolean star) throws EvalError {
        if (list.size() < 3) throw new EvalError((star ? "letrec*" : "letrec") + " requires bindings and body" + posStr(pos));
        Object bindingsRaw = unwrap(list.get(1));
        if (!(bindingsRaw instanceof List<?> bindingsList))
            throw new EvalError((star ? "letrec*" : "letrec") + " bindings must be a list" + posStr(pos));
        Env letEnv = new Env(mEnv);
        // Pre-define all names as VOID
        for (Object b : bindingsList) {
            Object bRaw = unwrap(b);
            if (!(bRaw instanceof List<?> binding) || binding.size() != 2)
                throw new EvalError("invalid letrec binding" + posStr(pos));
            String name = (String) unwrap(binding.get(0));
            letEnv.define(name, VOID);
        }
        if (bindingsList.isEmpty()) {
            startBody(list, 2, letEnv, mK);
            return;
        }
        Object bRaw = unwrap(bindingsList.get(0));
        List<?> binding = (List<?>) bRaw;
        String firstName = (String) unwrap(binding.get(0));
        // For letrec/letrec*, evalEnv = letEnv (bindings evaluated in the letrec env)
        mK = new LetBindK(firstName, bindingsList, 1, letEnv, letEnv, list, 2, mK);
        mExpr = binding.get(1); mEnv = letEnv; mApply = false;
    }

    @SuppressWarnings("unchecked")
    private void evalCondStep(List<?> form, int clauseIdx) throws EvalError {
        if (clauseIdx >= form.size()) {
            mVal = VOID; mApply = true; return;
        }
        Object clauseRaw = unwrap(form.get(clauseIdx));
        if (!(clauseRaw instanceof List<?> clause) || clause.isEmpty())
            throw new EvalError("invalid cond clause");
        if ("else".equals(unwrap(clause.get(0)))) {
            startBody(clause, 1, mEnv, mK);
            return;
        }
        mK = new CondK(clause, form, clauseIdx + 1, mEnv, mK);
        mExpr = clause.get(0); mApply = false; // eval test
    }

    @SuppressWarnings("unchecked")
    private void evalDoStep(List<?> list, Pos pos) throws EvalError {
        if (list.size() < 3) throw new EvalError("do requires bindings and test" + posStr(pos));
        Object varsRaw = unwrap(list.get(1));
        if (!(varsRaw instanceof List<?> varSpecs))
            throw new EvalError("do: bindings must be a list" + posStr(pos));
        Object testRaw = unwrap(list.get(2));
        if (!(testRaw instanceof List<?> testClause) || testClause.isEmpty())
            throw new EvalError("do: test must be a list" + posStr(pos));

        int numVars = varSpecs.size();
        String[] varNames = new String[numVars];
        Object[] stepExprs = new Object[numVars];
        boolean[] hasStep = new boolean[numVars];
        Object[] initExprs = new Object[numVars];

        for (int i = 0; i < numVars; i++) {
            Object specRaw = unwrap(varSpecs.get(i));
            if (!(specRaw instanceof List<?> spec) || spec.size() < 2)
                throw new EvalError("do: invalid variable spec" + posStr(pos));
            if (!(unwrap(spec.get(0)) instanceof String vname))
                throw new EvalError("do: variable name must be a symbol" + posStr(pos));
            varNames[i] = vname;
            initExprs[i] = spec.get(1);
            if (spec.size() >= 3) { stepExprs[i] = spec.get(2); hasStep[i] = true; }
        }

        Env doEnv = new Env(mEnv);

        if (numVars == 0) {
            mK = new DoTestK(varNames, stepExprs, hasStep, testClause, list, doEnv, mK);
            mExpr = testClause.get(0); mEnv = doEnv; mApply = false;
        } else {
            mK = new DoInitK(0, varNames, initExprs, stepExprs, hasStep, doEnv, testClause, list, mEnv, mK);
            mExpr = initExprs[0]; mApply = false;
            // mEnv stays as outer env for evaluating inits
        }
    }

    // ===== Lambda/CaseLambda creation =====

    private Lambda makeLambda(List<?> list, Env env, Pos pos) throws EvalError {
        if (list.size() < 3) throw new EvalError("lambda requires params and body" + posStr(pos));
        Object paramSpec = unwrap(list.get(1));
        if (!(paramSpec instanceof List<?> paramList))
            throw new EvalError("lambda params must be a list" + posStr(pos));
        List<String> params = new ArrayList<>();
        String restParam = parseParamList(paramList, 0, pos);
        for (int pi = 0; pi < paramList.size(); pi++) {
            String s = unwrap(paramList.get(pi)) instanceof String str ? str : null;
            if (s == null) throw new EvalError("parameter must be a symbol" + posStr(pos));
            if (".".equals(s)) break;
            params.add(s);
        }
        List<Object> body = new ArrayList<>(list.subList(2, list.size()));
        return new Lambda(params, restParam, body, env);
    }

    private CaseLambda makeCaseLambda(List<?> list, Env env, Pos pos) throws EvalError {
        List<Lambda> clauses = new ArrayList<>();
        for (int i = 1; i < list.size(); i++) {
            Object clauseRaw = unwrap(list.get(i));
            if (!(clauseRaw instanceof List<?> clause) || clause.size() < 2)
                throw new EvalError("case-lambda: invalid clause" + posStr(pos));
            Object paramSpec = unwrap(clause.get(0));
            if (!(paramSpec instanceof List<?> paramList))
                throw new EvalError("case-lambda: params must be a list" + posStr(pos));
            List<String> params = new ArrayList<>();
            String restParam = parseParamList(paramList, 0, pos);
            for (int pi = 0; pi < paramList.size(); pi++) {
                String s = unwrap(paramList.get(pi)) instanceof String str ? str : null;
                if (s == null) throw new EvalError("parameter must be a symbol" + posStr(pos));
                if (".".equals(s)) break;
                params.add(s);
            }
            List<Object> body = new ArrayList<>(clause.subList(1, clause.size()));
            clauses.add(new Lambda(params, restParam, body, env));
        }
        return new CaseLambda(clauses);
    }

    // ===== Helper methods =====

    private String parseParamList(List<?> paramList, int start, Pos pos) throws EvalError {
        for (int i = start; i < paramList.size(); i++) {
            String s = unwrap(paramList.get(i)) instanceof String str ? str : null;
            if (s == null) throw new EvalError("parameter must be a symbol" + posStr(pos));
            if (".".equals(s)) {
                if (i + 1 >= paramList.size()) throw new EvalError("missing rest parameter after ." + posStr(pos));
                String rp = unwrap(paramList.get(i + 1)) instanceof String r ? r : null;
                if (rp == null) throw new EvalError("rest parameter must be a symbol" + posStr(pos));
                return rp;
            }
        }
        return null;
    }

    boolean isFalse(Object val) {
        return val instanceof Boolean b && !b;
    }

    boolean schemeEqv(Object a, Object b) {
        if (a == b) return true;
        if (a instanceof Long la && b instanceof Long lb) return la.equals(lb);
        if (a instanceof SchemeRational ra && b instanceof SchemeRational rb) return ra.equals(rb);
        if (a instanceof Long la && b instanceof SchemeRational rb) return new SchemeRational(la, 1).equals(rb);
        if (a instanceof SchemeRational ra && b instanceof Long lb) return ra.equals(new SchemeRational(lb, 1));
        if (a instanceof Double da && b instanceof Double db) return da.equals(db);
        if (a instanceof Boolean ba && b instanceof Boolean bb) return ba.equals(bb);
        if (a instanceof String sa && b instanceof String sb) return sa.equals(sb);
        if (a instanceof SchemeChar ca && b instanceof SchemeChar cb) return ca.value() == cb.value();
        return false;
    }

    boolean schemeEqual(Object a, Object b) {
        return schemeEqualRec(a, b, new java.util.IdentityHashMap<>());
    }

    private boolean schemeEqualRec(Object a, Object b, java.util.IdentityHashMap<Object, Object> seen) {
        if (a == b) return true;
        if (schemeEqv(a, b)) return true;
        if (a instanceof SchemeString sa && b instanceof SchemeString sb) return sa.value().equals(sb.value());
        if (a == NIL && b == NIL) return true;
        if (a instanceof Cons ca && b instanceof Cons cb) {
            Object prev = seen.get(a);
            if (prev == b) return true;
            seen.put(a, b);
            return schemeEqualRec(ca.car, cb.car, seen) && schemeEqualRec(ca.cdr, cb.cdr, seen);
        }
        if (a instanceof SchemeVector va && b instanceof SchemeVector vb) {
            if (va.length() != vb.length()) return false;
            for (int i = 0; i < va.length(); i++) {
                if (!schemeEqualRec(va.data[i], vb.data[i], seen)) return false;
            }
            return true;
        }
        return false;
    }

    private Env bindLambdaArgs(Lambda lam, List<Object> args, Pos pos) throws EvalError {
        if (lam.restParam() != null) {
            if (args.size() < lam.params().size()) {
                throw new EvalError("expected at least " + lam.params().size() + " arguments, got " + args.size() + posStr(pos));
            }
        } else if (args.size() != lam.params().size()) {
            throw new EvalError("expected " + lam.params().size() + " arguments, got " + args.size() + posStr(pos));
        }
        Env callEnv = new Env(lam.env());
        for (int i = 0; i < lam.params().size(); i++) {
            callEnv.define(lam.params().get(i), args.get(i));
        }
        if (lam.restParam() != null) {
            Object rest = NIL;
            for (int i = args.size() - 1; i >= lam.params().size(); i--) {
                rest = new Cons(args.get(i), rest);
            }
            callEnv.define(lam.restParam(), rest);
        }
        return callEnv;
    }

    private Lambda findMatchingClause(CaseLambda cl, List<Object> args, Pos pos) throws EvalError {
        for (Lambda lam : cl.clauses()) {
            if (lam.restParam() != null) {
                if (args.size() >= lam.params().size()) return lam;
            } else {
                if (args.size() == lam.params().size()) return lam;
            }
        }
        throw new EvalError("no matching clause in case-lambda for " + args.size() + " arguments" + posStr(pos));
    }

    private boolean matchesCaseDatum(Object key, List<?> datumList) {
        for (Object d : datumList) {
            Object dv = unwrap(d);
            if (dv instanceof SchemeString || dv instanceof Long || dv instanceof Boolean
                    || dv instanceof Double || dv instanceof SchemeRational || dv instanceof SchemeChar) {
                if (schemeEqv(key, dv)) return true;
            } else if (dv instanceof String dsym && key instanceof String ks && ks.equals(dsym)) {
                return true;
            }
        }
        return false;
    }

    long requireLong(Object val) throws EvalError {
        if (val instanceof Long l) return l;
        throw new EvalError("expected number, got: " + schemeToString(val));
    }

    void requireArgCount(List<Object> args, int n, String name) throws EvalError {
        if (args.size() != n) {
            throw new EvalError(name + " requires " + n + " arguments, got " + args.size());
        }
    }

    // Legacy applyProc for Builtins compatibility
    Object applyProc(Object proc, List<Object> args, Pos pos) throws EvalError {
        // Save CEK state
        Object savedExpr = mExpr; Env savedEnv = mEnv; Kont savedK = mK;
        Object savedVal = mVal; boolean savedApply = mApply;
        // Set up to apply function
        mK = HaltK.INST;
        cekApplyFun(proc, args, pos);
        // Run CEK machine until done
        Object result;
        while (true) {
            if (mApply) {
                if (mK instanceof HaltK) { result = mVal; break; }
                applyStep();
            } else {
                evalStep();
            }
        }
        // Restore state
        mExpr = savedExpr; mEnv = savedEnv; mK = savedK;
        mVal = savedVal; mApply = savedApply;
        return result;
    }

    private Object[] expandMacroForm(SyntaxRules macro, List<?> form, Env env, Pos pos) throws EvalError {
        return macroExpander.expandMacroForm(macro, form, env, pos);
    }

    private void evalDefineSyntax(List<?> list, Env env, Pos pos) throws EvalError {
        macroExpander.evalDefineSyntax(list, env, pos);
    }

    private void evalDefineRecordType(List<?> list, Env env, Pos pos) throws EvalError {
        macroExpander.evalDefineRecordType(list, env, pos);
    }

    // ===== Conversion =====

    private Object javaToScheme(Object val) {
        if (val instanceof Token t) return javaToScheme(t.value());
        if (val instanceof List<?> list) {
            Object result = NIL;
            for (int i = list.size() - 1; i >= 0; i--) {
                result = new Cons(javaToScheme(list.get(i)), result);
            }
            return result;
        }
        return val;
    }

    // ===== Output formatting =====

    String displayString(Object val) {
        if (val instanceof SchemeString s) return s.value();
        return schemeToString(val);
    }

    String schemeToString(Object val) {
        return schemeToStringRec(val, new java.util.IdentityHashMap<>());
    }

    private String schemeToStringRec(Object val, java.util.IdentityHashMap<Object, Boolean> seen) {
        if (val == NIL) return "()";
        if (val instanceof Long l) return l.toString();
        if (val instanceof SchemeRational r) return r.isInteger() ? String.valueOf(r.toLong()) : r.num + "/" + r.den;
        if (val instanceof Double d) {
            if (d == Math.floor(d) && !Double.isInfinite(d)) return String.valueOf(d);
            return String.valueOf(d);
        }
        if (val instanceof Boolean b) return b ? "#t" : "#f";
        if (val instanceof SchemeString s) return "\"" + s.value() + "\"";
        if (val instanceof SchemeChar c) {
            return switch (c.value()) {
                case ' ' -> "#\\space";
                case '\n' -> "#\\newline";
                case '\t' -> "#\\tab";
                default -> "#\\" + c.value();
            };
        }
        if (val instanceof SchemeVector v) {
            if (seen.containsKey(v)) return "#<cycle>";
            seen.put(v, Boolean.TRUE);
            StringBuilder sb = new StringBuilder("#(");
            for (int i = 0; i < v.length(); i++) {
                if (i > 0) sb.append(" ");
                sb.append(schemeToStringRec(v.data[i], seen));
            }
            sb.append(")");
            seen.remove(v);
            return sb.toString();
        }
        if (val instanceof SchemeRecord r) return "#<record " + r.type.name + ">";
        if (val instanceof Builtin b) return "#<procedure " + b.name() + ">";
        if (val instanceof Lambda) return "#<procedure>";
        if (val instanceof CaseLambda) return "#<procedure>";
        if (val instanceof SchemeContinuation) return "#<procedure>";
        if (val instanceof Cons) {
            if (seen.containsKey(val)) return "#<cycle>";
            seen.put(val, Boolean.TRUE);
            StringBuilder sb = new StringBuilder("(");
            Object cur = val;
            boolean first = true;
            while (cur instanceof Cons c) {
                if (!first) {
                    if (seen.containsKey(cur)) { sb.append(" . #<cycle>"); break; }
                    seen.put(cur, Boolean.TRUE);
                }
                if (!first) sb.append(" ");
                first = false;
                sb.append(schemeToStringRec(c.car, seen));
                cur = c.cdr;
            }
            if (cur != NIL && !(cur instanceof Cons)) {
                sb.append(" . ");
                sb.append(schemeToStringRec(cur, seen));
            }
            sb.append(")");
            return sb.toString();
        }
        if (val instanceof List<?> list) {
            StringBuilder sb = new StringBuilder("(");
            for (int i = 0; i < list.size(); i++) {
                if (i > 0) sb.append(" ");
                sb.append(schemeToStringRec(list.get(i), seen));
            }
            sb.append(")");
            return sb.toString();
        }
        if (val instanceof String s) return s;
        return String.valueOf(val);
    }
}
