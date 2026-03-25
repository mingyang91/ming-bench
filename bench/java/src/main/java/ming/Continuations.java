package ming;

import java.util.ArrayList;
import java.util.List;

/**
 * CEK machine continuation types and related data structures for the Scheme interpreter.
 */
final class Continuations {

    private Continuations() {}

    // ===== Continuation types for CEK machine =====

    static abstract class Kont {}

    static final class HaltK extends Kont {
        static final HaltK INST = new HaltK();
    }

    static final class IfK extends Kont {
        final Object thenE, elseE;
        final Evaluator.Env env; final Kont k;
        IfK(Object t, Object e, Evaluator.Env env, Kont k) { thenE=t; elseE=e; this.env=env; this.k=k; }
    }

    // Sequence: evaluate exprs[idx], then exprs[idx+1], ..., with last in tail position
    static final class SeqK extends Kont {
        final List<?> exprs; final int idx;
        final Evaluator.Env env; final Kont k;
        SeqK(List<?> es, int i, Evaluator.Env env, Kont k) { exprs=es; idx=i; this.env=env; this.k=k; }
    }

    static final class SetK extends Kont {
        final String name; final Evaluator.Env env; final Pos pos; final Kont k;
        SetK(String n, Evaluator.Env env, Pos p, Kont k) { name=n; this.env=env; pos=p; this.k=k; }
    }

    static final class DefK extends Kont {
        final String name; final Evaluator.Env env; final Kont k;
        DefK(String n, Evaluator.Env env, Kont k) { name=n; this.env=env; this.k=k; }
    }

    // Evaluated operator, now start evaluating args
    static final class EvArgsK extends Kont {
        final List<Object> argExprs;
        final Evaluator.Env env; final Pos pos; final Kont k;
        EvArgsK(List<Object> ae, Evaluator.Env env, Pos pos, Kont k) { argExprs=ae; this.env=env; this.pos=pos; this.k=k; }
    }

    // Accumulating evaluated args
    static final class AccArgsK extends Kont {
        final Object fun;
        final List<Object> evaled;
        final List<Object> argExprs;
        final int nextIdx;
        final Evaluator.Env env; final Pos pos; final Kont k;
        AccArgsK(Object f, List<Object> ev, List<Object> ae, int ni, Evaluator.Env env, Pos pos, Kont k) {
            fun=f; evaled=ev; argExprs=ae; nextIdx=ni; this.env=env; this.pos=pos; this.k=k;
        }
    }

    static final class AndK extends Kont {
        final List<?> form; final int nextIdx;
        final Evaluator.Env env; final Kont k;
        AndK(List<?> f, int i, Evaluator.Env env, Kont k) { form=f; nextIdx=i; this.env=env; this.k=k; }
    }

    static final class OrK extends Kont {
        final List<?> form; final int nextIdx;
        final Evaluator.Env env; final Kont k;
        OrK(List<?> f, int i, Evaluator.Env env, Kont k) { form=f; nextIdx=i; this.env=env; this.k=k; }
    }

    // Let/let*/letrec bindings
    static final class LetBindK extends Kont {
        final String name;
        final List<?> bindings; final int nextIdx;
        final Evaluator.Env evalEnv; final Evaluator.Env letEnv;
        final List<?> form; final int bodyStart;
        final Kont k;
        LetBindK(String n, List<?> b, int ni, Evaluator.Env ee, Evaluator.Env le, List<?> f, int bs, Kont k) {
            name=n; bindings=b; nextIdx=ni; evalEnv=ee; letEnv=le; form=f; bodyStart=bs; this.k=k;
        }
    }

    // Cond: evaluated a test
    static final class CondK extends Kont {
        final List<?> clause;
        final List<?> form; final int nextClauseIdx;
        final Evaluator.Env env; final Kont k;
        CondK(List<?> c, List<?> f, int ni, Evaluator.Env env, Kont k) { clause=c; form=f; nextClauseIdx=ni; this.env=env; this.k=k; }
    }

    // Case: evaluated the key
    static final class CaseKeyK extends Kont {
        final List<?> form; final Evaluator.Env env; final Kont k;
        CaseKeyK(List<?> f, Evaluator.Env env, Kont k) { form=f; this.env=env; this.k=k; }
    }

    // Do: init evaluation
    static final class DoInitK extends Kont {
        final int idx;
        final String[] varNames; final Object[] initExprs;
        final Object[] stepExprs; final boolean[] hasStep;
        final Evaluator.Env doEnv; final List<?> testClause; final List<?> form;
        final Evaluator.Env outerEnv; final Kont k;
        DoInitK(int i, String[] vn, Object[] ie, Object[] se, boolean[] hs, Evaluator.Env de, List<?> tc, List<?> f, Evaluator.Env oe, Kont k) {
            idx=i; varNames=vn; initExprs=ie; stepExprs=se; hasStep=hs; doEnv=de; testClause=tc; form=f; outerEnv=oe; this.k=k;
        }
    }

    // Do: test evaluated
    static final class DoTestK extends Kont {
        final String[] varNames; final Object[] stepExprs; final boolean[] hasStep;
        final List<?> testClause; final List<?> form;
        final Evaluator.Env doEnv; final Kont k;
        DoTestK(String[] vn, Object[] se, boolean[] hs, List<?> tc, List<?> f, Evaluator.Env de, Kont k) {
            varNames=vn; stepExprs=se; hasStep=hs; testClause=tc; form=f; doEnv=de; this.k=k;
        }
    }

    // Do: after commands, start step evaluation
    static final class DoStepStartK extends Kont {
        final String[] varNames; final Object[] stepExprs; final boolean[] hasStep;
        final List<?> testClause; final List<?> form;
        final Evaluator.Env doEnv; final Kont k;
        DoStepStartK(String[] vn, Object[] se, boolean[] hs, List<?> tc, List<?> f, Evaluator.Env de, Kont k) {
            varNames=vn; stepExprs=se; hasStep=hs; testClause=tc; form=f; doEnv=de; this.k=k;
        }
    }

    // Do: evaluating step expressions
    static final class DoStepK extends Kont {
        final int idx; final Object[] newVals;
        final String[] varNames; final Object[] stepExprs; final boolean[] hasStep;
        final List<?> testClause; final List<?> form;
        final Evaluator.Env doEnv; final Kont k;
        DoStepK(int i, Object[] nv, String[] vn, Object[] se, boolean[] hs, List<?> tc, List<?> f, Evaluator.Env de, Kont k) {
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

    // Wind entry for dynamic-wind
    record WindEntry(Object inThunk, Object outThunk) {}

    // First-class continuation value
    static final class SchemeContinuation {
        final Kont k;
        final List<WindEntry> windStack;
        SchemeContinuation(Kont k, List<WindEntry> windStack) { this.k = k; this.windStack = windStack; }
    }

    // dynamic-wind: after in-thunk, evaluate body
    static final class DynWindBodyK extends Kont {
        final Object bodyThunk; final Object outThunk; final WindEntry entry;
        final Pos pos; final Kont k;
        DynWindBodyK(Object bt, Object ot, WindEntry e, Pos p, Kont k) {
            bodyThunk=bt; outThunk=ot; entry=e; pos=p; this.k=k;
        }
    }

    // dynamic-wind: after body, run out-thunk
    static final class DynWindOutK extends Kont {
        final Object outThunk; final WindEntry entry;
        final Object bodyVal; final Pos pos; final Kont k;
        DynWindOutK(Object ot, WindEntry e, Object bv, Pos p, Kont k) {
            outThunk=ot; entry=e; bodyVal=bv; pos=p; this.k=k;
        }
    }

    // dynamic-wind: after out-thunk, return body value
    static final class DynWindFinishK extends Kont {
        final Object bodyVal; final WindEntry entry; final Kont k;
        DynWindFinishK(Object bv, WindEntry e, Kont k) { bodyVal=bv; entry=e; this.k=k; }
    }

    // Wind transfer: run a sequence of thunks then continue
    static final class WindTransferK extends Kont {
        final List<Object> thunks; final int idx;
        final List<WindEntry> targetStack;
        final Object val; final Kont targetK;
        WindTransferK(List<Object> t, int i, List<WindEntry> ts, Object v, Kont tk) {
            thunks=t; idx=i; targetStack=ts; val=v; targetK=tk;
        }
    }

    // Multiple return values wrapper
    record SchemeValues(List<Object> values) {}

    // call-with-values: producer returned, now call consumer
    static final class CallWithValuesK extends Kont {
        final Object consumer; final Pos pos; final Kont k;
        CallWithValuesK(Object c, Pos p, Kont k) { consumer=c; pos=p; this.k=k; }
    }

    // Exception handler stack entry
    static class ExHandlerFrame {
        final Object handler;      // for with-exception-handler (null for guard)
        final boolean isGuard;
        final String guardVar;
        final List<?> clauseSpec;  // (var clause1 clause2 ...)
        final Evaluator.Env guardEnv;
        final Kont guardK;
        final List<WindEntry> guardWindStack;

        ExHandlerFrame(Object handler) {
            this.handler = handler; this.isGuard = false;
            this.guardVar = null; this.clauseSpec = null;
            this.guardEnv = null; this.guardK = null; this.guardWindStack = null;
        }

        ExHandlerFrame(String var, List<?> clauseSpec, Evaluator.Env env, Kont k, List<WindEntry> windStack) {
            this.handler = null; this.isGuard = true;
            this.guardVar = var; this.clauseSpec = clauseSpec;
            this.guardEnv = env; this.guardK = k; this.guardWindStack = new ArrayList<>(windStack);
        }
    }

    // guard body completed normally — pop handler
    static final class GuardBodyK extends Kont {
        final Kont k;
        GuardBodyK(Kont k) { this.k = k; }
    }

    // with-exception-handler thunk completed normally — pop handler
    static final class WithExHandlerK extends Kont {
        final Kont k;
        WithExHandlerK(Kont k) { this.k = k; }
    }

    // After wind-transfer for guard, evaluate cond clauses
    static final class GuardEvalK extends Kont {
        final String var;
        final Object exnVal;
        final List<?> clauseSpec;
        final Evaluator.Env guardEnv;
        final Kont k;
        GuardEvalK(String v, Object ev, List<?> cs, Evaluator.Env ge, Kont k) {
            var=v; exnVal=ev; clauseSpec=cs; guardEnv=ge; this.k=k;
        }
    }
}
