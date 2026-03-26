package ming

// ── CEK Machine Continuation Frames ─────────────────────────────

sealed private[ming] trait Kont
private[ming] case object HaltK                                                   extends Kont
private[ming] case class IfK(thenE: Expr, elseE: Option[Expr], env: Env, k: Kont) extends Kont
private[ming] case class SeqK(remaining: List[Expr], env: Env, k: Kont)           extends Kont
private[ming] case class DefineK(name: String, env: Env, k: Kont)                 extends Kont
private[ming] case class SetBangK(name: String, env: Env, k: Kont)                extends Kont
private[ming] case class EvFunK(argExprs: List[Expr], env: Env, k: Kont)          extends Kont

private[ming] case class EvArgsK(
  op: SchemeVal,
  done: List[SchemeVal],
  remaining: List[Expr],
  env: Env,
  k: Kont
) extends Kont
private[ming] case class AndK(remaining: List[Expr], env: Env, k: Kont)                        extends Kont
private[ming] case class OrK(remaining: List[Expr], env: Env, k: Kont)                         extends Kont
private[ming] case class CallCCK(k: Kont)                                                      extends Kont
private[ming] case class CondTestK(body: List[Expr], remaining: List[Expr], env: Env, k: Kont) extends Kont
private[ming] case class CaseK(clauses: List[Expr], env: Env, k: Kont)                         extends Kont

private[ming] case class LetrecBindK(
  name: String,
  remaining: List[(String, Expr)],
  localEnv: Env,
  body: List[Expr],
  k: Kont
) extends Kont
private[ming] case class TestBodyK(body: List[Expr], invert: Boolean, env: Env, k: Kont) extends Kont

// dynamic-wind frames
private[ming] case class DynWindAfterInK(inThunk: SchemeVal, bodyThunk: SchemeVal, outThunk: SchemeVal, k: Kont)
    extends Kont
private[ming] case class DynWindAfterBodyK(outThunk: SchemeVal, k: Kont) extends Kont
private[ming] case class DynWindAfterOutK(bodyVal: SchemeVal, k: Kont)   extends Kont

// wind transition frames (for continuation invocation across dynamic-wind boundaries)
sealed private[ming] trait WindAction
private[ming] case class DoUnwind(outThunk: SchemeVal)                               extends WindAction
private[ming] case class DoRewind(inThunk: SchemeVal, entry: (SchemeVal, SchemeVal)) extends WindAction

private[ming] case class WindContK(
  actions: List[WindAction],
  savedVal: SchemeVal,
  targetK: Kont
) extends Kont

private[ming] case class WindPushK(
  entry: (SchemeVal, SchemeVal),
  actions: List[WindAction],
  savedVal: SchemeVal,
  targetK: Kont
) extends Kont

// ── CEK Machine State ──────────────────────────────────────────

sealed private[ming] trait MState
private[ming] case class SEval(expr: Expr, env: Env, k: Kont) extends MState
private[ming] case class SApply(value: SchemeVal, k: Kont)    extends MState
