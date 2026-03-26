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

// ── CEK Machine State ──────────────────────────────────────────

sealed private[ming] trait MState
private[ming] case class SEval(expr: Expr, env: Env, k: Kont) extends MState
private[ming] case class SApply(value: SchemeVal, k: Kont)    extends MState
