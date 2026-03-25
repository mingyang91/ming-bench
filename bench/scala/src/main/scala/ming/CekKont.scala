package ming

sealed trait Kont
case object HaltK                                                                 extends Kont
private[ming] case class IfK(thenE: Expr, elseE: Option[Expr], env: Env, k: Kont) extends Kont
private[ming] case class SeqK(remaining: List[Expr], env: Env, k: Kont)           extends Kont
private[ming] case class DefineK(name: String, env: Env, k: Kont)                 extends Kont
private[ming] case class SetK(name: String, env: Env, k: Kont)                    extends Kont
private[ming] case class EvFunK(argExprs: List[Expr], env: Env, k: Kont)          extends Kont

private[ming] case class EvArgsK(
  func: Expr,
  evaledInOrder: List[Expr],
  remaining: List[Expr],
  env: Env,
  k: Kont
) extends Kont

private[ming] case class BindK(
  name: String,
  remaining: List[(String, Expr)],
  body: List[Expr],
  bindEnv: Env,
  evalEnv: Env,
  k: Kont
) extends Kont

private[ming] case class NamedLetBindK(
  name: String,
  paramNames: List[String],
  evaledRev: List[Expr],
  remaining: List[Expr],
  body: List[Expr],
  evalEnv: Env,
  k: Kont
) extends Kont

private[ming] case class AndK(remaining: List[Expr], env: Env, k: Kont)                    extends Kont
private[ming] case class OrK(remaining: List[Expr], env: Env, k: Kont)                     extends Kont
private[ming] case class CondK(body: List[Expr], remaining: List[Expr], env: Env, k: Kont) extends Kont
private[ming] case class CaseKeyK(clauses: List[Expr], env: Env, k: Kont)                  extends Kont

// dynamic-wind support
private[ming] class WindEntry(val inThunk: Expr, val outThunk: Expr)

private[ming] case class DynWindAfterInK(entry: WindEntry, bodyThunk: Expr, outThunk: Expr, k: Kont) extends Kont
private[ming] case class DynWindAfterBodyK(entry: WindEntry, outThunk: Expr, k: Kont)                extends Kont
private[ming] case class DynWindAfterOutK(bodyValue: Expr, k: Kont)                                  extends Kont

private[ming] case class DynWindTransferK(
  unwindOuts: List[Expr],
  rewindEntries: List[WindEntry],
  targetWinds: List[WindEntry],
  value: Expr,
  savedK: Kont
) extends Kont

private[ming] class CekState:
  var expr: Expr                 = null
  var env: Env                   = null
  var k: Kont                    = null
  var value: Expr                = null
  var evaluating: Boolean        = true
  var appPosExpr: Expr           = null
  var windStack: List[WindEntry] = Nil
