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

// Exception handler entries
sealed private[ming] trait ExnHandlerEntry:
  def windStack: List[WindEntry]

private[ming] class SimpleExnHandler(val handler: Expr, val windStack: List[WindEntry]) extends ExnHandlerEntry

private[ming] class GuardExnHandler(
  val varName: String,
  val clauses: List[Expr],
  val env: Env,
  val exitK: Kont,
  val windStack: List[WindEntry]
) extends ExnHandlerEntry

// Exception-related continuations
private[ming] case class PopExnHandlerK(k: Kont)                                                  extends Kont
private[ming] case object RaiseReturnK                                                            extends Kont
private[ming] case class CallExnHandlerK(handler: Expr, exnValue: Expr, afterK: Kont)             extends Kont
private[ming] case class GuardStartK(varName: String, clauses: List[Expr], env: Env, exitK: Kont) extends Kont

private[ming] case class GuardCondK(varName: String, body: List[Expr], remaining: List[Expr], env: Env, exitK: Kont)
    extends Kont

// call-with-values support
private[ming] case class CallWithValuesConsumerK(consumer: Expr, k: Kont) extends Kont

// syntax-case support
private[ming] case class SyntaxCaseMatchK(literals: List[String], clauses: List[Expr], env: Env, k: Kont) extends Kont
private[ming] case class SyntaxCaseCleanupK(k: Kont)                                                      extends Kont
private[ming] case class TransformerMacroReturnK(useEnv: Env, k: Kont)                                    extends Kont

private[ming] class CekState:
  var expr: Expr                         = null
  var env: Env                           = null
  var k: Kont                            = null
  var value: Expr                        = null
  var evaluating: Boolean                = true
  var appPosExpr: Expr                   = null
  var windStack: List[WindEntry]         = Nil
  var exnHandlers: List[ExnHandlerEntry] = Nil
