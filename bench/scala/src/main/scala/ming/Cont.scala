package ming

/** Entry in the dynamic-wind stack. Identity-compared for common-tail detection. */
class WindEntry(val inThunk: SchemeVal, val outThunk: SchemeVal)

/** Exception handler stack entries. */
sealed trait ExceptionHandler

object ExceptionHandler:
  case class Proc(handler: SchemeVal, winds: List[WindEntry]) extends ExceptionHandler

  case class Guard(exnVar: String, clauses: List[SchemeVal], env: Env, guardK: Cont, winds: List[WindEntry])
      extends ExceptionHandler

/** Continuation frames for the CEK machine. */
sealed trait Cont

object Cont:
  case object Halt                                                                                  extends Cont
  case class IfK(thenE: SchemeVal, elseE: Option[SchemeVal], env: Env, k: Cont)                     extends Cont
  case class SeqK(rest: List[SchemeVal], env: Env, k: Cont)                                         extends Cont
  case class DefValK(name: String, env: Env, k: Cont)                                               extends Cont
  case class SetValK(name: String, env: Env, k: Cont)                                               extends Cont
  case class EvOpK(argExprs: List[SchemeVal], env: Env, k: Cont)                                    extends Cont
  case class EvArgK(op: SchemeVal, done: List[SchemeVal], rest: List[SchemeVal], env: Env, k: Cont) extends Cont
  case class AndK(rest: List[SchemeVal], env: Env, k: Cont)                                         extends Cont
  case class OrK(rest: List[SchemeVal], env: Env, k: Cont)                                          extends Cont
  case class CondK(body: List[SchemeVal], remaining: List[SchemeVal], env: Env, k: Cont)            extends Cont
  case class CondArrowK(testValue: SchemeVal, env: Env, k: Cont)                                   extends Cont
  // dynamic-wind normal flow
  case class DynWindAfterInK(bodyThunk: SchemeVal, entry: WindEntry, k: Cont) extends Cont
  case class DynWindAfterBodyK(entry: WindEntry, k: Cont)                     extends Cont
  case class DynWindAfterOutK(bodyValue: SchemeVal, k: Cont)                  extends Cont

  // continuation invocation wind/unwind steps
  // actions: (isUnwind, entry) — unwind = run out-thunk, rewind = run in-thunk
  case class WindContinueK(
    remaining: List[(Boolean, WindEntry)],
    finalValue: SchemeVal,
    targetK: Cont,
    targetWinds: List[WindEntry]
  ) extends Cont

  case class WindPushK(
    entry: WindEntry,
    remaining: List[(Boolean, WindEntry)],
    finalValue: SchemeVal,
    targetK: Cont,
    targetWinds: List[WindEntry]
  ) extends Cont

  // exception handling (L20)
  case class WithHandlerK(k: Cont)                                                                  extends Cont
  case object RaiseReturnErrorK                                                                     extends Cont
  case class GuardAfterWindK(clauses: List[SchemeVal], exnValue: SchemeVal, env: Env, guardK: Cont) extends Cont

  case class GuardTestK(body: List[SchemeVal], remaining: List[SchemeVal], exnValue: SchemeVal, env: Env, guardK: Cont)
      extends Cont

  // L21 — call-with-values
  case class CallWithValuesK(consumer: SchemeVal, k: Cont) extends Cont

  // CPS let binding evaluation — ensures call/cc inside let bindings captures full continuation
  case class LetEvalK(
    currentName: String,                  // name for the value being evaluated
    bound: List[(String, SchemeVal)],     // already evaluated (name, value) pairs (reversed)
    remaining: List[(String, SchemeVal)], // remaining (name, initExpr) pairs
    body: List[SchemeVal],
    outerEnv: Env,
    k: Cont
  ) extends Cont

  // CPS named let binding evaluation
  case class NamedLetEvalK(
    loopName: String,
    paramNames: List[String],
    currentName: String,
    bound: List[(String, SchemeVal)],
    remaining: List[(String, SchemeVal)],
    body: List[SchemeVal],
    outerEnv: Env,
    k: Cont
  ) extends Cont
