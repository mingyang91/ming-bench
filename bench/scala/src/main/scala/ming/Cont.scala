package ming

/** Entry in the dynamic-wind stack. Identity-compared for common-tail detection. */
class WindEntry(val inThunk: SchemeVal, val outThunk: SchemeVal)

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
