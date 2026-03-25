package ming

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
