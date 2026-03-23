package ming

/** Trampoline type for CPS evaluator. Prevents stack overflow on tail calls. */
sealed trait Bounce

object Bounce:
  case class Done(value: Value)                                        extends Bounce
  case class More(thunk: () => Bounce)                                 extends Bounce
  case class TailEval(expr: Expr, env: Env, k: Value => Bounce)        extends Bounce
  case class TailBody(exprs: List[Expr], env: Env, k: Value => Bounce) extends Bounce
