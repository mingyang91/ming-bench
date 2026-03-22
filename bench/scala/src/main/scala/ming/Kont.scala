package ming

/** Continuation frames for CPS-based evaluation with call/cc support. */
sealed trait Kont

object Kont:
  case object Halt                                                extends Kont
  case class Seq(remaining: List[SchemeValue], env: Env, k: Kont) extends Kont

  case class EvalOp(
    argExprs: List[SchemeValue],
    env: Env,
    k: Kont,
    callPos: SourcePos
  ) extends Kont

  case class EvalArg(
    proc: SchemeValue,
    evaled: List[SchemeValue],
    remaining: List[SchemeValue],
    env: Env,
    k: Kont,
    callPos: SourcePos
  ) extends Kont

  case class IfK(
    thenE: SchemeValue,
    elseE: Option[SchemeValue],
    env: Env,
    k: Kont
  ) extends Kont

  case class SetK(name: String, env: Env, k: Kont)                 extends Kont
  case class DefineK(name: String, env: Env, k: Kont)              extends Kont
  case class AndK(remaining: List[SchemeValue], env: Env, k: Kont) extends Kont
  case class OrK(remaining: List[SchemeValue], env: Env, k: Kont)  extends Kont
  case class NotK(k: Kont)                                         extends Kont
  case class CallCCK(k: Kont)                                      extends Kont

  case class CondK(
    body: List[SchemeValue],
    remaining: List[SchemeValue],
    env: Env,
    k: Kont
  ) extends Kont

  case class LetInitK(
    params: List[String],
    evaled: List[SchemeValue],
    remaining: List[SchemeValue],
    body: List[SchemeValue],
    env: Env,
    k: Kont
  ) extends Kont

  case class NamedLetInitK(
    name: String,
    params: List[String],
    evaled: List[SchemeValue],
    remaining: List[SchemeValue],
    body: List[SchemeValue],
    env: Env,
    k: Kont
  ) extends Kont
