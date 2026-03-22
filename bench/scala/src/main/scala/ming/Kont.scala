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

  case class MapK(
    proc: SchemeValue,
    remainingGroups: List[List[SchemeValue]],
    accumulated: List[SchemeValue],
    k: Kont
  ) extends Kont

  case class LetrecInitK(
    currentName: String,
    remainingNames: List[String],
    remainingInits: List[SchemeValue],
    body: List[SchemeValue],
    frame: Env,
    k: Kont
  ) extends Kont

  case class LetStarInitK(
    name: String,
    remaining: List[(String, SchemeValue)],
    body: List[SchemeValue],
    env: Env,
    k: Kont
  ) extends Kont

  case class CaseK(
    clauses: List[SchemeValue],
    env: Env,
    k: Kont
  ) extends Kont

  case class ForEachK(
    proc: SchemeValue,
    remainingGroups: List[List[SchemeValue]],
    k: Kont
  ) extends Kont

  case class DynWindInK(
    bodyThunk: SchemeValue,
    entry: WinderEntry,
    k: Kont
  ) extends Kont

  case class DynWindMark(
    entry: WinderEntry,
    k: Kont
  ) extends Kont

  case class DynWindRetK(
    bodyValue: SchemeValue,
    k: Kont
  ) extends Kont

  case class DynUnwindK(
    toUnwind: List[WinderEntry],
    toRewind: List[WinderEntry],
    value: SchemeValue,
    targetK: Kont
  ) extends Kont

  case class DynRewindK(
    toRewind: List[WinderEntry],
    value: SchemeValue,
    targetK: Kont
  ) extends Kont

/** Identity-based entry for dynamic-wind winder tracking. */
class WinderEntry(val inThunk: SchemeValue, val outThunk: SchemeValue)
