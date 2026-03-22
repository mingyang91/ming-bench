package ming

import SchemeValue.*
import Evaluator.{EvalS, RaiseS, ReturnS, Step}

/** Exception handling: raise, guard, with-exception-handler. */
private[ming] object ExceptionHandling:

  def evalGuard(
    args: List[SchemeValue],
    env: Env,
    k: Kont,
    out: String
  ): Step = args match
    case SchemeList(SchemeSymbol(variable) :: clauses) :: body if body.nonEmpty =>
      val guardK = Kont.GuardK(variable, clauses, env, k)
      SpecialForms.startSequence(body, env, guardK, out)
    case _ => throw new EvalError("guard: bad syntax")

  def handleRaise(
    value: SchemeValue,
    k: Kont,
    out: String
  ): Step =
    findHandler(k) match
      case Some(HandlerFound.ExHandler(handler, restK)) =>
        ProcApply.applyProc(handler, List(value), restK, out)
      case Some(HandlerFound.Guard(variable, clauses, env, guardOuterK)) =>
        val exnEnv = env.extend(variable, value)
        startGuardClauses(value, clauses, variable, exnEnv, k, guardOuterK, out)
      case None =>
        throw new EvalError(s"unhandled exception: ${value.display}")

  def applyGuardTest(
    testResult: SchemeValue,
    exnValue: SchemeValue,
    body: List[SchemeValue],
    remaining: List[SchemeValue],
    variable: String,
    env: Env,
    raiseK: Kont,
    guardK: Kont,
    out: String
  ): Step =
    if !Evaluator.isFalsy(testResult) then
      if body.isEmpty then ProcApply.windTransition(raiseK, guardK, testResult, out)
      else
        val targetK = Kont.GuardClauseK(body, env, guardK)
        ProcApply.windTransition(raiseK, targetK, SchemeVoid, out)
    else startGuardClauses(exnValue, remaining, variable, env, raiseK, guardK, out)

  private def startGuardClauses(
    exnValue: SchemeValue,
    clauses: List[SchemeValue],
    variable: String,
    env: Env,
    raiseK: Kont,
    guardK: Kont,
    out: String
  ): Step = clauses match
    case Nil =>
      reRaise(exnValue, raiseK, out)
    case SchemeList(SchemeSymbol("else") :: body) :: _ =>
      val targetK = Kont.GuardClauseK(body, env, guardK)
      ProcApply.windTransition(raiseK, targetK, SchemeVoid, out)
    case SchemeList(test :: body) :: rest =>
      EvalS(
        test,
        env,
        Kont.GuardTestK(exnValue, body, rest, variable, env, raiseK, guardK),
        out
      )
    case other :: _ =>
      throw new EvalError(s"guard: bad clause: ${other.display}")

  private def reRaise(
    value: SchemeValue,
    raiseK: Kont,
    out: String
  ): Step =
    findHandler(raiseK) match
      case Some(HandlerFound.ExHandler(handler, restK)) =>
        ProcApply.applyProc(handler, List(value), restK, out)
      case Some(HandlerFound.Guard(variable, clauses, env, guardOuterK)) =>
        val exnEnv = env.extend(variable, value)
        startGuardClauses(value, clauses, variable, exnEnv, raiseK, guardOuterK, out)
      case None =>
        throw new EvalError(s"unhandled exception: ${value.display}")

  sealed private trait HandlerFound

  private object HandlerFound:
    case class ExHandler(handler: SchemeValue, restK: Kont) extends HandlerFound

    case class Guard(
      variable: String,
      clauses: List[SchemeValue],
      env: Env,
      guardOuterK: Kont
    ) extends HandlerFound

  @scala.annotation.tailrec
  private def findHandler(k: Kont): Option[HandlerFound] = k match
    case Kont.Halt => None
    case Kont.ExceptionHandlerK(handler, restK) =>
      Some(HandlerFound.ExHandler(handler, restK))
    case Kont.GuardK(variable, clauses, env, outerK) =>
      Some(HandlerFound.Guard(variable, clauses, env, outerK))
    case other => findHandler(parentOf(other))

  private def parentOf(k: Kont): Kont = k match
    case Kont.Halt                                  => Kont.Halt
    case Kont.Seq(_, _, next)                       => next
    case Kont.EvalOp(_, _, next, _)                 => next
    case Kont.EvalArg(_, _, _, _, next, _)          => next
    case Kont.IfK(_, _, _, next)                    => next
    case Kont.SetK(_, _, next)                      => next
    case Kont.DefineK(_, _, next)                   => next
    case Kont.AndK(_, _, next)                      => next
    case Kont.OrK(_, _, next)                       => next
    case Kont.CallCCK(next)                         => next
    case Kont.CondK(_, _, _, next)                  => next
    case Kont.LetInitK(_, _, _, _, _, next)         => next
    case Kont.NamedLetInitK(_, _, _, _, _, _, next) => next
    case Kont.MapK(_, _, _, next)                   => next
    case Kont.LetrecInitK(_, _, _, _, _, next)      => next
    case Kont.LetStarInitK(_, _, _, _, next)        => next
    case Kont.CaseK(_, _, next)                     => next
    case Kont.ForEachK(_, _, next)                  => next
    case Kont.DynWindMark(_, next)                  => next
    case Kont.DynWindInK(_, _, next)                => next
    case Kont.DynWindRetK(_, next)                  => next
    case Kont.DynUnwindK(_, _, _, next)             => next
    case Kont.DynRewindK(_, _, next)                => next
    case Kont.ExceptionHandlerK(_, next)            => next
    case Kont.GuardK(_, _, _, next)                 => next
    case Kont.GuardTestK(_, _, _, _, _, _, next)    => next
    case Kont.GuardClauseK(_, _, next)              => next
    case Kont.CallWithValuesK(_, next)              => next
