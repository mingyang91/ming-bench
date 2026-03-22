package ming

import SchemeValue.*
import InterpreterUtils.*
import Bounce.*

object CpsApply:
  type Env  = Map[String, SchemeValue]
  type Cont = (SchemeValue, Env) => Bounce

  def handleCallCCInline(
    args: List[SchemeValue],
    env: Env,
    out: Array[String],
    k: Cont,
    pos: Option[(Int, Int)]
  ): Bounce =
    args match
      case fExpr :: Nil =>
        CpsEval.evalK(
          fExpr,
          env,
          out,
          (f, _) =>
            val contVal = ContinuationVal(k)
            CpsEval.applyK(f, List(contVal), env, out, k, pos)
        )
      case _ =>
        throw new EvalError(
          s"call/cc: expected 1 argument, got ${args.length}${fmtPos(pos)}"
        )

  def handleCallCCAsValue(
    args: List[SchemeValue],
    env: Env,
    out: Array[String],
    k: Cont,
    pos: Option[(Int, Int)]
  ): Bounce =
    args match
      case f :: Nil =>
        val contVal = ContinuationVal(k)
        CpsEval.applyK(f, List(contVal), env, out, k, pos)
      case _ =>
        throw new EvalError(
          s"call/cc: expected 1 argument, got ${args.length}"
        )

  def handleApplyK(
    args: List[SchemeValue],
    callingEnv: Env,
    out: Array[String],
    k: Cont
  ): Bounce =
    args match
      case func :: rest if rest.nonEmpty =>
        val prefixArgs = rest.init
        val lastArg    = rest.last
        val listArgs   = schemeListToList(lastArg)
        CpsEval.applyK(func, prefixArgs ++ listArgs, callingEnv, out, k, None)
      case _ => throw new EvalError("apply: need at least 2 arguments")

  def checkArity(
    expected: Int,
    got: Int,
    restParam: Option[String],
    pos: Option[(Int, Int)]
  ): Unit =
    restParam match
      case None =>
        if got != expected then
          throw new EvalError(
            s"wrong number of arguments: expected $expected, got $got"
          )
      case Some(_) =>
        if got < expected then
          throw new EvalError(
            s"wrong number of arguments: expected at least $expected, got $got"
          )
