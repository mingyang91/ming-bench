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

  def handleMapK(
    args: List[SchemeValue],
    env: Env,
    out: Array[String],
    k: Cont
  ): Bounce =
    args match
      case func :: lists if lists.nonEmpty =>
        val listVals = lists.map(InterpreterUtils.schemeListToList)
        mapLoop(func, listVals, env, out, Nil, k)
      case _ => throw new EvalError("map: need procedure and at least one list")

  private def mapLoop(
    func: SchemeValue,
    lists: List[List[SchemeValue]],
    env: Env,
    out: Array[String],
    acc: List[SchemeValue],
    k: Cont
  ): Bounce =
    if lists.exists(_.isEmpty) then More(() => k(Builtins.listToPairs(acc.reverse), env))
    else
      val heads = lists.map(_.head)
      val tails = lists.map(_.tail)
      CpsEval.applyK(
        func,
        heads,
        env,
        out,
        (result, _) => mapLoop(func, tails, env, out, result :: acc, k),
        None
      )

  def handleForEachK(
    args: List[SchemeValue],
    env: Env,
    out: Array[String],
    k: Cont
  ): Bounce =
    args match
      case func :: lists if lists.nonEmpty =>
        val listVals = lists.map(InterpreterUtils.schemeListToList)
        forEachLoop(func, listVals, env, out, k)
      case _ => throw new EvalError("for-each: need procedure and at least one list")

  private def forEachLoop(
    func: SchemeValue,
    lists: List[List[SchemeValue]],
    env: Env,
    out: Array[String],
    k: Cont
  ): Bounce =
    if lists.exists(_.isEmpty) then More(() => k(SchemeValue.Void, env))
    else
      val heads = lists.map(_.head)
      val tails = lists.map(_.tail)
      CpsEval.applyK(
        func,
        heads,
        env,
        out,
        (_, _) => forEachLoop(func, tails, env, out, k),
        None
      )

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
