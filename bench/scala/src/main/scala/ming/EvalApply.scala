package ming

import Value.*
import EvalHelpers.evalError

/** Procedure application — mixed into Evaluator. */
private[ming] trait EvalApply:
  import Evaluator.{K, WindEntry}

  protected def tailBody(exprs: List[Expr], env: Env, k: K): Bounce
  protected def windStack: List[WindEntry]

  protected def doWindTransition(
    target: List[WindEntry],
    pos: Option[Pos],
    andThen: () => Bounce
  ): Bounce

  protected def applyDynamicWind(
    before: Value,
    during: Value,
    after: Value,
    pos: Option[Pos],
    k: K
  ): Bounce

  protected def applyBuiltinApply(
    args: List[Value],
    pos: Option[Pos],
    k: K
  ): Bounce

  protected def applyBuiltinMap(
    args: List[Value],
    pos: Option[Pos],
    k: K
  ): Bounce

  protected def applyBuiltinForEach(
    args: List[Value],
    pos: Option[Pos],
    k: K
  ): Bounce
  protected def raiseException(value: Value, pos: Option[Pos]): Bounce

  protected def evalWithExceptionHandler(
    handler: Value,
    thunk: Value,
    pos: Option[Pos],
    k: K
  ): Bounce

  protected def applyProc(
    proc: Value,
    values: List[Value],
    pos: Option[Pos],
    k: K
  ): Bounce =
    proc match
      case LambdaVal(params, restParam, body, closure) =>
        tailBody(body, closure.extendWithRest(params, restParam, values), k)
      case CaseLambdaVal(clauses, closure) =>
        applyCaseLambda(clauses, closure, values, pos, k)
      case ContinuationVal(invoke) =>
        values match
          case single :: Nil => invoke(single)
          case _             => invoke(ValuesVal(values))
      case BuiltinVal(name, _) if name == "call/cc" || name == "call-with-current-continuation" =>
        if values.length != 1 then evalError("call/cc: expected 1 argument", pos)
        val capturedWind = windStack
        applyProc(
          values.head,
          List(ContinuationVal(v => doWindTransition(capturedWind, pos, () => k(v)))),
          pos,
          k
        )
      case BuiltinVal("dynamic-wind", _) =>
        if values.length != 3 then evalError("dynamic-wind: expected 3 arguments", pos)
        applyDynamicWind(values(0), values(1), values(2), pos, k)
      case BuiltinVal("apply", _)            => applyBuiltinApply(values, pos, k)
      case BuiltinVal("map", _)              => applyBuiltinMap(values, pos, k)
      case BuiltinVal("for-each", _)         => applyBuiltinForEach(values, pos, k)
      case BuiltinVal("call-with-values", _) => applyCallWithValues(values, pos, k)
      case BuiltinVal("raise", _) =>
        if values.length != 1 then evalError("raise: expected 1 argument", pos)
        raiseException(values.head, pos)
      case BuiltinVal("with-exception-handler", _) =>
        if values.length != 2 then evalError("with-exception-handler: expected 2 arguments", pos)
        evalWithExceptionHandler(values(0), values(1), pos, k)
      case BuiltinVal(name, fn) =>
        try k(fn(values))
        catch
          case e: EvalError =>
            if e.getMessage.matches(".*\\d+:\\d+.*") then throw e
            else evalError(e.getMessage, pos)
          case e: ArithmeticException => evalError(e.getMessage, pos)
      case _ => evalError("not a procedure", pos)

  private def applyCaseLambda(
    clauses: List[(List[String], Option[String], List[Expr])],
    closure: Env,
    values: List[Value],
    pos: Option[Pos],
    k: K
  ): Bounce =
    val matched = clauses.find { case (params, restParam, _) =>
      restParam match
        case Some(_) => values.length >= params.length
        case None    => values.length == params.length
    }
    matched match
      case Some((params, restParam, body)) =>
        tailBody(body, closure.extendWithRest(params, restParam, values), k)
      case None =>
        evalError(
          s"case-lambda: no matching clause for ${values.length} arguments",
          pos
        )

  private def applyCallWithValues(
    values: List[Value],
    pos: Option[Pos],
    k: K
  ): Bounce =
    if values.length != 2 then evalError("call-with-values: expected 2 arguments", pos)
    applyProc(
      values(0),
      Nil,
      pos,
      result =>
        val args = result match
          case ValuesVal(vs) => vs
          case single        => List(single)
        applyProc(values(1), args, pos, k)
    )
