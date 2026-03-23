package ming

import SchemeValue.*
import Interpreter.EvalResult
import Interpreter.EvalResult.*

/** Procedure application helpers extracted from Interpreter. */
object ProcApply:

  /** Format an error message with optional position info. */
  private def posMsg(msg: String, pos: Option[SourcePos]): String =
    pos match
      case Some(p) => s"$msg [$p]"
      case None    => msg

  def applyProc(
    proc: SchemeValue,
    args: List[SchemeValue],
    callPos: Option[SourcePos] = None
  ): SchemeValue =
    proc match
      case LambdaVal(params, restParam, body, closure) =>
        bindAndEvalBody(params, restParam, args, body, closure, callPos)
      case BuiltinVal(_, func) =>
        try func(args)
        catch
          case e: EvalError =>
            if callPos.isDefined && !e.getMessage.matches(".*\\d+:\\d+.*") then
              throw new EvalError(posMsg(e.getMessage, callPos))
            else throw e
      case CaseLambdaVal(clauses, closure) =>
        val (params, restParam, body) = matchCaseLambdaClause(clauses, args.length, callPos)
        bindAndEvalBody(params, restParam, args, body, closure, callPos)
      case ContinuationVal(contId, bodyExprs, bodyEnv, ctxStack, seqRem, seqE, hasSame) =>
        val jumpValue = args match
          case Nil           => Void
          case single :: Nil => single
          case multiple      => ValuesVal(multiple)
        throw new ContinuationJump(contId, jumpValue, bodyExprs, bodyEnv, ctxStack, seqRem, seqE, hasSame)
      case _ =>
        throw new EvalError(posMsg("not a procedure", callPos))

  /** Bind parameters (including rest param) and evaluate body with TCO on last expr. */
  private[ming] def bindParamsAndTailCall(
    params: List[String],
    restParam: Option[String],
    args: List[SchemeValue],
    body: List[SchemeValue],
    closure: Environment,
    callPos: Option[SourcePos]
  ): EvalResult =
    val childEnv = bindParams(params, restParam, args, closure, callPos)
    Interpreter.evalBodyInit(body, childEnv)
    TailCall(body.last, childEnv)

  private def bindAndEvalBody(
    params: List[String],
    restParam: Option[String],
    args: List[SchemeValue],
    body: List[SchemeValue],
    closure: Environment,
    callPos: Option[SourcePos]
  ): SchemeValue =
    val savedCtx = ContinuationManager.bodyContext
    val childEnv = bindParams(params, restParam, args, closure, callPos)
    try
      Interpreter.evalBodyInit(body, childEnv)
      Interpreter.eval(body.last, childEnv)
    finally ContinuationManager.bodyContext = savedCtx

  private def bindParams(
    params: List[String],
    restParam: Option[String],
    args: List[SchemeValue],
    closure: Environment,
    callPos: Option[SourcePos]
  ): Environment =
    restParam match
      case Some(rest) =>
        if args.length < params.length then
          throw new EvalError(
            posMsg(s"wrong number of arguments: expected at least ${params.length}, got ${args.length}", callPos)
          )
        val childEnv = closure.child()
        params.zip(args).foreach((p, v) => childEnv.define(p, v))
        childEnv.define(rest, ListVal(args.drop(params.length)))
        childEnv
      case None =>
        if args.length != params.length then
          throw new EvalError(
            posMsg(s"wrong number of arguments: expected ${params.length}, got ${args.length}", callPos)
          )
        val childEnv = closure.child()
        params.zip(args).foreach((p, v) => childEnv.define(p, v))
        childEnv

  /** Find the matching clause in a case-lambda by argument count. */
  private[ming] def matchCaseLambdaClause(
    clauses: List[(List[String], Option[String], List[SchemeValue])],
    argCount: Int,
    callPos: Option[SourcePos]
  ): (List[String], Option[String], List[SchemeValue]) =
    clauses
      .find { (params, restParam, _) =>
        restParam match
          case Some(_) => argCount >= params.length
          case None    => argCount == params.length
      }
      .getOrElse(
        throw new EvalError(posMsg(s"case-lambda: no matching clause for $argCount arguments", callPos))
      )
