package ming

/** Procedure application helpers — call environment setup, clause matching, non-CPS apply. */
object Apply:

  def setupCallEnv(
    params: List[String],
    restParam: Option[String],
    args: List[SchemeVal],
    closure: Env
  ): Env =
    restParam match
      case None =>
        if params.length != args.length then
          throw new EvalError(s"expected ${params.length} arguments, got ${args.length}")
        val callEnv = Env(Some(closure))
        params.zip(args).foreach((p, a) => callEnv.define(p, a))
        callEnv
      case Some(rest) =>
        if args.length < params.length then
          throw new EvalError(s"expected at least ${params.length} arguments, got ${args.length}")
        val callEnv = Env(Some(closure))
        params.zip(args).foreach((p, a) => callEnv.define(p, a))
        callEnv.define(rest, SchemeVal.SList(args.drop(params.length)))
        callEnv

  def findClause(
    clauses: List[(List[String], Option[String], List[SchemeVal])],
    args: List[SchemeVal]
  ): (List[String], Option[String], List[SchemeVal]) =
    clauses
      .find { case (params, restParam, _) =>
        restParam match
          case None    => args.length == params.length
          case Some(_) => args.length >= params.length
      }
      .getOrElse(throw new EvalError(s"no matching clause for ${args.length} arguments"))

  /** Non-CPS apply — used by HigherOrder (map, for-each, apply). */
  private[ming] def applyProc(op: SchemeVal, args: List[SchemeVal]): SchemeVal =
    op match
      case SchemeVal.SLambda(params, restParam, body, closure) =>
        val callEnv = setupCallEnv(params, restParam, args, closure)
        Evaluator.evalBody(body, callEnv)
      case SchemeVal.SCaseLambda(clauses, closure) =>
        val (cparams, crest, cbody) = findClause(clauses, args)
        val callEnv                 = setupCallEnv(cparams, crest, args, closure)
        Evaluator.evalBody(cbody, callEnv)
      case SchemeVal.SSymbol(name)
          if name.startsWith("__record-ctor__:") ||
            name.startsWith("__record-pred__:") ||
            name.startsWith("__record-acc__:") =>
        RecordOps.applyRecordOp(name, args)
      case SchemeVal.SSymbol(name) =>
        applyBuiltinOrHOF(name, args)
      case _ => throw new EvalError(s"not a procedure: ${op.display}")

  private[ming] def applyBuiltinOrHOF(
    name: String,
    args: List[SchemeVal]
  ): SchemeVal =
    name match
      case "display" | "write" | "newline" | "apply" | "map" | "for-each" =>
        HigherOrder(name, args, applyProc)
      case _ => Builtins.applyBuiltin(name, args)
