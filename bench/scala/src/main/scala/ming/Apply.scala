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

  /** CPS-mode apply — used by the CEK machine (Evaluator.kontStep). */
  private[ming] def performApply(op: SchemeVal, args: List[SchemeVal], k: Cont): Evaluator.State =
    op match
      case SchemeVal.SLambda(params, restParam, body, closure) =>
        val callEnv = setupCallEnv(params, restParam, args, closure)
        Evaluator.evalBodyCek(body, callEnv, k)
      case SchemeVal.SCaseLambda(clauses, closure) =>
        val (cparams, crest, cbody) = findClause(clauses, args)
        val callEnv                 = setupCallEnv(cparams, crest, args, closure)
        Evaluator.evalBodyCek(cbody, callEnv, k)
      case SchemeVal.SContinuation(savedK, savedWinds) =>
        if args.isEmpty then throw new EvalError("continuation: expected at least 1 argument")
        val value        = if args.length == 1 then args.head else SchemeVal.SValues(args)
        val currentWinds = Evaluator.windStack.get()
        val actions      = DynWind.computeWindActions(currentWinds, savedWinds)
        if actions.isEmpty then Evaluator.State.Ko(value, savedK)
        else DynWind.startWindActions(actions, value, savedK, savedWinds)(performApply)
      case SchemeVal.SSymbol(name) if name == "call/cc" || name == "call-with-current-continuation" =>
        if args.length != 1 then throw new EvalError("call/cc: expected 1 argument")
        val contVal = SchemeVal.SContinuation(k, Evaluator.windStack.get())
        performApply(args.head, List(contVal), k)
      case SchemeVal.SSymbol("dynamic-wind") =>
        if args.length != 3 then throw new EvalError("dynamic-wind: expected 3 arguments")
        val (inThunk, bodyThunk, outThunk) = (args(0), args(1), args(2))
        val entry                          = new WindEntry(inThunk, outThunk)
        performApply(inThunk, Nil, Cont.DynWindAfterInK(bodyThunk, entry, k))
      case SchemeVal.SSymbol("values") =>
        if args.length == 1 then Evaluator.State.Ko(args.head, k)
        else Evaluator.State.Ko(SchemeVal.SValues(args), k)
      case SchemeVal.SSymbol("call-with-values") =>
        if args.length != 2 then throw new EvalError("call-with-values: expected 2 arguments")
        val (producer, consumer) = (args(0), args(1))
        performApply(producer, Nil, Cont.CallWithValuesK(consumer, k))
      case SchemeVal.SSymbol("raise") =>
        if args.length != 1 then throw new EvalError("raise: expected 1 argument")
        ExceptionOps.handleRaise(args.head, k, performApply)
      case SchemeVal.SSymbol("with-exception-handler") =>
        if args.length != 2 then throw new EvalError("with-exception-handler: expected 2 arguments")
        val (handler, thunk) = (args(0), args(1))
        Evaluator.handlerStack.set(
          ExceptionHandler.Proc(handler, Evaluator.windStack.get()) :: Evaluator.handlerStack.get()
        )
        performApply(thunk, Nil, Cont.WithHandlerK(k))
      case SchemeVal.SSymbol(name)
          if name.startsWith("__record-ctor__:") ||
            name.startsWith("__record-pred__:") ||
            name.startsWith("__record-acc__:") =>
        Evaluator.State.Ko(RecordOps.applyRecordOp(name, args), k)
      case SchemeVal.SSymbol(name) =>
        Evaluator.State.Ko(applyBuiltinOrHOF(name, args), k)
      case _ => throw new EvalError(s"not a procedure: ${op.display}")

  private[ming] def applyBuiltinOrHOF(
    name: String,
    args: List[SchemeVal]
  ): SchemeVal =
    name match
      case "display" | "write" | "newline" | "apply" | "map" | "for-each" =>
        HigherOrder(name, args, applyProc)
      case _ => Builtins.applyBuiltin(name, args)
