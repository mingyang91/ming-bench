package ming

object EvalApply:

  def applyFnBounce(op: Value, args: List[Value]): Eval.Bounce = op match
    case lam @ Value.Lambda(params, body, closure, selfName, restParam) =>
      applyLambda(lam, params, body, closure, selfName, restParam, args)
    case Value.Symbol("dynamic-wind") =>
      evalDynamicWind(args)
    case Value.Symbol("call/cc") | Value.Symbol("call-with-current-continuation") =>
      val (v, o) = evalCallCC(args)
      Eval.Bounce.Done(v, o)
    case Value.Symbol("map") =>
      val (v, o) = applyMap(args)
      Eval.Bounce.Done(v, o)
    case Value.Symbol("apply") =>
      applyApply(args)
    case Value.Symbol("raise") =>
      args match
        case v :: Nil => throw SchemeException(v)
        case _        => throw new EvalError("raise requires 1 argument")
    case Value.Symbol("values") =>
      args match
        case single :: Nil => Eval.Bounce.Done(single, "")
        case _             => Eval.Bounce.Done(Value.Values(args), "")
    case Value.Symbol("call-with-values") =>
      evalCallWithValues(args)
    case Value.Symbol("with-exception-handler") =>
      evalWithExceptionHandler(args)
    case Value.Continuation(id) =>
      args match
        case v :: Nil => throw ContinuationInvoked(id, v)
        case _        => throw ContinuationInvoked(id, Value.Values(args))
    case Value.RecordConstructor(tag, fieldCount) =>
      if args.length != fieldCount then throw new EvalError(s"expected $fieldCount arguments, got ${args.length}")
      Eval.Bounce.Done(Value.Record(tag, args), "")
    case Value.RecordPredicate(tag) =>
      applyRecordPredicate(tag, args)
    case Value.RecordAccessor(tag, idx) =>
      applyRecordAccessor(tag, idx, args)
    case Value.Symbol(name) =>
      val (v, o) = Builtins.call(name, args)
      Eval.Bounce.Done(v, o)
    case _ => throw new EvalError(s"not a procedure: ${op.display}")

  private def applyLambda(
    lam: Value.Lambda,
    params: List[String],
    body: List[Value],
    closure: Env,
    selfName: Option[String],
    restParam: Option[String],
    args: List[Value]
  ): Eval.Bounce =
    val closureWithSelf = selfName match
      case Some(name) => closure.extend(name, lam)
      case None       => closure
    restParam match
      case Some(rest) =>
        if args.length < params.length then
          throw new EvalError(s"expected at least ${params.length} arguments, got ${args.length}")
        val fixed    = args.take(params.length)
        val restArgs = Value.SList(args.drop(params.length))
        Eval.evalBodyBounce(body, closureWithSelf.extendAll(params :+ rest, fixed :+ restArgs), "")
      case None =>
        if params.length != args.length then
          throw new EvalError(s"expected ${params.length} arguments, got ${args.length}")
        Eval.evalBodyBounce(body, closureWithSelf.extendAll(params, args), "")

  private def applyRecordPredicate(tag: String, args: List[Value]): Eval.Bounce =
    args match
      case v :: Nil =>
        val result = v match
          case Value.Record(t, _) => t == tag
          case _                  => false
        Eval.Bounce.Done(Value.Bool(result), "")
      case _ => throw new EvalError("record predicate requires 1 argument")

  private def applyRecordAccessor(tag: String, idx: Int, args: List[Value]): Eval.Bounce =
    args match
      case Value.Record(t, fields) :: Nil =>
        if t != tag then throw new EvalError(s"accessor for $tag called on record of type $t")
        Eval.Bounce.Done(fields(idx), "")
      case _ :: Nil => throw new EvalError(s"accessor for $tag called on non-record")
      case _        => throw new EvalError("record accessor requires 1 argument")

  private def callbackBody(fn: Value): Option[List[Value]] = fn match
    case Value.Lambda(_, body, _, _, _) => Some(body)
    case _                              => None

  def evalCallCC(args: List[Value]): (Value, String) =
    args match
      case fn :: Nil =>
        val (fnVal, fnOut) = fn match
          case _: Value.Lambda | _: Value.Continuation | Value.Void => (fn, "")
          case _                                                    => Eval.eval(fn, Env.empty)
        ContState.pendingValue(0) match
          case Some((savedBodyOpt, v)) if pendingMatches(fnVal, savedBodyOpt) =>
            ContState.pendingValue(0) = None
            (v, fnOut)
          case _ =>
            val id = ContState.freshId()
            callbackBody(fnVal).foreach { body =>
              ContState.callbackBodies(0) = ContState.callbackBodies(0) + (id -> body)
            }
            ContState.currentCheckpoint(0).foreach { cp =>
              ContState.checkpoints(0) = ContState.checkpoints(0) + (id -> cp)
            }
            val k = Value.Continuation(id)
            try Eval.resolveBounce(applyFnBounce(fnVal, List(k)))
            catch
              case e: ContinuationInvoked if e.id == id =>
                (e.value, fnOut)
      case _ => throw new EvalError("call/cc requires 1 argument")

  private def pendingMatches(fn: Value, savedBodyOpt: Option[List[Value]]): Boolean =
    (callbackBody(fn), savedBodyOpt) match
      case (Some(current), Some(saved)) => current eq saved
      case (None, None)                 => true
      case _                            => false

  def evalDynamicWind(args: List[Value]): Eval.Bounce = args match
    case inThunk :: bodyThunk :: outThunk :: Nil =>
      val (_, inOut) = Eval.resolveBounce(applyFnBounce(inThunk, Nil))
      try
        val (bodyVal, bodyOut) = Eval.resolveBounce(applyFnBounce(bodyThunk, Nil))
        val (_, outOut)        = Eval.resolveBounce(applyFnBounce(outThunk, Nil))
        Eval.Bounce.Done(bodyVal, inOut + bodyOut + outOut)
      catch
        case e: ContinuationInvoked =>
          Eval.resolveBounce(applyFnBounce(outThunk, Nil))
          throw e
        case e: SchemeException =>
          Eval.resolveBounce(applyFnBounce(outThunk, Nil))
          throw e
    case _ => throw new EvalError("dynamic-wind requires 3 arguments")

  def applyApply(args: List[Value]): Eval.Bounce =
    if args.length < 2 then throw new EvalError("apply requires at least 2 arguments")
    val fn       = args.head
    val lastArg  = args.last
    val prefixes = args.slice(1, args.length - 1)
    val tailArgs = NumCharOps
      .toScalaList(lastArg)
      .getOrElse(throw new EvalError("apply: last argument must be a list"))
    applyFnBounce(fn, prefixes ++ tailArgs)

  def evalWithExceptionHandler(args: List[Value]): Eval.Bounce = args match
    case handler :: thunk :: Nil =>
      try
        val (v, o) = Eval.resolveBounce(applyFnBounce(thunk, Nil))
        Eval.Bounce.Done(v, o)
      catch
        case e: SchemeException =>
          Eval.prependOutput(
            applyFnBounce(handler, List(e.value)),
            ""
          )
    case _ => throw new EvalError("with-exception-handler requires 2 arguments")

  def applyMap(args: List[Value]): (Value, String) = args match
    case fn :: rest if rest.nonEmpty =>
      val lists = rest.map { v =>
        NumCharOps.toScalaList(v).getOrElse(throw new EvalError("map: argument is not a list"))
      }
      val transposed = lists.transpose
      val (results, out) = transposed.foldLeft((List.empty[Value], "")) { case ((acc, o), argList) =>
        val (v, o2) = Eval.resolveBounce(applyFnBounce(fn, argList))
        (acc :+ v, o + o2)
      }
      (Value.SList(results), out)
    case _ => throw new EvalError("map requires a procedure and at least one list")

  def evalCallWithValues(args: List[Value]): Eval.Bounce = args match
    case producer :: consumer :: Nil =>
      val (produced, o1) = Eval.resolveBounce(applyFnBounce(producer, Nil))
      val consumerArgs = produced match
        case Value.Values(vals) => vals
        case single             => List(single)
      Eval.prependOutput(applyFnBounce(consumer, consumerArgs), o1)
    case _ => throw new EvalError("call-with-values requires 2 arguments")
