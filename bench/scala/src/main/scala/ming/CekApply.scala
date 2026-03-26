package ming

import SchemeTypes.{errAt, pairToScalaList, Env, Pos, Value}
import CekSteps.bodyToCek

/** Function application dispatch for the CEK machine. */
object CekApply:

  def apply(
    func: Value,
    args: List[Value],
    pos: Pos,
    env: Env,
    k: Kont
  ): CekState = func match
    case Value.VBuiltin("call/cc") | Value.VBuiltin("call-with-current-continuation") =>
      if args.length != 1 then throw errAt(pos, "call/cc requires 1 argument")
      apply(args.head, List(Value.VContinuation(k, Evaluator.windStack)), pos, env, k)

    case Value.VBuiltin("raise") =>
      if args.length != 1 then throw errAt(pos, "raise requires 1 argument")
      throw new SchemeRaise(args.head)

    case Value.VBuiltin("with-exception-handler") =>
      if args.length != 2 then throw errAt(pos, "with-exception-handler requires 2 arguments")
      Evaluator.exceptionHandlers =
        ExceptionHandler.Proc(args(0), env, Evaluator.windStack) :: Evaluator.exceptionHandlers
      apply(args(1), Nil, pos, env, Kont.PopHandler(k))

    case Value.VBuiltin("dynamic-wind") =>
      if args.length != 3 then throw errAt(pos, "dynamic-wind requires 3 arguments")
      val entry   = new WindEntry(args(0), args(2))
      val afterIn = Kont.DynWindAfterIn(args(1), entry, env, pos, k)
      apply(args(0), Nil, pos, env, afterIn)

    case Value.VBuiltin("values") =>
      args match
        case single :: Nil => CekState.ApplyK(single, k)
        case _             => CekState.ApplyK(Value.VValues(args), k)

    case Value.VBuiltin("call-with-values") =>
      if args.length != 2 then throw errAt(pos, "call-with-values requires 2 arguments")
      apply(args(0), Nil, pos, env, Kont.CallWithValues(args(1), env, pos, k))

    case Value.VBuiltin("apply") =>
      if args.length < 2 then throw errAt(pos, "apply requires at least 2 arguments")
      val lastArg = args.last match
        case Value.VList(elems) => elems
        case Value.VPair(_)     => pairToScalaList(args.last, pos)
        case _                  => throw errAt(pos, "apply: last argument must be a list")
      apply(args.head, args.slice(1, args.length - 1) ++ lastArg, pos, env, k)

    case Value.VBuiltin(name) =>
      try CekState.ApplyK(Builtins(name, args, pos, env), k)
      catch case ci: ContinuationInvoke => CekState.ApplyK(ci.value, ci.kont)

    case Value.VLambda(params, restParam, body, closure) =>
      val callEnv = closure.child()
      EvalTail.bindArgs(params, restParam, args, callEnv, pos)
      bodyToCek(body, callEnv, k)

    case Value.VCaseLambda(clauses) =>
      clauses
        .find { case (params, restParam, _, _) =>
          restParam match
            case None    => args.length == params.length
            case Some(_) => args.length >= params.length
        }
        .map { case (params, restParam, body, closure) =>
          val callEnv = closure.child()
          EvalTail.bindArgs(params, restParam, args, callEnv, pos)
          bodyToCek(body, callEnv, k)
        }
        .getOrElse(throw errAt(pos, "wrong number of arguments"))

    case Value.VContinuation(savedK, savedWindStack) =>
      if args.isEmpty then throw errAt(pos, "continuation requires at least 1 argument")
      val result   = if args.length == 1 then args.head else Value.VValues(args)
      val common   = Evaluator.commonTail(Evaluator.windStack, savedWindStack)
      val toUnwind = Evaluator.windStack.take(Evaluator.windStack.length - common.length)
      val toRewind = savedWindStack.take(savedWindStack.length - common.length).reverse
      val ops      = toUnwind.map(e => (false, e)) ++ toRewind.map(e => (true, e))
      if ops.isEmpty then CekState.ApplyK(result, savedK)
      else
        CekState.ApplyK(
          Value.VVoid,
          Kont.DynWindTransition(ops, result, savedK, env, pos)
        )

    case _ => throw errAt(pos, "not a procedure")
