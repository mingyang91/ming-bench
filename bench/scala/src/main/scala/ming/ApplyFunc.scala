package ming

import SchemeTypes.{errAt, pairToScalaList, Env, Pos, Value}

/** Non-CEK function application, used by builtins like map/for-each. */
object ApplyFunc:

  private def evalBody(body: List[Expr], env: Env): Value =
    if body.isEmpty then Value.VVoid
    else Evaluator.runCekInternal(CekSteps.bodyToCek(body, env, Kont.Halt))

  def apply(
    func: Value,
    args: List[Value],
    pos: Pos,
    env: Env
  ): Value = func match
    case Value.VBuiltin("call/cc") | Value.VBuiltin("call-with-current-continuation") =>
      if args.length != 1 then throw errAt(pos, "call/cc requires 1 argument")
      val proc    = args.head
      val contVal = Value.VContinuation(Kont.Halt, Evaluator.windStack)
      apply(proc, List(contVal), pos, env)
    case Value.VBuiltin("apply") =>
      if args.length < 2 then throw errAt(pos, "apply requires at least 2 arguments")
      val innerFunc = args.head
      val lastArg = args.last match
        case Value.VList(elems) => elems
        case Value.VPair(_)     => pairToScalaList(args.last, pos)
        case _                  => throw errAt(pos, "apply: last argument must be a list")
      val prefixArgs = args.slice(1, args.length - 1)
      apply(innerFunc, prefixArgs ++ lastArg, pos, env)
    case Value.VBuiltin(name) => Builtins(name, args, pos, env)
    case Value.VLambda(params, restParam, body, closure) =>
      val callEnv = closure.child()
      EvalTail.bindArgs(params, restParam, args, callEnv, pos)
      evalBody(body, callEnv)
    case Value.VCaseLambda(clauses) =>
      val matched = clauses.find { case (params, restParam, _, _) =>
        restParam match
          case None    => args.length == params.length
          case Some(_) => args.length >= params.length
      }
      matched match
        case Some((params, restParam, body, closure)) =>
          val callEnv = closure.child()
          EvalTail.bindArgs(params, restParam, args, callEnv, pos)
          evalBody(body, callEnv)
        case None => throw errAt(pos, "wrong number of arguments")
    case Value.VContinuation(savedK, _) =>
      if args.length != 1 then throw errAt(pos, "continuation requires 1 argument")
      throw new ContinuationInvoke(args.head, savedK)
    case _ => throw errAt(pos, "not a procedure")
