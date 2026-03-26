package ming

import scala.collection.mutable

object ProcApply:

  def applyFunction(op: SchemeVal, args: List[SchemeVal], k: Kont): MState =
    op match
      case SchemeBuiltin("apply", _) =>
        if args.size < 2 then throw new EvalError("apply: expected at least 2 arguments")
        val proc = args.head
        val lastList = SchemeListOps
          .toScalaList(args.last)
          .getOrElse(throw new EvalError("apply: last argument must be a list"))
        val allArgs = args.slice(1, args.size - 1) ++ lastList
        applyFunction(proc, allArgs, k)

      case SchemeBuiltin(_, fn) =>
        SApply(fn(args), k)

      case SchemeLambda(params, restParam, body, closureEnv) =>
        val localEnv = EvalHelpers.bindArgs(params, restParam, args, closureEnv)
        Evaluator.evalBodyCEK(body, localEnv, k)

      case SchemeCaseLambda(clauses) =>
        val matching = clauses.find { lam =>
          lam.restParam match
            case None    => args.size == lam.params.size
            case Some(_) => args.size >= lam.params.size
        }
        matching match
          case Some(lam) => applyFunction(lam, args, k)
          case None      => throw new EvalError(s"no matching clause for ${args.size} arguments")

      case SchemeCallCC =>
        if args.size != 1 then throw new EvalError("call/cc: expected 1 argument")
        val cont = new SchemeContinuation(k, Evaluator.windStack)
        applyFunction(args.head, List(cont), k)

      case SchemeWithExceptionHandler =>
        if args.size != 2 then throw new EvalError("with-exception-handler: expected 2 arguments")
        Evaluator.handlerStack = WHHandler(args(0)) :: Evaluator.handlerStack
        applyFunction(args(1), Nil, ExHandlerPopK(k))

      case SchemeDynamicWind =>
        if args.size != 3 then throw new EvalError("dynamic-wind: expected 3 arguments")
        val List(inThunk, bodyThunk, outThunk) = args: @unchecked
        applyFunction(inThunk, Nil, DynWindAfterInK(inThunk, bodyThunk, outThunk, k))

      case cont: SchemeContinuation =>
        if args.size != 1 then throw new EvalError("continuation: expected 1 argument")
        cont.savedK match
          case targetK: Kont =>
            val targetWind = cont.savedWind
            val v          = args.head
            val common     = EvalHelpers.commonWindTail(Evaluator.windStack, targetWind)
            val toUnwind   = Evaluator.windStack.take(Evaluator.windStack.length - common.length)
            val toRewind   = targetWind.take(targetWind.length - common.length).reverse
            val actions: List[WindAction] =
              toUnwind.map(e => DoUnwind(e._2)) ++ toRewind.map(e => DoRewind(e._1, e))
            processWindActions(actions, v, targetK)
          case _ => throw new EvalError("invalid continuation")

      case _ => throw new EvalError(s"not a procedure: ${op.display}")

  def evalGuardForm(args: List[Expr], env: Env, k: Kont): MState =
    if args.size < 2 then throw new EvalError("guard: bad syntax")
    val guardSpec = args.head match
      case SList(elems, _) if elems.nonEmpty => elems
      case _                                 => throw new EvalError("guard: bad syntax")
    val variable = guardSpec.head match
      case Symbol(name, _) => name
      case _               => throw new EvalError("guard: expected variable name")
    val clauses = guardSpec.tail
    val body    = args.tail
    Evaluator.handlerStack = GuardExHandler(variable, clauses, env, k, Evaluator.windStack) :: Evaluator.handlerStack
    Evaluator.evalBodyCEK(body, env, GuardBodyPopK(k))

  def evaluateGuardClauses(
    exnVal: SchemeVal,
    variable: String,
    clauses: List[Expr],
    env: Env,
    k: Kont
  ): MState =
    if clauses.isEmpty then throw new SchemeRaisedException(exnVal)
    val clause = clauses.head match
      case SList(elems, _) if elems.nonEmpty => elems
      case _                                 => throw new EvalError("guard: bad clause")
    clause.head match
      case Symbol("else", _) =>
        val localEnv = new Env(mutable.Map(variable -> exnVal), Some(env))
        Evaluator.evalBodyCEK(clause.tail, localEnv, k)
      case test =>
        val localEnv = new Env(mutable.Map(variable -> exnVal), Some(env))
        SEval(test, localEnv, GuardTestK(variable, exnVal, clause.tail, clauses.tail, env, k))

  def processWindActions(
    actions: List[WindAction],
    savedVal: SchemeVal,
    targetK: Kont
  ): MState =
    actions match
      case Nil => SApply(savedVal, targetK)
      case DoUnwind(outThunk) :: rest =>
        Evaluator.windStack = Evaluator.windStack.tail
        applyFunction(outThunk, Nil, WindContK(rest, savedVal, targetK))
      case DoRewind(inThunk, entry) :: rest =>
        applyFunction(inThunk, Nil, WindPushK(entry, rest, savedVal, targetK))
