package ming

object CekHelpers:

  import Builtins.isFalsy
  import CekApply.{applyFunc, setupBody}

  def stepLogical(s: CekState, args: List[Expr], curEnv: Env, isAnd: Boolean): Unit =
    if args.isEmpty then
      s.value = Expr.Bool(isAnd)
      s.evaluating = false
    else if args.length == 1 then s.expr = args.head
    else
      s.k = if isAnd then AndK(args.tail, curEnv, s.k) else OrK(args.tail, curEnv, s.k)
      s.expr = args.head

  def stepLogicalKont(
    s: CekState,
    remaining: List[Expr],
    e: Env,
    kk: Kont,
    isAnd: Boolean
  ): Unit =
    if remaining.length == 1 then
      s.expr = remaining.head
      s.env = e
      s.k = kk
      s.evaluating = true
    else
      s.k = if isAnd then AndK(remaining.tail, e, kk) else OrK(remaining.tail, e, kk)
      s.expr = remaining.head
      s.env = e
      s.evaluating = true

  def stepDynWind(s: CekState, k: Kont): Unit = k match
    case DynWindAfterInK(entry, bodyThunk, outThunk, kk) =>
      s.windStack = entry :: s.windStack
      applyFunc(s, bodyThunk, Nil, DynWindAfterBodyK(entry, outThunk, kk))

    case DynWindAfterBodyK(entry, outThunk, kk) =>
      val bodyValue = s.value
      s.windStack = s.windStack.tail
      applyFunc(s, outThunk, Nil, DynWindAfterOutK(bodyValue, kk))

    case DynWindAfterOutK(bodyValue, kk) =>
      s.value = bodyValue
      s.k = kk

    case DynWindTransferK(unwindOuts, rewindEntries, targetWinds, value, savedK) =>
      if unwindOuts.nonEmpty then
        s.windStack = s.windStack.tail
        val nextK = DynWindTransferK(unwindOuts.tail, rewindEntries, targetWinds, value, savedK)
        applyFunc(s, unwindOuts.head, Nil, nextK)
      else if rewindEntries.nonEmpty then
        s.windStack = rewindEntries.head :: s.windStack
        val nextK = DynWindTransferK(Nil, rewindEntries.tail, targetWinds, value, savedK)
        applyFunc(s, rewindEntries.head.inThunk, Nil, nextK)
      else
        s.value = value
        s.k = savedK
        s.evaluating = false

    case _ => ()

  def stepBindKont(s: CekState, k: Kont): Unit = k match
    case BindK(name, remaining, body, bindEnv, evalEnv, kk) =>
      bindEnv.define(name, s.value)
      if remaining.isEmpty then setupBody(s, body, bindEnv, kk)
      else
        s.k = BindK(remaining.head._1, remaining.tail, body, bindEnv, evalEnv, kk)
        s.expr = remaining.head._2
        s.env = evalEnv
        s.evaluating = true

    case NamedLetBindK(name, paramNames, evaledRev, remaining, body, evalEnv, kk) =>
      val newEvaled = s.value :: evaledRev
      if remaining.isEmpty then
        val initVals = newEvaled.reverse
        val letEnv   = evalEnv.child()
        letEnv.define(name, Expr.Lambda(paramNames, None, body, letEnv))
        val localEnv = Evaluator.bindLambdaParams(paramNames, None, initVals, letEnv)
        setupBody(s, body, localEnv, kk)
      else
        s.k = NamedLetBindK(name, paramNames, newEvaled, remaining.tail, body, evalEnv, kk)
        s.expr = remaining.head
        s.env = evalEnv
        s.evaluating = true

    case _ => ()

  def stepGuard(s: CekState, rest: List[Expr], curEnv: Env): Unit =
    rest match
      case Expr.Lst(Expr.Sym(varName) :: clauses) :: body if body.nonEmpty =>
        val guardHandler = new GuardExnHandler(varName, clauses, curEnv, s.k, s.windStack)
        s.exnHandlers = guardHandler :: s.exnHandlers
        setupBody(s, body, curEnv, PopExnHandlerK(s.k))
      case _ => throw EvalError("guard: invalid syntax")
