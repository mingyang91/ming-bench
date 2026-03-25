package ming

object CekApply:
  import Builtins.isFalsy

  def parseBindingPairs(form: String, bindings: List[Expr]): List[(String, Expr)] =
    bindings.map {
      case Expr.Lst(List(Expr.Sym(name), valueExpr)) => (name, valueExpr)
      case _                                         => throw EvalError(s"$form: invalid binding")
    }

  def setupBody(s: CekState, body: List[Expr], e: Env, kk: Kont): Unit =
    s.evaluating = true
    s.env = e
    if body.length == 1 then
      s.expr = body.head
      s.k = kk
    else
      s.expr = body.head
      s.k = SeqK(body.tail, e, kk)

  def applyFunc(s: CekState, func: Expr, args: List[Expr], kk: Kont): Unit =
    func match
      case Expr.Lambda(params, restParam, body, closure) =>
        val localEnv = Evaluator.bindLambdaParams(params, restParam, args, closure)
        setupBody(s, body, localEnv, kk)
      case Expr.CaseLambda(clauses, closure) =>
        Applier.matchCaseLambda(clauses, args) match
          case Some((params, restParam, body)) =>
            val localEnv = Evaluator.bindLambdaParams(params, restParam, args, closure)
            setupBody(s, body, localEnv, kk)
          case None =>
            throw EvalError(s"case-lambda: no matching clause for ${args.length} arguments")
      case Expr.Cont(kontData) =>
        if args.length != 1 then throw EvalError("continuation: need exactly 1 argument")
        val (savedK, savedWinds) = kontData.asInstanceOf[(Kont, List[WindEntry])]
        val value                = args.head
        val currentWinds         = s.windStack
        val (toUnwind, toRewind) = computeWindTransfer(currentWinds, savedWinds)
        if toUnwind.isEmpty && toRewind.isEmpty then
          s.value = value
          s.k = savedK
          s.evaluating = false
        else
          val unwindOuts = toUnwind.map(_.outThunk)
          s.k = DynWindTransferK(unwindOuts, toRewind, savedWinds, value, savedK)
          s.evaluating = false
      case Expr.Sym(name) if name == "call/cc" || name == "call-with-current-continuation" =>
        if args.length != 1 then throw EvalError(s"$name: need exactly 1 argument")
        val contValue = Expr.Cont((kk, s.windStack))
        applyFunc(s, args.head, List(contValue), kk)
      case Expr.Sym("apply") =>
        if args.length < 2 then throw EvalError("apply: need at least 2 arguments")
        val f          = args.head
        val lastArg    = PairOps.toScalaList(args.last)
        val prefixArgs = args.slice(1, args.length - 1)
        applyFunc(s, f, prefixArgs ++ lastArg, kk)
      case Expr.Sym("map") =>
        s.value = applyMapCek(args)
        s.k = kk
        s.evaluating = false
      case Expr.Sym("for-each") =>
        s.value = applyForEachCek(args)
        s.k = kk
        s.evaluating = false
      case Expr.Sym("with-exception-handler") =>
        if args.length != 2 then throw EvalError("with-exception-handler: need exactly 2 arguments")
        val handler = args(0)
        val thunk   = args(1)
        s.exnHandlers = new SimpleExnHandler(handler, s.windStack) :: s.exnHandlers
        applyFunc(s, thunk, Nil, PopExnHandlerK(kk))
      case Expr.Sym("raise") =>
        if args.length != 1 then throw EvalError("raise: need exactly 1 argument")
        performRaise(s, args.head, kk)
      case Expr.Sym("raise-continuable") =>
        if args.length != 1 then throw EvalError("raise-continuable: need exactly 1 argument")
        performRaise(s, args.head, kk)
      case Expr.Sym("values") =>
        if args.length == 1 then
          s.value = args.head
          s.k = kk
          s.evaluating = false
        else
          s.value = Expr.Values(args)
          s.k = kk
          s.evaluating = false
      case Expr.Sym("call-with-values") =>
        if args.length != 2 then throw EvalError("call-with-values: need exactly 2 arguments")
        val producer = args(0)
        val consumer = args(1)
        applyFunc(s, producer, Nil, CallWithValuesConsumerK(consumer, kk))
      case Expr.Sym("dynamic-wind") =>
        if args.length != 3 then throw EvalError("dynamic-wind: need exactly 3 arguments")
        val (inThunk, bodyThunk, outThunk) = (args(0), args(1), args(2))
        val entry                          = new WindEntry(inThunk, outThunk)
        val afterInK                       = DynWindAfterInK(entry, bodyThunk, outThunk, kk)
        applyFunc(s, inThunk, Nil, afterInK)
      case Expr.Sym(name) if name.startsWith("%%record-") =>
        s.value = RecordOps.applyRecordOp(name, args)
        s.k = kk
        s.evaluating = false
      case Expr.Sym(name) =>
        s.value = Builtins.applyBuiltin(name, args)
        s.k = kk
        s.evaluating = false
      case _ =>
        throw EvalError(s"not a procedure: ${Display.display(func)}")

  def evalCondClauses(s: CekState, clauses: List[Expr], e: Env, kk: Kont): Unit =
    clauses match
      case Nil =>
        s.value = Expr.Bool(false)
        s.k = kk
        s.evaluating = false
      case Expr.Lst(Expr.Sym("else") :: body) :: _ =>
        setupBody(s, body, e, kk)
      case Expr.Lst(test :: body) :: rest =>
        s.k = CondK(body, rest, e, kk)
        s.expr = test
        s.env = e
        s.evaluating = true
      case _ => throw EvalError("cond: invalid clause")

  def evalCaseClauses(s: CekState, key: Expr, clauses: List[Expr], e: Env, kk: Kont): Unit =
    clauses match
      case Nil =>
        s.value = Expr.Bool(false)
        s.k = kk
        s.evaluating = false
      case Expr.Lst(Expr.Sym("else") :: body) :: _ =>
        setupBody(s, body, e, kk)
      case Expr.Lst(Expr.Lst(datums) :: body) :: rest =>
        if datums.exists(d => EqualityOps.eqv(key, d)) then setupBody(s, body, e, kk)
        else evalCaseClauses(s, key, rest, e, kk)
      case _ => throw EvalError("case: invalid clause")

  def stepDefine(s: CekState, rest: List[Expr], curEnv: Env): Unit =
    rest match
      case Expr.Sym(name) :: valExpr :: Nil =>
        s.k = DefineK(name, curEnv, s.k)
        s.expr = valExpr
      case Expr.Lst(Expr.Sym(name) :: params) :: body if body.nonEmpty =>
        val (pn, rp) = ParamUtils.extractParamsWithRest("define", params)
        curEnv.define(name, Expr.Lambda(pn, rp, body, curEnv))
        s.value = Expr.Bool(false)
        s.evaluating = false
      case _ => throw EvalError("define: invalid syntax")

  def stepLet(s: CekState, rest: List[Expr], curEnv: Env): Unit =
    rest match
      case Expr.Sym(name) :: Expr.Lst(bindings) :: body if body.nonEmpty =>
        val parsed     = parseBindingPairs("let", bindings)
        val paramNames = parsed.map(_._1)
        if parsed.isEmpty then
          val letEnv = curEnv.child()
          letEnv.define(name, Expr.Lambda(paramNames, None, body, letEnv))
          val localEnv = Evaluator.bindLambdaParams(paramNames, None, Nil, letEnv)
          setupBody(s, body, localEnv, s.k)
        else
          val valueExprs = parsed.map(_._2)
          s.k = NamedLetBindK(name, paramNames, Nil, valueExprs.tail, body, curEnv, s.k)
          s.expr = valueExprs.head
      case Expr.Lst(bindings) :: body if body.nonEmpty =>
        val parsed = parseBindingPairs("let", bindings)
        val letEnv = curEnv.child()
        if parsed.isEmpty then setupBody(s, body, letEnv, s.k)
        else
          s.k = BindK(parsed.head._1, parsed.tail, body, letEnv, curEnv, s.k)
          s.expr = parsed.head._2
      case _ => throw EvalError("let: invalid syntax")

  def stepLetStar(s: CekState, rest: List[Expr], curEnv: Env, form: String): Unit =
    rest match
      case Expr.Lst(bindings) :: body if body.nonEmpty =>
        val parsed = parseBindingPairs(form, bindings)
        val letEnv = curEnv.child()
        if form == "letrec*" then parsed.foreach((n, _) => letEnv.define(n, Expr.Bool(false)))
        if parsed.isEmpty then setupBody(s, body, letEnv, s.k)
        else
          s.k = BindK(parsed.head._1, parsed.tail, body, letEnv, letEnv, s.k)
          s.expr = parsed.head._2
          s.env = letEnv
      case _ => throw EvalError(s"$form: invalid syntax")

  def stepLetrec(s: CekState, rest: List[Expr], curEnv: Env): Unit =
    rest match
      case Expr.Lst(bindings) :: body if body.nonEmpty =>
        val parsed = parseBindingPairs("letrec", bindings)
        val letEnv = curEnv.child()
        parsed.foreach((n, _) => letEnv.define(n, Expr.Bool(false)))
        if parsed.isEmpty then setupBody(s, body, letEnv, s.k)
        else
          s.k = BindK(parsed.head._1, parsed.tail, body, letEnv, letEnv, s.k)
          s.expr = parsed.head._2
          s.env = letEnv
      case _ => throw EvalError("letrec: invalid syntax")

  def performRaise(s: CekState, exnValue: Expr, currentK: Kont): Unit =
    if s.exnHandlers.isEmpty then throw EvalError(s"unhandled exception: ${Display.display(exnValue)}")
    val entry = s.exnHandlers.head
    s.exnHandlers = s.exnHandlers.tail
    entry match
      case h: SimpleExnHandler =>
        val (toUnwind, toRewind) = computeWindTransfer(s.windStack, h.windStack)
        if toUnwind.isEmpty && toRewind.isEmpty then applyFunc(s, h.handler, List(exnValue), RaiseReturnK)
        else
          val unwindOuts = toUnwind.map(_.outThunk)
          s.k = DynWindTransferK(
            unwindOuts,
            toRewind,
            h.windStack,
            exnValue,
            CallExnHandlerK(h.handler, exnValue, RaiseReturnK)
          )
          s.evaluating = false
      case g: GuardExnHandler =>
        val clauseEnv = g.env.child()
        clauseEnv.define(g.varName, exnValue)
        val (toUnwind, toRewind) = computeWindTransfer(s.windStack, g.windStack)
        if toUnwind.isEmpty && toRewind.isEmpty then evalGuardClauses(s, g.varName, g.clauses, clauseEnv, g.exitK)
        else
          val unwindOuts = toUnwind.map(_.outThunk)
          s.k = DynWindTransferK(
            unwindOuts,
            toRewind,
            g.windStack,
            exnValue,
            GuardStartK(g.varName, g.clauses, clauseEnv, g.exitK)
          )
          s.evaluating = false

  def evalGuardClauses(s: CekState, varName: String, clauses: List[Expr], env: Env, exitK: Kont): Unit =
    clauses match
      case Nil =>
        val exnValue = env.lookup(varName)
        performRaise(s, exnValue, exitK)
      case Expr.Lst(Expr.Sym("else") :: body) :: _ =>
        setupBody(s, body, env, exitK)
      case Expr.Lst(test :: body) :: rest =>
        s.k = GuardCondK(varName, body, rest, env, exitK)
        s.expr = test
        s.env = env
        s.evaluating = true
      case _ => throw EvalError("guard: invalid clause")

  private def applyMapCek(args: List[Expr]): Expr =
    if args.length < 2 then throw EvalError("map: need at least 2 arguments")
    val proc  = args.head
    val lists = args.tail.map(PairOps.toScalaList)
    val len   = lists.head.length
    if !lists.forall(_.length == len) then throw EvalError("map: lists must have equal length")
    val result = (0 until len).toList.map { i =>
      val argSlice = lists.map(_(i))
      Applier.applyProc(proc, argSlice)
    }
    PairOps.makeList(result)

  private def applyForEachCek(args: List[Expr]): Expr =
    if args.length < 2 then throw EvalError("for-each: need at least 2 arguments")
    val proc  = args.head
    val lists = args.tail.map(PairOps.toScalaList)
    val len   = lists.head.length
    if !lists.forall(_.length == len) then throw EvalError("for-each: lists must have equal length")
    for i <- 0 until len do
      val argSlice = lists.map(_(i))
      Applier.applyProc(proc, argSlice)
    Expr.Bool(false)

  def computeWindTransfer(
    current: List[WindEntry],
    target: List[WindEntry]
  ): (List[WindEntry], List[WindEntry]) =
    val cLen = current.length
    val tLen = target.length
    var c    = current; var t = target
    if cLen > tLen then for _ <- 0 until (cLen - tLen) do c = c.tail
    else for _ <- 0 until (tLen - cLen) do t = t.tail
    while c.nonEmpty && !(c eq t) do
      c = c.tail; t = t.tail
    val commonLen = c.length
    val toUnwind  = current.take(cLen - commonLen)        // innermost first
    val toRewind  = target.take(tLen - commonLen).reverse // outermost first
    (toUnwind, toRewind)
