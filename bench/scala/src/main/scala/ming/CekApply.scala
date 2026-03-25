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
      case Expr.Cont(savedK) =>
        if args.length != 1 then throw EvalError("continuation: need exactly 1 argument")
        s.value = args.head
        s.k = savedK.asInstanceOf[Kont]
        s.evaluating = false
      case Expr.Sym(name) if name == "call/cc" || name == "call-with-current-continuation" =>
        if args.length != 1 then throw EvalError(s"$name: need exactly 1 argument")
        val contValue = Expr.Cont(kk)
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
