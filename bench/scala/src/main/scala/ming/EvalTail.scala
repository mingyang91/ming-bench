package ming

import SchemeTypes.{errAt, isTruthy, schemeList, Env, Pos, Value}

/** Result of a tail-position form: final value or continuation for TCO trampoline. */
private[ming] enum TcoResult:
  case Done(value: Value)
  case Bounce(expr: Expr, env: Env)

/** Extracted tail-position handlers for the Evaluator's TCO trampoline. */
private[ming] object EvalTail:
  import TcoResult.*

  def evalIf(
    rest: List[Expr],
    env: Env,
    p: Pos,
    evalFn: (Expr, Env) => Value
  ): TcoResult = rest match
    case cond :: thenExpr :: elseExpr :: Nil =>
      if isTruthy(evalFn(cond, env)) then Bounce(thenExpr, env)
      else Bounce(elseExpr, env)
    case cond :: thenExpr :: Nil =>
      if isTruthy(evalFn(cond, env)) then Bounce(thenExpr, env)
      else Done(Value.VVoid)
    case _ => throw errAt(p, "invalid if")

  def evalBegin(
    body: List[Expr],
    env: Env,
    evalFn: (Expr, Env) => Value
  ): TcoResult =
    if body.isEmpty then Done(Value.VVoid)
    else
      body.init.foreach(e => evalFn(e, env))
      Bounce(body.last, env)

  def bindArgs(
    params: List[String],
    restParam: Option[String],
    argVals: List[Value],
    env: Env,
    p: Pos
  ): Unit = restParam match
    case None =>
      if argVals.length != params.length then throw errAt(p, "wrong number of arguments")
      params.zip(argVals).foreach((pn, a) => env.define(pn, a))
    case Some(rest) =>
      if argVals.length < params.length then throw errAt(p, "wrong number of arguments")
      params.zip(argVals).foreach((pn, a) => env.define(pn, a))
      env.define(rest, schemeList(argVals.drop(params.length)))

  def evalAnd(
    args: List[Expr],
    env: Env,
    evalFn: (Expr, Env) => Value
  ): TcoResult =
    if args.isEmpty then return Done(Value.VBool(true))
    var remaining = args
    while remaining.tail.nonEmpty do
      evalFn(remaining.head, env) match
        case Value.VBool(false) => return Done(Value.VBool(false))
        case _                  => ()
      remaining = remaining.tail
    Bounce(remaining.head, env)

  def evalOr(
    args: List[Expr],
    env: Env,
    evalFn: (Expr, Env) => Value
  ): TcoResult =
    if args.isEmpty then return Done(Value.VBool(false))
    var remaining = args
    while remaining.tail.nonEmpty do
      val result = evalFn(remaining.head, env)
      if isTruthy(result) then return Done(result)
      remaining = remaining.tail
    Bounce(remaining.head, env)

  def evalLet(
    rest: List[Expr],
    env: Env,
    p: Pos,
    evalFn: (Expr, Env) => Value
  ): TcoResult = rest match
    case Expr.Symbol(name, _) :: Expr.SList(bindings, _) :: body if body.nonEmpty =>
      evalNamedLet(name, bindings, body, env, p, evalFn)
    case Expr.SList(bindings, _) :: body if body.nonEmpty =>
      evalSimpleLet(bindings, body, env, p, evalFn)
    case _ => throw errAt(p, "invalid let")

  private def evalNamedLet(
    name: String,
    bindings: List[Expr],
    body: List[Expr],
    env: Env,
    p: Pos,
    evalFn: (Expr, Env) => Value
  ): TcoResult =
    val paramNames = bindings.map {
      case Expr.SList(Expr.Symbol(n, _) :: _ :: Nil, _) => n
      case _                                            => throw errAt(p, "invalid let binding")
    }
    val initVals = bindings.map {
      case Expr.SList(_ :: initExpr :: Nil, _) => evalFn(initExpr, env)
      case _                                   => throw errAt(p, "invalid let binding")
    }
    val loopEnv = env.child()
    val lambda  = Value.VLambda(paramNames, None, body, loopEnv)
    loopEnv.define(name, lambda)
    val callEnv = loopEnv.child()
    paramNames.zip(initVals).foreach((pn, a) => callEnv.define(pn, a))
    body.init.foreach(e => evalFn(e, callEnv))
    Bounce(body.last, callEnv)

  private def evalSimpleLet(
    bindings: List[Expr],
    body: List[Expr],
    env: Env,
    p: Pos,
    evalFn: (Expr, Env) => Value
  ): TcoResult =
    val letEnv = env.child()
    for b <- bindings do
      b match
        case Expr.SList(Expr.Symbol(name, _) :: initExpr :: Nil, _) =>
          letEnv.define(name, evalFn(initExpr, env))
        case _ => throw errAt(p, "invalid let binding")
    body.init.foreach(e => evalFn(e, letEnv))
    Bounce(body.last, letEnv)

  def evalLetStar(
    rest: List[Expr],
    env: Env,
    p: Pos,
    evalFn: (Expr, Env) => Value
  ): TcoResult = rest match
    case Expr.SList(bindings, _) :: body if body.nonEmpty =>
      val letEnv = env.child()
      for b <- bindings do
        b match
          case Expr.SList(Expr.Symbol(name, _) :: initExpr :: Nil, _) =>
            letEnv.define(name, evalFn(initExpr, letEnv))
          case _ => throw errAt(p, "invalid let* binding")
      body.init.foreach(e => evalFn(e, letEnv))
      Bounce(body.last, letEnv)
    case _ => throw errAt(p, "invalid let*")

  def evalCond(
    clauses: List[Expr],
    env: Env,
    evalFn: (Expr, Env) => Value,
    posOf: Expr => Pos
  ): TcoResult =
    var remaining = clauses
    while remaining.nonEmpty do
      remaining.head match
        case Expr.SList(Expr.Symbol("else", _) :: body, _) =>
          return bounceBody(body, env, evalFn)
        case Expr.SList(test :: body, _) =>
          val testVal = evalFn(test, env)
          if isTruthy(testVal) then
            if body.nonEmpty then return bounceBody(body, env, evalFn)
            else return Done(testVal)
          else remaining = remaining.tail
        case e => throw errAt(posOf(e), "invalid cond")
    Done(Value.VVoid)

  private def bounceBody(
    body: List[Expr],
    env: Env,
    evalFn: (Expr, Env) => Value
  ): TcoResult =
    if body.nonEmpty then
      body.init.foreach(e => evalFn(e, env))
      Bounce(body.last, env)
    else Done(Value.VVoid)

  def evalApp(
    head: Expr,
    args: List[Expr],
    env: Env,
    p: Pos,
    evalFn: (Expr, Env) => Value
  ): TcoResult =
    val macroOpt = head match
      case Expr.Symbol(name, _) =>
        env.lookupOpt(name).collect { case m: Value.VMacro => m }
      case _ => None
    macroOpt match
      case Some(m) =>
        val (expanded, injections) =
          MacroExpander.expand(m, Expr.SList(head :: args, p), p)
        val macroEnv = env.child()
        injections.foreach((k, v) => macroEnv.define(k, v))
        Bounce(expanded, macroEnv)
      case None =>
        val func    = evalFn(head, env)
        val argVals = args.map(a => evalFn(a, env))
        applyForTco(func, argVals, p, env, evalFn)

  private def applyForTco(
    func: Value,
    argVals: List[Value],
    p: Pos,
    env: Env,
    evalFn: (Expr, Env) => Value
  ): TcoResult = func match
    case Value.VBuiltin(name) =>
      Done(Builtins(name, argVals, p, env))
    case Value.VLambda(params, restParam, body, closure) =>
      val callEnv = closure.child()
      bindArgs(params, restParam, argVals, callEnv, p)
      body.init.foreach(e => evalFn(e, callEnv))
      Bounce(body.last, callEnv)
    case Value.VCaseLambda(clauses) =>
      applyCaseLambda(clauses, argVals, p, evalFn)
    case _ => throw errAt(p, "not a procedure")

  private def applyCaseLambda(
    clauses: List[(List[String], Option[String], List[Expr], Env)],
    argVals: List[Value],
    p: Pos,
    evalFn: (Expr, Env) => Value
  ): TcoResult =
    val matched = clauses.find { case (params, restParam, _, _) =>
      restParam match
        case None    => argVals.length == params.length
        case Some(_) => argVals.length >= params.length
    }
    matched match
      case Some((params, restParam, body, closure)) =>
        val callEnv = closure.child()
        bindArgs(params, restParam, argVals, callEnv, p)
        body.init.foreach(e => evalFn(e, callEnv))
        Bounce(body.last, callEnv)
      case None => throw errAt(p, "wrong number of arguments")
