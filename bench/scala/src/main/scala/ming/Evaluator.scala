package ming

/** Scheme interpreter entry point. */
object Evaluator:

  private enum Step:
    case Ret(value: SchemeVal)
    case Tail(expr: SchemeVal, env: Env)

  private[ming] def isTruthy(v: SchemeVal): Boolean = v match
    case SchemeVal.SBool(false) => false
    case _                      => true

  private[ming] def evalBody(body: List[SchemeVal], env: Env): SchemeVal =
    body.foldLeft(SchemeVal.SVoid: SchemeVal)((_, expr) => eval(expr, env))

  def eval(initExpr: SchemeVal, initEnv: Env): SchemeVal =
    var curExpr: SchemeVal = initExpr
    var curEnv: Env        = initEnv

    while true do
      val expr = curExpr
      val env  = curEnv
      try
        evalExpr(expr, env) match
          case Step.Ret(v) => return v
          case Step.Tail(e, env2) =>
            curExpr = e; curEnv = env2
      catch
        case e: EvalError if !e.hasPosition =>
          expr.pos match
            case Some(p) =>
              throw new EvalError(
                s"${e.getMessage} at ${p.line}:${p.col}",
                hasPosition = true
              )
            case None => throw e
    throw new RuntimeException("unreachable")

  private def evalExpr(expr: SchemeVal, env: Env): Step =
    expr match
      case SchemeVal.SInt(_) | SchemeVal.SFloat(_) | SchemeVal.SRational(_, _) | SchemeVal.SBool(_) |
          SchemeVal.SString(_, _) | SchemeVal.SChar(_) | SchemeVal.SVoid | SchemeVal.SPair(_) | SchemeVal.SVector(_) =>
        Step.Ret(expr)
      case SchemeVal.SSymbol(name) => Step.Ret(env.get(name))
      case SchemeVal.SList(elems)  => dispatchList(elems, env)
      case other                   => Step.Ret(other)

  private def dispatchList(elems: List[SchemeVal], env: Env): Step =
    elems match
      case Nil => throw new EvalError("empty application")
      case SchemeVal.SSymbol("quote") :: args =>
        if args.length != 1 then throw new EvalError("quote: expected 1 argument")
        Step.Ret(args.head)
      case SchemeVal.SSymbol("if") :: args     => evalIfStep(args, env)
      case SchemeVal.SSymbol("define") :: args => Step.Ret(DefineForms.evalDefine(args, env))
      case SchemeVal.SSymbol("lambda") :: args => Step.Ret(DefineForms.evalLambda(args, env))
      case SchemeVal.SSymbol("and") :: args    => evalAndStep(args, env)
      case SchemeVal.SSymbol("or") :: args     => evalOrStep(args, env)
      case SchemeVal.SSymbol("let") :: args =>
        val (te, tenv) = BindingForms.evalLetTail(args, env)
        Step.Tail(te, tenv)
      case SchemeVal.SSymbol("set!") :: args    => Step.Ret(DefineForms.evalSet(args, env))
      case SchemeVal.SSymbol("begin") :: args   => evalBeginStep(args, env)
      case SchemeVal.SSymbol("cond") :: clauses => evalCondStep(clauses, env)
      case SchemeVal.SSymbol("define-syntax") :: args =>
        Step.Ret(DefineForms.evalDefineSyntax(args, env))
      case SchemeVal.SSymbol("case-lambda") :: args =>
        Step.Ret(BindingForms.evalCaseLambda(args, env))
      case SchemeVal.SSymbol("define-record-type") :: args =>
        Step.Ret(RecordOps.evalDefineRecordType(args, env))
      case SchemeVal.SSymbol("letrec") :: args =>
        val (te, tenv) = BindingForms.evalLetrecTail(args, env)
        Step.Tail(te, tenv)
      case SchemeVal.SSymbol("letrec*") :: args =>
        val (te, tenv) = BindingForms.evalLetrecStarTail(args, env)
        Step.Tail(te, tenv)
      case SchemeVal.SSymbol("case") :: args => Step.Ret(BindingForms.evalCase(args, env))
      case SchemeVal.SSymbol("do") :: args   => Step.Ret(BindingForms.evalDo(args, env))
      case SchemeVal.SSymbol("let*") :: args =>
        val (te, tenv) = BindingForms.evalLetStarTail(args, env)
        Step.Tail(te, tenv)
      case SchemeVal.SSymbol(name) :: _ if env.lookup(name).exists(isMacro) =>
        val macro_   = env.get(name).asMatchedMacro
        val expanded = Macro.expand(macro_, SchemeVal.SList(elems))
        Step.Tail(expanded, env)
      case head :: args => evalApplicationStep(head, args, env)

  private def evalIfStep(args: List[SchemeVal], env: Env): Step =
    if args.length < 2 || args.length > 3 then throw new EvalError("if: expected 2 or 3 arguments")
    val cond = eval(args(0), env)
    if isTruthy(cond) then Step.Tail(args(1), env)
    else if args.length == 3 then Step.Tail(args(2), env)
    else Step.Ret(SchemeVal.SVoid)

  private def evalAndStep(args: List[SchemeVal], env: Env): Step =
    if args.isEmpty then return Step.Ret(SchemeVal.SBool(true))
    var i = 0
    while i < args.length - 1 do
      val r = eval(args(i), env)
      if !isTruthy(r) then return Step.Ret(r)
      i += 1
    Step.Tail(args.last, env)

  private def evalOrStep(args: List[SchemeVal], env: Env): Step =
    if args.isEmpty then return Step.Ret(SchemeVal.SBool(false))
    var i = 0
    while i < args.length - 1 do
      val r = eval(args(i), env)
      if isTruthy(r) then return Step.Ret(r)
      i += 1
    Step.Tail(args.last, env)

  private def evalBeginStep(args: List[SchemeVal], env: Env): Step =
    if args.isEmpty then return Step.Ret(SchemeVal.SVoid)
    args.init.foreach(e => eval(e, env))
    Step.Tail(args.last, env)

  private def evalCondStep(clauses: List[SchemeVal], env: Env): Step =
    var remaining = clauses
    while remaining.nonEmpty do
      remaining.head match
        case SchemeVal.SList(SchemeVal.SSymbol("else") :: body) =>
          if body.isEmpty then return Step.Ret(SchemeVal.SVoid)
          body.init.foreach(e => eval(e, env))
          return Step.Tail(body.last, env)
        case SchemeVal.SList(test :: body) =>
          val testVal = eval(test, env)
          if isTruthy(testVal) then
            if body.isEmpty then return Step.Ret(testVal)
            body.init.foreach(e => eval(e, env))
            return Step.Tail(body.last, env)
          else remaining = remaining.tail
        case _ => throw new EvalError("cond: bad clause")
    Step.Ret(SchemeVal.SVoid)

  private def evalApplicationStep(
    head: SchemeVal,
    args: List[SchemeVal],
    env: Env
  ): Step =
    val op         = eval(head, env)
    val evaledArgs = args.map(eval(_, env))
    op match
      case SchemeVal.SLambda(params, restParam, body, closure) =>
        val callEnv = setupCallEnv(params, restParam, evaledArgs, closure)
        body.init.foreach(e => eval(e, callEnv))
        Step.Tail(body.last, callEnv)
      case SchemeVal.SCaseLambda(clauses, closure) =>
        val (params, restParam, body) = findClause(clauses, evaledArgs)
        val callEnv                   = setupCallEnv(params, restParam, evaledArgs, closure)
        body.init.foreach(e => eval(e, callEnv))
        Step.Tail(body.last, callEnv)
      case _ => Step.Ret(applyProc(op, evaledArgs))

  private def setupCallEnv(
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

  private def findClause(
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

  private def isMacro(v: SchemeVal): Boolean = v match
    case _: SchemeVal.SMacro => true
    case _                   => false

  private[ming] def applyProc(op: SchemeVal, args: List[SchemeVal]): SchemeVal =
    op match
      case SchemeVal.SLambda(params, restParam, body, closure) =>
        val callEnv = setupCallEnv(params, restParam, args, closure)
        evalBody(body, callEnv)
      case SchemeVal.SCaseLambda(clauses, closure) =>
        val (cparams, crest, cbody) = findClause(clauses, args)
        val callEnv                 = setupCallEnv(cparams, crest, args, closure)
        evalBody(cbody, callEnv)
      case SchemeVal.SSymbol(name)
          if name.startsWith("__record-ctor__:") ||
            name.startsWith("__record-pred__:") ||
            name.startsWith("__record-acc__:") =>
        RecordOps.applyRecordOp(name, args)
      case SchemeVal.SSymbol(name) =>
        applyBuiltinOrHOF(name, args)
      case _ => throw new EvalError(s"not a procedure: ${op.display}")

  private def applyBuiltinOrHOF(
    name: String,
    args: List[SchemeVal]
  ): SchemeVal =
    name match
      case "display" | "write" | "newline" | "apply" | "map" | "for-each" =>
        HigherOrder(name, args, applyProc)
      case _ => Builtins.applyBuiltin(name, args)

  private def makeGlobalEnv(): Env =
    val env = Env()
    for name <- Builtins.names do env.define(name, SchemeVal.SSymbol(name))
    env

  def evalStr(input: String): String =
    val exprs = Parser.parseAll(input)
    if exprs.isEmpty then throw new EvalError("empty input")
    val env = makeGlobalEnv()
    evalBody(exprs, env).display

  def evalStrWithOutput(input: String): (String, String) =
    val buf = HigherOrder.outputBuffer.get()
    buf.clear()
    val exprs = Parser.parseAll(input)
    if exprs.isEmpty then throw new EvalError("empty input")
    val env    = makeGlobalEnv()
    val result = evalBody(exprs, env).display
    val output = buf.toString
    buf.clear()
    (result, output)
