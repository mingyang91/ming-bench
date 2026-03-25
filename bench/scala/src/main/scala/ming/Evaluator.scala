package ming

/** Scheme interpreter entry point. */
object Evaluator:

  private[ming] def isTruthy(v: SchemeVal): Boolean = v match
    case SchemeVal.SBool(false) => false
    case _                      => true

  private[ming] def evalBody(body: List[SchemeVal], env: Env): SchemeVal =
    body.foldLeft(SchemeVal.SVoid: SchemeVal)((_, expr) => eval(expr, env))

  def eval(expr: SchemeVal, env: Env): SchemeVal =
    try
      expr match
        case SchemeVal.SInt(_) | SchemeVal.SFloat(_) | SchemeVal.SRational(_, _) | SchemeVal.SBool(_) |
            SchemeVal.SString(_, _) | SchemeVal.SChar(_) | SchemeVal.SVoid | SchemeVal.SPair(_, _) |
            SchemeVal.SVector(_) =>
          expr
        case SchemeVal.SSymbol(name) => env.get(name)
        case SchemeVal.SList(elems) =>
          elems match
            case Nil => throw new EvalError("empty application")
            case SchemeVal.SSymbol("quote") :: args =>
              if args.length != 1 then throw new EvalError("quote: expected 1 argument")
              args.head
            case SchemeVal.SSymbol("if") :: args                 => evalIf(args, env)
            case SchemeVal.SSymbol("define") :: args             => evalDefine(args, env)
            case SchemeVal.SSymbol("lambda") :: args             => evalLambda(args, env)
            case SchemeVal.SSymbol("and") :: args                => evalAnd(args, env)
            case SchemeVal.SSymbol("or") :: args                 => evalOr(args, env)
            case SchemeVal.SSymbol("let") :: args                => BindingForms.evalLet(args, env)
            case SchemeVal.SSymbol("set!") :: args               => evalSet(args, env)
            case SchemeVal.SSymbol("begin") :: args              => evalBegin(args, env)
            case SchemeVal.SSymbol("cond") :: args               => evalCond(args, env)
            case SchemeVal.SSymbol("define-syntax") :: args      => evalDefineSyntax(args, env)
            case SchemeVal.SSymbol("case-lambda") :: args        => BindingForms.evalCaseLambda(args, env)
            case SchemeVal.SSymbol("define-record-type") :: args => RecordOps.evalDefineRecordType(args, env)
            case SchemeVal.SSymbol("letrec") :: args             => BindingForms.evalLetrec(args, env)
            case SchemeVal.SSymbol("letrec*") :: args            => BindingForms.evalLetrecStar(args, env)
            case SchemeVal.SSymbol("case") :: args               => BindingForms.evalCase(args, env)
            case SchemeVal.SSymbol("do") :: args                 => BindingForms.evalDo(args, env)
            case SchemeVal.SSymbol("let*") :: args               => BindingForms.evalLetStar(args, env)
            case SchemeVal.SSymbol(name) :: _ if env.lookup(name).exists(isMacro) =>
              val macro_   = env.get(name).asMatchedMacro
              val expanded = Macro.expand(macro_, SchemeVal.SList(elems))
              eval(expanded, env)
            case head :: args =>
              val op         = eval(head, env)
              val evaledArgs = args.map(eval(_, env))
              applyProc(op, evaledArgs)
        case other => other
    catch
      case e: EvalError if !e.hasPosition =>
        expr.pos match
          case Some(p) =>
            throw new EvalError(
              s"${e.getMessage} at ${p.line}:${p.col}",
              hasPosition = true
            )
          case None => throw e

  private def isMacro(v: SchemeVal): Boolean = v match
    case _: SchemeVal.SMacro => true
    case _                   => false

  private def evalIf(args: List[SchemeVal], env: Env): SchemeVal =
    if args.length < 2 || args.length > 3 then throw new EvalError("if: expected 2 or 3 arguments")
    val cond = eval(args(0), env)
    if isTruthy(cond) then eval(args(1), env)
    else if args.length == 3 then eval(args(2), env)
    else SchemeVal.SVoid

  private def evalDefine(args: List[SchemeVal], env: Env): SchemeVal =
    args match
      case SchemeVal.SSymbol(name) :: valueExpr :: Nil =>
        env.define(name, eval(valueExpr, env))
        SchemeVal.SVoid
      case SchemeVal.SList(SchemeVal.SSymbol(name) :: params) :: body if body.nonEmpty =>
        val (paramNames, restParam) = parseParams(params)
        env.define(name, SchemeVal.SLambda(paramNames, restParam, body, env))
        SchemeVal.SVoid
      case _ => throw new EvalError("define: bad syntax")

  /** Parse parameter list, returning (fixed params, optional rest param) */
  private[ming] def parseParams(
    params: List[SchemeVal]
  ): (List[String], Option[String]) =
    val dotIdx = params.indexWhere(_ == SchemeVal.SSymbol("."))
    if dotIdx >= 0 then
      if dotIdx != params.length - 2 then throw new EvalError("bad dot syntax in parameter list")
      val fixed = params.take(dotIdx).map {
        case SchemeVal.SSymbol(n) => n
        case other =>
          throw new EvalError(
            s"expected parameter name, got ${other.display}"
          )
      }
      params(dotIdx + 1) match
        case SchemeVal.SSymbol(rest) => (fixed, Some(rest))
        case other =>
          throw new EvalError(
            s"expected parameter name, got ${other.display}"
          )
    else
      val names = params.map {
        case SchemeVal.SSymbol(n) => n
        case other =>
          throw new EvalError(
            s"expected parameter name, got ${other.display}"
          )
      }
      (names, None)

  private def evalLambda(args: List[SchemeVal], env: Env): SchemeVal =
    args match
      case SchemeVal.SList(params) :: body if body.nonEmpty =>
        val (paramNames, restParam) = parseParams(params)
        SchemeVal.SLambda(paramNames, restParam, body, env)
      case SchemeVal.SSymbol(rest) :: body if body.nonEmpty =>
        SchemeVal.SLambda(Nil, Some(rest), body, env)
      case _ => throw new EvalError("lambda: bad syntax")

  private def evalAnd(args: List[SchemeVal], env: Env): SchemeVal =
    args match
      case Nil         => SchemeVal.SBool(true)
      case last :: Nil => eval(last, env)
      case head :: tail =>
        val result = eval(head, env)
        if isTruthy(result) then evalAnd(tail, env) else result

  private def evalOr(args: List[SchemeVal], env: Env): SchemeVal =
    args match
      case Nil         => SchemeVal.SBool(false)
      case last :: Nil => eval(last, env)
      case head :: tail =>
        val result = eval(head, env)
        if isTruthy(result) then result else evalOr(tail, env)

  private def evalSet(args: List[SchemeVal], env: Env): SchemeVal =
    args match
      case SchemeVal.SSymbol(name) :: valueExpr :: Nil =>
        env.set(name, eval(valueExpr, env))
        SchemeVal.SVoid
      case _ => throw new EvalError("set!: bad syntax")

  private def evalDefineSyntax(args: List[SchemeVal], env: Env): SchemeVal =
    args match
      case SchemeVal.SSymbol(name) :: SchemeVal.SList(
            SchemeVal.SSymbol("syntax-rules") :: SchemeVal.SList(literals) :: clauses
          ) :: Nil =>
        val litNames = literals.collect { case SchemeVal.SSymbol(n) => n }.toSet
        val parsedClauses = clauses.map {
          case SchemeVal.SList(pattern :: template :: Nil) => (pattern, template)
          case other => throw new EvalError(s"syntax-rules: bad clause ${other.display}")
        }
        env.define(name, SchemeVal.SMacro(litNames, parsedClauses, env))
        SchemeVal.SVoid
      case _ => throw new EvalError("define-syntax: bad syntax")

  private def evalBegin(args: List[SchemeVal], env: Env): SchemeVal =
    evalBody(args, env)

  private def evalCond(clauses: List[SchemeVal], env: Env): SchemeVal =
    clauses match
      case Nil => SchemeVal.SVoid
      case clause :: rest =>
        clause match
          case SchemeVal.SList(SchemeVal.SSymbol("else") :: body) =>
            evalBody(body, env)
          case SchemeVal.SList(test :: body) =>
            val testVal = eval(test, env)
            if isTruthy(testVal) then
              if body.isEmpty then testVal
              else evalBody(body, env)
            else evalCond(rest, env)
          case _ => throw new EvalError("cond: bad clause")

  private def applyProc(op: SchemeVal, args: List[SchemeVal]): SchemeVal =
    op match
      case SchemeVal.SLambda(params, restParam, body, closure) =>
        restParam match
          case None =>
            if params.length != args.length then
              throw new EvalError(
                s"expected ${params.length} arguments, got ${args.length}"
              )
            val callEnv = Env(Some(closure))
            params.zip(args).foreach((p, a) => callEnv.define(p, a))
            evalBody(body, callEnv)
          case Some(rest) =>
            if args.length < params.length then
              throw new EvalError(
                s"expected at least ${params.length} arguments, got ${args.length}"
              )
            val callEnv = Env(Some(closure))
            params.zip(args).foreach((p, a) => callEnv.define(p, a))
            callEnv.define(rest, SchemeVal.SList(args.drop(params.length)))
            evalBody(body, callEnv)
      case SchemeVal.SCaseLambda(clauses, closure) =>
        val matching = clauses.find { case (params, restParam, _) =>
          restParam match
            case None    => args.length == params.length
            case Some(_) => args.length >= params.length
        }
        matching match
          case Some((params, restParam, body)) =>
            val callEnv = Env(Some(closure))
            params.zip(args).foreach((p, a) => callEnv.define(p, a))
            restParam.foreach(rest => callEnv.define(rest, SchemeVal.SList(args.drop(params.length))))
            evalBody(body, callEnv)
          case None =>
            throw new EvalError(s"no matching clause for ${args.length} arguments")
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
