package ming

object Evaluator:

  import Builtins.{applyBuiltin, display, isFalsy}

  private def eval(expr: Expr, env: Env): Expr =
    try
      expr match
        case Expr.Num(_) | Expr.Bool(_) | Expr.Str(_) | Expr.Chr(_) | Expr.Lambda(_, _, _) =>
          expr
        case Expr.Sym(name) => env.lookup(name)
        case Expr.Lst(Nil)  => throw EvalError("empty application")
        case Expr.Lst(Expr.Sym("quote") :: args) =>
          if args.length != 1 then throw EvalError("quote: need exactly 1 argument")
          args.head
        case Expr.Lst(Expr.Sym("if") :: args)      => evalIf(args, env)
        case Expr.Lst(Expr.Sym("define") :: args)  => evalDefine(args, env)
        case Expr.Lst(Expr.Sym("set!") :: args)    => evalSet(args, env)
        case Expr.Lst(Expr.Sym("lambda") :: args)  => evalLambda(args, env)
        case Expr.Lst(Expr.Sym("let") :: args)     => evalLet(args, env)
        case Expr.Lst(Expr.Sym("begin") :: args)   => evalBegin(args, env)
        case Expr.Lst(Expr.Sym("cond") :: clauses) => evalCond(clauses, env)
        case Expr.Lst(Expr.Sym("and") :: args)     => evalAnd(args, env)
        case Expr.Lst(Expr.Sym("or") :: args)      => evalOr(args, env)
        case Expr.Lst(op :: args) =>
          val func          = eval(op, env)
          val evaluatedArgs = args.map(a => eval(a, env))
          applyProc(func, evaluatedArgs)
    catch
      case e: EvalError if expr.line > 0 && !e.getMessage.matches(".*\\d+:\\d+.*") =>
        throw EvalError(s"${expr.line}:${expr.col}: ${e.getMessage}")

  private def evalIf(args: List[Expr], env: Env): Expr =
    if args.length < 2 || args.length > 3 then throw EvalError("if: need 2 or 3 arguments")
    val cond = eval(args.head, env)
    if !isFalsy(cond) then eval(args(1), env)
    else if args.length == 3 then eval(args(2), env)
    else Expr.Bool(false)

  private def evalDefine(args: List[Expr], env: Env): Expr = args match
    case Expr.Sym(name) :: value :: Nil =>
      env.define(name, eval(value, env))
      Expr.Bool(false)
    case Expr.Lst(Expr.Sym(name) :: params) :: body if body.nonEmpty =>
      val paramNames = extractParams("define", params)
      env.define(name, Expr.Lambda(paramNames, body, env))
      Expr.Bool(false)
    case _ => throw EvalError("define: invalid syntax")

  private def evalSet(args: List[Expr], env: Env): Expr = args match
    case Expr.Sym(name) :: value :: Nil =>
      env.set(name, eval(value, env))
      Expr.Bool(false)
    case _ => throw EvalError("set!: invalid syntax")

  private def evalLambda(args: List[Expr], env: Env): Expr = args match
    case Expr.Lst(params) :: body if body.nonEmpty =>
      Expr.Lambda(extractParams("lambda", params), body, env)
    case _ => throw EvalError("lambda: invalid syntax")

  private def evalLet(args: List[Expr], env: Env): Expr = args match
    case Expr.Sym(name) :: Expr.Lst(bindings) :: body if body.nonEmpty =>
      val letEnv     = env.child()
      val paramNames = List.newBuilder[String]
      val initVals   = List.newBuilder[Expr]
      for b <- bindings do
        b match
          case Expr.Lst(List(Expr.Sym(p), valueExpr)) =>
            paramNames += p
            initVals += eval(valueExpr, env)
          case _ => throw EvalError("let: invalid binding")
      val lambda = Expr.Lambda(paramNames.result(), body, letEnv)
      letEnv.define(name, lambda)
      applyProc(lambda, initVals.result())
    case Expr.Lst(bindings) :: body if body.nonEmpty =>
      val letEnv = env.child()
      for b <- bindings do
        b match
          case Expr.Lst(List(Expr.Sym(name), valueExpr)) =>
            letEnv.define(name, eval(valueExpr, env))
          case _ => throw EvalError("let: invalid binding")
      evalBody(body, letEnv)
    case _ => throw EvalError("let: invalid syntax")

  private def evalBegin(args: List[Expr], env: Env): Expr =
    if args.isEmpty then throw EvalError("begin: need at least 1 expression")
    evalBody(args, env)

  private def evalAnd(args: List[Expr], env: Env): Expr = args match
    case Nil         => Expr.Bool(true)
    case last :: Nil => eval(last, env)
    case head :: tail =>
      val v = eval(head, env)
      if isFalsy(v) then v else evalAnd(tail, env)

  private def evalOr(args: List[Expr], env: Env): Expr = args match
    case Nil         => Expr.Bool(false)
    case last :: Nil => eval(last, env)
    case head :: tail =>
      val v = eval(head, env)
      if !isFalsy(v) then v else evalOr(tail, env)

  private def evalCond(clauses: List[Expr], env: Env): Expr = clauses match
    case Nil                                     => Expr.Bool(false)
    case Expr.Lst(Expr.Sym("else") :: body) :: _ => evalBody(body, env)
    case Expr.Lst(test :: body) :: rest =>
      val v = eval(test, env)
      if !isFalsy(v) then if body.isEmpty then v else evalBody(body, env)
      else evalCond(rest, env)
    case _ => throw EvalError("cond: invalid clause")

  private def evalBody(exprs: List[Expr], env: Env): Expr =
    exprs.foldLeft(Expr.Bool(false): Expr)((_, e) => eval(e, env))

  private def extractParams(context: String, params: List[Expr]): List[String] =
    params.map {
      case Expr.Sym(p) => p
      case other       => throw EvalError(s"$context: invalid parameter: ${display(other)}")
    }

  private def applyProc(func: Expr, args: List[Expr]): Expr = func match
    case Expr.Sym(name) => applyBuiltin(name, args)
    case Expr.Lambda(params, body, closure) =>
      if params.length != args.length then
        throw EvalError(
          s"lambda: expected ${params.length} arguments, got ${args.length}"
        )
      val localEnv = closure.child()
      params.zip(args).foreach((p, a) => localEnv.define(p, a))
      evalBody(body, localEnv)
    case _ => throw EvalError(s"not a procedure: ${display(func)}")

  def evalStr(input: String): String =
    val parser = SchemeParser(input)
    val exprs  = parser.parseAll()
    if exprs.isEmpty then throw EvalError("no expressions")
    val env = Builtins.makeTopLevelEnv()
    display(exprs.foldLeft(Expr.Bool(false): Expr)((_, e) => eval(e, env)))

  def evalStrWithOutput(input: String): (String, String) =
    val parser = SchemeParser(input)
    val exprs  = parser.parseAll()
    if exprs.isEmpty then throw EvalError("no expressions")
    val env = Builtins.makeTopLevelEnv()
    val buf = new StringBuilder
    Builtins.outputBuffer.set(buf)
    try
      val result = display(exprs.foldLeft(Expr.Bool(false): Expr)((_, e) => eval(e, env)))
      (result, buf.toString)
    finally Builtins.outputBuffer.remove()
