package ming

object Evaluator:

  import Builtins.{applyBuiltin, display, isFalsy}

  private def eval(expr: Expr, env: Env): Expr =
    try
      expr match
        case Expr.Num(_) | Expr.Rational(_, _) | Expr.Real(_) | Expr.Bool(_) | Expr.Str(_) | Expr.Chr(_) |
            Expr.Lambda(_, _, _, _) | Expr.Pair(_, _) | Expr.Macro(_, _, _) =>
          expr
        case Expr.Sym(name) => env.lookup(name)
        case Expr.Lst(Nil)  => throw EvalError("empty application")
        case Expr.Lst(Expr.Sym("quote") :: args) =>
          if args.length != 1 then throw EvalError("quote: need exactly 1 argument")
          args.head
        case Expr.Lst(Expr.Sym("if") :: args)            => evalIf(args, env)
        case Expr.Lst(Expr.Sym("define") :: args)        => evalDefine(args, env)
        case Expr.Lst(Expr.Sym("set!") :: args)          => evalSet(args, env)
        case Expr.Lst(Expr.Sym("lambda") :: args)        => evalLambda(args, env)
        case Expr.Lst(Expr.Sym("let") :: args)           => evalLet(args, env)
        case Expr.Lst(Expr.Sym("begin") :: args)         => evalBegin(args, env)
        case Expr.Lst(Expr.Sym("cond") :: clauses)       => evalCond(clauses, env)
        case Expr.Lst(Expr.Sym("and") :: args)           => evalAnd(args, env)
        case Expr.Lst(Expr.Sym("or") :: args)            => evalOr(args, env)
        case Expr.Lst(Expr.Sym("define-syntax") :: args) => evalDefineSyntax(args, env)
        case Expr.Lst((head @ Expr.Sym(name)) :: _) if isMacro(name, env) =>
          val mac = env.lookup(name).asInstanceOf[Expr.Macro]
          val (expanded, hygieneEnv) =
            Macros.expandMacro(mac.literals, mac.rules, expr.asInstanceOf[Expr.Lst].elems, mac.defEnv, env)
          eval(expanded, hygieneEnv)
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
      val (paramNames, restParam) = extractParamsWithRest("define", params)
      env.define(name, Expr.Lambda(paramNames, restParam, body, env))
      Expr.Bool(false)
    case _ => throw EvalError("define: invalid syntax")

  private def evalSet(args: List[Expr], env: Env): Expr = args match
    case Expr.Sym(name) :: value :: Nil =>
      env.set(name, eval(value, env))
      Expr.Bool(false)
    case _ => throw EvalError("set!: invalid syntax")

  private def evalLambda(args: List[Expr], env: Env): Expr = args match
    case Expr.Lst(params) :: body if body.nonEmpty =>
      val (paramNames, restParam) = extractParamsWithRest("lambda", params)
      Expr.Lambda(paramNames, restParam, body, env)
    case Expr.Sym(restName) :: body if body.nonEmpty =>
      Expr.Lambda(Nil, Some(restName), body, env)
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
      val lambda = Expr.Lambda(paramNames.result(), None, body, letEnv)
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

  private def isMacro(name: String, env: Env): Boolean =
    env.lookupOpt(name).exists(_.isInstanceOf[Expr.Macro])

  private def evalDefineSyntax(args: List[Expr], env: Env): Expr = args match
    case Expr.Sym(name) :: Expr.Lst(Expr.Sym("syntax-rules") :: Expr.Lst(literals) :: rules) :: Nil =>
      val litNames = literals.map {
        case Expr.Sym(s) => s
        case _           => throw EvalError("define-syntax: literals must be symbols")
      }
      val rulesList = rules.map {
        case Expr.Lst(List(Expr.Lst(_ :: patternArgs), template)) =>
          (patternArgs, template)
        case _ => throw EvalError("define-syntax: invalid rule")
      }
      env.define(name, Expr.Macro(litNames, rulesList, env))
      Expr.Bool(false)
    case _ => throw EvalError("define-syntax: invalid syntax")

  private def evalBody(exprs: List[Expr], env: Env): Expr =
    exprs.foldLeft(Expr.Bool(false): Expr)((_, e) => eval(e, env))

  private def extractParamsWithRest(context: String, params: List[Expr]): (List[String], Option[String]) =
    val dotIdx = params.indexWhere(_ == Expr.Sym("."))
    if dotIdx >= 0 then
      if dotIdx != params.length - 2 then throw EvalError(s"$context: invalid dot syntax")
      val fixed = params.take(dotIdx).map {
        case Expr.Sym(p) => p
        case other       => throw EvalError(s"$context: invalid parameter: ${display(other)}")
      }
      val rest = params.last match
        case Expr.Sym(p) => p
        case other       => throw EvalError(s"$context: invalid rest parameter: ${display(other)}")
      (fixed, Some(rest))
    else
      (
        params.map {
          case Expr.Sym(p) => p
          case other       => throw EvalError(s"$context: invalid parameter: ${display(other)}")
        },
        None
      )

  private def applyProc(func: Expr, args: List[Expr]): Expr = func match
    case Expr.Sym(name) if name == "apply" => applyApply(args)
    case Expr.Sym(name) if name == "map"   => applyMap(Expr.Sym("map"), args)
    case Expr.Sym(name)                    => applyBuiltin(name, args)
    case Expr.Lambda(params, restParam, body, closure) =>
      restParam match
        case None =>
          if params.length != args.length then
            throw EvalError(s"lambda: expected ${params.length} arguments, got ${args.length}")
          val localEnv = closure.child()
          params.zip(args).foreach((p, a) => localEnv.define(p, a))
          evalBody(body, localEnv)
        case Some(rest) =>
          if args.length < params.length then
            throw EvalError(s"lambda: expected at least ${params.length} arguments, got ${args.length}")
          val localEnv = closure.child()
          params.zip(args).foreach((p, a) => localEnv.define(p, a))
          localEnv.define(rest, Expr.Lst(args.drop(params.length)))
          evalBody(body, localEnv)
    case _ => throw EvalError(s"not a procedure: ${display(func)}")

  private def applyMap(fn: Expr, args: List[Expr]): Expr =
    if args.length < 2 then throw EvalError("map: need at least 2 arguments")
    val proc = args.head
    val lists = args.tail.map {
      case Expr.Lst(elems) => elems
      case other           => throw EvalError(s"map: not a list: ${display(other)}")
    }
    val len = lists.head.length
    if !lists.forall(_.length == len) then throw EvalError("map: lists must have equal length")
    val result = (0 until len).toList.map { i =>
      val argSlice = lists.map(_(i))
      applyProc(proc, argSlice)
    }
    Expr.Lst(result)

  private def applyApply(args: List[Expr]): Expr =
    if args.length < 2 then throw EvalError("apply: need at least 2 arguments")
    val func = args.head
    val lastArg = args.last match
      case Expr.Lst(elems) => elems
      case other           => throw EvalError(s"apply: last argument must be a list")
    val prefixArgs = args.slice(1, args.length - 1)
    applyProc(func, prefixArgs ++ lastArg)

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
