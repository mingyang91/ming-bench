package ming

/** Scheme interpreter entry point. Agents implement this object. */
object Evaluator:

  /** Evaluate one or more Scheme expressions and return the string representation of the last result.
    */
  def evalStr(input: String): String =
    val expressions = SchemeParser.parseProgram(input)
    if expressions.isEmpty then throw EvalError("empty input")

    val globalEnv = Env.topLevel()
    val result = expressions.foldLeft[Value](Value.Void) { (_, expr) =>
      eval(expr, globalEnv)
    }
    SchemeRenderer.render(result)

  /** Evaluate Scheme expressions and return both the result string and any captured output from display/write/newline.
    */
  def evalStrWithOutput(input: String): (String, String) =
    (evalStr(input), "")

  private def eval(expr: Expr, env: Env): Value =
    expr match
      case Expr.IntLit(value)    => Value.IntVal(value)
      case Expr.BoolLit(value)   => Value.BoolVal(value)
      case Expr.StringLit(value) => Value.StringVal(value)
      case Expr.Symbol(name)     => env.lookup(name).getOrElse(throw EvalError(s"unbound variable: $name"))
      case Expr.ListExpr(items)  => evalList(items, env)

  private def evalList(items: List[Expr], env: Env): Value =
    items match
      case Nil => throw EvalError("cannot evaluate empty list")
      case Expr.Symbol("define") :: args =>
        evalDefine(args, env)
      case Expr.Symbol("if") :: args =>
        evalIf(args, env)
      case Expr.Symbol("quote") :: args =>
        evalQuote(args)
      case Expr.Symbol("lambda") :: args =>
        evalLambda(args, env)
      case Expr.Symbol("and") :: args =>
        evalAnd(args, env, Value.BoolVal(true))
      case Expr.Symbol("or") :: args =>
        evalOr(args, env)
      case head :: args =>
        applyProcedure(eval(head, env), args, env)

  private def evalDefine(args: List[Expr], env: Env): Value =
    args match
      case Expr.Symbol(name) :: valueExpr :: Nil =>
        env.define(name, eval(valueExpr, env))
        Value.Void
      case Expr.ListExpr(Expr.Symbol(name) :: params) :: body if body.nonEmpty =>
        val closure =
          Value.Closure(
            name = Some(name),
            params = parseParameterNames(params),
            body = body,
            env = env
          )
        env.define(name, closure)
        Value.Void
      case _ =>
        throw EvalError("invalid define")

  private def evalIf(args: List[Expr], env: Env): Value =
    args match
      case conditionExpr :: thenExpr :: elseExpr :: Nil =>
        if ValueSemantics.isTruthy(eval(conditionExpr, env)) then eval(thenExpr, env)
        else eval(elseExpr, env)
      case _ =>
        throw EvalError("if expects exactly 3 arguments")

  private def evalQuote(args: List[Expr]): Value =
    args match
      case expr :: Nil => ValueSemantics.quote(expr)
      case _           => throw EvalError("quote expects exactly 1 argument")

  private def evalLambda(args: List[Expr], env: Env): Value =
    args match
      case paramsExpr :: body if body.nonEmpty =>
        Value.Closure(name = None, params = parseParameterNames(paramsExpr), body = body, env = env)
      case _ =>
        throw EvalError("lambda expects parameters and at least one body expression")

  private def parseParameterNames(expr: Expr): List[String] =
    expr match
      case Expr.ListExpr(params) => parseParameterNames(params)
      case _                     => throw EvalError("parameter list must be a list")

  private def parseParameterNames(params: List[Expr]): List[String] =
    params.map {
      case Expr.Symbol(name) => name
      case _                 => throw EvalError("parameter names must be symbols")
    }

  private def applyProcedure(procedure: Value, args: List[Expr], env: Env): Value =
    val evaluatedArgs = args.map(eval(_, env))
    procedure match
      case Value.BuiltinProc(name) =>
        Builtins.invoke(name, evaluatedArgs)
      case Value.Closure(name, params, body, closureEnv) =>
        if evaluatedArgs.lengthCompare(params.length) != 0 then
          val procName = name.getOrElse("lambda")
          throw EvalError(s"$procName expects ${params.length} argument(s), got ${evaluatedArgs.length}")

        val callEnv = closureEnv.child()
        params.zip(evaluatedArgs).foreach { case (param, value) =>
          callEnv.define(param, value)
        }
        evalSequence(body, callEnv)
      case _ =>
        throw EvalError("attempted to call a non-procedure")

  private def evalSequence(exprs: List[Expr], env: Env): Value =
    exprs.foldLeft[Value](Value.Void) { (_, expr) =>
      eval(expr, env)
    }

  @annotation.tailrec
  private def evalAnd(args: List[Expr], env: Env, lastValue: Value): Value =
    args match
      case Nil => lastValue
      case head :: tail =>
        val value = eval(head, env)
        if ValueSemantics.isTruthy(value) then evalAnd(tail, env, value)
        else value

  @annotation.tailrec
  private def evalOr(args: List[Expr], env: Env): Value =
    args match
      case Nil => Value.BoolVal(false)
      case head :: tail =>
        val value = eval(head, env)
        if ValueSemantics.isTruthy(value) then value
        else evalOr(tail, env)
