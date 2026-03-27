package ming

/** Scheme interpreter entry point. Agents implement this object. */
object Evaluator:

  /** Evaluate one or more Scheme expressions and return the string representation of the last result.
    */
  def evalStr(input: String): String =
    val expressions = SchemeParser.parseProgram(input)
    if expressions.isEmpty then throw EvalError.at(SourcePos(1, 1), "empty input")

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
      case Expr.IntLit(value, _)    => Value.IntVal(value)
      case Expr.BoolLit(value, _)   => Value.BoolVal(value)
      case Expr.StringLit(value, _) => Value.StringVal(value)
      case Expr.Symbol(name, pos) =>
        env.lookup(name).getOrElse(throw EvalError.at(pos, s"unbound variable: $name"))
      case Expr.ListExpr(items, pos) =>
        evalList(items, env, pos)

  private def evalList(items: List[Expr], env: Env, pos: SourcePos): Value =
    items match
      case Nil => throw EvalError.at(pos, "cannot evaluate empty list")
      case Expr.Symbol("define", formPos) :: args =>
        evalDefine(args, env, formPos)
      case Expr.Symbol("if", formPos) :: args =>
        evalIf(args, env, formPos)
      case Expr.Symbol("quote", formPos) :: args =>
        evalQuote(args, formPos)
      case Expr.Symbol("lambda", formPos) :: args =>
        evalLambda(args, env, formPos)
      case Expr.Symbol("begin", _) :: args =>
        evalBegin(args, env)
      case Expr.Symbol("let", formPos) :: args =>
        evalLet(args, env, formPos)
      case Expr.Symbol("cond", formPos) :: args =>
        evalCond(args, env, formPos)
      case Expr.Symbol("and", _) :: args =>
        evalAnd(args, env, Value.BoolVal(true))
      case Expr.Symbol("or", _) :: args =>
        evalOr(args, env)
      case head :: args =>
        applyProcedure(eval(head, env), args, env, head.pos)

  private def evalDefine(args: List[Expr], env: Env, pos: SourcePos): Value =
    args match
      case Expr.Symbol(name, _) :: valueExpr :: Nil =>
        env.define(name, eval(valueExpr, env))
        Value.Void
      case Expr.ListExpr(Expr.Symbol(name, _) :: params, _) :: body if body.nonEmpty =>
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
        throw EvalError.at(pos, "invalid define")

  private def evalIf(args: List[Expr], env: Env, pos: SourcePos): Value =
    args match
      case conditionExpr :: thenExpr :: elseExpr :: Nil =>
        if ValueSemantics.isTruthy(eval(conditionExpr, env)) then eval(thenExpr, env)
        else eval(elseExpr, env)
      case _ =>
        throw EvalError.at(pos, "if expects exactly 3 arguments")

  private def evalQuote(args: List[Expr], pos: SourcePos): Value =
    args match
      case expr :: Nil => ValueSemantics.quote(expr)
      case _           => throw EvalError.at(pos, "quote expects exactly 1 argument")

  private def evalLambda(args: List[Expr], env: Env, pos: SourcePos): Value =
    args match
      case paramsExpr :: body if body.nonEmpty =>
        Value.Closure(name = None, params = parseParameterNames(paramsExpr), body = body, env = env)
      case _ =>
        throw EvalError.at(pos, "lambda expects parameters and at least one body expression")

  private def evalBegin(args: List[Expr], env: Env): Value =
    evalSequence(args, env)

  private def evalLet(args: List[Expr], env: Env, pos: SourcePos): Value =
    args match
      case Expr.ListExpr(bindings, _) :: body if body.nonEmpty =>
        val letEnv = env.child()
        bindLetValues(letEnv, parseLetBindings(bindings), env)
        evalSequence(body, letEnv)
      case Expr.Symbol(name, _) :: Expr.ListExpr(bindings, _) :: body if body.nonEmpty =>
        val parsedBindings = parseLetBindings(bindings)
        val evaluatedArgs  = parsedBindings.map { case (_, valueExpr) => eval(valueExpr, env) }
        val letEnv         = env.child()
        val closure =
          Value.Closure(
            name = Some(name),
            params = parsedBindings.map(_._1),
            body = body,
            env = letEnv
          )
        letEnv.define(name, closure)
        invokeProcedure(closure, evaluatedArgs, pos)
      case _ =>
        throw EvalError.at(pos, "invalid let")

  private def bindLetValues(targetEnv: Env, bindings: List[(String, Expr)], evalEnv: Env): Unit =
    bindings.foreach { case (name, valueExpr) =>
      targetEnv.define(name, eval(valueExpr, evalEnv))
    }

  private def parseLetBindings(bindings: List[Expr]): List[(String, Expr)] =
    bindings.map {
      case Expr.ListExpr(Expr.Symbol(name, _) :: valueExpr :: Nil, _) =>
        (name, valueExpr)
      case invalid =>
        throw EvalError.at(invalid.pos, "invalid let binding")
    }

  private def evalCond(clauses: List[Expr], env: Env, pos: SourcePos): Value =
    clauses match
      case Nil =>
        Value.Void
      case Expr.ListExpr(Expr.Symbol("else", _) :: body, clausePos) :: remaining =>
        if remaining.nonEmpty then throw EvalError.at(clausePos, "else clause must be last")
        if body.isEmpty then throw EvalError.at(clausePos, "else clause must have a body")
        evalSequence(body, env)
      case Expr.ListExpr(testExpr :: Nil, _) :: remaining =>
        val testValue = eval(testExpr, env)
        if ValueSemantics.isTruthy(testValue) then testValue
        else evalCond(remaining, env, pos)
      case Expr.ListExpr(testExpr :: body, _) :: remaining =>
        val testValue = eval(testExpr, env)
        if ValueSemantics.isTruthy(testValue) then evalSequence(body, env)
        else evalCond(remaining, env, pos)
      case invalid :: _ =>
        throw EvalError.at(invalid.pos, "invalid cond clause")

  private def parseParameterNames(expr: Expr): List[String] =
    expr match
      case Expr.ListExpr(params, _) => parseParameterNames(params)
      case other                    => throw EvalError.at(other.pos, "parameter list must be a list")

  private def parseParameterNames(params: List[Expr]): List[String] =
    params.map {
      case Expr.Symbol(name, _) => name
      case other                => throw EvalError.at(other.pos, "parameter names must be symbols")
    }

  private def applyProcedure(procedure: Value, args: List[Expr], env: Env, pos: SourcePos): Value =
    val evaluatedArgs = args.map(eval(_, env))
    invokeProcedure(procedure, evaluatedArgs, pos)

  private def invokeProcedure(procedure: Value, evaluatedArgs: List[Value], pos: SourcePos): Value =
    procedure match
      case Value.BuiltinProc(name) =>
        Builtins.invoke(name, evaluatedArgs, pos)
      case Value.Closure(name, params, body, closureEnv) =>
        if evaluatedArgs.lengthCompare(params.length) != 0 then
          val procName = name.getOrElse("lambda")
          throw EvalError.at(pos, s"$procName expects ${params.length} argument(s), got ${evaluatedArgs.length}")

        val callEnv = closureEnv.child()
        params.zip(evaluatedArgs).foreach { case (param, value) =>
          callEnv.define(param, value)
        }
        evalSequence(body, callEnv)
      case _ =>
        throw EvalError.at(pos, "attempted to call a non-procedure")

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
