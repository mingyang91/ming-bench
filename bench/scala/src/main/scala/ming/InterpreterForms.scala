package ming

import scala.annotation.tailrec

final private[ming] class InterpreterForms(
  env: Env,
  evalExpr: Expr => Value,
  evalSequenceIn: (List[Expr], Env) => Value,
  applyProcedure: (Value, List[Value], SourcePos) => Value
):

  def evalDefine(args: List[Expr], pos: SourcePos): Value =
    args match
      case Expr.Symbol(name, _) :: valueExpr :: Nil =>
        env.define(name, eval(valueExpr))
        Value.VoidVal

      case Expr.ListExpr(Expr.Symbol(name, _) :: params, signaturePos) :: body if body.nonEmpty =>
        val (fixedParams, restParam) = parseParameters(params, signaturePos)
        env.define(name, Value.Closure(fixedParams, restParam, body, env))
        Value.VoidVal

      case _ =>
        throw EvalError.at(pos, "define expects a name and expression")

  def evalSet(args: List[Expr], pos: SourcePos): Value =
    args match
      case Expr.Symbol(name, _) :: valueExpr :: Nil =>
        env.set(name, eval(valueExpr), pos)
        Value.VoidVal

      case _ =>
        throw EvalError.at(pos, "set! expects a name and expression")

  def evalIf(args: List[Expr], pos: SourcePos): Value =
    args match
      case condition :: thenBranch :: Nil =>
        if Value.isTruthy(eval(condition)) then eval(thenBranch)
        else Value.VoidVal

      case condition :: thenBranch :: elseBranch :: Nil =>
        if Value.isTruthy(eval(condition)) then eval(thenBranch)
        else eval(elseBranch)

      case _ =>
        throw EvalError.at(pos, "if expects 2 or 3 arguments")

  def evalQuote(args: List[Expr], pos: SourcePos): Value =
    args match
      case quoted :: Nil =>
        Value.fromQuotedExpr(quoted)

      case _ =>
        throw EvalError.at(pos, "quote expects exactly 1 argument")

  def evalLambda(args: List[Expr], pos: SourcePos): Value =
    args match
      case Expr.ListExpr(params, paramsPos) :: body if body.nonEmpty =>
        val (fixedParams, restParam) = parseParameters(params, paramsPos)
        Value.Closure(fixedParams, restParam, body, env)

      case _ =>
        throw EvalError.at(pos, "lambda expects a parameter list and body")

  def evalCaseLambda(args: List[Expr], pos: SourcePos): Value =
    if args.isEmpty then throw EvalError.at(pos, "case-lambda expects at least 1 clause")

    val clauses = args.map {
      case Expr.ListExpr(Expr.ListExpr(params, paramsPos) :: body, _) if body.nonEmpty =>
        val (fixedParams, restParam) = parseParameters(params, paramsPos)
        ProcedureClause(fixedParams, restParam, body)

      case Expr.ListExpr(_, clausePos) =>
        throw EvalError.at(clausePos, "case-lambda clause expects a parameter list and body")

      case other =>
        throw EvalError.at(MacroSupport.exprPos(other), "case-lambda clauses must be lists")
    }

    Value.CaseClosure(clauses, env)

  def evalLet(args: List[Expr], pos: SourcePos): Value =
    args match
      case Expr.Symbol(name, _) :: Expr.ListExpr(bindings, bindingsPos) :: body if body.nonEmpty =>
        evalNamedLet(name, bindings, body, bindingsPos, pos)

      case Expr.ListExpr(bindings, bindingsPos) :: body if body.nonEmpty =>
        val parsedBindings = parseBindings(bindings, bindingsPos)
        val names          = parsedBindings.map(_._1)
        val values         = parsedBindings.map { case (_, valueExpr) => eval(valueExpr) }
        evaluateAll(body, env.extend(names, values))

      case _ =>
        throw EvalError.at(pos, "let expects bindings and a body")

  def evalLetrec(args: List[Expr], pos: SourcePos, sequential: Boolean): Value =
    args match
      case Expr.ListExpr(bindings, bindingsPos) :: body if body.nonEmpty =>
        val parsedBindings = parseBindings(bindings, bindingsPos)
        val scope          = Env.child(env)
        val cells = parsedBindings.map { case (name, _) =>
          val cell = Cell(Value.UninitializedVal(name))
          scope.defineAlias(name, cell)
          cell
        }

        if sequential then
          parsedBindings.zip(cells).foreach { case ((_, valueExpr), cell) =>
            cell.set(evalIn(valueExpr, scope))
          }
        else
          val values = parsedBindings.map { case (_, valueExpr) => evalIn(valueExpr, scope) }
          cells.zip(values).foreach { case (cell, value) => cell.set(value) }

        evaluateAll(body, scope)

      case _ =>
        throw EvalError.at(pos, "letrec expects bindings and a body")

  def evalCond(clauses: List[Expr], pos: SourcePos): Value =
    clauses match
      case Nil =>
        Value.VoidVal

      case Expr.ListExpr(Nil, clausePos) :: _ =>
        throw EvalError.at(clausePos, "cond clause cannot be empty")

      case Expr.ListExpr(Expr.Symbol("else", _) :: body, clausePos) :: rest =>
        if rest.nonEmpty then throw EvalError.at(clausePos, "else must be the last cond clause")
        else if body.isEmpty then Value.VoidVal
        else evaluateAll(body)

      case Expr.ListExpr(test :: body, _) :: rest =>
        val testValue = eval(test)
        if Value.isTruthy(testValue) then if body.isEmpty then testValue else evaluateAll(body)
        else evalCond(rest, pos)

      case other :: _ =>
        throw EvalError.at(pos, s"invalid cond clause: ${other}")

  def evalCase(args: List[Expr], pos: SourcePos): Value =
    args match
      case selector :: clauses if clauses.nonEmpty =>
        evalCaseClauses(eval(selector), clauses, pos)

      case _ =>
        throw EvalError.at(pos, "case expects a key and at least 1 clause")

  def evalDo(args: List[Expr], pos: SourcePos): Value =
    args match
      case Expr.ListExpr(bindings, bindingsPos) :: Expr.ListExpr(testExpr :: resultExprs, _) :: body =>
        val parsedBindings = parseDoBindings(bindings, bindingsPos)
        val initialValues  = parsedBindings.map { case (_, initExpr, _) => eval(initExpr) }
        val loopEnv        = Env.child(env)
        val cells = parsedBindings.zip(initialValues).map { case ((name, _, _), value) =>
          val cell = Cell(value)
          loopEnv.defineAlias(name, cell)
          cell
        }

        @tailrec
        def loop(): Value =
          if Value.isTruthy(evalIn(testExpr, loopEnv)) then
            if resultExprs.isEmpty then Value.VoidVal
            else evaluateAll(resultExprs, loopEnv)
          else
            if body.nonEmpty then evaluateAll(body, loopEnv)

            val nextValues = parsedBindings.zip(cells).map {
              case ((_, _, Some(stepExpr)), _) =>
                evalIn(stepExpr, loopEnv)

              case ((_, _, None), cell) =>
                cell.get
            }

            cells.zip(nextValues).foreach { case (cell, value) => cell.set(value) }
            loop()

        loop()

      case Expr.ListExpr(_, _) :: Expr.ListExpr(Nil, testPos) :: _ =>
        throw EvalError.at(testPos, "do test clause must contain a test expression")

      case _ =>
        throw EvalError.at(pos, "do expects bindings, a test clause, and an optional body")

  @tailrec
  final def evalAnd(args: List[Expr], result: Value = Value.BoolVal(true)): Value =
    args match
      case Nil =>
        result

      case _ if !Value.isTruthy(result) =>
        result

      case expr :: rest =>
        evalAnd(rest, eval(expr))

  @tailrec
  final def evalOr(args: List[Expr], result: Value = Value.BoolVal(false)): Value =
    args match
      case Nil =>
        result

      case _ if Value.isTruthy(result) =>
        result

      case expr :: rest =>
        evalOr(rest, eval(expr))

  private def evalNamedLet(
    name: String,
    bindings: List[Expr],
    body: List[Expr],
    bindingsPos: SourcePos,
    pos: SourcePos
  ): Value =
    val parsedBindings = parseBindings(bindings, bindingsPos)
    val names          = parsedBindings.map(_._1)
    val values         = parsedBindings.map { case (_, valueExpr) => eval(valueExpr) }
    val loopEnv        = Env.child(env)
    val closure        = Value.Closure(names, None, body, loopEnv)

    loopEnv.define(name, closure)
    call(closure, values, pos)

  private def evalCaseClauses(key: Value, clauses: List[Expr], pos: SourcePos): Value =
    clauses match
      case Nil =>
        Value.VoidVal

      case Expr.ListExpr(Nil, clausePos) :: _ =>
        throw EvalError.at(clausePos, "case clause cannot be empty")

      case Expr.ListExpr(Expr.Symbol("else", _) :: body, clausePos) :: rest =>
        if rest.nonEmpty then throw EvalError.at(clausePos, "else must be the last case clause")
        else if body.isEmpty then Value.VoidVal
        else evaluateAll(body)

      case Expr.ListExpr(Expr.ListExpr(datums, _) :: body, _) :: rest =>
        if datums.exists(datum => Value.eqv(key, Value.fromQuotedExpr(datum))) then
          if body.isEmpty then Value.VoidVal
          else evaluateAll(body)
        else evalCaseClauses(key, rest, pos)

      case other :: _ =>
        throw EvalError.at(pos, s"invalid case clause: ${other}")

  private def parseBindings(bindings: List[Expr], pos: SourcePos): List[(String, Expr)] =
    bindings.map {
      case Expr.ListExpr(Expr.Symbol(name, _) :: valueExpr :: Nil, _) =>
        (name, valueExpr)

      case Expr.ListExpr(_, bindingPos) =>
        throw EvalError.at(bindingPos, "binding must contain exactly a name and expression")

      case _ =>
        throw EvalError.at(pos, "bindings must be lists")
    }

  private def parseDoBindings(bindings: List[Expr], pos: SourcePos): List[(String, Expr, Option[Expr])] =
    bindings.map {
      case Expr.ListExpr(Expr.Symbol(name, _) :: initExpr :: stepExpr :: Nil, _) =>
        (name, initExpr, Some(stepExpr))

      case Expr.ListExpr(Expr.Symbol(name, _) :: initExpr :: Nil, _) =>
        (name, initExpr, None)

      case Expr.ListExpr(_, bindingPos) =>
        throw EvalError.at(bindingPos, "do binding must contain a name, init expression, and optional step")

      case _ =>
        throw EvalError.at(pos, "do bindings must be lists")
    }

  private def parseParameters(params: List[Expr], pos: SourcePos): (List[String], Option[String]) =
    @tailrec
    def loop(
      remaining: List[Expr],
      acc: List[String]
    ): (List[String], Option[String]) =
      remaining match
        case Nil =>
          (acc.reverse, None)

        case Expr.Symbol(".", dotPos) :: Expr.Symbol(restName, restPos) :: Nil =>
          if restName == "." then throw EvalError.at(restPos, "parameter list contains an invalid rest parameter")
          (acc.reverse, Some(restName))

        case Expr.Symbol(".", dotPos) :: _ =>
          throw EvalError.at(dotPos, "dot must be followed by exactly one rest parameter")

        case Expr.Symbol(name, _) :: tail =>
          if name == "." then throw EvalError.at(pos, "parameter list contains an invalid dot")
          loop(tail, name :: acc)

        case _ =>
          throw EvalError.at(pos, "parameter list must contain only symbols")

    loop(params, Nil)

  private def eval(expr: Expr): Value =
    evalExpr(expr)

  private def evalIn(expr: Expr, scope: Env): Value =
    evalSequenceIn(List(expr), scope)

  private def evaluateAll(expressions: List[Expr], scope: Env = env): Value =
    evalSequenceIn(expressions, scope)

  private def call(proc: Value, args: List[Value], pos: SourcePos): Value =
    applyProcedure(proc, args, pos)
