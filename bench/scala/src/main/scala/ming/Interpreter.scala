package ming

import scala.annotation.tailrec

private[ming] object Interpreter:

  def evaluate(input: String): (Value, String) =
    val expressions = SchemeParser.parseProgram(input)
    if expressions.isEmpty then throw new EvalError("1:1: expected expression")

    val runtime = RuntimeContext()
    val env     = Env.root(Builtins.globalEnv(runtime))
    val result  = evalSequence(expressions, env)
    (result, runtime.capturedOutput)

  private def eval(expr: Expr, env: Env): Value =
    expr match
      case Expr.IntAtom(value, _) =>
        Value.IntVal(value)

      case Expr.BoolAtom(value, _) =>
        Value.BoolVal(value)

      case Expr.StringAtom(value, _) =>
        Value.StringVal(value)

      case Expr.Symbol(name, pos) =>
        env.lookup(name, pos)

      case Expr.ListExpr(Nil, pos) =>
        throw EvalError.at(pos, "cannot evaluate an empty list")

      case Expr.ListExpr(Expr.Symbol("define", _) :: args, pos) =>
        evalDefine(args, env, pos)

      case Expr.ListExpr(Expr.Symbol("if", _) :: args, pos) =>
        evalIf(args, env, pos)

      case Expr.ListExpr(Expr.Symbol("quote", _) :: args, pos) =>
        evalQuote(args, pos)

      case Expr.ListExpr(Expr.Symbol("lambda", _) :: args, pos) =>
        evalLambda(args, env, pos)

      case Expr.ListExpr(Expr.Symbol("begin", _) :: args, _) =>
        evalBegin(args, env)

      case Expr.ListExpr(Expr.Symbol("let", _) :: args, pos) =>
        evalLet(args, env, pos)

      case Expr.ListExpr(Expr.Symbol("cond", _) :: args, pos) =>
        evalCond(args, env, pos)

      case Expr.ListExpr(Expr.Symbol("and", _) :: args, _) =>
        evalAnd(args, env)

      case Expr.ListExpr(Expr.Symbol("or", _) :: args, _) =>
        evalOr(args, env)

      case Expr.ListExpr(head :: args, pos) =>
        applyProcedure(eval(head, env), args.map(arg => eval(arg, env)), pos)

  private def evalSequence(expressions: List[Expr], env: Env): Value =
    expressions.foldLeft[Value](Value.VoidVal) { (_, expr) =>
      eval(expr, env)
    }

  private def evalDefine(args: List[Expr], env: Env, pos: SourcePos): Value =
    args match
      case Expr.Symbol(name, _) :: valueExpr :: Nil =>
        env.define(name, eval(valueExpr, env))
        Value.VoidVal

      case Expr.ListExpr(Expr.Symbol(name, _) :: params, signaturePos) :: body if body.nonEmpty =>
        env.define(name, Value.Closure(parseParameters(params, signaturePos), body, env))
        Value.VoidVal

      case _ =>
        throw EvalError.at(pos, "define expects a name and expression")

  private def evalIf(args: List[Expr], env: Env, pos: SourcePos): Value =
    args match
      case condition :: thenBranch :: elseBranch :: Nil =>
        if Value.isTruthy(eval(condition, env)) then eval(thenBranch, env)
        else eval(elseBranch, env)

      case _ =>
        throw EvalError.at(pos, "if expects exactly 3 arguments")

  private def evalQuote(args: List[Expr], pos: SourcePos): Value =
    args match
      case quoted :: Nil =>
        Value.fromQuotedExpr(quoted)

      case _ =>
        throw EvalError.at(pos, "quote expects exactly 1 argument")

  private def evalLambda(args: List[Expr], env: Env, pos: SourcePos): Value =
    args match
      case Expr.ListExpr(params, paramsPos) :: body if body.nonEmpty =>
        Value.Closure(parseParameters(params, paramsPos), body, env)

      case _ =>
        throw EvalError.at(pos, "lambda expects a parameter list and body")

  private def evalBegin(args: List[Expr], env: Env): Value =
    evalSequence(args, env)

  private def evalLet(args: List[Expr], env: Env, pos: SourcePos): Value =
    args match
      case Expr.Symbol(name, _) :: Expr.ListExpr(bindings, bindingsPos) :: body if body.nonEmpty =>
        evalNamedLet(name, bindings, body, env, bindingsPos, pos)

      case Expr.ListExpr(bindings, bindingsPos) :: body if body.nonEmpty =>
        val parsedBindings = parseBindings(bindings, bindingsPos)
        val names          = parsedBindings.map(_._1)
        val values         = parsedBindings.map { case (_, valueExpr) => eval(valueExpr, env) }
        evalSequence(body, env.extend(names, values))

      case _ =>
        throw EvalError.at(pos, "let expects bindings and a body")

  private def evalNamedLet(
    name: String,
    bindings: List[Expr],
    body: List[Expr],
    env: Env,
    bindingsPos: SourcePos,
    pos: SourcePos
  ): Value =
    val parsedBindings = parseBindings(bindings, bindingsPos)
    val names          = parsedBindings.map(_._1)
    val values         = parsedBindings.map { case (_, valueExpr) => eval(valueExpr, env) }
    val loopEnv        = Env.child(env)
    val closure        = Value.Closure(names, body, loopEnv)

    loopEnv.define(name, closure)
    applyProcedure(closure, values, pos)

  private def evalCond(clauses: List[Expr], env: Env, pos: SourcePos): Value =
    clauses match
      case Nil =>
        Value.VoidVal

      case Expr.ListExpr(Nil, clausePos) :: _ =>
        throw EvalError.at(clausePos, "cond clause cannot be empty")

      case Expr.ListExpr(Expr.Symbol("else", _) :: body, clausePos) :: rest =>
        if rest.nonEmpty then throw EvalError.at(clausePos, "else must be the last cond clause")
        else if body.isEmpty then Value.VoidVal
        else evalSequence(body, env)

      case Expr.ListExpr(test :: body, _) :: rest =>
        val testValue = eval(test, env)
        if Value.isTruthy(testValue) then if body.isEmpty then testValue else evalSequence(body, env)
        else evalCond(rest, env, pos)

      case other :: _ =>
        throw EvalError.at(pos, s"invalid cond clause: ${other}")

  private def parseBindings(bindings: List[Expr], pos: SourcePos): List[(String, Expr)] =
    bindings.map {
      case Expr.ListExpr(Expr.Symbol(name, _) :: valueExpr :: Nil, _) =>
        (name, valueExpr)

      case Expr.ListExpr(_, bindingPos) =>
        throw EvalError.at(bindingPos, "binding must contain exactly a name and expression")

      case _ =>
        throw EvalError.at(pos, "bindings must be lists")
    }

  private def parseParameters(params: List[Expr], pos: SourcePos): List[String] =
    params.map {
      case Expr.Symbol(name, _) =>
        name

      case _ =>
        throw EvalError.at(pos, "parameter list must contain only symbols")
    }

  @tailrec
  private def evalAnd(
    args: List[Expr],
    env: Env,
    result: Value = Value.BoolVal(true)
  ): Value =
    args match
      case Nil =>
        result

      case _ if !Value.isTruthy(result) =>
        result

      case expr :: rest =>
        evalAnd(rest, env, eval(expr, env))

  @tailrec
  private def evalOr(
    args: List[Expr],
    env: Env,
    result: Value = Value.BoolVal(false)
  ): Value =
    args match
      case Nil =>
        result

      case _ if Value.isTruthy(result) =>
        result

      case expr :: rest =>
        evalOr(rest, env, eval(expr, env))

  private def applyProcedure(proc: Value, args: List[Value], pos: SourcePos): Value =
    proc match
      case Value.Builtin(_, fn) =>
        fn(args, pos)

      case Value.Closure(params, body, closureEnv) =>
        if params.length != args.length then
          throw EvalError.at(pos, s"expected ${params.length} arguments, got ${args.length}")

        evalSequence(body, closureEnv.extend(params, args))

      case other =>
        throw EvalError.at(pos, s"attempted to call a ${other.typeName} value")
