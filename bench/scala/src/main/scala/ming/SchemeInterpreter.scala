package ming

import scala.annotation.tailrec

object SchemeInterpreter:

  def evaluateProgram(input: String): Value =
    evaluateSequence(SchemeReader.readAll(input), Env.empty)._2

  private def evaluateSequence(expressions: List[Expr], env: Env): (Env, Value) = expressions match
    case Nil =>
      (env, Value.Void)
    case head :: tail =>
      val (nextEnv, value) = evaluateSequenceStep(head, env)
      evaluateSequenceTail(tail, nextEnv, value)

  @tailrec
  private def evaluateSequenceTail(
    remaining: List[Expr],
    env: Env,
    current: Value
  ): (Env, Value) =
    remaining match
      case Nil =>
        (env, current)
      case head :: tail =>
        val (nextEnv, value) = evaluateSequenceStep(head, env)
        evaluateSequenceTail(tail, nextEnv, value)

  private def evaluateSequenceStep(expr: Expr, env: Env): (Env, Value) = expr match
    case Expr.ListExpr(Expr.Symbol("define", position) :: arguments, _) =>
      (evaluateDefine(arguments, env, position), Value.Void)
    case Expr.ListExpr(Expr.Symbol("begin", _) :: arguments, _) =>
      evaluateSequence(arguments, env)
    case _ =>
      (env, evaluate(expr, env))

  private def evaluate(expr: Expr, env: Env): Value = expr match
    case Expr.Literal(value, _) =>
      value
    case Expr.Symbol(name, position) =>
      lookup(name, env).getOrElse(throw EvalError.at(position, s"unbound symbol '$name'"))
    case Expr.ListExpr(Nil, position) =>
      throw EvalError.at(position, "cannot evaluate empty list")
    case Expr.ListExpr(operator :: arguments, _) =>
      operator match
        case Expr.Symbol("if", position) =>
          evaluateIf(arguments, env, position)
        case Expr.Symbol("quote", position) =>
          evaluateQuote(arguments, position)
        case Expr.Symbol("lambda", position) =>
          evaluateLambda(arguments, env, position)
        case Expr.Symbol("and", _) =>
          evaluateAnd(arguments, env)
        case Expr.Symbol("or", _) =>
          evaluateOr(arguments, env)
        case Expr.Symbol("begin", _) =>
          evaluateBegin(arguments, env)
        case Expr.Symbol("let", position) =>
          evaluateLet(arguments, env, position)
        case Expr.Symbol("cond", _) =>
          evaluateCond(arguments, env)
        case Expr.Symbol("define", position) =>
          throw EvalError.at(position, "define is only allowed within a sequence")
        case _ =>
          applyProcedure(evaluate(operator, env), arguments, env, operator.sourcePos)

  private def evaluateDefine(arguments: List[Expr], env: Env, position: SourcePos): Env =
    arguments match
      case Expr.Symbol(name, _) :: valueExpr :: Nil =>
        bindRecursive(env, name)(recursiveEnv => evaluate(valueExpr, recursiveEnv))
      case Expr.ListExpr(Expr.Symbol(name, _) :: parameters, _) :: body if body.nonEmpty =>
        val parameterNames = parameters.map(expectParameterName)
        bindRecursive(env, name)(recursiveEnv => Value.Closure(parameterNames, body, recursiveEnv))
      case _ =>
        throw EvalError.at(position, "invalid define form")

  private def bindRecursive(env: Env, name: String)(build: Env => Value): Env =
    lazy val recursiveEnv: Env = env.define(name, build(recursiveEnv))
    recursiveEnv

  private def evaluateBegin(arguments: List[Expr], env: Env): Value =
    evaluateSequence(arguments, env)._2

  private def evaluateIf(arguments: List[Expr], env: Env, position: SourcePos): Value =
    arguments match
      case condition :: thenBranch :: elseBranch :: Nil =>
        if evaluate(condition, env).isTruthy then evaluate(thenBranch, env)
        else evaluate(elseBranch, env)
      case _ =>
        throw EvalError.at(position, "'if' expects exactly 3 arguments")

  private def evaluateQuote(arguments: List[Expr], position: SourcePos): Value =
    arguments match
      case datum :: Nil =>
        quoteToValue(datum)
      case _ =>
        throw EvalError.at(position, "'quote' expects exactly 1 argument")

  private def evaluateLambda(arguments: List[Expr], env: Env, position: SourcePos): Value =
    arguments match
      case parameterExpr :: body if body.nonEmpty =>
        Value.Closure(readParameters(parameterExpr, position), body, env)
      case _ =>
        throw EvalError.at(position, "invalid lambda form")

  private def evaluateLet(arguments: List[Expr], env: Env, position: SourcePos): Value =
    arguments match
      case bindingsExpr :: body if body.nonEmpty =>
        val bindings = readLetBindings(bindingsExpr, env, position)
        evaluateBody(body, env.extend(bindings))
      case _ =>
        throw EvalError.at(position, "invalid let form")

  private def readLetBindings(
    bindingsExpr: Expr,
    env: Env,
    position: SourcePos
  ): List[(String, Value)] = bindingsExpr match
    case Expr.ListExpr(bindings, _) =>
      bindings.map(readLetBinding(_, env))
    case _ =>
      throw EvalError.at(position, "let bindings must be a list")

  private def readLetBinding(binding: Expr, env: Env): (String, Value) = binding match
    case Expr.ListExpr(Expr.Symbol(name, _) :: valueExpr :: Nil, _) =>
      (name, evaluate(valueExpr, env))
    case _ =>
      throw EvalError.at(binding.sourcePos, "invalid let binding")

  private def evaluateCond(arguments: List[Expr], env: Env): Value = arguments match
    case Nil =>
      Value.Void
    case clause :: remaining =>
      evaluateCondClause(clause, remaining, env)

  private def evaluateCondClause(clause: Expr, remaining: List[Expr], env: Env): Value = clause match
    case Expr.ListExpr(Nil, clausePosition) =>
      throw EvalError.at(clausePosition, "cond clause cannot be empty")
    case Expr.ListExpr(Expr.Symbol("else", clausePosition) :: body, _) =>
      if remaining.nonEmpty then throw EvalError.at(clausePosition, "else clause must be last")
      else if body.isEmpty then throw EvalError.at(clausePosition, "else clause must not be empty")
      else evaluateBody(body, env)
    case Expr.ListExpr(test :: body, _) =>
      val conditionValue = evaluate(test, env)
      if conditionValue.isTruthy then if body.isEmpty then conditionValue else evaluateBody(body, env)
      else evaluateCond(remaining, env)
    case _ =>
      throw EvalError.at(clause.sourcePos, "cond clause must be a list")

  private def readParameters(parameterExpr: Expr, position: SourcePos): List[String] =
    parameterExpr match
      case Expr.ListExpr(parameters, _) =>
        parameters.map(expectParameterName)
      case _ =>
        throw EvalError.at(position, "lambda parameters must be a list")

  private def expectParameterName(expr: Expr): String = expr match
    case Expr.Symbol(name, _) => name
    case _                    => throw EvalError.at(expr.sourcePos, "parameter must be a symbol")

  private def quoteToValue(expr: Expr): Value = expr match
    case Expr.Literal(value, _) =>
      value
    case Expr.Symbol(name, _) =>
      Value.Symbol(name)
    case Expr.ListExpr(items, _) =>
      items.foldRight(Value.EmptyList: Value) { (item, tail) =>
        Value.Pair(quoteToValue(item), tail)
      }

  private def applyProcedure(procedure: Value, arguments: List[Expr], env: Env, position: SourcePos): Value =
    procedure match
      case Value.Builtin(name) =>
        applyBuiltin(name, evaluateArguments(arguments, env), position)
      case Value.Closure(parameters, body, closureEnv) =>
        val argumentValues = evaluateArguments(arguments, env).map(_.value)
        if parameters.length != argumentValues.length then
          throw EvalError.at(
            position,
            s"wrong argument count: expected ${parameters.length}, got ${argumentValues.length}"
          )
        else evaluateBody(body, closureEnv.extend(parameters.zip(argumentValues)))
      case _ =>
        throw EvalError.at(position, "attempted to call a non-procedure")

  private def evaluateBody(expressions: List[Expr], env: Env): Value = expressions match
    case Nil =>
      Value.Void
    case _ =>
      evaluateSequence(expressions, env)._2

  private def lookup(name: String, env: Env): Option[Value] =
    env.lookup(name).orElse(BuiltinProcedure.resolve(name))

  private def applyBuiltin(name: String, arguments: List[EvaluatedArg], position: SourcePos): Value =
    BuiltinProcedure(name, arguments, position)

  private def evaluateArguments(arguments: List[Expr], env: Env): List[EvaluatedArg] =
    arguments.map(argument => EvaluatedArg(evaluate(argument, env), argument.sourcePos))

  private def evaluateAnd(arguments: List[Expr], env: Env): Value =
    arguments match
      case Nil =>
        Value.Bool(true)
      case argument :: Nil =>
        evaluate(argument, env)
      case argument :: rest =>
        val value = evaluate(argument, env)
        if value.isTruthy then evaluateAnd(rest, env) else value

  private def evaluateOr(arguments: List[Expr], env: Env): Value =
    arguments match
      case Nil =>
        Value.Bool(false)
      case argument :: Nil =>
        evaluate(argument, env)
      case argument :: rest =>
        val value = evaluate(argument, env)
        if value.isTruthy then value else evaluateOr(rest, env)
