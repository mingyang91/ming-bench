package ming

import scala.annotation.tailrec

object SchemeInterpreter:

  def evaluateProgram(input: String): Value =
    evaluateAll(SchemeReader.readAll(input), Env.empty)

  final private case class EvaluatedArg(value: Value, position: SourcePos)

  private def evaluateAll(expressions: List[Expr], env: Env): Value = expressions match
    case Nil =>
      throw EvalError("expected at least one expression")
    case head :: tail =>
      val (nextEnv, value) = evaluateTopLevel(head, env)
      evaluateRemaining(tail, nextEnv, value)

  @tailrec
  private def evaluateRemaining(remaining: List[Expr], env: Env, current: Value): Value =
    remaining match
      case Nil => current
      case head :: tail =>
        val (nextEnv, value) = evaluateTopLevel(head, env)
        evaluateRemaining(tail, nextEnv, value)

  private def evaluateTopLevel(expr: Expr, env: Env): (Env, Value) = expr match
    case Expr.ListExpr(Expr.Symbol("define", position) :: arguments, _) =>
      (evaluateDefine(arguments, env, position), Value.Void)
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
        case Expr.Symbol("define", position) =>
          throw EvalError.at(position, "define is only allowed at the program top level")
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
    case head :: tail =>
      evaluateBodyTail(tail, env, evaluate(head, env))

  @tailrec
  private def evaluateBodyTail(remaining: List[Expr], env: Env, current: Value): Value =
    remaining match
      case Nil          => current
      case head :: tail => evaluateBodyTail(tail, env, evaluate(head, env))

  private def lookup(name: String, env: Env): Option[Value] =
    env.lookup(name).orElse(builtin(name))

  private def builtin(name: String): Option[Value] =
    name match
      case "+" | "-" | "*" | "/" | "<" | ">" | "=" | "<=" | "not" =>
        Some(Value.Builtin(name))
      case _ =>
        None

  private def applyBuiltin(name: String, arguments: List[EvaluatedArg], position: SourcePos): Value =
    name match
      case "+" =>
        Value.Number(expectNumbers(arguments).foldLeft(BigInt(0))(_ + _))
      case "*" =>
        Value.Number(expectNumbers(arguments).foldLeft(BigInt(1))(_ * _))
      case "-" =>
        Value.Number(evaluateSub(arguments, position))
      case "/" =>
        Value.Number(evaluateDiv(arguments, position))
      case "<" =>
        Value.Bool(compare(arguments, position)(_ < _))
      case ">" =>
        Value.Bool(compare(arguments, position)(_ > _))
      case "=" =>
        Value.Bool(compare(arguments, position)(_ == _))
      case "<=" =>
        Value.Bool(compare(arguments, position)(_ <= _))
      case "not" =>
        evaluateNot(arguments, position)
      case _ =>
        throw EvalError.at(position, s"unknown operator '$name'")

  private def evaluateArguments(arguments: List[Expr], env: Env): List[EvaluatedArg] =
    arguments.map(argument => EvaluatedArg(evaluate(argument, env), argument.sourcePos))

  private def expectNumbers(arguments: List[EvaluatedArg]): List[BigInt] =
    arguments.map(expectNumber)

  private def evaluateSub(arguments: List[EvaluatedArg], position: SourcePos): BigInt =
    expectNumbers(arguments) match
      case Nil =>
        throw EvalError.at(position, "'-' expects at least 1 argument")
      case head :: Nil =>
        -head
      case head :: tail =>
        tail.foldLeft(head)(_ - _)

  private def evaluateDiv(arguments: List[EvaluatedArg], position: SourcePos): BigInt =
    expectNumbers(arguments) match
      case _ :: Nil | Nil =>
        throw EvalError.at(position, "'/' expects at least 2 arguments")
      case head :: tail =>
        divide(head, tail, position)

  @tailrec
  private def divide(current: BigInt, remaining: List[BigInt], position: SourcePos): BigInt =
    remaining match
      case Nil =>
        current
      case head :: _ if head == 0 =>
        throw EvalError.at(position, "division by zero")
      case head :: tail =>
        divide(current / head, tail, position)

  private def compare(arguments: List[EvaluatedArg], position: SourcePos)(
    predicate: (BigInt, BigInt) => Boolean
  ): Boolean =
    expectNumbers(arguments) match
      case left :: right :: rest =>
        compareChain(right, rest, left, predicate)
      case _ =>
        throw EvalError.at(position, "comparison expects at least 2 arguments")

  @tailrec
  private def compareChain(
    current: BigInt,
    remaining: List[BigInt],
    previous: BigInt,
    predicate: (BigInt, BigInt) => Boolean
  ): Boolean =
    if !predicate(previous, current) then false
    else
      remaining match
        case Nil          => true
        case head :: tail => compareChain(head, tail, current, predicate)

  private def evaluateNot(arguments: List[EvaluatedArg], position: SourcePos): Value =
    arguments match
      case argument :: Nil =>
        Value.Bool(!argument.value.isTruthy)
      case _ =>
        throw EvalError.at(position, "'not' expects exactly 1 argument")

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

  private def expectNumber(argument: EvaluatedArg): BigInt =
    argument.value match
      case Value.Number(number) => number
      case _                    => throw EvalError.at(argument.position, "expected number")
