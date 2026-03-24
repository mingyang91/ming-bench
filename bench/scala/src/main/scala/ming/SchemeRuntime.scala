package ming

import scala.annotation.tailrec

private enum Value:
  case IntegerValue(value: Int)
  case BooleanValue(value: Boolean)
  case StringValue(value: String)
  case BuiltinValue(name: String, applyTo: List[Value] => Value)

private[ming] object SchemeRuntime:

  private val builtins: Map[String, List[Value] => Value] = Map(
    "+"   -> builtinAdd,
    "-"   -> builtinSubtract,
    "*"   -> builtinMultiply,
    "/"   -> builtinDivide,
    "<"   -> builtinLessThan,
    ">"   -> builtinGreaterThan,
    "="   -> builtinEqual,
    "<="  -> builtinLessThanOrEqual,
    "not" -> builtinNot
  )

  def evaluateProgramToString(expressions: List[Expr]): String =
    val result = expressions
      .foldLeft[Option[Value]](None) { (_, expr) =>
        Some(evaluate(expr))
      }
      .getOrElse(throw new EvalError("empty input"))
    renderValue(result)

  private def evaluate(expr: Expr): Value =
    expr match
      case Expr.IntegerLiteral(value) => Value.IntegerValue(value)
      case Expr.BooleanLiteral(value) => Value.BooleanValue(value)
      case Expr.StringLiteral(value)  => Value.StringValue(value)
      case Expr.Symbol(name)          => lookupBuiltin(name)
      case Expr.ListExpr(elements)    => evaluateList(elements)

  private def evaluateList(elements: List[Expr]): Value =
    elements match
      case Nil =>
        throw new EvalError("cannot evaluate empty list")
      case Expr.Symbol("and") :: rest =>
        evaluateAnd(rest)
      case Expr.Symbol("or") :: rest =>
        evaluateOr(rest)
      case operatorExpr :: argumentExprs =>
        val operator  = evaluate(operatorExpr)
        val arguments = argumentExprs.map(evaluate)
        applyBuiltin(operator, arguments)

  private def evaluateAnd(expressions: List[Expr]): Value =
    expressions match
      case Nil => Value.BooleanValue(true)
      case _   => evaluateAndLoop(expressions, Value.BooleanValue(true))

  @tailrec
  private def evaluateAndLoop(expressions: List[Expr], lastValue: Value): Value =
    expressions match
      case Nil => lastValue
      case expr :: rest =>
        val value = evaluate(expr)
        if !isTruthy(value) then value
        else evaluateAndLoop(rest, value)

  private def evaluateOr(expressions: List[Expr]): Value =
    expressions match
      case Nil => Value.BooleanValue(false)
      case _   => evaluateOrLoop(expressions, Value.BooleanValue(false))

  @tailrec
  private def evaluateOrLoop(expressions: List[Expr], lastValue: Value): Value =
    expressions match
      case Nil => lastValue
      case expr :: rest =>
        val value = evaluate(expr)
        if isTruthy(value) then value
        else evaluateOrLoop(rest, value)

  private def lookupBuiltin(name: String): Value =
    builtins
      .get(name)
      .map(fn => Value.BuiltinValue(name, fn))
      .getOrElse(throw new EvalError(s"unbound symbol: $name"))

  private def applyBuiltin(operator: Value, arguments: List[Value]): Value =
    operator match
      case Value.BuiltinValue(_, applyTo) => applyTo(arguments)
      case other =>
        throw new EvalError(s"attempted to call non-procedure: ${renderValue(other)}")

  private def renderValue(value: Value): String =
    value match
      case Value.IntegerValue(number) => number.toString
      case Value.BooleanValue(true)   => "#t"
      case Value.BooleanValue(false)  => "#f"
      case Value.StringValue(text)    => quoteString(text)
      case Value.BuiltinValue(name, _) =>
        s"#<procedure:$name>"

  private def quoteString(text: String): String =
    val escaped = text.flatMap {
      case '"'   => "\\\""
      case '\\'  => "\\\\"
      case '\n'  => "\\n"
      case '\r'  => "\\r"
      case '\t'  => "\\t"
      case other => other.toString
    }
    s"\"$escaped\""

  private def isTruthy(value: Value): Boolean =
    value match
      case Value.BooleanValue(false) => false
      case _                         => true

  private def expectInteger(value: Value): Int =
    value match
      case Value.IntegerValue(number) => number
      case other =>
        throw new EvalError(s"expected number, got ${renderValue(other)}")

  private def requireExactlyOne(name: String, arguments: List[Value]): Value =
    arguments match
      case argument :: Nil => argument
      case _ =>
        throw new EvalError(s"$name expects exactly 1 argument")

  private def requireAtLeastOne(name: String, arguments: List[Value]): List[Value] =
    if arguments.nonEmpty then arguments
    else throw new EvalError(s"$name expects at least 1 argument")

  private def requireAtLeastTwo(name: String, arguments: List[Value]): List[Value] =
    if arguments.lengthCompare(2) >= 0 then arguments
    else throw new EvalError(s"$name expects at least 2 arguments")

  private def builtinAdd(arguments: List[Value]): Value =
    Value.IntegerValue(arguments.map(expectInteger).sum)

  private def builtinSubtract(arguments: List[Value]): Value =
    requireAtLeastOne("-", arguments) match
      case argument :: Nil =>
        Value.IntegerValue(-expectInteger(argument))
      case first :: rest =>
        val initial = expectInteger(first)
        Value.IntegerValue(rest.foldLeft(initial) { (acc, argument) =>
          acc - expectInteger(argument)
        })
      case Nil =>
        throw new EvalError("- expects at least 1 argument")

  private def builtinMultiply(arguments: List[Value]): Value =
    Value.IntegerValue(arguments.map(expectInteger).product)

  private def builtinDivide(arguments: List[Value]): Value =
    requireAtLeastTwo("/", arguments) match
      case first :: rest =>
        val initial = expectInteger(first)
        Value.IntegerValue(rest.foldLeft(initial) { (acc, argument) =>
          val divisor = expectInteger(argument)
          if divisor == 0 then throw new EvalError("division by zero")
          acc / divisor
        })
      case Nil =>
        throw new EvalError("/ expects at least 2 arguments")

  private def builtinLessThan(arguments: List[Value]): Value =
    comparison(arguments, "<", _ < _)

  private def builtinGreaterThan(arguments: List[Value]): Value =
    comparison(arguments, ">", _ > _)

  private def builtinEqual(arguments: List[Value]): Value =
    comparison(arguments, "=", _ == _)

  private def builtinLessThanOrEqual(arguments: List[Value]): Value =
    comparison(arguments, "<=", _ <= _)

  private def comparison(
    arguments: List[Value],
    name: String,
    predicate: (Int, Int) => Boolean
  ): Value =
    requireAtLeastTwo(name, arguments)
    val numbers = arguments.map(expectInteger)
    Value.BooleanValue(numbers.zip(numbers.drop(1)).forall { (left, right) =>
      predicate(left, right)
    })

  private def builtinNot(arguments: List[Value]): Value =
    val argument = requireExactlyOne("not", arguments)
    Value.BooleanValue(!isTruthy(argument))
