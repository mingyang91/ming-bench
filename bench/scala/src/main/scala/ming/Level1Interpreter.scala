package ming

import scala.annotation.tailrec

private[ming] object Level1Interpreter:

  def evalProgram(input: String): String =
    val expressions = SchemeParser.parseProgram(input)
    if expressions.isEmpty then SchemeFailure.raise("expected expression", Position(1, 1))

    val result =
      expressions.foldLeft[Value](BoolValue(false)): (_, expression) =>
        eval(expression, builtins)

    result.render

  private val builtins: Map[String, Value] = Map(
    "+"   -> BuiltinValue("+", add),
    "-"   -> BuiltinValue("-", subtract),
    "*"   -> BuiltinValue("*", multiply),
    "/"   -> BuiltinValue("/", divide),
    "<"   -> BuiltinValue("<", compareNumbers("<")(_ < _)),
    ">"   -> BuiltinValue(">", compareNumbers(">")(_ > _)),
    "="   -> BuiltinValue("=", compareNumbers("=")(_ == _)),
    "<="  -> BuiltinValue("<=", compareNumbers("<=")(_ <= _)),
    "not" -> BuiltinValue("not", logicalNot)
  )

  private def eval(expression: Expr, env: Map[String, Value]): Value =
    expression match
      case IntExpr(value, _)    => IntValue(value)
      case BoolExpr(value, _)   => BoolValue(value)
      case StringExpr(value, _) => StringValue(value)
      case SymbolExpr(name, position) =>
        env.getOrElse(name, SchemeFailure.raise(s"unbound symbol: $name", position))
      case ListExpr(items, position) =>
        evalList(items, position, env)

  private def evalList(items: List[Expr], position: Position, env: Map[String, Value]): Value =
    items match
      case Nil =>
        SchemeFailure.raise("cannot evaluate an empty list", position)
      case SymbolExpr("and", _) :: rest =>
        evalAnd(rest, env)
      case SymbolExpr("or", _) :: rest =>
        evalOr(rest, env)
      case operator :: arguments =>
        val function           = eval(operator, env)
        val evaluatedArguments = arguments.map(argument => eval(argument, env))
        apply(function, evaluatedArguments, position)

  @tailrec
  private def evalAnd(expressions: List[Expr], env: Map[String, Value]): Value =
    expressions match
      case Nil =>
        BoolValue(true)
      case expression :: Nil =>
        eval(expression, env)
      case expression :: rest =>
        val result = eval(expression, env)
        if isTruthy(result) then evalAnd(rest, env)
        else result

  @tailrec
  private def evalOr(expressions: List[Expr], env: Map[String, Value]): Value =
    expressions match
      case Nil =>
        BoolValue(false)
      case expression :: rest =>
        val result = eval(expression, env)
        if isTruthy(result) then result
        else evalOr(rest, env)

  private def apply(function: Value, arguments: List[Value], position: Position): Value =
    function match
      case BuiltinValue(_, implementation) =>
        implementation(arguments, position)
      case other =>
        SchemeFailure.raise(
          s"attempted to call a non-procedure value: ${other.render}",
          position
        )

  private def add(arguments: List[Value], position: Position): Value =
    IntValue(arguments.map(expectNumber(_, "+", position)).foldLeft(BigInt(0))(_ + _))

  private def subtract(arguments: List[Value], position: Position): Value =
    val numbers = expectAtLeast(arguments, 1, "-", position).map(expectNumber(_, "-", position))
    val result =
      numbers match
        case number :: Nil  => -number
        case number :: rest => rest.foldLeft(number)(_ - _)
        case Nil            => unreachable("validated non-empty argument list")
    IntValue(result)

  private def multiply(arguments: List[Value], position: Position): Value =
    IntValue(arguments.map(expectNumber(_, "*", position)).foldLeft(BigInt(1))(_ * _))

  private def divide(arguments: List[Value], position: Position): Value =
    val numbers = expectAtLeast(arguments, 1, "/", position).map(expectNumber(_, "/", position))
    val result =
      numbers match
        case denominator :: Nil =>
          divideExactly(BigInt(1), denominator, position)
        case numerator :: rest =>
          rest.foldLeft(numerator)((accumulator, denominator) => divideExactly(accumulator, denominator, position))
        case Nil =>
          unreachable("validated non-empty argument list")
    IntValue(result)

  private def divideExactly(
    numerator: BigInt,
    denominator: BigInt,
    position: Position
  ): BigInt =
    if denominator == 0 then SchemeFailure.raise("division by zero", position)

    if numerator % denominator != 0 then SchemeFailure.raise("division produced a non-integer result", position)

    numerator / denominator

  private def compareNumbers(
    name: String
  )(predicate: (BigInt, BigInt) => Boolean): (List[Value], Position) => Value =
    (arguments, position) =>
      val numbers = expectAtLeast(arguments, 2, name, position).map(expectNumber(_, name, position))
      BoolValue(numbers.zip(numbers.tail).forall((left, right) => predicate(left, right)))

  private def logicalNot(arguments: List[Value], position: Position): Value =
    val value =
      expectExact(arguments, 1, "not", position) match
        case argument :: Nil => argument
        case _               => unreachable("validated single argument list")

    BoolValue(!isTruthy(value))

  private def expectExact(
    arguments: List[Value],
    expected: Int,
    name: String,
    position: Position
  ): List[Value] =
    if arguments.length != expected then
      SchemeFailure.raise(s"$name expected $expected argument(s), got ${arguments.length}", position)

    arguments

  private def expectAtLeast(
    arguments: List[Value],
    minimum: Int,
    name: String,
    position: Position
  ): List[Value] =
    if arguments.length < minimum then
      SchemeFailure.raise(s"$name expected at least $minimum argument(s), got ${arguments.length}", position)

    arguments

  private def expectNumber(value: Value, name: String, position: Position): BigInt =
    value match
      case IntValue(number) => number
      case other =>
        SchemeFailure.raise(s"$name expected a number, got ${typeName(other)}", position)

  private def isTruthy(value: Value): Boolean =
    value match
      case BoolValue(false) => false
      case _                => true

  private def typeName(value: Value): String =
    value match
      case IntValue(_)        => "number"
      case BoolValue(_)       => "boolean"
      case StringValue(_)     => "string"
      case BuiltinValue(_, _) => "procedure"

  private def unreachable(message: String): Nothing =
    throw new IllegalStateException(message)
