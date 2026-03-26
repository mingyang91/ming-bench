package ming

import SchemeSyntax.Expr

import scala.annotation.tailrec

private[ming] object SchemeInterpreter:

  def evalWithOutput(input: String): (String, String) =
    val expressions = SchemeParser.parseProgram(input)
    if expressions.isEmpty then fail("expected at least one expression")

    val result = expressions.foldLeft[Value](Value.Bool(false)) { (_, expr) =>
      eval(expr)
    }

    (render(result), "")

  private enum Value:
    case Number(value: BigInt)
    case Bool(value: Boolean)
    case Str(value: String)
    case Builtin(name: String, apply: List[Value] => Value)

  private val builtins: Map[String, Value] = Map(
    "+"   -> Value.Builtin("+", add),
    "-"   -> Value.Builtin("-", subtract),
    "*"   -> Value.Builtin("*", multiply),
    "/"   -> Value.Builtin("/", divide),
    "<"   -> Value.Builtin("<", compare("<")(_ < _)),
    ">"   -> Value.Builtin(">", compare(">")(_ > _)),
    "="   -> Value.Builtin("=", compare("=")(_ == _)),
    "<="  -> Value.Builtin("<=", compare("<=")(_ <= _)),
    "not" -> Value.Builtin("not", not)
  )

  private def eval(expr: Expr): Value =
    expr match
      case Expr.IntegerLiteral(value) => Value.Number(value)
      case Expr.BooleanLiteral(value) => Value.Bool(value)
      case Expr.StringLiteral(value)  => Value.Str(value)
      case Expr.Symbol(name) =>
        builtins.getOrElse(name, fail(s"unbound symbol: $name"))
      case Expr.ListExpr(items) =>
        items match
          case Nil =>
            fail("cannot evaluate an empty list")
          case Expr.Symbol("and") :: rest =>
            evalAnd(rest)
          case Expr.Symbol("or") :: rest =>
            evalOr(rest)
          case head :: tail =>
            val function  = eval(head)
            val arguments = tail.map(eval)
            apply(function, arguments)

  @tailrec
  private def evalAnd(expressions: List[Expr], lastValue: Value = Value.Bool(true)): Value =
    expressions match
      case Nil => lastValue
      case head :: tail =>
        val value = eval(head)
        if isTruthy(value) then evalAnd(tail, value)
        else value

  @tailrec
  private def evalOr(expressions: List[Expr]): Value =
    expressions match
      case Nil => Value.Bool(false)
      case head :: tail =>
        val value = eval(head)
        if isTruthy(value) then value
        else evalOr(tail)

  private def apply(function: Value, arguments: List[Value]): Value =
    function match
      case Value.Builtin(_, implementation) => implementation(arguments)
      case other =>
        fail(s"attempted to call a non-procedure: ${render(other)}")

  private def add(arguments: List[Value]): Value =
    Value.Number(numbersFor("+", arguments).foldLeft(BigInt(0))(_ + _))

  private def subtract(arguments: List[Value]): Value =
    val numbers = numbersFor("-", arguments)
    numbers match
      case Nil =>
        fail("'-' expects at least 1 argument")
      case value :: Nil =>
        Value.Number(-value)
      case value :: rest =>
        Value.Number(rest.foldLeft(value)(_ - _))

  private def multiply(arguments: List[Value]): Value =
    Value.Number(numbersFor("*", arguments).foldLeft(BigInt(1))(_ * _))

  private def divide(arguments: List[Value]): Value =
    val numbers = numbersFor("/", arguments)
    numbers match
      case first :: second :: rest =>
        val quotient = rest.foldLeft(dividePair(first, second))(dividePair)
        Value.Number(quotient)
      case _ =>
        fail("'/' expects at least 2 arguments")

  private def compare(name: String)(
    predicate: (BigInt, BigInt) => Boolean
  ): List[Value] => Value =
    arguments =>
      val numbers = numbersFor(name, arguments)
      if numbers.lengthCompare(2) < 0 then fail(s"'$name' expects at least 2 arguments")

      val matches = numbers.zip(numbers.tail).forall { (left, right) =>
        predicate(left, right)
      }
      Value.Bool(matches)

  private def not(arguments: List[Value]): Value =
    val argument = requireExactArity("not", arguments, 1)
    Value.Bool(!isTruthy(argument.head))

  private def numbersFor(name: String, arguments: List[Value]): List[BigInt] =
    arguments.map {
      case Value.Number(value) => value
      case other               => fail(s"'$name' expects numbers, got ${typeName(other)}")
    }

  private def requireExactArity(
    name: String,
    arguments: List[Value],
    expected: Int
  ): List[Value] =
    if arguments.lengthCompare(expected) != 0 then
      fail(s"'$name' expects $expected argument(s), got ${arguments.length}")
    arguments

  private def dividePair(left: BigInt, right: BigInt): BigInt =
    if right == 0 then fail("division by zero")
    left / right

  private def isTruthy(value: Value): Boolean =
    value match
      case Value.Bool(false) => false
      case _                 => true

  private def typeName(value: Value): String =
    value match
      case Value.Number(_)     => "number"
      case Value.Bool(_)       => "boolean"
      case Value.Str(_)        => "string"
      case Value.Builtin(_, _) => "procedure"

  private def render(value: Value): String =
    value match
      case Value.Number(number) => number.toString
      case Value.Bool(true)     => "#t"
      case Value.Bool(false)    => "#f"
      case Value.Str(string)    => quoteString(string)
      case Value.Builtin(name, _) =>
        s"#<procedure:$name>"

  private def quoteString(value: String): String =
    val escaped = value.flatMap {
      case '\\' => "\\\\"
      case '"'  => "\\\""
      case '\n' => "\\n"
      case '\r' => "\\r"
      case '\t' => "\\t"
      case char => char.toString
    }
    s""""$escaped""""

  private def fail(message: String): Nothing =
    throw new EvalError(message)
