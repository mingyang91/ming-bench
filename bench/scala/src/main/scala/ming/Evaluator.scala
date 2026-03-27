package ming

import scala.collection.mutable.ListBuffer

/** Scheme interpreter entry point. Agents implement this object. */
object Evaluator:

  /** Evaluate one or more Scheme expressions and return the string representation of the last result.
    */
  def evalStr(input: String): String =
    val expressions = Parser(input).parseProgram()
    if expressions.isEmpty then throw EvalError("empty input")

    val result = expressions.foldLeft[Value](Value.Void) { (_, expr) =>
      eval(expr)
    }
    render(result)

  /** Evaluate Scheme expressions and return both the result string and any captured output from display/write/newline.
    */
  def evalStrWithOutput(input: String): (String, String) =
    (evalStr(input), "")

  private def eval(expr: Expr): Value =
    expr match
      case Expr.IntLit(value)    => Value.IntVal(value)
      case Expr.BoolLit(value)   => Value.BoolVal(value)
      case Expr.StringLit(value) => Value.StringVal(value)
      case Expr.Symbol(name)     => throw EvalError(s"unbound variable: $name")
      case Expr.ListExpr(items)  => evalList(items)

  private def evalList(items: List[Expr]): Value =
    items match
      case Nil => throw EvalError("cannot evaluate empty list")
      case Expr.Symbol("and") :: args =>
        evalAnd(args, Value.BoolVal(true))
      case Expr.Symbol("or") :: args =>
        evalOr(args)
      case Expr.Symbol(name) :: args =>
        applyBuiltin(name, args)
      case head :: _ =>
        eval(head)
        throw EvalError("attempted to call a non-procedure")

  @annotation.tailrec
  private def evalAnd(args: List[Expr], lastValue: Value): Value =
    args match
      case Nil => lastValue
      case head :: tail =>
        val value = eval(head)
        if isTruthy(value) then evalAnd(tail, value)
        else value

  @annotation.tailrec
  private def evalOr(args: List[Expr]): Value =
    args match
      case Nil => Value.BoolVal(false)
      case head :: tail =>
        val value = eval(head)
        if isTruthy(value) then value
        else evalOr(tail)

  private def applyBuiltin(name: String, args: List[Expr]): Value =
    name match
      case "+" =>
        Value.IntVal(evalNumbers(name, args).sum)
      case "-" =>
        val numbers = requireMinArgs(name, evalNumbers(name, args), min = 1)
        numbers match
          case value :: Nil => Value.IntVal(-value)
          case value :: rest =>
            Value.IntVal(rest.foldLeft(value)(_ - _))
          case Nil =>
            throw EvalError(s"$name expects at least 1 argument")
      case "*" =>
        Value.IntVal(evalNumbers(name, args).product)
      case "/" =>
        val numbers = requireMinArgs(name, evalNumbers(name, args), min = 2)
        val result = numbers.tail.foldLeft(numbers.head) { (acc, divisor) =>
          if divisor == 0 then throw EvalError("division by zero")
          acc / divisor
        }
        Value.IntVal(result)
      case "<" =>
        compareNumbers(name, args)(_ < _)
      case ">" =>
        compareNumbers(name, args)(_ > _)
      case "=" =>
        compareNumbers(name, args)(_ == _)
      case "<=" =>
        compareNumbers(name, args)(_ <= _)
      case "not" =>
        val values = requireArgCount(name, evalArgs(args), expected = 1)
        Value.BoolVal(!isTruthy(values.head))
      case _ =>
        throw EvalError(s"unknown procedure: $name")

  private def compareNumbers(
    name: String,
    args: List[Expr]
  )(predicate: (Int, Int) => Boolean): Value =
    val numbers = requireMinArgs(name, evalNumbers(name, args), min = 2)
    Value.BoolVal(numbers.zip(numbers.tail).forall(predicate.tupled))

  private def evalArgs(args: List[Expr]): List[Value] =
    args.map(eval)

  private def evalNumbers(name: String, args: List[Expr]): List[Int] =
    evalArgs(args).map {
      case Value.IntVal(value) => value
      case other               => throw EvalError(s"$name expected a number, got ${typeName(other)}")
    }

  private def requireArgCount[T](name: String, args: List[T], expected: Int): List[T] =
    if args.lengthCompare(expected) != 0 then
      throw EvalError(s"$name expects $expected argument(s), got ${args.length}")
    args

  private def requireMinArgs[T](name: String, args: List[T], min: Int): List[T] =
    if args.lengthCompare(min) < 0 then throw EvalError(s"$name expects at least $min argument(s), got ${args.length}")
    args

  private def isTruthy(value: Value): Boolean =
    value match
      case Value.BoolVal(false) => false
      case _                    => true

  private def render(value: Value): String =
    value match
      case Value.IntVal(number)  => number.toString
      case Value.BoolVal(true)   => "#t"
      case Value.BoolVal(false)  => "#f"
      case Value.StringVal(text) => s""""${escapeString(text)}""""
      case Value.Void            => ""

  private def escapeString(text: String): String =
    text.flatMap {
      case '\\' => "\\\\"
      case '"'  => "\\\""
      case '\n' => "\\n"
      case '\r' => "\\r"
      case '\t' => "\\t"
      case ch   => ch.toString
    }

  private def typeName(value: Value): String =
    value match
      case Value.IntVal(_)    => "number"
      case Value.BoolVal(_)   => "boolean"
      case Value.StringVal(_) => "string"
      case Value.Void         => "void"

  private enum Expr:
    case IntLit(value: Int)
    case BoolLit(value: Boolean)
    case StringLit(value: String)
    case Symbol(name: String)
    case ListExpr(items: List[Expr])

  private enum Value:
    case IntVal(value: Int)
    case BoolVal(value: Boolean)
    case StringVal(value: String)
    case Void

  private object Parser:
    def apply(input: String): Parser = new Parser(input)

  final private class Parser(input: String):
    private var index = 0

    def parseProgram(): List[Expr] =
      val expressions = ListBuffer.empty[Expr]
      skipIgnored()
      while !atEnd do
        expressions += parseExpr()
        skipIgnored()
      expressions.toList

    private def parseExpr(): Expr =
      skipIgnored()
      if atEnd then fail("unexpected end of input")

      currentChar match
        case '(' =>
          index += 1
          parseList()
        case ')' =>
          fail("unexpected ')'")
        case '"' =>
          Expr.StringLit(parseString())
        case _ =>
          parseAtom()

    private def parseList(): Expr =
      val items = ListBuffer.empty[Expr]
      skipIgnored()
      while !atEnd && currentChar != ')' do
        items += parseExpr()
        skipIgnored()

      if atEnd then fail("unterminated list")

      index += 1
      Expr.ListExpr(items.toList)

    private def parseString(): String =
      index += 1
      val builder = new StringBuilder()

      while !atEnd do
        val ch = currentChar
        index += 1
        ch match
          case '"' =>
            return builder.toString
          case '\\' =>
            if atEnd then fail("unterminated escape sequence")
            val escaped = currentChar
            index += 1
            builder += (
              escaped match
                case '"'   => '"'
                case '\\'  => '\\'
                case 'n'   => '\n'
                case 'r'   => '\r'
                case 't'   => '\t'
                case other => other
            )
          case other =>
            builder += other

      fail("unterminated string literal")

    private def parseAtom(): Expr =
      val start = index
      while !atEnd && !isDelimiter(currentChar) do index += 1

      val token = input.substring(start, index)
      token match
        case "#t" => Expr.BoolLit(true)
        case "#f" => Expr.BoolLit(false)
        case _ if isIntegerToken(token) =>
          Expr.IntLit(token.toInt)
        case _ if token.nonEmpty =>
          Expr.Symbol(token)
        case _ =>
          fail("expected expression")

    private def skipIgnored(): Unit =
      var keepSkipping = true
      while keepSkipping && !atEnd do
        currentChar match
          case ch if ch.isWhitespace =>
            index += 1
          case ';' =>
            skipComment()
          case _ =>
            keepSkipping = false

    private def skipComment(): Unit =
      while !atEnd && currentChar != '\n' do index += 1

    private def isDelimiter(ch: Char): Boolean =
      ch.isWhitespace || ch == '(' || ch == ')' || ch == ';'

    private def isIntegerToken(token: String): Boolean =
      if token.isEmpty then false
      else
        val digits =
          token.head match
            case '+' | '-' => token.drop(1)
            case _         => token
        digits.nonEmpty && digits.forall(_.isDigit)

    private def atEnd: Boolean =
      index >= input.length

    private def currentChar: Char =
      input.charAt(index)

    private def fail(message: String): Nothing =
      throw EvalError(message)
