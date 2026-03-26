package ming

import scala.collection.mutable.ListBuffer

object SchemeInterpreter:

  def evalToString(input: String): String =
    render(evalProgram(input))

  def evalToStringWithOutput(input: String): (String, String) =
    (evalToString(input), "")

  private def evalProgram(input: String): Value =
    val expressions = Parser(input).parseProgram()
    if expressions.isEmpty then throw new EvalError("empty program")

    var result: Value = Value.BooleanValue(false)
    expressions.foreach(expr => result = eval(expr, baseEnv))
    result

  private def eval(expr: Expr, env: Map[String, Value]): Value =
    expr match
      case Expr.IntegerLiteral(value) => Value.IntegerValue(value)
      case Expr.BooleanLiteral(value) => Value.BooleanValue(value)
      case Expr.StringLiteral(value)  => Value.StringValue(value)
      case Expr.Symbol(name) =>
        env.getOrElse(name, throw new EvalError(s"unbound variable: $name"))
      case Expr.ListExpr(Nil) =>
        throw new EvalError("cannot evaluate an empty list")
      case Expr.ListExpr(Expr.Symbol("and") :: rest) =>
        evalAnd(rest, env)
      case Expr.ListExpr(Expr.Symbol("or") :: rest) =>
        evalOr(rest, env)
      case Expr.ListExpr(operator :: args) =>
        apply(eval(operator, env), args.map(eval(_, env)))

  private def evalAnd(args: List[Expr], env: Map[String, Value]): Value =
    var result: Value = Value.BooleanValue(true)
    val iterator      = args.iterator
    while iterator.hasNext do
      result = eval(iterator.next(), env)
      if !isTruthy(result) then return result
    result

  private def evalOr(args: List[Expr], env: Map[String, Value]): Value =
    var result: Value = Value.BooleanValue(false)
    val iterator      = args.iterator
    while iterator.hasNext do
      result = eval(iterator.next(), env)
      if isTruthy(result) then return result
    result

  private def apply(procedure: Value, args: List[Value]): Value =
    procedure match
      case Value.Builtin(_, implementation) => implementation(args)
      case other =>
        throw new EvalError(s"not a procedure: ${render(other)}")

  private def isTruthy(value: Value): Boolean =
    value match
      case Value.BooleanValue(false) => false
      case _                         => true

  private def render(value: Value): String =
    value match
      case Value.IntegerValue(number) => number.toString
      case Value.BooleanValue(flag)   => if flag then "#t" else "#f"
      case Value.StringValue(text)    => s""""${escapeString(text)}""""
      case Value.Builtin(name, _)     => s"#<procedure:$name>"

  private def escapeString(text: String): String =
    text.flatMap {
      case '"'  => "\\\""
      case '\\' => "\\\\"
      case '\n' => "\\n"
      case '\r' => "\\r"
      case '\t' => "\\t"
      case c    => c.toString
    }

  private def requireArgCount(name: String, args: List[Value], exact: Int): Unit =
    if args.length != exact then throw new EvalError(s"$name expected $exact argument(s), got ${args.length}")

  private def requireMinArgCount(name: String, args: List[Value], minimum: Int): Unit =
    if args.length < minimum then
      throw new EvalError(s"$name expected at least $minimum argument(s), got ${args.length}")

  private def numericArgs(name: String, args: List[Value]): List[BigInt] =
    args.map {
      case Value.IntegerValue(number) => number
      case other =>
        throw new EvalError(s"$name expected a number, got ${render(other)}")
    }

  private def numericComparator(name: String)(predicate: (BigInt, BigInt) => Boolean): Value =
    Value.Builtin(
      name,
      args =>
        val numbers = numericArgs(name, args)
        requireMinArgCount(name, args, 2)
        Value.BooleanValue(numbers.zip(numbers.tail).forall(predicate.tupled))
    )

  private val baseEnv: Map[String, Value] = Map(
    "+" -> Value.Builtin(
      "+",
      args => Value.IntegerValue(numericArgs("+", args).foldLeft(BigInt(0))(_ + _))
    ),
    "-" -> Value.Builtin(
      "-",
      args =>
        val numbers = numericArgs("-", args)
        requireMinArgCount("-", args, 1)
        val result =
          if numbers.length == 1 then -numbers.head
          else numbers.tail.foldLeft(numbers.head)(_ - _)
        Value.IntegerValue(result)
    ),
    "*" -> Value.Builtin(
      "*",
      args => Value.IntegerValue(numericArgs("*", args).foldLeft(BigInt(1))(_ * _))
    ),
    "/" -> Value.Builtin(
      "/",
      args =>
        val numbers = numericArgs("/", args)
        requireMinArgCount("/", args, 2)
        val result = numbers.tail.foldLeft(numbers.head) { (left, right) =>
          if right == 0 then throw new EvalError("division by zero")
          left / right
        }
        Value.IntegerValue(result)
    ),
    "<"  -> numericComparator("<")(_ < _),
    ">"  -> numericComparator(">")(_ > _),
    "="  -> numericComparator("=")(_ == _),
    "<=" -> numericComparator("<=")(_ <= _),
    "not" -> Value.Builtin(
      "not",
      args =>
        requireArgCount("not", args, 1)
        Value.BooleanValue(!isTruthy(args.head))
    )
  )

  private enum Expr:
    case IntegerLiteral(value: BigInt)
    case BooleanLiteral(value: Boolean)
    case StringLiteral(value: String)
    case Symbol(name: String)
    case ListExpr(items: List[Expr])

  private enum Value:
    case IntegerValue(value: BigInt)
    case BooleanValue(value: Boolean)
    case StringValue(value: String)
    case Builtin(name: String, implementation: List[Value] => Value)

  final private class Parser(input: String):
    private var index = 0

    def parseProgram(): List[Expr] =
      val expressions = ListBuffer.empty[Expr]
      skipTrivia()
      while !isAtEnd do
        expressions += parseExpr()
        skipTrivia()
      expressions.toList

    private def parseExpr(): Expr =
      skipTrivia()
      if isAtEnd then parseError("unexpected end of input")

      input.charAt(index) match
        case '(' =>
          index += 1
          parseList()
        case ')' =>
          parseError("unexpected ')'")
        case '"' =>
          parseString()
        case '#' =>
          parseBoolean()
        case _ =>
          parseAtom()

    private def parseList(): Expr =
      val items = ListBuffer.empty[Expr]
      skipTrivia()
      while !isAtEnd && input.charAt(index) != ')' do
        items += parseExpr()
        skipTrivia()

      if isAtEnd then parseError("unterminated list")
      index += 1
      Expr.ListExpr(items.toList)

    private def parseString(): Expr =
      index += 1
      val builder = new StringBuilder

      while !isAtEnd && input.charAt(index) != '"' do
        val char = input.charAt(index)
        if char == '\\' then
          index += 1
          if isAtEnd then parseError("unterminated string escape")
          val escaped = input.charAt(index)
          builder +=
            (escaped match
              case '"'   => '"'
              case '\\'  => '\\'
              case 'n'   => '\n'
              case 'r'   => '\r'
              case 't'   => '\t'
              case other => other)
        else builder += char
        index += 1

      if isAtEnd then parseError("unterminated string literal")
      index += 1
      Expr.StringLiteral(builder.result())

    private def parseBoolean(): Expr =
      if startsWith("#t") && tokenBoundary(index + 2) then
        index += 2
        Expr.BooleanLiteral(true)
      else if startsWith("#f") && tokenBoundary(index + 2) then
        index += 2
        Expr.BooleanLiteral(false)
      else parseError("invalid boolean literal")

    private def parseAtom(): Expr =
      val start = index
      while !isAtEnd && !isDelimiter(input.charAt(index)) do index += 1

      val token = input.substring(start, index)
      if token.matches("-?\\d+") then Expr.IntegerLiteral(BigInt(token))
      else Expr.Symbol(token)

    private def skipTrivia(): Unit =
      var keepSkipping = true
      while keepSkipping && !isAtEnd do
        while !isAtEnd && input.charAt(index).isWhitespace do index += 1

        if !isAtEnd && input.charAt(index) == ';' then while !isAtEnd && input.charAt(index) != '\n' do index += 1
        else keepSkipping = false

    private def startsWith(prefix: String): Boolean =
      input.regionMatches(index, prefix, 0, prefix.length)

    private def tokenBoundary(boundary: Int): Boolean =
      boundary >= input.length || isDelimiter(input.charAt(boundary))

    private def isDelimiter(char: Char): Boolean =
      char.isWhitespace || char == '(' || char == ')' || char == ';'

    private def isAtEnd: Boolean =
      index >= input.length

    private def parseError(message: String): Nothing =
      throw new EvalError(s"$message at offset $index")
