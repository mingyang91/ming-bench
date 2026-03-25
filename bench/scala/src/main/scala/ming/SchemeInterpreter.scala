package ming

private[ming] object SchemeInterpreter:

  sealed trait Expr

  object Expr:
    final case class Number(value: BigInt)       extends Expr
    final case class Bool(value: Boolean)        extends Expr
    final case class StringLit(value: String)    extends Expr
    final case class Symbol(name: String)        extends Expr
    final case class ListExpr(items: List[Expr]) extends Expr

  sealed trait Value

  object Value:
    final case class Number(value: BigInt)    extends Value
    final case class Bool(value: Boolean)     extends Value
    final case class StringLit(value: String) extends Value

  def evalProgram(input: String): Value =
    val expressions = Reader(input).readAll()
    if expressions.isEmpty then throw new EvalError("empty input")
    expressions.map(eval).last

  def evalProgramWithOutput(input: String): (Value, String) =
    (evalProgram(input), "")

  def render(value: Value): String =
    value match
      case Value.Number(value)    => value.toString
      case Value.Bool(true)       => "#t"
      case Value.Bool(false)      => "#f"
      case Value.StringLit(value) => "\"" + escapeString(value) + "\""

  private def eval(expr: Expr): Value =
    expr match
      case Expr.Number(value)    => Value.Number(value)
      case Expr.Bool(value)      => Value.Bool(value)
      case Expr.StringLit(value) => Value.StringLit(value)
      case Expr.Symbol(name)     => throw new EvalError(s"unbound variable: $name")
      case Expr.ListExpr(items) =>
        items match
          case Nil                        => throw new EvalError("cannot evaluate empty list")
          case Expr.Symbol("and") :: args => evalAnd(args)
          case Expr.Symbol("or") :: args  => evalOr(args)
          case Expr.Symbol(name) :: args  => applyBuiltin(name, args.map(eval))
          case head :: _ =>
            throw new EvalError(s"not a procedure: ${renderExpr(head)}")

  private def evalAnd(args: List[Expr]): Value =
    args match
      case Nil => Value.Bool(true)
      case head :: tail =>
        val value = eval(head)
        if !isTruthy(value) || tail.isEmpty then value
        else evalAnd(tail)

  private def evalOr(args: List[Expr]): Value =
    args match
      case Nil => Value.Bool(false)
      case head :: tail =>
        val value = eval(head)
        if isTruthy(value) || tail.isEmpty then value
        else evalOr(tail)

  private def applyBuiltin(name: String, args: List[Value]): Value =
    name match
      case "+" =>
        Value.Number(args.foldLeft(BigInt(0))((acc, value) => acc + asNumber(value, name)))
      case "-" =>
        requireAtLeast(name, args, 1)
        val numbers = args.map(asNumber(_, name))
        numbers match
          case value :: Nil  => Value.Number(-value)
          case value :: rest => Value.Number(rest.foldLeft(value)(_ - _))
          case Nil           => unreachable()
      case "*" =>
        Value.Number(args.foldLeft(BigInt(1))((acc, value) => acc * asNumber(value, name)))
      case "/" =>
        requireAtLeast(name, args, 2)
        val numbers = args.map(asNumber(_, name))
        numbers match
          case value :: rest => Value.Number(rest.foldLeft(value)(divide(_, _, name)))
          case Nil           => unreachable()
      case "<" =>
        Value.Bool(compareAdjacent(name, args)(_ < _))
      case ">" =>
        Value.Bool(compareAdjacent(name, args)(_ > _))
      case "=" =>
        Value.Bool(compareAdjacent(name, args)(_ == _))
      case "<=" =>
        Value.Bool(compareAdjacent(name, args)(_ <= _))
      case "not" =>
        requireExactly(name, args, 1)
        Value.Bool(!isTruthy(args.head))
      case other =>
        throw new EvalError(s"unknown procedure: $other")

  private def compareAdjacent(
    name: String,
    args: List[Value]
  )(predicate: (BigInt, BigInt) => Boolean): Boolean =
    requireAtLeast(name, args, 2)
    val numbers = args.map(asNumber(_, name))
    numbers.zip(numbers.tail).forall { case (left, right) => predicate(left, right) }

  private def asNumber(value: Value, context: String): BigInt =
    value match
      case Value.Number(number) => number
      case other                => throw new EvalError(s"$context expected number, got ${render(other)}")

  private def divide(left: BigInt, right: BigInt, context: String): BigInt =
    if right == 0 then throw new EvalError(s"$context division by zero")
    left / right

  private def requireExactly(name: String, args: List[Value], expected: Int): Unit =
    if args.length != expected then throw new EvalError(s"$name expected $expected arguments, got ${args.length}")

  private def requireAtLeast(name: String, args: List[Value], expected: Int): Unit =
    if args.length < expected then
      throw new EvalError(s"$name expected at least $expected arguments, got ${args.length}")

  private def isTruthy(value: Value): Boolean =
    value match
      case Value.Bool(false) => false
      case _                 => true

  private def renderExpr(expr: Expr): String =
    expr match
      case Expr.Number(value)    => value.toString
      case Expr.Bool(true)       => "#t"
      case Expr.Bool(false)      => "#f"
      case Expr.StringLit(value) => "\"" + escapeString(value) + "\""
      case Expr.Symbol(name)     => name
      case Expr.ListExpr(items) =>
        items.map(renderExpr).mkString("(", " ", ")")

  private def escapeString(value: String): String =
    val builder = new StringBuilder
    value.foreach {
      case '"'  => builder.append("\\\"")
      case '\\' => builder.append("\\\\")
      case '\n' => builder.append("\\n")
      case '\t' => builder.append("\\t")
      case ch   => builder.append(ch)
    }
    builder.result()

  private def unreachable(): Nothing =
    throw IllegalStateException("unreachable")

  final private class Reader(input: String):
    private var index = 0

    def readAll(): List[Expr] =
      val expressions = List.newBuilder[Expr]
      skipTrivia()
      while index < input.length do
        expressions += readExpr()
        skipTrivia()
      expressions.result()

    private def readExpr(): Expr =
      skipTrivia()
      if index >= input.length then throw new EvalError("unexpected end of input")

      input.charAt(index) match
        case '(' =>
          index += 1
          readList()
        case ')' =>
          throw new EvalError("unexpected )")
        case '"' =>
          readString()
        case '#' =>
          readBoolean()
        case _ =>
          readAtom()

    private def readList(): Expr =
      val items = List.newBuilder[Expr]
      skipTrivia()
      while index < input.length && input.charAt(index) != ')' do
        items += readExpr()
        skipTrivia()

      if index >= input.length then throw new EvalError("unterminated list")

      index += 1
      Expr.ListExpr(items.result())

    private def readString(): Expr =
      index += 1
      val builder = new StringBuilder
      var closed  = false

      while index < input.length && !closed do
        val ch = input.charAt(index)
        index += 1
        ch match
          case '"' =>
            closed = true
          case '\\' =>
            if index >= input.length then throw new EvalError("unterminated string escape")
            val escaped = input.charAt(index)
            index += 1
            builder.append(
              escaped match
                case '"'   => '"'
                case '\\'  => '\\'
                case 'n'   => '\n'
                case 't'   => '\t'
                case other => other
            )
          case other =>
            builder.append(other)

      if !closed then throw new EvalError("unterminated string")

      Expr.StringLit(builder.result())

    private def readBoolean(): Expr =
      if startsWithToken("#t") then
        index += 2
        Expr.Bool(true)
      else if startsWithToken("#f") then
        index += 2
        Expr.Bool(false)
      else throw new EvalError("invalid boolean literal")

    private def readAtom(): Expr =
      val start = index
      while index < input.length && !isDelimiter(input.charAt(index)) do index += 1

      val token = input.substring(start, index)
      if isIntegerToken(token) then Expr.Number(BigInt(token))
      else Expr.Symbol(token)

    private def skipTrivia(): Unit =
      var keepSkipping = true
      while keepSkipping do
        while index < input.length && input.charAt(index).isWhitespace do index += 1

        if index < input.length && input.charAt(index) == ';' then
          while index < input.length && input.charAt(index) != '\n' do index += 1
        else keepSkipping = false

    private def startsWithToken(token: String): Boolean =
      input.startsWith(token, index) && {
        val boundary = index + token.length
        boundary >= input.length || isDelimiter(input.charAt(boundary))
      }

    private def isDelimiter(ch: Char): Boolean =
      ch.isWhitespace || ch == '(' || ch == ')' || ch == '"' || ch == ';'

    private def isIntegerToken(token: String): Boolean =
      token.nonEmpty &&
        (token.forall(_.isDigit) ||
          (token.head == '-' && token.length > 1 && token.tail.forall(_.isDigit)))

  private object Reader:
    def apply(input: String): Reader = new Reader(input)
