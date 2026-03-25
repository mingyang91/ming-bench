package ming

private[ming] object SchemeReader:
  import SchemeInterpreter.Expr

  def readAll(input: String): List[Expr] =
    Reader(input).readAll()

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
