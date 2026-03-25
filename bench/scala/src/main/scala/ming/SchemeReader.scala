package ming

import scala.collection.mutable.ListBuffer

private[ming] object SchemeReader:
  import SchemeInterpreter.Expr

  def readAll(input: String): List[Expr] =
    Reader(input).readAll()

  final private class Reader(input: String):
    private var index = 0
    private var line  = 1
    private var col   = 1

    private def currentPos: SourcePos =
      SourcePos(line, col)

    def readAll(): List[Expr] =
      val expressions = List.newBuilder[Expr]
      skipTrivia()
      while index < input.length do
        expressions += readExpr()
        skipTrivia()
      expressions.result()

    private def readExpr(): Expr =
      skipTrivia()
      if index >= input.length then throw EvalError.at(currentPos, "unexpected end of input")

      val pos = currentPos
      input.charAt(index) match
        case '\'' =>
          advance()
          Expr.ListExpr(List(Expr.Symbol("quote", pos), readExpr()), pos)
        case '`' =>
          advance()
          Expr.ListExpr(List(Expr.Symbol("quasiquote", pos), readExpr()), pos)
        case ',' =>
          advance()
          if index < input.length && input.charAt(index) == '@' then
            advance()
            Expr.ListExpr(List(Expr.Symbol("unquote-splicing", pos), readExpr()), pos)
          else Expr.ListExpr(List(Expr.Symbol("unquote", pos), readExpr()), pos)
        case '(' =>
          advance()
          readList(pos)
        case ')' =>
          throw EvalError.at(pos, "unexpected )")
        case '"' =>
          readString(pos)
        case '#' =>
          readHashLiteral(pos)
        case _ =>
          readAtom(pos)

    private def readList(startPos: SourcePos): Expr =
      val items                                 = ListBuffer.empty[Expr]
      var dottedTail: Option[(SourcePos, Expr)] = None
      skipTrivia()
      while index < input.length && input.charAt(index) != ')' do
        val item = readExpr()
        item match
          case Expr.Symbol(".", dotPos) =>
            if items.isEmpty || dottedTail.nonEmpty then throw EvalError.at(dotPos, "invalid dotted list")
            skipTrivia()
            if index >= input.length || input.charAt(index) == ')' then
              throw EvalError.at(dotPos, "invalid dotted list")
            val tailExpr = readExpr()
            dottedTail = Some((dotPos, tailExpr))
            skipTrivia()
            if index >= input.length || input.charAt(index) != ')' then
              throw EvalError.at(dotPos, "invalid dotted list")
          case other =>
            if dottedTail.nonEmpty then throw EvalError.at(other.pos, "invalid dotted list")
            items += other
        skipTrivia()

      if index >= input.length then throw EvalError.at(startPos, "unterminated list")

      advance()
      dottedTail match
        case Some((dotPos, tailExpr)) =>
          Expr.ListExpr(items.toList :+ Expr.Symbol(".", dotPos) :+ tailExpr, startPos)
        case None =>
          Expr.ListExpr(items.toList, startPos)

    private def readString(startPos: SourcePos): Expr =
      advance()
      val builder = new StringBuilder
      var closed  = false

      while index < input.length && !closed do
        val ch = advance()
        ch match
          case '"' =>
            closed = true
          case '\\' =>
            if index >= input.length then throw EvalError.at(startPos, "unterminated string escape")
            val escaped = advance()
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

      if !closed then throw EvalError.at(startPos, "unterminated string")

      Expr.StringLit(builder.result(), startPos)

    private def readHashLiteral(startPos: SourcePos): Expr =
      if input.startsWith("#'", index) then
        advance()
        advance()
        Expr.ListExpr(List(Expr.Symbol("syntax", startPos), readExpr()), startPos)
      else if input.startsWith("#(", index) then readVector(startPos)
      else if startsWithToken("#t") then
        advance()
        advance()
        Expr.Bool(true, startPos)
      else if startsWithToken("#f") then
        advance()
        advance()
        Expr.Bool(false, startPos)
      else if input.startsWith("#\\", index) then readCharacter(startPos)
      else throw EvalError.at(startPos, "invalid boolean literal")

    private def readCharacter(startPos: SourcePos): Expr =
      advance()
      advance()

      val start = index
      while index < input.length && !isDelimiter(input.charAt(index)) do advance()

      val token = input.substring(start, index)
      val value = token match
        case "space"            => ' '
        case "newline"          => '\n'
        case s if s.length == 1 => s.charAt(0)
        case _                  => throw EvalError.at(startPos, "invalid character literal")

      Expr.Character(value, startPos)

    private def readVector(startPos: SourcePos): Expr =
      advance()
      advance()
      val items = ListBuffer.empty[Expr]
      skipTrivia()
      while index < input.length && input.charAt(index) != ')' do
        items += readExpr()
        skipTrivia()

      if index >= input.length then throw EvalError.at(startPos, "unterminated vector")

      advance()
      Expr.VectorExpr(items.toList, startPos)

    private def readAtom(startPos: SourcePos): Expr =
      val start = index
      while index < input.length && !isDelimiter(input.charAt(index)) do advance()

      val token = input.substring(start, index)
      SchemeNumber.parseToken(token) match
        case Some(number) => Expr.Number(number, startPos)
        case None         => Expr.Symbol(token, startPos)

    private def skipTrivia(): Unit =
      var keepSkipping = true
      while keepSkipping do
        while index < input.length && input.charAt(index).isWhitespace do advance()

        if index < input.length && input.charAt(index) == ';' then
          while index < input.length && input.charAt(index) != '\n' do advance()
        else keepSkipping = false

    private def startsWithToken(token: String): Boolean =
      input.startsWith(token, index) && {
        val boundary = index + token.length
        boundary >= input.length || isDelimiter(input.charAt(boundary))
      }

    private def isDelimiter(ch: Char): Boolean =
      ch.isWhitespace || ch == '\'' || ch == '`' || ch == ',' || ch == '(' || ch == ')' || ch == '"' || ch == ';'

    private def advance(): Char =
      val ch = input.charAt(index)
      index += 1
      if ch == '\n' then
        line += 1
        col = 1
      else col += 1
      ch

  private object Reader:
    def apply(input: String): Reader = new Reader(input)
