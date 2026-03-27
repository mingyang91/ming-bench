package ming

import scala.annotation.tailrec

private[ming] object SchemeParser:

  def parseProgram(input: String): List[Expr] =
    val (_, expressions) = parseExpressions(skipIgnored(Cursor(input, 0, 1, 1)), Nil)
    expressions.reverse

  @tailrec
  private def parseExpressions(cursor: Cursor, acc: List[Expr]): (Cursor, List[Expr]) =
    val current = skipIgnored(cursor)
    if current.atEnd then (current, acc)
    else
      val (next, expr) = parseExpr(current)
      parseExpressions(next, expr :: acc)

  private def parseExpr(cursor: Cursor): (Cursor, Expr) =
    val current = skipIgnored(cursor)
    if current.atEnd then fail(current, "unexpected end of input")

    current.currentChar match
      case '#' if current.peekChar().contains('\'') =>
        val syntaxPos    = current.position
        val (next, expr) = parseExpr(current.advance(2))
        (next, Expr.ListExpr(List(Expr.Symbol("syntax", syntaxPos), expr), syntaxPos))
      case '#' if current.peekChar().contains('(') =>
        parseVector(current.advance(2), current.position)
      case '(' =>
        parseList(current.advance(), current.position)
      case ')' =>
        fail(current, "unexpected ')'")
      case '`' =>
        val quasiquotePos = current.position
        val (next, expr)  = parseExpr(current.advance())
        (next, Expr.ListExpr(List(Expr.Symbol("quasiquote", quasiquotePos), expr), quasiquotePos))
      case ',' if current.peekChar().contains('@') =>
        val unquotePos   = current.position
        val (next, expr) = parseExpr(current.advance(2))
        (next, Expr.ListExpr(List(Expr.Symbol("unquote-splicing", unquotePos), expr), unquotePos))
      case ',' =>
        val unquotePos   = current.position
        val (next, expr) = parseExpr(current.advance())
        (next, Expr.ListExpr(List(Expr.Symbol("unquote", unquotePos), expr), unquotePos))
      case '\'' =>
        val quotePos     = current.position
        val (next, expr) = parseExpr(current.advance())
        (next, Expr.ListExpr(List(Expr.Symbol("quote", quotePos), expr), quotePos))
      case '"' =>
        val stringPos     = current.position
        val (next, value) = parseString(current.advance())
        (next, Expr.StringLit(value, stringPos))
      case _ =>
        parseAtom(current)

  @tailrec
  private def parseList(cursor: Cursor, startPos: SourcePos, acc: List[Expr] = Nil): (Cursor, Expr) =
    val current = skipIgnored(cursor)
    if current.atEnd then fail(current, "unterminated list")

    current.currentChar match
      case ')' =>
        (current.advance(), Expr.ListExpr(acc.reverse, startPos))
      case _ =>
        val (next, expr) = parseExpr(current)
        parseList(next, startPos, expr :: acc)

  @tailrec
  private def parseVector(cursor: Cursor, startPos: SourcePos, acc: List[Expr] = Nil): (Cursor, Expr) =
    val current = skipIgnored(cursor)
    if current.atEnd then fail(current, "unterminated vector")

    current.currentChar match
      case ')' =>
        (current.advance(), Expr.VectorExpr(acc.reverse, startPos))
      case _ =>
        val (next, expr) = parseExpr(current)
        parseVector(next, startPos, expr :: acc)

  private def parseString(cursor: Cursor): (Cursor, String) =
    @tailrec
    def loop(current: Cursor, acc: List[Char]): (Cursor, String) =
      if current.atEnd then fail(current, "unterminated string literal")

      current.currentChar match
        case '"' =>
          (current.advance(), acc.reverse.mkString)
        case '\\' =>
          val escaped = current.advance()
          if escaped.atEnd then fail(escaped, "unterminated escape sequence")
          loop(escaped.advance(), decodeEscape(escaped.currentChar) :: acc)
        case ch =>
          loop(current.advance(), ch :: acc)

    loop(cursor, Nil)

  private def decodeEscape(ch: Char): Char =
    ch match
      case '"'   => '"'
      case '\\'  => '\\'
      case 'n'   => '\n'
      case 'r'   => '\r'
      case 't'   => '\t'
      case other => other

  private def parseAtom(cursor: Cursor): (Cursor, Expr) =
    val pos   = cursor.position
    val next  = advanceWhile(cursor)(ch => !isDelimiter(ch))
    val token = cursor.input.substring(cursor.index, next.index)
    val expr =
      token match
        case "#t" => Expr.BoolLit(true, pos)
        case "#f" => Expr.BoolLit(false, pos)
        case _ if isCharToken(token) =>
          Expr.CharLit(parseCharToken(token, pos), pos)
        case _ if SchemeNumber.isNumericToken(token) =>
          SchemeNumber
            .parseToken(token)
            .map(_.toExpr(pos))
            .getOrElse(throw EvalError.at(pos, s"invalid number literal: $token"))
        case _ if token.nonEmpty =>
          Expr.Symbol(token, pos)
        case _ =>
          fail(cursor, "expected expression")

    (next, expr)

  @tailrec
  private def skipIgnored(cursor: Cursor): Cursor =
    if cursor.atEnd then cursor
    else
      cursor.currentChar match
        case ch if ch.isWhitespace =>
          skipIgnored(cursor.advance())
        case ';' =>
          skipIgnored(skipComment(cursor.advance()))
        case _ =>
          cursor

  @tailrec
  private def skipComment(cursor: Cursor): Cursor =
    if cursor.atEnd || cursor.currentChar == '\n' then cursor
    else skipComment(cursor.advance())

  @tailrec
  private def advanceWhile(cursor: Cursor)(predicate: Char => Boolean): Cursor =
    if cursor.atEnd || !predicate(cursor.currentChar) then cursor
    else advanceWhile(cursor.advance())(predicate)

  private def isDelimiter(ch: Char): Boolean =
    ch.isWhitespace || ch == '(' || ch == ')' || ch == ';'

  private def isCharToken(token: String): Boolean =
    token.startsWith("#\\") && token.length > 2

  private def parseCharToken(token: String, pos: SourcePos): Char =
    token.drop(2) match
      case "space"   => ' '
      case "newline" => '\n'
      case value if value.length == 1 =>
        value.charAt(0)
      case _ =>
        throw EvalError.at(pos, s"invalid character literal: $token")

  private def fail(cursor: Cursor, message: String): Nothing =
    throw EvalError.at(cursor.position, message)

  private case class Cursor(input: String, index: Int, line: Int, col: Int):

    def atEnd: Boolean =
      index >= input.length

    def currentChar: Char =
      input.charAt(index)

    def position: SourcePos =
      SourcePos(line, col)

    def peekChar(offset: Int = 1): Option[Char] =
      val targetIndex = index + offset
      Option.when(targetIndex < input.length)(input.charAt(targetIndex))

    def advance(step: Int = 1): Cursor =
      if step <= 0 || atEnd then this
      else
        val next =
          currentChar match
            case '\n' => copy(index = index + 1, line = line + 1, col = 1)
            case _    => copy(index = index + 1, col = col + 1)
        next.advance(step - 1)
