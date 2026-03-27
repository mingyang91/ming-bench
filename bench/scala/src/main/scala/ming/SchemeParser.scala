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
      case '(' =>
        parseList(current.advance(), current.position)
      case ')' =>
        fail(current, "unexpected ')'")
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
        case _ if isIntegerToken(token) =>
          Expr.IntLit(token.toInt, pos)
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

  private def isIntegerToken(token: String): Boolean =
    if token.isEmpty then false
    else
      val digits =
        token.head match
          case '+' | '-' => token.drop(1)
          case _         => token
      digits.nonEmpty && digits.forall(_.isDigit)

  private def fail(cursor: Cursor, message: String): Nothing =
    throw EvalError.at(cursor.position, message)

  private case class Cursor(input: String, index: Int, line: Int, col: Int):

    def atEnd: Boolean =
      index >= input.length

    def currentChar: Char =
      input.charAt(index)

    def position: SourcePos =
      SourcePos(line, col)

    def advance(step: Int = 1): Cursor =
      if step <= 0 || atEnd then this
      else
        val next =
          currentChar match
            case '\n' => copy(index = index + 1, line = line + 1, col = 1)
            case _    => copy(index = index + 1, col = col + 1)
        next.advance(step - 1)
