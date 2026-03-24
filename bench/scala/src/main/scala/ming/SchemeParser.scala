package ming

import scala.annotation.tailrec

private[ming] object SchemeParser:

  private case class Cursor(
    source: String,
    index: Int = 0,
    line: Int = 1,
    column: Int = 1
  ):
    def atEnd: Boolean = index >= source.length

    def currentChar: Char =
      source.charAt(index)

    def advance: Cursor =
      if atEnd then this
      else if currentChar == '\n' then copy(index = index + 1, line = line + 1, column = 1)
      else copy(index = index + 1, column = column + 1)

  def parseProgram(input: String): List[Expr] =
    @tailrec
    def loop(cursor: Cursor, parsed: List[Expr]): List[Expr] =
      val next = skipWhitespace(cursor)
      if next.atEnd then parsed.reverse
      else
        val (expr, rest) = parseExpr(next)
        loop(rest, expr :: parsed)

    loop(Cursor(input), Nil)

  private def parseExpr(cursor: Cursor): (Expr, Cursor) =
    if cursor.atEnd then fail(cursor, "unexpected end of input")
    else
      cursor.currentChar match
        case '('  => parseList(cursor.advance, Nil)
        case '"'  => parseString(cursor.advance, new StringBuilder)
        case '\'' => parseQuoted(cursor.advance)
        case ')'  => fail(cursor, "unexpected )")
        case _    => parseAtom(cursor)

  private def parseQuoted(cursor: Cursor): (Expr, Cursor) =
    val next               = skipWhitespace(cursor)
    val (quotedExpr, rest) = parseExpr(next)
    (Expr.ListExpr(List(Expr.Symbol("quote"), quotedExpr)), rest)

  @tailrec
  private def parseList(cursor: Cursor, elementsReversed: List[Expr]): (Expr, Cursor) =
    val next = skipWhitespace(cursor)
    if next.atEnd then fail(next, "unterminated list")
    else if next.currentChar == ')' then (Expr.ListExpr(elementsReversed.reverse), next.advance)
    else
      val (expr, rest) = parseExpr(next)
      parseList(rest, expr :: elementsReversed)

  @tailrec
  private def parseString(
    cursor: Cursor,
    builder: StringBuilder
  ): (Expr, Cursor) =
    if cursor.atEnd then fail(cursor, "unterminated string")
    else
      cursor.currentChar match
        case '"' =>
          (Expr.StringLiteral(builder.result()), cursor.advance)
        case '\\' =>
          val escapedCursor = cursor.advance
          if escapedCursor.atEnd then fail(escapedCursor, "unterminated string escape")
          else
            val escapedChar = escapedCursor.currentChar match
              case '"'  => '"'
              case '\\' => '\\'
              case 'n'  => '\n'
              case 'r'  => '\r'
              case 't'  => '\t'
              case other =>
                fail(escapedCursor, s"unsupported string escape: \\$other")
            builder.append(escapedChar)
            parseString(escapedCursor.advance, builder)
        case other =>
          builder.append(other)
          parseString(cursor.advance, builder)

  private def parseAtom(cursor: Cursor): (Expr, Cursor) =
    val (token, rest) = readToken(cursor, new StringBuilder)
    token match
      case "#t" => (Expr.BooleanLiteral(true), rest)
      case "#f" => (Expr.BooleanLiteral(false), rest)
      case integer if isIntegerToken(integer) =>
        (Expr.IntegerLiteral(integer.toInt), rest)
      case symbol if symbol.nonEmpty =>
        (Expr.Symbol(symbol), rest)
      case _ =>
        fail(cursor, "expected expression")

  @tailrec
  private def readToken(
    cursor: Cursor,
    builder: StringBuilder
  ): (String, Cursor) =
    if cursor.atEnd || isDelimiter(cursor.currentChar) then (builder.result(), cursor)
    else
      builder.append(cursor.currentChar)
      readToken(cursor.advance, builder)

  @tailrec
  private def skipWhitespace(cursor: Cursor): Cursor =
    if cursor.atEnd then cursor
    else if cursor.currentChar.isWhitespace then skipWhitespace(cursor.advance)
    else if cursor.currentChar == ';' then skipWhitespace(skipComment(cursor))
    else cursor

  @tailrec
  private def skipComment(cursor: Cursor): Cursor =
    if cursor.atEnd || cursor.currentChar == '\n' then cursor
    else skipComment(cursor.advance)

  private def isDelimiter(char: Char): Boolean =
    char.isWhitespace || char == '(' || char == ')'

  private def isIntegerToken(token: String): Boolean =
    token.nonEmpty && token != "+" && token != "-" &&
      token.zipWithIndex.forall { (char, index) =>
        char.isDigit || index == 0 && (char == '+' || char == '-')
      }

  private def fail(cursor: Cursor, message: String): Nothing =
    throw new EvalError(s"$message at ${cursor.line}:${cursor.column}")
