package ming

import scala.annotation.tailrec

object Parser:

  private case class Cursor(input: String, offset: Int):
    def atEnd: Boolean = offset >= input.length

  def parseProgram(input: String): List[Expr] =
    val expressions = parseExpressions(skipWhitespace(Cursor(input, 0)), Nil)
    expressions match
      case Nil => throw EvalError.syntax("expected at least one expression")
      case _   => expressions

  @tailrec
  private def parseExpressions(cursor: Cursor, acc: List[Expr]): List[Expr] =
    val skipped = skipWhitespace(cursor)
    if skipped.atEnd then acc.reverse
    else
      val (expression, next) = parseExpr(skipped)
      parseExpressions(next, expression :: acc)

  private def parseExpr(cursor: Cursor): (Expr, Cursor) =
    if cursor.atEnd then throw EvalError.syntax("unexpected end of input")
    cursor.input.charAt(cursor.offset) match
      case '('  => parseList(cursor.copy(offset = cursor.offset + 1))
      case ')'  => throw EvalError.syntax("unexpected ')'")
      case '\'' => parseQuoted(cursor.copy(offset = cursor.offset + 1))
      case '"'  => parseString(cursor.copy(offset = cursor.offset + 1))
      case _    => parseAtom(cursor)

  private def parseQuoted(cursor: Cursor): (Expr, Cursor) =
    val (expression, next) = parseExpr(skipWhitespace(cursor))
    (Expr.ListExpr(List(Expr.Symbol("quote"), expression)), next)

  private def parseList(cursor: Cursor): (Expr, Cursor) =
    val (items, next) = parseListItems(skipWhitespace(cursor), Nil)
    (Expr.ListExpr(items), next)

  @tailrec
  private def parseListItems(cursor: Cursor, acc: List[Expr]): (List[Expr], Cursor) =
    val skipped = skipWhitespace(cursor)
    if skipped.atEnd then throw EvalError.syntax("unterminated list")
    else if skipped.input.charAt(skipped.offset) == ')' then (acc.reverse, skipped.copy(offset = skipped.offset + 1))
    else
      val (expression, next) = parseExpr(skipped)
      parseListItems(next, expression :: acc)

  private def parseString(cursor: Cursor): (Expr, Cursor) =
    val (chars, next) = parseStringChars(cursor, Nil)
    (Expr.StringLiteral(chars.reverse.mkString), next)

  @tailrec
  private def parseStringChars(cursor: Cursor, acc: List[Char]): (List[Char], Cursor) =
    if cursor.atEnd then throw EvalError.syntax("unterminated string")
    else
      cursor.input.charAt(cursor.offset) match
        case '"' =>
          (acc, cursor.copy(offset = cursor.offset + 1))
        case '\\' =>
          val (escaped, next) = parseEscapedChar(cursor.copy(offset = cursor.offset + 1))
          parseStringChars(next, escaped :: acc)
        case char =>
          parseStringChars(cursor.copy(offset = cursor.offset + 1), char :: acc)

  private def parseEscapedChar(cursor: Cursor): (Char, Cursor) =
    if cursor.atEnd then throw EvalError.syntax("unterminated string escape")
    else
      val escaped = cursor.input.charAt(cursor.offset) match
        case 'n'  => '\n'
        case 't'  => '\t'
        case '"'  => '"'
        case '\\' => '\\'
        case char => char
      (escaped, cursor.copy(offset = cursor.offset + 1))

  private def parseAtom(cursor: Cursor): (Expr, Cursor) =
    val end   = findTokenEnd(cursor.input, cursor.offset)
    val token = cursor.input.substring(cursor.offset, end)
    (parseToken(token), Cursor(cursor.input, end))

  private def parseToken(token: String): Expr = token match
    case "#t" => Expr.BooleanLiteral(true)
    case "#f" => Expr.BooleanLiteral(false)
    case _ if isIntegerToken(token) =>
      Expr.IntegerLiteral(BigInt(token))
    case _ =>
      Expr.Symbol(token)

  @tailrec
  private def skipWhitespace(cursor: Cursor): Cursor =
    if cursor.atEnd || !cursor.input.charAt(cursor.offset).isWhitespace then cursor
    else skipWhitespace(cursor.copy(offset = cursor.offset + 1))

  @tailrec
  private def findTokenEnd(input: String, offset: Int): Int =
    if offset >= input.length || isDelimiter(input.charAt(offset)) then offset
    else findTokenEnd(input, offset + 1)

  private def isDelimiter(char: Char): Boolean =
    char.isWhitespace || char == '(' || char == ')' || char == '\''

  private def isIntegerToken(token: String): Boolean =
    token match
      case ""                           => false
      case _ if token.forall(_.isDigit) => true
      case _ if token.length > 1 && Set('-', '+').contains(token.head) =>
        token.tail.forall(_.isDigit)
      case _ => false
