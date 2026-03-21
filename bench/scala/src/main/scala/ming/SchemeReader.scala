package ming

import scala.annotation.tailrec

object SchemeReader:

  private enum Token:
    case LeftParen(position: SourcePos)
    case RightParen(position: SourcePos)
    case Quote(position: SourcePos)
    case Atom(value: String, position: SourcePos)
    case Str(value: String, position: SourcePos)

  final private case class Cursor(input: String, offset: Int, line: Int, column: Int):

    def atEnd: Boolean =
      offset >= input.length

    def position: SourcePos =
      SourcePos(line, column)

    def current: Char =
      input.charAt(offset)

    def advance: Cursor =
      if current == '\n' then copy(offset = offset + 1, line = line + 1, column = 1)
      else copy(offset = offset + 1, column = column + 1)

  def readAll(input: String): List[Expr] =
    val expressions = parseAll(tokenize(input))
    if expressions.isEmpty then throw EvalError("expected at least one expression")
    expressions

  private def tokenize(input: String): List[Token] =
    @tailrec
    def loop(cursor: Cursor, acc: List[Token]): List[Token] =
      if cursor.atEnd then acc.reverse
      else
        cursor.current match
          case char if char.isWhitespace =>
            loop(cursor.advance, acc)
          case '(' =>
            loop(cursor.advance, Token.LeftParen(cursor.position) :: acc)
          case ')' =>
            loop(cursor.advance, Token.RightParen(cursor.position) :: acc)
          case '\'' =>
            loop(cursor.advance, Token.Quote(cursor.position) :: acc)
          case '"' =>
            val start              = cursor.position
            val (nextCursor, body) = readString(cursor.advance, start, Nil)
            loop(nextCursor, Token.Str(body, start) :: acc)
          case _ =>
            val (nextCursor, atom) = readAtom(cursor, Nil)
            loop(nextCursor, Token.Atom(atom, cursor.position) :: acc)

    loop(Cursor(input, offset = 0, line = 1, column = 1), Nil)

  @tailrec
  private def readAtom(cursor: Cursor, acc: List[Char]): (Cursor, String) =
    if cursor.atEnd || isDelimiter(cursor.current) then (cursor, acc.reverse.mkString)
    else readAtom(cursor.advance, cursor.current :: acc)

  private def readString(cursor: Cursor, start: SourcePos, acc: List[Char]): (Cursor, String) =
    if cursor.atEnd then throw EvalError.at(start, "unterminated string")
    else
      cursor.current match
        case '"' =>
          (cursor.advance, acc.reverse.mkString)
        case '\\' =>
          readEscaped(cursor.advance, start, acc)
        case char =>
          readString(cursor.advance, start, char :: acc)

  private def readEscaped(cursor: Cursor, start: SourcePos, acc: List[Char]): (Cursor, String) =
    if cursor.atEnd then throw EvalError.at(start, "unterminated string")
    else
      val escaped = cursor.current match
        case '"'  => '"'
        case '\\' => '\\'
        case 'n'  => '\n'
        case 't'  => '\t'
        case char => char

      readString(cursor.advance, start, escaped :: acc)

  private def parseAll(tokens: List[Token]): List[Expr] =
    @tailrec
    def loop(remaining: List[Token], acc: List[Expr]): List[Expr] =
      remaining match
        case Nil => acc.reverse
        case _ =>
          val (expr, rest) = parseExpr(remaining)
          loop(rest, expr :: acc)

    loop(tokens, Nil)

  private def parseExpr(tokens: List[Token]): (Expr, List[Token]) = tokens match
    case Nil =>
      throw EvalError("unexpected end of input")
    case Token.LeftParen(position) :: rest =>
      val (items, remaining) = parseList(rest, position, Nil)
      (Expr.ListExpr(items, position), remaining)
    case Token.RightParen(position) :: _ =>
      throw EvalError.at(position, "unexpected ')'")
    case Token.Quote(position) :: rest =>
      val (quotedExpr, remaining) = parseExpr(rest)
      (
        Expr.ListExpr(
          List(Expr.Symbol("quote", position), quotedExpr),
          position
        ),
        remaining
      )
    case Token.Str(value, position) :: rest =>
      (Expr.Literal(Value.Str(value), position), rest)
    case Token.Atom(value, position) :: rest =>
      (parseAtom(value, position), rest)

  private def parseList(tokens: List[Token], start: SourcePos, acc: List[Expr]): (List[Expr], List[Token]) =
    tokens match
      case Nil =>
        throw EvalError.at(start, "unterminated list")
      case Token.RightParen(_) :: rest =>
        (acc.reverse, rest)
      case _ =>
        val (expr, remaining) = parseExpr(tokens)
        parseList(remaining, start, expr :: acc)

  private def parseAtom(value: String, position: SourcePos): Expr =
    value match
      case "#t" => Expr.Literal(Value.Bool(true), position)
      case "#f" => Expr.Literal(Value.Bool(false), position)
      case _ =>
        parseInteger(value) match
          case Some(number) => Expr.Literal(Value.Number(number), position)
          case None         => Expr.Symbol(value, position)

  private def parseInteger(value: String): Option[BigInt] =
    if value.nonEmpty && value.forall(_.isDigit) then Some(BigInt(value))
    else if value.startsWith("-") && value.length > 1 && value.tail.forall(_.isDigit) then Some(BigInt(value))
    else None

  private def isDelimiter(char: Char): Boolean =
    char.isWhitespace || char == '(' || char == ')'
