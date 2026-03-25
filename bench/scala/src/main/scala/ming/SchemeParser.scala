package ming

import scala.annotation.tailrec

private[ming] object SchemeParser:

  def parseProgram(input: String): List[Expr] =
    val (expressions, _) = parseProgram(skipTrivia(ParserState(input)), Nil)
    expressions.reverse

  @tailrec
  private def parseProgram(
    state: ParserState,
    acc: List[Expr]
  ): (List[Expr], ParserState) =
    if state.atEnd then (acc, state)
    else
      val (expression, nextState) = parseExpr(state)
      parseProgram(skipTrivia(nextState), expression :: acc)

  private def parseExpr(state: ParserState): (Expr, ParserState) =
    val trimmed = skipTrivia(state)
    if trimmed.atEnd then fail(trimmed.currentPos, "unexpected end of input")

    val start = trimmed.currentPos
    trimmed.currentChar match
      case '\'' =>
        parseQuote(start, trimmed.advance)

      case '(' =>
        parseList(start, trimmed.advance)

      case ')' =>
        fail(start, "unexpected ')'")

      case '"' =>
        parseString(start, trimmed)

      case '#' =>
        parseHashLiteral(start, trimmed)

      case _ if startsNumber(trimmed) =>
        parseNumber(start, trimmed)

      case _ =>
        parseSymbol(start, trimmed)

  private def parseQuote(start: SourcePos, state: ParserState): (Expr, ParserState) =
    val (quoted, nextState) = parseExpr(state)
    (Expr.ListExpr(List(Expr.Symbol("quote", start), quoted), start), nextState)

  private def parseList(start: SourcePos, state: ParserState): (Expr, ParserState) =
    val (items, nextState) = parseListItems(start, skipTrivia(state), Nil)
    (Expr.ListExpr(items, start), nextState)

  @tailrec
  private def parseListItems(
    start: SourcePos,
    state: ParserState,
    acc: List[Expr]
  ): (List[Expr], ParserState) =
    if state.atEnd then fail(start, "unterminated list")
    else if state.currentChar == ')' then (acc.reverse, state.advance)
    else
      val (expr, afterExpr) = parseExpr(state)
      parseListItems(start, skipTrivia(afterExpr), expr :: acc)

  private def parseString(start: SourcePos, state: ParserState): (Expr, ParserState) =
    val (value, nextState) = parseStringChars(start, state.advance, new StringBuilder)
    (Expr.StringAtom(value, start), nextState)

  @tailrec
  private def parseStringChars(
    start: SourcePos,
    state: ParserState,
    builder: StringBuilder
  ): (String, ParserState) =
    if state.atEnd then fail(start, "unterminated string literal")
    else
      state.currentChar match
        case '"' =>
          (builder.toString, state.advance)

        case '\\' =>
          val afterSlash = state.advance
          if afterSlash.atEnd then fail(start, "unterminated string literal")

          builder.append(decodeEscape(afterSlash.currentChar))
          parseStringChars(start, afterSlash.advance, builder)

        case ch =>
          builder.append(ch)
          parseStringChars(start, state.advance, builder)

  private def parseHashLiteral(start: SourcePos, state: ParserState): (Expr, ParserState) =
    if state.startsWith("#t") && isTokenBoundary(state, 2) then (Expr.BoolAtom(true, start), advanceBy(state, 2))
    else if state.startsWith("#f") && isTokenBoundary(state, 2) then (Expr.BoolAtom(false, start), advanceBy(state, 2))
    else if state.startsWith("#\\") then parseChar(start, advanceBy(state, 2))
    else fail(start, "unknown # literal")

  private def parseChar(start: SourcePos, state: ParserState): (Expr, ParserState) =
    val (token, nextState) = readToken(state)
    val value =
      token match
        case "" =>
          fail(start, "unterminated character literal")

        case "space" =>
          ' '

        case "newline" =>
          '\n'

        case single if single.length == 1 =>
          single.head

        case _ =>
          fail(start, s"unsupported character literal: #\\$token")

    (Expr.CharAtom(value, start), nextState)

  private def parseNumber(start: SourcePos, state: ParserState): (Expr, ParserState) =
    val (token, nextState) = readToken(state)
    val expr =
      SchemeNumber.parseLiteral(token) match
        case Right(SchemeNumber.Exact(numerator, denominator)) if denominator == 1 =>
          Expr.IntAtom(numerator, start)

        case Right(SchemeNumber.Exact(numerator, denominator)) =>
          Expr.RationalAtom(numerator, denominator, start)

        case Right(SchemeNumber.Inexact(value)) =>
          Expr.InexactAtom(value, start)

        case Left(message) =>
          fail(start, message)

    (expr, nextState)

  private def parseSymbol(start: SourcePos, state: ParserState): (Expr, ParserState) =
    val (token, nextState) = readToken(state)
    if token.isEmpty then fail(start, "expected symbol")

    (Expr.Symbol(token, start), nextState)

  @tailrec
  private def readToken(
    state: ParserState,
    acc: List[Char] = Nil
  ): (String, ParserState) =
    if state.atEnd || isDelimiter(state.currentChar) then (acc.reverse.mkString, state)
    else
      val ch = state.currentChar
      readToken(state.advance, ch :: acc)

  @tailrec
  private def skipTrivia(state: ParserState): ParserState =
    if state.atEnd then state
    else if state.currentChar.isWhitespace then skipTrivia(skipWhitespace(state))
    else if state.currentChar == ';' then skipTrivia(skipComment(state))
    else state

  @tailrec
  private def skipWhitespace(state: ParserState): ParserState =
    if state.atEnd || !state.currentChar.isWhitespace then state
    else skipWhitespace(state.advance)

  @tailrec
  private def skipComment(state: ParserState): ParserState =
    if state.atEnd || state.currentChar == '\n' then state
    else skipComment(state.advance)

  @tailrec
  private def advanceBy(state: ParserState, count: Int): ParserState =
    if count <= 0 then state
    else advanceBy(state.advance, count - 1)

  private def startsNumber(state: ParserState): Boolean =
    state.currentChar match
      case '+' | '-' =>
        state.peek(1).exists(_.isDigit) ||
          (state.peek(1).contains('.') && state.peek(2).exists(_.isDigit))

      case '.' =>
        state.peek(1).exists(_.isDigit)

      case ch =>
        ch.isDigit

  private def isTokenBoundary(state: ParserState, offset: Int): Boolean =
    state.peek(offset).forall(isDelimiter)

  private def isDelimiter(ch: Char): Boolean =
    ch.isWhitespace || ch == '\'' || ch == '(' || ch == ')' || ch == '"' || ch == ';'

  private def decodeEscape(ch: Char): Char =
    ch match
      case '\\' =>
        '\\'

      case '"' =>
        '"'

      case 'n' =>
        '\n'

      case 'r' =>
        '\r'

      case 't' =>
        '\t'

      case other =>
        other

  private def fail[A](pos: SourcePos, message: String): A =
    throw EvalError.at(pos, message)

final private case class ParserState(
  input: String,
  index: Int = 0,
  line: Int = 1,
  col: Int = 1
):

  def currentPos: SourcePos =
    SourcePos(line, col)

  def atEnd: Boolean =
    index >= input.length

  def currentChar: Char =
    input.charAt(index)

  def peek(offset: Int): Option[Char] =
    val target = index + offset
    Option.when(target >= 0 && target < input.length)(input.charAt(target))

  def startsWith(prefix: String): Boolean =
    input.regionMatches(index, prefix, 0, prefix.length)

  def advance: ParserState =
    if currentChar == '\n' then copy(index = index + 1, line = line + 1, col = 1)
    else copy(index = index + 1, col = col + 1)
