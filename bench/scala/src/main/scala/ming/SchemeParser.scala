package ming

import SchemeSyntax.Expr

import scala.annotation.tailrec

private[ming] object SchemeParser:

  def parseProgram(input: String): List[Expr] =
    parseExpressions(skipTrivia(ParseState(input)))

  @tailrec
  private def parseExpressions(state: ParseState, acc: List[Expr] = Nil): List[Expr] =
    if state.atEnd then acc.reverse
    else
      val (expr, nextState) = parseExpr(state)
      parseExpressions(skipTrivia(nextState), expr :: acc)

  private def parseExpr(state: ParseState): (Expr, ParseState) =
    if state.atEnd then fail("unexpected end of input")

    state.currentChar match
      case '(' =>
        parseList(state.advance())
      case ')' =>
        fail("unexpected ')'")
      case '"' =>
        val (value, nextState) = parseString(state.advance())
        (Expr.StringLiteral(value), nextState)
      case _ =>
        parseAtom(state)

  private def parseList(state: ParseState): (Expr, ParseState) =
    @tailrec
    def loop(current: ParseState, acc: List[Expr]): (Expr, ParseState) =
      if current.atEnd then fail("unterminated list")

      current.currentChar match
        case ')' =>
          (Expr.ListExpr(acc.reverse), current.advance())
        case _ =>
          val (expr, nextState) = parseExpr(current)
          loop(skipTrivia(nextState), expr :: acc)

    loop(skipTrivia(state), Nil)

  @tailrec
  private def parseString(state: ParseState, acc: List[Char] = Nil): (String, ParseState) =
    if state.atEnd then fail("unterminated string literal")

    state.currentChar match
      case '"' =>
        (acc.reverse.mkString, state.advance())
      case '\\' =>
        parseEscapedCharacter(state.advance(), acc)
      case char =>
        parseString(state.advance(), char :: acc)

  private def parseEscapedCharacter(state: ParseState, acc: List[Char]): (String, ParseState) =
    if state.atEnd then fail("unterminated escape sequence in string")

    val escaped = state.currentChar match
      case '"'  => '"'
      case '\\' => '\\'
      case 'n'  => '\n'
      case 'r'  => '\r'
      case 't'  => '\t'
      case other =>
        fail(s"unsupported escape sequence: \\$other")

    parseString(state.advance(), escaped :: acc)

  private def parseAtom(state: ParseState): (Expr, ParseState) =
    val endState = consumeToken(state)
    val token    = state.input.substring(state.index, endState.index)
    if token.isEmpty then fail("expected expression")

    (parseToken(token), endState)

  @tailrec
  private def consumeToken(state: ParseState): ParseState =
    if state.atEnd || isDelimiter(state.currentChar) then state
    else consumeToken(state.advance())

  @tailrec
  private def skipTrivia(state: ParseState): ParseState =
    if state.atEnd then state
    else
      state.currentChar match
        case char if char.isWhitespace =>
          skipTrivia(state.advance())
        case ';' =>
          skipTrivia(skipComment(state.advance()))
        case _ =>
          state

  @tailrec
  private def skipComment(state: ParseState): ParseState =
    if state.atEnd || state.currentChar == '\n' then state
    else skipComment(state.advance())

  private def parseToken(token: String): Expr =
    token match
      case "#t" => Expr.BooleanLiteral(true)
      case "#f" => Expr.BooleanLiteral(false)
      case integer if isIntegerLiteral(integer) =>
        Expr.IntegerLiteral(BigInt(integer))
      case symbol =>
        Expr.Symbol(symbol)

  private def isDelimiter(char: Char): Boolean =
    char.isWhitespace || char == '(' || char == ')' || char == ';'

  private def isIntegerLiteral(token: String): Boolean =
    val unsignedInteger =
      token.nonEmpty &&
        token != "+" &&
        token != "-" &&
        token.forall(_.isDigit)

    val signedInteger =
      token.length > 1 &&
        (token.head == '+' || token.head == '-') &&
        token.tail.forall(_.isDigit)

    unsignedInteger || signedInteger

  private def fail(message: String): Nothing =
    throw new EvalError(message)

  final private case class ParseState(input: String, index: Int = 0):

    def atEnd: Boolean =
      index >= input.length

    def currentChar: Char =
      input.charAt(index)

    def advance(amount: Int = 1): ParseState =
      copy(index = index + amount)
