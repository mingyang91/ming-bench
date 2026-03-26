package ming

import scala.annotation.tailrec

private[ming] object SchemeParser:

  def parseProgram(input: String): List[Expr] =
    parseExpressions(State(input), Nil)._2.reverse

  @tailrec
  private def parseExpressions(state0: State, expressions: List[Expr]): (State, List[Expr]) =
    val state = skipIgnorable(state0)
    if state.atEnd then (state, expressions)
    else
      val (nextState, expression) = parseExpr(state)
      parseExpressions(nextState, expression :: expressions)

  private def parseExpr(state0: State): (State, Expr) =
    val state = skipIgnorable(state0)
    if state.atEnd then SchemeFailure.raise("unexpected end of input", state.position)

    val start = state.position
    state.currentChar match
      case '(' =>
        parseList(advance(state), start)
      case ')' =>
        SchemeFailure.raise("unexpected ')'", start)
      case '"' =>
        parseString(advance(state), start)
      case _ =>
        parseAtom(state, start)

  private def parseList(state0: State, start: Position): (State, Expr) =
    @tailrec
    def loop(state1: State, items: List[Expr]): (State, List[Expr]) =
      val state = skipIgnorable(state1)
      if state.atEnd then SchemeFailure.raise("unterminated list", start)
      else if state.currentChar == ')' then (advance(state), items)
      else
        val (nextState, item) = parseExpr(state)
        loop(nextState, item :: items)

    val (nextState, items) = loop(state0, Nil)
    (nextState, ListExpr(items.reverse, start))

  private def parseString(state0: State, start: Position): (State, Expr) =
    @tailrec
    def loop(state: State, characters: List[Char]): (State, List[Char]) =
      if state.atEnd then SchemeFailure.raise("unterminated string", start)

      state.currentChar match
        case '"' =>
          (advance(state), characters)
        case '\\' =>
          val (nextState, escaped) = parseEscapedCharacter(advance(state), start)
          loop(nextState, escaped :: characters)
        case ch =>
          loop(advance(state), ch :: characters)

    val (nextState, characters) = loop(state0, Nil)
    (nextState, StringExpr(characters.reverse.mkString, start))

  private def parseEscapedCharacter(state: State, start: Position): (State, Char) =
    if state.atEnd then SchemeFailure.raise("unterminated string", start)

    val escaped =
      state.currentChar match
        case '"'  => '"'
        case '\\' => '\\'
        case 'n'  => '\n'
        case 'r'  => '\r'
        case 't'  => '\t'
        case other =>
          SchemeFailure.raise(s"unsupported escape sequence: \\$other", state.position)

    (advance(state), escaped)

  private def parseAtom(state0: State, start: Position): (State, Expr) =
    @tailrec
    def loop(state: State, characters: List[Char]): (State, String) =
      if state.atEnd || isDelimiter(state.currentChar) then (state, characters.reverse.mkString)
      else loop(advance(state), state.currentChar :: characters)

    val (nextState, token) = loop(state0, Nil)
    val expression =
      token match
        case "#t" => BoolExpr(true, start)
        case "#f" => BoolExpr(false, start)
        case _ if token.matches("[+-]?\\d+") =>
          IntExpr(BigInt(token), start)
        case _ =>
          SymbolExpr(token, start)

    (nextState, expression)

  @tailrec
  private def skipIgnorable(state0: State): State =
    val state = skipWhitespace(state0)
    if !state.atEnd && state.currentChar == ';' then skipIgnorable(skipComment(state))
    else state

  @tailrec
  private def skipWhitespace(state: State): State =
    if !state.atEnd && state.currentChar.isWhitespace then skipWhitespace(advance(state))
    else state

  @tailrec
  private def skipComment(state: State): State =
    if !state.atEnd && state.currentChar != '\n' then skipComment(advance(state))
    else state

  private def isDelimiter(ch: Char): Boolean =
    ch.isWhitespace || ch == '(' || ch == ')' || ch == ';'

  private def advance(state: State): State =
    if state.atEnd then state
    else
      val nextIndex = state.index + 1
      state.currentChar match
        case '\n' =>
          state.copy(index = nextIndex, line = state.line + 1, column = 1)
        case _ =>
          state.copy(index = nextIndex, column = state.column + 1)

  final private case class State(
    input: String,
    index: Int = 0,
    line: Int = 1,
    column: Int = 1
  ):
    def atEnd: Boolean     = index >= input.length
    def currentChar: Char  = input.charAt(index)
    def position: Position = Position(line, column)
