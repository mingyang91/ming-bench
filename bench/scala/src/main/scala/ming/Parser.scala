package ming

import scala.annotation.tailrec

case class Pos(line: Int, col: Int):
  override def toString: String = s"$line:$col"

object Pos:
  val none: Pos = Pos(0, 0)

/** Recursive-descent parser for Scheme s-expressions with position tracking. */
object Parser:

  private case class Token(text: String, pos: Pos)

  private case class ScanState(
    idx: Int,
    line: Int,
    col: Int,
    tokens: List[Token]
  )

  def parse(input: String): List[Value] =
    parseWithPos(input).map(_._1)

  def parseWithPos(input: String): List[(Value, Pos)] =
    val tokens = tokenize(input, ScanState(0, 1, 1, Nil))
    parseAll(tokens, Nil)

  // --- Tokenizer ---

  @tailrec
  private def tokenize(input: String, st: ScanState): List[Token] =
    if st.idx >= input.length then st.tokens.reverse
    else
      val ch = input.charAt(st.idx)
      ch match
        case '\n' =>
          tokenize(input, st.copy(idx = st.idx + 1, line = st.line + 1, col = 1))
        case '(' | ')' | '\'' =>
          val tok = Token(ch.toString, Pos(st.line, st.col))
          tokenize(
            input,
            st.copy(idx = st.idx + 1, col = st.col + 1, tokens = tok :: st.tokens)
          )
        case '"' =>
          readString(
            input,
            st.copy(idx = st.idx + 1, col = st.col + 1),
            Pos(st.line, st.col),
            new StringBuilder("\"")
          )
        case '#' if st.idx + 1 < input.length && input.charAt(st.idx + 1) == '\'' =>
          val tok = Token("#'", Pos(st.line, st.col))
          tokenize(input, st.copy(idx = st.idx + 2, col = st.col + 2, tokens = tok :: st.tokens))
        case ';' =>
          skipLineComment(input, st.copy(idx = st.idx + 1, col = st.col + 1))
        case c if c.isWhitespace =>
          tokenize(input, st.copy(idx = st.idx + 1, col = st.col + 1))
        case _ =>
          readAtom(input, st, st.idx, Pos(st.line, st.col))

  @tailrec
  private def skipLineComment(input: String, st: ScanState): List[Token] =
    if st.idx >= input.length then st.tokens.reverse
    else if input.charAt(st.idx) == '\n' then tokenize(input, st.copy(idx = st.idx + 1, line = st.line + 1, col = 1))
    else skipLineComment(input, st.copy(idx = st.idx + 1, col = st.col + 1))

  @tailrec
  private def readString(
    input: String,
    st: ScanState,
    startPos: Pos,
    buf: StringBuilder
  ): List[Token] =
    if st.idx >= input.length then throw new EvalError(s"$startPos: unterminated string")
    else
      val ch = input.charAt(st.idx)
      ch match
        case '"' =>
          buf.append('"')
          val tok = Token(buf.toString, startPos)
          tokenize(
            input,
            st.copy(idx = st.idx + 1, col = st.col + 1, tokens = tok :: st.tokens)
          )
        case '\\' if st.idx + 1 < input.length =>
          val next = input.charAt(st.idx + 1)
          buf.append('\\').append(next)
          val newSt =
            if next == '\n' then st.copy(idx = st.idx + 2, line = st.line + 1, col = 1)
            else st.copy(idx = st.idx + 2, col = st.col + 2)
          readString(input, newSt, startPos, buf)
        case '\n' =>
          buf.append(ch)
          readString(
            input,
            st.copy(idx = st.idx + 1, line = st.line + 1, col = 1),
            startPos,
            buf
          )
        case _ =>
          buf.append(ch)
          readString(
            input,
            st.copy(idx = st.idx + 1, col = st.col + 1),
            startPos,
            buf
          )

  @tailrec
  private def readAtom(
    input: String,
    st: ScanState,
    start: Int,
    startPos: Pos
  ): List[Token] =
    if st.idx >= input.length then
      val tok = Token(input.substring(start, st.idx), startPos)
      tokenize(input, st.copy(tokens = tok :: st.tokens))
    else
      val ch = input.charAt(st.idx)
      if ch.isWhitespace || ch == '(' || ch == ')' || ch == '"' || ch == ';' then
        val tok = Token(input.substring(start, st.idx), startPos)
        tokenize(input, st.copy(tokens = tok :: st.tokens))
      else readAtom(input, st.copy(idx = st.idx + 1, col = st.col + 1), start, startPos)

  // --- Parser ---

  @tailrec
  private def parseAll(
    tokens: List[Token],
    acc: List[(Value, Pos)]
  ): List[(Value, Pos)] = tokens match
    case Nil => acc.reverse
    case _ =>
      val (value, pos, rest) = parseExpr(tokens)
      parseAll(rest, (value, pos) :: acc)

  private def parseExpr(tokens: List[Token]): (Value, Pos, List[Token]) =
    tokens match
      case Nil => throw new EvalError("unexpected end of input")
      case Token("(", pos) :: rest =>
        val (value, remaining) = parseList(rest, Nil)
        (value, pos, remaining)
      case Token(")", pos) :: _ =>
        throw new EvalError(s"$pos: unexpected )")
      case Token("'", pos) :: rest =>
        val (quoted, _, remaining) = parseExpr(rest)
        (Value.SList(List(Value.Symbol("quote"), quoted)), pos, remaining)
      case Token("#'", pos) :: rest =>
        val (syntax, _, remaining) = parseExpr(rest)
        (Value.SList(List(Value.Symbol("syntax"), syntax)), pos, remaining)
      case Token(text, pos) :: rest =>
        (parseAtom(text), pos, rest)

  @tailrec
  private def parseList(
    tokens: List[Token],
    acc: List[Value]
  ): (Value, List[Token]) = tokens match
    case Nil                   => throw new EvalError("unterminated list")
    case Token(")", _) :: rest => (Value.SList(acc.reverse), rest)
    case _ =>
      val (value, _, remaining) = parseExpr(tokens)
      parseList(remaining, value :: acc)

  private def parseAtom(token: String): Value =
    if token == "#t" then Value.Bool(true)
    else if token == "#f" then Value.Bool(false)
    else if token.startsWith("\"") then Value.Str(token.substring(1, token.length - 1))
    else if token.startsWith("#\\") then parseCharLiteral(token)
    else
      token.toLongOption match
        case Some(n) => Value.Integer(n)
        case None    => parseNumericOrSymbol(token)

  private def parseNumericOrSymbol(token: String): Value =
    val slashIdx = token.indexOf('/')
    if slashIdx > 0 && slashIdx < token.length - 1 then
      val numStr = token.substring(0, slashIdx)
      val denStr = token.substring(slashIdx + 1)
      (numStr.toLongOption, denStr.toLongOption) match
        case (Some(num), Some(den)) => RationalOps.makeRational(num, den)
        case _                      => Value.Symbol(token)
    else
      try
        val d = token.toDouble
        if token.contains('.') then Value.Float(d)
        else Value.Symbol(token)
      catch case _: NumberFormatException => Value.Symbol(token)

  private def parseCharLiteral(token: String): Value =
    val rest = token.substring(2)
    rest match
      case "space"            => Value.Char(' ')
      case "newline"          => Value.Char('\n')
      case "tab"              => Value.Char('\t')
      case s if s.length == 1 => Value.Char(s.charAt(0))
      case _                  => throw new EvalError(s"bad character literal: $token")
