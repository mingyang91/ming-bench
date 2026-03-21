package ming

import scala.annotation.tailrec

/** Tokenizer and parser for Scheme expressions. */
object Parser:

  enum Token:
    case LParen
    case RParen
    case Quote
    case Atom(value: String)

  def parse(input: String): List[Value] =
    val tokens = tokenize(input)
    parseAll(tokens, List.empty)

  @tailrec
  private def parseAll(tokens: List[Token], acc: List[Value]): List[Value] =
    tokens match
      case Nil => acc
      case _ =>
        val (value, rest) = parseExpr(tokens)
        parseAll(rest, acc :+ value)

  private def parseExpr(tokens: List[Token]): (Value, List[Token]) =
    tokens match
      case Nil =>
        throw new EvalError("unexpected end of input")
      case Token.Quote :: rest =>
        val (value, remaining) = parseExpr(rest)
        (Value.PairVal(Value.Symbol("quote"), Value.PairVal(value, Value.NilVal)), remaining)
      case Token.LParen :: rest =>
        parseList(rest, List.empty)
      case Token.RParen :: _ =>
        throw new EvalError("unexpected )")
      case Token.Atom(s) :: rest =>
        (parseAtom(s), rest)

  @tailrec
  private def parseList(
    tokens: List[Token],
    acc: List[Value]
  ): (Value, List[Token]) =
    tokens match
      case Nil =>
        throw new EvalError("unexpected end of input, expected )")
      case Token.RParen :: rest =>
        val list = acc.foldRight(Value.NilVal: Value)(Value.PairVal(_, _))
        (list, rest)
      case _ =>
        val (value, rest) = parseExpr(tokens)
        parseList(rest, acc :+ value)

  private def parseAtom(s: String): Value =
    if s == "#t" then Value.BoolVal(true)
    else if s == "#f" then Value.BoolVal(false)
    else if s.startsWith("\"") && s.endsWith("\"") then Value.StringVal(s.substring(1, s.length - 1))
    else
      s.toLongOption match
        case Some(n) => Value.IntVal(n)
        case None    => Value.Symbol(s)

  private def tokenize(input: String): List[Token] =
    tokenizeLoop(input, 0, List.empty)

  private def tokenizeChar(
    input: String,
    pos: Int,
    ch: Char
  ): (Option[Token], Int) =
    ch match
      case _ if ch.isWhitespace => (None, pos + 1)
      case ';'                  => (None, skipLineComment(input, pos + 1))
      case '('                  => (Some(Token.LParen), pos + 1)
      case ')'                  => (Some(Token.RParen), pos + 1)
      case '\''                 => (Some(Token.Quote), pos + 1)
      case '"' =>
        val (str, next) = readString(input, pos + 1, pos + 1)
        (Some(Token.Atom("\"" + str + "\"")), next)
      case _ =>
        val end = readAtomEnd(input, pos)
        (Some(Token.Atom(input.substring(pos, end))), end)

  @tailrec
  private def tokenizeLoop(
    input: String,
    pos: Int,
    acc: List[Token]
  ): List[Token] =
    if pos >= input.length then acc
    else
      val (token, next) = tokenizeChar(input, pos, input.charAt(pos))
      token match
        case Some(t) => tokenizeLoop(input, next, acc :+ t)
        case None    => tokenizeLoop(input, next, acc)

  @tailrec
  private def skipLineComment(input: String, pos: Int): Int =
    if pos >= input.length || input.charAt(pos) == '\n' then pos
    else skipLineComment(input, pos + 1)

  @tailrec
  private def readString(
    input: String,
    pos: Int,
    start: Int
  ): (String, Int) =
    if pos >= input.length then throw new EvalError("unterminated string")
    else if input.charAt(pos) == '"' then (input.substring(start, pos), pos + 1)
    else if input.charAt(pos) == '\\' then readString(input, pos + 2, start)
    else readString(input, pos + 1, start)

  @tailrec
  private def readAtomEnd(input: String, pos: Int): Int =
    if pos >= input.length then pos
    else
      val ch = input.charAt(pos)
      if ch.isWhitespace || ch == '(' || ch == ')' || ch == ';' || ch == '"'
      then pos
      else readAtomEnd(input, pos + 1)
