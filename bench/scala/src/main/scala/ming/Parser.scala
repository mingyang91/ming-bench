package ming

import scala.annotation.tailrec

/** Tokenizer and parser for Scheme expressions. */
object Parser:

  enum Token:
    case LParen(line: Int, col: Int)
    case RParen(line: Int, col: Int)
    case Quote(line: Int, col: Int)
    case Atom(value: String, line: Int, col: Int)

  def parse(input: String): List[Value] =
    val tokens = tokenize(input)
    parseAll(tokens, List.empty)

  @tailrec
  private def parseAll(
    tokens: List[Token],
    acc: List[Value]
  ): List[Value] =
    tokens match
      case Nil => acc
      case _ =>
        val (value, rest) = parseExpr(tokens)
        parseAll(rest, acc :+ value)

  private def parseExpr(
    tokens: List[Token]
  ): (Value, List[Token]) =
    tokens match
      case Nil =>
        throw new EvalError("unexpected end of input")
      case Token.Quote(l, c) :: rest =>
        val (value, remaining) = parseExpr(rest)
        val inner              = Value.PairVal(value, Value.NilVal)
        val outer =
          Value.PairVal(Value.Symbol("quote"), inner, Some((l, c)))
        (outer, remaining)
      case Token.LParen(l, c) :: rest =>
        parseList(rest, List.empty, (l, c))
      case Token.RParen(_, _) :: _ =>
        throw new EvalError("unexpected )")
      case Token.Atom(s, l, c) :: rest =>
        (parseAtom(s, l, c), rest)

  private def parseList(
    tokens: List[Token],
    acc: List[Value],
    startPos: (Int, Int)
  ): (Value, List[Token]) =
    tokens match
      case Nil =>
        throw new EvalError("unexpected end of input, expected )")
      case Token.RParen(_, _) :: rest =>
        val list =
          acc.foldRight(Value.NilVal: Value)((h, t) => Value.PairVal(h, t))
        val result = list match
          case Value.PairVal(car, cdr, _) =>
            Value.PairVal(car, cdr, Some(startPos))
          case other => other
        (result, rest)
      case Token.Atom(".", _, _) :: rest =>
        val (cdrVal, rest2) = parseExpr(rest)
        rest2 match
          case Token.RParen(_, _) :: rest3 =>
            val list =
              acc.foldRight(cdrVal)((h, t) => Value.PairVal(h, t))
            val result = list match
              case Value.PairVal(car, cdr, _) =>
                Value.PairVal(car, cdr, Some(startPos))
              case other => other
            (result, rest3)
          case _ =>
            throw new EvalError("expected ) after dot notation")
      case _ =>
        val (value, rest) = parseExpr(tokens)
        parseList(rest, acc :+ value, startPos)

  private def parseAtom(s: String, line: Int, col: Int): Value =
    if s == "#t" then Value.BoolVal(true)
    else if s == "#f" then Value.BoolVal(false)
    else if s.startsWith("#\\") then parseCharLiteral(s.substring(2))
    else if s.startsWith("\"") && s.endsWith("\"") then Value.StringVal(s.substring(1, s.length - 1))
    else
      s.toLongOption match
        case Some(n) => Value.IntVal(n)
        case None    => Value.Symbol(s, Some((line, col)))

  private def parseCharLiteral(name: String): Value =
    val ch = name match
      case "space"            => ' '
      case "newline"          => '\n'
      case "tab"              => '\t'
      case s if s.length == 1 => s.charAt(0)
      case _                  => throw new EvalError(s"unknown character name: $name")
    Value.CharVal(ch)

  private def posOf(input: String, offset: Int): (Int, Int) =
    input.take(offset).foldLeft((1, 1)) {
      case ((line, _), '\n') => (line + 1, 1)
      case ((line, col), _)  => (line, col + 1)
    }

  private def tokenize(input: String): List[Token] =
    tokenizeLoop(input, 0, List.empty)

  private def tokenizeChar(
    input: String,
    offset: Int,
    ch: Char
  ): (Option[Token], Int) =
    val (line, col) = posOf(input, offset)
    ch match
      case _ if ch.isWhitespace => (None, offset + 1)
      case ';'                  => (None, skipLineComment(input, offset + 1))
      case '('                  => (Some(Token.LParen(line, col)), offset + 1)
      case ')'                  => (Some(Token.RParen(line, col)), offset + 1)
      case '\''                 => (Some(Token.Quote(line, col)), offset + 1)
      case '"' =>
        val (str, next) = readString(input, offset + 1, offset + 1)
        (Some(Token.Atom("\"" + str + "\"", line, col)), next)
      case _ =>
        val end = readAtomEnd(input, offset)
        val tok = Token.Atom(input.substring(offset, end), line, col)
        (Some(tok), end)

  @tailrec
  private def tokenizeLoop(
    input: String,
    offset: Int,
    acc: List[Token]
  ): List[Token] =
    if offset >= input.length then acc
    else
      val (token, next) =
        tokenizeChar(input, offset, input.charAt(offset))
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
