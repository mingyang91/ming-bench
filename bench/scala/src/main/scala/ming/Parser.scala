package ming

import SchemeValue.*

object Parser:

  private case class Token(text: String, offset: Int)

  def parse(input: String): List[SchemeValue] =
    val tokens = tokenize(input)
    parseAll(tokens, Nil, input)

  private def positionAt(input: String, offset: Int): (Int, Int) =
    input.substring(0, Math.min(offset, input.length)).foldLeft((1, 1)) {
      case ((line, _), '\n') => (line + 1, 1)
      case ((line, col), _)  => (line, col + 1)
    }

  private def tokenize(input: String): List[Token] =
    tokenizeLoop(input, 0, Nil).reverse

  @scala.annotation.tailrec
  private def tokenizeLoop(
    input: String,
    pos: Int,
    acc: List[Token]
  ): List[Token] =
    if pos >= input.length then acc
    else
      input.charAt(pos) match
        case ch if ch.isWhitespace => tokenizeLoop(input, pos + 1, acc)
        case ';' =>
          val end = input.indexOf('\n', pos)
          if end < 0 then acc
          else tokenizeLoop(input, end + 1, acc)
        case '('  => tokenizeLoop(input, pos + 1, Token("(", pos) :: acc)
        case ')'  => tokenizeLoop(input, pos + 1, Token(")", pos) :: acc)
        case '\'' => tokenizeLoop(input, pos + 1, Token("'", pos) :: acc)
        case '"' =>
          val (str, next) = readString(input, pos + 1)
          tokenizeLoop(input, next, Token(str, pos) :: acc)
        case _ =>
          val (tok, next) = readAtom(input, pos)
          tokenizeLoop(input, next, Token(tok, pos) :: acc)

  private def readString(input: String, start: Int): (String, Int) =
    readStringLoop(input, start, new StringBuilder("\""))

  @scala.annotation.tailrec
  private def readStringLoop(
    input: String,
    pos: Int,
    sb: StringBuilder
  ): (String, Int) =
    if pos >= input.length then throw new EvalError("unterminated string")
    else
      val ch = input.charAt(pos)
      if ch == '"' then (sb.append('"').toString, pos + 1)
      else if ch == '\\' && pos + 1 < input.length then
        sb.append('\\').append(input.charAt(pos + 1))
        readStringLoop(input, pos + 2, sb)
      else
        sb.append(ch)
        readStringLoop(input, pos + 1, sb)

  private def readAtom(input: String, start: Int): (String, Int) =
    readAtomLoop(input, start, start)

  @scala.annotation.tailrec
  private def readAtomLoop(
    input: String,
    start: Int,
    pos: Int
  ): (String, Int) =
    if pos >= input.length then (input.substring(start, pos), pos)
    else
      val ch = input.charAt(pos)
      if ch.isWhitespace || ch == '(' || ch == ')' || ch == '"' || ch == ';'
      then (input.substring(start, pos), pos)
      else readAtomLoop(input, start, pos + 1)

  @scala.annotation.tailrec
  private def parseAll(
    tokens: List[Token],
    acc: List[SchemeValue],
    input: String
  ): List[SchemeValue] =
    tokens match
      case Nil => acc.reverse
      case _ =>
        val (value, rest) = parseExpr(tokens, input)
        parseAll(rest, value :: acc, input)

  private def parseExpr(
    tokens: List[Token],
    input: String
  ): (SchemeValue, List[Token]) =
    tokens match
      case Nil =>
        throw new EvalError("unexpected end of input")
      case Token("(", offset) :: rest =>
        val pos = positionAt(input, offset)
        parseList(rest, Nil, input, Some(pos))
      case Token(")", _) :: _ =>
        throw new EvalError("unexpected )")
      case Token("'", offset) :: rest =>
        val pos                 = positionAt(input, offset)
        val (quoted, remaining) = parseExpr(rest, input)
        (ListVal(List(SymbolVal("quote"), quoted), Some(pos)), remaining)
      case Token(text, offset) :: rest =>
        val pos = positionAt(input, offset)
        (parseAtom(text, Some(pos)), rest)

  @scala.annotation.tailrec
  private def parseList(
    tokens: List[Token],
    acc: List[SchemeValue],
    input: String,
    listPos: Option[(Int, Int)]
  ): (SchemeValue, List[Token]) =
    tokens match
      case Nil =>
        throw new EvalError("unterminated list")
      case Token(")", _) :: rest =>
        (ListVal(acc.reverse, listPos), rest)
      case _ =>
        val (value, remaining) = parseExpr(tokens, input)
        parseList(remaining, value :: acc, input, listPos)

  private def parseAtom(
    token: String,
    pos: Option[(Int, Int)]
  ): SchemeValue =
    if token == "#t" then BoolVal(true)
    else if token == "#f" then BoolVal(false)
    else if token.startsWith("#\\") then parseCharLiteral(token)
    else
      token.toLongOption match
        case Some(n) => IntVal(n)
        case None =>
          if token.startsWith("\"") then StringVal(token.substring(1, token.length - 1))
          else SymbolVal(token, pos)

  private def parseCharLiteral(token: String): SchemeValue =
    val name = token.substring(2)
    name match
      case "space"            => CharVal(' ')
      case "newline"          => CharVal('\n')
      case "tab"              => CharVal('\t')
      case s if s.length == 1 => CharVal(s.charAt(0))
      case _                  => throw new EvalError(s"invalid character literal: $token")
