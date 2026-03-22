package ming

import SchemeValue.*

object Parser:

  def parse(input: String): List[SchemeValue] =
    val tokens = tokenize(input)
    parseAll(tokens, Nil)

  private def tokenize(input: String): List[String] =
    tokenizeLoop(input, 0, Nil).reverse

  @scala.annotation.tailrec
  private def tokenizeLoop(
    input: String,
    pos: Int,
    acc: List[String]
  ): List[String] =
    if pos >= input.length then acc
    else
      input.charAt(pos) match
        case ch if ch.isWhitespace => tokenizeLoop(input, pos + 1, acc)
        case ';' =>
          val end = input.indexOf('\n', pos)
          if end < 0 then acc
          else tokenizeLoop(input, end + 1, acc)
        case '('  => tokenizeLoop(input, pos + 1, "(" :: acc)
        case ')'  => tokenizeLoop(input, pos + 1, ")" :: acc)
        case '\'' => tokenizeLoop(input, pos + 1, "'" :: acc)
        case '"' =>
          val (str, next) = readString(input, pos + 1)
          tokenizeLoop(input, next, str :: acc)
        case _ =>
          val (tok, next) = readAtom(input, pos)
          tokenizeLoop(input, next, tok :: acc)

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
    tokens: List[String],
    acc: List[SchemeValue]
  ): List[SchemeValue] =
    tokens match
      case Nil => acc.reverse
      case _ =>
        val (value, rest) = parseExpr(tokens)
        parseAll(rest, value :: acc)

  private def parseExpr(tokens: List[String]): (SchemeValue, List[String]) =
    tokens match
      case Nil =>
        throw new EvalError("unexpected end of input")
      case "(" :: rest =>
        parseList(rest, Nil)
      case ")" :: _ =>
        throw new EvalError("unexpected )")
      case "'" :: rest =>
        val (quoted, remaining) = parseExpr(rest)
        (ListVal(List(SymbolVal("quote"), quoted)), remaining)
      case token :: rest =>
        (parseAtom(token), rest)

  @scala.annotation.tailrec
  private def parseList(
    tokens: List[String],
    acc: List[SchemeValue]
  ): (SchemeValue, List[String]) =
    tokens match
      case Nil =>
        throw new EvalError("unterminated list")
      case ")" :: rest =>
        (ListVal(acc.reverse), rest)
      case _ =>
        val (value, remaining) = parseExpr(tokens)
        parseList(remaining, value :: acc)

  private def parseAtom(token: String): SchemeValue =
    if token == "#t" then BoolVal(true)
    else if token == "#f" then BoolVal(false)
    else
      token.toLongOption match
        case Some(n) => IntVal(n)
        case None =>
          if token.startsWith("\"") then StringVal(token.substring(1, token.length - 1))
          else SymbolVal(token)
