package ming

import scala.annotation.tailrec

/** Tokenizer and reader for Scheme expressions. */
object Parser:

  /** Parse all expressions from the input string. */
  def parseAll(input: String): List[SchemeValue] =
    val tokens = tokenize(input)
    readAll(tokens, Nil)

  // --- Tokenizer ---

  private def tokenize(input: String): List[String] =
    tokenizeLoop(input, 0, Nil).reverse

  @tailrec
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
          val end  = input.indexOf('\n', pos)
          val next = if end < 0 then input.length else end + 1
          tokenizeLoop(input, next, acc)
        case '('  => tokenizeLoop(input, pos + 1, "(" :: acc)
        case ')'  => tokenizeLoop(input, pos + 1, ")" :: acc)
        case '\'' => tokenizeLoop(input, pos + 1, "'" :: acc)
        case '"'  => readString(input, pos + 1, acc)
        case _    => readAtom(input, pos, acc)

  private def readString(
    input: String,
    pos: Int,
    acc: List[String]
  ): List[String] =
    if pos >= input.length then throw new EvalError("unterminated string")
    else
      val end   = findStringEnd(input, pos)
      val token = input.substring(pos - 1, end + 1)
      tokenizeLoop(input, end + 1, token :: acc)

  @tailrec
  private def findStringEnd(input: String, pos: Int): Int =
    if pos >= input.length then throw new EvalError("unterminated string")
    else if input.charAt(pos) == '\\' then findStringEnd(input, pos + 2)
    else if input.charAt(pos) == '"' then pos
    else findStringEnd(input, pos + 1)

  private def readAtom(
    input: String,
    pos: Int,
    acc: List[String]
  ): List[String] =
    val end   = findAtomEnd(input, pos)
    val token = input.substring(pos, end)
    tokenizeLoop(input, end, token :: acc)

  private def isDelimiter(ch: Char): Boolean =
    ch.isWhitespace || ch == '(' || ch == ')' || ch == '"' || ch == ';'

  @tailrec
  private def findAtomEnd(input: String, pos: Int): Int =
    if pos >= input.length then pos
    else if isDelimiter(input.charAt(pos)) then pos
    else findAtomEnd(input, pos + 1)

  // --- Reader ---

  @tailrec
  private def readAll(
    tokens: List[String],
    acc: List[SchemeValue]
  ): List[SchemeValue] =
    tokens match
      case Nil => acc.reverse
      case _ =>
        val (value, rest) = readExpr(tokens)
        readAll(rest, value :: acc)

  private def readExpr(tokens: List[String]): (SchemeValue, List[String]) =
    tokens match
      case Nil         => throw new EvalError("unexpected end of input")
      case "(" :: rest => readList(rest, Nil)
      case "'" :: rest =>
        val (quoted, remaining) = readExpr(rest)
        (
          SchemeValue.SchemeList(
            List(SchemeValue.SchemeSymbol("quote"), quoted)
          ),
          remaining
        )
      case ")" :: _      => throw new EvalError("unexpected ')'")
      case token :: rest => (parseAtom(token), rest)

  @tailrec
  private def readList(
    tokens: List[String],
    acc: List[SchemeValue]
  ): (SchemeValue, List[String]) =
    tokens match
      case Nil         => throw new EvalError("unterminated list")
      case ")" :: rest => (SchemeValue.SchemeList(acc.reverse), rest)
      case _ =>
        val (value, rest) = readExpr(tokens)
        readList(rest, value :: acc)

  private def parseAtom(token: String): SchemeValue =
    if token == "#t" then SchemeValue.SchemeBool(true)
    else if token == "#f" then SchemeValue.SchemeBool(false)
    else if token.startsWith("\"") then SchemeValue.SchemeString(token.substring(1, token.length - 1))
    else
      token.toLongOption match
        case Some(n) => SchemeValue.SchemeInt(n)
        case None    => SchemeValue.SchemeSymbol(token)
