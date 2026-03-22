package ming

import scala.annotation.tailrec

/** Tokenizer and reader for Scheme expressions. */
object Parser:

  /** Parse all expressions from the input string. */
  def parseAll(input: String): List[SchemeValue] =
    val tokens = tokenize(input)
    readAll(input, tokens, Nil)

  // --- Tokenizer (produces (text, charOffset) pairs) ---

  private def tokenize(input: String): List[(String, Int)] =
    tokenizeLoop(input, 0, Nil).reverse

  @tailrec
  private def tokenizeLoop(
    input: String,
    pos: Int,
    acc: List[(String, Int)]
  ): List[(String, Int)] =
    if pos >= input.length then acc
    else
      input.charAt(pos) match
        case ch if ch.isWhitespace => tokenizeLoop(input, pos + 1, acc)
        case ';' =>
          val end  = input.indexOf('\n', pos)
          val next = if end < 0 then input.length else end + 1
          tokenizeLoop(input, next, acc)
        case '('  => tokenizeLoop(input, pos + 1, ("(", pos) :: acc)
        case ')'  => tokenizeLoop(input, pos + 1, (")", pos) :: acc)
        case '\'' => tokenizeLoop(input, pos + 1, ("'", pos) :: acc)
        case '"'  => readString(input, pos + 1, pos, acc)
        case _    => readAtom(input, pos, acc)

  private def readString(
    input: String,
    pos: Int,
    startPos: Int,
    acc: List[(String, Int)]
  ): List[(String, Int)] =
    if pos >= input.length then throw new EvalError("unterminated string")
    else
      val end   = findStringEnd(input, pos)
      val token = input.substring(startPos, end + 1)
      tokenizeLoop(input, end + 1, (token, startPos) :: acc)

  @tailrec
  private def findStringEnd(input: String, pos: Int): Int =
    if pos >= input.length then throw new EvalError("unterminated string")
    else if input.charAt(pos) == '\\' then findStringEnd(input, pos + 2)
    else if input.charAt(pos) == '"' then pos
    else findStringEnd(input, pos + 1)

  private def readAtom(
    input: String,
    pos: Int,
    acc: List[(String, Int)]
  ): List[(String, Int)] =
    val end   = findAtomEnd(input, pos)
    val token = input.substring(pos, end)
    tokenizeLoop(input, end, (token, pos) :: acc)

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
    input: String,
    tokens: List[(String, Int)],
    acc: List[SchemeValue]
  ): List[SchemeValue] =
    tokens match
      case Nil => acc.reverse
      case _ =>
        val (value, rest) = readExpr(input, tokens)
        readAll(input, rest, value :: acc)

  private def readExpr(
    input: String,
    tokens: List[(String, Int)]
  ): (SchemeValue, List[(String, Int)]) =
    tokens match
      case Nil => throw new EvalError("unexpected end of input")
      case ("(", offset) :: rest =>
        val (list, remaining) = readList(input, rest, Nil)
        val (line, col)       = posToLineCol(input, offset)
        (SchemeValue.SchemeLocated(list, line, col), remaining)
      case ("'", offset) :: rest =>
        val (quoted, remaining) = readExpr(input, rest)
        val inner = SchemeValue.SchemeList(
          List(SchemeValue.SchemeSymbol("quote"), quoted)
        )
        val (line, col) = posToLineCol(input, offset)
        (SchemeValue.SchemeLocated(inner, line, col), remaining)
      case (")", _) :: _ => throw new EvalError("unexpected ')'")
      case (token, offset) :: rest =>
        val (line, col) = posToLineCol(input, offset)
        (SchemeValue.SchemeLocated(parseAtom(token), line, col), rest)

  @tailrec
  private def readList(
    input: String,
    tokens: List[(String, Int)],
    acc: List[SchemeValue]
  ): (SchemeValue, List[(String, Int)]) =
    tokens match
      case Nil              => throw new EvalError("unterminated list")
      case (")", _) :: rest => (SchemeValue.SchemeList(acc.reverse), rest)
      case _ =>
        val (value, rest) = readExpr(input, tokens)
        readList(input, rest, value :: acc)

  private def parseAtom(token: String): SchemeValue =
    if token == "#t" then SchemeValue.SchemeBool(true)
    else if token == "#f" then SchemeValue.SchemeBool(false)
    else if token.startsWith("\"") then SchemeValue.SchemeString(token.substring(1, token.length - 1))
    else
      token.toLongOption match
        case Some(n) => SchemeValue.SchemeInt(n)
        case None    => SchemeValue.SchemeSymbol(token)

  private def posToLineCol(input: String, offset: Int): (Int, Int) =
    input.substring(0, offset.min(input.length)).foldLeft((1, 1)) {
      case ((l, _), '\n') => (l + 1, 1)
      case ((l, c), _)    => (l, c + 1)
    }
