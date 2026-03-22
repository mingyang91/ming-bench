package ming

import SchemeValue.*

/** Tokenizer and S-expression parser for Scheme source code. */
object Parser:

  private case class Token(text: String, pos: SourcePos)

  def parse(input: String): List[SchemeValue] =
    val tokens = tokenize(input)
    readAll(tokens, Nil)

  // --- Position helper ---

  private def posAt(input: String, offset: Int): SourcePos =
    val prefix = input.substring(0, offset)
    val line   = prefix.count(_ == '\n') + 1
    val col    = offset - prefix.lastIndexOf('\n')
    SourcePos(line, col)

  // --- Tokenizer ---

  private def tokenize(input: String): List[Token] =
    tokenizeLoop(input, 0, Nil)

  @scala.annotation.tailrec
  private def tokenizeLoop(input: String, pos: Int, acc: List[Token]): List[Token] =
    if pos >= input.length then acc.reverse
    else
      val ch = input.charAt(pos)
      ch match
        case ' ' | '\t' | '\n' | '\r' => tokenizeLoop(input, pos + 1, acc)
        case ';'                      => tokenizeLoop(input, skipLineComment(input, pos + 1), acc)
        case '(' =>
          tokenizeLoop(input, pos + 1, Token("(", posAt(input, pos)) :: acc)
        case ')' =>
          tokenizeLoop(input, pos + 1, Token(")", posAt(input, pos)) :: acc)
        case '"' =>
          val (str, next) = readString(input, pos + 1, new StringBuilder)
          tokenizeLoop(input, next, Token(s""""$str"""", posAt(input, pos)) :: acc)
        case '\'' =>
          tokenizeLoop(input, pos + 1, Token("'", posAt(input, pos)) :: acc)
        case _ =>
          val (tok, next) = readAtom(input, pos, new StringBuilder)
          tokenizeLoop(input, next, Token(tok, posAt(input, pos)) :: acc)

  @scala.annotation.tailrec
  private def skipLineComment(input: String, pos: Int): Int =
    if pos >= input.length || input.charAt(pos) == '\n' then pos
    else skipLineComment(input, pos + 1)

  @scala.annotation.tailrec
  private def readString(input: String, pos: Int, sb: StringBuilder): (String, Int) =
    if pos >= input.length then throw new EvalError("unterminated string")
    else
      val ch = input.charAt(pos)
      if ch == '"' then (sb.toString, pos + 1)
      else if ch == '\\' && pos + 1 < input.length then
        val escaped = input.charAt(pos + 1) match
          case 'n'   => '\n'
          case 't'   => '\t'
          case '\\'  => '\\'
          case '"'   => '"'
          case other => other
        readString(input, pos + 2, sb.append(escaped))
      else readString(input, pos + 1, sb.append(ch))

  @scala.annotation.tailrec
  private def readAtom(input: String, pos: Int, sb: StringBuilder): (String, Int) =
    if pos >= input.length then (sb.toString, pos)
    else
      val ch = input.charAt(pos)
      if ch == ' ' || ch == '\t' || ch == '\n' || ch == '\r' || ch == '(' || ch == ')' || ch == '"' || ch == ';'
      then (sb.toString, pos)
      else readAtom(input, pos + 1, sb.append(ch))

  // --- Reader ---

  @scala.annotation.tailrec
  private def readAll(tokens: List[Token], acc: List[SchemeValue]): List[SchemeValue] =
    tokens match
      case Nil => acc.reverse
      case _ =>
        val (value, rest) = readExpr(tokens)
        readAll(rest, value :: acc)

  private def readExpr(tokens: List[Token]): (SchemeValue, List[Token]) =
    tokens match
      case Nil => throw new EvalError("unexpected end of input")
      case Token("(", pos) :: rest =>
        val (elems, remaining) = readList(rest, Nil)
        (SchemeList(elems, pos), remaining)
      case Token(")", _) :: _ => throw new EvalError("unexpected )")
      case Token("'", pos) :: rest =>
        val (quoted, remaining) = readExpr(rest)
        (SchemeList(List(SchemeSymbol("quote", pos), quoted), pos), remaining)
      case Token(tok, pos) :: rest => (parseAtom(tok, pos), rest)

  @scala.annotation.tailrec
  private def readList(tokens: List[Token], acc: List[SchemeValue]): (List[SchemeValue], List[Token]) =
    tokens match
      case Nil                   => throw new EvalError("unterminated list")
      case Token(")", _) :: rest => (acc.reverse, rest)
      case _ =>
        val (value, rest) = readExpr(tokens)
        readList(rest, value :: acc)

  private def parseAtom(token: String, pos: SourcePos): SchemeValue =
    if token == "#t" then SchemeBool(true)
    else if token == "#f" then SchemeBool(false)
    else if token.startsWith("\"") then SchemeString(token.drop(1).dropRight(1))
    else if token.startsWith("#\\") then parseCharLiteral(token)
    else parseNumericOrSymbol(token, pos)

  private def parseNumericOrSymbol(token: String, pos: SourcePos): SchemeValue =
    token.toLongOption match
      case Some(n) => SchemeInt(n)
      case None    => parseFloatOrSymbol(token, pos)

  private def parseFloatOrSymbol(token: String, pos: SourcePos): SchemeValue =
    token.toDoubleOption match
      case Some(d) if token.exists(c => c == '.' || c == 'e' || c == 'E') =>
        SchemeFloat(d)
      case _ =>
        parseRational(token).getOrElse(SchemeSymbol(token, pos))

  private def parseRational(token: String): Option[SchemeValue] =
    val idx = token.indexOf('/')
    if idx <= 0 || idx == token.length - 1 then None
    else
      val numStr = token.substring(0, idx)
      val denStr = token.substring(idx + 1)
      (numStr.toLongOption, denStr.toLongOption) match
        case (Some(n), Some(d)) if d != 0 => Some(NumericBuiltins.makeRational(n, d))
        case _                            => None

  private def parseCharLiteral(token: String): SchemeValue =
    val name = token.drop(2)
    if name.length == 1 then SchemeChar(name.charAt(0))
    else
      name match
        case "space"   => SchemeChar(' ')
        case "newline" => SchemeChar('\n')
        case "tab"     => SchemeChar('\t')
        case _         => throw new EvalError(s"unknown character name: $name")
