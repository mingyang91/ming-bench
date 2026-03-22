package ming

import SchemeValue.*

/** Tokenizer and S-expression parser for Scheme source code. */
object Parser:

  def parse(input: String): List[SchemeValue] =
    val tokens = tokenize(input)
    readAll(tokens, Nil)

  // --- Tokenizer ---

  private def tokenize(input: String): List[String] =
    tokenizeLoop(input, 0, Nil)

  @scala.annotation.tailrec
  private def tokenizeLoop(input: String, pos: Int, acc: List[String]): List[String] =
    if pos >= input.length then acc.reverse
    else
      val ch = input.charAt(pos)
      ch match
        case ' ' | '\t' | '\n' | '\r' => tokenizeLoop(input, pos + 1, acc)
        case ';'                      => tokenizeLoop(input, skipLineComment(input, pos + 1), acc)
        case '('                      => tokenizeLoop(input, pos + 1, "(" :: acc)
        case ')'                      => tokenizeLoop(input, pos + 1, ")" :: acc)
        case '"' =>
          val (str, next) = readString(input, pos + 1, new StringBuilder)
          tokenizeLoop(input, next, s""""$str"""" :: acc)
        case '\'' => tokenizeLoop(input, pos + 1, "'" :: acc)
        case _ =>
          val (tok, next) = readAtom(input, pos, new StringBuilder)
          tokenizeLoop(input, next, tok :: acc)

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
  private def readAll(tokens: List[String], acc: List[SchemeValue]): List[SchemeValue] =
    tokens match
      case Nil => acc.reverse
      case _ =>
        val (value, rest) = readExpr(tokens)
        readAll(rest, value :: acc)

  private def readExpr(tokens: List[String]): (SchemeValue, List[String]) =
    tokens match
      case Nil         => throw new EvalError("unexpected end of input")
      case "(" :: rest => readList(rest, Nil)
      case ")" :: _    => throw new EvalError("unexpected )")
      case "'" :: rest =>
        val (quoted, remaining) = readExpr(rest)
        (SchemeList(List(SchemeSymbol("quote"), quoted)), remaining)
      case tok :: rest => (parseAtom(tok), rest)

  @scala.annotation.tailrec
  private def readList(tokens: List[String], acc: List[SchemeValue]): (SchemeValue, List[String]) =
    tokens match
      case Nil         => throw new EvalError("unterminated list")
      case ")" :: rest => (SchemeList(acc.reverse), rest)
      case _ =>
        val (value, rest) = readExpr(tokens)
        readList(rest, value :: acc)

  private def parseAtom(token: String): SchemeValue =
    if token == "#t" then SchemeBool(true)
    else if token == "#f" then SchemeBool(false)
    else if token.startsWith("\"") then SchemeString(token.drop(1).dropRight(1))
    else
      token.toLongOption match
        case Some(n) => SchemeInt(n)
        case None    => SchemeSymbol(token)
