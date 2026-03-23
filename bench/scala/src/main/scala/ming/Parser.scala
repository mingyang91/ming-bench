package ming

import SchemeValue.*

/** Recursive-descent parser for Scheme S-expressions. */
object Parser:

  def parse(input: String): List[SchemeValue] =
    val tokens = tokenize(input)
    parseAll(tokens, Nil)

  private def tokenize(input: String): List[String] =
    val buf = scala.collection.mutable.ListBuffer[String]()
    var i   = 0
    while i < input.length do i = scanOne(input, i, buf)
    buf.toList

  private def scanOne(input: String, pos: Int, buf: scala.collection.mutable.ListBuffer[String]): Int =
    input(pos) match
      case c if c.isWhitespace => pos + 1
      case ';'                 => skipLineComment(input, pos)
      case '(' | ')'           => buf += input(pos).toString; pos + 1
      case '\''                => buf += "'"; pos + 1
      case '"' =>
        val (tok, next) = scanString(input, pos)
        buf += tok
        next
      case '#' if pos + 1 < input.length => scanHash(input, pos, buf)
      case _                             => scanSymbol(input, pos, buf)

  private def skipLineComment(input: String, pos: Int): Int =
    var i = pos
    while i < input.length && input(i) != '\n' do i += 1
    i

  private def scanHash(input: String, pos: Int, buf: scala.collection.mutable.ListBuffer[String]): Int =
    val next = input(pos + 1)
    if next == 't' || next == 'f' then
      buf += input.substring(pos, pos + 2)
      pos + 2
    else scanSymbol(input, pos, buf)

  private def scanSymbol(input: String, pos: Int, buf: scala.collection.mutable.ListBuffer[String]): Int =
    val sb = new StringBuilder
    var i  = pos
    while i < input.length && !input(i).isWhitespace && input(i) != '(' && input(i) != ')' do
      sb += input(i)
      i += 1
    buf += sb.toString
    i

  private def scanString(input: String, start: Int): (String, Int) =
    val sb = new StringBuilder("\"")
    var i  = start + 1
    while i < input.length && input(i) != '"' do
      if input(i) == '\\' then
        sb += input(i)
        i += 1
        if i < input.length then
          sb += input(i)
          i += 1
      else
        sb += input(i)
        i += 1
    if i < input.length then
      sb += '"'
      i += 1
    (sb.toString, i)

  @scala.annotation.tailrec
  private def parseAll(tokens: List[String], acc: List[SchemeValue]): List[SchemeValue] =
    tokens match
      case Nil => acc.reverse
      case _ =>
        val (value, rest) = parseExpr(tokens)
        parseAll(rest, value :: acc)

  private def parseExpr(tokens: List[String]): (SchemeValue, List[String]) =
    tokens match
      case Nil => throw new EvalError("unexpected end of input")
      case "(" :: rest =>
        val (elements, remaining) = parseList(rest, Nil)
        (SList(elements), remaining)
      case "'" :: rest =>
        val (quoted, remaining) = parseExpr(rest)
        (SList(List(SSymbol("quote"), quoted)), remaining)
      case ")" :: _ => throw new EvalError("unexpected )")
      case token :: rest =>
        (parseAtom(token), rest)

  @scala.annotation.tailrec
  private def parseList(tokens: List[String], acc: List[SchemeValue]): (List[SchemeValue], List[String]) =
    tokens match
      case Nil         => throw new EvalError("unexpected end of input, expected )")
      case ")" :: rest => (acc.reverse, rest)
      case _ =>
        val (value, remaining) = parseExpr(tokens)
        parseList(remaining, value :: acc)

  private def parseAtom(token: String): SchemeValue =
    if token == "#t" then SBoolean(true)
    else if token == "#f" then SBoolean(false)
    else if token.startsWith("\"") then SString(unescapeString(token.substring(1, token.length - 1)))
    else
      token.toLongOption match
        case Some(n) => SInteger(n)
        case None    => SSymbol(token)

  private def unescapeString(s: String): String =
    val sb = new StringBuilder
    var i  = 0
    while i < s.length do
      if s(i) == '\\' && i + 1 < s.length then
        s(i + 1) match
          case 'n'   => sb += '\n'
          case 't'   => sb += '\t'
          case '\\'  => sb += '\\'
          case '"'   => sb += '"'
          case other => sb += '\\'; sb += other
        i += 2
      else
        sb += s(i)
        i += 1
    sb.toString
