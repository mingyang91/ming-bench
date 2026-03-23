package ming

import SchemeValue.*

/** Recursive-descent parser for Scheme S-expressions. */
object Parser:

  private case class Token(text: String, line: Int, col: Int)

  def parse(input: String): List[SchemeValue] =
    val tokens = tokenize(input)
    parseAll(tokens, Nil)

  private def tokenize(input: String): List[Token] =
    val lineCol = buildLineCol(input)
    val buf     = scala.collection.mutable.ListBuffer[Token]()
    var i       = 0
    while i < input.length do i = scanOne(input, i, lineCol, buf)
    buf.toList

  private def buildLineCol(input: String): Array[(Int, Int)] =
    val arr  = new Array[(Int, Int)](input.length)
    var line = 1
    var col  = 1
    for i <- input.indices do
      arr(i) = (line, col)
      if input(i) == '\n' then
        line += 1
        col = 1
      else col += 1
    arr

  private def scanOne(
    input: String,
    pos: Int,
    lineCol: Array[(Int, Int)],
    buf: scala.collection.mutable.ListBuffer[Token]
  ): Int =
    input(pos) match
      case c if c.isWhitespace => pos + 1
      case ';'                 => skipLineComment(input, pos)
      case '(' | ')' =>
        val (l, c) = lineCol(pos)
        buf += Token(input(pos).toString, l, c)
        pos + 1
      case '\'' =>
        val (l, c) = lineCol(pos)
        buf += Token("'", l, c)
        pos + 1
      case '"' =>
        val (l, c)      = lineCol(pos)
        val (tok, next) = scanString(input, pos)
        buf += Token(tok, l, c)
        next
      case '#' if pos + 1 < input.length => scanHash(input, pos, lineCol, buf)
      case _                             => scanSymbol(input, pos, lineCol, buf)

  private def skipLineComment(input: String, pos: Int): Int =
    var i = pos
    while i < input.length && input(i) != '\n' do i += 1
    i

  private def scanHash(
    input: String,
    pos: Int,
    lineCol: Array[(Int, Int)],
    buf: scala.collection.mutable.ListBuffer[Token]
  ): Int =
    val next = input(pos + 1)
    if next == 't' || next == 'f' then
      val (l, c) = lineCol(pos)
      buf += Token(input.substring(pos, pos + 2), l, c)
      pos + 2
    else if next == '\\' then
      val (l, c) = lineCol(pos)
      if pos + 2 < input.length then
        val charStart = pos + 2
        var end       = charStart + 1
        while end < input.length && input(end).isLetter do end += 1
        val tok = input.substring(pos, end)
        buf += Token(tok, l, c)
        end
      else throw new EvalError("incomplete character literal")
    else scanSymbol(input, pos, lineCol, buf)

  private def scanSymbol(
    input: String,
    pos: Int,
    lineCol: Array[(Int, Int)],
    buf: scala.collection.mutable.ListBuffer[Token]
  ): Int =
    val (l, c) = lineCol(pos)
    val sb     = new StringBuilder
    var i      = pos
    while i < input.length && !input(i).isWhitespace && input(i) != '(' && input(i) != ')' do
      sb += input(i)
      i += 1
    buf += Token(sb.toString, l, c)
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
  private def parseAll(tokens: List[Token], acc: List[SchemeValue]): List[SchemeValue] =
    tokens match
      case Nil => acc.reverse
      case _ =>
        val (value, rest) = parseExpr(tokens)
        parseAll(rest, value :: acc)

  private def parseExpr(tokens: List[Token]): (SchemeValue, List[Token]) =
    tokens match
      case Nil => throw new EvalError("unexpected end of input")
      case Token("(", line, col) :: rest =>
        val (elements, remaining) = parseList(rest, Nil)
        (SList(elements, (line, col)), remaining)
      case Token("'", line, col) :: rest =>
        val (quoted, remaining) = parseExpr(rest)
        (SList(List(SSymbol("quote"), quoted), (line, col)), remaining)
      case Token(")", _, _) :: _ => throw new EvalError("unexpected )")
      case Token(text, _, _) :: rest =>
        (parseAtom(text), rest)

  @scala.annotation.tailrec
  private def parseList(tokens: List[Token], acc: List[SchemeValue]): (List[SchemeValue], List[Token]) =
    tokens match
      case Nil                      => throw new EvalError("unexpected end of input, expected )")
      case Token(")", _, _) :: rest => (acc.reverse, rest)
      case _ =>
        val (value, remaining) = parseExpr(tokens)
        parseList(remaining, value :: acc)

  private def parseAtom(token: String): SchemeValue =
    if token == "#t" then SBoolean(true)
    else if token == "#f" then SBoolean(false)
    else if token.startsWith("#\\") then parseCharLiteral(token)
    else if token.startsWith("\"") then SchemeValue.makeString(unescapeString(token.substring(1, token.length - 1)))
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
