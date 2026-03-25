package ming

import scala.collection.mutable.ListBuffer

case class Token(text: String, line: Int, col: Int)

/** Tokenizer */
object Tokenizer:

  def tokenize(input: String): List[Token] =
    val tokens = ListBuffer[Token]()
    var i      = 0
    var line   = 1
    var col    = 1
    while i < input.length do
      val ch = input(i)
      ch match
        case '\n' =>
          i += 1; line += 1; col = 1
        case c if c.isWhitespace =>
          i += 1; col += 1
        case ';' =>
          while i < input.length && input(i) != '\n' do
            i += 1; col += 1
        case '(' =>
          tokens += Token("(", line, col); i += 1; col += 1
        case ')' =>
          tokens += Token(")", line, col); i += 1; col += 1
        case '\'' =>
          tokens += Token("'", line, col); i += 1; col += 1
        case '"' =>
          val startCol = col
          val sb       = new StringBuilder("\"")
          i += 1; col += 1
          while i < input.length && input(i) != '"' do
            if input(i) == '\\' then
              sb += input(i); i += 1; col += 1
              if i < input.length then
                sb += input(i); i += 1; col += 1
            else
              sb += input(i); i += 1; col += 1
          if i < input.length then
            sb += '"'; i += 1; col += 1
          tokens += Token(sb.toString, line, startCol)
        case '#' =>
          if i + 1 < input.length && input(i + 1) == '\'' then
            tokens += Token("#'", line, col)
            i += 2; col += 2
          else
            val startCol = col
            val start    = i
            i += 1; col += 1
            while i < input.length && !input(i).isWhitespace && input(i) != '(' && input(i) != ')' do
              i += 1; col += 1
            tokens += Token(input.substring(start, i), line, startCol)
        case _ =>
          val startCol = col
          val start    = i
          while i < input.length && !input(i).isWhitespace && input(i) != '(' && input(i) != ')' && input(
              i
            ) != '"' && input(i) != ';'
          do
            i += 1; col += 1
          tokens += Token(input.substring(start, i), line, startCol)
    tokens.toList

/** Parser */
object Parser:

  private def withPos(v: SchemeVal, t: Token): SchemeVal =
    v.pos = Some(Pos(t.line, t.col))
    v

  def parse(tokens: List[Token]): (SchemeVal, List[Token]) =
    tokens match
      case Nil => throw new EvalError("unexpected end of input")
      case t :: rest if t.text == "(" =>
        val elems     = ListBuffer[SchemeVal]()
        var remaining = rest
        while remaining.nonEmpty && remaining.head.text != ")" do
          val (expr, r) = parse(remaining)
          elems += expr
          remaining = r
        if remaining.isEmpty then throw new EvalError("missing closing parenthesis")
        (withPos(SchemeVal.SList(elems.toList), t), remaining.tail)
      case t :: _ if t.text == ")" => throw new EvalError("unexpected )")
      case t :: rest if t.text == "'" =>
        val (expr, r) = parse(rest)
        (withPos(SchemeVal.SList(List(SchemeVal.SSymbol("quote"), expr)), t), r)
      case t :: rest if t.text == "#'" =>
        val (expr, r) = parse(rest)
        (withPos(SchemeVal.SList(List(SchemeVal.SSymbol("syntax"), expr)), t), r)
      case t :: rest =>
        (withPos(parseAtom(t.text), t), rest)

  private def parseAtom(token: String): SchemeVal =
    if token == "#t" then SchemeVal.SBool(true)
    else if token == "#f" then SchemeVal.SBool(false)
    else if token.startsWith("#\\") then
      val charPart = token.substring(2)
      val c = charPart match
        case "space"            => ' '
        case "newline"          => '\n'
        case "tab"              => '\t'
        case s if s.length == 1 => s.charAt(0)
        case _                  => throw new EvalError(s"unknown character literal: $token")
      SchemeVal.SChar(c)
    else if token.startsWith("\"") && token.endsWith("\"") then
      SchemeVal.SString(new StringBuilder(unescapeString(token.substring(1, token.length - 1))), false)
    else
      token.toLongOption match
        case Some(n) => SchemeVal.SInt(n)
        case None    => parseNonInteger(token)

  private def parseNonInteger(token: String): SchemeVal =
    val slashIdx = token.indexOf('/')
    if slashIdx > 0 && slashIdx < token.length - 1 then parseRationalOrFallback(token, slashIdx)
    else
      token.toDoubleOption match
        case Some(d) if token.contains('.') => SchemeVal.SFloat(d)
        case _                              => SchemeVal.SSymbol(token)

  private def parseRationalOrFallback(token: String, slashIdx: Int): SchemeVal =
    val numStr = token.substring(0, slashIdx)
    val denStr = token.substring(slashIdx + 1)
    (numStr.toLongOption, denStr.toLongOption) match
      case (Some(n), Some(d)) if d != 0 => SchemeVal.makeRational(n, d)
      case _ =>
        token.toDoubleOption match
          case Some(d) => SchemeVal.SFloat(d)
          case None    => SchemeVal.SSymbol(token)

  private def unescapeString(s: String): String =
    val sb = new StringBuilder
    var i  = 0
    while i < s.length do
      if s(i) == '\\' && i + 1 < s.length then
        s(i + 1) match
          case 'n'   => sb += '\n'; i += 2
          case 't'   => sb += '\t'; i += 2
          case '\\'  => sb += '\\'; i += 2
          case '"'   => sb += '"'; i += 2
          case other => sb += '\\'; sb += other; i += 2
      else
        sb += s(i)
        i += 1
    sb.toString

  def parseAll(input: String): List[SchemeVal] =
    val tokens    = Tokenizer.tokenize(input)
    val exprs     = ListBuffer[SchemeVal]()
    var remaining = tokens
    while remaining.nonEmpty do
      val (expr, r) = parse(remaining)
      exprs += expr
      remaining = r
    exprs.toList
