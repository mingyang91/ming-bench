package ming

import scala.collection.mutable.ListBuffer

/** Tokenizer */
object Tokenizer:

  def tokenize(input: String): List[String] =
    val tokens = ListBuffer[String]()
    var i      = 0
    while i < input.length do i = tokenizeOne(input, i, tokens)
    tokens.toList

  private def tokenizeOne(input: String, pos: Int, tokens: ListBuffer[String]): Int =
    input(pos) match
      case c if c.isWhitespace => pos + 1
      case ';'                 => skipLineComment(input, pos + 1)
      case '('                 => tokens += "("; pos + 1
      case ')'                 => tokens += ")"; pos + 1
      case '\''                => tokens += "'"; pos + 1
      case '"' =>
        val (tok, next) = tokenizeString(input, pos)
        tokens += tok
        next
      case '#' => tokenizeHash(input, pos, tokens)
      case _   => tokenizeWord(input, pos, tokens)

  private def skipLineComment(input: String, start: Int): Int =
    var i = start
    while i < input.length && input(i) != '\n' do i += 1
    i

  private def tokenizeHash(input: String, pos: Int, tokens: ListBuffer[String]): Int =
    var i = pos + 1
    while i < input.length && !input(i).isWhitespace && input(i) != '(' && input(i) != ')' do i += 1
    tokens += input.substring(pos, i)
    i

  private def tokenizeWord(input: String, pos: Int, tokens: ListBuffer[String]): Int =
    var i = pos
    while i < input.length && !input(i).isWhitespace && input(i) != '(' && input(i) != ')' && input(i) != '"' && input(
        i
      ) != ';'
    do i += 1
    tokens += input.substring(pos, i)
    i

  private def tokenizeString(input: String, start: Int): (String, Int) =
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

/** Parser */
object Parser:

  def parse(tokens: List[String]): (SchemeVal, List[String]) =
    tokens match
      case Nil => throw new EvalError("unexpected end of input")
      case "(" :: rest =>
        val elems     = ListBuffer[SchemeVal]()
        var remaining = rest
        while remaining.nonEmpty && remaining.head != ")" do
          val (expr, r) = parse(remaining)
          elems += expr
          remaining = r
        if remaining.isEmpty then throw new EvalError("missing closing parenthesis")
        (SchemeVal.SList(elems.toList), remaining.tail)
      case ")" :: _ => throw new EvalError("unexpected )")
      case "'" :: rest =>
        val (expr, r) = parse(rest)
        (SchemeVal.SList(List(SchemeVal.SSymbol("quote"), expr)), r)
      case token :: rest =>
        (parseAtom(token), rest)

  private def parseAtom(token: String): SchemeVal =
    if token == "#t" then SchemeVal.SBool(true)
    else if token == "#f" then SchemeVal.SBool(false)
    else if token.startsWith("\"") && token.endsWith("\"") then
      SchemeVal.SString(unescapeString(token.substring(1, token.length - 1)))
    else
      token.toLongOption match
        case Some(n) => SchemeVal.SInt(n)
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
