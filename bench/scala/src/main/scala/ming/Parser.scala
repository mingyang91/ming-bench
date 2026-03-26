package ming

private[ming] case class Pos(line: Int, col: Int):
  override def toString: String = s"$line:$col"

private[ming] enum Expr:
  case Num(value: Long, pos: Pos = Pos(0, 0))
  case Flt(value: Double, pos: Pos = Pos(0, 0))
  case Rat(num: Long, den: Long, pos: Pos = Pos(0, 0))
  case Bool(value: Boolean, pos: Pos = Pos(0, 0))
  case Str(value: String, pos: Pos = Pos(0, 0))
  case Chr(value: Char, pos: Pos = Pos(0, 0))
  case Symbol(name: String, pos: Pos = Pos(0, 0))
  case SList(elems: List[Expr], pos: Pos = Pos(0, 0))

private[ming] object Parser:

  private case class Token(text: String, pos: Pos)

  private def tokenize(input: String): List[Token] =
    val tokens = scala.collection.mutable.ListBuffer[Token]()
    var i      = 0
    var line   = 1
    var col    = 1
    while i < input.length do
      input(i) match
        case '\n' =>
          i += 1; line += 1; col = 1
        case c if c.isWhitespace =>
          i += 1; col += 1
        case ';' =>
          val next = skipComment(input, i)
          col += (next - i)
          i = next
        case '(' | ')' | '\'' =>
          tokens += Token(input(i).toString, Pos(line, col))
          i += 1; col += 1
        case '"' =>
          val p           = Pos(line, col)
          val (tok, next) = readString(input, i)
          tokens += Token(tok, p)
          col += (next - i)
          i = next
        case '#' =>
          val p           = Pos(line, col)
          val (tok, next) = readHash(input, i)
          tokens += Token(tok, p)
          col += (next - i)
          i = next
        case _ =>
          val p           = Pos(line, col)
          val (tok, next) = readWord(input, i)
          tokens += Token(tok, p)
          col += (next - i)
          i = next
    tokens.toList

  def parseAll(input: String): List[Expr] =
    val tokens = tokenize(input)
    val exprs  = scala.collection.mutable.ListBuffer[Expr]()
    var rest   = tokens
    while rest.nonEmpty do
      val (expr, remaining) = parseExpr(rest)
      exprs += expr
      rest = remaining
    exprs.toList

  private def parseExpr(tokens: List[Token]): (Expr, List[Token]) = tokens match
    case Nil => throw EvalError("unexpected end of input")
    case Token("(", p) :: rest =>
      val (elems, remaining) = parseList(rest)
      (Expr.SList(elems, p), remaining)
    case Token("'", p) :: rest =>
      val (expr, remaining) = parseExpr(rest)
      (Expr.SList(List(Expr.Symbol("quote", p), expr), p), remaining)
    case Token(")", _) :: _     => throw EvalError("unexpected )")
    case Token(text, p) :: rest => (parseAtom(text, p), rest)

  private def parseList(tokens: List[Token]): (List[Expr], List[Token]) =
    val elems = scala.collection.mutable.ListBuffer[Expr]()
    var rest  = tokens
    while rest.nonEmpty && rest.head.text != ")" do
      val (expr, remaining) = parseExpr(rest)
      elems += expr
      rest = remaining
    if rest.isEmpty then throw EvalError("missing )")
    (elems.toList, rest.tail)

  private def parseAtom(token: String, pos: Pos): Expr =
    if token == "#t" then Expr.Bool(true, pos)
    else if token == "#f" then Expr.Bool(false, pos)
    else if token.startsWith("#\\") then parseCharLiteral(token.substring(2), pos)
    else if token.startsWith("\"") then Expr.Str(token.substring(1, token.length - 1), pos)
    else
      token.toLongOption match
        case Some(n) => Expr.Num(n, pos)
        case None =>
          tryParseRationalOrFloat(token, pos)

  private def tryParseRationalOrFloat(token: String, pos: Pos): Expr =
    val slashIdx = token.indexOf('/')
    if slashIdx > 0 && slashIdx < token.length - 1 then tryParseRational(token, slashIdx, pos)
    else
      token.toDoubleOption match
        case Some(d) if token.contains('.') => Expr.Flt(d, pos)
        case _                              => Expr.Symbol(token, pos)

  private def tryParseRational(token: String, slashIdx: Int, pos: Pos): Expr =
    val numPart = token.substring(0, slashIdx)
    val denPart = token.substring(slashIdx + 1)
    (numPart.toLongOption, denPart.toLongOption) match
      case (Some(n), Some(d)) if d != 0 =>
        val g    = gcd(math.abs(n), math.abs(d))
        val sign = if d < 0 then -1L else 1L
        val rn   = sign * n / g
        val rd   = sign * d / g
        if rd == 1L then Expr.Num(rn, pos)
        else Expr.Rat(rn, rd, pos)
      case _ => Expr.Symbol(token, pos)

  private def gcd(a: Long, b: Long): Long =
    if b == 0 then a else gcd(b, a % b)

  private def parseCharLiteral(name: String, pos: Pos): Expr =
    val c = name match
      case "space"            => ' '
      case "newline"          => '\n'
      case "tab"              => '\t'
      case s if s.length == 1 => s.charAt(0)
      case _                  => throw EvalError(s"$pos: unknown character name: $name")
    Expr.Chr(c, pos)

  private def readWord(input: String, start: Int): (String, Int) =
    val sb = new StringBuilder
    var i  = start
    while i < input.length && !input(i).isWhitespace && input(i) != '(' && input(i) != ')' do
      sb += input(i); i += 1
    (sb.toString, i)

  private def readString(input: String, start: Int): (String, Int) =
    val sb = new StringBuilder("\"")
    var i  = start + 1
    while i < input.length && input(i) != '"' do
      if input(i) == '\\' then
        sb += input(i); i += 1
        if i < input.length then
          sb += input(i); i += 1
      else
        sb += input(i); i += 1
    if i < input.length then
      sb += '"'; i += 1
    (sb.toString, i)

  private def readHash(input: String, start: Int): (String, Int) =
    if start + 1 >= input.length then ("#", start + 1)
    else
      val next = input(start + 1)
      if next == 't' || next == 'f' then (input.substring(start, start + 2), start + 2)
      else if next == '\\' then
        // Character literal: #\<char> or #\space, #\newline, etc.
        if start + 2 >= input.length then (input.substring(start), input.length)
        else
          // Read the rest as a word to handle named chars like #\space
          val (word, end) = readWord(input, start + 2)
          (s"#\\$word", end)
      else readWord(input, start)

  private def skipComment(input: String, start: Int): Int =
    var i = start
    while i < input.length && input(i) != '\n' do i += 1
    i
