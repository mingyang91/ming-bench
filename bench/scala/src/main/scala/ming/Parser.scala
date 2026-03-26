package ming

import scala.collection.mutable

// ── Tokenizer ────────────────────────────────────────────────────────
private[ming] object Tokenizer:

  sealed trait Token:
    def pos: Pos

  case class TOpen(pos: Pos)                extends Token
  case class TClose(pos: Pos)               extends Token
  case class TStr(value: String, pos: Pos)  extends Token
  case class TAtom(value: String, pos: Pos) extends Token
  case class TQuote(pos: Pos)               extends Token
  case class TSyntaxQuote(pos: Pos)         extends Token

  private def readString(input: String, start: Int): (String, Int) =
    var i  = start
    val sb = new StringBuilder
    while i < input.length && input(i) != '"' do
      if input(i) == '\\' then
        i += 1
        if i < input.length then
          input(i) match
            case 'n'   => sb += '\n'
            case 't'   => sb += '\t'
            case '\\'  => sb += '\\'
            case '"'   => sb += '"'
            case other => sb += '\\'; sb += other
          i += 1
      else
        sb += input(i)
        i += 1
    val end = if i < input.length then i + 1 else i
    (sb.toString, end)

  private def isAtomChar(c: Char): Boolean =
    !c.isWhitespace && c != '(' && c != ')' && c != ';' && c != '"'

  private def readAtom(input: String, start: Int): (String, Int) =
    var i  = start
    val sb = new StringBuilder
    while i < input.length && isAtomChar(input(i)) do
      sb += input(i)
      i += 1
    (sb.toString, i)

  private def posAt(input: String, idx: Int): Pos =
    var line = 1
    var col  = 1
    var j    = 0
    while j < idx do
      if input(j) == '\n' then
        line += 1
        col = 1
      else col += 1
      j += 1
    Pos(line, col)

  def tokenize(input: String): List[Token] =
    val tokens = mutable.ListBuffer[Token]()
    var i      = 0
    while i < input.length do
      input(i) match
        case c if c.isWhitespace => i += 1
        case ';' =>
          while i < input.length && input(i) != '\n' do i += 1
        case '(' =>
          tokens += TOpen(posAt(input, i)); i += 1
        case ')' =>
          tokens += TClose(posAt(input, i)); i += 1
        case '\'' =>
          tokens += TQuote(posAt(input, i)); i += 1
        case '"' =>
          val p          = posAt(input, i)
          val (str, end) = readString(input, i + 1)
          tokens += TStr(str, p)
          i = end
        case '#' if i + 1 < input.length && input(i + 1) == '\'' =>
          tokens += TSyntaxQuote(posAt(input, i))
          i += 2
        case _ =>
          val p           = posAt(input, i)
          val (atom, end) = readAtom(input, i)
          tokens += TAtom(atom, p)
          i = end
    tokens.toList

// ── Parser ───────────────────────────────────────────────────────────
private[ming] object Parser:
  import Tokenizer.*

  def parseAll(tokens: List[Token]): List[Expr] =
    val exprs = mutable.ListBuffer[Expr]()
    var rest  = tokens
    while rest.nonEmpty do
      val (expr, remaining) = parseExpr(rest)
      exprs += expr
      rest = remaining
    exprs.toList

  private def parseExpr(tokens: List[Token]): (Expr, List[Token]) =
    tokens match
      case (t @ TOpen(p)) :: rest =>
        val (elems, remaining) = parseList(rest)
        (SList(elems, p), remaining)
      case TQuote(p) :: rest =>
        val (expr, remaining) = parseExpr(rest)
        (SList(List(Symbol("quote", p), expr), p), remaining)
      case TSyntaxQuote(p) :: rest =>
        val (expr, remaining) = parseExpr(rest)
        (SList(List(Symbol("syntax-quote", p), expr), p), remaining)
      case TStr(v, p) :: rest =>
        (StringLit(v, p), rest)
      case TAtom(v, p) :: rest =>
        (parseAtom(v, p), rest)
      case TClose(_) :: _ =>
        throw new EvalError("unexpected )")
      case Nil =>
        throw new EvalError("unexpected end of input")

  private def parseList(tokens: List[Token]): (List[Expr], List[Token]) =
    val elems = mutable.ListBuffer[Expr]()
    var rest  = tokens
    while rest.nonEmpty && (rest.head match
        case TClose(_) => false;
        case _         => true)
    do
      val (expr, remaining) = parseExpr(rest)
      elems += expr
      rest = remaining
    rest match
      case TClose(_) :: tail => (elems.toList, tail)
      case _                 => throw new EvalError("missing )")

  private def parseAtom(s: String, p: Pos): Expr =
    if s == "#t" then BoolLit(true, p)
    else if s == "#f" then BoolLit(false, p)
    else if s.startsWith("#\\") then
      val rest = s.substring(2)
      val ch = rest match
        case "space"            => ' '
        case "newline"          => '\n'
        case "tab"              => '\t'
        case c if c.length == 1 => c.charAt(0)
        case _                  => throw new EvalError(s"$p: unknown character literal: $s")
      CharLit(ch, p)
    else
      s.toLongOption match
        case Some(n) => IntLit(n, p)
        case None =>
          parseRationalOrFloat(s, p)

  private def parseRationalOrFloat(s: String, p: Pos): Expr =
    val slashIdx = s.indexOf('/')
    if slashIdx > 0 && slashIdx < s.length - 1 then
      val numStr = s.substring(0, slashIdx)
      val denStr = s.substring(slashIdx + 1)
      (numStr.toLongOption, denStr.toLongOption) match
        case (Some(n), Some(d)) if d != 0 =>
          val g    = gcd(math.abs(n), math.abs(d))
          val sign = if d < 0 then -1L else 1L
          val sn   = sign * n / g
          val sd   = sign * d / g
          if sd == 1L then IntLit(sn, p)
          else RationalLit(sn, sd, p)
        case _ => Symbol(s, p)
    else
      s.toDoubleOption match
        case Some(d) => FloatLit(d, p)
        case None    => Symbol(s, p)

  private def gcd(a: Long, b: Long): Long =
    if b == 0 then a else gcd(b, a % b)
