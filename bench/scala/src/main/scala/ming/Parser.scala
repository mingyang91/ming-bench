package ming

import scala.collection.mutable

// ── Tokenizer ────────────────────────────────────────────────────────
private[ming] object Tokenizer:
  sealed trait Token
  case class TOpen()              extends Token
  case class TClose()             extends Token
  case class TStr(value: String)  extends Token
  case class TAtom(value: String) extends Token

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

  def tokenize(input: String): List[Token] =
    val tokens = mutable.ListBuffer[Token]()
    var i      = 0
    while i < input.length do
      input(i) match
        case c if c.isWhitespace => i += 1
        case ';' =>
          while i < input.length && input(i) != '\n' do i += 1
        case '(' => tokens += TOpen(); i += 1
        case ')' => tokens += TClose(); i += 1
        case '"' =>
          val (str, end) = readString(input, i + 1)
          tokens += TStr(str)
          i = end
        case _ =>
          val (atom, end) = readAtom(input, i)
          tokens += TAtom(atom)
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
      case TOpen() :: rest =>
        val (elems, remaining) = parseList(rest)
        (SList(elems), remaining)
      case TStr(v) :: rest =>
        (StringLit(v), rest)
      case TAtom(v) :: rest =>
        (parseAtom(v), rest)
      case TClose() :: _ =>
        throw new EvalError("unexpected )")
      case Nil =>
        throw new EvalError("unexpected end of input")

  private def parseList(tokens: List[Token]): (List[Expr], List[Token]) =
    val elems = mutable.ListBuffer[Expr]()
    var rest  = tokens
    while rest.nonEmpty && (rest.head match
        case TClose() => false;
        case _        => true)
    do
      val (expr, remaining) = parseExpr(rest)
      elems += expr
      rest = remaining
    rest match
      case TClose() :: tail => (elems.toList, tail)
      case _                => throw new EvalError("missing )")

  private def parseAtom(s: String): Expr =
    if s == "#t" then BoolLit(true)
    else if s == "#f" then BoolLit(false)
    else
      s.toLongOption match
        case Some(n) => IntLit(n)
        case None    => Symbol(s)
