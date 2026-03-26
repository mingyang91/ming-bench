package ming

import scala.collection.mutable

// ── AST ──────────────────────────────────────────────────────────────
sealed trait Expr
case class IntLit(value: Long)      extends Expr
case class BoolLit(value: Boolean)  extends Expr
case class StringLit(value: String) extends Expr
case class Symbol(name: String)     extends Expr
case class SList(elems: List[Expr]) extends Expr

// ── Scheme values ────────────────────────────────────────────────────
sealed trait SchemeVal:
  def display: String

case class SchemeInt(value: Long) extends SchemeVal:
  def display: String = value.toString

case class SchemeBool(value: Boolean) extends SchemeVal:
  def display: String = if value then "#t" else "#f"

case class SchemeString(value: String) extends SchemeVal:
  def display: String = s"\"$value\""

case class SchemeBuiltin(name: String, fn: List[SchemeVal] => SchemeVal) extends SchemeVal:
  def display: String = s"#<procedure:$name>"

case object SchemeVoid extends SchemeVal:
  def display: String = "#<void>"

// ── Tokenizer ────────────────────────────────────────────────────────
private object Tokenizer:
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
private object Parser:
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
    while rest.nonEmpty && !rest.head.isInstanceOf[TClose] do
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

// ── Environment ──────────────────────────────────────────────────────
private class Env(val bindings: mutable.Map[String, SchemeVal], val parent: Option[Env]):

  def get(name: String): SchemeVal =
    bindings.get(name) match
      case Some(v) => v
      case None =>
        parent match
          case Some(p) => p.get(name)
          case None    => throw new EvalError(s"unbound variable: $name")

  def set(name: String, value: SchemeVal): Unit =
    bindings(name) = value

// ── Evaluator ────────────────────────────────────────────────────────
object Evaluator:

  def evalStr(input: String): String =
    val tokens = Tokenizer.tokenize(input)
    val exprs  = Parser.parseAll(tokens)
    if exprs.isEmpty then throw new EvalError("empty input")
    val env               = makeGlobalEnv()
    var result: SchemeVal = SchemeVoid
    for expr <- exprs do result = eval(expr, env)
    result.display

  def evalStrWithOutput(input: String): (String, String) =
    throw new EvalError("not implemented")

  private def asLong(v: SchemeVal, op: String): Long = v match
    case SchemeInt(n) => n
    case _            => throw new EvalError(s"$op: expected number, got ${v.display}")

  private def makeGlobalEnv(): Env =
    val env = new Env(mutable.Map.empty, None)

    def arith(name: String, op: (Long, Long) => Long, identity: Long): SchemeBuiltin =
      SchemeBuiltin(
        name,
        args =>
          if name == "-" && args.size == 1 then SchemeInt(-asLong(args.head, name))
          else if args.size < 2 && name != "+" && name != "*" then
            throw new EvalError(s"$name: expected at least 2 arguments")
          else SchemeInt(args.map(a => asLong(a, name)).reduce(op))
      )

    env.set("+", SchemeBuiltin("+", args => SchemeInt(args.map(a => asLong(a, "+")).sum)))
    env.set(
      "-",
      SchemeBuiltin(
        "-",
        args =>
          if args.isEmpty then throw new EvalError("-: expected at least 1 argument")
          else if args.size == 1 then SchemeInt(-asLong(args.head, "-"))
          else SchemeInt(args.map(a => asLong(a, "-")).reduce(_ - _))
      )
    )
    env.set("*", SchemeBuiltin("*", args => SchemeInt(args.map(a => asLong(a, "*")).product)))
    env.set(
      "/",
      SchemeBuiltin(
        "/",
        args =>
          if args.size < 2 then throw new EvalError("/: expected at least 2 arguments")
          else
            val nums = args.map(a => asLong(a, "/"))
            if nums.tail.exists(_ == 0) then throw new EvalError("/: division by zero")
            SchemeInt(nums.reduce(_ / _))
      )
    )

    def cmp(name: String, op: (Long, Long) => Boolean): SchemeBuiltin =
      SchemeBuiltin(
        name,
        args =>
          if args.size < 2 then throw new EvalError(s"$name: expected at least 2 arguments")
          val nums = args.map(a => asLong(a, name))
          SchemeBool(nums.sliding(2).forall(w => op(w(0), w(1))))
      )

    env.set("<", cmp("<", _ < _))
    env.set(">", cmp(">", _ > _))
    env.set("=", cmp("=", _ == _))
    env.set("<=", cmp("<=", _ <= _))
    env.set(">=", cmp(">=", _ >= _))

    env.set(
      "not",
      SchemeBuiltin(
        "not",
        args =>
          if args.size != 1 then throw new EvalError("not: expected 1 argument")
          SchemeBool(isFalsy(args.head))
      )
    )

    env

  private def isFalsy(v: SchemeVal): Boolean = v match
    case SchemeBool(false) => true
    case _                 => false

  private def eval(expr: Expr, env: Env): SchemeVal =
    expr match
      case IntLit(v)    => SchemeInt(v)
      case BoolLit(v)   => SchemeBool(v)
      case StringLit(v) => SchemeString(v)
      case Symbol(name) => env.get(name)
      case SList(Nil)   => throw new EvalError("empty application")
      case SList(elems) => evalApplication(elems, env)

  private def evalApplication(elems: List[Expr], env: Env): SchemeVal =
    elems.head match
      case Symbol("and") => evalAnd(elems.tail, env)
      case Symbol("or")  => evalOr(elems.tail, env)
      case _ =>
        val op   = eval(elems.head, env)
        val args = elems.tail.map(e => eval(e, env))
        op match
          case SchemeBuiltin(_, fn) => fn(args)
          case _                    => throw new EvalError(s"not a procedure: ${op.display}")

  private def evalAnd(exprs: List[Expr], env: Env): SchemeVal =
    if exprs.isEmpty then SchemeBool(true)
    else
      var result: SchemeVal = SchemeBool(true)
      val iter              = exprs.iterator
      var done              = false
      while iter.hasNext && !done do
        result = eval(iter.next(), env)
        if isFalsy(result) then done = true
      result

  private def evalOr(exprs: List[Expr], env: Env): SchemeVal =
    if exprs.isEmpty then SchemeBool(false)
    else
      var result: SchemeVal = SchemeBool(false)
      val iter              = exprs.iterator
      var done              = false
      while iter.hasNext && !done do
        result = eval(iter.next(), env)
        if !isFalsy(result) then done = true
      result
