package ming

import scala.collection.mutable

// ── Source Position ──────────────────────────────────────────────────
case class Pos(line: Int, col: Int):
  override def toString: String = s"$line:$col"

object Pos:
  val zero: Pos = Pos(0, 0)

// ── AST ──────────────────────────────────────────────────────────────
sealed trait Expr:
  def pos: Pos

case class IntLit(value: Long, pos: Pos = Pos.zero)               extends Expr
case class FloatLit(value: Double, pos: Pos = Pos.zero)           extends Expr
case class RationalLit(num: Long, den: Long, pos: Pos = Pos.zero) extends Expr
case class BoolLit(value: Boolean, pos: Pos = Pos.zero)           extends Expr
case class StringLit(value: String, pos: Pos = Pos.zero)          extends Expr
case class CharLit(value: Char, pos: Pos = Pos.zero)              extends Expr
case class Symbol(name: String, pos: Pos = Pos.zero)              extends Expr
case class SList(elems: List[Expr], pos: Pos = Pos.zero)          extends Expr

// ── Scheme values ────────────────────────────────────────────────────
sealed trait SchemeVal:
  def display: String

case class SchemeInt(value: Long) extends SchemeVal:
  def display: String = value.toString

case class SchemeRational(num: Long, den: Long) extends SchemeVal:
  def display: String = s"$num/$den"

case class SchemeFloat(value: Double) extends SchemeVal:

  def display: String =
    if value == value.toLong && !value.isInfinite then s"${value.toLong}.0"
    else value.toString

case class SchemeBool(value: Boolean) extends SchemeVal:
  def display: String = if value then "#t" else "#f"

class SchemeString(val chars: Array[Char]) extends SchemeVal:
  def value: String   = new String(chars)
  def display: String = "\"" + value + "\""

object SchemeString:
  def apply(s: String): SchemeString          = new SchemeString(s.toCharArray)
  def unapply(ss: SchemeString): Some[String] = Some(ss.value)

case class SchemeBuiltin(name: String, fn: List[SchemeVal] => SchemeVal) extends SchemeVal:
  def display: String = s"#<procedure:$name>"

case class SchemeSymbol(name: String) extends SchemeVal:
  def display: String = name

case class SchemeLambda(params: List[String], restParam: Option[String], body: List[Expr], closureEnv: Env)
    extends SchemeVal:
  def display: String = "#<procedure>"

case class SchemeList(elems: List[SchemeVal]) extends SchemeVal:

  def display: String =
    "(" + elems.map(_.display).mkString(" ") + ")"

case class SchemePair(car: SchemeVal, cdr: SchemeVal) extends SchemeVal:

  def display: String =
    val sb = new StringBuilder("(")
    sb.append(car.display)
    var curr: SchemeVal = cdr
    var done            = false
    while !done do
      curr match
        case SchemeList(Nil) => done = true
        case SchemeList(elems) =>
          for e <- elems do sb.append(" ").append(e.display)
          done = true
        case SchemePair(a, d) =>
          sb.append(" ").append(a.display)
          curr = d
        case other =>
          sb.append(" . ").append(other.display)
          done = true
    sb.append(")")
    sb.toString

case class SchemeChar(value: Char) extends SchemeVal:

  def display: String = value match
    case ' '  => "#\\space"
    case '\n' => "#\\newline"
    case '\t' => "#\\tab"
    case c    => s"#\\$c"

case object SchemeVoid extends SchemeVal:
  def display: String = "#<void>"

case class SchemeMacro(literals: List[String], rules: List[(Expr, Expr)], defEnv: Env) extends SchemeVal:
  def display: String = "#<macro>"

// ── Environment ──────────────────────────────────────────────────────
private[ming] class Env(val bindings: mutable.Map[String, SchemeVal], val parent: Option[Env]):

  def get(name: String): SchemeVal =
    bindings.get(name) match
      case Some(v) => v
      case None =>
        parent match
          case Some(p) => p.get(name)
          case None    => throw new EvalError(s"unbound variable: $name")

  def set(name: String, value: SchemeVal): Unit =
    bindings(name) = value

  def update(name: String, value: SchemeVal): Unit =
    if bindings.contains(name) then bindings(name) = value
    else
      parent match
        case Some(p) => p.update(name, value)
        case None    => throw new EvalError(s"unbound variable: $name")
