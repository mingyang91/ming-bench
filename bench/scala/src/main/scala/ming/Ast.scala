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

class SchemeString(val chars: Array[Char], val immutable: Boolean = true) extends SchemeVal:
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

class SchemePair(var car: SchemeVal, var cdr: SchemeVal) extends SchemeVal:

  def display: String =
    val sb   = new StringBuilder("(")
    val seen = new java.util.IdentityHashMap[SchemePair, java.lang.Boolean]()
    seen.put(this, java.lang.Boolean.TRUE)
    sb.append(car.display)
    var curr: SchemeVal = cdr
    var done            = false
    while !done do
      curr match
        case SchemeList(Nil) => done = true
        case p: SchemePair =>
          if seen.containsKey(p) then
            sb.append(" ...")
            done = true
          else
            seen.put(p, java.lang.Boolean.TRUE)
            sb.append(" ").append(p.car.display)
            curr = p.cdr
        case SchemeList(elems) =>
          for e <- elems do sb.append(" ").append(e.display)
          done = true
        case other =>
          sb.append(" . ").append(other.display)
          done = true
    sb.append(")")
    sb.toString

object SchemePair:
  def apply(car: SchemeVal, cdr: SchemeVal): SchemePair    = new SchemePair(car, cdr)
  def unapply(p: SchemePair): Some[(SchemeVal, SchemeVal)] = Some((p.car, p.cdr))

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

case class SchemeCaseLambda(clauses: List[SchemeLambda]) extends SchemeVal:
  def display: String = "#<procedure>"

case class SchemeRecord(typeName: String, fields: mutable.Map[String, SchemeVal]) extends SchemeVal:
  def display: String = s"#<$typeName>"

class SchemeVector(val elems: Array[SchemeVal]) extends SchemeVal:
  def display: String = "#(" + elems.map(_.display).mkString(" ") + ")"

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

// ── List helpers ────────────────────────────────────────────────────
object SchemeListOps:

  /** Extract Scala list from a SchemeVal proper list (pair chain or SchemeList). Returns None for improper or circular
    * lists.
    */
  def toScalaList(v: SchemeVal): Option[List[SchemeVal]] =
    v match
      case SchemeList(Nil)   => Some(Nil)
      case SchemeList(elems) => Some(elems)
      case _ =>
        val buf   = List.newBuilder[SchemeVal]
        val seen  = new java.util.IdentityHashMap[SchemePair, java.lang.Boolean]()
        var curr  = v
        var going = true
        while going do
          curr match
            case SchemeList(Nil)   => going = false
            case SchemeList(elems) => buf ++= elems; going = false
            case p: SchemePair =>
              if seen.containsKey(p) then return None
              seen.put(p, java.lang.Boolean.TRUE)
              buf += p.car
              curr = p.cdr
            case _ => return None
        Some(buf.result())

  /** Build a proper list (pair chain ending in SchemeList(Nil)). */
  def makeList(elems: List[SchemeVal]): SchemeVal =
    if elems.isEmpty then SchemeList(Nil)
    else elems.foldRight[SchemeVal](SchemeList(Nil))((e, acc) => new SchemePair(e, acc))

  /** Check whether a value is a proper list (with cycle detection). */
  def isList(v: SchemeVal): Boolean =
    v match
      case SchemeList(_) => true
      case _ =>
        var slow = v
        var fast = v
        while true do
          fast match
            case SchemeList(_) => return true
            case fp: SchemePair =>
              fp.cdr match
                case SchemeList(_) => return true
                case fp2: SchemePair =>
                  slow = slow.asInstanceOf[SchemePair].cdr
                  fast = fp2.cdr
                  if slow eq fast then return false
                case _ => return false
            case _ => return false
        false
