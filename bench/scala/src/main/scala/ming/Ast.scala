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

case class SchemeSymbol(name: String) extends SchemeVal:
  def display: String = name

case class SchemeLambda(params: List[String], body: List[Expr], closureEnv: Env) extends SchemeVal:
  def display: String = "#<procedure>"

case class SchemeList(elems: List[SchemeVal]) extends SchemeVal:

  def display: String =
    "(" + elems.map(_.display).mkString(" ") + ")"

case object SchemeVoid extends SchemeVal:
  def display: String = "#<void>"

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
