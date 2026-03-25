package ming

import scala.collection.mutable

private[ming] enum Expr:
  case Num(value: Long)
  case Bool(value: Boolean)
  case Str(chars: Array[Char])
  case Chr(value: Char)
  case Sym(name: String)
  case Lst(elems: List[Expr])
  case Lambda(params: List[String], restParam: Option[String], body: List[Expr], closure: Env)

  var line: Int = 0
  var col: Int  = 0

  def withPos(l: Int, c: Int): Expr =
    line = l
    col = c
    this

private[ming] class Env(
  val bindings: mutable.Map[String, Expr],
  val parent: Option[Env]
):

  def lookup(name: String): Expr =
    bindings.get(name) match
      case Some(v) => v
      case None =>
        parent match
          case Some(p) => p.lookup(name)
          case None    => throw EvalError(s"unbound variable: $name")

  def define(name: String, value: Expr): Unit =
    bindings(name) = value

  def set(name: String, value: Expr): Unit =
    if bindings.contains(name) then bindings(name) = value
    else
      parent match
        case Some(p) => p.set(name, value)
        case None    => throw EvalError(s"unbound variable: $name")

  def child(): Env = Env(mutable.Map.empty, Some(this))
