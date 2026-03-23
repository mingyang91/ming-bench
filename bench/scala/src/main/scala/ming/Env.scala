package ming

import scala.collection.mutable

/** Lexical environment with mutable bindings for recursive definitions. */
class Env(
  private val bindings: mutable.Map[String, Value] = mutable.Map.empty,
  private val parent: Option[Env] = None
):

  def lookup(name: String, pos: Option[Pos] = None): Value =
    bindings.get(name) match
      case Some(v) => v
      case None =>
        parent match
          case Some(p) => p.lookup(name, pos)
          case None =>
            pos match
              case Some(p) => throw new EvalError(s"unbound variable: $name [$p]")
              case None    => throw new EvalError(s"unbound variable: $name")

  def define(name: String, value: Value): Unit =
    bindings(name) = value

  def extend(names: List[String], values: List[Value]): Env =
    if names.length != values.length then
      throw new EvalError(s"expected ${names.length} arguments, got ${values.length}")
    Env(mutable.Map.from(names.zip(values)), Some(this))
