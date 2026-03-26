package ming

import scala.collection.mutable

final private[ming] class Environment private (
  val parent: Option[Environment],
  initialBindings: Iterable[(String, Value)]
):
  private val bindings = mutable.LinkedHashMap.from(initialBindings)

  def define(name: String, value: Value): Unit =
    bindings.update(name, value)

  def reserve(name: String): Unit =
    bindings.update(name, UninitializedValue)

  def lookup(name: String, position: Position): Value =
    bindings.get(name) match
      case Some(UninitializedValue) =>
        SchemeFailure.raise(s"uninitialized symbol: $name", position)
      case Some(value) =>
        value
      case None =>
        parent match
          case Some(outer) => outer.lookup(name, position)
          case None        => SchemeFailure.raise(s"unbound symbol: $name", position)

private[ming] object Environment:

  def root(bindings: Iterable[(String, Value)] = Nil): Environment =
    new Environment(None, bindings)

  def child(parent: Environment, bindings: Iterable[(String, Value)] = Nil): Environment =
    new Environment(Some(parent), bindings)
