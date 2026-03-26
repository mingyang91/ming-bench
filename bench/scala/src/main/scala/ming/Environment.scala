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

  def assign(name: String, value: Value, position: Position): Unit =
    if bindings.contains(name) then bindings.update(name, value)
    else
      parent match
        case Some(outer) => outer.assign(name, value, position)
        case None        => SchemeFailure.raise(s"unbound symbol: $name", position)

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
