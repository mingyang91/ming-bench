package ming

import scala.collection.mutable

final private[ming] class Env private (
  private val parent: Option[Env],
  private val bindings: mutable.Map[String, Value]
):

  def child(): Env =
    Env.childOf(this)

  def define(name: String, value: Value): Unit =
    bindings.update(name, value)

  def assign(name: String, value: Value): Boolean =
    if bindings.contains(name) then
      bindings.update(name, value)
      true
    else parent.exists(_.assign(name, value))

  def lookup(name: String): Option[Value] =
    bindings
      .get(name)
      .orElse(parent.flatMap(_.lookup(name)))
      .orElse(Builtins.resolve(name))

private[ming] object Env:

  def topLevel(): Env =
    new Env(parent = None, bindings = mutable.Map.empty)

  def childOf(parent: Env): Env =
    new Env(parent = Some(parent), bindings = mutable.Map.empty)
