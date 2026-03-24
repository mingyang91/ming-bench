package ming

import Evaluator.Val

/** Lexical environment with mutable bindings and parent chain. */
private[ming] class Env(
  bindings: scala.collection.mutable.Map[String, Val],
  parent: Option[Env]
):

  def lookup(name: String): Option[Val] =
    bindings.get(name).orElse(parent.flatMap(_.lookup(name)))

  def define(name: String, value: Val): Unit = bindings(name) = value

  def set(name: String, value: Val): Boolean =
    if bindings.contains(name) then
      bindings(name) = value; true
    else parent.exists(_.set(name, value))

  /** Snapshot all currently bound names (for hygiene). */
  def boundNames: Set[String] =
    val names = bindings.keySet.toSet
    parent.map(p => names ++ p.boundNames).getOrElse(names)

private[ming] object Env:

  def empty(parent: Option[Env] = None): Env =
    new Env(scala.collection.mutable.Map[String, Val](), parent)
