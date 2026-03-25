package ming

import scala.collection.mutable

final private[ming] class EnvBinding(var value: SchemeInterpreter.Value)

final private[ming] class Env private (parent: Option[Env]):
  import SchemeInterpreter.Value

  private val bindings = mutable.HashMap.empty[String, EnvBinding]

  def define(name: String, value: Value): Unit =
    bindings.get(name) match
      case Some(binding) => binding.value = value
      case None          => bindings.update(name, new EnvBinding(value))

  def assign(name: String, value: Value, pos: SourcePos): Unit =
    resolve(name) match
      case Some(binding) => binding.value = value
      case None          => throw EvalError.at(pos, s"unbound variable: $name")

  def lookup(name: String, pos: SourcePos): Value =
    resolve(name) match
      case Some(binding) => binding.value
      case None          => throw EvalError.at(pos, s"unbound variable: $name")

  def resolveBinding(name: String): Option[EnvBinding] =
    resolve(name)

  def bindAlias(name: String, binding: EnvBinding): Unit =
    bindings.update(name, binding)

  private def resolve(name: String): Option[EnvBinding] =
    bindings.get(name) match
      case some @ Some(_) => some
      case None           => parent.flatMap(_.resolve(name))

private[ming] object Env:
  import SchemeInterpreter.Value

  def root(): Env =
    new Env(None)

  def child(parent: Env, bindings: Iterable[(String, Value)]): Env =
    val env = new Env(Some(parent))
    bindings.foreach { case (name, value) => env.define(name, value) }
    env
