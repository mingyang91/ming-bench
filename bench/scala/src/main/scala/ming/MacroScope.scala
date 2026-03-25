package ming

import scala.collection.mutable

final private[ming] class MacroBinding(var value: SyntaxRules.Transformer)

final private[ming] class MacroScope private (parent: Option[MacroScope]):
  private val bindings = mutable.HashMap.empty[String, MacroBinding]

  def define(name: String, value: SyntaxRules.Transformer): Unit =
    bindings.get(name) match
      case Some(binding) => binding.value = value
      case None          => bindings.update(name, new MacroBinding(value))

  def lookup(name: String): Option[SyntaxRules.Transformer] =
    resolve(name).map(_.value)

  def resolveBinding(name: String): Option[MacroBinding] =
    resolve(name)

  def bindAlias(name: String, binding: MacroBinding): Unit =
    bindings.update(name, binding)

  private def resolve(name: String): Option[MacroBinding] =
    bindings.get(name) match
      case some @ Some(_) => some
      case None           => parent.flatMap(_.resolve(name))

private[ming] object MacroScope:

  def root(): MacroScope =
    new MacroScope(None)

  def child(parent: MacroScope): MacroScope =
    new MacroScope(Some(parent))
