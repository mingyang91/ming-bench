package ming

import scala.collection.mutable

final private[ming] class BindingCell(var value: Value)

final private[ming] class Env private (
  private val parent: Option[Env],
  private val bindings: mutable.Map[String, BindingCell],
  private val syntaxBindings: mutable.Map[String, SyntaxTransformer]
):

  def child(): Env =
    Env.childOf(this)

  def define(name: String, value: Value): Unit =
    bindings.update(name, BindingCell(value))

  def defineAlias(name: String, cell: BindingCell): Unit =
    bindings.update(name, cell)

  def assign(name: String, value: Value): Boolean =
    lookupValueCell(name) match
      case Some(cell) =>
        cell.value = value
        true
      case None =>
        false

  def lookup(name: String): Option[Value] =
    lookupValueCell(name).map(_.value).orElse(Builtins.resolve(name))

  def lookupValueCell(name: String): Option[BindingCell] =
    bindings
      .get(name)
      .orElse(parent.flatMap(_.lookupValueCell(name)))

  def defineSyntax(name: String, transformer: SyntaxTransformer): Unit =
    syntaxBindings.update(name, transformer)

  def lookupSyntax(name: String): Option[SyntaxTransformer] =
    syntaxBindings
      .get(name)
      .orElse(parent.flatMap(_.lookupSyntax(name)))

private[ming] object Env:

  def topLevel(): Env =
    new Env(parent = None, bindings = mutable.Map.empty, syntaxBindings = mutable.Map.empty)

  def childOf(parent: Env): Env =
    new Env(parent = Some(parent), bindings = mutable.Map.empty, syntaxBindings = mutable.Map.empty)
