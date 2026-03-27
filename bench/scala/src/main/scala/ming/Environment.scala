package ming

import scala.collection.mutable

final private[ming] class BindingCell(var value: Value)

final private[ming] class Environment private (
  val parent: Option[Environment],
  initialBindings: Iterable[(String, BindingCell)],
  initialMacros: Iterable[(String, MacroDefinition)]
):
  private val bindings = mutable.LinkedHashMap.from(initialBindings)
  private val macros   = mutable.LinkedHashMap.from(initialMacros)

  def define(name: String, value: Value): Unit =
    bindings.get(name) match
      case Some(cell) =>
        cell.value = value
      case None =>
        bindings.update(name, BindingCell(value))

  def reserve(name: String): Unit =
    bindings.get(name) match
      case Some(cell) =>
        cell.value = UninitializedValue
      case None =>
        bindings.update(name, BindingCell(UninitializedValue))

  def defineAlias(name: String, cell: BindingCell): Unit =
    bindings.update(name, cell)

  def defineMacro(name: String, macroDefinition: MacroDefinition): Unit =
    macros.update(name, macroDefinition)

  def assign(name: String, value: Value, position: Position): Unit =
    lookupCell(name, position).value = value

  def lookup(name: String, position: Position): Value =
    lookupCell(name, position).value match
      case UninitializedValue =>
        SchemeFailure.raise(s"uninitialized symbol: $name", position)
      case value =>
        value

  def lookupMacro(name: String): Option[MacroDefinition] =
    macros.get(name) match
      case some @ Some(_) =>
        some
      case None =>
        parent match
          case Some(outer) => outer.lookupMacro(name)
          case None        => None

  def lookupCellOption(name: String): Option[BindingCell] =
    bindings.get(name) match
      case some @ Some(_) =>
        some
      case None =>
        parent.flatMap(_.lookupCellOption(name))

  private def lookupCell(name: String, position: Position): BindingCell =
    lookupCellOption(name) match
      case Some(cell) => cell
      case None       => SchemeFailure.raise(s"unbound symbol: $name", position)

private[ming] object Environment:

  def root(bindings: Iterable[(String, Value)] = Nil): Environment =
    new Environment(None, bindings.map((name, value) => name -> BindingCell(value)), Nil)

  def child(parent: Environment, bindings: Iterable[(String, Value)] = Nil): Environment =
    new Environment(
      Some(parent),
      bindings.map((name, value) => name -> BindingCell(value)),
      Nil
    )
