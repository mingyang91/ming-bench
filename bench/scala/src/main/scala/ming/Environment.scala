package ming

import scala.collection.mutable

final private[ming] class Cell(private var current: Value):

  def get: Value =
    current

  def set(value: Value): Unit =
    current = value

private[ming] object Cell:

  def apply(value: Value): Cell =
    new Cell(value)

final private[ming] class Env private (
  private val parent: Option[Env],
  private val bindings: mutable.LinkedHashMap[String, Cell]
):

  def define(name: String, value: Value): Unit =
    bindings.update(name, Cell(value))

  def defineAlias(name: String, cell: Cell): Unit =
    bindings.update(name, cell)

  def lookup(name: String, pos: SourcePos): Value =
    resolve(name)
      .map(_.get)
      .getOrElse(throw EvalError.at(pos, s"unbound variable: $name"))

  def set(name: String, value: Value, pos: SourcePos): Unit =
    resolve(name)
      .map(_.set(value))
      .getOrElse(throw EvalError.at(pos, s"unbound variable: $name"))

  def extend(names: List[String], values: List[Value]): Env =
    val child = Env.child(this)
    names.zip(values).foreach { case (name, value) =>
      child.define(name, value)
    }
    child

  def resolveCell(name: String): Option[Cell] =
    resolve(name)

  private def resolve(name: String): Option[Cell] =
    bindings.get(name).orElse(parent.flatMap(_.resolve(name)))

private[ming] object Env:

  def root(initial: Iterable[(String, Value)]): Env =
    val env = new Env(None, mutable.LinkedHashMap.empty)
    initial.foreach { case (name, value) =>
      env.define(name, value)
    }
    env

  def child(parent: Env): Env =
    new Env(Some(parent), mutable.LinkedHashMap.empty)
