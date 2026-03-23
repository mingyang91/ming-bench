package ming

/** Lexical environment for variable bindings. */
class Environment(
  private val bindings: scala.collection.mutable.Map[String, SchemeValue] = scala.collection.mutable.Map.empty,
  val parent: Option[Environment] = None
):

  def get(name: String): Option[SchemeValue] =
    bindings.get(name).orElse(parent.flatMap(_.get(name)))

  def define(name: String, value: SchemeValue): Unit =
    bindings(name) = value

  def set(name: String, value: SchemeValue): Boolean =
    if bindings.contains(name) then
      bindings(name) = value
      true
    else parent.exists(_.set(name, value))

  def child(): Environment = Environment(scala.collection.mutable.Map.empty, Some(this))
