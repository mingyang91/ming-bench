package ming

final class Binding private (thunk: () => SchemeValue):
  lazy val value: SchemeValue = thunk()

object Binding:

  def strict(value: SchemeValue): Binding =
    new Binding(() => value)

  def delayed(value: => SchemeValue): Binding =
    new Binding(() => value)

final case class Environment private (bindings: Map[String, Binding], parent: Option[Environment]):

  def extend(name: String, value: SchemeValue): Environment =
    Environment(Map(name -> Binding.strict(value)), Some(this))

  def extendMany(entries: List[(String, SchemeValue)]): Environment =
    entries match
      case Nil => this
      case _ =>
        Environment(
          entries.map { case (name, value) => name -> Binding.strict(value) }.toMap,
          Some(this)
        )

  def defineRecursive(name: String, value: => SchemeValue): Environment =
    Environment(Map(name -> Binding.delayed(value)), Some(this))

  def lookup(name: String): Option[SchemeValue] =
    bindings.get(name).map(_.value).orElse(parent.flatMap(_.lookup(name)))

object Environment:
  val empty: Environment = Environment(Map.empty, None)
