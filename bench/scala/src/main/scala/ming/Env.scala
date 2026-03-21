package ming

final case class Env(bindings: Map[String, () => Value], parent: Option[Env]):

  def lookup(name: String): Option[Value] =
    bindings.get(name).map(_()).orElse(parent.flatMap(_.lookup(name)))

  def define(name: String, value: => Value): Env =
    lazy val cached = value
    copy(bindings = bindings.updated(name, () => cached))

  def extend(entries: List[(String, Value)]): Env =
    Env(
      entries.foldLeft(Map.empty[String, () => Value]) { case (acc, (name, value)) =>
        lazy val cached = value
        acc.updated(name, () => cached)
      },
      Some(this)
    )

object Env:

  val empty: Env = Env(Map.empty, None)
