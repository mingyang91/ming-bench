package ming

/** Immutable environment with lexical scoping. */
final case class Environment(
  bindings: Map[String, SchemeValue],
  parent: Option[Environment]
):

  def lookup(name: String): Option[SchemeValue] =
    bindings.get(name).orElse(parent.flatMap(_.lookup(name)))

  def define(name: String, value: SchemeValue): Environment =
    copy(bindings = bindings + (name -> value))

  def extend(
    params: List[String],
    args: List[SchemeValue]
  ): Environment =
    assert(
      params.length == args.length,
      s"param/arg mismatch: ${params.length} vs ${args.length}"
    )
    Environment(params.zip(args).toMap, Some(this))

object Environment:
  val empty: Environment = Environment(Map.empty, None)
