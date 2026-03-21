package ming

/** Immutable environment with parent chain for lexical scoping. */
final case class Env(
  bindings: Map[String, Value],
  parent: Option[Env]
):

  def lookup(name: String): Value =
    bindings.get(name) match
      case Some(v) => v
      case None =>
        parent match
          case Some(p) => p.lookup(name)
          case None    => throw new EvalError(s"unbound variable: $name")

  def define(name: String, value: Value): Env =
    Env(bindings.updated(name, value), parent)

  def extend(params: List[String], args: List[Value]): Env =
    if params.length != args.length then
      throw new EvalError(
        s"wrong number of arguments: expected ${params.length}, got ${args.length}"
      )
    Env(params.zip(args).toMap, Some(this))
