package ming

/** Immutable environment with parent chain for lexical scoping. */
final case class Env(
  bindings: Map[String, Value],
  parent: Option[Env]
):

  def lookup(
    name: String,
    pos: Option[(Int, Int)] = None
  ): Value =
    bindings.get(name) match
      case Some(v) => v
      case None =>
        parent match
          case Some(p) => p.lookup(name, pos)
          case None =>
            throw EvalError.withPos(s"unbound variable: $name", pos)

  def define(name: String, value: Value): Env =
    Env(bindings.updated(name, value), parent)

  def extend(
    params: List[String],
    args: List[Value],
    pos: Option[(Int, Int)] = None
  ): Env =
    if params.length != args.length then
      throw EvalError.withPos(
        s"wrong number of arguments: expected ${params.length}, got ${args.length}",
        pos
      )
    Env(params.zip(args).toMap, Some(this))
