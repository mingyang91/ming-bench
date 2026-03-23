package ming

class Environment(
  private val bindings: scala.collection.mutable.Map[String, SchemeValue],
  val parent: Option[Environment]
):

  def lookup(name: String): SchemeValue =
    bindings.get(name) match
      case Some(v) => v
      case None =>
        parent match
          case Some(p) => p.lookup(name)
          case None    => throw new EvalError(s"unbound variable: $name")

  def define(name: String, value: SchemeValue): Unit =
    bindings(name) = value

  def extend(params: List[String], args: List[SchemeValue]): Environment =
    val m = scala.collection.mutable.Map[String, SchemeValue]()
    params.zip(args).foreach((k, v) => m(k) = v)
    Environment(m, Some(this))

object Environment:
  def empty: Environment = Environment(scala.collection.mutable.Map.empty, None)
