package ming

import scala.collection.mutable

/** Lexical environment with mutable bindings for recursive definitions. */
class Env(
  private val bindings: mutable.Map[String, Value] = mutable.Map.empty,
  private val parent: Option[Env] = None
):

  def lookupOption(name: String): Option[Value] =
    bindings.get(name).orElse(parent.flatMap(_.lookupOption(name)))

  def lookup(name: String, pos: Option[Pos] = None): Value =
    bindings.get(name) match
      case Some(v) => v
      case None =>
        parent match
          case Some(p) => p.lookup(name, pos)
          case None =>
            pos match
              case Some(p) => throw new EvalError(s"unbound variable: $name [$p]")
              case None    => throw new EvalError(s"unbound variable: $name")

  def define(name: String, value: Value): Unit =
    bindings(name) = value

  def set(name: String, value: Value, pos: Option[Pos] = None): Unit =
    if bindings.contains(name) then bindings(name) = value
    else
      parent match
        case Some(p) => p.set(name, value, pos)
        case None =>
          pos match
            case Some(p) => throw new EvalError(s"unbound variable: $name [$p]")
            case None    => throw new EvalError(s"unbound variable: $name")

  def extend(names: List[String], values: List[Value]): Env =
    if names.length != values.length then
      throw new EvalError(s"expected ${names.length} arguments, got ${values.length}")
    Env(mutable.Map.from(names.zip(values)), Some(this))

  def extendWithRest(names: List[String], restParam: Option[String], values: List[Value]): Env =
    restParam match
      case None => extend(names, values)
      case Some(rest) =>
        if values.length < names.length then
          throw new EvalError(s"expected at least ${names.length} arguments, got ${values.length}")
        val (required, extra) = values.splitAt(names.length)
        val restList          = extra.foldRight(Value.NilVal: Value)((v, acc) => Pair(v, acc))
        val bindings          = mutable.Map.from(names.zip(required))
        bindings(rest) = restList
        Env(bindings, Some(this))
