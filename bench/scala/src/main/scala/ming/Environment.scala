package ming

import java.util.{HashMap => JHashMap}

/** Environment with lexical scoping. Each scope owns a mutable HashMap
  * so that set! mutations are visible to all closures sharing a scope.
  * Scope structure (parent/fallback chains) is immutable.
  */
final class Environment(
  private val bindings: JHashMap[String, SchemeValue],
  val parent: Option[Environment],
  val fallback: Option[Environment]
):

  def lookup(name: String): Option[SchemeValue] =
    val v = bindings.get(name)
    if v != null then Some(v)
    else
      parent
        .flatMap(_.lookup(name))
        .orElse(fallback.flatMap(_.lookup(name)))

  /** Add or overwrite a binding in the current scope. Returns this. */
  def define(name: String, value: SchemeValue): Environment =
    bindings.put(name, value)
    this

  /** Mutate an existing binding, walking the scope chain.
    * Returns true if found and mutated, false if unbound.
    */
  def set(name: String, value: SchemeValue): Boolean =
    if bindings.containsKey(name) then
      bindings.put(name, value)
      true
    else
      parent.exists(_.set(name, value)) ||
      fallback.exists(_.set(name, value))

  /** Create a child scope with the given parameter bindings. */
  def extend(
    params: List[String],
    args: List[SchemeValue],
    fb: Option[Environment] = None
  ): Environment =
    assert(
      params.length == args.length,
      s"param/arg mismatch: ${params.length} vs ${args.length}"
    )
    val newMap = new JHashMap[String, SchemeValue](params.length * 2)
    params.zip(args).foreach((k, v) => newMap.put(k, v))
    new Environment(newMap, Some(this), fb)

object Environment:
  val empty: Environment =
    new Environment(new JHashMap[String, SchemeValue](), None, None)
