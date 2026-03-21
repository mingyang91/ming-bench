package ming

import scala.annotation.tailrec

/** Environment with mutable cells for lexical scoping and set! support.
  *
  * Each binding is a single-element Array[Value] acting as a mutable cell. Closures that share a lexical scope share
  * the same cell, so set! in one closure is visible in another.
  */
final case class Env(
  bindings: Map[String, Array[Value]],
  parent: Option[Env]
):

  def lookup(
    name: String,
    pos: Option[(Int, Int)] = None
  ): Value =
    bindings.get(name) match
      case Some(cell) => cell(0)
      case None =>
        parent match
          case Some(p) => p.lookup(name, pos)
          case None =>
            throw EvalError.withPos(s"unbound variable: $name", pos)

  def define(name: String, value: Value): Env =
    Env(bindings.updated(name, Array(value)), parent)

  /** Mutate an existing binding. Walks the parent chain to find the cell. */
  @tailrec
  def set(
    name: String,
    value: Value,
    pos: Option[(Int, Int)] = None
  ): Unit =
    bindings.get(name) match
      case Some(cell) => cell(0) = value
      case None =>
        parent match
          case Some(p) => p.set(name, value, pos)
          case None =>
            throw EvalError.withPos(
              s"set!: unbound variable: $name",
              pos
            )

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
    Env(params.zip(args.map(v => Array(v))).toMap, Some(this))

  def extendVariadic(
    params: List[String],
    restParam: Option[String],
    args: List[Value],
    pos: Option[(Int, Int)] = None
  ): Env =
    restParam match
      case None => extend(params, args, pos)
      case Some(rest) =>
        if args.length < params.length then
          throw EvalError.withPos(
            s"wrong number of arguments: expected at least ${params.length}, got ${args.length}",
            pos
          )
        val (fixed, remaining) = args.splitAt(params.length)
        val restList =
          remaining.foldRight(Value.NilVal: Value)(Value.PairVal(_, _))
        val allParams = params :+ rest
        val allArgs   = fixed :+ restList
        Env(allParams.zip(allArgs.map(v => Array(v))).toMap, Some(this))
