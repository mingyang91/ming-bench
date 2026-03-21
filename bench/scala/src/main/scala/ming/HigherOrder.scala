package ming

import Evaluator.{Bounce, Done, EvalResult}

/** Higher-order built-in procedures: map and apply. */
object HigherOrder:

  def evalMap(
    args: List[Value],
    pos: Option[(Int, Int)],
    out: String
  ): EvalResult =
    if args.length < 2 then throw EvalError.withPos("map requires at least 2 arguments", pos)
    val proc                = args.head
    val lists               = args.tail.map(Evaluator.toList)
    val (results, finalOut) = mapLoop(proc, lists, List.empty, pos, out)
    val result =
      results.foldRight(Value.NilVal: Value)((a, b) => Value.MutablePairVal(Array(a, b)))
    Done(result, Env(Map.empty, None), finalOut)

  @scala.annotation.tailrec
  private def mapLoop(
    proc: Value,
    lists: List[List[Value]],
    acc: List[Value],
    pos: Option[(Int, Int)],
    out: String
  ): (List[Value], String) =
    if lists.head.isEmpty then (acc, out)
    else
      val heads = lists.map(_.head)
      val tails = lists.map(_.tail)
      val (v, _, o) =
        Evaluator.applyProcTail(proc, heads, pos, out) match
          case Done(v, e, o)     => (v, e, o)
          case Bounce(e, env, o) => Evaluator.eval(e, env, o)
      mapLoop(proc, tails, acc :+ v, pos, o)

  def evalApply(
    args: List[Value],
    pos: Option[(Int, Int)],
    out: String
  ): EvalResult =
    if args.length < 2 then
      throw EvalError.withPos(
        "apply requires at least 2 arguments",
        pos
      )
    val proc       = args.head
    val prefixArgs = args.tail.init
    val lastArg    = args.last
    val listArgs   = Evaluator.toList(lastArg)
    Evaluator.applyProcTail(proc, prefixArgs ++ listArgs, pos, out)
