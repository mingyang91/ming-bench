package ming

import Value.*
import EvalHelpers.{evalError, valueToList}

/** Dynamic-wind support and CPS wrappers for apply/map — mixed into Evaluator. */
private[ming] trait EvalWind:
  import Evaluator.{K, WindEntry}

  protected def windStack: List[WindEntry]
  protected def windStack_=(ws: List[WindEntry]): Unit
  protected def applyProc(proc: Value, values: List[Value], pos: Option[Pos], k: K): Bounce
  protected def trampoline(thunk: => Bounce): Bounce

  protected def applyDynamicWind(
    inProc: Value,
    bodyProc: Value,
    outProc: Value,
    pos: Option[Pos],
    k: K
  ): Bounce =
    applyProc(
      inProc,
      Nil,
      pos,
      { _ =>
        windStack = WindEntry(inProc, outProc) :: windStack
        applyProc(
          bodyProc,
          Nil,
          pos,
          { bodyResult =>
            windStack = windStack.tail
            applyProc(outProc, Nil, pos, _ => k(bodyResult))
          }
        )
      }
    )

  /** Find common tail length of two wind stacks (shared persistent list suffix). */
  private def commonTailLength(a: List[WindEntry], b: List[WindEntry]): Int =
    var la = a; var lb = b
    if la.length > lb.length then la = la.drop(la.length - lb.length)
    else lb = lb.drop(lb.length - la.length)
    while (la ne lb) && la.nonEmpty do
      la = la.tail; lb = lb.tail
    la.length

  /** Transition wind stack from current to target, calling out/in thunks as needed. */
  protected def doWindTransition(target: List[WindEntry], pos: Option[Pos], andThen: () => Bounce): Bounce =
    val current   = windStack
    val commonLen = commonTailLength(current, target)
    val toUnwind  = current.take(current.length - commonLen)
    val toRewind  = target.take(target.length - commonLen).reverse

    def unwind(entries: List[WindEntry], next: () => Bounce): Bounce =
      entries match
        case Nil => next()
        case entry :: rest =>
          windStack = windStack.tail
          applyProc(entry.outThunk, Nil, pos, _ => trampoline(unwind(rest, next)))

    def rewind(entries: List[WindEntry], next: () => Bounce): Bounce =
      entries match
        case Nil => next()
        case entry :: rest =>
          windStack = entry :: windStack
          applyProc(entry.inThunk, Nil, pos, _ => trampoline(rewind(rest, next)))

    unwind(toUnwind, () => rewind(toRewind, andThen))

  protected def applyBuiltinApply(args: List[Value], pos: Option[Pos], k: K): Bounce =
    if args.length < 2 then evalError("apply: expected at least 2 arguments", pos)
    val proc       = args.head
    val prefixArgs = args.slice(1, args.length - 1).toList
    val trailing   = valueToList(args.last)
    applyProc(proc, prefixArgs ++ trailing, pos, k)

  protected def applyBuiltinMap(args: List[Value], pos: Option[Pos], k: K): Bounce =
    if args.length < 2 then evalError("map: expected at least 2 arguments", pos)
    val proc  = args.head
    val lists = args.tail.map(EvalHelpers.valueToList)
    mapLoop(proc, lists, Nil, pos, k)

  private def mapLoop(
    proc: Value,
    lists: List[List[Value]],
    acc: List[Value],
    pos: Option[Pos],
    k: K
  ): Bounce =
    if lists.head.isEmpty then
      val result = acc.reverse.foldRight(Value.NilVal: Value)((v, t) => Value.PairVal(v, t))
      k(result)
    else
      val heads = lists.map(_.head)
      val tails = lists.map(_.tail)
      applyProc(proc, heads, pos, v => trampoline(mapLoop(proc, tails, v :: acc, pos, k)))
