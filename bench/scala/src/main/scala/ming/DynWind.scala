package ming

/** Dynamic-wind stack management and wind/unwind action computation. */
object DynWind:

  /** Compute wind actions to transition from current to target wind stack. Returns list of (isUnwind, entry) pairs:
    * unwinds first (innermost→outermost), then rewinds (outermost→innermost).
    */
  def computeWindActions(current: List[WindEntry], target: List[WindEntry]): List[(Boolean, WindEntry)] =
    val cLen = current.length
    val tLen = target.length
    // Align lists to same length, then walk until shared tail found
    var c = if cLen > tLen then current.drop(cLen - tLen) else current
    var t = if tLen > cLen then target.drop(tLen - cLen) else target
    while (c ne t) && c.nonEmpty do
      c = c.tail
      t = t.tail
    val commonLen = c.length
    val toUnwind  = current.take(cLen - commonLen)        // innermost first
    val toRewind  = target.take(tLen - commonLen).reverse // outermost first
    toUnwind.map((true, _)) ++ toRewind.map((false, _))

  /** Start executing wind actions, or deliver the final value if none remain. */
  def startWindActions(
    actions: List[(Boolean, WindEntry)],
    value: SchemeVal,
    targetK: Cont,
    targetWinds: List[WindEntry]
  )(performApply: (SchemeVal, List[SchemeVal], Cont) => Evaluator.State): Evaluator.State =
    actions match
      case Nil =>
        Evaluator.windStack.set(targetWinds)
        Evaluator.State.Ko(value, targetK)
      case (isUnwind, entry) :: rest =>
        if isUnwind then
          // Pop entry from wind stack before calling out-thunk
          val ws = Evaluator.windStack.get()
          if ws.nonEmpty && (ws.head eq entry) then Evaluator.windStack.set(ws.tail)
          performApply(entry.outThunk, Nil, Cont.WindContinueK(rest, value, targetK, targetWinds))
        else
          // Call in-thunk; WindPushK will push entry after it completes
          performApply(entry.inThunk, Nil, Cont.WindPushK(entry, rest, value, targetK, targetWinds))
