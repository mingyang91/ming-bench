package ming

object DynamicWind:

  /** Find the length of the common tail of two wind stacks (by reference identity of list nodes) */
  def commonTailLength(a: List[WindEntry], b: List[WindEntry]): Int =
    val aLen = a.length
    val bLen = b.length
    var aa   = a
    var bb   = b
    if aLen > bLen then for _ <- 0 until (aLen - bLen) do aa = aa.tail
    else for _ <- 0 until (bLen - aLen) do bb = bb.tail
    while aa ne bb do
      aa = aa.tail
      bb = bb.tail
    aa.length

  def evalDynamicWind(
    inThunk: SchemeVal,
    bodyThunk: SchemeVal,
    outThunk: SchemeVal
  ): SchemeVal =
    val entry = new WindEntry(inThunk, outThunk)
    Evaluator.windStack.set(entry :: Evaluator.windStack.get())
    Evaluator.applyProc(inThunk, Nil)
    val result =
      try Evaluator.applyProc(bodyThunk, Nil)
      catch
        case cr: ContinuationReturn =>
          throw cr
        case sr: SchemeRaise =>
          Evaluator.windStack.set(Evaluator.windStack.get().tail)
          Evaluator.applyProc(outThunk, Nil)
          throw sr
    Evaluator.windStack.set(Evaluator.windStack.get().tail)
    Evaluator.applyProc(outThunk, Nil)
    result

  /** Perform wind/unwind when switching between dynamic extents */
  def doWindTransition(from: List[WindEntry], to: List[WindEntry]): Unit =
    val commonLen = commonTailLength(from, to)
    val toUnwind  = from.take(from.length - commonLen)
    for entry <- toUnwind do
      Evaluator.windStack.set(Evaluator.windStack.get().tail)
      Evaluator.applyProc(entry.outThunk, Nil)
    val toRewind = to.take(to.length - commonLen).reverse
    for entry <- toRewind do
      Evaluator.windStack.set(entry :: Evaluator.windStack.get())
      Evaluator.applyProc(entry.inThunk, Nil)
