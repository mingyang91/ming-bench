package ming

/** Vector built-in procedures. */
private[ming] object VectorBuiltins:

  def evalVector(args: List[Value]): Value =
    Value.VectorVal(args.toArray)

  def evalMakeVector(args: List[Value]): Value =
    args match
      case Value.IntVal(n) :: Nil =>
        Value.VectorVal(Array.fill(n.toInt)(Value.IntVal(0)))
      case Value.IntVal(n) :: fill :: Nil =>
        Value.VectorVal(Array.fill(n.toInt)(fill))
      case _ => throw new EvalError("make-vector requires size [and fill]")

  def evalVectorRef(args: List[Value]): Value =
    args match
      case Value.VectorVal(elems) :: Value.IntVal(i) :: Nil =>
        if i < 0 || i >= elems.length then throw new EvalError("vector-ref: index out of range")
        elems(i.toInt)
      case _ => throw new EvalError("vector-ref requires vector and index")

  def evalVectorSet(args: List[Value]): Value =
    args match
      case Value.VectorVal(elems) :: Value.IntVal(i) :: v :: Nil =>
        if i < 0 || i >= elems.length then throw new EvalError("vector-set!: index out of range")
        elems(i.toInt) = v
        Value.VoidVal
      case _ =>
        throw new EvalError("vector-set! requires vector, index, and value")

  def evalVectorLength(args: List[Value]): Value =
    args match
      case Value.VectorVal(elems) :: Nil => Value.IntVal(elems.length.toLong)
      case _                             => throw new EvalError("vector-length requires a vector")

  def evalVectorToList(args: List[Value]): Value =
    args match
      case Value.VectorVal(elems) :: Nil =>
        elems.foldRight(Value.NilVal: Value)(Value.PairVal(_, _))
      case _ => throw new EvalError("vector->list requires a vector")

  def evalListToVector(args: List[Value]): Value =
    args match
      case lst :: Nil =>
        val elems = collectListToArray(lst, List.empty)
        Value.VectorVal(elems.toArray)
      case _ => throw new EvalError("list->vector requires a list")

  @scala.annotation.tailrec
  private def collectListToArray(
    v: Value,
    acc: List[Value]
  ): List[Value] =
    v match
      case Value.NilVal           => acc.reverse
      case Value.PairVal(h, t, _) => collectListToArray(t, h :: acc)
      case _                      => throw new EvalError("list->vector: not a proper list")
