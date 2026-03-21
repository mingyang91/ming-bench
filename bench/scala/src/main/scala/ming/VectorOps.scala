package ming

/** Vector built-in operations. */
object VectorOps:

  def makeVectorOp(args: List[Value]): Value = args match
    case Value.Integer(n) :: Nil         => Value.Vec(Array.fill(n.toInt)(Value.Integer(0)))
    case Value.Integer(n) :: fill :: Nil => Value.Vec(Array.fill(n.toInt)(fill))
    case _                               => throw new EvalError("make-vector requires length and optional fill")

  def vectorRefOp(args: List[Value]): Value = args match
    case Value.Vec(elems) :: Value.Integer(idx) :: Nil =>
      if idx < 0 || idx >= elems.length then throw new EvalError("vector-ref: index out of bounds")
      else elems(idx.toInt)
    case _ => throw new EvalError("vector-ref requires vector and index")

  def vectorSetOp(args: List[Value]): Value = args match
    case Value.Vec(elems) :: Value.Integer(idx) :: v :: Nil =>
      if idx < 0 || idx >= elems.length then throw new EvalError("vector-set!: index out of bounds")
      else
        elems(idx.toInt) = v
        Value.Void
    case _ => throw new EvalError("vector-set! requires vector, index, and value")

  def vectorLengthOp(args: List[Value]): Value = args match
    case Value.Vec(elems) :: Nil => Value.Integer(elems.length.toLong)
    case _ :: Nil                => throw new EvalError("vector-length: not a vector")
    case _                       => throw new EvalError("vector-length requires 1 argument")

  def vectorToListOp(args: List[Value]): Value = args match
    case Value.Vec(elems) :: Nil => Value.SList(elems.toList)
    case _ :: Nil                => throw new EvalError("vector->list: not a vector")
    case _                       => throw new EvalError("vector->list requires 1 argument")

  def listToVectorOp(args: List[Value]): Value = args match
    case lst :: Nil =>
      NumCharOps.toScalaList(lst) match
        case Some(elems) => Value.Vec(elems.toArray)
        case None        => throw new EvalError("list->vector: not a list")
    case _ => throw new EvalError("list->vector requires 1 argument")

  def reverseOp(args: List[Value]): Value = args match
    case lst :: Nil =>
      NumCharOps.toScalaList(lst) match
        case Some(elems) => Value.SList(elems.reverse)
        case None        => throw new EvalError("reverse: not a list")
    case _ => throw new EvalError("reverse requires 1 argument")
