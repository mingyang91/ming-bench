package ming

import SchemeValue.*

/** Vector operations (L15). */
object BuiltinsVector:

  def vectorOp(args: List[SchemeValue]): SchemeValue =
    VectorVal(args.toArray)

  def makeVectorOp(args: List[SchemeValue]): SchemeValue =
    args match
      case IntVal(n, _) :: Nil =>
        VectorVal(Array.fill(n.toInt)(IntVal(0)))
      case IntVal(n, _) :: fill :: Nil =>
        VectorVal(Array.fill(n.toInt)(fill))
      case _ => throw new EvalError("make-vector: expected (size [fill])")

  def vectorRefOp(args: List[SchemeValue]): SchemeValue =
    args match
      case VectorVal(es, _) :: IntVal(idx, _) :: Nil =>
        if idx < 0 || idx >= es.length then throw new EvalError("vector-ref: index out of bounds")
        es(idx.toInt)
      case _ => throw new EvalError("vector-ref: expected (vector index)")

  def vectorSetOp(args: List[SchemeValue]): SchemeValue =
    args match
      case VectorVal(es, _) :: IntVal(idx, _) :: value :: Nil =>
        if idx < 0 || idx >= es.length then throw new EvalError("vector-set!: index out of bounds")
        es(idx.toInt) = value
        Void
      case _ => throw new EvalError("vector-set!: expected (vector index value)")

  def vectorLengthOp(args: List[SchemeValue]): SchemeValue =
    args match
      case VectorVal(es, _) :: Nil => IntVal(es.length.toLong)
      case _                       => throw new EvalError("vector-length: expected vector")

  def vectorCheck(args: List[SchemeValue]): SchemeValue =
    args match
      case v :: Nil => BoolVal(v.isInstanceOf[VectorVal])
      case _        => throw new EvalError("vector?: requires 1 argument")

  def vectorToListOp(args: List[SchemeValue]): SchemeValue =
    args match
      case VectorVal(es, _) :: Nil => ListVal(es.toList)
      case _                       => throw new EvalError("vector->list: expected vector")

  def listToVectorOp(args: List[SchemeValue]): SchemeValue =
    args match
      case ListVal(es, _) :: Nil => VectorVal(es.toArray)
      case _                     => throw new EvalError("list->vector: expected list")
