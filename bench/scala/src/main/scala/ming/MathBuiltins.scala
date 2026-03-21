package ming

/** Arithmetic and numeric built-in procedures. */
object MathBuiltins:

  private[ming] def asInt(
    v: Value,
    pos: Option[(Int, Int)]
  ): Long = v match
    case Value.IntVal(n) => n
    case other =>
      throw EvalError.withPos(
        s"expected integer, got: ${other.display}",
        pos
      )

  def evalAdd(
    args: List[Value],
    pos: Option[(Int, Int)]
  ): Value =
    Value.IntVal(args.foldLeft(0L)((acc, v) => acc + asInt(v, pos)))

  def evalSub(
    args: List[Value],
    pos: Option[(Int, Int)]
  ): Value = args match
    case Nil         => throw new EvalError("- requires at least 1 argument")
    case head :: Nil => Value.IntVal(-asInt(head, pos))
    case head :: tail =>
      Value.IntVal(
        tail.foldLeft(asInt(head, pos))((a, v) => a - asInt(v, pos))
      )

  def evalMul(
    args: List[Value],
    pos: Option[(Int, Int)]
  ): Value =
    Value.IntVal(args.foldLeft(1L)((acc, v) => acc * asInt(v, pos)))

  def evalDiv(
    args: List[Value],
    pos: Option[(Int, Int)]
  ): Value = args match
    case Nil =>
      throw new EvalError("/ requires at least 1 argument")
    case head :: Nil =>
      val d = asInt(head, pos)
      if d == 0L then throw EvalError.withPos("division by zero", pos)
      else Value.IntVal(1L / d)
    case head :: tail =>
      Value.IntVal(tail.foldLeft(asInt(head, pos)) { (a, v) =>
        val d = asInt(v, pos)
        if d == 0L then throw EvalError.withPos("division by zero", pos)
        else a / d
      })

  def evalCmp(
    args: List[Value],
    op: (Long, Long) => Boolean,
    pos: Option[(Int, Int)]
  ): Value =
    args match
      case a :: b :: Nil =>
        Value.BoolVal(op(asInt(a, pos), asInt(b, pos)))
      case _ =>
        throw new EvalError(
          "comparison requires exactly 2 arguments"
        )

  def evalAbs(args: List[Value], pos: Option[(Int, Int)]): Value =
    args match
      case v :: Nil => Value.IntVal(math.abs(asInt(v, pos)))
      case _        => throw new EvalError("abs requires exactly 1 argument")

  def evalModulo(args: List[Value], pos: Option[(Int, Int)]): Value =
    args match
      case a :: b :: Nil =>
        val x = asInt(a, pos); val y = asInt(b, pos)
        if y == 0L then throw EvalError.withPos("division by zero", pos)
        Value.IntVal(math.floorMod(x, y))
      case _ => throw new EvalError("modulo requires exactly 2 arguments")

  def evalRemainder(args: List[Value], pos: Option[(Int, Int)]): Value =
    args match
      case a :: b :: Nil =>
        val x = asInt(a, pos); val y = asInt(b, pos)
        if y == 0L then throw EvalError.withPos("division by zero", pos)
        Value.IntVal(x % y)
      case _ => throw new EvalError("remainder requires exactly 2 arguments")

  def evalQuotient(args: List[Value], pos: Option[(Int, Int)]): Value =
    args match
      case a :: b :: Nil =>
        val x = asInt(a, pos); val y = asInt(b, pos)
        if y == 0L then throw EvalError.withPos("division by zero", pos)
        Value.IntVal(x / y)
      case _ => throw new EvalError("quotient requires exactly 2 arguments")

  def evalMinMax(
    args: List[Value],
    pos: Option[(Int, Int)],
    op: (Long, Long) => Long
  ): Value =
    args match
      case Nil => throw new EvalError("min/max requires at least 1 argument")
      case head :: tail =>
        Value.IntVal(
          tail.foldLeft(asInt(head, pos))((a, v) => op(a, asInt(v, pos)))
        )

  def evalExpt(args: List[Value], pos: Option[(Int, Int)]): Value =
    args match
      case base :: exp :: Nil =>
        val b = asInt(base, pos); val e = asInt(exp, pos)
        Value.IntVal(longPow(b, e))
      case _ => throw new EvalError("expt requires exactly 2 arguments")

  @scala.annotation.tailrec
  private def longPow(base: Long, exp: Long, acc: Long = 1L): Long =
    if exp <= 0L then acc
    else longPow(base, exp - 1L, acc * base)

  def numPred(
    args: List[Value],
    pos: Option[(Int, Int)],
    pred: Long => Boolean
  ): Value =
    args match
      case v :: Nil => Value.BoolVal(pred(asInt(v, pos)))
      case _ =>
        throw new EvalError(
          "numeric predicate requires exactly 1 argument"
        )
