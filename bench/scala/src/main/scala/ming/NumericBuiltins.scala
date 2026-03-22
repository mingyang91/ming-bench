package ming

import SchemeValue.*

/** Numeric built-in procedures. */
object NumericBuiltins:

  def evalMinus(args: List[SchemeValue]): SchemeValue =
    if args.isEmpty then throw new EvalError("-: expected at least 1 argument")
    else if args.length == 1 then SchemeInt(-Builtins.asInt(args.head))
    else SchemeInt(args.map(Builtins.asInt).reduceLeft(_ - _))

  def evalDivide(args: List[SchemeValue]): SchemeValue =
    if args.length < 2 then throw new EvalError("/: expected at least 2 arguments")
    else
      val nums = args.map(Builtins.asInt)
      if nums.tail.contains(0L) then throw new EvalError("/: division by zero")
      else SchemeInt(nums.reduceLeft(_ / _))

  def compareOp(
    args: List[SchemeValue],
    cmp: (Long, Long) => Boolean
  ): SchemeValue =
    if args.length != 2 then throw new EvalError("comparison: expected 2 arguments")
    SchemeBool(cmp(Builtins.asInt(args.head), Builtins.asInt(args(1))))

  def evalAbs(args: List[SchemeValue]): SchemeValue =
    if args.length != 1 then throw new EvalError("abs: expected 1 argument")
    SchemeInt(math.abs(Builtins.asInt(args.head)))

  def evalModulo(args: List[SchemeValue]): SchemeValue =
    if args.length != 2 then throw new EvalError("modulo: expected 2 arguments")
    val (a, b) = (Builtins.asInt(args.head), Builtins.asInt(args(1)))
    if b == 0 then throw new EvalError("modulo: division by zero")
    SchemeInt(math.floorMod(a, b))

  def evalRemainder(args: List[SchemeValue]): SchemeValue =
    if args.length != 2 then throw new EvalError("remainder: expected 2 arguments")
    val (a, b) = (Builtins.asInt(args.head), Builtins.asInt(args(1)))
    if b == 0 then throw new EvalError("remainder: division by zero")
    SchemeInt(a % b)

  def evalQuotient(args: List[SchemeValue]): SchemeValue =
    if args.length != 2 then throw new EvalError("quotient: expected 2 arguments")
    val (a, b) = (Builtins.asInt(args.head), Builtins.asInt(args(1)))
    if b == 0 then throw new EvalError("quotient: division by zero")
    SchemeInt(a / b)

  def evalMinMax(args: List[SchemeValue], min: Boolean): SchemeValue =
    if args.isEmpty then throw new EvalError(s"${if min then "min" else "max"}: expected at least 1 argument")
    val nums = args.map(Builtins.asInt)
    SchemeInt(if min then nums.min else nums.max)

  def evalExpt(args: List[SchemeValue]): SchemeValue =
    if args.length != 2 then throw new EvalError("expt: expected 2 arguments")
    val (base, exp) = (Builtins.asInt(args.head), Builtins.asInt(args(1)))
    SchemeInt(longPow(base, exp))

  @scala.annotation.tailrec
  private def longPow(base: Long, exp: Long, acc: Long = 1L): Long =
    if exp <= 0 then acc
    else longPow(base, exp - 1, acc * base)

  def numPred(args: List[SchemeValue], pred: Long => Boolean): SchemeValue =
    if args.length != 1 then throw new EvalError("numeric predicate: expected 1 argument")
    SchemeBool(pred(Builtins.asInt(args.head)))
