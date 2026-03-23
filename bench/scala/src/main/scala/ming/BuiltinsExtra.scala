package ming

import Value.*

/** Extra built-in procedures: gcd, lcm, rounding, make-string, void. */
object BuiltinsExtra:

  def register(env: Env): Unit =
    Builtins.define(env, extraOps)

  private def extraOps: List[(String, List[Value] => Value)] =
    List(
      (
        "gcd",
        args =>
          val nums = BuiltinsArith.requireInts(args).map(Math.abs(_))
          IntVal(nums.foldLeft(0L)((a, b) => gcdLong(a, b)))
      ),
      (
        "lcm",
        args =>
          val nums = BuiltinsArith.requireInts(args).map(Math.abs(_))
          IntVal(nums.foldLeft(1L)((a, b) => if b == 0 then 0 else a / gcdLong(a, b) * b))
      ),
      (
        "truncate",
        args =>
          if args.length != 1 then throw new EvalError("truncate: expected 1 argument")
          args.head match
            case IntVal(n)   => IntVal(n)
            case FloatVal(d) => IntVal(d.toLong)
            case _           => throw new EvalError("truncate: not a number")
      ),
      (
        "round",
        args =>
          if args.length != 1 then throw new EvalError("round: expected 1 argument")
          args.head match
            case IntVal(n)   => IntVal(n)
            case FloatVal(d) => IntVal(Math.round(d))
            case _           => throw new EvalError("round: not a number")
      ),
      (
        "floor",
        args =>
          if args.length != 1 then throw new EvalError("floor: expected 1 argument")
          args.head match
            case IntVal(n)   => IntVal(n)
            case FloatVal(d) => IntVal(Math.floor(d).toLong)
            case _           => throw new EvalError("floor: not a number")
      ),
      (
        "ceiling",
        args =>
          if args.length != 1 then throw new EvalError("ceiling: expected 1 argument")
          args.head match
            case IntVal(n)   => IntVal(n)
            case FloatVal(d) => IntVal(Math.ceil(d).toLong)
            case _           => throw new EvalError("ceiling: not a number")
      ),
      (
        "make-string",
        args =>
          args match
            case IntVal(n) :: Nil =>
              StrVal(Array.fill(n.toInt)('\u0000'))
            case IntVal(n) :: CharVal(c) :: Nil =>
              StrVal(Array.fill(n.toInt)(c))
            case _ => throw new EvalError("make-string: expected (length) or (length, char)")
      ),
      (
        "string",
        args =>
          val chars = args.map {
            case CharVal(c) => c
            case _          => throw new EvalError("string: expected characters")
          }
          StrVal(chars.toArray)
      ),
      (
        "void",
        _ => VoidVal
      )
    )

  private def gcdLong(a: Long, b: Long): Long =
    if b == 0 then a else gcdLong(b, a % b)
