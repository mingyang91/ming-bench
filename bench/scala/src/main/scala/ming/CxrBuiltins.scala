package ming

object CxrBuiltins:

  def register(env: Env): Unit =
    env.define(
      "caar",
      SchemeVal.BuiltinProc(
        "caar",
        args =>
          if args.size != 1 then throw new EvalError("caar: expected 1 argument")
          pairCar(pairCar(args.head))
      )
    )
    env.define(
      "cadr",
      SchemeVal.BuiltinProc(
        "cadr",
        args =>
          if args.size != 1 then throw new EvalError("cadr: expected 1 argument")
          pairCar(pairCdr(args.head))
      )
    )
    env.define(
      "cdar",
      SchemeVal.BuiltinProc(
        "cdar",
        args =>
          if args.size != 1 then throw new EvalError("cdar: expected 1 argument")
          pairCdr(pairCar(args.head))
      )
    )
    env.define(
      "cddr",
      SchemeVal.BuiltinProc(
        "cddr",
        args =>
          if args.size != 1 then throw new EvalError("cddr: expected 1 argument")
          pairCdr(pairCdr(args.head))
      )
    )
    env.define(
      "caddr",
      SchemeVal.BuiltinProc(
        "caddr",
        args =>
          if args.size != 1 then throw new EvalError("caddr: expected 1 argument")
          pairCar(pairCdr(pairCdr(args.head)))
      )
    )
    env.define(
      "cadddr",
      SchemeVal.BuiltinProc(
        "cadddr",
        args =>
          if args.size != 1 then throw new EvalError("cadddr: expected 1 argument")
          pairCar(pairCdr(pairCdr(pairCdr(args.head))))
      )
    )

  private def pairCar(v: SchemeVal): SchemeVal = v match
    case SchemeVal.Pair(c)                                => c.car
    case SchemeVal.SList(elems) if elems.nonEmpty         => elems.head
    case SchemeVal.DottedList(elems, _) if elems.nonEmpty => elems.head
    case _                                                => throw new EvalError("car: not a pair")

  private def pairCdr(v: SchemeVal): SchemeVal = v match
    case SchemeVal.Pair(c) => c.cdr
    case SchemeVal.SList(elems) if elems.nonEmpty =>
      SchemeVal.SList(elems.tail)
    case SchemeVal.DottedList(elems, tail) if elems.nonEmpty =>
      if elems.tail.isEmpty then tail else SchemeVal.DottedList(elems.tail, tail)
    case _ => throw new EvalError("cdr: not a pair")
