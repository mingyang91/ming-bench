package ming

object Builtins:

  def register(env: Env, output: StringBuilder = new StringBuilder): Unit =
    NumericBuiltins.register(env)
    registerListOps(env)
    registerPredicates(env)
    registerApply(env)
    ListBuiltins.register(env)
    registerCxr(env)
    CollectionBuiltins.register(env)
    CharBuiltins.register(env)
    StringBuiltins.register(env, output)
    env.define("call/cc", SchemeVal.CallCCVal())
    env.define("call-with-current-continuation", SchemeVal.CallCCVal())
    env.define(
      "values",
      SchemeVal.BuiltinProc(
        "values",
        args =>
          if args.size == 1 then args.head
          else SchemeVal.MultipleValues(args)
      )
    )

  private def registerApply(env: Env): Unit =
    env.define(
      "apply",
      SchemeVal.BuiltinProc(
        "apply",
        args =>
          if args.size < 2 then throw new EvalError("apply: expected at least 2 arguments")
          val proc       = args.head
          val lastArg    = SchemeVal.toScalaList(args.last)
          val prefixArgs = args.slice(1, args.size - 1)
          Evaluator.apply(proc, prefixArgs ++ lastArg)
      )
    )

  private def registerListOps(env: Env): Unit =
    env.define(
      "cons",
      SchemeVal.BuiltinProc(
        "cons",
        args =>
          if args.size != 2 then throw new EvalError("cons: expected 2 arguments")
          SchemeVal.makePair(args(0), args(1))
      )
    )
    env.define(
      "car",
      SchemeVal.BuiltinProc(
        "car",
        args =>
          if args.size != 1 then throw new EvalError("car: expected 1 argument")
          args.head match
            case SchemeVal.Pair(cell)                             => cell.car
            case SchemeVal.SList(elems) if elems.nonEmpty         => elems.head
            case SchemeVal.DottedList(elems, _) if elems.nonEmpty => elems.head
            case _                                                => throw new EvalError("car: expected pair")
      )
    )
    env.define(
      "cdr",
      SchemeVal.BuiltinProc(
        "cdr",
        args =>
          if args.size != 1 then throw new EvalError("cdr: expected 1 argument")
          args.head match
            case SchemeVal.Pair(cell) => cell.cdr
            case SchemeVal.SList(elems) if elems.nonEmpty =>
              SchemeVal.SList(elems.tail)
            case SchemeVal.DottedList(elems, tail) if elems.nonEmpty =>
              if elems.tail.isEmpty then tail
              else SchemeVal.DottedList(elems.tail, tail)
            case _ => throw new EvalError("cdr: expected pair")
      )
    )
    env.define(
      "list",
      SchemeVal.BuiltinProc(
        "list",
        args =>
          if args.isEmpty then SchemeVal.SList(Nil)
          else args.foldRight(SchemeVal.SList(Nil): SchemeVal)((a, acc) => SchemeVal.makePair(a, acc))
      )
    )
    env.define(
      "length",
      SchemeVal.BuiltinProc(
        "length",
        args =>
          if args.size != 1 then throw new EvalError("length: expected 1 argument")
          args.head match
            case SchemeVal.SList(elems) => SchemeVal.IntVal(elems.size.toLong)
            case SchemeVal.Pair(_) =>
              SchemeVal.IntVal(SchemeVal.toScalaList(args.head).size.toLong)
            case _ => throw new EvalError("length: expected list")
      )
    )
    env.define(
      "null?",
      SchemeVal.BuiltinProc(
        "null?",
        args =>
          if args.size != 1 then throw new EvalError("null?: expected 1 argument")
          args.head match
            case SchemeVal.SList(elems) => SchemeVal.BoolVal(elems.isEmpty)
            case _                      => SchemeVal.BoolVal(false)
      )
    )
    env.define(
      "append",
      SchemeVal.BuiltinProc(
        "append",
        args =>
          if args.isEmpty then SchemeVal.SList(Nil)
          else if args.size == 1 then args.head
          else
            val lists = args.init.map(a => SchemeVal.toScalaList(a))
            val last  = args.last
            val flat  = lists.flatten
            if flat.isEmpty then last
            else
              last match
                case SchemeVal.SList(Nil) =>
                  flat.foldRight(SchemeVal.SList(Nil): SchemeVal)((a, acc) => SchemeVal.makePair(a, acc))
                case _ =>
                  flat.foldRight(last)((a, acc) => SchemeVal.makePair(a, acc))
      )
    )

  private def registerPredicates(env: Env): Unit =
    env.define(
      "number?",
      SchemeVal.BuiltinProc(
        "number?",
        args =>
          if args.size != 1 then throw new EvalError("number?: expected 1 argument")
          SchemeVal.BoolVal(SchemeVal.isNumeric(args.head))
      )
    )
    env.define(
      "string?",
      SchemeVal.BuiltinProc(
        "string?",
        args =>
          if args.size != 1 then throw new EvalError("string?: expected 1 argument")
          args.head match
            case SchemeVal.StringVal(_) => SchemeVal.BoolVal(true)
            case _                      => SchemeVal.BoolVal(false)
      )
    )
    env.define(
      "boolean?",
      SchemeVal.BuiltinProc(
        "boolean?",
        args =>
          if args.size != 1 then throw new EvalError("boolean?: expected 1 argument")
          args.head match
            case SchemeVal.BoolVal(_) => SchemeVal.BoolVal(true)
            case _                    => SchemeVal.BoolVal(false)
      )
    )
    env.define(
      "pair?",
      SchemeVal.BuiltinProc(
        "pair?",
        args =>
          if args.size != 1 then throw new EvalError("pair?: expected 1 argument")
          args.head match
            case SchemeVal.Pair(_)          => SchemeVal.BoolVal(true)
            case SchemeVal.SList(elems)     => SchemeVal.BoolVal(elems.nonEmpty)
            case SchemeVal.DottedList(_, _) => SchemeVal.BoolVal(true)
            case _                          => SchemeVal.BoolVal(false)
      )
    )
    env.define(
      "symbol?",
      SchemeVal.BuiltinProc(
        "symbol?",
        args =>
          if args.size != 1 then throw new EvalError("symbol?: expected 1 argument")
          args.head match
            case SchemeVal.Symbol(_) => SchemeVal.BoolVal(true)
            case _                   => SchemeVal.BoolVal(false)
      )
    )
    env.define(
      "procedure?",
      SchemeVal.BuiltinProc(
        "procedure?",
        args =>
          if args.size != 1 then throw new EvalError("procedure?: expected 1 argument")
          args.head match
            case SchemeVal.BuiltinProc(_, _)      => SchemeVal.BoolVal(true)
            case SchemeVal.LambdaProc(_, _, _, _) => SchemeVal.BoolVal(true)
            case SchemeVal.CaseLambdaProc(_, _)   => SchemeVal.BoolVal(true)
            case SchemeVal.ContinuationVal(_)     => SchemeVal.BoolVal(true)
            case SchemeVal.CallCCVal()            => SchemeVal.BoolVal(true)
            case _                                => SchemeVal.BoolVal(false)
      )
    )
    env.define(
      "not",
      SchemeVal.BuiltinProc(
        "not",
        args =>
          if args.size != 1 then throw new EvalError("not: expected 1 argument")
          args.head match
            case SchemeVal.BoolVal(false) => SchemeVal.BoolVal(true)
            case _                        => SchemeVal.BoolVal(false)
      )
    )
    env.define(
      "char?",
      SchemeVal.BuiltinProc(
        "char?",
        args =>
          if args.size != 1 then throw new EvalError("char?: expected 1 argument")
          args.head match
            case SchemeVal.CharVal(_) => SchemeVal.BoolVal(true)
            case _                    => SchemeVal.BoolVal(false)
      )
    )

  private def registerCxr(env: Env): Unit =
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
