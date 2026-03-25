package ming

object Builtins:

  def register(env: Env, output: StringBuilder = new StringBuilder): Unit =
    NumericBuiltins.register(env)
    registerListOps(env)
    registerPredicates(env)
    registerApply(env)
    registerListUtils(env)
    CollectionBuiltins.register(env)
    CharBuiltins.register(env)
    StringBuiltins.register(env, output)

  private def registerApply(env: Env): Unit =
    env.define(
      "apply",
      SchemeVal.BuiltinProc(
        "apply",
        args =>
          if args.size < 2 then throw new EvalError("apply: expected at least 2 arguments")
          val proc = args.head
          val lastArg = args.last match
            case SchemeVal.SList(elems) => elems
            case other =>
              throw new EvalError(
                s"apply: last argument must be a list, got ${SchemeVal.display(other)}"
              )
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
          args(1) match
            case SchemeVal.SList(elems)            => SchemeVal.SList(args(0) :: elems)
            case SchemeVal.DottedList(elems, tail) => SchemeVal.DottedList(args(0) :: elems, tail)
            case _                                 => SchemeVal.DottedList(List(args(0)), args(1))
      )
    )
    env.define(
      "car",
      SchemeVal.BuiltinProc(
        "car",
        args =>
          if args.size != 1 then throw new EvalError("car: expected 1 argument")
          args.head match
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
            case SchemeVal.SList(elems) if elems.nonEmpty => SchemeVal.SList(elems.tail)
            case SchemeVal.DottedList(elems, tail) if elems.nonEmpty =>
              if elems.tail.isEmpty then tail
              else SchemeVal.DottedList(elems.tail, tail)
            case _ => throw new EvalError("cdr: expected pair")
      )
    )
    env.define("list", SchemeVal.BuiltinProc("list", args => SchemeVal.SList(args)))
    env.define(
      "length",
      SchemeVal.BuiltinProc(
        "length",
        args =>
          if args.size != 1 then throw new EvalError("length: expected 1 argument")
          args.head match
            case SchemeVal.SList(elems) => SchemeVal.IntVal(elems.size.toLong)
            case _                      => throw new EvalError("length: expected list")
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
          else
            val lists = args.map {
              case SchemeVal.SList(elems) => elems
              case other =>
                throw new EvalError(s"append: expected list, got ${SchemeVal.display(other)}")
            }
            SchemeVal.SList(lists.flatten)
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

  private def registerListUtils(env: Env): Unit =
    env.define(
      "list-ref",
      SchemeVal.BuiltinProc(
        "list-ref",
        args =>
          if args.size != 2 then throw new EvalError("list-ref: expected 2 arguments")
          (args(0), args(1)) match
            case (SchemeVal.SList(elems), SchemeVal.IntVal(i)) =>
              if i < 0 || i >= elems.size then throw new EvalError("list-ref: index out of bounds")
              elems(i.toInt)
            case _ => throw new EvalError("list-ref: expected list and integer")
      )
    )
    env.define(
      "list-tail",
      SchemeVal.BuiltinProc(
        "list-tail",
        args =>
          if args.size != 2 then throw new EvalError("list-tail: expected 2 arguments")
          (args(0), args(1)) match
            case (SchemeVal.SList(elems), SchemeVal.IntVal(i)) =>
              if i < 0 || i > elems.size then throw new EvalError("list-tail: index out of bounds")
              SchemeVal.SList(elems.drop(i.toInt))
            case _ => throw new EvalError("list-tail: expected list and integer")
      )
    )
    env.define(
      "list?",
      SchemeVal.BuiltinProc(
        "list?",
        args =>
          if args.size != 1 then throw new EvalError("list?: expected 1 argument")
          args.head match
            case SchemeVal.SList(_) => SchemeVal.BoolVal(true)
            case _                  => SchemeVal.BoolVal(false)
      )
    )
    env.define(
      "assoc",
      SchemeVal.BuiltinProc(
        "assoc",
        args =>
          if args.size != 2 then throw new EvalError("assoc: expected 2 arguments")
          val key = args(0)
          args(1) match
            case SchemeVal.SList(elems) =>
              elems
                .collectFirst {
                  case found @ SchemeVal.SList(pair)
                      if pair.nonEmpty && SchemeVal.schemeEqual(
                        pair.head,
                        key
                      ) =>
                    found
                }
                .getOrElse(SchemeVal.BoolVal(false))
            case _ => throw new EvalError("assoc: expected list")
      )
    )
