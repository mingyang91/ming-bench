package ming

object ApplyBuiltins:

  def applyBuiltin: List[(String, SchemeVal)] = List(
    "apply" -> SchemeVal.BuiltinProc(
      "apply",
      args =>
        if args.length < 2 then throw new EvalError("apply: expected at least 2 arguments")
        val fn = args.head
        val lastArg = args.last match
          case v @ (SchemeVal.PairVal(_) | SchemeVal.ListVal(_)) => SchemeVal.toScalaList(v)
          case other => throw new EvalError(s"apply: last argument must be a list, got ${other.display}")
        val prefixArgs = args.slice(1, args.length - 1)
        val allArgs    = prefixArgs ++ lastArg
        Evaluator.applyProc(fn, allArgs)
    )
  )

  def valuesBuiltins: List[(String, SchemeVal)] = List(
    "values" -> SchemeVal.BuiltinProc(
      "values",
      args =>
        if args.length == 1 then args.head
        else SchemeVal.ValuesVal(args)
    ),
    "call-with-values" -> SchemeVal.BuiltinProc(
      "call-with-values",
      {
        case List(producer, consumer) =>
          val result = Evaluator.applyProc(producer, Nil)
          val consumerArgs = result match
            case SchemeVal.ValuesVal(vals) => vals
            case single                    => List(single)
          Evaluator.applyProc(consumer, consumerArgs)
        case args =>
          throw new EvalError(s"call-with-values: expected 2 arguments, got ${args.length}")
      }
    )
  )

  def callccBuiltin: List[(String, SchemeVal)] =
    val impl: List[SchemeVal] => SchemeVal = {
      case List(proc) =>
        val pending = Evaluator.pendingCCReturn.get()
        if pending != null then
          Evaluator.pendingCCReturn.set(null)
          pending
        else Evaluator.performCallCC(proc)
      case args => throw new EvalError(s"call/cc: expected 1 argument, got ${args.length}")
    }
    List(
      "call/cc"                        -> SchemeVal.BuiltinProc("call/cc", impl),
      "call-with-current-continuation" -> SchemeVal.BuiltinProc("call-with-current-continuation", impl)
    )
