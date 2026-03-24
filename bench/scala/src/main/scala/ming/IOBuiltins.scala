package ming

object IOBuiltins:

  val ioBuiltins: List[(String, SchemeVal)] = List(
    "display" -> SchemeVal.BuiltinProc(
      "display",
      {
        case List(v) =>
          Evaluator.outputBuffer.get().append(v.displayStr)
          SchemeVal.Void
        case args => throw new EvalError(s"display: expected 1 argument, got ${args.length}")
      }
    ),
    "write" -> SchemeVal.BuiltinProc(
      "write",
      {
        case List(v) =>
          Evaluator.outputBuffer.get().append(v.writeStr)
          SchemeVal.Void
        case args => throw new EvalError(s"write: expected 1 argument, got ${args.length}")
      }
    ),
    "newline" -> SchemeVal.BuiltinProc(
      "newline",
      {
        case Nil =>
          Evaluator.outputBuffer.get().append("\n")
          SchemeVal.Void
        case args => throw new EvalError(s"newline: expected 0 arguments, got ${args.length}")
      }
    )
  )

  val errorBuiltin: List[(String, SchemeVal)] = List(
    "error" -> SchemeVal.BuiltinProc(
      "error",
      args =>
        val msg = args.map(_.displayStr).mkString(" ")
        throw new EvalError(s"error: $msg")
    )
  )

  val exceptionBuiltins: List[(String, SchemeVal)] = List(
    "raise" -> SchemeVal.BuiltinProc(
      "raise",
      args =>
        if args.length != 1 then throw new EvalError("raise: expected 1 argument")
        throw new SchemeRaise(args.head)
    ),
    "with-exception-handler" -> SchemeVal.BuiltinProc(
      "with-exception-handler",
      args =>
        if args.length != 2 then throw new EvalError("with-exception-handler: expected 2 arguments")
        val handler = args(0)
        val thunk   = args(1)
        try Evaluator.applyProc(thunk, Nil)
        catch
          case sr: SchemeRaise =>
            Evaluator.applyProc(handler, List(sr.value))
    )
  )

  val all: List[(String, SchemeVal)] = ioBuiltins ++ errorBuiltin ++ exceptionBuiltins
