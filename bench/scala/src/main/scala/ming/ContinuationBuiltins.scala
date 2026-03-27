package ming

private[ming] object ContinuationBuiltins:

  import RuntimeSupport.*

  private val callCcBuiltin =
    BuiltinValue("call/cc", callWithCurrentContinuation)

  val values: Map[String, Value] = Map(
    "call/cc"                        -> callCcBuiltin,
    "call-with-current-continuation" -> callCcBuiltin
  )

  private def callWithCurrentContinuation(
    arguments: List[Value],
    position: Position,
    continuation: Continuation
  ): EvaluationStep =
    val function = expectSingleArgument(arguments, "call/cc", position)
    InterpreterEvaluator.deferApplication(
      function,
      List(ContinuationValue(continuation)),
      position,
      continuation
    )
