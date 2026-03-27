package ming

private[ming] object ContinuationBuiltins:

  import RuntimeSupport.*

  private val callCcBuiltin =
    BuiltinValue("call/cc", callWithCurrentContinuation)

  private val dynamicWindBuiltin =
    BuiltinValue("dynamic-wind", dynamicWind)

  val values: Map[String, Value] = Map(
    "call/cc"                        -> callCcBuiltin,
    "call-with-current-continuation" -> callCcBuiltin,
    "dynamic-wind"                   -> dynamicWindBuiltin
  )

  private def callWithCurrentContinuation(
    arguments: List[Value],
    position: Position,
    continuation: Continuation
  ): EvaluationStep =
    val function = expectSingleArgument(arguments, "call/cc", position)
    InterpreterEvaluator.deferApplication(
      function,
      List(ContinuationValue(continuation, DynamicWindRuntime.captureWindFrames)),
      position,
      continuation
    )

  private def dynamicWind(
    arguments: List[Value],
    position: Position,
    continuation: Continuation
  ): EvaluationStep =
    expectExact(arguments, 3, "dynamic-wind", position) match
      case inThunk :: bodyThunk :: outThunk :: Nil =>
        DynamicWindRuntime.executeDynamicWind(
          inThunk,
          bodyThunk,
          outThunk,
          position,
          continuation
        )
      case _ =>
        throw new IllegalStateException("validated dynamic-wind argument list")
