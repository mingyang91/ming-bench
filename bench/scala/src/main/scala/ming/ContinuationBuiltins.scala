package ming

private[ming] object ContinuationBuiltins:

  import RuntimeSupport.*

  private val callCcBuiltin =
    BuiltinValue("call/cc", callWithCurrentContinuation)

  private val callWithValuesBuiltin =
    BuiltinValue("call-with-values", callWithValues)

  private val dynamicWindBuiltin =
    BuiltinValue("dynamic-wind", dynamicWind)

  private val errorBuiltin =
    BuiltinValue("error", error)

  private val raiseBuiltin =
    BuiltinValue("raise", raise)

  private val valuesBuiltin =
    BuiltinValue("values", valuesProcedure)

  private val withExceptionHandlerBuiltin =
    BuiltinValue("with-exception-handler", withExceptionHandler)

  val values: Map[String, Value] = Map(
    "call/cc"                        -> callCcBuiltin,
    "call-with-values"               -> callWithValuesBuiltin,
    "call-with-current-continuation" -> callCcBuiltin,
    "dynamic-wind"                   -> dynamicWindBuiltin,
    "error"                          -> errorBuiltin,
    "raise"                          -> raiseBuiltin,
    "values"                         -> valuesBuiltin,
    "with-exception-handler"         -> withExceptionHandlerBuiltin
  )

  private def callWithCurrentContinuation(
    arguments: List[Value],
    position: Position,
    continuation: Continuation
  ): EvaluationStep =
    val function = expectSingleArgument(arguments, "call/cc", position)
    InterpreterEvaluator.deferApplication(
      function,
      List(
        ContinuationValue(
          continuation,
          DynamicWindRuntime.captureWindFrames,
          ExceptionRuntime.captureHandlerFrames
        )
      ),
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

  private def valuesProcedure(arguments: List[Value], position: Position): Value =
    arguments match
      case value :: Nil =>
        value
      case _ =>
        MultipleValuesValue(arguments)

  private def callWithValues(
    arguments: List[Value],
    position: Position,
    continuation: Continuation
  ): EvaluationStep =
    expectExact(arguments, 2, "call-with-values", position) match
      case producer :: consumer :: Nil =>
        InterpreterEvaluator.deferApplication(
          producer,
          Nil,
          position,
          produced =>
            InterpreterEvaluator.deferApplication(
              consumer,
              unpackValues(produced),
              position,
              continuation
            )
        )
      case _ =>
        throw new IllegalStateException("validated call-with-values argument list")

  private def raise(
    arguments: List[Value],
    position: Position,
    continuation: Continuation
  ): EvaluationStep =
    val value = expectSingleArgument(arguments, "raise", position)
    ExceptionRuntime.raiseValue(value, position)

  private def error(
    arguments: List[Value],
    position: Position,
    continuation: Continuation
  ): EvaluationStep =
    val allArguments = expectAtLeast(arguments, 1, "error", position)
    val message      = allArguments.iterator.map(_.renderDisplay).mkString
    ExceptionRuntime.raiseValue(StringValue(message), position)

  private def withExceptionHandler(
    arguments: List[Value],
    position: Position,
    continuation: Continuation
  ): EvaluationStep =
    expectExact(arguments, 2, "with-exception-handler", position) match
      case handler :: thunk :: Nil =>
        val frame = ExceptionHandlerFrame(
          exception =>
            InterpreterEvaluator.deferApplication(
              handler,
              List(exception.value),
              position,
              continuation
            ),
          DynamicWindRuntime.captureWindFrames
        )

        ExceptionRuntime.pushHandler(frame)
        InterpreterEvaluator.deferApplication(
          thunk,
          Nil,
          position,
          result =>
            ExceptionRuntime.popHandler(frame)
            InterpreterEvaluator.done(result, continuation)
        )
      case _ =>
        throw new IllegalStateException("validated with-exception-handler argument list")
