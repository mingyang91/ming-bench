package ming

private[ming] object ProcedureApplicationEvaluator:

  def apply(
    function: Value,
    arguments: List[Value],
    position: Position,
    continuation: Continuation
  ): EvaluationStep =
    function match
      case BuiltinValue(_, implementation) =>
        implementation(arguments, position, continuation)
      case ClosureValue(parameters, restParameter, body, closureEnv, _) =>
        applyClosure(parameters, restParameter, body, closureEnv, arguments, position, continuation)
      case CaseLambdaValue(clauses, _) =>
        applyCaseLambda(clauses, arguments, position, continuation)
      case ContinuationValue(savedContinuation, savedWindFrames, savedHandlerFrames) =>
        applyContinuation(
          arguments,
          position,
          savedContinuation,
          savedWindFrames,
          savedHandlerFrames
        )
      case other =>
        SchemeFailure.raise(
          s"attempted to call a non-procedure value: ${other.render}",
          position
        )

  private def applyContinuation(
    arguments: List[Value],
    position: Position,
    savedContinuation: Continuation,
    savedWindFrames: List[WindFrame],
    savedHandlerFrames: List[ExceptionHandlerFrame]
  ): EvaluationStep =
    arguments match
      case value :: Nil =>
        DynamicWindRuntime.transferToContinuation(
          value,
          savedContinuation,
          savedWindFrames,
          () => ExceptionRuntime.restoreHandlerFrames(savedHandlerFrames)
        )
      case _ =>
        SchemeFailure.raise(
          s"continuation expected 1 argument(s), got ${arguments.length}",
          position
        )

  private def applyCaseLambda(
    clauses: List[ClosureValue],
    arguments: List[Value],
    position: Position,
    continuation: Continuation
  ): EvaluationStep =
    clauses.find(clauseMatches(_, arguments.length)) match
      case Some(ClosureValue(parameters, restParameter, body, closureEnv, _)) =>
        applyClosure(parameters, restParameter, body, closureEnv, arguments, position, continuation)
      case None =>
        SchemeFailure.raise(
          s"case-lambda did not match ${arguments.length} argument(s)",
          position
        )

  private def applyClosure(
    parameters: List[String],
    restParameter: Option[String],
    body: List[Expr],
    closureEnv: Environment,
    arguments: List[Value],
    position: Position,
    continuation: Continuation
  ): EvaluationStep =
    validateClosureArity(parameters, restParameter, arguments, position)
    val fixedBindings = parameters.zip(arguments)
    val bindings =
      restParameter match
        case Some(restName) =>
          fixedBindings ++ List(restName -> RuntimeSupport.buildList(arguments.drop(parameters.length)))
        case None =>
          fixedBindings
    val callEnv = Environment.child(closureEnv, bindings)
    InterpreterEvaluator.deferSequence(body, callEnv, continuation)

  private def validateClosureArity(
    parameters: List[String],
    restParameter: Option[String],
    arguments: List[Value],
    position: Position
  ): Unit =
    restParameter match
      case None =>
        if arguments.length != parameters.length then
          SchemeFailure.raise(
            s"procedure expected ${parameters.length} argument(s), got ${arguments.length}",
            position
          )
      case Some(_) =>
        if arguments.length < parameters.length then
          SchemeFailure.raise(
            s"procedure expected at least ${parameters.length} argument(s), got ${arguments.length}",
            position
          )

  private def clauseMatches(clause: ClosureValue, argumentCount: Int): Boolean =
    clause.restParameter match
      case Some(_) => argumentCount >= clause.parameters.length
      case None    => argumentCount == clause.parameters.length
