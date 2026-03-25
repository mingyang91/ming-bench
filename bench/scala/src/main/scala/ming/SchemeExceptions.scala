package ming

private[ming] object SchemeExceptions:

  import BuiltinSupport.requireExactly
  import SchemeInterpreter.{EvalState, ExceptionHandlerFrame, Resume, Value}

  private type ApplyProcedureState =
    (Value, List[Value], SourcePos, Resume) => EvalState

  def raiseState(
    args: List[Value],
    pos: SourcePos,
    runtime: Runtime,
    applyProcedureState: ApplyProcedureState
  ): EvalState =
    requireExactly("raise", args, expected = 1, pos)
    args match
      case value :: Nil =>
        raiseValueState(value, pos, runtime, applyProcedureState)
      case _ =>
        throw new IllegalStateException("unreachable")

  def withExceptionHandlerState(
    args: List[Value],
    pos: SourcePos,
    cont: Resume,
    runtime: Runtime,
    applyProcedureState: ApplyProcedureState
  ): EvalState =
    requireExactly("with-exception-handler", args, expected = 2, pos)
    args match
      case handler :: thunk :: Nil =>
        val frame = ExceptionHandlerFrame(handler, runtime.windStack, cont)
        runtime.pushExceptionHandler(frame)
        applyProcedureState(
          thunk,
          Nil,
          pos,
          value =>
            // Keep handler cleanup on the trampoline so guarded tail calls stay constant-stack.
            EvalState.PopExceptionHandler(frame, value, cont)
        )
      case _ =>
        throw new IllegalStateException("unreachable")

  def raiseValueState(
    value: Value,
    pos: SourcePos,
    runtime: Runtime,
    applyProcedureState: ApplyProcedureState
  ): EvalState =
    runtime.handlerStack.lastOption match
      case Some(frame) =>
        val outerHandlers = runtime.handlerStack.dropRight(1)
        SchemeDynamicWind.transitionToState(
          frame.windStack,
          runtime,
          applyProcedureState, {
            runtime.setHandlerStack(outerHandlers)
            applyProcedureState(frame.handler, List(value), pos, frame.resume)
          }
        )
      case None =>
        throw EvalError.at(pos, s"uncaught exception: ${SchemeInterpreter.render(value)}")
