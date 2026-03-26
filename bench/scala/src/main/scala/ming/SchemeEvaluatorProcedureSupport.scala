package ming

import java.util.WeakHashMap
import java.util.concurrent.atomic.AtomicLong

import SchemeBuiltinSupport.*
import SchemeDynamicContext.*
import SchemeEvaluatorState.*
import SchemeModel.*
import SchemeRuntime.*

private[ming] trait SchemeEvaluatorProcedureSupport extends SchemeEvaluatorBuiltinProcedureSupport:

  protected def applyProcedure(
    procedure: Value,
    args: List[Value],
    continuation: Continuation,
    pos: Option[SourcePos]
  ): Computation

  private val capturedContinuations = new WeakHashMap[Value.Builtin, CapturedContinuation]()

  private val capturedContinuationImpl: List[Value] => Value =
    _ => throw new IllegalStateException("captured continuation should be handled by the evaluator")

  private val nextContinuationId = new AtomicLong()

  final protected def resetDynamicContext(): Unit =
    SchemeDynamicContext.reset()

  final protected def applyDynamicWind(
    args: List[Value],
    continuation: Continuation,
    pos: Option[SourcePos]
  ): Computation =
    requireArgCount("dynamic-wind", args, 3)
    val List(inThunk, bodyThunk, outThunk) = args
    val parentContext                      = current
    val frame                              = newFrame(inThunk, outThunk)

    applyProcedure(
      inThunk,
      Nil,
      _ =>
        evaluateDynamicWindBody(
          parentContext,
          frame,
          bodyThunk,
          outThunk,
          continuation,
          pos
        ),
      pos
    )

  final protected def applyWithExceptionHandler(
    handler: Value,
    thunk: Value,
    continuation: Continuation,
    pos: Option[SourcePos]
  ): Computation =
    val state = snapshot
    val frame = ExceptionHandlerFrame(handler, state.windContext, state.exceptionHandler)
    replaceState(state.copy(exceptionHandler = Some(frame)))
    applyProcedure(
      thunk,
      Nil,
      result =>
        replaceExceptionHandler(frame.previous)
        resume(continuation, result)
      ,
      pos
    )

  final protected def applyWithExceptionHandlerBuiltin(
    args: List[Value],
    continuation: Continuation,
    pos: Option[SourcePos]
  ): Computation =
    requireArgCount("with-exception-handler", args, 2)
    val List(handler, thunk) = args
    applyWithExceptionHandler(handler, thunk, continuation, pos)

  private def evaluateDynamicWindBody(
    parentContext: Vector[WindFrame],
    frame: WindFrame,
    bodyThunk: Value,
    outThunk: Value,
    continuation: Continuation,
    pos: Option[SourcePos]
  ): Computation =
    replace(parentContext :+ frame)
    applyProcedure(
      bodyThunk,
      Nil,
      bodyValue => exitDynamicWind(parentContext, outThunk, bodyValue, continuation, pos),
      pos
    )

  private def exitDynamicWind(
    parentContext: Vector[WindFrame],
    outThunk: Value,
    bodyValue: Value,
    continuation: Continuation,
    pos: Option[SourcePos]
  ): Computation =
    replace(parentContext)
    applyProcedure(
      outThunk,
      Nil,
      _ => resume(continuation, bodyValue),
      pos
    )

  final protected def applyCallWithCurrentContinuation(
    name: String,
    args: List[Value],
    continuation: Continuation,
    pos: Option[SourcePos]
  ): Computation =
    requireArgCount(name, args, 1)
    applyProcedure(args.head, List(captureContinuation(continuation)), continuation, pos)

  final protected def applyRaise(
    args: List[Value],
    pos: Option[SourcePos]
  ): Computation =
    requireArgCount("raise", args, 1)
    val List(exceptionValue) = args
    currentExceptionHandler match
      case Some(handlerFrame) =>
        transitionDynamicState(
          DynamicState(handlerFrame.windContext, handlerFrame.previous),
          pos
        ) {
          applyProcedure(
            handlerFrame.handler,
            List(exceptionValue),
            _ => throw new EvalError("exception handler returned"),
            pos
          )
        }
      case None =>
        throw new EvalError(s"uncaught exception: ${render(exceptionValue)}")

  final protected def applyCapturedContinuation(
    name: String,
    captured: CapturedContinuation,
    args: List[Value],
    pos: Option[SourcePos]
  ): Computation =
    val argument =
      args match
        case single :: Nil => single
        case _             => Value.MultiValues(args)
    transitionDynamicState(captured.dynamicState, pos) {
      suspend(captured.continuation(argument))
    }

  final protected def capturedContinuation(
    builtin: Value.Builtin
  ): Option[CapturedContinuation] =
    Option(capturedContinuations.get(builtin))

  final protected def captureContinuationValue(continuation: Continuation): Value =
    captureContinuation(continuation)

  private def captureContinuation(continuation: Continuation): Value =
    val builtin: Value.Builtin = Value.Builtin(
      s"continuation:${nextContinuationId.incrementAndGet()}",
      capturedContinuationImpl
    )
    capturedContinuations.put(
      builtin,
      CapturedContinuation(continuation, snapshot)
    )
    builtin

  private def transitionDynamicState(
    targetState: DynamicState,
    pos: Option[SourcePos]
  )(next: => Computation): Computation =
    val sourceState   = snapshot
    val sourceContext = sourceState.windContext
    val targetContext = targetState.windContext
    val sharedLength  = commonPrefixLength(sourceContext, targetContext)

    def exitFrames(sourceLength: Int): Computation =
      if sourceLength == sharedLength then enterFrames(sharedLength)
      else
        val frame = sourceContext(sourceLength - 1)
        replaceState(sourceState.copy(windContext = sourceContext.take(sourceLength - 1)))
        applyProcedure(
          frame.outThunk,
          Nil,
          _ => suspend(exitFrames(sourceLength - 1)),
          pos
        )

    def enterFrames(targetIndex: Int): Computation =
      if targetIndex == targetContext.length then
        replaceState(targetState)
        next
      else
        val frame = targetContext(targetIndex)
        replaceState(targetState.copy(windContext = targetContext.take(targetIndex)))
        applyProcedure(
          frame.inThunk,
          Nil,
          _ =>
            replaceState(targetState.copy(windContext = targetContext.take(targetIndex + 1)))
            suspend(enterFrames(targetIndex + 1))
          ,
          pos
        )

    exitFrames(sourceContext.length)
