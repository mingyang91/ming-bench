package ming

import scala.util.DynamicVariable

final private[ming] case class SchemeException(value: Value, position: Position)

final private[ming] class ExceptionHandlerFrame(
  val handler: SchemeException => EvaluationStep,
  val windFrames: List[WindFrame]
)

private[ming] object ExceptionHandlerFrame:

  def apply(
    handler: SchemeException => EvaluationStep,
    windFrames: List[WindFrame]
  ): ExceptionHandlerFrame =
    new ExceptionHandlerFrame(handler, windFrames)

private[ming] object ExceptionRuntime:

  final private class ExceptionState(var handlers: List[ExceptionHandlerFrame])

  private val activeExceptionState = DynamicVariable[Option[ExceptionState]](None)

  def withExceptionState[A](thunk: => A): A =
    activeExceptionState.value match
      case Some(_) =>
        thunk
      case None =>
        activeExceptionState.withValue(Some(ExceptionState(Nil)))(thunk)

  def captureHandlerFrames: List[ExceptionHandlerFrame] =
    exceptionState.handlers

  def restoreHandlerFrames(handlers: List[ExceptionHandlerFrame]): Unit =
    exceptionState.handlers = handlers

  def pushHandler(frame: ExceptionHandlerFrame): Unit =
    exceptionState.handlers = frame :: exceptionState.handlers

  def popHandler(expected: ExceptionHandlerFrame): Unit =
    exceptionState.handlers match
      case frame :: rest if frame eq expected =>
        exceptionState.handlers = rest
      case _ =>
        throw new IllegalStateException("exception handler stack is out of sync")

  def raiseValue(value: Value, position: Position): EvaluationStep =
    raiseException(SchemeException(value, position))

  def raiseException(exception: SchemeException): EvaluationStep =
    exceptionState.handlers match
      case frame :: rest =>
        exceptionState.handlers = rest
        DynamicWindRuntime.transferToWindFrames(
          frame.windFrames,
          () => frame.handler(exception)
        )
      case Nil =>
        SchemeFailure.raise(s"uncaught exception: ${exception.value.render}", exception.position)

  private def exceptionState: ExceptionState =
    activeExceptionState.value.getOrElse {
      throw new IllegalStateException("exception state is unavailable outside evaluation")
    }
