package ming

import scala.annotation.tailrec
import scala.util.DynamicVariable

private[ming] object DynamicWindRuntime:

  final private class WindState(var frames: List[WindFrame])

  private val activeWindState = DynamicVariable[Option[WindState]](None)

  def withWindState[A](thunk: => A): A =
    activeWindState.value match
      case Some(_) =>
        thunk
      case None =>
        activeWindState.withValue(Some(WindState(Nil)))(thunk)

  def captureWindFrames: List[WindFrame] =
    windState.frames

  def executeDynamicWind(
    inThunk: Value,
    bodyThunk: Value,
    outThunk: Value,
    position: Position,
    continuation: Continuation
  ): EvaluationStep =
    val frame = WindFrame(inThunk, outThunk, position)
    InterpreterEvaluator.deferApplication(
      inThunk,
      Nil,
      position,
      _ =>
        pushWindFrame(frame)
        InterpreterEvaluator.deferApplication(
          bodyThunk,
          Nil,
          position,
          bodyValue =>
            popWindFrame(frame)
            InterpreterEvaluator.deferApplication(
              outThunk,
              Nil,
              position,
              _ => InterpreterEvaluator.done(bodyValue, continuation)
            )
        )
    )

  def transferToContinuation(
    value: Value,
    continuation: Continuation,
    targetWindFrames: List[WindFrame]
  ): EvaluationStep =
    val currentWindFrames  = captureWindFrames
    val sharedPrefixLength = commonWindPrefixLength(currentWindFrames, targetWindFrames)
    val exitingFrames      = currentWindFrames.drop(sharedPrefixLength).reverse
    val enteringFrames     = targetWindFrames.drop(sharedPrefixLength)

    runExitThunks(
      exitingFrames,
      () =>
        runEnterThunks(
          enteringFrames,
          () => InterpreterEvaluator.done(value, continuation)
        )
    )

  private def windState: WindState =
    activeWindState.value.getOrElse {
      throw new IllegalStateException("dynamic-wind state is unavailable outside evaluation")
    }

  private def pushWindFrame(frame: WindFrame): Unit =
    windState.frames = windState.frames :+ frame

  private def popWindFrame(expected: WindFrame): Unit =
    val frames = windState.frames
    if frames.nonEmpty && (frames.last eq expected) then windState.frames = frames.init
    else throw new IllegalStateException("dynamic-wind stack is out of sync")

  private def runExitThunks(
    exitingFrames: List[WindFrame],
    next: () => EvaluationStep
  ): EvaluationStep =
    exitingFrames match
      case Nil =>
        next()
      case frame :: rest =>
        popWindFrame(frame)
        InterpreterEvaluator.deferApplication(
          frame.outThunk,
          Nil,
          frame.position,
          _ => runExitThunks(rest, next)
        )

  private def runEnterThunks(
    enteringFrames: List[WindFrame],
    next: () => EvaluationStep
  ): EvaluationStep =
    enteringFrames match
      case Nil =>
        next()
      case frame :: rest =>
        InterpreterEvaluator.deferApplication(
          frame.inThunk,
          Nil,
          frame.position,
          _ =>
            pushWindFrame(frame)
            runEnterThunks(rest, next)
        )

  @tailrec
  private def commonWindPrefixLength(
    currentWindFrames: List[WindFrame],
    targetWindFrames: List[WindFrame],
    prefixLength: Int = 0
  ): Int =
    (currentWindFrames, targetWindFrames) match
      case (currentFrame :: currentRest, targetFrame :: targetRest) if currentFrame eq targetFrame =>
        commonWindPrefixLength(currentRest, targetRest, prefixLength + 1)
      case _ =>
        prefixLength
