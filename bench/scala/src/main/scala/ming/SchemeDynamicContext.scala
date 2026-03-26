package ming

import java.util.concurrent.atomic.AtomicLong

import SchemeEvaluatorState.*
import SchemeModel.*

private[ming] object SchemeDynamicContext:

  final case class WindFrame(id: Long, inThunk: Value, outThunk: Value)

  final case class ExceptionHandlerFrame(
    handler: Value,
    windContext: Vector[WindFrame],
    previous: Option[ExceptionHandlerFrame]
  )

  final case class DynamicState(
    windContext: Vector[WindFrame],
    exceptionHandler: Option[ExceptionHandlerFrame]
  )

  final case class CapturedContinuation(
    continuation: Continuation,
    dynamicState: DynamicState
  )

  private val nextWindId = new AtomicLong()

  private val currentState =
    ThreadLocal.withInitial[DynamicState](() => DynamicState(Vector.empty, None))

  def snapshot: DynamicState =
    currentState.get()

  def current: Vector[WindFrame] =
    snapshot.windContext

  def replace(dynamicContext: Vector[WindFrame]): Unit =
    replaceState(snapshot.copy(windContext = dynamicContext))

  def replaceState(state: DynamicState): Unit =
    currentState.set(state)

  def currentExceptionHandler: Option[ExceptionHandlerFrame] =
    snapshot.exceptionHandler

  def replaceExceptionHandler(handler: Option[ExceptionHandlerFrame]): Unit =
    replaceState(snapshot.copy(exceptionHandler = handler))

  def reset(): Unit =
    replaceState(DynamicState(Vector.empty, None))

  def newFrame(inThunk: Value, outThunk: Value): WindFrame =
    WindFrame(nextWindId.incrementAndGet(), inThunk, outThunk)

  def commonPrefixLength(left: Vector[WindFrame], right: Vector[WindFrame]): Int =
    left.iterator
      .zip(right.iterator)
      .takeWhile { case (leftFrame, rightFrame) =>
        leftFrame.id == rightFrame.id
      }
      .length
