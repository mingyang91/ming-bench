package ming

import java.util.concurrent.atomic.AtomicLong

import SchemeEvaluatorState.*
import SchemeModel.*

private[ming] object SchemeDynamicContext:

  final case class WindFrame(id: Long, inThunk: Value, outThunk: Value)

  final case class CapturedContinuation(
    continuation: Continuation,
    dynamicContext: Vector[WindFrame]
  )

  private val nextWindId = new AtomicLong()

  private val currentContext =
    ThreadLocal.withInitial[Vector[WindFrame]](() => Vector.empty[WindFrame])

  def current: Vector[WindFrame] =
    currentContext.get()

  def replace(dynamicContext: Vector[WindFrame]): Unit =
    currentContext.set(dynamicContext)

  def reset(): Unit =
    replace(Vector.empty)

  def newFrame(inThunk: Value, outThunk: Value): WindFrame =
    WindFrame(nextWindId.incrementAndGet(), inThunk, outThunk)

  def commonPrefixLength(left: Vector[WindFrame], right: Vector[WindFrame]): Int =
    left.iterator
      .zip(right.iterator)
      .takeWhile { case (leftFrame, rightFrame) =>
        leftFrame.id == rightFrame.id
      }
      .length
