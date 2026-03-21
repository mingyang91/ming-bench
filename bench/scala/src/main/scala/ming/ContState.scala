package ming

import scala.util.control.NoStackTrace

/** Exception thrown when a continuation is invoked. */
case class ContinuationInvoked(id: Long, value: Value) extends Exception with NoStackTrace

/** Exception thrown by (raise value). */
case class SchemeException(value: Value) extends Exception with NoStackTrace

/** Mutable state for call/cc, using Array cells (no var). */
object ContState:

  private val nextIdCell: Array[Long] = Array(0L)

  val checkpoints: Array[Map[Long, (List[(Value, Pos)], Env, String)]] =
    Array(Map.empty)

  val currentCheckpoint: Array[Option[(List[(Value, Pos)], Env, String)]] =
    Array(None)

  /** Pending value tagged with the callback body reference for matching. */
  val pendingValue: Array[Option[(Option[List[Value]], Value)]] = Array(None)

  /** Callback body references keyed by continuation ID. */
  val callbackBodies: Array[Map[Long, List[Value]]] = Array(Map.empty)

  def freshId(): Long =
    val id = nextIdCell(0)
    nextIdCell(0) = id + 1
    id

  def reset(): Unit =
    nextIdCell(0) = 0L
    checkpoints(0) = Map.empty
    currentCheckpoint(0) = None
    pendingValue(0) = None
    callbackBodies(0) = Map.empty
