package ming

class EvalError(message: String) extends Exception(message)

object EvalError:

  def withPos(msg: String, pos: Option[(Int, Int)]): EvalError =
    val prefix = pos.map { case (l, c) => s"$l:$c: " }.getOrElse("")
    new EvalError(s"$prefix$msg")

/** Thrown by call/cc to communicate with evalAll. */
class CallCCSetup(
  val proc: Value,
  val output: String,
  val pos: Option[(Int, Int)]
) extends RuntimeException(null, null, true, false)

/** Thrown when a continuation value is invoked. */
class ContinuationInvoked(
  val tag: AnyRef,
  val value: Value,
  val output: String,
  val remaining: List[Value],
  val envThunk: () => Env,
  val capturedOut: String,
  val bodyLevel: Boolean = false,
  val windEntries: List[(Value, Value)] = Nil
) extends RuntimeException(null, null, true, false)
