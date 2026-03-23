package ming

/** Body context for continuation capture. */
case class BodyContext(exprs: List[SchemeValue], env: Environment)

/** Thrown when a continuation is invoked. */
class ContinuationJump(
  val contId: Long,
  val value: SchemeValue,
  val bodyExprs: List[SchemeValue],
  val bodyEnv: Environment,
  val contextStack: List[BodyContext],
  val seqRemaining: List[SchemeValue],
  val seqEnv: Environment,
  val hasSameBodyFrame: Boolean
) extends Throwable:
  override def fillInStackTrace(): Throwable = this

/** Manages continuation state during evaluation. */
object ContinuationManager:
  var bodyContext: BodyContext                    = BodyContext(Nil, Environment())
  var contextStack: List[BodyContext]             = Nil
  var seqRemaining: List[SchemeValue]             = Nil
  var seqEnv: Environment                         = Environment()
  var hasSameBodyFrame: Boolean                   = false
  var pendingReturn: Option[SchemeValue]          = None
  var windStack: List[(SchemeValue, SchemeValue)] = Nil
  private var nextId: Long                        = 0

  def freshId(): Long =
    val id = nextId
    nextId += 1
    id

  def reset(): Unit =
    pendingReturn = None
    nextId = 0
    bodyContext = BodyContext(Nil, Environment())
    contextStack = Nil
    seqRemaining = Nil
    seqEnv = Environment()
    hasSameBodyFrame = false
    windStack = Nil
