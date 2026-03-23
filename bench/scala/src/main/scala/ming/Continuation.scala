package ming

/** Body context for continuation capture. */
case class BodyContext(exprs: List[SchemeValue], env: Environment)

/** Thrown when a continuation is invoked. */
class ContinuationJump(
  val contId: Long,
  val value: SchemeValue,
  val bodyExprs: List[SchemeValue],
  val bodyEnv: Environment
) extends Throwable:
  override def fillInStackTrace(): Throwable = this

/** Manages continuation state during evaluation. */
object ContinuationManager:
  var bodyContext: BodyContext           = BodyContext(Nil, Environment())
  var pendingReturn: Option[SchemeValue] = None
  private var nextId: Long               = 0

  def freshId(): Long =
    val id = nextId
    nextId += 1
    id

  def reset(): Unit =
    pendingReturn = None
    nextId = 0
    bodyContext = BodyContext(Nil, Environment())
