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

/** Thrown when `raise` is called in Scheme. */
class SchemeRaise(val value: SchemeValue) extends Throwable:
  override def fillInStackTrace(): Throwable = this

/** Thread-local holder for continuation manager state. */
private[ming] class ContinuationState:
  var bodyContext: BodyContext                    = BodyContext(Nil, Environment())
  var contextStack: List[BodyContext]             = Nil
  var seqRemaining: List[SchemeValue]             = Nil
  var seqEnv: Environment                         = Environment()
  var hasSameBodyFrame: Boolean                   = false
  var pendingReturn: Option[SchemeValue]          = None
  var windStack: List[(SchemeValue, SchemeValue)] = Nil
  var nextId: Long                                = 0

/** Manages continuation state during evaluation — thread-safe via ThreadLocal. */
object ContinuationManager:
  private val local: ThreadLocal[ContinuationState] =
    ThreadLocal.withInitial(() => ContinuationState())

  private def st: ContinuationState = local.get()

  def bodyContext: BodyContext                          = st.bodyContext
  def bodyContext_=(v: BodyContext): Unit               = st.bodyContext = v
  def contextStack: List[BodyContext]                   = st.contextStack
  def contextStack_=(v: List[BodyContext]): Unit        = st.contextStack = v
  def seqRemaining: List[SchemeValue]                   = st.seqRemaining
  def seqRemaining_=(v: List[SchemeValue]): Unit        = st.seqRemaining = v
  def seqEnv: Environment                               = st.seqEnv
  def seqEnv_=(v: Environment): Unit                    = st.seqEnv = v
  def hasSameBodyFrame: Boolean                         = st.hasSameBodyFrame
  def hasSameBodyFrame_=(v: Boolean): Unit              = st.hasSameBodyFrame = v
  def pendingReturn: Option[SchemeValue]                = st.pendingReturn
  def pendingReturn_=(v: Option[SchemeValue]): Unit     = st.pendingReturn = v
  def windStack: List[(SchemeValue, SchemeValue)]       = st.windStack
  def windStack_=(v: List[(SchemeValue, SchemeValue)]): Unit = st.windStack = v

  def freshId(): Long =
    val s  = st
    val id = s.nextId
    s.nextId += 1
    id

  def reset(): Unit =
    val s = st
    s.pendingReturn = None
    s.nextId = 0
    s.bodyContext = BodyContext(Nil, Environment())
    s.contextStack = Nil
    s.seqRemaining = Nil
    s.seqEnv = Environment()
    s.hasSameBodyFrame = false
    s.windStack = Nil
