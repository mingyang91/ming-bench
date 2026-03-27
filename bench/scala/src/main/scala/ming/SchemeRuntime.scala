package ming

final private[ming] class DynamicWindContext(
  val inThunk: Value,
  val outThunk: Value,
  val pos: SourcePos
)

final private[ming] class ExceptionHandlerContext(
  val handler: Value,
  val outerFrames: List[ContinuationFrame],
  val outerWinds: List[DynamicWindContext],
  val outerHandlers: List[ExceptionHandlerContext],
  val pos: SourcePos
)

sealed private[ming] trait ContinuationFrame

private[ming] object ContinuationFrame:
  case object ProcedureBoundary                                               extends ContinuationFrame
  final case class Sequence(remaining: List[Expr], env: Env)                  extends ContinuationFrame
  final case class IfBranch(thenExpr: Expr, elseExpr: Option[Expr], env: Env) extends ContinuationFrame
  final case class DefineValue(name: String, env: Env)                        extends ContinuationFrame
  final case class SetValue(name: String, symbolPos: SourcePos, env: Env)     extends ContinuationFrame
  final case class CallHead(args: List[Expr], env: Env, pos: SourcePos)       extends ContinuationFrame
  final case class CallWithValuesConsumer(consumer: Value, pos: SourcePos)    extends ContinuationFrame
  case object CallCcResult                                                    extends ContinuationFrame

  final case class CallArg(
    procedure: Value,
    evaluatedRev: List[Value],
    remaining: List[Expr],
    env: Env,
    pos: SourcePos
  ) extends ContinuationFrame
  final case class And(remaining: List[Expr], env: Env) extends ContinuationFrame
  final case class Or(remaining: List[Expr], env: Env)  extends ContinuationFrame

  final case class CondClause(
    body: List[Expr],
    remainingClauses: List[Expr],
    env: Env,
    pos: SourcePos
  ) extends ContinuationFrame
  final case class CondArrowRecipient(argument: Value, pos: SourcePos)    extends ContinuationFrame
  final case class CaseKey(clauses: List[Expr], env: Env, pos: SourcePos) extends ContinuationFrame

  final case class LetrecSequentialValue(
    currentCell: BindingCell,
    remaining: List[(LetBinding, BindingCell)],
    recursiveEnv: Env,
    body: List[Expr]
  ) extends ContinuationFrame

  final case class LetrecParallelValue(
    currentCell: BindingCell,
    remaining: List[(LetBinding, BindingCell)],
    evaluatedRev: List[(BindingCell, Value)],
    recursiveEnv: Env,
    body: List[Expr]
  ) extends ContinuationFrame

  final case class MapResult(
    procedure: Value,
    remainingRows: List[List[Value]],
    accRev: List[Value],
    pos: SourcePos
  ) extends ContinuationFrame

  final case class ForEachResult(
    procedure: Value,
    remainingRows: List[List[Value]],
    pos: SourcePos
  ) extends ContinuationFrame

  final case class DynamicWindEntered(bodyThunk: Value, wind: DynamicWindContext) extends ContinuationFrame
  final case class DynamicWindBodyResult(wind: DynamicWindContext)                extends ContinuationFrame
  final case class DynamicWindOutResult(bodyResult: Value)                        extends ContinuationFrame
  final case class WithExceptionHandlerResult(handler: ExceptionHandlerContext)   extends ContinuationFrame
  final case class ExceptionHandlerReturned(raisePos: SourcePos)                  extends ContinuationFrame

  final case class ExceptionWindExit(
    remaining: List[DynamicWindContext],
    entering: List[DynamicWindContext],
    handler: ExceptionHandlerContext,
    exception: Value,
    raisePos: SourcePos
  ) extends ContinuationFrame

  final case class ExceptionWindEnter(
    current: DynamicWindContext,
    remaining: List[DynamicWindContext],
    handler: ExceptionHandlerContext,
    exception: Value,
    raisePos: SourcePos
  ) extends ContinuationFrame

  final case class ContinuationWindExit(
    remaining: List[DynamicWindContext],
    entering: List[DynamicWindContext],
    snapshot: ContinuationSnapshot,
    value: Value
  ) extends ContinuationFrame

  final case class ContinuationWindEnter(
    current: DynamicWindContext,
    remaining: List[DynamicWindContext],
    snapshot: ContinuationSnapshot,
    value: Value
  ) extends ContinuationFrame

final private[ming] class ContinuationSnapshot(
  val frames: List[ContinuationFrame],
  val winds: List[DynamicWindContext],
  val handlers: List[ExceptionHandlerContext]
)

final private[ming] class EvalContext:

  private val output = new StringBuilder

  def emit(text: String): Unit =
    output.append(text)

  def capturedOutput: String =
    output.toString
