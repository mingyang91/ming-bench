package ming

sealed private[ming] trait ContinuationFrame

private[ming] object ContinuationFrame:
  final case class Sequence(remaining: List[Expr], env: Env)                  extends ContinuationFrame
  final case class IfBranch(thenExpr: Expr, elseExpr: Option[Expr], env: Env) extends ContinuationFrame
  final case class DefineValue(name: String, env: Env)                        extends ContinuationFrame
  final case class SetValue(name: String, symbolPos: SourcePos, env: Env)     extends ContinuationFrame
  final case class CallHead(args: List[Expr], env: Env, pos: SourcePos)       extends ContinuationFrame

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

final private[ming] class ContinuationSnapshot(
  val frames: List[ContinuationFrame]
)

final private[ming] class EvalContext:

  private val output = new StringBuilder

  def emit(text: String): Unit =
    output.append(text)

  def capturedOutput: String =
    output.toString
