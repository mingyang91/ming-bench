package ming

import java.util.UUID

final private[ming] case class SyntaxRule(pattern: Expr, template: Expr)

sealed private[ming] trait MacroDefinition:
  def name: String
  def definitionEnv: Environment

final private[ming] case class SyntaxRulesMacro(
  name: String,
  literals: Set[String],
  rules: List[SyntaxRule],
  definitionEnv: Environment
) extends MacroDefinition

final private[ming] case class ProcedureMacro(
  name: String,
  transformer: ProcedureValue,
  definitionEnv: Environment
) extends MacroDefinition

final private[ming] case class MacroExpansion(
  expr: Expr,
  aliases: List[(String, BindingCell)]
)

sealed private[ming] trait PatternBinding
final private[ming] case class ScalarBinding(expr: Expr)                       extends PatternBinding
final private[ming] case class RepeatedBinding(values: Vector[PatternBinding]) extends PatternBinding

final private[ming] case class MacroInstantiationState(
  expansionId: String,
  nextFreshIndex: Long,
  aliases: Vector[(String, BindingCell)],
  capturedNames: Map[String, String]
):

  def addAlias(alias: String, cell: BindingCell): MacroInstantiationState =
    copy(aliases = aliases :+ (alias -> cell))

  def rememberCapture(name: String, capturedName: String): MacroInstantiationState =
    copy(capturedNames = capturedNames.updated(name, capturedName))

  def nextFresh(base: String): (String, MacroInstantiationState) =
    val freshIndex = nextFreshIndex + 1
    val freshName =
      s"__macro$$${expansionId}_${freshIndex}_${MacroInstantiationState.sanitize(base)}"
    (freshName, copy(nextFreshIndex = freshIndex))

private[ming] object MacroInstantiationState:

  def initial(): MacroInstantiationState =
    MacroInstantiationState(
      UUID.randomUUID().toString.replace("-", ""),
      0L,
      Vector.empty,
      Map.empty
    )

  private def sanitize(base: String): String =
    base.map:
      case ch if ch.isLetterOrDigit => ch
      case _                        => '_'
