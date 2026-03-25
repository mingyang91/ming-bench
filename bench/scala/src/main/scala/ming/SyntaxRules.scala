package ming

private[ming] object SyntaxRules:
  import SchemeInterpreter.Expr

  private[ming] val Ellipsis = "..."

  private[ming] val SyntaxKeywords = Set(
    "define",
    "define-syntax",
    "set!",
    "begin",
    "if",
    "let",
    "cond",
    "guard",
    "quote",
    "lambda",
    "and",
    "or",
    "syntax-rules"
  )

  sealed trait Capture

  object Capture:
    final case class Single(expr: Expr)              extends Capture
    final case class Repeated(values: List[Capture]) extends Capture

  final case class Rule(pattern: Expr.ListExpr, template: Expr)

  type Transformer = SyntaxTransformer

  def parse(name: String, expr: Expr, env: Env, macros: MacroScope): Transformer =
    SyntaxRuleParser.parse(name, expr, env, macros)

private[ming] object SyntaxFreshIds:
  private var nextId: Long = 0

  def next(): Long =
    nextId += 1
    nextId

private[ming] object SyntaxNames:

  def sanitize(name: String): String =
    val sanitized = name.map {
      case ch if ch.isLetterOrDigit => ch
      case _                        => '_'
    }

    if sanitized.nonEmpty then sanitized else "id"
