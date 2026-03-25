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
    "syntax",
    "syntax-case",
    "lambda",
    "with-syntax",
    "and",
    "or",
    "syntax-rules"
  )

  trait Transformer:
    def expand(call: Expr.ListExpr, pos: SourcePos): Expr

  sealed trait Capture

  object Capture:
    final case class Single(expr: Expr)              extends Capture
    final case class Repeated(values: List[Capture]) extends Capture

  final case class Rule(pattern: Expr.ListExpr, template: Expr)

  def parse(name: String, expr: Expr, env: Env, macros: MacroScope): Transformer =
    expr match
      case Expr.ListExpr(Expr.Symbol("syntax-rules", _) :: _, _) =>
        SyntaxRuleParser.parse(name, expr, env, macros)
      case _ =>
        SchemeInterpreter.evalExpr(expr, env, macros) match
          case procedure: SchemeInterpreter.Procedure =>
            new ProcedureMacroTransformer(name, procedure, env, macros)
          case other =>
            throw EvalError.at(
              expr.pos,
              s"define-syntax expected transformer procedure, got ${SchemeInterpreter.render(other)}"
            )

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
