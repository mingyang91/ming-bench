package ming

final private[ming] class SyntaxTransformer(
  val name: String,
  private val literals: Set[String],
  private val rules: List[SyntaxRules.Rule],
  private val definitionEnv: Env,
  private val definitionMacros: MacroScope,
  private val id: Long
) extends SyntaxRules.Transformer:

  import SchemeInterpreter.Expr
  import SyntaxRules.Rule

  private val expander = new SyntaxTemplateExpander(definitionEnv, definitionMacros, id)

  def expand(call: Expr.ListExpr, pos: SourcePos): Expr =
    rules.iterator
      .flatMap(rule => expandRule(rule, call))
      .take(1)
      .toList
      .headOption
      .getOrElse(throw EvalError.at(pos, s"no syntax-rules clause matched for $name"))

  private def expandRule(rule: Rule, call: Expr.ListExpr): Option[Expr] =
    SyntaxPatternMatcher.matchRule(rule.pattern, call, literals).map { bindings =>
      expander.instantiate(rule.template, bindings)
    }
