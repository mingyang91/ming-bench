package ming

import MacroSyntax.*

final private[ming] case class SyntaxRulesTransformer(
  name: String,
  literalNames: Set[String],
  rules: List[SyntaxRule],
  definitionEnv: Env
) extends SyntaxTransformer:

  override def expand(invocation: Expr.ListExpr): Expr =
    rules.iterator
      .map(rule =>
        SyntaxPatternMatcher.matchPattern(rule.pattern, invocation, literalNames + name).map(expandRule(rule, _))
      )
      .collectFirst { case Some(expanded) => expanded }
      .getOrElse(throw EvalError.at(invocation.pos, s"no matching syntax-rules clause for $name"))

  private def expandRule(rule: SyntaxRule, bindings: PatternBindings): Expr =
    SyntaxRuleTemplateExpander.expand(rule.template, bindings, definitionEnv)
