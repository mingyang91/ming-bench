package ming

final private[ming] case class MacroInstantiationContext(
  bindings: Map[String, PatternBinding],
  macroDefinition: SyntaxRulesMacro,
  scopeRenames: Map[String, String] = Map.empty,
  repetitionContext: List[Int] = Nil
):

  def inScope(introducedBindings: Map[String, String]): MacroInstantiationContext =
    copy(scopeRenames = scopeRenames ++ introducedBindings)

  def inRepetition(index: Int): MacroInstantiationContext =
    copy(repetitionContext = repetitionContext :+ index)
