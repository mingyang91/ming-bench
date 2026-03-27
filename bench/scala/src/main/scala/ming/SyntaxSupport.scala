package ming

private[ming] object SyntaxSupport:

  def parseLiteralIdentifiers(expression: Expr, position: Position): Set[String] =
    expression match
      case ListExpr(literalExpressions, _) =>
        literalExpressions
          .map:
            case SymbolExpr(name, _) => name
            case other =>
              SchemeFailure.raise(
                s"syntax-case expected literal identifiers, got ${other.getClass.getSimpleName}",
                other.position
              )
          .toSet
      case _ =>
        SchemeFailure.raise("syntax-case expected a literal identifier list", position)

  def instantiate(template: Expr, env: Environment, position: Position): SyntaxObjectValue =
    val definitionEnv                = MacroRuntime.definitionEnv(position)
    val (bindings, inheritedAliases) = collectTemplateBindings(template, env)
    SyntaxObjectValue(
      MacroInstantiator.instantiateSyntax(
        template,
        bindings,
        definitionEnv,
        inheritedAliases
      )
    )

  def datumToExpr(value: Value, position: Position): Expr =
    value match
      case IntValue(number) =>
        IntExpr(number, position)
      case RationalValue(numerator, denominator) =>
        RationalExpr(numerator, denominator, position)
      case InexactValue(number) =>
        InexactExpr(number, position)
      case BoolValue(boolean) =>
        BoolExpr(boolean, position)
      case stringValue: StringLikeValue =>
        StringExpr(stringValue.text, position)
      case CharValue(ch) =>
        CharExpr(ch, position)
      case SymbolValue(name) =>
        SymbolExpr(name, position)
      case EmptyListValue =>
        ListExpr(Nil, position)
      case pair: PairValue =>
        val elements =
          RuntimeSupport.expectProperList(pair, "datum->syntax", position)
        ListExpr(elements.map(datumToExpr(_, position)), position)
      case other =>
        SchemeFailure.raise(
          s"datum->syntax expected a datum, got ${RuntimeSupport.typeName(other)}",
          position
        )

  def bindingsToValues(
    bindings: Map[String, PatternBinding],
    inheritedAliases: List[(String, BindingCell)] = Nil
  ): List[(String, Value)] =
    bindings.toList.map((name, binding) => name -> bindingToValue(binding, inheritedAliases))

  def mergeBindings(
    left: Map[String, PatternBinding],
    right: Map[String, PatternBinding],
    position: Position,
    context: String
  ): Map[String, PatternBinding] =
    right.foldLeft(left):
      case (current, (name, binding)) =>
        current.get(name) match
          case Some(existing) if !sameBinding(existing, binding) =>
            SchemeFailure.raise(s"$context produced conflicting bindings for $name", position)
          case Some(_) =>
            current
          case None =>
            current.updated(name, binding)

  private def collectTemplateBindings(
    template: Expr,
    env: Environment
  ): (Map[String, PatternBinding], List[(String, BindingCell)]) =
    template match
      case ListExpr(List(SymbolExpr("quote", _), _), _) =>
        (Map.empty, Nil)
      case ListExpr(List(SymbolExpr("syntax", _), _), _) =>
        (Map.empty, Nil)
      case SymbolExpr("...", _) =>
        (Map.empty, Nil)
      case SymbolExpr(name, _) =>
        env.lookupCellOption(name) match
          case Some(cell) =>
            valueToPatternBinding(cell.value) match
              case Some((binding, aliases)) =>
                (Map(name -> binding), aliases)
              case None =>
                (Map.empty, Nil)
          case None =>
            (Map.empty, Nil)
      case ListExpr(items, _) =>
        items.foldLeft((Map.empty[String, PatternBinding], List.empty[(String, BindingCell)])):
          case ((collectedBindings, collectedAliases), item) =>
            val (itemBindings, itemAliases) = collectTemplateBindings(item, env)
            (
              mergeBindings(
                collectedBindings,
                itemBindings,
                item.position,
                "syntax template"
              ),
              dedupeAliases(collectedAliases ++ itemAliases)
            )
      case _ =>
        (Map.empty, Nil)

  private def valueToPatternBinding(
    value: Value
  ): Option[(PatternBinding, List[(String, BindingCell)])] =
    value match
      case SyntaxObjectValue(expansion) =>
        Some(ScalarBinding(expansion.expr) -> expansion.aliases)
      case SyntaxListValue(values) =>
        val converted = values.iterator.map(valueToPatternBinding).toList
        if converted.forall(_.nonEmpty) then
          val bindings = converted.flatten.map(_._1).toVector
          val aliases  = dedupeAliases(converted.flatten.flatMap(_._2))
          Some(RepeatedBinding(bindings) -> aliases)
        else None
      case _ =>
        None

  private def bindingToValue(
    binding: PatternBinding,
    inheritedAliases: List[(String, BindingCell)]
  ): SyntaxRuntimeValue =
    binding match
      case ScalarBinding(expr) =>
        SyntaxObjectValue(MacroExpansion(expr, inheritedAliases))
      case RepeatedBinding(values) =>
        SyntaxListValue(values.map(bindingToValue(_, inheritedAliases)))

  private def sameBinding(left: PatternBinding, right: PatternBinding): Boolean =
    (left, right) match
      case (ScalarBinding(leftExpr), ScalarBinding(rightExpr)) =>
        sameExpr(leftExpr, rightExpr)
      case (RepeatedBinding(leftValues), RepeatedBinding(rightValues)) =>
        leftValues.length == rightValues.length &&
        leftValues.zip(rightValues).forall((leftBinding, rightBinding) => sameBinding(leftBinding, rightBinding))
      case _ =>
        false

  private def sameExpr(left: Expr, right: Expr): Boolean =
    (left, right) match
      case (IntExpr(leftValue, _), IntExpr(rightValue, _)) =>
        leftValue == rightValue
      case (
            RationalExpr(leftNumerator, leftDenominator, _),
            RationalExpr(rightNumerator, rightDenominator, _)
          ) =>
        leftNumerator == rightNumerator && leftDenominator == rightDenominator
      case (InexactExpr(leftValue, _), InexactExpr(rightValue, _)) =>
        java.lang.Double.compare(leftValue, rightValue) == 0
      case (BoolExpr(leftValue, _), BoolExpr(rightValue, _)) =>
        leftValue == rightValue
      case (StringExpr(leftValue, _), StringExpr(rightValue, _)) =>
        leftValue == rightValue
      case (CharExpr(leftValue, _), CharExpr(rightValue, _)) =>
        leftValue == rightValue
      case (SymbolExpr(leftName, _), SymbolExpr(rightName, _)) =>
        leftName == rightName
      case (ListExpr(leftItems, _), ListExpr(rightItems, _)) =>
        leftItems.length == rightItems.length &&
        leftItems.zip(rightItems).forall((leftItem, rightItem) => sameExpr(leftItem, rightItem))
      case _ =>
        false

  private def dedupeAliases(
    aliases: List[(String, BindingCell)]
  ): List[(String, BindingCell)] =
    aliases.foldLeft(List.empty[(String, BindingCell)]):
      case (current, alias @ (name, _)) =>
        if current.exists(_._1 == name) then current else current :+ alias
