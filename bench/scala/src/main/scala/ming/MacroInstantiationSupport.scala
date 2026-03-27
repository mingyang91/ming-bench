package ming

private[ming] object MacroInstantiationSupport:

  private type Instantiator =
    (Expr, MacroInstantiationContext, MacroInstantiationState) => (Expr, MacroInstantiationState)

  def instantiateExpressions(
    expressions: List[Expr],
    context: MacroInstantiationContext,
    state: MacroInstantiationState,
    instantiateTemplate: Instantiator
  ): (List[Expr], MacroInstantiationState) =
    val (instantiated, nextState) =
      expressions.foldLeft((Vector.empty[Expr], state)):
        case ((items, currentState), expression) =>
          val (instantiatedExpression, updatedState) =
            instantiateTemplate(expression, context, currentState)
          (items :+ instantiatedExpression, updatedState)
    (instantiated.toList, nextState)

  def dedupeAliases(
    aliases: List[(String, BindingCell)]
  ): List[(String, BindingCell)] =
    aliases.foldLeft(List.empty[(String, BindingCell)]):
      case (current, alias @ (name, _)) =>
        if current.exists(_._1 == name) then current else current :+ alias

  def instantiateLetBindingList(
    template: Expr,
    context: MacroInstantiationContext,
    state: MacroInstantiationState,
    instantiateTemplate: Instantiator
  ): (Expr, Map[String, String], MacroInstantiationState) =
    template match
      case ListExpr(bindingExpressions, position) =>
        val initial = (Vector.empty[Expr], Map.empty[String, String], state)
        val (instantiatedBindings, introducedBindings, nextState) =
          bindingExpressions.foldLeft(initial):
            case ((items, bindings, currentState), bindingExpression) =>
              val (instantiatedBinding, introducedBinding, updatedState) =
                instantiateLetBinding(
                  bindingExpression,
                  context,
                  currentState,
                  instantiateTemplate
                )
              (
                items :+ instantiatedBinding,
                bindings ++ introducedBinding,
                updatedState
              )
        (ListExpr(instantiatedBindings.toList, position), introducedBindings, nextState)
      case _ =>
        SchemeFailure.raise(
          "macro expansion produced an invalid let binding list",
          template.position
        )

  private def instantiateLetBinding(
    bindingExpression: Expr,
    context: MacroInstantiationContext,
    state: MacroInstantiationState,
    instantiateTemplate: Instantiator
  ): (Expr, Map[String, String], MacroInstantiationState) =
    bindingExpression match
      case ListExpr(List(name, valueExpression), bindingPosition) =>
        val (instantiatedValue, afterValue) =
          instantiateTemplate(valueExpression, context, state)
        val (instantiatedName, introducedBinding, afterName) =
          instantiateBinder(name, context, afterValue)
        (
          ListExpr(List(instantiatedName, instantiatedValue), bindingPosition),
          introducedBinding,
          afterName
        )
      case other =>
        SchemeFailure.raise(
          "macro expansion produced an invalid let binding",
          other.position
        )

  def instantiateBinderSpec(
    template: Expr,
    context: MacroInstantiationContext,
    state: MacroInstantiationState
  ): (Expr, Map[String, String], MacroInstantiationState) =
    template match
      case symbol: SymbolExpr =>
        instantiateBinder(symbol, context, state)
      case ListExpr(items, position) =>
        val (instantiatedItems, introducedBindings, nextState) =
          instantiateBinderList(items, context, state)
        (ListExpr(instantiatedItems, position), introducedBindings, nextState)
      case _ =>
        SchemeFailure.raise(
          "macro expansion expected an identifier or identifier list in binding position",
          template.position
        )

  def instantiateBinderList(
    templates: List[Expr],
    context: MacroInstantiationContext,
    state: MacroInstantiationState
  ): (List[Expr], Map[String, String], MacroInstantiationState) =
    val initial = (Vector.empty[Expr], Map.empty[String, String], state)
    val (instantiated, introducedBindings, nextState) =
      templates.foldLeft(initial):
        case ((items, bindings, currentState), SymbolExpr(".", position)) =>
          (items :+ SymbolExpr(".", position), bindings, currentState)
        case ((items, bindings, currentState), template) =>
          val (instantiatedTemplate, introducedBinding, updatedState) =
            instantiateBinder(template, context, currentState)
          (
            items :+ instantiatedTemplate,
            bindings ++ introducedBinding,
            updatedState
          )
    (instantiated.toList, introducedBindings, nextState)

  def instantiateBinder(
    template: Expr,
    context: MacroInstantiationContext,
    state: MacroInstantiationState
  ): (Expr, Map[String, String], MacroInstantiationState) =
    template match
      case SymbolExpr(name, position) if context.bindings.contains(name) =>
        MacroInstantiationHygiene.selectScalar(
          context.bindings(name),
          context.repetitionContext,
          position
        ) match
          case symbol: SymbolExpr =>
            (symbol, Map.empty, state)
          case _ =>
            SchemeFailure.raise(
              "macro expansion expected an identifier in binding position",
              position
            )
      case SymbolExpr(name, position) =>
        val (freshName, nextState) = state.nextFresh(name)
        (SymbolExpr(freshName, position), Map(name -> freshName), nextState)
      case _ =>
        SchemeFailure.raise(
          "macro expansion expected an identifier in binding position",
          template.position
        )

  def instantiateListItems(
    items: List[Expr],
    context: MacroInstantiationContext,
    state: MacroInstantiationState,
    instantiateTemplate: Instantiator
  ): (List[Expr], MacroInstantiationState) =
    items match
      case Nil =>
        (Nil, state)
      case template :: SymbolExpr("...", _) :: rest =>
        val repetitions =
          MacroInstantiationHygiene.repetitionCount(
            template,
            context.bindings,
            context.repetitionContext
          )
        val (expanded, afterExpansion) =
          instantiateRepeatedTemplate(
            template,
            repetitions,
            context,
            state,
            instantiateTemplate
          )
        val (remaining, nextState) =
          instantiateListItems(rest, context, afterExpansion, instantiateTemplate)
        (expanded ++ remaining, nextState)
      case item :: rest =>
        val (instantiatedItem, afterItem) =
          instantiateTemplate(item, context, state)
        val (remaining, nextState) =
          instantiateListItems(rest, context, afterItem, instantiateTemplate)
        (instantiatedItem :: remaining, nextState)

  def instantiateListTemplate(
    items: List[Expr],
    tail: Option[Expr],
    position: Position,
    contextDescription: String,
    context: MacroInstantiationContext,
    state: MacroInstantiationState,
    instantiateTemplate: Instantiator
  ): (Expr, MacroInstantiationState) =
    val (instantiatedItems, afterItems) =
      instantiateListItems(items, context, state, instantiateTemplate)

    tail match
      case Some(tailTemplate) =>
        val (instantiatedTail, afterTail) =
          instantiateTemplate(tailTemplate, context, afterItems)
        (
          ListExprSupport.build(
            instantiatedItems,
            Some(instantiatedTail),
            position,
            contextDescription
          ),
          afterTail
        )
      case None =>
        (ListExpr(instantiatedItems, position), afterItems)

  def instantiateProperListTemplate(
    items: List[Expr],
    tail: Option[Expr],
    position: Position,
    contextDescription: String,
    context: MacroInstantiationContext,
    state: MacroInstantiationState,
    instantiateTemplate: Instantiator
  ): (List[Expr], MacroInstantiationState) =
    val (instantiatedList, nextState) =
      instantiateListTemplate(
        items,
        tail,
        position,
        contextDescription,
        context,
        state,
        instantiateTemplate
      )
    (
      ListExprSupport.requireProperList(instantiatedList, contextDescription),
      nextState
    )

  private def instantiateRepeatedTemplate(
    template: Expr,
    repetitions: Range,
    context: MacroInstantiationContext,
    state: MacroInstantiationState,
    instantiateTemplate: Instantiator
  ): (List[Expr], MacroInstantiationState) =
    val (instantiated, nextState) =
      repetitions.foldLeft((Vector.empty[Expr], state)):
        case ((items, currentState), index) =>
          val (instantiatedTemplate, updatedState) =
            instantiateTemplate(template, context.inRepetition(index), currentState)
          (items :+ instantiatedTemplate, updatedState)
    (instantiated.toList, nextState)
