package ming

private[ming] object MacroInstantiator:

  private lazy val defineInstantiator =
    new MacroDefineInstantiator(instantiateTemplate)

  def instantiate(
    template: Expr,
    bindings: Map[String, PatternBinding],
    macroDefinition: SyntaxRulesMacro
  ): MacroExpansion =
    val context = MacroInstantiationContext(bindings, macroDefinition)
    val (expanded, finalState) =
      instantiateTemplate(template, context, MacroInstantiationState.initial())
    MacroExpansion(expanded, finalState.aliases.toList)

  def instantiateSyntax(
    template: Expr,
    bindings: Map[String, PatternBinding],
    definitionEnv: Environment,
    inheritedAliases: List[(String, BindingCell)] = Nil
  ): MacroExpansion =
    val context = MacroInstantiationContext(
      bindings,
      SyntaxRulesMacro("<syntax>", Set.empty, Nil, definitionEnv)
    )
    val (expanded, finalState) =
      instantiateTemplate(template, context, MacroInstantiationState.initial())
    MacroExpansion(
      expanded,
      MacroInstantiationSupport.dedupeAliases(inheritedAliases ++ finalState.aliases.toList)
    )

  private def instantiateTemplate(
    template: Expr,
    context: MacroInstantiationContext,
    state: MacroInstantiationState
  ): (Expr, MacroInstantiationState) =
    template match
      case quoted @ ListExpr(List(SymbolExpr("quote", _), _), _) =>
        (quoted, state)
      case ListExpr(items, position) =>
        instantiateListExpr(items, position, context, state)
      case SymbolExpr(name, position) =>
        MacroInstantiationHygiene.instantiateSymbol(name, position, context, state)
      case _ =>
        (template, state)

  private def instantiateListExpr(
    items: List[Expr],
    position: Position,
    context: MacroInstantiationContext,
    state: MacroInstantiationState
  ): (Expr, MacroInstantiationState) =
    val decoded = ListExprSupport.decode(items, position, "macro template")

    decoded.items match
      case SymbolExpr("lambda", keywordPosition) :: parameterSpec :: body if body.nonEmpty || decoded.tail.nonEmpty =>
        instantiateLambda(
          keywordPosition,
          parameterSpec,
          body,
          decoded.tail,
          position,
          context,
          state
        )
      case SymbolExpr("case-lambda", keywordPosition) :: clauses if decoded.tail.isEmpty =>
        MacroCaseLambdaInstantiator.instantiate(
          keywordPosition,
          clauses,
          position,
          context,
          state,
          instantiateTemplate
        )
      case SymbolExpr("let", keywordPosition) ::
          (nameTemplate @ SymbolExpr(_, _)) ::
          bindingList ::
          body if body.nonEmpty || decoded.tail.nonEmpty =>
        instantiateNamedLet(
          keywordPosition,
          nameTemplate,
          bindingList,
          body,
          decoded.tail,
          position,
          context,
          state
        )
      case SymbolExpr("let", keywordPosition) :: bindingList :: body if body.nonEmpty || decoded.tail.nonEmpty =>
        instantiateLet(
          keywordPosition,
          bindingList,
          body,
          decoded.tail,
          position,
          context,
          state
        )
      case SymbolExpr("define", keywordPosition) ::
          ListExpr(nameTemplate :: parameters, parameterPosition) ::
          body if body.nonEmpty || decoded.tail.nonEmpty =>
        val (instantiatedDefine, _, nextState) =
          defineInstantiator.instantiateProcedure(
            keywordPosition,
            nameTemplate,
            parameters,
            parameterPosition,
            body,
            decoded.tail,
            position,
            context,
            state
          )
        (instantiatedDefine, nextState)
      case SymbolExpr("define", keywordPosition) :: nameTemplate :: value :: Nil if decoded.tail.isEmpty =>
        val (instantiatedDefine, _, nextState) =
          defineInstantiator.instantiateValue(
            keywordPosition,
            nameTemplate,
            value,
            position,
            context,
            state
          )
        (instantiatedDefine, nextState)
      case _ =>
        MacroInstantiationSupport.instantiateListTemplate(
          decoded.items,
          decoded.tail,
          position,
          "macro expansion",
          context,
          state,
          instantiateTemplate
        )

  private def instantiateLambda(
    keywordPosition: Position,
    parameterSpec: Expr,
    body: List[Expr],
    bodyTail: Option[Expr],
    position: Position,
    context: MacroInstantiationContext,
    state: MacroInstantiationState
  ): (Expr, MacroInstantiationState) =
    val (instantiatedParameterSpec, introducedBindings, afterParameters) =
      MacroInstantiationSupport.instantiateBinderSpec(parameterSpec, context, state)
    val (instantiatedBody, afterBody) =
      MacroBodyInstantiator.instantiate(
        body,
        context.inScope(introducedBindings),
        afterParameters,
        instantiateTemplate,
        defineInstantiator.instantiateValue,
        defineInstantiator.instantiateProcedure,
        bodyTail
      )
    if instantiatedBody.isEmpty then SchemeFailure.raise("macro expansion produced an invalid lambda form", position)
    (
      ListExpr(
        SymbolExpr("lambda", keywordPosition) :: instantiatedParameterSpec :: instantiatedBody,
        position
      ),
      afterBody
    )

  private def instantiateNamedLet(
    keywordPosition: Position,
    nameTemplate: Expr,
    bindingList: Expr,
    body: List[Expr],
    bodyTail: Option[Expr],
    position: Position,
    context: MacroInstantiationContext,
    state: MacroInstantiationState
  ): (Expr, MacroInstantiationState) =
    val (instantiatedName, nameBinding, afterName) =
      MacroInstantiationSupport.instantiateBinder(nameTemplate, context, state)
    val (instantiatedBindingList, afterBindings) =
      instantiateTemplate(bindingList, context, afterName)
    val (instantiatedBody, afterBody) =
      MacroBodyInstantiator.instantiate(
        body,
        context.inScope(nameBinding),
        afterBindings,
        instantiateTemplate,
        defineInstantiator.instantiateValue,
        defineInstantiator.instantiateProcedure,
        bodyTail
      )
    if instantiatedBody.isEmpty then SchemeFailure.raise("macro expansion produced an invalid let form", position)
    (
      ListExpr(
        SymbolExpr("let", keywordPosition) ::
          instantiatedName ::
          instantiatedBindingList ::
          instantiatedBody,
        position
      ),
      afterBody
    )

  private def instantiateLet(
    keywordPosition: Position,
    bindingList: Expr,
    body: List[Expr],
    bodyTail: Option[Expr],
    position: Position,
    context: MacroInstantiationContext,
    state: MacroInstantiationState
  ): (Expr, MacroInstantiationState) =
    val (instantiatedBindingList, introducedBindings, afterBindings) =
      MacroInstantiationSupport.instantiateLetBindingList(
        bindingList,
        context,
        state,
        instantiateTemplate
      )
    val (instantiatedBody, afterBody) =
      MacroBodyInstantiator.instantiate(
        body,
        context.inScope(introducedBindings),
        afterBindings,
        instantiateTemplate,
        defineInstantiator.instantiateValue,
        defineInstantiator.instantiateProcedure,
        bodyTail
      )
    if instantiatedBody.isEmpty then SchemeFailure.raise("macro expansion produced an invalid let form", position)
    (
      ListExpr(
        SymbolExpr("let", keywordPosition) :: instantiatedBindingList :: instantiatedBody,
        position
      ),
      afterBody
    )
