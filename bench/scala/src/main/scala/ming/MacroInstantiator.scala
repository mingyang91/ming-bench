package ming

private[ming] object MacroInstantiator:

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
    MacroExpansion(expanded, dedupeAliases(inheritedAliases ++ finalState.aliases.toList))

  private def instantiateTemplate(
    template: Expr,
    context: MacroInstantiationContext,
    state: MacroInstantiationState
  ): (Expr, MacroInstantiationState) =
    template match
      case quoted @ ListExpr(List(SymbolExpr("quote", _), _), _) =>
        (quoted, state)
      case ListExpr(SymbolExpr("lambda", keywordPosition) :: parameterSpec :: body, position) if body.nonEmpty =>
        instantiateLambda(keywordPosition, parameterSpec, body, position, context, state)
      case ListExpr(SymbolExpr("case-lambda", keywordPosition) :: clauses, position) =>
        MacroCaseLambdaInstantiator.instantiate(
          keywordPosition,
          clauses,
          position,
          context,
          state,
          instantiateTemplate
        )
      case ListExpr(
            SymbolExpr("let", keywordPosition) ::
            (nameTemplate @ SymbolExpr(_, _)) ::
            bindingList ::
            body,
            position
          ) if body.nonEmpty =>
        instantiateNamedLet(
          keywordPosition,
          nameTemplate,
          bindingList,
          body,
          position,
          context,
          state
        )
      case ListExpr(SymbolExpr("let", keywordPosition) :: bindingList :: body, position) if body.nonEmpty =>
        instantiateLet(keywordPosition, bindingList, body, position, context, state)
      case ListExpr(
            SymbolExpr("define", keywordPosition) ::
            ListExpr(nameTemplate :: parameters, parameterPosition) ::
            body,
            position
          ) if body.nonEmpty =>
        instantiateProcedureDefine(
          keywordPosition,
          nameTemplate,
          parameters,
          parameterPosition,
          body,
          position,
          context,
          state
        )
      case ListExpr(SymbolExpr("define", keywordPosition) :: nameTemplate :: value :: Nil, position) =>
        instantiateValueDefine(
          keywordPosition,
          nameTemplate,
          value,
          position,
          context,
          state
        )
      case ListExpr(items, position) =>
        val (instantiatedItems, nextState) =
          MacroInstantiationSupport.instantiateListItems(
            items,
            context,
            state,
            instantiateTemplate
          )
        (ListExpr(instantiatedItems, position), nextState)
      case SymbolExpr(name, position) =>
        MacroInstantiationHygiene.instantiateSymbol(name, position, context, state)
      case _ =>
        (template, state)

  private def instantiateLambda(
    keywordPosition: Position,
    parameterSpec: Expr,
    body: List[Expr],
    position: Position,
    context: MacroInstantiationContext,
    state: MacroInstantiationState
  ): (Expr, MacroInstantiationState) =
    val (instantiatedParameterSpec, introducedBindings, afterParameters) =
      MacroInstantiationSupport.instantiateBinderSpec(parameterSpec, context, state)
    val (instantiatedBody, afterBody) =
      MacroInstantiationSupport.instantiateExpressions(
        body,
        context.inScope(introducedBindings),
        afterParameters,
        instantiateTemplate
      )
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
    position: Position,
    context: MacroInstantiationContext,
    state: MacroInstantiationState
  ): (Expr, MacroInstantiationState) =
    val (instantiatedName, nameBinding, afterName) =
      MacroInstantiationSupport.instantiateBinder(nameTemplate, context, state)
    val (instantiatedBindingList, afterBindings) =
      instantiateTemplate(bindingList, context, afterName)
    val (instantiatedBody, afterBody) =
      MacroInstantiationSupport.instantiateExpressions(
        body,
        context.inScope(nameBinding),
        afterBindings,
        instantiateTemplate
      )
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
      MacroInstantiationSupport.instantiateExpressions(
        body,
        context.inScope(introducedBindings),
        afterBindings,
        instantiateTemplate
      )
    (
      ListExpr(
        SymbolExpr("let", keywordPosition) :: instantiatedBindingList :: instantiatedBody,
        position
      ),
      afterBody
    )

  private def instantiateValueDefine(
    keywordPosition: Position,
    nameTemplate: Expr,
    value: Expr,
    position: Position,
    context: MacroInstantiationContext,
    state: MacroInstantiationState
  ): (Expr, MacroInstantiationState) =
    val (instantiatedName, _, afterName) =
      MacroInstantiationSupport.instantiateBinder(nameTemplate, context, state)
    val (instantiatedValue, afterValue) =
      instantiateTemplate(value, context, afterName)
    (
      ListExpr(
        List(
          SymbolExpr("define", keywordPosition),
          instantiatedName,
          instantiatedValue
        ),
        position
      ),
      afterValue
    )

  private def instantiateProcedureDefine(
    keywordPosition: Position,
    nameTemplate: Expr,
    parameters: List[Expr],
    parameterPosition: Position,
    body: List[Expr],
    position: Position,
    context: MacroInstantiationContext,
    state: MacroInstantiationState
  ): (Expr, MacroInstantiationState) =
    val (instantiatedName, nameBinding, afterName) =
      MacroInstantiationSupport.instantiateBinder(nameTemplate, context, state)
    val (instantiatedParameters, parameterBindings, afterParameters) =
      MacroInstantiationSupport.instantiateBinderList(parameters, context, afterName)
    val bodyContext = context.inScope(nameBinding ++ parameterBindings)
    val (instantiatedBody, afterBody) =
      MacroInstantiationSupport.instantiateExpressions(
        body,
        bodyContext,
        afterParameters,
        instantiateTemplate
      )
    (
      ListExpr(
        SymbolExpr("define", keywordPosition) ::
          ListExpr(instantiatedName :: instantiatedParameters, parameterPosition) ::
          instantiatedBody,
        position
      ),
      afterBody
    )

  private def dedupeAliases(
    aliases: List[(String, BindingCell)]
  ): List[(String, BindingCell)] =
    aliases.foldLeft(List.empty[(String, BindingCell)]):
      case (current, alias @ (name, _)) =>
        if current.exists(_._1 == name) then current else current :+ alias
