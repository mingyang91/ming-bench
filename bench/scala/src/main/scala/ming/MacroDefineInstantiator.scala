package ming

final private[ming] class MacroDefineInstantiator(
  instantiateTemplate: (Expr, MacroInstantiationContext, MacroInstantiationState) => (
    Expr,
    MacroInstantiationState
  )
):

  def instantiateValue(
    keywordPosition: Position,
    nameTemplate: Expr,
    value: Expr,
    position: Position,
    context: MacroInstantiationContext,
    state: MacroInstantiationState
  ): (Expr, Map[String, String], MacroInstantiationState) =
    val (instantiatedName, introducedBindings, afterName) =
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
      introducedBindings,
      afterValue
    )

  def instantiateProcedure(
    keywordPosition: Position,
    nameTemplate: Expr,
    parameters: List[Expr],
    parameterPosition: Position,
    body: List[Expr],
    bodyTail: Option[Expr],
    position: Position,
    context: MacroInstantiationContext,
    state: MacroInstantiationState
  ): (Expr, Map[String, String], MacroInstantiationState) =
    val (instantiatedName, nameBinding, afterName) =
      MacroInstantiationSupport.instantiateBinder(nameTemplate, context, state)
    val (instantiatedParameters, parameterBindings, afterParameters) =
      MacroInstantiationSupport.instantiateBinderList(parameters, context, afterName)
    val bodyContext = context.inScope(nameBinding ++ parameterBindings)
    val (instantiatedBody, afterBody) =
      MacroBodyInstantiator.instantiate(
        body,
        bodyContext,
        afterParameters,
        instantiateTemplate,
        instantiateValue,
        instantiateProcedure,
        bodyTail
      )
    if instantiatedBody.isEmpty then SchemeFailure.raise("macro expansion produced an invalid define form", position)
    (
      ListExpr(
        SymbolExpr("define", keywordPosition) ::
          ListExpr(instantiatedName :: instantiatedParameters, parameterPosition) ::
          instantiatedBody,
        position
      ),
      nameBinding,
      afterBody
    )
