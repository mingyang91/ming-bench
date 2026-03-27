package ming

private[ming] object MacroBodyInstantiator:

  private type TemplateInstantiator =
    (Expr, MacroInstantiationContext, MacroInstantiationState) => (Expr, MacroInstantiationState)

  private type ValueDefineInstantiator =
    (
      Position,
      Expr,
      Expr,
      Position,
      MacroInstantiationContext,
      MacroInstantiationState
    ) => (Expr, Map[String, String], MacroInstantiationState)

  private type ProcedureDefineInstantiator =
    (
      Position,
      Expr,
      List[Expr],
      Position,
      List[Expr],
      Position,
      MacroInstantiationContext,
      MacroInstantiationState
    ) => (Expr, Map[String, String], MacroInstantiationState)

  def instantiate(
    expressions: List[Expr],
    context: MacroInstantiationContext,
    state: MacroInstantiationState,
    instantiateTemplate: TemplateInstantiator,
    instantiateValueDefine: ValueDefineInstantiator,
    instantiateProcedureDefine: ProcedureDefineInstantiator
  ): (List[Expr], MacroInstantiationState) =
    expressions match
      case Nil =>
        (Nil, state)
      case expression :: rest =>
        val (instantiatedExpression, introducedBindings, nextState) =
          instantiateExpression(
            expression,
            context,
            state,
            instantiateTemplate,
            instantiateValueDefine,
            instantiateProcedureDefine
          )
        val (instantiatedRest, finalState) =
          instantiate(
            rest,
            context.inScope(introducedBindings),
            nextState,
            instantiateTemplate,
            instantiateValueDefine,
            instantiateProcedureDefine
          )
        (instantiatedExpression :: instantiatedRest, finalState)

  private def instantiateExpression(
    expression: Expr,
    context: MacroInstantiationContext,
    state: MacroInstantiationState,
    instantiateTemplate: TemplateInstantiator,
    instantiateValueDefine: ValueDefineInstantiator,
    instantiateProcedureDefine: ProcedureDefineInstantiator
  ): (Expr, Map[String, String], MacroInstantiationState) =
    expression match
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
      case _ =>
        val (instantiatedExpression, nextState) =
          instantiateTemplate(expression, context, state)
        (instantiatedExpression, Map.empty, nextState)
