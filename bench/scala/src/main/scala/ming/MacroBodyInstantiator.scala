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
      Option[Expr],
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
    instantiateProcedureDefine: ProcedureDefineInstantiator,
    tailExpression: Option[Expr] = None
  ): (List[Expr], MacroInstantiationState) =
    expressions match
      case Nil =>
        tailExpression match
          case Some(tailTemplate) =>
            val (instantiatedTail, nextState) =
              instantiateTemplate(tailTemplate, context, state)
            val tailExpressions =
              ListExprSupport.requireProperList(
                instantiatedTail,
                "macro expansion produced an invalid body"
              )
            instantiate(
              tailExpressions,
              context,
              nextState,
              instantiateTemplate,
              instantiateValueDefine,
              instantiateProcedureDefine
            )
          case None =>
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
            instantiateProcedureDefine,
            tailExpression
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
      case ListExpr(items, position) =>
        val decoded = ListExprSupport.decode(items, position, "macro expansion")
        decoded.items match
          case SymbolExpr("define", keywordPosition) ::
              ListExpr(nameTemplate :: parameters, parameterPosition) ::
              body if body.nonEmpty || decoded.tail.nonEmpty =>
            instantiateProcedureDefine(
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
          case SymbolExpr("define", keywordPosition) :: nameTemplate :: value :: Nil if decoded.tail.isEmpty =>
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
      case _ =>
        val (instantiatedExpression, nextState) =
          instantiateTemplate(expression, context, state)
        (instantiatedExpression, Map.empty, nextState)
