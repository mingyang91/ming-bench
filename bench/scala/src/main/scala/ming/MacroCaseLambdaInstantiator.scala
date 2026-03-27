package ming

private[ming] object MacroCaseLambdaInstantiator:

  private type Instantiator =
    (Expr, MacroInstantiationContext, MacroInstantiationState) => (Expr, MacroInstantiationState)

  def instantiate(
    keywordPosition: Position,
    clauses: List[Expr],
    position: Position,
    context: MacroInstantiationContext,
    state: MacroInstantiationState,
    instantiateTemplate: Instantiator
  ): (Expr, MacroInstantiationState) =
    val (instantiatedClauses, nextState) =
      instantiateClauses(clauses, context, state, instantiateTemplate)
    (
      ListExpr(
        SymbolExpr("case-lambda", keywordPosition) :: instantiatedClauses,
        position
      ),
      nextState
    )

  private def instantiateClauses(
    clauses: List[Expr],
    context: MacroInstantiationContext,
    state: MacroInstantiationState,
    instantiateTemplate: Instantiator
  ): (List[Expr], MacroInstantiationState) =
    clauses match
      case Nil =>
        (Nil, state)
      case clause :: SymbolExpr("...", _) :: rest =>
        val repetitions =
          MacroInstantiationHygiene.repetitionCount(
            clause,
            context.bindings,
            context.repetitionContext
          )
        val (expandedClauses, afterExpansion) =
          instantiateRepeatedClause(
            clause,
            repetitions,
            context,
            state,
            instantiateTemplate
          )
        val (remainingClauses, nextState) =
          instantiateClauses(rest, context, afterExpansion, instantiateTemplate)
        (expandedClauses ++ remainingClauses, nextState)
      case clause :: rest =>
        val (instantiatedClause, afterClause) =
          instantiateClause(clause, context, state, instantiateTemplate)
        val (remainingClauses, nextState) =
          instantiateClauses(rest, context, afterClause, instantiateTemplate)
        (instantiatedClause :: remainingClauses, nextState)

  private def instantiateRepeatedClause(
    clause: Expr,
    repetitions: Range,
    context: MacroInstantiationContext,
    state: MacroInstantiationState,
    instantiateTemplate: Instantiator
  ): (List[Expr], MacroInstantiationState) =
    val (instantiated, nextState) =
      repetitions.foldLeft((Vector.empty[Expr], state)):
        case ((items, currentState), index) =>
          val (instantiatedClause, updatedState) =
            instantiateClause(
              clause,
              context.inRepetition(index),
              currentState,
              instantiateTemplate
            )
          (items :+ instantiatedClause, updatedState)
    (instantiated.toList, nextState)

  private def instantiateClause(
    clause: Expr,
    context: MacroInstantiationContext,
    state: MacroInstantiationState,
    instantiateTemplate: Instantiator
  ): (Expr, MacroInstantiationState) =
    clause match
      case ListExpr(items, clausePosition) =>
        val decoded =
          ListExprSupport.decode(items, clausePosition, "macro expansion produced an invalid case-lambda clause")
        decoded.items match
          case parameterSpec :: body if body.nonEmpty || decoded.tail.nonEmpty =>
            val (instantiatedParameterSpec, introducedBindings, afterParameters) =
              MacroInstantiationSupport.instantiateBinderSpec(parameterSpec, context, state)
            val (instantiatedBody, afterBody) =
              MacroInstantiationSupport.instantiateProperListTemplate(
                body,
                decoded.tail,
                clausePosition,
                "macro expansion produced an invalid case-lambda body",
                context.inScope(introducedBindings),
                afterParameters,
                instantiateTemplate
              )
            if instantiatedBody.isEmpty then
              SchemeFailure.raise(
                "macro expansion produced an invalid case-lambda clause",
                clausePosition
              )

            (
              ListExpr(instantiatedParameterSpec :: instantiatedBody, clausePosition),
              afterBody
            )
          case _ =>
            SchemeFailure.raise(
              "macro expansion produced an invalid case-lambda clause",
              clause.position
            )
      case _ =>
        SchemeFailure.raise(
          "macro expansion produced an invalid case-lambda clause",
          clause.position
        )
