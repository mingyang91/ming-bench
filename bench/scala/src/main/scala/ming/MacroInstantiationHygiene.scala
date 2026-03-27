package ming

private[ming] object MacroInstantiationHygiene:

  private val InternalPosition = Position(1, 1)

  private val SyntaxKeywords = Set(
    "and",
    "begin",
    "case",
    "case-lambda",
    "cond",
    "define",
    "define-syntax",
    "do",
    "guard",
    "if",
    "lambda",
    "let",
    "letrec",
    "letrec*",
    "or",
    "quote",
    "set!",
    "syntax",
    "syntax-case",
    "syntax-rules",
    "with-syntax"
  )

  def instantiateSymbol(
    name: String,
    position: Position,
    context: MacroInstantiationContext,
    state: MacroInstantiationState
  ): (Expr, MacroInstantiationState) =
    context.scopeRenames.get(name) match
      case Some(renamed) =>
        (SymbolExpr(renamed, position), state)
      case None =>
        context.bindings.get(name) match
          case Some(binding) =>
            (selectScalar(binding, context.repetitionContext, position), state)
          case None if shouldKeepIdentifier(name, context.macroDefinition) =>
            (SymbolExpr(name, position), state)
          case None =>
            instantiateCapturedIdentifier(name, position, context.macroDefinition, state)

  def repetitionCount(
    template: Expr,
    bindings: Map[String, PatternBinding],
    repetitionContext: List[Int]
  ): Range =
    val counts =
      repeatedPatternVariables(template, bindings, repetitionContext)
        .flatMap(name => remainingRepetitionCount(bindings(name), repetitionContext))
        .toList

    counts match
      case Nil =>
        SchemeFailure.raise(
          "template ellipsis must repeat a pattern variable",
          template.position
        )
      case count :: Nil =>
        0 until count
      case _ =>
        SchemeFailure.raise(
          "template ellipsis has incompatible repetition counts",
          template.position
        )

  def selectScalar(
    binding: PatternBinding,
    repetitionContext: List[Int],
    position: Position
  ): Expr =
    selectBinding(binding, repetitionContext, position) match
      case ScalarBinding(expr) =>
        expr
      case RepeatedBinding(_) =>
        SchemeFailure.raise(
          "pattern variable used outside ellipsis context",
          position
        )

  private def instantiateCapturedIdentifier(
    name: String,
    position: Position,
    macroDefinition: SyntaxRulesMacro,
    state: MacroInstantiationState
  ): (Expr, MacroInstantiationState) =
    state.capturedNames.get(name) match
      case Some(capturedName) =>
        (SymbolExpr(capturedName, position), state)
      case None =>
        val (capturedName, namedState) = state.nextFresh(name)
        val capturedState =
          macroDefinition.definitionEnv.lookupCellOption(name) match
            case Some(cell) =>
              namedState.rememberCapture(name, capturedName).addAlias(capturedName, cell)
            case None =>
              namedState.rememberCapture(name, capturedName)
        (SymbolExpr(capturedName, position), capturedState)

  private def repeatedPatternVariables(
    template: Expr,
    bindings: Map[String, PatternBinding],
    repetitionContext: List[Int]
  ): Set[String] =
    template match
      case ListExpr(List(SymbolExpr("quote", _), _), _) =>
        Set.empty
      case SymbolExpr(name, _)
          if bindings.contains(name) &&
            remainingRepetitionCount(bindings(name), repetitionContext).nonEmpty =>
        Set(name)
      case ListExpr(items, _) =>
        items.filterNot(isEllipsis).flatMap(item => repeatedPatternVariables(item, bindings, repetitionContext)).toSet
      case _ =>
        Set.empty

  private def remainingRepetitionCount(
    binding: PatternBinding,
    repetitionContext: List[Int]
  ): Option[Int] =
    selectBinding(binding, repetitionContext, InternalPosition) match
      case RepeatedBinding(values) =>
        Some(values.size)
      case ScalarBinding(_) =>
        None

  private def selectBinding(
    binding: PatternBinding,
    repetitionContext: List[Int],
    position: Position
  ): PatternBinding =
    repetitionContext.foldLeft(binding):
      case (RepeatedBinding(values), index) if values.isDefinedAt(index) =>
        values(index)
      case _ =>
        SchemeFailure.raise("invalid macro repetition context", position)

  private def shouldKeepIdentifier(name: String, macroDefinition: SyntaxRulesMacro): Boolean =
    macroDefinition.definitionEnv.lookupMacro(name).nonEmpty || SyntaxKeywords.contains(name)

  private def isEllipsis(expression: Expr): Boolean =
    expression match
      case SymbolExpr("...", _) => true
      case _                    => false
