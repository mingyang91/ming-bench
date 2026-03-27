package ming

private[ming] object MacroMatcher:

  def matchRule(
    application: ListExpr,
    macroDefinition: SyntaxRulesMacro
  ): Option[(SyntaxRule, Map[String, PatternBinding])] =
    macroDefinition.rules.iterator
      .map(rule =>
        matchPattern(
          rule.pattern,
          application,
          macroDefinition.literals,
          macroDefinition.name
        ).map(bindings => rule -> bindings)
      )
      .collectFirst { case Some(matchedRule) => matchedRule }

  private def matchPattern(
    pattern: Expr,
    input: Expr,
    literalIdentifiers: Set[String],
    macroName: String
  ): Option[Map[String, PatternBinding]] =
    pattern match
      case SymbolExpr("...", _) =>
        None
      case SymbolExpr(name, _) if isLiteralIdentifier(name, literalIdentifiers, macroName) =>
        input match
          case SymbolExpr(inputName, _) if inputName == name =>
            Some(Map.empty)
          case _ =>
            None
      case SymbolExpr(name, _) =>
        Some(Map(name -> ScalarBinding(input)))
      case IntExpr(value, _) =>
        input match
          case IntExpr(inputValue, _) if inputValue == value =>
            Some(Map.empty)
          case _ =>
            None
      case RationalExpr(numerator, denominator, _) =>
        input match
          case RationalExpr(inputNumerator, inputDenominator, _)
              if inputNumerator == numerator && inputDenominator == denominator =>
            Some(Map.empty)
          case _ =>
            None
      case InexactExpr(value, _) =>
        input match
          case InexactExpr(inputValue, _) if java.lang.Double.compare(inputValue, value) == 0 =>
            Some(Map.empty)
          case _ =>
            None
      case BoolExpr(value, _) =>
        input match
          case BoolExpr(inputValue, _) if inputValue == value =>
            Some(Map.empty)
          case _ =>
            None
      case StringExpr(value, _) =>
        input match
          case StringExpr(inputValue, _) if inputValue == value =>
            Some(Map.empty)
          case _ =>
            None
      case CharExpr(value, _) =>
        input match
          case CharExpr(inputValue, _) if inputValue == value =>
            Some(Map.empty)
          case _ =>
            None
      case ListExpr(patternItems, patternPosition) =>
        input match
          case ListExpr(inputItems, inputPosition) =>
            val decodedPattern =
              ListExprSupport.decode(patternItems, patternPosition, "syntax-rules pattern")
            val decodedInput =
              ListExprSupport.decode(inputItems, inputPosition, "syntax-rules input")
            matchList(
              decodedPattern.items,
              decodedPattern.tail,
              decodedInput.items,
              decodedInput.tail,
              inputPosition,
              literalIdentifiers,
              macroName
            )
          case _ =>
            None

  private def matchList(
    patternItems: List[Expr],
    patternTail: Option[Expr],
    inputItems: List[Expr],
    inputTail: Option[Expr],
    inputPosition: Position,
    literalIdentifiers: Set[String],
    macroName: String
  ): Option[Map[String, PatternBinding]] =
    patternItems match
      case Nil =>
        patternTail match
          case Some(tailPattern) =>
            matchPattern(
              tailPattern,
              ListExprSupport.build(inputItems, inputTail, inputPosition, "syntax-rules input"),
              literalIdentifiers,
              macroName
            )
          case None =>
            Option.when(inputItems.isEmpty && inputTail.isEmpty)(Map.empty)
      case pattern :: SymbolExpr("...", _) :: rest =>
        matchRepeatedPattern(
          pattern,
          rest,
          patternTail,
          inputItems,
          inputTail,
          inputPosition,
          literalIdentifiers,
          macroName
        )
      case pattern :: rest =>
        inputItems match
          case input :: remaining =>
            matchPattern(pattern, input, literalIdentifiers, macroName).flatMap(firstBindings =>
              matchList(
                rest,
                patternTail,
                remaining,
                inputTail,
                inputPosition,
                literalIdentifiers,
                macroName
              ).flatMap(remainingBindings => mergeBindings(firstBindings, remainingBindings))
            )
          case Nil =>
            None

  private def matchRepeatedPattern(
    pattern: Expr,
    rest: List[Expr],
    patternTail: Option[Expr],
    inputItems: List[Expr],
    inputTail: Option[Expr],
    inputPosition: Position,
    literalIdentifiers: Set[String],
    macroName: String
  ): Option[Map[String, PatternBinding]] =
    val repeatedVariables = patternVariables(pattern, literalIdentifiers, macroName)
    (0 to inputItems.length).iterator
      .map(repetitionCount =>
        val (prefix, suffix) = inputItems.splitAt(repetitionCount)
        val repeatedMatches  = matchRepeatedInputs(pattern, prefix, literalIdentifiers, macroName)
        val suffixBindings =
          matchList(
            rest,
            patternTail,
            suffix,
            inputTail,
            inputPosition,
            literalIdentifiers,
            macroName
          )
        repeatedMatches.flatMap(matches =>
          collectRepeatedBindings(repeatedVariables, matches)
            .flatMap(bindings => suffixBindings.flatMap(otherBindings => mergeBindings(bindings, otherBindings)))
        )
      )
      .collectFirst { case Some(matchedBindings) => matchedBindings }

  private def matchRepeatedInputs(
    pattern: Expr,
    inputs: List[Expr],
    literalIdentifiers: Set[String],
    macroName: String
  ): Option[List[Map[String, PatternBinding]]] =
    inputs.foldLeft(Option(List.empty[Map[String, PatternBinding]])):
      case (Some(matches), expression) =>
        matchPattern(pattern, expression, literalIdentifiers, macroName).map(matches :+ _)
      case (None, _) =>
        None

  private def collectRepeatedBindings(
    variables: Set[String],
    matches: List[Map[String, PatternBinding]]
  ): Option[Map[String, PatternBinding]] =
    variables.foldLeft(Option(Map.empty[String, PatternBinding])):
      case (Some(bindings), name) =>
        collectRepeatedBinding(name, matches).map(collected => bindings.updated(name, RepeatedBinding(collected)))
      case (None, _) =>
        None

  private def collectRepeatedBinding(
    name: String,
    matches: List[Map[String, PatternBinding]]
  ): Option[Vector[PatternBinding]] =
    if matches.isEmpty then Some(Vector.empty)
    else
      matches.foldLeft(Option(Vector.empty[PatternBinding])):
        case (Some(current), matchedBindings) =>
          matchedBindings.get(name).map(value => current :+ value)
        case (None, _) =>
          None

  private def mergeBindings(
    left: Map[String, PatternBinding],
    right: Map[String, PatternBinding]
  ): Option[Map[String, PatternBinding]] =
    right.foldLeft(Option(left)):
      case (Some(current), (name, binding)) =>
        current.get(name) match
          case Some(existing) if !sameBinding(existing, binding) =>
            None
          case Some(_) =>
            Some(current)
          case None =>
            Some(current.updated(name, binding))
      case (None, _) =>
        None

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

  private def patternVariables(
    pattern: Expr,
    literalIdentifiers: Set[String],
    macroName: String
  ): Set[String] =
    pattern match
      case SymbolExpr("...", _) =>
        Set.empty
      case SymbolExpr(name, _) if isLiteralIdentifier(name, literalIdentifiers, macroName) =>
        Set.empty
      case SymbolExpr(name, _) =>
        Set(name)
      case ListExpr(items, position) =>
        val decoded = ListExprSupport.decode(items, position, "syntax-rules pattern")
        decoded.items
          .filterNot(isEllipsis)
          .flatMap(item => patternVariables(item, literalIdentifiers, macroName))
          .toSet ++
          decoded.tail.toSet.flatMap(item => patternVariables(item, literalIdentifiers, macroName))
      case _ =>
        Set.empty

  private def isLiteralIdentifier(
    name: String,
    literalIdentifiers: Set[String],
    macroName: String
  ): Boolean =
    literalIdentifiers.contains(name) || name == macroName

  private def isEllipsis(expression: Expr): Boolean =
    expression match
      case SymbolExpr("...", _) => true
      case _                    => false
