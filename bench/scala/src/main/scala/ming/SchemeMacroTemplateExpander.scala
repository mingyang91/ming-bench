package ming

import scala.annotation.tailrec

import SchemeModel.*
import SchemeMacros.*

final private[ming] class SchemeMacroTemplateExpander(
  rule: SyntaxRule,
  bindings: PatternBindings,
  useSiteEnv: Env,
  definitionEnv: Env
):

  final private case class ExpansionState(
    expansionEnv: Env,
    capturedIdentifiers: Map[String, String]
  )

  def expand(template: Expr): ExpandedExpr =
    val initialState = ExpansionState(new Env(Some(useSiteEnv)), Map.empty)
    val (expanded, finalState) = instantiate(
      template,
      initialState,
      renamedBindings = Map.empty,
      repetitionIndex = None
    )
    ExpandedExpr(expanded, finalState.expansionEnv)

  private def instantiate(
    template: Expr,
    state: ExpansionState,
    renamedBindings: Map[String, String],
    repetitionIndex: Option[Int]
  ): (Expr, ExpansionState) =
    template match
      case Expr.Symbol(symbolName, pos) =>
        instantiateSymbol(symbolName, pos, state, renamedBindings, repetitionIndex)
      case Expr.ListExpr(items, pos) =>
        instantiateList(items, pos, state, renamedBindings, repetitionIndex)
      case other =>
        (other, state)

  private def instantiateSymbol(
    symbolName: String,
    pos: SourcePos,
    state: ExpansionState,
    renamedBindings: Map[String, String],
    repetitionIndex: Option[Int]
  ): (Expr, ExpansionState) =
    renamedBindings.get(symbolName) match
      case Some(renamed) =>
        (Expr.Symbol(renamed, pos), state)
      case None if rule.patternVariables.contains(symbolName) =>
        (lookupPatternBinding(symbolName, bindings, repetitionIndex), state)
      case None =>
        val (capturedIdentifier, nextState) = captureIdentifier(symbolName, state)
        (Expr.Symbol(capturedIdentifier, pos), nextState)

  private def instantiateList(
    items: List[Expr],
    pos: SourcePos,
    state: ExpansionState,
    renamedBindings: Map[String, String],
    repetitionIndex: Option[Int]
  ): (Expr, ExpansionState) =
    items match
      case Expr.Symbol("let", headPos) :: rest =>
        instantiateLet(headPos, rest, pos, state, renamedBindings, repetitionIndex)
      case Expr.Symbol("lambda", headPos) :: rest =>
        instantiateLambda(headPos, rest, pos, state, renamedBindings, repetitionIndex)
      case Expr.Symbol(keyword, headPos) :: rest if syntaxKeywords.contains(keyword) =>
        val (instantiatedRest, nextState) =
          instantiateItems(rest, state, renamedBindings, repetitionIndex)
        (Expr.ListExpr(Expr.Symbol(keyword, headPos) :: instantiatedRest, pos), nextState)
      case _ =>
        val (instantiatedItems, nextState) =
          instantiateItems(items, state, renamedBindings, repetitionIndex)
        (Expr.ListExpr(instantiatedItems, pos), nextState)

  private def instantiateItems(
    items: List[Expr],
    state: ExpansionState,
    renamedBindings: Map[String, String],
    repetitionIndex: Option[Int]
  ): (List[Expr], ExpansionState) =
    @tailrec
    def loop(
      remaining: List[Expr],
      currentState: ExpansionState,
      reversedItems: List[Expr]
    ): (List[Expr], ExpansionState) =
      remaining match
        case item :: Expr.Symbol("...", _) :: tail =>
          val count = repetitionCount(item, rule.patternVariables, bindings)
          val (expandedItems, nextState) =
            instantiateRepeatedItem(item, count, currentState, renamedBindings)
          loop(tail, nextState, expandedItems.reverse ::: reversedItems)
        case head :: tail =>
          val (instantiatedHead, nextState) =
            instantiate(head, currentState, renamedBindings, repetitionIndex)
          loop(tail, nextState, instantiatedHead :: reversedItems)
        case Nil =>
          (reversedItems.reverse, currentState)

    loop(items, state, Nil)

  private def instantiateRepeatedItem(
    template: Expr,
    count: Int,
    state: ExpansionState,
    renamedBindings: Map[String, String]
  ): (List[Expr], ExpansionState) =
    @tailrec
    def loop(
      index: Int,
      currentState: ExpansionState,
      reversedItems: List[Expr]
    ): (List[Expr], ExpansionState) =
      if index >= count then (reversedItems.reverse, currentState)
      else
        val (instantiatedItem, nextState) =
          instantiate(template, currentState, renamedBindings, Some(index))
        loop(index + 1, nextState, instantiatedItem :: reversedItems)

    loop(0, state, Nil)

  private def instantiateLet(
    headPos: SourcePos,
    rest: List[Expr],
    pos: SourcePos,
    state: ExpansionState,
    renamedBindings: Map[String, String],
    repetitionIndex: Option[Int]
  ): (Expr, ExpansionState) =
    rest match
      case Expr.ListExpr(rawBindings, bindingsPos) :: body if body.nonEmpty =>
        val (instantiatedBindings, extendedRenamed, afterBindingsState) =
          instantiateLetBindings(rawBindings, state, renamedBindings, repetitionIndex)
        val (instantiatedBody, afterBodyState) =
          instantiateItems(body, afterBindingsState, extendedRenamed, repetitionIndex)

        (
          Expr.ListExpr(
            Expr.Symbol("let", headPos) ::
              Expr.ListExpr(instantiatedBindings, bindingsPos) ::
              instantiatedBody,
            pos
          ),
          afterBodyState
        )
      case _ =>
        val (instantiatedRest, nextState) =
          instantiateItems(rest, state, renamedBindings, repetitionIndex)
        (Expr.ListExpr(Expr.Symbol("let", headPos) :: instantiatedRest, pos), nextState)

  private def instantiateLetBindings(
    rawBindings: List[Expr],
    state: ExpansionState,
    renamedBindings: Map[String, String],
    repetitionIndex: Option[Int]
  ): (List[Expr], Map[String, String], ExpansionState) =
    @tailrec
    def loop(
      remaining: List[Expr],
      currentState: ExpansionState,
      collectedRenames: Map[String, String],
      reversedBindings: List[Expr]
    ): (List[Expr], Map[String, String], ExpansionState) =
      remaining match
        case Expr.ListExpr(List(nameExpr, valueExpr), bindingPos) :: tail =>
          val (instantiatedName, nextRename) =
            instantiateBinderName(nameExpr, renamedBindings, repetitionIndex)
          val (instantiatedValue, nextState) =
            instantiate(valueExpr, currentState, renamedBindings, repetitionIndex)
          val updatedRenames = nextRename match
            case Some((originalName, freshName)) =>
              collectedRenames.updated(originalName, freshName)
            case None =>
              collectedRenames
          val instantiatedBinding =
            Expr.ListExpr(List(instantiatedName, instantiatedValue), bindingPos)

          loop(tail, nextState, updatedRenames, instantiatedBinding :: reversedBindings)
        case _ :: _ =>
          throw new EvalError("macro-generated let bindings must contain (name value) pairs")
        case Nil =>
          (reversedBindings.reverse, renamedBindings ++ collectedRenames, currentState)

    loop(rawBindings, state, Map.empty, Nil)

  private def instantiateLambda(
    headPos: SourcePos,
    rest: List[Expr],
    pos: SourcePos,
    state: ExpansionState,
    renamedBindings: Map[String, String],
    repetitionIndex: Option[Int]
  ): (Expr, ExpansionState) =
    rest match
      case Expr.ListExpr(rawParams, paramsPos) :: body if body.nonEmpty =>
        val (instantiatedParams, extendedRenamed) =
          instantiateParameterList(rawParams, renamedBindings, repetitionIndex)
        val (instantiatedBody, nextState) =
          instantiateItems(body, state, extendedRenamed, repetitionIndex)

        (
          Expr.ListExpr(
            Expr.Symbol("lambda", headPos) ::
              Expr.ListExpr(instantiatedParams, paramsPos) ::
              instantiatedBody,
            pos
          ),
          nextState
        )
      case _ =>
        val (instantiatedRest, nextState) =
          instantiateItems(rest, state, renamedBindings, repetitionIndex)
        (Expr.ListExpr(Expr.Symbol("lambda", headPos) :: instantiatedRest, pos), nextState)

  private def instantiateParameterList(
    rawParams: List[Expr],
    renamedBindings: Map[String, String],
    repetitionIndex: Option[Int]
  ): (List[Expr], Map[String, String]) =
    @tailrec
    def loop(
      remaining: List[Expr],
      currentRenamed: Map[String, String],
      reversedParams: List[Expr]
    ): (List[Expr], Map[String, String]) =
      remaining match
        case Expr.Symbol(".", pos) :: tail =>
          loop(tail, currentRenamed, Expr.Symbol(".", pos) :: reversedParams)
        case paramExpr :: tail =>
          val (instantiatedName, nextRename) =
            instantiateBinderName(paramExpr, currentRenamed, repetitionIndex)
          val updatedRenamed = nextRename match
            case Some((originalName, freshName)) =>
              currentRenamed.updated(originalName, freshName)
            case None =>
              currentRenamed
          loop(tail, updatedRenamed, instantiatedName :: reversedParams)
        case Nil =>
          (reversedParams.reverse, currentRenamed)

    loop(rawParams, renamedBindings, Nil)

  private def instantiateBinderName(
    expr: Expr,
    renamedBindings: Map[String, String],
    repetitionIndex: Option[Int]
  ): (Expr, Option[(String, String)]) =
    expr match
      case Expr.Symbol(".", pos) =>
        (Expr.Symbol(".", pos), None)
      case Expr.Symbol(name, pos) if renamedBindings.contains(name) =>
        (Expr.Symbol(renamedBindings(name), pos), None)
      case Expr.Symbol(name, _) if rule.patternVariables.contains(name) =>
        val inserted   = lookupPatternBinding(name, bindings, repetitionIndex)
        val identifier = requireIdentifier(inserted, "macro-generated binding names must be symbols")
        (Expr.Symbol(identifier, expr.pos), None)
      case Expr.Symbol(name, pos) =>
        val freshName = freshIdentifier(name)
        (Expr.Symbol(freshName, pos), Some(name -> freshName))
      case _ =>
        throw new EvalError("macro-generated binding names must be symbols")

  private def captureIdentifier(
    name: String,
    state: ExpansionState
  ): (String, ExpansionState) =
    state.capturedIdentifiers.get(name) match
      case Some(aliasName) =>
        (aliasName, state)
      case None =>
        val aliasName     = freshIdentifier(name)
        val aliasedValue  = definitionEnv.lookupValueCell(name)
        val aliasedSyntax = definitionEnv.lookupSyntax(name)

        aliasedValue.foreach(state.expansionEnv.defineAlias(aliasName, _))
        aliasedSyntax.foreach(state.expansionEnv.defineSyntax(aliasName, _))

        if aliasedValue.isEmpty && aliasedSyntax.isEmpty then throw new EvalError(s"unbound variable: $name")

        val nextState =
          state.copy(capturedIdentifiers = state.capturedIdentifiers.updated(name, aliasName))
        (aliasName, nextState)
