package ming

import scala.annotation.tailrec

import SchemeModel.*
import SchemeMacros.*

final private[ming] class SchemeMacroTemplateExpander(
  override protected val rule: SyntaxRule,
  override protected val bindings: PatternBindings,
  useSiteEnv: Env,
  override protected val definitionEnv: Env
) extends SchemeMacroTemplateFormSupport:

  def expand(template: Expr): ExpandedExpr =
    val initialState = ExpansionState(useSiteEnv, Map.empty)
    val (expanded, finalState) = instantiate(
      template,
      initialState,
      renamedBindings = Map.empty,
      repetitionIndex = None
    )
    ExpandedExpr(expanded, finalState.expansionEnv)

  override protected def instantiateTemplateExpr(
    template: Expr,
    state: ExpansionState,
    renamedBindings: Map[String, String],
    repetitionIndex: Option[Int]
  ): (Expr, ExpansionState) =
    instantiate(template, state, renamedBindings, repetitionIndex)

  override protected def instantiateRepeatedTemplateItem(
    template: Expr,
    count: Int,
    state: ExpansionState,
    renamedBindings: Map[String, String]
  ): (List[Expr], ExpansionState) =
    instantiateRepeatedItem(template, count, state, renamedBindings)

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

  override protected def instantiateItems(
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
