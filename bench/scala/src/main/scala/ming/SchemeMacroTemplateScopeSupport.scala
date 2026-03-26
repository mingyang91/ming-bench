package ming

import scala.annotation.tailrec

import SchemeModel.*
import SchemeMacros.*

private[ming] trait SchemeMacroTemplateScopeSupport extends SchemeMacroTemplateBindingSupport:

  protected def instantiateTemplateExpr(
    template: Expr,
    state: ExpansionState,
    renamedBindings: Map[String, String],
    repetitionIndex: Option[Int]
  ): (Expr, ExpansionState)

  protected def instantiateRepeatedTemplateItem(
    template: Expr,
    count: Int,
    state: ExpansionState,
    renamedBindings: Map[String, String]
  ): (List[Expr], ExpansionState)

  final protected def instantiateBodyItems(
    items: List[Expr],
    state: ExpansionState,
    renamedBindings: Map[String, String],
    repetitionIndex: Option[Int]
  ): (List[Expr], ExpansionState) =
    @tailrec
    def loop(
      remaining: List[Expr],
      currentState: ExpansionState,
      currentRenamed: Map[String, String],
      reversedItems: List[Expr]
    ): (List[Expr], ExpansionState) =
      remaining match
        case item :: Expr.Symbol("...", _) :: tail =>
          val count = repetitionCount(item, rule.patternVariables, bindings)
          val (expandedItems, nextState) =
            instantiateRepeatedTemplateItem(item, count, currentState, currentRenamed)
          loop(tail, nextState, currentRenamed, expandedItems.reverse ::: reversedItems)
        case head :: tail =>
          val (instantiatedHead, nextState) =
            instantiateTemplateExpr(head, currentState, currentRenamed, repetitionIndex)
          val nextRenamed =
            extendInternalDefineRenames(head, instantiatedHead, currentRenamed)
          loop(tail, nextState, nextRenamed, instantiatedHead :: reversedItems)
        case Nil =>
          (reversedItems.reverse, currentState)

    loop(items, state, renamedBindings, Nil)

  final protected def instantiateLetBindings(
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
            instantiateTemplateExpr(valueExpr, currentState, renamedBindings, repetitionIndex)
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

  private def extendInternalDefineRenames(
    original: Expr,
    instantiated: Expr,
    renamedBindings: Map[String, String]
  ): Map[String, String] =
    extractDefinedNamePair(original, instantiated) match
      case Some((originalName, instantiatedName))
          if !renamedBindings.contains(originalName) && !rule.patternVariables.contains(
            originalName
          ) =>
        renamedBindings.updated(originalName, instantiatedName)
      case _ =>
        renamedBindings

  private def extractDefinedNamePair(
    original: Expr,
    instantiated: Expr
  ): Option[(String, String)] =
    (original, instantiated) match
      case (
            Expr.ListExpr(Expr.Symbol("define", _) :: Expr.Symbol(originalName, _) :: _ :: Nil, _),
            Expr.ListExpr(
              Expr.Symbol("define", _) :: Expr.Symbol(instantiatedName, _) :: _ :: Nil,
              _
            )
          ) =>
        Some((originalName, instantiatedName))
      case (
            Expr.ListExpr(
              Expr.Symbol("define", _) :: Expr.ListExpr(Expr.Symbol(originalName, _) :: _, _) :: _,
              _
            ),
            Expr.ListExpr(
              Expr.Symbol("define", _) ::
              Expr.ListExpr(Expr.Symbol(instantiatedName, _) :: _, _) ::
              _,
              _
            )
          ) =>
        Some((originalName, instantiatedName))
      case _ =>
        None
