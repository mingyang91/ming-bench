package ming

import SchemeModel.*
import SchemeMacros.*

private[ming] trait SchemeMacroTemplateFormSupport extends SchemeMacroTemplateScopeSupport:

  protected def instantiateItems(
    items: List[Expr],
    state: ExpansionState,
    renamedBindings: Map[String, String],
    repetitionIndex: Option[Int]
  ): (List[Expr], ExpansionState)

  final protected def instantiateList(
    items: List[Expr],
    pos: SourcePos,
    state: ExpansionState,
    renamedBindings: Map[String, String],
    repetitionIndex: Option[Int]
  ): (Expr, ExpansionState) =
    items match
      case Expr.Symbol("quote", headPos) :: rest =>
        (Expr.ListExpr(Expr.Symbol("quote", headPos) :: rest, pos), state)
      case Expr.Symbol("define", headPos) :: rest =>
        instantiateDefine(headPos, rest, pos, state, renamedBindings, repetitionIndex)
      case Expr.Symbol("let", headPos) :: rest =>
        instantiateLet(headPos, rest, pos, state, renamedBindings, repetitionIndex)
      case Expr.Symbol("lambda", headPos) :: rest =>
        instantiateLambda(headPos, rest, pos, state, renamedBindings, repetitionIndex)
      case Expr.Symbol("guard", headPos) :: rest =>
        instantiateGuard(headPos, rest, pos, state, renamedBindings, repetitionIndex)
      case Expr.Symbol(keyword, headPos) :: rest if syntaxKeywords.contains(keyword) =>
        val (instantiatedRest, nextState) =
          instantiateItems(rest, state, renamedBindings, repetitionIndex)
        (Expr.ListExpr(Expr.Symbol(keyword, headPos) :: instantiatedRest, pos), nextState)
      case _ =>
        val (instantiatedItems, nextState) =
          instantiateItems(items, state, renamedBindings, repetitionIndex)
        (Expr.ListExpr(instantiatedItems, pos), nextState)

  private def instantiateDefine(
    headPos: SourcePos,
    rest: List[Expr],
    pos: SourcePos,
    state: ExpansionState,
    renamedBindings: Map[String, String],
    repetitionIndex: Option[Int]
  ): (Expr, ExpansionState) =
    rest match
      case (nameExpr @ Expr.Symbol(_, _)) :: valueExpr :: Nil =>
        val (instantiatedName, nextRename) =
          instantiateBinderName(nameExpr, renamedBindings, repetitionIndex)
        val extendedRenamed = nextRename match
          case Some((originalName, freshName)) =>
            renamedBindings.updated(originalName, freshName)
          case None =>
            renamedBindings
        val (instantiatedValue, nextState) =
          instantiateTemplateExpr(valueExpr, state, extendedRenamed, repetitionIndex)

        (
          Expr.ListExpr(
            List(Expr.Symbol("define", headPos), instantiatedName, instantiatedValue),
            pos
          ),
          nextState
        )
      case Expr.ListExpr(definedName :: rawParams, signaturePos) :: body if body.nonEmpty =>
        val (instantiatedName, nextRename) =
          instantiateBinderName(definedName, renamedBindings, repetitionIndex)
        val renamedWithName = nextRename match
          case Some((originalName, freshName)) =>
            renamedBindings.updated(originalName, freshName)
          case None =>
            renamedBindings
        val (instantiatedParams, extendedRenamed) =
          instantiateParameterList(rawParams, renamedWithName, repetitionIndex)
        val (instantiatedBody, nextState) =
          instantiateBodyItems(body, state, extendedRenamed, repetitionIndex)

        (
          Expr.ListExpr(
            Expr.Symbol("define", headPos) ::
              Expr.ListExpr(instantiatedName :: instantiatedParams, signaturePos) ::
              instantiatedBody,
            pos
          ),
          nextState
        )
      case _ =>
        val (instantiatedRest, nextState) =
          instantiateItems(rest, state, renamedBindings, repetitionIndex)
        (Expr.ListExpr(Expr.Symbol("define", headPos) :: instantiatedRest, pos), nextState)

  private def instantiateLet(
    headPos: SourcePos,
    rest: List[Expr],
    pos: SourcePos,
    state: ExpansionState,
    renamedBindings: Map[String, String],
    repetitionIndex: Option[Int]
  ): (Expr, ExpansionState) =
    rest match
      case (nameExpr @ Expr.Symbol(_, _)) :: Expr.ListExpr(rawBindings, bindingsPos) :: body if body.nonEmpty =>
        val (instantiatedName, nameRename) =
          instantiateBinderName(nameExpr, renamedBindings, repetitionIndex)
        val renamedWithName = nameRename match
          case Some((originalName, freshName)) =>
            renamedBindings.updated(originalName, freshName)
          case None =>
            renamedBindings
        val (instantiatedBindings, bindingsRenamed, afterBindingsState) =
          instantiateLetBindings(rawBindings, state, renamedBindings, repetitionIndex)
        val bodyRenamed = bindingsRenamed ++ renamedWithName
        val (instantiatedBody, afterBodyState) =
          instantiateBodyItems(body, afterBindingsState, bodyRenamed, repetitionIndex)

        (
          Expr.ListExpr(
            Expr.Symbol("let", headPos) ::
              instantiatedName ::
              Expr.ListExpr(instantiatedBindings, bindingsPos) ::
              instantiatedBody,
            pos
          ),
          afterBodyState
        )
      case Expr.ListExpr(rawBindings, bindingsPos) :: body if body.nonEmpty =>
        val (instantiatedBindings, extendedRenamed, afterBindingsState) =
          instantiateLetBindings(rawBindings, state, renamedBindings, repetitionIndex)
        val (instantiatedBody, afterBodyState) =
          instantiateBodyItems(body, afterBindingsState, extendedRenamed, repetitionIndex)

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
          instantiateBodyItems(body, state, extendedRenamed, repetitionIndex)

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

  private def instantiateGuard(
    headPos: SourcePos,
    rest: List[Expr],
    pos: SourcePos,
    state: ExpansionState,
    renamedBindings: Map[String, String],
    repetitionIndex: Option[Int]
  ): (Expr, ExpansionState) =
    rest match
      case Expr.ListExpr((exceptionName @ Expr.Symbol(_, _)) :: clauses, guardArgsPos) :: body if body.nonEmpty =>
        val (instantiatedExceptionName, nextRename) =
          instantiateBinderName(exceptionName, renamedBindings, repetitionIndex)
        val guardRenamed = nextRename match
          case Some((originalName, freshName)) =>
            renamedBindings.updated(originalName, freshName)
          case None =>
            renamedBindings
        val (instantiatedClauses, afterClausesState) =
          instantiateItems(clauses, state, guardRenamed, repetitionIndex)
        val (instantiatedBody, nextState) =
          instantiateBodyItems(body, afterClausesState, renamedBindings, repetitionIndex)

        (
          Expr.ListExpr(
            Expr.Symbol("guard", headPos) ::
              Expr.ListExpr(instantiatedExceptionName :: instantiatedClauses, guardArgsPos) ::
              instantiatedBody,
            pos
          ),
          nextState
        )
      case _ =>
        val (instantiatedRest, nextState) =
          instantiateItems(rest, state, renamedBindings, repetitionIndex)
        (Expr.ListExpr(Expr.Symbol("guard", headPos) :: instantiatedRest, pos), nextState)
