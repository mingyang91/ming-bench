package ming

import scala.annotation.tailrec

import SchemeModel.*
import SchemeMacros.*

private[ming] trait SchemeMacroTemplateBindingSupport:

  protected def rule: SyntaxRule
  protected def bindings: PatternBindings
  protected def definitionEnv: Env

  final protected case class ExpansionState(
    expansionEnv: Env,
    capturedIdentifiers: Map[String, String]
  )

  final protected def instantiateParameterList(
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

  final protected def instantiateBinderName(
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

  final protected def captureIdentifier(
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
