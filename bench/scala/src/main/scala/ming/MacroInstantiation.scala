package ming

import scala.annotation.tailrec

private[ming] object MacroInstantiation:

  import MacroRepetition.*
  import MacroSupport.*

  def instantiate(
    template: Expr,
    bindings: MatchBindings,
    macroDef: SyntaxMacro,
    macros: MacroState,
    scope: Map[String, String],
    repetitionIndex: Option[Int]
  ): Expr =
    template match
      case Expr.Symbol(name, pos) =>
        instantiateSymbol(name, pos, bindings, macroDef, macros, scope, repetitionIndex)

      case Expr.ListExpr(Expr.Symbol("quote", quotePos) :: quoted :: Nil, pos) =>
        Expr.ListExpr(List(Expr.Symbol("quote", quotePos), quoted), pos)

      case Expr.ListExpr(Expr.Symbol("lambda", lambdaPos) :: Expr.ListExpr(params, paramsPos) :: body, pos)
          if body.nonEmpty =>
        instantiateLambda(lambdaPos, params, paramsPos, body, pos, bindings, macroDef, macros, scope, repetitionIndex)

      case Expr.ListExpr(Expr.Symbol("case-lambda", caseLambdaPos) :: clauses, pos) =>
        instantiateCaseLambda(caseLambdaPos, clauses, pos, bindings, macroDef, macros, scope, repetitionIndex)

      case Expr.ListExpr(Expr.Symbol("let", letPos) :: Expr.ListExpr(letBindings, bindingsPos) :: body, pos)
          if body.nonEmpty =>
        instantiateLet(letPos, letBindings, bindingsPos, body, pos, bindings, macroDef, macros, scope, repetitionIndex)

      case Expr.ListExpr(items, pos) =>
        Expr.ListExpr(instantiateItems(items, bindings, macroDef, macros, scope, repetitionIndex), pos)

      case other =>
        other

  private def instantiateLambda(
    lambdaPos: SourcePos,
    params: List[Expr],
    paramsPos: SourcePos,
    body: List[Expr],
    pos: SourcePos,
    bindings: MatchBindings,
    macroDef: SyntaxMacro,
    macros: MacroState,
    scope: Map[String, String],
    repetitionIndex: Option[Int]
  ): Expr =
    val (expandedParams, paramScope) =
      instantiateBinderList(params, bindings, macroDef, macros, scope, repetitionIndex)

    val bodyScope = scope ++ paramScope
    val expandedBody = body.map { expr =>
      instantiate(expr, bindings, macroDef, macros, bodyScope, repetitionIndex)
    }

    Expr.ListExpr(
      Expr.Symbol("lambda", lambdaPos) :: Expr.ListExpr(expandedParams, paramsPos) :: expandedBody,
      pos
    )

  private def instantiateCaseLambda(
    caseLambdaPos: SourcePos,
    clauses: List[Expr],
    pos: SourcePos,
    bindings: MatchBindings,
    macroDef: SyntaxMacro,
    macros: MacroState,
    scope: Map[String, String],
    repetitionIndex: Option[Int]
  ): Expr =
    val expandedClauses = clauses.map {
      case Expr.ListExpr(Expr.ListExpr(params, paramsPos) :: body, clausePos) if body.nonEmpty =>
        val (expandedParams, paramScope) =
          instantiateBinderList(params, bindings, macroDef, macros, scope, repetitionIndex)

        val clauseScope = scope ++ paramScope
        val expandedBody = body.map { expr =>
          instantiate(expr, bindings, macroDef, macros, clauseScope, repetitionIndex)
        }

        Expr.ListExpr(Expr.ListExpr(expandedParams, paramsPos) :: expandedBody, clausePos)

      case other =>
        instantiate(other, bindings, macroDef, macros, scope, repetitionIndex)
    }

    Expr.ListExpr(
      Expr.Symbol("case-lambda", caseLambdaPos) :: expandedClauses,
      pos
    )

  private def instantiateLet(
    letPos: SourcePos,
    letBindings: List[Expr],
    bindingsPos: SourcePos,
    body: List[Expr],
    pos: SourcePos,
    bindings: MatchBindings,
    macroDef: SyntaxMacro,
    macros: MacroState,
    scope: Map[String, String],
    repetitionIndex: Option[Int]
  ): Expr =
    val expandedBindings = List.newBuilder[Expr]
    var bodyScope        = scope

    letBindings.foreach {
      case Expr.ListExpr(nameExpr :: valueExpr :: Nil, bindingPos) =>
        val (expandedName, introduced) =
          instantiateBinder(nameExpr, bindings, macroDef, macros, scope, repetitionIndex)
        val expandedValue = instantiate(valueExpr, bindings, macroDef, macros, scope, repetitionIndex)

        bodyScope = bodyScope ++ introduced
        expandedBindings += Expr.ListExpr(List(expandedName, expandedValue), bindingPos)

      case Expr.ListExpr(_, bindingPos) =>
        throw EvalError.at(bindingPos, "macro let bindings must contain exactly a name and expression")

      case other =>
        throw EvalError.at(exprPos(other), "macro let bindings must be lists")
    }

    val expandedBody = body.map { expr =>
      instantiate(expr, bindings, macroDef, macros, bodyScope, repetitionIndex)
    }

    Expr.ListExpr(
      Expr.Symbol("let", letPos) :: Expr.ListExpr(expandedBindings.result(), bindingsPos) :: expandedBody,
      pos
    )

  private def instantiateBinderList(
    binders: List[Expr],
    bindings: MatchBindings,
    macroDef: SyntaxMacro,
    macros: MacroState,
    scope: Map[String, String],
    repetitionIndex: Option[Int]
  ): (List[Expr], Map[String, String]) =
    val expanded   = List.newBuilder[Expr]
    var introduced = Map.empty[String, String]

    binders.foreach {
      case Expr.Symbol(".", pos) =>
        expanded += Expr.Symbol(".", pos)

      case binder =>
        val (expandedBinder, newScope) =
          instantiateBinder(binder, bindings, macroDef, macros, scope ++ introduced, repetitionIndex)
        expanded += expandedBinder
        introduced = introduced ++ newScope
    }

    (expanded.result(), introduced)

  private def instantiateBinder(
    binder: Expr,
    bindings: MatchBindings,
    macroDef: SyntaxMacro,
    macros: MacroState,
    scope: Map[String, String],
    repetitionIndex: Option[Int]
  ): (Expr, Map[String, String]) =
    binder match
      case Expr.Symbol(name, pos) =>
        boundPatternExpr(name, pos, bindings, repetitionIndex) match
          case Some(Expr.Symbol(boundName, boundPos)) =>
            (Expr.Symbol(boundName, boundPos), Map.empty)

          case Some(other) =>
            throw EvalError.at(exprPos(other), "macro binder must expand to an identifier")

          case None =>
            val fresh = macros.fresh(name)
            (Expr.Symbol(fresh, pos), Map(name -> fresh))

      case other =>
        throw EvalError.at(exprPos(other), "macro binder must be an identifier")

  private def instantiateItems(
    items: List[Expr],
    bindings: MatchBindings,
    macroDef: SyntaxMacro,
    macros: MacroState,
    scope: Map[String, String],
    repetitionIndex: Option[Int]
  ): List[Expr] =
    val expanded = List.newBuilder[Expr]

    @tailrec
    def loop(remaining: List[Expr]): Unit =
      remaining match
        case template :: Expr.Symbol("...", _) :: rest =>
          val repeatCount = repeatedCount(template, bindings, scope.keySet, repetitionIndex)
          (0 until repeatCount).foreach { index =>
            expanded += instantiate(template, bindings, macroDef, macros, scope, Some(index))
          }
          loop(rest)

        case template :: rest =>
          expanded += instantiate(template, bindings, macroDef, macros, scope, repetitionIndex)
          loop(rest)

        case Nil =>
          ()

    loop(items)
    expanded.result()

  private def instantiateSymbol(
    name: String,
    pos: SourcePos,
    bindings: MatchBindings,
    macroDef: SyntaxMacro,
    macros: MacroState,
    scope: Map[String, String],
    repetitionIndex: Option[Int]
  ): Expr =
    boundPatternExpr(name, pos, bindings, repetitionIndex).getOrElse {
      scope.get(name) match
        case Some(freshName) =>
          Expr.Symbol(freshName, pos)

        case None if syntaxKeywords.contains(name) || macros.isMacro(name) =>
          Expr.Symbol(name, pos)

        case None =>
          Expr.Symbol(resolveAlias(name, macroDef, macros), pos)
    }

  private def resolveAlias(name: String, macroDef: SyntaxMacro, macros: MacroState): String =
    macroDef.aliases.getOrElseUpdate(
      name,
      macroDef.definitionEnv.resolveCell(name) match
        case Some(cell) =>
          val alias = macros.fresh(name)
          macroDef.definitionEnv.defineAlias(alias, cell)
          alias

        case None =>
          macros.fresh(name)
    )
