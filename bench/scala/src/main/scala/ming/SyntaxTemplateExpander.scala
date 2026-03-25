package ming

import scala.collection.mutable

final private[ming] class SyntaxTemplateExpander(
  private val definitionEnv: Env,
  private val definitionMacros: MacroScope,
  private val id: Long
):

  import SchemeInterpreter.Expr
  import SyntaxRules.{Capture, Ellipsis, SyntaxKeywords}
  import Capture.*

  private val valueAliases = mutable.HashMap.empty[String, String]
  private val macroAliases = mutable.HashMap.empty[String, String]

  def instantiate(expr: Expr, bindings: Map[String, Capture]): Expr =
    instantiate(expr, bindings, Map.empty, Nil)

  private def instantiate(
    expr: Expr,
    bindings: Map[String, Capture],
    scope: Map[String, String],
    path: List[Int]
  ): Expr =
    expr match
      case symbol @ Expr.Symbol(name, pos) =>
        if bindings.contains(name) then bindingToExpr(bindings(name), path, pos)
        else
          scope.get(name) match
            case Some(alias) =>
              Expr.Symbol(alias, pos)
            case None =>
              captureValueIdentifier(symbol)

      case Expr.ListExpr(items, pos) =>
        items match
          case Expr.Symbol("quote", _) :: _ if !bindings.contains("quote") =>
            expr

          case Expr.Symbol("lambda", _) :: formals :: body if !bindings.contains("lambda") && body.nonEmpty =>
            val (expandedFormals, bodyScope) = expandFormals(formals, bindings, scope, path)
            val expandedBody                 = body.map(instantiate(_, bindings, bodyScope, path))
            Expr.ListExpr(items.head :: expandedFormals :: expandedBody, pos)

          case Expr.Symbol("let", _) :: rest if !bindings.contains("let") =>
            expandLet(items.head, rest, bindings, scope, path, pos)

          case _ =>
            Expr.ListExpr(expandCallItems(items, bindings, scope, path), pos)

      case Expr.VectorExpr(items, pos) =>
        Expr.VectorExpr(expandItems(items, bindings, scope, path), pos)

      case _ =>
        expr

  private def expandCallItems(
    items: List[Expr],
    bindings: Map[String, Capture],
    scope: Map[String, String],
    path: List[Int]
  ): List[Expr] =
    items match
      case Nil => Nil
      case head :: tail =>
        expandHead(head, bindings, scope, path) :: expandItems(tail, bindings, scope, path)

  private def expandItems(
    items: List[Expr],
    bindings: Map[String, Capture],
    scope: Map[String, String],
    path: List[Int]
  ): List[Expr] =
    items match
      case Nil =>
        Nil

      case item :: Expr.Symbol(Ellipsis, _) :: rest =>
        val count    = repetitionCount(item, bindings, path)
        val expanded = (0 until count).toList.map(index => instantiate(item, bindings, scope, path :+ index))
        expanded ++ expandItems(rest, bindings, scope, path)

      case item :: rest =>
        instantiate(item, bindings, scope, path) :: expandItems(rest, bindings, scope, path)

  private def expandHead(
    expr: Expr,
    bindings: Map[String, Capture],
    scope: Map[String, String],
    path: List[Int]
  ): Expr =
    expr match
      case Expr.Symbol(name, pos) =>
        if bindings.contains(name) then bindingToExpr(bindings(name), path, pos)
        else
          scope.get(name) match
            case Some(alias) =>
              Expr.Symbol(alias, pos)
            case None if SyntaxKeywords.contains(name) =>
              Expr.Symbol(name, pos)
            case None =>
              captureMacroIdentifier(name, pos).getOrElse(captureValueIdentifier(Expr.Symbol(name, pos)))

      case other =>
        instantiate(other, bindings, scope, path)

  private def expandFormals(
    formals: Expr,
    bindings: Map[String, Capture],
    scope: Map[String, String],
    path: List[Int]
  ): (Expr, Map[String, String]) =
    formals match
      case Expr.Symbol(name, pos) =>
        if bindings.contains(name) then (bindingToExpr(bindings(name), path, pos), scope)
        else
          val alias = freshIdentifier("arg", name)
          (Expr.Symbol(alias, pos), scope.updated(name, alias))

      case Expr.ListExpr(items, pos) =>
        val (expanded, nextScope) =
          items.foldLeft((List.empty[Expr], scope)) { case ((acc, currentScope), item) =>
            item match
              case Expr.Symbol(Ellipsis, ellipsisPos) =>
                (Expr.Symbol(Ellipsis, ellipsisPos) :: acc, currentScope)

              case Expr.Symbol(".", dotPos) =>
                (Expr.Symbol(".", dotPos) :: acc, currentScope)

              case Expr.Symbol(name, symbolPos) if bindings.contains(name) =>
                (bindingToExpr(bindings(name), path, symbolPos) :: acc, currentScope)

              case Expr.Symbol(name, symbolPos) =>
                val alias = freshIdentifier("arg", name)
                (Expr.Symbol(alias, symbolPos) :: acc, currentScope.updated(name, alias))

              case other =>
                (instantiate(other, bindings, currentScope, path) :: acc, currentScope)
          }

        (Expr.ListExpr(expanded.reverse, pos), nextScope)

      case other =>
        (instantiate(other, bindings, scope, path), scope)

  private def expandLet(
    head: Expr,
    rest: List[Expr],
    bindings: Map[String, Capture],
    scope: Map[String, String],
    path: List[Int],
    pos: SourcePos
  ): Expr =
    rest match
      case Expr.ListExpr(bindingExprs, bindingPos) :: body if body.nonEmpty =>
        val (expandedBindings, bodyScope) =
          expandLetBindings(bindingExprs, bindings, scope, scope, path)
        val expandedBody = body.map(instantiate(_, bindings, bodyScope, path))
        Expr.ListExpr(head :: Expr.ListExpr(expandedBindings, bindingPos) :: expandedBody, pos)

      case Expr.Symbol(name, namePos) :: Expr.ListExpr(bindingExprs, bindingPos) :: body if body.nonEmpty =>
        val (expandedName, namedScope) =
          if bindings.contains(name) then (bindingToExpr(bindings(name), path, namePos), scope)
          else
            val alias = freshIdentifier("let", name)
            (Expr.Symbol(alias, namePos), scope.updated(name, alias))

        val (expandedBindings, bodyScope) =
          expandLetBindings(bindingExprs, bindings, scope, namedScope, path)
        val expandedBody = body.map(instantiate(_, bindings, bodyScope, path))
        Expr.ListExpr(
          head :: expandedName :: Expr.ListExpr(expandedBindings, bindingPos) :: expandedBody,
          pos
        )

      case _ =>
        Expr.ListExpr(expandCallItems(head :: rest, bindings, scope, path), pos)

  private def expandLetBindings(
    bindingExprs: List[Expr],
    bindings: Map[String, Capture],
    initScope: Map[String, String],
    bodyScopeBase: Map[String, String],
    path: List[Int]
  ): (List[Expr], Map[String, String]) =
    bindingExprs.foldLeft((List.empty[Expr], bodyScopeBase)) { case ((acc, bodyScope), bindingExpr) =>
      bindingExpr match
        case Expr.ListExpr(List(Expr.Symbol(name, namePos), valueExpr), bindingPos) =>
          val (expandedName, nextBodyScope) =
            if bindings.contains(name) then (bindingToExpr(bindings(name), path, namePos), bodyScope)
            else
              val alias = freshIdentifier("bind", name)
              (Expr.Symbol(alias, namePos), bodyScope.updated(name, alias))

          val expandedValue = instantiate(valueExpr, bindings, initScope, path)
          (
            Expr.ListExpr(List(expandedName, expandedValue), bindingPos) :: acc,
            nextBodyScope
          )

        case other =>
          (instantiate(other, bindings, initScope, path) :: acc, bodyScope)
    } match
      case (expanded, bodyScope) =>
        (expanded.reverse, bodyScope)

  private def bindingToExpr(binding: Capture, path: List[Int], pos: SourcePos): Expr =
    selectCapture(binding, path, pos) match
      case Single(expr) => expr
      case Repeated(_) =>
        throw EvalError.at(pos, "template used repeated pattern variable without ellipsis")

  private def repetitionCount(
    template: Expr,
    bindings: Map[String, Capture],
    path: List[Int]
  ): Int =
    val counts = SyntaxPatternMatcher
      .collectTemplateVariables(template, bindings.keySet)
      .toList
      .flatMap { name =>
        selectCaptureOption(bindings(name), path) match
          case Some(Repeated(values)) => Some(values.length)
          case _                      => None
      }

    counts match
      case Nil =>
        throw EvalError.at(template.pos, "template ellipsis has no repeated pattern variable")
      case head :: tail if tail.forall(_ == head) =>
        head
      case _ =>
        throw EvalError.at(template.pos, "template ellipsis has mismatched repetition counts")

  private def selectCapture(binding: Capture, path: List[Int], pos: SourcePos): Capture =
    selectCaptureOption(binding, path).getOrElse {
      throw EvalError.at(pos, "invalid ellipsis nesting in template")
    }

  private def selectCaptureOption(binding: Capture, path: List[Int]): Option[Capture] =
    path.foldLeft(Option(binding)) {
      case (Some(Repeated(values)), index) => values.lift(index)
      case _                               => None
    }

  private def captureValueIdentifier(symbol: Expr.Symbol): Expr.Symbol =
    if SyntaxKeywords.contains(symbol.name) then symbol
    else
      definitionEnv.resolveBinding(symbol.name) match
        case Some(binding) =>
          val alias = valueAliases.get(symbol.name) match
            case Some(existing) => existing
            case None =>
              val created = freshIdentifier("val", symbol.name)
              valueAliases.update(symbol.name, created)
              definitionEnv.bindAlias(created, binding)
              created

          Expr.Symbol(alias, symbol.pos)

        case None =>
          symbol

  private def captureMacroIdentifier(name: String, pos: SourcePos): Option[Expr.Symbol] =
    definitionMacros.resolveBinding(name).map { binding =>
      val alias = macroAliases.get(name) match
        case Some(existing) => existing
        case None =>
          val created = freshIdentifier("mac", name)
          macroAliases.update(name, created)
          definitionMacros.bindAlias(created, binding)
          created

      Expr.Symbol(alias, pos)
    }

  private def freshIdentifier(kind: String, base: String): String =
    s"__syntax_${id}_${kind}_${SyntaxFreshIds.next()}_${SyntaxNames.sanitize(base)}"
