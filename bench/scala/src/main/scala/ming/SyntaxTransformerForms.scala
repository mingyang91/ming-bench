package ming

import SyntaxTransformerRuntime.*

private[ming] object SyntaxTransformerForms:

  def evalSyntaxCase(
    args: List[Expr],
    context: TransformerContext,
    pos: SourcePos
  ): TransformerValue =
    args match
      case targetExpr :: Expr.ListExpr(literalExprs, _) :: clauses if clauses.nonEmpty =>
        val literalNames = literalExprs.map(parseLiteralIdentifier).toSet
        val targetSyntax = requireSyntax(evalExpr(targetExpr, context), targetExpr.pos, "syntax-case input")

        clauses.iterator
          .map(clause => trySyntaxCaseClause(clause, targetSyntax, literalNames, context))
          .collectFirst { case Some(result) => result }
          .getOrElse(throw EvalError.at(pos, "no matching syntax-case clause"))
      case _ =>
        throw EvalError.at(pos, "invalid syntax-case")

  def evalWithSyntax(
    args: List[Expr],
    context: TransformerContext,
    pos: SourcePos
  ): TransformerValue =
    args match
      case Expr.ListExpr(bindingExprs, _) :: body if body.nonEmpty =>
        val boundContext = bindingExprs.foldLeft(context) { (currentContext, bindingExpr) =>
          bindWithSyntax(bindingExpr, currentContext)
        }
        evalBody(body, boundContext)
      case _ =>
        throw EvalError.at(pos, "invalid with-syntax")

  def evalIf(
    args: List[Expr],
    context: TransformerContext,
    pos: SourcePos
  ): TransformerValue =
    args match
      case conditionExpr :: thenExpr :: Nil =>
        val condition = requireDatum(evalExpr(conditionExpr, context), conditionExpr.pos, "if condition")
        if ValueSemantics.isTruthy(condition) then evalExpr(thenExpr, context)
        else TransformerValue.Datum(Value.Void)
      case conditionExpr :: thenExpr :: elseExpr :: Nil =>
        val condition = requireDatum(evalExpr(conditionExpr, context), conditionExpr.pos, "if condition")
        if ValueSemantics.isTruthy(condition) then evalExpr(thenExpr, context)
        else evalExpr(elseExpr, context)
      case _ =>
        throw EvalError.at(pos, "if expects 2 or 3 arguments")

  def evalLet(
    args: List[Expr],
    context: TransformerContext,
    pos: SourcePos,
    sequential: Boolean
  ): TransformerValue =
    args match
      case Expr.ListExpr(bindingExprs, _) :: body if body.nonEmpty =>
        val scopedContext =
          if sequential then
            bindingExprs.foldLeft(context) { (currentContext, bindingExpr) =>
              val (name, value) = evalLetBinding(bindingExpr, currentContext)
              currentContext.extend(Map(name -> value))
            }
          else
            val evaluatedBindings = bindingExprs.map(bindingExpr => evalLetBinding(bindingExpr, context))
            context.extend(evaluatedBindings.toMap)
        evalBody(body, scopedContext)
      case _ =>
        throw EvalError.at(pos, s"invalid ${if sequential then "let*" else "let"}")

  def evalAnd(args: List[Expr], context: TransformerContext): TransformerValue =
    @annotation.tailrec
    def loop(remaining: List[Expr]): TransformerValue =
      remaining match
        case Nil =>
          TransformerValue.Datum(Value.BoolVal(true))
        case head :: Nil =>
          evalExpr(head, context)
        case head :: tail =>
          val value = requireDatum(evalExpr(head, context), head.pos, "and")
          if ValueSemantics.isTruthy(value) then loop(tail)
          else TransformerValue.Datum(value)

    loop(args)

  def evalOr(args: List[Expr], context: TransformerContext): TransformerValue =
    @annotation.tailrec
    def loop(remaining: List[Expr]): TransformerValue =
      remaining match
        case Nil =>
          TransformerValue.Datum(Value.BoolVal(false))
        case head :: Nil =>
          evalExpr(head, context)
        case head :: tail =>
          val value = requireDatum(evalExpr(head, context), head.pos, "or")
          if ValueSemantics.isTruthy(value) then TransformerValue.Datum(value)
          else loop(tail)

    loop(args)

  def evalSyntaxToDatum(
    args: List[Expr],
    context: TransformerContext,
    pos: SourcePos
  ): TransformerValue =
    args match
      case syntaxExpr :: Nil =>
        val syntax = requireSyntax(evalExpr(syntaxExpr, context), syntaxExpr.pos, "syntax->datum")
        TransformerValue.Datum(ValueSemantics.quote(syntax))
      case _ =>
        throw EvalError.at(pos, "syntax->datum expects exactly 1 argument")

  def evalDatumToSyntax(
    args: List[Expr],
    context: TransformerContext,
    pos: SourcePos
  ): TransformerValue =
    args match
      case baseExpr :: datumExpr :: Nil =>
        val baseSyntax = requireSyntax(evalExpr(baseExpr, context), baseExpr.pos, "datum->syntax")
        val datum      = requireDatum(evalExpr(datumExpr, context), datumExpr.pos, "datum->syntax")
        TransformerValue.Syntax(datumToSyntax(datum, baseSyntax.pos))
      case _ =>
        throw EvalError.at(pos, "datum->syntax expects exactly 2 arguments")

  def evalIdentifierPredicate(
    args: List[Expr],
    context: TransformerContext,
    pos: SourcePos
  ): TransformerValue =
    args match
      case identifierExpr :: Nil =>
        val result =
          evalExpr(identifierExpr, context) match
            case TransformerValue.Syntax(Expr.Symbol(_, _)) => true
            case _                                          => false
        TransformerValue.Datum(Value.BoolVal(result))
      case _ =>
        throw EvalError.at(pos, "identifier? expects exactly 1 argument")

  def evalIdentifierEquality(
    args: List[Expr],
    context: TransformerContext,
    pos: SourcePos
  ): TransformerValue =
    args match
      case leftExpr :: rightExpr :: Nil =>
        val left  = evalExpr(leftExpr, context)
        val right = evalExpr(rightExpr, context)
        val same =
          (left, right) match
            case (
                  TransformerValue.Syntax(Expr.Symbol(leftName, _)),
                  TransformerValue.Syntax(Expr.Symbol(rightName, _))
                ) =>
              leftName == rightName
            case _ =>
              false
        TransformerValue.Datum(Value.BoolVal(same))
      case _ =>
        throw EvalError.at(pos, "identifier equality expects exactly 2 arguments")

  private def trySyntaxCaseClause(
    clause: Expr,
    targetSyntax: Expr,
    literalNames: Set[String],
    context: TransformerContext
  ): Option[TransformerValue] =
    clause match
      case Expr.ListExpr(pattern :: rest, _) if rest.nonEmpty =>
        SyntaxPatternMatcher.matchPattern(pattern, targetSyntax, literalNames).flatMap { bindings =>
          val clauseContext = context.extend(patternBindings(bindings))
          val (fenderOpt, body) =
            if rest.lengthCompare(1) == 0 then (None, rest)
            else (Some(rest.head), rest.tail)

          if body.isEmpty then throw EvalError.at(clause.pos, "syntax-case clause body cannot be empty")

          fenderOpt match
            case Some(fender) =>
              val condition = requireDatum(evalExpr(fender, clauseContext), fender.pos, "syntax-case fender")
              Option.when(ValueSemantics.isTruthy(condition))(evalBody(body, clauseContext))
            case None =>
              Some(evalBody(body, clauseContext))
        }
      case _ =>
        throw EvalError.at(clause.pos, "invalid syntax-case clause")

  private def bindWithSyntax(
    bindingExpr: Expr,
    context: TransformerContext
  ): TransformerContext =
    bindingExpr match
      case Expr.ListExpr(pattern :: valueExpr :: Nil, _) =>
        val syntaxValue = requireSyntax(evalExpr(valueExpr, context), valueExpr.pos, "with-syntax binding")
        val matched =
          SyntaxPatternMatcher
            .matchPattern(pattern, syntaxValue, Set.empty)
            .getOrElse(throw EvalError.at(pattern.pos, "with-syntax pattern did not match"))
        context.extend(patternBindings(matched))
      case _ =>
        throw EvalError.at(bindingExpr.pos, "invalid with-syntax binding")

  private def evalLetBinding(
    bindingExpr: Expr,
    context: TransformerContext
  ): (String, TransformerValue) =
    bindingExpr match
      case Expr.ListExpr(Expr.Symbol(name, _) :: valueExpr :: Nil, _) =>
        name -> evalExpr(valueExpr, context)
      case _ =>
        throw EvalError.at(bindingExpr.pos, "invalid let binding")

  private def parseLiteralIdentifier(expr: Expr): String =
    expr match
      case Expr.Symbol(name, _) =>
        name
      case _ =>
        throw EvalError.at(expr.pos, "syntax-case literals must be symbols")
