package ming

import RuntimeSupport.buildList
import RuntimeSupport.expectProperList

private[ming] object SpecialFormQuoteEvaluator:

  def evalQuote(arguments: List[Expr], position: Position): Value =
    arguments match
      case expression :: Nil =>
        quote(expression)
      case _ =>
        SchemeFailure.raise("quote expected 1 argument(s)", position)

  def evalQuasiquote(
    arguments: List[Expr],
    position: Position,
    env: Environment,
    continuation: Continuation
  ): EvaluationStep =
    arguments match
      case expression :: Nil =>
        evalQuasiquotedValue(expression, env, continuation)
      case _ =>
        SchemeFailure.raise("quasiquote expected 1 argument(s)", position)

  def quote(expression: Expr): Value =
    expression match
      case IntExpr(value, _) =>
        IntValue(value)
      case RationalExpr(numerator, denominator, _) =>
        NumericSupport.exactValue(numerator, denominator)
      case InexactExpr(value, _) =>
        InexactValue(value)
      case BoolExpr(value, _) =>
        BoolValue(value)
      case StringExpr(value, _) =>
        StringValue(value)
      case CharExpr(value, _) =>
        CharValue(value)
      case SymbolExpr(name, _) =>
        SymbolValue(name)
      case ListExpr(items, position) =>
        val decoded = ListExprSupport.decode(items, position, "quote")
        decoded.tail match
          case None =>
            buildList(decoded.items.map(quote))
          case Some(tail) =>
            decoded.items.foldRight[Value](quote(tail)): (item, currentTail) =>
              PairValue(quote(item), currentTail)

  private def evalQuasiquotedValue(
    expression: Expr,
    env: Environment,
    continuation: Continuation
  ): EvaluationStep =
    expression match
      case ListExpr(List(SymbolExpr("unquote", _), unquotedExpression), _) =>
        InterpreterEvaluator.deferExpr(unquotedExpression, env, continuation)
      case ListExpr(items, position) =>
        val decoded = ListExprSupport.decode(items, position, "quasiquote")
        evalQuasiquotedList(decoded.items, decoded.tail, env, position, continuation)
      case _ =>
        InterpreterEvaluator.done(quote(expression), continuation)

  private def evalQuasiquotedList(
    items: List[Expr],
    tail: Option[Expr],
    env: Environment,
    position: Position,
    continuation: Continuation
  ): EvaluationStep =
    evalQuasiquotedItems(
      items,
      env,
      position,
      Nil,
      accumulated =>
        tail match
          case Some(tailExpression) =>
            evalQuasiquotedValue(
              tailExpression,
              env,
              tailValue => InterpreterEvaluator.done(accumulated.foldRight(tailValue)(PairValue(_, _)), continuation)
            )
          case None =>
            InterpreterEvaluator.done(buildList(accumulated), continuation)
    )

  private def evalQuasiquotedItems(
    items: List[Expr],
    env: Environment,
    position: Position,
    reversed: List[Value],
    continuation: List[Value] => EvaluationStep
  ): EvaluationStep =
    items match
      case Nil =>
        continuation(reversed.reverse)
      case ListExpr(List(SymbolExpr("unquote-splicing", _), expression), _) :: rest =>
        InterpreterEvaluator.deferExpr(
          expression,
          env,
          value =>
            val splicedValues = expectProperList(value, "quasiquote", position)
            evalQuasiquotedItems(rest, env, position, splicedValues.reverse ::: reversed, continuation)
        )
      case expression :: rest =>
        evalQuasiquotedValue(
          expression,
          env,
          value => evalQuasiquotedItems(rest, env, position, value :: reversed, continuation)
        )
