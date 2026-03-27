package ming

import RuntimeSupport.buildList

private[ming] object SpecialFormQuoteEvaluator:

  def evalQuote(arguments: List[Expr], position: Position): Value =
    arguments match
      case expression :: Nil =>
        quote(expression)
      case _ =>
        SchemeFailure.raise("quote expected 1 argument(s)", position)

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
      case ListExpr(items, _) =>
        buildList(items.map(quote))
