package ming

import java.util.IdentityHashMap

private[ming] object SchemeInterpreterSyntax:

  import SchemeInterpreter.Expr
  import SchemeInterpreter.Value

  def readParams(formals: Expr): LambdaParams =
    formals match
      case Expr.Symbol(name, _) if name != "." =>
        LambdaParams(required = Nil, rest = Some(name))
      case Expr.Symbol(_, _) =>
        throw EvalError.at(formals.pos, s"invalid parameter list: ${renderExpr(formals)}")
      case Expr.ListExpr(params, _) =>
        readParamList(params)
      case other =>
        throw EvalError.at(other.pos, s"invalid parameter list: ${renderExpr(other)}")

  def readParamList(params: List[Expr]): LambdaParams =
    val dotIndices = params.zipWithIndex.collect { case (Expr.Symbol(".", _), index) =>
      index
    }

    dotIndices match
      case Nil =>
        LambdaParams.fixed(params.map(readParamName))
      case index :: Nil if index == params.length - 2 =>
        val required = params.take(index).map(readParamName)
        params(index + 1) match
          case Expr.Symbol(name, _) if name != "." =>
            LambdaParams(required, Some(name))
          case other =>
            throw EvalError.at(other.pos, s"invalid parameter: ${renderExpr(other)}")
      case index :: _ =>
        throw EvalError.at(params(index).pos, "invalid dotted parameter list")

  def readBindings(bindings: List[Expr]): List[(String, Expr)] =
    bindings.map {
      case Expr.ListExpr(List(Expr.Symbol(name, _), valueExpr), _) => (name, valueExpr)
      case other => throw EvalError.at(other.pos, s"invalid binding: ${renderExpr(other)}")
    }

  def splitDottedItems(items: List[Expr]): Option[(List[Expr], Expr)] =
    items match
      case Nil =>
        None
      case _ =>
        val dotIndices = items.zipWithIndex.collect { case (Expr.Symbol(".", _), index) => index }
        dotIndices match
          case Nil =>
            None
          case index :: Nil if index > 0 && index == items.length - 2 =>
            Some((items.take(index), items.last))
          case index :: _ =>
            throw EvalError.at(items(index).pos, "invalid dotted list")

  def quote(expr: Expr): Value =
    expr match
      case Expr.Number(value, _)     => Value.Number(value)
      case Expr.Bool(value, _)       => Value.Bool(value)
      case Expr.StringLit(value, _)  => Value.StringLit(value)
      case Expr.Character(value, _)  => Value.Character(value)
      case Expr.Symbol(name, _)      => Value.Symbol(name)
      case Expr.VectorExpr(items, _) => Value.Vector(items.map(quote))
      case Expr.ListExpr(items, _) =>
        splitDottedItems(items) match
          case Some((prefix, tail)) =>
            prefix.map(quote).foldRight(quote(tail))(Value.Pair(_, _))
          case None =>
            Value.list(items.map(quote))

  def datumToExpr(value: Value, pos: SourcePos, context: String): Expr =
    value match
      case Value.Number(number)      => Expr.Number(number, pos)
      case Value.Bool(boolean)       => Expr.Bool(boolean, pos)
      case Value.StringLit(text)     => Expr.StringLit(text, pos)
      case Value.MutableString(text) => Expr.StringLit(text, pos)
      case Value.Character(char)     => Expr.Character(char, pos)
      case Value.Symbol(name)        => Expr.Symbol(name, pos)
      case Value.EmptyList           => Expr.ListExpr(Nil, pos)
      case vector: Value.Vector =>
        Expr.VectorExpr(vector.toList.map(datumToExpr(_, pos, context)), pos)
      case pair: Value.Pair =>
        pairToExpr(pair, pos, context)
      case other =>
        throw EvalError.at(pos, s"$context expected datum, got ${SchemeInterpreter.render(other)}")

  def isTruthy(value: Value): Boolean =
    value match
      case Value.Bool(false) => false
      case _                 => true

  private def readParamName(param: Expr): String =
    param match
      case Expr.Symbol(name, _) if name != "." => name
      case other =>
        throw EvalError.at(other.pos, s"invalid parameter: ${renderExpr(other)}")

  private def pairToExpr(pair: Value.Pair, pos: SourcePos, context: String): Expr =
    val items          = List.newBuilder[Expr]
    val visited        = new IdentityHashMap[Value.Pair, java.lang.Boolean]()
    var current: Value = pair

    while true do
      current match
        case nextPair: Value.Pair =>
          if visited.containsKey(nextPair) then
            throw EvalError.at(pos, s"$context expected finite datum, got ${SchemeInterpreter.render(pair)}")
          visited.put(nextPair, java.lang.Boolean.TRUE)
          items += datumToExpr(nextPair.car, pos, context)
          current = nextPair.cdr
        case Value.EmptyList =>
          return Expr.ListExpr(items.result(), pos)
        case tail =>
          return Expr.ListExpr(items.result() :+ Expr.Symbol(".", pos) :+ datumToExpr(tail, pos, context), pos)

    throw new IllegalStateException("unreachable")

  private def renderExpr(expr: Expr): String =
    SchemeRendering.renderExpr(expr)
