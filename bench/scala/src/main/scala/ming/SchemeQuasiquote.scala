package ming

private[ming] object SchemeQuasiquote:

  import BuiltinSupport.asList
  import SchemeInterpreter.{Expr, Value}

  def eval(expr: Expr, env: Env, macros: MacroScope, pos: SourcePos): Value =
    evalQuoted(expr, env, macros, depth = 1, pos)

  private def evalQuoted(
    expr: Expr,
    env: Env,
    macros: MacroScope,
    depth: Int,
    pos: SourcePos
  ): Value =
    expr match
      case Expr.ListExpr(List(Expr.Symbol("unquote", _), valueExpr), _) if depth == 1 =>
        SchemeInterpreter.evalExpr(valueExpr, env, macros)

      case Expr.ListExpr(List(Expr.Symbol("unquote", _), valueExpr), _) =>
        Value.list(List(Value.Symbol("unquote"), evalQuoted(valueExpr, env, macros, depth - 1, pos)))

      case Expr.ListExpr(List(Expr.Symbol("unquote-splicing", _), _), splicePos) if depth == 1 =>
        throw EvalError.at(splicePos, "unquote-splicing not valid here")

      case Expr.ListExpr(List(Expr.Symbol("unquote-splicing", _), valueExpr), _) =>
        Value.list(List(Value.Symbol("unquote-splicing"), evalQuoted(valueExpr, env, macros, depth - 1, pos)))

      case Expr.ListExpr(List(Expr.Symbol("quasiquote", _), valueExpr), _) =>
        Value.list(List(Value.Symbol("quasiquote"), evalQuoted(valueExpr, env, macros, depth + 1, pos)))

      case Expr.ListExpr(items, _) =>
        evalList(items, env, macros, depth, pos)

      case Expr.VectorExpr(items, vectorPos) =>
        Value.Vector(evalVectorItems(items, env, macros, depth, vectorPos))

      case _ =>
        SchemeInterpreterSyntax.quote(expr)

  private def evalList(
    items: List[Expr],
    env: Env,
    macros: MacroScope,
    depth: Int,
    pos: SourcePos
  ): Value =
    val (prefix, tail) = SchemeInterpreterSyntax.splitDottedItems(items) match
      case Some((head, tailExpr)) => (head, evalQuoted(tailExpr, env, macros, depth, pos))
      case None                   => (items, Value.EmptyList)

    prefix.foldRight(tail) { (item, acc) =>
      spliceExpr(item) match
        case Some((spliceValueExpr, splicePos)) if depth == 1 =>
          prependSplice(SchemeInterpreter.evalExpr(spliceValueExpr, env, macros), acc, splicePos)
        case _ =>
          Value.Pair(evalQuoted(item, env, macros, depth, pos), acc)
    }

  private def evalVectorItems(
    items: List[Expr],
    env: Env,
    macros: MacroScope,
    depth: Int,
    pos: SourcePos
  ): List[Value] =
    items.flatMap {
      case item if depth == 1 =>
        spliceExpr(item) match
          case Some((spliceValueExpr, splicePos)) =>
            asList(SchemeInterpreter.evalExpr(spliceValueExpr, env, macros), "quasiquote", splicePos)
          case None =>
            List(evalQuoted(item, env, macros, depth, pos))
      case item =>
        List(evalQuoted(item, env, macros, depth, pos))
    }

  private def prependSplice(value: Value, tail: Value, pos: SourcePos): Value =
    asList(value, "quasiquote", pos).foldRight(tail)(Value.Pair(_, _))

  private def spliceExpr(expr: Expr): Option[(Expr, SourcePos)] =
    expr match
      case Expr.ListExpr(List(Expr.Symbol("unquote-splicing", _), valueExpr), splicePos) =>
        Some((valueExpr, splicePos))
      case _ =>
        None
