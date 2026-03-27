package ming

private[ming] object ValueSemantics:

  def isTruthy(value: Value): Boolean =
    value match
      case Value.BoolVal(false) => false
      case _                    => true

  def typeName(value: Value): String =
    value match
      case Value.IntVal(_)     => "number"
      case Value.BoolVal(_)    => "boolean"
      case Value.StringVal(_)  => "string"
      case Value.CharVal(_)    => "character"
      case Value.SymbolVal(_)  => "symbol"
      case Value.EmptyList     => "null"
      case Value.PairVal(_, _) => "pair"
      case Value.BuiltinProc(_) | Value.Closure(_, _, _, _) =>
        "procedure"
      case Value.Void => "void"

  def quote(expr: Expr): Value =
    expr match
      case Expr.IntLit(value, _)    => Value.IntVal(value)
      case Expr.BoolLit(value, _)   => Value.BoolVal(value)
      case Expr.StringLit(value, _) => Value.StringVal(MutableString.from(value))
      case Expr.CharLit(value, _)   => Value.CharVal(value)
      case Expr.Symbol(name, _)     => Value.SymbolVal(name)
      case Expr.ListExpr(items, _) =>
        items.foldRight[Value](Value.EmptyList) { (item, acc) =>
          Value.PairVal(quote(item), acc)
        }
