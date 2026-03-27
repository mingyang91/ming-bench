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
      case Value.BuiltinProc(_) | Value.Closure(_, _, _, _, _) =>
        "procedure"
      case Value.Void => "void"

  def isProperList(value: Value): Boolean =
    @annotation.tailrec
    def loop(current: Value): Boolean =
      current match
        case Value.EmptyList       => true
        case Value.PairVal(_, cdr) => loop(cdr)
        case _                     => false

    loop(value)

  def isEq(value1: Value, value2: Value): Boolean =
    (value1, value2) match
      case (Value.IntVal(left), Value.IntVal(right))       => left == right
      case (Value.BoolVal(left), Value.BoolVal(right))     => left == right
      case (Value.CharVal(left), Value.CharVal(right))     => left == right
      case (Value.SymbolVal(left), Value.SymbolVal(right)) => left == right
      case (Value.EmptyList, Value.EmptyList)              => true
      case (Value.BuiltinProc(left), Value.BuiltinProc(right)) =>
        left == right
      case (Value.Void, Value.Void) => true
      case (Value.StringVal(left), Value.StringVal(right)) =>
        left eq right
      case (leftPair @ Value.PairVal(_, _), rightPair @ Value.PairVal(_, _)) =>
        leftPair eq rightPair
      case (leftClosure @ Value.Closure(_, _, _, _, _), rightClosure @ Value.Closure(_, _, _, _, _)) =>
        leftClosure eq rightClosure
      case _ =>
        false

  def isEqual(value1: Value, value2: Value): Boolean =
    (value1, value2) match
      case (Value.StringVal(left), Value.StringVal(right)) =>
        left.text == right.text
      case (Value.PairVal(leftCar, leftCdr), Value.PairVal(rightCar, rightCdr)) =>
        isEqual(leftCar, rightCar) && isEqual(leftCdr, rightCdr)
      case _ =>
        isEq(value1, value2)

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

  def listFrom(values: List[Value]): Value =
    values.foldRight[Value](Value.EmptyList) { (value, acc) =>
      Value.PairVal(value, acc)
    }

  def toProperList(name: String, value: Value, pos: SourcePos): List[Value] =
    @annotation.tailrec
    def loop(current: Value, acc: List[Value]): List[Value] =
      current match
        case Value.EmptyList =>
          acc.reverse
        case Value.PairVal(car, cdr) =>
          loop(cdr, car :: acc)
        case other =>
          throw EvalError.at(pos, s"$name expected a list, got ${typeName(other)}")

    loop(value, Nil)
