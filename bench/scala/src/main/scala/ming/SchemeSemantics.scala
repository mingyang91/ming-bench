package ming

import scala.collection.mutable

private[ming] object ValueSemantics:

  def isTruthy(value: Value): Boolean =
    value match
      case Value.BoolVal(false) => false
      case _                    => true

  def isProcedure(value: Value): Boolean =
    value match
      case Value.BuiltinProc(_) | Value.RecordConstructor(_) | Value.RecordPredicate(_) |
          Value.RecordAccessor(_, _, _) | Value.CaseClosure(_, _, _) | Value.Closure(_, _, _, _, _) =>
        true
      case _ =>
        false

  def typeName(value: Value): String =
    value match
      case Value.IntVal(_) | Value.RationalVal(_, _) | Value.InexactVal(_) =>
        "number"
      case Value.BoolVal(_)   => "boolean"
      case Value.StringVal(_) => "string"
      case Value.CharVal(_)   => "character"
      case Value.SymbolVal(_) => "symbol"
      case Value.EmptyList    => "null"
      case _: Value.PairVal   => "pair"
      case Value.VectorVal(_) => "vector"
      case Value.BuiltinProc(_) | Value.RecordConstructor(_) | Value.RecordPredicate(_) |
          Value.RecordAccessor(_, _, _) | Value.CaseClosure(_, _, _) | Value.Closure(_, _, _, _, _) =>
        "procedure"
      case Value.RecordVal(_) => "record"
      case Value.Void         => "void"

  def isProperList(value: Value): Boolean =
    val seen = mutable.HashSet.empty[Value.PairVal]

    @annotation.tailrec
    def loop(current: Value): Boolean =
      current match
        case Value.EmptyList =>
          true
        case pair: Value.PairVal =>
          if seen.contains(pair) then false
          else
            seen += pair
            loop(pair.cdr)
        case _ =>
          false

    loop(value)

  def isEq(value1: Value, value2: Value): Boolean =
    (SchemeNumber.fromValue(value1), SchemeNumber.fromValue(value2)) match
      case (Some(left), Some(right)) =>
        SchemeNumber.areEqual(left, right)
      case _ =>
        (value1, value2) match
          case (Value.BoolVal(left), Value.BoolVal(right))     => left == right
          case (Value.CharVal(left), Value.CharVal(right))     => left == right
          case (Value.SymbolVal(left), Value.SymbolVal(right)) => left == right
          case (Value.EmptyList, Value.EmptyList)              => true
          case (Value.BuiltinProc(left), Value.BuiltinProc(right)) =>
            left == right
          case (Value.Void, Value.Void) => true
          case (Value.StringVal(left), Value.StringVal(right)) =>
            left eq right
          case (Value.VectorVal(left), Value.VectorVal(right)) =>
            left eq right
          case (
                leftProc @ Value.RecordConstructor(_),
                rightProc @ Value.RecordConstructor(_)
              ) =>
            leftProc eq rightProc
          case (
                leftProc @ Value.RecordPredicate(_),
                rightProc @ Value.RecordPredicate(_)
              ) =>
            leftProc eq rightProc
          case (
                leftProc @ Value.RecordAccessor(_, _, _),
                rightProc @ Value.RecordAccessor(_, _, _)
              ) =>
            leftProc eq rightProc
          case (leftPair @ Value.PairVal(_, _), rightPair @ Value.PairVal(_, _)) =>
            leftPair eq rightPair
          case (Value.RecordVal(left), Value.RecordVal(right)) =>
            left eq right
          case (
                leftClosure @ Value.Closure(_, _, _, _, _),
                rightClosure @ Value.Closure(_, _, _, _, _)
              ) =>
            leftClosure eq rightClosure
          case (
                leftClosure @ Value.CaseClosure(_, _, _),
                rightClosure @ Value.CaseClosure(_, _, _)
              ) =>
            leftClosure eq rightClosure
          case _ =>
            false

  def isEqv(value1: Value, value2: Value): Boolean =
    isEq(value1, value2)

  def isEqual(value1: Value, value2: Value): Boolean =
    val seen = mutable.HashSet.empty[(AnyRef, AnyRef)]

    def loop(left: Value, right: Value): Boolean =
      (left, right) match
        case (Value.StringVal(leftText), Value.StringVal(rightText)) =>
          leftText.text == rightText.text
        case (leftPair: Value.PairVal, rightPair: Value.PairVal) =>
          val key = (leftPair: AnyRef, rightPair: AnyRef)
          if seen.contains(key) then true
          else
            seen += key
            loop(leftPair.car, rightPair.car) && loop(leftPair.cdr, rightPair.cdr)
        case (Value.VectorVal(leftVector), Value.VectorVal(rightVector)) =>
          if leftVector.length != rightVector.length then false
          else
            val key = (leftVector: AnyRef, rightVector: AnyRef)
            if seen.contains(key) then true
            else
              seen += key
              leftVector.elements.zip(rightVector.elements).forall(loop.tupled)
        case _ =>
          isEq(left, right)

    loop(value1, value2)

  def quote(expr: Expr): Value =
    expr match
      case Expr.IntLit(value, _) => Value.IntVal(value)
      case Expr.RationalLit(numerator, denominator, _) =>
        Value.RationalVal(numerator, denominator)
      case Expr.InexactLit(value, _) => Value.InexactVal(value)
      case Expr.BoolLit(value, _)    => Value.BoolVal(value)
      case Expr.StringLit(value, _) =>
        Value.StringVal(MutableString.immutable(value))
      case Expr.CharLit(value, _) => Value.CharVal(value)
      case Expr.Symbol(name, _)   => Value.SymbolVal(name)
      case Expr.ListExpr(items, _) =>
        items.foldRight[Value](Value.EmptyList) { (item, acc) =>
          Value.PairVal(quote(item), acc)
        }

  def listFrom(values: List[Value]): Value =
    values.foldRight[Value](Value.EmptyList) { (value, acc) =>
      Value.PairVal(value, acc)
    }

  def toProperList(name: String, value: Value, pos: SourcePos): List[Value] =
    val seen = mutable.HashSet.empty[Value.PairVal]

    @annotation.tailrec
    def loop(current: Value, acc: List[Value]): List[Value] =
      current match
        case Value.EmptyList =>
          acc.reverse
        case pair: Value.PairVal =>
          if seen.contains(pair) then throw EvalError.at(pos, s"$name expected a proper list, got circular list")
          seen += pair
          loop(pair.cdr, pair.car :: acc)
        case other =>
          throw EvalError.at(pos, s"$name expected a list, got ${typeName(other)}")

    loop(value, Nil)
