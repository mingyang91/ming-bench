package ming

private[ming] object EqualityBuiltins:

  import BuiltinSupport.*
  import SchemeInterpreter.Value

  def all: List[Value.Builtin] =
    List(eqBuiltin, eqvBuiltin, equalBuiltin)

  def eqvValues(left: Value, right: Value): Boolean =
    (left, right) match
      case (Value.Number(a), Value.Number(b))       => SchemeNumber.areEqual(a, b)
      case (Value.Bool(a), Value.Bool(b))           => a == b
      case (Value.Character(a), Value.Character(b)) => a == b
      case (Value.Symbol(a), Value.Symbol(b))       => a == b
      case (Value.StringLit(a), Value.StringLit(b)) => a == b
      case (Value.EmptyList, Value.EmptyList)       => true
      case (leftPair: Value.Pair, rightPair: Value.Pair) =>
        leftPair eq rightPair
      case (leftVector: Value.Vector, rightVector: Value.Vector) =>
        leftVector eq rightVector
      case (leftRecord: Value.Record, rightRecord: Value.Record) =>
        leftRecord eq rightRecord
      case (leftBuiltin: Value.Builtin, rightBuiltin: Value.Builtin) =>
        leftBuiltin eq rightBuiltin
      case (leftClosure: Value.Closure, rightClosure: Value.Closure) =>
        leftClosure eq rightClosure
      case (leftCaseLambda: Value.CaseLambda, rightCaseLambda: Value.CaseLambda) =>
        leftCaseLambda eq rightCaseLambda
      case (Value.Void, Value.Void) => true
      case _                        => false

  def equalValues(left: Value, right: Value): Boolean =
    (left, right) match
      case (Value.Number(a), Value.Number(b))               => SchemeNumber.areEqual(a, b)
      case (Value.Bool(a), Value.Bool(b))                   => a == b
      case (Value.Character(a), Value.Character(b))         => a == b
      case (Value.Symbol(a), Value.Symbol(b))               => a == b
      case (Value.StringLit(a), Value.StringLit(b))         => a == b
      case (Value.StringLit(a), Value.MutableString(b))     => a == b
      case (Value.MutableString(a), Value.StringLit(b))     => a == b
      case (Value.MutableString(a), Value.MutableString(b)) => a == b
      case (Value.EmptyList, Value.EmptyList)               => true
      case (Value.Pair(leftCar, leftCdr), Value.Pair(rightCar, rightCdr)) =>
        equalValues(leftCar, rightCar) && equalValues(leftCdr, rightCdr)
      case (leftVector: Value.Vector, rightVector: Value.Vector) =>
        leftVector.length == rightVector.length &&
        leftVector.toList.zip(rightVector.toList).forall { case (leftValue, rightValue) =>
          equalValues(leftValue, rightValue)
        }
      case (leftRecord: Value.Record, rightRecord: Value.Record) =>
        leftRecord eq rightRecord
      case (leftBuiltin: Value.Builtin, rightBuiltin: Value.Builtin) =>
        leftBuiltin eq rightBuiltin
      case (leftClosure: Value.Closure, rightClosure: Value.Closure) =>
        leftClosure eq rightClosure
      case (leftCaseLambda: Value.CaseLambda, rightCaseLambda: Value.CaseLambda) =>
        leftCaseLambda eq rightCaseLambda
      case (Value.Void, Value.Void) => true
      case _                        => false

  private val eqBuiltin: Value.Builtin =
    Value.Builtin(
      "eq?",
      (args, pos) =>
        val (left, right) = twoArgs("eq?", args, pos)
        Value.Bool(eqvValues(left, right))
    )

  private val eqvBuiltin: Value.Builtin =
    Value.Builtin(
      "eqv?",
      (args, pos) =>
        val (left, right) = twoArgs("eqv?", args, pos)
        Value.Bool(eqvValues(left, right))
    )

  private val equalBuiltin: Value.Builtin =
    Value.Builtin(
      "equal?",
      (args, pos) =>
        val (left, right) = twoArgs("equal?", args, pos)
        Value.Bool(equalValues(left, right))
    )
