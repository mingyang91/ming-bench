package ming

import java.util.IdentityHashMap

private[ming] object EqualityBuiltins:

  import BuiltinSupport.*
  import SchemeInterpreter.{Procedure, Value}

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
      case (leftProcedure: Procedure, rightProcedure: Procedure) =>
        leftProcedure eq rightProcedure
      case (Value.Void, Value.Void) => true
      case _                        => false

  def equalValues(left: Value, right: Value): Boolean =
    equalValues(left, right, SeenComparisons())

  private def equalValues(left: Value, right: Value, seen: SeenComparisons): Boolean =
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
      case (leftPair: Value.Pair, rightPair: Value.Pair) =>
        if leftPair eq rightPair then true
        else if seen.contains(leftPair, rightPair) then true
        else
          seen.add(leftPair, rightPair)
          equalValues(leftPair.car, rightPair.car, seen) &&
          equalValues(leftPair.cdr, rightPair.cdr, seen)
      case (leftVector: Value.Vector, rightVector: Value.Vector) =>
        if leftVector eq rightVector then true
        else if leftVector.length != rightVector.length then false
        else if seen.contains(leftVector, rightVector) then true
        else
          seen.add(leftVector, rightVector)
          leftVector.toList.zip(rightVector.toList).forall { case (leftValue, rightValue) =>
            equalValues(leftValue, rightValue, seen)
          }
      case (leftRecord: Value.Record, rightRecord: Value.Record) =>
        leftRecord eq rightRecord
      case (leftProcedure: Procedure, rightProcedure: Procedure) =>
        leftProcedure eq rightProcedure
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

  final private class SeenComparisons private (
    private val seen: IdentityHashMap[AnyRef, IdentityHashMap[AnyRef, java.lang.Boolean]]
  ):

    def contains(left: AnyRef, right: AnyRef): Boolean =
      val rights = seen.get(left)
      rights != null && rights.containsKey(right)

    def add(left: AnyRef, right: AnyRef): Unit =
      val rights =
        Option(seen.get(left)).getOrElse {
          val created = new IdentityHashMap[AnyRef, java.lang.Boolean]()
          seen.put(left, created)
          created
        }
      rights.put(right, java.lang.Boolean.TRUE)

  private object SeenComparisons:

    def apply(): SeenComparisons =
      new SeenComparisons(new IdentityHashMap[AnyRef, IdentityHashMap[AnyRef, java.lang.Boolean]]())
