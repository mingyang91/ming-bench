package ming

import java.util.IdentityHashMap

import scala.annotation.tailrec
import scala.collection.mutable
import scala.util.{Failure, Success, Try}

import SchemeModel.*
import SchemeNumbers.*
import SchemeRuntime.*

private[ming] object SchemeBuiltinSupport:

  private enum ListShape:
    case Proper
    case Improper
    case Circular

  def requireMinArgCount(name: String, args: List[Value], minimum: Int): Unit =
    if args.length < minimum then
      throw new EvalError(s"$name expected at least $minimum argument(s), got ${args.length}")

  def requireNonNegativeIndex(name: String, value: Value): Int =
    val index = requireExactInteger(name, value)
    if !index.isValidInt || index.signum < 0 then throw new EvalError(s"$name index out of range")
    index.toInt

  def requireNumber(name: String, value: Value): Value =
    if isNumber(value) then value
    else throw new EvalError(s"$name expected a number, got ${SchemeRuntime.render(value)}")

  def numericArgs(name: String, args: List[Value]): List[Value] =
    args.map(requireNumber(name, _))

  def requireExactInteger(name: String, value: Value): BigInt =
    value match
      case Value.IntegerValue(number) => number
      case Value.RationalValue(numerator, denominator) if denominator != 0 && numerator % denominator == 0 =>
        numerator / denominator
      case other =>
        throw new EvalError(s"$name expected an exact integer, got ${SchemeRuntime.render(other)}")

  def requireInteger(name: String, value: Value): BigInt =
    integerValue(value) match
      case Some(number) => number
      case None         => throw new EvalError(s"$name expected an integer, got ${SchemeRuntime.render(value)}")

  def requireString(name: String, value: Value): SchemeString =
    value match
      case Value.StringValue(text) => text
      case other =>
        throw new EvalError(s"$name expected a string, got ${SchemeRuntime.render(other)}")

  def requireChar(name: String, value: Value): Int =
    value match
      case Value.CharValue(codePoint) => codePoint
      case other =>
        throw new EvalError(s"$name expected a character, got ${SchemeRuntime.render(other)}")

  def requireSymbol(name: String, value: Value): String =
    value match
      case Value.SymbolValue(symbol) => symbol
      case other =>
        throw new EvalError(s"$name expected a symbol, got ${SchemeRuntime.render(other)}")

  def requirePair(name: String, value: Value): Value.PairValue =
    value match
      case pair @ Value.PairValue(_, _) => pair
      case other =>
        throw new EvalError(s"$name expected a pair, got ${SchemeRuntime.render(other)}")

  def requireVector(name: String, value: Value): mutable.ArrayBuffer[Value] =
    value match
      case Value.VectorValue(elements) => elements
      case other =>
        throw new EvalError(s"$name expected a vector, got ${SchemeRuntime.render(other)}")

  def isProperList(value: Value): Boolean =
    listShape(value) == ListShape.Proper

  def eqValues(left: Value, right: Value): Boolean =
    (left, right) match
      case (Value.IntegerValue(a), Value.IntegerValue(b)) => a == b
      case (Value.RationalValue(aNum, aDen), Value.RationalValue(bNum, bDen)) =>
        aNum == bNum && aDen == bDen
      case (Value.InexactValue(a), Value.InexactValue(b)) => a == b
      case (Value.BooleanValue(a), Value.BooleanValue(b)) => a == b
      case (Value.CharValue(a), Value.CharValue(b))       => a == b
      case (Value.SymbolValue(a), Value.SymbolValue(b))   => a == b
      case (Value.NilValue, Value.NilValue)               => true
      case (Value.VoidValue, Value.VoidValue)             => true
      case (Value.StringValue(a), Value.StringValue(b))   => a eq b
      case _                                              => left.asInstanceOf[AnyRef] eq right.asInstanceOf[AnyRef]

  def eqvValues(left: Value, right: Value): Boolean =
    (left, right) match
      case (leftNumber, rightNumber) if isNumber(leftNumber) && isNumber(rightNumber) =>
        SchemeNumbers.equal(leftNumber, rightNumber)
      case (Value.BooleanValue(a), Value.BooleanValue(b)) => a == b
      case (Value.CharValue(a), Value.CharValue(b))       => a == b
      case (Value.SymbolValue(a), Value.SymbolValue(b))   => a == b
      case (Value.NilValue, Value.NilValue)               => true
      case (Value.VoidValue, Value.VoidValue)             => true
      case (Value.StringValue(a), Value.StringValue(b))   => a eq b
      case _                                              => left.asInstanceOf[AnyRef] eq right.asInstanceOf[AnyRef]

  def equalValues(left: Value, right: Value): Boolean =
    val seen = new IdentityHashMap[AnyRef, IdentityHashMap[AnyRef, java.lang.Boolean]]()

    def alreadySeen(leftRef: AnyRef, rightRef: AnyRef): Boolean =
      var rightRefs = seen.get(leftRef)
      if rightRefs == null then
        rightRefs = new IdentityHashMap[AnyRef, java.lang.Boolean]()
        seen.put(leftRef, rightRefs)
      val wasSeen = rightRefs.containsKey(rightRef)
      if !wasSeen then rightRefs.put(rightRef, java.lang.Boolean.TRUE)
      wasSeen

    def loop(leftValue: Value, rightValue: Value): Boolean =
      (leftValue, rightValue) match
        case (leftNumber, rightNumber) if isNumber(leftNumber) && isNumber(rightNumber) =>
          SchemeNumbers.equal(leftNumber, rightNumber)
        case (Value.BooleanValue(a), Value.BooleanValue(b)) => a == b
        case (Value.StringValue(a), Value.StringValue(b))   => a.text == b.text
        case (Value.CharValue(a), Value.CharValue(b))       => a == b
        case (Value.SymbolValue(a), Value.SymbolValue(b))   => a == b
        case (Value.NilValue, Value.NilValue)               => true
        case (Value.VoidValue, Value.VoidValue)             => true
        case (leftPair: Value.PairValue, rightPair: Value.PairValue) =>
          if alreadySeen(leftPair.asInstanceOf[AnyRef], rightPair.asInstanceOf[AnyRef]) then true
          else loop(leftPair.car, rightPair.car) && loop(leftPair.cdr, rightPair.cdr)
        case (Value.VectorValue(leftElements), Value.VectorValue(rightElements)) =>
          leftElements.length == rightElements.length &&
          (
            if alreadySeen(leftElements.asInstanceOf[AnyRef], rightElements.asInstanceOf[AnyRef]) then true
            else
              leftElements.iterator.zip(rightElements.iterator).forall { case (leftItem, rightItem) =>
                loop(leftItem, rightItem)
              }
          )
        case _ =>
          eqValues(leftValue, rightValue)

    loop(left, right)

  def requireIndex(
    name: String,
    value: Value,
    upperBound: Int,
    inclusiveUpperBound: Boolean = false
  ): Int =
    val index = requireExactInteger(name, value)
    if !index.isValidInt then throw new EvalError(s"$name index out of range")

    val intIndex = index.toInt
    val isValid =
      if inclusiveUpperBound then intIndex >= 0 && intIndex <= upperBound
      else intIndex >= 0 && intIndex < upperBound

    if !isValid then throw new EvalError(s"$name index out of range")
    intIndex

  def stringCodePoints(text: SchemeString): Array[Int] =
    text.codePointsArray

  def properListLength(name: String, value: Value): Int =
    if listShape(value) == ListShape.Circular then
      throw new EvalError(s"$name expected a proper list, got a circular list")

    @tailrec
    def loop(current: Value, length: Int): Int =
      current match
        case Value.NilValue =>
          length
        case Value.PairValue(_, cdr) =>
          loop(cdr, length + 1)
        case other =>
          throw new EvalError(s"$name expected a proper list, got ${SchemeRuntime.render(other)}")

    loop(value, 0)

  def properListElements(name: String, value: Value): List[Value] =
    if listShape(value) == ListShape.Circular then
      throw new EvalError(s"$name expected a proper list, got a circular list")

    @tailrec
    def loop(current: Value, reversedElements: List[Value]): List[Value] =
      current match
        case Value.NilValue =>
          reversedElements.reverse
        case Value.PairValue(car, cdr) =>
          loop(cdr, car :: reversedElements)
        case other =>
          throw new EvalError(s"$name expected a proper list, got ${SchemeRuntime.render(other)}")

    loop(value, Nil)

  def numericComparator(name: String)(predicate: Int => Boolean): Value =
    Value.Builtin(
      name,
      args =>
        val numbers = numericArgs(name, args)
        requireMinArgCount(name, args, 2)
        Value.BooleanValue(numbers.zip(numbers.tail).forall { case (left, right) =>
          predicate(SchemeNumbers.compare(left, right))
        })
    )

  def predicateBuiltin(name: String)(predicate: Value => Boolean): Value =
    Value.Builtin(
      name,
      args =>
        requireArgCount(name, args, 1)
        Value.BooleanValue(predicate(args.head))
    )

  def unaryStringBuiltin(name: String)(implementation: SchemeString => Value): Value =
    Value.Builtin(
      name,
      args =>
        requireArgCount(name, args, 1)
        implementation(requireString(name, args.head))
    )

  def appendValues(args: List[Value]): Value =
    args.reverse match
      case Nil =>
        Value.NilValue
      case last :: reversedPrefixes =>
        reversedPrefixes.foldLeft(last) { (result, listValue) =>
          properListElements("append", listValue).reverse.foldLeft(result) { (cdr, item) =>
            Value.PairValue(item, cdr)
          }
        }

  def buildSubstring(text: SchemeString, startArg: Value, endArg: Value): Value =
    val codePoints = stringCodePoints(text)
    val start =
      requireIndex("substring", startArg, codePoints.length, inclusiveUpperBound = true)
    val end =
      requireIndex("substring", endArg, codePoints.length, inclusiveUpperBound = true)
    if start > end then throw new EvalError("substring start index must not exceed end index")
    Value.StringValue(text.slice(start, end))

  def parseNumber(text: SchemeString): Value =
    SchemeNumbers.parseStringNumber(text.text) match
      case Some(number) => number
      case None         => Value.BooleanValue(false)

  private def listShape(value: Value): ListShape =
    var slow: Value = value
    var fast: Value = value

    while true do
      fast match
        case Value.NilValue =>
          return ListShape.Proper
        case pair: Value.PairValue =>
          fast = pair.cdr
        case _ =>
          return ListShape.Improper

      fast match
        case Value.NilValue =>
          return ListShape.Proper
        case pair: Value.PairValue =>
          fast = pair.cdr
        case _ =>
          return ListShape.Improper

      slow match
        case pair: Value.PairValue =>
          slow = pair.cdr
        case _ =>
          return ListShape.Improper

      if slow.asInstanceOf[AnyRef] eq fast.asInstanceOf[AnyRef] then return ListShape.Circular

    ListShape.Improper
