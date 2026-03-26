package ming

import scala.annotation.tailrec
import scala.util.{Failure, Success, Try}

import SchemeModel.*
import SchemeNumbers.*
import SchemeRuntime.*

private[ming] object SchemeBuiltinSupport:

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

  def isProperList(value: Value): Boolean =
    @tailrec
    def loop(current: Value): Boolean =
      current match
        case Value.NilValue          => true
        case Value.PairValue(_, cdr) => loop(cdr)
        case _                       => false

    loop(value)

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

  def equalValues(left: Value, right: Value): Boolean =
    (left, right) match
      case (leftNumber, rightNumber) if isNumber(leftNumber) && isNumber(rightNumber) =>
        SchemeNumbers.equal(leftNumber, rightNumber)
      case (Value.BooleanValue(a), Value.BooleanValue(b)) => a == b
      case (Value.StringValue(a), Value.StringValue(b))   => a.text == b.text
      case (Value.CharValue(a), Value.CharValue(b))       => a == b
      case (Value.SymbolValue(a), Value.SymbolValue(b))   => a == b
      case (Value.NilValue, Value.NilValue)               => true
      case (Value.VoidValue, Value.VoidValue)             => true
      case (Value.PairValue(leftCar, leftCdr), Value.PairValue(rightCar, rightCdr)) =>
        equalValues(leftCar, rightCar) && equalValues(leftCdr, rightCdr)
      case _ =>
        eqValues(left, right)

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
