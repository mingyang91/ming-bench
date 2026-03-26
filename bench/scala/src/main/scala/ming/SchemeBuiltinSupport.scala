package ming

import scala.annotation.tailrec
import scala.util.{Failure, Success, Try}

import SchemeModel.*
import SchemeRuntime.*

private[ming] object SchemeBuiltinSupport:

  def requireMinArgCount(name: String, args: List[Value], minimum: Int): Unit =
    if args.length < minimum then
      throw new EvalError(s"$name expected at least $minimum argument(s), got ${args.length}")

  def numericArgs(name: String, args: List[Value]): List[BigInt] =
    args.map {
      case Value.IntegerValue(number) => number
      case other =>
        throw new EvalError(s"$name expected a number, got ${render(other)}")
    }

  def requireInteger(name: String, value: Value): BigInt =
    value match
      case Value.IntegerValue(number) => number
      case other =>
        throw new EvalError(s"$name expected a number, got ${render(other)}")

  def requireString(name: String, value: Value): String =
    value match
      case Value.StringValue(text) => text
      case other =>
        throw new EvalError(s"$name expected a string, got ${render(other)}")

  def requireSymbol(name: String, value: Value): String =
    value match
      case Value.SymbolValue(symbol) => symbol
      case other =>
        throw new EvalError(s"$name expected a symbol, got ${render(other)}")

  def requirePair(name: String, value: Value): Value.PairValue =
    value match
      case pair @ Value.PairValue(_, _) => pair
      case other =>
        throw new EvalError(s"$name expected a pair, got ${render(other)}")

  def requireIndex(
    name: String,
    value: Value,
    upperBound: Int,
    inclusiveUpperBound: Boolean = false
  ): Int =
    val index = requireInteger(name, value)
    if !index.isValidInt then throw new EvalError(s"$name index out of range")

    val intIndex = index.toInt
    val isValid =
      if inclusiveUpperBound then intIndex >= 0 && intIndex <= upperBound
      else intIndex >= 0 && intIndex < upperBound

    if !isValid then throw new EvalError(s"$name index out of range")
    intIndex

  def stringCodePoints(text: String): Array[Int] =
    text.codePoints().toArray

  def properListLength(name: String, value: Value): Int =
    @tailrec
    def loop(current: Value, length: Int): Int =
      current match
        case Value.NilValue =>
          length
        case Value.PairValue(_, cdr) =>
          loop(cdr, length + 1)
        case other =>
          throw new EvalError(s"$name expected a proper list, got ${render(other)}")

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
          throw new EvalError(s"$name expected a proper list, got ${render(other)}")

    loop(value, Nil)

  def numericComparator(name: String)(predicate: (BigInt, BigInt) => Boolean): Value =
    Value.Builtin(
      name,
      args =>
        val numbers = numericArgs(name, args)
        requireMinArgCount(name, args, 2)
        Value.BooleanValue(numbers.zip(numbers.tail).forall(predicate.tupled))
    )

  def predicateBuiltin(name: String)(predicate: Value => Boolean): Value =
    Value.Builtin(
      name,
      args =>
        requireArgCount(name, args, 1)
        Value.BooleanValue(predicate(args.head))
    )

  def unaryStringBuiltin(name: String)(implementation: String => Value): Value =
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

  def buildSubstring(text: String, startArg: Value, endArg: Value): Value =
    val codePoints = stringCodePoints(text)
    val start =
      requireIndex("substring", startArg, codePoints.length, inclusiveUpperBound = true)
    val end =
      requireIndex("substring", endArg, codePoints.length, inclusiveUpperBound = true)
    if start > end then throw new EvalError("substring start index must not exceed end index")
    Value.StringValue(new String(codePoints.slice(start, end), 0, end - start))

  def parseInteger(text: String): Value =
    Try(BigInt(text)) match
      case Success(number) => Value.IntegerValue(number)
      case Failure(_)      => Value.BooleanValue(false)
