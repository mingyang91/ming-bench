package ming

import scala.annotation.tailrec

private[ming] trait BuiltinSupport:

  final protected def asNumbers(args: List[Value], pos: SourcePos, name: String): List[BigInt] =
    args.map(arg => expectNumber(arg, pos, name))

  final protected def expectSingleArg(args: List[Value], pos: SourcePos, name: String): Value =
    args match
      case value :: Nil =>
        value

      case _ =>
        throw EvalError.at(pos, s"$name expects exactly 1 argument")

  final protected def expectNumber(arg: Value, pos: SourcePos, name: String): BigInt =
    arg match
      case Value.IntVal(value) =>
        value

      case other =>
        throw EvalError.at(pos, s"$name expected number arguments, got ${other.typeName}")

  final protected def expectString(arg: Value, pos: SourcePos, name: String): String =
    new String(expectStringValue(arg, pos, name))

  final protected def expectStringValue(arg: Value, pos: SourcePos, name: String): Array[Char] =
    arg match
      case Value.StringVal(value) =>
        value

      case other =>
        throw EvalError.at(pos, s"$name expected string arguments, got ${other.typeName}")

  final protected def expectSymbol(arg: Value, pos: SourcePos, name: String): String =
    arg match
      case Value.SymbolVal(value) =>
        value

      case other =>
        throw EvalError.at(pos, s"$name expected a symbol, got ${other.typeName}")

  final protected def expectChar(arg: Value, pos: SourcePos, name: String): Char =
    arg match
      case Value.CharVal(value) =>
        value

      case other =>
        throw EvalError.at(pos, s"$name expected a char, got ${other.typeName}")

  final protected def expectIndex(arg: Value, pos: SourcePos, name: String): Int =
    val value = expectNumber(arg, pos, name)
    if value < 0 || !value.isValidInt then throw EvalError.at(pos, s"$name expected a non-negative integer index")

    value.toInt

  final protected def expectList(arg: Value, pos: SourcePos, name: String): Value =
    asProperList(arg, pos, name)
    arg

  final protected def asProperList(arg: Value, pos: SourcePos, name: String): List[Value] =
    @tailrec
    def loop(current: Value, acc: List[Value]): List[Value] =
      current match
        case Value.EmptyList =>
          acc.reverse

        case Value.PairVal(carValue, cdrValue) =>
          loop(cdrValue, carValue :: acc)

        case other =>
          throw EvalError.at(pos, s"$name expected a proper list, got ${other.typeName}")

    loop(arg, Nil)
