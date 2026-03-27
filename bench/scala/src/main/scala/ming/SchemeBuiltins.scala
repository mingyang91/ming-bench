package ming

import scala.annotation.tailrec

private[ming] object Builtins:

  private val builtinNames = Set(
    "+",
    "-",
    "*",
    "/",
    "<",
    ">",
    "=",
    "<=",
    "not",
    "cons",
    "car",
    "cdr",
    "append",
    "list",
    "length",
    "null?",
    "pair?",
    "number?",
    "string?",
    "boolean?",
    "symbol?"
  )

  def resolve(name: String): Option[Value] =
    if builtinNames.contains(name) then Some(Value.BuiltinProc(name))
    else None

  def invoke(name: String, args: List[Value], pos: SourcePos): Value =
    name match
      case "+" =>
        Value.IntVal(evalNumbers(name, args, pos).sum)
      case "-" =>
        val numbers = requireMinArgs(name, evalNumbers(name, args, pos), min = 1, pos)
        numbers match
          case value :: Nil => Value.IntVal(-value)
          case value :: rest =>
            Value.IntVal(rest.foldLeft(value)(_ - _))
          case Nil =>
            throw EvalError.at(pos, s"$name expects at least 1 argument")
      case "*" =>
        Value.IntVal(evalNumbers(name, args, pos).product)
      case "/" =>
        val numbers = requireMinArgs(name, evalNumbers(name, args, pos), min = 2, pos)
        val result = numbers.tail.foldLeft(numbers.head) { (acc, divisor) =>
          if divisor == 0 then throw EvalError.at(pos, "division by zero")
          acc / divisor
        }
        Value.IntVal(result)
      case "<" =>
        compareNumbers(name, args, pos)(_ < _)
      case ">" =>
        compareNumbers(name, args, pos)(_ > _)
      case "=" =>
        compareNumbers(name, args, pos)(_ == _)
      case "<=" =>
        compareNumbers(name, args, pos)(_ <= _)
      case "not" =>
        val values = requireArgCount(name, args, expected = 1, pos)
        Value.BoolVal(!ValueSemantics.isTruthy(values.head))
      case "cons" =>
        val values = requireArgCount(name, args, expected = 2, pos)
        Value.PairVal(values.head, values(1))
      case "car" =>
        val values   = requireArgCount(name, args, expected = 1, pos)
        val (car, _) = requirePair(name, values.head, pos)
        car
      case "cdr" =>
        val values   = requireArgCount(name, args, expected = 1, pos)
        val (_, cdr) = requirePair(name, values.head, pos)
        cdr
      case "append" =>
        append(name, args, pos)
      case "list" =>
        buildList(args)
      case "length" =>
        val values = requireArgCount(name, args, expected = 1, pos)
        Value.IntVal(toProperList(name, values.head, pos).length)
      case "null?" =>
        unaryPredicate(name, args, pos) {
          case Value.EmptyList => true
          case _               => false
        }
      case "pair?" =>
        unaryPredicate(name, args, pos) {
          case Value.PairVal(_, _) => true
          case _                   => false
        }
      case "number?" =>
        unaryPredicate(name, args, pos) {
          case Value.IntVal(_) => true
          case _               => false
        }
      case "string?" =>
        unaryPredicate(name, args, pos) {
          case Value.StringVal(_) => true
          case _                  => false
        }
      case "boolean?" =>
        unaryPredicate(name, args, pos) {
          case Value.BoolVal(_) => true
          case _                => false
        }
      case "symbol?" =>
        unaryPredicate(name, args, pos) {
          case Value.SymbolVal(_) => true
          case _                  => false
        }
      case _ =>
        throw EvalError.at(pos, s"unknown procedure: $name")

  private def compareNumbers(
    name: String,
    args: List[Value],
    pos: SourcePos
  )(predicate: (Int, Int) => Boolean): Value =
    val numbers = requireMinArgs(name, evalNumbers(name, args, pos), min = 2, pos)
    Value.BoolVal(numbers.zip(numbers.tail).forall(predicate.tupled))

  private def evalNumbers(name: String, args: List[Value], pos: SourcePos): List[Int] =
    args.map {
      case Value.IntVal(value) => value
      case other =>
        throw EvalError.at(pos, s"$name expected a number, got ${ValueSemantics.typeName(other)}")
    }

  private def unaryPredicate(name: String, args: List[Value], pos: SourcePos)(predicate: Value => Boolean): Value =
    val values = requireArgCount(name, args, expected = 1, pos)
    Value.BoolVal(predicate(values.head))

  private def requirePair(name: String, value: Value, pos: SourcePos): (Value, Value) =
    value match
      case Value.PairVal(car, cdr) => (car, cdr)
      case other =>
        throw EvalError.at(pos, s"$name expected a pair, got ${ValueSemantics.typeName(other)}")

  private def append(name: String, args: List[Value], pos: SourcePos): Value =
    args match
      case Nil =>
        Value.EmptyList
      case last :: Nil =>
        toProperList(name, last, pos)
        last
      case _ =>
        val last = args.last
        toProperList(name, last, pos)
        args.init.foldRight(last) { (listValue, acc) =>
          toProperList(name, listValue, pos).foldRight(acc) { (item, tail) =>
            Value.PairVal(item, tail)
          }
        }

  private def buildList(values: List[Value]): Value =
    values.foldRight[Value](Value.EmptyList) { (value, acc) =>
      Value.PairVal(value, acc)
    }

  private def toProperList(name: String, value: Value, pos: SourcePos): List[Value] =
    @tailrec
    def loop(current: Value, acc: List[Value]): List[Value] =
      current match
        case Value.EmptyList =>
          acc.reverse
        case Value.PairVal(car, cdr) =>
          loop(cdr, car :: acc)
        case other =>
          throw EvalError.at(pos, s"$name expected a list, got ${ValueSemantics.typeName(other)}")

    loop(value, Nil)

  private def requireArgCount[T](name: String, args: List[T], expected: Int, pos: SourcePos): List[T] =
    if args.lengthCompare(expected) != 0 then
      throw EvalError.at(pos, s"$name expects $expected argument(s), got ${args.length}")
    args

  private def requireMinArgs[T](name: String, args: List[T], min: Int, pos: SourcePos): List[T] =
    if args.lengthCompare(min) < 0 then
      throw EvalError.at(pos, s"$name expects at least $min argument(s), got ${args.length}")
    args
