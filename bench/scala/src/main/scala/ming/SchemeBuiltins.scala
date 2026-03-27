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

  def invoke(name: String, args: List[Value]): Value =
    name match
      case "+" =>
        Value.IntVal(evalNumbers(name, args).sum)
      case "-" =>
        val numbers = requireMinArgs(name, evalNumbers(name, args), min = 1)
        numbers match
          case value :: Nil => Value.IntVal(-value)
          case value :: rest =>
            Value.IntVal(rest.foldLeft(value)(_ - _))
          case Nil =>
            throw EvalError(s"$name expects at least 1 argument")
      case "*" =>
        Value.IntVal(evalNumbers(name, args).product)
      case "/" =>
        val numbers = requireMinArgs(name, evalNumbers(name, args), min = 2)
        val result = numbers.tail.foldLeft(numbers.head) { (acc, divisor) =>
          if divisor == 0 then throw EvalError("division by zero")
          acc / divisor
        }
        Value.IntVal(result)
      case "<" =>
        compareNumbers(name, args)(_ < _)
      case ">" =>
        compareNumbers(name, args)(_ > _)
      case "=" =>
        compareNumbers(name, args)(_ == _)
      case "<=" =>
        compareNumbers(name, args)(_ <= _)
      case "not" =>
        val values = requireArgCount(name, args, expected = 1)
        Value.BoolVal(!ValueSemantics.isTruthy(values.head))
      case "cons" =>
        val values = requireArgCount(name, args, expected = 2)
        Value.PairVal(values.head, values(1))
      case "car" =>
        val values   = requireArgCount(name, args, expected = 1)
        val (car, _) = requirePair(name, values.head)
        car
      case "cdr" =>
        val values   = requireArgCount(name, args, expected = 1)
        val (_, cdr) = requirePair(name, values.head)
        cdr
      case "append" =>
        append(name, args)
      case "list" =>
        buildList(args)
      case "length" =>
        val values = requireArgCount(name, args, expected = 1)
        Value.IntVal(toProperList(name, values.head).length)
      case "null?" =>
        unaryPredicate(name, args) {
          case Value.EmptyList => true
          case _               => false
        }
      case "pair?" =>
        unaryPredicate(name, args) {
          case Value.PairVal(_, _) => true
          case _                   => false
        }
      case "number?" =>
        unaryPredicate(name, args) {
          case Value.IntVal(_) => true
          case _               => false
        }
      case "string?" =>
        unaryPredicate(name, args) {
          case Value.StringVal(_) => true
          case _                  => false
        }
      case "boolean?" =>
        unaryPredicate(name, args) {
          case Value.BoolVal(_) => true
          case _                => false
        }
      case "symbol?" =>
        unaryPredicate(name, args) {
          case Value.SymbolVal(_) => true
          case _                  => false
        }
      case _ =>
        throw EvalError(s"unknown procedure: $name")

  private def compareNumbers(
    name: String,
    args: List[Value]
  )(predicate: (Int, Int) => Boolean): Value =
    val numbers = requireMinArgs(name, evalNumbers(name, args), min = 2)
    Value.BoolVal(numbers.zip(numbers.tail).forall(predicate.tupled))

  private def evalNumbers(name: String, args: List[Value]): List[Int] =
    args.map {
      case Value.IntVal(value) => value
      case other               => throw EvalError(s"$name expected a number, got ${ValueSemantics.typeName(other)}")
    }

  private def unaryPredicate(name: String, args: List[Value])(predicate: Value => Boolean): Value =
    val values = requireArgCount(name, args, expected = 1)
    Value.BoolVal(predicate(values.head))

  private def requirePair(name: String, value: Value): (Value, Value) =
    value match
      case Value.PairVal(car, cdr) => (car, cdr)
      case other                   => throw EvalError(s"$name expected a pair, got ${ValueSemantics.typeName(other)}")

  private def append(name: String, args: List[Value]): Value =
    args match
      case Nil =>
        Value.EmptyList
      case last :: Nil =>
        toProperList(name, last)
        last
      case _ =>
        val last = args.last
        toProperList(name, last)
        args.init.foldRight(last) { (listValue, acc) =>
          toProperList(name, listValue).foldRight(acc) { (item, tail) =>
            Value.PairVal(item, tail)
          }
        }

  private def buildList(values: List[Value]): Value =
    values.foldRight[Value](Value.EmptyList) { (value, acc) =>
      Value.PairVal(value, acc)
    }

  private def toProperList(name: String, value: Value): List[Value] =
    @tailrec
    def loop(current: Value, acc: List[Value]): List[Value] =
      current match
        case Value.EmptyList =>
          acc.reverse
        case Value.PairVal(car, cdr) =>
          loop(cdr, car :: acc)
        case other =>
          throw EvalError(s"$name expected a list, got ${ValueSemantics.typeName(other)}")

    loop(value, Nil)

  private def requireArgCount[T](name: String, args: List[T], expected: Int): List[T] =
    if args.lengthCompare(expected) != 0 then
      throw EvalError(s"$name expects $expected argument(s), got ${args.length}")
    args

  private def requireMinArgs[T](name: String, args: List[T], min: Int): List[T] =
    if args.lengthCompare(min) < 0 then throw EvalError(s"$name expects at least $min argument(s), got ${args.length}")
    args
