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
    "symbol?",
    "char?",
    "display",
    "write",
    "newline"
  ) ++ StringBuiltins.names

  def resolve(name: String): Option[Value] =
    if builtinNames.contains(name) then Some(Value.BuiltinProc(name))
    else None

  def invoke(name: String, args: List[Value], pos: SourcePos, context: EvalContext): Value =
    name match
      case "+" | "-" | "*" | "/" =>
        invokeArithmetic(name, args, pos)
      case "<" | ">" | "=" | "<=" =>
        invokeComparison(name, args, pos)
      case "not" | "cons" | "car" | "cdr" | "append" | "list" | "length" =>
        invokeCoreBuiltin(name, args, pos)
      case "null?" | "pair?" | "number?" | "string?" | "boolean?" | "symbol?" | "char?" =>
        invokePredicateBuiltin(name, args, pos)
      case "display" | "write" | "newline" =>
        invokeOutputBuiltin(name, args, pos, context)
      case builtin if StringBuiltins.handles(builtin) =>
        StringBuiltins.invoke(builtin, args, pos)
      case _ =>
        unknownProcedure(name, pos)

  private def invokeArithmetic(name: String, args: List[Value], pos: SourcePos): Value =
    name match
      case "+" =>
        Value.IntVal(evalNumbers(name, args, pos).sum)
      case "-" =>
        subtract(name, args, pos)
      case "*" =>
        Value.IntVal(evalNumbers(name, args, pos).product)
      case "/" =>
        divide(name, args, pos)
      case _ =>
        unknownProcedure(name, pos)

  private def subtract(name: String, args: List[Value], pos: SourcePos): Value =
    val numbers = requireMinArgs(name, evalNumbers(name, args, pos), min = 1, pos)
    numbers match
      case value :: Nil =>
        Value.IntVal(-value)
      case value :: rest =>
        Value.IntVal(rest.foldLeft(value)(_ - _))
      case Nil =>
        throw EvalError.at(pos, s"$name expects at least 1 argument")

  private def divide(name: String, args: List[Value], pos: SourcePos): Value =
    val numbers = requireMinArgs(name, evalNumbers(name, args, pos), min = 2, pos)
    val result = numbers.tail.foldLeft(numbers.head) { (acc, divisor) =>
      if divisor == 0 then throw EvalError.at(pos, "division by zero")
      acc / divisor
    }
    Value.IntVal(result)

  private def invokeComparison(name: String, args: List[Value], pos: SourcePos): Value =
    name match
      case "<" =>
        compareNumbers(name, args, pos)(_ < _)
      case ">" =>
        compareNumbers(name, args, pos)(_ > _)
      case "=" =>
        compareNumbers(name, args, pos)(_ == _)
      case "<=" =>
        compareNumbers(name, args, pos)(_ <= _)
      case _ =>
        unknownProcedure(name, pos)

  private def invokeCoreBuiltin(name: String, args: List[Value], pos: SourcePos): Value =
    name match
      case "not" =>
        Value.BoolVal(!ValueSemantics.isTruthy(requireSingleArg(name, args, pos)))
      case "cons" =>
        val values = requireArgCount(name, args, expected = 2, pos)
        Value.PairVal(values.head, values(1))
      case "car" =>
        val (car, _) = requirePair(name, requireSingleArg(name, args, pos), pos)
        car
      case "cdr" =>
        val (_, cdr) = requirePair(name, requireSingleArg(name, args, pos), pos)
        cdr
      case "append" =>
        append(name, args, pos)
      case "list" =>
        buildList(args)
      case "length" =>
        val value = requireSingleArg(name, args, pos)
        Value.IntVal(toProperList(name, value, pos).length)
      case _ =>
        unknownProcedure(name, pos)

  private def invokePredicateBuiltin(name: String, args: List[Value], pos: SourcePos): Value =
    name match
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
      case "char?" =>
        unaryPredicate(name, args, pos) {
          case Value.CharVal(_) => true
          case _                => false
        }
      case _ =>
        unknownProcedure(name, pos)

  private def invokeOutputBuiltin(
    name: String,
    args: List[Value],
    pos: SourcePos,
    context: EvalContext
  ): Value =
    name match
      case "display" =>
        context.emit(SchemeRenderer.renderForDisplay(requireSingleArg(name, args, pos)))
        Value.Void
      case "write" =>
        context.emit(SchemeRenderer.render(requireSingleArg(name, args, pos)))
        Value.Void
      case "newline" =>
        requireArgCount(name, args, expected = 0, pos)
        context.emit("\n")
        Value.Void
      case _ =>
        unknownProcedure(name, pos)

  private def requireSingleArg(name: String, args: List[Value], pos: SourcePos): Value =
    requireArgCount(name, args, expected = 1, pos).head

  private def unknownProcedure(name: String, pos: SourcePos): Nothing =
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
