package ming

private[ming] object Builtins:

  private val builtinNames = Set("+", "-", "*", "/", "<", ">", "=", "<=", "not")

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

  private def requireArgCount[T](name: String, args: List[T], expected: Int): List[T] =
    if args.lengthCompare(expected) != 0 then
      throw EvalError(s"$name expects $expected argument(s), got ${args.length}")
    args

  private def requireMinArgs[T](name: String, args: List[T], min: Int): List[T] =
    if args.lengthCompare(min) < 0 then throw EvalError(s"$name expects at least $min argument(s), got ${args.length}")
    args
