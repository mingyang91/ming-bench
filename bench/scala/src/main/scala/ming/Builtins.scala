package ming

private[ming] object Builtins:

  val globalEnv: Map[String, Value] = Map(
    "+"   -> Value.Builtin("+", add),
    "-"   -> Value.Builtin("-", subtract),
    "*"   -> Value.Builtin("*", multiply),
    "/"   -> Value.Builtin("/", divide),
    "<"   -> comparison("<", _ < _),
    ">"   -> comparison(">", _ > _),
    "="   -> comparison("=", _ == _),
    "<="  -> comparison("<=", _ <= _),
    "not" -> Value.Builtin("not", negate)
  )

  private def add(args: List[Value], pos: SourcePos): Value =
    Value.IntVal(asNumbers(args, pos, "+").foldLeft(BigInt(0))(_ + _))

  private def subtract(args: List[Value], pos: SourcePos): Value =
    val numbers = asNumbers(args, pos, "-")
    val result =
      numbers match
        case Nil =>
          throw EvalError.at(pos, "- expects at least 1 argument")

        case value :: Nil =>
          -value

        case value :: rest =>
          rest.foldLeft(value)(_ - _)

    Value.IntVal(result)

  private def multiply(args: List[Value], pos: SourcePos): Value =
    Value.IntVal(asNumbers(args, pos, "*").foldLeft(BigInt(1))(_ * _))

  private def divide(args: List[Value], pos: SourcePos): Value =
    val numbers = asNumbers(args, pos, "/")
    val result =
      numbers match
        case first :: second :: rest =>
          (second :: rest).foldLeft(first) { (left, right) =>
            if right == 0 then throw EvalError.at(pos, "division by zero")

            val (quotient, remainder) = left /% right
            if remainder != 0 then throw EvalError.at(pos, "/ expects an integer result")

            quotient
          }

        case _ =>
          throw EvalError.at(pos, "/ expects at least 2 arguments")

    Value.IntVal(result)

  private def negate(args: List[Value], pos: SourcePos): Value =
    args match
      case value :: Nil =>
        Value.BoolVal(!Value.isTruthy(value))

      case _ =>
        throw EvalError.at(pos, "not expects exactly 1 argument")

  private def comparison(name: String, relation: (BigInt, BigInt) => Boolean): Value =
    Value.Builtin(
      name,
      (args, pos) =>
        val numbers = asNumbers(args, pos, name)
        if numbers.lengthCompare(2) < 0 then throw EvalError.at(pos, s"$name expects at least 2 arguments")

        Value.BoolVal(numbers.zip(numbers.tail).forall(relation.tupled))
    )

  private def asNumbers(args: List[Value], pos: SourcePos, name: String): List[BigInt] =
    args.map(arg => expectNumber(arg, pos, name))

  private def expectNumber(arg: Value, pos: SourcePos, name: String): BigInt =
    arg match
      case Value.IntVal(value) =>
        value

      case other =>
        throw EvalError.at(pos, s"$name expected number arguments, got ${other.typeName}")
