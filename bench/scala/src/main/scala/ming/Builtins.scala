package ming

import scala.annotation.tailrec

private[ming] object Builtins:

  val globalEnv: Map[String, Value] = Map(
    "+"        -> Value.Builtin("+", add),
    "-"        -> Value.Builtin("-", subtract),
    "*"        -> Value.Builtin("*", multiply),
    "/"        -> Value.Builtin("/", divide),
    "<"        -> comparison("<", _ < _),
    ">"        -> comparison(">", _ > _),
    "="        -> comparison("=", _ == _),
    "<="       -> comparison("<=", _ <= _),
    "not"      -> Value.Builtin("not", negate),
    "cons"     -> Value.Builtin("cons", cons),
    "car"      -> Value.Builtin("car", car),
    "cdr"      -> Value.Builtin("cdr", cdr),
    "null?"    -> predicate("null?", _ == Value.EmptyList),
    "list"     -> Value.Builtin("list", list),
    "length"   -> Value.Builtin("length", length),
    "append"   -> Value.Builtin("append", append),
    "string?"  -> predicate("string?", _.isInstanceOf[Value.StringVal]),
    "number?"  -> predicate("number?", _.isInstanceOf[Value.IntVal]),
    "boolean?" -> predicate("boolean?", _.isInstanceOf[Value.BoolVal]),
    "pair?"    -> predicate("pair?", _.isInstanceOf[Value.PairVal]),
    "symbol?"  -> predicate("symbol?", _.isInstanceOf[Value.SymbolVal])
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

  private def cons(args: List[Value], pos: SourcePos): Value =
    args match
      case carValue :: cdrValue :: Nil =>
        Value.PairVal(carValue, cdrValue)

      case _ =>
        throw EvalError.at(pos, "cons expects exactly 2 arguments")

  private def car(args: List[Value], pos: SourcePos): Value =
    expectSingleArg(args, pos, "car") match
      case Value.PairVal(carValue, _) =>
        carValue

      case other =>
        throw EvalError.at(pos, s"car expected a pair, got ${other.typeName}")

  private def cdr(args: List[Value], pos: SourcePos): Value =
    expectSingleArg(args, pos, "cdr") match
      case Value.PairVal(_, cdrValue) =>
        cdrValue

      case other =>
        throw EvalError.at(pos, s"cdr expected a pair, got ${other.typeName}")

  private def list(args: List[Value], pos: SourcePos): Value =
    Value.list(args)

  private def length(args: List[Value], pos: SourcePos): Value =
    Value.IntVal(asProperList(expectSingleArg(args, pos, "length"), pos, "length").length)

  private def append(args: List[Value], pos: SourcePos): Value =
    args match
      case Nil =>
        Value.EmptyList

      case _ =>
        val prefixItems = args.init.flatMap(arg => asProperList(arg, pos, "append"))
        val suffix      = expectList(expectSingleArg(args.takeRight(1), pos, "append"), pos, "append")
        prefixItems.foldRight(suffix)(Value.PairVal(_, _))

  private def comparison(name: String, relation: (BigInt, BigInt) => Boolean): Value =
    Value.Builtin(
      name,
      (args, pos) =>
        val numbers = asNumbers(args, pos, name)
        if numbers.lengthCompare(2) < 0 then throw EvalError.at(pos, s"$name expects at least 2 arguments")

        Value.BoolVal(numbers.zip(numbers.tail).forall(relation.tupled))
    )

  private def predicate(name: String, test: Value => Boolean): Value =
    Value.Builtin(
      name,
      (args, pos) => Value.BoolVal(test(expectSingleArg(args, pos, name)))
    )

  private def asNumbers(args: List[Value], pos: SourcePos, name: String): List[BigInt] =
    args.map(arg => expectNumber(arg, pos, name))

  private def expectSingleArg(args: List[Value], pos: SourcePos, name: String): Value =
    args match
      case value :: Nil =>
        value

      case _ =>
        throw EvalError.at(pos, s"$name expects exactly 1 argument")

  private def expectNumber(arg: Value, pos: SourcePos, name: String): BigInt =
    arg match
      case Value.IntVal(value) =>
        value

      case other =>
        throw EvalError.at(pos, s"$name expected number arguments, got ${other.typeName}")

  private def expectList(arg: Value, pos: SourcePos, name: String): Value =
    asProperList(arg, pos, name)
    arg

  private def asProperList(arg: Value, pos: SourcePos, name: String): List[Value] =
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
