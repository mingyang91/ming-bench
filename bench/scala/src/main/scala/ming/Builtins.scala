package ming

import scala.annotation.tailrec

private[ming] object Builtins:

  def globalEnv(runtime: RuntimeContext): Map[String, Value] = Map(
    "+"              -> Value.Builtin("+", add),
    "-"              -> Value.Builtin("-", subtract),
    "*"              -> Value.Builtin("*", multiply),
    "/"              -> Value.Builtin("/", divide),
    "<"              -> comparison("<", _ < _),
    ">"              -> comparison(">", _ > _),
    "="              -> comparison("=", _ == _),
    "<="             -> comparison("<=", _ <= _),
    "not"            -> Value.Builtin("not", negate),
    "cons"           -> Value.Builtin("cons", cons),
    "car"            -> Value.Builtin("car", car),
    "cdr"            -> Value.Builtin("cdr", cdr),
    "null?"          -> predicate("null?", _ == Value.EmptyList),
    "list"           -> Value.Builtin("list", list),
    "length"         -> Value.Builtin("length", length),
    "append"         -> Value.Builtin("append", append),
    "display"        -> Value.Builtin("display", display(runtime)),
    "write"          -> Value.Builtin("write", write(runtime)),
    "newline"        -> Value.Builtin("newline", newline(runtime)),
    "string-append"  -> Value.Builtin("string-append", stringAppend),
    "string-copy"    -> Value.Builtin("string-copy", stringCopy),
    "string-length"  -> Value.Builtin("string-length", stringLength),
    "substring"      -> Value.Builtin("substring", substring),
    "string->number" -> Value.Builtin("string->number", stringToNumber),
    "number->string" -> Value.Builtin("number->string", numberToString),
    "symbol->string" -> Value.Builtin("symbol->string", symbolToString),
    "string->symbol" -> Value.Builtin("string->symbol", stringToSymbol),
    "string-ref"     -> Value.Builtin("string-ref", stringRef),
    "string-set!"    -> Value.Builtin("string-set!", stringSet),
    "string?"        -> predicate("string?", _.isInstanceOf[Value.StringVal]),
    "number?"        -> predicate("number?", _.isInstanceOf[Value.IntVal]),
    "boolean?"       -> predicate("boolean?", _.isInstanceOf[Value.BoolVal]),
    "pair?"          -> predicate("pair?", _.isInstanceOf[Value.PairVal]),
    "symbol?"        -> predicate("symbol?", _.isInstanceOf[Value.SymbolVal]),
    "char?"          -> predicate("char?", _.isInstanceOf[Value.CharVal])
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

  private def display(runtime: RuntimeContext)(args: List[Value], pos: SourcePos): Value =
    runtime.appendOutput(expectSingleArg(args, pos, "display").renderDisplay)
    Value.VoidVal

  private def write(runtime: RuntimeContext)(args: List[Value], pos: SourcePos): Value =
    runtime.appendOutput(expectSingleArg(args, pos, "write").render)
    Value.VoidVal

  private def newline(runtime: RuntimeContext)(args: List[Value], pos: SourcePos): Value =
    if args.nonEmpty then throw EvalError.at(pos, "newline expects exactly 0 arguments")
    runtime.appendOutput("\n")
    Value.VoidVal

  private def stringAppend(args: List[Value], pos: SourcePos): Value =
    Value.StringVal(args.map(arg => expectString(arg, pos, "string-append")).mkString.toCharArray)

  private def stringCopy(args: List[Value], pos: SourcePos): Value =
    Value.StringVal(expectStringValue(expectSingleArg(args, pos, "string-copy"), pos, "string-copy").clone())

  private def stringLength(args: List[Value], pos: SourcePos): Value =
    Value.IntVal(expectString(expectSingleArg(args, pos, "string-length"), pos, "string-length").length)

  private def substring(args: List[Value], pos: SourcePos): Value =
    args match
      case stringValue :: startValue :: endValue :: Nil =>
        val text  = expectString(stringValue, pos, "substring")
        val start = expectIndex(startValue, pos, "substring")
        val end   = expectIndex(endValue, pos, "substring")
        if start > end || end > text.length then throw EvalError.at(pos, "substring indices are out of bounds")

        Value.StringVal(text.substring(start, end).toCharArray)

      case _ =>
        throw EvalError.at(pos, "substring expects exactly 3 arguments")

  private def stringToNumber(args: List[Value], pos: SourcePos): Value =
    val text = expectString(expectSingleArg(args, pos, "string->number"), pos, "string->number").trim
    if text.isEmpty then Value.BoolVal(false)
    else
      try Value.IntVal(BigInt(text))
      catch
        case _: NumberFormatException =>
          Value.BoolVal(false)

  private def numberToString(args: List[Value], pos: SourcePos): Value =
    Value.StringVal(
      expectNumber(expectSingleArg(args, pos, "number->string"), pos, "number->string").toString.toCharArray
    )

  private def symbolToString(args: List[Value], pos: SourcePos): Value =
    Value.StringVal(expectSymbol(expectSingleArg(args, pos, "symbol->string"), pos, "symbol->string").toCharArray)

  private def stringToSymbol(args: List[Value], pos: SourcePos): Value =
    Value.SymbolVal(expectString(expectSingleArg(args, pos, "string->symbol"), pos, "string->symbol"))

  private def stringRef(args: List[Value], pos: SourcePos): Value =
    args match
      case stringValue :: indexValue :: Nil =>
        val text  = expectString(stringValue, pos, "string-ref")
        val index = expectIndex(indexValue, pos, "string-ref")
        if index >= text.length then throw EvalError.at(pos, "string-ref index is out of bounds")

        Value.CharVal(text.charAt(index))

      case _ =>
        throw EvalError.at(pos, "string-ref expects exactly 2 arguments")

  private def stringSet(args: List[Value], pos: SourcePos): Value =
    args match
      case stringValue :: indexValue :: charValue :: Nil =>
        val text  = expectStringValue(stringValue, pos, "string-set!")
        val index = expectIndex(indexValue, pos, "string-set!")
        if index >= text.length then throw EvalError.at(pos, "string-set! index is out of bounds")

        text(index) = expectChar(charValue, pos, "string-set!")
        Value.VoidVal

      case _ =>
        throw EvalError.at(pos, "string-set! expects exactly 3 arguments")

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

  private def expectString(arg: Value, pos: SourcePos, name: String): String =
    new String(expectStringValue(arg, pos, name))

  private def expectStringValue(arg: Value, pos: SourcePos, name: String): Array[Char] =
    arg match
      case Value.StringVal(value) =>
        value

      case other =>
        throw EvalError.at(pos, s"$name expected string arguments, got ${other.typeName}")

  private def expectSymbol(arg: Value, pos: SourcePos, name: String): String =
    arg match
      case Value.SymbolVal(value) =>
        value

      case other =>
        throw EvalError.at(pos, s"$name expected a symbol, got ${other.typeName}")

  private def expectChar(arg: Value, pos: SourcePos, name: String): Char =
    arg match
      case Value.CharVal(value) =>
        value

      case other =>
        throw EvalError.at(pos, s"$name expected a char, got ${other.typeName}")

  private def expectIndex(arg: Value, pos: SourcePos, name: String): Int =
    val value = expectNumber(arg, pos, name)
    if value < 0 || !value.isValidInt then throw EvalError.at(pos, s"$name expected a non-negative integer index")

    value.toInt

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
