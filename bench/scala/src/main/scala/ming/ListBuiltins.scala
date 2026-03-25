package ming

private[ming] object ListBuiltins:

  import BuiltinSupport.*
  import SchemeInterpreter.Value

  def all: List[Value.Builtin] =
    List(
      consBuiltin,
      carBuiltin,
      cdrBuiltin,
      nullBuiltin,
      listBuiltin,
      lengthBuiltin,
      appendBuiltin
    )

  private val consBuiltin: Value.Builtin =
    Value.Builtin(
      "cons",
      (args, pos) =>
        val (head, tail) = twoArgs("cons", args, pos)
        Value.ListValue(head :: asList(tail, "cons", pos))
    )

  private val carBuiltin: Value.Builtin =
    Value.Builtin(
      "car",
      (args, pos) =>
        val (head, _) = nonEmptyListArg("car", args, pos)
        head
    )

  private val cdrBuiltin: Value.Builtin =
    Value.Builtin(
      "cdr",
      (args, pos) =>
        val (_, tail) = nonEmptyListArg("cdr", args, pos)
        Value.ListValue(tail)
    )

  private val nullBuiltin: Value.Builtin =
    Value.Builtin(
      "null?",
      (args, pos) =>
        val value = singleArg("null?", args, pos)
        Value.Bool(value == Value.ListValue(Nil))
    )

  private val listBuiltin: Value.Builtin =
    Value.Builtin("list", (args, _) => Value.ListValue(args))

  private val lengthBuiltin: Value.Builtin =
    Value.Builtin(
      "length",
      (args, pos) =>
        val items = asList(singleArg("length", args, pos), "length", pos)
        Value.Number(BigInt(items.length))
    )

  private val appendBuiltin: Value.Builtin =
    Value.Builtin(
      "append",
      (args, pos) => Value.ListValue(args.flatMap(asList(_, "append", pos)))
    )
