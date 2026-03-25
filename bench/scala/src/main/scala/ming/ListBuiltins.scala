package ming

private[ming] object ListBuiltins:

  import BuiltinSupport.*
  import SchemeInterpreter.Value
  import EqualityBuiltins.equalValues

  def all: List[Value.Builtin] =
    List(
      consBuiltin,
      carBuiltin,
      cdrBuiltin,
      nullBuiltin,
      listBuiltin,
      lengthBuiltin,
      appendBuiltin,
      listRefBuiltin,
      listTailBuiltin,
      assocBuiltin,
      mapBuiltin
    )

  private val consBuiltin: Value.Builtin =
    Value.Builtin(
      "cons",
      (args, pos) =>
        val (head, tail) = twoArgs("cons", args, pos)
        Value.Pair(head, tail)
    )

  private val carBuiltin: Value.Builtin =
    Value.Builtin(
      "car",
      (args, pos) =>
        val (head, _) = asPair(singleArg("car", args, pos), "car", pos)
        head
    )

  private val cdrBuiltin: Value.Builtin =
    Value.Builtin(
      "cdr",
      (args, pos) =>
        val (_, tail) = asPair(singleArg("cdr", args, pos), "cdr", pos)
        tail
    )

  private val nullBuiltin: Value.Builtin =
    Value.Builtin(
      "null?",
      (args, pos) =>
        val value = singleArg("null?", args, pos)
        Value.Bool(value == Value.EmptyList)
    )

  private val listBuiltin: Value.Builtin =
    Value.Builtin("list", (args, _) => Value.list(args))

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
      (args, pos) => Value.list(args.flatMap(asList(_, "append", pos)))
    )

  private val listRefBuiltin: Value.Builtin =
    Value.Builtin(
      "list-ref",
      (args, pos) =>
        val (listValue, indexValue) = twoArgs("list-ref", args, pos)
        val items                   = asList(listValue, "list-ref", pos)
        val index                   = asIndex(indexValue, "list-ref", pos)
        items.lift(index).getOrElse(fail(pos, "list-ref index out of bounds"))
    )

  private val listTailBuiltin: Value.Builtin =
    Value.Builtin(
      "list-tail",
      (args, pos) =>
        val (listValue, indexValue) = twoArgs("list-tail", args, pos)
        val items                   = asList(listValue, "list-tail", pos)
        val index                   = asIndex(indexValue, "list-tail", pos)
        if index > items.length then fail(pos, "list-tail index out of bounds")
        Value.list(items.drop(index))
    )

  private val assocBuiltin: Value.Builtin =
    Value.Builtin(
      "assoc",
      (args, pos) =>
        val (key, alistValue) = twoArgs("assoc", args, pos)
        val entries           = asList(alistValue, "assoc", pos)

        entries
          .collectFirst {
            case entry @ Value.Pair(car, _) if equalValues(key, car) => entry
          }
          .getOrElse(Value.Bool(false))
    )

  private val mapBuiltin: Value.Builtin =
    Value.Builtin(
      "map",
      (args, pos) =>
        requireAtLeast("map", args, expected = 2, pos)
        val procedure = args.head
        val lists     = args.tail.map(asList(_, "map", pos))

        @annotation.tailrec
        def loop(current: List[List[Value]], acc: List[Value]): Value =
          if current.exists(_.isEmpty) then Value.list(acc.reverse)
          else
            val callArgs = current.map(_.head)
            val next     = current.map(_.tail)
            val result   = SchemeInterpreter.applyProcedure(procedure, callArgs, pos)
            loop(next, result :: acc)

        loop(lists, Nil)
    )
