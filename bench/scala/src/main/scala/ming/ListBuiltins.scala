package ming

import java.util.IdentityHashMap

private[ming] object ListBuiltins:

  import BuiltinSupport.*
  import SchemeInterpreter.Value
  import EqualityBuiltins.equalValues

  def all: List[Value.Builtin] =
    List(
      consBuiltin,
      carBuiltin,
      cdrBuiltin,
      setCarBuiltin,
      setCdrBuiltin
    ) ++ cxrBuiltins ++ List(
      nullBuiltin,
      listBuiltin,
      lengthBuiltin,
      appendBuiltin,
      listRefBuiltin,
      listTailBuiltin,
      memberBuiltin,
      assvBuiltin,
      assocBuiltin,
      reverseBuiltin,
      mapBuiltin,
      forEachBuiltin
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

  private val setCarBuiltin: Value.Builtin =
    Value.Builtin(
      "set-car!",
      (args, pos) =>
        val (pairValue, newCar) = twoArgs("set-car!", args, pos)
        val pair                = asPairObject(pairValue, "set-car!", pos)
        pair.setCar(newCar)
        Value.Void
    )

  private val setCdrBuiltin: Value.Builtin =
    Value.Builtin(
      "set-cdr!",
      (args, pos) =>
        val (pairValue, newCdr) = twoArgs("set-cdr!", args, pos)
        val pair                = asPairObject(pairValue, "set-cdr!", pos)
        pair.setCdr(newCdr)
        Value.Void
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
        Value.Number(SchemeNumber.exact(BigInt(items.length)))
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

  private val memberBuiltin: Value.Builtin =
    Value.Builtin(
      "member",
      (args, pos) =>
        val (value, listValue) = twoArgs("member", args, pos)
        findListTail(value, listValue, "member", pos)(equalValues)
    )

  private val assvBuiltin: Value.Builtin =
    Value.Builtin(
      "assv",
      (args, pos) =>
        val (key, alistValue) = twoArgs("assv", args, pos)
        findAssocEntry(key, alistValue, "assv", pos)(EqualityBuiltins.eqvValues)
    )

  private val reverseBuiltin: Value.Builtin =
    Value.Builtin(
      "reverse",
      (args, pos) =>
        val items = asList(singleArg("reverse", args, pos), "reverse", pos)
        Value.list(items.reverse)
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

  private val forEachBuiltin: Value.Builtin =
    Value.Builtin(
      "for-each",
      (args, pos) =>
        requireAtLeast("for-each", args, expected = 2, pos)
        val procedure = args.head
        val lists     = args.tail.map(asList(_, "for-each", pos))

        @annotation.tailrec
        def loop(current: List[List[Value]]): Value =
          if current.exists(_.isEmpty) then Value.Void
          else
            val callArgs = current.map(_.head)
            val next     = current.map(_.tail)
            SchemeInterpreter.applyProcedure(procedure, callArgs, pos)
            loop(next)

        loop(lists)
    )

  private val cxrBuiltins: List[Value.Builtin] =
    List(2, 3, 4).flatMap(generateCxrNames).map(cxrBuiltin)

  private def generateCxrNames(depth: Int): List[String] =
    def loop(remaining: Int, acc: String): List[String] =
      if remaining == 0 then List(s"c${acc}r")
      else loop(remaining - 1, acc + "a") ++ loop(remaining - 1, acc + "d")

    loop(depth, "")

  private def cxrBuiltin(name: String): Value.Builtin =
    Value.Builtin(
      name,
      (args, pos) =>
        name
          .substring(1, name.length - 1)
          .reverse
          .foldLeft(singleArg(name, args, pos)) { (current, selector) =>
            val (carValue, cdrValue) = asPair(current, name, pos)
            if selector == 'a' then carValue else cdrValue
          }
    )

  private def findListTail(
    target: Value,
    listValue: Value,
    context: String,
    pos: SourcePos
  )(matches: (Value, Value) => Boolean): Value =
    val visited = new IdentityHashMap[Value.Pair, java.lang.Boolean]()
    var current = listValue

    while true do
      current match
        case Value.EmptyList =>
          return Value.Bool(false)
        case pair: Value.Pair =>
          if visited.containsKey(pair) then
            fail(pos, s"$context expected list, got ${SchemeInterpreter.render(listValue)}")
          visited.put(pair, java.lang.Boolean.TRUE)
          if matches(target, pair.car) then return pair
          current = pair.cdr
        case _ =>
          fail(pos, s"$context expected list, got ${SchemeInterpreter.render(listValue)}")

    Value.Bool(false)

  private def findAssocEntry(
    key: Value,
    alistValue: Value,
    context: String,
    pos: SourcePos
  )(matches: (Value, Value) => Boolean): Value =
    val visited = new IdentityHashMap[Value.Pair, java.lang.Boolean]()
    var current = alistValue

    while true do
      current match
        case Value.EmptyList =>
          return Value.Bool(false)
        case pair: Value.Pair =>
          if visited.containsKey(pair) then
            fail(pos, s"$context expected list, got ${SchemeInterpreter.render(alistValue)}")
          visited.put(pair, java.lang.Boolean.TRUE)
          pair.car match
            case entry: Value.Pair if matches(key, entry.car) =>
              return entry
            case _ =>
              current = pair.cdr
        case _ =>
          fail(pos, s"$context expected list, got ${SchemeInterpreter.render(alistValue)}")

    Value.Bool(false)
