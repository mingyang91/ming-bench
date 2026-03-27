package ming

import scala.collection.mutable
import scala.annotation.tailrec

private[ming] object Level1ListBuiltins:

  import RuntimeSupport.*
  import Level1ValueSupport.*

  val values: Map[String, Value] = Map(
    "cons"      -> BuiltinValue("cons", cons),
    "car"       -> BuiltinValue("car", car),
    "cdr"       -> BuiltinValue("cdr", cdr),
    "caar"      -> BuiltinValue("caar", caar),
    "cadr"      -> BuiltinValue("cadr", cadr),
    "cdar"      -> BuiltinValue("cdar", cdar),
    "cddr"      -> BuiltinValue("cddr", cddr),
    "set-car!"  -> BuiltinValue("set-car!", setCar),
    "set-cdr!"  -> BuiltinValue("set-cdr!", setCdr),
    "apply"     -> BuiltinValue("apply", applyProcedure),
    "map"       -> BuiltinValue("map", mapValues),
    "for-each"  -> BuiltinValue("for-each", forEachValues),
    "list"      -> BuiltinValue("list", list),
    "list-ref"  -> BuiltinValue("list-ref", listRef),
    "list-tail" -> BuiltinValue("list-tail", listTail),
    "length"    -> BuiltinValue("length", length),
    "append"    -> BuiltinValue("append", append),
    "reverse"   -> BuiltinValue("reverse", reverse),
    "memq"      -> BuiltinValue("memq", memq),
    "memv"      -> BuiltinValue("memv", memv),
    "member"    -> BuiltinValue("member", member),
    "assq"      -> BuiltinValue("assq", assq),
    "assv"      -> BuiltinValue("assv", assv),
    "assoc"     -> BuiltinValue("assoc", assoc)
  )

  private def cons(arguments: List[Value], position: Position): Value =
    val (first, second) = expectTwoArguments(arguments, "cons", position)
    PairValue(first, second)

  private def car(arguments: List[Value], position: Position): Value =
    expectPair(expectSingleArgument(arguments, "car", position), "car", position).car

  private def cdr(arguments: List[Value], position: Position): Value =
    expectPair(expectSingleArgument(arguments, "cdr", position), "cdr", position).cdr

  private def caar(arguments: List[Value], position: Position): Value =
    val value = expectSingleArgument(arguments, "caar", position)
    expectPair(expectPair(value, "caar", position).car, "caar", position).car

  private def cadr(arguments: List[Value], position: Position): Value =
    val value = expectSingleArgument(arguments, "cadr", position)
    expectPair(expectPair(value, "cadr", position).cdr, "cadr", position).car

  private def cdar(arguments: List[Value], position: Position): Value =
    val value = expectSingleArgument(arguments, "cdar", position)
    expectPair(expectPair(value, "cdar", position).car, "cdar", position).cdr

  private def cddr(arguments: List[Value], position: Position): Value =
    val value = expectSingleArgument(arguments, "cddr", position)
    expectPair(expectPair(value, "cddr", position).cdr, "cddr", position).cdr

  private def setCar(arguments: List[Value], position: Position): Value =
    val (pairValue, replacement) = expectTwoArguments(arguments, "set-car!", position)
    expectPair(pairValue, "set-car!", position).updateCar(replacement)
    VoidValue

  private def setCdr(arguments: List[Value], position: Position): Value =
    val (pairValue, replacement) = expectTwoArguments(arguments, "set-cdr!", position)
    expectPair(pairValue, "set-cdr!", position).updateCdr(replacement)
    VoidValue

  private def applyProcedure(arguments: List[Value], position: Position): Value =
    expectAtLeast(arguments, 2, "apply", position) match
      case function :: rest =>
        val prefixArguments = rest.dropRight(1)
        val listArguments   = expectProperList(rest.last, "apply", position)
        InterpreterEvaluator.applyFunction(function, prefixArguments ++ listArguments, position)
      case _ =>
        throw new IllegalStateException("validated apply argument list")

  private def mapValues(arguments: List[Value], position: Position): Value =
    expectAtLeast(arguments, 2, "map", position) match
      case function :: listArguments =>
        val lists = listArguments.map(expectProperList(_, "map", position))
        ensureEqualLengths(lists, position)
        mapAcrossLists(function, lists, position)
      case _ =>
        throw new IllegalStateException("validated map argument list")

  private def forEachValues(arguments: List[Value], position: Position): Value =
    expectAtLeast(arguments, 2, "for-each", position) match
      case function :: listArguments =>
        val lists = listArguments.map(expectProperList(_, "for-each", position))
        ensureEqualLengths(lists, position)
        forEachAcrossLists(function, lists, position)
        VoidValue
      case _ =>
        throw new IllegalStateException("validated for-each argument list")

  private def list(arguments: List[Value], position: Position): Value =
    buildList(arguments)

  private def listRef(arguments: List[Value], position: Position): Value =
    val (listValue, indexValue) = expectTwoArguments(arguments, "list-ref", position)
    val index                   = expectIndex(indexValue, "list-ref", position)
    listElementAt(listTailValue(listValue, index, "list-ref", position), position)

  private def listTail(arguments: List[Value], position: Position): Value =
    val (listValue, indexValue) = expectTwoArguments(arguments, "list-tail", position)
    val index                   = expectIndex(indexValue, "list-tail", position)
    listTailValue(listValue, index, "list-tail", position)

  private def length(arguments: List[Value], position: Position): Value =
    val value = expectSingleArgument(arguments, "length", position)
    IntValue(expectProperList(value, "length", position).length)

  private def append(arguments: List[Value], position: Position): Value =
    val elements = arguments.flatMap(argument => expectProperList(argument, "append", position))
    buildList(elements)

  private def reverse(arguments: List[Value], position: Position): Value =
    val value = expectSingleArgument(arguments, "reverse", position)
    buildList(expectProperList(value, "reverse", position).reverse)

  private def memq(arguments: List[Value], position: Position): Value =
    membership(arguments, "memq", position)(eqValues)

  private def memv(arguments: List[Value], position: Position): Value =
    membership(arguments, "memv", position)(eqvValues)

  private def member(arguments: List[Value], position: Position): Value =
    membership(arguments, "member", position)(equalValues)

  private def assq(arguments: List[Value], position: Position): Value =
    association(arguments, "assq", position)(eqValues)

  private def assv(arguments: List[Value], position: Position): Value =
    association(arguments, "assv", position)(eqvValues)

  private def assoc(arguments: List[Value], position: Position): Value =
    association(arguments, "assoc", position)(equalValues)

  private def mapAcrossLists(
    function: Value,
    lists: List[List[Value]],
    position: Position,
    reversedResult: List[Value] = Nil
  ): Value =
    lists match
      case Nil =>
        throw new IllegalStateException("validated map argument list")
      case first :: _ if first.isEmpty =>
        buildList(reversedResult.reverse)
      case _ =>
        val (heads, tails) = collectHeadsAndTails(lists)
        val mappedValue    = InterpreterEvaluator.applyFunction(function, heads, position)
        mapAcrossLists(function, tails, position, mappedValue :: reversedResult)

  private def collectHeadsAndTails(lists: List[List[Value]]): (List[Value], List[List[Value]]) =
    lists.foldRight((List.empty[Value], List.empty[List[Value]])):
      case (head :: tail, (heads, tails)) =>
        (head :: heads, tail :: tails)
      case (Nil, _) =>
        throw new IllegalStateException("validated equal non-empty lists")

  @tailrec
  private def forEachAcrossLists(
    function: Value,
    lists: List[List[Value]],
    position: Position
  ): Unit =
    lists match
      case Nil =>
        throw new IllegalStateException("validated for-each argument list")
      case first :: _ if first.isEmpty =>
        ()
      case _ =>
        val (heads, tails) = collectHeadsAndTails(lists)
        InterpreterEvaluator.applyFunction(function, heads, position)
        forEachAcrossLists(function, tails, position)

  private def ensureEqualLengths(lists: List[List[Value]], position: Position): Unit =
    if lists.map(_.length).distinct.length > 1 then
      SchemeFailure.raise("map expected list arguments of equal length", position)

  private def membership(
    arguments: List[Value],
    name: String,
    position: Position
  )(matches: (Value, Value) => Boolean): Value =
    val (key, listValue) = expectTwoArguments(arguments, name, position)
    membershipValue(key, listValue, name, position)(matches)

  private def association(
    arguments: List[Value],
    name: String,
    position: Position
  )(matches: (Value, Value) => Boolean): Value =
    val (key, listValue) = expectTwoArguments(arguments, name, position)
    associationValue(key, listValue, name, position)(matches)

  private def listElementAt(value: Value, position: Position): Value =
    value match
      case PairValue(head, _) => head
      case EmptyListValue     => SchemeFailure.raise("list-ref index out of bounds", position)
      case other =>
        SchemeFailure.raise(s"list-ref expected a pair, got ${typeName(other)}", position)

  @tailrec
  private def listTailValue(
    value: Value,
    index: Int,
    name: String,
    position: Position
  ): Value =
    value match
      case pair @ PairValue(_, tail) =>
        if index == 0 then pair
        else listTailValue(tail, index - 1, name, position)
      case EmptyListValue =>
        if index == 0 then EmptyListValue
        else SchemeFailure.raise(s"$name index out of bounds", position)
      case other =>
        SchemeFailure.raise(s"$name expected a list, got ${typeName(other)}", position)

  private def membershipValue(
    key: Value,
    value: Value,
    name: String,
    position: Position
  )(matches: (Value, Value) => Boolean): Value =
    val visited = mutable.HashSet.empty[PairValue]
    var current = value

    while true do
      current match
        case EmptyListValue =>
          return BoolValue(false)
        case pair: PairValue =>
          if visited.contains(pair) then
            SchemeFailure.raise(s"$name expected a proper list, got circular list", position)

          visited += pair
          if matches(key, pair.car) then return pair
          current = pair.cdr
        case other =>
          SchemeFailure.raise(s"$name expected a proper list, got ${typeName(other)}", position)

    throw new IllegalStateException("unreachable membership traversal")

  private def associationValue(
    key: Value,
    value: Value,
    name: String,
    position: Position
  )(matches: (Value, Value) => Boolean): Value =
    val visited = mutable.HashSet.empty[PairValue]
    var current = value

    while true do
      current match
        case EmptyListValue =>
          return BoolValue(false)
        case pair: PairValue =>
          if visited.contains(pair) then
            SchemeFailure.raise(s"$name expected a proper list, got circular list", position)

          visited += pair
          pair.car match
            case entry: PairValue =>
              if matches(key, entry.car) then return entry
              current = pair.cdr
            case other =>
              SchemeFailure.raise(
                s"$name expected pairs in its association list, got ${typeName(other)}",
                position
              )
        case other =>
          SchemeFailure.raise(s"$name expected a proper list, got ${typeName(other)}", position)

    throw new IllegalStateException("unreachable association traversal")
