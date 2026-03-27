package ming

import scala.annotation.tailrec

private[ming] object Level1ListBuiltins:

  import RuntimeSupport.*
  import Level1ValueSupport.equalValues

  val values: Map[String, Value] = Map(
    "cons"      -> BuiltinValue("cons", cons),
    "car"       -> BuiltinValue("car", car),
    "cdr"       -> BuiltinValue("cdr", cdr),
    "apply"     -> BuiltinValue("apply", applyProcedure),
    "map"       -> BuiltinValue("map", mapValues),
    "list"      -> BuiltinValue("list", list),
    "list-ref"  -> BuiltinValue("list-ref", listRef),
    "list-tail" -> BuiltinValue("list-tail", listTail),
    "length"    -> BuiltinValue("length", length),
    "append"    -> BuiltinValue("append", append),
    "assoc"     -> BuiltinValue("assoc", assoc)
  )

  private def cons(arguments: List[Value], position: Position): Value =
    val (first, second) = expectTwoArguments(arguments, "cons", position)
    PairValue(first, second)

  private def car(arguments: List[Value], position: Position): Value =
    expectPair(expectSingleArgument(arguments, "car", position), "car", position).car

  private def cdr(arguments: List[Value], position: Position): Value =
    expectPair(expectSingleArgument(arguments, "cdr", position), "cdr", position).cdr

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

  private def assoc(arguments: List[Value], position: Position): Value =
    val (key, listValue) = expectTwoArguments(arguments, "assoc", position)
    assocValue(key, listValue, position)

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

  private def ensureEqualLengths(lists: List[List[Value]], position: Position): Unit =
    if lists.map(_.length).distinct.length > 1 then
      SchemeFailure.raise("map expected list arguments of equal length", position)

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

  @tailrec
  private def assocValue(key: Value, value: Value, position: Position): Value =
    value match
      case EmptyListValue =>
        BoolValue(false)
      case PairValue(entry, rest) =>
        entry match
          case pair: PairValue =>
            if equalValues(key, pair.car) then pair
            else assocValue(key, rest, position)
          case other =>
            SchemeFailure.raise(
              s"assoc expected pairs in its association list, got ${typeName(other)}",
              position
            )
      case other =>
        SchemeFailure.raise(s"assoc expected a proper list, got ${typeName(other)}", position)
