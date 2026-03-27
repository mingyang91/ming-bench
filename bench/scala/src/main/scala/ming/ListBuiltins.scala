package ming

import BuiltinSupport.*

private[ming] object ListBuiltins:

  private val coreNames = Set(
    "cons",
    "car",
    "cdr",
    "set-car!",
    "set-cdr!",
    "append",
    "list",
    "length"
  )

  private val utilityNames = Set(
    "list-ref",
    "list-tail",
    "list?",
    "assoc",
    "assq",
    "assv",
    "member",
    "memq",
    "memv",
    "reverse"
  )

  val names: Set[String] = coreNames ++ utilityNames

  def handlesCore(name: String): Boolean =
    coreNames.contains(name)

  def handlesUtility(name: String): Boolean =
    utilityNames.contains(name)

  def invokeCore(name: String, args: List[Value], pos: SourcePos): Value =
    name match
      case "cons" =>
        val values = requireArgCount(name, args, expected = 2, pos)
        Value.PairVal(values.head, values(1))
      case "car" =>
        val (car, _) = requirePair(name, requireSingleArg(name, args, pos), pos)
        car
      case "cdr" =>
        val (_, cdr) = requirePair(name, requireSingleArg(name, args, pos), pos)
        cdr
      case "set-car!" =>
        mutatePair(name, args, pos)(_.setCar(_))
      case "set-cdr!" =>
        mutatePair(name, args, pos)(_.setCdr(_))
      case "append" =>
        append(name, args, pos)
      case "list" =>
        ValueSemantics.listFrom(args)
      case "length" =>
        val value = requireSingleArg(name, args, pos)
        Value.IntVal(ValueSemantics.toProperList(name, value, pos).length)
      case _ =>
        unknownProcedure(name, pos)

  def invokeUtility(name: String, args: List[Value], pos: SourcePos): Value =
    name match
      case "list-ref" =>
        listRef(name, args, pos)
      case "list-tail" =>
        listTail(name, args, pos)
      case "list?" =>
        Value.BoolVal(ValueSemantics.isProperList(requireSingleArg(name, args, pos)))
      case "assoc" =>
        assoc(name, args, pos, ValueSemantics.isEqual)
      case "assq" =>
        assoc(name, args, pos, ValueSemantics.isEq)
      case "assv" =>
        assoc(name, args, pos, ValueSemantics.isEqv)
      case "member" =>
        member(name, args, pos, ValueSemantics.isEqual)
      case "memq" =>
        member(name, args, pos, ValueSemantics.isEq)
      case "memv" =>
        member(name, args, pos, ValueSemantics.isEqv)
      case "reverse" =>
        ValueSemantics.listFrom(ValueSemantics.toProperList(name, requireSingleArg(name, args, pos), pos).reverse)
      case _ =>
        unknownProcedure(name, pos)

  private def listRef(name: String, args: List[Value], pos: SourcePos): Value =
    val values = requireArgCount(name, args, expected = 2, pos)
    val index  = requireNonNegativeIndex(name, values(1), pos)

    @annotation.tailrec
    def loop(current: Value, remaining: Int): Value =
      if remaining == 0 then
        current match
          case Value.PairVal(car, _) => car
          case Value.EmptyList       => throw EvalError.at(pos, s"$name index out of range")
          case other => throw EvalError.at(pos, s"$name expected a list, got ${ValueSemantics.typeName(other)}")
      else
        current match
          case Value.PairVal(_, cdr) => loop(cdr, remaining - 1)
          case Value.EmptyList       => throw EvalError.at(pos, s"$name index out of range")
          case other => throw EvalError.at(pos, s"$name expected a list, got ${ValueSemantics.typeName(other)}")

    loop(values.head, index)

  private def listTail(name: String, args: List[Value], pos: SourcePos): Value =
    val values = requireArgCount(name, args, expected = 2, pos)
    val index  = requireNonNegativeIndex(name, values(1), pos)

    @annotation.tailrec
    def loop(current: Value, remaining: Int): Value =
      if remaining == 0 then current
      else
        current match
          case Value.PairVal(_, cdr) => loop(cdr, remaining - 1)
          case Value.EmptyList       => throw EvalError.at(pos, s"$name index out of range")
          case other => throw EvalError.at(pos, s"$name expected a list, got ${ValueSemantics.typeName(other)}")

    loop(values.head, index)

  private def assoc(
    name: String,
    args: List[Value],
    pos: SourcePos,
    predicate: (Value, Value) => Boolean
  ): Value =
    val values = requireArgCount(name, args, expected = 2, pos)
    val key    = values.head
    val alist  = ValueSemantics.toProperList(name, values(1), pos)

    @annotation.tailrec
    def loop(entries: List[Value]): Value =
      entries match
        case Nil =>
          Value.BoolVal(false)
        case (entry @ Value.PairVal(car, _)) :: rest =>
          if predicate(key, car) then entry
          else loop(rest)
        case other :: _ =>
          throw EvalError.at(pos, s"$name expected an association list, got ${ValueSemantics.typeName(other)}")

    loop(alist)

  private def member(
    name: String,
    args: List[Value],
    pos: SourcePos,
    predicate: (Value, Value) => Boolean
  ): Value =
    val values = requireArgCount(name, args, expected = 2, pos)
    val key    = values.head
    val seen   = scala.collection.mutable.HashSet.empty[Value.PairVal]

    @annotation.tailrec
    def loop(current: Value): Value =
      current match
        case Value.EmptyList =>
          Value.BoolVal(false)
        case pair: Value.PairVal =>
          if predicate(key, pair.car) then current
          else if seen.contains(pair) then throw EvalError.at(pos, s"$name expected a proper list, got circular list")
          else
            seen += pair
            loop(pair.cdr)
        case other =>
          throw EvalError.at(pos, s"$name expected a list, got ${ValueSemantics.typeName(other)}")

    loop(values(1))

  private def requireNonNegativeIndex(name: String, value: Value, pos: SourcePos): Int =
    val index = requireIndex(name, value, pos)
    if index < 0 then throw EvalError.at(pos, s"$name expected a non-negative index")
    index

  private def mutatePair(
    name: String,
    args: List[Value],
    pos: SourcePos
  )(update: (Value.PairVal, Value) => Unit): Value =
    val values = requireArgCount(name, args, expected = 2, pos)
    val pair   = requirePairValue(name, values.head, pos)
    update(pair, values(1))
    Value.Void

  private def append(name: String, args: List[Value], pos: SourcePos): Value =
    args match
      case Nil =>
        Value.EmptyList
      case last :: Nil =>
        last
      case _ =>
        val last = args.last
        args.init.foldRight(last) { (listValue, acc) =>
          ValueSemantics.toProperList(name, listValue, pos).foldRight(acc) { (item, tail) =>
            Value.PairVal(item, tail)
          }
        }
