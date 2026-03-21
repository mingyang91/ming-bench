package ming

/** List-related built-in operations. */
object ListBuiltins:

  def listLength(v: Value): Long = v match
    case Value.NilVal           => 0L
    case Value.PairVal(_, t, _) => 1L + listLength(t)
    case _                      => throw new EvalError("length: not a proper list")

  def evalListRef(args: List[Value]): Value =
    args match
      case lst :: Value.IntVal(idx) :: Nil => listRef(lst, idx.toInt)
      case _                               => throw new EvalError("list-ref requires list and index")

  @scala.annotation.tailrec
  private def listRef(v: Value, idx: Int): Value =
    v match
      case Value.PairVal(car, cdr, _) =>
        if idx == 0 then car else listRef(cdr, idx - 1)
      case _ => throw new EvalError("list-ref: index out of range")

  def evalListTail(args: List[Value]): Value =
    args match
      case lst :: Value.IntVal(idx) :: Nil => listTail(lst, idx.toInt)
      case _                               => throw new EvalError("list-tail requires list and index")

  @scala.annotation.tailrec
  private def listTail(v: Value, idx: Int): Value =
    if idx == 0 then v
    else
      v match
        case Value.PairVal(_, cdr, _) => listTail(cdr, idx - 1)
        case _                        => throw new EvalError("list-tail: index out of range")

  def evalListPred(args: List[Value]): Value =
    args match
      case v :: Nil => Value.BoolVal(isProperList(v))
      case _        => throw new EvalError("list? requires exactly 1 argument")

  @scala.annotation.tailrec
  def isProperList(v: Value): Boolean = v match
    case Value.NilVal           => true
    case Value.PairVal(_, t, _) => isProperList(t)
    case _                      => false

  def evalReverse(args: List[Value]): Value =
    args match
      case lst :: Nil => reverseList(lst, Value.NilVal)
      case _          => throw new EvalError("reverse requires exactly 1 argument")

  @scala.annotation.tailrec
  private def reverseList(lst: Value, acc: Value): Value = lst match
    case Value.NilVal               => acc
    case Value.PairVal(car, cdr, _) => reverseList(cdr, Value.PairVal(car, acc))
    case _                          => throw new EvalError("reverse: not a proper list")

  def evalAssoc(args: List[Value]): Value =
    args match
      case key :: lst :: Nil => assocSearch(key, lst)
      case _                 => throw new EvalError("assoc requires exactly 2 arguments")

  @scala.annotation.tailrec
  private def assocSearch(key: Value, lst: Value): Value =
    lst match
      case Value.NilVal => Value.BoolVal(false)
      case Value.PairVal(pair @ Value.PairVal(k, _, _), rest, _) =>
        if StringBuiltins.schemeEqual(key, k) then pair
        else assocSearch(key, rest)
      case _ => throw new EvalError("assoc: not a proper alist")
