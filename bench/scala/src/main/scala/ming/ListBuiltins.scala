package ming

import scala.annotation.tailrec

/** List-related built-in operations. */
object ListBuiltins:

  private def cdr(v: Value): Value = v match
    case Value.PairVal(_, t, _)     => t
    case Value.MutablePairVal(cell) => cell(1)
    case _                          => throw new EvalError("not a pair")

  private def car(v: Value): Value = v match
    case Value.PairVal(h, _, _)     => h
    case Value.MutablePairVal(cell) => cell(0)
    case _                          => throw new EvalError("not a pair")

  def listLength(v: Value): Long = v match
    case Value.NilVal               => 0L
    case Value.PairVal(_, t, _)     => 1L + listLength(t)
    case Value.MutablePairVal(cell) => 1L + listLength(cell(1))
    case _                          => throw new EvalError("length: not a proper list")

  def evalListRef(args: List[Value]): Value =
    args match
      case lst :: Value.IntVal(idx) :: Nil => listRef(lst, idx.toInt)
      case _                               => throw new EvalError("list-ref requires list and index")

  @tailrec
  private def listRef(v: Value, idx: Int): Value =
    v match
      case Value.PairVal(h, t, _) =>
        if idx == 0 then h else listRef(t, idx - 1)
      case Value.MutablePairVal(cell) =>
        if idx == 0 then cell(0) else listRef(cell(1), idx - 1)
      case _ => throw new EvalError("list-ref: index out of range")

  def evalListTail(args: List[Value]): Value =
    args match
      case lst :: Value.IntVal(idx) :: Nil => listTail(lst, idx.toInt)
      case _                               => throw new EvalError("list-tail requires list and index")

  @tailrec
  private def listTail(v: Value, idx: Int): Value =
    if idx == 0 then v
    else
      v match
        case Value.PairVal(_, t, _)     => listTail(t, idx - 1)
        case Value.MutablePairVal(cell) => listTail(cell(1), idx - 1)
        case _                          => throw new EvalError("list-tail: index out of range")

  def evalListPred(args: List[Value]): Value =
    args match
      case v :: Nil => Value.BoolVal(isProperList(v))
      case _        => throw new EvalError("list? requires exactly 1 argument")

  /** Cycle-safe list? using Floyd's tortoise-and-hare algorithm. */
  def isProperList(v: Value): Boolean =
    isProperListCycle(v, v, firstStep = true)

  @tailrec
  private def isProperListCycle(slow: Value, fast: Value, firstStep: Boolean): Boolean =
    pairCdr(fast) match
      case None =>
        fast match
          case Value.NilVal => true;
          case _            => false
      case Some(f1) =>
        pairCdr(f1) match
          case None =>
            f1 match
              case Value.NilVal => true;
              case _            => false
          case Some(f2) =>
            val s1 = pairCdr(slow).getOrElse(slow)
            if !firstStep && (s1 eq f2) then false
            else isProperListCycle(s1, f2, firstStep = false)

  private def pairCdr(v: Value): Option[Value] = v match
    case Value.PairVal(_, t, _)     => Some(t)
    case Value.MutablePairVal(cell) => Some(cell(1))
    case _                          => None

  def evalReverse(args: List[Value]): Value =
    args match
      case lst :: Nil => reverseList(lst, Value.NilVal)
      case _          => throw new EvalError("reverse requires exactly 1 argument")

  @tailrec
  private def reverseList(lst: Value, acc: Value): Value = lst match
    case Value.NilVal               => acc
    case Value.PairVal(h, t, _)     => reverseList(t, Value.MutablePairVal(Array(h, acc)))
    case Value.MutablePairVal(cell) => reverseList(cell(1), Value.MutablePairVal(Array(cell(0), acc)))
    case _                          => throw new EvalError("reverse: not a proper list")

  def evalAssoc(args: List[Value]): Value =
    args match
      case key :: lst :: Nil => assocSearch(key, lst)
      case _                 => throw new EvalError("assoc requires exactly 2 arguments")

  @tailrec
  private def assocSearch(key: Value, lst: Value): Value =
    lst match
      case Value.NilVal => Value.BoolVal(false)
      case Value.PairVal(pair @ Value.PairVal(k, _, _), rest, _) =>
        if StringBuiltins.schemeEqual(key, k) then pair
        else assocSearch(key, rest)
      case Value.MutablePairVal(cell) =>
        val pair = cell(0)
        val k    = car(pair)
        if StringBuiltins.schemeEqual(key, k) then pair
        else assocSearch(key, cell(1))
      case _ => throw new EvalError("assoc: not a proper alist")
