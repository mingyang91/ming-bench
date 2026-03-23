package ming

import Value.*

/** List utility built-in procedures: cadr/cddr family, assoc/member, list-ref/list-tail. */
object BuiltinsList:

  def register(env: Env): Unit =
    Builtins.define(env, listUtilOps ++ cxrOps)

  private def listUtilOps: List[(String, List[Value] => Value)] =
    List(
      (
        "list?",
        args =>
          if args.length != 1 then throw new EvalError("list?: expected 1 argument")
          BoolVal(isProperList(args.head))
      ),
      (
        "list-ref",
        args =>
          if args.length != 2 then throw new EvalError("list-ref: expected 2 arguments")
          args(1) match
            case IntVal(idx) => listRef(args(0), idx.toInt)
            case _           => throw new EvalError("list-ref: index must be a number")
      ),
      (
        "list-tail",
        args =>
          if args.length != 2 then throw new EvalError("list-tail: expected 2 arguments")
          args(1) match
            case IntVal(idx) => listTail(args(0), idx.toInt)
            case _           => throw new EvalError("list-tail: index must be a number")
      ),
      (
        "assoc",
        args =>
          if args.length != 2 then throw new EvalError("assoc: expected 2 arguments")
          assocLookup(args(0), args(1), Equality.equalCheck)
      ),
      (
        "assq",
        args =>
          if args.length != 2 then throw new EvalError("assq: expected 2 arguments")
          assocLookup(args(0), args(1), Equality.eqCheck)
      ),
      (
        "assv",
        args =>
          if args.length != 2 then throw new EvalError("assv: expected 2 arguments")
          assocLookup(args(0), args(1), Equality.eqvCheck)
      ),
      (
        "memq",
        args =>
          if args.length != 2 then throw new EvalError("memq: expected 2 arguments")
          memberLookup(args(0), args(1), Equality.eqCheck)
      ),
      (
        "memv",
        args =>
          if args.length != 2 then throw new EvalError("memv: expected 2 arguments")
          memberLookup(args(0), args(1), Equality.eqvCheck)
      ),
      (
        "member",
        args =>
          if args.length != 2 then throw new EvalError("member: expected 2 arguments")
          memberLookup(args(0), args(1), Equality.equalCheck)
      )
    )

  private def applyCxr(name: String, ops: String, v: Value): Value =
    var cur = v
    var i   = ops.length - 1
    while i >= 0 do
      cur match
        case PairVal(cell) =>
          cur = if ops(i) == 'a' then cell.car else cell.cdr
        case _ => throw new EvalError(s"$name: not a pair")
      i -= 1
    cur

  private def makeCxr(name: String): (String, List[Value] => Value) =
    val ops = name.substring(1, name.length - 1) // strip 'c' and 'r'
    (
      name,
      args =>
        if args.length != 1 then throw new EvalError(s"$name: expected 1 argument")
        applyCxr(name, ops, args.head)
    )

  private def cxrOps: List[(String, List[Value] => Value)] =
    val ads    = List("a", "d")
    val depth2 = for a <- ads; b <- ads yield s"c${a}${b}r"
    val depth3 = for a <- ads; b <- ads; c <- ads yield s"c${a}${b}${c}r"
    val depth4 = for a <- ads; b <- ads; c <- ads; d <- ads yield s"c${a}${b}${c}${d}r"
    (depth2 ++ depth3 ++ depth4).map(makeCxr)

  /** Cycle-safe proper list check using Floyd's tortoise-and-hare. */
  private def isProperList(v: Value): Boolean =
    var slow = v
    var fast = v
    while true do
      fast match
        case PairVal(c1) =>
          c1.cdr match
            case PairVal(c2) => fast = c2.cdr
            case NilVal      => return true
            case _           => return false
        case NilVal => return true
        case _      => return false
      slow match
        case PairVal(cs) => slow = cs.cdr
        case _           => return true
      (slow, fast) match
        case (PairVal(sc), PairVal(fc)) if sc eq fc => return false
        case _                                      => ()
    false

  private def listRef(lst: Value, idx: Int): Value =
    if idx < 0 then throw new EvalError("list-ref: index out of range")
    var cur = lst
    var i   = idx
    while i > 0 do
      cur match
        case PairVal(cell) => cur = cell.cdr; i -= 1
        case _             => throw new EvalError("list-ref: index out of range")
    cur match
      case PairVal(cell) => cell.car
      case _             => throw new EvalError("list-ref: index out of range")

  private def listTail(lst: Value, idx: Int): Value =
    if idx < 0 then throw new EvalError("list-tail: index out of range")
    var cur = lst
    var i   = idx
    while i > 0 do
      cur match
        case PairVal(cell) => cur = cell.cdr; i -= 1
        case _             => throw new EvalError("list-tail: index out of range")
    cur

  private def assocLookup(key: Value, alist: Value, cmp: (Value, Value) => Boolean): Value =
    var cur = alist
    while true do
      cur match
        case PairVal(outerCell) =>
          outerCell.car match
            case PairVal(innerCell) =>
              if cmp(key, innerCell.car) then return outerCell.car
              cur = outerCell.cdr
            case _ => throw new EvalError("assoc: not a proper alist")
        case NilVal => return BoolVal(false)
        case _      => throw new EvalError("assoc: not a proper alist")
    throw new AssertionError("unreachable")

  private def memberLookup(key: Value, lst: Value, cmp: (Value, Value) => Boolean): Value =
    var cur = lst
    while true do
      cur match
        case pv @ PairVal(cell) =>
          if cmp(key, cell.car) then return pv
          cur = cell.cdr
        case NilVal => return BoolVal(false)
        case _      => throw new EvalError("member: not a proper list")
    throw new AssertionError("unreachable")
