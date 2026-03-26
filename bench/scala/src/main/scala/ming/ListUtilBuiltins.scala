package ming

import SchemeTypes.{errAt, isProperList, pairToScalaList, schemeList, valuesEqual, Env, PairCell, Pos, Value}

private[ming] object ListUtilBuiltins:

  def applyListUtils(
    name: String,
    args: List[Value],
    pos: Pos,
    env: Env
  ): Value = name match
    case "list-ref"  => applyListRef(args, pos)
    case "list-tail" => applyListTail(args, pos)
    case "list?" =>
      if args.length != 1 then throw errAt(pos, "list? requires 1 argument")
      Value.VBool(isProperList(args.head))
    case "assoc" =>
      if args.length != 2 then throw errAt(pos, "assoc requires 2 arguments")
      val alist = pairToScalaList(args(1), pos)
      alist
        .collectFirst {
          case v @ Value.VPair(cell) if valuesEqual(cell.car, args(0)) => v
          case v @ Value.VList(key :: _) if valuesEqual(key, args(0))  => v
        }
        .getOrElse(Value.VBool(false))
    case "assv" =>
      if args.length != 2 then throw errAt(pos, "assv requires 2 arguments")
      val alist = pairToScalaList(args(1), pos)
      alist
        .collectFirst {
          case v @ Value.VPair(cell) if eqvCheck(cell.car, args(0)) => v
          case v @ Value.VList(key :: _) if eqvCheck(key, args(0))  => v
        }
        .getOrElse(Value.VBool(false))
    case "member" =>
      if args.length != 2 then throw errAt(pos, "member requires 2 arguments")
      findInList(args(1), v => valuesEqual(v, args(0)), pos)
    case "memq" =>
      if args.length != 2 then throw errAt(pos, "memq requires 2 arguments")
      findInList(args(1), v => eqCheck(v, args(0)), pos)
    case "memv" =>
      if args.length != 2 then throw errAt(pos, "memv requires 2 arguments")
      findInList(args(1), v => eqvCheck(v, args(0)), pos)
    case "assq" =>
      if args.length != 2 then throw errAt(pos, "assq requires 2 arguments")
      val alist = pairToScalaList(args(1), pos)
      alist
        .collectFirst {
          case v @ Value.VPair(cell) if eqCheck(cell.car, args(0)) => v
          case v @ Value.VList(key :: _) if eqCheck(key, args(0))  => v
        }
        .getOrElse(Value.VBool(false))
    case "map" =>
      if args.length < 2 then throw errAt(pos, "map requires at least 2 arguments")
      val func  = args.head
      val lists = args.tail.map(a => pairToScalaList(a, pos))
      val len   = lists.head.length
      val result = (0 until len).toList.map { i =>
        val mapArgs = lists.map(_(i))
        ApplyFunc(func, mapArgs, pos, env)
      }
      schemeList(result)
    case "for-each" =>
      if args.length < 2 then throw errAt(pos, "for-each requires at least 2 arguments")
      val func  = args.head
      val lists = args.tail.map(a => pairToScalaList(a, pos))
      val len   = lists.head.length
      (0 until len).foreach { i =>
        val fArgs = lists.map(_(i))
        ApplyFunc(func, fArgs, pos, env)
      }
      Value.VVoid
    case "equal?" =>
      if args.length != 2 then throw errAt(pos, "equal? requires 2 arguments")
      Value.VBool(valuesEqual(args(0), args(1)))
    case "eq?" =>
      if args.length != 2 then throw errAt(pos, "eq? requires 2 arguments")
      val result = (args(0), args(1)) match
        case (Value.VPair(a), Value.VPair(b))     => a eq b
        case (Value.VSymbol(a), Value.VSymbol(b)) => a == b
        case (Value.VNum(a), Value.VNum(b))       => a == b
        case (Value.VBool(a), Value.VBool(b))     => a == b
        case (Value.VChar(a), Value.VChar(b))     => a == b
        case (Value.VList(Nil), Value.VList(Nil)) => true
        case (a, b)                               => a eq b
      Value.VBool(result)
    case "eqv?" =>
      if args.length != 2 then throw errAt(pos, "eqv? requires 2 arguments")
      Value.VBool(eqvCheck(args(0), args(1)))
    case _ => throw errAt(pos, s"unknown list util: $name")

  private def applyListRef(args: List[Value], pos: Pos): Value =
    if args.length != 2 then throw errAt(pos, "list-ref requires 2 arguments")
    val idx = args(1) match
      case Value.VNum(i) => i.toInt
      case _             => throw errAt(pos, "list-ref: index must be a number")
    args(0) match
      case Value.VList(elems) => elems(idx)
      case Value.VPair(_) =>
        var cur = args(0)
        var i   = 0
        while i < idx do
          cur = cur match
            case Value.VPair(c)      => c.cdr
            case Value.VList(_ :: t) => Value.VList(t)
            case _                   => throw errAt(pos, "list-ref: index out of range")
          i += 1
        cur match
          case Value.VPair(c)      => c.car
          case Value.VList(h :: _) => h
          case _                   => throw errAt(pos, "list-ref: index out of range")
      case _ => throw errAt(pos, "list-ref: invalid arguments")

  private def applyListTail(args: List[Value], pos: Pos): Value =
    if args.length != 2 then throw errAt(pos, "list-tail requires 2 arguments")
    val idx = args(1) match
      case Value.VNum(i) => i.toInt
      case _             => throw errAt(pos, "list-tail: index must be a number")
    var cur = args(0)
    var i   = 0
    while i < idx do
      cur = cur match
        case Value.VPair(c)      => c.cdr
        case Value.VList(_ :: t) => Value.VList(t)
        case _                   => throw errAt(pos, "list-tail: index out of range")
      i += 1
    cur

  private def eqCheck(a: Value, b: Value): Boolean = (a, b) match
    case (Value.VPair(x), Value.VPair(y))     => x eq y
    case (Value.VSymbol(x), Value.VSymbol(y)) => x == y
    case (Value.VNum(x), Value.VNum(y))       => x == y
    case (Value.VBool(x), Value.VBool(y))     => x == y
    case (Value.VChar(x), Value.VChar(y))     => x == y
    case (Value.VList(Nil), Value.VList(Nil)) => true
    case (x, y)                               => x eq y

  private[ming] def eqvCheck(a: Value, b: Value): Boolean = (a, b) match
    case (Value.VPair(x), Value.VPair(y))                   => x eq y
    case (Value.VSymbol(x), Value.VSymbol(y))               => x == y
    case (Value.VNum(x), Value.VNum(y))                     => x == y
    case (Value.VFloat(x), Value.VFloat(y))                 => x == y
    case (Value.VRational(n1, d1), Value.VRational(n2, d2)) => n1 == n2 && d1 == d2
    case (Value.VBool(x), Value.VBool(y))                   => x == y
    case (Value.VChar(x), Value.VChar(y))                   => x == y
    case (Value.VList(Nil), Value.VList(Nil))               => true
    case (x, y)                                             => x eq y

  /** Search for element in a list, return the tail starting from match or #f. */
  private def findInList(lst: Value, pred: Value => Boolean, pos: Pos): Value =
    var cur  = lst
    val seen = new java.util.IdentityHashMap[PairCell, java.lang.Boolean]()
    while true do
      cur match
        case Value.VList(Nil) => return Value.VBool(false)
        case Value.VPair(cell) =>
          if seen.containsKey(cell) then return Value.VBool(false)
          seen.put(cell, java.lang.Boolean.TRUE)
          if pred(cell.car) then return cur
          cur = cell.cdr
        case Value.VList(elems) =>
          val idx = elems.indexWhere(pred)
          if idx >= 0 then return schemeList(elems.drop(idx))
          else return Value.VBool(false)
        case _ => return Value.VBool(false)
    Value.VBool(false)
