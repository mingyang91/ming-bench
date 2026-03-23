package ming

import SchemeValue.*

/** Extended builtin operations: strings, numerics, chars, list utilities. */
object BuiltinsExt:

  def stringAppendOp(args: List[SchemeValue]): SchemeValue =
    val sb = StringBuilder()
    args.foreach {
      case StringVal(s, _) => sb.append(s)
      case _               => throw new EvalError("string-append: expected string")
    }
    StringVal(sb.toString)

  def stringLengthOp(args: List[SchemeValue]): SchemeValue =
    args match
      case StringVal(s, _) :: Nil         => IntVal(s.length.toLong)
      case MutableStringVal(cs, _) :: Nil => IntVal(cs.length.toLong)
      case _                              => throw new EvalError("string-length: expected string")

  def substringOp(args: List[SchemeValue]): SchemeValue =
    args match
      case StringVal(s, _) :: IntVal(start, _) :: IntVal(end, _) :: Nil =>
        StringVal(s.substring(start.toInt, end.toInt))
      case _ => throw new EvalError("substring: expected (string start end)")

  def stringToNumberOp(args: List[SchemeValue]): SchemeValue =
    args match
      case StringVal(s, _) :: Nil =>
        try IntVal(s.toLong)
        catch case _: NumberFormatException => BoolVal(false)
      case _ => throw new EvalError("string->number: expected string")

  def numberToStringOp(args: List[SchemeValue]): SchemeValue =
    args match
      case IntVal(n, _) :: Nil         => StringVal(n.toString)
      case RationalVal(n, d, _) :: Nil => StringVal(s"$n/$d")
      case DoubleVal(d, _) :: Nil      => StringVal(Rational.formatDouble(d))
      case _                           => throw new EvalError("number->string: expected number")

  def symbolToStringOp(args: List[SchemeValue]): SchemeValue =
    args match
      case SymbolVal(name, _) :: Nil => StringVal(name)
      case _                         => throw new EvalError("symbol->string: expected symbol")

  def stringToSymbolOp(args: List[SchemeValue]): SchemeValue =
    args match
      case StringVal(s, _) :: Nil => SymbolVal(s)
      case _                      => throw new EvalError("string->symbol: expected string")

  def stringRefOp(args: List[SchemeValue]): SchemeValue =
    args match
      case StringVal(s, _) :: IntVal(idx, _) :: Nil =>
        if idx < 0 || idx >= s.length then throw new EvalError("string-ref: index out of bounds")
        CharVal(s.charAt(idx.toInt))
      case MutableStringVal(cs, _) :: IntVal(idx, _) :: Nil =>
        if idx < 0 || idx >= cs.length then throw new EvalError("string-ref: index out of bounds")
        CharVal(cs(idx.toInt))
      case _ => throw new EvalError("string-ref: expected (string index)")

  def stringSetOp(args: List[SchemeValue]): SchemeValue =
    args match
      case MutableStringVal(cs, _) :: IntVal(idx, _) :: CharVal(c, _) :: Nil =>
        if idx < 0 || idx >= cs.length then throw new EvalError("string-set!: index out of bounds")
        cs(idx.toInt) = c
        Void
      case StringVal(_, _) :: _ :: _ :: Nil =>
        throw new EvalError("string-set!: string is immutable")
      case _ => throw new EvalError("string-set!: expected (mutable-string index char)")

  def stringCopyOp(args: List[SchemeValue]): SchemeValue =
    args match
      case StringVal(s, _) :: Nil         => MutableStringVal(s.toCharArray)
      case MutableStringVal(cs, _) :: Nil => MutableStringVal(cs.clone())
      case _                              => throw new EvalError("string-copy: expected string")

  def stringToListOp(args: List[SchemeValue]): SchemeValue =
    args match
      case StringVal(s, _) :: Nil =>
        ListVal(s.toList.map(c => CharVal(c)))
      case MutableStringVal(cs, _) :: Nil =>
        ListVal(cs.toList.map(c => CharVal(c)))
      case _ => throw new EvalError("string->list: expected string")

  def listToStringOp(args: List[SchemeValue]): SchemeValue =
    args match
      case lst :: Nil =>
        val elems = Builtins.toScalaList(lst)
        val chars = elems.map:
          case CharVal(c, _) => c
          case other         => throw new EvalError(s"list->string: expected char, got ${other.display}")
        StringVal(String(chars.toArray))
      case _ => throw new EvalError("list->string: expected list")

  def absOp(args: List[SchemeValue]): SchemeValue =
    args match
      case IntVal(n, _) :: Nil => IntVal(math.abs(n))
      case _                   => throw new EvalError("abs: expected 1 number")

  def moduloOp(args: List[SchemeValue]): SchemeValue =
    args match
      case IntVal(a, _) :: IntVal(b, _) :: Nil =>
        if b == 0 then throw new EvalError("modulo: division by zero")
        IntVal(Math.floorMod(a, b))
      case _ => throw new EvalError("modulo: expected 2 numbers")

  def remainderOp(args: List[SchemeValue]): SchemeValue =
    args match
      case IntVal(a, _) :: IntVal(b, _) :: Nil =>
        if b == 0 then throw new EvalError("remainder: division by zero")
        IntVal(a % b)
      case _ => throw new EvalError("remainder: expected 2 numbers")

  def quotientOp(args: List[SchemeValue]): SchemeValue =
    args match
      case IntVal(a, _) :: IntVal(b, _) :: Nil =>
        if b == 0 then throw new EvalError("quotient: division by zero")
        IntVal(a / b) // truncates toward zero in Scala
      case _ => throw new EvalError("quotient: expected 2 numbers")

  def minOp(args: List[SchemeValue]): SchemeValue =
    if args.isEmpty then throw new EvalError("min: requires at least 1 argument")
    IntVal(args.map {
      case IntVal(n, _) => n
      case _            => throw new EvalError("min: expected number")
    }.min)

  def maxOp(args: List[SchemeValue]): SchemeValue =
    if args.isEmpty then throw new EvalError("max: requires at least 1 argument")
    IntVal(args.map {
      case IntVal(n, _) => n
      case _            => throw new EvalError("max: expected number")
    }.max)

  def exptOp(args: List[SchemeValue]): SchemeValue =
    args match
      case IntVal(base, _) :: IntVal(exp, _) :: Nil =>
        if exp < 0 then throw new EvalError("expt: negative exponent")
        IntVal(math.pow(base.toDouble, exp.toDouble).toLong)
      case _ => throw new EvalError("expt: expected 2 numbers")

  // --- List utilities (L13) ---

  def listRefOp(args: List[SchemeValue]): SchemeValue =
    args match
      case lst :: IntVal(idx, _) :: Nil =>
        val elems = Builtins.toScalaList(lst)
        if idx < 0 || idx >= elems.length then throw new EvalError("list-ref: index out of bounds")
        elems(idx.toInt)
      case _ => throw new EvalError("list-ref: expected (list index)")

  def listTailOp(args: List[SchemeValue]): SchemeValue =
    args match
      case lst :: IntVal(idx, _) :: Nil =>
        var current = lst
        var i       = 0
        while i < idx.toInt do
          current = current match
            case MutablePairVal(cells) => cells(1)
            case ListVal(_ :: t, _)    => ListVal(t)
            case PairVal(_, cdr)       => cdr
            case _                     => throw new EvalError("list-tail: index out of bounds")
          i += 1
        current
      case _ => throw new EvalError("list-tail: expected (list index)")

  def listCheck(args: List[SchemeValue]): SchemeValue =
    args match
      case v :: Nil => BoolVal(isProperList(v))
      case _        => throw new EvalError("list?: requires 1 argument")

  /** Cycle-safe proper list check using tortoise-and-hare algorithm. */
  private def isProperList(v: SchemeValue): Boolean =
    var slow  = v
    var fast  = v
    var first = true
    while true do
      if first then first = false
      else slow = stepCdr(slow)
      fast = stepCdr(fast)
      fast match
        case ListVal(Nil, _)                => return true
        case ListVal(_, _)                  => return true
        case _: MutablePairVal | _: PairVal => // continue
        case _                              => return false
      fast = stepCdr(fast)
      fast match
        case ListVal(Nil, _)                => return true
        case ListVal(_, _)                  => return true
        case _: MutablePairVal | _: PairVal => // continue
        case _                              => return false
      // Check if slow == fast (cycle detected)
      (slow, fast) match
        case (MutablePairVal(a), MutablePairVal(b)) if a eq b => return false
        case _                                                => ()
    false // unreachable

  private def stepCdr(v: SchemeValue): SchemeValue = v match
    case MutablePairVal(cells) => cells(1)
    case PairVal(_, cdr)       => cdr
    case ListVal(_ :: tail, _) => ListVal(tail)
    case other                 => other

  def assocOp(args: List[SchemeValue]): SchemeValue =
    args match
      case key :: lst :: Nil =>
        val elems = Builtins.toScalaList(lst)
        elems
          .collectFirst {
            case entry if isPairLike(entry) && Builtins.schemeEqual(key, pairCar(entry)) => entry
          }
          .getOrElse(BoolVal(false))
      case _ => throw new EvalError("assoc: expected (key alist)")

  private def isPairLike(v: SchemeValue): Boolean = v match
    case MutablePairVal(_)  => true
    case PairVal(_, _)      => true
    case ListVal(_ :: _, _) => true
    case _                  => false

  private def pairCar(v: SchemeValue): SchemeValue = v match
    case MutablePairVal(cells) => cells(0)
    case PairVal(car, _)       => car
    case ListVal(h :: _, _)    => h
    case _                     => throw new EvalError("car: not a pair")

  def mapOp(args: List[SchemeValue]): SchemeValue =
    if args.length < 2 then throw new EvalError("map: requires at least 2 arguments")
    val proc  = args.head
    val lists = args.tail.map(Builtins.toScalaList)
    val len   = lists.head.length
    if !lists.forall(_.length == len) then throw new EvalError("map: lists must have same length")
    val result = (0 until len).toList.map { i =>
      val elemArgs = lists.map(_(i))
      ProcApply.applyProc(proc, elemArgs)
    }
    schemeListFromScala(result)

  def forEachOp(args: List[SchemeValue]): SchemeValue =
    if args.length < 2 then throw new EvalError("for-each: requires at least 2 arguments")
    val proc  = args.head
    val lists = args.tail.map(Builtins.toScalaList)
    val len   = lists.head.length
    if !lists.forall(_.length == len) then throw new EvalError("for-each: lists must have same length")
    var i = 0
    while i < len do
      val elemArgs = lists.map(_(i))
      ProcApply.applyProc(proc, elemArgs)
      i += 1
    Void

  def reverseOp(args: List[SchemeValue]): SchemeValue =
    args match
      case v :: Nil =>
        val elems = Builtins.toScalaList(v)
        schemeListFromScala(elems.reverse)
      case _ => throw new EvalError("reverse: expected 1 list")

  /** Build a mutable-pair chain from a Scala list. */
  private[ming] def schemeListFromScala(elems: List[SchemeValue]): SchemeValue =
    elems.foldRight(ListVal(Nil): SchemeValue)((e, acc) => MutablePairVal(Array(e, acc)))

  def memberOp(args: List[SchemeValue]): SchemeValue =
    args match
      case obj :: lst :: Nil =>
        var current = lst
        while true do
          current match
            case MutablePairVal(cells) =>
              if Builtins.schemeEqual(obj, cells(0)) then return current
              current = cells(1)
            case ListVal(Nil, _) => return BoolVal(false)
            case ListVal(h :: t, _) =>
              if Builtins.schemeEqual(obj, h) then return current
              current = ListVal(t)
            case _ => return BoolVal(false)
        BoolVal(false)
      case _ => throw new EvalError("member: requires 2 arguments")
