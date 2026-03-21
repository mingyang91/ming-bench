package ming

/** Level 15 numeric, character, and string utility builtins. */
object NumCharOps:

  def callPure(name: String, args: List[Value]): Value =
    name match
      case "abs"              => absOp(args)
      case "modulo"           => moduloOp(args)
      case "remainder"        => remainderOp(args)
      case "quotient"         => quotientOp(args)
      case "min"              => minMaxOp(args, _ < _)
      case "max"              => minMaxOp(args, _ > _)
      case "expt"             => exptOp(args)
      case "zero?"            => numPred(args, "zero?", _ == 0)
      case "positive?"        => numPred(args, "positive?", _ > 0)
      case "negative?"        => numPred(args, "negative?", _ < 0)
      case "odd?"             => numPred(args, "odd?", n => n % 2 != 0)
      case "even?"            => numPred(args, "even?", n => n % 2 == 0)
      case "list-ref"         => listRefOp(args)
      case "list-tail"        => listTailOp(args)
      case "list?"            => listPred(args)
      case "assoc"            => assocOp(args)
      case "char-alphabetic?" => charPred(args, "char-alphabetic?", _.isLetter)
      case "char-numeric?"    => charPred(args, "char-numeric?", _.isDigit)
      case "char-upcase"      => charXform(args, "char-upcase", _.toUpper)
      case "char-downcase"    => charXform(args, "char-downcase", _.toLower)
      case "char=?"           => charCmp(args, "char=?", _ == _)
      case "char<?"           => charCmp(args, "char<?", _ < _)
      case "string=?"         => strCmp(args, "string=?", _ == _)
      case "string<?"         => strCmp(args, "string<?", (a, b) => a.compareTo(b) < 0)
      case "string-ci=?"      => strCiEq(args)
      case "string-upcase"    => strCase(args, "string-upcase", _.toUpperCase)
      case "string-downcase"  => strCase(args, "string-downcase", _.toLowerCase)
      case _                  => throw new EvalError(s"unknown procedure: $name")

  private def asLong(v: Value): Long = v match
    case Value.Integer(n) => n
    case _                => throw new EvalError(s"expected number, got: ${v.display}")

  private def asChar(v: Value): Char = v match
    case Value.Char(c) => c
    case _             => throw new EvalError(s"expected char, got: ${v.display}")

  private def asStr(v: Value): String = v match
    case Value.Str(s)        => s
    case Value.MutStr(chars) => new String(chars)
    case _                   => throw new EvalError(s"expected string, got: ${v.display}")

  private def absOp(args: List[Value]): Value = args match
    case v :: Nil => Value.Integer(math.abs(asLong(v)))
    case _        => throw new EvalError("abs requires 1 argument")

  private def moduloOp(args: List[Value]): Value = args match
    case a :: b :: Nil =>
      val (x, y) = (asLong(a), asLong(b))
      if y == 0 then throw new EvalError("modulo: division by zero")
      else Value.Integer(math.floorMod(x, y))
    case _ => throw new EvalError("modulo requires 2 arguments")

  private def remainderOp(args: List[Value]): Value = args match
    case a :: b :: Nil =>
      val (x, y) = (asLong(a), asLong(b))
      if y == 0 then throw new EvalError("remainder: division by zero")
      else Value.Integer(x % y)
    case _ => throw new EvalError("remainder requires 2 arguments")

  private def quotientOp(args: List[Value]): Value = args match
    case a :: b :: Nil =>
      val (x, y) = (asLong(a), asLong(b))
      if y == 0 then throw new EvalError("quotient: division by zero")
      else Value.Integer(x / y)
    case _ => throw new EvalError("quotient requires 2 arguments")

  private def minMaxOp(args: List[Value], better: (Long, Long) => Boolean): Value =
    if args.isEmpty then throw new EvalError("min/max requires at least 1 argument")
    val nums = args.map(asLong)
    Value.Integer(nums.tail.foldLeft(nums.head)((a, b) => if better(b, a) then b else a))

  private def exptOp(args: List[Value]): Value = args match
    case base :: exp :: Nil =>
      Value.Integer(pow(asLong(base), asLong(exp)))
    case _ => throw new EvalError("expt requires 2 arguments")

  @scala.annotation.tailrec
  private def pow(base: Long, exp: Long, acc: Long = 1): Long =
    if exp <= 0 then acc else pow(base, exp - 1, acc * base)

  private def numPred(args: List[Value], name: String, pred: Long => Boolean): Value =
    args match
      case v :: Nil => Value.Bool(pred(asLong(v)))
      case _        => throw new EvalError(s"$name requires 1 argument")

  private def listRefOp(args: List[Value]): Value = args match
    case lst :: Value.Integer(idx) :: Nil =>
      val elems = toScalaList(lst).getOrElse(throw new EvalError("list-ref: not a list"))
      if idx < 0 || idx >= elems.length then throw new EvalError("list-ref: index out of bounds")
      else elems(idx.toInt)
    case _ => throw new EvalError("list-ref requires list and index")

  private def listTailOp(args: List[Value]): Value = args match
    case lst :: Value.Integer(idx) :: Nil =>
      @scala.annotation.tailrec
      def drop(v: Value, n: Long): Value =
        if n <= 0 then v
        else
          v match
            case Value.Pair(c)       => drop(c(1), n - 1)
            case Value.SList(_ :: t) => drop(Value.SList(t), n - 1)
            case _                   => throw new EvalError("list-tail: index out of bounds")
      drop(lst, idx)
    case _ => throw new EvalError("list-tail requires list and index")

  private def listPred(args: List[Value]): Value = args match
    case v :: Nil => Value.Bool(isProperList(v))
    case _        => throw new EvalError("list? requires 1 argument")

  private def isProperList(v: Value): Boolean =
    @scala.annotation.tailrec
    def check(slow: Value, fast: Value): Boolean = fast match
      case Value.SList(Nil) => true
      case Value.Pair(fc) =>
        fc(1) match
          case Value.SList(Nil) => true
          case fp2 @ Value.Pair(fc2) =>
            val nextSlow = slow match
              case Value.Pair(sc) => sc(1)
              case _              => slow
            if fp2 eq nextSlow then false
            else check(nextSlow, fc2(1))
          case _ => false
      case _ => false
    v match
      case Value.SList(_) => true
      case p @ Value.Pair(c) =>
        c(1) match
          case Value.SList(Nil) => true
          case fp @ Value.Pair(_) =>
            if fp eq p then false
            else check(v, fp)
          case _ => false
      case _ => false

  private[ming] def toScalaList(v: Value): Option[List[Value]] =
    @scala.annotation.tailrec
    def collect(cur: Value, acc: List[Value]): Option[List[Value]] = cur match
      case Value.SList(Nil)   => Some(acc.reverse)
      case Value.SList(elems) => Some(acc.reverse ++ elems)
      case Value.Pair(c)      => collect(c(1), c(0) :: acc)
      case _                  => None
    collect(v, Nil)

  private def assocOp(args: List[Value]): Value = args match
    case key :: lst :: Nil =>
      val alist = toScalaList(lst).getOrElse(throw new EvalError("assoc: not a list"))
      alist
        .collectFirst {
          case pair @ Value.SList(k :: _) if Builtins.deepEqual(key, k) => pair
          case pair @ Value.Pair(c) if Builtins.deepEqual(key, c(0))    => pair
        }
        .getOrElse(Value.Bool(false))
    case _ => throw new EvalError("assoc requires key and list")

  private def charPred(args: List[Value], name: String, pred: Char => Boolean): Value =
    args match
      case v :: Nil => Value.Bool(pred(asChar(v)))
      case _        => throw new EvalError(s"$name requires 1 argument")

  private def charXform(args: List[Value], name: String, f: Char => Char): Value =
    args match
      case v :: Nil => Value.Char(f(asChar(v)))
      case _        => throw new EvalError(s"$name requires 1 argument")

  private def charCmp(args: List[Value], name: String, op: (Char, Char) => Boolean): Value =
    args match
      case a :: b :: Nil => Value.Bool(op(asChar(a), asChar(b)))
      case _             => throw new EvalError(s"$name requires 2 arguments")

  private def strCmp(args: List[Value], name: String, op: (String, String) => Boolean): Value =
    args match
      case a :: b :: Nil => Value.Bool(op(asStr(a), asStr(b)))
      case _             => throw new EvalError(s"$name requires 2 arguments")

  private def strCiEq(args: List[Value]): Value = args match
    case a :: b :: Nil => Value.Bool(asStr(a).equalsIgnoreCase(asStr(b)))
    case _             => throw new EvalError("string-ci=? requires 2 arguments")

  private def strCase(args: List[Value], name: String, f: String => String): Value =
    args match
      case v :: Nil => Value.Str(f(asStr(v)))
      case _        => throw new EvalError(s"$name requires 1 argument")
