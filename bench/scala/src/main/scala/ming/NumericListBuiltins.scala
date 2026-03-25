package ming

private[ming] object NumericListBuiltins:

  def applyNumericPredicate(name: String, args: List[Expr]): Expr =
    Builtins.unary(name, args) {
      case Expr.Num(n) =>
        name match
          case "zero?"     => Expr.Bool(n == 0)
          case "positive?" => Expr.Bool(n > 0)
          case "negative?" => Expr.Bool(n < 0)
          case "odd?"      => Expr.Bool(n % 2 != 0)
          case "even?"     => Expr.Bool(n % 2 == 0)
          case _           => throw EvalError(s"$name: unknown predicate")
      case _ => throw EvalError(s"$name: not a number")
    }

  def applyModulo(args: List[Expr]): Expr =
    if args.length != 2 then throw EvalError("modulo: need exactly 2 arguments")
    val (a, b) = (Builtins.asNum(args(0)), Builtins.asNum(args(1)))
    if b == 0 then throw EvalError("modulo: division by zero")
    Expr.Num(Math.floorMod(a, b))

  def applyRemainder(args: List[Expr]): Expr =
    if args.length != 2 then throw EvalError("remainder: need exactly 2 arguments")
    val (a, b) = (Builtins.asNum(args(0)), Builtins.asNum(args(1)))
    if b == 0 then throw EvalError("remainder: division by zero")
    Expr.Num(a % b)

  def applyQuotient(args: List[Expr]): Expr =
    if args.length != 2 then throw EvalError("quotient: need exactly 2 arguments")
    val (a, b) = (Builtins.asNum(args(0)), Builtins.asNum(args(1)))
    if b == 0 then throw EvalError("quotient: division by zero")
    Expr.Num(a / b)

  def applyMinMax(name: String, args: List[Expr], better: (Long, Long) => Boolean): Expr =
    if args.isEmpty then throw EvalError(s"$name: need at least 1 argument")
    val nums = args.map(Builtins.asNum)
    Expr.Num(nums.reduceLeft((a, b) => if better(b, a) then b else a))

  def applyExpt(args: List[Expr]): Expr =
    if args.length != 2 then throw EvalError("expt: need exactly 2 arguments")
    val (base, exp) = (Builtins.asNum(args(0)), Builtins.asNum(args(1)))
    Expr.Num(math.pow(base.toDouble, exp.toDouble).toLong)

  def applyAppend(args: List[Expr]): Expr =
    if args.isEmpty then Expr.Lst(Nil)
    else
      val lists = args.map {
        case Expr.Lst(elems) => elems
        case other           => throw EvalError(s"append: not a list: ${Builtins.display(other)}")
      }
      Expr.Lst(lists.flatten)

  def applyListRef(args: List[Expr]): Expr =
    if args.length != 2 then throw EvalError("list-ref: need exactly 2 arguments")
    args match
      case List(Expr.Lst(elems), Expr.Num(idx)) =>
        if idx < 0 || idx >= elems.length then throw EvalError("list-ref: index out of range")
        elems(idx.toInt)
      case _ => throw EvalError("list-ref: invalid arguments")

  def applyListTail(args: List[Expr]): Expr =
    if args.length != 2 then throw EvalError("list-tail: need exactly 2 arguments")
    args match
      case List(Expr.Lst(elems), Expr.Num(idx)) =>
        if idx < 0 || idx > elems.length then throw EvalError("list-tail: index out of range")
        Expr.Lst(elems.drop(idx.toInt))
      case _ => throw EvalError("list-tail: invalid arguments")

  def isList(e: Expr): Boolean = e match
    case Expr.Lst(_)     => true
    case Expr.Pair(_, _) => false
    case _               => false

  def applyAssoc(args: List[Expr]): Expr =
    if args.length != 2 then throw EvalError("assoc: need exactly 2 arguments")
    val key = args(0)
    args(1) match
      case Expr.Lst(elems) =>
        elems
          .find {
            case Expr.Lst(k :: _) => EqualityOps.schemeEqual(k, key)
            case _                => false
          }
          .getOrElse(Expr.Bool(false))
      case _ => throw EvalError("assoc: not a list")
