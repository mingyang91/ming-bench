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
      val allElems = args.flatMap(a => PairOps.toScalaList(a))
      PairOps.makeList(allElems)

  def applyListRef(args: List[Expr]): Expr =
    if args.length != 2 then throw EvalError("list-ref: need exactly 2 arguments")
    val idx = Builtins.asNum(args(1)).toInt
    var cur = args(0)
    var i   = 0
    while i < idx do
      cur = PairOps.cdrOf(cur)
      i += 1
    PairOps.carOf(cur)

  def applyListTail(args: List[Expr]): Expr =
    if args.length != 2 then throw EvalError("list-tail: need exactly 2 arguments")
    val idx = Builtins.asNum(args(1)).toInt
    var cur = args(0)
    var i   = 0
    while i < idx do
      cur = PairOps.cdrOf(cur)
      i += 1
    cur

  def applyAssoc(args: List[Expr]): Expr =
    if args.length != 2 then throw EvalError("assoc: need exactly 2 arguments")
    val key   = args(0)
    val elems = PairOps.toScalaList(args(1))
    elems
      .find { elem =>
        val k = PairOps.carOf(elem)
        EqualityOps.schemeEqual(k, key)
      }
      .getOrElse(Expr.Bool(false))
