package ming

import Value.*

/** Built-in procedure definitions for the global environment. */
object Builtins:

  def register(env: Env, output: StringBuilder): Unit =
    registerArithmetic(env)
    registerListOps(env)
    registerListUtils(env)
    registerTypePredicates(env)
    registerIOOps(env, output)
    registerValues(env)
    BuiltinsExt.register(env)

  private[ming] def define(env: Env, entries: List[(String, List[Value] => Value)]): Unit =
    entries.foreach((name, fn) => env.define(name, BuiltinVal(name, fn)))

  private def registerArithmetic(env: Env): Unit =
    define(
      env,
      List(
        ("+", args => arith(args, 0L, _ + _)),
        ("*", args => arith(args, 1L, _ * _)),
        ("-", args => subtractOp(args)),
        ("/", args => divideOp(args)),
        ("<", args => compareOp(args, _ < _)),
        (">", args => compareOp(args, _ > _)),
        ("=", args => compareOp(args, _ == _)),
        ("<=", args => compareOp(args, _ <= _)),
        (">=", args => compareOp(args, _ >= _)),
        (
          "not",
          args =>
            if args.length != 1 then throw new EvalError("not: expected 1 argument")
            BoolVal(!args.head.isTruthy)
        )
      )
    )

  private def registerListOps(env: Env): Unit =
    define(
      env,
      List(
        (
          "cons",
          args =>
            if args.length != 2 then throw new EvalError("cons: expected 2 arguments")
            PairVal(args(0), args(1))
        ),
        (
          "car",
          args =>
            if args.length != 1 then throw new EvalError("car: expected 1 argument")
            args.head match
              case PairVal(car, _) => car
              case _               => throw new EvalError("car: not a pair")
        ),
        (
          "cdr",
          args =>
            if args.length != 1 then throw new EvalError("cdr: expected 1 argument")
            args.head match
              case PairVal(_, cdr) => cdr
              case _               => throw new EvalError("cdr: not a pair")
        ),
        (
          "null?",
          args =>
            if args.length != 1 then throw new EvalError("null?: expected 1 argument")
            BoolVal(args.head == NilVal)
        ),
        (
          "list",
          args => args.foldRight(NilVal: Value)((v, acc) => PairVal(v, acc))
        ),
        (
          "length",
          args =>
            if args.length != 1 then throw new EvalError("length: expected 1 argument")
            var count = 0L
            var cur   = args.head
            while cur match
                case PairVal(_, cdr) => count += 1; cur = cdr; true
                case NilVal          => false
                case _               => throw new EvalError("length: not a proper list")
            do ()
            IntVal(count)
        ),
        (
          "append",
          args =>
            args.foldRight(NilVal: Value) { (lst, acc) =>
              appendList(lst, acc)
            }
        ),
        (
          "reverse",
          args =>
            if args.length != 1 then throw new EvalError("reverse: expected 1 argument")
            var acc: Value = NilVal
            var cur        = args.head
            while cur match
                case PairVal(h, t) => acc = PairVal(h, acc); cur = t; true
                case NilVal        => false
                case _             => throw new EvalError("reverse: not a proper list")
            do ()
            acc
        ),
        (
          "eq?",
          args =>
            if args.length != 2 then throw new EvalError("eq?: expected 2 arguments")
            BoolVal(Equality.eqCheck(args(0), args(1)))
        ),
        (
          "eqv?",
          args =>
            if args.length != 2 then throw new EvalError("eqv?: expected 2 arguments")
            BoolVal(Equality.eqvCheck(args(0), args(1)))
        ),
        (
          "equal?",
          args =>
            if args.length != 2 then throw new EvalError("equal?: expected 2 arguments")
            BoolVal(Equality.equalCheck(args(0), args(1)))
        )
      )
    )

  private def registerListUtils(env: Env): Unit =
    define(
      env,
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
            assocLookup(args(0), args(1))
        )
      )
    )

  private def registerTypePredicates(env: Env): Unit =
    define(
      env,
      List(
        ("string?", args => typePred(args, _.isInstanceOf[StrVal])),
        ("number?", args => typePred(args, _.isInstanceOf[IntVal])),
        ("boolean?", args => typePred(args, _.isInstanceOf[BoolVal])),
        ("pair?", args => typePred(args, _.isInstanceOf[PairVal])),
        ("symbol?", args => typePred(args, _.isInstanceOf[SymbolVal])),
        ("char?", args => typePred(args, _.isInstanceOf[CharVal]))
      )
    )

  private def registerIOOps(env: Env, output: StringBuilder): Unit =
    define(
      env,
      List(
        (
          "display",
          args =>
            if args.length != 1 then throw new EvalError("display: expected 1 argument")
            output.append(args.head.displayNoQuotes)
            BoolVal(true)
        ),
        (
          "write",
          args =>
            if args.length != 1 then throw new EvalError("write: expected 1 argument")
            output.append(args.head.display)
            BoolVal(true)
        ),
        (
          "newline",
          args =>
            if args.nonEmpty then throw new EvalError("newline: expected 0 arguments")
            output.append("\n")
            BoolVal(true)
        )
      )
    )

  private def registerValues(env: Env): Unit =
    define(
      env,
      List(
        (
          "values",
          args =>
            args match
              case single :: Nil => single
              case _             => ValuesVal(args)
        )
      )
    )

  private def appendList(lst: Value, tail: Value): Value =
    lst match
      case NilVal        => tail
      case PairVal(h, t) => PairVal(h, appendList(t, tail))
      case _             => throw new EvalError("append: not a proper list")

  private def typePred(args: List[Value], pred: Value => Boolean): Value =
    if args.length != 1 then throw new EvalError("type predicate: expected 1 argument")
    BoolVal(pred(args.head))

  private[ming] def requireInts(args: List[Value]): List[Long] =
    args.map {
      case IntVal(n) => n
      case other     => throw new EvalError(s"expected number, got ${other.display}")
    }

  private def arith(args: List[Value], identity: Long, op: (Long, Long) => Long): Value =
    val nums = requireInts(args)
    IntVal(nums.foldLeft(identity)(op))

  private def subtractOp(args: List[Value]): Value =
    val nums = requireInts(args)
    nums match
      case Nil       => IntVal(0)
      case n :: Nil  => IntVal(-n)
      case n :: rest => IntVal(rest.foldLeft(n)(_ - _))

  private def divideOp(args: List[Value]): Value =
    val nums = requireInts(args)
    nums match
      case Nil       => throw new EvalError("/: need at least 1 argument")
      case n :: Nil  => IntVal(1 / n)
      case n :: rest => IntVal(rest.foldLeft(n)(_ / _))

  private def compareOp(args: List[Value], op: (Long, Long) => Boolean): Value =
    val nums   = requireInts(args)
    val result = nums.zip(nums.tail).forall((a, b) => op(a, b))
    BoolVal(result)

  private def isProperList(v: Value): Boolean =
    v match
      case NilVal          => true
      case PairVal(_, cdr) => isProperList(cdr)
      case _               => false

  private def listRef(lst: Value, idx: Int): Value =
    if idx < 0 then throw new EvalError("list-ref: index out of range")
    var cur = lst
    var i   = idx
    while i > 0 do
      cur match
        case PairVal(_, cdr) => cur = cdr; i -= 1
        case _               => throw new EvalError("list-ref: index out of range")
    cur match
      case PairVal(car, _) => car
      case _               => throw new EvalError("list-ref: index out of range")

  private def listTail(lst: Value, idx: Int): Value =
    if idx < 0 then throw new EvalError("list-tail: index out of range")
    var cur = lst
    var i   = idx
    while i > 0 do
      cur match
        case PairVal(_, cdr) => cur = cdr; i -= 1
        case _               => throw new EvalError("list-tail: index out of range")
    cur

  private def assocLookup(key: Value, alist: Value): Value =
    var cur = alist
    while true do
      cur match
        case PairVal(pair @ PairVal(k, _), rest) =>
          if Equality.equalCheck(key, k) then return pair
          cur = rest
        case NilVal => return BoolVal(false)
        case _      => throw new EvalError("assoc: not a proper alist")
    throw new AssertionError("unreachable")
