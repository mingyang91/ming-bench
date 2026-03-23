package ming

import Value.*

/** Built-in procedure definitions for the global environment. */
object Builtins:

  def register(env: Env, output: StringBuilder): Unit =
    registerArithmetic(env)
    registerListOps(env)
    BuiltinsList.register(env)
    registerPairMutation(env)
    registerMisc(env)
    BuiltinsArith.registerTypePredicates(env)
    registerIOOps(env, output)
    registerValues(env)
    BuiltinsExt.register(env)

  private[ming] def define(env: Env, entries: List[(String, List[Value] => Value)]): Unit =
    entries.foreach((name, fn) => env.define(name, BuiltinVal(name, fn)))

  private def registerArithmetic(env: Env): Unit =
    import BuiltinsArith.*
    define(
      env,
      List(
        ("+", args => arithAdd(args)),
        ("*", args => arithMul(args)),
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
            Pair(args(0), args(1))
        ),
        (
          "car",
          args =>
            if args.length != 1 then throw new EvalError("car: expected 1 argument")
            args.head match
              case PairVal(cell) => cell.car
              case _             => throw new EvalError("car: not a pair")
        ),
        (
          "cdr",
          args =>
            if args.length != 1 then throw new EvalError("cdr: expected 1 argument")
            args.head match
              case PairVal(cell) => cell.cdr
              case _             => throw new EvalError("cdr: not a pair")
        ),
        (
          "null?",
          args =>
            if args.length != 1 then throw new EvalError("null?: expected 1 argument")
            BoolVal(args.head == NilVal)
        ),
        (
          "list",
          args => args.foldRight(NilVal: Value)((v, acc) => Pair(v, acc))
        ),
        (
          "length",
          args =>
            if args.length != 1 then throw new EvalError("length: expected 1 argument")
            var count = 0L
            var cur   = args.head
            while cur match
                case PairVal(cell) => count += 1; cur = cell.cdr; true
                case NilVal        => false
                case _             => throw new EvalError("length: not a proper list")
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
                case PairVal(cell) => acc = Pair(cell.car, acc); cur = cell.cdr; true
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

  private def registerPairMutation(env: Env): Unit =
    define(
      env,
      List(
        (
          "set-car!",
          args =>
            if args.length != 2 then throw new EvalError("set-car!: expected 2 arguments")
            args(0) match
              case PairVal(cell) => cell.setCar(args(1)); VoidVal
              case _             => throw new EvalError("set-car!: not a pair")
        ),
        (
          "set-cdr!",
          args =>
            if args.length != 2 then throw new EvalError("set-cdr!: expected 2 arguments")
            args(0) match
              case PairVal(cell) => cell.setCdr(args(1)); VoidVal
              case _             => throw new EvalError("set-cdr!: not a pair")
        )
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

  private def registerMisc(env: Env): Unit =
    define(
      env,
      List(
        (
          "error",
          args =>
            val msg = args.map(_.displayNoQuotes).mkString(" ")
            throw new EvalError(s"error: $msg")
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
      case PairVal(cell) => Pair(cell.car, appendList(cell.cdr, tail))
      case _             => throw new EvalError("append: not a proper list")
