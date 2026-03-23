package ming

import Value.*

/** Built-in procedure definitions for the global environment. */
object Builtins:

  def register(env: Env, output: StringBuilder): Unit =
    registerArithmetic(env)
    registerListOps(env)
    registerStringOps(env, output)

  private def define(env: Env, entries: List[(String, List[Value] => Value)]): Unit =
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
        ("string?", args => typePred(args, _.isInstanceOf[StrVal])),
        ("number?", args => typePred(args, _.isInstanceOf[IntVal])),
        ("boolean?", args => typePred(args, _.isInstanceOf[BoolVal])),
        ("pair?", args => typePred(args, _.isInstanceOf[PairVal])),
        ("symbol?", args => typePred(args, _.isInstanceOf[SymbolVal])),
        ("char?", args => typePred(args, _.isInstanceOf[CharVal]))
      )
    )

  private def registerStringOps(env: Env, output: StringBuilder): Unit =
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
        ),
        (
          "string-append",
          args =>
            val strs = args.map {
              case StrVal(s) => s
              case other     => throw new EvalError(s"string-append: not a string: ${other.display}")
            }
            StrVal(strs.mkString)
        ),
        (
          "string-length",
          args =>
            if args.length != 1 then throw new EvalError("string-length: expected 1 argument")
            args.head match
              case StrVal(s) => IntVal(s.length.toLong)
              case other     => throw new EvalError(s"string-length: not a string: ${other.display}")
        ),
        (
          "substring",
          args =>
            args match
              case StrVal(s) :: IntVal(start) :: IntVal(end) :: Nil =>
                StrVal(s.substring(start.toInt, end.toInt))
              case _ => throw new EvalError("substring: expected (string, start, end)")
        ),
        (
          "string->number",
          args =>
            if args.length != 1 then throw new EvalError("string->number: expected 1 argument")
            args.head match
              case StrVal(s) =>
                try IntVal(s.toLong)
                catch case _: NumberFormatException => BoolVal(false)
              case other => throw new EvalError(s"string->number: not a string: ${other.display}")
        ),
        (
          "number->string",
          args =>
            if args.length != 1 then throw new EvalError("number->string: expected 1 argument")
            args.head match
              case IntVal(n) => StrVal(n.toString)
              case other     => throw new EvalError(s"number->string: not a number: ${other.display}")
        ),
        (
          "symbol->string",
          args =>
            if args.length != 1 then throw new EvalError("symbol->string: expected 1 argument")
            args.head match
              case SymbolVal(name) => StrVal(name)
              case other           => throw new EvalError(s"symbol->string: not a symbol: ${other.display}")
        ),
        (
          "string->symbol",
          args =>
            if args.length != 1 then throw new EvalError("string->symbol: expected 1 argument")
            args.head match
              case StrVal(s) => SymbolVal(s)
              case other     => throw new EvalError(s"string->symbol: not a string: ${other.display}")
        ),
        (
          "string-ref",
          args =>
            args match
              case StrVal(s) :: IntVal(idx) :: Nil => CharVal(s.charAt(idx.toInt))
              case _                               => throw new EvalError("string-ref: expected (string, index)")
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

  private def requireInts(args: List[Value]): List[Long] =
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
