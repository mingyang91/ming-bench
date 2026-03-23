package ming

import Value.*

/** Extended built-in procedures: strings, numbers, chars. */
object BuiltinsExt:

  private def define(env: Env, entries: List[(String, List[Value] => Value)]): Unit =
    Builtins.define(env, entries)

  def register(env: Env): Unit =
    registerStringOps(env)
    registerNumericUtils(env)
    BuiltinsExtra.register(env)
    BuiltinsVector.register(env)
    BuiltinsChar.register(env)

  private def registerStringOps(env: Env): Unit =
    define(env, stringConversionOps ++ stringMutationOps)

  private def stringConversionOps: List[(String, List[Value] => Value)] =
    List(
      (
        "string-append",
        args =>
          val strs = args.map {
            case StrVal(chars) => new String(chars)
            case other         => throw new EvalError(s"string-append: not a string: ${other.display}")
          }
          StrVal(strs.mkString.toCharArray)
      ),
      (
        "string-length",
        args =>
          if args.length != 1 then throw new EvalError("string-length: expected 1 argument")
          args.head match
            case StrVal(chars) => IntVal(chars.length.toLong)
            case other         => throw new EvalError(s"string-length: not a string: ${other.display}")
      ),
      (
        "substring",
        args =>
          args match
            case StrVal(chars) :: IntVal(start) :: IntVal(end) :: Nil =>
              StrVal(java.util.Arrays.copyOfRange(chars, start.toInt, end.toInt))
            case _ => throw new EvalError("substring: expected (string, start, end)")
      ),
      (
        "string->number",
        args =>
          if args.length != 1 then throw new EvalError("string->number: expected 1 argument")
          args.head match
            case StrVal(chars) =>
              val s = new String(chars)
              try IntVal(s.toLong)
              catch case _: NumberFormatException => BoolVal(false)
            case other => throw new EvalError(s"string->number: not a string: ${other.display}")
      ),
      (
        "number->string",
        args =>
          if args.length != 1 then throw new EvalError("number->string: expected 1 argument")
          args.head match
            case IntVal(n) => StrVal(n.toString.toCharArray)
            case other     => throw new EvalError(s"number->string: not a number: ${other.display}")
      ),
      (
        "symbol->string",
        args =>
          if args.length != 1 then throw new EvalError("symbol->string: expected 1 argument")
          args.head match
            case SymbolVal(name) => StrVal(name.toCharArray)
            case other           => throw new EvalError(s"symbol->string: not a symbol: ${other.display}")
      ),
      (
        "string->symbol",
        args =>
          if args.length != 1 then throw new EvalError("string->symbol: expected 1 argument")
          args.head match
            case StrVal(chars) => SymbolVal(new String(chars))
            case other         => throw new EvalError(s"string->symbol: not a string: ${other.display}")
      ),
      (
        "char->integer",
        args =>
          if args.length != 1 then throw new EvalError("char->integer: expected 1 argument")
          args.head match
            case CharVal(c) => IntVal(c.toLong)
            case _          => throw new EvalError("char->integer: not a char")
      ),
      (
        "integer->char",
        args =>
          if args.length != 1 then throw new EvalError("integer->char: expected 1 argument")
          args.head match
            case IntVal(n) => CharVal(n.toChar)
            case _         => throw new EvalError("integer->char: not a number")
      )
    )

  private def stringMutationOps: List[(String, List[Value] => Value)] =
    List(
      (
        "string-ref",
        args =>
          args match
            case StrVal(chars) :: IntVal(idx) :: Nil => CharVal(chars(idx.toInt))
            case _                                   => throw new EvalError("string-ref: expected (string, index)")
      ),
      (
        "string-copy",
        args =>
          if args.length != 1 then throw new EvalError("string-copy: expected 1 argument")
          args.head match
            case StrVal(chars) =>
              val copy = chars.clone()
              Value.markStringMutable(copy)
              StrVal(copy)
            case other => throw new EvalError(s"string-copy: not a string: ${other.display}")
      ),
      (
        "string-set!",
        args =>
          if args.length != 3 then throw new EvalError("string-set!: expected 3 arguments")
          args match
            case StrVal(chars) :: IntVal(idx) :: CharVal(c) :: Nil =>
              if !Value.isStringMutable(chars) then throw new EvalError("string-set!: strings are immutable")
              val i = idx.toInt
              if i < 0 || i >= chars.length then
                throw new EvalError(s"string-set!: index $i out of range [0, ${chars.length})")
              chars(i) = c
              NilVal
            case _ => throw new EvalError("string-set!: expected (string, index, char)")
      ),
      (
        "string->list",
        args =>
          if args.length != 1 then throw new EvalError("string->list: expected 1 argument")
          args.head match
            case StrVal(chars) =>
              chars.foldRight(NilVal: Value)((c, acc) => Pair(CharVal(c), acc))
            case other => throw new EvalError(s"string->list: not a string: ${other.display}")
      ),
      (
        "list->string",
        args =>
          if args.length != 1 then throw new EvalError("list->string: expected 1 argument")
          val chars = EvalHelpers.valueToList(args.head).map {
            case CharVal(c) => c
            case other      => throw new EvalError(s"list->string: not a char: ${other.display}")
          }
          StrVal(chars.toArray)
      )
    )

  private def registerNumericUtils(env: Env): Unit =
    define(
      env,
      List(
        (
          "abs",
          args =>
            if args.length != 1 then throw new EvalError("abs: expected 1 argument")
            args.head match
              case IntVal(n) => IntVal(Math.abs(n))
              case _         => throw new EvalError("abs: not a number")
        ),
        (
          "modulo",
          args =>
            if args.length != 2 then throw new EvalError("modulo: expected 2 arguments")
            val (a, b) = requireTwoInts(args)
            IntVal(Math.floorMod(a, b))
        ),
        (
          "remainder",
          args =>
            if args.length != 2 then throw new EvalError("remainder: expected 2 arguments")
            val (a, b) = requireTwoInts(args)
            IntVal(a % b)
        ),
        (
          "quotient",
          args =>
            if args.length != 2 then throw new EvalError("quotient: expected 2 arguments")
            val (a, b) = requireTwoInts(args)
            IntVal(a / b)
        ),
        (
          "min",
          args =>
            val nums = BuiltinsArith.requireInts(args)
            if nums.isEmpty then throw new EvalError("min: expected at least 1 argument")
            IntVal(nums.min)
        ),
        (
          "max",
          args =>
            val nums = BuiltinsArith.requireInts(args)
            if nums.isEmpty then throw new EvalError("max: expected at least 1 argument")
            IntVal(nums.max)
        ),
        (
          "expt",
          args =>
            if args.length != 2 then throw new EvalError("expt: expected 2 arguments")
            val (base, exp) = requireTwoInts(args)
            var result      = 1L
            var i           = 0L
            while i < exp do
              result *= base; i += 1
            IntVal(result)
        ),
        (
          "zero?",
          args =>
            if args.length != 1 then throw new EvalError("zero?: expected 1 argument")
            args.head match
              case IntVal(n) => BoolVal(n == 0)
              case _         => throw new EvalError("zero?: not a number")
        ),
        (
          "positive?",
          args =>
            if args.length != 1 then throw new EvalError("positive?: expected 1 argument")
            args.head match
              case IntVal(n) => BoolVal(n > 0)
              case _         => throw new EvalError("positive?: not a number")
        ),
        (
          "negative?",
          args =>
            if args.length != 1 then throw new EvalError("negative?: expected 1 argument")
            args.head match
              case IntVal(n) => BoolVal(n < 0)
              case _         => throw new EvalError("negative?: not a number")
        ),
        (
          "odd?",
          args =>
            if args.length != 1 then throw new EvalError("odd?: expected 1 argument")
            args.head match
              case IntVal(n) => BoolVal(n % 2 != 0)
              case _         => throw new EvalError("odd?: not a number")
        ),
        (
          "even?",
          args =>
            if args.length != 1 then throw new EvalError("even?: expected 1 argument")
            args.head match
              case IntVal(n) => BoolVal(n % 2 == 0)
              case _         => throw new EvalError("even?: not a number")
        )
      )
    )

  private def requireTwoInts(args: List[Value]): (Long, Long) =
    (args(0), args(1)) match
      case (IntVal(a), IntVal(b)) => (a, b)
      case _                      => throw new EvalError("expected two numbers")
