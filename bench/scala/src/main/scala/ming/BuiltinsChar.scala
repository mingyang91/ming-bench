package ming

import Value.*

/** Built-in char and string comparison procedures. */
object BuiltinsChar:

  private def define(env: Env, entries: List[(String, List[Value] => Value)]): Unit =
    Builtins.define(env, entries)

  def register(env: Env): Unit =
    registerCharOps(env)
    registerStringCompare(env)

  private def registerCharOps(env: Env): Unit =
    define(
      env,
      List(
        (
          "char-alphabetic?",
          args =>
            if args.length != 1 then throw new EvalError("char-alphabetic?: expected 1 argument")
            args.head match
              case CharVal(c) => BoolVal(c.isLetter)
              case _          => throw new EvalError("char-alphabetic?: not a char")
        ),
        (
          "char-numeric?",
          args =>
            if args.length != 1 then throw new EvalError("char-numeric?: expected 1 argument")
            args.head match
              case CharVal(c) => BoolVal(c.isDigit)
              case _          => throw new EvalError("char-numeric?: not a char")
        ),
        (
          "char-upcase",
          args =>
            if args.length != 1 then throw new EvalError("char-upcase: expected 1 argument")
            args.head match
              case CharVal(c) => CharVal(c.toUpper)
              case _          => throw new EvalError("char-upcase: not a char")
        ),
        (
          "char-downcase",
          args =>
            if args.length != 1 then throw new EvalError("char-downcase: expected 1 argument")
            args.head match
              case CharVal(c) => CharVal(c.toLower)
              case _          => throw new EvalError("char-downcase: not a char")
        ),
        (
          "char=?",
          args =>
            if args.length != 2 then throw new EvalError("char=?: expected 2 arguments")
            (args(0), args(1)) match
              case (CharVal(a), CharVal(b)) => BoolVal(a == b)
              case _                        => throw new EvalError("char=?: not chars")
        ),
        (
          "char<?",
          args =>
            if args.length != 2 then throw new EvalError("char<?: expected 2 arguments")
            (args(0), args(1)) match
              case (CharVal(a), CharVal(b)) => BoolVal(a < b)
              case _                        => throw new EvalError("char<?: not chars")
        )
      )
    )

  private def registerStringCompare(env: Env): Unit =
    define(
      env,
      List(
        (
          "string=?",
          args =>
            if args.length != 2 then throw new EvalError("string=?: expected 2 arguments")
            (args(0), args(1)) match
              case (StrVal(a), StrVal(b)) => BoolVal(java.util.Arrays.equals(a, b))
              case _                      => throw new EvalError("string=?: not strings")
        ),
        (
          "string<?",
          args =>
            if args.length != 2 then throw new EvalError("string<?: expected 2 arguments")
            (args(0), args(1)) match
              case (StrVal(a), StrVal(b)) => BoolVal(new String(a).compareTo(new String(b)) < 0)
              case _                      => throw new EvalError("string<?: not strings")
        ),
        (
          "string-ci=?",
          args =>
            if args.length != 2 then throw new EvalError("string-ci=?: expected 2 arguments")
            (args(0), args(1)) match
              case (StrVal(a), StrVal(b)) =>
                BoolVal(new String(a).equalsIgnoreCase(new String(b)))
              case _ => throw new EvalError("string-ci=?: not strings")
        ),
        (
          "string>?",
          args =>
            if args.length != 2 then throw new EvalError("string>?: expected 2 arguments")
            (args(0), args(1)) match
              case (StrVal(a), StrVal(b)) => BoolVal(new String(a).compareTo(new String(b)) > 0)
              case _                      => throw new EvalError("string>?: not strings")
        ),
        (
          "string<=?",
          args =>
            if args.length != 2 then throw new EvalError("string<=?: expected 2 arguments")
            (args(0), args(1)) match
              case (StrVal(a), StrVal(b)) => BoolVal(new String(a).compareTo(new String(b)) <= 0)
              case _                      => throw new EvalError("string<=?: not strings")
        ),
        (
          "string>=?",
          args =>
            if args.length != 2 then throw new EvalError("string>=?: expected 2 arguments")
            (args(0), args(1)) match
              case (StrVal(a), StrVal(b)) => BoolVal(new String(a).compareTo(new String(b)) >= 0)
              case _                      => throw new EvalError("string>=?: not strings")
        ),
        (
          "string-upcase",
          args =>
            if args.length != 1 then throw new EvalError("string-upcase: expected 1 argument")
            args.head match
              case StrVal(chars) => StrVal(new String(chars).toUpperCase.toCharArray)
              case _             => throw new EvalError("string-upcase: not a string")
        ),
        (
          "string-downcase",
          args =>
            if args.length != 1 then throw new EvalError("string-downcase: expected 1 argument")
            args.head match
              case StrVal(chars) => StrVal(new String(chars).toLowerCase.toCharArray)
              case _             => throw new EvalError("string-downcase: not a string")
        )
      )
    )
