package ming

object StringBuiltins:

  private def typePredicate(
    name: String,
    test: SchemeVal => Boolean
  ): (String, SchemeVal) =
    name -> SchemeVal.BuiltinProc(
      name,
      {
        case List(v) => SchemeVal.BoolVal(test(v))
        case args =>
          throw new EvalError(s"$name: expected 1 argument, got ${args.length}")
      }
    )

  def all: List[(String, SchemeVal)] = List(
    "string-append" -> SchemeVal.BuiltinProc(
      "string-append",
      args =>
        val strs = args.map {
          case SchemeVal.StrVal(s) => new String(s)
          case other               => throw new EvalError(s"string-append: expected string, got ${other.display}")
        }
        SchemeVal.StrVal(strs.mkString.toCharArray)
    ),
    "string-length" -> SchemeVal.BuiltinProc(
      "string-length",
      {
        case List(SchemeVal.StrVal(s)) => SchemeVal.IntVal(s.length.toLong)
        case List(other)               => throw new EvalError(s"string-length: expected string, got ${other.display}")
        case args                      => throw new EvalError(s"string-length: expected 1 argument, got ${args.length}")
      }
    ),
    "substring" -> SchemeVal.BuiltinProc(
      "substring",
      {
        case List(SchemeVal.StrVal(s), SchemeVal.IntVal(start), SchemeVal.IntVal(end)) =>
          SchemeVal.StrVal(new String(s).substring(start.toInt, end.toInt).toCharArray)
        case _ => throw new EvalError("substring: expected (string, start, end)")
      }
    ),
    "string->number" -> SchemeVal.BuiltinProc(
      "string->number",
      {
        case List(SchemeVal.StrVal(s)) =>
          val str = new String(s)
          try SchemeVal.IntVal(str.toLong)
          catch case _: NumberFormatException => SchemeVal.BoolVal(false)
        case List(other) => throw new EvalError(s"string->number: expected string, got ${other.display}")
        case args        => throw new EvalError(s"string->number: expected 1 argument, got ${args.length}")
      }
    ),
    "number->string" -> SchemeVal.BuiltinProc(
      "number->string",
      {
        case List(SchemeVal.IntVal(n)) => SchemeVal.StrVal(n.toString.toCharArray)
        case List(other)               => throw new EvalError(s"number->string: expected number, got ${other.display}")
        case args => throw new EvalError(s"number->string: expected 1 argument, got ${args.length}")
      }
    ),
    "string-ref" -> SchemeVal.BuiltinProc(
      "string-ref",
      {
        case List(SchemeVal.StrVal(s), SchemeVal.IntVal(i)) =>
          SchemeVal.CharVal(s(i.toInt))
        case _ => throw new EvalError("string-ref: expected (string, index)")
      }
    ),
    "symbol->string" -> SchemeVal.BuiltinProc(
      "symbol->string",
      {
        case List(SchemeVal.SymVal(n)) => SchemeVal.StrVal(n.toCharArray)
        case List(other)               => throw new EvalError(s"symbol->string: expected symbol, got ${other.display}")
        case args => throw new EvalError(s"symbol->string: expected 1 argument, got ${args.length}")
      }
    ),
    "string->symbol" -> SchemeVal.BuiltinProc(
      "string->symbol",
      {
        case List(SchemeVal.StrVal(s)) => SchemeVal.SymVal(new String(s))
        case List(other)               => throw new EvalError(s"string->symbol: expected string, got ${other.display}")
        case args => throw new EvalError(s"string->symbol: expected 1 argument, got ${args.length}")
      }
    ),
    "string-copy" -> SchemeVal.BuiltinProc(
      "string-copy",
      {
        case List(SchemeVal.StrVal(s)) => SchemeVal.StrVal(s.clone())
        case List(other)               => throw new EvalError(s"string-copy: expected string, got ${other.display}")
        case args                      => throw new EvalError(s"string-copy: expected 1 argument, got ${args.length}")
      }
    ),
    "string-set!" -> SchemeVal.BuiltinProc(
      "string-set!",
      {
        case List(SchemeVal.StrVal(s), SchemeVal.IntVal(i), SchemeVal.CharVal(c)) =>
          s(i.toInt) = c
          SchemeVal.Void
        case _ => throw new EvalError("string-set!: expected (string, index, char)")
      }
    ),
    typePredicate("char?", _.isInstanceOf[SchemeVal.CharVal])
  )
