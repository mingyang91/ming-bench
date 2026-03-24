package ming

object StringBuiltins:

  // Track which Array[Char] instances are mutable (created by string-copy)
  private val mutableStrings = java.util.Collections.newSetFromMap(
    new java.util.IdentityHashMap[Array[Char], java.lang.Boolean]()
  )

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

  private def stringOps: List[(String, SchemeVal)] = List(
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
        case List(SchemeVal.StrVal(s)) =>
          val copy = s.clone()
          mutableStrings.add(copy)
          SchemeVal.StrVal(copy)
        case List(other)               => throw new EvalError(s"string-copy: expected string, got ${other.display}")
        case args                      => throw new EvalError(s"string-copy: expected 1 argument, got ${args.length}")
      }
    ),
    "string-set!" -> SchemeVal.BuiltinProc(
      "string-set!",
      {
        case List(SchemeVal.StrVal(s), SchemeVal.IntVal(i), SchemeVal.CharVal(c)) =>
          if !mutableStrings.contains(s) then
            throw new EvalError("string-set!: strings are immutable")
          s(i.toInt) = c
          SchemeVal.Void
        case _ => throw new EvalError("string-set!: expected (string, index, char)")
      }
    ),
    "string->list" -> SchemeVal.BuiltinProc(
      "string->list",
      {
        case List(SchemeVal.StrVal(s)) =>
          SchemeVal.ListVal(s.map(SchemeVal.CharVal(_)).toList)
        case List(other) => throw new EvalError(s"string->list: expected string, got ${other.display}")
        case args        => throw new EvalError(s"string->list: expected 1 argument, got ${args.length}")
      }
    ),
    "list->string" -> SchemeVal.BuiltinProc(
      "list->string",
      {
        case List(SchemeVal.ListVal(elems)) =>
          val chars = elems.map {
            case SchemeVal.CharVal(c) => c
            case other => throw new EvalError(s"list->string: expected char, got ${other.display}")
          }
          SchemeVal.StrVal(chars.toArray)
        case List(other) => throw new EvalError(s"list->string: expected list, got ${other.display}")
        case args => throw new EvalError(s"list->string: expected 1 argument, got ${args.length}")
      }
    ),
    "char->integer" -> SchemeVal.BuiltinProc(
      "char->integer",
      {
        case List(SchemeVal.CharVal(c)) => SchemeVal.IntVal(c.toLong)
        case List(other)                => throw new EvalError(s"char->integer: expected char, got ${other.display}")
        case args                       => throw new EvalError(s"char->integer: expected 1 argument, got ${args.length}")
      }
    ),
    "integer->char" -> SchemeVal.BuiltinProc(
      "integer->char",
      {
        case List(SchemeVal.IntVal(n)) => SchemeVal.CharVal(n.toChar)
        case List(other)               => throw new EvalError(s"integer->char: expected integer, got ${other.display}")
        case args                      => throw new EvalError(s"integer->char: expected 1 argument, got ${args.length}")
      }
    )
  )

  private def charOps: List[(String, SchemeVal)] = List(
    typePredicate("char?", _.isInstanceOf[SchemeVal.CharVal]),
    typePredicate("char-alphabetic?", { case SchemeVal.CharVal(c) => c.isLetter; case _ => false }),
    typePredicate("char-numeric?", { case SchemeVal.CharVal(c) => c.isDigit; case _ => false }),
    "char-upcase" -> SchemeVal.BuiltinProc(
      "char-upcase",
      {
        case List(SchemeVal.CharVal(c)) => SchemeVal.CharVal(c.toUpper)
        case _                          => throw new EvalError("char-upcase: expected char")
      }
    ),
    "char-downcase" -> SchemeVal.BuiltinProc(
      "char-downcase",
      {
        case List(SchemeVal.CharVal(c)) => SchemeVal.CharVal(c.toLower)
        case _                          => throw new EvalError("char-downcase: expected char")
      }
    ),
    "char=?" -> SchemeVal.BuiltinProc(
      "char=?",
      {
        case List(SchemeVal.CharVal(a), SchemeVal.CharVal(b)) => SchemeVal.BoolVal(a == b)
        case _                                                => throw new EvalError("char=?: expected 2 chars")
      }
    ),
    "char<?" -> SchemeVal.BuiltinProc(
      "char<?",
      {
        case List(SchemeVal.CharVal(a), SchemeVal.CharVal(b)) => SchemeVal.BoolVal(a < b)
        case _                                                => throw new EvalError("char<?: expected 2 chars")
      }
    )
  )

  private def stringComparisons: List[(String, SchemeVal)] = List(
    "string=?" -> SchemeVal.BuiltinProc(
      "string=?",
      {
        case List(SchemeVal.StrVal(a), SchemeVal.StrVal(b)) =>
          SchemeVal.BoolVal(java.util.Arrays.equals(a, b))
        case _ => throw new EvalError("string=?: expected 2 strings")
      }
    ),
    "string<?" -> SchemeVal.BuiltinProc(
      "string<?",
      {
        case List(SchemeVal.StrVal(a), SchemeVal.StrVal(b)) =>
          SchemeVal.BoolVal(new String(a).compareTo(new String(b)) < 0)
        case _ => throw new EvalError("string<?: expected 2 strings")
      }
    ),
    "string-ci=?" -> SchemeVal.BuiltinProc(
      "string-ci=?",
      {
        case List(SchemeVal.StrVal(a), SchemeVal.StrVal(b)) =>
          SchemeVal.BoolVal(new String(a).equalsIgnoreCase(new String(b)))
        case _ => throw new EvalError("string-ci=?: expected 2 strings")
      }
    ),
    "string-upcase" -> SchemeVal.BuiltinProc(
      "string-upcase",
      {
        case List(SchemeVal.StrVal(s)) => SchemeVal.StrVal(new String(s).toUpperCase.toCharArray)
        case _                         => throw new EvalError("string-upcase: expected string")
      }
    ),
    "string-downcase" -> SchemeVal.BuiltinProc(
      "string-downcase",
      {
        case List(SchemeVal.StrVal(s)) => SchemeVal.StrVal(new String(s).toLowerCase.toCharArray)
        case _                         => throw new EvalError("string-downcase: expected string")
      }
    )
  )

  def all: List[(String, SchemeVal)] =
    stringOps ++ charOps ++ stringComparisons
