package ming

object CharBuiltins:

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

  private def charOps: List[(String, SchemeVal)] = List(
    typePredicate("char?", _.isInstanceOf[SchemeVal.CharVal]),
    typePredicate(
      "char-alphabetic?",
      {
        case SchemeVal.CharVal(c) => c.isLetter; case _ => false
      }
    ),
    typePredicate(
      "char-numeric?",
      {
        case SchemeVal.CharVal(c) => c.isDigit; case _ => false
      }
    ),
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
        case List(SchemeVal.CharVal(a), SchemeVal.CharVal(b)) =>
          SchemeVal.BoolVal(a == b)
        case _ => throw new EvalError("char=?: expected 2 chars")
      }
    ),
    "char<?" -> SchemeVal.BuiltinProc(
      "char<?",
      {
        case List(SchemeVal.CharVal(a), SchemeVal.CharVal(b)) =>
          SchemeVal.BoolVal(a < b)
        case _ => throw new EvalError("char<?: expected 2 chars")
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
        case List(SchemeVal.StrVal(s)) =>
          SchemeVal.StrVal(new String(s).toUpperCase.toCharArray)
        case _ => throw new EvalError("string-upcase: expected string")
      }
    ),
    "string-downcase" -> SchemeVal.BuiltinProc(
      "string-downcase",
      {
        case List(SchemeVal.StrVal(s)) =>
          SchemeVal.StrVal(new String(s).toLowerCase.toCharArray)
        case _ => throw new EvalError("string-downcase: expected string")
      }
    )
  )

  def all: List[(String, SchemeVal)] = charOps ++ stringComparisons
