package ming

object StringBuiltins:

  // Track which Array[Char] instances are mutable (created by string-copy)
  private val mutableStrings = java.util.Collections.newSetFromMap(
    new java.util.IdentityHashMap[Array[Char], java.lang.Boolean]()
  )

  private def coreStringOps: List[(String, SchemeVal)] = List(
    "string-append" -> SchemeVal.BuiltinProc(
      "string-append",
      args =>
        val strs = args.map {
          case SchemeVal.StrVal(s) => new String(s)
          case other =>
            throw new EvalError(
              s"string-append: expected string, got ${other.display}"
            )
        }
        SchemeVal.StrVal(strs.mkString.toCharArray)
    ),
    "string-length" -> SchemeVal.BuiltinProc(
      "string-length",
      {
        case List(SchemeVal.StrVal(s)) => SchemeVal.IntVal(s.length.toLong)
        case List(other) =>
          throw new EvalError(
            s"string-length: expected string, got ${other.display}"
          )
        case args =>
          throw new EvalError(
            s"string-length: expected 1 argument, got ${args.length}"
          )
      }
    ),
    "substring" -> SchemeVal.BuiltinProc(
      "substring",
      {
        case List(
              SchemeVal.StrVal(s),
              SchemeVal.IntVal(start),
              SchemeVal.IntVal(end)
            ) =>
          SchemeVal.StrVal(
            new String(s).substring(start.toInt, end.toInt).toCharArray
          )
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
        case List(other) =>
          throw new EvalError(
            s"string->number: expected string, got ${other.display}"
          )
        case args =>
          throw new EvalError(
            s"string->number: expected 1 argument, got ${args.length}"
          )
      }
    ),
    "number->string" -> SchemeVal.BuiltinProc(
      "number->string",
      {
        case List(SchemeVal.IntVal(n)) =>
          SchemeVal.StrVal(n.toString.toCharArray)
        case List(other) =>
          throw new EvalError(
            s"number->string: expected number, got ${other.display}"
          )
        case args =>
          throw new EvalError(
            s"number->string: expected 1 argument, got ${args.length}"
          )
      }
    ),
    "string" -> SchemeVal.BuiltinProc(
      "string",
      args =>
        val chars = args.map {
          case SchemeVal.CharVal(c) => c
          case other => throw new EvalError(s"string: expected char, got ${other.display}")
        }
        SchemeVal.StrVal(chars.toArray)
    ),
    "make-string" -> SchemeVal.BuiltinProc(
      "make-string",
      {
        case List(SchemeVal.IntVal(n)) =>
          SchemeVal.StrVal(Array.fill(n.toInt)('\u0000'))
        case List(SchemeVal.IntVal(n), SchemeVal.CharVal(c)) =>
          SchemeVal.StrVal(Array.fill(n.toInt)(c))
        case _ => throw new EvalError("make-string: expected (length) or (length, char)")
      }
    ),
    "string-ref" -> SchemeVal.BuiltinProc(
      "string-ref",
      {
        case List(SchemeVal.StrVal(s), SchemeVal.IntVal(i)) =>
          SchemeVal.CharVal(s(i.toInt))
        case _ =>
          throw new EvalError("string-ref: expected (string, index)")
      }
    )
  )

  private def symbolConversionOps: List[(String, SchemeVal)] = List(
    "symbol->string" -> SchemeVal.BuiltinProc(
      "symbol->string",
      {
        case List(SchemeVal.SymVal(n)) => SchemeVal.StrVal(n.toCharArray)
        case List(other) =>
          throw new EvalError(
            s"symbol->string: expected symbol, got ${other.display}"
          )
        case args =>
          throw new EvalError(
            s"symbol->string: expected 1 argument, got ${args.length}"
          )
      }
    ),
    "string->symbol" -> SchemeVal.BuiltinProc(
      "string->symbol",
      {
        case List(SchemeVal.StrVal(s)) => SchemeVal.SymVal(new String(s))
        case List(other) =>
          throw new EvalError(
            s"string->symbol: expected string, got ${other.display}"
          )
        case args =>
          throw new EvalError(
            s"string->symbol: expected 1 argument, got ${args.length}"
          )
      }
    )
  )

  private def mutationOps: List[(String, SchemeVal)] = List(
    "string-copy" -> SchemeVal.BuiltinProc(
      "string-copy",
      {
        case List(SchemeVal.StrVal(s)) =>
          val copy = s.clone()
          mutableStrings.add(copy)
          SchemeVal.StrVal(copy)
        case List(other) =>
          throw new EvalError(
            s"string-copy: expected string, got ${other.display}"
          )
        case args =>
          throw new EvalError(
            s"string-copy: expected 1 argument, got ${args.length}"
          )
      }
    ),
    "string-set!" -> SchemeVal.BuiltinProc(
      "string-set!",
      {
        case List(
              SchemeVal.StrVal(s),
              SchemeVal.IntVal(i),
              SchemeVal.CharVal(c)
            ) =>
          if !mutableStrings.contains(s) then throw new EvalError("string-set!: strings are immutable")
          s(i.toInt) = c
          SchemeVal.Void
        case _ =>
          throw new EvalError("string-set!: expected (string, index, char)")
      }
    ),
    "string->list" -> SchemeVal.BuiltinProc(
      "string->list",
      {
        case List(SchemeVal.StrVal(s)) =>
          SchemeVal.schemeList(s.map(SchemeVal.CharVal(_)).toList)
        case List(other) =>
          throw new EvalError(
            s"string->list: expected string, got ${other.display}"
          )
        case args =>
          throw new EvalError(
            s"string->list: expected 1 argument, got ${args.length}"
          )
      }
    ),
    "list->string" -> SchemeVal.BuiltinProc(
      "list->string",
      {
        case List(v @ (SchemeVal.PairVal(_) | SchemeVal.ListVal(_))) =>
          val elems = SchemeVal.toScalaList(v)
          val chars = elems.map {
            case SchemeVal.CharVal(c) => c
            case other =>
              throw new EvalError(
                s"list->string: expected char, got ${other.display}"
              )
          }
          SchemeVal.StrVal(chars.toArray)
        case List(other) =>
          throw new EvalError(
            s"list->string: expected list, got ${other.display}"
          )
        case args =>
          throw new EvalError(
            s"list->string: expected 1 argument, got ${args.length}"
          )
      }
    ),
    "char->integer" -> SchemeVal.BuiltinProc(
      "char->integer",
      {
        case List(SchemeVal.CharVal(c)) => SchemeVal.IntVal(c.toLong)
        case List(other) =>
          throw new EvalError(
            s"char->integer: expected char, got ${other.display}"
          )
        case args =>
          throw new EvalError(
            s"char->integer: expected 1 argument, got ${args.length}"
          )
      }
    ),
    "integer->char" -> SchemeVal.BuiltinProc(
      "integer->char",
      {
        case List(SchemeVal.IntVal(n)) => SchemeVal.CharVal(n.toChar)
        case List(other) =>
          throw new EvalError(
            s"integer->char: expected integer, got ${other.display}"
          )
        case args =>
          throw new EvalError(
            s"integer->char: expected 1 argument, got ${args.length}"
          )
      }
    )
  )

  def all: List[(String, SchemeVal)] =
    coreStringOps ++ symbolConversionOps ++ mutationOps ++ CharBuiltins.all
