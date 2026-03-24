package ming

import Evaluator.Val
import Evaluator.Val.*

/** I/O, string, and character built-in procedures. */
object StringBuiltins:

  val all: List[(String, Val)] = List(
    // --- I/O ---
    "display" -> Builtin {
      case List(v) =>
        Evaluator.outputBuffer.append(Display.show(v))
        Void
      case _ => throw new EvalError("display requires 1 argument")
    },
    "write" -> Builtin {
      case List(v) =>
        Evaluator.outputBuffer.append(Display.write(v))
        Void
      case _ => throw new EvalError("write requires 1 argument")
    },
    "newline" -> Builtin {
      case scala.List() =>
        Evaluator.outputBuffer.append("\n")
        Void
      case _ => throw new EvalError("newline requires 0 arguments")
    },
    // --- String operations ---
    "string-append" -> Builtin { args =>
      val strs = args.map {
        case Str(chars) => new String(chars)
        case v          => throw new EvalError(s"string-append: not a string: ${Display.write(v)}")
      }
      Evaluator.mkStr(strs.mkString)
    },
    "string-length" -> Builtin {
      case List(Str(chars)) => Num(chars.length.toLong)
      case _                => throw new EvalError("string-length requires 1 string argument")
    },
    "substring" -> Builtin {
      case List(Str(chars), Num(start), Num(end)) =>
        Evaluator.mkStr(new String(chars).substring(start.toInt, end.toInt))
      case _ => throw new EvalError("substring requires a string and two integers")
    },
    "string->number" -> Builtin {
      case List(Str(chars)) =>
        val s = new String(chars)
        try Num(s.toLong)
        catch case _: NumberFormatException => Bool(false)
      case _ => throw new EvalError("string->number requires 1 string argument")
    },
    "number->string" -> Builtin {
      case List(Num(n)) => Evaluator.mkStr(n.toString)
      case _            => throw new EvalError("number->string requires 1 numeric argument")
    },
    "symbol->string" -> Builtin {
      case List(Symbol(name)) => Evaluator.mkStr(name)
      case _                  => throw new EvalError("symbol->string requires 1 symbol argument")
    },
    "string->symbol" -> Builtin {
      case List(Str(chars)) => Symbol(new String(chars))
      case _                => throw new EvalError("string->symbol requires 1 string argument")
    },
    "string-ref" -> Builtin {
      case List(Str(chars), Num(i)) => SchemeChar(chars(i.toInt))
      case _                        => throw new EvalError("string-ref requires a string and an integer")
    },
    // --- String mutation (L6) / immutability (L15) ---
    "string-set!" -> Builtin {
      case List(Str(chars), Num(i), SchemeChar(c)) =>
        if !Evaluator.mutableStrings.contains(chars) then throw new EvalError("string-set!: strings are immutable")
        chars(i.toInt) = c
        Void
      case _ => throw new EvalError("string-set! requires a string, an integer, and a character")
    },
    "string-copy" -> Builtin {
      case List(Str(chars)) =>
        val copy = chars.clone()
        Evaluator.mutableStrings.add(copy)
        Str(copy)
      case _ => throw new EvalError("string-copy requires 1 string argument")
    },
    "char?" -> Builtin {
      case List(SchemeChar(_)) => Bool(true)
      case List(_)             => Bool(false)
      case _                   => throw new EvalError("char? requires 1 argument")
    },
    // --- Character utilities ---
    "char-alphabetic?" -> Builtin {
      case List(SchemeChar(c)) => Bool(c.isLetter)
      case _                   => throw new EvalError("char-alphabetic? requires 1 character argument")
    },
    "char-numeric?" -> Builtin {
      case List(SchemeChar(c)) => Bool(c.isDigit)
      case _                   => throw new EvalError("char-numeric? requires 1 character argument")
    },
    "char-upcase" -> Builtin {
      case List(SchemeChar(c)) => SchemeChar(c.toUpper)
      case _                   => throw new EvalError("char-upcase requires 1 character argument")
    },
    "char-downcase" -> Builtin {
      case List(SchemeChar(c)) => SchemeChar(c.toLower)
      case _                   => throw new EvalError("char-downcase requires 1 character argument")
    },
    "char=?" -> Builtin {
      case List(SchemeChar(a), SchemeChar(b)) => Bool(a == b)
      case _                                  => throw new EvalError("char=? requires 2 character arguments")
    },
    "char<?" -> Builtin {
      case List(SchemeChar(a), SchemeChar(b)) => Bool(a < b)
      case _                                  => throw new EvalError("char<? requires 2 character arguments")
    },
    // --- String comparison/case utilities ---
    "string=?" -> Builtin {
      case List(Str(a), Str(b)) => Bool(java.util.Arrays.equals(a, b))
      case _                    => throw new EvalError("string=? requires 2 string arguments")
    },
    "string<?" -> Builtin {
      case List(Str(a), Str(b)) => Bool(new String(a) < new String(b))
      case _                    => throw new EvalError("string<? requires 2 string arguments")
    },
    "string-ci=?" -> Builtin {
      case List(Str(a), Str(b)) =>
        Bool(new String(a).equalsIgnoreCase(new String(b)))
      case _ => throw new EvalError("string-ci=? requires 2 string arguments")
    },
    "string-upcase" -> Builtin {
      case List(Str(chars)) => Evaluator.mkStr(new String(chars).toUpperCase)
      case _                => throw new EvalError("string-upcase requires 1 string argument")
    },
    "string-downcase" -> Builtin {
      case List(Str(chars)) => Evaluator.mkStr(new String(chars).toLowerCase)
      case _                => throw new EvalError("string-downcase requires 1 string argument")
    },
    // --- L15: string<->list and char<->integer ---
    "string->list" -> Builtin {
      case List(Str(chars)) =>
        chars.foldRight(Val.Nil: Val)((c, acc) => Pair(SchemeChar(c), acc))
      case _ => throw new EvalError("string->list requires 1 string argument")
    },
    "list->string" -> Builtin {
      case List(lst) =>
        val chars = Evaluator.toList(lst).map {
          case SchemeChar(c) => c
          case v             => throw new EvalError(s"list->string: not a character: ${Display.write(v)}")
        }
        Evaluator.mkStr(new String(chars.toArray))
      case _ => throw new EvalError("list->string requires 1 argument")
    },
    "char->integer" -> Builtin {
      case List(SchemeChar(c)) => Num(c.toLong)
      case _                   => throw new EvalError("char->integer requires 1 character argument")
    },
    "integer->char" -> Builtin {
      case List(Num(n)) => SchemeChar(n.toChar)
      case _            => throw new EvalError("integer->char requires 1 integer argument")
    }
  )
