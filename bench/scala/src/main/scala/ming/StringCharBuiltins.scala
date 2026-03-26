package ming

import SchemeTypes.{asNum, errAt, Value}

object StringCharBuiltins:

  def applyStringOps(
    name: String,
    args: List[Value],
    pos: Pos
  ): Value = name match
    case "string-append" =>
      val strs = args.map {
        case Value.VStr(chars, _) => new String(chars)
        case _                    => throw errAt(pos, "string-append: not a string")
      }
      Value.VStr(strs.mkString.toCharArray)
    case "string-length" =>
      if args.length != 1 then throw errAt(pos, "string-length requires 1 argument")
      args.head match
        case Value.VStr(chars, _) => Value.VNum(chars.length.toLong)
        case _                    => throw errAt(pos, "string-length: not a string")
    case "substring" =>
      if args.length != 3 then throw errAt(pos, "substring requires 3 arguments")
      (args(0), args(1), args(2)) match
        case (Value.VStr(chars, _), Value.VNum(start), Value.VNum(end)) =>
          Value.VStr(
            new String(chars)
              .substring(start.toInt, end.toInt)
              .toCharArray
          )
        case _ => throw errAt(pos, "substring: invalid arguments")
    case "string->number" =>
      if args.length != 1 then throw errAt(pos, "string->number requires 1 argument")
      args.head match
        case Value.VStr(chars, _) =>
          val s = new String(chars)
          try Value.VNum(s.toLong)
          catch case _: NumberFormatException => Value.VBool(false)
        case _ =>
          throw errAt(pos, "string->number: not a string")
    case "number->string" =>
      if args.length != 1 then throw errAt(pos, "number->string requires 1 argument")
      args.head match
        case Value.VNum(n) => Value.VStr(n.toString.toCharArray)
        case _             => throw errAt(pos, "number->string: not a number")
    case "symbol->string" =>
      if args.length != 1 then throw errAt(pos, "symbol->string requires 1 argument")
      args.head match
        case Value.VSymbol(n) => Value.VStr(n.toCharArray)
        case _                => throw errAt(pos, "symbol->string: not a symbol")
    case "string->symbol" =>
      if args.length != 1 then throw errAt(pos, "string->symbol requires 1 argument")
      args.head match
        case Value.VStr(chars, _) => Value.VSymbol(new String(chars))
        case _                    => throw errAt(pos, "string->symbol: not a string")
    case "string-ref" =>
      if args.length != 2 then throw errAt(pos, "string-ref requires 2 arguments")
      (args(0), args(1)) match
        case (Value.VStr(chars, _), Value.VNum(i)) =>
          Value.VChar(chars(i.toInt))
        case _ =>
          throw errAt(pos, "string-ref: invalid arguments")
    case "string-copy" =>
      if args.length != 1 then throw errAt(pos, "string-copy requires 1 argument")
      args.head match
        case Value.VStr(chars, _) => Value.VStr(chars.clone(), mutable = true)
        case _                    => throw errAt(pos, "string-copy: not a string")
    case "string-set!" =>
      if args.length != 3 then throw errAt(pos, "string-set! requires 3 arguments")
      (args(0), args(1), args(2)) match
        case (Value.VStr(chars, mut), Value.VNum(i), Value.VChar(c)) =>
          if !mut then throw errAt(pos, "string-set!: strings are immutable")
          chars(i.toInt) = c
          Value.VVoid
        case _ =>
          throw errAt(pos, "string-set!: invalid arguments")
    case "string->list" =>
      if args.length != 1 then throw errAt(pos, "string->list requires 1 argument")
      args.head match
        case Value.VStr(chars, _) => SchemeTypes.schemeList(chars.map(Value.VChar(_)).toList)
        case _                    => throw errAt(pos, "string->list: not a string")
    case "list->string" =>
      if args.length != 1 then throw errAt(pos, "list->string requires 1 argument")
      val elems = args.head match
        case Value.VList(elems) => elems
        case Value.VPair(_)     => SchemeTypes.pairToScalaList(args.head, pos)
        case _                  => throw errAt(pos, "list->string: not a list")
      val chars = elems.map {
        case Value.VChar(c) => c
        case _              => throw errAt(pos, "list->string: not a character")
      }
      Value.VStr(chars.toArray)
    case "make-string" =>
      args match
        case Value.VNum(n) :: Nil =>
          Value.VStr(Array.fill(n.toInt)('\u0000'))
        case Value.VNum(n) :: Value.VChar(c) :: Nil =>
          Value.VStr(Array.fill(n.toInt)(c))
        case _ => throw errAt(pos, "make-string: invalid arguments")
    case "string" =>
      val chars = args.map {
        case Value.VChar(c) => c
        case _              => throw errAt(pos, "string: not a character")
      }
      Value.VStr(chars.toArray)
    case _ => throw errAt(pos, s"unknown string op: $name")

  def applyCharOps(
    name: String,
    args: List[Value],
    pos: Pos
  ): Value =
    def asChar(v: Value): Char = v match
      case Value.VChar(c) => c
      case _              => throw errAt(pos, s"$name: not a character")
    name match
      case "char-alphabetic?" =>
        if args.length != 1 then throw errAt(pos, "char-alphabetic? requires 1 argument")
        Value.VBool(asChar(args.head).isLetter)
      case "char-numeric?" =>
        if args.length != 1 then throw errAt(pos, "char-numeric? requires 1 argument")
        Value.VBool(asChar(args.head).isDigit)
      case "char-upcase" =>
        if args.length != 1 then throw errAt(pos, "char-upcase requires 1 argument")
        Value.VChar(asChar(args.head).toUpper)
      case "char-downcase" =>
        if args.length != 1 then throw errAt(pos, "char-downcase requires 1 argument")
        Value.VChar(asChar(args.head).toLower)
      case "char=?" =>
        if args.length != 2 then throw errAt(pos, "char=? requires 2 arguments")
        Value.VBool(asChar(args(0)) == asChar(args(1)))
      case "char<?" =>
        if args.length != 2 then throw errAt(pos, "char<? requires 2 arguments")
        Value.VBool(asChar(args(0)) < asChar(args(1)))
      case "char->integer" =>
        if args.length != 1 then throw errAt(pos, "char->integer requires 1 argument")
        Value.VNum(asChar(args.head).toLong)
      case "integer->char" =>
        if args.length != 1 then throw errAt(pos, "integer->char requires 1 argument")
        args.head match
          case Value.VNum(n) => Value.VChar(n.toChar)
          case _             => throw errAt(pos, "integer->char: not an integer")
      case _ => throw errAt(pos, s"unknown char op: $name")

  def applyStringCompare(
    name: String,
    args: List[Value],
    pos: Pos
  ): Value =
    def asString(v: Value): String = v match
      case Value.VStr(chars, _) => new String(chars)
      case _                    => throw errAt(pos, s"$name: not a string")
    name match
      case "string=?" =>
        if args.length != 2 then throw errAt(pos, "string=? requires 2 arguments")
        Value.VBool(asString(args(0)) == asString(args(1)))
      case "string<?" =>
        if args.length != 2 then throw errAt(pos, "string<? requires 2 arguments")
        Value.VBool(asString(args(0)) < asString(args(1)))
      case "string-ci=?" =>
        if args.length != 2 then throw errAt(pos, "string-ci=? requires 2 arguments")
        Value.VBool(
          asString(args(0)).equalsIgnoreCase(asString(args(1)))
        )
      case "string-upcase" =>
        if args.length != 1 then throw errAt(pos, "string-upcase requires 1 argument")
        Value.VStr(asString(args.head).toUpperCase.toCharArray)
      case "string>?" =>
        if args.length != 2 then throw errAt(pos, "string>? requires 2 arguments")
        Value.VBool(asString(args(0)) > asString(args(1)))
      case "string<=?" =>
        if args.length != 2 then throw errAt(pos, "string<=? requires 2 arguments")
        Value.VBool(asString(args(0)) <= asString(args(1)))
      case "string>=?" =>
        if args.length != 2 then throw errAt(pos, "string>=? requires 2 arguments")
        Value.VBool(asString(args(0)) >= asString(args(1)))
      case "string-downcase" =>
        if args.length != 1 then throw errAt(pos, "string-downcase requires 1 argument")
        Value.VStr(asString(args.head).toLowerCase.toCharArray)
      case _ =>
        throw errAt(pos, s"unknown string compare op: $name")
