package ming

import SchemeValue.*

/** Built-in procedure implementations. */
object Builtins:

  def evalBuiltin(
    op: String,
    args: List[SchemeValue]
  ): (SchemeValue, String) = op match
    case "display" => evalDisplay(args)
    case "write"   => evalWrite(args)
    case "newline" => (SchemeVoid, "\n")
    case _         => (evalPure(op, args), "")

  private def evalPure(
    op: String,
    args: List[SchemeValue]
  ): SchemeValue = op match
    case "+"              => SchemeInt(args.map(asInt).sum)
    case "-"              => evalMinus(args)
    case "*"              => SchemeInt(args.map(asInt).product)
    case "/"              => evalDivide(args)
    case "<"              => compareOp(args, _ < _)
    case ">"              => compareOp(args, _ > _)
    case "="              => compareOp(args, _ == _)
    case "<="             => compareOp(args, _ <= _)
    case ">="             => compareOp(args, _ >= _)
    case "cons"           => evalCons(args)
    case "car"            => evalCar(args)
    case "cdr"            => evalCdr(args)
    case "null?"          => evalNullQ(args)
    case "list"           => SchemeList(args)
    case "append"         => evalAppend(args)
    case "length"         => evalLength(args)
    case "string?"        => typeCheck(args, _.isInstanceOf[SchemeString])
    case "number?"        => typeCheck(args, _.isInstanceOf[SchemeInt])
    case "boolean?"       => typeCheck(args, _.isInstanceOf[SchemeBool])
    case "pair?"          => evalPairQ(args)
    case "symbol?"        => typeCheck(args, _.isInstanceOf[SchemeSymbol])
    case "char?"          => typeCheck(args, _.isInstanceOf[SchemeChar])
    case "string-append"  => evalStringAppend(args)
    case "string-length"  => evalStringLength(args)
    case "substring"      => evalSubstring(args)
    case "string->number" => evalStringToNumber(args)
    case "number->string" => evalNumberToString(args)
    case "symbol->string" => evalSymbolToString(args)
    case "string->symbol" => evalStringToSymbol(args)
    case "string-ref"     => evalStringRef(args)
    case "string-copy"    => evalStringCopy(args)
    case "string-set!"    => evalStringSet(args)
    case _                => throw new EvalError(s"unknown procedure: $op")

  private def evalDisplay(args: List[SchemeValue]): (SchemeValue, String) =
    if args.length != 1 then throw new EvalError("display: expected 1 argument")
    (SchemeVoid, args.head.displayOutput)

  private def evalWrite(args: List[SchemeValue]): (SchemeValue, String) =
    if args.length != 1 then throw new EvalError("write: expected 1 argument")
    (SchemeVoid, args.head.display)

  private def evalMinus(args: List[SchemeValue]): SchemeValue =
    if args.isEmpty then throw new EvalError("-: expected at least 1 argument")
    else if args.length == 1 then SchemeInt(-asInt(args.head))
    else SchemeInt(args.map(asInt).reduceLeft(_ - _))

  private def evalDivide(args: List[SchemeValue]): SchemeValue =
    if args.length < 2 then throw new EvalError("/: expected at least 2 arguments")
    else
      val nums = args.map(asInt)
      if nums.tail.contains(0L) then throw new EvalError("/: division by zero")
      else SchemeInt(nums.reduceLeft(_ / _))

  private def evalCons(args: List[SchemeValue]): SchemeValue =
    if args.length != 2 then throw new EvalError("cons: expected 2 arguments")
    args(1) match
      case SchemeList(es) => SchemeList(args.head :: es)
      case other          => SchemeList(List(args.head, other))

  private def evalCar(args: List[SchemeValue]): SchemeValue =
    if args.length != 1 then throw new EvalError("car: expected 1 argument")
    args.head match
      case SchemeList(h :: _) => h
      case other              => throw new EvalError(s"car: not a pair: ${other.display}")

  private def evalCdr(args: List[SchemeValue]): SchemeValue =
    if args.length != 1 then throw new EvalError("cdr: expected 1 argument")
    args.head match
      case SchemeList(_ :: t) => SchemeList(t)
      case other              => throw new EvalError(s"cdr: not a pair: ${other.display}")

  private def evalNullQ(args: List[SchemeValue]): SchemeValue =
    if args.length != 1 then throw new EvalError("null?: expected 1 argument")
    SchemeBool(args.head == SchemeList(Nil))

  private def evalAppend(args: List[SchemeValue]): SchemeValue =
    val combined = args.foldLeft(List.empty[SchemeValue]) { (acc, arg) =>
      arg match
        case SchemeList(es) => acc ++ es
        case other =>
          throw new EvalError(s"append: not a list: ${other.display}")
    }
    SchemeList(combined)

  private def evalLength(args: List[SchemeValue]): SchemeValue =
    if args.length != 1 then throw new EvalError("length: expected 1 argument")
    args.head match
      case SchemeList(es) => SchemeInt(es.length.toLong)
      case other =>
        throw new EvalError(s"length: not a list: ${other.display}")

  private def evalPairQ(args: List[SchemeValue]): SchemeValue =
    SchemeBool(args.length == 1 && (args.head match
      case SchemeList(_ :: _) => true
      case _                  => false))

  private def evalStringAppend(args: List[SchemeValue]): SchemeValue =
    val sb = args.map {
      case SchemeString(s) => s
      case other           => throw new EvalError(s"string-append: not a string: ${other.display}")
    }
    SchemeString(sb.mkString)

  private def evalStringLength(args: List[SchemeValue]): SchemeValue =
    if args.length != 1 then throw new EvalError("string-length: expected 1 argument")
    args.head match
      case SchemeString(s) => SchemeInt(s.length.toLong)
      case other           => throw new EvalError(s"string-length: not a string: ${other.display}")

  private def evalSubstring(args: List[SchemeValue]): SchemeValue =
    if args.length != 3 then throw new EvalError("substring: expected 3 arguments")
    (args.head, args(1), args(2)) match
      case (SchemeString(s), SchemeInt(start), SchemeInt(end)) =>
        SchemeString(s.substring(start.toInt, end.toInt))
      case _ => throw new EvalError("substring: invalid arguments")

  private def evalStringToNumber(args: List[SchemeValue]): SchemeValue =
    if args.length != 1 then throw new EvalError("string->number: expected 1 argument")
    args.head match
      case SchemeString(s) =>
        s.toLongOption match
          case Some(n) => SchemeInt(n)
          case None    => SchemeBool(false)
      case other => throw new EvalError(s"string->number: not a string: ${other.display}")

  private def evalNumberToString(args: List[SchemeValue]): SchemeValue =
    if args.length != 1 then throw new EvalError("number->string: expected 1 argument")
    args.head match
      case SchemeInt(n) => SchemeString(n.toString)
      case other        => throw new EvalError(s"number->string: not a number: ${other.display}")

  private def evalSymbolToString(args: List[SchemeValue]): SchemeValue =
    if args.length != 1 then throw new EvalError("symbol->string: expected 1 argument")
    args.head match
      case SchemeSymbol(name) => SchemeString(name)
      case other              => throw new EvalError(s"symbol->string: not a symbol: ${other.display}")

  private def evalStringToSymbol(args: List[SchemeValue]): SchemeValue =
    if args.length != 1 then throw new EvalError("string->symbol: expected 1 argument")
    args.head match
      case SchemeString(s) => SchemeSymbol(s)
      case other           => throw new EvalError(s"string->symbol: not a string: ${other.display}")

  private def evalStringRef(args: List[SchemeValue]): SchemeValue =
    if args.length != 2 then throw new EvalError("string-ref: expected 2 arguments")
    (args.head, args(1)) match
      case (SchemeString(s), SchemeInt(idx)) =>
        if idx < 0 || idx >= s.length then throw new EvalError("string-ref: index out of bounds")
        SchemeChar(s.charAt(idx.toInt))
      case _ => throw new EvalError("string-ref: invalid arguments")

  private def evalStringCopy(args: List[SchemeValue]): SchemeValue =
    if args.length != 1 then throw new EvalError("string-copy: expected 1 argument")
    args.head match
      case SchemeString(s)         => new SchemeMutableString(s.toCharArray)
      case ms: SchemeMutableString => new SchemeMutableString(ms.chars.clone())
      case other                   => throw new EvalError(s"string-copy: not a string: ${other.display}")

  private def evalStringSet(args: List[SchemeValue]): SchemeValue =
    if args.length != 3 then throw new EvalError("string-set!: expected 3 arguments")
    (args.head, args(1), args(2)) match
      case (ms: SchemeMutableString, SchemeInt(idx), SchemeChar(c)) =>
        if idx < 0 || idx >= ms.chars.length then throw new EvalError("string-set!: index out of bounds")
        ms.chars(idx.toInt) = c
        SchemeVoid
      case (_: SchemeString, _, _) =>
        throw new EvalError("string-set!: string is immutable")
      case _ => throw new EvalError("string-set!: invalid arguments")

  private def typeCheck(
    args: List[SchemeValue],
    pred: SchemeValue => Boolean
  ): SchemeValue =
    SchemeBool(args.length == 1 && pred(args.head))

  private def compareOp(
    args: List[SchemeValue],
    cmp: (Long, Long) => Boolean
  ): SchemeValue =
    if args.length != 2 then throw new EvalError("comparison: expected 2 arguments")
    SchemeBool(cmp(asInt(args.head), asInt(args(1))))

  def asInt(v: SchemeValue): Long = v match
    case SchemeInt(n) => n
    case other =>
      throw new EvalError(s"expected integer, got: ${other.display}")
