package ming

/** Built-in procedure implementations. */
object Builtins:

  import SchemeValue.*

  def applyNamedBuiltin(
    name: String,
    args: List[SchemeValue]
  ): SchemeValue = name match
    case "+"        => arithmeticOp(args, _ + _, 0)
    case "*"        => arithmeticOp(args, _ * _, 1)
    case "-"        => subtractOp(args)
    case "/"        => divideOp(args)
    case "<"        => comparisonOp(args, _ < _)
    case ">"        => comparisonOp(args, _ > _)
    case "="        => comparisonOp(args, _ == _)
    case "<="       => comparisonOp(args, _ <= _)
    case ">="       => comparisonOp(args, _ >= _)
    case "not"      => evalNot(args)
    case "cons"     => evalCons(args)
    case "car"      => evalCar(args)
    case "cdr"      => evalCdr(args)
    case "null?"    => evalNullPred(args)
    case "list"     => SchemeList(args)
    case "length"   => evalLength(args)
    case "string?"  => typePred(args, _.isInstanceOf[SchemeString])
    case "number?"  => typePred(args, _.isInstanceOf[SchemeInt])
    case "boolean?" => typePred(args, _.isInstanceOf[SchemeBool])
    case "pair?" =>
      typePred(
        args,
        { case SchemeList(_ :: _) => true; case _ => false }
      )
    case "symbol?"        => typePred(args, _.isInstanceOf[SchemeSymbol])
    case "char?"          => typePred(args, _.isInstanceOf[SchemeChar])
    case "string-append"  => evalStringAppend(args)
    case "string-length"  => evalStringLength(args)
    case "substring"      => evalSubstring(args)
    case "string->number" => evalStringToNumber(args)
    case "number->string" => evalNumberToString(args)
    case "symbol->string" => evalSymbolToString(args)
    case "string->symbol" => evalStringToSymbol(args)
    case "string-ref"     => evalStringRef(args)
    case _                => throw new EvalError(s"unbound variable: $name")

  private def requireInt(v: SchemeValue): Long = v match
    case SchemeInt(n) => n
    case _ =>
      throw new EvalError(s"expected number, got: ${v.display}")

  private def arithmeticOp(
    args: List[SchemeValue],
    op: (Long, Long) => Long,
    identity: Long
  ): SchemeValue =
    SchemeInt(
      args.foldLeft(identity)((acc, v) => op(acc, requireInt(v)))
    )

  private def subtractOp(args: List[SchemeValue]): SchemeValue =
    args match
      case Nil          => throw new EvalError("- requires at least 1 argument")
      case List(single) => SchemeInt(-requireInt(single))
      case head :: tail =>
        val first = requireInt(head)
        SchemeInt(
          tail.foldLeft(first)((acc, v) => acc - requireInt(v))
        )

  private def divideOp(args: List[SchemeValue]): SchemeValue =
    args match
      case Nil =>
        throw new EvalError("/ requires at least 1 argument")
      case List(single) =>
        val n = requireInt(single)
        if n == 0 then throw new EvalError("division by zero")
        SchemeInt(1 / n)
      case head :: tail =>
        val first = requireInt(head)
        SchemeInt(tail.foldLeft(first) { (acc, v) =>
          val n = requireInt(v)
          if n == 0 then throw new EvalError("division by zero")
          acc / n
        })

  private def comparisonOp(
    args: List[SchemeValue],
    op: (Long, Long) => Boolean
  ): SchemeValue =
    args match
      case Nil | _ :: Nil =>
        throw new EvalError(
          "comparison requires at least 2 arguments"
        )
      case _ =>
        val nums = args.map(requireInt)
        SchemeBool(
          nums.zip(nums.tail).forall((a, b) => op(a, b))
        )

  private def evalNot(args: List[SchemeValue]): SchemeValue =
    args match
      case List(single) =>
        single match
          case SchemeBool(false) => SchemeBool(true)
          case _                 => SchemeBool(false)
      case _ =>
        throw new EvalError("not expects exactly 1 argument")

  private def evalCons(args: List[SchemeValue]): SchemeValue =
    args match
      case List(head, SchemeNil)         => SchemeList(List(head))
      case List(head, SchemeList(elems)) => SchemeList(head :: elems)
      case _                             => throw new EvalError("cons expects 2 arguments")

  private def evalCar(args: List[SchemeValue]): SchemeValue =
    args match
      case List(SchemeList(head :: _)) => head
      case _                           => throw new EvalError("car: not a pair")

  private def evalCdr(args: List[SchemeValue]): SchemeValue =
    args match
      case List(SchemeList(_ :: tail)) =>
        if tail.isEmpty then SchemeNil else SchemeList(tail)
      case _ => throw new EvalError("cdr: not a pair")

  private def evalNullPred(args: List[SchemeValue]): SchemeValue =
    args match
      case List(SchemeNil)       => SchemeBool(true)
      case List(SchemeList(Nil)) => SchemeBool(true)
      case List(_)               => SchemeBool(false)
      case _                     => throw new EvalError("null? expects 1 argument")

  private def evalLength(args: List[SchemeValue]): SchemeValue =
    args match
      case List(SchemeNil)         => SchemeInt(0)
      case List(SchemeList(elems)) => SchemeInt(elems.length.toLong)
      case _                       => throw new EvalError("length: not a list")

  private def requireString(v: SchemeValue): String = v match
    case SchemeString(s) => s
    case _ =>
      throw new EvalError(s"expected string, got: ${v.display}")

  private def evalStringAppend(args: List[SchemeValue]): SchemeValue =
    SchemeString(args.map(requireString).mkString)

  private def evalStringLength(args: List[SchemeValue]): SchemeValue =
    args match
      case List(v) => SchemeInt(requireString(v).length.toLong)
      case _       => throw new EvalError("string-length expects 1 argument")

  private def evalSubstring(args: List[SchemeValue]): SchemeValue =
    args match
      case List(s, start, end) =>
        SchemeString(
          requireString(s).substring(
            requireInt(start).toInt,
            requireInt(end).toInt
          )
        )
      case _ => throw new EvalError("substring expects 3 arguments")

  private def evalStringToNumber(args: List[SchemeValue]): SchemeValue =
    args match
      case List(v) =>
        requireString(v).toLongOption match
          case Some(n) => SchemeInt(n)
          case None    => SchemeBool(false)
      case _ =>
        throw new EvalError("string->number expects 1 argument")

  private def evalNumberToString(args: List[SchemeValue]): SchemeValue =
    args match
      case List(v) => SchemeString(requireInt(v).toString)
      case _ =>
        throw new EvalError("number->string expects 1 argument")

  private def evalSymbolToString(args: List[SchemeValue]): SchemeValue =
    args match
      case List(SchemeSymbol(name)) => SchemeString(name)
      case _ =>
        throw new EvalError("symbol->string expects a symbol")

  private def evalStringToSymbol(args: List[SchemeValue]): SchemeValue =
    args match
      case List(v) => SchemeSymbol(requireString(v))
      case _ =>
        throw new EvalError("string->symbol expects 1 argument")

  private def evalStringRef(args: List[SchemeValue]): SchemeValue =
    args match
      case List(s, idx) =>
        val str = requireString(s)
        val i   = requireInt(idx).toInt
        SchemeChar(str.charAt(i))
      case _ =>
        throw new EvalError("string-ref expects 2 arguments")

  private def typePred(
    args: List[SchemeValue],
    check: SchemeValue => Boolean
  ): SchemeValue =
    args match
      case List(v) => SchemeBool(check(v))
      case _ =>
        throw new EvalError("type predicate expects 1 argument")

  private val outputBuiltins: Set[String] =
    Set("display", "write", "newline")

  private def applyOutputBuiltin(
    name: String,
    args: List[SchemeValue]
  ): (SchemeValue, String) =
    name match
      case "display" =>
        args match
          case List(v) => (SchemeVoid, v.toDisplayStr)
          case _ =>
            throw new EvalError("display expects 1 argument")
      case "write" =>
        args match
          case List(v) => (SchemeVoid, v.display)
          case _ =>
            throw new EvalError("write expects 1 argument")
      case "newline" =>
        args match
          case Nil => (SchemeVoid, "\n")
          case _ =>
            throw new EvalError("newline expects 0 arguments")
      case _ =>
        throw new EvalError(s"unknown output builtin: $name")

  def applyProc(
    func: SchemeValue,
    args: List[SchemeValue]
  ): (SchemeValue, String) =
    func match
      case SchemeSymbol(name) if outputBuiltins.contains(name) =>
        applyOutputBuiltin(name, args)
      case SchemeSymbol(name) =>
        (applyNamedBuiltin(name, args), "")
      case lam @ SchemeLambda(params, body, closure, nameOpt) =>
        if params.length != args.length then
          throw new EvalError(
            s"expected ${params.length} args, got ${args.length}"
          )
        val closureWithSelf = nameOpt match
          case Some(n) => closure.define(n, lam)
          case None    => closure
        val innerEnv = closureWithSelf.extend(params, args)
        Interpreter.evalBody(body, innerEnv)
      case _ =>
        throw new EvalError(s"not a procedure: ${func.display}")
