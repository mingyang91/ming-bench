package ming

/** Built-in procedure dispatch and default environment. */
object Builtins:

  def applyBuiltin(
    name: String,
    args: List[Value],
    pos: Option[(Int, Int)]
  ): Value =
    name match
      case "+"  => evalAdd(args, pos)
      case "-"  => evalSub(args, pos)
      case "*"  => evalMul(args, pos)
      case "/"  => evalDiv(args, pos)
      case "<"  => evalCmp(args, _ < _, pos)
      case ">"  => evalCmp(args, _ > _, pos)
      case "="  => evalCmp(args, _ == _, pos)
      case "<=" => evalCmp(args, _ <= _, pos)
      case ">=" => evalCmp(args, _ >= _, pos)
      case "cons" =>
        args match
          case a :: b :: Nil => Value.PairVal(a, b)
          case _ =>
            throw new EvalError("cons requires exactly 2 arguments")
      case "car" =>
        args match
          case Value.PairVal(h, _, _) :: Nil => h
          case _ =>
            throw new EvalError("car requires a pair argument")
      case "cdr" =>
        args match
          case Value.PairVal(_, t, _) :: Nil => t
          case _ =>
            throw new EvalError("cdr requires a pair argument")
      case "null?" =>
        args match
          case Value.NilVal :: Nil => Value.BoolVal(true)
          case _ :: Nil            => Value.BoolVal(false)
          case _ =>
            throw new EvalError("null? requires exactly 1 argument")
      case "list" =>
        args.foldRight(Value.NilVal: Value)(Value.PairVal(_, _))
      case "length" =>
        args match
          case head :: Nil => Value.IntVal(listLength(head))
          case _ =>
            throw new EvalError("length requires exactly 1 argument")
      case "string?"  => typePred(args, v => v.isInstanceOf[Value.StringVal] || v.isInstanceOf[Value.MutableStringVal])
      case "number?"  => typePred(args, _.isInstanceOf[Value.IntVal])
      case "boolean?" => typePred(args, _.isInstanceOf[Value.BoolVal])
      case "pair?"    => typePred(args, _.isInstanceOf[Value.PairVal])
      case "symbol?"  => typePred(args, _.isInstanceOf[Value.Symbol])
      case "char?"    => typePred(args, _.isInstanceOf[Value.CharVal])
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
      case _                => throw new EvalError(s"unknown procedure: $name")

  private def typePred(
    args: List[Value],
    pred: Value => Boolean
  ): Value =
    args match
      case v :: Nil => Value.BoolVal(pred(v))
      case _ =>
        throw new EvalError(
          "type predicate requires exactly 1 argument"
        )

  private def listLength(v: Value): Long = v match
    case Value.NilVal           => 0L
    case Value.PairVal(_, t, _) => 1L + listLength(t)
    case _                      => throw new EvalError("length: not a proper list")

  private def asInt(
    v: Value,
    pos: Option[(Int, Int)]
  ): Long = v match
    case Value.IntVal(n) => n
    case other =>
      throw EvalError.withPos(
        s"expected integer, got: ${other.display}",
        pos
      )

  private def asString(v: Value): String =
    v.stringContent.getOrElse(
      throw new EvalError(s"expected string, got: ${v.display}")
    )

  private def evalAdd(
    args: List[Value],
    pos: Option[(Int, Int)]
  ): Value =
    Value.IntVal(args.foldLeft(0L)((acc, v) => acc + asInt(v, pos)))

  private def evalSub(
    args: List[Value],
    pos: Option[(Int, Int)]
  ): Value = args match
    case Nil         => throw new EvalError("- requires at least 1 argument")
    case head :: Nil => Value.IntVal(-asInt(head, pos))
    case head :: tail =>
      Value.IntVal(
        tail.foldLeft(asInt(head, pos))((a, v) => a - asInt(v, pos))
      )

  private def evalMul(
    args: List[Value],
    pos: Option[(Int, Int)]
  ): Value =
    Value.IntVal(args.foldLeft(1L)((acc, v) => acc * asInt(v, pos)))

  private def evalDiv(
    args: List[Value],
    pos: Option[(Int, Int)]
  ): Value = args match
    case Nil =>
      throw new EvalError("/ requires at least 1 argument")
    case head :: Nil =>
      val d = asInt(head, pos)
      if d == 0L then throw EvalError.withPos("division by zero", pos)
      else Value.IntVal(1L / d)
    case head :: tail =>
      Value.IntVal(tail.foldLeft(asInt(head, pos)) { (a, v) =>
        val d = asInt(v, pos)
        if d == 0L then throw EvalError.withPos("division by zero", pos)
        else a / d
      })

  private def evalCmp(
    args: List[Value],
    op: (Long, Long) => Boolean,
    pos: Option[(Int, Int)]
  ): Value =
    args match
      case a :: b :: Nil =>
        Value.BoolVal(op(asInt(a, pos), asInt(b, pos)))
      case _ =>
        throw new EvalError(
          "comparison requires exactly 2 arguments"
        )

  private def evalStringAppend(args: List[Value]): Value =
    Value.StringVal(args.map(asString).mkString)

  private def evalStringLength(args: List[Value]): Value =
    args match
      case v :: Nil =>
        val s = asString(v)
        Value.IntVal(s.length.toLong)
      case _ =>
        throw new EvalError("string-length requires 1 string argument")

  private def evalSubstring(args: List[Value]): Value =
    args match
      case v :: Value.IntVal(start) :: Value.IntVal(end) :: Nil =>
        val s = asString(v)
        Value.StringVal(s.substring(start.toInt, end.toInt))
      case _ =>
        throw new EvalError("substring requires string, start, end")

  private def evalStringToNumber(args: List[Value]): Value =
    args match
      case v :: Nil =>
        val s = asString(v)
        s.toLongOption match
          case Some(n) => Value.IntVal(n)
          case None    => Value.BoolVal(false)
      case _ =>
        throw new EvalError("string->number requires 1 string argument")

  private def evalNumberToString(args: List[Value]): Value =
    args match
      case Value.IntVal(n) :: Nil => Value.StringVal(n.toString)
      case _ =>
        throw new EvalError("number->string requires 1 integer argument")

  private def evalSymbolToString(args: List[Value]): Value =
    args match
      case Value.Symbol(name, _) :: Nil => Value.StringVal(name)
      case _ =>
        throw new EvalError("symbol->string requires 1 symbol argument")

  private def evalStringToSymbol(args: List[Value]): Value =
    args match
      case v :: Nil =>
        val s = asString(v)
        Value.Symbol(s)
      case _ =>
        throw new EvalError("string->symbol requires 1 string argument")

  private def evalStringRef(args: List[Value]): Value =
    args match
      case v :: Value.IntVal(i) :: Nil =>
        val s = asString(v)
        Value.CharVal(s.charAt(i.toInt))
      case _ =>
        throw new EvalError("string-ref requires string and index")

  private def evalStringCopy(args: List[Value]): Value =
    args match
      case v :: Nil =>
        val s = asString(v)
        Value.MutableStringVal(s.toCharArray)
      case _ =>
        throw new EvalError("string-copy requires 1 string argument")

  private def evalStringSet(args: List[Value]): Value =
    args match
      case Value.MutableStringVal(chars) :: Value.IntVal(i) :: Value.CharVal(c) :: Nil =>
        chars(i.toInt) = c
        Value.VoidVal
      case _ =>
        throw new EvalError("string-set! requires mutable string, index, and char")

  val defaultEnv: Env = Env(Map.empty, None)
    .define("+", Value.Symbol("+"))
    .define("-", Value.Symbol("-"))
    .define("*", Value.Symbol("*"))
    .define("/", Value.Symbol("/"))
    .define("<", Value.Symbol("<"))
    .define(">", Value.Symbol(">"))
    .define("=", Value.Symbol("="))
    .define("<=", Value.Symbol("<="))
    .define(">=", Value.Symbol(">="))
    .define("cons", Value.Symbol("cons"))
    .define("car", Value.Symbol("car"))
    .define("cdr", Value.Symbol("cdr"))
    .define("null?", Value.Symbol("null?"))
    .define("list", Value.Symbol("list"))
    .define("length", Value.Symbol("length"))
    .define("string?", Value.Symbol("string?"))
    .define("number?", Value.Symbol("number?"))
    .define("boolean?", Value.Symbol("boolean?"))
    .define("pair?", Value.Symbol("pair?"))
    .define("symbol?", Value.Symbol("symbol?"))
    .define("char?", Value.Symbol("char?"))
    .define("display", Value.Symbol("display"))
    .define("write", Value.Symbol("write"))
    .define("newline", Value.Symbol("newline"))
    .define("string-append", Value.Symbol("string-append"))
    .define("string-length", Value.Symbol("string-length"))
    .define("substring", Value.Symbol("substring"))
    .define("string->number", Value.Symbol("string->number"))
    .define("number->string", Value.Symbol("number->string"))
    .define("symbol->string", Value.Symbol("symbol->string"))
    .define("string->symbol", Value.Symbol("string->symbol"))
    .define("string-ref", Value.Symbol("string-ref"))
    .define("string-copy", Value.Symbol("string-copy"))
    .define("string-set!", Value.Symbol("string-set!"))
