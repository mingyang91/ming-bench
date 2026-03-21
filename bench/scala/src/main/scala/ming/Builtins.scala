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
      case "string?" =>
        typePred(args, _.isInstanceOf[Value.StringVal])
      case "number?" =>
        typePred(args, _.isInstanceOf[Value.IntVal])
      case "boolean?" =>
        typePred(args, _.isInstanceOf[Value.BoolVal])
      case "pair?" =>
        typePred(args, _.isInstanceOf[Value.PairVal])
      case "symbol?" =>
        typePred(args, _.isInstanceOf[Value.Symbol])
      case _ => throw new EvalError(s"unknown procedure: $name")

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
