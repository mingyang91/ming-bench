package ming

/** Scheme interpreter entry point. Agents implement this object. */
object Evaluator:

  /** Evaluate one or more Scheme expressions and return the string representation of the last result.
    */
  def evalStr(input: String): String =
    val exprs = Parser.parse(input)
    if exprs.isEmpty then throw new EvalError("no expressions")
    exprs.map(eval).last.display

  /** Evaluate Scheme expressions and return both the result string and any captured output from display/write/newline.
    */
  def evalStrWithOutput(input: String): (String, String) =
    val result = evalStr(input)
    (result, "")

  private def eval(value: Value): Value = value match
    case Value.IntVal(_)    => value
    case Value.BoolVal(_)   => value
    case Value.StringVal(_) => value
    case Value.NilVal       => value
    case Value.Symbol(name) =>
      throw new EvalError(s"unbound variable: $name")
    case Value.PairVal(car, _) => evalCall(car, value)

  private def evalCall(operator: Value, expr: Value): Value =
    operator match
      case Value.Symbol(name) => evalSpecialOrCall(name, toList(expr).tail)
      case _                  => throw new EvalError("not a procedure")

  private def evalSpecialOrCall(name: String, args: List[Value]): Value =
    name match
      case "and" => evalAnd(args)
      case "or"  => evalOr(args)
      case "not" => evalNot(args)
      case _     => evalBuiltin(name, args.map(eval))

  private def evalBuiltin(name: String, args: List[Value]): Value =
    name match
      case "+"  => evalAdd(args)
      case "-"  => evalSub(args)
      case "*"  => evalMul(args)
      case "/"  => evalDiv(args)
      case "<"  => evalCmp(args, _ < _)
      case ">"  => evalCmp(args, _ > _)
      case "="  => evalCmp(args, _ == _)
      case "<=" => evalCmp(args, _ <= _)
      case ">=" => evalCmp(args, _ >= _)
      case _    => throw new EvalError(s"unknown procedure: $name")

  private def evalAdd(args: List[Value]): Value =
    Value.IntVal(args.foldLeft(0L)((acc, v) => acc + asInt(v)))

  private def evalSub(args: List[Value]): Value = args match
    case Nil          => throw new EvalError("- requires at least 1 argument")
    case head :: Nil  => Value.IntVal(-asInt(head))
    case head :: tail => Value.IntVal(tail.foldLeft(asInt(head))((a, v) => a - asInt(v)))

  private def evalMul(args: List[Value]): Value =
    Value.IntVal(args.foldLeft(1L)((acc, v) => acc * asInt(v)))

  private def evalDiv(args: List[Value]): Value = args match
    case Nil          => throw new EvalError("/ requires at least 1 argument")
    case head :: Nil  => Value.IntVal(1 / asInt(head))
    case head :: tail => Value.IntVal(tail.foldLeft(asInt(head))((a, v) => a / asInt(v)))

  private def evalCmp(
    args: List[Value],
    op: (Long, Long) => Boolean
  ): Value =
    args match
      case a :: b :: Nil => Value.BoolVal(op(asInt(a), asInt(b)))
      case _             => throw new EvalError("comparison requires exactly 2 arguments")

  private def evalAnd(args: List[Value]): Value = args match
    case Nil         => Value.BoolVal(true)
    case head :: Nil => eval(head)
    case head :: tail =>
      val v = eval(head)
      if isFalsy(v) then v else evalAnd(tail)

  private def evalOr(args: List[Value]): Value = args match
    case Nil         => Value.BoolVal(false)
    case head :: Nil => eval(head)
    case head :: tail =>
      val v = eval(head)
      if !isFalsy(v) then v else evalOr(tail)

  private def evalNot(args: List[Value]): Value = args match
    case head :: Nil => Value.BoolVal(isFalsy(eval(head)))
    case _           => throw new EvalError("not requires exactly 1 argument")

  private def isFalsy(v: Value): Boolean = v match
    case Value.BoolVal(false) => true
    case _                    => false

  private def asInt(v: Value): Long = v match
    case Value.IntVal(n) => n
    case other           => throw new EvalError(s"expected integer, got: ${other.display}")

  private def toList(v: Value): List[Value] = v match
    case Value.NilVal        => Nil
    case Value.PairVal(h, t) => h :: toList(t)
    case other               => List(other)
