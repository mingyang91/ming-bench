package ming

/** Core evaluation logic for the Scheme interpreter. */
object Interpreter:

  import SchemeValue.*

  def eval(expr: SchemeValue): SchemeValue = expr match
    case SchemeInt(_)    => expr
    case SchemeBool(_)   => expr
    case SchemeString(_) => expr
    case SchemeNil       => expr
    case SchemeSymbol(name) =>
      if isBuiltin(name) then expr
      else throw new EvalError(s"unbound variable: $name")
    case SchemeList(elements) => evalList(elements)

  private def evalList(elements: List[SchemeValue]): SchemeValue =
    elements match
      case Nil                           => SchemeNil
      case SchemeSymbol("and") :: args   => evalAnd(args)
      case SchemeSymbol("or") :: args    => evalOr(args)
      case SchemeSymbol("quote") :: args => evalQuote(args)
      case head :: args =>
        val func          = eval(head)
        val evaluatedArgs = args.map(eval)
        applyBuiltin(func, evaluatedArgs)

  private def evalQuote(args: List[SchemeValue]): SchemeValue =
    args match
      case List(value) => value
      case _           => throw new EvalError("quote expects exactly 1 argument")

  private def evalAnd(args: List[SchemeValue]): SchemeValue =
    args match
      case Nil         => SchemeBool(true)
      case last :: Nil => eval(last)
      case head :: tail =>
        eval(head) match
          case SchemeBool(false) => SchemeBool(false)
          case _                 => evalAnd(tail)

  private def evalOr(args: List[SchemeValue]): SchemeValue =
    args match
      case Nil         => SchemeBool(false)
      case last :: Nil => eval(last)
      case head :: tail =>
        val result = eval(head)
        result match
          case SchemeBool(false) => evalOr(tail)
          case _                 => result

  private def applyBuiltin(
    func: SchemeValue,
    args: List[SchemeValue]
  ): SchemeValue =
    func match
      case SchemeSymbol(name) => applyNamedBuiltin(name, args)
      case _                  => throw new EvalError(s"not a procedure: ${func.display}")

  private def applyNamedBuiltin(
    name: String,
    args: List[SchemeValue]
  ): SchemeValue = name match
    case "+"   => arithmeticOp(args, _ + _, 0)
    case "*"   => arithmeticOp(args, _ * _, 1)
    case "-"   => subtractOp(args)
    case "/"   => divideOp(args)
    case "<"   => comparisonOp(args, _ < _)
    case ">"   => comparisonOp(args, _ > _)
    case "="   => comparisonOp(args, _ == _)
    case "<="  => comparisonOp(args, _ <= _)
    case ">="  => comparisonOp(args, _ >= _)
    case "not" => evalNot(args)
    case _     => throw new EvalError(s"unbound variable: $name")

  private def requireInt(v: SchemeValue): Long = v match
    case SchemeInt(n) => n
    case _            => throw new EvalError(s"expected number, got: ${v.display}")

  private def arithmeticOp(
    args: List[SchemeValue],
    op: (Long, Long) => Long,
    identity: Long
  ): SchemeValue =
    SchemeInt(args.foldLeft(identity)((acc, v) => op(acc, requireInt(v))))

  private def subtractOp(args: List[SchemeValue]): SchemeValue =
    args match
      case Nil          => throw new EvalError("- requires at least 1 argument")
      case List(single) => SchemeInt(-requireInt(single))
      case head :: tail =>
        val first = requireInt(head)
        SchemeInt(tail.foldLeft(first)((acc, v) => acc - requireInt(v)))

  private def divideOp(args: List[SchemeValue]): SchemeValue =
    args match
      case Nil => throw new EvalError("/ requires at least 1 argument")
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
        throw new EvalError("comparison requires at least 2 arguments")
      case _ =>
        val nums = args.map(requireInt)
        SchemeBool(nums.zip(nums.tail).forall((a, b) => op(a, b)))

  private val builtinNames: Set[String] =
    Set("+", "-", "*", "/", "<", ">", "=", "<=", ">=", "not")

  private def isBuiltin(name: String): Boolean = builtinNames.contains(name)

  private def evalNot(args: List[SchemeValue]): SchemeValue =
    args match
      case List(single) =>
        single match
          case SchemeBool(false) => SchemeBool(true)
          case _                 => SchemeBool(false)
      case _ => throw new EvalError("not expects exactly 1 argument")
