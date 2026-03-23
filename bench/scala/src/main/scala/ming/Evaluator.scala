package ming

import SchemeValue.*

/** Scheme interpreter entry point. Agents implement this object. */
object Evaluator:

  /** Evaluate one or more Scheme expressions and return the string representation of the last result. */
  def evalStr(input: String): String =
    val exprs = Parser.parse(input)
    if exprs.isEmpty then throw new EvalError("no expressions")
    exprs.map(eval).last.display

  /** Evaluate Scheme expressions and return both the result string and any captured output from display/write/newline.
    */
  def evalStrWithOutput(input: String): (String, String) =
    val result = evalStr(input)
    (result, "")

  private def eval(expr: SchemeValue): SchemeValue = expr match
    case SInteger(_) | SBoolean(_) | SString(_) => expr
    case SSymbol(name)                          => throw new EvalError(s"unbound variable: $name")
    case SList(Nil)                             => throw new EvalError("empty application")
    case SList(SSymbol(op) :: args)             => evalBuiltin(op, args)
    case SList(head :: _)                       => throw new EvalError(s"not a procedure: ${head.display}")

  private def evalBuiltin(op: String, args: List[SchemeValue]): SchemeValue = op match
    case "+"   => evalAdd(args)
    case "-"   => evalSub(args)
    case "*"   => evalMul(args)
    case "/"   => evalDiv(args)
    case "<"   => evalCompare(args, _ < _)
    case ">"   => evalCompare(args, _ > _)
    case "="   => evalCompare(args, _ == _)
    case "<="  => evalCompare(args, _ <= _)
    case ">="  => evalCompare(args, _ >= _)
    case "not" => evalNot(args)
    case "and" => evalAnd(args)
    case "or"  => evalOr(args)
    case _     => throw new EvalError(s"unknown procedure: $op")

  private def asInteger(v: SchemeValue): Long = v match
    case SInteger(n) => n
    case other       => throw new EvalError(s"expected number, got ${other.display}")

  private def evalAdd(args: List[SchemeValue]): SchemeValue =
    SInteger(args.map(a => asInteger(eval(a))).sum)

  private def evalSub(args: List[SchemeValue]): SchemeValue =
    args match
      case Nil           => throw new EvalError("-: requires at least 1 argument")
      case single :: Nil => SInteger(-asInteger(eval(single)))
      case first :: rest =>
        val firstVal = asInteger(eval(first))
        SInteger(rest.foldLeft(firstVal)((acc, a) => acc - asInteger(eval(a))))

  private def evalMul(args: List[SchemeValue]): SchemeValue =
    SInteger(args.map(a => asInteger(eval(a))).product)

  private def evalDiv(args: List[SchemeValue]): SchemeValue =
    args match
      case Nil => throw new EvalError("/: requires at least 1 argument")
      case single :: Nil =>
        val v = asInteger(eval(single))
        if v == 0 then throw new EvalError("division by zero")
        SInteger(1 / v)
      case first :: rest =>
        val firstVal = asInteger(eval(first))
        SInteger(rest.foldLeft(firstVal) { (acc, a) =>
          val v = asInteger(eval(a))
          if v == 0 then throw new EvalError("division by zero")
          acc / v
        })

  private def evalCompare(args: List[SchemeValue], cmp: (Long, Long) => Boolean): SchemeValue =
    val vals = args.map(a => asInteger(eval(a)))
    SBoolean(vals.zip(vals.tail).forall((a, b) => cmp(a, b)))

  private def isTruthy(v: SchemeValue): Boolean = v match
    case SBoolean(false) => false
    case _               => true

  private def evalNot(args: List[SchemeValue]): SchemeValue =
    args match
      case single :: Nil => SBoolean(!isTruthy(eval(single)))
      case _             => throw new EvalError(s"not: requires exactly 1 argument")

  @scala.annotation.tailrec
  private def evalAnd(args: List[SchemeValue]): SchemeValue =
    args match
      case Nil         => SBoolean(true)
      case last :: Nil => eval(last)
      case head :: rest =>
        val v = eval(head)
        if !isTruthy(v) then v else evalAnd(rest)

  @scala.annotation.tailrec
  private def evalOr(args: List[SchemeValue]): SchemeValue =
    args match
      case Nil         => SBoolean(false)
      case last :: Nil => eval(last)
      case head :: rest =>
        val v = eval(head)
        if isTruthy(v) then v else evalOr(rest)
