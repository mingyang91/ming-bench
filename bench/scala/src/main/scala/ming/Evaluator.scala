package ming

import SchemeValue.*

/** Scheme interpreter entry point. Agents implement this object. */
object Evaluator:

  /** Evaluate one or more Scheme expressions and return the string representation of the last result. */
  def evalStr(input: String): String =
    val exprs = Parser.parse(input)
    if exprs.isEmpty then throw new EvalError("empty input")
    exprs.map(eval).last.display

  /** Evaluate Scheme expressions and return both the result string and any captured output from display/write/newline.
    */
  def evalStrWithOutput(input: String): (String, String) =
    val result = evalStr(input)
    (result, "")

  private def eval(expr: SchemeValue): SchemeValue = expr match
    case SchemeInt(_)                         => expr
    case SchemeBool(_)                        => expr
    case SchemeString(_)                      => expr
    case SchemeSymbol(name)                   => throw new EvalError(s"unbound variable: $name")
    case SchemeList(Nil)                      => throw new EvalError("empty application")
    case SchemeList(SchemeSymbol(op) :: args) => evalSpecialOrCall(op, args)
    case SchemeList(head :: _)                => throw new EvalError(s"not a procedure: ${head.display}")

  private def evalSpecialOrCall(op: String, args: List[SchemeValue]): SchemeValue = op match
    case "and" => evalAnd(args)
    case "or"  => evalOr(args)
    case "not" => evalNot(args)
    case _     => evalBuiltin(op, args.map(eval))

  private def evalAnd(args: List[SchemeValue]): SchemeValue = args match
    case Nil         => SchemeBool(true)
    case last :: Nil => eval(last)
    case head :: tail =>
      val v = eval(head)
      if isFalsy(v) then v else evalAnd(tail)

  private def evalOr(args: List[SchemeValue]): SchemeValue = args match
    case Nil         => SchemeBool(false)
    case last :: Nil => eval(last)
    case head :: tail =>
      val v = eval(head)
      if isFalsy(v) then evalOr(tail) else v

  private def evalNot(args: List[SchemeValue]): SchemeValue =
    if args.length != 1 then throw new EvalError("not: expected 1 argument")
    val v = eval(args.head)
    SchemeBool(isFalsy(v))

  private def isFalsy(v: SchemeValue): Boolean = v match
    case SchemeBool(false) => true
    case _                 => false

  private def evalBuiltin(op: String, args: List[SchemeValue]): SchemeValue = op match
    case "+" => SchemeInt(args.map(asInt).sum)
    case "-" =>
      if args.isEmpty then throw new EvalError("-: expected at least 1 argument")
      else if args.length == 1 then SchemeInt(-asInt(args.head))
      else SchemeInt(args.map(asInt).reduceLeft(_ - _))
    case "*" => SchemeInt(args.map(asInt).product)
    case "/" =>
      if args.length < 2 then throw new EvalError("/: expected at least 2 arguments")
      else SchemeInt(args.map(asInt).reduceLeft(_ / _))
    case "<"  => compareOp(args, _ < _)
    case ">"  => compareOp(args, _ > _)
    case "="  => compareOp(args, _ == _)
    case "<=" => compareOp(args, _ <= _)
    case ">=" => compareOp(args, _ >= _)
    case _    => throw new EvalError(s"unknown procedure: $op")

  private def compareOp(args: List[SchemeValue], cmp: (Long, Long) => Boolean): SchemeValue =
    if args.length != 2 then throw new EvalError("comparison: expected 2 arguments")
    SchemeBool(cmp(asInt(args.head), asInt(args(1))))

  private def asInt(v: SchemeValue): Long = v match
    case SchemeInt(n) => n
    case other        => throw new EvalError(s"expected integer, got: ${other.display}")
