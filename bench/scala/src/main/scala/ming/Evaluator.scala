package ming

import Value.*
import Expr.*

/** Scheme interpreter entry point. Agents implement this object. */
object Evaluator:

  /** Evaluate one or more Scheme expressions and return the string representation of the last result.
    */
  def evalStr(input: String): String =
    val exprs = Parser.parseAll(input)
    if exprs.isEmpty then throw new EvalError("empty input")
    exprs.map(eval).last.display

  /** Evaluate Scheme expressions and return both the result string and any captured output from display/write/newline.
    */
  def evalStrWithOutput(input: String): (String, String) =
    val result = evalStr(input)
    (result, "")

  private def eval(expr: Expr): Value = expr match
    case Num(n)  => IntVal(n)
    case Bool(b) => BoolVal(b)
    case Str(s)  => StrVal(s)
    case Sym(name) =>
      throw new EvalError(s"unbound variable: $name")
    case SList(Nil) =>
      throw new EvalError("empty application")
    case SList(Sym("and") :: args) => evalAnd(args)
    case SList(Sym("or") :: args)  => evalOr(args)
    case SList(Sym("not") :: args) => evalNot(args)
    case SList(head :: args) =>
      val operator = head match
        case Sym(name) => name
        case _         => throw new EvalError("not a procedure")
      val values = args.map(eval)
      applyPrimitive(operator, values)

  private def evalAnd(args: List[Expr]): Value =
    args match
      case Nil         => BoolVal(true)
      case last :: Nil => eval(last)
      case head :: tail =>
        val v = eval(head)
        if !v.isTruthy then v else evalAnd(tail)

  private def evalOr(args: List[Expr]): Value =
    args match
      case Nil         => BoolVal(false)
      case last :: Nil => eval(last)
      case head :: tail =>
        val v = eval(head)
        if v.isTruthy then v else evalOr(tail)

  private def evalNot(args: List[Expr]): Value =
    if args.length != 1 then throw new EvalError("not: expected 1 argument")
    BoolVal(!eval(args.head).isTruthy)

  private def applyPrimitive(op: String, args: List[Value]): Value =
    op match
      case "+"  => arith(args, 0L, _ + _)
      case "*"  => arith(args, 1L, _ * _)
      case "-"  => subtractOp(args)
      case "/"  => divideOp(args)
      case "<"  => compareOp(args, _ < _)
      case ">"  => compareOp(args, _ > _)
      case "="  => compareOp(args, _ == _)
      case "<=" => compareOp(args, _ <= _)
      case ">=" => compareOp(args, _ >= _)
      case _    => throw new EvalError(s"unknown procedure: $op")

  private def requireInts(args: List[Value]): List[Long] =
    args.map {
      case IntVal(n) => n
      case other     => throw new EvalError(s"expected number, got ${other.display}")
    }

  private def arith(args: List[Value], identity: Long, op: (Long, Long) => Long): Value =
    val nums = requireInts(args)
    IntVal(nums.foldLeft(identity)(op))

  private def subtractOp(args: List[Value]): Value =
    val nums = requireInts(args)
    nums match
      case Nil       => IntVal(0)
      case n :: Nil  => IntVal(-n)
      case n :: rest => IntVal(rest.foldLeft(n)(_ - _))

  private def divideOp(args: List[Value]): Value =
    val nums = requireInts(args)
    nums match
      case Nil       => throw new EvalError("/: need at least 1 argument")
      case n :: Nil  => IntVal(1 / n)
      case n :: rest => IntVal(rest.foldLeft(n)(_ / _))

  private def compareOp(args: List[Value], op: (Long, Long) => Boolean): Value =
    val nums   = requireInts(args)
    val result = nums.zip(nums.tail).forall((a, b) => op(a, b))
    BoolVal(result)
