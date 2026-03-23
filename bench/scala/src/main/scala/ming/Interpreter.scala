package ming

import SchemeValue.*

/** Core eval logic for Level 1: atoms, arithmetic, comparisons, booleans. */
object Interpreter:

  def eval(expr: SchemeValue): SchemeValue =
    expr match
      case IntVal(_) | BoolVal(_) | StringVal(_) | Void => expr
      case SymbolVal(name) =>
        throw new EvalError(s"unbound variable: $name")
      case ListVal(Nil) =>
        throw new EvalError("empty application")
      case ListVal(SymbolVal(op) :: args) =>
        evalSpecialOrCall(op, args)
      case ListVal(_) =>
        throw new EvalError("invalid application")

  private def evalSpecialOrCall(
    op: String,
    args: List[SchemeValue]
  ): SchemeValue =
    op match
      case "and" => evalAnd(args)
      case "or"  => evalOr(args)
      case _     => evalBuiltin(op, args.map(eval))

  private def evalAnd(args: List[SchemeValue]): SchemeValue =
    args match
      case Nil         => BoolVal(true)
      case last :: Nil => eval(last)
      case head :: tail =>
        val v = eval(head)
        if !v.isTruthy then v else evalAnd(tail)

  private def evalOr(args: List[SchemeValue]): SchemeValue =
    args match
      case Nil         => BoolVal(false)
      case last :: Nil => eval(last)
      case head :: tail =>
        val v = eval(head)
        if v.isTruthy then v else evalOr(tail)

  private def evalBuiltin(
    op: String,
    args: List[SchemeValue]
  ): SchemeValue =
    op match
      case "+" => arith(args, _ + _, 0)
      case "*" => arith(args, _ * _, 1)
      case "-" =>
        args match
          case Nil              => throw new EvalError("-: requires at least 1 argument")
          case IntVal(n) :: Nil => IntVal(-n)
          case IntVal(first) :: rest =>
            IntVal(rest.foldLeft(first) {
              case (acc, IntVal(n)) => acc - n
              case _                => throw new EvalError("-: expected number")
            })
          case _ => throw new EvalError("-: expected number")
      case "/" =>
        args match
          case Nil => throw new EvalError("/: requires at least 1 argument")
          case IntVal(first) :: rest =>
            IntVal(rest.foldLeft(first) {
              case (acc, IntVal(n)) =>
                if n == 0 then throw new EvalError("division by zero")
                acc / n
              case _ => throw new EvalError("/: expected number")
            })
          case _ => throw new EvalError("/: expected number")
      case "<"  => compare(args, _ < _)
      case ">"  => compare(args, _ > _)
      case "="  => compare(args, _ == _)
      case "<=" => compare(args, _ <= _)
      case ">=" => compare(args, _ >= _)
      case "not" =>
        args match
          case v :: Nil => BoolVal(!v.isTruthy)
          case _        => throw new EvalError("not: requires 1 argument")
      case _ =>
        throw new EvalError(s"unknown procedure: $op")

  private def arith(
    args: List[SchemeValue],
    op: (Long, Long) => Long,
    identity: Long
  ): SchemeValue =
    IntVal(args.foldLeft(identity) {
      case (acc, IntVal(n)) => op(acc, n)
      case _                => throw new EvalError("arithmetic: expected number")
    })

  private def compare(
    args: List[SchemeValue],
    cmp: (Long, Long) => Boolean
  ): SchemeValue =
    args match
      case IntVal(a) :: IntVal(b) :: Nil => BoolVal(cmp(a, b))
      case _                             => throw new EvalError("comparison: expected 2 numbers")
