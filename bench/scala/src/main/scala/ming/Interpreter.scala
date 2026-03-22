package ming

import SchemeValue.*

object Interpreter:

  type Env = Map[String, SchemeValue]

  val defaultEnv: Env = Map.empty

  def eval(expr: SchemeValue, env: Env): SchemeValue =
    expr match
      case IntVal(_)    => expr
      case BoolVal(_)   => expr
      case StringVal(_) => expr
      case Void         => expr
      case SymbolVal(name) =>
        env.getOrElse(name, throw new EvalError(s"unbound variable: $name"))
      case ListVal(Nil)                      => expr
      case ListVal(SymbolVal("and") :: args) => evalAnd(args, env)
      case ListVal(SymbolVal("or") :: args)  => evalOr(args, env)
      case ListVal(head :: args) =>
        val func = eval(head, env)
        applyBuiltin(func, args.map(a => eval(a, env)))
      case ListVal(_) => throw new EvalError("invalid expression")

  private def evalAnd(args: List[SchemeValue], env: Env): SchemeValue =
    args match
      case Nil         => BoolVal(true)
      case last :: Nil => eval(last, env)
      case head :: tail =>
        val result = eval(head, env)
        if result.isTruthy then evalAnd(tail, env)
        else result

  private def evalOr(args: List[SchemeValue], env: Env): SchemeValue =
    args match
      case Nil         => BoolVal(false)
      case last :: Nil => eval(last, env)
      case head :: tail =>
        val result = eval(head, env)
        if result.isTruthy then result
        else evalOr(tail, env)

  private def applyBuiltin(
    func: SchemeValue,
    args: List[SchemeValue]
  ): SchemeValue =
    func match
      case SymbolVal(name) =>
        name match
          case "+" => arithOp(args, 0L, _ + _)
          case "-" =>
            args match
              case Nil              => throw new EvalError("-: need at least 1 argument")
              case IntVal(n) :: Nil => IntVal(-n)
              case _                => arithOp(args.tail, asInt(args.head), _ - _)
          case "*" => arithOp(args, 1L, _ * _)
          case "/" =>
            args match
              case Nil => throw new EvalError("/: need at least 1 argument")
              case _ =>
                args.tail.foldLeft(asInt(args.head)) { (acc, v) =>
                  val d = asInt(v)
                  if d == 0 then throw new EvalError("division by zero")
                  else acc / d
                } |> IntVal.apply
          case "<"  => cmpOp(args, _ < _)
          case ">"  => cmpOp(args, _ > _)
          case "="  => cmpOp(args, _ == _)
          case "<=" => cmpOp(args, _ <= _)
          case ">=" => cmpOp(args, _ >= _)
          case "not" =>
            args match
              case v :: Nil => BoolVal(!v.isTruthy)
              case _        => throw new EvalError("not: expects 1 argument")
          case _ => throw new EvalError(s"unknown procedure: $name")
      case _ => throw new EvalError("not a procedure")

  private def asInt(v: SchemeValue): Long = v match
    case IntVal(n) => n
    case other     => throw new EvalError(s"expected number, got: ${other.display}")

  private def arithOp(
    args: List[SchemeValue],
    init: Long,
    op: (Long, Long) => Long
  ): SchemeValue =
    IntVal(args.foldLeft(init)((acc, v) => op(acc, asInt(v))))

  private def cmpOp(
    args: List[SchemeValue],
    op: (Long, Long) => Boolean
  ): SchemeValue =
    args match
      case a :: b :: Nil => BoolVal(op(asInt(a), asInt(b)))
      case _             => throw new EvalError("comparison expects 2 arguments")

  extension [A](a: A) private def |>[B](f: A => B): B = f(a)
