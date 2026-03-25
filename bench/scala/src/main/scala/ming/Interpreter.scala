package ming

import scala.annotation.tailrec

private[ming] object Interpreter:

  private type Env = Map[String, Value]
  private val globalEnv: Env = Builtins.globalEnv

  def evaluate(input: String): (Value, String) =
    val expressions = SchemeParser.parseProgram(input)
    if expressions.isEmpty then throw new EvalError("1:1: expected expression")

    val result = expressions.foldLeft[Value](Value.BoolVal(false)) { (_, expr) =>
      eval(expr, globalEnv)
    }
    (result, "")

  private def eval(expr: Expr, env: Env): Value =
    expr match
      case Expr.IntAtom(value, _) =>
        Value.IntVal(value)

      case Expr.BoolAtom(value, _) =>
        Value.BoolVal(value)

      case Expr.StringAtom(value, _) =>
        Value.StringVal(value)

      case Expr.Symbol(name, pos) =>
        env.getOrElse(name, throw EvalError.at(pos, s"unbound variable: $name"))

      case Expr.ListExpr(Nil, pos) =>
        throw EvalError.at(pos, "cannot evaluate an empty list")

      case Expr.ListExpr(Expr.Symbol("and", _) :: args, _) =>
        evalAnd(args, env)

      case Expr.ListExpr(Expr.Symbol("or", _) :: args, _) =>
        evalOr(args, env)

      case Expr.ListExpr(head :: args, pos) =>
        applyProcedure(eval(head, env), args.map(arg => eval(arg, env)), pos)

  @tailrec
  private def evalAnd(
    args: List[Expr],
    env: Env,
    result: Value = Value.BoolVal(true)
  ): Value =
    args match
      case Nil =>
        result

      case _ if !Value.isTruthy(result) =>
        result

      case expr :: rest =>
        evalAnd(rest, env, eval(expr, env))

  @tailrec
  private def evalOr(
    args: List[Expr],
    env: Env,
    result: Value = Value.BoolVal(false)
  ): Value =
    args match
      case Nil =>
        result

      case _ if Value.isTruthy(result) =>
        result

      case expr :: rest =>
        evalOr(rest, env, eval(expr, env))

  private def applyProcedure(proc: Value, args: List[Value], pos: SourcePos): Value =
    proc match
      case Value.Builtin(_, fn) =>
        fn(args, pos)

      case other =>
        throw EvalError.at(pos, s"attempted to call a ${other.typeName} value")
