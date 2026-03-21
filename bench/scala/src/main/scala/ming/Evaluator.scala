package ming

/** Scheme interpreter entry point. */
object Evaluator:

  def evalStr(input: String): String =
    val exprs = Parser.parse(input)
    if exprs.isEmpty then throw new EvalError("no expressions")
    val (lastVal, _) = evalAll(exprs, Builtins.defaultEnv)
    lastVal.display

  def evalStrWithOutput(input: String): (String, String) =
    val result = evalStr(input)
    (result, "")

  private def evalAll(
    exprs: List[Value],
    env: Env
  ): (Value, Env) =
    exprs match
      case Nil         => (Value.VoidVal, env)
      case head :: Nil => eval(head, env)
      case head :: tail =>
        val (_, newEnv) = eval(head, env)
        evalAll(tail, newEnv)

  private def eval(expr: Value, env: Env): (Value, Env) =
    expr match
      case Value.IntVal(_)    => (expr, env)
      case Value.BoolVal(_)   => (expr, env)
      case Value.StringVal(_) => (expr, env)
      case Value.NilVal       => (expr, env)
      case Value.VoidVal      => (expr, env)
      case _: Value.LambdaVal => (expr, env)
      case Value.Symbol(name, pos) =>
        (env.lookup(name, pos), env)
      case Value.PairVal(car, _, pos) =>
        val args = toList(expr).tail
        evalForm(car, args, env, pos)

  private def evalForm(
    op: Value,
    args: List[Value],
    env: Env,
    pos: Option[(Int, Int)]
  ): (Value, Env) =
    op match
      case Value.Symbol("define", _) => evalDefine(args, env, pos)
      case Value.Symbol("if", _)     => evalIf(args, env, pos)
      case Value.Symbol("quote", _)  => evalQuote(args, env)
      case Value.Symbol("lambda", _) => evalLambda(args, env)
      case Value.Symbol("let", _)    => evalLet(args, env)
      case Value.Symbol("begin", _)  => evalBegin(args, env)
      case Value.Symbol("cond", _)   => evalCond(args, env)
      case Value.Symbol("and", _)    => (evalAnd(args, env), env)
      case Value.Symbol("or", _)     => (evalOr(args, env), env)
      case Value.Symbol("not", _)    => (evalNot(args, env), env)
      case _ =>
        val (proc, _)  = eval(op, env)
        val evaledArgs = args.map(a => eval(a, env)._1)
        (applyProc(proc, evaledArgs, pos), env)

  private def evalDefine(
    args: List[Value],
    env: Env,
    pos: Option[(Int, Int)]
  ): (Value, Env) =
    args match
      case Value.PairVal(
            Value.Symbol(name, _),
            paramsList,
            _
          ) :: body =>
        val params = toList(paramsList).map {
          case Value.Symbol(s, _) => s
          case other =>
            throw new EvalError(
              s"expected symbol in parameter list, got: ${other.display}"
            )
        }
        val lambda = Value.LambdaVal(params, body, env, Some(name))
        (Value.VoidVal, env.define(name, lambda))
      case Value.Symbol(name, _) :: valueExpr :: Nil =>
        val (v, _) = eval(valueExpr, env)
        (Value.VoidVal, env.define(name, v))
      case _ =>
        throw EvalError.withPos("bad define syntax", pos)

  private def evalIf(
    args: List[Value],
    env: Env,
    pos: Option[(Int, Int)]
  ): (Value, Env) =
    args match
      case cond :: thenBranch :: elseBranch =>
        val (condVal, _) = eval(cond, env)
        if !isFalsy(condVal) then eval(thenBranch, env)
        else
          elseBranch match
            case eb :: Nil => eval(eb, env)
            case Nil       => (Value.VoidVal, env)
            case _ =>
              throw EvalError.withPos("bad if syntax", pos)
      case _ => throw EvalError.withPos("bad if syntax", pos)

  private def evalQuote(
    args: List[Value],
    env: Env
  ): (Value, Env) =
    args match
      case datum :: Nil => (datum, env)
      case _ =>
        throw new EvalError("quote requires exactly 1 argument")

  private def evalLambda(
    args: List[Value],
    env: Env
  ): (Value, Env) =
    args match
      case paramExpr :: body if body.nonEmpty =>
        val params = toList(paramExpr).map {
          case Value.Symbol(s, _) => s
          case other =>
            throw new EvalError(
              s"expected symbol in parameter list, got: ${other.display}"
            )
        }
        (Value.LambdaVal(params, body, env, None), env)
      case _ => throw new EvalError("bad lambda syntax")

  private def applyProc(
    proc: Value,
    args: List[Value],
    pos: Option[(Int, Int)]
  ): Value =
    proc match
      case lam @ Value.LambdaVal(
            params,
            body,
            closure,
            nameOpt
          ) =>
        val closureWithSelf = nameOpt match
          case Some(n) => closure.define(n, lam)
          case None    => closure
        val localEnv    = closureWithSelf.extend(params, args, pos)
        val (result, _) = evalAll(body, localEnv)
        result
      case Value.Symbol(name, _) =>
        Builtins.applyBuiltin(name, args, pos)
      case _ =>
        throw EvalError.withPos(
          s"not a procedure: ${proc.display}",
          pos
        )

  private def evalAnd(args: List[Value], env: Env): Value =
    args match
      case Nil         => Value.BoolVal(true)
      case head :: Nil => eval(head, env)._1
      case head :: tail =>
        val v = eval(head, env)._1
        if isFalsy(v) then v else evalAnd(tail, env)

  private def evalOr(args: List[Value], env: Env): Value =
    args match
      case Nil         => Value.BoolVal(false)
      case head :: Nil => eval(head, env)._1
      case head :: tail =>
        val v = eval(head, env)._1
        if !isFalsy(v) then v else evalOr(tail, env)

  private def evalNot(args: List[Value], env: Env): Value =
    args match
      case head :: Nil =>
        Value.BoolVal(isFalsy(eval(head, env)._1))
      case _ =>
        throw new EvalError("not requires exactly 1 argument")

  private def evalLet(
    args: List[Value],
    env: Env
  ): (Value, Env) =
    args match
      case bindings :: body if body.nonEmpty =>
        val bindingList = toList(bindings)
        val localEnv = bindingList.foldLeft(env) { (acc, binding) =>
          val pair = toList(binding)
          pair match
            case Value.Symbol(name, _) :: valExpr :: Nil =>
              val (v, _) = eval(valExpr, env)
              acc.define(name, v)
            case _ => throw new EvalError("bad let binding")
        }
        val (result, _) = evalAll(body, localEnv)
        (result, env)
      case _ => throw new EvalError("bad let syntax")

  private def evalBegin(
    args: List[Value],
    env: Env
  ): (Value, Env) =
    evalAll(args, env)

  private def evalCond(
    clauses: List[Value],
    env: Env
  ): (Value, Env) =
    clauses match
      case Nil => (Value.VoidVal, env)
      case clause :: rest =>
        val parts = toList(clause)
        parts match
          case Value.Symbol("else", _) :: body =>
            evalAll(body, env)
          case test :: body =>
            val (testVal, _) = eval(test, env)
            if !isFalsy(testVal) then evalAll(body, env)
            else evalCond(rest, env)
          case _ => throw new EvalError("bad cond clause")

  private def isFalsy(v: Value): Boolean = v match
    case Value.BoolVal(false) => true
    case _                    => false

  private def toList(v: Value): List[Value] = v match
    case Value.NilVal           => Nil
    case Value.PairVal(h, t, _) => h :: toList(t)
    case other                  => List(other)
