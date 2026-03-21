package ming

/** Scheme interpreter entry point. */
object Evaluator:

  def evalStr(input: String): String =
    evalStrWithOutput(input)._1

  def evalStrWithOutput(input: String): (String, String) =
    val exprs = Parser.parse(input)
    if exprs.isEmpty then throw new EvalError("no expressions")
    val (lastVal, _, output) = evalAll(exprs, Builtins.defaultEnv, "")
    (lastVal.display, output)

  private def evalAll(
    exprs: List[Value],
    env: Env,
    out: String
  ): (Value, Env, String) =
    exprs match
      case Nil         => (Value.VoidVal, env, out)
      case head :: Nil => eval(head, env, out)
      case head :: tail =>
        val (_, newEnv, out2) = eval(head, env, out)
        evalAll(tail, newEnv, out2)

  private def eval(
    expr: Value,
    env: Env,
    out: String
  ): (Value, Env, String) =
    expr match
      case Value.IntVal(_)           => (expr, env, out)
      case Value.BoolVal(_)          => (expr, env, out)
      case Value.StringVal(_)        => (expr, env, out)
      case Value.CharVal(_)          => (expr, env, out)
      case Value.NilVal              => (expr, env, out)
      case Value.VoidVal             => (expr, env, out)
      case _: Value.MutableStringVal => (expr, env, out)
      case _: Value.LambdaVal        => (expr, env, out)
      case Value.Symbol(name, pos) =>
        (env.lookup(name, pos), env, out)
      case Value.PairVal(car, _, pos) =>
        val args = toList(expr).tail
        evalForm(car, args, env, pos, out)

  private def evalForm(
    op: Value,
    args: List[Value],
    env: Env,
    pos: Option[(Int, Int)],
    out: String
  ): (Value, Env, String) =
    op match
      case Value.Symbol("define", _) => evalDefine(args, env, pos, out)
      case Value.Symbol("if", _)     => evalIf(args, env, pos, out)
      case Value.Symbol("quote", _)  => evalQuote(args, env, out)
      case Value.Symbol("lambda", _) =>
        (makeLambda(args, env), env, out)
      case Value.Symbol("let", _)   => evalLet(args, env, out)
      case Value.Symbol("begin", _) => evalAll(args, env, out)
      case Value.Symbol("cond", _)  => evalCond(args, env, out)
      case Value.Symbol("and", _) =>
        val (v, out2) = evalAnd(args, env, out)
        (v, env, out2)
      case Value.Symbol("or", _) =>
        val (v, out2) = evalOr(args, env, out)
        (v, env, out2)
      case Value.Symbol("not", _) =>
        val (v, out2) = evalNot(args, env, out)
        (v, env, out2)
      case Value.Symbol("display", _) =>
        evalDisplayForm(args, env, out, _.displayRepr)
      case Value.Symbol("write", _) =>
        evalDisplayForm(args, env, out, _.display)
      case Value.Symbol("newline", _) =>
        (Value.VoidVal, env, out + "\n")
      case _ =>
        val (proc, _, out2)    = eval(op, env, out)
        val (evaledArgs, out3) = evalArgs(args, env, out2)
        val (result, out4)     = applyProc(proc, evaledArgs, pos, out3)
        (result, env, out4)

  private def evalDisplayForm(
    args: List[Value],
    env: Env,
    out: String,
    fmt: Value => String
  ): (Value, Env, String) =
    args match
      case arg :: Nil =>
        val (v, _, out2) = eval(arg, env, out)
        (Value.VoidVal, env, out2 + fmt(v))
      case _ =>
        throw new EvalError("display/write requires exactly 1 argument")

  private def evalArgs(
    args: List[Value],
    env: Env,
    out: String
  ): (List[Value], String) =
    args.foldLeft((List.empty[Value], out)) { case ((acc, o), arg) =>
      val (v, _, o2) = eval(arg, env, o)
      (acc :+ v, o2)
    }

  private def evalDefine(
    args: List[Value],
    env: Env,
    pos: Option[(Int, Int)],
    out: String
  ): (Value, Env, String) =
    args match
      case Value.PairVal(
            Value.Symbol(name, _),
            paramsList,
            _
          ) :: body =>
        val params = extractParams(paramsList)
        val lambda = Value.LambdaVal(params, body, env, Some(name))
        (Value.VoidVal, env.define(name, lambda), out)
      case Value.Symbol(name, _) :: valueExpr :: Nil =>
        val (v, _, out2) = eval(valueExpr, env, out)
        (Value.VoidVal, env.define(name, v), out2)
      case _ =>
        throw EvalError.withPos("bad define syntax", pos)

  private def evalIf(
    args: List[Value],
    env: Env,
    pos: Option[(Int, Int)],
    out: String
  ): (Value, Env, String) =
    args match
      case cond :: thenBranch :: elseBranch =>
        val (condVal, _, out2) = eval(cond, env, out)
        if !isFalsy(condVal) then eval(thenBranch, env, out2)
        else
          elseBranch match
            case eb :: Nil => eval(eb, env, out2)
            case Nil       => (Value.VoidVal, env, out2)
            case _ =>
              throw EvalError.withPos("bad if syntax", pos)
      case _ => throw EvalError.withPos("bad if syntax", pos)

  private def evalQuote(
    args: List[Value],
    env: Env,
    out: String
  ): (Value, Env, String) =
    args match
      case datum :: Nil => (datum, env, out)
      case _ =>
        throw new EvalError("quote requires exactly 1 argument")

  private def makeLambda(args: List[Value], env: Env): Value =
    args match
      case paramExpr :: body if body.nonEmpty =>
        val params = extractParams(paramExpr)
        Value.LambdaVal(params, body, env, None)
      case _ => throw new EvalError("bad lambda syntax")

  private def extractParams(paramExpr: Value): List[String] =
    toList(paramExpr).map {
      case Value.Symbol(s, _) => s
      case other =>
        throw new EvalError(
          s"expected symbol in parameter list, got: ${other.display}"
        )
    }

  private def applyProc(
    proc: Value,
    args: List[Value],
    pos: Option[(Int, Int)],
    out: String
  ): (Value, String) =
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
        val localEnv          = closureWithSelf.extend(params, args, pos)
        val (result, _, out2) = evalAll(body, localEnv, out)
        (result, out2)
      case Value.Symbol(name, _) =>
        (Builtins.applyBuiltin(name, args, pos), out)
      case _ =>
        throw EvalError.withPos(
          s"not a procedure: ${proc.display}",
          pos
        )

  private def evalAnd(
    args: List[Value],
    env: Env,
    out: String
  ): (Value, String) =
    args match
      case Nil => (Value.BoolVal(true), out)
      case head :: Nil =>
        val (v, _, out2) = eval(head, env, out)
        (v, out2)
      case head :: tail =>
        val (v, _, out2) = eval(head, env, out)
        if isFalsy(v) then (v, out2) else evalAnd(tail, env, out2)

  private def evalOr(
    args: List[Value],
    env: Env,
    out: String
  ): (Value, String) =
    args match
      case Nil => (Value.BoolVal(false), out)
      case head :: Nil =>
        val (v, _, out2) = eval(head, env, out)
        (v, out2)
      case head :: tail =>
        val (v, _, out2) = eval(head, env, out)
        if !isFalsy(v) then (v, out2) else evalOr(tail, env, out2)

  private def evalNot(
    args: List[Value],
    env: Env,
    out: String
  ): (Value, String) =
    args match
      case head :: Nil =>
        val (v, _, out2) = eval(head, env, out)
        (Value.BoolVal(isFalsy(v)), out2)
      case _ =>
        throw new EvalError("not requires exactly 1 argument")

  private def evalLet(
    args: List[Value],
    env: Env,
    out: String
  ): (Value, Env, String) =
    args match
      case bindings :: body if body.nonEmpty =>
        val bindingList = toList(bindings)
        val (localEnv, out2) =
          bindingList.foldLeft((env, out)) { case ((acc, o), binding) =>
            val pair = toList(binding)
            pair match
              case Value.Symbol(name, _) :: valExpr :: Nil =>
                val (v, _, o2) = eval(valExpr, env, o)
                (acc.define(name, v), o2)
              case _ => throw new EvalError("bad let binding")
          }
        val (result, _, out3) = evalAll(body, localEnv, out2)
        (result, env, out3)
      case _ => throw new EvalError("bad let syntax")

  private def evalCond(
    clauses: List[Value],
    env: Env,
    out: String
  ): (Value, Env, String) =
    clauses match
      case Nil => (Value.VoidVal, env, out)
      case clause :: rest =>
        val parts = toList(clause)
        parts match
          case Value.Symbol("else", _) :: body =>
            evalAll(body, env, out)
          case test :: body =>
            val (testVal, _, out2) = eval(test, env, out)
            if !isFalsy(testVal) then evalAll(body, env, out2)
            else evalCond(rest, env, out2)
          case _ => throw new EvalError("bad cond clause")

  private def isFalsy(v: Value): Boolean = v match
    case Value.BoolVal(false) => true
    case _                    => false

  private def toList(v: Value): List[Value] = v match
    case Value.NilVal           => Nil
    case Value.PairVal(h, t, _) => h :: toList(t)
    case other                  => List(other)
