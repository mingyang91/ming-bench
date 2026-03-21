package ming

import Evaluator.{Bounce, Done, EvalResult}

/** Special form evaluators extracted from Evaluator. */
private[ming] object Forms:

  def evalDisplayForm(
    args: List[Value],
    env: Env,
    out: String,
    fmt: Value => String
  ): EvalResult =
    args match
      case arg :: Nil =>
        val (v, _, out2) = Evaluator.eval(arg, env, out)
        Done(Value.VoidVal, env, out2 + fmt(v))
      case _ =>
        throw new EvalError("display/write requires exactly 1 argument")

  def evalSet(
    args: List[Value],
    env: Env,
    pos: Option[(Int, Int)],
    out: String
  ): EvalResult =
    args match
      case Value.Symbol(name, namePos) :: valueExpr :: Nil =>
        val (v, _, out2) = Evaluator.eval(valueExpr, env, out)
        env.set(name, v, namePos.orElse(pos))
        Done(Value.VoidVal, env, out2)
      case _ =>
        throw EvalError.withPos("bad set! syntax", pos)

  def evalDefine(
    args: List[Value],
    env: Env,
    pos: Option[(Int, Int)],
    out: String
  ): EvalResult =
    args match
      case Value.PairVal(
            Value.Symbol(name, _),
            paramsList,
            _
          ) :: body =>
        val params = extractParams(paramsList)
        val envRef = () => env
        val lambda = Value.LambdaVal(params, body, envRef, Some(name))
        Done(Value.VoidVal, env.define(name, lambda), out)
      case Value.Symbol(name, _) :: valueExpr :: Nil =>
        val (v, _, out2) = Evaluator.eval(valueExpr, env, out)
        Done(Value.VoidVal, env.define(name, v), out2)
      case _ =>
        throw EvalError.withPos("bad define syntax", pos)

  def evalIf(
    args: List[Value],
    env: Env,
    pos: Option[(Int, Int)],
    out: String
  ): EvalResult =
    args match
      case cond :: thenBranch :: elseBranch =>
        val (condVal, _, out2) = Evaluator.eval(cond, env, out)
        if !Evaluator.isFalsy(condVal) then Bounce(thenBranch, env, out2)
        else
          elseBranch match
            case eb :: Nil => Bounce(eb, env, out2)
            case Nil       => Done(Value.VoidVal, env, out2)
            case _ =>
              throw EvalError.withPos("bad if syntax", pos)
      case _ => throw EvalError.withPos("bad if syntax", pos)

  def evalQuote(
    args: List[Value],
    env: Env,
    out: String
  ): EvalResult =
    args match
      case datum :: Nil => Done(datum, env, out)
      case _ =>
        throw new EvalError("quote requires exactly 1 argument")

  def makeLambda(args: List[Value], env: Env): Value =
    args match
      case paramExpr :: body if body.nonEmpty =>
        val params = extractParams(paramExpr)
        val envRef = () => env
        Value.LambdaVal(params, body, envRef, None)
      case _ => throw new EvalError("bad lambda syntax")

  private def extractParams(paramExpr: Value): List[String] =
    Evaluator.toList(paramExpr).map {
      case Value.Symbol(s, _) => s
      case other =>
        throw new EvalError(
          s"expected symbol in parameter list, got: ${other.display}"
        )
    }

  def evalAnd(
    args: List[Value],
    env: Env,
    out: String
  ): EvalResult =
    args match
      case Nil => Done(Value.BoolVal(true), env, out)
      case head :: Nil =>
        Bounce(head, env, out)
      case head :: tail =>
        val (v, _, out2) = Evaluator.eval(head, env, out)
        if Evaluator.isFalsy(v) then Done(v, env, out2)
        else evalAnd(tail, env, out2)

  def evalOr(
    args: List[Value],
    env: Env,
    out: String
  ): EvalResult =
    args match
      case Nil => Done(Value.BoolVal(false), env, out)
      case head :: Nil =>
        Bounce(head, env, out)
      case head :: tail =>
        val (v, _, out2) = Evaluator.eval(head, env, out)
        if !Evaluator.isFalsy(v) then Done(v, env, out2)
        else evalOr(tail, env, out2)

  def evalNotInner(
    args: List[Value],
    env: Env,
    out: String
  ): (Value, String) =
    args match
      case head :: Nil =>
        val (v, _, out2) = Evaluator.eval(head, env, out)
        (Value.BoolVal(Evaluator.isFalsy(v)), out2)
      case _ =>
        throw new EvalError("not requires exactly 1 argument")

  def evalLet(
    args: List[Value],
    env: Env,
    out: String
  ): EvalResult =
    args match
      case Value.Symbol(name, _) :: bindings :: body if body.nonEmpty =>
        evalNamedLet(name, bindings, body, env, out)
      case bindings :: body if body.nonEmpty =>
        evalRegularLet(bindings, body, env, out)
      case _ => throw new EvalError("bad let syntax")

  private def evalNamedLet(
    name: String,
    bindings: Value,
    body: List[Value],
    env: Env,
    out: String
  ): EvalResult =
    val bindingList = Evaluator.toList(bindings)
    val (params, inits, out2) =
      bindingList.foldLeft(
        (List.empty[String], List.empty[Value], out)
      ) { case ((ps, is, o), binding) =>
        val pair = Evaluator.toList(binding)
        pair match
          case Value.Symbol(p, _) :: initExpr :: Nil =>
            val (v, _, o2) = Evaluator.eval(initExpr, env, o)
            (ps :+ p, is :+ v, o2)
          case _ => throw new EvalError("bad let binding")
      }
    val envRef = () => env
    val lambda = Value.LambdaVal(params, body, envRef, Some(name))
    Evaluator.applyProcTail(lambda, inits, None, out2)

  private def evalRegularLet(
    bindings: Value,
    body: List[Value],
    env: Env,
    out: String
  ): EvalResult =
    val bindingList = Evaluator.toList(bindings)
    val (localEnv, out2) =
      bindingList.foldLeft((env, out)) { case ((acc, o), binding) =>
        val pair = Evaluator.toList(binding)
        pair match
          case Value.Symbol(name, _) :: valExpr :: Nil =>
            val (v, _, o2) = Evaluator.eval(valExpr, env, o)
            (acc.define(name, v), o2)
          case _ => throw new EvalError("bad let binding")
      }
    Evaluator.evalBodyTail(body, localEnv, out2)

  def evalCond(
    clauses: List[Value],
    env: Env,
    out: String
  ): EvalResult =
    clauses match
      case Nil => Done(Value.VoidVal, env, out)
      case clause :: rest =>
        val parts = Evaluator.toList(clause)
        parts match
          case Value.Symbol("else", _) :: body =>
            Evaluator.evalBodyTail(body, env, out)
          case test :: body =>
            val (testVal, _, out2) = Evaluator.eval(test, env, out)
            if !Evaluator.isFalsy(testVal) then Evaluator.evalBodyTail(body, env, out2)
            else evalCond(rest, env, out2)
          case _ => throw new EvalError("bad cond clause")
