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
        val (params, rest) = extractParamsWithRest(paramsList)
        val envRef         = () => env
        val lambda         = Value.LambdaVal(params, body, envRef, Some(name), rest)
        Done(Value.VoidVal, env.define(name, lambda), out)
      case Value.Symbol(name, _) :: valueExpr :: Nil =>
        val (v, _, out2) = Evaluator.eval(valueExpr, env, out)
        val named = v match
          case cl: Value.CaseLambdaVal => cl.copy(name = Some(name))
          case other                   => other
        Done(Value.VoidVal, env.define(name, named), out2)
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
        val (params, rest) = extractParamsWithRest(paramExpr)
        val envRef         = () => env
        Value.LambdaVal(params, body, envRef, None, rest)
      case _ => throw new EvalError("bad lambda syntax")

  def makeCaseLambda(args: List[Value], env: Env): Value =
    val clauses = args.map { clause =>
      val elems = Evaluator.toList(clause)
      elems match
        case paramExpr :: body if body.nonEmpty =>
          val (params, rest) = extractParamsWithRest(paramExpr)
          (params, rest, body)
        case _ => throw new EvalError("bad case-lambda clause")
    }
    val envRef = () => env
    Value.CaseLambdaVal(clauses, envRef, None)

  private def extractParams(paramExpr: Value): List[String] =
    extractParamsWithRest(paramExpr)._1

  private[ming] def extractParamsWithRest(
    paramExpr: Value
  ): (List[String], Option[String]) =
    paramExpr match
      case Value.NilVal          => (Nil, None)
      case Value.Symbol(name, _) => (Nil, Some(name))
      case Value.PairVal(_, _, _) =>
        val (elems, tail) = collectParamPairs(paramExpr)
        val rest = tail match
          case Value.NilVal       => None
          case Value.Symbol(n, _) => Some(n)
          case _                  => throw new EvalError("invalid rest parameter")
        (elems, rest)
      case _ => throw new EvalError("invalid parameter list")

  private def collectParamPairs(
    v: Value
  ): (List[String], Value) =
    v match
      case Value.PairVal(Value.Symbol(name, _), cdr, _) =>
        val (rest, tail) = collectParamPairs(cdr)
        (name :: rest, tail)
      case other => (Nil, other)

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
