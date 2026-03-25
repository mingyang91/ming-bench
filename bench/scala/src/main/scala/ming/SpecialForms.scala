package ming

import Evaluator.{Bounce, Cont, Done, More}

/** Special form evaluators extracted from Evaluator to keep file sizes under limits. */
object SpecialForms:

  def evalDefineK(args: List[SchemeVal], env: Env, k: Cont): Bounce =
    args match
      case SchemeVal.Symbol(name) :: value :: Nil =>
        Evaluator.evalK(
          value,
          env,
          v =>
            env.define(name, v); k(SchemeVal.Void)
        )
      case SchemeVal.SList(elems) :: body if elems.nonEmpty && body.nonEmpty =>
        elems.head match
          case SchemeVal.Symbol(name) =>
            val (params, rest) = parseParams(elems.tail)
            env.define(name, SchemeVal.LambdaProc(params, body, env, rest))
            k(SchemeVal.Void)
          case other => throw new EvalError(s"define: expected name, got $other")
      case SchemeVal.DottedList(elems, SchemeVal.Symbol(restParam)) :: body if elems.nonEmpty && body.nonEmpty =>
        elems.head match
          case SchemeVal.Symbol(name) =>
            val params = elems.tail.map {
              case SchemeVal.Symbol(p) => p
              case other               => throw new EvalError(s"expected parameter name, got $other")
            }
            env.define(name, SchemeVal.LambdaProc(params, body, env, Some(restParam)))
            k(SchemeVal.Void)
          case other => throw new EvalError(s"define: expected name, got $other")
      case _ => throw new EvalError("define: bad syntax")

  def evalIfK(args: List[SchemeVal], env: Env, k: Cont): Bounce =
    args match
      case cond :: thenBranch :: elseBranch :: Nil =>
        Evaluator.evalK(
          cond,
          env,
          condVal =>
            if Evaluator.isTruthy(condVal) then More(() => Evaluator.evalK(thenBranch, env, k))
            else More(() => Evaluator.evalK(elseBranch, env, k))
        )
      case cond :: thenBranch :: Nil =>
        Evaluator.evalK(
          cond,
          env,
          condVal =>
            if Evaluator.isTruthy(condVal) then More(() => Evaluator.evalK(thenBranch, env, k))
            else k(SchemeVal.Void)
        )
      case _ => throw new EvalError("if: bad syntax")

  def evalAndK(exprs: List[SchemeVal], env: Env, k: Cont): Bounce =
    exprs match
      case Nil         => k(SchemeVal.BoolVal(true))
      case last :: Nil => More(() => Evaluator.evalK(last, env, k))
      case head :: tail =>
        Evaluator.evalK(
          head,
          env,
          v =>
            if !Evaluator.isTruthy(v) then k(v)
            else More(() => evalAndK(tail, env, k))
        )

  def evalOrK(exprs: List[SchemeVal], env: Env, k: Cont): Bounce =
    exprs match
      case Nil         => k(SchemeVal.BoolVal(false))
      case last :: Nil => More(() => Evaluator.evalK(last, env, k))
      case head :: tail =>
        Evaluator.evalK(
          head,
          env,
          v =>
            if Evaluator.isTruthy(v) then k(v)
            else More(() => evalOrK(tail, env, k))
        )

  def evalSetK(args: List[SchemeVal], env: Env, k: Cont): Bounce =
    args match
      case SchemeVal.Symbol(name) :: value :: Nil =>
        Evaluator.evalK(
          value,
          env,
          v =>
            env.set(name, v); k(SchemeVal.Void)
        )
      case _ => throw new EvalError("set!: bad syntax")

  def parseParams(paramList: List[SchemeVal]): (List[String], Option[String]) =
    val dotIdx = paramList.indexWhere {
      case SchemeVal.Symbol(".") => true
      case _                     => false
    }
    if dotIdx >= 0 then
      val fixed = paramList.take(dotIdx).map {
        case SchemeVal.Symbol(p) => p
        case other               => throw new EvalError(s"expected parameter name, got $other")
      }
      paramList.drop(dotIdx + 1) match
        case SchemeVal.Symbol(rest) :: Nil => (fixed, Some(rest))
        case _                             => throw new EvalError("bad dot syntax in parameter list")
    else
      val params = paramList.map {
        case SchemeVal.Symbol(p) => p
        case other               => throw new EvalError(s"expected parameter name, got $other")
      }
      (params, None)

  def evalLambda(args: List[SchemeVal], env: Env): SchemeVal =
    args match
      case SchemeVal.SList(paramList) :: body if body.nonEmpty =>
        val (params, rest) = parseParams(paramList)
        SchemeVal.LambdaProc(params, body, env, rest)
      case SchemeVal.DottedList(paramList, SchemeVal.Symbol(restParam)) :: body if body.nonEmpty =>
        val params = paramList.map {
          case SchemeVal.Symbol(p) => p
          case other               => throw new EvalError(s"expected parameter name, got $other")
        }
        SchemeVal.LambdaProc(params, body, env, Some(restParam))
      case SchemeVal.Symbol(restParam) :: body if body.nonEmpty =>
        SchemeVal.LambdaProc(Nil, body, env, Some(restParam))
      case _ => throw new EvalError("lambda: bad syntax")

  def evalDefineSyntax(args: List[SchemeVal], env: Env): Unit =
    args match
      case SchemeVal.Symbol(name) :: SchemeVal.SList(srElems) :: Nil =>
        srElems.head match
          case SchemeVal.Symbol("syntax-rules") =>
            val literals = srElems(1) match
              case SchemeVal.SList(lits) =>
                lits.map {
                  case SchemeVal.Symbol(s) => s
                  case other               => throw new EvalError(s"syntax-rules: expected literal, got $other")
                }
              case _ => throw new EvalError("syntax-rules: expected literal list")
            val rules = srElems.drop(2).map {
              case SchemeVal.SList(List(pattern, template)) => (pattern, template)
              case _ => throw new EvalError("syntax-rules: expected (pattern template) clause")
            }
            env.define(name, SchemeVal.MacroVal(name, literals, rules, env))
          case _ => throw new EvalError("define-syntax: expected syntax-rules")
      case _ => throw new EvalError("define-syntax: bad syntax")

  def evalCaseLambda(clauses: List[SchemeVal], env: Env): SchemeVal =
    val parsed = clauses.map {
      case SchemeVal.SList(elems) if elems.nonEmpty =>
        elems.head match
          case SchemeVal.SList(paramList) =>
            val (params, rest) = parseParams(paramList)
            (params, rest, elems.tail)
          case SchemeVal.DottedList(paramList, SchemeVal.Symbol(restParam)) =>
            val params = paramList.map {
              case SchemeVal.Symbol(p) => p
              case other               => throw new EvalError(s"expected parameter name, got $other")
            }
            (params, Some(restParam), elems.tail)
          case SchemeVal.Symbol(restParam) =>
            (Nil, Some(restParam), elems.tail)
          case _ => throw new EvalError("case-lambda: bad clause")
      case _ => throw new EvalError("case-lambda: bad clause")
    }
    SchemeVal.CaseLambdaProc(parsed, env)
