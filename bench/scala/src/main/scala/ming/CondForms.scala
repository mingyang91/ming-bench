package ming

import Evaluator.{Done, EvalResult}

/** Cond, case, letrec, and letrec* form evaluators. */
private[ming] object CondForms:

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

  def evalLetrec(
    args: List[Value],
    env: Env,
    out: String
  ): EvalResult =
    args match
      case bindings :: body if body.nonEmpty =>
        val bindingList = Evaluator.toList(bindings)
        val names = bindingList.map {
          case Value.PairVal(Value.Symbol(n, _), _, _) => n
          case _                                       => throw new EvalError("bad letrec binding")
        }
        val localEnv = names.foldLeft(env)((e, n) => e.define(n, Value.VoidVal))
        val out2 = bindingList.foldLeft(out) { case (o, binding) =>
          val pair = Evaluator.toList(binding)
          pair match
            case Value.Symbol(name, _) :: valExpr :: Nil =>
              val (v, _, o2) = Evaluator.eval(valExpr, localEnv, o)
              localEnv.set(name, v)
              o2
            case _ => throw new EvalError("bad letrec binding")
        }
        Evaluator.evalBodyTail(body, localEnv, out2)
      case _ => throw new EvalError("bad letrec syntax")

  def evalLetrecStar(
    args: List[Value],
    env: Env,
    out: String
  ): EvalResult =
    args match
      case bindings :: body if body.nonEmpty =>
        val bindingList = Evaluator.toList(bindings)
        val names = bindingList.map {
          case Value.PairVal(Value.Symbol(n, _), _, _) => n
          case _                                       => throw new EvalError("bad letrec* binding")
        }
        val localEnv = names.foldLeft(env)((e, n) => e.define(n, Value.VoidVal))
        val out2 = bindingList.foldLeft(out) { case (o, binding) =>
          val pair = Evaluator.toList(binding)
          pair match
            case Value.Symbol(name, _) :: valExpr :: Nil =>
              val (v, _, o2) = Evaluator.eval(valExpr, localEnv, o)
              localEnv.set(name, v)
              o2
            case _ => throw new EvalError("bad letrec* binding")
        }
        Evaluator.evalBodyTail(body, localEnv, out2)
      case _ => throw new EvalError("bad letrec* syntax")

  def evalCase(
    args: List[Value],
    env: Env,
    out: String
  ): EvalResult =
    args match
      case keyExpr :: clauses if clauses.nonEmpty =>
        val (keyVal, _, out2) = Evaluator.eval(keyExpr, env, out)
        evalCaseClauses(keyVal, clauses, env, out2)
      case _ => throw new EvalError("bad case syntax")

  @scala.annotation.tailrec
  private def evalCaseClauses(
    key: Value,
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
          case datums :: body if body.nonEmpty =>
            val datumList = Evaluator.toList(datums)
            if datumList.exists(d => StringBuiltins.schemeEq(key, d)) then Evaluator.evalBodyTail(body, env, out)
            else evalCaseClauses(key, rest, env, out)
          case _ => throw new EvalError("bad case clause")
