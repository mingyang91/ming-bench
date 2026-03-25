package ming

import Evaluator.State

/** CEK step functions for define, cond, let, letrec, letrec*, and let*. */
object LetForms:

  def evalDefineStep(args: List[SchemeVal], env: Env, k: Cont): State =
    args match
      case SchemeVal.SSymbol(name) :: valueExpr :: Nil =>
        State.Ev(valueExpr, env, Cont.DefValK(name, env, k))
      case SchemeVal.SList(SchemeVal.SSymbol(name) :: params) :: body if body.nonEmpty =>
        val (paramNames, restParam) = DefineForms.parseParams(params)
        env.define(name, SchemeVal.SLambda(paramNames, restParam, body, env))
        State.Ko(SchemeVal.SVoid, k)
      case (nameAndParams @ SchemeVal.SPair(_)) :: body if body.nonEmpty =>
        val (name, paramNames, restParam) = DefineForms.parseNameAndParams(nameAndParams)
        env.define(name, SchemeVal.SLambda(paramNames, restParam, body, env))
        State.Ko(SchemeVal.SVoid, k)
      case _ => throw new EvalError("define: bad syntax")

  def evalCondStep(clauses: List[SchemeVal], env: Env, k: Cont): State =
    clauses match
      case Nil => State.Ko(SchemeVal.SVoid, k)
      case clause :: rest =>
        clause match
          case SchemeVal.SList(SchemeVal.SSymbol("else") :: body) =>
            Evaluator.evalBodyCek(body, env, k)
          case SchemeVal.SList(test :: body) =>
            State.Ev(test, env, Cont.CondK(body, rest, env, k))
          case _ => throw new EvalError("cond: bad clause")

  def evalLetStep(args: List[SchemeVal], env: Env, k: Cont): State =
    args match
      case SchemeVal.SSymbol(name) :: SchemeVal.SList(bindings) :: body if body.nonEmpty =>
        // Named let — CPS evaluation of init expressions
        val parsed = bindings.map {
          case SchemeVal.SList(SchemeVal.SSymbol(n) :: initExpr :: Nil) => (n, initExpr)
          case _                                                        => throw new EvalError("let: bad binding")
        }
        val paramNames = parsed.map(_._1)
        parsed match
          case Nil =>
            val letEnv = Env(Some(env))
            letEnv.define(name, SchemeVal.SLambda(paramNames, None, body, letEnv))
            Evaluator.evalBodyCek(body, letEnv, k)
          case (n, expr) :: rest =>
            State.Ev(expr, env, Cont.NamedLetEvalK(name, paramNames, n, Nil, rest, body, env, k))
      case SchemeVal.SList(bindings) :: body if body.nonEmpty =>
        // Regular let — CPS evaluation of init expressions
        val parsed = bindings.map {
          case SchemeVal.SList(SchemeVal.SSymbol(n) :: initExpr :: Nil) => (n, initExpr)
          case _                                                        => throw new EvalError("let: bad binding")
        }
        parsed match
          case Nil =>
            val letEnv = Env(Some(env))
            Evaluator.evalBodyCek(body, letEnv, k)
          case (n, expr) :: rest =>
            State.Ev(expr, env, Cont.LetEvalK(n, Nil, rest, body, env, k))
      case _ => throw new EvalError("let: bad syntax")

  def evalLetrecStep(args: List[SchemeVal], env: Env, k: Cont): State =
    args match
      case SchemeVal.SList(bindings) :: body if body.nonEmpty =>
        val letEnv = Env(Some(env))
        val names = bindings.map {
          case SchemeVal.SList(SchemeVal.SSymbol(n) :: _ :: Nil) => n
          case _                                                 => throw new EvalError("letrec: bad binding")
        }
        names.foreach(n => letEnv.define(n, SchemeVal.SVoid))
        bindings.foreach {
          case SchemeVal.SList(SchemeVal.SSymbol(n) :: initExpr :: Nil) =>
            letEnv.set(n, Evaluator.eval(initExpr, letEnv))
          case _ => throw new EvalError("letrec: bad binding")
        }
        Evaluator.evalBodyCek(body, letEnv, k)
      case _ => throw new EvalError("letrec: bad syntax")

  def evalLetrecStarStep(args: List[SchemeVal], env: Env, k: Cont): State =
    args match
      case SchemeVal.SList(bindings) :: body if body.nonEmpty =>
        val letEnv = Env(Some(env))
        bindings.foreach {
          case SchemeVal.SList(SchemeVal.SSymbol(n) :: initExpr :: Nil) =>
            letEnv.define(n, Evaluator.eval(initExpr, letEnv))
          case _ => throw new EvalError("letrec*: bad binding")
        }
        Evaluator.evalBodyCek(body, letEnv, k)
      case _ => throw new EvalError("letrec*: bad syntax")

  def evalLetStarStep(args: List[SchemeVal], env: Env, k: Cont): State =
    args match
      case SchemeVal.SList(bindings) :: body if body.nonEmpty =>
        val letEnv = Env(Some(env))
        bindings.foreach {
          case SchemeVal.SList(SchemeVal.SSymbol(n) :: initExpr :: Nil) =>
            letEnv.define(n, Evaluator.eval(initExpr, letEnv))
          case _ => throw new EvalError("let*: bad binding")
        }
        Evaluator.evalBodyCek(body, letEnv, k)
      case _ => throw new EvalError("let*: bad syntax")
