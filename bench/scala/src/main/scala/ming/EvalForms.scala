package ming

import Value.*
import Expr.*
import EvalHelpers.{evalError, parseParams}

/** CPS form evaluators (define, let, cond, and, or) mixed into Evaluator. */
private[ming] trait EvalForms extends EvalDo:

  protected def evalDefineCps(
    args: List[Expr],
    env: Env,
    pos: Option[Pos],
    k: Value => Bounce
  ): Bounce =
    args match
      case Sym(name, _) :: valueExpr :: Nil =>
        eval(
          valueExpr,
          env,
          { v =>
            env.define(name, v); k(BoolVal(true))
          }
        )
      case SList(Sym(name, _) :: params, _) :: body if body.nonEmpty =>
        val (paramNames, restParam) = parseParams(params, "define", pos)
        env.define(name, LambdaVal(paramNames, restParam, body, env))
        k(BoolVal(true))
      case _ => evalError("define: bad syntax", pos)

  protected def evalLetCps(
    args: List[Expr],
    env: Env,
    pos: Option[Pos],
    k: Value => Bounce
  ): Bounce =
    args match
      case Sym(name, _) :: SList(bindings, _) :: body if body.nonEmpty =>
        evalBindingsCps(
          bindings,
          env,
          pos,
          { pairs =>
            val paramNames = pairs.map(_._1)
            val initVals   = pairs.map(_._2)
            val localEnv   = env.extend(Nil, Nil)
            localEnv.define(name, LambdaVal(paramNames, None, body, localEnv))
            val callEnv = localEnv.extend(paramNames, initVals)
            tailBody(body, callEnv, k)
          }
        )
      case SList(bindings, _) :: body if body.nonEmpty =>
        evalBindingsCps(
          bindings,
          env,
          pos,
          { pairs =>
            val localEnv = env.extend(pairs.map(_._1), pairs.map(_._2))
            tailBody(body, localEnv, k)
          }
        )
      case _ => evalError("let: bad syntax", pos)

  protected def evalBindingsCps(
    bindings: List[Expr],
    env: Env,
    pos: Option[Pos],
    k: List[(String, Value)] => Bounce
  ): Bounce =
    bindings match
      case Nil => k(Nil)
      case SList(Sym(name, _) :: valExpr :: Nil, _) :: rest =>
        eval(valExpr, env, v => trampoline(evalBindingsCps(rest, env, pos, pairs => k((name, v) :: pairs))))
      case _ => evalError("let: bad binding", pos)

  protected def evalCondCps(
    clauses: List[Expr],
    env: Env,
    pos: Option[Pos],
    k: Value => Bounce
  ): Bounce =
    clauses match
      case Nil => evalError("cond: no matching clause", pos)
      case SList(Sym("else", _) :: body, _) :: Nil =>
        evalBody(body, env, k)
      case SList(test :: body, _) :: rest =>
        eval(
          test,
          env,
          tv =>
            if tv.isTruthy then
              if body.isEmpty then k(tv) // (cond (test)) — return test value
              else evalBody(body, env, k)
            else evalCondCps(rest, env, pos, k)
        )
      case _ => evalError("cond: bad syntax", pos)

  protected def evalLetStarCps(
    args: List[Expr],
    env: Env,
    pos: Option[Pos],
    k: Value => Bounce
  ): Bounce =
    args match
      case SList(bindings, _) :: body if body.nonEmpty =>
        val localEnv = env.extend(Nil, Nil)
        evalLetStarBindings(bindings, localEnv, pos, () => tailBody(body, localEnv, k))
      case _ => evalError("let*: bad syntax", pos)

  private def evalLetStarBindings(
    bindings: List[Expr],
    env: Env,
    pos: Option[Pos],
    k: () => Bounce
  ): Bounce =
    bindings match
      case Nil => k()
      case SList(Sym(name, _) :: valExpr :: Nil, _) :: rest =>
        eval(
          valExpr,
          env,
          { v =>
            env.define(name, v)
            trampoline(evalLetStarBindings(rest, env, pos, k))
          }
        )
      case _ => evalError("let*: bad binding", pos)

  protected def evalLetrecCps(
    args: List[Expr],
    env: Env,
    pos: Option[Pos],
    k: Value => Bounce
  ): Bounce =
    args match
      case SList(bindings, _) :: body if body.nonEmpty =>
        val localEnv = env.extend(Nil, Nil)
        val names = bindings.map {
          case SList(Sym(name, _) :: _ :: Nil, _) => name
          case _                                  => evalError("letrec: bad binding", pos)
        }
        names.foreach(n => localEnv.define(n, VoidVal))
        val valExprs = bindings.map {
          case SList(_ :: valExpr :: Nil, _) => valExpr
          case _                             => evalError("letrec: bad binding", pos)
        }
        evalLetrecBindings(names, valExprs, localEnv, pos, () => tailBody(body, localEnv, k))
      case _ => evalError("letrec: bad syntax", pos)

  private def evalLetrecBindings(
    names: List[String],
    valExprs: List[Expr],
    env: Env,
    pos: Option[Pos],
    k: () => Bounce
  ): Bounce =
    (names, valExprs) match
      case (Nil, Nil) => k()
      case (name :: restN, valExpr :: restV) =>
        eval(
          valExpr,
          env,
          { v =>
            env.define(name, v)
            trampoline(evalLetrecBindings(restN, restV, env, pos, k))
          }
        )
      case _ => evalError("letrec: internal error", pos)

  protected def evalLetrecStarCps(
    args: List[Expr],
    env: Env,
    pos: Option[Pos],
    k: Value => Bounce
  ): Bounce =
    args match
      case SList(bindings, _) :: body if body.nonEmpty =>
        val localEnv = env.extend(Nil, Nil)
        val names = bindings.map {
          case SList(Sym(name, _) :: _ :: Nil, _) => name
          case _                                  => evalError("letrec*: bad binding", pos)
        }
        names.foreach(n => localEnv.define(n, VoidVal))
        evalLetStarBindings(bindings, localEnv, pos, () => tailBody(body, localEnv, k))
      case _ => evalError("letrec*: bad syntax", pos)

  protected def evalDefineRecordType(
    args: List[Expr],
    env: Env,
    pos: Option[Pos],
    k: Value => Bounce
  ): Bounce =
    args match
      case Sym(typeName, _) :: SList(Sym(ctorName, _) :: ctorFields, _) :: Sym(predName, _) :: fieldDefs =>
        val fieldNames = ctorFields.map {
          case Sym(n, _) => n
          case _         => evalError("define-record-type: bad constructor field", pos)
        }
        val tag = typeName
        // Define constructor
        env.define(
          ctorName,
          BuiltinVal(
            ctorName,
            vals =>
              if vals.length != fieldNames.length then
                throw new EvalError(s"$ctorName: expected ${fieldNames.length} arguments, got ${vals.length}")
              RecordVal(tag, vals.toArray)
          )
        )
        // Define predicate
        env.define(
          predName,
          BuiltinVal(
            predName,
            {
              case List(RecordVal(t, _)) => BoolVal(t == tag)
              case List(_)               => BoolVal(false)
              case vals                  => throw new EvalError(s"$predName: expected 1 argument, got ${vals.length}")
            }
          )
        )
        // Define field accessors
        fieldDefs.foreach {
          case SList(Sym(fieldName, _) :: Sym(accessorName, _) :: Nil, _) =>
            val idx = fieldNames.indexOf(fieldName)
            if idx < 0 then evalError(s"define-record-type: unknown field $fieldName", pos)
            env.define(
              accessorName,
              BuiltinVal(
                accessorName,
                {
                  case List(RecordVal(t, fields)) if t == tag => fields(idx)
                  case List(_)                                => throw new EvalError(s"$accessorName: not a $typeName")
                  case vals => throw new EvalError(s"$accessorName: expected 1 argument, got ${vals.length}")
                }
              )
            )
          case _ => evalError("define-record-type: bad field spec", pos)
        }
        k(VoidVal)
      case _ => evalError("define-record-type: bad syntax", pos)

  protected def evalAndCps(args: List[Expr], env: Env, k: Value => Bounce): Bounce =
    args match
      case Nil         => k(BoolVal(true))
      case last :: Nil => eval(last, env, k)
      case head :: tail =>
        eval(head, env, v => if !v.isTruthy then k(v) else trampoline(evalAndCps(tail, env, k)))

  protected def evalOrCps(args: List[Expr], env: Env, k: Value => Bounce): Bounce =
    args match
      case Nil         => k(BoolVal(false))
      case last :: Nil => eval(last, env, k)
      case head :: tail =>
        eval(head, env, v => if v.isTruthy then k(v) else trampoline(evalOrCps(tail, env, k)))
