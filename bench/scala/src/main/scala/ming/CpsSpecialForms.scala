package ming

import SchemeValue.*
import InterpreterUtils.*
import Bounce.*

/** CPS special form handlers extracted from CpsEval. */
object CpsSpecialForms:

  import CpsEval.{applyK, evalBodyK, evalK, evalSequenceK, Cont, Env}

  def evalDefineSyntaxK(
    rest: List[SchemeValue],
    pos: Option[(Int, Int)],
    env: Env,
    out: Array[String],
    k: Cont
  ): Bounce =
    rest match
      case SymbolVal(name, _) ::
          ListVal(
            SymbolVal("syntax-rules", _) :: ListVal(literals, _) :: rules,
            _
          ) :: Nil =>
        val literalNames = literals.map {
          case SymbolVal(n, _) => n
          case _ =>
            throw new EvalError("syntax-rules: literals must be identifiers")
        }
        val parsedRules = rules.map {
          case ListVal(ListVal(pattern, _) :: template :: Nil, _) =>
            (pattern, template)
          case _ => throw new EvalError("syntax-rules: invalid rule")
        }
        val macroVal = MacroVal(literalNames, parsedRules, env)
        More(() => k(Void, env + (name -> makeCell(macroVal))))
      case _ =>
        throw new EvalError(s"define-syntax: bad syntax${fmtPos(pos)}")

  def evalIfK(
    rest: List[SchemeValue],
    pos: Option[(Int, Int)],
    env: Env,
    out: Array[String],
    k: Cont
  ): Bounce =
    rest match
      case cond :: thenBr :: elseBr :: Nil =>
        evalK(
          cond,
          env,
          out,
          (cv, _) =>
            if cv.isTruthy then evalK(thenBr, env, out, k)
            else evalK(elseBr, env, out, k)
        )
      case cond :: thenBr :: Nil =>
        evalK(
          cond,
          env,
          out,
          (cv, _) =>
            if cv.isTruthy then evalK(thenBr, env, out, k)
            else More(() => k(Void, env))
        )
      case _ => throw new EvalError(s"if: bad syntax${fmtPos(pos)}")

  def evalSetBangK(
    name: String,
    namePos: Option[(Int, Int)],
    valueExpr: SchemeValue,
    env: Env,
    out: Array[String],
    k: Cont
  ): Bounce =
    val cell = env.getOrElse(
      name,
      throw new EvalError(s"set!: unbound variable: $name${fmtPos(namePos)}")
    )
    evalK(
      valueExpr,
      env,
      out,
      (v, _) =>
        cell match
          case Cell(arr) =>
            arr(0) = v
            More(() => k(Void, env))
          case _ => throw new EvalError(s"set!: invalid binding for $name")
    )

  def evalDefineK(
    rest: List[SchemeValue],
    pos: Option[(Int, Int)],
    env: Env,
    out: Array[String],
    k: Cont
  ): Bounce =
    rest match
      case SymbolVal(name, _) :: value :: Nil =>
        evalK(
          value,
          env,
          out,
          (v, _) =>
            val bound = v match
              case LambdaVal(params, body, closure, _, restParam) =>
                LambdaVal(params, body, closure, Some(name), restParam)
              case other => other
            More(() => k(Void, env + (name -> makeCell(bound))))
        )
      case ListVal(SymbolVal(name, _) :: params, _) :: body =>
        val (ps, rp) = extractParamsWithRest(params)
        val lambda   = LambdaVal(ps, body, env, Some(name), rp)
        More(() => k(Void, env + (name -> makeCell(lambda))))
      case _ => throw new EvalError(s"define: bad syntax${fmtPos(pos)}")

  def evalAndK(
    args: List[SchemeValue],
    env: Env,
    out: Array[String],
    k: Cont
  ): Bounce =
    args match
      case Nil         => More(() => k(BoolVal(true), env))
      case last :: Nil => evalK(last, env, out, k)
      case head :: tail =>
        evalK(
          head,
          env,
          out,
          (result, _) =>
            if result.isTruthy then evalAndK(tail, env, out, k)
            else More(() => k(result, env))
        )

  def evalOrK(
    args: List[SchemeValue],
    env: Env,
    out: Array[String],
    k: Cont
  ): Bounce =
    args match
      case Nil         => More(() => k(BoolVal(false), env))
      case last :: Nil => evalK(last, env, out, k)
      case head :: tail =>
        evalK(
          head,
          env,
          out,
          (result, _) =>
            if result.isTruthy then More(() => k(result, env))
            else evalOrK(tail, env, out, k)
        )

  def evalCondK(
    clauses: List[SchemeValue],
    env: Env,
    out: Array[String],
    k: Cont
  ): Bounce =
    clauses match
      case Nil => More(() => k(Void, env))
      case ListVal(SymbolVal("else", _) :: body, _) :: _ =>
        evalBodyK(body, env, out, k)
      case ListVal(test :: body, _) :: rest =>
        evalK(
          test,
          env,
          out,
          (tv, _) =>
            if tv.isTruthy then evalBodyK(body, env, out, k)
            else evalCondK(rest, env, out, k)
        )
      case _ => throw new EvalError("invalid cond clause")

  def evalLetK(
    rest: List[SchemeValue],
    env: Env,
    out: Array[String],
    k: Cont
  ): Bounce =
    rest match
      case SymbolVal(name, _) :: ListVal(bindings, _) :: body =>
        evalBindingsK(
          bindings,
          env,
          out,
          Nil,
          initVals =>
            val paramNames = bindings.map {
              case ListVal(SymbolVal(p, _) :: _ :: Nil, _) => p
              case _                                       => throw new EvalError("invalid let binding")
            }
            val lambda = LambdaVal(paramNames, body, env, Some(name))
            applyK(lambda, initVals, env, out, k, None)
        )
      case ListVal(bindings, _) :: body =>
        evalLetBindingsK(
          bindings,
          env,
          env,
          out,
          letEnv => evalBodyK(body, letEnv, out, k)
        )
      case _ => throw new EvalError("invalid let syntax")

  private def evalBindingsK(
    bindings: List[SchemeValue],
    env: Env,
    out: Array[String],
    acc: List[SchemeValue],
    k: List[SchemeValue] => Bounce
  ): Bounce =
    bindings match
      case Nil => More(() => k(acc))
      case ListVal(SymbolVal(_, _) :: valExpr :: Nil, _) :: rest =>
        evalK(
          valExpr,
          env,
          out,
          (v, _) => evalBindingsK(rest, env, out, acc :+ v, k)
        )
      case _ => throw new EvalError("invalid let binding")

  private def evalLetBindingsK(
    bindings: List[SchemeValue],
    origEnv: Env,
    letEnv: Env,
    out: Array[String],
    k: Env => Bounce
  ): Bounce =
    bindings match
      case Nil => More(() => k(letEnv))
      case ListVal(SymbolVal(name, _) :: valExpr :: Nil, _) :: rest =>
        evalK(
          valExpr,
          origEnv,
          out,
          (v, _) =>
            evalLetBindingsK(
              rest,
              origEnv,
              letEnv + (name -> makeCell(v)),
              out,
              k
            )
        )
      case _ => throw new EvalError("invalid let binding")
