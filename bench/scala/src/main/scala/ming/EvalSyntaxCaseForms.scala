package ming

import Value.*
import EvalHelpers.evalError

/** Syntax-case, with-syntax, and call/cc — mixed into Evaluator. */
private[ming] trait EvalSyntaxCaseForms:
  import Evaluator.{K, WindEntry}

  protected def eval(expr: Expr, env: Env, k: K): Bounce
  protected def evalBody(exprs: List[Expr], env: Env, k: K): Bounce
  protected def tailBody(exprs: List[Expr], env: Env, k: K): Bounce
  protected def applyProc(proc: Value, values: List[Value], pos: Option[Pos], k: K): Bounce
  protected def trampoline(thunk: => Bounce): Bounce
  protected def doWindTransition(target: List[WindEntry], pos: Option[Pos], andThen: () => Bounce): Bounce
  protected def windStack: List[WindEntry]
  protected def currentSyntaxBindings: Map[String, SyntaxCase.Binding]
  protected def currentSyntaxBindings_=(v: Map[String, SyntaxCase.Binding]): Unit
  protected def currentSyntaxEnvs: (Env, Env)
  protected def currentSyntaxEnvs_=(v: (Env, Env)): Unit
  protected def pendingReturns: java.util.IdentityHashMap[Expr, Value]
  protected def bodyRemaining: List[Expr]
  protected def bodyEnvRef: Env
  protected def bodyK: K

  protected def evalCallCc(callccExpr: Expr, procExpr: Expr, env: Env, pos: Option[Pos], k: K): Bounce =
    val pending = pendingReturns.remove(callccExpr)
    if pending != null then k(pending)
    else
      val capturedRemaining = bodyRemaining
      val capturedEnv       = bodyEnvRef
      val capturedK         = bodyK
      val capturedWind      = windStack
      val cpsK              = k
      var active            = true
      eval(
        procExpr,
        env,
        { proc =>
          val contVal = ContinuationVal { v =>
            if active then
              active = false
              doWindTransition(capturedWind, pos, () => cpsK(v))
            else
              doWindTransition(
                capturedWind,
                pos,
                () =>
                  pendingReturns.put(callccExpr, v)
                  tailBody(capturedRemaining, capturedEnv, capturedK)
              )
          }
          applyProc(
            proc,
            List(contVal),
            pos,
            { procResult =>
              active = false
              k(procResult)
            }
          )
        }
      )

  protected def evalSyntaxCaseClauses(
    scrutinee: Value,
    literalExprs: List[Expr],
    clauses: List[Expr],
    env: Env,
    pos: Option[Pos],
    k: K
  ): Bounce =
    val literals = literalExprs.map {
      case Expr.Sym(name, _) => name
      case _                 => evalError("syntax-case: literal must be identifier", pos)
    }.toSet
    clauses match
      case Nil => evalError("syntax-case: no matching pattern", pos)
      case Expr.SList(pattern :: fenderAndBody, _) :: rest =>
        SyntaxCase.matchPattern(pattern, scrutinee, literals) match
          case Some(bindings) =>
            evalSyntaxCaseMatch(bindings, fenderAndBody, scrutinee, literalExprs, rest, env, pos, k)
          case None =>
            evalSyntaxCaseClauses(scrutinee, literalExprs, rest, env, pos, k)
      case _ => evalError("syntax-case: bad clause", pos)

  private def evalSyntaxCaseMatch(
    bindings: Map[String, SyntaxCase.Binding],
    fenderAndBody: List[Expr],
    scrutinee: Value,
    literalExprs: List[Expr],
    rest: List[Expr],
    env: Env,
    pos: Option[Pos],
    k: K
  ): Bounce =
    val savedBindings = currentSyntaxBindings
    val savedEnvs     = currentSyntaxEnvs
    currentSyntaxBindings = savedBindings ++ bindings
    currentSyntaxEnvs = (env, env)
    val localEnv = env.extend(Nil, Nil)
    bindings.foreach {
      case (name, SyntaxCase.Binding.Single(v))    => localEnv.define(name, v)
      case (name, SyntaxCase.Binding.Ellipsis(vs)) => localEnv.define(name, listToValue(vs))
    }
    def restore(result: Value): Bounce =
      currentSyntaxBindings = savedBindings
      currentSyntaxEnvs = savedEnvs
      k(result)
    fenderAndBody match
      case body :: Nil =>
        eval(body, localEnv, restore)
      case fender :: body :: Nil =>
        eval(
          fender,
          localEnv,
          fv =>
            if fv.isTruthy then eval(body, localEnv, restore)
            else
              currentSyntaxBindings = savedBindings
              currentSyntaxEnvs = savedEnvs
              evalSyntaxCaseClauses(scrutinee, literalExprs, rest, env, pos, k)
        )
      case _ => evalError("syntax-case: bad clause", pos)

  protected def evalWithSyntax(
    bindingExprs: List[Expr],
    body: List[Expr],
    env: Env,
    pos: Option[Pos],
    k: K
  ): Bounce =
    bindingExprs match
      case Nil =>
        evalBody(body, env, k)
      case Expr.SList(pattern :: valExpr :: Nil, _) :: rest =>
        eval(
          valExpr,
          env,
          { v =>
            val literals = Set.empty[String]
            SyntaxCase.matchPattern(pattern, v, literals) match
              case Some(newBindings) =>
                val savedBindings = currentSyntaxBindings
                currentSyntaxBindings = currentSyntaxBindings ++ newBindings
                val localEnv = env.extend(Nil, Nil)
                newBindings.foreach {
                  case (name, SyntaxCase.Binding.Single(sv))   => localEnv.define(name, sv)
                  case (name, SyntaxCase.Binding.Ellipsis(vs)) => localEnv.define(name, listToValue(vs))
                }
                trampoline(
                  evalWithSyntax(
                    rest,
                    body,
                    localEnv,
                    pos,
                    { result =>
                      currentSyntaxBindings = savedBindings
                      k(result)
                    }
                  )
                )
              case None => evalError("with-syntax: pattern match failed", pos)
          }
        )
      case _ => evalError("with-syntax: bad syntax", pos)

  private def listToValue(vs: List[Value]): Value =
    vs.foldRight(Value.NilVal: Value)((v, acc) => Pair(v, acc))
