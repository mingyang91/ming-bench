package ming

import SchemeEvaluatorState.*
import SchemeEvaluatorSupport.*
import SchemeModel.*
import SchemeRuntime.*

private[ming] trait SchemeEvaluatorBindingForms extends SchemeEvaluatorSpecialForms:

  final protected def evalLet(
    args: List[Expr],
    env: Env,
    continuation: Continuation,
    callPos: SourcePos
  ): Computation =
    args match
      case Expr.Symbol(name, _) :: bindingsExpr :: body if body.nonEmpty =>
        evalNamedLet(name, bindingsExpr, body, env, continuation, callPos)
      case bindingsExpr :: body if body.nonEmpty =>
        evalPlainLet(bindingsExpr, body, env, continuation, callPos)
      case _ =>
        throw new EvalError("invalid let form")

  final protected def evalLetStar(
    args: List[Expr],
    env: Env,
    continuation: Continuation,
    pos: SourcePos
  ): Computation =
    args match
      case bindingsExpr :: body if body.nonEmpty =>
        val bindings = parseLetStarBindings(bindingsExpr)
        val letEnv   = new Env(Some(env))

        def loop(remaining: List[(String, Expr)]): Computation =
          remaining match
            case Nil =>
              evalSequence(body, letEnv, continuation)
            case (name, valueExpr) :: tail =>
              eval(
                valueExpr,
                letEnv,
                contextualCont(pos) { value =>
                  letEnv.define(name, value)
                  loop(tail)
                }
              )

        loop(bindings)
      case _ =>
        throw new EvalError("invalid let* form")

  final protected def evalLetRec(
    args: List[Expr],
    env: Env,
    continuation: Continuation,
    sequential: Boolean,
    pos: SourcePos
  ): Computation =
    args match
      case bindingsExpr :: body if body.nonEmpty =>
        val bindings = parseBindings(bindingsExpr)
        val letEnv   = new Env(Some(env))

        bindings.foreach { case (name, _) =>
          letEnv.define(name, Value.UninitializedValue(name))
        }

        if sequential then initializeLetRecSequential(bindings, body, letEnv, continuation, pos)
        else initializeLetRecParallel(bindings, body, letEnv, continuation, pos)
      case _ =>
        val formName = if sequential then "letrec*" else "letrec"
        throw new EvalError(s"invalid $formName form")

  final protected def evalDo(
    args: List[Expr],
    env: Env,
    continuation: Continuation,
    pos: SourcePos
  ): Computation =
    args match
      case bindingsExpr :: Expr.ListExpr(testExpr :: resultExprs, _) :: body =>
        val bindings = parseDoBindings(bindingsExpr)
        val doEnv    = new Env(Some(env))

        evalExprList(bindings.map(_.initExpr), env) { initialValues =>
          contextual(pos) {
            bindings.zip(initialValues).foreach { case (binding, value) =>
              doEnv.define(binding.name, value)
            }
            runDoLoop(bindings, testExpr, resultExprs, body, doEnv, continuation, pos)
          }
        }
      case _ =>
        throw new EvalError("invalid do form")

  private def evalPlainLet(
    bindingsExpr: Expr,
    body: List[Expr],
    env: Env,
    continuation: Continuation,
    pos: SourcePos
  ): Computation =
    val bindings = parseBindings(bindingsExpr)
    evalExprList(bindings.map(_._2), env) { values =>
      contextual(pos) {
        val letEnv = new Env(Some(env))
        bindings.zip(values).foreach { case ((name, _), value) =>
          letEnv.define(name, value)
        }
        evalSequence(body, letEnv, continuation)
      }
    }

  private def evalNamedLet(
    name: String,
    bindingsExpr: Expr,
    body: List[Expr],
    env: Env,
    continuation: Continuation,
    callPos: SourcePos
  ): Computation =
    val bindings = parseBindings(bindingsExpr)
    val params   = bindings.map(_._1)

    evalExprList(bindings.map(_._2), env) { args =>
      contextual(callPos) {
        val letEnv                 = new Env(Some(env))
        val closure: Value.Closure = Value.Closure(Some(name), params, None, body, letEnv)

        letEnv.define(name, closure)
        applyProcedure(closure, args, continuation, Some(callPos))
      }
    }

  private def initializeLetRecSequential(
    bindings: List[(String, Expr)],
    body: List[Expr],
    letEnv: Env,
    continuation: Continuation,
    pos: SourcePos
  ): Computation =
    def loop(remaining: List[(String, Expr)]): Computation =
      remaining match
        case Nil =>
          evalSequence(body, letEnv, continuation)
        case (name, valueExpr) :: tail =>
          eval(
            valueExpr,
            letEnv,
            contextualCont(pos) { value =>
              letEnv.set(name, value)
              loop(tail)
            }
          )

    loop(bindings)

  private def initializeLetRecParallel(
    bindings: List[(String, Expr)],
    body: List[Expr],
    letEnv: Env,
    continuation: Continuation,
    pos: SourcePos
  ): Computation =
    evalExprList(bindings.map(_._2), letEnv) { values =>
      contextual(pos) {
        bindings.zip(values).foreach { case ((name, _), value) =>
          letEnv.set(name, value)
        }
        evalSequence(body, letEnv, continuation)
      }
    }

  private def runDoLoop(
    bindings: List[DoBinding],
    testExpr: Expr,
    resultExprs: List[Expr],
    body: List[Expr],
    doEnv: Env,
    continuation: Continuation,
    pos: SourcePos
  ): Computation =
    eval(
      testExpr,
      doEnv,
      contextualCont(pos) { testValue =>
        if isTruthy(testValue) then
          if resultExprs.isEmpty then resume(continuation, Value.VoidValue)
          else evalSequence(resultExprs, doEnv, continuation)
        else
          evalSequence(
            body,
            doEnv,
            contextualCont(pos) { _ =>
              evalDoStepValues(bindings, doEnv, pos) { nextValues =>
                contextual(pos) {
                  bindings.zip(nextValues).foreach { case (binding, value) =>
                    doEnv.set(binding.name, value)
                  }
                  runDoLoop(bindings, testExpr, resultExprs, body, doEnv, continuation, pos)
                }
              }
            }
          )
      }
    )

  private def evalDoStepValues(
    bindings: List[DoBinding],
    doEnv: Env,
    pos: SourcePos
  )(continuation: List[Value] => Computation): Computation =
    def loop(remaining: List[DoBinding], reversedValues: List[Value]): Computation =
      remaining match
        case Nil =>
          suspend(continuation(reversedValues.reverse))
        case binding :: tail =>
          binding.stepExpr match
            case Some(stepExpr) =>
              eval(stepExpr, doEnv, value => suspend(loop(tail, value :: reversedValues)))
            case None =>
              contextual(pos) {
                loop(tail, doEnv.lookup(binding.name) :: reversedValues)
              }

    loop(bindings, Nil)
