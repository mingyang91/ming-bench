package ming

import SchemeBuiltinSupport.*
import SchemeEvaluatorState.*
import SchemeEvaluatorSupport.*
import SchemeMacros.*
import SchemeModel.*
import SchemeRuntime.*

abstract private[ming] class SchemeEvaluatorSpecialForms:

  protected def eval(expr: Expr, env: Env): Value

  protected def evalQuote(args: List[Expr]): StepResult =
    args match
      case expr :: Nil =>
        StepResult.Final(quoteExpr(expr))
      case _ =>
        throw new EvalError(s"quote expected 1 argument, got ${args.length}")

  protected def evalIf(args: List[Expr], env: Env): StepResult =
    args match
      case condition :: whenTrue :: Nil =>
        if isTruthy(eval(condition, env)) then continueExpr(whenTrue, env)
        else StepResult.Final(Value.VoidValue)
      case condition :: whenTrue :: whenFalse :: Nil =>
        if isTruthy(eval(condition, env)) then continueExpr(whenTrue, env)
        else continueExpr(whenFalse, env)
      case _ =>
        throw new EvalError(s"if expected 2 or 3 arguments, got ${args.length}")

  protected def evalCase(args: List[Expr], env: Env): StepResult =
    args match
      case keyExpr :: clauses if clauses.nonEmpty =>
        val key = eval(keyExpr, env)

        @annotation.tailrec
        def loop(remaining: List[Expr]): StepResult =
          remaining match
            case Nil =>
              StepResult.Final(Value.VoidValue)
            case Expr.ListExpr(Nil, _) :: _ =>
              throw new EvalError("case clauses must be non-empty lists")
            case Expr.ListExpr(Expr.Symbol("else", _) :: expressions, _) :: tail =>
              if tail.nonEmpty then throw new EvalError("case else clause must be last")
              continueSequence(expressions, env)
            case Expr.ListExpr(Expr.ListExpr(datums, _) :: expressions, _) :: tail =>
              if datums.exists(datum => eqvValues(key, quoteExpr(datum))) then continueSequence(expressions, env)
              else loop(tail)
            case _ =>
              throw new EvalError("case clauses must begin with a datum list or else")

        loop(clauses)
      case _ =>
        throw new EvalError("invalid case form")

  protected def evalDefine(args: List[Expr], env: Env): StepResult =
    args match
      case Expr.Symbol(name, _) :: valueExpr :: Nil =>
        env.define(name, eval(valueExpr, env))
        StepResult.Final(Value.VoidValue)
      case Expr.ListExpr(Expr.Symbol(name, _) :: params, _) :: body if body.nonEmpty =>
        env.define(name, buildClosure(params, body, env, Some(name)))
        StepResult.Final(Value.VoidValue)
      case _ =>
        throw new EvalError("invalid define form")

  protected def evalDefineSyntax(args: List[Expr], env: Env): StepResult =
    args match
      case Expr.Symbol(name, _) :: transformer :: Nil =>
        env.defineSyntax(name, parseSyntaxRules(name, transformer, env))
        StepResult.Final(Value.VoidValue)
      case _ =>
        throw new EvalError("invalid define-syntax form")

  protected def evalLambda(args: List[Expr], env: Env): StepResult =
    args match
      case Expr.ListExpr(params, _) :: body if body.nonEmpty =>
        StepResult.Final(buildClosure(params, body, env, None))
      case _ =>
        throw new EvalError("invalid lambda form")

  protected def evalCaseLambda(args: List[Expr], env: Env): StepResult =
    StepResult.Final(buildCaseClosure(args, env))

  protected def evalSet(args: List[Expr], env: Env): StepResult =
    args match
      case Expr.Symbol(name, _) :: valueExpr :: Nil =>
        env.set(name, eval(valueExpr, env))
        StepResult.Final(Value.VoidValue)
      case _ =>
        throw new EvalError("invalid set! form")

  protected def evalCond(clauses: List[Expr], env: Env): StepResult =
    @annotation.tailrec
    def loop(remaining: List[Expr]): StepResult =
      remaining match
        case Nil =>
          StepResult.Final(Value.VoidValue)
        case Expr.ListExpr(Nil, _) :: _ =>
          throw new EvalError("cond clauses must be non-empty lists")
        case Expr.ListExpr(Expr.Symbol("else", _) :: expressions, _) :: tail =>
          if tail.nonEmpty then throw new EvalError("cond else clause must be last")
          continueSequence(expressions, env)
        case Expr.ListExpr(testExpr :: expressions, _) :: tail =>
          val testValue = eval(testExpr, env)
          if isTruthy(testValue) then
            if expressions.isEmpty then StepResult.Final(testValue)
            else continueSequence(expressions, env)
          else loop(tail)
        case _ =>
          throw new EvalError("cond clauses must be non-empty lists")

    loop(clauses)

  protected def evalLet(args: List[Expr], env: Env, callPos: SourcePos): StepResult =
    args match
      case Expr.Symbol(name, _) :: bindingsExpr :: body if body.nonEmpty =>
        evalNamedLet(name, bindingsExpr, body, env, callPos)
      case bindingsExpr :: body if body.nonEmpty =>
        evalPlainLet(bindingsExpr, body, env)
      case _ =>
        throw new EvalError("invalid let form")

  protected def evalLetRec(args: List[Expr], env: Env, sequential: Boolean): StepResult =
    args match
      case bindingsExpr :: body if body.nonEmpty =>
        val bindings = parseBindings(bindingsExpr)
        val letEnv   = new Env(Some(env))

        bindings.foreach { case (name, _) =>
          letEnv.define(name, Value.UninitializedValue(name))
        }

        initializeLetRecBindings(bindings, letEnv, sequential)
        continueSequence(body, letEnv)
      case _ =>
        val formName = if sequential then "letrec*" else "letrec"
        throw new EvalError(s"invalid $formName form")

  protected def evalDo(args: List[Expr], env: Env): StepResult =
    args match
      case bindingsExpr :: Expr.ListExpr(testExpr :: resultExprs, _) :: body =>
        val bindings = parseDoBindings(bindingsExpr)
        val doEnv    = new Env(Some(env))

        bindDoVariables(bindings, env, doEnv)
        runDoLoop(bindings, testExpr, resultExprs, body, doEnv)
      case _ =>
        throw new EvalError("invalid do form")

  protected def evalAnd(args: List[Expr], env: Env): StepResult =
    @annotation.tailrec
    def loop(remaining: List[Expr]): StepResult =
      remaining match
        case Nil =>
          StepResult.Final(Value.BooleanValue(true))
        case last :: Nil =>
          continueExpr(last, env)
        case head :: tail =>
          val next = eval(head, env)
          if isTruthy(next) then loop(tail)
          else StepResult.Final(next)

    loop(args)

  protected def evalOr(args: List[Expr], env: Env): StepResult =
    @annotation.tailrec
    def loop(remaining: List[Expr]): StepResult =
      remaining match
        case Nil =>
          StepResult.Final(Value.BooleanValue(false))
        case last :: Nil =>
          continueExpr(last, env)
        case head :: tail =>
          val next = eval(head, env)
          if isTruthy(next) then StepResult.Final(next)
          else loop(tail)

    loop(args)

  private def continueExpr(expr: Expr, env: Env): StepResult =
    StepResult.Continue(EvalState.ExprState(expr, env))

  private def continueSequence(expressions: List[Expr], env: Env): StepResult =
    StepResult.Continue(EvalState.SequenceState(expressions, env))

  private def continueCall(
    procedure: Value,
    args: List[Value],
    pos: Option[SourcePos]
  ): StepResult =
    StepResult.Continue(EvalState.CallState(procedure, args, pos))

  private def evalPlainLet(bindingsExpr: Expr, body: List[Expr], env: Env): StepResult =
    val bindings = parseBindings(bindingsExpr)
    val values = bindings.map { case (_, valueExpr) =>
      eval(valueExpr, env)
    }
    val letEnv = new Env(Some(env))

    bindings.zip(values).foreach { case ((name, _), value) =>
      letEnv.define(name, value)
    }

    continueSequence(body, letEnv)

  private def evalNamedLet(
    name: String,
    bindingsExpr: Expr,
    body: List[Expr],
    env: Env,
    callPos: SourcePos
  ): StepResult =
    val bindings = parseBindings(bindingsExpr)
    val params   = bindings.map(_._1)
    val args = bindings.map { case (_, valueExpr) =>
      eval(valueExpr, env)
    }
    val letEnv                 = new Env(Some(env))
    val closure: Value.Closure = Value.Closure(Some(name), params, None, body, letEnv)

    letEnv.define(name, closure)
    continueCall(closure, args, Some(callPos))

  private def initializeLetRecBindings(
    bindings: List[(String, Expr)],
    letEnv: Env,
    sequential: Boolean
  ): Unit =
    if sequential then
      bindings.foreach { case (name, valueExpr) =>
        letEnv.set(name, eval(valueExpr, letEnv))
      }
    else
      val values = bindings.map { case (_, valueExpr) =>
        eval(valueExpr, letEnv)
      }

      bindings.zip(values).foreach { case ((name, _), value) =>
        letEnv.set(name, value)
      }

  private def bindDoVariables(bindings: List[DoBinding], sourceEnv: Env, doEnv: Env): Unit =
    val initialValues = bindings.map(binding => eval(binding.initExpr, sourceEnv))
    bindings.zip(initialValues).foreach { case (binding, value) =>
      doEnv.define(binding.name, value)
    }

  @annotation.tailrec
  private def runDoLoop(
    bindings: List[DoBinding],
    testExpr: Expr,
    resultExprs: List[Expr],
    body: List[Expr],
    doEnv: Env
  ): StepResult =
    if isTruthy(eval(testExpr, doEnv)) then
      if resultExprs.isEmpty then StepResult.Final(Value.VoidValue)
      else continueSequence(resultExprs, doEnv)
    else
      evalSequenceBody(body, doEnv)
      advanceDoBindings(bindings, doEnv)
      runDoLoop(bindings, testExpr, resultExprs, body, doEnv)

  private def advanceDoBindings(bindings: List[DoBinding], doEnv: Env): Unit =
    val nextValues = bindings.map(nextDoBindingValue(_, doEnv))
    bindings.zip(nextValues).foreach { case (binding, value) =>
      doEnv.set(binding.name, value)
    }

  private def nextDoBindingValue(binding: DoBinding, doEnv: Env): Value =
    binding.stepExpr match
      case Some(stepExpr) => eval(stepExpr, doEnv)
      case None           => doEnv.lookup(binding.name)

  private def evalSequenceBody(expressions: List[Expr], env: Env): Unit =
    expressions.foreach(expr => eval(expr, env))
