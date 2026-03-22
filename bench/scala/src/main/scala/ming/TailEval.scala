package ming

import SchemeValue.*
import InterpreterUtils.*
import Interpreter.{Env, Output}
import TcoResult.*

/** Tail-position evaluation methods for TCO support. */
private[ming] object TailEval:

  def evalTail(expr: SchemeValue, env: Env): TcoResult =
    expr match
      // Special forms that are NOT tail-call-relevant but must be recognized
      case ListVal(SymbolVal("quote", _) :: arg :: Nil, _) =>
        Value(arg, "")
      case ListVal(SymbolVal("define", _) :: rest, pos) =>
        val (v, _, o) = Interpreter.evalDefine(rest, pos, env)
        Value(v, o)
      case ListVal(SymbolVal("lambda", _) :: ListVal(params, _) :: body, _) =>
        Value(LambdaVal(extractParams(params), body, env), "")

      // Tail-position forms
      case ListVal(SymbolVal("if", _) :: rest, pos) =>
        evalIfTail(rest, pos, env)
      case ListVal(SymbolVal("cond", _) :: clauses, _) =>
        evalCondTail(clauses, env)
      case ListVal(SymbolVal("and", _) :: args, _) =>
        evalAndTail(args, env)
      case ListVal(SymbolVal("or", _) :: args, _) =>
        evalOrTail(args, env)
      case ListVal(SymbolVal("begin", _) :: body, _) =>
        evalSequenceTail(body, env)
      case ListVal(SymbolVal("let", _) :: rest, _) =>
        evalLetTail(rest, env)
      case ListVal(head :: args, pos) =>
        evalApplicationTail(head, args, pos, env)
      case _ =>
        val (v, _, o) = Interpreter.eval(expr, env)
        Value(v, o)

  private def evalIfTail(
    rest: List[SchemeValue],
    pos: Option[(Int, Int)],
    env: Env
  ): TcoResult =
    rest match
      case cond :: thenBr :: elseBr :: Nil =>
        val (cv, _, o1) = Interpreter.eval(cond, env)
        val branch      = if cv.isTruthy then thenBr else elseBr
        prependOutput(evalTail(branch, env), o1)
      case cond :: thenBr :: Nil =>
        val (cv, _, o1) = Interpreter.eval(cond, env)
        if cv.isTruthy then prependOutput(evalTail(thenBr, env), o1)
        else Value(Void, o1)
      case _ => throw new EvalError(s"if: bad syntax${fmtPos(pos)}")

  def evalCondTail(
    clauses: List[SchemeValue],
    env: Env
  ): TcoResult =
    clauses match
      case Nil => Value(Void, "")
      case ListVal(SymbolVal("else", _) :: body, _) :: _ =>
        evalBodyTail(body, env)
      case ListVal(test :: body, _) :: rest =>
        val (tv, _, o1) = Interpreter.eval(test, env)
        if tv.isTruthy then prependOutput(evalBodyTail(body, env), o1)
        else prependOutput(evalCondTail(rest, env), o1)
      case _ => throw new EvalError("invalid cond clause")

  def evalAndTail(
    args: List[SchemeValue],
    env: Env
  ): TcoResult =
    args match
      case Nil         => Value(BoolVal(true), "")
      case last :: Nil => evalTail(last, env)
      case head :: tail =>
        val (result, _, o1) = Interpreter.eval(head, env)
        if result.isTruthy then prependOutput(evalAndTail(tail, env), o1)
        else Value(result, o1)

  def evalOrTail(
    args: List[SchemeValue],
    env: Env
  ): TcoResult =
    args match
      case Nil         => Value(BoolVal(false), "")
      case last :: Nil => evalTail(last, env)
      case head :: tail =>
        val (result, _, o1) = Interpreter.eval(head, env)
        if result.isTruthy then Value(result, o1)
        else prependOutput(evalOrTail(tail, env), o1)

  private def evalSequenceTail(
    exprs: List[SchemeValue],
    env: Env
  ): TcoResult =
    exprs match
      case Nil         => Value(Void, "")
      case last :: Nil => evalTail(last, env)
      case head :: tail =>
        val (_, newEnv, o) = Interpreter.eval(head, env)
        prependOutput(evalSequenceTail(tail, newEnv), o)

  def evalLetTail(
    rest: List[SchemeValue],
    env: Env
  ): TcoResult =
    rest match
      // named let
      case SymbolVal(name, _) :: ListVal(bindings, _) :: body =>
        val (paramNames, initVals, bindOutput) =
          bindings.foldLeft((List.empty[String], List.empty[SchemeValue], "")) { case ((ps, vs, o), binding) =>
            binding match
              case ListVal(SymbolVal(p, _) :: valExpr :: Nil, _) =>
                val (v, _, vo) = Interpreter.eval(valExpr, env)
                (ps :+ p, vs :+ v, o + vo)
              case _ => throw new EvalError("invalid let binding")
          }
        val lambda = LambdaVal(paramNames, body, env, Some(name))
        prependOutput(Interpreter.applyStep(lambda, initVals, env), bindOutput)
      // regular let
      case ListVal(bindings, _) :: body =>
        val (letEnv, bindOutput) =
          bindings.foldLeft((env, "")) { case ((e, o), binding) =>
            binding match
              case ListVal(SymbolVal(name, _) :: valExpr :: Nil, _) =>
                val (v, _, vo) = Interpreter.eval(valExpr, env)
                (e + (name -> v), o + vo)
              case _ => throw new EvalError("invalid let binding")
          }
        prependOutput(evalBodyTail(body, letEnv), bindOutput)
      case _ => throw new EvalError("invalid let syntax")

  private def evalApplicationTail(
    head: SchemeValue,
    args: List[SchemeValue],
    pos: Option[(Int, Int)],
    env: Env
  ): TcoResult =
    val (func, _, o1)    = Interpreter.eval(head, env)
    val (evaledArgs, o2) = Interpreter.evalArgs(args, env)
    try
      func match
        case lam @ LambdaVal(params, _, _, _) =>
          if params.length != evaledArgs.length then
            throw new EvalError(
              s"wrong number of arguments: expected ${params.length}, got ${evaledArgs.length}"
            )
          TailCall(lam, evaledArgs, env, o1 + o2)
        case SymbolVal(name, _) =>
          val (rv, o3) = Builtins.applyBuiltin(name, evaledArgs)
          Value(rv, o1 + o2 + o3)
        case _ => throw new EvalError(s"not a procedure${fmtPos(pos)}")
    catch
      case e: EvalError if !hasPos(e.getMessage) =>
        throw new EvalError(s"${e.getMessage}${fmtPos(pos)}")

  // ---------------------------------------------------------------------------
  // Body evaluation (tail-aware)
  // ---------------------------------------------------------------------------

  def evalBodyTail(
    body: List[SchemeValue],
    env: Env
  ): TcoResult =
    val (defines, rest) = body.span(isDefine)
    val (bodyEnv, defOutput) =
      if defines.isEmpty then (env, "")
      else Interpreter.processDefines(defines, env)
    rest match
      case Nil => Value(Void, defOutput)
      case last :: Nil =>
        prependOutput(evalTail(last, bodyEnv), defOutput)
      case head :: tail =>
        val (_, newEnv, o) = Interpreter.eval(head, bodyEnv)
        prependOutput(evalBodySeqTail(tail, newEnv), defOutput + o)

  private def evalBodySeqTail(
    body: List[SchemeValue],
    env: Env
  ): TcoResult =
    body match
      case Nil         => Value(Void, "")
      case last :: Nil => evalTail(last, env)
      case head :: tail =>
        val (_, newEnv, o) = Interpreter.eval(head, env)
        prependOutput(evalBodySeqTail(tail, newEnv), o)

  def prependOutput(result: TcoResult, prefix: Output): TcoResult =
    if prefix.isEmpty then result
    else
      result match
        case Value(v, o)                   => Value(v, prefix + o)
        case TailCall(f, a, callingEnv, o) => TailCall(f, a, callingEnv, prefix + o)
