package ming

import Evaluator.{Done, EvalResult}

/** make-parameter and parameterize support for L27. */
object Parameters:

  /** Evaluate (make-parameter init [converter]). */
  def evalMakeParameter(
    args: List[Value],
    env: Env,
    out: String
  ): EvalResult =
    args match
      case initExpr :: Nil =>
        val (initVal, _, out2) = Evaluator.eval(initExpr, env, out)
        Done(Value.ParameterVal(Array(initVal), None), env, out2)
      case initExpr :: convExpr :: Nil =>
        val (initVal, _, out2)  = Evaluator.eval(initExpr, env, out)
        val (convProc, _, out3) = Evaluator.eval(convExpr, env, out2)
        val (converted, _, out4) =
          applyConverter(convProc, initVal, out3)
        Done(Value.ParameterVal(Array(converted), Some(convProc)), env, out4)
      case _ =>
        throw new EvalError("make-parameter requires 1 or 2 arguments")

  /** Evaluate (parameterize ((param val) ...) body ...). */
  def evalParameterize(
    args: List[Value],
    env: Env,
    out: String
  ): EvalResult =
    args match
      case bindings :: body if body.nonEmpty =>
        val bindingList   = Evaluator.toList(bindings)
        val (saves, out2) = prepareBindings(bindingList, env, out, Nil)
        try
          val (v, e, o) = trampolineBody(body, env, out2)
          restoreBindings(saves)
          Done(v, e, o)
        catch
          case e: Throwable =>
            restoreBindings(saves)
            throw e
      case _ =>
        throw new EvalError("parameterize requires bindings and body")

  private def trampolineBody(
    body: List[Value],
    env: Env,
    out: String
  ): (Value, Env, String) =
    Apply.trampolineResult(Evaluator.evalBodyTail(body, env, out))

  private def prepareBindings(
    bindings: List[Value],
    env: Env,
    out: String,
    acc: List[(Array[Value], Value)]
  ): (List[(Array[Value], Value)], String) =
    bindings match
      case Nil => (acc.reverse, out)
      case binding :: rest =>
        val elems = Evaluator.toList(binding)
        elems match
          case paramExpr :: valExpr :: Nil =>
            val (param, _, out2)  = Evaluator.eval(paramExpr, env, out)
            val (newVal, _, out3) = Evaluator.eval(valExpr, env, out2)
            param match
              case Value.ParameterVal(cell, converter) =>
                val oldVal = cell(0)
                val (finalVal, _, out4) = converter match
                  case Some(conv) => applyConverter(conv, newVal, out3)
                  case None       => (newVal, env, out3)
                cell(0) = finalVal
                prepareBindings(rest, env, out4, (cell, oldVal) :: acc)
              case _ =>
                throw new EvalError(
                  "parameterize: not a parameter object"
                )
          case _ =>
            throw new EvalError("parameterize: bad binding form")

  private def restoreBindings(
    saves: List[(Array[Value], Value)]
  ): Unit =
    saves.foreach { case (cell, oldVal) => cell(0) = oldVal }

  private def applyConverter(
    conv: Value,
    value: Value,
    out: String
  ): (Value, Env, String) =
    Apply.applyProcTail(conv, List(value), None, out) match
      case Done(v, e, o) => (v, e, o)
      case Evaluator.Bounce(e2, env2, o) =>
        Evaluator.eval(e2, env2, o)
      case gb: Evaluator.GuardBounce =>
        ExceptionHandling.guardLoop(gb)
