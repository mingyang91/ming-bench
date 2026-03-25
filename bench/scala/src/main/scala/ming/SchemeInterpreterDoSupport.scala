package ming

private[ming] object SchemeInterpreterDoSupport:

  import SchemeInterpreter.{Expr, Value}

  import SchemeInterpreterSyntax.isTruthy

  final private case class DoBinding(
    name: String,
    initExpr: Expr,
    stepExpr: Option[Expr],
    pos: SourcePos
  )

  def eval(
    args: List[Expr],
    env: Env,
    macros: MacroScope,
    pos: SourcePos,
    evalExpr: (Expr, Env, MacroScope) => Value,
    evalSequenceExprs: (List[Expr], Env, MacroScope) => Value
  ): Value =
    args match
      case Expr.ListExpr(bindingExprs, _) :: Expr.ListExpr(testExpr :: resultExprs, _) :: body =>
        val bindings   = readDoBindings(bindingExprs)
        val initValues = bindings.map(binding => evalExpr(binding.initExpr, env, macros))
        val doEnv      = Env.child(env, bindings.map(_.name).zip(initValues))
        val doMacros   = MacroScope.child(macros)

        while true do
          if isTruthy(evalExpr(testExpr, doEnv, doMacros)) then
            return if resultExprs.isEmpty then Value.Void else evalSequenceExprs(resultExprs, doEnv, doMacros)

          evalSequenceExprs(body, doEnv, doMacros)

          val nextValues = bindings.map { binding =>
            binding.stepExpr match
              case Some(stepExpr) => evalExpr(stepExpr, doEnv, doMacros)
              case None           => doEnv.lookup(binding.name, binding.pos)
          }

          bindings.zip(nextValues).foreach { case (binding, value) =>
            doEnv.assign(binding.name, value, binding.pos)
          }

        Value.Void
      case _ =>
        throw EvalError.at(pos, "invalid do")

  private def readDoBindings(bindingExprs: List[Expr]): List[DoBinding] =
    bindingExprs.map {
      case Expr.ListExpr(List(Expr.Symbol(name, _), initExpr), bindingPos) =>
        DoBinding(name, initExpr, None, bindingPos)
      case Expr.ListExpr(List(Expr.Symbol(name, _), initExpr, stepExpr), bindingPos) =>
        DoBinding(name, initExpr, Some(stepExpr), bindingPos)
      case other =>
        throw EvalError.at(other.pos, s"invalid do binding: ${SchemeRendering.renderExpr(other)}")
    }
