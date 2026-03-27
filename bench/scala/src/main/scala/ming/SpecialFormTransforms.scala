package ming

private[ming] object SpecialFormTransforms:

  def desugarLet(args: List[Expr], pos: SourcePos): Expr =
    args match
      case Expr.ListExpr(bindings, _) :: body if body.nonEmpty =>
        makeLetApplication(EvaluatorForms.parseLetBindings(bindings), body, pos)
      case Expr.Symbol(name, _) :: Expr.ListExpr(bindings, _) :: body if body.nonEmpty =>
        val parsedBindings = EvaluatorForms.parseLetBindings(bindings)
        val paramsExpr     = Expr.ListExpr(parsedBindings.map(binding => Expr.Symbol(binding.name, pos)), pos)
        val lambdaExpr =
          Expr.ListExpr(List(Expr.Symbol("lambda", pos), paramsExpr) ++ body, pos)
        val namedBinding =
          Expr.ListExpr(List(Expr.Symbol(name, pos), lambdaExpr), pos)
        Expr.ListExpr(
          List(
            Expr.Symbol("letrec", pos),
            Expr.ListExpr(List(namedBinding), pos),
            Expr.ListExpr(Expr.Symbol(name, pos) :: parsedBindings.map(_.valueExpr), pos)
          ),
          pos
        )
      case _ =>
        throw EvalError.at(pos, "invalid let")

  def desugarLetStar(args: List[Expr], pos: SourcePos): Expr =
    args match
      case Expr.ListExpr(bindings, _) :: body if body.nonEmpty =>
        nestLetBindings(EvaluatorForms.parseLetBindings(bindings), body, pos)
      case _ =>
        throw EvalError.at(pos, "invalid let*")

  def desugarDo(args: List[Expr], pos: SourcePos): Expr =
    args match
      case Expr.ListExpr(bindings, _) :: Expr.ListExpr(testExpr :: resultExprs, _) :: body =>
        val parsedBindings = EvaluatorForms.parseDoBindings(bindings)
        val loopName       = MacroSyntax.freshIdentifier("do_loop")
        val paramsExpr =
          Expr.ListExpr(parsedBindings.map(binding => Expr.Symbol(binding.name, pos)), pos)
        val loopCallArgs = parsedBindings.map {
          case DoBinding(name, _, Some(stepExpr)) =>
            stepExpr
          case DoBinding(name, _, None) =>
            Expr.Symbol(name, pos)
        }
        val recurseExpr =
          Expr.ListExpr(Expr.Symbol(loopName, pos) :: loopCallArgs, pos)
        val falseBranch =
          beginExpr(body :+ recurseExpr, pos)
        val trueBranch =
          beginExpr(resultExprs, pos)
        val loopBody =
          Expr.ListExpr(
            List(
              Expr.Symbol("if", pos),
              testExpr,
              trueBranch,
              falseBranch
            ),
            pos
          )
        val lambdaExpr =
          Expr.ListExpr(
            List(
              Expr.Symbol("lambda", pos),
              paramsExpr,
              loopBody
            ),
            pos
          )
        val bindingExpr =
          Expr.ListExpr(List(Expr.Symbol(loopName, pos), lambdaExpr), pos)
        Expr.ListExpr(
          List(
            Expr.Symbol("letrec", pos),
            Expr.ListExpr(List(bindingExpr), pos),
            Expr.ListExpr(Expr.Symbol(loopName, pos) :: parsedBindings.map(_.initExpr), pos)
          ),
          pos
        )
      case _ =>
        throw EvalError.at(pos, "invalid do")

  def desugarGuard(args: List[Expr], pos: SourcePos): Expr =
    args match
      case Expr.ListExpr(Expr.Symbol(name, namePos) :: clauses, _) :: body if body.nonEmpty =>
        val returnName    = MacroSyntax.freshIdentifier("guard_return")
        val exceptionName = MacroSyntax.freshIdentifier("guard_exception")
        val condClauses =
          if clauses.exists(isElseClause) then clauses
          else clauses :+ reRaiseGuardClause(name, namePos, pos)

        val condExpr =
          Expr.ListExpr(Expr.Symbol("cond", pos) :: condClauses, pos)
        val bindingExpr =
          Expr.ListExpr(List(Expr.Symbol(name, namePos), Expr.Symbol(exceptionName, pos)), pos)
        val letExpr =
          Expr.ListExpr(
            List(
              Expr.Symbol("let", pos),
              Expr.ListExpr(List(bindingExpr), pos),
              condExpr
            ),
            pos
          )
        val handlerExpr =
          Expr.ListExpr(List(Expr.Symbol(returnName, pos), letExpr), pos)
        val handlerLambda =
          Expr.ListExpr(
            List(
              Expr.Symbol("lambda", pos),
              Expr.ListExpr(List(Expr.Symbol(exceptionName, pos)), pos),
              handlerExpr
            ),
            pos
          )
        val thunkLambda =
          Expr.ListExpr(
            List(
              Expr.Symbol("lambda", pos),
              Expr.ListExpr(Nil, pos)
            ) ++ body,
            pos
          )
        val withExceptionHandlerExpr =
          Expr.ListExpr(
            List(
              Expr.Symbol("with-exception-handler", pos),
              handlerLambda,
              thunkLambda
            ),
            pos
          )
        val outerLambda =
          Expr.ListExpr(
            List(
              Expr.Symbol("lambda", pos),
              Expr.ListExpr(List(Expr.Symbol(returnName, pos)), pos),
              withExceptionHandlerExpr
            ),
            pos
          )

        Expr.ListExpr(
          List(
            Expr.Symbol("call/cc", pos),
            outerLambda
          ),
          pos
        )
      case _ =>
        throw EvalError.at(pos, "invalid guard")

  private def makeLetApplication(bindings: List[LetBinding], body: List[Expr], pos: SourcePos): Expr =
    val paramsExpr = Expr.ListExpr(bindings.map(binding => Expr.Symbol(binding.name, pos)), pos)
    val lambdaExpr =
      Expr.ListExpr(List(Expr.Symbol("lambda", pos), paramsExpr) ++ body, pos)
    Expr.ListExpr(lambdaExpr :: bindings.map(_.valueExpr), pos)

  private def nestLetBindings(bindings: List[LetBinding], body: List[Expr], pos: SourcePos): Expr =
    bindings match
      case Nil =>
        Expr.ListExpr(List(Expr.Symbol("let", pos), Expr.ListExpr(Nil, pos)) ++ body, pos)
      case binding :: Nil =>
        Expr.ListExpr(
          List(
            Expr.Symbol("let", pos),
            Expr.ListExpr(List(letBindingExpr(binding, pos)), pos)
          ) ++ body,
          pos
        )
      case binding :: remaining =>
        Expr.ListExpr(
          List(
            Expr.Symbol("let", pos),
            Expr.ListExpr(List(letBindingExpr(binding, pos)), pos),
            nestLetBindings(remaining, body, pos)
          ),
          pos
        )

  private def letBindingExpr(binding: LetBinding, pos: SourcePos): Expr =
    Expr.ListExpr(List(Expr.Symbol(binding.name, pos), binding.valueExpr), pos)

  private def beginExpr(body: List[Expr], pos: SourcePos): Expr =
    Expr.ListExpr(Expr.Symbol("begin", pos) :: body, pos)

  private def isElseClause(expr: Expr): Boolean =
    expr match
      case Expr.ListExpr(Expr.Symbol("else", _) :: _, _) => true
      case _                                             => false

  private def reRaiseGuardClause(name: String, namePos: SourcePos, pos: SourcePos): Expr =
    Expr.ListExpr(
      List(
        Expr.Symbol("else", pos),
        Expr.ListExpr(
          List(
            Expr.Symbol("raise", pos),
            Expr.Symbol(name, namePos)
          ),
          pos
        )
      ),
      pos
    )
