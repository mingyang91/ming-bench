package ming

object CekSyntaxSteps:

  import CekApply.{applyFunc, setupBody}

  def stepEvalSyntaxCase(
    s: CekState,
    stxExpr: Expr,
    literals: List[Expr],
    clauses: List[Expr]
  ): Unit =
    val litNames = literals.map {
      case Expr.Sym(n) => n
      case _           => throw EvalError("syntax-case: literals must be symbols")
    }
    s.k = SyntaxCaseMatchK(litNames, clauses, s.env, s.k)
    s.expr = stxExpr

  def stepEvalSyntax(s: CekState, template: Expr, curEnv: Env): Unit =
    val bindings = Macros.currentSyntaxBindings
    if bindings.isEmpty then
      s.value = template
      s.evaluating = false
    else
      template match
        case Expr.Sym(name) if bindings.contains(name) =>
          bindings(name) match
            case Left(v)  => s.value = v
            case Right(_) => throw EvalError(s"syntax: ellipsis variable $name used without ellipsis")
          s.evaluating = false
        case _ =>
          val (expanded, hygieneEnv) = Macros.expandSyntaxTemplate(template, curEnv)
          s.value = Expr.SyntaxExpanded(expanded, hygieneEnv)
          s.evaluating = false

  def stepEvalWithSyntax(s: CekState, bindings: List[Expr], body: List[Expr], curEnv: Env): Unit =
    val pairs = bindings.map {
      case Expr.Lst(List(Expr.Sym(name), expr)) => (name, CekMachine.eval(expr, curEnv))
      case _                                    => throw EvalError("with-syntax: invalid binding")
    }
    val newBindings = pairs.map((name, value) => name -> Left(value).asInstanceOf[Either[Expr, List[Expr]]]).toMap
    Macros.addSyntaxBindings(newBindings)
    s.k = SyntaxCaseCleanupK(s.k)
    setupBody(s, body, curEnv, s.k)

  def stepEvalMacro(s: CekState, name: String, lst: Expr.Lst, curEnv: Env): Unit =
    curEnv.lookup(name) match
      case mac: Expr.Macro =>
        val (expanded, hygieneEnv) =
          Macros.expandMacro(mac.literals, mac.rules, lst.elems, mac.defEnv, curEnv)
        s.expr = expanded
        s.env = hygieneEnv
      case transf: Expr.TransformerMacro =>
        Macros.pushSyntaxContext(transf.defEnv, curEnv)
        CekApply.applyFunc(s, transf.transformer, List(lst), TransformerMacroReturnK(curEnv, s.k))
      case _ => throw EvalError(s"$name is not a macro")

  def stepKontSyntaxCaseMatch(
    s: CekState,
    literals: List[String],
    clauses: List[Expr],
    env: Env,
    kk: Kont
  ): Unit =
    val input   = s.value
    var matched = false
    for clause <- clauses if !matched do
      clause match
        case Expr.Lst(pattern :: bodyParts) if bodyParts.nonEmpty =>
          Macros.matchSyntaxCasePattern(pattern, input, literals) match
            case Some(bindings) =>
              matched = true
              Macros.addSyntaxBindings(bindings)
              val body = if bodyParts.length > 1 then bodyParts.last else bodyParts.head
              s.k = SyntaxCaseCleanupK(kk)
              s.expr = body
              s.env = env
              s.evaluating = true
            case None => ()
        case _ => throw EvalError("syntax-case: invalid clause")
    if !matched then throw EvalError("syntax-case: no matching pattern")

  def stepKontTransformerMacroReturn(s: CekState, useEnv: Env, kk: Kont): Unit =
    Macros.popSyntaxContext()
    s.value match
      case Expr.SyntaxExpanded(expr, hygieneEnv) =>
        s.expr = expr
        s.env = hygieneEnv
        s.evaluating = true
        s.k = kk
      case other =>
        s.expr = other
        s.env = useEnv
        s.evaluating = true
        s.k = kk
