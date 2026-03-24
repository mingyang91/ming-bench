package ming

import scala.collection.mutable

/** Syntax-case evaluation: syntax-case forms, syntax templates, with-syntax, and macro expansion. */
object SyntaxCaseEval:

  /** Evaluate a syntax-case form */
  def evalSyntaxCase(stxExpr: Expr, litExprs: List[Expr], clauses: List[Expr], env: Env): SchemeVal =
    val stx = Evaluator.eval(stxExpr, env)
    val stxE = stx match
      case SchemeVal.SyntaxObj(e) => e
      case _                      => throw new EvalError("syntax-case: expected a syntax object")
    val literals = litExprs.map {
      case Expr.Symbol(n) => n
      case _              => throw new EvalError("syntax-case: literals must be identifiers")
    }.toSet
    for clause <- clauses do
      clause match
        case Expr.SList(pattern :: fender :: body :: Nil) =>
          matchSyntaxCasePattern(pattern, stxE, literals, env).foreach { childEnv =>
            if Evaluator.isTruthy(Evaluator.eval(fender, childEnv)) then return Evaluator.evalBody(List(body), childEnv)
          }
        case Expr.SList(pattern :: body :: Nil) =>
          matchSyntaxCasePattern(pattern, stxE, literals, env).foreach { childEnv =>
            return Evaluator.evalBody(List(body), childEnv)
          }
        case _ => throw new EvalError("syntax-case: invalid clause")
    throw new EvalError("syntax-case: no matching clause")

  private def matchSyntaxCasePattern(
    pattern: Expr,
    stxExpr: Expr,
    literals: Set[String],
    env: Env
  ): Option[Env] =
    MacroExpander.matchOne(pattern, stxExpr, literals).map { bindings =>
      val childEnv = new Env(mutable.Map.empty, Some(env))
      for (name, binding) <- bindings do
        binding match
          case Left(e)   => childEnv.define(name, SchemeVal.SyntaxObj(e))
          case Right(es) => childEnv.define(name, SchemeVal.SyntaxList(es))
      childEnv
    }

  /** Evaluate a syntax (template) form */
  def evalSyntaxForm(tmpl: Expr, env: Env): SchemeVal =
    val renaming = MacroExpander.currentRenaming.get()
    if renaming != null then SchemeVal.SyntaxObj(MacroExpander.expandSyntaxTemplate(tmpl, env, renaming))
    else
      tmpl match
        case Expr.Symbol(n) =>
          env.lookupOpt(n) match
            case Some(stx: SchemeVal.SyntaxObj) => stx
            case Some(sl: SchemeVal.SyntaxList) => sl
            case _                              => SchemeVal.SyntaxObj(tmpl)
        case _ => SchemeVal.SyntaxObj(tmpl)

  /** Evaluate with-syntax form */
  def evalWithSyntax(bindings: List[Expr], body: List[Expr], env: Env): SchemeVal =
    val childEnv = new Env(mutable.Map.empty, Some(env))
    for binding <- bindings do
      binding match
        case Expr.SList(Expr.Symbol(name) :: valExpr :: Nil) =>
          Evaluator.eval(valExpr, env) match
            case stx: SchemeVal.SyntaxObj => childEnv.define(name, stx)
            case sl: SchemeVal.SyntaxList => childEnv.define(name, sl)
            case other                    => childEnv.define(name, SchemeVal.SyntaxObj(EvalHelpers.valToExpr(other)))
        case _ => throw new EvalError("with-syntax: invalid binding")
    Evaluator.evalBody(body, childEnv)

  /** Expand and evaluate a syntax-case macro transformer */
  def expandSyntaxCaseMacro(form: Expr, name: String, mt: SchemeVal.MacroTransformer, useEnv: Env): SchemeVal =
    val prevRenaming = MacroExpander.currentRenaming.get()
    val renaming     = mutable.Map[String, String]()
    MacroExpander.currentRenaming.set(renaming)
    try
      val stxObj = SchemeVal.SyntaxObj(form)
      val result = Evaluator.applyProc(mt.proc, List(stxObj))
      result match
        case SchemeVal.SyntaxObj(expandedExpr) =>
          for (origName, gsName) <- renaming do mt.defEnv.lookupOpt(origName).foreach(v => useEnv.define(gsName, v))
          Evaluator.eval(expandedExpr, useEnv)
        case other =>
          throw new EvalError(s"$name: transformer must return a syntax object, got ${other.display}")
    finally MacroExpander.currentRenaming.set(prevRenaming)
