package ming

import SchemeValue.*
import Evaluator.{EvalS, ReturnS, Step}

/** Syntax-related special form handlers extracted from SpecialForms. */
private[ming] object SyntaxFormEval:

  def evalDefineSyntax(
    args: List[SchemeValue],
    env: Env,
    k: Kont,
    out: String
  ): Step = args match
    case SchemeSymbol(name) :: syntaxForm :: Nil =>
      syntaxForm match
        case SchemeList(SchemeSymbol("syntax-rules") :: _) =>
          val m        = Macros.parseSyntaxRules(syntaxForm, env)
          val newEnv   = env.extend(name, m)
          val updatedK = Evaluator.updateSeqEnv(k, newEnv)
          ReturnS(SchemeVoid, updatedK, out)
        case SchemeList(SchemeSymbol("lambda") :: _) =>
          val lam      = Evaluator.eval(syntaxForm, env).asInstanceOf[SchemeLambda]
          val m        = SchemeProcMacro(lam, env)
          val newEnv   = env.extend(name, m)
          val updatedK = Evaluator.updateSeqEnv(k, newEnv)
          ReturnS(SchemeVoid, updatedK, out)
        case _ =>
          val m        = Macros.parseSyntaxRules(syntaxForm, env)
          val newEnv   = env.extend(name, m)
          val updatedK = Evaluator.updateSeqEnv(k, newEnv)
          ReturnS(SchemeVoid, updatedK, out)
    case _ => throw new EvalError("bad define-syntax syntax")

  def evalSyntaxCase(
    args: List[SchemeValue],
    env: Env,
    k: Kont,
    out: String
  ): Step = args match
    case scrutinee :: SchemeList(lits) :: clauses if clauses.nonEmpty =>
      val litNames = lits.map {
        case SchemeSymbol(n) => n
        case other           => throw new EvalError(s"syntax-case: bad literal: ${other.display}")
      }
      EvalS(scrutinee, env, Kont.SyntaxCaseK(litNames, clauses, env, k), out)
    case _ => throw new EvalError("syntax-case: bad syntax")

  def evalSyntaxQuote(
    args: List[SchemeValue],
    env: Env,
    k: Kont,
    out: String
  ): Step =
    if args.length != 1 then throw new EvalError("syntax-quote: expected 1 argument")
    val template = args.head
    val sb = env.get(SyntaxCase.BindingsKey) match
      case Some(sb: SchemeSyntaxBindings) => sb
      case _                              => SchemeSyntaxBindings(Map.empty, env)
    val expanded = SyntaxCase.expandTemplate(template, sb.bindings, sb.defEnv)
    ReturnS(expanded, k, out)

  def evalWithSyntax(
    args: List[SchemeValue],
    env: Env,
    k: Kont,
    out: String
  ): Step = args match
    case SchemeList(bindings) :: body if body.nonEmpty =>
      val parsed = bindings.map {
        case SchemeList(SchemeSymbol(name) :: expr :: Nil) => (name, expr)
        case other => throw new EvalError(s"with-syntax: bad binding: ${other.display}")
      }
      parsed match
        case Nil => SpecialForms.startBody(body, env, k, out)
        case (name, expr) :: rest =>
          EvalS(expr, env, Kont.WithSyntaxK(name, rest, body, env, k), out)
    case _ => throw new EvalError("with-syntax: bad syntax")
