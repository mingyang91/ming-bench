package ming

import SchemeValue.*
import Evaluator.{EvalS, ReturnS, Step}

/** Overflow continuation handlers extracted from KontApply. */
private[ming] object KontApplyExt:

  def applySyntaxCase(
    scrutinee: SchemeValue,
    literals: List[String],
    clauses: List[SchemeValue],
    env: Env,
    nextK: Kont,
    out: String
  ): Step =
    val (bindings, body) = SyntaxCase.matchClauses(scrutinee, literals, clauses)
    val existing = env.get(SyntaxCase.BindingsKey) match
      case Some(sb: SchemeSyntaxBindings) => sb
      case _                              => SchemeSyntaxBindings(Map.empty, env)
    val merged = SchemeSyntaxBindings(existing.bindings ++ bindings, existing.defEnv)
    val newEnv = env.extend(SyntaxCase.BindingsKey, merged)
    EvalS(body, newEnv, nextK, out)

  def applyWithSyntax(
    value: SchemeValue,
    name: String,
    remaining: List[(String, SchemeValue)],
    body: List[SchemeValue],
    env: Env,
    nextK: Kont,
    out: String
  ): Step =
    val existing = env.get(SyntaxCase.BindingsKey) match
      case Some(sb: SchemeSyntaxBindings) => sb
      case _                              => SchemeSyntaxBindings(Map.empty, env)
    val updated = SchemeSyntaxBindings(
      existing.bindings + (name -> SyntaxCase.Single(value)),
      existing.defEnv
    )
    val newEnv = env.extend(SyntaxCase.BindingsKey, updated)
    remaining match
      case Nil =>
        SpecialForms.startBody(body, newEnv, nextK, out)
      case (nextName, nextExpr) :: rest =>
        EvalS(nextExpr, newEnv, Kont.WithSyntaxK(nextName, rest, body, newEnv, nextK), out)

  def applyNamedLetInit(
    value: SchemeValue,
    name: String,
    params: List[String],
    evaled: List[SchemeValue],
    remaining: List[SchemeValue],
    body: List[SchemeValue],
    letEnv: Env,
    nextK: Kont,
    out: String
  ): Step =
    val newEvaled = evaled :+ value
    remaining match
      case Nil =>
        val recEnv = Env.RecursiveFrame(
          name,
          closure => SchemeLambda(params, None, body, closure),
          letEnv
        )
        SpecialForms.startSequence(body, recEnv.extend(params, newEvaled), nextK, out)
      case next :: rest =>
        EvalS(
          next,
          letEnv,
          Kont.NamedLetInitK(name, params, newEvaled, rest, body, letEnv, nextK),
          out
        )
