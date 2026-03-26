package ming

import java.util.UUID

import SchemeModel.*
import SchemeSyntaxSupport.*

private[ming] object SchemeMacros:

  private[ming] val syntaxKeywords: Set[String] = Set(
    "quote",
    "if",
    "case",
    "define",
    "define-record-type",
    "lambda",
    "case-lambda",
    "set!",
    "and",
    "or",
    "begin",
    "cond",
    "guard",
    "let",
    "let*",
    "letrec",
    "letrec*",
    "do",
    "define-syntax",
    "syntax-rules",
    "syntax",
    "syntax-case",
    "with-syntax"
  )

  private[ming] type PatternBindings = Map[String, Vector[Expr]]

  final case class SyntaxRule(
    pattern: Expr,
    template: Expr,
    patternVariables: Set[String]
  )

  def parseSyntaxRules(name: String, transformer: Expr, definitionEnv: Env): SyntaxTransformer =
    transformer match
      case Expr.ListExpr(
            Expr.Symbol("syntax-rules", _) :: Expr.ListExpr(literalExprs, _) :: ruleExprs,
            _
          ) if ruleExprs.nonEmpty =>
        val literals = literalExprs.map {
          case Expr.Symbol(literal, _) if literal != "..." => literal
          case _ =>
            throw new EvalError("syntax-rules literals must be symbols")
        }.toSet

        val rules = ruleExprs.map(parseRule(name, literals, _))
        new SyntaxRulesTransformer(name, literals, rules, definitionEnv)
      case _ =>
        throw new EvalError("invalid syntax-rules form")

  def buildProcedureSyntaxTransformer(
    name: String,
    procedure: Value,
    definitionEnv: Env
  ): SyntaxTransformer =
    new ProcedureSyntaxTransformer(name, procedure, definitionEnv)

  def expandSyntaxTemplate(template: Expr, context: SyntaxContext): ExpandedExpr =
    val syntheticRule = SyntaxRule(template, template, context.patternVariables)
    new SchemeMacroTemplateExpander(
      syntheticRule,
      context.patternBindings,
      context.useSiteEnv,
      context.definitionEnv
    ).expand(template)

  private def parseRule(name: String, literals: Set[String], ruleExpr: Expr): SyntaxRule =
    ruleExpr match
      case Expr.ListExpr(List(pattern, template), _) =>
        SyntaxRule(pattern, template, collectPatternVariables(pattern, name, literals))
      case _ =>
        throw new EvalError("syntax-rules clauses must contain a pattern and template")

  private[ming] def collectPatternVariables(
    pattern: Expr,
    macroName: String,
    literals: Set[String]
  ): Set[String] =
    pattern match
      case Expr.Symbol(name, _) if isPatternVariable(name, macroName, literals) =>
        Set(name)
      case Expr.ListExpr(items, _) =>
        items.foldLeft(Set.empty[String]) { (acc, item) =>
          item match
            case Expr.Symbol("...", _) => acc
            case _                     => acc ++ collectPatternVariables(item, macroName, literals)
        }
      case _ =>
        Set.empty

  final private class SyntaxRulesTransformer(
    name: String,
    literals: Set[String],
    rules: List[SyntaxRule],
    definitionEnv: Env
  ) extends SyntaxTransformer:

    override def expand(invocation: Expr, useSiteEnv: Env): ExpandedExpr =
      val normalizedInvocation = normalizeInvocation(invocation)

      rules.iterator
        .map(rule => tryExpand(rule, normalizedInvocation, useSiteEnv))
        .collectFirst { case Some(expanded) => expanded }
        .getOrElse(throw new EvalError(s"no matching syntax-rules clause for $name"))

    private def tryExpand(
      rule: SyntaxRule,
      invocation: Expr,
      useSiteEnv: Env
    ): Option[ExpandedExpr] =
      SchemeMacroPatternMatcher
        .matchPattern(rule.pattern, invocation, Map.empty, name, literals)
        .map(bindings =>
          new SchemeMacroTemplateExpander(rule, bindings, useSiteEnv, definitionEnv)
            .expand(rule.template)
        )

    private def normalizeInvocation(invocation: Expr): Expr =
      invocation match
        case Expr.ListExpr(operator :: rest, pos) =>
          Expr.ListExpr(Expr.Symbol(name, operator.pos) :: rest, pos)
        case other =>
          other

  final private class ProcedureSyntaxTransformer(
    name: String,
    procedure: Value,
    definitionEnv: Env
  ) extends SyntaxTransformer:

    override def expand(invocation: Expr, useSiteEnv: Env): ExpandedExpr =
      val context = SyntaxContext(definitionEnv, useSiteEnv, Map.empty, Set.empty)
      val result = SchemeEvaluator.applyProcedureInCurrentContext(
        contextualizeProcedure(procedure, context),
        List(Value.SyntaxObject(invocation, useSiteEnv)),
        Some(invocation.pos)
      )

      result match
        case Value.SyntaxObject(expr, contextEnv) =>
          ExpandedExpr(expr, contextEnv)
        case _ =>
          throw new EvalError(s"syntax transformer $name must return a syntax object")

    private def contextualizeProcedure(procedure: Value, context: SyntaxContext): Value =
      procedure match
        case Value.Closure(name, fixedParams, restParam, body, closureEnv) =>
          Value.Closure(
            name,
            fixedParams,
            restParam,
            body,
            injectMacroContext(closureEnv, context)
          )
        case Value.CaseClosure(clauses) =>
          Value.CaseClosure(
            clauses.map(clause => clause.copy(env = injectMacroContext(clause.env, context)))
          )
        case other =>
          other

  private[ming] def isPatternVariable(
    name: String,
    macroName: String,
    literals: Set[String]
  ): Boolean =
    name != "..." && name != "_" && name != macroName && !literals.contains(name)

  private[ming] def requireIdentifier(expr: Expr, message: String): String =
    expr match
      case Expr.Symbol(name, _) => name
      case _                    => throw new EvalError(message)

  private[ming] def lookupPatternBinding(
    name: String,
    bindings: PatternBindings,
    repetitionIndex: Option[Int]
  ): Expr =
    bindings.get(name) match
      case Some(values) =>
        repetitionIndex match
          case Some(index) if index < values.length =>
            values(index)
          case Some(_) if values.length == 1 =>
            values.head
          case Some(_) =>
            throw new EvalError(s"macro variable $name requires ellipsis")
          case None if values.length == 1 =>
            values.head
          case None =>
            throw new EvalError(s"macro variable $name requires ellipsis")
      case None =>
        throw new EvalError(s"macro variable $name has no match")

  private[ming] def repetitionCount(
    template: Expr,
    patternVariables: Set[String],
    bindings: PatternBindings
  ): Int =
    val counts = collectReferencedPatternVariables(template, patternVariables).toList.map { name =>
      bindings.get(name).map(_.length).getOrElse(0)
    }

    if counts.isEmpty then throw new EvalError("ellipsis template must reference a pattern variable")

    val repeatedCounts = counts.filter(_ != 1).distinct
    if repeatedCounts.nonEmpty then
      if repeatedCounts.length != 1 then throw new EvalError("inconsistent macro repetition lengths")
      repeatedCounts.head
    else counts.head

  private def collectReferencedPatternVariables(
    expr: Expr,
    patternVariables: Set[String]
  ): Set[String] =
    expr match
      case Expr.Symbol(name, _) if patternVariables.contains(name) =>
        Set(name)
      case Expr.ListExpr(items, _) =>
        items.foldLeft(Set.empty[String]) { (acc, item) =>
          item match
            case Expr.Symbol("...", _) => acc
            case _                     => acc ++ collectReferencedPatternVariables(item, patternVariables)
        }
      case _ =>
        Set.empty

  private[ming] def freshIdentifier(base: String): String =
    val sanitized = sanitizeIdentifierBase(base)
    val suffix    = UUID.randomUUID().toString.replace('-', '_')
    s"__macro_${sanitized}_$suffix"

  private def sanitizeIdentifierBase(base: String): String =
    if base.isEmpty then "tmp"
    else
      base.map {
        case char if char.isLetterOrDigit => char
        case _                            => '_'
      }
