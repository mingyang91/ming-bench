package ming

import scala.util.DynamicVariable

private[ming] object SchemeSyntax:

  import BuiltinSupport.isTruthy
  import SchemeInterpreter.{Expr, Procedure, Value}
  import SchemeInterpreterSyntax.{datumToExpr, quote}
  import SyntaxRules.Capture
  import SyntaxRules.Capture.*

  final private case class SyntaxCaseClause(
    pattern: Expr,
    fender: Option[Expr],
    body: List[Expr],
    pos: SourcePos
  )

  final private case class TemplateContext(env: Env, macros: MacroScope)

  private val syntaxBindingsVar  = new DynamicVariable[Map[String, Capture]](Map.empty)
  private val templateContextVar = new DynamicVariable[Option[TemplateContext]](None)

  def expandTransformer(
    procedure: Procedure,
    call: Expr.ListExpr,
    pos: SourcePos,
    definitionEnv: Env,
    definitionMacros: MacroScope
  ): Expr =
    val result = templateContextVar.withValue(Some(TemplateContext(definitionEnv, definitionMacros))) {
      syntaxBindingsVar.withValue(Map.empty) {
        SchemeInterpreter.applyProcedure(procedure, List(Value.SyntaxObject(call)), pos)
      }
    }
    asSyntaxObject(result, pos, "macro transformer").expr

  def evalSyntax(
    args: List[Expr],
    env: Env,
    macros: MacroScope,
    pos: SourcePos
  ): Value =
    args match
      case template :: Nil =>
        Value.SyntaxObject(instantiateTemplate(template, env, macros))
      case _ =>
        throw EvalError.at(pos, s"syntax expected 1 argument, got ${args.length}")

  def evalSyntaxCase(
    args: List[Expr],
    env: Env,
    macros: MacroScope,
    pos: SourcePos
  ): Value =
    args match
      case targetExpr :: Expr.ListExpr(literalExprs, _) :: clauseExprs if clauseExprs.nonEmpty =>
        val input = asSyntaxObject(SchemeInterpreter.evalExpr(targetExpr, env, macros), targetExpr.pos, "syntax-case")
        val literals = literalExprs.map {
          case Expr.Symbol(name, _) => name
          case other =>
            throw EvalError.at(other.pos, "syntax-case literals must be identifiers")
        }.toSet

        clauseExprs.iterator
          .flatMap(clause => evalSyntaxCaseClause(clause, input.expr, literals, env, macros))
          .take(1)
          .toList
          .headOption
          .getOrElse(throw EvalError.at(pos, "syntax-case did not match any clause"))

      case _ =>
        throw EvalError.at(pos, "invalid syntax-case")

  def evalWithSyntax(
    args: List[Expr],
    env: Env,
    macros: MacroScope,
    pos: SourcePos
  ): Value =
    args match
      case Expr.ListExpr(bindingExprs, _) :: body if body.nonEmpty =>
        val bindings = collectWithSyntaxBindings(bindingExprs, env, macros)
        val bodyEnv  = Env.child(env, bindingValues(bindings))
        withBindings(bindings) {
          SchemeInterpreter.evalSequence(body, bodyEnv, macros)
        }

      case _ =>
        throw EvalError.at(pos, "invalid with-syntax")

  def syntaxToDatum(value: Value, pos: SourcePos, context: String): Value =
    quote(asSyntaxObject(value, pos, context).expr)

  def datumToSyntax(
    contextValue: Value,
    datumValue: Value,
    pos: SourcePos,
    context: String
  ): Value.SyntaxObject =
    val syntaxContext = asSyntaxObject(contextValue, pos, context)
    Value.SyntaxObject(datumToExpr(datumValue, syntaxContext.expr.pos, context))

  private def instantiateTemplate(expr: Expr, env: Env, macros: MacroScope): Expr =
    val context = templateContextVar.value.getOrElse(TemplateContext(env, macros))
    new SyntaxTemplateExpander(context.env, context.macros, SyntaxFreshIds.next()).instantiate(
      expr,
      syntaxBindingsVar.value
    )

  private def evalSyntaxCaseClause(
    clauseExpr: Expr,
    input: Expr,
    literals: Set[String],
    env: Env,
    macros: MacroScope
  ): Option[Value] =
    val clause = parseSyntaxCaseClause(clauseExpr)
    SyntaxPatternMatcher.matchPattern(clause.pattern, input, literals).flatMap { bindings =>
      val clauseEnv = Env.child(env, bindingValues(bindings))
      val allowed =
        clause.fender.forall { fenderExpr =>
          withBindings(bindings) {
            isTruthy(SchemeInterpreter.evalExpr(fenderExpr, clauseEnv, macros))
          }
        }

      Option.when(allowed) {
        withBindings(bindings) {
          SchemeInterpreter.evalSequence(clause.body, clauseEnv, macros)
        }
      }
    }

  private def parseSyntaxCaseClause(expr: Expr): SyntaxCaseClause =
    expr match
      case Expr.ListExpr(pattern :: bodyExpr :: Nil, clausePos) =>
        SyntaxCaseClause(pattern, None, List(bodyExpr), clausePos)

      case Expr.ListExpr(pattern :: fender :: body, clausePos) if body.nonEmpty =>
        SyntaxCaseClause(pattern, Some(fender), body, clausePos)

      case Expr.ListExpr(_ :: Nil, clausePos) =>
        throw EvalError.at(clausePos, "syntax-case clause requires a template")

      case other =>
        throw EvalError.at(other.pos, "invalid syntax-case clause")

  private def collectWithSyntaxBindings(
    bindingExprs: List[Expr],
    env: Env,
    macros: MacroScope
  ): Map[String, Capture] =
    bindingExprs.foldLeft(Map.empty[String, Capture]) { (acc, bindingExpr) =>
      bindingExpr match
        case Expr.ListExpr(List(pattern, valueExpr), _) =>
          val bindingEnv = Env.child(env, bindingValues(acc))
          val syntaxValue = withBindings(acc) {
            asSyntaxObject(SchemeInterpreter.evalExpr(valueExpr, bindingEnv, macros), valueExpr.pos, "with-syntax")
          }
          val matched = SyntaxPatternMatcher.matchPattern(pattern, syntaxValue.expr, Set.empty).getOrElse {
            throw EvalError.at(pattern.pos, "with-syntax pattern did not match")
          }
          SyntaxPatternMatcher.mergeCaptures(acc, matched).getOrElse {
            throw EvalError.at(pattern.pos, "with-syntax produced inconsistent bindings")
          }

        case other =>
          throw EvalError.at(other.pos, "invalid with-syntax binding")
    }

  private def bindingValues(bindings: Map[String, Capture]): List[(String, Value)] =
    bindings.toList.map { case (name, capture) =>
      name -> captureToValue(capture)
    }

  private def captureToValue(capture: Capture): Value =
    capture match
      case Single(expr) =>
        Value.SyntaxObject(expr)
      case Repeated(values) =>
        Value.list(values.map(captureToValue))

  private def withBindings[A](bindings: Map[String, Capture])(body: => A): A =
    syntaxBindingsVar.withValue(syntaxBindingsVar.value ++ bindings)(body)

  private def asSyntaxObject(value: Value, pos: SourcePos, context: String): Value.SyntaxObject =
    value match
      case syntax: Value.SyntaxObject =>
        syntax
      case other =>
        Value.SyntaxObject(datumToExpr(other, pos, context))
