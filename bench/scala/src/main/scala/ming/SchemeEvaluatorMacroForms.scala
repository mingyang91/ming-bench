package ming

import SchemeBuiltinSupport.*
import SchemeEvaluatorState.*
import SchemeMacros.*
import SchemeModel.*
import SchemeRuntime.*
import SchemeSyntaxSupport.*

private[ming] trait SchemeEvaluatorMacroForms extends SchemeEvaluatorSpecialForms:

  final private case class WithSyntaxBindingSpec(pattern: Expr, valueExpr: Expr)

  final override protected def evalDefineSyntax(
    args: List[Expr],
    env: Env,
    continuation: Continuation,
    pos: SourcePos
  ): Computation =
    args match
      case Expr.Symbol(name, _) :: transformer :: Nil =>
        transformer match
          case Expr.ListExpr(Expr.Symbol("syntax-rules", _) :: _, _) =>
            env.defineSyntax(name, parseSyntaxRules(name, transformer, env))
            resume(continuation, Value.VoidValue)
          case _ =>
            eval(
              transformer,
              env,
              contextualCont(pos) { value =>
                if !isProcedure(value) then throw new EvalError("define-syntax transformer must be a procedure")
                env.defineSyntax(name, buildProcedureSyntaxTransformer(name, value, env))
                resume(continuation, Value.VoidValue)
              }
            )
      case _ =>
        throw new EvalError("invalid define-syntax form")

  final override protected def evalSyntax(
    args: List[Expr],
    env: Env,
    continuation: Continuation,
    pos: SourcePos
  ): Computation =
    args match
      case template :: Nil =>
        val context  = requireMacroContext(env, "syntax")
        val expanded = expandSyntaxTemplate(template, context)
        resume(continuation, Value.SyntaxObject(expanded.expr, expanded.env))
      case _ =>
        throw new EvalError(s"syntax expected 1 argument, got ${args.length}")

  final override protected def evalSyntaxCase(
    args: List[Expr],
    env: Env,
    continuation: Continuation,
    pos: SourcePos
  ): Computation =
    args match
      case inputExpr :: Expr.ListExpr(literalExprs, _) :: clauseExprs if clauseExprs.nonEmpty =>
        val literals = literalExprs.map {
          case Expr.Symbol(name, _) if name != "..." => name
          case _ =>
            throw new EvalError("syntax-case literals must be symbols")
        }.toSet

        eval(
          inputExpr,
          env,
          contextualCont(pos) { value =>
            val syntaxObject = requireSyntaxObject("syntax-case", value)
            suspend(
              evalSyntaxCaseClauses(
                syntaxObject,
                literals,
                clauseExprs,
                env,
                continuation
              )
            )
          }
        )
      case _ =>
        throw new EvalError("invalid syntax-case form")

  final override protected def evalWithSyntax(
    args: List[Expr],
    env: Env,
    continuation: Continuation,
    pos: SourcePos
  ): Computation =
    args match
      case bindingsExpr :: body if body.nonEmpty =>
        val bindingSpecs = parseWithSyntaxBindings(bindingsExpr)
        val baseContext  = requireMacroContext(env, "with-syntax")

        evalWithSyntaxBindings(
          bindingSpecs,
          env,
          Map.empty,
          Map.empty,
          Set.empty
        ) { (patternBindings, runtimeBindings, patternVariables) =>
          val bindingEnv = new Env(Some(env))
          bindRuntimeSyntaxVariables(bindingEnv, runtimeBindings)
          val bodyEnv = extendMacroContext(
            bindingEnv,
            extendContext(baseContext, patternBindings, patternVariables)
          )
          evalSequence(body, bodyEnv, continuation)
        }
      case _ =>
        throw new EvalError("invalid with-syntax form")

  private def evalSyntaxCaseClauses(
    syntaxObject: Value.SyntaxObject,
    literals: Set[String],
    clauses: List[Expr],
    env: Env,
    continuation: Continuation
  ): Computation =
    val baseContext = requireMacroContext(env, "syntax-case")

    def loop(remaining: List[Expr]): Computation =
      remaining match
        case Nil =>
          throw new EvalError("syntax-case found no matching clause")
        case Expr.ListExpr(pattern :: bodyExprs, _) :: tail =>
          SchemeMacroPatternMatcher.matchPattern(
            pattern,
            syntaxObject.expr,
            Map.empty,
            "",
            literals
          ) match
            case Some(bindings) =>
              val bindingEnv = new Env(Some(env))
              bindRuntimeSyntaxVariables(
                bindingEnv,
                runtimeSyntaxBindings(bindings, syntaxObject.contextEnv)
              )
              val clauseEnv = extendMacroContext(
                bindingEnv,
                extendContext(
                  baseContext,
                  bindings,
                  collectPatternVariables(pattern, "", literals)
                )
              )

              bodyExprs match
                case outputExpr :: Nil =>
                  eval(outputExpr, clauseEnv, continuation)
                case fenderExpr :: outputExpr :: Nil =>
                  eval(
                    fenderExpr,
                    clauseEnv,
                    contextualCont(fenderExpr.pos) { value =>
                      if isTruthy(value) then eval(outputExpr, clauseEnv, continuation)
                      else loop(tail)
                    }
                  )
                case _ =>
                  throw new EvalError(
                    "syntax-case clauses must contain a pattern and 1 or 2 expressions"
                  )
            case None =>
              loop(tail)
        case _ =>
          throw new EvalError("syntax-case clauses must be non-empty lists")

    loop(clauses)

  private def parseWithSyntaxBindings(bindingsExpr: Expr): List[WithSyntaxBindingSpec] =
    bindingsExpr match
      case Expr.ListExpr(bindingExprs, _) =>
        val parsed = bindingExprs.map {
          case Expr.ListExpr(List(pattern, valueExpr), _) =>
            WithSyntaxBindingSpec(pattern, valueExpr)
          case _ =>
            throw new EvalError("with-syntax bindings must contain (pattern expr) pairs")
        }
        ensureDistinct(
          parsed.flatMap(spec => collectPatternVariables(spec.pattern, "", Set.empty).toList),
          "with-syntax bindings"
        )
        parsed
      case _ =>
        throw new EvalError("with-syntax bindings must be a list")

  private def evalWithSyntaxBindings(
    bindingSpecs: List[WithSyntaxBindingSpec],
    env: Env,
    patternBindings: PatternBindings,
    runtimeBindings: RuntimeSyntaxBindings,
    patternVariables: Set[String]
  )(
    continuation: (PatternBindings, RuntimeSyntaxBindings, Set[String]) => Computation
  ): Computation =
    bindingSpecs match
      case Nil =>
        continuation(patternBindings, runtimeBindings, patternVariables)
      case WithSyntaxBindingSpec(pattern, valueExpr) :: tail =>
        eval(
          valueExpr,
          env,
          contextualCont(valueExpr.pos) { value =>
            val syntaxObject = requireSyntaxObject("with-syntax", value)
            val currentBindings =
              SchemeMacroPatternMatcher
                .matchPattern(pattern, syntaxObject.expr, Map.empty, "", Set.empty)
                .getOrElse(throw new EvalError("with-syntax pattern did not match"))

            suspend(
              evalWithSyntaxBindings(
                tail,
                env,
                patternBindings ++ currentBindings,
                runtimeBindings ++ runtimeSyntaxBindings(
                  currentBindings,
                  syntaxObject.contextEnv
                ),
                patternVariables ++ collectPatternVariables(pattern, "", Set.empty)
              )(continuation)
            )
          }
        )
