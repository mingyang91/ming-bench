package ming

import SchemeModel.*
import SchemeRuntime.*

private[ming] object SchemeEvaluatorSupport:

  final case class DoBinding(name: String, initExpr: Expr, stepExpr: Option[Expr])

  def parseBindings(bindingsExpr: Expr): List[(String, Expr)] =
    parseBindingPairs(bindingsExpr, "let", requireDistinctNames = true)

  def parseLetStarBindings(bindingsExpr: Expr): List[(String, Expr)] =
    parseBindingPairs(bindingsExpr, "let*", requireDistinctNames = false)

  private def parseBindingPairs(
    bindingsExpr: Expr,
    formName: String,
    requireDistinctNames: Boolean
  ): List[(String, Expr)] =
    bindingsExpr match
      case Expr.ListExpr(bindings, _) =>
        val parsed = bindings.map {
          case Expr.ListExpr(List(Expr.Symbol(name, _), valueExpr), _) =>
            (name, valueExpr)
          case Expr.ListExpr(List(_, _), _) =>
            throw new EvalError(s"$formName bindings must have symbol names")
          case _ =>
            throw new EvalError(s"$formName bindings must contain (name value) pairs")
        }
        if requireDistinctNames then ensureDistinct(parsed.map(_._1), s"$formName bindings")
        parsed
      case _ =>
        throw new EvalError(s"$formName bindings must be a list")

  def buildClosure(
    paramsExpr: List[Expr],
    body: List[Expr],
    env: Env,
    name: Option[String]
  ): Value.Closure =
    val clause = buildCaseLambdaClause(paramsExpr, body, env)
    Value.Closure(name, clause.fixedParams, clause.restParam, clause.body, clause.env)

  def buildCaseClosure(clausesExpr: List[Expr], env: Env): Value.CaseClosure =
    if clausesExpr.isEmpty then throw new EvalError("case-lambda expected at least 1 clause")
    Value.CaseClosure(clausesExpr.map(parseCaseLambdaClause(_, env)))

  def parseDoBindings(bindingsExpr: Expr): List[DoBinding] =
    bindingsExpr match
      case Expr.ListExpr(bindings, _) =>
        val parsed = bindings.map {
          case Expr.ListExpr(List(Expr.Symbol(name, _), initExpr), _) =>
            DoBinding(name, initExpr, None)
          case Expr.ListExpr(List(Expr.Symbol(name, _), initExpr, stepExpr), _) =>
            DoBinding(name, initExpr, Some(stepExpr))
          case Expr.ListExpr(List(_, _), _) | Expr.ListExpr(List(_, _, _), _) =>
            throw new EvalError("do bindings must have symbol names")
          case _ =>
            throw new EvalError("do bindings must contain (name init) or (name init step) forms")
        }
        ensureDistinct(parsed.map(_.name), "do bindings")
        parsed
      case _ =>
        throw new EvalError("do bindings must be a list")

  def quoteExpr(expr: Expr): Value =
    expr match
      case Expr.IntegerLiteral(value, _) => Value.IntegerValue(value)
      case Expr.RationalLiteral(numerator, denominator, _) =>
        SchemeNumbers.exactRational(numerator, denominator)
      case Expr.InexactLiteral(value, _) => Value.InexactValue(value)
      case Expr.BooleanLiteral(value, _) => Value.BooleanValue(value)
      case Expr.StringLiteral(value, _)  => Value.StringValue(SchemeString.fromLiteral(value))
      case Expr.CharLiteral(value, _)    => Value.CharValue(value)
      case Expr.Symbol(name, _)          => Value.SymbolValue(name)
      case Expr.ListExpr(items, _) =>
        quoteList(items)
      case Expr.VectorExpr(items, _) =>
        Value.VectorValue(scala.collection.mutable.ArrayBuffer.from(items.map(quoteExpr)))

  private def quoteList(items: List[Expr]): Value =
    val dotIndices = items.zipWithIndex.collect { case (Expr.Symbol(".", _), index) => index }

    dotIndices match
      case Nil =>
        makeList(items.map(quoteExpr))
      case index :: Nil if index > 0 && index == items.length - 2 =>
        val tail = quoteExpr(items.last)
        items.take(index).reverse.foldLeft(tail) { (cdr, item) =>
          Value.PairValue(quoteExpr(item), cdr)
        }
      case _ =>
        throw new EvalError("invalid dotted list")

  private def parseClosureParams(paramsExpr: List[Expr]): (List[String], Option[String]) =
    val dotIndex = paramsExpr.indexWhere {
      case Expr.Symbol(".", _) => true
      case _                   => false
    }

    if dotIndex < 0 then (paramsExpr.map(requireParamName), None)
    else
      val fixedParams = paramsExpr.take(dotIndex).map(requireParamName)
      paramsExpr.drop(dotIndex) match
        case Expr.Symbol(".", _) :: Expr.Symbol(restName, _) :: Nil if restName != "." =>
          (fixedParams, Some(restName))
        case _ =>
          throw new EvalError("invalid lambda parameter list")

  private def parseCaseLambdaClause(clauseExpr: Expr, env: Env): CaseLambdaClause =
    clauseExpr match
      case Expr.ListExpr(Expr.ListExpr(params, _) :: body, _) if body.nonEmpty =>
        buildCaseLambdaClause(params, body, env)
      case _ =>
        throw new EvalError("invalid case-lambda clause")

  private def buildCaseLambdaClause(
    paramsExpr: List[Expr],
    body: List[Expr],
    env: Env
  ): CaseLambdaClause =
    val (fixedParams, restParam) = parseClosureParams(paramsExpr)
    ensureDistinct(fixedParams ++ restParam.toList, "lambda parameters")
    CaseLambdaClause(fixedParams, restParam, body, env)

  private def requireParamName(expr: Expr): String =
    expr match
      case Expr.Symbol(name, _) if name != "." => name
      case _                                   => throw new EvalError("lambda parameters must be symbols")
