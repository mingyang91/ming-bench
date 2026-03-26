package ming

import SchemeModel.*
import SchemeRuntime.*

private[ming] object SchemeEvaluatorSupport:

  def parseBindings(bindingsExpr: Expr): List[(String, Expr)] =
    bindingsExpr match
      case Expr.ListExpr(bindings, _) =>
        val parsed = bindings.map {
          case Expr.ListExpr(List(Expr.Symbol(name, _), valueExpr), _) =>
            (name, valueExpr)
          case Expr.ListExpr(List(_, _), _) =>
            throw new EvalError("let bindings must have symbol names")
          case _ =>
            throw new EvalError("let bindings must contain (name value) pairs")
        }
        ensureDistinct(parsed.map(_._1), "let bindings")
        parsed
      case _ =>
        throw new EvalError("let bindings must be a list")

  def buildClosure(
    paramsExpr: List[Expr],
    body: List[Expr],
    env: Env,
    name: Option[String]
  ): Value =
    val (fixedParams, restParam) = parseClosureParams(paramsExpr)
    ensureDistinct(fixedParams ++ restParam.toList, "lambda parameters")
    Value.Closure(name, fixedParams, restParam, body, env)

  def quoteExpr(expr: Expr): Value =
    expr match
      case Expr.IntegerLiteral(value, _) => Value.IntegerValue(value)
      case Expr.RationalLiteral(numerator, denominator, _) =>
        SchemeNumbers.exactRational(numerator, denominator)
      case Expr.InexactLiteral(value, _) => Value.InexactValue(value)
      case Expr.BooleanLiteral(value, _) => Value.BooleanValue(value)
      case Expr.StringLiteral(value, _)  => Value.StringValue(SchemeString.fromText(value))
      case Expr.CharLiteral(value, _)    => Value.CharValue(value)
      case Expr.Symbol(name, _)          => Value.SymbolValue(name)
      case Expr.ListExpr(items, _) =>
        makeList(items.map(quoteExpr))

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

  private def requireParamName(expr: Expr): String =
    expr match
      case Expr.Symbol(name, _) if name != "." => name
      case _                                   => throw new EvalError("lambda parameters must be symbols")
